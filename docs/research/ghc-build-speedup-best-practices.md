# GHC ビルド高速化 ベストプラクティス調査

調査日: 2026-03-14

Malgo のビルド高速化計画 (`bench/build-speedup-plan.md`) の各テーマについて、既知のベストプラクティスと根拠を調査した。

---

## テーマ A1: Dynamic オブジェクト無効化

### 背景

cabal はデフォルトで static (.o) と shared/dynamic (.dyn_o) の**両方**をビルドする。GHC の `-dynamic-too` フラグにより1パスで両方生成する場合もあるが、CodeGen は2倍近くになる。

### ベストプラクティス

**`--disable-library-vanilla` + `--enable-executable-dynamic`** または **`--disable-shared`** で片方のみビルドする。

[rybczak.net の計測](http://rybczak.net/2016/03/26/how-to-reduce-compilation-times-of-haskell-projects/) によると:

> shared-only builds being **40% faster** than static-only (idris の例)。一般的には **17-40% の高速化**。

#### Malgo への適用方法

`cabal.project.local` に以下を追加:

```cabal
package malgo
  shared: False
```

**注意**: Template Haskell は動的リンクを必要とする場合がある (GHC のバージョン・プラットフォーム依存)。Malgo は `Malgo.Prelude` と `Malgo.Id` で `makeStore` / `makeFieldsNoPrefix` を使っているため、TH が `-dynamic-too` を強制する可能性がある。適用後にビルドが通るか検証が必要。

macOS (aarch64) + GHC 9.12.2 の組み合わせでは、TH が static のみで動作するケースが多い。

### リスク: 低

GHCi / `cabal repl` を使う場合は dynamic が必要になるが、`cabal.project.local` で開発用に設定を分ければよい。

---

## テーマ B3: Effectful Plugin の限定適用

### 背景

`effectful-plugin` は effectful のエフェクト解決を GHC プラグインで支援するオプショナルなツール。Malgo では `package.yaml` のグローバル `ghc-options` で **全モジュール** に `-fplugin=Effectful.Plugin` を適用している。

### ベストプラクティス

[effectful-plugin README](https://github.com/haskell-effectful/effectful/tree/master/effectful-plugin):

> effectful-plugin は**オプション**であり、なくても動作する。ない場合は型注釈を明示的に書く必要がある場面が増える。

GHC プラグインはモジュールごとに typechecker パスで実行されるため、effectful を使わないモジュール（Parser, Sequent IR 定義, Syntax 等）では**純粋なオーバーヘッド**になる。

#### 適用方法

1. `package.yaml` のグローバル `ghc-options` から `-fplugin=Effectful.Plugin` を削除
2. effectful を使用するモジュールのみに `{-# OPTIONS_GHC -fplugin=Effectful.Plugin #-}` を追加

対象モジュール候補:
- `Malgo.Monad` (effectful のモナドスタック定義)
- `Malgo.Driver` (パイプライン実行)
- `Malgo.Rename.Pass` (Eff を使用)
- `Malgo.Infer` 系 (Eff を使用)
- その他 `Eff` を import しているモジュール

#### 期待効果

32モジュール中、effectful を使わないモジュールが半数以上あれば、TC パスの合計で **300-500ms 削減** が見込める。

### リスク: 低

プラグインを外したモジュールで型推論エラーが出る場合は、型注釈を追加するか、そのモジュールにプラグインを戻せばよい。

---

## テーマ B2: 不要な deriving の削除

### 背景

Malgo の dump-timings 分析で、**CodeGen が多くのモジュールで最大ボトルネック**であることが判明。特に standalone deriving が多い `Malgo.Syntax` (22箇所), `Malgo.Sequent.Core.Full` (19箇所) で顕著。

### ベストプラクティス

#### deriving のコスト序列

[Kowainik の Strategic Deriving](https://kowainik.github.io/posts/deriving):

| Strategy | コスト | 用途 |
|----------|--------|------|
| `newtype` | 最小 (coercion) | newtype ラッパー |
| `stock` (Eq, Show) | 低〜中 | 基本的な型クラス |
| `stock` (Ord) | 中 | 比較可能性が必要な場合のみ |
| `stock` (Generic) | **高** (二次的) | シリアライゼーション等 |
| `anyclass` (via Generic) | **高** | Generic ベースのインスタンス |

[Neil Mitchell のブログ](http://neilmitchell.blogspot.com/2019/02/quadratic-deriving-generic-compile-times.html):

> `deriving Generic` のコンパイル時間はコンストラクタ数に対して**二次的**に増加する。354 コンストラクタの型で 30秒かかった例がある。

#### Matt Parsons のアドバイス

[Keeping Compilation Fast](https://www.parsonsmatt.org/2019/11/27/keeping_compilation_fast.html):

> "Derived type class instances are work that GHC must redo every time the module is compiled."

→ 使われていない instance は純粋な無駄。

#### Malgo への適用方法

1. `Eq`, `Ord`, `Show` の実使用箇所を `grep` で調査
2. テストでのみ使われる instance は `test/` 配下の orphan instance モジュールに移動
3. `Ord` は特に使用頻度が低い可能性が高い → 確認後に削除

### リスク: 中

- テストで `Show` を使っている可能性 (Hspec のエラー表示)
- `Eq` はパターンマッチやテストの `shouldBe` で必要
- 削除前に依存関係の確認が必須

---

## テーマ B1: Malgo.Prelude の分割

### 背景

`Malgo.Prelude` は 314行、52 imports、TH splice (makeStore x3, makeFieldsNoPrefix x1) を含むモノリスモジュール。Renamer/TC だけで 596ms かかっている。全モジュールが依存しているため、変更時に全再コンパイルが発生する。

### ベストプラクティス

[Matt Parsons - Template Haskell Performance Tips](https://www.parsonsmatt.org/2021/07/12/template_haskell_performance_tips.html):

> 500行のモジュールを「20行の TH モジュール + 480行のロジックモジュール」に分割すれば、GHC は小さい方だけを再コンパイルし、大きい方はスキップできる。

> "Recompilation Cascade" — TH モジュールの変更が全依存モジュールの再ビルドを引き起こす。TH を隔離することでこれを防げる。

[Keeping Compilation Fast](https://www.parsonsmatt.org/2019/11/27/keeping_compilation_fast.html):

> "Factor concepts out of your `Project.Types` module." — 巨大な型定義モジュールを分割し、re-export しない。

#### Malgo への適用方法

```
Malgo.Prelude          -- 純粋な re-export のみ (TH なし)
Malgo.Prelude.Store    -- makeStore 系の TH splice
Malgo.Prelude.Optics   -- makeFieldsNoPrefix 系
```

- `Malgo.Prelude` 本体から TH import (`Control.Lens.TH`, `Data.Store.TH`) を除去
- TH を使うモジュール (`Malgo.Id` 等) のみが `Malgo.Prelude.Store` を import

### 期待効果

- クリーンビルド: Prelude の TH 初期化コスト分離で **100-200ms 削減**
- **増分ビルド**: Prelude 本体の変更が TH 再実行を引き起こさなくなる → 大きな効果

### リスク: 低

import パスの変更のみで、ロジックの変更はない。

---

## テーマ C1: 型族 (Type Family) の最適化

### 背景

Malgo は Trees That Grow パターンを採用し、`Malgo.Syntax.Extension` に 61 の type family / type instance を定義。`Malgo.Syntax` の standalone deriving では `ForallExpX Eq x, ForallClauseX Eq x, ...` のような複雑な制約コンテキストが生成されている。

### ベストプラクティス

[GHC Issue #8095 - TypeFamilies painfully slow](https://gitlab.haskell.org/ghc/ghc/-/issues/8095):

> Type family の解決はコンストラクタ数・制約数に対して超線形に遅くなる可能性がある。

[GHC Trees That Grow Guidance](https://gitlab.haskell.org/ghc/ghc/-/wikis/implementing-trees-that-grow/trees-that-grow-guidance):

GHC 自身も Trees That Grow を採用しており、以下のガイダンスを提供:

1. **Extension point は必要最小限に** — すべてのコンストラクタに拡張点を置くのではなく、実際に拡張が必要な箇所のみ
2. **制約族 (constraint family) を避ける** — `ForallExpX Show x` のような制約族は TC を遅くする。代わりに具体的な phase でのインスタンスを直接定義する
3. **standalone deriving の代わりに手動インスタンス** — 複雑な制約コンテキストを持つ standalone deriving は避け、phase ごとに具体的なインスタンスを書く

#### Malgo への適用方法

現在:
```haskell
deriving stock instance (ForallExpX Show x, ForallClauseX Show x,
  ForallPatX Show x, ForallCoPatX Show x, ForallStmtX Show x,
  ForallTypeX Show x, Show (XId x)) => Show (Expr x)
```

改善案:
```haskell
-- Phase ごとに具体的なインスタンスを定義
deriving stock instance Show (Expr (Malgo Parse))
deriving stock instance Show (Expr (Malgo Rename))
```

これにより:
- 制約解決のコストが大幅に削減
- CodeGen で生成されるコードも具体的な型に特化されて小さくなる

### 期待効果: 大

`Malgo.Syntax` の TC (251ms) + CodeGen (466ms) + Simplifier (200ms) のすべてに効く可能性がある。

### リスク: 高

- 設計の根幹に関わる変更
- phase を追加するたびにインスタンスの追加が必要になる
- 新しい phase に対する拡張性が低下する

---

## テーマ追加: GHC RTS オプション

### ベストプラクティス

[Matt Parsons](https://www.parsonsmatt.org/2019/11/27/keeping_compilation_fast.html) / [rybczak.net](http://rybczak.net/2016/03/26/how-to-reduce-compilation-times-of-haskell-projects/):

```bash
cabal build --ghc-options="-j4 +RTS -A128m -n2m -RTS"
```

| オプション | 効果 |
|-----------|------|
| `-j4` | GHC 内部の並列コンパイル (モジュール内) |
| `+RTS -A128m` | GC allocation area を 128MB に拡大 → GC 頻度 80% 削減 |
| `+RTS -n2m` | nursery チャンク 2MB → minor GC 効率化 |

rybczak.net の計測では GC 設定だけで **25% の高速化**。

#### Malgo への適用

`cabal.project.local`:
```cabal
program-options
  ghc-options: -j4 +RTS -A128m -n2m -RTS
```

### リスク: 低

メモリ使用量が増えるがビルド時のみ。

---

## 総合推奨: 施策適用順序

| 順序 | 施策 | 期待効果 | リスク | 工数 |
|------|------|----------|--------|------|
| 1 | A1: shared: False | 17-40% | 低 (TH 検証要) | 1行 |
| 2 | RTS: -A128m -n2m | ~25% (GC) | 低 | 1行 |
| 3 | B3: Plugin 限定化 | 5-10% | 低 | 小 |
| 4 | B1: Prelude 分割 | 増分ビルド大改善 | 低 | 中 |
| 5 | B2: deriving 削減 | 5-15% | 中 | 中 |
| 6 | C1: 型族最適化 | 大 | 高 | 大 |

施策 1-3 はコード変更なし (cabal 設定のみ) で即座に効果を確認できる。
施策 4-5 は小規模なリファクタリング。
施策 6 は設計判断を伴う。

---

## Sources

- [Keeping Compilation Fast - Matt Parsons (2019)](https://www.parsonsmatt.org/2019/11/27/keeping_compilation_fast.html)
- [Template Haskell Performance Tips - Matt Parsons (2021)](https://www.parsonsmatt.org/2021/07/12/template_haskell_performance_tips.html)
- [Quadratic "deriving Generic" Compile Times - Neil Mitchell (2019)](http://neilmitchell.blogspot.com/2019/02/quadratic-deriving-generic-compile-times.html)
- [How to reduce compilation times of Haskell projects - rybczak.net (2016)](http://rybczak.net/2016/03/26/how-to-reduce-compilation-times-of-haskell-projects/)
- [Making GHC faster at emitting code - Tweag (2022)](https://www.tweag.io/blog/2022-12-22-making-ghc-faster-at-emitting-code/)
- [Strategic Deriving - Kowainik](https://kowainik.github.io/posts/deriving)
- [GHC Issue #8095: TypeFamilies painfully slow](https://gitlab.haskell.org/ghc/ghc/-/issues/8095)
- [GHC Issue #5642: Deriving Generic of a big type](https://gitlab.haskell.org/ghc/ghc/-/issues/5642)
- [Trees That Grow Guidance - GHC Wiki](https://gitlab.haskell.org/ghc/ghc/-/wikis/implementing-trees-that-grow/trees-that-grow-guidance)
- [effectful-plugin README](https://github.com/haskell-effectful/effectful/tree/master/effectful-plugin)
- [cabal.project Reference](https://cabal.readthedocs.io/en/3.6/cabal-project.html)
