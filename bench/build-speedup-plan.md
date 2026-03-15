# ビルド時間改善計画

## 現状 (ベースライン 2026-03-14)

- クリーンビルド: **23.5秒** (wall), 37.3秒 (CPU)
- テスト: **21.2秒**
- 全 32 モジュール (static) 合計: 12,591ms
- 上位 5 モジュールで全体の 40%

## ボトルネック分析

### パス別の傾向

| Module | CodeGen | Renamer/TC | Desugar | Simplifier | 特記 |
|--------|---------|------------|---------|------------|------|
| Malgo.Syntax (1222ms) | **466ms** | 251ms | 60ms | 200ms | 大量の standalone deriving + 型族 |
| Malgo.Prelude (1075ms) | 42ms | **596ms** | 130ms | - | 52 imports, TH (makeStore x3), ConsistencyCheck 282ms |
| Malgo.Parser.CStyle (985ms) | **278ms** | 265ms | 46ms | 90ms | megaparsec コンビネータ |
| Malgo.Rename.Pass (938ms) | **250ms** | 205ms | 118ms | 81ms | |
| Malgo.Sequent.Core.Full (764ms) | **294ms** | 54ms | 30ms | 149ms | 151行で764ms → deriving が重い |

**共通パターン**: CodeGen が多くのモジュールで最大ボトルネック。`-O0` でも重い。

### 根本原因

1. **CodeGen の肥大化**: 大量の standalone deriving (Syntax: 22箇所, Core.Full: 19箇所) が生成コード量を膨張させている
2. **Malgo.Prelude の Renamer/TC (596ms)**: 52個の import + TH splice (makeStore x3, makeFieldsNoPrefix) の型チェック
3. **Effectful Plugin**: 全モジュールで `-fplugin=Effectful.Plugin` が動作 → 各モジュールの TC/Simplifier に上乗せ
4. **dyn オブジェクト二重ビルド**: 各モジュールは static + dynamic の両方がビルドされている (cabal のデフォルト)

---

## 改善案

### A. 即効性が高い (コード変更なし)

#### A1. dynamic オブジェクト無効化
**期待効果: ビルド時間 30-40% 削減**

`cabal.project` に追加:
```
package malgo
  shared: False
```

または:
```
library-vanilla: True
shared: False
```

現在 static + dyn の2回ビルドが走っている。dyn を無効化すれば単純にほぼ半減。

#### A2. `-j` 並列度の確認と最適化
**期待効果: wall time 短縮**

```bash
cabal build -j4  -- CPU コア数に合わせる
```

現在の CPU 利用率 158% は 2コア相当。明示的に並列度を設定して確認。

#### A3. `-fno-code` でのテスト用型チェックのみビルド (開発中)
**期待効果: 型チェックだけで良い場面で 50%+ 削減**

CodeGen が最大ボトルネックなので、型チェックだけのモードがあれば大きく効く。

### B. 中程度の効果 (小規模コード変更)

#### B1. Malgo.Prelude の分割
**期待効果: 増分ビルド時の再コンパイル削減**

現在 Prelude は 52 imports のモノリス。変更が入ると全モジュールが再コンパイルされる。
TH 部分 (makeStore 等) を `Malgo.Prelude.TH` に分離すれば、TH 不要なモジュールの再コンパイルを回避できる。

#### B2. 不要な deriving の削除
**期待効果: Syntax, Core.Full 等で 100-300ms 削減**

`Eq`, `Ord`, `Show` の standalone deriving が CodeGen を肥大化させている。
実際に使われている instance のみに絞る。特に `Ord` は使用頻度が低い可能性が高い。

#### B3. Effectful Plugin の必要性検証
**期待効果: 各モジュール 10-50ms 削減 (合計 300-500ms)**

`-fplugin=Effectful.Plugin` は全モジュールに適用されている。
effectful を使わないモジュール (Parser, Sequent IR 定義等) では不要。
`package.yaml` のグローバル設定から外し、必要なモジュールのみに `OPTIONS_GHC` で適用する。

### C. 効果大だが変更も大きい

#### C1. Syntax の型族 (Type Family) 簡素化
**期待効果: Syntax 251ms (TC) + 依存モジュール全体に波及**

`ForallExpX`, `ForallClauseX` 等の制約族が standalone deriving のコンテキストを複雑化している。
Trees That Grow パターンの制約を簡略化するか、Generic 経由の deriving に切り替える。

#### C2. GHC の `-fsplit-sections` / `-split-objs` 検討
CodeGen と Linker 時間のトレードオフ。プロファイル次第。

---

## 優先順位 (推奨実施順序)

| 優先度 | 施策 | 期待効果 | リスク | 工数 |
|--------|------|----------|--------|------|
| **1** | A1: dyn 無効化 | 30-40% | 低 (GHCi は使わない前提) | 1行 |
| **2** | B3: Effectful Plugin 限定化 | 5-10% | 低 | 小 |
| **3** | B2: 不要 deriving 削除 | 5-15% | 中 (テスト依存確認) | 中 |
| **4** | B1: Prelude 分割 | 増分ビルド改善 | 低 | 中 |
| **5** | C1: 型族簡素化 | 大 | 高 (設計変更) | 大 |

---

## 次のアクション

1. A1 を適用して再計測 → 効果を確認
2. B3 (Effectful Plugin 限定化) を適用して再計測
3. B2 の前に `Eq`/`Ord`/`Show` の実使用箇所を調査
