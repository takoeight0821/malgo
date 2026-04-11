# 依存パッケージ削減計画

Date: 2026-04-10

関連 issue: #285, #286

## Context

CI のキャッシュミス時、依存パッケージのビルドに ~10 分かかる。パッケージ数を減らすことでこの時間を直接短縮できる。現在の推移的依存は freeze ファイル基準で ~238 パッケージ。

## 変更内容

### Phase 0: malgo-lsp パッケージ分割（最大インパクト）

`lsp` が `lens >= 5.1`、`co-log-core` に依存しており、これらが malgo 本体の依存ツリーを肥大化させている。LSP 関連コードは 3 ファイル（`LSP.hs`, `LSP/Handlers.hs`, `LSP/Diagnostics.hs`）に完全に閉じており、malgo コアへの逆依存はない。

**パッケージ分割でライブラリから除去できる依存**:
- `lsp` + `lsp-types` + `lsp-test` + 推移的依存（大量）
- `lens` + 推移的依存（~13パッケージ: adjunctions, free, kan-extensions, profunctors 等）
- `co-log-core`

**構成**:

```
malgo/                    ← ライブラリ + malgo exe（LSP 依存なし）
  package.yaml
  src/Malgo/              ← コアモジュール（Parser, Rename, Sequent, Driver, Query 等）
  app/malgo/Main.hs

malgo-lsp/                ← 別パッケージ（malgo ライブラリに依存）
  package.yaml
  src/Malgo/LSP.hs
  src/Malgo/LSP/Handlers.hs
  src/Malgo/LSP/Diagnostics.hs
  app/malgo-lsp/Main.hs
```

**移行手順**:
1. `malgo-lsp/` ディレクトリを作成し `package.yaml` を記述
   - dependencies: `malgo` (ライブラリ), `lsp`, `lens`, `co-log-core`
2. `src/Malgo/LSP.hs`, `src/Malgo/LSP/Handlers.hs`, `src/Malgo/LSP/Diagnostics.hs` を `malgo-lsp/src/` に移動
3. `app/malgo-lsp/Main.hs` を `malgo-lsp/app/` に移動
4. malgo 本体の `package.yaml` から `lsp`, `lens`, `co-log-core` を削除
5. `cabal.project` に `packages: *.cabal malgo-lsp/*.cabal` を追加
6. malgo 本体で `lens` の代わりに `microlens` + `microlens-th` を使用
   - `Control.Lens` → `Lens.Micro` / `Lens.Micro.TH`
   - `(??)` (1箇所) → `flip (<&>)` で代替
7. CI の `cabal build all` / `cabal test all` はマルチパッケージ対応なので変更不要

**対象ファイル**:
- `package.yaml` — `lsp`, `lens`, `co-log-core` 依存の削除、LSP モジュール・exe の除外
- `cabal.project` — `packages` フィールドの追加
- `malgo-lsp/package.yaml` — 新規作成
- `src/Malgo/LSP.hs` → `malgo-lsp/src/Malgo/LSP.hs`
- `src/Malgo/LSP/Handlers.hs` → `malgo-lsp/src/Malgo/LSP/Handlers.hs`
- `src/Malgo/LSP/Diagnostics.hs` → `malgo-lsp/src/Malgo/LSP/Diagnostics.hs`
- `app/malgo-lsp/Main.hs` → `malgo-lsp/app/Main.hs`
- `src/Malgo/Prelude.hs` — `Control.Lens` → `Lens.Micro` の import 変更
- `src/Malgo/Syntax.hs` — `Control.Lens` → `Lens.Micro` / `Lens.Micro.TH`
- 他 lens import のある 4 ファイル — import パス変更
- `.github/workflows/build.yml` — ジョブ分割（後述）

**削減（malgo 本体）**: `lsp` + `lens` + `co-log-core` + 推移的依存で **推定 30-50 パッケージ**

#### 0-2. CI ジョブ分割

パッケージ分割に伴い、CI を `build-malgo` と `build-malgo-lsp` の 2 ジョブに分割する。

**設計方針**:
- `build-malgo`: malgo 本体のビルド・テスト。軽量な依存のみ
- `build-malgo-lsp`: malgo-lsp のビルド。malgo ジョブ完了後に実行（`needs: build-malgo`）。テストなし（LSP サーバーはテストスイートを持たない現状）
- キャッシュは共有（cabal store は同じキーで両ジョブから利用）
- パスフィルタで不要なジョブをスキップ可能（将来的）

**build.yml イメージ**:

```yaml
name: build
on:
  push:
    branches: [main, master]
  pull_request:
    branches: [main, master]

jobs:
  build-malgo:
    name: malgo (GHC ${{ matrix.ghc-version }})
    runs-on: ubuntu-latest
    strategy:
      fail-fast: false
      matrix:
        ghc-version: ['9.12.4']
    steps:
      - uses: actions/checkout@v6
        with:
          fetch-depth: 2
      - name: Restore file timestamps
        run: |
          rev=HEAD
          for f in $(git ls-tree -r -t --full-name --name-only "$rev"); do
            touch -d "$(git log --pretty=format:%cI -1 "$rev" -- "$f")" "$f"
          done
      - uses: haskell-actions/setup@v2
        id: setup
        with:
          ghc-version: ${{ matrix.ghc-version }}
          cabal-version: latest
          cabal-update: true
      - name: Configure
        run: cabal configure --enable-tests --disable-documentation
      - name: Cache cabal store
        uses: actions/cache@v5
        with:
          path: ${{ steps.setup.outputs.cabal-store }}
          key: ${{ runner.os }}-ghc-${{ matrix.ghc-version }}-store-${{ hashFiles('cabal.project.freeze') }}
          restore-keys: |
            ${{ runner.os }}-ghc-${{ matrix.ghc-version }}-store-
      - name: Cache dist-newstyle
        uses: actions/cache@v5
        with:
          path: dist-newstyle
          key: ${{ runner.os }}-ghc-${{ matrix.ghc-version }}-dist-${{ hashFiles('cabal.project.freeze') }}-${{ hashFiles('src/**/*.hs', '*.cabal', 'package.yaml') }}
          restore-keys: |
            ${{ runner.os }}-ghc-${{ matrix.ghc-version }}-dist-${{ hashFiles('cabal.project.freeze') }}-
      - name: Build malgo
        run: cabal build malgo-lib malgo-exe --only-dependencies && cabal build malgo-lib malgo-exe
      - name: Test malgo
        run: cabal test malgo-test --test-option=--jobs=4

  build-malgo-lsp:
    name: malgo-lsp (GHC ${{ matrix.ghc-version }})
    needs: build-malgo
    runs-on: ubuntu-latest
    strategy:
      fail-fast: false
      matrix:
        ghc-version: ['9.12.4']
    steps:
      - uses: actions/checkout@v6
        with:
          fetch-depth: 2
      - name: Restore file timestamps
        run: |
          rev=HEAD
          for f in $(git ls-tree -r -t --full-name --name-only "$rev"); do
            touch -d "$(git log --pretty=format:%cI -1 "$rev" -- "$f")" "$f"
          done
      - uses: haskell-actions/setup@v2
        id: setup
        with:
          ghc-version: ${{ matrix.ghc-version }}
          cabal-version: latest
          cabal-update: true
      - name: Configure
        run: cabal configure --disable-tests --disable-documentation
      - name: Cache cabal store
        uses: actions/cache@v5
        with:
          path: ${{ steps.setup.outputs.cabal-store }}
          key: ${{ runner.os }}-ghc-${{ matrix.ghc-version }}-store-${{ hashFiles('cabal.project.freeze') }}
          restore-keys: |
            ${{ runner.os }}-ghc-${{ matrix.ghc-version }}-store-
      - name: Cache dist-newstyle
        uses: actions/cache@v5
        with:
          path: dist-newstyle
          key: ${{ runner.os }}-ghc-${{ matrix.ghc-version }}-dist-lsp-${{ hashFiles('cabal.project.freeze') }}-${{ hashFiles('malgo-lsp/**/*.hs', 'malgo-lsp/*.cabal', 'malgo-lsp/package.yaml') }}
          restore-keys: |
            ${{ runner.os }}-ghc-${{ matrix.ghc-version }}-dist-lsp-${{ hashFiles('cabal.project.freeze') }}-
      - name: Build malgo-lsp
        run: cabal build malgo-lsp-exe --only-dependencies && cabal build malgo-lsp-exe
```

**ポイント**:
- `build-malgo-lsp` は `needs: build-malgo` で malgo ジョブ完了後に実行
- cabal store キャッシュは同じキーを共有。`build-malgo` がキャッシュを生成し、`build-malgo-lsp` がヒットさせる
- dist-newstyle は別キー（`dist-lsp-`）で分離。malgo 本体の変更で malgo-lsp のキャッシュが壊れない
- `build-malgo` は `cabal build malgo-lib malgo-exe` でスコープを限定（malgo-lsp の依存を引かない）
- `build-malgo-lsp` はテストなし（`--disable-tests`）

**注意**: 実際のコンポーネント名（`malgo-lib`, `malgo-exe`, `malgo-lsp-exe`）は package.yaml の設定に依存。hpack 生成後の .cabal ファイルで確認が必要

### Phase 1: 確実に削除可能な依存（低リスク・低工数）

#### 1-1. `store` → `binary` 移行

`store` + `store-core` (2パッケージ) を削除し、GHC ブートライブラリの `binary` に移行。

**現状**:
- ~26 型が `deriving anyclass (Store)`
- 7 型が `makeStore` TH
- encode/decode 呼び出しは 3 箇所のみ（Module.hs x2, Engine.hs x1）
- `.mlgi`（interface）と `.sqt`（compiled program）のディスクキャッシュに使用

**移行手順**:
1. `package.yaml` から `store` を削除
2. `deriving anyclass (Store)` → `deriving anyclass (Binary)` を全型で置換
3. `makeStore ''Type` TH（7箇所）→ 削除し `instance Binary Type` で Generic デフォルトに
4. `Data.Store (encode)` → `Data.Binary (encode)`
5. `Data.Store (decodeEx)` → `Data.Binary (decode)` (型が `decode :: ByteString -> a` ではなく `decode :: ByteString -> a` で例外を投げるのでラッパーが必要な場合あり)
6. `ViaStore` wrapper → `ViaBinary` にリネーム
7. `.malgo-work/` の既存キャッシュファイルは互換性がないため削除が必要（`rm -rf .malgo-work` で再生成）

**対象ファイル**:
- `package.yaml` — `store` 依存の削除
- `src/Malgo/Module.hs` — `ViaStore`, `Resource` クラス、orphan instances
- `src/Malgo/Prelude.hs` — `makeStore` TH、`Data.Store` import
- `src/Malgo/Id.hs` — `makeStore` TH
- `src/Malgo/Syntax/Extension.hs` — `makeStore` TH
- `src/Malgo/Interface.hs` — `Store` deriving
- `src/Malgo/Query/Engine.hs` — `decodeEx` 呼び出し
- `src/Malgo/Sequent/Fun.hs` — `Store` deriving
- `src/Malgo/Sequent/Core/Full.hs` — `Store` deriving
- `src/Malgo/Sequent/Core/Flat.hs` — `Store` deriving
- `src/Malgo/Sequent/Core/Join.hs` — `Store` deriving

**削減**: 2 パッケージ（store, store-core）

#### 1-2. `pretty-simple` 削除

**現状**: 2 ファイルのみで使用
- `src/Malgo/Module.hs` — `pShowNoColor` in `ViaShow` instance
- `test/Malgo/TestUtils.hs` — `pShowOpt` for test output formatting

**移行手順**:
1. `Module.hs` の `ViaShow` — `show` または `prettyprinter`（既存依存）の `pretty` で代替
2. `TestUtils.hs` — `show` で代替（テスト出力のフォーマットは厳密でなくてよい）
3. `package.yaml` から `pretty-simple` を削除

**削減**: 1+ パッケージ

#### 1-3. `extra` の使用状況精査・削除

**現状**: Prelude.hs で re-export
- `applyWhen` (Data.Function.Extra) — ソースコードで未使用
- `ifM` (Control.Monad.Extra) — 使用箇所を要確認

**移行手順**:
1. `applyWhen`, `ifM` のグレップ確認
2. 未使用なら import 削除
3. 使用箇所があれば `bool` や `if-then-else` で代替（trivial な関数）
4. `extra` の他の re-export が使われていないか確認
5. 全て不要なら `package.yaml` から削除

**削減**: 1 パッケージ

### Phase 2: 中工数の削減

#### 2-1. `string-conversions` 削除

**現状**: Prelude 経由で 13 ファイルが `convertString` を使用

**移行手順**:
1. `convertString` の各呼び出しを型を確認して適切な変換関数に置換:
   - `Text → ByteString`: `Data.Text.Encoding.encodeUtf8`
   - `ByteString → Text`: `Data.Text.Encoding.decodeUtf8`
   - `String → Text`: `Data.Text.pack`
   - `Text → String`: `Data.Text.unpack`
   - Lazy 版も同様
2. `ConvertibleStrings` 型クラス制約を具体型に置換
3. `package.yaml` から `string-conversions` を削除

**削減**: 1 パッケージ

**注意**: `ConvertibleStrings` が型クラス制約として使われている箇所（関数シグネチャ）は慎重に対応が必要。工数が見積もりより大きくなる可能性あり。

## スコープ外

- **`store` → `serialise`（CBOR）**: `binary`（ブートライブラリ）の方が追加依存ゼロで優れている
- **`s-cargot`**: 9 ファイルで使用、コア機能。削除不可
- **`unordered-containers`**: 他パッケージの推移的依存として残る可能性が高い

## 期待される効果

| Phase | 削減パッケージ数（malgo 本体） | 工数 |
|---|---:|---|
| Phase 0 (LSP 分割 + lens → microlens) | 30-50（推定） | 中（1日程度） |
| Phase 1 (store, pretty-simple, extra) | 4-5 | 低〜中（半日程度） |
| Phase 2 (string-conversions) | 1 | 中（13ファイル変更） |
| 合計 | 35-56（推定） | |

推移的依存の削減は freeze ファイル再生成後に正確に計測する。

## 実施順序

Phase 0 → Phase 1 → Phase 2 の順に実施。Phase 0 が最大インパクトだが、Phase 1 の store → binary は Phase 0 と独立して並列実施可能。

## 検証

各 Phase 完了後:
```bash
mise run format && mise run test
```

全体完了後:
```bash
cabal freeze
wc -l cabal.project.freeze
```

freeze ファイルの行数が現在の 317 行から減少していることを確認。
