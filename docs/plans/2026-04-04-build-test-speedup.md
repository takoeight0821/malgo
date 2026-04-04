# ビルド・テスト高速化

Date: 2026-04-04

関連 issue: #285 (テスト高速化), #286 (ビルド高速化)

## Context

### 現状の問題

**テスト**

`test/Malgo/` 配下には 10 個の Spec ファイルがあり、`test/testcases/malgo/` の 83 個の `.mlg` ファイルを処理する。調査で判明した主なボトルネックは以下の通り：

1. **Builtin/Prelude の重複コンパイル**  
   `setupEvalBuiltin` / `setupEvalPrelude` は `EvalSpec` と `BigStepEvalSpec` の両方の `runIO` ブロックで独立して呼ばれる。Builtin.mlg と Prelude.mlg をそれぞれのSpecが個別にコンパイルしており、同じ処理が2回走る。

2. **BigStepEvalSpec での二重コンパイル**  
   `BigStepEvalSpec` では各テストケースを golden テスト (`driveBigStepEval`) と consistency テストで別々に `compileTestCase` している。83 ケース × 2 = 166 回のコンパイルが発生。

3. **ToCoreSpec での高コスト**  
   各テストケースが `driveToCore` / `driveFlat` / `driveJoin` の 3 パターン × 83 ケース = 249 回の parse-rename-compile サイクルを実行。それぞれ `withFreshQueryDB` で新しい DB を生成。

4. **hspec の並列スレッド数未指定**  
   各 Spec ファイルは `parallel` を使っているが、`cabal test` 実行時に `--num-threads` が渡されていないため、デフォルト（シングルスレッド）で動作する可能性がある。

**ビルド**

`cabal.project` には既に以下の最適化が適用済み：
- `jobs: $ncpus`（パッケージ並列ビルド）
- `-j +RTS -A128m -n2m -RTS`（GHC 内部並列化 + GC チューニング）

残る改善余地：
- テスト用ビルドに `-O0` が設定されていない。テストコードは最適化不要なので `-O0` にすることでコンパイル時間を大幅短縮できる。
- CI の `cabal test all` コマンドに `--test-option=--num-threads=N` が渡されていない。

### 現在の設定（参考）

```
# cabal.project
jobs: $ncpus
package *
  ghc-options: +RTS -A128m -n2m -RTS
package malgo
  ghc-options: -fno-ignore-asserts -j +RTS -A128m -n2m -RTS
  shared: False
```

```
# mise.toml（test タスク）
cabal test --test-show-details=direct
```

## Design Choices

### テスト共有セットアップ（Spec.hs のリファクタリング）

**選択**: `Spec.hs` の `main` を `hspec-discover` の自動検出から手動セットアップに変更し、builtin/prelude のコンパイル結果を各 Spec に引数として渡す。

- **採用理由**: `hspec-discover` によるオートディスカバリは `spec :: Spec` シグネチャを要求するが、引数を受け取る `specWith :: ArtifactPath -> ArtifactPath -> Spec` に変更することで共有が実現できる。変更範囲は `Spec.hs` と `EvalSpec`・`BigStepEvalSpec` の 3 ファイルのみで局所的。
- **却下案**: グローバル `IORef`（`unsafePerformIO`）による共有 → テストの独立性が損なわれ、実行順序依存のバグが起きやすい。

### BigStepEvalSpec の重複排除

**選択**: golden テストと consistency テストで `compileTestCase` の結果を共有する。

- `it` の中で `compileTestCase` を1回呼び、結果を両方のアサーションで使う。

### テスト用 GHC オプション `-O0`

**選択**: `cabal.project` で `malgo-test` パッケージに `ghc-options: -O0` を追加する。

- テストコードの最適化は不要。`-O0` により GHC のコンパイル時間を削減できる（経験則で 30〜50% 短縮）。
- 被テストコード（`malgo` ライブラリ）は最適化を維持。

### hspec 並列スレッド数の明示

**選択**: `mise.toml` の `test` タスクと CI の `build.yml` に `--test-option=--num-threads=4` を追加する。

- `parallel` 指定済みの Spec は追加コストなしで並列実行される。
- スレッド数は CI 環境（GitHub Actions ubuntu-latest: 2 コア）を考慮して 4 を上限に設定。

## Implementation Plan

各タスクは独立して実装可能。

### Task 1: テスト用 GHC オプション最適化

- **Goal**: テストバイナリのビルド時間を短縮する
- **Scope**: `cabal.project`
- **Dependencies**: なし
- **Steps**:
  1. `cabal.project` に以下を追加：
     ```
     package malgo
       -- （既存の設定の下に追記）
     
     -- テスト用：最適化なしで高速ビルド
     package malgo-test
       ghc-options: -O0
     ```
  2. `mise run build` でビルドが通ることを確認
- **Verification**: `cabal build malgo-test` の実行時間がベースラインより短縮されること

### Task 2: hspec 並列スレッド数の設定

- **Goal**: hspec の並列実行を実際に活用し、テスト実行時間を短縮する
- **Scope**: `mise.toml`, `.github/workflows/build.yml`
- **Dependencies**: なし
- **Steps**:
  1. `mise.toml` の test タスクを変更：
     ```toml
     [tasks.test]
     run = "cabal test --test-show-details=direct --test-option=--num-threads=4"
     depends = ["build"]
     ```
  2. `.github/workflows/build.yml` の `cabal test all` を同様に変更：
     ```yaml
     run: cabal test all --test-show-details=direct --test-option=--num-threads=4
     ```
- **Verification**: `mise run test` が以前より短い時間で完了すること（`parallel` ブロックのある EvalSpec, BigStepEvalSpec, ToCoreSpec が並列実行される）

### Task 3: Builtin/Prelude セットアップの共有化

- **Goal**: `setupEvalBuiltin`/`setupEvalPrelude` を1回だけ実行し、EvalSpec と BigStepEvalSpec で結果を共有する
- **Scope**: `test/Spec.hs`, `test/Malgo/Sequent/EvalSpec.hs`, `test/Malgo/Sequent/BigStepEvalSpec.hs`
- **Dependencies**: なし（Task 1, 2 と並列実装可能）
- **Steps**:
  1. `EvalSpec.hs` の `spec :: Spec` を `specWith :: ArtifactPath -> ArtifactPath -> Spec` にリネーム：
     ```haskell
     specWith :: ArtifactPath -> ArtifactPath -> Spec
     specWith builtin prelude = parallel do
       -- runIO による setupEvalBuiltin/setupEvalPrelude の呼び出しを削除
       testcases <- runIO do
         files <- listDirectory testcaseDir
         pure $ filter (isExtensionOf "mlg") files
       for_ testcases \testcase -> do
         golden (takeBaseName testcase) (driveEval builtin prelude (testcaseDir </> testcase))
     ```
  2. 同様に `BigStepEvalSpec.hs` の `spec` を `specWith` に変更
  3. `test/Spec.hs` を `hspec-discover` から手動 `main` に変更：
     ```haskell
     -- test/Spec.hs
     module Main (main) where
     
     import Malgo.TestUtils (setupEvalBuiltin, setupEvalPrelude)
     import Malgo.Sequent.EvalSpec qualified as EvalSpec
     import Malgo.Sequent.BigStepEvalSpec qualified as BigStepEvalSpec
     -- その他のSpecは通常通りインポート
     import Malgo.ParserSpec qualified as ParserSpec
     import Malgo.RenameSpec qualified as RenameSpec
     import Malgo.InferSpec qualified as InferSpec
     import Malgo.ElaborateSpec qualified as ElaborateSpec
     import Malgo.FeaturesSpec qualified as FeaturesSpec
     import Malgo.Backend.SchemeSpec qualified as SchemeSpec
     import Malgo.Sequent.ToFunSpec qualified as ToFunSpec
     import Malgo.Sequent.ToCoreSpec qualified as ToCoreSpec
     import Test.Hspec (hspec, describe)
     
     main :: IO ()
     main = do
       builtin <- setupEvalBuiltin
       prelude <- setupEvalPrelude
       hspec do
         describe "Parser" ParserSpec.spec
         describe "Rename" RenameSpec.spec
         describe "Infer" InferSpec.spec
         describe "Elaborate" ElaborateSpec.spec
         describe "Features" FeaturesSpec.spec
         describe "Backend/Scheme" SchemeSpec.spec
         describe "Sequent/ToFun" ToFunSpec.spec
         describe "Sequent/ToCore" ToCoreSpec.spec
         describe "Sequent/Eval" (EvalSpec.specWith builtin prelude)
         describe "Sequent/BigStepEval" (BigStepEvalSpec.specWith builtin prelude)
     ```
  4. `package.yaml` の `tests` stanza で `main-is: Spec.hs` のまま確認（または `main-is: Main.hs` に変更が必要かも）
- **Verification**: `mise run test` が通ること。setupEvalBuiltin/setupEvalPrelude が1回だけ実行されること（ログで確認）。

### Task 4: BigStepEvalSpec の重複コンパイル排除

- **Goal**: 各テストケースの `compileTestCase` 呼び出しを golden と consistency テストで共有する
- **Scope**: `test/Malgo/Sequent/BigStepEvalSpec.hs`
- **Dependencies**: Task 3（`specWith` への移行後に実施推奨）
- **Steps**:
  1. `for_ testcases` のループを変更し、`runIO` で `compileTestCase` を1回だけ実行してから両テストに渡す：
     ```haskell
     for_ testcases \testcase -> do
       (moduleName, program) <- runIO $ compileTestCase builtin prelude (testcaseDir </> testcase)
       -- golden テスト
       it (takeBaseName testcase) do
         result <- runEval bigStepEvalProgram moduleName program
         -- golden assertion
       -- consistency テスト
       it (takeBaseName testcase <> " matches small-step") do
         smallStepResult <- try @SomeException $ runEval evalProgram moduleName program
         bigStepResult <- try @SomeException $ runEval bigStepEvalProgram moduleName program
         ...
     ```
  2. `driveBigStepEval` 関数の引数に `(ModuleName, Program)` を受け取るバージョンを追加（または既存関数を修正）
- **Verification**: BigStepEvalSpec のテスト数が変わらず、実行時間が短縮されること

## Verification

全タスク完了後：

```bash
mise run test
```

- 全テストがパスすること
- ゴールデンファイルの差分がないこと（`git diff test/`）
- CI の build.yml が通ること

## Risks

| Risk | Mitigation |
|------|------------|
| `hspec-discover` から手動 `main` への移行で Spec の検出漏れが起きる | `Spec.hs` に全 Spec を明示的にリストアップし、CI で確認 |
| `runIO` で取得した `(ModuleName, Program)` を並列テスト間で共有すると競合が起きる | `Program` は read-only なデータ構造なので問題なし。`runIO` は spec 組み立て時（単一スレッド）に実行される |
| `-O0` でテストの挙動が変わる | テストコードは純粋な IO なので最適化依存のバグは発生しない。被テストコードの最適化は変更しない |
| CI のスレッド数指定が環境によって遅くなる | GitHub Actions は 2 コアなので `--num-threads=4` は上限として機能する。問題があれば `--num-threads=2` に戻す |
