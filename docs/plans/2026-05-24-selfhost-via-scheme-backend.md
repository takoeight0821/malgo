# セルフホスト実装をSchemeバックエンドで実行する

Date: 2026-05-24

## Context

現在のセルフホストテスト (`scripts/selfhost-golden.sh`) は、HaskellのEvalPassを使って
`runtime/malgo/compiler/Main.mlg` を実行している。

```bash
# 現在の実行方式
$MALGO eval runtime/malgo/compiler/Main.mlg "$src"
```

HaskellのSchemeバックエンド (`src/Malgo/Backend/Scheme.hs`) は既にJoin IRからScheme
ソースコードへの変換を完全実装しているが、実際にSchemeを実行するインフラがない。

また、`runtime/malgo/compiler/Scheme.mlg` というMalgo実装のSchemeバックエンドがあるが：
- `Main.mlg` から**インポートされていない**（使われていない）
- `scripts/selfhost-golden.sh` のprecompileリストにも含まれていない
- Haskell版の `SchemePass` と重複する役割

**目標**: `Scheme.mlg` を削除し、セルフホスト版をSchemeバックエンド経由で実行する。

```
# 新しい実行方式
malgo eval --target scheme Main.mlg > main.scm   # Schemeにコンパイル（1回）
scheme --script main.scm "$src"                   # Schemeで実行（テストごと）
```

### 既知のバグ：`compileToScheme` の `malgo-main` 参照

現在の `compileToScheme` は出力末尾に以下を生成する（`src/Malgo/Backend/Scheme.hs:39`）：

```scheme
(malgo-main malgo-print-result)
```

`malgo-main` はschemeRuntimeに定義されておらず、実際のmain関数は
`HelloBoxed_dot_mlg_dot_main` などにマングルされる。この名前不一致により
Scheme実行時にエラーになる（生成コードを実際に実行していなかったため未発覚）。

加えて `malgo-print-result` はUnit値 `'()` を `()\n` として出力するため、
ゴールデンテストの期待値と不一致になる。正しくは `malgo-finish` を使う。

同様のバグが `runtime/malgo/compiler/Scheme.mlg:168` にも存在するが、
そのファイル自体を削除するため自動的に解消される。

## Design Choices

**Scheme処理系**: Chez Scheme を採用
- Haskell SchemePass のランタイムが `get-string-all`, `put-string`, `string-join` 等の
  R6RS/Chez組み込みを使用しており相性が良い
- vendor/ziku にも ChezScheme が存在しており、プロジェクトの慣習に沿う
- ubuntu-latest CI では `apt-get install chezscheme` で導入可能
- 起動コマンドは `scheme --script file arg...`（`chez` ではなくポータブルな `scheme` を使用）

**`malgo-finish` vs `malgo-print-result`**:
実行可能プログラムとして動かす場合、`main :: () -> ()` の戻り値 `'()` を表示すべきでない。
既存ゴールデンファイルもIO副作用のみを期待しているため `malgo-finish` を使う。

**SchemePassへのモジュール名受け渡し**: `ToFunPass` と同様に `Reader ModuleName` Effectを
使う。`SchemePass` の `Effects` に追加し、`Driver.hs` で `runReader modName` をラップする。

## Implementation Plan

### Task 1: `compileToScheme` のバグ修正

**Goal**: モジュール名を考慮した正しいmain呼び出し生成

**Scope**: `src/Malgo/Backend/Scheme.hs`

**Dependencies**: なし

**Steps**:
1. `SchemePass` の `Effects` に `Reader ModuleName :> es` を追加
2. `runPassImpl` で `ask @ModuleName` してモジュール名を取得
3. `compileToScheme :: ModuleName -> Join.Program -> Text` にシグネチャ変更
4. 末尾の呼び出しを以下に変更：
   ```haskell
   "(" <> mangleText (moduleNameToString modName <> ".main") <> " malgo-finish)\n"
   ```
5. `schemeRuntime` の `malgo-print-result` は削除せず残す（他用途への互換性）

**Verification**: `mise run test -- --match=Scheme` でユニットテストが通ること

---

### Task 2: `Driver.hs` の更新

**Goal**: SchemePass に `modName` を渡す

**Scope**: `src/Malgo/Driver.hs`

**Dependencies**: Task 1

**Steps**:
1. `TargetScheme` ブランチを以下に変更：
   ```haskell
   TargetScheme -> do
     schemeCode <- runReader modName $ runPass SchemePass core
     liftIO $ putStr $ convertString schemeCode
   ```

**Verification**: `malgo eval --target scheme test/testcases/malgo/Hello.mlg` が実行可能なSchemeコードを出力すること。末尾が `(Hello_dot_mlg_dot_main malgo-finish)` になっていること。

---

### Task 3: `SchemeSpec.hs` のテスト更新

**Goal**: Readerに対応したテスト修正と期待値更新

**Scope**: `test/Malgo/Backend/SchemeSpec.hs`

**Dependencies**: Task 1

**Steps**:
1. `compileTestcaseToScheme` を `runReader renamed.moduleName $ runPass SchemePass program`
   で実装するよう変更（現在は `compileToScheme program` を直接呼び出している）
2. `"malgo-main"` を含む `shouldContain` アサーション（line 86）を削除し、
   モジュール修飾された正しい名前に更新
   - 例: `Test2.mlg` のmain → `"Test2_dot_mlg_dot_main"`
   - 例: `HelloBoxed.mlg` のmain → `"HelloBoxed_dot_mlg_dot_main"`
3. 単体テストの `compileToScheme` 直接呼び出しを `runReader` 付きの `runPass SchemePass` に変更
4. `"malgo-main malgo-print-result"` の存在確認テストを、正しいエントリポイント行の確認に置き換え

**Verification**: `mise run test -- --match=Scheme` で25テストすべて通ること

---

### Task 4: `runtime/malgo/compiler/Scheme.mlg` の削除

**Goal**: 未使用の自己ホスト版Schemeバックエンドを削除

**Scope**: `runtime/malgo/compiler/Scheme.mlg`

**Dependencies**: なし（Main.mlgからインポートされていない）

**Steps**:
1. `runtime/malgo/compiler/Scheme.mlg` を削除

**Verification**: `mise run build` でビルドが通ること（依存がないため影響なし）

---

### Task 5: `scripts/selfhost-golden.sh` の更新

**Goal**: セルフホストテストをScheme経由で実行するよう変更

**Scope**: `scripts/selfhost-golden.sh`

**Dependencies**: Task 1, Task 2, Task 4

**Steps**:

1. スクリプト冒頭に `SCHEME` 変数を追加：
   ```bash
   SCHEME=${SCHEME:-scheme}
   ```

2. precompileリストから `runtime/malgo/compiler/Scheme.mlg` を削除（もし含まれていれば）

3. precompileフェーズ完了後、Main.mlgをSchemeにコンパイル：
   ```bash
   SCHEME_MAIN=".malgo-work/main.scm"
   mkdir -p .malgo-work
   log "compiling Main.mlg to Scheme"
   if ! $MALGO eval --target scheme runtime/malgo/compiler/Main.mlg > "$SCHEME_MAIN"; then
     log "Scheme compilation failed"
     exit 1
   fi
   log "Scheme compilation done"
   ```

4. ゴールデンテスト実行部分を変更：
   ```bash
   # 変更前
   printf 'Hello\n' | timeout "$CASE_TIMEOUT" $MALGO eval runtime/malgo/compiler/Main.mlg "$src" >"$out"

   # 変更後
   printf 'Hello\n' | timeout "$CASE_TIMEOUT" $SCHEME --script "$SCHEME_MAIN" "$src" >"$out"
   ```

**Verification**: `scripts/selfhost-golden.sh` を手動実行してgoldenテストが通ること

---

### Task 6: CI更新

**Goal**: selfhost-goldenジョブにChez Schemeをインストール

**Scope**: `.github/workflows/build.yml`

**Dependencies**: Task 5

**Steps**:
1. `selfhost-golden` ジョブの `Run self-hosted eval goldens` ステップの前に追加：
   ```yaml
   - name: Install Chez Scheme
     run: sudo apt-get install -y chezscheme
   ```

**Verification**: CIのselfhost-goldenジョブが通ること

## Verification

全変更完了後の確認手順：

```bash
# 1. Haskellテスト（SchemeSpecを含む）
mise run test -- --match=Scheme

# 2. Schemeコード生成の手動確認
cabal exec malgo -- eval --target scheme test/testcases/malgo/Hello.mlg > /tmp/hello.scm
scheme --script /tmp/hello.scm   # "Hello, world" が出力されること

# 3. セルフホストgoldenテスト
scripts/selfhost-golden.sh

# 4. 全テスト
mise run test
```

## Risks

| Risk | Mitigation |
|------|------------|
| Chez Schemeのスタック深さ制限による再帰的パーサの失敗 | Chez SchemeはTCO完全実装。ただし非末尾再帰（Parser等）の深い再帰でstackoverflowの可能性。発生したら `(collect)` でGCトリガーを検討 |
| `get-string-all` / `put-string` がChez Schemeのデフォルト環境で利用不可 | Ubuntu上で事前に手動確認。必要なら `(import (rnrs io ports))` を schemeRuntime に追加 |
| `(command-line)` の `--script` モードでの挙動 | Chez Scheme `--script` では `(cdr (command-line))` がスクリプト以降の引数。事前に手動検証済み（User's Guide "Scheme Shell Scripts" セクション） |
| goldenファイルとScheme実行結果の不一致 | main関数の戻り値 `'()` を `malgo-finish` で捨てることで一致するはず。CI失敗時は `mise run reset` でgolden再生成 |
