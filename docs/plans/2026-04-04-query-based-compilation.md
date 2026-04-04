# クエリベースコンパイルとLSPサーバの導入

Date: 2026-04-04

## Context

### 現在の問題

Malgoコンパイラの`Driver.hs`は線形なパスチェーンで構成されている:

```
compile → ParserPass → RenamePass → [ElaboratePass] → [InferPass]
        → ToFunPass → ToCorePass → FlatPass → JoinPass → linkSequent → Eval/Scheme
```

この設計では:
- ソース変更時に全パスを最初から再実行する必要がある
- モジュール間の依存関係が暗黙的（`linkSequent`で後付けマージ）
- LSPのようなインクリメンタルな解析ができない

### 目標

1. rock/Salsaパターンに倣ったクエリベースコンパイルを**Effectfulネイティブで自前実装**
2. 現在のDriver.hsをクエリ経由のパイプラインに移行（既存Passは変更なし）
3. クエリDBをバックエンドとしたLSPサーバを構築

### 現行アーキテクチャの詳細

**エフェクトスタック** (`Monad.hs:11-29`):
```haskell
runMalgoM :: Flag -> Eff '[Reader Flag, State Uniq, State (Map ModuleName Interface),
                           Features, State Pragma, Workspace, IOE] b -> IO b
```

**Pass型クラス** (`Pass.hs:28-44`):
```haskell
class (Exception (ErrorType pass)) => Pass pass where
  type Input pass; type Output pass; type ErrorType pass
  type Effects pass (es :: [Effect]) :: Constraint
  runPass :: (Effects pass es, Error CompileError :> es) => pass -> Input pass -> Eff es (Output pass)
```

**共有状態**:
- `State Uniq`: グローバル一意ID生成。全パスが使用（Rename, Elaborate, Infer, ToFun, ToCore等）
- `State (Map ModuleName Interface)`: インターフェースキャッシュ。Rename時に構築・参照
- `Workspace`: IORefベースのモジュールパスキャッシュ（`.malgo-work/`管理）

**テスト** (`TestUtils.hs`):
- `setupBuiltin`/`setupPrelude`は`Driver.compile`を経由
- `setupEvalBuiltin`等は`runPass`を直接呼ぶ独自パイプライン
- ゴールデンテスト(hspec-golden)で各パス出力を検証

## Design Choices

### 1. rockパッケージを使わず自前実装

**理由**:
- rockの`Task f a = ReaderT (Fetch f) IO a`はEffectfulの`Eff`と非互換
- `dependent-hashmap`, `dependent-sum`等の重い依存が必要
- GHC 9.12での動作未検証、メンテナンスが不活発（最終リリース2023-10）

**代替案**: rockのコアロジック（約300行）をEffectful上に再実装。設計パターン（GADTクエリ、メモ化、依存追跡）は踏襲する。

### 2. クエリ種別ごとに独立した`IORef (Map ModuleName result)`でキャッシュ

**理由**: `DMap`（dependent map）を避け、標準の`Map`とIORefだけで実装。型安全性はクエリエンジンのパターンマッチで保証。

**却下案**: `dependent-hashmap`を使ったヘテロジニアスなキャッシュ。型安全だがボイラープレートが多く、GHC 9.12互換性のリスクがある。

### 3. Phase 1では`State Uniq`を変更しない

**理由**: クエリをシーケンシャルに実行する限り、Uniq値は現パイプラインと同一順序で生成される。ゴールデンテストが回帰テストとして機能する。

並列クエリ実行が必要になるPhase 4でIORefベースに移行する。

### 4. 既存Passの実装は変更しない

**理由**: 変更のblast radiusを最小化。クエリエンジンが各PassのInput/Outputを繋ぐ「グルーコード」の役割を担う。テストユーティリティの`setupEvalBuiltin`等がPassを直接呼ぶコードも影響を受けない。

## Implementation Plan

### Task 1: クエリデータベースとエンジンの実装

- **Goal**: `Malgo.Query`, `Malgo.Query.Database`, `Malgo.Query.Engine`モジュールを新設
- **Scope**: 新規ファイルのみ
  - `src/Malgo/Query.hs`
  - `src/Malgo/Query/Database.hs`
  - `src/Malgo/Query/Engine.hs`
- **Dependencies**: なし
- **Steps**:
  1. `Query` GADTを定義（全クエリ種別）
  2. `Database`型を定義（クエリ種別ごとのIORefキャッシュ）
  3. `newDatabase`関数: 全キャッシュを空で初期化
  4. `invalidateModule`関数: 指定モジュールの全キャッシュをクリア
  5. `QueryDB`エフェクトを定義（Dynamic dispatch）
  6. `fetch`関数: `send (Fetch query)`のラッパー
  7. `runQueryDB`関数: `interpret_`で各クエリをハンドル
     - キャッシュチェック→ミス時に`runPass`呼び出し→結果をキャッシュ
     - 依存クエリは再帰的に`fetch`
     - `RenamedModule`クエリでは`buildInterface`→`.mlgi`保存も実行
     - `LinkedProgram`クエリではdependency setの.sqtをロードしてマージ
  8. `updateSource`関数: ソーステキストをDBに登録（LSP用）
- **Verification**:
  - `mise run build`でコンパイルが通ること
  - Engine.hsのクエリハンドラが現行Driver.hsの`compileToCore`/`generateSequent`/`linkSequent`と同等のロジックを持つこと（コードレビューで確認）

### Task 2: Driver.hsのクエリベース書き換え

- **Goal**: Driver.hsを`fetch`ベースに書き換え、既存テストが全て通ること
- **Scope**:
  - `src/Malgo/Driver.hs` — 大幅書き換え
  - `package.yaml` — 新モジュールをexposed-modulesに追加
- **Dependencies**: Task 1
- **Steps**:
  1. `compile`関数を書き換え:
     - `newDatabase`でDB作成
     - `runQueryDB db`でクエリスコープに入る
     - `updateSource`でソースをDBに登録
     - `fetch (ParsedModule modName)`でパース
     - `fetch (LinkedProgram modName)`でリンク済みプログラム取得
     - `runPass SchemePass`/`EvalPass`でコード生成・実行（これらはクエリ化しない — 副作用のため）
  2. `compileFromAST`を書き換え:
     - `compileToCore`の代わりに`fetch (LinkedProgram modName)`
  3. `compileToCore`, `generateSequent`, `linkSequent`を削除
     - ロジックはTask 1でEngine.hsに移動済み
  4. `withDump`は保持（デバッグ出力用）
  5. package.yamlに`Malgo.Query`, `Malgo.Query.Database`, `Malgo.Query.Engine`を追加
  6. `Monad.hs`は**変更しない** — `QueryDB`はDriver内でローカルに追加
- **Verification**:
  ```bash
  mise run format && mise run test
  ```
  全ゴールデンテスト通過。出力がリファクタリング前と同一であること。

  **注意**: `TestUtils.hs`の`setupEvalBuiltin`/`setupEvalPrelude`/`compileTestCase`は`runPass`を直接呼ぶため、Driver.hsの変更に影響されない。`setupBuiltin`/`setupPrelude`は`Driver.compile`経由だが、compile関数のAPIシグネチャは変わらないため互換。

### Task 3: Interfaceキャッシュのクエリ統合

- **Goal**: `State (Map ModuleName Interface)`を`ModuleInterface`クエリに一本化
- **Scope**:
  - `src/Malgo/Interface.hs`
  - `src/Malgo/Monad.hs`
  - `src/Malgo/Rename/Pass.hs`
  - `src/Malgo/Query/Engine.hs`
  - `test/Malgo/TestUtils.hs`（エフェクトスタック変更の影響）
- **Dependencies**: Task 2
- **Steps**:
  1. `Interface.hs`: `loadInterface`の型シグネチャを変更
     - `State (Map ModuleName Interface)`制約を`QueryDB`制約に変更
     - 内部で`fetch (ModuleInterface modName)`を使用
     - QueryDBがスコープにない場合（テストユーティリティ等）のために、`loadInterfaceFromDisk`（現行実装）も残す
  2. `Monad.hs`: `State (Map ModuleName Interface)`をエフェクトスタックから削除
     - `runMalgoM`のスタックを更新
  3. `Rename/Pass.hs`: `RenamePass`の`Effects`制約から`State (Map ModuleName Interface)`を削除、`QueryDB`を追加
  4. `Engine.hs`: `runQueryDB`のエフェクト制約から`State (Map ModuleName Interface)`を削除
  5. `TestUtils.hs`: `runMalgoM`のスタック変更に合わせて更新
     - `setupEvalBuiltin`等がRenamePassを直接呼ぶ箇所: QueryDBなしで動くように`loadInterfaceFromDisk`を使うか、テスト内でもQueryDBを使うか決定
- **Verification**:
  ```bash
  mise run format && mise run test
  ```

### Task 4: LSPサーバ（MVP）

- **Goal**: Diagnostics, Go to Definition, Hover を提供するLSPサーバ
- **Scope**:
  - `src/Malgo/LSP.hs` (新規)
  - `src/Malgo/LSP/Diagnostics.hs` (新規)
  - `src/Malgo/LSP/Handlers.hs` (新規)
  - `app/malgo-lsp/Main.hs` (新規)
  - `package.yaml` — `malgo-lsp`実行ファイル、`lsp`/`lsp-types`依存追加
- **Dependencies**: Task 3
- **Steps**:
  1. `package.yaml`に`malgo-lsp`実行ファイルと依存を追加
  2. `LSP/Diagnostics.hs`: `CompileError` → LSP `Diagnostic`変換
     - `Range`(1-indexed SourcePos) → LSP `Range`(0-indexed Position)
     - パースエラー、リネームエラーの変換
  3. `LSP/Handlers.hs`:
     - `didOpen`/`didChange`: `updateSource` → `invalidateModule` → `fetch (RenamedModule modName)` を`try`で実行 → Diagnostics発行
     - `definition`: RenamedModuleのRnEnvから名前解決情報を取得 → 定義位置を返す
     - `hover`: カーソル位置のRnIdを特定 → モジュール名・Id種別を表示
  4. `LSP.hs`: `ServerDefinition`を構成、ハンドラを登録
  5. `app/malgo-lsp/Main.hs`: エントリポイント
  6. `mise run build`で`malgo-lsp`がビルドできること確認
- **Verification**:
  1. `mise run test` — 既存テスト通過
  2. `malgo-lsp`をVS Code + generic LSP clientで起動し、`.mlg`ファイルの:
     - 構文エラー → Diagnostics表示
     - 識別子にカーソル → Hover情報表示
     - Go to Definition → 定義元にジャンプ

## Verification

全タスク完了後の統合検証:

```bash
mise run format && mise run test
```

加えて:
- `malgo eval examples/malgo/Hello.mlg` — バッチコンパイルが正常動作
- `malgo eval --target scheme examples/malgo/Hello.mlg` — Scheme出力が正常
- `malgo-lsp` — LSPプロトコルでの対話的操作

## Risks

| Risk | Mitigation |
|------|------------|
| Uniq値の変化でゴールデンテスト失敗 | Task 2ではクエリ実行順序を現パイプラインと同一に保つ。テスト全通過を確認 |
| 循環クエリ依存 | Malgoは循環importを禁止。Engine.hsに処理中クエリのSetで循環検出を入れる |
| `lsp`パッケージのGHC 9.12非互換 | Task 4開始前にHackageで確認。問題あればcabal.project.freezeで互換バージョンを固定 |
| TestUtils.hsのエフェクトスタック変更 | Task 3で`loadInterfaceFromDisk`を残し、テスト側は`QueryDB`なしでも動くようにする |
| メモリ増加（キャッシュ蓄積） | バッチコンパイルではDB作成→破棄で問題なし。LSPは将来Phase 4でGC検討 |
