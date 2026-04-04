# クエリベースコンパイル + LSP 実装計画

作成日: 2026-04-04

## Context

Malgoコンパイラは現在、`Driver.hs`で線形にパスを連鎖実行する設計。ソースを変更すると全パスを再実行する必要があり、LSP（インクリメンタルな再解析が必要）に不向き。

**目標**: rock/Salsaに倣ったクエリベースコンパイルを自前実装し、既存パイプラインをクエリ化した上で、LSPサーバを構築する。

**設計判断**:
- rockパッケージは使わない（Effectful非互換、GHC 9.12未検証、dependent-hashmap依存が重い）
- rockの設計パターンをEffectfulネイティブで再実装する（コアロジック約300行）
- 既存の`Pass`型クラスと全パス実装は変更しない（ラップのみ）

## Phase 1: クエリフレームワーク基盤

**目標**: `Malgo.Query`モジュールを新設し、Driver.hsをクエリ経由に書き換える。既存テストが全て通ること。

### 新規ファイル

#### `src/Malgo/Query.hs` — クエリ定義

```haskell
data Query a where
  SourceText       :: ModuleName -> Query (FilePath, TL.Text)
  ParsedModule     :: ModuleName -> Query (Module (Malgo Parse))
  BuiltinRnEnv     :: Query RnEnv
  RenamedModule    :: ModuleName -> Query (Module (Malgo Rename), RnState)
  ModuleInterface  :: ModuleName -> Query Interface
  ElaboratedModule :: ModuleName -> Query (BindGroup (Malgo Rename))
  InferredModule   :: ModuleName -> Query (BindGroup (Malgo Rename))
  FunProgram       :: ModuleName -> Query Fun.Program
  JoinProgram      :: ModuleName -> Query Join.Program
  LinkedProgram    :: ModuleName -> Query Join.Program
```

GADT型パラメータで結果の型を表現。DMapは使わず、**クエリ種別ごとに独立した`IORef (Map ModuleName result)`** で実装して`dependent-hashmap`依存を回避する。

#### `src/Malgo/Query/Database.hs` — データベース型

```haskell
data Database = Database
  { sourceCache     :: IORef (Map ModuleName (FilePath, TL.Text))
  , parsedCache     :: IORef (Map ModuleName (Module (Malgo Parse)))
  , renamedCache    :: IORef (Map ModuleName (Module (Malgo Rename), RnState))
  , interfaceCache  :: IORef (Map ModuleName Interface)
  , elaboratedCache :: IORef (Map ModuleName (BindGroup (Malgo Rename)))
  , inferredCache   :: IORef (Map ModuleName (BindGroup (Malgo Rename)))
  , funCache        :: IORef (Map ModuleName Fun.Program)
  , joinCache       :: IORef (Map ModuleName Join.Program)
  , linkedCache     :: IORef (Map ModuleName Join.Program)
  , builtinRnEnvCache :: IORef (Maybe RnEnv)
  }

newDatabase :: MonadIO m => m Database
invalidateModule :: MonadIO m => Database -> ModuleName -> m ()
```

`invalidateModule`は指定モジュールの全キャッシュをクリア + 逆依存モジュールのキャッシュもクリア（Interface.dependenciesから構築）。

#### `src/Malgo/Query/Engine.hs` — QueryDB エフェクトとインタプリタ

```haskell
data QueryDB :: Effect where
  Fetch :: Query a -> QueryDB m a

type instance DispatchOf QueryDB = Dynamic

fetch :: (QueryDB :> es) => Query a -> Eff es a

runQueryDB ::
  ( State Uniq :> es, Reader Flag :> es, Workspace :> es
  , Features :> es, IOE :> es, Error CompileError :> es
  ) => Database -> Eff (QueryDB : es) a -> Eff es a
```

`runQueryDB`の`interpret_`内で各クエリをパターンマッチし、キャッシュチェック→ミス時に`runPass`呼び出し→結果をキャッシュ、という処理。依存クエリは再帰的に`fetch`を呼ぶ。

### 変更ファイル

#### `src/Malgo/Driver.hs` — クエリベースに書き換え

現在の関数を以下のように変更:

- `compileToCore` → 削除。ロジックは`Engine.hs`の各クエリハンドラに分散
- `generateSequent` → 削除。同上
- `linkSequent` → 削除。`LinkedProgram`クエリのハンドラに移動
- `compile` → `Database`を作成し`runQueryDB`内で`fetch (LinkedProgram modName)`を呼ぶ
- `compileFromAST` → `fetch`ベースに書き換え
- `withDump` → そのまま保持

```haskell
compile srcPath = do
  db <- newDatabase
  -- ... ソース読み込み、ArtifactPath解決 ...
  runQueryDB db $ runCompileError do
    updateSource db modName (srcPath, src)
    parsedAst <- fetch (ParsedModule modName)
    -- debugMode dump ...
    case flags.target of
      TargetScheme -> do
        linked <- fetch (LinkedProgram modName)
        schemeCode <- runPass SchemePass linked
        liftIO $ putStr $ convertString schemeCode
      TargetEval -> do
        linked <- fetch (LinkedProgram modName)
        runPass EvalPass (modName, handlers, linked)
```

#### `src/Malgo/Monad.hs` — エフェクトスタック変更なし（Phase 1）

Phase 1では`runMalgoM`は変更しない。`QueryDB`は`Driver.hs`内で`compile`関数がローカルに追加する。`State (Map ModuleName Interface)`は当面維持し、Engine.hs内で`ModuleInterface`クエリが内部的にこのStateも更新する（互換性のため）。

#### `package.yaml` — 新モジュール追加

`exposed-modules`に`Malgo.Query`, `Malgo.Query.Database`, `Malgo.Query.Engine`を追加。新規依存なし。

### Uniq処理

Phase 1では**現状維持**。`State Uniq`は`runMalgoM`のスタックにあり、`runQueryDB`はそのスコープ内で動く。クエリはシーケンシャルに実行されるため、Uniqの値は現在と同じ順序で生成される。

### 検証

```bash
mise run format && mise run test
```

全ゴールデンテストが既存と同一出力であること。テストユーティリティ (`TestUtils.hs`) は変更不要 — `setupBuiltin`等は`Driver.compile`経由で動くため、Driver.hsの内部リファクタリングは透過的。

---

## Phase 2: Interface読み込みのクエリ化

**目標**: `State (Map ModuleName Interface)`を`ModuleInterface`クエリで完全に置き換える。

### 変更ファイル

#### `src/Malgo/Interface.hs`

`loadInterface`を2つの実装に分割:
- `loadInterfaceFromDisk`: 現在の実装（ディスクから`.mlgi`をロード）
- `loadInterface`: `QueryDB`がスコープにある場合は`fetch (ModuleInterface modName)`を使う

あるいは、`loadInterface`が`State (Map ModuleName Interface)`を引き続き使い、Engine.hs側で`ModuleInterface`クエリが`.mlgi`ロード→Stateにも反映するパターン（Phase 1と同じ）を維持しつつ、Stateをクエリキャッシュに一本化。

#### `src/Malgo/Monad.hs`

`State (Map ModuleName Interface)`をエフェクトスタックから削除。代わりに`Database`のinterfaceCacheがその役割を担う。

#### `src/Malgo/Rename/Pass.hs`

RenamePass内の`loadInterface`呼び出しがQueryDB経由になる。Effectsの制約から`State (Map ModuleName Interface)`を削除し、`QueryDB`を追加。

### 検証

```bash
mise run format && mise run test
```

---

## Phase 3: LSPサーバ（MVP）

**目標**: Diagnostics, Go to Definition, Hover を提供するLSPサーバ。

### 新規ファイル

#### `app/malgo-lsp/Main.hs` — エントリポイント

```haskell
main :: IO ()
main = do
  db <- newDatabase
  runServer (serverDefinition db)
```

#### `src/Malgo/LSP.hs` — サーバ定義

```haskell
data ServerState = ServerState
  { database :: Database
  , flag     :: Flag
  }

serverDefinition :: Database -> ServerDefinition ServerState
```

`lsp`ライブラリの`ServerDefinition`に各ハンドラを登録。

#### `src/Malgo/LSP/Diagnostics.hs`

`CompileError` → LSP `Diagnostic`変換。`Range` → LSP `Position` (0-indexed) 変換。

```haskell
toDiagnostics :: CompileError -> [Diagnostic]
rangeToLspRange :: Range -> LSP.Range
```

#### `src/Malgo/LSP/Handlers.hs`

- **textDocument/didOpen, didChange**: `updateSource` → `invalidateModule` → `fetch (RenamedModule modName)` を`try`で実行 → エラーをDiagnosticsとして発行
- **textDocument/definition**: `fetch (RenamedModule modName)` → RnStateから名前解決情報を取得 → 定義位置を返す
- **textDocument/hover**: `fetch (RenamedModule modName)` → カーソル位置のRnIdを特定 → モジュール名・種別を表示

### 変更ファイル

#### `package.yaml`

```yaml
executables:
  malgo-lsp:
    main: Main.hs
    source-dirs: app/malgo-lsp/
    ghc-options: [-threaded, -rtsopts, -with-rtsopts=-N]
    dependencies:
      - malgo
      - lsp >= 2.4
      - lsp-types
```

#### `src/Malgo/Query/Engine.hs`

`updateSource`関数を追加: ソーステキストをDBに書き込み＋当該モジュールのキャッシュを無効化。

### 検証

1. `mise run test` — 既存テスト通過
2. 手動テスト: VS Code + generic LSP client拡張で`.mlg`ファイルを開き、構文エラーのDiagnosticsが表示されること
3. 将来: LSPメッセージを標準入出力で送受信する統合テスト

---

## Phase 4: インクリメンタル再計算（将来）

Phase 1-3完了後のオプション。

- 依存関係の自動追跡（`fetch`呼び出し時に記録）
- Early Cutoff（パース結果が同一ならRename再実行をスキップ）
- `Uniq`をIORefベースに変更（並列クエリ実行への準備）
- 逆依存グラフ構築（モジュールA変更時に、Aをimportするモジュール群を自動無効化）

---

## 変更しないファイル一覧

以下は全Phase通じて変更なし:

- `src/Malgo/Parser.hs`, `src/Malgo/Parser/*.hs`
- `src/Malgo/Elaborate.hs`
- `src/Malgo/Infer.hs`, `src/Malgo/Infer/*.hs`
- `src/Malgo/Sequent/ToFun.hs`, `src/Malgo/Sequent/ToCore.hs`
- `src/Malgo/Sequent/Core/Flat.hs`, `src/Malgo/Sequent/Core/Join.hs`
- `src/Malgo/Sequent/Eval.hs`, `src/Malgo/Sequent/BigStepEval.hs`
- `src/Malgo/Backend/Scheme.hs`
- `src/Malgo/Syntax.hs`, `src/Malgo/Syntax/*.hs`
- `src/Malgo/Id.hs`, `src/Malgo/Prelude.hs`
- `src/Malgo/Pass.hs`
- `src/Malgo/Features.hs`
- `test/` 以下全ファイル

## リスク

| リスク | 対策 |
|--------|------|
| Uniq値が変わりゴールデンテスト失敗 | Phase 1ではクエリ実行順序を現パイプラインと同一に保つ |
| 循環クエリ依存 | Malgoは循環importを禁止。実行時に処理中クエリのSetで検出 |
| LSPライブラリのGHC 9.12互換性 | Phase 3開始前にHackageで確認。問題あればGHCバージョン調整 |
| テストユーティリティの互換性 | `TestUtils.hs`は`Driver.compile`経由→Driverの内部変更は透過的 |

## 参考資料

- [Three Architectures for a Responsive IDE](https://rust-analyzer.github.io/blog/2020/07/20/three-architectures-for-responsive-ide.html) (matklad)
- [Query-Based Compilers](https://ollef.github.io/blog/posts/query-based-compilers.html) (Olle Fredriksson)
- [Build Systems a la Carte](https://www.microsoft.com/en-us/research/publication/build-systems-la-carte/) (Mokhov, Mitchell, Peyton Jones, 2018)
- [rock on Hackage](https://hackage.haskell.org/package/rock)
- [Salsa Book](https://salsa-rs.github.io/salsa/)
