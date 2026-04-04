# クエリベースコンパイル導入計画

作成日: 2026-04-02

## 目標

1. クエリベースコンパイルのアルゴリズムを導入する
2. 現在のインタプリタ(パイプライン)をクエリベースに移行する
3. その上にLSPを構築する
4. 将来のネイティブコンパイラにも対応できる基盤とする

## 背景: 現在のアーキテクチャの問題

現在のパイプライン (`Driver.hs`) は**線形なパス連鎖**:

```
Source → Parse → Rename → [Elaborate] → [Infer] → ToFun → ToCore → Flat → Join → Eval
```

**問題点**:
- パスが逐次実行され、インクリメンタルな再実行ができない
- 共有状態 (`State Uniq`, `State (Map ModuleName Interface)`, `Workspace`) がグローバルに散在
- モジュールリンクがコンパイル後の後処理 (`linkSequent`)
- どのパスがどの成果物に依存するか追跡できない
- ソース変更時に全パスを再実行する必要がある → LSPに不向き

## 設計方針

### クエリベースコンパイルとは

rust-analyzer / Salsa で実績のあるアプローチ:

- コンパイルの各ステップを**クエリ (query)** として定義
- クエリは入力から出力への**純粋な関数** (+ メモ化)
- クエリの結果は**データベース**にキャッシュされる
- 入力が変わったクエリだけを**再計算** (インクリメンタル)
- クエリ間の**依存関係**を自動追跡

### Malgoへの適用

各パスを独立したクエリに変換する:

```haskell
-- 入力クエリ (外部からセットされる)
sourceText   :: ModuleName -> Query Text
featureFlags :: Query FeatureFlags

-- 派生クエリ (他のクエリから計算される)
parsedModule   :: ModuleName -> Query (Module (Malgo Parse))
renamedModule  :: ModuleName -> Query (Module (Malgo Rename), RnState)
interface      :: ModuleName -> Query Interface
elaborated     :: ModuleName -> Query (BindGroup (Malgo Rename))
inferred       :: ModuleName -> Query (BindGroup (Malgo Rename))
funProgram     :: ModuleName -> Query Fun.Program
joinProgram    :: ModuleName -> Query Join.Program
linkedProgram  :: ModuleName -> Query Join.Program
```

## 段階的移行計画

### Phase 0: 準備 — クエリデータベースの実装

**目標**: Salsa風のクエリフレームワークをMalgo内に構築する。

**やること**:

1. **Query型の定義**
   ```haskell
   -- クエリキー: 何を計算するかを表す
   data QueryKey
     = SourceTextQ ModuleName
     | ParseQ ModuleName
     | RenameQ ModuleName
     | InterfaceQ ModuleName
     | ElaborateQ ModuleName
     | InferQ ModuleName
     | ToFunQ ModuleName
     | JoinQ ModuleName
     | LinkQ ModuleName
     deriving (Eq, Ord, Show)
   ```

2. **クエリデータベース**
   ```haskell
   data QueryDB = QueryDB
     { cache     :: IORef (Map QueryKey CachedValue)
     , revisions :: IORef (Map QueryKey Revision)
     , deps      :: IORef (Map QueryKey (Set QueryKey))
     }
   
   data CachedValue = forall a. (Typeable a) => CachedValue a Revision
   type Revision = Int
   ```

3. **メモ化とインバリデーション**
   ```haskell
   query :: (Typeable a) => QueryKey -> (QueryDB -> IO a) -> QueryDB -> IO a
   -- 1. キャッシュにあり、依存先が変わっていなければキャッシュを返す
   -- 2. なければ計算し、依存関係を記録し、結果をキャッシュ
   
   invalidate :: QueryKey -> QueryDB -> IO ()
   -- 1. 指定キーのリビジョンをインクリメント
   -- 2. 依存しているキーを再帰的にインバリデート
   ```

**新規ファイル**: `src/Malgo/Query.hs`

**設計判断**:
- 外部ライブラリ (rock, salsa-like) を使うか、自前実装か
  - `rock` パッケージ: Haskellでのクエリベースコンパイルライブラリ、実績あり
  - 自前実装: Malgoの規模なら十分実装可能、依存を増やさない
  - → **まず自前で最小限を実装し、必要に応じて`rock`に移行**を推奨

---

### Phase 1: パスのクエリ化 — 既存パスをクエリでラップ

**目標**: 既存の `Pass` 実装をそのまま保持しつつ、Driver.hsをクエリ経由の呼び出しに置き換える。

**やること**:

1. **各パスをクエリ関数でラップ**
   ```haskell
   -- Before (Driver.hs):
   parsed <- runPass ParserPass (srcPath, sourceText)
   (renamed, rnState) <- runPass RenamePass (parsed, rnEnv)
   
   -- After:
   parsed <- queryParsedModule db moduleName
   (renamed, rnState) <- queryRenamedModule db moduleName
   ```

2. **入力クエリの実装**
   ```haskell
   querySourceText :: QueryDB -> ModuleName -> IO Text
   querySourceText db modName = query (SourceTextQ modName) $ \_ -> do
     path <- resolveModulePath modName
     T.readFile (toFilePath path)
   ```

3. **Driver.hsの書き換え**
   - `compile` / `compileFromAST` をクエリベースに
   - `generateSequent` をクエリの連鎖に分解
   - `linkSequent` をクエリ化

4. **Uniq生成の局所化**
   - 現在: グローバルな `State Uniq` を全パスが共有
   - 変更: 各クエリが独自のUniqカウンタを持つ (モジュール名ベースのネームスペース)
   - or: QueryDB内にUniq生成器を持たせる

**影響範囲**:
- `src/Malgo/Driver.hs` — 大幅書き換え
- `src/Malgo/Monad.hs` — エフェクトスタックの変更
- 各Pass実装 — **変更なし** (ラップするだけ)

---

### Phase 2: インクリメンタル対応

**目標**: ソース変更時に必要なクエリだけを再計算する。

**やること**:

1. **ソース変更通知**
   ```haskell
   setSourceText :: QueryDB -> ModuleName -> Text -> IO ()
   setSourceText db modName text = do
     updateInput db (SourceTextQ modName) text
     invalidate (SourceTextQ modName) db
   ```

2. **依存関係の自動追跡**
   - クエリ実行中に他のクエリを呼ぶと、依存関係として記録
   - Reader効果やIORefで実現

3. **Early Cutoff最適化**
   - パースの結果が以前と同じなら、Renameを再実行しない
   - 結果のハッシュ比較で判断

**テスト**:
- 単一ファイル変更時に、変更モジュールのパスだけ再実行されることを確認
- 依存モジュールが変わっていなければ、依存先のリネームは再実行されないことを確認

---

### Phase 3: LSP統合

**目標**: クエリDBをLSPサーバのバックエンドとして使用する。

**やること**:

1. **LSPサーバ骨格** (`src/Malgo/LSP.hs`, `app/malgo-lsp/Main.hs`)
   - `lsp` パッケージを使用
   - QueryDBをサーバ状態として保持

2. **Diagnostics**
   ```haskell
   getDiagnostics :: QueryDB -> ModuleName -> IO [Diagnostic]
   getDiagnostics db modName = do
     -- 各クエリを試行し、エラーをDiagnosticに変換
     parseResult <- try $ queryParsedModule db modName
     case parseResult of
       Left err -> pure [parseToDiagnostic err]
       Right _ -> do
         renameResult <- try $ queryRenamedModule db modName
         ...
   ```

3. **textDocument/didChange**
   ```haskell
   onDidChange :: QueryDB -> Uri -> Text -> IO ()
   onDidChange db uri newText = do
     let modName = uriToModuleName uri
     setSourceText db modName newText
     -- Diagnosticsを再計算して送信
     diags <- getDiagnostics db modName
     publishDiagnostics uri diags
   ```

4. **Go to Definition**
   - RenamePassの名前解決情報をクエリから取得
   - `RnState` のマッピング (PsId → RnId with Range) を利用

5. **Hover (型情報)**
   - InferPassの型情報をクエリから取得
   - Range → Type のマッピングを構築

6. **Completion**
   - RnEnvのスコープ情報をクエリから取得

**新規ファイル**:
- `src/Malgo/LSP.hs` — LSPハンドラ
- `src/Malgo/LSP/Diagnostics.hs` — エラー→Diagnostic変換
- `src/Malgo/LSP/Definition.hs` — Go to Definition
- `src/Malgo/LSP/Hover.hs` — Hover
- `app/malgo-lsp/Main.hs` — LSPサーバエントリポイント

**package.yaml変更**:
- `lsp`, `lsp-types` 依存追加
- `malgo-lsp` 実行ファイル追加

---

## 既存型との対応

| 現在 | クエリベース後 |
|------|----------------|
| `State (Map ModuleName Interface)` | `InterfaceQ :: ModuleName -> Query Interface` |
| `Workspace` (IORef) | `ModulePathQ :: ModuleName -> Query FilePath` |
| `State Uniq` (グローバル) | QueryDB内のカウンタ or パスローカル |
| `State Pragma` | `PragmaQ :: ModuleName -> Query Pragma` |
| `Reader Flag` | QueryDB初期化時に設定 |
| `.malgo-work/*.mlgi` ファイルI/O | `InterfaceQ` のキャッシュ |
| `.malgo-work/*.sqt` ファイルI/O | `JoinQ` のキャッシュ |

## リスクと対策

| リスク | 対策 |
|--------|------|
| Uniqの衝突 (パスが独立するとID空間が重複) | モジュール名+パス名でプレフィクスを付ける or QueryDB内で一元管理 |
| エフェクトスタックの大幅変更 | Phase 1で既存Passは変更せずラップするだけにする |
| パフォーマンス劣化 (キャッシュオーバーヘッド) | Phase 2のEarly Cutoffで軽減。ベンチマーク必須 |
| 循環依存 (相互再帰モジュール) | 現在も非対応なので後回し |

## 未決定事項

- [ ] クエリフレームワーク: 自前実装 vs `rock` パッケージ
- [ ] Uniq管理方式: QueryDB一元管理 vs パスローカル + ネームスペース
- [ ] LSPライブラリ: `lsp` (haskell-language-server由来) の具体バージョン
- [ ] エラーリカバリ: パースエラー時にどこまで部分的な結果を返すか
- [ ] ファイル監視: LSPのdidChangeで十分か、fsnotify併用か
