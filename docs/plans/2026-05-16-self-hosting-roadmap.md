# Malgo セルフホスト ロードマップ

Date: 2026-05-16

## Context

### セルフホストとは

「セルフホスト」とは、言語のコンパイラ/インタプリタを、その言語自身で記述することである。これは言語の成熟度を示す重要なマイルストーンであり、以下の効果をもたらす。

- **ドッグフーディング**: 言語設計の改善点が自然に見えてくる
- **言語の完全性検証**: コンパイラを書けるだけの表現力があることの証明
- **移植性向上**: Haskell 依存を段階的に排除できる
- **エコシステムの拡大**: Malgo で書かれたツールの充実

### 先行事例の教訓

| 言語 | ホスト言語 | 期間 | 戦略 |
|------|-----------|------|------|
| Rust | OCaml | ~2年 | インタプリタ・ブートストラップ |
| Go | C | ~3年 | 段階的書き換え |
| OCaml | C | ~4年 | 段階的書き換え |
| SML/NJ | SML | ~3年 | マルチステージブートストラップ |

関数型言語のセルフホストで共通する知見：
1. 型推論は後回しにできる（最初は型注釈必須でも可）
2. 最小限のサブセットでまずインタプリタを動かす
3. ADT とパターンマッチングが最重要機能
4. ファイル I/O とコマンドライン引数は必須

### 現在の Malgo の状況

**パイプライン:**
```
Source (.mlg)
  → ParserPass → RenamePass → [InferPass] → [RefinePass]
  → ToFunPass → ToCorePass → FlatPass → JoinPass
  → EvalPass（Haskell インタプリタ）
  → [SchemePass]（Chez Scheme コード生成）
```

**実装済み機能（セルフホストに活用できる）:**
- ADT（代数的データ型）とパターンマッチング ✅
- Hindley-Milner 型推論 ✅
- モジュールシステム（import/export）✅
- レコード型・タプル型 ✅
- 高階関数・クロージャ・再帰 ✅
- 文字列操作（length, charAt, substring, append, cons）✅
- 算術・比較演算（Int32/Int64/Float/Double）✅
- I/O: `printString`, `printChar`, `getChar`, `getContents`（stdin全読み）✅
- リスト操作（map, filter, foldl, foldr, zip, take, drop …）✅
- Lazy 評価（ブロック構文 `{expr}`）✅
- コパターン・label/goto・行多相型（実験的）🟡
- Scheme バックエンド（新規実装済み）🟡

---

## セルフホストに必要な未実装機能の分析

### 1. Critical（必須・現在ブロッキング）

#### 1.1 ファイル I/O プリミティブ
**現状:** `getContents`（stdin全読み）と `getChar` のみ。ファイル読み書き不可。  
**必要理由:** コンパイラはソースファイルを読み込む必要がある。  
**必要なもの:**
```malgo
foreign import malgo_read_file   : String# -> String#   -- ファイル全読み込み
foreign import malgo_write_file  : String# -> String# -> ()  -- ファイル書き込み
foreign import malgo_file_exists : String# -> Int32#    -- ファイル存在確認
foreign import malgo_get_line    : () -> String#        -- stdin から1行読み込み
```

#### 1.2 コマンドライン引数
**現状:** `getArgs` 相当の機能が存在しない。  
**必要理由:** `malgo eval foo.mlg` のようにファイル名を受け取る必要がある。  
**必要なもの:**
```malgo
foreign import malgo_get_args : () -> ???  -- List String を返す
```

#### 1.3 整数のパース
**現状:** `toStringInt32`/`toStringInt64` は存在するが、逆方向（文字列→整数）がない。  
**必要理由:** レキサーで数値リテラルをパースする際に必要。  
**必要なもの:**
```malgo
foreign import malgo_string_to_int32 : String# -> Int32#
foreign import malgo_string_to_int64 : String# -> Int64#
```

### 2. Important（品質・実用性に必要）

#### 2.1 `Either`/`Result` 型の標準ライブラリ化
**現状:** `examples/malgo/Either.mlg` に実装例はあるが、Prelude には含まれていない。  
**必要理由:** コンパイラの各パスはエラーを収集・伝播する必要がある。  
**対処:** Prelude に `Either a b = Left a | Right b` と基本操作を追加する。

#### 2.2 連想リストベースの辞書型
**現状:** 辞書/マップに相当するデータ構造が標準ライブラリにない。  
**必要理由:** 変数環境（スコープ）・型環境・名前解決などに必要。  
**対処:** `data Map k v = Empty | Node k v (Map k v) (Map k v)` などをライブラリとして実装。  
型クラスなしでも実装可能（比較関数を引数で渡す）。

#### 2.3 stderr への出力
**現状:** 標準エラー出力がない（全て stdout）。  
**必要理由:** エラーメッセージと正常出力を分離するため。  
**必要なもの:**
```malgo
foreign import malgo_print_string_stderr : String# -> ()
```

#### 2.4 終了コード
**現状:** `exitFailure` はあるが `exitSuccess`/`exitWith` がない。  
**必要理由:** コンパイラとして適切な終了コードを返すため。

### 3. Nice to Have（品質向上・長期的）

#### 3.1 型クラス（Typeclass）
**現状:** 型クラスシステムなし。  
**影響:** `Show`、`Eq`、`Ord` 相当の関数を型ごとに個別定義する必要がある（冗長だが可能）。  
**優先度:** セルフホストの第1段階では不要。後続フェーズで検討。

#### 3.2 可変状態（IORef）
**現状:** 純粋関数型のみ。  
**影響:** コンパイラ内部状態（カウンタ、キャッシュなど）を状態モナドで表現する必要がある。  
**対処:** `label`/`goto` や純粋な関数パッシングで代替可能。

#### 3.3 末尾呼び出し最適化（TCO）
**現状:** Haskell の遅延評価と GHC の最適化に依存している。大きな AST で再帰が深くなるとスタックオーバーフローの懸念。  
**対処:** 反復的アルゴリズム（明示的スタックを使う）で回避可能。

---

## ブートストラップ戦略

**採用戦略: インタプリタ・ブートストラップ**

Malgo には Haskell 製インタプリタが既に存在するため、最も低コストな戦略を採る：

```
Phase 1: Malgo で書いた Malgo インタプリタ
    ↓ 既存の Haskell インタプリタで実行
Phase 2: Scheme バックエンドで Native バイナリ相当を生成
    ↓ Chez Scheme で実行
Phase 3: 完全セルフホスト（Haskell 不要）
```

---

## 実装計画

### Phase 0: ランタイム拡張（推定 1〜2週間）

**目標:** セルフホストに必須の不足プリミティブを Haskell ランタイムに追加する。

- **Scope:** `src/Malgo/Sequent/Eval.hs`、`runtime/malgo/Builtin.mlg`、Haskell ランタイム C コード
- **Dependencies:** なし
- **Steps:**
  1. `malgo_read_file`（C → Haskell FFI）を Eval.hs に追加
  2. `malgo_write_file` を追加
  3. `malgo_get_line` を追加
  4. `malgo_get_args` を追加（`List String` 相当の値を返す）
  5. `malgo_string_to_int32`/`malgo_string_to_int64` を追加
  6. `malgo_print_string_stderr` を追加
  7. `runtime/malgo/Builtin.mlg` に対応する `foreign import` と型安全なラッパーを追加
- **Verification:** `examples/malgo/` に file I/O と getArgs を使う小さなプログラムを作成し `mise run test` で通過を確認

---

### Phase 1: 標準ライブラリ整備（推定 1〜2週間）

**目標:** コンパイラ実装に必要な共通データ構造・モジュールを Malgo で実装する。

- **Scope:** `runtime/malgo/` 以下の新規 `.mlg` ファイル群
- **Dependencies:** Phase 0
- **Steps:**
  1. `runtime/malgo/Either.mlg` — `Either a b` 型と `mapRight`/`bindRight`/`fromRight` などの基本操作
  2. `runtime/malgo/Map.mlg` — 順序付き連想リスト（または AVL 木）として実装。`empty`, `insert`, `lookup`, `delete`, `toList`, `fromList`
  3. `runtime/malgo/Set.mlg` — Map を使った集合
  4. `runtime/malgo/IO.mlg` — `readFile`, `writeFile`, `getLine`, `getArgs` を型安全に束ねたモジュール
  5. `Prelude.mlg` に `Either` と基本的なユーティリティを追加
- **Verification:** 各モジュールに対応するテストケースを `test/testcases/malgo/` に追加

---

### Phase 2: Malgo レキサー（推定 2〜3週間）

**目標:** Malgo のソーステキストをトークン列に変換するレキサーを Malgo で実装する。

- **Scope:** `runtime/malgo/compiler/Lexer.mlg`（新規）
- **Dependencies:** Phase 0, Phase 1
- **主要なトークン型:**
  ```malgo
  data Token
    = TkInt Int64
    | TkFloat Double
    | TkChar Char
    | TkString String
    | TkIdent String       -- 変数・型名
    | TkOperator String    -- 演算子
    | TkKeyword String     -- def, data, type, import, ...
    | TkLParen | TkRParen
    | TkLBrace | TkRBrace
    | TkLBracket | TkRBracket
    | TkComma | TkSemicolon | TkColon | TkArrow | TkEquals
    | TkEOF
    | TkError String       -- 不正なトークン
  
  type TokenWithPos = { token: Token, line: Int32, col: Int32 }
  ```
- **Steps:**
  1. `tokenize : String -> List TokenWithPos` を実装
  2. コメント（`--` 行コメント、`{- -}` ブロックコメント）の処理
  3. 文字列・文字リテラルのエスケープ処理
  4. 数値リテラルのサフィックス（`L`/`l`/`f`/`F`/`#`）処理
  5. 演算子文字列のトークン化
- **Verification:** 既存の `examples/malgo/*.mlg` ファイルをすべてトークン化し、出力を人力確認

---

### Phase 3: Malgo パーサー（推定 3〜4週間）

**目標:** トークン列から Malgo AST を構築する再帰下降パーサーを Malgo で実装する。

- **Scope:** `runtime/malgo/compiler/AST.mlg`、`runtime/malgo/compiler/Parser.mlg`（新規）
- **Dependencies:** Phase 2
- **主要な AST 型:**
  ```malgo
  data Expr
    = EVar String
    | ELit Literal
    | EApp Expr Expr
    | ELam (List Pattern) Expr
    | ELet String Expr Expr
    | ECase Expr (List (Pattern, Expr))
    | ERecord (List (String, Expr))
    | EField Expr String
    | ETuple (List Expr)
    | EBlock Expr                   -- 遅延 {expr}
    | ...
  
  data Decl
    = DData String (List String) (List (String, List Type))
    | DType String (List String) Type
    | DDef String (Maybe Type) Expr
    | DForeign String Type
    | DInfix InfixKind Int32 String
    | DImport (List String) String  -- module {..} = import "path"
  
  data Module = Module (List Decl)
  ```
- **Steps:**
  1. パーサーコンビネータを `Either String (a, List Token)` として実装
  2. 式パーサー（演算子優先順位を Pratt 法で処理）
  3. パターンパーサー
  4. 型パーサー
  5. 宣言パーサー
  6. モジュールパーサー
- **Verification:** `examples/malgo/*.mlg` 全ファイルをパースし、Haskell パーサーの AST と比較

---

### Phase 4: 名前解決パス（推定 2〜3週間）

**目標:** パース済み AST に対して名前解決（スコープ解析・import 展開）を行うパスを Malgo で実装する。

- **Scope:** `runtime/malgo/compiler/Rename.mlg`（新規）
- **Dependencies:** Phase 3
- **Steps:**
  1. 環境型 `Env = Map String String`（ユーザー名 → 内部名）を定義
  2. import 解決（import パスからファイルを読み込み、ロードされた名前を環境に追加）
  3. グローバル・ローカルスコープの管理
  4. 未定義変数のエラー報告（`Either (List Error) Module`）
  5. コンストラクタ・型名の名前解決
- **Verification:** `examples/malgo/*.mlg` 全ファイルを名前解決し、エラーなし

---

### Phase 5: メタ循環評価器（推定 4〜6週間）

**目標:** 名前解決済み AST を評価するインタプリタを Malgo で実装する（メタ循環評価器）。

- **Scope:** `runtime/malgo/compiler/Eval.mlg`（新規）
- **Dependencies:** Phase 4
- **値の表現:**
  ```malgo
  data Value
    = VInt Int64
    | VFloat Double
    | VChar Char
    | VString String
    | VBool Bool
    | VUnit
    | VCon String (List Value)      -- コンストラクタ値
    | VRecord (Map String Value)    -- レコード
    | VTuple (List Value)
    | VFun (Value -> Value)         -- クロージャ（環境キャプチャ）
    | VThunk {Value}                -- 遅延評価
  ```
- **Steps:**
  1. プリミティブ演算（算術・文字列・比較）の実装
  2. `eval : Env -> Expr -> Either Error Value`
  3. パターンマッチング `match : Pattern -> Value -> Maybe Env`
  4. 再帰定義の評価（`fix` を使った自己参照）
  5. モジュールロード（ファイルを読み込み → レキサー → パーサー → 名前解決 → 評価）
  6. foreign import のディスパッチ（組み込み関数テーブル）
- **Verification:** `examples/malgo/*.mlg` を自作インタプリタで実行し、Haskell 版と出力を比較

---

### Phase 6: ブートストラップ検証（推定 2〜3週間）

**目標:** 自作インタプリタ自身を自作インタプリタで動かす（メタ循環評価の成立を確認）。

- **Scope:** すべての `runtime/malgo/compiler/*.mlg`
- **Dependencies:** Phase 5
- **Steps:**
  1. Haskell インタプリタで自作インタプリタを動かし、`examples/malgo/Hello.mlg` を実行
  2. より複雑なプログラム（FizzBuzz, BinaryTree, Fib）で出力比較
  3. 自作インタプリタを使って自作インタプリタ自身を実行（2段階ブートストラップ）
  4. 性能測定と最適化（必要に応じて）
- **Verification:** 少なくとも以下が成立すること：
  ```
  # Stage 0: Haskell インタプリタで Hello.mlg を実行
  malgo eval Hello.mlg  →  "Hello, world!"

  # Stage 1: Haskell インタプリタで自作インタプリタを使い Hello.mlg を実行
  malgo eval runtime/malgo/compiler/Main.mlg -- Hello.mlg  →  "Hello, world!"

  # Stage 2: Stage1 の出力と Stage0 の出力が一致
  diff <(stage0 Hello.mlg) <(stage1 Hello.mlg)  →  空
  ```

---

### Phase 7: Scheme バックエンドによるネイティブ化（推定 4〜8週間）

**目標:** 既存の Scheme バックエンドを使い、自作インタプリタを Chez Scheme を経由して実行可能にする（Haskell 不要）。

- **Scope:** `src/Malgo/Backend/Scheme.hs`、`runtime/malgo/compiler/`
- **Dependencies:** Phase 6、Scheme バックエンドの安定化
- **Steps:**
  1. Scheme バックエンドの不足機能を特定・補完
  2. 自作インタプリタを `--target scheme` でコンパイル
  3. Chez Scheme で生成コードを実行し、動作を確認
  4. CI に Scheme ビルドのテストを追加
- **Verification:**
  ```
  # Scheme で実行した自作インタプリタが Hello.mlg を正しく実行
  chez --script compiled-malgo-interp.scm -- Hello.mlg  →  "Hello, world!"
  ```

---

## 設計上の重要な決定事項

### 型推論を省略する

初期のセルフホスト版インタプリタは **型なし/動的** とする。型注釈はオプションとして無視し、型推論は Phase 7 以降の課題とする。  
**理由:** 型推論（HM 制約解法）の実装は困難であり、まず動くインタプリタを作ることが優先。

### 連想リストを使った単純な環境

型クラスなしでの `Map` の実装は、まず `type Env a = List (String, a)` の連想リストで始め、性能が問題になった時点で AVL 木等に置き換える。

### Foreign import の実装

自作インタプリタでの foreign import の評価は、組み込み関数のディスパッチテーブルで対応する：
```malgo
def evalForeign : String -> List Value -> Value
def evalForeign = { name args ->
  case name {
    "malgo_add_int32_t" -> ...,
    "malgo_print_string" -> ...,
    ...
  }
}
```

### エラーモデル

初期は `Either Error a` を使い、エラーが発生した時点で停止する単純なモデルを採用。後続フェーズでエラー回復と複数エラー収集を追加する。

---

## 検証計画

各フェーズの終了時に以下を実施：

1. `mise run test` で既存テスト 832 件が全パスすること
2. `examples/malgo/*.mlg` の全プログラムが正しく動作すること
3. 各フェーズ固有の新規テストが追加・パスすること

---

## リスク

| リスク | 深刻度 | 対策 |
|--------|--------|------|
| 再帰が深くスタックオーバーフロー | 中 | 明示的スタックによる反復的実装に切り替える |
| getArgs で返す List の表現方法 | 中 | Haskell 側でリスト値を直接構築して返す |
| Scheme バックエンドの未実装機能 | 高 | Phase 7 前に Scheme バックエンドの安定化を先行させる |
| 型クラスなしの冗長なコード | 低 | 「型クラスは後で」と割り切り先へ進む |
| 演算子優先順位の自己定義（infixl/r） | 中 | Pratt パーサーで `infixl`/`infixr` 宣言を動的に扱う |
| モジュールの循環 import | 中 | 最初は循環 import を禁止（エラーにする）で対応 |

---

## タイムライン（概算）

| フェーズ | 内容 | 期間 |
|---------|------|------|
| Phase 0 | ランタイム拡張 | 1〜2週間 |
| Phase 1 | 標準ライブラリ整備 | 1〜2週間 |
| Phase 2 | レキサー | 2〜3週間 |
| Phase 3 | パーサー | 3〜4週間 |
| Phase 4 | 名前解決 | 2〜3週間 |
| Phase 5 | 評価器 | 4〜6週間 |
| Phase 6 | ブートストラップ検証 | 2〜3週間 |
| Phase 7 | Scheme ネイティブ化 | 4〜8週間 |
| **合計** | | **19〜31週間** |

---

## 参考

- [Self-Hosting Compilers - Wikipedia](https://en.wikipedia.org/wiki/Self-hosting_(compilers))
- [Bootstrapping a Compiler - Rust 事例](https://blog.rust-lang.org/2016/01/09/Rust-1.6.html)
- [CakeML - 検証済み ML コンパイラ](https://cakeml.org/)
- `docs/architecture.md` — Malgo のパイプライン詳細
- `todos/malgo-2025.md` — ziku 移植タスクリスト
- `runtime/malgo/Builtin.mlg` — 現在の foreign import 一覧
