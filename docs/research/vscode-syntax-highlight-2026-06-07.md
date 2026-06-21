# VS Code シンタックスハイライト実装リサーチ

**日付**: 2026-06-07  
**対象**: Malgo 言語向け VS Code 拡張のシンタックスハイライト実装方法  
**調査規模**: 102エージェント、25クレーム検証（13確定 / 12否決）、20ソース

---

## 概要

VS Code カスタム言語のシンタックスハイライトは、**TextMate 文法**（.tmLanguage.json）と **LSP セマンティックトークン**の2層構造で実装する。
両者は排他ではなく加算的に共存し、セマンティックトークンが TextMate ハイライトの上に重ねて適用される。

---

## 1. TextMate 文法（.tmLanguage.json）

### スコープ名の命名規則 [confidence: high, vote: 2-1]

- ルートスコープは `source.malgo`（`source.` プレフィックス + 言語名末尾）
- 一般規則: `text` または `source` を最初に置き、言語名を末尾に置く
- 例外: HTML 埋め込み言語は `text.html.markdown` のように中間に置く

出典: [TextMate 公式マニュアル](https://macromates.com/manual/en/language_grammars)

### begin/end ルールペア [confidence: high, vote: 3-0]

- `begin`/`end` ペアで**複数行構造**を表現できる
- `begin` パターンのキャプチャを `end` パターン内で後方参照（`\1` 等）できる
- 「正規表現は1行しか扱えない」は**誤り**（否決）

```json
{
  "name": "string.quoted.double.malgo",
  "begin": "\"",
  "end": "\""
}
```

ヒアドキュメント例: `begin: "<<(\\w+)"` / `end: "^\\1$"`

### Malgo 向けスコープ設計（推奨）

```
source.malgo
  keyword.other.malgo          → def data foreign import module infix* let type with forall exists class impl goto label
  keyword.control.malgo        → if then else
  storage.type.malgo           → 型名（大文字始まり識別子）
  entity.name.function.malgo   → 関数定義名
  entity.name.type.malgo       → data/type 宣言の型名
  variable.other.malgo         → 小文字始まり識別子
  constant.language.malgo      → True False Nothing
  constant.numeric.malgo       → 数値リテラル（整数・浮動小数・アンボックス）
  string.quoted.double.malgo   → 文字列リテラル "..."
  string.quoted.single.malgo   → 文字リテラル '.'
  comment.line.double-dash.malgo   → -- コメント
  comment.block.malgo          → {- ... -} ブロックコメント
  punctuation.definition.malgo → { } ( ) [ ]
  keyword.operator.malgo       → 演算子
```

---

## 2. VS Code 拡張の package.json 登録 [confidence: high, vote: 3-0]

### 基本構造

```json
{
  "name": "malgo-language",
  "displayName": "Malgo Language Support",
  "contributes": {
    "languages": [
      {
        "id": "malgo",
        "aliases": ["Malgo", "malgo"],
        "extensions": [".mlg"],
        "configuration": "./language-configuration.json"
      }
    ],
    "grammars": [
      {
        "language": "malgo",
        "scopeName": "source.malgo",
        "path": "./syntaxes/malgo.tmLanguage.json"
      }
    ]
  }
}
```

### セマンティックトークン用 contribution points [confidence: high, vote: 3-0]

セマンティックトークンには3つの contribution point が存在する:

```json
{
  "contributes": {
    "semanticTokenTypes": [
      { "id": "typeConstructor", "description": "型コンストラクタ" }
    ],
    "semanticTokenModifiers": [
      { "id": "unboxed", "description": "アンボックス型" }
    ],
    "semanticTokenScopes": [
      {
        "language": "malgo",
        "scopes": {
          "function": ["entity.name.function.malgo"],
          "type": ["storage.type.malgo"],
          "variable": ["variable.other.malgo"]
        }
      }
    ]
  }
}
```

`semanticTokenScopes` はテーマ互換性のためのフォールバック機構で、セマンティックトークンを TextMate スコープにマッピングする。

### language-configuration.json

```json
{
  "comments": {
    "lineComment": "--",
    "blockComment": ["{-", "-}"]
  },
  "brackets": [
    ["{", "}"],
    ["(", ")"],
    ["[", "]"]
  ],
  "autoClosingPairs": [
    { "open": "{", "close": "}" },
    { "open": "(", "close": ")" },
    { "open": "\"", "close": "\"" }
  ]
}
```

---

## 3. LSP セマンティックトークン実装

### Haskell lsp ライブラリ [confidence: high, vote: 3-0]

- リポジトリ: https://github.com/haskell/lsp
- 3パッケージ構成:
  - `lsp` (v2.8.0.0) — サーバー構築ライブラリ
  - `lsp-types` (v2.4.0.0) — 型安全な LSP 型定義
  - `lsp-test` — 機能テストフレームワーク
- **LSP 3.16 以降のセマンティックトークンをサポート**（「3.15のみ」は否決）

### サーバー実装パターン

`textDocument/semanticTokens/full` ハンドラーを `ServerDefinition` に登録する:

```haskell
-- package.yaml に lsp, lsp-types を追加
-- セマンティックトークンのレジェンドを定義
semanticTokensLegend :: SemanticTokensLegend
semanticTokensLegend = SemanticTokensLegend
  { _tokenTypes     = ["function", "type", "variable", "parameter", "typeParameter"]
  , _tokenModifiers = ["declaration", "definition"]
  }

-- ServerCapabilities に登録
serverCapabilities :: ServerCapabilities
serverCapabilities = def
  { _semanticTokensProvider = Just $ InL $ SemanticTokensOptions
      { _legend    = semanticTokensLegend
      , _range     = Just $ InL False
      , _full      = Just $ InL True
      , _workDoneProgress = Nothing
      }
  }

-- ハンドラー実装
handleSemanticTokensFull :: Uri -> LspM Config (Either ResponseError SemanticTokens)
handleSemanticTokensFull uri = do
  -- AST から (TokenType, line, startChar, length, modifiers) を計算
  -- lsp-types の makeSemanticTokens で encode
  pure $ Right $ SemanticTokens Nothing (encodeTokens tokens)
```

### HLS hls-semantic-tokens-plugin（参考実装）[confidence: medium, vote: 2-1]

- PR: https://github.com/haskell/haskell-language-server/pull/3892（2024年1月マージ）
- 実装パターン:
  1. `GetHieAst` で型付き構文木を取得
  2. `NameSemanticMap` を構築（名前 → トークン型のマッピング）
  3. `(Name, Span)` ペアを走査して semantic tokens を生成
- Malgo への適用: Rename パスの成果物（`RenamedAST`）から同様のマッピングを構築可能

### VS Code クライアント側 [confidence: high, vote: 3-0]

```typescript
// LSP経由なら vscode-languageclient が自動的にセマンティックトークンを有効化する
// package.json の engines.vscode は 1.43.0 以上を指定
vscode.languages.registerDocumentSemanticTokensProvider(
  { language: 'malgo' },
  provider,
  legend
)
```

---

## 4. TextMate 文法 ↔ セマンティックトークンの関係 [confidence: high, vote: 3-0]

- **加算的（additive）**: 2つのシステムは共存する
- セマンティックトークンが TextMate ハイライトの**上に重ねて**適用される
- テーマにセマンティックルールがなければ `semanticTokenScopes` 経由で TextMate スコープにフォールバック
- 実装順序: TextMate 文法でベースラインを作り、LSP でセマンティックハイライトを追加

出典: [VS Code Semantic Highlight Guide](https://code.visualstudio.com/api/language-extensions/semantic-highlight-guide)

---

## 5. 未確定事項（要注意）

| 質問 | 状態 |
|------|------|
| セマンティックハイライトはデフォルト有効か（テーマ側 opt-in 不要か） | 「明示的 opt-in 必要」は否決されたが、デフォルト挙動の詳細は要検証 |
| `textDocument/semanticTokens/full/delta` の lsp 2.8.x サポート状況 | HLS PR では「将来作業」とされていたが否決。要確認 |
| `contributes.languages` / `grammars` の必須フィールドの正確なセット | 「id/aliases/extensions が必須」「3フィールドのみ必須」はいずれも否決。公式ドキュメントを直接確認 |

---

## 実装ロードマップ

### Phase 1: TextMate 文法（VS Code 拡張）
1. `editors/vscode/` ディレクトリに VS Code 拡張を作成
2. `syntaxes/malgo.tmLanguage.json` を作成（キーワード・コメント・リテラル）
3. `language-configuration.json` を作成（コメント・ブラケット定義）
4. `package.json` に `contributes.languages` / `contributes.grammars` を登録

### Phase 2: LSP セマンティックトークン（malgo-lsp 拡張）
1. `lsp-types` の `SemanticTokensOptions` を `serverCapabilities` に追加
2. Rename パスの結果から `NameSemanticMap` を構築するロジックを実装
3. `textDocument/semanticTokens/full` ハンドラーを登録
4. VS Code 拡張側で LSP サーバーを起動する `LanguageClient` を設定

---

## 参考ソース

| ソース | 品質 | 用途 |
|--------|------|------|
| [TextMate 公式マニュアル](https://macromates.com/manual/en/language_grammars) | primary | 文法書き方の正規ソース |
| [VS Code Contribution Points](https://code.visualstudio.com/api/references/contribution-points) | primary | package.json 登録方法 |
| [VS Code Semantic Highlight Guide](https://code.visualstudio.com/api/language-extensions/semantic-highlight-guide) | primary | セマンティックトークン API |
| [haskell/lsp GitHub](https://github.com/haskell/lsp) | primary | Haskell LSP ライブラリ |
| [HLS semantic tokens PR #3892](https://github.com/haskell/haskell-language-server/pull/3892) | primary | Haskell 向け参考実装 |
| [VS Code Semantic Highlighting Overview](https://github.com/microsoft/vscode/wiki/Semantic-Highlighting-Overview) | primary | 2層構造の仕様 |
