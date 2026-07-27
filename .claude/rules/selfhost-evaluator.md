---
paths:
  - "runtime/malgo/**/*.mlg"
  - "scripts/selfhost*.sh"
---

# Malgo コーディングガイドライン

`runtime/malgo/compiler/` 以下の自己ホスト型コンパイラ・インタープリタを
編集する際に守るべきルール。

---

## 1. IO 副作用は必ず `let _ = ...` で束縛する

### 背景

自己ホスト型評価器（`Eval.mlg`）は **遅延評価** を採用している。
`eval` の `ExLet` 分岐では:

- `let _ = expr; body` → `expr` を即時評価し、結果を捨ててから `body` へ進む
- `let x = expr; body` → `expr` を VThunk に包んで `x` に束縛し、`body` へ進む

後者では `x` が `body` 内で参照されなければ `expr` は **永遠に評価されない**。

### ルール

戻り値を使わない IO 操作（出力・終了など）には必ず `_` を束縛変数にする。

```malgo
-- 悪い例: printString が実行されないことがある
let u       = printString s; Right VUnit
let printed = printString (toStringInt32 n); Right VUnit
let n       = newline ();    Right VUnit

-- 良い例: 確実に副作用が実行される
let _ = printString s;                Right VUnit
let _ = printString (toStringInt32 n); Right VUnit
let _ = newline ();                    Right VUnit
```

対象関数: `printString`, `printChar`, `printInt32`, `printInt64`,
`newline`, `print`, `malgo_print`, `malgo_newline`, `putStr`, `putStrLn` など。

---

## 2. ビルトイン追加時は 3 箇所をセットで更新する

### 背景

ビルトインの登録は以下の 3 か所で分散管理されている。
どれか一つが欠けると「名前はあるが実装がない」か「実装はあるが呼び出せない」
という状態になる。

### ルール

新しいビルトイン `foo` を追加するときは必ず以下をすべて更新する:

| 場所 | 目的 |
|------|------|
| `builtinNames` の対応リスト | `makeBaseEnv` が `VBuiltin("foo", ...)` を登録 |
| `foreignArity` / `foreignArity2/3/4/...` | 引数の個数を返す |
| `applyBuiltin` / `applyBuiltin2/3/...` の `if` チェーン | 実際の処理実装 |

---

## 3. `makeBaseEnv` に登録したビルトインは上書きしない

### 背景

Level 2（メタ循環インタープリタ）では、内側の評価器が
`makeBaseEnv` を初期環境として Main.mlg のモジュール群を読み込む。
このとき Malgo ソース定義（`DeclDef`）が VBuiltin エントリを
VThunk で上書きすると、字句解析に必要な `parseIntString32/64` などが
消えて整数リテラルのトークン化が失敗する。

### ルール

`evalDecls` の `DeclDef` 処理では既存の VBuiltin を上書きしない
（現在の実装は `envLookup` で VBuiltin を検出したらスキップする）。

Malgo ソースに定義があるにもかかわらずビルトインとしても登録したい関数を
追加する場合は、この保護機構が正しく機能することを確認する。

---

## 4. Level 2 動作確認手順

### 背景

Level 1 では通る変更が、Level 2（Malgo → Malgo → Malgo）では失敗する
ことがある。評価意味論が異なるため、自己ホスト型コンパイラを変更したら
必ず Level 2 を手動確認する。

Level 1/2 はどちらも Zig バックエンド経由で走る。`Main.mlg` を
`malgo compile` でネイティブバイナリにし、そのバイナリが評価器になる。
以前は Scheme バックエンド経由だったが、`malgo_read_file` の実装により
移行した（Scheme バックエンドは削除済み）。

### 手順

```bash
# 1. 依存モジュールをプリコンパイル（初回のみ時間がかかる）
for f in runtime/malgo/Builtin.mlg runtime/malgo/Prelude.mlg \
          runtime/malgo/Either.mlg \
          runtime/malgo/compiler/{AST,Token,Diagnostic,Lexer,Parser,Value,Eval,FunIR,Rename,ToFun,Main}.mlg; do
  lean/.lake/build/bin/malgo eval "$f" </dev/null >/dev/null
done

# 2. Level 1 評価器をネイティブバイナリにコンパイル
lean/.lake/build/bin/malgo compile runtime/malgo/compiler/Main.mlg -o /tmp/malgoc --opt release-fast

# 3. Level 2 テスト（例: Echo）
printf 'Hello\n' | /tmp/malgoc \
  runtime/malgo/compiler/Main.mlg test/testcases/malgo/Echo.mlg

# 4. フルスクリプト
PRECOMPILE_TIMEOUT=300 CASE_TIMEOUT=1200 bash scripts/selfhost-level2.sh
```

Level 2 は Level 1 より 50〜200 倍遅く、さらに Zig バックエンドは同じ
ケースで Chez Scheme の約7.5倍遅い（実測: Fib で 220秒 対 29秒）。
タイムアウトは余裕を持たせること。改善は #385 で追跡している。
