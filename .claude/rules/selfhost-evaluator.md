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

## 2. ビルトイン追加時は 4 箇所をセットで更新する

### 背景

ビルトインの登録は以下の 4 か所で分散管理されている。
最初の 3 か所のどれか一つが欠けると「名前はあるが実装がない」か
「実装はあるが呼び出せない」という状態になり、実行時のエラーで
すぐに気付ける。4 番目の `isPassthroughBuiltin` だけは性質が異なる
（次項参照）。

### ルール

新しいビルトイン `foo` を追加するときは必ず以下をすべて更新する:

| 場所 | 目的 |
|------|------|
| `builtinNames` の対応リスト | `makeBaseEnv` が `VBuiltin("foo", ...)` を登録 |
| `foreignArity` / `foreignArity2/3/4/...` | 引数の個数を返す |
| `applyBuiltin` / `applyBuiltin2/3/...` の `if` チェーン | 実際の処理実装 |
| `isPassthroughBuiltin` | ボックス化された引数をそのまま渡すか、`unwrapBuiltinArgs` で unbox するかを決める（deny リスト） |

### `isPassthroughBuiltin` は他の 3 箇所と違い「サイレントに壊れる」

`isPassthroughBuiltin` は `unwrapBuiltinArgs` が引数を unbox するかどうかを
決める deny リスト（`True` を返す名前は unbox されない）。

新しいビルトインが、`print`/`malgo_print`/`malgo_panic` のように
`valueToText` などでボックス化された値をそのまま観察する必要がある場合、
この関数に名前を追加し忘れると:

- コンパイルは通る
- 実行時エラーにもならない
- ただし出力がオラクル（Lean 側インタープリタ）と食い違う
  （例: 期待される `Int32#(5)` ではなく unbox 済みの `5` が出力される）

という形で気付きにくく壊れる。他の 3 箇所の欠落が「呼べない」という
分かりやすい失敗になるのに対し、ここだけは目視レビューでしか捕まえられない。
新しいビルトインを追加・変更する際は、その実装が引数のボックスをそのまま
観察する（`valueToText` に渡す、コンストラクタタグで分岐する、など）かどうかを
必ず確認し、必要ならこのリストに追加すること。

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

Chez Scheme バックエンド自体は #416 で再度存在する（`malgo eval --target
scheme`、`scripts/scheme-golden.sh` でcorrectness gate 済み）が、これは
nix-config のスクリプト実行用であり、Level 1/2 の自己ホストとは無関係。
`scripts/selfhost-level2.sh` に TARGET=scheme のような切り替えは存在しない
— Level 1/2 は今も Zig バックエンド経由でのみ走る。過去の「Zig は Chez の
7.5倍遅い」という #385 の数字を再測定する必要が出た場合は、#404 以前の
コミット（Level 2 に TARGET 切り替えがあった頃）を参照すること。

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
PRECOMPILE_TIMEOUT=300 bash scripts/selfhost-level2.sh
```

Level 2 は Level 1 より 50〜200 倍遅い。CI では `l2-build`（評価器を1回だけ
ビルドして artifact 化）+ `l2-case`（1 ジョブ 1 ケース、matrix）に分割して
いるので、ローカルで手動確認する際も `BUILD_ONLY=1` / `EVAL_BIN=PATH` /
`L2_CASES="A B"` が使える（詳細は `scripts/selfhost-level2.sh` 冒頭のコメント）。
タイムアウトは余裕を持たせること。

---

## 5. `builtinResultWrapper` は `isPassthroughBuiltin` の出力側版

### 背景

`unwrapBuiltinArgs`（項目2の4箇所目、`isPassthroughBuiltin` が制御）は
ディスパッチ**前**にボックス化された引数を unbox する。`builtinResultWrapper`
（`reboxBuiltinResult` から呼ばれ、`apply` の `VBuiltin` ケースでディスパッチ
**後**に一箇所だけ適用される）はその逆方向を担う、5箇所目の分散サイトである。
`Int32`/`Int64`/`Char`/`String` を返し、かつ Builtin.mlg/Prelude.mlg 側の本来の
定義が「引数を unbox して計算し、結果を再び box する」形（box対称、例:
`addInt32 = {(Int32# x)(Int32# y) -> Int32# (addInt32# x y)}`）のビルトインだけが
対象。比較演算（`Bool` を返す）、IO（`Unit` を返す）、`isPassthroughBuiltin` に
載っている制御フロー系（`print`/`if`/`case`/`goto`/`|>`/`<|`/`const`/
`malgo_unsafe_cast`/`malgo_panic`）、および `#` サフィックス/`malgo_` プレフィックス
の raw 版（`addInt64#`, `malgo_add_int64_t`, `toStringInt32#` など、
Builtin.mlg 側の型自体が raw = `Int32# -> Int32# -> Int32#` で box を経由しない）
は対象外（malgo#451 参照）。

### ルール

新しいビルトイン `foo` を追加・変更する際、その実装が
「引数を unbox して計算し、box 対称な結果を返す」形（`foo` の Builtin.mlg/
Prelude.mlg 側の定義が対応する box 型を返す）なら、`builtinResultWrapper`
（`builtinResultWrapper2/3/...`)に `"foo" -> Just "Int32#"` のようなエントリを
追加すること。

`isPassthroughBuiltin` と同じ理由で、ここへの追加漏れは:

- コンパイルは通る
- 実行時エラーにもならない
- ただし `print`/`valueToText` で直接観察したときだけオラクルと食い違う
  （例: 期待される `Int32#(7)` ではなく unbox 済みの `7` が出力される）

という形で気付きにくく壊れる。ただし多くの場合、後続の別ビルトイン呼び出しが
`unwrapBuiltinArgs` で同じ値を再度 unbox するため、中間結果が
box/unbox を往復して観察できないケースも多い（例: `toStringInt32(addInt32(x,
y))` は `addInt32` の reboxing 漏れがあっても出力に現れない）。ズレが実際に
見えるのは、結果を `print`/`malgo_print` に直接渡す、または明示的に
`Foo#` パターンで分解するなど、box を経由せず値を観察する経路だけ。
