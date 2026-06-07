# 自己ホスト型インタープリタの開発ガイドライン

`runtime/malgo/compiler/Eval.mlg` および自己ホスト型コンパイラモジュールを編集する際に守るべきルール。

## 1. IO 副作用には必ず `let _ = ...` を使う

自己ホスト型評価器はデフォルトで **遅延評価** を行う。
`let name = expr; body` は `name` が `body` 内で参照されなければ `expr` を評価しない（VThunk のまま放置される）。

**悪い例（副作用が実行されない）:**
```malgo
let u = printString s; Right VUnit
let n = newline ();     Right VUnit
```

**良い例（副作用が確実に実行される）:**
```malgo
let _ = printString s; Right VUnit
let _ = newline ();     Right VUnit
```

`_` を束縛変数にすると評価器が即時に式を評価してから `body` に進む
（`eval` の `ExLet` 分岐で `name == "_"` のとき `bindRight` で eager 評価する）。

対象: `printString`, `printChar`, `newline`, `print`, `malgo_print`,
`malgo_newline`, `putStr`, `putStrLn`, `printInt32`, `printInt64` など
戻り値を使わない IO 操作すべて。

## 2. `makeBaseEnv` に登録したビルトインは上書きしない

`makeBaseEnv` は `builtinNames` にある名前を `VBuiltin` として登録する。
`evalDecls` の `DeclDef` 処理で既存の `VBuiltin` を上書きしてはならない。

**理由:** Level 2（メタ循環インタープリタ）実行時、内側の評価器が
Main.mlg のモジュールを読み込む際、Malgo ソース定義が
`makeBaseEnv` の VBuiltin を上書きすると、`parseIntString32/64` など
字句解析に必要なビルトインが消失して整数リテラルのトークン化に失敗する。

**チェックリスト:**
- 新しいビルトインを追加するときは `builtinNames` の適切なリストに追加する
- `applyBuiltin` チェーンにも対応するハンドラを追加する
- 既存の Malgo ソース定義と名前が重複する場合は `evalDecls` の VBuiltin チェックが保護してくれるが、意図的な上書きでないことを確認する

## 3. Level 2 動作確認手順

変更後は以下で Level 2 の動作を確認する:

```bash
# キャッシュが温まっている前提（malgo eval で各ファイルをプリコンパイル済み）
cabal exec malgo -- eval --target scheme runtime/malgo/compiler/Main.mlg > /tmp/main.scm

# 各テストケースを検証
printf 'Hello\n' | scheme --script /tmp/main.scm runtime/malgo/compiler/Main.mlg test/testcases/malgo/Echo.mlg

# フルスクリプト（PRECOMPILE_TIMEOUT を長めに設定）
PRECOMPILE_TIMEOUT=300 CASE_TIMEOUT=600 bash scripts/selfhost-level2.sh
```

Level 2 は Level 1 より 50〜200 倍遅いため、タイムアウトは余裕を持たせること。
