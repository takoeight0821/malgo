# Go バックエンド設計

## 動機

Malgo には現在2つの生成コードバックエンドがある。両者は正反対の欠点を持つ。

| | 実装規模 | 外部依存 | selfhost-l2 実測 |
|---|---|---|---|
| Zig | Lean パス 2,500行 + `runtime.zig` 1,995行 | zig 0.16 | 245s |
| Scheme | `Scheme.lean` 765行（うち lowering は約150行） | Chez 10.4.1 処理系 | 47s |

Zig が大きいのは、Zig に GC も末尾呼び出し保証も無いからである。GC が無いために Perceus RC 一式（`Perceus`/`Reuse`/`RcCheck` の3パス、`self` 自己渡し規約、`ClosureConv` 557行、dup/drop/dropReuse/leak 検査）を背負い、末尾呼び出し保証が無いためにトランポリンを持つ。Scheme が小さいのは、Chez が GC と proper tail call の両方を処理系として提供するからであり、その代償として配布に Chez 処理系そのものを要求する。

Go は GC を持ち、末尾呼び出し保証を持たない。したがって Zig の重さのうち RC 由来の部分だけが消え、トランポリンは残る。結果として、Scheme 並みに薄い emitter を、処理系への外部依存なしに単一ネイティブバイナリへ落とせる。

## 性能の基準線

基準線は Zig ではなく Chez である（`bench/perf-baseline.json` の `l2_ratio`: Chez 47s / Zig 245s）。Go が Chez の速度を上回る保証は無い。Go が Chez に対して確実に優位なのは配布形態（単一の静的リンクバイナリ）であり、Zig に対しては実装の小ささである。速度については実測してから主張する。

生成バイナリのサイズは達成目標に含めない。Go は静的リンクで最小 1–2MB になり、Zig より大きくなる。

### 実測結果（2026-09-12、Darwin arm64、`--opt release-fast`）

| | `BenchFibDeep` | selfhost Level 1（`Fib.mlg`） | Level 1 評価器のバイナリ |
|---|---|---|---|
| Zig | 0.31s | 0.24s | 5.99 MB |
| Go | 0.46s | 0.41s | 11.34 MB |
| Chez | 0.19s | 0.73s | — |

Go は現実的なワークロード（Level 1）では Zig と Chez の中間に入り、純粋な算術（fib-deep）では最下位である。前者が代表的であり、後者は interface boxing と GC が最も不利に出るケースである。fib-deep では Chez が Zig を上回っており、これは `l2_ratio` と同じ向きだが Level 1 とは逆向きである。つまり、どちらか一方の microbench だけで順位を語ることはできない。

Level 1 の `dispatches` は Go が 11.3M、Zig が 9.0M である。差は非 escaping な join point に由来する——Zig の `ClosureConv.classifyJoins` が `Local` と判定して畳むものを、Go は毎回クロージャ化して dispatch している。

### 追記（同日、改善後）

`classifyJoins` を `Malgo.Sequent.Core.Escape` へ移して Go でも使い、`Str` にスカラのキャッシュを入れた結果、Level 1 は **0.41s → 0.30s（-27%）**、dispatch は **11,275,440 → 9,028,448** となり Zig の 9,028,449 とほぼ一致した。残る差は 1 dispatch あたりの単価で、Go の ABI が決めるため下げられない。

効かなかった案（interface boxing の除去、`[]rune` キャッシュ、generics、リフレクション、トランポリンの形の変更）とその実測値は `wiki/2026-09-12-go-backend-performance-investigation.md` に記録した。

## 位置づけ

第3のバックエンドとして Zig・Scheme と併存する。メモリ管理は Go GC に全面委任する。

`docs/plans/2026-08-11-chez-scheme-backend-and-nix-config-scripting.md` は、Scheme バックエンドが2度削除された理由を「恒久的な消費者が無かった」ことだと記録している。Go バックエンドも同じ論法の対象になりうるため、golden ゲートを最初から必須とする（Step 6）。消費者候補は nix-config のスクリプト用途（Chez への外部依存を単一バイナリで置き換える）だが、移行そのものは本設計の範囲外とする。

---

## 設計

### 全体像

```
Join IR ──Normalize──> Join IR ──Go.compileToGo──> Go ソース ──go build──> 実行ファイル
```

中間 IR は増やさない。ANF も closure conversion も行わない。Go が本物のクロージャと GC を持つため、Join IR から Go テキストへ直接落とせる。構造は Scheme バックエンドと同型であり、差は「式の入れ子」ではなく「文と `return Action{...}`」で書く点だけである。

### 持たないもの

Zig バックエンドが持ち、Go バックエンドが持たないもの。

- `ClosureConv`（557行）とラムダリフティング。Go のクロージャ literal に落とす
- 自己渡し規約（`self` / `no_self` / `FuncKind` / `MkClosure` / `ReadCapture` / `capturesOf` / captures 配列）。これは「callee が captures を dup してから self を drop する」という RC プロトコルのために存在するので、GC と同時に消える
- `Perceus`（257行）/ `Reuse`（169行）/ `RcCheck`（229行）/ `DropReuse` / reuse token 線形性
- leak check（exit 83、`MALGO-LEAK`）と `g_live_objects`
- `Ir.lean`（286行）と `Stage.lean`。ANF を持たないため不要

### 持つもの

- **トランポリン**。Go も末尾呼び出しを保証しない。Go のスタックは伸長するが `debug.SetMaxStack` の既定は 1GB であり、Zig 側で計測された fib-deep の約 1.85GB には足りない。加えて `dispatches` カウンタは機種非依存で決定的な唯一の性能ゲートを与える
- `MAX_ARGS = 2`。#407 が 220k 以上の生成呼び出し地点で実測して確定した値であり、フロントエンドの構造（`ToFun` の単一引数ラムダ + `ToCore` が付ける1つの consumer）から来ている
- `Normalize`。`Cut (Mu x s) k ⇒ s[x := k]` と `Join m (Label j) s ⇒ s[m := j]` を先に潰す

### 値表現

Go の interface に入れる具体型が1ワードのポインタ形でないと、変換のたびに暗黙のヒープ確保が走る。`string` は2ワードなので `type Str string` は毎回確保する。そこで `Value` に入る具体型をすべてポインタ形か func 型に揃える。

```go
type Value interface{ malgoValue() }

type Int32  struct{ V int32 }          // 常に *Int32
type Int64  struct{ V int64 }
type Float  struct{ V float32 }
type Double struct{ V float64 }
type Char   struct{ V rune }
type Str    struct{ V string }         // 常に *Str
type Struct struct{ Tag string; Fields []Value }   // タグ "tuple" はタプル
type Unit   struct{}

type NamedField struct{ Name string; Code Fn }     // フィールドはサンク
type Record     struct{ Fields []NamedField }      // Name 昇順のスライス

type Fn func(args []Value) Action      // func 型は1ワード、直接格納される
```

レコードのフィールドを `map[string]Fn` にしてはならない。Go の map のイテレーション順は意図的にランダム化されているため、フィールド集合を走査する経路が1つでもあれば golden が間欠的に落ちる。Zig の `NamedField` スライスと Scheme の昇順ソートに揃える。

`Int32` は Zig の `mkInt32` と同じく `-128..1024` を事前確保テーブルで intern する。Zig 側で「`fib 5` を Malgo 製インタプリタ経由で計算すると 4.57M 回確保する」と測られた効果がそのまま効く。

Consumer（継続）は `Fn` そのものであり、`Closure` 型は要らない。Zig が `Closure{code, captures}` を必要としたのは captures を RC 管理するためである。

#### 実測（2026-09-12、go 1.26.7 darwin/arm64）

`testing.AllocsPerRun` による確認結果。

| 操作 | allocs/op |
|---|---|
| `MkInt32(42)`（intern 範囲内） | 0 |
| `MkInt32(99999)`（範囲外） | 1 |
| `MkUnit()` | 0 |
| `MkStr("hello")` | 1 |
| `Fn` を `Value` へ格納 | 0 |
| `Action` の構築 | 0 |

トランポリンの確保回数は dispatch 数に依存しない。1,000 / 10,000 / 100,000 ステップのいずれでも 1 回の実行あたり 4 allocs（ループのセットアップ分のみ）であり、**dispatch あたり 0** である。`go build -gcflags=-m` でも `Action` はヒープへ逃げない。

この表現で設計を進めてよい。

### トランポリン

```go
type Action struct {
    Code Fn          // nil なら完了、結果は Argv[0]
    Argv [2]Value
    Argc int
}

func Run(code Fn, args []Value) Value {
    cur := Action{Code: code}
    copy(cur.Argv[:], args)
    cur.Argc = len(args)
    for cur.Code != nil {
        next := cur.Code(cur.Argv[:cur.Argc])
        cur = next
        gDispatches++
    }
    return cur.Argv[0]
}
```

`Action` は 4ワードの値型であり、Go 1.17 以降のレジスタ ABI（9ワードまで）に収まる。`self` が無いため Zig の 5ワードより小さい。`cur = c(...)` と直接書かず `next` を経由するのは Zig と同じ理由による——呼び出し先が `cur.Argv` を指したまま `cur` へ直接構築するエイリアシングを避ける。

#### `Force` の入れ子は `Pattern.expand` だけに残る

Zig が `Force` を式にして入れ子 `Run` を必要としたのは ANF だからである。Go は Join IR から直接落とすので、`Consumer.project` は自然に終端（`return projectField(v, f, k)`）になり、そこでは入れ子が要らない。

残るのは `Pattern.expand` だけである。複数フィールドを束縛するためブロック中間でサンクを強制する必要があり、ここは `forceField`（identity 継続を渡して `done` まで回す入れ子 `Run`）を使う。

実測（2026-09-12）: レコードを使わない `BenchFibDeep` は `force_depth_max=0`、レコードを使う `RecordTest` / `RecordFieldAccess` / `TaggedRecordConstruct` / `TaggedRecordDiamondUse` はいずれも 1 である。ネイティブスタックが伸びるのはこの1箇所だけで、深さは reduction step 数ではなくレコード強制の入れ子段数で決まる。

### lowering

各 `Definition` は `func Mod_name(args []Value) Action` になる。各 `Statement` は Go 文の列で、終端が `return Action{...}` である。

Statement（7種）:

| Join | Go |
|---|---|
| `cut p k` | `return apply(k, <p>)` |
| `join m c s` | `m := func(args []Value) Action { ... }`。再帰する場合は `var m Fn; m = func(...)` |
| `invoke f k` | `return Action{Code: Mod_f, Argv: [2]Value{k}, Argc: 1}` |
| `primitive` / `externalCall` / `binOp` | `v := malgo_xxx(a, b)` の後、継続へ |
| `ifz c t e` | `if isZero(c) { ... } else { ... }` |

Consumer（6種）:

| Join | Go |
|---|---|
| `label j` | 束縛済みの `Fn` 変数をそのまま参照 |
| `apply ps ks` | `func(args []Value) Action { return callFn(args[0], ...) }` |
| `project f k` | `return projectField(v, "f", k)`。フィールドサンクを強制する2箇所のうちの1つ |
| `then x s` | `func(args []Value) Action { x := args[0]; ... }` |
| `finish` | 恒等継続（`done`） |
| `select bs` | 型 switch + タグ比較の `if` 連鎖。どの分岐にも合致しなければ panic |

Producer（6種）: `var` / `literal` はそのまま、`construct` は `&Struct{Tag, Fields}`、`lambda` は Go のクロージャ literal、`object` は `&Record{Fields}`（`NamedField` 昇順）、`mu` は `Normalize` 後に現れない。

`Pattern.expand` はフィールドサンクを強制するもう1つの箇所であり、`project` と対で実装する。どちらかを落とすと静かに壊れる。

`.mu` アームが不要である根拠: `Normalize` が `Cut (Mu x s) k ⇒ s[x := k]` を潰す。`Mu` が引数位置に現れないことは、`lean/Test/Main.lean:709-715` の `isValueProducer` が `.mu => false` を返し、`primitive` / `externalCall` / `binOp` / `construct` の引数位置がすべて値であることを `ir-invariants` ゲートが強制していることによる。したがって `Normalize` 後の Join IR に `Mu` は残らず、emitter は全域関数になる。

### Go 固有の制約

- **未使用ローカル変数はコンパイルエラー**である。Zig 側は `Ir.suffixFreeVars` で liveness を解いたが、Go では束縛ごとに `_ = x` を無条件に吐けばコンパイラが消す。liveness 解析の移植は不要
- **文字列はコードポイント単位**でなければならない（Haskell `Text` 意味論。Zig の `utf8ByteOffsetOfScalar` 相当）。Go の `s[i]` はバイト添字なので走査が要る。`Str` はコードポイント数と ASCII フラグをキャッシュして、ASCII ならバイト添字で済ませる。`[]rune` をキャッシュしてはいけない——実測で 2 倍遅くなる
- **primitive 名は `malgo_*` のまま Go の関数名にする**。そうすれば `runtime.go` を grep するだけの coverage ゲートが書ける。Zig も Scheme も primitive 欠落は golden diff でしか分からないが、Go についてはその穴が最初から閉じる

### 最適化（v2、初版に含めない）

- ~~非 escaping な join point のクロージャ確保を消す~~ — 実装済み。`goto` ではなく Zig と同じインライン展開を採った（上記の追記を参照）
- `Peephole` 相当（節マッチが確保する scrutinee タプルの除去）。`Peephole.lean` 自体は ANF の `Ir.Path` / `Ir.Test` に依存していて流用できないので、Join IR 上の別パス（コンストラクタが静的に分かる `cut` で `select` を融合する）になる

---

## 実装手順

### Step 0: 値表現の実測（完了）

上記「実測」節のとおり。結果は設計を支持する。

### Step 1: `Normalize` の移設（先行する独立コミット）

`lean/Malgo/Backend/Zig/Normalize.lean` を `lean/Malgo/Sequent/Core/Normalize.lean` へ移す。Zig 側の import を差し替えるだけの機械的変更である。Zig に触るため独立したコミットにし、`mise run test` と `zig-golden` が通ることを確認してから次へ進む。

### Step 2: 差し込み面（6ファイル）

Scheme バックエンド復活時（`docs/plans/2026-08-11-...` の Phase 0.1）と同じ面である。

- `lean/Malgo/Prelude.lean:326` — `Target` に `| go` を追加
- `lean/Main.lean:32` `parseTargetArg`、`:49` `usage`、`:231` `runEval` の dispatch
- `lean/Malgo/Driver.lean` — `compileGo`（`compileScheme:245` と同型、`linkForCli:213` の後ろ）
- `lean/Malgo.lean` — import
- `lean/Malgo/Backend/Go.lean` — 本体
- `mise.toml` — `go` のバージョンを pin（実測に使った版は 1.26.7）

`main` が無いモジュールの扱いは Zig に倣う（`entry := none` で no-op 実行ファイル）。Scheme のように無条件に main 呼び出しを吐くと、golden ゲートに skip list が必要になる。

### Step 3: `runtime/go/runtime.go`

`runtime/malgo/Builtin.mlg` の 99 個の `foreign import` をすべて実装する。Go 標準ライブラリがあるため、Zig の 1,995行に対して 700〜900行程度に収まる見込みである。内訳は算術・比較・文字分類・コードポイント単位の文字列操作・ファイル IO・環境変数・`os/exec` によるプロセス起動・panic である。

`Builtin.mlg` へ新しい primitive を足す作業は発生しない。既存の 99 個を Go で実装するだけである。

`include_str` で埋め込むと、Lake の追跡漏れ（`lean/Malgo/Backend/Zig/Runtime.lean:11-35` が実測付きで記録している問題）をそのまま継承する。`mise.toml` の `bust-runtime` タスク（`mise.toml:27-29`）に Go 側の `.olean` / `.trace` / `.c` の削除を追加する。

### Step 4: `lean/Malgo/Backend/Go/Toolchain.lean`

`Zig/Toolchain.lean`（106行）と同型である。ワークスペース内に `go.mod` と `main.go` を書き、`go build -o OUT` を走らせる。`findOnPath "go"` の事前確認は Zig と同じ理由で必要である（Lean の `IO.Process.output` はコマンド不在で例外を投げず、非ゼロ終了の `.ok` を返す）。

ネットワークを踏まないために2点を守る。

- `GOTOOLCHAIN=local` を環境変数で設定する。`go.mod` の `go` 行が手元の toolchain より新しいと、Go は toolchain を自動ダウンロードしようとする。標準ライブラリしか使わなくてもここでネットワークアクセスが発生し、sandbox で失敗する
- `go.mod` の `go` 行を、`mise.toml` で pin した版と一致させる

`GOCACHE` は、Zig が `--cache-dir` / `--global-cache-dir` をワークスペース配下へ寄せているのに揃え、`ws.dir` 配下へ向ける。

### Step 5: `malgo compile` の target 対応

現在 `malgo compile` は Zig に固定されており（`lean/Main.lean:270`）、`--opt` の `OptMode` は Zig の型である（`Main.lean:252`）。Go を配布形態の売りにする以上、`--target` を `parseCompile` に通し、`OptMode` を共有型へ引き上げる必要がある。実装が2つになる時点なので、CLAUDE.md の YAGNI 規則はここで抽象化を許可する。

Go の最適化モードは `debug` / `release-safe` / `release-fast` を `go build` のフラグ（`-gcflags=all=-N -l` / 既定 / `-ldflags=-s -w`）に対応づける。

---

## 検証

### Step 6: golden ゲート（`scripts/go-golden.sh`）

`scripts/scheme-golden.sh`（200行）を雛形とする。leak チェックは不要である（GC）。

- `.malgo-work` ミラーを `Builtin` / `Prelude` / `Either` の `malgo eval` で事前 seed する（既存2本と同じ。bare-name import はミラー経由でしか解決しない）
- `.golden/Malgo.Sequent.Eval/<Case>/golden` を持つ全ケースを `malgo compile --target go` の既定モード（最適化フラグなし）でビルドし、`printf 'Hello\n'` を stdin に与えて stdout をバイト単位で比較する。Zig が `--opt debug` を使うのは DebugAllocator で leak を取るためであり、Go にその理由は無い。`-gcflags=all=-N -l` はトランポリンの dispatch を極端に遅くするので sweep には使わない
- 失敗を compile-fail / run-fail / mismatch / timeout に分類する
- 発見ケース数が 0 なら失敗扱いにする
- panic ゲート（`Panic` / `CondPanic` / `PanicNamedImport`）を別途持つ。終了コード非ゼロ、stdout 一致、stderr の `Malgo: <msg>` を検査する

既知の不一致は持ち込まない（Scheme 復活時の Phase 0.3 と同じ方針）。ゲートを入れる前に直す。

### Step 7: deep-recursion ゲート（`scripts/go-deep-recursion.sh`）

`scripts/zig-deep-recursion.sh` と同じ構造である。`bench/fixtures/BenchFibDeep.mlg` を release-fast でビルドして実行し、`75025` と約 18.8M dispatches を確認する。トランポリンの退行（終端が Go のネイティブ呼び出しに戻る、ヘルパが `Action` を返さず dispatch する）はここでしか捕まらない。golden sweep のケースはすべて浅い。

失敗シグネチャは Zig と異なる。Zig は SIGSEGV（終了コード 139）を検査するが、Go はスタック上限超過で `fatal error: goroutine stack exceeds ...` を stderr に出し、終了コード 2 で落ちる。検査対象をこちらに合わせる。

### Step 8: 性能記録

`bench/perf-baseline.json` に `go` の tier を追加する。ratchet ゲートにできるのは `dispatches`（および `Force` の入れ子が残った場合の `force_depth_max`）だけである。`runtime.ReadMemStats().Mallocs` は決定的でないので、記録はしてもゲートにはしない。

`l2_ratio` に相当する selfhost-l2 の実測は未取得である。Zig だけで16分かかるものを3バックエンド分走らせる価値は現時点では無い。Level 1 の実測（「性能の基準線」節）が代わりの答えになっている。L2 が必要になったときは `scripts/perf-baseline.sh` の `l2-ratio` tier が入口になる。

### Step 9: CI

`.github/workflows/lean.yml` に `lean-go-golden` ジョブを追加する。Scheme ジョブ（`:241-294`）に倣い、`if:` を付けない（`ci-gates.env` のフラグは、PR ごとのフィードバックには遅すぎるゲートのキルスイッチである）。

`actions-timeline` の `needs:` リスト（`:321`）に新ジョブを追加する。忘れるとタイムラインレポートから静かに脱落する。

### 手元での確認手順

```bash
mise run build
mise run test                              # 既存ゲートの退行が無いこと
lean/.lake/build/bin/malgo eval --target go examples/malgo/Hello.mlg
lean/.lake/build/bin/malgo compile --target go examples/malgo/Hello.mlg -o /tmp/hello && /tmp/hello
bash scripts/go-golden.sh
bash scripts/go-deep-recursion.sh
```

---

## リスク

| リスク | 内容 | 対応 |
|---|---|---|
| Chez より遅い | Chez は世代別 GC と proper tail call を持つ成熟した処理系である。Go は GC を持つがトランポリンを挟む。速度で負ける可能性が実在する | Step 8 で測る。速度で負けても配布形態（単一バイナリ）の優位は残るので、その場合は位置づけを配布用途に限定する |
| 恒久的な消費者が無い | Scheme バックエンドが2度削除された理由そのものである | golden ゲートを最初から必須にする。消費者候補（nix-config の Chez 置き換え）は本設計の範囲外だが記録しておく |
| バイナリサイズ | Go は静的リンクで最小 1–2MB であり、Zig より大きい | 達成目標から外す |
