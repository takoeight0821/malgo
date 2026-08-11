# ChezScheme バックエンドの復活と、malgo の nix-config スクリプト言語化

Date: 2026-08-11
Issue: #416

## Context

### Scheme バックエンドの3回目の人生

`lean/Malgo/Backend/Scheme.lean` は今の master には存在しない。過去2回、別の目的で
現れて消えている。

1. **M4（Lean 移植期）**: `Driver.compileScheme` / `malgo eval --target scheme` として導入。
   Join IR から Chez Scheme ソースへの変換（`src/Malgo/Backend/Scheme.hs` の Lean 移植）。
2. **#386 で削除**: セルフホストが Zig バックエンド経由に移った時点で「もう要らない」という
   判断。しかしこの移行で Level 2（メタ循環評価）が Zig 経由だと Chez 経由の約7.5倍遅くなり、
   `LEAN_SELFHOST_L2=0` で CI から外す事態になった（#385 として追跡）。
3. **#401 で復活**: #385 の「7.5倍」という数字を再測定するための、比較対象（Chez側の分母）
   としてのみ復活。**性能の基準線であって正しさの基準ではない**と明記され、golden gate は
   一度も持たなかった。実際に73件のgoldenを流すと71件しか一致せず、`EmptyConstructor`
   （空コンストラクタの表示が異なる）と `LabelGoto`（継続が `number->string` に漏れる）の
   2件は既知の不一致として放置された。
4. **#404 で再削除**: #385 が closed し、v4.0.0 が出た（2026-07-27、以後リリースはまだない）
   時点で「測定用の消費者がいなくなった」として削除。最終計測値は chez 30s / zig 149s /
   比率4.96x。

つまり2回とも「一時的な性能比較のための control」という位置づけで、恒久的な使い道を
持たされたことがない。それが両方とも削除された理由である。

### 今回のモチベーション

nix-config 側の別セッションで `tasks/*.sh`（bash + jq、最大231行の
`sync-claude-plugins.sh`）の書き直し言語を検討していた。当初は「読みにくいから」
という理由で Ruby が候補に挙がったが、advisor との対話で実際の要求は
「実行前に型検査でロジックバグ・タイポを捕まえたい」ことだと判明し、動的型付けの
Ruby は要件を満たさないと分かった。Deno+TypeScript・Go・Haskell（Nix化コストが高く
不採用）が代替候補として挙がっていたところに、ユーザーから本セッションで
「malgo を nix-config のスクリプト言語に使う」という直接の指示が出た。malgo は
Hindley-Milner 型推論と ADT を最初から持つ言語であり、この要求に正面から応える。

したがって今回のバックエンド復活は過去2回と性質が違う。**nix-config が実際に実行する
プログラムを生成する、恒常的な消費者を持つ。** これまで免除されていた golden gate を
今回は必須にする。

### 現状のギャップ

`runtime/malgo/Builtin.mlg` / `lean/Malgo/Sequent/Eval.lean` の foreign import 一覧を
確認した。ファイル I/O（`malgo_read_file`/`malgo_write_file`）、標準入出力、
`malgo_get_args`、`malgo_exit_success`/`malgo_exit_failure`（0/1固定）はあるが、
**サブプロセス実行と環境変数取得に相当するものが存在しない**。`tasks/*.sh` の実体は
`nix`/`sops`/`gh`/`jq` を呼ぶサブプロセス起動が中心なので、これが今回最大のギャップになる。

一方、Lean 側にはこの用途に転用できる実装が既にある。`lean/Malgo/Backend/Zig/Toolchain.lean`
が `zig build-exe` を呼ぶのに `IO.Process.output { cmd, args }` を使っており、
`args : Array String` を渡す形（シェル経由の文字列展開なし）になっている。インタプリタ側の
サブプロセス実行はこれと同じ形で実装できる。

### nix-config 側の制約

`flake.nix` の `system` は `aarch64-darwin` 固定（macOS, Apple Silicon）。ChezScheme が
nixpkgs のこのプラットフォームでビルド／キャッシュされるかどうかは未確認 — Phase 2 で
検証する。ここが崩れた場合は「Zig ネイティブバイナリを flake でビルドして配布する」を
代替の配布経路とする（下記 Design Choices 参照）。

## Design Choices

### なぜ Chez なのか：Zig より速く、安定している

nix-config のスクリプト実行先として Chez を選ぶ一番の理由は、性能と安定性が Zig
バックエンドより優れているという実績である。lightweight な配布経路であることは
副次的な利点にすぎない。

- **性能**: #385 の最終計測（PR #401 本文）は Level 2 セルフホストで chez 30s
  対 zig 149s、比率 4.96x。Zig バックエンドは Perceus 参照カウント・トランポリン
  呼び出し規約など、ネイティブコードとして正しく動かすための機構そのものに
  ランタイムコストを払っている。Chez は成熟した最適化ネイティブコンパイラ + 世代別
  GC が最初から相殺している。
- **安定性**: Zig バックエンドはトランポリン化（#360）以前、深い再帰で約15万
  reduction step を超えると SIGSEGV していた（ネイティブスタックが線形に伸びる
  呼び出し規約だったため）。この種の「深さでネイティブに壊れる」クラスの不具合が
  Chez 側では起きていない。nix-config のタスクスクリプトは対話的に何度も実行され、
  失敗の許容度が低い — 遅い上に稀に壊れるバックエンドで実行するには向かない用途である。

### 配布・実行モデル：ビルド時コンパイル + 実行時は Chez のみ

```
.mlg（nix-config 側で書く）
  → malgo eval --target scheme（Nix 評価/ビルド時に1回。malgo/Lean/Zig ツールチェイン必要）
  → .scm（生成物。Nix ストアにキャッシュされる）
  → scheme --script（実行時。chez-scheme ランタイムだけで動く）
```

副次的な利点として、`malgo eval`（インタプリタ）は実行毎にLeanパイプライン全体を
起こし、`malgo compile`（Zig）はコンパイル毎にZigツールチェインでのリンクが要る。
どちらも「日常的に何度も叩く小さなスクリプト」には向かない。Chez 経由なら、重い
ビルドとツールチェイン依存は Nix 導出の中に閉じ込められ、実行時の依存は軽量な
`chez-scheme` だけになる。ziku（vendor/ziku）でも Chez を使っており、社内の実績とも
一致する。

Phase 2 の検証（`chez-scheme` が aarch64-darwin でビルド/キャッシュされるか）で
この前提が崩れたら、「`malgo compile` の Zig バイナリを flake の `packages` として
配布する」に切り替える。ただしその場合も上記の性能・安定性の懸念は残るため、単なる
配布経路の代替であって性能面の同等な代替ではないことに注意する。

### サブプロセス実行は argv 配列で行う（シェル経由禁止）

`foreign import` として文字列を1本渡して `sh -c` するような API は shell injection の
リスクを持ち込む。`nix`/`sops`/`gh` などを呼ぶ既存の bash スクリプトはそれぞれ引数を
配列で構築しているので、malgo 側も `List String`（コマンド名 + 引数配列）を受け取り、
`execvp` 相当（シェルを経由しない起動）で実行する形に合わせる。返り値は終了コード・
stdout・stderr の組。Lean インタプリタ側は `Toolchain.lean` と同じ `IO.Process.output`
パターンを再利用できるが、**Chez ランタイム側でシェル経由なしのプロセス起動手段が
提供されているかは未検証** — Phase 1 の実装時に最初に確認すべきリスクとして明記する。

## Implementation Plan

### Phase 0: バックエンドの復元

#### Task 0.1: `Backend/Scheme.lean` 一式の復元

**Goal**: #404 で削除された Lean 側一式を復元する

**Scope**: `lean/Malgo/Backend/Scheme.lean`（新規）、`lean/Malgo.lean`（import）、
`lean/Malgo/Driver.lean`（`compileScheme`）、`lean/Malgo/Prelude.lean`
（`Target` の `scheme` コンストラクタ）、`lean/Main.lean`（CLI引数解析・usage）、
`mise.toml`（`chezscheme` pin）

**Dependencies**: なし

**Steps**:
1. `git show <#404の親コミット>:lean/Malgo/Backend/Scheme.lean` などで #401 時点の
   最終形を取得する（#404 の PR 本文に変更ファイル一覧が全て載っている）。
2. `Join.lean` は #404 以降変更されていないことを確認済み（コミット履歴で確認した）
   なので、#401 が既に対応済みの cocase/destructor 削除（#387 対応）以外の
   追加調整は不要と見込む。`lake build` で最終確認する。
3. `Driver.lean`/`Prelude.lean`/`Main.lean` に `scheme` ターゲットの分岐を復元。

**Verification**: `lake build` が通ること。`malgo eval --target scheme
examples/malgo/Hello.mlg` が実行可能な Scheme ソースを出力すること。

---

#### Task 0.2: 正しさの golden gate を新設する

**Goal**: 「性能比較用、golden gate なし」から「zig-golden.sh 相当の正しさゲート」に
格上げする。これが今回の復活を過去2回と区別する核心。

**Scope**: `scripts/scheme-golden.sh`（新規、`scripts/zig-golden.sh` を土台にする）、
`.github/workflows/lean.yml`（CI ジョブ追加、`chezscheme` インストール）

**Dependencies**: Task 0.1, Task 0.3

**Steps**:
1. 73件の golden testcase を全て `--target scheme` でコンパイルし `scheme --script`
   で実行、インタプリタの golden 出力とバイト単位で比較するスクリプトを書く。
2. CI に `scheme-golden` ジョブを追加し、`chezscheme` のインストール手順を入れる。

**Verification**: `scripts/scheme-golden.sh` で 73/73 一致すること（Task 0.3 の
2件のバグ修正が前提）。

---

#### Task 0.3: 既知の2バグを修正する

**Goal**: #401 時点で残っていた `EmptyConstructor`（空コンストラクタの表示差異）と
`LabelGoto`（継続が `number->string` に漏れる）を修正する。golden gate を新設する
以上、放置したまま持ち込まない。

**Scope**: `lean/Malgo/Backend/Scheme.lean`

**Dependencies**: Task 0.1

**Steps**: 復元後に実際に再現させ、原因を特定して修正する（現時点では現象しか
分かっていない。原因調査自体がこのタスクの本体）。

**Verification**: `scripts/scheme-golden.sh` でこの2件を含めて一致すること。

---

### Phase 1: スクリプティングに必要なランタイム拡張

#### Task 1.1: 環境変数取得

**Goal**: `foreign import malgo_get_env : String# -> Maybe String#` を追加する
（`Prelude.mlg` に既存の `Maybe a = Nothing | Just a` を使う）

**Scope**: `runtime/malgo/Builtin.mlg`、`lean/Malgo/Sequent/Eval.lean`（インタプリタ側）、
`lean/Malgo/Backend/Scheme.lean` の `schemeRuntime`（`getenv` 相当）

**Dependencies**: Task 0.1

**Verification**: 新規 testcase（環境変数を設定して読み出す）

---

#### Task 1.2: 任意の終了コード

**Goal**: `foreign import malgo_exit_with : Int32# -> a` を追加する
（既存の `malgo_exit_success`/`malgo_exit_failure` は 0/1 固定）

**Scope**: Task 1.1 と同じ3箇所

**Dependencies**: Task 0.1

**Verification**: 新規 testcase

---

#### Task 1.3: サブプロセス実行（最大の欠落機能）

**Goal**: `nix`/`sops`/`gh`/`jq` などの既存呼び出しを malgo から再現できるようにする。
これがない限り `tasks/*.sh` の実質的なロジックは1本も移植できない。

**Scope**: `runtime/malgo/Builtin.mlg`、`lean/Malgo/Sequent/Eval.lean`
（`IO.Process.output` を `Toolchain.lean` と同じ形で再利用）、
`lean/Malgo/Backend/Scheme.lean` の `schemeRuntime`

**Dependencies**: Task 0.1

**Steps**:
1. `foreign import malgo_run_process : List String# -> (Int32#, String#, String#)`
   相当の API を決める（コマンド名 + 引数配列、終了コード・stdout・stderr を返す）。
2. インタプリタ側は `IO.Process.output` で実装する。
3. Chez ランタイム側の実装方法を最初に調査する。**Chez Scheme にシェルを経由しない
   プロセス起動手段（`execvp` 相当）があるかどうかが未検証のリスク** — もし
   `(system ...)` のようなシェル経由の手段しかない場合、追加のFFI実装が必要になる。

**Verification**: 新規 testcase（`echo` を argv 経由で呼び出し、終了コードと
stdout を確認する）。インタプリタと Chez の両方で一致すること。

---

### Phase 2: Nix 配布

#### Task 2.1: `chez-scheme` が aarch64-darwin でビルド/キャッシュされるか検証する

**Goal**: Design Choices の前提を実機で確認する

**Steps**: nix-config と同じ `aarch64-darwin` 上で `nix build nixpkgs#chez-scheme` を
実行し、ビルドが通るか、バイナリキャッシュから取得できるかを確認する。

**Verification**: ビルド成功。失敗した場合は代替案（Zig ネイティブバイナリを flake で
配布）に切り替える判断をここで下す。

---

#### Task 2.2: malgo の flake.nix を整備する

**Goal**: nix-config がバージョン固定で malgo（または生成済みの `.scm`）を取り込める
ようにする

**Scope**: malgo リポジトリ側に `flake.nix` を新設

**Dependencies**: Task 2.1

**Steps**: Task 2.1 の結果に応じて設計する。Chez 経路が通れば
「`.mlg` → `.scm` を生成する Nix 導出」を提供する形、Zig 経路に切り替えた場合は
malgo バイナリ自体をパッケージ化する形になる。

---

### Phase 3: パイロット移行

#### Task 3.1: nix-config の1本を試験移行する

**Goal**: `tasks/*.sh` のうち小さめの1本（`idempotency-check.sh` など）を `.mlg` に
移植し、実際に `darwin-rebuild build` 経路で動かして確認する。231行の
`sync-claude-plugins.sh` のような大物は、ここで得た知見を踏まえて Phase 4 で扱う。

**Dependencies**: Phase 1, Phase 2

**Verification**: 既存の `tests/test-*.sh` が期待する振る舞いと一致すること

---

### Phase 4: 本格移行

`tasks/*.sh` を1本ずつ、独立した PR で `.mlg` に移植する。優先順位・対象範囲は
Phase 3 の実績を見てから nix-config 側で決める。

## Risks

| リスク | 深刻度 | 対策 |
|---|---|---|
| `chez-scheme` が aarch64-darwin でキャッシュされない/ビルドが重い | 高 | Task 2.1 で早期検証。崩れたら Zig ネイティブ配布に切替 |
| Chez にシェル経由なしのプロセス起動手段がない | 高 | Task 1.3 で最初に調査。無ければ C FFI 追加実装が必要になる |
| 復元したバックエンドが再び「使われず削除」を三度目繰り返す | 中 | nix-config を実消費者として明記し、golden gate を必須にする（Task 0.2）。nix-config が malgo スクリプトを使わなくなったときのみ削除を検討する |
| `EmptyConstructor`/`LabelGoto` 以外にも未知の不一致がある | 中 | Task 0.2 の golden gate で網羅的に検出する |
| 型注釈を省略した動的スクリプトが増え、型推論の利点を活かせない | 低 | nix-config 側の `.mlg` に型注釈を書く文化をパイロット移行（Phase 3）で確立する |

## 補足：v4.0.0 のリリースホールドについて

`README.md` の「Current hold: v4.0.0 リリースPR(#398)はドラフト」という記述は
2026-07-27 の v4.0.0 リリース（#398 マージ、#385 milestone 完了）より前の状態を指す
古い記述で、実際には v4.0.0 は既にリリース済みである。本ロードマップは master に対する
新規の作業であり、リリースホールドの制約は受けない。
