# Issue #379: 残る Tier 2 証明の壁

Date: 2026-08-28

## Context

[#379](https://github.com/takoeight0821/malgo/issues/379) は #359 から切り出された、Lean 4 移植で `partial def` のまま残っている termination proof のバックログ。issue 本文には過去の bounded-effort 調査ノートがそのまま残されており、「難しそうだから」ではなく実際に手を動かして「ここで詰まる」と確認された記録になっている。今回はそのノートを起点に、現状のコードを再調査した上でプランを立てる。

### 現状(このプラン作成時点で判明した差分)

issue 本文は #359 からのコピーのため、1点だけ**古い**。commit `1d23788`(PR #457, "fix(lean): remove partial from Json.lean's fromJson? codecs (#379 slice)")で **`Malgo/Sequent/Core/Json.lean` の `fromJson` 系はすでに de-partialize 済み**。`patternFromJson`/`producerFromJson`/`consumerFromJson`/`statementFromJson`/`branchFromJson` はすべて plain `def` + `termination_by sizeOf j` / `decreasing_by all_goals simp_wf; all_goals omega` で証明済み。鍵となる補題 `getArr?_eq_ok`(`j.getArr? = .ok xs → j = .arr xs`、Lean core にはこの形の特徴づけ補題が存在しないため新規に追加)と、それを使う `jParseArrAttach`(依存ペアリストで「取り出した要素は元の Json より真に小さい」証明を運ぶヘルパ)がこの成功の型。commit メッセージ自身が「roundtrip 定理は別の大きなフォローアップ」と明記しており、これが残タスク。

残る3項目 + stretch goal の現状(今回のコード調査で確認):

| 項目 | ファイル | 状態 |
|---|---|---|
| 1. Json roundtrip 定理 | `Sequent/Core/Json.lean` | `fromJson`/`toJson` とも de-partialize 済み。`fromJson? (toJson p) = .ok p` の**証明**が未着手。`roundtrips`(451行目付近)は `#guard` によるサンプル1点ずつのスポットチェックのみで `∀`定理ではない |
| 2. Normalize elimination pass | `Backend/Zig/Normalize.lean`(147行) | 置換ヘルパ(`substStatement`等、33–86行目)は #376 で de-partialize 済み。`normalizeStatement`/`normalizeProducer`/`normalizeConsumer`/`normalizeBranch`(88–127行目)は今も `partial def` |
| 3. ToFun copattern compiler | `Sequent/ToFun.lean`(305行) | `classify`/`insertGrouped` は plain `def`。`fromExpr`/`fromStmts`/`fromClauses`/`fromClause`/`fromCoClauses`/`build`/`buildCase`/`buildLambda`/`buildObject` の9関数 mutual block(151–281行目)が `partial def` のまま |
| 4. stretch: Perceus/Reuse/RcCheck | `Backend/Zig/{Perceus,Reuse,RcCheck}.lean` | 3ファイルとも `termination_by`/`decreasing_by` 皆無。最優先度は低い |

各項目の技術的な詰まりどころ(調査で確認した具体的な理由):

**(1) Json roundtrip** — 純粋に証明量の問題。`fromJson`側は依存ペア(`jParseArrAttach`)経由で構造的再帰になっているが、roundtrip定理 `fromJson? (toJson p) = .ok p` を通すには (a) leaf型(`SourcePos`/`Range`/`Id`/`Tag`/`Literal`/`IdSort`/`ModuleName`)のroundtrip補題、(b) `jParseArrAttach`/`jList`をconstructorで組み立てたJson値に対して展開し `List.map`/`.mapM` に戻す unfolding 補題、(c) 上記を使う `Producer`/`Consumer`/`Statement`/`Branch`(+ `Pattern`)の mutual 構造的帰納、の3層が必要。既存の `Prelude.lean` の `sizeOf_*` 補題群や `SExpr.lean`/`Sequent/Core/Join.lean` の `toSExpr` が使っている `Array.sizeOf_lt_of_mem`/`Array.mem_toList_iff` は流用できる。**新規テクニックは不要**、ケース数(10コンストラクタ×2方向前後)が多いだけ。

**(2) Normalize** — issue の指摘通り、`.cut`/`.join` ケースは *substitution で書き換えた後の項* を再帰対象にする(`normalizeStatement (substStatement x k s)`)。さらに悪いことに、この `s` は生の入力の部分項ではなく **`normalizeProducer p` という別の相互再帰呼び出しの"出力"から取り出した値**(`match normalizeProducer p with | .mu _ x s => ...`)。つまりこれは「別の再帰呼び出しの結果に対してさらに再帰する」nested recursion で、Lean の `termination_by`/`decreasing_by` だけでは表現できない依存関係を持つ。加えて `Id`/`IdSort` の `uniq : Nat` フィールドにより Lean が自動導出する `sizeOf`(`Nat` の `sizeOf` は恒等関数)は、置換で若い uniq の名前を古い/新しい uniq の名前に置き換えるだけで値が増減しうるため、素朴な `sizeOf` を尺度に使うこと自体が **偽になりうる**(issue 本文の指摘を実装レベルで再確認)。

**(3) ToFun** — `CoClause'.body : ToFunM Fun.Expr` は「評価前のモナドアクション」を保持するデータなので、`build`/`buildCase`/`buildLambda`/`buildObject` の再帰自体は `Expr`/`CoClause` の構造を一切参照しない、`CoClause'` のリストだけで閉じた自己完結の再帰。今回の調査で `buildLambda`/`buildObject`/`buildCase` の実装を確認し、issue が示唆する尺度が具体的にどう効くか確定できた(詳細は Task 3 参照)。**新規テクニックは "2要素の辞書式順序" 1つだけ**で、(2)ほど深刻ではない。なお `Elaborate.lean`(`malgo2025` フラグ裏の並行実装)は issue 本文が「証明済み」と書いているが、**実際には未証明**(`termination_by` なし、全関数 `partial def`)。構造はほぼ同型なので、ToFun 側の証明戦略はそのまま転用できるはずだが、Elaborate.lean 自体の証明は #379 のスコープ外(下記 Design Choices で扱う)。

**(4) stretch (Perceus/Reuse/RcCheck)** — `RcCheck` の mutual group と `Reuse` の大部分は Normalize.lean と同型の Tier-3 パターンで低コスト。`Perceus` の `insertBlock ↔ goStmts ↔ goLive` の4者相互再帰は `goStmts → goLive` が同じ `stmts` リストを渡す(要素を消費しない)呼び出しを含むため、辞書式尺度が要る。`RcCheck.accessible` は `TreeMap Name Name` のポインタチェイスで、非巡回性という外部不変量に依存しており、fuel か非巡回性の証明のどちらかが要る。issue 自身が「最も価値が高いが最も労力が要る」と明記しており、このプランでは**最後に着手する任意項目**として扱う。

## Design Choices

### 方針: 4つの独立したタスクに分割し、着手順は難易度の昇順

各ファイルは互いに import 関係がなく(Json.lean は Sequent/Core、Normalize.lean は Backend/Zig、ToFun.lean は Sequent、Perceus/Reuse/RcCheck は Backend/Zig 内で Normalize.lean の後段)、証明も独立に進められる。並行 worktree で進めてよいが、**難易度と新規性の低い順(Json roundtrip → ToFun → Normalize → stretch)に着手することを推奨**する。理由:

- Json roundtrip は既存パターンの延長(ケース数が多いだけ)で、失敗リスクがほぼゼロ。先に片付けて #379 の一部を確実にクローズできる。
- ToFun は新規テクニックが1つ(2要素辞書式順序)だけで、`Prod.lex`/`WellFoundedRelation` の組み合わせは Lean 4 の標準機能で表現可能。中程度のリスク。
- Normalize は「別の相互再帰呼び出しの出力に対する再帰」という、このリポジトリで前例のないパターン。最もリスクが高く、時間を区切った調査が必要。
- stretch は Normalize で確立した手法(または別の手法)の後続作業として位置づけ、Normalize が完了するまで着手しない。

### Normalize.lean の termination 戦略(検討した代替案)

3つの案を検討した。

1. **Fuel(燃料)パラメータ方式**: `normalizeStatement : Nat → Statement → Option Statement` にして燃料が尽きたら `none` を返す形にすれば構造的再帰で即座に通る。しかし呼び出し側で燃料の上限を決め打ちする必要があり、「上限以内で必ず正規形に達する」ことを証明しない限り `none` が返る可能性を排除できない——結局「簡約列が有限で停止する」という同じ定理が必要になり、証明を先送りするだけで本質的な解決にならない。**却下**。
2. **多重集合順序などの汎用簡約順序**: 項書き換え理論の標準的な手法(multiset path ordering 等)を輸入する案。この cut-elimination 風の簡約系(`Cut (Mu x s) k → subst`, `Join m (Label j) s → subst`)に対しては汎用すぎて過剰投資になる。**却下**(将来 Perceus のような複雑な簡約系が増えたら再検討)。
3. **「不変量を運ぶ返り値」で well-founded recursion を書く(推奨)**: `Id`/`IdSort` の `uniq` を無視し純粋に AST のノード数を数える尺度 `nodeCount : Statement/Producer/Consumer/Branch → Nat`(手書き、`sizeOf` は使わない)を定義する。まず `nodeCount (substStatement x k s) = nodeCount s` (置換はノードを増減させない)を独立補題として証明する——これは `substStatement` の構造的帰納だけで閉じるので簡単。次に本体の停止性は、`normalizeStatement` の返り値の型を `(s : Statement) → {s' : Statement // nodeCount s' ≤ nodeCount s}` のように**不変量付き部分型**へ強化して well-founded recursion (`WellFounded.fix`, 尺度は `nodeCount`)で直接構築する。この強化により、`.cut`/`.join` ケースで `normalizeProducer p` の呼び出し結果 `s'` に対して「`nodeCount s' ≤ nodeCount p` (前段の再帰呼び出しの証明義務そのものから得られる)」が使え、`nodeCount (substStatement x k s') = nodeCount s' ≤ nodeCount p < nodeCount (.cut p k)` という連鎖で最終的な `decreasing_by` の義務を閉じられる。この「返り値の型に不変量を持たせて nested recursion を解決する」手法は Lean/Coq の well-founded recursion では知られたパターンだが、**このリポジトリでは前例がない**——issue が "novel proof technique" と呼んでいるのはこれ。最終的に外部に見せる関数シグネチャ(`Statement → Statement`)は `Subtype.val` で不変量を剥がして提供すればよく、呼び出し側 (`Driver.lean` 等)への影響はない。

Normalize タスクは、着手前に「不変量付き返り値」アプローチが Lean 4 の `termination_by`/`decreasing_by` 構文でどこまで自動化できるか(手書き `WellFounded.fix` まで降りる必要があるか、`termination_by` の `decreasing_by` 内で補題を呼ぶだけで済むか)を30分程度の時間box調査してから本実装に入ること。もし部分型アプローチ自体が Lean 側の制約(相互再帰する4つの部分型付き関数の型を書き下す煩雑さ等)で破綻した場合は、素直にコードを詫びて issue にその旨を記録し、次点で「`.cut`/`.join` の1ステップ簡約と、正規形に達するまでのループを分離する」(`reduceStep : Statement → Statement ⊕ Statement`(Redex ならもう1項、正規形ならその値) + 外側で `Nat` 燃料 + 燃料が尽きないことを別途証明)という2案目に切り替える判断をタスク側に委ねる。

### ToFun.lean の termination 戦略

`accessorTotal(clauses) := Σ c.copats.length`、尺度は `(accessorTotal clauses, clauses.length)` の辞書式順序(`Prod.lex` または `WellFoundedRelation` の `invImage` + `Prod.lex_wf`)。`buildLambda`/`buildObject` は最初の成分を strict decrease、`buildCase` は最初の成分を不変に保ち2番目を strict decrease(`build` 自体は尺度に対して neutral な純粋ディスパッチ)。必要な補助補題:

- `classify` が `.function` を返すとき、全 `clauses` の `copats` が `.applyP` で始まる(`buildLambda` 内の `throw` 分岐が到達不能であることの証明に必要)。`.field`(→`buildObject`)についても同様に `.projectP` 始まりを保証。
- `List.partition`/`List.mapM` が長さに関して満たす標準的な事実(`noCoPats` が空でないことは `classify = .case` の定義から従うはず)。

`Elaborate.lean` は本 issue のスコープ外だが、構造がほぼ同型(`Accessor`/`ElabClause.accessors` ↔ `CoPat'`/`CoClause'.copats`)なので、ToFun 側で確立した尺度と補助補題はコピーしてそのまま使える可能性が高い。Task 3 の完了後、時間があれば別 issue として切り出すことを提案する(このプランには含めない)。

### スコープに含めないもの

- `Elaborate.lean` の証明(上記の通り、独立 issue 向けの提案に留める)
- Perceus/Reuse/RcCheck の**証明**そのもの(Task 4 は termination だけを対象とし、issue のもう一段先にある「Perceus の出力は常に RcCheck-valid」という correctness 定理は範囲外——これは #379 のチェックリストでも "stretch goal" の下にネストされた別ゴールであり、今回のプランでは着手判断を Task 4 完了後に持ち越す)

## Implementation Plan

### Task 1: Json roundtrip 定理

- **Goal**: `fromJson? (toJson p) = .ok p` を `Pattern`/`Producer`/`Consumer`/`Statement`/`Branch`(必要なら `Program`/`Resource` も系として)について証明し、既存の `#guard roundtrips ...` によるスポットチェックを `∀`定理に置き換える(あるいは定理を追加した上でスポットチェックは回帰用に残す)。
- **Scope**: `lean/Malgo/Sequent/Core/Json.lean` のみ。必要なら `lean/Malgo/Prelude.lean` に汎用補題を追加。
- **Dependencies**: none
- **Steps**:
  1. leaf型のroundtrip補題を先に用意する: `SourcePos`/`Range`/`Id`/`Tag`/`Literal`/`IdSort`/`ModuleName`/`ArtifactPath`(`Malgo/Interface.lean` にある型を含む)。これらは非再帰か浅い再帰なので `simp`/`rfl` 級で閉じるはず。
  2. `jParseArrAttach`/`jList` を、`toJson`側が実際に組み立てる `Json.arr #[...]` 形の値に適用したときにどう簡約されるかの unfolding 補題を書く(`jParseArrAttach (Json.arr xs) = .ok (xs.toList.attach.map ...)` 相当)。ここが一番手間がかかる箇所。
  3. `Pattern` の roundtrip を単独(非 mutual)で先に通す — 最小のケースで手法を確立してから mutual group に進む。
  4. `mutual` ブロックで `Producer`/`Consumer`/`Statement`/`Branch` の roundtrip を相互帰納で証明。各コンストラクタケースは `simp only [producerToJson, jParseArrAttach, ...]` で `fromJson`側の分岐を評価し、IH を適用する形になるはず。
  5. `Program`/`Resource` インスタンスの roundtrip を系として追加(コストが低ければ)。
  6. 既存の `#guard roundtrips ...` は残してよい(regression golden として)が、コメントで「本体の証明は上記定理」と明記する。
- **Verification**: `lake build`(warning/error なし、`sorry` なし)。`mise run test` が既存 golden に影響しないことを確認(振る舞いは変えていないので当然パスするはず)。

### Task 2: ToFun copattern compiler の termination

- **Goal**: `Sequent/ToFun.lean` の `fromExpr`/`fromStmts`/`fromClauses`/`fromClause`/`fromCoClauses`/`build`/`buildCase`/`buildLambda`/`buildObject` から `partial` を除去し、`termination_by`/`decreasing_by` で証明する。
- **Scope**: `lean/Malgo/Sequent/ToFun.lean` のみ。
- **Dependencies**: none
- **Steps**:
  1. `accessorTotal : List CoClause' → Nat := fun cs => (cs.map (·.copats.length)).sum` を定義。
  2. `classify` の返り値と `copats` の形の対応を述べる補助補題(`classify clauses = .function → ∀ c ∈ clauses, ∃ x pat rest, c.copats = .applyP x pat :: rest` 等)を証明する。`buildLambda`/`buildObject` 内の到達不能な `throw` 分岐を消すのに必要。
  3. `mutual` ブロック全体(9関数)に対して `termination_by` で尺度を割り当てる。`fromExpr`/`fromStmts`/`fromClauses`/`fromClause`/`fromCoClauses` は元の `Expr .rename`/`Clause .rename` 側の構造で通常の `sizeOf` 降下(既存パターンと同型のはず)。`build`/`buildCase`/`buildLambda`/`buildObject` は `(accessorTotal clauses, clauses.length)` の辞書式順序。2つの異なる尺度を跨ぐ mutual group を Lean の `termination_by` でどう表現するか(関数ごとに異なる measure function を書けるはずだが、共通の well-founded relation 型に落とす必要がある点)を最初に確認する。
  4. `decreasing_by` を埋める。`buildCase` の `rest.length < clauses.length` は `List.partition` の性質(`noCoPats` が空でない)から。`buildLambda`/`buildObject` の accessorTotal 減少は手順2の補題と `List.sum` の単調性から。
- **Verification**: `lake build`。`mise run test`(挙動不変であることの確認)。`grep -n "partial def" lean/Malgo/Sequent/ToFun.lean` がヒットなしになること。

### Task 3: Normalize.lean の elimination pass の termination

- **Goal**: `Backend/Zig/Normalize.lean` の `normalizeStatement`/`normalizeProducer`/`normalizeConsumer`/`normalizeBranch` から `partial` を除去する。
- **Scope**: `lean/Malgo/Backend/Zig/Normalize.lean` のみ。
- **Dependencies**: none(Task 1/2 と並行可能だが、リスクが最も高いので着手順は最後を推奨——Design Choices 参照)
- **Steps**:
  1. `nodeCount : Statement/Producer/Consumer/Branch → Nat` を手書きで定義(`Id`/`IdSort` の `uniq` を無視し、ノード数だけを数える。相互再帰する4つの `def` になる)。
  2. `nodeCount (substStatement x k s) = nodeCount s` (および `substProducer`/`substConsumer`/`substBranch` 版)を、`substStatement` 等の構造に対する構造的帰納で証明する独立補題。
  3. 「不変量付き返り値」アプローチ(Design Choices 参照)を Lean の `termination_by`/`decreasing_by` でどこまで書けるか30分程度で時間box調査する。イケそうなら本実装、厳しければ2段階分離案(`reduceStep` + 燃料)に切り替える判断をこのタスクの担当者に委ねる——どちらの場合も、選んだ理由と却下した理由を実装コミットのメッセージか `docs/reports/` に短く記録すること(将来の担当者が同じ調査を繰り返さないため、Task 1で参照した `docs/reports/2026-07-22-issue-359-proof-introduction.md` と同じ役割)。
  4. 相互再帰4関数すべてに termination 証明を通す。
- **Verification**: `lake build`。`bash scripts/zig-golden.sh`(Normalize.lean は Zig バックエンドの正規化パスなので、golden 出力が1バイトも変わらないことまで確認する——termination 証明の追加自体は意味論を変えないはずだが、万一 `decreasing_by` を通すために簡約規則を書き換えた場合に検出できるようにするため)。`mise run test`。

### Task 4 (stretch, optional): Perceus/Reuse/RcCheck の termination

- **Goal**: `RcCheck` の mutual group(`goBlock`/`goStmts`/`goTerm`)と `Reuse` の `pairGo`/`reuseBlockWithSunk` を de-partialize する(Normalize.lean と同型の Tier-3 パターン)。時間が許せば `Perceus` の4者相互再帰(`insertBlock`/`goStmts`/`goLive`/`insertTerminator`)にも着手する。
- **Scope**: `lean/Malgo/Backend/Zig/{Perceus,Reuse,RcCheck}.lean`
- **Dependencies**: Task 3 の完了(同じ `Backend/Zig` 内の `Statement`/`Block`/`Stmt` IR に対する `nodeCount` 系の補題を再利用できる可能性が高いため、Task 3 の成果を踏まえてから着手する)
- **Steps**:
  1. `RcCheck.goBlock`/`goStmts`/`goTerm` と `Reuse.pairGo`/`reuseBlockWithSunk` を先に片付ける(既存パターンの延長、`Reuse.pairGo` の `reuseHint` 分岐だけ `List.dropWhile` がリストの部分列であることの小さな補題が要る)。
  2. `Perceus` は `goStmts → goLive` が同じ `stmts` を渡す(要素を消費しない)ホップを含むため、`(stmts.length, phase)` のような辞書式尺度が必要。あるいは `goStmts`/`goLive` を1つの関数に統合するリファクタも選択肢として検討する。
  3. `RcCheck.accessible` は `aliasRoot : TreeMap Name Name` のポインタチェイスで、非巡回性という外部不変量に依存する。Perceus/Reuse が非巡回なグラフしか作らないことを証明するか、fuel を足すかのどちらかが要る——どちらも本タスクの中で新規に確立する必要がある。
- **Verification**: `lake build`。`bash scripts/zig-golden.sh` と `bash scripts/zig-deep-recursion.sh`(RC 周りの変更なので leak-check golden とdeep recursion gate の両方)。

## Verification

各タスク完了ごとに:
- `lake build`(warning 0、`sorry` 0、対象関数から `partial` が消えていること)
- `mise run test`(golden 含む全体スイート)

Task 3/4 は Zig バックエンドの意味論に関わるため追加で:
- `bash scripts/zig-golden.sh`(73/73 golden 出力の完全一致 + leak なし)
- Task 4 で Perceus/RcCheck に触れた場合は `bash scripts/zig-deep-recursion.sh` も実行

全タスク完了後:
- `mise run test` 全体を最終確認として再実行
- issue #379 のチェックリスト(fromJson / Normalize / ToFun / stretch)を実施結果で更新し、未完了(Task 4 の Perceus 相互再帰、あるいはさらに先の correctness 定理)があれば新しい issue として切り出す

## Risks

| Risk | Mitigation |
|------|------------|
| Normalize.lean の「不変量付き返り値」アプローチが Lean 4 の `termination_by` 構文では表現しきれない(手書き `WellFounded.fix` が必要になり、相互再帰4関数分の展開が煩雑) | Task 3 の Step 3 で30分の時間box調査を明示的に設け、ダメなら2段階分離(`reduceStep` + 燃料)に切り替える。判断基準と結果を記録して次の担当者が調査を繰り返さないようにする |
| Json roundtrip 定理がケース数(10コンストラクタ×mutual 4型+Pattern)で単純に長時間かかる | 新規テクニックは無いので失敗リスクは低いが、着手前に「1ケースだけ手で通してパターンを確立してから残りを機械的に繰り返す」進め方を徹底し、時間見積もりのブレを抑える |
| ToFun の2尺度(`accessorTotal`, `clauses.length`)を跨ぐ `termination_by` の書き方が Lean 側の制約に当たる | 事前に `Prod.lex`/`WellFoundedRelation` の他の利用例(このリポジトリの `Query/Engine.lean` の `((...).size, frontier.size)` パターン)を参照して同じ書き方を踏襲する |
| Task 4 の `Perceus`/`RcCheck.accessible` が想定より深い(非巡回性の証明が新たな不変量の追加を要求する) | 最優先度を最後に置き、Task 1–3 が完了した時点で改めて着手判断する。issue 本文もこれを "stretch goal" と明記しており、未完了のまま残しても #379 の主要3項目はクローズできる |
| termination 証明の追加時に誤って簡約規則そのものを書き換えてしまい、意味論が変わる | Task 3/4 で `zig-golden.sh`/`zig-deep-recursion.sh` を必須の検証手順にし、golden 出力のバイト一致を機械的に確認する |
