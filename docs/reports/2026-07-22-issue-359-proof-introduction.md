# issue #359: Lean 4 移植への証明導入

## 背景

Lean 4 移植（`lean/`、M0からM9まで完了、#358）には、当初「証明を持たない」という方針があった。
`lean/README.md` はこれを「構造的でない再帰は `partial def` で書く」と明文化していた。
移植全体には約343個の `partial def` があり、その正しさはすべて Haskell 版とのゴールデンファイル一致に委ねられていて、機械的に検証された性質は一つもなかった。

issue #359 は、この方針を「一切の証明を持たない」から「段階的に証明を導入する」へ切り替え、実際にどこまで証明できるかを調査する作業として立てられた。
本レポートは、その結果を三つの階層（Tier）に分けて記録する。

## Tier 1: `Data/IntMap.lean` の不変条件

`IntMap` はビッグエンディアンの二分木（Patricia trie）で、もとから構造的な再帰しか使っておらず、`partial def` を含まない。
このため、最初に着手する対象として選ばれた。

ここでは三つの定理を証明した。

- **`WF`**：木の形そのものに対する整構造性の不変条件である。内部ノード `bin prefix mask l r` について、左右の部分木が空でないこと、そして左の部分木に格納されたすべてのキーが `prefix` と `mask` より上位のビットで一致し、かつ `mask` の位置のビットが0であること（右は対称に1であること）を要求する。
- **`lookup_insert`**：`lookup? key (insert key val m) = some val`。あるキーで値を挿入した直後に同じキーで検索すれば、挿入した値が返ることを述べる。
- **`insert_wf`**：`WF m → WF (insert k v m)`。挿入操作は整構造性を保存する。

`insert_wf` の証明には、ビット演算に関する六つの補題、部分木のキー集合とprefix/maskの対応を述べる `Represents` という不変条件、`link_tip_wf`、`hasKey_link`/`hasKey_insert` など、相応の補助定理が必要になった。

この過程で `WF.bin` の定義自体を修正する必要が生じた。
もとの定義では、左右の部分木が空である縮退したノードに対して何の制約もなく、その状態では `insert_wf` が成り立たないという反例が実際に構成できた。
そこで `WF.bin` に「左右の部分木は空でない」という条件を追加した。
`WF.bin` を参照する箇所は `IntMap.lean` の外に存在しなかったため、この変更は他のコードに影響を与えない。

## Tier 2: パスの再帰に関する構造的不変条件

Tier 2 は、`partial def` のうち特に価値の高いものについて、そのパス自身の再帰構造に対する帰納法で不変条件を証明する層である。

### `Sequent/SaturateCtor.lean`: 飽和済みコンストラクタ呼び出しの排除

`SaturateCtor` は CPS 変換の直前に走るパスで、完全に飽和したカリー化コンストラクタ呼び出し（`Cons x xs` など）を、コンストラクタ自身のクロージャを経由せず `Fun.Construct` へ直接書き換える。
この最適化はすべてのバックエンド（Eval/Scheme/Zig）で共有されている。

証明に先立って、`peel`/`goExpr`/`goBranch` を `partial def` から通常の `def` へ書き換える必要があった。
`partial def` は Lean において実行方程式を一切持たない。
`unfold` も `simp` も帰納法も適用できないことを実際に確認した。
つまり、`partial def` のままでは、その出力に関する証明はいかなる形でも成立しない。

de-partial化のあとで、`saturateProgram_no_saturated_spine` を証明した。
`saturateProgram` の実行後、出力のどこにも完全飽和したカリー化コンストラクタ呼び出しが残っていないことを述べる定理である。
実際には、より一般的な補題 `goExpr_trySaturate_none` を式引数について全称量化した形で証明し、「木のどこにも残らない」という強い形はそこから直接得られる。

### `Malgo/Module.lean`: `ModuleName` の順序型クラス

`Query.Engine` は `Std.TreeSet ModuleName`/`Std.TreeMap ModuleName _` を多用するが、`ModuleName` の派生 `Ord` インスタンスには、`Std.TreeSet`/`TreeMap` が要求する型クラス（`Std.TransCmp`、その前提となる `Std.OrientedCmp`）が一つも付いていなかった。
実際に `Std.TransCmp (compare (α := ModuleName))` の型クラス探索を試みると失敗し、同じ探索が `Nat`/`String` では成功することを確認した。

`ModuleName` の派生 `compare` は、両方の値のコンストラクタで場合分けし、同じコンストラクタ同士の場合は最終的に `String.compare` へ落ちる関数である。
この `compare` が実際に推移的な前順序であることを、コンストラクタで場合分けし `String` 自身のインスタンスに帰着させる形で証明した。

この証明自体は Malgo の意味論に関するものではない。
Lean の派生した `compare` が、コンテナライブラリの要求する法則を実際に満たしていることを示すものである。
この型クラスが揃ったことで、`Query.Engine` 内の `Std.TreeSet`/`TreeMap` について初めて証明が可能になった。

### `Query/Engine.lean`: `reverseDepClosureGo` の停止性

`reverseDepClosureGo` は、依存関係グラフ（`depsOf : ModuleName → Set ModuleName`）上で「`target` に依存するモジュール群」を幅優先探索で求める。
グラフは循環しうるため、この関数はもともと `partial def` だった。

停止性の証明には、辞書式順序の測度 `((depsUniverse depsOf \ acc).size, frontier.size)` を用いた。
`depsUniverse` は `depsOf` が言及するモジュール全体の固定された集合である。
再帰の各段階は二つの場合に分かれる。

- 次の段階で追加される集合（`next`）が空でない場合、`acc` は真に新しい要素を得るので、`depsUniverse \ acc` の要素数が真に減る。
- `next` が空の場合、`acc` は変化しないが、次の呼び出しでは `frontier` が `next`（空集合）になり即座に停止する。この一段階では、測度の第一成分は変わらず、第二成分（`frontier.size`）が0へ真に減る。

「`acc` が真に大きくなる」場合を扱うには、「ある集合のすべての要素がもう一方の集合にも属し、かつ後者にはそこにない要素が存在するなら、前者は後者より真に小さい」という一般的な事実が必要になる。
`Std.TreeSet`/`TreeMap` にはこれに相当する補題が存在しなかった。
既存の恒等式（`isEmpty_diff_iff`、`size_diff_add_size_inter_eq_size_left`、`size_inter_le_size_right`）だけからこの事実を導出し、`Std.TreeSet.size_le_of_forall_mem`/`size_lt_of_forall_mem_of_not_mem` として `Prelude.lean` に追加した。

この導出では `Std.TreeSet` の外延性（`Equiv.of_forall_mem_iff`）を経由する道を避けた。
その道は `LawfulEqCmp` という型クラスを要求するが、`ModuleName` はこれを実際に満たさない。
`ModuleName.artifact` は、`relPath` が同じで他のフィールドが異なる二つの値を `.eq` と比較するよう設計されており（探索経路が違っても同じモジュールとして扱うための意図的な設計）、この二つの値は `compare` で等しくても Lean の `=` としては等しくない。
`LawfulEqCmp` を安易に前提とする証明は、この型に対しては成り立たない。

## Tier 3: 機械的な整理

Tier 3 は、新しい定理を一つも追加しない層である。
再帰が実際には構造的であるにもかかわらず、`.map`/`.foldl`/`.attach` の裏に隠れているために Lean の停止性検査器が自動では認識できない `partial def` から、単に `partial` を外す作業である。

対象は `Sequent/Core/{Full,Flat,Join}.lean` の `toSExpr` 系列、`Json.lean` の `toJson` 方向、`Rename/Pass.lean` の純粋なヘルパー群、`Syntax.lean` の `dump`/`boundVars`/`freevars`、`SExpr.lean` の印字関数、`Debug/PrettyIR.lean` の約15個の描画関数、`Debug/DiffView.lean` の差分ヘルパー群、そして前述の `SaturateCtor.lean` である。
それぞれについて、`termination_by sizeOf` による測度か、フィルタや分解を経た再帰呼び出しを親の要素数に結びつける明示的な補題（`mem_sortAssocAscending` および `sizeOf_*_of_mem` 系列。いずれも `Prelude.lean` にある）のどちらかが必要だった。
この階層では、いずれの関数についても実行時の挙動は変わっていない。

## 残された課題

四つの候補のうち三つは、束縛された作業量では証明が完了せず、後続の issue #379 へ切り出した。

- `Sequent/Core/Json.lean` の `fromJson`。再帰自体は `Json` 引数について構造的だが、部分項の取り出しが `Except` を返す不透明な抽出関数（`.getArr?`/`.getStr?`）を経由しており、それらの特性を述べる補題が `Std`/core に見当たらない。
- `Backend/Zig/Normalize.lean` の簡約パス。再帰が入力の部分項ではなく、置換によって書き換えられた項の上で行われる。素朴な `sizeOf` による測度はこの関数に対して偽ですらありうることを確認した。`Nat` の `sizeOf` が恒等関数であるため、置換によって単調に増加する一意番号を持つ構造では、置換後に `sizeOf` が増加しうる。
- `Sequent/ToFun.lean` の余パターンコンパイラ。9個の関数からなる相互再帰全体が、異なる型にまたがる一つの停止性証明を共有する必要があり、この移植ではまだ試みていない技法（辞書式測度の異種混合）を要求する。

これらの詳細な調査記録は #379 に引き継いだ。
`Perceus`/`Reuse`/`RcCheck` に対する不変条件の証明も、価値は最も高いが着手前の前提作業（`Perceus`/`RcCheck` 自身の相互再帰に対する停止性証明）が必要であり、同じく #379 の対象として残している。

## 副産物

`Query/Engine.lean` の証明の過程で見つかった二つの事実は、issue #359 の範囲を超えて有用である。

一つは、この Lean プロジェクトが Mathlib に依存していないという制約である。
`by_contra`、`push_neg`、`set ... with h`、`rintro` は標準的な Lean の戦術に見えるが、実際には Mathlib 由来であり、このプロジェクトでは使えない。
使用すると「unknown tactic」という、原因の分かりにくいエラーになる。
`rcases`/`obtain` と場合分け、`Nat.eq_zero_or_pos`、素の `let`、`intro` と `obtain` の組み合わせで代替できる。

もう一つは、`Std.TreeSet.size_le_of_forall_mem`/`size_lt_of_forall_mem_of_not_mem` という二つの補題である。
これらは `Query/Engine.lean` に限らず、今後 `Std.TreeSet`/`TreeMap` を使うどの証明からも再利用できる。
