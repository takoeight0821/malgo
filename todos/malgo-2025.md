# Malgo Version 2025 — ziku完全移植

## Goal
ziku (Lean 4) の全機能をmalgo (Haskell) に移植し、データ/コデータの二項性、シークエント計算IR、行多相型、label/goto制御フロー、Schemeバックエンドを備えた次世代malgoを実現する。

## 差分サマリ

| 機能領域 | malgo (現在) | ziku | 移植要否 |
|---------|-------------|------|---------|
| コデータ型 | 限定的（Object/cocase） | 完全なコパターンマッチング (#記法) | 要拡張 |
| label/goto | なし | 構造化非局所制御フロー、⊥型 | 新規 |
| 行多相性 | なし | レコード・バリアント行変数 | 新規 |
| ⊥型 (Bottom) | なし | goto式の型、型伝播 | 新規 |
| Elaborate pass | なし | コパターン→レコード/ラムダ脱糖 | 新規 |
| 型推論 | InferPass/RefinePass（スキップ可） | 制約ベースHM + let多相 + ⊥伝播 | 要刷新 |
| Translate | ToFun/ToCore（段階的） | Surface→IR直接変換 | 要見直し |
| IR | Fun/Core/Flat/Join（4段階） | λμμ̃ Producer/Consumer/Statement | 既存活用+拡張 |
| 大ステップ評価 | なし（小ステップのみ） | 小ステップ＋大ステップ | 新規 |
| Schemeバックエンド | なし | CPS変換→Chez Scheme | 新規 |
| 組み込み関数 | 100+個のforeign import | 最小限12個 (str/IO中心) | 統合 |
| インポート | Module/Interface | import式 + .zikiシグネチャ | 要拡張 |
| 衛生的名前 | Uniq ID | #プレフィックスシステム | 既存で十分 |
| レコード型 | record構文あり | 行多相レコード { x:Int | r } | 要拡張 |
| バリアント型 | data定義 | 行多相バリアント [Con a | r] | 要拡張 |
| 構文 | Regular + CStyle | 独自構文 (match/cocase/label/goto) | 要拡張 |

## Tasks

### Phase 1: 構文拡張 (Surface Language)
- [ ] 1.1 Syntax.hs にコパターン構文を追加 (#記法、Copattern型、Accessor型)
- [ ] 1.2 Syntax.hs に label/goto 式を追加 (Label, Goto コンストラクタ)
- [ ] 1.3 Syntax.hs に行多相型を追加 (レコード型・バリアント型に行変数)
- [ ] 1.4 Syntax.hs に ⊥型 (Bottom) を追加
- [ ] 1.5 Syntax.hs に tilde型 (~T, コ値型) を追加
- [ ] 1.6 Parser に新構文のパース実装 (match/cocase/label/goto/#/行多相)
- [ ] 1.7 パーサのテストケース追加 (testcases/ + golden tests)

### Phase 2: Elaborate パス (新規)
- [ ] 2.1 Malgo/Elaborate.hs を新規作成 (ElaboratePass)
- [ ] 2.2 コパターン→レコード/ラムダ変換の実装
- [ ] 2.3 衛生的名前生成 (#プレフィックス) の実装
- [ ] 2.4 パターンガード→外側match式変換
- [ ] 2.5 Elaborate パスのテスト追加

### Phase 3: 型システム刷新
- [ ] 3.1 制約ベース型推論エンジンの実装 (Malgo/Infer.hs 刷新)
- [ ] 3.2 let多相性 (レベルベース generalization) の実装
- [ ] 3.3 行多相性の型推論 (レコード・バリアント行変数の単一化)
- [ ] 3.4 ⊥型の伝播ルール実装 (BottomProp制約)
- [ ] 3.5 コ値型 (~T) の型チェック実装
- [ ] 3.6 型推論エラーメッセージの充実
- [ ] 3.7 型推論テストの追加

### Phase 4: IR拡張 (λμμ̃計算の完全実装)
- [ ] 4.1 IR に Producer/Consumer/Statement の3カテゴリ構造を強化
- [ ] 4.2 μ抽象 (継続キャプチャ) の追加
- [ ] 4.3 μ̃抽象 (値キャプチャ) の追加
- [ ] 4.4 cocase Producer (デストラクタマッチング) の追加
- [ ] 4.5 case Consumer (コンストラクタマッチング) の拡張
- [ ] 4.6 destructor Consumer の追加
- [ ] 4.7 fix Producer (固定点) の追加
- [ ] 4.8 dataCon Producer の拡張
- [ ] 4.9 record Producer の行多相対応
- [ ] 4.10 binOp/ifz/call/builtin Statement の追加・拡張
- [ ] 4.11 externalCall Statement の追加

### Phase 5: Surface→IR変換 (Translate パス)
- [ ] 5.1 Malgo/Sequent/Translate.hs の新規作成 or ToFun/ToCoreの刷新
- [ ] 5.2 基本式の変換 (var, lit, binOp, if)
- [ ] 5.3 let/letRec の変換 (μ/μ̃変換)
- [ ] 5.4 ラムダ/適用の変換 (cocase/destructor)
- [ ] 5.5 パターンマッチングの変換 (case consumer + join points)
- [ ] 5.6 コデータの変換 (cocase producer)
- [ ] 5.7 label/goto の変換 (μ抽象)
- [ ] 5.8 レコード/フィールドアクセスの変換
- [ ] 5.9 extern の変換 (externalCall)
- [ ] 5.10 Translate パスのテスト追加

### Phase 6: 評価器拡張
- [ ] 6.1 小ステップ評価器の μ/μ̃ reduction 対応
- [ ] 6.2 大ステップ評価器の新規実装 (Malgo/Sequent/BigStepEval.hs)
- [ ] 6.3 label/goto の評価 (非局所ジャンプ)
- [ ] 6.4 コデータ (cocase) の評価
- [ ] 6.5 行多相レコードの評価
- [ ] 6.6 extern (外部呼び出し) の評価
- [ ] 6.7 組み込み関数の追加 (strLen, strAt, strSub, etc.)
- [ ] 6.8 評価器テストの追加

### Phase 7: Schemeバックエンド (新規)
- [ ] 7.1 Malgo/Backend/Scheme.hs の新規作成
- [ ] 7.2 Producer→Scheme変換 (var, lit, μ, cocase, record, fix, dataCon)
- [ ] 7.3 Consumer→Scheme変換 (covar, μ̃, case, destructor)
- [ ] 7.4 Statement→Scheme変換 (cut, binOp, ifz, call, builtin)
- [ ] 7.5 識別子マングリング (#→_hash_, ギリシャ文字変換)
- [ ] 7.6 文字列エスケープ処理
- [ ] 7.7 Chez Scheme ランタイムサポート
- [ ] 7.8 Schemeバックエンドのテスト追加

### Phase 8: パイプライン統合
- [ ] 8.1 Driver.hs のパイプライン更新 (Elaborate パス追加)
- [ ] 8.2 CLIに新オプション追加 (--scheme, --big-step, --eval モード拡張)
- [ ] 8.3 インポートシステムの拡張 (import式、.zikiシグネチャ)
- [ ] 8.4 Features.hs にフィーチャーフラグ追加 (Malgo2025)

### Phase 9: ランタイム・標準ライブラリ
- [ ] 9.1 runtime/malgo/Builtin.mlg の更新 (コデータ/行多相対応)
- [ ] 9.2 runtime/malgo/Prelude.mlg の更新 (新構文対応)
- [ ] 9.3 zikuのサンプルプログラムの移植 (examples/)

### Phase 10: テスト・検証
- [ ] 10.1 zikuのゴールデンテストの移植 (parser/infer/ir-eval/scheme)
- [ ] 10.2 小ステップ↔大ステップ一貫性テスト
- [ ] 10.3 MALインタプリタ例の移植テスト
- [ ] 10.4 全テスト通過確認 (mise run test)
