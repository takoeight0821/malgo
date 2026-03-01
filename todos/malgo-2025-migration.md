# Malgo 2025 Migration

## Goal
ziku (Lean 4) の全機能を malgo (Haskell) に移植する。Phase 1 の並列実装は完了。Phase 2 以降で統合・stub 実体化・E2E テストを進める。

## Phase 1: 並列実装 (完了)

- [x] Unit 1: Feature Flag Infrastructure — Malgo2025 フラグ追加 (2026-03-01, 9209c2b1)
- [x] Unit 2: Surface Language Extensions — Label/Goto, TyBottom, TyTilde, 行多相 (2026-03-01, e5f53141)
- [x] Unit 3: IR Extensions — Fix, Mu, Cocase, Destructor, ExternalCall, BinOp, Ifz (2026-03-01, 935584be)
- [x] Unit 4: Elaborate Pass — コパターン→レコード/ラムダ変換 (2026-03-01, 6d32dbc7)
- [x] Unit 5: Type Inference Engine — 制約ベース HM 推論 (2026-03-01, e5f53141)
- [x] Unit 6: Scheme Backend — Join IR → Chez Scheme コード生成 (2026-03-01, 1b4fc7d4)
- [x] Unit 7: Big-Step Evaluator — 大ステップ評価器 (2026-03-01, e5f53141)
- [x] Unit 8: Runtime/Examples/Tests — Builtin/Prelude 拡充 (2026-03-01, 72c1e89d)

## Phase 2: 統合作業

現状: ビルド成功、832 テスト中 0 失敗。

### テスト修正 (完了)
- [x] Fib/FibCopattern の zipWith 重複名エラー修正 (14 件: Elaborate, Rename, ToFun, ToCore)
- [x] BigStepEval 新テストケース失敗修正 (ArithInt32, StringOps, MapFilter)
- [x] Eval 新テストケース失敗修正 (ArithInt32, StringOps, MapFilter)

### パイプライン統合 (Driver.hs) (完了)
- [x] ElaboratePass をパイプラインに組み込み (#malgo-2025 pragma で自動有効化)
- [x] InferPass をパイプラインに組み込み (`--infer` フラグ)
- [x] SchemePass をパイプラインに組み込み (`--target scheme` フラグ)
- [x] BigStepEvalPass をパイプラインに組み込み (`--eval-mode bigstep` フラグ)

### 品質確認 (完了)
- [x] package.yaml: 全新モジュールの exposed-modules 整理 (source-dirs: src で自動)
- [x] ビルド・テスト通過確認 (832 tests, 0 failures)

## Phase 3: Stub 実装の実体化

- [ ] Rename/Pass.hs: Label/Goto の実際の rename 実装
- [ ] ToFun.hs: Label/Goto → Fun IR 翻訳実装
- [ ] ToCore.hs: Fix → Core IR 翻訳実装
- [ ] Eval.hs: Mu/Cocase/Destructor/ExternalCall/BinOp/Ifz の評価実装
- [ ] BigStepEval.hs: 同上の評価実装

## Phase 4: 新構文を使ったテスト・サンプル

- [ ] label/goto を使ったテストケース作成
- [ ] bottom 型/tilde 型を使ったテストケース作成
- [ ] 行多相型を使ったテストケース作成
- [ ] コデータ型 (Elaborate 経由) の E2E テスト
- [ ] Scheme バックエンド出力の検証テスト
