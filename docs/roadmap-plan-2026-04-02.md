# Malgo 今後の実装計画

作成日: 2026-04-02

## 概要

以下の4機能を実装したい:

1. **Minimum Version Selection + マークル木ベースのパッケージマネージャ**
2. **LSP (Language Server Protocol)**
3. **Malgoで実装された完璧なLispインタプリタ**
4. **ネイティブコンパイラ**

## 現状分析

| 領域 | 現状 | 主要ファイル |
|------|------|-------------|
| モジュール | ファイルベースのimport、`.malgo-work/`キャッシュ、バージョニングなし | Module.hs, Interface.hs |
| LSP | インフラなし。ただしAST全体にRange(位置情報)あり | — |
| 言語表現力 | ADT, 再帰, HOF, I/O, パターンマッチ, 多相 | Syntax.hs, Prelude.mlg |
| バックエンド | インタプリタ(Eval.hs) + 参照用Scheme出力(Scheme.hs) | Join.hs, Eval.hs |

## 推奨実装順序

```
(1) Lispインタプリタ ──→ (2) ネイティブコンパイラ
                              ↑
(3) パッケージマネージャ ──→ (4) LSP
```

### Phase 1: Malgo製Lispインタプリタ

**理由**: 既存の言語機能で実装可能。言語の実用性を証明し、不足機能を洗い出すドッグフーディング。

**必要な言語機能の確認**:
- [x] 再帰的データ型 (S式表現)
- [x] パターンマッチ (eval/apply)
- [x] 文字列操作 (パーサ)
- [x] I/O (REPL)
- [ ] 可変参照 or 環境のMap (環境の実装に必要 — 現状は連想リストで代替可能)

**サブタスク**:
1. S式パーサ (文字列→AST)
2. 環境と評価器 (eval/apply)
3. 基本組み込み関数 (car, cdr, cons, +, -, *, eq?, etc.)
4. lambda, define, if, cond, quote
5. REPL ループ
6. テストスイート

**成果物**: `examples/malgo/Lisp.mlg` (単一ファイルで完結)

---

### Phase 2: ネイティブコンパイラ

**理由**: Join IRは逐次計算系(sequent calculus)ベースで、CPS変換済みに近い構造。ネイティブコード生成への変換が比較的素直。

**アプローチ選択肢**:

| 方式 | 利点 | 欠点 |
|------|------|------|
| **A. LLVM IR出力** | 最適化パスが豊富、クロスプラットフォーム | llvm-hs依存、ビルド重い |
| **B. C出力** | ポータブル、デバッグしやすい | GC実装が必要、最適化がCコンパイラ依存 |
| **C. アセンブリ直接出力** | 完全制御 | 工数大、プラットフォーム依存 |

**推奨**: **B. C出力** から始め、必要に応じてLLVMへ移行。

**サブタスク**:
1. ランタイムシステム設計 (メモリレイアウト、クロージャ表現、タグ付きユニオン)
2. GC方式選定 (Boehm GC or カスタムコピーGC)
3. Join IR → C変換 (`Backend/C.hs`)
4. Builtin関数のC実装 (`runtime/c/`)
5. ビルドシステム統合 (cabalからCコンパイラ呼び出し)
6. 最適化パス追加 (インライニング、デッドコード除去、定数畳み込み)
7. ベンチマーク (vs インタプリタ、vs Scheme出力)

**データ表現案** (Join IRとの対応):
```
Producer  →  tagged pointer (ヒープ割り当て or スタック)
Consumer  →  continuation closure
Statement →  C関数 (tail call → goto/trampoline)
Join点    →  ラベル + goto
```

---

### Phase 3: パッケージマネージャ (MVS + マークル木)

**理由**: ネイティブコンパイラがあると配布可能なバイナリが作れる。パッケージマネージャの実用性が増す。

**設計方針**:
- **Minimum Version Selection** (Go modules方式): 依存解決が決定的、lock file不要
- **マークル木**: パッケージの整合性検証、再現可能ビルドの保証

**既存インフラの拡張点**:
- `Module.hs`: ModuleNameにバージョン情報を追加
- `Interface.hs`: インターフェースファイルにハッシュを含める
- `.malgo-work/`: パッケージキャッシュとして拡張

**サブタスク**:
1. パッケージマニフェスト形式の設計 (`malgo.toml` or `malgo.json`)
2. バージョン表現とMVSアルゴリズム実装
3. マークル木によるパッケージハッシュ計算
4. レジストリプロトコル設計 (git-based? HTTP API?)
5. `malgo.sum` (ハッシュサムファイル) の生成と検証
6. CLI統合 (`malgo get`, `malgo build`, `malgo publish`)
7. Module.hsの拡張 (バージョン付き依存解決)

**マニフェスト例**:
```toml
[package]
name = "my-app"
version = "1.0.0"

[dependencies]
"github.com/takoeight0821/malgo-stdlib" = "0.1.0"
```

---

### Phase 4: LSP

**理由**: 他の3機能が揃った段階で、開発体験の向上として最も効果が高い。パッケージマネージャがあるとワークスペース管理もLSPに統合しやすい。

**既存の利点**:
- AST全ノードにRange情報あり → Position変換は機械的
- パス構造が明確 → 各パスのエラーをDiagnosticに変換可能

**サブタスク**:
1. `lsp` パッケージ依存追加 (Haskellの `lsp` ライブラリ)
2. Diagnostics (Parse/Rename/Typecheck エラー → LSP Diagnostic)
3. Go to Definition (Rename passの名前解決情報を利用)
4. Hover (型情報表示、Infer passの結果を利用)
5. Completion (スコープ内の名前候補)
6. Document Symbols (トップレベル定義の一覧)
7. パッケージマネージャとの統合 (依存パッケージのシンボル解決)

---

## 依存関係と優先度

```
Phase 1 (Lisp) → 独立。すぐ着手可能。言語の限界を知るために最初にやる。
Phase 2 (コンパイラ) → Phase 1の知見を反映。最も工数が大きい。
Phase 3 (パッケージ) → Phase 2があると配布が現実的に。Phase 1なしでも着手可。
Phase 4 (LSP) → 全フェーズの恩恵を受ける。最後が効率的。
```

Phase 1と3は独立しているため並行作業も可能。

## 未決定事項

- [ ] ネイティブコンパイラのターゲット (C出力 vs LLVM vs その他)
- [ ] GC方式 (Boehm GC vs カスタム)
- [ ] パッケージレジストリの形態 (git-based vs 中央レジストリ)
- [ ] LSPのHaskellライブラリ選定 (`lsp` vs `lsp-types` + 自前実装)
- [ ] Lispインタプリタの方言 (Scheme R7RS subset? Common Lisp subset? 独自?)
