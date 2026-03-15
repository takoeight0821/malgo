# ビルド・テスト ベースラインプロファイル

- 計測日: 2026-03-14 〜 2026-03-15
- GHC: 9.12.2
- ビルドプロファイル: `-O0` (noopt)
- OS: macOS (Darwin 25.3.0, aarch64)

## ベースライン (semaphore: true → false 変更後)

hyperfine による計測。`semaphore: true` は hyperfine (stdin /dev/null) と非互換のため `false` に変更。

| シナリオ | 平均 | σ | Min | Max | Runs |
|---|---|---|---|---|---|
| クリーンビルド | 32.3s | 0.6s | 31.7s | 33.2s | 5 |
| no-op | 195ms | 3ms | 191ms | 198ms | 10 |
| leaf (Scheme 変更) | 8.1s | 0.5s | 7.8s | 9.5s | 10 |
| hub (Prelude 変更) | 7.8s | 1.4s | 7.3s | 11.8s | 10 |

## shared: False 適用後

`cabal.project` に `shared: False` を追加し、dynamic object (.dyn_o) のビルドを無効化。

| シナリオ | 平均 | σ | Min | Max | Runs | 改善 |
|---|---|---|---|---|---|---|
| クリーンビルド | 26.8s | 0.2s | 26.5s | 27.0s | 5 | **-17%** |
| no-op | 195ms | 2ms | 191ms | 197ms | 10 | 変化なし |
| leaf (Scheme 変更) | 6.8s | 0.5s | 6.6s | 8.2s | 10 | **-16%** |
| hub (Prelude 変更) | 7.1s | 2.1s | 6.3s | 13.0s | 10 | **-9%** |

## cabal.project の変更点

1. `semaphore: false` — hyperfine 等のベンチツールとの互換性確保
2. `shared: False` (package malgo) — dyn_o ビルドを無効化し 17% 高速化

## モジュール別コンパイル時間 Top 20 (static, `-ddump-timings`)

時間はミリ秒。各モジュールの全パス（Parser, Renamer, Desugar, Simplifier, CodeGen 等）の合計。

| # | Module | Time (ms) |
|---|--------|-----------|
| 1 | Malgo.Syntax | 1221.5 |
| 2 | Malgo.Prelude | 1074.9 |
| 3 | Malgo.Parser.CStyle | 984.9 |
| 4 | Malgo.Rename.Pass | 938.3 |
| 5 | Malgo.Sequent.Core.Full | 763.8 |
| 6 | Malgo.Elaborate | 665.6 |
| 7 | Malgo.Sequent.ToFun | 650.6 |
| 8 | Malgo.Infer | 647.3 |
| 9 | Malgo.Sequent.Eval | 544.1 |
| 10 | Malgo.Infer.Unify | 538.5 |
| 11 | Malgo.Sequent.Core.Flat | 516.4 |
| 12 | Malgo.Module | 514.1 |
| 13 | Malgo.Backend.Scheme | 443.6 |
| 14 | Malgo.Sequent.Core.Join | 342.3 |
| 15 | Malgo.Infer.Constraint | 333.5 |
| 16 | Malgo.Sequent.Fun | 313.8 |
| 17 | Malgo.Rename.RnEnv | 293.3 |
| 18 | Malgo.Sequent.ToCore | 256.3 |
| 19 | Malgo.Interface | 209.6 |
| 20 | Malgo.Syntax.Extension | 194.3 |

全 32 モジュール (static) 合計: **12,591ms**

## テスト実行時間

```
889 examples, 0 failures
Finished in 21.9 seconds
```

## 次フェーズへの示唆

1. **Malgo.Syntax (1.2s)** と **Malgo.Prelude (1.1s)** が突出 — 型族 (type family) やインスタンス解決が重い可能性
2. **Malgo.Parser.CStyle (1.0s)** — megaparsec のコンビネータ展開コスト
3. **Malgo.Rename.Pass (0.9s)** — 名前解決パスの複雑さ
4. テスト実行時間 (21.9s) はビルド時間と同程度 — テスト自体の最適化も効果的
5. Effectful Plugin の A/B テスト (Step 4) は dump-timings で plugin 寄与を確認してから判断
