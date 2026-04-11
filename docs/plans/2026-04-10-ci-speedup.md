# CI 高速化: キャッシュ安定化 + GHC セットアップ高速化

Date: 2026-04-10

関連 issue: #285, #286

## Context

CI の GHC 9.12.4 ジョブは master でキャッシュミス時に約 12 分かかる。内訳:

| ステップ | 所要時間 | 割合 |
|---|---:|---:|
| GHC/cabal インストール (ghcup) | ~112s | 15% |
| 依存パッケージビルド (キャッシュミス時) | ~562s | 75% |
| Build (malgo 本体) | ~26s | 3% |
| Test | ~27s | 4% |

Build/Test は既に高速（合計 ~53s）。ボトルネックは GHC セットアップと依存ビルド。

## Track A: キャッシュ安定化

### 現状の問題

1. **キャッシュキーが `plan.json` ベース** — `cabal build all --dry-run` で生成される plan.json の hash をキーにしている。Hackage の新バージョン公開や `haskell-deps-update` ワークフロー（毎週月曜）で頻繁に無効化される
2. **`cabal.project.freeze` が未活用** — 317 行の freeze ファイルが既にあるが CI で使われていない
3. **`dist-newstyle` が未キャッシュ** — cabal store のみキャッシュ。malgo 本体のビルド結果は毎回捨てられる
4. **ファイルタイムスタンプ問題** — `actions/checkout` がタイムスタンプをリセットするため、キャッシュ復元後も GHC が不要な再ビルドを行う可能性がある

### 変更内容

#### A-1. cabal store と dist-newstyle を別キャッシュに分離

`cabal.project.freeze` 更新時に全依存パッケージをリビルドしない仕組みにする。

**cabal store キャッシュ**（依存パッケージの compiled objects）:
```yaml
- name: Cache cabal store
  uses: actions/cache@v4
  with:
    path: ${{ steps.setup.outputs.cabal-store }}
    key: ${{ runner.os }}-ghc-${{ matrix.ghc-version }}-store-${{ hashFiles('cabal.project.freeze') }}
    restore-keys: |
      ${{ runner.os }}-ghc-${{ matrix.ghc-version }}-store-
```

- `restore-keys` で部分ヒット: freeze 更新時も前回の store を復元し、**変更された依存のみ差分ビルド**
- cabal store はパッケージ単位で独立しているため、古いキャッシュの上に新パッケージを追加しても整合性が壊れない

**dist-newstyle キャッシュ**（malgo 本体のビルド結果）:
```yaml
- name: Cache dist-newstyle
  uses: actions/cache@v4
  with:
    path: dist-newstyle
    key: ${{ runner.os }}-ghc-${{ matrix.ghc-version }}-dist-${{ hashFiles('cabal.project.freeze') }}-${{ hashFiles('**/*.hs', '**/*.cabal', 'package.yaml') }}
    restore-keys: |
      ${{ runner.os }}-ghc-${{ matrix.ghc-version }}-dist-${{ hashFiles('cabal.project.freeze') }}-
```

- freeze + ソースファイル hash で完全一致キー
- freeze が同じでソースのみ変わった場合は `restore-keys` で部分ヒット → 変更ファイルのみ再コンパイル
- freeze が変わった場合は dist-newstyle を捨てる（依存のバージョンが変わると dist-newstyle の整合性が保証できないため）

**分離の理由**:
- cabal store は freeze 更新時も部分再利用が安全（パッケージ単位で独立）
- dist-newstyle は freeze 更新時に不整合が起きうるため、完全一致でのみ使用

#### A-2. ファイルタイムスタンプ復元

```yaml
- uses: actions/checkout@v4
  with:
    fetch-depth: 2

- name: Restore file timestamps
  run: |
    rev=HEAD
    for f in $(git ls-tree -r -t --full-name --name-only "$rev"); do
      touch -d "$(git log --pretty=format:%cI -1 "$rev" -- "$f")" "$f"
    done
```

`fetch-depth: 2` で直近のコミット履歴を取得し、各ファイルのタイムスタンプを最終コミット時刻に復元。これにより GHC が不要な再コンパイルを回避できる。

#### A-3. `cabal configure` で freeze ファイルを明示利用

`cabal.project.freeze` が存在する場合、cabal は自動的に読み込む。追加設定は不要だが、`cabal build all --dry-run` ステップを削除し、`cabal configure --enable-tests --disable-documentation` のみ残す。

### 対象ファイル

- `.github/workflows/build.yml`

---

## Track B: GHC セットアップ高速化

### 現状の問題

1. **`haskell/ghcup-setup` + 手動 `ghcup install`** — ghcup のセットアップ後に GHC と cabal を個別にインストール。約 2 分かかる
2. **`cabal update` が毎回実行** — Hackage インデックスのダウンロードに時間がかかる

### 変更内容

#### B-1. `haskell-actions/setup` に移行

```yaml
- uses: haskell-actions/setup@v2
  id: setup
  with:
    ghc-version: ${{ matrix.ghc-version }}
    cabal-version: latest
    cabal-update: true
```

利点:
- GitHub ランナーにプリインストール済みの GHC があればそれを使う（ダウンロード不要）
- プリインストールがない場合も ghcup fallback で同等
- `cabal-store` パスが outputs で提供される（現在と同じ）
- `cabal update` が統合済み（`cabal-update: true`）

これにより `Set up GHCup` + `Install GHC and cabal` の 2 ステップが 1 ステップに統合される。

#### B-2. GHC バージョン指定の柔軟化（オプション）

```yaml
ghc-version: '9.12'  # パッチバージョンを省略
```

パッチバージョンを省略すると、ランナーにプリインストール済みの 9.12.x を使える確率が上がる。ただし再現性とのトレードオフがあるため、`9.12.4` のまま維持が無難。

---

## 統合後の build.yml イメージ

```yaml
name: build
on:
  push:
    branches: [main, master]
  pull_request:
    branches: [main, master]

jobs:
  build:
    name: GHC ${{ matrix.ghc-version }}
    runs-on: ubuntu-latest
    strategy:
      fail-fast: false
      matrix:
        ghc-version: ['9.12.4']

    steps:
      - uses: actions/checkout@v4
        with:
          fetch-depth: 2

      - name: Restore file timestamps
        run: |
          rev=HEAD
          for f in $(git ls-tree -r -t --full-name --name-only "$rev"); do
            touch -d "$(git log --pretty=format:%cI -1 "$rev" -- "$f")" "$f"
          done

      - uses: haskell-actions/setup@v2
        id: setup
        with:
          ghc-version: ${{ matrix.ghc-version }}
          cabal-version: latest
          cabal-update: true

      - name: Configure the build
        run: cabal configure --enable-tests --disable-documentation

      - name: Cache cabal store
        uses: actions/cache@v4
        with:
          path: ${{ steps.setup.outputs.cabal-store }}
          key: ${{ runner.os }}-ghc-${{ matrix.ghc-version }}-store-${{ hashFiles('cabal.project.freeze') }}
          restore-keys: |
            ${{ runner.os }}-ghc-${{ matrix.ghc-version }}-store-

      - name: Cache dist-newstyle
        uses: actions/cache@v4
        with:
          path: dist-newstyle
          key: ${{ runner.os }}-ghc-${{ matrix.ghc-version }}-dist-${{ hashFiles('cabal.project.freeze') }}-${{ hashFiles('**/*.hs', '**/*.cabal', 'package.yaml') }}
          restore-keys: |
            ${{ runner.os }}-ghc-${{ matrix.ghc-version }}-dist-${{ hashFiles('cabal.project.freeze') }}-

      - name: Install dependencies
        run: cabal build all --only-dependencies

      - name: Build
        run: cabal build all

      - name: Run tests
        run: cabal test all --test-option=--jobs=4
```

## 期待される効果

| シナリオ | 現在 | 改善後（見込み） |
|---|---:|---:|
| 完全キャッシュヒット（ソース変更のみ） | ~3m46s | ~1-2m |
| freeze 更新（依存追加/更新） | ~12m26s (全リビルド) | ~3-5m (差分ビルド) |
| 完全キャッシュミス | ~12m26s | ~10m |
| キャッシュ無効化頻度 | 毎週（deps update） | freeze 更新時のみ |

主な改善:
- **完全キャッシュヒット時**: dist-newstyle キャッシュ + タイムスタンプ復元で malgo 本体の再ビルドも回避 → ~1-2 分
- **freeze 更新時**: cabal store の部分ヒットにより、変更された依存のみ差分ビルド → ~3-5 分（従来の ~12 分から大幅短縮）
- **キャッシュ無効化頻度**: plan.json → freeze ファイルベースで大幅に安定化
- **GHC セットアップ**: haskell-actions/setup でプリインストール活用時 ~30s 短縮

## リスク

| リスク | 対策 |
|---|---|
| `haskell-actions/setup` が GHC 9.12.4 をプリインストールしていない場合、ghcup fallback で現在と同等 | 9.12.4 は比較的新しいため fallback の可能性あり。速度は現状維持、統合によるステップ削減の利点は残る |
| `dist-newstyle` キャッシュが肥大化 | GitHub Actions のキャッシュ上限は 10GB。malgo の dist-newstyle は ~500MB 程度なので問題なし |
| ファイルタイムスタンプ復元で `fetch-depth: 2` が不十分 | 通常のコミットでは十分。squash merge 等で問題が出た場合は `fetch-depth: 0` に変更 |
| freeze ファイルを更新せずに依存を追加するとビルド失敗 | deps-update ワークフローで freeze も同時更新する運用が必要 |

## 検証

1. Track A + B を適用した build.yml で PR を作成
2. CI が PASS すること
3. 2回目以降の実行でキャッシュヒットし、ジョブ時間が短縮されること
4. `cabal.project.freeze` を変更しない限りキャッシュが維持されること
