/home/vscode/.local/bin/mise trust
/home/vscode/.local/bin/mise install
# /home/vscode/.local/bin/mise run setup
GHCUP="/home/vscode/.local/share/mise/installs/ghcup/latest/ghcup"
"$GHCUP" install ghc 9.14.1
"$GHCUP" set ghc 9.14.1
"$GHCUP" install cabal 3.16.1.0
"$GHCUP" set cabal 3.16.1.0
cabal update
cabal install hpack --overwrite-policy=always
cabal install ormolu --overwrite-policy=always
# /home/vscode/.local/bin/mise run setup-hls
ghcup compile hls -g 2.13.0.0 --ghc 9.14.1 --cabal-update
