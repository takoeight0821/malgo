export LANG=C.UTF-8
export LC_ALL=C.UTF-8
export LC_CTYPE=C.UTF-8

/home/vscode/.local/bin/mise trust
/home/vscode/.local/bin/mise install
/home/vscode/.local/bin/mise run setup
# HLS setup is skipped: HLS 2.13.0.0 does not yet support GHC 9.12.4.
# Run `mise run setup-hls` manually once HLS gains GHC 9.12.4 support.