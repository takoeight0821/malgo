FROM mcr.microsoft.com/devcontainers/base:debian

RUN apt-get update && apt-get install -y build-essential curl libffi-dev libffi8ubuntu1 libncurses-dev libncurses6 libtinfo6 libgc-dev lsb-release wget gnupg pkg-config ripgrep retry && rm -rf /var/lib/apt/lists/*

RUN rm -rf /var/lib/apt/lists/*

USER vscode

# install haskell

# ENV BOOTSTRAP_HASKELL_NONINTERACTIVE=1
#
# ENV BOOTSTRAP_HASKELL_GHC_VERSION=9.12.2
# ENV BOOTSTRAP_HASKELL_CABAL_VERSION=3.12.1.0
# # Disable HLS installation
# # ENV BOOTSTRAP_HASKELL_INSTALL_HLS=0
# ENV BOOTSTRAP_HASKELL_HLS_VERSION=2.11.0.0
#
# RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh

# install mise
RUN curl https://mise.run | sh

RUN echo 'eval "$(/home/vscode/.local/bin/mise activate bash)"' >> /home/vscode/.bashrc