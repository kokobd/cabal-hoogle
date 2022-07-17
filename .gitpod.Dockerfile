FROM gitpod/workspace-full

ADD ./.gitpod.cabal_config /home/gitpod/.cabal/config

RUN sudo chown -R gitpod: $HOME/.cabal && \
    sudo install-packages build-essential curl libffi-dev libffi7 libgmp-dev libgmp10 \
        libncurses-dev libncurses5 libtinfo5 && \
    curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | BOOTSTRAP_HASKELL_NONINTERACTIVE=1 BOOTSTRAP_HASKELL_MINIMAL=1 sh && \
    echo 'source $HOME/.ghcup/env' >> $HOME/.bashrc && \
    echo 'export PATH=$HOME/.cabal/bin:$HOME/.local/bin:$PATH' >> $HOME/.bashrc && \
    . /home/gitpod/.ghcup/env && \
    ghcup install ghc 9.2.2 --set && \
    ghcup install hls --set && \
    ghcup install cabal --set && \
    ghcup install stack --set && \
    cabal update && \
    cabal install implicit-hie hoogle && \
    pip install pre-commit && \
    npm install -g http-server
