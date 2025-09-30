FROM haskell:9.12.2-bookworm

# Avoid prompts during installation
ENV DEBIAN_FRONTEND=noninteractive

# Install all necessary packages in a single RUN to reduce layers
RUN apt-get update && apt-get -y install \
    git-all \
    sudo \
    tzdata \
    vim \
    wget \
    curl \
    build-essential \
    libgmp-dev \
    zlib1g-dev \
    libtinfo-dev \
    libnuma-dev \
    libncurses-dev \
    pkg-config \
    python3 \
    python3-pip \
    && echo "${TZ}" > /etc/timezone \
    && dpkg-reconfigure -f noninteractive tzdata \
    && rm -rf /var/lib/apt/lists/*

# Install GHCup
RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | \
    BOOTSTRAP_HASKELL_NONINTERACTIVE=1 \
    BOOTSTRAP_HASKELL_MINIMAL=1 \
    sh

# Set PATH for ghcup
ENV PATH="/root/.ghcup/bin:${PATH}"

# Install and set recommended versions
RUN ghcup install ghc 9.6.7 --set && \
    ghcup install cabal 3.12.1.0 --set && \
    ghcup install hls 2.10.0.0 --set && \
    ghcup install stack 3.3.1 --set

RUN cabal update \
    && cabal install --global \
        hlint \
        hoogle \
        ormolu \
        fourmolu \
        ghcid \
        hasktags \
        implicit-hie \
        hpack \
        --overwrite-policy=always \
    && cabal install --lib \
        array \
        transformers \
        random-shuffle \
        mtl-2.3.1 \
        lifted-base \
        containers \
        bytestring \
        text \
        vector \
        split \
        attoparsec \
        aeson \
        time \
        operational \
        pipes

# Generate hoogle database
RUN hoogle generate

# Set timezone
ENV TZ=Asia/Tokyo

# Set up locale
ENV LANG=C.UTF-8 \
    LC_ALL=C.UTF-8

# Update PATH to include all tool locations
ENV PATH="/root/.cabal/bin:/root/.local/bin:/root/.ghcup/bin:${PATH}"

# Git and terminal configuration in a single layer
RUN git config --global credential.helper 'cache --timeout 36000' \
    && git config --global --add safe.directory /work \
    && echo 'PS1="\[\033[01;36m\]\u@\h\[\033[00m\]:\[\033[01;32m\]\w\[\033[35m\] [\D{%T}]\[\033[00m\]\$ "' > /root/.colors \
    && echo 'alias ls="ls --color=auto"' >> /root/.colors \
    && cat /root/.colors >> /root/.bashrc \
    && echo ':set prompt "\ESC[33m\STXλ> \ESC[m\STX"' > /root/.ghci \
    && echo ':set prompt-cont "\ESC[33m\STX | \ESC[m\STX"' >> /root/.ghci \
    && chmod 644 /root/.ghci

# Cache common Stack resolver (GHC 9.6.7)
RUN stack update && \
    echo "resolver: lts-21.25" > /tmp/stack.yaml && \
    cd /tmp && \
    stack setup && \
    rm -rf /tmp/stack.yaml

RUN rm -rf /root/.cabal 2>/dev/null || true