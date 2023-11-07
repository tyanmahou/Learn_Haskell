# Ubuntuベースのイメージを使用
FROM ubuntu:latest

# GHCUPをインストールするための依存関係をインストール
RUN apt-get update -y && \
    apt-get install -y \
        curl \
        libgmp-dev \
        zlib1g-dev \
        build-essential \
        libtinfo-dev && \
    apt-get clean

# GHCUPのインストール
RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh

# GHCUPのバイナリディレクトリをPATHに追加
ENV PATH="/root/.ghcup/bin:$PATH"

# GHCUPを使ってGHCをインストール
RUN ghcup install ghc  # インストールしたいバージョンを指定

# GHCのバージョンを選択
RUN ghcup set ghc  # 使用したいバージョンを指定

# コンテナ内で作業ディレクトリを設定
WORKDIR /app

# ホストマシンからコンテナ内にファイルをコピー
COPY . /app

# ターミナルセッションを開始
CMD ["bash"]
