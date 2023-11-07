# GHCの公式イメージをベースにする
FROM haskell:latest

# コンテナ内で作業ディレクトリを設定
WORKDIR /app

# ホストマシンからコンテナ内にファイルをコピー
COPY . /app

# パッケージの更新と基本的なビルドツールのインストール
RUN apt-get update -y && \
    apt-get install -y \
        curl \
        libgmp-dev \
        zlib1g-dev && \
    apt-get clean

# Haskellプロジェクトのビルドに必要なライブラリのインストール
RUN stack setup

# ターミナルセッションを開始
CMD ["bash"]
