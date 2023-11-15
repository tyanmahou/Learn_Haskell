# Dockerfile
FROM haskell:latest

# Install GHCup
RUN curl https://get-ghcup.haskell.org -sSf | sh
ENV PATH="/root/.ghcup/bin:${PATH}"

RUN ghcup install hls

# Set working directory
WORKDIR /workspace

# Copy project files to the container
COPY . /workspace