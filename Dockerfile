FROM fpco/stack-build:lts-8.19

COPY . /app/
WORKDIR /app
RUN stack install --install-ghc && stack clean
ENTRYPOINT ["/root/.local/bin/traceinstances"]

