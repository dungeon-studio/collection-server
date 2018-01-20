FROM alpine:3.7 as builder
MAINTAINER Alex Brandt <alunduil@alunduil.com>

RUN apk add --no-cache musl-dev zlib-dev
RUN apk add --no-cache cabal ghc

WORKDIR /usr/local/src/collection-server

RUN cabal update

COPY ./*.cabal ./
RUN cabal install -j --only-dependencies

COPY . ./
RUN cabal build -j --ghc-options="-static -optc-static -optl-static -optl-pthread"

FROM scratch
MAINTAINER Alex Brandt <alunduil@alunduil.com>

COPY --from=builder /usr/local/src/collection-server/dist/build/collection-server/collection-server /

ENTRYPOINT [ "/collection-server" ]
