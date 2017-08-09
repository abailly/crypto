FROM haskell:latest

RUN mkdir /app
WORKDIR /app

COPY . /
RUN stack install
