FROM nimlang/nim:1.6.8 AS build
RUN apt-get update && apt-get install -y libopenblas-dev
COPY . /app
WORKDIR /app
RUN nimble build -y