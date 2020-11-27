FROM ubuntu:18.04 as app

RUN apt-get update && apt-get install -y curl

RUN mkdir -p /opt/app
WORKDIR /opt/app

COPY marvin-exe .

CMD ["/opt/app/marvin-exe"]


