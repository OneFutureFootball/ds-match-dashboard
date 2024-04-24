FROM base1ff

COPY data ./data
COPY fonts ./fonts
COPY images ./images
COPY sounds ./sounds
COPY R ./R
COPY ./local_execution.R ./local_execution.R

RUN mkdir -p /output
RUN mkdir -p /graphics
RUN mkdir -p /input

RUN apt-get update && apt-get install -y \
    python3 \
    python3-pip \
    && pip3 install awscli --upgrade --user
