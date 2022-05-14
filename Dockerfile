FROM ubuntu:18.04

RUN apt-get update && apt-get install -y \
      wget \
      vim \
      git \
      curl \
      time \
      build-essential \
      sudo \
      && rm -rf /var/lib/apt/lists/*

RUN useradd -ms /bin/bash -u 1000 user
RUN usermod -aG sudo user
RUN echo 'user:typeisolation' | chpasswd

#Run Container as nonroot
USER user
WORKDIR /home/user
