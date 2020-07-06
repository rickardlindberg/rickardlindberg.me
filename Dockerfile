FROM fedora
WORKDIR /rickardlindberg.me
RUN dnf install -y ghc
RUN dnf install -y ghc-hakyll
RUN dnf install -y ghc-hakyll-devel
RUN dnf install -y glibc-locale-source glibc-langpack-en
RUN localedef -v -c -i en_US -f UTF-8 en_US.UTF-8 || true
ENV LANG en_US.UTF-8
ENV LANGUAGE en_US:en
ENV LC_ALL en_US.UTF-8
COPY . .
CMD ["bash", "-c", "./bin/build"]
