# Running this recipe through docker build gets us an image in which
# we can convert odd->rnc.

# Sample invocation (from root directory):

# docker build -f .Dockerfile -t debian-for-odd-conversion ./

# You can then run things something like this:

# docker run --rm -w "/mnt/" -ti \
#         --mount type=bind,source="$(pwd)",destination=/mnt/ \
#         debian-for-odd-conversion \
#         bash tools/bin/sarit-preflight.el

FROM debian:stretch

RUN apt-get update && \
        apt-get --quiet --yes --no-install-recommends install \
        bash \
        make \
        git \
        emacs-nox \
        emacs24-common \
        jing \
        xmlstarlet \
        libxml2-utils \
        parallel \
        libxml2 \
        openjdk-8-jdk

CMD ["bash"]
