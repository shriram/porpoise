# This is a Dockerfile for a containerized Porpoise.
#
# To build the image, execute the following command:
# > `docker build --target server-with-logins --tag porpoise .`
# To build the image without `logins.sexp`, use the `server` target instead.
#
# To start a container, execute the following command:
# > `docker run --env OPENAI_API_KEY --publish 8000:8000 porpoise`
# Be sure to set the OPENAI_API_KEY environment variable so that it can be forwarded to the
# container. The web interface should be reachable at http://localhost:8000/servlets/standalone.rkt.
# When using the image without `logins.sexp`, make sure to provide the necessary files in
# `/porpoise/` by some other means (e.g., by bind mounting).

FROM debian:stable-slim as builder

# install prerequisites
RUN apt-get update && \
  apt-get install -y git && \
  rm -rf /var/lib/apt/lists/*

# prepare program
WORKDIR /porpoise-build
COPY . .
RUN git apply docker.patch && \
  rm docker.patch

# remove logins.sexp; it is re-added in the target server-with-logins
RUN rm logins.sexp


FROM racket/racket:8.10-full as server

# install porpoise
WORKDIR /porpoise
COPY --from=builder /porpoise-build .
RUN mkdir LOGS

# restrict permissions
RUN adduser --disabled-password --gecos "" --no-create-home porpoise
RUN chmod --recursive u=rwX,g=rX,o= . && \
  chown --recursive root:porpoise . && \
  chown porpoise LOGS
USER porpoise

CMD ["/usr/bin/racket", "serve.rkt"]


FROM server as server-with-logins

# add logins.sexp
COPY --chmod=640 --chown=root:porpoise logins.sexp .
