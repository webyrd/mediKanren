FROM racket/racket:8.0
RUN mkdir -p /local
COPY target/medikanren2 /local/medikanren2
RUN mkdir -p /local/ci_artifacts/status/pass
RUN mkdir -p /local/ci_artifacts/status/fail
#weird error: /usr/bin/raco: Unrecognized command: make
#  RUN bash /local/medikanren2/.compile.sh
RUN bash /local/medikanren2/.install_pkgs.sh
CMD [ "/usr/bin/racket", "/local/medikanren2/server.rkt" ]
