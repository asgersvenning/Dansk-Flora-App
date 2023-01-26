ARG test=FALSE
FROM learndfv_base
COPY renv.lock.prod renv.lock
RUN R -e 'renv::restore()'
COPY learndfv_*.tar.gz /app.tar.gz
RUN R -e 'remotes::install_local("/app.tar.gz",upgrade="never")'
RUN rm /app.tar.gz
EXPOSE 80
RUN if [[ -z "$test"]] ; then CMD R -e "options('shiny.port'=80,shiny.host='0.0.0.0');library(learndfv);learndfv::run_app(port = 80)" ; else CMD R tests/testthat/test-app.R ; fi   
