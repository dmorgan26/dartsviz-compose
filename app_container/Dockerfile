FROM rocker/verse:3.6.1

# add shiny server
RUN export ADD=shiny && bash /etc/cont-init.d/add

################
#install linux deps
################

RUN apt-get update -y && \
	apt-get install -y \ 
		curl

############
#install devtools
###########

RUN R -e "install.packages('remotes')"
RUN R -e "remotes::install_github('dmorgan26/dartsviz/plot-app', build_vignettes = TRUE)"
RUN R -e "library(dartsviz)"

################
# configure shiny server
################


COPY ./shiny-server.conf /etc/shiny-server/shiny-server.conf
COPY ./plot-app /home/daniel/Documents/dartsviz

EXPOSE 8787
