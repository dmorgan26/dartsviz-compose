FROM rocker/verse:3.6.1

###########
# Add shiny server
############

RUN export ADD=shiny && bash /etc/cont-init.d/add

################
# Install linux dependencies
################

RUN apt-get update -y && \
	apt-get install -y \ 
		curl

############
# Install remotes package
###########

RUN R -e "install.packages('remotes')"
RUN R -e "remotes::install_github('dmorgan26/dartsviz/plot-app', build_vignettes = TRUE)"

################
# Configure shiny server
################

COPY ./shiny-server.conf /etc/shiny-server/shiny-server.conf
