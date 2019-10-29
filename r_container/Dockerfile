FROM rocker/r-base

############
## Install linux dependencies
###########

RUN apt-get update -qq && apt-get install -y \
  git-core \
  libssl-dev \
  libcurl4-gnutls-dev \
  libpq-dev

#########
## Install required R packages
#######

RUN install2.r plumber dplyr tidyr sqldf RPostgreSQL

##########
## Copy plumber API file to container
###########

COPY plumber.R .

###########
## Expose port 8000 on the container 
###########

EXPOSE 8000

############
## Execute on container startup
## commandArgs[4] points to the fourth parameter sent to ENTRYPOINT, which
## in this case is the filename of the script that contains plumber API syntax in the CMD call
#############

ENTRYPOINT ["R", "-e", "pr <- plumber::plumb(commandArgs()[4]); pr$run(host='0.0.0.0', port=8000)"]
CMD ["plumber.R"]
