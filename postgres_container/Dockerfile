FROM postgres:11.1-alpine

##############
## Copy pg_dump SQL file containing instructions to build and populate postgres database
############

COPY dumpfile.sql /docker-entrypoint-initdb.d/dumpfile.sql

###########
## Allow containers to listen on port 5432 if instructed
##########

EXPOSE 5432
