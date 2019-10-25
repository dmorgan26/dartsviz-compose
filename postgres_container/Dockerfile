FROM postgres:11.1-alpine

COPY dumpfile.sql /docker-entrypoint-initdb.d/dumpfile.sql

EXPOSE 5432
ENV PGDATA=/data
