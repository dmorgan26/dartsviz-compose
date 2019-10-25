# dartsviz-compose
Shiny app, postgres database and plumber API deployed as services using docker-compose

docker-compose file launches 3 services:

shiny
plumber_api
dartsviz_db

Open terminal in cloned repo and run ``docker-compose up`` to launch services

When plumber_api returns 'Starting to listen on port 8000' setup is complete

Visit http://localhost:8787 to launch and use 'dartsviz' package in RStudio session
Visit http://localhost:8000/player_stats?<playername> to call plumber API and return player stats

API Example:

http://localhost:8000/player_stats?player_name=garyanderson


