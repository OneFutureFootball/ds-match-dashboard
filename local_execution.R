rm(list=ls())
tic <- Sys.time()
library(tidyverse)
library(jsonlite)

library(tidyverse)
library(jsonlite)
library(here)

season_no <- 3

sapply(list.files('input',pattern='\\.',full.names=TRUE, recursive=TRUE),function(x) file.remove(x))
sapply(grep(list.files('output',pattern='\\.',full.names=TRUE, recursive=TRUE),pattern='Clock',invert=TRUE,value=TRUE),
       function(x) file.remove(x))
sapply(list.files('graphics',pattern='\\.',full.names=TRUE, recursive=TRUE),function(x) file.remove(x))
sapply(list.files('input/matches',full.names=TRUE),function(x) file.remove(x))

aws_account <- 'prod'
match_bucket <- paste0('s3://oneff-engine-data/season_',season_no,'/matches')
stats_bucket <- paste0('s3://oneff-engine-data/season_',season_no,'/stats')
graphics_bucket <- paste0('s3://oneff-engine-data/season_',season_no,'/graphics')
player_bucket <- paste0('s3://oneff-engine-data/season_',season_no,'/players')
data_bucket <- paste0('s3://oneff-engine-data/season_',season_no,'/data')
dump_folder <- path.expand(paste0(here(),'/input'))
graphics_folder <- path.expand(paste0(here(),'/graphics'))

system(paste0('aws s3 sync "',player_bucket,'" "',dump_folder,'" --profile ',aws_account))
system(paste0('aws s3 sync "',data_bucket,'" "',dump_folder,'" --profile ',aws_account))
sapply(list.files('input',pattern='fixture|roles|identity|map|list',full.names=TRUE),function(x) file.rename(x,str_replace(x,'input','data')))


full_fixture <- fromJSON(list.files('data',pattern='fixture',full.names=TRUE)) %>% 
    select(season_no,round_no,match,match_id,
           home=home_id,home_name=home_short_name,
           away=away_id,away_name=away_short_name,
           utc, final_type)

completed_matches <- data.frame(code=system(paste0('aws s3 ls s3://oneff-engine-data/season_',season_no,'/graphics/ --profile prod'),intern=TRUE)) %>% 
    subset(str_detect(code,'/')) %>% 
    mutate(match_id = str_replace(str_replace(str_replace(code,' ',''),'PRE',''),'/',''),
           match_id = as.numeric(match_id)) %>% 
    pull(match_id)

this_match <- full_fixture %>% 
    subset(match_id%in%completed_matches) %>% 
    arrange(utc) %>% 
    tail(1)
this_match <- full_fixture %>% subset(match_id==3006)
system(paste0('aws s3 sync "',match_bucket,'/',this_match$match_id,'" "',dump_folder,'" --profile ',aws_account))
system(paste0('aws s3 sync "',stats_bucket,'/',this_match$match_id,'" "',dump_folder,'" --profile ',aws_account))
system(paste0('aws s3 sync "',graphics_bucket,'/',this_match$match_id,'" "',graphics_folder,'" --profile ',aws_account))

source('R/setup.R')
source('R/movie/build_movie.R')
source('R/movie/final_movie.R')

toc <- Sys.time()
print(toc - tic)