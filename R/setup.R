suppressPackageStartupMessages(library(av))
suppressPackageStartupMessages(library(png))
suppressPackageStartupMessages(library(grid))
suppressPackageStartupMessages(library(ggpubr))
suppressPackageStartupMessages(library(magick))
suppressPackageStartupMessages(library(ggforce))
suppressPackageStartupMessages(library(foreach))
suppressPackageStartupMessages(library(ggimage))
suppressPackageStartupMessages(library(jsonlite))
suppressPackageStartupMessages(library(showtext))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(gganimate))
suppressPackageStartupMessages(library(doParallel))
suppressPackageStartupMessages(library(toOrdinal))
suppressPackageStartupMessages(library(cropcircles))
suppressPackageStartupMessages(library(lubridate))

for(i in list.files('fonts',full.names=TRUE)) font_add(str_split(str_replace(i,'.ttf',''),'/')[[1]][2],i)
showtext_auto()

fixture <- fromJSON('data/fixture_S03.json')
active_cores <- 8

this_match <- fromJSON('input/match.json')

for(x in list.files('R/support',recursive=TRUE,full.names=TRUE)) source(x)
for(x in list.files('R/frames',recursive=TRUE,full.names=TRUE)) source(x)