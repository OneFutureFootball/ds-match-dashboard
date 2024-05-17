temp <- fromJSON('input/match_output.json')
this_period <- 2
this_time <- c(76,20); this_time <- 60*this_time[1] + this_time[2] - (45*60*(this_period-1))

temp %>% subset(time>=this_time) %>% 
    subset(period==this_period) %>% 
    subset(match_time!='02:00') %>% 
    select(period,match_time,full_name,action,outcome,x,y,X,Y,xg) %>% 
    head(10) %>% 
    print()

temp %>% 
    mutate(
        Y = case_when(
            period==2 & match_time=='76:22' ~ 58,
            TRUE ~ Y
        ),
        X = case_when(
            period==1 & match_time=='00:17' ~ 81,
            period==1 & match_time=='11:06' ~ 76,
            period==2 & match_time=='52:04' ~ 103,
            TRUE ~ X
        )
    ) %>% toJSON(pretty=TRUE) %>% 
    write(file='input/match_output.json')
source('R/support/data_prep.R')

for(i in list.files('output/layers/04',full.names=TRUE)) file.remove(i)
for(i in list.files('output/layers/99',full.names=TRUE)) file.remove(i)
