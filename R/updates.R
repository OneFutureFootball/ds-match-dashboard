temp <- fromJSON('input/match_output.json')
this_period <- 2
this_time <- c(80,0); this_time <- 60*this_time[1] + this_time[2] - (45*60*(this_period-1))

temp %>% subset(time>=this_time) %>% 
    subset(period==this_period) %>% 
    select(period,time,match_time,state,full_name,action,outcome,x,y,X,Y,xg) %>% 
    head(10) %>% 
    print()

temp %>% 
    group_by(period,match_time) %>% 
    mutate(N = n()) %>% 
    ungroup() %>% 
    mutate(
        time = case_when(
            match_time=='80:07'& state=='Goal'~2108,
            TRUE ~time
        ),
        Y = case_when(
            match_time=='06:24' ~ 25,
            match_time=='35:01' ~ 57,
            match_time=='43:44' ~ 55,
            period==2 & match_time=='47:46' ~ 60,
            period==2 & match_time=='55:16' ~ 29,
            period==2 & match_time=='66:07' ~ 55,
            period==2 & match_time=='80:07'& action=='shoot'~26,
            TRUE ~ Y
        ),
        X = case_when(
            match_time=='35:01' ~ 108,
            TRUE ~ X
        )
    ) %>% toJSON(pretty=TRUE) %>% 
    write(file='input/match_output.json')
source('R/support/data_prep.R')

for(i in list.files('output/layers/04',full.names=TRUE)) file.remove(i)
for(i in list.files('output/layers/99',full.names=TRUE)) file.remove(i)
for(i in list.files('output/frames',full.names=TRUE)) file.remove(i)
