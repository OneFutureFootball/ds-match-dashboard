temp <- fromJSON('input/match_output.json')
this_period <- 1
this_time <- c(0,0); this_time <- 60*this_time[1] + this_time[2] - (45*60*(this_period-1))

temp %>% subset(time>=this_time) %>% 
    subset(period==this_period) %>% 
    select(period,match_time,full_name,action,outcome,x,y,X,Y,xg) %>% 
    head(10) %>% 
    print()

temp %>% 
    group_by(period,match_time) %>% 
    mutate(N = n()) %>% 
    ungroup() %>% 
    mutate(
        Y = case_when(
            TRUE ~ Y
        ),
        X = case_when(
            TRUE ~ X
        )
    ) %>% toJSON(pretty=TRUE) %>% 
    write(file='input/match_output.json')
source('R/support/data_prep.R')

for(i in list.files('output/layers/04',full.names=TRUE)) file.remove(i)
for(i in list.files('output/layers/99',full.names=TRUE)) file.remove(i)
for(i in list.files('output/frames',full.names=TRUE)) file.remove(i)
