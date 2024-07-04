temp <- fromJSON('input/match_output.json')
this_period <- 2
this_time <- c(61,0); this_time <- 60*this_time[1] + this_time[2] - (45*60*(this_period-1))

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
        Y = case_when(
            period==2 & match_time=='58:33'& action=='shoot' ~ 22,
            period==2 & match_time=='61:04'& action=='shoot' ~ 22,
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
