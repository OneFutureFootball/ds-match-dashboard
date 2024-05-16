penalty_overlays <- function(){
    penalties <- key_moments %>% 
        subset(state=='PENALTY') %>% 
        select(IDX,period,time)
    
    UT <- ymd_hms(match_details$utc,tz='UTC')
    local_timezone <- match_details %>% 
        select(home_id,away_id) %>% 
        gather('travel','team_id') %>% 
        mutate(timezone = case_when(
            team_id == 1 ~ 'Asia/Calcutta',
            team_id == 2 ~ 'Australia/Sydney',
            team_id == 3 ~ 'America/Buenos_Aires',
            team_id == 4 ~ 'America/Los_Angeles',
            team_id == 5 ~ 'Asia/Riyadh',
            team_id == 6 ~ 'America/New_York',
            team_id == 7 ~ 'Asia/Jakarta',
            team_id == 8 ~ 'Africa/Lagos',
            team_id == 9 ~ 'Europe/Paris',
            team_id == 10 ~ 'America/Manaus',
            team_id == 11 ~ 'Europe/London',
            team_id == 12 ~ 'Asia/Tokyo'
        )) %>%
        select(-team_id) %>% 
        spread(travel,timezone)
    HT <- with_tz(UT,local_timezone$home_id)
    match_times <- as.numeric(format(HT,'%H'))
    pen_image <- paste0('images/overlays/penalty-',ifelse(match_times>=18,'night','day'),'.png')
    
    for(i in penalties$IDX){
        this_penalty <- penalties %>% subset(IDX==i)
        plot_output <- ggplot() +
            coord_cartesian(xlim = c(0,1920),
                            ylim = c(0,1080)) +
            theme_void() + 
            background_image(readPNG(pen_image))
        ggsave(paste0('output/layers/07/Overlay_',this_penalty$period,'_',str_pad(this_penalty$time,4,pad='0'),'.png'),
               plot_output,
               height=1080, width=1920, dpi=300, units='px')
    }
}