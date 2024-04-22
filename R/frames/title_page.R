title_page <- function(){
    match_teams <- match_details %>% select(home_id,away_id) %>% gather('travel','team_id') %>% 
        left_join(teams,by='team_id') %>% 
        mutate(X = 620 + 200*ifelse(travel=='home_id',-1,1),
               Y = 320,
               crest = str_replace(crest,'-256',''))
    
    UT <- ymd_hms(match_details$utc,tz='UTC')
    HT <- with_tz(UT,c('Asia/Jakarta'))
    AT <- with_tz(UT,c('America/New_York'))
    match_times <- data.frame(tz = c('utc','home','away'),
                              date = c(format(UT,'%B %d'),
                                       format(HT,'%B %d'),
                                       format(AT,'%B %d')),
                              hour = c(format(UT,'%H:'),
                                       format(HT,'%H:'),
                                       format(AT,'%H:')),
                              mins = c(format(UT,'%M %Z'),
                                       format(HT,'%M %Z'),
                                       format(AT,'%M %Z')),
                              y = c(160,120,80))
    
    img_dir <- '~/Google Drive/Shared drives/1FF/1FF Creative/Social Media/1FF Socials/Content - Season 3/S3 Automated Image Library/Scoreline Background Images/Club Stadiums'
    img_option <- list.files(img_dir,pattern=match_details$home_short_name,full.names=TRUE,recursive=TRUE)

    plot_output <- ggplot() +
        theme_void() +
        coord_cartesian(xlim=c(0,1920),ylim=c(0,1080)) +
        background_image(readPNG(img_option)) +
        background_image(readPNG('images/title_page.png')) +
        geom_image(match_teams,mapping = aes(x=X,y=Y,image=crest),size=0.28) +
        annotate('text',x=620,y=10,hjust=0.5,vjust=0.5,label=match_details$stadium_location, 
                 colour='#FFFFFF', family='Montserrat-Medium',size=10) +
        annotate('text',x=620,y=60,hjust=0.5,vjust=0.5,label=match_details$stadium_name, 
                 colour='#FFFFFF', family='Montserrat-ExtraBold',size=10) +
        annotate('text',x=620,y=110,hjust=0.5,vjust=0.5,
                 label=paste0(wday(UT,label=TRUE,abbr=FALSE),', ',
                              match_times %>% subset(tz=='utc') %>% pull(date)),
                 colour='#FFFFFF', family='Montserrat-Black',size=10) +
        geom_text(match_details,mapping=aes(x=1450,y=1030,hjust=1,vjust=0.5,label=paste0('Season ',season_no)),
                  colour='#FFFFFF', family='Montserrat-ExtraBold',size=15) +
        geom_text(match_details %>% 
                      mutate(LAB = case_when(
                          is.na(final_type) ~ paste0('Round ',round_no),
                          final_type=='final' ~ 'Final',
                          final_type=='semi_final' ~ 'Semi Final',
                          final_type=='qualifying_final' ~ 'Qualifying Final',
                          final_type=='elimination_final' ~ 'Elimination Final'
                      )),
                  mapping = aes(x=1470,y=1030,label=LAB),
                  hjust=0,vjust=0.5, colour='#FFFFFF', family='Montserrat-Medium',size=15)
    ggsave('output/layers/title_page.png',
           plot_output,
           height=1080, width=1920, units='px', dpi=300)
    
    image_write(image_convert(image_read('output/layers/title_page.png'),'jpeg'),'output/title_page.jpg', quality=95)
}