sub_overlay <- function(idx){
    this_sub <- match_file %>% 
        subset(state=='Substitution') %>% 
        select(period,time) %>% 
        unique() %>% 
        slice(idx) %>% 
        left_join(match_file,by=c('period','time'))
    
    sub_on <- this_sub %>% 
        select(full_name,position) %>%
        mutate(LAB = paste0(full_name, ' (',position,')')) %>% 
        mutate(IDX = row_number(),
               X = 960 + 60,
               Y = 600 - 150*IDX)
    sub_off <- this_sub %>% 
        select(oth_full_name,oth_position) %>% 
        mutate(LAB = paste0(oth_full_name, ' (',oth_position,')')) %>% 
        mutate(IDX = row_number(),
               X = 960 - 60,
               Y = 600 - 150*IDX)
    
    plot_output <- ggplot() +
        coord_cartesian(xlim = c(0,1920),
                        ylim = c(0,1080)) +
        theme_void() +
        geom_rect(mapping = aes(xmin = 960 - 600, xmax = 960 + 600,
                                ymin = 100, ymax = 800),
                  fill='#150928') +
        geom_text(mapping = aes(x=960, y=700, label='SUBSTITUTION'),
                  family='Montserrat-ExtraBold',colour='#9776e3'
                  ,size=16, hjust=0.5,vjust=0.5) +
        geom_image(mapping = aes(x=600,y=700,image=teams$crest[teams$team_id%in%this_sub$team_id]),
                   size=0.08) +
        geom_text(sub_off,mapping = aes(x=X,y=Y,label=toupper(LAB)),
                  family='Montserrat-Bold',colour='white',size=8, hjust=1,vjust=0.5) +
        geom_text(sub_on,mapping = aes(x=X,y=Y,label=toupper(LAB)),
                  family='Montserrat-Bold',colour='white',size=8, hjust=0,vjust=0.5) +
        geom_text(sub_off,mapping = aes(x=900,y=550,label='OFF'),
                  family='Montserrat-ExtraBold',colour='white',size=10, hjust=1,vjust=0.5) +
        geom_text(sub_on,mapping = aes(x=1020,y=550,label='ON'),
                  family='Montserrat-ExtraBold',colour='white',size=10, hjust=0,vjust=0.5)
    
    ggsave(paste0('output/layers/07/Overlay_',unique(this_sub$period),'_',str_pad(round(unique(this_sub$time)),4,pad='0'),'.png'),
           plot_output,
           height=1080, width=1920, units='px', dpi=300)
    
}