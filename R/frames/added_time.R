added_time <- function(half){
    seconds <- match_file %>% 
        subset(period==half) %>% 
        summarise(MAX = max(time)) %>% 
        pull(MAX) - 45*60
    
    goals <- match_file %>% 
        subset(period==half & time>=45*60) %>% 
        subset(state=='Goal') %>% 
        nrow()
    
    subs <- match_file %>% 
        subset(period==half & time>=45*60) %>% 
        subset(state=='Substitution' & oth_role=='tactical') %>% 
        select(period,time) %>% 
        unique() %>% 
        nrow()
    
    cardinj <- match_file %>% 
        subset(period==half & time>=45*60) %>% 
        subset(state=='Substitution' & oth_role=='tactical') %>% 
        select(period,time) %>% 
        unique() %>% 
        nrow()
    
    minutes <- floor((seconds - goals*45 - subs*20 - cardinj*30)/60)
    
    plot_output <- ggplot() +
        #background_image(readPNG('output/layers/01/Match.png')) +
        geom_rect(mapping = aes(xmin=-100,xmax=2100,ymin=-100,ymax=1200),fill='black',alpha=0.8) +
        coord_cartesian(xlim = c(0,1920),
                        ylim = c(0,1080)) +
        theme_void() + 
        geom_image(mapping = aes(x=960, y=600, image='images/purple.png'),size=0.5) +
        geom_text(mapping = aes(x=960,y=600, label=paste0(minutes,"'",'\nADDED TIME')),
                  family='Montserrat-Bold',size=16,colour='white',hjust=0.5,vjust=0.5,lineheight=1.1)
    ggsave(paste0('output/layers/07/Overlay_',half,'_2700.png'),
           plot_output,
           height=1080, width=1920,
           units='px', dpi=300)
}