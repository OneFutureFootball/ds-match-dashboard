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
        coord_cartesian(xlim = c(0,1920),
                        ylim = c(0,1080)) +
        theme_void() + 
        background_image(readPNG('images/overlays/added_time.png')) +
        geom_text(mapping = aes(x=920,y=625, label=minutes),
                  family='LED',size=90,colour='#FF4242',hjust=0.5,vjust=0.5,lineheight=0.7, angle=1.7)
    ggsave(paste0('output/layers/07/Overlay_',half,'_2700.png'),
           plot_output,
           height=1080, width=1920,
           units='px', dpi=300)
}