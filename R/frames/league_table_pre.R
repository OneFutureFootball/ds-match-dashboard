league_table_pre <- function(){
    
    LT <- data.frame(img = list.files('graphics',pattern='table_pre.png',recursive=TRUE,full.names=TRUE)) %>% 
        mutate(X = 960,
               Y = 580)
    ggplot() +
        background_image(readPNG('output/layers/title_page.png')) +
        geom_rect(mapping = aes(xmin=-1000,xmax=3000,
                                ymin=-1000,ymax=2000),
                  fill='black',alpha=0.8) +
        coord_cartesian(xlim=c(0,1920),ylim=c(0,1080)) +
        theme_void() +
        geom_image(LT,mapping = aes(x=X,y=Y,image=img),
                   size=0.7)
    ggsave('output/layers/league_table_pre.png',
           height=1080,
           width=1920,
           units='px',dpi=300)
    
}
