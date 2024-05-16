league_table_post <- function(){
    
    LT <- data.frame(img = list.files('graphics',pattern='table_post.png',recursive=TRUE,full.names=TRUE)) %>% 
        mutate(X = 960,
               Y = 540)
    ggplot() +
        coord_cartesian(xlim=c(0,1920),ylim=c(0,1080)) +
        theme_void() +
        background_image(readPNG('images/overlays/black_overlay.png')) +
        geom_image(LT,mapping = aes(x=X,y=Y,image=img),
                   size=0.85)
    ggsave('output/layers/league_table_post.png',
           height=1080,
           width=1920,
           units='px',dpi=300)
    
}
