manager_faceoff <- function(){
    
    plot_output <- ggplot() +
        background_image(readPNG('output/layers/title_page.png')) +
        geom_rect(mapping = aes(xmin=-1000,xmax=3000,
                                ymin=-1000,ymax=2000),
                  fill='black',alpha=0.8) +
        background_image(readPNG('images/backgrounds/managers.png')) +
        theme_void() +
        coord_cartesian(xlim=c(0,1920), ylim=c(0,1080)) +
        geom_image(mapping = aes(x=480, y=540, image=list.files('images/managers',pattern=this_match$home_name,full.names=TRUE)), size=0.48) +
        geom_image(mapping = aes(x=1440, y=540, image=list.files('images/managers',pattern=this_match$away_name,full.names=TRUE)), size=0.48)
    ggsave('output/layers/manager_faceoff.png',
           plot_output,
           height=1080,
           width=1920,
           dpi=300, units='px')
}