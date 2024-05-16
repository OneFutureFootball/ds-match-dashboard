clock_overlay <- function(clock_time,force=FALSE){
    
    if(file.exists(paste0('output/layers/06/Clock_',
                          clock_time$period,'_',
                          str_pad(clock_time$secs,4,pad='0'),'.png')) & !force) return(NULL)
    
    clock_output <- ggplot() + 
        coord_cartesian(xlim=c(0,1920),ylim=c(0,1080)) +
        theme_void() +
        geom_text(clock_time,
                  mapping = aes(x=960, y=1096, label=TIME),
                  hjust = 0.5, vjust=0.5, family='Montserrat-Medium', size=8, colour='white', lineheight=0.6)
    
    ggsave(paste0('output/layers/06/Clock_',clock_time$period,'_',str_pad(clock_time$secs,4,pad='0'),'.png'),
           clock_output,
           height=1080,
           width=1920,
           units='px',
           dpi=300)
}
