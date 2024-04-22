build_frame <- function(idx, force=FALSE){

    
    frame <- frame_index %>% subset(IDX==idx)
    if(!force & file.exists(paste0('output/layers/99/Frame_',str_pad(frame$IDX,5,pad='0'),'.png'))) return(NULL)
    
    FRAME <- image_read(frame$base)
    
    if(!is.na(frame$filename)) FRAME <- FRAME %>% image_composite(image_read(paste0('output/layers/06/Clock',frame$filename)))
    if(!is.na(frame$minute)) FRAME <- FRAME %>% image_composite(image_read(paste0('output/layers/02/',frame$minute)))
    if(!is.na(frame$key)) FRAME <- FRAME %>% image_composite(image_read(paste0('output/layers/03/',frame$key)))
    if(!is.na(frame$trx)) FRAME <- FRAME %>% image_composite(image_read(paste0('output/layers/04/',frame$trx)))
    if(!is.na(frame$lineup)) FRAME <- FRAME %>% image_composite(image_read(paste0('output/layers/05/',frame$lineup)))
    if(!is.na(frame$card)) FRAME <- FRAME %>% image_composite(image_read(paste0('output/layers/08/',frame$card)))
    if(!is.na(frame$crest)) FRAME <- FRAME %>% image_composite(image_read(paste0('output/layers/',frame$crest)))
    if(!is.na(frame$overlay)){
        FRAME <- FRAME %>% image_composite(image_read('images/black_overlay.png'))
        FRAME <- FRAME %>% image_composite(image_read(frame$overlay))
    }
    image_write(FRAME,paste0('output/layers/99/Frame_',str_pad(frame$IDX,5,pad='0'),'.png'))
    
}