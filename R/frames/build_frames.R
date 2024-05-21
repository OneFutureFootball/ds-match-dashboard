build_frames <- function(idx_range){
    
    all_frames <- frame_index %>% 
        select(IDX,base,filename,minute,key,trx,text,lineup,card,crest,overlay) %>% 
        gather('layer','FILE',-IDX) %>% 
        group_by(layer) %>% 
        mutate(LATEST = FILE) %>% 
        fill(LATEST,.direction='down') %>% 
        mutate(PREV = lag(FILE)) %>% 
        mutate(
            FILENAME = paste0(case_when(
                layer=='base' ~ '',
                layer=='filename' ~ 'output/layers/06/Clock',
                layer=='minute' ~ 'output/layers/02/',
                layer=='key' ~ 'output/layers/03/',
                layer=='trx' ~ 'output/layers/04/',
                layer=='text' ~ 'output/layers/09/',
                layer=='lineup' ~ 'output/layers/05/',
                layer=='card' ~ 'output/layers/08/',
                layer=='crest' ~ 'output/layers/',
                layer=='overlay' ~ ''
            ),FILE),
            LATEST = paste0(case_when(
                layer=='base' ~ '',
                layer=='filename' ~ 'output/layers/06/Clock',
                layer=='minute' ~ 'output/layers/02/',
                layer=='key' ~ 'output/layers/03/',
                layer=='trx' ~ 'output/layers/04/',
                layer=='text' ~ 'output/layers/09/',
                layer=='lineup' ~ 'output/layers/05/',
                layer=='card' ~ 'output/layers/08/',
                layer=='crest' ~ 'output/layers/',
                layer=='overlay' ~ ''
            ),LATEST)) %>%
        ungroup() %>% 
        arrange(IDX) %>% 
        subset(IDX%in%idx_range)
    frame_setup <- all_frames %>% subset(IDX==min(IDX))
    frame_range <- all_frames %>% subset(FILE!=PREV|is.na(PREV)|is.na(FILE))
    
    for(i in paste0(unique(all_frames$layer),'_png')) if(exists(i)) rm(list=i)
    for(lyr in unique(all_frames$layer)){
        FILENAME <- frame_setup %>% subset(layer==lyr) %>% pull(LATEST)
        if(!str_detect(FILENAME,'NA')) assign(paste0(lyr,'_png'),image_read(FILENAME))
    } 
    for(idx in unique(frame_range$IDX)){
        this_frame <- frame_range %>% 
            subset(IDX==idx)
        for(lyr in unique(this_frame$layer)){
            FILENAME <- this_frame %>% subset(layer==lyr) %>% pull(FILENAME)
            FILE <- this_frame %>% subset(layer==lyr) %>% pull(FILE)
            if(is.na(FILE)) next
            if(FILE=='NA') next
            assign(paste0(lyr,'_png'),image_read(FILENAME))
        }
        for(lyr in unique(all_frames$layer)){
            if(lyr=='base'){
                output <- base_png
            }else{
                FILE <- tail(c('X',this_frame %>% subset(layer==lyr) %>% pull(FILE)),1)
                if(is.na(FILE)) next
                if(exists(paste0(lyr,'_png'))) output <- output %>% image_composite(get(paste0(lyr,'_png')))
            }
        }
        image_write(output,paste0('output/layers/99/Frame_',str_pad(idx,5,pad='0'),'.png'))
    }
}