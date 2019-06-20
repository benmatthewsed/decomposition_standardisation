DG612numerator<-function(df,y1,y2,yj){
  a12=df[,grepl(y1,names(df)) & grepl(y2,names(df))]
  a2j=df[,grepl(y2,names(df)) & !grepl(paste0("vs",y2),names(df)) & grepl(yj,names(df))]
  a2jn=-df[,grepl(paste0("vs",y2),names(df)) & grepl(yj,names(df))]
  a1j=-df[,grepl(y1,names(df)) & !grepl(paste0("vs",y1),names(df)) & grepl(yj,names(df))]
  a1jn=df[,grepl(paste0("vs",y1),names(df)) & grepl(yj,names(df))]
  tibble(
    a12=a12[[1]],
    a2j=ifelse(length(a2j!=0),a2j,a2jn) %>% unlist,
    a1j=ifelse(length(a1j!=0),a1j,a1jn) %>% unlist
  ) %>% rowSums()
}


fact_effects_Npops<-function(factoreffs,ps,all_p){
  y1=ps[1]
  y2=ps[2]
  #all_y=pws_pops %>% unlist %>% unique
  map_dfc(all_p[!(all_p %in% c(y1,y2))],~DG612numerator(factoreffs,y1,y2,.)/length(all_p)) %>%
    tibble(
      dg612=rowSums(.),
      a12=factoreffs[,grepl(y1,names(factoreffs)) & grepl(y2,names(factoreffs))][[1]],
      !!paste0("diff",y1,"_",y2):=a12-dg612
    ) %>% select(paste0("diff",y1,"_",y2))
}


std_rates_Npops<-function(srates,all_p,y,fctr){
  map(all_p[!(all_p %in% y)],
      ~srates[,grepl(paste0(fctr,".pop",.x),names(srates))]
  ) %>% 
    map(.,~mutate_at(.,vars(matches(y)),~(-length(vars(matches(y)))*.x))) %>%
    map(.,rowSums) %>% as_tibble(.,.name_repair="universal") %>%
    mutate(
      sum2=rowSums(.)/length(srates),
      sum1=rowSums(srates[,grepl(paste0(fctr,".pop",y),names(srates))])/sum(grepl(paste0(fctr,".pop",y),names(srates))),
      !!(paste0("pop",y)):=sum1+sum2
    ) %>% select(!!paste0("pop",y))
}
