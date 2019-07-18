DGmodel<-function(dgdata,rawdata,pop1,pop2){
  
  enquo(!!paste(unique(dgdata$factor),collapse="*"))
  rdata2 %>% mutate(crude=(expr(paste(unique(dgdata$factor),collapse="*")))) %>%
    filter(year %in% c(pop1,pop2)) %>%
    group_by(year) %>% summarise(cruderate=sum(crude)) %>%
    mutate(diff=c(0,diff(cruderate,lag=1))) -> cruderates
  dgdata %>% group_by(factor) %>%
    summarise(
      !!paste(pop1):=sum(get(paste0("pop",pop1))),
      !!paste(pop2):=sum(get(paste0("pop",pop2))),
      difference = sum(get(paste0("diff",pop1,"_",pop2)))
    ) -> decomps
  list("cruderates"=cruderates,"decomps"=decomps)
}


summary.dgmod<-function(dgmod){
  list("decomposition"=dgmod$decomps,
     "sum of decomposition effects"=sum(dgmod$decomps$difference),
     "crude"=tibble(
      factor="crude_rate",
      !!paste(pop1):=dgmod$cruderates %>% filter(year==pop1) %>% pull(cruderate),
      !!paste(pop2):=dgmod$cruderates %>% filter(year==pop2) %>% pull(cruderate),
      difference=dgmod$cruderates %>% filter(year==pop2) %>% pull(diff)
    ),
    "model fit"=1-(sum(dgmod$decomps$difference)-sum(dgmod$cruderates$diff))
  ) 
}



na.omit(rdata2) %>% filter(year<2008) %>%
  DasGupt_Npop(.,pop=year,prev_reconv,freq_reconv,age_str,id_vars=c(Age,Gender,laa)) -> dgdata
dgdata

dgdata
na.omit(rdata2) %>% filter(year<2008) %>% gather(factor,raw_factor,8:10) %>% 
  mutate(year=paste0("pop",year),
         factor=paste0("raw_",factor)) %>%
  select(factor,raw_factor,year,laa,Gender,Age) %>% 
  spread(year,raw_factor) -> d

bind_cols(dgdata,d) %>% summary


