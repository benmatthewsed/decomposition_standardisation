source("script/dg_p.R")
testdf3<-readRDS("data/testdf_3years.RDS")


#DasGupt_TS<-function(df,years=list(),baseline=NULL,method="DG",...){
factrs=c("prev","age_str","freq","disposal_prop","crime_type_prop")
#  factrs=map_chr(enquos(...),quo_name)
  nfact=length(factrs)

  
  #if(is.null(baseline)){
    distinct(testdf3,year) %>% pull(year) %>% combinat::combn(.,2) %>% as_tibble() -> pairwise_pops
  #} else{
  #  distinct(testdf3,year) %>%
  #    filter(year!=baseline) %>% pull(year) %>% combinat::combn(.,1) -> compyears
  #  matrix(c(rep(baseline,length(compyears)),compyears),ncol=length(compyears),byrow=T) %>%
  #    as_tibble() -> pairwise_pops
  #}
  
  
  map(pairwise_pops,~filter(testdf3,year %in% .x)) %>%
  map(.,~dg_p(.,year,prev,age_str,freq,disposal_prop,crime_type_prop)) -> 
  
  effs %>% str
  names(effs)<-pairwise_pops %>% as.character()
  
  testdf3 %>% select(age,crime_type,disposal) %>% distinct %>%
    bind_cols(.,
              effs %>% map(.,"prev_effect")  %>% as_tibble
    ) -> effs 
  head(effs)
  
  
  
  
  
}











unique(testdf3$year) %>% combinat::combn(.,2) %>% as_tibble() -> pairwise_pops

tsDG<-function(df,years=list(),baseline=NULL,method="DG")
  
  
  map(pairwise_pops,~filter(testdf3,year %in% .x)) %>%
  map(.,~dg_p(.,year,prev,age_str,freq,disposal_prop,crime_type_prop))-> effs #%>% 

effs %>% str
names(effs)<-pairwise_pops %>% as.character()

testdf3 %>% select(age,crime_type,disposal) %>% distinct %>%
  bind_cols(.,
            effs %>% map(.,"prev_effect")  %>% as_tibble
  ) -> effs 
head(effs)

#sum of population 2v1 and 3v2 should == 3v1
effs
sum(effs[1,c(4,6)])
abs(effs[1,5])


#currently rowmeans, because not sure how the composite time series bit works
map(unique(testdf3$year),~as.character(pairwise_pops)[grepl(.,as.character(pairwise_pops))]) %>%
  map(.,~rowMeans(effs[.])) %>% 
  bind_cols(testdf3 %>% select(age,crime_type,disposal) %>% distinct, .) %>%
  group_by(age) %>%
  summarise(
    y1=sum(V1),
    y2=sum(V2),
    y3=sum(V3)
  ) %>% gather(.,"timepoint","prev_effect",y1:y3) %>%
  ggplot(.,aes(x=age, y=prev_effect))+geom_point()+
  transition_states(timepoint)+
  NULL

require(gganimate)  

