library(tidyverse)
library(here)
library(readxl)
require(matrixStats)
require(combinat)
library(conflicted)
#library(DasGuptR)
conflict_prefer("filter","dplyr")
conflict_prefer("select","dplyr")
lapply(list.files("../DasGuptR/R",full.names = T),source)

rdata<-readRDS("recdata.RDS")
popdata<-readRDS("age_structure_Scotland.RDS")


left_join(rdata,
          popdata %>% filter(year>=2004)
) %>% filter(year<2018) -> rdata

rdata %>% group_by(Gender,Age,year) %>%
  summarise_at(vars(starts_with("num")),sum) %>%
  left_join(popdata) %>%
  mutate(
    prev_reconv = ifelse(is.nan(num_reconvicted/num_offenders),0,num_reconvicted/num_offenders),
    freq_reconv = ifelse(is.nan(num_reconvictions/num_reconvicted),0,num_reconvictions/num_offenders)
  ) -> rdata2

na.omit(rdata2) %>% #ungroup %>% filter(year<2008) %>%
  DasGupt_Npop(.,pop=year,prev_reconv,freq_reconv,age_str,id_vars=c(Age,Gender)) -> dg_decompose

#############################
#
#############################
#
na.omit(rdata2) %>% mutate(rate=age_str*prev_reconv*freq_reconv) %>%
  group_by(year) %>% 
  summarise(
    factor="crude",
    rate=sum(rate)
  ) -> crudes

crudes %>% mutate(
    diff=c(0,diff(rate)), #difference between years
    cumdiff=cumsum(diff) #cumulative differnce - gives diff to first year
  )
sum(dg_decompose$diff2004_2016) # get all the diffs, rowSum, then sum

dg_decompose %>% select(Age,Gender,factor,starts_with("diff")) %>% 
  gather(years,effect,starts_with("diff")) %>% 
  mutate(years=gsub("diff","",years)) %>%
  separate(years,c("year1","year2"),"_",convert=T) %>%
  mutate(diff=year2-year1) %>% filter(diff==1) %>%
  group_by(year1,year2,factor) %>% summarise(effect=sum(effect,na.rm=T)) %>%
  spread(factor,effect) %>% 
  # gather(factor,effect,3:5) %>%
  # ggplot(.,aes(x=year2,y=effect,col=factor))+geom_line()
  mutate(
    totalchange=abs(freq_reconv)+abs(prev_reconv)+abs(age_str),
    prevP=prev_reconv/totalchange*100,
    freqP=freq_reconv/totalchange*100,
    age_strP=age_str/totalchange*100
  ) %>%
  ungroup() %>%
  gather(.,factor,effect,ends_with("P")) %>% arrange(factor,year2) %>%
  mutate(effect=abs(effect)) %>%
  ggplot(.,aes(x=year2,y=effect))+
  geom_area(aes(fill=factor))+#facet_wrap(~laa)+
  NULL

DasGupt_Npop(na.omit(rdata2),pop=year,prev_reconv, freq_reconv,age_str) %>%
  map(., "standardised_rates") %>% unlist(recursive=F) %>%
  as_tibble %>% bind_cols(
    na.omit(rdata2) %>% select(Age,Gender) %>% distinct,.
  ) %>%
  gather(.,"var","rate",3:ncol(.)) %>% 
  separate(var,c("factor","year"),"\\.") %>%
  mutate(year=as.numeric(gsub("pop","",year))) -> rates_long

dg_tests<-map(c(2004,2016),~DasGupt_Npop(na.omit(rdata2),pop=year,prev_reconv, freq_reconv,age_str,baseline=.))
dg2004<-dg_tests[[1]]
dg2016<-dg_tests[[2]]
map(dg2004, "standardised_rates") %>% unlist(recursive=F) %>%
  as_tibble %>% bind_cols(
    na.omit(rdata2) %>% select(Age,Gender,laa) %>% distinct,.
  ) %>%
  gather(.,"var","rate",4:ncol(.)) %>% 
  separate(var,c("factor","year"),"\\.") %>%
  mutate(year=as.numeric(gsub("pop","",year))) -> rates_long2004
map(dg2016, "standardised_rates") %>% unlist(recursive=F) %>%
  as_tibble %>% bind_cols(
    na.omit(rdata2) %>% select(Age,Gender,laa) %>% distinct,.
  ) %>%
  gather(.,"var","rate",4:ncol(.)) %>% 
  separate(var,c("factor","year"),"\\.") %>%
  mutate(year=as.numeric(gsub("pop","",year))) -> rates_long2016

bind_rows(rates_long %>% 
              mutate(year=as.numeric(year)) %>% 
              group_by(year,factor) %>% 
              summarise(rate=sum(rate),baseline="std") %>% 
              ungroup,
          rates_long2004 %>% 
            mutate(year=as.numeric(year)) %>% 
            group_by(year,factor) %>% 
            summarise(rate=sum(rate),baseline="2005") %>% 
            ungroup) %>%
  bind_rows(.,
            rates_long2016 %>% 
              mutate(year=as.numeric(year)) %>% 
              group_by(year,factor) %>% 
              summarise(rate=sum(rate),baseline="2017") %>% 
              ungroup) %>%
  
  
  rates_long %>% 
  mutate(year=as.numeric(year)) %>% 
  group_by(year,factor) %>% 
  summarise(rate=sum(rate),baseline="std") %>% 
  ungroup %>%
  bind_rows(.,crudes) %>% 
  ggplot(.,aes(x=year,y=rate,group=baseline,col=baseline))+geom_line()+facet_wrap(~factor)










map(dg_decompose,"factor_effects") %>% unlist(recursive=F) %>% as_tibble(.name_repair = "universal") %>% 
  gather("fact_year","difference") %>% 
  separate(fact_year,c("factor","comp"),"\\.") %>%
  spread(difference,comp)
  




testdata<-read_csv("data/decomposition_error_reprex.csv")
#testdata2<-testdata
#fake some data
newdat<-function(y){tibble(age=unique(testdata$age),year=y,prev=sample(testdata$prev,75),age_str=sample(testdata$age_str,75),freq=sample(testdata$age_str,75))}
map_dfr(1990:2020,~newdat(.)) -> testdata2
dg_decompose_test <- DasGupt_Npop(testdata2,pop=year,prev,age_str,freq)
map(dg_decompose_test, "standardised_rates") %>% unlist(recursive=F) %>%
  as_tibble %>% bind_cols(
    na.omit(testdata2) %>% select(age) %>% distinct,.
  ) %>%
  gather(.,"var","rate",2:ncol(.)) %>% 
  separate(var,c("factor","year"),"\\.",convert=T) %>%
  mutate(year=gsub("pop","",year)) %>% 
  ggplot(.,aes(x=age,y=year,col=rate))+geom_point(size=3)+facet_wrap(~factor)+
  scale_colour_distiller(palette = "Reds", direction = 1)+
  NULL


