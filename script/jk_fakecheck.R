library(tidyverse)
library(matrixStats)
library(conflicted)
library(combinat)
conflict_prefer("filter", "dplyr")
#devtools::install_github("josiahpjking/DasGuptR",force=T)
#library(DasGuptR)
lapply(list.files("../DasGuptR/R",full.names = T),source)
testdata<-read_csv("data/decomposition_error_reprex.csv")

#fake some data
newdat<-function(y){tibble(age=unique(testdata$age),year=y,prev=sample(testdata$prev,75),age_str=sample(testdata$age_str,75),freq=sample(testdata$age_str,75))}
map_dfr(1990:2005,~newdat(.)) -> testdata2

#get crude rates
testdata2 %>% mutate(
  crude=prev*age_str*freq
) %>% 
  group_by(year) %>% 
  summarise(
    cruderate=sum(crude)*10000
  ) %>% mutate(
    diff=c(0,diff(cruderate)), #difference between years
    cumdiff=cumsum(diff) #cumulative differnce - gives diff to first year
  ) %>% print -> crudes



#some decomposing..
dg_decompose <- DasGupt_Npop(testdata2,pop=year,prev,age_str,freq)
#takes time! we should have a sensible print out

dg_decompose %>% str
map(dg_decompose, "factor_effects") %>% unlist(recursive=F) %>%
  as_tibble %>% bind_cols(
    testdata2 %>% select(age) %>% distinct,.
  ) %>%
  gather(.,"var","effect",2:ncol(.)) %>% 
  separate(var,c("factor","year"),"\\.") %>%
  mutate(year=gsub("diff","",year)) -> factor_effects_long
  
factor_effects_long %>%
separate(year,c("year1","year2"),"_",convert=T) %>%
  mutate(diff=year2-year1) %>% filter(diff==1) %>%
  group_by(year1,year2,factor) %>% summarise(effect=sum(effect)) %>%
  spread(factor,effect) %>%
  mutate(
    totalchange=abs(age_str)+abs(freq)+abs(prev),
    age_strP=age_str/totalchange*100,
    prevP=prev/totalchange*100,
    freqP=freq/totalchange*100
  ) %>%
  select(year1,year2,ends_with("P")) %>% ungroup() %>%
  gather(.,factor,effect,ends_with("P")) %>% arrange(factor,year2) %>%
  mutate(effect=abs(effect)) %>%
  ggplot(.,aes(x=year2,y=effect))+
  geom_area(aes(fill=factor))+
  NULL

