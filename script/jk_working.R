require(here)
require(tidyverse)
require(matrixStats)
require(conflicted)
require(combinat)
conflict_prefer("filter", "dplyr")
splitAt <- function(x, pos) unname(split(x, cumsum(seq_along(x) %in% pos)))

source("script/functions/get_effect.R")
source("script/functions/dg_p.R")
source("script/functions/Npop_innerfunctions.R")
source("script/functions/DasGupt_TS.R")
#e.g. taken from Ben's code.
readRDS(here("data","testdf_long.RDS")) -> testdf
readRDS(here("data","BMresult.RDS")) -> bm

testdf3<-readRDS("data/testdf_3years.RDS")

#run dg_p on 2 populations.
dg_p(testdf,year,c("prev","age_str","freq","disposal_prop","crime_type_prop")) %>%
  map(.,~bind_cols(testdf %>% select(age,crime_type,disposal) %>% distinct,.)) -> jk_output

#extract the factor effects
jk_output %>% map(.,magrittr::extract,"factoreffect") %>% unlist(recursive=F) %>% as_tibble() %>%
  bind_cols(testdf %>% select(age,crime_type,disposal) %>% distinct,.)-> jk
#check same as ben's
left_join(bm,jk,by=c("age","crime_type","disposal")) %>% select_if(is.numeric) %>% select(-age) %>%
  cor
#(JK results are .factoreffect, BM's are _effect. perfect correlation! hooray!)


#now lets do this for N populations.
DasGupt_TS(testdf3,f="prev",pop=year,prev,age_str,freq,disposal_prop,crime_type_prop) -> dgtimeseries

dgtimeseries
#THIS... (i think) is the age,freq,ctype,disposal adjusted prevalence rates standardized across years by DG's method, AND the factor effects.

bind_cols(testdf3 %>% select(age,crime_type,disposal) %>% distinct,dgtimeseries) %>%
  group_by(age) %>%
  summarise_at(vars(matches("pop")),~sum(.)*10000) %>% ggplot(.,aes(x=age))+
  geom_point(aes(y=pop1989))+stat_smooth(aes(y=pop1989))+
  geom_point(aes(y=pop2011))+stat_smooth(aes(y=pop2011),col="red")+
  geom_point(aes(y=pop2018))+stat_smooth(aes(y=pop2018),col="green")+
  NULL


#crude rates?
testdf3 %>% group_by(year) %>% summarise(crude=mean(prev)*10000)
#adjusted rates
bind_cols(testdf3 %>% select(age,crime_type,disposal) %>% distinct,dgtimeseries) %>%
  summarise_at(vars(matches("pop")),~sum(.)*10000)

