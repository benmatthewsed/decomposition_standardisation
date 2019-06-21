library(here)
library(tidyverse)
library(matrixStats)
library(conflicted)
library(combinat)
conflict_prefer("filter", "dplyr")
#devtools::install_github("josiahpjking/DasGuptR",force=T)
library(DasGuptR)

#e.g. taken from Ben's code.
readRDS(here("data","testdf_long.RDS")) -> testdf
readRDS(here("data","BMresult.RDS")) -> bm
readRDS("data/testdf_3years.RDS") -> testdf3

#some decomposing..
dg2pops <- DasGupt_2pop(testdf,year,c("prev","age_str","freq","disposal_prop","crime_type_prop"))
dg2pops_again <- DasGupt_Npop(testdf,pop=year,prev,age_str,freq,disposal_prop,crime_type_prop)
dgtimeseries <- DasGupt_Npop(testdf3,pop=year,prev,age_str,freq,disposal_prop,crime_type_prop)

####
#MODEL OUTPUT
####
#get the prev factor diffs
dgtimeseries$prev$factor_effects
#or standardised rates
dgtimeseries$prev$standardised_rates


####
#MODEL FIT!
####
fe<-map(dgtimeseries,"factor_effects")  # extract factor effects 
fe<-fe[-1] #weirdly the output seems to have an empty element at the start. we'll need to get rid of that
map(fe,"diff1989_2011") %>% as_tibble(.,name_repair=F) %>% rowSums %>% sum(.)*10000 # get all the diffs, rowSum, then sum
map(fe,"diff1989_2018") %>% as_tibble(.,name_repair=F) %>% rowSums %>% sum(.)*10000 

#this is the original data, 
testdf3 %>% mutate(
  crude=prev*age_str*freq*disposal_prop*crime_type_prop   #crude rates
) %>% 
  group_by(year) %>% 
  summarise(
    cruderate=sum(crude)*10000
  ) %>% mutate(
    diff=c(0,diff(cruderate)), #difference between years
    cumdiff=cumsum(diff) #cumulative differnce - gives diff to first year
  )

#mine matches?