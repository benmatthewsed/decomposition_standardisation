require(tidyverse)
require(matrixStats)

#take from Ben's code.
testdf <- test_dat$data.x[[1]] %>% mutate(year=1989) %>%
  bind_rows(.,
            test_dat$data.y[[1]] %>% mutate(year=2011)
  )
testdf


#below, i'm essentially trying to generalise the dg_five_nest function to P factors. 
#

#arguments (to be !!'d once running)
testdf$pop<-testdf$year
df<-testdf
facts=c("prev","age_str","freq","disposal_prop","crime_type_prop")
nfact=5


dg_p<-function(df,pop,...){
  #function stuffs
  out=enquo(out)
  pop=enquo(pop)
  nfact = length(...)
  
  
  df %>% group_by(pop) %>%    
    nest() %>%
    #these are the factors we're interested in
    mutate(
      factors_2_std = map(data, magrittr::extract,c("age_str","freq","disposal_prop","crime_type_prop")) %>% 
        map(.,as.matrix) #replace strings with sym(...)
    ) %>% 
    #select(-data) %>% spread(pop, factors_2_std) %>% unnest()  #not here.. 
    mutate(
      pop_prods=map(factors_2_std,rowProds)     #calculate total products (can simply divide by \alpha afterwards)
    ) %>% print
    # now comes difficult part.. the different combinations..
    facts
  
  
  
}


       