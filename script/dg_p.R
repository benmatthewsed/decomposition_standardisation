require(here)
require(tidyverse)
require(matrixStats)
require(conflicted)
require(combinat)
conflict_prefer("filter", "dplyr")
splitAt <- function(x, pos) unname(split(x, cumsum(seq_along(x) %in% pos)))
source(here("script","get_effect.R"))

dg_p<-function(df,pop,...){
#   #function stuffs
#   out=enquo(out)
  pop=enquo(pop)
  factrs=map_chr(enquos(...),quo_name)
  nfact=length(factrs)
  print(factrs)
  #########
  #the eg. arguments (to be !!'d once running)
  #########
  #testdf$pop<-testdf$year
  #df<-testdf
  #factrs=c("prev","age_str","freq","disposal_prop","crime_type_prop")
  #nfact=5
  


  ######
  #function code
  ######
  #nest and make factor matrix
  df %>% group_by(!!pop) %>%
    nest() %>%
    mutate(
      factor_df = map(data, magrittr::extract,factrs), #replace factrs with sym(...)
      factor_mat = map(factor_df,as.matrix),
      pop_prods=map(factor_mat,rowProds) #calculate total products (can simply divide by \alpha afterwards)
    ) -> df_nested

  #
  #equivalent to Q1, Q2, ....  in Ben's function, this will loop over them
  suppressMessages(map_dfc(1:nfact,~get_effect(df_nested,pop,.x,factrs))) #%>%
    #bind_cols(df %>% select(-factrs,-!!pop) %>% distinct,.))
}




#e.g. taken from Ben's code.
readRDS(here("data","testdf_long.RDS")) -> testdf
readRDS(here("data","BMresult.RDS")) -> bm


jk<-dg_p(testdf,year,prev,age_str,freq,disposal_prop,crime_type_prop) %>% 
  bind_cols(testdf %>% select(age,crime_type,disposal) %>% distinct,.)       
#check same as ben's
left_join(bm,jk) %>% select_if(is.numeric) %>% select(-age) %>% mutate_all(.,abs) %>%
  cor %>%
  ggcorrplot::ggcorrplot(.,lab=T)

