dg_p<-function(df,pop,factrs){
#   #function stuffs
#   out=enquo(out)
  pop=enquo(pop)
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
  decomp_out<-suppressMessages(map(1:nfact,~get_effect(df_nested,pop,.x,factrs)))
  names(decomp_out)<-factrs
  return(decomp_out)
    #bind_cols(df %>% select(-factrs,-!!pop) %>% distinct,.))
  #rename_at(vars(names(.)),~paste0(factrs,"_effect"))
}




# #e.g. taken from Ben's code.
# readRDS(here("data","testdf_long.RDS")) -> testdf
# readRDS(here("data","BMresult.RDS")) -> bm
# 
# 
# dg_p(testdf,year,c("prev","age_str","freq","disposal_prop","crime_type_prop")) %>% 
#   map(.,~bind_cols(testdf %>% select(age,crime_type,disposal) %>% distinct,.)) -> jk_output       
# 
# 
# 
# 
# jk_output %>% map(.,magrittr::extract,"factoreffect") %>% unlist(recursive=F) %>% as_tibble() %>% 
#   bind_cols(testdf %>% select(age,crime_type,disposal) %>% distinct,.)-> jk
# 
# 
# #check same as ben's
# left_join(bm,jk,by=c("age","crime_type","disposal")) %>% select_if(is.numeric) %>% select(-age) %>% 
#   cor %>%
#   ggcorrplot::ggcorrplot(.,lab=T)
# 
# jk %>% group_by(age) %>%
#   summarise(
#     prev_effect = sum(prev.factoreffect),
#     age_effect = sum(age_str.factoreffect),
#     freq_effect = sum(freq.factoreffect)
#   ) %>% ggplot(.,aes(x=age,y=prev_effect))+geom_point()+stat_smooth()
# 
# jk %>% #group_by(age) %>%
#   summarise(
#     prev_effect = sum(prev.factoreffect),
#     age_effect = sum(age_str.factoreffect),
#     freq_effect = sum(freq.factoreffect),
#     disposal_prop_effect = sum(disposal_prop.factoreffect),
#     crime_type_prop_effect = sum(crime_type_prop.factoreffect)
#   ) %>% mutate(
#     totaldiff=rowSums(abs(.)),
#     prev=100*(prev_effect/totaldiff),
#     age_ef=100*(age_effect/totaldiff),
#     freq=100*(freq_effect/totaldiff),
#     disp=100*(disposal_prop_effect/totaldiff),
#     crim=100*(crime_type_prop_effect/totaldiff)
#   ) %>% select(prev,age_ef,freq,disp,crim) %>% abs
# #this is the percentage distribution of effects.
# 
# jk_output %>% map(.,magrittr::extract,c("pop1989","pop2011")) %>% unlist(recursive=F) %>% as_tibble() %>% 
#   bind_cols(testdf %>% select(age,crime_type,disposal) %>% distinct,.) %>%
#   group_by(age) %>%
#   summarise_at(vars(matches(".pop")),~sum(.x)*10000) %>% 
#   gather(.,"factor","adj_rate",prev.pop1989:crime_type_prop.pop2011) %>%
#   separate(factor,c("factor","pop"),"\\.") %>% 
#   spread(pop,adj_rate) %>%
#     ggplot(.,aes(x=age))+
#     geom_point(aes(y=pop1989))+stat_smooth(aes(y=pop1989))+
#     geom_point(aes(y=pop2011),col="darkorchid")+stat_smooth(aes(y=pop2011),col="darkorchid",fill="darkorchid")+
#     facet_wrap(~factor)+
#     NULL
#     
# testdf %>% mutate(rawrate=prev*freq*age_str) %>%
#   group_by(age,year) %>% summarise(rate=sum(rawrate)*10000) %>%
#   ggplot(.,aes(x=age,y=rate,col=factor(year)))+geom_point()+stat_smooth()

