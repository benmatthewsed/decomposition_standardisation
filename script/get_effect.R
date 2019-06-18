get_effect <- function(df2,pop,i,factrnames){
  nfact=length(factrnames)
  
  df2 %>% mutate(
    factor_df2 = map(factor_df, magrittr::extract, factrnames[-i]),
    alpha = map(factor_df, magrittr::extract, factrnames[i]) %>% map(.,1),
    pop_prod = map2(pop_prods,alpha, ~ (.x/.y))
  ) -> qdf
  
  pop_facts<-qdf %>% select(!!pop,factor_df2) %>% spread(!!pop,factor_df2) %>% unnest()
  
  #these are the permutations of column indices in pop facts according to DG formula (p15)
  r=ceiling(nfact/2)-1
  all_perms<-map(1:r,~unique(c(combinat::permn(c(rep(1,nfact-1-.x),rep(2,.x))), combinat::permn(c(rep(2,nfact-1-.x),rep(1,.x))))))
  #relevant later
  length_perms <- map_dbl(all_perms,length)
  denominators=map_dbl(1:length(length_perms),~nfact*(ncol(utils::combn(nfact-1,.))))
  
  #extract values, calculate products
  prod_tibs<-tibble(
    colnums = all_perms %>% unlist(.,recursive=F) %>% map(.,~c(which(.x>1)+(nfact-1),which(.x<2))),
    colcombs = rep(length_perms,times=length_perms),
    fact_vals = map(colnums,~pop_facts[,.x]),
    fact_valsm = map(fact_vals,as.matrix),
    prods = map(fact_valsm,rowProds)
  ) %>% pull(prods) %>% as_tibble(.,.name_repair="universal")
  
  map(splitAt(1:ncol(prod_tibs),cumsum(length_perms+1)), ~prod_tibs[.x]) %>%
    map(.,rowSums) %>%
    map2_dfc(.,denominators, ~(.x/.y)) %>% 
    #as_tibble(.,.name_repair="universal") %>% 
    #add in the first part of the equation (abcd+ABCD)
    mutate(
      p0=qdf %>% select(!!pop,pop_prod) %>% 
        spread(!!pop,pop_prod) %>% 
        unnest %>% 
        rowSums() %>% 
        map2_dbl(.,nfact,~(.x/.y))
    ) -> sum_prods
  
  #extract alpha and multiply by Q
  qdf %>% select(!!pop,alpha) %>% spread(!!pop,alpha) %>% unnest %>%
    map(.,~.x*rowSums(sum_prods)) -> effects
  return(effects[[2]]-effects[[1]])
}

