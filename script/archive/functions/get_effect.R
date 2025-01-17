get_effect <- function(df2,pop,i,factrnames){
  #how many factors?
  nfact=length(factrnames)
  
  df2 %>% mutate(
    factor_df2 = map(factor_df, magrittr::extract, factrnames[-i]),
    alpha = map(factor_df, magrittr::extract, factrnames[i]) %>% map(.,1),
    pop_prod = map2(pop_prods,alpha, ~ (.x/.y))
  ) -> qdf
  
  #these are all the population factors (for both populations), spread. 
  #this means that indices 1:n/2 are pop1, and n/2:n are pop2.
  pop_facts<-qdf %>% select(!!pop,factor_df2) %>% spread(!!pop,factor_df2) %>% unnest()
  
  
  #DG's formula on p15 requires all different permutations of sets of all factors where factors are taken from either population.
  #I figured the easiest way to do this might be to use permutations of column indices in pop_facts
  r=ceiling(nfact/2)-1
  all_perms<-map(1:r,~unique(c(combinat::permn(c(rep(1,nfact-1-.x),rep(2,.x))), combinat::permn(c(rep(2,nfact-1-.x),rep(1,.x))))))
  #relevant later
  length_perms <- map_dbl(all_perms,length)
  denominators=map_dbl(1:length(length_perms),~nfact*(ncol(utils::combn(nfact-1,.))))
  
  #extract values, calculate products
  prod_tibs<-tibble(
    #these are translating the all_perms values into column indices
    colnums = all_perms %>% unlist(.,recursive=F) %>% map(.,~c(which(.x>1)+(nfact-1),which(.x<2))),
    # how many combinations are there? (these are the denominators for DG formula)
    colcombs = rep(length_perms,times=length_perms),
    # factor values
    fact_vals = map(colnums,~pop_facts[,.x]),
    # as matrix and rowProduct
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
  tibble(
    !!paste0("pop",qdf[2,quo_name(pop)]):=effects[[2]],
    !!paste0("pop",qdf[1,quo_name(pop)]):=effects[[1]],
    factoreffect=effects[[2]]-effects[[1]]
  )
}
