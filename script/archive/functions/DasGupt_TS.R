#df = dataframe
#f = factor of interest, character
#pop = population varaible (e.g. year)
#populations = character vector of populations to include
#baseline = baseline population to standardize against
#method?
#... factors to adjust by
#DasGupt_TS<-function(df,f,pop,populations=NULL,baseline=NULL,method="DG",...){
DasGupt_TS<-function(df,f,pop,...){
  pop=enquo(pop)
  factrs=map_chr(enquos(...),quo_name)
  nfact=length(factrs)
  #factrs=c("prev","age_str","freq","disposal_prop","crime_type_prop")
  
  #factor of interest.
  #f
  allpops=distinct(df,!!pop) %>% unlist %>% unname
  
  populations=NULL
  if(!is.null(populations)){
    allpops=allpops[allpops %in% populations]
  }
  
  baseline=NULL
  # if(is.null(baseline)){
    allpops %>% combinat::combn(.,2) %>% as_tibble(.,.name_repair="universal") -> pairwise_pops
  # } else{
  #   allpops[allpops!=baseline] %>% combinat::combn(.,1) -> compyears
  #   matrix(c(rep(baseline,length(compyears)),compyears),ncol=length(compyears),byrow=T) %>%
  #     as_tibble() -> pairwise_pops
  # }
    
  #okay, so start by applying dg_p() to each pairwise combination
  map(pairwise_pops,~filter(df,!!pop %in% .x)) %>%
  map(.,~dg_p(.,!!pop,factrs)) -> dgp_res
  #let's change the names to ensure we keep track of which pairwise standardisations are which
  names(dgp_res)<-map(pairwise_pops,~paste0("pops",paste(.,collapse="vs")))
  #unlist and tibble up
  dgp_res %>% map(.,~unlist(.,recursive=F) %>% as_tibble) -> dgp_res
  
  
  #separate out the adjusted rates and the factor effects. DG has d
  dgp_res %>% unlist(recursive = F) %>% as_tibble() %>%
    select(contains(f),-contains("factor")) -> dgp_rates

  
  
  #std_rates
  standardized_rates<-map_dfc(allpops,~std_rates_Npops(dgp_rates,allpops,as.character(.),f))
  #these are the standardized rate for factor f in each year, stnadardixed over all Ys.
  
  #now for the factor effects
  dgp_res %>% unlist(recursive = F) %>% as_tibble() %>%
    select(contains(paste0(f,".factor"))) -> dgp_facteffs
  difference_effects<-map_dfc(pairwise_pops,~fact_effects_Npops(dgp_facteffs,.,allpops))
  
  
  bind_cols(standardized_rates, difference_effects)
}
