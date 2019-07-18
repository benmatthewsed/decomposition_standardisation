require(tidyverse)
require(matrixStats)
library(conflicted)
require(combinat)
conflict_prefer("filter","dplyr")
conflict_prefer("select","dplyr")
rdata<-readRDS("recdata.RDS") # this is the reconviction data


#######
#plot "reconviction rate"

rdata %>% group_by(year,Gender,Age) %>%
  summarise_at(vars(contains("num")),sum) %>%
  mutate(
    rate = (num_reconvicted/num_offenders)*100,
    freq = num_reconvictions/num_offenders,
    Age = fct_relevel(Age,"under 21")
  ) %>% 
  ggplot(.,aes(x=year,y=rate,col=Gender))+
  geom_path()+facet_wrap(~Age)+
  ylab("Reconviction rate")+
  theme_bw()+theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  NULL -> p
plotly::ggplotly(p)

######
#DAS GUPTR

#get population age structure
popdata<-readRDS("script/archive/age_structure_Scotland.RDS") %>% 
  rename(pop_agestr=age_str)
#popdata2<-readRDS("script/agestructure_scotlandNUM.RDS")

rdata %>% group_by(year,Gender,Age) %>%
  summarise_at(vars(contains("num")),sum) %>% 
  left_join(., rdata %>% group_by(year) %>% summarise(total=sum(num_offenders))
  ) %>%
  mutate(
    age_str=num_offenders/total, # this is the age structure of convicted population
    prev=num_reconvicted/num_offenders, # this is the prevalence of reconviction
    freq=num_reconvictions/num_reconvicted) %>% # this is the frequency or reconviction (according to SG)
  left_join(.,popdata) %>% ungroup -> rdata

popdata2<-readRDS("script/agestructure_scotlandNUM.RDS")
rdata %>% left_join(.,popdata2) -> rdata



lapply(list.files("../DasGuptR/R/",full.names = T),source)
# or library(DasGuptR)
na.omit(rdata) %>% 
  DasGupt_Npop(.,pop=year,prev,freq,age_str,id_vars=c(Age,Gender)) -> dg_decompose
#DasGupt_Npop(.,pop=year,prev,freq,age_str,pop_agestr,id_vars=c(Age,Gender)) -> dg_decompose

#######
#results

#the fall in reconviction rates between 
pop1=2004
pop2=2016



#these are our 'crude rates' in terms of DG "rate as product of factors"
rdata %>% mutate(dgcrude_rate=prev*freq*age_str) %>% 
  group_by(year) %>% summarise_at(vars(matches("rate|num")),sum) %>%
  mutate(sg_rate=num_reconvictions/num_offenders,
         s=num_reconvictions/num_offenders) #they are the same as the SG "average number of reconvictions per offender"

#CRUDE RATE DIFFERENCE
rdata %>% mutate(rate=prev*freq*age_str) %>%
  filter(year %in% c(pop1,pop2)) %>%
  group_by(year) %>% summarise(cruderate=sum(rate)) %>%
  mutate(factor="crude",
         diff=c(0,diff(cruderate,lag=1)),
         cumdiff=cumsum(diff)
  )
sum(dg_decompose$diff2004_2016) # quick check that our decomposition effect sum to crude diff


#ADJUSTED RATES
dg_decompose %>% 
  group_by(factor) %>% 
  summarise(
    !!paste0("rate",pop1):=sum(get(paste0("pop",pop1))),
    !!paste0("rate",pop2):=sum(get(paste0("pop",pop2)))
  ) %>% gather(year,rate,starts_with("rate")) %>% mutate(year=as.numeric(gsub("rate","",year))) -> std_rates

#DG TABLE - CRUDE & DECOMP
rdata %>% mutate(rate=prev*freq*age_str) %>%
  filter(year %in% c(pop1,pop2)) %>%
  group_by(year) %>% summarise(rate=sum(rate)) %>%
  mutate(factor="crude") %>%
  spread(year,rate) %>% mutate(difference=get(paste0(pop1))-get(paste0(pop2))) %>%
  #bind, and diff
  bind_rows(std_rates %>%
              spread(year,rate) %>%
              mutate(difference=get(paste0(pop1))-get(paste0(pop2))),
            .) %>%
  mutate(percent_dist=100*(difference/difference[factor=="crude"]))

#sum(dg_decompose$diff2004_2016[dg_decompose$factor=="prev"]) # another check

#So, the change in the crude rate ("average number of reconvictions per offender") is largely due to the change in the prevalence of reconviction, rather than the frequency. the changing age structure of the convicted population is also pretty important.


##########
#PLOTTING

#can we visualise the adjusted rates?
#yes!
rdata %>% mutate(rate=prev*freq*age_str) %>%
  group_by(year) %>% 
  summarise(
    rate=sum(rate),
    factor="crude"
  ) %>% 
  bind_rows(.,
            dg_decompose %>% 
              group_by(factor) %>%
              summarise_at(vars(starts_with("pop")),sum) %>% 
              gather(year,rate,starts_with("pop")) %>%
              mutate(year=as.numeric(gsub("pop","",year)))
  ) %>%
  mutate(
    rate_type=fct_recode(factor(factor),
                         "Crude"="crude",
                         "adjusted for frequency and\nprevalence of reconvictions"="age_str",
                         "adjusted for offender mix and\nfrequency of reconvictions"="prev",
                         "adjusted for offender mix and\nprevalence of reconvictions"="freq"
    )
  ) %>%
  ggplot(.,aes(x=year,y=rate,col=rate_type))+
  geom_path()+
  #scale_color_manual(values = c("red","black", "blue", "green"))+
  theme_bw()+
  ylab("average number of convictions per offender")+
  NULL -> p1
plotly::ggplotly(p1)
# red line. what it would look like if prevalence of reconviction and numbers of reconvictions per reconvicted were held constant
# blue line. what it would look like if prevalence of reconviction and offender mix were held constant
# green line. what it would look like if number of reconvictions per reconvicted and offender mix were held constant



########
#and for each age/sex group?
na.omit(rdata) %>% 
  DasGupt_Npop(.,pop=year,prev,freq,id_vars=c(Age,Gender)) -> dg_decompose2
#get the crude rates for each age/sex group
rdata %>% mutate(rate=prev*freq) %>%
  group_by(year,Age,Gender) %>% 
  summarise(
    rate=sum(rate),
    factor="crude"
  ) %>% 
  #get the adjusted rates for each age/sex group
  bind_rows(.,
            dg_decompose2 %>% 
              group_by(factor,Age,Gender) %>%
              summarise_at(vars(starts_with("pop")),sum) %>% 
              gather(year,rate,starts_with("pop")) %>%
              mutate(year=as.numeric(gsub("pop","",year))) #%>%
            #left_join(rdata %>% select(year,Age,Gender,age_str)) %>%
            #mutate(rate=rate/age_str)
  ) %>% ungroup %>%
  mutate(
    rate_type=fct_recode(factor(factor),
                         "Crude"="crude",
                         "Frequency adjusted"="prev",
                         "Prevalence adjusted"="freq"
    ),
    Age=fct_relevel(Age,"under 21")
  ) %>%
  ggplot(.,aes(x=year,y=rate,lty=factor,col=Gender))+
  facet_grid(~Age)+
  geom_path()+
  theme_bw()+
  NULL -> p2
plotly::ggplotly(p2)






########
#and this just freq and age/sex adjusted
na.omit(rdata) %>% mutate(freq=num_reconvictions/num_offenders) %>%
  DasGupt_Npop(.,pop=year,age_str,freq,id_vars=c(Age,Gender)) -> dg_decompose3
#get the crude rates for each age/sex group
rdata %>% mutate(freq=num_reconvictions/num_offenders) %>%
  mutate(rate=freq*age_str) %>%
  group_by(year) %>% 
  summarise(
    rate=sum(rate),
    factor="crude"
  ) %>% 
  bind_rows(.,
            dg_decompose3 %>% 
              group_by(factor) %>%
              summarise_at(vars(starts_with("pop")),sum) %>% 
              gather(year,rate,starts_with("pop")) %>%
              mutate(year=as.numeric(gsub("pop","",year)))
  ) %>%
  mutate(
    rate_type=fct_recode(factor(factor),
                         "Crude"="crude",
                         "Frequency adjusted"="age_str",
                         "age/sex adjusted"="freq"
    )
  ) %>%
  ggplot(.,aes(x=year,y=rate,col=rate_type))+
  geom_path()+
  scale_color_manual(values = c("red","black", "blue", "green"))+
  theme_bw()+
  ylab("average number of convictions per offender")+
  NULL




######
#and this just prev and age/sex adjusted
na.omit(rdata) %>% 
  DasGupt_Npop(.,pop=year,age_str,prev,id_vars=c(Age,Gender)) -> dg_decompose4
#get the crude rates for each age/sex group
rdata %>% 
  mutate(rate=prev*age_str) %>%
  group_by(year) %>% 
  summarise(
    rate=sum(rate),
    factor="crude"
  ) %>% 
  bind_rows(.,
            dg_decompose4 %>% 
              group_by(factor) %>%
              summarise_at(vars(starts_with("pop")),sum) %>% 
              gather(year,rate,starts_with("pop")) %>%
              mutate(year=as.numeric(gsub("pop","",year)))
  ) %>%
  mutate(
    rate=rate*100,
    rate_type=fct_recode(factor(factor),
                         "Crude"="crude",
                         "prevalence of reconv adjusted"="age_str",
                         "age/sex adjusted"="prev"
    )
  ) %>%
  ggplot(.,aes(x=year,y=rate,col=rate_type))+
  geom_path()+
  scale_color_manual(values = c("red","black", "blue", "green"))+
  theme_bw()+
  ylab("reconviction rate\n(percentage of offenders reconvicted)")+
  NULL->p3
plotly::ggplotly(p3)


