library(tidyverse)
library(here)
library(readxl)
require(matrixStats)
require(combinat)
library(conflicted)
#library(DasGuptR)
conflict_prefer("filter","dplyr")
conflict_prefer("select","dplyr")
lapply(list.files("../DasGuptR/R",full.names = T),source)

#download.file("https://www.gov.scot/binaries/content/documents/govscot/publications/statistics/2019/06/reconviction-rates-scotland-2016-17-offender-cohort/documents/additional-domestic-abuse-reconviction-tables-reconvictions-bulletin-2019/additional-domestic-abuse-reconviction-tables-reconvictions-bulletin-2019/govscot%3Adocument/additional-domestic-abuse-reconviction-tables-reconvictions-bulletin-2019.xlsx",              destfile = here("DAreconv.xlsx"),mode = "wb")
#download.file("https://www.gov.scot/binaries/content/documents/govscot/publications/statistics/2019/06/reconviction-rates-scotland-2016-17-offender-cohort/documents/additional-datasets--reconictionsbulletin2019/additional-datasets--reconictionsbulletin2019/govscot%3Adocument/additional-datasets-reconvictions-bulletin-2019.xlsx",destfile = here("reconv.xlsx"),mode = "wb")

excel_sheets(here("reconv.xlsx"))
rdata<-read_xlsx(here("reconv.xlsx"),sheet = 6,col_names=F,na="**")
names(rdata)<-c("ID","laa","year",as.character(rdata[2,4:10]),"nsure")
rdata[-c(1:3),] %>% select(-nsure,-ID) -> rdata

head(rdata)
names(rdata)
rdata %>% rename(
  num_offenders=`Number of offenders`,
  num_reconvicted=`no. reconvicted`,
  num_reconvictions=`number of reconvictions`
) %>% mutate_at(vars(matches("num|reconv")),as.numeric) %>%
  separate(year,c("year","year2"),"-",convert=T) %>%
  #mutate(
  #  year=year+2000
  #) %>% filter(year<2018) %>%
  select(-`recon freq. rate`,-`recon rate`) -> rdata

rdata <- rdata %>% filter(Age!="All", Gender!="All") %>% select(-year2)

saveRDS(rdata,"recdata.RDS")

left_join(rdata,
          popests %>% filter(year>2004)
) %>% filter(year<2018) -> rdata

rdata %>% mutate(
    prev_reconv = num_reconvicted/num_offenders,
    freq_reconv = num_reconvictions/num_offenders
  ) -> rdata2


dg_decompose <- DasGupt_Npop(rdata2,pop=year,prev_reconv, freq_reconv,age_str)

dg_decompose %>% str

map(dg_decompose, "factor_effects") %>% unlist(recursive=F) %>%
  as_tibble %>% bind_cols(
    rdata %>% select(Age,Gender,laa) %>% distinct,.
  ) %>%
  gather(.,"var","effect",4:ncol(.)) %>% 
  separate(var,c("factor","year"),"\\.") %>%
  mutate(year=gsub("diff","",year)) -> factor_effects_long

factor_effects_long %>%
  separate(year,c("year1","year2"),"_",convert=T) %>%
  mutate(diff=year2-year1) %>% filter(diff==1) %>%
  group_by(year1,year2,factor) %>% summarise(effect=sum(effect,na.rm=T)) %>%
  spread(factor,effect) %>%
  mutate(
    totalchange=abs(freq_reconv)+abs(prev_reconv)+abs(age_str),
    prevP=prev_reconv/totalchange*100,
    freqP=freq_reconv/totalchange*100,
    age_strP=age_str/totalchange*100
  ) %>%
  select(year1,year2,ends_with("P")) %>% ungroup() %>%
  gather(.,factor,effect,ends_with("P")) %>% arrange(factor,year2) %>%
  mutate(effect=abs(effect)) %>%
  ggplot(.,aes(x=year2,y=effect))+
  geom_area(aes(fill=factor))+
  NULL



map(dg_decompose, "standardised_rates") %>% unlist(recursive=F) %>%
  as_tibble %>% bind_cols(
    rdata2 %>% select(Age,Gender,laa) %>% distinct,.
  ) %>%
  gather(.,"var","rate",4:ncol(.))%>% 
  separate(var,c("factor","year"),"\\.",convert=T) %>%
  mutate(year=gsub("pop","",year)) -> rates_long

rdata2 %>% 
  mutate(rate=prev_reconv*freq_reconv*age_str) %>%
  group_by(year,laa) %>% summarise(cruder=sum(rate,na.rm=T)) %>%
  mutate(
    diff=c(0,diff(cruder,lag=1)),
    cdiff=cumsum(diff)
  ) %>% print -> crude_res
fe<-map(dg_decompose,"factor_effects")  # extract factor effects 
fe<-fe[-1] #weirdly the output seems to have an empty element at the start. we'll need to get rid of that
map(fe,"diff2005_2006") %>% as_tibble(.,name_repair=F) %>%
  bind_cols(.,distinct(rdata2,Age,Gender,laa)) %>%
  group_by(laa) %>% summarise_if(is.numeric,sum) %>% select_if(is.numeric) %>%
  rowSums(.,na.rm=T) #%>% sum(.)

crude_res %>% filter(year==2006)
map(fe,"diff1989_2018") %>% as_tibble(.,name_repair=F) %>% rowSums %>% sum(.)*10000 



rates_long %>% spread(factor,rate)
rates_long %>% group_by(year,factor) %>% summarise(rate=sum(rate)) %>%
  ggplot(.,aes(x=year,y=rate))+geom_point()+facet_wrap(~factor)













         