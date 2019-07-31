library(tidyverse)
library(here)
library(readxl)
library(conflicted)
#library(DasGuptR)
conflict_prefer("filter","dplyr")
conflict_prefer("select","dplyr")
lapply(list.files("../DasGuptR/R",full.names = T),source)

#download.file("https://www.nrscotland.gov.uk/files//statistics/population-estimates/time-series/mid-18/mid-year-pop-est-18-time-series-1.xlsx",destfile = here("council_pops.xlsx"),mode = "wb")
excel_sheets(here("council_pops.xlsx"))
#so, 25:39

extract_pops<-function(sheeti){
  popdata<-read_xlsx(here("council_pops.xlsx"),sheet = sheeti)
  year<-excel_sheets(here("council_pops.xlsx"))[sheeti]
  
  mdata<-popdata[which(popdata$...2=="Males"):(which(popdata$...2=="Females")-1),]
  names(mdata)<-paste0(mdata[1,])
  mdata<-mdata[-1,]
  gather(mdata, age,num,4:94) %>% 
    rename(council=Males) %>%
    mutate(
      num=as.numeric(num),
      age=as.numeric(gsub("\\+","",age)),
      age_bin=.bincode(age,breaks=c(-1,21,25,30,40,100),T),
      Gender="Male"
    ) %>% group_by(age_bin,Gender,council) %>%
    summarise(
      num=sum(num)
    ) -> mdata
  fdata<-popdata[which(popdata$...2=="Females"):nrow(popdata),]
  names(fdata)<-paste0(fdata[1,])
  fdata<-fdata[-1,]
  gather(fdata, age,num,4:94) %>% 
    rename(council=Females) %>%
    mutate(
      num=as.numeric(num),
      age=as.numeric(gsub("\\+","",age)),
      age_bin=.bincode(age,breaks=c(-1,21,25,30,40,100),T),
      Gender="Female"
    ) %>% group_by(age_bin,Gender,council) %>%
    summarise(
      num=sum(num)
    ) -> fdata
  
  bind_rows(fdata,mdata) %>% mutate(year=year)
}
  
popests<-map_dfr(25:39,extract_pops) %>% na.omit()

popests %>% mutate(
  Age=factor(age_bin),
  Age=fct_recode(Age,"under 21"="1","21 to 25"="2","26 to 30"="3","31 to 40"="4","over 40"="5"),
  laa=fct_recode(factor(council),"Aberdeen City and Aberdeenshire"="Aberdeen City","Aberdeen City and Aberdeenshire"="Aberdeenshire",
                 "Argyll & Bute"="Argyll and Bute",
                 "Ayrshire, East, North, and South"="East Ayrshire",
                 "Ayrshire, East, North, and South"="South Ayrshire",
                 "Ayrshire, East, North, and South"="North Ayrshire",
                 "Dumfries & Galloway"="Dumfries and Galloway",
                 "Dunbartonshire, East and West"="East Dunbartonshire",
                 "Dunbartonshire, East and West"="West Dunbartonshire",
                 "Edinburgh and Midlothian"="City of Edinburgh",
                 "Edinburgh and Midlothian"="Midlothian",
                 "Lanarkshire, North and South"="South Lanarkshire",
                 "Lanarkshire, North and South"="North Lanarkshire",
                 "Perth & Kinross"="Perth and Kinross",
                 "Renfrewshire and East Renfrewshire"="Renfrewshire",
                 "Renfrewshire and East Renfrewshire"="East Renfrewshire")
) %>% filter(laa!="Scotland") %>%
  group_by(year,laa,Gender,Age) %>%
  summarise(
    num=sum(num)
  ) -> popests

popests %>% group_by(year,laa) %>% summarise(totalpop=sum(num)) %>%
  left_join(popests,.) %>% ungroup %>%
  mutate(
    year=as.numeric(year)
  ) -> popests

saveRDS(popests,"data/LAA_popstructure.RDS")
