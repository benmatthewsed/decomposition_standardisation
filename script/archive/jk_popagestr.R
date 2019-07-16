download.file("https://www.nrscotland.gov.uk/files//statistics/population-projections/2016-based-scot/pop-pyramid-16-based.xlsm",destfile = here("pop_ests.xlsm"),mode = "wb")
mpopdata<-read_xlsx(here("pop_ests.xlsm"),sheet = 4)[-1,]
names(mpopdata)<-paste0("age",mpopdata[1,])
fpopdata<-read_xlsx(here("pop_ests.xlsm"),sheet = 5)[-1,]
names(fpopdata)<-paste0("age",fpopdata[1,])
mpopdata<-mpopdata[-1,-c(93,94)]
fpopdata<-fpopdata[-1,-c(93,94)]

names(mpopdata)
mpopdata %>% mutate(ageMales=as.numeric(ageMales)) %>% 
  rename(year=ageMales) %>%
  filter(year>=2004, year<=2018) %>%
  gather(age,num,2:92) %>% mutate(
    age=as.numeric(gsub("age|\\+","",age)),
    age_bin=.bincode(age,breaks=c(-1,21,25,30,40,100),T),
    num=as.numeric(num),
    Gender="Male"
  ) %>% group_by(age_bin,year,Gender) %>%
  summarise(
    num=sum(num)
  ) %>%
  bind_rows(.,
            fpopdata %>% mutate(ageFemales=as.numeric(ageFemales)) %>% 
              rename(year=ageFemales) %>%
              filter(year>=2004, year<=2018) %>%
              gather(age,num,2:92) %>% mutate(
                age=as.numeric(gsub("age|\\+","",age)),
                age_bin=.bincode(age,breaks=c(-1,21,25,30,40,100),T),
                num=as.numeric(num),
                Gender="Female"
                )  %>% group_by(age_bin,year,Gender) %>%
              summarise(
                num=sum(num)
              )
  ) -> longpop

longpop %>% group_by(year) %>% summarise(totalpop=sum(num)) %>%
  left_join(longpop,.) %>%
  mutate(
    age_str=num/totalpop
  ) %>% 
  mutate(
    Age=factor(age_bin),
    Age=fct_recode(Age,"under 21"="1","21 to 25"="2","26 to 30"="3","31 to 40"="4","over 40"="5")
  ) %>% ungroup %>% select(Age,age_str,year,Gender) -> popdata

rm(longpop,mpopdata,fpopdata)
