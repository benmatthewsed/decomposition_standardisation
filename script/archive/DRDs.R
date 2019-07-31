require(tidyverse)
require(matrixStats)
library(conflicted)
require(combinat)
require(here)
conflict_prefer("filter","dplyr")
conflict_prefer("select","dplyr")

download.file("https://www.nrscotland.gov.uk/files//statistics/drug-related-deaths/drd2016/drug-related-deaths-16-tab4.csv",              destfile = here("DRD_agesex.csv"),mode = "wb")

sheet<-read_csv(here("DRD_agesex.csv")) 
sheet<-sheet[34:54,]
names(sheet)<-as.character(sheet[2,])
sheet<-sheet[-c(2:4),]
sheet<-sheet[,-which(names(sheet)=="NA")]
men<-sheet[2:18,1:7]
women<-sheet[2:18,c(1,9:14)]
names(men)[1]<-"year"
names(women)[1]<-"year"

men %>% mutate(
  Gender="m"
) %>% select(-`All ages`) %>%
  gather(Age,num_deaths,2:6) -> men

women %>% mutate(
  Gender="f"
) %>% select(-`All ages`) %>%
  gather(Age,num_deaths,2:6) -> women


bind_rows(men,women) %>%
  mutate(
    year=as.numeric(year),
    num_deaths=as.numeric(num_deaths)
  ) -> drds




popage<-readRDS("drd_popstr.RDS")

popage %>% select(year,Gender,num,totalpop,age_str,Age) %>%
  mutate(
    Gender=fct_recode(factor(Gender),"m"="Male","f"="Female"),
    Age=fct_recode(factor(Age),"24 and under"="under 24","25 - 34"="25 to 34","35 - 44"="35 to 44","45 - 54"="45 to 54","55 and over"="over 55")
  ) %>% left_join(drds,.) -> drds

drds %>% mutate(
  prevdeath=num_deaths/num
) -> drds

require(DasGuptR)

dg_decompose <- 
  drds %>% 
  na.omit() %>% 
  DasGupt_Npop(.,pop=year,prevdeath,age_str,id_vars=c(Age,Gender))


drds %>% group_by(year,Age,Gender) %>%
  summarise_at(vars(starts_with("num")),sum) %>%
  mutate(prevdeath=10000*num_deaths/num) %>%
  ggplot(.,aes(x=year,y=prevdeath,group=interaction(Age,Gender),col=Age))+
  geom_line()+
  facet_grid(~Gender)+
  NULL
  
  


drds %>% 
  mutate(rate = prevdeath * age_str) %>%
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
                         "Adjusted for prevalence"="age_str",
                         "Adjusted for age/sex mix"="prevdeath"
    ),
    rate_type=fct_relevel(rate_type,"Crude"),
    rate=rate*10000
  ) %>%
  ggplot(.,aes(x=year,y=rate,col=rate_type))+
  geom_path()+
  scale_color_manual(values = c("black","#1b9e77","#d95f02"))+
  theme_bw()+
  labs(y = "DRDs per 10000",
       colour = "Rate")+
  theme(legend.position="bottom",axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(colour = guide_legend(nrow = 3, byrow = TRUE)) +
  NULL



require(gganimate)
require(lubridate)
readRDS("pop_agesexmix_agenum.RDS") %>% na.omit %>% filter(year<2019) %>%
  mutate(
    year=ymd(paste0(year,"-01-01"))
  ) %>% 
  ggplot(.,aes(x=age,y=ifelse(Gender=="Male",num,-num),fill=Gender))+geom_bar(stat="identity")+coord_flip()+
  theme_bw()+
  transition_time(year)+
  labs(title = "Year: {frame_time}") -> p

  anim <- animate(p)
  anim_save("scotland_poppyramid.gif", anim)
