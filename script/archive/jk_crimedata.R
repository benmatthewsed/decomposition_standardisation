#download.file("http://reshare.ukdataservice.ac.uk/852854/2/RegionalCrimeTrends_ScotlandLAs_EnglandWalesCSPs_2004_2016.xlsx",              destfile = here("regional_crimetrends.xlsx"),mode = "wb")
excel_sheets(here("regional_crimetrends.xlsx"))
cdata<-read_xlsx(here("regional_crimetrends.xlsx"),sheet = 11,col_names=F)
names(cdata)<-as.character(cdata[3,])
cdata<-cdata[-c(1:3),]

cdata %>% mutate(
  laa=fct_recode(factor(LAName),"Aberdeen City and Aberdeenshire"="Aberdeen City","Aberdeen City and Aberdeenshire"="Aberdeenshire",
                 "Argyll & Bute"="Argyll and Bute",
                 "Ayrshire, East, North, and South"="East Ayrshire",
                 "Ayrshire, East, North, and South"="South Ayrshire",
                 "Ayrshire, East, North, and South"="North Ayrshire",
                 "Dumfries & Galloway"="Dumfries and Galloway",
                 "Dunbartonshire, East and West"="East Dunbartonshire",
                 "Dunbartonshire, East and West"="West Dunbartonshire",
                 "Edinburgh and Midlothian"="Edinburgh, City of",
                 "Edinburgh and Midlothian"="Midlothian",
                 "Lanarkshire, North and South"="South Lanarkshire",
                 "Lanarkshire, North and South"="North Lanarkshire",
                 "Perth & Kinross"="Perth and Kinross",
                 "Renfrewshire and East Renfrewshire"="Renfrewshire",
                 "Renfrewshire and East Renfrewshire"="East Renfrewshire",
                 "Na h- Eilean Siar"="Na h- Eilean Siar / Comhairle nan Eilean Siar")
) %>% 
  group_by(laa) %>% summarise_at(vars(starts_with("VIO"),starts_with("MYP")),~sum(as.numeric(.))) -> cdata_new
  
cdata_new %>% select(laa,starts_with("VIO")) %>%
  gather(year,vcrime,starts_with("VIO")) %>%
  mutate(
    year=as.numeric(substr(year,nchar(year)-3,nchar(year)-2))+2000
  ) %>%
  left_join(.,
            cdata_new %>% select(laa,starts_with("MYP")) %>%
              gather(year,myp,starts_with("MYP")) %>%
              mutate(
                year=as.numeric(gsub("MYP","",year))
              )
  ) %>% 
  mutate(
    vcrimerate=vcrime/myp
  ) %>%
  select(laa,year,vcrimerate) %>% spread(year,vcrimerate) %>%
saveRDS(.,"regionalcrime.RDS")
