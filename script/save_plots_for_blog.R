library(tidyverse)
library(conflicted)
library(gghighlight)
library(kableExtra)

require(DasGuptR)
# make sure devtools::install_github("josiahpjking/DasGuptR",force=TRUE)
conflict_prefer("filter","dplyr")
conflict_prefer("select","dplyr")


# plot 1 ------------------------------------------------------------------

rdata <- readRDS("recdata.RDS")

reconv_plot <- 
rdata %>%
  group_by(year, Gender, Age) %>%
  summarise_at(vars(contains("num")), sum) %>%
  mutate(
    rate = (num_reconvicted / num_offenders) * 100,
    sg_freq = num_reconvictions / num_offenders,
    Age = fct_relevel(Age, "under 21")
  ) %>%
  ggplot(., aes(x = year, y = rate, col = Gender)) +
  geom_path() + 
  facet_grid(~Age) +
  labs(y = "Reconviction Rate",
       x = "Year")+
  #ylab("Average number of reconvictions per offender") +
  #ggtitle("SG reconviction rate") +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  NULL

ggsave(
  here::here("figures", "reconv_rate_age.png"),
  reconv_plot,
  type = "cairo-png",
  height = 6,
  width = 10
)

ggsave(
  here::here("figures", "reconv_rate_age.svg"),
  reconv_plot,
  height = 6,
  width = 10
)


rdata %>%
  group_by(year, Gender, Age) %>%
  summarise_at(vars(contains("num")), sum) %>%
  left_join(., 
            rdata %>% 
              group_by(year) %>% 
              summarise(total = sum(num_offenders))) %>%
  mutate(
    age_str = num_offenders / total, # this is the age structure of convicted population
    prev = num_reconvicted / num_offenders, # this is the prevalence of reconviction
    freq = num_reconvictions / num_reconvicted ## this is the frequency or reconviction (but not SG's ARpO)
  ) %>% 
  ungroup() -> rdata

rdata <- 
  rdata %>% 
  mutate(reconvs = prev)

dg_decompose <- 
  rdata %>% 
  na.omit() %>% 
  DasGupt_Npop(.,pop=year,reconvs,age_str,id_vars=c(Age,Gender))

std_plot <- 
rdata %>% 
  mutate(rate = reconvs * age_str) %>%
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
                         "Adjusted for reconviction prevalence"="age_str",
                         "Adjusted for offender mix"="reconvs"
    ),
    rate_type=fct_relevel(rate_type,"Crude"),
    rate=rate*100
  ) %>%
  ggplot(.,aes(x=year,y=rate,col=rate_type))+
  geom_path()+
  scale_color_manual(values = c("black","#1b9e77","#d95f02"))+
  theme_bw()+
  labs(y = "Reconviction Rate",
       colour = "Rate",
       x = "Year")+
  theme(legend.position="bottom",axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(colour = guide_legend(nrow = 3, byrow = TRUE)) +
  scale_x_continuous(breaks=2004:2016,labels=2004:2016)+
  geom_label(aes(x=2011.75,y=31,label="If reconviction rates for each group stayed\nthe same, the changing 'offender mix'\nwould mean the overall rate would still decrease"),col="#555555",size=3,label.size=NA,hjust=0,vjust=.5)+
  geom_curve(aes(x=2011.5,y=31,xend=2012,yend=29.7),col="#555555",curvature=.2,arrow=arrow(length=unit(.03,"npc")))+
  NULL

ggsave(
  here::here("figures", "std_rate.png"),
  std_plot,
  type = "cairo-png",
  height = 6,
  width = 10
)

ggsave(
  here::here("figures", "std_rate.svg"),
  std_plot,
  height = 6,
  width = 10
)
