library(tidyverse)
library(matrixStats)
library(conflicted)
library(combinat)
conflict_prefer("filter", "dplyr")

#devtools::install_github("josiahpjking/DasGuptR")
library(DasGuptR)

#e.g. taken from Ben's code.
readRDS(here("data","testdf_long.RDS")) -> testdf
readRDS(here("data","BMresult.RDS")) -> bm
readRDS("data/testdf_3years.RDS") -> testdf3

dg2pops <- DasGupt_2pop(testdf,year,c("prev","age_str","freq","disposal_prop","crime_type_prop"))
dgtimeseries <- DasGupt_Npop(testdf3,pop=year,prev,age_str,freq,disposal_prop,crime_type_prop)

#get the prev factor diffs
dgtimeseries$prev$factor_effects
#or standardised rates
dgtimeseries$prev$standardised_rates