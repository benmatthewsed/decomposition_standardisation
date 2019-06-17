# Decomposition example

library(here)
library(tidyverse)


# the below is based on equations in
# https://www.census.gov/content/dam/Census/library/publications/1993/demo/p23-186.pdf


das_gupta_eg <- tribble(~measure, ~black_men, ~white_men,
        "total earnings/total population", 7846.56, 13703.73,
        "total earnings/people earned", 10930, 16591,
        "people earned/total population", 0.717892, 0.825974)


# pop1 = black men
# pop2 = white men
# fct 1 = a = mean earnings
# fct 2 = b = proportion earnings

# R = mean earnings based on all earnings


a <- pop_1_fct_1 <- 
  das_gupta_eg %>% 
  filter(str_detect(measure, "total earnings/people earned")) %>% 
  select(black_men) %>% .[[1]]

b <- pop_1_fct_2 <- 
  das_gupta_eg %>% 
  filter(str_detect(measure, "people earned/total population")) %>% 
  select(black_men) %>% .[[1]]
  
  
A <- pop_2_fct_1 <- 
  das_gupta_eg %>% 
  filter(str_detect(measure, "total earnings/people earned")) %>% 
  select(white_men) %>% .[[1]]

B <- pop_2_fct_2 <- 
  das_gupta_eg %>% 
  filter(str_detect(measure, "people earned/total population")) %>% 
  select(white_men) %>% .[[1]]

b_std_rate_pop_1 <- (b + B)/2 * A
b_std_rate_pop_2 <- (b + B)/2 * a

a_std_rate_pop_1 <- (a + A)/2 * B
a_std_rate_pop_2 <- (a + A)/2 * b

a_effect <- (b + B) / 2 * (a - A)
b_effect <- (a + A) / 2 * (b - B)


# this works!



# decomposition with three factors ----------------------------------------

# Austria is pop 1
# Chile is pop 2

das_gupta_eg_3_tidy <- tribble(~country, ~fert_rate, ~childbearing_woman_rate, ~women_rate,
        "Austria", 51.76748, 0.45919, 0.52638,
        "Chile", 84.90502, 0.75756, 0.51065)

A <- das_gupta_eg_3_tidy %>% 
  filter(country == "Austria") %>% 
  select(fert_rate) %>% .[[1]]

B <- das_gupta_eg_3_tidy %>% 
  filter(country == "Austria") %>% 
  select(childbearing_woman_rate) %>% .[[1]]


C <- das_gupta_eg_3_tidy %>% 
  filter(country == "Austria") %>% 
  select(women_rate) %>% .[[1]]

a <- das_gupta_eg_3_tidy %>% 
  filter(country == "Chile") %>% 
  select(fert_rate) %>% .[[1]]

b <- das_gupta_eg_3_tidy %>% 
  filter(country == "Chile") %>% 
  select(childbearing_woman_rate) %>% .[[1]]

c <- das_gupta_eg_3_tidy %>% 
  filter(country == "Chile") %>% 
  select(women_rate) %>% .[[1]]


bc_std_rate_pop_1 <- 
  (((b*c + B*C) / 3 ) + ((b*C + B*c) / 6)) * A

bc_std_rate_pop_2 <- 
  (((b*c + B*C) / 3 ) + ((b*C + B*c) / 6)) * a

ac_std_rate_pop_1 <- 
  (((a*c + A*C) / 3 ) + ((a*C + A*c) / 6)) * B

ac_std_rate_pop_2 <- 
  (((a*c + A*C) / 3 ) + ((a*C + A*c) / 6)) * b

ab_std_rate_pop_1 <- 
  (((a*b + A*B) / 3 ) + ((a*B + A*b) / 6)) * C

ab_std_rate_pop_2 <- 
  (((a*b + A*B) / 3 ) + ((a*B + A*b) / 6)) * c

a_effect <- 
  (((b*c + B*C) / 3 ) + ((b*C + B*c) / 6)) * (a - A)

c_effect <- 
  (((a*b + A*B) / 3 ) + ((a*B + A*b) / 6)) * (c - C)




# three vector factors  -----------------------------------------------

# this is basically the same as above because R is already vectorized.
# convenient!

decomp_eg <- read_csv(here::here("data", "raw", "decomposition_eg_ew.csv"))


dg_stdize <-
  function(df, year_one, year_two) {

    # subsetting data for calculations
    A <- df %>%
      filter(year == year_one) %>%
      select(prev)

    a <- df %>%
      filter(year == year_two) %>%
      select(prev)

    B <- df %>%
      filter(year == year_one) %>%
      select(age_str)

    b <- df %>%
      filter(year == year_two) %>%
      select(age_str)

    C <- df %>%
      filter(year == year_one) %>%
      select(freq)

    c <- df %>%
      filter(year == year_two) %>%
      select(freq)

    # calculate sums for convenience later
    # could I do this with crossing or similar?
    ABC <- A * B * C
    abc <- a * b * c
    Abc <- A * b * c
    AbC <- A * b * C
    aBc <- a * B * c
    aBC <- a * B * C
    ABc <- A * B * c
    abC <- a * b * C

    
    # calculate prevalence effect
    QA <- ((Abc + ABC) / 3) + ((AbC + ABc) / 6)
    
    Qa <- ((abc + aBC) / 3) + ((abC + aBc) / 6)

    prev_effect <- Qa - QA

    # rename variables for convenience
    prev_effect <- rename(prev_effect, prev_effect = prev)
    QA <- rename(QA, QA = prev)
    Qa <- rename(Qa, Qa = prev)

    
    # calculate age effect
    QB <- ((aBc + ABC) / 3) + ((aBC + ABc) / 6)
    Qb <- ((abc + AbC) / 3) + ((abC + Abc) / 6)

    age_effect <- Qb - QB

    age_effect <- rename(age_effect, age_effect = prev)
    QB <- rename(QB, QB = prev)
    Qb <- rename(Qb, Qb = prev)
    
    # calculate frequency effect
    QC <- ((abC + ABC) / 3) + ((aBC + AbC) / 6)
    Qc <- ((abc + ABc) / 3) + ((aBc + Abc) / 6)

    freq_effect <- Qc - QC

    freq_effect <- rename(freq_effect, freq_effect = prev)
    QC <- rename(QC, QC = prev)
    Qc <- rename(Qc, Qc = prev)
    
    # combine results
    age <-
      df %>%
      filter(year == year_one) %>%
      select(age)

    result <- bind_cols(age, 
                        prev_effect, 
                        age_effect, 
                        freq_effect)

    return(result)
  }

dg_stdize(decomp_eg, 1989, 2011) %>% 
  gather(effect, value, 2:4) %>% 
  ggplot(aes(x = age, y = value, colour = effect)) +
  geom_line()

# it works!

# now with another dataset

decomp_eg_men <- read.csv(here::here("data", "raw", "decomposition_eg_men_ew.csv"))


men <- dg_stdize(decomp_eg_men, 1989, 2011)

# it still works! 

# totals

men %>% 
  summarise(age_effect = sum(age_effect),
            prev_effect = sum(prev_effect),
            freq_effect = sum(freq_effect)) %>% 
  gather(effect, value) %>% 
  mutate(percent = value / sum(value) * 100)

men %>% 
  ggplot(aes(x = age)) +
  geom_line(aes(y = prev_effect)) +
  geom_line(aes(y = age_effect)) +
  geom_line(aes(y = freq_effect)) +
  geom_hline(yintercept = 0)





# decomposition with five factors -----------------------------------------

# make data

# population 1 = 1970
# population 2 = 1960

# this based on DG example 2.4

dg_5_eg <-
  tribble(~year, ~fert_rate, ~p_married, ~noncontraception, ~abortion, ~lact_infec, ~fecund,
        1970, 4.05, 0.58, 0.76, 0.84, 0.66, 16.573,
        1960, 6.13, 0.72, 0.97, 0.97, 0.56, 16.158)


# subsetting data for calculations


A <- dg_5_eg %>%
  filter(year == 1970) %>%
  select(p_married)

a <- dg_5_eg %>%
  filter(year == 1960) %>%
  select(p_married)

B <- dg_5_eg %>%
  filter(year == 1970) %>%
  select(noncontraception)

b <- dg_5_eg %>%
  filter(year == 1960) %>%
  select(noncontraception)

C <- dg_5_eg %>%
  filter(year == 1970) %>%
  select(abortion)

c <- dg_5_eg %>%
  filter(year == 1960) %>%
  select(abortion)

D <- dg_5_eg %>%
  filter(year == 1970) %>%
  select(lact_infec)

d <- dg_5_eg %>%
  filter(year == 1960) %>%
  select(lact_infec)

E <- dg_5_eg %>%
  filter(year == 1970) %>%
  select(fecund)

e <- dg_5_eg %>%
  filter(year == 1960) %>%
  select(fecund)


Q1 <- 
(((b * c * d * e) +
  (B * C * D * E)) /
  5) +
  (((b * c * d * E) +
    (b * c * D * e) +
    (b * C * d * e) +
    (B * c * d * e) +
    (B * C * D * e) +
    (B * C * d * E) +
    (B * c * D * E) +
    (b * C * D * E)) /
    20) +
  (((b * c * D * E) +
    (b * C * d * E) +
    (b * C * D * e) +
    (B * C * d * e) +
    (B * c * D * e) +
    (B * c * d * E)) /
    30)

QA <- Q1 * A
Qa <- Q1 * a

Q2 <- 
  (((a * c * d * e) +
      (A * C * D * E)) /
     5) +
  (((a * c * d * E) +
      (a * c * D * e) +
      (a * C * d * e) +
      (A * c * d * e) +
      (A * C * D * e) +
      (A * C * d * E) +
      (A * c * D * E) +
      (a * C * D * E)) /
     20) +
  (((a * c * D * E) +
      (a * C * d * E) +
      (a * C * D * e) +
      (A * C * d * e) +
      (A * c * D * e) +
      (A * c * d * E)) /
     30)

QB <- Q2 * B
Qb <- Q2 * b


Q3 <- 
  (((a * b * d * e) +
      (A * B * D * E)) /
     5) +
  (((a * b * d * E) +
      (a * b * D * e) +
      (a * B * d * e) +
      (A * b * d * e) +
      (A * B * D * e) +
      (A * B * d * E) +
      (A * b * D * E) +
      (a * B * D * E)) /
     20) +
  (((a * b * D * E) +
      (a * B * d * E) +
      (a * B * D * e) +
      (A * B * d * e) +
      (A * b * D * e) +
      (A * b * d * E)) /
     30)

QC <- Q3 * C
Qc <- Q3 * c


Q4 <- 
  (((a * b * c * e) +
      (A * B * C * E)) /
     5) +
  (((a * b * c * E) +
      (a * b * C * e) +
      (a * B * c * e) +
      (A * b * c * e) +
      (A * B * C * e) +
      (A * B * c * E) +
      (A * b * C * E) +
      (a * B * C * E)) /
     20) +
  (((a * b * C * E) +
      (a * B * c * E) +
      (a * B * C * e) +
      (A * B * c * e) +
      (A * b * C * e) +
      (A * b * c * E)) /
     30)

QD <- Q4 * D
Qd <- Q4 * d


Q5 <- 
  (((a * b * c * d) +
      (A * B * C * D)) /
     5) +
  (((a * b * c * D) +
      (a * b * C * d) +
      (a * B * c * d) +
      (A * b * c * d) +
      (A * B * C * d) +
      (A * B * c * D) +
      (A * b * C * D) +
      (a * B * C * D)) /
     20) +
  (((a * b * C * D) +
      (a * B * c * D) +
      (a * B * C * d) +
      (A * B * c * d) +
      (A * b * C * d) +
      (A * b * c * D)) /
     30)

QE <- Q5 * E
Qe <- Q5 * e

qa_effect <- Qa - QA

dg_five <- function(df, year_one, year_two) {
  A <- df %>%
    filter(year == year_one) %>%
    select(prev)
  
  a <- df %>%
    filter(year == year_two) %>%
    select(prev)
  
  B <- df %>%
    filter(year == year_one) %>%
    select(age_str)
  
  b <- df %>%
    filter(year == year_two) %>%
    select(age_str)
  
  C <- df %>%
    filter(year == year_one) %>%
    select(freq)
  
  c <- df %>%
    filter(year == year_two) %>%
    select(freq)
  
  D <- df %>%
    filter(year == year_one) %>%
    select(disposal_prop)
  
  d <- df %>%
    filter(year == year_two) %>%
    select(disposal_prop)
  
  E <- df %>%
    filter(year == year_one) %>%
    select(crime_type_prop)
  
  e <- df %>%
    filter(year == year_two) %>%
    select(crime_type_prop)
  
  
  Q1 <-
    (((b * c * d * e) +
        (B * C * D * E)) /
       5) +
    (((b * c * d * E) +
        (b * c * D * e) +
        (b * C * d * e) +
        (B * c * d * e) +
        (B * C * D * e) +
        (B * C * d * E) +
        (B * c * D * E) +
        (b * C * D * E)) /
       20) +
    (((b * c * D * E) +
        (b * C * d * E) +
        (b * C * D * e) +
        (B * C * d * e) +
        (B * c * D * e) +
        (B * c * d * E)) /
       30)
  
  QA <- Q1 * A
  Qa <- Q1 * a
  
  prev_effect <- Qa - QA
  prev_effect <- rename(prev_effect, prev_effect = age_str)
  
  Q2 <-
    (((a * c * d * e) +
        (A * C * D * E)) /
       5) +
    (((a * c * d * E) +
        (a * c * D * e) +
        (a * C * d * e) +
        (A * c * d * e) +
        (A * C * D * e) +
        (A * C * d * E) +
        (A * c * D * E) +
        (a * C * D * E)) /
       20) +
    (((a * c * D * E) +
        (a * C * d * E) +
        (a * C * D * e) +
        (A * C * d * e) +
        (A * c * D * e) +
        (A * c * d * E)) /
       30)
  
  QB <- Q2 * B
  Qb <- Q2 * b
  
  age_str_effect <- Qb - QB
  age_str_effect <- rename(age_str_effect, age_str_effect = prev)
  
  Q3 <-
    (((a * b * d * e) +
        (A * B * D * E)) /
       5) +
    (((a * b * d * E) +
        (a * b * D * e) +
        (a * B * d * e) +
        (A * b * d * e) +
        (A * B * D * e) +
        (A * B * d * E) +
        (A * b * D * E) +
        (a * B * D * E)) /
       20) +
    (((a * b * D * E) +
        (a * B * d * E) +
        (a * B * D * e) +
        (A * B * d * e) +
        (A * b * D * e) +
        (A * b * d * E)) /
       30)
  
  QC <- Q3 * C
  Qc <- Q3 * c
  
  freq_effect <- Qc - QC
  freq_effect  <- rename(freq_effect , freq_effect = prev)
  
  Q4 <-
    (((a * b * c * e) +
        (A * B * C * E)) /
       5) +
    (((a * b * c * E) +
        (a * b * C * e) +
        (a * B * c * e) +
        (A * b * c * e) +
        (A * B * C * e) +
        (A * B * c * E) +
        (A * b * C * E) +
        (a * B * C * E)) /
       20) +
    (((a * b * C * E) +
        (a * B * c * E) +
        (a * B * C * e) +
        (A * B * c * e) +
        (A * b * C * e) +
        (A * b * c * E)) /
       30)
  
  QD <- Q4 * D
  Qd <- Q4 * d
  
  disposal_prop_effect <- Qd - QD
  disposal_prop_effect  <- rename(disposal_prop_effect , disposal_prop_effect = prev) 
  
  Q5 <-
    (((a * b * c * d) +
        (A * B * C * D)) /
       5) +
    (((a * b * c * D) +
        (a * b * C * d) +
        (a * B * c * d) +
        (A * b * c * d) +
        (A * B * C * d) +
        (A * B * c * D) +
        (A * b * C * D) +
        (a * B * C * D)) /
       20) +
    (((a * b * C * D) +
        (a * B * c * D) +
        (a * B * C * d) +
        (A * B * c * d) +
        (A * b * C * d) +
        (A * b * c * D)) /
       30)
  
  QE <- Q5 * E
  Qe <- Q5 * e
  
  crime_type_prop_effect <- Qe - QE
  crime_type_prop_effect  <- rename(crime_type_prop_effect,
                                    crime_type_prop_effect = prev) 
  
  # combine results
  ref_vars <-
    df %>%
    filter(year == year_one) %>%
    select(age, crime_type, disposal)
  
  result <- bind_cols(
    ref_vars,
    prev_effect,
    age_str_effect,
    freq_effect,
    disposal_prop_effect,
    crime_type_prop_effect
  )
  
  return(as.tibble(result))
}


dg_five_nest <- function(year_one, year_two, df1, df2) {
  
  
  
  A <- df1 %>%
    select(prev)
  
  a <- df2 %>%
    select(prev)
  
  B <- df1 %>%
    select(age_str)
  
  b <- df2 %>% 
    select(age_str)
  
  C <- df1 %>%
    select(freq)
  
  c <- df2 %>% 
    select(freq)
  
  D <- df1 %>%
    select(disposal_prop)
  
  d <- df2 %>% 
    select(disposal_prop)
  
  E <- df1 %>%
    select(crime_type_prop)
  
  e <- df2 %>% 
    select(crime_type_prop)
  
  
  Q1 <-
    (((b * c * d * e) +
        (B * C * D * E)) /
       5) +
    (((b * c * d * E) +
        (b * c * D * e) +
        (b * C * d * e) +
        (B * c * d * e) +
        (B * C * D * e) +
        (B * C * d * E) +
        (B * c * D * E) +
        (b * C * D * E)) /
       20) +
    (((b * c * D * E) +
        (b * C * d * E) +
        (b * C * D * e) +
        (B * C * d * e) +
        (B * c * D * e) +
        (B * c * d * E)) /
       30)
  
  QA <- Q1 * A
  Qa <- Q1 * a
  
  prev_effect <- Qa - QA
  prev_effect <- rename(prev_effect, prev_effect = age_str)
  
  Q2 <-
    (((a * c * d * e) +
        (A * C * D * E)) /
       5) +
    (((a * c * d * E) +
        (a * c * D * e) +
        (a * C * d * e) +
        (A * c * d * e) +
        (A * C * D * e) +
        (A * C * d * E) +
        (A * c * D * E) +
        (a * C * D * E)) /
       20) +
    (((a * c * D * E) +
        (a * C * d * E) +
        (a * C * D * e) +
        (A * C * d * e) +
        (A * c * D * e) +
        (A * c * d * E)) /
       30)
  
  QB <- Q2 * B
  Qb <- Q2 * b
  
  age_str_effect <- Qb - QB
  age_str_effect <- rename(age_str_effect, age_str_effect = prev)
  
  Q3 <-
    (((a * b * d * e) +
        (A * B * D * E)) /
       5) +
    (((a * b * d * E) +
        (a * b * D * e) +
        (a * B * d * e) +
        (A * b * d * e) +
        (A * B * D * e) +
        (A * B * d * E) +
        (A * b * D * E) +
        (a * B * D * E)) /
       20) +
    (((a * b * D * E) +
        (a * B * d * E) +
        (a * B * D * e) +
        (A * B * d * e) +
        (A * b * D * e) +
        (A * b * d * E)) /
       30)
  
  QC <- Q3 * C
  Qc <- Q3 * c
  
  freq_effect <- Qc - QC
  freq_effect  <- rename(freq_effect, freq_effect = prev)
  
  Q4 <-
    (((a * b * c * e) +
        (A * B * C * E)) /
       5) +
    (((a * b * c * E) +
        (a * b * C * e) +
        (a * B * c * e) +
        (A * b * c * e) +
        (A * B * C * e) +
        (A * B * c * E) +
        (A * b * C * E) +
        (a * B * C * E)) /
       20) +
    (((a * b * C * E) +
        (a * B * c * E) +
        (a * B * C * e) +
        (A * B * c * e) +
        (A * b * C * e) +
        (A * b * c * E)) /
       30)
  
  QD <- Q4 * D
  Qd <- Q4 * d
  
  disposal_prop_effect <- Qd - QD
  disposal_prop_effect  <- rename(disposal_prop_effect, disposal_prop_effect = prev) 
  
  Q5 <-
    (((a * b * c * d) +
        (A * B * C * D)) /
       5) +
    (((a * b * c * D) +
        (a * b * C * d) +
        (a * B * c * d) +
        (A * b * c * d) +
        (A * B * C * d) +
        (A * B * c * D) +
        (A * b * C * D) +
        (a * B * C * D)) /
       20) +
    (((a * b * C * D) +
        (a * B * c * D) +
        (a * B * C * d) +
        (A * B * c * d) +
        (A * b * C * d) +
        (A * b * c * D)) /
       30)
  
  QE <- Q5 * E
  Qe <- Q5 * e
  
  crime_type_prop_effect <- Qe - QE
  crime_type_prop_effect  <- rename(crime_type_prop_effect,
                                    crime_type_prop_effect = prev) 
  
  # combine results
  ref_vars <-
    df1 %>%
    select(age, crime_type, disposal)
  
  result <- bind_cols(
    ref_vars,
    prev_effect,
    age_str_effect,
    freq_effect,
    disposal_prop_effect,
    crime_type_prop_effect
  )
  
  result <- 
  result %>% 
    mutate(year_1 = year_one,
           year_2 = year_two)
  
  return(as.tibble(result))
}


# creating example data for five vector factor ----------------------------
set.seed(123)


cross_classified_eg <- 
decomp_eg_men %>% 
  mutate(prop_prison = rnorm(n(), mean = 0.25, sd = 0.05),
         prop_community = rnorm(n(), mean = 0.25, sd = 0.05),
         prop_fine = rnorm(n(), mean = 0.25, sd = 0.05),
         prop_other_disp = rnorm(n(), mean = 0.25, sd = 0.05)) %>% 
  mutate(prop_prison_st = prop_prison / (prop_prison + 
                                        prop_community +
                                        prop_fine +
                                        prop_other_disp),
         prop_community_st = prop_community / (prop_prison + 
                                              prop_community +
                                              prop_fine +
                                              prop_other_disp),
         prop_fine_st = prop_fine / (prop_prison + 
                                              prop_community +
                                              prop_fine +
                                              prop_other_disp),
         prop_other_disp_st = prop_other_disp / (prop_prison + 
                                              prop_community +
                                              prop_fine +
                                              prop_other_disp)) %>% 
  select(1, 3:6, 11:14) %>% 
  gather("disposal", "disposal_prop", 6:9) %>% 
  mutate(prop_vio = rnorm(n(), mean = 0.25, sd = 0.05),
         prop_drugs = rnorm(n(), mean = 0.25, sd = 0.05),
         prop_theft = rnorm(n(), mean = 0.25, sd = 0.05),
         prop_other_type = rnorm(n(), mean = 0.25, sd = 0.05)) %>% 
  mutate(prop_vio_st = prop_vio / (prop_vio + 
                                           prop_drugs +
                                           prop_theft +
                                           prop_other_type),
         prop_drugs_st = prop_drugs / (prop_vio + 
                                                 prop_drugs +
                                                 prop_theft +
                                                 prop_other_type),
         prop_theft_st = prop_theft / (prop_vio + 
                                       prop_drugs +
                                       prop_theft +
                                       prop_other_type),
         prop_other_type_st = prop_other_type / (prop_vio + 
                                                   prop_drugs +
                                                   prop_theft +
                                                   prop_other_type)) %>% 
  select(1:7, 12:15) %>% 
  gather("crime_type", "crime_type_prop", 8:11)


cross_classified_eg <- 
cross_classified_eg %>% 
  select(year, age, crime_type, disposal, everything(), freq) %>% 
  arrange(year, age, crime_type, disposal)


# seems to work okay, but need to validate

tmp1 <-  cross_classified_eg %>%
       dg_five(., 1989, 2011) %>%
       group_by(age) %>%
       summarise(
           prev_effect = sum(prev_effect),
           age_effect = sum(age_str_effect),
           freq_effect = sum(freq_effect)
        )


tmp2 <- dg_stdize(decomp_eg_men, 1989, 2011)

tmp1 <- tmp1 %>% 
  mutate(source = "five_fct")

tmp2 <- tmp2 %>% 
  mutate(source = "three_fct")

bind_rows(tmp1, tmp2) %>% 
  group_by(age) %>% 
  summarise(prev_diff = prev_effect[source == "five_fct"] - prev_effect[source == "three_fct"],
            age_diff = age_effect[source == "five_fct"] - age_effect[source == "three_fct"],
            freq_diff = freq_effect[source == "five_fct"] - freq_effect[source == "three_fct"]) %>% 
  arrange(desc(prev_diff))

# they're off by rounding error, so it's probably fine!


# calcualting summary of effects ------------------------------------------

# raw rate difference

raw_rate_diff <- 
decomp_eg_men %>% 
  mutate(raw_rate = prev * age_str * freq) %>% 
  group_by(year) %>% 
  summarise(raw_rate = sum(raw_rate) * 10000) %>% 
  ungroup() %>% 
  summarise(raw_rate_diff = raw_rate[year == 2011] - raw_rate[year == 1989]) %>% 
  .[[1]]

# sum of decomposition effects

decomp_diff <- 
dg_stdize(decomp_eg_men, 1989, 2011) %>% 
  summarise(age_effect = sum(age_effect),
            prev_effect = sum(prev_effect),
            freq_effect = sum(freq_effect)) %>% 
  gather(effect, value) %>% 
  mutate(value = value * 10000) %>% 
  summarise(decomp_diff = sum(value)) %>% 
  .[[1]]


# they're off by rounding error - so this is probably fine!

raw_rate_diff - decomp_diff
    
# testing with five factor decomposition

# this gives effect for each disposal
dg_five(cross_classified_eg, 1989, 2011) %>% 
  group_by(disposal) %>% 
  summarise(disposal_effect = sum(disposal_prop_effect))

# need to find right part of das gupta to check example
# calculate effects from five factor example

sum_10000 <- function(x){
  result <- sum(x) * 10000
  return(result)
}

dg_five(cross_classified_eg, 1989, 2011) %>% 
  summarise_at(4:8, sum_10000)






# constructing time-series dataset ----------------------------------------

# make list of years
year_list <- tibble(year = seq(1989, 2015))

year_list <- 
year_list %>% 
  mutate(year2 = list(tibble(year2 = seq(1989, 2015)))) %>% 
  unnest()

nest_data <- 
cross_classified_eg %>% 
  group_by(year) %>% 
  nest()

# try with test data
test_dat <- 
left_join(year_list, nest_data, by = "year")  %>%
  left_join(., nest_data, by = c("year2" = "year")) %>% 
  filter(year == 1989, year2 == 2011)

# apply function
test_dat %>% 
  mutate(stdized = pmap(list(year_one = year,
                             year_two = year2,
                             df1 = data.x,
                             df2 = data.y),
                        dg_five_nest)) %>% 
  select(stdized) %>% 
  unnest() %>% 
  summarise_at(4:8, sum_10000)

# it works!
all_data <- 
left_join(year_list, nest_data, by = "year")  %>%
  left_join(., nest_data, by = c("year2" = "year"))

all_data %>% 
  mutate(stdized = pmap(list(year_one = year,
                             year_two = year2,
                             df1 = data.x,
                             df2 = data.y),
                        dg_five_nest))

# this gives an error - not the right structure with all the NULLs?


# so next I want to calculate the right weighted average effects

tmp <- 
test_dat %>% 
  mutate(stdized = pmap(list(year_one = year,
                             year_two = year2,
                             df1 = data.x,
                             df2 = data.y),
                        dg_five_nest)) %>% 
  select(stdized) %>% unnest

tmp %>% 
  mutate(comp1 = year_1 - 1988,
         comp2 = year_2 - 1988) %>% 
  filter()


# time series decomposition -----------------------------------------------

decomp_men_2000_2007 <- read_csv(here::here("data", "raw", "decomp_men_2000_2007_ew.csv"))

decomp_men_2000_2007 %>% 
  group_by(year) %>% 
  nest()


# adding extra variables

cross_classified_2000_2007 <- 
decomp_men_2000_2007 %>% 
  mutate(prop_prison = rnorm(n(), mean = 0.25, sd = 0.05),
         prop_community = rnorm(n(), mean = 0.25, sd = 0.05),
         prop_fine = rnorm(n(), mean = 0.25, sd = 0.05),
         prop_other_disp = rnorm(n(), mean = 0.25, sd = 0.05)) %>% 
  mutate(prop_prison_st = prop_prison / (prop_prison + 
                                           prop_community +
                                           prop_fine +
                                           prop_other_disp),
         prop_community_st = prop_community / (prop_prison + 
                                                 prop_community +
                                                 prop_fine +
                                                 prop_other_disp),
         prop_fine_st = prop_fine / (prop_prison + 
                                       prop_community +
                                       prop_fine +
                                       prop_other_disp),
         prop_other_disp_st = prop_other_disp / (prop_prison + 
                                                   prop_community +
                                                   prop_fine +
                                                   prop_other_disp)) %>% 
  select(1, 3:6, 11:14) %>% 
  gather("disposal", "disposal_prop", 6:9) %>% 
  mutate(prop_vio = rnorm(n(), mean = 0.25, sd = 0.05),
         prop_drugs = rnorm(n(), mean = 0.25, sd = 0.05),
         prop_theft = rnorm(n(), mean = 0.25, sd = 0.05),
         prop_other_type = rnorm(n(), mean = 0.25, sd = 0.05)) %>% 
  mutate(prop_vio_st = prop_vio / (prop_vio + 
                                     prop_drugs +
                                     prop_theft +
                                     prop_other_type),
         prop_drugs_st = prop_drugs / (prop_vio + 
                                         prop_drugs +
                                         prop_theft +
                                         prop_other_type),
         prop_theft_st = prop_theft / (prop_vio + 
                                         prop_drugs +
                                         prop_theft +
                                         prop_other_type),
         prop_other_type_st = prop_other_type / (prop_vio + 
                                                   prop_drugs +
                                                   prop_theft +
                                                   prop_other_type)) %>% 
  select(1:7, 12:15) %>% 
  gather("crime_type", "crime_type_prop", 8:11)


# nesting

nest_2000_2007 <- 
cross_classified_2000_2007 %>% 
  group_by(year) %>% 
  nest()


# adding to previous nested dataset

nest_full <- bind_rows(nest_2000_2007, nest_data)

test_4 <- 
left_join(year_list, nest_full, by = "year") %>% 
  filter(year == 1989 |
           year == 2000 |
           year == 2007 |
           year == 2011) %>% 
  left_join(., nest_full, by = c("year2" = "year")) %>% 
  filter(year2 == 1989 |
           year2 == 2000 |
           year2 == 2007 |
           year2 == 2011)

test_4 %>% 
  mutate(stdized = pmap(list(year_one = year,
                             year_two = year2,
                             df1 = data.x,
                             df2 = data.y),
                        dg_five_nest)) %>% 
  select(stdized) %>% 
  unnest()

# looks like there's a problem in here somewhere...
# all the effects are coming out at zero

tmp <- 
tmp %>% 
  mutate(comp1 = year_1 - 1988,
         comp2 = year_2 - 1988)


# filter based on logic
# then do the maths
# then stitch together

comp1_1 <-   
tmp %>% 
  filter(comp1 == 1)


# I guess I'll have to do lots of summing here to
# get the right answers?

# eg group_by age will give the effect for each year of
# age? except prevalence for each age is multiplied a bunch of times
# likewise the age structure effect

comp1_tmp1 <- 
comp1_1 %>% 
  summarise(prev_effect = sum(prev_effect) / 2)

comp2_1 <-   
  tmp %>% 
  filter(comp2 == 1)

comp2_1 %>% 
  
  

  
# three populations -------------------------------------------------------

# we take the different factor effects and do pairwise comparisons
# for each pair of years
# and then take an average of the pairwise effects

a1.2 <- 2.870
a1.3 <- 2.866
a2.3 <- 3.133
a2.1 <- 2.871
a3.2 <- 3.141
a3.1 <- 2.901


# formula from DG 6.2
a12 <- a2.1 - a1.2
a13 <- a3.1 - a1.3
a23 <- a3.2 - a2.3

a1.23 <- 
  ((a1.2 + a1.3) / 2) +
  (((a2.3 - a2.1) +
   (a3.2 - a3.1))
   / 6)

a12.3 <- 
  a12 - ((a12 + a23 - a13) / 3)


# this works fine for three populations, but I'll end up with ~25
# populations

# need to convert different aspects of this process into factors?
# and then reproduce this example (or the four population one)
# with those factors?


# time series decomposition: four variables -------------------------------



# see DG page 99

a1.234 <- 
  ((a1.2 + a1.3 + a1.4) / 3) +
  (((a2.3 + a2.4 - 2 * a2.1) +
     (a3.2 + a3.4 - 2 * a3.1) +
     (a4.2 + a4.3 - 2 * a4.1))/ 
     12) 


a12.34 <- a12 - ((a12 + a23 - a13) + (a12 + a24 - a14)) / 4



