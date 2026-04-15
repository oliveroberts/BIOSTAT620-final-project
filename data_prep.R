library(ipumsr)
library(dplyr)
library(haven)
library(survey)
library(srvyr)
library(tidyverse)
library(ggplot2)
library(tidyr)
library(knitr)
library(kableExtra)
library(plotly)
library(shiny)

options(warn = -1)
options(digits=20)

path = "/Users/olivi/University of Michigan Dropbox/Olivia Roberts/CAsToR-DAD/Survey-Data/NHIS/Data/Data-AllTobacco-IPUMS/"

ddi <- read_ipums_ddi(paste0(path,"nhis_00010.xml"))
NHIS <- read_ipums_micro(ddi)

# cigarette use
NHIS$smk_ever <- ifelse(NHIS$SMOKEV == 2, 1,  
                          ifelse(NHIS$SMOKEV == 1, 0, NA))
NHIS$smk_current <- ifelse(
  NHIS$SMOKEV == 2 & NHIS$SMOKFREQNOW %in% c(2, 3), 1,
  ifelse(NHIS$SMOKEV == 1 | (NHIS$SMOKEV == 2 & NHIS$SMOKFREQNOW == 1),0,NA))

# e-cigarette use
NHIS$ecig_ever <- ifelse(NHIS$ECIGEV == 2, 1, 
                             ifelse(NHIS$ECIGEV == 1, 0, NA))
NHIS$ecig_current <- ifelse(NHIS$ECIGEV == 2 & NHIS$ECIGED %in% c(1,2), 1, 
                                ifelse(NHIS$ECIGEV == 1 | 
                                         NHIS$ECIGEV == 2 & NHIS$ECIGED == 3, 0, NA))

# cigar use
NHIS$cigar_ever <- ifelse(NHIS$CIGAREV == 2, 1,
                              ifelse(NHIS$CIGAREV == 1, 0, NA))
NHIS$cigar_current <- ifelse(
  NHIS$CIGAREV == 2 & NHIS$CIGARFREQNOW %in% c(1, 2), 1,
  ifelse(
    NHIS$CIGAREV == 1 | (NHIS$CIGAREV == 2 & NHIS$CIGARFREQNOW == 3),
    0,
    NA))

# smokeless tobacco use
NHIS$smkless_ever <- ifelse(NHIS$SMKLSEV == 2, 1, 
                                ifelse(NHIS$SMKLSEV == 1, 0, NA))
NHIS$smkless_current <- ifelse(NHIS$SMKLSEV == 2 & NHIS$SMKLSFREQNOW %in% c(1,2), 1, 
                                   ifelse(NHIS$SMKLSEV == 1 | 
                                            NHIS$SMKLSEV == 2 & NHIS$SMKLSFREQNOW == 4, 0, NA))

# pipe use
NHIS$pipe_ever <- ifelse(NHIS$PIPEV == 2, 1, 
                             ifelse(NHIS$PIPEV == 1, 0, NA))
NHIS$pipe_current <- ifelse(NHIS$PIPEV == 2 & NHIS$PIPEFREQNOW %in% c(1,2), 1, 
                                ifelse(NHIS$PIPEV == 1 | 
                                         NHIS$PIPEV == 2 & NHIS$PIPEFREQNOW == 3, 0, NA))

# race & ethnicity
NHIS$race_eth <- ifelse(NHIS$HISPETH %in% c(20,60), "Hispanic",  # Hispanic
                             ifelse(NHIS$HISPETH == 10 & NHIS$RACENEW == 100, "NHW",  # non-Hispanic White
                                    ifelse(NHIS$HISPETH == 10 & NHIS$RACENEW == 200, "NHB",  # non-Hispanic Black
                                           ifelse(NHIS$HISPETH == 10 & NHIS$RACENEW == 400, "NHA",  # non-Hispanic Asian
                                                  ifelse(NHIS$HISPETH == 10 & NHIS$RACENEW %in% c(300, 510, 542), "NHO", NA)))))  # non-Hispanic Other
NHIS$race_eth <- factor(NHIS$race_eth, 
                            levels = c("Hispanic","NHA", "NHB", "NHO", "NHW"))


options(survey.lonely.psu = "adjust")

NHIS$sexcat <- dplyr::case_when(
  NHIS$SEX == 1 ~ "Male",
  NHIS$SEX == 2 ~ "Female",
  TRUE ~ NA_character_)

NHIS$agecat <- dplyr::case_when(
  NHIS$AGE >= 18 & NHIS$AGE <= 24 ~ "18–24",
  NHIS$AGE >= 25 & NHIS$AGE <= 34 ~ "25–34",
  NHIS$AGE >= 35 & NHIS$AGE <= 54 ~ "35–54",
  NHIS$AGE >= 55 ~ "55+",
  TRUE ~ NA_character_)

NHIS$race_eth <- dplyr::recode(
  NHIS$race_eth,
  "Hispanic" = "Hispanic",
  "NHA" = "Non-Hispanic Asian",
  "NHB" = "Non-Hispanic Black",
  "NHO" = "Non-Hispanic Other",
  "NHW" = "Non-Hispanic White",
  .default = NA_character_)

NHIS <- NHIS %>% 
  select(YEAR, agecat, sexcat, race_eth, smk_current, ecig_current,
         cigar_current, smkless_current, pipe_current,
         smk_ever, ecig_ever, cigar_ever, smkless_ever, pipe_ever,
         PSU, STRATA, SAMPWEIGHT)

NHIS <- NHIS %>%  filter(
  !is.na(YEAR),
  !is.na(agecat),
  !is.na(sexcat),
  !is.na(race_eth),
  !is.na(PSU),
  !is.na(STRATA),
  !is.na(SAMPWEIGHT))

saveRDS(NHIS, "NHIS.rds")

NHIS <- NHIS |>
  sample_n(50000)

saveRDS(NHIS, "NHIS_reduced.rds")
