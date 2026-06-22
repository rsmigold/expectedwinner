# ============================================================
# Expected winner and trust in vote count
# ============================================================
library(tidyr)
library(dplyr)
library(survey)     # svyset equivalent
library(psych)      # alpha()
library(ggplot2)
library(marginaleffects) # for probit marginal effects
library(labelled)
library(expss)
library(readxl)
library(scales)
library(stargazer)
library(broom)
library(modelsummary)
library(forcats)
library(weights)
library(correlation)
library(gt)

# ---- Load Data ----
df <- read.csv("~/Documents/anes_mergedfile_2016-2020-2024panel_20260519.csv")
# ---- Clean ----
df <- df %>%
  mutate(
  #PID --note 2024 is the new version of PID coding; can revert back to old
    rpid3_24 = case_when(
      V241227x < 0 ~ NA_real_,
      V241227x > 0 & V241227x < 4 ~ 1, 
      V241227x == 4 ~ 2,
      V241227x > 4 ~ 3,
      TRUE ~ NA_real_
    ),
    rpid3_24 = factor(rpid3_24, levels = 1:3,
                   labels = c("Democrat", "Independent", "Republican")),
    rpid3_20 = case_when(
      V201231x < 0 ~ NA_real_,
      V201231x > 0 & V201231x < 4 ~ 1, 
      V201231x == 4 ~ 2,
      V201231x > 4 ~ 3,
      TRUE ~ NA_real_
    ),
    rpid3_20 = factor(rpid3_20, levels = 1:3,
                      labels = c("Democrat", "Independent", "Republican")),
    rpid3_16 = case_when(
      V161158x < 0 ~ NA_real_,
      V161158x > 0 & V161158x < 4 ~ 1, 
      V161158x == 4 ~ 2,
      V161158x > 4 ~ 3,
      TRUE ~ NA_real_
    ),
    rpid3_16 = factor(rpid3_16, levels = 1:3,
                      labels = c("Democrat", "Independent", "Republican")),

  #Expected winner
  win_24 = case_when(
    V241211 < 0 ~ NA_real_,
    V241211 == 1 ~ 1, #Dem
    V241211 == 2 ~ 2, #Rep
    V241211 == 5 ~ 3, #Other,
    TRUE ~ NA_real_
  ),
  win_24 = factor(win_24, levels = 1:3, 
                  labels = c("Democratic candidate", "Republican candidate",
                            "Other candidate")),
  win_20 = case_when(
    V201217 < 0 ~ NA_real_,
    V201217 == 1 ~ 1, #Dem
    V201217 == 2 ~ 2, #Rep
    V201217 == 5 ~ 3, #Other,
    TRUE ~ NA_real_
  ),
  win_20 = factor(win_20, levels = 1:3, 
                  labels = c("Democratic candidate", "Republican candidate",
                            "Other candidate")),
  win_16 = case_when(
    V161146 < 0 ~ NA_real_,
    V161146 == 1 | V161146 == 3 ~ 1, #Dem
    V161146 == 2 | V161146 == 4 ~ 2, #Rep
    V161146 >= 5 ~ 3, #Other,
    TRUE ~ NA_real_
  ),
  win_16 = factor(win_16, levels = 1:3, 
                  labels = c("Democratic candidate", "Republican candidate",
                            "Other candidate")),
  #Close election
  #V241212
  #V201218
  #V161147
  
  #Pre-vote choice/intent/pref
    vc24 = if_else(V241039 < 0, NA_real_, V241039), #prevote prewho
    vi24 = if_else(V241043 < 0, NA_real_, V241043), #prevote intent
    pref24 = if_else(V241046 < 0, NA_real_, V241046), #prevote preference 
    prevote_24 = coalesce(vc24, vi24, pref24),
    prevote_24 = if_else(prevote_24 > 2, NA_real_, prevote_24), #remove third party
#    prevote_24 = factor(prevote_24, levels = 1:2), 
#                  labels = c("Democratic candidate", "Republican candidate"),
  
   vc20 = if_else(V201029 < 0, NA_real_, V201029), #prevote prewho
   vi20 = if_else(V201033 < 0, NA_real_, V201033), #prevote intent
   pref20 = if_else(V201036 < 0, NA_real_, V201036), #prevote preference
   prevote_20 = coalesce(vc20, vi20, pref20),
   prevote_20 = if_else(prevote_20 > 2, NA_real_, prevote_20), #remove third party
#   prevote_20 = factor(prevote_20, levels = 1:2), 
#                labels = c("Democratic candidate", "Republican candidate"),
  
   vc16 = if_else(V161027 < 0, NA_real_, V161027), #prevote prewho
   vi16 = if_else(V161031 < 0, NA_real_, V161031), #prevote intent
   pref16 = if_else(V161034 < 0, NA_real_, V161034), #prevote preference
   prevote_16 = coalesce(vc16, vi16, pref16),
   prevote_16 = if_else(prevote_16 > 2, NA_real_, prevote_16), #remove third party
 #  prevote_16 = factor(prevote_16, levels = 1:2), 
 #              labels = c("Democratic candidate", "Republican candidate"),
  
  #prevote matches expected winner
  prevotematch_24 = case_when(
    prevote_24 == 1 & win_24 == "Democratic candidate" ~ 1L,
    prevote_24 == 2 & win_24 == "Republican candidate" ~ 1L,
    is.na(prevote_24) | is.na(win_24)       ~ NA_integer_,
    TRUE                                  ~ 0L
  ),
  prevotematch_20 = case_when(
    prevote_20 == 1 & win_20 == "Democratic candidate" ~ 1L,
    prevote_20 == 2 & win_20 == "Republican candidate" ~ 1L,
    is.na(prevote_20) | is.na(win_20)       ~ NA_integer_,
    TRUE                                  ~ 0L
  ),
  prevotematch_16 = case_when(
    prevote_16 == 1 & win_16 == "Democratic candidate" ~ 1L,
    prevote_16 == 2 & win_16 == "Republican candidate" ~ 1L,
    is.na(prevote_16) | is.na(win_16)       ~ NA_integer_,
    TRUE                                  ~ 0L
  ),
  #partisanship matches expected winner
  partymatch_24 = case_when(
      rpid3_24 == "Democrat" & win_24 == "Democratic candidate" ~ 1L,
      rpid3_24 == "Republican" & win_24 == "Republican candidate" ~ 1L,
      is.na(rpid3_24) | is.na(win_24)       ~ NA_integer_,
      TRUE                                  ~ 0L
    ),
  partymatch_20 = case_when(
    rpid3_20 == "Democrat" & win_20 == "Democratic candidate" ~ 1L,
    rpid3_20 == "Republican" & win_20 == "Republican candidate" ~ 1L,
    is.na(rpid3_20) | is.na(win_20)       ~ NA_integer_,
    TRUE                                  ~ 0L
  ),
  partymatch_16 = case_when(
    rpid3_16 == "Democrat" & win_16 == "Democratic candidate" ~ 1L,
    rpid3_16 == "Republican" & win_16 == "Republican candidate" ~ 1L,
    is.na(rpid3_16) | is.na(win_16)       ~ NA_integer_,
    TRUE                                  ~ 0L
  ),
  #Votes are counted fairly? Post (16-20-24); higher values = trust count  
  countfair_24 = if_else(V242207 > 0, 1 - ((V242207-1)/4), NA_real_), 
  countfair_20 = if_else(V202219 > 0, 1 - ((V202219-1)/4), NA_real_),
  countfair_16 = if_else(V162219 > 0, 1 - ((V162219-1)/4), NA_real_),
  #Votes counted accurately? Pre (20-24)
  countacc_24 = if_else(V241314 > 0, ((V241314-1)/4), NA_real_),
  countacc_20 = if_else(V201351 > 0, ((V201351-1)/4), NA_real_),
  #How much do you trust the officials who oversee elections where you live? (20-24)
  trust_24 = if_else(V241315 > 0, (V241315-1)/4, NA_real_),
  trust_20 = if_else(V201352 > 0, (V201352-1)/4, NA_real_),    
)

var_label(df) <- list(
  rpid3_24 = "2024: Party ID",
  rpid3_20 = "2020: Party ID",
  rpid3_16 = "2016: Party ID"
)

# ---- Set weight ----
svy_design <- svydesign( 
  ids     = ~xx,
  strata  = ~yy,
  weights = ~zz,
  data    = df,
  nest    = TRUE
)

# ---- Analyze ----
table(df$partymatch_24, df$prevotematch_24)

#correlations of partymatch and votematch with countfair, countacc and trust

#mean trust, countfair, countacc at match = 0, 1 at each point in time

#model: DVs: trust, countfair, countacc; IVs: partymatch or votematch, education, 
#interpersonal trust, followpol, party(?)


