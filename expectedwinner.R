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
library(readr)
library(jtools)

# ---- Load Data ----
df <- read_csv("anes_mergedfile_2016-2020-2024panel_20260519.csv")

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
  #Education
  redu_24 = if_else(V241465x < 0, NA_real_, V241465x),
  redu_20 = if_else(V201511x < 0, NA_real_, V201511x),
  redu_16 = case_when(
      V161270 < 0 ~ NA_real_,
      V161270 %in% c(1,8) ~ 1,  
      V161270 == 9 | V161270 == 90 ~ 2, 
      V161270 %in% c(10,12) ~ 3, 
      V161270 == 13 ~ 4, 
      V161270 %in% c(14,16) ~ 5
  ),
  
  #Follow politics
  campint_24 = if_else(V241004 < 0, NA_real_, V241004), #higher val = never 
  campint_20 = if_else(V201005 < 0, NA_real_, V201005),  
  campint_16 = if_else(V161003 < 0, NA_real_, V161003),   
  #Interpersonal trust
  ppltrust_24 = if_else(V241234 < 0, NA_real_, V241234), #higher val = never trust
  ppltrust_20 = if_else(V201237 < 0, NA_real_, V201237), 
  ppltrust_16 = if_else(V161219 < 0, NA_real_, V161219),
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
  rpid3_16 = "2016: Party ID",
  campint_24 = "Never follow politics",
  campint_20 = "Never follow politics",
  campint_16 = "Never follow politics",
  ppltrust_24 = "Never trust other people",
  ppltrust_20 = "Never trust other people",
  ppltrust_16 = "Never trust other people"
)

# -- subset to those who completed 2024
df <- subset(df, !is.na(V240106b))

# ---- Set weights ----
svy24 <- svydesign( 
  ids     = ~V240106c,
  strata  = ~V240106d,
  weights = ~V240106b,
  data    = df,
  nest    = TRUE
)
svy20 <- svydesign( 
  ids     = ~V200011c,
  strata  = ~V200011d,
  weights = ~V200011b,
  data    = df,
  nest    = TRUE
)
svy16 <- svydesign( 
  ids     = ~V160202,
  strata  = ~V160201,
  weights = ~V160102,
  data    = df,
  nest    = TRUE
)
options(survey.lonely.psu = "adjust")

#relevel pid for models
svy24$variables$rpid3_24 <- relevel(svy24$variables$rpid3_24, ref = "Independent")
svy20$variables$rpid3_20 <- relevel(svy20$variables$rpid3_20, ref = "Independent")
svy16$variables$rpid3_16 <- relevel(svy16$variables$rpid3_16, ref = "Independent")

# ---- Analyze ----
table(df$partymatch_24, df$prevotematch_24)

#CORRELATIONS
#correlations of partymatch and votematch with countfair, countacc and trust
#match measures by year
svycor(~partymatch_24 + prevotematch_24, design = svy24, na.rm = TRUE, boot = TRUE) #.7
svycor(~partymatch_20 + prevotematch_20, design = svy20, na.rm = TRUE, boot = TRUE) #.54
svycor(~partymatch_16 + prevotematch_16, design = svy16, na.rm = TRUE, boot = TRUE) #.64


svycor(~partymatch_24 + prevotematch_24 + countfair_24 + trust_24 + countacc_24, 
       design = svy24, na.rm = TRUE, boot = TRUE) 
#low correlations between match and outcomes; highest is prevotematch & countacc @ .10
svycor(~partymatch_20 + prevotematch_20 + countfair_20 + trust_20 + countacc_20, 
       design = svy20, na.rm = TRUE, boot = TRUE) 
#low correlations between match and outcomes; highest is prevotematch & countacc @ .05
svycor(~partymatch_16 + prevotematch_16 + countfair_16, 
       design = svy16, na.rm = TRUE, boot = TRUE) 

#MEANS
#mean trust, countfair, countacc at match = 0, 1 at each point in time
svyby(~countfair_24, ~prevotematch_24, svy24, svymean, na.rm = TRUE) #0.04
svyby(~countfair_20, ~prevotematch_20, svy20, svymean, na.rm = TRUE) #-0.02
svyby(~countfair_16, ~prevotematch_16, svy16, svymean, na.rm = TRUE) #0.01

#similar size differences (~0.04)
svyby(~trust_24, ~prevotematch_24, svy24, svymean, na.rm = TRUE) 
svyby(~trust_20, ~prevotematch_20, svy20, svymean, na.rm = TRUE) 
#similar size differences (~0.07)
svyby(~trust_24, ~partymatch_24, svy24, svymean, na.rm = TRUE) 
svyby(~trust_20, ~partymatch_20, svy20, svymean, na.rm = TRUE) 

#MODELS
#model: DVs: trust, countfair, countacc; IVs: partymatch or votematch, education, 
#interpersonal trust, followpol, party
#2024
fair24_party <- svyglm(countfair_24 ~ partymatch_24 + redu_24 + rpid3_24 + 
                 ppltrust_24 + campint_24, 
                 design = svy24, na.action = na.omit, family = "gaussian")
summary(fair24_party)
fair24_prevote <- svyglm(countfair_24 ~ prevotematch_24 + redu_24 + rpid3_24 + 
                         ppltrust_24 + campint_24, 
                       design = svy24, na.action = na.omit, family = "gaussian")
summary(fair24_prevote)

acc24_party <- svyglm(countacc_24 ~ partymatch_24 + redu_24 + rpid3_24 + 
                         ppltrust_24 + campint_24, 
                       design = svy24, na.action = na.omit, family = "gaussian")
summary(acc24_party)
acc24_prevote <- svyglm(countacc_24 ~ prevotematch_24 + redu_24 + rpid3_24 + 
                           ppltrust_24 + campint_24, 
                         design = svy24, na.action = na.omit, family = "gaussian")
summary(acc24_prevote)

trust24_party <- svyglm(trust_24 ~ partymatch_24 + redu_24 + rpid3_24 + 
                        ppltrust_24 + campint_24, 
                      design = svy24, na.action = na.omit, family = "gaussian")
summary(trust24_party)
trust24_prevote <- svyglm(trust_24 ~ prevotematch_24 + redu_24 + rpid3_24 + 
                          ppltrust_24 + campint_24, 
                        design = svy24, na.action = na.omit, family = "gaussian")
summary(trust24_prevote)

#2020
fair20_party <- svyglm(countfair_20 ~ partymatch_20 + redu_20 + rpid3_20 + 
                         ppltrust_20 + campint_20, 
                       design = svy20, na.action = na.omit, family = "gaussian")
summary(fair20_party)
fair20_prevote <- svyglm(countfair_20 ~ prevotematch_20 + redu_20 + rpid3_20 + 
                           ppltrust_20 + campint_20, 
                         design = svy20, na.action = na.omit, family = "gaussian")
summary(fair20_prevote)
acc20_party <- svyglm(countacc_20 ~ partymatch_20 + redu_20 + rpid3_20 + 
                        ppltrust_20 + campint_20, 
                      design = svy20, na.action = na.omit, family = "gaussian")
summary(acc20_party)
acc20_prevote <- svyglm(countacc_20 ~ prevotematch_20 + redu_20 + rpid3_20 + 
                          ppltrust_20 + campint_20, 
                        design = svy20, na.action = na.omit, family = "gaussian")
summary(acc20_prevote)
trust20_party <- svyglm(trust_20 ~ partymatch_20 + redu_20 + rpid3_20 + 
                          ppltrust_20 + campint_20, 
                        design = svy20, na.action = na.omit, family = "gaussian")
summary(trust20_party)
trust20_prevote <- svyglm(trust_20 ~ prevotematch_20 + redu_20 + rpid3_20 + 
                            ppltrust_20 + campint_20, 
                          design = svy20, na.action = na.omit, family = "gaussian")
summary(trust20_prevote)

fair16_party <- svyglm(countfair_16 ~ partymatch_16 + redu_16 + rpid3_16 + 
                         ppltrust_16 + campint_16, 
                       design = svy16, na.action = na.omit, family = "gaussian")
summary(fair16_party)
fair16_prevote <- svyglm(countfair_16 ~ prevotematch_16 + redu_16 + rpid3_16 + 
                           ppltrust_16 + campint_16, 
                         design = svy16, na.action = na.omit, family = "gaussian")
summary(fair16_prevote)

#export
stargazer(fair24_party, fair24_prevote, acc24_party, acc24_prevote,
          trust24_party, trust24_prevote,
          type = "html", out = "expwin24.html")

export_summs(fair24_party, fair20_party, fair16_party, 
             fair24_prevote, fair20_prevote, fair16_prevote, 
             acc24_party, acc20_party, acc24_prevote, acc20_prevote,
             trust24_party, trust20_party, trust24_prevote, trust20_prevote,
             model.names = c("Votes Counted Fairly (2024)", "Votes Counted Fairly (2020)", "Votes Counted Fairly (2016)",
                             "Votes Counted Fairly (2024)",  "Votes Counted Fairly (2020)", "Votes Counted Fairly (2016)",   
                             "Votes Counted Accurately (2024)", "Votes Counted Accurately (2020)", 
                             "Votes Counted Accurately (2024)", "Votes Counted Accurately (2020)",
                             "Trust Election Officials (2024)", "Trust Election Officials (2020)", 
                             "Trust Election Officials (2024)", "Trust Election Officials (2020)"),
             error_format = "({std.error})", 
             coefs = c("Expect own party to win" = "partymatch_24",
                       "Expect own party to win" = "partymatch_20",
                       "Expect own party to win" = "partymatch_16",
                       "Expect preferred candidate to win" = "prevotematch_24",
                       "Expect preferred candidate to win" = "prevotematch_20",
                       "Expect preferred candidate to win" = "prevotematch_16",
                       "Democrat" = "rpid3_24Democrat",
                       "Democrat" = "rpid3_20Democrat", "Democrat" = "rpid3_16Democrat",
                       "Republican" = "rpid3_24Republican", 
                       "Republican" = "rpid3_20Republican", "Republican" = "rpid3_16Republican",
                       "Education level" = "redu_24",
                       "Education level" = "redu_20", "Education level" = "redu_16",
                       "Never trust people" = "ppltrust_24",
                       "Never trust people" = "ppltrust_20", "Never trust people" = "ppltrust_16",
                       "Never follow politics" = "campint_24",
                       "Never follow politics" = "campint_20", "Never follow politics" = "campint_16"),
             to.file = "html", file.name = "expwin.html")
