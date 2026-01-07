library(nhanesA) # make it available in current R session
library(dplyr) 
#library(ggplot2) 
#library(e1071)

# ------------ OUTCOME ------------
# import cognitive functioning table (found in Q/Questionnaire)
# 'H' is for the cycle 2013-2014
cfq_h <- nhanes('CFQ_H') # conducted only on 60 yo and older
# CFDDS: DSST (Digit Symbol Substitution Test)
cfq_h_select_cols <- c('SEQN', 'CFDDS')
cfq_h_custom <- cfq_h[cfq_h_select_cols]
summary(cfq_h_custom, useNA = 'if_any') # there are 193 NAs

# ----- CANDIDATE PREDICTORS -----
####################### Data from Demography
# import demo_h table from DEMO/Demography component
demo_h <- nhanes('DEMO_H')
# RIAGENDR: gender, categorical, nominal
# RIDAGEYR: age, continuous, TODO needs re-centering
# DMDMARTL: marital status, 20+ yo, categorical, nominal
# DMDEDUC2: education level, highest degree, 20+ yo, categorical, ordinal
demo_h_select_cols <- c('SEQN', 'RIAGENDR', 'RIDAGEYR', 'DMDMARTL', 'DMDEDUC2')
demo_h_custom <- demo_h[demo_h_select_cols]
table(demo_h$RIAGENDR, useNA = 'ifany') # 2 levels
table(demo_h$DMDMARTL, useNA = 'ifany') # 6 levels, 2 Refused, 1 Don't Know, 4406 NAs, TODO needs re-coding
table(demo_h$DMDEDUC2, useNA = 'ifany') # 5 levels, 2 Refused, 5 Don't Know, 4406 NAs, TODO needs re-coding 

####################### Data from Questionnaire
## import income related table from dataset
inq_h <- nhanes('INQ_H')
# INDFMMPI:  Family monthly poverty level index, continuous
inq_h_select_cols <- c("SEQN", "INDFMMPI")
inq_h_custom <- inq_h[inq_h_select_cols]
## import alcohol related table from dataset
alq_h <- nhanes('ALQ_H')
# total drinks/week-month-year in the past year from ALQ120Q+ALQ120U
alq_h_select_cols <- c("SEQN", "ALQ120Q", "ALQ120U")
alq_h_custom <- alq_h[alq_h_select_cols]
# add new variable: alcohol_drinks_week, continuous
alq_h_custom <- alq_h_custom %>%
  mutate(
    alcohol_drinks_week = case_when(
      ALQ120Q == 0        ~ 0,
      is.na(ALQ120Q)      ~ NA_real_,
      # Refused (777) and Don't know (999) will be converted to NA
      ALQ120U == "Week"   ~ ALQ120Q,
      ALQ120U == "Month"  ~ ALQ120Q / 4.345, # weeks per month
      ALQ120U == "Year"  ~ ALQ120Q / 52.14, # weeks per year
      TRUE ~ NA_real_ 
    )
  )
## import smoking related table from dataset
smq_h <- nhanes('SMQ_H')
# SMQ040: Do you now smoke cigarettes, categorical, ordinal
smq_h_select_cols <- c("SEQN", "SMQ040")
smq_h_custom <- smq_h[smq_h_select_cols]
# check if it's ordered:
is.ordered(smq_h_custom$SMQ040) # outputs FALSE
table(smq_h_custom$SMQ040, useNA = "ifany") # 3 levels, 4589 NAs
# make the variable ordinal
smq_h_custom$SMQ040 <- factor(
  smq_h_custom$SMQ040,
  levels = c("Not at all", "Some days", "Every day"),
  ordered = TRUE
)
## import sleep related table from dataset
slq_h <- nhanes('SLQ_H')
# SLD010H: How much sleep do you get (hours), continuous
slq_select_cols <- c("SEQN", "SLD010H")
slq_h_custom <- slq_h[slq_select_cols]

## import diet related table from dataset
dbq_h <- nhanes('DBQ_H')
# DBQ700: How healthy is your diet, subjective, categorical, ordinal
dbq_h_select_cols <- c("SEQN", "DBQ700")
dbq_h_custom <- dbq_h[dbq_h_select_cols]
table(dbq_h_custom$DBQ700, useNA = "ifany") # 5 levels, 1 Don't know, 3711 NAs, TODO needs re-coding

## import diabetes related table from dataset
diq_h <- nhanes('DIQ_H')
# DIQ010: Doctor told you have diabetes, subjective, nominal
diq_h_select_cols <- c("SEQN", "DIQ010")
diq_h_custom <- diq_h[diq_h_select_cols]
table(diq_h_custom$DIQ010, useNA = "ifany") # 3 levels, 1 Refused, 5 Don't know, 1 NA
#TODO: re-code later after merging and checking frequency of levels

## import cardiovascular and neurological health related table from dataset
mcq_h <- nhanes('MCQ_H')
# MCQ160C: Ever told you had coronary heart disease, subjective, nominal
# MCQ160F: Ever told you had a stroke, subjective, nominal
mcq_h_select_cols <- c("SEQN", "MCQ160C", "MCQ160F")
mcq_h_custom <- mcq_h[mcq_h_select_cols]
table(mcq_h_custom$MCQ160C, useNA = "ifany") # 2 levels, 18 Don't know, 4001 NAs
table(mcq_h_custom$MCQ160F, useNA = "ifany") # 2 levels, 5 Don't know, 4001 NAs
# TODO: re-code later after merging and checking frequency of levels

## import depression related table from dataset
dpq_h <- nhanes('DPQ_H', translate=FALSE)
# Columns 2 to 10 are PHQ-9 questions
phq_cols <- 2:10
# there are instances where all 9 questions were answered, 
# instances where some where answered and others not, 
# and instances where there were no answers at all
# Initialize counters
n_total <- nrow(dpq_h)
n_all_na <- 0
n_partial_invalid <- 0

seqn_phq_partial_invalid <- c() # keeps track of where I forced NA

dpq_h$phq_9_score <- sapply(seq_len(nrow(dpq_h)), function(i) {
  x <- dpq_h[i, phq_cols]
  
  x_valid   <- x %in% 0:3
  x_invalid <- x %in% c(7, 9) | is.na(x)
  
  if (all(x_invalid)) {
    n_all_na <<- n_all_na + 1
    NA_real_
  } else if (any(x_invalid)) {
    seqn_phq_partial_invalid <<- c(seqn_phq_partial_invalid, dpq_h$SEQN[i])
    n_partial_invalid <<- n_partial_invalid + 1
    NA_real_
  } else {
    sum(x)
  }
})
# Summary
# cat("Total rows:", n_total, "\n")
# cat("All NA:", n_all_na, "\n")
# cat("Partial invalid (some valid, some invalid/missing):", n_partial_invalid, "\n")
# cat("Fully valid PHQ-9 scores:", sum(!is.na(dpq_h$phq_9_score)), "\n")
# there are 527 all NA answers, 25 with some valid/some invalid answers, and 5372 fully valid answers

## import physical activity related table from dataset
paq_h <- nhanes('PAQ_H', translated = FALSE)
# PAQ650: Vigorous recreational activities
# PAQ655: Days Vigorous recreational activities
# PAD660: Minutes Vigorous recreational activities
# PAQ665: Moderate recreational activities
# PAQ670: Days moderate recreational activities
# PAD675: Minutes moderate recreational activities
paq_h_select_cols <- c("SEQN", "PAQ650", "PAQ655", "PAD660", "PAQ665", "PAQ670", "PAD675")
paq_h_custom <- paq_h[paq_h_select_cols] 
table(paq_h_custom$PAQ650, useNA = 'ifany') # 2059 Yes, 5087 No, 1 Don't know, 2337 NAs
table(paq_h_custom$PAQ655, useNA = 'ifany') # 2 Don't know, 7426 NAs
table(paq_h_custom$PAD660, useNA = 'ifany') # 3 Don't know, 7429 NAs
table(paq_h_custom$PAQ665, useNA = 'ifany') # 3059 Yes, 4084 No, 2 Don't know, 2339 NAs
table(paq_h_custom$PAQ670, useNA = 'ifany') # 3 Don't know, 6425 NAs
table(paq_h_custom$PAD675, useNA = 'ifany') # 6428 NAs
# sum of (recreational) physical activity per week
paq_h_custom <- paq_h[paq_h_select_cols] %>%
  mutate(
    # ---- FLAGS: forced NA due to DK / Refused ----
    PAQ650_forced_na = PAQ650 %in% c(7, 9),
    PAQ655_forced_na = PAQ655 %in% c(77, 99),
    PAD660_forced_na = PAD660 %in% c(7777, 9999),
    PAQ665_forced_na = PAQ665 %in% c(7, 9),
    PAQ670_forced_na = PAQ670 %in% c(77, 99),
    PAD675_forced_na = PAD675 %in% c(7777, 9999),
    
    any_forced_na = PAQ650_forced_na | PAQ655_forced_na | PAD660_forced_na |
      PAQ665_forced_na | PAQ670_forced_na | PAD675_forced_na,
    
    # ---- Re-code invalid responses to NA ----
    PAQ650 = ifelse(PAQ650_forced_na, NA, PAQ650),
    PAQ655 = ifelse(PAQ655_forced_na, NA, PAQ655),
    PAD660 = ifelse(PAD660_forced_na, NA, PAD660),
    PAQ665 = ifelse(PAQ665_forced_na, NA, PAQ665),
    PAQ670 = ifelse(PAQ670_forced_na, NA, PAQ670),
    PAD675 = ifelse(PAD675_forced_na, NA, PAD675),
    
    # ---- Total recreational PA (vigorous + moderate), minutes/week, continuous ----
    rec_pa_minutes_week = case_when(
      # No vigorous AND no moderate
      PAQ650 == 2 & PAQ665 == 2 ~ 0,
      
      # Both vigorous and moderate
      PAQ650 == 1 & PAQ665 == 1 &
        !is.na(PAQ655) & !is.na(PAD660) &
        !is.na(PAQ670) & !is.na(PAD675) ~
        PAQ655 * PAD660 + PAQ670 * PAD675,
      
      # Vigorous only
      PAQ650 == 1 & !is.na(PAQ655) & !is.na(PAD660) ~
        PAQ655 * PAD660,
      
      # Moderate only
      PAQ665 == 1 & !is.na(PAQ670) & !is.na(PAD675) ~
        PAQ670 * PAD675,
      
      TRUE ~ NA_real_
    )
  )
seqn_pa_forced_na <- paq_h_custom %>%
  filter(any_forced_na) %>%
  distinct(SEQN) %>%
  pull(SEQN)

## import blood pressure related table from dataset
bpq_h <- nhanes('BPQ_H')
# BPQ020: ever told you had high BP, nominal
# this will be merged later with current readings of BP
bpq_h_select_cols <- c("SEQN", "BPQ020")
bpq_h_custom <- bpq_h[bpq_h_select_cols]
table(bpq_h_custom$BPQ020, useNA = 'ifany') # 2 levels, 5 Don't know

####################### Data from Examination
## import body measurements related table from dataset
bmx_h <- nhanes('BMX_H')
# BMXBMI: Body Mass Index, continuous
bmx_h_select_cols <- c("SEQN", "BMXBMI")
bmx_h_custom <- bmx_h[bmx_h_select_cols]

## import blood pressure related table from dataset
bpx_h <- nhanes('BPX_H')
# BPXSY1:BPXSY4 up to 4 current measurements of systolic BP
# BPXDI1:BPXDI4 up to 4 current measurements of diastolic BP
bpx_h_custom <- bpx_h %>%
  select(
    SEQN,
    BPXSY1, BPXSY2, BPXSY3, BPXSY4,
    BPXDI1, BPXDI2, BPXDI3, BPXDI4
  ) %>%
  mutate(
    mean_systolic  = rowMeans(across(BPXSY1:BPXSY4), na.rm = TRUE),
    mean_diastolic = rowMeans(across(BPXDI1:BPXDI4), na.rm = TRUE)
  )
# create current hypertension indication variable
bpx_h_custom <- bpx_h_custom %>%
  mutate(
    current_hypertension = case_when(
      is.na(mean_systolic) & is.na(mean_diastolic) ~ NA_character_,
      mean_systolic >= 140 | mean_diastolic >= 90 ~ "Yes",
      TRUE ~ "No" 
    )
  )
# merge the blood pressure hypertension data
bpq_h_custom <- bpq_h_custom %>%
  left_join(
    bpx_h_custom %>% select(SEQN, current_hypertension),
    by = "SEQN"
  )

# contingency table
table(bpq_h_custom$BPQ020, bpq_h_custom$current_hypertension, useNA = "ifany")
# new variable for hypertension evidence (previously diagnosed or currently indicated)
bpq_h_custom <- bpq_h_custom %>%
  mutate(
    hypertension_evidence = case_when(
      BPQ020 == "Yes" | current_hypertension == "Yes" ~ "Yes",
      BPQ020 == "No"  & current_hypertension == "No"  ~ "No",
      TRUE ~ NA_character_ # e.g., one NA/DK and the other NA/NO, set to NA
    )
  )
# making new variable categorical
bpq_h_custom$hypertension_evidence <- factor(
  bpq_h_custom$hypertension_evidence,
  levels = c("No", "Yes")
)
table(bpq_h_custom$hypertension_evidence, useNA = 'ifany') # there are 283 NAs

####################### Merging with cognitive functioning data
my_data <- cfq_h_custom %>%
  select(SEQN, dsst_score = CFDDS) %>%
  
  left_join(
    demo_h_custom %>%
      select(
        SEQN,
        gender = RIAGENDR,
        age = RIDAGEYR,
        marital_status = DMDMARTL,
        educ_level = DMDEDUC2
      ),
    by = "SEQN"
  ) %>%
  
  left_join(
    inq_h_custom %>%
      select(SEQN, income_ratio = INDFMMPI),
    by = "SEQN"
  ) %>%
  
  left_join(
    alq_h_custom %>%
      select(SEQN, alcohol_drinks_week),
    by = "SEQN"
  ) %>%
  
  left_join(
    smq_h_custom %>%
      select(SEQN, smoking = SMQ040),
    by = "SEQN"
  ) %>%
  
  left_join(
    slq_h_custom %>%
      select(SEQN, sleep_hours = SLD010H),
    by = "SEQN"
  ) %>%
  
  left_join(
    dbq_h_custom %>%
      select(SEQN, healthy_diet = DBQ700),
    by = "SEQN"
  ) %>%
  
  left_join(
    bmx_h_custom %>%
      select(SEQN, bmi = BMXBMI),
    by = "SEQN"
  ) %>%
  
  left_join(
    diq_h_custom %>%
      select(SEQN, diabetes = DIQ010),
    by = "SEQN"
  ) %>%
  
  left_join(
    mcq_h_custom %>%
      select(SEQN, coronary_dis = MCQ160C, stroke = MCQ160F),
    by = "SEQN"
  ) %>%
  
  left_join(
    dpq_h %>%
      select(SEQN, phq_9_score),
    by = "SEQN"
  ) %>%
  
  left_join(
    paq_h_custom %>%
      select(SEQN, rec_pa_minutes_week),
    by = "SEQN"
  ) %>%
  
  left_join(
    bpq_h_custom %>%
      select(SEQN, hypertension_evidence),
    by = "SEQN"
  )
