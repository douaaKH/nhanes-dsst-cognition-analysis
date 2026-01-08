str(my_data)

# we will first look at each variable summary
for (nm in names(my_data)) {
  cat("\n---", nm, "---\n")
  
  x <- my_data[[nm]]
  
  if (is.numeric(x)) {
    print(summary(x))
  } else {
    print(table(x, useNA = "ifany"))
  }
}

# dsst has 193 NAs, some reasons are given
# --
# gender has no missing values
# --
# age has no missing values, TODO needs re-centering, also 80+ are top-coded at 80
# --
# marital_status has 1 Refused, 1 Don't Know, both will be re-coded to NA
my_data$marital_status[my_data$marital_status %in% c("Refused", "Don't Know")] <- NA
my_data$marital_status <- droplevels(my_data$marital_status) 
# --
# educ_level has 0 Refused and 3 Don't Know
my_data$educ_level[my_data$educ_level %in% c("Refused", "Don't Know")] <- NA
my_data$educ_level <- droplevels(my_data$educ_level) 
# --
# income_ratio has 201 NAs, also 5+ values are top-coded at 5, worth checking: INDFMMPC - Family monthly poverty level category
# --
# alcohol_drinks_week has 410 NAs (4 or less of them answered 'Don't know', insignificant)
# --
# smoking has 895 NAs, about 50%
# --
table(my_data$sleep_hours, useNA = 'ifany')
# sleep_hours has 3 NAs and 2 Don't know that will be re-coded to NA
my_data$sleep_hours[my_data$sleep_hours == 99] <- NA
# --
# healthy_diet has 1 Don't know that will be re-coded to NA
my_data$healthy_diet[my_data$healthy_diet %in% c("Refused", "Don't know")] <- NA
my_data$healthy_diet <- droplevels(my_data$healthy_diet) 
# --
# bmi has 36 NAs
# --
# diabetes has 0 Refused and 1 Don't know that will be re-coded to NA
my_data$diabetes[my_data$diabetes %in% c("Refused", "Don't know")] <- NA
my_data$diabetes <- droplevels(my_data$diabetes)
# --
# coronary_dis has 12 Don't know that will be re-coded to NA
my_data$coronary_dis[my_data$coronary_dis %in% c("Refused", "Don't know")] <- NA
my_data$coronary_dis <- droplevels(my_data$coronary_dis)
# --
# stroke has 4 Don't know that will be re-coded to NA
my_data$stroke[my_data$stroke %in% c("Refused", "Don't know")] <- NA
my_data$stroke <- droplevels(my_data$stroke)
# --
# phq_9_score has 143 NAs, among them are people who gave partial valid answers 
sum(
  is.na(my_data$phq_9_score) &
    my_data$SEQN %in% seqn_phq_partial_invalid
)
# there are 18 people who gave partial valid answers (answered only some of the 9 questions) //
# and were re-coded as NA (around 12.58% of the NAs)
# --
# rec_pa_minutes_week has no missing values
# --
# hypertension_evidence has 26 NAs (some of them are potentially manually set \\
# e.g. when there wasn't at least one Yes or two Nos -> set to NA)
