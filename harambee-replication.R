########################################################
## Allen Roberts
## February 19, 2019
## Reproduce figures and tables for Harambee! paper
########################################################

rm(list = ls())

set.seed(06221989)

library(tidyverse)

load("data.RData")

##########################################
## Uptake
##########################################
## BMI
(hw 
 %>% group_by(., site, date)
 %>% summarise(n())
)

## Blood Pressure
(bp
  %>% group_by(., site, date)
  %>% summarise(n())
)

## Blood Testing
(cc
  %>% group_by(., site, date)
  %>% summarise(n())
)

##########################################
## Demographics
##########################################
## Age Groups
data$age_group <- cut(data$age, breaks = c(0, 30, 40, 50, 60, 70, Inf), labels = c("<30", "30-39", "40-49", "50-59", "60-69", "70+"), include.lowest = TRUE, right = FALSE)

(data
  %>% group_by(., age_group)
  %>% summarise("count" = n(), "pct" = round(100*n()/nrow(data)))
)

## remove missing values
(data[!is.na(data$age_group), ]
  %>% group_by(., age_group)
  %>% summarise("count" = n(), "pct" = round(100*n()/nrow(data[!is.na(data$age_group),])))
)

median(data$age, na.rm = TRUE)

## Gender
table(data$gender, exclude = NULL)
(data
  %>% group_by(., gender)
  %>% summarise("count" = n(), "pct" = round(100*n()/nrow(data)))
)

## Race
(data
  %>% group_by(., race)
  %>% summarise("count" = n(), "pct" = round(100*n()/nrow(data)))
)

## Religion
(data
  %>% group_by(., religion)
  %>% summarise("count" = n(), "pct" = round(100*n()/nrow(data)))
)

## Currently employed
mean(data$employed, na.rm = TRUE)

(data
  %>% group_by(., employed)
  %>% summarise("count" = n(), "pct" = round(100*n()/nrow(data)))
)

## Education
(data
  %>% group_by(., school)
  %>% summarise(., count = n(), "pct" = round(100*n()/nrow(data)))
)

## Language spoken at home
colSums(data[, which(colnames(data)=="lang_Eng"):which(colnames(data)=="lang_Other")], na.rm = TRUE)
round(100*apply(data[, which(colnames(data)=="lang_Eng"):which(colnames(data)=="lang_Other")], 2, mean, na.rm = TRUE), 0)

sum(apply(data[, which(colnames(data)=="lang_Eng"):which(colnames(data)=="lang_Other")], 1, function(x) sum(!is.na(x))) == 0) ## Number missing 

## Birth place
## Foreign born status
data$foreign_born <- ifelse(data$country_birth == "USA", 0, 1)
data$foreign_born[is.na(data$country_birth)] <- NA
data$foreign_born[data$study_id == "3-024-4"] <- 1 ## Emigrated in 2014, speaks Somali and Kiswahili at home

## African-born
data$african_born <- ifelse(data$country_birth %in% c("Eritrea", "Ethiopia", "Kenya", "Needs Translation", "Sierra Leone", "Somalia", "Sudan"), 1, 0) ## Need to verify that needs translation is african language
data$african_born[is.na(data$country_birth)] <- NA

## Can identify some of the missingness based on other responses
data$african_born[data$study_id == "3-024-4"] <- 1 ## Emigrated in 2014, speaks Somali and Kiswahili at home

## fb: either USA, Africa, or non-USA, non-Africa
data$fb <- data$foreign_born
data$fb[data$fb == 1] <- 2
data$fb[data$african_born == 1] <- 1
data$fb <- factor(data$fb, levels = c(0, 1, 2), labels = c("USA", "Africa", "Neither USA nor Africa"))

table(data$african_born, exclude = NULL)
mean(data$african_born, na.rm = TRUE)

(data
  %>% group_by(., country_birth)
  %>% summarise(., count = n(), "pct" = round(100*n()/nrow(data)))
)

## Language spoken at home
data[data$study_id %in% c("3-017-3", "3-021-7"), "lang_Amh"] <- 0 ## These are invalid IDs but both are from Mexico
table(data$lang_Amh, exclude = NULL)

mean(data$lang_Amh[data$african_born == 1])

## % born in Ethiopia
mean(data$country_birth == "Ethiopia", na.rm = TRUE)

## Emigrated within last 5 years
sum(data$year_emigrate > 2013, na.rm = TRUE)
mean(data$year_emigrate > 2013, na.rm = TRUE)

table(data$year_emigrate[data$foreign_born == 1] > 2013, exclude = NULL)

## Traveled outside of USA in last 5 years
mean(data$travel, na.rm = TRUE)
sum(data$travel, na.rm = TRUE)

table(data$travel, exclude = NULL)

##########################################
## Engagement in health care
##########################################
## Health insurance status by foreign born status
table(data$health_care_cov, exclude = NULL, deparse.level = 2)
table(data$health_care_cov, data$fb, exclude = NULL, deparse.level = 2)

table(data$health_care_cov, exclude = NULL)

## Personal doctor/Health care provider by foreign born status
table(data$personal_doc, exclude = NULL, deparse.level = 2)
table(data$personal_doc, data$fb, exclude = NULL, deparse.level = 2)

table(data$personal_doc,exclude = NULL, deparse.level = 2)

## Last routine checkup by foreign-born status
(data
  %>% group_by(., fb, routine_checkup)
  %>% summarise(., count = n())
)

table(data$routine_checkup, exclude = NULL, deparse.level = 2)

## Ever tested for HIV
table(data$ever_test, data$fb, exclude = NULL, deparse.level = 2)
table(data$ever_test, exclude = NULL, deparse.level = 2)

## Tested within the last five years
table(data$hiv_test_last_5_years, data$fb, exclude = NULL, deparse.level = 2)

## Accepted HIV testing
table(data$client_test_hiv, data$fb, exclude = NULL, deparse.level = 2)

table(data$ever_test[data$client_test_hiv == 1], data$fb[data$client_test_hiv == 1], exclude = NULL, deparse.level = 2)

## Risk factors among African-born individuals accepting HIV testing
table(data$num_partners_12[data$african_born == 1], exclude = NULL)
## 68% reported at least one partner in the last year

## 
table(data$last_test_usa[data$african_born == 1 & data$ever_test == "Yes"], exclude = NULL) ## Of those who had previously tested, 48% had previously tested in the US

table(data$ever_test[data$african_born == 1 & data$client_test_hiv == 1], exclude = NULL)

table(data$last_test_usa[data$african_born == 1 & data$client_test_hiv == 1], data$ever_test[data$african_born == 1 & data$client_test_hiv == 1], exclude = NULL, deparse.level = 2)

## Reasons for declining HIV test
table(data$reason_no_hiv_test_alreadyknows, data$fb, exclude = NULL)
## 7/18 - already knows HIV status
table(data$reason_no_hiv_test_alreadyknows)
table(data$reason_no_hiv_test_alreadyknows[data$client_test_hiv == 0], exclude = NULL)

table(data$reason_no_hiv_test_notatrisk, data$fb, exclude = NULL)
## 7/18 - not at risk
table(data$reason_no_hiv_test_notatrisk)

## Lived in same house as someone with TB
table(data$lived_tb, data$fb, exclude = NULL, deparse.level = 2)
table(data$lived_tb, exclude = NULL, deparse.level = 2)

## Taken any medications for TB treatment
table(data$tb_meds, data$fb, exclude = NULL, deparse.level = 2)
table(data$tb_meds, exclude = NULL, deparse.level = 2)

## Barriers around HIV testing

## Barriers faced in testing for HIV
colSums(data[, which(colnames(data)=="bar_transport"):which(colnames(data)=="bar_other")], na.rm = TRUE)
round(100*apply(data[, which(colnames(data)=="bar_transport"):which(colnames(data)=="bar_other")], 2, mean, na.rm = TRUE), 0)

data$barriers_other[data$bar_other == 1]

## Barriers around confidentiality, shame, stigma, or isolation
data$bar_conf_comp <- ifelse(data$bar_confidentiality == 1 | data$bar_mightberecognized == 1 | data$bar_afraidlosepartner == 1 | data$bar_afraidloseinsurance == 1, 1, 0)
data$bar_conf_comp[data$barriers_other %in% c("Afraid of people knowing", "Shame", "Afraid of being isolated by community")] <- 1
table(data$bar_conf_comp, exclude = NULL)

## Barriers around access to testing
data$bar_access_comp <- ifelse(data$bar_dontknowwhere == 1 | data$bar_notime == 1 | data$bar_toofar == 1 | data$barriers_other %in% c("No one ever offered", "cost/insurance"), 1, 0)
table(data$bar_access_comp, exclude = NULL)

## Other barriers
data$bar_other_comp <- ifelse(data$bar_conf_comp == 0 & data$bar_access_comp == 0 & data$bar_none == 0, 1, 0)
table(data$bar_other_comp, exclude = NULL)

table(data$bar_none, exclude = NULL)

##########################################
## NCDs by foreign-born status
##########################################
## BMI
bmi_fb <- merge(data, hw[, c("study_id", "bmi")], by = "study_id", all.x = TRUE)
bmi_fb$bmi_cat <- cut(bmi_fb$bmi, breaks = c(0, 18.5, 25, 30, Inf), labels = c("Underweight", "Healthy", "Overweight", "Obese"), include.lowest = FALSE, right = FALSE)

(bmi_fb 
  %>% group_by(., fb, bmi_cat)
  %>% summarise(count = n())
)

(bmi_fb 
  %>% group_by(., fb, bmi_cat)
  %>% filter(!is.na(bmi_cat))
  %>% summarise(count = n())
  %>% mutate(pct = count/sum(count))
)

(bmi_fb 
  %>% group_by(.,  bmi_cat)
  %>% filter(!is.na(bmi_cat))
  %>% summarise(count = n())
  %>% mutate(pct = count/sum(count))
)

table(bmi_fb$bmi_cat)

## Blood Pressure
bp_fb <- merge(data, bp[, c("study_id", "sys", "dias")], by = "study_id", all.x = TRUE)

bp_fb$bp_cat <- NA
bp_fb$bp_cat[bp_fb$sys < 130 & bp_fb$dias < 80] <- "Normal/Elevated"
bp_fb$bp_cat[(bp_fb$sys >= 130 & bp_fb$sys < 140) | (bp_fb$dias >= 80 & bp_fb$dias < 90)] <- "Stage 1"
bp_fb$bp_cat[(bp_fb$sys >= 140) | (bp_fb$dias>= 90)] <- "Stage 2/Hypertensive crisis"

(bp_fb 
  %>% group_by(., fb, bp_cat)
  %>% summarise(count = n())
)

(bp_fb 
  %>% group_by(., fb, bp_cat)
  %>% filter(!is.na(bp_cat))
  %>% summarise(count = n())
  %>% mutate(pct = count/sum(count))
)

(bp_fb 
  %>% group_by(., bp_cat)
  %>% filter(!is.na(bp_cat))
  %>% summarise(count = n())
  %>% mutate(pct = count/sum(count))
)

table(bp_fb$bp_cat)

## Cardiochek
cc_fb <- merge(data, cc[, c("study_id", "glucose", "total_chol", "hdl")], by = "study_id", all.x = TRUE)

## Glucose
# Normal non-fasting glucose: <200
cc_fb$glucose_cat <- cut(cc_fb$glucose, breaks = c(0, 200, Inf), labels = c("Normal", "Elevated"), include.lowest = FALSE, right = FALSE)
table(cc_fb$glucose_cat, cc_fb$fb, exclude = NULL, deparse.level = 2)

table(cc_fb$glucose_cat)

(cc_fb 
  %>% group_by(., fb, glucose_cat)
  %>% filter(!is.na(glucose_cat))
  %>% summarise(count = n())
  %>% mutate(pct = count/sum(count))
)

(cc_fb 
  %>% group_by(., glucose_cat)
  %>% filter(!is.na(glucose_cat))
  %>% summarise(count = n())
  %>% mutate(pct = count/sum(count))
)


## Total cholesterol
cc_fb$total_chol_cat <- cut(cc_fb$total_chol, breaks = c(0, 200, Inf), labels = c("Normal", "Elevated"), include.lowest = FALSE, right = FALSE)
table(cc_fb$total_chol_cat, cc_fb$fb, deparse.level = 2, exclude = NULL)

table(cc_fb$total_chol_cat)

(cc_fb 
  %>% group_by(., fb, total_chol_cat)
  %>% filter(!is.na(total_chol_cat))
  %>% summarise(count = n())
  %>% mutate(pct = count/sum(count))
)

(cc_fb 
  %>% group_by(., total_chol_cat)
  %>% filter(!is.na(total_chol_cat))
  %>% summarise(count = n())
  %>% mutate(pct = count/sum(count))
)

## HDL
cc_fb$hdl_cat <- cut(cc_fb$hdl, breaks = c(0, 40, Inf),  labels = c("Low", "Normal"), include.lowest = TRUE, right = FALSE)
table(cc_fb$hdl_cat, cc_fb$fb, deparse.level = 2, exclude = NULL)

table(cc_fb$hdl_cat)

(cc_fb 
  %>% group_by(., fb, hdl_cat)
  %>% filter(!is.na(hdl_cat))
  %>% summarise(count = n())
  %>% mutate(pct = count/sum(count))
)

(cc_fb 
  %>% group_by(., hdl_cat)
  %>% filter(!is.na(hdl_cat))
  %>% summarise(count = n())
  %>% mutate(pct = count/sum(count))
)

## Average values
(cc_fb
  %>% group_by(., fb)
  %>% summarise(mean_chol = mean(total_chol, na.rm = TRUE), mean_gluc = mean(glucose, na.rm = TRUE), mean_hdl = mean(hdl, na.rm = TRUE))
)

## PHQ >= 3
(data
  %>% group_by(., fb)
  %>% summarise(., pos = sum(phq_score >= 3, na.rm = TRUE), neg = sum(phq_score < 3, na.rm = TRUE))
  %>% mutate(pct_pos = pos/(pos+neg), pct_neg = neg/(pos+neg))
)

(data
  %>% summarise(., pos = sum(phq_score >= 3, na.rm = TRUE), neg = sum(phq_score < 3, na.rm = TRUE))
  %>% mutate(pct_pos = pos/(pos+neg), pct_neg = neg/(pos+neg))
)


## HIV
(data
  %>% group_by(., fb)
  %>% summarise(., neg_hiv = sum(client_test_hiv == 1, na.rm = TRUE))
)
##########################################
## Predictors of HIV testing
##########################################
## Age
data$age_group_hiv <- cut(data$age, breaks = c(0, 35, 50, Inf), labels = c("<35", "35-50", "50+"), include.lowest = TRUE, right = FALSE)

(data
  %>% group_by(., age_group_hiv, client_test_hiv)
  %>% summarise(n())
)

chisq.test(data$age_group_hiv[!is.na(data$age_group_hiv) & !is.na(data$client_test_hiv)], data$client_test_hiv[!is.na(data$age_group_hiv) & !is.na(data$client_test_hiv)])

## Gender
(data
  %>% group_by(., gender, client_test_hiv)
  %>% summarise(n())
)
chisq.test(data$gender[data$gender %in% c("Male", "Female") & !is.na(data$client_test_hiv)], data$client_test_hiv[data$gender %in% c("Male", "Female") & !is.na(data$client_test_hiv)])

## Foreign-born
(data
  %>% group_by(., fb, client_test_hiv)
  %>% summarise(n())
)

chisq.test(data$fb[!is.na(data$fb) & !is.na(data$client_test_hiv)], data$client_test_hiv[!is.na(data$fb) & !is.na(data$client_test_hiv)])


## Ever tested for HIV
(data
  %>% group_by(., ever_test, client_test_hiv)
  %>% summarise(n())
)

chisq.test(data$ever_test[!is.na(data$ever_test) & !is.na(data$client_test_hiv)], data$client_test_hiv[!is.na(data$ever_test) & !is.na(data$client_test_hiv)])

## Tested for HIV within last year
(data
  %>% group_by(., hiv_test_last_year, client_test_hiv)
  %>% summarise(n())
)

data$hiv_test_last_year_cat <- data$hiv_test_last_year
data$hiv_test_last_year_cat[is.na(data$hiv_test_last_year)] <- "Missing"
chisq.test(data$hiv_test_last_year_cat[data$hiv_test_last_year_cat %in% c("0", "1") & !is.na(data$client_test_hiv)], data$client_test_hiv[data$hiv_test_last_year_cat %in% c("0", "1") & !is.na(data$client_test_hiv)])

## Tested for HIV within last 5 years
(data
  %>% group_by(., hiv_test_last_5_years, client_test_hiv)
  %>% summarise(n())
)

## Number of partners
(data
  %>% group_by(., num_partners_12, client_test_hiv)
  %>% summarise(n())
)

data$num_partners_12_cat <- data$num_partners_12
data$num_partners_12_cat[data$num_partners_12 >= 2] <- "2+"
chisq.test(data$num_partners_12_cat[!is.na(data$num_partners_12_cat) & !is.na(data$client_test_hiv)], data$client_test_hiv[!is.na(data$num_partners_12_cat) & !is.na(data$client_test_hiv)], simulate.p.value = TRUE)

## Friend or family known to be HIV positive
(data
  %>% group_by(., friend_fam_hiv, client_test_hiv)
  %>% summarise(n())
)

chisq.test(data$friend_fam_hiv[!is.na(data$friend_fam_hiv) & !is.na(data$client_test_hiv)], data$client_test_hiv[!is.na(data$friend_fam_hiv) & !is.na(data$client_test_hiv)], simulate.p.value = TRUE)

## Insurance status
(data
  %>% group_by(., health_care_cov, client_test_hiv)
  %>% summarise(n())
)

chisq.test(data$health_care_cov[data$health_care_cov %in% c("Yes", "No") & !is.na(data$client_test_hiv)], data$client_test_hiv[data$health_care_cov %in% c("Yes", "No") & !is.na(data$client_test_hiv)], simulate.p.value = TRUE)

## Personal doctor/health care provider
(data
  %>% group_by(., personal_doc, client_test_hiv)
  %>% summarise(n())
)
chisq.test(data$personal_doc, data$client_test_hiv, simulate.p.value = TRUE)



