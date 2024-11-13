###############################################################################
### This script will estimate diabetes proportions from the National Health
### Interview Survey in 2020 - 2022.
###############################################################################

### Make sure to load all dependencies, helper functions, and color palette and
### set working directory by first running the setupEnvironAndDeps.R file!
source("code/setupEnvironAndDeps.R")
### Set working directory
setwd("~/TTTAsianHispDiabetes/LTBITestAndTreatAsianHispDiabetes")

###############################################################################
###############################################################################
###############################################################################
### Use NHIS IPUMS to attempt to pool over 2020 - 2022
###############################################################################
### Read in the data file
ipumsNHIS <- read.csv("data/nhis_00002.csv")

###############################################################################
# Summarize the data into meaningful variables
NHIS2020_2022 <- ipumsNHIS %>%
  filter(HISPRACE %in% c(1,4)) %>%
  mutate(AGE = ifelse(AGE < 150, AGE, NA),
         "Age group" = cut(AGE,
                           breaks=c(-Inf,4,14,24,34,44,54,64,
                                    74,84,Inf),
                           labels=c("0-4",   "5-14",  "15-24",
                                    "25-34", "35-44", "45-54",
                                    "55-64", "65-74", "75-84",
                                    "85p")),
         # "Hispanic" = ifelse(HISP_A == 1, "Yes", "No"),
         # "Asian" = ifelse(HISP_A != 1 & RACEALLP_A == 3, "Yes", "No"),
         # "Race-ethnicity" = case_when(HISP_A != 1 & RACEALLP_A == 3 ~"Asian",
         #                              HISP_A == 1 ~"Hispanic"),
         "Race-ethnicity" = case_when(HISPRACE == 4 ~"Asian",
                                      HISPRACE == 1 ~"Hispanic"),
         "Nativity"  = case_when(USBORN %in% c(20,11) ~ "USB",
                                 USBORN %in% c(10,12) ~ "NUSB",
                                 USBORN > 90 ~ NA),
         # "Diabetes status" = case_when(DIABETICEV == 2 & DIADIAGPREG ! = 2  ~ "Diabetes",
         #                               DIABETICEV != 2 & DIADIAGPREG == 2 ~ "Gestational",
         #                               DIADIAGPREG == 1 ~ "None")#,
         "Diabetes status"= case_when(DIABETICEV == 2  ~ "Diabetes",
                                      DIABETICEV %in% c(0,3,7,8,9) ~ NA,
                                      DIABETICEV == 1 ~ "None")
  ) %>%
  dplyr::select(PSU,
                STRATA,
                SAMPWEIGHT,
                `Age group`,
                `Race-ethnicity`,
                Nativity,
                `Diabetes status`)


###############################################################################
### Survey weighting
NHIS2020_2022data <- svydesign(id      = ~ PSU,
                               strata  = ~ STRATA,
                               weights = ~ SAMPWEIGHT,
                               nest    = TRUE,
                               data    = NHIS2020_2022)

### save the survey object
saveRDS(NHIS2020_2022data, file = "data/2020_2022_NHIS_survey_objT.rds", version = 2)

###############################################################################
### Check some proportions

# Create subsets for each race ethnicity x nativity
asianDataUSB2020_2022 <- subset(NHIS2020_2022data, subset = `Race-ethnicity` == "Asian" & Nativity == "USB")
saveRDS(asianDataUSB2020_2022, file = "data/2020_2022_NHIS_AsianUSB_survey_obj.rds", version = 2)

asianDataNUSB2020_2022 <- subset(NHIS2020_2022data, subset = `Race-ethnicity` == "Asian" & Nativity == "NUSB")
saveRDS(asianDataNUSB2020_2022, file = "data/2020_2022_NHIS_AsianNUSB_survey_obj.rds", version = 2)

hispanicDataUSB2020_2022 <- subset(NHIS2020_2022data, subset = `Race-ethnicity` == "Hispanic" & Nativity == "USB")
saveRDS(hispanicDataUSB2020_2022, file = "data/2020_2022_NHIS_HispanicUSB_survey_obj.rds", version = 2)

hispanicDataNUSB2020_2022 <- subset(NHIS2020_2022data, subset = `Race-ethnicity` == "Hispanic" & Nativity == "NUSB")
saveRDS(hispanicDataNUSB2020_2022, file = "data/2020_2022_NHIS_HispanicNUSB_survey_obj.rds", version = 2)

asianData2020_2022 <- subset(NHIS2020_2022data, subset = `Race-ethnicity` == "Asian")
hispanicData2020_2022 <- subset(NHIS2020_2022data, subset = `Race-ethnicity` == "Hispanic")

### Diabetes prevalence
### Stratified by race-ethnicity
svyby(~`Diabetes status` + Nativity, by=~`Race-ethnicity`, NHIS2020_2022data, svymean,
      na.rm=T, deff=T, vartype=c("se", "cvpct"))

svyby(~`Diabetes status`, by=~`Nativity`, asianData2020_2022, svymean,
      na.rm=T, deff=T, vartype=c("se", "cvpct"))

svyby(~`Diabetes status`, by=~Nativity, hispanicData2020_2022, svymean,
      na.rm=T, deff=T, vartype=c("se", "cvpct"))


### stratified by nativity only
svyby(~`Diabetes status`, by=~`Nativity`, asianData2020_2022, svymean,
      na.rm=T, deff=T, vartype=c("se", "cvpct"))[2]
svyby(~`Diabetes status`, by=~`Nativity`, hispanicData2020_2022, svymean,
      na.rm=T, deff=T, vartype=c("se", "cvpct"))[2]

### Stratified by age, nativity, and race-ethnicity
cbind(svyby(~`Diabetes status`, by=~`Age group`, asianDataUSB2020_2022, svymean,
            na.rm=T, deff=T, vartype=c("se", "cvpct"))[2],

      svyby(~`Diabetes status`, by=~`Age group`, asianDataNUSB2020_2022, svymean,
            na.rm=T, deff=T, vartype=c("se", "cvpct"))[2])

cbind(svyby(~`Diabetes status`, by=~`Age group`, hispanicDataUSB2020_2022, svymean,
            na.rm=T, deff=T, vartype=c("se", "cvpct"))[2],

      svyby(~`Diabetes status`, by=~`Age group`, hispanicDataNUSB2020_2022, svymean,
            na.rm=T, deff=T, vartype=c("se", "cvpct"))[2])
