###############################################################################
### This script will estimate diabetes proportions from the National Health 
### Interview Survey in 2022. 
###############################################################################

### Make sure to load all dependencies, helper functions, and color palette and 
### set working directory by first running the setupEnvironAndDeps.R file!

###############################################################################
### Read in the NHIS data 
### Adult questionnaire
adultQuest0 <- read.csv("adult22csv/adult22.csv")
###############################################################################
### Filter the adult questionnaire to the columns of interest: 
### Weighting/survey variables 
### WTFA_A - weight
### PPSU- primary sampling unit
### PSTRAT

### Demographic data 
### NATUSBORN_A - born in the united states or territories
### AGEP_A - age of sample 
### HISP_A
### HISPALLP_A
### RACEALLP_A

### diabetes and gestational diabetes 
### DIBEV_A - ever had diabetes
### GESDIB_A - ever gestational diabetes

adultQuest <- adultQuest0 %>% dplyr::select(PPSU, 
                                            PSTRAT, 
                                            WTFA_A,
                                            NATUSBORN_A, 
                                            AGEP_A,
                                            HISP_A,
                                            HISPALLP_A,
                                            RACEALLP_A,
                                            DIBEV_A,
                                            GESDIB_A) 

###############################################################################
# Summarize the data into meaningful variables
NHIS2022data0 <- adultQuest %>%
                mutate("Age group" = cut(AGEP_A, 
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
                       "Race-ethnicity" = case_when(HISPALLP_A == 4 ~"Asian",
                                                    HISPALLP_A == 1 ~"Hispanic"),
                       "Nativity"  = case_when(NATUSBORN_A == 1 ~ "USB", 
                                               NATUSBORN_A == 2 ~ "NUSB"),
                       "Diabetes status w/Gest" = case_when(DIBEV_A == 1 & GESDIB_A !=1  ~ "Diabetes", 
                                                     DIBEV_A == 1 & GESDIB_A == 1 ~ "Gestational", 
                                                     DIBEV_A == 2 ~ "None"), 
                       "Diabetes status"= case_when(DIBEV_A == 1 & GESDIB_A !=1  ~ "Diabetes", 
                                                     DIBEV_A == 1 & GESDIB_A == 1 ~ NA, 
                                                     DIBEV_A == 2 ~ "None")) %>%
                        dplyr::select(PPSU, 
                                      PSTRAT, 
                                      WTFA_A,
                                      `Age group`, 
                                      `Race-ethnicity`,
                                      Nativity,
                                      `Diabetes status`)
                       

###############################################################################
### Survey weighting 
NHIS2022data <- svydesign(id      = ~ PPSU,
                            strata  = ~ PSTRAT,
                            weights = ~ WTFA_A,
                            nest    = TRUE,
                            data    = NHIS2022data0)

### save the survey object 
saveRDS(NHIS2022data, file = "data/2022_NHIS_survey_obj.rds", version = 2)

###############################################################################
### Check some proportions 

# Create subsets for each race ethnicity x nativity
asianDataUSB <- subset(NHIS2022data, subset = `Race-ethnicity` == "Asian" & Nativity == "USB")
asianDataNUSB <- subset(NHIS2022data, subset = `Race-ethnicity` == "Asian" & Nativity == "NUSB")
hispanicDataUSB <- subset(NHIS2022data, subset = `Race-ethnicity` == "Hispanic" & Nativity == "USB")
hispanicDataNUSB <- subset(NHIS2022data, subset = `Race-ethnicity` == "Hispanic" & Nativity == "NUSB")

### Diabetes prevalence 
### Stratified by race-ethnicity
svyby(~`Diabetes status`, by=~`Race-ethnicity`, NHIS2022data, svymean, 
      na.rm=T, deff=T, vartype=c("se", "cvpct"))

### Stratified by age, nativity, and race-ethnicity
svyby(~`Diabetes status`, by=~`Age group`, asianDataUSB, svymean, 
      na.rm=T, deff=T, vartype=c("se", "cvpct"))

svyby(~`Diabetes status`, by=~`Age group`, asianDataNUSB, svymean, 
      na.rm=T, deff=T, vartype=c("se", "cvpct"))

svyby(~`Diabetes status`, by=~`Age group`, hispanicDataUSB, svymean, 
      na.rm=T, deff=T, vartype=c("se", "cvpct"))

svyby(~`Diabetes status`, by=~`Age group`, hispanicDataNUSB, svymean, 
      na.rm=T, deff=T, vartype=c("se", "cvpct"))


###############################################################################
###############################################################################
###############################################################################
### Age stratified uncertainty is high in old population 
### Use IPUMS to attempt to pool over 2020 - 2022
###############################################################################
### Read in the data file
ipumsNHIS <- read.csv("nhis_00002.csv")

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
           # "Diabetes status w/Gest" = case_when(DIBEV_A == 1 & GESDIB_A !=1  ~ "Diabetes", 
           #                                      DIBEV_A == 1 & GESDIB_A == 1 ~ "Gestational", 
           #                                      DIBEV_A == 2 ~ "None"), 
           "Diabetes status"= case_when(DIABETICEV == 2  ~ "Diabetes", 
                                        DIABETICEV %in% c(0,3,7,8,9) ~ NA, 
                                        DIABETICEV == 1 ~ "None")) %>%
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
saveRDS(NHIS2020_2022data, file = "~/Documents/TTT & Diabetes/Data/2020_2022_NHIS_survey_obj.rds", version = 2)

###############################################################################
### Check some proportions 

# Create subsets for each race ethnicity x nativity
asianDataUSB2020_2022 <- subset(NHIS2020_2022data, subset = `Race-ethnicity` == "Asian" & Nativity == "USB")
saveRDS(asianDataUSB2020_2022, file = "~/Documents/TTT & Diabetes/Data/2020_2022_NHIS_AsianUSB_survey_obj.rds", version = 2)

asianDataNUSB2020_2022 <- subset(NHIS2020_2022data, subset = `Race-ethnicity` == "Asian" & Nativity == "NUSB")
saveRDS(asianDataNUSB2020_2022, file = "~/Documents/TTT & Diabetes/Data/2020_2022_NHIS_AsianNUSB_survey_obj.rds", version = 2)

hispanicDataUSB2020_2022 <- subset(NHIS2020_2022data, subset = `Race-ethnicity` == "Hispanic" & Nativity == "USB")
saveRDS(hispanicDataUSB2020_2022, file = "~/Documents/TTT & Diabetes/Data/2020_2022_NHIS_HispanicUSB_survey_obj.rds", version = 2)

hispanicDataNUSB2020_2022 <- subset(NHIS2020_2022data, subset = `Race-ethnicity` == "Hispanic" & Nativity == "NUSB")
saveRDS(hispanicDataNUSB2020_2022, file = "~/Documents/TTT & Diabetes/Data/2020_2022_NHIS_HispanicNUSB_survey_obj.rds", version = 2)

asianData2020_2022 <- subset(NHIS2020_2022data, subset = `Race-ethnicity` == "Asian")
hispanicData2020_2022 <- subset(NHIS2020_2022data, subset = `Race-ethnicity` == "Hispanic")

### Diabetes prevalence 
### Stratified by race-ethnicity
svyby(~`Diabetes status` + Nativity, by=~`Race-ethnicity`, NHIS2020_2022data, svymean, 
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
