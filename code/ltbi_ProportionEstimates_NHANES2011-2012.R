###############################################################################

### Make sure to load all dependencies, helper functions, and color palette and 
### set working directory by first running the setupEnvironAndDeps.R file!

###############################################################################

###############################################################################
# Download NHANES1112 2011-2012 to temporary file   
# Demographic data
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/DEMO_G.XPT", 
              tfT <- tempfile(), mode="wb")

# TST data
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/TBX_G.XPT", 
              tf2T <- tempfile(), mode="wb")

# Diabetes data 
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/DIQ_G.XPT", 
              tf3T <- tempfile(), mode="wb")

# Laboratory data 
# IGRA
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/TB_G.XPT", 
              tf4T <- tempfile(), mode="wb")

# Fasting glucose
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/GLU_G.XPT", 
              tf5T <- tempfile(), mode="wb")


# Reproductive data 
# This file is necessary to remove those with gestational diabetes from the analysis 
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/RHQ_G.XPT", 
              tf6T <- tempfile(), mode="wb")

# Create Data Frames From Temporary Files
demographicNHANES1112 <- foreign::read.xport(tfT)
tstNHANES1112 <- foreign::read.xport(tf2T)
diabetesNHANES1112 <- foreign::read.xport(tf3T)
igraNHANES1112 <- foreign::read.xport(tf4T)
glucoseNHANES1112 <- foreign::read.xport(tf5T)
reprodNHANES1112 <- foreign::read.xport(tf6T)

###############################################################################
# Retain relevant variables 
demographicNHANES1112 <- demographicNHANES1112[c("SEQN",      # Respondent sequence number
                                                 "SDMVPSU",   # Person
                                                 "SDMVSTRA",  # Strata
                                                 "WTMEC2YR",  # Weights
                                                 "WTINT2YR",
                                                 "RIDAGEYR",  # Age in years at screening
                                                 "RIDRETH3",  # Race/Hispanic origin w/ NH Asian
                                                 "DMDBORN4")] # Country of birth

tstNHANES1112 <- tstNHANES1112[c("SEQN",      # Respondent sequence number
                                 "TBDRUIND")]  # Induration in mm for tst 

diabetesNHANES1112 <- diabetesNHANES1112[c("SEQN",    # Respondent sequence number
                                           "DIQ010",  # Doctor told you have diabetes
                                           "DIQ160",  # Ever told you have prediabetes
                                           "DIQ280")] # What was your last A1C level

### of note - this round of NHANES does not record a session variable as glucose 
### was only tested in the morning session.
glucoseNHANES1112 <- glucoseNHANES1112[c("SEQN",     # Respondent sequence number
                                         "WTSAF2YR", # Subsample weights
                                         "LBXGLU",  # LBXGLU - Fasting Glucose (mg/dL)
                                          "PHAFSTHR")] # Fasting hours 

igraNHANES1112 <- igraNHANES1112[c("SEQN",      # Respondent sequence number
                                   "LBXTBIN")]  # Coded result of IGRA test

reprodNHANES1112 <- reprodNHANES1112[c("SEQN",      # Respondent sequence number
                                       "RHQ162")] # While pregnant told diabetes

###############################################################################
# Merge data
tbMerge <- merge(tstNHANES1112,
                 igraNHANES1112,
                 by = "SEQN", all=TRUE)

diabetesMerge0 <- merge(diabetesNHANES1112,
                        reprodNHANES1112,
                       by = "SEQN", all=TRUE) 

diabetesMerge <- merge(diabetesMerge0,
                        glucoseNHANES1112,
                        by = "SEQN", all=TRUE) 

mergedData0 <- merge(tbMerge,
                     diabetesMerge,
                     by = "SEQN", all=TRUE) 

mergedData <- merge(demographicNHANES1112,
                    mergedData0,
                    by = "SEQN", all=TRUE) 

###############################################################################
# Summarize the data into meaningful variables
NHANES1112Data <- mergedData %>%
                  filter(RIDAGEYR > 14) %>% 
                    mutate("Age group" = cut(RIDAGEYR, breaks=c(-Inf,24,34,44,54,64,74,Inf),
                                             labels=c("15-24", "25-34", "35-44",
                                                      "45-54","55-64","65-74","75p")),
                  # mutate("Age group" = cut(RIDAGEYR, breaks=c(-Inf,4,14,24,34,44,54,64,74,Inf),
                  #                        labels=c("0-4","5-14", "15-24", "25-34", "35-44",
                  #                                 "45-54","55-64","65-74","75p")),
                        "Race-ethnicity" = case_when(RIDRETH3 == 6 ~"Asian",
                                                     RIDRETH3 == 1 | RIDRETH3 == 2 ~"Hispanic"),
                        "Hispanic" = ifelse(RIDRETH3 == 1 | RIDRETH3 == 2, "Yes", "No"),
                        "Asian" = ifelse(RIDRETH3 == 6, "Yes", "No"),
                        "Nativity" = case_when(DMDBORN4 == 1 ~ "USB", 
                                               DMDBORN4 == 2 ~ "NUSB"),
                       "TST" = case_when(TBDRUIND >= 10 ~ "Positive", 
                                         TBDRUIND >= 8 ~ "Borderline", 
                                         TBDRUIND < 8 ~ "Negative"),
                       "Told diabetes" = case_when(DIQ010 == 1 ~ "Yes",
                                                   DIQ010 == 2 ~ "No",
                                                   DIQ010 == 3 ~ "Borderline"),
                       "IGRA" = case_when(LBXTBIN == 1 ~ "Positive", 
                                          LBXTBIN == 2 ~ "Negative", 
                                          LBXTBIN == 3 ~ "Interminate"),
                       "IGRA positive" = ifelse(IGRA == "Positive", "Yes", "No"),
                       "Fast glucose" = cut(LBXGLU, breaks=c(-Inf, 125, Inf),
                                            labels=c("Normal", "High")), 
                       "A1c" = cut(DIQ280, breaks=c(-Inf, 6.5, Inf),
                                   labels=c("Not high", "High")),
                       "TB status" = ifelse(TST == "Positive" | IGRA == "Positive", "TB", "None"), 
                       "Diabetes status" = case_when(`Told diabetes` == "No" | 
                                                    `Fast glucose` == "Normal" |
                                                     A1c == "Not High"    ~ "None", 
                                                     `Told diabetes` == "Yes" & RHQ162 != 1  ~ "Diabetes",
                                                     `Told diabetes` == "Yes" & RHQ162 == 1  ~ "Gestational",
                                                     `Fast glucose` == "High" & PHAFSTHR %in% 8:24 ~ "Diabetes",
                                                     `Fast glucose` == "High" & ! PHAFSTHR %in% 8:24 ~ "Unknown",
                                                     `A1c` == "High" ~ "Diabetes")) %>% 
                dplyr::select("SEQN", "SDMVPSU", "SDMVSTRA", "WTMEC2YR", "WTSAF2YR", "WTINT2YR",
                              "Age group", "Hispanic", "Asian","Race-ethnicity", "Nativity",  
                              "TST", "IGRA", "IGRA positive",
                                          # "Told prediabetes", "A1c",
                                          "Told diabetes", "Fast glucose", 
                                          "TB status", "Diabetes status") %>% 
                mutate("Comorbid" = ifelse(`TB status` == "TB" & `Diabetes status` == "Diabetes", "Yes", "No"))

###############################################################################
# Survey weighting 
NHANES1112Data <- svydesign(id      = ~ SDMVPSU,
                        strata  = ~ SDMVSTRA,
                        weights = ~ WTINT2YR,
                        nest    = TRUE,
                        data    = NHANES1112Data)
### how many were filtered out due to inadequate fasting
sum(na.omit(NHANES1112Data$variables$`Diabetes status`) == "Diabetes")

### how many were filtered out due to inadequate fasting
sum(na.omit(NHANES1112Data$variables$`Diabetes status`) == "Unknown")
### how many with gestational diabetes
sum(na.omit(NHANES1112Data$variables$`Diabetes status`) == "Gestational")

saveRDS(NHANES1112Data, file = "~/Documents/TTT & Diabetes/Data/2011_12_NHANES_survey_obj_newA1c.rds", version = 2)
###############################################################################
asianData<- subset(NHANES1112Data, subset = `Race-ethnicity` == "Asian")
hispanicData <- subset(NHANES1112Data, subset = `Race-ethnicity` == "Hispanic")

# Create subsets for each race ethnicity x nativity
asianDataUSB <- subset(NHANES1112Data, subset = `Race-ethnicity` == "Asian" & Nativity == "USB")
asianDataNUSB <- subset(NHANES1112Data, subset = `Race-ethnicity` == "Asian" & Nativity == "NUSB")
hispanicDataUSB <- subset(NHANES1112Data, subset = `Race-ethnicity` == "Hispanic" & Nativity == "USB")
hispanicDataNUSB <- subset(NHANES1112Data, subset = `Race-ethnicity` == "Hispanic" & Nativity == "NUSB")

# And with diabetes only 
asianDataUSBDiab <- subset(NHANES1112Data, subset = `Race-ethnicity` == "Asian" & 
                                                 Nativity == "USB" & 
                                                `Diabetes status` == "Diabetes")
asianDataNUSBDiab <- subset(NHANES1112Data, subset = `Race-ethnicity` == "Asian" & 
                                                  Nativity == "NUSB" & 
                                                 `Diabetes status` == "Diabetes")
hispanicDataUSBDiab <- subset(NHANES1112Data, subset = `Race-ethnicity` == "Hispanic" & 
                                                    Nativity == "USB" & 
                                                   `Diabetes status` == "Diabetes")
hispanicDataNUSBDiab <- subset(NHANES1112Data, subset = `Race-ethnicity` == "Hispanic" &
                                                     Nativity == "NUSB" & 
                                                    `Diabetes status` == "Diabetes")

###############################################################################
# Generate proportions of TB status 

svyby(~`IGRA`, by=~`Race-ethnicity`, NHANES1112Data, svymean, 
      na.rm=T, deff=T, vartype=c("se", "cvpct"))

##### Check that we are within plausibility with comparison with Haddad
svyby(~`TB status`, by=~`Diabetes status`, asianData, svymean, 
      na.rm=T, deff=T, vartype=c("se", "cvpct"))

svyby(~`TB status`, by=~`Diabetes status`, hispanicData, svymean, 
      na.rm=T, deff=T, vartype=c("se", "cvpct"))

##### Neither of these are exactly as Haddad reports, but we define diabetes 
##### differently. Hispanic is much closer to her reported values. Need to 
##### discuss further. 


##### Asian LTBI by Nativity
svyby(~`TB status`, by=~`Nativity`, asianData, svymean, 
                    na.rm=T, deff=T, vartype=c("se", "cvpct"))
##### Hispanic LTBI by Nativity
svyby(~`TB status`, by=~`Nativity`, hispanicData, svymean, 
      na.rm=T, deff=T, vartype=c("se", "cvpct"))

##### Asian with Diabetes LTBI by Nativity
svymean(~`TB status`, asianDataNUSBDiab, 
      na.rm=T, deff=T, vartype=c("se", "cvpct"))

##### Hispanic with Diabetes LTBI by Nativity
svymean(~`TB status`, hispanicDataNUSBDiab, 
        na.rm=T, deff=T, vartype=c("se", "cvpct"))
svymean(~`TB status`, hispanicDataUSBDiab, 
        na.rm=T, deff=T, vartype=c("se", "cvpct"))

###############################################################################
# Generate proportions of diabetes status x TB status 
##### Asian population 
svyby(~`Comorbid`, by=~`Nativity`, denominator=~Nativity, asianData, svymean, 
      na.rm=T, deff=T, vartype=c("se", "cvpct"))

##### Hispanic population 
svyby(~`Comorbid`, by=~`Nativity`, hispanicData, svymean, 
      na.rm=T, deff=T, vartype=c("se", "cvpct"))

###############################################################################
# LTBI prevalence by age group 
##### Total Asian population 
svyby(~`TB status`, by=~`Age group`, asianDataUSB, svymean, 
      na.rm=T, deff=T, vartype=c("se", "cvpct"))

svyby(~`TB status`, by=~`Age group`, asianDataNUSB, svymean, 
      na.rm=T, deff=T, vartype=c("se", "cvpct"))

##### Total Hispanic population 
svyby(~`TB status`, by=~`Age group`, hispanicDataUSB, svymean, 
      na.rm=T, deff=T, vartype=c("se", "cvpct"))

svyby(~`TB status`, by=~`Age group`, hispanicDataNUSB, svymean, 
      na.rm=T, deff=T, vartype=c("se", "cvpct"))

################## LTBI prevalence among those with diabetes ##################
##### Asian population with Diabetes
svyby(~`TB status`, by=~`Age group`, asianDataUSBDiab, svymean, 
      na.rm=T, deff=T, vartype=c("se", "cvpct"))

svyby(~`TB status`, by=~`Age group`, asianDataNUSBDiab, svymean, 
      na.rm=T, deff=T, vartype=c("se", "cvpct"))

##### Hispanic population with Diabetes
svyby(~`TB status`, by=~`Age group`, hispanicDataUSBDiab, svymean, 
      na.rm=T, deff=T, vartype=c("se", "cvpct"))

svyby(~`TB status`, by=~`Age group`, hispanicDataNUSBDiab, svymean, 
      na.rm=T, deff=T, vartype=c("se", "cvpct"))


###############################################################################
# Incorporate age groups for those with both conditions 

##### Asian population 
svyby(~`Comorbid`, by=~`Age group`, asianDataUSB, svymean, 
      na.rm=T, deff=T, vartype=c("se", "cvpct"))

svyby(~`Comorbid`, by=~`Age group`, asianDataNUSB, svymean, 
      na.rm=T, deff=T, vartype=c("se", "cvpct"))

##### Hispanic population 
svyby(~`Comorbid`, by=~`Age group`, hispanicDataUSB, svymean, 
      na.rm=T, deff=T, vartype=c("se", "cvpct"))

svyby(~`Comorbid`, by=~`Age group`, hispanicDataNUSB, svymean, 
      na.rm=T, deff=T, vartype=c("se", "cvpct"))





