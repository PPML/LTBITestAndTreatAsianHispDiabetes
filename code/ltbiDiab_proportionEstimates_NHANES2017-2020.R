###############################################################################

### Make sure to load all dependencies, helper functions, and color palette and 
### set working directory by first running the setupEnvironAndDeps.R file!

###############################################################################
# Download NHANES1720 2017-2020 to temporary file   
# Demographic data
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/P_DEMO.XPT", 
              tf <- tempfile(), mode="wb")

# Diabetes data
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/P_DIQ.XPT", 
              tf2 <- tempfile(), mode="wb")

# Laboratory data - plasma fasting glucose 
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/P_GLU.XPT", 
              tf3 <- tempfile(), mode="wb")

# Create Data Frames From Temporary Files
demographicNHANES1720 <- foreign::read.xport(tf)
diabetesNHANES1720 <- foreign::read.xport(tf2)
glucoseNHANES1720 <- foreign::read.xport(tf3)

###############################################################################
# Retain relevant variables 
demographicNHANES1720 <- demographicNHANES1720[c("SEQN",      # Respondent sequence number
                                         "SDMVPSU",   # Person
                                         "SDMVSTRA",  # Strata
                                         "WTINTPRP",  # Weights
                                         "WTMECPRP",  # Weights  MEC exam weight
                                         "RIDAGEYR",  # Age in years at screening
                                         "RIDRETH3",  # Race/Hispanic origin w/ NH Asian
                                         "DMDBORN4")] #Country of birth

diabetesNHANES1720 <- diabetesNHANES1720[c("SEQN",    # Respondent sequence number
                                   "DIQ010",  # Doctor told you have diabetes
                                   "DIQ160",  # Ever told you have prediabetes
                                   "DIQ280")] # What was your last A1C level

glucoseNHANES1720 <- glucoseNHANES1720[c("SEQN",     # Respondent sequence number
                                 "WTSAFPRP", # Subsample weights
                                 "LBXGLU")]

###############################################################################
# Merge data
mergedData0 <- merge(demographicNHANES1720,
                     diabetesNHANES1720,
                     by = "SEQN", all=TRUE)

mergedData <- merge(mergedData0,
                    glucoseNHANES1720,
                    by = "SEQN", all=TRUE) 

###############################################################################
# Summarize the data into meaningful variables

NHANES1720Data <- mergedData %>% mutate("Age group" = cut(RIDAGEYR, breaks=c(-Inf,4,14,24,34,44,54,64,74,84,94,Inf),
                                                       labels=c("0-4","5-14", "15-24", "25-34", "35-44",
                                                                "45-54","55-64","65-74","75-84","85-94","95p")),
                                    "Race-ethnicity" = case_when(RIDRETH3 == 6 ~"Asian",
                                                                 RIDRETH3 == 1 | RIDRETH3 == 2 ~"Hispanic"),
                                    "Hispanic" = ifelse(RIDRETH3 == 1 | RIDRETH3 == 2, "Yes", "No"),
                                    "Asian" = ifelse(RIDRETH3 == 6, "Yes", "No"),
                                    "Nativity" = case_when(DMDBORN4 == 1 ~ "USB", 
                                                           DMDBORN4 == 2 ~ "NUSB"),
                                    "Told diabetes" = case_when(DIQ010 == 1 ~ "Yes",
                                                                DIQ010 == 2 ~ "No",
                                                                DIQ010 == 3 ~ "Borderline"),
                                    "Told prediabetes" = case_when(DIQ160 == 1 ~ "Yes",
                                                                   DIQ160 == 2 ~ "No"), 
                                    "A1c" = cut(DIQ280, breaks=c(-Inf, 7, Inf),
                                                labels=c("Controlled", "Uncontrolled")),
                                    "Fast glucose" = cut(LBXGLU, breaks=c(-Inf, 125, Inf),
                                                     labels=c("Normal", "High")), 
                                    "Diabetes status" = case_when(`Told diabetes` == "Yes" | `Fast glucose` == "High" ~ "Diabetes",
                                                                  `Told diabetes` == "No" ~ "None", 
                                                                  `Fast glucose` == "Normal" ~ "None")) %>% 
                            dplyr::select("SEQN", "SDMVPSU", "SDMVSTRA", "WTMECPRP", "WTINTPRP", "WTSAFPRP", 
                                          "Age group", "Nativity", "RIDRETH3","Race-ethnicity", "Asian", "Hispanic",
                                          "Told diabetes",
                                          "Told prediabetes", "A1c", "Fast glucose",
                                          "Diabetes status")
                                    
###############################################################################
# Survey weighting 
NHANES1720Data <- svydesign(id      = ~ SDMVPSU,
                            strata  = ~ SDMVSTRA,
                            weights = ~ WTINTPRP,
                            nest    = TRUE,
                            data    = NHANES1720Data)

###############################################################################
# Descriptive Statistics 
# Population by race-ethnicity x nativity 
pop0 <- as.vector(svytotal(~interaction(`Nativity`, `Race-ethnicity`), NHANES1720Data, na.rm = TRUE))/1e6
names(pop0) <- c("Asian NUSB", "Asian USB", 
                 "Hispanic NUSB", "Hispanic USB")
popASC <- c(12.1, 5.53, 20.30, 38.18)
names(popASC) <- names(pop0)

# Plot the population by race-ethnicity x nativity 
plot <- barplot(pop0, ylab = "Population in millions", 
                main = "Population by race-ethnicity and nativity", 
                ylim = c(0,40), 
                col = "lavender")
points(x = plot, y=popASC, pch = 19)

# Percent difference between the ASC and NHANES1720 population sizes
(pop0 - popASC) / pop0 * 100


# Population by race-ethnicity x nativity x age
### Using svytotal will give us SE estimates 
pop1 <- as.data.frame(svyby(~`Race-ethnicity`, ~ Nativity + `Age group`, NHANES1720Data, na = TRUE, svytotal))
totPop <- pop1 %>% melt() %>% 
    mutate("Race-ethnicity" = ifelse(variable %in% c("`Race-ethnicity`Asian", "se.`Race-ethnicity`Asian"), "Asian", "Hispanic"),
           "Estimate" = ifelse(variable %in% c("`Race-ethnicity`Asian", "`Race-ethnicity`Hispanic"), "Point estimate", "Standard error")) %>%
    rename("N" = value) %>% select(!variable) %>% 
    complete(`Race-ethnicity`, Nativity , `Age group`, Estimate, fill = list(N = 0))

# Confirm that diabetes and pre-diabetes are exclusive conditions
svytotal(~interaction(`Told diabetes`, `Told prediabetes`), NHANES1720Data, na.rm = TRUE)
# No overlap -- we can treat these separately;
# Note: if Told diabetes = 1, Told prediabetes is NA by survey design 

#### What are the reported numbers of diabetes from NHANES1720 with NHANES1720 denominators 
svytable(~`Diabetes status`, design = NHANES1720Data) / 1e6
svytable(~interaction(`Diabetes status`, `Race-ethnicity`), design = NHANES1720Data) / 1e6

### Try to run proportions 
# svyby(~`Told diabetes`, ~`Asian`, NHANES1720Data, svyciprop)
svyby(~`Diabetes status`, ~`Asian`, NHANES1720Data, svyciprop)


svyby(~`Told diabetes`, ~`Nativity` + `Asian` + `Age group`, NHANES1720Data, svyciprop)
svyby(~`Told diabetes`, ~`Nativity` + `Asian` + `Age group`, NHANES1720Data, svyciprop)

svyby(~`Told diabetes`, ~`Nativity` + `Race-ethnicity` + `Age group`, NHANES1720Data, svyciprop)


############## ONLY ASIAN POP FOR NEEMA #####################
tmpHispPop <- svytable(~interaction(`Diabetes status`, Hispanic, Nativity, `Age group`), design = NHANES1720Data) %>% 
    prop.table() %>% as_data_frame() #%>% #mutate("Nativity" = ifelse(str_detect(`interaction(`Diabetes status`, Hispanic, Nativity,`Age group`)`,"NUSB") ==TRUE, "NUSB", "USB"))
# tmpHispPop <- tmpHispPop[seq(3,88, by = 4),]
tmpHispPop$Nativity <- rep(c(rep("NUSB",4), rep("USB",4)), 11)
tmpHispPop$`Age group` <- rep(c("0-4","5-14", "15-24", "25-34", "35-44",
                                "45-54","55-64","65-74","75-84","85-94","95p"), each = 8)
tmpHispPop$N <- tmpHispPop$n * totHisp #totHisp is sum of ACS data 
tmpHispPop <- tmpHispPop[seq(1,87,by=2),]

sum((tmpHispPop %>% filter(Nativity == "NUSB"))$N)
sum((tmpHispPop %>% filter(Nativity == "USB"))$N)

############################
totAsian <- sum(population_denominators[["Asian"]]$Population)/1e6

tmpAsianPop <- svytable(~interaction(`Diabetes status`, Asian, Nativity, `Age group`), design = NHANES1720Data) %>% 
    prop.table() %>% as_data_frame() 
tmpAsianPop$Nativity <- rep(c(rep("NUSB",4), rep("USB",4)), 11)
tmpAsianPop$`Age group` <- rep(c("0-4","5-14", "15-24", "25-34", "35-44",
                                "45-54","55-64","65-74","75-84","85-94","95p"), each = 8)
tmpAsianPop$N <- tmpAsianPop$n * totAsian #totAsian is sum of ACS data 
tmpAsianPop <- tmpAsianPop[seq(1,87,by=2),]

sum((tmpAsianPop %>% filter(Nativity == "NUSB"))$N)
sum((tmpAsianPop %>% filter(Nativity == "USB"))$N)


tmpBothPops <- list("Asian" = tmpAsianPop, 
                    "Hispanic" = tmpHispPop)

# bothPops <- list("Asian" = asianPop, 
#                  "Hispanic" = hispanicPop)

###############################################################################
# Save the list with the data for each population
# saveRDS(tmpBothPops, file = "data/TEMP2diabetesPopulationData.rds", version = 2)



