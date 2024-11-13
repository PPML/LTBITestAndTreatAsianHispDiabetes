###############################################################################
### This code will use NHANES survey objects as inputs to
### model that will calculate LTBI prevalence.
###############################################################################

### Make sure to load all dependencies, helper functions, and color palette and
### set working directory by first running the setupEnvironAndDeps.R file!
###############################################################################
setwd("~/TTTAsianHispDiabetes/LTBITestAndTreatAsianHispDiabetes/")

source("code/setupEnvironAndDeps.R")
###############################################################################
###### Regression to estimate IGRA+ fraction from NHANES 2011-2012 data
###### Covariates include: age, nativity, race, and diabetes status
###############################################################################

###############################################################################
##### Import survey object
###############################################################################
# NHANES1112Data <- readRDS(file = "~/Documents/TTT & Diabetes/Data/2011_12_NHANES_survey_obj_filtAge.rds")
# NHANES1112Data <- readRDS(file = "~/Documents/TTT & Diabetes/Data/2011_12_NHANES_survey_obj.rds")
NHANES1112Data <- readRDS(file = "data/2011_12_NHANES_survey_obj_newA1c.rds")

###############################################################################
##### Define covariates
##### These were primarily defined in the survey object, but
##### we need to ensure that they are of the appropriate type
###############################################################################

##### AGE
if(class(NHANES1112Data$variables$`Age group`) != "factor") print("Age group is not a factor.")
levels(NHANES1112Data$variables$`Age group`)

##### NATIVITY
if(class(NHANES1112Data$variables$`Nativity`) != "factor"){
    print("Nativity was not a factor. Attempting to coerce.")
    NHANES1112Data$variables$`Nativity` <- as.factor(NHANES1112Data$variables$`Nativity`)
    if(class(NHANES1112Data$variables$`Nativity`) == "factor") print("Success!")
}

##### RACE/ETHNICITY
if(class(NHANES1112Data$variables$`Race-ethnicity`) != "factor"){
    print("Race-ethnicity was not a factor. Attempting to coerce.")
    NHANES1112Data$variables$`Race-ethnicity` <- as.factor(NHANES1112Data$variables$`Race-ethnicity`)
    if(class(NHANES1112Data$variables$`Race-ethnicity`) == "factor") print("Success!")
}
##### DIABETES STATUS
if(class(NHANES1112Data$variables$`Diabetes status`) != "factor"){
    print("Diabetes status was not a factor. Attempting to coerce.")
    NHANES1112Data$variables$`Diabetes status` <- as.factor(NHANES1112Data$variables$`Diabetes status`)
    if(class(NHANES1112Data$variables$`Diabetes status`) == "factor") print("Success!")
}

##### TB STATUS
if(class(NHANES1112Data$variables$`TB status`) != "factor"){
    print("TB status was not a factor. Attempting to coerce.")
    NHANES1112Data$variables$`TB status` <- as.factor(NHANES1112Data$variables$`TB status`)
    if(class(NHANES1112Data$variables$`TB status`) == "factor") print("Success!")
}

##### IGRA POSITIVE
if(class(NHANES1112Data$variables$`IGRA positive`) != "factor"){
    print("IGRA positivity was not a factor. Attempting to coerce.")
    NHANES1112Data$variables$`IGRA positive` <- as.factor(NHANES1112Data$variables$`IGRA positive`)
    if(class(NHANES1112Data$variables$`IGRA positive`) == "factor") print("Success!")
}

###############################################################################
##### Fit models using svyglm
###############################################################################
##### We fit separate models for the population with and without diabetes as
##### two models will allow us to capture any relationship between diabetes
##### status and the other covariates of interest.
fitIGRA <-svyglm(`IGRA positive`~ `Age group` + Nativity + `Race-ethnicity` + `Diabetes status`,
                 family = quasibinomial,
                 design = NHANES1112Data,
                 na.action = na.omit)

summary(fitIGRA)
##### Predict using fitted model
##### Create additional data for the prediction
new_dat <- as.data.frame(expand.grid(levels(NHANES1112Data$variables$`Age group`), #[c(-1,-10,-11)],
                                     levels(NHANES1112Data$variables$`Nativity`),
                                     levels(NHANES1112Data$variables$`Race-ethnicity`),
                                     levels(NHANES1112Data$variables$`Diabetes status`)))
names(new_dat) <- c("Age group", "Nativity", "Race-ethnicity", "Diabetes status")

new_dat <- new_dat %>%
  filter(`Diabetes status` %in% c("None", "Diabetes"))

##### Predict for new data
pred_link <- predict(fitIGRA, vcov=T, se.fit=T, type="link", newdata=new_dat)
vcv_link  <- vcov(pred_link)
est_link  <- coef(pred_link)
# plot(density(est_link))

##### Draw random samples for igra values
nSim <- 1e5 # no samples
set.seed(1)
sims0 <- mvrnorm(nSim, mu = est_link, Sigma = vcv_link)
igra_prev_sims  <- t(invlgt(sims0))

###############################################################################
### Calculate LTBI prevalence from IGRA positivity
###############################################################################
# igra = ltbi*sens + (1-ltbi) * (1-spec)
#      = ltbi*sens + 1-ltbi-spec+ltbi*spec
#      = 1 + ltbi*(sens+spec-1) - spec
# ltbi = (igra-1+spec)/(sens+spec-1)

# Stout : sens                   spec
# FB      78.9 (69.6 to 90.2)    98.5 (96.1 to 99.8)
# USB     78.0 (65.0 to 91.0)    97.9 (96.0 to 99.4)

##### Simulate sens/spec of IGRA by US/NUS status
sens_par_nusb <- betapar( c(78.9, 69.6, 90.2)/100 )
sens_nusb <- rbeta(shape1=sens_par_nusb[1],shape2=sens_par_nusb[2], n=nSim)

sens_par_usb <- betapar( c(78.0, 65.0, 91.0)/100 )
sens_usb <- rbeta(shape1=sens_par_usb[1],shape2=sens_par_usb[2], n=nSim)

spec_par_nusb <- betapar( c(98.5, 96.1, 99.8)/100 )
spec_nusb <- rbeta(shape1=spec_par_nusb[1],shape2=spec_par_nusb[2], n=nSim)

spec_par_usb <- betapar( c(97.9, 96.0, 99.4)/100 )
spec_usb <- rbeta(shape1=spec_par_usb[1],shape2=spec_par_usb[2], n=nSim)

##### BY RACE-ETHNICITY #######
##### LTBI prevalence Asian USB
# indexAsian <- which(new_dat$`Race-ethnicity`=="Asian")
indexAsianUSB <- c(8:14, 36:42)
ltbiPrevSimAsianUSB <- (igra_prev_sims[indexAsianUSB,] - 1 + spec_usb) / (sens_usb + spec_usb - 1)
tempPrevAsianUSB <- apply(X = ltbiPrevSimAsianUSB, MARGIN = 1, FUN = quantile, probs=c(.025, .50, 0.975))
ltbiPrevAsianUSB <- cbind(new_dat[indexAsianUSB,], t(tempPrevAsianUSB))

##### LTBI prevalence Asian NUSB
# indexAsian <- which(new_dat$`Race-ethnicity`=="Asian")
indexAsianNUSB <- c(1:7, 29:35)
ltbiPrevSimAsianNUSB <- (igra_prev_sims[indexAsianNUSB,] - 1 + spec_nusb) / (sens_nusb + spec_nusb - 1)
tempPrevAsianNUSB <- apply(X = ltbiPrevSimAsianNUSB, MARGIN = 1, FUN = quantile, probs=c(.025, .50, 0.975))
ltbiPrevAsianNUSB <- cbind(new_dat[indexAsianNUSB,], t(tempPrevAsianNUSB))


##### LTBI prevalence Hispanic USB
# indexHispanic <- which(new_dat$`Race-ethnicity`=="Hispanic")
indexHispanicUSB <- c(22:28,50:56)
ltbiPrevSimHispanicUSB <- (igra_prev_sims[indexHispanicUSB,] - 1 + spec_usb) / (sens_usb + spec_usb - 1)
tempPrevHispanicUSB <- apply(X = ltbiPrevSimHispanicUSB, MARGIN = 1, FUN = quantile, probs=c(.025, .50, 0.975))
ltbiPrevHispanicUSB <- cbind(new_dat[indexHispanicUSB,], t(tempPrevHispanicUSB))

##### LTBI prevalence Hispanic NUSB
# indexHispanic <- which(new_dat$`Race-ethnicity`=="Hispanic")
indexHispanicNUSB <- c(15:21, 43:49)
ltbiPrevSimHispanicNUSB <- (igra_prev_sims[indexHispanicNUSB,] - 1 + spec_nusb) / (sens_nusb + spec_nusb - 1)
tempPrevHispanicNUSB <- apply(X = ltbiPrevSimHispanicNUSB, MARGIN = 1, FUN = quantile, probs=c(.025, .50, 0.975))
ltbiPrevHispanicNUSB <- cbind(new_dat[indexHispanicNUSB,], t(tempPrevHispanicNUSB))

##### Create a single dataframe
ltbiPrev <- rbind(ltbiPrevAsianUSB, ltbiPrevAsianNUSB, ltbiPrevHispanicUSB, ltbiPrevHispanicNUSB)

###############################################################################
##### Plot of stratified estimates
###############################################################################
diabCompPLot <-ggplot(data=ltbiPrev %>%
                        filter(`Diabetes status` %in% c("Diabetes", "None"))) +
  geom_point(aes(y=`Age group`, x=`50%`,
                 color=Nativity, shape=`Diabetes status`),
             position=position_dodge(width = 1), size=2.5) +
  facet_grid(cols=vars(`Race-ethnicity`)) + theme_minimal() +
  geom_errorbarh(aes(y=`Age group`, xmax=`97.5%`, xmin=`2.5%`,
                     color=Nativity, shape=`Diabetes status`),
                 position=position_dodge(width = 1)) +
  scale_x_continuous(labels = scales::percent) +
  scale_color_manual(values=safe_colorblind_palette[c(2,4)]) +
  theme(text=element_text(size=18), panel.spacing = unit(2, "lines"),
        legend.position = "bottom") +
  xlab("LTBI prevalence")

# ggsave(filename = "~/Documents/TTT & Diabetes/Plots/LTBIPrevalenceDiabComp11_12_fixedA1c.png",
#        plot = diabCompPLot, width = 8.5, height = 9, units = "in")

###############################################################################
##### We need to estimate total LTBI prevalence among these groups
##### regardless of diabetes status
###############################################################################
fitIGRATot <-svyglm(`IGRA positive`~ `Age group` + Nativity + `Race-ethnicity`,
                 family = binomial,
                 design = NHANES1112Data,
                 na.action = na.omit)

summary(fitIGRATot)
##### Predict using fitted model
##### Create additional data for the prediction
new_datTot <- as.data.frame(expand.grid(levels(NHANES1112Data$variables$`Age group`), #[c(-1,-10,-11)],
                                     levels(NHANES1112Data$variables$`Nativity`),
                                     levels(NHANES1112Data$variables$`Race-ethnicity`)))
names(new_datTot) <- c("Age group", "Nativity", "Race-ethnicity")

##### Predict for new data
pred_linkTot <- predict(fitIGRATot, vcov=T, se.fit=T, type="link", newdata=new_datTot)
vcv_linkTot  <- vcov(pred_linkTot)
est_linkTot  <- coef(pred_linkTot)
# plot(density(est_link))

##### Draw random samples for igra values
nSim <- 1e5 # no samples
set.seed(1)
sims0Tot <- mvrnorm(nSim, mu = est_linkTot, Sigma = vcv_linkTot)
igra_prev_simsTot  <- t(invlgt(sims0Tot))

##### BY RACE-ETHNICITY #######
##### LTBI prevalence Asian
indexAsianTot <- which(new_datTot$`Race-ethnicity`=="Asian")
ltbiPrevSimAsianTot <- (igra_prev_simsTot[indexAsianTot,] - 1 + spec_usb) / (sens_usb + spec_usb - 1)
tempPrevAsianTot <- apply(X = ltbiPrevSimAsianTot, MARGIN = 1, FUN = quantile, probs=c(.025, .50, 0.975))
ltbiPrevAsianTot <- cbind(new_datTot[indexAsianTot,], t(tempPrevAsianTot))

##### LTBI prevalence Hispanic
indexHispanicTot <- which(new_datTot$`Race-ethnicity`=="Hispanic")
ltbiPrevSimHispanicTot <- (igra_prev_simsTot[indexHispanicTot,] - 1 + spec_usb) / (sens_usb + spec_usb - 1)
tempPrevHispanicTot <- apply(X = ltbiPrevSimHispanicTot, MARGIN = 1, FUN = quantile, probs=c(.025, .50, 0.975))
ltbiPrevHispanicTot <- cbind(new_datTot[indexHispanicTot,], t(tempPrevHispanicTot))

##### Create a single dataframe
ltbiPrevTot <- rbind(ltbiPrevAsianTot, ltbiPrevHispanicTot)

###############################################################################
##### Create a single dataframe of general population and diabetes ltbi prev
ltbiPrevTot$`Population` <- ltbiPrevTot$`Diabetes status` <- "General population"
ltbiPrev$`Population` <- ifelse(ltbiPrev$`Diabetes status` == "Diabetes" ~ "Population with diabetes",
                                   ifelse(ltbiPrev$`Diabetes status` == "None"   ~  "Population without diabetes",
                                   ifelse(ltbiPrev$`Diabetes status` %in% c("Gestational", "Unknown") ~ ltbiPrev$`Diabetes status`)))
ltbiPrevAll <- rbind(ltbiPrev, ltbiPrevTot)

###############################################################################
##### Plot of stratified estimates
###############################################################################
ltbiPrevAll$`Race-ethnicity` <- ifelse(ltbiPrevAll$`Race-ethnicity` == "Asian",
                                         "Non-Hispanic Asian population",
                                         "Hispanic population")


ltbiPrev11_12Plot <- ggplot(data=ltbiPrevAll %>%
                                filter(Population %in% c("Population with diabetes",
                                                         "General population"))) +
    geom_point(aes(y=`Age group`, x=`50%`,
                   color=Nativity, shape=Population),
               position=position_dodge(width = 1.1), size=2.5) +
    facet_grid(cols=vars(`Race-ethnicity`)) + theme_minimal() +
    geom_errorbarh(aes(y=`Age group`, xmax=`97.5%`, xmin=`2.5%`,
                       color=Nativity, shape=Population),
                   position=position_dodge(width = 1.1)) +
    scale_x_continuous(labels = scales::percent) +
    scale_color_manual(values=safe_colorblind_palette[c(2,4)]) +
    theme(text=element_text(size=18), panel.spacing = unit(2, "lines"),
          legend.position = "bottom") +
    guides(color = guide_legend(nrow = 2), shape = guide_legend(nrow = 2)) +
    xlab("LTBI prevalence")

# ggsave(filename = "~/Documents/TTT & Diabetes/Plots/LTBIPrevalence11_12_fixedA1c.png",
#        plot = ltbiPrev11_12Plot, width = 8.5, height = 10, units = "in")

### All three populations

ggplot(data=ltbiPrevAll %>%filter(`Population` %in% c("Population with diabetes",
                                                        "Population without diabetes",
                                                         "General population"))) +
    geom_point(aes(y=`Age group`, x=`50%`,
                   color=Nativity, shape=Population),
               position=position_dodge(width = 1.1), size=2.5) +
    facet_grid(cols=vars(`Race-ethnicity`)) + theme_minimal() +
    geom_errorbarh(aes(y=`Age group`, xmax=`97.5%`, xmin=`2.5%`,
                       color=Nativity, shape=Population),
                   position=position_dodge(width = 1.1)) +
    scale_x_continuous(labels = scales::percent) +
    scale_color_manual(values=safe_colorblind_palette[c(2,4)]) +
    theme(text=element_text(size=18), panel.spacing = unit(2, "lines"),
          legend.position = "bottom") +
    guides(color = guide_legend(nrow = 2), shape = guide_legend(nrow = 2)) +
    xlab("LTBI prevalence")


###############################################################################
##### Population estimates
###############################################################################
### Read in the ACS population estimates
pop <- readRDS("~/Documents/TTT & Diabetes/Data/ACS_Pop_Asian_Hisp_AgeGrp.rds")
asianPop <- pop[[1]]
hispanicPop <- pop[[2]]

comboPopulation <- pop[[1]][,1:3]
comboPopulation[,"Population"] <- pop[[1]][,"Population"] + pop[[2]][,"Population"]
###############################################################################
##### Standardized population estimates of LTBI prevalence
###############################################################################
#### ASIAN POPULATION VALUES ####
#################################
#### U.S.-born Asian population with diabetes
ltbiPrevUSBAsianDiab <- ltbiPrevAll %>% filter(`Population` == "Population with diabetes",
                       `Race-ethnicity` == "Non-Hispanic Asian population",
                        Nativity == "USB") %>%
                        select(`Age group`, `2.5%`, `50%`, `97.5%`)

ltbiPrevUSBAsianDiabRate <- data.frame(
                            "Lower bound" =
                                sum(c(ltbiPrevUSBAsianDiab[,"2.5%"],
                                      rep(ltbiPrevUSBAsianDiab[7,"2.5%"],2)) *
                                        comboPopulation[14:22,"Population"]) /
                                sum(comboPopulation[14:22,"Population"]),
                            "Median" =
                                sum(c(ltbiPrevUSBAsianDiab[,"50%"],
                                      rep(ltbiPrevUSBAsianDiab[7,"50%"],2)) *
                                        comboPopulation[14:22,"Population"]) /
                                sum(comboPopulation[14:22,"Population"]),
                            "Upper bound" =
                                sum(c(ltbiPrevUSBAsianDiab[,"97.5%"],
                                      rep(ltbiPrevUSBAsianDiab[7,"97.5%"],2)) *
                                        comboPopulation[14:22,"Population"]) /
                                sum(comboPopulation[14:22,"Population"]),
                            check.names = FALSE)

#### non-U.S.-born Asian population without diabetes
ltbiPrevNUSBAsianDiab <- ltbiPrevAll %>%
                         filter(`Population` == "Population with diabetes",
                                `Race-ethnicity` == "Non-Hispanic Asian population",
                                 Nativity == "NUSB") %>%
                         select(`Age group`, `2.5%`, `50%`, `97.5%`)

ltbiPrevNUSBAsianDiabRate <- data.frame(
                                "Lower bound" =
                                    sum(c(ltbiPrevNUSBAsianDiab[,"2.5%"],
                                          rep(ltbiPrevNUSBAsianDiab[7,"2.5%"],2)) *
                                            comboPopulation[3:11,"Population"]) /
                                    sum(comboPopulation[3:11,"Population"]),
                                "Median" =
                                    sum(c(ltbiPrevNUSBAsianDiab[,"50%"],
                                          rep(ltbiPrevNUSBAsianDiab[7,"50%"],2)) *
                                            comboPopulation[3:11,"Population"]) /
                                    sum(comboPopulation[3:11,"Population"]),
                                "Upper bound" =
                                    sum(c(ltbiPrevNUSBAsianDiab[,"97.5%"],
                                          rep(ltbiPrevNUSBAsianDiab[7,"97.5%"],2)) *
                                            comboPopulation[3:11,"Population"]) /
                                    sum(comboPopulation[3:11,"Population"]),
                                check.names = FALSE)


####################################
#### HISPANIC POPULATION VALUES ####
####################################
#### U.S.-born Hispanic population with diabetes
ltbiPrevUSBHispanicDiab <- ltbiPrevAll %>%
                        filter(`Population` == "Population with diabetes",
                               `Race-ethnicity` == "Hispanic population",
                                Nativity == "USB") %>%
                        select(`Age group`, `2.5%`, `50%`, `97.5%`)

ltbiPrevUSBHispanicDiabRate <- data.frame(
                                "Lower bound" =
                                    sum(c(ltbiPrevUSBHispanicDiab[,"2.5%"],
                                          rep(ltbiPrevUSBHispanicDiab[7,"2.5%"],2)) *
                                            comboPopulation[14:22,"Population"]) /
                                    sum(comboPopulation[14:22,"Population"]),
                                "Median" =
                                    sum(c(ltbiPrevUSBHispanicDiab[,"50%"],
                                          rep(ltbiPrevUSBHispanicDiab[7,"50%"],2)) *
                                            comboPopulation[14:22,"Population"]) /
                                    sum(comboPopulation[14:22,"Population"]),
                                "Upper bound" =
                                    sum(c(ltbiPrevUSBHispanicDiab[,"97.5%"],
                                          rep(ltbiPrevUSBHispanicDiab[7,"97.5%"],2)) *
                                            comboPopulation[14:22,"Population"]) /
                                    sum(comboPopulation[14:22,"Population"]),
                                check.names = FALSE)

#### non-U.S.-born Hispanic population without diabetes
ltbiPrevNUSBHispanicDiab <- ltbiPrevAll %>%
    filter(`Population` == "Population with diabetes",
           `Race-ethnicity` == "Hispanic population",
           Nativity == "NUSB") %>%
    select(`Age group`, `2.5%`, `50%`, `97.5%`)

ltbiPrevNUSBHispanicDiabRate <- data.frame(
                                "Lower bound" =
                                sum(c(ltbiPrevNUSBHispanicDiab[,"2.5%"],
                                   rep(ltbiPrevNUSBHispanicDiab[7,"2.5%"],2)) *
                                     comboPopulation[3:11,"Population"]) /
                                sum(comboPopulation[3:11,"Population"]),
                                "Median" =
                                 sum(c(ltbiPrevNUSBHispanicDiab[,"50%"],
                                       rep(ltbiPrevNUSBHispanicDiab[7,"50%"],2)) *
                                 comboPopulation[3:11,"Population"]) /
                                 sum(comboPopulation[3:11,"Population"]),
                                "Upper bound" =
                                 sum(c(ltbiPrevNUSBHispanicDiab[,"97.5%"],
                                       rep(ltbiPrevNUSBHispanicDiab[7,"97.5%"],2)) *
                                 comboPopulation[3:11,"Population"]) /
                                 sum(comboPopulation[3:11,"Population"]),
                                check.names = FALSE)


###############################################################################
##### Create rate ratios using Ekramnia et al 2024 estimates
###############################################################################
nusbLtbiPrev <- c(.14, .16, .19)
usbLtbiPrev  <- c(.024, .03, .04)

rateRatios <- rbind(
                # "USB non-Hispanic Asian with Diabetes"
                    ltbiPrevUSBAsianDiabRate / usbLtbiPrev,
                # "NUSB non-Hispanic Asian with Diabetes"
                    ltbiPrevNUSBAsianDiabRate / nusbLtbiPrev,
                # "USB Hispanic with Diabetes"
                    ltbiPrevUSBHispanicDiabRate / usbLtbiPrev,
                # "NUSB Hispanic with Diabetes"
                    ltbiPrevNUSBHispanicDiabRate / nusbLtbiPrev)

rateRatios$Nativity <- rep(c("USB", "NUSB"),2)
rateRatios$`Race-ethnicity` <- rep(c("non-Hispanic Asian", "Hispanic"), each = 2)

##### Save for use in the MITUS code
# saveRDS(rateRatios,
#         file = "~/Documents/TTT & Diabetes/Data/rateRatiosLTBI.rds",
#         version = 2)
