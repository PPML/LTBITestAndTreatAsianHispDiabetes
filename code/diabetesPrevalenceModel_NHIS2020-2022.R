### This code will use NHIS 2020-2022 survey objects as inputs to 
### model that will calculate diabetes prevalence.
###############################################################################

### Make sure to load all dependencies, helper functions, and color palette and 
### set working directory by first running the setupEnvironAndDeps.R file!

###############################################################################
###### Regression to estimate diabetes fraction from NHIS 2020-2022
###### Covariates include: age, nativity, race, and diabetes status
###############################################################################

###############################################################################
##### Import survey objects 
###############################################################################
NHIS2020_2022Data <- readRDS(file = "data/2020_2022_NHIS_survey_obj.rds")
###############################################################################
##### Define covariates 
##### These were primarily defined in the survey object, but 
##### we need to ensure that they are of the appropriate type
###############################################################################

##### AGE
if(class(NHIS2020_2022Data$variables$`Age group`) != "factor"){
    print("Age group is not a factor.")
    NHIS2020_2022Data$variables$`Age group` <- as.factor(NHIS2020_2022Data$variables$`Age group`)
    if(class(NHIS2020_2022Data$variables$`Age group`) == "factor") print("Success!")
} 
levels(NHIS2020_2022Data$variables$`Age group`)

##### NATIVITY 
if(class(NHIS2020_2022Data$variables$`Nativity`) != "factor"){
    print("Nativity was not a factor. Attempting to coerce.")
    NHIS2020_2022Data$variables$`Nativity` <- as.factor(NHIS2020_2022Data$variables$`Nativity`)
    if(class(NHIS2020_2022Data$variables$`Nativity`) == "factor") print("Success!")
}

##### RACE/ETHNICITY 
if(class(NHIS2020_2022Data$variables$`Race-ethnicity`) != "factor"){
    print("Race-ethnicity was not a factor. Attempting to coerce.")
    NHIS2020_2022Data$variables$`Race-ethnicity` <- as.factor(NHIS2020_2022Data$variables$`Race-ethnicity`)
    if(class(NHIS2020_2022Data$variables$`Race-ethnicity`) == "factor") print("Success!")
}
##### DIABETES STATUS 
if(class(NHIS2020_2022Data$variables$`Diabetes status`) != "factor"){
    print("Diabetes status was not a factor. Attempting to coerce.")
    NHIS2020_2022Data$variables$`Diabetes status` <- as.factor(NHIS2020_2022Data$variables$`Diabetes status`)
    if(class(NHIS2020_2022Data$variables$`Diabetes status`) == "factor") print("Success!")
}
levels(NHIS2020_2022Data$variables$`Diabetes status`)
NHIS2020_2022Data$variables$`Diabetes status` <- relevel(NHIS2020_2022Data$variables$`Diabetes status`, ref = "None")

##### Asian USB Model #####
NHIS2020_2022AsianUSBData <- readRDS(file = "data/2020_2022_NHIS_AsianUSB_survey_obj.rds")
NHIS2020_2022AsianNUSBData <- readRDS(file = "data/2020_2022_NHIS_AsianNUSB_survey_obj.rds")
NHIS2020_2022HispanicUSBData <- readRDS(file = "data/2020_2022_NHIS_HispanicUSB_survey_obj.rds")
NHIS2020_2022HispanicNUSBData <- readRDS(file = "data/2020_2022_NHIS_HispanicNUSB_survey_obj.rds")

###############################################################################
##### Define covariates 
##### These were primarily defined in the survey object, but 
##### we need to ensure that they are of the appropriate type
###############################################################################
##### AGE
if(class(NHIS2020_2022AsianUSBData$variables$`Age group`) != "factor"){
    print("Age group is not a factor.")
    NHIS2020_2022AsianUSBData$variables$`Age group` <- as.factor(NHIS2020_2022AsianUSBData$variables$`Age group`)
    if(class(NHIS2020_2022AsianUSBData$variables$`Age group`) == "factor") print("Success!")
} 
# levels(NHIS2020_2022AsianUSBData$variables$`Age group`)

##### NATIVITY 
if(class(NHIS2020_2022AsianUSBData$variables$`Nativity`) != "factor"){
    print("Nativity was not a factor. Attempting to coerce.")
    NHIS2020_2022AsianUSBData$variables$`Nativity` <- as.factor(NHIS2020_2022AsianUSBData$variables$`Nativity`)
    if(class(NHIS2020_2022AsianUSBData$variables$`Nativity`) == "factor") print("Success!")
}

##### RACE/ETHNICITY 
if(class(NHIS2020_2022AsianUSBData$variables$`Race-ethnicity`) != "factor"){
    print("Race-ethnicity was not a factor. Attempting to coerce.")
    NHIS2020_2022AsianUSBData$variables$`Race-ethnicity` <- as.factor(NHIS2020_2022AsianUSBData$variables$`Race-ethnicity`)
    if(class(NHIS2020_2022AsianUSBData$variables$`Race-ethnicity`) == "factor") print("Success!")
}
##### DIABETES STATUS 
if(class(NHIS2020_2022AsianUSBData$variables$`Diabetes status`) != "factor"){
    print("Diabetes status was not a factor. Attempting to coerce.")
    NHIS2020_2022AsianUSBData$variables$`Diabetes status` <- as.factor(NHIS2020_2022AsianUSBData$variables$`Diabetes status`)
    if(class(NHIS2020_2022AsianUSBData$variables$`Diabetes status`) == "factor") print("Success!")
}
levels(NHIS2020_2022AsianUSBData$variables$`Diabetes status`)
NHIS2020_2022AsianUSBData$variables$`Diabetes status` <- relevel(NHIS2020_2022AsianUSBData$variables$`Diabetes status`, ref = "None")
############ ASIAN NUSB ############ ############ ############ ############ 
##### AGE
if(class(NHIS2020_2022AsianNUSBData$variables$`Age group`) != "factor"){
    print("Age group is not a factor.")
    NHIS2020_2022AsianNUSBData$variables$`Age group` <- as.factor(NHIS2020_2022AsianNUSBData$variables$`Age group`)
    if(class(NHIS2020_2022AsianNUSBData$variables$`Age group`) == "factor") print("Success!")
} 
# levels(NHIS2020_2022AsianNUSBData$variables$`Age group`)

##### NATIVITY 
if(class(NHIS2020_2022AsianNUSBData$variables$`Nativity`) != "factor"){
    print("Nativity was not a factor. Attempting to coerce.")
    NHIS2020_2022AsianNUSBData$variables$`Nativity` <- as.factor(NHIS2020_2022AsianNUSBData$variables$`Nativity`)
    if(class(NHIS2020_2022AsianNUSBData$variables$`Nativity`) == "factor") print("Success!")
}

##### RACE/ETHNICITY 
if(class(NHIS2020_2022AsianNUSBData$variables$`Race-ethnicity`) != "factor"){
    print("Race-ethnicity was not a factor. Attempting to coerce.")
    NHIS2020_2022AsianNUSBData$variables$`Race-ethnicity` <- as.factor(NHIS2020_2022AsianNUSBData$variables$`Race-ethnicity`)
    if(class(NHIS2020_2022AsianNUSBData$variables$`Race-ethnicity`) == "factor") print("Success!")
}
##### DIABETES STATUS 
if(class(NHIS2020_2022AsianNUSBData$variables$`Diabetes status`) != "factor"){
    print("Diabetes status was not a factor. Attempting to coerce.")
    NHIS2020_2022AsianNUSBData$variables$`Diabetes status` <- as.factor(NHIS2020_2022AsianNUSBData$variables$`Diabetes status`)
    if(class(NHIS2020_2022AsianNUSBData$variables$`Diabetes status`) == "factor") print("Success!")
}
levels(NHIS2020_2022AsianNUSBData$variables$`Diabetes status`)
NHIS2020_2022AsianNUSBData$variables$`Diabetes status` <- relevel(NHIS2020_2022AsianNUSBData$variables$`Diabetes status`, ref = "None")

############ HISPANIC USB ############ ############ ############ ############ 
##### AGE
if(class(NHIS2020_2022HispanicUSBData$variables$`Age group`) != "factor"){
    print("Age group is not a factor.")
    NHIS2020_2022HispanicUSBData$variables$`Age group` <- as.factor(NHIS2020_2022HispanicUSBData$variables$`Age group`)
    if(class(NHIS2020_2022HispanicUSBData$variables$`Age group`) == "factor") print("Success!")
} 
# levels(NHIS2020_2022HispanicUSBData$variables$`Age group`)

##### NATIVITY 
if(class(NHIS2020_2022HispanicUSBData$variables$`Nativity`) != "factor"){
    print("Nativity was not a factor. Attempting to coerce.")
    NHIS2020_2022HispanicUSBData$variables$`Nativity` <- as.factor(NHIS2020_2022HispanicUSBData$variables$`Nativity`)
    if(class(NHIS2020_2022HispanicUSBData$variables$`Nativity`) == "factor") print("Success!")
}

##### RACE/ETHNICITY 
if(class(NHIS2020_2022HispanicUSBData$variables$`Race-ethnicity`) != "factor"){
    print("Race-ethnicity was not a factor. Attempting to coerce.")
    NHIS2020_2022HispanicUSBData$variables$`Race-ethnicity` <- as.factor(NHIS2020_2022HispanicUSBData$variables$`Race-ethnicity`)
    if(class(NHIS2020_2022HispanicUSBData$variables$`Race-ethnicity`) == "factor") print("Success!")
}
##### DIABETES STATUS 
if(class(NHIS2020_2022HispanicUSBData$variables$`Diabetes status`) != "factor"){
    print("Diabetes status was not a factor. Attempting to coerce.")
    NHIS2020_2022HispanicUSBData$variables$`Diabetes status` <- as.factor(NHIS2020_2022HispanicUSBData$variables$`Diabetes status`)
    if(class(NHIS2020_2022HispanicUSBData$variables$`Diabetes status`) == "factor") print("Success!")
}
levels(NHIS2020_2022HispanicUSBData$variables$`Diabetes status`)
NHIS2020_2022HispanicUSBData$variables$`Diabetes status` <- relevel(NHIS2020_2022HispanicUSBData$variables$`Diabetes status`, ref = "None")
############ Hispanic NUSB ############ ############ ############ ############ 
##### AGE
if(class(NHIS2020_2022HispanicNUSBData$variables$`Age group`) != "factor"){
    print("Age group is not a factor.")
    NHIS2020_2022HispanicNUSBData$variables$`Age group` <- as.factor(NHIS2020_2022HispanicNUSBData$variables$`Age group`)
    if(class(NHIS2020_2022HispanicNUSBData$variables$`Age group`) == "factor") print("Success!")
} 
# levels(NHIS2020_2022HispanicNUSBData$variables$`Age group`)

##### NATIVITY 
if(class(NHIS2020_2022HispanicNUSBData$variables$`Nativity`) != "factor"){
    print("Nativity was not a factor. Attempting to coerce.")
    NHIS2020_2022HispanicNUSBData$variables$`Nativity` <- as.factor(NHIS2020_2022HispanicNUSBData$variables$`Nativity`)
    if(class(NHIS2020_2022HispanicNUSBData$variables$`Nativity`) == "factor") print("Success!")
}

##### RACE/ETHNICITY 
if(class(NHIS2020_2022HispanicNUSBData$variables$`Race-ethnicity`) != "factor"){
    print("Race-ethnicity was not a factor. Attempting to coerce.")
    NHIS2020_2022HispanicNUSBData$variables$`Race-ethnicity` <- as.factor(NHIS2020_2022HispanicNUSBData$variables$`Race-ethnicity`)
    if(class(NHIS2020_2022HispanicNUSBData$variables$`Race-ethnicity`) == "factor") print("Success!")
}
##### DIABETES STATUS 
if(class(NHIS2020_2022HispanicNUSBData$variables$`Diabetes status`) != "factor"){
    print("Diabetes status was not a factor. Attempting to coerce.")
    NHIS2020_2022HispanicNUSBData$variables$`Diabetes status` <- as.factor(NHIS2020_2022HispanicNUSBData$variables$`Diabetes status`)
    if(class(NHIS2020_2022HispanicNUSBData$variables$`Diabetes status`) == "factor") print("Success!")
}
levels(NHIS2020_2022HispanicNUSBData$variables$`Diabetes status`)
NHIS2020_2022HispanicNUSBData$variables$`Diabetes status` <- relevel(NHIS2020_2022HispanicNUSBData$variables$`Diabetes status`, ref = "None")
###############################################################################
##### Fit models using svyglm
###############################################################################
fitDiab20_22 <-svyglm(`Diabetes status`~ `Age group` + Nativity + `Race-ethnicity`,
                 family = quasibinomial, 
                 design = NHIS2020_2022Data, 
                 na.action = na.omit) 

summary(fitDiab20_22)

##### Predict using fitted model 
##### Create additional data for the prediction
new_dat <- as.data.frame(expand.grid(levels(NHIS2020_2022Data$variables$`Age group`),
                                     levels(NHIS2020_2022Data$variables$`Nativity`),
                                     levels(NHIS2020_2022Data$variables$`Race-ethnicity`)))
names(new_dat) <- c("Age group", "Nativity", "Race-ethnicity")

##### Predict for new data
pred_link20_22 <- predict(fitDiab20_22, vcov=T, se.fit=T, type="link", newdata=new_dat)
# pred20_22 <- predict(fitDiab20_22, vcov=T, se.fit=T, type = "response", newdata=new_dat)
vcv_link20_22  <- vcov(pred_link20_22)
est_link20_22  <- coef(pred_link20_22)
# plot(density(est_link))

##### Draw random samples for diabetes prevalence values
nSim <- 1e5 # no samples
set.seed(1)
sims020_22 <- mvrnorm(nSim, mu = est_link20_22, Sigma = vcv_link20_22)
diab_prev_sims20_22  <- t(invlgt(sims020_22))

##### Critical values of diabetes prevalence
tempPrev20_22 <- apply(X = diab_prev_sims20_22, MARGIN = 1, FUN = quantile, probs=c(.025, .50, 0.975))
diabPrev20_22 <- cbind(new_dat, t(tempPrev20_22))

diabPrev20_22$`Race-ethnicity` <- ifelse(diabPrev20_22$`Race-ethnicity` == "Asian", 
                                         "Non-Hispanic Asian population",
                                         "Hispanic population")

diabPrev20_22Plot <- 
    ggplot(data=diabPrev20_22) + geom_point(aes(y=`Age group`, x=`50%`, color=Nativity), 
                                            position=position_dodge(width = .75), size=2.5) + 
    facet_grid(cols=vars(`Race-ethnicity`)) + theme_minimal() + 
    geom_errorbarh(aes(y=`Age group`, xmax=`97.5%`, xmin=`2.5%`,  color=Nativity), 
                   position=position_dodge(width = .75)) +
    scale_x_continuous(labels = scales::percent) + 
    scale_color_manual(values=safe_colorblind_palette[c(2,4)]) +
    theme(text=element_text(size=18), panel.spacing = unit(2, "lines"), 
          legend.position = "bottom") + 
    xlab("Diabetes prevalence")

###############################################################################
##### Stratified models 
###############################################################################
##### Asian USB
fitDiabAsianUSB20_22 <-svyglm(`Diabetes status`~ `Age group`,
                      family = quasibinomial, 
                      design = NHIS2020_2022AsianUSBData, 
                      na.action = na.omit) 

summary(fitDiabAsianUSB20_22)
##### Predict using fitted model 
##### Create additional data for the prediction
new_dat <- data.frame("Age group" = levels(NHIS2020_2022Data$variables$`Age group`), 
                      check.names = FALSE)
# names(new_dat) <- c("Age group")

##### Predict for new data
pred_linkAsianUSB20_22 <- predict(fitDiabAsianUSB20_22, vcov=T, 
                                  se.fit=T, type="link", newdata=new_dat)
# pred20_22 <- predict(fitDiab20_22, vcov=T, se.fit=T, type = "response", newdata=new_dat)
vcv_linkAsianUSB20_22  <- vcov(pred_linkAsianUSB20_22)
est_linkAsianUSB20_22  <- coef(pred_linkAsianUSB20_22)
# plot(density(est_link))

##### Draw random samples for diabetes prevalence values
nSim <- 1e5 # no samples
set.seed(1)
sims0AsianUSB20_22 <- mvrnorm(nSim, mu = est_linkAsianUSB20_22, Sigma = vcv_linkAsianUSB20_22)
diab_prev_simsAsianUSB20_22  <- t(invlgt(sims0AsianUSB20_22))

##### Critical values of diabetes prevalence
tempPrevAsianUSB20_22 <- apply(X = diab_prev_simsAsianUSB20_22, MARGIN = 1, FUN = quantile, probs=c(.025, .50, 0.975))
diabPrevAsianUSB20_22 <- cbind(new_dat, t(tempPrevAsianUSB20_22))

##### Asian NUSB
fitDiabAsianNUSB20_22 <-svyglm(`Diabetes status`~ `Age group`,
                              family = quasibinomial, 
                              design = NHIS2020_2022AsianNUSBData, 
                              na.action = na.omit) 

summary(fitDiabAsianNUSB20_22)
##### Predict using fitted model 
##### Create additional data for the prediction
new_dat <- data.frame("Age group" = levels(NHIS2020_2022Data$variables$`Age group`), 
                      check.names = FALSE)
# names(new_dat) <- c("Age group")

##### Predict for new data
pred_linkAsianNUSB20_22 <- predict(fitDiabAsianNUSB20_22, vcov=T, 
                                  se.fit=T, type="link", newdata=new_dat)
# pred20_22 <- predict(fitDiab20_22, vcov=T, se.fit=T, type = "response", newdata=new_dat)
vcv_linkAsianNUSB20_22  <- vcov(pred_linkAsianNUSB20_22)
est_linkAsianNUSB20_22  <- coef(pred_linkAsianNUSB20_22)
# plot(density(est_link))

##### Draw random samples for diabetes prevalence values
nSim <- 1e5 # no samples
set.seed(1)
sims0AsianNUSB20_22 <- mvrnorm(nSim, mu = est_linkAsianNUSB20_22, Sigma = vcv_linkAsianNUSB20_22)
diab_prev_simsAsianNUSB20_22  <- t(invlgt(sims0AsianNUSB20_22))

##### Critical values of diabetes prevalence
tempPrevAsianNUSB20_22 <- apply(X = diab_prev_simsAsianNUSB20_22, MARGIN = 1, FUN = quantile, probs=c(.025, .50, 0.975))
diabPrevAsianNUSB20_22 <- cbind(new_dat, t(tempPrevAsianNUSB20_22))

##### Hispanic USB
fitDiabHispanicUSB20_22 <-svyglm(`Diabetes status`~ `Age group`,
                              family = quasibinomial, 
                              design = NHIS2020_2022HispanicUSBData, 
                              na.action = na.omit) 

summary(fitDiabHispanicUSB20_22)
##### Predict using fitted model 
##### Create additional data for the prediction
new_dat <- data.frame("Age group" = levels(NHIS2020_2022Data$variables$`Age group`), 
                      check.names = FALSE)
# names(new_dat) <- c("Age group")

##### Predict for new data
pred_linkHispanicUSB20_22 <- predict(fitDiabHispanicUSB20_22, vcov=T, 
                                  se.fit=T, type="link", newdata=new_dat)
# pred20_22 <- predict(fitDiab20_22, vcov=T, se.fit=T, type = "response", newdata=new_dat)
vcv_linkHispanicUSB20_22  <- vcov(pred_linkHispanicUSB20_22)
est_linkHispanicUSB20_22  <- coef(pred_linkHispanicUSB20_22)
# plot(density(est_link))

##### Draw random samples for diabetes prevalence values
nSim <- 1e5 # no samples
set.seed(1)
sims0HispanicUSB20_22 <- mvrnorm(nSim, mu = est_linkHispanicUSB20_22, Sigma = vcv_linkHispanicUSB20_22)
diab_prev_simsHispanicUSB20_22  <- t(invlgt(sims0HispanicUSB20_22))

##### Critical values of diabetes prevalence
tempPrevHispanicUSB20_22 <- apply(X = diab_prev_simsHispanicUSB20_22, MARGIN = 1, FUN = quantile, probs=c(.025, .50, 0.975))
diabPrevHispanicUSB20_22 <- cbind(new_dat, t(tempPrevHispanicUSB20_22))

##### Hispanic NUSB
fitDiabHispanicNUSB20_22 <-svyglm(`Diabetes status`~ `Age group`,
                               family = quasibinomial, 
                               design = NHIS2020_2022HispanicNUSBData, 
                               na.action = na.omit) 

summary(fitDiabHispanicNUSB20_22)
##### Predict using fitted model 
##### Create additional data for the prediction
new_dat <- data.frame("Age group" = levels(NHIS2020_2022Data$variables$`Age group`), 
                      check.names = FALSE)
# names(new_dat) <- c("Age group")

##### Predict for new data
pred_linkHispanicNUSB20_22 <- predict(fitDiabHispanicNUSB20_22, vcov=T, 
                                   se.fit=T, type="link", newdata=new_dat)
# pred20_22 <- predict(fitDiab20_22, vcov=T, se.fit=T, type = "response", newdata=new_dat)
vcv_linkHispanicNUSB20_22  <- vcov(pred_linkHispanicNUSB20_22)
est_linkHispanicNUSB20_22  <- coef(pred_linkHispanicNUSB20_22)
# plot(density(est_link))

##### Draw random samples for diabetes prevalence values
nSim <- 1e5 # no samples
set.seed(1)
sims0HispanicNUSB20_22 <- mvrnorm(nSim, mu = est_linkHispanicNUSB20_22, Sigma = vcv_linkHispanicNUSB20_22)
diab_prev_simsHispanicNUSB20_22  <- t(invlgt(sims0HispanicNUSB20_22))

##### Critical values of diabetes prevalence
tempPrevHispanicNUSB20_22 <- apply(X = diab_prev_simsHispanicNUSB20_22, MARGIN = 1, FUN = quantile, probs=c(.025, .50, 0.975))
diabPrevHispanicNUSB20_22 <- cbind(new_dat, t(tempPrevHispanicNUSB20_22))

##### Create new dataframe of all the values 
diabPrevAll20_22 <- data.frame("Race-ethnicity" = c(rep("Non-Hispanic Asian population", 20),
                                                    rep("Hispanic population", 20)), 
                               "Nativity" = rep(rep(c("USB", "NUSB"), each = 10), 2),
                               check.names = FALSE)

diabPrevAll20_22 <- cbind(diabPrevAll20_22, rbind(diabPrevAsianUSB20_22, 
                                                  diabPrevAsianNUSB20_22, 
                                                  diabPrevHispanicUSB20_22, 
                                                  diabPrevHispanicNUSB20_22))

diabPrevAll20_22$`Age group` <- factor(diabPrevAll20_22$`Age group`, 
                                       levels = levels(NHIS2020_2022AsianNUSBData$variables$`Age group`)
)

diabPrev20_22AllPlot <- 
    ggplot(data=diabPrevAll20_22) + geom_point(aes(y=`Age group`, x=`50%`, color=Nativity), 
                                            position=position_dodge(width = .75), size=2.5) + 
    facet_grid(cols=vars(`Race-ethnicity`)) + theme_minimal() + 
    geom_errorbarh(aes(y=`Age group`, xmax=`97.5%`, xmin=`2.5%`,  color=Nativity), 
                   position=position_dodge(width = .75)) +
    scale_x_continuous(labels = scales::percent) + 
    scale_color_manual(values=safe_colorblind_palette[c(2,4)]) +
    theme(text=element_text(size=18), panel.spacing = unit(2, "lines"), 
          legend.position = "bottom") + 
    xlab("Diabetes prevalence")

ggsave(filename = "plots/DiabetesPrevalenceAll20_22.png",
       plot = diabPrev20_22AllPlot, width = 8.5, height = 7, units = "in")

diabPrev20_22 <- diabPrevAll20_22 
###############################################################################
##### Population estimates 
###############################################################################
### Read in the ACS population estimates
pop <- readRDS("data/ACS_Pop_Asian_Hisp_AgeGrp.rds")
asianPop <- pop[[1]]
hispanicPop <- pop[[2]]

comboPopulation <- pop[[1]][,1:3]
comboPopulation[,"Population"] <- pop[[1]][,"Population"] + pop[[2]][,"Population"]
###
sum(c(diabPrev20_22[1:10, "50%"],diabPrev20_22[10,"50%"])*comboPopulation[1:11,"Population"]) / sum(comboPopulation[1:11,"Population"])
sum(c(diabPrev20_22[11:20, "50%"],diabPrev20_22[20,"50%"])*comboPopulation[12:22,"Population"]) / sum(comboPopulation[12:22,"Population"])

sum(c(diabPrev20_22[21:30, "50%"],diabPrev20_22[30,"50%"])*comboPopulation[1:11,"Population"]) / sum(comboPopulation[1:11,"Population"])
sum(c(diabPrev20_22[31:40, "50%"],diabPrev20_22[40,"50%"])*comboPopulation[12:22,"Population"]) / sum(comboPopulation[12:22,"Population"])
###############################################################################
##### Check the total number of diabetics in each population 
###############################################################################
asianPop$`Age group` <- as.factor(asianPop$`Age group`)

diabPopAsian <- full_join(asianPop %>% mutate(`Age group` = ifelse(`Age group` %in% c("85-94", "95p"), "85p", as.character(`Age group`))),
                     diabPrev20_22 %>% filter(`Race-ethnicity` == "Non-Hispanic Asian population"), 
                               by = c("Age group", "Nativity")) %>% 
    mutate("Population with diabetes" = Population * `50%`, 
                  "Lower bound" = Population * `2.5%`,
                  "Upper bound" = Population * `97.5%`)


sum(diabPopAsian$`Population with diabetes`)/1e6
sum(diabPopAsian$`Population with diabetes`)/sum(diabPopAsian$`Population`)*100


### by nativity
### NUSB
sum(diabPopAsian$`Population with diabetes`[1:11])/1e6
### Percentage
sum(diabPopAsian$`Population with diabetes`[1:11]) / sum(diabPopAsian$`Population with diabetes`) *100
### USB
sum(diabPopAsian$`Population with diabetes`[12:22])/1e6 
sum(diabPopAsian$`Population with diabetes`[12:22]) / sum(diabPopAsian$`Population with diabetes`) *100


### by age 
### 0-44
sum(diabPopAsian$`Population with diabetes`[c(1:5, 12:16)])/sum(diabPopAsian$`Population`[c(1:5, 12:16)])*100
### 15-44
sum(diabPopAsian$`Population with diabetes`[c(3:5, 14:16)])/sum(diabPopAsian$`Population`[c(3:5, 14:16)])*100
### 45-64
sum(diabPopAsian$`Population with diabetes`[c(6,7,17,18)])/sum(diabPopAsian$`Population`[c(6,7,17,18)])*100
### 65-74
sum(diabPopAsian$`Population with diabetes`[c(8,19)])/sum(diabPopAsian$`Population`[c(8,19)])*100

###############################
##### HISPANIC POPULATION #####
###############################
hispanicPop$`Age group` <- as.factor(hispanicPop$`Age group`)

diabPopHispanic <- full_join(hispanicPop %>% mutate(`Age group` = ifelse(`Age group` %in% c("85-94", "95p"), "85p", as.character(`Age group`))),
                          diabPrev20_22 %>% filter(`Race-ethnicity` == "Hispanic population"), 
                          by = c("Age group", "Nativity")) %>% 
    mutate("Population with diabetes" = Population * `50%`, 
           "Lower bound" = Population * `2.5%`,
           "Upper bound" = Population * `97.5%`)

sum(diabPopHispanic$`Population with diabetes`)/1e6
sum(diabPopHispanic$`Population with diabetes`)/sum(diabPopHispanic$`Population`)*100

### by nativity
### NUSB
sum(diabPopHispanic$`Population with diabetes`[1:11])/1e6
### USB
sum(diabPopHispanic$`Population with diabetes`[12:22])/1e6

### by age 
### 0-44
sum(diabPopHispanic$`Population with diabetes`[c(1:5, 12:16)])/sum(diabPopHispanic$`Population`[c(1:5, 12:16)])*100
### 15-44
sum(diabPopHispanic$`Population with diabetes`[c(3:5, 14:16)])/sum(diabPopHispanic$`Population`[c(3:5, 14:16)])*100
### 45-64
sum(diabPopHispanic$`Population with diabetes`[c(6,7,17,18)])/sum(diabPopHispanic$`Population`[c(6,7,17,18)])*100
### 65-74
sum(diabPopHispanic$`Population with diabetes`[c(8,19)])/sum(diabPopHispanic$`Population`[c(8,19)])*100

##### Save this as input for the MITUS model run ##### 
bothDiabPops <- list("Asian" = diabPopAsian, 
                     "Hispanic" = diabPopHispanic)
saveRDS(bothDiabPops, file = "data/diabetesPopulations.rds", version = 2)

