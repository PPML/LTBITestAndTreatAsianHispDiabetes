### This code will use NHIS 2020-2022 survey objects as inputs to
### model that will calculate diabetes prevalence.
###############################################################################

### Make sure to load all dependencies, helper functions, and color palette and
### set working directory by first running the setupEnvironAndDeps.R file!
###############################################################################
##### Define working directory
###############################################################################
setwd("~/TTTAsianHispDiabetes/LTBITestAndTreatAsianHispDiabetes/")
source("code/setupEnvironAndDeps.R")

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


###############################################################################
##### Fit models using svyglm
###############################################################################
fitDiab20_22 <-svyglm(`Diabetes status`~ `Age group` + Nativity + `Race-ethnicity`+
                      `Race-ethnicity`:Nativity + `Race-ethnicity`:`Age group` + `Age group`:Nativity +
                        `Race-ethnicity`:Nativity:`Age group`,
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

# diabPrev20_22[which(diabPrev20_22$`Age group` == "85p"), "Age group"] <- "85+"
diabPrev20_22$`Age group` <- revalue(diabPrev20_22$`Age group`, c("85p" = "85+") )

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

ggsave(filename = "plots/DiabetesPrevalenceAll20_22.png",
       plot = diabPrev20_22Plot, width = 8.5, height = 10, units = "in")


### Read in the ACS population estimates
pop <- readRDS("data/ACS_Pop_Asian_Hisp_AgeGrp.rds")
asianPop <- pop[[1]]
hispanicPop <- pop[[2]]

##############################################################################
##### Check the total number of diabetics in each population
###############################################################################
asianPop$`Age group` <- as.factor(asianPop$`Age group`)

diabPopAsian <- full_join(asianPop %>% mutate(`Age group` = ifelse(`Age group` %in% c("85-94", "95p"), "85p", as.character(`Age group`))),
                          diabPrev20_22 %>% filter(`Race-ethnicity` == "Non-Hispanic Asian population"),
                          by = c("Age group", "Nativity")) %>%
  mutate("Population with diabetes" = Population * `50%`,
         "Lower bound" = Population * `2.5%`,
         "Upper bound" = Population * `97.5%`)

### Total Asian Population with Diabetes (in millions)
sum(diabPopAsian$`Population with diabetes`)/1e6

### Percentage of Asian Population with Diabetes
sum(diabPopAsian$`Population with diabetes`)/sum(diabPopAsian$`Population`)*100

### Examine the populations by nativity
### ### NUSB

### Total Asian Population with Diabetes (in millions)
sum(diabPopAsian$`Population with diabetes`[1:11])/1e6

### Total Asian Population 15+ with Diabetes (in millions)
sum(diabPopAsian$`Population with diabetes`[3:11])/1e6

### Percentage
sum(diabPopAsian$`Population with diabetes`[1:11]) / sum(diabPopAsian$`Population`[1:11]) *100
### Percentage over 15
sum(diabPopAsian$`Population with diabetes`[3:11]) / sum(diabPopAsian$`Population`[3:11]) *100

sum(diabPopAsian$`Population`[3:11])/1e6
# ### USB
# sum(diabPopAsian$`Population with diabetes`[12:22])/1e6
# sum(diabPopAsian$`Population with diabetes`[12:22]) / sum(diabPopAsian$`Population with diabetes`) *100
#
### Percentage over 15
sum(diabPopAsian$`Population with diabetes`[13:22]) / sum(diabPopAsian$`Population`[13:22]) *100

# ### by age
# ### 0-44
# sum(diabPopAsian$`Population with diabetes`[c(1:5, 12:16)])/sum(diabPopAsian$`Population`[c(1:5, 12:16)])*100
# ### 15-44
# sum(diabPopAsian$`Population with diabetes`[c(3:5, 14:16)])/sum(diabPopAsian$`Population`[c(3:5, 14:16)])*100
# ### 45-64
# sum(diabPopAsian$`Population with diabetes`[c(6,7,17,18)])/sum(diabPopAsian$`Population`[c(6,7,17,18)])*100
# ### 65-74
# sum(diabPopAsian$`Population with diabetes`[c(8,19)])/sum(diabPopAsian$`Population`[c(8,19)])*100

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



### Examine the populations by nativity
### ### NUSB

### Total Hispanic Population with Diabetes (in millions)
sum(diabPopHispanic$`Population with diabetes`[1:11])/1e6

### Total Hispanic Population 15+ with Diabetes (in millions)
sum(diabPopHispanic$`Population with diabetes`[3:11])/1e6

### Percentage
sum(diabPopHispanic$`Population with diabetes`[1:11]) / sum(diabPopHispanic$`Population`[1:11]) *100
### Percentage over 15
sum(diabPopHispanic$`Population with diabetes`[3:11]) / sum(diabPopHispanic$`Population`[3:11]) *100

sum(diabPopHispanic$`Population`[3:11])/1e6

###############################################################################
### By nativity
### Total Hispanic NUSB Population
sum(diabPopHispanic$`Population with diabetes`[1:11])/1e6
### 15 Hispanic NUSB population
sum(diabPopHispanic$`Population with diabetes`[3:11]) /1e6
### Percentage with Diabetes among Hispanic NUSB over 15
sum(diabPopHispanic$`Population with diabetes`[3:11]) / sum(diabPopHispanic$`Population`[3:11]) *100
### Total Hispanic USB Population
sum(diabPopHispanic$`Population with diabetes`[12:22])/1e6
### 15 Hispanic USB population
sum(diabPopHispanic$`Population with diabetes`[14:22]) /1e6
### Percentage with Diabetes among USB over 15
sum(diabPopHispanic$`Population with diabetes`[14:22]) / sum(diabPopHispanic$`Population`[14:22]) *100

###############################################################################
# ### By age
# ### 0-44
# sum(diabPopHispanic$`Population with diabetes`[c(1:5, 12:16)])/sum(diabPopHispanic$`Population`[c(1:5, 12:16)])*100
# ### 15-44
# sum(diabPopHispanic$`Population with diabetes`[c(3:5, 14:16)])/sum(diabPopHispanic$`Population`[c(3:5, 14:16)])*100
# ### 45-64
# sum(diabPopHispanic$`Population with diabetes`[c(6,7,17,18)])/sum(diabPopHispanic$`Population`[c(6,7,17,18)])*100
# ### 65-74
# sum(diabPopHispanic$`Population with diabetes`[c(8,19)])/sum(diabPopHispanic$`Population`[c(8,19)])*100

##### Save this as input for the MITUS model run #####
bothDiabPops <- list("Asian" = diabPopAsian,
                     "Hispanic" = diabPopHispanic)

saveRDS(bothDiabPops, file = "data/diabetesPopulations.rds", version = 2)

sum(diabPopAsian$`Population with diabetes`[3:11]) / sum(diabPopAsian$`Population`[3:11]) *100
sum(diabPopAsian$`Population with diabetes`[14:22]) / sum(diabPopAsian$`Population`[14:22]) *100
sum(diabPopHispanic$`Population with diabetes`[3:11]) / sum(diabPopHispanic$`Population`[3:11]) *100
sum(diabPopHispanic$`Population with diabetes`[14:22]) / sum(diabPopHispanic$`Population`[14:22]) *100

round(sum(diabPopAsian$`Lower bound`[3:11]) / sum(diabPopAsian$`Population`[3:11]) *100, 1)
round(sum(diabPopAsian$`Lower bound`[14:22]) / sum(diabPopAsian$`Population`[14:22]) *100,1)
round(sum(diabPopHispanic$`Lower bound`[3:11]) / sum(diabPopHispanic$`Population`[3:11]) *100,1)
round(sum(diabPopHispanic$`Lower bound`[14:22]) / sum(diabPopHispanic$`Population`[14:22]) *100,1)

round(sum(diabPopAsian$`Upper bound`[3:11]) / sum(diabPopAsian$`Population`[3:11]) *100,1)
round(sum(diabPopAsian$`Upper bound`[14:22]) / sum(diabPopAsian$`Population`[14:22]) *100, 1)
round(sum(diabPopHispanic$`Upper bound`[3:11]) / sum(diabPopHispanic$`Population`[3:11]) *100, 1)
round(sum(diabPopHispanic$`Upper bound`[14:22]) / sum(diabPopHispanic$`Population`[14:22]) *100,1)
