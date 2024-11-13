### Make sure to load all dependencies, helper functions, and color palette and
### set working directory by first running the setupEnvironAndDeps.R file!
###############################################################################
setwd("~/TTTAsianHispDiabetes/LTBITestAndTreatAsianHispDiabetes/")

source("code/setupEnvironAndDeps.R")

### Load in the LTBI prevalence estimates
###############################################################################
load("data/ltbi_prev_par_diab_Aug_29_2024.rData")

ltbiPrev <- ltbi_prev_par_diab %>% mutate("Diabetes status" = ifelse(Diabetes2 == "Yes", "Diabetes", "None"),
                                          "97.5%" = ci_hi,
                                          "2.5%" =  ci_lo) %>%
  filter(Race.ethnicity != "Other") %>% dplyr::rename(`Race-ethnicity` = Race.ethnicity,
                                               `Age group` = Age.group,
                                               `50%` = mean)

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

ggsave(filename = "~/TTTAsianHispDiabetes/LTBIPrevalenceDiabComp11_12_bayesian.png",
       plot = diabCompPLot, width = 8.5, height = 9, units = "in")


###############################################################################
##### Population estimates
###############################################################################
### Read in the ACS population estimates
pop <- readRDS("data/ACS_Pop_Asian_Hisp_AgeGrp.rds")
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
ltbiPrevUSBAsianDiab <- ltbiPrev %>% filter(`Diabetes status` == "Diabetes",
                       `Race-ethnicity` == "Asian",
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

#### non-U.S.-born Asian population with diabetes
ltbiPrevNUSBAsianDiab <- ltbiPrev %>% filter(`Diabetes status` == "Diabetes",
                                             `Race-ethnicity` == "Asian",
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
ltbiPrevUSBHispanicDiab <- ltbiPrev %>% filter(`Diabetes status` == "Diabetes",
                                               `Race-ethnicity` == "Hispanic",
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
ltbiPrevNUSBHispanicDiab <- ltbiPrev %>% filter(`Diabetes status` == "Diabetes",
                                                `Race-ethnicity` == "Hispanic",
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
saveRDS(rateRatios,
        file = "data/rateRatiosLTBI.rds",
        version = 2)
