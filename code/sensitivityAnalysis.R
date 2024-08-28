##############################################################################|
##### This code will implement targeted testing and treatment interventions
##### among Asian and Hispanic populations with and without Diabetes. It will
##### use the MITUS model.
##############################################################################|

##############################################################################|
#####                                 Setup                          #########
##############################################################################|
##### Load in packages
library(MITUS)
library(dplyr)
library(purrr)
library(ggplot2)
library(plyr)
library(tidyverse)
library(TubercuCost)

##### Source in ICER function
source("code/calculateICERs.R")

##### load in model data
model_load()

##### Set up palette for plots
pal <- c("#A51C30", "#306298", "#946eb7", "#fcb315", "#117733", "#ec8f9c")
gradientPal <- c("#A51C30", "#306298", "#D28E98", "#9badca")

##############################################################################|
#####                           Population data                      #########
##############################################################################|
##### Total population
##### Read in the total population estimates from the ACS
pop <- readRDS("~/Documents/TTT & Diabetes/Data/ACS_Pop_Asian_Hisp_AgeGrp.rds")
asianPop <- pop[[1]]
asianNatTotPop <- asianPop %>% group_by(Nativity) %>% reframe(Population = sum(Population))
hispanicPop <- pop[[2]]
hispanicNatTotPop <- hispanicPop %>% group_by(Nativity) %>% reframe(Population = sum(Population))

##### Population with diabetes
##### Read in the diabetes population estimates
diabPops <- readRDS(file = "~/Documents/TTT & Diabetes/Data/diabetesPopulations.rds")
asianDiabPop <- diabPops[[1]] %>%
  dplyr::select(`Age group`, Nativity, `Race-ethnicity`,
                `Population with diabetes`, `Lower bound`, `Upper bound`)
hispanicDiabPop <- diabPops[[2]] %>%
  dplyr::select(`Age group`, Nativity, `Race-ethnicity`,
                `Population with diabetes`, `Lower bound`, `Upper bound`)

##############################################################################|
#####                   Define population characteristics            #########
##############################################################################|
##### Total population
usbProgressionRR <- nusbProgressionRR <- 1
usbMortRR <-nusbMortRR <- 1

##### Population with diabetes
diabProgressionRR <- 1.6
diabMortRR <- 1.9

##### Rate ratio of LTBI prevalence needs to be in respect to all usb or nusb
##### individuals. Example Asian USB have XXX times LTBI prevalence than all
##### USB individuals.
##### Read in the age standardized rate ratios from ltbiPrevalenceModel.R script
ltbiRateRatios <- readRDS("~/Documents/TTT & Diabetes/Data/rateRatiosLTBI.rds")
##### Total population
usbAsianLtbiRR <- 1
nusbAsianLtbiRR <- 1
usbHispanicLtbiRR <- 1
nusbHispanicLtbiRR <- 1

##### Population with diabetes
usbAsianDiabLtbiRR <- ltbiRateRatios[1,2]
nusbAsianDiabLtbiRR <- ltbiRateRatios[2,2]
usbHispanicDiabLtbiRR <- ltbiRateRatios[3,2]
nusbHispanicDiabLtbiRR <- ltbiRateRatios[4,2]
#### Create a named vector for sensitivity analysis
ltbiPrevLow <- c(ltbiRateRatios[,1])
ltbiPrevHigh <- c(ltbiRateRatios[,3])

names(ltbiPrevLow) <- names(ltbiPrevHigh) <- c("usbAsian", "nusbAsian",
                                               "usbHispanic", "nusbHispanic")

##############################################################################|
##### Define fraction for intervention and care cascade              #########
##############################################################################|
tttFrcScrn <- 1.0 ### One time, universal offer
##### Care cascade
usbCareCascade <- nusbCareCascade <- diabCareCascade <- def_care_cascade()

##### Define care cascade matrix for upper and lower bound values
careCascadeMatrix <- matrix(0,7,3)
rownames(careCascadeMatrix) <- c("ttt_ltbi_init", "ttt_ltbi_sens_US",
                                 "ttt_ltbi_sens_NUS", "ttt_ltbi_spec_US",
                                 "ttt_ltbi_spec_NUS", "ttt_ltbi_comp",
                                 "ttt_ltbi_eff")
colnames(careCascadeMatrix) <- c("Mean", "LB", "UB")

careCascadeMatrix["ttt_ltbi_init",] <- c(0.77, 0.70, 0.85)

careCascadeMatrix["ttt_ltbi_sens_US",] <- c(0.78, 0.65, 0.91)
careCascadeMatrix["ttt_ltbi_sens_NUS",] <- c(0.79, 0.70,0.90)

careCascadeMatrix["ttt_ltbi_spec_US",] <- c(0.98, 0.96, 0.99)
careCascadeMatrix["ttt_ltbi_spec_NUS",] <- c(0.99, 0.96, 1.00)

careCascadeMatrix["ttt_ltbi_comp",] <- c(0.87, 0.84, 0.90)
careCascadeMatrix["ttt_ltbi_eff",]  <- c(0.93, 0.90, 0.96)

##############################################################################|
##### Define program changes
##############################################################################|
usbPrgChng <- def_prgchng(ParVec = Par[1,])
nusbPrgChng <- def_prgchng(ParVec = Par[1,])
diabPrgChng <- def_prgchng(ParVec = Par[1,])
diabPrgChng["frc_3hp"] <- 0.25
diabPrgChng["frc_3hr"] <- 0.07
diabPrgChng["frc_4r"] <- 0.68
diabPrgChng["start_yr"] <- 2024
diabPrgChng["IGRA_frc"] <- 1;

##############################################################################|
#####                 Define care cascade changes                    #########
##############################################################################|
diabCareCascade <- def_care_cascade()

##############################################################################|
#####                Define intervention populations                 #########
##############################################################################|
###### U.S.-born non-Hispanic Asian population (with diabetes) ################

usbAsianDiabTTT <- def_ttt_nat_ag()
usbAsianDiabTTT[["NRiskGrp"]] <- sum(asianDiabPop %>%
                                       filter(Nativity == "USB",
                                              `Age group` != "0-4",
                                              `Age group` != "5-14") %>%
                                       dplyr::select(`Population with diabetes`))

usbAsianDiabTTT[["AgeDistUS"]] <- c(0,0, as.vector(unlist((asianDiabPop %>%
                                                             filter(Nativity == "USB",
                                                                    `Age group` != "0-4",
                                                                    `Age group` != "5-14") %>%
                                                             dplyr::select(`Population with diabetes`)) /
                                                            usbAsianDiabTTT[["NRiskGrp"]])))

usbAsianDiabTTT[["AgeDistNUS"]] <- rep(0,11)
usbAsianDiabTTT[["FrcScrn"]] <- tttFrcScrn
usbAsianDiabTTT[["StartYr"]] <- 2024
usbAsianDiabTTT[["EndYr"]] <- 2025
usbAsianDiabTTT[["RRprg"]] <- diabProgressionRR
usbAsianDiabTTT[["RRmu"]] <- diabMortRR
usbAsianDiabTTT[["RRPrev"]] <- usbAsianDiabLtbiRR

###### non-U.S.-born non-Hispanic Asian population (with diabetes) ############
nusbAsianDiabTTT <- def_ttt_nat_ag()
nusbAsianDiabTTT[["NRiskGrp"]] <- sum(asianDiabPop %>%
                                        filter(Nativity == "NUSB",
                                               `Age group` != "0-4",
                                               `Age group` != "5-14") %>%
                                        dplyr::select(`Population with diabetes`))

nusbAsianDiabTTT[["AgeDistNUS"]] <- c(0, 0, as.vector(unlist((asianDiabPop %>%
                                                                filter(Nativity == "NUSB",
                                                                       `Age group` != "0-4",
                                                                       `Age group` != "5-14") %>%
                                                                dplyr::select(`Population with diabetes`)) /
                                                               nusbAsianDiabTTT[["NRiskGrp"]])))
nusbAsianDiabTTT[["AgeDistUS"]] <- rep(0,11)
nusbAsianDiabTTT[["FrcScrn"]] <- tttFrcScrn
nusbAsianDiabTTT[["StartYr"]] <- 2024
nusbAsianDiabTTT[["EndYr"]] <- 2025
nusbAsianDiabTTT[["RRprg"]] <- diabProgressionRR
nusbAsianDiabTTT[["RRmu"]] <- diabMortRR
nusbAsianDiabTTT[["RRPrev"]] <- nusbAsianDiabLtbiRR



###### U.S.-born Hispanic population (with diabetes) ##########################
usbHispanicDiabTTT <- def_ttt_nat_ag()
usbHispanicDiabTTT[["NRiskGrp"]] <- sum(hispanicDiabPop %>%
                                          filter(Nativity == "USB",
                                                 `Age group` != "0-4",
                                                 `Age group` != "5-14") %>%
                                          dplyr::select(`Population with diabetes`))

usbHispanicDiabTTT[["AgeDistUS"]] <- c(0, 0, as.vector(unlist((hispanicDiabPop %>%
                                                                 filter(Nativity == "USB",
                                                                        `Age group` != "0-4",
                                                                        `Age group` != "5-14") %>%
                                                                 dplyr::select(`Population with diabetes`)) /
                                                                usbHispanicDiabTTT[["NRiskGrp"]])))
usbHispanicDiabTTT[["AgeDistNUS"]] <- rep(0,11)
usbHispanicDiabTTT[["FrcScrn"]] <- tttFrcScrn
usbHispanicDiabTTT[["StartYr"]] <- 2024
usbHispanicDiabTTT[["EndYr"]] <- 2025
usbHispanicDiabTTT[["RRprg"]] <- diabProgressionRR
usbHispanicDiabTTT[["RRmu"]] <- diabMortRR
usbHispanicDiabTTT[["RRPrev"]] <- usbHispanicDiabLtbiRR

###### non-U.S.-born Hispanic population (with diabetes) ######################
nusbHispanicDiabTTT <- def_ttt_nat_ag()
nusbHispanicDiabTTT[["NRiskGrp"]] <- sum(hispanicDiabPop %>%
                                           filter(Nativity == "NUSB",
                                                  `Age group` != "0-4",
                                                  `Age group` != "5-14") %>%
                                           dplyr::select(`Population with diabetes`))

nusbHispanicDiabTTT[["AgeDistNUS"]] <- c(0,0, as.vector(unlist((hispanicDiabPop %>%
                                                                  filter(Nativity == "NUSB",
                                                                         `Age group` != "0-4",
                                                                         `Age group` != "5-14") %>%
                                                                  dplyr::select(`Population with diabetes`)) /
                                                                 nusbHispanicDiabTTT[["NRiskGrp"]])))
nusbHispanicDiabTTT[["AgeDistUS"]] <- rep(0,11)
nusbHispanicDiabTTT[["FrcScrn"]] <- tttFrcScrn
nusbHispanicDiabTTT[["StartYr"]] <- 2024
nusbHispanicDiabTTT[["EndYr"]] <- 2025
nusbHispanicDiabTTT[["RRprg"]] <- diabProgressionRR
nusbHispanicDiabTTT[["RRmu"]] <- diabMortRR
nusbHispanicDiabTTT[["RRPrev"]] <- nusbHispanicDiabLtbiRR

##############################################################################|
#####                          Run the MITUS model                       ######
##############################################################################|
###### Baseline model #########################################################
prms <- national_param_init(P = Par[1,],loc ="US", prg_chng = def_prgchng(Par[1,]),
                            ttt_list = list(def_ttt_nat_ag()))
range(rowSums(prms$ImmAct) + rowSums(prms$ImmNon) + rowSums(prms$ImmFst) + rowSums(prms$ImmNon))*1e6
plot((rowSums(prms$ImmAct) + rowSums(prms$ImmNon) + rowSums(prms$ImmFst) + rowSums(prms$ImmNon))*1e6)

plot(rowSums(prms$ImmAct)*1e6)
plot(rowSums(prms$ImmFst)*1e6)
plot(rowSums(prms$ImmLat)*1e6)
plot(rowSums(prms$ImmNon)*1e6)

baselineRes <- national_OutputsZint(samp_i = 1, ParMatrix = Par, loc = "US",
                                    prg_chng = diabPrgChng,
                                    ttt_list = list(def_ttt_nat_ag()), endyr = 2099)

populationVector <- c("usbAsian", "nusbAsian", "usbHispanic", "nusbHispanic")
allPopsResPercSA <- allPopsResSA <- list()
allPopsPlotSAPerc <- allPopsPlotSA <- list()
tbICERs <- list()

pop.labels <- c("U.S.-born Asian", "non-U.S.-born Asian",
                "U.S.-born Hispanic", "non-U.S.-born Hispanic")
names(pop.labels) <- populationVector

##### Calculate the main analysis healthcare costs
unitCosts <- unitCostsMain <- HealthServiceUnitCosts()
##### Set the main analysis utility weights
utilityWeightsMain <- c(1, 0.75, 0.75, 0.89)

for (pop.i in populationVector){

  print(pop.i)

  popParamList <- get(paste0(pop.i, "DiabTTT"))

  tbICERs <- tempResSA <- list("Main" = NULL,
                                "CompHigh" = NULL,
                                "CompLow" = NULL,
                                "InitHigh" = NULL,
                                "InitLow" = NULL,
                                "EffHigh" = NULL,
                                "EffLow" = NULL,
                                "MortHigh" = NULL,
                                "MortLow" = NULL,
                                "ProgHigh" = NULL,
                                "ProgLow" = NULL,
                                "LtbiHigh" = NULL,
                                "LtbiLow" = NULL,
                                "LtbiTestCostHigh" = NULL,
                                "LtbiTestCostLow" = NULL,
                                "LTBITxCostHigh" = NULL,
                                "LTBITxCostLow" = NULL,
                                "TBTxCostHigh" = NULL,
                                "TBTxCostLow" = NULL,
                                "LtbiTxUWHigh" = NULL,
                                "LtbiTxUWLow" = NULL,
                                # "TbUW1High" = NULL,
                                # "TbUW1Low" = NULL,
                               "TBUwHigh" = NULL,
                               "TBUwLow" = NULL)

  diabCareCascade <- def_care_cascade()

  ######## Main analysis #########################################################
  tempResSA[["Main"]] <- national_OutputsZint(samp_i = 1, ParMatrix = Par, loc = "US",
                                          prg_chng = diabPrgChng,
                                          care_cascade = diabCareCascade,
                                          ttt_list = list(popParamList), endyr = 2099)

  tbICERs[["Main"]] <- generateICER(tempResSA[["Main"]] - baselineRes,
                                    UnitCosts = unitCostsMain,
                                    utilityWeights = utilityWeightsMain)

  ######## LTBI treatment completion ############################################
  diabCareCascade["ttt_ltbi_comp"] <- careCascadeMatrix["ttt_ltbi_comp", "UB"]
  tempResSA[["CompHigh"]] <- national_OutputsZint(samp_i = 1, ParMatrix = Par, loc = "US",
                                                    prg_chng = diabPrgChng,
                                                    care_cascade = diabCareCascade,
                                                    ttt_list = list(popParamList), endyr = 2099)

  ### Calculate economic outcomes
  tbICERs[["CompHigh"]] <- generateICER(tempResSA[["CompHigh"]] - baselineRes,
                                        UnitCosts = unitCostsMain,
                                        utilityWeights = utilityWeightsMain)

  ### Lower bound
  diabCareCascade["ttt_ltbi_comp"] <- careCascadeMatrix["ttt_ltbi_comp", "LB"]
  tempResSA[["CompLow"]] <- national_OutputsZint(samp_i = 1, ParMatrix = Par, loc = "US",
                                                        prg_chng = diabPrgChng,
                                                        care_cascade = diabCareCascade,
                                                        ttt_list = list(popParamList), endyr = 2099)
  ### Calculate economic outcomes
  tbICERs[["CompLow"]] <- generateICER(tempResSA[["CompLow"]] - baselineRes,
                                       UnitCosts = unitCostsMain,
                                       utilityWeights = utilityWeightsMain)

  ### Reset to main analysis level
  diabCareCascade["ttt_ltbi_comp"] <- 0

  ######## LTBI treatment initiation ############################################
  diabCareCascade["ttt_ltbi_init"] <- careCascadeMatrix["ttt_ltbi_init", "UB"]
  tempResSA[["InitHigh"]] <- national_OutputsZint(samp_i = 1, ParMatrix = Par, loc = "US",
                                                       prg_chng = diabPrgChng,
                                                       care_cascade = diabCareCascade,
                                                       ttt_list = list(popParamList), endyr = 2099)
  ### Calculate economic outcomes
  tbICERs[["InitHigh"]] <- generateICER(tempResSA[["InitHigh"]] - baselineRes,
                                        UnitCosts = unitCostsMain,
                                        utilityWeights = utilityWeightsMain)

  diabCareCascade["ttt_ltbi_init"] <- careCascadeMatrix["ttt_ltbi_init", "LB"]
  tempResSA[["InitLow"]]<- national_OutputsZint(samp_i = 1, ParMatrix = Par, loc = "US",
                                                       prg_chng = diabPrgChng,
                                                       care_cascade = diabCareCascade,
                                                       ttt_list = list(popParamList), endyr = 2099)
  ### Calculate economic outcomes
  tbICERs[["InitLow"]] <- generateICER(tempResSA[["InitLow"]]  - baselineRes,
                                       UnitCosts = unitCostsMain,
                                       utilityWeights = utilityWeightsMain)

  ### Reset to main analysis level
  diabCareCascade["ttt_ltbi_init"] <- 0

  ######## LTBI treatment efficacy ##############################################
  diabCareCascade["ttt_ltbi_eff"] <- careCascadeMatrix["ttt_ltbi_eff", "UB"]
  tempResSA[["EffHigh"]] <- national_OutputsZint(samp_i = 1, ParMatrix = Par, loc = "US",
                                                        prg_chng = diabPrgChng,
                                                        care_cascade = diabCareCascade,
                                                        ttt_list = list(popParamList), endyr = 2099)
  ### Calculate economic outcomes
  tbICERs[["EffHigh"]] <- generateICER(tempResSA[["EffHigh"]]  - baselineRes,
                                       UnitCosts = unitCostsMain,
                                       utilityWeights = utilityWeightsMain)

  diabCareCascade["ttt_ltbi_eff"] <- careCascadeMatrix["ttt_ltbi_eff", "LB"]
  tempResSA[["EffLow"]] <- national_OutputsZint(samp_i = 1, ParMatrix = Par, loc = "US",
                                                       prg_chng = diabPrgChng,
                                                       care_cascade = diabCareCascade,
                                                       ttt_list = list(popParamList), endyr = 2099)
  ### Calculate economic outcomes
  tbICERs[["EffLow"]] <- generateICER(tempResSA[["EffLow"]] - baselineRes,
                                      UnitCosts = unitCostsMain,
                                      utilityWeights = utilityWeightsMain)

  ### Reset to main analysis level
  diabCareCascade["ttt_ltbi_eff"] <- 0

  ######## IGRA test sensitivity ################################################
  diabCareCascade[["ttt_ltbi_sens"]] <- c(careCascadeMatrix["ttt_ltbi_sens_US", "UB"], careCascadeMatrix["ttt_ltbi_sens_NUS", "UB"])
  tempResSA[["SensHigh"]] <- national_OutputsZint(samp_i = 1, ParMatrix = Par, loc = "US",
                                                         prg_chng = diabPrgChng,
                                                         care_cascade = diabCareCascade,
                                                         ttt_list = list(popParamList), endyr = 2099)

  ### Calculate economic outcomes
  tbICERs[["SensHigh"]] <- generateICER(tempResSA[["SensHigh"]] - baselineRes,
                                        UnitCosts = unitCostsMain,
                                        utilityWeights = utilityWeightsMain)

  diabCareCascade[["ttt_ltbi_sens"]] <- c(careCascadeMatrix["ttt_ltbi_sens_US", "LB"], careCascadeMatrix["ttt_ltbi_sens_NUS", "LB"])
  tempResSA[["SensLow"]] <- national_OutputsZint(samp_i = 1, ParMatrix = Par, loc = "US",
                                                        prg_chng = diabPrgChng,
                                                        care_cascade = diabCareCascade,
                                                        ttt_list = list(popParamList), endyr = 2099)

  ### Calculate economic outcomes
  tbICERs[["SensLow"]] <- generateICER(tempResSA[["SensLow"]]  - baselineRes,
                                       UnitCosts = unitCostsMain,
                                       utilityWeights = utilityWeightsMain)

  ### Reset to main analysis level
  diabCareCascade["ttt_ltbi_sens"] <- 0

  ######## IGRA test specificity ################################################
  diabCareCascade[["ttt_ltbi_spec"]] <- 1 - c(careCascadeMatrix["ttt_ltbi_spec_US", "UB"], careCascadeMatrix["ttt_ltbi_spec_NUS", "UB"])
  tempResSA[["SpecHigh"]] <- national_OutputsZint(samp_i = 1, ParMatrix = Par, loc = "US",
                                                          prg_chng = diabPrgChng,
                                                          care_cascade = diabCareCascade,
                                                          ttt_list = list(popParamList), endyr = 2099)
  ### Calculate economic outcomes
  tbICERs[["SpecHigh"]] <- generateICER(tempResSA[["SpecHigh"]] - baselineRes,
                                        UnitCosts = unitCostsMain,
                                        utilityWeights = utilityWeightsMain)

  diabCareCascade[["ttt_ltbi_spec"]] <- 1 - c(careCascadeMatrix["ttt_ltbi_spec_US", "LB"], careCascadeMatrix["ttt_ltbi_spec_NUS", "LB"])
  tempResSA[["SpecLow"]] <- national_OutputsZint(samp_i = 1, ParMatrix = Par, loc = "US",
                                                         prg_chng = diabPrgChng,
                                                         care_cascade = diabCareCascade,
                                                         ttt_list = list(popParamList), endyr = 2099)

  ### Calculate economic outcomes
  tbICERs[["SpecLow"]] <- generateICER( tempResSA[["SpecLow"]]- baselineRes,
                                       UnitCosts = unitCostsMain,
                                       utilityWeights = utilityWeightsMain)

  ### Reset to main analysis level
  diabCareCascade["ttt_ltbi_spec"] <- 0

  ######## Mortality rate ratio ###############################################
  popParamList[["RRmu"]] <- 2.03
  tempResSA[["MortHigh"]] <- national_OutputsZint(samp_i = 1, ParMatrix = Par, loc = "US",
                                                  prg_chng = diabPrgChng,
                                                  care_cascade = diabCareCascade,
                                                  ttt_list = list(popParamList), endyr = 2099)

  ### Calculate economic outcomes
  tbICERs[["MortHigh"]] <- generateICER(tempResSA[["MortHigh"]] - baselineRes,
                                        UnitCosts = unitCostsMain,
                                        utilityWeights = utilityWeightsMain)

  popParamList[["RRmu"]] <- 1.84
  tempResSA[["MortLow"]] <- national_OutputsZint(samp_i = 1, ParMatrix = Par, loc = "US",
                                                 prg_chng = diabPrgChng,
                                                 care_cascade = diabCareCascade,
                                                 ttt_list = list(popParamList), endyr = 2099)
  ### Calculate economic outcomes
  tbICERs[["MortLow"]] <- generateICER(tempResSA[["MortLow"]] - baselineRes,
                                       UnitCosts = unitCostsMain,
                                       utilityWeights = utilityWeightsMain)

  ### Reset to main analysis level
  popParamList[["RRmu"]] <- 1.93

  ######## Progression rate ratio ###############################################
  popParamList[["RRprg"]] <- 1.73
  tempResSA[["ProgHigh"]] <- national_OutputsZint(samp_i = 1, ParMatrix = Par, loc = "US",
                                                  prg_chng = diabPrgChng,
                                                  care_cascade = diabCareCascade,
                                                  ttt_list = list(popParamList), endyr = 2099)

  ### Calculate economic outcomes
  tbICERs[["ProgHigh"]] <- generateICER(tempResSA[["ProgHigh"]] - baselineRes,
                                        UnitCosts = unitCostsMain,
                                        utilityWeights = utilityWeightsMain)

  popParamList[["RRprg"]] <- 1.47
  tempResSA[["ProgLow"]] <- national_OutputsZint(samp_i = 1, ParMatrix = Par, loc = "US",
                                                 prg_chng = diabPrgChng,
                                                 care_cascade = diabCareCascade,
                                                 ttt_list = list(popParamList), endyr = 2099)

  ### Calculate economic outcomes
  tbICERs[["ProgLow"]] <- generateICER(tempResSA[["ProgLow"]] - baselineRes,
                                       UnitCosts = unitCostsMain,
                                       utilityWeights = utilityWeightsMain)

  ### Reset to main analysis level
  popParamList[["RRprg"]] <- 1.6

  ######## LTBI prevalence rate ratio #########################################
  popParamList[["RRPrev"]] <- ltbiPrevHigh[pop.i]
  tempResSA[["LtbiHigh"]] <- national_OutputsZint(samp_i = 1, ParMatrix = Par, loc = "US",
                                                  prg_chng = diabPrgChng,
                                                  care_cascade = diabCareCascade,
                                                  ttt_list = list(popParamList), endyr = 2099)
  ### Calculate economic outcomes
  tbICERs[["LtbiHigh"]] <- generateICER(tempResSA[["LtbiHigh"]] - baselineRes,
                                        UnitCosts = unitCostsMain,
                                        utilityWeights = utilityWeightsMain)

  popParamList[["RRPrev"]] <- ltbiPrevLow[pop.i]
  tempResSA[["LtbiLow"]] <- national_OutputsZint(samp_i = 1, ParMatrix = Par, loc = "US",
                                                 prg_chng = diabPrgChng,
                                                 care_cascade = diabCareCascade,
                                                 ttt_list = list(popParamList), endyr = 2099)

  ### Calculate economic outcomes
  tbICERs[["LtbiLow"]] <- generateICER(tempResSA[["LtbiLow"]] - baselineRes,
                                       UnitCosts = unitCostsMain,
                                       utilityWeights = utilityWeightsMain)

  ### Reset to main analysis level
  popParamList[["RRPrev"]] <- get(paste0(pop.i, "DiabLtbiRR"))

  ##############################################################################|
  #####            Calculate ICERs for the economic parameters             ######
  ##############################################################################|
  ######## Total cost of LTBI testing ###########################################
  ### Update unit health costs for LTBI testing (upper bound)
  unitCosts['IGRACost'] <- unitCostsMain['IGRACost'] * 1.25
  unitCosts['NoTBCost'] <- unitCostsMain['NoTBCost'] * 1.25

  ### Calculate economic outcomes
  tbICERs[["LtbiTestCostHigh"]] <- generateICER(tempResSA[["Main"]] - baselineRes,
                                                UnitCosts = unitCosts,
                                                utilityWeights = utilityWeightsMain)

  ### Update unit health costs for LTBI testing (lower bound)
  unitCosts['IGRACost'] <- unitCostsMain['IGRACost'] * 0.75
  unitCosts['NoTBCost'] <- unitCostsMain['NoTBCost'] * 0.75

  ### Calculate economic outcomes
  tbICERs[["LtbiTestCostLow"]] <- generateICER(tempResSA[["Main"]]- baselineRes,
                                               UnitCosts = unitCosts,
                                               utilityWeights = utilityWeightsMain)

  ### Reset to main analysis level
  unitCosts <- unitCostsMain

  ######## Total cost of TB treatment #########################################
  ### Update unit health costs for LTBI testing (upper bound)
  unitCosts['3HPCost'] <- unitCostsMain['3HPCost'] * 1.25
  unitCosts['4RCost'] <- unitCostsMain['4RCost'] * 1.25
  unitCosts['3HRCost'] <- unitCostsMain['3HRCost'] * 1.25

  ### Calculate economic outcomes
  tbICERs[["LTBITxCostHigh"]] <- generateICER(tempResSA[["Main"]] - baselineRes,
                                            UnitCosts = unitCosts,
                                            utilityWeights = utilityWeightsMain)

  ### Update unit health costs for LTBI testing (lower bound)
  unitCosts['3HPCost'] <- unitCostsMain['3HPCost'] * 0.75
  unitCosts['4RCost'] <- unitCostsMain['4RCost'] * 0.75
  unitCosts['3HRCost'] <- unitCostsMain['3HRCost'] * 0.75

  ### Calculate economic outcomes
  tbICERs[["LTBITxCostLow"]] <- generateICER(tempResSA[["Main"]] - baselineRes,
                                           UnitCosts = unitCosts,
                                           utilityWeights = utilityWeightsMain)

  ### Reset to main analysis level
  unitCosts <- unitCostsMain

  ######## Total cost of TB treatment #########################################
  ### Update unit health costs for LTBI testing (upper bound)
  unitCosts['TBtx'] <- unitCostsMain['TBtx'] * 1.25

  ### Calculate economic outcomes
  tbICERs[["TBTxCostHigh"]] <- generateICER( tempResSA[["Main"]] - baselineRes,
                                            UnitCosts = unitCosts,
                                            utilityWeights = utilityWeightsMain)

  ### Update unit health costs for LTBI testing (lower bound)
  unitCosts['TBtx'] <- unitCostsMain['TBtx'] * 0.75

  ### Calculate economic outcomes
  tbICERs[["TBTxCostLow"]] <- generateICER(tempResSA[["Main"]] - baselineRes,
                                           UnitCosts = unitCosts,
                                           utilityWeights = utilityWeightsMain)

  ### Reset to main analysis level
  unitCosts <- unitCostsMain

  ######## Utility weights for LTBI testing with toxicity #####################
  ### Update utility weight for LTBI testing with toxicity (upper bound)
  utilityWeights <- utilityWeightsMain
  utilityWeights[2] <- utilityWeightsMain[2] * 1.25

  ### Calculate economic outcomes
  tbICERs[["LtbiTxUWHigh"]] <- generateICER(baselineRes - tempResSA[["Main"]],
                                            UnitCosts = unitCostsMain,
                                            utilityWeights = utilityWeights)

  ### Update utility weight for LTBI testing with toxicity (lower bound)
  utilityWeights[2] <- utilityWeightsMain[2] * 0.75

  ### Calculate economic outcomes
  tbICERs[["LtbiTxUWLow"]] <- generateICER(baselineRes - tempResSA[["Main"]],
                                            UnitCosts = unitCostsMain,
                                            utilityWeights = utilityWeights)

  ### Reset to main analysis level
  utilityWeights <- utilityWeightsMain

  ######## Utility weights for TB disease / treatment #####################
  ### Update utility weight (upper bound)
  utilityWeights[3] <- 0.83
  utilityWeights[4] <- 0.98

  ### Calculate economic outcomes
  tbICERs[["TBUwHigh"]] <- generateICER(baselineRes - tempResSA[["Main"]],
                                         UnitCosts = unitCostsMain,
                                         utilityWeights = utilityWeights)

  ### Update utility weight (lower bound)
  utilityWeights[3] <- 0.64
  utilityWeights[4] <- 0.79

  ### Calculate economic outcomes
  ### Need to adjust the sign to calculate QALYs gained
  tbICERs[["TBUwLow"]] <- generateICER(baselineRes - tempResSA[["Main"]],
                                        UnitCosts = unitCostsMain,
                                        utilityWeights = utilityWeights)

  ### Reset to main analysis level
  utilityWeights <- utilityWeightsMain

  ### Save the results to a list
  allPopsResSA[[pop.i]] <- tempResSA



  ##############################################################################|
  #####               Create a dataframe of the differences                ######
  ##############################################################################|

  differencesTbICERs <- data.frame("LTBI completion" = c(tbICERs[["CompLow"]]["ICER (TB perspective)"], tbICERs[["CompHigh"]]["ICER (TB perspective)"],
                                                         tbICERs[["Main"]]["ICER (TB perspective)"] - tbICERs[["CompHigh"]]["ICER (TB perspective)"]),
                                   "Initiation" = c(tbICERs[["InitLow"]]["ICER (TB perspective)"], tbICERs[["InitHigh"]]["ICER (TB perspective)"],
                                                    tbICERs[["Main"]]["ICER (TB perspective)"] - tbICERs[["InitHigh"]]["ICER (TB perspective)"]),
                                   "Treatment\neffectiveness" = c(tbICERs[["EffLow"]]["ICER (TB perspective)"], tbICERs[["EffHigh"]]["ICER (TB perspective)"],
                                                          tbICERs[["Main"]]["ICER (TB perspective)"] - tbICERs[["EffHigh"]]["ICER (TB perspective)"]),
                                   "Sensitivity" = c(tbICERs[["SensLow"]]["ICER (TB perspective)"], tbICERs[["SensHigh"]]["ICER (TB perspective)"],
                                                     tbICERs[["Main"]]["ICER (TB perspective)"] - tbICERs[["SensHigh"]]["ICER (TB perspective)"]),
                                   "Specificity" = c(tbICERs[["SpecLow"]]["ICER (TB perspective)"], tbICERs[["SpecHigh"]]["ICER (TB perspective)"],
                                                     tbICERs[["Main"]]["ICER (TB perspective)"] - tbICERs[["SpecHigh"]]["ICER (TB perspective)"]),
                                   "Mortality" = c(tbICERs[["MortLow"]]["ICER (TB perspective)"], tbICERs[["MortHigh"]]["ICER (TB perspective)"],
                                                   tbICERs[["Main"]]["ICER (TB perspective)"] - tbICERs[["MortHigh"]]["ICER (TB perspective)"]),
                                   "TB progression" = c(tbICERs[["ProgLow"]]["ICER (TB perspective)"], tbICERs[["ProgHigh"]]["ICER (TB perspective)"],
                                                        tbICERs[["Main"]]["ICER (TB perspective)"] - tbICERs[["ProgHigh"]]["ICER (TB perspective)"]),
                                   "LTBI prevalence" = c(tbICERs[["LtbiLow"]]["ICER (TB perspective)"], tbICERs[["LtbiHigh"]]["ICER (TB perspective)"],
                                                         tbICERs[["Main"]]["ICER (TB perspective)"] - tbICERs[["LtbiHigh"]]["ICER (TB perspective)"]),
                            check.names = FALSE)

  rownames(differencesTbICERs) <- c("Lower bound", "Upper bound", "UB diff")

  differencesTbICERs <- as.data.frame(t(differencesTbICERs))
  differencesTbICERs$Parameter <- rownames(differencesTbICERs)

  ######## Order the dataframe by upper bound differencesTbICERs #######################
  order.parameters <- differencesTbICERs %>% arrange(`UB diff`) %>%
    mutate(Parameter=factor(x=Parameter, levels=Parameter)) %>%
    select(Parameter) %>% unlist() %>% levels()

  ######## Set the baseValue for the main analysis difference ###################
  baseValue <- tbICERs[["Main"]]["ICER (TB perspective)"]
  width <- 0.95
  ######## Reshape the data frame for ggplot and geom_rect ######################
  differencesTbICERs.2 <- differencesTbICERs %>%
    # gather columns Lower_Bound and Upper_Bound into a single column using gather
    gather(key='type', value='outputValue', `Lower bound`:`Upper bound`) %>%
    # just reordering columns
    select(Parameter, type, outputValue, `UB diff`) %>%
    # create the columns for geom_rect
    mutate(Parameter=factor(Parameter, levels=order.parameters),
           ymin=pmin(outputValue, baseValue),
           ymax=pmax(outputValue, baseValue),
           xmin=as.numeric(Parameter)-width/2,
           xmax=as.numeric(Parameter)+width/2)

  ##############################################################################|
  #####                         Make a tornado plot                       ######
  ##############################################################################|

  allPopsPlotSA[[pop.i]] <- ggplot() +
                        geom_rect(data = differencesTbICERs.2,
                                  aes(ymax=ymax, ymin=ymin, xmax=xmax, xmin=xmin, fill=type)) +
                        theme_minimal(base_size = 16) +
                        theme(axis.title.y=element_blank(), legend.position = 'bottom',
                              legend.title = element_blank()) +
                        geom_hline(yintercept = baseValue) +
                        scale_x_continuous(breaks = c(1:length(order.parameters)),
                                           labels = order.parameters) +
                        coord_flip() + ylab("Incremental cost effectiveness ratio") +
                        scale_fill_manual(values = pal[c(1,2)])

  ##############################################################################|
  #####           Create a dataframe of the percent differences            ######
  ##############################################################################|

  # percDiffsTbICERs <- data.frame("LTBI treatment\ncompletion" = c((tbICERs[["CompLow"]]["ICER (TB perspective)"] - tbICERs[["Main"]]["ICER (TB perspective)"]) / tbICERs[["Main"]]["ICER (TB perspective)"],
  #                                                        (tbICERs[["CompHigh"]]["ICER (TB perspective)"] - tbICERs[["Main"]]["ICER (TB perspective)"]) / tbICERs[["Main"]]["ICER (TB perspective)"],
  #                                                        (tbICERs[["CompHigh"]]["ICER (TB perspective)"] - tbICERs[["Main"]]["ICER (TB perspective)"]) / tbICERs[["Main"]]["ICER (TB perspective)"]),
  #                                  "LTBI treatment\ninitiation" = c((tbICERs[["InitLow"]]["ICER (TB perspective)"] - tbICERs[["Main"]]["ICER (TB perspective)"]) / tbICERs[["Main"]]["ICER (TB perspective)"],
  #                                                   (tbICERs[["InitHigh"]]["ICER (TB perspective)"] - tbICERs[["Main"]]["ICER (TB perspective)"]) / tbICERs[["Main"]]["ICER (TB perspective)"],
  #                                                   (tbICERs[["InitHigh"]]["ICER (TB perspective)"] - tbICERs[["Main"]]["ICER (TB perspective)"]) / tbICERs[["Main"]]["ICER (TB perspective)"]),
  #                                  "LTBI treatment\neffectiveness" = c((tbICERs[["EffLow"]]["ICER (TB perspective)"] - tbICERs[["Main"]]["ICER (TB perspective)"]) / tbICERs[["Main"]]["ICER (TB perspective)"],
  #                                                                 (tbICERs[["EffHigh"]]["ICER (TB perspective)"] - tbICERs[["Main"]]["ICER (TB perspective)"]) / tbICERs[["Main"]]["ICER (TB perspective)"],
  #                                                                 (tbICERs[["EffHigh"]]["ICER (TB perspective)"] - tbICERs[["Main"]]["ICER (TB perspective)"]) / tbICERs[["Main"]]["ICER (TB perspective)"]),
  #                                  "IGRA Sensitivity" = c((tbICERs[["SensLow"]]["ICER (TB perspective)"] - tbICERs[["Main"]]["ICER (TB perspective)"]) / tbICERs[["Main"]]["ICER (TB perspective)"],
  #                                                    (tbICERs[["SensHigh"]]["ICER (TB perspective)"] - tbICERs[["Main"]]["ICER (TB perspective)"]) / tbICERs[["Main"]]["ICER (TB perspective)"],
  #                                                    (tbICERs[["SensHigh"]]["ICER (TB perspective)"] - tbICERs[["Main"]]["ICER (TB perspective)"]) / tbICERs[["Main"]]["ICER (TB perspective)"]),
  #                                  "IGRA Specificity" = c((tbICERs[["SpecLow"]]["ICER (TB perspective)"] - tbICERs[["Main"]]["ICER (TB perspective)"]) / tbICERs[["Main"]]["ICER (TB perspective)"],
  #                                                    (tbICERs[["SpecHigh"]]["ICER (TB perspective)"] - tbICERs[["Main"]]["ICER (TB perspective)"]) / tbICERs[["Main"]]["ICER (TB perspective)"],
  #                                                    (tbICERs[["SpecHigh"]]["ICER (TB perspective)"] - tbICERs[["Main"]]["ICER (TB perspective)"]) / tbICERs[["Main"]]["ICER (TB perspective)"]),
  #                                  "Diabetes associated\nall-cause mortality" = c((tbICERs[["MortLow"]]["ICER (TB perspective)"] - tbICERs[["Main"]]["ICER (TB perspective)"]) / tbICERs[["Main"]]["ICER (TB perspective)"],
  #                                                  (tbICERs[["MortHigh"]]["ICER (TB perspective)"] - tbICERs[["Main"]]["ICER (TB perspective)"]) / tbICERs[["Main"]]["ICER (TB perspective)"],
  #                                                  (tbICERs[["MortHigh"]]["ICER (TB perspective)"] - tbICERs[["Main"]]["ICER (TB perspective)"]) / tbICERs[["Main"]]["ICER (TB perspective)"]),
  #                                  "TB progression" = c((tbICERs[["ProgLow"]]["ICER (TB perspective)"] - tbICERs[["Main"]]["ICER (TB perspective)"]) / tbICERs[["Main"]]["ICER (TB perspective)"],
  #                                                       (tbICERs[["ProgHigh"]]["ICER (TB perspective)"] - tbICERs[["Main"]]["ICER (TB perspective)"]) / tbICERs[["Main"]]["ICER (TB perspective)"],
  #                                                       (tbICERs[["ProgHigh"]]["ICER (TB perspective)"] - tbICERs[["Main"]]["ICER (TB perspective)"]) / tbICERs[["Main"]]["ICER (TB perspective)"]),
  #                                  "LTBI prevalence" = c((tbICERs[["LtbiLow"]]["ICER (TB perspective)"] - tbICERs[["Main"]]["ICER (TB perspective)"]) / tbICERs[["Main"]]["ICER (TB perspective)"],
  #                                                        (tbICERs[["LtbiHigh"]]["ICER (TB perspective)"] - tbICERs[["Main"]]["ICER (TB perspective)"]) / tbICERs[["Main"]]["ICER (TB perspective)"],
  #                                                        (tbICERs[["LtbiHigh"]]["ICER (TB perspective)"] - tbICERs[["Main"]]["ICER (TB perspective)"]) / tbICERs[["Main"]]["ICER (TB perspective)"]),
  #                                 "Total cost of\nLTBI testing" = c((tbICERs[["LtbiTestCostLow"]]["ICER (TB perspective)"] - tbICERs[["Main"]]["ICER (TB perspective)"]) / tbICERs[["Main"]]["ICER (TB perspective)"],
  #                                                      (tbICERs[["LtbiTestCostHigh"]]["ICER (TB perspective)"] - tbICERs[["Main"]]["ICER (TB perspective)"]) / tbICERs[["Main"]]["ICER (TB perspective)"],
  #                                                      (tbICERs[["LtbiTestCostHigh"]]["ICER (TB perspective)"] - tbICERs[["Main"]]["ICER (TB perspective)"]) / tbICERs[["Main"]]["ICER (TB perspective)"]),
  #                                "Average cost of\nLTBI treatment" = c((tbICERs[["LTBITxCostLow"]]["ICER (TB perspective)"] - tbICERs[["Main"]]["ICER (TB perspective)"]) / tbICERs[["Main"]]["ICER (TB perspective)"],
  #                                                                  (tbICERs[["LTBITxCostHigh"]]["ICER (TB perspective)"] - tbICERs[["Main"]]["ICER (TB perspective)"]) / tbICERs[["Main"]]["ICER (TB perspective)"],
  #                                                                  (tbICERs[["LTBITxCostHigh"]]["ICER (TB perspective)"] - tbICERs[["Main"]]["ICER (TB perspective)"]) / tbICERs[["Main"]]["ICER (TB perspective)"]),
  #                                "Cost of\nTB treatment" = c((tbICERs[["TBTxCostLow"]]["ICER (TB perspective)"] - tbICERs[["Main"]]["ICER (TB perspective)"]) / tbICERs[["Main"]]["ICER (TB perspective)"],
  #                                                                      (tbICERs[["TBTxCostHigh"]]["ICER (TB perspective)"] - tbICERs[["Main"]]["ICER (TB perspective)"]) / tbICERs[["Main"]]["ICER (TB perspective)"],
  #                                                                      (tbICERs[["TBTxCostHigh"]]["ICER (TB perspective)"] - tbICERs[["Main"]]["ICER (TB perspective)"]) / tbICERs[["Main"]]["ICER (TB perspective)"]),
  #                                "Utility weight LTBI" = c((tbICERs[["LtbiTxUWLow"]]["ICER (TB perspective)"] - tbICERs[["Main"]]["ICER (TB perspective)"]) / tbICERs[["Main"]]["ICER (TB perspective)"],
  #                                                            (tbICERs[["LtbiTxUWHigh"]]["ICER (TB perspective)"] - tbICERs[["Main"]]["ICER (TB perspective)"]) / tbICERs[["Main"]]["ICER (TB perspective)"],
  #                                                            (tbICERs[["LtbiTxUWHigh"]]["ICER (TB perspective)"] - tbICERs[["Main"]]["ICER (TB perspective)"]) / tbICERs[["Main"]]["ICER (TB perspective)"]),
  #                                "Utility weight TB" = c((tbICERs[["TBUwLow"]]["ICER (TB perspective)"] - tbICERs[["Main"]]["ICER (TB perspective)"]) / tbICERs[["Main"]]["ICER (TB perspective)"],
  #                                                           (tbICERs[["TBUwHigh"]]["ICER (TB perspective)"] - tbICERs[["Main"]]["ICER (TB perspective)"]) / tbICERs[["Main"]]["ICER (TB perspective)"],
  #                                                           (tbICERs[["TBUwHigh"]]["ICER (TB perspective)"] - tbICERs[["Main"]]["ICER (TB perspective)"]) / tbICERs[["Main"]]["ICER (TB perspective)"]),
  #                                  check.names = FALSE)


  percDiffsTbICERs <- data.frame("LTBI treatment\ncompletion" = c((tbICERs[["CompLow"]]["ICER (TB perspective)"] - tbICERs[["Main"]]["ICER (TB perspective)"]) / tbICERs[["Main"]]["ICER (TB perspective)"],
                                                                  (tbICERs[["CompHigh"]]["ICER (TB perspective)"] - tbICERs[["Main"]]["ICER (TB perspective)"]) / tbICERs[["Main"]]["ICER (TB perspective)"],
                                                                  (tbICERs[["CompHigh"]]["ICER (TB perspective)"] - tbICERs[["Main"]]["ICER (TB perspective)"]) / tbICERs[["Main"]]["ICER (TB perspective)"]),
                                 "LTBI treatment\ninitiation" = c((tbICERs[["InitLow"]]["ICER (TB perspective)"] - tbICERs[["Main"]]["ICER (TB perspective)"]) / tbICERs[["Main"]]["ICER (TB perspective)"],
                                                                  (tbICERs[["InitHigh"]]["ICER (TB perspective)"] - tbICERs[["Main"]]["ICER (TB perspective)"]) / tbICERs[["Main"]]["ICER (TB perspective)"],
                                                                  (tbICERs[["InitHigh"]]["ICER (TB perspective)"] - tbICERs[["Main"]]["ICER (TB perspective)"]) / tbICERs[["Main"]]["ICER (TB perspective)"]),
                                 "LTBI treatment\neffectiveness" = c((tbICERs[["EffLow"]]["ICER (TB perspective)"] - tbICERs[["Main"]]["ICER (TB perspective)"]) / tbICERs[["Main"]]["ICER (TB perspective)"],
                                                                     (tbICERs[["EffHigh"]]["ICER (TB perspective)"] - tbICERs[["Main"]]["ICER (TB perspective)"]) / tbICERs[["Main"]]["ICER (TB perspective)"],
                                                                     (tbICERs[["EffHigh"]]["ICER (TB perspective)"] - tbICERs[["Main"]]["ICER (TB perspective)"]) / tbICERs[["Main"]]["ICER (TB perspective)"]),
                                 "IGRA Sensitivity" = c((tbICERs[["SensLow"]]["ICER (TB perspective)"] - tbICERs[["Main"]]["ICER (TB perspective)"]) / tbICERs[["Main"]]["ICER (TB perspective)"],
                                                        (tbICERs[["SensHigh"]]["ICER (TB perspective)"] - tbICERs[["Main"]]["ICER (TB perspective)"]) / tbICERs[["Main"]]["ICER (TB perspective)"],
                                                        (tbICERs[["SensHigh"]]["ICER (TB perspective)"] - tbICERs[["Main"]]["ICER (TB perspective)"]) / tbICERs[["Main"]]["ICER (TB perspective)"]),
                                 "IGRA Specificity" = c((tbICERs[["SpecLow"]]["ICER (TB perspective)"] - tbICERs[["Main"]]["ICER (TB perspective)"]) / tbICERs[["Main"]]["ICER (TB perspective)"],
                                                        (tbICERs[["SpecHigh"]]["ICER (TB perspective)"] - tbICERs[["Main"]]["ICER (TB perspective)"]) / tbICERs[["Main"]]["ICER (TB perspective)"],
                                                        (tbICERs[["SpecHigh"]]["ICER (TB perspective)"] - tbICERs[["Main"]]["ICER (TB perspective)"]) / tbICERs[["Main"]]["ICER (TB perspective)"]),
                                 "Diabetes associated\nall-cause mortality" = c((tbICERs[["MortLow"]]["ICER (TB perspective)"] - tbICERs[["Main"]]["ICER (TB perspective)"]) / tbICERs[["Main"]]["ICER (TB perspective)"],
                                                                                (tbICERs[["MortHigh"]]["ICER (TB perspective)"] - tbICERs[["Main"]]["ICER (TB perspective)"]) / tbICERs[["Main"]]["ICER (TB perspective)"],
                                                                                (tbICERs[["MortHigh"]]["ICER (TB perspective)"] - tbICERs[["Main"]]["ICER (TB perspective)"]) / tbICERs[["Main"]]["ICER (TB perspective)"]),
                                 "TB progression" = c((tbICERs[["ProgLow"]]["ICER (TB perspective)"] - tbICERs[["Main"]]["ICER (TB perspective)"]) / tbICERs[["Main"]]["ICER (TB perspective)"],
                                                      (tbICERs[["ProgHigh"]]["ICER (TB perspective)"] - tbICERs[["Main"]]["ICER (TB perspective)"]) / tbICERs[["Main"]]["ICER (TB perspective)"],
                                                      (tbICERs[["ProgHigh"]]["ICER (TB perspective)"] - tbICERs[["Main"]]["ICER (TB perspective)"]) / tbICERs[["Main"]]["ICER (TB perspective)"]),
                                 "LTBI prevalence" = c((tbICERs[["LtbiLow"]]["ICER (TB perspective)"] - tbICERs[["Main"]]["ICER (TB perspective)"]) / tbICERs[["Main"]]["ICER (TB perspective)"],
                                                       (tbICERs[["LtbiHigh"]]["ICER (TB perspective)"] - tbICERs[["Main"]]["ICER (TB perspective)"]) / tbICERs[["Main"]]["ICER (TB perspective)"],
                                                       (tbICERs[["LtbiHigh"]]["ICER (TB perspective)"] - tbICERs[["Main"]]["ICER (TB perspective)"]) / tbICERs[["Main"]]["ICER (TB perspective)"]),
                                 "Total cost of\nLTBI testing" = c((tbICERs[["LtbiTestCostLow"]]["ICER (TB perspective)"] - tbICERs[["Main"]]["ICER (TB perspective)"]) / tbICERs[["Main"]]["ICER (TB perspective)"],
                                                                   (tbICERs[["LtbiTestCostHigh"]]["ICER (TB perspective)"] - tbICERs[["Main"]]["ICER (TB perspective)"]) / tbICERs[["Main"]]["ICER (TB perspective)"],
                                                                   (tbICERs[["LtbiTestCostHigh"]]["ICER (TB perspective)"] - tbICERs[["Main"]]["ICER (TB perspective)"]) / tbICERs[["Main"]]["ICER (TB perspective)"]),
                                 "Average cost of\nLTBI treatment" = c((tbICERs[["LTBITxCostLow"]]["ICER (TB perspective)"] - tbICERs[["Main"]]["ICER (TB perspective)"]) / tbICERs[["Main"]]["ICER (TB perspective)"],
                                                                       (tbICERs[["LTBITxCostHigh"]]["ICER (TB perspective)"] - tbICERs[["Main"]]["ICER (TB perspective)"]) / tbICERs[["Main"]]["ICER (TB perspective)"],
                                                                       (tbICERs[["LTBITxCostHigh"]]["ICER (TB perspective)"] - tbICERs[["Main"]]["ICER (TB perspective)"]) / tbICERs[["Main"]]["ICER (TB perspective)"]),
                                 "Cost of\nTB treatment" = c((tbICERs[["TBTxCostLow"]]["ICER (TB perspective)"] - tbICERs[["Main"]]["ICER (TB perspective)"]) / tbICERs[["Main"]]["ICER (TB perspective)"],
                                                             (tbICERs[["TBTxCostHigh"]]["ICER (TB perspective)"] - tbICERs[["Main"]]["ICER (TB perspective)"]) / tbICERs[["Main"]]["ICER (TB perspective)"],
                                                             (tbICERs[["TBTxCostHigh"]]["ICER (TB perspective)"] - tbICERs[["Main"]]["ICER (TB perspective)"]) / tbICERs[["Main"]]["ICER (TB perspective)"]),
                                 "Utility weight LTBI" = c((tbICERs[["LtbiTxUWLow"]]["ICER (TB perspective)"] - tbICERs[["Main"]]["ICER (TB perspective)"]) / tbICERs[["Main"]]["ICER (TB perspective)"],
                                                           (tbICERs[["LtbiTxUWHigh"]]["ICER (TB perspective)"] - tbICERs[["Main"]]["ICER (TB perspective)"]) / tbICERs[["Main"]]["ICER (TB perspective)"],
                                                           (tbICERs[["LtbiTxUWHigh"]]["ICER (TB perspective)"] - tbICERs[["Main"]]["ICER (TB perspective)"]) / tbICERs[["Main"]]["ICER (TB perspective)"]),
                                 "Utility weight TB" = c((tbICERs[["TBUwLow"]]["ICER (TB perspective)"] - tbICERs[["Main"]]["ICER (TB perspective)"]) / tbICERs[["Main"]]["ICER (TB perspective)"],
                                                         (tbICERs[["TBUwHigh"]]["ICER (TB perspective)"] - tbICERs[["Main"]]["ICER (TB perspective)"]) / tbICERs[["Main"]]["ICER (TB perspective)"],
                                                         (tbICERs[["TBUwHigh"]]["ICER (TB perspective)"] - tbICERs[["Main"]]["ICER (TB perspective)"]) / tbICERs[["Main"]]["ICER (TB perspective)"]),
                                 check.names = FALSE)

  rownames(percDiffsTbICERs) <- c("Lower bound", "Upper bound", "UB diff")

  percDiffsTbICERs <- as.data.frame(t(percDiffsTbICERs))
  percDiffsTbICERs$Parameter <- rownames(percDiffsTbICERs)

  ######## Order the dataframe by upper bound percDiffsTbICERs #######################
  order.parameters <- percDiffsTbICERs %>% arrange(`UB diff`) %>%
    mutate(Parameter=factor(x=Parameter, levels=Parameter)) %>%
    select(Parameter) %>% unlist() %>% levels()

  ######## Set the baseValue for the main analysis difference ###################
  baseValue <- 0
  width <- 0.95
  ######## Reshape the data frame for ggplot and geom_rect ######################
  percDiffsTbICERs.2 <- percDiffsTbICERs %>%
    # gather columns Lower_Bound and Upper_Bound into a single column using gather
    gather(key='type', value='outputValue', `Lower bound`:`Upper bound`) %>%
    # just reordering columns
    select(Parameter, type, outputValue, `UB diff`) %>%
    # create the columns for geom_rect
    mutate(Parameter=factor(Parameter, levels=order.parameters),
           ymin=pmin(outputValue, baseValue),
           ymax=pmax(outputValue, baseValue),
           xmin=as.numeric(Parameter)-width/2,
           xmax=as.numeric(Parameter)+width/2)

  allPopsResPercSA[[pop.i]] <- percDiffsTbICERs.2
  ##############################################################################|
  #####                         Make a tornado plot                       ######
  ##############################################################################|


  allPopsPlotSAPerc[[pop.i]] <-
  ggplot() +
    geom_rect(data = percDiffsTbICERs.2,
              aes(ymax=ymax, ymin=ymin, xmax=xmax, xmin=xmin, fill=type), color = "black") +
    theme_minimal(base_size = 16) +
    theme(axis.title.y=element_blank(), legend.position = 'bottom',
          legend.title = element_blank(),
          panel.grid.minor = element_blank()) +
    geom_hline(yintercept = baseValue) +
    scale_x_continuous(breaks = c(1:length(order.parameters)),
                       labels = order.parameters) +
    scale_y_continuous(labels = scales::percent) +
    coord_flip() +
    ylab("Percentage change in ICER") +
    scale_fill_manual(values = gradientPal) +
    ggtitle(paste(pop.labels[pop.i], "population"))
}


#### Try to make a combined plot for both NUSB populations
nusbHispPercDiffsIcers <- allPopsResPercSA[["nusbHispanic"]]
nusbHispPercDiffsIcers$Population <- rep("Hispanic", nrow(nusbHispPercDiffsIcers))
nusbAsianPercDiffsIcers <- allPopsResPercSA[["nusbAsian"]]
nusbAsianPercDiffsIcers$Population <- rep("Asian", nrow(nusbAsianPercDiffsIcers))

nusbPopsPercDiffsICERs <- rbind(nusbHispPercDiffsIcers, nusbAsianPercDiffsIcers)
nusbPopsPercDiffsICERs$Population <- as.factor(nusbPopsPercDiffsICERs$Population)

usbHispPercDiffsIcers <- allPopsResPercSA[["usbHispanic"]]
usbHispPercDiffsIcers$Population <- rep("Hispanic", nrow(usbHispPercDiffsIcers))
usbAsianPercDiffsIcers <- allPopsResPercSA[["usbAsian"]]
usbAsianPercDiffsIcers$Population <- rep("Asian", nrow(usbAsianPercDiffsIcers))

usbPopsPercDiffsICERs <- rbind(usbHispPercDiffsIcers, usbAsianPercDiffsIcers)
usbPopsPercDiffsICERs$Population <- as.factor(usbPopsPercDiffsICERs$Population)

ggplot() +
  geom_rect(data = nusbPopsPercDiffsICERs %>% filter(Population == "Asian"),
            aes(ymax=ymax, ymin=ymin, xmax=xmax, xmin=xmin, fill=type), color = "black") +
  theme_minimal(base_size = 16) +
  theme(axis.title.y=element_blank(), legend.position = 'bottom',
        legend.title = element_blank(),
        panel.grid.minor = element_blank()) +
  geom_hline(yintercept = baseValue) +
  scale_x_continuous(breaks = c(1:length(order.parameters)),
                     labels = order.parameters) +
  scale_y_continuous(labels = scales::percent) +
  coord_flip() +
  ylab("Percentage change in ICER") +
  scale_fill_manual(values = gradientPal) +
  ggtitle("non-U.S.-born Asian population")

ggplot() +
  geom_rect(data = nusbPopsPercDiffsICERs %>% filter(Population == "Hispanic"),
            aes(ymax=ymax, ymin=ymin, xmax=xmax, xmin=xmin, fill=type), color = "black") +
  theme_minimal(base_size = 16) +
  theme(axis.title.y=element_blank(), legend.position = 'bottom',
        legend.title = element_blank(),
        panel.grid.minor = element_blank()) +
  geom_hline(yintercept = baseValue) +
  scale_x_continuous(breaks = c(1:length(order.parameters)),
                     labels = order.parameters) +
  scale_y_continuous(labels = scales::percent) +
  coord_flip() +
  ylab("Percentage change in ICER") +
  scale_fill_manual(values = gradientPal) +
  ggtitle("non-U.S.-born Hispanic population")

ggplot() +
  geom_rect(data = usbPopsPercDiffsICERs %>% filter(Population == "Asian"),
            aes(ymax=ymax, ymin=ymin, xmax=xmax, xmin=xmin, fill=type), color = "black") +
  theme_minimal(base_size = 16) +
  theme(axis.title.y=element_blank(), legend.position = 'bottom',
        legend.title = element_blank(),
        panel.grid.minor = element_blank()) +
  geom_hline(yintercept = baseValue) +
  scale_x_continuous(breaks = c(1:length(order.parameters)),
                     labels = order.parameters) +
  scale_y_continuous(labels = scales::percent) +
  coord_flip() +
  ylab("Percentage change in ICER") +
  scale_fill_manual(values = gradientPal) +
  ggtitle("U.S.-born Asian population")

ggplot() +
  geom_rect(data = usbPopsPercDiffsICERs %>% filter(Population == "Hispanic"),
            aes(ymax=ymax, ymin=ymin, xmax=xmax, xmin=xmin, fill=type), color = "black") +
  theme_minimal(base_size = 16) +
  theme(axis.title.y=element_blank(), legend.position = 'bottom',
        legend.title = element_blank(),
        panel.grid.minor = element_blank()) +
  geom_hline(yintercept = baseValue) +
  scale_x_continuous(breaks = c(1:length(order.parameters)),
                     labels = order.parameters) +
  scale_y_continuous(labels = scales::percent) +
  coord_flip() +
  ylab("Percentage change in ICER") +
  scale_fill_manual(values = gradientPal) +
  ggtitle("U.S.-born Hispanic population")
