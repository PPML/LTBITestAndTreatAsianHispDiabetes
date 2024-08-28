###############################################################################
##### This code will implement targeted testing and treatment interventions
##### among Asian and Hispanic populations with and without Diabetes. It will
##### use the MITUS model.
###############################################################################

###############################################################################
##### Load in packages
###############################################################################
library(MITUS)
library(dplyr)
library(purrr)
model_load()

###############################################################################
##### Population data
###############################################################################
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

###############################################################################
##### Define population characteristics
###############################################################################
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

###############################################################################
##### Define fraction for intervention and care cascade
###############################################################################
tttFrcScrn <- 1.0 ### One time, universal offer
##### Care cascade
usbCareCascade <- nusbCareCascade <- diabCareCascade <- def_care_cascade()

###############################################################################
##### Define program changes
###############################################################################
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

###############################################################################
##### Define intervention populations
###############################################################################
##### Populations with diabetes by nativity #####
##### U.S.-born non-Hispanic Asian population (with diabetes)

usbAsianDiabTTT <- def_ttt_nat_ag()
usbAsianDiabTTT[["NRiskGrp"]] <- sum(asianDiabPop %>%
                                       filter(Nativity == "USB",
                                              `Age group` != "0-4",
                                              `Age group` != "5-14") %>%
                                       dplyr::select(`Population with diabetes`))
#### Lower bound
sum(asianDiabPop %>%
      filter(Nativity == "USB",
             `Age group` != "0-4",
             `Age group` != "5-14") %>%
      dplyr::select(`Lower bound`)) / 1e6

#### Upper bound
sum(asianDiabPop %>%
      filter(Nativity == "USB",
             `Age group` != "0-4",
             `Age group` != "5-14") %>%
      dplyr::select(`Upper bound`)) / 1e6

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

# saveRDS(usbAsianDiabTTT, "~/Documents/TTT & Diabetes/Inputs/usbAsianDiabInputs.rds", version = 2)

##### non-U.S.-born non-Hispanic Asian population (with diabetes)
nusbAsianDiabTTT <- def_ttt_nat_ag()
nusbAsianDiabTTT[["NRiskGrp"]] <- sum(asianDiabPop %>%
                                        filter(Nativity == "NUSB",
                                               `Age group` != "0-4",
                                               `Age group` != "5-14") %>%
                                        dplyr::select(`Population with diabetes`))
#### Lower bound
sum(asianDiabPop %>%
      filter(Nativity == "NUSB",
             `Age group` != "0-4",
             `Age group` != "5-14") %>%
      dplyr::select(`Lower bound`)) / 1e6

#### Upper bound
sum(asianDiabPop %>%
      filter(Nativity == "NUSB",
             `Age group` != "0-4",
             `Age group` != "5-14") %>%
      dplyr::select(`Upper bound`)) / 1e6

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

saveRDS(nusbAsianDiabTTT, "~/Documents/TTT & Diabetes/Inputs/nusbAsianDiabInputs.rds", version = 2)


##### U.S.-born Hispanic population (with diabetes)
usbHispanicDiabTTT <- def_ttt_nat_ag()
usbHispanicDiabTTT[["NRiskGrp"]] <- sum(hispanicDiabPop %>%
                                          filter(Nativity == "USB",
                                                 `Age group` != "0-4",
                                                 `Age group` != "5-14") %>%
                                          dplyr::select(`Population with diabetes`))

#### Lower bound
sum(hispanicDiabPop %>%
      filter(Nativity == "USB",
             `Age group` != "0-4",
             `Age group` != "5-14") %>%
      dplyr::select(`Lower bound`)) / 1e6

#### Upper bound
sum(hispanicDiabPop %>%
      filter(Nativity == "USB",
             `Age group` != "0-4",
             `Age group` != "5-14") %>%
      dplyr::select(`Upper bound`)) / 1e6

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

saveRDS(usbHispanicDiabTTT, "~/Documents/TTT & Diabetes/Inputs/usbHispanicDiabInputs.rds", version = 2)

##### non-U.S.-born Hispanic population (with diabetes)
nusbHispanicDiabTTT <- def_ttt_nat_ag()
nusbHispanicDiabTTT[["NRiskGrp"]] <- sum(hispanicDiabPop %>%
                                           filter(Nativity == "NUSB",
                                                  `Age group` != "0-4",
                                                  `Age group` != "5-14") %>%
                                           dplyr::select(`Population with diabetes`))

#### Lower bound
sum(hispanicDiabPop %>%
      filter(Nativity == "NUSB",
             `Age group` != "0-4",
             `Age group` != "5-14") %>%
      dplyr::select(`Lower bound`)) / 1e6

#### Upper bound
sum(hispanicDiabPop %>%
      filter(Nativity == "NUSB",
             `Age group` != "0-4",
             `Age group` != "5-14") %>%
      dplyr::select(`Upper bound`)) / 1e6

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

saveRDS(nusbHispanicDiabTTT, "~/Documents/TTT & Diabetes/Inputs/nusbHispanicDiabInputs.rds", version = 2)
###############################################################################
##### Run the MITUS model
###############################################################################
##### Baseline model
prms <- national_param_init(P = Par[1,],loc ="US", prg_chng = def_prgchng(Par[1,]),
                            ttt_list = list(def_ttt_nat_ag()))
range(rowSums(prms$ImmAct) + rowSums(prms$ImmNon) + rowSums(prms$ImmFst) + rowSums(prms$ImmNon))*1e6
plot((rowSums(prms$ImmAct) + rowSums(prms$ImmNon) + rowSums(prms$ImmFst) + rowSums(prms$ImmNon))*1e6)

plot(rowSums(prms$ImmAct)*1e6)
plot(rowSums(prms$ImmFst)*1e6)
plot(rowSums(prms$ImmLat)*1e6)
plot(rowSums(prms$ImmNon)*1e6)
# baselineRes <- national_OutputsZint(samp_i = 1, ParMatrix = Par, loc = "US",
#                                     prg_chng = def_prgchng(Par[1,]),
#                                     ttt_list = list(def_ttt_nat_ag()), endyr = 2099)

baselineRes <- national_OutputsZint(samp_i = 1, ParMatrix = Par, loc = "US",
                                    prg_chng = diabPrgChng,
                                    ttt_list = list(def_ttt_nat_ag()), endyr = 2099)

# plot(baselineRes[75:101,"NOTIF_ALL"]*1e6)
# plot(baselineRes[75:101,"NOTIF_US"]*1e6)
# plot((baselineRes[75:101,"NOTIF_F1"] + baselineRes[75:101,"NOTIF_F2"]) *1e6)
#
# plot(baselineRes[1:101,"N_Ac"]*1e6)
# plot(baselineRes[1:101,"N_Lf"]*1e6)
# plot(baselineRes[1:101,"N_Ls"]*1e6)
#
#
# plot(baselineRes[75:101,2])


# saveRDS(baselineRes, "~/MITUS/inst/US/UStempBaseCaseTest.rds", version = 2)

# saveRDS(baselineRes, "~/Documents/TTT & Diabetes/ModelOutputs/baselineResults.rds", version = 2)

# saveRDS(baselineRes, file = "~/MITUS/inst/US/UStempBaseCaseTest.rds", version =2)
allIGRAprgChng <- def_prgchng(Par[1,]); allIGRAprgChng["IGRA_frc"] <- 1; allIGRAprgChng["start_yr"] <- 2024
allIGRARes <- national_OutputsZint(samp_i = 1, ParMatrix = Par, loc = "US", prg_chng = allIGRAprgChng, endyr = 2099)

##### Population with diabetes
##### U.S.-born non-Hispanic Asian population (Total)
# usbAsianDiabTTT$NRiskGrp <- 100
# usbAsianDiabTTT$RRPrev <- 1
# usbAsianDiabTTT[["RRprg"]] <- 1.6

prms <- national_param_init(P = Par[1,],loc ="US", prg_chng = allIGRAprgChng,
                            ttt_list = list(def_ttt_nat_ag()))

usbAsianDiabRes <- national_OutputsZint(samp_i = 1, ParMatrix = Par, loc = "US",
                                        prg_chng = diabPrgChng,
                                        # prg_chng = def_prgchng(Par[1,]),
                                        care_cascade = diabCareCascade,
                                        ttt_list = list(usbAsianDiabTTT), endyr = 2099)

# usbAsianDiabTTT$NRiskGrp
# identical(usbAsianDiabRes, baselineRes)
# usbAsian_TBcases <- usbAsian_results[, "NOTIF_ALL"] + usbAsian_results[, "NOTIF_MORT_ALL"]
# usbAsian_TBcases[75:85]
# (sum(usbAsianDiabRes[75, grep("N_LtbiTxInits_", colnames(usbAsianDiabRes))[1:22]]) - sum(baselineRes[75, grep("N_LtbiTxInits_", colnames(baselineRes))[1:22]]))*1e6
# sum(usbAsianDiabRes[75, grep("N_LtbiTxInits_TP", colnames(usbAsianDiabRes))[1:22]])*1e6


# saveRDS(usbAsianDiabRes, "~/Documents/TTT & Diabetes/ModelOutputs/usbAsianTTTResults.rds", version = 2)

##### N.U.S.-born non-Hispanic Asian population (Diabal)
# nusbAsianDiabTTT$RRPrev <- 1
nusbAsianDiabRes <- national_OutputsZint(samp_i = 1, ParMatrix = Par, loc = "US",
                                         prg_chng = diabPrgChng,
                                         # prg_chng = def_prgchng(Par[1,]),
                                         # care_cascade = diabCareCascade,
                                         ttt_list = list(nusbAsianDiabTTT), endyr = 2099)

# nusbAsianDiabTTT$NRiskGrp
# identical(usbAsianDiabRes, nusbAsianDiabRes)

# nusbAsian_TBcases <- nusbAsianDiabRes[, "NOTIF_ALL"] + nusbAsianDiabRes[, "NOTIF_MORT_ALL"]
# nusbAsian_TBcases[75:85]
# plot(nusbAsian_TBcases[75:85])
# plot(nusbAsian_results[75:85, "NOTIF_ALL"])
# plot(nusbAsian_results[75:85, "NOTIF_MORT_ALL"])
# nusbAsian_TBcasesUS <- nusbAsian_results[, "NOTIF_US"] + nusbAsian_results[, "NOTIF_MORT_US"]
# plot(nusbAsian_TBcasesUS[75:85])
# nusbAsian_TBcasesNUS <- nusbAsian_results[, "NOTIF_F1"] + nusbAsian_results[, "NOTIF_F2"] +
#   nusbAsian_results[, "NOTIF_MORT_F1"] + nusbAsian_results[, "NOTIF_MORT_F2"]
# plot(nusbAsian_TBcasesNUS[75:85])



# saveRDS(nusbAsianDiabRes, "~/Documents/TTT & Diabetes/ModelOutputs/nusbAsianTTTResults.rds", version = 2)

##### U.S.-born non-Hispanic Asian population (Diabal)
usbHispanicDiabRes <- national_OutputsZint(samp_i = 1, ParMatrix = Par, loc = "US",
                                           prg_chng = diabPrgChng,
                                           # prg_chng = def_prgchng(Par[1,]),
                                           # care_cascade = diabCareCascade,
                                           ttt_list = list(usbHispanicDiabTTT), endyr = 2099)
# usbHispanicDiabTTT$NRiskGrp

# saveRDS(usbHispanicDiabRes, "~/Documents/TTT & Diabetes/ModelOutputs/usbHispanicTTTResults.rds", version = 2)

##### U.S.-born non-Hispanic Asian population (Diabal)
nusbHispanicDiabRes <- national_OutputsZint(samp_i = 1, ParMatrix = Par, loc = "US",
                                            prg_chng = diabPrgChng,
                                            # prg_chng = def_prgchng(Par[1,]),
                                            # care_cascade = diabCareCascade,
                                            ttt_list = list(nusbHispanicDiabTTT), endyr = 2099)
# nusbHispanicDiabTTT$NRiskGrp

# saveRDS(nusbHispanicDiabRes, "~/Documents/TTT & Diabetes/ModelOutputs/nusbHispanicTTTResults.rds", version = 2)


sum((baselineRes[75:150,"NOTIF_ALL"] + baselineRes[75:150,"NOTIF_MORT_ALL"]) -
      (nusbHispanicDiabRes[75:150,"NOTIF_ALL"] + nusbHispanicDiabRes[75:150,"NOTIF_MORT_ALL"])) * 1e6
