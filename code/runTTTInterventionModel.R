###############################################################################
##### This code will implement targeted testing and treatment interventions
##### among Asian and Hispanic populations with and without Diabetes. It will
##### use the MITUS model.
###############################################################################

### Make sure to load all dependencies, helper functions, and color palette and 
### set working directory by first running the setupEnvironAndDeps.R file!

###############################################################################
##### Load in MITUS model data
###############################################################################

model_load(loc = "US")

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

###############################################################################
##### Define intervention populations
###############################################################################
##### Populations with diabetes by nativity #####
##### U.S.-born non-Hispanic Asian population (with diabetes)
usbAsianDiabTTT <- def_ttt_nat_ag()
usbAsianDiabTTT[["NRiskGrp"]] <- sum(asianDiabPop %>%
                                    filter(Nativity == "USB") %>%
                                     dplyr::select(`Population with diabetes`))
usbAsianDiabTTT[["AgeDistUS"]] <- as.vector(unlist((asianDiabPop %>%
                                      filter(Nativity == "USB") %>%
                                      dplyr::select(`Population with diabetes`)) /
                                 usbAsianDiabTTT[["NRiskGrp"]]))
usbAsianDiabTTT[["AgeDistNUS"]] <- rep(0,11)
usbAsianDiabTTT[["FrcScrn"]] <- tttFrcScrn
usbAsianDiabTTT[["StartYr"]] <- 2024
usbAsianDiabTTT[["EndYr"]] <- 2025
usbAsianDiabTTT[["RRprg"]] <- diabProgressionRR
usbAsianDiabTTT[["RRmu"]] <- diabMortRR
usbAsianDiabTTT[["RRprev"]] <- usbAsianDiabLtbiRR

##### non-U.S.-born non-Hispanic Asian population (with diabetes)
nusbAsianDiabTTT <- def_ttt_nat_ag()
nusbAsianDiabTTT[["NRiskGrp"]] <- sum(asianDiabPop %>%
                                        filter(Nativity == "NUSB") %>%
                                        dplyr::select(`Population with diabetes`))
nusbAsianDiabTTT[["AgeDistNUS"]] <- as.vector(unlist((asianDiabPop %>%
                                      filter(Nativity == "NUSB") %>%
                                      dplyr::select(`Population with diabetes`)) /
                                  nusbAsianDiabTTT[["NRiskGrp"]]))
nusbAsianDiabTTT[["AgeDistUS"]] <- rep(0,11)
nusbAsianDiabTTT[["FrcScrn"]] <- tttFrcScrn
nusbAsianDiabTTT[["StartYr"]] <- 2024
nusbAsianDiabTTT[["EndYr"]] <- 2025
nusbAsianDiabTTT[["RRprg"]] <- diabProgressionRR
nusbAsianDiabTTT[["RRmu"]] <- diabMortRR
nusbAsianDiabTTT[["RRprev"]] <- nusbAsianDiabLtbiRR

##### U.S.-born Hispanic population (with diabetes)
usbHispanicDiabTTT <- def_ttt_nat_ag()
usbHispanicDiabTTT[["NRiskGrp"]] <- sum(hispanicDiabPop %>%
                                         filter(Nativity == "USB") %>%
                                         dplyr::select(`Population with diabetes`))
usbHispanicDiabTTT[["AgeDistUS"]] <- as.vector(unlist((hispanicDiabPop %>%
                                       filter(Nativity == "USB") %>%
                                       dplyr::select(`Population with diabetes`)) /
    usbHispanicDiabTTT[["NRiskGrp"]]))
usbHispanicDiabTTT[["AgeDistNUS"]] <- rep(0,11)
usbHispanicDiabTTT[["FrcScrn"]] <- tttFrcScrn
usbHispanicDiabTTT[["StartYr"]] <- 2024
usbHispanicDiabTTT[["EndYr"]] <- 2025
usbHispanicDiabTTT[["RRprg"]] <- diabProgressionRR
usbHispanicDiabTTT[["RRmu"]] <- diabMortRR
usbHispanicDiabTTT[["RRprev"]] <- usbHispanicDiabLtbiRR

##### non-U.S.-born Hispanic population (with diabetes)
nusbHispanicDiabTTT <- def_ttt_nat_ag()
nusbHispanicDiabTTT[["NRiskGrp"]] <- sum(hispanicDiabPop %>%
                                          filter(Nativity == "NUSB") %>%
                                          dplyr::select(`Population with diabetes`))
nusbHispanicDiabTTT[["AgeDistNUS"]] <- as.vector(unlist((hispanicDiabPop %>%
                                        filter(Nativity == "NUSB") %>%
                                        dplyr::select(`Population with diabetes`)) /
    nusbHispanicDiabTTT[["NRiskGrp"]]))
nusbHispanicDiabTTT[["AgeDistUS"]] <- rep(0,11)
nusbHispanicDiabTTT[["FrcScrn"]] <- tttFrcScrn
nusbHispanicDiabTTT[["StartYr"]] <- 2024
nusbHispanicDiabTTT[["EndYr"]] <- 2025
nusbHispanicDiabTTT[["RRprg"]] <- diabProgressionRR
nusbHispanicDiabTTT[["RRmu"]] <- diabMortRR
nusbHispanicDiabTTT[["RRprev"]] <- nusbHispanicDiabLtbiRR

###############################################################################
##### Run the MITUS model
###############################################################################
##### Baseline model
baselineRes <- national_OutputsZint(samp_i = 1, ParMatrix = Par, loc = "US")
allIGRAprgChng <- def_prgchng(Par[1,]); allIGRAprgChng["IGRA_frc"] <- 1
allIGRARes <- national_OutputsZint(samp_i = 1, ParMatrix = Par, loc = "US", prg_chng = allIGRAprgChng)

scrnCovprgChng <- def_prgchng(Par[1,]); scrnCovprgChng["scrn_cov"] <- .5
scrnCovRes <- national_OutputsZint(samp_i = 1, ParMatrix = Par, loc = "US", prg_chng = scrnCovprgChng)
##### Total population
##### U.S.-born non-Hispanic Asian population (Total)
# usbAsianTotRes <- OutputsZint(samp_i = 1, ParMatrix = Par, loc = "US",
#                               prg_chng = usbPrgChng, care_cascade = usbCareCascade,
#                               ttt_list = usbAsianTotTTT)
#
# ##### U.S.-born non-Hispanic Asian population (Total)
# nusbAsianTotRes <- OutputsZint(samp_i = 1, ParMatrix = Par, loc = "US",
#                               prg_chng = nusbPrgChng, care_cascade = nusbCareCascade,
#                               ttt_list = nusbAsianTotTTT)
#
# ##### U.S.-born non-Hispanic Asian population (Total)
# usbHispanicTotRes <- OutputsZint(samp_i = 1, ParMatrix = Par, loc = "US",
#                               prg_chng = usbPrgChng, care_cascade = usbCareCascade,
#                               ttt_list = usbHispanicTotTTT)
#
# ##### U.S.-born non-Hispanic Asian population (Total)
# nusbHispanicTotRes <- OutputsZint(samp_i = 1, ParMatrix = Par, loc = "US",
#                               prg_chng = nusbPrgChng, care_cascade = nusbCareCascade,
#                               ttt_list = nusbHispanicTotTTT)

##### Population with diabetes
##### U.S.-born non-Hispanic Asian population (Total)
usbAsianDiabRes <- national_OutputsZint(samp_i = 1, ParMatrix = Par, loc = "US",
                                        prg_chng = diabPrgChng,
                                        care_cascade = diabCareCascade,
                                        ttt_list = list(usbAsianDiabTTT))
usbAsianDiabTTT$NRiskGrp

##### N.U.S.-born non-Hispanic Asian population (Diabal)
nusbAsianDiabRes <- national_OutputsZint(samp_i = 1, ParMatrix = Par, loc = "US",
                               prg_chng = diabPrgChng, care_cascade = diabCareCascade,
                               ttt_list = list(nusbAsianDiabTTT))
nusbAsianDiabTTT$NRiskGrp

##### U.S.-born non-Hispanic Asian population (Diabal)
usbHispanicDiabRes <- national_OutputsZint(samp_i = 1, ParMatrix = Par, loc = "US",
                                 prg_chng = diabPrgChng, care_cascade = diabCareCascade,
                                 ttt_list = list(usbHispanicDiabTTT))

##### U.S.-born non-Hispanic Asian population (Diabal)
nusbHispanicDiabRes <- national_OutputsZint(samp_i = 1, ParMatrix = Par, loc = "US",
                                  prg_chng = diabPrgChng, care_cascade = diabCareCascade,
                                  ttt_list = list(nusbHispanicDiabTTT))

###############################################################################
##### Calculate the outcome differences
###############################################################################
##### Define a outcome difference function
case_diff <- function(results1 = baselineRes,
                      results2,
                      outcomeIndexVec){
      if(length(outcomeIndexVec))
      outDiff <-  rowSum(results2[,outcomeIndexVec]) - rowSum(results1[,outcomeIndexVec])
}
##### TB cases


##### TB deaths
