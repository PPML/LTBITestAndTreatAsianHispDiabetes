##############################################################################|
#####                         Load in packages                           #####
##############################################################################|
library(TubercuCost, lib.loc="~/apps/R.4.2.2")

#############################################################################
### Define a function that wraps the individual functions
#############################################################################

generateEconRes <- function(results, intvScalar =1,
                            seed = 1){

  print(seed)
  set.seed(seed)

  #############################################################################
  ### Setup
  #############################################################################

  yearIndex <- 75:150

  ##### Cases
  caseAgeIndex <- grep("NOTIF_\\d+", colnames(results))
  caseMortAgeIndex <- grep("NOTIF_MORT_\\d+", colnames(results))

  casesAgeAnnual <- data.frame("Year" = 1950:2099,
                               "Scenario" = "",
                               results[,caseAgeIndex] + results[,caseMortAgeIndex],
                               check.names = FALSE)

  ##### Deaths
  ageDeathIndex <- grep("TOTMORT_W_TB", colnames(results))

  deathsAgeAnnual <- data.frame("Year" = 1950:2099,
                                "Scenario" = "",
                                results[,ageDeathIndex],
                                check.names = FALSE)

  ##### LTBI tests
  ageLTBITestIndex <- grep("N_LtbiTests_", colnames(results))

  ltbiTestAgeAnnual <- data.frame("Year" = 1950:2099,
                                  "Scenario" = "",
                                  (results[,ageLTBITestIndex[1:11]] + results[,ageLTBITestIndex[12:22]]) *intvScalar,
                                  check.names = FALSE)

  ##### LTBI treatment initiations

  ageLTBITxIndex <- grep("N_LtbiTxInits_", colnames(results))

  ltbiTxInitAgeAnnual <- data.frame("Year" = 1950:2099,
                                    "Scenario" = "",
                                    (results[,ageLTBITxIndex[1:11]] + results[,ageLTBITxIndex[12:22]]) * intvScalar,
                                    check.names = FALSE)

  #############################################################################
  ### Call cost/effect functions
  #############################################################################

  ### Productivity costs
  ProdCosts <- calculateProdCost(TbCases = casesAgeAnnual[yearIndex,-c(1:2)],
                                 TbDeaths = deathsAgeAnnual[yearIndex,-c(1:2)],
                                 LtbiTxInits = ltbiTxInitAgeAnnual[yearIndex,-c(1:2)],
                                 discount = 0,
                                 Ages = "AgeGroups",
                                 StartYear = 2024,
                                 uncertainty = TRUE)

  discount_ProdCosts <- calculateProdCost(TbCases = casesAgeAnnual[yearIndex,-c(1:2)],
                                          TbDeaths = deathsAgeAnnual[yearIndex,-c(1:2)],
                                          LtbiTxInits = ltbiTxInitAgeAnnual[yearIndex,-c(1:2)],
                                          discount = 0.03,
                                          Ages = "AgeGroups",
                                          StartYear = 2024,
                                          uncertainty = TRUE)
  ### Health service costs

  HealthCosts <- calculateHealthCost(TbCases = casesAgeAnnual[yearIndex,-c(1:2)],
                                     TbDeaths = deathsAgeAnnual[yearIndex,-c(1:2)],
                                     LtbiTests = ltbiTestAgeAnnual[yearIndex,-c(1:2)],
                                     LtbiTxInits = ltbiTxInitAgeAnnual[yearIndex,-c(1:2)],
                                     discount = 0, IGRA_frc = 1.0,
                                     Ages = "AgeGroups",
                                     StartYear = 2024,
                                     uncertainty = TRUE)

  discount_HealthCosts <- calculateHealthCost(TbCases = casesAgeAnnual[yearIndex,-c(1:2)],
                                              TbDeaths = deathsAgeAnnual[yearIndex,-c(1:2)],
                                              LtbiTests = ltbiTestAgeAnnual[yearIndex,-c(1:2)],
                                              LtbiTxInits = ltbiTxInitAgeAnnual[yearIndex,-c(1:2)],
                                              discount = 0.03, IGRA_frc = 1.0,
                                              Ages = "AgeGroups",
                                              StartYear = 2024,
                                              uncertainty = TRUE)

  ### QALYs

  QALYS <- calculateQALYs(TbCases = casesAgeAnnual[yearIndex,-c(1:2)],
                          TbDeaths = deathsAgeAnnual[yearIndex,-c(1:2)],
                          LtbiTxInits = ltbiTxInitAgeAnnual[yearIndex,-c(1:2)],
                          discount = 0,
                          Ages = "AgeGroups",
                          StartYear = 2024,
                          uncertainty = TRUE)

  discount_QALYS <- calculateQALYs(TbCases = casesAgeAnnual[yearIndex,-c(1:2)],
                                   TbDeaths = deathsAgeAnnual[yearIndex,-c(1:2)],
                                   LtbiTxInits = ltbiTxInitAgeAnnual[yearIndex,-c(1:2)],
                                   discount = 0.03,
                                   Ages = "AgeGroups",
                                   StartYear = 2024,
                                   uncertainty = TRUE)

  #############################################################################
  ### Create a dataframe of all results
  #############################################################################

  econResults <- c( "QALYs" = sum(QALYS[[4]])*1e6,
                    "Discount QALYs" = sum(discount_QALYS[[4]])*1e6,
                    "Life years" = sum(QALYS[[3]])*1e6,
                    "Discount life years" = sum(discount_QALYS[[3]])*1e6,
                    "TB health costs" = sum(HealthCosts[[1]], HealthCosts[[2]])*1e6,
                    "Discount TB health costs" = sum(discount_HealthCosts[[1]], discount_HealthCosts[[2]])*1e6,
                    "Health costs" = sum(HealthCosts[[4]])*1e6,
                    "Discount health costs" = sum(discount_HealthCosts[[4]])*1e6,
                    "Productivity costs" = sum(ProdCosts[[4]])*1e6,
                    "Discount productivity costs" = sum(discount_ProdCosts[[4]])*1e6)
  return(econResults)
}


#############################################################################
### Read in the results
#############################################################################
### Create a vector of strings that will be used to set up the run
popVec <- c("baseline", "usbAsian", "nusbAsian",
            "usbHispanic", "nusbHispanic")

pop.i <- as.numeric(commandArgs(trailingOnly=TRUE))

pop <-  popVec[pop.i]

results0 <- readRDS(paste0("fullModelRes/", pop, "TTTResultsFiltered.rds"))
if(pop != "baseline") {
  resultsBC <- readRDS(paste0("fullModelRes/baselineTTTResultsFiltered.rds"))
  popIntvScalar <- - 1
} else {
  resultsBC <- array(0, dim = dim(results0))
  popIntvScalar <- 1
}

# results0 <- readRDS(paste0("~/Desktop/", pop, "TTTResultsFiltered.rds"))
# if(pop != "baseline") {
#   resultsBC <- readRDS(paste0("~/Desktop/baselineTTTResultsFiltered.rds"))
#   popIntvScalar <- - 1
# } else {
#   resultsBC <- array(0, dim = dim(results0))
#   popIntvScalar <- 1
#
# }

# usbHispanic_results <- readRDS("~/Documents/TTT & Diabetes/ModelOutputs/Quantiles/usbHispanicTTTResultsQuantiles.rds")
############################################################################
##### generate vectors of random seeds that can be used
#############################################################################
originalSeed <- 123
set.seed(originalSeed)
seedVec <- sample(1:10000, dim(results0)[1], replace = FALSE)

#############################################################################
### Apply function across the 3D vector of results
#############################################################################

# fullEconRes <- apply(results0, c(1), generateEconRes, seedVec)

tmpEconRes <- matrix(NA, nrow = length(seedVec), ncol = 10)

for(i in 1:length(seedVec)){
  tmpRes <- cbind(results0[i,,1], resultsBC[i,,-1] - results0[i,,-1])
  colnames(tmpRes)[1] <- "Year"
  seed <- seedVec[i]
  if(i == 1){
    colnames(tmpEconRes) <- names(generateEconRes(tmpRes))
  }
  tmpEconRes[i,] <- generateEconRes(results = tmpRes,
                                    seed = seed,
                                    intvScalar = popIntvScalar)
}

tmpEconResList <- list("seedVec" = seedVec,
                       "econResults" = tmpEconRes)

saveRDS(tmpEconResList, file = paste0("fullModelRes/",pop, "econResultsAll.rds"), version = 2)

econResIntv <- apply(tmpEconRes, 2, quantile, prob = c(0.025, 0.5, 0.975))
rownames(econResIntv) <- c("Lower bound", "Median", "Upper bound")
colnames(econResIntv) <- colnames(tmpEconRes)

saveRDS(econResIntv, file = paste0("fullModelRes/",pop, "econResultsIntvs.rds"), version = 2)

