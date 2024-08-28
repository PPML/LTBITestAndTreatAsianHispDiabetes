generateICER <- function(results,
                         UnitCosts,
                         utilityWeights){

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
                                            (results[,ageLTBITestIndex[1:11]] + results[,ageLTBITestIndex[12:22]]),
                                            check.names = FALSE)

    ##### LTBI treatment initiations

    ageLTBITxIndex <- grep("N_LtbiTxInits_", colnames(results))

    ltbiTxInitAgeAnnual <- data.frame("Year" = 1950:2099,
                                              "Scenario" = "",
                                              (results[,ageLTBITxIndex[1:11]] + results[,ageLTBITxIndex[12:22]]),
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
                                            uncertainty = FALSE)

    discount_ProdCosts <- calculateProdCost(TbCases = casesAgeAnnual[yearIndex,-c(1:2)],
                                            TbDeaths = deathsAgeAnnual[yearIndex,-c(1:2)],
                                            LtbiTxInits = ltbiTxInitAgeAnnual[yearIndex,-c(1:2)],
                                            discount = 0.03,
                                            Ages = "AgeGroups",
                                            StartYear = 2024,
                                            uncertainty = FALSE)
    ### Health service costs

    HealthCosts <- calculateHealthCost(TbCases = casesAgeAnnual[yearIndex,-c(1:2)],
                                                TbDeaths = deathsAgeAnnual[yearIndex,-c(1:2)],
                                                LtbiTests = ltbiTestAgeAnnual[yearIndex,-c(1:2)],
                                                LtbiTxInits = ltbiTxInitAgeAnnual[yearIndex,-c(1:2)],
                                                discount = 0, IGRA_frc = 1.0,
                                                tx_dist = c(0.25, 0.07, 0.68),
                                                Ages = "AgeGroups",
                                                StartYear = 2024,
                                                uncertainty = FALSE,
                                                UnitCosts = UnitCosts)

    discount_HealthCosts <- calculateHealthCost(TbCases = casesAgeAnnual[yearIndex,-c(1:2)],
                                                TbDeaths = deathsAgeAnnual[yearIndex,-c(1:2)],
                                                LtbiTests = ltbiTestAgeAnnual[yearIndex,-c(1:2)],
                                                LtbiTxInits = ltbiTxInitAgeAnnual[yearIndex,-c(1:2)],
                                                discount = 0.03, IGRA_frc = 1.0,
                                                tx_dist = c(0.25, 0.07, 0.68),
                                                Ages = "AgeGroups",
                                                StartYear = 2024,
                                                uncertainty = FALSE,
                                                UnitCosts = UnitCosts)

    ### QALYs
    QALYS <- calculateQALYs(TbCases = casesAgeAnnual[yearIndex,-c(1:2)],
                                     TbDeaths = deathsAgeAnnual[yearIndex,-c(1:2)],
                                     LtbiTxInits = ltbiTxInitAgeAnnual[yearIndex,-c(1:2)],
                                     discount = 0,
                                     Ages = "AgeGroups",
                                     StartYear = 2024,
                                     uncertainty = FALSE,
                                     utilityWeights = utilityWeights)

    discount_QALYS <- calculateQALYs(TbCases = casesAgeAnnual[yearIndex,-c(1:2)],
                                     TbDeaths = deathsAgeAnnual[yearIndex,-c(1:2)],
                                     LtbiTxInits = ltbiTxInitAgeAnnual[yearIndex,-c(1:2)],
                                     discount = 0.03,
                                     Ages = "AgeGroups",
                                     StartYear = 2024,
                                     uncertainty = FALSE,
                                     utilityWeights = utilityWeights)

    #############################################################################
    ### Create a dataframe of all results
    #############################################################################

    econResults <- c("QALYs" = sum(QALYS[[4]])*1e6,
                      "Discount QALYs" = sum(discount_QALYS[[4]])*1e6,
                      "Life years" = sum(QALYS[[3]])*1e6,
                      "Discount life years" = sum(discount_QALYS[[3]])*1e6,
                      "TB health costs" = sum(HealthCosts[[1]], HealthCosts[[2]])*1e6,
                      "Discount TB health costs" = sum(discount_HealthCosts[[1]], discount_HealthCosts[[2]])*1e6,
                      "ICER (TB perspective)" = sum(discount_HealthCosts[[1]], discount_HealthCosts[[2]]) / sum(discount_QALYS[[4]]),
                      "Health costs" = sum(HealthCosts[[4]])*1e6,
                      "Discount health costs" = sum(discount_HealthCosts[[4]])*1e6,
                      "ICER (Health service perspective)" = sum(discount_HealthCosts[[4]]) / sum(discount_QALYS[[4]]),
                      "Productivity costs" = sum(ProdCosts[[4]])*1e6,
                      "Discount productivity costs" = sum(discount_ProdCosts[[4]])*1e6,
                      "ICER (Societal perspective)" = sum(discount_ProdCosts[[4]]) / sum(discount_QALYS[[4]])
    )
    return(econResults)
}


###############################################################################
differenceOutcomes <- function(mainResults,
                               LowerBoundResults,
                               UpperBoundResults){
  differencesCases <- data.frame("LTBI completion" = c(sum(tempResSA[["CompLow"]][75:150,"NOTIF_ALL"], tempResSA[["CompLow"]][75:150,"NOTIF_MORT_ALL"]),
                                                       sum(tempResSA[["CompHigh"]][75:150,"NOTIF_ALL"], tempResSA[["CompHigh"]][75:150,"NOTIF_MORT_ALL"]),
                                                       sum(tempResSA[["Main"]][75:150,"NOTIF_ALL"] - tempResSA[["CompHigh"]][75:150,"NOTIF_ALL"],
                                                           tempResSA[["Main"]][75:150,"NOTIF_MORT_ALL"] - tempResSA[["CompHigh"]][75:150,"NOTIF_MORT_ALL"])),
                                 "Initiation" = c(sum(tempResSA[["InitLow"]][75:150,"NOTIF_ALL"], tempResSA[["InitLow"]][75:150,"NOTIF_MORT_ALL"]),
                                                  sum(tempResSA[["InitHigh"]][75:150,"NOTIF_ALL"], tempResSA[["InitHigh"]][75:150,"NOTIF_MORT_ALL"]),
                                                  sum(tempResSA[["Main"]][75:150,"NOTIF_ALL"] - tempResSA[["InitHigh"]][75:150,"NOTIF_ALL"],
                                                      tempResSA[["Main"]][75:150,"NOTIF_MORT_ALL"] - tempResSA[["InitHigh"]][75:150,"NOTIF_MORT_ALL"])),
                                 "Tx effectiveness" = c(sum(tempResSA[["EffLow"]][75:150,"NOTIF_ALL"], tempResSA[["EffLow"]][75:150,"NOTIF_MORT_ALL"]),
                                                        sum(tempResSA[["EffHigh"]][75:150,"NOTIF_ALL"], tempResSA[["EffHigh"]][75:150,"NOTIF_MORT_ALL"]),
                                                        sum(tempResSA[["Main"]][75:150,"NOTIF_ALL"] - tempResSA[["EffHigh"]][75:150,"NOTIF_ALL"],
                                                            tempResSA[["Main"]][75:150,"NOTIF_MORT_ALL"] - tempResSA[["EffHigh"]][75:150,"NOTIF_MORT_ALL"])),
                                 "Sensitivity" = c(sum(tempResSA[["SensLow"]][75:150,"NOTIF_ALL"], tempResSA[["SensLow"]][75:150,"NOTIF_MORT_ALL"]),
                                                   sum(tempResSA[["SensHigh"]][75:150,"NOTIF_ALL"], tempResSA[["SensHigh"]][75:150,"NOTIF_MORT_ALL"]),
                                                   sum(tempResSA[["Main"]][75:150,"NOTIF_ALL"] - tempResSA[["SensHigh"]][75:150,"NOTIF_ALL"],
                                                       tempResSA[["Main"]][75:150,"NOTIF_MORT_ALL"] - tempResSA[["SensHigh"]][75:150,"NOTIF_MORT_ALL"])),
                                 "Specificity" = c(sum(tempResSA[["SpecLow"]][75:150,"NOTIF_ALL"], tempResSA[["SpecLow"]][75:150,"NOTIF_MORT_ALL"]),
                                                   sum(tempResSA[["SpecHigh"]][75:150,"NOTIF_ALL"], tempResSA[["SpecHigh"]][75:150,"NOTIF_MORT_ALL"]),
                                                   sum(tempResSA[["Main"]][75:150,"NOTIF_ALL"] - tempResSA[["SpecHigh"]][75:150,"NOTIF_ALL"],
                                                       tempResSA[["Main"]][75:150,"NOTIF_MORT_ALL"] - tempResSA[["SpecHigh"]][75:150,"NOTIF_MORT_ALL"])),
                                 "Mortality" = c(sum(tempResSA[["MortLow"]][75:150,"NOTIF_ALL"], tempResSA[["MortLow"]][75:150,"NOTIF_MORT_ALL"]),
                                                 sum(tempResSA[["MortHigh"]][75:150,"NOTIF_ALL"], tempResSA[["MortHigh"]][75:150,"NOTIF_MORT_ALL"]),
                                                 sum(tempResSA[["Main"]][75:150,"NOTIF_ALL"] - tempResSA[["MortHigh"]][75:150,"NOTIF_ALL"],
                                                     tempResSA[["Main"]][75:150,"NOTIF_MORT_ALL"] - tempResSA[["MortHigh"]][75:150,"NOTIF_MORT_ALL"])),
                                 "TB progression" = c(sum(tempResSA[["ProgLow"]][75:150,"NOTIF_ALL"], tempResSA[["ProgLow"]][75:150,"NOTIF_MORT_ALL"]),
                                                      sum(tempResSA[["ProgHigh"]][75:150,"NOTIF_ALL"], tempResSA[["ProgHigh"]][75:150,"NOTIF_MORT_ALL"]),
                                                      sum(tempResSA[["Main"]][75:150,"NOTIF_ALL"] - tempResSA[["ProgHigh"]][75:150,"NOTIF_ALL"],
                                                          tempResSA[["Main"]][75:150,"NOTIF_MORT_ALL"] - tempResSA[["ProgHigh"]][75:150,"NOTIF_MORT_ALL"])),
                                 "LTBI prevalence" = c(sum(tempResSA[["LtbiLow"]][75:150,"NOTIF_ALL"], tempResSA[["LtbiLow"]][75:150,"NOTIF_MORT_ALL"]),
                                                       sum(tempResSA[["LtbiHigh"]][75:150,"NOTIF_ALL"], tempResSA[["LtbiHigh"]][75:150,"NOTIF_MORT_ALL"]),
                                                       sum(tempResSA[["Main"]][75:150,"NOTIF_ALL"] - tempResSA[["LtbiHigh"]][75:150,"NOTIF_ALL"],
                                                           tempResSA[["Main"]][75:150,"NOTIF_MORT_ALL"] - tempResSA[["LtbiHigh"]][75:150,"NOTIF_MORT_ALL"])),
                                 check.names = FALSE)

  rownames(differencesCases) <- c("Lower bound", "Upper bound", "UB diff")

  differencesCases <- as.data.frame(t(differencesCases)) *1e6
  differencesCases$Parameter <- rownames(differencesCases)
}
