##############################################################################|
##### This code will implement targeted testing and treatment interventions
##### among Asian and Hispanic populations with and without Diabetes. It will
##### use the MITUS model.
##############################################################################|

##############################################################################|
#####                Set the seed from command line args                 #####
##############################################################################|
seed <- commandArgs(trailingOnly=TRUE)
print(seed)

##############################################################################|
#####                         Load in packages                           #####
##############################################################################|
library(MITUS, lib.loc="~/apps/R.4.2.2/tttDiabMITUS")
library(dplyr)
library(purrr)

##############################################################################|
#####                   Load in the model parameters                 #########
##############################################################################|

model_load()

##############################################################################|
#####                  Read in the IMIS parameter sets                ########
##############################################################################|

load(paste0("Inputs/imis_result_temp_07_21_24_S_", seed, ".rData"))

imisParameters0 <- imis_res_tmp$resample

##### Convert these to parameter space
Par0 <- imisParameters0
##### Create an empty matrix to hold the transformed parameters
imisParameters <- matrix(Par[1,], nrow = nrow(imisParameters0), ncol = ncol(Par), byrow = TRUE)
colnames(imisParameters) <- colnames(Par)
##to facilitate comparisons. These first two steps convert these parameters back to their
##distributions
# normal to uniform
Par2 <- pnorm(Par0,0,1)
# uniform to true
Par3 <- Par2
for (i in 1:ncol(Par3)){
  if(idZ0[i]) {
    Par3[,i] <- qbeta( Par2[,i], shape1  = ParamInitZ[i,6], shape2 = ParamInitZ[i,7])
  } else if (idZ1[i]){
    Par3[,i] <- qgamma(Par2[,i], shape   = ParamInitZ[i,6], rate   = ParamInitZ[i,7])
  } else if (idZ2[i]){
    Par3[,i] <- qnorm( Par2[,i], mean    = ParamInitZ[i,6], sd     = ParamInitZ[i,7])
  }}

imisParameters[,ii] <- Par3

###############################################################################|
#####                 Define intervention populations                 #########
###############################################################################|
##### Populations with diabetes by nativity #####
##### U.S.-born non-Hispanic Asian population (with diabetes)

usbAsianDiabTTT <- readRDS("Inputs/usbAsianDiabInputs.rds")

##### non-U.S.-born non-Hispanic Asian population (with diabetes)

nusbAsianDiabTTT <- readRDS("Inputs/nusbAsianDiabInputs.rds")

##### U.S.-born Hispanic population (with diabetes)

usbHispanicDiabTTT <- readRDS("Inputs/usbHispanicDiabInputs.rds")

##### non-U.S.-born Hispanic population (with diabetes)

nusbHispanicDiabTTT <- readRDS("Inputs/nusbHispanicDiabInputs.rds")

##############################################################################|
#####                      Define program changes                    #########
##############################################################################|
diabPrgChng <- def_prgchng(ParVec = Par[1,])
diabPrgChng["frc_3hp"] <- 0.25
diabPrgChng["frc_3hr"] <- 0.07
diabPrgChng["frc_4r"] <- 0.68
diabPrgChng["start_yr"] <- 2024
diabPrgChng["IGRA_frc"] <- 1;

##############################################################################|
#####                       Run the MITUS model                      #########
##############################################################################|
###########                   Baseline model                         #########
##############################################################################|
baselineRes <- national_OutputsInt(ParMatrix = imisParameters,
                                   loc = "US",
                                   prg_chng = diabPrgChng,
                                   ttt_list = list(def_ttt_nat_ag()),
                                   care_cascade = def_care_cascade(),
                                   endyr = 2099)

saveRDS(baselineRes, file = paste0("baselineTTTResults", seed , ".rds"), version = 2)

##############################################################################|
#########                   Population with diabetes                  #########
##############################################################################|
##### U.S.-born non-Hispanic Asian population (Diabetic)
usbAsianDiabRes <- national_OutputsInt(ParMatrix = imisParameters,
                                       loc = "US",
                                       prg_chng = diabPrgChng,
                                       care_cascade = def_care_cascade(),
                                       ttt_list = list(usbAsianDiabTTT),
                                       endyr = 2099)

saveRDS(usbAsianDiabRes, file = paste0("usbAsianTTTResults", seed , ".rds"), version = 2)

##### N.U.S.-born non-Hispanic Asian population (Diabetic)
nusbAsianDiabRes <- national_OutputsInt(ParMatrix = imisParameters,
                                        loc = "US",
                                        prg_chng = diabPrgChng,
                                        care_cascade = def_care_cascade(),
                                        ttt_list = list(nusbAsianDiabTTT), endyr = 2099)

saveRDS(nusbAsianDiabRes,  file = paste0("nusbAsianTTTResults", seed , ".rds"), version = 2)

##### U.S.-born non-Hispanic Asian population (Diabetic)
usbHispanicDiabRes <- national_OutputsInt(ParMatrix = imisParameters,
                                          loc = "US",
                                          prg_chng = diabPrgChng,
                                          care_cascade = def_care_cascade(),
                                          ttt_list = list(usbHispanicDiabTTT), endyr = 2099)

saveRDS(usbHispanicDiabRes,  file = paste0("usbHispanicTTTResults", seed , ".rds"), version = 2)

##### U.S.-born non-Hispanic Asian population (Diabetic)
nusbHispanicDiabRes <- national_OutputsInt(ParMatrix = imisParameters,
                                           loc = "US",
                                           prg_chng = diabPrgChng,
                                           care_cascade = def_care_cascade(),
                                           ttt_list = list(nusbHispanicDiabTTT), endyr = 2099)

saveRDS(nusbHispanicDiabRes, file = paste0("nusbHispanicTTTResults", seed , ".rds"), version = 2)

