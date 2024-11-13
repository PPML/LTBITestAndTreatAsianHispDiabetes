###############################################################################
### This script queries IPUMS for population estimates from the 2022 5YR ACS.
### It collects non-Hispanic Asian and Hispanic population estimates.
### It then formats that data into the strata necessary for input
### into the MITUS model. Specifically:
### U.S.-born vs. non-U.S.-born
### 10 year age groups (0-4, 5-14, 15-25, ..., 85-94, 95p)
### Finally, it saves those populations to RDS files for use in the analysis.
###############################################################################

### Make sure to load all dependencies, helper functions, and color palette and
### set working directory by first running the setupEnvironAndDeps.R file!
setwd("~/TTTAsianHispDiabetes/LTBITestAndTreatAsianHispDiabetes/")
###############################################################################
### Asian population
### Define the query to the ACS
Asian_ext_def <- define_extract_usa(
    description = "2022 non-Hispanic Asian Population",
    samples = c("us2022c"),
    variables = list(
                "AGE", "CITIZEN",
                var_spec("HISPAN", case_selections = "0"), ### non-Hispanic
                var_spec("RACASIAN", case_selections = "2"), ### Asian Race
                "REPWT"
               ))
### Submit the extract request
Asian_ext_submitted <- submit_extract(Asian_ext_def)

### Check if abstract is ready to download
### If yes, then download the extract

downloaded <- FALSE
while (downloaded == FALSE) {
  if(is_extract_ready(Asian_ext_submitted)==TRUE){
    print("Extract ready. Downloading now: ")
    filepathAsian <- download_extract(Asian_ext_submitted)
    downloaded <- TRUE
  } else {
    print("Extract is not ready for download. Will try again in 1 minute.")
    Sys.sleep(60)
  }
}


### Read the data extract
### Data extract usa:67
AsianPop0 <- read_ipums_micro(filepathAsian)

### Create a nativity variable
AsianPop0 <- AsianPop0 %>% mutate("Nativity" = ifelse(CITIZEN < 2, "USB", "NUSB"),
                                 "Age group" = cut(AGE, breaks=c(-Inf,4,14,24,34,44,54,64,74,84,94,Inf),
                                                        labels=c("0-4","5-14", "15-24", "25-34", "35-44",
                                                                 "45-54","55-64","65-74","75-84","85-94","95p")))
### Aggregate across single year of age and nativity
### Mean estimate
asianPop <- aggregate(PERWT~AGE+Nativity, AsianPop0, sum)

### Aggregate across age group and nativity
### Mean estimate
asianPopAgeGrpNat <- aggregate(PERWT~ `Age group` + Nativity, AsianPop0, sum)

### Aggregate across nativity
### Mean estimate
asianPopTotNat <- aggregate(PERWT~Nativity, AsianPop0, sum)

### All replicate weights
allRepWgtsAsianPop <- asianPop[,1:2]
allRepWgtsAsianPopAgeGrpNat <- asianPopAgeGrpNat[,1:2]
allRepWgtsAsianPopTotNat <- as.data.frame(asianPopTotNat[,1])

### Find the first replicate weight index
wgtIndx <- which(colnames(AsianPop0) == "REPWT1") - 1

### Aggregate across the replicate weights
for (i in 1:80){
    allRepWgtsAsianPop[,i+2] <- aggregate(get(colnames(AsianPop0)[wgtIndx+i])~AGE+Nativity, AsianPop0, sum)[,3]
    allRepWgtsAsianPopTotNat[,i+1] <- aggregate(get(colnames(AsianPop0)[wgtIndx+i])~Nativity, AsianPop0, sum)[,2]
    allRepWgtsAsianPopAgeGrpNat[,i+2] <- aggregate(get(colnames(AsianPop0)[wgtIndx+i])~`Age group`+ Nativity, AsianPop0, sum)[,3]
}

### Calculate the standard error
### function for square differences
squareDiff <- function(x, xMean){
    return((x-xMean)^2)
}
### Single year of age SE
asianPop$SE <- sqrt(4/80*rowSums(apply(allRepWgtsAsianPop[,3:82], MARGIN = 2, squareDiff, xMean=asianPop$PERWT)))

### Age groups SE
asianPopAgeGrpNat$SE<- sqrt(4/80*rowSums(apply(allRepWgtsAsianPopAgeGrpNat[,3:82], MARGIN = 2, squareDiff, xMean=asianPopAgeGrpNat$PERWT)))

### Nativity totals of Asian Population SE
asianPopTotNat$SE <-  sqrt(4/80*rowSums(apply(allRepWgtsAsianPopTotNat[,2:81], MARGIN = 2, squareDiff, xMean=asianPopTotNat[,2])))

### Print range of standard errors
range(asianPop$SE/asianPop$PERWT*100)
range(asianPopTotNat$SE/asianPopTotNat$PERWT*100)

### Remove the downloaded files
### If information is needed again, it will be stored in the IPUMS history
file.remove(Sys.glob(paste0(strsplit(filepathAsian, split = "[.]")[[1]][1],"*")))
### Remove the original files to free memory
rm(AsianPop0)

#### confidence interval
(asianPopTotNat[,2] - (1.96*asianPopTotNat[,3]))/1e6 ###lower bound
(asianPopTotNat[,2] + (1.96*asianPopTotNat[,3]))/1e6 ### upper bound

###############################################################################
### Output total populations ###
print(paste("Total Asian population is", sum(asianPop$PERWT)/1e6, "million"))
print(paste("Total nusbAsian population is", sum(asianPop$PERWT[1:98])/1e6, "million"))
print(paste("15+ nusbAsian population is", sum(asianPop$PERWT[16:98])/1e6, "million"))

print(paste("Total usbAsian population is", sum(asianPop$PERWT[99:195])/1e6, "million"))
print(paste("15+ usbAsian population is", sum(asianPop$PERWT[114:195])/1e6, "million"))

### Output percentage of population over 65 ###
# print(paste0(round(sum((filter(asianPop %>% filter(AGE > 64)))[,2])/sum(asianPop$PERWT)*100,2), "% of the Asian population is 65+"))
# print(paste0(round(sum((filter(nusbAsianPop %>% filter(AGE > 64)))[,2])/sum(nusbAsianPop$PERWT)*100,2), "% of the nusbAsian population is 65+"))

### Output percentage of population non-USB ###
# print(paste0(round(sum((filter(asianPop %>% filter(Nativity == "NUSB")))[,2])/sum(asianPop$PERWT)*100,2), "% of the Asian population is NUSB"))

### Output population non-USB ###
# print(paste(sum((asianPop %>% filter(Nativity == "NUSB"))[,2])/1e6, "million of the Asian population is NUSB"))
### Output percentage of non-USB population that is 65p ###
# print(paste0(round(sum((asianPop %>% filter(Nativity == "NUSB" & AGE > 64))[,2])/
#                        sum((asianPop %>% filter(Nativity == "NUSB"))[,2])*100,2), "% of the NUSB Asian population is 65+"))


###############################################################################
### Hispanic population
### Define the query to the ACS
Hispanic_ext_def <- define_extract_usa(
    description = "2022 Hispanic Population",
    samples = c("us2022c"),
    variables = list(
        "AGE", "CITIZEN",
        var_spec("HISPAN", case_selections = c("1", "2", "3", "4")), ### Hispanic
        "REPWT"
    )
)
### Submit the extract request
Hispanic_ext_submitted <- submit_extract(Hispanic_ext_def)

### Check if abstract is ready to download
### If yes, then download the extract
downloaded <- FALSE
while (downloaded == FALSE) {
  if(is_extract_ready(Hispanic_ext_submitted)==TRUE){
    print("Extract ready. Downloading now: ")
    filepathHispanic<- download_extract(Hispanic_ext_submitted)
    downloaded <- TRUE
} else {
  print("Extract is not ready for download. Will try again in 1 minute.")
  Sys.sleep(60)
}
}
### Read the data extract
### Data extract usa:68
HispanicPop0 <- read_ipums_micro(filepathHispanic)

### Create a nativity variable
HispanicPop0 <- HispanicPop0 %>% mutate("Nativity" = ifelse(CITIZEN < 2, "USB", "NUSB"),
                                        "Age group" = cut(AGE, breaks=c(-Inf,4,14,24,34,44,54,64,74,84,94,Inf),
                                                    labels=c("0-4","5-14", "15-24", "25-34", "35-44",
                                                             "45-54","55-64","65-74","75-84","85-94","95p")))
### Aggregate across single year of age and nativity
### Mean estimate
hispanicPop <- aggregate(PERWT~AGE+Nativity, HispanicPop0, sum)

### Aggregate across age group and nativity
### Mean estimate
hispanicPopAgeGrpNat <- aggregate(PERWT~ `Age group` + Nativity, HispanicPop0, sum)

### Aggregate across nativity
### Mean estimate
hispanicPopTotNat <- aggregate(PERWT~Nativity, HispanicPop0, sum)

### All replicate weights
allRepWgtsHispanicPop <- hispanicPop[,1:2]
allRepWgtsHispanicPopAgeGrpNat <- hispanicPopAgeGrpNat[,1:2]
allRepWgtsHispanicPopTotNat <- as.data.frame(hispanicPopTotNat[,1])

### Find the first replicate weight index
wgtIndx <- which(colnames(HispanicPop0) == "REPWT1") - 1

### Aggregate across the replicate weights
for (i in 1:80){
    allRepWgtsHispanicPop[,i+2] <- aggregate(get(colnames(HispanicPop0)[wgtIndx+i])~AGE+Nativity, HispanicPop0, sum)[,3]
    allRepWgtsHispanicPopTotNat[,i+1] <- aggregate(get(colnames(HispanicPop0)[wgtIndx+i])~Nativity, HispanicPop0, sum)[,2]
    allRepWgtsHispanicPopAgeGrpNat[,i+2] <- aggregate(get(colnames(HispanicPop0)[wgtIndx+i])~`Age group`+ Nativity, HispanicPop0, sum)[,3]
}

### Calculate the standard error
### Single year of age SE
hispanicPop$SE <- sqrt(4/80*rowSums(apply(allRepWgtsHispanicPop[,3:82], MARGIN = 2, squareDiff, xMean=hispanicPop$PERWT)))

### Age groups SE
hispanicPopAgeGrpNat$SE<- sqrt(4/80*rowSums(apply(allRepWgtsHispanicPopAgeGrpNat[,3:82], MARGIN = 2, squareDiff, xMean=hispanicPopAgeGrpNat$PERWT)))

### Nativity totals of Hispanic Population SE
hispanicPopTotNat$SE <-  sqrt(4/80*rowSums(apply(allRepWgtsHispanicPopTotNat[,2:81], MARGIN = 2, squareDiff, xMean=hispanicPopTotNat[,2])))

### Print range of standard errors
range(hispanicPop$SE/hispanicPop$PERWT*100)
range(hispanicPopTotNat$SE/hispanicPopTotNat$PERWT*100)


#### confidence interval
(hispanicPopTotNat[,2] - (1.96*hispanicPopTotNat[,3]))/1e6 ###lower bound
(hispanicPopTotNat[,2] + (1.96*hispanicPopTotNat[,3]))/1e6 ### upper bound

###############################################################################
### Output total populations ###
print(paste("Total Hispanic population is", sum(hispanicPop$PERWT)/1e6, "million"))
# print(paste("15 Hispanic population is", sum(hispanicPop$PERWT)/1e6, "million"))
print(paste("Total nusbHispanic population is", sum(hispanicPop$PERWT[1:97])/1e6, "million"))
print(paste("15+ nusbHispanic population is", sum(hispanicPop$PERWT[16:97])/1e6, "million"))

print(paste("Total usbHispanic population is", sum(hispanicPop$PERWT[98:193])/1e6, "million"))
print(paste("15+ usbHispanic population is", sum(hispanicPop$PERWT[113:193])/1e6, "million"))


# ### Output percentage of population over 65 ###
# print(paste0(round(sum((filter(hispanicPop %>% filter(AGE > 64)))[,2])/sum(hispanicPop$PERWT)*100,2), "% of the Hispanic population is 65+"))
# # print(paste0(round(sum((filter(nusbHispanicPop %>% filter(AGE > 64)))[,2])/sum(nusbHispanicPop$PERWT)*100,2), "% of the nusbHispanic population is 65+"))
#
# ### Output population non-USB ###
# print(paste(sum((hispanicPop %>% filter(Nativity == "NUSB"))[,2])/1e6, "million of the Hispanic population is NUSB"))
# ### Output percentage of non-USB population that is 65p ###
# print(paste0(round(sum((hispanicPop %>% filter(Nativity == "NUSB" & AGE > 64))[,2])/
#                        sum((hispanicPop %>% filter(Nativity == "NUSB"))[,2])*100,2), "% of the NUSB Hispanic population is 65+"))
#

###############################################################################
### Remove the downloaded files
### If information is needed again, it will be stored in the IPUMS history
file.remove(Sys.glob(paste0(strsplit(filepathHispanic, split = "[.]")[[1]][1],"*")))
### Remove the original files to free memory
rm(HispanicPop0)

###############################################################################
### Final formatting of the data
### Rename PERWT as Population
asianPop <- asianPop %>% dplyr::rename(Population = PERWT)
hispanicPop <- hispanicPop %>% dplyr::rename(Population = PERWT)
asianPopAgeGrpNat <- asianPopAgeGrpNat %>% dplyr::rename(Population = PERWT)
hispanicPopAgeGrpNat <- hispanicPopAgeGrpNat %>% dplyr::rename(Population = PERWT)
asianPopTotNat <- asianPopTotNat %>% dplyr::rename(Population = PERWT)
hispanicPopTotNat <- hispanicPopTotNat %>% dplyr::rename(Population = PERWT)

###############################################################################
### Add both populations to a list
bothPopSingleYrAge <- list("Asian" = asianPop,
                           "Hispanic" = hispanicPop)

bothPopAgeGrps <- list("Asian" = asianPopAgeGrpNat,
                       "Hispanic" = hispanicPopAgeGrpNat)

bothPopNat <- list("Asian" = asianPopTotNat,
                   "Hispanic" = hispanicPopTotNat)

###############################################################################
### Save & output the list as a data file
saveRDS(bothPopAgeGrps, file = "data/ACS_Pop_Asian_Hisp_AgeGrp.rds", version = 2)
saveRDS(bothPopSingleYrAge, file = "data/ACS_Pop_Asian_Hisp_SingleYrAge.rds", version = 2)
saveRDS(bothPopNat, file = "data/ACS_Pop_Asian_Hisp_Nat.rds", version = 2)

