###############################################################################
##### Load in libraries
###############################################################################
library(MITUS)
library(survey)
library(foreign)
library(plyr)
library(MASS)
library(stringr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(reshape2)
library(ipumsr)
library(purrr)

###############################################################################
##### Define working directory
###############################################################################
setwd("~/TTTAsianHispDiabetes/LTBITestAndTreatAsianHispDiabetes/")

###############################################################################
##### Define palette
###############################################################################
safe_colorblind_palette <- c("#88CCEE", "#CC6677", "#DDCC77",
                             "#117733", "#332288", "#AA4499",
                             "#44AA99", "#999933", "#882255",
                             "#661100", "#6699CC", "#888888")

###############################################################################
##### Load in helper functions
###############################################################################
invlgt <- function(x) 1/(1+exp(-x))

mdlohi <- function(x) as.numeric(quantile(x,c(20,1,39)/40,na.rm=T))

pretty <- function(x0,r) { x <- gsub(" ","",format(round(x0,r),nsmall=r,big.mark=","));
paste0(x[1],"\n(",x[2],", ",x[3],")") }

gammapar2 <- function(tgt) {
  tgt <- as.numeric(tgt)
  shape= tgt[1]^2/tgt[2]^2
  rate = tgt[1]/tgt[2]^2
  return(c(shape,rate))  }

betapar <- function(tgt) {
  tgt <- as.numeric(tgt)
  mn <- tgt[1]; cir <- (tgt[3]-tgt[2])
  xopt <- function(xx,mn=mn,cir=cir) {
    cir2 <- qbeta(c(1,39)/40,xx*(mn/(1-mn)),xx);
    cir2 <- cir2[2]-cir2[1]
    sum((cir2-cir)^2) }
  zz <- optimize(xopt,c(0.2,100000),mn=mn,cir=cir)$minimum
  bp <-  c(zz*(mn/(1-mn)),zz)
  if(sum(abs(bp-1))<0.2) { c(1,1) } else { bp }  }

###############################################################################
##### Read in data
###############################################################################

NHANES1112Data <- readRDS(file = "data/2011_12_NHANES_survey_obj_newA1c.rds")
names(NHANES1112Data)
unique(NHANES1112Data$variables$`Age group`)
###############################################################################
##### Define covariates
##### These were primarily defined in the survey object, but
##### we need to ensure that they are of the appropriate type
###############################################################################

##### AGE
if(class(NHANES1112Data$variables$`Age group`) != "factor") print("Age group is not a factor.")
levels(NHANES1112Data$variables$`Age group`)

##### NATIVITY
if(class(NHANES1112Data$variables$`Nativity`) != "factor"){
  print("Nativity was not a factor. Attempting to coerce.")
  NHANES1112Data$variables$`Nativity` <- as.factor(NHANES1112Data$variables$`Nativity`)
  if(class(NHANES1112Data$variables$`Nativity`) == "factor") print("Success!")
}

##### RACE/ETHNICITY
#~~ Set other races from NA to 'Other'
NHANES1112Data$variables$`Race-ethnicity`[is.na(NHANES1112Data$variables$`Race-ethnicity`)] <- "Other"
if(class(NHANES1112Data$variables$`Race-ethnicity`) != "factor"){
  print("Race-ethnicity was not a factor. Attempting to coerce.")
  NHANES1112Data$variables$`Race-ethnicity` <- as.factor(NHANES1112Data$variables$`Race-ethnicity`)
  if(class(NHANES1112Data$variables$`Race-ethnicity`) == "factor") print("Success!")
}
##### DIABETES STATUS
#~~ Binary variable for diab vs. non-diab
NHANES1112Data$variables$`Diabetes2` <- ifelse(NHANES1112Data$variables$`Diabetes status`=="Diabetes","Yes","No")
if(class(NHANES1112Data$variables$`Diabetes2`) != "factor"){
  print("Diabetes status was not a factor. Attempting to coerce.")
  NHANES1112Data$variables$`Diabetes2` <- as.factor(NHANES1112Data$variables$`Diabetes2`)
  if(class(NHANES1112Data$variables$`Diabetes2`) == "factor") print("Success!")
}

##### TB STATUS
if(class(NHANES1112Data$variables$`TB status`) != "factor"){
  print("TB status was not a factor. Attempting to coerce.")
  NHANES1112Data$variables$`TB status` <- as.factor(NHANES1112Data$variables$`TB status`)
  if(class(NHANES1112Data$variables$`TB status`) == "factor") print("Success!")
}

##### IGRA POSITIVE
if(class(NHANES1112Data$variables$`IGRA positive`) != "factor"){
  print("IGRA positivity was not a factor. Attempting to coerce.")
  NHANES1112Data$variables$`IGRA positive` <- as.factor(NHANES1112Data$variables$`IGRA positive`)
  if(class(NHANES1112Data$variables$`IGRA positive`) == "factor") print("Success!")
}

fitIGRA <-svyglm(`IGRA positive`~ `Age group` + Nativity + `Race-ethnicity` +
                   # Nativity:`Diabetes status` + #`Age group`:`Diabetes status` +
                   `Race-ethnicity`:`Diabetes2`,
                 family = quasibinomial,
                 design = NHANES1112Data,
                 na.action = na.omit)

summary(fitIGRA)
##### Predict using fitted model
##### Create additional data for the prediction
new_dat <- as.data.frame(expand.grid(levels(NHANES1112Data$variables$`Age group`), #[c(-1,-10,-11)],
                                     levels(NHANES1112Data$variables$`Nativity`),
                                     levels(NHANES1112Data$variables$`Race-ethnicity`),
                                     levels(NHANES1112Data$variables$`Diabetes2`)))

names(new_dat) <- c("Age group", "Nativity", "Race-ethnicity", "Diabetes2")

##### Predict for new data
pred_link <- predict(fitIGRA, vcov=T, se.fit=T, type="link", newdata=new_dat)
vcv_link  <- vcov(pred_link)
est_link  <- coef(pred_link)

##### Draw random samples for igra values
nSim <- 1e5 # no samples
set.seed(1)
sims0 <- mvrnorm(nSim, mu = est_link, Sigma = vcv_link)
igra_prev_sims  <- t(invlgt(sims0))

#~~ create intervals from simulations , to create prirs for Baseyain fitting
igra_prev_par_diab <- data.frame(new_dat[,1:4], mean=NA,ci_lo=NA,ci_hi=NA)

for(i in 1:nrow(igra_prev_par_diab)){
  igra_prev_par_diab[i,5:7] <- c(mean(igra_prev_sims[i,]),quantile(igra_prev_sims[i,],c(1,39)/40))
}

save(igra_prev_par_diab, file="igra_prev_par_diab_Aug_29_2024.rData")
#~~ from here operations done in the other script, script below this not used. 

###############################################################################
### Calculate LTBI prevalence from IGRA positivity
###############################################################################
# igra = ltbi*sens + (1-ltbi) * (1-spec)
#      = ltbi*sens + 1-ltbi-spec+ltbi*spec
#      = 1 + ltbi*(sens+spec-1) - spec
# ltbi = (igra-1+spec)/(sens+spec-1)

# Stout : sens                   spec
# FB      78.9 (69.6 to 90.2)    98.5 (96.1 to 99.8)
# USB     78.0 (65.0 to 91.0)    97.9 (96.0 to 99.4)

##### Simulate sens/spec of IGRA by US/NUS status
sens_par_nusb <- betapar( c(78.9, 69.6, 90.2)/100 )
sens_nusb <- rbeta(shape1=sens_par_nusb[1],shape2=sens_par_nusb[2], n=nSim)

sens_par_usb <- betapar( c(78.0, 65.0, 91.0)/100 )
sens_usb <- rbeta(shape1=sens_par_usb[1],shape2=sens_par_usb[2], n=nSim)

spec_par_nusb <- betapar( c(98.5, 96.1, 99.8)/100 )
spec_nusb <- rbeta(shape1=spec_par_nusb[1],shape2=spec_par_nusb[2], n=nSim)

spec_par_usb <- betapar( c(97.9, 96.0, 99.4)/100 )
spec_usb <- rbeta(shape1=spec_par_usb[1],shape2=spec_par_usb[2], n=nSim)

##### BY RACE-ETHNICITY #######
##### LTBI prevalence Asian USB
# indexAsian <- which(new_dat$`Race-ethnicity`=="Asian")
indexAsianUSB <- c(8:14, 36:42)
ltbiPrevSimAsianUSB <- (igra_prev_sims[indexAsianUSB,] - 1 + spec_usb) / (sens_usb + spec_usb - 1)
tempPrevAsianUSB <- apply(X = ltbiPrevSimAsianUSB, MARGIN = 1, FUN = quantile, probs=c(.025, .50, 0.975))
ltbiPrevAsianUSB <- cbind(new_dat[indexAsianUSB,], t(tempPrevAsianUSB))

##### LTBI prevalence Asian NUSB
# indexAsian <- which(new_dat$`Race-ethnicity`=="Asian")
indexAsianNUSB <- c(1:7, 29:35)
ltbiPrevSimAsianNUSB <- (igra_prev_sims[indexAsianNUSB,] - 1 + spec_nusb) / (sens_nusb + spec_nusb - 1)
tempPrevAsianNUSB <- apply(X = ltbiPrevSimAsianNUSB, MARGIN = 1, FUN = quantile, probs=c(.025, .50, 0.975))
ltbiPrevAsianNUSB <- cbind(new_dat[indexAsianNUSB,], t(tempPrevAsianNUSB))


##### LTBI prevalence Hispanic USB
# indexHispanic <- which(new_dat$`Race-ethnicity`=="Hispanic")
indexHispanicUSB <- c(22:28,50:56)
ltbiPrevSimHispanicUSB <- (igra_prev_sims[indexHispanicUSB,] - 1 + spec_usb) / (sens_usb + spec_usb - 1)
tempPrevHispanicUSB <- apply(X = ltbiPrevSimHispanicUSB, MARGIN = 1, FUN = quantile, probs=c(.025, .50, 0.975))
ltbiPrevHispanicUSB <- cbind(new_dat[indexHispanicUSB,], t(tempPrevHispanicUSB))

##### LTBI prevalence Hispanic NUSB
# indexHispanic <- which(new_dat$`Race-ethnicity`=="Hispanic")
indexHispanicNUSB <- c(15:21, 43:49)
ltbiPrevSimHispanicNUSB <- (igra_prev_sims[indexHispanicNUSB,] - 1 + spec_nusb) / (sens_nusb + spec_nusb - 1)
tempPrevHispanicNUSB <- apply(X = ltbiPrevSimHispanicNUSB, MARGIN = 1, FUN = quantile, probs=c(.025, .50, 0.975))
ltbiPrevHispanicNUSB <- cbind(new_dat[indexHispanicNUSB,], t(tempPrevHispanicNUSB))

##### Create a single dataframe
ltbiPrev <- rbind(ltbiPrevAsianUSB, ltbiPrevAsianNUSB, ltbiPrevHispanicUSB, ltbiPrevHispanicNUSB)
