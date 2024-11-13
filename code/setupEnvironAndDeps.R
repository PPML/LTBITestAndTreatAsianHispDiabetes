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
