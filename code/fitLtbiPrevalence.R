################################################################################

### SET-UP
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

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

### SENS SPEC
igra_sens_par_nus <- betapar(c(78.9, 69.6, 90.2)/100 )
igra_sens_par_usb <- betapar(c(78.0, 65.0, 91.0)/100 )

igra_spec_par_nus <- betapar(c(98.5, 96.1, 99.8)/100 )
igra_spec_par_usb <- betapar(c(97.9, 96.0, 99.4)/100 )

################################################################################
### Create beta pars for igra results
load("igra_prev_par_diab_Aug_29_2024.rData")
# igra_prev_par_diab

n.sim <- 1e4

igra_prev_par_diab$b <- igra_prev_par_diab$a <- NA

for(i in 1:nrow(igra_prev_par_diab)){ # i=1
  igra_prev_par_diab[i,8:9] <- betapar(igra_prev_par_diab[i,5:7])
}

################################################################################
### Initialize stan inputs
source("fit_ltbi_prev_V4.stan")

################################################################################
ltbi_prev_sims_diab <- matrix(NA,nrow(igra_prev_par_diab),n.sim)

mn_sens <- mn_spec  <- NA
N_divergent  <- NA

igra_prev_par_diab$Nativity2 <- ifelse(igra_prev_par_diab$Nativity=="NUSB",1,2)

for(i in 1:nrow(igra_prev_par_diab)){ # i=1
  datList <- list(
    N        = 1,
    pri_test = igra_prev_par_diab[i,c("a","b")],
    pri_sens = list(igra_sens_par_nus,igra_sens_par_usb)[[igra_prev_par_diab$Nativity2[i]]],
    pri_spec = list(igra_spec_par_nus,igra_spec_par_usb)[[igra_prev_par_diab$Nativity2[i]]] )

    fit <- rstan::stan(model_code=stan_code,
                      control=list(adapt_delta=0.85,max_treedepth=10),
                      seed=1234,chains=4,iter=10000+n.sim/4,warmup=10000,
                      data = datList )

  samps <- rstan::extract(fit)

  ltbi_prev_sims_diab[i,] <- samps$ltbi[order(samps$test)]
  mn_sens[i] <- mean(samps$sens)
  mn_spec[i] <- mean(samps$spec)
  N_divergent[i] <-  sum(sapply(get_sampler_params(fit,inc_warmup=F),function(x) sum(x[,"divergent__"])))

  print(paste0("#############~~~~~~~~~~~~~ ",i," ~~~~~~~~~~~~~~#############"))
}

save(ltbi_prev_sims_diab,file="samps_ltbi_igra_fit_diab_Aug-29-2024.rData",version=2)

ltbi_prev_par_diab <- data.frame(igra_prev_par_diab[,1:4], mean=NA,ci_lo=NA,ci_hi=NA)

for(i in 1:nrow(igra_prev_par_diab)){
  ltbi_prev_par_diab[i,5:7] <- c(mean(ltbi_prev_sims_diab[i,]),quantile(ltbi_prev_sims_diab[i,],c(1,39)/40))
}

save(ltbi_prev_par_diab, file="ltbi_prev_par_diab_Aug_29_2024.rData")


################################################################################
dim(ltbi_prev_sims_diab)
#############################################


