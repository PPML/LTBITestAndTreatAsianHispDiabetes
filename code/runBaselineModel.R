###############################################################################
##### This code will run the MITUS model under baseline assumptions. 
##### The results of this model run will be used as the base
###############################################################################

### Make sure to load all dependencies, helper functions, and color palette and 
### set working directory by first running the setupEnvironAndDeps.R file!

###############################################################################
##### Load in MITUS model data
###############################################################################

model_load(loc = "US")

###############################################################################
##### Run model without any additional intervention scenario (noTTT)
###############################################################################

baselineRes <- national_OutputsZint(samp_i = 1, 
                                    ParMatrix = Par, 
                                    loc = "US")

###############################################################################
##### Save baseline results
###############################################################################

saveRDS(baselineRes, file = "data/baselineModelResults.rds", version = 2)