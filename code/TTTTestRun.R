### Make sure to load all dependencies, helper functions, and color palette and 
### set working directory by first running the setupEnvironAndDeps.R file!


### Run the param_init code

usbAsianDiabTTT
sum(usbAsianDiabTTT$AgeDistUS)

tempParamUSBA <- national_param_init(PV = Par[1,],loc = "US",
                    Int1=0,Int2=0,Int3=0,Int4=0,Int5=0,
                                Scen1=0, Scen2=0, Scen3=0, Scen4=0, Scen5=0,
                                Scen6=0, prg_chng = def_prgchng(Par[1,]),
                                ttt_list = list(usbAsianDiabTTT), delay=0,
                                immig=99, noiseParams=list(1,1,1))
tempParamUSBA$ttt_month
results <- out[3,,]
pops <- results[75, 33:54]
sum(pops)
sampDist <- tempParamUSBA$ttt_sampling_dist
sum(pops*sampDist[1,])*1e6
range(sampDist)
dim(sampDist)
usbAsianDiabTTT$NRiskGrp

usbAsianDiabRes <- national_OutputsZint(samp_i = 1, ParMatrix = Par, loc = "US",
                                        prg_chng = diabPrgChng,
                                        care_cascade = diabCareCascade,
                                        ttt_list = list(usbAsianDiabTTT))

# View(tempParamUSBA$ttt_sampling_dist * 1e6)
tempParamNUSBA <- national_param_init(PV = Par[1,],loc = "US",
                                     Int1=0,Int2=0,Int3=0,Int4=0,Int5=0,
                                     Scen1=0, Scen2=0, Scen3=0, Scen4=0, Scen5=0,
                                     Scen6=0, prg_chng = def_prgchng(Par[1,]),
                                     ttt_list = list(nusbAsianDiabTTT), delay=0,
                                     immig=99, noiseParams=list(1,1,1))

identical(tempParamUSBA$ttt_sampling_dist,
          tempParamNUSBA$ttt_sampling_dist)

dim(tempParam$ttt_sampling_dist)






