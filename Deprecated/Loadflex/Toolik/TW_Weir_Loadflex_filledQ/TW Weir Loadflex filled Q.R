library(loadflex)

#create interpolation data
head(TW_Weir_filledQ)
TW_Weir_Q = data.frame(Date=TW_Weir_filledQ$date,
                       Q=TW_Weir_filledQ$flow) #will be prediction data
TW_Weir_intdat = merge(TW_Weir_Q, TW_Weir_Si_WRTDS, by=c("Date"))

#site metadata
TW_Weir_meta = metadata(constituent="Si", flow="Q", 
                     dates="Date", conc.units="mg L^-1", flow.units="cms", load.units="kg", 
                     load.rate.units="kg d^-1", site.name="TW_Weir")

#run regression model
TW_Weir_li = loadInterp(interp.format="conc", interp.function=linearInterpolation,
                     data=TW_Weir_intdat, metadata=TW_Weir_meta)
#evaluate model fit
regmodelfit=getFittedModel(TW_Weir_li)

#get residuals
compres = data.frame("Date"=getResiduals(TW_Weir_li,"flux")[,1],
                     "Residuals"=getResiduals(TW_Weir_li,"flux")[,2])

#point predictions and daily load estimates
TW_Weir_preds = predictSolute(TW_Weir_li,"flux",TW_Weir_Q,se.pred=T,date=T)
names(TW_Weir_preds)[names(TW_Weir_preds)=="fit"] = "Si_load_kg.day"
TW_Weir_preds$site = "TW_Weir"

#save Loadflex output
write.csv(TW_Weir_preds, file="TW_Weir_dailyLoadflex_Si_filledQ.csv")
