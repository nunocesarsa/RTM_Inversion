library(ggplot2)
library(ggthemes)
library(reshape2)

gc()
setwd("C:/Paper/Tables/Final_Data")

errbg.df = read.csv2("MTMTSTST_Temp_KFold_OutOfBag_csv2_Corrected_600epoch.csv")
errbg.df.2 = read.csv2("MTMTSTST_Temp_KFold_OutOfBag_csv2_Corrected_600epoch_1850.csv")
errbg.df.3 = read.csv2("MTMTSTST_Temp_KFold_OutOfBag_csv2_Corrected_600epoch_3350.csv")

train.df = read.csv2("MTMTSTST_Temp_KFold_TrainingError_csv2_Corrected_600epoch.csv")
train.df.2 = read.csv2("MTMTSTST_Temp_KFold_TrainingError_csv2_Corrected_600epoch_1850.csv")
train.df.3 = read.csv2("MTMTSTST_Temp_KFold_TrainingError_csv2_Corrected_600epoch_3350.csv")

errbg.df <- rbind(errbg.df,errbg.df.2,errbg.df.3)
train.df <- rbind(train.df,train.df.2,train.df.3)

#to maintain everything easily comparable, we will use MAPE to visualize the rror
mape.bag.df = errbg.df[,c(2,3,7,8,15)]
mape.trn.df = train.df[,c(2,3,7,8,15)]


agg.bag.df = aggregate(MAPE~.,data = mape.bag.df[,-4],
                       FUN=mean, na.rm=TRUE)
agg.bag.df.sd = aggregate(MAPE~.,data = mape.bag.df[,-4],
                          FUN=sd, na.rm=TRUE)

agg.trn.df = aggregate(MAPE~.,data = mape.trn.df[,-4],
                       FUN=mean, na.rm=TRUE)
agg.trn.df.sd = aggregate(MAPE~.,data = mape.trn.df[,-4],
                          FUN=sd, na.rm=TRUE)

agg.bag.df$sd <- agg.bag.df.sd$MAPE
agg.trn.df$sd <- agg.trn.df.sd$MAPE

#adding a tag
agg.bag.df$ErrorType = "Out-of-bag"
agg.trn.df$ErrorType = "Training"

#binding to a dataframe
full.df = rbind(agg.bag.df,agg.trn.df)

names(full.df)
head(full.df)
unique(full.df$ErrorType)


#naming 
full.df$ErrorTypeNames = NA
full.df$ErrorTypeNames[full.df$ErrorType=="Out-of-bag"] <- "Validation error"
full.df$ErrorTypeNames[full.df$ErrorType=="Training"] <- "Training error"

#creating a new variable to direct the plotting order
full.df$Model_ErrorType <- paste(full.df$Model,full.df$ErrorTypeNames)


unique(full.df$NSamples)

#creating a point type column

full.df$PT_type = NA
full.df$PT_type[full.df$Model=="ANN"] = 17
full.df$PT_type[full.df$Model=="GPR"] = 17
full.df$PT_type[full.df$Model=="MTN"] = 17
full.df$PT_type[full.df$Model=="RFR"] = 17


########## PRINTING - error from 0 to 80

list.files()
#write.csv2(full.df,"PURERTM_Final_Processed (forPlots).csv")

#saving aggregated data prepared for plots
write.csv2(full.df,"PURERTM_Final_Aggregated.csv")


#preparing a full dataset for boxplots
errbg.df$ErrorType = "Validation error"
train.df$ErrorType = "Training error"

full.boxplot = rbind(errbg.df,train.df)
full.boxplot$PT_type = 17

full.boxplot$Model_ErrorType = paste(full.boxplot$Model,full.boxplot$ErrorType)

write.csv2(full.boxplot,"PURERTM_Final_Full.csv")
