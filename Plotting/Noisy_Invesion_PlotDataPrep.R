
library(ggplot2)
library(ggthemes)
library(reshape2)


gc()
setwd("C:/Paper/Tables/Final_Data")

errbg.df = read.csv2("Optim_S2000_Temp_Noise_KFold_TrainingError_csv2.csv")
errbg.df.2 = read.csv2("Optim_S2000_Temp_Noise_KFold_TrainingError_csv2_10perc.csv")
errbg.df.3 = read.csv2("Optim_S2000_Temp_Noise_KFold_TrainingError_csv2_25perc.csv")
errbg.df.4 = read.csv2("Optim_S2000_Temp_Noise_KFold_TrainingError_csv2_50perc.csv")

train.df = read.csv2("Optim_S2000_Temp_Noise_KFold_OutOfBag_csv2.csv")
train.df.2 = read.csv2("Optim_S2000_Temp_Noise_KFold_OutOfBag_csv2_10perc.csv")
train.df.3 = read.csv2("Optim_S2000_Temp_Noise_KFold_OutOfBag_csv2_25perc.csv")
train.df.4 = read.csv2("Optim_S2000_Temp_Noise_KFold_OutOfBag_csv2_50perc.csv")

errbg.df <- rbind(errbg.df,errbg.df.2,errbg.df.3,errbg.df.4)
train.df <- rbind(train.df,train.df.2,train.df.3,train.df.4)

unique(train.df$NoiseLevel)

names(train.df)

#to maintain everything easily comparable, we will use MAPE to visualize the rror
mape.bag.df = errbg.df[,c(2,3,4,8,16)]
mape.trn.df = train.df[,c(2,3,4,8,16)]

names(mape.bag.df)
head(mape.bag.df)
#aggreating
agg.bag.df = aggregate(MAPE~.,data = mape.bag.df,
                       FUN=mean, na.rm=TRUE)
agg.bag.df.sd = aggregate(MAPE~.,data = mape.bag.df,
                          FUN=sd, na.rm=TRUE)

agg.trn.df = aggregate(MAPE~.,data = mape.trn.df,
                       FUN=mean, na.rm=TRUE)
agg.trn.df.sd = aggregate(MAPE~.,data = mape.trn.df,
                          FUN=sd, na.rm=TRUE)

#bringing everything together
agg.bag.df$sd <- agg.bag.df.sd$MAPE
agg.trn.df$sd <- agg.trn.df.sd$MAPE

#adding a tag
agg.bag.df$ErrorType = "Validation error"
agg.trn.df$ErrorType = "Training error"

#binding to a dataframe
full.df = rbind(agg.bag.df,agg.trn.df)

names(full.df)
head(full.df)
unique(full.df$NoiseLevel)

#creating a new variable to direct the plotting order
full.df$Model_ErrorType <- paste(full.df$Model,full.df$ErrorType)

full.df$Variable_upper <- NA
full.df$Variable_upper[full.df$Variable == "cab"] <- "Cab"
full.df$Variable_upper[full.df$Variable == "cm"] <- "Cm"
full.df$Variable_upper[full.df$Variable == "cw"] <- "Cw"
full.df$Variable_upper[full.df$Variable == "lai"] <- "LAI"
#### plotting time

#converting to percentage
full.df$NoiseLevel = full.df$NoiseLevel*100

#saving aggregated data prepared for plots
write.csv2(full.df,"NOISERTM_Final_Aggregated.csv")




#preparing a full dataset for boxplots
mape.bag.df$ErrorType = "Validation error"
mape.trn.df$ErrorType = "Training error"

full.boxplot = rbind(mape.bag.df,mape.trn.df)
full.boxplot$PT_type = 17

full.boxplot$Model_ErrorType = paste(full.boxplot$Model,full.boxplot$ErrorType)

write.csv2(full.boxplot,"NOISERTM_Final_Full.csv")

