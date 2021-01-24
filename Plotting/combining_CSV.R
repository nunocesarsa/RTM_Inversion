
library(ggplot2)
library(ggthemes)
library(reshape2)

gc()
setwd("C:/Paper/Tables/Final_Data")

#loading data - pure
errbg.df = read.csv2("MTMTSTST_Temp_KFold_OutOfBag_csv2_Corrected_600epoch.csv")
errbg.df.2 = read.csv2("MTMTSTST_Temp_KFold_OutOfBag_csv2_Corrected_600epoch_1850.csv")
errbg.df.3 = read.csv2("MTMTSTST_Temp_KFold_OutOfBag_csv2_Corrected_600epoch_3350.csv")

train.df = read.csv2("MTMTSTST_Temp_KFold_TrainingError_csv2_Corrected_600epoch.csv")
train.df.2 = read.csv2("MTMTSTST_Temp_KFold_TrainingError_csv2_Corrected_600epoch_1850.csv")
train.df.3 = read.csv2("MTMTSTST_Temp_KFold_TrainingError_csv2_Corrected_600epoch_3350.csv")

errbg.df <- rbind(errbg.df,errbg.df.2,errbg.df.3)
train.df <- rbind(train.df,train.df.2,train.df.3)

all.df <- rbind(errbg.df,train.df)

write.csv2(all.df,"Inversion_PureRTM.csv")


#loading data - noise
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

all.df <- rbind(errbg.df,train.df)

write.csv2(all.df,"Inversion_NoisyRTM.csv")


