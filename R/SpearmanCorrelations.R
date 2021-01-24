
library(gtools)

my.df <- read.csv2("C:/Paper/Tables/Final_Data/Traits_Spectra_car.csv")
my.df <- my.df[,-1]


head(my.df)

out.df <- data.frame("Band"=0,"cab"=NA,"cw"=NA,"cm"=NA,"LAI"=NA,"car"=NA)
out.df.pval <- data.frame("Band"=0,"cab"=NA,"cw"=NA,"cm"=NA,"LAI"=NA,"car"=NA)

for (i in names(my.df)[1:9]){
  print(i)
  
  
  #cab_r = cor.test(my.df[,c(i)],my.df[,c('cab')],method="spearman")[[1]]
  #cw_r = cor.test(my.df[,c(i)],my.df[,c('cw')],method="spearman")[[1]]
  #cm_r = cor.test(my.df[,c(i)],my.df[,c('cm')],method="spearman")[[1]]
  #lai_r = cor.test(my.df[,c(i)],my.df[,c('LAI')],method="spearman")[[1]]
  #car_r = cor.test(my.df[,c(i)],my.df[,c('car')],method="spearman")[[1]]
  
  temp.df <- data.frame("Band"=i,
                        "cab"=cor.test(my.df[,c(i)],my.df[,c('cab')],method="spearman")$estimate[[1]],
                        "cw"=cor.test(my.df[,c(i)],my.df[,c('cw')],method="spearman")$estimate[[1]],
                        "cm"=cor.test(my.df[,c(i)],my.df[,c('cm')],method="spearman")$estimate[[1]],
                        "LAI"=cor.test(my.df[,c(i)],my.df[,c('lai')],method="spearman")$estimate[[1]],
                        "car"=cor.test(my.df[,c(i)],my.df[,c('car')],method="spearman")$estimate[[1]])
  
  temp.df.pval <- data.frame("Band"=i,
                        "cab"=stars.pval(cor.test(my.df[,c(i)],my.df[,c('cab')],method="spearman")$p.value),
                        "cw"=stars.pval(cor.test(my.df[,c(i)],my.df[,c('cw')],method="spearman")$p.value),
                        "cm"=stars.pval(cor.test(my.df[,c(i)],my.df[,c('cm')],method="spearman")$p.value),
                        "LAI"=stars.pval(cor.test(my.df[,c(i)],my.df[,c('lai')],method="spearman")$p.value),
                        "car"=stars.pval(cor.test(my.df[,c(i)],my.df[,c('car')],method="spearman")$p.value))
  
  
  
  out.df <- rbind(out.df,temp.df)
  out.df.pval <- rbind(out.df.pval,temp.df.pval)
}

#removing uncessary first column
out.df <- out.df[-1,]
out.df.pval <- out.df.pval[-1,]

write.csv2(out.df,"C:/Paper/Tables/Final_Data/SpearmanCorrelation_Final.csv")
write.csv2(out.df.pval,"C:/Paper/Tables/Final_Data/SpearmanCorrelation_Final_Pstars.csv")
