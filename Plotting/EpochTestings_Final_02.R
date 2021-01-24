library(ggplot2)
library(ggthemes)
library(reshape2)
library(RColorBrewer)


setwd("C:/Paper/Tables/Final_Data")
gc()


#my.df <- read.csv2("C:/Users/Nuno/Desktop/ChangeNet_Temp_EpochTesting_OptimizedOnly.csv")
my.df <- read.csv2("MTMTSTST_01_Final_EpochTesting_OptimizedOnly.csv")

#fixing the table
my.df.2 = my.df[,c(2,3,4,5,13)]

my.df.2$Model = substr(my.df.2$Model,1,3)

my.df.2$Valid_Model = paste(my.df.2$Model,my.df.2$ValidType,sep=" ")
my.df.2$Epochs_mdl_valid = paste(my.df.2$Epochs,my.df.2$Model,my.df.2$ValidType,sep=" ")


##boxplots works but are not pretty
ggplot(my.df.2, aes(x=Epochs, y=MAPE, fill=Valid_Model,group=interaction(Epochs,Valid_Model))) + 
  #geom_line(linetype="dashed")+
  #geom_point(shape=full.df$ptshape,size=1.8)+
  #geom_point()+
  #geom_jitter()+
  geom_boxplot(position = "dodge2")+
  #ylim(0,25)+
  #xlim(0,300)+
  scale_colour_manual(values= c('chocolate1', 'chocolate4',
                                'green1','darkgreen'))+
  
  labs(title="Neural networks - MAPE error in function of training iterations",
       x ="Epochs", 
       y ="Mean absolute percentage error (%)")+
  facet_wrap(~Variable,scales="free_y",ncol=2)+
  #theme(strip.text = element_text(hjust = 0))+
  theme_hc()+
  theme(text=element_text(size=18,  family="serif"),legend.title = element_blank(),
        panel.spacing = unit(1.5, "lines"))


#aggregating
my.df.2 = my.df[,c(2,3,4,5,13)]
my.df.2$Model = substr(my.df.2$Model,1,3)

#there is a big outlier on the 50 epochs training that breaks down the error bars
my.df.2 = my.df.2[-c(67,68),]

agg.df = aggregate(MAPE~.,data =my.df.2,
                   FUN=mean, na.rm=TRUE)
agg.df.sd = aggregate(MAPE~.,data =my.df.2,
                      FUN=sd, na.rm=TRUE)
agg.df$sd = agg.df.sd$MAPE
agg.df$Valid_Model = paste(agg.df$Model,agg.df$ValidType,sep=" ")

pdf("ANN_By_Epochs.pdf",width=18,height=8,paper='special')

plt1 <- ggplot(agg.df, aes(x=Epochs, y=MAPE, colour=Valid_Model)) + 
  geom_line(linetype="dashed",size=.1)+
  #geom_point(shape=full.df$ptshape,size=1.8)+
  geom_point(size=1.5)+
  geom_errorbar(aes(ymin=MAPE-sd, ymax=MAPE+sd), width=.2)+
  #ylim(0,25)+
  scale_colour_manual(values= c('chocolate1', 'chocolate4',
                                'green1','darkgreen'))+
  
  labs(#title="Neural networks - MAPE error in function of training iterations",
       x ="Epochs", 
       y ="Mean absolute percentage error (%)")+
  facet_wrap(~Variable,scales="free_y",ncol=2,
             labeller = labeller(Variable = c("cab" = "Cab",
                                              "cm" = "Cm",
                                              "cw" = "Cw",
                                              "lai"= "LAI")))+
  #theme(strip.text = element_text(hjust = 0))+
  theme_hc()+
  theme(text=element_text(size=18,  family="serif"),legend.title = element_blank(),
        panel.spacing = unit(1.5, "lines"))

print(plt1)
dev.off()


############### old code
names(my.df)

my.df.2 = my.df[,c(2,3,4,5,13)]
my.df.2$Model = substr(my.df.2$Model,1,3)


agg.df = aggregate(MAPE~.,data =my.df.2,
                   FUN=mean, na.rm=TRUE)

agg.df$Valid_Model = paste(agg.df$Model,agg.df$ValidType,sep=" ")



plt.fig <- ggplot(agg.df, aes(x=Epochs, y=MAPE, colour=Valid_Model)) + 
  geom_line(linetype="dashed")+
  #geom_point(shape=full.df$ptshape,size=1.8)+
  geom_point()+
  geom_errorbar(aes(ymin=MAPE-sd, ymax=MAPE+sd), width=.2)+
  #ylim(0,25)+
  scale_colour_manual(values= c('chocolate1', 'chocolate4',
                                'green1','darkgreen'))+
  
  labs(title="Neural networks - MAPE error in function of training iterations",
       x ="Epochs", 
       y ="Mean absolute percentage error (%)")+
  facet_wrap(~Variable,scales="free_y",ncol=2)+
  #theme(strip.text = element_text(hjust = 0))+
  theme_hc()+
  theme(text=element_text(size=18,  family="serif"),legend.title = element_blank(),
        panel.spacing = unit(1.5, "lines"))

print(plt.fig)




#as.character(unique(agg.df$Model))

#agg.df.2 = agg.df[agg.df$Model==c("ANN_optimized"),]
#agg.df.2 = rbind(agg.df.2,agg.df[agg.df$Model==c("MTN_optimized"),])

agg.df$Valid_Model = paste(agg.df$Model,agg.df$ValidType,sep="_")

#png("EpochComparision_0to25.png",
#    width = 800, height = 500, units = "px",pointsize=12)

plt.fig <- ggplot(agg.df, aes(x=Epochs, y=MAPE, colour=Valid_Model)) + 
  geom_line(linetype="dashed")+
  #geom_point(shape=full.df$ptshape,size=1.8)+
  geom_point()+
  ylim(0,25)+
  scale_colour_manual(values= c('chocolate1', 'chocolate4',
                                'green1','darkgreen'))+
  
  labs(title="Neural networks - MAPE error in function of training iterations",
       x ="Epochs", 
       y ="Mean absolute percentage error (%)")+
  facet_wrap(~Variable,scales="free_y",ncol=2)+
  #theme(strip.text = element_text(hjust = 0))+
  theme_hc()+
  theme(text=element_text(size=18,  family="serif"),legend.title = element_blank(),
        panel.spacing = unit(1.5, "lines"))

print(plt.fig)

#dev.off()

