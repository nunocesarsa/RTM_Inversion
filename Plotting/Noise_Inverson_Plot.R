library(ggplot2)
library(ggthemes)
library(reshape2)
library(tidyverse)

gc()
setwd("C:/Paper/Tables/Final_Data")

agg.df <- read.csv2("NOISERTM_Final_Aggregated.csv")
full.df <-read.csv2("NOISERTM_Final_Full.csv")

names(agg.df)
names(full.df)



#### Line plot ####

plt.fig <- ggplot(agg.df, aes(x=NoiseLevel, y=MAPE, colour=Model_ErrorType)) + 
  geom_line(linetype="dashed",size=1)+
  geom_point(size=2)+
  geom_errorbar(aes(ymin=MAPE-sd, ymax=MAPE+sd), width=.2)+
  #geom_point()+
  ylim(0,50)+
  xlim(0,10)+
  scale_colour_manual(values= c('#762a83', '#af8dc3',
                                '#d6604d','#f4a582',
                                'darkgreen','green',
                                #'#4393c3','#92c5de',
                                '#053061','#2166ac'))+
  
  labs(#title="Noise RTM inversion",
    x ="Noise level (%)", 
    y = "Mean absolute percentage error (%)")+
  facet_grid(NoiseType~Variable_upper,
             #ncol=4,
             #strip.position = c("top","right")
  )+
  #theme(strip.text = element_text(hjust = 0))+
  theme_hc()+
  theme(text=element_text(size=24,  family="serif"),legend.title = element_blank(),
        panel.spacing = unit(1.5, "lines"))

print(plt.fig)

####

pdf("Results_RTM_Inv_noise_2_zoom_thick.pdf",width=18,height=8,paper='special')

print(plt.fig)

dev.off()




### boxplot plot ####
head(full.df)
#as.factor(NoiseLevel*100)
plt.fig <- ggplot(full.df, aes(x=Model, y=MAPE, fill=Model_ErrorType)) + 
  geom_boxplot()+
  ylim(0,75)+
  scale_colour_manual(values= c('#762a83', '#af8dc3',
                                '#d6604d','#f4a582',
                                'darkgreen','green',
                                #'#4393c3','#92c5de',
                                '#053061','#2166ac'),
                      aesthetics = "fill")+
  
  labs(#title="Noise RTM inversion",
    x ="Model", 
    y = "Mean absolute percentage error (%)")+
  facet_grid(NoiseType~Variable,
             labeller = labeller(Variable = c("cab" ='Cab',
                                              "cm" = "Cm",
                                              "cw" = "Cw",
                                              "lai"= "LAI"))
             
  )+
  #theme(strip.text = element_text(hjust = 0))+
  theme_hc()+
  theme(text=element_text(size=24,  family="serif"),legend.title = element_blank(),
        panel.spacing = unit(1.5, "lines"))

print(plt.fig)





