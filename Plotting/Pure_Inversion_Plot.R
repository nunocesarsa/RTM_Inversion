library(ggplot2)
library(ggthemes)
library(reshape2)
library(tidyverse)

gc()
setwd("C:/Paper/Tables/Final_Data")

agg.df <- read.csv2("PURERTM_Final_Aggregated.csv")
full.df <-read.csv2("PURERTM_Final_Full.csv")

names(agg.df)
names(full.df)



## Line plot ###########
plt.fig2 <- ggplot(agg.df, aes(x=NSamples, y=MAPE, colour=Model_ErrorType)) + 
  geom_line(linetype="dashed",size=1)+
  geom_point(size=2)+
  #  geom_line(linetype="dashed",size=.1)+
  #  geom_point(aes(shape=Model),size=2)+
  geom_errorbar(aes(ymin=MAPE-sd, ymax=MAPE+sd), width=.2)+
  ylim(0,80)+
  scale_colour_manual(values= c('#762a83', '#af8dc3',
                                '#d6604d','#f4a582',
                                'darkgreen','green',
                                #'#4393c3','#92c5de',
                                '#053061','#2166ac'))+
  
  labs(title="Pure RTM inversion",
       x ="Nr of samples", 
       y = "Mean absolute percentage error (%)")+
  facet_grid(~Variable,
             labeller = labeller(Variable = c("cab" = "Cab",
                                              "cm" = "Cm",
                                              "cw" = "Cw",
                                              "lai"= "LAI")))+
  #scale_shape_manual(values=c(15,15,16,16,17,17,18,18))+
  #guides(shape = guide_legend(override.aes = list(fill = "black")))
  
  theme_hc()+
  theme(text=element_text(size=24,  family="serif"),legend.title = element_blank(),
        panel.spacing = unit(1.5, "lines"))

print(plt.fig2)

#### Saving the plot ###3

pdf("Results_RTM_Inv_pure_2_thick.pdf",width=18,height=8,paper='special')

print(plt.fig2)

dev.off()


### Box plot ####

plt.fig2.boxplot <- ggplot(full.df,aes(x=Model, y=MAPE, fill=Model_ErrorType))+
  geom_boxplot()+
  ylim(0,100)+
  labs(#title="Pure RTM inversion",
       x ="Model", 
       y = "Mean absolute percentage error (%)")+
  #scale_colour_manual(values= c('#762a83', '#af8dc3',
  #                              '#d6604d','#f4a582',
  #                              'darkgreen','green',
  #                              #'#4393c3','#92c5de',
  #                              '#053061','#2166ac'))+
  #scale_colour_manual(values= c('#053061','#2166ac',
  #                              '#d6604d','#f4a582',
  #                              'darkgreen','green',
                                #'#4393c3','#92c5de',
  #                              '#762a83', '#af8dc3'))+
  facet_grid(~Variable,#ColNames,
             labeller = labeller(Variable = c("cab" ='Cab',
                                              "cm" = "Cm",
                                              "cw" = "Cw",
                                              "lai"= "LAI"))
             
  )+
  scale_colour_manual(values= c('#762a83', '#af8dc3',
                                '#d6604d','#f4a582',
                                'darkgreen','green',
                                #'#4393c3','#92c5de',
                                '#053061','#2166ac'),
                      aesthetics = "fill")+
  theme_hc()+
  theme(text=element_text(size=24,  family="serif"),legend.title = element_blank(),
        panel.spacing = unit(1.5, "lines"))#,


print(plt.fig2.boxplot)
