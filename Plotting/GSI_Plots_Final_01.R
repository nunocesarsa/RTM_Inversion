#library for PROSAIL and also Spectral Angle Mapper
library(hsdar)

#Latin hypercube comes from here
library(FME)

#for more options on generating random samples
library(MCMCglmm)

#the single target classifier
#library(randomForest)

#machine learning librarys
#library(randomForestSRC)
#library(ANN2)

#for scaling and unscaling variables
library(DMwR)

#GIS/RS
library(maptools)
library(rgeos)
library(rgdal)
library(raster)
library(sp)

#sensitivity analysis
library(sensitivity)

#plotting packages
library(ggplot2)
library(ggthemes)

#data frame operations5
library(reshape2)


#importing fonts
library(extrafont)
loadfonts(device="win")
#font_import()
y#latex font
par(family = "LM Roman 10")



##############################################################
############ by sample number ################################
##############################################################


#loading the csv
all.df = read.csv2("C:/Paper/R_Scripts/All_sensitivity.csv")
m_mSI = all.df[all.df$type=="mSI",]
m_iSI = all.df[all.df$type=="iSI",]
m_tSI = all.df[all.df$type=="tSI",]


head(all.df)

all.df$Order <- NA
all.df$Order[all.df$type=="mSI"] <- "First-order"
all.df$Order[all.df$type=="iSI"] <- "Interactions"
all.df$Order[all.df$type=="tSI"] <- "Total-order"

gc()

head(all.df)


#png("C:/Paper/R_Scripts/GSI_By_Samples_2.png",
#    width = 1000, height = 300, units = "px",pointsize=1)
pdf("C:/Paper/Tables/Final_Data/Results_GSI_BySample.pdf",width=25,height=10,paper='special')

plot.fig <- ggplot(all.df[all.df$variable!="GSI" &
                            all.df$type!="iSI" &
                          all.df$SampleNr < 6000,],aes(SampleNr,value,color=Trait))+
  geom_point(pch=19,size=2)+geom_line(linetype="dashed",size=.1)+
  
  #ggtitle("First order GSI")+
  facet_wrap(Order~variable,ncol=9)+
  xlab("Samples")+
  ylab("Sensitivity index")+
  theme_hc()+
  theme(text=element_text(size=16,  family="serif"),legend.title = element_blank())
  

print(plot.fig)

dev.off()


### Barplot of the interaction and band variance
df.gsi <- read.csv2("C:/Paper/R_Scripts/All_sensitivity_2000_samples.csv")

#the logic for the normalized graphic is on excel sheet normalized square
head(df.gsi)
unique(df.gsi$variable)


#making color fills
#http://sape.inf.usi.ch/quick-reference/ggplot2/colour
df.gsi$ColorFill = NA
df.gsi$ColorFill[df.gsi$variable=="B02"] <- "blue2"
df.gsi$ColorFill[df.gsi$variable=="B03"] <- "green4"
df.gsi$ColorFill[df.gsi$variable=="B04"] <- "red"
df.gsi$ColorFill[df.gsi$variable=="B05"] <- "orange"
df.gsi$ColorFill[df.gsi$variable=="B06"] <- "orange3"
df.gsi$ColorFill[df.gsi$variable=="B07"] <- "orange4"
df.gsi$ColorFill[df.gsi$variable=="B8A"] <- "indianred"
df.gsi$ColorFill[df.gsi$variable=="B11"] <- "gray"
df.gsi$ColorFill[df.gsi$variable=="B12"] <- "black"
df.gsi$ColorFill[df.gsi$variable=="GSI"] <- "black"

#we have to cheat the plotting device due to the order of the bands
df.gsi$band_order <- as.character(df.gsi$variable)
#df.gsi$band_order[81:85] <- "B08"
df.gsi$band_order[df.gsi$band_order=="B8A"] <- "B08"




temp.df <- df.gsi[df.gsi$variable!="GSI",]# &
                    #df.gsi$type != 'iSI',]

temp.df$type_2 = NA
temp.df$type_2[temp.df$type=="iSI"] = "BiSI"
temp.df$type_2[temp.df$type=="mSI"] = "AmSI"
temp.df$type_2[temp.df$type=="tSI"] = "CtSI"

#png("C:/Paper/R_Scripts/GSI_by_Trait_3si.png",
#    width = 600, height = 400, units = "px",pointsize=12)

pdf("C:/Paper/Tables/Final_Data/Results_GSI_By_Trait.pdf",width=20,height=8,paper='special')

#in one go
plt.fig <- ggplot(temp.df,aes(Trait,value, fill=band_order))+
  geom_bar(stat="identity",position="dodge")+
  scale_fill_manual(values=unique(temp.df$ColorFill),
                                  labels=c("B02","B03","B04",
                                           "B05","B06","B07",
                                           "B8A","B11","B12"))+
  ylab("Sensitivity index")+
  xlab("Biophysical traits")+
  #ggtitle("First order GSI")+
  coord_flip()+
  facet_wrap(~type_2,ncol=3,labeller = labeller(type_2 = c("BiSI" = "Interactions",
                                                       "AmSI" = "First-order",
                                                       "CtSI" = "Total-order")))+
  theme_hc()+
  theme(text=element_text(size=25,  family="serif"),legend.title = element_blank(),
        panel.spacing = unit(1.5, "lines"))

print(plt.fig)

dev.off()



###  facet_wrap(~type_2,ncol=3,labeller = labeller(type = c("iSI" = "Interactions",
###"mSI" = "First-order",
###"tSI" = "Total-order")))+




####### to rerun the 2k example, run here #####################
custom.prosail.df = function(x){
  
  Cab = x[,1]
  Car = x[,2]
  Cw  = x[,3]
  Cm  = x[,4]
  LAI = x[,5]
  train.spclib = PROSAIL(Cab=Cab,Car=Car,Cw=Cw,Cm=Cm,LAI = LAI,
                         tts = sun_zenith,
                         tto = obs_zenith,
                         psi = rel_azimut)
  
  #to make this hyperspectral, just comment the next line
  train.spclib <- spectralResampling(train.spclib,"Sentinel2",response_function = T)[,c(2,3,4,5,6,7,9,12,13)]
  #if output matrix
  #mat.out = spectra(train.spclib)
  #if output df
  df.out = as.data.frame(spectra(train.spclib))
  
  #and of course comment here also if you want to do hyperspectral 
  names(df.out)<- c("B02","B03","B04",
                    "B05","B06","B07",
                    "B8A","B11","B12")
  
  return(df.out)
}
args.efast =list( factors=c("Cab","Car","Cw","Cm","LAI"), n=2000, q = "qunif",
                  q.arg = list(list(min=5, max=120),
                               list(min=5, max=60),
                               list(min = 0.01, max = 0.05),
                               list(min = 0.01, max = 0.05),
                               list(min = 0.5,  max = 10)))

sens.99fast = multisensi(design = fast99, model = custom.prosail.df,
                         center = FALSE, reduction = NULL, analysis = analysis.sensitivity,
                         design.args=args.efast,
                         analysis.args=list(keep.outputs=FALSE))

plot(sens.99fast, color = terrain.colors)
plot(sens.99fast,normalized = T, color = terrain.colors, gsi.plot = FALSE,cumul=T)


m_SI  = sens.99fast$SI
m_mSI = sens.99fast$mSI
m_tSI = sens.99fast$tSI
m_iSI = sens.99fast$iSI


t_mSI$Trait = rownames(t_mSI)
t_tSI$Trait = rownames(t_tSI)
t_iSI$Trait = rownames(t_iSI)



m_mSI=melt(t_mSI,ID="Trait")
m_tSI=melt(t_tSI,ID="Trait")
m_iSI=melt(t_iSI,ID="Trait")

m_mSI$type = "mSI"
m_tSI$type = "tSI"
m_iSI$type = "iSI"

#and finally the number of samples
m_mSI$SampleNr = 2000
m_tSI$SampleNr = 2000
m_iSI$SampleNr = 2000

all.df = rbind(m_mSI,m_iSI,m_tSI)
write.csv2(all.df,"C:/Paper/R_Scripts/All_sensitivity_2000_samplesexplore.csv")


all.df = rbind(m_mSI,m_iSI,m_tSI)














warnings()

unique(m_mSI$type)

summary(m_mSI)

names(all.df)
c_step
#first order plot
head(m_mSI)
ggplot(m_mSI[m_mSI$variable!="GSI",],aes(SampleNr,value,color=Trait))+
  geom_point(size=3)+geom_line(linetype="dashed")+
  ggtitle("First order GSI")+
  facet_wrap(~variable,ncol=5)+
  theme_hc()

ggplot(m_iSI[m_iSI$variable!="GSI",],aes(SampleNr,value,color=Trait))+
  geom_point(size=3)+geom_line(linetype="dashed")+
  ggtitle("Interaction GSI")+
  facet_wrap(~variable,ncol=5)+
  theme_hc()

ggplot(m_tSI[m_tSI$variable!="GSI",],aes(SampleNr,value,color=Trait))+
  geom_point(size=3)+geom_line(linetype="dashed")+
  ggtitle("Total order GSI")+
  facet_wrap(~variable,ncol=5)+
  theme_hc()




##### not working
http://iltabiai.github.io/tips/latex/2015/09/15/latex-tikzdevice-r.html
library(tikzDevice)

y <- exp(seq(1,10,.1))
x <- 1:length(y)
data <- data.frame(x = x, y = y)




tikz(file = "C:/Paper/R_Scripts/plot_test.tex", width = 5, height = 5)

plot <- ggplot(data, aes(x = x, y = y)) + 
  geom_line() +
  #Space does not appear after Latex
  ggtitle( paste("Fancy \\LaTeX ", "\\hspace{0.01cm} title")) +
  labs( x = "$x$ = Time", y = "$\\Phi$ = Innovation output") +
  theme_bw()


dev.off()
