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



#links of interest
#https://www.rdocumentation.org/packages/sensitivity/versions/1.17.1
#https://cran.r-project.org/web/packages/multisensi/vignettes/multisensi-vignette.pdf
#https://cran.r-project.org/web/packages/sensitivity/sensitivity.pdf
#https://www.rdocumentation.org/packages/multisensi/versions/2.1-1/topics/multisensi





#generating samples
param.maxmin <- matrix(c(#1.5, 1.9, #leaf layers or leaf structure
  15,45, #Cab
  10,50, #Car
  0.01,0.02, #Cw #original it was from [0.01 to 0.02] but i suspect saturation
  0.01,0.02, #Cm
  0.1,4.5),#LAI
  #0.05,0.1), #hotstop
  nrow=5,ncol = 2,byrow = T)

#creating a training space
#train.n <- 50 * nrow(param.maxmin) #this represents the number of runs that prosail will be, x pts per Trait
#train.LHS <- Latinhyper(param.maxmin,train.n)



#sentinel position
sun_zenith = 45
obs_zenith = 45
rel_azimut = 0


#actually we have to generate a function: 
names(train.trait.df)
x=c(10,1,.002,.002,10)
custom.prosail = function(x){
  
  Cab = x[1]
  Car = x[2]
  Cw  = x[3]
  Cm  = x[4]
  LAI = x[5]
  train.spclib = PROSAIL(Cab=Cab,Car=Car,Cw=Cw,Cm=Cm,LAI = LAI,
                         tts = sun_zenith,
                         tto = obs_zenith,
                         psi = rel_azimut)
  train.spclib <- spectralResampling(train.spclib,"Sentinel2",response_function = T)[,c(2,3,4,5,6,7,9,12,13)]
  #if output matrix
  #mat.out = spectra(train.spclib)
  #if output df
  df.out = as.data.frame(spectra(train.spclib))
  names(df.out)<- c("B02","B03","B04",
                    "B05","B06","B07",
                    "B8A","B11","B12")
  
  return(df.out)
}

x.df = t(as.data.frame(x))
names(x.df) = c("Cab","Cab","Cw","Cm","LAI")
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

library(multisensi)
custom.prosail(x)
custom.prosail.df(x.df)

#time to call the functions for one case

args.efast =list( factors=c("Cab","Car","Cw","Cm","LAI"), n=500, q = "qunif",
                  q.arg = list(list(min=5, max=120),
                               list(min=5, max=60),
                               list(min = 0.01, max = 0.05),
                               list(min = 0.01, max = 0.05),
                               list(min = 0.5,  max = 10)))

sens.99fast = multisensi(design = fast99, model = custom.prosail.df,
                         center = FALSE, reduction = NULL, analysis = analysis.sensitivity,
                         design.args=args.efast,
                         analysis.args=list(keep.outputs=FALSE))

str(sens.99fast)
sens.99fast$call.info

#multisensi::plot.gsi(sens.99fast)
plot(sens.99fast, normalized = F, color = terrain.colors)
plot(sens.99fast, normalized = T, color = terrain.colors, gsi.plot = FALSE)

m_SI  = sens.99fast$SI
m_mSI = sens.99fast$mSI
m_tSI = sens.99fast$tSI
m_iSI = sens.99fast$iSI

#First step is to find the nr of samples that stabilizess the analysis of variance


#oldschoool iterations because im sleepy
maxstep = 5000
c_step = vector()
c_step = c(c_step,100)
it = 1
while (c_step[it] <= maxstep){
  #there was this cool trick with qexp but it was creting too many iterations plot(50+(50*qexp((1:100)/100)))
  c_step = c(c_step,ceiling(c_step[it]*2))
  it=it+1
  
}



k=1
#for (i in 100:105){ #use this for testing smaller loops
for (i in c_step){
  
  print(paste("Processing sample nr:",i))
  
  args.efast =list( factors=c("Cab","Car","Cw","Cm","LAI"), n=i, q = "qunif",
                    q.arg = list(list(min=5, max=120),
                                 list(min=5, max=60),
                                 list(min = 0.01, max = 0.05),
                                 list(min = 0.01, max = 0.05),
                                 list(min = 0.5,  max = 10)))
  
  
  sens.99fast = multisensi(design = fast99, model = custom.prosail.df,dimension = NULL,
                           center = FALSE, reduction = NULL, analysis = analysis.sensitivity,
                           design.args=args.efast,
                           analysis.args=list(keep.outputs=FALSE))
  
  if (k == 1){
    #t_SI  = sens.99fast$SI
    #$Trait = rownames(t_SI)
    t_mSI = sens.99fast$mSI
    t_tSI = sens.99fast$tSI
    t_iSI = sens.99fast$iSI
    
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
    m_mSI$SampleNr = i
    m_tSI$SampleNr = i
    m_iSI$SampleNr = i
    #and we update k
    k = k+1
  }
  
  if (k>1){
    #$Trait = rownames(t_SI)
    t_mSI = sens.99fast$mSI
    t_tSI = sens.99fast$tSI
    t_iSI = sens.99fast$iSI
    
    t_mSI$Trait = rownames(t_mSI)
    t_tSI$Trait = rownames(t_tSI)
    t_iSI$Trait = rownames(t_iSI)
    
    t_mSI=melt(t_mSI,ID="Trait")
    t_tSI=melt(t_tSI,ID="Trait")
    t_iSI=melt(t_iSI,ID="Trait")
    
    t_mSI$type = "mSI"
    t_tSI$type = "tSI"
    t_iSI$type = "iSI"
    
    t_mSI$SampleNr = i
    t_tSI$SampleNr = i
    t_iSI$SampleNr = i
    
    #and now we add them to the bottom of the dataframe
    
    m_mSI <- rbind(m_mSI,t_mSI)
    m_tSI <- rbind(m_tSI,t_tSI)
    m_iSI <- rbind(m_iSI,t_iSI)
  
    k = k +1
  }

  
}


#saving all the data to separate csv
all.df = rbind(m_mSI,m_iSI,m_tSI)
write.csv2(all.df,"C:/Paper/R_Scripts/All_sensitivity")

#loading the csv
all.df = read.csv2("C:/Paper/R_Scripts/All_sensitivity.csv")
m_mSI = all.df[all.df$type=="mSI",]
m_iSI = all.df[all.df$type=="iSI",]
m_tSI = all.df[all.df$type=="tSI",]



ggplot(m_tSI[m_mSI$variable=="GSI",],aes(SampleNr,value,color=variable))+
  geom_point(size=3)+geom_line(linetype="dashed")+
  ggtitle("First order GSI")+
  facet_wrap(~Trait,ncol=5)+
  theme_hc()



unique(m_mSI$type)

summary(m_mSI)

names(all.df)
c_step
#first order plot
head(m_mSI)
ggplot(m_mSI[m_mSI$variable!="GSI",],aes(SampleNr,value,color=variable))+
  geom_point(size=3)+geom_line(linetype="dashed")+
  ggtitle("First order GSI")+
  facet_wrap(~Trait,ncol=5)+
  theme_hc()

ggplot(m_iSI,aes(SampleNr,value,color=Trait))+
  geom_point(size=3)+geom_line(linetype="dashed")+
  ggtitle("Interaction GSI")+
  facet_wrap(~variable,ncol=5)+
  theme_hc()

ggplot(m_tSI,aes(SampleNr,value,color=Trait))+
  geom_point(size=3)+geom_line(linetype="dashed")+
  ggtitle("Total order GSI")+
  facet_wrap(~variable,ncol=5)+
  theme_hc()




############################################################################################################
### repeating the run with only 2000 samples ###############################################################
############################################################################################################
### Notice -> this will override the above steps -> load the csv if you want to repeat them ################
############################################################################################################

#bar plot of Trait ~ GSI
#REmoving the GSI

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


plot(sens.99fast, normalized = T, color = terrain.colors, gsi.plot = FALSE)

#and creating the table
t_mSI = sens.99fast$mSI
t_tSI = sens.99fast$tSI
t_iSI = sens.99fast$iSI

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


#bar plot of Trait ~ GSI
#REmoving the GSI

combine.df = rbind(m_mSI,m_tSI,m_iSI)
write.csv2(combine.df,"C:/Paper/R_Scripts/All_sensitivity_2000_samples.csv")

#in one go
ggplot(combine.df[combine.df$variable!="GSI",],aes(Trait,value, fill=variable))+
  geom_bar(stat="identity",position="dodge")+
  #ggtitle("First order GSI")+
  coord_flip()+
  facet_wrap(~type,ncol=3,labeller = labeller(type = c("iSI" = "Interaction GSI",
                                                       "mSI" = "First order GSI",
                                                       "tSI" = "Total order GSI")))
  theme_hc()

#one by one

ggplot(m_mSI[m_mSI$variable!="GSI",],aes(Trait,value, fill=variable))+
  geom_bar(stat="identity",position="dodge")+
  ggtitle("First order GSI")+
  coord_flip()+
  theme_hc()

ggplot(m_iSI[m_iSI$variable!="GSI",],aes(Trait,value, fill=variable))+
  geom_bar(stat="identity",position="dodge")+
  ggtitle("Interaction GSI")+
  coord_flip()+
  theme_hc()

ggplot(m_tSI[m_tSI$variable!="GSI",],aes(Trait,value, fill=variable))+
  geom_bar(stat="identity",position="dodge")+
  ggtitle("Total order GSI")+
  coord_flip()+
  theme_hc()


#summary of first order + TSI vs bands on X axis, per trait

head(combine.df)
#names(combine.df) <- c("Trait","variable","value","Type","SampleNr")
#in one go


#lets summarize the data
head(combine.df)

combine.df <- read.csv2("C:/Paper/R_Scripts/All_sensitivity_2000_samples")

agg.df = aggregate(value~variable+type,data=combine.df[combine.df$variable!="GSI",],FUN=sum)

agg.df 

library(dplyr)

#removing the interaction because the interaction is Total - single
ymin_vec = agg.df[agg.df$type=="mSI",]
ymax_vec = agg.df[agg.df$type=="tSI",]

ggplot(agg.df[agg.df$type!="iSI",],aes(variable,value,color=type,group=type))+
  geom_point(size=3)+geom_line(linetype="dashed")+
  geom_ribbon(aes(ymin=agg.df$value,ymax=agg.df$value))+
  ylim(0,1.5)+
  theme_hc()


ggplot(ymin_vec,aes(variable,value,color="red",group=type))+
  scale_fill_discrete(labels = c("First order Sensitivity index", "Total order sensitivity index"))+
  geom_ribbon(aes(ymin=ymin_vec$value,ymax=ymax_vec$value),fill = "grey50")+
  geom_point(size=5)+
  geom_line(linetype="dashed",size=2)+
  geom_point(data=ymax_vec,aes(variable,value,color="blue",group=type),size=5)+
  geom_line(data=ymax_vec,aes(variable,value,color="blue",group=type),linetype="dashed",size=2)+
  ylim(0,1.5)+
  theme_hc()





ggplot(agg.df[agg.df$type!="iSI",],aes(variable,value,color=type,group=type))+
  geom_line(size=3)+geom_line(linetype="dashed")+
  #geom_ribbon(aes(ymin=,ymax=1))+
  ylim(0,1.5)+
  theme_hc()


help(geom_ribbon)



huron <- data.frame(year = 1875:1972, level = as.vector(LakeHuron))
