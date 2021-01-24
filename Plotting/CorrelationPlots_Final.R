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
#library(sensitivity)

#plotting packages
library(ggplot2)
library(ggthemes)

#data frame operations5
library(reshape2)

#gridExtra
library(gridExtra)

#importing fonts
library(extrafont)
#font_import()

loadfonts(device="win")
loadfonts()

## custom function to reshape text, subscripts and etc
text_convert <- function(text_in){
  
  if (text_in =='cab'){
    text_out = expression('C'['ab'])
  }
  if (text_in =='cw'){
    text_out = expression('C'['w'])
  }
  if (text_in =='cm'){
    text_out = expression('C'['m'])
  }
  if (text_in =='lai'){
    text_out = expression('LAI')
  }
  if (text_in =='car'){
    text_out = expression('Car')
  }

  return(text_out)    
}

## custom function to generate the breaks of each input trait
break_lims <- function(text_in){

  if (text_in =='cab'){
    out_list = c(0,60,120)
  }
  if (text_in =='cw'){
    out_list = c(0,0.005,0.01)
  }
  if (text_in =='cm'){
    out_list = c(0,0.005,0.01)
  }
  if (text_in =='lai'){
    out_list = c(0,5,10)
  }
  if (text_in =='car'){
    out_list = c(0,30,60)
  }
  
  return(out_list )
}
break_lims('cab')

## custom function to generate the breaks of each input band
break_lims_bands <- function(text_in){
  
  if (text_in =='B2'){
    out_list = c(.02,.03,.04)
  }
  if (text_in =='B3'){
    out_list = c(.02,.06,.10)
  }
  if (text_in =='B4'){
    out_list = c(.02,.03,.04,.05)
  }
  if (text_in =='B5'){
    out_list = c(0,.1,.2)
  }
  if (text_in =='B6'){
    out_list = c(.2,.3,.4,.5)
  }
  if (text_in =='B7'){
    out_list = c(.3,.4,.5,.6)
  }
  if (text_in =='B8A'){
    out_list = c(.3,.4,.5,.6)
  }
  if (text_in =='B11'){
    out_list = c(.2,.3,.4)
  }
  if (text_in =='B12'){
    out_list = c(.1,.15,.2)
  }

  
  return(out_list )
}
break_lims_bands('B2')


#multiplot function -> http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}



#latex font
par(family = "LM Roman 10")


#my.df <- read.csv2("C:/Paper/Tables/Final_Data/Traits_Spectra_car.csv")
#head(my.df)
#my.df <- my.df[,-1]
#head(my.df)
#sample.rows <- sample(seq(1,nrow(my.df)),100)
#sub.df <- my.df[sample.rows,]
#sel.df <- my.df[sample.rows,]
#write.csv2(sel.df,"C:/Paper/Tables/Final_Data/Traits_Spectra_car_PlotSel.csv")

#fetching selected
sel.df <- read.csv2("C:/Paper/Tables/Final_Data/Traits_Spectra_car_PlotSel.csv")
sel.df <- sel.df[,-1]


#setting standard background theme
txt_size = 26
theme_set(theme_light(base_family ='serif',#"LM Roman 10",
                        base_size=txt_size ))

#a list to store all the pltos
plot_list <- list()

#number of breaks in the axis plots
n_steps = 3


#plot counter
i = 1

for (t_i in 10:14){
  
  print(t_i)
  print(names(sel.df)[t_i])
  
  for (b_i in 1:9){
    
    
    #loading row column
    temp.df = sel.df[,c(t_i,b_i)]
    
    #naming to 
    names(temp.df)=c("Trait","Band")
    
    #getting text for band
    band_txt = names(sel.df)[b_i]
    print(band_txt)
    
    #controling the breaks
    #max_x = max(temp.df$Trait)
    #min_x = min(temp.df$Trait)
    #stp_x = (max_x-min_x)/n_steps
    
    #max_y = max(temp.df$Band)
    #min_y = min(temp.df$Band)
    #stp_y = (max_y-min_y)/n_steps
    
    #controlling the Breaks y 
    brk_list_bands = break_lims_bands(names(sel.df)[b_i])
    brk_list = break_lims(names(sel.df)[t_i])
    
    if (i <= 9){
      txt_title = band_txt
    } else{
      txt_title = ""
    }
    
    if (band_txt == "B2"){
      
      y_txt = text_convert(names(sel.df)[t_i])
     
      
      
      p1= ggplot(temp.df,aes(x=Trait,y=Band))+
      #p1= ggplot(x=my.df[,b_i],y=my.df[,t_i])+
        #geometry types
        geom_point()+
        geom_smooth(method = "lm", linetype = "dashed")+
        
        #breaks
        scale_x_continuous(breaks = brk_list)+
        scale_y_continuous(breaks = brk_list_bands)+
        
        #theme stuff
        theme(axis.title.y=element_text(angle = 0,vjust=.5,size=txt_size + 2),
              title = element_text(hjust=.5,size=txt_size +2)#,
              #axis.text.x=element_text(txt_size+2),
              #axis.text.y=element_text(txt_size+2)
              #text=element_text(),
              )+
        labs(x="",
             y=y_txt,
             title=txt_title)#+
      
      #theme_minimal()     
      
      }
    
    
    
    else{
      
      
      
      p1= ggplot(temp.df,aes(x=Trait,y=Band))+
      #p1= ggplot(x=my.df[,b_i],y=my.df[,t_i])+
        #geometry types
        geom_point()+
        geom_smooth(method = "lm", linetype = "dashed")+
        
        #breaks
        scale_x_continuous(breaks = brk_list)+
        scale_y_continuous(breaks = brk_list_bands)+
        
        #theme stuff
        theme(#axis.title.y=element_text(angle = 0,vjust=.5,size=18),
              title = element_text(hjust=.5,size=txt_size +2)#,
              #axis.text.x=element_text(txt_size+2),
              #axis.text.y=element_text(txt_size+2)
              #text=element_text(),
        )+
        labs(x="",
             y="",
             title=txt_title)
        #+
        #theme_minimal()  
      
    }
      
    
    #adding to the list
    plot_list[[i]] <- p1
    i = i+1
    #plotter
    #plot(p1)
    
  }
  
}



library(cowplot)

pdf("C:/Paper/Tables/Final_Data/PearsonR_ByBandPlot_Corrected_Reviewed_02.pdf",width=30,height=20,paper='special')

plot_grid(plot_list[[1]],plot_list[[2]],plot_list[[3]],plot_list[[4]],plot_list[[5]],plot_list[[6]],plot_list[[7]],plot_list[[8]],plot_list[[9]],
          plot_list[[10]],plot_list[[11]],plot_list[[12]],plot_list[[13]],plot_list[[14]],plot_list[[15]],plot_list[[16]],plot_list[[17]],plot_list[[18]],
          plot_list[[19]],plot_list[[20]],plot_list[[21]],plot_list[[22]],plot_list[[23]],plot_list[[24]],plot_list[[25]],plot_list[[26]],plot_list[[27]],
          plot_list[[28]],plot_list[[29]],plot_list[[30]],plot_list[[31]],plot_list[[32]],plot_list[[33]],plot_list[[34]],plot_list[[35]],plot_list[[36]],
          plot_list[[37]],plot_list[[38]],plot_list[[39]],plot_list[[40]],plot_list[[41]],plot_list[[42]],plot_list[[43]],plot_list[[44]],plot_list[[45]],
          ncol=9,nrow=5,scale=.965, align = 'vh')

dev.off()


