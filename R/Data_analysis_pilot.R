###########################
## Pensions website Experiment
## Data Analysis
## Author: Denise Laroze
### May 2022
############################


library(naniar)
library(readr)
library(matrixStats)
library(BBmisc)
library(dplyr)
library(ggpubr)
library(ggplot2)

setwd("C:/Users/Denise Laroze/Dropbox/Sitios web/Datos Piloto/Superintendencia de Pensiones/online")

rm(list=ls())

load("Pilot_data.Rdata")

df<-encuestas




# Data Manipulation

web<-as.factor(df$website)
df$Treatments<-recode(web, 
                 "1" = "Profile",
                 "2" = "Video",
                 "3" = "VideoProfile",
                 "4" = "Profile",
                 "5" = "Video",
                 "6" = "VideoProfile",
                 "7" = "Baseline",
                 "8" = "Baseline"
)


df$utilidad<-as.numeric(df$InfoUtil_1)
df$toomuchinfo<-as.numeric(df$InfoAbruma2_1)

######



table(df$treat, df$correct_response)

names(df)
## reached the end





#### Graphics parameters
pilot_data<-df
my_comparisons <- rev(list( c("Baseline", "Profile"), c("Profile", "Video"), c("Profile", "VideoProfile"), c("Video", "VideoProfile"), c("Video", "Baseline"), c("Baseline", "VideoProfile") ))



### Color theme
theme_gppr <- function(){ 
  font <- "Georgia"   #assign font family up front
  
  ggthemes::theme_economist_white() %+replace%    #replace elements we want to change
    
    theme(
      legend.position = "bottom",
      #grid elements
      panel.grid.major = element_blank(),    #strip major gridlines
      panel.grid.minor = element_blank(),    #strip minor gridlines
      
      #since theme_minimal() already strips axis lines, 
      #we don't need to do that again
      
      #text elements
      plot.title = element_text(             #title
        family = font,            #set font family
        size = 20,                #set font size
        face = 'bold',            #bold typeface
        hjust = 0,                #left align
        vjust = 2),               #raise slightly
      
      plot.subtitle = element_text(          #subtitle
        family = font,            #font family
        size = 14),               #font size
      
      plot.caption = element_text(           #caption
        family = font,            #font family
        size = 9,                 #font size
        hjust = 1),               #right align
      
      axis.title = element_text(             #axis titles
        family = font,            #font family
        size = 10),               #font size
      
      axis.text = element_text(              #axis text
        family = font,            #axis famuly
        size = 10),                #font size
      
      axis.text.x = element_text(            #margin for axis text
        margin=margin(5, b = 10)),
      axis.title.x = element_text(size=12),
      axis.title.y = element_text(size=12, angle = 90, vjust = +3)
      
      #since the legend often requires manual tweaking 
      #based on plot content, don't define it here
    )
}



### 

h <- round(mean(pilot_data$correct_response, na.rm = TRUE), digits = 1)


  ggplot(pilot_data, aes(y = correct_response, x = Treatments, color=Treatments)) +
  geom_boxplot() +
  geom_jitter(width=0.15) +
  xlab("Treatments") +
  ylab("Corrects Responses") +
  geom_hline(aes(yintercept = h), linetype = 2, color = "gray") +
  geom_text(aes(y=h+0.5, label=paste0("Mean ", prettyNum(h,big.mark=",")), x=0.1), colour='gray', hjust=-0.1 , vjust = 1) +
  #geom_text(aes(h, label=h, hjust=-0.1)) +
  # geom_text(aes(y=mean(pilot_data$n_clicks),
  #               label=prettyNum(round(mean(pilot_data$n_clicks)),big.mark=","), x=1),
  #           colour='blue' )   +
  theme_gppr()  +
  ggsci::scale_color_aaas() + 
  ggpubr::stat_compare_means(method = "anova", vjust = 1, hjust = -0.05) +      # Add global p-value
  ggpubr::stat_compare_means(comparisons = my_comparisons, 
                             label = "p.signif", method = "wilcox.test")
#ggpubr::stat_compare_means(method = "wilcox.test")
#scale_fill_distiller()
#paletteer::scale_color_paletteer_d("colorBlindness::paletteMartin")
#scale_colour_brewer(type = "seq", palette = "Spectral")



table(df$correct_response, df$Treatments)




### Utilidad
ggplot(pilot_data, aes(y = utilidad, x = Treatments, color=Treatments)) +
  geom_boxplot() +
  geom_jitter(width=0.15) +
  xlab("Treatments") +
  ylab("Usefull information") +
  geom_hline(aes(yintercept = h), linetype = 2, color = "gray") +
  geom_text(aes(y=h+0.5, label=paste0("Mean ", prettyNum(h,big.mark=",")), x=0.1), colour='gray', hjust=-0.1 , vjust = 1) +
  #geom_text(aes(h, label=h, hjust=-0.1)) +
  # geom_text(aes(y=mean(pilot_data$n_clicks),
  #               label=prettyNum(round(mean(pilot_data$n_clicks)),big.mark=","), x=1),
  #           colour='blue' )   +
  theme_gppr()  +
  ggsci::scale_color_aaas() + 
  ggpubr::stat_compare_means(method = "anova", vjust = 1, hjust = -0.05) +      # Add global p-value
  ggpubr::stat_compare_means(comparisons = my_comparisons, 
                             label = "p.signif", method = "wilcox.test")



### too much info
ggplot(pilot_data, aes(y = toomuchinfo , x = Treatments, color=Treatments)) +
  geom_boxplot() +
  geom_jitter(width=0.15) +
  xlab("Treatments") +
  ylab("Too much information") +
  geom_hline(aes(yintercept = h), linetype = 2, color = "gray") +
  geom_text(aes(y=h+0.5, label=paste0("Mean ", prettyNum(h,big.mark=",")), x=0.1), colour='gray', hjust=-0.1 , vjust = 1) +
  #geom_text(aes(h, label=h, hjust=-0.1)) +
  # geom_text(aes(y=mean(pilot_data$n_clicks),
  #               label=prettyNum(round(mean(pilot_data$n_clicks)),big.mark=","), x=1),
  #           colour='blue' )   +
  theme_gppr()  +
  ggsci::scale_color_aaas() + 
  ggpubr::stat_compare_means(method = "anova", vjust = 1, hjust = -0.05) +      # Add global p-value
  ggpubr::stat_compare_means(comparisons = my_comparisons, 
                             label = "p.signif", method = "wilcox.test")



