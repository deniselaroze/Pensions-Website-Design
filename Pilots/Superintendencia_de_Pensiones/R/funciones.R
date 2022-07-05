delete.na <- function(DF, n=0) {
  DF[rowSums(is.na(DF)) <= n,]
}

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
      
      # axis.title = element_text(             #axis titles
      #              family = font,            #font family
      #              size = 10),               #font size
      # 
      # axis.text = element_text(              #axis text
      #              family = font,            #axis famuly
      #              size = 10),                #font size
      
      #axis.text.x = element_text(            #margin for axis text
      # margin=margin(5, b = 10)),
      #axis.title.x = element_text(size=12),
      axis.title.y = element_text(size=12, angle = 90, vjust = +3)
      
      #since the legend often requires manual tweaking 
      #based on plot content, don't define it here
    )
}