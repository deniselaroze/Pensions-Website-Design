################################
#### Ideas to test - not clean
#### author: Denise Laroze
################################
library(stargazer)
library(MASS)
#library(naniar)


#path_datos <- "C:/Users/Usuario/Documents/INVESTIGACION/MiInvestigacion/Pensions-Website-Design/"
#path_datos <- "C:/Users/Denise Laroze/Dropbox/Sitios web/Datos Estudio Online/"
path_datos <- "C:/Users/Profesor/Dropbox/Sitios web/Datos Estudio Online/"

# If you don´t use Rprojects functionality setwd
#setwd("C:/Users/Usuario/Documents/INVESTIGACION/MiInvestigacion/Pensions-Website-Design/")
#path_github <- "C:/Users/Usuario/Documents/INVESTIGACION/MiInvestigacion/Pensions-Website-Design/Pilots/Superintendencia_de_Pensiones"

#path_github <- "C:/Users/Denise Laroze/Documents/GitHub/Pensions Website Design/"
path_github <- "C:/Users/Profesor/Documents/GitHub/Pensions-Website-Design/"



#sitios_sm <- readRDS(paste0(path_datos, "sitios_complete.rds"))


source(paste0(path_github,"Online/R/paquetes.R"))
source(paste0(path_github,"Online/R/funciones.R"))
#source(paste0(path_github,"Online/R/etl_surveys.R"))
#source(paste0(path_github,"Online/R/etl_sites.R"))
#source(paste0(path_github,"Online/R/etl_complete.R"))


df <- readRDS(paste0(path_datos, "online_data.rds"))
View(is.na(df))



##############################
###########3 Data Management
##############################
#Overconfidence
df$Confidence<-as.numeric(df$Confidence_1)
df$overconfidence<- df$Confidence/10 - df$correct_response/7


#Change opinion about advisor
df$Change_Advisor<-ifelse(df$PAdvice=="No" & df$Advisor=="No", "Maintain No", 
                                  ifelse (df$PAdvice=="No" & df$Advisor=="Sí", "> advisor",
                                          ifelse(df$PAdvice=="Sí" & df$Advisor=="Sí", "Maintain Yes",
                                                 ifelse(df$PAdvice=="Sí" & df$Advisor=="No", "< advisor",
                                                        ifelse(df$PAdvice=="No lo ha pensado" & df$Advisor=="Sí", "> advisor", 
                                                               ifelse(df$PAdvice=="No lo ha pensado" & df$Advisor=="No", "< advisor", "Error"
                                                               ))))))




df$pb_d<-ifelse(df$present_bias>19999, "alto", "bajo")


###############################
########## Data Analysis
###############################



df.pv<-df[df$Pension_Type=="Private",]
df.pp<-df[df$Pension_Type=="Public",]

df.ns<-df[df$PlanJubi=="No sabe",]



####### Balance Tests 



require(nnet)
multinom_model1 <- multinom(Treatments ~ Age + Gender + Educ, data = df)

multinom_model2 <- multinom(Treatments ~ Age + Gender + Educ + pb_d + as.factor(financial_lit), data = df)


stargazer(multinom_model1, multinom_model2)

table(df$present_bias, df$Treatments)


### simple models  

##### Opt Out
  opt_out<- glm(as.factor(OptOut) ~ Treatments, data = df, family = "binomial")
  
    opt_out.pp<- glm(as.factor(OptOut) ~ Treatments, data = df[df$Pension_Type=="Public",], family = "binomial")
  
  
    opt_out.pv<- glm(as.factor(OptOut) ~ Treatments, data =df[df$Pension_Type=="Private",], family = "binomial")
  stargazer(opt_out, opt_out.pp, opt_out.pv)  
  

#### Conceptually - Attrition is not correlated with treatments    
  
  
# Correct responses on treatment effects 
  df %>%
  group_by(Treatments) %>%
    dplyr::summarize(Mean = mean(correct_response, na.rm=TRUE),
                     sd = sd(correct_response, na.rm=TRUE))
  
  df %>%
    group_by(Treatments, Pension_Type) %>%
    dplyr::summarize(Mean = mean(correct_response, na.rm=TRUE),
                     sd = sd(correct_response, na.rm=TRUE))
  

#### Changes in preferences regarding pensions
  
  
table(df.pv$PlanJubi, df.pv$MB_Despues) ### Not clear how to interpret this

  
  
  
  ### Correct Response      
  lm_CR <- lm(correct_response ~ Treatments, 
                 data = df) 
  
  lm_CR_pv <- lm(correct_response ~ Treatments, 
              data = df[df$Pension_Type=="Public",]) 
  
  lm_CR_pp <- lm(correct_response ~ Treatments, 
              data = df[df$Pension_Type=="Private",]) 
  
  lm_CR_F <- lm(correct_response ~ Treatments, 
              data = df[df$Gender=="F",]) 
  
  lm_CR_M <- lm(correct_response ~ Treatments, 
              data = df[df$Gender=="M",]) 
  lm_CR_ns <- lm(correct_response ~ Treatments, 
                 data = df.ns) 
  stargazer(lm_CR, lm_CR_pv, lm_CR_pp, lm_CR_F, lm_CR_M, lm_CR_ns)

  
### Correct Responses with Controls
  
  
  
  lm_CR <- lm(correct_response ~ Treatments+ Age + Gender + Educ + pb_d + as.factor(financial_lit), 
              data = df) 
  
  lm_CR_pv <- lm(correct_response ~ Treatments + Age + Gender + Educ + pb_d + as.factor(financial_lit), 
                 data = df[df$Pension_Type=="Public",]) 
  
  lm_CR_pp <- lm(correct_response ~ Treatments + Age + Gender + Educ + pb_d + as.factor(financial_lit), 
                 data = df[df$Pension_Type=="Private",]) 
  
  lm_CR_F <- lm(correct_response ~ Treatments + Age  + Educ + pb_d + as.factor(financial_lit), 
                data = df[df$Gender=="F",]) 
  
  lm_CR_M <- lm(correct_response ~ Treatments + Age + Educ + pb_d + as.factor(financial_lit), 
                data = df[df$Gender=="M",]) 
  lm_CR_ns <- lm(correct_response ~ Treatments + Age + Gender + Educ + pb_d , 
                 data = df.ns) 
  stargazer(lm_CR, lm_CR_pv, lm_CR_pp, lm_CR_F, lm_CR_M, lm_CR_ns)
  
  
  
  
  
### Facilidad
  
  df$facil<-parse_number(df$Facilidad)

  lm_facil <- lm(facil ~ Treatments, 
                data = df[df$Pension_Type=="Public",]) 
  stargazer(lm_facil)

### Info util para tomar una decisión /////////// Potencial espacio de heterogeneidad entre públicos y privados
  
  
  df$InfoUtil_1<-as.numeric(df$InfoUtil_1)
  lm_util <- lm(InfoUtil_1 ~ Treatments, 
                   data = df) 
  lm_util.pp <- lm(InfoUtil_1 ~ Treatments, 
                   data = df[df$Pension_Type=="Public",]) 
  lm_util.pv <- lm(InfoUtil_1 ~ Treatments, 
                   data = df[df$Pension_Type=="Private",]) 
    stargazer(lm_util, lm_util.pp, lm_util.pv)
  
  
### Advisor #### Nulo
  advisor<- glm(as.factor(Advisor) ~ Treatments, data = df, family = "binomial")
 stargazer(advisor) ### Puede dar el video, pero se necesitan más datos para tener más seguridad
  
  
  Chang_Adv <- glm(as.factor(Change_Advisor) ~ Treatments, data = df, family = "binomial")
  summary(Chang_Adv)
  
  
### Recomendar### Los perfiles parecen generar rechazo en privados pero gusto en públicos  
  
  df$Recomendar<-as.numeric(df$Recomendar)
  lm_recomendar<- lm(Recomendar ~ Treatments, 
                         data = df) 
  lm_recomendar.pv <- lm(Recomendar ~ Treatments, 
                   data = df[df$Pension_Type=="Private",]) 
  lm_recomendar.pp <- lm(Recomendar ~ Treatments, 
                   data = df[df$Pension_Type=="Public",]) 
  
  stargazer(lm_recomendar, lm_recomendar.pp, lm_recomendar.pv)
  
  mean(df$Recomendar, na.rm = T)
  
  
### Exceso de Confianza // Videos reduce excess confidence
  
 mean(df$Confidence, na.rm=T)
 
 lm_overconf<-lm(overconfidence~ Treatments, 
                 data = df)
 
 lm_overconf.pp<-lm(overconfidence~ Treatments, 
                 data = df[df$Pension_Type=="Public",])
 
 lm_overconf.pv<-lm(overconfidence~ Treatments, 
                 data = df[df$Pension_Type=="Private",])
 
 stargazer(lm_overconf, lm_overconf.pp, lm_overconf.pv)
 
 
 
 
  
#########################
### Gráficos
#######################
 # Parameters
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
 my_comparisons <- list( c("Baseline", "Perfil"), c("Perfil", "Video"), c("Video", "VideoPerfil"), c("Video", "Baseline"), c("Perfil", "VideoPerfil"), c("Baseline", "VideoPerfil") )
 df.g<-df[!is.na(df$correct_response),]

 #### Opt Out
 df$OO.n<-as.numeric(as.factor(df$OptOut))-1
 mean<-mean(as.numeric(df$OO.n), na.rm=T)
 
 proporciones_oo <- df %>%
   dplyr::select(Treatments, OO.n) %>%
   na.omit() %>%
   group_by(Treatments) %>% 
   summarise(prop = sum(OO.n ==1)/n()) %>%  
   na.omit() 
 #proporciones
 
 
 proporciones_oo %>%
   ggplot(aes(x=Treatments, y=prop, fill = Treatments)) +
   geom_bar(stat="identity") +
   theme_gppr() +
   ggsci::scale_fill_aaas() +
   theme(axis.title.x=element_blank(), axis.text.x=element_blank()) +
   geom_hline(aes(yintercept = mean), linetype = 2, color = "gray") +
   geom_text(aes(y=0.55, label=paste0("0.5"), x=0.1), colour='gray', hjust=-0.1 , vjust = 1) +
   ylab("Opt Out of responding questions")
 
 ggsave(paste0(path_github,"online/Graphs/OptOut.pdf"))
 
 
 ### Usefulness of the information
 
 h <- round(mean(as.numeric(df.g$InfoUtil_1), na.rm = TRUE), digits = 1)
 
 df.g %>%
   dplyr::select(InfoUtil_1, Treatments)  %>%
   ggplot(aes(y = as.numeric(InfoUtil_1), x = as.factor(Treatments))) +
   geom_boxplot() +
   geom_hline(aes(yintercept = h), linetype = 2, color = "gray") +
   geom_text(aes(y=h+0.5, label=paste0("Mean ", prettyNum(h,big.mark=",")), x=0.1), colour='gray', hjust=-0.1 , vjust = 1) +
   scale_y_continuous(breaks = seq(0, 10, 2), limits = c(0,16)) +
   theme_gppr() +
   scale_colour_brewer(type = "seq", palette = "Dark2")+ 
   labs(x ="Treatments", y = "Level of usefulness", title = "")  +
   theme(axis.title.y = element_text(vjust = +3),
         axis.ticks.x = element_blank(),
         plot.title = element_text(vjust = -1, size = 12)) +
   ggpubr::stat_compare_means(comparisons = my_comparisons, 
                              label = "p.signif", method = "wilcox.test")
 
 ggsave(paste0(path_github,"online/Graphs/usefulness.pdf"))
 
 ##### Usefulness Private/Public
 h_res <- round(mean(as.numeric(df.g$InfoUtil_1), na.rm = TRUE), digits = 1)
 
 df.g %>%
   dplyr::select(InfoUtil_1, Treatments, Pension_Type)  %>%
   ggplot(aes(y = as.numeric(InfoUtil_1), x = as.factor(Treatments), color = Pension_Type)) +
   geom_boxplot() +
   geom_point(position = position_jitterdodge()) +
   geom_hline(aes(yintercept = h_res), linetype = 2, color = "gray") +
   geom_text(aes(y=h_res+0.5, label=paste0("Mean ", prettyNum(h_res,big.mark=",")), x=0.1), colour='gray', hjust=-0.1 , vjust = 1) +
   scale_y_continuous(breaks = seq(0, 10, 2), limits = c(0,12)) +
   theme_gppr() +
   scale_colour_brewer(type = "seq", palette = "Dark2")+ 
   labs(x ="Treatments", y = "Level of usefulness", title = "")  +
   theme(axis.title.y = element_text(vjust = +3),
         axis.ticks.x = element_blank(),
         plot.title = element_text(vjust = -1, size = 12)) +
   stat_compare_means(aes(group = Pension_Type), label = "p.signif")
 
 ggsave(paste0(path_github,"online/Graphs/usefulness_het_pensiontype.pdf"))
 
 
 
 ### Correct Responses

 h <- round(mean(df.g$correct_response, na.rm = TRUE), digits = 1)
 
 df.g %>%
   dplyr::select(correct_response, Treatments)  %>%
   ggplot(aes(y = correct_response, x = as.factor(Treatments))) +
   geom_boxplot() +
   geom_hline(aes(yintercept = h), linetype = 2, color = "gray") +
   geom_text(aes(y=h+0.5, label=paste0("Mean ", prettyNum(h,big.mark=",")), x=0.1), colour='gray', hjust=-0.1 , vjust = 1) +
   scale_y_continuous(breaks = seq(0, 10, 2), limits = c(0,12)) +
   theme_gppr() +
   scale_colour_brewer(type = "seq", palette = "Dark2")+ 
   labs(x ="Treatments", y = "Num. Correct Response", title = "")  +
   theme(axis.title.y = element_text(vjust = +3),
         axis.ticks.x = element_blank(),
         plot.title = element_text(vjust = -1, size = 12)) +
   ggpubr::stat_compare_means(comparisons = my_comparisons, 
                              label = "p.signif", method = "wilcox.test")
 
 ggsave(paste0(path_github,"online/Graphs/responses.pdf"))
 
 
 
 ### Correct responses Gender Het
 h <- round(mean(df.g$correct_response, na.rm = TRUE), digits = 1)

 df.g %>%
   ggplot(aes(y = correct_response, x = Treatments, color = factor(Gender))) +
   geom_boxplot() +
   geom_point(position = position_jitterdodge()) +
   ylab("Num. Correct Responses") +
   geom_hline(aes(yintercept = h), linetype = 2, color = "gray") +
   geom_text(aes(y=h+0.5, label=paste0("Mean ", prettyNum(h,big.mark=",")), x=0.1), colour='gray', hjust=-0.1 , vjust = 1) +
   theme_gppr() +
   ggsci::scale_color_jco() + 
   stat_compare_means(aes(group = Gender), label = "p.signif")+
   theme(axis.text.x=element_blank(),
         axis.title.y = element_text(vjust = +3),
         plot.title = element_text(vjust = -1, size = 12),
         axis.ticks.x = element_blank(),
         axis.title.x = element_blank()) +
   scale_y_continuous(breaks = seq(0, 10, 2), limits = c(0,12))
 ggsave(paste0(path_github,"online/Graphs/responses_het_gender.pdf"))
 
 ### Correct Responses Heterogeneity Public/Private
 
 h_res <- round(mean(df.g$correct_response, na.rm = TRUE), digits = 1)
 
 df.g %>%
   dplyr::select(correct_response, Treatments, Pension_Type)  %>%
   ggplot(aes(y = correct_response, x = as.factor(Treatments), color = Pension_Type)) +
   geom_boxplot() +
   geom_point(position = position_jitterdodge()) +
   geom_hline(aes(yintercept = h_res), linetype = 2, color = "gray") +
   geom_text(aes(y=h_res+0.5, label=paste0("Mean ", prettyNum(h_res,big.mark=",")), x=0.1), colour='gray', hjust=-0.1 , vjust = 1) +
   scale_y_continuous(breaks = seq(0, 10, 2), limits = c(0,12)) +
   theme_gppr() +
   scale_colour_brewer(type = "seq", palette = "Dark2")+ 
   labs(x ="Treatments", y = "Num. Correct Response", title = "")  +
   theme(axis.title.y = element_text(vjust = +3),
         axis.ticks.x = element_blank(),
         plot.title = element_text(vjust = -1, size = 12)) +
   stat_compare_means(aes(group = Pension_Type), label = "p.signif")
 ggsave(paste0(path_github,"online/Graphs/responses_het_pensiontype.pdf"))
 
 
#### Self reported measures   
 
 ##########################################################
 
 ######
 h_cu <- round(mean(dependent$Curiosity_1, na.rm = TRUE), digits = 1)
 h_co <- round(mean(dependent$Confidence_1, na.rm = TRUE), digits = 1)
 h_a <- round(mean(dependent$InfoAbruma2_1, na.rm = TRUE), digits = 1)
 cu <- dependent %>%
   mutate_if(is.factor,as.character) %>%
   mutate(Treatments = as.factor(Treatments)) %>%
   mutate_if(is.character,as.numeric) %>%
   ggplot(aes(y = Curiosity_1, x = as.factor(Treatments), color = Treatments)) +
   geom_boxplot() +
   geom_jitter() +
   geom_hline(aes(yintercept = h_cu), linetype = 2, color = "gray") +
   geom_text(aes(y=h_cu+0.5, label=paste0("Mean ", prettyNum(h_cu,big.mark=",")), x=0.1), colour='gray', hjust=-0.1 , vjust = 1) +
   scale_y_continuous(breaks = seq(0, 10, 2), limits = c(0,12)) +
   theme_gppr() +
   ggpubr::stat_compare_means(comparisons = my_comparisons, 
                              label = "p.signif", method = "wilcox.test") +
   ggsci::scale_color_aaas() +
   ggsci::scale_fill_aaas() +
   labs(x ="", y = "", title = "Curiosity")  +
   theme(axis.text.x=element_blank(),
         axis.title.y = element_text(vjust = +6),
         axis.ticks.x = element_blank(),
         plot.title = element_text(vjust = -1, size = 12))
 co <- dependent %>%
   mutate_if(is.factor,as.character) %>%
   mutate(Treatments = as.factor(Treatments)) %>%
   mutate_if(is.character,as.numeric) %>%
   ggplot(aes(y = Confidence_1, x = as.factor(Treatments), color = Treatments)) +
   geom_boxplot() +
   geom_jitter() +
   geom_hline(aes(yintercept = h_co), linetype = 2, color = "gray") +
   geom_text(aes(y=h_co+0.5, label=paste0("Mean ", prettyNum(h_co,big.mark=",")), x=0.1), colour='gray', hjust=-0.1 , vjust = 1) +
   scale_y_continuous(breaks = seq(0, 10, 2), limits = c(0,12)) +
   theme_gppr() +
   ggsci::scale_color_aaas() +
   ggsci::scale_fill_aaas() +
   ggpubr::stat_compare_means(comparisons = my_comparisons, 
                              label = "p.signif", method = "wilcox.test")+
   labs(x ="", y = "", title = "Confidence")  +
   theme(axis.text.x=element_blank(),
         axis.title.y = element_text(vjust = +6),
         plot.title = element_text(vjust = -1, size = 12),
         axis.ticks.x = element_blank())
 abru <- dependent %>%
   mutate_if(is.factor,as.character) %>%
   mutate(Treatments = as.factor(Treatments)) %>%
   mutate_if(is.character,as.numeric) %>%
   ggplot(aes(y = InfoAbruma2_1, x = as.factor(Treatments), color = Treatments)) +
   geom_boxplot() +
   geom_jitter() +
   geom_hline(aes(yintercept = h_a), linetype = 2, color = "gray") +
   geom_text(aes(y=h_a+0.5, label=paste0("Mean ", prettyNum(h_a,big.mark=",")), x=0.1), colour='gray', hjust=-0.1 , vjust = 1) +
   scale_y_continuous(breaks = seq(0, 10, 2), limits = c(0,12)) +
   theme_gppr() +
   ggsci::scale_color_aaas() +
   ggsci::scale_fill_aaas() +
   labs(x ="", y = "Too much information", title = "")  +
   ggpubr::stat_compare_means(comparisons = my_comparisons, 
                              label = "p.signif", method = "wilcox.test")+
   theme(axis.text.x=element_blank(),
         axis.title.y = element_text(vjust = +3),
         plot.title = element_text(vjust = -1, size = 12),
         axis.ticks.x = element_blank())
 
 ggarrange(cu, co,  abru, common.legend = TRUE, legend = "bottom", ncol = 3)
 ```
 
 ```{r, utility}
 symnum.args = list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 0.1, 1), 
                    symbols = c("****", "***", "**", "*",".", "ns"))
 mis_comparaciones <- list( c("Baseline", "Perfil"), c("Perfil", "Video"), c("Video", "VideoPerfil"), c("Video", "Baseline"), c("Perfil", "VideoPerfil"), c("Baseline", "VideoPerfil") )
 h_u <- round(mean(dependent$InfoUtil_1, na.rm = TRUE), digits = 1)
 dependent %>%
   mutate_if(is.factor,as.character) %>%
   mutate(Treatments = as.factor(Treatments)) %>%
   mutate_if(is.character,as.numeric) %>%
   ggplot(aes(y = InfoUtil_1, x = as.factor(Treatments), color = Treatments)) +
   geom_boxplot() +
   geom_jitter() +
   geom_hline(aes(yintercept = h_u), linetype = 2, color = "gray") +
   geom_text(aes(y=h_u+0.5, label=paste0("Mean ", prettyNum(h_a,big.mark=",")), x=0.1), colour='gray', hjust=-0.1 , vjust = 1)  +
   theme_gppr() +
   ggsci::scale_color_aaas() +
   ggsci::scale_fill_aaas() +
   labs(x ="", y = "Utility", title = "")  +
   ggpubr::stat_compare_means(comparisons = my_comparisons, 
                              label = "p.signif", method = "wilcox.test", 
                              hide.ns = TRUE) +
   theme(axis.text.x=element_blank(),
         axis.title.y = element_text(vjust = +3),
         plot.title = element_text(vjust = -1, size = 12),
         axis.ticks.x = element_blank()) +
   coord_cartesian(ylim = c(2,15))
 ggsave(paste0(path_github,"Pilots/Superintendencia_de_Pensiones/figures/utility.pdf"))
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 symnum.args = list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 0.1, 1), 
                     symbols = c("****", "***", "**", "*",".", "ns"))
  mis_comparaciones <- list( c("Baseline", "Perfil"), c("Perfil", "Video"), c("Video", "VideoPerfil"), c("Video", "Baseline"), c("Perfil", "VideoPerfil"), c("Baseline", "VideoPerfil") )
  h_u <- round(mean(df$Recomendar, na.rm = TRUE), digits = 1)
    mutate_if(is.factor,as.character) %>%
    mutate(Treatments = as.factor(Treatments)) %>%
    mutate_if(is.character,as.numeric) %>%
    ggplot(aes(y = Recomendar, x = as.factor(Treatments), color = Treatments)) +
    geom_boxplot() +
    geom_jitter() +
    geom_hline(aes(yintercept = h_u), linetype = 2, color = "gray") +
    geom_text(aes(y=h_u+0.5, label=paste0("Mean ", prettyNum(h_a,big.mark=",")), x=0.1), colour='gray', hjust=-0.1 , vjust = 1)  +
    theme_gppr() +
    ggsci::scale_color_aaas() +
    ggsci::scale_fill_aaas() +
    labs(x ="", y = "Recomendar", title = "")  +
    ggpubr::stat_compare_means(comparisons = my_comparisons, 
                               label = "p.signif", method = "wilcox.test", 
                               hide.ns = TRUE) +
    theme(axis.text.x=element_blank(),
          axis.title.y = element_text(vjust = +3),
          plot.title = element_text(vjust = -1, size = 12),
          axis.ticks.x = element_blank()) +
    coord_cartesian(ylim = c(2,15))
  ggsave(paste0(path_github,"Pilots/Superintendencia_de_Pensiones/figures/utility.pdf"))
  
  
#### Correct Response Graph by Pension Type  
 h <- round(mean(df$correct_response, na.rm = TRUE), digits = 1)
 
 df %>%
   ggplot(aes(y = correct_response, x = Treatments, color=Treatments)) +
   geom_boxplot() +
   geom_jitter(width=0.15) +
   #xlab("Treatments") +
   ylab("Correct Responses") +
   geom_hline(aes(yintercept = h), linetype = 2, color = "gray") +
   geom_text(aes(y=h+0.5, label=paste0("Mean ", prettyNum(h,big.mark=",")), x=0.1), colour='gray', hjust=-0.1 , vjust = 1) +
   facet_wrap(~ as.factor(Pension_Type))
 
 #### Correct Response Graph by Gender  
 h <- round(mean(df$correct_response, na.rm = TRUE), digits = 1)
 
 df %>%
   ggplot(aes(y = correct_response, x = Treatments, color=Treatments)) +
   geom_boxplot() +
   geom_jitter(width=0.15) +
   #xlab("Treatments") +
   ylab("Correct Responses") +
   geom_hline(aes(yintercept = h), linetype = 2, color = "gray") +
   geom_text(aes(y=h+0.5, label=paste0("Mean ", prettyNum(h,big.mark=",")), x=0.1), colour='gray', hjust=-0.1 , vjust = 1) +
   facet_wrap(~ as.factor(Gender))
 
 
 
 #### Recomendar Graph by Pension Type
 h <- round(mean(df$Recomendar, na.rm = TRUE), digits = 1)
 
 df %>%
   ggplot(aes(y = Recomendar, x = Treatments, color=Treatments)) +
   geom_boxplot() +
   geom_jitter(width=0.15) +
   #xlab("Treatments") +
   ylab("Recomendar") +
   geom_hline(aes(yintercept = h), linetype = 2, color = "gray") +
   geom_text(aes(y=h+0.5, label=paste0("Mean ", prettyNum(h,big.mark=",")), x=0.1), colour='gray', hjust=-0.1 , vjust = 1) +
   facet_wrap(~ as.factor(Pension_Type))
 

 #### Exceso de confianza y Pension_type
 h <- round(mean(df$overconfidence, na.rm = TRUE), digits = 1)
 
 df %>%
   ggplot(aes(y = overconfidence, x = Treatments, color=Treatments)) +
   geom_boxplot() +
   geom_jitter(width=0.15) +
   #xlab("Treatments") +
   ylab("Exceso de confianza") +
   geom_hline(aes(yintercept = h), linetype = 2, color = "gray") +
   geom_text(aes(y=h+0.5, label=paste0("Mean ", prettyNum(h,big.mark=",")), x=0.1), colour='gray', hjust=-0.1 , vjust = 1) +
   facet_wrap(~ as.factor(Pension_Type))
 
 
 #### Exceso de confianza y género
 h <- round(mean(df$overconfidence, na.rm = TRUE), digits = 1)
 
 df %>%
   ggplot(aes(y = overconfidence, x = Treatments, color=Treatments)) +
   geom_boxplot() +
   geom_jitter(width=0.15) +
   #xlab("Treatments") +
   ylab("Exceso de confianza") +
   geom_hline(aes(yintercept = h), linetype = 2, color = "gray") +
   geom_text(aes(y=h+0.5, label=paste0("Mean ", prettyNum(h,big.mark=",")), x=0.1), colour='gray', hjust=-0.1 , vjust = 1) +
   facet_wrap(~ as.factor(Gender))
 
 
 
   
   #geom_text(aes(h, label=h, hjust=-0.1)) +
   # geom_text(aes(y=mean(df$n_clicks),
   #               label=prettyNum(round(mean(df$n_clicks)),big.mark=","), x=1),
   #           colour='blue' )   +
   #theme_gppr() +

  


