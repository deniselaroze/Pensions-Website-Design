################################
#### Ideas to test - not clean
#### author: Denise Laroze
################################
library(stargazer)
library(MASS)
#library(naniar)

#path_datos <- "C:/Users/Usuario/Documents/INVESTIGACION/MiInvestigacion/Pensions-Website-Design/"
path_datos <- "C:/Users/Denise Laroze/Dropbox/Sitios web/Datos Estudio Online/"
#path_datos <- "C:/Users/Profesor/Dropbox/Sitios web/Datos Estudio Online/"

# If you don´t use Rprojects functionality setwd
#setwd("C:/Users/Usuario/Documents/INVESTIGACION/MiInvestigacion/Pensions-Website-Design/")
#path_github <- "C:/Users/Usuario/Documents/INVESTIGACION/MiInvestigacion/Pensions-Website-Design/Pilots/Superintendencia_de_Pensiones"

path_github <- "C:/Users/Denise Laroze/Documents/GitHub/Pensions Website Design/"
#path_github <- "C:/Users/Profesor/Documents/GitHub/Pensions-Website-Design/"



#sitios_sm <- readRDS(paste0(path_datos, "sitios_complete.rds"))


source(paste0(path_github,"Online/R/paquetes.R"))
source(paste0(path_github,"Online/R/funciones.R"))
#source(paste0(path_github,"Online/R/etl_surveys.R"))
#source(paste0(path_github,"Online/R/etl_sites.R"))
#source(paste0(path_github,"Online/R/etl_complete.R"))


df <- readRDS(paste0(path_datos, "online_data.rds"))




##############################
########### Data Management
##############################
#Overconfidence
df$Confidence<-as.numeric(df$Confidence_1)
df$overconfidence<- (df$Confidence/10) / (df$correct_response/7)

View(df[, c("correct_response", "Confidence", "overconfidence")])


#Change opinion about advisor
df$Change_Advisor<-ifelse(df$PAdvice=="No" & df$Advisor=="No", "Maintain No", 
                                  ifelse (df$PAdvice=="No" & df$Advisor=="Sí", "> advisor",
                                          ifelse(df$PAdvice=="Sí" & df$Advisor=="Sí", "Maintain Yes",
                                                 ifelse(df$PAdvice=="Sí" & df$Advisor=="No", "< advisor",
                                                        ifelse(df$PAdvice=="No lo ha pensado" & df$Advisor=="Sí", "> advisor", 
                                                               ifelse(df$PAdvice=="No lo ha pensado" & df$Advisor=="No", "< advisor", "Error"
                                                               ))))))




df$pb_d<-ifelse(df$present_bias>19999, "alto", "bajo")


### Alternative Financial Literacy
### Correct answers for Finantial Literacy Questions 

#table(DF$QMath1)
df$QMath1b_correct<-ifelse(df$QMath1=="Más de $125.000.000",  1, 
                           ifelse(is.na(df$QMath1), NA, 0))
#table(DF$QMath1, DF$QMath1_correct)

#table(DF$QMath2)
df$QMath2b_correct<-ifelse(df$QMath2=="Nunca se terminaría de pagar el crédito", 1, 
                           ifelse(is.na(df$QMath1), NA, 0))
#table(DF$QMath2, DF$QMath2_correct)

tmp<-df[, c("QMath1b_correct", "QMath2b_correct") ]

tmp$financial_lit_b<-rowSums(tmp)


df$financial_lit_b<-tmp$financial_lit_b
rm(tmp)




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
  


  
  ### Correct Response      
  lm_CR <- lm(correct_response ~ Treatments + as.factor(financial_lit) , 
                 data = df) 
  
  lm_CR_pv <- lm(correct_response ~ Treatments + as.factor(financial_lit), 
              data = df[df$Pension_Type=="Public",]) 
  
  lm_CR_pp <- lm(correct_response ~ Treatments + as.factor(financial_lit), 
              data = df[df$Pension_Type=="Private",]) 
  
  lm_CR_F <- lm(correct_response ~ Treatments + as.factor(financial_lit), 
              data = df[df$Gender=="F",]) 
  
  lm_CR_M <- lm(correct_response ~ Treatments + as.factor(financial_lit), 
              data = df[df$Gender=="M",]) 
  
  lm_CR_ns <- lm(correct_response ~ Treatments + as.factor(financial_lit), 
                 data = df.ns) 
  
  stargazer(lm_CR, lm_CR_pv, lm_CR_pp, lm_CR_F, lm_CR_M, lm_CR_ns)

  
  
  ### testing financial literacy measure // there are statistical differences, keep larger model
  
  lm_CR <- lm(correct_response ~ Treatments + as.factor(financial_lit) , 
              data = df) 
  lm_CR2 <- lm(correct_response ~ Treatments + as.factor(financial_lit_b) , 
              data = df) 
  
  anova( lm_CR,lm_CR2 )
  
  
  
### Correct Responses with other controls
  
    lm_CR <- lm(correct_response ~ Treatments+ Age + Gender + Educ + pb_d + as.factor(financial_lit), 
              data = df) 
  
  lm_CR_pp <- lm(correct_response ~ Treatments + Age + Gender + Educ + pb_d + as.factor(financial_lit), 
                 data = df[df$Pension_Type=="Public",]) 
  
  lm_CR_pv <- lm(correct_response ~ Treatments + Age + Gender + Educ + pb_d + as.factor(financial_lit), 
                 data = df[df$Pension_Type=="Private",]) 
  
  lm_CR_F <- lm(correct_response ~ Treatments + Age  + Educ + pb_d + as.factor(financial_lit), 
                data = df[df$Gender=="F",]) 
  
  lm_CR_M <- lm(correct_response ~ Treatments + Age + Educ + pb_d + as.factor(financial_lit), 
                data = df[df$Gender=="M",]) 
  lm_CR_ns <- lm(correct_response ~ Treatments + Age + Gender + Educ + pb_d , 
                 data = df.ns) 
  stargazer(lm_CR, lm_CR_pp, lm_CR_pv, lm_CR_F, lm_CR_M, lm_CR_ns)
  
  
  
  
  
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
### Graphs
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
 df.g$Gender<-ifelse(df.g$Gender=="F", "Female", "Male")
 
 

 #### Opt Out
 library(broom)
 
p <- df %>%
   group_by(Treatments) %>% 
   summarise(out = sum(OptOut =="Out", na.rm=T),
             n = n()) %>%
   rowwise() %>%
   mutate(tst = list(broom::tidy(prop.test(out, n, conf.level = 0.95)))) %>%
   tidyr::unnest(tst)
 
 prop_test<-p[-5,]
 
 
 prop_test %>%  
   ggplot(aes(x=Treatments, y=estimate, fill = Treatments)) +
   geom_bar(stat="identity") +
   geom_errorbar(aes(ymin=conf.low, ymax = conf.high)) +
   theme_gppr() +
   ggsci::scale_fill_aaas() +
   theme(axis.title.x=element_blank(), axis.text.x=element_blank()) +
   geom_hline(aes(yintercept = 0.8), linetype = 2, color = "gray") +
   geom_text(aes(y=0.85, label=paste0("0.8"), x=0.1), colour='gray', hjust=-0.1 , vjust = 1) +
   ylab("Opt Out")
 
 ggsave(paste0(path_github,"online/Graphs/OptOut.pdf"))
 
 
 ### Usefulness of the information
 
 h <- round(mean(as.numeric(df.g$InfoUtil_1), na.rm = TRUE), digits = 1)
 
 df.g %>%
   dplyr::select(InfoUtil_1, Treatments)  %>%
   ggplot(aes(y = as.numeric(InfoUtil_1), x = as.factor(Treatments), color=Treatments)) +
   geom_boxplot() +
   geom_hline(aes(yintercept = h), linetype = 2, color = "gray") +
   geom_text(aes(y=h+0.5, label=paste0("Mean ", prettyNum(h,big.mark=",")), x=0.1), colour='gray', hjust=-0.1 , vjust = 1) +
   scale_y_continuous(breaks = seq(0, 10, 2), limits = c(0,16)) +
   theme_gppr() +
   scale_colour_brewer(type = "seq", palette = "Dark2")+ 
   labs(x ="", y = "Level of usefulness", title = "")  +
   theme(axis.title.y = element_text(vjust = +3),
         axis.ticks.x = element_blank(),
         plot.title = element_text(vjust = -1, size = 12),
         legend.position="none") +
   ggpubr::stat_compare_means(comparisons = my_comparisons, 
                              label = "p.signif", method = "wilcox.test")
 
 ggsave(paste0(path_github,"online/Graphs/usefulness.pdf"))
 
 ##### Usefulness Private/Public
 h_res <- round(mean(as.numeric(df.g$InfoUtil_1), na.rm = TRUE), digits = 1)
 
 df.g %>%
   dplyr::select(InfoUtil_1, Treatments, Pension_Type)  %>%
   ggplot(aes(y = as.numeric(InfoUtil_1), x = as.factor(Treatments), color = Pension_Type)) +
   geom_boxplot() +
   geom_hline(aes(yintercept = h_res), linetype = 2, color = "gray") +
   geom_text(aes(y=h_res+0.5, label=paste0("Mean ", prettyNum(h_res,big.mark=",")), x=0.1), colour='gray', hjust=-0.1 , vjust = 1) +
   scale_y_continuous(breaks = seq(0, 10, 2), limits = c(0,12)) +
   theme_gppr() +
   scale_colour_brewer(type = "seq", palette = "Dark2", name = "Type of Pension")+ 
   labs(x ="", y = "Level of usefulness", title = "")  +
   theme(axis.title.y = element_text(vjust = +3),
         axis.ticks.x = element_blank(),
         plot.title = element_text(vjust = -1, size = 12)) +
   stat_compare_means(aes(group = Pension_Type), label = "p.signif")
 
 ggsave(paste0(path_github,"online/Graphs/usefulness_het_pensiontype.pdf"))
 
 
 
 ### Correct Responses

 h <- round(mean(df.g$correct_response, na.rm = TRUE), digits = 1)
 
 df.g %>%
   dplyr::select(correct_response, Treatments)  %>%
   ggplot(aes(y = correct_response, x = as.factor(Treatments), color=Treatments)) +
   geom_boxplot() +
   geom_hline(aes(yintercept = h), linetype = 2, color = "gray") +
   geom_text(aes(y=h+0.5, label=paste0("Mean ", prettyNum(h,big.mark=",")), x=0.1), colour='gray', hjust=-0.1 , vjust = 1) +
   scale_y_continuous(breaks = seq(0, 7, 1), limits = c(0,12)) +
   theme_gppr() +
   scale_colour_brewer(type = "seq", palette = "Dark2")+ 
   labs(x ="", y = "Num. Correct Response", title = "")  +
   theme(axis.title.y = element_text(vjust = +3),
         axis.ticks.x = element_blank(),
         plot.title = element_text(vjust = -1, size = 12),
         legend.position="none") +
   ggpubr::stat_compare_means(comparisons = my_comparisons, 
                              label = "p.signif", method = "wilcox.test")
 
 ggsave(paste0(path_github,"online/Graphs/responses.pdf"))
 
 
 
 ### Correct responses Gender Het
 h <- round(mean(df.g$correct_response, na.rm = TRUE), digits = 1)

 df.g %>%
   ggplot(aes(y = correct_response, x = Treatments, color = Gender)) +
   geom_boxplot() +
   ylab("Num. Correct Responses") +
   geom_hline(aes(yintercept = h), linetype = 2, color = "gray") +
   geom_text(aes(y=h+0.5, label=paste0("Mean ", prettyNum(h,big.mark=",")), x=0.1), colour='gray', hjust=-0.1 , vjust = 1) +
   theme_gppr() +
   ggsci::scale_color_jco() + 
   stat_compare_means(aes(group = Gender), label = "p.signif")+
   theme(axis.title.y = element_text(vjust = +3),
         plot.title = element_text(vjust = -1, size = 12),
         #axis.ticks.x = element_blank(),
         #axis.title.x = element_blank()
         ) +
   scale_y_continuous(breaks = seq(0, 7, 1), limits = c(0,8))
 ggsave(paste0(path_github,"online/Graphs/responses_het_gender.pdf"))
 
 ### Correct Responses Heterogeneity Public/Private
 
 h_res <- round(mean(df.g$correct_response, na.rm = TRUE), digits = 1)
 
 df.g %>%
   dplyr::select(correct_response, Treatments, Pension_Type)  %>%
   ggplot(aes(y = correct_response, x = as.factor(Treatments), color = Pension_Type)) +
   geom_boxplot() +
   geom_hline(aes(yintercept = h_res), linetype = 2, color = "gray") +
   geom_text(aes(y=h_res+0.5, label=paste0("Mean ", prettyNum(h_res,big.mark=",")), x=0.1), colour='gray', hjust=-0.1 , vjust = 1) +
   scale_y_continuous(breaks = seq(0, 7, 1), limits = c(0,8)) +
   theme_gppr() +
   scale_colour_brewer(type = "seq", palette = "Dark2", name = "Type of Pension")+ 
   labs(x ="", y = "Num. Correct Response", title = "")  +
   theme(axis.title.y = element_text(vjust = +3),
         axis.ticks.x = element_blank(),
         plot.title = element_text(vjust = -1, size = 12)) +
   stat_compare_means(aes(group = Pension_Type), label = "p.signif")
 ggsave(paste0(path_github,"online/Graphs/responses_het_pensiontype.pdf"))
 
 
#### Self reported measures   
 
 ##########################################################
 
 ###### Confidence
 h_co <- round(mean(as.numeric(df.g$Confidence), na.rm = TRUE), digits = 1)
 
 df.g %>%
   ggplot(aes(y = as.numeric(df.g$Confidence), x = as.factor(Treatments), color = Treatments)) +
   geom_boxplot() +
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
 
 ###### overconfidence

 df.g %>%
   ggplot(aes(y = overconfidence, x = as.factor(Treatments), color = Treatments)) +
   geom_boxplot() +
   geom_hline(aes(yintercept = 0), linetype = 2, color = "gray") +
   #scale_y_continuous(limits = c(0,0.2)) +
   theme_gppr() +
   ggsci::scale_color_aaas() +
   ggsci::scale_fill_aaas() +
   ggpubr::stat_compare_means(comparisons = my_comparisons, 
                              label = "p.signif", method = "wilcox.test")+
   labs(x ="", y = "Overconfidence", title = "")  + 
   theme(legend.position="none",
         #axis.title.y = element_text(vjust = +6),
         #plot.title = element_text(vjust = -1, size = 12),
         axis.ticks.x = element_blank())

  ggsave(paste0(path_github,"online/Graphs/overconfidence.pdf"))
 
  
  #over confidence gender
  df.g %>%
    ggplot(aes(y = overconfidence, x = Treatments, color = Gender)) +
    geom_boxplot() +
    #geom_point(position = position_jitterdodge()) +
    ylab("Overconfidence") + xlab("") +
    geom_hline(aes(yintercept = 0), linetype = 2, color = "gray") +
    theme_gppr() +
    ggsci::scale_color_jco() + 
    stat_compare_means(aes(group = Gender), label = "p.signif")+
    theme(#axis.text.x=element_blank(),
          axis.title.y = element_text(vjust = +3),
          plot.title = element_text(vjust = -1, size = 12),
          axis.ticks.x = element_blank(),
          axis.title.x = element_blank()) #+
    #scale_y_continuous(limits = c(0,0.15))
  ggsave(paste0(path_github,"online/Graphs/overconfidence_het_gender.pdf"))
  
  #Promoter
  