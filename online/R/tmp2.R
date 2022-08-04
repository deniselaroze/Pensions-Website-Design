################################
#### Ideas to test - not clean
#### author: Denise Laroze
################################
library(stargazer)
library(MASS)
#library(naniar)


#path_datos <- "C:/Users/Usuario/Documents/INVESTIGACION/MiInvestigacion/Pensions-Website-Design/"
path_datos <- "C:/Users/Denise Laroze/Dropbox/Sitios web/Datos Estudio Online/"
# If you don´t use Rprojects functionality setwd
#setwd("C:/Users/Usuario/Documents/INVESTIGACION/MiInvestigacion/Pensions-Website-Design/")
#path_github <- "C:/Users/Usuario/Documents/INVESTIGACION/MiInvestigacion/Pensions-Website-Design/Pilots/Superintendencia_de_Pensiones"

path_github <- "C:/Users/Denise Laroze/Documents/GitHub/Pensions Website Design/"



#sitios_sm <- readRDS(paste0(path_datos, "sitios_complete.rds"))


source(paste0(path_github,"Pilots/Superintendencia_de_Pensiones/R/paquetes.R"))
source(paste0(path_github,"Pilots/Superintendencia_de_Pensiones/R/funciones.R"))
#source(paste0(path_github,"Online/R/etl_surveys.R"))
#source(paste0(path_github,"Online/R/etl_sites.R"))
#source(paste0(path_github,"Online/R/etl_complete.R"))


df <- readRDS(paste0(path_datos, "online_data.rds"))


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




###############################
########## Data Analysis
###############################

####### Balance Tests 

require(nnet)
multinom_model <- multinom(Treatments ~ Age + Gender + Educ, data = df)

multinom_model <- multinom(Treatments ~ Age + Gender + Educ + financial_lit + present_bias, data = df)

stargazer(multinom_model)
df$present_bias



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
  
df.pv<-df[df$Pension_Type=="Private",]
df.pp<-df[df$Pension_Type=="Public",]

df.ns<-df[df$PlanJubi=="No sabe",]
  
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
  
  summary(lm_CR_ns)
  
  stargazer(lm_CR, lm_CR_pv, lm_CR_pp, lm_CR_F, lm_CR_M)

### Facilidad
  
  df$facil<-parse_number(df$Facilidad)

  lm_facil <- lm(facil ~ Treatments, 
                data = df[df$Pension_Type=="Public",]) 
  summary(lm_facil)

### Info ultil /////////// Potencial espacio de heterogeneidad entre públicos y privados
  
  
  df$InfoUtil_1<-as.numeric(df$InfoUtil_1)
  
  lm_util.pp <- lm(InfoUtil_1 ~ Treatments, 
                   data = df[df$Pension_Type=="Public",]) 
  lm_util.pv <- lm(InfoUtil_1 ~ Treatments, 
                   data = df[df$Pension_Type=="Private",]) 
    stargazer(lm_util.pp, lm_util.pv)
  
  
### Advisor #### Nulo
  advisor<- glm(as.factor(Advisor) ~ Treatments, data = df, family = "binomial")
  summary(advisor)
  
  
  Chang_Adv <- glm(as.factor(Change_Advisor) ~ Treatments, data = df, family = "binomial")
  summary(Chang_Adv)
  
  
### Recomendar### Los perfiles parecen generar rechazo en privados pero gusto en públicos  
  
  df$Recomendar<-as.numeric(df$Recomendar)
  
  lm_recomendar.pv <- lm(InfoUtil_1 ~ Treatments, 
                   data = df[df$Pension_Type=="Private",]) 
  lm_recomendar.pp <- lm(InfoUtil_1 ~ Treatments, 
                   data = df[df$Pension_Type=="Public",]) 
  
  stargazer(lm_recomendar.pp, lm_recomendar.pv)
  
  mean(df$Recomendar, na.rm = T)
  
  
### Exceso de Confianza
  
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

  


