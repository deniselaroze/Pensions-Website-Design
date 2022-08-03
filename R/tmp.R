################################
#### Ideas to test - not clean
#### author: Denise Laroze
################################


#path_datos <- "C:/Users/Usuario/Documents/INVESTIGACION/MiInvestigacion/Pensions-Website-Design/"
path_datos <- "C:/Users/Denise Laroze/Dropbox/Sitios web/Datos Estudio Online/"
# If you don´t use Rprojects functionality setwd
#setwd("C:/Users/Usuario/Documents/INVESTIGACION/MiInvestigacion/Pensions-Website-Design/")
#path_github <- "C:/Users/Usuario/Documents/INVESTIGACION/MiInvestigacion/Pensions-Website-Design/Pilots/Superintendencia_de_Pensiones"

path_github <- "C:/Users/Denise Laroze/Documents/GitHub/Pensions Website Design/"



df <- readRDS(paste0(path_datos, "Pilot_data.rds"))
#sitios_sm <- readRDS(paste0(path_datos, "sitios_complete.rds"))


source(paste0(path_github,"Pilots/Superintendencia_de_Pensiones/R/paquetes.R"))
#source(paste0(path_github,"Pilots/Superintendencia_de_Pensiones/R/funciones.R"))



### Overconfidence graph
  ggplot(df, aes(y = correct_response, x = Treatments, fill=Treatments)) +
  geom_boxplot() +
  labs(x ="", y = "opt out")

  
  
  
  
  opt_out<- glm(as.factor(OptOut) ~ Treatments, data = df, family = "binomial")
  
  summary(opt_out)
  
  m_respuestas <- lm(correct_response ~ Treatments, 
                     data = df)
  
  
  m_Confidence_1 <- lm(Confidence_1 ~ Treatments, 
                       data = df)
  m_n_clicks <- lm(n_clicks ~ Treatments, 
                   data = df)
 
  m_tiempo_min_1click_nclick <- lm(tiempo_min_1click_nclick ~ Treatments, 
                                   data = df)
  
  df$oo<-as.numeric(df$OptOut)
  
  
  
  
  
  
  
  
  
  
  
  
  
  
### 
  
  names(encuestaB_Privada_online)
  
  
  
  
  table(df$QMath1)
  table(df$QMath2)
  table(df$QMath3)
  
  ### Correct answers for Finantial Literacy Questions 
  
   #table(df$Qmath1)
  df$QMath1_correct<-ifelse(df$QMath1=="Más de $125.000.000", 1, 0)
  #table(df$QMath1, df$QMath1_correct)
 
  #table(df$Qmath2)
  df$QMath2_correct<-ifelse(df$QMath2=="Nunca se terminaría de pagar el crédito", 1, 0)
  #table(df$QMath2, df$QMath2_correct)
  
  #df$Qmath3num<-parse_number(df$Qmath3)
  df$QMath3_correct<-ifelse(df$QMath3 == 5000,1, ifelse(df$QMath3==5, 1, 0))
  #table(df$QMath3, df$QMath3_correct)
  
  
  tmp<-df[, c("QMath1_correct", "QMath2_correct", "QMath3_correct") ]
  
  tmp[is.na(tmp)] <- 0
  tmp$financial_lit<-rowSums(tmp)
  df$financial_lit<-tmp$financial_lit
  rm(tmp)
  
  

  
  
  # Time preferences
  df1<-grep("Q1", names(df), value=TRUE)
  df2<-grep("Q2", names(df), value=TRUE)  
  df<-as.factor(c(tmp1, tmp2)) 
  tmp<-df[, C(df)]
  
   tmp$timevalue<-NA
   for (i in 1:nrow(tmp)){
     NonNAindex <- which(!is.na(tmp[i,]))
     last <- max(NonNAindex)
     tmp$timevalue[i]<-colnames(tmp)[last]
   }

 tmp$pb<-as.numeric(gsub('\\D+','',tmp$timevalue))
 
 df$present_bias<-tmp$pb
 
 
 rm(df1, df2, df, tmp, tmp1, tmp2)
 
 
   # 
  # df$timevalue<-tmp$timevalue
  
  
 h <- round(mean(df$correct_response, na.rm = TRUE), digits = 1)
 
 df %>%
   ggplot(aes(y = correct_response, x = Treatments, color=Treatments)) +
   geom_boxplot() +
   geom_jitter(width=0.15) +
   #xlab("Treatments") +
   ylab("Correct Responses") +
   geom_hline(aes(yintercept = h), linetype = 2, color = "gray") +
   geom_text(aes(y=h+0.5, label=paste0("Mean ", prettyNum(h,big.mark=",")), x=0.1), colour='gray', hjust=-0.1 , vjust = 1) +
   facet_wrap(~ as.factor(financial_lit))
   
   #geom_text(aes(h, label=h, hjust=-0.1)) +
   # geom_text(aes(y=mean(df$n_clicks),
   #               label=prettyNum(round(mean(df$n_clicks)),big.mark=","), x=1),
   #           colour='blue' )   +
   #theme_gppr() +
   #ggsci::scale_color_aaas() + 
   #ggpubr::stat_compare_means(method = "anova", vjust = 1, hjust = -0.05) +
   #theme(axis.text.x=element_blank(),
         axis.title.y = element_text(vjust = +3),
         plot.title = element_text(vjust = -1, size = 12),
         axis.ticks.x = element_blank(),
         axis.title.x = element_blank()) +
   #scale_y_continuous(breakgrs = seq(0, 10, 2), limits = c(0,12)) 
 ggsave(paste0(path_github,"Pilots/Superintendencia_de_Pensiones/figures/responses.pdf"))
 
  
  
  


