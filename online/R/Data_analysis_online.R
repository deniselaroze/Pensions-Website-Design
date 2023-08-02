################################
#### Ideas to test - not clean
#### author: Denise Laroze
################################
library(stargazer)
library(MASS)
library(broom)
library(ggpubr)
library(naniar)
library(nnet)


rm(list=ls())
#path_datos <- "C:/Users/Usuario/Documents/INVESTIGACION/MiInvestigacion/Pensions-Website-Design/"
#path_datos <- "C:/Users/Usach/Dropbox/Sitios web/Datos Estudio Online/"
path_datos <- "C:/Users/Denise/Dropbox/Sitios web/Datos Estudio Online/"
#path_datos <- "C:/Users/Denise Laroze/Dropbox/Sitios web/Datos Estudio Online/"


# If you don´t use Rprojects functionality setwd
#path_github <- "C:/Users/Usach/OneDrive - usach.cl/Documents/GitHub/Pensions-Website-Design/"
path_github <- "C:/Users/Denise/Documents/GitHub/Pensions-Website-Design/"
#path_github <- "C:/Users/Denise Laroze/Documents/GitHub/Pensions Website Design/"



#sitios_sm <- readRDS(paste0(path_datos, "sitios_complete.rds"))


source(paste0(path_github,"online/R/paquetes.R"))
source(paste0(path_github,"online/R/funciones.R"))
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

#View(df[, c("correct_response", "Confidence", "overconfidence")])


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

# Income level proxy though health care provider
table(df$HSist)
df$private_health<-ifelse( df$HSist == "ISAPRE" | df$HSist == "FF.AA. y el Orden", "Private healthcare", "Public Health or other")

df$private_health <-factor(df$private_health, levels = c("Public Health or other", "Private healthcare"))

table(df$HSist, df$private_health)



df$Profile<-ifelse(df$Treatments %in% c("Perfil", "VideoPerfil"), "Profile", "Product")
#table(df$Treatments, df$Profile)
#df$Profile <- ordered(df$Profile, levels = c("Profile", "Product"))
#levels(df$Profile)
df$Profile<-as.factor(df$Profile)
df<- within(df, Profile <- relevel(Profile, ref = "Product"))



df$Video<-ifelse(df$Treatments %in% c("Video", "VideoPerfil"), "Video", "Text")
table(df$Treatments, df$Video)

df$Profile_Video<-ifelse(df$Treatments=="VideoPerfil", "Profile_Video", "Other")
#table(df$Treatments, df$Profile_Video)



############################
###### Subsets of data
##################################
# subgroups

df.f<-df[!is.na(df$correct_response),]


df.pv<-df[df$Pension_Type=="Private",]
df.pp<-df[df$Pension_Type=="Public",]

df.ns<-df[df$PlanJubi=="No sabe",]

############################
###### Descriptive Statistics
##################################


### Correct responses

encuestas <- readRDS(paste0(path_datos, "encuestas_clean2.rds"))

prop.table(table(encuestas$obliga))
prop.table(table(encuestas$inicio))
prop.table(table(encuestas$elegir))
prop.table(table(encuestas$sirve.scmp))
prop.table(table(encuestas$scmp.twice))
prop.table(table(encuestas$asesor))
prop.table(table(encuestas$propiedad))

prop.table(table(encuestas$ncomp1))
prop.table(table(encuestas$ncomp2))
prop.table(table(encuestas$ncomp3))
prop.table(table(encuestas$ncomp4))
prop.table(table(encuestas$ncomp5))
prop.table(table(encuestas$ncomp6))
prop.table(table(encuestas$ncomp7))

###

# summary statistics


prop.table(table(df.f$financial_lit_b))
prop.table(table(df.f$HSist))


prop.table(table(df.f$genero))
prop.table(table(df.f$Educ))
summary(2022-as.numeric(df.f$Birth))

table(df.f$Pension_Type)



###############################
########## Data Analysis
###############################

####### Balance Tests 
multinom_model1 <- multinom(Treatments ~ Age + Gender + Educ + private_health + pb_d + financial_lit_b, data = df)

multinom_model2 <- multinom(Treatments ~ Age + Gender + Educ + private_health  + pb_d + as.factor(financial_lit_b), data = df)


stargazer(multinom_model2)

stargazer(multinom_model2, out=paste0(path_github,"online/Outputs/balance.tex"), type="latex",
          covariate.labels = c("Age", "Male", "Education: High School", "University or technical college", 
                               "Postgraduate degree", "Private healthcare", "Low present bias",  "Mid Fin. Lit.", "High Fin. Lit.", "Constant"), 
          dep.var.labels = c("T.Profile", "T.Video", "T.Video and Profile"), # keep.stat=c("n", "ll"),
          dep.var.caption = "", star.cutoffs = c(0.05, 0.01, 0.001), notes.align = "l", table.placement = "H",
          label="tbl:balance",
          title = "Multinomial logit models on Treatment assignment by socio-demoraphic characteristics  - balance test", no.space=TRUE)


### simple models  

##### Opt Out
  opt_out1<- glm(as.factor(OptOut) ~ Treatments + as.factor(financial_lit_b), data = df, family = "binomial")
  
  ### Out out robustness tests ---- Missing review on just C
  opt_out2<- glm(as.factor(OptOut) ~ Treatments + Age + Gender + Educ + private_health  + pb_d + as.factor(financial_lit_b), data = df, family = "binomial")
  #opt_out3<- glm(as.factor(OptOut) ~ Treatments, data = df, family = "binomial")
  #opt_out4<- glm(as.factor(OptOut) ~ Treatments Age + Gender + Educ + private_health  + pb_d + as.factor(financial_lit_b), data = df, family = "binomial")
  opt_out.pp<- glm(as.factor(OptOut) ~ Treatments, data = df[df$Pension_Type=="Public",], family = "binomial")
  opt_out.pv<- glm(as.factor(OptOut) ~ Treatments, data =df[df$Pension_Type=="Private",], family = "binomial")
  
  stargazer(opt_out1, opt_out2)  
  

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
  

##################################################3
  ###### Table Opt out and correct responses online
###################################################  
  ##### Opt Out
  opt_out1<- glm(as.factor(OptOut) ~  Profile + Video + Profile_Video  + as.factor(financial_lit_b), data = df, family = "binomial")
  
  ### Correct Response      
  lm_CR <- lm(correct_response ~ Profile + Video + Profile_Video + as.factor(financial_lit_b) , 
              data = df) 
  
  lm_CR_pv <- lm(correct_response ~ Profile + Video + Profile_Video + as.factor(financial_lit_b), 
                 data = df[df$Pension_Type=="Public",]) 
  
  lm_CR_pp <- lm(correct_response ~ Profile + Video + Profile_Video + as.factor(financial_lit_b), 
                 data = df[df$Pension_Type=="Private",]) 
  
  lm_CR_F <- lm(correct_response ~ Profile + Video + Profile_Video + as.factor(financial_lit_b), 
                data = df[df$Gender=="F",]) 
  
  lm_CR_M <- lm(correct_response ~ Profile + Video + Profile_Video + as.factor(financial_lit_b), 
                data = df[df$Gender=="M",]) 
  
  lm_CR_ns <- lm(correct_response ~ Profile + Video + Profile_Video + as.factor(financial_lit_b), 
                 data = df[df$PlanJubi=="No sabe",]) 
  
  stargazer(opt_out1, lm_CR, lm_CR_pv, lm_CR_pp, lm_CR_F, lm_CR_M, lm_CR_ns)
  
  stargazer(opt_out1, lm_CR, lm_CR_pv, lm_CR_pp, lm_CR_F, lm_CR_M, lm_CR_ns, out=paste0(path_github,"online/Outputs/main_results_correct_response.tex"), type="latex",
            covariate.labels = c("Profile", "Video", "Video and Profile", "Mid Fin. Lit.", "High Fin. Lit.", "Constant"), 
            dep.var.labels = c("Finish Tutorial", "Number of correct responses"), # keep.stat=c("n", "ll"),
            dep.var.caption = "", star.cutoffs = c(0.05, 0.01, 0.001), notes.align = "l", table.placement = "H",
            label="tbl:Main_results_correct_response",
            title = "Column 1 presents a logit model on completing the tutorial. Columns 2-7 are OLS models on the number
            of correct responses, for different sub-groups of the sample", no.space=TRUE)
  
  
  
  
  
  
  
  
  
    
  
  ### testing financial literacy measure / no difference between the models, keep smaller one
  
  lm_CR <- lm(correct_response ~ Treatments + as.factor(financial_lit) , 
              data = df) 
  
  df$financial_lit_b<-ifelse(df$financial_lit==3, 2, df$financial_lit)
  View(df[c("financial_lit", "financial_lit_b")])
  
  lm_CR2 <- lm(correct_response ~ Treatments + as.factor(financial_lit_b) , 
              data = df) 
  
  anova( lm_CR,lm_CR2 )
  
  
  
### Correct Responses with other controls
  
    lm_CR <- lm(correct_response ~ Treatments+ Age + Gender + Educ + pb_d + as.factor(financial_lit_b), 
              data = df) 
  
  lm_CR_pp <- lm(correct_response ~ Treatments + Age + Gender + Educ + pb_d + as.factor(financial_lit_b), 
                 data = df[df$Pension_Type=="Public",]) 
  
  lm_CR_pv <- lm(correct_response ~ Treatments + Age + Gender + Educ + pb_d + as.factor(financial_lit_b), 
                 data = df[df$Pension_Type=="Private",]) 
  
  lm_CR_F <- lm(correct_response ~ Treatments + Age  + Educ + pb_d + as.factor(financial_lit_b), 
                data = df[df$Gender=="F",]) 
  
  lm_CR_M <- lm(correct_response ~ Treatments + Age + Educ + pb_d + as.factor(financial_lit_b), 
                data = df[df$Gender=="M",]) 
  lm_CR_ns <- lm(correct_response ~ Treatments + Age + Gender + Educ + pb_d , 
                 data = df.ns) 
  stargazer(lm_CR, lm_CR_pp, lm_CR_pv, lm_CR_F, lm_CR_M, lm_CR_ns)
  

  ###############################################
  ### H4 - Self-reported measures of the tutorial  
  ###############################################
  
  #Estimate NPS by treatment
  library(marketr)
  library(xtable)
  
  nps_question<-as.numeric(df.f$Recomendar)
  nps_group<-df.f$Treatments
  nps_date<-as.Date("2023-07-06")
  d <- data.frame(nps_question, nps_date, nps_group)
  tbl<-nps_calc(d, nps_group)
  
  xt<-xtable(tbl)
  print(xt, type="latex", file=paste0(path_github,"online/Outputs/nps_table.tex"), 
        floating=FALSE, include.rownames=FALSE)
  
  
  #NPS by treatment regression
  nps<-lm(as.numeric(Recomendar) ~ Treatments, df.f)
  summary(nps)
  
  
  
  ####################################
  ### Financial literacy Heterogeneity
  ####################################
  
  library(BayesTree)
  set.seed(89536)
  
  # Data set up including calculating ability rank
  df.b <- df.f
  
  
  #set up  - divide into 1 treatment group and 0 control
  df.b$treat <- df.b$Treatments
  risk.vars<-c("Baseline" ,     "Perfil")
  df.b$treat.het<-ifelse(df.b$treat %in% risk.vars, 0, 1)
  df.b$gender <- ifelse(df.b$Gender == "F",1,0)
  df.b$private <- ifelse(df.b$Pension_Type == "Private",1,0) 
  df.b$health<-ifelse(df.b$HSist=="ISAPRE",  1, 0)
  
  # Define model variables incl. outcome as column 1
  vars <- c("correct_response", "treat.het", "financial_lit_b", "gender", "private")
  
  
  df.b <- df.b[,vars]
  df.b <- df.b[complete.cases(df.b),]
  
  # Separate outcome and training data
  y <- df.b$correct_response
  train <- df.b[,-1]
  
  # Gen. test data where those treated become untreated, for use in calculating ITT
  test <- train
  test$treat.het <- ifelse(test$treat.het == 1,0,ifelse(test$treat.het == 0,1,NA))
  
  # Run BART for predicted values of observed and synthetic observations
  bart.out <- bart(x.train = train, y.train = y, x.test = test)
  
  # Recover CATE estimates and format into dataframe
  # Logic: Take predictions for those actually treated and minus counterfactual
  #        Then take counterfactually treated and deduct prediction for those actually in control
  CATE <- c(bart.out$yhat.train.mean[train$treat.het == 1] - bart.out$yhat.test.mean[test$treat.het == 0],
            bart.out$yhat.test.mean[test$treat.het == 1] - bart.out$yhat.train.mean[train$treat.het == 0])
  
  CATE_df <- data.frame(CATE = CATE)
  covars <- rbind(train[train$treat.het == 1,2], test[test$treat.het==1,2])
  
  CATE_df <- cbind(CATE_df,covars)
  CATE_df <- CATE_df[order(CATE_df$CATE),]
  CATE_df$id <- c(1:length(CATE))
  
  # Descriptive results reported in main text:
  mean(CATE_df$CATE)
  summary(CATE_df$CATE)
  
  # Proportion of CATEs that are negative:
  sum(CATE_df$CATE < 0)/nrow(CATE_df)
  sum(CATE_df$CATE < mean(CATE_df$CATE))/nrow(CATE_df)
  
  # FL 0 participant: prop. below mean
  sum(CATE_df$CATE < mean(CATE_df$CATE) & CATE_df$financial_lit_b == 0 )/sum(CATE_df$financial_lit_b == 0)
  
  
  # FL 1 participant: prop. below mean
  sum(CATE_df$CATE < mean(CATE_df$CATE) & CATE_df$financial_lit_b == 1 )/sum(CATE_df$financial_lit_b == 1)
  
  # FL2 participant: prop. below 
  sum(CATE_df$CATE < mean(CATE_df$CATE) & CATE_df$financial_lit_b == 2 )/sum(CATE_df$financial_lit_b == 2)
  
  
  
  # CATE Heterogeneity plot
  hist <- CATE_df
  
  effectsPlot <- ggplot(hist, aes(x=id, y = CATE)) +
    geom_line() +
    geom_hline(yintercept= 0, linetype="dashed", color="red") +
    geom_hline(yintercept = mean(hist$CATE), color = "blue") +
    labs(x="Individual",y = "CATE") +
    theme_minimal() +
    scale_x_continuous(limits = c(0,nrow(train)))
  #ggsave(effectsPlot, filename= "test.pdf")
  # Mode histogram 
  
  modePlot <- ggplot(hist, aes(x=id, fill=factor(financial_lit_b))) +
    geom_histogram(binwidth = 60,position="stack") +
    theme(legend.position="bottom") +
    labs(y = "Count", x = "Individual")+
    #scale_x_continuous(limits = c(0,nrow(train))) + 
    scale_fill_discrete(name = "")
  #scale_fill_manual(name="Mode", values=colours) +
  modePlot
  
  # Combine all plots into one chart
  FL_het <- ggarrange(effectsPlot, modePlot,
                      ncol = 1, nrow = 2, heights = c(2,2))
  FL_het
  ggsave(FL_het, filename = "Outputs/Correct_Response_het_financial_lit_online.pdf", path=path_github, device = "pdf", height = 8, width = 6, dpi = 300)
  
  
  
  ####################################
  ### Gender Heterogeneity
  ####################################
  library(BayesTree)
  set.seed(89536)
  
  # Data set up including calculating ability rank
  df.b <- df.f
  
  
  #set up  - divide into 1 treatment group and 0 control
  df.b$treat <- df.b$Treatments
  risk.vars<-c("Baseline" ,     "Perfil")
  df.b$treat.het<-ifelse(df.b$treat %in% risk.vars, 0, 1)
  df.b$gender <- ifelse(df.b$Gender == "F",1,0)
  df.b$private <- ifelse(df.b$Pension_Type == "Private",1,0) 
  df.b$health<-ifelse(df.b$HSist=="ISAPRE",  1, 0)
  
  # Define model variables incl. outcome as column 1
  vars <- c("correct_response", "treat.het",  "gender", "financial_lit_b","private")
  
  
  
  df.b <- df.b[,vars]
  df.b <- df.b[complete.cases(df.b),]
  
  # Separate outcome and training data
  y <- df.b$correct_response
  train <- df.b[,-1]
  
  # Gen. test data where those treated become untreated, for use in calculating ITT
  test <- train
  test$treat.het <- ifelse(test$treat.het == 1,0,ifelse(test$treat.het == 0,1,NA))
  
  # Run BART for predicted values of observed and synthetic observations
  bart.out <- bart(x.train = train, y.train = y, x.test = test)
  
  
  # Recover CATE estimates and format into dataframe
  # Logic: Take predictions for those actually treated and minus counterfactual
  #        Then take counterfactually treated and deduct prediction for those actually in control
  CATE <- c(bart.out$yhat.train.mean[train$treat.het == 1] - bart.out$yhat.test.mean[test$treat.het == 0],
            bart.out$yhat.test.mean[test$treat.het == 1] - bart.out$yhat.train.mean[train$treat.het == 0])
  
  CATE_df <- data.frame(CATE = CATE)
  covars <- rbind(train[train$treat.het == 1,2], test[test$treat.het==1,2])
  
  CATE_df <- cbind(CATE_df,covars)
  CATE_df <- CATE_df[order(CATE_df$CATE),]
  CATE_df$id <- c(1:length(CATE))
  
  # Descriptive results reported in main text:
  mean(CATE_df$CATE)
  summary(CATE_df$CATE)
  
  # Proportion of CATEs that are negative:
  sum(CATE_df$CATE < 0)/nrow(CATE_df)
  sum(CATE_df$CATE < mean(CATE_df$CATE))/nrow(CATE_df)
  
  # Female participant: prop. below mean
  sum(CATE_df$CATE < mean(CATE_df$CATE) & CATE_df$gender == 1 )/sum(CATE_df$gender == 1)
  
  
  # Male participant: prop. below mean
  sum(CATE_df$CATE < mean(CATE_df$CATE) & CATE_df$gender == 0 )/sum(CATE_df$gender == 0)
  
  
  
  # CATE Heterogeneity plot
  hist <- CATE_df
  
  effectsPlot <- ggplot(hist, aes(x=id, y = CATE)) +
    geom_line() +
    geom_hline(yintercept= 0, linetype="dashed", color="red") +
    geom_hline(yintercept = mean(hist$CATE), color = "blue") +
    labs(x="Individual",y = "CATE") +
    theme_minimal() +
    scale_x_continuous(limits = c(0,nrow(train)))
  #ggsave(effectsPlot, filename= "test.pdf")
  # Mode histogram 
  
  modePlot <- ggplot(hist, aes(x=id, fill=factor(gender))) +
    geom_histogram(binwidth = 60,position="stack") +
    theme(legend.position="bottom") +
    labs(y = "Count", x = "Individual")+
    #scale_x_continuous(limits = c(0,nrow(train))) + 
    scale_fill_discrete(name = "", )
  #scale_fill_manual(name="Mode", values=colours) +
  modePlot
  
  # Combine all plots into one chart
  Gen_het <- ggarrange(effectsPlot, modePlot,
                       ncol = 1, nrow = 2, heights = c(2,2))
  Gen_het
  
  ggsave(Gen_het, filename = "Outputs/Correct_Response_het_gender_online.pdf", path=path_github, device = "pdf", height = 8, width = 6, dpi = 300)
  
  
  
  ####################################
  ### Pension type Heterogeneity 
  ####################################
  library(BayesTree)
  set.seed(89536)
  
  # Data set up including calculating ability rank
  df.b <- df.f
  
  
  #set up  - divide into 1 treatment group and 0 control
  #set up  - divide into 1 treatment group and 0 control
  df.b$treat <- df.b$Treatments
  risk.vars<-c("Baseline" ,     "Perfil")
  df.b$treat.het<-ifelse(df.b$treat %in% risk.vars, 0, 1)
  df.b$gender <- ifelse(df.b$Gender == "F",1,0)
  df.b$private <- ifelse(df.b$Pension_Type == "Private",1,0) 
  df.b$health<-ifelse(df.b$HSist=="ISAPRE",  1, 0)
  
  # Define model variables incl. outcome as column 1
  vars <- c("correct_response", "treat.het","private", "gender", "financial_lit_b")
  
  
  
  df.b <- df.b[,vars]
  df.b <- df.b[complete.cases(df.b),]
  
  # Separate outcome and training data
  y <- df.b$correct_response
  train <- df.b[,-1]
  
  # Gen. test data where those treated become untreated, for use in calculating ITT
  test <- train
  test$treat.het <- ifelse(test$treat.het == 1,0,ifelse(test$treat.het == 0,1,NA))
  
  # Run BART for predicted values of observed and synthetic observations
  bart.out <- bart(x.train = train, y.train = y, x.test = test)
  
  
  # Recover CATE estimates and format into dataframe
  # Logic: Take predictions for those actually treated and minus counterfactual
  #        Then take counterfactually treated and deduct prediction for those actually in control
  CATE <- c(bart.out$yhat.train.mean[train$treat.het == 1] - bart.out$yhat.test.mean[test$treat.het == 0],
            bart.out$yhat.test.mean[test$treat.het == 1] - bart.out$yhat.train.mean[train$treat.het == 0])
  
  CATE_df <- data.frame(CATE = CATE)
  covars <- rbind(train[train$treat.het == 1,2], test[test$treat.het==1,2])
  
  CATE_df <- cbind(CATE_df,covars)
  CATE_df <- CATE_df[order(CATE_df$CATE),]
  CATE_df$id <- c(1:length(CATE))
  
  # Descriptive results reported in main text:
  mean(CATE_df$CATE)
  summary(CATE_df$CATE)
  
  # Proportion of CATEs that are negative:
  sum(CATE_df$CATE < 0)/nrow(CATE_df)
  sum(CATE_df$CATE < mean(CATE_df$CATE))/nrow(CATE_df)
  
  # Private participant: prop. below mean
  sum(CATE_df$CATE < mean(CATE_df$CATE) & CATE_df$private == 1 )/sum(CATE_df$private == 1)
  
  
  # Public participant: prop. below mean
  sum(CATE_df$CATE < mean(CATE_df$CATE) & CATE_df$private == 0 )/sum(CATE_df$private == 0)
  
  
  
  # CATE Heterogeneity plot
  hist <- CATE_df
  
  effectsPlot <- ggplot(hist, aes(x=id, y = CATE)) +
    geom_line() +
    geom_hline(yintercept= 0, linetype="dashed", color="red") +
    geom_hline(yintercept = mean(hist$CATE), color = "blue") +
    labs(x="Individual",y = "CATE") +
    theme_minimal() +
    scale_x_continuous(limits = c(0,nrow(train)))
  #ggsave(effectsPlot, filename= "test.pdf")
  # Mode histogram 
  
  modePlot <- ggplot(hist, aes(x=id, fill=factor(private))) +
    geom_histogram(binwidth = 60,position="stack") +
    theme(legend.position="bottom") +
    labs(y = "Count", x = "Individual")+
    #scale_x_continuous(limits = c(0,nrow(train))) + 
    scale_fill_discrete(name = "", )
  #scale_fill_manual(name="Mode", values=colours) +
  modePlot
  
  # Combine all plots into one chart
  Priv_het <- ggarrange(effectsPlot, modePlot,
                       ncol = 1, nrow = 2, heights = c(2,2))
  Priv_het
  
  ggsave(Priv_het, filename = "Outputs/Correct_Response_het_pensiontype_online.pdf", path=path_github, device = "pdf", height = 8, width = 6, dpi = 300)
  
  
    
    
  
  
  
  
###########################################3
#### Other dependent variables of interest  
############################################
  
    
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
   scale_y_continuous(breaks = seq(0, 1, 0.2), limits = c(0,1)) +
   theme(axis.title.x=element_blank()) +
   geom_hline(aes(yintercept = 0.2), linetype = 2, color = "gray") +
   geom_text(aes(y=0.2, label=paste0("0.2"), x=0.1), colour='gray', hjust=-0.1 , vjust = 1) +
   ylab("Opt Out")
  ggsave(paste0(path_github,"online/Outputs/OptOut.pdf"))
 
 
  #### Opt out pension type
  
  p <- df %>%
    group_by(Treatments, Pension_Type) %>% 
    summarise(out = sum(OptOut =="Out", na.rm=T),
              n = n()) %>%
    rowwise() %>%
    mutate(tst = list(broom::tidy(prop.test(out, n, conf.level = 0.95)))) %>%
    tidyr::unnest(tst)
  
  prop_test<-p[-9,]
  
  
  prop_test %>%  
    ggplot(aes(x=Treatments, y=estimate, color = Pension_Type, fill =Pension_Type)) +
    geom_bar(stat="identity", position= "dodge2") +
    geom_errorbar(aes(ymin=conf.low, ymax = conf.high ,  position="dodge2")) +
    theme_gppr() +
    ggsci::scale_fill_aaas() +
    scale_y_continuous(breaks = seq(0, 1, 0.2), limits = c(0,1)) +
    theme(axis.title.x=element_blank(),) +
    geom_hline(aes(yintercept = 0.2), linetype = 2, color = "gray") +
    geom_text(aes(y=0.2, label=paste0("0.2"), x=0.1), colour='gray', hjust=-0.1 , vjust = 1) +
    ylab("Opt Out") 
  
  #ggsave(paste0(path_github,"online/Outputs/OptOut_pension_type.pdf"))
  
  
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
 
 ggsave(paste0(path_github,"online/Outputs/usefulness.pdf"))
 
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
 
 ggsave(paste0(path_github,"online/Outputs/usefulness_het_pensiontype.pdf"))
 
 
 
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
 
 ggsave(paste0(path_github,"online/Outputs/responses.pdf"))
 
 
 
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
 ggsave(paste0(path_github,"online/Outputs/responses_het_gender.pdf"))
 
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
 ggsave(paste0(path_github,"online/Outputs/responses_het_pensiontype.pdf"))
 
 
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
   scale_colour_brewer(type = "seq", palette = "Dark2")+ 
   ggpubr::stat_compare_means(comparisons = my_comparisons, 
                              label = "p.signif", method = "wilcox.test")+
   labs(x ="", y = "Overconfidence", title = "")  + 
   theme(legend.position="none",
         #axis.title.y = element_text(vjust = +6),
         #plot.title = element_text(vjust = -1, size = 12),
         axis.ticks.x = element_blank())

  ggsave(paste0(path_github,"online/Outputs/overconfidence.pdf"))
 
  
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
  ggsave(paste0(path_github,"online/Outputs/overconfidence_het_gender.pdf"))
  
  #Promoter
  
