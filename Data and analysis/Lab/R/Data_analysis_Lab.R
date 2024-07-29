################################
#### Data analysis
#### author: Denise Laroze
################################
library(stargazer)
library(MASS)
library(broom)
library(ggpubr)
library(naniar)
library(nnet)
library(lmtest)
library(sandwich)


#rm(list=ls())
#path_github <- "C:/Users/Denise Laroze/Documents/GitHub/Pensions Website Design/Data and analysis/Lab/"
#path_datos<-"C:/Users/Denise Laroze/Documents/GitHub/Pensions Website Design/Data and analysis/Lab/Lab Data/Surveys and websites/"


df <- readRDS(paste0(path_datos, "lab_data.rds"))
df.f<-df[!is.na(df$correct_response),]

df.en <- readRDS(paste0(path_datos, "encuestas_clean.rds"))



##########################
### Attrition   
##########################


#Descriptive statistics
table(df$attrition) ### Referenced in the manuscript page NNNNNN
prop.table(table(as.numeric(as.factor(df$attrition[df$Treatments=="Baseline"]))))
prop.table(table(as.numeric(as.factor(df$attrition[df$Treatments=="Perfil"]))))
prop.table(table(as.numeric(as.factor(df$attrition[df$Treatments=="Video"]))))
prop.table(table(as.numeric(as.factor(df$attrition[df$Treatments=="VideoPerfil"]))))


table(df$encuesta) ### Of those that responded the questionnaire, what options they took B = effort, C = no effort 



# model estimations
lw<-glm(as.factor(attrition) ~ Profile + Video + Profile_Video , data = df, family = "binomial")
lw2<-glm(as.factor(attrition) ~ Profile + Video + Profile_Video + Age + Gender + educ_eng + private_health, data = df, family = "binomial")

eff<-glm(as.factor(effort) ~ Profile + Video + Profile_Video , data = df, family = "binomial")
eff2<-glm(as.factor(effort) ~ Profile + Video + Profile_Video + Age + Gender + educ_eng + private_health, data = df, family = "binomial")

stargazer(lw, lw2, eff, eff2)

# Attrition is not correlated with treatments    

stargazer(lw, lw2, eff, eff2, out=paste0(path_github,"Lab/Outputs/attrition_lab.tex"), type="latex",
          covariate.labels = c("Profile$\\_i$", "Video$\\_j$", "Profile$\\_i$xVideo$\\_j$", "Age", "Male", "High School", "University or technical college", 
                               "Private healthcare", "Constant"), 
          dep.var.labels = c("Finish Tutorial", "Opt into Comprehension Test"), # keep.stat=c("n", "ll"),
          dep.var.caption = "", star.cutoffs = c(0.05, 0.01, 0.001), notes.align = "l", table.placement = "H",
          label="tbl:attrition_lab",
          title = "Columns 1 and 2 present logit models on the likelihood of completing the tutorial vs droping out at the website stage. Columns 3-4 are logit models on whether a 
          person chose to respond to the comprehension questions or not - for the lab sample", no.space=TRUE)



################################
# summary of sample statistics on page NNNNNNN
################################

df.trd<-df[!is.na(df$Progress.y),] ### people that reached the third stage of the experiment

#age
summary(2022-as.numeric(df.trd$Birth))

#gender
prop.table(table(df.trd$Gender))


#education
prop.table(table(df.trd$educ_eng))

#Financial literacy
prop.table(table(df.trd$financial_lit_b))


#Private (top 20% income) vs public healthcare
prop.table(table(df.trd$HSist))





tbl<-table(df.trd$Pension_Type)
tbl[1]/(tbl[1]+tbl[2])
tbl[2]/(tbl[1]+tbl[2])


# Earnings
summary(df.trd$total_reward)
summary(df.trd$total_reward[df.trd$Pension_Type=="Private"])
summary(df.trd$total_reward[df.trd$Pension_Type=="Public"])


#Time

df.trd$StartDate <- dmy_hm(df.trd$StartDate.x)
df.trd$EndDate <- ymd_hms(df.trd$EndDate.y)

# Calculate the time difference in seconds and minutes
df.trd$DiffInSeconds <- as.duration(interval(df.trd$StartDate, df.trd$EndDate)) / dseconds(1)
df.trd$DiffInMinutes <- df.trd$DiffInSeconds / 60

mean(df.trd$DiffInSeconds)
mean(df.trd$DiffInMinutes)

filtered_df <- subset(df.trd, DiffInMinutes < 120)

# Calculate the mean of DiffInMinutes for the filtered observations
mean_diff_in_minutes <- mean(filtered_df$DiffInMinutes, na.rm = TRUE)
mean_diff_in_minutes

rm(filtered_df)

######################################################
####### Balance Tests
########################################################

### Balance summary statistics full sample

# Ensure financial_lit_b is a factor
df$financial_lit_b <- as.factor(df$financial_lit_b)

names<-c( "Variable",  "Treatment", "N")
# Summarize the data
s1 <- df %>%
  group_by( Gender, Treatments) %>%
  summarise(
    Gender_count = length(unique(uemail)),
  )
names(s1)<-names
print(s1)

s2 <- df %>%
  group_by( financial_lit_b, Treatments) %>%
  summarise(
    Fin_lit = length(unique(uemail)),
  )
names(s2)<-names
s2<-s2[-c(13:16),]
print(s2)

s3 <- df %>%
  group_by( educ_eng, Treatments) %>%
  summarise(
    Educ = length(unique(uemail)),
  )
names(s3)<-names

s4 <- df %>%
  group_by( private_health, Treatments) %>%
  summarise(
    health = length(unique(uemail)),
  )
names(s4)<-names

tbl<-rbind(s1, s2, s3, s4)

tbl_wide <- tbl %>%
  pivot_wider(names_from = Treatment, values_from = N)

tbl_wide$Total<-rowSums(tbl_wide[2:5])

s5 <- df %>%
  group_by(Treatments) %>%
  summarise(
    Age_mean = round(mean(Age, na.rm = TRUE),2),
    Age_sd = round(sd(Age, na.rm = TRUE),2),
  )%>%
  pivot_longer(cols = c(Age_mean, Age_sd), names_to = "Variable", values_to = "Value") %>%
  pivot_wider(names_from = Treatments, values_from = Value)

print(s4)


tbl<-rbind(tbl_wide, s5)

tbl$PC_Baseline<-round(tbl$Baseline/tbl$Total, 4)*100
tbl$PC_T.1<-round(tbl$Perfil/tbl$Total, 4)*100
tbl$PC_T.2<-round(tbl$Video/tbl$Total, 4)*100
tbl$PC_T.3<-round(tbl$VideoPerfil/tbl$Total, 4)*100

# Change the row names
df.s <- as.data.frame(tbl)

# Set the row names
values <- c(
  "Female", "Male", "Low Fin. Lit.", "Mid Fin. Lit.", "High Fin. Lit.",
  "Post-graduate degree", "Primary or high-school degree", "University degree", 
  "Public Health or other", "Private healthcare", "Age (mean)", "Age (sd)"
)

df.s[[1]]<-values

# Set the column names
colnames(df.s) <- c("Variable","Baseline", "Profile", "Video", "Profile and Video", "Total", 
                    "% Baseline", "% Profile", "% Video", "% Profile and Video")

xt<-xtable(df.s)
print(xt, type="latex", file=paste0(path_github,"/Outputs/balance_numbers_lab.tex"), floating=FALSE, include.rownames=FALSE)

rm(tbl_wide, s1, s2, s3, s4, s5, tbl2, df.s)


### Balance summary statistics private

df.pv<-df[df$Pension_Type=="Private",]

# Ensure financial_lit_b is a factor
df.pv$financial_lit_b <- as.factor(df.pv$financial_lit_b)

names<-c( "Variable",  "Treatment", "N")
# Summarize the data
s1 <- df.pv %>%
  group_by( Gender, Treatments) %>%
  summarise(
    Gender_count = length(unique(uemail)),
  )
names(s1)<-names
print(s1)

s2 <- df.pv %>%
  group_by( financial_lit_b, Treatments) %>%
  summarise(
    Fin_lit = length(unique(uemail)),
  )
names(s2)<-names
s2<-s2[-c(13:16),]
print(s2)

s3 <- df.pv %>%
  group_by( educ_eng, Treatments) %>%
  summarise(
    Educ = length(unique(uemail)),
  )
names(s3)<-names

s4 <- df.pv %>%
  group_by( private_health, Treatments) %>%
  summarise(
    health = length(unique(uemail)),
  )
names(s4)<-names

tbl<-rbind(s1, s2, s3, s4)

tbl_wide <- tbl %>%
  pivot_wider(names_from = Treatment, values_from = N)

tbl_wide$Total<-rowSums(tbl_wide[2:5])

s5 <- df.pv %>%
  group_by(Treatments) %>%
  summarise(
    Age_mean = round(mean(Age, na.rm = TRUE),2),
    Age_sd = round(sd(Age, na.rm = TRUE),2),
  )%>%
  pivot_longer(cols = c(Age_mean, Age_sd), names_to = "Variable", values_to = "Value") %>%
  pivot_wider(names_from = Treatments, values_from = Value)

print(s4)


tbl<-rbind(tbl_wide, s5)

tbl$PC_Baseline<-round(tbl$Baseline/tbl$Total, 4)*100
tbl$PC_T.1<-round(tbl$Perfil/tbl$Total, 4)*100
tbl$PC_T.2<-round(tbl$Video/tbl$Total, 4)*100
tbl$PC_T.3<-round(tbl$VideoPerfil/tbl$Total, 4)*100

# Change the row names
df.s <- as.data.frame(tbl)

# Set the row names
values <- c(
  "Female", "Male", "Low Fin. Lit.", "Mid Fin. Lit.", "High Fin. Lit.",
  "Post-graduate degree", "Primary or high-school degree", "University degree", 
  "Public Health or other", "Private healthcare", "Age (mean)", "Age (sd)"
)

df.s[[1]]<-values

# Set the column names
colnames(df.s) <- c("Variable","Baseline", "Profile", "Video", "Profile and Video", "Total", 
                    "% Baseline", "% Profile", "% Video", "% Profile and Video")



xt<-xtable(df.s)
print(xt, type="latex", file=paste0(path_github,"/Outputs/balance_numbers_lab_private.tex"), floating=FALSE, include.rownames=FALSE)


rm(tbl_wide, s1, s2, s3, s4, s5, tbl2, df.s, df.pv)



### Balance summary statistics Public

df.pp<-df[df$Pension_Type=="Public",]



# Ensure financial_lit_b is a factor
df.pp$financial_lit_b <- as.factor(df.pp$financial_lit_b)

names<-c( "Variable",  "Treatment", "N")
# Summarize the data
s1 <- df.pp %>%
  group_by( Gender, Treatments) %>%
  summarise(
    Gender_count = length(unique(uemail)),
  )
names(s1)<-names
print(s1)

s2 <- df.pp %>%
  group_by( financial_lit_b, Treatments) %>%
  summarise(
    Fin_lit = length(unique(uemail)),
  )
names(s2)<-names
s2<-s2[-c(13:16),]
print(s2)

s3 <- df.pp %>%
  group_by( educ_eng, Treatments) %>%
  summarise(
    Educ = length(unique(uemail)),
  )
names(s3)<-names

s4 <- df.pp %>%
  group_by( private_health, Treatments) %>%
  summarise(
    health = length(unique(uemail)),
  )
names(s4)<-names

tbl<-rbind(s1, s2, s3, s4)

tbl_wide <- tbl %>%
  pivot_wider(names_from = Treatment, values_from = N)

tbl_wide$Total<-rowSums(tbl_wide[2:5])

s5 <- df.pp %>%
  group_by(Treatments) %>%
  summarise(
    Age_mean = round(mean(Age, na.rm = TRUE),2),
    Age_sd = round(sd(Age, na.rm = TRUE),2),
  )%>%
  pivot_longer(cols = c(Age_mean, Age_sd), names_to = "Variable", values_to = "Value") %>%
  pivot_wider(names_from = Treatments, values_from = Value)

print(s4)


tbl<-rbind(tbl_wide, s5)

tbl$PC_Baseline<-round(tbl$Baseline/tbl$Total, 4)*100
tbl$PC_T.1<-round(tbl$Perfil/tbl$Total, 4)*100
tbl$PC_T.2<-round(tbl$Video/tbl$Total, 4)*100
tbl$PC_T.3<-round(tbl$VideoPerfil/tbl$Total, 4)*100

# Change the row names
df.s <- as.data.frame(tbl)

# Set the row names
values <- c(
  "Female", "Male", "Low Fin. Lit.", "Mid Fin. Lit.", "High Fin. Lit.",
  "Post-graduate degree", "Primary or high-school degree", "University degree", 
  "Public Health or other", "Private healthcare", "Age (mean)", "Age (sd)"
)

df.s[[1]]<-values

# Set the column names
colnames(df.s) <- c("Variable","Baseline", "Profile", "Video", "Profile and Video", "Total", 
                    "% Baseline", "% Profile", "% Video", "% Profile and Video")



xt<-xtable(df.s)
print(xt, type="latex", file=paste0(path_github,"/Outputs/balance_numbers_lab_public.tex"), floating=FALSE, include.rownames=FALSE)


rm(tbl_wide, s1, s2, s3, s4, s5, df.s, df.pp)




##### with multinomial logit



multinom_model2 <- multinom(Treatments ~ Age + Gender + private_health  + as.factor(financial_lit_b), data = df)


stargazer(multinom_model2)

n<-nrow(multinom_model2$fitted.values)

stargazer(multinom_model2, out=paste0(path_github,"Lab/Outputs/balance_lab.tex"), type="latex",
          covariate.labels = c("Age", "Male","Private healthcare", "Mid Fin. Lit.", "High Fin. Lit.", "Constant"), 
          dep.var.labels = c("T.Profile", "T.Video", "T.Video and Profile"),  keep.stat=c("n", "ll"),
          add.lines=list(c("Observations", n,  "", "")),
          dep.var.caption = "", star.cutoffs = c(0.05, 0.01, 0.001), notes.align = "l", table.placement = "H",
          label="tbl:balance_lab",
          title = "Multinomial logit models on Treatment assignment by socio-demoraphic characteristics for the lab sample - balance test", no.space=TRUE)


#Balance test for Public benefits subgroup in supplementary material
multinom_model2 <- multinom(Treatments ~ Age + Gender + private_health  + as.factor(financial_lit_b), data = df[df$Pension_Type=="Public",])
stargazer(multinom_model2)
n<-nrow(multinom_model2$fitted.values)

stargazer(multinom_model2, out=paste0(path_github,"Lab/Outputs/balance_public_lab.tex"), type="latex",
          covariate.labels = c("Age", "Male", "Private healthcare", "Mid Fin. Lit.", "High Fin. Lit.", "Constant"), 
          dep.var.labels = c("T.Profile", "T.Video", "T.Video and Profile"), # keep.stat=c("n", "ll"),
          add.lines=list(c("Observations", n,  "", "")),
          dep.var.caption = "", star.cutoffs = c(0.05, 0.01, 0.001), notes.align = "l", table.placement = "H",
          label="tbl:balance_public_lab",
          title = "Multinomial logit models on Treatment assignment by socio-demoraphic characteristics for the sub-group that observed the Public Benefits information in the Lab - balance test", no.space=TRUE)

table(df.f$private_health[df.f$Pension_Type=="Public"])


#Balance test for Private pensions subgroup in supplementary material

multinom_model2 <- multinom(Treatments ~ Age + Gender + private_health + as.factor(financial_lit_b), data = df[df$Pension_Type=="Private",])
n<-nrow(multinom_model2$fitted.values)

stargazer(multinom_model2)

stargazer(multinom_model2, out=paste0(path_github,"Lab/Outputs/balance_private_lab.tex"), type="latex",
          covariate.labels = c("Age", "Male", "Private healthcare", "Mid Fin. Lit.", "High Fin. Lit.", "Constant"), 
          dep.var.labels = c("T.Profile", "T.Video", "T.Video and Profile"), # keep.stat=c("n", "ll"),
          add.lines=list(c("Observations", n,  "", "")),
          dep.var.caption = "", star.cutoffs = c(0.05, 0.01, 0.001), notes.align = "l", table.placement = "H",
          label="tbl:balance_private_lab",
          title = "Multinomial logit models on Treatment assignment by socio-demoraphic characteristics for the sub-group that observed the Private Pensions information in the Lab  - balance test", no.space=TRUE)


##################################################
###### Correct responses Lab
###################################################  
#### Descriptives
### Correct responses
prop.table(table(df.en$obliga))
prop.table(table(df.en$inicio))
prop.table(table(df.en$elegir))
prop.table(table(df.en$sirve.scmp))
prop.table(table(df.en$scmp.twice))
prop.table(table(df.en$asesor))
prop.table(table(df.en$propiedad))

prop.table(table(df.en$ncomp1))
prop.table(table(df.en$ncomp2))
prop.table(table(df.en$ncomp3))
prop.table(table(df.en$ncomp4))
prop.table(table(df.en$ncomp5))
prop.table(table(df.en$ncomp6))
prop.table(table(df.en$ncomp7))

###


### Correct Response 

  lm_CR <- lm(correct_response ~ Profile + Video + Profile_Video + as.factor(financial_lit_b) , 
              data = df) 
  
  lm_CR2<-coeftest(lm_CR, vcov = vcovHC(lm_CR, type = 'HC0'))
  
  
  lm_CR_pv <- lm(correct_response ~ Profile + Video + Profile_Video + as.factor(financial_lit_b), 
                 data = df[df$Pension_Type=="Public",]) 
  lm_CR_pv2<-coeftest(lm_CR_pv, vcov = vcovHC(lm_CR_pv, type = 'HC0'))
  
  
  lm_CR_pp <- lm(correct_response ~ Profile + Video + Profile_Video + as.factor(financial_lit_b), 
                 data = df[df$Pension_Type=="Private",]) 
  lm_CR_pp2<-coeftest(lm_CR_pp, vcov = vcovHC(lm_CR_pp, type = 'HC0'))
  
  
  lm_CR_F <- lm(correct_response ~ Profile + Video + Profile_Video + as.factor(financial_lit_b), 
                data = df[df$Gender=="F",]) 
  lm_CR_F2<-coeftest(lm_CR_F, vcov = vcovHC(lm_CR_F, type = 'HC0'))
  
  
  lm_CR_M <- lm(correct_response ~ Profile + Video + Profile_Video + as.factor(financial_lit_b), 
                data = df[df$Gender=="M",]) 
  lm_CR_M2<-coeftest(lm_CR_M, vcov = vcovHC(lm_CR_M, type = 'HC0'))
  
  
  stargazer(lm_CR2, lm_CR_pv2, lm_CR_pp2, lm_CR_F2, lm_CR_M2)
  
  stargazer(lm_CR2, lm_CR_pv2, lm_CR_pp2, lm_CR_F2, lm_CR_M2, out=paste0(path_github,"Lab/Outputs/main_results_correct_response_lab.tex"), type="latex",
            covariate.labels = c("Profile$\\_i$", "Video$\\_j$", "Profile$\\_i$xVideo$\\_j$", "Mid Fin. Lit.", "High Fin. Lit.", "Constant"), 
            dep.var.labels = c("Number of correct responses"), # keep.stat=c("n", "ll"),
            column.labels = c("Full Lab", "Private", "Public", "Female", "Male"),
            dep.var.caption = "", star.cutoffs = c(0.05, 0.01, 0.001), notes.align = "l", table.placement = "H",
            add.lines=list(c("Observations", nobs(lm_CR),nobs(lm_CR_pv), nobs(lm_CR_pp), nobs(lm_CR_F), nobs(lm_CR_M)),
                           c("$R\\^2$", round(summary(lm_CR)$r.squared, 3), round(summary(lm_CR_pv)$r.squared, 3), round(summary(lm_CR_pp)$r.squared, 3), 
                             round(summary(lm_CR_F)$r.squared, 3) , round(summary(lm_CR_M)$r.squared, 3)),
                           c("$Adjusted R\\^2$", round(summary(lm_CR)$adj.r.squared, 3), round(summary(lm_CR_pv)$adj.r.squared, 3), 
                             round(summary(lm_CR_pp)$adj.r.squared, 3), round(summary(lm_CR_F)$adj.r.squared, 3) , round(summary(lm_CR_M)$adj.r.squared,3))),
            label="tbl:Main_results_correct_response_lab",
            title = "The table presents OLS models on the number of correct responses, for different sub-groups of the sample. The models are estimated
            using heteroscedasticity robust standard errors.", no.space=TRUE)
  
  
# Correct response no controls  Appendix table
  lm_CR <- lm(correct_response ~ Profile + Video + Profile_Video, 
              data = df) 
  lm_CR2<-coeftest(lm_CR, vcov = vcovHC(lm_CR, type = 'HC0'))
  
  
  lm_CR_pv <- lm(correct_response ~ Profile + Video + Profile_Video, 
                 data = df[df$Pension_Type=="Public",]) 
  lm_CR_pv2<-coeftest(lm_CR_pv, vcov = vcovHC(lm_CR_pv, type = 'HC0'))
  
  
  lm_CR_pp <- lm(correct_response ~ Profile + Video + Profile_Video, 
                 data = df[df$Pension_Type=="Private",]) 
  lm_CR_pp2<-coeftest(lm_CR_pp, vcov = vcovHC(lm_CR_pp, type = 'HC0'))
  
  
  lm_CR_F <- lm(correct_response ~ Profile + Video + Profile_Video, 
                data = df[df$Gender=="F",]) 
  lm_CR_F2<-coeftest(lm_CR_F, vcov = vcovHC(lm_CR_F, type = 'HC0'))
  
  lm_CR_M <- lm(correct_response ~ Profile + Video + Profile_Video, 
                data = df[df$Gender=="M",]) 
  lm_CR_M2<-coeftest(lm_CR_M, vcov = vcovHC(lm_CR_M, type = 'HC0'))
  

  stargazer(lm_CR2, lm_CR_pv2, lm_CR_pp2, lm_CR_F2, lm_CR_M2)
  
  stargazer(lm_CR2, lm_CR_pv2, lm_CR_pp2, lm_CR_F2, lm_CR_M2, out=paste0(path_github,"Lab/Outputs/main_results_correct_response_no_controls_lab.tex"), type="latex",
            covariate.labels = c("Profile$\\_i$", "Video$\\_j$", "Profile$\\_i$xVideo$\\_j$", "Constant"), 
            dep.var.labels = c("Number of correct responses"), # keep.stat=c("n", "ll"),
            column.labels = c("Full Lab", "Private", "Public", "Female", "Male"),
            dep.var.caption = "", star.cutoffs = c(0.05, 0.01, 0.001), notes.align = "l", table.placement = "H",
            add.lines=list(c("Observations", nobs(lm_CR),nobs(lm_CR_pv), nobs(lm_CR_pp), nobs(lm_CR_F), nobs(lm_CR_M)),
                           c("$R\\^2$", round(summary(lm_CR)$r.squared, 3), round(summary(lm_CR_pv)$r.squared, 3), round(summary(lm_CR_pp)$r.squared, 3), 
                             round(summary(lm_CR_F)$r.squared, 3) , round(summary(lm_CR_M)$r.squared, 3)),
                           c("$Adjusted R\\^2$", round(summary(lm_CR)$adj.r.squared, 3), round(summary(lm_CR_pv)$adj.r.squared, 3), 
                             round(summary(lm_CR_pp)$adj.r.squared, 3), round(summary(lm_CR_F)$adj.r.squared, 3) , round(summary(lm_CR_M)$adj.r.squared,3))),
            label="tbl:Main_results_CR_no_control_lab",
            title = "The table presents OLS models on the number of correct responses, for different sub-groups of the Lab sample. The models are estimated
            using heteroscedasticity robust standard errors.", no.space=TRUE)

  ###############################################
  ### Self-reported measures of the tutorial  
  ###############################################
  
  
  df$nps_question<-as.numeric(df$Recomendar)
  
  lm_nps<- lm(nps_question ~ Profile + Video + Profile_Video + as.factor(financial_lit_b), data = df) 
  
  lm_nps2<-coeftest(lm_nps, vcov = vcovHC(lm_nps, type = 'HC0'))
  
  
  stargazer(lm_nps2, out=paste0(path_github,"/Outputs/nps_models_lab.tex"), type="latex",
            covariate.labels = c("Profile$\\_i$", "Video$\\_j$", "Profile$\\_i$xVideo$\\_j$", "Mid Fin. Lit.", "High Fin. Lit.", "Constant"), 
            dep.var.labels = c("NPS Lab"), # keep.stat=c("n", "ll"),7
            add.lines=list(c("Observations", nobs(lm_nps)),
                           c("R\\^2", round(summary(lm_nps)$r.squared, 3)),
                           c("Adjusted R\\^2", round(summary(lm_nps)$adj.r.squared, 3))),
            dep.var.caption = "", star.cutoffs = c(0.05, 0.01, 0.001), notes.align = "l", table.placement = "H",
            label="tbl:nps_reg",
            title = "The table presents OLS models on a 0-10 recomendations score, with 10 as the highest value, estimated with 
            heteroscedasticity robust standard errors.", no.space=TRUE)
  
  
  
  
  #Estimate NPS by treatment
  library(marketr)
  library(xtable)
  library(dplyr)
  
  nps_question<-as.numeric(df.f$Recomendar)
  nps_group<-df.f$Treatments
  nps_date<-as.Date("2023-07-06")

  #Mean and variance
  d <- data.frame(nps_question, nps_date, nps_group)
  d<-d[!is.na(d$nps_question),]
  tbl1<-d %>%
        group_by(nps_group) %>%
        summarise(mean_nps = mean(nps_question, na.rm=T),
                  sd_nps= sd(nps_question, na.rm=T))

    
  tbl<-nps_calc(d, nps_group)
  
  tbl2<-left_join(tbl1, tbl, by = "nps_group")  
  
  colnames(tbl2) <- c("Treatment", "Mean_Promoter", "s.d._Promoter", "Net_Promoter_Score", "Number_of_responses")
  
  
  
  xt<-xtable(tbl2)
  print(xt, type="latex", file=paste0(path_github,"Lab/Outputs/nps_table_Lab.tex"), 
        floating=FALSE, include.rownames=FALSE)
  
  rm(tbl, tbl1, tbl2)
  
  
  #### Regressions on self-reported measures of positive experience during the website navigation

  
  df$easy<-recode(df$Facilidad, "2" = 2, "3" = 3, "4" = 4, "Muy fácil  5" = 5, "Muy fácil\n5"= 5, "Nada fácil  1" = 1, "Nada fácil\n1" = 1 )
  
  easy<- lm(easy ~ Profile + Video + Profile_Video + as.factor(financial_lit_b), df)
  easy2<-coeftest(easy, vcov = vcovHC(easy, type = 'HC0'))
  
  
  useful<- lm(as.numeric(InfoUtil_1) ~ Profile + Video + Profile_Video + as.factor(financial_lit_b), df)
  useful2<-coeftest(useful, vcov = vcovHC(useful, type = 'HC0'))
  
  nps<-lm(as.numeric(Recomendar) ~ Profile + Video + Profile_Video + as.factor(financial_lit_b), df)
  nps2<-coeftest(nps, vcov = vcovHC(nps, type = 'HC0'))
  
  stargazer(easy, useful, nps)
  
  stargazer(easy2, useful2, nps2, out=paste0(path_github,"Lab/Outputs/self_reported_lab.tex"), type="latex",
            covariate.labels = c("Profile$\\_i$", "Video$\\_j$", "Profile$\\_i$xVideo$\\_j$", "Mid Fin. Lit.", "High Fin. Lit.", "Constant"), 
            column.labels = c("Easy", "Useful", "Recomend"), # keep.stat=c("n", "ll"),
            dep.var.caption = "", star.cutoffs = c(0.05, 0.01, 0.001), notes.align = "l", table.placement = "H",
            add.lines=list(c("Observations", nobs(easy),nobs(useful), nobs(nps)),
                           c("$R\\^2$", round(summary(easy)$r.squared, 3), round(summary(useful)$r.squared, 3), round(summary(nps)$r.squared, 3)),
                           c("$Adjusted R\\^2$", round(summary(easy)$adj.r.squared, 3), round(summary(useful)$adj.r.squared, 3), round(summary(nps)$adj.r.squared, 3))),
            label="tbl:self_reported_lab",
            title = "The table presents OLS models on lab self-reported measures of evaluating the questions: 
            ``How easy was it to find the information you were looking for?'' (1-5 scale), 
            ``Is the information available on the website sufficient to make a decision?'' (0-10 scale) and
            ``Considering your experience on this website, how likely are you to recommend it to a family member or friend?'' (0-10 scale).
            Model estimations include heteroscedasticity robust standard errors.
          ", no.space=TRUE)
  
  
  
  ####################################
  ### Financial literacy Heterogeneity
  ####################################
  
  library(BayesTree)
  set.seed(89536)
  
  # Data set up including calculating ability rank
  df.b <- df.f
  
  
  #set up  - divide into 1 treatment group and 0 control
  df.b$treat <- df.b$Video
  df.b$treat.het<-ifelse(df.b$treat=="Video", 1, 0)
  df.b$gender <- ifelse(df.b$Gender == "M",1,0)
  df.b$private <- ifelse(df.b$Pension_Type == "Private",1,0) 
  
  # Define model variables incl. outcome as column 1
  vars <- c("correct_response", "treat.het", "financial_lit_b", "private")
  
  
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
  hist$FL<-NA
  hist$FL<-ifelse(hist$financial_lit_b==0, "Low Fin. Lit.", ifelse(hist$financial_lit_b==1, "Mid Fin. Lit.", "High Fin. Lit.") )
  hist$FL<- factor(hist$FL, levels=c('Low Fin. Lit.', 'Mid Fin. Lit.', 'High Fin. Lit.'))
  
  
  effectsPlot <- ggplot(hist, aes(x=id, y = CATE)) +
    geom_line() +
    geom_hline(yintercept= 0, linetype="dashed", color="red") +
    geom_hline(yintercept = mean(hist$CATE), color = "blue") +
    labs(x="Individual",y = "CATE") +
    theme_minimal() +
    scale_x_continuous(limits = c(0,nrow(train)))
  #ggsave(effectsPlot, filename= "test.pdf")
  # Mode histogram 
  
  modePlot <- ggplot(hist, aes(x=id, fill=factor(FL))) +
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
  ggsave(FL_het, filename = "Lab/Outputs/Figure_A4b.pdf", path=path_github, device = "pdf", height = 8, width = 6, dpi = 300)
  
  
  
  ####################################
  ### Gender Heterogeneity
  ####################################
  library(BayesTree)
  set.seed(89536)
  # Data set up including calculating ability rank
  df.b <- df.f
  
  
  #set up  - divide into 1 treatment group and 0 control
  df.b$treat <- df.b$Video
  df.b$treat.het<-ifelse(df.b$treat=="Video", 1, 0)
  df.b$gender <- ifelse(df.b$Gender == "M",1,0)
  df.b$private <- ifelse(df.b$Pension_Type == "Private",1,0) 
  
  
  # Define model variables incl. outcome as column 1
  vars <- c("correct_response", "treat.het",  "gender", "private")
  
  
  
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
  
  # Male participant: prop. below mean
  sum(CATE_df$CATE < mean(CATE_df$CATE) & CATE_df$gender == 1 )/sum(CATE_df$gender == 1)
  
  
  # Female participant: prop. below mean
  sum(CATE_df$CATE < mean(CATE_df$CATE) & CATE_df$gender == 0 )/sum(CATE_df$gender == 0)
  
  
  
  # CATE Heterogeneity plot
  # CATE Heterogeneity plot
  hist <- CATE_df
  
  hist$g<-NA
  hist$g<-ifelse(hist$gender==0, "Women", "Men")
  hist$g<- factor(hist$g, levels=c('Women', 'Men'))
  
  
  
  effectsPlot <- ggplot(hist, aes(x=id, y = CATE)) +
    geom_line() +
    geom_hline(yintercept= 0, linetype="dashed", color="red") +
    geom_hline(yintercept = mean(hist$CATE), color = "blue") +
    labs(x="Individual",y = "CATE") +
    theme_minimal() +
    scale_x_continuous(limits = c(0,nrow(train)))
  #ggsave(effectsPlot, filename= "test.pdf")
  # Mode histogram 
  
  modePlot <- ggplot(hist, aes(x=id, fill=factor(g))) +
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
  
  ggsave(Gen_het, filename = "Lab/Outputs/Figure_A3b.pdf", path=path_github, device = "pdf", height = 8, width = 6, dpi = 300)
  
  
  
  ####################################
  ### Pension type Heterogeneity 
  ####################################
  library(BayesTree)
  set.seed(53698)
  
  # Data set up including calculating ability rank
  df.b <- df.f
  table(df.f$private_health)
  
  #set up  - divide into 1 treatment group and 0 control
  df.b$treat <- df.b$Video
  df.b$treat.het<-ifelse(df.b$treat=="Video", 1, 0)
  df.b$gender <- ifelse(df.b$Gender == "M",1,0)
  df.b$private <- ifelse(df.b$Pension_Type == "Private",1,0) 
  df.b$Educ<- ifelse(df.b$educ_eng == "Primary or high-school degree",0, 1 ) # With or without a graduate degree
  
  
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
  
  hist$p<-NA
  hist$p<-ifelse(hist$private==1, "Private Pensions", "Public Benefits")
  hist$p<- factor(hist$p, levels=c('Public Benefits', 'Private Pensions'))
  
  effectsPlot <- ggplot(hist, aes(x=id, y = CATE)) +
    geom_line() +
    geom_hline(yintercept= 0, linetype="dashed", color="red") +
    geom_hline(yintercept = mean(hist$CATE), color = "blue") +
    labs(x="Individual",y = "CATE") +
    theme_minimal() +
    scale_x_continuous(limits = c(0,nrow(train)))
  #ggsave(effectsPlot, filename= "test.pdf")
  # Mode histogram 
  
  modePlot <- ggplot(hist, aes(x=id, fill=factor(p))) +
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
  
  ggsave(Priv_het, filename = "Lab/Outputs/Figure_A2b.pdf", path=path_github, device = "pdf", height = 8, width = 6, dpi = 300)
  
 
####################################################
#### Other possible dependent variables of interest  
####################################################

  
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
  
