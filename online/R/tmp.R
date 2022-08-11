



### Correct answers for Financial Literacy Questions 

#table(df$QMath1)
df$QMath1_correct<-ifelse(df$QMath1=="Más de $125.000.000", 1, 
                                  ifelse(is.na(df$QMath1), NA, 0))
#table(df$QMath1, df$QMath1_correct)

#table(df$QMath2)
df$QMath2_correct<-ifelse(df$QMath2=="Nunca se terminaría de pagar el crédito", 1, 
                                  ifelse(is.na(df$QMath1), NA, 0))
#table(df$QMath2, df$QMath2_correct)

#df$Qmath3num<-parse_number(df$Qmath3)
df$QMath3_correct<-ifelse(df$QMath3 == 5000,1, 
                                  ifelse(df$QMath3==5, 1,
                                         ifelse(is.na(df$QMath1), NA, 0)))
#table(df$QMath3, df$QMath3_correct)


tmp<-df[, c("QMath1_correct", "QMath2_correct", "QMath3_correct") ]

#tmp[is.na(tmp)] <- 0
tmp$financial_lit<-rowSums(tmp)


df$financial_lit<-tmp$financial_lit
rm(tmp)

table(df$financial_lit)

120/(120+236+80+22)
236/(120+236+80+22)
80/(120+236+80+22)
22/(120+236+80+22)


####################33



###############33
### Correlate with correct response

cor.test(df$financial_lit_b, df$correct_response)
cor.test(df$QMath3_correct, df$correct_response)
cor.test(df$financial_lit, df$correct_response)










