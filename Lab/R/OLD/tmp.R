View(df[, c( "Treatments", "OptOut" , "total_reward" , "contesta" )])

tmp<-df

df$OptOut = ifelse(data$terminaron == "si" & data$contesta == "B" , "In", "Out")
table(df$terminaron)

table(df$contesta)
summary(df$contesta)


View(df.f[, c( "Treatments", "PlanJubi", "Age" , "Gender" , "pb_d", "financial_lit_b" )])

Treatments+ Age + Gender  + pb_d + as.factor(financial_lit_b)