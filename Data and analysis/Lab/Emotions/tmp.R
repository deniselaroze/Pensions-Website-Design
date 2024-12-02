
library(stargazer)

lm1<-lm(ValenceX2 ~ t_perfil + t_video + Profile_Video , data = df)

lm2<-lm(ArousalX2 ~ t_perfil + t_video + Profile_Video , data = df)

lm3<-lm(Happy2 ~ t_perfil + t_video + Profile_Video , data = df)

lm4<-lm(Sad2 ~ t_perfil + t_video + Profile_Video , data = df)
lm5<-lm(Angry2 ~ t_perfil + t_video + Profile_Video , data = df)

lm6<-lm(Surprised2 ~ t_perfil + t_video + Profile_Video , data = df)

lm7<-lm(Scared2 ~ t_perfil + t_video + Profile_Video , data = df)

lm8<-lm(Disgusted2 ~ t_perfil + t_video + Profile_Video , data = df)

#stargazer(lm1, lm2, lm3, lm4, lm5, lm6, lm7, lm8)

stargazer(lm1, lm2, lm3, lm4, lm5, lm6, lm7, lm8, out=paste0(path_datos,"emotions.html"), type="html")



          #covariate.labels = c("Profile$\\_i$", "Video$\\_j$", "Profile$\\_i$xVideo$\\_j$","Constant"), 
          #dep.var.labels = c("Valence","Arousal"),  keep.stat=c("n", "rsq", "f"),
          #dep.var.caption = "", star.cutoffs = c(0.1, 0.05, 0.01, 0.001), notes.align = "l", table.placement = "H",
          #label="tbl:arousal",
          #title = " Linear regressions on the level of valence and arousal experienced by participants during the website navigation", no.space=TRUE)

