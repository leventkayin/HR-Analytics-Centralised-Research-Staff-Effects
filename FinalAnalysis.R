## Levent Kayın - HR Analytics Final

setwd("/Users/leventkayin/Desktop/School/HR Analytics/Final/")

finaldata = read.csv2("FINALdata.csv", sep = ",")
str(finaldata)

dataprep = function(df) {
  
  x2 = subset(df, df$dMtg == "No Engagement")
  x1 = subset(df, df$dMtg == "Engagement")
  
  f1 = data.frame(year = c(17,18),margmean = c(mean(x1$dept17),mean(x1$dept18)),stddev = c(sd(x1$dept17),sd(x1$dept18)),type ="dept",dMtg = "Engagement")
  f2 = data.frame(year = c(17,18),margmean = c(mean(x1$preaward17),mean(x1$preaward18)),stddev = c(sd(x1$preaward17),sd(x1$preaward18)),type ="preaward",dMtg = "Engagement")
  f3 = data.frame(year = c(17,18),margmean = c(mean(x1$postaward17),mean(x1$postaward18)),stddev = c(sd(x1$postaward17),sd(x1$postaward18)),type ="postaward",dMtg = "Engagement")
  f4 = data.frame(year = c(17,18),margmean = c(mean(x1$inst17),mean(x1$inst18)),stddev = c(sd(x1$inst17),sd(x1$inst18)),type ="inst",dMtg = "Engagement")
  
  f5 = data.frame(year = c(17,18),margmean = c(mean(x2$dept17),mean(x2$dept18)),stddev = c(sd(x2$dept17),sd(x2$dept18)),type ="dept",dMtg = "No Engagement")
  f6 = data.frame(year = c(17,18),margmean = c(mean(x2$preaward17),mean(x2$preaward18)),stddev = c(sd(x2$preaward17),sd(x2$preaward18)),type ="preaward",dMtg = "No Engagement")
  f7 = data.frame(year = c(17,18),margmean = c(mean(x2$postaward17),mean(x2$postaward18)),stddev = c(sd(x2$postaward17),sd(x2$postaward18)),type ="postaward",dMtg = "No Engagement")
  f8 = data.frame(year = c(17,18),margmean = c(mean(x2$inst17),mean(x2$inst18)),stddev = c(sd(x2$inst17),sd(x2$inst18)),type ="inst",dMtg = "No Engagement")
  
  
  f9 = data.frame(year = c(17,18),margmean = c(mean(x1$avg17),mean(x1$avg18)),stddev = c(sd(x1$avg17),sd(x1$avg18)),type ="average",dMtg = "Engagement")
  f10 = data.frame(year = c(17,18),margmean = c(mean(x2$avg17),mean(x2$avg18)),stddev = c(sd(x2$avg17),sd(x2$avg18)),type ="average",dMtg = "No Engagement")
  
  endframe= rbind(f1,f2,f3,f4,f9,f5,f6,f7,f8,f10)
  
  return(endframe)
}

### Data manipulation - Setting variables in correct formats for modeling ----
library(plyr)

finaldata$avg17 = apply(finaldata[,c(2:5)],1,mean)
finaldata$avg18 = apply(finaldata[,c(6:9)],1,mean)

finaldata$delt_dept = finaldata$dept18-finaldata$dept17
finaldata$delt_preaward = finaldata$preaward18-finaldata$preaward17
finaldata$delt_postaward = finaldata$postaward18-finaldata$postaward17
finaldata$delt_inst = finaldata$inst18-finaldata$inst17

finaldata$awardtotal = as.numeric(as.character(finaldata$awardtotal))
finaldata$proposaltotal = as.numeric(as.character(finaldata$proposaltotal))
finaldata$age = as.numeric(as.character(finaldata$age))
finaldata$yos = as.numeric(as.character(finaldata$yos))

finaldata$chair = revalue(as.factor(finaldata$chair),c("0" = "Not Chair","1" = "Chair"))

finaldata$rasunit = revalue(as.factor(finaldata$rasunit),c("1" = "ABOSS",
                                                     "2" = "Basic Science",
                                                     "3" = "CAPS",
                                                     "4" = "Cancer and Imaging",
                                                     "5" = "Dept of Medicine",
                                                     "6" = "Hospital and Speciality Services",
                                                     "7" = "Pediatrics",
                                                     "8" = "Public Health and Nursing",
                                                     "9" = "Yerkes"))

finaldata$award = revalue(as.factor(finaldata$award),c("0" = "No Awards","1" = "Recieved Awards"))

finaldata$proposal = revalue(as.factor(finaldata$proposal),c("0" = "No Submitted Proposals","1" = "Submitted Proposals"))

finaldata$doctortype = revalue(as.factor(finaldata$doctortype),c("0" = "No Doctorate",
                                                                 "1" = "Ph. D.",
                                                                 "2" = "M.D.",
                                                                 "3" = "M.D., Ph. D."))

finaldata$female = revalue(as.factor(finaldata$female),c("0" = "Male","1" = "Female"))

finaldata$ethnicgrp = revalue(as.factor(finaldata$ethnicgrp),c("0" = "White",
                                                               "1" = "Black",
                                                               "2" = "Hispanic",
                                                               "3" = "Asian",
                                                               "4" = "American Indian"))

finaldata$white = revalue(as.factor(finaldata$white),c("0" = "Not White","1" = "White"))

finaldata$asian = revalue(as.factor(finaldata$asian),c("0" = "Not Asian","1" = "Asian"))

finaldata$dMtg = revalue(as.factor(finaldata$dMtg),c("0" = "No Engagement","1" = "Engagement"))

finaldata$tenuretrack = revalue(as.factor(finaldata$tenuretrack),c("0" = "No Tenure","1" = "Tenure Track"))

str(finaldata)
colnames(finaldata)

modeldata = finaldata

modeldata$preaward17 = factor(modeldata$preaward17,ordered = TRUE)
modeldata$postaward17 = factor(modeldata$postaward17,ordered = TRUE)
modeldata$dept17 = factor(modeldata$dept17,ordered = TRUE)
modeldata$inst17 = factor(modeldata$inst17,ordered = TRUE)

modeldata$preaward18 = factor(modeldata$preaward18,ordered = TRUE)
modeldata$postaward18 = factor(modeldata$postaward18,ordered = TRUE)
modeldata$dept18 = factor(modeldata$dept18,ordered = TRUE)
modeldata$inst18 = factor(modeldata$inst18,ordered = TRUE)

modeldata$delt_dept = factor(modeldata$delt_dept,ordered = TRUE)
modeldata$delt_preaward = factor(modeldata$delt_preaward,ordered = TRUE)
modeldata$delt_postaward = factor(modeldata$delt_postaward,ordered = TRUE)
modeldata$delt_inst = factor(modeldata$delt_inst,ordered = TRUE)
modeldata$delt_avg = factor(round(modeldata$avg18-modeldata$avg17),ordered = TRUE)

str(modeldata)


### Q1 -------


library(ggplot2)

summ_alldata = dataprep(finaldata)

prof_plot1 = qplot(data = summ_alldata, x = year, y =margmean,geom = c("point","line"),color = type,ylab = "Mean Rating",
      main = "Profile Plot for 4 margmeans",facets = .~dMtg,ylim = c(1,3))+theme_minimal()

prof_plot1
#### Q2 -----

library(cowplot)

## male Female----

# Plots

females = subset(finaldata,finaldata$female == "Female")

male   = subset(finaldata,finaldata$female == "Male")

indv_gender_plot =qplot(data = finaldata, x = id,y = (avg18-avg17),col = I("darkgreen"),main ="Average rating change by employee",facets = female~.) + geom_hline(yintercept = mean(females$avg18-females$avg17), col = "darkred")
indv_gender_plot

summ_female = dataprep(females)

genderplot1 = qplot(data = summ_female, x = year, y =margmean,geom = c("point","line"),color = type,ylab = "Mean Rating",
                   main = "Female",facets = .~dMtg,ylim = c(1,3))+theme_minimal()+theme(legend.position = "bottom")


summ_male = dataprep(male)

genderplot2 = qplot(data = summ_male, x = year, y =margmean,geom = c("point","line"),color = type,ylab = "Mean Rating",
                      main = "Male",facets = .~dMtg,ylim = c(1,3))+theme_minimal()+theme(legend.position = "bottom")

genderplots = plot_grid(genderplot1,genderplot2)

genderplots

# Stats
library(emmeans)

gender_emmeans17 = emmeans(glm(avg17~female+dMtg,data=modeldata),c("female","dMtg"))
gender_emmeans18 =emmeans(glm(avg18~female+dMtg,data=modeldata),c("female","dMtg"))
gendermanova = manova(cbind(preaward17,postaward17,dept17,inst17,avg17) ~ female + dMtg,data = modeldata)

gender_emmeans17
gender_emmeans18
summary.aov(gendermanova)
arrange(summ_female,year,dMtg,type)
arrange(summ_male,year,dMtg,type)

## Tenure No Tenure ----

tenure = subset(finaldata,finaldata$tenuretrack == "Tenure Track")
notenure   = subset(finaldata,finaldata$tenuretrack == "No Tenure")

summ_tenure = dataprep(tenure)

tenureplot = qplot(data = summ_tenure, x = year, y =margmean,geom = c("point","line"),color = type,ylab = "Mean Rating",
                      main = "Tenure",facets = .~dMtg,ylim = c(1,3))+theme_minimal()+theme(legend.position = "bottom")

summ_notenure = dataprep(male)

notenureplot = qplot(data = summ_notenure, x = year, y =margmean,geom = c("point","line"),color = type,ylab = "Mean Rating",
                      main = "No Tenure",facets = .~dMtg,ylim = c(1,3))+theme_minimal()+theme(legend.position = "bottom")

tenureplots = plot_grid(tenureplot,notenureplot)

tenureplots

indv_tenure_plot =qplot(data = finaldata, x = id,y = (avg18-avg17),col = I("darkgreen"),main ="Average rating change by employee",facets = tenuretrack~.) + geom_hline(yintercept = mean(females$avg18-females$avg17), col = "darkred")
indv_tenure_plot


# Stats

tenure_emmeans17 = emmeans(glm(avg17~tenuretrack+dMtg,data=modeldata),c("tenuretrack","dMtg"))
tenure_emmeans18 =emmeans(glm(avg18~tenuretrack+dMtg,data=modeldata),c("tenuretrack","dMtg"))
tenuremanova = manova(cbind(preaward17,postaward17,dept17,inst17,avg17) ~ tenuretrack + dMtg,data = modeldata)

tenure_emmeans17
tenure_emmeans18
summary.aov(tenuremanova)
arrange(summ_tenure,year,dMtg,type)

# Overall Plot


individual_plot =qplot(data = finaldata, x = id,y = (avg18-avg17),col = I("darkgreen"),main ="Average rating change by employee",facets = tenuretrack~female) + geom_hline(yintercept = mean(females$avg18-females$avg17), col = "darkred")
individual_plot

### Q3 -----

library(MASS)

avg_model = polr(delt_avg~female+white+asian+yos+chair+tenuretrack+dMtg,data = modeldata)
summary(avg_model)

inst_model = polr(delt_inst~female+white+asian+yos+chair+tenuretrack+dMtg,data = modeldata)
summary(inst_model)
 

dept_model = polr(delt_dept~female+white+asian+yos+chair+tenuretrack+dMtg,data = modeldata)
summary(dept_model)
 

preaward_model = polr(delt_preaward~female+white+asian+yos+chair+tenuretrack+dMtg,data = modeldata)
summary(preaward_model)

postaward_model = polr(delt_postaward~female+white+asian+yos+chair+tenuretrack+dMtg,data = modeldata)
summary(postaward_model)

### Q4 ------

proposal_model = glm(proposal~rasunit+female+white+asian+yos+chair+tenuretrack+doctortype,data = modeldata,family = "binomial")
summary(proposal_model)

awards_model = glm(awardnum~rasunit+female+white+asian+yos+chair+tenuretrack+doctortype,data = modeldata)
summary(awards_model)

tot_proposals_model = glm(proposaltotal~rasunit+female+white+asian+yos+chair+tenuretrack+doctortype,data = modeldata,subset = proposal =="Submitted Proposals")
summary(tot_proposals_model)

tot_awards_model = glm(awardtotal~rasunit+female+white+asian+yos+chair+tenuretrack+doctortype,data = modeldata)
summary(awards_model)


