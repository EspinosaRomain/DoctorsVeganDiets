rm(list=ls())

#Import libraries
library(plyr)
library(pastecs)
library(factoextra)
library(dplyr)
library(psy)
library(ivreg)
library(stargazer)
#library(ivpack)
library(lmtest)
library(sandwich)
library(modelsummary)
library(AER)
library(censReg)
library(sampleSelection)
library("texreg")
library(stargazer)

# Prevent scientific notation for clarity, set to 0 for scientific notation
options(scipen=999)


#Set working Directory
# Romain
#setwd("~/Dropbox/Recherche/Expe Corps Médical/Expé Médecins/DataAnalysis")
#setwd("~/Library/CloudStorage/Dropbox/Recherche/Expe Corps Médical/Expé Médecins/DataAnalysis")
# Thibaut
setwd("/Users/ThibautArpinon_1/Dropbox/Expé Médecins/DataAnalysis/")

#Load user-written functions
source("Scripts/FunctionMatrixStatDesc.R")

#Set seed
set.seed(123)

#Import data
mydata=readRDS("Data/treatedData.RDS")

# Setting Bonferroni adjustment 
Bonf_adj <- 3

# Setting minimim statistical threshold Alpha
Alpha <- 0.05



#-----------------------------------------------------------------------------------------#
# Test exclusion rules with time spent on survey

#Time spent on the survey (in seconds)
mydata$timeSurvey=(mydata$`_DUREEXPAGE001` +mydata$`_DUREEXPAGE002` +mydata$`_DUREEXPAGE003` +mydata$`_DUREEXPAGE004` +mydata$`_DUREEXPAGE005` +mydata$`_DUREEXPAGE006` +mydata$`_DUREEXPAGE007`+mydata$`_DUREEXPAGE008` +mydata$`_DUREEXPAGE018` +mydata$`_DUREEXPAGE019` +mydata$`_DUREEXPAGE020` +mydata$`_DUREEXPAGE023` +mydata$`_DUREEXPAGE024`  +mydata$`_DUREEXPAGE025` +mydata$`_DUREEXPAGE026` +mydata$`_DUREEXPAGE027`)/1000
mydata= mydata[(mydata$timeSurvey>=90),]
stat.desc(mydata$timeSurvey)
# N=400: No observation excluded


# Sample description
listDemographics=c("liberal","female","urbanArea","ageBelow40","age40to49","age50to59","age60andAbove")
stat.desc(mydata[,listDemographics])["mean",]
stat.desc(mydata[mydata$treatment==0,listDemographics])["mean",]
stat.desc(mydata[mydata$treatment==1,listDemographics])["mean",]

# Veganism Disapproval Index (VDI)

# Compute mean 
mean(mydata[mydata$treatment==0,]$VDI)
mean(mydata[mydata$treatment==1,]$VDI)

# Tobit estimation recovering marginal effects
estVDI <- censReg(VDI ~ treatment, left=0, right=1, data = mydata)
margEffVDI <- margEff(estVDI) #Compute marginal effects
sVDI <- summary(margEffVDI)
sVDI

#H0^1: beta=>SESOI (with SESOI= -0.10)
ifelse((sVDI[1]+0.10)/sVDI[2]<qnorm(Alpha/Bonf_adj), "H0^1: Rejected", "H0^1: Not rejected")
#--> Successful campaign
#P-value of the test: pnorm((sVDI[1]+0.10)/sVDI[2])


#Export data for graph
vecCasPratique1=c("deconseille","carences","mauvaislongterme","sansprobleme","benefique","VDI")
matrixStatDes=cbind(matMeanSEWeighted(mydata[mydata$treatment==0,],vecCasPratique1,"weight"),
                    matMeanSEWeighted(mydata[mydata$treatment==1,],vecCasPratique1,"weight"))
matrixStatDes


# Proper Medical Practice Index (PMPI)

# Compute mean 
mean(mydata[mydata$treatment==0,]$PMPI)
mean(mydata[mydata$treatment==1,]$PMPI)

# Tobit estimation recovering marginal effects
estPMPI <- censReg(PMPI ~ treatment, left=0, right=1, data = mydata) #Tobit model
margEffPMPI <- margEff(estPMPI) #Compute marginal effects
sPMPI <- summary(margEffPMPI)
sPMPI

#H0^1: beta<=SESOI (with SESOI=0.125)
ifelse((sPMPI[1]-0.125)/sPMPI[2]>qnorm(1-Alpha/Bonf_adj), "H0^1: Rejected", "H0^1: Not rejected")

#H0^2: beta>=SESOI (with SESOI=0.125)
ifelse((sPMPI[1]-0.125)/sPMPI[2]<qnorm(Alpha/Bonf_adj), "H0^2: Rejected", "H0^2: Not rejected")

#H0^3: beta>=0
ifelse(sPMPI[1]/sPMPI[2]<qnorm(Alpha/Bonf_adj), "H0^3: Rejected", "H0^3: Not rejected")

#H0^4: beta<=0
ifelse(sPMPI[1]/sPMPI[2]>qnorm(1-Alpha/Bonf_adj), "H0^4: Rejected", "H0^4: Not rejected")
#--> Weakly successful campaign
#1-pnorm(sPMPI[1]/sPMPI[2])=0.0343

#Export data for graph
vecCasPratique2=c("T1","T2","T3","T4","T5","T6","T7","T8","PMPI")
matrixStatDes=cbind(matMeanSEWeighted(mydata[mydata$treatment==0,],vecCasPratique2,"weight"),
                    matMeanSEWeighted(mydata[mydata$treatment==1,],vecCasPratique2,"weight"))
matrixStatDes
mean(mydata[mydata$treatment==0,]$T4)
mean(mydata[mydata$treatment==1,]$T4)



#EXPLORATORY ANALYSES

#Impact on single elements for VDI
summary(lm(mydata$deconseille ~ treatment, data=mydata))$coef[2,4]
summary(lm(mydata$carences ~ treatment, data=mydata))$coef[2,4]
summary(lm(mydata$mauvaislongterme ~ treatment, data=mydata))$coef[2,4]
summary(lm(mydata$sansprobleme ~ treatment, data=mydata))$coef[2,4]
summary(lm(mydata$benefique ~ treatment, data=mydata))$coef[2,4]
    

#Impact on single elements for PMPI
summary(lm(mydata$T1 ~ treatment, data=mydata))$coef[2,4]
summary(lm(mydata$T2 ~ treatment, data=mydata))$coef[2,4]
summary(lm(mydata$T3 ~ treatment, data=mydata))$coef[2,4]
summary(lm(mydata$T4 ~ treatment, data=mydata))$coef[2,4]
summary(lm(mydata$T5 ~ treatment, data=mydata))$coef[2,4]
summary(lm(mydata$T6 ~ treatment, data=mydata))$coef[2,4]
summary(lm(mydata$T7 ~ treatment, data=mydata))$coef[2,4]
summary(lm(mydata$T8 ~ treatment, data=mydata))$coef[2,4]

#No Zinc PMPI
mydata$PMPINoZinc=(mydata$T1-(mydata$T2+mydata$T3+mydata$T5+mydata$T6+mydata$T7+mydata$T8)+6)/7
estPMPINoZinc <- censReg(PMPINoZinc ~ treatment, left=0, right=1, data = mydata) #Tobit model
margEffPMPINoZinc <- margEff(estPMPINoZinc) #Compute marginal effects
sPMPINoZinc <- summary(margEffPMPINoZinc)
sPMPINoZinc
#1-pnorm(sPMPINoZinc[1]/sPMPINoZinc[2])


#VPI
stat.desc(cbind(mydata$VPI,mydata[mydata$treatment==0,]$VPI,mydata[mydata$treatment==1,]$VPI))
table(mydata$VPI,mydata$treatment)/200
estVPI <- censReg(VPI ~ VDI, left=0, right=1, data = mydata) #Tobit model
margEffVPI <- margEff(estVPI) #Compute marginal effects
sVPI <- summary(margEffVPI)
sVPI

#VDI
lmVDI <- lm(VDI ~ treatment, data = mydata)
summary(lmVDI)
mydata$timeTreatmentAll=ifelse(is.na(mydata$timeTreatment),0,mydata$timeTreatment)
lmVDIinteracted <- lm(VDI ~ treatment + timeTreatmentAll, data = mydata)
summary(lmVDIinteracted)
lmVDItime <- lm(VDI ~ timeTreatmentAll, data = mydata) 
summary(lmVDItime)

#Generate variable
mydata$ageBelow49=mydata$age40to49+mydata$ageBelow40


#Heterogeneity analysis: VDI
VDI_model1 <- censReg(VDI ~ treatment, left=0, right=1, data = mydata) #Tobit model
summary(VDI_model1)
VDI_model2 <- censReg(VDI ~ treatment*female, left=0, right=1, data = mydata) #Tobit model
summary(VDI_model2)
VDI_model3 <- censReg(VDI ~ treatment*urbanArea, left=0, right=1, data = mydata) #Tobit model
summary(VDI_model3)
VDI_model4 <- censReg(VDI ~ treatment*ageBelow49, left=0, right=1, data = mydata) #Tobit model
summary(VDI_model4)
VDI_model5 <- censReg(VDI ~ treatment*female + treatment*urbanArea + treatment*ageBelow49, left=0, right=1, data = mydata)
summary(VDI_model5)
# No heterogeneous effect of treatment on VDI

# Display regression output
screenreg(list(VDI_model1, VDI_model2, VDI_model3, VDI_model4, VDI_model5), digits = 3, include.bic = FALSE, include.aic = FALSE, include.adjrs = FALSE)

# Display regression output as LaTeX code
texreg(list(VDI_model1, VDI_model2, VDI_model3, VDI_model4, VDI_model5), digits = 3)

#Heterogeneity analysis:  PMPI
PMPI_model1 <- censReg(PMPI ~ treatment, left=0, right=1, data = mydata) #Tobit model
summary(PMPI_model1)
PMPI_model2 <- censReg(PMPI ~ treatment*female, left=0, right=1, data = mydata) #Tobit model
summary(PMPI_model2)
PMPI_model3 <- censReg(PMPI ~ treatment*urbanArea, left=0, right=1, data = mydata) #Tobit model
summary(PMPI_model3)
PMPI_model4 <- censReg(PMPI ~ treatment*ageBelow49, left=0, right=1, data = mydata) #Tobit model
summary(PMPI_model4)
PMPI_model5 <- censReg(PMPI ~ treatment*female + treatment*urbanArea + treatment*ageBelow49, left=0, right=1, data = mydata)
summary(PMPI_model5)
# No heterogeneous effect of treatment on PMPI

# Display regression output
screenreg(list(PMPI_model1, PMPI_model2, PMPI_model3, PMPI_model4, PMPI_model5), digits = 3, include.bic = FALSE, include.aic = FALSE, include.adjrs = FALSE)

# Display regression output as LaTeX code
texreg(list(PMPI_model1, PMPI_model2, PMPI_model3, PMPI_model4, PMPI_model5), digits = 3)



# Info for table in appendix

# Non-Weighted
Table3=matrix(nrow=16,ncol=7,data=NA)
listVar=c("VDI","deconseille","carences","mauvaislongterme","sansprobleme","benefique","PMPI",
          "T1","T2","T3","T4","T5","T6","T7","T8","PMPINoZinc")
i=1
for(k in listVar){
  Table3[i,1]=round(stat.desc(mydata[mydata$treatment==0, k])["mean",],3)
  Table3[i,2]=round(stat.desc(mydata[mydata$treatment==0, k])["std.dev",],3)
  Table3[i,3]=round(stat.desc(mydata[mydata$treatment==1, k])["mean",],3)
  Table3[i,4]=round(stat.desc(mydata[mydata$treatment==1, k])["std.dev",],3)
  mydata$tmpVar=mydata[[k]]
  if(i<8 | i==16){
    estTobit <- censReg(tmpVar ~ treatment, left=0, right=1, data = mydata)
    margEffTobit <- margEff(estTobit) #Compute marginal effects
    sum_var <- summary(margEffTobit)
    Table3[i,5]=round(sum_var[1],3)
    Table3[i,6]=round(sum_var[2],3)
    Table3[i,7]=round(sum_var[4],3)
  }
  if(i>7 & i<16){
    sum_var=summary(lm(tmpVar ~ treatment, data=mydata))$coefficients
    Table3[i,5]=round(sum_var[2,1],3)
    Table3[i,6]=round(sum_var[2,2],3)
    Table3[i,7]=round(sum_var[2,4],3)
  }
  i=i+1
}
Table3

# PMPI no zinc
mydata$PMPINoZinc=(mydata$T1-(mydata$T2+mydata$T3+mydata$T5+mydata$T6+mydata$T7+mydata$T8)+6)/7
stat.desc(mydata[mydata$treatment==0, "PMPINoZinc"])["mean",]
stat.desc(mydata[mydata$treatment==0, "PMPINoZinc"])["std.dev",]

stat.desc(mydata[mydata$treatment==1, "PMPINoZinc"])["mean",]
stat.desc(mydata[mydata$treatment==1, "PMPINoZinc"])["std.dev",]

estPMPINoZinc <- censReg(PMPINoZinc ~ treatment, left=0, right=1, data = mydata) #Tobit model
margEffPMPINoZinc <- margEff(estPMPINoZinc) #Compute marginal effects
sPMPINoZinc <- summary(margEffPMPINoZinc)
sPMPINoZinc
#1-pnorm(sPMPINoZinc[1]/sPMPINoZinc[2])

#Exploring attention effect
stat.desc(mydata$timeTreatment)
stat.desc(mydata$timeTreatment)["median"]/60 
subdata=mydata[mydata$treatment==0 | (mydata$treatment==1 & mydata$timeTreatment>=90),]

estVDI_subdata <- censReg(VDI ~ treatment, left=0, right=1, data = subdata)
margEffVDI_subdata <- margEff(estVDI_subdata) #Compute marginal effects
sVDI_subdata <- summary(margEffVDI_subdata)
sVDI_subdata

estPMPI_subdata <- censReg(PMPI ~ treatment, left=0, right=1, data = subdata) #Tobit model
margEffPMPI_subdata <- margEff(estPMPI_subdata) #Compute marginal effects
sPMPI_subdata <- summary(margEffPMPI_subdata)
sPMPI_subdata


#DISCUSSION

#Discussion on opinions about plant-based diets
dataControl=mydata[mydata$treatment==0,]
table(dataControl$deconseille)
listItems=cPa("deconseille","carences","sansprobleme","mauvaislongterme","benefique")
listNewVar=c("deconseilleAgree","carencesAgree","sansproblemeAgree","mauvaislongtermeAgree","benefiqueAgree")
for(i in 1:length(listItems)){
  dataControl[[listNewVar[i]]]=ifelse(dataControl[[listItems[i]]]<=0.3,"Disagree",ifelse(dataControl[[listItems[i]]]>0.6,"Agree","Neither"))
}
matAverage=matrix(data=NA,ncol=3,nrow=5)
m=1
for(k in listNewVar){
  matAverage[m,1]=sum(ifelse(dataControl[[k]]=="Disagree",dataControl$weight,0))/sum(dataControl$weight)*100
  matAverage[m,2]=sum(ifelse(dataControl[[k]]=="Neither",dataControl$weight,0))/sum(dataControl$weight)*100
  matAverage[m,3]=sum(ifelse(dataControl[[k]]=="Agree",dataControl$weight,0))/sum(dataControl$weight)*100
  m=m+1
}
matAverage=round(matAverage,1)
matAverage


#Discussion on opinions about knowledge about diets
vecNutrition=c("knowledgeNutrition","knowledgePlantBased","knowledgePBOthers","opinionPB","opinionPBOthers")
listNewVar=c("knowledgeNutritionAgree","knowledgePlantBasedAgree","knowledgePBOthersAgree","opinionPBAgree","opinionPBOthersAgree")
for(i in 1:length(vecNutrition)){
  dataControl[[listNewVar[i]]]=ifelse(dataControl[[vecNutrition[i]]]<=3,"Disagree",ifelse(dataControl[[vecNutrition[i]]]>6,"Agree","Neither"))
}
matAverage=matrix(data=NA,ncol=3,nrow=5)
m=1
for(k in listNewVar){
  matAverage[m,1]=sum(ifelse(dataControl[[k]]=="Disagree",dataControl$weight,0))/sum(dataControl$weight)*100
  matAverage[m,2]=sum(ifelse(dataControl[[k]]=="Neither",dataControl$weight,0))/sum(dataControl$weight)*100
  matAverage[m,3]=sum(ifelse(dataControl[[k]]=="Agree",dataControl$weight,0))/sum(dataControl$weight)*100
  m=m+1
}
matAverage=round(matAverage,1)
matAverage


#Discussion about neg consequences 
vecAvis=c("hideSymptoms","changeDoctor","alternativeMedecine")
listNewVar=c("hideSymptomsAgree","changeDoctorAgree","alternativeMedecineAgree")
for(i in 1:length(vecNutrition)){
  dataControl[[listNewVar[i]]]=ifelse(dataControl[[vecAvis[i]]]<=3,"Disagree",ifelse(dataControl[[vecAvis[i]]]>6,"Agree","Neither"))
}
matAverage=matrix(data=NA,ncol=3,nrow=3)
m=1
for(k in listNewVar){
  matAverage[m,1]=sum(ifelse(dataControl[[k]]=="Disagree",dataControl$weight,0))/sum(dataControl$weight)*100
  matAverage[m,2]=sum(ifelse(dataControl[[k]]=="Neither",dataControl$weight,0))/sum(dataControl$weight)*100
  matAverage[m,3]=sum(ifelse(dataControl[[k]]=="Agree",dataControl$weight,0))/sum(dataControl$weight)*100
  m=m+1
}
matAverage=round(matAverage,1)
matAverage


#Discussion recommending less meat for certain pathologies
vecPathologies=c("adviceDyslipidemia","adviceCardiopathy","adviceOverweight","adviceDiabetes","adviceHypertension")
listNewVar=c("adviceDyslipidemiaYes","adviceCardiopathyYes","adviceOverweightYes","adviceDiabetesYes","adviceHypertensionYes")

for(i in 1:length(vecPathologies)){
  dataControl[[listNewVar[i]]]=ifelse(dataControl[[vecPathologies[i]]]<=3,"Disagree",ifelse(dataControl[[vecPathologies[i]]]>6,"Agree","Neither"))
}
matAverage=matrix(data=NA,ncol=3,nrow=5)
m=1
for(k in listNewVar){
  matAverage[m,1]=sum(ifelse(dataControl[[k]]=="Disagree",dataControl$weight,0))/sum(dataControl$weight)*100
  matAverage[m,2]=sum(ifelse(dataControl[[k]]=="Neither",dataControl$weight,0))/sum(dataControl$weight)*100
  matAverage[m,3]=sum(ifelse(dataControl[[k]]=="Agree",dataControl$weight,0))/sum(dataControl$weight)*100
  m=m+1
}
matAverage=round(matAverage,1)
matAverage


# Cohen's D VDI 
(mean(mydata[mydata$treatment==0,]$VDI)-mean(mydata[mydata$treatment==1,]$VDI))/sd(mydata$VDI)
# 0.714

# Cohen's D PMPI with Zinc
abs((mean(mydata[mydata$treatment==0,]$PMPI)-mean(mydata[mydata$treatment==1,]$PMPI))/sd(mydata$PMPI))
# 0.215

# Cohen's D PMPI without Zinc
abs((mean(mydata[mydata$treatment==0,]$PMPINoZinc)-mean(mydata[mydata$treatment==1,]$PMPINoZinc))/sd(mydata$PMPINoZinc))
# 0.351








