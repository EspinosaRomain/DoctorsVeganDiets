#Import libraries
library(plyr)
library(pastecs)
library(factoextra)
library(dplyr)
library(psy)
library(ivreg)
library(stargazer)
library(ivpack)
library(lmtest)
library(sandwich)
library(modelsummary)
library(AER)
library(censReg)
library(sampleSelection)
library("texreg")
library(stargazer)

# Prevent scientific notation for clarity, set to 0 for scientific notation
#options(scipen=999)


#Set working Directory
# Romain
setwd("~/Dropbox/Recherche/Expe Corps Médical/Expé Médecins/DataAnalysis")
# Thibaut
#setwd("/Users/ThibautArpinon_1/Dropbox/Expé Médecins/DataAnalysis/")

#Load user-written functions
source("Scripts/FunctionMatrixStatDesc.R")

#Set seed
set.seed(123)

#Import data
mydata=readRDS("Data/treatedData.RDS")

# Setting Bonferroni adjustment 
Bonf_adj <- 2

# Setting minimim statistical threshold Alpha
Alpha <- 0.05


#-----------------------------------------------------------------------------------------#
# Exclusion rules

#Exclude participants which spent less than 90 seconds looking at the info campaign
stat.desc(mydata[mydata$treatment==1,]$timeTreatment)
count(mydata[mydata$treatment==1 & mydata$timeTreatment<90,]) 
# --> 21 participants spent less than 90 seconds 

# Delete the 21 observations
mydata=mydata[mydata$treatment==0 | (mydata$treatment==1 & mydata$timeTreatment>=90),]
# --> 379 participants left
#-----------------------------------------------------------------------------------------#

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
ifelse(sPMPI[1]/sPMPI[2]<qnorm(Alpha/Bonf_adj), "H0^2: Rejected", "H0^2: Not rejected")

#H0^4: beta<=0
ifelse(sPMPI[1]/sPMPI[2]>qnorm(1-Alpha/Bonf_adj), "H0^2: Rejected", "H0^2: Not rejected")
#--> Weakly successful campaign
#1-pnorm(sPMPI[1]/sPMPI[2])=0.0152

#Export data for graph
vecCasPratique2=c("T1","T2","T3","T4","T5","T6","T7","T8","PMPI")
matrixStatDes=cbind(matMeanSEWeighted(mydata[mydata$treatment==0,],vecCasPratique2,"weight"),
                    matMeanSEWeighted(mydata[mydata$treatment==1,],vecCasPratique2,"weight"))
matrixStatDes
mean(mydata[mydata$treatment==0,]$T4)
mean(mydata[mydata$treatment==1,]$T4)


#EXPLORATORY ANALYSES

mydata$ageBelow49=mydata$age40to49+mydata$ageBelow40

# VDI
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



# PMPI
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


# VPI
summary(censReg(VPI ~ treatment*female + treatment*urbanArea + treatment*ageBelow49, left=0, right=2, data = mydata)) # No treatment effect on VPI 





#DISCUSSION

#Discussion on opinions about plant-based diets
dataControl=mydata[mydata$treatment==0,]
table(dataControl$deconseille)
listItems=c("deconseille","carences","sansprobleme","mauvaislongterme","benefique")
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




