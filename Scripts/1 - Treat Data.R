#Import libraries
library(plyr)
library(dplyr)
library(factoextra)
library(readxl)

#Set working Directory
# Romain 
#setwd("~/Dropbox/Recherche/Expe Corps Médical/Expé Médecins/DataAnalysis")
# Thibaut
setwd("/Users/ThibautArpinon_1/Dropbox/Expé Médecins/DataAnalysis/")


#Load user-written functions
source("Scripts/userFunctions.R")

#Set seed
set.seed(123)

#Import data
mydata=read_xlsx("RawData/DataExpDoctors.xlsx", sheet=2)

#Generate useful variables
mydata$treatment=ifelse(mydata$ECHANTILLON=="Contrôle",0,1)
mydata=mydata %>% rename(weight=REDRESSEMENT)
mydata$liberal=ifelse(mydata$S1=="Exclusivement en libéral",1,0)
mydata$female=ifelse(mydata$S2=="Une femme",1,0)
mydata$ageBelow40=ifelse(mydata$S3=="Moins de 40 ans",1,0)
mydata$age40to49=ifelse(mydata$S3=="40 - 49 ans",1,0)
mydata$age50to59=ifelse(mydata$S3=="50 - 59 ans",1,0)
mydata$age60andAbove=ifelse(mydata$S3=="60 ans et plus",1,0)
mydata$urbanArea=ifelse(mydata$AGGLOV=="> 100 000 habitants" | mydata$AGGLOV=="Agglomération parisienne",1,0)


#Time spent on the information campaign (in seconds)
mydata$timeTreatment=(mydata$`_DUREEXPAGE009`+mydata$`_DUREEXPAGE010`+mydata$`_DUREEXPAGE011`+mydata$`_DUREEXPAGE012`+
  mydata$`_DUREEXPAGE013`+mydata$`_DUREEXPAGE014`+mydata$`_DUREEXPAGE015`+mydata$`_DUREEXPAGE016`+mydata$`_DUREEXPAGE017`)/1000

#Generate first dependent variable: VDI
mydata$VDI=(mydata$`Q1(1)`+mydata$`Q1(2)`-mydata$`Q1(3)`+mydata$`Q1(4)`-mydata$`Q1(5)`+2)/5
mydata=mydata %>% rename(deconseille=`Q1(1)`,
                           carences=`Q1(2)`,
                         sansprobleme=`Q1(3)`,
                         mauvaislongterme=`Q1(4)`,
                         benefique=`Q1(5)`)

#Generate second dependent variable: 
mydata$T1=mydata$T2=mydata$T3=mydata$T4=mydata$T5=mydata$T6=mydata$T7=mydata$T8=mydata$T9=mydata$T10=0
vecTestVar=c("Q3_1","Q3_2","Q3_3","Q3_4","Q3_5","Q3_6","Q3_7","Q3_8","Q3_9","Q3_10","Q3_11")
for(i in vecTestVar){
  mydata$T1=ifelse(!is.na(mydata[[i]]) & mydata[[i]]=="Acide méthylmalonique urinaire",1,mydata$T1)
  mydata$T2=ifelse(!is.na(mydata[[i]]) & mydata[[i]]=="Bilan phospho-calcique",1,mydata$T2)
  mydata$T3=ifelse(!is.na(mydata[[i]]) & mydata[[i]]=="Vitamine B9 (acide folique)",1,mydata$T3)
  mydata$T4=ifelse(!is.na(mydata[[i]]) & mydata[[i]]=="Zinc sérique",1,mydata$T4)
  mydata$T5=ifelse(!is.na(mydata[[i]]) & mydata[[i]]=="Albuminémie",1,mydata$T5)
  mydata$T6=ifelse(!is.na(mydata[[i]]) & mydata[[i]]=="Ferritinémie",1,mydata$T6)
  mydata$T7=ifelse(!is.na(mydata[[i]]) & mydata[[i]]=="25 (OH)-vitamine D",1,mydata$T7)
  mydata$T8=ifelse(!is.na(mydata[[i]]) & mydata[[i]]=="TSH",1,mydata$T8)
  mydata$T9=ifelse(!is.na(mydata[[i]]) & mydata[[i]]=="Vitamine B12 sérique",1,mydata$T9)
  mydata$T10=ifelse(!is.na(mydata[[i]]) & mydata[[i]]=="NFS, plaquettes",1,mydata$T10)
}
mydata$PMPI=(mydata$T1-(mydata$T2+mydata$T3+mydata$T4+mydata$T5+mydata$T6+mydata$T7+mydata$T8)+7)/8

#Generate third dependent variable: VPI
mydata=mydata %>% rename(VPI=`Q4(1)`)
mydata$VPI=gsub('€', '', mydata$VPI)
mydata$VPI=gsub(',', '.', mydata$VPI)
mydata$VPI=as.numeric(mydata$VPI)

#Alimentation
mydata = mydata %>% rename(
  fruitsVegetables=`Q15(1)`,
  wholeGrains=`Q15(2)`,
  legumes=`Q15(3)`,
  nuts=`Q15(4)`,
  meat=`Q15(5)`,
  dairy=`Q15(6)`,
  eggs=`Q15(7)`,
  fish=`Q15(8)`
)

#Connaissances nutrition
mydata = mydata %>% rename(
  knowledgeNutrition=`Q6(1)`,
  knowledgePlantBased=`Q7(1)`,
  knowledgePBOthers=`Q8(1)`,
  opinionPB=`Q9(1)`,
  opinionPBOthers=`Q10(1)`
)

#Avis sur les patients
mydata = mydata %>% rename(
  hideSymptoms=`Q12(1)`,
  changeDoctor=`Q13(1)`,
  alternativeMedecine=`Q14(1)`
)

#Advice Plant-based diets
mydata = mydata %>% rename(
  adviceDyslipidemia=`Q16(1)`,
  adviceCardiopathy=`Q16(2)`,
  adviceOverweight=`Q16(3)`,
  adviceDiabetes=`Q16(4)`,
  adviceHypertension=`Q16(5)`
)

#Know platforms
mydata = mydata %>% rename(
  knowONAV=`Q17(1)`,
  knowLundiVert=`Q17(2)`,
  knowVegecantine=`Q17(3)`,
  knowVegetarismeFR=`Q17(4)`,
  knowVegeclic=`Q17(5)`,
  knowVeganPratique=`Q17(6)`
)
vecPlatforms=c("knowONAV","knowLundiVert","knowVegecantine","knowVegetarismeFR","knowVegeclic","knowVeganPratique")
for(i in vecPlatforms){
  mydata[[i]]=ifelse(mydata[[i]]=="Oui",1,0)
}

#Save treated data
saveRDS(mydata,"Data/treatedData.RDS")

