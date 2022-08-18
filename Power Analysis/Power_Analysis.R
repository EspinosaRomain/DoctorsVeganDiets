#Import libraries
library(censReg)

#Set general parameters
N=400 #Number of total observations
J=1000 #number of simulations for each effect size
bonferroniAdj=3 #Three hypotheses to be tested in the paper
alpha=0.05 #Significance level

#-------------------------------------------------------------------------------#

###   TEST 1  - VDI ###
#Vegan disapproval (takes values between 0 and 1 included).
#The intervention is projected to decrease the level of VDI.
#SESOI (smallest effect size of interest) = 0.1 and defined according to Espinosa and Stoop (2021)
#Based on the pilot data, we assume: MEAN=0.3768 (H1) and SD=0.2185 (H2)
#We assume data are normally distributed among each group but are censored at 0 and 1. (H3)

#Set seed for replication
set.seed(123456789)

#Set parameters for simulation (defined according to SESOI)
minimumEF=0.1

#Vector to store the results of the simulations
vectorResults=rep(NA,J)

#Simulation
for(j in 1:J){
  t=ifelse(rnorm(N)>0,1,0) #Random Group generation
  y=rnorm(N, mean=0.3768, sd=0.2185)-minimumEF*t #Data simulation
  y=ifelse(y>0,y,0) #Censorship below 0
  y=ifelse(y<1,y,1) #Censorship above 1
  df=data.frame(cbind(t,y))
  
  estResult <- censReg(y ~ t, left=0, right=1, data = df ) #Tobit model
  me=margEff(estResult) #Compute marginal effects
  s=summary(me) 
  
  #Test H0: t<=0
  vectorResults[j]=ifelse(s[1]/s[2]<qnorm(alpha/bonferroniAdj),1,0)
  
}

#Statistical power:
powerForVDI=mean(vectorResults)
powerForVDI

#-------------------------------------------------------------------------------#

###   TEST 2   ###
#Proper Medical Practice Index (takes values between 0 and 1 included).
#It is made of the sum of separate biological tests.
#The intervention is projected to increase the level of PMPI.
#SESOI (smallest effect size of interest) = 1/n = 1/8 = 0.125 and defined as an improvement by at least one additional test in doctors’ prescriptions (either a useless test is abandoned, or an additional useful test is prescribed).
#It follows a binomial distribution normalized between 0 and 1.
#Based on the pilot data, the probability of a positive event is 0.625.
#The standard deviation in the pilot data is 0.220.

#Set seed for replication
set.seed(123456789)

#Average proba of positive event (before normalization)
predicted_p=0.625

#Set parameters for simulation
minimumEF=0.125 

#Vector to store the results of the simulations
vectorResults=rep(NA,J)

#Simulation
for(j in 1:J){
  
  y=rbinom(N/2, size=8, prob=predicted_p)/8 #Simulate data in the control
  y=c(y,rbinom(N/2, size=8, prob=predicted_p+minimumEF)/8) #Simulate data with treatment effect
  t=c(rep(0,N/2),rep(1,N/2))
  df=data.frame(cbind(t,y))
  
  estResult <- censReg(y ~ t, left=0, right=1, data = df ) #Tobit model
  me=margEff(estResult) #Compute marginal effects
  s=summary(me) 
  
  #Test H0: t<=0
  vectorResults[j]=ifelse(s[1]/s[2]>qnorm(1-alpha/bonferroniAdj),1,0)
  
}

#Statistical power:
powerForPMPI=mean(vectorResults)
powerForPMPI

#-------------------------------------------------------------------------------#

###   TEST 3  - VPI ###
#Vegan promotion index (takes values between 0 and 1 included).
#The intervention is projected to increase the level of VPI.
#SESOI (smallest effect size of interest) = 0.12 and defined according to Espinosa and Treich (2021).
#Based on the pilot data, we assume: MEAN=1.2064 (H1) and SD=0.8703151 (H2)
#We assume data are normally distributed among each group but are censored at 0 and 1. (H3)

#Set seed for replication
set.seed(123456789)

#Set parameters for simulation
minimumEF=0.12

#Vector to store the results of the simulations
vectorResults=rep(NA,J)

#Simulation
for(j in 1:J){
  t=ifelse(rnorm(N)>0,1,0) #Random Group generation
  y=rnorm(N, mean=1.2064, sd=0.8703151)+minimumEF*t #Simulate data
  y=ifelse(y>0,y,0) #Censorship below 0
  y=ifelse(y<1,y,1) #Censorship above 1
  df=data.frame(cbind(t,y))
  
  estResult <- censReg(y ~ t, left=0, right=1, data = df ) #Tobit model
  me=margEff(estResult) #Compute marginal effects
  s=summary(me) 
  
  #Test H0: t<=0
  vectorResults[j]=ifelse(s[1]/s[2]>qnorm(1-alpha/bonferroniAdj),1,0)
  
}

#Statistical power:
powerForVPI=mean(vectorResults)
powerForVPI

#-------------------------------------------------------------------------------#

#Show results:
powerForVDI #Predicted power is 0.996 or 99.6%.
powerForPMPI #Predicted power is 1 or 100%.
powerForVPI #Predicted power is 0.157 or 15.7%.
