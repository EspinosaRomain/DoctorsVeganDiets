library(pastecs)

matStatDes=function(df_funct,listVar_funct){
  Mat_funct=matrix(data=NA,nrow=length(listVar_funct),ncol=7)
  colnames(Mat_funct)=c("Mean","SD","Median","Min","Max","N","SE")
  rownames(Mat_funct)=listVar_funct
  k_funct=1
  for(j_funct in listVar_funct){
    tempStatDes=stat.desc(df_funct[[j_funct]])
    Mat_funct[k_funct,1]=round(tempStatDes["mean"],3)
    Mat_funct[k_funct,2]=round(tempStatDes["std.dev"],3)
    Mat_funct[k_funct,3]=round(tempStatDes["median"],3)
    Mat_funct[k_funct,4]=round(tempStatDes["min"],3)
    Mat_funct[k_funct,5]=round(tempStatDes["max"],3)
    Mat_funct[k_funct,6]=round(tempStatDes["nbr.val"],3)
    Mat_funct[k_funct,7]=round(tempStatDes["SE.mean"],3)
    k_funct=k_funct+1
  }
  Mat_funct
}

library(diagis)
matMeanSEWeighted=function(df_funct,listVar_funct,weight_funct){
  Mat_funct=matrix(data=NA,nrow=length(listVar_funct),ncol=3)
  colnames(Mat_funct)=c("Mean","SE","Median")
  rownames(Mat_funct)=listVar_funct
  k_funct=1
  for(j_funct in listVar_funct){
    tempStatDes=stat.desc(df_funct[[j_funct]])
    Mat_funct[k_funct,1]=round(weighted_mean(df_funct[[j_funct]], df_funct[[weight_funct]], na.rm=TRUE),3)
    Mat_funct[k_funct,2]=round(weighted_se(df_funct[[j_funct]], df_funct[[weight_funct]], na.rm=TRUE),3)
    Mat_funct[k_funct,3]=round(weighted_quantile(df_funct[[j_funct]], df_funct[[weight_funct]], na.rm=TRUE,probs=0.5),3)
    k_funct=k_funct+1
  }
  Mat_funct
}

library(plyr)
matTreatmentDiff=function(df_funct,listVar_funct,treatment_var_funct){
  Mat_funct=matrix(data=NA,nrow=length(listVar_funct),ncol=3)
  colnames(Mat_funct)=c("Control","Treated","Pvalue")
  rownames(Mat_funct)=listVar_funct
  df0=df_funct[df_funct[[treatment_var_funct]]==0,]
  df1=df_funct[df_funct[[treatment_var_funct]]==1,]
  k_funct=1
  for(j_funct in listVar_funct){
    Mat_funct[k_funct,1]=round(mean(df0[[j_funct]]),3)
    Mat_funct[k_funct,2]=round(mean(df1[[j_funct]]),3)
    Mat_funct[k_funct,3]=round(wilcox.test(df0[[j_funct]],df1[[j_funct]],exact=FALSE)$p.value,3)
    k_funct=k_funct+1
  }
  Mat_funct
}

#Returns the frequency of each categories for a series of variables with the same answer space
matCategoryFreq=function(df_funct,listVar_funct,listCaterogies_funct){
  Mat_funct=matrix(data=NA,nrow=length(listVar_funct),ncol=length(listCaterogies_funct))
  colnames(Mat_funct)=listCaterogies_funct
  rownames(Mat_funct)=listVar_funct
  k_funct=1
  for(j_funct in listVar_funct){
    for(l_funct in 1:length(listCaterogies_funct)){
      Mat_funct[k_funct,l_funct]=as.numeric(count(df_funct[df_funct[[j_funct]]==listCaterogies_funct[l_funct],]))/dim(df_funct)[1]*100
    }
    k_funct=k_funct+1
  }
  Mat_funct
}

#Returns the frequency of each categories for a series of variables with the same answer space WITH WEIGHTS
matCategoryFreqWeighted=function(df_funct,listVar_funct,listCaterogies_funct,weight_vec_funct){
  Mat_funct=matrix(data=NA,nrow=length(listVar_funct),ncol=length(listCaterogies_funct))
  colnames(Mat_funct)=listCaterogies_funct
  rownames(Mat_funct)=listVar_funct
  k_funct=1
  for(j_funct in listVar_funct){
    for(l_funct in 1:length(listCaterogies_funct)){
      Mat_funct[k_funct,l_funct]=sum(ifelse(df_funct[[j_funct]]==vecCategories[l_funct],df_funct[[weight_vec_funct]],0))/sum(df_funct[[weight_vec_funct]])*100
    }
    k_funct=k_funct+1
  }
  Mat_funct
}


