# Date/Author: 27 Sept 2007, by A. Kagawa; updated 5 May 2015, by M. Bartlett and M. Caringella; updated April 2019 by Joseph Zailaa
# Description: generates a correlation matrix (matrix of Pearson correlation coefficients for RAW and LOG data, and Spearman correlation coefficients for RANK data)
# Notes: May a more efficient (matrix operation) method, but this uses nested loops 

#####correlation matrix analysis functions####

##Function: initfile##
##Function used to create subsets of larger dataset and write files for each species based on 
##required file types specified.
##Used to create versions of intial dataset that are raw, log, or rank transformed. 
initfile<-function(spnames=NA, #species names
                   dat=NA, #dataframe
                   raw=F, log=F, rank=F){#file type
  
  for(sp in spnames){#create for loop to create .csv file for each species
    
    spc<-subset(dat, spcode == sp)#subset large dataset into one for each species
    
  #The following if statement should write species-specific files, name them 
  #accordingly and produce files without species and individual info: 'species',
  #'spcode','individual'.
    if (raw == T){
      
      write.csv(spc[,-c(1:3)],paste(sp, "RAWData.csv", sep="_"), row.names = F, na="")#write csv for each species
      
    }else if(log == T){
      
      write.csv(spc[,-c(1:3)],paste(sp, "LOGData.csv", sep="_"), row.names = F, na = "")#write csv for each species
      
    }else {
      
      write.csv(spc[,-c(1:3)],paste(sp,"RANKData.csv", sep = "_"), row.names = F, na = "")#write csv for each species
    }
  }
}

##Function: corr_mat##
##Function used to turning correlation matrix operations 
#into function to allow for intraspecific and interspecific relationships. 

corr_mat<-function(dat=dat, raw=F, rank=F, log=F, file1='Something is wrong',file2='Somethingwentwrong'){

n<-dim(dat)[2]                                  # number of variables/columns

cor.matrix <- matrix(nrow=n, ncol=n)            # create empty matrices
t.cor <- matrix(nrow=n, ncol=n)
p.cor <- matrix(nrow=n, ncol=n)


for (i in 1:n) {                                # for each column of data = x
  
  for (j in 1:n)  {                            # for each column of data = y
    
    if(is.na(j|i)){
      
    }else{
      
      lm.fit <- cor.test(dat[,i],dat[,j], method = "pearson", use = "pairwise.complete.obs", exact = FALSE) # Calculate correlation among variables in dat
    
      }

    cor.matrix[i,j] <- round(lm.fit$estimate,digits=2)# and place in matrix

    p.cor[i,j] <- lm.fit$p.value     # Pick out p value from two-tailed t test on r
  }
}

cor.df<-as.data.frame(cor.matrix)               # change the output matrix into a dataframe (to allow naming of columns and rows)
p.df<-as.data.frame(p.cor)


names(cor.df)<-names(dat); row.names(cor.df)<-names(dat)  # name columns and rows
names(p.df)<-names(dat); row.names(p.df)<-names(dat)      # name columns and rows

  if(raw == T){
    
    write.csv(cor.df, file=file1)# write output to text files
    write.csv(p.df, file=file2)
  
  } else if (rank == T) {
    
    write.csv(cor.df, file= file1)# write output to text files
    write.csv(p.df, file=file2)  
  
  } else {
    
    write.csv(cor.df, file=file1)# write output to text files
    write.csv(p.df, file=file2)
  }
  
}

##Function:corr_adj##
##Function used to update "p value files" created by corr_mat function to only have 
##stars or ns based on the significance of the correlation.

corr_adj<-function(corData, pData, filename='what is wrong'){ #raw=T, log=F, rank=F,){
  
  nn = dim(corData)[2]
  Star = corData
  DataStar = pData
  
  for (ii in 1:nn){
    for(jj in 1:nn){
      Star[ii,jj] = as.character(cut(pData[ii,jj],
                                     breaks = c(0, 0.001, 0.01, 0.05, 1),
                                     include.lowest = T,
                                     labels = c('***', '**', '*', 'ns')))
      DataStar[ii,jj] = paste(lapply(corData[ii,jj], round, 2), Star[ii,jj], sep = "")
    }}
  
  write.csv(DataStar, file=filename, na = "")
}

#####finalmat#######
### Create corr matrix with the p values and correlation coefficients together
finalmat<-function(dat1,#raw data
                   dat2,#log data 
                   dat3=NA,#rank data
                   filename="Why pree dis?"){
  
  FinalCorrMatrix = dat1
  nn = dim(dat1)[2]
  
  for (ii in 1:nn){
    for(jj in 1:nn){
      
      if (is.na(dat3)){
      FinalCorrMatrix[ii,jj] = paste(round(dat1[ii,jj], 2), round(dat2[ii,jj], 2),
                                     sep ="; ")
      }else{
      FinalCorrMatrix[ii,jj] = paste(dat1[ii,jj], dat2[ii,jj], dat3[ii,jj],
                                       sep ="; ")
         }
      }
  }
  
  write.csv(FinalCorrMatrix, file=filename)
  
}


##count how many significant correlations there 
sigsum<-function(dat,#raw data
                 filename="Why pree dis?"){
  
  sigsum = dat
  nn = dim(sigsum)[2]
  
  for (ii in 1:nn){
    for(jj in 1:nn){
      
      if (dat[ii,jj]<=0.05){
        sigsum[ii,jj] = 1
      }else{
        sigsum[ii,jj] = 0
      }
    }
  }
  
  write.csv(sigsum, file=filename)
  
}

sigsum2<-function(dat){#raw data
  
  sigsum = dat
  nn = dim(sigsum)[2]
  
  for (ii in 1:nn){
    for(jj in 1:nn){
      
      if (dat[ii,jj]<=0.05){
        
        sigsum[ii,jj] = 1
     
       }else{
        
        sigsum[ii,jj] = 0
       }
    }
  }
  return(sigsum)
}

sigsum2.1<-function(dat){#raw data
  
  sigsum = dat
  nn = dim(sigsum)[2]
  
  for (ii in 1:nn){
    for(jj in 1:nn){
      
      if (dat[ii,jj]<=0.05){
        
        sigsum[ii,jj] = 1
        
      }else{
        
        sigsum[ii,jj] = 0
      }
    }
  }
  return(sigsum)
}
## Function 4: high_tab ##
## Function used to create corr matrix with YES or NO for cell highlighting step in excel
high_tab <- function(RAWp,#raw data
                     LOGp,#log data 
                     RANKp,#rank data
                     filename=""){
  RAWp<-read.csv(RAWp)
  LOGp<-read.csv(LOGp)
  RANKp<-read.csv(RANKp)
  
  HighTab <- matrix(NA, ncol = ncol(pData), nrow = nrow(pData))
  
  for(ii in 1:ncol(RAWp)){
    for(jj in 1:ncol(RAWp)){
      if((RANKp[ii,jj]<0.05) & (RAWp[ii,jj]<0.05) | (LOGp[ii,jj]<0.05 )){
        HighTab[ii,jj] <- "YES"}
      else {HighTab[ii,jj] <- "NO" }
    }
  }
  HighTab <- as.data.frame(HighTab)
  colnames(HighTab) <- colnames(data)
  rownames(HighTab) <- colnames(data)
  
  write.csv(HighTab, file = filename)
}
