# Date/Author: 25 Oct 2021, by A. Kagawa; updated 5 May 2015, by M. Bartlett and M. Caringella; updated April 2019 by Joseph Zailaa
# Description: generates a correlation matrix (matrix of Pearson correlation coefficients for RAW and LOG data, and Spearman correlation coefficients for RANK data)
# Notes: May a more efficient (matrix operation) method, but this uses nested loops 
#
#This is an continuation of the correlation matrix script for conducting partial correlations of variables. 
#This accounts for the correlation among two variables with respect to another variable
#function requires the ppcor package


#####partial correlation matrix analysis functions####

##Function: corr_mat##
##Function used to turning correlation matrix operations 
#into function to allow for intraspecific and interspecific relationships. 

p_corr_mat<-function(dat=dat,k, #dat frame and the variable that we need to account for... maybe change the name of the argument to make more sense? 
                     raw=F, rank=F, log=F, file1='Something is wrong',file2='Somethingwentwrong'){

require(generalCorr)

n<-dim(dat)[2]                                  # number of variables/columns

cor.matrix <- matrix(nrow=n, ncol=n)            # create empty matrices
t.cor <- matrix(nrow=n, ncol=n)
p.cor <- matrix(nrow=n, ncol=n)


for (i in 1:n) {                                # for each column of data = x
  
  for (j in 1:n)  {                            # for each column of data = y
    
    if(is.na(j|i)){
      
    }else{
      
      #pcor.fit <- psych::partial.r(data=dat,x=c(names(dat)[i],names(dat)[j]), y=names(dat)[k], method = "pearson", use = "pairwise.complete.obs")#, exact = FALSE) # Calculate correlation among variables in dat
      pcor.fit<-parcor_ijk(dat[,i],dat[,j], dat[,k])#[[1]]
      
      }

    #cor.matrix[i,j] <- round(pcor.fit$estimate,digits=2)# and place in matrix
    cor.matrix[i,j] <- round(pcor.fit,digits=2)# and place in matrix
    
    #p.cor[i,j] <- pcor.fit$p.value     # Pick out p value from two-tailed t test on r
  }
}

cor.df<-as.data.frame(cor.matrix)               # change the output matrix into a dataframe (to allow naming of columns and rows)
#p.df<-as.data.frame(p.cor)


names(cor.df)<-names(dat); row.names(cor.df)<-names(dat)  # name columns and rows
#names(p.df)<-names(dat); row.names(p.df)<-names(dat)      # name columns and rows

  if(raw == T){
    
    write.csv(cor.df, file=file1)# write output to text files
    #write.csv(p.df, file=file2)
  
  } else if (rank == T) {
    
    write.csv(cor.df, file= file1)# write output to text files
    #write.csv(p.df, file=file2)  
  
  } else {
    
    write.csv(cor.df, file=file1)# write output to text files
    #write.csv(p.df, file=file2)
  }
  
}

