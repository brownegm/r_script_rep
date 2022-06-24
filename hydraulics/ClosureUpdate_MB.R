#Created by Megan Bartlett; little touches by Marvin
#up to date version of the line fitting script as of Feb 23,2018


# Setup -------------------------------------------------------------------

#set up environment
library(here)
library(tidyverse)
library(readxl)

#load scripts 
source(here('r_script',"r_fxs_update_MB_mb.R"))
#dd = read.csv("MetaAnalysis.csv",header=TRUE)
(dd = read_xlsx(here('data', 'all_NEON.xlsx')))

dd<-dd%>%rename(species=Species, psi=Psi_lowest, kl=K)%>%#rename var names to fit with the rest of the syntax below.
    mutate(data.type=rep(1,dim(dd)[1]))

dd<-dd%>%select(!c(Individual,Leaf,site))
#data prep
dd$to_split = paste(dd$species, dd$data.type)
aggregate(dd, by =list(dd$to_split), FUN=length) -> speciesN
speciesN[which(speciesN$to_split > 5),1] -> species_list
#Trying to fit curves with less than 6 points returns -inf for the AICcor. Are those the binned data? Would it be possible to make the bins smaller so there are more points?


# first, just to make sure that things work as expected, let's print out
# the gs column for each species
for (ii in 1:length(species_list)){
  subset(dd, dd$to_split == species_list[ii], select =c(1:5)) -> data_by_sp
  print(data_by_sp)
}
#I couldn't get the list format to save as a usable .csv file, so I changed everything to dataframes


# Line Fitting ------------------------------------------------------------

# use these gs's as inputs to the likelihood function
#Linear
pdf(file = here('outputs','Linear_fits.pdf'), width =8.5, height=5, onefile=T)
#Open a pdf file to save fitted curve plots in 
for (ii in 1:length(species_list)){
  subset(dd, dd$to_split == species_list[ii], select =c(1:5)) -> data_by_sp
  Linear_fits = do_the_thing_linear(data_by_sp, Linear)
  Linear_fits$D.NA<- NA # placeholder to keep all of the data frames the same size
  Linear_fits$sterror.NA <- NA
  rbind(modelfitting_results, as.data.frame(Linear_fits))-> modelfitting_results
  print(ii)
} 
dev.off()

#For every species, fit the curve and record the results  
modelfitting_results$psi_k20<- (0.8-1)*(-modelfitting_results$A.A)*-1/(modelfitting_results$B.B)
modelfitting_results$psi_k50<- (0.5-1)*(-modelfitting_results$A.A)*-1/(modelfitting_results$B.B)
modelfitting_results$psi_k80<- (0.2-1)*(-modelfitting_results$A.A)*-1/(modelfitting_results$B.B)
modelfitting_results$psi_k95<- (0.05-1)*(-modelfitting_results$A.A)*-1/(modelfitting_results$B.B)
#Every model has different formulas for k50 and k95
which(is.na(as.numeric(modelfitting_results[,12]))=='TRUE' | is.na(as.numeric(modelfitting_results[,13]))=='TRUE'| is.na(as.numeric(modelfitting_results[,14]))=='TRUE') -> problem_rows
#Look for curves that returns NAs for the parameter error values
write.csv(modelfitting_results, file = here('outputs',"Linear_fits.csv"))
cat("Linear has ", length(problem_rows), "curve(s) with NAs: ", problem_rows)


#Logistic
#Declare the pdf file to hold the plots
pdf(file = here('outputs','Logistic_fits.pdf'), width =8.5, height=5, onefile=T)
#Empty the data frame of the previous results
modelfitting_results[FALSE,] -> modelfitting_results
for (ii in 1:length(species_list)){
  subset(dd, dd$to_split == species_list[ii], select =c(1:5)) -> data_by_sp
  define_parsL(data_by_sp) -> par_estimates
  parsL=par_estimates[[1]]
  par_loL=par_estimates[[2]]
  par_highL=par_estimates[[3]]
  Logistic_fits = do_the_thing_nonlinear(data_by_sp, Logistic, parsL, par_loL, par_highL)
  rbind(modelfitting_results, as.data.frame(Logistic_fits))-> modelfitting_results
  print(ii)
}
dev.off()
modelfitting_results$psi_k20<- modelfitting_results$C.Xo*(1/0.8-1)^(1/modelfitting_results$B.B)
modelfitting_results$psi_k50<- modelfitting_results$C.Xo*(1/0.5-1)^(1/modelfitting_results$B.B)
modelfitting_results$psi_k80<- modelfitting_results$C.Xo*(1/0.2-1)^(1/modelfitting_results$B.B)
modelfitting_results$psi_k95<- modelfitting_results$C.Xo*(1/0.05-1)^(1/modelfitting_results$B.B)
which(is.na(as.numeric(modelfitting_results[,12]))=='TRUE' | is.na(as.numeric(modelfitting_results[,13]))=='TRUE'| is.na(as.numeric(modelfitting_results[,14]))=='TRUE'| is.na(as.numeric(modelfitting_results[,15]))=='TRUE') -> problem_rows
write.csv(modelfitting_results, file = here('outputs',"Logistic_fits.csv"))
cat("Logistic has ", length(problem_rows), "curve(s) with NAs: ", problem_rows)


#Sigmoidal
#Declare the pdf file to hold the plots
pdf(file = here('outputs','Sigmoidal_fits.pdf'), width =8.5, height=5, onefile=T)
#Empty the data frame of results
modelfitting_results[FALSE,] -> modelfitting_results
for (ii in 1:length(species_list)){
  subset(dd, dd$to_split == species_list[ii], select =c(1:5)) -> data_by_sp
  define_parsS(data_by_sp) -> par_estimates
  parsS=par_estimates[[1]]
  par_loS=par_estimates[[2]]
  par_highS=par_estimates[[3]]
  Sigmoidal_fits = do_the_thing_nonlinear(data_by_sp, Sigmoidal, parsS, par_loS, par_highS)
  rbind(modelfitting_results, as.data.frame(Sigmoidal_fits))-> modelfitting_results
  print(ii)
}
dev.off()
modelfitting_results$psi_k20<- modelfitting_results$C.Xo - modelfitting_results$B.B*log(1/0.8 -1)
modelfitting_results$psi_k50<- modelfitting_results$C.Xo - modelfitting_results$B.B*log(1/0.5 -1)
modelfitting_results$psi_k80<- modelfitting_results$C.Xo - modelfitting_results$B.B*log(1/0.2 -1)
modelfitting_results$psi_k95<- modelfitting_results$C.Xo - modelfitting_results$B.B*log(1/0.05 -1)
which(is.na(as.numeric(modelfitting_results[,12]))=='TRUE' | is.na(as.numeric(modelfitting_results[,13]))=='TRUE'| is.na(as.numeric(modelfitting_results[,14]))=='TRUE'| is.na(as.numeric(modelfitting_results[,15]))=='TRUE') -> problem_rows
write.csv(modelfitting_results, file = here('outputs',"Sigmoidal_fits.csv"))
cat("Sigmoidal has ", length(problem_rows), "curve(s) with NAs: ", problem_rows)

#Exponential
pdf(file = here('outputs','Exponential_fits.pdf'), width =8.5, height=5, onefile=T)
modelfitting_results[FALSE,] -> modelfitting_results
for (ii in 1:length(species_list)){
  subset(dd, dd$to_split == species_list[ii], select =c(1:5)) -> data_by_sp
  define_parsE(data_by_sp) -> par_estimates
  parsE=par_estimates[[1]]
  par_loE=par_estimates[[2]]
  par_highE=par_estimates[[3]]
  Exponential_fits = do_the_thing_nonlinear(data_by_sp, Exponential, parsE, par_loE, par_highE)
  Exponential_fits$D.NA<- NA 
  Exponential_fits$sterror.NA <- NA
  rbind(modelfitting_results, as.data.frame(Exponential_fits))-> modelfitting_results
  print(ii)
}
dev.off()
modelfitting_results$psi_k20<- log(0.8)/(-modelfitting_results$B.B)
modelfitting_results$psi_k50<- log(0.5)/(-modelfitting_results$B.B)
modelfitting_results$psi_k80<- log(0.2)/(-modelfitting_results$B.B)
modelfitting_results$psi_k95<- log(0.05)/(-modelfitting_results$B.B)
which(is.na(as.numeric(modelfitting_results[,12]))=='TRUE' | is.na(as.numeric(modelfitting_results[,13]))=='TRUE'| is.na(as.numeric(modelfitting_results[,14]))=='TRUE') -> problem_rows
write.csv(modelfitting_results, file = here('outputs',"Exponential_fits.csv"))
cat("Exponential has ", length(problem_rows), "curve(s) with NAs: ", problem_rows)

#Exponential2
pdf(file = here('outputs','Exponential2_fits.pdf'), width =8.5, height=5, onefile=T)
modelfitting_results[FALSE,] -> modelfitting_results
for (ii in 1:length(species_list)){
  subset(dd, dd$to_split == species_list[ii], select =c(1:5)) -> data_by_sp
  define_parsE2(data_by_sp) -> par_estimates
  parsE2=par_estimates[[1]]
  par_loE2=par_estimates[[2]]
  par_highE2=par_estimates[[3]]
  Exponential2_fits = do_the_thing_nonlinear(data_by_sp, Exponential2, parsE2, par_loE2, par_highE2)
  Exponential2_fits[which(is.na(Exponential2_fits)== TRUE)] = -999
  Exponential2_fits[which(Exponential2_fits== "N/A")] = -999
#########Update from Megan: this fit returns NaN (not a number) or "N/A" instead of NA at times (probably when C is very small),
####and this converts everything else in the modelfitting_results columns to NA-
###this replaces the NaNs with -999, so they'll be easy to find
  rbind(modelfitting_results, as.data.frame(Exponential2_fits))-> modelfitting_results
  print(ii)
}
dev.off()
modelfitting_results$psi_k20<- log(0.8- modelfitting_results$C.C/modelfitting_results$A.A)/(-modelfitting_results$B.B)
modelfitting_results$psi_k50<- log(0.5- modelfitting_results$C.C/modelfitting_results$A.A)/(-modelfitting_results$B.B)
modelfitting_results$psi_k80<- log(0.2- modelfitting_results$C.C/modelfitting_results$A.A)/(-modelfitting_results$B.B)
modelfitting_results$psi_k95<- log(0.05- modelfitting_results$C.C/modelfitting_results$A.A)/(-modelfitting_results$B.B)
#########Update from Megan: these are the functions for psi at different k thresholds
which(is.na(as.numeric(modelfitting_results[,12]))=='TRUE' | as.numeric(modelfitting_results[,12])==-999 |is.na(as.numeric(modelfitting_results[,13]))=='TRUE'| as.numeric(modelfitting_results[,13])==-999 | is.na(as.numeric(modelfitting_results[,14]))=='TRUE'| as.numeric(modelfitting_results[,14])==-999 ) -> problem_rows
write.csv(modelfitting_results, file = here('outputs',"Exponential2_fits.csv"))
cat("Exponential2 has ", length(problem_rows), "curve(s) with NAs: ", problem_rows)

