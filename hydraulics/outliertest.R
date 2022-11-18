
####Outlier test for vulnerability curve data


# Setup -------------------------------------------------------------------

library(here)
library(tidyverse)
library(outliers)


##functions##
#function creating list of subsetted dataframes
# s_create<-function(dataframe, spcode_col_num){#make sure you set the spns
# 
#   spns<-unique(dataframe[,spcode_col_num])#create list of species codes to filter by
# 
#   subsets<-lapply(spns, function(s) filter(dataframe, spcode_col_num==s))#create list of subset dataframes for each species
# 
# }

# DataInput ---------------------------------------------------------------

data<-readxl::read_excel(here("data/EFM_Summer2022.xlsx"), 
                         sheet = "OSBS_EFMoutliertest")%>%
  rename(Kleaf_Tcorr=`Kleaf Tcorr`, Psileaf=`Lowest Psileaf`)
data<-read.csv('test.csv')

bin<-0.5 #0.5 MPa Psi_leaf bins

# DixonTestPrep -----------------------------------------------------------

(splist<-unique(data$Species))#list of unique species names for reference

(acru<-data%>%#create df for Quercus laevis
  filter(Species=="ACRU")%>%
  select(Psi_lowest, K)%>%#select  psi and K columns
  arrange(Psi_lowest))#%>%#arrange in increasing order based on the psi
#mutate(psi_bin= cut(Psileaf, breaks=c(0, 0.5,1,1.5,2,2.5,3,3.5,max(Psileaf)))))

acru_cut<-cut(acru$Psi_lowest,c(0,0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0), include.lowest = TRUE)
acru$cut<-acru_cut

bins<-unique(acru$cut)

lapply(bins, function(x) {dixon.test(acru%>%filter(cut==x)%>%select(K))})



quge<-data%>%#create df for Quercus laevis
  filter(Species=="QUGE")%>%
  select(Psileaf, Kleaf_Tcorr)%>%#select  psi and K columns
  arrange(Psileaf)#arrange in increasing order based on the psi


# ComputeDixonTest --------------------------------------------------------


bins<-unique(qula$psi_bin)
tt<-qula%>%filter(psi_bin==bins[1])%>%pull(Kleaf_Tcorr)
#create something to hold output so that R doesn't complain in the loop....
temp<-lapply(1:bins, function(x) {

  test<-dixon.test(x=qula)

  result<-data.frame(species=mysps[[1]][1,]$spcode,variable=colnames(mysps[[1]][,x]), dqr_hyp=test$alternative,dqr_stat=test$statistic,dqr_pval=test$p.value)

})

output<-list()#again so that R doesn't scream about nonexistence

sp<-notmy

for(s in 1:length(spns)){

  ds[[s]]<-lapply(cols, function(x) {

    test<-dixon.test(x=sp[[s]][,x]%>%dplyr::pull(var=1), opposite = F)#if you want to check the opposite as the chosen value, opposite = T

    result<-data.frame(species=sp[[s]][1,]$spcode,
                       variable=colnames(sp[[s]][,x]),
                       dqr_hyp=test$alternative,
                       dqr_stat=test$statistic,
                       dqr_pval=test$p.value,
                       two_fold=max(sp[[s]][,x]%>%dplyr::pull(var=1))/min(sp[[s]][,x]%>%dplyr::pull(var=1)))
  })

  print(c(s, nrow(sp[[s]])))

  output[[s]]<-do.call(rbind,ds[[s]])
}

outoutput<-do.call(rbind,output)
row.names(outoutput)<-c(1:nrow(outoutput))


# OutputResults -----------------------------------------------------------


write.csv(outoutput, "dixonq_pv_notmbf.csv", row.names = F)