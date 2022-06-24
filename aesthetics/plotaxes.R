##Aug 6th, 2020
##by Marvin Browne
##function: plotaxis
##The purpose of this function is to create  even plot ticks at the minimum, middle and max values of the plot. 

plotaxis<-function(dat,#name of dataframe with variable 
                   colName,#what variable do you need to find intervals for (make sure there are quotations around the colnames )
                   round=2#round digits to?
){

  require(tidyverse)#piping and dplyr constructions will be employed....
  
  dd<-dat%>% #df specified in argument "dat"
    ungroup%>%
    select(!!sym(colName))#filter df to column specified in "colName"; **sym is used for passsing strings into function as a argument.
    
  #determining minimum, middle, and max of the particular variable plotted....default is to round to 2 digits. 
  min<-min(dd, na.rm=T)%>%round(digits=round)# calculate the minimum value of vector of values dd
  mid<-mean(c(min(dd, na.rm =T),max(dd, na.rm =T)))%>%round(digits=round)# calculate the middle value of vector of values dd
  max<-max(dd, na.rm =T)%>%round(digits=round)# calculate the max value of vector of values dd
  
  #output
  axes<-c(min, mid, max)#save each of those previously calculated values. 
  
}

###usage example

# #create three evenly spaced axis ticks for weight column on mtcars dataset
# axis_ticks<-plotaxis(dat=mtcars,colName = "wt")


# #Slightly more complicated 
# 
# #create three evenly spaced axis ticks for airquality dataset
# 
# months<-unique(airquality$Month)#unique list of months of year sampled
# 
# dats<-lapply(months, function (month) filter(airquality,Month==month))#create list of datasets, one for each month
# 
# axis_ticks<-lapply(dats, function(x) plotaxis(dat = x, colName = "Ozone"))#create set of evenly spaced axis tick values for all datasets. 
# 

