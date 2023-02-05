# Function for working hydraulic vulnerability curve data. Tests for 
# outliers via the dixon Q test function. 
# Date 2.5.2023
# Written by : Marvin Browne

# Setup -------------------------------------------------------------------

library(here) # organization
#library(readr)
library(dplyr) # data wrangling
library(outliers) # dixon.test function

# Data Management ---------------------------------------------------------
#(gs_data <- read_csv("C:/Users/krazy/Downloads/gs_data.csv"))
load(here("gs_data.rda"))

gs_quag<-gs_data%>%# just Quercus agrifolia
  filter(species=="quag")

# construct something to work with in the non-functional version. 
df_bin<-gs_quag %>% 
  mutate(k_bins_numeric=cut(psi,
                    breaks = seq(from=0, to=max(psi)+0.5, by=0.5), # break from psi=0 to the max value + 0.5 to cover the driest points.
                    include.lowest = TRUE, 
                    labels=F),# instead of factor output vector on bin numbers
         k_bins_factor=cut(psi,
                            breaks = seq(from=0, to=max(psi)+0.5, by=0.5), # break from psi=0 to the max value + 0.5 to cover the driest points.
                            include.lowest = TRUE,ordered=TRUE)) # bins as factor and ordered


# search for bins that have less than 3 observations, add them to the next bin
# this is run "twice": first to check in general, and a second time to make sure 
# that the new bins also do not have less than 3 observations. 
df_bin_newbins<- df_bin%>% 
  group_by(k_bins_numeric)%>%
  mutate(k_bins_new=ifelse(n()<3, k_bins_numeric+1, k_bins_numeric))%>%
  group_by(k_bins_new)%>% # group by the new bins
  mutate(k_bins_new=ifelse(n()<3, k_bins_new+1, k_bins_new)) # values themselves are just to categorize the psi values 

# Non-functional version --------------------------------------------------

dixon_test.list <- lapply(unique(df_bin_newbins$k_bins_new), function(bin) {
  
  test <-
    dixon.test(x = df_bin_newbins[df_bin_newbins$k_bins_new==bin,]$gs, opposite = F)#if you want to check the opposite as the chosen value, opposite = T
 
  bin_asfactor<-df_bin_newbins[df_bin_newbins$k_bins_new==bin,]$k_bins_factor
  
   output.df <- data.frame(
    bin_fac = ifelse(min(bin_asfactor)==max(bin_asfactor), 
                     substring(as.character(unique(bin_asfactor)), 2, 6), 
                     paste(substring(min(bin_asfactor), 2,2), substring(max(bin_asfactor),4,6), sep = ",")),# may have a problem if bin is on one that got changed?
    bin_num = bin,
    dqr_hyp = test$alternative,
    dqr_stat = test$statistic,
    dqr_pval = test$p.value, 
    row.names=NULL) # end output.df
 
  return(output.df)
  } # end function
)

dixon_test.df <- do.call(rbind, dixon_test.list) 


# Vulnerability Curve Dixon Function --------------------------------------

vul.cur_dixon<-function(data,
                        breaks_by=0.5, # values to break of psi bins by 
                        test_var=character(), # column name of conductance measurement
                        ...){ # additional options for dixon test function. See ?dixon.test for options

  df_wbins<-data %>% 
  mutate(k_bins_numeric=cut(psi,
                            breaks = seq(from=0, to=max(psi)+breaks_by, by=breaks_by), # break from psi=0 to the max value + 0.5 to cover the driest points.
                            include.lowest = TRUE, 
                            labels=F),# instead of factor output vector on bin numbers
         k_bins_factor=cut(psi,
                           breaks = seq(from=0, to=max(psi)+breaks_by, by=breaks_by), # break from psi=0 to the max value + 0.5 to cover the driest points.
                           include.lowest = TRUE, ordered=TRUE))%>% # bins as factor and ordered%>% 
  
  group_by(k_bins_numeric)%>%
  
  mutate(k_bins_new=ifelse(n()<3, k_bins_numeric+1, k_bins_numeric))%>%
  
  group_by(k_bins_new)%>% # group by the new bins
  
  mutate(k_bins_new=ifelse(n()<3, k_bins_new+1, k_bins_new)) # values themselves are just to categorize the psi values 


dixon_test.list <- lapply(unique(df_wbins$k_bins_new), function(bin) {
  
  test <-
    dixon.test(x = df_wbins[[test_var]][df_wbins$k_bins_new==bin], ...)#if you want to check the opposite as the chosen value, opposite = T
  
  bin_asfactor<-df_wbins[df_wbins$k_bins_new==bin,]$k_bins_factor
  
  output.df <- data.frame(
    bin_fac = ifelse(min(bin_asfactor)==max(bin_asfactor), 
                     substring(as.character(unique(bin_asfactor)), 2, 6), 
                     paste(substring(min(bin_asfactor), 2,2), substring(max(bin_asfactor),4,6), sep = ",")),# may have a problem if bin is on one that got changed?
    bin_num = bin,
    dqr_hyp = test$alternative,
    dqr_stat = test$statistic,
    dqr_pval = test$p.value, 
    row.names=NULL) # end output.df
  
  return(output.df)
  } # end function
)

dixon_test.df <- do.call(rbind, dixon_test.list) 

return(dixon_test.df)
}

# Test Function -----------------------------------------------------------

vul.cur_dixon(gs_quag,
           breaks_by = 0.5, 
           test_var = "gs", 
           opposite=F)
