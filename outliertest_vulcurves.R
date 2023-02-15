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

hf_quru<-hf%>%filter(species.site=="HF_QURU")
# construct something to work with in the non-functional version. 
df_bin<-hf_quru %>% 
  mutate(k_bins_numeric=cut(Psi_lowest,
                            breaks = seq(from=0, to=max(Psi_lowest)+0.5, by=0.5), # break from psi=0 to the max value + 0.5 to cover the driest points.
                            include.lowest = TRUE, 
                            labels=F),# instead of factor output vector on bin numbers
         k_bins_factor=cut(Psi_lowest,
                           breaks = seq(from=0, to=max(Psi_lowest)+0.5, by=0.5), # break from psi=0 to the max value + 0.5 to cover the driest points.
                           include.lowest = TRUE, ordered=TRUE), # bins as factor and ordered%>% 
         max_bin=max(k_bins_numeric))%>% 
  
  group_by(k_bins_numeric)%>%
  mutate(n_bybin=n(),
         k_bins_new = ifelse(n_bybin<3 & k_bins_numeric==max_bin, k_bins_numeric-1, 
                             ifelse(n_bybin<3,  k_bins_numeric+1, k_bins_numeric)))%>%
  group_by(k_bins_new)%>% # group by the new bins
  
  mutate(k_bins_new=ifelse(n()<3, k_bins_new+1, 
                           k_bins_new)) # values themselves are just to categorize the psi values 

# search for bins that have less than 3 observations, add them to the next bin
# this is run "twice": first to check in general, and a second time to make sure 
# that the new bins also do not have less than 3 observations. 
df_bin_newbins<- df_bin%>% 
  group_by(k_bins_numeric)%>%
  mutate(k_bins_new=ifelse(n()<3, k_bins_numeric+1, k_bins_numeric))%>%
  group_by(k_bins_new)%>% # group by the new bins
  mutate(k_bins_new=ifelse(n()<3, k_bins_new+1, k_bins_new)) # values themselves are just to categorize the psi values 

# Non-functional version --------------------------------------------------

dixon_test.list <- lapply(unique(df_bin$k_bins_new), function(bin) {
  
  test <-
    dixon.test(x = df_bin[df_bin$k_bins_new==bin,]$K, opposite = F)#if you want to check the opposite as the chosen value, opposite = T
 
  bin_asfactor<-df_bin[df_bin$k_bins_new==bin,]$k_bins_factor
  
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
                        test_var, # column name of conductance measurement
                        psi_col="psi",
                        sps_col,
                        ...){ # additional options for dixon test function. See ?dixon.test for options

  df_wbins<-data %>% 
  mutate(k_bins_numeric=cut({{psi_col}},
                            breaks = seq(from=0, to=max({{psi_col}})+breaks_by, by=breaks_by), # break from psi=0 to the max value + 0.5 to cover the driest points.
                            include.lowest = TRUE, 
                            labels=F),# instead of factor output vector on bin numbers
         k_bins_factor=cut({{psi_col}},
                           breaks = seq(from=0, to=max({{psi_col}})+breaks_by, by=breaks_by), # break from psi=0 to the max value + 0.5 to cover the driest points.
                           include.lowest = TRUE, ordered=TRUE), # bins as factor and ordered%>% 
         max_bin=max(k_bins_numeric))%>% 
  
    group_by(k_bins_numeric)%>%
  
    mutate(n_bybin=n(),
         k_bins_new = ifelse(n_bybin<3 & k_bins_numeric==max_bin, k_bins_numeric-1, 
                             ifelse(n_bybin<3,  k_bins_numeric+1, k_bins_numeric)))%>% # bins as factor and ordered%>% 
    
  group_by(k_bins_new)%>% # group by the new bins
  
    mutate(k_bins_new=ifelse(n()<3, k_bins_new+1, 
                             k_bins_new)) # values themselves are just to categorize the psi values 
  

dixon_test.list <- lapply(unique(df_wbins$k_bins_new), function(bin) {
  
  test <-
    dixon.test(x = df_wbins[[{{test_var}}]][df_wbins$k_bins_new==bin], ...)#if you want to check the opposite as the chosen value, opposite = T
  
  IQR_bin <- IQR(df_wbins[[{{test_var}}]][df_wbins$k_bins_new==bin])
  
  Q1_Q3<-quantile(df_wbins[[{{test_var}}]][df_wbins$k_bins_new==bin], 
                  probs=c(0.25,0.75))
  upper_bound<-Q1_Q3[[2]] + (1.5 * IQR_bin)
  lower_bound<-Q1_Q3[[1]] - (1.5 * IQR_bin)
  
  bin_asfactor<-df_wbins[df_wbins$k_bins_new==bin,]$k_bins_factor
  
  output.df <- data.frame(
    species = ifelse(is.na(sps_col), df_wbins$Species,df_wbins[[{{sps_col}}]]),
    bin_fac = ifelse(min(bin_asfactor)==max(bin_asfactor), 
                     substring(as.character(unique(bin_asfactor)), 2, 6), 
                     paste(substring(min(bin_asfactor), 2,2), substring(max(bin_asfactor),4,6), sep = ",")),# may have a problem if bin is on one that got changed?
    bin_num = bin,
    dqr_hyp = test$alternative,
    dqr_stat = test$statistic,
    dqr_pval = test$p.value, 
    iqr_test.max = ifelse(max(df_wbins[[{{test_var}}]][df_wbins$k_bins_new==bin])>upper_bound,"Yes", "No"),
    iqr_test.min =ifelse(min(df_wbins[[{{test_var}}]][df_wbins$k_bins_new==bin])<lower_bound,"Yes", "No"),
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



vul.cur_dixon(hf_quru,
              breaks_by = 0.5, 
              test_var = "K", 
              psi_col = Psi_lowest, sps_col = "Species",
              opposite=F)
