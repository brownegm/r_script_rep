# Outlier test for grouped data
# Date>>2.4.2023
# Written by>>Marvin Browne

# Setup -------------------------------------------------------------------

library(here) # file placement
library(dplyr) # piping and wrangling
library(outliers) # dixon.test function

## NOTE::see ?dixon.test for more information on the use of the dixon.text function.
## The test "performs several variants of the Dixon test for detecting outlier in 
## data sample" 

# Data --------------------------------------------------------------------s
gmin.df <- read.csv(here("Downloads", "gmin_compiled_r.csv"))# raw compiled gmin data

names(gmin.df)
#[1] "SPECIES"       "INDIVIDUAL"    "REPLICATE"     "TREATMENT"    
#[5] "gmin_interval" "gmin_slope"  

#miscellaneous objects
species_codes<-unique(gmin.df$SPECIES) #[1] "MALA" "SAAP" "RHIN"

treatments<-unique(gmin.df$TREATMENT) #[1] "Control"       "Abaxial_Tape"  "Adaxial_Tape"  "Perpendicular"
# [5] "Parallel"      "Whole_Tape"    "Whole_Vas"     "Adaxial_Vas"  
# [9] "Abaxial_Vas"  

#create subset for species and treatment
gmin_split_ST<-gmin.df %>% split(f=list(.$SPECIES, .$TREATMENT))

names(gmin_split_ST) # names of each data.frame in the list 

# Functions ---------------------------------------------------------------

outlier_dixon <- function(data,# a data frame
                          grp, grp2 = NA, # grouping variables
                          variable, # variable to run dixon test on 
                          ...) { # ... refers to inputs for the dixon.test. For example, opposite = F or two.sided = TRUE. See dixon.test help file for more info. 
  
  ifelse(is.na(grp2), # for the condition when grp2= NA
  split_grp.list <- data %>% split(f = list(.[, grp])),
  split_grp.list <- data %>% split(f = list(.[, grp], .[,grp2]))
  )
  
  var <- variable
  
  dixon_test.list <- lapply(split_grp.list, function(df) {
    
    test <-dixon.test(x = df[, var],...) # do the dixon for each group
    
    output.df <- data.frame(# create output data.frame
      variable = var,
      dqr_hyp = unique(test$alternative),
      dqr_stat = unique(test$statistic),
      dqr_pval = unique(test$p.value),
      two_fold = unique(max(df[, var], na.rm = T) / min(df[, var], na.rm = T))
    ) # end output.df
    
    return(output.df) #return the summarized dixon results
    
  } # end function
  
  )
  
  dixon_test.df <- do.call(rbind, dixon_test.list) %>%
    tibble::rownames_to_column("spcode.treatment")
  
  return(dixon_test.df)
  
}

# Run Dixon Q test for data -----------------------------------------------

dixon_highest<-outlier_dixon(data=gmin.df, grp = "SPECIES", grp2 = "TREATMENT", variable="gmin_slope", opposite= F)

dixon_lowest<-outlier_dixon(data=gmin.df, grp = "SPECIES", grp2 = "TREATMENT", variable="gmin_slope", opposite= T)

# Non-function version ----------------------------------------------------

dfs.list<-gmin_split_ST

var<-"gmin_slope"

dixon_test.list <- lapply(dfs.list, function(df) {
  test <-
    dixon.test(x = df[, var], opposite = F)#if you want to check the opposite as the chosen value, opposite = T
  output.df <- data.frame(
    variable = var,
    dqr_hyp = unique(test$alternative),
    dqr_stat = unique(test$statistic),
    dqr_pval = unique(test$p.value),
    two_fold = unique(max(df[,var], na.rm=T) / min(df[,var], na.rm=T))
  ) # end output.df
  
  return(output.df)
  } # end function
) 
  dixon_test.df<-do.call(rbind,dixon_test.list)%>%
   tibble::rownames_to_column("spcode.treatment")

