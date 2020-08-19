

#### ---- Setup ----
# Use these packages
library(tidyverse) # for data crunching
options(scipen=999) # To avoid scientific notation


#### ---- Data ----
# Import the assumptions used for MDA (best estimates, low and high estimates, by EU size) Fitzpatrick et al https://doi.org/10.1371/journal.pntd.0005037
eu_mda <- read_csv("tis_failure_mda_inputs.csv")

# Add the assumptions of survey costs from Stelmach et al DOI 10.1371/journal.pntd.0007605
medSurvCost <-8298
hiSurvCost <- 10111
loSurvCost <- 6532

# Add the assumption of average survey pass rate from Tropical Data (Anthony Solomon)
SurvPassProp <- .68

#### ---- Crunching ----

# Create data frame including EU sizes and both sets of assumptions
df <- data.frame(eu_mda, medSurvCost,hiSurvCost,loSurvCost,SurvPassProp) %>%
  # Generate variables
  mutate(# Caclulate the cost of MDA alone (MDA * best estimate * adaptation to 2019 USD)
         MDA = as.integer(eu*best*1.0342), 
         MDA_lo = as.integer(eu*lo*1.0342),
         MDA_hi = as.integer(eu*hi*1.0342),
         # Calculate the proportion of the cost of survey within total cost of MDA
         prop_med=as.integer(100-(medSurvCost/MDA*100)),
         prop_lo=as.integer(100-(loSurvCost/MDA_lo*100)),
         prop_hi=as.integer(100-(hiSurvCost/MDA_hi*100)),
         # Calculate the cost of MDA at 68% continuation rate (32% failure rate) + with survey costs
         surv_68=medSurvCost+((1-SurvPassProp)*MDA), 
         surv_68_lo=loSurvCost+((1-SurvPassProp)*MDA_lo),
         surv_68_hi=hiSurvCost+((1-SurvPassProp)*MDA_hi)) 

#### ---- Formatting ----

df_format <- df %>%
  # Add seperation comma and remove digits
  mutate_at(vars(eu, MDA,MDA_lo, MDA_hi, surv_68, surv_68_lo, surv_68_hi),
            funs(format(., big.mark = ",", digits = 0))) %>%
  # Combine best estimates and low and high estimates into one variable
  mutate(surv_68_ci=paste0(surv_68, " (", surv_68_lo, "-", surv_68_hi,")"),
         MDA_ci=paste0(MDA, " (", MDA_lo, "-", MDA_hi,")"),
         prop_ci=paste0(prop_med, " (", prop_lo,"-", prop_hi,")")) %>% 
  # Select and arrange variables
  select(eu, surv_68_ci,MDA_ci, prop_ci) %>%
  arrange(eu)

#### ---- Export ----

# export to CSV format
write_csv(df_format,"df_format.csv")






