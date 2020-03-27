#setwd("/Users/codyreed/desktop/Cornell University/R projects")
install.packages("tidyverse")
library(tidyverse)
set.seed(041903)

###Featherman and Hauser (1978) data from Hout (1983)
  father_occ <- as.character(c(1,2,3,4,5,1,2,3,4,5,1,2,3,4,5,1,2,3,4,5,
                1,2,3,4,5))
  son_occ <- as.character(c(1,1,1,1,1,2,2,2,2,2,3,3,3,3,3,4,4,4,4,4,
             5,5,5,5,5))
  freq <- c(1414,724,798,756,409,521,524,648,914,357,302,
          254,856,771,441,643,703,1676,3325,1611,40,48,
          108,237,1832)
  
#combine variables
mobility_dat <- cbind.data.frame(father_occ,son_occ,freq)

#recreate table on page 11
xtabs(freq ~ father_occ + son_occ)  

######################################################################
###model assuming perfect mobility
perfect_mobility <- glm(freq ~ father_occ + son_occ, family = "poisson", data = mobility_dat)

#table of fitted values
pm_fitted <- perfect_mobility$fitted.values

#table matches results on page 14
xtabs(pm_fitted ~ father_occ + son_occ)

######################################################################
###QPM
#creating diagonal
mobility_dat <- mobility_dat %>%
  mutate(diag = ifelse(father_occ==son_occ, freq, 0)) %>%
  mutate(diag_c = ifelse(diag>0, 1, 0))

#QPM model
quasi_perfect_mobility <- glm(freq ~ son_occ + father_occ + factor(diag),
                              family = "poisson", data = mobility_dat)

#table of fitted values
qpm_fitted <- quasi_perfect_mobility$fitted.values

#table matches results on page 14
xtabs(qpm_fitted ~ father_occ + son_occ) #diagonal doesn't match

#have to predict without diag
temp_data <- mobility_dat %>%
  mutate(diag = 0)

#new fitted values
qpm_fitted_diag <- predict.glm(quasi_perfect_mobility, newdata = temp_data, type = "response")

#matches the QPM p. 23
xtabs(qpm_fitted_diag ~ father_occ + son_occ)

###constrained
quasi_perfect_mobility_c <- glm(freq ~ son_occ + father_occ + factor(diag_c),
                              family = gaussian(link = "identity"), data = mobility_dat)

qpmc_fitted <- quasi_perfect_mobility_c$fitted.values

temp_data <- mobility_dat %>%
  mutate(diag = 0)

#fitted values
qpmc_fitted <- predict.glm(quasi_perfect_mobility_c, newdata = temp_data, type = "response")

#table matches symmetry modell p. 26
xtabs(qpmc_fitted ~ father_occ + son_occ) 

######################################################################
#match corners 
mobility_dat <- mobility_dat %>%
  mutate(corners = ifelse(father_occ==1 & son_occ==2, freq,
                          ifelse(father_occ==2 & son_occ==1, freq,
                                 ifelse(father_occ==4 & son_occ==5, freq,
                                        ifelse(father_occ==5 & son_occ==4, freq, 0)))))

#corners model
corners_mobility <- glm(freq ~ factor(son_occ) + factor(father_occ) + factor(diag) + factor(corners),
                              family = "poisson", data = mobility_dat)

#exclude corners and diag for fitted values
temp_data <- mobility_dat %>%
  mutate(diag = 0, corners = 0)

#fitted values
corners_fitted <- predict.glm(corners_mobility, newdata = temp_data, type = "response")

#matches corner model p. 25
xtabs(corners_fitted ~ father_occ + son_occ)

######################################################################
###symmetry models
#perfectly symmetric table
tab <- xtabs(freq ~ father_occ + son_occ)

tab[lower.tri(tab)] <- t(tab)[lower.tri(tab)]

tab <- tab %>%
  as_tibble() %>%
  rename(symmetry = "n")

#merge with main data
mobility_dat <- left_join(mobility_dat, tab, by = c("father_occ", "son_occ"))
  
##symmetry model
symmetry_mobility <- glm(freq ~ factor(symmetry),
                        family = "poisson", data = mobility_dat)

#fitted values
symmetry_fitted <- symmetry_mobility$fitted.values

#table matches symmetry modell p. 26
xtabs(symmetry_fitted ~ father_occ + son_occ) 

##quasi symmetry model
quasi_symmetry_mobility <- glm(freq ~ father_occ + son_occ + factor(symmetry),
                         family = "poisson", data = mobility_dat)

#fitted values
quasi_symmetry_fitted <- quasi_symmetry_mobility$fitted.values

#table matches quasi symmetry model p. 26
xtabs(quasi_symmetry_fitted ~ father_occ + son_occ) 





