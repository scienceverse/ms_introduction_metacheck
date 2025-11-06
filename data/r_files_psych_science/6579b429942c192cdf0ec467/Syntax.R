library(plyr)
library(dplyr)
library(tidyr)
library(psych)
library(haven)
library(lavaan)
library(lm.beta)
library(fastDummies)

# Import data -------------------------------------------------------------

setwd("***YOUR WORKING DIRECTORY")

### Demographics

dat_background_2019 = read_sav("avars_201905_EN_1.0p.sav") 
dat_background_2020 = read_sav("avars_202005_EN_1.0p.sav") 
dat_background_2021 = read_sav("avars_202105_EN_1.0p.sav") 
dat_background_2022 = read_sav("avars_202205_EN_1.0p.sav") 
dat_background_2023 = read_sav("avars_202305_EN_1.0p.sav") 

# attach survey wave to variable names for combination

colnames(dat_background_2019) = c("nomem_encr",paste0(colnames(dat_background_2019),"_19")[-1])
colnames(dat_background_2020) = c("nomem_encr",paste0(colnames(dat_background_2020),"_20")[-1])
colnames(dat_background_2021) = c("nomem_encr",paste0(colnames(dat_background_2021),"_21")[-1])
colnames(dat_background_2022) = c("nomem_encr",paste0(colnames(dat_background_2022),"_22")[-1])
colnames(dat_background_2023) = c("nomem_encr",paste0(colnames(dat_background_2023),"_23")[-1])

### Personality Module

dat_2019 = read_sav("cp19k_EN_1.0p.sav") # 201905 201906
dat_2020 = read_sav("cp20l_EN_1.0p.sav") # 202005 202006
dat_2021 = read_sav("cp21m_EN_1.0p.sav") # 202105 202106
dat_2022 = read_sav("cp22n_EN_1.0p.sav") # 202205 202206
dat_2023 = read_sav("cp23o_EN_1.0p.sav") # 202305 202306

### Merge data

dat = dat_2019 %>%
  full_join(dat_2020,by = "nomem_encr") %>%
  full_join(dat_2021,by = "nomem_encr") %>%
  full_join(dat_2022,by = "nomem_encr") %>%
  full_join(dat_2023,by = "nomem_encr") %>%
  left_join(dat_background_2019,by = "nomem_encr") %>%
  left_join(dat_background_2020,by = "nomem_encr") %>%
  left_join(dat_background_2021,by = "nomem_encr") %>%
  left_join(dat_background_2022,by = "nomem_encr") %>%
  left_join(dat_background_2023,by = "nomem_encr")

nrow(dat) # n = 8337

# Recode variable --------------------------------------------------------

### gender

dat$geslacht = ifelse(!is.na(dat$geslacht_19),dat$geslacht_19,
               ifelse(!is.na(dat$geslacht_20),dat$geslacht_20,
               ifelse(!is.na(dat$geslacht_21),dat$geslacht_21,
               ifelse(!is.na(dat$geslacht_22),dat$geslacht_22,
                      dat$geslacht_23))))

dat$gender = ifelse(is.na(dat$geslacht), "NA",
             ifelse(dat$geslacht == 1, "Male",
             ifelse(dat$geslacht == 2, "Female",
             ifelse(dat$geslacht == 3, "Other",NA)))) %>%
  factor(level = c("Male","Female","Other","NA"))

dat$female = ifelse(dat$geslacht == 1, 0,
             ifelse(dat$geslacht == 2, 1,
             ifelse(dat$geslacht == 3, NA,NA))) # other gender as NA for modelling

### age

dat$gebjaar = ifelse(!is.na(dat$gebjaar_19),dat$gebjaar_19,
              ifelse(!is.na(dat$gebjaar_20),dat$gebjaar_20,
              ifelse(!is.na(dat$gebjaar_21),dat$gebjaar_21,
              ifelse(!is.na(dat$gebjaar_22),dat$gebjaar_22,
                    dat$gebjaar_23))))

dat$age = 2019 - dat$gebjaar

dat$age_group = ifelse(dat$age < 40, "20_40",
                ifelse(dat$age < 60, "40_60",
                ifelse(dat$age >= 60, "60_80",NA)))

dat = dat %>% dummy_cols(select_columns = c('age_group'))

### education
             
dat$oplcat = ifelse(!is.na(dat$oplcat_19),dat$oplcat_19,
             ifelse(!is.na(dat$oplcat_20),dat$oplcat_20,
             ifelse(!is.na(dat$oplcat_21),dat$oplcat_21,
             ifelse(!is.na(dat$oplcat_22),dat$oplcat_22,
             ifelse(!is.na(dat$oplcat_23),dat$oplcat_23,
                    NA)))))

dat$college = ifelse(dat$oplcat %in% 5:6,1,0)

### valuing happiness

dat$v_hap_19 = dat$cp19k122
dat$v_hap_20 = dat$cp20l122
dat$v_hap_21 = dat$cp21m122
dat$v_hap_22 = dat$cp22n122
dat$v_hap_23 = dat$cp23o122

### valuing happiness (ipsatized)

ipsatization = function(val,list) {
  mean = mean(list,na.rm = TRUE)
  sd = sd(list,na.rm = TRUE)
  if (is.na(sd)) {NaN} # only one valid value
  else if (sd == 0) {0}  # all same values  
  else {(val-mean)/sd} # ipsatization
}

dat$v_hap_ip_19 = mapply(ipsatization, dat$cp19k122, dat %>% select(cp19k117:cp19k134) %>% t %>% as.data.frame)
dat$v_hap_ip_20 = mapply(ipsatization, dat$cp20l122, dat %>% select(cp20l117:cp20l134) %>% t %>% as.data.frame)
dat$v_hap_ip_21 = mapply(ipsatization, dat$cp21m122, dat %>% select(cp21m117:cp21m134) %>% t %>% as.data.frame)
dat$v_hap_ip_22 = mapply(ipsatization, dat$cp22n122, dat %>% select(cp22n117:cp22n134) %>% t %>% as.data.frame)
dat$v_hap_ip_23 = mapply(ipsatization, dat$cp23o122, dat %>% select(cp23o117:cp23o134) %>% t %>% as.data.frame)


### life satisfaction

dat$ls_19_1 = dat$cp19k014
dat$ls_19_2 = dat$cp19k015
dat$ls_19_3 = dat$cp19k016
dat$ls_19_4 = dat$cp19k017
dat$ls_19_5 = dat$cp19k018

dat$ls_20_1 = dat$cp20l014
dat$ls_20_2 = dat$cp20l015
dat$ls_20_3 = dat$cp20l016
dat$ls_20_4 = dat$cp20l017
dat$ls_20_5 = dat$cp20l018

dat$ls_21_1 = dat$cp21m014
dat$ls_21_2 = dat$cp21m015
dat$ls_21_3 = dat$cp21m016
dat$ls_21_4 = dat$cp21m017
dat$ls_21_5 = dat$cp21m018

dat$ls_22_1 = dat$cp22n014
dat$ls_22_2 = dat$cp22n015
dat$ls_22_3 = dat$cp22n016
dat$ls_22_4 = dat$cp22n017
dat$ls_22_5 = dat$cp22n018

dat$ls_23_1 = dat$cp23o014
dat$ls_23_2 = dat$cp23o015
dat$ls_23_3 = dat$cp23o016
dat$ls_23_4 = dat$cp23o017
dat$ls_23_5 = dat$cp23o018

# alpha(dat %>% select(ls_19_1:ls_19_5) %>% na.omit)
# alpha(dat %>% select(ls_20_1:ls_20_5) %>% na.omit)
# alpha(dat %>% select(ls_21_1:ls_21_5) %>% na.omit)
# alpha(dat %>% select(ls_22_1:ls_22_5) %>% na.omit)
# alpha(dat %>% select(ls_23_1:ls_23_5) %>% na.omit)

dat$ls_19 = (dat$ls_19_1 + dat$ls_19_2 + dat$ls_19_3 + dat$ls_19_4 + dat$ls_19_5)/5
dat$ls_20 = (dat$ls_20_1 + dat$ls_20_2 + dat$ls_20_3 + dat$ls_20_4 + dat$ls_20_5)/5
dat$ls_21 = (dat$ls_21_1 + dat$ls_21_2 + dat$ls_21_3 + dat$ls_21_4 + dat$ls_21_5)/5
dat$ls_22 = (dat$ls_22_1 + dat$ls_22_2 + dat$ls_22_3 + dat$ls_22_4 + dat$ls_22_5)/5
dat$ls_23 = (dat$ls_23_1 + dat$ls_23_2 + dat$ls_23_3 + dat$ls_23_4 + dat$ls_23_5)/5

# PANAS

dat$p_19_1 = dat$cp19k146
dat$p_19_2 = dat$cp19k148 
dat$p_19_3 = dat$cp19k150 
dat$p_19_4 = dat$cp19k154 
dat$p_19_5 = dat$cp19k155 
dat$p_19_6 = dat$cp19k157 
dat$p_19_7 = dat$cp19k159 
dat$p_19_8 = dat$cp19k161 
dat$p_19_9 = dat$cp19k162 
dat$p_19_10 = dat$cp19k164

dat$n_19_1 = dat$cp19k147 
dat$n_19_2 = dat$cp19k149 
dat$n_19_3 = dat$cp19k151 
dat$n_19_4 = dat$cp19k152 
dat$n_19_5 = dat$cp19k153 
dat$n_19_6 = dat$cp19k156 
dat$n_19_7 = dat$cp19k158
dat$n_19_8 = dat$cp19k160 
dat$n_19_9 = dat$cp19k163 
dat$n_19_10 = dat$cp19k165

dat$p_20_1 = dat$cp20l146
dat$p_20_2 = dat$cp20l148 
dat$p_20_3 = dat$cp20l150 
dat$p_20_4 = dat$cp20l154 
dat$p_20_5 = dat$cp20l155 
dat$p_20_6 = dat$cp20l157 
dat$p_20_7 = dat$cp20l159 
dat$p_20_8 = dat$cp20l161 
dat$p_20_9 = dat$cp20l162 
dat$p_20_10 = dat$cp20l164

dat$n_20_1 = dat$cp20l147 
dat$n_20_2 = dat$cp20l149 
dat$n_20_3 = dat$cp20l151 
dat$n_20_4 = dat$cp20l152 
dat$n_20_5 = dat$cp20l153 
dat$n_20_6 = dat$cp20l156 
dat$n_20_7 = dat$cp20l158
dat$n_20_8 = dat$cp20l160 
dat$n_20_9 = dat$cp20l163 
dat$n_20_10 = dat$cp20l165

dat$p_21_1 = dat$cp21m146
dat$p_21_2 = dat$cp21m148 
dat$p_21_3 = dat$cp21m150 
dat$p_21_4 = dat$cp21m154 
dat$p_21_5 = dat$cp21m155 
dat$p_21_6 = dat$cp21m157 
dat$p_21_7 = dat$cp21m159 
dat$p_21_8 = dat$cp21m161 
dat$p_21_9 = dat$cp21m162 
dat$p_21_10 = dat$cp21m164

dat$n_21_1 = dat$cp21m147 
dat$n_21_2 = dat$cp21m149 
dat$n_21_3 = dat$cp21m151 
dat$n_21_4 = dat$cp21m152 
dat$n_21_5 = dat$cp21m153 
dat$n_21_6 = dat$cp21m156 
dat$n_21_7 = dat$cp21m158
dat$n_21_8 = dat$cp21m160 
dat$n_21_9 = dat$cp21m163 
dat$n_21_10 = dat$cp21m165

dat$p_22_1 = dat$cp22n146
dat$p_22_2 = dat$cp22n148 
dat$p_22_3 = dat$cp22n150 
dat$p_22_4 = dat$cp22n154 
dat$p_22_5 = dat$cp22n155 
dat$p_22_6 = dat$cp22n157 
dat$p_22_7 = dat$cp22n159 
dat$p_22_8 = dat$cp22n161 
dat$p_22_9 = dat$cp22n162 
dat$p_22_10 = dat$cp22n164

dat$n_22_1 = dat$cp22n147 
dat$n_22_2 = dat$cp22n149 
dat$n_22_3 = dat$cp22n151 
dat$n_22_4 = dat$cp22n152 
dat$n_22_5 = dat$cp22n153 
dat$n_22_6 = dat$cp22n156 
dat$n_22_7 = dat$cp22n158
dat$n_22_8 = dat$cp22n160 
dat$n_22_9 = dat$cp22n163 
dat$n_22_10 = dat$cp22n165

dat$p_23_1 = dat$cp23o146
dat$p_23_2 = dat$cp23o148 
dat$p_23_3 = dat$cp23o150 
dat$p_23_4 = dat$cp23o154 
dat$p_23_5 = dat$cp23o155 
dat$p_23_6 = dat$cp23o157 
dat$p_23_7 = dat$cp23o159 
dat$p_23_8 = dat$cp23o161 
dat$p_23_9 = dat$cp23o162 
dat$p_23_10 = dat$cp23o164

dat$n_23_1 = dat$cp23o147 
dat$n_23_2 = dat$cp23o149 
dat$n_23_3 = dat$cp23o151 
dat$n_23_4 = dat$cp23o152 
dat$n_23_5 = dat$cp23o153 
dat$n_23_6 = dat$cp23o156 
dat$n_23_7 = dat$cp23o158
dat$n_23_8 = dat$cp23o160 
dat$n_23_9 = dat$cp23o163 
dat$n_23_10 = dat$cp23o165

# alpha(dat %>% select(p_19_1:p_19_10) %>% na.omit)
# alpha(dat %>% select(p_20_1:p_20_10) %>% na.omit)
# alpha(dat %>% select(p_21_1:p_21_10) %>% na.omit)
# alpha(dat %>% select(p_22_1:p_22_10) %>% na.omit)
# alpha(dat %>% select(p_23_1:p_23_10) %>% na.omit)

# alpha(dat %>% select(n_19_1:n_19_10) %>% na.omit)
# alpha(dat %>% select(n_20_1:n_20_10) %>% na.omit)
# alpha(dat %>% select(n_21_1:n_21_10) %>% na.omit)
# alpha(dat %>% select(n_22_1:n_22_10) %>% na.omit)
# alpha(dat %>% select(n_23_1:n_23_10) %>% na.omit)

dat$p_19 = (dat$p_19_1 + dat$p_19_2 + dat$p_19_3 + dat$p_19_4 + dat$p_19_5 + dat$p_19_6 + dat$p_19_7 + dat$p_19_8 + dat$p_19_9 + dat$p_19_10)/10
dat$p_20 = (dat$p_20_1 + dat$p_20_2 + dat$p_20_3 + dat$p_20_4 + dat$p_20_5 + dat$p_20_6 + dat$p_20_7 + dat$p_20_8 + dat$p_20_9 + dat$p_20_10)/10
dat$p_21 = (dat$p_21_1 + dat$p_21_2 + dat$p_21_3 + dat$p_21_4 + dat$p_21_5 + dat$p_21_6 + dat$p_21_7 + dat$p_21_8 + dat$p_21_9 + dat$p_21_10)/10
dat$p_22 = (dat$p_22_1 + dat$p_22_2 + dat$p_22_3 + dat$p_22_4 + dat$p_22_5 + dat$p_22_6 + dat$p_22_7 + dat$p_22_8 + dat$p_22_9 + dat$p_22_10)/10
dat$p_23 = (dat$p_23_1 + dat$p_23_2 + dat$p_23_3 + dat$p_23_4 + dat$p_23_5 + dat$p_23_6 + dat$p_23_7 + dat$p_23_8 + dat$p_23_9 + dat$p_23_10)/10

dat$n_19 = (dat$n_19_1 + dat$n_19_2 + dat$n_19_3 + dat$n_19_4 + dat$n_19_5 + dat$n_19_6 + dat$n_19_7 + dat$n_19_8 + dat$n_19_9 + dat$n_19_10)/10
dat$n_20 = (dat$n_20_1 + dat$n_20_2 + dat$n_20_3 + dat$n_20_4 + dat$n_20_5 + dat$n_20_6 + dat$n_20_7 + dat$n_20_8 + dat$n_20_9 + dat$n_20_10)/10
dat$n_21 = (dat$n_21_1 + dat$n_21_2 + dat$n_21_3 + dat$n_21_4 + dat$n_21_5 + dat$n_21_6 + dat$n_21_7 + dat$n_21_8 + dat$n_21_9 + dat$n_21_10)/10
dat$n_22 = (dat$n_22_1 + dat$n_22_2 + dat$n_22_3 + dat$n_22_4 + dat$n_22_5 + dat$n_22_6 + dat$n_22_7 + dat$n_22_8 + dat$n_22_9 + dat$n_22_10)/10
dat$n_23 = (dat$n_23_1 + dat$n_23_2 + dat$n_23_3 + dat$n_23_4 + dat$n_23_5 + dat$n_23_6 + dat$n_23_7 + dat$n_23_8 + dat$n_23_9 + dat$n_23_10)/10


# big five 

dat$E_19_1 = dat$cp19k020
dat$E_19_2 = 6 - dat$cp19k025 
dat$E_19_3 = dat$cp19k030
dat$E_19_4 = 6 - dat$cp19k035
dat$E_19_5 = dat$cp19k040
dat$E_19_6 = 6 - dat$cp19k045
dat$E_19_7 = dat$cp19k050
dat$E_19_8 = 6 - dat$cp19k055
dat$E_19_9 = dat$cp19k060
dat$E_19_10 = 6 -dat$cp19k065

dat$A_19_1 = 6 - dat$cp19k021
dat$A_19_2 = dat$cp19k026
dat$A_19_3 = 6 - dat$cp19k031
dat$A_19_4 = dat$cp19k036
dat$A_19_5 = 6 - dat$cp19k041
dat$A_19_6 = dat$cp19k046
dat$A_19_7 = 6 - dat$cp19k051
dat$A_19_8 = dat$cp19k056
dat$A_19_9 = dat$cp19k061
dat$A_19_10 = dat$cp19k066

dat$C_19_1 = dat$cp19k022
dat$C_19_2 = 6 - dat$cp19k027 
dat$C_19_3 = dat$cp19k032
dat$C_19_4 = 6 - dat$cp19k037
dat$C_19_5 = dat$cp19k042
dat$C_19_6 = 6 - dat$cp19k047
dat$C_19_7 = dat$cp19k052
dat$C_19_8 = 6 - dat$cp19k057
dat$C_19_9 = dat$cp19k062
dat$C_19_10 = dat$cp19k067

dat$N_19_1 = dat$cp19k023
dat$N_19_2 = 6 - dat$cp19k028 
dat$N_19_3 = dat$cp19k033
dat$N_19_4 = 6 - dat$cp19k038
dat$N_19_5 = dat$cp19k043
dat$N_19_6 = dat$cp19k048
dat$N_19_7 = dat$cp19k053
dat$N_19_8 = dat$cp19k058
dat$N_19_9 = dat$cp19k063
dat$N_19_10 = dat$cp19k068

dat$O_19_1 = dat$cp19k024
dat$O_19_2 = 6 - dat$cp19k029 
dat$O_19_3 = dat$cp19k034
dat$O_19_4 = 6 - dat$cp19k039
dat$O_19_5 = dat$cp19k044
dat$O_19_6 = 6 - dat$cp19k049
dat$O_19_7 = dat$cp19k054
dat$O_19_8 = dat$cp19k059
dat$O_19_9 = dat$cp19k064
dat$O_19_10 = dat$cp19k069

dat$E_20_1 = dat$cp20l020
dat$E_20_2 = 6 - dat$cp20l025 
dat$E_20_3 = dat$cp20l030
dat$E_20_4 = 6 - dat$cp20l035
dat$E_20_5 = dat$cp20l040
dat$E_20_6 = 6 - dat$cp20l045
dat$E_20_7 = dat$cp20l050
dat$E_20_8 = 6 - dat$cp20l055
dat$E_20_9 = dat$cp20l060
dat$E_20_10 = 6 -dat$cp20l065

dat$A_20_1 = 6 - dat$cp20l021
dat$A_20_2 = dat$cp20l026
dat$A_20_3 = 6 - dat$cp20l031
dat$A_20_4 = dat$cp20l036
dat$A_20_5 = 6 - dat$cp20l041
dat$A_20_6 = dat$cp20l046
dat$A_20_7 = 6 - dat$cp20l051
dat$A_20_8 = dat$cp20l056
dat$A_20_9 = dat$cp20l061
dat$A_20_10 = dat$cp20l066

dat$C_20_1 = dat$cp20l022
dat$C_20_2 = 6 - dat$cp20l027 
dat$C_20_3 = dat$cp20l032
dat$C_20_4 = 6 - dat$cp20l037
dat$C_20_5 = dat$cp20l042
dat$C_20_6 = 6 - dat$cp20l047
dat$C_20_7 = dat$cp20l052
dat$C_20_8 = 6 - dat$cp20l057
dat$C_20_9 = dat$cp20l062
dat$C_20_10 = dat$cp20l067

dat$N_20_1 = dat$cp20l023
dat$N_20_2 = 6 - dat$cp20l028 
dat$N_20_3 = dat$cp20l033
dat$N_20_4 = 6 - dat$cp20l038
dat$N_20_5 = dat$cp20l043
dat$N_20_6 = dat$cp20l048
dat$N_20_7 = dat$cp20l053
dat$N_20_8 = dat$cp20l058
dat$N_20_9 = dat$cp20l063
dat$N_20_10 = dat$cp20l068

dat$O_20_1 = dat$cp20l024
dat$O_20_2 = 6 - dat$cp20l029 
dat$O_20_3 = dat$cp20l034
dat$O_20_4 = 6 - dat$cp20l039
dat$O_20_5 = dat$cp20l044
dat$O_20_6 = 6 - dat$cp20l049
dat$O_20_7 = dat$cp20l054
dat$O_20_8 = dat$cp20l059
dat$O_20_9 = dat$cp20l064
dat$O_20_10 = dat$cp20l069

dat$E_21_1 = dat$cp21m020
dat$E_21_2 = 6 - dat$cp21m025 
dat$E_21_3 = dat$cp21m030
dat$E_21_4 = 6 - dat$cp21m035
dat$E_21_5 = dat$cp21m040
dat$E_21_6 = 6 - dat$cp21m045
dat$E_21_7 = dat$cp21m050
dat$E_21_8 = 6 - dat$cp21m055
dat$E_21_9 = dat$cp21m060
dat$E_21_10 = 6 -dat$cp21m065

dat$A_21_1 = 6 - dat$cp21m021
dat$A_21_2 = dat$cp21m026
dat$A_21_3 = 6 - dat$cp21m031
dat$A_21_4 = dat$cp21m036
dat$A_21_5 = 6 - dat$cp21m041
dat$A_21_6 = dat$cp21m046
dat$A_21_7 = 6 - dat$cp21m051
dat$A_21_8 = dat$cp21m056
dat$A_21_9 = dat$cp21m061
dat$A_21_10 = dat$cp21m066

dat$C_21_1 = dat$cp21m022
dat$C_21_2 = 6 - dat$cp21m027 
dat$C_21_3 = dat$cp21m032
dat$C_21_4 = 6 - dat$cp21m037
dat$C_21_5 = dat$cp21m042
dat$C_21_6 = 6 - dat$cp21m047
dat$C_21_7 = dat$cp21m052
dat$C_21_8 = 6 - dat$cp21m057
dat$C_21_9 = dat$cp21m062
dat$C_21_10 = dat$cp21m067

dat$N_21_1 = dat$cp21m023
dat$N_21_2 = 6 - dat$cp21m028 
dat$N_21_3 = dat$cp21m033
dat$N_21_4 = 6 - dat$cp21m038
dat$N_21_5 = dat$cp21m043
dat$N_21_6 = dat$cp21m048
dat$N_21_7 = dat$cp21m053
dat$N_21_8 = dat$cp21m058
dat$N_21_9 = dat$cp21m063
dat$N_21_10 = dat$cp21m068

dat$O_21_1 = dat$cp21m024
dat$O_21_2 = 6 - dat$cp21m029 
dat$O_21_3 = dat$cp21m034
dat$O_21_4 = 6 - dat$cp21m039
dat$O_21_5 = dat$cp21m044
dat$O_21_6 = 6 - dat$cp21m049
dat$O_21_7 = dat$cp21m054
dat$O_21_8 = dat$cp21m059
dat$O_21_9 = dat$cp21m064
dat$O_21_10 = dat$cp21m069

dat$E_22_1 = dat$cp22n020
dat$E_22_2 = 6 - dat$cp22n025 
dat$E_22_3 = dat$cp22n030
dat$E_22_4 = 6 - dat$cp22n035
dat$E_22_5 = dat$cp22n040
dat$E_22_6 = 6 - dat$cp22n045
dat$E_22_7 = dat$cp22n050
dat$E_22_8 = 6 - dat$cp22n055
dat$E_22_9 = dat$cp22n060
dat$E_22_10 = 6 -dat$cp22n065

dat$A_22_1 = 6 - dat$cp22n021
dat$A_22_2 = dat$cp22n026
dat$A_22_3 = 6 - dat$cp22n031
dat$A_22_4 = dat$cp22n036
dat$A_22_5 = 6 - dat$cp22n041
dat$A_22_6 = dat$cp22n046
dat$A_22_7 = 6 - dat$cp22n051
dat$A_22_8 = dat$cp22n056
dat$A_22_9 = dat$cp22n061
dat$A_22_10 = dat$cp22n066

dat$C_22_1 = dat$cp22n022
dat$C_22_2 = 6 - dat$cp22n027 
dat$C_22_3 = dat$cp22n032
dat$C_22_4 = 6 - dat$cp22n037
dat$C_22_5 = dat$cp22n042
dat$C_22_6 = 6 - dat$cp22n047
dat$C_22_7 = dat$cp22n052
dat$C_22_8 = 6 - dat$cp22n057
dat$C_22_9 = dat$cp22n062
dat$C_22_10 = dat$cp22n067

dat$N_22_1 = dat$cp22n023
dat$N_22_2 = 6 - dat$cp22n028 
dat$N_22_3 = dat$cp22n033
dat$N_22_4 = 6 - dat$cp22n038
dat$N_22_5 = dat$cp22n043
dat$N_22_6 = dat$cp22n048
dat$N_22_7 = dat$cp22n053
dat$N_22_8 = dat$cp22n058
dat$N_22_9 = dat$cp22n063
dat$N_22_10 = dat$cp22n068

dat$O_22_1 = dat$cp22n024
dat$O_22_2 = 6 - dat$cp22n029 
dat$O_22_3 = dat$cp22n034
dat$O_22_4 = 6 - dat$cp22n039
dat$O_22_5 = dat$cp22n044
dat$O_22_6 = 6 - dat$cp22n049
dat$O_22_7 = dat$cp22n054
dat$O_22_8 = dat$cp22n059
dat$O_22_9 = dat$cp22n064
dat$O_22_10 = dat$cp22n069

dat$E_23_1 = dat$cp23o020
dat$E_23_2 = 6 - dat$cp23o025 
dat$E_23_3 = dat$cp23o030
dat$E_23_4 = 6 - dat$cp23o035
dat$E_23_5 = dat$cp23o040
dat$E_23_6 = 6 - dat$cp23o045
dat$E_23_7 = dat$cp23o050
dat$E_23_8 = 6 - dat$cp23o055
dat$E_23_9 = dat$cp23o060
dat$E_23_10 = 6 -dat$cp23o065

dat$A_23_1 = 6 - dat$cp23o021
dat$A_23_2 = dat$cp23o026
dat$A_23_3 = 6 - dat$cp23o031
dat$A_23_4 = dat$cp23o036
dat$A_23_5 = 6 - dat$cp23o041
dat$A_23_6 = dat$cp23o046
dat$A_23_7 = 6 - dat$cp23o051
dat$A_23_8 = dat$cp23o056
dat$A_23_9 = dat$cp23o061
dat$A_23_10 = dat$cp23o066

dat$C_23_1 = dat$cp23o022
dat$C_23_2 = 6 - dat$cp23o027 
dat$C_23_3 = dat$cp23o032
dat$C_23_4 = 6 - dat$cp23o037
dat$C_23_5 = dat$cp23o042
dat$C_23_6 = 6 - dat$cp23o047
dat$C_23_7 = dat$cp23o052
dat$C_23_8 = 6 - dat$cp23o057
dat$C_23_9 = dat$cp23o062
dat$C_23_10 = dat$cp23o067

dat$N_23_1 = dat$cp23o023
dat$N_23_2 = 6 - dat$cp23o028 
dat$N_23_3 = dat$cp23o033
dat$N_23_4 = 6 - dat$cp23o038
dat$N_23_5 = dat$cp23o043
dat$N_23_6 = dat$cp23o048
dat$N_23_7 = dat$cp23o053
dat$N_23_8 = dat$cp23o058
dat$N_23_9 = dat$cp23o063
dat$N_23_10 = dat$cp23o068

dat$O_23_1 = dat$cp23o024
dat$O_23_2 = 6 - dat$cp23o029 
dat$O_23_3 = dat$cp23o034
dat$O_23_4 = 6 - dat$cp23o039
dat$O_23_5 = dat$cp23o044
dat$O_23_6 = 6 - dat$cp23o049
dat$O_23_7 = dat$cp23o054
dat$O_23_8 = dat$cp23o059
dat$O_23_9 = dat$cp23o064
dat$O_23_10 = dat$cp23o069

# alpha(dat %>% select(E_19_1:E_19_10) %>% na.omit)
# alpha(dat %>% select(E_20_1:E_20_10) %>% na.omit)
# alpha(dat %>% select(E_21_1:E_21_10) %>% na.omit)
# alpha(dat %>% select(E_22_1:E_22_10) %>% na.omit)
# alpha(dat %>% select(E_23_1:E_23_10) %>% na.omit)
# 
# alpha(dat %>% select(A_19_1:A_19_10) %>% na.omit)
# alpha(dat %>% select(A_20_1:A_20_10) %>% na.omit)
# alpha(dat %>% select(A_21_1:A_21_10) %>% na.omit)
# alpha(dat %>% select(A_22_1:A_22_10) %>% na.omit)
# alpha(dat %>% select(A_23_1:A_23_10) %>% na.omit)
# 
# alpha(dat %>% select(C_19_1:C_19_10) %>% na.omit)
# alpha(dat %>% select(C_20_1:C_20_10) %>% na.omit)
# alpha(dat %>% select(C_21_1:C_21_10) %>% na.omit)
# alpha(dat %>% select(C_22_1:C_22_10) %>% na.omit)
# alpha(dat %>% select(C_23_1:C_23_10) %>% na.omit)
# 
# alpha(dat %>% select(N_19_1:N_19_10) %>% na.omit)
# alpha(dat %>% select(N_20_1:N_20_10) %>% na.omit)
# alpha(dat %>% select(N_21_1:N_21_10) %>% na.omit)
# alpha(dat %>% select(N_22_1:N_22_10) %>% na.omit)
# alpha(dat %>% select(N_23_1:N_23_10) %>% na.omit)
# 
# alpha(dat %>% select(O_19_1:O_19_10) %>% na.omit)
# alpha(dat %>% select(O_20_1:O_20_10) %>% na.omit)
# alpha(dat %>% select(O_21_1:O_21_10) %>% na.omit)
# alpha(dat %>% select(O_22_1:O_22_10) %>% na.omit)
# alpha(dat %>% select(O_23_1:O_23_10) %>% na.omit)

dat$E_19 = (dat$E_19_1 + dat$E_19_2 + dat$E_19_3 + dat$E_19_4 + dat$E_19_5 + dat$E_19_6 + dat$E_19_7 + dat$E_19_8 + dat$E_19_9 + dat$E_19_10)/10
dat$E_20 = (dat$E_20_1 + dat$E_20_2 + dat$E_20_3 + dat$E_20_4 + dat$E_20_5 + dat$E_20_6 + dat$E_20_7 + dat$E_20_8 + dat$E_20_9 + dat$E_20_10)/10
dat$E_21 = (dat$E_21_1 + dat$E_21_2 + dat$E_21_3 + dat$E_21_4 + dat$E_21_5 + dat$E_21_6 + dat$E_21_7 + dat$E_21_8 + dat$E_21_9 + dat$E_21_10)/10
dat$E_22 = (dat$E_22_1 + dat$E_22_2 + dat$E_22_3 + dat$E_22_4 + dat$E_22_5 + dat$E_22_6 + dat$E_22_7 + dat$E_22_8 + dat$E_22_9 + dat$E_22_10)/10
dat$E_23 = (dat$E_23_1 + dat$E_23_2 + dat$E_23_3 + dat$E_23_4 + dat$E_23_5 + dat$E_23_6 + dat$E_23_7 + dat$E_23_8 + dat$E_23_9 + dat$E_23_10)/10

dat$A_19 = (dat$A_19_1 + dat$A_19_2 + dat$A_19_3 + dat$A_19_4 + dat$A_19_5 + dat$A_19_6 + dat$A_19_7 + dat$A_19_8 + dat$A_19_9 + dat$A_19_10)/10
dat$A_20 = (dat$A_20_1 + dat$A_20_2 + dat$A_20_3 + dat$A_20_4 + dat$A_20_5 + dat$A_20_6 + dat$A_20_7 + dat$A_20_8 + dat$A_20_9 + dat$A_20_10)/10
dat$A_21 = (dat$A_21_1 + dat$A_21_2 + dat$A_21_3 + dat$A_21_4 + dat$A_21_5 + dat$A_21_6 + dat$A_21_7 + dat$A_21_8 + dat$A_21_9 + dat$A_21_10)/10
dat$A_22 = (dat$A_22_1 + dat$A_22_2 + dat$A_22_3 + dat$A_22_4 + dat$A_22_5 + dat$A_22_6 + dat$A_22_7 + dat$A_22_8 + dat$A_22_9 + dat$A_22_10)/10
dat$A_23 = (dat$A_23_1 + dat$A_23_2 + dat$A_23_3 + dat$A_23_4 + dat$A_23_5 + dat$A_23_6 + dat$A_23_7 + dat$A_23_8 + dat$A_23_9 + dat$A_23_10)/10

dat$C_19 = (dat$C_19_1 + dat$C_19_2 + dat$C_19_3 + dat$C_19_4 + dat$C_19_5 + dat$C_19_6 + dat$C_19_7 + dat$C_19_8 + dat$C_19_9 + dat$C_19_10)/10
dat$C_20 = (dat$C_20_1 + dat$C_20_2 + dat$C_20_3 + dat$C_20_4 + dat$C_20_5 + dat$C_20_6 + dat$C_20_7 + dat$C_20_8 + dat$C_20_9 + dat$C_20_10)/10
dat$C_21 = (dat$C_21_1 + dat$C_21_2 + dat$C_21_3 + dat$C_21_4 + dat$C_21_5 + dat$C_21_6 + dat$C_21_7 + dat$C_21_8 + dat$C_21_9 + dat$C_21_10)/10
dat$C_22 = (dat$C_22_1 + dat$C_22_2 + dat$C_22_3 + dat$C_22_4 + dat$C_22_5 + dat$C_22_6 + dat$C_22_7 + dat$C_22_8 + dat$C_22_9 + dat$C_22_10)/10
dat$C_23 = (dat$C_23_1 + dat$C_23_2 + dat$C_23_3 + dat$C_23_4 + dat$C_23_5 + dat$C_23_6 + dat$C_23_7 + dat$C_23_8 + dat$C_23_9 + dat$C_23_10)/10

dat$N_19 = (dat$N_19_1 + dat$N_19_2 + dat$N_19_3 + dat$N_19_4 + dat$N_19_5 + dat$N_19_6 + dat$N_19_7 + dat$N_19_8 + dat$N_19_9 + dat$N_19_10)/10
dat$N_20 = (dat$N_20_1 + dat$N_20_2 + dat$N_20_3 + dat$N_20_4 + dat$N_20_5 + dat$N_20_6 + dat$N_20_7 + dat$N_20_8 + dat$N_20_9 + dat$N_20_10)/10
dat$N_21 = (dat$N_21_1 + dat$N_21_2 + dat$N_21_3 + dat$N_21_4 + dat$N_21_5 + dat$N_21_6 + dat$N_21_7 + dat$N_21_8 + dat$N_21_9 + dat$N_21_10)/10
dat$N_22 = (dat$N_22_1 + dat$N_22_2 + dat$N_22_3 + dat$N_22_4 + dat$N_22_5 + dat$N_22_6 + dat$N_22_7 + dat$N_22_8 + dat$N_22_9 + dat$N_22_10)/10
dat$N_23 = (dat$N_23_1 + dat$N_23_2 + dat$N_23_3 + dat$N_23_4 + dat$N_23_5 + dat$N_23_6 + dat$N_23_7 + dat$N_23_8 + dat$N_23_9 + dat$N_23_10)/10

dat$O_19 = (dat$O_19_1 + dat$O_19_2 + dat$O_19_3 + dat$O_19_4 + dat$O_19_5 + dat$O_19_6 + dat$O_19_7 + dat$O_19_8 + dat$O_19_9 + dat$O_19_10)/10
dat$O_20 = (dat$O_20_1 + dat$O_20_2 + dat$O_20_3 + dat$O_20_4 + dat$O_20_5 + dat$O_20_6 + dat$O_20_7 + dat$O_20_8 + dat$O_20_9 + dat$O_20_10)/10
dat$O_21 = (dat$O_21_1 + dat$O_21_2 + dat$O_21_3 + dat$O_21_4 + dat$O_21_5 + dat$O_21_6 + dat$O_21_7 + dat$O_21_8 + dat$O_21_9 + dat$O_21_10)/10
dat$O_22 = (dat$O_22_1 + dat$O_22_2 + dat$O_22_3 + dat$O_22_4 + dat$O_22_5 + dat$O_22_6 + dat$O_22_7 + dat$O_22_8 + dat$O_22_9 + dat$O_22_10)/10
dat$O_23 = (dat$O_23_1 + dat$O_23_2 + dat$O_23_3 + dat$O_23_4 + dat$O_23_5 + dat$O_23_6 + dat$O_23_7 + dat$O_23_8 + dat$O_23_9 + dat$O_23_10)/10

dat$E = dat %>% select(E_19:E_23) %>% rowMeans(na.rm = TRUE)
dat$A = dat %>% select(A_19:A_23) %>% rowMeans(na.rm = TRUE)
dat$C = dat %>% select(C_19:C_23) %>% rowMeans(na.rm = TRUE)
dat$N = dat %>% select(N_19:N_23) %>% rowMeans(na.rm = TRUE)
dat$O = dat %>% select(O_19:O_23) %>% rowMeans(na.rm = TRUE)


dat$E_cate = ifelse(dat$E < dat$E %>% quantile(c(1/3),na.rm = TRUE),"low",
             ifelse(dat$E <= dat$E %>% quantile(c(2/3),na.rm = TRUE),"middle",
                    "high")) %>% factor(levels = c("low","middle","high"))
dat$A_cate = ifelse(dat$A < dat$A %>% quantile(c(1/3),na.rm = TRUE),"low",
             ifelse(dat$A <= dat$A %>% quantile(c(2/3),na.rm = TRUE),"middle",
                    "high")) %>% factor(levels = c("low","middle","high"))
dat$C_cate = ifelse(dat$C < dat$C %>% quantile(c(1/3),na.rm = TRUE),"low",
             ifelse(dat$C <= dat$C %>% quantile(c(2/3),na.rm = TRUE),"middle",
                    "high")) %>% factor(levels = c("low","middle","high"))
dat$N_cate = ifelse(dat$N < dat$N %>% quantile(c(1/3),na.rm = TRUE),"low",
             ifelse(dat$N <= dat$N %>% quantile(c(2/3),na.rm = TRUE),"middle",
                    "high")) %>% factor(levels = c("low","middle","high"))
dat$O_cate = ifelse(dat$O < dat$O %>% quantile(c(1/3),na.rm = TRUE),"low",
             ifelse(dat$O <= dat$O %>% quantile(c(2/3),na.rm = TRUE),"middle",
                    "high")) %>% factor(levels = c("low","middle","high"))


# Removing participants with missing values ----------------------------------------------------------------

dat$n_missing = dat %>% select(v_hap_19:v_hap_23,ls_19:ls_23) %>% t %>% as.data.frame %>% lapply(function(x) sum(is.na(x))) %>% unlist 

dat = dat %>% filter(n_missing < 10)

nrow(dat) #8331


# Baseline correlation ----------------------------------------------------

cor(dat %>% select(v_hap_19,v_hap_ip_19,ls_19,p_19,n_19) %>% na.omit) %>% round(2)


# RICLPM (Life Satisfaction) ----------------------------------------------------------------

RICLPM_1 <- '
  # between random intercepts
  RIx =~ 1*v_hap_19 + 1*v_hap_20 + 1*v_hap_21 + 1*v_hap_22 + 1*v_hap_23
  RIy =~ 1*ls_19 + 1*ls_20 + 1*ls_21 + 1*ls_22 + 1*ls_23
  
  # within-person centered variables
  wx1 =~ 1*v_hap_19
  wx2 =~ 1*v_hap_20
  wx3 =~ 1*v_hap_21
  wx4 =~ 1*v_hap_22
  wx5 =~ 1*v_hap_23
  wy1 =~ 1*ls_19
  wy2 =~ 1*ls_20
  wy3 =~ 1*ls_21
  wy4 =~ 1*ls_22
  wy5 =~ 1*ls_23

  # lagged effects
  wx2 ~ a*wx1 + b*wy1 
  wy2 ~ c*wx1 + d*wy1
  wx3 ~ a*wx2 + b*wy2
  wy3 ~ c*wx2 + d*wy2
  wx4 ~ a*wx3 + b*wy3
  wy4 ~ c*wx3 + d*wy3
  wx5 ~ a*wx4 + b*wy4
  wy5 ~ c*wx4 + d*wy4
  
  # covariance between within-person centered variables at first wave
  wx1 ~~ wy1 
  
  # covariances between residuals of within-person centered variables 
  wx2 ~~ cov*wy2
  wx3 ~~ cov*wy3
  wx4 ~~ cov*wy4 
  wx5 ~~ cov*wy5

  # variance and covariance of random intercepts
  RIx ~~ RIx
  RIy ~~ RIy
  RIx ~~ RIy

  # (residual) variance of within-person centered variables
  wx1 ~~ wx1 # Variance
  wy1 ~~ wy1 
  wx2 ~~ vx*wx2 # Residual variance
  wy2 ~~ vy*wy2 
  wx3 ~~ vx*wx3 
  wy3 ~~ vy*wy3 
  wx4 ~~ vx*wx4 
  wy4 ~~ vy*wy4 
  wx5 ~~ vx*wx5
  wy5 ~~ vy*wy5
'

RICLPM_2 <- '
  # between components
  RIx =~ 1*v_hap_ip_19 + 1*v_hap_ip_20 + 1*v_hap_ip_21 + 1*v_hap_ip_22 + 1*v_hap_ip_23
  RIy =~ 1*ls_19 + 1*ls_20 + 1*ls_21 + 1*ls_22 + 1*ls_23
  
  # within-person centered variables
  wx1 =~ 1*v_hap_ip_19
  wx2 =~ 1*v_hap_ip_20
  wx3 =~ 1*v_hap_ip_21
  wx4 =~ 1*v_hap_ip_22
  wx5 =~ 1*v_hap_ip_23
  wy1 =~ 1*ls_19
  wy2 =~ 1*ls_20
  wy3 =~ 1*ls_21
  wy4 =~ 1*ls_22
  wy5 =~ 1*ls_23

  # lagged effects between within-person centered variables
  wx2 ~ a*wx1 + b*wy1 
  wy2 ~ c*wx1 + d*wy1
  wx3 ~ a*wx2 + b*wy2
  wy3 ~ c*wx2 + d*wy2
  wx4 ~ a*wx3 + b*wy3
  wy4 ~ c*wx3 + d*wy3
  wx5 ~ a*wx4 + b*wy4
  wy5 ~ c*wx4 + d*wy4
  
  # covariance between within-person centered variables at first wave
  wx1 ~~ wy1 # Covariance
  
  # covariances between residuals of within-person centered variables 
  wx2 ~~ cov*wy2
  wx3 ~~ cov*wy3
  wx4 ~~ cov*wy4 
  wx5 ~~ cov*wy5

  # variance and covariance of random intercepts
  RIx ~~ RIx
  RIy ~~ RIy
  RIx ~~ RIy

  # (residual) variance of within-person centered variables
  wx1 ~~ wx1 # Variance
  wy1 ~~ wy1 
  wx2 ~~ vx*wx2 # Residual variance
  wy2 ~~ vy*wy2 
  wx3 ~~ vx*wx3 
  wy3 ~~ vy*wy3 
  wx4 ~~ vx*wx4 
  wy4 ~~ vy*wy4 
  wx5 ~~ vx*wx5
  wy5 ~~ vy*wy5
'

RICLPM.fit.1 <- lavaan(RICLPM_1, 
                     data = dat, 
                     missing = 'FIML', 
                     meanstructure = T, 
                     int.ov.free = T)

RICLPM.fit.2 <- lavaan(RICLPM_2, 
                       data = dat, 
                       missing = 'FIML', 
                       meanstructure = T, 
                       int.ov.free = T)

summary(RICLPM.fit.1, standardized = T,fit.measures = T)
summary(RICLPM.fit.2, standardized = T,fit.measures = T)

standardizedsolution(RICLPM.fit.1, level=.95)
standardizedsolution(RICLPM.fit.2, level=.95)

# RICLPM (Positive affect)  -------------------------------------------------------------------

RICLPM_p1 <- '
  # between random intercepts
  RIx =~ 1*v_hap_19 + 1*v_hap_20 + 1*v_hap_21 + 1*v_hap_22 + 1*v_hap_23
  RIy =~ 1*p_19 + 1*p_20 + 1*p_21 + 1*p_22 + 1*p_23
  
  # within-person centered variables
  wx1 =~ 1*v_hap_19
  wx2 =~ 1*v_hap_20
  wx3 =~ 1*v_hap_21
  wx4 =~ 1*v_hap_22
  wx5 =~ 1*v_hap_23
  wy1 =~ 1*p_19
  wy2 =~ 1*p_20
  wy3 =~ 1*p_21
  wy4 =~ 1*p_22
  wy5 =~ 1*p_23

  # lagged effects
  wx2 ~ a*wx1 + b*wy1 
  wy2 ~ c*wx1 + d*wy1
  wx3 ~ a*wx2 + b*wy2
  wy3 ~ c*wx2 + d*wy2
  wx4 ~ a*wx3 + b*wy3
  wy4 ~ c*wx3 + d*wy3
  wx5 ~ a*wx4 + b*wy4
  wy5 ~ c*wx4 + d*wy4
  
  # covariance between within-person centered variables at first wave
  wx1 ~~ wy1 # Covariance
  
  # covariances between residuals of within-person centered variables 
  wx2 ~~ cov*wy2
  wx3 ~~ cov*wy3
  wx4 ~~ cov*wy4 
  wx5 ~~ cov*wy5

  # variance and covariance of random intercepts
  RIx ~~ RIx
  RIy ~~ RIy
  RIx ~~ RIy

  # (residual) variance of within-person centered variables
  wx1 ~~ wx1 # Variance
  wy1 ~~ wy1 
  wx2 ~~ vx*wx2 # Residual variance
  wy2 ~~ vy*wy2 
  wx3 ~~ vx*wx3 
  wy3 ~~ vy*wy3 
  wx4 ~~ vx*wx4 
  wy4 ~~ vy*wy4 
  wx5 ~~ vx*wx5
  wy5 ~~ vy*wy5
'

RICLPM_p2 <- '
  # between components (random intercepts)
  RIx =~ 1*v_hap_ip_19 + 1*v_hap_ip_20 + 1*v_hap_ip_21 + 1*v_hap_ip_22 + 1*v_hap_ip_23
  RIy =~ 1*p_19 + 1*p_20 + 1*p_21 + 1*p_22 + 1*p_23
  
  # within-person centered variables
  wx1 =~ 1*v_hap_ip_19
  wx2 =~ 1*v_hap_ip_20
  wx3 =~ 1*v_hap_ip_21
  wx4 =~ 1*v_hap_ip_22
  wx5 =~ 1*v_hap_ip_23
  wy1 =~ 1*p_19
  wy2 =~ 1*p_20
  wy3 =~ 1*p_21
  wy4 =~ 1*p_22
  wy5 =~ 1*p_23

  # lagged effects between within-person centered variables
  wx2 ~ a*wx1 + b*wy1 
  wy2 ~ c*wx1 + d*wy1
  wx3 ~ a*wx2 + b*wy2
  wy3 ~ c*wx2 + d*wy2
  wx4 ~ a*wx3 + b*wy3
  wy4 ~ c*wx3 + d*wy3
  wx5 ~ a*wx4 + b*wy4
  wy5 ~ c*wx4 + d*wy4
  
  # covariance between within-person centered variables at first wave
  wx1 ~~ wy1 # Covariance
  
  # covariances between residuals of within-person centered variables 
  wx2 ~~ cov*wy2
  wx3 ~~ cov*wy3
  wx4 ~~ cov*wy4 
  wx5 ~~ cov*wy5

  # variance and covariance of random intercepts
  RIx ~~ RIx
  RIy ~~ RIy
  RIx ~~ RIy

  # (residual) variance of within-person centered variables
  wx1 ~~ wx1 # Variance
  wy1 ~~ wy1 
  wx2 ~~ vx*wx2 # Residual variance
  wy2 ~~ vy*wy2 
  wx3 ~~ vx*wx3 
  wy3 ~~ vy*wy3 
  wx4 ~~ vx*wx4 
  wy4 ~~ vy*wy4 
  wx5 ~~ vx*wx5
  wy5 ~~ vy*wy5
'


RICLPM.fit.p1 <- lavaan(RICLPM_p1, 
                        data = dat, 
                        missing = 'FIML', 
                        meanstructure = T, 
                        int.ov.free = T)
RICLPM.fit.p2 <- lavaan(RICLPM_p2, 
                        data = dat, 
                        missing = 'FIML', 
                        meanstructure = T, 
                        int.ov.free = T)

summary(RICLPM.fit.p1, standardized = T,fit.measures = T)
summary(RICLPM.fit.p2, standardized = T,fit.measures = T)

standardizedsolution(RICLPM.fit.p1, level=.95)
standardizedsolution(RICLPM.fit.p2, level=.95)


# RICLPM (Negative affect)  -------------------------------------------------------------------

RICLPM_n1 <- '
  # between random intercepts
  RIx =~ 1*v_hap_19 + 1*v_hap_20 + 1*v_hap_21 + 1*v_hap_22 + 1*v_hap_23
  RIy =~ 1*n_19 + 1*n_20 + 1*n_21 + 1*n_22 + 1*n_23
  
  # within-person centered variables
  wx1 =~ 1*v_hap_19
  wx2 =~ 1*v_hap_20
  wx3 =~ 1*v_hap_21
  wx4 =~ 1*v_hap_22
  wx5 =~ 1*v_hap_23
  wy1 =~ 1*n_19
  wy2 =~ 1*n_20
  wy3 =~ 1*n_21
  wy4 =~ 1*n_22
  wy5 =~ 1*n_23

  # lagged effects
  wx2 ~ a*wx1 + b*wy1 
  wy2 ~ c*wx1 + d*wy1
  wx3 ~ a*wx2 + b*wy2
  wy3 ~ c*wx2 + d*wy2
  wx4 ~ a*wx3 + b*wy3
  wy4 ~ c*wx3 + d*wy3
  wx5 ~ a*wx4 + b*wy4
  wy5 ~ c*wx4 + d*wy4
  
  # covariance between within-person centered variables at first wave
  wx1 ~~ wy1 # Covariance
  
  # covariances between residuals of within-person centered variables 
  # (i.e., constrained)
  wx2 ~~ cov*wy2
  wx3 ~~ cov*wy3
  wx4 ~~ cov*wy4 
  wx5 ~~ cov*wy5

  # variance and covariance of random intercepts
  RIx ~~ RIx
  RIy ~~ RIy
  RIx ~~ RIy

  # (residual) variance of within-person centered variables
  wx1 ~~ wx1 # Variance
  wy1 ~~ wy1 
  wx2 ~~ vx*wx2 # Residual variance
  wy2 ~~ vy*wy2 
  wx3 ~~ vx*wx3 
  wy3 ~~ vy*wy3 
  wx4 ~~ vx*wx4 
  wy4 ~~ vy*wy4 
  wx5 ~~ vx*wx5
  wy5 ~~ vy*wy5
'

RICLPM_n2 <- '
  # between components (random intercepts)
  RIx =~ 1*v_hap_ip_19 + 1*v_hap_ip_20 + 1*v_hap_ip_21 + 1*v_hap_ip_22 + 1*v_hap_ip_23
  RIy =~ 1*n_19 + 1*n_20 + 1*n_21 + 1*n_22 + 1*n_23
  
  # within-person centered variables
  wx1 =~ 1*v_hap_ip_19
  wx2 =~ 1*v_hap_ip_20
  wx3 =~ 1*v_hap_ip_21
  wx4 =~ 1*v_hap_ip_22
  wx5 =~ 1*v_hap_ip_23
  wy1 =~ 1*n_19
  wy2 =~ 1*n_20
  wy3 =~ 1*n_21
  wy4 =~ 1*n_22
  wy5 =~ 1*n_23

  # lagged effects between within-person centered variables
  wx2 ~ a*wx1 + b*wy1 
  wy2 ~ c*wx1 + d*wy1
  wx3 ~ a*wx2 + b*wy2
  wy3 ~ c*wx2 + d*wy2
  wx4 ~ a*wx3 + b*wy3
  wy4 ~ c*wx3 + d*wy3
  wx5 ~ a*wx4 + b*wy4
  wy5 ~ c*wx4 + d*wy4
  
  # covariance between within-person centered variables at first wave
  wx1 ~~ wy1 # Covariance
  
  # covariances between residuals of within-person centered variables 
  wx2 ~~ cov*wy2
  wx3 ~~ cov*wy3
  wx4 ~~ cov*wy4 
  wx5 ~~ cov*wy5

  # variance and covariance of random intercepts
  RIx ~~ RIx
  RIy ~~ RIy
  RIx ~~ RIy

  # (residual) variance of within-person centered variables
  wx1 ~~ wx1 # Variance
  wy1 ~~ wy1 
  wx2 ~~ vx*wx2 # Residual variance
  wy2 ~~ vy*wy2 
  wx3 ~~ vx*wx3 
  wy3 ~~ vy*wy3 
  wx4 ~~ vx*wx4 
  wy4 ~~ vy*wy4 
  wx5 ~~ vx*wx5
  wy5 ~~ vy*wy5
'


RICLPM.fit.n1 <- lavaan(RICLPM_n1, 
                        data = dat, 
                        missing = 'FIML', 
                        meanstructure = T, 
                        int.ov.free = T)
RICLPM.fit.n2 <- lavaan(RICLPM_n2, 
                        data = dat, 
                        missing = 'FIML', 
                        meanstructure = T, 
                        int.ov.free = T)

summary(RICLPM.fit.n1, standardized = T,fit.measures = T)
summary(RICLPM.fit.n2, standardized = T,fit.measures = T)

standardizedsolution(RICLPM.fit.n1, level=.95)
standardizedsolution(RICLPM.fit.n2, level=.95)

# RICLPM with covariates -----------------------------------

RICLPM_1_adjusted = paste0(RICLPM_1,
                           "ls_19 + ls_20 + ls_21 + ls_22 + ls_23 ~ s1*female", "\n ",
                           "ls_19 + ls_20 + ls_21 + ls_22 + ls_23 ~ s2*age_group_40_60 + s3*age_group_60_80", "\n ",
                           "ls_19 + ls_20 + ls_21 + ls_22 + ls_23 ~ s4*college", "\n ")

RICLPM_2_adjusted = paste0(RICLPM_2,
                           "ls_19 + ls_20 + ls_21 + ls_22 + ls_23 ~ s1*female", "\n ",
                           "ls_19 + ls_20 + ls_21 + ls_22 + ls_23 ~ s2*age_group_40_60 + s3*age_group_60_80", "\n ",
                           "ls_19 + ls_20 + ls_21 + ls_22 + ls_23 ~ s4*college", "\n ")

RICLPM_p1_adjusted = paste0(RICLPM_p1,
                           "p_19 + p_20 + p_21 + p_22 + p_23 ~ s1*female", "\n ",
                           "p_19 + p_20 + p_21 + p_22 + p_23 ~ s2*age_group_40_60 + s3*age_group_60_80", "\n ",
                           "p_19 + p_20 + p_21 + p_22 + p_23 ~ s4*college", "\n ")

RICLPM_p2_adjusted = paste0(RICLPM_p2,
                            "p_19 + p_20 + p_21 + p_22 + p_23 ~ s1*female", "\n ",
                            "p_19 + p_20 + p_21 + p_22 + p_23 ~ s2*age_group_40_60 + s3*age_group_60_80", "\n ",
                            "p_19 + p_20 + p_21 + p_22 + p_23 ~ s4*college", "\n ")

RICLPM_n1_adjusted = paste0(RICLPM_n1,
                            "n_19 + n_20 + n_21 + n_22 + n_23 ~ s1*female", "\n ",
                            "n_19 + n_20 + n_21 + n_22 + n_23 ~ s2*age_group_40_60 + s3*age_group_60_80", "\n ",
                            "n_19 + n_20 + n_21 + n_22 + n_23 ~ s4*college", "\n ")

RICLPM_n2_adjusted = paste0(RICLPM_n2,
                            "n_19 + n_20 + n_21 + n_22 + n_23 ~ s1*female", "\n ",
                            "n_19 + n_20 + n_21 + n_22 + n_23 ~ s2*age_group_40_60 + s3*age_group_60_80", "\n ",
                            "n_19 + n_20 + n_21 + n_22 + n_23 ~ s4*college", "\n ")


RICLPM.fit.1.adjusted = lavaan(RICLPM_1_adjusted, 
                               data = dat, 
                               missing = 'FIML', 
                               meanstructure = T, 
                               int.ov.free = T)

RICLPM.fit.2.adjusted = lavaan(RICLPM_2_adjusted, 
                                data = dat, 
                                missing = 'FIML', 
                                meanstructure = T, 
                                int.ov.free = T)

RICLPM.fit.p1.adjusted = lavaan(RICLPM_p1_adjusted, 
                               data = dat, 
                               missing = 'FIML', 
                               meanstructure = T, 
                               int.ov.free = T)

RICLPM.fit.p2.adjusted = lavaan(RICLPM_p2_adjusted, 
                                data = dat, 
                                missing = 'FIML', 
                                meanstructure = T, 
                                int.ov.free = T)

RICLPM.fit.n1.adjusted = lavaan(RICLPM_n1_adjusted, 
                                data = dat, 
                                missing = 'FIML', 
                                meanstructure = T, 
                                int.ov.free = T)

RICLPM.fit.n2.adjusted = lavaan(RICLPM_n2_adjusted, 
                                 data = dat, 
                                 missing = 'FIML', 
                                 meanstructure = T, 
                                 int.ov.free = T)

summary(RICLPM.fit.1.adjusted, standardized = T,fit.measures = T)
summary(RICLPM.fit.2.adjusted, standardized = T,fit.measures = T)
summary(RICLPM.fit.p1.adjusted, standardized = T,fit.measures = T)
summary(RICLPM.fit.p2.adjusted, standardized = T,fit.measures = T)
summary(RICLPM.fit.n1.adjusted, standardized = T,fit.measures = T)
summary(RICLPM.fit.n2.adjusted, standardized = T,fit.measures = T)

standardizedsolution(RICLPM.fit.1.adjusted, level = .95)
standardizedsolution(RICLPM.fit.2.adjusted, level = .95)
standardizedsolution(RICLPM.fit.p1.adjusted, level = .95)
standardizedsolution(RICLPM.fit.p2.adjusted, level = .95)
standardizedsolution(RICLPM.fit.n1.adjusted, level = .95)
standardizedsolution(RICLPM.fit.n2.adjusted, level = .95)


# Multe-group SEM --------------------------------------------------------------

### You can test different moderator by changing lavaan(...group = "female")
### to other variables including O_cate, C_cate, E_cate, A_cate, N_cate

# life satisfaction (two groups) 

ls_raw_MG_free = "
  # between random intercepts
  RIx =~ 1*v_hap_19 + 1*v_hap_20 + 1*v_hap_21 + 1*v_hap_22 + 1*v_hap_23
  RIy =~ 1*ls_19 + 1*ls_20 + 1*ls_21 + 1*ls_22 + 1*ls_23
  
  # within-person centered variables
  wx1 =~ 1*v_hap_19
  wx2 =~ 1*v_hap_20
  wx3 =~ 1*v_hap_21
  wx4 =~ 1*v_hap_22
  wx5 =~ 1*v_hap_23
  wy1 =~ 1*ls_19
  wy2 =~ 1*ls_20
  wy3 =~ 1*ls_21
  wy4 =~ 1*ls_22
  wy5 =~ 1*ls_23

  # lagged effects
  wx2 ~ c(a1, a2)*wx1 + c(b1, b2)*wy1 
  wy2 ~ c(c1, c2)*wx1 + c(d1, d2)*wy1
  wx3 ~ c(a1, a2)*wx2 + c(b1, b2)*wy2
  wy3 ~ c(c1, c2)*wx2 + c(d1, d2)*wy2
  wx4 ~ c(a1, a2)*wx3 + c(b1, b2)*wy3
  wy4 ~ c(c1, c2)*wx3 + c(d1, d2)*wy3
  wx5 ~ c(a1, a2)*wx4 + c(b1, b2)*wy4
  wy5 ~ c(c1, c2)*wx4 + c(d1, d2)*wy4
  
  # covariance between within-person centered variables at first wave
  wx1 ~~ wy1 # Covariance
  
  # covariances between residuals of within-person centered variables 
  wx2 ~~ c(cov1, cov2)*wy2
  wx3 ~~ c(cov1, cov2)*wy3
  wx4 ~~ c(cov1, cov2)*wy4 
  wx5 ~~ c(cov1, cov2)*wy5
  
  # variance and covariance of random intercepts
  RIx ~~ RIx
  RIy ~~ RIy
  RIx ~~ c(int1, int2)*RIy

  # (residual) variance of within-person centered variables
  wx1 ~~ wx1 # Variance
  wy1 ~~ wy1 
  wx2 ~~ c(vx1, vx2)*wx2 # Residual variance
  wy2 ~~ c(vy1, vy2)*wy2 
  wx3 ~~ c(vx1, vx2)*wx3 
  wy3 ~~ c(vy1, vy2)*wy3 
  wx4 ~~ c(vx1, vx2)*wx4 
  wy4 ~~ c(vy1, vy2)*wy4 
  wx5 ~~ c(vx1, vx2)*wx5
  wy5 ~~ c(vy1, vy2)*wy5
"

ls_raw_MG_fixed_int = paste0(ls_raw_MG_free,
                              "int1 == int2", "\n ")
ls_raw_MG_fixed_crosslag = paste0(ls_raw_MG_free,
                                      "b1 == b2", "\n ",
                                      "c1 == c2", "\n ")
ls_raw_MG_fixed_int_crosslag = paste0(ls_raw_MG_fixed_int,
                                      "b1 == b2", "\n ",
                                      "c1 == c2", "\n ")

fit.ls_raw_MG_free = lavaan(ls_raw_MG_free, 
                            data = dat, 
                            missing = 'FIML',
                            group = "female", 
                            meanstructure = T, 
                            int.ov.free = T)

fit.ls_raw_MG_fixed_int = lavaan(ls_raw_MG_fixed_int, 
                                  data = dat, 
                                  missing = 'FIML',
                                  group = "female", 
                                  meanstructure = T, 
                                  int.ov.free = T)

fit.ls_raw_MG_fixed_crosslag = lavaan(ls_raw_MG_fixed_crosslag, 
                                      data = dat, 
                                      missing = 'FIML',
                                      group = "female", 
                                      meanstructure = T, 
                                      int.ov.free = T)
  

anova(fit.ls_raw_MG_free,fit.ls_raw_MG_fixed_int)
anova(fit.ls_raw_MG_free,fit.ls_raw_MG_fixed_crosslag)


# life satisfaction (three groups)

ls_raw_MG_free_3group = "
  # between random intercepts
  RIx =~ 1*v_hap_19 + 1*v_hap_20 + 1*v_hap_21 + 1*v_hap_22 + 1*v_hap_23
  RIy =~ 1*ls_19 + 1*ls_20 + 1*ls_21 + 1*ls_22 + 1*ls_23
  
  # within-person centered variables
  wx1 =~ 1*v_hap_19
  wx2 =~ 1*v_hap_20
  wx3 =~ 1*v_hap_21
  wx4 =~ 1*v_hap_22
  wx5 =~ 1*v_hap_23
  wy1 =~ 1*ls_19
  wy2 =~ 1*ls_20
  wy3 =~ 1*ls_21
  wy4 =~ 1*ls_22
  wy5 =~ 1*ls_23

  # lagged effects
  wx2 ~ c(a1, a2, a3)*wx1 + c(b1, b2, b3)*wy1 
  wy2 ~ c(c1, c2, c3)*wx1 + c(d1, d2, d3)*wy1
  wx3 ~ c(a1, a2, a3)*wx2 + c(b1, b2, b3)*wy2
  wy3 ~ c(c1, c2, c3)*wx2 + c(d1, d2, d3)*wy2
  wx4 ~ c(a1, a2, a3)*wx3 + c(b1, b2, b3)*wy3
  wy4 ~ c(c1, c2, c3)*wx3 + c(d1, d2, d3)*wy3
  wx5 ~ c(a1, a2, a3)*wx4 + c(b1, b2, b3)*wy4
  wy5 ~ c(c1, c2, c3)*wx4 + c(d1, d2, d3)*wy4
  
  # covariance between within-person centered variables at first wave
  wx1 ~~ wy1 # Covariance
  
  # covariances between residuals of within-person centered variables 
  # (i.e., constrained)
  wx2 ~~ c(cov1, cov2, cov3)*wy2
  wx3 ~~ c(cov1, cov2, cov3)*wy3
  wx4 ~~ c(cov1, cov2, cov3)*wy4 
  wx5 ~~ c(cov1, cov2, cov3)*wy5
  
  # variance and covariance of random intercepts
  RIx ~~ RIx
  RIy ~~ RIy
  RIx ~~ c(int1, int2, int3)*RIy

  # (residual) variance of within-person centered variables
  wx1 ~~ wx1 # Variance
  wy1 ~~ wy1 
  wx2 ~~ c(vx1, vx2, vx3)*wx2 # Residual variance
  wy2 ~~ c(vy1, vy2, vx3)*wy2 
  wx3 ~~ c(vx1, vx2, vx3)*wx3 
  wy3 ~~ c(vy1, vy2, vx3)*wy3 
  wx4 ~~ c(vx1, vx2, vx3)*wx4 
  wy4 ~~ c(vy1, vy2, vx3)*wy4 
  wx5 ~~ c(vx1, vx2, vx3)*wx5
  wy5 ~~ c(vy1, vy2, vx3)*wy5
"

ls_raw_MG_fixed_int_3group = paste0(ls_raw_MG_free_3group,
                             "int1 == int2", "\n ",
                             "int1 == int3", "\n ")
ls_raw_MG_fixed_crosslag_3group = paste0(ls_raw_MG_free_3group,
                                  "b1 == b2", "\n ",
                                  "b1 == b3", "\n ",
                                  "c1 == c2", "\n ",
                                  "c1 == c3", "\n ")
ls_raw_MG_fixed_int_crosslag_3group = paste0(ls_raw_MG_fixed_int_3group,
                                             "b1 == b2", "\n ",
                                             "b1 == b3", "\n ",
                                             "c1 == c2", "\n ",
                                             "c1 == c3", "\n ")

fit.ls_raw_MG_free_3group = lavaan(ls_raw_MG_free_3group, 
                            data = dat, 
                            missing = 'FIML',
                            group = "age_group", 
                            meanstructure = T, 
                            int.ov.free = T)

fit.ls_raw_MG_fixed_int_3group = lavaan(ls_raw_MG_fixed_int_3group, 
                                 data = dat, 
                                 missing = 'FIML',
                                 group = "age_group", 
                                 meanstructure = T, 
                                 int.ov.free = T)

fit.ls_raw_MG_fixed_crosslag_3group = lavaan(ls_raw_MG_fixed_crosslag_3group, 
                                      data = dat, 
                                      missing = 'FIML',
                                      group = "age_group", 
                                      meanstructure = T, 
                                      int.ov.free = T)

fit.ls_raw_MG_fixed_int_crosslag_3group = lavaan(ls_raw_MG_fixed_int_crosslag_3group, 
                                          data = dat, 
                                          missing = 'FIML',
                                          group = "age_group", 
                                          meanstructure = T, 
                                          int.ov.free = T)



anova(fit.ls_raw_MG_free_3group,fit.ls_raw_MG_fixed_int_3group)
anova(fit.ls_raw_MG_free_3group,fit.ls_raw_MG_fixed_crosslag_3group)


# positve affect (two groups)

p_raw_MG_free = "
  # between random intercepts
  RIx =~ 1*v_hap_19 + 1*v_hap_20 + 1*v_hap_21 + 1*v_hap_22 + 1*v_hap_23
  RIy =~ 1*p_19 + 1*p_20 + 1*p_21 + 1*p_22 + 1*p_23
  
  # within-person centered variables
  wx1 =~ 1*v_hap_19
  wx2 =~ 1*v_hap_20
  wx3 =~ 1*v_hap_21
  wx4 =~ 1*v_hap_22
  wx5 =~ 1*v_hap_23
  wy1 =~ 1*p_19
  wy2 =~ 1*p_20
  wy3 =~ 1*p_21
  wy4 =~ 1*p_22
  wy5 =~ 1*p_23

  # lagged effects
  wx2 ~ c(a1, a2)*wx1 + c(b1, b2)*wy1 
  wy2 ~ c(c1, c2)*wx1 + c(d1, d2)*wy1
  wx3 ~ c(a1, a2)*wx2 + c(b1, b2)*wy2
  wy3 ~ c(c1, c2)*wx2 + c(d1, d2)*wy2
  wx4 ~ c(a1, a2)*wx3 + c(b1, b2)*wy3
  wy4 ~ c(c1, c2)*wx3 + c(d1, d2)*wy3
  wx5 ~ c(a1, a2)*wx4 + c(b1, b2)*wy4
  wy5 ~ c(c1, c2)*wx4 + c(d1, d2)*wy4
  
  # covariance between within-person centered variables at first wave
  wx1 ~~ wy1 # Covariance
  
  # covariances between residuap of within-person centered variables 
  wx2 ~~ c(cov1, cov2)*wy2
  wx3 ~~ c(cov1, cov2)*wy3
  wx4 ~~ c(cov1, cov2)*wy4 
  wx5 ~~ c(cov1, cov2)*wy5
  
  # variance and covariance of random intercepts
  RIx ~~ RIx
  RIy ~~ RIy
  RIx ~~ c(int1, int2)*RIy

  # (residual) variance of within-person centered variables
  wx1 ~~ wx1 # Variance
  wy1 ~~ wy1 
  wx2 ~~ c(vx1, vx2)*wx2 # Residual variance
  wy2 ~~ c(vy1, vy2)*wy2 
  wx3 ~~ c(vx1, vx2)*wx3 
  wy3 ~~ c(vy1, vy2)*wy3 
  wx4 ~~ c(vx1, vx2)*wx4 
  wy4 ~~ c(vy1, vy2)*wy4 
  wx5 ~~ c(vx1, vx2)*wx5
  wy5 ~~ c(vy1, vy2)*wy5
"

p_raw_MG_fixed_int = paste0(p_raw_MG_free,
                            "int1 == int2", "\n ")
p_raw_MG_fixed_crosslag = paste0(p_raw_MG_free,
                                 "b1 == b2", "\n ",
                                 "c1 == c2", "\n ")
p_raw_MG_fixed_int_crosslag = paste0(p_raw_MG_fixed_int,
                                     "b1 == b2", "\n ",
                                     "c1 == c2", "\n ")

fit.p_raw_MG_free = lavaan(p_raw_MG_free, 
                           data = dat, 
                           missing = 'FIML',
                           group = "female", 
                           meanstructure = T, 
                           int.ov.free = T)

fit.p_raw_MG_fixed_int = lavaan(p_raw_MG_fixed_int, 
                                data = dat, 
                                missing = 'FIML',
                                group = "female", 
                                meanstructure = T, 
                                int.ov.free = T)

fit.p_raw_MG_fixed_crosslag = lavaan(p_raw_MG_fixed_crosslag, 
                                     data = dat, 
                                     missing = 'FIML',
                                     group = "female", 
                                     meanstructure = T, 
                                     int.ov.free = T)


anova(fit.p_raw_MG_free,fit.p_raw_MG_fixed_int)
anova(fit.p_raw_MG_free,fit.p_raw_MG_fixed_crosslag)


# positve affect (three groups)

p_raw_MG_free_3group = "
  # between random intercepts
  RIx =~ 1*v_hap_19 + 1*v_hap_20 + 1*v_hap_21 + 1*v_hap_22 + 1*v_hap_23
  RIy =~ 1*p_19 + 1*p_20 + 1*p_21 + 1*p_22 + 1*p_23
  
  # within-person centered variables
  wx1 =~ 1*v_hap_19
  wx2 =~ 1*v_hap_20
  wx3 =~ 1*v_hap_21
  wx4 =~ 1*v_hap_22
  wx5 =~ 1*v_hap_23
  wy1 =~ 1*p_19
  wy2 =~ 1*p_20
  wy3 =~ 1*p_21
  wy4 =~ 1*p_22
  wy5 =~ 1*p_23

  # lagged effects
  wx2 ~ c(a1, a2, a3)*wx1 + c(b1, b2, b3)*wy1 
  wy2 ~ c(c1, c2, c3)*wx1 + c(d1, d2, d3)*wy1
  wx3 ~ c(a1, a2, a3)*wx2 + c(b1, b2, b3)*wy2
  wy3 ~ c(c1, c2, c3)*wx2 + c(d1, d2, d3)*wy2
  wx4 ~ c(a1, a2, a3)*wx3 + c(b1, b2, b3)*wy3
  wy4 ~ c(c1, c2, c3)*wx3 + c(d1, d2, d3)*wy3
  wx5 ~ c(a1, a2, a3)*wx4 + c(b1, b2, b3)*wy4
  wy5 ~ c(c1, c2, c3)*wx4 + c(d1, d2, d3)*wy4
  
  # covariance between within-person centered variables at first wave
  wx1 ~~ wy1 # Covariance
  
  # covariances between residuap of within-person centered variables 
  # (i.e., constrained)
  wx2 ~~ c(cov1, cov2, cov3)*wy2
  wx3 ~~ c(cov1, cov2, cov3)*wy3
  wx4 ~~ c(cov1, cov2, cov3)*wy4 
  wx5 ~~ c(cov1, cov2, cov3)*wy5
  
  # variance and covariance of random intercepts
  RIx ~~ RIx
  RIy ~~ RIy
  RIx ~~ c(int1, int2, int3)*RIy

  # (residual) variance of within-person centered variables
  wx1 ~~ wx1 # Variance
  wy1 ~~ wy1 
  wx2 ~~ c(vx1, vx2, vx3)*wx2 # Residual variance
  wy2 ~~ c(vy1, vy2, vx3)*wy2 
  wx3 ~~ c(vx1, vx2, vx3)*wx3 
  wy3 ~~ c(vy1, vy2, vx3)*wy3 
  wx4 ~~ c(vx1, vx2, vx3)*wx4 
  wy4 ~~ c(vy1, vy2, vx3)*wy4 
  wx5 ~~ c(vx1, vx2, vx3)*wx5
  wy5 ~~ c(vy1, vy2, vx3)*wy5
"

p_raw_MG_fixed_int_3group = paste0(p_raw_MG_free_3group,
                                   "int1 == int2", "\n ",
                                   "int1 == int3", "\n ")
p_raw_MG_fixed_crosslag_3group = paste0(p_raw_MG_free_3group,
                                        "b1 == b2", "\n ",
                                        "b1 == b3", "\n ",
                                        "c1 == c2", "\n ",
                                        "c1 == c3", "\n ")
p_raw_MG_fixed_int_crosslag_3group = paste0(p_raw_MG_fixed_int_3group,
                                            "b1 == b2", "\n ",
                                            "b1 == b3", "\n ",
                                            "c1 == c2", "\n ",
                                            "c1 == c3", "\n ")

fit.p_raw_MG_free_3group = lavaan(p_raw_MG_free_3group, 
                                  data = dat, 
                                  missing = 'FIML',
                                  group = "age_group", 
                                  meanstructure = T, 
                                  int.ov.free = T)

fit.p_raw_MG_fixed_int_3group = lavaan(p_raw_MG_fixed_int_3group, 
                                       data = dat, 
                                       missing = 'FIML',
                                       group = "age_group", 
                                       meanstructure = T, 
                                       int.ov.free = T)

fit.p_raw_MG_fixed_crosslag_3group = lavaan(p_raw_MG_fixed_crosslag_3group, 
                                            data = dat, 
                                            missing = 'FIML',
                                            group = "age_group", 
                                            meanstructure = T, 
                                            int.ov.free = T)

fit.p_raw_MG_fixed_int_crosslag_3group = lavaan(p_raw_MG_fixed_int_crosslag_3group, 
                                                data = dat, 
                                                missing = 'FIML',
                                                group = "age_group", 
                                                meanstructure = T, 
                                                int.ov.free = T)



anova(fit.p_raw_MG_free_3group,fit.p_raw_MG_fixed_int_3group)
anova(fit.p_raw_MG_free_3group,fit.p_raw_MG_fixed_crosslag_3group)


# negative affect (two groups)

n_ip_MG_free = "
  # between random intercepts
  RIx =~ 1*v_hap_ip_19 + 1*v_hap_ip_20 + 1*v_hap_ip_21 + 1*v_hap_ip_22 + 1*v_hap_ip_23
  RIy =~ 1*n_19 + 1*n_20 + 1*n_21 + 1*n_22 + 1*n_23
  
  # within-person centered variables
  wx1 =~ 1*v_hap_ip_19
  wx2 =~ 1*v_hap_ip_20
  wx3 =~ 1*v_hap_ip_21
  wx4 =~ 1*v_hap_ip_22
  wx5 =~ 1*v_hap_ip_23
  wy1 =~ 1*n_19
  wy2 =~ 1*n_20
  wy3 =~ 1*n_21
  wy4 =~ 1*n_22
  wy5 =~ 1*n_23

  # lagged effects
  wx2 ~ c(a1, a2)*wx1 + c(b1, b2)*wy1 
  wy2 ~ c(c1, c2)*wx1 + c(d1, d2)*wy1
  wx3 ~ c(a1, a2)*wx2 + c(b1, b2)*wy2
  wy3 ~ c(c1, c2)*wx2 + c(d1, d2)*wy2
  wx4 ~ c(a1, a2)*wx3 + c(b1, b2)*wy3
  wy4 ~ c(c1, c2)*wx3 + c(d1, d2)*wy3
  wx5 ~ c(a1, a2)*wx4 + c(b1, b2)*wy4
  wy5 ~ c(c1, c2)*wx4 + c(d1, d2)*wy4
  
  # covariance between within-person centered variables at first wave
  wx1 ~~ wy1 # Covariance
  
  # covariances between residuan of within-person centered variables 
  wx2 ~~ c(cov1, cov2)*wy2
  wx3 ~~ c(cov1, cov2)*wy3
  wx4 ~~ c(cov1, cov2)*wy4 
  wx5 ~~ c(cov1, cov2)*wy5
  
  # variance and covariance of random intercepts
  RIx ~~ RIx
  RIy ~~ RIy
  RIx ~~ c(int1, int2)*RIy

  # (residual) variance of within-person centered variables
  wx1 ~~ wx1 # Variance
  wy1 ~~ wy1 
  wx2 ~~ c(vx1, vx2)*wx2 # Residual variance
  wy2 ~~ c(vy1, vy2)*wy2 
  wx3 ~~ c(vx1, vx2)*wx3 
  wy3 ~~ c(vy1, vy2)*wy3 
  wx4 ~~ c(vx1, vx2)*wx4 
  wy4 ~~ c(vy1, vy2)*wy4 
  wx5 ~~ c(vx1, vx2)*wx5
  wy5 ~~ c(vy1, vy2)*wy5
"

n_ip_MG_fixed_int = paste0(n_ip_MG_free,
                           "int1 == int2", "\n ")
n_ip_MG_fixed_crosslag = paste0(n_ip_MG_free,
                                "b1 == b2", "\n ",
                                "c1 == c2", "\n ")
n_ip_MG_fixed_int_crosslag = paste0(n_ip_MG_fixed_int,
                                    "b1 == b2", "\n ",
                                    "c1 == c2", "\n ")

fit.n_ip_MG_free = lavaan(n_ip_MG_free, 
                          data = dat, 
                          missing = 'FIML',
                          group = "female", 
                          meanstructure = T, 
                          int.ov.free = T)

fit.n_ip_MG_fixed_int = lavaan(n_ip_MG_fixed_int, 
                               data = dat, 
                               missing = 'FIML',
                               group = "female", 
                               meanstructure = T, 
                               int.ov.free = T)

fit.n_ip_MG_fixed_crosslag = lavaan(n_ip_MG_fixed_crosslag, 
                                    data = dat, 
                                    missing = 'FIML',
                                    group = "female", 
                                    meanstructure = T, 
                                    int.ov.free = T)

anova(fit.n_ip_MG_free,fit.n_ip_MG_fixed_int)
anova(fit.n_ip_MG_free,fit.n_ip_MG_fixed_crosslag)


# negative affect (three groups)

n_ip_MG_free_3group = "
  # between random intercepts
  RIx =~ 1*v_hap_ip_19 + 1*v_hap_ip_20 + 1*v_hap_ip_21 + 1*v_hap_ip_22 + 1*v_hap_ip_23
  RIy =~ 1*n_19 + 1*n_20 + 1*n_21 + 1*n_22 + 1*n_23
  
  # within-person centered variables
  wx1 =~ 1*v_hap_ip_19
  wx2 =~ 1*v_hap_ip_20
  wx3 =~ 1*v_hap_ip_21
  wx4 =~ 1*v_hap_ip_22
  wx5 =~ 1*v_hap_ip_23
  wy1 =~ 1*n_19
  wy2 =~ 1*n_20
  wy3 =~ 1*n_21
  wy4 =~ 1*n_22
  wy5 =~ 1*n_23

  # lagged effects
  wx2 ~ c(a1, a2, a3)*wx1 + c(b1, b2, b3)*wy1 
  wy2 ~ c(c1, c2, c3)*wx1 + c(d1, d2, d3)*wy1
  wx3 ~ c(a1, a2, a3)*wx2 + c(b1, b2, b3)*wy2
  wy3 ~ c(c1, c2, c3)*wx2 + c(d1, d2, d3)*wy2
  wx4 ~ c(a1, a2, a3)*wx3 + c(b1, b2, b3)*wy3
  wy4 ~ c(c1, c2, c3)*wx3 + c(d1, d2, d3)*wy3
  wx5 ~ c(a1, a2, a3)*wx4 + c(b1, b2, b3)*wy4
  wy5 ~ c(c1, c2, c3)*wx4 + c(d1, d2, d3)*wy4
  
  # covariance between within-person centered variables at first wave
  wx1 ~~ wy1 # Covariance
  
  # covariances between residuan of within-person centered variables 
  # (i.e., constrained)
  wx2 ~~ c(cov1, cov2, cov3)*wy2
  wx3 ~~ c(cov1, cov2, cov3)*wy3
  wx4 ~~ c(cov1, cov2, cov3)*wy4 
  wx5 ~~ c(cov1, cov2, cov3)*wy5
  
  # variance and covariance of random intercepts
  RIx ~~ RIx
  RIy ~~ RIy
  RIx ~~ c(int1, int2, int3)*RIy

  # (residual) variance of within-person centered variables
  wx1 ~~ wx1 # Variance
  wy1 ~~ wy1 
  wx2 ~~ c(vx1, vx2, vx3)*wx2 # Residual variance
  wy2 ~~ c(vy1, vy2, vx3)*wy2 
  wx3 ~~ c(vx1, vx2, vx3)*wx3 
  wy3 ~~ c(vy1, vy2, vx3)*wy3 
  wx4 ~~ c(vx1, vx2, vx3)*wx4 
  wy4 ~~ c(vy1, vy2, vx3)*wy4 
  wx5 ~~ c(vx1, vx2, vx3)*wx5
  wy5 ~~ c(vy1, vy2, vx3)*wy5
"

n_ip_MG_fixed_int_3group = paste0(n_ip_MG_free_3group,
                                  "int1 == int2", "\n ",
                                  "int1 == int3", "\n ")
n_ip_MG_fixed_crosslag_3group = paste0(n_ip_MG_free_3group,
                                       "b1 == b2", "\n ",
                                       "b1 == b3", "\n ",
                                       "c1 == c2", "\n ",
                                       "c1 == c3", "\n ")
n_ip_MG_fixed_int_crosslag_3group = paste0(n_ip_MG_fixed_int_3group,
                                           "b1 == b2", "\n ",
                                           "b1 == b3", "\n ",
                                           "c1 == c2", "\n ",
                                           "c1 == c3", "\n ")


fit.n_ip_MG_free_3group = lavaan(n_ip_MG_free_3group, 
                                 data = dat, 
                                 missing = 'FIML',
                                 group = "age_group", 
                                 meanstructure = T, 
                                 int.ov.free = T)

fit.n_ip_MG_fixed_int_3group = lavaan(n_ip_MG_fixed_int_3group, 
                                      data = dat, 
                                      missing = 'FIML',
                                      group = "age_group", 
                                      meanstructure = T, 
                                      int.ov.free = T)

fit.n_ip_MG_fixed_crosslag_3group = lavaan(n_ip_MG_fixed_crosslag_3group, 
                                           data = dat, 
                                           missing = 'FIML',
                                           group = "age_group", 
                                           meanstructure = T, 
                                           int.ov.free = T)

fit.n_ip_MG_fixed_int_crosslag_3group = lavaan(n_ip_MG_fixed_int_crosslag_3group, 
                                               data = dat, 
                                               missing = 'FIML',
                                               group = "age_group", 
                                               meanstructure = T, 
                                               int.ov.free = T)

anova(fit.n_ip_MG_free_3group,fit.n_ip_MG_fixed_int_3group)
anova(fit.n_ip_MG_free_3group,fit.n_ip_MG_fixed_crosslag_3group)


# Fixed effects model -----------------------------------------------------------

dat_long = dat %>% select(ID,
                          v_hap_19,v_hap_20,v_hap_21,v_hap_22,v_hap_23,
                          wb_19,wb_20,wb_21,wb_22,wb_23,
                          p_19,p_20,p_21,p_22,p_23,
                          n_19,n_20,n_21,n_22,n_23) %>%
  pivot_longer(
    cols = c(-ID),
    names_to = c(".value", "year"),
    names_pattern = "(.*)_(..)"
  )

library(lm.beta)

m1 = lm(wb ~ v_hap + ID,dat_long)
m2 = lm(p ~ v_hap + ID,dat_long)
m3 = lm(n ~ v_hap + ID,dat_long)

tidy(lm.beta(m1),x, conf.int = TRUE, conf.level = 0.95)
tidy(lm.beta(m2),x, conf.int = TRUE, conf.level = 0.95)
tidy(lm.beta(m3),x, conf.int = TRUE, conf.level = 0.95)
