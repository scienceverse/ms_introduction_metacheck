### EFFECT SIZE CALCULATIONS FOR MIXED EFFECTS CONDITION COMPARISONS ###

# Calculated by dividing the estimated difference over the squareroot of the sum of variance components #
# (estimated diff/sqrt(var.intercept.subject+var.intercept.scenario.+residual)) #

#### STUDY 1 ####

## MORALITY ##

# Interaction #
## raw interaction parameter +/- std. err of parameter (returns lower/upper 95% bounds for parameter)
kvs1_moral_int_lower<-(-.640-(1.96*.094)) # lower bound raw diff
kvs1_moral_int_raw<- -.640
kvs1_moral_int_upper<-(-.640+(1.96*.094)) # upper bound raw diff

# No Choice (Stranger v Kin)#
## raw No Choice parameter (from emmeans output) +/- std. err of parameter (returns lower/upper 95% bounds for parameter)
(kvs1_moral_nc_lower<-(.199-(1.96*.066))) # lower bound raw diff
(kvs1_moral_nc_raw<-.199) # observed raw diff
(kvs1_moral_nc_upper<-(.199+(1.96*.066))) # upper bound raw diff
## d and 95% CI calculations for No Choice parameter 
(kvs1_moral_nc_lower/(sqrt(.37698+.04606+.91343))) # lower bound d
(kvs1_moral_nc_raw/(sqrt(.37698+.04606+.91343))) # observed d
(kvs1_moral_nc_upper/(sqrt(.37698+.04606+.91343))) # upper bound d

# Choice (Stranger v Kin)#
## raw Choice parameter (from emmeans output) +/- std. err of parameter (returns lower/upper 95% bounds for parameter)
(kvs1_moral_c_lower<-(-.442-(1.96*.066))) # lower bound raw diff
(kvs1_moral_c_raw<- -.422) # observed raw diff
(kvs1_moral_c_upper<-(-.442+(1.96*.066))) # upper bound raw diff
## d and 95% CI calcuations for Choice parameter
(kvs1_moral_c_lower/(sqrt(.37698+.04606+.91343))) # lower bound d
(kvs1_moral_c_raw/(sqrt(.37698+.04606+.91343))) # observed d
(kvs1_moral_c_upper/(sqrt(.37698+.04606+.91343))) # upper bound d


## TRUSTWORTHINESS ##

# Interaction #
(kvs1_trust_int_lower<-(-.772-(1.96*.094)))
(kvs1_trust_int_raw<- -.772)
(kvs1_trust_int_upper<- (-.772+(1.96*.094)))

# No Choice #
## raw diff
(kvs1_trust_nc_lower<- (.213-(1.96*.067)))
(kvs1_trust_nc_raw<- .213)
(kvs1_trust_nc_upper<- (.213+(1.96*.067)))
## d
(kvs1_trust_nc_lower/sqrt(.40804+.03976+.92962))
(kvs1_trust_nc_raw/sqrt(.40804+.03976+.92962))
(kvs1_trust_nc_upper/sqrt(.40804+.03976+.92962))

# Choice #
## raw diffs
(kvs1_trust_c_lower<- (-.559-(1.96*.067)))
(kvs1_trust_c_raw<- -.559)
(kvs1_trust_c_upper<- (-.559+(1.96*.067)))
# d
(kvs1_trust_c_lower/sqrt(.40804+.03976+.92962))
(kvs1_trust_c_raw/sqrt(.40804+.03976+.92962))
(kvs1_trust_c_upper/sqrt(.40804+.03976+.92962))


## WRONGNESS ##

# Interaction #
(kvs1_wrong_int_lower<-(.844-(1.96*.105)))
(kvs1_wrong_int_raw<- .844)
(kvs1_wrong_int_upper<- (.844+(1.96*.105)))

# No Choice #
## raw diffs
(kvs1_wrong_nc_lower<- (-.217-(1.96*.067)))
(kvs1_wrong_nc_raw<- -.217)
(kvs1_wrong_nc_upper<- (-.217+(1.96*.067)))
# d
(kvs1_wrong_nc_lower/sqrt(.58698+.07004+1.15225))
(kvs1_wrong_nc_raw/sqrt(.58698+.07004+1.15225))
(kvs1_wrong_nc_upper/sqrt(.58698+.07004+1.15225))

# Choice #
## raw diffs
(kvs1_wrong_c_lower<- (.627-(1.96*.067)))
(kvs1_wrong_c_raw<- .627)
(kvs1_wrong_c_upper<- (.627+(1.96*.067)))
# d
(kvs1_wrong_c_lower/sqrt(.58698+.07004+1.15225))
(kvs1_wrong_c_raw/sqrt(.58698+.07004+1.15225))
(kvs1_wrong_c_upper/sqrt(.58698+.07004+1.15225))


## DIAGNOSTICTY ##

# Interaction #
(kvs1_diag_int_lower<-(-.453-(1.96*.086)))
(kvs1_diag_int_raw<- -.453)
(kvs1_diag_int_upper<- (-.453+(1.96*.086)))

# No Choice #
## raw diffs
(kvs1_diag_nc_lower<- (.247-(1.96*.061)))
(kvs1_diag_nc_raw<- .247)
(kvs1_diag_nc_upper<- (.247+(1.96*.061)))
# d
(kvs1_diag_nc_lower/sqrt(.58698+.07004+1.15225))
(kvs1_diag_nc_raw/sqrt(.58698+.07004+1.15225))
(kvs1_diag_nc_upper/sqrt(.58698+.07004+1.15225))

# Choice #
## raw diffs
(kvs1_diag_c_lower<- (-.205-(1.96*.061)))
(kvs1_diag_c_raw<- -.205)
(kvs1_diag_c_upper<- (-.205+(1.96*.061)))
# d
(kvs1_diag_c_lower/sqrt(.58698+.07004+1.15225))
(kvs1_diag_c_raw/sqrt(.58698+.07004+1.15225))
(kvs1_diag_c_upper/sqrt(.58698+.07004+1.15225))



#### STUDY 2 ####


## MORALITY ##

# Interaction #
## raw interaction parameter +/- std. err of parameter (returns lower/upper 95% bounds for parameter)
(kvs2_moral_int_lower<-(-.532-(1.96*.073))) # lower bound raw diff
(kvs2_moral_int_raw<- -.532) # raw diff
(kvs2_moral_int_upper<-(-.532+(1.96*.073))) # upper bound raw diff

# No Choice (Stranger v Kin)#
## raw No Choice parameter (from emmeans output) +/- std. err of parameter (returns lower/upper 95% bounds for parameter)
(kvs2_moral_nc_lower<-(.221-(1.96*.052))) # lower bound raw diff
(kvs2_moral_nc_raw<-.221) # observed raw diff
(kvs2_moral_nc_upper<-(.221+(1.96*.052))) # upper bound raw diff
## d and 95% CI calculations for No Choice parameter 
(kvs2_moral_nc_lower/(sqrt(.27503+.02227+.51316))) # lower bound d
(kvs2_moral_nc_raw/(sqrt(.27503+.02227+.51316))) # observed d
(kvs2_moral_nc_upper/(sqrt(.27503+.02227+.51316))) # upper bound d

# Choice (Stranger v Kin)#
## raw Choice parameter (from emmeans output) +/- std. err of parameter (returns lower/upper 95% bounds for parameter)
(kvs2_moral_c_lower<-(-.311-(1.96*.052))) # lower bound raw diff
(kvs2_moral_c_raw<- -.311) # observed raw diff
(kvs2_moral_c_upper<-(-.311+(1.96*.052))) # upper bound raw diff
## d and 95% CI calcuations for Choice parameter
(kvs2_moral_c_lower/(sqrt(.27503+.02227+.51316))) # lower bound d
(kvs2_moral_c_raw/(sqrt(.27503+.02227+.51316))) # observed d
(kvs2_moral_c_upper/(sqrt(.27503+.02227+.51316))) # upper bound d


## TRUSTWORTHINESS ##

# Interaction #
## raw interaction parameter +/- std. err of parameter (returns lower/upper 95% bounds for parameter)
(kvs2_trust_int_lower<-(-.518-(1.96*.073))) # lower bound raw diff
(kvs2_trust_int_raw<- -.518) # raw diff
(kvs2_trust_int_upper<-(-.518+(1.96*.073))) # upper bound raw diff

# No Choice (Stranger v Kin)#
## raw No Choice parameter (from emmeans output) +/- std. err of parameter (returns lower/upper 95% bounds for parameter)
(kvs2_trust_nc_lower<-(.129-(1.96*.052))) # lower bound raw diff
(kvs2_trust_nc_raw<-.129) # observed raw diff
(kvs2_trust_nc_upper<-(.129+(1.96*.052))) # upper bound raw diff
## d and 95% CI calculations for No Choice parameter 
(kvs2_trust_nc_lower/(sqrt(.28713+.01243+.51621))) # lower bound d
(kvs2_trust_nc_raw/(sqrt(.28713+.01243+.51621))) # observed d
(kvs2_trust_nc_upper/(sqrt(.28713+.01243+.51621))) # upper bound d

# Choice (Stranger v Kin)#
## raw Choice parameter (from emmeans output) +/- std. err of parameter (returns lower/upper 95% bounds for parameter)
(kvs2_trust_c_lower<-(-.389-(1.96*.052))) # lower bound raw diff
(kvs2_trust_c_raw<- -.389) # observed raw diff
(kvs2_trust_c_upper<-(-.389+(1.96*.052))) # upper bound raw diff
## d and 95% CI calcuations for Choice parameter
(kvs2_trust_c_lower/(sqrt(.28713+.01243+.51621))) # lower bound d
(kvs2_trust_c_raw/(sqrt(.28713+.01243+.51621))) # observed d
(kvs2_trust_c_upper/(sqrt(.28713+.01243+.51621))) # upper bound d


## HARM ##

# Interaction #
## raw interaction parameter +/- std. err of parameter (returns lower/upper 95% bounds for parameter)
(kvs2_harm_int_lower<-(.175-(1.96*.059))) # lower bound raw diff
(kvs2_harm_int_raw<- .175) # raw diff
(kvs2_harm_int_upper<-(.175+(1.96*.059))) # upper bound raw diff

# No Choice (Stranger v Kin)#
## raw No Choice parameter (from emmeans output) +/- std. err of parameter (returns lower/upper 95% bounds for parameter)
(kvs2_harm_nc_lower<-(-.076-(1.96*.042))) # lower bound raw diff
(kvs2_harm_nc_raw<- -.076) # observed raw diff
(kvs2_harm_nc_upper<-(-.076+(1.96*.042))) # upper bound raw diff
## d and 95% CI calculations for No Choice parameter 
(kvs2_harm_nc_lower/(sqrt(.19483+.01573+.33844))) # lower bound d
(kvs2_harm_nc_raw/(sqrt(.19483+.01573+.33844))) # observed d
(kvs2_harm_nc_upper/(sqrt(.19483+.01573+.33844))) # upper bound d

# Choice (Stranger v Kin)#
## raw Choice parameter (from emmeans output) +/- std. err of parameter (returns lower/upper 95% bounds for parameter)
(kvs2_harm_c_lower<-(.099-(1.96*.042))) # lower bound raw diff
(kvs2_harm_c_raw<- .099) # observed raw diff
(kvs2_harm_c_upper<-(.099+(1.96*.042))) # upper bound raw diff
## d and 95% CI calcuations for Choice parameter
(kvs2_harm_c_lower/(sqrt(.19483+.01573+.33844))) # lower bound d
(kvs2_harm_c_raw/(sqrt(.19483+.01573+.33844))) # observed d
(kvs2_harm_c_upper/(sqrt(.19483+.01573+.33844))) # upper bound d


## UNPREDICTABILITY ##

# Interaction #
## raw interaction parameter +/- std. err of parameter (returns lower/upper 95% bounds for parameter)
(kvs2_unpred_int_lower<-(.706-(1.96*.089))) # lower bound raw diff
(kvs2_unpred_int_raw<- .706) # raw diff
(kvs2_unpred_int_upper<-(.706+(1.96*.089))) # upper bound raw diff

# No Choice (Stranger v Kin)#
## raw No Choice parameter (from emmeans output) +/- std. err of parameter (returns lower/upper 95% bounds for parameter)
(kvs2_unpred_nc_lower<-(.279-(1.96*.063))) # lower bound raw diff
(kvs2_unpred_nc_raw<- .279) # observed raw diff
(kvs2_unpred_nc_upper<-(.279+(1.96*.063))) # upper bound raw diff
## d and 95% CI calculations for No Choice parameter 
(kvs2_unpred_nc_lower/(sqrt(.29755+.04479+.75203))) # lower bound d
(kvs2_unpred_nc_raw/(sqrt(.29755+.04479+.75203))) # observed d
(kvs2_unpred_nc_upper/(sqrt(.29755+.04479+.75203))) # upper bound d

# Choice (Stranger v Kin)#
## raw Choice parameter (from emmeans output) +/- std. err of parameter (returns lower/upper 95% bounds for parameter)
(kvs2_unpred_c_lower<-(.985-(1.96*.063))) # lower bound raw diff
(kvs2_unpred_c_raw<- .985) # observed raw diff
(kvs2_unpred_c_upper<-(.985+(1.96*.063))) # upper bound raw diff
## d and 95% CI calcuations for Choice parameter
(kvs2_unpred_c_lower/(sqrt(.29755+.04479+.75203))) # lower bound d
(kvs2_unpred_c_raw/(sqrt(.29755+.04479+.75203))) # observed d
(kvs2_unpred_c_upper/(sqrt(.29755+.04479+.75203))) # upper bound d


## TYPICALITY ##

# Interaction #
## raw interaction parameter +/- std. err of parameter (returns lower/upper 95% bounds for parameter)
(kvs2_typ_int_lower<-(-.388-(1.96*.084))) # lower bound raw diff
(kvs2_typ_int_raw<- -.388) # raw diff
(kvs2_typ_int_upper<-(-.388+(1.96*.084))) # upper bound raw diff

# No Choice (Stranger v Kin)#
## raw No Choice parameter (from emmeans output) +/- std. err of parameter (returns lower/upper 95% bounds for parameter)
(kvs2_typ_nc_lower<-(-.641-(1.96*.060))) # lower bound raw diff
(kvs2_typ_nc_raw<- -.641) # observed raw diff
(kvs2_typ_nc_upper<-(-.641+(1.96*.060))) # upper bound raw diff
## d and 95% CI calculations for No Choice parameter 
(kvs2_typ_nc_lower/(sqrt(.2515+.1005+.6796))) # lower bound d
(kvs2_typ_nc_raw/(sqrt(.2515+.1005+.6796))) # observed d
(kvs2_typ_nc_upper/(sqrt(.2515+.1005+.6796))) # upper bound d

# Choice (Stranger v Kin)#
## raw Choice parameter (from emmeans output) +/- std. err of parameter (returns lower/upper 95% bounds for parameter)
(kvs2_typ_c_lower<-(-1.028-(1.96*.060))) # lower bound raw diff
(kvs2_typ_c_raw<- -1.028) # observed raw diff
(kvs2_typ_c_upper<-(-1.028+(1.96*.060))) # upper bound raw diff
## d and 95% CI calcuations for Choice parameter
(kvs2_typ_c_lower/(sqrt(.2515+.1005+.6796))) # lower bound d
(kvs2_typ_c_raw/(sqrt(.2515+.1005+.6796))) # observed d
(kvs2_typ_c_upper/(sqrt(.2515+.1005+.6796))) # upper bound d


#### STUDY 3 ####


## MORALITY ##

# Interaction (No Choice vs Choice) #
## raw interaction parameter +/- std. err of parameter (returns lower/upper 95% bounds for parameter)
(kvs3_moral_int1_lower<-(-.536-(1.96*.103))) # lower bound raw diff
(kvs3_moral_int1_raw<- -.536) # raw diff
(kvs3_moral_int1_upper<-(-.536+(1.96*.103))) # upper bound raw diff

# Interaction (Choice vs Failed) # 
## raw interaction parameter +/- std. err of parameter (returns lower/upper 95% bounds for parameter)
(kvs3_moral_int2_lower<-(-.589-(1.96*.103))) # lower bound raw diff
(kvs3_moral_int2_raw<- -.589) # raw diff
(kvs3_moral_int2_upper<-(-.589+(1.96*.103))) # upper bound raw diff

# No Choice (Stranger v Kin)#
## raw No Choice parameter (from emmeans output) +/- std. err of parameter (returns lower/upper 95% bounds for parameter)
(kvs3_moral_nc_lower<-(.098-(1.96*.073))) # lower bound raw diff
(kvs3_moral_nc_raw<-.098) # observed raw diff
(kvs3_moral_nc_upper<-(.098+(1.96*.073))) # upper bound raw diff
## d and 95% CI calculations for No Choice parameter 
(kvs3_moral_nc_lower/(sqrt(.09569+.01817+.80948))) # lower bound d
(kvs3_moral_nc_raw/(sqrt(.09569+.01817+.80948))) # observed d
(kvs3_moral_nc_upper/(sqrt(.09569+.01817+.80948))) # upper bound d

# Choice (Stranger v Kin)#
## raw Choice parameter (from emmeans output) +/- std. err of parameter (returns lower/upper 95% bounds for parameter)
(kvs3_moral_c_lower<-(-.438-(1.96*.073))) # lower bound raw diff
(kvs3_moral_c_raw<- -.438) # observed raw diff
(kvs3_moral_c_upper<-(-.438+(1.96*.073))) # upper bound raw diff
## d and 95% CI calcuations for Choice parameter
(kvs3_moral_c_lower/(sqrt(.09569+.01817+.80948))) # lower bound d
(kvs3_moral_c_raw/(sqrt(.09569+.01817+.80948))) # observed d
(kvs3_moral_c_upper/(sqrt(.09569+.01817+.80948))) # upper bound d

# Failed (Stranger v Kin) #
## raw Failed parameter (from emmeans output) +/- std. err of parameter (returns lower/upper 95% bounds for parameter)
(kvs3_moral_f_lower<-(.151-(1.96*.073))) # lower bound raw diff
(kvs3_moral_f_raw<- .151) # observed raw diff
(kvs3_moral_f_upper<-(.151+(1.96*.073))) # upper bound raw diff
## d and 95% CI calcuations for Choice parameter
(kvs3_moral_f_lower/(sqrt(.09569+.01817+.80948))) # lower bound d
(kvs3_moral_f_raw/(sqrt(.09569+.01817+.80948))) # observed d
(kvs3_moral_f_upper/(sqrt(.09569+.01817+.80948))) # upper bound d

# Stranger Over Kin vs Failed Kin #
## raw SOK vs FHK parameter
(kvs3_moral_SOKvFHK_lower<-(.960-(1.96*.073))) # lower bound raw diff
(kvs3_moral_SOKvFHK_raw<- .960) # observed raw diff
(kvs3_moral_SOKvFHK_upper<-(.960+(1.96*.073))) # upper bound raw diff
# d
(kvs3_moral_SOKvFHK_lower/(sqrt(.09569+.01817+.80948))) # lower bound d
(kvs3_moral_SOKvFHK_raw/(sqrt(.09569+.01817+.80948))) # observed d
(kvs3_moral_SOKvFHK_upper/(sqrt(.09569+.01817+.80948))) # upper bound d


## UNPREDICTABILITY ##

# Interaction (No Choice vs Choice) #
## raw interaction parameter +/- std. err of parameter (returns lower/upper 95% bounds for parameter)
(kvs3_unpred_int1_lower<-(.753-(1.96*.110))) # lower bound raw diff
(kvs3_unpred_int1_raw<- .753) # raw diff
(kvs3_unpred_int1_upper<-(.753+(1.96*.110))) # upper bound raw diff

# Interaction (Choice vs Failed) # 
## raw interaction parameter +/- std. err of parameter (returns lower/upper 95% bounds for parameter)
(kvs3_unpred_int2_lower<-(1.29-(1.96*.110))) # lower bound raw diff
(kvs3_unpred_int2_raw<- 1.29) # raw diff
(kvs3_unpred_int2_upper<-(1.29+(1.96*.110))) # upper bound raw diff

# No Choice (Stranger v Kin)#
## raw No Choice parameter (from emmeans output) +/- std. err of parameter (returns lower/upper 95% bounds for parameter)
(kvs3_unpred_nc_lower<-(.366-(1.96*.078))) # lower bound raw diff
(kvs3_unpred_nc_raw<-.366) # observed raw diff
(kvs3_unpred_nc_upper<-(.366+(1.96*.078))) # upper bound raw diff
## d and 95% CI calculations for No Choice parameter 
(kvs3_unpred_nc_lower/(sqrt(.26632+.02559+.91474))) # lower bound d
(kvs3_unpred_nc_raw/(sqrt(.26632+.02559+.91474))) # observed d
(kvs3_unpred_nc_upper/(sqrt(.26632+.02559+.91474))) # upper bound d

# Choice (Stranger v Kin)#
## raw Choice parameter (from emmeans output) +/- std. err of parameter (returns lower/upper 95% bounds for parameter)
(kvs3_unpred_c_lower<-(1.12-(1.96*.078))) # lower bound raw diff
(kvs3_unpred_c_raw<- 1.12) # observed raw diff
(kvs3_unpred_c_upper<-(1.12+(1.96*.078))) # upper bound raw diff
## d and 95% CI calcuations for Choice parameter
(kvs3_unpred_c_lower/(sqrt(.26632+.02559+.91474))) # lower bound d
(kvs3_unpred_c_raw/(sqrt(.26632+.02559+.91474))) # observed d
(kvs3_unpred_c_upper/(sqrt(.26632+.02559+.91474))) # upper bound d

# Failed (Stranger v Kin) #
## raw Failed parameter (from emmeans output) +/- std. err of parameter (returns lower/upper 95% bounds for parameter)
(kvs3_unpred_f_lower<-(-.169-(1.96*.078))) # lower bound raw diff
(kvs3_unpred_f_raw<- -.169) # observed raw diff
(kvs3_unpred_f_upper<-(-.169+(1.96*.078))) # upper bound raw diff
## d and 95% CI calcuations for Choice parameter
(kvs3_unpred_f_lower/(sqrt(.26632+.02559+.91474))) # lower bound d
(kvs3_unpred_f_raw/(sqrt(.26632+.02559+.91474))) # observed d
(kvs3_unpred_f_upper/(sqrt(.26632+.02559+.91474))) # upper bound d

# Stranger Over Kin vs Failed Kin #
## raw SOK vs FHK parameter
(kvs3_unpred_SOKvFHK_lower<-(.619-(1.96*.078))) # lower bound raw diff
(kvs3_unpred_SOKvFHK_raw<- .619) # observed raw diff
(kvs3_unpred_SOKvFHK_upper<-(.619+(1.96*.078))) # upper bound raw diff
# d
(kvs3_unpred_SOKvFHK_lower/(sqrt(.26632+.02559+.91474))) # lower bound d
(kvs3_unpred_SOKvFHK_raw/(sqrt(.26632+.02559+.91474))) # observed d
(kvs3_unpred_SOKvFHK_upper/(sqrt(.26632+.02559+.91474))) # upper bound d



#### STUDY 4 ####

## MORALITY ##

# Interaction (No Choice vs Choice) #
## raw interaction parameter +/- std. err of parameter (returns lower/upper 95% bounds for parameter)
(kvs4_moral_int1_lower<-(-.643-(1.96*.164))) # lower bound raw diff
(kvs4_moral_int1_raw<- -.643) # raw diff
(kvs4_moral_int1_upper<-(-.643+(1.96*.164))) # upper bound raw diff

# Interaction (Choice vs Failed) # 
## raw interaction parameter +/- std. err of parameter (returns lower/upper 95% bounds for parameter)
(kvs4_moral_int2_lower<-(-1.057-(1.96*.164))) # lower bound raw diff
(kvs4_moral_int2_raw<- -1.057) # raw diff
(kvs4_moral_int2_upper<-(-1.057+(1.96*.164))) # upper bound raw diff

# No Choice (Stranger v Kin)#
## raw No Choice parameter (from emmeans output) +/- std. err of parameter (returns lower/upper 95% bounds for parameter)
(kvs4_moral_nc_lower<-(.102-(1.96*.117))) # lower bound raw diff
(kvs4_moral_nc_raw<-.102) # observed raw diff
(kvs4_moral_nc_upper<-(.102+(1.96*.117))) # upper bound raw diff
## d and 95% CI calculations for No Choice parameter 
(kvs4_moral_nc_lower/(sqrt(.25367+.03304+2.05704))) # lower bound d
(kvs4_moral_nc_raw/(sqrt(.25367+.03304+2.05704))) # observed d
(kvs4_moral_nc_upper/(sqrt(.25367+.03304+2.05704))) # upper bound d

# Choice (Stranger v Kin)#
## raw Choice parameter (from emmeans output) +/- std. err of parameter (returns lower/upper 95% bounds for parameter)
(kvs4_moral_c_lower<-(-.541-(1.96*.116))) # lower bound raw diff
(kvs4_moral_c_raw<- -.541) # observed raw diff
(kvs4_moral_c_upper<-(-.541+(1.96*.116))) # upper bound raw diff
## d and 95% CI calcuations for Choice parameter
(kvs4_moral_c_lower/(sqrt(.25367+.03304+2.05704))) # lower bound d
(kvs4_moral_c_raw/(sqrt(.25367+.03304+2.05704))) # observed d
(kvs4_moral_c_upper/(sqrt(.25367+.03304+2.05704))) # upper bound d

# Failed (Stranger v Kin) #
## raw Failed parameter (from emmeans output) +/- std. err of parameter (returns lower/upper 95% bounds for parameter)
(kvs4_moral_f_lower<-(.516-(1.96*.116))) # lower bound raw diff
(kvs4_moral_f_raw<- .516) # observed raw diff
(kvs4_moral_f_upper<-(.516+(1.96*.116))) # upper bound raw diff
## d and 95% CI calcuations for Choice parameter
(kvs4_moral_f_lower/(sqrt(.25367+.03304+2.05704))) # lower bound d
(kvs4_moral_f_raw/(sqrt(.25367+.03304+2.05704))) # observed d
(kvs4_moral_f_upper/(sqrt(.25367+.03304+2.05704))) # upper bound d

# Stranger Over Kin vs Failed Kin #
## raw SOK vs FHK parameter
(kvs4_moral_SOKvFHK_lower<-(1.522-(1.96*.116))) # lower bound raw diff
(kvs4_moral_SOKvFHK_raw<- 1.522) # observed raw diff
(kvs4_moral_SOKvFHK_upper<-(1.522+(1.96*.116))) # upper bound raw diff
# d
(kvs4_moral_SOKvFHK_lower/(sqrt(.25367+.03304+2.05704))) # lower bound d
(kvs4_moral_SOKvFHK_raw/(sqrt(.25367+.03304+2.05704))) # observed d
(kvs4_moral_SOKvFHK_upper/(sqrt(.25367+.03304+2.05704))) # upper bound d


## TRUSTWORTHINESS ##

# Interaction (No Choice vs Choice) #
## raw interaction parameter +/- std. err of parameter (returns lower/upper 95% bounds for parameter)
(kvs4_trust_int1_lower<-(-.922-(1.96*.175))) # lower bound raw diff
(kvs4_trust_int1_raw<- -.922) # raw diff
(kvs4_trust_int1_upper<-(-.922+(1.96*.175))) # upper bound raw diff

# Interaction (Choice vs Failed) # 
## raw interaction parameter +/- std. err of parameter (returns lower/upper 95% bounds for parameter)
(kvs4_trust_int2_lower<-(-1.598-(1.96*.175))) # lower bound raw diff
(kvs4_trust_int2_raw<- -1.598) # raw diff
(kvs4_trust_int2_upper<-(-1.498+(1.96*.175))) # upper bound raw diff

# No Choice (Stranger v Kin)#
## raw No Choice parameter (from emmeans output) +/- std. err of parameter (returns lower/upper 95% bounds for parameter)
(kvs4_trust_nc_lower<-(.022-(1.96*.124))) # lower bound raw diff
(kvs4_trust_nc_raw<-.022) # observed raw diff
(kvs4_trust_nc_upper<-(.022+(1.96*.124))) # upper bound raw diff
## d and 95% CI calculations for No Choice parameter 
(kvs4_trust_nc_lower/(sqrt(.26821+.01292+2.33846))) # lower bound d
(kvs4_trust_nc_raw/(sqrt(.26821+.01292+2.33846))) # observed d
(kvs4_trust_nc_upper/(sqrt(.26821+.01292+2.33846))) # upper bound d

# Choice (Stranger v Kin)#
## raw Choice parameter (from emmeans output) +/- std. err of parameter (returns lower/upper 95% bounds for parameter)
(kvs4_trust_c_lower<-(-.901-(1.96*.124))) # lower bound raw diff
(kvs4_trust_c_raw<- -.901) # observed raw diff
(kvs4_trust_c_upper<-(-.901+(1.96*.124))) # upper bound raw diff
## d and 95% CI calcuations for Choice parameter
(kvs4_trust_c_lower/(sqrt(.26821+.01292+2.33846))) # lower bound d
(kvs4_trust_c_raw/(sqrt(.26821+.01292+2.33846))) # observed d
(kvs4_trust_c_upper/(sqrt(.26821+.01292+2.33846))) # upper bound d

# Failed (Stranger v Kin) #
## raw Failed parameter (from emmeans output) +/- std. err of parameter (returns lower/upper 95% bounds for parameter)
(kvs4_trust_f_lower<-(.698-(1.96*.124))) # lower bound raw diff
(kvs4_trust_f_raw<- .698) # observed raw diff
(kvs4_trust_f_upper<-(.698+(1.96*.124))) # upper bound raw diff
## d and 95% CI calcuations for Choice parameter
(kvs4_trust_f_lower/(sqrt(.26821+.01292+2.33846))) # lower bound d
(kvs4_trust_f_raw/(sqrt(.26821+.01292+2.33846))) # observed d
(kvs4_trust_f_upper/(sqrt(.26821+.01292+2.33846))) # upper bound d

# Stranger Over Kin vs Failed Kin #
## raw SOK vs FHK parameter
(kvs4_trust_SOKvFHK_lower<-(1.062-(1.96*.124))) # lower bound raw diff
(kvs4_trust_SOKvFHK_raw<- 1.062) # observed raw diff
(kvs4_trust_SOKvFHK_upper<-(1.062+(1.96*.124))) # upper bound raw diff
# d
(kvs4_trust_SOKvFHK_lower/(sqrt(.26821+.01292+2.33846))) # lower bound d
(kvs4_trust_SOKvFHK_raw/(sqrt(.26821+.01292+2.33846))) # observed d
(kvs4_trust_SOKvFHK_upper/(sqrt(.26821+.01292+2.33846))) # upper bound d


## OBLIGATION ##

# Interaction (No Choice vs Choice) #
## raw interaction parameter +/- std. err of parameter (returns lower/upper 95% bounds for parameter)
(kvs4_oblig_int1_lower<-(-.704-(1.96*.186))) # lower bound raw diff
(kvs4_oblig_int1_raw<- -.704) # raw diff
(kvs4_oblig_int1_upper<-(-.704+(1.96*.186))) # upper bound raw diff

# Interaction (Choice vs Failed) # 
## raw interaction parameter +/- std. err of parameter (returns lower/upper 95% bounds for parameter)
(kvs4_oblig_int2_lower<-(-1.648-(1.96*.186))) # lower bound raw diff
(kvs4_oblig_int2_raw<- -1.648) # raw diff
(kvs4_oblig_int2_upper<-(-1.648+(1.96*.186))) # upper bound raw diff

# No Choice (Stranger v Kin)#
## raw No Choice parameter (from emmeans output) +/- std. err of parameter (returns lower/upper 95% bounds for parameter)
(kvs4_oblig_nc_lower<-(-.364-(1.96*.132))) # lower bound raw diff
(kvs4_oblig_nc_raw<- -.364) # observed raw diff
(kvs4_oblig_nc_upper<-(-.364+(1.96*.132))) # upper bound raw diff
## d and 95% CI calculations for No Choice parameter 
(kvs4_oblig_nc_lower/(sqrt(.288914+.008866+2.647826))) # lower bound d
(kvs4_oblig_nc_raw/(sqrt(.288914+.008866+2.647826))) # observed d
(kvs4_oblig_nc_upper/(sqrt(.288914+.008866+2.647826))) # upper bound d

# Choice (Stranger v Kin)#
## raw Choice parameter (from emmeans output) +/- std. err of parameter (returns lower/upper 95% bounds for parameter)
(kvs4_oblig_c_lower<-(-1.068-(1.96*.132))) # lower bound raw diff
(kvs4_oblig_c_raw<- -1.068) # observed raw diff
(kvs4_oblig_c_upper<-(-1.068+(1.96*.132))) # upper bound raw diff
## d and 95% CI calcuations for Choice parameter
(kvs4_oblig_c_lower/(sqrt(.288914+.008866+2.647826))) # lower bound d
(kvs4_oblig_c_raw/(sqrt(.288914+.008866+2.647826))) # observed d
(kvs4_oblig_c_upper/(sqrt(.288914+.008866+2.647826))) # upper bound d

# Failed (Stranger v Kin) #
## raw Failed parameter (from emmeans output) +/- std. err of parameter (returns lower/upper 95% bounds for parameter)
(kvs4_oblig_f_lower<-(.580-(1.96*.132))) # lower bound raw diff
(kvs4_oblig_f_raw<- .580) # observed raw diff
(kvs4_oblig_f_upper<-(.580+(1.96*.132))) # upper bound raw diff
## d and 95% CI calcuations for Choice parameter
(kvs4_oblig_f_lower/(sqrt(.288914+.008866+2.647826))) # lower bound d
(kvs4_oblig_f_raw/(sqrt(.288914+.008866+2.647826))) # observed d
(kvs4_oblig_f_upper/(sqrt(.288914+.008866+2.647826))) # upper bound d

# Stranger Over Kin vs Failed Kin #
## raw SOK vs FHK parameter
(kvs4_oblig_SOKvFHK_lower<-(.898-(1.96*.132))) # lower bound raw diff
(kvs4_oblig_SOKvFHK_raw<- .898) # observed raw diff
(kvs4_oblig_SOKvFHK_upper<-(.898+(1.96*.132))) # upper bound raw diff
# d
(kvs4_oblig_SOKvFHK_lower/(sqrt(.288914+.008866+2.647826))) # lower bound d
(kvs4_oblig_SOKvFHK_raw/(sqrt(.288914+.008866+2.647826))) # observed d
(kvs4_oblig_SOKvFHK_upper/(sqrt(.288914+.008866+2.647826))) # upper bound d


## RIGHTNESSS ##

# Interaction (No Choice vs Choice) #
## raw interaction parameter +/- std. err of parameter (returns lower/upper 95% bounds for parameter)
(kvs4_right_int1_lower<-(-.954-(1.96*.188))) # lower bound raw diff
(kvs4_right_int1_raw<- -.954) # raw diff
(kvs4_right_int1_upper<-(-.954+(1.96*.188))) # upper bound raw diff

# Interaction (Choice vs Failed) # 
## raw interaction parameter +/- std. err of parameter (returns lower/upper 95% bounds for parameter)
(kvs4_right_int2_lower<-(-1.689-(1.96*.188))) # lower bound raw diff
(kvs4_right_int2_raw<- -1.689) # raw diff
(kvs4_right_int2_upper<-(-1.689+(1.96*.188))) # upper bound raw diff

# No Choice (Stranger v Kin)#
## raw No Choice parameter (from emmeans output) +/- std. err of parameter (returns lower/upper 95% bounds for parameter)
(kvs4_right_nc_lower<-(.056-(1.96*.133))) # lower bound raw diff
(kvs4_right_nc_raw<- .056) # observed raw diff
(kvs4_right_nc_upper<-(.056+(1.96*.133))) # upper bound raw diff
## d and 95% CI calculations for No Choice parameter 
(kvs4_right_nc_lower/(sqrt(.33638+.03551+2.69772))) # lower bound d
(kvs4_right_nc_raw/(sqrt(.33638+.03551+2.69772))) # observed d
(kvs4_right_nc_upper/(sqrt(.33638+.03551+2.69772))) # upper bound d

# Choice (Stranger v Kin)#
## raw Choice parameter (from emmeans output) +/- std. err of parameter (returns lower/upper 95% bounds for parameter)
(kvs4_right_c_lower<-(-.899-(1.96*.133))) # lower bound raw diff
(kvs4_right_c_raw<- -.899) # observed raw diff
(kvs4_right_c_upper<-(-.899+(1.96*.133))) # upper bound raw diff
## d and 95% CI calcuations for Choice parameter
(kvs4_right_c_lower/(sqrt(.33638+.03551+2.69772))) # lower bound d
(kvs4_right_c_raw/(sqrt(.33638+.03551+2.69772))) # observed d
(kvs4_right_c_upper/(sqrt(.33638+.03551+2.69772))) # upper bound d

# Failed (Stranger v Kin) #
## raw Failed parameter (from emmeans output) +/- std. err of parameter (returns lower/upper 95% bounds for parameter)
(kvs4_right_f_lower<-(.790-(1.96*.133))) # lower bound raw diff
(kvs4_right_f_raw<- .790) # observed raw diff
(kvs4_right_f_upper<-(.790+(1.96*.133))) # upper bound raw diff
## d and 95% CI calcuations for Choice parameter
(kvs4_right_f_lower/(sqrt(.33638+.03551+2.69772))) # lower bound d
(kvs4_right_f_raw/(sqrt(.33638+.03551+2.69772))) # observed d
(kvs4_right_f_upper/(sqrt(.33638+.03551+2.69772))) # upper bound d

# Stranger Over Kin vs Failed Kin #
## raw SOK vs FHK parameter
(kvs4_right_SOKvFHK_lower<-(1.307-(1.96*.133))) # lower bound raw diff
(kvs4_right_SOKvFHK_raw<- 1.307) # observed raw diff
(kvs4_right_SOKvFHK_upper<-(1.307+(1.96*.133))) # upper bound raw diff
# d
(kvs4_right_SOKvFHK_lower/(sqrt(.33638+.03551+2.69772))) # lower bound d
(kvs4_right_SOKvFHK_raw/(sqrt(.33638+.03551+2.69772))) # observed d
(kvs4_right_SOKvFHK_upper/(sqrt(.33638+.03551+2.69772))) # upper bound d


## PRAISEWORTHINESS ##

# Interaction (No Choice vs Choice) #
## raw interaction parameter +/- std. err of parameter (returns lower/upper 95% bounds for parameter)
(kvs4_praise_int1_lower<-(-.685-(1.96*.177))) # lower bound raw diff
(kvs4_praise_int1_raw<- -.685) # raw diff
(kvs4_praise_int1_upper<-(-.685+(1.96*.177))) # upper bound raw diff

# Interaction (Choice vs Failed) # 
## raw interaction parameter +/- std. err of parameter (returns lower/upper 95% bounds for parameter)
(kvs4_praise_int2_lower<-(-1.060-(1.96*.177))) # lower bound raw diff
(kvs4_praise_int2_raw<- -1.060) # raw diff
(kvs4_praise_int2_upper<-(-1.060+(1.96*.177))) # upper bound raw diff

# No Choice (Stranger v Kin)#
## raw No Choice parameter (from emmeans output) +/- std. err of parameter (returns lower/upper 95% bounds for parameter)
(kvs4_praise_nc_lower<-(.138-(1.96*.126))) # lower bound raw diff
(kvs4_praise_nc_raw<- .138) # observed raw diff
(kvs4_praise_nc_upper<-(.138+(1.96*.126))) # upper bound raw diff
## d and 95% CI calculations for No Choice parameter 
(kvs4_praise_nc_lower/(sqrt(.28352+.01821+2.38571))) # lower bound d
(kvs4_praise_nc_raw/(sqrt(.28352+.01821+2.38571))) # observed d
(kvs4_praise_nc_upper/(sqrt(.28352+.01821+2.38571))) # upper bound d

# Choice (Stranger v Kin)#
## raw Choice parameter (from emmeans output) +/- std. err of parameter (returns lower/upper 95% bounds for parameter)
(kvs4_praise_c_lower<-(-.548-(1.96*.125))) # lower bound raw diff
(kvs4_praise_c_raw<- -.548) # observed raw diff
(kvs4_praise_c_upper<-(-.548+(1.96*.125))) # upper bound raw diff
## d and 95% CI calcuations for Choice parameter
(kvs4_praise_c_lower/(sqrt(.28352+.01821+2.38571))) # lower bound d
(kvs4_praise_c_raw/(sqrt(.28352+.01821+2.38571))) # observed d
(kvs4_praise_c_upper/(sqrt(.28352+.01821+2.38571))) # upper bound d

# Failed (Stranger v Kin) #
## raw Failed parameter (from emmeans output) +/- std. err of parameter (returns lower/upper 95% bounds for parameter)
(kvs4_praise_f_lower<-(.512-(1.96*.125))) # lower bound raw diff
(kvs4_praise_f_raw<- .512) # observed raw diff
(kvs4_praise_f_upper<-(.512+(1.96*.125))) # upper bound raw diff
## d and 95% CI calcuations for Choice parameter
(kvs4_praise_f_lower/(sqrt(.28352+.01821+2.38571))) # lower bound d
(kvs4_praise_f_raw/(sqrt(.28352+.01821+2.38571))) # observed d
(kvs4_praise_f_upper/(sqrt(.28352+.01821+2.38571))) # upper bound d

# Stranger Over Kin vs Failed Kin #
## raw SOK vs FHK parameter
(kvs4_praise_SOKvFHK_lower<-(1.550-(1.96*.125))) # lower bound raw diff
(kvs4_praise_SOKvFHK_raw<- 1.550) # observed raw diff
(kvs4_praise_SOKvFHK_upper<-(1.550+(1.96*.125))) # upper bound raw diff
# d
(kvs4_praise_SOKvFHK_lower/(sqrt(.28352+.01821+2.38571))) # lower bound d
(kvs4_praise_SOKvFHK_raw/(sqrt(.28352+.01821+2.38571))) # observed d
(kvs4_praise_SOKvFHK_upper/(sqrt(.28352+.01821+2.38571))) # upper bound d


## FAIRNESS ##

# Interaction (No Choice vs Choice) #
## raw interaction parameter +/- std. err of parameter (returns lower/upper 95% bounds for parameter)
(kvs4_fair_int1_lower<-(-1.045-(1.96*.201))) # lower bound raw diff
(kvs4_fair_int1_raw<- -1.045) # raw diff
(kvs4_fair_int1_upper<-(-1.045+(1.96*.201))) # upper bound raw diff

# Interaction (Choice vs Failed) # 
## raw interaction parameter +/- std. err of parameter (returns lower/upper 95% bounds for parameter)
(kvs4_fair_int2_lower<-(-1.847-(1.96*.201))) # lower bound raw diff
(kvs4_fair_int2_raw<- -1.847) # raw diff
(kvs4_fair_int2_upper<-(-1.847+(1.96*.201))) # upper bound raw diff

# No Choice (Stranger v Kin)#
## raw No Choice parameter (from emmeans output) +/- std. err of parameter (returns lower/upper 95% bounds for parameter)
(kvs4_fair_nc_lower<-(.010-(1.96*.143))) # lower bound raw diff
(kvs4_fair_nc_raw<- .010) # observed raw diff
(kvs4_fair_nc_upper<-(.010+(1.96*.143))) # upper bound raw diff
## d and 95% CI calculations for No Choice parameter 
(kvs4_fair_nc_lower/(sqrt(.4489+.0565+3.0877))) # lower bound d
(kvs4_fair_nc_raw/(sqrt(.4489+.0565+3.0877))) # observed d
(kvs4_fair_nc_upper/(sqrt(.4489+.0565+3.0877))) # upper bound d

# Choice (Stranger v Kin)#
## raw Choice parameter (from emmeans output) +/- std. err of parameter (returns lower/upper 95% bounds for parameter)
(kvs4_fair_c_lower<-(-1.035-(1.96*.143))) # lower bound raw diff
(kvs4_fair_c_raw<- -1.035) # observed raw diff
(kvs4_fair_c_upper<-(-1.035+(1.96*.143))) # upper bound raw diff
## d and 95% CI calcuations for Choice parameter
(kvs4_fair_c_lower/(sqrt(.4489+.0565+3.0877))) # lower bound d
(kvs4_fair_c_raw/(sqrt(.4489+.0565+3.0877))) # observed d
(kvs4_fair_c_upper/(sqrt(.4489+.0565+3.0877))) # upper bound d

# Failed (Stranger v Kin) #
## raw Failed parameter (from emmeans output) +/- std. err of parameter (returns lower/upper 95% bounds for parameter)
(kvs4_fair_f_lower<-(.812-(1.96*.143))) # lower bound raw diff
(kvs4_fair_f_raw<- .812) # observed raw diff
(kvs4_fair_f_upper<-(.812+(1.96*.143))) # upper bound raw diff
## d and 95% CI calcuations for Choice parameter
(kvs4_fair_f_lower/(sqrt(.4489+.0565+3.0877))) # lower bound d
(kvs4_fair_f_raw/(sqrt(.4489+.0565+3.0877))) # observed d
(kvs4_fair_f_upper/(sqrt(.4489+.0565+3.0877))) # upper bound d

# Stranger Over Kin vs Failed Kin #
## raw SOK vs FHK parameter
(kvs4_fair_SOKvFHK_lower<-(.900-(1.96*.143))) # lower bound raw diff
(kvs4_fair_SOKvFHK_raw<- .900) # observed raw diff
(kvs4_fair_SOKvFHK_upper<-(.900+(1.96*.143))) # upper bound raw diff
# d
(kvs4_fair_SOKvFHK_lower/(sqrt(.4489+.0565+3.0877))) # lower bound d
(kvs4_fair_SOKvFHK_raw/(sqrt(.4489+.0565+3.0877))) # observed d
(kvs4_fair_SOKvFHK_upper/(sqrt(.4489+.0565+3.0877))) # upper bound d


## PREDICTABILITY ##

# Interaction (No Choice vs Choice) #
## raw interaction parameter +/- std. err of parameter (returns lower/upper 95% bounds for parameter)
(kvs4_pred_int1_lower<-(-1.254-(1.96*.192))) # lower bound raw diff
(kvs4_pred_int1_raw<- -1.254) # raw diff
(kvs4_pred_int1_upper<-(-1.254+(1.96*.192))) # upper bound raw diff

# Interaction (Choice vs Failed) # 
## raw interaction parameter +/- std. err of parameter (returns lower/upper 95% bounds for parameter)
(kvs4_pred_int2_lower<-(-2.132-(1.96*.192))) # lower bound raw diff
(kvs4_pred_int2_raw<- -2.132) # raw diff
(kvs4_pred_int2_upper<-(-2.132+(1.96*.192))) # upper bound raw diff

# No Choice (Stranger v Kin)#
## raw No Choice parameter (from emmeans output) +/- std. err of parameter (returns lower/upper 95% bounds for parameter)
(kvs4_pred_nc_lower<-(-.343-(1.96*.136))) # lower bound raw diff
(kvs4_pred_nc_raw<- -.343) # observed raw diff
(kvs4_pred_nc_upper<-(-.343+(1.96*.136))) # upper bound raw diff
## d and 95% CI calculations for No Choice parameter 
(kvs4_pred_nc_lower/(sqrt(.67733+.09846+2.80555))) # lower bound d
(kvs4_pred_nc_raw/(sqrt(.67733+.09846+2.80555))) # observed d
(kvs4_pred_nc_upper/(sqrt(.67733+.09846+2.80555))) # upper bound d

# Choice (Stranger v Kin)#
## raw Choice parameter (from emmeans output) +/- std. err of parameter (returns lower/upper 95% bounds for parameter)
(kvs4_pred_c_lower<-(-1.596-(1.96*.136))) # lower bound raw diff
(kvs4_pred_c_raw<- -1.596) # observed raw diff
(kvs4_pred_c_upper<-(-1.596+(1.96*.136))) # upper bound raw diff
## d and 95% CI calcuations for Choice parameter
(kvs4_pred_c_lower/(sqrt(.67733+.09846+2.80555))) # lower bound d
(kvs4_pred_c_raw/(sqrt(.67733+.09846+2.80555))) # observed d
(kvs4_pred_c_upper/(sqrt(.67733+.09846+2.80555))) # upper bound d

# Failed (Stranger v Kin) #
## raw Failed parameter (from emmeans output) +/- std. err of parameter (returns lower/upper 95% bounds for parameter)
(kvs4_pred_f_lower<-(.536-(1.96*.136))) # lower bound raw diff
(kvs4_pred_f_raw<- .536) # observed raw diff
(kvs4_pred_f_upper<-(.536+(1.96*.136))) # upper bound raw diff
## d and 95% CI calcuations for Choice parameter
(kvs4_pred_f_lower/(sqrt(.67733+.09846+2.80555))) # lower bound d
(kvs4_pred_f_raw/(sqrt(.67733+.09846+2.80555))) # observed d
(kvs4_pred_f_upper/(sqrt(.67733+.09846+2.80555))) # upper bound d

# Stranger Over Kin vs Failed Kin #
## raw SOK vs FHK parameter
(kvs4_pred_SOKvFHK_lower<-(-.795-(1.96*.136))) # lower bound raw diff
(kvs4_pred_SOKvFHK_raw<- -.795) # observed raw diff
(kvs4_pred_SOKvFHK_upper<-(-.795+(1.96*.136))) # upper bound raw diff
# d
(kvs4_pred_SOKvFHK_lower/(sqrt(.67733+.09846+2.80555))) # lower bound d
(kvs4_pred_SOKvFHK_raw/(sqrt(.67733+.09846+2.80555))) # observed d
(kvs4_pred_SOKvFHK_upper/(sqrt(.67733+.09846+2.80555))) # upper bound d


## TYPICALITY ##

# Interaction (No Choice vs Choice) #
## raw interaction parameter +/- std. err of parameter (returns lower/upper 95% bounds for parameter)
(kvs4_typ_int1_lower<-(-1.181-(1.96*.224))) # lower bound raw diff
(kvs4_typ_int1_raw<- -1.181) # raw diff
(kvs4_typ_int1_upper<-(-1.181+(1.96*.224))) # upper bound raw diff

# Interaction (Choice vs Failed) # 
## raw interaction parameter +/- std. err of parameter (returns lower/upper 95% bounds for parameter)
(kvs4_typ_int2_lower<-(-3.765-(1.96*.224))) # lower bound raw diff
(kvs4_typ_int2_raw<- -3.765) # raw diff
(kvs4_typ_int2_upper<-(-3.765+(1.96*.224))) # upper bound raw diff

# No Choice (Stranger v Kin)#
## raw No Choice parameter (from emmeans output) +/- std. err of parameter (returns lower/upper 95% bounds for parameter)
(kvs4_typ_nc_lower<-(-1.116-(1.96*.159))) # lower bound raw diff
(kvs4_typ_nc_raw<- -1.116) # observed raw diff
(kvs4_typ_nc_upper<-(-1.116+(1.96*.159))) # upper bound raw diff
## d and 95% CI calculations for No Choice parameter 
(kvs4_typ_nc_lower/(sqrt(.3473+.1469+3.8277))) # lower bound d
(kvs4_typ_nc_raw/(sqrt(.3473+.1469+3.8277))) # observed d
(kvs4_typ_nc_upper/(sqrt(.3473+.1469+3.8277))) # upper bound d

# Choice (Stranger v Kin)#
## raw Choice parameter (from emmeans output) +/- std. err of parameter (returns lower/upper 95% bounds for parameter)
(kvs4_typ_c_lower<-(-2.297-(1.96*.159))) # lower bound raw diff
(kvs4_typ_c_raw<- -2.297) # observed raw diff
(kvs4_typ_c_upper<-(-2.297+(1.96*.159))) # upper bound raw diff
## d and 95% CI calcuations for Choice parameter
(kvs4_typ_c_lower/(sqrt(.3473+.1469+3.8277))) # lower bound d
(kvs4_typ_c_raw/(sqrt(.3473+.1469+3.8277))) # observed d
(kvs4_typ_c_upper/(sqrt(.3473+.1469+3.8277))) # upper bound d

# Failed (Stranger v Kin) #
## raw Failed parameter (from emmeans output) +/- std. err of parameter (returns lower/upper 95% bounds for parameter)
(kvs4_typ_f_lower<-(1.468-(1.96*.159))) # lower bound raw diff
(kvs4_typ_f_raw<- 1.468) # observed raw diff
(kvs4_typ_f_upper<-(1.468+(1.96*.159))) # upper bound raw diff
## d and 95% CI calcuations for Choice parameter
(kvs4_typ_f_lower/(sqrt(.3473+.1469+3.8277))) # lower bound d
(kvs4_typ_f_raw/(sqrt(.3473+.1469+3.8277))) # observed d
(kvs4_typ_f_upper/(sqrt(.3473+.1469+3.8277))) # upper bound d

# Stranger Over Kin vs Failed Kin #
## raw SOK vs FHK parameter
(kvs4_typ_SOKvFHK_lower<-(-.914-(1.96*.159))) # lower bound raw diff
(kvs4_typ_SOKvFHK_raw<- -.914) # observed raw diff
(kvs4_typ_SOKvFHK_upper<-(-.914+(1.96*.159))) # upper bound raw diff
# d
(kvs4_typ_SOKvFHK_lower/(sqrt(.3473+.1469+3.8277))) # lower bound d
(kvs4_typ_SOKvFHK_raw/(sqrt(.3473+.1469+3.8277))) # observed d
(kvs4_typ_SOKvFHK_upper/(sqrt(.3473+.1469+3.8277))) # upper bound d



#### STUDY 5 ####


## MORALITY ##

# 3-way Interaction (Relationship * Choice * Context for No Choice vs Choice) #
## raw interaction parameter +/- std. err of parameter (returns lower/upper 95% bounds for parameter)
(kvs5_moral_int1_lower<-(.997-(1.96*.236))) # lower bound raw diff
(kvs5_moral_int1_raw<- .997) # raw diff
(kvs5_moral_int1_upper<-(.997+(1.96*.236))) # upper bound raw diff

# 3-way Interaction (Relationship * Choice * Context for Choice vs Failed) # 
## raw interaction parameter +/- std. err of parameter (returns lower/upper 95% bounds for parameter)
(kvs5_moral_int2_lower<-(1.600-(1.96*.236))) # lower bound raw diff
(kvs5_moral_int2_raw<- 1.600) # raw diff
(kvs5_moral_int2_upper<-(1.600+(1.96*.236))) # upper bound raw diff

# PARTIALITY Interaction (No Choice vs Choice) #
## raw interaction parameter +/- std. err of parameter (returns lower/upper 95% bounds for parameter)
(kvs5_moral_p_int1_lower<-(-0.943-(1.96*.165))) # lower bound raw diff
(kvs5_moral_p_int1_raw<- -0.943) # raw diff
(kvs5_moral_p_int1_upper<-(-0.943+(1.96*.165))) # upper bound raw diff

# PARTIALITY Interaction (Choice vs Failed) # 
## raw interaction parameter +/- std. err of parameter (returns lower/upper 95% bounds for parameter)
(kvs5_moral_p_int2_lower<-(-1.246-(1.96*.165))) # lower bound raw diff
(kvs5_moral_p_int2_raw<- -1.246) # raw diff
(kvs5_moral_p_int2_upper<-(-1.246+(1.96*.165))) # upper bound raw diff

# PARTIALITY: No Choice (Stranger v Kin)#
## raw No Choice parameter (from emmeans output) +/- std. err of parameter (returns lower/upper 95% bounds for parameter)
(kvs5_moral_p_nc_lower<-(.167-(1.96*.117))) # lower bound raw diff
(kvs5_moral_p_nc_raw<- .167) # observed raw diff
(kvs5_moral_p_nc_upper<-(.167+(1.96*.117))) # upper bound raw diff
## d and 95% CI calculations for No Choice parameter 
(kvs5_moral_p_nc_lower/(sqrt(.11726+.02058+1.29832))) # lower bound d
(kvs5_moral_p_nc_raw/(sqrt(.11726+.02058+1.29832))) # observed d
(kvs5_moral_p_nc_upper/(sqrt(.11726+.02058+1.29832))) # upper bound d

# PARTIALITY: Choice (Stranger v Kin)#
## raw Choice parameter (from emmeans output) +/- std. err of parameter (returns lower/upper 95% bounds for parameter)
(kvs5_moral_p_c_lower<-(-.776-(1.96*.117))) # lower bound raw diff
(kvs5_moral_p_c_raw<- -.776) # observed raw diff
(kvs5_moral_p_c_upper<-(-.776+(1.96*.117))) # upper bound raw diff
## d and 95% CI calcuations for Choice parameter
(kvs5_moral_p_c_lower/(sqrt(.11726+.02058+1.29832))) # lower bound d
(kvs5_moral_p_c_raw/(sqrt(.11726+.02058+1.29832))) # observed d
(kvs5_moral_p_c_upper/(sqrt(.11726+.02058+1.29832))) # upper bound d

# PARTIALITY: Failed (Stranger v Kin) #
## raw Failed parameter (from emmeans output) +/- std. err of parameter (returns lower/upper 95% bounds for parameter)
(kvs5_moral_p_f_lower<-(.471-(1.96*.117))) # lower bound raw diff
(kvs5_moral_p_f_raw<- .471) # observed raw diff
(kvs5_moral_p_f_upper<-(.471+(1.96*.117))) # upper bound raw diff
## d and 95% CI calcuations for Choice parameter
(kvs5_moral_p_f_lower/(sqrt(.11726+.02058+1.29832))) # lower bound d
(kvs5_moral_p_f_raw/(sqrt(.11726+.02058+1.29832))) # observed d
(kvs5_moral_p_f_upper/(sqrt(.11726+.02058+1.29832))) # upper bound d


# IMPARTIALITY Interaction (No Choice vs Choice) #
## raw interaction parameter +/- std. err of parameter (returns lower/upper 95% bounds for parameter)
(kvs5_moral_i_int1_lower<-(.052-(1.96*.168))) # lower bound raw diff
(kvs5_moral_i_int1_raw<- .052) # raw diff
(kvs5_moral_i_int1_upper<-(.052+(1.96*.168))) # upper bound raw diff

# IMPARTIALITY Interaction (Choice vs Failed) # 
## raw interaction parameter +/- std. err of parameter (returns lower/upper 95% bounds for parameter)
(kvs5_moral_i_int2_lower<-(.356-(1.96*.168))) # lower bound raw diff
(kvs5_moral_i_int2_raw<- .356) # raw diff
(kvs5_moral_i_int2_upper<-(.356+(1.96*.168))) # upper bound raw diff

# IMPARTIALITY: No Choice (Stranger v Kin)#
## raw No Choice parameter (from emmeans output) +/- std. err of parameter (returns lower/upper 95% bounds for parameter)
(kvs5_moral_i_nc_lower<-(.294-(1.96*.119))) # lower bound raw diff
(kvs5_moral_i_nc_raw<- .294) # observed raw diff
(kvs5_moral_i_nc_upper<-(.294+(1.96*.119))) # upper bound raw diff
## d and 95% CI calculations for No Choice parameter 
(kvs5_moral_i_nc_lower/(sqrt(.09831+.04174+1.37549))) # lower bound d
(kvs5_moral_i_nc_raw/(sqrt(.09831+.04174+1.37549))) # observed d
(kvs5_moral_i_nc_upper/(sqrt(.09831+.04174+1.37549))) # upper bound d

# IMPARTIALITY: Choice (Stranger v Kin)#
## raw Choice parameter (from emmeans output) +/- std. err of parameter (returns lower/upper 95% bounds for parameter)
(kvs5_moral_i_c_lower<-(.346-(1.96*.119))) # lower bound raw diff
(kvs5_moral_i_c_raw<- .346) # observed raw diff
(kvs5_moral_i_c_upper<-(.346+(1.96*.119))) # upper bound raw diff
## d and 95% CI calcuations for Choice parameter
(kvs5_moral_i_c_lower/(sqrt(.09831+.04174+1.37549))) # lower bound d
(kvs5_moral_i_c_raw/(sqrt(.09831+.04174+1.37549))) # observed d
(kvs5_moral_i_c_upper/(sqrt(.09831+.04174+1.37549))) # upper bound d

# IMPARTIALITY: Failed (Stranger v Kin) #
## raw Failed parameter (from emmeans output) +/- std. err of parameter (returns lower/upper 95% bounds for parameter)
(kvs5_moral_i_f_lower<-(-.010-(1.96*.119))) # lower bound raw diff
(kvs5_moral_i_f_raw<- -.010) # observed raw diff
(kvs5_moral_i_f_upper<-(-.010+(1.96*.119))) # upper bound raw diff
## d and 95% CI calcuations for Choice parameter
(kvs5_moral_i_f_lower/(sqrt(.09831+.04174+1.37549))) # lower bound d
(kvs5_moral_i_f_raw/(sqrt(.09831+.04174+1.37549))) # observed d
(kvs5_moral_i_f_upper/(sqrt(.09831+.04174+1.37549))) # upper bound d


## TRUSTWORTHINESS ##

# 3-way Interaction (Relationship * Choice * Context for No Choice vs Choice) #
## raw interaction parameter +/- std. err of parameter (returns lower/upper 95% bounds for parameter)
(kvs5_trust_int1_lower<-(1.229-(1.96*.241))) # lower bound raw diff
(kvs5_trust_int1_raw<- 1.229) # raw diff
(kvs5_trust_int1_upper<-(1.229+(1.96*.241))) # upper bound raw diff

# 3-way Interaction (Relationship * Choice * Context for Choice vs Failed) # 
## raw interaction parameter +/- std. err of parameter (returns lower/upper 95% bounds for parameter)
(kvs5_trust_int2_lower<-(1.910-(1.96*.240))) # lower bound raw diff
(kvs5_trust_int2_raw<- 1.910) # raw diff
(kvs5_trust_int2_upper<-(1.910+(1.96*.240))) # upper bound raw diff

# PARTIALITY Interaction (No Choice vs Choice) #
## raw interaction parameter +/- std. err of parameter (returns lower/upper 95% bounds for parameter)
(kvs5_trust_p_int1_lower<-(-1.076-(1.96*.162))) # lower bound raw diff
(kvs5_trust_p_int1_raw<- -1.076) # raw diff
(kvs5_trust_p_int1_upper<-(-1.076+(1.96*.162))) # upper bound raw diff

# PARTIALITY Interaction (Choice vs Failed) # 
## raw interaction parameter +/- std. err of parameter (returns lower/upper 95% bounds for parameter)
(kvs5_trust_p_int2_lower<-(-1.574-(1.96*.162))) # lower bound raw diff
(kvs5_trust_p_int2_raw<- -1.574) # raw diff
(kvs5_trust_p_int2_upper<-(-1.574+(1.96*.162))) # upper bound raw diff

# PARTIALITY: No Choice (Stranger v Kin)#
## raw No Choice parameter (from emmeans output) +/- std. err of parameter (returns lower/upper 95% bounds for parameter)
(kvs5_trust_p_nc_lower<-(.066-(1.96*.115))) # lower bound raw diff
(kvs5_trust_p_nc_raw<- .066) # observed raw diff
(kvs5_trust_p_nc_upper<-(.066+(1.96*.115))) # upper bound raw diff
## d and 95% CI calculations for No Choice parameter 
(kvs5_trust_p_nc_lower/(sqrt(.212305+.009364+1.262629))) # lower bound d
(kvs5_trust_p_nc_raw/(sqrt(.212305+.009364+1.262629))) # observed d
(kvs5_trust_p_nc_upper/(sqrt(.212305+.009364+1.262629))) # upper bound d

# PARTIALITY: Choice (Stranger v Kin)#
## raw Choice parameter (from emmeans output) +/- std. err of parameter (returns lower/upper 95% bounds for parameter)
(kvs5_trust_p_c_lower<-(-1.010-(1.96*.115))) # lower bound raw diff
(kvs5_trust_p_c_raw<- -1.010) # observed raw diff
(kvs5_trust_p_c_upper<-(-1.010+(1.96*.115))) # upper bound raw diff
## d and 95% CI calcuations for Choice parameter
(kvs5_trust_p_c_lower/(sqrt(.212305+.009364+1.262629))) # lower bound d
(kvs5_trust_p_c_raw/(sqrt(.212305+.009364+1.262629))) # observed d
(kvs5_trust_p_c_upper/(sqrt(.212305+.009364+1.262629))) # upper bound d

# PARTIALITY: Failed (Stranger v Kin) #
## raw Failed parameter (from emmeans output) +/- std. err of parameter (returns lower/upper 95% bounds for parameter)
(kvs5_trust_p_f_lower<-(.564-(1.96*.115))) # lower bound raw diff
(kvs5_trust_p_f_raw<- .564) # observed raw diff
(kvs5_trust_p_f_upper<-(.564+(1.96*.115))) # upper bound raw diff
## d and 95% CI calcuations for Choice parameter
(kvs5_trust_p_f_lower/(sqrt(.212305+.009364+1.262629))) # lower bound d
(kvs5_trust_p_f_raw/(sqrt(.212305+.009364+1.262629))) # observed d
(kvs5_trust_p_f_upper/(sqrt(.212305+.009364+1.262629))) # upper bound d


# IMPARTIALITY Interaction (No Choice vs Choice) #
## raw interaction parameter +/- std. err of parameter (returns lower/upper 95% bounds for parameter)
(kvs5_trust_i_int1_lower<-(.148-(1.96*.177))) # lower bound raw diff
(kvs5_trust_i_int1_raw<- .148) # raw diff
(kvs5_trust_i_int1_upper<-(.148+(1.96*.177))) # upper bound raw diff

# IMPARTIALITY Interaction (Choice vs Failed) # 
## raw interaction parameter +/- std. err of parameter (returns lower/upper 95% bounds for parameter)
(kvs5_trust_i_int2_lower<-(.346-(1.96*.177))) # lower bound raw diff
(kvs5_trust_i_int2_raw<- .346) # raw diff
(kvs5_trust_i_int2_upper<-(.346+(1.96*.177))) # upper bound raw diff

# IMPARTIALITY: No Choice (Stranger v Kin)#
## raw No Choice parameter (from emmeans output) +/- std. err of parameter (returns lower/upper 95% bounds for parameter)
(kvs5_trust_i_nc_lower<-(.291-(1.96*.126))) # lower bound raw diff
(kvs5_trust_i_nc_raw<- .291) # observed raw diff
(kvs5_trust_i_nc_upper<-(.291+(1.96*.126))) # upper bound raw diff
## d and 95% CI calculations for No Choice parameter 
(kvs5_trust_i_nc_lower/(sqrt(.13002+.05204+1.52817))) # lower bound d
(kvs5_trust_i_nc_raw/(sqrt(.13002+.05204+1.52817))) # observed d
(kvs5_trust_i_nc_upper/(sqrt(.13002+.05204+1.52817))) # upper bound d

# IMPARTIALITY: Choice (Stranger v Kin)#
## raw Choice parameter (from emmeans output) +/- std. err of parameter (returns lower/upper 95% bounds for parameter)
(kvs5_trust_i_c_lower<-(.438-(1.96*.125))) # lower bound raw diff
(kvs5_trust_i_c_raw<- .438) # observed raw diff
(kvs5_trust_i_c_upper<-(.438+(1.96*.125))) # upper bound raw diff
## d and 95% CI calcuations for Choice parameter
(kvs5_trust_i_c_lower/(sqrt(.13002+.05204+1.52817))) # lower bound d
(kvs5_trust_i_c_raw/(sqrt(.13002+.05204+1.52817))) # observed d
(kvs5_trust_i_c_upper/(sqrt(.13002+.05204+1.52817))) # upper bound d

# IMPARTIALITY: Failed (Stranger v Kin) #
## raw Failed parameter (from emmeans output) +/- std. err of parameter (returns lower/upper 95% bounds for parameter)
(kvs5_trust_i_f_lower<-(.092-(1.96*.126))) # lower bound raw diff
(kvs5_trust_i_f_raw<- .092) # observed raw diff
(kvs5_trust_i_f_upper<-(.092+(1.96*.126))) # upper bound raw diff
## d and 95% CI calcuations for Choice parameter
(kvs5_trust_i_f_lower/(sqrt(.13002+.05204+1.52817))) # lower bound d
(kvs5_trust_i_f_raw/(sqrt(.13002+.05204+1.52817))) # observed d
(kvs5_trust_i_f_upper/(sqrt(.13002+.05204+1.52817))) # upper bound d


## OBLIGATION ##

# 3-way Interaction (Relationship * Choice * Context for No Choice vs Choice) #
## raw interaction parameter +/- std. err of parameter (returns lower/upper 95% bounds for parameter)
(kvs5_oblig_int1_lower<-(.814-(1.96*.270))) # lower bound raw diff
(kvs5_oblig_int1_raw<- .814) # raw diff
(kvs5_oblig_int1_upper<-(.814+(1.96*.270))) # upper bound raw diff

# 3-way Interaction (Relationship * Choice * Context for Choice vs Failed) # 
## raw interaction parameter +/- std. err of parameter (returns lower/upper 95% bounds for parameter)
(kvs5_oblig_int2_lower<-(2.110-(1.96*.270))) # lower bound raw diff
(kvs5_oblig_int2_raw<- 2.110) # raw diff
(kvs5_oblig_int2_upper<-(2.110+(1.96*.270))) # upper bound raw diff

# PARTIALITY Interaction (No Choice vs Choice) #
## raw interaction parameter +/- std. err of parameter (returns lower/upper 95% bounds for parameter)
(kvs5_oblig_p_int1_lower<-(-.898-(1.96*.177))) # lower bound raw diff
(kvs5_oblig_p_int1_raw<- -.898) # raw diff
(kvs5_oblig_p_int1_upper<-(-.898+(1.96*.177))) # upper bound raw diff

# PARTIALITY Interaction (Choice vs Failed) # 
## raw interaction parameter +/- std. err of parameter (returns lower/upper 95% bounds for parameter)
(kvs5_oblig_p_int2_lower<-(-1.689-(1.96*.177))) # lower bound raw diff
(kvs5_oblig_p_int2_raw<- -1.689) # raw diff
(kvs5_oblig_p_int2_upper<-(-1.689+(1.96*.177))) # upper bound raw diff

# PARTIALITY: No Choice (Stranger v Kin)#
## raw No Choice parameter (from emmeans output) +/- std. err of parameter (returns lower/upper 95% bounds for parameter)
(kvs5_oblig_p_nc_lower<-(-.309-(1.96*.125))) # lower bound raw diff
(kvs5_oblig_p_nc_raw<- -.309) # observed raw diff
(kvs5_oblig_p_nc_upper<-(-.309+(1.96*.125))) # upper bound raw diff
## d and 95% CI calculations for No Choice parameter 
(kvs5_oblig_p_nc_lower/(sqrt(.16167+.01279+1.49441))) # lower bound d
(kvs5_oblig_p_nc_raw/(sqrt(.16167+.01279+1.49441))) # observed d
(kvs5_oblig_p_nc_upper/(sqrt(.16167+.01279+1.49441))) # upper bound d

# PARTIALITY: Choice (Stranger v Kin)#
## raw Choice parameter (from emmeans output) +/- std. err of parameter (returns lower/upper 95% bounds for parameter)
(kvs5_oblig_p_c_lower<-(-1.207-(1.96*.125))) # lower bound raw diff
(kvs5_oblig_p_c_raw<- -1.207) # observed raw diff
(kvs5_oblig_p_c_upper<-(-1.207+(1.96*.125))) # upper bound raw diff
## d and 95% CI calcuations for Choice parameter
(kvs5_oblig_p_c_lower/(sqrt(.16167+.01279+1.49441))) # lower bound d
(kvs5_oblig_p_c_raw/(sqrt(.16167+.01279+1.49441))) # observed d
(kvs5_oblig_p_c_upper/(sqrt(.16167+.01279+1.49441))) # upper bound d

# PARTIALITY: Failed (Stranger v Kin) #
## raw Failed parameter (from emmeans output) +/- std. err of parameter (returns lower/upper 95% bounds for parameter)
(kvs5_oblig_p_f_lower<-(.482-(1.96*.125))) # lower bound raw diff
(kvs5_oblig_p_f_raw<- .482) # observed raw diff
(kvs5_oblig_p_f_upper<-(.482+(1.96*.125))) # upper bound raw diff
## d and 95% CI calcuations for Choice parameter
(kvs5_oblig_p_f_lower/(sqrt(.16167+.01279+1.49441))) # lower bound d
(kvs5_oblig_p_f_raw/(sqrt(.16167+.01279+1.49441))) # observed d
(kvs5_oblig_p_f_upper/(sqrt(.16167+.01279+1.49441))) # upper bound d


# IMPARTIALITY Interaction (No Choice vs Choice) #
## raw interaction parameter +/- std. err of parameter (returns lower/upper 95% bounds for parameter)
(kvs5_oblig_i_int1_lower<-(-.086-(1.96*.204))) # lower bound raw diff
(kvs5_oblig_i_int1_raw<- -.086) # raw diff
(kvs5_oblig_i_int1_upper<-(-.086+(1.96*.204))) # upper bound raw diff

# IMPARTIALITY Interaction (Choice vs Failed) # 
## raw interaction parameter +/- std. err of parameter (returns lower/upper 95% bounds for parameter)
(kvs5_oblig_i_int2_lower<-(.420-(1.96*.204))) # lower bound raw diff
(kvs5_oblig_i_int2_raw<- .420) # raw diff
(kvs5_oblig_i_int2_upper<-(.420+(1.96*.204))) # upper bound raw diff

# IMPARTIALITY: No Choice (Stranger v Kin)#
## raw No Choice parameter (from emmeans output) +/- std. err of parameter (returns lower/upper 95% bounds for parameter)
(kvs5_oblig_i_nc_lower<-(.377-(1.96*.145))) # lower bound raw diff
(kvs5_oblig_i_nc_raw<- .377) # observed raw diff
(kvs5_oblig_i_nc_upper<-(.377+(1.96*.145))) # upper bound raw diff
## d and 95% CI calculations for No Choice parameter 
(kvs5_oblig_i_nc_lower/(sqrt(.06865+.03849+2.01751))) # lower bound d
(kvs5_oblig_i_nc_raw/(sqrt(.06865+.03849+2.01751))) # observed d
(kvs5_oblig_i_nc_upper/(sqrt(.06865+.03849+2.01751))) # upper bound d

# IMPARTIALITY: Choice (Stranger v Kin)#
## raw Choice parameter (from emmeans output) +/- std. err of parameter (returns lower/upper 95% bounds for parameter)
(kvs5_oblig_i_c_lower<-(.291-(1.96*.144))) # lower bound raw diff
(kvs5_oblig_i_c_raw<- .291) # observed raw diff
(kvs5_oblig_i_c_upper<-(.291+(1.96*.144))) # upper bound raw diff
## d and 95% CI calcuations for Choice parameter
(kvs5_oblig_i_c_lower/(sqrt(.06865+.03849+2.01751))) # lower bound d
(kvs5_oblig_i_c_raw/(sqrt(.06865+.03849+2.01751))) # observed d
(kvs5_oblig_i_c_upper/(sqrt(.06865+.03849+2.01751))) # upper bound d

# IMPARTIALITY: Failed (Stranger v Kin) #
## raw Failed parameter (from emmeans output) +/- std. err of parameter (returns lower/upper 95% bounds for parameter)
(kvs5_oblig_i_f_lower<-(-.129-(1.96*.145))) # lower bound raw diff
(kvs5_oblig_i_f_raw<- -.129) # observed raw diff
(kvs5_oblig_i_f_upper<-(-.129+(1.96*.145))) # upper bound raw diff
## d and 95% CI calcuations for Choice parameter
(kvs5_oblig_i_f_lower/(sqrt(.06865+.03849+2.01751))) # lower bound d
(kvs5_oblig_i_f_raw/(sqrt(.06865+.03849+2.01751))) # observed d
(kvs5_oblig_i_f_upper/(sqrt(.06865+.03849+2.01751))) # upper bound d


## PREDICTABILITY ##

# 3-way Interaction (Relationship * Choice * Context for No Choice vs Choice) #
## raw interaction parameter +/- std. err of parameter (returns lower/upper 95% bounds for parameter)
(kvs5_pred_int1_lower<-(.424-(1.96*.269))) # lower bound raw diff
(kvs5_pred_int1_raw<- .424) # raw diff
(kvs5_pred_int1_upper<-(.424+(1.96*.269))) # upper bound raw diff

# 3-way Interaction (Relationship * Choice * Context for Choice vs Failed) # 
## raw interaction parameter +/- std. err of parameter (returns lower/upper 95% bounds for parameter)
(kvs5_pred_int2_lower<-(1.182-(1.96*.269))) # lower bound raw diff
(kvs5_pred_int2_raw<- 1.182) # raw diff
(kvs5_pred_int2_upper<-(1.182+(1.96*.269))) # upper bound raw diff

# PARTIALITY Interaction (No Choice vs Choice) #
## raw interaction parameter +/- std. err of parameter (returns lower/upper 95% bounds for parameter)
(kvs5_pred_p_int1_lower<-(-1.311-(1.96*.194))) # lower bound raw diff
(kvs5_pred_p_int1_raw<- -1.311) # raw diff
(kvs5_pred_p_int1_upper<-(-1.311+(1.96*.194))) # upper bound raw diff

# PARTIALITY Interaction (Choice vs Failed) # 
## raw interaction parameter +/- std. err of parameter (returns lower/upper 95% bounds for parameter)
(kvs5_pred_p_int2_lower<-(-2.397-(1.96*.194))) # lower bound raw diff
(kvs5_pred_p_int2_raw<- -2.397) # raw diff
(kvs5_pred_p_int2_upper<-(-2.397+(1.96*.194))) # upper bound raw diff

# PARTIALITY: No Choice (Stranger v Kin)#
## raw No Choice parameter (from emmeans output) +/- std. err of parameter (returns lower/upper 95% bounds for parameter)
(kvs5_pred_p_nc_lower<-(-.544-(1.96*.138))) # lower bound raw diff
(kvs5_pred_p_nc_raw<- -.544) # observed raw diff
(kvs5_pred_p_nc_upper<-(-.544+(1.96*.138))) # upper bound raw diff
## d and 95% CI calculations for No Choice parameter 
(kvs5_pred_p_nc_lower/(sqrt(.036177+.06837+1.80269))) # lower bound d
(kvs5_pred_p_nc_raw/(sqrt(.036177+.06837+1.80269))) # observed d
(kvs5_pred_p_nc_upper/(sqrt(.036177+.06837+1.80269))) # upper bound d

# PARTIALITY: Choice (Stranger v Kin)#
## raw Choice parameter (from emmeans output) +/- std. err of parameter (returns lower/upper 95% bounds for parameter)
(kvs5_pred_p_c_lower<-(-1.855-(1.96*.138))) # lower bound raw diff
(kvs5_pred_p_c_raw<- -1.855) # observed raw diff
(kvs5_pred_p_c_upper<-(-1.855+(1.96*.138))) # upper bound raw diff
## d and 95% CI calcuations for Choice parameter
(kvs5_pred_p_c_lower/(sqrt(.036177+.06837+1.80269))) # lower bound d
(kvs5_pred_p_c_raw/(sqrt(.036177+.06837+1.80269))) # observed d
(kvs5_pred_p_c_upper/(sqrt(.036177+.06837+1.80269))) # upper bound d

# PARTIALITY: Failed (Stranger v Kin) #
## raw Failed parameter (from emmeans output) +/- std. err of parameter (returns lower/upper 95% bounds for parameter)
(kvs5_pred_p_f_lower<-(.542-(1.96*.138))) # lower bound raw diff
(kvs5_pred_p_f_raw<- .542) # observed raw diff
(kvs5_pred_p_f_upper<-(.542+(1.96*.138))) # upper bound raw diff
## d and 95% CI calcuations for Choice parameter
(kvs5_pred_p_f_lower/(sqrt(.036177+.06837+1.80269))) # lower bound d
(kvs5_pred_p_f_raw/(sqrt(.036177+.06837+1.80269))) # observed d
(kvs5_pred_p_f_upper/(sqrt(.036177+.06837+1.80269))) # upper bound d


# IMPARTIALITY Interaction (No Choice vs Choice) #
## raw interaction parameter +/- std. err of parameter (returns lower/upper 95% bounds for parameter)
(kvs5_pred_i_int1_lower<-(-.888-(1.96*.185))) # lower bound raw diff
(kvs5_pred_i_int1_raw<- -.888) # raw diff
(kvs5_pred_i_int1_upper<-(-.888+(1.96*.185))) # upper bound raw diff

# IMPARTIALITY Interaction (Choice vs Failed) # 
## raw interaction parameter +/- std. err of parameter (returns lower/upper 95% bounds for parameter)
(kvs5_pred_i_int2_lower<-(-1.184-(1.96*.185))) # lower bound raw diff
(kvs5_pred_i_int2_raw<- -1.184) # raw diff
(kvs5_pred_i_int2_upper<-(-1.184+(1.96*.185))) # upper bound raw diff

# IMPARTIALITY: No Choice (Stranger v Kin)#
## raw No Choice parameter (from emmeans output) +/- std. err of parameter (returns lower/upper 95% bounds for parameter)
(kvs5_pred_i_nc_lower<-(-.199-(1.96*.131))) # lower bound raw diff
(kvs5_pred_i_nc_raw<- -.199) # observed raw diff
(kvs5_pred_i_nc_upper<-(-.199+(1.96*.131))) # upper bound raw diff
## d and 95% CI calculations for No Choice parameter 
(kvs5_pred_i_nc_lower/(sqrt(.2832+1.6785))) # lower bound d
(kvs5_pred_i_nc_raw/(sqrt(.2832+1.6785))) # observed d
(kvs5_pred_i_nc_upper/(sqrt(.2832+1.6785))) # upper bound d

# IMPARTIALITY: Choice (Stranger v Kin)#
## raw Choice parameter (from emmeans output) +/- std. err of parameter (returns lower/upper 95% bounds for parameter)
(kvs5_pred_i_c_lower<-(-1.088-(1.96*.131))) # lower bound raw diff
(kvs5_pred_i_c_raw<- -1.088) # observed raw diff
(kvs5_pred_i_c_upper<-(-1.088+(1.96*.131))) # upper bound raw diff
## d and 95% CI calcuations for Choice parameter
(kvs5_pred_i_c_lower/(sqrt(.2832+1.6785))) # lower bound d
(kvs5_pred_i_c_raw/(sqrt(.2832+1.6785))) # observed d
(kvs5_pred_i_c_upper/(sqrt(.2832+1.6785))) # upper bound d

# IMPARTIALITY: Failed (Stranger v Kin) #
## raw Failed parameter (from emmeans output) +/- std. err of parameter (returns lower/upper 95% bounds for parameter)
(kvs5_pred_i_f_lower<-(.097-(1.96*.131))) # lower bound raw diff
(kvs5_pred_i_f_raw<- .097) # observed raw diff
(kvs5_pred_i_f_upper<-(.097+(1.96*.131))) # upper bound raw diff
## d and 95% CI calcuations for Choice parameter
(kvs5_pred_i_f_lower/(sqrt(.2832+1.6785))) # lower bound d
(kvs5_pred_i_f_raw/(sqrt(.2832+1.6785))) # observed d
(kvs5_pred_i_f_upper/(sqrt(.2832+1.6785))) # upper bound d


## TYPICALITY ##

# 3-way Interaction (Relationship * Choice * Context for No Choice vs Choice) #
## raw interaction parameter +/- std. err of parameter (returns lower/upper 95% bounds for parameter)
(kvs5_typ_int1_lower<-(.650-(1.96*.308))) # lower bound raw diff
(kvs5_typ_int1_raw<- .650) # raw diff
(kvs5_typ_int1_upper<-(.650+(1.96*.308))) # upper bound raw diff

# 3-way Interaction (Relationship * Choice * Context for Choice vs Failed) # 
## raw interaction parameter +/- std. err of parameter (returns lower/upper 95% bounds for parameter)
(kvs5_typ_int2_lower<-(1.918-(1.96*.308))) # lower bound raw diff
(kvs5_typ_int2_raw<- 1.918) # raw diff
(kvs5_typ_int2_upper<-(1.918+(1.96*.308))) # upper bound raw diff

# PARTIALITY Interaction (No Choice vs Choice) #
## raw interaction parameter +/- std. err of parameter (returns lower/upper 95% bounds for parameter)
(kvs5_typ_p_int1_lower<-(-1.099-(1.96*.225))) # lower bound raw diff
(kvs5_typ_p_int1_raw<- -1.099) # raw diff
(kvs5_typ_p_int1_upper<-(-1.099+(1.96*.225))) # upper bound raw diff

# PARTIALITY Interaction (Choice vs Failed) # 
## raw interaction parameter +/- std. err of parameter (returns lower/upper 95% bounds for parameter)
(kvs5_typ_p_int2_lower<-(-3.382-(1.96*.225))) # lower bound raw diff
(kvs5_typ_p_int2_raw<- -3.382) # raw diff
(kvs5_typ_p_int2_upper<-(-3.382+(1.96*.225))) # upper bound raw diff

# PARTIALITY: No Choice (Stranger v Kin)#
## raw No Choice parameter (from emmeans output) +/- std. err of parameter (returns lower/upper 95% bounds for parameter)
(kvs5_typ_p_nc_lower<-(-.966-(1.96*.159))) # lower bound raw diff
(kvs5_typ_p_nc_raw<- -.966) # observed raw diff
(kvs5_typ_p_nc_upper<-(-.966+(1.96*.159))) # upper bound raw diff
## d and 95% CI calculations for No Choice parameter 
(kvs5_typ_p_nc_lower/(sqrt(.2469+.1428+2.4092))) # lower bound d
(kvs5_typ_p_nc_raw/(sqrt(.2469+.1428+2.4092))) # observed d
(kvs5_typ_p_nc_upper/(sqrt(.2469+.1428+2.4092))) # upper bound d

# PARTIALITY: Choice (Stranger v Kin)#
## raw Choice parameter (from emmeans output) +/- std. err of parameter (returns lower/upper 95% bounds for parameter)
(kvs5_typ_p_c_lower<-(-2.065-(1.96*.159))) # lower bound raw diff
(kvs5_typ_p_c_raw<- -2.065) # observed raw diff
(kvs5_typ_p_c_upper<-(-2.065+(1.96*.159))) # upper bound raw diff
## d and 95% CI calcuations for Choice parameter
(kvs5_typ_p_c_lower/(sqrt(.2469+.1428+2.4092))) # lower bound d
(kvs5_typ_p_c_raw/(sqrt(.2469+.1428+2.4092))) # observed d
(kvs5_typ_p_c_upper/(sqrt(.2469+.1428+2.4092))) # upper bound d

# PARTIALITY: Failed (Stranger v Kin) #
## raw Failed parameter (from emmeans output) +/- std. err of parameter (returns lower/upper 95% bounds for parameter)
(kvs5_typ_p_f_lower<-(1.317-(1.96*.159))) # lower bound raw diff
(kvs5_typ_p_f_raw<- 1.317) # observed raw diff
(kvs5_typ_p_f_upper<-(1.317+(1.96*.159))) # upper bound raw diff
## d and 95% CI calcuations for Choice parameter
(kvs5_typ_p_f_lower/(sqrt(.2469+.1428+2.4092))) # lower bound d
(kvs5_typ_p_f_raw/(sqrt(.2469+.1428+2.4092))) # observed d
(kvs5_typ_p_f_upper/(sqrt(.2469+.1428+2.4092))) # upper bound d


# IMPARTIALITY Interaction (No Choice vs Choice) #
## raw interaction parameter +/- std. err of parameter (returns lower/upper 95% bounds for parameter)
(kvs5_typ_i_int1_lower<-(-.444-(1.96*.209))) # lower bound raw diff
(kvs5_typ_i_int1_raw<- -.444) # raw diff
(kvs5_typ_i_int1_upper<-(-.444+(1.96*.209))) # upper bound raw diff

# IMPARTIALITY Interaction (Choice vs Failed) # 
## raw interaction parameter +/- std. err of parameter (returns lower/upper 95% bounds for parameter)
(kvs5_typ_i_int2_lower<-(-1.449-(1.96*.209))) # lower bound raw diff
(kvs5_typ_i_int2_raw<- -1.449) # raw diff
(kvs5_typ_i_int2_upper<-(-1.449+(1.96*.209))) # upper bound raw diff

# IMPARTIALITY: No Choice (Stranger v Kin)#
## raw No Choice parameter (from emmeans output) +/- std. err of parameter (returns lower/upper 95% bounds for parameter)
(kvs5_typ_i_nc_lower<-(-.592-(1.96*.148))) # lower bound raw diff
(kvs5_typ_i_nc_raw<- -.592) # observed raw diff
(kvs5_typ_i_nc_upper<-(-.592+(1.96*.148))) # upper bound raw diff
## d and 95% CI calculations for No Choice parameter 
(kvs5_typ_i_nc_lower/(sqrt(.1813+2.1474))) # lower bound d
(kvs5_typ_i_nc_raw/(sqrt(.1813+2.1474))) # observed d
(kvs5_typ_i_nc_upper/(sqrt(.1813+2.1474))) # upper bound d

# IMPARTIALITY: Choice (Stranger v Kin)#
## raw Choice parameter (from emmeans output) +/- std. err of parameter (returns lower/upper 95% bounds for parameter)
(kvs5_typ_i_c_lower<-(-1.036-(1.96*.148))) # lower bound raw diff
(kvs5_typ_i_c_raw<- -1.036) # observed raw diff
(kvs5_typ_i_c_upper<-(-1.036+(1.96*.148))) # upper bound raw diff
## d and 95% CI calcuations for Choice parameter
(kvs5_typ_i_c_lower/(sqrt(.1813+2.1474))) # lower bound d
(kvs5_typ_i_c_raw/(sqrt(.1813+2.1474))) # observed d
(kvs5_typ_i_c_upper/(sqrt(.1813+2.1474))) # upper bound d

# IMPARTIALITY: Failed (Stranger v Kin) #
## raw Failed parameter (from emmeans output) +/- std. err of parameter (returns lower/upper 95% bounds for parameter)
(kvs5_typ_i_f_lower<-(.413-(1.96*.148))) # lower bound raw diff
(kvs5_typ_i_f_raw<- .413) # observed raw diff
(kvs5_typ_i_f_upper<-(.413+(1.96*.148))) # upper bound raw diff
## d and 95% CI calcuations for Choice parameter
(kvs5_typ_i_f_lower/(sqrt(.1813+2.1474))) # lower bound d
(kvs5_typ_i_f_raw/(sqrt(.1813+2.1474))) # observed d
(kvs5_typ_i_f_upper/(sqrt(.1813+2.1474))) # upper bound d


## EXPECTEDNESS ##

# 3-way Interaction (Relationship * Choice * Context for No Choice vs Choice) #
## raw interaction parameter +/- std. err of parameter (returns lower/upper 95% bounds for parameter)
(kvs5_exp_int1_lower<-(.081-(1.96*.301))) # lower bound raw diff
(kvs5_exp_int1_raw<- .081) # raw diff
(kvs5_exp_int1_upper<-(.081+(1.96*.301))) # upper bound raw diff

# 3-way Interaction (Relationship * Choice * Context for Choice vs Failed) # 
## raw interaction parameter +/- std. err of parameter (returns lower/upper 95% bounds for parameter)
(kvs5_exp_int2_lower<-(1.752-(1.96*.300))) # lower bound raw diff
(kvs5_exp_int2_raw<- 1.752) # raw diff
(kvs5_exp_int2_upper<-(1.752+(1.96*.300))) # upper bound raw diff

# PARTIALITY Interaction (No Choice vs Choice) #
## raw interaction parameter +/- std. err of parameter (returns lower/upper 95% bounds for parameter)
(kvs5_exp_p_int1_lower<-(-0.826-(1.96*.219))) # lower bound raw diff
(kvs5_exp_p_int1_raw<- -0.826) # raw diff
(kvs5_exp_p_int1_upper<-(-0.826+(1.96*.219))) # upper bound raw diff

# PARTIALITY Interaction (Choice vs Failed) # 
## raw interaction parameter +/- std. err of parameter (returns lower/upper 95% bounds for parameter)
(kvs5_exp_p_int2_lower<-(-3.213-(1.96*.219))) # lower bound raw diff
(kvs5_exp_p_int2_raw<- -3.213) # raw diff
(kvs5_exp_p_int2_upper<-(-3.213+(1.96*.219))) # upper bound raw diff

# PARTIALITY: No Choice (Stranger v Kin)#
## raw No Choice parameter (from emmeans output) +/- std. err of parameter (returns lower/upper 95% bounds for parameter)
(kvs5_exp_p_nc_lower<-(-1.121-(1.96*.155))) # lower bound raw diff
(kvs5_exp_p_nc_raw<- -1.121) # observed raw diff
(kvs5_exp_p_nc_upper<-(-1.121+(1.96*.155))) # upper bound raw diff
## d and 95% CI calculations for No Choice parameter 
(kvs5_exp_p_nc_lower/(sqrt(.4241+.1074+2.2925))) # lower bound d
(kvs5_exp_p_nc_raw/(sqrt(.4241+.1074+2.2925))) # observed d
(kvs5_exp_p_nc_upper/(sqrt(.4241+.1074+2.2925))) # upper bound d

# PARTIALITY: Choice (Stranger v Kin)#
## raw Choice parameter (from emmeans output) +/- std. err of parameter (returns lower/upper 95% bounds for parameter)
(kvs5_exp_p_c_lower<-(-1.946-(1.96*.155))) # lower bound raw diff
(kvs5_exp_p_c_raw<- -1.946) # observed raw diff
(kvs5_exp_p_c_upper<-(-1.946+(1.96*.155))) # upper bound raw diff
## d and 95% CI calcuations for Choice parameter
(kvs5_exp_p_c_lower/(sqrt(.4241+.1074+2.2925))) # lower bound d
(kvs5_exp_p_c_raw/(sqrt(.4241+.1074+2.2925))) # observed d
(kvs5_exp_p_c_upper/(sqrt(.4241+.1074+2.2925))) # upper bound d

# PARTIALITY: Failed (Stranger v Kin) #
## raw Failed parameter (from emmeans output) +/- std. err of parameter (returns lower/upper 95% bounds for parameter)
(kvs5_exp_p_f_lower<-(1.267-(1.96*.155))) # lower bound raw diff
(kvs5_exp_p_f_raw<- 1.267) # observed raw diff
(kvs5_exp_p_f_upper<-(1.267+(1.96*.155))) # upper bound raw diff
## d and 95% CI calcuations for Choice parameter
(kvs5_exp_p_f_lower/(sqrt(.4241+.1074+2.2925))) # lower bound d
(kvs5_exp_p_f_raw/(sqrt(.4241+.1074+2.2925))) # observed d
(kvs5_exp_p_f_upper/(sqrt(.4241+.1074+2.2925))) # upper bound d


# IMPARTIALITY Interaction (No Choice vs Choice) #
## raw interaction parameter +/- std. err of parameter (returns lower/upper 95% bounds for parameter)
(kvs5_exp_i_int1_lower<-(-.740-(1.96*.205))) # lower bound raw diff
(kvs5_exp_i_int1_raw<- -.740) # raw diff
(kvs5_exp_i_int1_upper<-(-.740+(1.96*.205))) # upper bound raw diff

# IMPARTIALITY Interaction (Choice vs Failed) # 
## raw interaction parameter +/- std. err of parameter (returns lower/upper 95% bounds for parameter)
(kvs5_exp_i_int2_lower<-(-1.424-(1.96*.205))) # lower bound raw diff
(kvs5_exp_i_int2_raw<- -1.424) # raw diff
(kvs5_exp_i_int2_upper<-(-1.424+(1.96*.205))) # upper bound raw diff

# IMPARTIALITY: No Choice (Stranger v Kin)#
## raw No Choice parameter (from emmeans output) +/- std. err of parameter (returns lower/upper 95% bounds for parameter)
(kvs5_exp_i_nc_lower<-(-.342-(1.96*.145))) # lower bound raw diff
(kvs5_exp_i_nc_raw<- -.342) # observed raw diff
(kvs5_exp_i_nc_upper<-(-.342+(1.96*.145))) # upper bound raw diff
## d and 95% CI calculations for No Choice parameter 
(kvs5_exp_i_nc_lower/(sqrt(.1994+2.0521))) # lower bound d
(kvs5_exp_i_nc_raw/(sqrt(.1994+2.0521))) # observed d
(kvs5_exp_i_nc_upper/(sqrt(.1994+2.0521))) # upper bound d

# IMPARTIALITY: Choice (Stranger v Kin)#
## raw Choice parameter (from emmeans output) +/- std. err of parameter (returns lower/upper 95% bounds for parameter)
(kvs5_exp_i_c_lower<-(-1.082-(1.96*.145))) # lower bound raw diff
(kvs5_exp_i_c_raw<- -1.082) # observed raw diff
(kvs5_exp_i_c_upper<-(-1.082+(1.96*.145))) # upper bound raw diff
## d and 95% CI calcuations for Choice parameter
(kvs5_exp_i_c_lower/(sqrt(.1994+2.0521))) # lower bound d
(kvs5_exp_i_c_raw/(sqrt(.1994+2.0521))) # observed d
(kvs5_exp_i_c_upper/(sqrt(.1994+2.0521))) # upper bound d

# IMPARTIALITY: Failed (Stranger v Kin) #
## raw Failed parameter (from emmeans output) +/- std. err of parameter (returns lower/upper 95% bounds for parameter)
(kvs5_exp_i_f_lower<-(.342-(1.96*.145))) # lower bound raw diff
(kvs5_exp_i_f_raw<- .342) # observed raw diff
(kvs5_exp_i_f_upper<-(.342+(1.96*.145))) # upper bound raw diff
## d and 95% CI calcuations for Choice parameter
(kvs5_exp_i_f_lower/(sqrt(.1994+2.0521))) # lower bound d
(kvs5_exp_i_f_raw/(sqrt(.1994+2.0521))) # observed d
(kvs5_exp_i_f_upper/(sqrt(.1994+2.0521))) # upper bound d
