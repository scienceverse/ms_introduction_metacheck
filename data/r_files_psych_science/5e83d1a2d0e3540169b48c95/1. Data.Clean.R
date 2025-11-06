require(haven)
require(PsychMisc)
require(lmerTest)
require(optimx)
require(glmmTMB)
require(DHARMa)
require(bbmle)
require(broom.mixed)
require(redres)
require(DescTools)
require(psych)
require(lm.beta)
require(olsrr)
require(broom)
require(powerMediation)
require(WebPower)
require(RSPS)
require(tidyverse)


# Functions ---------------------------------------------------------------

# adjust distribution by specified transformation
distfix <- 
  function(x, type){
    if(min(x, na.rm=T) < 1){
      x <- x + 1 
    }
    trans <- match.fun(type)
    x <- trans(x)
  }

# adjust distribution by specified transformation, reversed:
reflect.distfix <- 
  function(x, type){
    x <- revCode(x)
    if(min(x, na.rm=T) < 1){
      x <- x + 1 
    }
    trans <- match.fun(type)
    x <- trans(x)
    x <- revCode(x)
  }



# data load ---------------------------------------------------------------

dat <- read_spss("./originaldata/CLS Data.sav")
datw <- read_spss("./originaldata/CLS Data_wide format.sav")


# Data cleaning ---------------------------------------------
# .Nomination models -----------------------------------------------------


# construct key variables:

dat <- 
  dat %>% 
  mutate(
    # narcissism
    narc = rowMeans(select(., matches("N[1-9]0?")), na.rm = T),
    # Standardized
    narc_z = as.numeric(scale(narc)),
    # log-transformed classroom size (# potential nominators):
    # (needs as.vector so it will slip by some hypersensitive 
    # assumption-checking for offsets in glmmTMB)
    Lnominators_log = log(Lnominators) %>% as.vector,
    # log-transformed case nominations (for log-linear model)
    Lnominations_log = log(Lnominations+1),
    # Agentic and communal traits:
    agentic = rowMeans(select(., T1, T3, T4, T5, T8), na.rm=T),
    agentic_z = as.numeric(scale(agentic)),
    communal = rowMeans(select(., T2, T11, T13, T14), na.rm=T),
    # observation-level deviance parameter (for overdispersed model):
    obs_effect  = 1:n(),
    # letter-based class codes to make categories easier to read below:
    Classroom_anonymized = letters[Classroom_anonymized],
    
  ) %>% 
  # Demographics/Group comparison calculations:
  group_by(Classroom_anonymized) %>% 
  mutate(
    mean_age = mean(Age_fractional, na.rm=T),
    size = n(),
    mean_age_exclusive = (mean_age*size - Age_fractional) / (size-1),
    # Age as a proportion of class peers' ages:
    relative_age = Age_fractional / mean_age_exclusive,
    # Mean gender (proportion female) by class:
    mean_gender = mean(Gender, na.rm=T)
    
  ) %>% 
  ungroup() %>% 
  # Some renames for ease:
  rename(
    age = Age_fractional, 
    gender = Gender,
    school = School_anonymized, 
    group = Groupnumber,
    class = Classroom_anonymized
  ) %>% 
  # grand-mean centered variables (for s-curves w/count data):
  mutate_at(.vars = vars(age, relative_age, gender, mean_gender),
            .funs = list(
              u = ~mean(., na.rm=T),
              c = ~. - mean(., na.rm=T)
            )
  ) %>% 
  # Drop case w/missing predictor:
  filter(!is.na(narc_z))


# expanding on control scheme iterations to aid convergence:
newcontrol <-  
  glmmTMBControl(optCtrl =  list(iter.max=900, eval.max=1200))


# saving this for later use by s-curves and others:
save(dat, newcontrol, file = "./processeddata/data.processed.rda")
write_csv(dat, path = "./processeddata/data.processed.csv")

# .Group task: ----------------------------------------------

# B. GROUP-LEVEL ANALYSIS
# leader-level narcissism predicting:
# 1.	self-perceived leader authority (reported by leader) + self-perceived leader effectiveness (reported by leader)
# 2.	follower-perceived leader authority (as reported by the two followers) + follower-perceived leader effectiveness(as reported by the two followers)
# 3.	self-perceived future leader preference (reported by leader)
# 4.	follower-perceived future leader preference (reported by the two followers)
# 5.	coded leadership behaviour (coded from behavior)
# 6.  coded overall leadership functioning (coded from behavior)
# 7.	group performance (dichotomous objective outcome)

datw_merged <- 
  datw %>% 
  mutate(
    # Authority + effectiveness, self:
    auth_self = select(., LP1.1.00, LP2.1.00, LP3.1.00, LP4.1.00) %>% rowMeans(na.rm=T),
    
    effect_self = 
      select(., LP10.1.00, LP11.1.00, LP12.1.00, LP13.1.00) %>% rowMeans(na.rm=T),
    # Authority + effectiveness, other: 
    auth_others = 
      select(., LP1.2.00, LP2.2.00, LP3.2.00, LP4.2.00,
             LP1.3.00, LP2.3.00, LP3.3.00, LP4.3.00) %>% rowMeans(na.rm=T),
    effect_others = 
      select(., LP10.2.00, LP11.2.00, LP12.2.00, LP13.2.00,
             LP10.3.00, LP11.3.00, LP12.3.00, LP13.3.00) %>% rowMeans(na.rm=T),
    # individual ratings, used in reshaping for some tests:
    auth_other_1 = 
      select(., LP1.2.00, LP2.2.00, LP3.2.00, LP4.2.00) %>% rowMeans(na.rm=T),
    auth_other_2= 
      select(., LP1.3.00, LP2.3.00, LP3.3.00, LP4.3.00) %>% rowMeans(na.rm=T),
    effect_other_1 = 
      select(., LP10.2.00, LP11.2.00, LP12.2.00, LP13.2.00) %>% rowMeans(na.rm=T),
    effect_other_2 = 
      select(., LP10.3.00, LP11.3.00, LP12.3.00, LP13.3.00) %>% rowMeans(na.rm=T),
    # individual preference:
    leader_other_1 = case_when(L_emer_1.2.00 == 1 ~ 3,
                               L_emer_2.2.00 == 1 ~ 2,
                               L_emer_3.2.00 == 1 ~ 1,
                               TRUE ~ NA_real_),
    leader_other_2 = case_when(L_emer_1.3.00 == 1 ~ 3,
                               L_emer_2.3.00 == 1 ~ 2,
                               L_emer_3.3.00 == 1 ~ 1,
                               TRUE ~ NA_real_),

    # Coded leadership behavior:
    coded_LB = 
      select(., "LA1.Vraagvoorkeur.1.00", "LA2.Vraaginhoud.1.00", 
             "LB1.Voorkeuraangeven.1.00", "LB2.Inhoudgeven.1.00", 
             "LC1.Hoevaakparticipantspreekt.1.00", "LD1.Takenverdelen.1.00", 
             "LD2.Conclusies.1.00") %>% rowMeans(na.rm=T),
    
    # Subtracting 1 from group_correct so it's a proper binary for analysis
    group_correct_binary = group_correct - 1,
    
    # OTHER COVARIATES:
    # Relative age compared to group:
    relative_age_group = 
      Age_fractional.1.00 / ((Age_fractional.2.00 + Age_fractional.3.00)/2),
    # Group average female:
    gender_group = (Gender.1.00 + Gender.2.00 + Gender.3.00)/3,
    # Dummy codes for experimenter:
    experimenter_dummy = Name_experimenter.1.00 - 1,
    # Dummy codes for task version:
    version1_dummy = ifelse(Task_version == 1, 1, 0),
    version2_dummy = ifelse(Task_version == 2, 1, 0),
    # dummy code for group homogeneity:
    group_homogeneous_dummy =
      select(., a = Classroom_anonymized.1.00, 
             b = Classroom_anonymized.2.00, 
             c = Classroom_anonymized.3.00) %>%
      pmap_lgl(function(a,b,c){(a == b && b == c)}) %>%
      as.numeric,
    # Session length, in minutes:
    session_length = (Duurvaninteractie_tijd %>% as.numeric)/60,
  ) %>% 
  
  # Few things just renamed for ease:
  rename(
    # Self-rated future leadership preference
    leader_self = leader_self.1.00,
    # Others preference:
    leader_others = leader_others.1.00,
    # Overall coded leadership functioning:
    coded_LF = LF.Totaalscoreleider03.1.00,
    # shared and unshared info
    si_leader = SHARED_INFO_LEADER, 
    ui_leader = UNSHARED_INFO_LEADER,
    si_group = SHARED_INFO_GROUP, 
    ui_group = UNSHARED_INFO_GROUP,
    
    # ID 
    ID = ID.1.00, 
    # Age, gender, school, group, 
    age = Age_fractional.1.00, 
    gender = Gender.1.00, 
    school = School_anonymized, 
    group = Groupnumber,
    class = Classroom_anonymized.1.00
  ) %>% 
  
  # Merge in narcissism and agentic, so we can use whole-sample standardization:
  left_join(
    dat %>% 
      select(ID, narc, narc_z, agentic, agentic_z) 
  )

#  ..Transform DVs ----
# 
# 1.	None
# 2.	Square root for positive skewness, reflect and square root for negative skewness
# 3.	Log + 1 for positive skewness, reflect and log + 1 for negative skewness


#  self-ratings of authoritativeness:
datw_merged$auth_self %>% table %>% plot # neg skew

(datw_merged$auth_self_rsqrt <- 
    reflect.distfix(datw_merged$auth_self, type = "sqrt")) %>% 
  table %>% plot # better
(datw_merged$auth_self_rlog <- 
    reflect.distfix(datw_merged$auth_self, type = "log")) %>% 
  table %>% plot # arguably even better

#  self-ratings of effectiveness:
datw_merged$effect_self %>% table %>% plot # strong neg skew

(datw_merged$effect_self_rsqrt <- 
    reflect.distfix(datw_merged$effect_self, type = "sqrt")) %>% 
  table %>% plot # better
(datw_merged$effect_self_rlog <- 
    reflect.distfix(datw_merged$effect_self, type = "log")) %>% 
  table %>% plot # arguably even better

#  other-ratings of authoritativeness:
datw_merged$auth_others %>% table %>% plot # neg skew

(datw_merged$auth_others_rsqrt <- 
    reflect.distfix(datw_merged$auth_others, type = "sqrt")) %>% 
  table %>% plot # better
(datw_merged$auth_others_rlog <- 
    reflect.distfix(datw_merged$auth_others, type = "log")) %>% 
  table %>% plot # arguably even better

#  other-ratings of effectiveness:
datw_merged$effect_others %>% table %>% plot # strong neg skew

(datw_merged$effect_others_rsqrt <- 
    reflect.distfix(datw_merged$effect_others, type = "sqrt")) %>% 
  table %>% plot # better
(datw_merged$effect_others_rlog <- 
    reflect.distfix(datw_merged$effect_others, type = "log")) %>% 
  table %>% plot # arguably even better

# distributions of individual raters:

datw_merged$auth_other_1 %>% table %>% plot # neg skew

(datw_merged$auth_other_rsqrt_1 <- 
    reflect.distfix(datw_merged$auth_other_1, type = "sqrt")) %>% 
  table %>% plot # better
(datw_merged$auth_other_rlog_1 <- 
    reflect.distfix(datw_merged$auth_other_1, type = "log")) %>% 
  table %>% plot # arguably even better

#  other-ratings of effectiveness:
datw_merged$effect_other_1 %>% table %>% plot # strong neg skew

(datw_merged$effect_other_rsqrt_1 <- 
    reflect.distfix(datw_merged$effect_other_1, type = "sqrt")) %>% 
  table %>% plot # better
(datw_merged$effect_other_rlog_1 <- 
    reflect.distfix(datw_merged$effect_other_1, type = "log")) %>% 
  table %>% plot # arguably even better

datw_merged$auth_other_2 %>% table %>% plot # neg skew

(datw_merged$auth_other_rsqrt_2 <- 
    reflect.distfix(datw_merged$auth_other_2, type = "sqrt")) %>% 
  table %>% plot # better
(datw_merged$auth_other_rlog_2 <- 
    reflect.distfix(datw_merged$auth_other_2, type = "log")) %>% 
  table %>% plot # arguably even better

#  other-ratings of effectiveness:
datw_merged$effect_other_2 %>% table %>% plot # strong neg skew

(datw_merged$effect_other_rsqrt_2 <- 
    reflect.distfix(datw_merged$effect_other_2, type = "sqrt")) %>% 
  table %>% plot # better
(datw_merged$effect_other_rlog_2 <- 
    reflect.distfix(datw_merged$effect_other_2, type = "log")) %>% 
  table %>% plot # arguably even better


# Coded leadership behaviors:
datw_merged$coded_LB %>% hist # positive skew

(datw_merged$coded_LB_sqrt <- 
    distfix(datw_merged$coded_LB, type = "sqrt")) %>% 
  hist # better
(datw_merged$coded_LB_log <- 
    distfix(datw_merged$coded_LB, type = "log")) %>% 
  hist # arguably even better

# Coded Leadership functioning:
datw_merged$coded_LF %>% table %>% plot # doesn't really need transformation

# Leader shared information:
datw_merged$si_leader %>% hist # positive skew

(datw_merged$si_leader_sqrt <- 
    distfix(datw_merged$si_leader, type = "sqrt")) %>% 
  hist # better
(datw_merged$si_leader_log <- 
    distfix(datw_merged$si_leader, type = "log")) %>% 
  hist # probably too much

# Leader unshared information:
datw_merged$ui_leader %>% hist # positive skew

(datw_merged$ui_leader_sqrt <- 
    distfix(datw_merged$ui_leader, type = "sqrt")) %>% 
  hist # better
(datw_merged$ui_leader_log <- 
    distfix(datw_merged$ui_leader, type = "log")) %>% 
  hist # probably too much

# ..Shorten/reshape -------------------------------------------------------

datw_short <- 
  datw_merged %>% 
  select(
    # Location variables:
    school, class, group, 
    # Narcissism:
    narc, narc_z,
    agentic_z,
    # Self-rating
    auth_self, auth_self_rsqrt, auth_self_rlog,  
    effect_self, effect_self_rsqrt, effect_self_rlog,  
    # other rating: 
    auth_others, auth_others_rsqrt, auth_others_rlog,  
    effect_others, effect_others_rsqrt, effect_others_rlog,  
    # individual ratings for reshaping:
    auth_other_1, auth_other_rsqrt_1, auth_other_rlog_1,  
    auth_other_2, auth_other_rsqrt_2, auth_other_rlog_2,  
    effect_other_1, effect_other_rsqrt_1, effect_other_rlog_1,  
    effect_other_2, effect_other_rsqrt_2, effect_other_rlog_2, 
    leader_other_1, leader_other_2,
    # Leadership rankings, self
    leader_self,
    # leadership rankings, others
    leader_others,
    # Coded leadership behaviors:
    coded_LB, coded_LB_sqrt, coded_LB_log, coded_LF,
    # shared and unshared info:
    si_leader, si_leader_sqrt, si_leader_log,
    ui_leader, ui_leader_sqrt, ui_leader_log,
    si_group, ui_group, 
    # Correct group †ask:
    group_correct, group_correct_binary,
    # Demographics of target individual and environment:
    age, relative_age_group, 
    gender, gender_group,
    experimenter_dummy,
    version1_dummy, version2_dummy,
    session_length,
    group_homogeneous_dummy)


datw_long <- 
  pivot_longer(data = datw_short, 
               cols = c(
                 auth_other_1, auth_other_rsqrt_1, auth_other_rlog_1,  
                 auth_other_2, auth_other_rsqrt_2, auth_other_rlog_2,  
                 effect_other_1, effect_other_rsqrt_1, effect_other_rlog_1,  
                 effect_other_2, effect_other_rsqrt_2, effect_other_rlog_2,
                 leader_other_1, leader_other_2),
               names_to = c(".value", "member"),
               names_pattern = "(.*)_([12])"
  )

save(datw_short, datw_long, file = "./processeddata/data.processed.wide.rda")
write_csv(datw_short, path = "./processeddata/data.processed.wide.csv")




