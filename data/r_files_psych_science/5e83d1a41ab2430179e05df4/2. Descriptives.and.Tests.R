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


# code to invert log links in term estimates:

exponentiate.tidy <- function(ret){
  vv <- intersect(c("estimate", "conf.low", "conf.high"), names(ret))
  ret <- (ret %>% mutate_at(vars(vv), ~exp(.)) %>% 
            mutate(std.error = std.error * estimate))
  return(ret)}

# Logistic models:
printout.logistic <- function(mod, .term = "narc_z"){
  tidy(mod, conf.int = TRUE) %>% 
    filter(term == .term) %>% 
    mutate(
      output = 
        paste0(
          "B = ", Format(estimate, digits = 2, sci = NA), ", ",
          "95% CI = [", Format(conf.low, digits = 2, sci = NA), ", ", 
          Format(conf.high, digits = 2, sci = NA), "], ",
          "OR = ", Format(exp(estimate), digits = 2, sci = NA), ", ",
          ifelse(p.value >= .001, 
                 paste0("p = ", Format(p.value, digits = 3, sci = NA, leading = "drop")),
                 "p < .001")
        )
    ) %>% 
    pull(output)
}

snip <-  function(input) {
  #Standard snip: Outputs as indvidual lines
  pb <- pipe("pbcopy", "w")
  write(print(input), file=pb)
  close(pb)
}



# data load ---------------------------------------------------------------

load("./processeddata/data.processed.rda")
load("./processeddata/data.processed.wide.rda")

# Power analysis ----------------------------------------------------------

# 1. peer nomination/leadership emergence task:

# reference is Poorthuis et al., 2019, who saw:
# popularity M=3.25 SD=0.77
# narcicissm M=2.07 SD=0.46
# ß = .39

# In our case:
dat$Lnominations %>% describe
dat %>% pull(Lnominations) %>% 
  {log(. + 1)} %>% 
  describe

# Working from that distribution of data, a 
# ballpark equivalent of change similar to ß = .39 
# would be
(1.12 + .86*.39)/1.12 # 1.30, or 
(1.12 - .86*.39)/1.12 # 0.70

# As a b for standardized narcissism, this would be
log(1.2994) # 0.26

# Maximal overdispersion in this data:
summary(odt <- glmmTMB(Lnominations ~ 1, offset = Lnominators_log, family="poisson", dat))
sim_odt <- simulateResiduals(odt)
testDispersion(sim_odt)

# Given what we have:
(nb.power <-
    negbin.pow(331, # 331 since one person did not answer the narcissism
               3.38, # Mean counts
               k = 1.28, # Approximate estimate from Poorthuis
               # k = seq(1.1, 1.4, .1), # range of possibilities
               disp = 2.0323, # dispersion estimate from above
               # disp = seq(1, 2.5, .25), # range of dispersion possibilities
               alpha = .05,
               6789, 2000, monitor = TRUE, 3))

# 2. linear models:
wp.correlation(n = 107, r=0.27) # Based off of self-ratings in Brunell et al., 2008

# 3.logistic regression

# Identifying where we have approx 80% power based on published numbers:
powerLogisticCon(107,
                 .36, # Base rate of performance from Nevicka et al., 2011
                 1.76,
                 alpha = 0.05)

log(1.76) # That's a raw b ~ .56


# Descriptive Stats -------
# .Nomination model -------------------------------------------------------
# ..Participants ----------------------------------------------------------

# of 321 actual participants, statistics of age, gender:
participant.stats <- 
  dat %>% 
  summarize(
    total = n(),
    total_participate = filter(., !is.na(age)) %>% nrow,
    min_age = min(age, na.rm=T),
    max_age = max(age, na.rm=T),
    m_age = mean(age, na.rm=T),
    sd_age = sd(age, na.rm = T),
    m_gender = 1-mean(gender, na.rm=T),
    n_groups = n_distinct(group, na.rm=T),
    mean_dutch = mean(Geboorteland_dichotomized)
  )

# 1 missing from narc (already removed)
dat$narc %>% is.na %>% table
# 1 missing for Geboorteland_dichotomized:
dat %>% 
  filter(is.na(Geboorteland_dichotomized)) %>% 
  select(ID, Geboorteland_dichotomized)



# ..Variables ------------------------------------------------------

# Narc alpha & distribution:
dat %>% select(N1:N10) %>% {psych::alpha(.)} %>% pluck("total")
dat %>% pull(narc) %>% {QuantPsyc::norm(.)}
dat %>% pull(narc) %>% shapiro.test()

# Nominations counts/distribution
# Mean 3.38 SD 3.73, median 2
dat %>% pull(Lnominations) %>% describe
# Positive skew, leptokurtic:
dat %>% pull(Lnominations) %>% {QuantPsyc::norm(.)}
# Very non normal:
dat %>% pull(Lnominations) %>% shapiro.test()

# 89 / 332 had 0 noms (26.8% / 73.2%  0 / >0)
(dat$Lnominations > 0) %>% table %>% addmargins
(dat$Lnominations > 0) %>% table %>% prop.table

# .Group task ----------------------------------------------------------


# ..Participants ----------------------------------------------------------

participant.stats.grouptask <- 
  dat %>% 
  filter(!is.na(group)) %>% 
  summarize(
    total = n(),
    total_participate = filter(., !is.na(age)) %>% nrow,
    min_age = min(age, na.rm=T),
    max_age = max(age, na.rm=T),
    m_age = mean(age, na.rm=T),
    sd_age = sd(age, na.rm = T),
    m_gender = 1-mean(gender, na.rm=T),
    n_groups = n_distinct(group, na.rm=T),
    mean_dutch = mean(Geboorteland_dichotomized)
  )

# Count of homo/heterogeneous groups:
datw_short %>% 
  group_by(group_homogeneous_dummy) %>% 
  summarize(n())

# How long did it take for each group to finish?:
datw_short %>% 
  summarize(m_time = mean(session_length, na.rm=T),
            sd_time = sd(session_length, na.rm=T)
  )


# ..Alphas ----------------------------------------------------------------

# Authority, self alpha
datw %>% 
  select(LP1.1.00, LP2.1.00, LP3.1.00, LP4.1.00) %>% 
  {psych::alpha(.)} %>% 
  pluck("total")

# Authority, others, alpha
dat %>% 
  ungroup() %>% 
  filter(Role != 1) %>% 
  select(LP1:LP4) %>% 
  {psych::alpha(.)} %>% 
  pluck("total")


# Effectiveness, self alpha 
datw %>% 
  select(LP10.1.00, LP11.1.00, LP12.1.00, LP13.1.00) %>% 
  {psych::alpha(.)} %>% 
  pluck("total")

# Effectiveness, others, alpha:
dat %>% 
  ungroup() %>% 
  filter(Role != 1) %>% 
  select(LP10:LP13) %>% 
  {psych::alpha(.)} %>% 
  pluck("total")

# Alpha for overall functioning, leader
dat %>% 
  filter(Role == 1) %>% 
  select(LA1.Vraagvoorkeur,
         LA2.Vraaginhoud,
         LB1.Voorkeuraangeven,
         LB2.Inhoudgeven,
         LC1.Hoevaakparticipantspreekt,
         LD1.Takenverdelen,
         LD2.Conclusies) %>% 
  {psych::alpha(.)}

# Alpha for overall functioning, followers
dat %>% 
  filter(Role != 1) %>% 
  select(LA1.Vraagvoorkeur,
         LA2.Vraaginhoud,
         LB1.Voorkeuraangeven,
         LB2.Inhoudgeven,
         LC1.Hoevaakparticipantspreekt,
         LD1.Takenverdelen,
         LD2.Conclusies) %>% 
  {psych::alpha(.)}


# ..rWG -------------------------------------------------------------------

# follower authority ratings:
datw_long %>% 
  filter(!is.na(group_correct)) %>%  
  select(auth_other, group) %>% 
  {multilevel::rwg(.$auth_other, .$group)} %>% 
  summarize(
    m_rwg = mean(rwg, na.rm=T),
    sd_rwg = sd(rwg, na.rm=T),
    median_rwg = median(rwg, na.rm=T)
  )

# follower effectiveness ratings:
datw_long %>% 
  filter(!is.na(group_correct)) %>%  
  select(effect_other, group) %>% 
  {multilevel::rwg(.$effect_other, .$group)} %>% 
  summarize(meanrwg = mean(rwg, na.rm=T),
            sd= sd(rwg, na.rm=T)
  )

# follower leadership rankings:
datw_long %>% 
  filter(!is.na(group_correct)) %>%  
  select(leader_other, group) %>% 
  {multilevel::rwg(.$leader_other, .$group)} %>% 
  summarize(meanrwg = mean(rwg, na.rm=T),
            sd= sd(rwg, na.rm=T),
            over = mean(rwg > .7, na.rm=T)
  )


# ..ICCs ------------------------------------------------------------------

# ICCs for leadership RA codings:
dat %>% select(LA1.Vraagvoorkeur, LA1.M) %>% as.matrix %>% ICC
dat %>% select(LA2.Vraaginhoud, LA2.M) %>% as.matrix %>% ICC
dat %>% select(LB1.Voorkeuraangeven, LB1.M) %>% as.matrix %>% ICC
dat %>% select(LB2.Inhoudgeven, LB2.M) %>% as.matrix %>% ICC
dat %>% select(LC1.Hoevaakparticipantspreekt, LC1.M) %>% as.matrix %>% ICC
dat %>% select(LD1.Takenverdelen, LD1.M) %>% as.matrix %>% ICC
dat %>% select(LD2.Conclusies, LD2.M) %>% as.matrix %>% ICC
dat %>% select(LF.Totaalscoreleider03, LF.M) %>% as.matrix %>% ICC


# ICCs for others
datw_short %>% select(auth_other_1, auth_other_2) %>% as.matrix %>%  ICC
datw_short %>% select(effect_other_1, effect_other_2) %>% as.matrix %>% ICC
datw_short %>% select(leader_other_1, leader_other_2) %>% as.matrix %>% ICC


# ..Other things ----------------------------------------------------------

# How long did it take for each group to finish?
datw_short %>% 
  summarize_at(.vars = vars(session_length), 
               .funs = list(mean = mean, sd = sd)
  )

dat$leader_role <- dat$Role == 1

# Narcissism variation by role:
t.test(narc ~ leader_role, dat, var.equal = TRUE)

# Nomination variation by role:
t.test(Lnominations ~ leader_role, dat, var.equal = TRUE)
# Age by role:
t.test(age ~ leader_role, dat, var.equal = TRUE)

# Gender by role:
t.test(gender ~ leader_role, dat, var.equal = TRUE)

# Overall coded leadership behavior:
dat$coded_LB <- 
  makevar(dat, c("LA1.Vraagvoorkeur", "LA2.Vraaginhoud", 
                 "LB1.Voorkeuraangeven", "LB2.Inhoudgeven", 
                 "LC1.Hoevaakparticipantspreekt", "LD1.Takenverdelen", 
                 "LD2.Conclusies"))

# Overall coded leadership functioning:
dat$coded_LF <- 
  dat$LF.Totaalscoreleider03



# Leaders vs followers leadership scores
dat %>% 
  group_by(Role == 1) %>% 
  summarize_at(.vars = vars(coded_LB, coded_LF), 
               .funs = list(mean = mean, sd = sd)
  )

# Leadership behavior by role:
t.test(coded_LB ~ leader_role, dat, var.equal = TRUE)

# Leadership functioning by role
t.test(coded_LF ~ leader_role, dat, var.equal = TRUE)

# Correlation of specific/global leadership:

datw_short %>% {cor.test(.$coded_LB, .$coded_LF)}


# effectiveness ratings based on correctness
t.test(effect_others ~ group_correct_binary, data = datw_short, var.equal = TRUE)

# likelihood of being right based on leadership behavior:
glm(group_correct_binary ~ coded_LB, datw_short, family = "binomial") %>% summary
glm(group_correct_binary ~ coded_LF, datw_short, family = "binomial") %>% summary
glm(group_correct_binary ~ ui_leader, datw_short, family = "binomial") %>% summary
glm(group_correct_binary ~ si_leader, datw_short, family = "binomial") %>% summary


# Descriptives for sharing of information by leader:
datw_short %>% 
  summarize(m_si = mean(si_leader, na.rm=T),
            sd_si = sd(si_leader, na.rm = T),
            m_ui = mean(ui_leader, na.rm=T),
            sd_ui = sd(ui_leader, na.rm = T)
  )         

# missingness in follower responses for group 118
datw_long %>% 
  filter(group == 118) %>% 
  select(auth_other) 

# 1 case missing:
datw_long$group_correct %>% is.na %>% table
n_distinct(datw_long$group)

#Related to footnote 2:

# We also examined how often all group members (rather than just the leader)
# exchanged shared (M = 27.93 times, SD = 15.95) and unshared (M = 27.47 times,
# SD = 16.40) information. Group-level information exchange was unrelated to
# leader narcissism (shared information: r[105] = .01, p = .923, unshared
# information r[105] = .08, p = .415), but group-level exchange of unshared
# information did predict better group performance, OR = 1.06, B = .06, p < .001. 

datw_short %>% 
  summarize_at(.vars = vars(si_group, ui_group),
               .funs = list(mean = mean, sd = sd),
  ) %>% unlist

cor.test(datw_short$si_group, datw_short$narc_z)
cor.test(datw_short$ui_group, datw_short$narc_z)

printout.logistic(
  glm(group_correct_binary ~ ui_group, datw_short,family = "binomial"),
  .term = "ui_group"
)
