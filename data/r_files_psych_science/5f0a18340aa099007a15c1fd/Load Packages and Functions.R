#############################################
#### LOAD PACKAGES AND UTILITY FUNCTIONS ####
#############################################

# Turn off scientific notation
options(scipen=999)

# Load packages
if (!require("tidyverse")) install.packages("tidyverse") #Range of convenience functions
if (!require("magrittr")) install.packages("magrittr") #Additional pipe operators (%$%)
if (!require("psych")) install.packages("psych") #Range of convenience functions
if (!require("lm.beta")) install.packages("lm.beta") #Get standardized betas from regression models
if (!require("lmerTest")) install.packages("lmerTest") #Performs significance tests for LMMs
if (!require("optimx")) install.packages("optimx") #Optimizers for lmer models
if (!require("r2glmm")) install.packages("r2glmm") #Gets effect size for LMMs
if (!require("broom")) install.packages("broom") #Helps organize analysis output
if (!require("ggeffects")) install.packages("ggeffects") #Generates predicted values from models for plotting
if (!require("scales")) install.packages("scales") #Allows rescaling of variables
if (!require("kableExtra")) install.packages("kableExtra") #For rendering tables in HTML (Rmd)
if (!require("printr")) install.packages("printr") #Efficient backup method of rendering tables in HTML
if (!require("extrafont")) install.packages("extrafont") #For expanding font options
if (!require("Cairo")) install.packages("Cairo") #For rendering graphs with high quality anti-aliasing
if (!require("sjstats")) install.packages("sjstats") #Gives CIs around Std.Beta
if (!require("Hmisc")) install.packages("Hmisc") #For corstars
if (!require("xtable")) install.packages("xtable") #For corstars
if (!require("afex")) install.packages("afex") #For ANCOVA
if (!require("emmeans")) install.packages("emmeans") #For plotting ANCOVA
select <- dplyr::select #Ensuring that the default for "select" is from dplyr
summarize <- dplyr::summarize #Same with summarize()

#### PLOTTING ####
# Set up plot theme
apatheme <- theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line())


#### REGRESSION FUNCTIONS ####
display.lm <- function(model, digits = 2) {
  df <- summary(model) %>% with(df)
  display.obj <- tidy(model) %>% bind_cols(confint_tidy(model)) %>% 
    as.data.frame() %>% mutate(p.value.char = ifelse(p.value < .001, "< .001", p.value),
                               p.value.3 = round(p.value, 3),
                               ` ` = ifelse(p.value < .001, "***", 
                                            ifelse(p.value < .01, "**", 
                                                   ifelse(p.value < .05, "\\*", 
                                                          ifelse(p.value < .10, "\\.", " "))))) %>%
    mutate(p.value = as.character(ifelse(p.value < .001, "< .001", p.value.3)),
           df = df[2]) %>%
    mutate(Std.Beta = lm.beta(model) %>% coefficients() %>% as.vector()) %>% 
    select(term, B = estimate, SE = std.error, Std.Beta, t = statistic, df, p.value, conf.low, conf.high, ` `) %>%
    mutate_if(is.numeric, round, digits)
  return(display.obj)
}


display.glm <- function(model, digits = 2, 
                        OR = FALSE, CI = TRUE, conf.level = .95) {
  df <- summary(model) %>% with(df)
  display.obj <- tidy(model) %>% 
    as.data.frame() %>% mutate(p.value.char = ifelse(p.value < .001, "< .001", p.value),
                               p.value.3 = round(p.value, 3),
                               ` ` = ifelse(p.value < .001, "***", 
                                            ifelse(p.value < .01, "**", 
                                                   ifelse(p.value < .05, "\\*", 
                                                          ifelse(p.value < .10, "\\.", " "))))) %>%
    mutate(p.value = as.character(ifelse(p.value < .001, "< .001", p.value.3)),
           df = df[2])
  if(OR == T){
    if(CI == T){
      odds <- exp(cbind(OR = coef(model), confint(model, level = conf.level))) %>% 
        as.data.frame() %>% rownames_to_column("term")
      names(odds) <- c("term", "OR", "conf.low", "conf.high")
      
      display.obj <- display.obj %>% left_join(odds) %>% 
        dplyr::select(term, B = estimate, SE = std.error, Odds.Ratio = OR, z = statistic, df, 
                      p.value, conf.low, conf.high, ` `)
    } else {
      odds <- exp(coef(model)) %>% as.vector()
      display.obj <- display.obj %>% 
        mutate(OR = exp(coef(model)) %>% as.vector()) %>% 
        dplyr::select(term, B = estimate, SE = std.error, Odds.Ratio = OR,
                      z = statistic, df, p.value, ` `)
    }
  } else {
    if(CI == T){
      display.obj <- display.obj %>% 
        bind_cols(confint_tidy(model, conf.level = conf.level)) %>% 
        dplyr::select(term, B = estimate, SE = std.error, z = statistic, df, 
                      p.value, conf.low, conf.high, ` `)
    } else {
      display.obj <- display.obj %>% 
        dplyr::select(term, B = estimate, SE = std.error, z = statistic, df, 
                      p.value, ` `)
    }
  }
  display.obj <- display.obj %>% 
    mutate_if(is.numeric, round, digits)
  return(display.obj)
}

display.lmer <- function(model, digits = 2, CI = TRUE) {
  display.obj <- summary(model) %>% coefficients() %>% as.data.frame() %>% 
    rownames_to_column() %>% rename(term = rowname, p.value = `Pr(>|t|)`) %>% 
    as.data.frame() %>% mutate(p.value.char = ifelse(p.value < .001, "< .001", p.value),
                               p.value.3 = round(p.value, 3),
                               ` ` = ifelse(p.value < .001, "***", 
                                            ifelse(p.value < .01, "**", 
                                                   ifelse(p.value < .05, "\\*", 
                                                          ifelse(p.value < .10, "\\.", " "))))) %>%
    mutate(p.value = as.character(ifelse(p.value < .001, "< .001", p.value.3))) %>%
    select(term, Estimate, SE = `Std. Error`, t = `t value`, df, p.value, ` `) %>%
    mutate_if(is.numeric, round, digits)
  if(CI == TRUE) {
    CI <- confint(model) %>% as.data.frame() %>% rownames_to_column("term") %>% 
      rename(conf.low = `2.5 %`, conf.high = `97.5 %`) %>% 
      mutate(conf.low = round(conf.low, digits), conf.high = round(conf.high, digits))
    display.obj.print <- left_join(display.obj, CI) %>% 
      select(term, Estimate, SE, t, df, p.value, conf.low, conf.high, ` `)
  } else {
    display.obj.print <- display.obj
  }
  return(display.obj.print)
}

random.lmer <- function(model, digits = 2) {
  display.obj <- VarCorr(model) %>% as.data.frame() %>% 
    select(Groups = grp, Var1 = var1, Var2 = var2, 
           `Variance/\nCovariance` = vcov, 
           `SD/Correlation` = sdcor) %>%
    mutate_if(is.numeric, round, digits) %>% 
    mutate(Var1 = ifelse(is.na(Var1) == T, " ", Var1),
           Var2 = ifelse(is.na(Var2) == T, " ", Var2))
  return(display.obj)
}

display.glmer <- function(model, digits = 2) {
  display.obj <- summary(model) %>% coefficients() %>% as.data.frame() %>% rownames_to_column() %>% 
    rename(term = rowname, p.value = `Pr(>|z|)`) %>% 
    as.data.frame() %>% mutate(p.value.char = ifelse(p.value < .001, "< .001", p.value),
                               p.value.3 = round(p.value, 3),
                               ` ` = ifelse(p.value < .001, "***", 
                                            ifelse(p.value < .01, "**", 
                                                   ifelse(p.value < .05, "\\*", 
                                                          ifelse(p.value < .10, "\\.", " "))))) %>%
    mutate(p.value = as.character(ifelse(p.value < .001, "< .001", p.value.3))) %>%
    select(term, Estimate, SE = `Std. Error`, Z = `z value`, p.value, ` `) %>%
    mutate_if(is.numeric, round, digits)
  return(display.obj)
}

## T.Test

display.t <- function(model, digits = 2, CI = FALSE){
  data <- unlist(str_split(model$data.name, " by ")) %>% 
    str_split("\\$")
  DV <- pull(get(data[[1]][1])[data[[1]][2]])
  Group <- pull(get(data[[2]][1])[data[[2]][2]])
  d <- effsize::cohen.d(DV, Group)
  desc.out <- psych::describeBy(DV, group = Group, mat = T) %>% 
    select(group1, M=mean, SE=se) %>% gather(var, value, -group1) %>% 
    arrange(group1) %>% 
    mutate(Stat = str_c(var, " (", group1,")")) %>% select(-group1, -var) %>% 
    mutate(Stat = factor(Stat, levels = .$Stat[1:4])) %>% 
    spread(Stat, value)
  model.out <- broom::tidy(model) %>% 
    mutate(`Cohen's d` = d$estimate,
           p.value = ifelse(p.value < .001, "< .001", round(p.value, digits))) %>% 
    select(t = statistic, df = parameter, p.value, `Cohen's d`)
  
  if(CI == TRUE){
    display.obj <- bind_cols(desc.out, model.out) %>% 
      mutate(conf.low = as.vector(d$conf.int[1]),
             conf.high = as.vector(d$conf.int[2])) %>% 
      mutate_if(is.numeric, round, digits)
  } else {
    display.obj <- bind_cols(desc.out, model.out) %>% 
      mutate_if(is.numeric, round, digits)
  }
  return(display.obj)
}




## Additional steps for formatting kable
htable <- function(display.obj, style = "default") {
  if (style == "default"){kab <- kable(display.obj)}
  if (style == "center"){kab <- kable(display.obj, align = c("l", rep('c', ncol(display.obj) - 1)))}
  kab %>% kable_styling(bootstrap_options = c("hover", "condensed", "responsive"), 
                        full_width = F, position = "center")
}

#### FREQUENCY TABLES ####
freq <- function(x, digits = 2){
  table <- as.data.frame(table(x))
  prop <- as.data.frame(prop.table(table(x))) %>% 
    mutate(`%` = round(100*Freq,digits)) %>% select(-Freq)
  final <- left_join(table, prop, by="x") %>% dplyr::rename(Response = x)
  return(final)
}

#Alpha
display.alpha <- function(df, digits = 3, check.keys = T){
  display <- as.data.frame(df) %>% 
    psych::alpha(check.keys = check.keys) %>% with(total) %>% round(digits) %>% 
    dplyr::select(raw.alpha = raw_alpha, std.alpha, mean, sd)
  return(display)
}
