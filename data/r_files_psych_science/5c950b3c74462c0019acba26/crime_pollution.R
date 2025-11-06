#' # Commentary: Polluted Morality
#' 
#' Load packages and data:

suppressMessages({
  library("tidyverse")
  library("mgcv")
  library("gridExtra")
  library("psych")
  library("lubridate")
  library("ggplot2")
  library("MASS")
})

load("data/pollution.rda")
pollution



#' Check and remove outliers of crime rates per district
#' (e.g., drops to zero or other low numbers due to misreporting)
ggplot(pollution, aes(x =date, y=crime, col = lad_code))+
  geom_line(alpha = .5) +
  theme_bw()+
  theme(legend.position = "none")
nrow(pollution)

pollution <- pollution %>%
  group_by(lad_code) %>% 
  filter(crime > median(crime) - 2 * IQR(crime)) %>% 
  ungroup %>% 
  mutate(pollution = c(scale(pollution)))
nrow(pollution)
ggplot(pollution, aes(x =date, y=crime, col = lad_code))+
  geom_line(alpha = .5)+
  theme_bw()+
  theme(legend.position = "none") 



#' ## Summary information for data set
#' 
#' Number of observations:
nrow(pollution)
#' Number of districts:
length(unique(pollution$lad_code))
#' First record:
min(pollution$date)
#' Last record:
max(pollution$date)
#' Composite score for air pollution: Correlations of indices are very high:
corr.test(pollution[,c("no2", "pm2.5", "pm10", "pollution")])



#' ## Plot Trends
#' 
#' Temporal trends of pollution and crime rates are opposed to each other:

sc <- sapply(pollution[,c("crime", "pollution")], 
             function(x) c(mean(x), sd(x)))
poll <- pollution %>% 
  dplyr::select(date, lad_code, crime, pollution) %>% 
  mutate(crime = c(scale(crime)) * 6) %>% 
  gather("Dependent variable", "value", -c(date, lad_code)) %>%
  mutate(`Dependent variable` = recode(`Dependent variable`, 
                                       crime = "Number of crimes",
                                       pollution = "Air pollution")) 
ggplot(poll, aes(x = date, 
                 color = `Dependent variable`, 
                 lty = `Dependent variable`, y = value))+
  geom_smooth(alpha = .3, se = TRUE, level = .95,
              method = 'gam', formula = y ~ s(x, bs = "cs"))+
  theme_bw()+
  scale_y_continuous("Air pollution (z-standardized)", 
                     sec.axis = sec_axis(~ . / 6 * sc[2,1] + sc[1,1], 
                                         name = "Number of crimes (per district / month)\n"))+
  scale_color_discrete(name = "Dependent variable")+
  scale_linetype_discrete(name = "Dependent variable")+
  scale_x_date(breaks = seq(ymd("2012-01-01"), ymd("2018-12-31"), "6 month"), #"6 months", 
               # date_minor_breaks = "6 months",
               minor_breaks = NULL,
               date_labels = "%Y %b")+
  theme(legend.position  = c(.15,.9), 
        legend.background = element_rect(fill = NA),
        axis.text.x = element_text(angle = 45, hjust = 1))+
  xlab(NULL)
ggsave("plots/trends.png", width = 7, height = 4.5, dpi = 300)




#' ## Modeling: Generalized Additive Model (GAM)
#'
#' We model the monthly number of crimes per district by a negative-binomial
#' model which is less restrictive and more robust than a Poisson model
#' (cf. Gardner et al., 1995, Psychological Bulletin).
#'
#' The model includes the following predictors:
#'
#' * pollution: composite z-score of air pollution 
#' * log(population): log of the population per district and month 
#' * date: thin-plate smoothing spline for time trends
#' * lad_code: random effect for districts 
#' * longitude, latitude: longitude and latitude of the centroid per district

# for spline: k=initial degrees of freedom needs to be large enough
k_date <- length(unique(pollution$date))
pollution$lad_code <- factor(pollution$lad_code)
pollution$num_date <- as.numeric(pollution$date)

fit <- gam(crime ~ pollution + log(population) +
             s(num_date, bs = "tp", k = k_date)+
             s(lad_code, bs = "re") +
             s(longitude, latitude, bs = "tp"),       
           family=nb(link = "log"),
           data=pollution)

#' Check residuals and autocorrelation:
gam.check(fit)
acf(pollution$crime)  # autocorrelation for raw data
acf(resid(fit))       # autocorrelation for model residuals



#' Results
#'
#' * Estimated seasonal trends and effect size (odds ratio) for pollution
plot(fit, select = 1, shade = TRUE)  # fitted seasonal trend of crime rates
exp(coef(fit)["pollution"])          # Effect size: odds ratios

#' * Fit nested model without pollution as predictor:
fit_no_pollution <- update(fit, .~. - pollution)


#' * Summary of LLGG's effect sizes (required to estimate statistical power)
#' * effect sizes reported in LLGG, Table 2 (slopes in Poisson regression)
beta_LLGG <- c(0.399,0.395,0.064,0.050,0.079,0.070,0.132,
               0.124,0.040,0.039,0.008,0.004,0.056,0.062)
beta_H1 <- median(beta_LLGG)
# as odds ratios (OR):
summary(exp(beta_LLGG))


#' * Posterior probabilities/Bayes factors for air pollution via BIC values
#' * function for transforming BIC values into posterior model probabilities/Bayes factors
#' * see Wagenmakers (2004)
#' * p-values via Wald tests and approximate statistical power
post_prob <- function(model, model_nested, alpha = .01){
  
  # BIC approximation for Bayes factor
  BIC <- BIC(model, model_nested)$BIC
  delta_BIC <- (BIC - min(BIC))
  postprob <- exp(-.5 * delta_BIC)/sum(exp(-.5 * delta_BIC))
  
  # p-values for GAM: Wald test
  label <- setdiff(names(coef(model)), names(coef(model_nested)))
  power <- NA
  if (length(label) == 1){
    tab <- summary(model)[[ifelse("gam" %in% class(model), "p.table", "coefficients")]]
    pval <- tab[label,4]
    power <- 1 - pnorm(qnorm(1 - alpha/2), beta_H1 / tab[label, 2]) # approximate power
  } else if (all(grepl("s(", label, fixed = TRUE))) {
    label2 <- sub(".[0-9]+", "", label)[1]
    tab <- summary(model)$s.table
    pval <- tab[label2,4]
  } else {
    lrt <- anova(model_nested, model, test = "Chisq")
    pval <- lrt[2,"Pr(Chi)"]
  }
  
  c("pp_nested" = postprob[2], "BF_nested" = postprob[2] / postprob[1],
    "pval" = pval, "power" = power)
}
# evidence for null effect of air pollution:
post_prob(fit, fit_no_pollution)




#' Explained variance of air pollution:
r2 <- c("R2_with_pollution" = summary(fit)$r.sq,
        "R2_without_pollution" = summary(fit_no_pollution)$r.sq)
round(c(r2, "R2_diff" = diff(r2)), 7)


#' * Omission of the circular trend over months results in a large 
#' **negative** effect of air pollution:
#'
fit_no_time <- update(fit, .~. - s(num_date, bs = "tp", k = k_date))
fit_no_timepoll <- update(fit_no_time, .~.-pollution)
post_prob(fit_no_time, fit_no_timepoll)
exp(coef(fit_no_time)["pollution"])  # odds ratio


#' * GLM with fixed effects for date and district 
#'   (identical to LLGG's model except for link function)
#' 
fit_glm <- glm.nb(crime ~ pollution + log(population) +
                    + factor(date) +
                    + factor(lad_code), data = pollution)
fit_glm_nested <- update(fit_glm, .~ . - pollution)

# effect size (odds ratios):
exp(coef(fit_glm)[1:4])   
# evidence for null effect of air pollution:
post_prob(fit_glm, fit_glm_nested)




#' ## Analysis for specific crime categories
#' 
#' Proportion of specific crime categories relative to total number of crimes:
n_crime <- pollution %>% 
  dplyr::select(`Anti-social behaviour`:`Violence and sexual offences`, crime) %>%
  colSums

pct_crimetype <- n_crime / n_crime["crime"] * 100
round(t(t(pct_crimetype)), 1)


#' ### Fit different model versions
#' 
#' * Dependent variable: number of crimes per category/month/district
#' * Model: GAM vs. GLM

# dv = name of dependent variable
# data = pollution data set
fit_all <- function(dv, data){
  
  # rename dependent variable (compatibility with mgcv::bam)
  dv2 <- gsub("[ -]", "_", dv)
  data[[dv2]] <- data[[dv]]
  data$num_date <- as.numeric(data$date)
  data$lad_code <- factor(data$lad_code)
  contrasts(data$lad_code) <- contr.sum(levels(data$lad_code))
  data$date_factor <- factor(data$date)
  contrasts(data$date_factor) <- contr.sum(levels(data$date_factor))
  
  # formula parts for regression models
  f_pollution <- paste(dv2, "~ pollution + log(population)")
  f_date_factor <- "factor(date) + factor(lad_code)"
  f_lad_re <- "s(lad_code, bs = 're')"
  
  # k = maximum degrees of freedom for the spline for date in GAM 
  # (sufficiently large number should be chosen)
  k_date <- length(unique(data$date[!is.na(data[[dv2]])]))
  f_date_s <- paste0("s(num_date, bs = 'tp', k = ", k_date, ")")
  
  ########### GAM 
  f_gam <- paste(f_pollution, f_date_s, f_lad_re, 
                 "s(longitude, latitude, bs = 'tp')", sep = " + ")

  fit_gam <- gam(as.formula(f_gam), data = data, family = nb(link = "log"))
  fit_gam_wopoll <- update(fit_gam, . ~ . - pollution)
  fit_gam_wopop  <- update(fit_gam, . ~ . - log(population))
  fit_gam_wodate <- update(fit_gam, as.formula(paste0(". ~ . - ", f_date_s)))
  
  pp_gam_poll <- post_prob(fit_gam, fit_gam_wopoll)
  pp_gam_pop  <- post_prob(fit_gam, fit_gam_wopop)
  pp_gam_date <- post_prob(fit_gam, fit_gam_wodate)
  
  # estimates:
  gam_summ <- summary(fit_gam)
  df_gam <- data.frame(dv = dv, model = "GAM", 
                       parameter = rownames(gam_summ$p.table), gam_summ$p.table[,1:2], 
                       check.names = FALSE, row.names = 1:nrow(gam_summ$p.table))
  df_gam[df_gam$parameter == "pollution", names(pp_gam_poll)] <- pp_gam_poll
  df_gam[df_gam$parameter == "log(population)", names(pp_gam_pop)] <- pp_gam_pop
  
  
  ########### GLM
  f_glm <- paste(f_pollution, f_date_factor, sep = " + ")
  fit_glm <- glm.nb(as.formula(f_glm), data = data)
  fit_glm_wopoll <- update(fit_glm, . ~ . - pollution)
  fit_glm_wopop  <- update(fit_glm, . ~ . - log(population))
  fit_glm_wodate <- update(fit_glm, . ~ . - factor(date))
  
  pp_glm_poll <- post_prob(fit_glm, fit_glm_wopoll)
  pp_glm_pop  <- post_prob(fit_glm, fit_glm_wopop)
  pp_glm_date <- post_prob(fit_glm, fit_glm_wodate)
  
  # estimates:
  glm_summ <- summary(fit_glm)
  df_glm <- data.frame(dv = dv, model = "GLM", 
                       parameter = rownames(glm_summ$coef), glm_summ$coef[,1:2], 
                       check.names = FALSE, row.names = 1:nrow(glm_summ$coef))
  df_glm[df_glm$parameter == "pollution", names(pp_glm_poll)] <- pp_glm_poll
  df_glm[df_glm$parameter == "log(population)", names(pp_glm_pop)] <- pp_glm_pop
  
  list("estimates" = rbind(df_gam, df_glm),
       "pp_date" = data.frame("model" = c("GAM", "GLM"),
                              rbind(pp_gam_date, pp_glm_date)) )
}


#' Loop across all crime categories
col_idx <- 
  which(colnames(pollution) == "Anti-social behaviour"):
  which(colnames(pollution) == "Violence and sexual offences")
crimetypes <- c("crime", colnames(pollution)[col_idx])

res_list <- list()
for(ct in crimetypes){
  res_list[[ct]] <- fit_all(ct, pollution)
}
estimates <- do.call("rbind", lapply(res_list, "[[", "estimates"))



#' Effect of seasonal trends
bf_date <- do.call("rbind", lapply(res_list, "[[", "pp_date"))
bf_date %>% mutate(BF_date = 1/BF_nested) %>% 
  filter(model == "GAM") %>% 
  print(digits = 3)


#' Results for each crime category and model
estimates %>% 
  filter(parameter == "pollution") %>% 
  mutate(OR = exp(Estimate), dv = substr(dv, 1, 16)) %>%
  dplyr::select(dv, model, OR, pval, power, BF_nested, pp_nested) %>% 
  print(digits = 3)
# summary of results:
estimates %>% 
  group_by(model) %>% 
  filter(parameter == "pollution") %>% 
  mutate(OR = exp(Estimate)) %>%
  dplyr::select(dv, model, OR, pval, power, BF_nested, pp_nested) %>% 
  summarize_at(vars(OR, pval, power, BF_nested), 
               .funs = c(min = min, median = median, max = max)) %>% 
  as.data.frame %>% print(digits = 3)



#' ## Save results
#' 
#' Excel table:
cbind("GAM" = estimates %>% 
        filter(model == "GAM", parameter == "pollution") %>% 
        mutate(`Odds Ratio` = exp(Estimate)) %>% 
        dplyr::select(dv, Estimate, `Std. Error`, `Odds Ratio`, pval, power, BF_nested),
      estimates %>% 
        filter(model == "GLM", parameter == "pollution") %>% 
        mutate(`Odds Ratio` = exp(Estimate)) %>% 
        dplyr::select(Estimate, `Std. Error`, `Odds Ratio`, pval, power, BF_nested)) %>% 
  mutate_at(vars(-1), round, digits = 3) %>% 
  write_csv("results/estimates.csv")

#' Fitted models etc:
save(list = ls(), file = "results/estimates.rda")

