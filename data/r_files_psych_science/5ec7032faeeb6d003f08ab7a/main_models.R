main_pred <- 'screentime'
filter_missing_background <- T
filter_multiple_semesters <- F
source('load_data.R')

cor(course_ind_df$grade,course_ind_df$screentime)
cor(ind_avr_df$gpa,ind_avr_df$screentime)
cor(ind_avr_df$screentime_outofclass,ind_avr_df$screentime)

ind_avr_df_rel <- ind_avr_df %>% select(gpa,screentime,screentime_outofclass,
                                        age, hs_gpa, 
                                        parent_edu_max, parent_inc_mean,
                                        agreeableness, conscientiousness, extraversion, neuroticism,
                                        openness, locus_of_control, bmi,smoker,male)

bootstrap_corrs <- function() {
  d <- ind_avr_df_rel %>% sample_frac(size = 1, replace = T)
  corrs <- cor(d %>% select(-male,-smoker))[c('gpa','screentime','screentime_outofclass'),]
  malecorr <- d %>% select(male, gpa, screentime,screentime_outofclass) %>% group_by(male) %>% summarise_all(mean)
  smokecorr <- d %>% select(smoker, gpa, screentime,screentime_outofclass) %>% group_by(smoker) %>% summarise_all(mean)
  male <- c(unname(unlist(malecorr[2,-1] - malecorr[1,-1])))
  smoker <- c(unname(unlist(smokecorr[2,-1] - smokecorr[1,-1])))
  corrs <- cbind(corrs,male,smoker)
  return (corrs)
}

set.seed(1256)
bootstrapped_corrs <- map(1:5000, function(i) bootstrap_corrs())
bootstrapped_corrs_gpa <- bind_rows(map(bootstrapped_corrs, function(mat) as.list(mat[1,])))
bootstrapped_corrs_scrt <- bind_rows(map(bootstrapped_corrs, function(mat) as.list(mat[2,])))
bootstrapped_corrs_scrtooc <- bind_rows(map(bootstrapped_corrs, function(mat) as.list(mat[3,])))
corrs_cont <- cor(ind_avr_df_rel %>% select_if(is.numeric))
malemeans <- ind_avr_df_rel %>% select(male, gpa, screentime,screentime_outofclass) %>% group_by(male) %>% summarise_all(mean)
malemeansdiff <- c(unlist(malemeans[2,-1] - malemeans[1,-1]))
smokermeans <- ind_avr_df_rel %>% select(smoker, gpa, screentime,screentime_outofclass) %>% group_by(smoker) %>% summarise_all(mean)
smokermeansdiff <- c(unlist(smokermeans[2,-1] - smokermeans[1,-1]))
corrs_gpa <- c(corrs_cont[1,],malemeansdiff['gpa'], smokermeansdiff['gpa'])
names(corrs_gpa) <- c(names(corrs_gpa)[1:14],c('male','smoker'))
corrs_scrt <- c(corrs_cont[2,],malemeansdiff['screentime'], smokermeansdiff['screentime'])
names(corrs_scrt) <- c(names(corrs_scrt)[1:14],c('male','smoker'))
corrs_scrtooc <- c(corrs_cont[3,],malemeansdiff['screentime_outofclass'], smokermeansdiff['screentime_outofclass'])
names(corrs_scrtooc) <- c(names(corrs_scrtooc)[1:14],c('male','smoker'))

collect_corrs <- function(corrs, booted) {
  booted <- booted %>% name_scheme(type='contcat') %>%
    rename('Smartphone use in-class'=screentime,'Grade average'=gpa, 'Smartphone use out-of-class'=screentime_outofclass)
  lwr <- booted %>% summarise_all(function(col) quantile(col,0.05)) %>% gather()
  upr <- booted %>% summarise_all(function(col) quantile(col,0.95)) %>% gather()
  res <- data_frame(partner=names(booted),cor=corrs,lwr=lwr$value,upr=upr$value)
  res %>% mutate(me=ifelse(lwr==1,T,F))
}
corrs_gpa_df <- collect_corrs(corrs_gpa,bootstrapped_corrs_gpa)
corrs_scrt_df <- collect_corrs(corrs_scrt,bootstrapped_corrs_scrt)
corrs_scrtooc_df <- collect_corrs(corrs_scrtooc,bootstrapped_corrs_scrtooc)

plot_corrs_cont <- function(corrs, title, ylabels) {
  corrs <- corrs %>% filter(!is.element(partner,c('Male','Smoker'))) %>%
    mutate(partner = factor(partner, levels = rev(c('Grade average', 'Smartphone use in-class','Smartphone use out-of-class',
                                                    'High school grade average',
                                                    'Age', 'Parents\' mean income', 'Parents\' max years of education',
                                                    'BMI','Agreeableness', 'Extraversion',
                                                    'Neuroticism', 'Openness', 'Conscientiousness',
                                                    'Locus of control'))))
  #min_y <- min(corrs$lwr)
  #max_y <- max(corrs[!corrs$me,]$upr)
  p <- corrs %>%
    ggplot(aes(x=partner)) +
    geom_hline(yintercept=0,color='steelblue') +
    geom_point(aes(y=cor)) +
    geom_errorbar(aes(ymin=lwr,ymax=upr),width=0) +
    scale_y_continuous(breaks=seq(-1,1,0.25),limits=c(-0.55, 0.75)) +
    ylab('Correlation') +
    coord_flip() +
    ggtitle(title)
  if (ylabels) {
    p <- p + theme(axis.title.y = element_blank(),
                   #axis.title.x = element_text(size=12),
                   #axis.text = element_text(size=11),
                   plot.title = element_text(hjust=0.5))
  }
  else {
    p <- p + theme(axis.title.y = element_blank(),
                   #axis.title.x = element_text(size=12),
                   #axis.text.x = element_text(size=11),
                   axis.text.y = element_blank(),
                   axis.ticks.y = element_blank(),
                   plot.title = element_text(hjust=0.5))
  }
  p
}
cor_cont_gpa_plt <- plot_corrs_cont(corrs_gpa_df,'Grade average',T)
cor_cont_scrt_plt <- plot_corrs_cont(corrs_scrt_df,'Smartphone use in-class',F)
cor_cont_scrtooc_plt <- plot_corrs_cont(corrs_scrtooc_df,'Smartphone use out-of-class',F)

plot_corrs_cat <- function(corrs, ylabels) {
  corrs <- corrs %>% filter(is.element(partner,c('Male','Smoker'))) %>%
    mutate(partner = factor(partner, levels = rev(c('Male','Smoker'))))
  #min_y <- min(corrs$lwr)
  #max_y <- max(corrs[!corrs$me,]$upr)
  p <- corrs %>%
    ggplot(aes(x=partner)) +
    geom_hline(yintercept=0,color='steelblue') +
    geom_point(aes(y=cor)) +
    geom_errorbar(aes(ymin=lwr,ymax=upr),width=0) +
    scale_y_continuous(limits=c(-3.5, 3), breaks = seq(-3,3,1)) +
    ylab('Difference in group means') +
    coord_flip()
  if (ylabels) {
    p <- p + theme(axis.title.y = element_blank(),
                   #axis.title.x = element_text(size=12),
                   #axis.text = element_text(size=11),
                   plot.title = element_text(hjust=0.5))
  }
  else {
    p <- p + theme(axis.title.y = element_blank(),
                   #axis.title.x = element_text(size=12),
                   #axis.text.x = element_text(size=11),
                   axis.text.y = element_blank(),
                   axis.ticks.y = element_blank(),
                   plot.title = element_text(hjust=0.5))
  }
  p
}
cor_cat_gpa_plt <- plot_corrs_cat(corrs_gpa_df,T)
cor_cat_scrt_plt <- plot_corrs_cat(corrs_scrt_df,F)
cor_cat_scrtooc_plt <- plot_corrs_cat(corrs_scrtooc_df,F)


library(gridExtra)
grid.arrange(grobs=list(cor_cont_gpa_plt, cor_cont_scrt_plt, cor_cont_scrtooc_plt,
                        cor_cat_gpa_plt + theme(plot.margin = margin(5.5,5.5,5.5,100,'pt')),
                        cor_cat_scrt_plt,cor_cat_scrtooc_plt),
             nrow=2, widths = c(20,13,13), heights = c(16, 4))

