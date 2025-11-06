library(splines)

source('main_models.R')

screentime_knots_clm <- quantile(ind_avr_df$screentime, c(0.25,0.5,0.75))
clm_nonlin <- lm(paste0('gpa~ns(screentime, knots=screentime_knots_clm)+',paste0(ctrls,collapse='+')),ind_avr_df)
anova(clm,clm_nonlin)$'Pr(>F)'[2]
AIC(clm)
AIC(clm_nonlin)

screentime_knots_plm <- quantile(course_ind_df$screentime, c(0.25,0.5,0.75))
pooled <- lm(paste0('grade',form),data=course_ind_df)
pooled_nonlin <- lm(paste0('grade~ns(screentime,knots=screentime_knots_plm)+',
                           paste0(ctrls,collapse='+')),
                    course_ind_df)
anova(pooled,pooled_nonlin)$'Pr(>F)'[2]
AIC(pooled)
AIC(pooled_nonlin)

fe_both_nonlin <- lm(grade~ns(screentime,knots=screentime_knots_plm)+user_idx+course_num_sem,
                     course_ind_df)
anova(fe_both,fe_both_nonlin)$'Pr(>F)'[2]
AIC(fe_both)
AIC(fe_both_nonlin)












