#combines the one-at-a-time LIWC tables
#and puts them into a more readable form
library(data.table)

coef_tbl = fread( paste0("../results/fulltext_liwc_and_control_coefs.csv"))
fulltext_coef_tbl = coef_tbl[is.na(`Controls only (LASSO & unstandardized)`), .(Variable,
             b = `Coef for all controls & one-at-a-time LIWC (unregularized & standardized)`,
             p = `p-value for all controls & one-at-a-time LIWC (unregularized & standardized)`,
             se = `SE for all controls & one-at-a-time LIWC (unregularized & standardized)`)][
               , .(Variable,
                   b_fulltext = paste0(round(b, 3), ifelse(p<.001, "***",
                                                  ifelse(p<.01, "**",
                                                         ifelse(p<.05, "*",
                                                                ifelse(p<0.1, "+", ""))))),
                   se_fulltext = paste0("[", round(se, 3), "]"))
             ]

coef_tbl = fread(paste0("../results/study_liwc_and_control_coefs.csv"))
study_coef_tbl = coef_tbl[is.na(`Controls only (LASSO & unstandardized)`), .(Variable,
                                                            b = `Coef for all controls & one-at-a-time LIWC (unregularized & standardized)`,
                                                            p = `p-value for all controls & one-at-a-time LIWC (unregularized & standardized)`,
                                                            se = `SE for all controls & one-at-a-time LIWC (unregularized & standardized)`)][
                                                              , .(Variable,
                                                                  b_study = paste0(round(b, 3), ifelse(p<.001, "***",
                                                                                                          ifelse(p<.01, "**",
                                                                                                                 ifelse(p<.05, "*",
                                                                                                                        ifelse(p<0.1, "+", ""))))),
                                                                  se_study = paste0("[", round(se, 3), "]"))
                                                            ]

coef_tbl = fread(paste0("../results/abstract_liwc_and_control_coefs.csv"))
abstract_coef_tbl = coef_tbl[is.na(`Controls only (LASSO & unstandardized)`), .(Variable,
                                                            b = `Coef for all controls & one-at-a-time LIWC (unregularized & standardized)`,
                                                            p = `p-value for all controls & one-at-a-time LIWC (unregularized & standardized)`,
                                                            se = `SE for all controls & one-at-a-time LIWC (unregularized & standardized)`)][
                                                              , .(Variable,
                                                                  b_abstract = paste0(round(b, 3), ifelse(p<.001, "***",
                                                                                                          ifelse(p<.01, "**",
                                                                                                                 ifelse(p<.05, "*",
                                                                                                                        ifelse(p<0.1, "+", ""))))),
                                                                  se_abstract = paste0("[", round(se, 3), "]"))
                                                            ]


fulltext_coef_tbl[study_coef_tbl, `:=`(b_study = b_study,
                                       se_study = se_study),
                  on = "Variable"]
fulltext_coef_tbl[abstract_coef_tbl, `:=`(b_abstract = b_abstract,
                                       se_abstract = se_abstract),
                  on = "Variable"]
fulltext_coef_tbl[, `:=`(b_study = ifelse(is.na(b_study), "N/A", b_study),
                         se_study = ifelse(is.na(se_study), "", se_study),
                         b_abstract = ifelse(is.na(b_abstract), "N/A", b_abstract),
                         se_abstract = ifelse(is.na(se_abstract), "", se_abstract))]

neat_table = data.table(Variable = rep("", nrow(fulltext_coef_tbl)*2))

neat_table[1:.N %% 2 != 0, `:=`(Variable = fulltext_coef_tbl$Variable,
                                Study = fulltext_coef_tbl$b_study,
                                `Full Text` = fulltext_coef_tbl$b_fulltext,
                                Abstract = fulltext_coef_tbl$b_abstract)]
neat_table[1:.N %% 2 == 0, `:=`(Study = fulltext_coef_tbl$se_study,
                                `Full Text` = fulltext_coef_tbl$se_fulltext,
                                Abstract = fulltext_coef_tbl$se_abstract)]

fwrite(neat_table, paste0("../results/one_at_a_time_liwc_coefficients_neat.csv"))