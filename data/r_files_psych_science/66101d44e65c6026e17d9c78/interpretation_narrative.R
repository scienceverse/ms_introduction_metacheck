#NOTE: by default, when you open the file R should make "code" your working directory
#if not, setwd() first to make sure you're in the right place.

#This file runs the narrative arcs portion of the interpretation analysis

#read in various helper functions
#warning that this will install a bunch of packages (if you don't have them already)
source("utils.R")
library(readxl)

metadata_filename = "../processed_data/metadata_imputed.csv"

#non-textual data about each paper
full_data = get_meta(metadata_filename)

setkey(full_data, study_id)

#get narrative arc scores
abstract_narrative = data.table(read_excel("../processed_data/narrative_arc.xlsx", sheet = "abstract"))
abstract_narrative[, ID := as.character(ID)]
fulltext_narrative = data.table(read_excel("../processed_data/narrative_arc.xlsx", sheet = "fulltext"))
fulltext_narrative[, ID := as.character(ID)]
study_narrative = data.table(read_excel("../processed_data/narrative_arc.xlsx", sheet = "study"))
study_narrative[, study_id := ifelse(is.na(suffix), ID, paste0(ID, suffix))]

#merge into metadata (ignore "grand narrative" as it is just the mean of the other 3)
full_data[abstract_narrative, `:=`(abstract_staging = `staging`,
                                   abstract_plot_progression = `plot progression`,
                                   abstract_cognitive_tension = `cognitive tension`),
          on = c("paper_id" = "ID")]
full_data[fulltext_narrative, `:=`(fulltext_staging = `staging`,
                                   fulltext_plot_progression = `plot progression`,
                                   fulltext_cognitive_tension = `cognitive tension`),
          on = c("paper_id" = "ID")]
full_data[study_narrative, `:=`(study_staging = `staging`,
                                study_plot_progression = `plot progression`,
                                study_cognitive_tension = `cognitive tension`),
          on = "study_id"]

#get matrix of all features of interest (standardized)
scores_mat = scale(as.matrix(full_data[, .(abstract_staging, abstract_plot_progression, abstract_cognitive_tension,
                                           fulltext_staging, fulltext_plot_progression, fulltext_cognitive_tension,
                                           study_staging, study_plot_progression, study_cognitive_tension)]))
abstract_scores = scale(as.matrix(full_data[, .(abstract_staging, 
                                                abstract_plot_progression, 
                                                abstract_cognitive_tension)]))
fulltext_scores = scale(as.matrix(full_data[, .(fulltext_staging, 
                                                fulltext_plot_progression, 
                                                fulltext_cognitive_tension)]))
study_scores = scale(as.matrix(full_data[, .(study_staging, 
                                             study_plot_progression, 
                                             study_cognitive_tension)]))

controls_mat = prepare_controls(full_data)

#keep only keywords that have at least 2 each of successful/unsuccessful replications
sparse_keywords = full_data[, !((sum(replicated) > 1) & (sum(!replicated) > 1)), by = "keyword"][V1 == TRUE, keyword]
#make these sparse keywords collapsed into [discipline]_other
full_data[keyword %chin% sparse_keywords, keyword := paste0(discipline, "_other")]
#for econ specifically, the only sparse category is "group behavior"
#lump it in with non-cooperative games
full_data[keyword == "Economics_other", keyword := "non-cooperative games"]
keyword_mat = prepare_keywords(full_data)
controls_mat = cbind(controls_mat, keyword_mat)

#remove observations with missing abstract scores (no abstract or <100 words)
missing_abstract_inds = is.na(abstract_scores[,1])
abstract_scores_subset = abstract_scores[!missing_abstract_inds,]

#after excluding papers with short abstracts, only 1 paper left in "consumer economics" keyword
#drop this keyword column (implicitly gets lumped in with "non-cooperative games" baseline)
#Bartling, Fehr, and Schmidt (2012) is a principal-agent game so this is appropriate
keyword_to_drop = which(colnames(controls_mat) == "keywordconsumer economics")
controls_mat_subset = controls_mat[!missing_abstract_inds,-keyword_to_drop]

#run joint logistic regression of all metadata and summary features
#separately for each section
abstract_glm = glm(replicated ~ controls_mat_subset + abstract_scores_subset, 
                   data = full_data[!missing_abstract_inds,], family = binomial())
fulltext_glm = glm(replicated ~ controls_mat + fulltext_scores, data = full_data, family = binomial())
study_glm = glm(replicated ~ controls_mat + study_scores, data = full_data, family = binomial())

#pull out the coefficients for the narrative arc scores
joint_coefs = rbind(summary(abstract_glm)$coef[-(1:(ncol(controls_mat_subset)+1)),],
                    summary(fulltext_glm)$coef[-(1:(ncol(controls_mat)+1)),],
                    summary(study_glm)$coef[-(1:(ncol(controls_mat)+1)),])

#for each summary feature, run unregularized logistic regression with just that 1 feature
#(with and without metadata) and store in matrix
alt_coefs = matrix(ncol = 4, nrow = ncol(scores_mat))
for(i in 1:ncol(scores_mat)){
  if(grepl("abstract", colnames(scores_mat)[i])){
    tmp = glm(replicated ~ controls_mat_subset + scores_mat[!missing_abstract_inds, i],
              data = full_data[!missing_abstract_inds], family = binomial())
    alt_coefs[i,] = summary(tmp)$coef[-(1:(ncol(controls_mat_subset)+1)),]
  }else{
    tmp = glm(replicated ~ controls_mat + scores_mat[, i], data = full_data, family = binomial())
    alt_coefs[i,] = summary(tmp)$coef[-(1:(ncol(controls_mat)+1)),]
  }
}


#put coefficients in a table and save down
coef_tbl = data.table(colnames(scores_mat),
           joint_coefs,
           alt_coefs)
colnames(coef_tbl) = c("Variable", 
                       "Coef for joint regression (all controls and scores; one section at a time)",
                       "SE for joint regression (all controls and scores; one section at a time)",
                       "z-score for joint regression (all controls and scores; one section at a time)",
                       "p-value for joint regression (all controls and scores; one section at a time)",
                       "Coef for one-at-a-time regression (with controls)",
                       "SE for one-at-a-time regression (with controls)",
                       "z-score for one-at-a-time regression (with controls)",
                       "p-value for one-at-a-time regression (with controls)")
fwrite(coef_tbl, paste0("../results/narrative_arc_coefs.csv"))

coef_abstract = summary(abstract_glm)$coef
coef_study = summary(study_glm)$coef
coef_fulltext = summary(fulltext_glm)$coef

#make an "NA" row for the dropped keyword in the abstract columns
dropped_keyword_ind = grep("consumer economics", rownames(coef_fulltext))
coef_abstract = rbind(coef_abstract[1:(dropped_keyword_ind-1),], rep(NA, 4), 
                      coef_abstract[-(1:(dropped_keyword_ind-1)),])

coef_all = data.table(b_study = paste0(round(coef_study[,1], 3),
                            ifelse(coef_study[,4] < .001, "***",
                                   ifelse(coef_study[,4] < .01, "**",
                                          ifelse(coef_study[,4] < .05, "*",
                                                 ifelse(coef_study[,4] < .1, "+", ""))))),
           se_study = paste0("[", round(coef_study[,2], 3), "]"),
           b_fulltext = paste0(round(coef_fulltext[,1], 3),
                               ifelse(coef_fulltext[,4] < .001, "***",
                                      ifelse(coef_fulltext[,4] < .01, "**",
                                             ifelse(coef_fulltext[,4] < .05, "*",
                                                    ifelse(coef_fulltext[,4] < .1, "+", ""))))),
           se_fulltext = paste0("[", round(coef_fulltext[,2], 3), "]"),
           b_abstract = paste0(round(coef_abstract[,1], 3),
                               ifelse(coef_abstract[,4] < .001, "***",
                                      ifelse(coef_abstract[,4] < .01, "**",
                                             ifelse(coef_abstract[,4] < .05, "*",
                                                    ifelse(coef_abstract[,4] < .1, "+", ""))))),
           se_abstract = paste0("[", round(coef_abstract[,2], 3), "]"))

neat_table = data.table(Variable = rep("", nrow(coef_all)*2))

neat_table[1:.N %% 2 != 0, `:=`(Variable = c("Intercept", colnames(controls_mat),
                                             "Staging", "Plot Progression",
                                             "Cognitive Tension"),
                                Study = coef_all$b_study,
                                `Full Text` = coef_all$b_fulltext,
                                Abstract = coef_all$b_abstract)]
neat_table[1:.N %% 2 == 0, `:=`(Study = coef_all$se_study,
                                `Full Text` = coef_all$se_fulltext,
                                Abstract = coef_all$se_abstract)]

fwrite(neat_table, paste0("../results/arc_of_narrative_coefficients_neat.csv"))
