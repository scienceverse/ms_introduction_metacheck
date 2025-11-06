#a minimal Unix loop that can be run in command line to run this script with all parameter settings
#for i in {1..3}; do SGE_TASK_ID=$i Rscript ./interpretation_main.R; done

#NOTE: by default, when you open the file R should make "code" your working directory
#if not, setwd() first to make sure you're in the right place.

#This file runs the LIWC portion of the interpretation analysis to understand what kind 
#of textual features correlate positively/negatively to replication. Also includes
#measures of obfuscation and readability from past literature.
#This includes:
  #a single LASSO logistic regression including all metadata variables and LIWC dictionaries
    #here, only the LIWC dictionaries are penalized (LASSO penalty is 0 for metadata variables)
    #to ensure that the estimated LIWC coefficients are fully residual of metadata
  #one-at-a-time regressions where we include all metadata variables + a single LIWC dictionary
    #here, the model spec is an unregularized logistic regression; this gets correlation b/t 
    #a LIWC dictionary and replication residual of metadata but not other LIWC dictionaries

#read in various helper functions
#warning that this will install a bunch of packages (if you don't have them already)
source("utils.R")

liwc_filename = "../processed_data/liwc2015_scores.csv"
metadata_filename = "../processed_data/metadata_imputed.csv"
readability_filename = "../processed_data/readability_scores.csv"

full_data = get_meta(metadata_filename)

#if running as a task array
task_id = as.integer(Sys.getenv("SGE_TASK_ID"))

#one of abstract, study, or fulltext
section = "abstract"
# section = c("abstract", "fulltext", "study")[((task_id-1) %% 3) + 1]


#LIWC outputs
liwc = get_liwc(liwc_filename, section)

#join metadata and LIWC data
if(section == "study"){
  #when at study level, there is 1 row in the LIWC data per study
  full_data = full_data[liwc, on = "study_id"]
}else{
  #when at abstract or fulltext level, many-to-one mapping
  #because a single paper can have multiple studies
  full_data = full_data[liwc, on = "paper_id"]
}

#read in readability data
readability_table = fread(readability_filename)

#merge into data
full_data[readability_table, readability := get(paste0(section, "_readability")), on = "study_id"]

#obfuscation scores: Markowitz and Hancock (2016)
#obfuscation formula: jargon + abstraction + causation - positive emotion - reading ease
#jargon is just % of words not in LIWC dictionary
full_data[, jargon := 100 - Dic]
#abstraction is "the inverse of the sum of the standardized LIWC scores 
#for articles, prepositions, and quantifiers"
#I think by "inverse" here they must mean negative, as 1/x would make no sense
#when summing standardized variables which could be pos or neg
full_data[, abstraction := -(scale(article) + scale(prep) + scale(quant))]
full_data[, obfuscation := scale(cause) + scale(jargon) + scale(abstraction) - 
            scale(readability) - scale(posemo)]


#sort data and get matrices of covariates
setkey(full_data, study_id)

#cuz prepare_liwc uses column names of liwc to determine which variables to grab
#can just add blank columns with the relevant names

liwc[, `:=`(c("readability", "abstraction", "obfuscation"), NA)]
liwc_mat = prepare_liwc(full_data)

#To reduce the high collinearity of the LIWC variables passed to the LASSO, we exclude summary variables as well as 
#readability and obfuscation measures (these are a linear transformation of other LIWC dictionaries)
#however, we leave them in for one-at-a-time regressions (where collinearity isn't a concern)
extra_cols_remove = c("function", "pronoun", "ppron", "affect", "negemo", "informal",
                      "social", "cogproc", "percept", "bio", "drives", "relativ",
                      "AllPunc", "Authentic", "Analytic", "Clout", "Tone", "Dic",
                      "readability", "abstraction", "obfuscation")
liwc_mat_alt = liwc_mat[,!(colnames(liwc_mat) %in% extra_cols_remove)]
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


#make cross-validation folds to tune LASSO penalty
set.seed(27)
folds = createFolds(full_data$replicated, k = 10, list = FALSE)

#only metadata variables
controls_lasso = cv.glmnet(x = controls_mat,
                           y = full_data$replicated,
                           foldid = folds,
                           family = "binomial",
                           alpha = 1,
                           type.measure = "auc")

controls_coef = coef(controls_lasso, s = "lambda.min")

#make sure that metadata variables don't get penalized
penalty_factor = c(rep(0, ncol(controls_mat)),
                   rep(1, ncol(liwc_mat_alt)))


#all metadata and LIWC variables
liwc_lasso = cv.glmnet(x = cbind(controls_mat, liwc_mat_alt),
                       y = full_data$replicated,
                       foldid = folds,
                       penalty.factor = penalty_factor,
                       family = "binomial",
                       alpha = 1,
                       type.measure = "auc")
liwc_coefs = coef(liwc_lasso, s = "lambda.min")

#combine all into a table
coef_tbl = merge(data.table(as.matrix(controls_coef), keep.rownames = TRUE),
                 data.table(as.matrix(liwc_coefs), keep.rownames = TRUE),
                 by = "rn", all = TRUE)

#add variable names
tmp = data.table(rn = rownames(liwc_coefs), row = 1:nrow(liwc_coefs))
coef_tbl[tmp, row := row, on = "rn"]
setorder(coef_tbl, row)
coef_tbl[, row := NULL]

#one-at-a-time regressions

#We exclude high level LIWC dictionaries since we report the dictionaries nested under each of these. 
#The parent dictionaries contain all the words of its nested dictionaries.
high_level_cols = c("function", "pronoun", "ppron", "affect", "negemo", "informal",
                    "social", "cogproc", "percept", "bio", "drives", "relativ")

liwc_mat_alt2 = liwc_mat[,!(colnames(liwc_mat) %in% high_level_cols)]

#first get coefficients for metadata variables by themselves
controls_glm = glm(full_data$replicated ~ controls_mat,
                   family = binomial())
alt_coef_mat = matrix(nrow = ncol(controls_mat)+ncol(liwc_mat_alt2) + 1, ncol = 4)
alt_coef_mat[1:(ncol(controls_mat)+1),] = summary(controls_glm)$coef

#loop through LIWC dictionaries and run regression using
# all metadata + a single LIWC dictionary
for(i in 1:ncol(liwc_mat_alt2)){
  alt_coef_mat[ncol(controls_mat) + 1 + i,] =
    summary(glm(full_data$replicated ~ controls_mat + liwc_mat_alt2[,i],
      family = binomial()))$coef[ncol(controls_mat) + 2,]
}

alt_coef_mat = data.table(alt_coef_mat,rn = c('(Intercept)',colnames(controls_mat),colnames(liwc_mat_alt2)))
#merge coefficient tables
coef_tbl = merge(data.table(coef_tbl, keep.rownames = TRUE),
                 data.table(alt_coef_mat, keep.rownames = TRUE),
                 by = "rn", all = TRUE)

#add variable names
tmp = data.table(rn = c('(Intercept)',colnames(controls_mat),colnames(liwc_mat_alt2)), row = 1:nrow(alt_coef_mat))
coef_tbl[tmp, row := row, on = "rn"]
setorder(coef_tbl, row)
coef_tbl[, row := NULL]

#calculate standardized coefficients for comparability (multiply each coefficient by SD of X)
col_sds = c(Intercept = NA, apply(cbind(controls_mat, liwc_mat_alt2), 2, sd))
standardized_coef_tbl = coef_tbl[, lapply(.SD, function(x){x*col_sds}), .SDcols = 2:5]

#output big table of all coefficients, standardized and unstandardized
coef_tbl = cbind(coef_tbl, standardized_coef_tbl, coef_tbl[, .(V3, V4)])
colnames(coef_tbl) = c("Variable",
                       "Controls only (LASSO & unstandardized)",
                       "All included (LASSO & unstandardized)",
                       "Coef for all controls & one-at-a-time LIWC (unregularized & unstandardized)",
                       "SE for all controls & one-at-a-time LIWC (unregularized & unstandardized)",
                       "z-score for all controls & one-at-a-time LIWC (unregularized & unstandardized)",
                       "p-value for all controls & one-at-a-time LIWC (unregularized & unstandardized)",
                       "Controls only (LASSO & standardized)",
                       "All included (LASSO & standardized)",
                       "Coef for all controls & one-at-a-time LIWC (unregularized & standardized)",
                       "SE for all controls & one-at-a-time LIWC (unregularized & standardized)",
                       "z-score for all controls & one-at-a-time LIWC (unregularized & standardized)",
                       "p-value for all controls & one-at-a-time LIWC (unregularized & standardized)")
fwrite(coef_tbl, paste0("../results/", section, "_liwc_and_control_coefs.csv"))