source("utils.R")
library("missForest")

liwc_filename = "../processed_data/liwc2015_scores.csv"
metadata_filename = "../processed_data/metadata_2024.csv"

full_data = get_meta(metadata_filename)
# sort data and get matrices of covariates
setkey(full_data, study_id)

controls_mat = prepare_controls(full_data)
controls_cols = colnames(controls_mat)

#put together joint matrix of metadata/keywords
keyword_mat = prepare_keywords(full_data)
controls_mat = cbind(controls_mat, keyword_mat)

#use random forest imputation (relatively large # of trees to reduce sampling variability)
set.seed(24)
controls_mat = impute_missing_vals(controls_mat, ntree = 10000)

# imputed variables are log(n.o), effect size, power, p-value, full prof ratio
#sample size is integer, overwrite with decimal (otherwise imputed values get rounded)
full_data[, n.o := as.double(n.o)]
full_data[is.na(n.o), n.o := exp(controls_mat[is.na(full_data$n.o),'log(n.o)'])]
full_data[is.na(effect_size.o), effect_size.o := 
            controls_mat[is.na(full_data[,effect_size.o]), 'effect_size.o']]
full_data[is.na(power.o), power.o := 
            controls_mat[is.na(full_data[,power.o]), 'power.o']]
full_data[is.na(p_value.o), p_value.o := 
            controls_mat[is.na(full_data[,p_value.o]), 'p_value.o']]
full_data[is.na(`Full Prof Ratio`), `Full Prof Ratio` := 
            controls_mat[is.na(full_data[,`Full Prof Ratio`]), '`Full Prof Ratio`']]

# join metadata and embeddings
fwrite(full_data, "../processed_data/metadata_imputed.csv")