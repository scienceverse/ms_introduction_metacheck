#Various utility/helper functions used in the other code files
#will be source()'d in where needed, no need to run directly

#Loads required packages (installs first if not already installed)
#for data reading, writing, and processing
if(!require(data.table)){install.packages("data.table"); library(data.table)}
if(!require(tidyr)){install.packages("tidyr"); library(tidyr)}
#for statistical power calculations
if(!require(pwr)){install.packages("pwr"); library(pwr)}
#for making train-test and cross-validation splits of data
if(!require(caret)){install.packages("caret"); library(caret)}
#for fitting ridge regression models
if(!require(glmnet)){install.packages("glmnet"); library(glmnet)}
#for calculating AUC
if(!require(MLmetrics)){install.packages("MLmetrics"); library(MLmetrics)}
#for running XGBoost model
if(!require(xgboost)){install.packages("xgboost"); library(xgboost)}
if(!require(NMOF)){install.packages("NMOF"); library(NMOF)}
if(!require(dplyr)){install.packages("dplyr"); library(dplyr)}
if(!require(readxl)){install.packages("readxl"); library(readxl)}

get_meta<-function(metadata_filename){
  #loading and cleaning the metadata (non-textual data) about each paper
  #calculates post-hoc power and performs mean imputation on missing values
  #read file
  metadata <- fread(metadata_filename, colClasses = c(paper_id = "character"))

  #calculate post-hoc power based on effect size and sample size
  metadata[!(is.na(n.o) | is.na(effect_size.o)), 
           power.o := pwr.r.test(n = n.o, r = effect_size.o, sig.level = 0.05)$power]
  
  #replication projects, encoded into categorical variable
  #ml (includes ml1,2,3,4) - 49 studies
  #rpp - 96 studies
  #ssrp - 20 studies
  #ee - 18 studies
  #other (includes pn, rrr, spsi and odr) - 116 studies
  
  #collapse all the many labs projects (ml1, ml2, ml3, ml4) together into just "ml"
  metadata[, project_collapsed := sub("[1-4]", "", project)]
  #collapse pn, rrr, spsi, and odr into "other" category
  metadata[project_collapsed %chin% c("pn", "rrr", "spsi", "odr"), project_collapsed := "other"]
  #code into factor with "other" as first so that it's dropped as the reference level in regression
  metadata[, project_collapsed := factor(project_collapsed, levels = c("other", "ml", "ee", "ssrp", "rpp"))]

  #concatenate the paper ID and suffix to get the filename/study ID
  metadata[, study_id := paste0(paper_id, suffix)]
  
  #fill in missing keywords as discipline-specific "other"
  metadata[is.na(keyword), keyword := paste0(discipline, "_other")]
  metadata[keyword == "", keyword := paste0(discipline, "_other")]
  
  #move the ID columns to the front
  setcolorder(metadata, c("paper_id", "study_id"))
  return(metadata)
}

impute_missing_vals = function(dat, maxiter = 50, ntree = 100){
  #use random forest to iteratively impute handful of missing metadata variables
  dt.mf = missForest(dat, variablewise = TRUE, verbose = TRUE, 
                     maxiter = maxiter, ntree = ntree)$ximp
  
  #replace NAs with the imputed values
  dat[is.na(dat[,'log(n.o)']), 'log(n.o)'] = dt.mf[is.na(dat[,'log(n.o)']), 'log(n.o)']
  dat[is.na(dat[,'effect_size.o']), 'effect_size.o'] = dt.mf[is.na(dat[,'effect_size.o']), 'effect_size.o']
  dat[is.na(dat[,'power.o']), 'power.o'] = dt.mf[is.na(dat[,'power.o']), 'power.o']
  dat[is.na(dat[,'p_value.o']), 'p_value.o'] = dt.mf[is.na(dat[,'p_value.o']), 'p_value.o']
  dat[is.na(dat[,'`Full Prof Ratio`']), '`Full Prof Ratio`'] = dt.mf[is.na(dat[,'`Full Prof Ratio`']), '`Full Prof Ratio`']
  
  #return filled in dataset
  return(dat)
}

get_embed = function(embed_filename){
  #reads in the embedding data and cleans up the column names
  embed = fread(embed_filename, header = TRUE)
  colnames(embed)[-1] = paste('dim',colnames(embed)[-1],sep='')
  colnames(embed)[1] = "study_id"
  
  return(embed)
}

get_liwc<-function(liwc_filename, section){
  #liwc_filename: filepath to the LIWC 2015 output scores
  #section: the section of text to be analyzed
    #abstract: the abstract
    #fulltext: the main text of the paper except the abstract
    #study: the text describing the study being replicated
  #output: table of LIWC scores for the given section
  
  #read in LIWC scores and subset to relevant section
  liwc_all = fread(liwc_filename)
  
  liwc_section = liwc_all[grep(section, Filename)]
  #paper ID: just the numbers from the filename
  liwc_section[, paper_id := gsub("[^0-9]*", "", Filename)]
  #study ID: paper ID plus an optional suffix if multiple studies from same paper
    #i.e., removing section name and ".txt" extension
  liwc_section[, study_id := substring(Filename, nchar(section)+1, nchar(Filename)-4)]
  
  #remove papers that aren't actually included in text analysis
  if(section == "study"){
    liwc_section = liwc_section[study_id %chin% full_data$study_id]
  }else{
    liwc_section = liwc_section[paper_id %chin% full_data$paper_id]
  }
  
  #check which columns have non-zero values for at least 50% of documents
  keep_cols = liwc_section[, sapply(.SD, function(x){mean(x == 0) < 0.5})]
  #remove nonsense dictionaries
  keep_cols[c("netspeak", "informal", "filler", "nonflu", "swear")] = FALSE
  print(paste0("columns removed: ",paste0(names(keep_cols)[!keep_cols],collapse=", ")))
  
  #log-transform the word count
  liwc_section[,log_WC:=log(WC)]
  
  #don't need these anymore either
  keep_cols[c("Filename", "Segment", "WC")] = FALSE
  
  #keep only the columns that aren't too sparse
  liwc_section[, `:=`(which(!keep_cols), NULL)]
  
  #move the derived columns to the front
  setcolorder(liwc_section, c("paper_id", "study_id", "log_WC"))
  
  return(liwc_section)
}

get_narrative = function(narrative_filename, section){
  #assumes "full_data" exists in the environment
  #and will just modify in place to add narrative arc scores directly
  if(section == "study"){
    narrative = data.table(read_excel(narrative_filename, sheet = "study"))
    narrative[, study_id := ifelse(is.na(suffix), ID, paste0(ID, suffix))]
    full_data[narrative, `:=`(staging = `staging`,
                                    plot_progression = `plot progression`,
                                    cognitive_tension = `cognitive tension`),
              on = "study_id"]
  }else if(section == "fulltext"){
    narrative = data.table(read_excel(narrative_filename, sheet = "fulltext"))
    narrative[, ID := as.character(ID)]
    full_data[narrative, `:=`(staging = `staging`,
                              plot_progression = `plot progression`,
                              cognitive_tension = `cognitive tension`),
              on = c("paper_id" = "ID")]
  }else if(section == "abstract"){
    narrative = data.table(read_excel(narrative_filename, sheet = "abstract"))
    narrative[, ID := as.character(ID)]
    full_data[narrative, `:=`(staging = `staging`,
                                       plot_progression = `plot progression`,
                                       cognitive_tension = `cognitive tension`),
              on = c("paper_id" = "ID")]
  }
  
}

prepare_controls<-function(dat){
  #extract, transform, and dummy code (where applicable) paper metadata to control for, convert to matrix
  #make sure NAs don't get dropped (so they can be imputed later)
  options(na.action='na.pass')
  model.matrix(replicated ~ 
                 power.o +
                 p_value.o +
                 effect_size.o +
                 log(1+citations) +
                 log(n.o) +
                 n_authors.o +
                 discipline +
                 effect_type +
                 authors_male.o +
                 us_lab.o +
                 subjects.o +
                 num_refs +
                 `Full Prof Ratio` +
                 num_tables +
                 num_figures +
                 supp_material +
                 num_studies +
                 I(pub_year-2000)+
                 project_collapsed+
                 author_involvement, 
               data = dat)[,-1]
}

prepare_keywords=function(dat){
  #expand into dummy matrix
  keywords_dat<-model.matrix(~ 0 + keyword, dat)
  #drop the biggest "other" category for intercept identification
  #drop the biggest econ category to avoid collinearity with the econ discipline indicator
    #since econ-related keywords are nested with the econ discipline indicator
  return(keywords_dat[,-which(colnames(keywords_dat) %in% c("keywordCognitive_other", "keywordnon-cooperative games"))])
}

prepare_liwc = function(dat){
  #extracts the LIWC dictionary values and converts to matrix
  #assumes data table named "liwc" exists in environment with relevant column names
  liwc_colnames = setdiff(names(liwc),c("paper_id", "study_id"))
  as.matrix(dat[, ..liwc_colnames])
}

prepare_text<-function(dat){
  #extracts the average embeddings for each doc and converts to matrix
  as.matrix(dat[,.SD,.SDcols=patterns('dim')])
}

return_train_inds<-function(dat, p = 0.8){
  #randomly select fraction p of total papers as training set
  #note: since sometimes there are multiple studies from the same paper,
  #we avoid data leakage by splitting by *paper* ID, not *study* ID
 
  #slight complication: we want to make train/test splits balanced by replication outcome,
  #but are splitting by paper rather than study.
  #occasionally, a paper has studies with differing replication outcomes
  #in these cases, use "majority" outcome 
  #(e.g., if 2 out of 3 studies in a paper replicated, put it in the replicated stratum)
  #in the rare case of a tie, tie-break randomly
  #do this *inside* the loop over train/test splits so any randomness
  #arising from the tie-breaking also gets captured by the variability across splits
  
  #calculate number of replicated/non-replicated studies by each paper
  rep_summary = dat[, .N, by = c("paper_id", "replicated")]
  #generate random numbers for tie-breaking
  rep_summary[, rand := runif(.N)]
  #sort by paper_id, 
  #*then* by number of replicated/non-replicated studies (descending order)
  #*then* by random number to tie-break
  setorder(rep_summary, paper_id, -N, rand)
  
  #unique() keeps *first* observation of each paper_id
  #which by the above sorting means the majority outcome is retained
  paper_outcomes = unique(rep_summary[, .(paper_id, replicated)], by = "paper_id")
  
  #get a proportion p subsample balanced by replication outcomes
  trainIDs  <- createDataPartition(paper_outcomes$replicated, p = p, list = FALSE) #indices - balanced samples
  
  #map paper IDs back to study-level true/false
  traininds <- dat[, paper_id %in% paper_outcomes[trainIDs, paper_id]] #indices
  return(traininds)
}

train_ridge_model<-function(x, y, folds)
{
  #estimate a ridge regression model with penalty selected by cross-validation
  #range of possible ridge penalties
  lambdas = 10^seq(3, -2, by = -.1)
  
  #fit a ridge logistic regression using cross-validation
  #maximizing the validation AUC
  model = cv.glmnet(x,
                    alpha = 0,#ridge
                    lambda = lambdas,
                    y,
                    family = "binomial",
                    type.measure = "auc",
                    foldid = folds)
  return(model)
}

train_elasticnet_model<-function(x, y, folds)
{
  #I do not specify a lambda range for lasso regularization because we expect the range to be different than that specified for ridge.
  #I just allow the solver to pick a range
  
  #fit an elastic net logistic regression using cross-validation
  #maximizing the validation AUC
  model = cv.glmnet(x,
                    alpha = 0.5,#elastic net
                    y,
                    family = "binomial",
                    type.measure = "auc",
                    foldid = folds)
  return(model)
}


CV_xgboost_model<-function(x, y, folds)
{ # we only pass training data to this for the cross validation - training, validation split
  # cross validation + grid search across different parameters to find the xgboost task parameters with the best validation auc
  
  eval <- "auc"
  nrounds<-20
  
  # Parameter list to search
  params <- list(eta = c(0.7,0.1,0.01), #learning_rate
                 max_depth = c(2,3,4,6)) #maximum depth of the tree (more depth is more complex and more likely to over fit)
  
  xgbCV <- function (parameters) {
    
  #performs k-fold cross validation on a model that is fit with a given set of parameters passed to it by the grid search function defined later
    fit <- xgb.cv(
      metrics = eval,
      objective = 'binary:logistic',
      type="classification",
      data = x, #do we want to cross validate over only the training set? parameters would change every split with change in training data
      label = y, 
      # param = parameters,
      missing = NA, 
      folds=folds, 
      prediction = FALSE,
      scale_pos_weight = 1,
      maximize = TRUE, #minimize log loss, maximize auc
      nrounds = nrounds,
      verbose = FALSE,
      early_stopping_rounds = 3,
      eta = parameters$eta,
      max_depth = parameters$max_depth
    )
    
    idx <- fit$best_iteration
    val <- fit$evaluation_log[idx]$test_auc_mean
    return(val)
    }

  sol <- gridSearch(fun = xgbCV,
                    levels = params,
                    keepNames = TRUE,
                    asList = TRUE)
  
  aucs<-sol$values
  
  #return the hyperparameters associated with the highest auc values
  best_params <- list(eta=sol$levels[which.max(aucs)][[1]]$eta,max_depth=sol$levels[which.max(aucs)][[1]]$max_depth,objective = 'binary:logistic', eval_metric = eval,nrounds=8)
  return(best_params)
}


train_xgboost_model<-function(x,y,folds){
  #cross validate to find best parameters for the model
  solved_params<-CV_xgboost_model(x,y,folds)
  
  #training model with entire training dataset
  dtrain0 <- xgb.DMatrix(x, label = y)
  model   <- xgb.train(data = dtrain0,
                       param = solved_params,
                       nrounds=solved_params$nrounds)
  return(model)
}


#functions for evaluating test performance of ridge regression model
  #AUC, correlation between predictions and outcomes, classification accuracy
  calc_auc = function(x, y, model){
    MLmetrics::AUC(predict(model, newx = x, s = "lambda.min", type = "response"), y)
  }
  
  calc_auc_xgb = function(x, y, model){
    MLmetrics::AUC(predict(model, newdata = x), y)
  }

calc_all_performance = function(x, inds, y, fun, model_list, paper_study_map){
  #function for calculating test set accuracy of the 5 focal models
  
  #getting relevant x/y variables for bootstrap sample
  controls_b = x[inds,colnames(controls_mat)]
  text_b = x[inds,colnames(text_mat)]
  liwc_b = x[inds,colnames(liwc_mat)]
  y_b = y[inds]
  
  
  #calculating performance on the bootstrap test sample
  model1_r = do.call(fun,list(x=controls_b,y=y_b,model=model_list[[1]]))
  model2_r = do.call(fun,list(x=text_b,y=y_b,model=model_list[[2]]))
  model3_r = do.call(fun,list(x=cbind(controls_b,text_b),y=y_b,model=model_list[[3]]))
  model4_r = do.call(fun,list(x=liwc_b,y=y_b,model=model_list[[4]]))
  model5_r = do.call(fun,list(x=cbind(controls_b,liwc_b),y=y_b,model=model_list[[5]]))
  model6_r = do.call(fun,list(x=cbind(controls_b,liwc_b,text_b),y=y_b,model=model_list[[6]]))

  #outputting row vector of performance metrics
  res = cbind(model1_r,model2_r,model3_r,model4_r,model5_r,model6_r)
  return(res)
}

