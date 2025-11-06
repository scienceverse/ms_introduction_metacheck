#NOTE: by default, when you open the file R should make "code" your working directory
#if not, setwd() first to make sure you're in the right place.

#This file runs a supplementary analysis where we train alternate models to our main ridge model on the training set,
#then calculate the AUC of the models on the test set
#The model being trained in this script is the XGboost trees model
#predictions are calculated for 6 models:
  #metadata (control) variables only
  #Word2vec embedding summaries only
  #metadata + Word2vec embedding summaries
  #LIWC dictionaries only
  #metadata + LIWC dictionaries
  #metadata + LIWC dictionaries + Word2vec embeddings

#how many processor threads to use for parallel computation
#set to 1 to run serially
ncores = 100
#how many train-test splits to use
#we use 10,000 in the paper for precision but if running locally, I recommend 100
#10,000 takes about 50 minutes on 15 cores
nrep = 10000

########### Script settings
#this section determines which version of the analysis to run:
#section: which text of the paper to analyze (abstract, main text, or just the study being replicated)
#by default runs for the study section

#optionally can run all different versions of the analysis in a command line task array
#create a task array from and define the task ID below to determine data settings to use
#tasks from 1-3
#line below shows example if using Sun Grid Engine task arrays
# task_id = as.integer(Sys.getenv("SGE_TASK_ID"))

#which section of the papers to run the analyses on
  #abstract: the abstract
  #fulltext: the main text of the paper except the abstract
  #study: the text describing the study being replicated
section = "study"
#if running as a task array:
  #abstract: task_id = 1
  #fulltext: task_id = 2
  #study: task_id = 3
# section = c("abstract", "fulltext", "study")[((task_id-1) %% 3) + 1]

#which Word2vec model to use for the analysis
#for this robustness check we only use our "main" specification
embedding_params = "sg0_window3_dim100_epochs50_stopwords1"

print(paste("Calculating", nrep, "train-test splits for", section, 
            "on", ncores, "threads using XGboost"))

########### Loading in packages, helper functions, and data
#read in various helper functions
#warning that this will install a bunch of packages (if you don't have them already)
source("utils.R")
#used for parallelization
#if running on a windows system, use doPar instead of doMC
library(doMC)
#ensures that random seeds are consistent when using parallelization
library(doRNG)

#register parallel backend
registerDoMC(ncores)

#the five datasets I read are - metadata, embeddings file, liwc dictionaries files, files with the readability scores and the narrative arc scores
#we specify the filename of the embedding file we want to use for this analyses and the section of the text we would be using for liwc
#to use a different section of the academic papers (study/fulltext/abstract), change the embedding filename and the section argument of get_liwc function
#also supplement LIWC dictionaries with Flesch reading ease scores and narrative arc scores
liwc_filename = "../processed_data/liwc2015_scores.csv"
embed_filename = paste0("../processed_data/", section, "_embeddings_", embedding_params, ".csv")
metadata_filename = "../processed_data/metadata_imputed.csv"
readability_filename = "../processed_data/readability_scores.csv"
narrative_filename = "../processed_data/narrative_arc.xlsx"

#non-textual data about each paper
full_data = get_meta(metadata_filename)
#LIWC outputs
liwc = get_liwc(liwc_filename, section)
#average word embeddings per document
embed = get_embed(embed_filename)
#join metadata and embeddings
full_data = full_data[embed, on = "study_id"]

#join metadata and LIWC data
if(section == "study"){
  #when at study level, there is 1 row in the LIWC data per study
  full_data = full_data[liwc, on = "study_id"]
}else{
  #when at abstract or fulltext level, many-to-one mapping
  #because a single paper can have multiple studies
  full_data = full_data[liwc, on = "paper_id"]
}

#just to make sure the ordering of obs is the same regardless of section
setkey(full_data, study_id)

#read in readability data
readability_table = fread(readability_filename)

#merge into data
full_data[readability_table, readability := get(paste0(section, "_readability")), on = "study_id"]

#don't bother with obfuscation and abstraction since these are linear combinations
#of other LIWC variables

#arc of narrative
#no assignment needed as this does modification-in-place of "full_data"
get_narrative(narrative_filename, section)

#cuz prepare_liwc uses column names of liwc to determine which variables to grab
#can just add blank columns with the relevant names
liwc[, `:=`(c("readability", "staging", "plot_progression", "cognitive_tension"), NA)]

#preparing our inputs for the models
#basically subsetting relevant columns and converting to numeric matrices
text_mat = prepare_text(full_data)
liwc_mat = prepare_liwc(full_data)
controls_mat = prepare_controls(full_data)

#add keyword dummies to metadata
keywords_mat = prepare_keywords(full_data)
controls_mat = cbind(controls_mat, keywords_mat)

if(section == "abstract"){
  #remove observations with abstracts <100 words (narrative arc missing for these obs)
  short_abstracts_inds = is.na(liwc_mat[, "staging"])
  controls_mat = controls_mat[!short_abstracts_inds,]
  text_mat = text_mat[!short_abstracts_inds,]
  liwc_mat = liwc_mat[!short_abstracts_inds,]
  full_data = full_data[!short_abstracts_inds]
}

#-------------------------------------------------------------------------------------------------------------------------
#This is where the actual analysis starts

#To run our prediction models, we use nested cross validation loops
#the outer loop iterates through different train-test draws and within the inner loop, 
#each model goes through a cross validation loop to select optimal hyperparameters

start_time = Sys.time()

set.seed(903)

out_list = foreach(i = 1:nrep) %dorng% {
  #train-test draw to be used
  #note: since sometimes there are multiple studies from the same paper,
  #we avoid data leakage by train/test splitting by *paper* ID, not *study* ID
  train_inds = return_train_inds(full_data, p = 0.8)
  
  #splitting training indices further into 10 CV folds for hyper parameter tuning, 9 training and 1 validation
  #balanced by replication outcome; here, just split by study ID rather than paper ID
  #(folds are only used for tuning, not performance evaluation, so leakage less of a concern)
  folds = createFolds(full_data$replicated[train_inds], k=10, list=TRUE)
  
  #xgboost errors out if any cross-validation fold has all 1s or all 0s
  #in these cases, redraw folds to avoid that edge case
  #this is more likely to be needed when running on the grid for 10000 iterations
  y_prop_folds = sapply(folds, function(x){mean(full_data$replicated[train_inds][x])})
  edge_case = any(y_prop_folds == 0) | any(y_prop_folds == 1)
  while(edge_case){
    folds <- caret::createFolds(full_data$replicated[train_inds], k=10, list=TRUE)
    y_prop_folds = sapply(folds, function(x){mean(full_data$replicated[train_inds][x])})
    edge_case = any(y_prop_folds == 0) | any(y_prop_folds == 1)
  }
    
  #controls only
  model1 = train_xgboost_model(x=controls_mat[train_inds,],
                               y=full_data$replicated[train_inds],
                               folds = folds)
  
  #text only
  model2 = train_xgboost_model(x=text_mat[train_inds,],
                               y=full_data$replicated[train_inds],
                               folds = folds)
  
  #controls and text
  model3 = train_xgboost_model(x=cbind(controls_mat,text_mat)[train_inds,],
                               y=full_data$replicated[train_inds],
                               folds = folds)
  #liwc only
  model4 = train_xgboost_model(x=liwc_mat[train_inds,],
                               y=full_data$replicated[train_inds],
                               folds = folds)
  #controls and liwc
  model5 = train_xgboost_model(x=cbind(controls_mat,liwc_mat)[train_inds,],
                               y=full_data$replicated[train_inds],
                               folds = folds)
  
  #controls and liwc and text
  model6 = train_xgboost_model(x=cbind(controls_mat,liwc_mat,text_mat)[train_inds,],
                               y=full_data$replicated[train_inds],
                               folds = folds)
  
  model_list = list(model1, model2, model3, model4, model5, model6)
  
  #test_auc stores the AUC statistic returned for each model for the entire test set 
  test_auc = calc_all_performance(cbind(controls_mat, liwc_mat, text_mat),
                                  inds = !train_inds,
                                  y = full_data$replicated,
                                  fun = "calc_auc_xgb",
                                  model_list = model_list)
  
  return(test_auc)
  
}

end_time = Sys.time()
print(paste(nrep, "splits completed in", 
            round(as.numeric(difftime(end_time, start_time, units = "mins")), 2), 
            "minutes total"))

test_aucs = do.call(rbind, out_list)
avg_aucs = colMeans(test_aucs)
sd_aucs = apply(test_aucs, 2, sd)
prop_improved = c(NA, colMeans(test_aucs[,-1] - test_aucs[,1] > 0))

performance_mat = rbind(avg_aucs, 
                        sd_aucs, 
                        prop_improved)
colnames(performance_mat) = c("Controls",
                              "Text",
                              "Controls + Text",
                              "LIWC",
                              "Controls + LIWC",
                              "Controls + LIWC + Text")
performance_table = data.table(performance_mat)
performance_table[, Stat := c("Average test AUC",
                              "SD of test AUC across splits",
                              "Proportion of splits with improvement")]

performance_table[, Data := paste0(section, ", XGBoost")]

performance_table[, `Word2Vec Settings` := embedding_params]
performance_table[, `Number of Splits` := nrep]
setcolorder(performance_table, c("Data", "Number of Splits", 
                                 "Stat", "Word2Vec Settings"))
fwrite(performance_table, "../results/performance_summary_supp.csv", append = TRUE)

