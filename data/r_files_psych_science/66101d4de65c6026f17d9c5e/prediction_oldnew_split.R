#a minimal Unix loop that can be run in command line to run this script with all parameter settings
#for i in {1..21}; do SGE_TASK_ID=$i Rscript ./prediction_oldnew_split.R; done

#NOTE: by default, when you open the file R should make "code" your working directory
#if not, setwd() first to make sure you're in the right place.

#This file runs the supplementary prediction analysis where we split papers into
#those published 2011 or earlier (train), or published 2012 or later (test)
#then calculate the AUC of the models on the test set
#predictions are calculated for 6 models:
  #metadata (control) variables only
  #Word2vec embedding summaries only
  #metadata + Word2vec embedding summaries
  #LIWC dictionaries only
  #metadata + LIWC dictionaries
  #metadata + Word2vec embeddings + LIWC dictionaries


########### Script settings
#this section determines which version of the analysis to run:
  #section: which text of the paper to analyze (abstract, main text, or just the study being replicated)
  #embedding_params: which Word2vec model to use
#code below reproduces our main results (study text, not excluding econ, preferred Word2vec settings)
#can change the section, exclude_econ, and embedding_params to run alternate specs/robustness checks

#optionally can run all different versions of the analysis in a command line task array
#create a task array from and define the task ID below to determine data settings to use
#tasks from 1-3 if only using main Word2vec model; 1-21 if also computing for alternate Word2vec specs
#line below shows example if using Sun Grid Engine task arrays
# task_id = as.integer(Sys.getenv("SGE_TASK_ID"))

#which section of the papers to run the analyses on
  #abstract: the abstract
  #fulltext: the main text of the paper except the abstract
  #study: the text describing the study being replicated
section = "study"
#if running as a task array:
  #abstract: task_id = 1, 4, 7, 10, ...
  #fulltext: task_id = 2, 5, 8, 11, ...
  #study: task_id = 3, 6, 9, 12, ...
# section = c("abstract", "fulltext", "study")[((task_id-1) %% 3) + 1]

#which Word2vec model to use for the analysis
#main specification:
  #sg: default continuous bag of words (0), alternately skipgram (1)
  #window: default context window of +/-3 words, alternately 5
  #dim: default 100 dimensions of embedding space, alternately 50 or 150
  #epochs: default training by 50 passes through data, alternately 100
  #stopwords: default including stopwords (1), alternately exclude them (0)
embedding_params = "sg0_window3_dim100_epochs50_stopwords1"
#if running as a task array:
  #default/main specification: task_id = 1-3
  #skipgram: task_id = 4-6
  #window size 5: task_id = 7-9
  #50 dimensions: task_id = 10-12
  #150 dimensions: task_id = 13-15
  #100 epochs: task_id = 16-18
#excluding stopwords: task_id = 19-21
# embedding_params = c("sg0_window3_dim100_epochs50_stopwords1",
#                      "sg1_window3_dim100_epochs50_stopwords1",
#                      "sg0_window5_dim100_epochs50_stopwords1",
#                      "sg0_window3_dim50_epochs50_stopwords1",
#                      "sg0_window3_dim150_epochs50_stopwords1",
#                      "sg0_window3_dim100_epochs100_stopwords1",
#                      "sg0_window3_dim100_epochs50_stopwords0")[((task_id - 1) %/% 3) + 1]

print(paste("Calculating old/new paper split for", section, 
            "and word2vec settings", embedding_params))

########### Loading in packages, helper functions, and data
#read in various helper functions
#warning that this will install a bunch of packages (if you don't have them already)
source("utils.R")

#the three datasets I read are - metadata, embeddings file, and the liwc dictionaries files
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

#append keywords onto metadata matrix
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

#For this analysis, there is no nested cross-validation since there is only 1 split (old vs. new papers)
start_time = Sys.time()

set.seed(25)

#train-test draw to be used
train_inds = full_data[, pub_year <= 2011]

#splitting training indices further into 10 CV folds for hyper parameter tuning, 9 training and 1 validation
#balanced by replication outcome; here, just split by study ID rather than paper ID
#(folds are only used for tuning, not performance evaluation, so leakage less of a concern)
folds = createFolds(full_data$replicated[train_inds], k = 10, list = FALSE)

#controls only
model1 = train_ridge_model(x=controls_mat[train_inds,],
                           y=full_data$replicated[train_inds],folds=folds)
#text only
model2 = train_ridge_model(x=text_mat[train_inds,],
                           y=full_data$replicated[train_inds],folds=folds)
#controls and text
model3 = train_ridge_model(x=cbind(controls_mat,text_mat)[train_inds,],
                           y=full_data$replicated[train_inds],folds=folds)
#liwc only
model4 = train_ridge_model(x=liwc_mat[train_inds,],
                           y=full_data$replicated[train_inds],folds=folds)
#controls and liwc
model5 = train_ridge_model(x=cbind(controls_mat,liwc_mat)[train_inds,],
                           y=full_data$replicated[train_inds],folds=folds)

#controls and liwc and text
model6 = train_ridge_model(x=cbind(controls_mat,liwc_mat,text_mat)[train_inds,],
                           y=full_data$replicated[train_inds],folds=folds)

model_list = list(model1, model2, model3, model4, model5, model6)

#test_auc stores the AUC statistic returned for each model for the entire test set 
test_auc = calc_all_performance(cbind(controls_mat, liwc_mat, text_mat),
                                inds = !train_inds,
                                y = full_data$replicated,
                                fun = "calc_auc",
                                model_list = model_list)

#save down results
colnames(test_auc) = c("Controls", 
                              "Text", 
                              "Controls + Text", 
                              "LIWC", 
                              "Controls + LIWC",
                              "Controls + LIWC + Text")
performance_table = data.table(test_auc)
performance_table[, Stat := "Test AUC"]

performance_table[, Data := paste0(section, ", predicting new from old")]

performance_table[, `Word2Vec Settings` := embedding_params]
performance_table[, `Number of Splits` := NA]
setcolorder(performance_table, c("Data", "Number of Splits", "Stat", "Word2Vec Settings"))
fwrite(performance_table, "../results/performance_summary_main.csv", append = TRUE)
end_time = Sys.time()
difftime(end_time, start_time)
