#NOTE: by default, when you open the file R should make "code" your working directory
#if not, setwd() first to make sure you're in the right place.

#This file calculates Flesch reading ease scores for all the data

#read in various helper functions
#warning that this will install a bunch of packages (if you don't have them already)
source("utils.R")
#for calculating readability scores
library(koRpus)
#may need to be installed by install.koRpus.lang("en")
library(koRpus.lang.en)

metadata_filename = "../processed_data/metadata_2024.csv"

full_data = get_meta(metadata_filename)

#calculate readability scores
#pre-allocate vector for storing scores
readability_mat = matrix(NA, nrow = nrow(full_data), ncol = 3)

#loop over papers to calculate readability (might take a few minutes)
for(i in 1:nrow(full_data)){
  #identify text file path using study ID for section == "study", paper ID o/w
  #use koRpus to tokenize into sentences
  #abstract
    #need to check the paper actually has an abstract first
  if(paste0("abstract", full_data$paper_id[i], ".txt") %in% list.files("../text_data/main_papers")){
    tkns = tokenize(paste0("../text_data/main_papers/abstract", 
                           full_data$paper_id[i], ".txt"),
                    lang = "en",
                    fileEncoding = 'latin1')
    #obtain Flesch reading ease score
    rd = readability(tkns, index = "Flesch", quiet = TRUE)
    #store in vector
    readability_mat[i,1] = rd@Flesch$RE
  }
  
  #fulltext
  tkns = tokenize(paste0("../text_data/main_papers/fulltext", 
                         full_data$paper_id[i], ".txt"),
                  lang = "en",
                  fileEncoding = 'latin1')
  #obtain Flesch reading ease score
  rd = readability(tkns, index = "Flesch", quiet = TRUE)
  #store in vector
  readability_mat[i,2] = rd@Flesch$RE
  #study
  tkns = tokenize(paste0("../text_data/main_papers/study", 
                         full_data$study_id[i], ".txt"),
                  lang = "en",
                  fileEncoding = 'latin1')
  #obtain Flesch reading ease score
  rd = readability(tkns, index = "Flesch", quiet = TRUE)
  #store in vector
  readability_mat[i,3] = rd@Flesch$RE
  
  #progress tracker
  if((i %% 10) == 0){print(i)}
}

colnames(readability_mat) = c("abstract_readability", "fulltext_readability", "study_readability")
readability_tbl = data.table(study_id = full_data$study_id, readability_mat)
fwrite(readability_tbl, "../processed_data/readability_scores.csv")
