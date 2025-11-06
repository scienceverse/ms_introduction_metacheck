#NOTE: by default, when you open the file R should make "code" your working directory
#if not, setwd() first to make sure you're in the right place.

########### Script settings
#this section determines which version of the analysis to run:

#selection of papers to run this analysis on
#for this analysis, we choose papers in which we have multiple studies - some that replicated and some that did not. 
#The purpose of this selection is to rule out across paper/author differences in how writing differs across replicated vs. 
#not replicated studies

#which section of the papers to run the analyses on
#for this supplemental analysis, just used study text
section = "study"

#which Word2vec model to use for the analysis
#just used default specification for this
embedding_params = "sg0_window3_dim100_epochs50_stopwords1"

########### Loading in packages, helper functions, and data
#read in various helper functions
#warning that this will install a bunch of packages (if you don't have them already)
source("utils.R")

#the two datasets I read are - metadata and the liwc dictionaries files
#we specify the filename of the embedding file we want to use for this analyses and the section of the text we would be using for liwc
#to use a different section of the academic papers (study/fulltext/abstract), change the embedding filename and the section argument of get_liwc function

metadata_filename = "../processed_data/metadata_2024.csv"
liwc_filename = "../processed_data/liwc2015_scores.csv"
readability_filename = "../processed_data/readability_scores.csv"
narrative_filename = "../processed_data/narrative_arc.xlsx"

#non-textual data about each paper
full_data = get_meta(metadata_filename)

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

#just to make sure the ordering of obs is the same regardless of section
setkey(full_data, study_id)

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

#arc of narrative
#no assignment needed as this does modification-in-place of "full_data"
get_narrative(narrative_filename, section)

#separating out vignette/sibling studies
vignette_studies = full_data[,.(N_study = .N,
                                N_replicated = sum(replicated)),
                             by = "paper_id"]
#papers with 2+ studies, with at least 1 successful and 1 failed replication
vignette_studies = vignette_studies[(N_replicated >= 1) & ((N_study - N_replicated) >= 1) ,paper_id]
vignette_data = full_data[paper_id %in% vignette_studies]

liwc_cols = colnames(liwc)[c(-1,-2)]

#creating the necessary datasets for this analysis
#we calculate difference-in-means between replicated/non-replicated studies in the vignette subset
#but due to the small sample size, for performing t-tests, we calculate the standard deviation
#using the entire dataset
subset_1 = vignette_data[replicated==1]
subset_0 = vignette_data[replicated==0]

full_1 = full_data[replicated==1]
full_0 = full_data[replicated==0]

########### differences in means for replicated vs non replicated paired studies

#variables to calc differences for. LIWC, supplemented by narrative and other summary measures
liwc_cols = c(liwc_cols, 'readability', 'obfuscation', 'abstraction', 'staging',
              "plot_progression", "cognitive_tension")

#for a paired difference test, aggregate studies from same paper with same replication outcome
agg_1 = subset_1[, lapply(.SD, mean), by = "paper_id", .SDcols = liwc_cols]
agg_0 = subset_0[, lapply(.SD, mean), by = "paper_id", .SDcols = liwc_cols]

#pre-allocate matrix for storing results
result_mat = matrix(nrow = length(liwc_cols), ncol = 2)
i = 1
for(col in liwc_cols){
  mean_1 = mean(agg_1[[col]])
  mean_0 = mean(agg_0[[col]])
  
  #calculating pooled sd - both samples assumed to come from populations with the same variance
  sd_1 = sd(full_1[[col]])
  sd_0 = sd(full_0[[col]])
  N_1 = nrow(full_1)
  N_0 = nrow(full_0)
  pooled.sd = ((N_1-1)*sd_1 + (N_0-1)*sd_0)/(N_1+N_0-2)
  
  #cohen's d to get a standardized measure of effect size
  diffmean = mean_1 - mean_0
  d = diffmean/pooled.sd
  
  result_mat[i,] = c(diffmean, d)
  i=i+1
}

colnames(result_mat) = c("Difference in means", "Cohen's d")
result_tbl = data.table(variable = liwc_cols, result_mat)

fwrite(result_tbl, paste0("../results/", section, "_vignette_comparisons.csv"))
