#Note: this was run with system locale/encoding set to en_US.UTF-8
library(readr)

setwd("../text_data/main_papers")
files_toread = list.files()
write_path = "../main_papers_remove_spurious_words/"

#make the subfolder to store edited text
if(!file.exists(write_path)){
  dir.create(write_path)
}

#These are author names/proper nouns that show up in the top 50
#most frequent keywords for at least 1 LIWC dictionary
#This is due to wildcard matches (e.g., "Nosek" is classified as a "body" word
#due to starting with the word "nose").
#We remove these names from the text files when calculating LIWC scores
#likewise, remove common notation (e.g., IV for independent variable)
#that get erroneously classified in LIWC dictionaries (e.g., IV as a health/bio word)
words_to_remove = c(
  #author names
  "Nosek",
  "Brainard",
  "Brainerd",
  "Gal",
  "Grillon",
  "Guyer",
  "Tversky",
  "Payne",
  "Harmon-Jones",
  "Saint-Paul",
  "Shiffrin",
  "Shiner",
  "Virginia",
  "Humphreys",
  "King",
  #notation/variable names (case insensitive)
  "[Rr][Tt]",
  "[Kk]",
  "[Ii][Vv]",
  "[Mm][RrSsAa]",
  "[Uu]",
  "[Dd]ummy" #gets classified as a swear word/anger word
)

#\< and \> match empty strings at the beginning/end of a word
#so for instance this way the letter "U" doesn't get removed
#unless it's a standalone word
words_to_remove = paste0("\\<", words_to_remove, "\\>")

#loop through files
for(i in 1:length(files_toread)){
  tmp = read_file(files_toread[i])
  #remove names
  for(j in words_to_remove){
    tmp = gsub(j, "", tmp)
  }
  #write to new folder
  write_file(tmp, paste0(write_path, files_toread[i]))
}

