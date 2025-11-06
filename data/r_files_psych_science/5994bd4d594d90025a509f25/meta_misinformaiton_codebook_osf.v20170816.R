require(memisc)
setwd("[list_dir]")
dfs <- read.csv("meta_misinformaiton_osf.v20170517.csv", header = TRUE)
dfs <- as.data.set(dfs)

dfs <- within(dfs,{
  description(RecordID) <- "Record ID"
  description(StudyID) <- "Report ID"
  description(Authors) <- "Authors of the Report"
  description(ExpNum) <- "Number of Experiment"
  description(RowNoteCond) <- "Descriptions of Records/Conditions"
  description(StudyDesign) <- "Between- or Within-Subjects Design"
  description(PublicationYear) <- "Year of Publication"
  description(SampleSize) <- "Size of Sample"
  description(MeanAge) <- "Mean of Age"
  description(PctFemale) <- "Percentage of Females"
  description(MisinformationEffectSizes) <- "Effect Sizes of Misinformation"
  description(DebunkingEffectSizes) <- "Effect Sizes of Debunking"
  description(MisinformationPersistenceEffectSizes) <- "Effect Sizes of Misinformation-Persistence"
  description(OnlineLabDataCollection) <- "Data Collection at Lab or Online"
  description(PublicationStatus) <- "Publication Status"
  description(ZGenerationExplanations) <- "Standardised Scores of the Generation of Explanations"
  description(ZGenerationCounterarguments) <- "Standardised Scores of the Generation of Counterarguments"
  description(DebunkingDetails) <- "Detailed or Not-Detailed Debunking"
  
  foreach(x=c(ExpNum,StudyDesign,OnlineLabDataCollection,PublicationStatus,DebunkingDetails),{
    measurement(x) <- "nominal"
  })
  
  foreach (x=c(PublicationYear,MeanAge,PctFemale,MisinformationEffectSizes,DebunkingEffectSizes,MisinformationPersistenceEffectSizes),{
    measurement(x) <- "ratio"
  })

  labels(ExpNum) <- c("experiment 1"=1,"experiment 1.5"=1.5,"experiment 2"=2,"experiment 2.5"=2.5,"experiment 3"=3,"experiment 3.5"=3.5)
  labels(StudyDesign) <- c("between"=1,"within"=2)
  labels(OnlineLabDataCollection) <- c("online"=1,"lab"=2)
  labels(PublicationStatus) <- c("working paper"=1,"thesis"=2, "journal article"=3)
  labels(DebunkingDetails) <- c("detailed"=1,"not detailed"=2)

  # missing.values(ExpNum,StudyDesign,OnlineLabDataCollection,PublicationStatus,DebunkingDetails,PublicationYear,MeanAge,PctFemale,MisinformationEffectSizes,DebunkingEffectSizes,MisinformationPersistenceEffectSizes) <- c("")
})

description(dfs)
codebook(dfs)
