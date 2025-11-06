require(memisc)
setwd("[list_dir]")
dfs <- read.csv("/meta_misinformaiton_osf.v20171231.csv", header = TRUE)
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
  #V1 formaula: Overall N with the inverse N term = (1/N) + G^2/(2*(M+2), where M = N -1
  description(MisinformationEffectSizesV1) <- "Variances of Effect Sizes of Misinformation - F1"
  description(DebunkingEffectSizesV1) <- "Variances of Effect Sizes of Debunking - F1"
  description(MisinformationPersistenceEffectSizesV1) <- "Variances of Effect Sizes of Misinformation-Persistence - F1"
  #V2 formula: Only Ns in the comparison with the inverse nt term = (1/nt) + G^2/(2*(M+2)), where nt =(N/2)*(N/2)/(N+N) and M = N-2
  description(MisinformationEffectSizesV2) <- "Variances of Effect Sizes of Misinformation - F2"
  description(DebunkingEffectSizesV2) <- "Variances of Effect Sizes of Debunking - F2"
  description(MisinformationPersistenceEffectSizesV2) <- "Variances of Effect Sizes of Misinformation-Persistence - F2"
  #V3 formula: Overall N with the inverse nt term = (1/nt) + G^2/(2*(M+2)), where nt =(N/2)*(N/2)/(N+N) and M = N-2
  description(MisinformationEffectSizesV3) <- "Variances of Effect Sizes of Misinformation - F3"
  description(DebunkingEffectSizesV3) <- "Variances of Effect Sizes of Debunking - F3"
  description(MisinformationPersistenceEffectSizesV3) <- "Variances of Effect Sizes of Misinformation-Persistence - F3"
  
  foreach(x=c(ExpNum,StudyDesign,OnlineLabDataCollection,PublicationStatus,DebunkingDetails),{
    measurement(x) <- "nominal"
  })
  
  foreach (x=c(PublicationYear,MeanAge,PctFemale,
               MisinformationEffectSizes,DebunkingEffectSizes,MisinformationPersistenceEffectSizes,
               MisinformationEffectSizesV1,DebunkingEffectSizesV1,MisinformationPersistenceEffectSizesV1,
               MisinformationEffectSizesV2,DebunkingEffectSizesV2,MisinformationPersistenceEffectSizesV2,
               MisinformationEffectSizesV3,DebunkingEffectSizesV3,MisinformationPersistenceEffectSizesV3),{
    measurement(x) <- "ratio"
  })

  labels(ExpNum) <- c("experiment 1"=1,"experiment 1.5"=1.5,"experiment 2"=2,"experiment 2.5"=2.5,"experiment 3"=3,"experiment 3.5"=3.5)
  labels(StudyDesign) <- c("between"=1,"within"=2)
  labels(OnlineLabDataCollection) <- c("online"=1,"lab"=2)
  labels(PublicationStatus) <- c("working paper"=1,"thesis"=2, "journal article"=3)
  # labels(DebunkingDetails) <- c("detailed"=1,"not detailed"=2)
})

description(dfs)
codebook(dfs)
