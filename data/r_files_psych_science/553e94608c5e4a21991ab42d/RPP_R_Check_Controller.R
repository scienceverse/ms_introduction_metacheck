# Required packages:
library("markdown")
library("rmarkdown")
library("knitr")
library("rmarkdown")
library("magrittr")
library("devtools")
library("parallel")
library("googlesheets")
source("http://sachaepskamp.com/files/OSF/getOSFfile.R") # the getOSFfile function

# Set locale:
Sys.setlocale('LC_ALL','C') 

# Load MASTER data:
File <- getOSFfile("https://osf.io/fgjvw/")
MASTER <- read.csv(File)[1:167, ]

# Fix names:
names(MASTER) <- 
  names(MASTER) %>%
  gsub("\\."," ",.) %>%
  gsub(" (O|R) ","(\\1)",.) %>%
  gsub("X(\\d)","\\1",.) %>%
  gsub(" s ","'s ",.) %>%
  gsub("^X\\s+","# ",.) %>%
  gsub("P value","P-value",.) %>%
  gsub("(\\d{2})  power","\\1% power",.)

# Store working directory:
WORKINGDIR <- getwd()

# Read markdown template:
MDorig <- readLines("http://sachaepskamp.com/files/OSF/MarkdownTemplate.Rmd", encoding = "UTF8")

# Data frame to check progress:
AuditResults <- MASTER[,c("Project URL","R check location (R)")]
AuditResults$Auditor <- NA
AuditResults$AuditorMail <- NA
AuditResults$AuditorCorrect <- FALSE
AuditResults$error <- NA
AuditResults$install <- FALSE
AuditResults$noscript <- FALSE
AuditResults$codeCorrect <- TRUE

AuditResults$TestStatistic <- NA
AuditResults$Pvalue <- NA
AuditResults$SampleSize <- NA
AuditResults$EffectSize <- NA
AuditResults$PassedAudit <- NA

# Create dirs:
dir.create("Rscripts")
file.remove(list.files("markdown",full.names = TRUE)) # Clean up if needed
dir.create("markdown")
dir.create("html")
dir.create("Overwrite")
overwriteFiles <- list.files("Overwrite",full.names = TRUE)
try(file.remove("Audits.zip"))

# Vector to store HTML files:
HTMLs <- character(0)

#### MAIN LOOP ###
for (i in seq_len(nrow(MASTER))){
  
  # Check if audit file exists:
  if (is.na(MASTER[["R check location (R)"]][i]) || MASTER[["R check location (R)"]][i]==''){
    AuditResults$noscript[i] <- TRUE
    next
  }
  
  # Extract info:
  URL <- MASTER[['Project URL']][i]
  Code <- gsub("(https://osf.io/)|(/)","",URL)
  R <- MASTER[["R check location (R)"]][i]
  
  # Markdown from template:
  MD <- MDorig
  
  # Loop over names and replace codes:
  for (j in seq_len(ncol(MASTER))){
    MD <- gsub(paste0("@@",names(MASTER)[j],"@@"),gsub("\"","'",MASTER[[j]][i]), MD, fixed = TRUE )
  }
  MD <- MD[!is.na(MD)] 
  
  ### MIX IN R CODES ###
  # Read R script:
  Rfile <- getOSFfile(R, "Rscripts")
  
  # Check if name of file is correct:
  if (!grepl(Code,gsub("(RPP_)|(\\.R)","",basename(Rfile),ignore.case=TRUE))){
    AuditResults$codeCorrect[i] <- FALSE
  }
  
  # Check if overwrite file exists:
  if (any(grepl(Code, overwriteFiles))){
    file.copy(grep(Code, overwriteFiles,value=TRUE), Rfile, overwrite = TRUE)
  }
  
  # Check if file can be read:
  isR <- try({
    R <- readLines(Rfile)
  })
  if (is(isR,"try-error") | length(R) == 0){
    AuditResults$noscript[i] <- TRUE
    next
  }
  
  # Extract codes and check if R file is correct:
  corR <- try({
    
    # Read the R files:
    R <- readLines(Rfile)
    
    # Double all backslashes:
    R <- gsub("\\\\","\\\\\\\\",R)
    
    # Remove stupid typo:
    R <- gsub("DATA ANLAYSIS","DATA ANALYSIS", R)
    
    # Split in sections:
    secs <- split(R, cumsum(seq_along(R) %in% grep("##@@.*@@##", R)))
    names(secs) <- sapply(secs,function(x)gsub("(^\\s*)|(##@@ )|( @@##)","",x[1]))
    
    # General information:
    # Coder:
    MD <- gsub("@@@RCODER@@@", gsub("#@ Coder name:\\s*","",grep("#@ Coder name:",secs[['GENERAL INFORMATION']], value = TRUE)) , MD)
    # Coder e-mail:
    MD <- gsub("@@@RCODEREMAIL@@@", gsub("#@ Coder e-mail:\\s*","",grep("#@ Coder e-mail:",secs[['GENERAL INFORMATION']], value = TRUE)) , MD)
    MD <- gsub("@@@TESTTYPE@@@", gsub("#@ Type of statistic:\\s*","",grep("#@ Type of statistic:",secs[['GENERAL INFORMATION']], value=TRUE)), MD)
    MD <- gsub("@@@REPREPORT@@@", gsub("#@ OSF link replication report:\\s*","",grep("#@ OSF link replication report:",secs[['GENERAL INFORMATION']], value=TRUE)), MD)
    MD <- gsub("@@@EFFECTTYPE@@@", gsub("#@ Type of effect-size:\\s*","",grep("#@ Type of effect-size:",secs[['GENERAL INFORMATION']], value=TRUE)), MD)
    
    # Function to convert R codes:
    prepareRcodes <- function(x){
      x <- x[!grepl("##?@@?", x)]
      x <- paste(x, collapse="\n")
      return(x)
    }
    
    # Mix in R codes:
    for (j in seq_along(secs)){
      MD <- gsub(sprintf("@@@%s@@@",names(secs)[j]), prepareRcodes(secs[[j]]), MD)  
    }
    
    # Remove header:
    MD <- MD[-10]
    
    # Write markdown file:
    writeLines(MD, paste0("markdown/",gsub("\\.R",".Rmd",basename(Rfile),ignore.case = TRUE)))
  })

  # Check if correct:
  if (is(corR, "try-error")){
    AuditResults$AuditorCorrect[i] <- FALSE
    AuditResults$error[i] <- as.character(corR)
    next
  }
  
  # Setup empty cluster to run markdown in:
  cl <- makeCluster(getOption("cl.cores", 1))
  clusterExport(cl, c("secs", "prepareRcodes"))

  # Run markdown:
  tryres <- parLapply(cl,Rfile,function(R){

    library("rmarkdown")
    try({
      file <- paste0("markdown/",gsub("\\.R",".Rmd",basename(R),ignore.case = TRUE))
      html <- rmarkdown::render(iconv(file), html_document(), output_dir = "../html",quiet=TRUE,
                                encoding = "Latin1")        
      
      list(
        TestStatistic = eval(parse(text=prepareRcodes(secs[["STATISTIC"]]))),
        Pvalue = eval(parse(text=prepareRcodes(secs[["P-VALUE"]]))),
        SampleSize = eval(parse(text=prepareRcodes(secs[["SAMPLE SIZE"]]))),
        EffectSize = eval(parse(text=prepareRcodes(secs[["EFFECT SIZE"]]))),
        PassedAudit = eval(parse(text=prepareRcodes(secs[["AGREEMENT WITH AUTHORS"]]))),
        html = html)
    })
  })[[1]]

  stopCluster(cl)
  
  # If markdown was generated without error, add HTML to the list:
  if (!is(tryres,"try-error")){
    html <- tryres$html
    HTMLs <- c(HTMLs,html)
    if (!is.null(tryres$TestStatistic)) AuditResults$TestStatistic[i] <- tryres$TestStatistic
    if (!is.null(tryres$Pvalue)) AuditResults$Pvalue[i] <- tryres$Pvalue
    if (!is.null(tryres$SampleSize)) AuditResults$SampleSize[i] <- tryres$SampleSize
    if (!is.null(tryres$EffectSize)) AuditResults$EffectSize[i] <- tryres$EffectSize
    if (!is.null( tryres$PassedAudit  )) AuditResults$PassedAudit[i] <- tryres$PassedAudit  
  }
  
  # Write progress results:
  AuditResults$Auditor[i] <-  gsub("#@ Coder name:\\s*","",grep("#@ Coder name:",secs[['GENERAL INFORMATION']], value = TRUE))
  AuditResults$AuditorMail[i] <- gsub("#@ Coder e-mail:\\s*","",grep("#@ Coder e-mail:",secs[['GENERAL INFORMATION']], value = TRUE))
  AuditResults$AuditorCorrect[i] <- !is(tryres,"try-error")
  
  
  if (!AuditResults$AuditorCorrect[i])  AuditResults$error[i] <- as.character(tryres)
  AuditResults$install[i] <- any(grepl("install",R))  
    View(AuditResults)
}

# Store results to disk:
save(AuditResults,file="AuditResults.RData")
zip("Audits.zip", HTMLs, flags = "-j")
write.csv(as.data.frame(AuditResults), file = "AuditResults.csv")