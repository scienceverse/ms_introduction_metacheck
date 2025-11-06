# ====================================================================================================
# BASIC & STATS
# ====================================================================================================

## is.nanM~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#apply is.nan to all columns
is.nanM<-function(x)do.call(cbind, lapply(x, is.nan))

## findSegments~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# detect segments based on threshold
findSegments <- function(x, threshold){
  hit <- which(x > threshold)
  n <- length(hit)
  ind <- which(hit[-1] - hit[-n] > 1)
  starts <- c(hit[1], hit[ ind+1 ])
  ends <- c(hit[ ind ], hit[n])
  cbind(starts,ends)
}

## lagfunc~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
lagfunc <-function(var,numpositions)
{
  TEMP <-c()
  for(i in 1:numpositions) TEMP<-append(TEMP,NA)
  TEMP <- c(TEMP,var)
  TEMP<-TEMP[1:(length(TEMP)-numpositions)] 
  return(TEMP)
}


## norm_for_wse~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#function for normalizing for within-subject error bars
norm_for_wse <- function(dv,ID){
  dat <- tibble(dv = dv,ID=ID)
  dat2 <- dat %>%
    mutate(grand_mean = mean(dv,na.rm=T)) %>%
    group_by(ID) %>%
    mutate(submean = mean(dv,na.rm=T)) %>%
    ungroup() %>%
    mutate(normdv = dv - submean + grand_mean)
  return(dat2$normdv)
}

## summarySE~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  require(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

## Summarizes data, handling within-subjects variables by removing inter-subject variability.
## It will still work if there are no within-S variables.
## Gives count, un-normed mean, normed mean (with same between-group mean),
##   standard deviation, standard error of the mean, and confidence interval.
## If there are within-subject variables, calculate adjusted values using method from Morey (2008).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   betweenvars: a vector containing names of columns that are between-subjects variables
##   withinvars: a vector containing names of columns that are within-subjects variables
##   idvar: the name of a column that identifies each subject (or matched subjects)
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySEwithin <- function(data=NULL, measurevar, betweenvars=NULL, withinvars=NULL,
                            idvar=NULL, na.rm=FALSE, conf.interval=.95, .drop=TRUE) {
  
  # Ensure that the betweenvars and withinvars are factors
  factorvars <- vapply(data[, c(betweenvars, withinvars), drop=FALSE],
                       FUN=is.factor, FUN.VALUE=logical(1))
  
  if (!all(factorvars)) {
    nonfactorvars <- names(factorvars)[!factorvars]
    message("Automatically converting the following non-factors to factors: ",
            paste(nonfactorvars, collapse = ", "))
    data[nonfactorvars] <- lapply(data[nonfactorvars], factor)
  }
  
  # Get the means from the un-normed data
  datac <- summarySE(data, measurevar, groupvars=c(betweenvars, withinvars),
                     na.rm=na.rm, conf.interval=conf.interval, .drop=.drop)
  
  # Drop all the unused columns (these will be calculated with normed data)
  datac$sd <- NULL
  datac$se <- NULL
  datac$ci <- NULL
  
  # Norm each subject's data
  ndata <- normDataWithin(data, idvar, measurevar, betweenvars, na.rm, .drop=.drop)
  
  # This is the name of the new column
  measurevar_n <- paste(measurevar, "_norm", sep="")
  
  # Collapse the normed data - now we can treat between and within vars the same
  ndatac <- summarySE(ndata, measurevar_n, groupvars=c(betweenvars, withinvars),
                      na.rm=na.rm, conf.interval=conf.interval, .drop=.drop)
  
  # Apply correction from Morey (2008) to the standard error and confidence interval
  #  Get the product of the number of conditions of within-S variables
  nWithinGroups    <- prod(vapply(ndatac[,withinvars, drop=FALSE], FUN=nlevels,
                                  FUN.VALUE=numeric(1)))
  correctionFactor <- sqrt( nWithinGroups / (nWithinGroups-1) )
  
  # Apply the correction factor
  ndatac$sd <- ndatac$sd * correctionFactor
  ndatac$se <- ndatac$se * correctionFactor
  ndatac$ci <- ndatac$ci * correctionFactor
  
  # Combine the un-normed means with the normed results
  merge(datac, ndatac)
}

## normWS & normDS~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# get normalized value for within-subject error (watch out for 3 levels!)
normWS<-function(d,s,dv){eval(substitute(d%>%group_by(s)%>%mutate(sAV=mean(dv,na.rm=TRUE))%>%
                                           ungroup()%>%mutate(gAV=mean(dv,na.rm=TRUE))%>%
                                           mutate(DV_n=dv-sAV+gAV),
                                         list(s=as.name(s),dv=as.name(dv))))}

normDS<-function(d,s,dv){eval(substitute(d%>%group_by(s)%>%summarise(sAV=mean(dv,na.rm=TRUE))%>%
                                           mutate(gAV=mean(sAV,na.rm=TRUE)) %>%
                                           mutate(DV_n=dv-sAV+gAV),
                                         list(s=as.name(s),dv=as.name(dv))))}

# the old version of norm function : using sAV to get gAV....
# normWS_old<-function(d,s,dv){eval(substitute(d%>%group_by(s)%>%mutate(sAV=mean(dv))%>%
#                                            mutate(gAV=mean(sAV))%>%
#                                            mutate(DV_n=dv-sAV+gAV),
#                                           list(s=as.name(s),dv=as.name(dv))))}
# 
# Example) # # Switch * Music position (new!)
# rt_pos<-subset(ds,MCONTEXT=="Music")%>%group_by(SUBID,SWITCH)%>%summarise(DV=mean(RT,na.rm=TRUE))%>%
#   normWS("SUBID","DV")%>%group_by(SWITCH)%>%summarize(DVm=mean(DV),se=sd(DV_n)/sqrt(n()),wseb=se*1.96,n=n());print(rt_pos)

# Test data (http://www.cogsci.nl/blog/tutorials/156-an-easy-way-to-create-graphs-with-within-subject-error-bars)
# ds_test<-data.frame(SUBID=rep(1:6,2),value=c(2,1,8,3,7,3,5,2,9,6,9,5),c=rep(c("B","A"),each=6))
# ds_test%>%normWS("SUBID","value")%>%group_by(c)%>%summarize(DVm=mean(DV_n),se=sd(DV_n)/sqrt(n()),wseb=se*1.96,n=n())

## effS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#calculates effect size from output of ezANOVA
effS<-function(r){r$ANOVA$MSE = r$ANOVA$SSd/r$ANOVA$DFd;r$ANOVA$eta <- r$ANOVA$SSn/(r$ANOVA$SSn+r$ANOVA$SSd);return(r)};

## deviance ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#deviance test for multilevel model comparisons 

deviance <- function(a, b) {
  diffneg2LL <- (-2*as.numeric(logLik(a))) - (-2*as.numeric(logLik(b)))
  dfneg2LL <- (attr(logLik(b), "df") - attr(logLik(a), "df"))
  p<-(1 - pchisq(diffneg2LL, dfneg2LL))
  return(print(paste("The -2LL difference is ", round(diffneg2LL, digits=3), "with ", dfneg2LL, "df, p = ", round(p, digits=3))))
}

## std_beta_MLM ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Extract case-specific coefficients + standardized coefs in tidy format for MLM models
# Currently, it only works for level2. Not pipable!! EX) r<-std_beta_MLM(m)
std_beta_MLM <-function(fit, ci.lvl = .95,type = "std",..){
  # Standardized coefficients + ci
  # code from Ben Bolker + sjstats
  # http://stackoverflow.com/a/26206119/2094622
  # https://github.com/strengejacke/sjstats/blob/master/R/std_b.R
  # arm::standardize()->https://cran.r-project.org/web/packages/arm/arm.pdf
  sdy <- stats::sd(lme4::getME(fit, "y"))
  sdx <- apply(lme4::getME(fit, "X"), 2, sd)
  # sc <- lme4::fixef(fit) * sdx / sdy
  # se.fixef <- stats::coef(summary(fit))[, "Std. Error"]
  # se <- se.fixef * sdx / sdy
  r1 <- std_beta(fit,ci.lvl=ci.lvl,type=type);
  
  # Standardized coeffs for each case 
  c <-coef(fit);cn=names(c)
  feL <- setNames(split(c[[cn]], seq(nrow(c[[cn]]))), rownames(c[[cn]]))
  sc_i<-bind_rows(lapply(feL,function(f){f * sdx/sdy}),.id = cn)#sc for all cases
  r2=gather(sc_i,term,std.estimate,-1)%>%dplyr::filter(term!="(Intercept)")
  
  # Output
  std.coef <- setClass("std.coef", slots = c(fixef="data.frame", coef="data.frame"))
  return(new("std.coef",fixef=r1,coef=r2))
}


# ====================================================================================================
# DECODING
# ====================================================================================================

# LETTERS_ex:~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# adjuted LETTER function compatible for any number of levels
LETTERS_ex <- function(n) {
                unlist(Reduce(paste0, replicate(n %/% length(LETTERS), 
                LETTERS, simplify=FALSE),
                init=LETTERS,accumulate=TRUE))[1:n] 
}

# preprop_decode--------------------------------------------------------------------------------------
# applies 1) filtering, 2) cutting, 3) balancing, and 4) factorization
# 2) - 3) are optional, step 1) and 4) are always performed. 
# ds = template ds (e.g., beh data) to be merged to neural data
# dsN = neural data (eeg)
# pset = setting for preprocessing 
preprop_decode <- function(ds,dsN,pset) {
  # Setting
  modelV = pset$modelV;
  filtIDX = pset$filtIDX;
  cutV = pset$cutV;
  balanceV = pset$balanceV;
  
  # Filtering ?
  ds$filtIDX<-filtIDX;
  dsN<-dsN[ds$filtIDX,,,];#Trial,Time,Frequency,Electrode
  ds<-ds[filtIDX==T,];
  
  #Cut # of trials for individual difference?
  if (!is.na(cutV)){
    #Get index of selected samples and combine with filtIDX
    if (dim(ds)[1] < cutV) {cutV = dim(ds)[1]}
    ind<-sample(1:dim(ds)[1],cutV)
    ds<-slice(ds,ind);dsN<-dsN[ind,,,];
    print(paste0("Data set is cut off to be ", cutV));
  }
  
  #Balance # of trials by not-to-be-decoded variable?
  if (!is.na(balanceV)){
    #Prepare index to merge back to the original data after sub-sampling with lowest # of data
    set.seed(as.numeric(gsub("A","",s)));#Get same sets of trials for each subject
    c=select_(ds,balanceV)%>%group_by_(balanceV)%>%summarize(n=n())
    ind<-ds[,.SD[sample(.N, min(min(c$n),.N))],by = balanceV]
    ind$filtIDX<-TRUE;#update filtIDX (keep all from balanceed data)
    
    #Get index of selected samples and combine with filtIDX
    ds<-merge(ds[,-c("filtIDX")],ind[,c("SUBID","BLOCK","TRIAL","filtIDX")],by=c("SUBID",'BLOCK','TRIAL'),all.x=T)
    dsN<-dsN[!is.na(ds$filtIDX),,,];#Trial,Time,Frequency,Electrode
    ds<-ds[!is.na(ds$filtIDX),];#CAUTION!! ALWAYS FILTER dsN first!!!
    print(paste0("Data set is balanced by ", balanceV," variable...!!"));
  }
  
  #Factorize to-be-modeled variable (and use arbitrary characters for each level)
  ALPHABETS<-LETTERS_ex(length(c(unique(ds_b[[modelV]]))));#comes from other function
  if (min(ds[[modelV]])==0){ds[[modelV]]<-ds[[modelV]]+1}
  ds[[modelV]]<-ALPHABETS[as.matrix(ds[[modelV]])];
  ds[[modelV]]<-ds[[modelV]]%>%unlist%>%factor;
  ds[[modelV]]<-droplevels(ds[[modelV]])
  
  return(list(ds,dsN))
}



# Spatial Projection----------------------------------------------------------------------------------
# m = model object from "train" function, varL = feature labels
# dd = observed data (trial, time, features)
# AMAP is averaging coefficients across all classes (change this?)
# AMAP is only valid when linear algorithm is used
weight2topo <- function(m,dd,varL) {
  
  #Calculation of variable importance (standard method)
  i<-varImp(m, scale=T);#this is slow!
  #rownames(i$importance)<-varL;ggplot(i,top=14)
  imp<-i$importance;
  imp$ALL<-rowMeans(imp);
  imp$Vars<-varL;#rownames(i$importance)<-varL;
  imp$Freq<-gsub("_.*","",imp$Vars);
  imp$Elec<-gsub(".*_","",imp$Vars)
  
  #Weight projection
  # X = observed data (XXt is covariance)
  # W = coefficients, filter 
  # Y = component (multiply of X and W)
  # xx = (feature,trial* segmented time samples)
  coefs=coef(m$finalModel)
  w = as.vector(coefs[-1,2])
  wm = as.vector(apply(coefs[-1,],1,mean))
  x <- apply(dd,c(2,3),mean)# average across trials
  x <- t(apply(x,1,scale))# 0 mean over time
  
  #Method 1 (BieSman,2012;Haufe et al, 2014): A = XXtW
  #X = stats::cov(x)
  #imp$AMAP <- t(x) %*% (x) %*% wm
  
  #Method 2 (Parra,2005): A = XYt (YYt)^-1
  y = t(wm) %*% t(x);# w is individual wm is average coefs for all classes
  imp$AMAP <- t(x) %*% t(y) %*% (y %*% t(y)) ^-1

  return(imp)
}


# ====================================================================================================
# PLOTTING
# ====================================================================================================
## theme0 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ggplot theme striping off everything!
theme0 <- function(...) theme( legend.position = "none",
                               panel.background = element_blank(),
                               panel.grid.major = element_blank(),
                               panel.grid.minor = element_blank(),
                               panel.spacing  = unit(0,"null"),
                               axis.ticks = element_blank(),
                               axis.text.x = element_blank(),
                               axis.text.y = element_blank(),
                               axis.title.x = element_blank(),
                               axis.title.y = element_blank(),
                               axis.ticks.length = unit(0,"null"),
                               panel.border=element_rect(color=NA),...)

## shift_axis ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# shift axis for traditional ERP plot
shift_axis <- function(p, y=0){
  g <- ggplotGrob(p)
  dummy <- data.frame(y=y)
  ax <- g[["grobs"]][g$layout$name== "axis-b"][[1]]
  p + annotation_custom(grid::grobTree(ax, vp = grid::viewport(y=1, height=sum(ax$height))), 
                        ymax=y, ymin=y) +
    geom_hline(aes(yintercept=y), data = dummy) +
    theme(axis.text.x = element_blank(),  axis.ticks.x=element_blank())
}

## plot_topo ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ds = data set containing values to plot topographic pattern
# vn = variable name
prep_topo <- function (ds,vn){
  #Load chan locations
  path<-path.expand("~/Dropbox/w_ONGOINGRFILES/w_OTHERS/chanlocs_20E_Oregon.txt")
  chanlocs<-fread(paste0(path),header = TRUE);
  chanlocs$X<-rescale(chanlocs$X,c(0,1));chanlocs$Y<-rescale(chanlocs$Y,c(0,1));
  ds$Elec<-factor(ds$Elec);ds<-inner_join(ds,chanlocs,by="Elec")
  
  #GAM
  ds$DV <-ds[[vn]];
  splm <- gam(DV ~ s(X,Y,bs="sos",k = length(ds$Elec)-2), data=ds)
  ds_spl<-data.frame(expand.grid(X=seq(-0.3, 1.3, 0.01),Y=seq(-0.3, 1.3, 0.001)))
  ds_spl$value <- predict(splm,ds_spl, type = "response")

  #HEAD
  c_center=c(0.5,0.5);c_rad<-1.25;nose_y<-(c_rad/2)+c_center[1];
  circledat <- circleFun(c_center,c_rad, npoints = 100) # center on [.5, .5]
  ds_spl$incircle <- (ds_spl$X - c_center[1])^2 + (ds_spl$Y - c_center[2])^2 < (c_rad/2)^2 # mark
  ds_spl <- ds_spl[ds_spl$incircle,]

  return(list(ds_spl,chanlocs,circledat,nose_y,c_center,c_rad))
}

## circleFun ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#calculate xy cordinates of a specified circle (for topograpgic plot)
circleFun <- function(center = c(0,0),diameter = 1, npoints = 100){
  r = diameter / 2
  tt <- seq(0,2*pi,length.out = npoints)
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}


# ====================================================================================================
# OTHERS
# ====================================================================================================

## tic & toc ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Matlab tic & toc equivalant functions
tic <- function(gcFirst = TRUE, type=c("elapsed", "user.self", "sys.self"))
{
   type <- match.arg(type)
   assign(".type", type, envir=baseenv())
   if(gcFirst) gc(FALSE)
   tic <- proc.time()[type]         
   assign(".tic", tic, envir=baseenv())
   invisible(tic)
}

toc <- function()
{
   type <- get(".type", envir=baseenv())
   toc <- proc.time()[type]
   tic <- get(".tic", envir=baseenv())
   print(toc - tic)
   invisible(toc)
}


## .ls.objects ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# improved list of objects
.ls.objects <- function (pos = 1, pattern, order.by,
                         decreasing=FALSE, head=FALSE, n=5) {
  napply <- function(names, fn) sapply(names, function(x)
    fn(get(x, pos = pos)))
  names <- ls(pos = pos, pattern = pattern)
  obj.class <- napply(names, function(x) as.character(class(x))[1])
  obj.mode <- napply(names, mode)
  obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class)
  obj.size <- napply(names, object.size)
  obj.dim <- t(napply(names, function(x)
    as.numeric(dim(x))[1:2]))
  vec <- is.na(obj.dim)[, 1] & (obj.type != "function")
  obj.dim[vec, 1] <- napply(names, length)[vec]
  out <- data.frame(obj.type, obj.size, obj.dim)
  names(out) <- c("Type", "Size", "Rows", "Columns")
  if (!missing(order.by))
    out <- out[order(out[[order.by]], decreasing=decreasing), ]
  if (head)
    out <- head(out, n)
  out
}
# shorthand
lsos <- function(..., n=10) {
  .ls.objects(..., order.by="Size", decreasing=TRUE, head=TRUE, n=n)
}

