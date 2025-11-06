# Miscellaneous R functions for analysis output
#
# DongWon Oh (Columbia University)
# Last update: circa 2021

print_t_result <- function(model,digit=2,p.digit=3,
                           p.lowercase=T,include.zero.p=F,ci=T,
                           include.mean=T,mean1.name="1",mean2.name="2",mean.unit="",
                           verbose=T){

  require(stringr)
  
  df <- sprintf(paste0("%0.",digit,"f"), round(model$parameter,digit))
  t.value <-  sprintf(paste0("%0.",digit,"f"), round(model$statistic,digit))
  
  if (model$method=="Welch Two Sample t-test"|model$method==" Two Sample t-test"){
    mean1 <- sprintf(paste0("%0.",digit,"f"), round(model$estimate[1],digit))
    mean2 <- sprintf(paste0("%0.",digit,"f"), round(model$estimate[2],digit))
  }
  
  if (model$method=="One Sample t-test"){
    mean1 <- sprintf(paste0("%0.",digit,"f"), round(model$estimate[1],digit))
    mean2 <- NA
  }
  
  if (p.lowercase){
    pletter <- "p"
  } else {
    pletter <- "P"
  }
  
  pnumber <- sprintf(paste0("%0.",p.digit,"f"), round(model$p.value,p.digit))
  
  if (!include.zero.p){
    pnumber <- str_sub(pnumber,2,99) # excludee the 0 at the beginning
  }
  
  if (round(model$p.value,p.digit)==0){
    if (include.zero.p){
      p <- paste0(pletter,"<","0",str_pad(".",pad="0",width=p.digit,side="right"),"1")
    } else {
      p <- paste0(pletter,"<",str_pad(".",pad="0",width=p.digit,side="right"),"1")
    }
  } else {
      p <- paste0(pletter,"=",pnumber)
  }
    
  if (ci){
    conflo <- round(model$conf.int[1],digit)
    confhi <- round(model$conf.int[2],digit)
    result <- paste0("t(",df,")=",t.value,", CI 95% [",conflo,",",confhi,"], ",p)
  } else {
    result <- paste0("t(",df,")=",t.value,", ",p)
  }
  
  
  if (include.mean){
    if (mean.unit!=""){
      mean.unit <- paste0(" ",mean.unit)
    }
    if (model$method=="Welch Two Sample t-test"|model$method==" Two Sample t-test"){
        result <- paste0("M_",mean1.name,"=",mean1,mean.unit,     #", SD_",mean1.name,"=",sd1,mean.unit,
                         ", M_", mean2.name,"=",mean2,mean.unit,  #", SD_",mean2.name,"=",sd2,mean.unit,
                         ", ",
                         result)
    }
    if (model$method=="One Sample t-test"){
      result <- paste0("M=",mean1,mean.unit,
                       ", ",
                       result)
    }
  }
    
  if (verbose){
    return(result)
  }
  
}


# linear model result
print_lm_result <- function(model,var,digit=2,p.digit=3,
                             p.lowercase=T,include.zero.p=F,ci=T,
                             summary=F){
  s <- summary(model)
  c <- coef(s)
  if(summary) print(s)
  pvalue <- round(c[var,"Pr(>|t|)"],p.digit)
  if (pvalue==0){
    p <- paste0("<.",str_pad("1",width=p.digit,side="left",pad="0"))
  } else {
    p <- paste0("=",as.character(str_sub(pvalue,2,999)))
  }
  
  tvalue <- round(c[var,'t value'],digit)
  tvalue_div_by_decpoint <- str_split(as.character(tvalue),pattern="\\.",simplify=T)
  t <- paste0(tvalue_div_by_decpoint[1],
              ".",
              str_pad(tvalue_div_by_decpoint[2],width=digit,side="right",pad="0"))
  
  print(paste0("B=",round(c[var,"Estimate"],2),", SE=",round(c[var,"Std. Error"],digit),
               ", 95% CI [",round(c[var,"Estimate"] - 1.96 * c[var,"Std. Error"],digit),
               ",", round(c[var,"Estimate"] + 1.96 * c[var,"Std. Error"],digit),
               "], t=", t,
               ", p", p))
}


# linear mixed-effects model result (takes the lmerTest output)
print_lme_result <- function(model,var,digit=2,p.digit=3,
                            p.lowercase=T,include.zero.p=F,ci=T,
                            summary=F){
  s <- summary(model)
  c <- coef(s)
  if(summary) print(s)
  
  pvalue <- round(c[var,"Pr(>|t|)"],p.digit)
  if (pvalue==0){
    p <- paste0("<.",str_pad("1",width=p.digit,side="left",pad="0"))
  } else {
    p <- paste0("=",as.character(str_sub(pvalue,2,999)))
  }
  
  tvalue <- round(c[var,'t value'],digit)
  tvalue_div_by_decpoint <- str_split(as.character(tvalue),pattern="\\.",simplify=T)
  t <- paste0(tvalue_div_by_decpoint[1],
              ".",
              str_pad(tvalue_div_by_decpoint[2],width=digit,side="right",pad="0"))
  
  print(paste0("B=",round(c[var,"Estimate"],2),", SE=",round(c[var,"Std. Error"],digit),
               ", 95% CI [",round(c[var,"Estimate"] - 1.96 * c[var,"Std. Error"],digit),
               ",", round(c[var,"Estimate"] + 1.96 * c[var,"Std. Error"],digit),
               "], t=", t,
               ", p", p))
}


print_GEE_result <- function(model,var,digit=2,p.digit=3,
                           p.lowercase=T,include.zero.p=F,ci=T,
                           summary=F){
  s <- summary(model)
  c <- coef(s)
  if(summary) print(s)
  pvalue <- round(c[var,]$`Pr(>|W|)`,p.digit)
  if (pvalue==0){
    p <- paste0("<.",str_pad("1",width=p.digit,side="left",pad="0"))
  } else {
    p <- paste0("=",as.character(str_sub(pvalue,2,999)))
  }
  
  
  zvalue <- round(c[var,]$Wald^.5,digit)
  zvalue_div_by_decpoint <- str_split(as.character(zvalue),pattern="\\.",simplify=T)
  z <- paste0(zvalue_div_by_decpoint[1],
              ".",
              str_pad(zvalue_div_by_decpoint[2],width=digit,side="right",pad="0"))
  
  print(paste0("B=",round(c[var,"Estimate"],2),", SE=",round(c[var,"Std.err"],digit),
               ", 95% CI [",round(c[var,"Estimate"] - 1.96 * c[var,"Std.err"],digit),
               ",", round(c[var,"Estimate"] + 1.96 * c[var,"Std.err"],digit),
               "], Z=", z,
               ", p", p))
}



# linear mediation model result (takes the 'mediate' output)
print_lme_result <- function(model,var,digit=2,p.digit=3,
                             p.lowercase=T,include.zero.p=F,ci=T,
                             summary=F){
  s <- summary(model)
  c <- coef(s)
  if(summary) print(s)
  
  pvalue <- round(c[var,"Pr(>|t|)"],p.digit)
  if (pvalue==0){
    p <- paste0("<.",str_pad("1",width=p.digit,side="left",pad="0"))
  } else {
    p <- paste0("=",as.character(str_sub(pvalue,2,999)))
  }

  tvalue <- round(c[var,'t value'],digit)
  tvalue_div_by_decpoint <- str_split(as.character(tvalue),pattern="\\.",simplify=T)
  t <- paste0(tvalue_div_by_decpoint[1],
              ".",
              str_pad(tvalue_div_by_decpoint[2],width=digit,side="right",pad="0"))
  
  bvalue <- round(c[var,'Estimate'],digit)
  bvalue_div_by_decpoint <- str_split(as.character(bvalue),pattern="\\.",simplify=T)
  b <- paste0(bvalue_div_by_decpoint[1],
              ".",
              str_pad(bvalue_div_by_decpoint[2],width=digit,side="right",pad="0"))
  
  sevalue <- round(c[var,'Std. Error'],digit)
  sevalue_div_by_decpoint <- str_split(as.character(sevalue),pattern="\\.",simplify=T)
  se <- paste0(sevalue_div_by_decpoint[1],
              ".",
              str_pad(sevalue_div_by_decpoint[2],width=digit,side="right",pad="0"))

  print(paste0("B=",b,", SE=",se,
               ", 95% CI [",round(c[var,"Estimate"] - 1.96 * c[var,"Std. Error"],digit),
               ",", round(c[var,"Estimate"] + 1.96 * c[var,"Std. Error"],digit),
               "], t=", t,
               ", p", p))
}


print_cor_result <- function(model,digit=2,p.digit=3,n=NULL){
  require(psychometric)
  
  pvalue <- round(model$p.value,p.digit)
  if (pvalue==0){
    p <- paste0("<.",str_pad("1",width=p.digit,side="left",pad="0"))
  } else {
    p <- paste0("=",as.character(str_sub(pvalue,2,999)))
  }
  
  if (model$estimate>0){
    corcoef <- paste0("=",as.character(str_sub(round(model$estimate,digit),2,999)))
  } else {
    corcoef <- paste0("=-",as.character(str_sub(round(model$estimate,digit),3,999)))
  }
  
  if (substr(model$method,1,8)=="Pearson'"){
    
    n <- model$parameter + 2
    ci_lo <- round(CIr(model$estimate, n, level = 0.95)[1],digit)# %>% substr(2,999) # excluding 0 at beginning
    ci_hi <- round(CIr(model$estimate, n, level = 0.95)[2],digit)# %>% substr(2,999)
    
    print(paste0("r",corcoef,
                 ", 95% CI [",ci_lo,",", ci_hi,
                 "], t(",round(model$parameter,3),")=",round(model$statistic,digit),", p",p))
  }
  if (substr(model$method,1,8)=="Spearman"){
    
    cis <- sort(tanh(atanh(model$estimate) + c(-1,1)*sqrt((1+model$estimate^2/2)/(n-3))*qnorm(p = 0.05/2)))
    ci_lo <- round(cis[1],digit)# %>% substr(2,999) # excluding 0 at beginning
    ci_hi <- round(cis[2],digit)# %>% substr(2,999)
    
    
    print(paste0("rho",corcoef,
                 ", 95% CI [",ci_lo,",", ci_hi,
                 "], p",p))
  }

}
  
