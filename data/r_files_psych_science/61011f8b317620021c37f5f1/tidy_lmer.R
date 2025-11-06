# source: https://github.com/tidymodels/broom/issues/309 
tidy_lmer <- function(x, effects = c("ran_pars","fixed"),
                      scales = NULL, ## c("sdcor",NA),
                      ran_prefix=NULL,
                      conf.int = FALSE,
                      conf.level = 0.95,
                      conf.method = "Wald",
                      ...) {
  effect_names <- c("ran_pars", "fixed", "ran_modes")
  if (!is.null(scales)) {
    if (length(scales) != length(effects)) {
      stop("if scales are specified, values (or NA) must be provided ",
           "for each effect")
    }
  }
  if (length(miss <- setdiff(effects,effect_names))>0)
    stop("unknown effect type ",miss)
  base_nn <- c("estimate", "std.error", "df", "statistic", "p.value")
  ret_list <- list()
  if ("fixed" %in% effects) {
    # return tidied fixed effects rather than random
    ret <- stats::coef(summary(x))
    
    # p-values may or may not be included
    nn <- base_nn[1:ncol(ret)]
    
    if (conf.int) {
      cifix <- confint(x,parm="beta_",method=conf.method,...)
      ret <- data.frame(ret,cifix)
      nn <- c(nn,"conf.low","conf.high")
    }
    if ("ran_pars" %in% effects || "ran_modes" %in% effects) {
      ret <- data.frame(ret,group="fixed")
      nn <- c(nn,"group")
    }
    ret_list$fixed <-
      fix_data_frame(ret, newnames = nn)
  }
  if ("ran_pars" %in% effects) {
    if (is.null(scales)) {
      rscale <- "sdcor"
    } else rscale <- scales[effects=="ran_pars"]
    if (!rscale %in% c("sdcor","vcov"))
      stop(sprintf("unrecognized ran_pars scale %s",sQuote(rscale)))
    ret <- as.data.frame(VarCorr(x))
    ret[] <- lapply(ret, function(x) if (is.factor(x))
      as.character(x) else x)
    if (is.null(ran_prefix)) {
      ran_prefix <- switch(rscale,
                           vcov=c("var","cov"),
                           sdcor=c("sd","cor"))
    }
    pfun <- function(x) {
      v <- na.omit(unlist(x))
      if (length(v)==0) v <- "Observation"
      p <- paste(v,collapse=".")
      if (!identical(ran_prefix,NA)) {
        p <- paste(ran_prefix[length(v)],p,sep="_")
      }
      return(p)
    }
    
    rownames(ret) <- paste(apply(ret[c("var1","var2")],1,pfun),
                           ret[,"grp"], sep = ".")
    
    ## FIXME: this is ugly, but maybe necessary?
    ## set 'term' column explicitly, disable fix_data_frame
    ##  rownames -> term conversion
    ## rownames(ret) <- seq(nrow(ret))
    
    if (conf.int) {
      ciran <- confint(x,parm="theta_",method=conf.method,...)
      ret <- data.frame(ret,ciran)
      nn <- c(nn,"conf.low","conf.high")
    }
    
    
    ## replicate lme4:::tnames, more or less
    ret_list$ran_pars <- fix_data_frame(ret[c("grp", rscale)],
                                        newnames = c("group", "estimate"))
  }
  if ("ran_modes" %in% effects) {
    ## fix each group to be a tidy data frame
    
    nn <- c("estimate", "std.error")
    re <- ranef(x,condVar=TRUE)
    getSE <- function(x) {
      v <- attr(x,"postVar")
      setNames(as.data.frame(sqrt(t(apply(v,3,diag)))),
               colnames(x))
    }
    fix <- function(g,re,.id) {
      newg <- fix_data_frame(g, newnames = colnames(g), newcol = "level")
      # fix_data_frame doesn't create a new column if rownames are numeric,
      # which doesn't suit our purposes
      newg$level <- rownames(g)
      newg$type <- "estimate"
      
      newg.se <- getSE(re)
      newg.se$level <- rownames(re)
      newg.se$type <- "std.error"
      
      data.frame(rbind(newg,newg.se),.id=.id,
                 check.names=FALSE)
      ## prevent coercion of variable names
    }
    
    mm <- do.call(rbind,Map(fix,coef(x),re,names(re)))
    
    ## block false-positive warnings due to NSE
    type <- spread <- est <- NULL
    mm %>% gather(term, estimate, -.id, -level, -type) %>%
      spread(type,estimate) -> ret
    
    ## FIXME: doesn't include uncertainty of population-level estimate
    
    if (conf.int) {
      if (conf.method != "Wald")
        stop("only Wald CIs available for conditional modes")
      
      mult <- qnorm((1+conf.level)/2)
      ret <- transform(ret,
                       conf.low=estimate-mult*std.error,
                       conf.high=estimate+mult*std.error)
    }
    
    ret <- dplyr::rename(ret,group=.id)
    ret_list$ran_modes <- ret
  }
  return(plyr::rbind.fill(ret_list))
}