# functions for Lure of Counterfactual Curiosity analysis

# internal random effects meta analysis of fixed effects parameter estimates from mixed effects models
meta_func <- function(model_list, studies){
  names(model_list) <- studies
  coefs <- data.frame(lapply(model_list, function(x){
    summary(x)$coef[1:nrow(summary(x)$coef),1:2, drop = F]}))
  coefs <- coefs[,grepl(paste(gsub(" ", ".", studies),collapse="|"), colnames(coefs))]
  metas <- lapply(as.list(row.names(coefs)), function(predictor){
    data <- data.frame(yi = as.numeric(coefs[predictor,seq(1,ncol(coefs)-1,2)]),
                       sei = as.numeric(coefs[predictor,seq(2,ncol(coefs),2)]))
    rma.uni(yi=yi, sei=sei, data = data, slab = studies, method = "REML")
  })
  names(metas) <- row.names(coefs)
  df <- data.frame(
    beta = unlist(lapply(metas, function(x)x$beta)),
    se = unlist(lapply(metas, function(x)x$se)),
    ci.lb = unlist(lapply(metas, function(x)x$ci.lb)),
    ci.ub = unlist(lapply(metas, function(x)x$ci.ub)),
    z = unlist(lapply(metas, function(x)x$zval)),
    p = unlist(lapply(metas, function(x)x$pval)),
    psig = ifelse(unlist(lapply(metas, function(x)x$pval))<0.05,"*",""),
    I2 = unlist(lapply(metas, function(x)x$I2)),
    Q = unlist(lapply(metas, function(x)x$QE)),
    Qp = unlist(lapply(metas, function(x)x$QEp)),
    Qpsig = ifelse(unlist(lapply(metas, function(x)x$QEp))<0.05,"*","")
  )
  list(metas=metas, df=df)
}

# internal random effects meta analysis of path estimates from SEMs
meta_func_sem <- function(med_list, studies){
  pe1 <- med_list[[1]]$pe
  paths <- paste(pe1$lhs[pe1$label!=""], pe1$op[pe1$label!=""], pe1$rhs[pe1$label!=""])
  labels <- pe1$label
  pe <- lapply(med_list, function(x)x$pe)
  pe <- as.data.frame(data.table::rbindlist(pe))
  pe$path <- paste(pe$lhs, pe$op, pe$rhs)
  metas <- lapply(as.list(paths), function(path){
    data <- data.frame(yi = as.numeric(pe[pe$path==path,"est"]),
                       sei = as.numeric(pe[pe$path==path,"se"]))
    rma.uni(yi=yi, sei=sei, data = data, slab = studies, method = "REML")
  })
  names(metas) <- paths
  df <- data.frame(
    lhs = pe1$lhs[pe1$label!=""],
    op = pe1$op[pe1$label!=""],
    rhs = pe1$rhs[pe1$label!=""],
    label = labels[pe1$label!=""],
    est = unlist(lapply(metas, function(x)x$beta)),
    se = unlist(lapply(metas, function(x)x$se)),
    ci.lower = unlist(lapply(metas, function(x)x$ci.lb)),
    ci.upper = unlist(lapply(metas, function(x)x$ci.ub)),
    z = unlist(lapply(metas, function(x)x$zval)),
    pvalue = unlist(lapply(metas, function(x)x$pval)),
    psig = ifelse(unlist(lapply(metas, function(x)x$pval))<0.05,"*",""),
    I2 = unlist(lapply(metas, function(x)x$I2)),
    Q = unlist(lapply(metas, function(x)x$QE)),
    Qp = unlist(lapply(metas, function(x)x$QEp)),
    Qpsig = ifelse(unlist(lapply(metas, function(x)x$QEp))<0.05,"*","")
  )
  list(metas=metas, df=df)
}

# functions from the semoutput package, package not available for R 4.0.1 (https://rdrr.io/github/dr-JT/semoutput/src/R/sem_sig.R)

# function to export all SEM paths in data frame format
sem_paths <- function(x, standardized = TRUE, ci = "standardized",
                      ci.level = 0.95, digits = 3, print = TRUE){
  if (standardized == FALSE){
    ci <- "unstandardized"
  }
  if (ci == "standardized"){
    table <- lavaan::standardizedSolution(x, level = ci.level)
    table <- dplyr::filter(table, op == "~" | op == ":=")
    table <- dplyr::mutate(table,
                           stars = ifelse(pvalue < .001, "***",
                                          ifelse(pvalue < .01, "**",
                                                 ifelse(pvalue < .05, "*", ""))))
    table <- dplyr::select(table, Predictor = rhs, DV = lhs,
                           `Path Values` = est.std, SE = se, z, 'sig' = stars,
                           p = pvalue, Lower.CI = ci.lower, Upper.CI = ci.upper)
    
    if (print == TRUE){
      table <- knitr::kable(table, digits = digits, format = "html",
                            caption = "Regression Paths")
      table <- kableExtra::kable_styling(table)
      table <- kableExtra::add_header_above(table, c(" ", " ",
                                                     "Standardized" = 7))
    }
  }
  
  if (ci == "unstandardized"){
    table <- lavaan::parameterEstimates(x, standardized = standardized)
    table <- dplyr::filter(table, op == "~" | op == ":=")
    table <- dplyr::mutate(table,
                           stars = ifelse(pvalue < .001, "***",
                                          ifelse(pvalue < .01, "**",
                                                 ifelse(pvalue < .05, "*", ""))))
    if (standardized == TRUE){
      table <- dplyr::select(table, Predictor = rhs, DV = lhs,
                             `Path Values` = est, SE = se, z, 'sig' = stars,
                             p = pvalue, Lower.CI = ci.lower, Upper.CI = ci.upper,
                             Loadings.std = std.all)
      if (print == TRUE){
        table <- knitr::kable(table, digits = digits, format = "html",
                              caption = "Regression Paths")
        table <- kableExtra::kable_styling(table)
        table <- kableExtra::add_header_above(table,
                                              c(" ", " ",
                                                "Unstandardized" = 7,
                                                "Standardized" = 1))
      }
    }
    
    if (standardized == FALSE){
      table <- dplyr::select(table, Predictor = rhs, DV = lhs,
                             `Path Values` = est, SE = se, z, 'sig' = stars,
                             p = pvalue, Lower.CI = ci.lower, Upper.CI = ci.upper)
      if (print == TRUE){
        table <- knitr::kable(table, digits = digits, format = "html",
                              caption = "Regression Paths")
        table <- kableExtra::kable_styling(table)
        table <- kableExtra::add_header_above(table,
                                              c(" ", " ",
                                                "Unstandardized" = 7))
      }
    }
  }
  if (print == FALSE){
    table <- as.data.frame(table)
  }
  return(table)
}

# function to give fit statistics from SEM
sem_sig <- function(x, print = TRUE){
  stats <- lavaan::fitMeasures(x, c("ntotal", "chisq", "pvalue", "df"))
  table <- data.frame('Sample Size' = stats[["ntotal"]],
                      'Chi-Square' = stats[["chisq"]],
                      df = stats[["df"]], 'p-value' = stats[["pvalue"]])
  
  if (print == TRUE){
    table <- knitr::kable(table, digits = 3, format = "html",
                          caption = "Model Significance", row.names = FALSE)
    table <- kableExtra::kable_styling(table, full_width = FALSE,
                                       position = "left")
  } else if (print == FALSE){
    table <- as.data.frame(table)
  }
  
  return(table)
}

# rain plot with default labelling
rain_plot <- function(data){
  y="emotion_change_rating"
  col="`Information seeking`"
  geom="mean_ci"
  palette="Set1"
  guide="legend"
  y.lab = "Mean emotion change rating"
  title="a"
  facet=NA
  col_int = NA
  guide.name=""
  
  p <- ggplot(data,aes_string(x=1,y=y,fill=col))+
    geom_flat_violin(aes_string(fill=col),position=position_nudge(x=-0.5,y=0),adjust=1.5,trim=F,alpha=.5,colour=NA)+
    scale_fill_brewer(palette=palette, guide=guide, name = guide.name)+
    geom_point(aes_string(x="as.numeric(factor(experiment))/12-.05",y=y,colour=ifelse(!is.na(col_int), paste0("interaction(",col, ", ", col_int, ")"), col)), position=position_jitter(width=.025),size=.5,shape=20,alpha=0.7)+
    labs(title=title, y=y.lab)+
    theme_classic()+
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.x = element_blank(), axis.line.x = element_blank())
  
  if(!is.na(col_int)){
    
    p <- p+scale_color_manual(values=c(brewer.pal(n=9, name="Blues")[c(3,4,5,7,8)], brewer.pal(n=9, name="Reds")[c(3,4,5,7,8)]), guide=F)+new_scale_color()
    
  }else{
    
    p <- p+scale_colour_brewer(palette=palette, guide=guide, name = guide.name)
  }
  
  switch (geom,
          boxplot = p <- p+geom_boxplot(aes_string(x=1,y=y,fill=col),outlier.shape=NA,alpha=.5,width=.1,colour="black"),
          mean_ci =  p <- p+stat_summary(aes_string(x=0.45,colour=col), size = 0.4, fun.data = mean_cl_boot, position = position_dodge(width=0.05), geom="errorbar", width = 0)+stat_summary(aes_string(x=0.45,colour=col), size = 1.5, fun.data = mean_cl_boot, position = position_dodge(width=0.05), geom="point")+ scale_colour_brewer(palette=palette, guide=guide, name = guide.name)
  )
  
  if(!is.na(facet)){
    p <- p+facet_wrap(facet)+theme(legend.position = c(0.85,0.25))
  }
  p <- p+theme(legend.position = c(0.85,0.85))+coord_cartesian(ylim = c(-200,200))
  p
}

# flat violin plot
source("https://gist.githubusercontent.com/benmarwick/2a1bb0133ff568cbe28d/raw/fb53bd97121f7f9ce947837ef1a4c65a73bffb3f/geom_flat_violin.R")

# another plot function for the replication analysis (the original one was not sufficiently flexible)
plot_summary <- function(data, y, col, geom="boxplot", palette="Set1", title, guide="legend", y.lab = "Emotion change rating", facet=NA, col_int = NA, guide.name="", adjust=1.5){
  
  p <- ggplot(data,aes_string(x=1,y=y,fill=col))+
    geom_flat_violin(aes_string(fill=col),position=position_nudge(x=.07,y=0),adjust=adjust,trim=F,alpha=.5,colour=NA)+
    scale_fill_brewer(palette=palette, guide=guide, name = guide.name)+
    geom_point(aes_string(x=1-.03,y=y,colour=ifelse(!is.na(col_int), paste0("interaction(",col, ", ", col_int, ")"), col)), position=position_jitter(width=.03),size=.5,shape=20,alpha=0.7)+
    labs(title=title, y=y.lab)+
    theme_classic()+
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.x = element_blank(), axis.line.x = element_blank())
  
  if(!is.na(col_int)){
    
    p <- p+scale_color_manual(values=c(brewer.pal(n=9, name="Blues")[c(3,4,5,7,8)], brewer.pal(n=9, name="Reds")[c(3,4,5,7,8)]), guide=F)+new_scale_color()
    
  }else{
    
    p <- p+scale_colour_brewer(palette=palette, guide=guide, name = guide.name)
  }
  
  switch (geom,
          boxplot = p <- p+geom_boxplot(aes_string(x=1,y=y,fill=col),outlier.shape=NA,alpha=.5,width=.1,colour="black"),
          mean_ci =  p <- p+stat_summary(aes_string(x=1.035,colour=col), size = 0.4, fun.data = mean_cl_boot, geom="errorbar", width=0, position = position_dodge(width=0.05))+
            stat_summary(aes_string(x=1.035,colour=col), geom="point", size = 1.5, fun.data = mean_cl_boot, position = position_dodge(width=0.05))+
            scale_colour_brewer(palette=palette, guide=guide, name = guide.name)
          
  )
  
  if(!is.na(facet)){
    p <- p+facet_wrap(facet)+theme(legend.position = c(0.85,0.25))
  }
  p
}

# forest plots for showing parameter estimates of all parameters in internal meta analysis

ggforest = function(x, intercept=F, labels = NULL, hetero = "none", palette = RColorBrewer::display.brewer.pal(6,"Set1"), x_lab = "Observed Outcome"){
  require("ggplot2")
  # Function to convert REM results in `rma`-format into a data.frame
  rma2df = function(x){
    df <- as.data.frame(data.table::rbindlist(lapply(x, function(x){
      rbind(
        data.frame(Study = "RE Model", LogFC = x$b, CILB=x$ci.lb, CIUB=x$ci.ub,
                   p = x$pval, group = "Meta-\nanalysis",
                   I2 = x$I2,
                   Qp = x$QEp,
                   stringsAsFactors = FALSE),
        data.frame(Study = x$slab, LogFC = x$yi, 
                   CILB=x$yi - 2*sqrt(x$vi),
                   CIUB=x$yi + 2*sqrt(x$vi), 
                   p = x$pval, group = "Experiments",
                   I2 = NA,
                   Qp = NA,
                   stringsAsFactors = FALSE)
      )})))
    df$predictor <- rep(names(x), each = nrow(df)/length(x))
    df
  }
  remresdf = rma2df(x)
  remresdf <- transform(remresdf, interval = CIUB - CILB)
  remresdf <- transform(remresdf, RelConf = 1/interval)
  if(!intercept){
    remresdf <- remresdf[remresdf$predictor!="(Intercept)",]
    remresdf$predictor <- factor(remresdf$predictor, levels=unique(remresdf$predictor))
    if(!is.null(labels)){
      remresdf$predictor <- factor(remresdf$predictor, labels=labels)
    }
  }
  p = ggplot(remresdf, 
             aes(LogFC, factor(Study, c(rev(unique(remresdf$Study)[2:length(unique(remresdf$Study))]), "RE Model")), xmax=CIUB, xmin=CILB, shape = group, colour = predictor)) + 
    geom_vline(xintercept = 0.0, linetype=2, alpha=0.75) +
    geom_errorbarh(alpha=0.5, color="black", height = 0.1) + 
    geom_point(aes(size = RelConf)) +
    scale_size(range = c(2, 5), guide=FALSE) +
    geom_point(data = subset(remresdf, Study=="RE Model"), size=3) +
    theme_bw() + 
    theme(text = element_text(size=16), panel.grid.minor = element_blank()) +
    facet_grid(group~predictor, scales= 'free', space = 'free_y') +
    labs(x = x_lab, y = NULL)+
    scale_colour_manual(values = palette)+
    guides(colour = F, shape = F)+
    scale_x_continuous(breaks = scales::pretty_breaks(3), labels = function(x) round(as.numeric(x), digits=2))+ 
    expand_limits(x = c(-0.02,0.02))
  if(hetero == "I2"){
    p <- p+geom_label(aes(x = Inf, y = -Inf, 
                          hjust = 1,
                          vjust = 0,
                          label=ifelse(is.na(I2), I2, paste0("I\u00B2 = ",round(I2,0), "%"))), 
                      #position = position_nudge(y = -0.4), 
                      colour = 'black',label.size=0)
  }else if(hetero == "Qp"){
    p <- p+geom_label(aes(label=ifelse(is.na(Qp), NA, ifelse(Qp<0.001, "***",ifelse(Qp<0.01, "**", ifelse(Qp<0.05, "*", ""))))), position = position_nudge(y = 0.2), colour = 'black',label.size=0)
  }
  return(p)
}


