#checking descriptives

descs <- function(data){
        desc <- psych::describe(data)
        a <- desc
        b <- knitr::kable(x=desc, align = "c", caption="Descriptive Statistics", digits=2)
        viewDescs <- list("quick" = a, "knitted" = b)
        return(viewDescs)
}

##plots

hists <- function(data, color, ...){
        for(i in 1:length(data)){
                barplot(table(data[i]), col = color,ylim=c(0,max(table(data[i]))+10), main = names(data[i]))
                box()
        }
}

#####CFA############

#Run CFA

runCFA <- function(model, data, estimator="mlr", missing="fiml", std.lv=TRUE){
        fit <- lavaan::cfa(model, data, estimator=estimator, std.lv=std.lv, missing=missing)
        return(fit)
}

#extract model fit
getFit <- function(modelfit, ic=FALSE, ...) {
        fits <- lavaan::fitMeasures(modelfit)
        if(ic==TRUE && "cfi.robust"%in%names(fits) == TRUE){
                fits_rel <-
                        as.table(round(fits[c(
                                "chisq.scaled",
                                "df.scaled",
                                "pvalue.scaled",
                                "cfi.robust",
                                "rmsea.robust",
                                "rmsea.ci.lower.robust",
                                "rmsea.ci.upper.robust",
                                "srmr_mplus",
                                "aic",
                                "bic",
                                "bic2"
                        )], 3))
                a <- fits_rel
                b <-
                        knitr::kable(
                                x = as.data.frame(fits_rel),
                                align = "c",
                                caption = "Modellfit",
                                digits = 3,
                                col.names = c("Fitmaß", "Wert")
                        )
                viewFit <- list("quick" = a, "knitted" = b)
                return(viewFit)}
        else if (ic == FALSE && "cfi.robust"%in%names(fits) == TRUE) {
                fits_rel <-
                        as.table(round(fits[c(
                                "chisq.scaled",
                                "df.scaled",
                                "pvalue.scaled",
                                "cfi.robust",
                                "rmsea.robust",
                                "rmsea.ci.lower.robust",
                                "rmsea.ci.upper.robust",
                                "srmr_mplus"
                        )], 3))
                a <- fits_rel
                b <-
                        knitr::kable(
                                x = as.data.frame(fits_rel),
                                align = "c",
                                caption = "Modellfit",
                                digits = 3,
                                col.names = c("Fitmaß", "Wert")
                        )
                viewFit <- list("quick" = a, "knitted" = b)
                return(viewFit)
        } else if (ic == TRUE && "cfi.robust"%in%names(fits) == FALSE) {
                fits_rel <-
                        as.table(round(fits[c(
                                "chisq",
                                "df",
                                "pvalue",
                                "cfi",
                                "rmsea",
                                "rmsea.ci.lower",
                                "rmsea.ci.upper",
                                "srmr_mplus",
                                "aic",
                                "bic",
                                "bic2"
                        )], 3))
                a <- fits_rel
                b <-
                        knitr::kable(
                                x = as.data.frame(fits_rel),
                                align = "c",
                                caption = "Modellfit",
                                digits = 3,
                                col.names = c("Fitmaß", "Wert")
                        )
                viewFit <- list("quick" = a, "knitted" = b)
                return(viewFit)
        } else{
                fits_rel <-
                        as.table(round(fits[c(
                                "chisq",
                                "df",
                                "pvalue",
                                "cfi",
                                "rmsea",
                                "rmsea.ci.lower",
                                "rmsea.ci.upper",
                                "srmr_mplus"
                        )], 3))
                a <- fits_rel
                b <-
                        knitr::kable(
                                x = as.data.frame(fits_rel),
                                align = "c",
                                caption = "Modellfit",
                                digits = 3,
                                col.names = c("Fitmaß", "Wert")
                        )
                viewFit <- list("quick" = a, "knitted" = b)
                return(viewFit)}
}

getFitOrd <- function(modelfit, ...) {
        fits <- lavaan::fitMeasures(modelfit)
       fits_rel <-
                        as.table(round(fits[c(
                                "chisq.scaled",
                                "df.scaled",
                                "pvalue.scaled",
                                "cfi.scaled",
                                "rmsea.scaled",
                                "rmsea.ci.lower.scaled",
                                "rmsea.ci.upper.scaled",
                                "srmr_bentler"
                        )], 3))
                a <- fits_rel
                b <-knitr::kable(
                                x = as.data.frame(fits_rel),
                                align = "c",
                                caption = "Modellfit",
                                digits = 3,
                                col.names = c("Fitmaß", "Wert")
                        )
                viewFit <- list("quick" = a, "knitted" = b)
                return(viewFit)}


#Parceling
#scale.method can be rowSums or rowMeans

parseq <- function(data, nparc, scale.method){
        library(dplyr)
        parcels_list <- c()
        # Schleife
        for (i in 1:ncol(data[,c(1:nparc)])){
                # gruppiere Items sequenziell und berechne Mittelwerte/Summenwerte ?ber die Items der Parcels   
                parcels_list[[i]] = scale.method(data[,c(seq(from=i, to=length(data), by=nparc))])
        }
        
        # Füge Parcels in einem Datensatz zusammen  
        bind_cols(parcels_list)
        
}

parceling <- function(data, parcels, method=rowMeans, na.rm=TRUE){
        parcellist <- list(NULL)
        for(i in 1:parcels){
                parcellist[[i]] <- method(data[,c(seq(i,ncol(data), parcels))], na.rm=na.rm)
        }
        names(parcellist) <- c(paste("Parc", seq(1,parcels,1), sep = ""))
        dataNew <- as.data.frame(cbind(data, bind_cols(parcellist)))
        return(dataNew)
}

#Omega W (also for MI)

omega2 <- function(fit){
        std_fit <-  standardizedSolution(fit, type="std.all")
        if("group"%in% colnames(std_fit)==TRUE){
        loadsByGroup <- list(NULL)
        for(i in 1:max(std_fit$group)){
                loadsByGroup[[i]] <- subset(std_fit, std_fit$group==i)
              #  return(loadsByGroup)
        }
        latent_vars <- list(NULL)
        factor_names <- list(NULL)
        num.factors <- list(NULL)
        for (i in 1:max(std_fit$group)){
                latent_vars[[i]] <-  subset(loadsByGroup[[i]], op == "=~")
                factor_names[[i]] <- unique(latent_vars[[i]]$lhs)
                num.factors[[i]] <-  length(factor_names[[i]])
        }
        omega_list <- list(NULL)
        for(j in 1:max(std_fit$group)){
                omega_list[[j]] <- vector("list", 0)
        }
        for(i in 1:max(std_fit$group)){
                for(j in 1:max(num.factors[[i]])){
                        sub_lhs <-  subset(latent_vars[[i]], lhs == factor_names[[i]][[j]])
                        para_i  <-  sub_lhs$est.std
                        parameters_sq <-  para_i^2
                        parameters_sq_diff_to_one <-  1-parameters_sq
                        ratio <- parameters_sq/parameters_sq_diff_to_one
                        cum_ratio <- sum(ratio)
                        omega_w_i <- round(cum_ratio/(cum_ratio+1), 2)
                        omega_list[[i]][j] = omega_w_i}
        }
        for(i in 1:max(std_fit$group)){
                names(omega_list[[i]]) <- c(unlist(factor_names[i]))
        }
        omegas_group <- data.frame(matrix(unlist(omega_list), nrow=length(omega_list), byrow=TRUE))
        colnames(omegas_group) <- factor_names[[1]]
        row.names(omegas_group) <- c(fit@Data@group.label)
        return(omegas_group)
        }else{
                latent_vars <-  subset(std_fit, op == "=~")
                factor_names <- unique(latent_vars$lhs)
                num.factors <-  length(factor_names)
                omega_list <- vector("list", 0)
                for(i in 1:num.factors){
                        sub_lhs <-  subset(latent_vars, lhs == factor_names[i])
                        para_i  <-  sub_lhs$est.std
                        parameters_sq <-  para_i^2
                        parameters_sq_diff_to_one 	<-  1-parameters_sq
                        ratio <- parameters_sq/parameters_sq_diff_to_one
                        cum_ratio <- sum(ratio)
                        omega_w_i <- round(cum_ratio/(cum_ratio+1), 2)
                        omega_list[factor_names[i]] = omega_w_i
                }
                omega_w <- sapply(omega_list, "[", 1)
                as.data.frame(omega_w)
        }
}



