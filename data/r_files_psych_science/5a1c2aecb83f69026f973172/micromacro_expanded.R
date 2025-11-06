micromacro.lm <- function (model, adjusted.predictors, y, unequal.groups = NULL) 
{
  data = cbind(adjusted.predictors, y)
  uncorrected.output = lm(model, data)
  model.terms = attr(terms.formula(model), "term.labels")
  model.variables = model.terms[!grepl(":", model.terms)]
  interaction.terms = model.terms[grepl(":", model.terms)]
  if (length(interaction.terms) > 0) {
    interactions = matrix(NA, nrow = dim(data)[1], ncol = length(interaction.terms))
    for (i in 1:length(interaction.terms)) {
      interactions[, i] = apply(data[strsplit(interaction.terms, 
                                              ":")[[i]]], 1, prod)
    }
    u = cbind(rep(1, dim(data)[1]), as.matrix(cbind(data[model.variables], 
                                                    interactions)))
  }
  else {
    u = cbind(rep(1, dim(data)[1]), as.matrix(cbind(data[model.variables])))
  }
  e = uncorrected.output$residuals
  p = solve(t(u) %*% u)
  h = diag(u %*% p %*% t(u))
  d = e^2/(1 - h)
  v = p %*% t(u) %*% diag(d) %*% u %*% p
  se = sqrt(diag(v))
  estim = summary(uncorrected.output)$coefficients[, 1:2]
  t.stats = estim[, 1]/se
  p.values = pt(-abs(t.stats), rep(uncorrected.output$df.residual, 
                                   length(model.terms) + 1)) * 2
  effect.sizes = sqrt(t.stats^2/(t.stats^2 + uncorrected.output$df.residual))
  corrected.Fs<- t.stats^2
  denom.df <- uncorrected.output$df.residual
  num.df <- rep(1, length(uncorrected.output$coefficients))
  alt.effect.sizes = (num.df/denom.df*corrected.Fs)/(1+(num.df/denom.df*corrected.Fs))
  estim1 = cbind(estim, se, rep(uncorrected.output$df.residual, 
                                length(model.terms) + 1), t.stats, p.values, effect.sizes, alt.effect.sizes)
  dimnames(estim1)[[2]] = c("Estimate", "Uncorrected S.E.", 
                            "Corrected S.E.", "df", "t", "Pr(>|t|)", "r", "R^2")
  if (is.null(unequal.groups)) {
    t.stats2 = estim[, 1]/estim[, 2]
    p.values2 = pt(-abs(t.stats2), rep(uncorrected.output$df.residual, 
                                       length(model.terms) + 1)) * 2
    effect.sizes2 = sqrt(t.stats2^2/(t.stats2^2 + uncorrected.output$df.residual))
    corrected.Fs2<- t.stats2^2
    denom.df2 <- uncorrected.output$df.residual
    num.df2 <- rep(1, length(uncorrected.output$coefficients))
    alt.effect.sizes2 = (num.df2/denom.df2*corrected.Fs2)/(1+(num.df2/denom.df2*corrected.Fs2))
    estim2 = cbind(estim, rep(uncorrected.output$df.residual, 
                              length(model.terms) + 1), t.stats2, p.values2, effect.sizes2, alt.effect.sizes2)
    dimnames(estim2)[[2]] = c("Estimate", "S.E.", "df", "t", 
                              "Pr(>|t|)", "r", "R^2")
    report = list(statistics = estim2, rsquared = summary(uncorrected.output)$r.squared, 
                  rsquared.adjusted = summary(uncorrected.output)$adj.r.squared, 
                  residuals = uncorrected.output$residuals, fitted.values = uncorrected.output$fitted.values, 
                  fstatistic = summary(uncorrected.output)$fstatistic, 
                  AIC = AIC(uncorrected.output), BIC = BIC(uncorrected.output), 
                  model.formula = model)
  }
  else if (isTRUE(unequal.groups)) {
    report = list(statistics = estim1, rsquared = summary(uncorrected.output)$r.squared, 
                  rsquared.adjusted = summary(uncorrected.output)$adj.r.squared, 
                  residuals = uncorrected.output$residuals, fitted.values = uncorrected.output$fitted.values, 
                  fstatistic = summary(uncorrected.output)$fstatistic,  
                  AIC = AIC(uncorrected.output), BIC = BIC(uncorrected.output), 
                  model.formula = model)
  }
  else {
    t.stats2 = estim[, 1]/estim[, 2]
    p.values2 = pt(-abs(t.stats2), rep(uncorrected.output$df.residual, 
                                       length(model.terms) + 1)) * 2
    effect.sizes2 = sqrt(t.stats2^2/(t.stats2^2 + uncorrected.output$df.residual))
    corrected.Fs2<- t.stats2^2
    denom.df2 <- uncorrected.output$df.residual
    num.df2 <- rep(1, length(uncorrected.output$coefficients))
    alt.effect.sizes2 = (num.df2/denom.df2*corrected.Fs2)/(1+(num.df2/denom.df2*corrected.Fs2))
    estim2 = cbind(estim, rep(uncorrected.output$df.residual, 
                              length(model.terms) + 1), t.stats2, p.values2, effect.sizes2, alt.effect.sizes2)
    dimnames(estim2)[[2]] = c("Estimate", "S.E.", "df", "t", 
                              "Pr(>|t|)", "r", "R^2")
    report = list(statistics = estim2, rsquared = summary(uncorrected.output)$r.squared, 
                  rsquared.adjusted = summary(uncorrected.output)$adj.r.squared, 
                  residuals = uncorrected.output$residuals, fitted.values = uncorrected.output$fitted.values, 
                  fstatistic = summary(uncorrected.output)$fstatistic,  
                  AIC = AIC(uncorrected.output), BIC = BIC(uncorrected.output), 
                  model.formula = model)
  }
  return(report)
}