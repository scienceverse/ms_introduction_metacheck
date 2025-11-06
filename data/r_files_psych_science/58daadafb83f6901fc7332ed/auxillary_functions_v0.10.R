# Auxillary functions
# NOTE: while I have error checked these, they are not guarenteed to work across all contexts
# They are further not optimized for speed, nor robust to failures of input. Use at your own risk

estimatePoints = function(formula, data, modelFn, ...) {
  # Takes a formula with an interaction term and generates point estimates and standard errors for different values of 
  # the interaction terms. Currently only dichotomous and continuous variables supported (no categorical).
  # Dichotomous variables are probed at each level, and continuous values are probed at +/- 1 standard deviation
  # (more flexibility to be supported in the future. Maybe). 
  # Report errors to: Paul Stillman (paul.e.stillman@gmail.com)
  #
  # Args: 
  #   formula: formula specification that you want to generate point estimates for.
  #            IMPORTANT -- all interaction terms must precede any covariates (all *'s must be to the left of +'s).
  #            Not currently robust to lots of weird stuff in the formula. 
  #   data: dataset containing the variables defined in formula. Any categorical variables 
  #         must be dummy or effects coded (for now). Only 2 levels currently supported
  #   modelFn: the function used to model the formula (e.g., lm, glm, lmer, glmer, etc.).
  #   ...: extra values passed to the modelFn. For exampel, when passing glm, one might also want to specify
  #        family=binomial
  #
  # Returns:
  #   A dataframe with estimated value and standard error with every combination of the various levels of the IVs  
  #
  # Example uses:
  #   estimatePoints(continuousDV ~ IV1 * IV2, myData, lm)
  #   estimatePoints(dichotomousDV ~ IV1 * IV2 * IV3 + covariate1 + covariate2, myData, glm, family = binomial)
  #   estimatePoints(continuousDV ~ IV1 * IV2 + (1|groupingVar), myData, lmer)
  #   estimatePoints(mpg ~ vs * hp + carb, mtcars, lm)
  
  require(formula.tools)
  require(stringr)
  if (class(formula) == 'character') formula <- formula(formula)  # If provided with a string, convert to formula
  
  mf = get_all_vars(formula=formula, data=data)  # Extract the IVs
  
  # Currently assumes all interaction terms will be to the left of the +, so 
  # split the formula string where the plus is (if there is a plus)
  form_char = as.character(formula)
  plusStart = str_locate(form_char, '\\+') 
  
  if(!is.na(plusStart[1])) {
    covars = str_sub(form_char, start = plusStart[1]) 
  } else {
    covars = ""
  }
  
  # Isolates just the interaction variables
  numIntTerms = 1 + str_count(form_char, '\\*')
  df_IVs = mf[, -1]  #chops off the first, which is the DV.
  df_IVs = df_IVs[, 1:numIntTerms]  #just keep the interaction terms
  
  # Center the IVs at different levels. Returns a list of lists, which contain both the centered variables,
  # And the value around which they are centered for each IV
  centered_vars = apply(df_IVs, 2, function(iv) {
    if(length(unique(iv[!is.na(iv)])) == 2) {
      # IV is dichotomous
      vals = sort(unique(iv[!is.na(iv)]))
      high_point = vals[2]
      low_point = vals[1]
      dichotomous = TRUE
    }
    else {
      # not dichotmous, center and whatever
      high_point = mean(iv, na.rm = TRUE) + sd(iv, na.rm = TRUE)
      low_point = mean(iv, na.rm = TRUE) - sd(iv, na.rm= TRUE)
      dichotomous = FALSE
    }
    centered_low = iv - low_point
    centered_high = iv - high_point
    
    low = list()
    low$var = centered_low
    low$val = low_point
    high = list()
    high$var = centered_high
    high$val = high_point
    combo = list(high=high, low=low)
    #combo = list(high = high$var, low = low$var) #this gives all the variables (no values) in a single list, formatted nicely
    #combo = list(high=high$val, low=low$val) #This just gives the vals
  })
  
  # Add the centered variables to a new model data frame
  mf_plus = mf
  iv_names = names(df_IVs)
  for (i in 1:length(centered_vars)) {
    # Add in the new centered variables to the dataframe:
    mf_plus[[paste(iv_names[i], 'high', sep='_')]] = centered_vars[[i]]$high$var
    mf_plus[[paste(iv_names[i], 'low', sep='_')]] = centered_vars[[i]]$low$var
  }
  
  # Get all the permutations of low and high
  tmp = list()
  tmp$first = 'low'
  tmp$second = 'high'
  levList = rep(list(tmp), length(centered_vars))
  levels = expand.grid(levList)
  
  # Run each model. First sequentially constructs the formulas to be used
  dv_str = paste(all.vars(formula)[1], '~', sep='')  # starts with the DV
  outMat = apply(levels, 1, function(set) {  
    # 'set' refers to a single combination of lows and highs (e.g., low-low or low-high)
    # for each set, construct a formula that corresponds to that combination of the IVs
    
    form_str = dv_str 
    outDF = data.frame(matrix(nrow = 1, ncol=2 + numIntTerms))  # Dataframe to put model estimates into
    names(outDF) = c(iv_names, 'estimate', 'se')
    for (i in 1:length(set)) {
      # get each of the centered variables, and continually append to the formula we're building
      form_str = paste(form_str, iv_names[i], '_', set[i], sep='')
      if (i != length(set)) {
        form_str = paste(form_str, '*', sep='') # if it isn't the last one, we need the * for the interaction
      }
      # Enter the value that the IV is tested at. That way we don't need to use labels like 'high' and 'low', but
      # instead use the actual value that corresponds to low and high
      outDF[[iv_names[i]]] = centered_vars[[iv_names[i]]][[set[[i]]]]$val 
    }
    # Add the covariates back into the model. So each model might now look like 'DV ~ IV1_low * IV2_high + covariate1'
    # And we've added the low and high values already to the dataframe above
    form_str = paste(form_str, covars)
    point_form = formula(form_str)
    
    # Run the model, and grab the estimate and standard error for the intercept.
    # Note: this uses the trick that if you center the IVs at the desired level, then take the intercept
    # estimate and standard error as the point estimate. I'm pretty sure this works with HLM as well, 
    # but there may be some edge cases where this won't work
    coeffs = summary(modelFn(point_form, data=mf_plus, ...))[["coefficients"]][1,1:2] 
    outDF[['estimate']] = coeffs[1]
    outDF[['se']] = coeffs[2]
    return(outDF)
  })
  
  # Bind all models run together, then return
  output = do.call("rbind", outMat)
  return(output)
}

theme_apa <- function(textSize = 16, textFam = 'sans', plot.box = FALSE){
  
  if (Sys.info()["sysname"] != "Windows") {
    windowsFonts <- NULL
  }
  
  if (Sys.info()["sysname"] == "Windows") {
    windowsFonts(RMN=windowsFont("Times New Roman"))
    RMN <- "RMN"
  } else {
    RMN <- "Times New Roman"
  }
  
  out <- theme(
    plot.title=element_text(family=textFam, size=textSize, face="bold", colour="black"),
    axis.title.x=element_text(family=textFam, size=textSize, colour="black"),
    axis.title.y=element_text(family=textFam, size=textSize, angle=90, colour="black"),
    axis.text.x=element_text(family=textFam, size=textSize, colour="black"),
    axis.text.y=element_text(family=textFam, size=textSize, colour="black"),
    axis.ticks=element_line(colour="black"),
    legend.text = element_text(family=textFam, size=textSize, colour="black"),
    legend.title = element_text(family=textFam, size=textSize, colour="black"),
    strip.text.x = element_text(family=textFam, size=textSize, colour="black"))  
  
  if (!plot.box) {
    out <- out + theme(panel.background = element_rect(fill = "white", 
                                                       colour = "black"), panel.border = element_rect(fill = NA, 
                                                                                                      colour = "white"), axis.line = element_line()) 
  } else {
    out <- out + theme(panel.background = element_rect(fill = "white", 
                                                       colour = "white"), panel.border = element_rect(fill = NA, 
                                                                                                      colour = "grey50")) 
  }
  return(out)
  
}

theme_apa_bold <- function(textSize = 16, textFam = 'sans', plot.box = FALSE){
  
  if (Sys.info()["sysname"] != "Windows") {
    windowsFonts <- NULL
  }
  
  if (Sys.info()["sysname"] == "Windows") {
    windowsFonts(RMN=windowsFont("Times New Roman"))
    RMN <- "RMN"
  } else {
    RMN <- "Times New Roman"
  }
  
  out <- theme(
    plot.title=element_text(family=textFam, size=textSize, face="bold", colour="black"),
    axis.title.x=element_text(family=textFam, size=textSize, face="bold", colour="black"),
    axis.title.y=element_text(family=textFam, size=textSize, face="bold", angle=90, colour="black"),
    axis.text.x=element_text(family=textFam, size=textSize, face="bold", colour="black"),
    axis.text.y=element_text(family=textFam, size=textSize, face="bold", colour="black"),
    axis.ticks=element_line(colour="black"),
    legend.text = element_text(family=textFam, size=textSize, face="bold", colour="black"),
    legend.title = element_text(family=textFam, size=textSize, face="bold", colour="black"),
    strip.text.x = element_text(family=textFam, size=textSize, face="bold", colour="black"))  
  
  if (!plot.box) {
    out <- out + theme(panel.background = element_rect(fill = "white", 
                                                       colour = "black"), panel.border = element_rect(fill = NA, 
                                                                                                      colour = "white"), axis.line = element_line()) 
  } else {
    out <- out + theme(panel.background = element_rect(fill = "white", 
                                                       colour = "white"), panel.border = element_rect(fill = NA, 
                                                                                                      colour = "grey50")) 
  }
  return(out)
  
}

probeInteraction = function(formula, data, modelFn, ...) {
  # Takes a formula with an interaction term and generates simple slopes 
  # Currently only dichotomous and continuous variables supported (no categorical).
  # Dichotomous variables are probed at each level, and continuous values are probed at +/- 1 standard deviation
  # (more flexibility to be supported in the future. Maybe). 
  # Report errors to: Paul Stillman (paul.e.stillman@gmail.com)
  #
  # Args: 
  #   formula: formula specification that you want to generate point estimates for.
  #            IMPORTANT -- all interaction terms must precede any covariates (all *'s must be to the left of +'s).
  #            Not currently robust to lots of weird shit in the formula. 
  #   data: dataset containing the variables defined in formula. Any categorical variables 
  #         must be dummy or effects coded (for now). Only 2 levels currently supported
  #   modelFn: the function used to model the formula (e.g., lm, glm, lmer, glmer, etc.).
  #   ...: extra values passed to the modelFn. For exampel, when passing glm, one might also want to specify
  #        family=binomial
  #
  # Returns:
  #   A dataframe with estimated value and standard error with every combination of the various levels of the IVs  
  #
  # Example uses:
  #   probeInteraction(continuousDV ~ IV1 * IV2, myData, lm)
  #   probeInteraction(dichotomousDV ~ IV1 * IV2 * IV3 + covariate1 + covariate2, myData, glm, family = binomial)
  #   probeInteraction(continuousDV ~ IV1 * IV2 + (1|groupingVar), myData, lmer)
  #   probeInteraction(mpg ~ vs * hp + carb, mtcars, lm)
  
  require(formula.tools)
  require(stringr)
  if (class(formula) == 'character') formula = formula(formula)  # If provided with a string, convert to formula
  
  mf = get_all_vars(formula=formula, data=data)  # Extract the Variabless
  
  # Currently assumes all interaction terms will be to the left of the +, so 
  # split the formula string where the plus is (if there is a plus)
  form_char = as.character(formula)
  plusStart = str_locate(form_char, '\\+') 
  
  if(!is.na(plusStart[1])) {
    covars = str_sub(form_char, start = plusStart[1]) 
  } else {
    covars = ""
  }
  
  # Isolates just the interaction variables
  numIntTerms = 1 + str_count(form_char, '\\*')
  df_IVs = mf[, -1]  #chops off the first, which is the DV.
  df_x = df_IVs[, 1] # Retain X (which won't change)
  df_IVs = df_IVs[, 2:numIntTerms, drop = F]  #just keep the interaction terms (need to keep this as a df)
  
  # Center all IVs but the first at different levels. 
  # Returns a list of lists, which contain both the centered variables,
  # and the value around which they are centered for each IV
  centered_vars = apply(df_IVs, 2, function(iv) {
    if(length(unique(iv[!is.na(iv)])) == 2) {
      # IV is dichotomous
      vals = sort(unique(iv[!is.na(iv)]))
      high_point = vals[2]
      low_point = vals[1]
      dichotomous = TRUE
    }
    else {
      # not dichotmous, center and whatever
      high_point = mean(iv, na.rm = TRUE) + sd(iv, na.rm = TRUE)
      low_point = mean(iv, na.rm = TRUE) - sd(iv, na.rm= TRUE)
      dichotomous = FALSE
    }
    centered_low = iv - low_point
    centered_high = iv - high_point
    
    low = list()
    low$var = centered_low
    low$val = low_point
    high = list()
    high$var = centered_high
    high$val = high_point
    combo = list(high=high, low=low)
  })
  
  # Add the centered variables to a new model data frame
  mf_plus = mf
  iv_names = names(df_IVs)
  for (i in 1:length(centered_vars)) {
    # Add in the new centered variables to the dataframe:
    mf_plus[[paste(iv_names[i], 'high', sep='_')]] = centered_vars[[i]]$high$var
    mf_plus[[paste(iv_names[i], 'low', sep='_')]] = centered_vars[[i]]$low$var
  }
  
  # Get all the permutations of low and high
  tmp = list()
  tmp$first = 'low'
  tmp$second = 'high'
  levList = rep(list(tmp), length(centered_vars))
  levels = expand.grid(levList)
  
  # Run each model. First sequentially constructs the formulas to be used
  dv_str = paste(all.vars(formula)[1], '~', all.vars(formula)[2], '* ', sep=' ')  # starts with the DV and X
  outMat = apply(levels, 1, function(set) {  
    # 'set' refers to a single combination of lows and highs (e.g., low-low or low-high)
    # for each set, construct a formula that corresponds to that combination of the IVs
    
    form_str = dv_str 
    # slightly different outputs for lmer and lm, so we need to figure out which one we are? Better way to do this but oh well
    if(isTRUE(all.equal(modelFn, lm)) ||  isTRUE(all.equal(modelFn, glmer))) {
      # it's the lm function or the glmer function (no 'df' column in coefficients)
      outDF = data.frame(matrix(nrow = 1, ncol= numIntTerms - 1 + 4))  # Dataframe to put model estimates into
      names(outDF) = c(iv_names, 'estimate', 'se', 't', 'p')
    } else {
      #it's lmer
      outDF = data.frame(matrix(nrow = 1, ncol= numIntTerms - 1 + 5))  # Dataframe to put model estimates into
      names(outDF) = c(iv_names, 'estimate', 'se', 'df', 't', 'p')
    }
    
    for (i in 1:length(set)) {
      # get each of the centered variables, and continually append to the formula we're building
      # uncomment form_str = paste(form_str, iv_names[i], '_', set[i], sep='')
      form_str = paste(form_str, iv_names[[i]], '_', set[[i]], sep='') # for debugging
      if (i != length(set)) {
        form_str = paste(form_str, ' * ', sep='') # if it isn't the last one, we need the * for the interaction
      }
      # Enter the value that the IV is tested at. That way we don't need to use labels like 'high' and 'low', but
      # instead use the actual value that corresponds to low and high
      outDF[[iv_names[i]]] = centered_vars[[iv_names[i]]][[set[[i]]]]$val 
    }
    # Add the covariates back into the model. So each model might now look like 'DV ~ IV1_low * IV2_high + covariate1'
    # And we've added the low and high values already to the dataframe above
    form_str = paste(form_str, covars)
    point_form = formula(form_str)

    # Run the model, and grab the estimate and standard error for the intercept.
    # Note: this uses the trick that if you center the IVs at the desired level, then take the intercept
    # estimate and standard error as the point estimate. I'm pretty sure this works with HLM as well, 
    # but there may be some edge cases where this won't work
    coeffs = coefficients(summary(modelFn(point_form, data=mf_plus, ...)))
    # coeffs = coefficients(summary(modelFn(point_form, data=mf_plus))) # For debugging
    x_effect = coeffs[2, ]
    outDF[, -c(1:numIntTerms-1)] = x_effect # A little ugly, but put the output into the outDF
    return(outDF)
  })
  
  # Bind all models run together, then return
  output = do.call("rbind", outMat)
  return(output)
}

estimate_traj = function(formula, data, modelFn, ..., control_vars = NULL) {
  # Estimates each 101 X and Y values from mouse-tracker output and a formula specifying how to estimate the points
  # Takes the formula and models each X and Y value from that formula
  # Then takes the resulting coefficients and generates predicted values for high and low values of all predictors
  # For dichotomous predictors, just estimates them at 0 and 1, for continuous variables, estimates at +/- 1 std deviation
  # Currently does not support controling for any variables (all variables given will be )
  # NOTE: This function is still experimental and is not guaranteed to work nicely with all data and formulas, use at your own risk
  # The present function was designed with interaction terms in mind, but may work even without them
  # Report errors to: Paul Stillman (paul.e.stillman@gmail.com)
  #
  # Args: 
  #   formula: formula specification that you want to generate the XY estimates for.
  #            The value given on the left-hand-side is ignored (the DV), and can be any variable in the dataframe
  #            Not currently robust to lots of weird stuff in the formula. 
  #   data: dataset containing the variables defined in formula. Any categorical variables 
  #         must be dummy or effects coded (for now). Only 2 levels currently supported
  #         as this is primarily a mouse-tracker function, assumes the variables X_1 to X_101 and Y_1 to Y_101 exist
  #   modelFn: the function used to model the formula (e.g., lm, glm, lmer, glmer, etc.).
  #   ...: extra values passed to the modelFn. For exampel, when passing glm, one might also want to specify
  #        family=binomial
  #
  # Returns:
  #   A dataframe with estimated values for each point (1-101) for x and y for each combination of high and low of the predictor variables  
  #
  # Example uses:
  #   estimatePoints(continuousDV ~ IV1 * IV2, myData, lm)
  #   estimatePoints(dichotomousDV ~ IV1 * IV2 * IV3 + covariate1 + covariate2, myData, glm, family = binomial)
  #   estimatePoints(continuousDV ~ IV1 * IV2 + (1|groupingVar), myData, lmer)
  #   estimatePoints(mpg ~ vs * hp + carb, mtcars, lm)
  
  require(formula.tools)
  require(stringr)
  require(tidyverse)
  if (class(formula) == 'character') formula <- formula(formula)  # If provided with a string, convert to formula
  
  formula_rhs <- formula[[3]]
  xys <- c(str_c("X_",1:101), str_c("Y_", 1:101))
  
  coefs <- map(xys, function(dv) { # model each X and Y value using the given formula (takes a while)
    form_call <- call('~', as.name(dv), formula_rhs)
    model_coefs <- modelFn(form_call, data = data, ...) %>% summary() %>% coefficients()
    # model_coefs <- modelFn(form_call, data = data) %>% summary() %>% coefficients()
    model_coefs[, 'Estimate']
  })
  coefs_mat <- do.call(rbind, coefs) # bind them together
  rownames(coefs_mat) <- xys 
  predictors <- colnames(coefs_mat)[-1] # get the predictors (1 = intercept)
  
  preds_noInt <- predictors[!str_detect(predictors, ":")] # remove interaction terms
  # The below gets the low and high value of each predictor
  # these will be used to get the point estimates
  # for dichotomous, low and high just correspond to the two options
  # for continuous, low = -1 std from mean, high = +1 std from mean
  preds_vals <- map(preds_noInt, function(iv_name) {
    iv <- data[[iv_name]]
    if (length(unique(iv[!is.na(iv)])) == 2) {
      # IV is dichotomous
      vals = sort(unique(iv[!is.na(iv)]))
      return(vals)
    } else {
      # IV is continuous 
      # probe at +/- 1 std deviation
      high_point = mean(iv, na.rm = TRUE) + sd(iv, na.rm = TRUE)
      low_point = mean(iv, na.rm = TRUE) - sd(iv, na.rm = TRUE)
      return(c(low_point, high_point))
    }
  })
  names(preds_vals) <- preds_noInt
  all_pred_vals <- expand.grid(preds_vals) # all values of the IVs
  
  # The below gets the labels for output (currently very messy, may improve in the future)
  pred_labels <- rep(list(c("low", "high")), length(preds_noInt))
  names(pred_labels) <- preds_noInt
  pred_labels_all <- pred_labels %>% 
    expand.grid(stringsAsFactors = FALSE) %>% 
    apply(1, function(row) str_c(row, preds_noInt) %>% paste(collapse = '_'))

  # The following is needed to build the interaction terms 
  test_vals <- apply(all_pred_vals, 1, FUN = function(pred_vals) {
    map_dbl(predictors, function(pred) {
      if (!str_detect(pred, ":")) {
        # not an interaction term
        return(pred_vals[pred]) # return the predictor value
      } else {
        # interaction term, need to multply the constituent
        int_preds = (str_split(pred, ":"))[[1]] 
        int_preds = map_dbl(int_preds, function(name) pred_vals[name])
        return(prod(int_preds))
      }
    })
  })
  
  colnames(test_vals) <- pred_labels_all 
  test_vals <- as_tibble(test_vals)
  
  # now we can evaluate all the points (I probably could rewrite this with broom)
  all_points <- map(test_vals, function(vals) { 
    vals_withIntercept <- c(1, vals)
    point_estimates <- apply(coefs_mat, 1, function(point_coef) {
      vals_withIntercept %*% point_coef # dot product gives us the estimated point
    })
    return_list = list()
    return_list$x <- point_estimates[1:101]
    return_list$y <- point_estimates[102:202]
    return_list
  })
  
  # The below puts the output into a more user-friendly tibble, rather than lists of lists
  all_points <- all_points %>% transpose()
  xs <- map(seq_along(all_points$x), function(index) {
    set <- all_points$x[[index]]
    x_tibble <- tibble(x = set, condition = names(all_points$x)[index], point = 1:101)
  })
  ys <- map(seq_along(all_points$y), function(index) {
    set <- all_points$y[[index]]
    x_tibble <- tibble(y = set, condition = names(all_points$y)[index], point = 1:101)
  })
  xs_all <- do.call(rbind, xs)
  ys_all <- do.call(rbind, ys)
  output <- left_join(xs_all, ys_all, by = c("condition", "point")) %>%
    select(condition, point, x, y)
  output
}
