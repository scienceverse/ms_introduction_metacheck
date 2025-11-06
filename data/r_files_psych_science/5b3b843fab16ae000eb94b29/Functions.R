# Functions for analyzing Bayesian hierarchical modeling (Oberauer, 2017)
# of the two-paramater mixture model (Zhang & Luck, 2008)

# -----------------------------------------------------------------------

# Paul Bays' function for transforming K to SD (in radians)
k2sd <- function (K) {
  S <- matrix(0,1,length(K))
  for (j in 1:length(K)) {
    if (K[j]==0) S[j] = Inf
    if (is.infinite(K[j])) S[j] = 0    
    if (K[j] >= 0 & !is.infinite(K[j])) {
      S[j] = sqrt(-2*log(besselI(K[j],1)/besselI(K[j],0)));    
    }
  }
  S
}

# -----------------------------------------------------------------------

HDIofMCMC = function( sampleVec , credMass=0.95 ) {
  # Computes highest density interval from a sample of representative values,
  #   estimated as shortest credible interval.
  # Arguments:
  #   (1) sampleVec is a vector of representative values from a probability distribution.
  #   (2) credMass is a scalar between 0 and 1, indicating the mass within the credible
  #       interval that is to be estimated.
  # Value:
  #   HDIlim is a vector containing the limits of the HDI
  sortedPts = sort( sampleVec )
  ciIdxInc = floor( credMass * length( sortedPts ) )
  nCIs = length( sortedPts ) - ciIdxInc   #number of points excluded
  ciWidth = rep( 0 , nCIs )
  for ( i in 1:nCIs ) {
    ciWidth[ i ] = sortedPts[ i + ciIdxInc ] - sortedPts[ i ]  #computes width of CI on x axis for every possible starting point (and corresponding end point 95% of data later) 
  }
  HDImin = sortedPts[ which.min( ciWidth ) ]   #which.min returns index of the first minimum of the argument vector: selects minimum width for CI
  HDImax = sortedPts[ which.min( ciWidth ) + ciIdxInc ]
  HDIlim = c( HDImin , HDImax )
  return( HDIlim )
}

# -----------------------------------------------------------------------

posterior_summary_table = function(posterior_matrix,parameter_names,parameter_new_names) {
  
  # The following function recieves the following inputs:
  # (1) a posterior matrix with chain iterations as rows and parameters as columns
  # (2) the parameters from the posterior matrix to be checked
  # (3) new parameter names for display in the descriptive summary 
  # and returns posterior summary (mean, median, 95% HDIs) of the requried parameters
  
  # Build posterior summary data frame
  posterior_summary_table = as.data.frame(matrix(nrow=length(parameter_names), ncol=4))
  rownames(posterior_summary_table) = as.character(parameter_new_names)
  colnames(posterior_summary_table) = c("Mean", "Median", "95%HDI_Low", "95%HDI_high")
  for (i in 1:length(parameter_names)) {
    posterior_summary_table[i,1] = mean(posterior_matrix[,parameter_names[i]])
    posterior_summary_table[i,2] = median(posterior_matrix[,parameter_names[i]])
    posterior_summary_table[i,3:4] = HDIofMCMC(posterior_matrix[,parameter_names[i]])
  } # end for
  
  return(posterior_summary_table)
  
} # end function

# -----------------------------------------------------------------------

SD_PG_conversion_mat = function(posterior_matrix,column_names_convert_SD,column_names_convert_PG) {
  
  # The following function recieves a posterior matrix with chain iterations
  # as rows and parameters as columns, and the column names for which 
  # conversion to SD and PG is requried, and returns the same full posterior 
  # matrix, with conversion of kappa values to degrees, and PM (prob. in 
  # memory) values to PG (prob. to guess)
  
  # Find column numbers for SD and PG
  column_num_SD = which(colnames(posterior_matrix) %in% grep(column_names_convert_SD, colnames(posterior_matrix), value=TRUE))
  column_num_PG = c(which(colnames(posterior_matrix) %in% grep(column_names_convert_PG[1], colnames(posterior_matrix), value=TRUE)),
                    which(colnames(posterior_matrix) %in% grep(column_names_convert_PG[2], colnames(posterior_matrix), value=TRUE)))
  
  # Turn all kappa values to degrees and PM to PG
  SD_PG_conversion_mat = posterior_matrix
  SD_PG_conversion_mat[,column_num_SD] = k2sd(SD_PG_conversion_mat[,column_num_SD]) * 180 / pi
  SD_PG_conversion_mat[,c(column_num_PG)] = 1 - SD_PG_conversion_mat[,c(column_num_PG)]
  
  return(SD_PG_conversion_mat)
  
} # end function

# -----------------------------------------------------------------------

create_violin_figure = function(data_all_conditions) {
  
  # The following function creates violin figures with 95% HDIs 
  # out of the posterior iterations for all conditions inserted 
  # into the data.frame.
  
  # Prepare HDIs and Mean
  data_summary <- function(x) {
    m <- mean(x)
    ymin <- HDIofMCMC(x)[1]
    ymax <- HDIofMCMC(x)[2]
    return(c(y=m,ymin=ymin,ymax=ymax))
  }
  
  create_violin_figure <- ggplot(data_all_conditions, 
                                 aes(x=Condition, y=SD, color=Condition)) + 
    geom_violin(trim=FALSE) + stat_summary(fun.data = data_summary,
                                           geom="pointrange", color="black") + 
    labs(y = "Estimated mean precision (degrees)") + 
    theme(text = element_text(size = 14, family = "Helvetica"))
  
  return(create_violin_figure)
  
} # funtion

# -----------------------------------------------------------------------

create_non_ggplot_density = function(list_of_data) {
  
  # the following function computes density plots without
  # using ggplot (to reduce the size of the plot)
  
  plot.multi.dens <- function(s)
  {
    junk.x = NULL
    junk.y = NULL
    for(i in 1:length(s)) {
      junk.x = c(junk.x, density(s[[i]])$x)
      junk.y = c(junk.y, density(s[[i]])$y)
    }
    xr <- c(15,35)#range(junk.x)
    yr <- range(junk.y)
    plot(density(s[[1]]), xlim = xr, ylim = yr, main = "")
    for(i in 1:length(s)) {
      lines(density(s[[i]]), xlim = xr, ylim = yr, col = i)
    }
  }
  
  # Prepare HDIs and Mean
  HDIs_Mean = data.frame(matrix(nrow = length(list_of_data), ncol = 3))
  for (i in 1:length(list_of_data)){
    HDIs_Mean[i,1:2] = HDIofMCMC(as.data.frame(list_of_data)[,i])
    HDIs_Mean[i,3] = mean(as.data.frame(list_of_data)[,i])
  }
  
  # The input of the following function MUST be a numeric list
  plot.multi.dens(list_of_data)
  segments(x0=HDIs_Mean[1,1],y0=0.01,x1=HDIs_Mean[1,2],y1=0.01)
  segments(x0=HDIs_Mean[2,1],y0=0.01,x1=HDIs_Mean[2,2],y1=0.01)
  points(x=HDIs_Mean[1,3],y=0.01, bg = "white")
  points(x=HDIs_Mean[2,3],y=0.01, bg = "white")
  
}