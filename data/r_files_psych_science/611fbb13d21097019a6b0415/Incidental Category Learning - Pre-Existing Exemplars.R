##################################
############ PACKAGES ############
##################################

library(plyr)
library(tidyr)
library(ggplot2)
library(gridExtra)

##################################
########## DESCRIPTION ###########
##################################

# The purpose of this script is to implement a mechanism for the ready to learn effect, in 
# which incidental exposure to category exemplars fosters subsequent category learning.
# The mechanism implemented in this script is that the category exemplars observed during
# incidental exposure are stored in memory. When a learner begins learning the categories
# under supervision with feedback, the learned associations between exemplars and category labels
# are generalized to other exemplars stored in memory, in proportion to their similarity. Because
# exemplars from the same category are more similar than exemplars in different categories,
# this mechanism allows a category label learned for one exemplar to "generalize" to exemplars stored in memory 
# from the same category. 

# This idea is implemented by simulating two conditions: (1) Incidental exposure to categories,
# in which a SUSTAIN model is populated with a cluster for each exemplar prior to supervised category 
# learning, and (2) Baseline, in which SUSTAIN contains no clusters prior to supervised category learning 
# (as is typically the case for SUSTAIN). 

# This script implements a modified version of SUSTAIN described in Love, Medin & Gureckis 2004
# See the paper for a full description of the original model.

# The original version of SUSTAIN implements a winner-take-all process in which the current 
# stimulus initially activates all clusters, but following lateral inhibition between clusters,
# only the most activated cluster contributes to the response. When the response is correct,
# only the weights between the most activated cluster and the correct response are updated based
# on feedback.

# To implement the idea that learned category labels can generalize to exemplars stored in memory
# from the same category, the script makes the following modifications to SUSTAIN:
# (1) Lateral inhibition is removed, and all clusters contribute to the response in proportion
# to their activation
# (2) Feedback updates weights for all clusters in proportion to their activation

# The steps implemented in this script are:
# (1) Define a function for coding exemplars using one-hot encoding used by SUSTAIN
# (2) Define a series of functions that implement the modified SUSTAIN model
# (3) Define a function that simulates probabilities of correct responses during 
#     supervised category learning
# (4) Simulate probabilities of correct responses during supervised category learning
#     in the two conditions given various values of SUSTAIN's free parameters

##################################
### FUNCTIONS: DATA PROCESSING ###
##################################

# The recode_features function takes the set of features that comprise 
# a stimulus (including the label) and converts it into one-hot encoding. 
# In the output, each feature is a vector containing 0s and a single 1. 
# For each dimension, the length of the vectors for features equals the number 
# of possible feature values for that dimension (e.g., if the dimension "shape" 
# has 2 values, the vectors will be of length 2). The position of the 1 in 
# the vector discriminates between the different feature values of a dimension 
# (e.g., "triangle" = 0 1, and "square" = 1 0).
# Arguments:
# features: Vector of original feature values for each dimension of a stimulus.
#           Make sure that the original feature values for each dimension are 
#           encoded starting at 1 (arbitrary feature codes will not work), because
#           they are used to index the position of the 1 in the one-hot vector.
# feature_variants: Vector containing the number possible features for each dimension 
# Output:
# A list of the new one-hot vectors for each feature of a stimulus.

recode_features <- function(features, feature_variants) {
  output_list <- list()  # Create an empty list
  
  # For each dimension feature...
  for (i in 1:length(features)) {
    
    # make vector of 0's with length feature_variants[i] (num of features in the dimension)
    dim_vector <- rep(0, feature_variants[i])
    
    # set the value of the vector at the position of the feature to 1. 
    # Use the value of the feature in features as an index of the position 
    # where the 1 should go. Note: This works b/c we assume all feature values
    # are numbered consecutively starting at 1. 
    dim_vector[ features[i] ] <- 1
    
    # add dim_vector to the output in the correct position for the dimension
    output_list[[i]] <- dim_vector
  }
  
  return(output_list)
}


# Example: uncomment to run
# recode_features(c( 1, 2, 4), c(3, 3, 4))

####################################
######## FUNCTIONS: SUSTAIN ########
####################################

### PART I: STIMULUS -> RESPONSE ###


# Calculate the distance between the stimulus and a cluster 
# on a dimension.
# Both the feature value of the stimulus on a dimension and the
# position of the cluster on a dimension are vectors (for 
# features of stimuli, these are one-hot encoded vectors, e.g.,
# (0, 1, 0))
# Equation 4 in SUSTAIN paper that calculates Mu_ij
dim_distance <- function(stim_dim, cluster_dim) {
  difference <- abs(stim_dim - cluster_dim)
  distance <- sum(difference)/2
  return(distance)
}

# Calculate the activation of a cluster based on: (1) The distances
# for each dimension between the position of the cluster and the
# feature value of the stimulus, (2) The lambda (i.e., tuning/attention weight)
# for each dimension, and (3) The freely estimated attentional parameter r, which
# modulates the effects of lambdas (large r accentuates lambda differences; small
# r tends to equalize them)
# Equation 5 in SUSTAIN paper that calculates Hact
cluster_activation <- function(distances, lambdas, r) {
  numerator <- sum( (lambdas^r) * exp(-lambdas*distances) )
  denominator <- sum( (lambdas^r) )
  activation <- numerator/denominator
  return(activation)
}

# Find the index of the cluster with the highest activation
find_winning_cluster <- function(activations) {
  winner_index <- which(activations == max(activations))
  return(winner_index[1]) # take the first one in case there are ties
}


# Use the cluster outputs and weights with each response unit to calculate the 
# activation of each response unit. 
# Equation 7 in SUSTAIN paper to calculate Cout
# Note: Equation 7 calculates activation for only queried response unit. However, we
# calculate activation for all response units, and index only the queried unit later.
response_unit_output <- function(cluster_activations, weights) {
  response_unit_outputs <- cluster_activations %*% weights # matrix multiplication
  return(response_unit_outputs)
}

# Calculate the probabilities of each response on the queried dimension based on
# (1) The response unit activation for only responses on the queried dimension, 
# and (2) The freely estimated parameter d, which controls response determinism.
# Equation 8 in SUSTAIN paper that calculates Pr(k)
response_probabilities <- function( queried_unit_outputs, d ) {
  numerators <- exp(d * queried_unit_outputs)
  denominator <- sum(numerators)
  probabilities <- numerators/denominator
  return(probabilities)
}

# Combine all component functions in PART I into "forward_pass" function

# Arguments:
# The current sustain model (free parameters r, beta, d, eta, tau*, cluster positions, lambdas, weights),
# the stimulus on a trial, the dimension(s) queried on a trial, the dimensions present in 
# the stimulus, and an argument that specifies whether learning is unsupervised.

# What function does:
# Identifies indeces of response units corresponding to queried dimension(s)
# Checks if there are 0 clusters. If so, returns empty output for unsupervised, and output that 
# contains only equal probabilities across queried response units for supervised
# If there are clusters, the stimulus activates the clusters based on distances, clusters inhibit 
# each other, the winning cluster activates response units via weights, and response unit activations lead to 
# response probabilities.

# Output:
# Probabilities of each response on queried dimension. 
# For updating sustain (see PART II), output also includes cluster activations, distances, index of 
# winning cluster, and response unit activations.

# Note: Stimulus and cluster positions will be in this general format
# stimulus <- list( c(0,1,0),
# c(0,0,1),
# c(0, 1) )

forward_pass <- function(sustain, stimulus, queried_dimensions, present_dimensions, lateral_inhibition = TRUE) {
  
  # To start, it is useful to identify indeces of response units that correspond to the queried dimension(s)
  # This is because, in both the objects that will contain response unit weights and probabilities, 
  # values corresponding to each response unit are concatenated across dimensions. 
  # For example, the columns of the weights matrix are response units across dimensions. So, it is important 
  # to know which response units correspond to features on the queried dimensions.
  
  # Initialize queried_indeces, which will be a list, in which each element contains the indeces corresponding
  # to a queried dimension 
  queried_indeces <- rep(list(NA), length(queried_dimensions))
  for(i in 1:length(queried_dimensions)) {
    # To find the first index corresponding to a queried dimension:
    # If queried_dimension == 1, then the first index == 1. 
    # If queried dimension > 1: count the number of features on dimensions leading up to 
    # the queried dimension (i.e., sum(lengths(stimulus)[1:(queried_dimension-1)]) ), and then start with the next column
    first_queried_index <- ifelse(queried_dimensions[i] == 1, 1, 
                                  sum(lengths(stimulus)[1:(queried_dimensions[i]-1)]) + 1)
    
    # To find the last index corresponding to a queried dimension:
    # Count the number of features on dimensions leading up to and including the queried dimension features
    last_queried_index <- sum(lengths(stimulus)[1:(queried_dimensions[i])])
    
    # All indeces corresponding to a queried dimension
    dimension_indeces <- first_queried_index:last_queried_index
    
    # Add to vector of queried indeces
    queried_indeces[[i]] <- dimension_indeces
  }
  
  # First, check whether there 0 clusters (which will be the case on the first trial).
  # If there are 0 clusters and learning is supervised, we need to just set the probabilities 
  # of each response for a given queried dimension to be equal (because there are no clusters to activate).
  if(length(sustain$clusters) == 0) {
    
    # Set the probabilities of each response for a given queried dimension to be equal
    probabilities <- rep(NA, sum(lengths(stimulus)))
    for (i in 1:length(queried_dimensions)) {
      dimension_probabilities <- rep(1/lengths(stimulus[queried_dimensions[i]]), lengths(stimulus[queried_dimensions[i]])) 
      probabilities[queried_indeces[[i]]] <- dimension_probabilities
    }
    
    return_list <- list( probabilities = probabilities, Hout = NA, 
                         distances = NA, winner_index = NA, 
                         Cout = NA )
    return(return_list)
    
  }
  
  # Create vector of the subset of lambdas for only the dimensions present in stimulus
  present_lambdas <- sustain$lambdas[present_dimensions]
  
  # Initialize a vector of activations for each cluster
  activations <- rep(NA, length(sustain$clusters))
  
  # Initialize a matrix of distances between each cluster and the stimulus on each dimension.
  # Each row is a cluster, each column is a dimension
  # Note: Distances for all dimensions are calculated first, and then only distances for present
  # dimensions are used to calculate cluster activations.
  cluster_distances <- matrix(,nrow=length(sustain$clusters), ncol=length(stimulus))
  
  # Fill in the cluster_distances matrix, and use it to fill in the activations vector.
  # First, go through each cluster present in SUSTAIN (represented as a list of cluster dimensions)
  for (cluster_index in 1:length(sustain$clusters)) {
    
    # Initialize a distances vector that will be filled in with distances for each dimension
    # (present dimensions indexed later)
    distances <- rep(NA, length(stimulus))
    
    # For each dimension... 
    for (dimension_index in 1:length(stimulus)) {
      # fill in the distance between the stimulus and the current cluster
      distances[dimension_index] <- dim_distance(stimulus[[dimension_index]], 
                                                 sustain$clusters[[cluster_index]][[dimension_index]] )
    }
    
    # use the distances vector to fill in the distances between the stimulus and the current cluster
    cluster_distances[cluster_index,] <- distances
    
    # Fill in the activation of the current cluster using distances and lambdas for only present dimensions
    # (Hact in SUSTAIN paper)
    activations[cluster_index] <- cluster_activation(distances[present_dimensions], present_lambdas, sustain$r)
    
  }
  
  # Identify the index of the winning cluster (i.e., the one with the highest activation)
  winner_index <- find_winning_cluster(activations)
  
  # Calculate the activation of all response units based on the cluster outputs
  response_outputs <- response_unit_output(activations, sustain$weights)
  
  
  # For supervised learning, use parameter d and response_outputs for response units on only the queried dimension(s),
  # to calculate response probabilities. 
  # Note: response_outputs is a matrix where each row is a cluster, and each column is a response unit. 
  # The columns of the matrix are response units *across all dimensions*. Response units columns are ordered by dimension 
  # and feature (e.g., the first column is the response unit corresponding to the first feature on a dimension). 
  # So, we can use queried_indeces defined above to identify the columns of response_outputs 
  # that correspond to the queried dimension(s).
  
  # Initialize a vector of probabilities across all response units
  probabilities <- rep(NA, sum(lengths(stimulus)))
  
  # Fill in the probabilities for response units on only the queried dimension(s)
  for (i in 1:length(queried_dimensions)) {
    queried_outputs <- response_outputs[,queried_indeces[[i]]]  # Identify response_outputs only for queried dimension
    queried_probabilities <- response_probabilities(queried_outputs, sustain$d) # Calculate probability of queried response units
    
    probabilities[queried_indeces[[i]]] <- queried_probabilities
  }
  
  # For supervised learning, return winning cluster distances and activations, response
  # probabilities, the index of the winning cluster, and activation of response units.
  # Note to self: originally Hout = outputs[winner_index], but now returning all outputs
  return_list <- list(  distances = cluster_distances[winner_index,], 
                        Hout = activations, 
                        winner_index = winner_index, 
                        Cout = response_outputs,
                        probabilities = probabilities
                       )
  return(return_list)
}


########### TEST EFFECT OF REMOVING LATERAL INHIBITION ##################

# Fake stimuli
stimulus1 <- list(c(1, 0),
                  c(0, 1),
                  c(1, 0))

stimulus2 <- list(c(0, 1),
                  c(1, 0),
                  c(0, 1))

# A fake SUSTAIN model with clusters
sustain_clusters <- list( r = 1.0, beta = 1.0, d = 10.0, eta= 0.5,
                          # Each cluster is a list. The list contains vectors that specify
                          # the position of the cluster on each dimension. 
                          # These positions are updated with learning.
                          clusters = list(
                            # cluster 1
                            list(c(0.9, 0.1),
                                 c(0.1, 0.9),
                                 c(1,   0)),
                            # cluster 2
                            list(c(0.1, 0.9),
                                 c(0.9, 0.1),
                                 c(0,   1))
                          ),
                          # Lambas that specify attention tuning for each dimension
                          lambdas = c(1.5, 1.0, 1.0),
                          
                          # Weights that specify the weight between each cluster and each
                          # response unit. Rows are clusters, response units are columns
                          # In this example, 2 clusters, 8 response units (summed number
                          # of features across dimensions - i.e., 3 + 3 + 2)
                          weights = matrix( c(0, 0, 0, 0, .8, .2, 
                                              0, 0, 0, 0, .2, .8),
                                            nrow = 2, ncol = 6, byrow=TRUE)
)

### PART II: UPDATE AFTER FEEDBACK ###


# When the response is correct, adjust the position of the winning
# cluster to be closer to the position of the stimulus (modulated
# by eta, the freely estimated learning rate parameter) 
# Equation 12 in SUSTAIN paper
adjust_cluster <- function(cluster, stimulus, eta) {
  
  # Calculate the difference between the feature values of the stimulus 
  # and the position of the cluster for each dimension 
  differences <- mapply('-', stimulus, cluster, SIMPLIFY = FALSE)
  # Multiply each difference by eta
  deltas <- lapply(differences, '*', eta)
  #Set the new position of the winning cluster to be closer to the stimulus
  new_cluster <- mapply('+', cluster, deltas, SIMPLIFY = FALSE)
  
  # e.g. cluster = c(0,1,0, 0,0.25,0.75) 
  return(new_cluster)
}

# Adjust the lambdas for each dimension based on the distances between the
# stimulus and the winning cluster on each dimension. 
# Note: When the response was incorrect, the "winning cluster" is a new cluster
# positioned on the stimulus. Thus, all distances will be 0, and all lambdas will
# be updated equally. 
# Equation 13 in SUSTAIN paper

adjust_lambdas <- function(lambdas, distances, eta) {
  new_lambdas <- lambdas + (eta * exp(-lambdas*distances) * (1-lambdas*distances))
  return(new_lambdas)
}

# Adjust the weights between the winning cluster (newly created cluster in case
# of incorrect response) and the response units for the queried dimension based
# on: 
# (1) The difference between the response unit output and the correct response
# conveyed by feedback
# (2) The learning rate, and 
# (3) The output of the winning cluster (so that only weights for the winning cluster
#     are updated) 

adjust_weights <- function(weights, feedback, response_unit_output, cluster_output, eta) {
  new_weights <- weights + (eta * (feedback - response_unit_output) * cluster_output)
  return(new_weights)
}

# Adjust feedback according to humble teacher. 
# Note: Feedback will be the one-hot vector for the queried dimension of
# the stimulus (or, if multiple features are queried, the concatenation of these vectors)
# Equation 9 in SUSTAIN paper

humble_teacher <- function(output, feedback) {
  adjusted_feedback <- rep(NA, length(feedback)) #Initialize a vector of NAs
  for(i in 1:length(feedback)){
    if (feedback[i] == 0) {
      adjusted_feedback[i] <- min(output[i], 0)
    }
    else {
      adjusted_feedback[i] <- max(output[i], 1)
    }
  }
  return(adjusted_feedback)
}

# Combine all component functions in PART II into "update_sustain" function

# Arguments:
# The current sustain model (free parameters r, beta, d, eta, tau*, cluster positions, lambdas, weights),
# the stimulus on a trial, the dimension(s) queried on a trial, the dimensions present in 
# the stimulus, the list output by the forward_pass function ("trial_output"), and an argument that 
# specifies whether learning is unsupervised.

# What function does:
# Identifies the indeces of response units that correspond to the queried dimension(s)
# Identifies whether a new cluster must be made (e.g., if there are no clusters), and if so, makes 
# a new cluster positioned on the stimulus with weights initialized to 0, then re-runs forward_pass
# When no new cluster is formed, adjusts existing clusters/lambdas/weights

# Output:
# The updated sustain model

update_sustain <- function(sustain, stimulus, queried_dimensions, 
                           present_dimensions, trial_output,
                           incorr_form_cluster = TRUE) {
  
  # To start, use the same approach as in forward_pass to make a list, in which each element
  # contains the indeces that correspond to the response units on a queried dimension
  queried_indeces <- rep(list(NA), length(queried_dimensions))
  for(i in 1:length(queried_dimensions)) {
    first_queried_index <- ifelse(queried_dimensions[i] == 1, 1, 
                                  sum(lengths(stimulus)[1:(queried_dimensions[i]-1)]) + 1)
    last_queried_index <- sum(lengths(stimulus)[1:(queried_dimensions[i])])
    dimension_indeces <- first_queried_index:last_queried_index
    queried_indeces[[i]] <- dimension_indeces
  }
  
  # First, check whether conditions for forming a new cluster are met.
  # Initialize form_new_cluster to FALSE
  form_new_cluster = FALSE
  
  # Condition 1: When there are no clusters
  if (length(sustain$clusters)==0) {
    form_new_cluster <- TRUE
  } else {
    # Condition 2: When learning is supervised, and the response was incorrect. 
    # Compares two vectors: the response unit probabilities for each feature on the 
    # queried dimension(s), and the feedback in which the correct feature on the queried dimension(s) == 1,
    # and other features == 0. To compare, checks whether the index of the highest response
    # probability is the same as the index of the correct feature.
    
    # NOTE: Response is determined to be correct or incorrect based on whether the highest probability 
    # response is the same as the correct response. Since these are probabilities, it is conceivable 
    # that an "actual" response would be different. However, feedback & updating is based on probabilities
    # of responses, not "actual" responses.
      accuracy <- rep(NA, length(queried_dimensions))
      # Check accuracy of response for each queried dimension
      for(i in 1:length(accuracy)) {
        # First identify the probabilities for each response unit on the queried dimension, and the
        # correct response unit valies for the queried dimension
        queried_probabilities <- trial_output$probabilities[queried_indeces[[i]]]
        queried_dimension <- stimulus[[queried_dimensions[i]]]
        # Then identify the index of the response unit with the maximum probability, and of the 
        # response that is correct
        max_probability_index <- which(queried_probabilities == max(queried_probabilities))[1]
        correct_index <- which(queried_dimension == 1)
        
        #If indeces are the same, accuracy = 1; else, it is 0
        accuracy[i] <- ifelse(max_probability_index == correct_index, 1, 0)
        
        #print(queried_probabilities)
        #print(max_probability_index)
        #print(correct_index)
        #print(accuracy)
      }
      
      # Form a new cluster if: (1) any of the responses were incorrect and (2) Adding a cluster when incorrect == TRUE
      if(any(accuracy == 0) & incorr_form_cluster) {
        form_new_cluster <- TRUE
      }
    }
  
  # If any condition for forming a new cluster is met, form a new cluster at the position of
  # the stimulus and add it to the sustain model. Also add weights between new cluster and
  # response units. Run forward_pass again with this new cluster included to get activations,
  # weights, etc for this new cluster.
  if (form_new_cluster) {
    # Add stimulus as a new cluster
    sustain$clusters <- c( sustain$clusters, list(stimulus))
    
    # Add new row the the weights matrix (0 prior to running forward_pass again)
    sustain$weights <- as.matrix(rbind(sustain$weights, rep(0, sum(lengths(stimulus))))
    )
    
    # re-run the forward pass now that new cluster is added to get the correct values for the update
    trial_output <- forward_pass(sustain, stimulus, queried_dimensions, present_dimensions)
    
  }
  
  # When no new cluster was formed:
  # Adjust the position of the winning cluster to be closer to the stimulus (equation 12)
  sustain$clusters[[trial_output$winner_index]] <- adjust_cluster(sustain$clusters[[trial_output$winner_index]], 
                                                                  stimulus, sustain$eta)
  
  # Adjust the lambdas based on the dimension distances between the stimulus and 
  # the winning cluster (equation 13) (note: trial_output$distances only contains distances between
  # the stimulus and the winning cluster, not for all clusters, so no indexing needed)
  sustain$lambdas <- adjust_lambdas(sustain$lambdas, trial_output$distances, sustain$eta)
  
  # Adjust the weights between the winning cluster and the response units for the 
  # queried dimension.
  queried_columns <- unlist(queried_indeces)
  
  # Use humble teacher to get the modified feedback (t_zk) from the paper (equation 9)
  # The raw feedback is the stimulus vector(s) for the queried dimension(s). Unlist is used
  # so that if there are multiple queried dimensions, vectors across queried dimensions are
  # concatenated.
  feedback <- humble_teacher(trial_output$Cout[queried_columns], unlist(stimulus[queried_dimensions]))
  
  # Update the weights between the winning cluster and queried response units based on feedback.
  # Note: Weights is a matrix in which clusters are rows, and response units are columns.
  # Hout is a vector in which each element is a cluster.
  
  for(cluster in 1:nrow(sustain$weights)) {
    sustain$weights[cluster, queried_columns] <- adjust_weights(sustain$weights[cluster, queried_columns], 
                                                                feedback, 
                                                                trial_output$Cout[queried_columns], 
                                                                trial_output$Hout[cluster], 
                                                                sustain$eta)
  }
  
  # Original version in which weights were only updated for winning cluster. 
  # Note: added index for Hout (vector) because trial_output now returns it for all clusters
  #sustain$weights[trial_output$winner_index, queried_columns] <- 
  #adjust_weights(sustain$weights[trial_output$winner_index, queried_columns], 
  #feedback, trial_output$Cout[queried_columns], 
  #trial_output$Hout[trial_output$winner_index], sustain$eta)
  
  return(sustain)
  
}

####################################
######## SIMULATE PROB ACC #########
####################################

get_prob_accuracies <- function(stimuli, queried_dimensions, present_dimensions, 
                                sustain, 
                                return_sustain = FALSE, 
                                incorr_form_cluster = TRUE) {
  
  
  prob_accuracies <- rep(NA, length(stimuli))
  sustain_list <- rep(list(NA), length(stimuli))
  
  # For each trial...
  for (i in 1:length(stimuli)) {
    trial_output <- forward_pass(sustain, stimuli[[i]], queried_dimensions[[i]], present_dimensions[[i]])
    
    sustain_list[[i]] <- sustain
    queried_indeces <- c()
    
    # For each queried dimension on the trial, get the indeces of probabilities corresponding to that
    # dimension 
    for(j in 1:length(queried_dimensions[[i]])) {
      first_queried_index <- ifelse(queried_dimensions[[i]][j] == 1, 1, 
                                    sum(lengths(stimuli[[i]])[1:(queried_dimensions[[i]][j]-1)]) + 1)
      last_queried_index <- sum(lengths(stimuli[[i]])[1:(queried_dimensions[[i]][j])])
      dimension_indeces <- first_queried_index:last_queried_index
      queried_indeces <- c(queried_indeces, dimension_indeces)
    }
    
    queried_probabilities <- trial_output$probabilities[queried_indeces]
    queried_dimension <- stimuli[[i]][[queried_dimensions[[i]]]]
    correct_index <- which(queried_dimension == 1)
    prob_accuracies[i] <- queried_probabilities[correct_index]
    
    sustain <- update_sustain(sustain, stimuli[[i]], queried_dimensions[[i]], present_dimensions[[i]], trial_output, 
                              incorr_form_cluster = incorr_form_cluster)
  }
  
  if(return_sustain) {
    return(list(sustain_list, prob_accuracies))
  } else {
    return(prob_accuracies) 
  }
}


########################################
### SIMULATION: CATEGORY VS BASELINE ###
########################################

# Load in exemplars presented during the Category Exposure condition of the Exposure Phase,
# and during the Explicit Phase

setwd("LOCATION OF .CSV FILES")

# Read in exemplars and recode using one-hot encoding
exemplars <- read.csv("exemplars.csv", header = F)

# Convert each exemplar into a list element
exemplars_list <- rep(list(NA), nrow(exemplars))
for(i in 1:nrow(exemplars)) {
  exemplars_list[[i]] <- as.numeric(exemplars[i,])
}
# Recode exemplars to use one-hot endoding
n_featurelab_variants <- rep(2, ncol(exemplars))
exemplars_coded <- lapply(exemplars_list, recode_features, n_featurelab_variants )


# Define make_sustain function, which takes a set of parameter values
# and generates an untrained sustain model that either: (1) Is populated
# with a cluster for each exemplar (if use_exemplars = TRUE), or
# (2) Contains no clusters
# In both cases, attention tuning is the same across all dimensions,
# and there are no weights between clusters and category responses

make_sustain <- function(r, beta, d, eta, tau, use_exemplars = FALSE) {
  if(use_exemplars) {
    sustain_initial <- list( r = r, beta = beta, d = d, eta = eta, tau = tau,
                             clusters = exemplars_coded,
                             lambdas = rep(1, 8), # 8 lambdas - 1 for each exemplar dimension + label dimension
                             weights = matrix(0, 
                                              nrow = length(exemplars_coded), 
                                              ncol = sum(lengths(exemplars_coded[[1]])))
    )
  } else {
    sustain_initial <- list( r = r, beta = beta, d = d, eta = eta, tau = tau,
                           clusters = list(),
                           lambdas = rep(1, 8), # 8 lambdas - 1 for each exemplar dimension + label dimension
                           weights = NULL
    )
  }
  
  return(sustain_initial)
}



# Read in exemplars presented during supervised category learning in
# the Explicit Phase
explicit <- read.csv("explicit trials.csv", header = T)

# Define shuffle_trials function, which shuffles trials within a block
# of the Explicit Phase, as in the experiments
shuffle_trials <- function(input.block) {
  shuffle_block <- input.block[sample(1:nrow(input.block)),]
  return(shuffle_block)
}


# Define a function to simulate probabilities of correct responses in
# each condition, given a set of parameter values
simulate <- function(params) {
  sustain_cat_exposure <- make_sustain(r = params$r, beta = params$beta, d = params$d, eta = params$eta, tau = params$tau,
                                       use_exemplars = TRUE)
  sustain_baseline <- make_sustain(r = params$r, beta = params$beta, d = params$d, eta = params$eta, tau = params$tau)
  
  explicit <- ddply(explicit, .(block), shuffle_trials)
  explicit$block <- NULL
  
  # Convert each exposure exemplar into a vector in a list element
  explicit_list <- rep(list(NA), nrow(explicit))
  for(i in 1:nrow(explicit)) {
    explicit_list[[i]] <- as.numeric(explicit[i,])
  }
  
  # Recode each feature as one-hot vector
  n_featurelab_variants <- rep(2, ncol(explicit))
  explicit_coded <- lapply(explicit_list, recode_features, n_featurelab_variants)
  
  explicit_queried = rep(list(8), nrow(explicit))
  explicit_present = rep(list(1:7), nrow(explicit))
  
  accuracies_baseline <- get_prob_accuracies(explicit_coded, explicit_queried, explicit_present, sustain_baseline, 
                                             incorr_form_cluster = TRUE)
  accuracies_category <- get_prob_accuracies(explicit_coded, explicit_queried, explicit_present, sustain_cat_exposure, 
                                             incorr_form_cluster = FALSE)
  
  accuracies_baseline <- setNames(data.frame(1:nrow(explicit), "baseline", accuracies_baseline), c("Trial", "Condition", "Acc"))
  accuracies_category <- setNames(data.frame(1:nrow(explicit), "category", accuracies_category), c("Trial", "Condition", "Acc"))
  accuracies <- rbind(accuracies_baseline, accuracies_category)
  return(accuracies)
}



# Define a function that simulates probabilities of correct responses given
# 200 instances of the Explicit Phase with trials shuffled within blocks
simulate_params <- function(params) {
  simulate_accuracies <- rdply(200, simulate(params), .id = "simulation")
  return(simulate_accuracies)
}


# Select a set of parameter values to use for simulations
#r_values <- c(1, 3, 5)
d_values <- c(2, 3)
eta_values <- c(.06, .08, .1)

# Make an object where each row is a unique combination of 
# numbers of parameter values
test_params <- setNames(expand_grid(d_values, eta_values),
                        c("d", "eta"))
test_params$beta <- NA
test_params$tau <- NA
test_params$r <- 3

# Simulate probabilities of correct responses in each condition
# for each set of parameter values
simulations <- adply(test_params, 1, simulate_params)

simulations$Conditon <- factor(simulations$Condition, levels = c("category", "baseline"))

# Define a function to plot the probabilities of correct responses
# for each simulation
plot_simulation <- function(simulation) {
  d <- unique(simulation$d)
  eta <- unique(simulation$eta)
  output <- ggplot(simulation, aes(x = Trial, y = Acc)) +
    stat_summary(aes(group = Condition, color = Condition), fun = "mean", geom = "line", show.legend = F) +
    geom_hline(yintercept = .5, linetype = "dashed") +
    scale_y_continuous(name = "Prob Accuracy") +
    scale_color_manual(values = c("gray", "blue")) +
    ggtitle(paste("eta = ", eta, ", d = ", d)) +
    theme_bw() +
    theme(panel.grid = element_blank()) +
    coord_cartesian(ylim = c(.25, 1))
  
  return(output)
}

# Plot probabilities of correct responses in each condition 
# for each set of parameter values
plot_list <- dlply(simulations, .(eta, d), plot_simulation)

grid.arrange(grobs = plot_list, nrow =3, ncol = 2)
