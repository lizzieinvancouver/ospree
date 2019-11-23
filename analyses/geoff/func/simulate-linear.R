## Generalized simulation function written by Geoffrey Legault (10 October 2019) ##
##
## Description TBD
##
## Currently able to simulate normal, Poisson family
##
simulate.linear <- function(model.parameters,
                           model.random,
                           env,
                           response.distribution,
                           n.species,
                           n.observations){
    # Look for interactions
    interact.regex <- grep(pattern = c(".X."), x = names(model.parameters)) # interaction terms
    if(length(interact.regex) == 0){ # If none,
        # Create samples of environmental variables
        env.samples <- sapply(env, FUN = function(X){
            sample(X, size = n.species * n.observations, replace = TRUE)})
        # Create model matrix
        mm <- model.matrix(~env.samples)
    } else{ # If there are interactions,
        # Create initial samples of environmental variables
        env.samples <- sapply(env, FUN = function(X){
            sample(X, size = n.species * n.observations, replace = TRUE)})
        # Determine which environmental variables interact
        names.temp <- names(model.parameters)[interact.regex]
        names.temp <- gsub(".X.", "|", names.temp) # interactions marked by ".X"
        names.temp <- gsub(".coef", "", names.temp) # remove text to align with colnames
        env.pairs <- sapply(1:length(interact.regex), FUN = function(X){
            grep(pattern = names.temp[X], x = colnames(env.samples))
        })
        # Add these interactions (product) to env.samples        
        env.interactions <- sapply(1:ncol(env.pairs), FUN = function(X){
            apply(env.samples[, env.pairs[, X]], MARGIN = 1, FUN = prod)
        })
        env.samples2 <- cbind(env.samples, env.interactions)
        # Create model matrix
        mm <- model.matrix(~env.samples2)
    }
    # Create parameter matrix
    parameters.temp <- matrix(unlist(model.parameters), ncol = length(model.parameters), nrow = n.species * n.observations, byrow = TRUE)
    # Which parameters are random?
    random.regex <- grep(pattern = paste(model.random, collapse = "|"), x = names(model.parameters))
    # Generate random parameters (by species)
    for(i in 1:length(random.regex)){
        parameters.temp[, i] <- sapply(1:n.species, FUN = function(X){
            rep(rnorm(n = 1, mean = model.parameters[[random.regex[i]]], sd = 2), n.observations)})}
    # Calculate response
    if(response.distribution %in% c("rnorm")){
        response <- sapply(1:nrow(env.samples), FUN = function(X){
            rnorm(n = 1, mean = mm[X, ] %*% parameters.temp[X, ], sd = .9)})
    }
    if(response.distribution %in% c("rpois")){
        parameters.temp <- log(parameters.temp)
        response <- sapply(1:nrow(env.samples), FUN = function(X){
            rpois(n = 1, lambda = exp(mm[X, ] %*% parameters.temp[X, ]))})
    }   
    # Bring everything together
    cbind(data.frame(Species = as.vector(sapply(1:n.species, FUN = function(X) rep(X, n.observations))),
                     Response = response),
          env.samples,
          Intercept = parameters.temp[, 1])
}
