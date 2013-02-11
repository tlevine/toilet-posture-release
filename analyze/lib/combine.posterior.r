# Combine relative.loglikelihood distribution coefficients for a 2^3 design
# The argument "relative.loglikelihood" is a 1000 * 8 matrix.
# The columns might be
# [1] "(Intercept)"         "dirty"               "defecate"            "male"
# [5] "dirty:defecate"      "dirty:male"          "defecate:male"       "dirty:defecate:male"
combine.relative.loglikelihood.3 <- function(MCMCglmm.categorical.model) {

    # Add the random effect
    m <- MCMCglmm.categorical.model
    relative.loglikelihood <- m$Sol

    # Start computing absolute loglikelihoods instead of relative loglikelihoods
    absolute.loglikelihood <- relative.loglikelihood

    # Add the intercept
    absolute.loglikelihood[,-1] <- absolute.loglikelihood[,-1] + relative.loglikelihood[,rep(1, 7)] 

    # Add the main effects
    absolute.loglikelihood[,5:7] <- absolute.loglikelihood[,5:7] +
        relative.loglikelihood[,c(2,2,3)] + relative.loglikelihood[,c(3,4,4)] 
    absolute.loglikelihood[,8] <- absolute.loglikelihood[,8] +
        rowSums(relative.loglikelihood[,2:4])

    # Add the second-order interactions
    absolute.loglikelihood[,8] <- absolute.loglikelihood[,8] +
        rowSums(relative.loglikelihood[,5:7])

    # Compute probabilities
    absolute.likelihood <- exp(absolute.loglikelihood)
    absolute.likelihood / (absolute.likelihood + 1)
}


# Combine relative.loglikelihood distribution coefficients for a 2^4 design
# colnames(posturetype.all.model$Sol)
# [1] "(Intercept)"                "dirty"                      "public"                     "defecate"                  
# [5] "male"                       "dirty:public"               "dirty:defecate"             "public:defecate"           
# [9] "dirty:male"                 "public:male"                "defecate:male"              "dirty:public:defecate"     
# [13] "dirty:public:male"          "dirty:defecate:male"        "public:defecate:male"       "dirty:public:defecate:male"
combine.relative.loglikelihood.4 <- function(MCMCglmm.categorical.model) {

    # Add the random effect
    m <- MCMCglmm.categorical.model
    relative.loglikelihood <- m$Sol

    # Start computing absolute loglikelihoods instead of relative loglikelihoods
    absolute.loglikelihood <- relative.loglikelihood

    # Add the intercept
    absolute.loglikelihood[,-1] <- absolute.loglikelihood[,-1] + relative.loglikelihood[,rep(1, 15)] 

    # Add the main effects
    absolute.loglikelihood[,6:15] <- absolute.loglikelihood[,6:15] +
        relative.loglikelihood[,c(2,2,3,2,3,4, 2,2,2,3)] + relative.loglikelihood[,c(3,4,4,5,5,5, 3,3,4,4)] 

    absolute.loglikelihood[,12:15] <- absolute.loglikelihood[,12:15] +
        relative.loglikelihood[,c(4,5,5,5)]

    absolute.loglikelihood[,16] <- absolute.loglikelihood[,16] +
        rowSums(relative.loglikelihood[,2:5])

    # Add the second-order interactions
    absolute.loglikelihood[,12:15] <- absolute.loglikelihood[,12:15] +
        relative.loglikelihood[,c(6,6,7,8)] + relative.loglikelihood[,c(8,10,11,11)] + relative.loglikelihood[,c(7,9,9,10)] 

    absolute.loglikelihood[,16] <- absolute.loglikelihood[,16] +
        rowSums(relative.loglikelihood[,12:15])

    # Add the third-order interactions
    absolute.loglikelihood[,16] <- absolute.loglikelihood[,16] +
        rowSums(relative.loglikelihood[,12:15])

    # Compute probabilities
    absolute.likelihood <- exp(absolute.loglikelihood)
    absolute.likelihood / (absolute.likelihood + 1)
}

# colnames(m$Sol)
# [1]  "(Intercept)"                      "unspecified"                     
# [3]  "dirty"                            "defecate"                                   
# [5]  "male"                             "public"                                     
# [7]  "unspecified:defecate"             "dirty:defecate"                  
# [9]  "unspecified:male"                 "dirty:male"                      
# [11] "defecate:male"                    "unspecified:public"              
# [13] "dirty:public"                     "defecate:public"                            
# [15] "male:public"                      "unspecified:defecate:male"       
# [17] "dirty:defecate:male"              "unspecified:defecate:public"     
# [19] "dirty:defecate:public"            "unspecified:male:public"         
# [21] "dirty:male:public"                "defecate:male:public"                       
# [23] "unspecified:defecate:male:public" "dirty:defecate:male:public"      

combine.relative.loglikelihood.3.2.2.2 <- function(MCMCglmm.categorical.model) {

    # Add the random effect
    m <- MCMCglmm.categorical.model
    relative.loglikelihood <- m$Sol

    # Start computing absolute loglikelihoods instead of relative loglikelihoods
    absolute.loglikelihood <- relative.loglikelihood

    # Add the intercept
    absolute.loglikelihood[,-1] <- absolute.loglikelihood[,-1] + relative.loglikelihood[,rep(1, 23)] 

#   # Add the main effects
#   absolute.loglikelihood[,7:23] <- absolute.loglikelihood[,7:23] +
#       absolute.loglikelihood[,c(2,3,)] + absolute.loglikelihood[,c()]




#       absolute.loglikelihood[,c()] +
#       absolute.loglikelihood[,c()] +
#       absolute.loglikelihood[,c()] +
#       absolute.loglikelihood[,c()] +

}


combine.relative.loglikelihood.logodds <- function(MCMCglmm.categorical.model) {

    # Add the random effect
    m <- MCMCglmm.categorical.model
    relative.loglikelihood <- m$Sol

    # Start computing absolute loglikelihoods instead of relative loglikelihoods
    absolute.loglikelihood <- relative.loglikelihood * 0

    # Add the intercept
    k <- ncol(relative.loglikelihood)
    absolute.loglikelihood <- absolute.loglikelihood + relative.loglikelihood[,rep(1, k)] 

    # Add everything else
    for (colname in colnames(absolute.loglikelihood)[-1]) {

        # Get the associated variables
        main.effects <- str_split(colname, ":")[[1]]

        if (length(main.effects) >= 2) {
            second.order <- apply(combn(main.effects,2), 2, function(var.names) { paste(var.names, collapse = ':' ) })
        } else {
            second.order <- c()
        }

        if (length(main.effects) >= 3) {
            third.order <- apply(combn(main.effects,3), 2, function(var.names) { paste(var.names, collapse = ':' ) })
        } else {
            third.order <- c()
        }

        # Add the variables
        for (effect in c(main.effects, second.order, third.order)) {
            if (!(effect %in% colnames(absolute.loglikelihood))) {
                print('Oops 1')
            }
            if (!(effect %in% colnames(relative.loglikelihood))) {
                print('Oops 2')
            }
            absolute.loglikelihood[,colname] <- absolute.loglikelihood[,colname] + relative.loglikelihood[,effect]
        }
    }

    # Compute probabilities
    absolute.loglikelihood
}

combine.relative.loglikelihood <- function(MCMCglmm.categorical.model) {
  a <- exp(combine.relative.loglikelihood.logodds(MCMCglmm.categorical.model))
  a / (a + 1)
}
