
expit <- function(x){
  exp(x)/(1 + exp(x))
}

gen_params <- function(scen, np){
  
  params_list <- list()
  params_list$scen <- scen
  
  #######################################
  ## Shared information across scenarios
  #######################################

  # --- -- np: total number of participants
  params_list$np <- np
  
  if(scen %in% c(50, 0, 10:14)){
    # --- time.periods: numeric, number of time periods
    params_list$time.periods <- 5
    # --- -- nc: number of clusters
    params_list$nc <- 30
  }else if(scen %in% c(1:4)){
    # --- time.periods: numeric, number of time periods
    params_list$time.periods <- 5
    # --- -- nc: number of clusters
    params_list$nc <- 1
  }else if(scen %in% c(5:9)){
    # --- time.periods: numeric, number of time periods
    params_list$time.periods <- 5
    # --- -- nc: number of clusters
    params_list$nc <- 10
  }else if(scen %in% c(15:19)){
    # --- time.periods: numeric, number of time periods
    params_list$time.periods <- 5
    # --- -- nc: number of clusters
    params_list$nc <- 50
  }else if(scen %in% c(20:24)){
    # --- time.periods: numeric, number of time periods
    params_list$time.periods <- 5
    # --- -- nc: number of clusters
    params_list$nc <- 100
  }else if(scen %in% c(30:34)){
    # --- time.periods: numeric, number of time periods
    params_list$time.periods <- 5
    # --- -- nc: number of clusters
    params_list$nc <- 400
  }
  
  ##################################
  ## Cluster Random effects functions
  ##################################
  
  # --- -- nc: number of clusters
  if(scen %in% c(50, 1, 5, 10, 15, 20, 30)){
    # --- -- No random effects by cluster in these scenarios
    params_list$cluster.rand.eff <- FALSE
  }else if(scen %in% c(0, 2:4, 6:9, 11:14, 16:19, 21:24, 31:34)){
    # --- -- Random effects by cluster in these scenarios
    params_list$cluster.rand.eff <- FALSE
  }
  # --- -- psi.c.fun: function to generate cluster random effects
  params_list$psi.c.fun <- function(nc){
    rnorm(nc, mean = 0, sd = 0.5)
  }

  ###########################################################
  # staggering and number of pre-treatment periods by scenario 
  ########################################################### 
  
  if(scen %in% c(50, 1:4)){
    # --- Number of pre-treatment periods
    params_list$npretrt <- 2
    
    # --- Staggering flag 
    params_list$staggering <- TRUE
    
    # --- Number of forced post treatment periods
    params_list$npostrt <- 0
  }
  
  if(scen %in% c(0, 5:34)){
    # --- Number of pre-treatment periods 
    params_list$npretrt <- 2
    
    # --- Number of forced post treatment periods
    params_list$npostrt <- 0
    
    # --- Staggering flag 
    params_list$staggering <- TRUE
  }
  
  if(scen %in% c(c(0:34), 50)){
    
    # --- -- pc: number of cluster level covariates (excluding intercept)
    params_list$pc <- 2 
    
    # --- -- pc: number of cluster level categories (V1 has 4 categories (multinomial),
    #           ad V2 has 2 (binary))
    params_list$pc_cats <- 4 + 1 
    
    # --- -- pcind: number of cluster level covariates that 
    #               inform individual level covariates (eg average age)
    params_list$pcind <- 3
    
    # --- -- px: number of individual level covariates (including intercept)
    params_list$px <- 1
    
    # --- -- include.switch decides whether patients can switch clusters
    params_list$include.switch <- FALSE
    
    # --- -- prob.switch is the probability of switching cluster 
    params_list$prob.switch <- 0.05
    
    # --- -- type of switch indicates whether it decays with time or is constant
    params_list$type.switch <- 'constant'
    
    #######################
    ## cluster information 
    ######################
    
    # --- -- prop.c: vector of length number of clusters, contains info on 
    #                proportion of members on each cluster at time t0
    params_list$prop.c <- rep(1/params_list$nc, params_list$nc)
    
    # --- -- mc: number of members per cluster
    # params_list$mc <- round(params_list$prop.c[1:(length(params_list$prop.c)-1)] * params_list$np, 0)
    # 
    # # --- -- Ic: array of matrices. For each t, Ic[,,t] 
    # #            contains a matrix with nrow = number of observations and ncol = nc. 
    # #            each matrix indicates if member i belongs to cluster c at time t
    # # due to rounding, we allocate remaining obs to last cluster 
    # if(params_list$np - sum(params_list$mc) > 0 ){
    #   params_list$mc[length(params_list$prop.c)] <- params_list$np - sum(params_list$mc)
    # }
     
    # Compute base size and remainder
    base_size <- floor(params_list$np / params_list$nc)  # Minimum individuals per cluster
    remainder <- params_list$np %% params_list$nc        # Remaining individuals to distribute
    
    # Create vector with base size for all clusters
    params_list$mc <- rep(base_size, params_list$nc)
    
    # Distribute the remainder across the first 'remainder' clusters
    if (remainder > 0) {
      params_list$mc[1:remainder] <- params_list$mc[1:remainder] + 1
    }

    #################
    ## Time Trends
    ##################
    
    # --- -- mu.t: vector of length equal to time.periods, time effects under no treatment
    # --- -- rho.t: vector of length equal to time.periods, time effects under treatment 
    
    params_list$mu.t <- 1:params_list$time.periods
    params_list$rho.t <- 1:params_list$time.periods
    
    ############################################
    ## Coefficients for mutinomial probabilities
    ############################################
    
    gen_theta <- function(pc_cats, pc_ind, ntimes){
      
      nvariables <- pc_cats + pc_ind + 1
      
      # The coefficients that are related to the individual level covariates will be 0
      
      out <- matrix(0, nrow =  nvariables, ncol = ntimes)
      
      time.1 <- matrix(c(seq(from = 1, to = 3, 
                             length.out = pc_cats + 1), 
                       rep(0, pc_ind)), 
                       nrow = nvariables, 
                       ncol = 1, 
                       byrow = FALSE)
      
      k <- 0.95
      out[, 1] <- time.1
      for(i in 2:ntimes){
        out[, i] <- out[, i -1] * k
        k <- k * k
      }
      # negative intercept
      out[1,c(1:ntimes)] <-  out[1,c(1:ntimes)] * -1
      # negative coefficient for urban/rural variable
      out[6,c(1:ntimes)] <-  out[6,c(1:ntimes)] * -1
      return(out)
    }
  
  }
  
  ###################################################
  # Individual level coefficients 
  ###################################################
  
  # --- -- beta.t: matrix nrow = number of covariates, ncol = time.periods, coefficients for patient characteristics under no treatment  
  # --- -- zeta.t: matrix nrow = number of covariates, ncol = time.periods. coefficients for patient characteristics under treatment 
  
  #browser()
  # coefficients for potential outcomes 
  if(scen == 50){
    params_list$beta.t <- matrix(0, nrow = params_list$px + params_list$pcind, 
                                 ncol = params_list$time.periods)
    params_list$zeta.t <- matrix(0, nrow = params_list$px + params_list$pcind, 
                                 ncol = params_list$time.periods)
  }else{
    params_list$beta.t <- matrix(seq(from = 1, 
                                     length.out = params_list$time.periods, 
                                     by = 1/params_list$time.periods), 
                                 nrow = params_list$px + params_list$pcind, 
                                 ncol = params_list$time.periods, 
                                 byrow = TRUE)
    
    params_list$zeta.t <- matrix(seq(from = 1, 
                                     length.out = params_list$time.periods, 
                                     by = 1/params_list$time.periods), 
                                 nrow = params_list$px + params_list$pcind, 
                                 ncol = params_list$time.periods, 
                                 byrow = TRUE)
  }
  
  
  ##############################################
  # cluster level coefficients for outcome model 
  ############################################## 
  
  #### Parameters of cluster level covariates (eg rural, practice) used for outcome model 
  if(scen == 50){
    params_list$gamma.t <-  matrix(seq(from = 0, 
                                       length.out = params_list$time.periods, 
                                       by = 0), 
                                   nrow = params_list$pc_cats + 1, 
                                   ncol = params_list$time.periods, 
                                   byrow = TRUE)
    params_list$xi.t <-  matrix(seq(from = 0, 
                                    length.out = params_list$time.periods, 
                                    by = 0),
                                nrow = params_list$pc_cats + 1, 
                                ncol = params_list$time.periods, 
                                byrow = TRUE)
    }else{
      params_list$gamma.t <- matrix(seq(from = 1, 
                                        length.out = params_list$time.periods, 
                                        by = 1/params_list$time.periods), 
                                     nrow = params_list$pc_cats + 1, 
                                     ncol = params_list$time.periods, 
                                    byrow = TRUE)
      params_list$xi.t <- matrix(seq(from = 1, 
                                     length.out = params_list$time.periods, 
                                     by = 1/params_list$time.periods),
                                     nrow = params_list$pc_cats + 1, 
                                     ncol = params_list$time.periods, 
                                     byrow = TRUE)
    }
  
  ######################
  ## Treatment effects
  #######################
  
  # no effect
  if(scen %in% c(50, 0)){
    params_list$fgt <- function(g,t){
      return(0)
    }
  }
  
  # constant effect
  if(scen %in% c(1, 5, 9, 10, 14, 15, 19, 20, 24, 25, 29, 30)){
    params_list$fgt <- function(g,t){
      if(t-g >= 0) {
        return(2)
      }else{
        return(0)
      }
    }
  }
  
  # time heterogeneity: lagged effect 
  if(scen %in% c(2, 6, 11, 16, 21, 31)){
    params_list$fgt <- function(g,t){
      if(t-g >= 1){
        return(2)
      }else{
        return(0)
      }
    }
  }
  
  # group-time heterogeneity (prev: increasing in time)
  if(scen %in% c(3, 7, 12, 17, 22, 32)){
    params_list$fgt <- function(g, t){
      if(t - g >= 0) {
        out <- 0 # need to initialize outcome because we also generate counterfactuals 
        # for unobserved g's which are set to 0 in this setting. 
        if(g == 3){
          out <- ifelse(t == 3, 1, ifelse(t == 4, 2, 3))
        } else if(g == 4){  # Changed to `else if` to ensure only one condition executes
          out <- ifelse(t == 4, 2, ifelse(t == 5, 3, 4))  # Fixed second condition
        } else if(g == 5){
          out <- 5
        }
        return(out)
      } else {
        return(0)
      }
    }
  }
  
  # group heterogeneity
  if(scen %in% c(4, 8, 13, 18, 23, 33)){
    params_list$fgt <- function(g,t){
      if(t-g >= 0) {
        out <- ifelse(g == 3, 1, ifelse(g == 4, 2, 3))
        return(out)
      }else{
        return(0)
      }
    }
   }
  
  
  ###################################################
  ## cluster level covariates and coefficients 
  ###################################################
  
  # --- -- V: array of matrices. For each t, V[,,t] contains cluster level covariates at time t 
  #            each element in the array is a matrix of nrow = number of clusters, ncol = number of cluster covariates 
  # --- -- genV: list of length ncol(V[,,t]) that contains function calls that generates cluster level covariates 
  # --- -- Vind: array of matrices of dimension nrow = nc, ncol = number of cluster covariates = pcind
  #               For each t, Vind[,,t] contains cluster level information that informs data generating mechanism for individuals
  # --- -- genVind: list of length ncol(Vin) that contains function calls that generates cluster level covariates 
  # --- -- Vprob: array of matrices with all cluster level covariates
  # --- -- ind.cov: vector of 1's and 0's of length ncol(V) [numer of cluster covariates]. If 1, this vector is used to generate individual level covariates 
  # --- -- gamma.t: matrix nrow = time.periods, ncol = number of cluster covariates, coefficients for cluster level covariates under no treatment 
  # --- -- xi.t: matrix nrow = time.periods, ncol = number of cluster covariates, coefficients for cluster level covariates under treatment  
  # --- -- cluster.rand.eff = indicator (TRUE or FALSE) of whether we should add a cluster random effect 
  
  ##### Covariates for fixed cluster level data 
  params_list$genV <- list()
  # Type of specialist (5 types)
  params_list$genV[[1]] <- function(nc){V1 <- t(rmultinom(n = nc, size = 1,
                                                          prob = c(0.4, 0.3, 0.3, 0)))
  return(V1)}
  
  # Urban rural 
  params_list$genV[[2]] <- function(nc){V2 <- rbinom(n = nc, size = 1, prob = 0.70)
  return(V2)}  
  
  #################################################################
  # Distribution of covariates that determine individual level data 
  #################################################################
  
  params_list$genVind <- list()
  
  # Proportion of females 
  params_list$genVind[[1]] <- function(nc){Vind1 <- runif(n = nc, min = 0.4, max = 0.6)
  return(Vind1)} 
  
  # Average Age
  params_list$genVind[[2]] <- function(nc){Vind2 <- round(rnorm(n = nc, mean = 5, sd = 4), 0)
  return(Vind2)}
  
  # Average ERG score 
  params_list$genVind[[3]] <- function(nc){Vind3 <- round(rnorm(n = nc, mean = 5, sd = 1), 0)
  return(Vind3)} 
  
  # Auxiliary simulation functions to generate covariates 
  params_list$simCovVind <- list()
  # Females (depends on cluster info)
  params_list$simCovVind[[1]] <- function(n, p){rbinom(n = n, size = 1, prob = p)}
  # Age (depends on cluster info)
  params_list$simCovVind[[2]] <- function(n, mu){round(rnorm(n = n, mean = mu, sd = 10),0)}
  # ERG score (depends on cluster info)
  params_list$simCovVind[[3]] <- function(n, mu){round(rnorm(n = n, mean = mu, sd = 1),0)}
  # Additional individual confounder whose distribution varies across time 
  params_list$simCovVind[[4]] <- function(n, t){round(rnorm(n = n, mean = sqrt(t), sd = 2),0)}

  
  ##################################
  ## Error functions
  ##################################
  
  # --- -- eps.it.dist.fun: function to generate errors under no treatment
  # --- -- nu.it.dist.fun: function to generate errors under treatment
  
  params_list$eps.it.fun <- function(nobs, t, ...){
    rnorm(nobs, mean = 0, sd = 0.5)
  }
  params_list$nu.it.fun <- function(nobs, t, ...){
    rnorm(nobs, mean = 0, sd = 0.5)
  }
  
  ##################################
  ## Individual Random effects
  ##################################
  
  # --- -- eta.i.fun: function to generate individual random effects under no treatment
  # --- -- alpha.i.fun: function to generate individual random effects under treatment 
  
  params_list$eta.i.fun <- function(nobs, ...){
    rnorm(nobs, mean = 0, sd = 0.5)
  }
  params_list$alpha.i.fun <- function(nobs, ...){
    rnorm(nobs, mean = 0, sd = 0.5)
  }

  ####################################################
  ### Coefficients of probability of joining program 
  ####################################################
  
  # --- -- theta.g: matrix of nrow = number of cluster level covariates + 
  #                 number of cluster level covariates that inform individual level covariates + 1 (intercept) 
  #                 ncol = length time.periods Vprob %*% theta.g[, g] = prob of joining at time g
  
  # browser()

  if(params_list$staggering){
    if(scen == 50){
      params_list$theta.g <- matrix(0, nrow = params_list$pc_cats + params_list$pcind + 1, 
                                    ncol = params_list$time.periods)
    }else{
      # to do: pick theta.g[1, 1],...theta.g[1, 10] manually for each time point (weight for each covariate)
      # we know it is fixed at 10 because we always have 10 time points
      # Maybe fix theta.g[,1], then dilute by time
      params_list$theta.g <- gen_theta(pc_cats = params_list$pc_cats,
                                       pc_ind = params_list$pcind, 
                                       ntimes = params_list$time.periods)
    }
  }else{
    params_list$period.all <- 3 
  }
  
  ##################################
  ## Outcome formulas
  ##################################
  
  # --- -- out.y0.t: function for counterfactual under no treatment 
  # --- -- out.y1.t: function for the counterfactuals under treatment
  # --- -- fgt: function that dictates relationship between g and t in outcome model 
  
  out.y0.t <- function(t,  mu.t, eta.i, cluster.rand.eff, X, beta.t, V, gamma.t, eps.it){
    y0.t <-  mu.t[t] + eta.i + X[, ,t] %*% matrix(beta.t[, t]) + 
      V[, ,t] %*% matrix(gamma.t[, t]) + eps.it[, t] +  cluster.rand.eff[, t]
  }    
  
  params_list$out.y0.t <- out.y0.t 
  
  out.y1.t <- function(t, rho.t, alpha.i, cluster.rand.eff, X, zeta.t, V, xi.t, nu.it, Gt, fgt){
    fgt_app <- mapply(fgt, Gt, t)
    y1.t <- rho.t[t] + alpha.i + X[, ,t] %*% matrix(zeta.t[, t]) + 
      V[, ,t] %*% matrix(xi.t[, t]) + (Gt <= t) * fgt_app + 
      nu.it[, t] + cluster.rand.eff[, t]
  } 
  
  params_list$out.y1.t <- out.y1.t 
  
  
  return(params_list)
  
}





