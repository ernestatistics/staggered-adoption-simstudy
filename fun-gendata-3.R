

# Cost Eff Simulation Functions
# Author: Ernesto Ulloa 
# Last updated: April 12, 2024

# functions: 
# gen_data (np, ntimes, covariates, error dist, correlation structure)
# start with a basic function then fit the models 

# #gen.data.ind <- function(params_dat){

gen.data.cluster <- function(params_dat){
  
  # gen.data: creates panel data under a staggered adoption setting with clustering
  # using information from params_dat
  
  # Input:
  # --- params_dat: a list with the following elements:
  
  ## Study information 
  # --- -- np: number of participants 
  # --- -- nc: number of clusters
  # --- -- n.periods: numeric, number of time periods
  # --- -- px: number of individual level covariates
  # --- -- pcind: number of cluster level covariates that 
  #               inform individual level covariates (ex: average age)
  # --- -- pc: number of cluster level covariates
  # --- -- npretrt: number of pretreatment periods we wish to have prior to treatment
  
  ## cluster information 
  # --- -- prop.c: vector of length number of clusters, proportion of members on each cluster 
  # --- -- mc: number of members per cluster (NOT USED AT THIS TIME)
  # --- -- Ic: array of matrices. For each t, Ic[,,t] 
  #            contains a matrix with nrow = number of participants and ncol = nc. 
  #            each matrix indicates if member i belongs to cluster c at time t
  # C_1,...C_n.periods: numeric vectors of length np that contain cluster number of each member
  # C_s_1,...C_s_n.periods: numeric vectors of length np that contain cluster number of each member after potential switching
  # --- -- include.switch (logic) if TRUE then patients can switch clusters
  # --- -- prob.switch is the probability of switching cluster (not being used in this function)
  # --- --  type.switch: type of switch indicates whether it decays with time or is constant (not being used in this function)
  # --- -- Ic_switch: array of updated matrices with switching. For each t, Ic[,,t] 
  #            contains a matrix with nrow = number of participants and ncol = nc. 
  #            each matrix indicates if member i belongs to cluster c at time t
  
  # Individual level covariates and coefficients 
  # --- -- X: array of matrices. For each t, X[,,t] contains individual level information at time t
  #           each element in the list is a matrix of nrow = number of participants, ncol = number of individual covariates 
  # --- -- beta.t: matrix nrow = number of covariates, ncol = n.periods, coefficients for patient characteristics under no treatment at time t
  # --- -- zeta.t: matrix nrow = number of covariates, ncol = n.periods, coefficients for patient characteristics under treatment at time t
  
  ## cluster level covariates and coefficients 
  
  # --- -- V: array of matrices. For each t, V[,,t] contains cluster level information at time t 
  #           each element in the array is a matrix of nrow = number of clusters, ncol = number of cluster covariates 
  #           the first covariate is the intercept, the last covariate corresponds to urban rural, 
  #           values in between are the categories of the multinomial
  
  # --- -- Vprob: array of dimension  nc, pcind + pc, and n.periods
  #        Vprob[, , t] is a matrix of dimension nrow = nc, ncol=  pcind + pc
  #        With theta.g, Vprob is used to generate probabilities of joining program at a given time t
  
  # --- -- gamma.t: matrix nrow = n.periods, ncol = number of cluster covariates, coefficients for cluster level covariates under no treatment 
  # --- -- xi.t: matrix nrow = n.periods, ncol = number of cluster covariates, coefficients for cluster level covariates under no treatment  
  
  ## Coefficients of probability of joining program 
  
  # --- -- staggering: logical, if TRUE there is staggering in the simulation 
  # --- -- if FALSE, all clusters enter at time period = period.all
  # --- -- theta.g: matrix: nrow = number of cluster level covariates, ncol = length n.periods 
  #         Note that Vprob[i,,t] %*% theta.g[, t] = prob of cluster i joining program at time t
  
  ## Time Trends 
  
  # --- -- mu.t: vector of length equal to n.periods, time effects under no treatment
  # --- -- rho.t: vector of length equal to n.periods, time effects under treatment 
  
  ## Errors 
  
  # --- -- eps.it.dist.fun: function to generate errors under no treatment
  # --- -- nu.it.dist.fun: function to generate errors under treatment
  
  ## Individual Random effects
  
  # --- -- eta.i: function to generate individual random effects under no treatment
  # --- -- alpha.i: function to generate individual random effects under treatment 
  
  ## Cluster level random effects: 
  
  # --- -- cluster.rand.eff: if TRUE then we include the random effects
  # --- -- psi.c.fun: function to generate cluster random effects
  
  ## Outcome formulas
  
  # --- -- out.y0.t: function for counterfactual under no treatment 
  # --- -- out.y1.t: function for the counterfactuals under treatment
  # --- -- fgt: function that dictates relationship between g (time of beggining intervention) and t in outcome model 
  
  # Output:
  
  # --- Data frame: number of rows = np * n.periods, variables are
  # --- id: subject id
  # --- t: time 
  # --- pt: Probability of joining program at time t
  # --- gt = period in which they enrolled in the intervention
  # --- trt = indicator whether patient is enrolled in treatment 
  # --- y0.t = counterfactual under no treatment at time t
  # --- y1.t = counterfactual under treatment at time t
  # --- y.t  = observed outcome 
  # --- y.t.switch = counterfactual under no treatment at time t when cluster switching is present
  # --- y.t.switch = counterfactual under treatment at time t when cluster switching is present 
  # --- x.t = covariates at time t 
  # --- v.t = cluster covariates at time t
  
  ################################################################################
  # Load Parameters
  ################################################################################
  
  # number of participants 
  np <- params_dat$np 
  # number of clusters 
  nc <- params_dat$nc 
  
  # time periods
  time.periods <- params_dat$time.periods
  # number of time periods
  n.periods <- time.periods
  all.periods <- 1:n.periods
  
  # number of individual level covariates 
  px <- params_dat$px 
  # number of cluster level covariates 
  pc <- params_dat$pc 
  # number of pre-treatment periods prior to treatment 
  npretrt <- params_dat$npretrt
  npostrt <- params_dat$npostrt 
  # Number of cluster level covariates that inform individual level covariates (MAY NOT NEED)
  pcind <- params_dat$pcind
  
  ### Cluster information 
  
  # Proportion of patients under each cluster 
  prop.c <- params_dat$prop.c 
  
  # Number of members and matrix of indicators belonging to each cluster
  mc <- params_dat$mc 
  Ic <-  params_dat$Ic
  
  ## Cluster switching 
  include.switch <- params_dat$include.switch
  
  # Covariate functions that depend on cluster level summaries 
  genVind <- list()
  genVind[[1]] <- params_dat$genVind[[1]]
  genVind[[2]] <- params_dat$genVind[[2]]
  genVind[[3]] <- params_dat$genVind[[3]]
  
  #### Auxiliary functions to generate cluster info
  # Females (depends on cluster info)
  simCovVind <- list()
  simCovVind[[1]] <- params_dat$simCovVind[[1]] 
  # Age (depends on cluster info)
  simCovVind[[2]] <- params_dat$simCovVind[[2]] 
  # ERG score (depends on cluster info)
  simCovVind[[3]] <- params_dat$simCovVind[[3]] 
  # Additional individual confounder whose distribution varies across time 
  simCovVind[[4]] <- params_dat$simCovVind[[4]] 
  
  genV <- list()
  genV[[1]] <- params_dat$genV[[1]]
  genV[[2]] <- params_dat$genV[[2]]
  
  # switch indicator
  include.switch <- params_dat$include.switch
  
  # Matrix of indicators belonging to each cluster with switching 
  # Ic_switch <-  params_dat$Ic_switch
  
  ## Individual level covariates and coefficients
  beta.t <- params_dat$beta.t
  zeta.t <- params_dat$zeta.t 
  
  ## cluster level coefficients
  cluster.rand.eff <- params_dat$cluster.rand.eff
  gamma.t <- params_dat$gamma.t 
  xi.t <- params_dat$xi.t
  
  ## V contains cluster level covariates 
  #V <- params_dat$V
  #Vprob <- params_dat$Vprob
  
  ## Probability coefs for joining intervention
  if(params_dat$staggering){
    theta.g <- params_dat$theta.g 
  }else{
    period.all <- params_dat$period.all 
  }
  
  ## Time trends 
  mu.t <- params_dat$mu.t 
  rho.t <- params_dat$rho.t 
  
  ## Random errors 
  eps.it.fun <- params_dat$eps.it.fun 
  nu.it.fun <- params_dat$nu.it.fun
  
  ## Individual random effects 
  eta.i.fun <- params_dat$eta.i.fun
  alpha.i.fun <- params_dat$alpha.i.fun
  
  ## Cluster random effects 
  cluster.rand.eff <-  params_dat$cluster.rand.eff
  psi.c.fun <- params_dat$psi.c.fun
  
  ## Outcome regression functions 
  out.y0.t <- params_dat$out.y0.t
  out.y1.t <- params_dat$out.y1.t
  fgt <- params_dat$fgt
  
  #########################
  ## Begin generating data
  #########################
  
  ###################################################
  # Individual level covariates  
  ###################################################
  
  # --- -- X: array of matrices. For each t, X[,,t] contains individual-level information at time t
  #           each element in the list is a matrix of nrow = number of observations, ncol = number of individual covariates 
  
  # Fill it with 1's so we already have the intercept
  X <- array(dim = c(np, px + pcind, time.periods), 1)
  
  #################################################################
  # Distribution of covariates that determine individual level data 
  #################################################################
  
  Vind <- array(dim = c(nrow = nc, ncol = pcind, time.periods))
  
  for(i in 1:pcind){
    Vind[, i, 1] <- genVind[[i]](nc)
    Vind[, i, 2:time.periods] <- Vind[, i, 1]
  }
  
  # This for loop generates individual level covariates at time t
  #set.seed(1)
  
  for(t in 1:time.periods){
    # Loop for time
    for(p in 1:(px + pcind -1)){
      # Loop for number of variables
      # auxiliary variable for indexing
      start_index <- 0
      for(c in 1:nc){
        # Loop for number of clusters 
        # We include the intercept in the individual level covariates 
        if(p <= pcind){
          if(p <= pcind - 1){
            # if time is 1, then we simulate sex and age, if time is greater than 1 we take the first value generated at time 1
            X[(start_index + 1):(start_index + mc[c]), p + 1, t] <- I(t == 1) * simCovVind[[p]](mc[c], Vind[c, p, t]) +
              I(t != 1) * X[(start_index + 1):(start_index + mc[c]), p + 1, 1]  
          }else{
            # ERG score can change in time, so we simulate it every time
            X[(start_index + 1):(start_index + mc[c]), p + 1, t] <- simCovVind[[p]](mc[c], Vind[c, p, t]) 
          }
        }else{
          # finally we simulate an individual level covariate that also changes in time 
          X[(start_index + 1):(start_index + mc[c]), p + 1, t] <- simCovVind[[p]](mc[c], t)
        }
        start_index <- start_index + mc[c]
      }
    }
  }
  
  ###################################################
  # Cluster level covariates
  ###################################################
  
  #browser()
  # Begin to create matrix with cluster level covariates 
  # Start with intercept
  temp <- matrix(1, nrow = nc, ncol = 1)
  
  # Generate and bind cluster level variables
  for(i in 1:pc){
    temp <- cbind(temp, genV[[i]](nc))
  }
  
  # In this setting cluster level covariates do not change across time 
  V <- array(dim = c(nrow(temp), ncol(temp), time.periods))
  for(t in 1:time.periods){
    V[,,t] <- temp
  }
  
  # --- -- cluster level summaries (defined here, no need to provide as input)
  # this information is not used to generate data, but can be used to estimate parameters
  
  cluster_summaries <- list()
  
  for(t in 1:time.periods){
    # Loop for time
    start_index <- 0 
    temp <- list()
    for(c in 1:nc){
      # Loop for cluster
      temp[[c]] <- matrix(nrow = 1, ncol = px + pcind)
      temp[[c]] <- colMeans(X[(start_index + 1):(start_index + mc[c]), , t]) 
      start_index <- start_index + mc[c]
    }
    cluster_summaries[[t]] <- do.call(rbind, temp)
    cluster_summaries[[t]] <- data.frame(cluster_summaries[[t]])
    cluster_summaries[[t]]$c <- 1:c
    cluster_summaries[[t]]$t <- t
  }
  
  cluster_summaries_marginal <- do.call(rbind, cluster_summaries)
  # cluster_summaries_marginal <- cluster_summaries_marginal %>% 
  #   group_by(c) %>% 
  #   summarise(across(where(is.numeric), mean)) %>%
  #   select(-t)
  
  ##################################
  # cluster membership
  ##################################
  
  # create array of matrices that denotes, for each time, which members are part of which cluster 
  Ic <- array(dim = c(np, nc, time.periods))
  # for a given time, each row represents an individual and each column a cluster 
  for(t in 1:time.periods){
    Ic[, , t]  <- matrix(0, nrow = np, ncol = nc)
    start_index <- 1
    for (i in 1:nc) {
      if(nc == 1){
        Ic[, , t][start_index:(start_index + mc[i] - 1)] <- 1
        start_index <- start_index + mc[i]
      }else{
        Ic[, , t][start_index:(start_index + mc[i] - 1), i] <- 1
        start_index <- start_index + mc[i]
      }
      
    }
  }
  
  ##################################
  # Switching clusters 
  ##################################
  
  # Probabilities for switching from one cluster to another cluster 
  # the cluster that they switch to is random 
  # it is just a probability of switching or not switching (or perhaps we can do unenrollment) and Ic[,,t]
  # it depends on both cluster level covariates and individual level covariates 
  
  if(include.switch){
    if(type.switch == 'constant'){
      # whether they switch or not 
      switch.ind <- rbinom(n = np, size = 1, prob = prob.switch)
      # randomly select to which cluster they will move to 
      switch.clin <- round(runif(n = np, min = 1, max = nc),0)
      # randomly select at which time they will make the switch 
      switch.time <- round(runif(n = np, min = 2, max = time.periods-1),0)
      # modify the cluster matrix
      Ic_switch <- Ic
      for(i in 1:np){
        if(switch.ind[i] == 1){
          # if they switch enter and modify the matrix manually
          idx.cluster <- which(Ic[i, , switch.time[i]] == 1)
          # in original cluster it will be 0
          Ic_switch[i, idx.cluster, switch.time[i]:time.periods] <- 0
          # in new cluster it will be 1 
          Ic_switch[i, switch.clin[i], switch.time[i]:time.periods] <- 1
        }
      }
    }else{
      include.switch <- FALSE
      switch.ind <- FALSE
    }
    
    #if(type.switch == 'decaying'){
    #  in these cases the probabilities of switching will need to decrease in time 
    #}
  }else{
    Ic_switch <- Ic
    switch.ind <- include.switch
  }
  
  ######################
  ## Cluster indicator
  #######################
  
  tempCind <- list()
  
  if(nc == 1){
    for(t in 1:time.periods){
      tempCind[[paste0('C_', t)]] <- rep(1, np)
      tempCind[[paste0('C_s_', t)]] <- rep(1, np)
    }
  }else{
    for(t in 1:time.periods){
      tempCind[[paste0('C_', t)]] <- rep(0, np)
      tempCind[[paste0('C_s_', t)]] <- rep(0, np)
      tempCind[[paste0('C_', t)]] <- as.vector(Ic[, , t] %*% 1:nc)
      tempCind[[paste0('C_s_', t)]] <- as.vector(Ic_switch[, , t] %*% 1:nc)
    }
  }
  
  ######################
  # Generate error terms
  ######################
  
  eps.it <- matrix(0, nrow = np, ncol = n.periods)
  nu.it <- matrix(0, nrow = np, ncol = n.periods)
  
  for(t in 1:n.periods){
    eps.it[, t] <- eps.it.fun(np, t)
    nu.it[, t] <- nu.it.fun(np, t)
  }
  
  ####################################
  # Individual level random effects
  ####################################
  
  # Generate random effects under no treatment
  eta.i <-  eta.i.fun(np)
  
  # Generate random effects under treatment 
  # if (random effects under treatment is null do same random effects under no treatment)
  if(is.null(alpha.i.fun)){
    alpha.i <- eta.i
  }else{
    alpha.i <- alpha.i.fun(np)
  }
  
  ####################################
  # Cluster level random effects
  ####################################
  
  # psi.c contains cluster random effects
  # Note: these do not change across time 
  psi.c <- matrix(0, nrow = nc, ncol = n.periods)
  
  if(cluster.rand.eff){
    temp <- psi.c.fun(nc)
    for(t in 1:n.periods){
      psi.c[, t] <- temp
    }
  }
  
  # Define random effects at the individual level depending on which cluster they belong to 
  cluster_rand_eff <- matrix(0, nrow = np, ncol = n.periods)
  cluster_rand_eff_switch <- matrix(0, nrow = np, ncol = n.periods)
  
  for(t in 1:n.periods){
    for(c in 1:nc){
      cluster_patients <- which(Ic[, c, t] == 1)
      cluster_rand_eff[cluster_patients, t] <- rep(psi.c[c, t], length(cluster_patients))
      cluster_patients_switch <- which(Ic_switch[, c, t] == 1)
      cluster_rand_eff_switch[cluster_patients_switch, t] <- rep(psi.c[c, t], length(cluster_patients_switch))
    }
  }
  
  ##################
  # Generate Treatment 
  ##################
  
  ## Combine cluster level covariates and their averages
  ## Vprob is used to generate probabilities of joining program at a given time 
  ## Vprob contains both V and summaries of Vind
  ## Vind is attached to X as individual level covariates and so parameters for individual level covariates affect it as well
  ## V contains cluster level summaries only and affects outcome through cluster level covariates
  
  if(nc == 1){
    Vprob <- array(dim = c(nrow = nc, ncol = length(V[, , 1]) + length(Vind[, , 1]), time.periods))
    for(t in 1:time.periods){
      Vprob[,,t] <- c(V[,,t], Vind[,,t])
    }
  } else{
    Vprob <- array(dim = c(nrow = nc, ncol = ncol(V[, , 1]) + ncol(Vind[, , 1]),  time.periods))
    for(t in 1:time.periods){
      Vprob[,,t] <- cbind(V[,,t], Vind[,,t])
    }
  }
  
  #center_matrix <- function(matrix) {
  #  centered_matrix <- apply(matrix, 2, function(col) col - mean(col))
  #  return(centered_matrix)
  #}
  #Vprob_c <- center_matrix(Vprob[, , 1])
  Vprob_c <- Vprob[, , 1]
  
  if(params_dat$staggering){
    # Generate probability of treatment for all clusters
    # ser()
    temp <- exp(Vprob_c %*% theta.g)
    # standardize probabilities to accommodate the 0's
    pr <- temp / rowSums(temp)
    # adjust probabilities of treatment so that we have npretrt periods without treatment 
    # Sample from probability of treatment to get entry points
    Gt <- apply(pr, 1, function(pvec) sample(seq(1, n.periods), size = 1, prob = pvec))
    
    if(npretrt > 0){
      #browser()
      theta.g[, 1:npretrt] <- -10^3
      temp <- exp(Vprob_c %*% theta.g)
      pr <- temp / rowSums(temp)
      # vector of probabilities
      #vProb = cbind(exp(mX%*%vCoef1), exp(mX%*%vCoef2), exp(mX%*%vCoef3))
      # multinomial draws
      mChoices = t(apply(pr , 1, rmultinom, n = 1, size = 1))
      Gt <- apply(mChoices, 1, function(x) which(x == 1))
      
    }
    
    
    # adjust probabilities of treatment so that we have npretrt periods without treatment 
    if(npostrt > 0){
      idxpost <- (n.periods-npostrt +1):n.periods
      temp[,  idxpost] <- rep(0, nc * npostrt)
    }
    
    
  }else{
    
    pr <- matrix(0, ncol = n.periods, nrow = nc)
    # because there is no staggering, every cluster received treatment at time period.all
    pr[, period.all] <- 1 
    Gt <- rep(period.all, nc)
    
  }
  
  # browser()
  ## Gt_ind and Gt_ind_switch are not properly defined 
  ## Gt_ind and Gt_ind_switch are actually Ds_ind and Ds_ind_switch 
  ## using Callaway and Sant'Anna paper notation 
  ## to get Gt we need to merge Gt by cluster 
  ## we need to add cluster information for each id. that is easier
  ## then keep going with Gt properly defined
  ## rename Gt_ind and Gt_ind_switch as treat and treat_switch variables 
  
  # Define treatment group and treatment indicator at the individual level
  Gt_ind <- matrix(-1, nrow = np, ncol = n.periods)
  treat_ind <- matrix(-1, nrow = np, ncol = n.periods)
  Gt_ind_switch <- matrix(-1, nrow = np, ncol = n.periods)
  treat_ind_switch <- matrix(-1, nrow = np, ncol = n.periods)
  
  # Generate the treatment matrix based on the provided information
  for(t in 1:n.periods){
    for (c in 1:nc){
      # treatment matrix without switching
      cluster_patients <- which(Ic[, c, t] == 1)
      
      # define treatment indicator and treatment group
      treat_ind[cluster_patients, t] <- ifelse(t >= Gt[c], 1, 0)
      Gt_ind[cluster_patients, t] <- Gt[c]
      
      # treatment matrix with switching
      cluster_patients_switch <- which(Ic_switch[, c, t] == 1)
      treat_ind_switch[cluster_patients_switch, t] <- ifelse(t >= Gt[c], 1, 0)
      Gt_ind_switch[cluster_patients_switch, t] <- Gt[c]
      
    }
  }
  
  ################################################################################
  # Cluster level covariates for individuals
  ################################################################################
  
  # --- -- Vind: array of matrices. For each t, Vind[,,t] contains same cluster level information from V at time t at individual level
  #             Each element in the array is a matrix of nrow = number of individuals, ncol = number of cluster covariates 
  # --- -- Vind_switch: array of matrices. For each t, Vind[,,t] contains same cluster level information at time t at individual level
  #             Each element in the array is a matrix of nrow = number of individuals, ncol = number of cluster covariates 
  
  Vind <- array(dim = c(np, dim(V)[2], n.periods))
  Vind_switch <- array(dim = c(np, dim(V)[2], n.periods))
  
  # browser()
  if(nc == 1){
    for(t in 1:n.periods){
      Vind[, , t] <- matrix(rep(V[, , t], times = np), nrow = np, byrow = TRUE)
      Vind_switch[, ,t] <- matrix(rep(V[, , t], times = np), nrow = np, byrow = TRUE)
    }
  }else{
    for(t in 1:n.periods){
      Vind[, , t] <- Ic[, , t] %*% V[, , t]
      Vind_switch[, ,t] <-  Ic_switch[, , t] %*% V[, , t]
    }
  }
  
  
  ################################################################################
  # Outcomes
  ################################################################################
  
  # Generate outcome under no treatment
  y0.t <- sapply(1:n.periods, function(t){out.y0.t(t, mu.t, eta.i, cluster_rand_eff, X, 
                                                   beta.t, Vind, gamma.t, eps.it)})
  
  # Generate outcome for each value each possible value of time for each possible value of treatment group g
  y1 <- array(dim = c(np, n.periods, n.periods))
  
  #Error in if (t - g >= 2) { : the condition has length > 1
  #browser()
  for(t in 1:n.periods){
    # We generate Yit(g) for each g
    for(g in 1:n.periods){
      Gt_temp <- rep(g, np)
      y1[, g , t] <- out.y1.t(t, rho.t, alpha.i, cluster_rand_eff, X, 
                              zeta.t, Vind, xi.t, nu.it, Gt_temp, fgt)
    }
  }
  
  # Generate outcome under treatment arm trajectory and at the observed G
  y1.t <- sapply(1:n.periods, function(t){
    out.y1.t(t, rho.t, alpha.i, cluster_rand_eff, X, zeta.t, Vind, xi.t, nu.it,
             Gt_ind[, t], fgt)})
  
  # Generate observed outcome 
  y.obs <- sapply(1:n.periods, function(t){ 
    ifelse(Gt_ind[, t]<= t, y1.t[, t], y0.t[, t])})
  
  # Generate outcomes under cluster switching
  if(include.switch){
    y0.t.switch <- sapply(1:n.periods, 
                          function(t){out.y0.t(t, mu.t, eta.i, cluster_rand_eff_switch, 
                                               X, beta.t, Vind_switch, gamma.t, eps.it)})
    
    y1.t.switch <- sapply(1:n.periods, function(t){
      out.y1.t(t, rho.t, alpha.i, cluster_rand_eff_switch, X, zeta.t,
               Vind_switch, xi.t, nu.it, Gt_ind_switch[, t], fgt)})
    
    
    y.obs.switch <- sapply(1:n.periods, 
                           function(t){(Gt_ind_switch[, t] <= t) * y1.t.switch[, t] + 
                               (Gt_ind_switch[, t] > t) * y0.t.switch[, t]})
    
  }else{
    y0.t.switch <- y0.t
    y1.t.switch <- y1.t
    y.obs.switch <- y.obs
  }
  
  # Build data set 
  
  # Long format 
  
  # Function to convert vectors to long data format
  make.long <- function(dat.wide, name, labelvar = 't'){
    
    # Input: matrix where each column is a time period and each row is an observation
    #        or each column is a cluster and each row is an observation (change labelvar to 'cluster')
    # Output: data where each row is an observation at a given time point (or cluster)
    
    dat_vec <- c(dat.wide)
    t <- rep(1:ncol(dat.wide), each = nrow(dat.wide))
    id <- rep(1:nrow(dat.wide), ncol(dat.wide))
    dat_vec <- cbind(t, id, dat_vec)
    colnames(dat_vec) <- c(labelvar, 'id', name)
    
    return(dat_vec)
  }
  
  # Store results in long format
  
  eps.it_long <- make.long(eps.it, 'eps')
  nu.it_long <- make.long(nu.it, 'nu')
  y0.t_long  <-  make.long(y0.t, 'Y0')
  y1.t_long <-  make.long(y1.t, 'Y1')
  y.obs_long <-  make.long(y.obs, 'Y')
  y.obs.switch_long <-  make.long(y.obs.switch, 'Y')
  
  ## Store potential outcomes in long format
  temp.y1 <- list()
  temp.y1[[1]] <- make.long(y1[, 1 ,], paste0('Yg', 1))
  
  for(g in 2:n.periods){
    # y1[,g , ] contains potential outcomes of intervening at time g
    temp.y1[[g]] <- make.long(y1[, g , ], paste0('Yg', g))
  }
  
  y1_long <- do.call(cbind, temp.y1)
  y1_long <- y1_long[, unique(colnames(y1_long))]
  
  ##### Need to double check X
  
  ## Store individual level variables in long format
  
  ##### X_wide will have individual level covariates (individual and cluster level informed) 
  ##### for each time period each variable is indexed by time t with suffix _t
  temp.X <- list()
  for(t in 1:n.periods){
    temp.X[[t]] <- data.frame(X[, , t])
    colnames(temp.X[[t]]) <- paste0('X', 1:ncol(X), '_', t)
  }
  
  #browser()
  temp.X <- do.call(cbind, temp.X)
  temp.X$id <- 1:np
  X_wide <-  temp.X
  
  ##### V_wide will have cluster level covariates at individual level for each time
  ##### for each time period each variable is indexed by time t with suffix _t
  
  temp.V <- list()
  
  for(t in 1:n.periods){
    temp.V[[t]] <- data.frame(Vind[, , t])
    colnames(temp.V[[t]]) <- paste0('V', 1:ncol(Vind), '_', t)
  }
  
  temp.V.switch <- list()
  for(t in 1:n.periods){
    temp.V.switch[[t]] <- data.frame(Vind_switch[, , t])
    colnames(temp.V.switch[[t]]) <- paste0('V_s', 1:ncol(Vind), '_', t)
  }
  
  
  temp.V <- do.call(cbind, temp.V)
  temp.V$id <- 1:np
  V_wide <-  temp.V
  
  temp.V.switch <- do.call(cbind, temp.V.switch)
  temp.V.switch$id <- 1:np
  V_wide.switch <-  temp.V.switch
  
  ### Now we define X_long, V_long and V_long.switch in long formats with time period in an additional column 
  
  # browser()
  temp.V <- list()
  for(t in 1:n.periods){
    temp.V[[t]] <- data.frame(Vind[, , t])
    colnames(temp.V[[t]]) <- paste0('V', 1:ncol(Vind))
    temp.V[[t]]$t <- t
    temp.V[[t]]$id <- 1:np
  }
  
  V_long <- do.call(rbind, temp.V)
  
  temp.V.switch <- list()
  for(t in 1:n.periods){
    temp.V.switch[[t]] <- data.frame(Vind_switch[, , t])
    colnames(temp.V.switch[[t]]) <- paste0('V_s', 1:ncol(Vind_switch))
    temp.V.switch[[t]]$t <- t
    temp.V.switch[[t]]$id <- 1:np
  }
  
  V_long.switch <- do.call(rbind, temp.V.switch)
  
  temp.X <- list()
  for(t in 1:n.periods){
    temp.X[[t]] <- data.frame(X[, , t])
    colnames(temp.V[[t]]) <- paste0('X', 1:ncol(X))
    temp.X[[t]]$t <- t
    temp.X[[t]]$id <- 1:np
  }
  
  X_long <- do.call(rbind, temp.X)
  
  ## Store cluster level indicators in long and wide format 
  temp.I <- list()
  temp.I.switch <- list()
  
  #browser()
  C_wide <- data.frame(C_1 = rep(0, np))
  C_wide_switch <- data.frame(C_s_1 = rep(0, np))
  
  
  for(t in 1:n.periods){
    
    temp.I[[t]] <- data.frame(Ic[, , t])
    # 1) temp.I[[t]][i, c] = 1 if observation i belongs to cluster c at time t 
    colnames(temp.I[[t]]) <- paste('I', 1:nc, t, sep = '_')
    
    temp.I.switch[[t]] <- data.frame(Ic_switch[,,t])
    # 1) temp.I.switch[[t]][i, c] = 1 if observation i belongs to cluster c at time t 
    colnames(temp.I.switch[[t]]) <- paste('Is', 1:nc, t, sep = '_')
    
    C_wide[[paste0('C_', t)]] <- tempCind[[paste0('C_', t)]]
    C_wide_switch[[paste0('C_s_',t)]] <- tempCind[[paste0('C_s_', t)]]
    
  }
  
  # 1) I_c_t[i, ] = 1 if observation i belongs to cluster c at time t 
  I_wide <- do.call(cbind, temp.I)
  I_wide$id <- 1:np
  I_wide_switch <- do.call(cbind, temp.I.switch)
  I_wide_switch$id <- 1:np
  
  C_wide$id <- 1:np
  C_wide_switch$id <- 1:np
  
  C_long <- pivot_longer(C_wide, 
                         cols = starts_with("C"), 
                         names_to = "t", 
                         values_to = "C")
  
  C_long$t <- as.numeric(gsub("C_", "", C_long$t))
  
  C_switch_long <-  pivot_longer(C_wide_switch, 
                                 cols = starts_with("C_s"), 
                                 names_to = "t", 
                                 values_to = "C_s")
  
  C_switch_long$t <- as.numeric(gsub("C_s_", "", C_switch_long$t))
  
  # Function to converts I_wide in wide format to long format by time 
  convert_Iwide_to_long <- function(df, include.switch){
    
    #browser()
    if(!include.switch){
      # Reshape the data from wide to long format
      df_long <- df %>%
        pivot_longer(cols = -id, names_to = "variable", values_to = "value") %>%
        separate(variable, into = c("I", "c", "t"), sep = "_")
      
      
      # Pivot the data to create 'I_c' columns
      df_wide <- df_long %>%
        select(-I) %>%
        spread(key = c, value = value)
      
      colnames(df_wide)[3:length(colnames(df_wide))] <- paste0('I_', colnames(df_wide)[3:length(colnames(df_wide))]) 
      
      ### reorder the indicators of cluster membership
      
      # Extract the numeric part of column names
      numeric_part <- as.numeric(gsub("I_", "", grep("^I_", colnames(df_wide), value = TRUE)))
      
      # Order of columns, we add a 2 to exclude id and t 
      custom_order <- order(numeric_part) + 2
      
      # Reorder the columns
      df_wide <- df_wide %>%
        select(id, t, all_of(custom_order))
      
    }else{
      # Reshape the data from wide to long format
      df_long <- df %>%
        pivot_longer(cols = -id, names_to = "variable", values_to = "value") %>%
        separate(variable, into = c("Is", "c", "t"), sep = "_")
      
      # Pivot the data to create 'I_c_s' columns
      df_wide <- df_long %>%
        select(-Is) %>%
        spread(key = c, value = value)
      
      colnames(df_wide)[3:length(colnames(df_wide))] <- paste0('I_s', colnames(df_wide)[3:length(colnames(df_wide))] )
      
      ### reorder the indicators of cluster membership 
      
      # Extract the numeric part of column names
      numeric_part <- as.numeric(gsub("I_s", "", grep("^I_", colnames(df_wide), value = TRUE)))
      
      # Define order of columns, we add a 2 to exclude id and t 
      custom_order <- order(numeric_part) + 2
      
      # Reorder the columns
      df_wide <- df_wide %>%
        select(id, t, all_of(custom_order))
      
    }
    
    
    return(df_wide)
  }
  
  #browser()
  I_long_t <- convert_Iwide_to_long(I_wide, FALSE)
  
  if(include.switch){
    I_long_t_switch <- convert_Iwide_to_long(I_wide_switch, include.switch)
  }
  
  # 2) and in long format we want for every i and t, a matrix with 15 columns indicating whether observation i belongs to corresponding column (= cluster) at time t
  
  treat_long <- expand.grid(id = 1:np, t = 1:n.periods)
  treat_long$treat <- -1
  
  Gt_long <- expand.grid(id = 1:np, t = 1:n.periods)
  Gt_long$Gt <- -1
  
  for(j in 1:n.periods){
    curr_time <- j    
    treat_long[treat_long$t == curr_time, ]$treat <- treat_ind[, curr_time]
    Gt_long[treat_long$t == curr_time, ]$Gt <- Gt_ind[, curr_time]
  }
  
  if(include.switch){
    treat_switch_long <- expand.grid(id = 1:np, t = 1:n.periods)
    treat_switch_long$treat_switch <- -1
    
    Gt_switch_long  <- expand.grid(id = 1:np, t = 1:n.periods)
    Gt_switch_long$treat_switch <- -1
    
  }
  if(include.switch){
    for(j in 1:n.periods){
      curr_time <- j  
      treat_switch_long[treat_switch_long$t == curr_time, ]$treat_switch <- treat_ind_switch[, curr_time]
      Gt_switch_long[treat_switch_long$t == curr_time, ]$Gt_switch <- Gt_ind_switch[, curr_time]
    }
  }
  
  # browser()
  
  # Store probabilities of treatment 
  pt_id <- data.frame(id = 1:np)
  # probabilities are at cluster level, here we assign them to each individual
  # Function to expand matrix based on vector
  expand_matrix <- function(vector, matrix) {
    # Initialize an empty list to store duplicated rows
    duplicated_rows <- list()
    
    # Iterate through each row in the matrix
    for (i in 1:nrow(matrix)) {
      # Duplicate the current row based on the corresponding number in the vector
      duplicated_row <- matrix(rep(matrix[i, , drop = FALSE], 
                                   as.integer(vector[i])), 
                               nrow = as.integer(vector[i]), byrow = TRUE)
      
      # Append duplicated row to the list
      duplicated_rows[[i]] <- duplicated_row
    }
    
    # Combine all duplicated rows into a single matrix
    expanded_matrix <- do.call(rbind, duplicated_rows)
    
    return(expanded_matrix)
  }
  
  pr_2 <-  expand_matrix(mc, pr)
  pt_id <- cbind(pr_2, pt_id) 
  colnames(pt_id)[1:n.periods] <- paste0('prtrt', 1:n.periods)
  
  # Begin setting up long data format 
  long_dat <- merge(eps.it_long, nu.it_long, by = c('id', 't'))
  long_dat <- merge(long_dat, y0.t_long, by = c('id', 't'))     
  long_dat <- merge(long_dat, y1.t_long, by = c('id', 't'))     
  long_dat <- merge(long_dat, y.obs_long, by = c('id', 't'))
  long_dat <- merge(long_dat, y1_long, by = c('id', 't'))     
  long_dat <- merge(long_dat, treat_long, by = c('id', 't'))  
  long_dat <- merge(long_dat, Gt_long, by = c('id', 't'))  
  long_dat <- merge(long_dat, C_long, by = c('id', 't'))
  
  if(include.switch){
    long_dat <- merge(long_dat, treat_switch_long, by = c('id', 't'))  
    long_dat <- merge(long_dat, Gt_switch_long, by = c('id', 't'))  
    long_dat <- merge(long_dat, C_switch_long, by = c('id', 't'))
  }
  
  long_dat <- merge(long_dat, pt_id, by = 'id')  
  
  # Determine the columns to convert
  variables_to_wide <- names(long_dat)[-(1:2)]
  
  ################################################################################
  ## Construct wide data 
  ################################################################################
  
  # Convert to wide format
  wide_dat <- long_dat %>% pivot_wider(names_from = t, 
                                       values_from = all_of(variables_to_wide))
  
  # Order wide data by time 
  # to do: finish ordering names
  
  # Get the variable names matching the pattern "treat_" followed by one or more digits
  var_names <- names(wide_dat)[grepl(paste0("^",variables_to_wide[1],"_\\d+$"), names(wide_dat))]
  # Order the variable names
  ordered_var_names <- var_names[order(as.numeric(gsub(paste0("^",variables_to_wide[1],"_"),"", var_names)))]
  
  # Reorder the columns of the data frame based on the ordered variable names
  for(i in 2:length(variables_to_wide)){
    var_names <- names(wide_dat)[grepl(paste0("^",variables_to_wide[i],"_\\d+$"), names(wide_dat))]
    # Order the variable names
    ordered_var_names <- c(ordered_var_names, 
                           var_names[order(as.numeric(gsub(paste0("^",variables_to_wide[i],"_"),"", var_names)))])
  }  
  
  # Merge wide data with covariates and cluster information 
  wide_dat <- merge(wide_dat, I_wide, by = 'id')
  wide_dat <- merge(wide_dat, C_wide, by = 'id')
  wide_dat <- merge(wide_dat, X_wide, by = 'id')
  wide_dat <- merge(wide_dat, V_wide, by = 'id')
  
  if(include.switch){
    wide_dat <- merge(wide_dat, V_wide.switch, by = 'id')
    wide_dat <- merge(wide_dat, I_wide_switch, by = 'id')
    wide_dat <- merge(wide_dat, C_wide_switch, by = 'id')
  }
  
  # Add covariates to long format and cluster information 
  long_dat <- merge(long_dat, X_long, by = c('id', 't')) 
  long_dat <- merge(long_dat, V_long, by = c('id', 't'))
  long_dat <- merge(long_dat, I_long_t, by = c('id', 't'))
  
  if(include.switch){
    long_dat <- merge(long_dat, V_long.switch, by = c('id', 't'))  
    long_dat <- merge(long_dat, I_long_t_switch, by = c('id', 't'))     
  }
  # Define G as numeric 
  #long_dat$G <- long_dat$Gt
  # make Gt a factor variable 
  long_dat$Gt_f <- as.factor(long_dat$Gt)
  # Create treatment indicator for TWFE model 
  long_dat$Gt_t <- ifelse(long_dat$Gt <= long_dat$t, 1, 0)
  long_dat$Gt_t_f <- as.factor(long_dat$Gt_t)
  # Make time a factor 
  long_dat$t_f <- as.factor(long_dat$t)
  
  #browser()
  #if(! scen %in% 1:4){
  if(TRUE){
    # compute all available distances between Gt and time in observed data 
    Gt_vals <- unique(long_dat$Gt)
    long_dat$dist <- 0
    long_dat$label_dis <- ''
    k <- 1
    store_dist_vals <- matrix(nrow = length(Gt_vals)*length(all.periods), ncol = 4)
    for(i in 1:length(Gt_vals)){
      for(j in 1:length(all.periods)){
        curr_dist <- all.periods[j] - Gt_vals[i] 
        curr_idx <- long_dat$Gt == Gt_vals[i] & long_dat$t ==  all.periods[j]
        if(sum(curr_idx)>0){
          long_dat$dist[curr_idx] <- curr_dist 
          long_dat$label[curr_idx] <- paste0('G',  Gt_vals[i], 'T', all.periods[j])
          store_dist_vals[k, 1] <- paste0('G',  Gt_vals[i], 'T', all.periods[j])
          store_dist_vals[k, 2] <- Gt_vals[i]
          store_dist_vals[k, 3] <- all.periods[j]
          store_dist_vals[k, 4] <- curr_dist  
        }
        k <- k + 1
      }
    }
    colnames(store_dist_vals) <- c('Label', 'G', 't', 'Dtg')
    long_dat$dist_f <- as.factor(long_dat$dist)
    long_dat$label_f <- as.factor(long_dat$label)
    
  }
  
  return(list(long_dat = long_dat, 
              wide_dat = wide_dat, 
              Gt = Gt, 
              store_dist_vals =  store_dist_vals))
  
}




