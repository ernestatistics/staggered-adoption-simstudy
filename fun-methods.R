

################################################################################
#### to do: define group.out and time.out so that we have all possible combinations 
#### starting on group = 1, time = 1, and so on until group = max(gt -1), and time = max(time)
###############################################################################

#library(devtools)
#install_github(repo = "jantonelli111/HeterogeneousTEpanel")
#library(HeterogeneousTEpanel)

# function calls:  
# did_wrapper(longdat, group, time, all_params$params.did)
# twfe_wrapper(longdat, all_params$params.twfe)

compare_rows <- function(matrix1, matrix2){
  # Initialize a logical vector to store the comparison results
  equal_rows <- logical(nrow(matrix1))
  
  # Iterate through each row of matrix1
  for (i in 1:nrow(matrix1)) {
    # Iterate through each row of matrix2
    for (j in 1:nrow(matrix2)) {
      # Compare the current rows from both matrices
      if (all(matrix1[i,] == matrix2[j,])) {
        equal_rows[i] <- TRUE
        break  # Found a match, move to the next row of matrix1
      }
    }
  }
  
  return(equal_rows)
}


# this sets methods with clustering at individual level (doing this for curiosity)
set_methods <- function(params_scen = NULL, scen){
  
  params.did.1 <- NULL
  params.did.2 <- NULL
  params.antonelli <- NULL
  params.twfe <- NULL
  
  #### Group time
  if(scen %in% c(1:4)){
    # no staggering
    group.out = c(5)
    time.out =  c(1,2,3,4,5)
  }else if(scen %in% c(0, 5:14, 50)){
    group.out = c(3,4,5)
    # Time begins at t = 1, so minimum time.out is 2 (so t-1 in DifinDif is 1)
    time.out =  c(1,2,3,4,5)
  }else if(scen %in% c(15:24, 30)){
    #### Group time
    # Time begins at t =1, so minimum time.out is 2 (so t-1 in DifinDif is 1)
    group.out = c(3,4,5)
    time.out =  c(1,2,3,4,5)
  }
  
  if(scen == 50){
    # no confounding 
    
    #### DID Regression
    params.did.0 <- list()
    params.did.0$clustervar <- 'id'
    params.did.0$ctgroup <- "notyettreated"
    params.did.0$estmthod <- 'reg'
    #params.did.1$form <- ~ X2 + X3 + X4 + V2 + V3 + V4 + V6
    params.did.0$form <- ~ X2 + X3 + X4
    params.did.0$unbalanced_panel <- FALSE 
    params.did.0$name <- 'Did Reg Not yet treated IndCov'
    
    #### DID
    params.did.1 <- list()
    params.did.1$clustervar <- ''
    params.did.1$ctgroup <- "notyettreated"
    params.did.1$estmthod <- 'reg'
    params.did.1$form <- ~ 1
    params.did.1$unbalanced_panel <- FALSE 
    params.did.1$name <- 'Dif in Dif Not yet treated Regression'
    
    #### DID Double Robust
    params.did.2 <- list()
    params.did.2$clustervar <- ''
    params.did.2$ctgroup  <-  "notyettreated"
    params.did.2$estmthod <- 'dr'
    params.did.2$form <- ~ 1
    params.did.2$unbalanced_panel <- FALSE 
    params.did.2$name <- 'Did DR Not yet treated'
    
    #### DID Weighting
    params.did.3 <- list()
    params.did.3$clustervar <- ''
    params.did.3$ctgroup  <-  "notyettreated"
    params.did.3$estmthod <- 'ipw'
    params.did.3$form <- ~ 1
    params.did.3$unbalanced_panel <- FALSE 
    params.did.3$name <- 'Did Wgt Not yet treated'
    
    #### TWFE
    params.twfe.1 <- list()
    params.twfe.1$form <- as.formula('Y ~ Gt_t')
    params.twfe.1$name <- 'TWFE'
    params.twfe.1$subset.data <- FALSE
    
    #### Dynamic TWFE
    params.twfe.2 <- list()
    params.twfe.2$form <- as.formula('Y ~ t_f + Gt_f + dist_f')
    params.twfe.2$subset.data <- FALSE
    params.twfe.2$name <- 'Dynamic TWFE'
    
    #### Two stage DID
    params.did2s.1 <- list()
    params.did2s.1$clustervar <- "id"
    params.did2s.1$trt_ind  <- "treat"
    params.did2s.1$outcomename <- "Y"
    params.did2s.1$form <-  ~ 0 | C + t_f
    params.did2s.1$name <- "TwoStageDid"
    
    #### Two Stage Did
    params.did2s.2 <- list()
    params.did2s.2$clustervar <- "id"
    params.did2s.2$trt_ind  <- "Gt_t"
    params.did2s.2$outcomename <- "Y"
    #params.did2s.1$form <-  ~ X2 + X3 + X4 + V2 + V3 + V4 + V6 | Gt_f + t_f
    #params.did2s.2$form <-  ~ X2 + X3 + X4 | Gt_f + t_f
    params.did2s.2$form <-  ~ X2 + X3 + X4 | C + t_f
    params.did2s.2$name <- "TwoStageDidIndCov"
    
    
    #### Two way Mundlack regression
    params.etwfe.1 <- list()
    params.etwfe.1$clustervar <- "id"
    params.etwfe.1$trt_ind  <- "treat"
    params.etwfe.1$outcomename <- "Y"
    params.etwfe.1$form <-  ~ t_f
    params.etwfe.1$name <- "TwoWayMundlackRegression"
    
    #### Two way Mundlack regression
    params.etwfe.1  <- list(
      clustervar = "id",
      tvar = "t",
      grp_ind  = "Gt",
      outcomename = "Y",
      form = "1", 
      name = "TwoWayMundlackRegression")
    
    #### Two way Mundlack regression
    params.etwfe.2  <- list(
      clustervar = "id",
      tvar = "t",
      grp_ind  = "Gt",
      outcomename = "Y",
      # form = "X2 + X3 + X4 + V2 + V3 + V4 + V6", 
      form = "X2 + X3 + X4", 
      name = "TwoWayMundlackRegressionIndCov")
    
    #### Sun and Abraham 
    params.sunab.1  <- list(form = Y ~ X2 + X3 + X4 + V2 + V3 + V6 + i(dist, i.Gt, ref = c(-1, -4)) | t_f, 
                            clustervar = eval(substitute(as.formula(paste("~", 'id')))), 
                            name = "sunabahamAllL")
    
    params.sunab.2  <-list(form = Y ~ i(dist, i.Gt, ref = c(-1, -4)) | X2 + X3 + X4 + V2 + V3 + V6 +  t_f, 
                           clustervar = eval(substitute(as.formula(paste("~", 'id')))), 
                           name = "sunabahamAllR")
    
    params.sunab.3  <-list(form = Y ~ X2 + X3 + X4 + i(dist, i.Gt, ref = c(-1, -4)) | t_f, 
                           clustervar = eval(substitute(as.formula(paste("~", 'id')))), 
                           name = "sunabahamIndCovL")
    
    params.sunab.4  <-list(form = Y ~ i(dist, i.Gt, ref = c(-1, -4)) | X2 + X3 + X4 + t_f, 
                           clustervar = eval(substitute(as.formula(paste("~", 'id')))), 
                           name = "sunabahamIndCovR")
    
    params.sunab.5  <-list(form = Y ~ V2 + V3 + V6 + i(dist, i.Gt, ref = c(-1, -4)) | t_f, 
                           clustervar = eval(substitute(as.formula(paste("~", 'id')))), 
                           name = "sunabahamClusterCovL")
    
    params.sunab.6  <-list(form = Y ~ i(dist, i.Gt, ref = c(-1, -4)) | V2 + V3 + V6 + t_f, 
                           clustervar = eval(substitute(as.formula(paste("~", 'id')))), 
                           name = "sunabahamClusterCovR")
    #### Antonelli
    # params.antonelli <- list()
    # params.antonelli$mthd1 <- list()
    # params.antonelli$mthd1$covars.cluster <- FALSE
    #   params.antonelli$mthd1$zeroMat = matrix(c(1,1,2,2), nrow=2)
    #   for (i in 1:(params_scen$np - 1)) {
    #     for (j in i:params_scen$np) {
    #       params.antonelli$mthd1$zeroMat = rbind(params.antonelli$mthd1$zeroMat, c(i,j))
    #     }
    #   }
    #   w = which(abs(params.antonelli$mthd1$zeroMat[,1] - params.antonelli$mthd1$zeroMat[,2]) < 1)
    #   params.antonelli$mthd1$zeroMat = params.antonelli$mthd1$zeroMat[-w,]
    # }
    #combinations <- combn(params_scen$np, 2)
    # Create the matrix with the desired columns
    #params.antonelli$mthd1$zeroMat <- t(combinations)
    
  } 
  
  if(scen %in% c(1:4)){
    
    # cluster level multinomial variable: V2-V5
    # cluster level urban-rural: V6
    
    #### DID Regression
    # Note: In low number of cluster scenarios we cannot adjust for cluster-level covariates 
    params.did.1 <- list()
    params.did.1$clustervar <- ''
    params.did.1$ctgroup <- "notyettreated"
    params.did.1$estmthod <- 'reg'
    # Removed V4 because multinomial only has 3 levels (previously 4)
    #params.did.1$form <- ~ X2 + X3 + X4 + V2 + V3 + V4 + V6
    params.did.1$form <- ~ X2 + X3 + X4 + V2 + V3 + V6
    params.did.1$unbalanced_panel <- FALSE  
    params.did.1$name <- 'Did Reg Not yet treated'
    
    #### DID Double Robust
    params.did.2 <- list()
    params.did.2$clustervar <- ''
    params.did.2$ctgroup  <-  "notyettreated"
    params.did.2$estmthod <- 'dr'
    # Removed V4 because multinomial only has 3 levels (previously 4)
    # params.did.2$form <- ~ X2 + X3 + X4 + V2 + V3 + V4 + V6
    params.did.2$form <- ~ X2 + X3 + X4 + V2 + V3 + V6
    params.did.2$unbalanced_panel <- FALSE 
    params.did.2$name <- 'Did DR Not yet treated'
    
    #### DID Weighting
    params.did.3 <- list()
    params.did.3$clustervar <- ''
    params.did.3$ctgroup  <-  "notyettreated"
    params.did.3$estmthod <- 'ipw'
    # Removed V4 because multinomial only has 3 levels (previously 4)
    # params.did.3$form <- ~ X2 + X3 + X4 + V2 + V3 + V4 + V6
    # params.did.3$form <- ~ X2 + X3 + X4 + V2 + V3 + V6
    # Removed individual covariates because variance is blowing up
    params.did.3$form <- ~  V2 + V3 + V6
    params.did.3$unbalanced_panel <- FALSE 
    params.did.3$name <- 'Did Wgt Not yet treated'
    
    #### TWFE
    params.twfe.1 <- list()
    params.twfe.1$name <- 'TWFE'
    params.twfe.1$subset.data <- FALSE
    # t_f = time fixed effect
    # Gt_f = group time fixed effect
    # Gt_t = indicator of treatment = estimate of Overall ATT 
    # params.twfe.1$form <- as.formula('Y ~ t_f + Gt_t + X2 + X3 + X4 +
    #                                 V2 + V3 + V4 + V6')
    params.twfe.1$form <- as.formula('Y ~ t_f + Gt_t + X2 + X3 + X4 +
                                     V2 + V3 + V6')
    
    #### TWFE Dynamic
    params.twfe.2 <- list()
    params.twfe.2$name <- 'Dynamic TWFE'
    params.twfe.2$subset.data <- FALSE
    # t_f = time fixed effect
    # Gt_f = group time fixed effect
    # dist_f = d_t = distance between time and group = e 
    # Coefficient of interest comes from the dist_f variable
    # params.twfe.2$form <- as.formula('Y ~ t_f + dist_f + X2 + X3 + X4 +
    #                                  V2 + V3 + V4 + V6')
    params.twfe.2$form <- as.formula('Y ~ t_f + dist_f + X2 + X3 + X4 +
                                     V2 + V3 + V6')
    
    #### Two Stage Did
    params.did2s.1 <- list()
    params.did2s.1$clustervar <- 'id'
    params.did2s.1$trt_ind  <- "Gt_t"
    params.did2s.1$outcomename <- "Y"
    # params.did2s.1$form <- ~ X2 + X3 + X4 + V2 + V3 + V4 + V6 | t_f
    # params.did2s.1$form <- ~ X2 + X3 + X4 + V2 + V3 + V6 | t_f
    params.did2s.1$form <- ~ X2 + X3 + X4 + V2 + V3 + V6 | C + t_f
    params.did2s.1$name <- "TwoStageDid"
    
    #### Two Stage Did
    params.did2s.2 <- list()
    params.did2s.2$clustervar <- 'id'
    params.did2s.2$trt_ind  <- "Gt_t"
    params.did2s.2$outcomename <- "Y"
    #params.did2s.1$form <-  ~ X2 + X3 + X4 + V2 + V3 + V4 + V6 | Gt_f + t_f
    #params.did2s.2$form <-  ~ X2 + X3 + X4 | Gt_f + t_f
    params.did2s.2$form <-  ~ X2 + X3 + X4 | C + t_f
    params.did2s.2$name <- "TwoStageDidIndCov"
    
    
    #### Two way Mundlack regression
    params.etwfe.1  <- list(
      clustervar = 'id',
      tvar = "t",
      grp_ind  = "Gt",
      outcomename = "Y",
      #form = "X2 + X3 + X4 + V2 + V3 + V4 + V6", 
      form = "X2 + X3 + X4 + V2 + V3 + V6", 
      name = "TwoWayMundlackRegression")
    
    #### Sun and Abraham 
    params.sunab.1  <- list(form = Y ~ X2 + X3 + X4 + V2 + V3 + V6 + i(dist, i.Gt, ref = c(-1, -4)) | t_f, 
                            clustervar = eval(substitute(as.formula(paste("~", 'C')))), 
                            name = "sunabahamAllL")
    
    params.sunab.2  <-list(form = Y ~ i(dist, i.Gt, ref = c(-1, -4)) | X2 + X3 + X4 + V2 + V3 + V6 +  t_f, 
                           clustervar = eval(substitute(as.formula(paste("~", 'C')))), 
                           name = "sunabahamAllR")
    
    params.sunab.3  <-list(form = Y ~ X2 + X3 + X4 + i(dist, i.Gt, ref = c(-1, -4)) | t_f, 
                           clustervar = eval(substitute(as.formula(paste("~", 'C')))), 
                           name = "sunabahamIndCovL")
    
    params.sunab.4  <-list(form = Y ~ i(dist, i.Gt, ref = c(-1, -4)) | X2 + X3 + X4 + t_f, 
                           clustervar = eval(substitute(as.formula(paste("~", 'C')))), 
                           name = "sunabahamIndCovR")
    
    params.sunab.5  <-list(form = Y ~ V2 + V3 + V6 + i(dist, i.Gt, ref = c(-1, -4)) | t_f, 
                           clustervar = eval(substitute(as.formula(paste("~", 'C')))), 
                           name = "sunabahamClusterCovL")
    
    params.sunab.6  <-list(form = Y ~ i(dist, i.Gt, ref = c(-1, -4)) | V2 + V3 + V6 + t_f, 
                           clustervar = eval(substitute(as.formula(paste("~", 'C')))), 
                           name = "sunabahamClusterCovR")
    
    
  }
  
  if(scen %in% c(0, 5:14)){
    
    #### DID Regression
    params.did.0 <- list()
    params.did.0$clustervar <- 'id'
    params.did.0$ctgroup <- "notyettreated"
    params.did.0$estmthod <- 'reg'
    #params.did.1$form <- ~ X2 + X3 + X4 + V2 + V3 + V4 + V6
    params.did.0$form <- ~ X2 + X3 + X4
    params.did.0$unbalanced_panel <- FALSE 
    params.did.0$name <- 'Did Reg Not yet treated IndCov'
    
    #### DID Regression
    params.did.1 <- list()
    params.did.1$clustervar <- 'id'
    params.did.1$ctgroup <- "notyettreated"
    params.did.1$estmthod <- 'reg'
    #params.did.1$form <- ~ X2 + X3 + X4 + V2 + V3 + V4 + V6
    params.did.1$form <- ~ X2 + X3 + X4 + V2 + V3 + V6
    params.did.1$unbalanced_panel <- FALSE 
    params.did.1$name <- 'Did Reg Not yet treated'
    
    #### DID Double Robust
    params.did.2 <- list()
    params.did.2$clustervar <- 'id'
    params.did.2$ctgroup  <-  "notyettreated"
    params.did.2$estmthod <- 'dr'
    #params.did.2$form <- ~ X2 + X3 + X4 + V2 + V3 + V4 + V6
    params.did.2$form <- ~ X2 + X3 + X4 + V2 + V3 + V6
    params.did.2$unbalanced_panel <- FALSE 
    params.did.2$name <- 'Did DR Not yet treated'
    
    #### DID Weighting
    params.did.3 <- list()
    params.did.3$clustervar <- 'id'
    params.did.3$ctgroup  <-  "notyettreated"
    params.did.3$estmthod <- 'ipw'
    #params.did.3$form <- ~ X2 + X3 + X4 + V2 + V3 + V4 + V6
    #params.did.1$form <- ~ X2 + X3 + X4 + V2 + V3 + V6
    params.did.3$form <- ~ V2 + V3 + V6
    params.did.3$unbalanced_panel <- FALSE 
    params.did.3$name <- 'Did Wgt Not yet treated'
    
    #### TWFE
    params.twfe.1 <- list()
    params.twfe.1$name <- 'TWFE'
    params.twfe.1$subset.data <- FALSE
    # t_f = time fixed effect
    # Gt_f = group time fixed effect
    # Gt_t = indicator of treatment = estimate of Overall ATT 
    # params.twfe.1$form <- as.formula('Y ~ t_f + Gt_f + Gt_t + X2 + X3 + X4 +
    #                                 V2 + V3 + V4 + V6')
    params.twfe.1$form <- as.formula('Y ~ t_f + Gt_f + Gt_t + X2 + X3 + X4 + V2 + V3 + V6')
    
    #### TWFE Dynamic
    params.twfe.2 <- list()
    params.twfe.2$name <- 'Dynamic TWFE'
    params.twfe.2$subset.data <- FALSE
    # t_f = time fixed effect
    # Gt_f = group time fixed effect
    # dist_f = d_t = distance between time and group = e 
    # Coefficient of interest comes from the dist_f variable
    # params.twfe.2$form <- as.formula('Y ~ t_f + Gt_f + dist_f + X2 + X3 + X4 +
    #                                 V2 + V3 + V4 + V6')
    params.twfe.2$form <- as.formula('Y ~ t_f + Gt_f + dist_f + X2 + X3 + X4 + V2 + V3 + V6')
    
    #### Two Stage Did
    params.did2s.1 <- list()
    params.did2s.1$clustervar <- "id"
    params.did2s.1$trt_ind  <- "Gt_t"
    params.did2s.1$outcomename <- "Y"
    # params.did2s.1$form <- ~ X2 + X3 + X4 + V2 + V3 + V4 + V6 | Gt_f + t_f
    params.did2s.1$form <- ~  X2 + X3 + X4 + V2 + V3 + V6 | C + t_f
    params.did2s.1$name <- "TwoStageDid"
    
    #### Two Stage Did
    params.did2s.2 <- list()
    params.did2s.2$clustervar <- "id"
    params.did2s.2$trt_ind  <- "Gt_t"
    params.did2s.2$outcomename <- "Y"
    # params.did2s.1$form <- ~ X2 + X3 + X4 + V2 + V3 + V4 + V6 | Gt_f + t_f
    params.did2s.2$form <- ~ X2 + X3 + X4 | C + t_f
    params.did2s.2$name <- "TwoStageDidIndCov"
    
    #### Two way Mundlack regression
    params.etwfe.1  <- list(
      clustervar = "id",
      tvar = "t",
      grp_ind  = "Gt",
      outcomename = "Y",
      # form = "X2 + X3 + X4 + V2 + V3 + V4 + V6", 
      form = "X2 + X3 + X4 + V2 + V3 + V6", 
      name = "TwoWayMundlackRegression")
    
    params.etwfe.2  <- list(
      clustervar = "id",
      tvar = "t",
      grp_ind  = "Gt",
      outcomename = "Y",
      # form = "X2 + X3 + X4 + V2 + V3 + V4 + V6", 
      form = "X2 + X3 + X4", 
      name = "TwoWayMundlackRegressionIndCov")
    
    #### Sun and Abraham 
    
    params.sunab.1  <- list(form = Y ~ X2 + X3 + X4 + V2 + V3 + V6 + i(dist, i.Gt, ref = c(-1, -4)) | t_f, 
                            clustervar = eval(substitute(as.formula(paste("~", 'id')))), 
                            name = "sunabahamAllL")
    
    params.sunab.2  <-list(form = Y ~ i(dist, i.Gt, ref = c(-1, -4)) | X2 + X3 + X4 + V2 + V3 + V6 +  t_f, 
                           clustervar = eval(substitute(as.formula(paste("~", 'id')))), 
                           name = "sunabahamAllR")
    
    params.sunab.3  <-list(form = Y ~ X2 + X3 + X4 + i(dist, i.Gt, ref = c(-1, -4)) | t_f, 
                           clustervar = eval(substitute(as.formula(paste("~", 'id')))), 
                           name = "sunabahamIndCovL")
    
    params.sunab.4  <-list(form = Y ~ i(dist, i.Gt, ref = c(-1, -4)) | X2 + X3 + X4 + t_f, 
                           clustervar = eval(substitute(as.formula(paste("~", 'id')))), 
                           name = "sunabahamIndCovR")
    
    params.sunab.5  <-list(form = Y ~ V2 + V3 + V6 + i(dist, i.Gt, ref = c(-1, -4)) | t_f, 
                           clustervar = eval(substitute(as.formula(paste("~", 'id')))), 
                           name = "sunabahamClusterCovL")
    
    params.sunab.6  <-list(form = Y ~ i(dist, i.Gt, ref = c(-1, -4)) | V2 + V3 + V6 + t_f, 
                           clustervar = eval(substitute(as.formula(paste("~", 'id')))), 
                           name = "sunabahamClusterCovR")
    
  }
  
  if(scen %in% c(15:24,30)){
    
    #### DID Regression
    params.did.0 <- list()
    params.did.0$clustervar <- 'id'
    params.did.0$ctgroup <- "notyettreated"
    params.did.0$estmthod <- 'reg'
    #params.did.1$form <- ~ X2 + X3 + X4 + V2 + V3 + V4 + V6
    params.did.0$form <- ~ X2 + X3 + X4
    params.did.0$unbalanced_panel <- FALSE  
    params.did.0$name <- 'Did Reg Not yet treated IndCov'
    
    #### DID Regression
    params.did.1 <- list()
    params.did.1$clustervar <- 'id'
    params.did.1$ctgroup <- "notyettreated"
    params.did.1$estmthod <- 'reg'
    #params.did.1$form <- ~ X2 + X3 + X4 + V2 + V3 + V4 + V6
    params.did.1$form <- ~ X2 + X3 + X4 + V2 + V3 + V6
    params.did.1$unbalanced_panel <- FALSE 
    params.did.1$name <- 'Did Reg Not yet treated AllCov'
    
    #### DID Double Robust
    params.did.2 <- list()
    params.did.2$clustervar <- 'id'
    params.did.2$ctgroup  <-  "notyettreated"
    params.did.2$estmthod <- 'dr'
    #params.did.2$form <- ~ X2 + X3 + X4 + V2 + V3 + V4 + V6
    params.did.2$form <- ~ X2 + X3 + X4 + V2 + V3 + V6
    params.did.2$unbalanced_panel <- FALSE 
    params.did.2$name <- 'Did DR Not yet treated'
    
    #### DID Weighting
    params.did.3 <- list()
    params.did.3$clustervar <- 'id'
    params.did.3$ctgroup  <-  "notyettreated"
    params.did.3$estmthod <- 'ipw'
    #params.did.3$form <- ~ X2 + X3 + X4 + V2 + V3 + V4 + V6
    #params.did.3$form <- ~ X2 + X3 + X4 + V2 + V3 + V6
    params.did.3$form <- ~ V2 + V3 + V6
    params.did.3$unbalanced_panel <- FALSE 
    params.did.3$name <- 'Did Wgt Not yet treated'
    
    #### TWFE
    params.twfe.1 <- list()
    # params.twfe.1$form <- as.formula('Y ~ t_f + Gt_f + Gt_t + X2 + X3 + X4 + 
    #                                  V2 + V3 + V4 + V6')
    params.twfe.1$form <- as.formula('Y ~ t_f + Gt_f + Gt_t + X2 + X3 + X4 + 
                                     V2 + V3 + V6')
    params.twfe.1$name <- 'TWFE'
    params.twfe.1$subset.data <- FALSE
    
    #### Dynamic TWFE
    params.twfe.2 <- list()
    # params.twfe.2$form <- as.formula('Y ~ t_f + Gt_f + dist_f + X2 + X3 + X4 + 
    #                                  V2 + V3 + V4 + V6')
    params.twfe.2$form <- as.formula('Y ~ t_f + Gt_f + dist_f + X2 + X3 + X4 + 
                                     V2 + V3 + V6')
    params.twfe.2$name <- 'Dynamic TWFE'
    params.twfe.2$subset.data <- FALSE
    
    #### Two Stage Did
    params.did2s.1 <- list()
    params.did2s.1$clustervar <- 'id'
    params.did2s.1$trt_ind  <- "Gt_t"
    params.did2s.1$outcomename <- "Y"
    #params.did2s.1$form <-  ~ X2 + X3 + X4 + V2 + V3 + V4 + V6 | Gt_f + t_f
    #params.did2s.1$form <-  ~ X2 + X3 + X4 + V2 + V3 + V6 | Gt_f + t_f
    params.did2s.1$form <-  ~ X2 + X3 + X4 + V2 + V3 + V6 | C + t_f
    params.did2s.1$name <- "TwoStageDid"
    
    #### Two Stage Did
    params.did2s.2 <- list()
    params.did2s.2$clustervar <- 'id'
    params.did2s.2$trt_ind  <- "Gt_t"
    params.did2s.2$outcomename <- "Y"
    #params.did2s.1$form <-  ~ X2 + X3 + X4 + V2 + V3 + V4 + V6 | Gt_f + t_f
    #params.did2s.2$form <-  ~ X2 + X3 + X4 | Gt_f + t_f
    params.did2s.2$form <-  ~ X2 + X3 + X4 | C + t_f
    params.did2s.2$name <- "TwoStageDidIndCov"
    
    #### Two way Mundlack regression
    params.etwfe.1  <- list(
      clustervar = 'id',
      tvar = "t",
      grp_ind  = "Gt",
      outcomename = "Y",
      # form = "X2 + X3 + X4 + V2 + V3 + V4 + V6", 
      form = "X2 + X3 + X4 + V2 + V3 + V6", 
      name = "TwoWayMundlackRegression")
    
    #### Two way Mundlack regression
    params.etwfe.2  <- list(
      clustervar = 'id',
      tvar = "t",
      grp_ind  = "Gt",
      outcomename = "Y",
      # form = "X2 + X3 + X4 + V2 + V3 + V4 + V6", 
      form = "X2 + X3 + X4", 
      name = "TwoWayMundlackRegressionIndCov")
    
    #### Sun and Abraham 
    params.sunab.1  <- list(form = Y ~ X2 + X3 + X4 + V2 + V3 + V6 + i(dist, i.Gt, ref = c(-1, -4)) | t_f, 
                            clustervar = eval(substitute(as.formula(paste("~", 'C')))), 
                            name = "sunabahamAllL")
    
    params.sunab.2  <-list(form = Y ~ i(dist, i.Gt, ref = c(-1, -4)) | X2 + X3 + X4 + V2 + V3 + V6 +  t_f, 
                           clustervar = eval(substitute(as.formula(paste("~", 'C')))), 
                           name = "sunabahamAllR")
    
    params.sunab.3  <-list(form = Y ~ X2 + X3 + X4 + i(dist, i.Gt, ref = c(-1, -4)) | t_f, 
                           clustervar = eval(substitute(as.formula(paste("~", 'C')))), 
                           name = "sunabahamIndCovL")
    
    params.sunab.4  <-list(form = Y ~ i(dist, i.Gt, ref = c(-1, -4)) | X2 + X3 + X4 + t_f, 
                           clustervar = eval(substitute(as.formula(paste("~", 'C')))), 
                           name = "sunabahamIndCovR")
    
    params.sunab.5  <-list(form = Y ~ V2 + V3 + V6 + i(dist, i.Gt, ref = c(-1, -4)) | t_f, 
                           clustervar = eval(substitute(as.formula(paste("~", 'C')))), 
                           name = "sunabahamClusterCovL")
    
    params.sunab.6  <-list(form = Y ~ i(dist, i.Gt, ref = c(-1, -4)) | V2 + V3 + V6 + t_f, 
                           clustervar = eval(substitute(as.formula(paste("~", 'C')))), 
                           name = "sunabahamClusterCovR")
    
  }
  
  return(list(group.out = group.out, 
              time.out = time.out, 
              params.did = list(params.did.0 = params.did.0, 
                                params.did.1 = params.did.1, 
                                params.did.2 = params.did.2, 
                                params.did.3 = params.did.3),
              params.did2s = list(params.did2s.1 = params.did2s.1, 
                                  params.did2s.2 = params.did2s.2),
              params.twfe = list(params.twfe.1 = params.twfe.1, 
                                 params.twfe.2 = params.twfe.2),
              params.etwfe = list(params.etwfe.1 = params.etwfe.1, 
                                  params.etwfe.2 = params.etwfe.2),
              params.sunab = list(params.sunab.1 = params.sunab.1, 
                                  params.sunab.2 = params.sunab.2, 
                                  params.sunab.3 = params.sunab.3,
                                  params.sunab.4 = params.sunab.4,
                                  params.sunab.5 = params.sunab.5,
                                  params.sunab.6 = params.sunab.6),
              params.antonelli = params.antonelli))
  
}

# this sets methods with clustering at cluster level
set_methods_c <- function(params_scen = NULL, scen){
  
  params.did.1 <- NULL
  params.did.2 <- NULL
  params.antonelli <- NULL
  params.twfe <- NULL
  
  #### Group time
  if(scen %in% c(1:4)){
    # no staggering
    group.out = c(5)
    time.out =  c(1,2,3,4,5)
  }else if(scen %in% c(0, 5:14, 50)){
    group.out = c(3,4,5)
    # Time begins at t = 1, so minimum time.out is 2 (so t-1 in DifinDif is 1)
    time.out =  c(1,2,3,4,5)
  }else if(scen %in% c(15:24, 30)){
    #### Group time
    # Time begins at t =1, so minimum time.out is 2 (so t-1 in DifinDif is 1)
    group.out = c(3,4,5)
    time.out =  c(1,2,3,4,5)
  }
  
  if(scen == 50){
    # no confounding 
    
    #### DID Regression
    params.did.0 <- list()
    params.did.0$clustervar <- 'C'
    params.did.0$ctgroup <- "notyettreated"
    params.did.0$estmthod <- 'reg'
    #params.did.1$form <- ~ X2 + X3 + X4 + V2 + V3 + V4 + V6
    params.did.0$form <- ~ X2 + X3 + X4
    params.did.0$unbalanced_panel <- FALSE 
    params.did.0$name <- 'Did Reg Not yet treated IndCov'
    
    #### DID
    params.did.1 <- list()
    params.did.1$clustervar <- ''
    params.did.1$ctgroup <- "notyettreated"
    params.did.1$estmthod <- 'reg'
    params.did.1$form <- ~ 1
    params.did.1$unbalanced_panel <- FALSE 
    params.did.1$name <- 'Dif in Dif Not yet treated Regression'
    
    #### DID Double Robust
    params.did.2 <- list()
    params.did.2$clustervar <- ''
    params.did.2$ctgroup  <-  "notyettreated"
    params.did.2$estmthod <- 'dr'
    params.did.2$form <- ~ 1
    params.did.2$unbalanced_panel <- FALSE 
    params.did.2$name <- 'Did DR Not yet treated'
    
    #### DID Weighting
    params.did.3 <- list()
    params.did.3$clustervar <- ''
    params.did.3$ctgroup  <-  "notyettreated"
    params.did.3$estmthod <- 'ipw'
    params.did.3$form <- ~ 1
    params.did.3$unbalanced_panel <- FALSE 
    params.did.3$name <- 'Did Wgt Not yet treated'
    
    #### TWFE
    params.twfe.1 <- list()
    params.twfe.1$form <- as.formula('Y ~ Gt_t')
    params.twfe.1$name <- 'TWFE'
    params.twfe.1$subset.data <- FALSE
    
    #### Dynamic TWFE
    params.twfe.2 <- list()
    params.twfe.2$form <- as.formula('Y ~ t_f + Gt_f + dist_f')
    params.twfe.2$subset.data <- FALSE
    params.twfe.2$name <- 'Dynamic TWFE'
    
    #### Two stage DID
    params.did2s.1 <- list()
    params.did2s.1$clustervar <- "C"
    params.did2s.1$trt_ind  <- "treat"
    params.did2s.1$outcomename <- "Y"
    params.did2s.1$form <-  ~ 0 | C + t_f
    params.did2s.1$name <- "TwoStageDid"
    
    #### Two Stage Did
    params.did2s.2 <- list()
    params.did2s.2$clustervar <- "C"
    params.did2s.2$trt_ind  <- "Gt_t"
    params.did2s.2$outcomename <- "Y"
    #params.did2s.1$form <-  ~ X2 + X3 + X4 + V2 + V3 + V4 + V6 | Gt_f + t_f
    #params.did2s.2$form <-  ~ X2 + X3 + X4 | Gt_f + t_f
    params.did2s.2$form <-  ~ X2 + X3 + X4 | C + t_f
    params.did2s.2$name <- "TwoStageDidIndCov"
    
    
    #### Two way Mundlack regression
    params.etwfe.1 <- list()
    params.etwfe.1$clustervar <- "C"
    params.etwfe.1$trt_ind  <- "treat"
    params.etwfe.1$outcomename <- "Y"
    params.etwfe.1$form <-  ~ t_f
    params.etwfe.1$name <- "TwoWayMundlackRegression"
    
    #### Two way Mundlack regression
    params.etwfe.1  <- list(
      clustervar = "C",
      tvar = "t",
      grp_ind  = "Gt",
      outcomename = "Y",
      form = "1", 
      name = "TwoWayMundlackRegression")
    
    #### Two way Mundlack regression
    params.etwfe.2  <- list(
      clustervar = "C",
      tvar = "t",
      grp_ind  = "Gt",
      outcomename = "Y",
      # form = "X2 + X3 + X4 + V2 + V3 + V4 + V6", 
      form = "X2 + X3 + X4", 
      name = "TwoWayMundlackRegressionIndCov")
    
    #### Sun and Abraham 
    params.sunab.1  <- list(form = Y ~ X2 + X3 + X4 + V2 + V3 + V6 + i(dist, i.Gt, ref = c(-1, -4)) | t_f, 
                            clustervar = eval(substitute(as.formula(paste("~", 'C')))), 
                            name = "sunabahamAllL")
    
    params.sunab.2  <-list(form = Y ~ i(dist, i.Gt, ref = c(-1, -4)) | X2 + X3 + X4 + V2 + V3 + V6 +  t_f, 
                           clustervar = eval(substitute(as.formula(paste("~", 'C')))), 
                           name = "sunabahamAllR")
    
    params.sunab.3  <-list(form = Y ~ X2 + X3 + X4 + i(dist, i.Gt, ref = c(-1, -4)) | t_f, 
                           clustervar = eval(substitute(as.formula(paste("~", 'C')))), 
                           name = "sunabahamIndCovL")
    
    params.sunab.4  <-list(form = Y ~ i(dist, i.Gt, ref = c(-1, -4)) | X2 + X3 + X4 + t_f, 
                           clustervar = eval(substitute(as.formula(paste("~", 'C')))), 
                           name = "sunabahamIndCovR")
    
    params.sunab.5  <-list(form = Y ~ V2 + V3 + V6 + i(dist, i.Gt, ref = c(-1, -4)) | t_f, 
                           clustervar = eval(substitute(as.formula(paste("~", 'C')))), 
                           name = "sunabahamClusterCovL")
    
    params.sunab.6  <-list(form = Y ~ i(dist, i.Gt, ref = c(-1, -4)) | V2 + V3 + V6 + t_f, 
                           clustervar = eval(substitute(as.formula(paste("~", 'C')))), 
                           name = "sunabahamClusterCovR")
    #### Antonelli
    # params.antonelli <- list()
    # params.antonelli$mthd1 <- list()
    # params.antonelli$mthd1$covars.cluster <- FALSE
    #   params.antonelli$mthd1$zeroMat = matrix(c(1,1,2,2), nrow=2)
    #   for (i in 1:(params_scen$np - 1)) {
    #     for (j in i:params_scen$np) {
    #       params.antonelli$mthd1$zeroMat = rbind(params.antonelli$mthd1$zeroMat, c(i,j))
    #     }
    #   }
    #   w = which(abs(params.antonelli$mthd1$zeroMat[,1] - params.antonelli$mthd1$zeroMat[,2]) < 1)
    #   params.antonelli$mthd1$zeroMat = params.antonelli$mthd1$zeroMat[-w,]
    # }
    #combinations <- combn(params_scen$np, 2)
    # Create the matrix with the desired columns
    #params.antonelli$mthd1$zeroMat <- t(combinations)
    
  } 
  
  if(scen %in% c(1:4)){
    
    # cluster level multinomial variable: V2-V5
    # cluster level urban-rural: V6
    
    #### DID Regression
    # Note: In low number of cluster scenarios we cannot adjust for cluster-level covariates 
    params.did.1 <- list()
    params.did.1$clustervar <- ''
    params.did.1$ctgroup <- "notyettreated"
    params.did.1$estmthod <- 'reg'
    # Removed V4 because multinomial only has 3 levels (previously 4)
    #params.did.1$form <- ~ X2 + X3 + X4 + V2 + V3 + V4 + V6
    params.did.1$form <- ~ X2 + X3 + X4 + V2 + V3 + V6
    params.did.1$unbalanced_panel <- FALSE  
    params.did.1$name <- 'Did Reg Not yet treated'
    
    #### DID Double Robust
    params.did.2 <- list()
    params.did.2$clustervar <- ''
    params.did.2$ctgroup  <-  "notyettreated"
    params.did.2$estmthod <- 'dr'
    # Removed V4 because multinomial only has 3 levels (previously 4)
    # params.did.2$form <- ~ X2 + X3 + X4 + V2 + V3 + V4 + V6
    params.did.2$form <- ~ X2 + X3 + X4 + V2 + V3 + V6
    params.did.2$unbalanced_panel <- FALSE 
    params.did.2$name <- 'Did DR Not yet treated'
    
    #### DID Weighting
    params.did.3 <- list()
    params.did.3$clustervar <- ''
    params.did.3$ctgroup  <-  "notyettreated"
    params.did.3$estmthod <- 'ipw'
    # Removed V4 because multinomial only has 3 levels (previously 4)
    # params.did.3$form <- ~ X2 + X3 + X4 + V2 + V3 + V4 + V6
    # params.did.3$form <- ~ X2 + X3 + X4 + V2 + V3 + V6
    # Removed individual covariates because variance is blowing up
    params.did.3$form <- ~  V2 + V3 + V6
    params.did.3$unbalanced_panel <- FALSE 
    params.did.3$name <- 'Did Wgt Not yet treated'
    
    #### TWFE
    params.twfe.1 <- list()
    params.twfe.1$name <- 'TWFE'
    params.twfe.1$subset.data <- FALSE
    # t_f = time fixed effect
    # Gt_f = group time fixed effect
    # Gt_t = indicator of treatment = estimate of Overall ATT 
    # params.twfe.1$form <- as.formula('Y ~ t_f + Gt_t + X2 + X3 + X4 +
    #                                 V2 + V3 + V4 + V6')
    params.twfe.1$form <- as.formula('Y ~ t_f + Gt_t + X2 + X3 + X4 +
                                     V2 + V3 + V6')
    
    #### TWFE Dynamic
    params.twfe.2 <- list()
    params.twfe.2$name <- 'Dynamic TWFE'
    params.twfe.2$subset.data <- FALSE
    # t_f = time fixed effect
    # Gt_f = group time fixed effect
    # dist_f = d_t = distance between time and group = e 
    # Coefficient of interest comes from the dist_f variable
    # params.twfe.2$form <- as.formula('Y ~ t_f + dist_f + X2 + X3 + X4 +
    #                                  V2 + V3 + V4 + V6')
    params.twfe.2$form <- as.formula('Y ~ t_f + dist_f + X2 + X3 + X4 +
                                     V2 + V3 + V6')
    
    #### Two Stage Did
    params.did2s.1 <- list()
    params.did2s.1$clustervar <- "C"
    params.did2s.1$trt_ind  <- "Gt_t"
    params.did2s.1$outcomename <- "Y"
    # params.did2s.1$form <- ~ X2 + X3 + X4 + V2 + V3 + V4 + V6 | t_f
    # params.did2s.1$form <- ~ X2 + X3 + X4 + V2 + V3 + V6 | t_f
    params.did2s.1$form <- ~ X2 + X3 + X4 + V2 + V3 + V6 | C + t_f
    params.did2s.1$name <- "TwoStageDid"
    
    #### Two Stage Did
    params.did2s.2 <- list()
    params.did2s.2$clustervar <- "C"
    params.did2s.2$trt_ind  <- "Gt_t"
    params.did2s.2$outcomename <- "Y"
    #params.did2s.1$form <-  ~ X2 + X3 + X4 + V2 + V3 + V4 + V6 | Gt_f + t_f
    #params.did2s.2$form <-  ~ X2 + X3 + X4 | Gt_f + t_f
    params.did2s.2$form <-  ~ X2 + X3 + X4 | C + t_f
    params.did2s.2$name <- "TwoStageDidIndCov"
    
    
    #### Two way Mundlack regression
    params.etwfe.1  <- list(
      clustervar = "C",
      tvar = "t",
      grp_ind  = "Gt",
      outcomename = "Y",
      #form = "X2 + X3 + X4 + V2 + V3 + V4 + V6", 
      form = "X2 + X3 + X4 + V2 + V3 + V6", 
      name = "TwoWayMundlackRegression")
    
    #### Sun and Abraham 
    params.sunab.1  <- list(form = Y ~ X2 + X3 + X4 + V2 + V3 + V6 + i(dist, i.Gt, ref = c(-1, -4)) | t_f, 
                            clustervar = eval(substitute(as.formula(paste("~", 'C')))), 
                            name = "sunabahamAllL")
    
    params.sunab.2  <-list(form = Y ~ i(dist, i.Gt, ref = c(-1, -4)) | X2 + X3 + X4 + V2 + V3 + V6 +  t_f, 
                           clustervar = eval(substitute(as.formula(paste("~", 'C')))), 
                           name = "sunabahamAllR")
    
    params.sunab.3  <-list(form = Y ~ X2 + X3 + X4 + i(dist, i.Gt, ref = c(-1, -4)) | t_f, 
                           clustervar = eval(substitute(as.formula(paste("~", 'C')))), 
                           name = "sunabahamIndCovL")
    
    params.sunab.4  <-list(form = Y ~ i(dist, i.Gt, ref = c(-1, -4)) | X2 + X3 + X4 + t_f, 
                           clustervar = eval(substitute(as.formula(paste("~", 'C')))), 
                           name = "sunabahamIndCovR")
    
    params.sunab.5  <-list(form = Y ~ V2 + V3 + V6 + i(dist, i.Gt, ref = c(-1, -4)) | t_f, 
                           clustervar = eval(substitute(as.formula(paste("~", 'C')))), 
                           name = "sunabahamClusterCovL")
    
    params.sunab.6  <-list(form = Y ~ i(dist, i.Gt, ref = c(-1, -4)) | V2 + V3 + V6 + t_f, 
                           clustervar = eval(substitute(as.formula(paste("~", 'C')))), 
                           name = "sunabahamClusterCovR")
    
    
  }
  
  if(scen %in% c(0, 5:14)){
    
    #### DID Regression
    params.did.0 <- list()
    params.did.0$clustervar <- 'C'
    params.did.0$ctgroup <- "notyettreated"
    params.did.0$estmthod <- 'reg'
    #params.did.1$form <- ~ X2 + X3 + X4 + V2 + V3 + V4 + V6
    params.did.0$form <- ~ X2 + X3 + X4
    params.did.0$unbalanced_panel <- FALSE 
    params.did.0$name <- 'Did Reg Not yet treated IndCov'
    
    #### DID Regression
    params.did.1 <- list()
    params.did.1$clustervar <- 'C'
    params.did.1$ctgroup <- "notyettreated"
    params.did.1$estmthod <- 'reg'
    #params.did.1$form <- ~ X2 + X3 + X4 + V2 + V3 + V4 + V6
    params.did.1$form <- ~ X2 + X3 + X4 + V2 + V3 + V6
    params.did.1$unbalanced_panel <- FALSE 
    params.did.1$name <- 'Did Reg Not yet treated'
    
    #### DID Double Robust
    params.did.2 <- list()
    params.did.2$clustervar <- 'C'
    params.did.2$ctgroup  <-  "notyettreated"
    params.did.2$estmthod <- 'dr'
    #params.did.2$form <- ~ X2 + X3 + X4 + V2 + V3 + V4 + V6
    params.did.2$form <- ~ X2 + X3 + X4 + V2 + V3 + V6
    params.did.2$unbalanced_panel <- FALSE 
    params.did.2$name <- 'Did DR Not yet treated'
    
    #### DID Weighting
    params.did.3 <- list()
    params.did.3$clustervar <- 'C'
    params.did.3$ctgroup  <-  "notyettreated"
    params.did.3$estmthod <- 'ipw'
    #params.did.3$form <- ~ X2 + X3 + X4 + V2 + V3 + V4 + V6
    #params.did.1$form <- ~ X2 + X3 + X4 + V2 + V3 + V6
    params.did.3$form <- ~ V2 + V3 + V6
    params.did.3$unbalanced_panel <- FALSE 
    params.did.3$name <- 'Did Wgt Not yet treated'
    
    #### TWFE
    params.twfe.1 <- list()
    params.twfe.1$name <- 'TWFE'
    params.twfe.1$subset.data <- FALSE
    # t_f = time fixed effect
    # Gt_f = group time fixed effect
    # Gt_t = indicator of treatment = estimate of Overall ATT 
    # params.twfe.1$form <- as.formula('Y ~ t_f + Gt_f + Gt_t + X2 + X3 + X4 +
    #                                 V2 + V3 + V4 + V6')
    params.twfe.1$form <- as.formula('Y ~ t_f + Gt_f + Gt_t + X2 + X3 + X4 + V2 + V3 + V6')
    
    #### TWFE Dynamic
    params.twfe.2 <- list()
    params.twfe.2$name <- 'Dynamic TWFE'
    params.twfe.2$subset.data <- FALSE
    # t_f = time fixed effect
    # Gt_f = group time fixed effect
    # dist_f = d_t = distance between time and group = e 
    # Coefficient of interest comes from the dist_f variable
    # params.twfe.2$form <- as.formula('Y ~ t_f + Gt_f + dist_f + X2 + X3 + X4 +
    #                                 V2 + V3 + V4 + V6')
    params.twfe.2$form <- as.formula('Y ~ t_f + Gt_f + dist_f + X2 + X3 + X4 + V2 + V3 + V6')
    
    #### Two Stage Did
    params.did2s.1 <- list()
    params.did2s.1$clustervar <- "C"
    params.did2s.1$trt_ind  <- "Gt_t"
    params.did2s.1$outcomename <- "Y"
    # params.did2s.1$form <- ~ X2 + X3 + X4 + V2 + V3 + V4 + V6 | Gt_f + t_f
    params.did2s.1$form <- ~  X2 + X3 + X4 + V2 + V3 + V6 | C + t_f
    params.did2s.1$name <- "TwoStageDid"
    
    #### Two Stage Did
    params.did2s.2 <- list()
    params.did2s.2$clustervar <- "C"
    params.did2s.2$trt_ind  <- "Gt_t"
    params.did2s.2$outcomename <- "Y"
    # params.did2s.1$form <- ~ X2 + X3 + X4 + V2 + V3 + V4 + V6 | Gt_f + t_f
    params.did2s.2$form <- ~ X2 + X3 + X4 | C + t_f
    params.did2s.2$name <- "TwoStageDidIndCov"
    
    #### Two way Mundlack regression
    params.etwfe.1  <- list(
      clustervar = "C",
      tvar = "t",
      grp_ind  = "Gt",
      outcomename = "Y",
      # form = "X2 + X3 + X4 + V2 + V3 + V4 + V6", 
      form = "X2 + X3 + X4 + V2 + V3 + V6", 
      name = "TwoWayMundlackRegression")
    
    params.etwfe.2  <- list(
      clustervar = "C",
      tvar = "t",
      grp_ind  = "Gt",
      outcomename = "Y",
      # form = "X2 + X3 + X4 + V2 + V3 + V4 + V6", 
      form = "X2 + X3 + X4", 
      name = "TwoWayMundlackRegressionIndCov")
    
    #### Sun and Abraham 
    
    params.sunab.1  <- list(form = Y ~ X2 + X3 + X4 + V2 + V3 + V6 + i(dist, i.Gt, ref = c(-1, -4)) | t_f, 
                            clustervar = eval(substitute(as.formula(paste("~", 'C')))), 
                            name = "sunabahamAllL")
    
    params.sunab.2  <-list(form = Y ~ i(dist, i.Gt, ref = c(-1, -4)) | X2 + X3 + X4 + V2 + V3 + V6 +  t_f, 
                           clustervar = eval(substitute(as.formula(paste("~", 'C')))), 
                           name = "sunabahamAllR")
    
    params.sunab.3  <-list(form = Y ~ X2 + X3 + X4 + i(dist, i.Gt, ref = c(-1, -4)) | t_f, 
                           clustervar = eval(substitute(as.formula(paste("~", 'C')))), 
                           name = "sunabahamIndCovL")
    
    params.sunab.4  <-list(form = Y ~ i(dist, i.Gt, ref = c(-1, -4)) | X2 + X3 + X4 + t_f, 
                           clustervar = eval(substitute(as.formula(paste("~", 'C')))), 
                           name = "sunabahamIndCovR")
    
    params.sunab.5  <-list(form = Y ~ V2 + V3 + V6 + i(dist, i.Gt, ref = c(-1, -4)) | t_f, 
                           clustervar = eval(substitute(as.formula(paste("~", 'C')))), 
                           name = "sunabahamClusterCovL")
    
    params.sunab.6  <-list(form = Y ~ i(dist, i.Gt, ref = c(-1, -4)) | V2 + V3 + V6 + t_f, 
                           clustervar = eval(substitute(as.formula(paste("~", 'C')))), 
                           name = "sunabahamClusterCovR")
    
  }
  
  if(scen %in% c(15:24,30)){
    
    #### DID Regression
    params.did.0 <- list()
    params.did.0$clustervar <- 'C'
    params.did.0$ctgroup <- "notyettreated"
    params.did.0$estmthod <- 'reg'
    #params.did.1$form <- ~ X2 + X3 + X4 + V2 + V3 + V4 + V6
    params.did.0$form <- ~ X2 + X3 + X4
    params.did.0$unbalanced_panel <- FALSE  
    params.did.0$name <- 'Did Reg Not yet treated IndCov'
    
    #### DID Regression
    params.did.1 <- list()
    params.did.1$clustervar <- 'C'
    params.did.1$ctgroup <- "notyettreated"
    params.did.1$estmthod <- 'reg'
    #params.did.1$form <- ~ X2 + X3 + X4 + V2 + V3 + V4 + V6
    params.did.1$form <- ~ X2 + X3 + X4 + V2 + V3 + V6
    params.did.1$unbalanced_panel <- FALSE 
    params.did.1$name <- 'Did Reg Not yet treated AllCov'
    
    #### DID Double Robust
    params.did.2 <- list()
    params.did.2$clustervar <- 'C'
    params.did.2$ctgroup  <-  "notyettreated"
    params.did.2$estmthod <- 'dr'
    #params.did.2$form <- ~ X2 + X3 + X4 + V2 + V3 + V4 + V6
    params.did.2$form <- ~ X2 + X3 + X4 + V2 + V3 + V6
    params.did.2$unbalanced_panel <- FALSE 
    params.did.2$name <- 'Did DR Not yet treated'
    
    #### DID Weighting
    params.did.3 <- list()
    params.did.3$clustervar <- 'C'
    params.did.3$ctgroup  <-  "notyettreated"
    params.did.3$estmthod <- 'ipw'
    #params.did.3$form <- ~ X2 + X3 + X4 + V2 + V3 + V4 + V6
    #params.did.3$form <- ~ X2 + X3 + X4 + V2 + V3 + V6
    params.did.3$form <- ~ V2 + V3 + V6
    params.did.3$unbalanced_panel <- FALSE 
    params.did.3$name <- 'Did Wgt Not yet treated'
    
    #### TWFE
    params.twfe.1 <- list()
    # params.twfe.1$form <- as.formula('Y ~ t_f + Gt_f + Gt_t + X2 + X3 + X4 + 
    #                                  V2 + V3 + V4 + V6')
    params.twfe.1$form <- as.formula('Y ~ t_f + Gt_f + Gt_t + X2 + X3 + X4 + 
                                     V2 + V3 + V6')
    params.twfe.1$name <- 'TWFE'
    params.twfe.1$subset.data <- FALSE
    
    #### Dynamic TWFE
    params.twfe.2 <- list()
    # params.twfe.2$form <- as.formula('Y ~ t_f + Gt_f + dist_f + X2 + X3 + X4 + 
    #                                  V2 + V3 + V4 + V6')
    params.twfe.2$form <- as.formula('Y ~ t_f + Gt_f + dist_f + X2 + X3 + X4 + 
                                     V2 + V3 + V6')
    params.twfe.2$name <- 'Dynamic TWFE'
    params.twfe.2$subset.data <- FALSE
    
    #### Two Stage Did
    params.did2s.1 <- list()
    params.did2s.1$clustervar <- "C"
    params.did2s.1$trt_ind  <- "Gt_t"
    params.did2s.1$outcomename <- "Y"
    #params.did2s.1$form <-  ~ X2 + X3 + X4 + V2 + V3 + V4 + V6 | Gt_f + t_f
    #params.did2s.1$form <-  ~ X2 + X3 + X4 + V2 + V3 + V6 | Gt_f + t_f
    params.did2s.1$form <-  ~ X2 + X3 + X4 + V2 + V3 + V6 | C + t_f
    params.did2s.1$name <- "TwoStageDid"
    
    #### Two Stage Did
    params.did2s.2 <- list()
    params.did2s.2$clustervar <- "C"
    params.did2s.2$trt_ind  <- "Gt_t"
    params.did2s.2$outcomename <- "Y"
    #params.did2s.1$form <-  ~ X2 + X3 + X4 + V2 + V3 + V4 + V6 | Gt_f + t_f
    #params.did2s.2$form <-  ~ X2 + X3 + X4 | Gt_f + t_f
    params.did2s.2$form <-  ~ X2 + X3 + X4 | C + t_f
    params.did2s.2$name <- "TwoStageDidIndCov"
    
    #### Two way Mundlack regression
    params.etwfe.1  <- list(
      clustervar = "C",
      tvar = "t",
      grp_ind  = "Gt",
      outcomename = "Y",
      # form = "X2 + X3 + X4 + V2 + V3 + V4 + V6", 
      form = "X2 + X3 + X4 + V2 + V3 + V6", 
      name = "TwoWayMundlackRegression")
    
    #### Two way Mundlack regression
    params.etwfe.2  <- list(
      clustervar = "C",
      tvar = "t",
      grp_ind  = "Gt",
      outcomename = "Y",
      # form = "X2 + X3 + X4 + V2 + V3 + V4 + V6", 
      form = "X2 + X3 + X4", 
      name = "TwoWayMundlackRegressionIndCov")
    
    #### Sun and Abraham 
    params.sunab.1  <- list(form = Y ~ X2 + X3 + X4 + V2 + V3 + V6 + i(dist, i.Gt, ref = c(-1, -4)) | t_f, 
                            clustervar = eval(substitute(as.formula(paste("~", 'C')))), 
                            name = "sunabahamAllL")
    
    params.sunab.2  <-list(form = Y ~ i(dist, i.Gt, ref = c(-1, -4)) | X2 + X3 + X4 + V2 + V3 + V6 +  t_f, 
                           clustervar = eval(substitute(as.formula(paste("~", 'C')))), 
                           name = "sunabahamAllR")
    
    params.sunab.3  <-list(form = Y ~ X2 + X3 + X4 + i(dist, i.Gt, ref = c(-1, -4)) | t_f, 
                           clustervar = eval(substitute(as.formula(paste("~", 'C')))), 
                           name = "sunabahamIndCovL")
    
    params.sunab.4  <-list(form = Y ~ i(dist, i.Gt, ref = c(-1, -4)) | X2 + X3 + X4 + t_f, 
                           clustervar = eval(substitute(as.formula(paste("~", 'C')))), 
                           name = "sunabahamIndCovR")
    
    params.sunab.5  <-list(form = Y ~ V2 + V3 + V6 + i(dist, i.Gt, ref = c(-1, -4)) | t_f, 
                           clustervar = eval(substitute(as.formula(paste("~", 'C')))), 
                           name = "sunabahamClusterCovL")
    
    params.sunab.6  <-list(form = Y ~ i(dist, i.Gt, ref = c(-1, -4)) | V2 + V3 + V6 + t_f, 
                           clustervar = eval(substitute(as.formula(paste("~", 'C')))), 
                           name = "sunabahamClusterCovR")
    
  }
  
  return(list(group.out = group.out, 
              time.out = time.out, 
              params.did = list(params.did.0 = params.did.0, 
                                params.did.1 = params.did.1, 
                                params.did.2 = params.did.2, 
                                params.did.3 = params.did.3),
              params.did2s = list(params.did2s.1 = params.did2s.1, 
                                  params.did2s.2 = params.did2s.2),
              params.twfe = list(params.twfe.1 = params.twfe.1, 
                                 params.twfe.2 = params.twfe.2),
              params.etwfe = list(params.etwfe.1 = params.etwfe.1, 
                                  params.etwfe.2 = params.etwfe.2),
              params.sunab = list(params.sunab.1 = params.sunab.1, 
                                  params.sunab.2 = params.sunab.2, 
                                  params.sunab.3 = params.sunab.3,
                                  params.sunab.4 = params.sunab.4,
                                  params.sunab.5 = params.sunab.5,
                                  params.sunab.6 = params.sunab.6),
              params.antonelli = params.antonelli))
  
}

#################
#### Did wrapper
##################

did_wrapper <- function(longdat, all_params, group, time, store_dist_vals){
  
  res.did <- list()
  clean.res.did <- list()
  clean.res.did.marginal.group <- list()
  clean.res.did.marginal.reltime <- list()
  clean.res.did.marginal <- list()
  
  j <- 1
  
  # for(i in 1:length(all_params)){
  #   if(!is.null(all_params[[i]])){
  #     # we may have null all_params (for instance run one version of the method but not another)
  #     # so j needs to be running in a different script
  #     res.did[[j]] <- fit_did(longdat, group, time, all_params[[i]])
  #     clean.res.did[[j]] <- clean_res_did(res.did[[j]], all_params[[i]],  group, time)
  #     #clean.res.did.marginals[[j]] <- clean_res_did_marginals(res.did[[j]], group, time)
  #     j <- j + 1
  #   }
  # }
  
  for(i in 1:length(all_params)){
    if(!is.null(all_params[[i]])){
      # we may have null all_params (for instance run one version of the method but not another)
      # so j needs to be running in a different script
      res.did[[j]] <- tryCatch({
        # Code that may potentially throw an error
        result <- fit_did(longdat, all_params[[i]], group, time)
        result.group <- aggte(result, type = "group")
        result.time <- aggte(result, type = "dynamic")
        result.overall <- aggte(result, type = "simple")
        result.out <- list(result = result, 
                           result.group = result.group,
                           result.time = result.time,
                           result.overall =  result.overall)
      }, error = function(e) {
        # Code to handle the error
        print(paste("An error occurred:", e))
        # Define the object to return in case of an error
        result <- -1
        return(result)
      })
      
      temp <- clean_res_did(res.did[[j]], all_params[[i]], 
                            group, time, store_dist_vals)
      
      clean.res.did[[j]] <- temp[[1]]
      clean.res.did.marginal.group[[j]] <- temp[[2]]
      clean.res.did.marginal.reltime[[j]] <- temp[[3]]
      clean.res.did.marginal[[j]] <- temp[[4]]
      
      j <- j + 1
      
    }
  }
  
  return(list(clean.res.grouptime = clean.res.did, 
              clean.res.group = clean.res.did.marginal.group, 
              clean.res.reltime = clean.res.did.marginal.reltime, 
              clean.res.marginal =  clean.res.did.marginal))
}

fit_did <- function(longdat, params.method, group, time, store_dist_vals){
  
  clustervar <- params.method$clustervar
  ctgroup <- params.method$ctgroup
  reg <- params.method$reg
  estmthod <- params.method$estmthod
  formula_method <- params.method$form
  unbalanced_panel <- params.method$unbalanced_panel
  
  if(clustervar == ''){
    
    out.did <- att_gt(yname = "Y", gname = "Gt", idname = "id", 
                      tname = "t", xformla = formula_method, 
                      allow_unbalanced_panel = unbalanced_panel,
                      control_group = ctgroup,
                      data = longdat, 
                      est_method =  estmthod, 
                      print_details = TRUE)
  }else{
    
    out.did <- att_gt(yname = "Y", gname = "Gt", idname = "id", 
                      tname = "t", xformla = formula_method, 
                      allow_unbalanced_panel = unbalanced_panel,
                      clustervars = clustervar,
                      control_group = ctgroup,
                      data = longdat, 
                      est_method = estmthod)
  }
  
  return(out.did)  
  
}

clean_res_did_2 <- function(out.did, params.did, group, time, store_dist_vals){
  
  # this function returns the parameters of interest
  group.time.out <- expand.grid(group, time)
  group.out <- data.frame(group.time.out)
  time.out <- data.frame(group.time.out)
  names(group.out) <- c('G','t')
  names(time.out) <- c('G','t')
  time.out$Dgt <- time.out$G - time.out$t
  time.out <- time.out %>% dplyr::select(Dgt)
  group.out <- group.out %>% dplyr::select(G)
  
  # left here need to fix relative time 
  if(!is.numeric(out.did[[1]])){
    #browser()
    # Define columns
    group.time.fit <- cbind(out.did[[1]]$group, out.did[[1]]$t)
    ####### Group time fit 
    rows.out.group.time <- compare_rows(group.time.fit, group.time.out)
    res.out <- matrix(0, nrow = sum(rows.out.group.time), ncol = 8)
    res.out[, 1:4] <- cbind(out.did[[1]]$group, out.did[[1]]$t, out.did[[1]]$att, out.did[[1]]$se)
    res.out[, 5] <- out.did[[1]]$att - out.did[[1]]$c * out.did[[1]]$se
    res.out[, 6] <-  out.did[[1]]$att + out.did[[1]]$c * out.did[[1]]$se
    res.out[, 7] <- out.did[[1]]$att - qnorm(0.975) * out.did[[1]]$se
    res.out[, 8] <-  out.did[[1]]$att + qnorm(0.975) * out.did[[1]]$se
    
    colnames(res.out) <- c('group', 'time', 'att', 
                           'se', 'SimultLowCI', 'SimultUppCI', 
                           'IndLowCI', 'IndUppCI')
    
    res.out <-  res.out[rows.out.group.time, ]
    res.out.2 <- data.frame(res.out)
    res.out.2$method <- params.did$name
    res.out.2$ran <- TRUE
    
    ####### Group effects
    res.out.group <- matrix(0, nrow = length(out.did[[2]]$egt), ncol = 7)
    res.out.group[, 1:3] <- cbind(out.did[[2]]$egt, out.did[[2]]$att.egt, out.did[[2]]$se.egt)
    res.out.group[, 4] <- out.did[[2]]$att.egt - out.did[[2]]$crit.val.egt * out.did[[2]]$se.egt
    res.out.group[, 5] <-  out.did[[2]]$att.egt + out.did[[2]]$crit.val.egt * out.did[[2]]$se.egt
    res.out.group[, 6] <- out.did[[2]]$att.egt - qnorm(0.975) * out.did[[2]]$se.egt
    res.out.group[, 7] <-  out.did[[2]]$att.egt + qnorm(0.975) * out.did[[2]]$se.egt
    
    colnames(res.out.group) <- c('group', 'att', 
                                 'se', 'SimultLowCI', 'SimultUppCI', 
                                 'IndLowCI', 'IndUppCI')
    
    
    res.out.group.2 <- data.frame(res.out.group)
    res.out.group.2$method <- params.did$name
    res.out.group.2$ran <- TRUE
    
    ####### Time to event effects
    res.out.reltime <- matrix(0, nrow = length(out.did[[3]]$egt), ncol = 7)
    res.out.reltime[, 1:3] <- cbind( out.did[[3]]$egt, out.did[[3]]$att.egt, out.did[[3]]$se.egt)
    res.out.reltime[, 4] <- out.did[[3]]$att.egt - out.did[[3]]$crit.val.egt * out.did[[3]]$se.egt
    res.out.reltime[, 5] <-  out.did[[3]]$att.egt + out.did[[3]]$crit.val.egt * out.did[[3]]$se.egt
    res.out.reltime[, 6] <- out.did[[3]]$att.egt - qnorm(0.975) * out.did[[3]]$se.egt
    res.out.reltime[, 7] <- out.did[[3]]$att.egt + qnorm(0.975) * out.did[[3]]$se.egt
    
    colnames(res.out.reltime) <-c('timetoev', 'att', 
                                    'se', 'SimultLowCI', 'SimultUppCI', 
                                    'IndLowCI', 'IndUppCI')
    
    res.out.reltime.2 <- data.frame(res.out.reltime)
    res.out.reltime.2$method <- params.did$name
    res.out.reltime.2$ran <- TRUE
    
    ####### Overall effect
    res.out.marginal <- data.frame(att = out.did[[4]]$overall.att)
    res.out.marginal$se <- out.did[[4]]$overall.se
    res.out.marginal$SimultLowCI <- res.out.marginal$att - qnorm(0.975) * res.out.marginal$se 
    res.out.marginal$SimultUppCI <- res.out.marginal$att + qnorm(0.975) * res.out.marginal$se 
    res.out.marginal$IndLowCI <- res.out.marginal$att - qnorm(0.975) * res.out.marginal$se 
    res.out.marginal$IndUppCI <- res.out.marginal$att + qnorm(0.975) * res.out.marginal$se 
    res.out.marginal$method <- params.did$name
    res.out.marginal$ran <- TRUE
    
    return(list(clean.res.grouptime = res.out.2,
                clean.res.group = res.out.group.2, 
                clean.res.reltime = res.out.reltime.2,
                clean.res.marginal = res.out.marginal))
    
  }else{
    
    #### Group time effects 
    rows_out <- group.time.out
    res.out <- matrix(0, nrow = nrow(rows_out), ncol = 8)
    colnames(res.out) <- c('group', 'time', 'att', 
                           'se', 'SimultLowCI', 'SimultUppCI', 
                           'IndLowCI', 'IndUppCI')
    
    res.out[, 1:2] <- as.matrix(group.time.out)
    res.out[, 3:8] <- matrix(-1, nrow = nrow(rows_out), ncol = 6)
    res.out <- data.frame(res.out)
    res.out$method <- params.did$name
    res.out$ran <- FALSE
    
    ##### Group effects
    group.fit <- unique(group.out$G)
    res.out.group<- matrix(0, nrow = length(group.fit), ncol = 7)
    
    colnames(res.out.group) <- c('group', 'att', 
                                 'se', 'SimultLowCI', 'SimultUppCI', 
                                 'IndLowCI', 'IndUppCI')
    
    res.out.group[, 1] <- as.matrix(group.fit)
    res.out.group[, 2:7] <- matrix(-1, nrow = length(group.fit), ncol = 6)
    res.out.group <- data.frame(res.out.group)
    res.out.group$method <- params.did$name
    res.out.group$ran <- FALSE
    
    ##### Time to event effects 
    time.fit <- unique(time.out$Dgt)
    res.out.reltime <- matrix(0, nrow = length(time.fit), ncol = 7)
    
    colnames(res.out.reltime) <- c('timetoev', 'att', 
                                    'se', 'SimultLowCI', 'SimultUppCI', 
                                    'IndLowCI', 'IndUppCI')
    
    res.out.reltime[, 1] <- as.matrix(time.fit)
    res.out.reltime[, 2:7] <- matrix(-1, nrow = length(time.fit), ncol = 6)
    res.out.reltime <- data.frame( res.out.reltime)
    res.out.reltime$method <- params.did$name
    res.out.reltime$ran <- FALSE
    
    ####### Overall effect
    res.out.marginal <- data.frame(att = -1)
    res.out.marginal$se <- -1
    res.out.marginal$SimultLowCI <- -1
    res.out.marginal$SimultUppCI <- -1
    res.out.marginal$IndLowCI <- -1
    res.out.marginal$IndUppCI <- -1
    res.out.marginal$method <- params.did$name
    res.out.marginal$ran <- FALSE
    
    return(list(clean.res.grouptime = res.out,
                clean.res.group = res.out.group, 
                clean.res.reltime = res.out.reltime,
                clean.res.marginal = res.out.marginal))
  }
  
}

clean_res_did <- function(out.did, params.did, group, time, store_dist_vals){
  
  # this function returns the parameters of interest
  group.time.out <- expand.grid(group, time)
  group.out <- data.frame(group.time.out)
  time.out <- data.frame(group.time.out)
  names(group.out) <- c('G','t')
  names(time.out) <- c('G','t')
  time.out$Dgt <- time.out$G - time.out$t
  time.out <- time.out %>% dplyr::select(Dgt)
  group.out <- group.out %>% dplyr::select(G)
  
  # left here need to fix relative time 
  if(!is.numeric(out.did[[1]])){
    #browser()
    # Define columns
    group.time.fit <- cbind(out.did[[1]]$group, out.did[[1]]$t)
    ####### Group time fit 
    rows.out.group.time <- compare_rows(group.time.fit, group.time.out)
    res.out <- matrix(0, nrow = sum(rows.out.group.time), ncol = 8)
    res.out[, 1:4] <- cbind(out.did[[1]]$group, out.did[[1]]$t, out.did[[1]]$att, out.did[[1]]$se)
    res.out[, 5] <- out.did[[1]]$att - out.did[[1]]$c * out.did[[1]]$se
    res.out[, 6] <-  out.did[[1]]$att + out.did[[1]]$c * out.did[[1]]$se
    res.out[, 7] <- out.did[[1]]$att - qnorm(0.975) * out.did[[1]]$se
    res.out[, 8] <-  out.did[[1]]$att + qnorm(0.975) * out.did[[1]]$se
    
    colnames(res.out) <- c('group', 'time', 'att', 
                           'se', 'SimultLowCI', 'SimultUppCI', 
                           'IndLowCI', 'IndUppCI')
    
    res.out <-  res.out[rows.out.group.time, ]
    res.out.2 <- data.frame(res.out)
    res.out.2$method <- params.did$name
    res.out.2$ran <- TRUE
    
    ####### Group effects
    res.out.group <- matrix(0, nrow = length(out.did[[2]]$egt), ncol = 7)
    res.out.group[, 1:3] <- cbind(out.did[[2]]$egt, out.did[[2]]$att.egt, out.did[[2]]$se.egt)
    res.out.group[, 4] <- out.did[[2]]$att.egt - out.did[[2]]$crit.val.egt * out.did[[2]]$se.egt
    res.out.group[, 5] <-  out.did[[2]]$att.egt + out.did[[2]]$crit.val.egt * out.did[[2]]$se.egt
    res.out.group[, 6] <- out.did[[2]]$att.egt - qnorm(0.975) * out.did[[2]]$se.egt
    res.out.group[, 7] <-  out.did[[2]]$att.egt + qnorm(0.975) * out.did[[2]]$se.egt
    
    colnames(res.out.group) <- c('group', 'att', 
                                 'se', 'SimultLowCI', 'SimultUppCI', 
                                 'IndLowCI', 'IndUppCI')
    
    
    res.out.group.2 <- data.frame(res.out.group)
    res.out.group.2$method <- params.did$name
    res.out.group.2$ran <- TRUE
    
    ####### Time to event effects
    res.out.reltime <- matrix(0, nrow = length(out.did[[3]]$egt), ncol = 7)
    res.out.reltime[, 1:3] <- cbind( out.did[[3]]$egt, out.did[[3]]$att.egt, out.did[[3]]$se.egt)
    res.out.reltime[, 4] <- out.did[[3]]$att.egt - out.did[[3]]$crit.val.egt * out.did[[3]]$se.egt
    res.out.reltime[, 5] <-  out.did[[3]]$att.egt + out.did[[3]]$crit.val.egt * out.did[[3]]$se.egt
    res.out.reltime[, 6] <- out.did[[3]]$att.egt - qnorm(0.975) * out.did[[3]]$se.egt
    res.out.reltime[, 7] <- out.did[[3]]$att.egt + qnorm(0.975) * out.did[[3]]$se.egt
    
    colnames(res.out.reltime) <-c('timetoev', 'att', 
                                  'se', 'SimultLowCI', 'SimultUppCI', 
                                  'IndLowCI', 'IndUppCI')
    
    res.out.reltime.2 <- data.frame(res.out.reltime)
    res.out.reltime.2$method <- params.did$name
    res.out.reltime.2$ran <- TRUE
    
    ####### Overall effect
    res.out.marginal <- data.frame(att = out.did[[4]]$overall.att)
    res.out.marginal$se <- out.did[[4]]$overall.se
    res.out.marginal$SimultLowCI <- res.out.marginal$att - qnorm(0.975) * res.out.marginal$se 
    res.out.marginal$SimultUppCI <- res.out.marginal$att + qnorm(0.975) * res.out.marginal$se 
    res.out.marginal$IndLowCI <- res.out.marginal$att - qnorm(0.975) * res.out.marginal$se 
    res.out.marginal$IndUppCI <- res.out.marginal$att + qnorm(0.975) * res.out.marginal$se 
    res.out.marginal$method <- params.did$name
    res.out.marginal$ran <- TRUE
    
    return(list(clean.res.grouptime = res.out.2,
                clean.res.group = res.out.group.2, 
                clean.res.reltime = res.out.reltime.2,
                clean.res.marginal = res.out.marginal))
    
  }else{
    
    res.out <- NULL
    res.out.group <- NULL
    res.out.reltime <- NULL
    res.out.marginal <- NULL
    
    return(list(clean.res.grouptime = res.out,
                clean.res.group = res.out.group, 
                clean.res.reltime = res.out.reltime,
                clean.res.marginal = res.out.marginal))
  }
  
}

#################
#### TWFE wrapper
##################

twfe_wrapper <- function(longdat, group, time, all_params, store_dist_vals){
  
  res.twfe <- list()
  clean.res.twfe <- list()
  j <- 1
  
  for(i in 1:length(all_params)){
    if(!is.null(all_params[[i]])){
      # we may have null all_params (for instance run one version of the method but not another)
      # so j needs to be running in a different script
      res.twfe[[j]] <- tryCatch({
          # Code that may potentially throw an error
          result <- fit_twfe(longdat, all_params[[i]], group, time)
        }, error = function(e) {
          # Code to handle the error
          print(paste("An error occurred:", e))
          # Define the object to return in case of an error
          result <- -1
          return(result)
        })

      clean.res.twfe[[j]] <- clean_res_twfe(res.twfe[[j]], all_params[[i]], 
                                            group, time, store_dist_vals) 
      j <- j + 1
    }
  }
  
  return(clean.res.twfe)
  
}

fit_twfe <- function(longdat, params_twfe, group, time){
  
  if(params_twfe$subset.data){
    # for this feature maybe subset to one time-grup combination that is always available
    # to avoid having problems
  }else{
    form.twfe <- params_twfe$form
    twfe <- lm(formula = form.twfe, data = longdat)
  }
  return(twfe)
}

clean_res_twfe_2 <- function(twfe.model.fit, 
                           params.twfe, 
                           group, 
                           time, 
                           store_dist_vals){
  
  group.time.out <- expand.grid(group, time)
  
  if(!is.numeric(twfe.model.fit)){
    if(params.twfe$name == 'TWFE'){
      # only one point estimate to return with this model 
      coef.twfe <- as.numeric(coef(twfe.model.fit)['Gt_t'])
      std.error.twfe <- coef(summary(twfe.model.fit))['Gt_t', "Std. Error"]
      conf.int.twfe <- confint(twfe.model.fit)['Gt_t', ]
      # TWFE is fitted in fit_twfe, but the object returned is the lm model
      # in DID case we return a more curated version with group and time
      # so this is why clean_res_twfe is a bit different
      # the resulting object out of this function is the same as the DID Object
      res.out <- matrix(0, nrow = nrow(group.time.out), ncol = 8)
      res.out[, 1:4] <- cbind(group.time.out[, 1],  group.time.out[, 2], 
                              coef.twfe, std.error.twfe)
      # these are repeated because in DID we have simultaneous confidence intervals
      # but in TWFE this does not happen
      res.out[, 5] <- conf.int.twfe[1]
      res.out[, 6] <- conf.int.twfe[2] 
      res.out[, 7] <- conf.int.twfe[1]
      res.out[, 8] <- conf.int.twfe[2] 
      res.out <- data.frame(res.out)
      colnames(res.out) <- c('Gt', 't', 'att', 'se', 'SimultLowCI', 
                             'SimultUppCI', 'IndLowCI', 'IndUppCI')
      
      res.out <- res.out %>% rename(group= Gt, time = t)
      res.out <-  res.out[order(res.out$group, res.out$time), ]
      res.out$method <- 'TWFE'
      res.out$ran <- TRUE
      
      ####### Group fit
      res.out.group <- matrix(0, nrow = length(group), ncol = 7)
      res.out.group[, 1] <- group 
      res.out.group[, 2] <- coef.twfe
      res.out.group[, 3] <- std.error.twfe
      res.out.group[, 4] <- conf.int.twfe[1] 
      res.out.group[, 5] <- conf.int.twfe[2]
      res.out.group[, 6] <- conf.int.twfe[1] 
      res.out.group[, 7] <- conf.int.twfe[2] 
      
      colnames(res.out.group) <- c('group', 'att', 
                                   'se', 'SimultLowCI', 'SimultUppCI', 
                                   'IndLowCI', 'IndUppCI')
      
      
      res.out.group.2 <- data.frame(res.out.group)
      res.out.group.2$method <- params.twfe$name
      res.out.group.2$ran <- TRUE
      
      ####### Time to Event effect
      time.to.event <- unique(group.time.out$Var1 - group.time.out$Var2)
      res.out.reltime <- matrix(0, nrow = length(time.to.event), ncol = 7)
      res.out.reltime[, 1] <- time.to.event 
      res.out.reltime[, 2] <- coef.twfe
      res.out.reltime[, 3] <- std.error.twfe
      res.out.reltime[, 4] <- conf.int.twfe[1] 
      res.out.reltime[, 5] <- conf.int.twfe[2]
      res.out.reltime[, 6] <- conf.int.twfe[1] 
      res.out.reltime[, 7] <- conf.int.twfe[2] 
      
      colnames(res.out.reltime) <-c('timetoev', 'att', 
                                   'se', 'SimultLowCI', 'SimultUppCI', 
                                   'IndLowCI', 'IndUppCI')
      
      res.out.reltime.2 <- data.frame(res.out.reltime)
      res.out.reltime.2$method <- params.twfe$name
      res.out.reltime.2$ran <- TRUE
      
      ####### Overall effect
      res.out.marginal <- data.frame(att = coef.twfe)
      res.out.marginal$se <- std.error.twfe
      res.out.marginal$SimultLowCI <- conf.int.twfe[1] 
      res.out.marginal$SimultUppCI <- conf.int.twfe[2]
      res.out.marginal$IndLowCI <-  conf.int.twfe[1] 
      res.out.marginal$IndUppCI <- conf.int.twfe[2]
      res.out.marginal$method <- params.twfe$name
      res.out.marginal$ran <- TRUE
    
          return(list(clean.res.grouptime = res.out,
                clean.res.group = res.out.group, 
                clean.res.reltime = res.out.reltime,
                clean.res.marginal = res.out.marginal))

    }
    
    if(params.twfe$name == 'Dynamic TWFE'){
      
      # match estimate and CI of each twfe estimate to store_dist_vals
      # match group & time to store_dist_vals
      # return matrix of group, time, and estimates as well as 95%CI
      # as many as unique values in distance values matrix estimates to return with this model
      
      # Get estimates from coefficients from  dist_f_number
      coef.twfe <- coef(twfe.model.fit)[grep("dist_f", names(coef(twfe.model.fit)))]
      # Get standard errors 
      std.error.twfe <- coef(summary(twfe.model.fit))[grep("dist_f", names(coef(twfe.model.fit))), 'Std. Error']
      std.error.twfe <- ifelse(!is.na(coef.twfe),   std.error.twfe, NA )
      conf.int.twfe <- confint(twfe.model.fit)[grep("dist_f", names(coef(twfe.model.fit))), ]
      
      temp <- data.frame(att = coef.twfe, se = std.error.twfe, 
                         SimultLowCI = conf.int.twfe[,1], 
                         SimultUppCI = conf.int.twfe[,2], 
                         IndLowCI = conf.int.twfe[,1], 
                         IndUppCI = conf.int.twfe[,2])
      
      temp$Dtg <- rownames(temp)
      temp$Dtg <- gsub('dist_f','',temp$Dtg)
      rownames(temp) <- NULL
      # Store results in res.out using store_dist_vals as the intermediate 
      # matrix for merging 
      res.out <- matrix(0, nrow = nrow(group.time.out), ncol = 2)
      res.out[, 1:2] <- cbind(group.time.out[, 1],  group.time.out[, 2])
      res.out <- data.frame(res.out)
      colnames(res.out) <- c('Gt', 't')
      store_dist_vals_2 <- data.frame(store_dist_vals)
      res.out.2 <- merge(res.out, store_dist_vals_2, by.x = c('Gt', 't'),
                         by.y = c('G', 't'), all.y = FALSE) 
      res.out.3 <- merge(res.out.2, temp, by = 'Dtg')
      
      res.out.4 <- res.out.3 %>% dplyr::select(-c(Dtg, Label))
      res.out.5 <- res.out.4 %>% rename(group= Gt, time = t)
      res.out.5 <-  res.out.5[order( res.out.5$group,  res.out.5$time), ]
      
      res.out.5$method <- 'Dynamic TWFE'
      res.out.5$ran <- TRUE
      return(res.out.5)
    }
    }else{
      
      res.out <- matrix(0, nrow = nrow(group.time.out), ncol = 8)
      res.out[, 1:4] <- cbind(group.time.out[, 1],  group.time.out[, 2], 
                              -1, -1)
      res.out[, 5] <- -1
      res.out[, 6] <- -1 
      res.out[, 7] <- -1
      res.out[, 8] <- -1
      res.out <- data.frame(res.out)
      colnames(res.out) <- c('Gt', 't', 'att', 'se', 'SimultLowCI', 
                             'SimultUppCI', 'IndLowCI', 'IndUppCI')
      res.out <- data.frame(res.out)
      res.out$ran <- FALSE
      
      return(res.out)
  }
 
}

clean_res_twfe <- function(twfe.model.fit, 
                             params.twfe, 
                             group, 
                             time, 
                             store_dist_vals){
  
  group.time.out <- expand.grid(group, time)
  
  if(!is.numeric(twfe.model.fit)){
    if(params.twfe$name == 'TWFE'){
      # only one point estimate to return with this model 
      coef.twfe <- as.numeric(coef(twfe.model.fit)['Gt_t'])
      std.error.twfe <- coef(summary(twfe.model.fit))['Gt_t', "Std. Error"]
      conf.int.twfe <- confint(twfe.model.fit)['Gt_t', ]
      # TWFE is fitted in fit_twfe, but the object returned is the lm model
      # in DID case we return a more curated version with group and time
      # so this is why clean_res_twfe is a bit different
      # the resulting object out of this function is the same as the DID Object
      res.out <- matrix(0, nrow = nrow(group.time.out), ncol = 8)
      res.out[, 1:4] <- cbind(group.time.out[, 1],  group.time.out[, 2], 
                              coef.twfe, std.error.twfe)
      # these are repeated because in DID we have simultaneous confidence intervals
      # but in TWFE this does not happen
      res.out[, 5] <- conf.int.twfe[1]
      res.out[, 6] <- conf.int.twfe[2] 
      res.out[, 7] <- conf.int.twfe[1]
      res.out[, 8] <- conf.int.twfe[2] 
      res.out <- data.frame(res.out)
      colnames(res.out) <- c('Gt', 't', 'att', 'se', 'SimultLowCI', 
                             'SimultUppCI', 'IndLowCI', 'IndUppCI')
      
      res.out <- res.out %>% rename(group= Gt, time = t)
      res.out <-  res.out[order(res.out$group, res.out$time), ]
      res.out$method <- 'TWFE'
      res.out$ran <- TRUE
      
      ####### Group fit
      res.out.group <- matrix(0, nrow = length(group), ncol = 7)
      res.out.group[, 1] <- group 
      res.out.group[, 2] <- coef.twfe
      res.out.group[, 3] <- std.error.twfe
      res.out.group[, 4] <- conf.int.twfe[1] 
      res.out.group[, 5] <- conf.int.twfe[2]
      res.out.group[, 6] <- conf.int.twfe[1] 
      res.out.group[, 7] <- conf.int.twfe[2] 
      
      colnames(res.out.group) <- c('group', 'att', 
                                   'se', 'SimultLowCI', 'SimultUppCI', 
                                   'IndLowCI', 'IndUppCI')
      
      
      res.out.group.2 <- data.frame(res.out.group)
      res.out.group.2$method <- params.twfe$name
      res.out.group.2$ran <- TRUE
      
      ####### Time to Event effect
      time.to.event <- unique(group.time.out$Var1 - group.time.out$Var2)
      res.out.reltime <- matrix(0, nrow = length(time.to.event), ncol = 7)
      res.out.reltime[, 1] <- time.to.event 
      res.out.reltime[, 2] <- coef.twfe
      res.out.reltime[, 3] <- std.error.twfe
      res.out.reltime[, 4] <- conf.int.twfe[1] 
      res.out.reltime[, 5] <- conf.int.twfe[2]
      res.out.reltime[, 6] <- conf.int.twfe[1] 
      res.out.reltime[, 7] <- conf.int.twfe[2] 
      
      colnames(res.out.reltime) <-c('timetoev', 'att', 
                                    'se', 'SimultLowCI', 'SimultUppCI', 
                                    'IndLowCI', 'IndUppCI')
      
      res.out.reltime.2 <- data.frame(res.out.reltime)
      res.out.reltime.2$method <- params.twfe$name
      res.out.reltime.2$ran <- TRUE
      
      ####### Overall effect
      res.out.marginal <- data.frame(att = coef.twfe)
      res.out.marginal$se <- std.error.twfe
      res.out.marginal$SimultLowCI <- conf.int.twfe[1] 
      res.out.marginal$SimultUppCI <- conf.int.twfe[2]
      res.out.marginal$IndLowCI <-  conf.int.twfe[1] 
      res.out.marginal$IndUppCI <- conf.int.twfe[2]
      res.out.marginal$method <- params.twfe$name
      res.out.marginal$ran <- TRUE
      
      return(list(clean.res.grouptime = res.out,
                  clean.res.group = res.out.group.2, 
                  clean.res.reltime = res.out.reltime.2,
                  clean.res.marginal = res.out.marginal))
      
    }
    if(params.twfe$name == 'Dynamic TWFE'){
      
      # match estimate and CI of each twfe estimate to store_dist_vals
      # match group & time to store_dist_vals
      # return matrix of group, time, and estimates as well as 95%CI
      # as many as unique values in distance values matrix estimates to return with this model
      
      # Get estimates from coefficients from  dist_f_number
      coef.twfe <- coef(twfe.model.fit)[grep("dist_f", names(coef(twfe.model.fit)))]
      # Get standard errors 
      std.error.twfe <- coef(summary(twfe.model.fit))[grep("dist_f", names(coef(twfe.model.fit))), 'Std. Error']
      std.error.twfe <- ifelse(!is.na(coef.twfe),   std.error.twfe, NA )
      conf.int.twfe <- confint(twfe.model.fit)[grep("dist_f", names(coef(twfe.model.fit))), ]
      
      temp <- data.frame(att = coef.twfe, se = std.error.twfe, 
                         SimultLowCI = conf.int.twfe[,1], 
                         SimultUppCI = conf.int.twfe[,2], 
                         IndLowCI = conf.int.twfe[,1], 
                         IndUppCI = conf.int.twfe[,2])
      
      temp$Dtg <- rownames(temp)
      temp$Dtg <- gsub('dist_f','',temp$Dtg)
      rownames(temp) <- NULL
      # Store results in res.out using store_dist_vals as the intermediate 
      # matrix for merging 
      res.out <- matrix(0, nrow = nrow(group.time.out), ncol = 2)
      res.out[, 1:2] <- cbind(group.time.out[, 1],  group.time.out[, 2])
      res.out <- data.frame(res.out)
      colnames(res.out) <- c('Gt', 't')
      store_dist_vals_2 <- data.frame(store_dist_vals)
      res.out.2 <- merge(res.out, store_dist_vals_2, by.x = c('Gt', 't'),
                         by.y = c('G', 't'), all.y = FALSE) 
      res.out.3 <- merge(res.out.2, temp, by = 'Dtg')
      
      res.out.4 <- res.out.3 %>% dplyr::select(-c(Dtg, Label))
      res.out.5 <- res.out.4 %>% rename(group= Gt, time = t)
      res.out.5 <-  res.out.5[order( res.out.5$group,  res.out.5$time), ]
      
      res.out.5$method <- 'Dynamic TWFE'
      res.out.5$ran <- TRUE
      return(res.out.5)
    }
  }else{
    
    res.out <- NULL
    res.out.group <- NULL
    res.out.reltime <- NULL
    res.out.marginal <- NULL
    
    return(list(clean.res.grouptime = res.out,
                clean.res.group = res.out.group, 
                clean.res.reltime = res.out.reltime,
                clean.res.marginal = res.out.marginal))
  
  }

}

###################################
#### 2 stage Did wrapper functions
####################################

fit_did2s <- function(longdat, params.method, 
                      group, time){
  
  longdat <- longdat %>% mutate(Dgt = as.factor(paste0(Gt, t)))
  clustervar <- params.method$clustervar
  trt_ind <- params.method$trt_ind 
  outcomename <- params.method$outcomename
  formula_method <- params.method$form
  
  if(clustervar == ''){
    
    # time to event 
    out.did2s.ell <- did2s(longdat,
                       yname = outcomename, 
                       first_stage = formula_method, 
                       second_stage = ~i(dist_f, 
                                         ref=c(-1, Inf)), 
                       treatment = trt_ind)
    # overall ATT
    out.did2s.aggr <- did2s(longdat,
                        yname = outcomename, 
                        first_stage =  formula_method,
                        second_stage = ~i(Gt_t), 
                        treatment = trt_ind)
  
  }else{
  
    out.did2s.gt <- did2s(longdat,
                          yname = "Y", 
                          first_stage = formula_method, 
                          second_stage = ~i(Dgt),
                          treatment = trt_ind, 
                          cluster_var = clustervar)
    
    # time to event 
    out.did2s.ell <- did2s(longdat,
                       yname = "Y", 
                       first_stage = formula_method, 
                       second_stage = ~i(dist_f, ref=c(-1, Inf)),
                       treatment = trt_ind, 
                       cluster_var = clustervar)
    
    # overall ATT
    out.did2s.aggr <- did2s(longdat,
                           yname = "Y", 
                           first_stage = formula_method, 
                           second_stage = ~i(Gt_t), 
                           treatment = trt_ind, 
                           cluster_var = clustervar)
  }
  
  return(list(out.did2s.gt = out.did2s.gt,
              out.did2s.ell = out.did2s.ell, 
              out.did2s.aggr = out.did2s.aggr)) 
  
}

did2s_wrapper <- function(longdat, all_params, 
                          group, time, store_dist_vals){
  
  res.did2s <- list()
  clean.res.did2s <- list()
  
  clean.res.did2s.grouptime <- list()
  clean.res.did2s.reltime <- list()
  clean.res.did2s.marginal <- list()
  
  j <- 1
  
  for(i in 1:length(all_params)){
    if(!is.null(all_params[[i]])){
      # we may have null all_params (for instance run one version of the method but not another)
      # so j needs to be running in a different script
      res.did2s[[j]] <- tryCatch({
        # Code that may potentially throw an error
        result <- fit_did2s(longdat, all_params[[i]], group, time)
      }, error = function(e) {
        # Code to handle the error
        print(paste("An error occurred:", e))
        # Define the object to return in case of an error
        result <- -1
        return(result)
      })
      
      clean.res.did2s <- clean_res_did2s(res.did2s[[j]], all_params[[i]], 
                                         group, time, store_dist_vals)
  
      clean.res.did2s.grouptime[[j]] <- clean.res.did2s$clean.res.grouptime 
      clean.res.did2s.reltime[[j]] <- clean.res.did2s$clean.res.reltime
      clean.res.did2s.marginal[[j]] <- clean.res.did2s$clean.res.marginal
        
      j <- j + 1
    }
  }
  
  return(list(clean.res.grouptime = clean.res.did2s.grouptime, 
              clean.res.reltime = clean.res.did2s.reltime, 
              clean.res.marginal = clean.res.did2s.marginal))
}

clean_res_did2s <- function(out.did2s.all, params.did2s, 
                            group, time, store_dist_vals){
  
  # Match estimate and CI of each twfe estimate to store_dist_vals
  # Match group & time to store_dist_vals
  # Return matrix of group, time, and estimates as well as 95%CI
  # As many as unique values in distance values matrix estimates to return with this model
  group.time.out <- expand.grid(group, time)
  
  #browser()
  if(!is.numeric(out.did2s.all)){
  
    out.did2s.gt <- out.did2s.all$out.did2s.gt
    out.did2s.ell <- out.did2s.all$out.did2s.ell
    out.did2s.aggr <- out.did2s.all$out.did2s.aggr
    
    #### group time 
    # Get estimates from coefficients from did2s
    coef.did2s <-  coefficients(out.did2s.gt)
    # Get standard errors 
    se.did2s <- se(out.did2s.gt)
    # Get lower and upper CI
    coef.did2s.low <- coef.did2s - qnorm(1-0.05/2) *  se.did2s
    coef.did2s.high <- coef.did2s + qnorm(1-0.05/2) *  se.did2s
    
    temp <- data.frame(att = coef.did2s, 
                       se =  se.did2s, 
                       SimultLowCI = coef.did2s.low, 
                       SimultUppCI = coef.did2s.high, 
                       IndLowCI = coef.did2s.low, 
                       IndUppCI = coef.did2s.high)
    
    gt <- rownames(temp)
    gt <- gsub('Dgt::','',gt)
    gt <- as.numeric(gt)
    rownames(temp) <- NULL
    # Extract group and time
    g <- gt %/% 10  # Get the first digit
    t <- gt %% 10 
    temp$group <- g
    temp$time <- t
    temp <-  temp %>% select(group, time, everything())
    res.out.gt <- as.data.frame(temp)
    colnames(res.out.gt) <- c('group', 'time', 'att', 
                              'se', 'SimultLowCI', 'SimultUppCI', 
                              'IndLowCI', 'IndUppCI')
    res.out.gt$method <- params.did2s$name
    res.out.gt$ran <- TRUE
    
    # # Store results in res.out using store_dist_vals as the intermediate 
    # # matrix for merging 
    # res.out <- matrix(0, nrow = nrow(group.time.out), ncol = 2)
    # res.out[, 1:2] <- cbind(group.time.out[, 1],  group.time.out[, 2])
    # res.out <- data.frame(res.out)
    # colnames(res.out) <- c('Gt', 't')
    # store_dist_vals_2 <- data.frame(store_dist_vals)
    # res.out.2 <- merge(res.out, store_dist_vals_2, by.x = c('Gt', 't'),
    #                    by.y = c('G', 't'), all.y = FALSE) 
    # res.out.3 <- merge(res.out.2, temp, by = 'Dtg')
    # 
    # res.out.4 <- res.out.3 %>% dplyr::select(-c(Dtg, Label))
    # res.out.5 <- res.out.4 %>% rename(group= Gt, time = t)
    # res.out.5 <-  res.out.5[order( res.out.5$group,  res.out.5$time), ]
    # 
    # res.out.5$method <- params.did2s$name
    # res.out.5$ran <- TRUE
    
    ####### Time to event effects 
    time.to.event <- unique(group.time.out$Var1 - group.time.out$Var2)
    # Get estimates from coefficients from did2s
    coef.did2s <-  coefficients(out.did2s.ell)
    # Get standard errors 
    se.did2s <- se(out.did2s.ell)
    # Get lower and upper CI
    coef.did2s.low <- coef.did2s - qnorm(1-0.05/2) *  se.did2s
    coef.did2s.high <- coef.did2s + qnorm(1-0.05/2) *  se.did2s
    
    temp <- data.frame(att = coef.did2s, 
                       se =  se.did2s, 
                       SimultLowCI = coef.did2s.low, 
                       SimultUppCI = coef.did2s.high, 
                       IndLowCI = coef.did2s.low, 
                       IndUppCI = coef.did2s.high)
    

    temp$timetoev <- rownames(temp)
    temp$timetoev <- gsub('dist_f::','',temp$timetoev)
    rownames(temp) <- NULL
    
    # rearrange order of variables to match other results 
    res.out.reltime <- temp %>% relocate(timetoev)
    res.out.reltime$method <- params.did2s$name
    res.out.reltime$ran <- TRUE
    
    ####### Overall effect
    # Get estimates from coefficients from did2s
    
    coef.did2s <- coefficients(out.did2s.aggr)['Gt_t::1'] 
    if(length(coef.did2s) >0 ){
      # Get standard errors 
      se.did2s <- se(out.did2s.aggr)['Gt_t::1']
      # Get lower and upper CI
      coef.did2s.low <- coef.did2s - qnorm(1-0.05/2) *  se.did2s
      coef.did2s.high <- coef.did2s + qnorm(1-0.05/2) *  se.did2s
      # Store results 
      res.out.marginal <- data.frame(att = coef.did2s)
      res.out.marginal$se <- se.did2s
      res.out.marginal$SimultLowCI <- res.out.marginal$att - qnorm(0.975) * res.out.marginal$se 
      res.out.marginal$SimultUppCI <- res.out.marginal$att + qnorm(0.975) * res.out.marginal$se 
      res.out.marginal$IndLowCI <- res.out.marginal$att - qnorm(0.975) * res.out.marginal$se 
      res.out.marginal$IndUppCI <- res.out.marginal$att + qnorm(0.975) * res.out.marginal$se 
      res.out.marginal$method <- params.did2s$name
      res.out.marginal$ran <- TRUE
    }else{
      res.out.marginal <- NULL
    }

    return(list(clean.res.grouptime = res.out.gt,
                clean.res.reltime = res.out.reltime,
                clean.res.marginal = res.out.marginal))
  }else{
    
    res.out <- NULL
    res.out.reltime <- NULL
    res.out.marginal <- NULL
    
    return(list(clean.res.grouptime = res.out,
                clean.res.reltime = res.out.reltime,
                clean.res.marginal = res.out.marginal))
  }
  
}

clean_res_did2s_2 <- function(out.did2s.all, params.did2s, 
                            group, time, store_dist_vals){
  
  # Match estimate and CI of each twfe estimate to store_dist_vals
  # Match group & time to store_dist_vals
  # Return matrix of group, time, and estimates as well as 95%CI
  # As many as unique values in distance values matrix estimates to return with this model
  group.time.out <- expand.grid(group, time)
  
  #browser()
  if(!is.numeric(out.did2s.all)){
    
    out.did2s.ell <- out.did2s.all$out.did2s.ell
    out.did2s.aggr <- out.did2s.all$out.did2s.aggr
    
    #### group time 
    # Get estimates from coefficients from did2s
    coef.did2s <-  coefficients(out.did2s.ell)
    # Get standard errors 
    se.did2s <- se(out.did2s.ell)
    # Get lower and upper CI
    coef.did2s.low <- coef.did2s - qnorm(1-0.05/2) *  se.did2s
    coef.did2s.high <- coef.did2s + qnorm(1-0.05/2) *  se.did2s
    
    temp <- data.frame(att = coef.did2s, 
                       se =  se.did2s, 
                       SimultLowCI = coef.did2s.low, 
                       SimultUppCI = coef.did2s.high, 
                       IndLowCI = coef.did2s.low, 
                       IndUppCI = coef.did2s.high)
    
    temp$Dtg <- rownames(temp)
    temp$Dtg <- gsub('dist_f::','',temp$Dtg)
    rownames(temp) <- NULL
    
    # Store results in res.out using store_dist_vals as the intermediate 
    # matrix for merging 
    res.out <- matrix(0, nrow = nrow(group.time.out), ncol = 2)
    res.out[, 1:2] <- cbind(group.time.out[, 1],  group.time.out[, 2])
    res.out <- data.frame(res.out)
    colnames(res.out) <- c('Gt', 't')
    store_dist_vals_2 <- data.frame(store_dist_vals)
    res.out.2 <- merge(res.out, store_dist_vals_2, by.x = c('Gt', 't'),
                       by.y = c('G', 't'), all.y = FALSE) 
    res.out.3 <- merge(res.out.2, temp, by = 'Dtg')
    
    res.out.4 <- res.out.3 %>% dplyr::select(-c(Dtg, Label))
    res.out.5 <- res.out.4 %>% rename(group= Gt, time = t)
    res.out.5 <-  res.out.5[order( res.out.5$group,  res.out.5$time), ]
    
    res.out.5$method <- params.did2s$name
    res.out.5$ran <- TRUE
    
    ####### Time to event effects 
    time.to.event <- unique(group.time.out$Var1 - group.time.out$Var2)
    # Get estimates from coefficients from did2s
    coef.did2s <-  coefficients(out.did2s.ell)
    # Get standard errors 
    se.did2s <- se(out.did2s.ell)
    # Get lower and upper CI
    coef.did2s.low <- coef.did2s - qnorm(1-0.05/2) *  se.did2s
    coef.did2s.high <- coef.did2s + qnorm(1-0.05/2) *  se.did2s
    
    temp <- data.frame(att = coef.did2s, 
                       se =  se.did2s, 
                       SimultLowCI = coef.did2s.low, 
                       SimultUppCI = coef.did2s.high, 
                       IndLowCI = coef.did2s.low, 
                       IndUppCI = coef.did2s.high)
    
    temp$timetoev <- rownames(temp)
    temp$timetoev <- gsub('dist_f::','',temp$timetoev)
    rownames(temp) <- NULL
    
    # rearrange order of variables to match other results 
    res.out.reltime <- temp %>% relocate(timetoev)
    res.out.reltime$method <- params.did2s$name
    res.out.reltime$ran <- TRUE
    
    ####### Overall effect
    # Get estimates from coefficients from did2s
    
    # to do: add if else statement here
    # to do: make a function that takes in parameters and returns a null for 
    # each case (marginal, reltime, grouptime)
    # so that we can make this uniform across methods.... 
    
    coef.did2s <- coefficients(out.did2s.aggr)['Gt_t::1'] 
    # Get standard errors 
    se.did2s <- se(out.did2s.aggr)['Gt_t::1']
    # Get lower and upper CI
    coef.did2s.low <- coef.did2s - qnorm(1-0.05/2) *  se.did2s
    coef.did2s.high <- coef.did2s + qnorm(1-0.05/2) *  se.did2s
    # Store results 
    res.out.marginal <- data.frame(att = coef.did2s)
    res.out.marginal$se <- se.did2s
    res.out.marginal$SimultLowCI <- res.out.marginal$att - qnorm(0.975) * res.out.marginal$se 
    res.out.marginal$SimultUppCI <- res.out.marginal$att + qnorm(0.975) * res.out.marginal$se 
    res.out.marginal$IndLowCI <- res.out.marginal$att - qnorm(0.975) * res.out.marginal$se 
    res.out.marginal$IndUppCI <- res.out.marginal$att + qnorm(0.975) * res.out.marginal$se 
    res.out.marginal$method <- params.did2s$name
    res.out.marginal$ran <- TRUE
    
    return(list(clean.res.grouptime = res.out.5,
                clean.res.reltime = res.out.reltime,
                clean.res.marginal = res.out.marginal))
    
  }else{
    
    # If method did not run we store -1 as values in results 
    ####### Group-Time to event effects 
    res.out <- matrix(0, nrow = nrow(group.time.out), ncol = 8)
    res.out[, 1:2] <- cbind(group.time.out[, 1],  
                            group.time.out[, 2])
    res.out[, 3:8] <- -1
    res.out <- data.frame(res.out)
    colnames(res.out) <- c('Gt', 't', 'att', 'se', 'SimultLowCI', 
                           'SimultUppCI', 'IndLowCI', 'IndUppCI')
    res.out <- data.frame(res.out)
    res.out$method <- params.did2s$name
    res.out$ran <- FALSE
    
    ####### Time to event effects 
    time.to.event <- unique(group.time.out$Var1 - group.time.out$Var2)
    res.out.reltime <- matrix(0, nrow = nrow(time.to.event), ncol = 7)
    res.out.reltime[, 1] <-time.to.event
    res.out.reltime[, 2:7] <- -1
    res.out.reltime <- data.frame(res.out.reltime)
    
    colnames(res.out.reltime) <- c('timetoev', 'att', 'se', 'SimultLowCI', 
                                   'SimultUppCI', 'IndLowCI', 'IndUppCI')
    
    res.out.reltime <- data.frame(res.out.reltime)
    res.out.reltime$method <- params.did2s$name
    res.out.reltime$ran <- FALSE
    
    ####### Overall effect
    res.out.marginal <- data.frame(att = -1)
    res.out.marginal$se <- -1
    res.out.marginal$SimultLowCI <- -1
    res.out.marginal$SimultUppCI <- -1
    res.out.marginal$IndLowCI <- -1
    res.out.marginal$IndUppCI <- -1
    res.out.marginal$method <- params.did2s$name
    res.out.marginal$ran <- FALSE
    
    return(list(clean.res.grouptime = res.out,
                clean.res.reltime = res.out.reltime,
                clean.res.marginal = res.out.marginal))
  }
  
}

#######################################################
#### Two Way Mundlack regression wrapper functions
########################################################

# this function comes from the package that has the emfx wrapper function but 
# I added group-time marginalization 
emfx_2 <- function(object, type = c("simple", "group", "calendar", "event", "group-time"), 
          by_xvar = "auto", collapse = "auto", post_only = TRUE, ...) {
  dots = list(...)
  .Dtreat = NULL
  type = match.arg(type)
  etwfe_attr = attr(object, "etwfe")
  gvar = etwfe_attr[["gvar"]]
  tvar = etwfe_attr[["tvar"]]
  ivar = etwfe_attr[["ivar"]]
  xvar = etwfe_attr[["xvar"]]
  gref = etwfe_attr[["gref"]]
  tref = etwfe_attr[["tref"]]
  cgroup = etwfe_attr[["cgroup"]]
  if (!by_xvar %in% c("auto", TRUE, FALSE)) 
    stop("\"by_xvar\" has to be \"auto\", TRUE, or FALSE.")
  if (!collapse %in% c("auto", TRUE, FALSE)) 
    stop("\"collapse\" has to be \"auto\", TRUE, or FALSE.")
  if (isTRUE(by_xvar)) {
    if (is.null(xvar)) {
      warning("An \"xvar\" attribute was not found as part of the supplied model object. ", 
              "(Did your original `etwfe()` call include a valid `xvar = ...` argument?)", 
              "Average margins are reported instead.")
      by_xvar = FALSE
    }
  }
  if (by_xvar == "auto") 
    by_xvar = !is.null(xvar)
  dat = as.data.table(eval(object$call$data, object$call_env))
  if ("group" %in% names(dat)) 
    dat[["group"]] = NULL
  nrows = NULL
  if (collapse == "auto") {
    nrows = nrow(dat)
    if (nrows >= 5e+05) {
      collapse = TRUE
    }
    else {
      collapse = FALSE
    }
  }
  if (cgroup == "never") {
    dat = dat[dat[[gvar]] != gref, , drop = FALSE]
    if (type != "event") {
      dat = dat[dat[[".Dtreat"]], , drop = FALSE]
      dat = dat[dat[[tvar]] >= dat[[gvar]], , drop = FALSE]
    }
  }
  else if (type == "event" & isFALSE(post_only)) {
    dat = dat[dat[[gvar]] != gref, , drop = FALSE]
  }
  else if (".Dtreat" %in% names(dat)) {
    dat = dat[dat[[".Dtreat"]], , drop = FALSE]
  }
  if (collapse & is.null(ivar)) {
    if (by_xvar) {
      dat_weights = dat[(.Dtreat)][, .N, by = c(gvar, 
                                                tvar, xvar)]
    }
    else {
      dat_weights = dat[(.Dtreat)][, .N, by = c(gvar, 
                                                tvar)]
    }
    if (!is.null(nrows) && nrows > 5e+05) 
      warning("\nNote: Dataset larger than 500k rows detected. The data will be ", 
              "collapsed by period-cohort groups to reduce estimation times. ", 
              "However, this shortcut can reduce the accuracy of the reported ", 
              "marginal effects. ", "To override this default behaviour, specify: ", 
              "`emfx(..., collapse = FALSE)`\n")
    dat = dat[(.Dtreat)][, lapply(.SD, mean), by = c(gvar, 
                                                     tvar, xvar, ".Dtreat")]
    dat = setDT(dat)[, merge(.SD, dat_weights, all.x = TRUE)]
  }
  else if (collapse & !is.null(ivar)) {
    warning("\"ivar\" is not NULL. Marginal effects are calculated without collapsing.")
    dat$N = 1L
  }
  else {
    dat$N = 1L
  }
  if (type == "simple") {
    by_var = ".Dtreat"
  }
  else if (type == "group") {
    by_var = gvar
  }
  else if (type == "calendar") {
    by_var = tvar
  }
  else if (type == "event") {
    dat[["event"]] = dat[[tvar]] - dat[[gvar]]
    by_var = "event"
  }else if(type == "group-time"){
    by_var = c('t', 'Gt')
  }
  if (by_xvar) 
    by_var = c(by_var, xvar)
  #browser()
  mfx = slopes(object, newdata = dat, wts = "N", variables = ".Dtreat", 
               by = by_var,...)
  return(mfx)
}

fit_etwfe <- function(longdat, params.method, group, time){
  
  clustervar <- params.method$clustervar
  t_var <- params.method$tvar
  grp_ind <- params.method$grp_ind
  outcomename <- params.method$outcomename
  formula_method <- as.formula(paste(outcomename, params.method$form, sep = "~")) 

  if(clustervar == ''){
    out.etwfe <- etwfe(
      fml  =  formula_method, # outcome ~ controls
      tvar = t_var ,  # time variable
      gvar = grp_ind, # group variable
      data = longdat,  # dataset
    )
    
    out.etwfe.aggr <- emfx_2(out.etwfe)
    out.etwfe.ell <- emfx_2(out.etwfe, type = "event")
    out.etwfe.grptime <- emfx_2(out.etwfe, type = "simple")
    out.etwfe.grptime <- emfx_2(out.etwfe, type = "group-time")
    
  }else{
    
    out.etwfe <- etwfe(
      fml  = formula_method, # outcome ~ controls
      tvar = t_var,  # time variable
      gvar = grp_ind, # group variable
      data = longdat,       # dataset
      vcov = ~ C   # vcov adjustment (here: clustered)
    )
    
    out.etwfe.aggr <- emfx_2(out.etwfe)
    out.etwfe.ell <- emfx_2(out.etwfe, type = "event")
    out.etwfe.grptime <- emfx_2(out.etwfe, type = "group-time")
    out.etwfe.grp <- emfx_2(out.etwfe, type = "group")
  }
  
  return(list(out.etwfe = out.etwfe, 
              out.etwfe.aggr = out.etwfe.aggr, 
              out.etwfe.ell = out.etwfe.ell, 
              out.etwfe.grptime = out.etwfe.grptime,
              out.etwfe.grp =  out.etwfe.grp) ) 
  
}

etwfe_wrapper <- function(longdat, all_params, 
                          group, time, store_dist_vals){
  
  res.etwfe <- list()
  clean.res.etwfe.grouptime <- list()
  clean.res.etwfe.group <- list()
  clean.res.etwfe.reltime <- list()
  clean.res.etwfe.marginal <- list()
  
  j <- 1
  
  for(i in 1:length(all_params)){
    if(!is.null(all_params[[i]])){
      # we may have null all_params (for instance run one version of the method,
      # but not another)
      # so j needs to be running in a different script
      res.etwfe[[j]] <- tryCatch({
        # Code that may potentially throw an error
        result <- fit_etwfe(longdat, all_params[[i]], group, time)
      }, error = function(e) {
        # Code to handle the error
        print(paste("An error occurred:", e))
        # Define the object to return in case of an error
        result <- -1
        return(result)
      })
      
      clean.res.etwfe <- clean_res_etwfe(res.etwfe[[j]], 
                                              all_params[[i]],
                                              group, time,
                                              store_dist_vals)
      
      
      clean.res.etwfe.grouptime[[j]] <- clean.res.etwfe$clean.res.grouptime 
      clean.res.etwfe.group[[j]] <- clean.res.etwfe$clean.res.group 
      clean.res.etwfe.reltime[[j]] <- clean.res.etwfe$clean.res.reltime
      clean.res.etwfe.marginal[[j]] <- clean.res.etwfe$clean.res.marginal
      
      j <- j + 1
    }
  }

  return(list(clean.res.grouptime = clean.res.etwfe.grouptime,
              clean.res.group = clean.res.etwfe.group, 
              clean.res.reltime = clean.res.etwfe.reltime,
              clean.res.marginal = clean.res.etwfe.marginal))
  
}

clean_res_etwfe_2 <- function(out.etwfe, params.etwfe, 
                            group, time, store_dist_vals){
  
  # this function returns the parameters of interest
  group.time.out <- expand.grid(group, time)
  group.out <- data.frame(group.time.out)
  time.out <- data.frame(group.time.out)
  names(group.out) <- c('G','t')
  names(time.out) <- c('G','t')
  time.out$Dgt <- time.out$G - time.out$t
  time.out <- time.out %>% dplyr::select(Dgt)
  group.out <- group.out %>% dplyr::select(G)

  if(!is.numeric(out.etwfe)){
    
    ####### Group time fit
    
    # extract results
    group_res <- out.etwfe$out.etwfe.grptime$Gt
    time_res <-  out.etwfe$out.etwfe.grptime$t
    att_res <-  out.etwfe$out.etwfe.grptime$estimate
    se_res <- out.etwfe$out.etwfe.grptime$std.error
    group.time.fit <- cbind(group_res, time_res)
    
    # construct output
    rows.out.group.time <- compare_rows(group.time.fit, group.time.out)
    res.out <- matrix(0, nrow = sum(rows.out.group.time), ncol = 8)
    res.out[, 1:4] <- cbind(group_res, time_res, att_res, se_res)
    res.out[, 5] <- att_res - qnorm(0.975) * se_res
    res.out[, 6] <- att_res +  qnorm(0.975) * se_res
    res.out[, 7] <- att_res - qnorm(0.975) * se_res
    res.out[, 8] <- att_res + qnorm(0.975) * se_res
    
    # filter rows
    #if(sum(rows.out.group.time) == 1){
    #res.out <- res.out
    #}else{
     # res.out <-  res.out[rows.out.group.time, ]
    #}
    
    res.out.2 <- as.data.frame(res.out)
    colnames(res.out.2) <- c('group', 'time', 'att', 
                           'se', 'SimultLowCI', 'SimultUppCI', 
                           'IndLowCI', 'IndUppCI')

    res.out.2$method <- params.etwfe$name
    res.out.2$ran <- TRUE
    
    ####### Group effects
    
    # extract results
    group_res <- out.etwfe$out.etwfe.grp$Gt
    att_res <-  out.etwfe$out.etwfe.grp$estimate
    se_res <- out.etwfe$out.etwfe.grp$std.error
    
    # construct output
    res.out.group <- matrix(0, nrow = length(group_res), ncol = 7)
    res.out.group[, 1:3] <- cbind(group_res, att_res, se_res)
    res.out.group[, 4] <- att_res - qnorm(0.975) * se_res
    res.out.group[, 5] <- att_res +  qnorm(0.975) * se_res
    res.out.group[, 6] <- att_res - qnorm(0.975) * se_res
    res.out.group[, 7] <- att_res + qnorm(0.975) * se_res
    
    colnames(res.out.group) <- c('group', 'att', 
                                 'se', 'SimultLowCI', 'SimultUppCI', 
                                 'IndLowCI', 'IndUppCI')
    
    
    res.out.group.2 <- data.frame(res.out.group)
    res.out.group.2$method <- params.etwfe$name
    res.out.group.2$ran <- TRUE
    
    ####### Time to event effects
    
    # extract results
    time_res <- out.etwfe$out.etwfe.ell$event
    att_res <- out.etwfe$out.etwfe.ell$estimate
    se_res <- out.etwfe$out.etwfe.ell$std.error
    
    # construct output
    res.out.reltime <- matrix(0, nrow = length(time_res), ncol = 7)
    res.out.reltime[, 1:3] <- cbind(time_res, att_res, se_res)
    res.out.reltime[, 4] <- att_res - qnorm(0.975) * se_res
    res.out.reltime[, 5] <- att_res + qnorm(0.975) * se_res
    res.out.reltime[, 6] <- att_res - qnorm(0.975) * se_res
    res.out.reltime[, 7] <- att_res + qnorm(0.975) * se_res
    
    colnames(res.out.reltime) <-c('timetoev', 'att', 
                                   'se', 'SimultLowCI', 'SimultUppCI', 
                                   'IndLowCI', 'IndUppCI')
    
    res.out.reltime.2 <- data.frame(res.out.reltime)
    res.out.reltime.2$method <- params.etwfe$name
    res.out.reltime.2$ran <- TRUE
    
    ####### Overall effect
    
    # extract results
    att_res <-  out.etwfe$out.etwfe.aggr$estimate
    se_res <- out.etwfe$out.etwfe.aggr$std.error
    
    res.out.marginal <- data.frame(att = att_res)
    res.out.marginal$se <- se_res
    res.out.marginal$SimultLowCI <-  att_res - qnorm(0.975) * se_res
    res.out.marginal$SimultUppCI <- att_res + qnorm(0.975) * se_res
    res.out.marginal$IndLowCI <- att_res - qnorm(0.975) * se_res
    res.out.marginal$IndUppCI <- att_res + qnorm(0.975) * se_res
    res.out.marginal$method <- params.etwfe$name
    res.out.marginal$ran <- TRUE
    
    return(list(clean.res.grouptime = res.out.2,
                clean.res.group = res.out.group.2, 
                clean.res.reltime = res.out.reltime.2,
                clean.res.marginal = res.out.marginal))
    
  }else{
    
    
    #### Group time effects 
    rows_out <- group.time.out
    res.out <- matrix(0, nrow = nrow(rows_out), ncol = 8)
    colnames(res.out) <- c('group', 'time', 'att', 
                           'se', 'SimultLowCI', 'SimultUppCI', 
                           'IndLowCI', 'IndUppCI')
    
    res.out[, 1:2] <- as.matrix(group.time.out)
    res.out[, 3:8] <- matrix(-1, nrow = nrow(rows_out), ncol = 6)
    res.out <- data.frame(res.out)
    res.out$method <- params.etwfe$name
    res.out$ran <- FALSE
    
    ##### Group effects
    group.fit <- unique(group.out$G)
    res.out.group<- matrix(0, nrow = length(group.fit), ncol = 7)
    
    colnames(res.out.group) <- c('group', 'att', 
                                 'se', 'SimultLowCI', 'SimultUppCI', 
                                 'IndLowCI', 'IndUppCI')
    
    res.out.group[, 1] <- as.matrix(group.fit)
    res.out.group[, 2:7] <- matrix(-1, nrow = length(group.fit), ncol = 6)
    res.out.group <- data.frame(res.out.group)
    res.out.group$method <- params.etwfe$name
    res.out.group$ran <- FALSE
    
    ##### Time to event effects 
    time.fit <- unique(time.out$Dgt)
    res.out.reltime <- matrix(0, nrow = length(time.fit), ncol = 7)
    
    colnames(res.out.reltime) <- c('timetoev', 'att', 
                                    'se', 'SimultLowCI', 'SimultUppCI', 
                                    'IndLowCI', 'IndUppCI')
    
    res.out.reltime[, 1] <- as.matrix(time.fit)
    res.out.reltime[, 2:7] <- matrix(-1, nrow = length(time.fit), ncol = 6)
    res.out.reltime <- data.frame(res.out.reltime)
    res.out.reltime$method <- params.etwfe$name
    res.out.reltime$ran <- FALSE
    
    ####### Overall effect
    res.out.marginal <- data.frame(att = -1)
    res.out.marginal$se <- -1
    res.out.marginal$SimultLowCI <- -1
    res.out.marginal$SimultUppCI <- -1
    res.out.marginal$IndLowCI <- -1
    res.out.marginal$IndUppCI <- -1
    res.out.marginal$method <- params.etwfe$name
    res.out.marginal$ran <- FALSE
    
    return(list(clean.res.grouptime = res.out,
                clean.res.group = res.out.group, 
                clean.res.reltime = res.out.reltime,
                clean.res.marginal = res.out.marginal))
  }
}

clean_res_etwfe <- function(out.etwfe, params.etwfe, 
                            group, time, store_dist_vals){
  
  # this function returns the parameters of interest
  group.time.out <- expand.grid(group, time)
  group.out <- data.frame(group.time.out)
  time.out <- data.frame(group.time.out)
  names(group.out) <- c('G','t')
  names(time.out) <- c('G','t')
  time.out$Dgt <- time.out$G - time.out$t
  time.out <- time.out %>% dplyr::select(Dgt)
  group.out <- group.out %>% dplyr::select(G)
  
  if(!is.numeric(out.etwfe)){
    
    ####### Group time fit
    
    # extract results
    group_res <- out.etwfe$out.etwfe.grptime$Gt
    time_res <-  out.etwfe$out.etwfe.grptime$t
    att_res <-  out.etwfe$out.etwfe.grptime$estimate
    se_res <- out.etwfe$out.etwfe.grptime$std.error
    group.time.fit <- cbind(group_res, time_res)
    
    # construct output
    rows.out.group.time <- compare_rows(group.time.fit, group.time.out)
    res.out <- matrix(0, nrow = sum(rows.out.group.time), ncol = 8)
    res.out[, 1:4] <- cbind(group_res[rows.out.group.time], 
                            time_res[rows.out.group.time], 
                            att_res[rows.out.group.time], 
                            se_res[rows.out.group.time])
    
    res.out[, 5] <- att_res[rows.out.group.time] - qnorm(0.975) * se_res[rows.out.group.time]
    res.out[, 6] <- att_res[rows.out.group.time] +  qnorm(0.975) * se_res[rows.out.group.time]
    res.out[, 7] <- att_res[rows.out.group.time] - qnorm(0.975) * se_res[rows.out.group.time]
    res.out[, 8] <- att_res[rows.out.group.time] + qnorm(0.975) * se_res[rows.out.group.time]
    
    # filter rows
    # if(sum(rows.out.group.time) == 1){
    #   res.out <- res.out
    # }else{
    # res.out <-  res.out[rows.out.group.time, ]
    #}
    res.out.2 <- as.data.frame(res.out)
    colnames(res.out.2) <- c('group', 'time', 'att', 
                             'se', 'SimultLowCI', 'SimultUppCI', 
                             'IndLowCI', 'IndUppCI')
    
    res.out.2$method <- params.etwfe$name
    res.out.2$ran <- TRUE
    
    ####### Group effects
    
    # extract results
    group_res <- out.etwfe$out.etwfe.grp$Gt
    att_res <-  out.etwfe$out.etwfe.grp$estimate
    se_res <- out.etwfe$out.etwfe.grp$std.error
    
    # construct output
    res.out.group <- matrix(0, nrow = length(group_res), ncol = 7)
    res.out.group[, 1:3] <- cbind(group_res, att_res, se_res)
    res.out.group[, 4] <- att_res - qnorm(0.975) * se_res
    res.out.group[, 5] <- att_res +  qnorm(0.975) * se_res
    res.out.group[, 6] <- att_res - qnorm(0.975) * se_res
    res.out.group[, 7] <- att_res + qnorm(0.975) * se_res
    
    colnames(res.out.group) <- c('group', 'att', 
                                 'se', 'SimultLowCI', 'SimultUppCI', 
                                 'IndLowCI', 'IndUppCI')
    
    
    res.out.group.2 <- data.frame(res.out.group)
    res.out.group.2$method <- params.etwfe$name
    res.out.group.2$ran <- TRUE
    
    ####### Time to event effects
    
    # extract results
    time_res <- out.etwfe$out.etwfe.ell$event
    att_res <- out.etwfe$out.etwfe.ell$estimate
    se_res <- out.etwfe$out.etwfe.ell$std.error
    
    # construct output
    res.out.reltime <- matrix(0, nrow = length(time_res), ncol = 7)
    res.out.reltime[, 1:3] <- cbind(time_res, att_res, se_res)
    res.out.reltime[, 4] <- att_res - qnorm(0.975) * se_res
    res.out.reltime[, 5] <- att_res + qnorm(0.975) * se_res
    res.out.reltime[, 6] <- att_res - qnorm(0.975) * se_res
    res.out.reltime[, 7] <- att_res + qnorm(0.975) * se_res
    
    colnames(res.out.reltime) <-c('timetoev', 'att', 
                                  'se', 'SimultLowCI', 'SimultUppCI', 
                                  'IndLowCI', 'IndUppCI')
    
    res.out.reltime.2 <- data.frame(res.out.reltime)
    res.out.reltime.2$method <- params.etwfe$name
    res.out.reltime.2$ran <- TRUE
    
    ####### Overall effect
    
    # extract results
    att_res <-  out.etwfe$out.etwfe.aggr$estimate
    se_res <- out.etwfe$out.etwfe.aggr$std.error
    
    res.out.marginal <- data.frame(att = att_res)
    res.out.marginal$se <- se_res
    res.out.marginal$SimultLowCI <-  att_res - qnorm(0.975) * se_res
    res.out.marginal$SimultUppCI <- att_res + qnorm(0.975) * se_res
    res.out.marginal$IndLowCI <- att_res - qnorm(0.975) * se_res
    res.out.marginal$IndUppCI <- att_res + qnorm(0.975) * se_res
    res.out.marginal$method <- params.etwfe$name
    res.out.marginal$ran <- TRUE
    
    return(list(clean.res.grouptime = res.out.2,
                clean.res.group = res.out.group.2, 
                clean.res.reltime = res.out.reltime.2,
                clean.res.marginal = res.out.marginal))
    
  }else{
  
    res.out <- NULL
    res.out.group <- NULL
    res.out.reltime <- NULL
    res.out.marginal <- NULL
    
    return(list(clean.res.grouptime = res.out,
                clean.res.group = res.out.group, 
                clean.res.reltime = res.out.reltime,
                clean.res.marginal = res.out.marginal))
  }
}

#######################################################
#### Sun and Abraham regression wrapper functions
########################################################

fit_sunab <- function(longdat, params.method, group, time){
  
  clustervar <- params.method$clustervar
  formula_method <- params.method$form
  
  if(clustervar == ''){
    
    # event time
    out.sunab.ell <- feols(formula_method, 
                           data = longdat)
    # overall ATT
    #out.sunab.aggr <- summary(out.sunab.ell, agg = 'att')
    out.sunab.aggr <- aggregate(out.sunab.ell,  c("ATT" = "dist::[^-]"))
    
    # aggregate event timet effects 
    d0 <- aggregate(out.sunab.ell,  c("dist::0" = "dist::0"))
    d1 <- aggregate(out.sunab.ell,  c("dist::1" = "dist::1"))
    out.sunab.ell.d <- data.frame(rbind(d0,d1))
    names(out.sunab.ell.d)[1:2] <- c('Estimate', 'StdError')

  }else{
    
    # time to event 
    out.sunab.ell <- feols(formula_method, 
                           data = longdat, 
                           vcov = clustervar)

    # marginalize group time effects
    # g3t3 <- aggregate(out.sunab.ell,  c("3:3" = "dist::0:Gt::3"))
    # g3t4 <- aggregate(out.sunab.ell,  c("3:4" = "dist::1:Gt::3"))
    # g4t4 <- aggregate(out.sunab.ell,  c("4:4" = "dist::0:Gt::4"))
    # 
    # out.sunab.gt <- data.frame(rbind(g3t3, g3t4, g4t4))
    # names(out.sunab.gt)[1:2] <- c('Estimate', 'StdError')
    
    # Function to safely aggregate and return NULL if an error occurs
    safe_aggregate <- function(data, mapping) {
      tryCatch(
        {
          aggregate(data, mapping)
        },
        error = function(e) {
          message(paste("Aggregation failed for:", deparse(substitute(mapping)), " - Skipping"))
          return(NULL)
        }
      )
    }
    
    # Attempt to aggregate data
    g3t3 <- safe_aggregate(out.sunab.ell, c("3:3" = "dist::0:Gt::3"))
    g3t4 <- safe_aggregate(out.sunab.ell, c("3:4" = "dist::1:Gt::3"))
    g4t4 <- safe_aggregate(out.sunab.ell, c("4:4" = "dist::0:Gt::4"))
    
    # Filter out NULL values
    aggregated_list <- Filter(Negate(is.null), list(g3t3, g3t4, g4t4))
    
    # Bind only the successful aggregations
    if (length(aggregated_list) > 0) {
      out.sunab.gt <- data.frame(do.call(rbind, aggregated_list))
      names(out.sunab.gt)[1:2] <- c('Estimate', 'StdError')
    } else {
      message("No valid aggregations found.")
      out.sunab.gt <- data.frame()  # Return an empty data frame if nothing succeeds
    }
    
    
    # aggregate time to event effects 
    # d0 <- aggregate(out.sunab.ell,  c("dist::0" = "dist::0"))
    # d1 <- aggregate(out.sunab.ell,  c("dist::1" = "dist::1"))
    # out.sunab.ell.d <- data.frame(rbind(d0,d1))
    # names(out.sunab.ell.d)[1:2] <- c('Estimate', 'StdError')
    
    # Attempt to aggregate data
    d0 <- safe_aggregate(out.sunab.ell, c("dist::0" = "dist::0"))
    d1 <- safe_aggregate(out.sunab.ell, c("dist::1" = "dist::1"))
    
    # Filter out NULL values
    aggregated_list_d <- Filter(Negate(is.null), list(d0, d1))
    
    # Bind only the successful aggregations
    if (length(aggregated_list_d) > 0) {
      out.sunab.ell.d <- data.frame(do.call(rbind, aggregated_list_d))
      names(out.sunab.ell.d)[1:2] <- c('Estimate', 'StdError')
    } else {
      message("No valid aggregations found for time-to-event effects.")
      out.sunab.ell.d <- data.frame()  # Return an empty data frame if nothing succeeds
    }
    
    
    # overall ATT
    #out.sunab.aggr <- summary(out.sunab.ell, agg = 'att') 
    out.sunab.aggr <- data.frame(aggregate(out.sunab.ell,  
                                           c("ATT" = "dist::[^-]")))
    names(out.sunab.aggr)[1:2] <- c('Estimate', 'StdError')
    
  }
  
  return(list(out.sunab.gt = out.sunab.gt,
              out.sunab.ell = out.sunab.ell.d, 
              out.sunab.aggr = out.sunab.aggr)) 
  
}

sunab_wrapper <- function(longdat, all_params, 
                          group, time, store_dist_vals){
  
  res.sunab <- list()
  
  clean.res.sunab.reltime <- list()
  clean.res.sunab.marginal <- list()
  clean.res.sunab.grouptime <- list()
  j <- 1
  
  # If there are no never-treated  then we may set C = {max{Ei}}, i.e.
  # the latest-treated cohort and estimate regression on observations 
  # from t = 0, . . . , max{Ei} − 1. 
  longdat_2 <- longdat %>% filter(t != max(Gt))
  
  for(i in 1:length(all_params)){
    if(!is.null(all_params[[i]])){
      # we may have null all_params (for instance run one version of the method but not another)
      # so j needs to be running in a different script
      res.sunab[[j]] <- tryCatch({
        # Code that may potentially throw an error
        result <- fit_sunab(longdat_2, all_params[[i]], group, time)
      }, error = function(e) {
        # Code to handle the error
        print(paste("An error occurred:", e))
        # Define the object to return in case of an error
        result <- 'err'
        return(result)
      })
      
      clean.res.sunab <- clean_res_sunab(res.sunab[[j]], all_params[[i]], 
                                         group, time, store_dist_vals)
      
      clean.res.sunab.grouptime[[j]] <- clean.res.sunab$clean.res.grouptime
      clean.res.sunab.reltime[[j]] <- clean.res.sunab$clean.res.reltime
      clean.res.sunab.marginal[[j]] <- clean.res.sunab$clean.res.marginal
      
      j <- j + 1
    }
  }
  
  return(list(clean.res.grouptime =  clean.res.sunab.grouptime,
              clean.res.reltime = clean.res.sunab.reltime,
              clean.res.marginal = clean.res.sunab.marginal))
  
}

clean_res_sunab <- function(out.sunab.all, params.sunab, 
                            group, time, store_dist_vals){
  
  # Match estimate and CI of each twfe estimate to store_dist_vals
  # Match group & time to store_dist_vals
  # Return matrix of group, time, and estimates as well as 95%CI
  # As many as unique values in distance values matrix estimates to return with this model
  out.sunab.gt <- out.sunab.all$out.sunab.gt
  out.sunab.ell <- out.sunab.all$out.sunab.ell
  out.sunab.aggr <- out.sunab.all$out.sunab.aggr
  group.time.out <- expand.grid(group, time)
  
  #browser()
  if(!is.character(out.sunab.ell)){
    
    # #### group time 
    # Get estimates from coefficients from sunab
    coef.sunab <-  out.sunab.gt$Estimate
    # # Get standard errors
    se.sunab <- out.sunab.gt$StdError
    # # Get lower and upper CI
    coef.sunab.low <- coef.sunab - qnorm(1-0.05/2) *  se.sunab
    coef.sunab.high <- coef.sunab + qnorm(1-0.05/2) *  se.sunab
    
    temp <- data.frame(att = coef.sunab,
                      se =  se.sunab,
                      SimultLowCI = coef.sunab.low,
                      SimultUppCI = coef.sunab.high,
                      IndLowCI = coef.sunab.low,
                      IndUppCI = coef.sunab.high)
    
    temp$gt <- rownames(out.sunab.gt) 
    temp <-  temp %>% separate(gt, into = c("group", "time"), 
                               sep = ":", convert = TRUE)  %>% # Move the new columns to the left
    select(group, time, everything())
    
    res.out.gt <- as.data.frame(temp)
    colnames(res.out.gt) <- c('group', 'time', 'att', 
                             'se', 'SimultLowCI', 'SimultUppCI', 
                             'IndLowCI', 'IndUppCI')
    
    res.out.gt$method <- params.sunab$name
    res.out.gt$ran <- TRUE
    
    ####### Time to event effects 
    time.to.event <- unique(group.time.out$Var1 - group.time.out$Var2)
    # Get estimates from coefficients from sunab
    
    est.sunab <-  out.sunab.ell$Estimate
    # Get standard errors 
    se.sunab <-  out.sunab.ell$StdError
    # Get lower and upper CI
    est.sunab.low <- est.sunab - qnorm(1-0.05/2) *  se.sunab
    est.sunab.high <- est.sunab + qnorm(1-0.05/2) *  se.sunab
    
    temp <- data.frame(att = est.sunab, 
                       se =  se.sunab, 
                       SimultLowCI = est.sunab.low, 
                       SimultUppCI = est.sunab.high, 
                       IndLowCI = est.sunab.low, 
                       IndUppCI = est.sunab.high)
    
    temp$timetoev <- rownames(out.sunab.ell)
    temp$timetoev <- gsub('dist::','',temp$timetoev)
    rownames(temp) <- NULL
    
    # rearrange order of variables to match other results 
    res.out.reltime <- temp %>% relocate(timetoev)
    res.out.reltime$method <- params.sunab$name
    res.out.reltime$ran <- TRUE
    
    ####### Overall effect
    # Get estimates from coefficients from sunab
    est.sunab <- out.sunab.aggr$Estimate
    # Get standard errors 
    se.sunab <- out.sunab.aggr$StdError
    # Get lower and upper CI
    est.sunab.low <- est.sunab - qnorm(1-0.05/2) *  se.sunab
    est.sunab.high <- est.sunab + qnorm(1-0.05/2) *  se.sunab
    # Store results 
    res.out.marginal <- data.frame(att = est.sunab)
    res.out.marginal$se <- se.sunab
    res.out.marginal$SimultLowCI <- res.out.marginal$att - qnorm(0.975) * res.out.marginal$se 
    res.out.marginal$SimultUppCI <- res.out.marginal$att + qnorm(0.975) * res.out.marginal$se 
    res.out.marginal$IndLowCI <- res.out.marginal$att - qnorm(0.975) * res.out.marginal$se 
    res.out.marginal$IndUppCI <- res.out.marginal$att + qnorm(0.975) * res.out.marginal$se 
    res.out.marginal$method <- params.sunab$name
    res.out.marginal$ran <- TRUE
    
    return(list(clean.res.grouptime = res.out.gt, 
      clean.res.reltime = res.out.reltime,
      clean.res.marginal = res.out.marginal))
    
  }else{
    
    res.out.gt <- NULL
    res.out.reltime <- NULL
    res.out.marginal <- NULL
    
    return(list(clean.res.grouptime = res.out.gt,
      clean.res.reltime = res.out.reltime,
      clean.res.marginal = res.out.marginal))
  }
  
}

# make_dat_antonelli <- function(wide_dat, params){
#   
#   outcomes <-  as.matrix(wide_dat[, grep('Y_', colnames(wide_dat))])
#   treatments <- as.matrix(wide_dat[, grep('treat_', colnames(wide_dat))])
#   covariates_X <- as.matrix(wide_dat[, grep('X[0123456789]_1', colnames(wide_dat))])
#   covariates_V <- as.matrix(wide_dat[, grep('V[0123456789]_1', colnames(wide_dat))])
#   if(params$covars.cluster){
#     covariates <- cbind(covariates_X, covariates_V)
#   }else{
#     covariates <- covariates_X
#   }
#   
#   out = list(outcomes = outcomes,
#              treatments = treatments,
#              covariates = covariates,
#              covariates_X = covariates_X, 
#              covariates_V = covariates_V)
#   
#   return(out)
#   
# }
# 
# fit_antonelli <- function(dat, group, time, params){
#   
#   outcomes <- dat$outcomes
#   treatments <- dat$treatments
#   covariates <- dat$covariates
#   zeroMat <- params$zeroMat
#   
#   modelFit = HeterogeneousTEpanel(outcomes=outcomes,
#                                   treatments=treatments,
#                                   covariates=covariates,
#                                   nTimesOut = 10,
#                                   nScans = 1000, 
#                                   nBurn=500, 
#                                   thin=4,
#                                   zeroMat = zeroMat,
#                                   smoothEffects = FALSE)
#   
#   
# }

# pre_process_did <- function (yname, tname, idname, gname, xformla = NULL, data, 
#           panel = TRUE, allow_unbalanced_panel, 
#           control_group = c("nevertreated", "notyettreated"), 
#           anticipation = 0, weightsname = NULL, 
#           alp = 0.05, bstrap = FALSE, cband = FALSE, biters = 1000, 
#           clustervars = NULL, est_method = "dr", base_period = "varying", 
#           print_details = TRUE, pl = FALSE, cores = 1, call = NULL) {
#   control_group <- control_group[1]
#   if (!(control_group %in% c("nevertreated", "notyettreated"))) {
#     stop("control_group must be either 'nevertreated' or 'notyettreated'")
#   }
#   if (!all(class(data) == "data.frame")) {
#     data <- as.data.frame(data)
#   }
#   if (!(is.numeric(data[, tname]))) 
#     stop("data[, tname] must be numeric")
#   if (!(is.numeric(data[, gname]))) 
#     stop("data[, gname] must be numeric")
#   if (is.null(xformla)) {
#     xformla <- ~1
#   }
#   data <- cbind.data.frame(data[, c(idname, tname, yname, gname, 
#                                     weightsname, clustervars)], model.frame(xformla, data = data, 
#                                                                             na.action = na.pass))
#   n_orig <- nrow(data)
#   data <- data[complete.cases(data), ]
#   n_diff <- n_orig - nrow(data)
#   if (n_diff != 0) {
#     warning(paste0("dropped ", n_diff, " rows from original data due to missing data"))
#   }
#   ifelse(is.null(weightsname), w <- rep(1, nrow(data)), w <- data[, 
#                                                                   weightsname])
#   if (".w" %in% colnames(data)) 
#     stop("`did` tried to use column named \".w\" internally, but there was already a column with this name")
#   data$.w <- w
#   tlist <- unique(data[, tname])[order(unique(data[, tname]))]
#   asif_never_treated <- (data[, gname] > max(tlist, na.rm = TRUE))
#   asif_never_treated[is.na(asif_never_treated)] <- FALSE
#   data[asif_never_treated, gname] <- 0
#   glist <- unique(data[, gname], )[order(unique(data[, gname]))]
#   if (length(glist[glist == 0]) == 0) {
#     if (control_group == "nevertreated") {
#       stop("There is no available never-treated group")
#     }
#     else {
#       data <- subset(data, (data[, tname] < (max(glist) - 
#                                                anticipation)))
#       tlist <- sort(unique(data[, tname]))
#       glist <- sort(unique(data[, gname]))
#       #glist <- glist[glist < max(glist)]
#     }
#   }
#   glist <- glist[glist > 0]
#   first.period <- tlist[1]
#   glist <- glist[glist > first.period + anticipation]
#   treated_first_period <- (data[, gname] <= first.period) & 
#     (!(data[, gname] == 0))
#   treated_first_period[is.na(treated_first_period)] <- FALSE
#   nfirstperiod <- ifelse(panel, length(unique(data[treated_first_period, 
#   ][, idname])), nrow(data[treated_first_period, ]))
#   if (nfirstperiod > 0) {
#     warning(paste0("Dropped ", nfirstperiod, " units that were already treated in the first period."))
#     data <- data[data[, gname] %in% c(0, glist), ]
#     tlist <- unique(data[, tname])[order(unique(data[, tname]))]
#     glist <- unique(data[, gname], )[order(unique(data[, 
#                                                        gname]))]
#     glist <- glist[glist > 0]
#     first.period <- tlist[1]
#     glist <- glist[glist > first.period + anticipation]
#   }
#   if (!is.null(idname)) {
#     if (!(is.numeric(data[, idname]))) 
#       stop("data[, idname] must be numeric")
#   }
#   true_repeated_cross_sections <- FALSE
#   if (!panel) {
#     true_repeated_cross_sections <- TRUE
#   }
#   if (panel) {
#     if (allow_unbalanced_panel) {
#       panel <- FALSE
#       true_repeated_cross_sections <- FALSE
#       if (!is.numeric(data[, idname])) {
#         stop("Must provide a numeric id")
#       }
#     }
#     else {
#       keepers <- complete.cases(data)
#       n <- length(unique(data[, idname]))
#       n.keep <- length(unique(data[keepers, idname]))
#       if (nrow(data[keepers, ]) < nrow(data)) {
#         warning(paste0("Dropped ", (n - n.keep), " observations that had missing data."))
#         data <- data[keepers, ]
#       }
#       n.old <- length(unique(data[, idname]))
#       data <- BMisc::makeBalancedPanel(data, idname, tname)
#       n <- length(unique(data[, idname]))
#       if (n < n.old) {
#         warning(paste0("Dropped ", n.old - n, " observations while converting to balanced panel."))
#       }
#       if (nrow(data) == 0) {
#         stop("All observations dropped to converted data to balanced panel. Consider setting `panel = FALSE' and/or revisit 'idname'.")
#       }
#       n <- nrow(data[data[, tname] == tlist[1], ])
#     }
#   }
#   if (!panel) {
#     keepers <- complete.cases(data)
#     if (nrow(data[keepers, ]) < nrow(data)) {
#       warning(paste0("Dropped ", nrow(data) - nrow(data[keepers, 
#       ]), " observations that had missing data."))
#       data <- data[keepers, ]
#     }
#     if (nrow(data) == 0) {
#       stop("All observations dropped due to missing data problems.")
#     }
#     if (true_repeated_cross_sections) {
#       data$.rowid <- seq(1:nrow(data))
#       idname <- ".rowid"
#     }
#     else {
#       data$.rowid <- data[, idname]
#     }
#     n <- length(unique(data[, idname]))
#   }
#   if (length(glist) == 0) {
#     stop("No valid groups. The variable in 'gname' should be expressed as the time a unit is first treated (0 if never-treated).")
#   }
#   if (length(tlist) == 2) {
#     cband <- FALSE
#   }
#   gsize <- aggregate(data[, gname], by = list(data[, gname]), 
#                      function(x) length(x)/length(tlist))
#   reqsize <- length(BMisc::rhs.vars(xformla)) + 5
#   gsize <- subset(gsize, x < reqsize)
#   if (nrow(gsize) > 0) {
#     gpaste <- paste(gsize[, 1], collapse = ",")
#     warning(paste0("Be aware that there are some small groups in your dataset.\n  Check groups: ", 
#                    gpaste, "."))
#     if ((0 %in% gsize[, 1]) & (control_group == "nevertreated")) {
#       stop("never treated group is too small, try setting control_group=\"notyettreated\"")
#     }
#   }
#   nT <- length(tlist)
#   nG <- length(glist)
#   data <- data[order(data[, idname], data[, tname]), ]
#   dp <- DIDparams(yname = yname, tname = tname, idname = idname, 
#                   gname = gname, xformla = xformla, data = as.data.frame(data), 
#                   control_group = control_group, anticipation = anticipation, 
#                   weightsname = weightsname, alp = alp, bstrap = bstrap, 
#                   biters = biters, clustervars = clustervars, cband = cband, 
#                   print_details = print_details, pl = pl, cores = cores, 
#                   est_method = est_method, base_period = base_period, panel = panel, 
#                   true_repeated_cross_sections = true_repeated_cross_sections, 
#                   n = n, nG = nG, nT = nT, tlist = tlist, glist = glist, 
#                   call = call)
# }

sunab <- function (cohort, period, ref.c = NULL, ref.p = -1, bin, bin.rel, 
                   bin.c, bin.p, att = FALSE, no_agg = FALSE) {
  check_arg(cohort, "mbt vector")
  check_arg(period, "mbt vector len(data)", .data = cohort)
  check_arg(ref.c, "NULL vector no na")
  check_arg(att, no_agg, "logical scalar")
  check_arg(bin, bin.c, bin.p, bin.rel, "NULL list | vector")
  cohort_name = deparse_long(substitute(cohort))
  period_name = deparse_long(substitute(period))
  period_name = gsub("^[[:alpha:]][[:alpha:]_\\.]*\\$", "", 
                     period_name)
  is_bin = !missnull(bin)
  is_bin.c = !missnull(bin.c)
  is_bin.p = !missnull(bin.p)
  if (is_bin && (is_bin.c || is_bin.p)) {
    stop("You cannot have the argument 'bin' with the arguments 'bin.p' or 'bin.c' at the same time. Use only the latter.")
  }
  n_origin = length(cohort)
  IS_NA = which(is.na(cohort) | is.na(period))
  ANY_NA = length(IS_NA) > 0
  if (ANY_NA) {
    cohort = cohort[-IS_NA]
    period = period[-IS_NA]
  }
  n = length(cohort)
  period_unik = unique(period)
  cohort_unik = unique(cohort)
  if (is_bin.c) {
    cohort = bin_factor(bin.c, cohort, cohort_name)
    cohort_unik = unique(cohort)
  }
  if (is_bin.p) {
    period = bin_factor(bin.p, period, period_name)
    period_unik = unique(period)
  }
  is_CASE_1 = FALSE
  if (is.numeric(period) && 0 %in% period_unik && min(period_unik) < 
      0 && max(period_unik) > 0) {
    is_CASE_1 = TRUE
    if (is_bin) {
      stop("You cannot use 'bin' when the argument 'period' contains relative periods. To use 'bin', 'period' should represent \"calendar\" periods.")
    }
  }
  else {
    if (is_bin) {
      period = bin_factor(bin, period, period_name)
      cohort = bin_factor(bin, cohort, cohort_name, no_error = TRUE)
      period_unik = unique(period)
      cohort_unik = unique(cohort)
    }
    refs = setdiff(cohort_unik, period_unik)
    if (length(refs) == length(cohort_unik)) {
      stop("Problem in the creation of the relative time periods. We expected the cohort to be the treated period, yet not a single 'cohort' value was found in 'period'.")
    }
    qui_keep = which(!cohort %in% refs)
    cohort_valid = cohort[qui_keep]
    period_valid = period[qui_keep]
    if (is.numeric(period_valid) || is.numeric(cohort_valid)) {
      if (!is.numeric(cohort_valid)) 
        cohort_valid = as.numeric(cohort_valid)
      if (!is.numeric(period_valid)) 
        period_valid = as.numeric(period_valid)
      rel_period = period_valid - cohort_valid
    }
    else {
      sunik_period = sort(unique(period_valid))
      dict_period = seq_along(sunik_period)
      names(dict_period) = sunik_period
      period_valid = dict_period[as.character(period_valid)]
      cohort_valid = dict_period[as.character(cohort_valid)]
      rel_period = period_valid - cohort_valid
    }
    new_period = rep(-1, n)
    new_period[qui_keep] = rel_period
    period = new_period
  }
  .F = period_min = min(period)
  .L = period_max = max(period)
  period_list = list(.F = period_min, .L = period_max)
  check_set_arg(ref.p, "evalset integer vector no na", .data = period_list)
  if (missing(ref.p)) 
    ref.p = ref.p
  cohort_int = quickUnclassFactor(cohort)
  c_order = order(cohort_int)
  info = cpp_find_never_always_treated(cohort_int[c_order], 
                                       period[c_order])
  if (!is.null(ref.c)) {
    qui_drop = which(cohort_int %in% info$ref | period %in% 
                       ref.p | cohort %in% ref.c)
  }
  else {
    qui_drop = which(cohort_int %in% info$ref | period %in% 
                       ref.p)
  }
  qui_NA = info$always_treated
  cohort = cohort[-qui_drop]
  period = period[-qui_drop]
  if (!missing(bin.rel)) {
    period = bin_factor(bin.rel, period, "relative period")
  }
  res_raw = i(factor_var = period, f2 = cohort, f_name = period_name)
  if (ANY_NA) {
    res = matrix(NA_real_, nrow = n_origin, ncol = ncol(res_raw), 
                 dimnames = list(NULL, colnames(res_raw)))
    res[-IS_NA, ][qui_drop, ] = 0
    res[-IS_NA, ][-qui_drop, ] = res_raw
    if (length(qui_NA) > 0) {
      res[-IS_NA, ][qui_NA, ] = NA_real_
    }
  }
  else {
    res = matrix(0, nrow = n_origin, ncol = ncol(res_raw), 
                 dimnames = list(NULL, colnames(res_raw)))
    res[-qui_drop, ] = res_raw
    if (length(qui_NA) > 0) {
      res[qui_NA, ] = NA_real_
    }
  }
  if (!no_agg) {
    is_GLOBAL = FALSE
    for (where in 1:min(8, sys.nframe())) {
      if (exists("GLOBAL_fixest_mm_info", parent.frame(where))) {
        GLOBAL_fixest_mm_info = get("GLOBAL_fixest_mm_info", 
                                    parent.frame(where))
        is_GLOBAL = TRUE
        break
      }
    }
    if (is_GLOBAL) {
      agg_att = c(ATT = paste0("\\Q", period_name, "\\E::[[:digit:]]+:cohort"))
      agg_period = paste0("(\\Q", period_name, "\\E)::(-?[[:digit:]]+):cohort")
      if (att) {
        agg = agg_att
      }
      else {
        agg = agg_period
        info = list()
        period_unik = sort(unique(c(period, ref.p)))
        info$coef_names_full = paste0(period_name, "::", 
                                      period_unik)
        info$items = period_unik
        if (length(ref.p) > 0) {
          info$ref_id = c(which(info$items %in% ref.p[1]), 
                          which(info$items %in% ref.p[-1]))
          info$ref = info$items[info$ref_id]
        }
        info$f_name = period_name
        info$is_num = TRUE
        info$is_inter_num = info$is_inter_fact = FALSE
        attr(agg, "model_matrix_info") = info
      }
      GLOBAL_fixest_mm_info$sunab = list(agg = agg, agg_att = agg_att, 
                                         agg_period = agg_period, ref.p = ref.p)
      assign("GLOBAL_fixest_mm_info", GLOBAL_fixest_mm_info, 
             parent.frame(where))
    }
  }
  res
}
