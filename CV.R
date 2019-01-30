#'====================================================================================================
#'
#' CROSS VALIDATION SCHEMES
#'
#'====================================================================================================

#' version 0.3
#' date: 29th december
#' author: GMFCN  germano.cneto@usp.br
#'====================================================================================================

# random sampling
cv1 <- function(df.y, y, eta, f=.2,fold=10,nIter = 2E4,burnIn = 2E3,thin = 5,saveAt = 'dcv1_'){
  yHat <- c()
  yHat. <- c()
  for(i in 1:fold){
    cat(paste("#----------------------------------------------------------------------------#","\n",sep=""))
    cat(paste("Starting fold",i,"at:",Sys.time(),"\n",sep=" "))
    s <- length(y) * f
    df.ycopy <- df.y
    tst <- sample(1:length(y), size = s, replace = FALSE)
    df.ycopy$value[tst] <- NA
    
    fit <- BGLR(y = df.ycopy$value,
                ETA = eta,
                nIter = nIter,
                burnIn = burnIn,
                thin = thin,
                saveAt = saveAt ,
                verbose = FALSE)
  
    yHat <- rbind(yHat,data.frame(obs=y[tst],pred=fit$yHat[tst],env=df.y$env[tst],gid=df.y$gid[tst],pop="V",fold=i))
    yHat. <- rbind(yHat.,data.frame(obs=y[-tst],pred=fit$yHat[-tst],env=df.y$env[-tst],gid=df.y$gid[-tst],pop="T",fold=i))
  }
  
  
  cat(paste("#----------------------------------------------------------------------------#","\n",sep=""))
  cat(paste("Finished at:",Sys.time(),"\n",sep=" "))
  cat(paste("#----------------------------------------------------------------------------#","\n",sep=""))
  
  return(rbind(yHat,yHat.))
}


# new genotypes x new envs
cv2 <- function(df.y, y, env, part.env=1, eta, seed=1234, f=.2,fold=10,nIter = 2E4,burnIn = 2E3,thin = 5,saveAt = 'dcv1_'){
  df.y$gid <- as.factor(df.y$gid)
  df.y$env <- as.factor(df.y$env)
  yHat  <- c()
  yHat. <- c()
  ENVS<-nlevels(env)
  GIDS <- nlevels(df.y$gid)
  starttime <- Sys.time()
  set.seed(seed)
  for(i in 1:fold){
    
    cat(paste("#----------------------------------------------------------------------------#","\n",sep=""))
    cat(paste("Starting fold",i,"at:",Sys.time(),"\n",sep=" "))
    df.ycopy <- df.y
    # definying new env
    #cat(paste("Sampling environments...","\n",sep=" "))
    tenv <- unique(df.ycopy$env)[sample(1:ENVS, size = part.env, replace = TRUE)]
    df.ycopy$value[!(df.ycopy$env %in% tenv)] <- NA

    # definying new gid
   # cat(paste("Sampling genotypes...","\n",sep=" "))
    tgid <- unique(df.ycopy$gid)[sample(1:GIDS, size = GIDS * f, replace = FALSE)]
    df.ycopy$value[!(df.ycopy$gid %in% tgid)] <- NA
    
    fit <- BGLR(y = df.ycopy$value,
                ETA = eta,
                nIter = nIter,
                burnIn = burnIn,
                thin = thin,
                saveAt = saveAt ,
                verbose = FALSE)
    
    yHat <- data.frame(obs= df.y$value, pred = fit$yHat, gid = df.y$gid, env=df.y$env,pop=NA, fold=i)
    
    # setting scenario comparisions
    yHat$pop[(df.ycopy$env %in% tenv)   &  (df.ycopy$gid %in% tgid) ]   <- "KnowEnv|KnowGid"
    yHat$pop[!(df.ycopy$env %in% tenv)  &  !(df.ycopy$gid %in% tgid)]   <- "NewEnv|NewGid"
    yHat$pop[!(df.ycopy$env %in% tenv)  &  (df.ycopy$gid %in% tgid) ]   <- "NewEnv|KnowGid"
    yHat$pop[(df.ycopy$env %in% tenv)   &  !(df.ycopy$gid %in% tgid)]   <- "KnowEnv|NewGid"
    
    yHat. <- rbind(yHat.,yHat)
  }
  

  cat(paste("#----------------------------------------------------------------------------#","\n",sep=""))
  cat(paste("Finished at:",Sys.time(),"\n",sep=" "))
  cat(paste("#----------------------------------------------------------------------------#","\n",sep=""))
  fineshedttime <- Sys.time()
  saveRDS(time=list(starttime,fineshedttime))
  return(yHat.)
}

dcv1 <- function(df.y,eta, f,y) {
  
  cat(paste("#----------------------------------------------------------------------------#","\n",sep=""))
  cat(paste("Starting process at:",Sys.time(),"\n",sep=" "))
  cat(paste("#----------------------------------------------------------------------------#","\n",sep=""))
 
  s <- length(y) * f
  df.ycopy <- df.y
  tst <- sample(1:length(y), size = s, replace = FALSE)
  df.ycopy$value[tst] <- NA
  
  fit <- BGLR(y = df.ycopy$value,
              ETA = eta,
              nIter = 2E4,
              burnIn = 2E3,
              thin = 5,
              saveAt = 'gblup2_',
              verbose = FALSE)
  
  #yHat <- data.frame(df.y,value.hat=fit$yHat,pop=NA)
  #yHat$pop[-tst] <- "T"
  #yHat$pop[tst] <- "V"
  
  
  yHat <- data.frame(obs=y[tst],pred=fit$yHat[tst],env=df.y$env[tst],gid=df.y$gid[tst])
  
  cat(paste("#----------------------------------------------------------------------------#","\n",sep=""))
  cat(paste("Finished at:",Sys.time(),"\n",sep=" "))
  cat(paste("#----------------------------------------------------------------------------#","\n",sep=""))
  
  return(yhat=yHat)
}



dcv1 <- function(df.y,
                 eta,
                 f=.3,
                 nIter = 2E4, 
                 burnIn = 3E3,
                 thin = 5,
                 saveAt = 'dcv1_') {
  
  cat(paste("#----------------------------------------------------------------------------#","\n",sep=""))
  cat(paste("Starting process at:",Sys.time(),"\n",sep=" "))
  cat(paste("#----------------------------------------------------------------------------#","\n",sep=""))
  y <- df.y$value
  s <- length(y) * f
  df.ycopy <- df.y
  tst <- sample(1:length(y), size = s, replace = FALSE)
  df.ycopy$value[tst] <- NA

  fit <- BGLR(y = df.ycopy$value,
              ETA = eta,
              nIter = nIter,
              burnIn = burnIn,
              thin = thin,
              saveAt = saveAt ,
              verbose = FALSE)

  rhotst <- cor(fit$yHat[ tst], y[ tst])
  rhotrt <- cor(fit$yHat[-tst], y[-tst])
  
  yHat <- data.frame(obs=y[tst],pred=fit$yHat[tst],env=df.y$env[tst],gid=df.y$gid[tst])
  
  cat(paste("#----------------------------------------------------------------------------#","\n",sep=""))
  cat(paste("Finished at:",Sys.time(),"\n",sep=" "))
  cat(paste("#----------------------------------------------------------------------------#","\n",sep=""))
  
  return(list(yhat=yHat))
  
}

# predicting new genotypes at know environments
dcv2 <- function(df.y,
                 eta,
                 f=.3,
                 nIter = 2E4, 
                 burnIn = 3E3,
                 thin = 5,
                 saveAt = 'dcv1_') {
  
  cat(paste("#----------------------------------------------------------------------------#","\n",sep=""))
  cat(paste("Starting process at:",Sys.time(),"\n",sep=" "))
  cat(paste("#----------------------------------------------------------------------------#","\n",sep=""))
  y <- df.y$value
  s <- length(unique(df.y$gid)) * f
  df.ycopy <- df.y
  TST <- unique(df.ycopy$gid)[sample(1:length(unique(df.y$gid)), size = s, replace = FALSE)]
  df.ycopy$value[!(df.ycopy$gid %in% TST)] <- NA
  
  fit <- BGLR(y = df.ycopy$value,
              ETA = eta,
              nIter = nIter,
              burnIn = burnIn,
              thin = thin,
              saveAt = saveAt ,
              verbose = FALSE)
  
  yHat <- data.frame(df.y,value.hat=fit$yHat,pop=NA)
  yHat$pop[!(yHat$gid %in% TST)] <- "V"
  yHat$pop[(yHat$gid %in% TST)] <- "T"
  cat(paste("#----------------------------------------------------------------------------#","\n",sep=""))
  cat(paste("Finished at:",Sys.time(),"\n",sep=" "))
  cat(paste("#----------------------------------------------------------------------------#","\n",sep=""))
  
  return(list(yhat=yHat))
  
}



## CV otimizado -----

# otimizado
cv.opt <- function(df.y,  # phenotypic dataframe
                   eta,   # BGLR eta
                   TST    # trained population (Akdemir)
) {
  
  df.ycopy <- df.y
  cat(paste("#----------------------------------------------------------------------------#","\n",sep=""))
  cat(paste("Starting process at:",Sys.time(),"\n",sep=" "))
  cat(paste("#----------------------------------------------------------------------------#","\n",sep=""))
  df.ycopy$value[!(df.ycopy$gid %in% TST)] <- NA


  fit <- BGLR(y = df.ycopy$value,
              ETA = eta,
              nIter = 1E4,
              burnIn = 2E3,
              thin = 5,
              saveAt = 'gblup2_',
              verbose = FALSE)
  
  yHat <- data.frame(df.y,value.hat=fit$yHat,pop=NA)
  yHat$pop[!(yHat$gid %in% TST)] <- "V"
  yHat$pop[(yHat$gid %in% TST)] <- "T"
  rhot <- cor(yHat$value.hat[!(df.y$gid %in% TST)], yHat$value[!(df.y$gid %in% TST)], use="complete.obs")
  rho <- cor(yHat$value.hat[(df.y$gid %in% TST)], yHat$value[(df.y$gid %in% TST)], use="complete.obs")
  
  return(list(rho=rho,rhot=rhot,yhat=yHat))
  cat(paste("Finished at:",Sys.time(),"\n",sep=" "))
  cat(paste("#----------------------------------------------------------------------------#","\n",sep=""))
  
}


# new genotypes and new environments (otimizado)
cv2.opt <-function(y,gen, env,TST, df, eta){
  cat(paste("======================================================","\n",sep=""))
  cat(paste("Starting cross-validation:",Sys.time(),"\n",sep=" "))
  cat(paste("======================================================","\n",sep=""))
  
  phenoGE<-data.frame(value=df[,y],gid=df[,gen], env=df[,env])
  df.ycopy <- phenoGE
  df.ycopy$value[!(phenoGE$gid %in% TST)] <- NA # selected genotypes
  EN<-levels(phenoGE$env)
  ENVS<-nlevels(phenoGE$env)
  rho <-data.frame(matrix(NA,nrow=ENVS,ncol=3));names(rho) <-c("env","rho_trt","rho_tst")
  
  for(EN in 1:ENVS){
    cat(paste("Boot:",EN, Sys.time(),"\n",sep=" "))
    
    df.ycopy$value[(phenoGE$env %in% ENVS[EN])] <- NA
    TST<-which(is.na(df.ycopy$value), arr.ind=TRUE)
    TRT<-which(!is.na(df.ycopy$value), arr.ind=TRUE)
    fit <- BGLR(y = df.ycopy$value,
                ETA = eta,
                nIter = 1E4,
                burnIn = 2E3,
                thin = 5,
                saveAt = 'gblup2_',
                verbose = FALSE)
    
    rho[EN,1] <- levels(phenoGE$env)[EN]
    rho[EN,2] <- cor(fit$yHat[TRT],phenoGE$value[TRT],use = "complete.obs")
    rho[EN,3] <- cor(fit$yHat[TST],phenoGE$value[TST],use = "complete.obs")
    
  }
  cat(paste("Finshed:", Sys.time(),"\n",sep=" "))
  cat("-------------------------------------------\n")
  return(rho)}


## PA error
error.cv <- function(sd,N,nt=.7,nv=.3){
  E <- sd *sqrt ((1/N)+(V/T))
  return(E)
}

