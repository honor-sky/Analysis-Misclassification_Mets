one_FeatureImp <- function(model, train, target.name, seed=100, task="regression") {

  ds <- train[, -which(names(train)==target.name)]
  cl <- train[, target.name]
  #col.mean <- colMeans(ds)
  N <- nrow(ds)

  pred.all <- predict(model,ds)
  # if (task=="regression") {
  #   PF.all <- sum(abs(pred.all-cl))  # mae
  # } else {
  #   PF.all <- mean(pred.all==cl)      # accuracy
  # }

  f_class <- c(); f_feature <- c()
  f_imp_F1 <- c(); f_imp_other <- c();
  f_contribute_F1 <- c(); f_contribute_other <- c()
  f_contribute_common <- c()
  f_interact <- c()
  cnt <- 1

  for(i in 1:ncol(ds)) {
    tmp.F1 <- ds                      # copy dataset
    tmp.other <- ds                   # copy dataset
    tmp.F1[,i] <-shuffle(tmp.F1[,i], seed)    # shuffle


    for (k in 1:ncol(ds)) {
      if (k==i) next
      tmp.other[,k] <- shuffle(tmp.other[,k],seed)    # shuffle
    }

    pred.F1 <- predict(model,tmp.other)
    pred.other <- predict(model, tmp.F1)

    if (task=="regression") {
      F1    <- abs(pred.F1-cl)
      OTHER <- abs(pred.other-cl)
      ALL   <- abs(pred.all-cl)

      f_class[i] <- NA
      f_feature[i] <- names(ds)[i]

      idx1 <- which((OTHER > ALL & ALL > F1) |        # positive
                      (F1 > ALL & ALL > OTHER))         # negative
      f_contribute_F1[i] <-  sum(OTHER[idx1]-ALL[idx1]) / N

      #idx2 <- which((OTHER > F1 & F1 > ALL) |        # positive
      #              (ALL > F1 & F1 > OTHER))         # negative

      f_contribute_other[i] <- 0

      idx3 <- which(ALL == OTHER & OTHER == F1)
      f_contribute_common[i] <-  sum(OTHER[idx3]-ALL[idx3]) / N   ##

      idx4 <- which((F1 > ALL & OTHER > ALL) |        # positive
                      (F1 < ALL & OTHER < ALL) )
      f_interact[i] <- sum(OTHER[idx4]-ALL[idx4]) / N

      f_imp_F1[i] <- f_contribute_F1[i] + f_interact[i]
      f_imp_other[i] <- f_contribute_other[i] + f_interact[i]

    } else {    # classification ----------------------------------------
      class.name <- unique(train[,target.name])

      for (cname in class.name) {
        f_class[cnt] <- cname
        f_feature[cnt] <- names(ds)[i]

        base <- pred.all!=cl & cl==cname #수정

        idx1 <- which(pred.F1 == cl & pred.other == cl & base) #수정
        f_interact[cnt] <- length(idx1) / N

        idx2 <- which(pred.F1 != cl & pred.other != cl & base) #수정
        f_contribute_common[cnt] <- length(idx2) / N

        idx3 <- which(pred.F1 != cl & pred.other == cl & base) #수정
        f_contribute_F1[cnt] <- length(idx3) / N

        idx4 <- which(pred.F1 == cl & pred.other != cl & base) #수정
        f_contribute_other[cnt] <- length(idx4) / N

        f_imp_F1[cnt] <- f_contribute_F1[cnt] + f_interact[cnt]
        f_imp_other[cnt] <- f_contribute_other[cnt] + f_interact[cnt]

        cnt <- cnt+1
      }
    }


  } # end for

  if (task=="classification") {
    # for all class in classification
    for (fs in names(ds)) {
      f_class[cnt] <- "_all_"
      f_feature[cnt] <- fs
      f_interact[cnt] <- sum(f_interact[f_feature==fs], na.rm=T)
      f_contribute_common[cnt] <- sum(f_contribute_common[f_feature==fs], na.rm=T)
      f_contribute_F1[cnt] <- sum(f_contribute_F1[f_feature==fs], na.rm=T)
      f_contribute_other[cnt] <- sum(f_contribute_other[f_feature==fs], na.rm=T)

      f_imp_F1[cnt] <- sum(f_imp_F1[f_feature==fs], na.rm=T)
      f_imp_other[cnt] <- sum( f_imp_other[f_feature==fs], na.rm=T)

      cnt <- cnt+1
    }
  }


  overall <- data.frame(class=f_class, feature=f_feature,
                        cont_F1=f_contribute_F1,
                        cont_other=f_contribute_other,
                        cont_common=f_contribute_common,
                        interact=f_interact)
  importance <- data.frame(class=f_class, feature=f_feature,
                           imp_F1=f_imp_F1,
                           imp_other=f_imp_other)

  return(list(task=task, overall=overall, importance=importance))
}

###########################################################################
one_FeatureInt <- function(model, train, target.name, F1, F2, seed=100, task="regression", class) {

  ds <- train[, -which(names(train)==target.name)]
  cl <- train[, target.name]
  N <- nrow(ds)

  pred.all <- predict(model,ds)

  # shuffle except F1, F2 -------------------------
  tmp.F1F2 <- ds
  for (k in 1:ncol(ds)) {
    if (names(ds)[k] == F1 | names(ds)[k] == F2) next
    tmp.F1F2[,k] <- shuffle(tmp.F1F2[,k],seed)    # shuffle
  }

  # if (task=="regression") {
  #   PF.all <- sum(abs(pred.all-cl))   # mae
  # } else {
  #   PF.all <- sum(pred.all==cl)/N      # accuracy
  # }

  f_contribute_F1 <- NA; f_contribute_F2 <- NA
  f_contribute_common <- NA
  f_interact <- NA

  #for(i in 1:ncol(ds)) {
  {
    tmp.F1 <- tmp.F1F2                      # copy dataset
    tmp.F2 <- tmp.F1F2                      # copy dataset
    tmp.F1[,F1] <-shuffle(tmp.F1[,F1], seed)    # shuffle
    tmp.F2[,F2] <-shuffle(tmp.F2[,F2], seed)    # shuffle

    pred.F1 <- predict(model,tmp.F2)
    pred.F2 <- predict(model,tmp.F1)
    pred.F1F2 <- predict(model,tmp.F1F2)

    if (task=="regression") {
      EF1  <- abs(pred.F1-cl)
      EF2  <- abs(pred.F2-cl)
      EALL <- abs(pred.F1F2-cl)


      idx4 <- which((EF1 > EALL & EF2 > EALL) |        # positive
                      (EF1 < EALL & EF2 < EALL) )
      f_interact <- (sum(EF2[idx4]-EALL[idx4]) +
                       sum(EF1[idx4]-EALL[idx4])) / (2*N)


    } else { # classification
      base <- pred.F1F2 != cl & pred.all!=cl & cl==class   #수정
      
      idx1 <- which(pred.F1 == cl & pred.F2 == cl & base ) #수정
      f_interact <- length(idx1) / N
      
      idx2 <- which(pred.F1 != cl & pred.F2 != cl & base ) #수정
      f_contribute_common <- length(idx2) / N
      
      idx3 <- which(pred.F1 != cl & pred.F2 == cl & base ) #수정
      f_contribute_F1 <- length(idx3) / N
      
      idx4 <- which(pred.F1 == cl & pred.F2 != cl & base ) #수정
      f_contribute_F2 <- length(idx4) / N

    }

  } # end for

  result <- list(interact=f_interact, cont_F1=f_contribute_F1,
                 cont_F2=f_contribute_F2, cont_common=f_contribute_common)

  return(result)
}

######################################################################################

pair_FeatureInteract <- function(model, train, target.name, F1, F2, itr=10, task="regression", class) {

  result <- one_FeatureInt(model, train, target.name, F1, F2, seed=100, task=task, class)

  if (itr >=2) {
    for (i in 2:itr) {
      tmp <- one_FeatureInt(model, train, target.name, F1, F2, seed=100+i, task=task, class)

      result$interact <- result$interact + tmp$interact
      result$cont_F1  <- result$cont_F1 + tmp$cont_F1
      result$cont_F2  <- result$cont_F2 + tmp$cont_F2
      result$cont_common <- result$cont_common + tmp$cont_common

    }
    result$interact <- result$interact / itr
    result$cont_F1  <- result$cont_F1  / itr
    result$cont_F2  <- result$cont_F2  / itr
    result$cont_common <- result$cont_common  / itr

  }

  return(result)
}


######################################################################################
majority <- function(df, cl) {
  cl.names <- levels(cl)
  tbl <- rep(0, length(cl.names))
  for(i in 1:ncol(df)) {
    idx <- which(df[1,i]== cl.names)
    tbl[idx] <- tbl[idx] +1
  }
  idx <- which(tbl==max(tbl))[1]
  return(cl.names[idx])
}

########################################################################
generate.data <- function(model, train, target.name, F1, F2, itr, task) {

  ds <- train[, -which(target.name==names(train))]
  cl <- train[, target.name]

  ds.F1 <- ds
  ds.F2 <- ds
  ds.F1F2 <- ds

  pred.all <- predict(model, ds)
  pred.F1 <- data.frame(matrix(NA, nrow=nrow(ds), ncol=itr+1))
  pred.F2 <- data.frame(matrix(NA, nrow=nrow(ds), ncol=itr+1))
  pred.F1F2 <- data.frame(matrix(NA, nrow=nrow(ds), ncol=itr+1))

  for (i in 1:itr) {
    # shupple data
    for (fs in names(ds)) {
      if (fs == F1) next
      ds.F1[,fs] <- shuffle(ds.F1[,fs])
    }
    for (fs in names(ds)) {
      if (fs == F2) next
      ds.F2[,fs] <- shuffle(ds.F2[,fs])
    }
    for (fs in names(ds)) {
      if (fs == F1 | fs == F2) next
      ds.F1F2[,fs] <- shuffle(ds.F1F2[,fs])
    }

    pred.F1[,i] <- predict(model, ds.F1)
    pred.F2[,i] <- predict(model, ds.F2)
    pred.F1F2[,i] <- predict(model, ds.F1F2)
  }

  # find majority
  for(i in 1:nrow(ds)) {
    if (task == "regression") {
      pred.F1[i,itr+1] <- mean(unlist(pred.F1[i,1:itr]-cl))
      pred.F2[i,itr+1] <- mean(unlist(pred.F2[i,1:itr]-cl))
      pred.F1F2[i,itr+1] <- mean(unlist(pred.F1F2[i,1:itr]-cl))

    } else { # classification
      pred.F1[i,itr+1] <- majority(pred.F1[i,1:itr], cl)
      pred.F2[i,itr+1] <- majority(pred.F2[i,1:itr], cl)
      pred.F1F2[i,itr+1] <- majority(pred.F1F2[i,1:itr], cl)
    }
  }

  groups <- rep("ERR", nrow(ds))
  if (task == "regression") {

    idx <- which(pred.F1[,itr+1] <= pred.F1F2[,itr+1] &
                   pred.F2[,itr+1] > pred.F1F2[,itr+1])
    groups[idx] <- "G1"
    idx <- which(pred.F1[,itr+1] > pred.F1F2[,itr+1] &
                   pred.F2[,itr+1] <= pred.F1F2[,itr+1])
    groups[idx] <- "G2"
    idx <- which(pred.F1[,itr+1] == pred.F1F2[,itr+1] &
                   pred.F2[,itr+1] == pred.F1F2[,itr+1])
    groups[idx] <- "G3"
    idx <- which(pred.F1[,itr+1] < pred.F1F2[,itr+1] &
                   pred.F2[,itr+1] < pred.F1F2[,itr+1])
    groups[idx] <- "G4neg"
    idx <- which(pred.F1[,itr+1] > pred.F1F2[,itr+1] &
                   pred.F2[,itr+1] > pred.F1F2[,itr+1])
    groups[idx] <- "G4pos"


  } else { # classification
    base <- (pred.all!= cl & pred.F1F2[,itr+1]!=cl)  #수정

    idx <- which(pred.F1[,itr+1]==cl & pred.F2[,itr+1] != cl & base)
    groups[idx] <- "G1"
    idx <- which(pred.F1[,itr+1]!=cl & pred.F2[,itr+1] == cl & base)
    groups[idx] <- "G2"
    idx <- which(pred.F1[,itr+1]==cl & pred.F2[,itr+1] == cl & base)
    groups[idx] <- "G3"
    idx <- which(pred.F1[,itr+1]!=cl & pred.F2[,itr+1] != cl & base)
    groups[idx] <- "G4"

  }

  return(groups)
}

