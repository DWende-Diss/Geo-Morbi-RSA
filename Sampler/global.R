rm(list=ls())

source('dependencies.R')
options(shiny.maxRequestSize=500*1024^2)

library("Matrix")

sampling_ready <- FALSE
steps <- read.csv2("help.csv")
intro <- read.csv2("intro.csv")

workEnvir <-  new.env()


### Funktion umgeschriebn um große Probleme zu bearbeiten
#Anzahl an Variablen, die unter das Multikollinaritäts-Kriterium fallen
#krit=1E-3
#length(which(C<krit))

multi.col <- function(X,krit){
  D <- diag(krit,ncol(X))
  XX <- crossprod(X)
  SX <- XX + D
  
  n <- nrow(X)
  
  Nenner <- diag(solve(SX))

  Mean <- colSums(X)/n
  
  Zaehler <- (colSums((X^2)) - n*(Mean^2))
  
  C <- sqrt((1/Zaehler)/Nenner)
  #length(which(C<krit))
  
  if(length(which(C<krit))>0){
    
    Namen <- ""
    for(i in c(which(C<krit))){
      Namen <- paste0(Namen," ",names(C[i]))
    }
    
    cat(paste0("Multicol detected: ",Namen,"\n"))
    return(which(C<krit))
    
  }
  
}


# Extremwert Anpassung

update.extrems <- function(weights.temp,Status){
  
  base.volumn <- Status[["Tr.total"]][1]
  repeat{  
    weights.outrun <- weights.temp > Status[["weight.max"]]
    if(sum(weights.outrun)>0){
      weights.temp[weights.outrun] <- Status[["weight.max"]]
      absolut.grad <- base.volumn/sum(weights.temp)
      weights.temp <- absolut.grad*weights.temp
    }else{
      absolut.grad <- base.volumn/sum(weights.temp)
      weights.temp <- absolut.grad*weights.temp
      break
    }
  }
  
  
  return(weights.temp)
  
}


# Gewichte ---------------------

update.weights <- function(Status){
  
  cat <- (Status[["X"]] %*% (Status[["coefs"]] - (Status[["ss"]] * Status[["Newton"]])))@x
  weights.temp <- c(exp(cat))
  
  Status[["weights.ebal"]] <- update.extrems(weights.temp,Status)
  
  return(Status)
  
}


update.Newton <- function(Status){
  ###############################################################################################################################
  # Umgang mit Ausreißern, je größer Lambda, desto weniger relevant sind Ausreißer   
  solver <- function(){
    # tryCatch({
    #  solve(hessian,gradient)@x
    #}, error = function(e){
    solve(hessian + diag(10^-1,ncol(hessian)),gradient)@x
    #})
  }  
  
  hessian = crossprod(Status[["X"]],(Status[["weights.ebal"]] * Status[["X"]]))  
  
  Tr   <- c((Status[["weights.ebal"]] %*% Status[["X"]])@x)
  gradient   <- (Tr - Status[["Tr.total"]])
  
  newton <-  solver()
  
  Status[["gradient"]]  <- gradient
  Status[["Newton"]]  <- newton
  
  return(Status)
  
}

# line search -------------------------

# Optimierungsfunktion zum finden der besten Schrittweite
line.searcher <- function(Status,ss){
  
  Status[["ss"]] <- ss
  
  Status   <- update.weights(Status)
  
  weights.temp <- Status[["weights.ebal"]]
  Tr     <- c((weights.temp %*% Status[["X"]])@x)
  gradient   <- (Tr - Status[["Tr.total"]])
  
  deviation.max <- max(abs(gradient))
  maxdiff      <- deviation.max
  return(maxdiff)
}

# trim weights --------------

trim <- function(Status,print.level){
  
  
  Status[["Base.weight"]] -> Base.weight  
  w.trimming <- 1
  Status[["ss"]] <- 0
  
  weights.ebal <- get.weights(Status)
  for(iter.trim in 1:10) {
    
    if(max(weights.ebal)>Status[["weight.max"]]){
      
      ### Trimmen der Stichprobengewichte die vom Benutzer als zu "hoch" gesetzt wurden
      
      cat("Trim iteration",format(iter.trim,digits=3),"\n")
      
      w.trimming <- w.trimming*ifelse(weights.ebal>Status[["weight.max"]]
                                      ,w.trimming*0.95,1)
      
      w.trimming <- w.trimming*(sum(weights.ebal))/sum(w.trimming)
      
      Status[["Base.weight"]] <- w.trimming * weights.ebal
      Status <- eb_step(Status,
                        print.level=print.level
      ) 
    }
    
  }  
  Status[["Base.weight"]] <- Base.weight  
  return(Status)
  
}

# eb_step  ------------------------

eb_step <- function(Status,print.level=print.level) {
  
  Status <- update.Newton(Status)
  
  which.is.worst <- which(abs(Status[["gradient"]])==max(abs(Status[["gradient"]])))
  
  if(print.level>=2){ cat("maximum deviation (",(Status[["X"]])@Dimnames[[2]][which.is.worst],") is ="
                          ,format(Status[["gradient"]][which.is.worst],digits=4)
                          ,"| mean deviation is =",format(mean(abs(Status[["gradient"]])),digits=4)
                          ,"\n")
  }
  
  loss.new <- line.searcher(Status,1)
  loss.old <- line.searcher(Status,0)
  
  
  
  
  if(loss.old <= loss.new){
    if(print.level>=3){cat("               new loss",loss.new,"old loss=",loss.old,"\n")}
    
    ss.out <- optimize(line.searcher,
                       lower=.00001,upper=1,maximum=FALSE,tol=0.01,
                       Status=Status)
    
    Status[["ss"]] <- ss.out$minimum
    if(print.level>=3){cat("               LS Step",ss.out$minimum,"with Loss",ss.out$objective,"\n")}
    
  }else{
    Status[["ss"]] <- 1
  }
  
  Status[["coefs"]] <- Status[["coefs"]] - (Status[["ss"]] * Status[["Newton"]])
  Status[["ss"]] <- 0
  Status <- update.weights(Status)
  
  return(Status)
}





sampling <- function(X
                     ,sample.size
                     ,constraints
                     ,constrain.weight=NULL
                     ,base.weight=NULL
                     ,coefs = NULL
                     ,constraint.normalize=NULL
                     ,weight.max=NULL
                     ,weight.min=NULL
                     ,test.multicol = FALSE
                     ,max.iterations = 200
                     ,constraint.tolerance = 1
                     ,print.level=2
){
  
  X <- as(X,"Matrix")
  
  if (sum(is.na(X@x))>0){
    stop("NA in X")
  }
  
  
  if (length(max.iterations) != 1 ) {
    stop("length(max.iterations) != 1")
  }
  if (length(constraint.tolerance) != 1 ) {
    stop("length(constraint.tolerance) != 1")
  }
  if (length(sample.size) != 1 ) {
    stop("length(sample.size) != 1")
  }
  #####################################################################################################################
  # Test auf Multikolinearität, Startwert: 
  
  all_out <- c()
  repeat{
    out <- multi.col(X,multicoll)[1]
    if(length(out)>0){
      X <- X[,-out]
      constraints <- constraints[-out]
      all_out <- c(all_out,X@Dimnames[[2]][out])
    }else{
      cat(paste0("Ausschluss: ",all_out,"   ","\n"))
      break}
  }
  
  base.size <- nrow(X)
  constrains.size <- ncol(X)
  
  if (is.null(base.weight)) {
    base.weight = rep(sample.size/base.size, base.size)
  }
  if ( length(constraints) !=  constrains.size) {
    stop("length(constraints) !=  constrains.size")
  }
  
  
  if(is.null(constraint.normalize)){
    tr.total <- c(sample.size,constraints)
    co.x <- cBind(rep(1,nrow(X)),X)
  }else{
    tr.total <- c(sample.size*constraints)
    normalization <- which(X@Dimnames[[2]] %in% constraint.normalize)
    tr.total[normalization] <- constraints[normalization]
    
    tr.total <- c(sample.size,tr.total)
    co.x <- cBind(rep(1,nrow(X)),X)
  }
  
  
  if(is.null(coefs)) {
    coefs = c(log(tr.total[1]/sum(base.weight)),rep(0,(ncol(co.x)-1)))
  }
  
  if(length(coefs) != ncol(co.x)) {
    stop("coefs needs to have same length as number of covariates plus one")
  }
  
  ## run algo
  
  Status <- list(
    base.size = base.size
    ,Base.weight  = base.weight
    ,weights.ebal = base.weight
    ,X = co.x
    ,Tr.total   =  tr.total
    ,coefs   = coefs
    ,Newton   = 0
    ,gradient = 99999999
    ,weight.max   = weight.max
    ,ss   = 0
  )
  
  for(i in 1:max.iterations){
    if(mean(abs(Status[["gradient"]])>10)){
      cat(paste0("Iteration: ", i, "   "))
      Status <- eb_step(Status,
                        print.level=print.level)
    }
    
  }
  
  #  Status <- trim(Status,
  #                    print.level=print.level) 
  
  if(print.level>=0) {
    cat("Converged within tolerance \n")
  }
  
  
  
  z <- list(
    target.margins = tr.total,
    co.xdata = co.x,
    w=Status[["weights.ebal"]],
    coefs=Status[["coefs"]],
    maxdiff=0,
    norm.constant = sample.size,
    constrain.weight = 0,
    constraint.tolerance=0,
    max.iterations=0,
    base.weight=base.weight,
    print.level=print.level,
    converged=0
  )
  
  class(z) <- "ebalance"
  return(z)
  
}


