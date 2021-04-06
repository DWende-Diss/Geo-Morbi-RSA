# Kernel ------------------------------------------------------------------

setClass(
  # Name der Klasse
  "Kernel",
  
  # Slots der Klasse
  slots = c(
    typ="character",                # Name des Modells
    W = "dgCMatrix",                   # Gewichtung
    variable = "numeric",
    notes_of_var="numeric",
    Match = "integer"
    
  )
  
)

setClass(
  # Name der Klasse
  "fitKernel",
  
  # Slots der Klasse
  slots = c(
    b = "numeric",
    yhat = "numeric"
  )
  , contains = "Kernel"
)

# Distanz ----------------------------------------------------------

setGeneric(name="distanz",
           def=function(object,...)
           {
             standardGeneric("distanz")
           }
)

setMethod(f="distanz",
          signature="numeric"
          ,definition=function(object){
            
          D <- matrix(NA,length(object),length(object))  
            for( i in 1:length(object)){
              D[,i] <- abs(object[i]-object)
            }
            
          colnames(D) <- object
          
            return(D)
            
})            

         
setMethod(f="distanz",
          signature="data.frame"
          ,definition=function(object,notes_of_object){
              
            object <- object[match(notes_of_object,object$UID),]
            D<-matrix(NA,nrow(object),nrow(object))
              
              for(i in 1:nrow(object)){
                x<-rep(object[i,1],nrow(object))-object[,1]
                y<-rep(object[i,2],nrow(object))-object[,2]
                D[,i]<-sqrt(x^2+y^2)
              }
                colnames(D) <- notes_of_object
                
            return(D)
            
})             


# Gewichtung --------------------------------------------------------------

setGeneric(name="weight",
           def=function(object,typ,...)
           {
             standardGeneric("weight")
           }
)

setMethod(f="weight",
          signature="matrix"
          ,definition=function(object
                               ,typ
                               ,krit=NULL
                               ,GID=NULL
                               ,...){
              
            if(typ=="invers"){
              W <- 1/D
              W[D==Inf] <- 1
              
            }
            
            # print(krit)
            if(typ=="bisquare"){
              W <- (1-(object/krit)^2)^2
              W[object>krit] <- 0
              
            }
            
           
            
            if(typ=="GWR"){
              W <- matrix(0,nrow(object),ncol(object))
              n <- length(GID)
              
              Vers <- data.table(Vers=rep(1,n),GID=GID)
              Vers <- data.frame(Vers[,sum(Vers),by=GID])
              
              M <- match(GID,Vers$GID)
              V<-Vers[M,]
              V$V1[is.na(V$V1)]<-0
              
              for(i in 1:ncol(D)){
                o<-order(D[,i])
                A<-V[o,]
                K<-which(cumsum(A$V1)<krit)
                if(length(K)==0){
                  K<-1
                }
                lK<-K[length(K)]
                g<-D[o[lK],i]
                W[o[1:lK],i]<-(1-D[o[1:lK],i]^2/g^2)^2
              }
              diag(W)<-1
              
            }
            
            return(W)
            
          }
)
            


         


# initialisieren ----------------------------------------------------------

setGeneric(name="setKernel",
           def=function(object,typ,krit,...)
           {
             standardGeneric("setKernel")
           }
)

setMethod(f="setKernel",
          signature="integer"
          ,definition=function(object
                               ,typ=NULL
                               ,krit=NULL
                               ,GID=NULL
                               ,nNotes=100){
            
            object[is.na(object)]<-0L
            
            W <- sparse.model.matrix(~as.factor(object)-1)
            
            W@Dimnames[[2]] <- substring(W@Dimnames[[2]],14,100)
            Match <- match(object,as.numeric(W@Dimnames[[2]]))
            
            Kern <- new("Kernel"
                        ,typ="exakt"
                        ,W=W
                        ,variable=object
                        ,notes_of_var=unique(object)
                        ,Match=Match)
            
            return(Kern)
            
          }
)

setMethod(f="setKernel",
          signature="numeric"
          ,definition=function(object
                               ,typ
                               ,krit=NULL
                               ,GID=NULL
                               ,nNotes=100){
                 
            
                notes_of_object <- quantile(object,c(1:nNotes)/(nNotes))
                 
                 D <- distanz(notes_of_object)
                 W <- weight(D,typ=typ,krit=krit,GID=GID)
                 
                 Match <- rep(NA,length(object))
                 
                 for(ii in nNotes:1){
                    Match[object<=notes_of_object[ii]] <- ii
                 }
                 
                 W[W<0.01] <- 0L
                 W <- as(W,"dgCMatrix")
                 
                 Kern <- new("Kernel"
                               ,typ=typ
                               ,W=W[Match,]
                               ,variable=object
                               ,notes_of_var=notes_of_object
                               ,Match=Match)
                 
                 return(Kern)
            
          }
)


setMethod(f="setKernel",
          signature="data.frame"
          ,definition=function(object
                               ,typ
                               ,krit=NULL
                               ,GID=NULL
                               ,nNotes=100){
            
            
            notes_of_object <- unique(GID)
            
            D <- distanz(object,notes_of_object)
            W <- weight(D,typ=typ,krit=krit,GID=GID)
            
            Match <- match(GID,notes_of_object)
            
            W[W<0.01] <- 0L
            W <- as(W,"dgCMatrix")
            
            Kern <- new("Kernel"
                        ,typ=typ
                        ,W=W[Match,]
                        ,variable=GID
                        ,notes_of_var=notes_of_object
                        ,Match=Match)
            
            return(Kern)
            
          }
)




# Estimation --------------------------------------------------------------




setGeneric(name="estKernel",
           def=function(object,Y,w)
           {
             standardGeneric("estKernel")
           }
)

setMethod(f="estKernel",
          signature="Kernel",
          definition=function(object,Y,w)
          {

            b <- crossprod(object@W,Y*w)@x / crossprod(object@W,w)@x
            Yhat <- b[object@Match]
            
            fitKern <- new("fitKernel"
                          ,typ=object@typ
                          ,W=object@W
                          ,variable=object@variable
                          ,notes_of_var=object@notes_of_var
                          ,Match=object@Match
                          ,b=b
                          ,yhat=Yhat)
            
            return(fitKern)

          }
)
