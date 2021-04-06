require(Matrix)

######################################################################
# Ein morbiSet ist ein RSA spezifisches Datenpaket
# Das morbiSet enthält alle regressionsspezifischen Informationen 



# Definition --------------------------------------------------------------

morbiSet <- setClass(
  # Name der Klasse = morbiSet
  "morbiSet",
  
  # Definiere slots der Klasse
  slots = c(
    jahr="integer",         # Jahr der Vorlaufenden SA100
    model = "character",     # ggf. Bezeichner des Morbi-Modells
    pseudo = "numeric",     # Pseudonym spalte, kann leer sein
    X = "dgCMatrix",        # dünn besetzte Matrix
    Kost = "numeric",       # Kosten in Euro
    tage="integer",         # Versichertentage
    AGS="factor",           # Wohnort der Versicherten als AGS-8 (R-factor kodiert)
    Zusatz="dgCMatrix",     # ggf Zusatzinformationen für spätere Deckungsbeitragsanalyse
    sample="numeric"        # ggf. GKV-Stichprobengewichte
  ),
  
  
  # Standardwerte der slots
  prototype=list(
    pseudo=1:100000,
    X = as(Matrix(matrix(c(0,1),10,10)),"dgCMatrix"),      # 10x10 Matrix aus 0 und 1
    Kost = rep(1,10),                                      # 1x10 Vektor bestehend aus der Zahl 1
    tage = rep(365L,10),# 1x10 Vektor bestehend aus der Zahl 365
    X = as(Matrix(matrix(c(0,1),10,10)),"dgCMatrix"),      # 10x10 Matrix aus 0 und 1
    jahr = 9999L,
    sample=1,
    model = "nicht angegeben"
  ),
  
  # Funtion zur kontrolle der Validität der eingebundenen Daten
  # Nicht ausgeführt wenn eine Initialisierungsfunktion ausgeführt wird!
  validity=function(object)
  {
    if(!length(object@Kost) == nrow(object@X) ||
       !length(object@Kost) == length(object@tage)
    ) {
      return("Objektlaenge ungleich")
    }# Wenn die Objekte unterschiedliche Dimensionen haben wird ein Fehler ausgegeben
    # if(det(crossprod(object@X)) == 0){
    #    return("X nicht invertierbar!")
    # }
    
    return(TRUE)
  }
)


# Summary -----------------------------------------------------------------

summary.morbiSet<-function(theObject){
  
  cat(paste0("Model: ",theObject@model, " im Datenjahr: ",theObject@jahr,"\n\n"))
  cat(paste0("Anz. Versicherte: ", round(nrow(theObject@X)/1000000,2)," Mio.","\n"))
  cat(paste0("Anz. Versichertenjahre: ", round((sum(theObject@tage)/365)/1000000,2)," Mio.","\n"))
  cat(paste0("Risikogruppen: ", ncol(theObject@X),"\n"))
  cat(paste0("Anz. Regionen: ",length(unique(theObject@AGS)),"\n"))
}



# rbind  -----------------------------------------------------------



rbind.morbiSet <- function(Set1, Set2){
  
  
  if(Set1@jahr==Set2@jahr & Set1@model==Set2@model){
    jahr <- Set1@jahr
    model <- Set1@model
    pseudo <- c(Set1@pseudo,Set2@pseudo)
    
    
    
    
    HMG_combine <- function(X1,X2){
      fehlt_in_1 <- names2[!(names2 %in% names1)]
      
      if(!length(fehlt_in_1)==0){
        for(HMG in fehlt_in_1){
          print(paste0(HMG," fehlt in Set1"))
          Stelle <- which(names2==HMG)
          if(Stelle>ncol(X1)){
            X1 <- cBind(X1[,1:(Stelle-1)],0)
            X1@Dimnames[[2]][Stelle] <- HMG
          }else{
            X1 <- cBind(X1[,1:(Stelle-1)],0,X1[,(Stelle):ncol(X1)])
            X1@Dimnames[[2]][Stelle] <- HMG
          }
        }
      }
      
      fehlt_in_2 <- names1[!(names1 %in% names2)]
      
      
      if(!length(fehlt_in_2)==0){
        for(HMG in fehlt_in_2){
          print(paste0(HMG," fehlt in Set2"))
          Stelle <- which(names1==HMG)
          if(Stelle>ncol(X2)){
            X2 <- cBind(X2[,1:(Stelle-1)],0)
            X2@Dimnames[[2]][Stelle] <- HMG
          }else{
            X2 <- cBind(X2[,1:(Stelle-1)],0,X2[,(Stelle):ncol(X2)])
            X2@Dimnames[[2]][Stelle] <- HMG
          }
        }
      }
      
      Match <- match(X1@Dimnames[[2]],X2@Dimnames[[2]])
      X      <- rBind(X1,X2[,Match])
      
      
      return(X)
      
    }
    
    
    
    
    X1 <- Set1@X
    X2 <- Set2@X
    
    names1 <- X1@Dimnames[[2]]
    names2 <- X2@Dimnames[[2]]
    
    if(length(names1)==length(names2)){
     
      if(!all.equal(names1,names2)){
        X <- HMG_combine(X1,X2)
      }else{
        X <- rBind(X1,X2)
      }
    }else{
      X <- HMG_combine(X1,X2)
    }
    
    
    
    
    Kost   <- c(Set1@Kost,Set2@Kost)
    tage   <- c(Set1@tage,Set2@tage)
    AGS    <- c(Set1@AGS,Set2@AGS)
    Zusatz <- rbind(Set1@Zusatz,Set2@Zusatz)
    sample <- c(Set1@sample,Set2@sample)

  }else{
    stop("MorbiSets passen nicht zueinander")
  }

  
  
  Set3 <-morbiSet(
    jahr=jahr,
    model=model,
    pseudo=pseudo,
    X=X,
    Kost=Kost,
    tage=tage,
    AGS=AGS,
    Zusatz=Zusatz,
    sample=sample
  )
  
    return(Set3)
  
}
