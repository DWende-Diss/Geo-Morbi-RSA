
source('MRSA/morbiSet.R', echo=FALSE)
source('MRSA/fitMorbiRSA.R', echo=FALSE)
source('MRSA/Hochkosten_funktion.R', echo=FALSE)


# Definition der Funktion für ein theObject
# Funktion entspricht dem Morbi-RSA
# angewendet auf das theObject nach Kost = Xb + Fehler


MRSA<-function(BVA,
               Standfehler=TRUE,
               Hochkosten="kein",
               Hierarchie="keine",
               sample=TRUE,
               Annualisierung="",
               ...){
  
  # theObject - Daten der classe theObject
  # Standfehler - Standardfehler wird berechnet
  # Hochkosten - ein Hochkostenmodell wird angewendet (kein, RP1, RP2, HCP, KP100)
  # neg_coef_control - negative Regressionskoeffizienten werden eliminiert
  # ...              - weitere Einstellungen für Hochkostenmodelle (siehe jeweiliges Hochkostenmodell)
  
  ### Hilfsfunktion zur Lösung eines linearen Glleichungssystems mittels qr-Zerlegung dünnbesetzer Matrizen


# Lösung des Gleichungssystems --------------------------------------------

    
  
  solver<-function(X,Y,w){
    
    # X - Matrix (dünn)
    # Y - Zielvektor
    # w - Gewichtungsmatrix
    # M - Matrix die den "Ort" der nicht Null Elemente von X angibt
    
    
    Xw<-X        # Zeiger Xw auf die Daten von X
    Xw<-X*w   # Zeiger der Werte von Xw die nicht Null sind auf die Werte der Gewichtungsmatrix die am Gleichen Ort stehen
    # dieser Schritt Ordnet allen Werten von Xw die nicht Null sind einen neuen Zeiger zu der auf die Werte von w zeigt,
    # die sich ergeben würden bei X*w
    
    return(qr.solve(crossprod(Xw,X),crossprod(Xw,Y)))  # Lösung des Gleichungssystems (X'WX)b=X'Y mittels qr Zerlegung
  }


# Nicht Negativitätsbedingung ---------------------------------------------

  
  Null_Kontrolle <- function(B){
    for(i in 1:length(B)){
      if(B[i] < 0 & (substring(names(B[i]),1,2)=="AG")==FALSE){
        cat(paste0(names(B)[i]," < 0 "))
        cat("\n")
      }
    }
    return(which(B<0 & (substring(names(B),1,2)=="AG")==FALSE))
  }
  
  Null_Anwenden  <- function(Fehler,theObject){
    X <- theObject@X
    X <- X[,-Fehler]
    theObject@X <- X
    return(theObject)
  }

# Main body ---------------------------------------------------------------

  
  print(paste("Sample:",sample))
  
  fit <- new("fitMorbiRSA",BVA)  # Initialisiere ein leeres Objekt vom Typ fitMorbiRSA

  w <- BVA@tage/max(BVA@tage)    # Definiere Gewichtung als Tage/max(Tage)
  
  HK <- Risikopool[[Hochkosten]](BVA,...)
  Y <- HK$LA_NK/w
  Z_HK<-HK$HK_Z 
  
  # # Auswahl des Hochkostenfalls
  # if(Hochkosten=="kein"){
  #   Y<-BVA@Kost/w                   # Anualisieren und Ergebnis mit dem Zeiger Y versehen
  #   Z_HK<-rep(0,length(Y))               # Hochkosten sind 0 und jeder hat diese Hochkosten von 0 -> Vektor nur mit 0
  # }else if(Hochkosten=="KP100"){
  #   print("Risikopool KP100")
  #   HK<-KP100(BVA,...)              # Hochkosten aus KP100 angewendet auf theObject unter den Einstellungen ...
  #   Y<-HK$LA_NK/w                        # Anualisieren und Ergebnis aus Normalkosten mit dem Zeiger Y versehen
  #   Z_HK<-HK$HK_Z                        # Hochkosten sind das Ergebnise und Ergeben extra Zuschlage
  # }else if(Hochkosten=="RP1"){
  #   print("Risikopool RP1")
  #   HK<-RP1(BVA,...)                # Hochkosten aus KP100 angewendet auf theObject unter den Einstellungen ...
  #   Y<-HK$LA_NK/w                        # Anualisieren und Ergebnis aus Normalkosten mit dem Zeiger Y versehen
  #   Z_HK<-HK$HK_Z
  # }else if(Hochkosten=="RP2"){
  #   print("Risikopool RP2")
  #   HK<-RP2(BVA,...)                # Hochkosten aus KP100 angewendet auf theObject unter den Einstellungen ...
  #   Y<-HK$LA_NK/w                        # Anualisieren und Ergebnis aus Normalkosten mit dem Zeiger Y versehen
  #   Z_HK<-HK$HK_Z
  # }else if(Hochkosten=="HCP"){
  #   print("Risikopool HCP")
  #   HK<-HCP(BVA,...)                # Hochkosten aus KP100 angewendet auf theObject unter den Einstellungen ...
  #   Y<-HK$LA_NK/w                        # Anualisieren und Ergebnis aus Normalkosten mit dem Zeiger Y versehen
  #   Z_HK<-HK$HK_Z
  # }else{
  #   stop("Kein Risikopool definiert")
  # }
  
#  print("0-Modell")
  
  if(sample==TRUE){
    w <- w*BVA@sample
    w[w==0] <- 1E-10
  }
  
  B<-solver(BVA@X,Y,w)                         # Löse Gleichungssystem über solver
  
  if(!Hierarchie=="keine"){
    
    if(Hierarchie=="negativ"|Hierarchie=="Hierarchie"){
      B_orig <- B0 <- B
      Fehler <- c()
      BVA_temp <- BVA
      anzFehler<-0
      repeat{
        #if(sum(B<0)<1){break}
        Fehler <- c(Fehler,Null_Kontrolle(B))
        if(length(Fehler)==anzFehler){
          print("stop")
          break
        } else
        {
          anzFehler<-length(Fehler)
          print(length(Fehler))
        }
        BVA_temp <- Null_Anwenden(Fehler,BVA)
        B0 <- solver(BVA_temp@X,Y,w)
        
        B <- B_orig
        B[Fehler] <- 0
        B[-Fehler] <- B0
        
      }
      
    }else if(Hierarchie=="Hierarchie"){
      
      data(Hierarchie_Daten)
      if(!BVA@jahr %in% Hierarchie_Daten$Jahr){
        stop(paste0("Keine Hierarchisierung für Datenjahr ",BVA@jahr," möglich"))
      }
      Fehler_0 <- Fehler
      
      B_orig <- B
      
      Fehler<-Hierarchie_Kontrolle(B,BVA_temp)
      BVA_temp<-Hierarchie_Anwendung(Fehler,BVA_temp)
      
      B0 <- solver(BVA_temp@X,Y,w,BVA_temp@X@i+1)
      
      B <- Hierarchie_Koef(B0,B_orig,Fehler)
      
      B[Fehler_0] <- 0
      
    }
    
    
    
  }
  Z0 <- (BVA@X%*%B)@x
  Z <- Z0*BVA@tage/max(BVA@tage) + Z_HK
  
  fit<-setZuweisung(fit,Z,Z_HK)                          # Erweitere das Ojekt fit um Zuweisungen und Hochkosten
  
  
  # Prüfe die Summentreue
  # Wenn Zuweisung und Kosten mehr als 100 Euro in Summe auseinander dann Fehler
  
  if(sample==TRUE){
  
  if(abs(sum((Z-fit@Kost)*fit@sample))>100){
    warning(paste0("Fehler in der Regresion (Zuw.|Kosten): ",sum(Z*fit@sample)," | ",sum(fit@Kost*fit@sample)))  # Sage mir das etwas Falsch ist
  }
  }else{
    
    if(abs(sum((Z-fit@Kost)))>100){
      warning(paste0("Fehler in der Regresion (Zuw.|Kosten): ",sum(Z)," | ",sum(fit@Kost)))  # Sage mir das etwas Falsch ist
    }  
    
  }
    
  
  # Berechne Standardfehler von Wahr


# Berechne Verteilung -----------------------------------------------------

if(sample==TRUE & length(fit@sample)==nrow(fit@X)){
  N <- crossprod(fit@X,fit@sample)@x
}else{
  N <- colSums(fit@X)
}  
  
if(!length(names(B))==length(B)){
  names(B) <- rep("NA",length(B))
}  
  
  
# Standardfehler ----------------------------------------------------------

  
  if(Standfehler==TRUE){
    print("Varianz-Kovarianz")
    
    freiheitsG<-nrow(fit@X)-(ncol(fit@X))     # Freiheitsgrade sind Zeilen von X minus Spalten von X
    
    u<-fit@Kost-Z                                  # Fehler u ist Kosten minus Zuweisung
    sigma<-(t(u)%*%u)/freiheitsG	                      # Standardabweichung der Fehler (sigma) ist Summe u^2 / Freiheit
    sigma<-sigma[1,1]                                   # sigma zu skalar (bzw. erste Element der Einelementigen Matrix)
    
    
    V<-diag(qr.solve(crossprod(fit@X*w,fit@X)))*sigma # Varianz der Koeffizienten siehe Standardwerke der Statistik
    
    fit<-setCoef(fit,V,B,N)              # Ergänze Objekt fit um Koeffizienten und deren Standardfehler
  }
  else{
    fit<-setCoef(fit,rep(0,length(B)),B,N)              # Ergänze Objekt fit aber ohne Standardfehler
  }
  
  
  return(fit)                                         # Gibt fit zurück
  
}