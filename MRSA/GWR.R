# Definition der Funktion für ein morbiSet
# Funktion entspricht dem Morbi-RSA mit GWR-Regionalfaktor
# angewendet auf das morbiSet nach Kost = Xb + GWR + Fehler

source('MRSA/MRSA.R', echo=FALSE)
source('MRSA/Kernel.R', echo=FALSE)


GWR  <- function(object
              ,Kerneltyp="GWR"
              ,GIDtyp="AGS"
              ,krit=1
              ,use_sample=TRUE
              ,...){	# GWR-Regression

  # morbiSet - Daten der classe morbiSet
  # k_krit   - kritischer Wert für die Gewichtung
  # exakt    - GWR exakt oder aproximativ (exakt nur bei kleinem Datensatz notwenig)
  # Standfehler - Standardfehler wird berechnet
  # Hochkosten - ein Hochkostenmodell wird angewendet (kein, RP1, RP2, HCP, KP100)
  # neg_coef_control - negative Regressionskoeffizienten werden eliminiert
  # ...              - weitere Einstellungen für Hochkostenmodelle (siehe jeweiliges Hochkostenmodell)

eval(parse(text=paste0("object","@",GIDtyp,"->","GID")))
GID <- as.numeric(as.character(GID))
  
print("Kernel")

load("Referenzdaten/Ref_AGS_Points.Rdata")

GID_is_valid <- GID %in% Point$UID
GID_valid <- GID[GID_is_valid]
valid <- match(GID,GID_valid)

Kernel <- setKernel(Point,typ="GWR",GID=GID_valid,krit=krit )

  print("MRSA")

  
  
  if(sum(GID_is_valid)<length(object@Kost)){
    warning("Versicherte ohne PLZ gefunden")
    object@X <- cBind(object@X,!GID_is_valid)  # Wenn es unbekannte PLz gibt, dann sind diese als Restgröße (Dummy) an die Designmatrix anzuhängen (cBind kopiert links dran)
  }
  

  fit.MRSA.0 <- MRSA(object,Standfehler=FALSE,sample=use_sample,...)	     # 
  E <- fit.MRSA.0@Kost-fit.MRSA.0@Zuweisung							# E - Deckungslücke des MRSA



#################################################################################################
#                                                                                               #
#                                                                                               #
#                           GWR-Regression				                                        #
#                                                                                               #
#################################################################################################
    print("GWR")

GWR.start <- FALSE
 
repeat{     
      fit.Kernel <- estKernel(Kernel,E,fit.MRSA.0@tage*fit.MRSA.0@sample)
      
      if(GWR.start==FALSE){
        object@X <- cBind(object@X,REG=fit.Kernel@yhat[valid])
        GWR.start <- TRUE
      }else{
        object@X[,dim(object@X)[2]] <- fit.MRSA@Coef[dim(object@X)[2],]$B*object@X[,dim(object@X)[2]]+fit.Kernel@yhat[valid]
      }
      
      fit.MRSA <- MRSA(object,Standfehler=FALSE,sample=use_sample,...)
      E <- fit.MRSA@Kost-fit.MRSA@Zuweisung
      
      if(abs(fit.MRSA@Coef[dim(object@X)[2],]$B - 1) < 10^-5){
        fit.MRSA <- MRSA(object,sample=use_sample,...)
        break
      }else{
        print(fit.MRSA@Coef[dim(object@X)[2],]$B)
      }
}      


    fit@Regionalfaktor <- data.frame(fit.Kernel@notes_of_var,fit.Kernel@b)


  return(fit)  # Rückgabe Ergebnisobjekt

}
