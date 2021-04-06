require(Matrix)
require(data.table)

######################################################################
# Ein morbiSet als Ergebnis der RSA-Regression

# Definition --------------------------------------------------------------


fitMorbiRSA <- setClass(
  # Name der Klasse
  "fitMorbiRSA",

  # Slots der Klasse
  slots = c(
    Zuweisung = "numeric",             # Zuweisungen der Versicherten
	  Hochkosten = "numeric",            # Hockosten der Versicherten
    Coef="data.frame",                 # Koeffizienten der Regression
    Guete="data.frame",                # Guete der Regression
    Anteile="data.frame",
    Regionalfaktor="data.frame"
  ), 
  contains = "morbiSet",

  prototype=list(
    Zuweisung = rep(1,10)
  ),
  
  # Make a function that can test to see if the data is consistent.
  # This is not called if you have an initialize function defined!
  # validity=function(object)
  # {
  #   if(abs(sum(object@Kost) - sum(object@Zuweisung))<1000) {          # Teste of Summe der Kosten gleich Summe der Zuweisung
  #     return("Kosten ungleich Zuweisung")
  #   }
  #   return(TRUE)
  # }
)


# set Zuweisung -----------------------------------------------------------------

setGeneric(name="setZuweisung",
           def=function(theObject,Zuweisung,Hochkosten)
           {
             standardGeneric("setZuweisung")
           }
)

setMethod(f="setZuweisung",
          signature="fitMorbiRSA",
          definition=function(theObject,Zuweisung,Hochkosten)
          {
            theObject@Zuweisung<-Zuweisung                     # Setze Zuweisung
            
            theObject@Hochkosten<-Hochkosten					 # Setze Hochkosten
            
            sample <- theObject@sample
            
            R2 <- 1 - sum((Zuweisung-theObject@Kost)^2)/sum((theObject@Kost-mean(theObject@Kost))^2)             # Berechne R2 usw.
            MAPE <- sum(abs(theObject@Kost-Zuweisung))/length(theObject@Kost)
            CPM <- 1-sum(abs(theObject@Kost-Zuweisung))/sum(abs(theObject@Kost-mean(theObject@Kost)))
            
            
            theObject@Guete<-data.frame(R2,MAPE,CPM)                     # Packe alles in data.frame Guete
            
            
            return(theObject)                                  # Zurück an das Objekt
          }
)



# Set Coef ----------------------------------------------------------------


setGeneric(name="setCoef",
           def=function(theObject,V,B,N
           )
           {
             standardGeneric("setCoef")
           }
)

setMethod(f="setCoef",
          signature="fitMorbiRSA",
          definition=function(theObject,V,B,N
                              #,N1,N2,N3,N4,N5,N6,N7,N8
          )
          {
            Coef<-data.frame(Coef=names(B),B=B,RF=B/mean(theObject@Kost),Koepfe=N,SD=sqrt(V),t=B/sqrt(V))    # Sezte Koeffizienten, Risikofaktoren, Standardabweichung und t-Test
            n<-length(theObject@Kost)
            if(n==10){n<-Inf}
            Coef$p<-1-pt(abs(Coef$t),n-length(B))                                   # p-Wert für t-Test
            Coef$star<-""                                                                           # Signifikanz-"Sternchen" für t-Test
            for(i in 1:nrow(Coef)){
              if(!is.na(Coef$p[i])){
                if(Coef$p[i]<0.01){
                  Coef$star[i]<-"***"
                }else if(Coef$p[i]<0.025){
                  Coef$star[i]<-"**"
                }else if(Coef$p[i]<0.05){
                  Coef$star[i]<-"*"
                }
              }
            }
            names(Coef)[1]<-"coef"   
            
            Coef$Anteil_abs <- Coef$B*N
            Coef$Anteil_rel <- Coef$B*N/sum(Coef$B*N)
            

            theObject@Coef<-Coef                                                                    # Binde an das Objekt
            
            Temp <- data.table(Coef)
            Temp$typ <- substring(names(B),1,3)
              
            Anteile <- data.table(Temp[,list(Anteil_abs=sum(Anteil_abs)),by=typ])
            Anteile$Anteil_rel <- Anteile$Anteil_abs/sum(Anteile$Anteil_abs)
            theObject@Anteile <- Anteile
            
            return(theObject)                                                                       # Rückgabe des Objekt
          }
)

