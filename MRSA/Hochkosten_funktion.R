### Gewichtungsfunktionen für Hochkostenfälle




##############################################################################################################
#                                                                                                            #
#                                      Hochkostengewichte nach Huber (Modell RP2)                            #
#                                                                                                            #
##############################################################################################################



# Gewichtung nach Huber, Lösung des Problem im Papier von Schillo und Wasem mit Variablen kritischen Wert

# weight <- function(Kost,X,tage,rkrit=0.01,type="huber",p=.95){
# 
#   # Kost - Kosten der Versicherten
#   # X    - Designmatrix
#   # tage - Versichertentage
#   ###  referenzieren auf morbiSet und werden in Funktion z.B. MRSA verwendet
# 
#   # rkrit - kritischer Wert für die Wiederholung der Gewichtung
#   # p    - kritischer Wert für Hochkostenfälle (alpha-Fehler der Detektion) standard 1%
#   # type - Type der "robusten" Regressionsgewichte (huber, bisquare)
# 
# # Berechnet die Stelle (x) an der Verteilungsfunktion für Hochkostenfälle die einen p-Wert von rkrit hat
# # Berechnung siehe Publikationen von Huber und White
# 
#   if(type=="huber"){
#     level<-9.34682 - 1.55279*qchisq(p,2) + 1.60174*qchisq(p,3) +0.000004*(qf(p,2,2))+2.53344*log(p)+8.46828*p^(1/2)-19.67303*p^(1/3)
#     # Standardwert in der Literatur, auch postuliert als robuste Regression ~ p von 0,05
#     #level<-1.345
#   }else{
#     level<-0.07955 - 0.10426*qchisq(p,2) - 1.34641*qchisq(p,3) + 1.38632*qchisq(p,4)-0.00018*(qf(p,2,2))+0.68758*(qf(p,7,7))
#     # Standardwert in der Literatur, auch postuliert als robuste Regression ~ p von 0,05
#     #level<-4.685
#   }
# 
# 
# # Lösung des linearen Gleichungssystems, siehe MRSA für Erklärung
# 
#   solver<-function(X,Y,w,M){
#     Xw<-X
#     Xw@x<-w[M]
#     return(qr.solve(crossprod(Xw,X),crossprod(Xw,Y)))
#   }
# 
# 
#   M<-X@i +1                 # Stelle an der X Elemente enthalten die nicht null sind (Beachten R zählt von 1 nicht von 0)
#   w<-w2<-tage/365           # Gewichtungsvektor aufgrund der Versichertentage (w2 ist das Tagesgewicht /  w ist veränderlich)
#   Y<-Kost/w                 # Anualisierung (Kosten durch Gewichtung)
# 
#   # Wiederhohle bis Änderungen kleiner rkrit
#   repeat{
# 
#     B<-solver(X,Y,w,M)                      # Löse lineares Gleichungssystem mit Gewichtung w
# 
#     # Ermitteln der Huber-Gewichte
#     if(type=="huber"){
#       r<-abs(Y-X%*%B)@x                    # Absoluter Fehler der Regression (~ MAPE)
#       sigma<-median(r)/0.6745              # robuste Schätzung der Standardabweichung der Fehler
# 
#       k<-level*sigma                       # oberer Grenzwert für Hochkosten ~ Quantil * Standardabweichung
#       w3<-rep(1,length(Y))                 # Definiere Standardgewichtung zu 1 (~ keine Hochkkosten)
#       w3[r>k]<-k/r[r>k]                    # Wer k übersteigt erhält das Gewicht k/r also Schwelle/absolut Fehler
# 
#     # Ermittelt der biquare-Gewichte (siehe Huber)
#     }else if(type=="bisquare"){
#       r<-(Y-X%*%B)@x
#       sigma<-median(abs(r))/0.6745
# 
#       k<-level*sigma
#       w3<-rep(0,length(Y))                 # Wer k übersteigt erhält das Gewicht k/r also Schwelle/absolut Fehler
#       w3[abs(r)<k]<-(1-(r[abs(r)<k]/k)^2)^2
#     }else{
# 
#       Z<-log((Y-X%*%B)^2)                 # Schätzer für die Varianz der individuellen Fehler ~ sigma(u_i)
# 
#       w3<-1/sqrt(exp(X%*%solver(X,Z,w2,M)))@x # Alternative Schätzung der Gewichte über Varianz der Einzelfehler
#     }
# 
#     w<-w2*w3                             # Neue Gewichtung ist Tagesgewicht mal Hochkostengewicht
# 
#     B2<-solver(X,Y,w,M)                  # Neuer Lösungsvektor durch lineare Lösungsgleichung unter neuen GEwichten
# 
#     krit<-abs(sum(B2)/sum(B)-1)          # Teste ob absolutbetrag der relativen Veränderung des Lösungsvektor kleiner als r_krit ist
#     print(krit)                          # Ausgabe von krit auf dem Monitor
# 
#     if(krit<rkrit){
#       break                             # Wenn kriterium Erfüllt dann stopp sonst nächste Iteratation
#     }
#     #print(k)
#   }
# 
#   return(w3)                           # Ausgabe des letzten Vektors für Hochkostengewicht
# }

##############################################################################################################
#                                                                                                            #
#                                      Cross-Validierung für Hochkosten (Modell HCP)                         #
#                                                                                                            #
##############################################################################################################

# Funktion zur Cross-Validierung (Wer ist ein Hochkostenfall)
# Hochkostenfall -> Hochkostenpool
# -> Hochkostenpool bekommt mittlere Zuschläge aller Hochkostenfälle
# -> nicht Hochkostenfall bekommt Morbi-RSA Zuschlag
# -> trade off zwischen Zuweisungsgenauigkeit der Hochkosten und Zuweisungsgenauigkeit Morbi-RSA
# -> Wirkung auf R^2 apriori unklar aber empirisch optimal bei ca 1% Versicherte sind Hochkosten


CV_q<-function(Kost,X,tage,part=10,chain=1,k=seq(.9,1,.01)){

  # morbiSet = Daten als morbiSet
  # part	= Größe und Anzahl der Testmengen (10 - 10 Testmengen a 1/10 gelernt auf den jeweils anderen 9/10)
  # chain = wieviele Zufallsstarts
  # k		= Erwarteter Bereich der Hochkosten hier 90% bis 100%-Quantil (teuersten) in 0,1% Schritten



  library(speedglm)                            # hier brauche ich kurz die speedglm

  RSS<-TSS<-matrix(NA,(chain),length(k))       # Definiere Kontainer für Ergebnisse
  N<-POOL<-CV<-c(1:length(k))                  # Definiere Länge der Durchläufe


# Lösung für lineare Gleichungssystem, siehe MRSA
  solver<-function(X,Y,w){
    M<-X@i +1
    Xw<-X
    Xw@x<-w[M]
    return(solve(crossprod(Xw,X),crossprod(Xw,Y)))
  }


  n_data<-nrow(X)                              # Wahre Anzahl der Daten
  frac<-rep(c(1:part),n_data/part)             # Zerlege Daten in Teile (regelmäßig d.h. z.B. 1,2,3,4, ... 1,2,3,4, ... usw.)
  frac<-sample(frac)                           # Mische die Zuordnung zu den Teilen

# Berechnungsfunktion für den Risikopool
  pool<-function(Y,k){

    # Y - Zielvektor der Kosten
    # k - aktuelles Quantil der Hochkosten

    q<-quantile(Y,k)                          # kritischer Wert der Hochkosten (k-tes Quantil der Kosten)
    quant<-Y<=rep(q,length(Y))                # Hochkosten = Falsch wenn Kosten größer kritisch


    pool<-mean(Y[Y>rep(q,length(Y))])         # Poolzuweisung ist Mittelwert der Kosten die größer als kritisch sind
    return(list(q=q,quant=quant,pool=pool))   # Rückgabe der Werte
  }

# Berechnung der Nichthochkosten Zuweisung
  reg<-function(Y,X,w,quant){

    # Wie immer sowie
    # quant - Vektor der Wahr ist wenn nicht Hochkostenversicherter

     if(sum(quant==FALSE)==0){
        B<-solver(X,Y,w)                         #
     }else{
       B<-solver(X[quant,],Y[quant],w[quant])
      #B<-speedglm.wfit(Y,X,weights=w)$coef       # Lösung des Gleichungssystems mit der Speedglm (kontrolle auf )
     }
    return(B)
  }


  # Berechnung der Bewertungsindizes für die Cross-validierung
  Res<-function(B,X,Y,w,pool,quant){

    # Werte oben erklärt (X,Y,w)
    # B - Ergebnisvektor
    # pool - Grenzwert für den Pool
    # quant - Vektor der Wahr wenn im Pool

    if(sum(quant==FALSE)==0){
      RES1<-sum(((Y-X%*%B)*w)^2)       # Residualquadratsumme der nicht-Pool-Versicherten
      RES2<-0                          # Residualquadratsumme der Pool-Versicherten
    }else{
      quant<-Y<=pool                   # keine Ahnung warum nochmal, hatte bestimmt einen Sinn
      n<-length(Y)                     # Anzahl der Beobachtungen
      RES1<-sum(((Y[quant]-X[quant,]%*%B)*w[quant])^2)                                     # Residualquadratsumme der nicht-Pool-Versicherten
      RES2<-sum(((Y[quant==FALSE]-rep(pool,(n-sum(quant))))*w[quant==FALSE])^2)            # Residualquadratsumme der Pool-Versicherten
    }
    return(list(RES1=RES1,RES2=RES2,RES=RES1+RES2))        # Rückgabe der Ergebnisse
  }

  w<-tage/max(tage)                                       # Versichtentag-Gewicht
  Y<-Kost/w                                               # Annualisierung


  for (j in 1:chain){                                     # für 1 bis n Zufallsversuche


    for (i in 1:length(k)){                               # für alle kritischen Werte

      print(paste("k= ",k[i]," ON CHAIN: ",j,sep=""))     # Zeige was er tut
      A<-pool(Y[!frac==j],k[i])                           # Informationen aus Funktion pool für Lernmenge j und kritischen Wert i
      n<-nrow(X[!frac==j,])-sum(A$quant)                  # Anzahl Beobachtung bei siehe oben drüber

      B<-solver(X[!frac==j,],Y[!frac==j],w[!frac==j])     # MRSA für Lernmenge -> Lösungsvektor

      R<-Res(B,X[frac==j,],Y[frac==j],w[frac==j],A$pool,A$quant)  # Zielwert der Cross-Validierung für Testmenge

      # Kopieren der Ergebnisse in Kontainer
      N[i]<-n
      POOL[i]<-A$pool
      RSS[j,i]<-R$RES                                                                        # residual sum of squares
      TSS[j,i]<-sum(((Y[frac==j]-rep(mean(Y[frac==j]),length(Y[frac==j])))*w[frac==j])^2)    # total sum of squares
      print(R$RES)
    }

  }

  CV<-1-colSums(RSS)/colSums(TSS)                                                           # R2 als RSS/TSS (als Mittel über alle Versuche eines kritischen Werts)
  plot(k,CV)                                                                                # plotten der Ergebnisse


  return(list(CV=CV,n=N,pool=POOL,TSS=TSS,RSS=RSS,k=k))                                     # Rückgabe der Ergebnisse
}



##############################################################################################################
#                                                                                                            #
#                                     Hochkostenmodell (KP100 - Trunkierung)                                 #
#                                                                                                            #
##############################################################################################################

# Hochkostenfunktion
KP100<-function(Data,HK_Grenze=100000,HK_Ausgleich=0.8){

  # Daten vom typ morbiset
  # HK_Grenze (wenn nichts anderes angegeben bei ..., dann 100.000)
  # HK_Ausgleich (relativer Ausgleich, wenn nicht angegeben bei ..., dann 80%)

  HK<-Data@Kost>HK_Grenze							# Vektor immer Wahr wenn Grenze überschritten

  HK_Z<-rep(0,length(Data@Kost))      # Hochkostenzuweisung ist apriori 0
  HK_Z[HK]<-(Data@Kost[HK]-HK_Grenze)*HK_Ausgleich	# Hochkostenzuschlag ist x% Ausgleich für alle Hochkostennfälle
  LA_NK<-Data@Kost                    # alle nicht Hochkosten sind Normalkosten
  #LA_NK[HK]<-HK_Grenze                # alle Hochkosten werden Trunkiert auf Grenze

  ### 20% der Hochkoste werden nicht ausgeglichen ->
  # Geld muss als Gieskanne auf alle Verteilt werden sonst fehlt es in Summe
  # Alle Kosten als Gieskanne um den Fehlbetrag pro Kopf erhöhen hat den gleichen Effekt
  # Hier also Normalkosten alle Kosten + Fehlkosten pro Kopf

  # LA_NK<-LA_NK+sum((Data@Kost[HK]-HK_Grenze)*(1-HK_Ausgleich))/length(LA_NK)
  LA_NK[HK] <- HK_Grenze+(LA_NK[HK]-HK_Grenze)*(1-HK_Ausgleich)
  
  return(list(LA_NK=LA_NK,HK_Z=HK_Z))  # Rückgabe der Ergebnisse

}

##############################################################################################################
#                                                                                                            #
#                                     Hochkostenmodell (RP1 / Wasem)                                         #
#                                                                                                            #
##############################################################################################################

# Hochkostenfunktion
RP1<-function(Data,HK_Grenze=30000,HK_Ausgleich=0.8){


  # Daten vom typ morbiset
  # HK_Grenze (wenn nichts anderes angegeben bei ..., dann 30.000)
  # HK_Ausgleich (relativer Ausgleich, wenn nicht angegeben bei ..., dann 80%)

  fit.MRSA<-MRSA(Data,Standfehler=FALSE)                  # Morbi-RSA Regression

  HK<-(fit.MRSA@Kost-fit.MRSA@Zuweisung)>HK_Grenze  		  # fixe Grenze für Deckungslücke bei 30.000

  HK_Z<-rep(0,length(Data@Kost))                          # Hochkostenzuweisung als 0 festlegen
  HK_Z[HK]<-(Data@Kost[HK]-fit.MRSA@Zuweisung[HK])*HK_Ausgleich                    # Hochkostenzuweisung von Hochkkostenversicherten

  LA_NK<-Data@Kost                                        # Ausgleich im Morbi-RSA sind alle Kosten
  LA_NK[HK]<-LA_NK[HK]-HK_Z[HK]		                        # - Bereits durch ISt-Ausgleich erfüllte Kosten

  return(list(LA_NK=LA_NK,HK_Z=HK_Z))                     # Rückgabe der Werte

}

##############################################################################################################
#                                                                                                            #
#                                     Hochkostenmodell (RP2 / Huber)                                         #
#                                                                                                            #
##############################################################################################################

# Hochkostenfunktion
RP2<-function(Data,HK_Grenze=10000000,HK_Ausgleich=0){

  # Daten vom typ morbiset
  # HK_Grenze (wenn nichts anderes angegeben bei ..., dann 10.000.000)
  # HK_Ausgleich hat hier keinen Nutzen wenn nicht anders angegeben (sonst ist es der Ausgleich ~ RP1)

  w.out<-weight(Data@Kost,Data@X,Data@tage,rkrit=0.05,p=1-1/HK_Grenze)			# Huber Gewichte durch Funktion weight

  if(HK_Ausgleich>0){
    w.out[w.out<1]<-HK_Ausgleich              # Falls Ausgleich angegeben, dann für alle Hochkostenfälle anwenden
  }

  HK<-w.out<1                                 # Hochkosten sind alle mit Gewicht kleiner 1

  HK_Z<-rep(0,length(Data@Kost))              # Hochkostenzuweisung immer 0
  HK_Z<-Data@Kost*(1-w.out)										# 1-Huber Gewichte als Anteil am Risikopool multipliziert mit Kosten ergibt Hochkostenzuschlag

  LA_NK<-Data@Kost*w.out											# Huber Gewichte als verbleibende Anteil in der Regression

  return(list(LA_NK=LA_NK,HK_Z=HK_Z))         # Rückgabe der Werte

}

##############################################################################################################
#                                                                                                            #
#                                     Hochkostenmodell (HCP, Capitation)                                     #
#                                                                                                            #
##############################################################################################################

# Hochkostenfunktion
HCP<-function(Data,HK_Grenze=0,HK_Ausgleich=0){

  # Daten vom typ morbiset

  HCP<-CV_q(Data@Kost,Data@X,Data@tage,part=10,chain=1,k=seq(.97,1,.01))		# dynamische Berechnung der Hochkonstegrenze als Quantil
                                                                            # Siehe Funktion "Oben"

  k<-HCP$k[which(HCP$CV==max(HCP$CV))]						# der beste Quantilswert im Sinne des R2

  if(HK_Grenze>0){
    HK_Ausgleich<-HK_Grenze
    warning(paste0("Grenze haendisch gewaehlt bei: ", HK_Ausgleich))
  }else{
    HK_Ausgleich<-quantile(Data@Kost,k)          # Hochkostengrenze Aufgrund der Berechnung
  }

  if(HK_Grenze>0){
    warning("Hochkosten Ausgleichsfaktor hat keinen Effekt")
  }

  HK<-Data@Kost>HK_Ausgleich                    # Hochkostenversicherte sind die über der Grenze

  HK_Z<-rep(0,length(Data@Kost))                # Zuweisungen für Hochkosten sind erstmal 0

  GP_HK<-mean(Data@Kost[HK])										# Mittelwert der Kosten = Zuweisung

  HK_Z[HK]<-GP_HK                               # Hochkostenzuschlag
  LA_NK<-Data@Kost
  LA_NK[HK]<-0                                  # Jeder mit Hochkosten erhält !keine Zuschläge durch Morbi-RSA


  return(list(LA_NK=LA_NK,HK_Z=HK_Z))           # Rückgabe der Werte

}


# kein

kein <- function(Data){
  HK <- rep(0,length(Data@Kost))                    # Hochkostenversicherte sind die über der Grenze
  
  HK_Z<-rep(0,length(Data@Kost))                # Zuweisungen für Hochkosten sind erstmal 0
  
  LA_NK<-Data@Kost
  
  return(list(LA_NK=LA_NK,HK_Z=HK_Z))           # Rückgabe der Werte
}

Risikopool <- list(kein=kein
                   ,KP100=KP100
                   ,RP1=RP1
                   ,RP2=RP2
                   ,HCP=HCP)