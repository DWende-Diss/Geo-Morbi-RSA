Namen.Anpassen <- function(names){
  
  add.0 <- function(nummer,n=3){
    for(i in 1:length(nummer)){
       if(nchar(nummer[i])<n){
         for(j in 1:(n-(nchar(nummer[i])))){
           nummer[i] <- paste0("0",nummer[i])
         } 
      }
    }
    return(nummer)
  }
  
  tryCatch({
  agg <- substring(names,1,3) == "AGG" & nchar(names) < 7
  nummer <- substring(names[agg],4,99)
  names[agg] <- paste0("AGG",add.0(nummer))
  }, error =function(e){}
  )
  
tryCatch({  
  agg <- substring(names,1,3) == "EMG"
  nummer <- substring(names[agg],4,99)
  names[agg] <- paste0("EMG",add.0(nummer))
}, error =function(e){}
)
  tryCatch({    
  agg <- substring(names,1,3) == "KEG"
  nummer <- substring(names[agg],4,99)
  names[agg] <- paste0("KEG",add.0(nummer))
  }, error =function(e){}
  )
  
  tryCatch({    
  hmg <- substring(names,1,3) == "HMG"
  nummer <- substring(names[hmg],4,99)
  names[hmg] <- paste0("HMG",add.0(nummer))
  }, error =function(e){}
  )
  
  tryCatch({    
  agg <- substring(names,1,7) == "AGG_Tod"
  nummer <- substring(names[agg],8,99)
  names[agg] <- paste0("AGG",add.0(nummer),"_Tod")
  }, error =function(e){}
  )
  
  tryCatch({    
    agg <- substring(names,1,10) == "Bundesland"
    nummer <- substring(names[agg],11,99)
    names[agg] <- paste0("BL",as.numeric(nummer))
  }, error =function(e){}
  )
  
  return(names)
  
}