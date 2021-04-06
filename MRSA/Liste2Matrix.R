list.toMatrix <- function (liste, pseudo = c()) 
{
  if (!length(pseudo) == 0) {
    obs <- length(pseudo)
  }
  else {
    pseudo <- unique(liste[, 1])
    obs <- length(pseudo)
  }
  
  IN <- liste[,1] %in% pseudo
  liste <- liste[IN,]
  
  dim <- length(unique(liste[, 2]))
  M <- new("dgTMatrix")
  M@Dim <- c(obs, dim)
  M@i <- match(liste[, 1], pseudo) - 1L
  M@j <- match(liste[, 2], unique(liste[, 2])) - 1L
  M@x <- rep(1, nrow(liste))
  M@Dimnames <- list(as.character(pseudo), as.character(unique(liste[,2])))
  return(as(M, "dgCMatrix"))
}