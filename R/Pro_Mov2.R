#' Promedio Móvil 2.
#'
#' Realiza el segundo suavizamiento de los datos.
#'
#' @param data salida de la función promedio movil1.
#' @param d (vector) variable que determina el orden del segundo promedio movil a trabajar.
#' @return Un data frame con los cálculos del segundo dato suavizado.
#' @export
#'
#' @examples
#' \dontrun{
#' # Recibe los datos calculados de la primera función
#' data <- promedio_movil2(data, d = 12)
#' }
promedio_movil2 <- function(data, d=12){
  pmov2 <- data[,"pmovil"]
  n <- length(pmov2)
  p_movil2 <- numeric()
  longitud <-n-d+1
  for(i in 1:longitud){
    p_movil2[i] <-mean(pmov2[((d-1)+i):i])
  }
  p_movil2 <-c(rep(NA, d-1), p_movil2)
  data$pmovil2 <-p_movil2
  return(data)
}
