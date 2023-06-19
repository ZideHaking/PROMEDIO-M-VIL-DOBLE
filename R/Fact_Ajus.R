#' Factores de ajuste.
#'
#' Realiza calculos de ajuste en los datos suavizados.
#'
#' @param data base de datos suavizados con la funcion del promedio movil1 y promedio movil2.
#' @param d (vector) variable que determina el orden del promedio movil.
#' @return Un data frame con los cálculos de los factores de ajsute.
#' @export
#'
#' @examples
#' \dontrun{
#' # Recibe los datos calculados por la segunda función
#' data <- fa(data, d = 12)
#' }
fa <- function(data, d=12){
  d2 <- d+1
  at <- (2*data$pmovil)-data$pmovil2
  data$dps <-at
  bt <-(2/(d-1))*(data$pmovil-data$pmovil2)
  data$fa <- bt
  pro <- data$dps+(data$fa*(d2-d))
  data$pronostico <-pro
  return(data)
}
