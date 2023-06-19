#' Gráfico del pronostico del periodo t, más el pronóstico al futuro.
#'
#' Realiza el pronóstico a la distancia requerida.
#'
#' @param base (data.frame) base de datos modificados con las funciones anteriores.
#' @param x (vector) es la variable independiente.
#'  @param d (vector) variable que determina la distancia de la predicción a ralizar.
#'@return Devuelve una grafica del pronostico.
#' @export
#'
#' @examples
#' \dontrun{
#' # Recibe los factores de ajustes calculados por la tercera función para hacer predicciones al futuro.
#' grafica(x="produccion", data, di = 12)
#' }
grafica <- function(x, data, di=12){
  produc <- data[,x]
  data$com <-produc
  n <- nrow(data)
  a <- data$dps[n]
  b <- data$fa[n]
  c <- c(1:di)
  pron <- a+(b*c)
  z <- 1:(n+di)
  y_pron <-c(data$pronostico, pron)
  y_prod <- c(data$com, rep(NA, di))
  gra <- plot (z, y_prod, type="l", col="black")
  lines(z, y_pron, type="l", col="blue")
  return(pron)
}
