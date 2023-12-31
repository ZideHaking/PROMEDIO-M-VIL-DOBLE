% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Pro_Mov2.R
\name{promedio_movil2}
\alias{promedio_movil2}
\title{Promedio Móvil 2.}
\usage{
promedio_movil2(data, d = 12)
}
\arguments{
\item{data}{salida de la función promedio movil1.}

\item{d}{(vector) variable que determina el orden del segundo promedio movil a trabajar.}
}
\value{
Un data frame con los cálculos del segundo dato suavizado.
}
\description{
Realiza el segundo suavizamiento de los datos.
}
\examples{
\dontrun{
# Recibe los datos calculados de la primera función
data <- promedio_movil2(data, d = 12)
}
}
