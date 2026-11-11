library(quantmod)
library(FinTS)
library(xts)
library(dygraphs)
library(RColorBrewer)
library(PerformanceAnalytics)
library(fBasics)

#Periodo de inicio-fin
start <- "2018-02-01"
end   <- "2020-12-31"


#Se obtienen los redimientos directamente de Yahoo Finanza
rend <- function(simbolo, start, end){
  
  datos <- getSymbols(
    simbolo,
    src = "yahoo",
    auto.assign = FALSE
  )
  
  datos <- na.omit(datos)
  
  precios <- Ad(datos)
  
  rendimiento <- periodReturn(
    precios,
    period = "daily",
    subset = paste(start, end, sep = "::"),
    type = "arithmetic"
  )
  
  assign(simbolo, rendimiento, envir = .GlobalEnv)
}

#Descarga de datos de Facebook y Apple
rend("META", start, end)
rend("AAPL", start, end)


#Se unen los 2 conjuntos de datos, y se eliminan datos faltantes
rends <- na.omit(merge(META, AAPL))

colnames(rends) <- c("META", "APPLE")

#Gráfica para lustrar los rendimientos diarios tanto de Meta como de Apple
dygraph(
  rends,
  main = "Rendimientos diarios META y APPLE"
) %>%
  dyAxis("y", label = "Rendimiento") %>%
  dyOptions(colors = brewer.pal(4, "Set1"))

#Se obtienen algunos valores estadisticos
basicStats(rends[,1]) #Meta
basicStats(rends[,2]) #Apple


#Se calcula el rendimiento promedio y el riesgo de las acciones de Meta y Apple
media_META <- mean(rends[,1])
media_AAPL <- mean(rends[,2])

riesgo_META <- sd(rends[,1])
riesgo_AAPL <- sd(rends[,2])

cat("META\n")
cat("Rendimiento promedio:", media_META, "\n")
cat("Riesgo:", riesgo_META, "\n\n")

cat("APPLE\n")
cat("Rendimiento promedio:", media_AAPL, "\n")
cat("Riesgo:", riesgo_AAPL, "\n")


#Tabla de riesgo y rendimiento
riesgo_rendimiento <- data.frame(
  Activo = c("Meta", "Apple"),
  Riesgo = c(riesgo_META, riesgo_AAPL),
  Rendimiento = c(media_META, media_AAPL)
)

print(riesgo_rendimiento)


#Comparación del riesgo vs rendimiento
plot(
  riesgo_rendimiento$Riesgo,
  riesgo_rendimiento$Rendimiento,
  pch = 19,
  col = c("blue", "red"),
  cex = 2,
  xlab = "Riesgo (Desviación estándar)",
  ylab = "Rendimiento esperado",
  main = "Relación Riesgo-Rendimiento"
)

lines(
  riesgo_rendimiento$Riesgo,
  riesgo_rendimiento$Rendimiento,
  lwd = 2,
  col = "darkgreen"
)

text(
  riesgo_rendimiento$Riesgo[1],
  riesgo_rendimiento$Rendimiento[1],
  labels = "META",
  pos = 2,      
  col = "blue"
)

text(
  riesgo_rendimiento$Riesgo[2],
  riesgo_rendimiento$Rendimiento[2],
  labels = "APPLE",
  pos = 4,     
  col = "red"
)

#Evaluación del rendimiento de inversión por cada unidad de riesgo
SharpeRatio(rends[,1])
SharpeRatio(rends[,2])


#Correlación entre activos
correlacion <- cor(
  rends[,1],
  rends[,2]
)

cat("Correlación:", correlacion, "\n")


#Se analiza la variación entre Meta y Apple en un periodo de 90 dias (90 observaciones)
chart.RollingCorrelation(
  rends[,1],
  rends[,2],
  width = 90,
  main = "Correlación META vs APPLE"
)

#Se crean 2 histogramas comparando los rendimientos diarios con distribución normal 
par(mfrow = c(1,2))

chart.Histogram(
  rends[,1],
  methods = c("add.normal","add.density"),
  colorset = c("gray","blue","red"),
  main = "META"
)

legend(
  "topright",
  c("Histograma","Densidad","Normal"),
  col = c("gray","blue","red"),
  lty = 1,
  cex = 0.7
)

chart.Histogram(
  rends[,2],
  methods = c("add.normal","add.density"),
  colorset = c("gray","blue","red"),
  main = "AAPL"
)

legend(
  "topright",
  c("Histograma","Densidad","Normal"),
  col = c("gray","blue","red"),
  lty = 1,
  cex = 0.7
)

#Mide el nivel de rendimientos: alto, bajo
covarianza <- cov(
  rends[,1],
  rends[,2]
)

pesos <- seq(0, 1, by = 0.01)

#Medición de rendimiento esperado (rend_port)
rend_port <- numeric(length(pesos))
#Riesgo de inversión (riesgo_port)
riesgo_port <- numeric(length(pesos))

#Cálculo del rendimiento y riesgo esperado
for(i in seq_along(pesos)){
  
  w <- pesos[i]
  
  rend_port[i] <-
    w * media_META +
    (1 - w) * media_AAPL
  
  riesgo_port[i] <-
    sqrt(
      (w^2) * (riesgo_META^2) +
        ((1-w)^2) * (riesgo_AAPL^2) +
        2 * w * (1-w) * covarianza
    )
}


#Elimina los valores inválidos
idx <- is.finite(riesgo_port) &
  is.finite(rend_port)

riesgo_port <- riesgo_port[idx]
rend_port <- rend_port[idx]


#Gráfica comparativa de los rendimientos esperados vs riesgo
plot(
  riesgo_port,
  rend_port,
  type = "l",
  lwd = 2,
  col = "darkgreen",
  xlab = "Riesgo",
  ylab = "Rendimiento esperado",
  main = "Combinaciones de Portafolio META-AAPL"
)

points(
  c(riesgo_META, riesgo_AAPL),
  c(media_META, media_AAPL),
  pch = 19,
  col = c("blue", "red"),
  cex = 1.5
)

text(
  riesgo_META,
  media_META,
  labels = "META",
  pos = 2,
  col = "blue",
  font = 2
)

text(
  riesgo_AAPL,
  media_AAPL,
  labels = "AAPL",
  pos = 4,
  col = "red",
  font = 2
)
