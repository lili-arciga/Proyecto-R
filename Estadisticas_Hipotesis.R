# Carguemos las librerías
library(plotly)
library(dplyr)

# Carguemos los datos
setwd("C:/BEDU/Estadisticas")

match <- read.csv("match.data.csv")

setwd("C:/BEDU/Estadisticas/datos")

#Para nuestra primera hipotesis vamos a extraer el numero de goles para locales y visitantes
local.gol <- match$home.score
visit.gol <- match$away.score

#Veamos si hay diferencia estadistica entre cada una de las distribuciones 
pvalue <- as.numeric(t.test(local.gol, visit.gol, alternative = 'greater')[3])

#Grafiquemos los datos
goles <- data.frame (Equipo  = c(rep("Local",3800),rep("Visitante",3800)),
                     Numero_goles = c(local.gol,visit.gol)
)

fig <- goles %>%
  plot_ly(
    x = ~Equipo,
    y = ~Numero_goles,
    split = ~Equipo,
    type = 'violin',
    box = list(
      visible = T
    ),
    meanline = list(
      visible = T
    )
  ) 

fig <- fig %>%
  layout(
    title= list(text =paste("Distribucion de goles por equipo \nT-test: ",pvalue)), 
    xaxis = list(
      title = "Rol del equipo"
    ),
    yaxis = list(
      title = "Número de goles",
      zeroline = F
    )
  )

fig
############################################################################################################################
#Veamos nuestra segunda hipotesis
#Los equipos al estar como locales ganan mas partido que como visitantes 
# Cuantas veces gano el local respecto al visitante


#Asignemos el nombre de las temporadas
t1 <- read.csv("SP1-1112.csv")
t2 <- read.csv("SP1-1213.csv")
t3 <- read.csv("SP1-1314.csv")
t4 <- read.csv("SP1-1415.csv")
t5 <- read.csv("SP1-1516.csv")
t6 <- read.csv("SP1-1617.csv")
t7 <- read.csv("SP1-1718.csv")
t8 <- read.csv("SP1-1819.csv")
t9 <- read.csv("SP1-1920.csv")

#Creemos una funcion  para que nos diga cuantos gano la casa, cuantos el visitante, y cuantos empate

fucho <- function(df){
  h <- length(which(df$FTR=="H"))
  a <- length(which(df$FTR=="A"))
  e <- length(which(df$FTR=="D"))
  return(c(h,a,e))
}

#Hagamos un ciclo para poder separar los datos para cada una de las temporadas
#declaremos las variables donde se van a almacenar el resultado de los partidos
home_sea.win <- c()
visi_sea.win <- c()
empatados <- c()

dfs <-list(t1,t2,t3,t4,t5,t6,t7,t8,t9)

#Para cada temporada dime cuantas gano local, cuantas gano visitante y cuantos empate
for (i in dfs){
  home_sea.win <- c(home_sea.win,fucho(i)[1])
  visi_sea.win <- c(visi_sea.win,fucho(i)[2])
  empatados <- c(empatados,fucho(i)[3])
}

#Creamos un df para poder graficar
resumen_temp <- data.frame (Resultado  = c(rep("Ganó Local",9),rep("Ganó Visit",9),rep("Empate",9)),
                            Numero_Partidos = c(home_sea.win,visi_sea.win,empatados)
)


#Grafiquemos nuestras hipotesis
fig1 <- resumen_temp %>%
  plot_ly(
    x = ~Resultado,
    y = ~Numero_Partidos,
    split = ~Resultado,
    type = 'violin',
    box = list(
      visible = T
    ),
    meanline = list(
      visible = T
    )
  ) 

fig1 <- fig1 %>%
  layout(
    title= list(text ="Resultados a lo largo de las temporadas"), 
    xaxis = list(
      title = "Resultado final de los partidos"
    ),
    yaxis = list(
      title = "Número de partidos",
      zeroline = F
    )
  )

fig1

#Si hacemos las t test de una cola podemos ver que hay significancia, 
#Incluso si hicieramos una prueba de multiples comparaciones de Bonferroni
paste("Local gano mas que visitante",as.numeric(t.test(local.gol, visit.gol, alternative = 'greater')[3]))
paste("Local gano mas que empató",as.numeric(t.test(home_sea.win, empatados, alternative = 'greater')[3]))


###################################################################################################################################
#Analicemos la tercera hipotesis  Los equipo visitantes cometen mas faltas

#Extraigamos el numero de faltas por partido que corresponden a visita y a local
faltas_locales <- c()
faltas_visita <- c()

#Para cada temporada dime cuantas faltas hizo en cada partico el local y el visitante
for (i in dfs){
  faltas_visita <- c(faltas_visita,i$AF)
  faltas_locales <- c(faltas_locales,i$HF)
}

#Creamos un df para poder graficar
faltas_df <- data.frame (Equipo  = c(rep("Local",3420),rep("Visitante",3420)),
                         Faltas = c(faltas_locales,faltas_visita)
)

pvalue2 <- as.numeric(t.test(faltas_locales,faltas_visita)[3])

#Grafiquemos nuestras hipotesis
fig2 <- faltas_df %>%
  plot_ly(
    x = ~Equipo,
    y = ~Faltas,
    split = ~Equipo,
    type = 'violin',
    box = list(
      visible = T
    ),
    meanline = list(
      visible = T
    )
  ) 

fig2 <- fig2 %>%
  layout(
    title= list(text = paste("Distribución de faltas por partido \nT-test:", round(pvalue2,4))), 
    xaxis = list(
      title = "Equipo"
    ),
    yaxis = list(
      title = "Número de faltas",
      zeroline = F
    )
  )

fig2

