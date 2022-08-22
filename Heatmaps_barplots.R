# Importamos las bibliotecas requeridas

library(lattice)
library(dplyr)
library(ggplot2)

# Fijamos el directorio de trabajo  donde tenemos nuestros datos

setwd('c:/BEDU/Proyecto/datos')
dir()

# Extraemos la información necesaria de los archivos

lista <-lapply(dir(), read.csv, fileEncoding ="UTF-8")
lista <- lapply(lista, mutate, Date = as.Date(Date, format = "%d/%m/%y")) 
lista <- lapply(lista, select, Date:FTAG)

T1011 <- lista[[1]]
T1112 <- lista[[2]]
T1213 <- lista[[3]]
T1314 <- lista[[4]]
T1415 <- lista[[5]]
T1516 <- lista[[6]]
T1617 <- lista[[7]]
T1718 <- lista[[8]]
T1819 <- lista[[9]]
T1920 <- lista[[10]]

# CREAMOS LA TABLA DE CONTINGENCIA PARA TEMPORADA
  
  # Primero creamos la tabla de probabilidades conjuntas

goles.local <- T1011$FTHG
goles.visitante <- T1011$FTAG 
tabla <- table(goles.local,goles.visitante)
total.goles <- sum(tabla)
tabla <- round(tabla/total.goles,5)

  # Agregamos las probabilidades marginales de ambas columnas

marginal.gl <- marginSums(tabla,1)

tabla <- cbind(tabla,marginal.gl)

marginal.gv <- marginSums(tabla,2)

tabla <- rbind(tabla,marginal.gv)

# GRAFICAMOS LAS PROBABILIDADES MARGINALES
  
  # Probabilidad de anotar del equipo de casa 

n = dim(tabla)[1] ; m = dim(tabla)[2]
goles <- row.names(tabla)[1:n-1]
prob <- marginal.gl
df<-as.data.frame(cbind(goles,prob))

  #Guardamos la imagen
jpeg(file="c:/BEDU/Estadisticas/www/Local10-11.jpeg", width = 550, height = 450)
ggplot(df,aes(row.names(df),marginal.gl))+geom_col(fill='darkgreen')+
  xlab('Goles') + ylab('Probabilidad de anotar')+
  ggtitle('Local') + 
  theme(plot.title = element_text(hjust = 0.5, size=20))
dev.off()
  
  # Probabilidad de anotar del equipo visitante

goles <- colnames(tabla)[1:m-1]
prob <- marginal.gv[1:m-1]
df <- as.data.frame(cbind(goles,prob))

  #Guardamos la imagen
jpeg(file="c:/BEDU/Estadisticas/www/Visitante10-11.jpeg", width = 550, height = 450)
ggplot(df)+geom_col(aes(goles,prob), fill='#cc9933')+
  xlab('Goles') + ylab('Probabilidad de anotar')+
  ggtitle('Visitante') + 
  theme(plot.title = element_text(hjust = 0.5, size=20))
dev.off()
  
  
# GRAFICAMOS EL MAPA DE CALOR

color <- colorRampPalette(c('black','limegreen'))
color <-color(20)

  #Guardamos la imagen
jpeg(file="c:/BEDU/Estadisticas/www/hm10-11.jpeg", width = 550, height = 450)
levelplot(tabla[1:n-1,1:m-1]*100,col.regions= color,
          main='Probabilidad del resultado final',
          xlab='Goles local', ylab='Goles visitante')
grid::grid.text('(%)', y=unit(0.92, "npc"), 
                x=unit(0.89, "npc"))
dev.off()


# SE REPITE EL PROCESO PARA CADA TEMPORADA


##################################################################

# CREAMOS LA TABLA DE CONTINGENCIA PARA TEMPORADA

  # Primero creamos la tabla de probabilidades conjuntas


goles.local <- T1112$FTHG
goles.visitante <- T1112$FTAG 
tabla <- table(goles.local,goles.visitante)
total.goles <- sum(tabla)
tabla <- round(tabla/total.goles,5)

  # Agregamos las probabilidades marginales de ambas variables

marginal.gl <- marginSums(tabla,1)

tabla <- cbind(tabla,marginal.gl)

marginal.gv <- marginSums(tabla,2)

tabla <- rbind(tabla,marginal.gv)

# GRAFICAMOS LAS PROBABILIDADES MARGINALES

  # Probabilidad de anotar del equipo de casa 

n = dim(tabla)[1] ; m = dim(tabla)[2]
goles <- row.names(tabla)[1:n-1]
prob <- marginal.gl
df<-as.data.frame(cbind(goles,prob))

jpeg(file="c:/BEDU/Estadisticas/www/Local11-12.jpeg",width = 550, height = 450)
ggplot(df,aes(goles,prob))+geom_col(fill='darkgreen')+
  xlab('Goles') + ylab('Probabilidad de anotar')+
  ggtitle('Local') + 
  theme(plot.title = element_text(hjust = 0.5, size=20))
dev.off()

  # Probabilidad de anotar del equipo visitante

goles <- colnames(tabla)[1:m-1]
prob <- marginal.gv[1:m-1]
df <- as.data.frame(cbind(goles,prob))

jpeg(file="c:/BEDU/Estadisticas/www/Visitante11-12.jpeg",width = 550, height = 450)
ggplot(df)+geom_col(aes(goles,prob), fill='#cc9933')+
  xlab('Goles') + ylab('Probabilidad de anotar')+
  ggtitle('Visitante') + 
  theme(plot.title = element_text(hjust = 0.5, size=20))
dev.off()


# GRAFICAMOS EL MAPA DE CALOR

color <- colorRampPalette(c('black','limegreen'))
color <-color(20)

jpeg(file="c:/BEDU/Estadisticas/www/hm11-12.jpeg",width = 550, height = 450)
levelplot(tabla[1:n-1,1:m-1]*100,col.regions= color,
          main='Probabilidad del resultado final',
          xlab='Goles local', ylab='Goles visitante')
grid::grid.text('(%)', y=unit(0.92, "npc"), 
                x=unit(0.89, "npc"))
dev.off()


###################################################################  

# CREAMOS LA TABLA DE CONTINGENCIA PARA TEMPORADA

  # Primero creamos la tabla de probabilidades conjuntas

goles.local <- T1213$FTHG
goles.visitante <- T1213$FTAG 
tabla <- table(goles.local,goles.visitante)
total.goles <- sum(tabla)
tabla <- round(tabla/total.goles,5)

  # Agregamos las probabilidades marginales de ambas variables

marginal.gl <- marginSums(tabla,1)

tabla <- cbind(tabla,marginal.gl)

marginal.gv <- marginSums(tabla,2)

tabla <- rbind(tabla,marginal.gv)

# GRAFICAMOS LAS PROBABILIDADES MARGINALES

  # Probabilidad de anotar del equipo de casa 

n = dim(tabla)[1] ; m = dim(tabla)[2]
goles <- row.names(tabla)[1:n-1]
prob <- marginal.gl
df<-as.data.frame(cbind(goles,prob))

jpeg(file="c:/BEDU/Estadisticas/www/Local12-13.jpeg",width = 550, height = 450)
ggplot(df,aes(goles,prob))+geom_col(fill='darkgreen')+
  xlab('Goles') + ylab('Probabilidad de anotar')+
  ggtitle('Local') + 
  theme(plot.title = element_text(hjust = 0.5, size=20))
dev.off()
 
   # Probabilidad de anotar del equipo visitante

goles <- colnames(tabla)[1:m-1]
prob <- marginal.gv[1:m-1]
df <- as.data.frame(cbind(goles,prob))

jpeg(file="c:/BEDU/Estadisticas/www/Visitante12-13.jpeg",width = 550, height = 450)
ggplot(df)+geom_col(aes(goles,prob), fill='#cc9933')+
  xlab('Goles') + ylab('Probabilidad de anotar')+
  ggtitle('Visitante') + 
  theme(plot.title = element_text(hjust = 0.5, size=20))
dev.off()


# GRAFICAMOS EL MAPA DE CALOR

color <- colorRampPalette(c('black','limegreen'))
color <-color(20)

jpeg(file="c:/BEDU/Estadisticas/www/hm12-13.jpeg",width = 550, height = 450)
levelplot(tabla[1:n-1,1:m-1]*100,col.regions= color,
          main='Probabilidad del resultado final',
          xlab='Goles local', ylab='Goles visitante')
grid::grid.text('(%)', y=unit(0.92, "npc"), 
                x=unit(0.90, "npc"))
dev.off()
#############################################################
  
# CREAMOS LA TABLA DE CONTINGENCIA PARA TEMPORADA

  # Primero creamos la tabla de probabilidades conjuntas

goles.local <- T1314$FTHG
goles.visitante <- T1314$FTAG 
tabla <- table(goles.local,goles.visitante)
total.goles <- sum(tabla)
tabla <- round(tabla/total.goles,5)

  # Agregamos las probabilidades marginales de ambas variables

marginal.gl <- marginSums(tabla,1)

tabla <- cbind(tabla,marginal.gl)

marginal.gv <- marginSums(tabla,2)

tabla <- rbind(tabla,marginal.gv)

# GRAFICAMOS LAS PROBABILIDADES MARGINALES

  # Probabilidad de anotar del equipo de casa 

n = dim(tabla)[1] ; m = dim(tabla)[2]
goles <- row.names(tabla)[1:n-1]
prob <- marginal.gl
df<-as.data.frame(cbind(goles,prob))

jpeg(file="c:/BEDU/Estadisticas/www/Local13-14.jpeg",width = 550, height = 450)
ggplot(df,aes(goles,prob))+geom_col(fill='darkgreen')+
  xlab('Goles') + ylab('Probabilidad de anotar')+
  ggtitle('Local') + 
  theme(plot.title = element_text(hjust = 0.5, size=20))
dev.off()
 
   # Probabilidad de anotar del equipo visitante

goles <- colnames(tabla)[1:m-1]
prob <- marginal.gv[1:m-1]
df <- as.data.frame(cbind(goles,prob))

jpeg(file="c:/BEDU/Estadisticas/www/Visitante13-14.jpeg",width = 550, height = 450)
ggplot(df)+geom_col(aes(goles,prob), fill='#cc9933')+
  xlab('Goles') + ylab('Probabilidad de anotar')+
  ggtitle('Visitante') + 
  theme(plot.title = element_text(hjust = 0.5, size=20))
dev.off()


# GRAFICAMOS EL MAPA DE CALOR

color <- colorRampPalette(c('black','limegreen'))
color <-color(20)

jpeg(file="c:/BEDU/Estadisticas/www/hm13-14.jpeg",width = 550, height = 450)
levelplot(tabla[1:n-1,1:m-1]*100,col.regions= color,
          main='Probabilidad del resultado final',
          xlab='Goles local', ylab='Goles visitante')
grid::grid.text('(%)', y=unit(0.93, "npc"), 
                x=unit(0.94, "npc"))
dev.off()
#############################################################

# CREAMOS LA TABLA DE CONTINGENCIA PARA TEMPORADA

  # Primero creamos la tabla de probabilidades conjuntas

goles.local <- T1415$FTHG
goles.visitante <- T1415$FTAG 
tabla <- table(goles.local,goles.visitante)
total.goles <- sum(tabla)
tabla <- round(tabla/total.goles,5)

  # Agregamos las probabilidades marginales de ambas variables

marginal.gl <- marginSums(tabla,1)

tabla <- cbind(tabla,marginal.gl)

marginal.gv <- marginSums(tabla,2)

tabla <- rbind(tabla,marginal.gv)

# GRAFICAMOS LAS PROBABILIDADES MARGINALES

  # Probabilidad de anotar del equipo de casa 

n = dim(tabla)[1] ; m = dim(tabla)[2]
goles <- row.names(tabla)[1:n-1]
prob <- marginal.gl
df<-as.data.frame(cbind(goles,prob))

jpeg(file="c:/BEDU/Estadisticas/www/Local14-15.jpeg",width = 550, height = 450)
ggplot(df,aes(goles,prob))+geom_col(fill='darkgreen')+
  xlab('Goles') + ylab('Probabilidad de anotar')+
  ggtitle('Local') + 
  theme(plot.title = element_text(hjust = 0.5, size=20))
dev.off()
  
  # Probabilidad de anotar del equipo visitante

goles <- colnames(tabla)[1:m-1]
prob <- marginal.gv[1:m-1]
df <- as.data.frame(cbind(goles,prob))

jpeg(file="c:/BEDU/Estadisticas/www/Visitante14-15.jpeg",width = 550, height = 450)
ggplot(df)+geom_col(aes(goles,prob), fill='#cc9933')+
  xlab('Goles') + ylab('Probabilidad de anotar')+
  ggtitle('Visitante') + 
  theme(plot.title = element_text(hjust = 0.5, size=20))
dev.off()


# GRAFICAMOS EL MAPA DE CALOR

color <- colorRampPalette(c('black','limegreen'))
color <-color(20)

jpeg(file="c:/BEDU/Estadisticas/www/hm14-15.jpeg",width = 550, height = 450)
levelplot(tabla[1:n-1,1:m-1]*100,col.regions= color,
          main='Probabilidad del resultado final',
          xlab='Goles local', ylab='Goles visitante')
grid::grid.text('(%)', y=unit(0.89, "npc"), 
                x=unit(0.94, "npc"))
dev.off()
#############################################################

# CREAMOS LA TABLA DE CONTINGENCIA PARA TEMPORADA

  # Primero creamos la tabla de probabilidades conjuntas

goles.local <- T1516$FTHG
goles.visitante <- T1516$FTAG 
tabla <- table(goles.local,goles.visitante)
total.goles <- sum(tabla)
tabla <- round(tabla/total.goles,5)

  # Agregamos las probabilidades marginales de ambas variables

marginal.gl <- marginSums(tabla,1)

tabla <- cbind(tabla,marginal.gl)

marginal.gv <- marginSums(tabla,2)

tabla <- rbind(tabla,marginal.gv)

# GRAFICAMOS LAS PROBABILIDADES MARGINALES

  # Probabilidad de anotar del equipo de casa 

n = dim(tabla)[1] ; m = dim(tabla)[2]
goles <- row.names(tabla)[1:n-1]
prob <- marginal.gl
df<-as.data.frame(cbind(goles,prob))

jpeg(file="c:/BEDU/Estadisticas/www/Local15-16.jpeg",width = 550, height = 450)
ggplot(df,aes(goles,prob))+geom_col(fill='darkgreen')+
  xlab('Goles') + ylab('Probabilidad de anotar')+
  ggtitle('Local') + 
  theme(plot.title = element_text(hjust = 0.5, size=20))
dev.off()
 
   # Probabilidad de anotar del equipo visitante

goles <- colnames(tabla)[1:m-1]
prob <- marginal.gv[1:m-1]
df <- as.data.frame(cbind(goles,prob))

jpeg(file="c:/BEDU/Estadisticas/www/Visitante15-16.jpeg",width = 550, height = 450)
ggplot(df)+geom_col(aes(goles,prob), fill='#cc9933')+
  xlab('Goles') + ylab('Probabilidad de anotar')+
  ggtitle('Visitante') + 
  theme(plot.title = element_text(hjust = 0.5, size=20))
dev.off()


# GRAFICAMOS EL MAPA DE CALOR

color <- colorRampPalette(c('black','limegreen'))
color <-color(20)

jpeg(file="c:/BEDU/Estadisticas/www/hm15-16.jpeg",width = 550, height = 450)
levelplot(tabla[1:n-1,1:m-1]*100,col.regions= color,
          main='Probabilidad del resultado final',
          xlab='Goles local', ylab='Goles visitante')
grid::grid.text('(%)', y=unit(0.90, "npc"), 
                x=unit(0.94, "npc"))
dev.off()
#############################################################

# CREAMOS LA TABLA DE CONTINGENCIA PARA TEMPORADA

  # Primero creamos la tabla de probabilidades conjuntas

goles.local <- T1617$FTHG
goles.visitante <- T1617$FTAG 
tabla <- table(goles.local,goles.visitante)
total.goles <- sum(tabla)
tabla <- round(tabla/total.goles,5)

  # Agregamos las probabilidades marginales de ambas variables

marginal.gl <- marginSums(tabla,1)

tabla <- cbind(tabla,marginal.gl)

marginal.gv <- marginSums(tabla,2)

tabla <- rbind(tabla,marginal.gv)

# GRAFICAMOS LAS PROBABILIDADES MARGINALES

  # Probabilidad de anotar del equipo de casa 

n = dim(tabla)[1] ; m = dim(tabla)[2]
goles <- row.names(tabla)[1:n-1]
prob <- marginal.gl
df<-as.data.frame(cbind(goles,prob))

jpeg(file="c:/BEDU/Estadisticas/www/Local16-17.jpeg",width = 550, height = 450)
ggplot(df,aes(goles,prob))+geom_col(fill='darkgreen')+
  xlab('Goles') + ylab('Probabilidad de anotar')+
  ggtitle('Local') + 
  theme(plot.title = element_text(hjust = 0.5, size=20))
dev.off()
  
  # Probabilidad de anotar del equipo visitante

goles <- colnames(tabla)[1:m-1]
prob <- marginal.gv[1:m-1]
df <- as.data.frame(cbind(goles,prob))

jpeg(file="c:/BEDU/Estadisticas/www/Visitante16-17.jpeg",width = 550, height = 450)
ggplot(df)+geom_col(aes(goles,prob), fill='#cc9933')+
  xlab('Goles') + ylab('Probabilidad de anotar')+
  ggtitle('Visitante') + 
  theme(plot.title = element_text(hjust = 0.5, size=20))
dev.off()


# GRAFICAMOS EL MAPA DE CALOR

color <- colorRampPalette(c('black','limegreen'))
color <-color(20)

jpeg(file="c:/BEDU/Estadisticas/www/hm16-17.jpeg",width = 550, height = 450)
levelplot(tabla[1:n-1,1:m-1]*100,col.regions= color,
          main='Probabilidad del resultado final',
          xlab='Goles local', ylab='Goles visitante')
grid::grid.text('(%)', y=unit(0.92, "npc"), 
                x=unit(0.89, "npc"))
dev.off()
#############################################################

# CREAMOS LA TABLA DE CONTINGENCIA PARA TEMPORADA

  # Primero creamos la tabla de probabilidades conjuntas

goles.local <- T1718$FTHG
goles.visitante <- T1718$FTAG 
tabla <- table(goles.local,goles.visitante)
total.goles <- sum(tabla)
tabla <- round(tabla/total.goles,5)

  # Agregamos las probabilidades marginales de ambas variables

marginal.gl <- marginSums(tabla,1)

tabla <- cbind(tabla,marginal.gl)

marginal.gv <- marginSums(tabla,2)

tabla <- rbind(tabla,marginal.gv)

# GRAFICAMOS LAS PROBABILIDADES MARGINALES

  # Probabilidad de anotar del equipo de casa 

n = dim(tabla)[1] ; m = dim(tabla)[2]
goles <- row.names(tabla)[1:n-1]
prob <- marginal.gl
df<-as.data.frame(cbind(goles,prob))

jpeg(file="c:/BEDU/Estadisticas/www/Local17-18.jpeg",width = 550, height = 450)
ggplot(df,aes(goles,prob))+geom_col(fill='darkgreen')+
  xlab('Goles') + ylab('Probabilidad de anotar')+
  ggtitle('Local') + 
  theme(plot.title = element_text(hjust = 0.5, size=20))
dev.off()
  
  # Probabilidad de anotar del equipo visitante

goles <- colnames(tabla)[1:m-1]
prob <- marginal.gv[1:m-1]
df <- as.data.frame(cbind(goles,prob))

jpeg(file="c:/BEDU/Estadisticas/www/Visitante17-18.jpeg",width = 550, height = 450)
ggplot(df)+geom_col(aes(goles,prob), fill='#cc9933')+
  xlab('Goles') + ylab('Probabilidad de anotar')+
  ggtitle('Visitante') + 
  theme(plot.title = element_text(hjust = 0.5, size=20))
dev.off()


# GRAFICAMOS EL MAPA DE CALOR

color <- colorRampPalette(c('black','limegreen'))
color <-color(20)

jpeg(file="c:/BEDU/Estadisticas/www/hm17-18.jpeg",width = 550, height = 450)
levelplot(tabla[1:n-1,1:m-1]*100,col.regions= color,
          main='Probabilidad del resultado final',
          xlab='Goles local', ylab='Goles visitante')
grid::grid.text('(%)', y=unit(0.92, "npc"), 
                x=unit(0.89, "npc"))
dev.off()
#############################################################

# CREAMOS LA TABLA DE CONTINGENCIA PARA TEMPORADA

  # Primero creamos la tabla de probabilidades conjuntas

goles.local <- T1819$FTHG
goles.visitante <- T1819$FTAG 
tabla <- table(goles.local,goles.visitante)
total.goles <- sum(tabla)
tabla <- round(tabla/total.goles,5)

  # Agregamos las probabilidades marginales de ambas variables

marginal.gl <- marginSums(tabla,1)

tabla <- cbind(tabla,marginal.gl)

marginal.gv <- marginSums(tabla,2)

tabla <- rbind(tabla,marginal.gv)

# GRAFICAMOS LAS PROBABILIDADES MARGINALES

  # Probabilidad de anotar del equipo de casa 

n = dim(tabla)[1] ; m = dim(tabla)[2]
goles <- row.names(tabla)[1:n-1]
prob <- marginal.gl
df<-as.data.frame(cbind(goles,prob))

jpeg(file="c:/BEDU/Estadisticas/www/Local18-19.jpeg",width = 550, height = 450)
ggplot(df,aes(goles,prob))+geom_col(fill='darkgreen')+
  xlab('Goles') + ylab('Probabilidad de anotar')+
  ggtitle('Local') + 
  theme(plot.title = element_text(hjust = 0.5, size=20))
dev.off()
  
  # Probabilidad de anotar del equipo visitante

goles <- colnames(tabla)[1:m-1]
prob <- marginal.gv[1:m-1]
df <- as.data.frame(cbind(goles,prob))

jpeg(file="c:/BEDU/Estadisticas/www/Visitante18-19.jpeg",width = 550, height = 450)
ggplot(df)+geom_col(aes(goles,prob), fill='#cc9933')+
  xlab('Goles') + ylab('Probabilidad de anotar')+
  ggtitle('Visitante') + 
  theme(plot.title = element_text(hjust = 0.5, size=20))
dev.off()


# GRAFICAMOS EL MAPA DE CALOR

color <- colorRampPalette(c('black','limegreen'))
color <-color(20)

jpeg(file="c:/BEDU/Estadisticas/www/hm18-19.jpeg",width = 550, height = 450)
levelplot(tabla[1:n-1,1:m-1]*100,col.regions= color,
          main='Probabilidad del resultado final',
          xlab='Goles local', ylab='Goles visitante')
grid::grid.text('(%)', y=unit(0.93, "npc"), 
                x=unit(0.86, "npc"))
dev.off()
#############################################################

# CREAMOS LA TABLA DE CONTINGENCIA PARA TEMPORADA

  # Primero creamos la tabla de probabilidades conjuntas

goles.local <- T1920$FTHG
goles.visitante <- T1920$FTAG 
tabla <- table(goles.local,goles.visitante)
total.goles <- sum(tabla)
tabla <- round(tabla/total.goles,5)

  # Agregamos las probabilidades marginales de ambas variables

marginal.gl <- marginSums(tabla,1)

tabla <- cbind(tabla,marginal.gl)

marginal.gv <- marginSums(tabla,2)

tabla <- rbind(tabla,marginal.gv)

# GRAFICAMOS LAS PROBABILIDADES MARGINALES

  # Probabilidad de anotar del equipo de casa 

n = dim(tabla)[1] ; m = dim(tabla)[2]
goles <- row.names(tabla)[1:n-1]
prob <- marginal.gl
df<-as.data.frame(cbind(goles,prob))

jpeg(file="c:/BEDU/Estadisticas/www/Local19-20.jpeg",width = 550, height = 450)
ggplot(df,aes(goles,prob))+geom_col(fill='darkgreen')+
  xlab('Goles') + ylab('Probabilidad de anotar')+
  ggtitle('Local') + 
  theme(plot.title = element_text(hjust = 0.5, size=20))
dev.off()
  
  # Probabilidad de anotar del equipo visitante

goles <- colnames(tabla)[1:m-1]
prob <- marginal.gv[1:m-1]
df <- as.data.frame(cbind(goles,prob))

jpeg(file="c:/BEDU/Estadisticas/www/Visitante19-20.jpeg",width = 550, height = 450)
ggplot(df)+geom_col(aes(goles,prob), fill='#cc9933')+
  xlab('Goles') + ylab('Probabilidad de anotar')+
  ggtitle('Visitante') + 
  theme(plot.title = element_text(hjust = 0.5, size=20))
dev.off()


# GRAFICAMOS EL MAPA DE CALOR

color <- colorRampPalette(c('black','limegreen'))
color <-color(20)

jpeg(file="c:/BEDU/Estadisticas/www/hm19-20.jpeg",width = 550, height = 450)
levelplot(tabla[1:n-1,1:m-1]*100,col.regions= color,
          main='Probabilidad del resultado final',
          xlab='Goles local', ylab='Goles visitante')
grid::grid.text('(%)', y=unit(0.92, "npc"), 
                x=unit(0.90, "npc"))
dev.off()
#############################################################

