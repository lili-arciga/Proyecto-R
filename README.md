# Proyecto-R
Proyecto del equipo 6

### Integrantes
- Liliana Inés Árciga Moreno
- Alfredo Cuevas Sánchez
- Jorge Arista
- José María Montes Montiel
- Rodrigo Garmendia Issa

## Elaboración de los heatmaps



## Planteamiento de las hipótesis

##### Carguemos las librerías
        library(plotly)
        library(dplyr)
        setwd("~/Desktop/Proyecto/datos")

##### Carguemos los datos
        match <- read.csv("match.data.csv")

##### Para nuestra primera hipótesis vamos a extraer el número de goles para locales y visitantes
        local.gol <- match$home.score
        visit.gol <- match$away.score

##### Veamos si hay diferencia estadística entre cada una de las distribuciones 
        pvalue <- as.numeric(t.test(local.gol, visit.gol, alternative = 'greater')[3])
        pvalue

##### Grafiquemos los datos
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

#### Veamos nuestra segunda hipótesis: Los equipos al estar como locales ganan mas partido que como visitantes, ¿cuántas veces ganó el local respecto al visitante?


##### Asignemos el nombre de las temporadas
        t1 <- read.csv("SP1-1112.csv")
        t2 <- read.csv("SP1-1213.csv")
        t3 <- read.csv("SP1-1314.csv")
        t4 <- read.csv("SP1-1415.csv")
        t5 <- read.csv("SP1-1516.csv")
        t6 <- read.csv("SP1-1617.csv")
        t7 <- read.csv("SP1-1718.csv")
        t8 <- read.csv("SP1-1819.csv")
        t9 <- read.csv("SP1-1920.csv")

##### Creemos una función para que nos diga cuantos gano la casa, cuantos el visitante, y cuantos empate

        fucho <- function(df){
          h <- length(which(df$FTR=="H"))
          a <- length(which(df$FTR=="A"))
          e <- length(which(df$FTR=="D"))
          return(c(h,a,e))
        }

##### Hagamos un ciclo para poder separar los datos para cada una de las temporadas, declaremos las variables donde se van a almacenar el resultado de los partidos
        home_sea.win <- c()
        visi_sea.win <- c()
        empatados <- c()

        dfs <-list(t1,t2,t3,t4,t5,t6,t7,t8,t9)

##### Para cada temporada cuantas gano local, cuantas gano visitante y cuantos empate.
        for (i in dfs){
          home_sea.win <- c(home_sea.win,fucho(i)[1])
          visi_sea.win <- c(visi_sea.win,fucho(i)[2])
          empatados <- c(empatados,fucho(i)[3])
        }

##### Creamos un df para poder graficar
        resumen_temp <- data.frame (Resultado  = c(rep("Ganó Local",9),rep("Ganó Visit",9),rep("Empate",9)),
                                    Numero_Partidos = c(home_sea.win,visi_sea.win,empatados)
        )

##### Grafiquemos nuestras hipótesis
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

Si hacemos las t test de una cola podemos ver que hay significancia, incluso si hicieramos una prueba de multiples comparaciones de Bonferroni:
        
        paste("Local gano mas que visitante",as.numeric(t.test(local.gol, visit.gol, alternative = 'greater')[3]))
        paste("Local gano mas que empató",as.numeric(t.test(home_sea.win, empatados, alternative = 'greater')[3]))

#### Analicemos la tercera hipótesis:  Los equipo visitantes cometen más faltas

##### Extraigamos el número de faltas por partido que corresponden a visita y a local
        faltas_locales <- c()
        faltas_visita <- c()

##### Para cada temporada dime cuantas faltas hizo en cada partico el local y el visitante
        for (i in dfs){
          faltas_visita <- c(faltas_visita,i$AF)
          faltas_locales <- c(faltas_locales,i$HF)
        }

##### Creamos un df para poder graficar
        faltas_df <- data.frame (Equipo  = c(rep("Local",3420),rep("Visitante",3420)),
                                 Faltas = c(faltas_locales,faltas_visita)
        )

        pvalue2 <- as.numeric(t.test(faltas_locales,faltas_visita)[3])

##### Grafiquemos nuestras hipótesis
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


## Elaboración del dashboard

#### Iniciamos cargando las librerías necesarias para crear el dashboard
    library(shinythemes)
    library(shiny)
    library(shinydashboard)
    library(fontawesome)

#### Seleccionamos el directorio de trabajo en donde se almacenan los datos del archivo match.data.cvs
    setwd("~/Desktop/Proyecto/datos")
    getwd()
    match.data <- read.csv("match.data.csv")

#### Creamos el ambiente del ui para el dashboard

    temps <- c("10-11","11-12","12-13","13-14","14-15","15-16","16-17","17-18","18-19","19-20")

    ui <- 

    fluidPage(

      dashboardPage(

        skin = c("green"),

        dashboardHeader(title = "Dashboard Momios"),

        dashboardSidebar(

          sidebarUserPanel("¡Bienvenido!", 
                           subtitle = a(href = "#", icon("futbol fa-spin", verify_fa = FALSE), "Seleccione:"),
                           image = ("200w.gif")) ,        
          sidebarMenu(
            menuItem("Histograma", tabName = "Dashboard", icon = icon("chart-column")),
            menuItem("Mapas y Graficas", tabName = "graphs", icon = icon("border-all","regular")),
            menuItem("Tabla de Datos", tabName = "data_table", icon = icon("th", verify_fa = FALSE)),
            menuItem("Imágenes", tabName = "img", icon = icon("file-photo-o", verify_fa = FALSE) ),
            menuItem("Hipótesis", tabName = "hipotesis", icon = icon("lightbulb-o", verify_fa = FALSE) )          

          )
        ),

        dashboardBody(

          tabItems(
          
          
##### 1. Histograma
          
            tabItem(tabName = "Dashboard", 
                    fluidRow(
                      titlePanel(h3("Histograma de las variables del data set match.data")), 
                      selectInput("xteam", label = tags$p(fa("futbol", fill = "green"),
                                                          "Seleccione el valor de X:"),
                                  choices = c("home.score", "away.score"),
                                  width = 200),

                      box(plotOutput("plot1", height = 600, width = 600), width = "12"),

                    )
            ),

          
##### 2. Gráficas
          
            tabItem(tabName = "graphs", 
                    fluidRow(
                      titlePanel(h3("Mapas y Gráficas")),                    
                      box(
                        titlePanel(h4("Mapas de calor por temporada")),
                        selectInput("hm", label = tags$p(fa("futbol", fill = "green"), 
                                                       "Selecciona la temporada"),
                                    choices = temps),
                        imageOutput("heatmap"),

                        br(), 
                        titlePanel(h4("Gráficas de Probabilidades Marginales")),

                        selectInput("br", label = tags$p(fa("futbol", fill = "green"),
                                                       "Selecciona equipo"),                                  
                                  choices = c('Local','Visitante')),
                        imageOutput("barplot"), 

                        width = 12
                        ) 

                    )
            ),
          
              
##### 3. Data Table          
          
            tabItem(tabName = "data_table",
                    fluidRow(        
                      titlePanel(h3("Tabla de Datos")),
                      h5(tags$p(fa("futbol", fill = "green"), 
                                                 "Tabla Base", bold = T),                                
                                  choices = temps),                    
                      dataTableOutput ("data_table")
                    )
            ), 

          
##### 4. Imágenes          
          
            tabItem(tabName = "img",
                    fluidRow(
                      titlePanel(h3("Imágenes de Secuencia de Juegos (Momios)")),

                      box(titlePanel(h4(icon("futbol", verify_fa = FALSE, fill = "green"), "Momios Máximos")),
                        img( src = "MomiosMáximos.png", 
                             height = 350, width = 580), width = 12),
                      box(titlePanel(h4(icon("futbol", verify_fa = FALSE), "Momios Promedio")),
                        img( src = "MomiosPromedio.png",
                             height = 350, width = 580), width = 12)  
                    )
            ),

                  
##### 5. Hipótesis          
          
            tabItem(tabName = "hipotesis",
                    fluidRow(
                      titlePanel(h3("Plateamiento de hipótesis"))),
                    box(
                      p("El equipo se centró en analizar algunos de los efectos que son consecuencia de jugar como local o visitante.
                        En primera instancia se analizó el número de goles. Mediante una prueba t de Welch de una sola cola, se evaluó
                        con el archivo match.csv las siguientes hipótesis: "),
                        strong("Ho: u1 <= u2 y Ha: u1 > u2"),
                      br(), br(),

                      #Foto1
                      img( src = "hipotesis1.png", 
                           height = 350, width = 550),
                      br(), br(),
                      p("Mediante el valor de probabilidad o pvalue que es menor a 0.05 podemos afirmar que el equipo local acierta mayor 
                        cantidad de goles que el equipo visitante. No obstante, quisimos confirmar que ese aumento en el número de goles 
                        se viera reflejado totalmente en victorias. Para lograr este objetivo mediante los datos de cada una de las temporadas
                        extrajimos el número de partidos empatados, ganados y perdidos por los locales hubo."
                        ),
                      br(),
                      #Foto2
                      img( src = "hipotesis2.png", 
                           height = 350, width = 550),
                      br(), br(),
                      p("Visualmente es muy fácil concluir que los equipos locales tienen mayor probabilidad de ganar que cuando se juega como visitante
                        Realizamos 2 pruebas de t-student de una cola para confirmar lo ya antes concluido, cabe mencionar que los valores de p eran 
                        tan pequeños que ya no se realizo la prueba de múltiples comparaciones. Incluso con un test de Bonferroni seguiría siendo significativo"
                        ),
                      br(),
                      p("Como equipo atribuimos que el hecho de que el equipo que juega como local tenga aproximadamente 1.6 veces más oportunidades de ganar, 
                        se puede atribuir a factores relacionados a la porra y número de seguidores que puede tener un equipo al jugar dentro de su propio estadio.
                        Claro que para afirmar tal aseveración necesitaríamos analizar el número de aficionados por equipo en cada partido."
                        ),
                      br(),

                      p("Por último quisimos analizar qué rol cometío más faltas a lo largo de todos los partidos de las 9 temporadas analizadas. El gráfico que se muestra
                        a continuación más la prueba estadistica t-student de dos colas comprueban que no hay diferencias significativas entre el número de faltas cometidas
                        y el rol que fungen los equipos"
                        ),
                      br(),
                      #Foto3
                      img( src = "hipotesis3.png", 
                           height = 350, width = 550)

                      , width = 12),
                    fluidRow(" ")


            )


          )
        )
      )
    )


#### De aquí en adelante es la parte que corresponde al server

    server <- function(input, output, session) {
    library(ggplot2)
    match.data <- read.csv("match.data.csv")
  
  
  ##### 1. Gráfico de Histograma
  
    output$plot1 <- renderPlot({
      ggplot(match.data, aes(x = as.factor(match.data[,input$xteam]))) + 
        geom_bar(color="white", fill = "#00A65A") +
        facet_wrap(as.factor(match.data$away.team)) +
        xlab("Goles")+ 
        ylab("Frecuencia")
      }
      )

    
  ##### 2. Mapas de calor
  
    output$heatmap <- renderImage({ 

      fileName <- normalizePath(file.path('./www',
                                          paste('hm',input$hm,'.jpeg', sep='')))
      list(
        src = fileName,
        contentType = "image/jpeg",
        width = 550,
        height = 440
      )
    }, deleteFile = FALSE)      

    output$barplot <- renderImage({ 

      fileName <- normalizePath(file.path('./www',
                                          paste(input$br,input$hm,'.jpeg', sep='')))
      list(
        src = fileName,
        contentType = "image/jpeg",
        width = 550,
        height = 440
      )
    }, deleteFile = FALSE)

  
  ##### 3. Data Table
  
    output$data_table <- renderDataTable(

      {match.data},
      options = list(lengthMenu = c(10,25,50), pageLength = 10)
                                          )

    }


    shinyApp(ui, server)

##### El enlace para acceder al dashboard se encuentra en el siguiente link:
