# Proyecto-R
Proyecto del equipo 6

### Integrantes
- Liliana Inés Árciga Moreno
- Alfredo Cuevas Sánchez
- Jorge Arista
- José María Montes Montiel
- Rodrigo Garmendia


#### Iniciamos cargando las librerías necesarias para crear el dashboard
    library(shinythemes)
    library(shiny)
    library(shinydashboard)
    library(fontawesome)

#### Seleccionamis el directorio de trabajo en donde se almacenan los datos del archivo match.data.cvs
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
                      p("Como equipo atribuimos que el hecho de que el equipo que juega como local tenga aproximadamente 1.6 veces mas chances de ganar, 
                        se puede atribuir a factores relacionados a la porra y número de seguidores que puede tener un equipo al jugar dentro de su propio estadio.
                        Claro que para afirmar tal aseveración necesitaríamos analizar el número de aficionados por equipo en cada partido."
                        ),
                      br(),

                      p("Por último quisimos analizar que rol cometío mas faltas a lo largo de todos los partidos de las 9 temporadas analizadas. El grafico que se muestra
                        a continuación mas la prueba estadistica t-student de dos colas comprueban que no hay diferencias significativas entre el número de faltas cometidas
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
