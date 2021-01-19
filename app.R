#------------------ Packages ----------------------------

library(dplyr)
library(shiny)
library(ggplot2)
library(corrplot)
library(Momocs)
library(shinydashboard)
library(scales)

#------------------ Preparing the data ------------------

Songs <- read.csv("https://raw.githubusercontent.com/NomadThisTime/Songs/main/genres_v2.csv")

# getting rid of unnecessary data (missing values or redundant)
Songs$type <- NULL
Songs$id <- NULL
Songs$uri <- NULL
Songs$track_href <- NULL
Songs$analysis_url <- NULL
Songs$time_signature <- NULL
Songs$Unnamed..0 <- NULL
Songs$title. <- NULL
Songs$mode <- NULL

# danceability is a chr. column filled with numbers
Songs$danceability <- as.numeric(Songs$danceability)

# removing incorrectly coded rows
Songs <- Songs[!(is.na(Songs$energy) | Songs$energy==""), ]

Count <- count(Songs, genre)
Count <- transform(Count, n2 = n / 41056)
Count$n2 <- percent(Count$n2, accuracy = 0.01)

#------------------ Analysis of the data ------------------

header <- dashboardHeader(title = "Songs of Spotify")

sidebar <- dashboardSidebar(sidebarMenu(
  menuItem("Genre Distribution", tabName = "genre", icon = icon("music")),
  menuItem("Song Features", tabName = "feat", icon = icon("tasks")),
  menuItem("Features by Genre", tabName = "fege", icon = icon("th")),
  menuItem("Features Correlation", tabName = "cor", icon = icon("random"))
))

body <- dashboardBody(
  tabItems(
    # First tab content
    tabItem(tabName = "genre",
            fluidRow(
              box(width = 12, plotOutput("plot1", height = 500)),
            )),
    # Second tab content
    tabItem(tabName = "feat",
            fluidRow(
              box(width = 2,
                  selectInput("dist", "Feature:",
                              c("Acousticness" = "aco",
                                "Danceability" = "dan",
                                "Duration in ms" = "drt",
                                "Energy" = "ene",
                                "Instrumentalness" = "ins",
                                "Key" = "key",
                                "Liveness" = "liv",
                                "Loudness" = "lou",
                                "Speechiness" = "spe",
                                "Tempo" = "tmp",
                                "Valence" = "val"))),
              box(width = 3, sliderInput("bins",
                                         "Number of bins (histogram):",
                                         min = 1,
                                         max = 100,
                                         value = 50)),
              box(width = 1,
                  checkboxInput('g1', 'Histogram', value=T),
                  checkboxInput('g2', 'Boxchart', value=F),
                  checkboxInput('g3', 'Q-Q Plot', value=F)),
              box(width = 5, title = 'Q_Q Plot', solidHeader = TRUE, status = 'info', plotOutput("plot4", height = 300)),
              box(width = 12, title = 'Histogram', solidHeader = TRUE, status = 'info', plotOutput("plot2", height = 300)),
              box(width = 12, title = 'Boxchart', solidHeader = TRUE, status = 'info', plotOutput("plot3", height = 100)),
            )),
    # Third tab content
    tabItem(tabName = "fege",
            fluidRow(
              box(width = 2,
                  selectInput("dist2", "Feature:",
                              c("Acousticness" = "aco",
                                "Danceability" = "dan",
                                "Duration in ms" = "drt",
                                "Energy" = "ene",
                                "Instrumentalness" = "ins",
                                "Key" = "key",
                                "Liveness" = "liv",
                                "Loudness" = "lou",
                                "Speechiness" = "spe",
                                "Tempo" = "tmp",
                                "Valence" = "val"))),
              box(width = 12, title = 'Boxplots', solidHeader = TRUE, status = 'info', plotOutput("plot5", height = 500))
            )),
    # Fourth tab content
    tabItem(tabName = "cor",
            fluidRow(
              box(width = 2,
                  selectInput("f1", "Feature_1(x):",
                              c("Acousticness" = "aco",
                                "Danceability" = "dan",
                                "Duration in ms" = "drt",
                                "Energy" = "ene",
                                "Instrumentalness" = "ins",
                                "Key" = "key",
                                "Liveness" = "liv",
                                "Loudness" = "lou",
                                "Speechiness" = "spe",
                                "Tempo" = "tmp",
                                "Valence" = "val")),
                  selectInput("f2", "Feature_2(y):",
                              c("Acousticness" = "aco2",
                                "Danceability" = "dan2",
                                "Duration in ms" = "drt2",
                                "Energy" = "ene2",
                                "Instrumentalness" = "ins2",
                                "Key" = "key2",
                                "Liveness" = "liv2",
                                "Loudness" = "lou2",
                                "Speechiness" = "spe2",
                                "Tempo" = "tmp2",
                                "Valence" = "val2"
                              )))),
            fluidRow(
              box(width = 8, title = 'Scatterplot', solidHeader = TRUE, status = 'info', plotOutput("plot6", height = 500)),
              box(width = 4, title = 'Correlation Matrix', solidHeader = TRUE, status = 'info', plotOutput("plot7", height = 500))
    ))
  ))


server <- function(input, output) {
  
  # plot 1
  output$plot1 <- renderPlot({
    ggplot(Songs) +
      geom_bar(aes(x=genre, fill=genre)) +
      geom_text(data=Count, aes(x=genre, y=n, label=n2),vjust=-1) +
      theme(legend.position="none") +
      xlab('') +
      ylab('')
  })
  
  # plot 2
  pt1 <- reactive({
    asd <- switch(input$dist,
                  aco = Songs$acousticness,
                  dan = Songs$danceability,
                  drt = Songs$duration_ms,
                  ene = Songs$energy,
                  ins = Songs$instrumentalness,
                  key = Songs$key,
                  liv = Songs$liveness,
                  lou = Songs$loudness,
                  spe = Songs$speechiness,
                  tmp = Songs$tempo,
                  val = Songs$valence)
    if (input$g1){
      return(
        ggplot(Songs) +
          geom_histogram(aes(x=asd), bins=input$bins, colour='blue', fill='red', alpha=0.5) +
          ylab('') +
          xlab('')
      )
    } else {
      return(NULL)
    }
  })
  
  # plot 3
  pt2 <- reactive({
    asd <- switch(input$dist,
                  aco = Songs$acousticness,
                  dan = Songs$danceability,
                  drt = Songs$duration_ms,
                  ene = Songs$energy,
                  ins = Songs$instrumentalness,
                  key = Songs$key,
                  liv = Songs$liveness,
                  lou = Songs$loudness,
                  spe = Songs$speechiness,
                  tmp = Songs$tempo,
                  val = Songs$valence)
    
    if (input$g2){
      return(
        ggplot(Songs) +
          geom_boxplot(aes(x=asd), notch=T, notchwidth = 0.7, outlier.size=1,
                       outlier.colour="black", outlier.alpha=0.7, colour='blue', fill='red',
                       alpha=0.5) +
          ylab('') +
          xlab('')
      )
    } else {
      return(NULL)
    }
  })
  
  # plot 4
  pt3 <- reactive({
    asd <- switch(input$dist,
                  aco = Songs$acousticness,
                  dan = Songs$danceability,
                  drt = Songs$duration_ms,
                  ene = Songs$energy,
                  ins = Songs$instrumentalness,
                  key = Songs$key,
                  liv = Songs$liveness,
                  lou = Songs$loudness,
                  spe = Songs$speechiness,
                  tmp = Songs$tempo,
                  val = Songs$valence)
    if (input$g3){
      return(
        ggplot(Songs) +
          geom_qq(aes(sample=asd)) +
          geom_qq_line(aes(sample=asd), cex = 1.1, color = 'red')
      )
    } else {
      return(NULL)
    }
  })
  
  output$plot2 <- renderPlot({
    pt1()
  })
  output$plot3 <- renderPlot({
    pt2()  
  })
  output$plot4 <- renderPlot({
    pt3()  
  })
  
  # plot 5
  output$plot5 <- renderPlot({
    asd <- switch(input$dist2,
                  aco = Songs$acousticness,
                  dan = Songs$danceability,
                  drt = Songs$duration_ms,
                  ene = Songs$energy,
                  ins = Songs$instrumentalness,
                  key = Songs$key,
                  liv = Songs$liveness,
                  lou = Songs$loudness,
                  spe = Songs$speechiness,
                  tmp = Songs$tempo,
                  val = Songs$valence)
    
    ggplot(Songs, aes(x=genre, y=asd, fill=genre)) +
      geom_boxplot(notch=T, notchwidth = 0.7, width=0.5, outlier.size=1.5, outlier.colour="red", outlier.alpha=input$alpha) +
      theme(legend.position="none") +
      xlab('') +
      ylab('')
  })
  
  # plot 6
  Songs.s <- sample_frac(Songs, 0.05)
  
  output$plot6 <- renderPlot({
    asd <- switch(input$f1,
                  aco = Songs.s$acousticness,
                  dan = Songs.s$danceability,
                  drt = Songs.s$duration_ms,
                  ene = Songs.s$energy,
                  ins = Songs.s$instrumentalness,
                  key = Songs.s$key,
                  liv = Songs.s$liveness,
                  lou = Songs.s$loudness,
                  spe = Songs.s$speechiness,
                  tmp = Songs.s$tempo,
                  val = Songs.s$valence
    )
    
    qwe <- switch(input$f2,
                  aco2 = Songs.s$acousticness,
                  dan2 = Songs.s$danceability,
                  drt2 = Songs.s$duration_ms,
                  ene2 = Songs.s$energy,
                  ins2 = Songs.s$instrumentalness,
                  key2 = Songs.s$key,
                  liv2 = Songs.s$liveness,
                  lou2 = Songs.s$loudness,
                  spe2 = Songs.s$speechiness,
                  tmp2 = Songs.s$tempo,
                  val2 = Songs.s$valence
    )
    
    ggplot(Songs.s, aes(x=asd, y=qwe)) +
      geom_point(color="orange", fill="#69b3a2", shape=21, alpha=0.5, size=2, stroke = 2) +
      geom_smooth(method=lm, color="red", orientation='y', se=T,size=1) +
      xlab('') +
      ylab('')
  })
  
  # plot 7
  a<-data.frame(Acousticness = Songs$acousticness, Danceability = Songs$danceability, Duration.ms = Songs$duration_ms,
                Energy = Songs$energy, Instrumentalness = Songs$instrumentalness, Key = Songs$key, Liveness = Songs$liveness,
                Loudness = Songs$loudness, Speechiness = Songs$speechiness, Tempo = Songs$tempo, Valence = Songs$valence)
  b<-cor(a)
  
  output$plot7 <- renderPlot({
    corrplot(b, method="color")
  })
}

ui <- dashboardPage(skin = 'purple', header, sidebar, body)

shinyApp(ui, server)