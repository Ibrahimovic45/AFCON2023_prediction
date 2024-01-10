#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#markdown?
#install.packages("shinydashboardPlus")
#install.packages("shinydashboardPlus")
#install.packages("SwimmeR")
#install.packages("shiny")
#install.packages("shinydashboard")
#install.packages("reticulate")
#install.packages("spsComps")
#install.packages("zeallot")
#install.packages("rintrojs")
#install.packages("shinyBS")
#install.packages("shinycssloaders")
#install.packages("shinyjs")
#install.packages("shinyWidgets")
#install.packages("survival")
#install.packages("ggpubr")
#install.packages("survminer")
#install.packages("viridis")
#install.packages("zoo")

#py_install("pandas")
#setwd("C:/Users/DELL/Desktop/project x/can_2024/can24")
library(shiny)
library(rintrojs)
library(shinyBS)
library(shinycssloaders)
library(shinydashboard)
library(shinyjs)
library(shinyWidgets)
library(survival)
library(ggpubr)
library(survminer)
library(tidyverse)
library(viridis)
library(zoo)
library(shinydashboardPlus)
library(lubridate)
library(plotly)
library(SwimmeR)
library(zeallot)
library(spsComps)
library(leaflet)



#Sys.setenv(RETICULATE_PYTHON = "C:/Users/DELL/anaconda3/envs/env-reticulate/python.exe")
library(reticulate)
#path_to_python <- "C:/Users/DELL/anaconda3/envs/env-reticulate/python.exe"
#use_python(path_to_python)



# create a new environment
#conda_create("env-reticulate")

# indicate that we want to use a specific condaenv
use_condaenv("env-reticulate")


#reticulate::conda_install(
#packages = c("scikit-learn", "pandas","lxml","networkx","pygraphviz",
#             "matplotlib","seaborn","graphviz"),
#envname = "env-reticulate"
#)



#virtualenv_create("d-reticulate")
#virtualenv_install(envname = "d-reticulate",packages = c("matplotlib", "seaborn",
                   #"scikit-learn", "pandas","lxml","networkx",
                   #"tmtoolkit", "gensim"))
#virtualenv_install(envname = "d-reticulate",packages = c("graphviz","pygraphviz"),
                   #pip_options = shQuote('--global-option=build_ext --global-option="-IC:\\Program Files\\Graphviz2.38\\include" --global-option="-LC:\\Program Files\\Graphviz2.38\\lib\\"'))

#use_virtualenv("d-reticulate")



matplolib <- import("matplotlib")
seaborn <- import("seaborn")
networkx <- import("networkx")
pandas <- import("pandas")
sklearn <- import("sklearn")
#graphviz <- import("graphviz")
pygraphviz<- import("pygraphviz")
#reticulate::py_install(
  #packages = c("matplotlib,seaborn"),
  #envname = "d-reticulate",
  #pip = TRUE
  #)


#reticulate::conda_install(
 # packages = "anaconda::pygraphviz",
  #envname = "d-reticulate")


#reticulate::py_install(
  #packages = c("tmtoolkit", "gensim"),
 #envname = "d-reticulate",
 #pip = TRUE
#)
#library(reticulate)

#library(checkpoint)
#heckpoint(snapshotDate ='2024-01-6')
#library(AMR)
#library(data.table)
#library(DT)
#library(ggridges)

#library(qicharts2)


#library(shinydashboard)
#library(shiny)

#withConsoleRedirect <- function(containerId, expr) {
  # Change type="output" to type="message" to catch stderr
  # (messages, warnings, and errors) instead of stdout.
#  txt <- capture.output(results <- expr, type = "output")
 # if (length(txt) > 0) {
  #  insertUI(paste0("#", containerId), where = "beforeEnd",
   #          ui = paste0(txt, "\n", collapse = "")
    #)
#  }
 # results
#}

source_python("afcon_simulation_f4.py")
source_python("plot.py")








# Define UI for application that draws a histogram
ui = dashboardPage( 
  skin = "black",
  title = "AFCON 2024",
  #notificationItem(
  #text = ("hello"),
  ##),
  # HEADER ------------------------------------------------------------------

  header = dashboardHeader(
    title = span(img(src = "www/mascot.jpg", height = 35), "AFCON 2024"),
    titleWidth = 300),

  sidebar = dashboardSidebar(width = 300,
                   sidebarMenu(id = "sidebarID",
                     menuItem("Intro", tabName = "intro", icon = icon("dashboard")),
                     menuItem("AFCON simulation", icon = icon("th"), tabName = "simulation",
                              badgeLabel = "new", badgeColor = "green"),
                     menuItem("Favorites", id = "fav", tabName = "Favorites", icon = icon("fa-duotone fa-futbol"), expandedName = "FAVORITES",
                              menuSubItem("Ivory Coast", tabName = "CIV"),
                              menuSubItem("Morocco", tabName = "MRC"),
                              menuSubItem("Nigeria", tabName = "NGA"),
                              menuSubItem("Senegal", tabName = "SEN"),
                              menuSubItem("Egypt", tabName = "EGY")),
                     hidden(menuItem("hiddenfavs", tabName = "hiddenfavs")),
                     menuItem("About", tabName = "about", icon = icon("dashboard"))


                   )),



  body = dashboardBody(
                useShinyjs(),
                 #introjsUI(),
                 spsDepend("shinyCatch"),
                 tabItems(
                   tabItem(tabName = "intro",
                           h2("Here we go!"),
                           fluidRow(
                             column(7,
                                    includeText("intro.txt"),
                                    br()

                             ),
                             column(5,
                                    leafletOutput("mymap")
                             )
                           )
                   ),

                   tabItem(tabName = "simulation",
                           #tags$style('.popover ul {padding: 15px; text-align: justify;}',
                                      #'.popover-content {text-align: justify;}'),
                           h2("Competition simulation"),

                           actionButton("simulate", "Simulate!", class = "btn-success"),
                           #pre(id = "console")
                           #verbatimTextOutput("textOutput")
                           #uiOutput("textOutput"),
                           #textOutput("textOutput"),
                           #tags$style(type="text/css", "#textOutput {white-space: pre-wrap;}")
                           fluidRow(
                             column(5,
                                    tagAppendAttributes(textOutput("textOutput"), style="white-space:pre-line;")),
                             column(6,
                                    textOutput("AUC",container = tags$h4),
                           #plotOutput("plot"),
                           plotOutput("plot2")),
                           
                   )
                 ),
                 tabItem("hiddenfavs", "Favorites"),
                 tabItem("CIV",
                         h2("Ivory Coast"),
                         fluidRow(
                           column(4,
                         includeText("Ivory_Coast.txt")),
                         column(1),
                         column(7,
                           imageOutput("civ_img")))

                 ),
                 tabItem("MRC",
                         h2("Morocco"),
                         fluidRow(
                           column(4,
                                  includeText("Morocco.txt")),
                           column(1),
                           column(7,
                                  imageOutput("mrc_img")))

                 ),
                 tabItem("NGA",
                         h2("Nigeria"),
                         fluidRow(
                           column(4,
                                  includeText("Nigeria.txt")),
                           column(1),
                           column(7,
                                  imageOutput("nga_img")))

                 ),
                 tabItem("SEN",
                         h2("Senegal"),
                         fluidRow(
                           column(4,
                                  includeText("Senegal.txt")),
                           column(1),
                           column(7,
                                  imageOutput("sen_img")))

                 ),
                 tabItem("EGY",
                         h2("Egypt"),
                         fluidRow(
                           column(4,
                                  includeText("Egypt.txt")),
                           column(1),
                           column(7,
                                  imageOutput("egy_img")))

                 ),
                 tabItem(tabName = "about",
                         h2("Credits"),
                         h4("HTML: Obele Giovanna."),
                         h4("Adapted from FIFA prediction by SERGIO PESSOA"),
                         uiOutput("link"),
                         #h4("Designed by B.")
                         tags$footer(strong("Designed by B."), # strong() = bold
                                     align = "right", 
                                     style = "
                 position:fixed;
                 bottom:11.5px;
                 width:50%;
                 height:20px; 
                 color: black;
                 padding: 0px;
                 
                 z-index: 100;
                ") 
                         #background-color: lightgrey;       
                        
                         )
                        
                                  
                           )

                 # MAIN BODY ---------------------------------------------------------------

  )
)
    #footer = dashboardFooter(
        #  left = "Html: Obele Giovanna",
          #right = "Designed by B.")




# Define server logic required to draw a histogram
server <- function(input, output,session) {

  pal <- colorFactor(pal = c("#1b9e77", "#d95f02", "#7570b3"), domain = c("Charity", "Government", "Private"))



  stadiums <- tibble(names = c("Alassane Ouattara Stadium, Abidjan", "Felix Houphouet-Boigny Stadium, Abidjan",
                               "Stade Charles Konan Banny, Yamoussoukro", "Stadium of Peace, Bouake",
                               "Amadou Gon Coulibaly Stadium, Korhogo", "Laurent Pokou Stadium, San Pedro"),
                     latitude = c(5.48081, 5.3282, 6.8292, 7.6830, 9.41022, 4.81695),
                     longitude = c(-4.07456, -4.0188,-5.2464,-5.0449,-5.6285,-6.62946))

  output$mymap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%  # Add default OpenStreetMap map tiles
      addMarkers(data = stadiums, lat = ~latitude, lng= ~longitude, popup=~names)
  })
  #observeEvent(input$simulate, {
    #shinyCatch({afcon_sim()}, prefix = '')})



  #c(playoffs_log,text,final_table_log) %<-% afcon_sim()



  observeEvent(input$simulate, {
    withProgress(message = "Computing...", {
      for (i in 1:105) {
        incProgress(1/105)
        Sys.sleep(0.25)
      }
      
        #Sys.sleep(0.5)
        #incProgress(0.2)
        results <- afcon_sim()
                    a <- format(results[2], fill = getOption("width"))#, justify = "left", trim = TRUE)#
                    a <- gsub(",", "", gsub("([a-zA-Z]),", "\\1 ", a))
                              

     output$textOutput <-renderText(paste(a, sep = "") )
     
     output$AUC <- renderText(paste("The model accuracy is:",as.numeric(unlist(results[6])*100)))
     
   

    df <- unlist(results[1])
    champion <- unlist(results[4])
    #output$plot <- renderPlot({

      #draw_bracket(
       # teams = c(df["Round of 161"],df["Round of 162"],df["Round of 165"],df["Round of 166"],df["Round of 169"],df["Round of 1610"],
        #          df["Round of 1613"],df["Round of 1614"],df["Round of 1617"],df["Round of 1618"],df["Round of 1621"],
         #         df["Round of 1622"],df["Round of 1625"],df["Round of 1626"],df["Round of 1629"],df["Round of 1630"]),
        #text_size = 1,
        #title = "Knockout stage",
        #round_two = c(df["Quarter-Final1"],df["Quarter-Final2"],df["Quarter-Final5"],df["Quarter-Final6"],df["Quarter-Final9"],
         #             df["Quarter-Final10"],df["Quarter-Final13"],df["Quarter-Final14"]),
        #round_three = c(df["Semi-Final1"],df["Semi-Final2"],df["Semi-Final5"],df["Semi-Final6"]),
        #round_four = c(df["Final1"],df["Final2"]),
        #champion = champion
      #)
    #})
    
    output$plot2 <- renderPlot({bracket()},width = 660, height = 700, res = 70)
    })
  })



  #values <- reactiveValues()
  #logText <- reactive({
  #values[["log"]] <- capture.output(data <- afcon_sim()) })

  #lapply(text, function(i){
  #  output$simulation_text <- renderPrint({x})
  #})


  #rv <- reactiveValues()

  #output$txt <- renderPrint({
    #vals <- list()
    #for (val in reactiveValuesToList(rv)) {
      #vals[[val]] <- val
    #}
    #for (i in seq_len(length(vals))) {
      #print(vals[[i]])
    #}
  #})


    #logText()
    #return(print(capture.output(data <- afcon_sim())))
    # You could also use grep("Warning", values[["log"]]) to get warning messages and use shinyBS package
    # to create alert message
  #})
  #observe({
    #invalidateLater(1000)

    #withConsoleRedirect("console", {
     #afcon_sim()
    #})
  #})

  output$civ_img <- renderImage({

    list(src = "www/civ.jpg",
         width = "100%",
         height = 330)

  }, deleteFile = F)
  output$mrc_img <- renderImage({

    list(src = "www/mrc.jpeg",
         width = "100%",
         height = 330)

  }, deleteFile = F)
  output$nga_img <- renderImage({

    list(src = "www/nga.jpg",
         width = "100%",
         height = 330)

  }, deleteFile = F)
  output$sen_img <- renderImage({

    list(src = "www/sen.jpg",
         width = "100%",
         height = 330)

  }, deleteFile = F)
  output$egy_img <- renderImage({

    list(src = "www/egy.jpg",
         width = "100%",
         height = 330)

  }, deleteFile = F)

  url <- a("Here!", href="https://www.kaggle.com/code/sslp23/predicting-fifa-2022-world-cup-with-ml")
  output$link <- renderUI({
    tagList("Link to Kaggle:", url)
  })




}

# Run the application
shinyApp(ui = ui, server = server)



