#   ____________________________________________________________________________
#   UI                                                                      ####uyy
library(shiny)
library(leaflet)
library(plotly)
library(shinyjs)
library(shinyBS)
source("appParts.R")
source("readData.R")
#setwd("/cloud/project/Dashboard_oferta_demanda_Laboral/intelligentsia-master/intelligentsia-master")
### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Colors                                                                  ####

#C10250 purple
#03BCC0 green
#D2D945 yellow/green
#FCB040 orange
#FF5850 red
#436983 hipster blue

shinyUI(navbarPage(title = "InmoVision",
                   theme = "style/style.css",
                   footer = includeHTML("footer.html"),
                   fluid = TRUE, 
                   collapsible = TRUE,
                     tags$head(
  tags$script(src = "//translate.google.com/translate_a/element.js?cb=googleTranslateElementInit"),
  tags$script(HTML("
      function googleTranslateElementInit() {
        new google.translate.TranslateElement({pageLanguage: 'en'}, 'google_translate_element');
      }
    "))
),
div(id = "google_translate_element"),
                   # ----------------------------------
                   # tab panel 1 - Home
                   tabPanel("Inicio",
                            includeHTML("home.html"),
                            tags$script(src = "plugins/scripts.js"),
                            tags$head(
                              tags$link(rel = "stylesheet", 
                                        type = "text/css", 
                                        href = "plugins/font-awesome-4.7.0/css/font-awesome.min.css"),
                              tags$link(rel = "stylesheet", 
                                        type = "text/css", 
                                        href = "plugins/carousel.css"),
                              tags$link(rel = "icon", 
                                        type = "image/png", 
                                        href = "images/logo_icon.png")
                            )
                   ),
                   
                   # ----------------------------------
                   # tab panel 2 - Neighborhood Browser
                   tabPanel("Descubre",
                            neighborhoodDescription()
                   ),
                   tabPanel("Personaliza tu busqueda",
                            neighborhoodDescription2()
                   ),
                   # ----------------------------------
                   # tab panel 3 - Location Comparison
                   tabPanel("Compara ubicaciones",
                            comparison()
                   ),
                   tabPanel("Proyecta tu inversion",
                            neighborhoodDescription4()
                   )
                   
))
