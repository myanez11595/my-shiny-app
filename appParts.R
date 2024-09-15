#   ____________________________________________________________________________
#   Neighborhood Browser                                                    ####

neighborhoodDescription <- function() {
    tagList(
        div(class = "container",
            h1("Descubre", class = "title fit-h1"),
            column(6,
                   p("¿Es usted nuevo en Ecuador o una inversión en bienes raíces? Utilice nuestra herramienta 'Descubrir' para identificar focos de oportunidades en la ciudad."),
                   p("Navega por el mapa web, activa y desactiva las capas y haz clic en la ubicación donde deseas conocer el precio actual e histórico de la casa, departamentos y terreno.")
                   ),
            column(6,
                   img(src = "images/DISCOVER.gif", align = "left", height = '250px', width = '500px')),
            column(12,
                   br()),
            fluidRow(
                column(12,
                       withLoader(leafletOutput("map", height = 600, width = 1100))
                       )
            )
        )
    )
}

neighborhoodDescription2 <- function() {
  tagList(
    div(class = "container",
        h1("Personaliza tu busqueda", class = "title fit-h1"),
        column(6,
               p("¿Eres nuevo en Nuevo Ecuador o en inversión inmobiliaria? Utilice el navegador de barrios de Inmovision para identificar focos de oportunidad en el país."),
               p("Utilice el mapa para explorar los lugares en proceso de gentrificación en Ecuador.")),
        column(6,
               img(src = "images/CUSTOMIZE.gif", align = "left", height = '250px', width = '500px')),
        column(12,br()),
        column(12,fluidRow(wellPanel(
          style = "background-color: #eff7d2; padding: 10px; border-color: #D3D3D3; height: 200px;",
          column(4,sliderInput('terrenos', 'Costo m2 Terrenos', min = (round(min(HEX$T_ene24),0)-1), max = (round(max(HEX$T_ene24),0)+1), value = c((round(min(HEX$T_ene24),0)+1), (round(max(HEX$T_ene24),0)-1)))),
          column(4,sliderInput('casas', 'Costo m2 Casas', min = (round(min(HEX$C_ene24),0)-1), max = (round(max(HEX$C_ene24),0)+1), value = c((round(min(HEX$C_ene24),0)+1), (round(max(HEX$C_ene24),0)-1)))),
          column(4,sliderInput('departamentos', 'Costo m2 Departamentos', min = (round(min(HEX$D_ene24),0)-1), max = (round(max(HEX$D_ene24),0)+1), value = c((round(min(HEX$D_ene24),0)+1), (round(max(HEX$D_ene24),0)-1)))),
          column(4,sliderInput('terrenos_av', 'Costo Promedio Terrenos', min = (round(min(HEX$t_cost[!is.na(HEX$t_cost)]),0)-1), max = (round(max(HEX$t_cost[!is.na(HEX$t_cost)]),0)+1), value = c((round(min(HEX$t_cost[!is.na(HEX$t_cost)]),0)+1), (round(max(HEX$t_cost[!is.na(HEX$t_cost)]),0)-1)))),
          column(4,sliderInput('casas_av', 'Costo Promedio Casas', min = (round(min(HEX$c_cost[!is.na(HEX$c_cost)]),0)-1), max = (round(max(HEX$c_cost[!is.na(HEX$c_cost)]),0)+1), value = c((round(min(HEX$c_cost[!is.na(HEX$c_cost)]),0)+1), (round(max(HEX$c_cost[!is.na(HEX$c_cost)]),0)-1)))),
          column(4,sliderInput('departamentos_av', 'Costo Promedio Departamentos', min = (round(min(HEX$d_cost[!is.na(HEX$d_cost)]),0)-1), max = (round(max(HEX$d_cost[!is.na(HEX$d_cost)]),0)+1), value = c((round(min(HEX$d_cost[!is.na(HEX$d_cost)]),0)+1), (round(max(HEX$d_cost[!is.na(HEX$d_cost)]),0)-1))))
        )
        )),
        fluidRow(
          column(12,
                 withLoader(leafletOutput("map2", height = 600, width = 1100))
          )
        )
    )
  )
}

comparison <- function() {
  tagList(
    div(class = "container",
        h1("¡Explora y Decide con Confianza!", class = "title fit-h1"),
        column(6,
               p("En ¡Inmovision! te ofrecemos una herramienta poderosa para tomar decisiones informadas sobre tus inversiones inmobiliarias. Imagina tener el poder de comparar dos ubicaciones de manera rápida y precisa, viendo no solo a que tiempo se encuentran servicios esenciales como educación, salud y entretenimiento, sino también cómo ha evolucionado el precio en el tiempo en cada una de estas áreas."),
               p("Navega sobre los mapas, haz clic en los puntos de tu interés y conoce su información de servicios y económicos.")),
        column(6,img(src = "images/COMPARE.gif", align = "left", height = '250px', width = '500px')),
        column(12,br()),
fluidRow(
  column(6, withLoader(leafletOutput("map3", height = 600, width = "100%"))),
  column(6, withLoader(leafletOutput("map4", height = 600, width = "100%")))
)
),
fluidRow(
  column(12,
         #hidden(
         div(class = "container",#id = "reactiveOutput9", 
             style = "margin-top: 30px; font-size: 1.5em; align=center; text-align: center;width: 80%;",
             hr(),
             h3("Proximidad Inteligente: Comparación de Accesibilidad a Servicios"),
             DT::dataTableOutput("CTcomparisonTable"))
  )),#)

fluidRow(
  column(12,
         #hidden(
         div(class = "container",#id = "reactiveOutput9", 
             style = "margin-top: 30px; font-size: 1.5em; align=center; text-align: center;width: 80%;",
             hr(),
             h3("Rastreando la Evolución: Visualización Dinámica del Valor Inmobiliario"),
             column(6, plotlyOutput("lineplot1")),
             column(6, plotlyOutput("lineplot2")))
  ))

)
}

neighborhoodDescription4 <- function() {
  tagList(
    div(class = "container",
        h1("Adelantarte al Mercado ", class = "title fit-h1"),
        column(6,
               p("Imagina tener la capacidad de visualizar el crecimiento urbano y rural a lo largo del tiempo, desde el pasado hasta predicciones detalladas. Nuestra herramienta avanzada de análisis inmobiliario te brinda una visión clara y precisa de cómo las ciudades se expanden, permitiéndote identificar las mejores oportunidades de inversión antes que nadie. Esta herramienta te ofrece el poder de tomar decisiones informadas y estratégicas basadas en datos maximizando el valor de tu inversión."),
               p("Navega sobre los mapas, haz clic en los puntos de tu interés y conoce su información dinámica urbano")),
        column(6,
               img(src = "images/PREDICT.gif", align = "left", height = '250px', width = '500px')),
        column(12,
               br()),
        fluidRow(
          column(12,
                 withLoader(leafletOutput("map_pred", height = 600, width = 1100))
          )
        )
    )
  )
}