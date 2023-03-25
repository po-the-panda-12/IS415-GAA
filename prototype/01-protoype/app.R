pacman::p_load(shiny, tidyverse, sf, tmap)

sgpools <- read_csv("data/aspatial/SGPools_svy21.csv")
sgpools_sf <- st_as_sf(sgpools,
                       coords = c("XCOORD", "YCOORD"),
                       crs = 3414)

ui <- fluidPage(

    titlePanel("Static Proportionate Symbol Map"),
    sidebarLayout(
      sidebarPanel(),
      mainPanel(
        plotOutput("mapPlot")
      )
    )
)

server <- function(input, output) {
  
  output$mapPlot <- renderPlot({
    tm_shape(sgpools_sf) + 
      tm_bubbles(col = "OUTLET TYPE", 
                 size = "Gp1Gp2 Winnings",
                 border.col = "black",
                 border.lwd = 0.5)
  })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
