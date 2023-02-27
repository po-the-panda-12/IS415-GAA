pacman::p_load(shiny, tidyverse, sf, tmap, DT)

mpsz <- st_read(dsn = "data/geospatial", 
                layer = "MP14_SUBZONE_WEB_PL")

popdata <- read_csv("data/aspatial/respopagesextod2011to2020.csv")

#chapter 2
pop <- popdata %>%
  filter(Time == 2019) %>%
  group_by(PA,SZ, AG) %>%
  summarise(`POP` = sum(`Pop`)) %>%
  ungroup () %>%
  pivot_wider(names_from = AG,
              values_from = POP) %>%
  # spread(AG, Pop) %>%
  mutate(YOUNG = `0_to_4`+`5_to_9`+`10_to_14`+
           `15_to_19`+`20_to_24`) %>%
  mutate(`ECONOMY ACTIVE` = rowSums(.[9:13])+
           rowSums(.[15:17]))%>%
  mutate(`AGED`=rowSums(.[18:22])) %>%
  mutate(`TOTAL`=rowSums(.[5:22])) %>%  
  mutate(`DEPENDENCY` = (`YOUNG` + `AGED`)
         /`ECONOMY ACTIVE`) %>%
  mutate_at(.vars = vars(PA, SZ), 
            .funs = funs(toupper)) %>%
  select(`PA`, `SZ`, `YOUNG`, 
         `ECONOMY ACTIVE`, `AGED`, 
         `TOTAL`, `DEPENDENCY`) %>%
  filter(`ECONOMY ACTIVE` > 0)
mpsz_demog <- left_join(mpsz,
                        pop,
                        by = c("SUBZONE_N" = "SZ"))

ui <- fluidPage(

    titlePanel("Choropleth Mapping System"),
    sidebarLayout(
      sidebarPanel(
        
        #mapping variable
        selectInput(inputId = "variable",
                    label = "Mapping variable",
                    choices = list("Dependency" = "DEPENDENCY",
                                   "Young" = "YOUNG",
                                   "Economy Active" = "ECONOMY ACTIVE",
                                   "Aged" = "AGED"),
                    selected = "DEPENDENCY"),
        
        #classification method
        selectInput(inputId = "classification",
                    label = "Classification method:",
                    choices = list("fixed" = "fixed", 
                                   "sd" = "sd", 
                                   "equal" = "equal", 
                                   "pretty" = "pretty", 
                                   "quantile" = "quantile", 
                                   "kmeans" = "kmeans", 
                                   "hclust" = "hclust", 
                                   "bclust" = "bclust", 
                                   "fisher" = "fisher", 
                                   "jenks" = "jenks"),
                    selected = "pretty"),
        
        #number of classes
        sliderInput(inputId = "classes", 
                    label = "Number of classes",
                    min = 6,
                    max = 12,
                    value = c(8)),
        
        #colour scheme
        selectInput(inputId = "colour",
                    label = "Colour scheme:",
                    choices = list("blues" = "Blues", 
                                   "reds" = "Reds", 
                                   "greens" = "Greens",
                                   "Yellow-Orange-Red" = "YlOrRd",
                                   "Yellow-Orange-Brown" = "YlOrBr",
                                   "Yellow-Green" = "YlGn",
                                   "Orange-Red" = "OrRd"),
                    selected = "YlOrRd")
        
      ),
      mainPanel(
        tmapOutput("mapPlot",
                   width = "100%",
                   height = 400) #600 will touch base
      )
    )
)

server <- function(input, output) {

  output$mapPlot <- renderTmap({
    sf_use_s2(FALSE)
    #tmap_options(check.and.fix = TRUE) + #one buggy polygon in data
      tm_shape(mpsz_demog) + 
      tm_fill(col = input$variable,
              n = input$classes,
              style = input$classification,
              palette = input$colour) + 
      tm_borders(lwd = 0.1, alpha = 1) + 
        tm_view(set.zoom.limits = c(11,16))
  })
 

    
}

# Run the application 
shinyApp(ui = ui, server = server)
