#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(leaflet)
library(ggplot2)


server <- function(input, output, session) {
  
  dat<-reactive({
    long <- runif(input$myslider, -121, -77 )
    lat <- runif(input$myslider, 33, 48)
    x <- runif(input$myslider, 1, 50)
    y <- rnorm(input$myslider, 1, 50)
    data.frame(latitude = lat, longitude = long, x, y)
  })
  
  br <- reactive({
    brushedPoints(dat(), input$plot_brush)
  })
  
  output$plot <- renderPlot({
    ggplot(dat(), aes(x, y, color=y)) + geom_point() +
      ggtitle("Select points by brushing graph")
  })
  
  output$mymap <- renderLeaflet({
    
    # if nothing has been selected use the data itself, otherwise
    # use a brushed version
    ifelse(is.null(input$plot_brush), dat <- dat(), dat <- br() )
    
    leaflet(data = dat) %>%
      addProviderTiles("CartoDB.Positron",
                       options = providerTileOptions(noWrap = TRUE)) %>%
      addMarkers(~longitude, ~latitude) %>%
      setView(-98, 38.6, zoom=3)
  })
  
  
  
  output$table_brushedpoints <- renderTable({
    br()[,c("x", "y")]
  })
}


ui <- fluidPage(
  
  titlePanel("Interactive map using brushing"),
  
  sidebarLayout(
    
    sidebarPanel(
      h3("Slider changes number of points"),
      sliderInput(inputId = "myslider", "Number of points:",
                  min = 5, max = 20, value = 10, step = 1),
      plotOutput("plot", height = 250,
                 brush = brushOpts(id = "plot_brush", resetOnNew = TRUE,
                                   fill = "green", stroke = "#036", opacity = 0.3)),
      h4("Detail Selected brushed points"),
      tableOutput("table_brushedpoints")
    ), #endsidebarpanel
    
    mainPanel(
      tabsetPanel(
        tabPanel("Map", leafletOutput("mymap")), 
        tabPanel("Documentation",
                 p(h1("Leaflet Interactive map using brushing")),
                 helpText("In the map tab you will have two options for interacting with the graph, one will be by selecting your prefered number of points from the slide bar.  The second option will be by brushing for the number of points you want to be displayed on the map (refer to the explanation below) "),
                 helpText("Graph Brushing is selecting one or more data points on a graph to identify the corresponding worksheet information. Use brushing to investigate characteristics of data points of interest, for example: Identify the rows that contain the outliers. Determine whether points in a brushing region share other characteristics.  In our example it will show the latittude and longitude in the map only for the selected points that are in the brushed area"),
                 img(src='Selection Methods.png', align = "center", height="333", width="450"),
                  HTML("<style>
                      body {
                      font-family: 'Abel';font-size: 22px;
                      <br>
                      <table style=width:100%>
                      <tr>
                      <th></th>
                      </tr>
                      </table><br>
                         }")
                 )
                    )
    )#end mainpanel
  )# end sidebarlayout
)


shinyApp(ui = ui, server = server)