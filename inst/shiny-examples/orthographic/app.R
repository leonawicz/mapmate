library(shiny)
data(borders)
borders <- dplyr::mutate(borders, frameID=1)
id <- "frameID"

ui <- shinyUI(fluidPage(
   titlePanel("Orthographic projection example"),
   sidebarLayout(
      sidebarPanel(
          h4("Wait a moment for map to load. It may lag behind slider input changes."),
         sliderInput("lon", "Longitude (lon)", min=-180, max=180, value=0),
         sliderInput("lat", "Latitude (lat)", min=-90, max=90, value=0),
         sliderInput("orientation", "Orientation (rotation.axis)", min=0, max=360, value=24)
      ),
      mainPanel(
         plotOutput("orthoMap")
      )
   )
))

server <- shinyServer(function(input, output) {
   output$orthoMap <- renderPlot({
     save_map(borders, id=id, lon=input$lon, lat=input$lat, col="black", type="maplines", rotation.axis=input$orientation, save.plot=FALSE, return.plot=TRUE)
   })
})

shinyApp(ui, server)
