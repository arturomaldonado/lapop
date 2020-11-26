# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(rio)
library(shiny)
library(haven)
#Read data
#lapop <- read_dta("LAPOP_Merge_2004_2018.dta")
lapop$pais = as.factor(lapop$pais)
levels(lapop$pais) <- c("México", "Guatemala", "El Salvador", "Honduras",
                        "Nicaragua","Costa Rica", "Panamá", "Colombia", 
                        "Ecuador", "Bolivia", "Perú", "Paraguay", "Chile",
                        "Uruguay", "Brasil", "Venezuela", "Argentina", 
                        "Rep. Dom.", "Haití", "Jamaica", "Guyana", 
                        "Trinidad y Tobago", "Belice", "Surinám", "Bahamas",
                        "Barbados", "Granada", "Santa Lucía","Dominica", 
                        "Antigua y Barbuda", "San Vicente y las Granadinas", 
                        "San Kitts y Nevis", "Estados Unidos", "Canada")
lapop$wave = as.factor(lapop$wave)
# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("Barómetro de las Américas por LAPOP"),
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput("var",
                  label="Selecciona una variable",
                  choices = c("Ideología", "Democracia"),
                  selected="Ideología"),
      selectInput("pais", label = "País:",
                  choices = unique(lapop$pais)),
      selectInput("año", label = "Año:",
                  choices = unique(lapop$wave)),
    ),
    
    mainPanel(
      plotOutput("distPlot"),
      tableOutput("tabla"),
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$distPlot <- renderPlot({
    if(input$var=="Ideología"){
      hist(lapop$l1[lapop$pais==input$pais & lapop$wave==input$año], freq=F, 
           col = 'darkgray', border = 'white', main="Histograma de ideología
                por país", xlim=c(1, 10), xlab="Ideología")}
    if(input$var=="Democracia"){
      hist(lapop$ing4[lapop$pais==input$pais & lapop$wave==input$año], freq=F, 
           col = 'darkgray', border = 'white', main="Histograma de ideología
                por país", xlim=c(1, 7), xlab="Ideología")}
  })
  
  output$tabla <- renderTable({
    if(input$var=="Ideología"){
      table(lapop$l1[lapop$pais==input$pais & lapop$wave==input$año])}
    if(input$var=="Democracia"){
      table(lapop$ing4[lapop$pais==input$pais & lapop$wave==input$año])}
  })
}

# Run the application 
shinyApp(ui = ui, server = server)