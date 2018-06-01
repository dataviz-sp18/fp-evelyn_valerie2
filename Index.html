library(shiny)
library(tidyverse)
library(shinythemes)
library(plotly) 
library(png)

hom <- read_csv("hom_en.csv")

ui <- fluidPage(theme = shinytheme("cerulean"),
  titlePanel("How did violence affect people like you?"),              
  br(),
  sidebarLayout(
    sidebarPanel(
      radioButtons("genderInput", "Gender",
                   choices = c("Female", "Male"),
                   selected = "Female"),
      selectInput("ageInput", "Age",
                choices = c("0 - 5", "6 - 11", "12 - 15", "16 - 18", "19 - 25", "26 - 40", "41 - 64", "65 +")),
      selectInput("eduInput", "Education level",
                choices = c("Primary (1 to 6 years)", "Secondary (7 to 9 years)", "High School (10 to 12 years)", "Bachelor Degree and more (12 to 15)", "No Education")),
      selectInput("edo_nomInput", "State",
                choices = c("Aguascalientes", "Baja California", "Baja California Sur", "Campeche", "Chiapas", "Chihuahua", 
                            "Coahuila", "Colima", "Durango", "Guanajuato", "Guerrero", "Hidalgo", "Jalisco",
                            "Morelos", "Nayarit", "Oaxaca", "Puebla", "Quintana Roo", 
                            "Sinaloa", "Sonora", "Tabasco", "Tamaulipas", "Tlaxcala", "Veracruz", "Zacatecas"))),     
    mainPanel(plotOutput("coolsplot"),
                br(),
                br(),
                textOutput("cooltext"),
                imageOutput("image"))
  ))
server <- function(input, output, session) {
  output$coolsplot <- renderPlot({
    filtered <- hom_en %>%
      filter(sex == input$genderInput,
             rango_edad == input$ageInput,
             escolaridad == input$eduInput,
             edo_nom == input$edo_nomInput
      )
    
      ggplot(filtered, aes(x = year, y = homtot)) +
      geom_col(aes(fill = year)) + 
      ylab("total homicides") + theme_minimal() + theme(legend.position = "none") +
      geom_text(aes(label = homtot), data = filtered, nudge_y = 1)
  })
  output$cooltext <- renderText({paste(input$genderInput, "between", 
                                       input$ageInput, "years with", input$eduInput, "from", input$edo_nomInput)
  }) 
  
  output$image <- renderImage({
    if (is.null(input$genderInput))
      return(NULL)
    
    if (input$genderInput == "Female") {
      return(list(
        src = "woman.png",
        contentType = "image/png",
        alt = "guns"
      ))
    } else if (input$genderInput == "Male") {
      return(list(
        src = "man.png",
        filetype = "image/png",
        alt = "man"
      ))
    }
    
  }, deleteFile = FALSE)
  
}
shinyApp(ui = ui, server = server)

