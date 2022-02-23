# https://top-names.info/
library(shiny)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(readxl)

babynames <- read_excel("african_names.xlsx")

inputCountry <- sort(unique(pull(babynames, "Country")))

ui <- fluidPage(theme = shinythemes::shinytheme("cosmo"),
    
    titlePanel("Top African Baby Names"),
    
    sidebarLayout(
        sidebarPanel(
            selectInput("country", "Country", inputCountry, "Tunisia"),
            radioButtons("sex", "Sex", c("M", "F"), "F"),
            sliderInput("topn", "Top N", 1, 10, 5)
        ),
        
        mainPanel(
            plotOutput("plot")
        )
    )
)

server <- function(input, output, session) {
    
    selectedData <- reactive({
        babynames %>% 
            filter(Country==input$country, Sex %in% input$sex) %>%
            top_n(input$topn, Freq)
    })
    
    output$plot <- renderPlot({
      lab <- ifelse(input$sex == "F", "Girl", "Boy")
      col <- ifelse(input$sex == "F", "pink", "lightblue")
      ggplot(selectedData(), aes(reorder(toupper(Name), Freq), Freq)) + 
        geom_bar(stat="identity", fill = col) + 
        coord_flip() +
        xlab("") +
        ylab("Frequency") +
        ggtitle(paste("Top", lab, "Names in", input$country)) +
        theme(text = element_text(size=20))
    })

}

shinyApp(ui = ui, server = server)
