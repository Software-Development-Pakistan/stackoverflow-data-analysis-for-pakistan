library(shiny)
library(ggplot2)

freqfunc <- function(x, n){
  tail(sort(table(unlist(strsplit(as.character(x), ";")))), n)
}

function(input, output, session) {
  
  
  selectedData <- reactive({
    subset(df, Country == input$country, select=c("Country",input$question))
    
  })
  
  
  output$plot1 <- renderPlot({

    feature_data = selectedData()
    colnames(feature_data) = c("Country","question")

    sel_df = as.data.frame(freqfunc(feature_data$question, 10))
    ggplot(data=sel_df, aes(x=sel_df$Var1, y=sel_df$Freq , fill = factor(sel_df$Var1))) +
      geom_bar(stat="identity") +
      xlab("Respondents choices") +
      ylab("Number of Resonpses") +
      theme(legend.position="none")
  })
  
}