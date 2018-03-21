library(shiny)
library(ggplot2)
library(dplyr)


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
  
  genderData <- reactive({
    subset(so_data, Country == 'Pakistan', select=c("Country","Gender"))
    
  })
  
  output$plot2<-renderPlot({
    
    
    feature_data = genderData()
    colnames(feature_data) = c("Country","Gender")
    
    sel_df <- so_data %>%
      filter(Country=='Pakistan') %>%
      count(Gender) %>%
      na.omit()
    
    #sel_df = as.data.frame(freqfunc(feature_data$question, 10))
    
    ggplot(data=sel_df, aes(x=sel_df$Gender, y=sel_df$n , fill = factor(sel_df$Gender))) +
      ggtitle("Gender Diversity of Developers in Pakistan") +
      geom_bar(stat="identity") +
      xlab("Responndants choices") +
      ylab("Number of Resonpses") +
      theme(legend.position="none")
  }
  
  
  )
  
}