library(shiny)
library(ggplot2)

freqfunc <- function(x, n){
  tail(sort(table(unlist(strsplit(as.character(x), ";")))), n)
}

function(input, output, session) {
  
  # Combine the selected variables into a new data frame
  selectedData <- reactive({
    subset(df, Country == input$country, select=c("Country",input$question))
    #iris[, c(input$xcol, input$ycol)]
  })
  
  # clusters <- reactive({
  #   kmeans(selectedData(), input$clusters)
  # })
  
  output$plot1 <- renderPlot({
    # palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
    #           "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
    feature_data = selectedData()
    colnames(feature_data) = c("Country","question")
    
    #feature_data = feature_data[complete.cases(feature_data), ]
    #question = trimws(as.character(feature_data$question),which = c("both", "left", "right"))
    #question <- strsplit(question, ";")
    #sel_df = head(data.frame(table(unlist(strsplit(tolower(question), ";")))),5)
    sel_df = as.data.frame(freqfunc(feature_data$question, 10))
    ggplot(data=sel_df, aes(x=sel_df$Var1, y=sel_df$Freq , fill = factor(sel_df$Var1))) +
      geom_bar(stat="identity") +
      xlab("Responndants choices") +
      ylab("Number of Resonpses") +
      theme(legend.position="none")
    # par(mar = c(5.1, 4.1, 0, 1))
    # plot(selectedData(),
    #      col = clusters()$cluster,
    #      pch = 20, cex = 3)
    # points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
  })
  
}