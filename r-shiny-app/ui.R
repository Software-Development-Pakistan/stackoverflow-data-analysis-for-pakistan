library(shiny)
library(ggplot2)

fluidPage(
  headerPanel('Stackoverflow Survey Data'),
  sidebarPanel(
    selectInput('country', 'Select Country', choices = df$Country, selected = "Pakistan"),
    selectInput('question', 'Select Question',choices = c('HaveWorkedLanguage','WantWorkLanguage','HaveWorkedFramework','WantWorkFramework','HaveWorkedPlatform','WantWorkPlatform'),selected='HaveWorkedLanguage'),
    width = 4
  ),
  mainPanel(
    plotOutput('plot1',width = "100%", height = "300px"),
    plotOutput('plot2',width = "100%", height = "300px")
  )
)