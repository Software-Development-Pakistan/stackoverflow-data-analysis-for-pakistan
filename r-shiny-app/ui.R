library(shiny)
library(ggplot2)

pageWithSidebar(
  headerPanel('Stackoverflow Survey Data'),
  sidebarPanel(
    selectInput('country', 'Select Country', choices = df$Country, selected = "Pakistan"),
    selectInput('question', 'Select Question',choices = c('HaveWorkedLanguage','WantWorkLanguage','HaveWorkedFramework','WantWorkFramework','HaveWorkedPlatform','WantWorkPlatform'),selected='HaveWorkedLanguage'),
    width = 3
    #numericInput('clusters', 'Cluster count', 3,
     #            min = 1, max = 9)
  ),
  mainPanel(
    plotOutput('plot1',width = "100%", height = "300px")
  )
)