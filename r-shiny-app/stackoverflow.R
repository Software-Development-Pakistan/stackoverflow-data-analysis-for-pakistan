
library(ggplot2)


text_split <- function(text) {
  slist = strsplit(text, ";")
  return(slist)
}

data = read.csv("./data/survey_results_public.csv")

df = data[,c('HaveWorkedLanguage','WantWorkLanguage','HaveWorkedFramework','WantWorkFramework','HaveWorkedPlatform','WantWorkPlatform')]

HaveWorkedLanguage = as.character(df$HaveWorkedLanguage)

HaveWorkedLanguage <- strsplit(HaveWorkedLanguage, ";")


sdf = head(data.frame(table(unlist(strsplit(tolower(df$WantWorkLanguage), ";")))),10)


head(subset(data, Country == "Pakistan", select=c(Country, University)))


ggplot(data=sdf, aes(x=Var1, y=Freq)) +
  geom_bar(stat="identity", fill="steelblue")


freqfunc <- function(x, n){
  tail(sort(table(unlist(strsplit(as.character(x), ";")))), n)
}

as.data.frame(freqfunc(df$HaveWorkedLanguage, 10))

