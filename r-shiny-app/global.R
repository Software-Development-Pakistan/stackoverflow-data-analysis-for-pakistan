

so_data = readRDS("./data/data2017.rds")
#so_data = read.csv("./data/survey_results_public.csv")
df = so_data[,c("Country",'HaveWorkedLanguage','WantWorkLanguage','HaveWorkedFramework','WantWorkFramework','HaveWorkedPlatform','WantWorkPlatform')]
