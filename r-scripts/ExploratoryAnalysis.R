library(dplyr)
library(ggplot2)
library(stringr)
library(jsonlite)
library(data.table)
library(reshape2)
library(tidyr)

# Job Sastifaction of developer type , pakistan compared with other big wig countries in IT (tested)

jobDf = so_data %>%
  select(DeveloperType,JobSatisfaction,Country) %>%
  filter(Country=='Pakistan' | Country=='China' | Country == 'United States' | Country == 'India') %>%
  mutate(DeveloperType = strsplit(as.character(DeveloperType), ";")) %>%
  unnest(DeveloperType) %>%
  na.omit(.) %>%
  mutate(DeveloperType=trimws(DeveloperType)) %>%
  group_by(DeveloperType,Country) %>%
  summarise_all(funs(sum))


setDT(jobDf)
jobDf_melt=melt(jobDf)
ggplot(data=jobDf_melt, aes(x=jobDf_melt$DeveloperType, y = jobDf_melt$value,fill=Country))+
  theme(axis.text.x = element_text(face="bold", color="#993333",size=8, angle=90)) +
  ggtitle("Job Satisfaction of Developers") +
  geom_bar(stat="identity") +
  xlab("Developer type") +
  ylab("Number of Respondents") +
  theme()

exportJson2<-jobDf %>% toJSON(pretty = T)

write(exportJson2, "Job_Satisfaction_Of_Each_Developer_Type_In_4_Big_IT-Countires.json")



# Relation between Have worked and want to work language among developers in Pakistan (tested)


haveWorked=so_data %>% 
  select(HaveWorkedLanguage,Country) %>%
  filter(Country=='Pakistan') %>%
  select(-Country) %>%
  na.omit() %>%
  mutate_if(is.factor,as.character) %>%
  mutate(HaveWorkedLanguage=gsub(" ","",HaveWorkedLanguage)) %>%
  pull(HaveWorkedLanguage) %>%
  str_c(.,collapse=';') %>%
  str_split(';') %>%
  unlist() %>%
  table() %>%
  as.data.frame()

haveWant=so_data %>% 
  select(WantWorkLanguage,Country) %>%
  filter(Country=='Pakistan') %>%
  select(-Country) %>%
  na.omit() %>%
  mutate_if(is.factor,as.character) %>%
  mutate(WantWorkLanguage=gsub(" ","",WantWorkLanguage)) %>%
  pull(WantWorkLanguage) %>%
  str_c(.,collapse=';') %>%
  str_split(';') %>%
  unlist() %>%
  table() %>%
  as.data.frame()


colnames(haveWorked)<-c("language","worked")
colnames(haveWant)<-c("language","want")
language_join<-inner_join(haveWant,haveWorked,by="language")
language_join %>% 
  ggplot(aes(y=want,x=worked,color=language))+geom_point()+
  ggrepel::geom_label_repel(aes(label=language))+theme(legend.position = "None")+
  geom_abline(intercept = 45,color="red")+labs(title="Have worked and want to work(Language) in Pakistan")

exportJson2<-language_join %>% toJSON(pretty = T)

write(exportJson2,"Relaton_between_haveWorked_and_WantToWork_with_Languages_in_pakistan.json")


# Relation between job stasifaction and working years in  (tested)

career_satisfied<-so_data %>% 
  select(YearsCodedJob,JobSatisfaction,Country) %>%
  filter(Country=='Pakistan') %>%
  mutate(YearsCodedJob = as.character(YearsCodedJob)) %>%
  na.omit() %>%
  mutate(YearsCodedJob=ifelse(test=YearsCodedJob == 'Less than a year',yes="1 year",no=YearsCodedJob))

  career_satisfied$YearsCodedJob %<>% 
    str_sub(1,2) %>% 
    str_trim() %>% 
    as.integer()
  
  career_satisfied %>%
  arrange(YearsCodedJob) %>% 
  group_by(YearsCodedJob) %>% 
  summarise(JobSatisfaction=mean(JobSatisfaction)) %>% 
  ggplot(aes(x=YearsCodedJob,y=JobSatisfaction))+
  geom_point()+
  geom_smooth(level=0,method = 'loess')+
  labs(title="The relation between jobsatisfaction and working years in Pakistan")
  
  
  df = career_satisfied %>%
  arrange(YearsCodedJob) %>% 
  group_by(YearsCodedJob) %>% 
  summarise(JobSatisfaction=mean(JobSatisfaction))
    
  
  exportJson = df %>% toJSON(pretty = T)
  write(exportJson, "Pak_JobSatisfaction.json")
  
  

# Relation between Learning New Tech and working years in Pakistan (doubtful)
  
  
 learningnew_tech<-so_data %>% 
  select(YearsCodedJob,LearningNewTech,Country) %>%
  filter(Country=='Pakistan') %>%
  mutate(YearsCodedJob = as.character(YearsCodedJob)) %>%
  na.omit() %>%
  mutate(YearsCodedJob=ifelse(test=YearsCodedJob == 'Less than a year',yes="1 year",no=YearsCodedJob)) %>%
  mutate(LearningNewTech=ifelse((LearningNewTech=='Agree' | LearningNewTech=='Strongly agree' |  LearningNewTech=='Somewhat agree'),yes=1,no=0))
 
 
  learningnew_tech$YearsCodedJob %<>% 
    str_sub(1,2) %>% 
    str_trim() %>% 
    as.integer()
  
  
  learningnew_tech %>%
  arrange(YearsCodedJob) %>% 
  group_by(YearsCodedJob) %>% 
  summarise(learning_new_tech=mean(LearningNewTech)) %>% 
  ggplot(aes(x=YearsCodedJob,y=learning_new_tech))+
  geom_point()+
  geom_smooth(level=0,method = 'loess')+
  labs(title="The relation between Learning New Tech  and working years in Pakistan")
  
  df2 =  learningnew_tech %>%
  arrange(YearsCodedJob) %>% 
  group_by(YearsCodedJob) %>% 
  summarise(learning_new_tech=sum(LearningNewTech))
  
  exportJson2 = df2 %>% toJSON(pretty = T)
  write(exportJson2, "Pak_LearningNewTech.json")
  
# Most Famous Language among Students in Pakistan  (tested)
  
  language_students=so_data %>% 
    filter(Country=='Pakistan') %>%
    filter(Professional=='Student') %>%
    select(Professional,HaveWorkedLanguage) %>%
    na.omit(.) %>%
  mutate_if(is.factor,as.character) %>%
 mutate(HaveWorkedLanguage=gsub(" ","",HaveWorkedLanguage)) %>%
  pull(HaveWorkedLanguage) %>%
  str_c(.,collapse=';') %>%
  str_split(';') %>%
  unlist() %>%
  table() %>%
  as.data.frame() 
  
  colnames(language_students)<-c("language","count")
  language_students = language_students %>%
    mutate(language=as.character(language))
  
  
    ggplot(data=language_students,aes(x=language_students$language,y=language_students$count))+
 geom_bar(stat="identity") +
  xlab("Developer type") +
  ylab("Number of Students") +
  theme() + 
  labs(title="Popular Languages among Students in Pakistan ")
    
    exportJson2<-language_students %>% toJSON(pretty = T)
    write(exportJson2, "Languages_used_by_students_in_pakistan.json")
    
    
# Female friendly languages  in Pakistan (tested)
    
    language <- so_data %>% 
      filter(grepl("Female", Gender)) %>%
      filter(Country=='Pakistan') %>%
      select(Gender,HaveWorkedLanguage) %>%
      na.omit() %>%
      mutate_if(is.factor,as.character) %>%
 mutate(HaveWorkedLanguage=gsub(" ","",HaveWorkedLanguage)) %>%
  pull(HaveWorkedLanguage) %>%
  str_c(.,collapse=';') %>%
  str_split(';') %>%
  unlist() %>%
  table() %>%
  as.data.frame() 
  
  colnames(language)<-c("language","count")
  
  female_language = language %>%
    mutate(language=as.character(language))
  group_by(language) %>% 
      summarise(Total = round(n()))
  
female_language$language <- factor(female_language$language, levels = female_language$language)  # convert to factor to retain sorted order in plot.

exportJson2<-female_language %>% toJSON(pretty = T)

write(exportJson2, "Lamguages_used_by_females_in_pakistan.json")

ggplot(female_language, aes(x = language  , y = count,fill = language )) + 
    geom_bar(width = 0.85, stat="identity") +
    coord_polar(theta = "y") +    
    xlab("") + ylab("") +
    ylim(c(0,30)) + 
    geom_text(data = female_language, hjust = 1, size = 3, aes(x = language, y = 0, label = language )) +
    theme(legend.position = "right", axis.text.y = element_blank() , axis.ticks = element_blank()) + 
   labs(title="Most used languages by females in Pakistan")


# Unemployment ratio of each developer type in Pakistan and rest of the world (tested)

#note express unemployment count as % since count of develoepr type varies according to each country 

count_dev<-so_data%>%select(Country,DeveloperType) %>%
  filter(Country=='Pakistan' | Country=='China' | Country == 'United States' | Country == 'India') %>%
  na.omit(.) %>%
  mutate_if(is.factor,as.character) %>%
  mutate(DeveloperType=gsub(" ","",DeveloperType)) %>%
  mutate(DeveloperType = strsplit(as.character(DeveloperType), ";")) %>%
  unnest(DeveloperType) %>%
  mutate(DeveloperType=trimws(DeveloperType)) %>%
  gather(Country, DeveloperType) %>% 
  group_by(Country,DeveloperType) %>% 
  filter(DeveloperType!='Other') %>% 
  summarise(Total_Count= n())


unemployment_dev<-so_data%>%select(Country,DeveloperType,EmploymentStatus) %>%
  filter((Country=='Pakistan' | Country=='China' | Country == 'United States' | Country == 'India') & (EmploymentStatus=='Not employed, and not looking for work' | EmploymentStatus=='Not employed, but looking for work')) %>%
  na.omit(.) %>%
  mutate_if(is.factor,as.character) %>%
  mutate(DeveloperType=gsub(" ","",DeveloperType)) %>%
  mutate(DeveloperType = strsplit(as.character(DeveloperType), ";")) %>%
  unnest(DeveloperType) %>%
  mutate(DeveloperType=trimws(DeveloperType)) %>%
  mutate(EmploymentStatus=ifelse(EmploymentStatus=='Not employed, and not looking for work' | EmploymentStatus=='Not employed, but looking for work',yes='unemployed',.)) %>%
  select(-EmploymentStatus) %>%
  group_by(Country,DeveloperType) %>% 
  filter(DeveloperType!='Other') %>% 
  summarise(Unemployment_count= n())

unemployment <- count_dev %>% 
  left_join(id=c('Country','DeveloperType'),unemployment_dev) 
  
unemployment[is.na(unemployment)] <- 0




# The dataframe is prepared just need tp plot here 

  setDT(unemployment)  
  unemployment_melt=melt(unemployment)
 
  ggplot(data=unemployment_melt, aes(x=unemployment_melt$DeveloperType, y = unemployment_melt$value,fill=unemployment_melt$variable))+
  theme(axis.text.x = element_text(face="bold", color="#993333",size=8, angle=90)) +
  ggtitle("Unemployment among Developers across the world") +
  geom_bar(stat="identity") +
  xlab("Developer type") +
  ylab("Number of Respondents") +
  theme()  
  
  
  
  
  
  
  

  # Find Gender pay gap in countries with most number of respondents  (test it)
  
  # total count of developers in each country  (only male and female)
  so_data %>%
    group_by(Country) %>%
    summarise(Count=n()) %>%
    arrange(desc(Count)) %>%
    head(20) -> top_countries
  
  top_20C <- top_countries %>% select(Country) %>% pull(.)
  # total count of male/female developers in each country 
  
  
country_mf <-  so_data %>%
    filter(Country %in% top_countries$Country) %>%
    group_by(Country) %>%
    summarise(Males=sum(!is.na(Gender) & Gender == "Male"),
              Females=sum(!is.na(Gender) & Gender == "Female")) %>%
              mutate(Total=as.integer(Males) + as.integer(Females))
  
 # Taking top 20 countries which have most no of respondents (both make and female)
  

    
      pay_gap <- so_data %>%  
    select(Country,Gender,Salary) %>%
   na.omit(.) %>%
   filter(Country %in% top_20C &(Gender=='Male' | Gender=='Female')) %>%
        mutate(Country=as.character(Country)) %>%
        mutate(Gender=as.character(Gender))  %>%
      group_by(Gender,Country) %>% summarise_all(funs(mean))
      
      pay_gapDt<-as.data.table(pay_gap)  
      setkey(pay_gapDt,Country)
      pay_gapDt<-pay_gapDt %>% select(Country,Salary)
      pay_gapDt[ , Pay_Gap := c(NA, abs(diff(Salary))), by = Country]   
      pay_gapDt <- pay_gapDt %>% select(Country,Pay_Gap) %>% na.omit(.) %>% arrange(desc(Pay_Gap))      
      
      genderPayGap <- pay_gapDt %>% inner_join(country_mf) %>% 
        mutate(Ratio_of_female_devs_percentage=as.double(Females/Total*100))
    
      exportJson2<-genderPayGap %>%
      select(Country,Males,Females,Ratio_of_female_devs_percentage) %>% 
        toJSON(pretty = T)
    
      
    write(exportJson2, "%_Female_dev_in_countries_with_highest_no_of_respondents.json")
    
    exportJson2<-genderPayGap %>%
      select(Country,Males,Females,Pay_Gap) %>% 
      toJSON(pretty = T)
    
    write(exportJson2, "Avg_pay_gap_in_countries_with_highest_no_of_respondents.json")
    
    # draw two maps here 
      # one showing gender equaltiy in work force 
      # other showing gender pay gap 
      
    
