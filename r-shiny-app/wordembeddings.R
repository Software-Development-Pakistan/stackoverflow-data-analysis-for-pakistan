concat_features=so_data %>% 
  select(IDE,HaveWorkedLanguage,Country,HaveWorkedDatabase,HaveWorkedFramework,HaveWorkedPlatform,WantWorkLanguage,WantWorkFramework,WantWorkDatabase,WantWorkPlatform) %>%
  filter(Country=='Pakistan') %>%
  select(-Country) %>%
  na.omit(.) %>%
  mutate_if(is.factor,as.character) %>%
  mutate(IDE=paste(IDE,";",sep="")) %>%
  mutate(HaveWorkedLanguage=paste(HaveWorkedLanguage,";",sep="")) %>%
  mutate(HaveWorkedDatabase=paste(HaveWorkedDatabase,";",sep="")) %>%
  mutate(HaveWorkedFramework=paste(HaveWorkedFramework,";",sep="")) %>%
  mutate(WantWorkLanguage=paste(WantWorkLanguage,";",sep="")) %>%
  mutate(WantWorkDatabase=paste(WantWorkDatabase,";",sep="")) %>%
  mutate(WantWorkFramework=paste(WantWorkFramework,";",sep="")) %>%
  mutate(concat_have=paste(IDE,HaveWorkedLanguage,HaveWorkedDatabase,HaveWorkedFramework,HaveWorkedPlatform)) %>%
  mutate(concat_have=gsub(" ","",concat_have)) %>%
  mutate(concat_want=paste(IDE,WantWorkLanguage,WantWorkDatabase,WantWorkFramework,WantWorkPlatform)) %>%
  mutate(concat_want=gsub(" ","",concat_want))

  vectorAllWant=concat_features %>%
    pull(concat_want)
  
  vectorAllHave=concat_features %>%
    pull(concat_have)
  
  embeddingAllWant=unlist(strsplit(as.character(vectorAllWant),';'))
  embeddingAllHave=unlist(strsplit(as.character(vectorAllHave),';'))

# embedding is a character array of all the features. We will vectorize them using word2vec
# and tsne will be used for dimensionality reduction 

 #word2vec=read.binary.vectors(filename='/home/mustufain/Downloads/GoogleNews-vectors-negative300.bin')
 