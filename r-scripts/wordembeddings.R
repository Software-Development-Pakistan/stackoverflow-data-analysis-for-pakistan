library(wordcloud)
library(RColorBrewer)
library(tm)
library(Rtsne)
library(text2vec)

# ## gender and developer type 
# 
# genderDev=so_data %>% 
#   select(DeveloperType,Country) %>%
#   filter(Country=='Pakistan') %>%
#   na.omit() %>%
#   mutate(gender_developer=gsub(" ","",DeveloperType)) %>%
#   pull(gender_developer)
#   
# write(genderDev,'gender_dev.txt')
# prep_word2vec(origin='gender_dev.txt',destination = 'gender_dev_vectors.txt',lowercase = T)
# model = train_word2vec("gender_dev_vectors.txt",vectors=100,threads=4,window=5,iter=1000,negative_samples=0,force = T)
# word2vec=model[2:nrow(model),]  # remove at end of string


#tsne (dimensionality reduction)
# reduction <- Rtsne(word2vec, dims = 2, initial_dims = 50,
#                    perplexity = 2, theta = 0.05, check_duplicates = F,
#                    pca = F, max_iter = 1000, verbose = T,
#                    is_distance = F, Y_init = NULL)
# df <- as.data.frame(reduction$Y)
# rows <- rownames(word2vec)
#rownames(df) <- rows
#df=df %>% filter(rownames(df) != '</s>')
#rownames(df) <- rows[2:65]

# Create t-SNE plot and save as jpeg
# ggplot(df) +
#   geom_point(aes(x = V1, y = V2), color = "red") +
#   geom_text_repel(aes(x = V1, y = V2, label = rownames(df))) +
#   xlab("Dimension 1") +
#   ylab("Dimension 2 ") +
#   # geom_text(fontface = 2, alpha = .8) +
#   theme_bw(base_size = 12) +
#   theme(legend.position = "none") +
#   ggtitle(paste0("2D reduction of Word Embedding Model on stack over flow data of pakistan using t_SNE"))
# 
# pca = prcomp(word2vec,scale=T) 
# plot(pca$x, t='n', main="pca")
# text(pca$x, labels=rownames(df))
# x=pca$x[,1]
# y=pca$x[,2]
# 
# hc = hclust(dist(cbind(x,y)), method = 'ward.D')
# plot(hc, axes=F,xlab='', ylab='',sub ='', main='Comp 1/2')
# rect.hclust(hc, k=5, border='red')

######################################
  


concat_features=so_data %>% 
  select(IDE,HaveWorkedLanguage,Country,HaveWorkedDatabase,HaveWorkedFramework,HaveWorkedPlatform,WantWorkLanguage,WantWorkFramework,WantWorkDatabase,WantWorkPlatform) %>%
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
  
  all=c(embeddingAllWant,embeddingAllHave)
  doc=Corpus(VectorSource(all))
  dtm <- TermDocumentMatrix(doc)
  m <- as.matrix(dtm)
  v <- sort(rowSums(m),decreasing=TRUE)
  d <- data.frame(word = names(v),freq=v)
  set.seed(1234)
  wordcloud(words = d$word, freq = d$freq, min.freq = 1,
            max.words=200, random.order=FALSE, rot.per=0.35, 
            colors=brewer.pal(8, "Dark2"))
  
  
  
  write(all,'features.txt')
  write(embeddingAllHave,'features_have.txt')
  # question (should we include all and have as one vector and then form term document matrix ?)
  # embedding is a character array of all the features. We will vectorize them using word2vec
  # and tsne will be used for dimensionality reduction 
  
  # word2vec hyper parameter tuning 
  vectors=seq(100,500)
  min_count=seq(5,10)
  negative_samples=seq(5,15)
  iter=seq(100,1000)
  window=seq(1,20)
  
  
  ##################################
  
  prep_word2vec(origin='features_have.txt',destination = 'vectors.txt',lowercase = T)
  model = train_word2vec("vectors.txt",vectors=300,threads=4,window=5,iter=1000,negative_samples=0,min_count = 10,force = T)
  word2vec=model[2:nrow(model),]  # remove at end of string
  #tsne (dimensionality reduction)
  reduction <- Rtsne(word2vec, dims = 2, initial_dims = 50,
                     perplexity = 10, theta = 0.05, check_duplicates = F,
                     pca = F, max_iter = 1000, verbose = T,
                     is_distance = F, Y_init = NULL)
  df <- as.data.frame(reduction$Y)
  rows <- rownames(word2vec)
  rownames(df) <- rows
  #df=df %>% filter(rownames(df) != '</s>')
  #rownames(df) <- rows[2:65]
 
  # Create t-SNE plot and save as jpeg
  ggplot(df) +
    geom_point(aes(x = V1, y = V2), color = "red") +
    geom_text_repel(aes(x = V1, y = V2, label = rownames(df))) +
    xlab("Dimension 1") +
    ylab("Dimension 2 ") +
    # geom_text(fontface = 2, alpha = .8) +
    theme_bw(base_size = 12) +
    theme(legend.position = "none") +
    ggtitle(paste0("2D reduction of Word Embedding Model on stack over flow data of pakistan using t_SNE"))
  
  
  pca = prcomp(word2vec,scale=T) 
  plot(pca$x, t='n', main="pca")
  text(pca$x, labels=rownames(df))
  x=pca$x[,1]
  y=pca$x[,2]
  

  # hierarchical clustering using pca 
  hc = hclust(dist(cbind(x,y)), method = 'ward.D')
  plot(hc, axes=F,xlab='', ylab='',sub ='', main='Comp 1/2')
  rect.hclust(hc, k=3, border='red')
  
  # hierachical clustering using tsne 
  
  hc = hclust(dist(cbind(xt,yt)), method = 'ward.D')
  plot(hc, axes=F,xlab='', ylab='',sub ='', main='Comp 1/2')
  rect.hclust(hc, k=3, border='red')
  
  
  ggsave('wordembeddings.png')