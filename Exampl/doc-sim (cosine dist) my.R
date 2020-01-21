################################################
### COSINE SIMILARITY (TF | BINARY | TF-IDF) ###
################################################

rm(list = ls())

# Constants
PATH = "C:/Users/Tifani/Documents/GitHub/Document-Similarity/Document Similarity"
CORPUS_TEST_PATH = "C:/Users/Tifani/Documents/GitHub/Document-Similarity/Document Similarity/corpus/test"
BOOKSUM_ALL = "C:/Users/Tifani/Documents/GitHub/Document-Similarity/Document Similarity/corpus/book/"
MIN_FREQ_PLOT = 50
MIN_FREQ_CLOUD = 25

# Initialization
setwd(PATH)
for (package in c('lsa', 'tm', 'SnowballC', 'slam', 'xlsx')) {
    if (!require(package, character.only=T, quietly=T)) {
        install.packages(package)
        library(package, character.only=T)
    }
}

# Pre-processing Corpus
corpus <- Corpus(DirSource(BOOKSUM_ALL))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, stemDocument)

# Creating Term Document Matrix
tdm.tf <- TermDocumentMatrix(myCorpus)
tdm.bin <- weightBin(tdm.tf)
tdm.tfidf <- weightTfIdf(tdm.tf, normalize = TRUE)

# Calculate Cosine Distance (Normal)
library(slam)
cosine.dist.bin <- crossprod_simple_triplet_matrix(tdm.bin)/(sqrt(col_sums(tdm.bin^2) %*% t(col_sums(tdm.bin^2))))
cosine.dist.tf <- crossprod_simple_triplet_matrix(tdm.tf)/(sqrt(col_sums(tdm.tf^2) %*% t(col_sums(tdm.tf^2))))
cosine.dist.tfidf <- crossprod_simple_triplet_matrix(tdm.tfidf)/(sqrt(col_sums(tdm.tfidf^2) %*% t(col_sums(tdm.tfidf^2))))

# Calculate Cosine Distance (LSA)
lsa.space.bin <- lsa(tdm.bin)
lsa.space.tf <- lsa(tdm.tf)
lsa.space.tfidf <- lsa(tdm.tfidf)

lsa.mat.bin <- diag(lsa.space.bin$sk) %*% t(lsa.space.bin$dk)
lsa.mat.tf <- diag(lsa.space.tf$sk) %*% t(lsa.space.tf$dk)
lsa.mat.tfidf <- diag(lsa.space.tfidf$sk) %*% t(lsa.space.tfidf$dk)

lsa.cosine.dist.bin <- lsa::cosine(lsa.mat.bin)
lsa.cosine.dist.tf <- lsa::cosine(lsa.mat.tf)
lsa.cosine.dist.tfidf <- lsa::cosine(lsa.mat.tfidf)

# Print Result
cosine.dist.bin
lsa.cosine.dist.bin
cosine.dist.tf
lsa.cosine.dist.tf
cosine.dist.tfidf
lsa.cosine.dist.tfidf

corrplot(lsa.cosine.dist.tfidf)

# Print Result To Excel
m.vector.space <- as.matrix(tdm.tf)
m.bin <- as.matrix(cosine.dist.bin)   
m.tf <- as.matrix(cosine.dist.tf)   
m.tfidf <- as.matrix(cosine.dist.tfidf)
lsa.m.bin <- as.matrix(lsa.cosine.dist.bin)   
lsa.m.tf <- as.matrix(lsa.cosine.dist.tf)   
lsa.m.tfidf <- as.matrix(lsa.cosine.dist.tfidf)
# (need more memory) write.xlsx(m.vector.space, file="doc_sim.xlsx", sheetName="vector_space")
write.xlsx(m.bin, file="doc_sim (cosine).xlsx", sheetName="binary")
write.xlsx(lsa.m.bin, file="doc_sim (cosine).xlsx", sheetName="lsa_binary", append=TRUE)
write.xlsx(m.tf, file="doc_sim (cosine).xlsx", sheetName="tf", append=TRUE)
write.xlsx(lsa.m.tf, file="d	oc_sim (cosine).xlsx", sheetName="lsa_tf", append=TRUE)
write.xlsx(m.tfidf, file="doc_sim (cosine).xlsx", sheetName="tf-idf", append=TRUE)
write.xlsx(lsa.m.tfidf, file="doc_sim (cosine).xlsx", sheetName="lsa_tf-idf", append=TRUE)

# Word Frequency
dtm <- DocumentTermMatrix(corpus)
freq <- colSums(as.matrix(dtm))
freq <- sort(colSums(as.matrix(dtm)), decreasing=TRUE)    
wordfreq <- data.frame(freq=freq)   
head(wordfreq, 10) 

# Plot Word Frequencies
library(ggplot2)
wf <- data.frame(word=names(freq), freq=freq)      
p <- ggplot(subset(wf, freq>MIN_FREQ_PLOT), aes(word, freq))  
p <- p + geom_bar(stat="identity")   
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))   
p

# Wordcloud
library(wordcloud)   
set.seed(142)   
