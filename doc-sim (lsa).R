####################################################
### COSINE SIMILARITY (LATENT SEMANTIC ANALYSIS) ###
####################################################

rm(list = ls())

# Constants
PATH = "C:/Users/Tifani/Documents/GitHub/Document-Similarity/Document Similarity"
CORPUS_TEST_PATH = "C:/Users/Tifani/Documents/GitHub/Document-Similarity/Document Similarity/corpus/test"
BOOKSUM_ALL = "C:/Users/Tifani/Documents/GitHub/Document-Similarity/Document Similarity/corpus/book/"

# Initialization
setwd(PATH)
for (package in c('lsa', 'tm', 'SnowballC', 'slam', 'xlsx')) {
    if (!require(package, character.only=T, quietly=T)) {
        install.packages(package)
        library(package, character.only=T)
    }
}

# Pre-processing Corpus
corpus <- Corpus(DirSource(CORPUS_TEST_PATH))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, stemDocument)

# Creating Term Document Matrix
tdm <- TermDocumentMatrix(corpus)
lsaSpace <- lsa(tdm)
# lsaMatrix now is a k x (num doc) matrix, in k-dimensional LSA space
lsaMatrix <- diag(lsaSpace$sk) %*% t(lsaSpace$dk)
# Use the `cosine` function in `lsa` package to get cosine similarities matrix
distMatrix <- cosine(lsaMatrix)
