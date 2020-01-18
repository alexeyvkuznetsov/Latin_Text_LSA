setwd("D:/GitHub/Latin_Text_LSA/")

# load required libraries

library(tm)
library(udpipe)
library(lsa)
library(ggplot2)

#library(readr)
library(quanteda)
library(tidytext)


prologus<-paste(scan(file ="files/01 prologus.txt",what='character'),collapse=" ")
historia_g<-paste(scan(file ="files/02 historia_g.txt",what='character'),collapse=" ")
recapitulatio<-paste(scan(file ="files/03 recapitulatio.txt",what='character'),collapse=" ")
historia_w<-paste(scan(file ="files/04 historia_w.txt",what='character'),collapse=" ")
historia_s<-paste(scan(file ="files/05 historia_s.txt",what='character'),collapse=" ")

prologus<-data.frame(texts=prologus)
historia_g<-data.frame(texts=historia_g)
recapitulatio<-data.frame(texts=recapitulatio)
historia_w<-data.frame(texts=historia_w)
historia_s<-data.frame(texts=historia_s)

prologus$book<-"01 Prologus"
historia_g$book<-"02 Historia Gothorum"
recapitulatio$book<-"03 Recapitulatio"
historia_w$book<-"04 Historia Wandalorum"
historia_s$book<-"05 Historia Suevorum"


historia<-rbind(prologus,historia_g,recapitulatio,historia_w,historia_s)

#historia$texts <- stripWhitespace(historia$texts)
historia$texts <- tolower(historia$texts)
historia$texts <- removePunctuation(historia$texts)
historia$texts <- removeNumbers(historia$texts)

# Stopwords 

customStopWords <- c("ann", "annus", "aer", "aes", "aera", "suus", "filius", "multus", "num._rom.", "xnum._rom.", "xxnum._rom.", "xxxnum._rom.", "cdxlnum._rom.")

#load("lat_stopwords.Rda")

load("lat_stop_perseus.Rda")

MyStopwords <- c(lat_stop_perseus, customStopWords)

historia$texts <- removeWords(historia$texts, c(lat_stopwords, customStopWords))

historia$texts <- removeWords(historia$texts, MyStopwords)


# UDPipe annotation
#udmodel_latin <- udpipe_download_model(language = "latin_ittb")
#udmodel_latin <- udpipe_load_model(ud_model$file_model)
udmodel_latin <- udpipe_load_model(file = "latin-ittb-ud-2.4-190531.udpipe")

x <- udpipe_annotate(udmodel_latin, x = historia$texts, doc_id = historia$book, tagger = "default", parser = "default", trace = TRUE)
x <- as.data.frame(x)

save(x,file="historia_annotated_dataset.Rda")


## Get a data.frame with 1 row per doc_id/lemma or specific POS tag

#dtf <- document_term_frequencies(x[, c("doc_id", "lemma")])

dtf <- subset(x, upos %in% c("ADJ", "ADV", "PROPN", "VERB", "NOUN"))

dtf <- document_term_frequencies(dtf, document = "doc_id", term = "lemma")


## Create a document/term/matrix for building a topic model
dtm <- document_term_matrix(x = dtf)
## Remove words which do not occur that much
dtm <- dtm_remove_lowfreq(dtm, minfreq = 3)
head(dtm_colsums(dtm))

## Remove nouns which you really do not like (mostly too common nouns)
dtm <- dtm_remove_terms(dtm, terms = c("ann", "adipio", "annus", "aer", "aes", "aera", "suus", "filius", "multus", "num._rom.", "xnum._rom.", "xxnum._rom.", "xxxnum._rom.", "cdxlnum._rom."))
## Or keep of these nouns the top 50 based on mean term-frequency-inverse document frequency
#dtm <- dtm_remove_tfidf(dtm, top = 50)



## Convert dtm to a list of text
dtm.to.list <- apply(dtm, 1, function(x) {
  paste(rep(names(x), x), collapse=" ")
})

## convert list of text to a Corpus

myCorpus <- VCorpus(VectorSource(dtm.to.list))
inspect(myCorpus)

# Created tdm

tdm <- TermDocumentMatrix(myCorpus)

td_matrix <- as.matrix(tdm)




# Calculate a weighted document-term matrix according to the chosen local and/or global weighting scheme

tdm.tfidf <- lw_tf(td_matrix) * gw_idf(td_matrix) # weighting

#tdm.tfidf <- lw_bintf(td_matrix) * gw_idf(td_matrix) # weighting





# Вариант взвешивания
# https://github.com/tifaniwarnita/Document-Similarity/blob/97939d7733965ff322682e98850e426858588357/Document%20Similarity/doc-sim%20(cosine%20dist).R
tdm.tfidf <- weightTfIdf(tdm, normalize = TRUE)

tdm.tfidf <- as.matrix(weightTfIdf(tdm, normalize = TRUE))

tdm.tfidf




# Calculate the latent semantic space for the give document-term matrix and create lsaSpace:
# Created LSA space

lsaSpace <- lsa::lsa(tdm.tfidf, dims=dimcalc_share()) # create LSA space
#lsaSpace <- lsa(td.mat.lsa) # create LSA space

as.textmatrix(lsaSpace)




#LSA и кластерный анализ
# https://joparga3.github.io/Udemy_text_analysis/#latent-semantic-analysis
# https://github.com/Sathiyarajan/data-science-repo-r-py/blob/9a76df4b28f328e7fb43ecd66876e0d834a6d8ac/R/Mastering-R-Programming/Codes/Section%207/7.3.R


# RUN LSA
#lsaSpace = lsa::lsa(tdm.tfidf, dims = lsa::dimcalc_share())


# reduced information for the terms
#lsaSpace$tk[1:5,]


# reduced information for the documents
#rownames(lsaSpace$dk) = n
#lsaSpace$dk


# information contributed by the dimensions
#lsaSpace$sk



# Using TK and DK to cluster the documents

# DOCS data.frame
docs_mat = lsaSpace$dk[,c(1:2)]
plotmat_docs_df = as.data.frame(docs_mat)
colnames(plotmat_docs_df) = c("x","y")


# KMEANS to cluster
set.seed(100)
clus = kmeans(plotmat_docs_df, 3)
plotmat_docs_df$cluster = factor(clus$cluster)
plotmat_docs_df


# Plot
library(ggplot2)
library(ggrepel)

g = ggplot(plotmat_docs_df, aes(x = x, y = y))
g = g + geom_point(size = 4, aes(color = cluster))
g = g + ggrepel::geom_text_repel(aes(label = rownames(plotmat_docs_df))
                                 , data = plotmat_docs_df, size = 5)
g = g + theme_bw()
g



#Косинусное сходство
#https://github.com/katyalrajat/corpus_mining/blob/9b805cd229b2f5260bfa1007765b0aa6c992fc8d/code.R
############################ LSA ##############################
###############################################################
tdm <- TermDocumentMatrix(myCorpus)
tdm.matrix <- as.matrix(tdm)
#check class
dim(tdm.matrix)

#weight terms and docs
tdm.tfidf <- lw_tf(tdm.matrix) * gw_idf(tdm.matrix)
dim(tdm.tfidf)

#compute the Latent semantic space
lsaSpace <- lsa(tdm.tfidf, dimcalc_share()) # create LSA space
#examine output
names(lsaSpace)

lsaMatrix <- as.textmatrix(lsaSpace)

#Calculate similarity of documents in LSA space
cosineSim <- function(x){
  as.dist(x%*%t(x)/(sqrt(rowSums(x^2) %*% t(rowSums(x^2)))))
}

#Similarity matrix

cosine.lsa <- as.matrix(cosineSim(t(lsaMatrix)))
#write.csv(cs.lsa,"cs_lsa.csv")

library(corrplot)
corrplot(cosine.lsa)
corrplot(cosine.lsa, method = "square")


