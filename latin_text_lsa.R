setwd("D:/GitHub/Latin_Text_LSA/")

# load required libraries

library(tm)
library(udpipe)
library(lsa)
library(ggplot2)
library(scatterplot3d)
library(corrplot)

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
load("historia_annotated_dataset.Rda")


## Get a data.frame with 1 row per doc_id/lemma or specific POS tag

#dtf <- document_term_frequencies(x[, c("doc_id", "lemma")])

dtf <- subset(x, upos %in% c("ADJ", "ADV", "PROPN", "VERB", "NOUN"))

dtf <- document_term_frequencies(dtf, document = "doc_id", term = "lemma")


## Create a document/term/matrix for building a topic model
dtm <- document_term_matrix(x = dtf)
## Remove words which do not occur that much
dtm <- dtm_remove_lowfreq(dtm, minfreq = 2)
head(dtm_colsums(dtm))


# +xstincum
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



############
# ДО ЭТОГО МЕСТА ПОКА БЕЗ ВОПРОСОВ
############


# Calculate a weighted document-term matrix according to the chosen local and/or global weighting scheme

tdm.tfidf <- lw_tf(td_matrix) * gw_idf(td_matrix) # weighting

#tdm.tfidf <- lw_bintf(td_matrix) * gw_idf(td_matrix) # weighting



# Calculate the latent semantic space for the give document-term matrix and create lsaSpace:
# Created LSA space

lsaSpace <- lsa::lsa(tdm.tfidf, dims=dimcalc_share()) # create LSA space
#lsaSpace <- lsa(td.mat.lsa) # create LSA space

as.textmatrix(lsaSpace)

#lsaMatrix <- as.textmatrix(lsaSpace)

############
# ВРОДЕ ТОЖЕ ПОНЯТНО
############


# Вариант из Mastering Text Mining with R и 
# https://github.com/pmtempone/tec_semantica/blob/627f79c01389a39ba07621c90e695336268e424c/tec_semantica_R/ls.R


dist.mat.lsa <- dist(t(as.textmatrix(lsaSpace))) # compute distance matrix

dist.mat.lsa <- dist(t(lsaMatrix))

dist.mat.lsa # check distance mantrix

# Plot the distance matrix:
### Заработало
fit <- cmdscale(dist.mat.lsa, eig=TRUE, k=2) # Classical (Metric) Multidimensional Scaling

points <- data.frame(x=fit$points[, 1], y=fit$points[, 2])

ggplot(points,aes(x=x, y=y)) + 
  geom_point(data=points,aes(x=x, y=y, color=historia$book)) + 
  geom_text(data=points,aes(x=x, y=y-2, label=row.names(historia)))



#3D plot

library(scatterplot3d)

fit <- cmdscale(dist.mat.lsa, eig=TRUE, k=3)

points <- data.frame(x=fit$points[, 1], y=fit$points[, 2])

colors <- rep(c("blue", "green", "red", "purple", "orange" ))

s3d <- scatterplot3d(fit$points[, 1], fit$points[, 2], fit$points[, 3], color=colors, pch=20, angle = 65, box = FALSE,
                     main=" ", xlab="x", ylab="y", zlab="z", type="h")
legend("top", legend = c("Prologus", "Historia Gothorum", "Recapitulatio", "Historia Wandalorum", "Historia Suevorum"),
       col =  c("blue", "green", "red", "purple", "orange"), pch = 16, bty = "n", bg = "transparent",
       inset = -0.1, xpd = TRUE)

s3d$points3d(seq(0,0,0), seq(0,0,0), seq(0,0,0), col="red", type="h", pch=8)








#COSINE similarity
# ЭТО ВРОДЕ РАБОТАЕТ
# compute cosine distance matrix
# ЭТО МОЁ ТВОРЧЕСТВО

lsaMatrix <- as.textmatrix(lsaSpace)

#mat.lsa.cosine <- cosine(lsaMatrix) #Similarity matrix similarity_matrix

mat.lsa.cosine <- cosine(as.textmatrix(lsaSpace)) #Cosin similarity matrix

mat.lsa.cosine

library(corrplot)
corrplot(mat.lsa.cosine)
corrplot(mat.lsa.cosine, method = "number")

## КОНЕЦ МОЕГО ТВОРЧЕСТВА
############################

lsaMatrix <- as.textmatrix(lsaSpace)

#Calculate similarity of documents in LSA space
cosineSim <- function(x){
  as.dist(x%*%t(x)/(sqrt(rowSums(x^2) %*% t(rowSums(x^2)))))
}

#Similarity matrix
cs.lsa <- as.matrix(cosineSim(t(lsaMatrix)))
write.csv(cs.lsa,"cs_lsa.csv")

library(corrplot)
corrplot(cs.lsa)
corrplot(cs.lsa, method = "square")





dist.mat.lsa.cosine <- dist(cosine(as.textmatrix(lsaSpace))) # compute cosine distance matrix
dist.mat.lsa.cosine <- dist(mat.lsa.cosine) # compute cosine distance matrix
dist.mat.lsa.cosine

# Plot the distance matrix:

fit <- cmdscale(dist.mat.lsa.cosine, eig=TRUE, k=2) # Classical (Metric) Multidimensional Scaling

points <- data.frame(x=fit$points[, 1], y=fit$points[, 2])

ggplot(points,aes(x=x, y=y)) + 
  geom_point(data=points,aes(x=x, y=y, color=historia$book)) + 
  geom_text(data=points,aes(x=x, y=y-0.03, label=row.names(historia)))

#3D plot
# Info:
# http://www.sthda.com/english/wiki/scatterplot3d-3d-graphics-r-software-and-data-visualization

library(scatterplot3d)

fit <- cmdscale(dist.mat.lsa.cosine, eig=TRUE, k=3)

points <- data.frame(x=fit$points[, 1], y=fit$points[, 2])

colors <- rep(c("blue", "green", "red", "purple", "orange" ))

s3d <- scatterplot3d(fit$points[, 1], fit$points[, 2], fit$points[, 3], color=colors, pch=20, angle = 65, box = FALSE,
                     main=" ", xlab="x", ylab="y", zlab="z", type="h")
legend("top", legend = c("Prologus", "Historia Gothorum", "Recapitulatio", "Historia Wandalorum", "Historia Suevorum"),
       col =  c("blue", "green", "red", "purple", "orange"), pch = 16, bty = "n", bg = "transparent",
       inset = -0.25, xpd = TRUE)







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







#COSINE similarity
# ЭТО ВРОДЕ РАБОТАЕТ
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
cosine.lsa <- as.matrix(cosine(t(lsaMatrix)))
#write.csv(cs.lsa,"cs_lsa.csv")

library(corrplot)
corrplot(cosine.lsa)
corrplot(cosine.lsa, method = "number")
corrplot(cosine.lsa, method = "color")
corrplot(cosine.lsa, method = "square")

corrplot(distMatrix, method = "square")

corrplot(lsa.cosine.dist.tfidf, method = "square")
















#COSINE similarity
#https://github.com/tifaniwarnita/Document-Similarity/blob/97939d7733965ff322682e98850e426858588357/Document%20Similarity/doc-sim%20(cosine%20dist).R

# Creating Term Document Matrix
tdm.tf <- TermDocumentMatrix(myCorpus)
tdm.bin <- weightBin(tdm.tf)
tdm.tfidf <- weightTfIdf(tdm.tf, normalize = TRUE)


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

lsa.cosine.dist.bin

lsa.cosine.dist.tf

lsa.cosine.dist.tfidf







#COSINE similarity

# compute cosine distance matrix

dist.mat.lsa.cosine <- dist(cosine(as.textmatrix(lsaSpace)))

# Plot the distance matrix:

fit <- cmdscale(dist.mat.lsa.cosine, eig=TRUE, k=2) # Classical (Metric) Multidimensional Scaling

points <- data.frame(x=fit$points[, 1], y=fit$points[, 2])

ggplot(points,aes(x=x, y=y)) + 
  geom_point(data=points,aes(x=x, y=y, color=historia$book)) + 
  geom_text(data=points,aes(x=x, y=y-0.1, label=row.names(historia)))

#3D plot

library(scatterplot3d)

fit <- cmdscale(dist.mat.lsa.cosine, eig=TRUE, k=3)

points <- data.frame(x=fit$points[, 1], y=fit$points[, 2])

colors <- rep(c("blue", "green", "red", "purple", "orange" ))

s3d <- scatterplot3d(fit$points[, 1], fit$points[, 2], fit$points[, 3], color=colors, pch=20, angle = 65, box = FALSE,
                     main=" ", xlab="x", ylab="y", zlab="z", type="h")
legend("top", legend = c("Prologus", "Historia Gothorum", "Recapitulatio", "Historia Wandalorum", "Historia Suevorum"),
       col =  c("blue", "green", "red", "purple", "orange"), pch = 16, bty = "n", bg = "transparent",
       inset = -0.25, xpd = TRUE)

