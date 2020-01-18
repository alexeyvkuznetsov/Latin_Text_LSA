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




####################
####################



# 1 Вариант из статьи: A Guide to Text Analysis with Latent Semantic Analysis in R with Annotated Code: Studying Online Reviews and the Stack Exchange Community

# This command will show the value-weighted matrix of Terms
tk2 = t(lsaSpace$sk * t(lsaSpace$tk))
tk2

# This will show the matrix of Documents
# This command will show the value-weighted matrix of Documents
dk2 = t(lsaSpace$sk * t(lsaSpace$dk))
dk2


# Because the $sk matrix only has values on the diagonal, R stores it as a numeric vector.
lsaSpace$sk

#As a footnote to the above analysis, had we forced three factors on the SVD by specifying dims=3 rather
#than the default, then the $tk matrix could have been more revealing.
lsaSpace3 <- lsa(tdmtfidf, dims=3)
tk3 = t(lsaSpace3$sk * t(lsaSpace3$tk))
tk3


# The two lines of code must be run together. The first line of code creates a plot of the first two 
# dimensions of $tk, marking the dots as red dots. The second line superimposes term names. 
plot(tk2[,1], y= tk2[,2], col="red", cex=.50, main="TK Plot")
text(tk2[,1], y= tk2[,2], labels=rownames(tk2) , cex=.70)
# This can be done with the documents too. The added parameter cex determines text size. 
plot(dk2[,1], y= dk2[,2], col="blue", pch=" ", main="Семантическая схожесть документов")
text(dk2[,1], y= dk2[,2], col="red", labels=rownames(dk2), cex=1.5)




# Вариант 2 из Mastering Text Mining with R и 
# https://github.com/pmtempone/tec_semantica/blob/627f79c01389a39ba07621c90e695336268e424c/tec_semantica_R/ls.R


dist.mat.lsa <- dist(t(as.textmatrix(lsaSpace))) # compute distance matrix

dist.mat.lsa # check distance mantrix

# Plot the distance matrix:
### Заработало
fit <- cmdscale(dist.mat.lsa, eig=TRUE, k=2) # Classical (Metric) Multidimensional Scaling

points <- data.frame(x=fit$points[, 1], y=fit$points[, 2])

ggplot(points,aes(x=x, y=y)) + 
  geom_point(data=points,aes(x=x, y=y, color=historia$book)) + 
  geom_text(data=points,aes(x=x, y=y-0.6, label=row.names(historia)))



library(scatterplot3d)
fit <- cmdscale(dist.mat.lsa, eig=TRUE, k=3)
colors <- rep(c("blue", "green", "red", "purple", "yellow" ))
scatterplot3d(fit$points[, 1], fit$points[, 2], fit$points[, 3], color=colors, pch=20, 
              main="Semantic Space Scaled to 3D", xlab="x", ylab="y", zlab="z", type="h")


###

# Тоже работает Большие точки

fit<- cmdscale(dist.mat.lsa, eig = TRUE, k=2)

points <- data.frame(x = fit$points[, 1], y = fit$points[, 2])

ggplot(points, aes(x = x, y = y)) +
  geom_point(data = points, aes(x = x, y = y, size=5, color = historia$book)) +
  geom_text(data = points, aes(x = x, y = y, label = row.names(historia)))

#####


###???

# compute cosine distance matrix
dist.mat.lsa.cosine <- dist(cosine(as.textmatrix(lsaSpace)))

# Plot the distance matrix:

fit <- cmdscale(dist.mat.lsa.cosine, eig=TRUE, k=2) # Classical (Metric) Multidimensional Scaling

points <- data.frame(x=fit$points[, 1], y=fit$points[, 2])

ggplot(points,aes(x=x, y=y)) + 
  geom_point(data=points,aes(x=x, y=y, color=historia$book)) + 
  geom_text(data=points,aes(x=x, y=y-0.1, label=row.names(historia)))

###




# 3 вариант

# https://github.com/Sathiyarajan/data-science-repo-r-py/blob/9a76df4b28f328e7fb43ecd66876e0d834a6d8ac/R/Mastering-R-Programming/Codes/Section%207/7.3.R

# Run LSA
lsa_out = lsa::lsa(tdm.tfidf, dims=lsa::dimcalc_share())
lsa_out

# docs_df
docs_mat <- lsa_out$dk[, c(1:2)]
plotmat_docs_df <- as.data.frame(docs_mat)
colnames(plotmat_docs_df) <- c("Dim1", "Dim2")

# k-means cluster the docs dataframe.
set.seed(101)
clus <- kmeans(plotmat_docs_df, 3)
plotmat_docs_df$cluster <- factor(clus$cluster)


# plot documents in ggplot2
# devtools::install_github("slowkow/ggrepel@0.6.2")
library(ggplot2)
library(ggrepel)
ggplot2::ggplot(plotmat_docs_df, aes(x=Dim1, y=Dim2)) +
  ggplot2::geom_point(size=2, aes(color=cluster)) +
  ggrepel::geom_text_repel(aes(label = rownames(plotmat_docs_df)), 
                           data = plotmat_docs_df, size=3) + 
  ggplot2::theme_bw()



# https://joparga3.github.io/Udemy_text_analysis/#latent-semantic-analysis

# RUN LSA
lsa_out = lsa::lsa(tdm.tfidf, dims = lsa::dimcalc_share())


# reduced information for the terms
#lsa_out$tk[1:5,]


# reduced information for the documents
#rownames(lsa_out$dk) = n
#lsa_out$dk


# information contributed by the dimensions
#lsa_out$sk



# Using TK and DK to cluster the documents

# DOCS data.frame
docs_mat = lsa_out$dk[,c(1:2)]
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































#### Это работает но убого на вид

fit <- cmdscale(dist.mat.lsa, eig=TRUE, k=2)
points <- data.frame(x=fit$points[, 1], y=fit$points[, 2])

ggplot(points, aes(x=x, y=y)) + 
  geom_point(data=points, aes(x=x, y=y))

####


####################################
##COSINE DISTANCE
#https://stackoverflow.com/questions/15229584/compute-cosine-similarities-between-documents-in-semantic-space-using-r-lsa-pac


# Calculate Cosine Distance (LSA)
# compute distance matrix

lsa.mat.tfidf <- diag(lsaSpace$sk) %*% t(lsaSpace$dk)

#lsaMatrix <- diag(lsaSpace$sk) %*% t(lsaSpace$dk)

# Use the `cosine` function in `lsa` package to get cosine similarities matrix
# (subtract from 1 to get dissimilarity matrix)

fit <- lsa::cosine(lsa.mat.tfidf)

#SM <- as.matrix(cosine(lsaMatrix))

points <- data.frame(x=fit$points[, 1], y=fit$points[, 2])

#points <- data.frame(x=SM$points[, 1], y=SM$points[, 2])


ggplot(points,aes(x=x, y=y)) + 
  geom_point(data=points,aes(x=x, y=y, color=historia$book)) + 
  geom_text(data=points,aes(x=x, y=y-0.05, label=row.names(historia)))

###################################################




#lsaSpace <- lsa(tdm)
# lsaMatrix now is a k x (num doc) matrix, in k-dimensional LSA space
lsaMatrix <- diag(lsaSpace$sk) %*% t(lsaSpace$dk)
# Use the `cosine` function in `lsa` package to get cosine similarities matrix
distMatrix <- cosine(lsaMatrix)
distMatrix2 <- 1 - cosine(lsaMatrix)



#https://github.com/katyalrajat/corpus_mining/blob/9b805cd229b2f5260bfa1007765b0aa6c992fc8d/code.R
############################ LSA ##############################
###############################################################
tdm <- TermDocumentMatrix(docs)
tdm.matrix <- as.matrix(tdm)
#check class
dim(tdm.matrix)

#weight terms and docs
tdm.matrix.lsa <- lw_tf(tdm.matrix) * gw_idf(tdm.matrix)
dim(tdm.matrix.lsa)

#compute the Latent semantic space
lsaSpace <- lsa(tdm.matrix.lsa, dimcalc_share()) # create LSA space
#examine output
names(lsaSpace)

LSAMat <- as.textmatrix(lsaSpace)

#Calculate similarity of documents in LSA space
cosineSim <- function(x){
  as.dist(x%*%t(x)/(sqrt(rowSums(x^2) %*% t(rowSums(x^2)))))
}

#Similarity matrix
#cs.lsa <- as.matrix(lsa::cosine(t(LSAMat)))

cs.lsa <- as.matrix(cosineSim(t(LSAMat)))
write.csv(cs.lsa,"cs_lsa.csv")

library(corrplot)
corrplot(cs.lsa)
corrplot(cs.lsa, method = "square")


##################
##################





lsa.space = lsa(td.mat.tfidf, dims=dimcalc_share())
dist.mat = dist(t(as.textmatrix(lsa.space)))
dist.mat
doc.mds = cmdscale(dist.mat, k=2)
data = data.frame(x = doc.mds[,1], y = doc.mds[,2], topic = historia$book, id = row.names(historia))
ggplot(data, aes(x = x, y = y, color = topic)) + geom_point() + geom_text(aes(x = x, y = y - 0.2, label = row.names(historia)))



las.space = lsa(td.mat.w, dims = 3);
dist.mat = dist(t(as.textmatrix(lsa.space)));
dist.mat;
doc.mds = cmdscale(dist.mat, k=2);
data = data.frame(x = doc.mds[,1], y = doc.mds[,2], topic = df$topic, id = row.names(df));
ggplot(data, aes(x = x, y = y, color = topic)) + geom_point() + geom_text(aes(x = x, y = y - 0.2, label = id))







#https://github.com/pmtempone/tec_semantica/blob/627f79c01389a39ba07621c90e695336268e424c/tec_semantica_R/ls.R

# compute distance matrix
td.mat <- as.matrix(TermDocumentMatrix(corpus))
td.mat.lsa <- lw_bintf(td.mat) * gw_idf(td.mat) # weighting
lsaSpace <- lsa(td.mat.lsa) # create LSA space
dist.mat.lsa <- dist(t(as.textmatrix(lsaSpace))) # compute distance matrix

# MDS
fit <- cmdscale(dist.mat.lsa, eig=TRUE, k=2)
points <- data.frame(x=fit$points[, 1], y=fit$points[, 2])
ggplot(points, aes(x=x, y=y)) + geom_point(data=points, aes(x=x, y=y))

ggplot(points, aes(x=x, y=y)) + geom_point(data=points, aes(x=x, y=y)) + geom_text(data=points,aes(x=x, y=y-0.2, label=row.names(dtf$doc_id)))



#lsa_mds.R
fit <- cmdscale(dist.mat.lsa, eig=TRUE, k=2)
points <- data.frame(x=fit$points[, 1], y=fit$points[, 2])
ggplot(points,aes(x=x, y=y)) + 
  geom_point(data=points,aes(x=x, y=y, color=historia$book)) + 
  geom_text(data=points,aes(x=x, y=y-0.2, label=row.names(historia)))



#https://github.com/ds10/Personal-Corpus/blob/5f009ac29b53a27f69855cdc4b787bf48791b524/R/blog_text_similarity.R

fit <- cmdscale(dist.mat.lsa, eig = TRUE, k = 2)
points <- data.frame(x = fit$points[, 1], y = fit$points[, 2])
ggplot(points, aes(x = x, y = y)) + geom_point(data = points, aes(x = x, y = y, 
                                                                  color = merge.frame$poster)) + geom_text(data = points, aes(x = x, y = y - 0.2, label = row.names(merge.frame)))


fit <- cmdscale(dist.mat.lsa, eig = TRUE, k = 2)
points <- data.frame(x = fit$points[, 1], y = fit$points[, 2])
ggplot(points, aes(x = x, y = y)) + geom_point(data = points, aes(x = x, y = y, 
                                                                  color = merge.frame$poster) )


########################

# 3. MDS with LSA
td.mat.lsa <- lw_bintf(td.mat) * gw_idf(td.mat) # weighting
lsaSpace <- lsa(td.mat.lsa) # create LSA space
dist.mat.lsa <- dist(t(as.textmatrix(lsaSpace))) # compute distance matrix
dist.mat.lsa # check distance mantrix

# MDS
fit <- cmdscale(dist.mat.lsa, eig=TRUE, k=2)
points <- data.frame(x=fit$points[, 1], y=fit$points[, 2])
ggplot(points,aes(x=x, y=y)) + 
  geom_point(data=points,aes(x=x, y=y, color=df$view)) + 
  geom_text(data=points,aes(x=x, y=y-0.2, label=row.names(df)))

library(scatterplot3d)
fit <- cmdscale(dist.mat.lsa, eig=TRUE, k=3)
colors <- rep(c("blue", "green", "red"), each=3)
scatterplot3d(fit$points[, 1], fit$points[, 2], fit$points[, 3], color=colors, pch=16, 
              main="Semantic Space Scaled to 3D", xlab="x", ylab="y", zlab="z", type="h")























############################
# Из Kumar p 133

distnce_matrix_lsa<- dist(t(as.textmatrix(lsaSpace)))
distnce_matrix_lsa

library(ggplot2)

fit<- cmdscale(distnce_matrix_lsa , eig = TRUE, k = 2)

points<- data.frame(x = fit$points[, 1], y = fit$points[, 2])

ggplot(points, aes(x = x, y = y)) + geom_point(data = points, aes(x = x, y = y,size=5, color = IM_DataFrame$view)) + geom_text(data = points, aes(x = x, y = y - 0.2, label = row.names(IM_DataFrame)))



#########################


#Primer
library(ggplot2)
library(scatterplot3d)

#------------------------------------------------------------------------------

# MDS with raw term-document matrix compute distance matrix
td.mat <- as.matrix(TermDocumentMatrix(myCorpus))
td.mat
dist.mat <- dist(t(as.matrix(td.mat)))
dist.mat  # check distance matrix



#------------------------------------------------------------------------------

# MDS
fit <- cmdscale(dist.mat, eig = TRUE, k = 2)
points <- data.frame(x = fit$points[, 1], y = fit$points[, 2])
ggplot(points, aes(x = x, y = y)) + geom_point(data = points, aes(x = x, y = y, color = df$view)) + geom_text(data = points, aes(x = x, y = y - 0.2, label = row.names(df)))




#------------------------------------------------------------------------------

# MDS with LSA
td.mat.lsa <- lw_bintf(td.mat) * gw_idf(td.mat)  # weighting
lsaSpace <- lsa(td.mat.lsa)  # create LSA space
dist.mat.lsa <- dist(t(as.textmatrix(lsaSpace)))  # compute distance matrix
dist.mat.lsa  # check distance mantrix


#------------------------------------------------------------------------------

# MDS
fit <- cmdscale(dist.mat.lsa, eig = TRUE, k = 2)
points <- data.frame(x = fit$points[, 1], y = fit$points[, 2])
ggplot(points, aes(x = x, y = y)) + geom_point(data = points, aes(x = x, y = y, color = df$view)) + geom_text(data = points, aes(x = x, y = y - 0.2, label = row.names(df)))


#------------------------------------------------------------------------------

# plot
fit <- cmdscale(dist.mat.lsa, eig = TRUE, k = 5)
colors <- rep(c("blue"), each = 5)
scatterplot3d(fit$points[, 1], fit$points[, 2], fit$points[, 3], fit$points[, 4], fit$points[, 5],color = colors, pch = 16, main = "Semantic Space Scaled to 3D", xlab = "x", ylab = "y", zlab = "z", type = "h")



















tdm.lsa <- lw_bintf(tdm_matrix)*gw_idf(tdm_matrix)


TDMlsa <- textmatrix(myCorpus)


library(tidytext)

dfm <- tidytext::cast_dfm(dtf, doc_id, term, freq)

tdm <- cast_tdm(dtf, doc_id, term, freq)


# Convert A Dfm To An Lsa "Textmatrix" quanteda
tdmlsa <- convert(dfm, to = "lsa") 

tdmlsa <- as.matrix(tdmlsa)

library(LSAfun)
library(lsa)


tdmtfidf <- lw_tf(dfm) * gw_idf(dfm)
tdmtfidf








#STOPWORDS

customStopWords <- c("ann", "annus", "aer", "aes", "aera", "sunt")

stopwords_latin <-paste0(scan(file ="latin_stopwords.txt",what='character'),collapse=" ")

head(stopwords_latin)

historia$texts <- tokens_remove(historia$texts, stopwords_latin)

lat_stop_perseus <- c("ab", "ac", "ad", "adhic", "aliqui", "aliquis", "an", "ante", "apud", "at", "atque", "aut", "autem", "cum", "cur", "de", "deinde", "dum", "ego", "enim", "ergo", "es", "est", "et", "etiam", "etsi", "ex", "fio", "haud", "hic", "iam", "idem", "igitur", "ille", "in", "infra", "inter", "interim", "ipse", "is", "ita", "magis", "modo", "mox", "nam", "ne", "nec", "necque", "neque", "nisi", "non", "nos", "o", "ob", "per", "possum", "post", "pro", "quae", "quam", "quare", "qui", "quia", "quicumque", "quidem", "quilibet", "quis", "quisnam", "quisquam", "quisque", "quisquis", "quo", "quoniam", "sed", "si", "sic", "sive", "sub", "sui", "sum", "super", "suus", "tam", "tamen", "trans", "tu", "tum", "ubi", "uel", "uero", "unus", "ut")


Mystopwords <- c(lat_stop_perseus, customStopWords, stopwords_latin)



#library(readr)
#stopwords_latin <- read_lines("stopwords_latin.txt", skip_empty_rows = TRUE)

current_latin <- read_lines("stopwords_latin.txt")
current_latin_count <- count_items_in_txt_list("stopwords_latin.txt")


historia$texts <- removeWords(historia$texts, c(stopwords_latin, customStopWords))

historia$texts <- removeWords(historia$texts, testVector)


airbnb$comments <- removeWords(airbnb$comments,c(stopwords('en'),'Boston'))



historia_g <- VectorSource(historia_g)
historia_g <- VCorpus(historia_g)

historia_g <- tm_map(historia_g, removeWords, stopwords_latin)



prologus <- removeWords(prologus, stopwords)

airbnb$comments <- removeWords(airbnb$comments,c(stopwords('en'),'Boston'))

current_latin <- read_lines("latin_stopwords.txt")
current_latin <- utf8::utf8_normalize(current_latin)
write_lines(current_latin, "my_stopwords_latin.txt")

require(stringr)
testVector <- str_split(readLines("latin_stopwords.txt"), pattern = " ")

as.character(testVector)


library(tm)
Mystopwords <- read.csv("latin_stopwords.csv", header = FALSE)
Mystopwords <- as.character(Mystopwords$V1)
stopwords <- c(stopwords, stopwords())

historia$texts

historia$texts <- tm_map(historia$texts, removeWords, stopwords)


MyData <- 1

write.csv(MyData, file = "MyData.csv")

write.csv(MyData, file = "MyData.csv",row.names=FALSE)

historia$text<-tm_map(historia$text,removeWords,c(stopwords_latin, myStopwords))

historia$texts <- removeWords(historia$texts, stopwords_latin)



prologus <- removeWords(prologus, stopwords)

removeWords <- c(myStopwords, stopwords("english"))

line <- readLines("stopwords_latin.txt", n=-1, warn=FALSE, collapse = " ")

stopwords_latin <-paste(scan(file ="stopwords_latin.txt", what=character()), encoding = "UTF-8", collapse=" ")


stopwords_latin <-scan(file ="latin_stopwords.txt",what='character')



sw <- c(stopwords_latin)
sw
class(sw)

