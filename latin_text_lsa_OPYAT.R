setwd("D:/GitHub/Latin_Text_LSA/")

# load required libraries
library(tm)
library(udpipe)
library(lsa)
library(ggplot2)
library(scatterplot3d)
library(corrplot)
#library(readr)
#library(quanteda)
#library(tidytext)

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

prologus$book<-"1 Prologus"
historia_g$book<-"2 Historia Gothorum"
recapitulatio$book<-"3 Recapitulatio"
historia_w$book<-"4 Historia Wandalorum"
historia_s$book<-"5 Historia Suevorum"

historia<-rbind(prologus,historia_g,recapitulatio,historia_w,historia_s)

#historia$texts <- stripWhitespace(historia$texts)
historia$texts <- tolower(historia$texts)
historia$texts <- removePunctuation(historia$texts)
historia$texts <- removeNumbers(historia$texts)

# Stopwords 

customStopWords <- c("ann", "annus", "aer", "aes", "aera", "num._rom.", "xnum._rom.", "xxnum._rom.", "xxxnum._rom.", "cdxlnum._rom.")
lat_stopwords_romnum <- c("i", "ii", "iii", "iiii", "iv", "v", "vii", "viii", "ix", "x", "xi", "xii", "xiii", "xiv", "xv", "xvi", "xvii", "xviii", "xix", "xx", "xxi", "xxii", "xxiii", "xxiv", "xxv", "xxvi", "xxvii", "xxviii", "xxix", "xxx", "xxxi", "xxxii", "xxxiii", "xxxiv", "xxxv", "xxxvi", "xxxvii", "xxxviii", "xxxix", "xl", "xli", "xlii", "xliii", "xliv", "xlv", "xlvi", "xlvii", "xlviii", "xlix", "l", "li", "lii", "liii", "liv", "lv", "lvi", "lvii", "lviii", "lix", "lx", "lxi", "lxii", "lxiii", "lxiv", "lxv", "lxvi", "lxvii", "lxviii", "lxix", "lxx", "lxxi", "lxxii", "lxxiii", "lxxiv", "lxxv", "lxxvi", "lxxvii", "lxxviii", "lxxix", "lxxx", "lxxxi", "lxxxii", "lxxxiii", "lxxxiv", "lxxxv", "lxxxvi", "lxxxvii", "lxxxviii", "lxxxix", "xc", "xci", "xcii", "xciii", "xciv", "xcv", "xcvi", "xcvii", "xcviii", "xcix", "c")
lat_stop_perseus <- c("ab", "ac", "ad", "adhic", "aliqui", "aliquis", "an", "ante", "apud", "at", "atque", "aut", "autem", "cum", "cur", "de", "deinde", "dum", "ego", "enim", "ergo", "es", "est", "et", "etiam", "etsi", "ex", "fio", "haud", "hic", "iam", "idem", "igitur", "ille", "in", "infra", "inter", "interim", "ipse", "is", "ita", "magis", "modo", "mox", "nam", "ne", "nec", "necque", "neque", "nisi", "non", "nos", "o", "ob", "per", "possum", "post", "pro", "quae", "quam", "quare", "qui", "quia", "quicumque", "quidem", "quilibet", "quis", "quisnam", "quisquam", "quisque", "quisquis", "quo", "quoniam", "sed", "si", "sic", "sive", "sub", "sui", "sum", "super", "suus", "tam", "tamen", "trans", "tu", "tum", "ubi", "uel", "uero", "unus", "ut")

#save(lat_stop_perseus,file="lat_stop_perseus.Rda")

load("lat_stop_perseus.Rda")

MyStopwords <- c(lat_stop_perseus, customStopWords, lat_stopwords_romnum)

#historia$texts <- removeWords(historia$texts, c(lat_stopwords, customStopWords))

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
#dtm <- dtm_remove_terms(dtm, terms = c("ann", "adipio", "annus", "aer", "aes", "aera", "suus", "filius", "multus", "num._rom.", "xnum._rom.", "xstincum", "xxnum._rom.", "xxxnum._rom.", "cdxlnum._rom."))

dtm <- dtm_remove_terms(dtm, terms = c("ann", "adipio", "annus", "aer", "aes", "aera", "num._rom.", "xnum._rom.", "xstincum", "xxnum._rom.", "xxxnum._rom.", "cdxlnum._rom.", "cdlxvus", "cdxcnum._rom.", "cdxcus", "cdxix", "cdxlnum._rom.", "cdxlvium", "cdxlvus", "cdxx", "cdxxcvus", "cdxxxnum._rom.", "clxxnum._rom.", "cxiium", "cxx", "dclix", "dcxliix", "dcxlis", "dcxxnum._rom.", "dcxxxix", "dlxnum._rom.", "dlxxxnum._rom.", "dlxxxvus", "dxnum._rom.", "dxxvus"))

## Or keep of these nouns the top 50 based on mean term-frequency-inverse document frequency
#dtm <- dtm_remove_tfidf(dtm, top = 50)

## Convert dtm to a list of text
dtm.to.list <- apply(dtm, 1, function(x) {
  paste(rep(names(x), x), collapse=" ")
})

## convert list of text to a Corpus

myCorpus <- VCorpus(VectorSource(dtm.to.list))
inspect(myCorpus)

# Created term-document matrix

tdm <- TermDocumentMatrix(myCorpus)

td_matrix <- as.matrix(tdm)


###################################################


# Calculate a weighted term-document matrix according to the chosen local and/or global weighting scheme

tdm.tfidf <- lw_tf(td_matrix) * gw_idf(td_matrix) # weighting

#tdm.tfidf <- lw_bintf(td_matrix) * gw_idf(td_matrix) # weighting


# Calculate the latent semantic space for the give document-term matrix and create lsaSpace:
# Created LSA space

lsaSpace <- lsa::lsa(tdm.tfidf, dims=dimcalc_share()) # create LSA space

lsaMatrix <- as.textmatrix(lsaSpace)

############
# Begining
############
############
####
####

# Example from: Mastering Text Mining with R 
# https://github.com/pmtempone/tec_semantica/blob/627f79c01389a39ba07621c90e695336268e424c/tec_semantica_R/ls.R
#Compute distance between documents and scale the multidimentional semantic space (MDS) onto two dimensions

#dist.mat.lsa <- dist(t(as.textmatrix(lsaSpace))) # compute distance matrix

dist.mat.lsa <- dist(t(lsaMatrix))

dist.mat.lsa # check distance mantrix

# Plot the distance matrix:

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
legend("top", legend = c("1 Prologus", "2 Historia Gothorum", "3 Recapitulatio", "4 Historia Wandalorum", "5 Historia Suevorum"),
       col =  c("blue", "green", "red", "purple", "orange"), pch = 16, bty = "n", bg = "transparent",
       inset = 0.2, xpd = TRUE)

s3d$points3d(seq(0,0,0), seq(0,0,0), seq(0,0,0), col="red", type="h", pch=8)

####
####
############
############


#COSINE similarity
# 
# compute cosine distance matrix
# 

#lsaMatrix <- as.textmatrix(lsaSpace)

mat.lsa.cosine <- cosine(lsaMatrix) #Cosin similarity matrix

#mat.lsa.cosine <- cosine(as.textmatrix(lsaSpace)) #Cosin similarity matrix

mat.lsa.cosine

round((mat.lsa.cosine), 2) # round the results to a couple of decimals

mat.lsa.cosine

library(corrplot)
corrplot(mat.lsa.cosine)

corrplot(mat.lsa.cosine, method = "number")

#corrplot(mat.lsa.cosine, method = "number", order = "hclust", hclust.method = "complete")



############################
## END
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
corrplot(cs.lsa, method = "number")

############################
## END 2
############################


#############################################################
##  A Guide to Text Analysis with Latent Semantic          ##
##  Analysis in R with Annotated Code: Studying Online     ##
##  Reviews and the Stack Exchange Community               ##
#############################################################


library(LSAfun)
library(lsa)

###
###
###

tdm <- TermDocumentMatrix(myCorpus)
TDM <- as.matrix(tdm)

TDM <- as.matrix(tdm)

#########################
summary.textmatrix(TDM)

#########################

TDM2 <- lw_tf(TDM) * gw_idf(TDM) 
TDM2

#########################
miniLSAspace <- lsa(TDM2, dims=dimcalc_share()) 
as.textmatrix(miniLSAspace) 

#########################
# This command will show the value-weighted matrix of Terms
tk2 = t(miniLSAspace$sk * t(miniLSAspace$tk))
tk2

#########################
# This command will show the value-weighted matrix of Documents
dk2 = t(miniLSAspace$sk * t(miniLSAspace$dk))
dk2

#########################
# Because the $sk matrix only has values on the diagonal, R stores it as a numeric vector. 
miniLSAspace$sk

#########################
miniLSAspace3 <- lsa(TDM2, dims=3) 
tk3 = t(miniLSAspace3$sk * t(miniLSAspace3$tk)) 
tk3 

#########################
# The two lines of code must be run together. The first line of code creates a plot of the first two 
# dimensions of $tk, marking the dots as red dots. The second line superimposes term names. 
plot(tk2[,1], y= tk2[,2], col="red", cex=.50, main="TK Plot")
text(tk2[,1], y= tk2[,2], labels=rownames(tk2) , cex=.70)
# This can be done with the documents too. The added parameter cex determines text size. 
plot(dk2[,1], y= dk2[,2], col="blue", pch="+", main="DK Plot")
text(dk2[,1], y= dk2[,2], labels=rownames(dk2), cex=.70)




#########################
myDocs <- rownames(dk2)
myDocs

#########################
# This provides us with a similarity matrix between documents
myCosineSpace3 <- multicos(myDocs, tvectors=dk2, breakdown=F)
myCosineSpace3

corrplot(myCosineSpace3, method = "number")





## КАК ВАРИАНТ ПРЕДЛОЖИТЬ БЕЗ TF IDF


###########
# lsaMatrix <- diag(lsaSpace$sk) %*% t(lsaSpace$dk)
# Похоже это аналог
# lsaMatrix <- as.textmatrix(lsaSpace)
###########
#Странный результат
# https://github.com/tifaniwarnita/Document-Similarity/blob/master/Document%20Similarity/doc-sim%20(lsa).R

# Creating Term Document Matrix
tdm <- TermDocumentMatrix(myCorpus)
tdm <- as.matrix(tdm)

# ЭТО БЕЗ TF IDF. Поэтому другой результат

lsaSpace <- lsa::lsa(tdm, dims=dimcalc_share()) # create LSA space

#lsaSpace <- lsa::lsa(tdm.tfidf, dims=dimcalc_share()) # create LSA space

#lsaMatrix <- as.textmatrix(lsaSpace)

# lsaMatrix now is a k x (num doc) matrix, in k-dimensional LSA space
lsaMatrix <- diag(lsaSpace$sk) %*% t(lsaSpace$dk)
# ЭТО АНАЛОГИ
lsaMatrix <- as.textmatrix(lsaSpace)
# Use the `cosine` function in `lsa` package to get cosine similarities matrix
distMatrix <- cosine(lsaMatrix)

distMatrix

corrplot(distMatrix, method = "number")




# Creating Term Document Matrix
tdm <- TermDocumentMatrix(myCorpus)
lsaSpace <- lsa(tdm)
# lsaMatrix now is a k x (num doc) matrix, in k-dimensional LSA space
lsaMatrix <- diag(lsaSpace$sk) %*% t(lsaSpace$dk)
# Use the `cosine` function in `lsa` package to get cosine similarities matrix
distMatrix <- cosine(lsaMatrix)

distMatrix

corrplot(distMatrix, method = "number")


##########
##########
###
###

#https://github.com/DivyaMaharshi/rsudio_setup_trial/blob/2dc2216155ba6e4ae154cdd5c27df4949a241579/content_similarity.R

td.mat <- TermDocumentMatrix(myCorpus)

# БЕЗ TF IDF другой результат
#------------------------------------------------------------------------------
# MDS with LSA
lsaSpace <- lsa(td.mat)  # create LSA space

lsaMatrix <- diag(lsaSpace$sk) %*% t(lsaSpace$dk)
# Use the `cosine` function in `lsa` package to get cosine similarities matrix
# (subtract from 1 to get dissimilarity matrix)
distMatrix <- cosine(lsaMatrix)
corrplot(distMatrix, method = "number")

###
###
###########
###########










library(svs)
lsaMatrix2 <- t(lsaMatrix)
dcos<-dist_cosine(lsaMatrix2, diag = FALSE, upper = FALSE)
dcos <- as.matrix(dcos)
corrplot(dcos)

pc_plot(dcos)

d <- dist(mydata) # euclidean distances between the rows
fit <- cmdscale(dcos,eig=TRUE, k=2) # k is the number of dim
fit # view results

# plot solution 
x <- fit$points[,1]
y <- fit$points[,2]
plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2", 
     main="Metric  MDS",    type="n")
text(x, y, labels = row.names(mydata), cex=.7)

heatmap(dcos, Rowv=as.dendrogram(rc), Colv=NA)



# https://stats.stackexchange.com/questions/6890/plotting-a-heatmap-given-a-dendrogram-and-a-distance-matrix-in-r
set.seed(1)
dat<-matrix(ncol=4, nrow=10, data=rnorm(40))
rd<-dist(dat)
rc<-hclust(rd)
cd<-dist(t(dat))
cc<-hclust(cd)
# Dendrogram for rows only
heatmap(dat, Rowv=as.dendrogram(rc), Colv=NA)
# Dendrogram for columns only
heatmap(dat, Rowv=NA, Colv=as.dendrogram(cc))

