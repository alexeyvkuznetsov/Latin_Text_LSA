
#set working directory
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

rome_number<-paste(scan(file ="rom number 1000.txt",what='character'),collapse=" ")
rome_number<-tolower(rome_number)
rome_number
write(rome_number, file="rome_number_v.txt")



load("rome_number_1000.Rda")

customStopWords <- c("ann", "annus", "aer", "aes", "aera", "num._rom.", "xnum._rom.", "xxnum._rom.", "xxxnum._rom.", "cdxlnum._rom.")

#lat_stopwords_romnum <- c("i", "ii", "iii", "iiii", "iv", "v", "vii", "viii", "ix", "x", "xi", "xii", "xiii", "xiv", "xv", "xvi", "xvii", "xviii", "xix", "xx", "xxi", "xxii", "xxiii", "xxiv", "xxv", "xxvi", "xxvii", "xxviii", "xxix", "xxx", "xxxi", "xxxii", "xxxiii", "xxxiv", "xxxv", "xxxvi", "xxxvii", "xxxviii", "xxxix", "xl", "xli", "xlii", "xliii", "xliv", "xlv", "xlvi", "xlvii", "xlviii", "xlix", "l", "li", "lii", "liii", "liv", "lv", "lvi", "lvii", "lviii", "lix", "lx", "lxi", "lxii", "lxiii", "lxiv", "lxv", "lxvi", "lxvii", "lxviii", "lxix", "lxx", "lxxi", "lxxii", "lxxiii", "lxxiv", "lxxv", "lxxvi", "lxxvii", "lxxviii", "lxxix", "lxxx", "lxxxi", "lxxxii", "lxxxiii", "lxxxiv", "lxxxv", "lxxxvi", "lxxxvii", "lxxxviii", "lxxxix", "xc", "xci", "xcii", "xciii", "xciv", "xcv", "xcvi", "xcvii", "xcviii", "xcix", "c")

lat_stop_perseus <- c("ab", "ac", "ad", "adhic", "aliqui", "aliquis", "an", "ante", "apud", "at", "atque", "aut", "autem", "cum", "cur", "de", "deinde", "dum", "ego", "enim", "ergo", "es", "est", "et", "etiam", "etsi", "ex", "fio", "haud", "hic", "iam", "idem", "igitur", "ille", "in", "infra", "inter", "interim", "ipse", "is", "ita", "magis", "modo", "mox", "nam", "ne", "nec", "necque", "neque", "nisi", "non", "nos", "o", "ob", "per", "possum", "post", "pro", "quae", "quam", "quare", "qui", "quia", "quicumque", "quidem", "quilibet", "quis", "quisnam", "quisquam", "quisque", "quisquis", "quo", "quoniam", "sed", "si", "sic", "sive", "sub", "sui", "sum", "super", "suus", "tam", "tamen", "trans", "tu", "tum", "ubi", "uel", "uero", "unus", "ut", "quoque", "xiix")

#save(lat_stop_perseus,file="lat_stop_perseus.Rda")

#load("lat_stop_perseus.Rda")

#MyStopwords <- c(lat_stop_perseus, customStopWords, lat_stopwords_romnum)

MyStopwords <- c(lat_stop_perseus, rome_number_1000)

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
###
###

#COSINE similarity
# 
# compute cosine distance matrix

lsaMatrix <- as.textmatrix(lsaSpace)

mat.lsa.cosine <- lsa::cosine(lsaMatrix) #Cosine similarity matrix

#mat.lsa.cosine <- lsa::cosine(as.textmatrix(lsaSpace)) #Cosine similarity matrix

mat.lsa.cosine


#colnames(mat.lsa.cosine) <- c("1. Prolog", "2. Historia Gothorum", "3. Recapitulatio", "4. Historia Wandalorum", "5. Historia Suevorum")

rownames(mat.lsa.cosine) <- c("1. Prolog", "2. Historia Gothorum", "3. Recapitulatio", "4. Historia Wandalorum", "5. Historia Suevorum")

round((mat.lsa.cosine), 2) # round the results to a couple of decimals

mat.lsa.cosine

# Plot cosine similarity matrix
# https://cran.r-project.org/web/packages/corrplot/vignettes/corrplot-intro.html

library(corrplot)
corrplot(mat.lsa.cosine)

col <- colorRampPalette(c("red", "white", "lightblue")) 

#diag(mat.lsa.cosine) = " "

#corrplot(mat.lsa.cosine, method = "number")

corrplot(mat.lsa.cosine, method="color", addCoef.col = "black", col = col(10), tl.srt = 30, tl.col = "black")

#corrplot(mat.lsa.cosine, method="color", addCoef.col = "black", col = col(10), cl.pos = "b", tl.srt = 30, tl.col = "black")

#corrplot(mat.lsa.cosine, method = "circle", addCoef.col = "black")

#corrplot(mat.lsa.cosine, method = "number", order = "hclust", hclust.method = "complete")




# http://www.sthda.com/english/wiki/ggcorrplot-visualization-of-a-correlation-matrix-using-ggplot2
# https://github.com/kassambara/ggcorrplot
library(ggcorrplot)
ggcorrplot(mat.lsa.cosine, lab = TRUE)




# Hierarchical Clustering in R
# https://datascienceplus.com/hierarchical-clustering-in-r/
# https://www.datacamp.com/community/tutorials/hierarchical-clustering-R
# https://www.rdocumentation.org/packages/pvclust/versions/2.2-0/topics/pvclust
# https://github.com/shimo-lab/pvclust

# Hierarchical clustering. It creates a hierarchy of clusters, and presents the hierarchy 
# in a dendrogram. This method does not require the number of clusters to be specified at 
# the beginning. Distance connectivity between observations is the measure. 

# The dendrogram is a multilevel hierarchy where clusters at one level are joined together
# to form the clusters at the next levels. This makes it possible to decide the level at
# which to cut the tree for generating suitable groups of a data objects.

# Agglomerative clustering works in a “bottom-up” manner. That is, each object is
# initially considered as a single-element cluster (leaf). At each step of the algorithm,
# the two clusters that are the most similar are combined into a new bigger cluster
# (nodes). This procedure is iterated until all points are member of just one single big
# cluster (root) (see figure below)
# https://en.proft.me/2017/01/29/exploring-hierarchical-clustering-r/


# https://rpubs.com/gaston/dendrograms


# Dissimilarity matrix
d <- dist(mat.lsa.cosine)
d
result <- hclust(d, method = 'average')
# Hierarchical clustering using Complete Linkage
result <- hclust(d, method = "complete")
plot(result)

plot(result, type = "triangle", ylab = "Height")

clusters <- hclust(dist(mat.lsa.cosine))

clusters <- agnes(mat.lsa.cosine, method = "complete")

clusters <- diana(mat.lsa.cosine)

plot(clusters)






# МОЕ НАОДНОЕ ТВОРЧЕСТВО
# Dissimilarity matrix
d <- dist(mat.lsa.cosine, method = "euclidean")
# Hierarchical clustering using Complete Linkage
result <- hclust(d, method = "complete")
#result <- hclust(d, method = 'average')
plot(result)

library(factoextra)

fviz_dend(result, k = 4, # Cut in 3 groups
          cex = 1, # label size
          k_colors = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
          color_labels_by_k = TRUE, # color labels by groups
          #rect_border = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
          # rect_fill = TRUE,
          rect = TRUE # Add rectangle around groups
)



require("igraph")
fviz_dend(result, k = 4, k_colors = "jco",
          type = "phylogenic", repel = TRUE)



require("igraph")
fviz_dend(result, k = 4, # Cut in four groups
          cex = 1, # label size
          #k_colors = "jco",
          k_colors = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
          type = "phylogenic", repel = TRUE,
          phylo_layout = "layout.gem")






library(dendextend)
avg_dend_obj <- as.dendrogram(result)
avg_col_dend <- color_branches(avg_dend_obj, h = 3)
plot(avg_col_dend)


#hc_single <- agnes(lsaMatrix, method = "single")
#hc_complete <- agnes(lsaMatrix, method = "complete")



# Сравнение методов кластеризации

library(dendextend)


lsaSpace <- lsa::lsa(tdm.tfidf, dims=dimcalc_share()) # create LSA space

lsaMatrix <- as.textmatrix(lsaSpace)

mat.lsa.cosine <- lsa::cosine(lsaMatrix) #Cosine similarity matrix

mat.lsa.cosine

round((mat.lsa.cosine), 2) # round the results to a couple of decimals

mat.lsa.cosine

# Dissimilarity matrix
# Compute distance matrix
d <- dist(mat.lsa.cosine, method = "euclidean")

# Compute 2 hierarchical clusterings

hc1 <- hclust(d, method = "average")
hc2 <- hclust(d, method = "ward.D2")

# # converting to dendogram objects as dendextend works with dendogram objects
dend1 <- as.dendrogram (hc1)
dend2 <- as.dendrogram (hc2)

# Create a list to hold dendrograms
dend_list <- dendlist(dend1, dend2)

# Визуализация
tanglegram(dend1, dend2)






library(ggplot2)
library(dendextend)
# Rectangle dendrogram using ggplot2
ggd1 <- as.ggdend(result)
ggplot(ggd1) 



library(cluster)
res.agnes <- agnes(x = lsaMatrix, # data matrix
                   stand = TRUE, # Standardize the data
                   metric = "euclidean", # metric for distance matrix
                   method = "ward" # Linkage method
)

plot(res.agnes)





## ЭКСПЕРИМЕНТ


mat.lsa.pearson <- cor(lsaMatrix, method="pearson")

mat.lsa.pearson

#colnames(mat.lsa.pearson) <- c("1. Prolog", "2. Historia Gothorum", "3. Recapitulatio", "4. Historia Wandalorum", "5. Historia Suevorum")

rownames(mat.lsa.pearson) <- c("1. Prolog", "2. Historia Gothorum", "3. Recapitulatio", "4. Historia Wandalorum", "5. Historia Suevorum")

round((mat.lsa.pearson), 2) # round the results to a couple of decimals

corrplot(mat.lsa.pearson, method="color", addCoef.col = "black", col = col(10), cl.pos = "b", tl.srt = 30, tl.col = "black")

ggcorrplot(mat.lsa.pearson, lab = TRUE)




#spearman

mat.lsa.spearman <- cor(lsaMatrix, method="spearman")

mat.lsa.spearman

#colnames(mat.lsa.spearman) <- c("1. Prolog", "2. Historia Gothorum", "3. Recapitulatio", "4. Historia Wandalorum", "5. Historia Suevorum")

rownames(mat.lsa.spearman) <- c("1. Prolog", "2. Historia Gothorum", "3. Recapitulatio", "4. Historia Wandalorum", "5. Historia Suevorum")

round((mat.lsa.spearman), 2) # round the results to a couple of decimals

corrplot(mat.lsa.spearman, method="color", addCoef.col = "black", col = col(10), cl.pos = "b", tl.srt = 30, tl.col = "black")

ggcorrplot(mat.lsa.spearman, lab = TRUE)




#kendall

mat.lsa.kendall <- cor(lsaMatrix, method="spearman")

mat.lsa.kendall

#colnames(mat.lsa.kendall) <- c("1. Prolog", "2. Historia Gothorum", "3. Recapitulatio", "4. Historia Wandalorum", "5. Historia Suevorum")

rownames(mat.lsa.kendall) <- c("1. Prolog", "2. Historia Gothorum", "3. Recapitulatio", "4. Historia Wandalorum", "5. Historia Suevorum")

round((mat.lsa.kendall), 2) # round the results to a couple of decimals

corrplot(mat.lsa.kendall, method="color", addCoef.col = "black", col = col(10), cl.pos = "b", tl.srt = 30, tl.col = "black")

ggcorrplot(mat.lsa.kendall, lab = TRUE)



############################
## END
############################
# https://github.com/katyalrajat/corpus_mining/blob/9b805cd229b2f5260bfa1007765b0aa6c992fc8d/code.R

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




library(LSAfun)

neighbors("gens", n=36, tvectors = lsaMatrix)

plot_neighbors("gens", n=6, tvectors = lsaMatrix, method = "MDS", dims = 3, col = c("black","blue","red"))



