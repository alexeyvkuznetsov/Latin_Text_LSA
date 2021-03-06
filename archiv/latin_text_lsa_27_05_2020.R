#########################################################################
#########################################################################
##  The computer analysis of Latin texts:                              ##
##  Latent Semantic Analysis of “Historia de regibus Gothorum,         ##
##  Wandalorum et Suevorum” by Isidore of Seville.                     ##
##  Author: Alexey Kuznetsov                                           ##
##  URL: https://github.com/alexeyvkuznetsov/Latin_Text_LSA            ##
##       https://alexeyvkuznetsov.github.io                            ##
#########################################################################
#########################################################################

#set working directory
setwd("D:/GitHub/Latin_Text_LSA/")

# load required libraries
require(tm)
require(udpipe)
require(lsa)
require(ggplot2)
require(scatterplot3d)
require(corrplot)
require(factoextra)
#library(textmineR)
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


historia$texts <- tolower(historia$texts)
historia$texts <- removePunctuation(historia$texts)
historia$texts <- removeNumbers(historia$texts)
historia$texts <- stripWhitespace(historia$texts)

#########################################################################
### REMOVE STOPWORDS                                                  ###
#########################################################################

#rome_number<-paste(scan(file ="rom number 1000.txt",what='character'),collapse=" ")
#rome_number<-tolower(rome_number)
#rome_number
#write(rome_number, file="rome_number_v.txt")



load("rome_number_1000.Rda")

customStopWords <- c("ann", "annus", "aer", "aes", "aera", "num._rom.", "xnum._rom.", "xxnum._rom.", "xxxnum._rom.", "cdxlnum._rom.")

lat_stop_perseus <- c("ab", "ac", "ad", "adhic", "aliqui", "aliquis", "an", "ante", "apud", "at", "atque", "aut", "autem", "cum", "cur", "de", "deinde", "dum", "ego", "enim", "ergo", "es", "est", "et", "etiam", "etsi", "ex", "fio", "haud", "hic", "iam", "idem", "igitur", "ille", "in", "infra", "inter", "interim", "ipse", "is", "ita", "magis", "modo", "mox", "nam", "ne", "nec", "necque", "neque", "nisi", "non", "nos", "o", "ob", "per", "possum", "post", "pro", "quae", "quam", "quare", "qui", "quia", "quicumque", "quidem", "quilibet", "quis", "quisnam", "quisquam", "quisque", "quisquis", "quo", "quoniam", "sed", "si", "sic", "sive", "sub", "sui", "sum", "super", "suus", "tam", "tamen", "trans", "tu", "tum", "ubi", "uel", "uero", "unus", "ut", "quoque", "xiix")

#save(lat_stop_perseus,file="lat_stop_perseus.Rda")

#load("lat_stop_perseus.Rda")

#MyStopwords <- c(lat_stop_perseus, customStopWords, lat_stopwords_romnum)

MyStopwords <- c(lat_stop_perseus, rome_number_1000, customStopWords)

#historia$texts <- removeWords(historia$texts, c(lat_stop_perseus, rome_number_1000))

historia$texts <- removeWords(historia$texts, MyStopwords)

historia$texts <- stripWhitespace(historia$texts)


#########################################################################
### UDPipe ANNOTATION                                                 ###
### http://ufal.mff.cuni.cz/udpipe/models/                            ###
#########################################################################

#udmodel_latin <- udpipe_download_model(language = "latin_ittb")
#udmodel_latin <- udpipe_load_model(ud_model$file_model)
udmodel_latin <- udpipe_load_model(file = "latin-ittb-ud-2.5-191206.udpipe")

x <- udpipe_annotate(udmodel_latin, x = historia$texts, doc_id = historia$book, tagger = "default", parser = "default", trace = TRUE)
x <- as.data.frame(x)

save(x,file="historia_annotated_dataset_2_5.Rda")

load("historia_annotated_dataset_2_5.Rda")

## Get a data.frame with 1 row per doc_id/lemma or specific POS tag

#dtf <- document_term_frequencies(x[, c("doc_id", "lemma")])

dtf <- subset(x, upos %in% c("ADJ", "ADV", "PROPN", "VERB", "NOUN"))

dtf <- document_term_frequencies(dtf, document = "doc_id", term = "lemma")


## Create a document-term matrix
dtm <- document_term_matrix(x = dtf)
## Remove words which do not occur that much
dtm <- dtm_remove_lowfreq(dtm, minfreq = 2)
head(dtm_colsums(dtm))


## Remove nouns which you really do not like (mostly too common nouns)
#dtm <- dtm_remove_terms(dtm, terms = c("ann", "adipio", "annus", "aer", "aes", "aera", "suus", "filius", "multus", "num._rom.", "xnum._rom.", "xstincum", "xxnum._rom.", "xxxnum._rom.", "cdxlnum._rom."))

dtm <- dtm_remove_terms(dtm, terms = c("ann", "annus", "aer", "aes", "aera", "num._rom.", "xnum._rom.", "xstincum", "xxnum._rom.", "xxxnum._rom.", "cdxlnum._rom.", "cdlxvus", "cdxcnum._rom.", "cdxcus", "cdxix", "cdxlnum._rom.", "cdxlvium", "cdxlvus", "cdxx", "cdxxcvus", "cdxxxnum._rom.", "clxxnum._rom.", "cxiium", "cxx", "dclix", "dcxliix", "dcxlis", "dcxxnum._rom.", "dcxxxix", "dlxnum._rom.", "dlxxxnum._rom.", "dlxxxvus", "dxnum._rom.", "dxxvus", "obnonum._rom."))

## Or keep of these nouns the top 50 based on mean term-frequency-inverse document frequency
#dtm <- dtm_remove_tfidf(dtm, top = 50)


#########################################################################
### CREATE TERM-DOCUMENT MATRIX                                       ###
#########################################################################

#dtm <- as.matrix(dtm)
#tdm <- t(dtm)

tdm <- t(as.matrix(dtm))



#########################################################################
### CREATE LATENT SEMANTIC SPACE (TF-IDF)                             ###
#########################################################################
# Calculate a weighted term-document matrix according to the chosen local and/or global weighting scheme
# Calculate a TF-IDF weighted term-document matrix

tdm.tfidf <- lw_tf(tdm) * gw_idf(tdm) # tf-idf weighting


# Calculate the latent semantic space for the give document-term matrix and create lsaSpace:
# create the latent semantic space
# Non weighted
lsaSpace <- lsa::lsa(tdm, dims=dimcalc_share()) # create latent semantic space
#Weighted
lsaSpace <- lsa::lsa(tdm.tfidf, dims=dimcalc_share()) # create latent semantic space
lsaSpace <- lsa::lsa(tdm.tfidf, dims=5) # create latent semantic space


# display it as a textmatrix again

lsaMatrix <- as.textmatrix(lsaSpace)
# =
#lsaMatrix <- diag(lsaSpace$sk) %*% t(lsaSpace$dk)

lsaMatrix


# https://github.com/pmtempone/tec_semantica/blob/627f79c01389a39ba07621c90e695336268e424c/tec_semantica_R/ls.R


lsaSpace$dk
docmatrix <- as.matrix(lsaSpace$dk)
dk <- as.matrix(lsaSpace$dk)

#########################
# This command will show the value-weighted matrix of Terms
tk2 = t(lsaSpace$sk * t(lsaSpace$tk))
tk2


#########################
# This command will show the value-weighted matrix of Documents
dk2 = t(lsaSpace$sk * t(lsaSpace$dk))
dk2


#########################
lsaSpace3 <- lsa(tdm, dims=3) 
tk3 = t(lsaSpace3$sk * t(lsaSpace3$tk)) 
tk3 

#########################
# The two lines of code must be run together. The first line of code creates a plot of the first two 
# dimensions of $tk, marking the dots as red dots. The second line superimposes term names. 
plot(tk2[,1], y= tk2[,2], col="red", cex=.50, main="TK Plot")
text(tk2[,1], y= tk2[,2], labels=rownames(tk2) , cex=.70)
# This can be done with the documents too. The added parameter cex determines text size. 
plot(dk2[,1], y= dk2[,2], col="blue", pch="+", main="DK Plot")
text(dk2[,1], y= dk2[,2], labels=rownames(dk2), cex=.70)

plot(dk[,1], y= dk[,2], col="blue", pch="+", main="DK Plot")
text(dk[,1], y= dk[,2], labels=rownames(dk), cex=.70)

#########################################################################
### LATENT SEMANTIC ANALYSIS                                          ###
#########################################################################


# Example from: Mastering Text Mining with R 
# https://github.com/pmtempone/tec_semantica/blob/627f79c01389a39ba07621c90e695336268e424c/tec_semantica_R/ls.R
# Compute distance between documents and scale the multidimentional semantic space (MDS) onto two dimensions

#dist.mat.lsa <- dist(t(as.textmatrix(lsaSpace))) # compute distance matrix

dist.mat.lsa <- dist(t(lsaMatrix))

dist.mat.lsa # check distance mantrix

# Plot the distance matrix:

fit <- cmdscale(dist.mat.lsa, eig=TRUE, k=2) # Classical (Metric) Multidimensional Scaling

points <- data.frame(x=fit$points[, 1], y=fit$points[, 2])

Documents = c("1 Prologus", "2 Historia Gothorum", "3 Recapitulatio", "4 Historia Wandalorum", "5 Historia Suevorum")

ggplot(points,aes(x=x, y=y)) + 
  geom_point(data=points,aes(x=x, y=y, col=Documents), size = 3) + 
  geom_text(data=points,aes(x=x, y=y-3, label=row.names(historia)))



#p <- ggplot(points,aes(x=x, y=y)) + 
#  geom_point(data=points,aes(x=x, y=y, col=Documents), size = 3) + 
#  geom_text(data=points,aes(x=x, y=y-3, label=row.names(historia)))
#ggplot_build(p)$data

#geom_point(colour = c("blue", "green", "red", "purple", "orange"), size = 3)

#3D plot
#Compute distance between documents and scale the multidimentional semantic space (MDS) onto three dimensions
library(scatterplot3d)

fit <- cmdscale(dist.mat.lsa, eig=TRUE, k=3)

points <- data.frame(x=fit$points[, 1], y=fit$points[, 2])

colors <- rep(c("#F8766D", "#A3A500", "#00BF7D", "#00B0F6", "#E76BF3"))

s3d <- scatterplot3d(fit$points[, 1], fit$points[, 2], fit$points[, 3], color=colors, pch=19, angle = 65, box = FALSE,
                     main=" ", xlab="x", ylab="y", zlab="z", type="h")
legend("top", legend = c("1 Prologus", "2 Historia Gothorum", "3 Recapitulatio", "4 Historia Wandalorum", "5 Historia Suevorum"),
       col =  c("#F8766D", "#A3A500", "#00BF7D", "#00B0F6", "#E76BF3"), pch = 19, bty = "n", bg = "transparent",
       inset = 0.05, xpd = TRUE)

s3d$points3d(seq(0,0,0), seq(0,0,0), seq(0,0,0), col="red", type="h", pch=17)


#########################################################################
### COSINE SIMILARITY IN LATENT SEMANTIC SPACE (TF-IDF)               ###
#########################################################################

# compute cosine similarity matrix

lsaMatrix <- as.textmatrix(lsaSpace)

lsa.cosine.sim.mat <- lsa::cosine(lsaMatrix) #Cosine similarity matrix

lsa.cosine.sim.mat

colnames(lsa.cosine.sim.mat) <- c("1", "2", "3", "4", "5")
#rownames(lsa.cosine.sim.mat) <- c("1", "2", "3", "4", "5")
#rownames(lsa.cosine.sim.mat) <- c("1. Prologus", "2. Historia Gothorum", "3. Recapitulatio", "4. Historia Wandalorum", "5. Historia Suevorum")

round((lsa.cosine.sim.mat), 2) # round the results to a couple of decimals

lsa.cosine.sim.mat.round <- as.matrix(round((lsa.cosine.sim.mat), 2))

lsa.cosine.sim.mat.round 

# Plot cosine similarity matrix
# https://cran.r-project.org/web/packages/corrplot/vignettes/corrplot-intro.html

library(corrplot)

#corrplot(lsa.cosine.sim.mat)

col <- colorRampPalette(c("red", "white", "lightblue")) 

#corrplot(lsa.cosine.sim.mat, method = "number")

corrplot(lsa.cosine.sim.mat, method="color", addCoef.col = "black", col = col(10), tl.srt = 30, tl.col = "black")


#corrplot(lsa.cosine.sim.mat, method="color", addCoef.col = "black", col = col(10), cl.pos = "b", tl.srt = 30, tl.col = "black")
#corrplot(lsa.cosine.sim.mat, method = "circle", addCoef.col = "black")
#corrplot(lsa.cosine.sim.mat, method = "number", order = "hclust", hclust.method = "complete")




# http://www.sthda.com/english/wiki/ggcorrplot-visualization-of-a-correlation-matrix-using-ggplot2
# https://github.com/kassambara/ggcorrplot
library(ggcorrplot)
ggcorrplot(lsa.cosine.sim.mat, lab = TRUE)



#########################################################################
### HIERARCHICAL CLUSTERING OF DOCUMENTS                              ###
#########################################################################

# Compute cosine distance matrix

####
# https://www.rtextminer.com/articles/b_document_clustering.html
# We convert cosine similarity to cosine distance by subtracting it from 1. 
#library(textmineR)

rownames(lsa.cosine.sim.mat) <- c("1", "2", "3", "4", "5")
rownames(lsa.cosine.sim.mat) <- c("1. Prologus", "2. Historia Gothorum", "3. Recapitulatio", "4. Historia Wandalorum", "5. Historia Suevorum")

lsa.cosine.dist.mat <- as.dist(1 - lsa.cosine.sim.mat, diag = TRUE, upper = TRUE)


####

#lsa.cosine.dist.mat <- dist(lsa.cosine.sim.mat, method = "euclidean", diag = TRUE, upper = FALSE)

lsa.cosine.dist.mat

round((lsa.cosine.dist.mat), 2)

lsa.cosine.dist.mat.round <- as.matrix(round((lsa.cosine.dist.mat), 2))

# Hierarchical clustering using Complete Linkage

result <- hclust(lsa.cosine.dist.mat, method = "complete")
#result <- hclust(lsa.cosine.dist.mat, method = 'average')

plot(result, main = "", ylab = "", xlab = "")

#plot(result, main = "Hierarchical clustering of 100 NIH grant abstracts", ylab = "", xlab = "", yaxt = "n")

# https://rpkgs.datanovia.com/factoextra/index.html

library(factoextra)

fviz_dend(result, k = 4, # Cut in 4 groups
          cex = 1, # label size
          main = "  ", xlab = "  ",
          k_colors = c("#2E9FDF", "green", "#E7B800", "#FC4E07"),
          color_labels_by_k = TRUE, # color labels by groups
          rect_border = c("#2E9FDF", "green", "#E7B800", "#FC4E07"),
          #rect_border = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
          # rect_fill = TRUE,
          rect = TRUE # Add rectangle around groups
)



fviz_dend(result, rect = TRUE) # dendrogam

fviz_silhouette(result) # silhouette plot

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


