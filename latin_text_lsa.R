setwd("D:/GitHub/Latin_Text_LSA/")


library(tm)
library(udpipe)

library(readr)
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

prologus$book<-"Prologus"
historia_g$book<-"Historia_Gothorum"
recapitulatio$book<-"Recapitulatio"
historia_w$book<-"Historia_Wandalorum"
historia_s$book<-"Historia_Suevorum"


historia<-rbind(prologus,historia_g,recapitulatio,historia_w,historia_s)

#historia$texts <- stripWhitespace(historia$texts)
historia$texts <- tolower(historia$texts)
historia$texts <- removePunctuation(historia$texts)
historia$texts <- removeNumbers(historia$texts)

# Stopwords 

customStopWords <- c("ann", "annus", "aer", "aes", "aera", "sunt")

load("lat_stopwords.Rda")

MyStopwords <- c(lat_stopwords, customStopWords)

historia$texts <- removeWords(historia$texts, c(lat_stopwords, customStopWords))


#udmodel_latin <- udpipe_download_model(language = "latin_ittb")
#udmodel_latin <- udpipe_load_model(ud_model$file_model)
udmodel_latin <- udpipe_load_model(file = "latin-ittb-ud-2.4-190531.udpipe")

x <- udpipe_annotate(udmodel_latin, x = historia$texts, doc_id = historia$book, tagger = "default", parser = "default", trace = TRUE)
x <- as.data.frame(x)

save(x,file="historia_annotated_dataset.Rda")


## Get a data.frame with 1 row per doc_id/lemma

dtf <- document_term_frequencies(x[, c("doc_id", "lemma")])

dtf <- subset(x, upos %in% c("ADJ", "ADV", "PROPN", "VERB", "NOUN"))

dtf <- document_term_frequencies(dtf, document = "doc_id", term = "lemma")


## Create a document/term/matrix for building a topic model
dtm <- document_term_matrix(x = dtf)
## Remove words which do not occur that much
dtm_clean <- dtm_remove_lowfreq(dtm, minfreq = 3)
head(dtm_colsums(dtm_clean))

## Remove nouns which you really do not like (mostly too common nouns)
dtm_clean <- dtm_remove_terms(dtm_clean, terms = c("appartement", "appart", "eter"))
## Or keep of these nouns the top 50 based on mean term-frequency-inverse document frequency
dtm_clean <- dtm_remove_tfidf(dtm_clean, top = 50)



## Convert dtm to a list of text
dtm2list <- apply(dtm, 1, function(x) {
  paste(rep(names(x), x), collapse=" ")
})

## convert to a Corpus

myCorpus <- VCorpus(VectorSource(dtm2list))
inspect(myCorpus)

# Created tdm_matrix

tdmtm <- TermDocumentMatrix(myCorpus)

tdm_matrix <- as.matrix(tdmtm)

# Created LSA space

tdmtfidf <- lw_tf(tdm_matrix) * gw_idf(tdm_matrix)

tdmtfidf

lsaSpace <- lsa(tdmtfidf, dims=dimcalc_share())

as.textmatrix(lsaSpace)


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

dfm <- cast_dfm(dtf, doc_id, term, freq)

tdm <- cast_tdm(dtf, doc_id, term, freq)


# Convert A Dfm To An Lsa "Textmatrix" quanteda
tdmlsa <- convert(dfm, to = "lsa") 


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

