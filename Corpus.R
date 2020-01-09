
library(tm)

doc1 <- "drugs hospitals doctors"
doc2 <- "smog pollution environment"
doc3 <- "doctors hospitals healthcare"
doc4 <- "pollution environment water"
corpus <- c(doc1, doc2, doc3, doc4)
tm_corpus <- Corpus(VectorSource(corpus))

inspect(tm_corpus)

str(tm_corpus)

tm_corpus <- tm_map(tm_corpus, tolower)
tm_corpus <- tm_map(tm_corpus, removeWords, stopwords("english"))
tm_corpus <- tm_map(tm_corpus, removeNumbers)
tm_corpus <- tm_map(tm_corpus, PlainTextDocument)
tm_corpus <- tm_map(tm_corpus, stemDocument, language="english")
tm_corpus <- tm_map(tm_corpus, stripWhitespace)
tm_corpus <- tm_map(tm_corpus, PlainTextDocument)

tdm <- TermDocumentMatrix(tm_corpus)

as.matrix(tdm)




# prepare corpus from data frame
corpus <- Corpus(VectorSource(df$text))
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, function(x) removeWords(x, stopwords("english")))



# prepare corpus from data frame

reviews$comments = as.character(reviews$comments)
review_corpus <- corpus(fivebooks, docid_field = "book", text_field = "texts")

review_corpus <- corpus(fivebooks, docid_field = "book", text_field = "texts")

summary(corpus(reviews, docid_field = "id", text_field = "comments"))

review_corpus <- corpus(reviews,docid_field = "id",text_field = as.character("comments"))





