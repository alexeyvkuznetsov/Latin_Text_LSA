library(lsa); library(tm)

d1 = "Human machine interface for ABC computer applications"
d2 = "A survey of user opinion of computer system response time"
d3 = "The EPS user interface management system"
d4 = "System and human system engineering testing of EPS"
d5 <- "Relation of user perceived response time to error measurement"
d6 <- "The generation of random, binary, ordered trees"
d7 <- "The intersection graph of paths in trees"
d8 <- "Graph minors IV: Widths of trees and well-quasi-ordering"
d9 <- "Graph minors: A survey"

# Words that appear in at least two of the titles
D <- c(d1, d2, d3, d4, d5, d6, d7, d8, d9)

corpus <- Corpus(VectorSource(D))

# Remove Punctuation
corpus <- tm_map(corpus, removePunctuation)

# tolower
corpus <- tm_map(corpus, content_transformer(tolower))

# Stopword Removal
corpus <- tm_map(corpus, function(x) removeWords(x, stopwords("english")))

# term document Matrix
myMatrix <- TermDocumentMatrix(corpus)

# Delete terms that only appear in a document
rowTotals <- apply(myMatrix, 1, sum)
myMatrix.new <- myMatrix[rowTotals > 1, ]

# Correlation Matrix of terms
cor(t(as.matrix(myMatrix.new)))

# lsaSpace <- lsa(myMatrix.new)
# myMatrix.reduced <- lsaSpace$tk %*% diag(lsaSpace$sk) %*% t(lsaSpace$dk)

mySVD <- svd(myMatrix.new)
inspect(myMatrix.new)

Mp <- mySVD$u[, c(1,2)] %*% diag(mySVD$d)[c(1, 2), c(1, 2)] %*% t(mySVD$v[, c(1, 2)])

rownames(Mp) <- rownames(myMatrix.new)
cor(t(Mp))
mydata <- data.frame(Mp, fit$cluster) 
wss <- (nrow(Mp)-1)*sum(apply(Mp,2,var))
for (i in 2:5) wss[i] <- sum(kmeans(Mp,centers=i)$withinss) 
plot( wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")