# load required libraries
library(tm)
library(ggplot2)
library(lsa)

# 1. Prepare mock data
text <- c("transporting food by cars will cause global warming. so we should go local.",
          "we should try to convince our parents to stop using cars because it will cause global warming.",
          "some food, such as mongo, requires a warm weather to grow. so they have to be transported to canada.",
          "a typical electronic circuit can be built with a battery, a bulb, and a switch.",
          "electricity flows from batteries to the bulb, just like water flows through a tube.",
          "batteries have chemical energe in it. then electrons flow through a bulb to light it up.",
          "birds can fly because they have feather and they are light.",
          "why some birds like pigeon can fly while some others like chicken cannot?",
          "feather is important for birds' fly. if feather on a bird's wings is removed, this bird cannot fly.")
view <- factor(rep(c("view 1", "view 2", "view 3"), each=3))
df <- data.frame(text, view, stringsAsFactors=FALSE)

# prepare corpus
corpus <- Corpus(VectorSource(df$text))
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, function(x) removeWords(x, stopwords("english")))text
corpus <- tm_map(corpus, stemDocument, language = "english")
corpus # check corpus

# 2. MDS with raw term-document matrix
# compute distance matrix
td.mat <- as.matrix(TermDocumentMatrix(corpus))
dist.mat <- dist(t(as.matrix(td.mat)))
dist.mat # check distance matrix

# MDS
fit <- cmdscale(dist.mat, eig=TRUE, k=2)
points <- data.frame(x=fit$points[, 1], y=fit$points[, 2])
ggplot(points, aes(x=x,y=y)) + 
  geom_point(data=points,aes(x=x, y=y, color=df$view)) + 
  geom_text(data=points,aes(x=x, y=y-0.2, label=row.names(df)))

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

# 4. Play with Twitter data
# you will need additional source files
# See http://bodongchen.com/blog/?p=290 for details
setwd("~/src/r/twitter-analytics/twitter-hashtag-analytics")
source("utilities.R")
source("get_tweets.R")
source("munge_tweets.R")
source("semantic_analysis.R")

# get tweets from #LAK13
lak13 <- GetTweetsBySearch('#LAK13', 500)
lak13 <- PreprocessTweets(lak13)
corpus <- ConstructCorpus(lak13$text, removeTags=TRUE, removeUsers=TRUE, stemming=TRUE)

# compute distance matrix
td.mat <- as.matrix(TermDocumentMatrix(corpus))
td.mat.lsa <- lw_bintf(td.mat) * gw_idf(td.mat) # weighting
lsaSpace <- lsa(td.mat.lsa) # create LSA space
dist.mat.lsa <- dist(t(as.textmatrix(lsaSpace))) # compute distance matrix

# MDS
fit <- cmdscale(dist.mat.lsa, eig=TRUE, k=2)
points <- data.frame(x=fit$points[, 1], y=fit$points[, 2])
ggplot(points, aes(x=x, y=y)) + 
  geom_point(data=points, aes(x=x, y=y))