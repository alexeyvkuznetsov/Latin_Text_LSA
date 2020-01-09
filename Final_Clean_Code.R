##### Scrape Data in other file
library(tidyverse)
library(rgl)
library(tm)
library(lsa)
library(LSAfun)
library(tidytext)
library(network)
library(VBLPCM)
library(igraph)

load("uniqueSongs2.Rdata")
load("words_we_want2.Rdata")
load("full_lyrics2.Rdata")


### Naive, non-network based approach 

words_we_want2 <- tm_map(words_we_want, stemDocument)
tdm.full <- TermDocumentMatrix(words_we_want2)
tdm.full.mat <- as.matrix(tdm.full)

affin_score <- get_sentiments("afin")
affin_score$word <- stemDocument(affin_score$word)
affin_score <- affin_score[!duplicated(affin_score$word),]
sent <- rep(0, NROW(rownames(tdm.full)))
for (i in 1:NROW(rownames(tdm.full))) {
  word <- rownames(tdm.full)[i]
  if (word %in% affin_score$word) {
    sent[i] <- affin_score$score[affin_score$word==word]
  }
}

tdm.norm.mat <- apply(tdm.full.mat, 2, function(u) u/sum(u))
doc_scores <- t(tdm.norm.mat)%*%sent

cuts <- quantile(doc_scores, c(0.025, 0.975))
# top 2.5% most negative songs
uniqueSongs[doc_scores < cuts[1],]
# top 2.5% most positive songs
uniqueSongs[doc_scores > cuts[2],]

cuts <- quantile(doc_scores, c(0.25, 0.75))
uniqueSongs$sentiment <- "Neutral"
uniqueSongs$sentiment[doc_scores > cuts[2]] <- "Pos"
uniqueSongs$sentiment[doc_scores < cuts[1]] <- "Negative"

prop.table(table(uniqueSongs$PostTrump, uniqueSongs$sentiment), margin=1)

### Another Naive, non-network based approach without word levels

bing_neg <- (get_sentiments("bing") %>% filter(sentiment == "negative"))$word
stemmed_neg <- stemDocument(bing_neg)
all_neg <- stemmed_neg[!duplicated(stemmed_neg)]

bing_pos <- (get_sentiments("bing") %>% filter(sentiment == "positive"))$word
stemmed_pos <- stemDocument(bing_pos)
all_pos <- stemmed_pos[!duplicated(stemmed_pos)]

POS_VOC <- which(rownames(tdm.full) %in% all_pos)
NEG_VOC <- which(rownames(tdm.full) %in% all_neg)

sent_vec <- rep(0, NROW(rownames(tdm.full)))
sent_vec[POS_VOC] <- 1
sent_vec[NEG_VOC] <- -1

doc_scores2 <- t(tdm.norm.mat)%*%sent_vec

cuts <- quantile(doc_scores2, c(0.025, 0.975))
# top 2.5% most negative songs
uniqueSongs[doc_scores2 < cuts[1],]
# top 2.5% most positive songs
uniqueSongs[doc_scores2 > cuts[2],]

cuts <- quantile(doc_scores2, c(0.25, 0.75))
uniqueSongs$sentiment2 <- "Neutral"
uniqueSongs$sentiment2[doc_scores2 > cuts[2]] <- "Pos"
uniqueSongs$sentiment2[doc_scores2 < cuts[1]] <- "Negative"

prop.table(table(uniqueSongs$PostTrump, uniqueSongs$sentiment2), margin=1)


### Network based approach

maxDocFreq <- 200
tdm <- TermDocumentMatrix(words_we_want2, control=list(bounds = list(global = c(2, maxDocFreq))))
tdm.idf <- weightTfIdf(tdm, normalize = TRUE)

mat1 <- as.matrix(tdm)
mat2 <- as.matrix(tdm.idf)

maxTermFreq <- 10
mat1[mat1>maxTermFreq] <- maxTermFreq 

# create adjacency matrix
adj.mat <- t(mat1)%*%mat1 
adj.mat2 <- t(mat2)%*%mat2

quantile(adj.mat, seq(0,1,length.out=11))
cutoff <- 100
adj.mat[adj.mat< cutoff] <- 0
adj.mat[adj.mat >= cutoff] <- 1

quantile(adj.mat2, seq(0,1,length.out=11))
cutoff2 <- 0.016
adj.mat2[adj.mat2< cutoff2] <- 0
adj.mat2[adj.mat2 >= cutoff2] <- 1

network1 <- network(x = adj.mat, matrix.type="adjacency", directed=FALSE)
set.vertex.attribute(network1, "Trump", uniqueSongs$PostTrump)


adj.mat4 <- adj.mat[-c(102, 105),]
adj.mat4 <- adj.mat4[,-c(102, 105)]
network1_1 <- graph_from_adjacency_matrix(adj.mat4, mode = "undirected")
network1_1 <- simplify(network1_1, remove.multiple = TRUE, remove.loops = TRUE)
colors <- uniqueSongs$PostTrump[-c(102, 105)]
colors[colors == "0"] <- "#D863CB"
colors[colors == "1"] <- "#69BCE9"
V(network1_1)$color <- colors
  

bmp(file="saving_plot3.bmp",
    width=12, height=12, units="in", res=100)
plot(network1_1, vertex.label = NA, vertex.size = 4, layout = layout_with_fr, edge.width = 0.3)
dev.off()
set.seed(125)
p <- plot(network1_1, vertex.label = NA, vertex.size = 4, layout = layout_with_fr, edge.width = 0.3)

transitivity(network1_1)
diameter(network1_1)
mean(degree(network1_1))
clique_num(network1_1)
clusters(network1_1)
cohesion(network1_1)
components(network1_1)

network.idf <- network(x = adj.mat2, matrix.type="adjacency", directed=FALSE)

## VBLPCM Try 1 

dim <- 2
clust <- 4
v.start <- vblpcmstart(network1,G= clust,d=dim,LSTEPS=1e3)
v.fit <- vblpcmfit(v.start, STEPS=20)
vblpcmgroups(v.fit)

dim(v.fit$Y)
which(apply(adj.mat, 1, sum)<2)

plot(v.fit)


latent_pos <- data.frame(v.fit$V_z)
names(latent_pos) <- c("x", "y")
latent_pos$postTrump <- uniqueSongs$PostTrump[-c(102, 105)]
latent_pos$ID <- uniqueSongs$ID[-c(102,105)]
plot(latent_pos$x, latent_pos$y, col=as.factor(latent_pos$postTrump), pch=19)

latent_pos2 <- latent_pos
latent_pos2$postTrump[latent_pos$postTrump == 0] <- "Pre-Election"
latent_pos2$postTrump[latent_pos$postTrump == 1] <- "Post-Election"

ggplot(latent_pos2) +
  aes(x = x, y = y, col = postTrump) +
  geom_point() +
  theme_light() +
  labs(x = "Latent Position x", y = "Latent Position y") +
  scale_color_manual(values = c("#D863CB", "#69BCE9")) + guides(col =guide_legend(title=NULL))

ggsave("Latent.png", plot = p, width = 8, height = 4, units = "in")

## try KNN

distmat <- as.matrix(dist(latent_pos[,c(1,2)], method = "euclidean"))
sames <- rep(0, NROW(distmat))
diffs <- rep(0, NROW(distmat))
correct <- rep(0, NROW(distmat))
for (i in 1:NROW(distmat)) {
  trump <- latent_pos$postTrump[i]
  same <- which(latent_pos$postTrump == trump & latent_pos$ID != latent_pos$ID[i])
  diff <- which(latent_pos$postTrump != trump & latent_pos$ID != latent_pos$ID[i])
  sames[i] <- sum(distmat[i, same])/NROW(same)
  diffs[i] <- sum(distmat[i, diff])/NROW(diff)
  correct[i] <- sames[i] < diffs[i]
}
mean(correct)

neighbClass <- rep(0, NROW(distmat))
k <- 5
for (i in 1:NROW(distmat)) {
  trump <- latent_pos$postTrump[i]
  knn <- names(sort(distmat[i,])[1:k])
  neighbClass[i] <- mean(as.numeric(latent_pos[knn,]$postTrump))
}
class <- neighbClass
class[class<0.5] <- 0
class[class>0.5] <- 1
table(class, latent_pos$postTrump)

## see closest neighbors
neighbs <- list()
for (i in 1:NROW(distmat)) {
  knn <- names(sort(distmat[i,])[2:(k+1)])
  neighbs[[i]] <- paste(uniqueSongs[(latent_pos[knn,]$ID),]$Title, uniqueSongs[(latent_pos[knn,]$ID),]$Artist)
}

names(neighbs) <- paste(uniqueSongs$Title, uniqueSongs$Artist, sep = " - ")[-c(102, 105)]

## Try Network with other adjacency matrix
v.start2<-vblpcmstart(network.idf,G=clust,d=dim,LSTEPS=1e3)
v.fit2<-vblpcmfit(v.start,STEPS=20)
vblpcmgroups(v.fit2)

dim(v.fit2$Y)

latent_pos2_2 <- data.frame(v.fit2$V_z)
names(latent_pos2_2) <- c("x", "y")
latent_pos2_2$postTrump <- uniqueSongs$PostTrump
latent_pos2_2$ID <- uniqueSongs$ID
plot(latent_pos2_2$x, latent_pos2_2$y, col=as.factor(latent_pos2_2$postTrump), pch=19)
