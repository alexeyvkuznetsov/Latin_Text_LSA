### Some notes (for poster, etc)
### Idea of LSA is that co-occurrences of words across documents (songs) 
### suggest that those terms are related: synonymous or reflect a shared latent concept.
### Terms can be related even if they do not co-occur in the same document as long as both terms
### co-occur with shared other terms. 
### LSA represents terms internally as vectors of a given rank (number of dimensions)
#### based on a transformation of the co-occurrence matrix. 
#### The co-occurrences of terms across the documents may also indicate that the documents too, 
### and not only the terms within them, can be factored into groups based on those co-occurrences. 
#### Can try to observe clusters of documents may suggest
#### DECENTLY DETAILED SOURCE: https://aisel.aisnet.org/cgi/viewcontent.cgi?article=4025&context=cais


##### TRANSFORMATIONS TO CONSIDER
#### remove things not recognized in English dictionary?? Might help with the few non-english songs
#### remove stopwords
#### Stemming <- recognize that cat and cats or jump and jumping should be same (good idea)


##### Start doing work. Room for more cleaning here. 
#### CAN TRY WITH AND WITHOUT "stemming".
#### Sounds like a good idea, but im not convinced it worked

require(tm)
load("uniqueSongs.Rdata")
load("words_we_want.Rdata")
load("full_lyrics.Rdata")
words_we_want2 <- tm_map(words_we_want, stemDocument)
tdm <- TermDocumentMatrix(words_we_want2, control=list(bounds = list(global = c(2, 300))))
tdm.idf <- weightTfIdf(tdm, normalize = TRUE)


findFreqTerms(tdm,lowfreq = 200)
findFreqTerms(tdm.idf, lowfreq=3)
termcount <-apply(tdm,1,sum)
doccount <- apply(tdm,1,function(u) sum(u>0))

termcount <-apply(tdm,1,sum)
head(termcount[order(termcount,decreasing = T)],20)

#### OBSERVATION: I don't care if a song has a word 100 times
#### vs. 20 times. Let's truncate matrix entries to the 95\%
#### Percentile. 
dtm_mat <- as.matrix(tdm)
dtm_mat[dtm_mat>4] <- 4
idf_mat <- as.matrix(tdm.idf)
idf_mat[idf_mat>0.599] <- 0.599

##### LSA
require(lsa)
txt_mat<- as.textmatrix(dtm_mat)
txt_mat_idf<- as.textmatrix(idf_mat)


miniLSAspace <- lsa(txt_mat, dims=2)
mediumLSAspace <- lsa(txt_mat, dims=10)
miniLSAspace.idf <- lsa(txt_mat_idf, dims=2)
mediumLSAspace.idf <- lsa(txt_mat_idf, dims=10)


tk10 <-t(mediumLSAspace$sk * t(mediumLSAspace$tk)) 
dk10 <- t(mediumLSAspace$sk * t(mediumLSAspace$dk))
tk2 <-t(miniLSAspace$sk * t(miniLSAspace$tk)) 
dk2 <- t(miniLSAspace$sk * t(miniLSAspace$dk))


tk10.idf <-t(mediumLSAspace.idf$sk * t(mediumLSAspace.idf$tk)) 
dk10.idf <- t(mediumLSAspace.idf$sk * t(mediumLSAspace.idf$dk))
tk2.idf <-t(miniLSAspace.idf$sk * t(miniLSAspace.idf$tk)) 
dk2.idf <- t(miniLSAspace.idf$sk * t(miniLSAspace.idf$dk))


plot(tk2[,1], y= tk2[,2], col="red", cex=.50, main="Term Plot, No Doc Weights")
text(tk2[,1], y= tk2[,2], labels=rownames(tk2) , cex=.70) 

plot(tk2.idf[,1], y= tk2.idf[,2], col="red", cex=.50, main="Term Plot, IDF Weight")
text(tk2.idf[,1], y= tk2.idf[,2], labels=rownames(tk2) , cex=.70) 


prePost <- uniqueSongs[uniqueSongs$ID==rownames(dk2),]$PostTrump

plot(dk2[,1], y= dk2[,2], col=as.factor(prePost), cex=.50, main="Document Similarity Plot", pch=19)
text(dk2[,1], y= dk2[,2], labels=rownames(dk2) , cex=.70) 
plot(dk2.idf[,1], y= dk2.idf[,2], col=as.factor(prePost), cex=.50, main="Document Similarity Plot", pch=19)
text(dk2.idf[,1], y= dk2.idf[,2], labels=rownames(dk2) , cex=.70) 


### TAKEAWAY: it did NOT learn about "pretrump" and "posttrump" clusters.
### Did it learn anything interesting? Lets try to figure out
### which songs were rated as similar and why!!



library(LSAfun)
simMat <- multicos(rownames(dk2), tvectors=dk2, breakdown=F) 
simMat10 <- multicos(rownames(dk10), tvectors=dk10, breakdown=F) 


##### RESEARCH QUESTION:
### For each pre-trump song, compute average cosine distance between it
### and other pretrump songs. Then compute average cosine distance between it
### and other post-trump songs. "YES" if it tends to be closer to other
### pretrump than post trump.
for (song in 1:NROW(uniqueSongs)) {
  sameDists <- simMat[song, (uniqueSongs$PostTrump ==uniqueSongs[i,]$PostTrump & uniqueSongs$Title)]  
  difDists <- simMat[song, uniqueSongs$PostTrump!=uniqueSongs[i,]$PostTrump]  
  
}


#### OBSERVATION: most things are way too similar to one another
#### Is it because I didn't do IDF?

### For a few seed songs, let's find their nearest neighbors and farthest neighbors
neighbors = list()
for (i in 1:NROW(uniqueSongs)) {
  song <- uniqueSongs[i,]
  ID <- song$ID
  sorted <- sort(simMat[ID,])
  closest <- names(sorted)[370:375]
  farthest <- names(sorted)[1:5]
  neighbors[[ID]] <- list("song" = paste(song$Title, song$Artist, sep=', '),
                          "closest" = sapply(closest, function(u) paste(uniqueSongs$Title[uniqueSongs$ID==u], uniqueSongs$Artist[uniqueSongs$ID==u], sep=", ")),
                          "farthest" = sapply(farthest, function(u) paste(uniqueSongs$Title[uniqueSongs$ID==u], uniqueSongs$Artist[uniqueSongs$ID==u], sep=", ")) )               
}

neighbors10 = list()
for (i in 1:NROW(uniqueSongs)) {
  song <- uniqueSongs[i,]
  ID <- song$ID
  sorted <- sort(simMat10[ID,])
  closest <- names(sorted)[370:375]
  farthest <- names(sorted)[1:5]
  neighbors10[[ID]] <- list("song" = paste(song$Title, song$Artist, sep=', '),
                          "closest" = sapply(closest, function(u) paste(uniqueSongs$Title[uniqueSongs$ID==u], uniqueSongs$Artist[uniqueSongs$ID==u], sep=", ")),
                          "farthest" = sapply(farthest, function(u) paste(uniqueSongs$Title[uniqueSongs$ID==u], uniqueSongs$Artist[uniqueSongs$ID==u], sep=", ")) )               
}




####
capture.output( {
for (song in neighbors) {
  print(song$song)
  print(paste("closest:", paste(as.character(song$closest), collapse=" - ")))
  print(paste("farthest", paste(as.character(song$farthest), collapse=" - ")))
  print("----------------------------------------------")
}
}, file = "LSA_10.idf.txt")


##### Will more dimensions help?? Let's try 10 
capture.output( {
  for (song in neighbors10) {
    print(song$song)
    print(paste("closest:", paste(as.character(song$closest), collapse=" - ")))
    print(paste("farthest", paste(as.character(song$farthest), collapse=" - ")))
    print("----------------------------------------------")
  }
}, file = "send_to_emily_10dim.txt")




