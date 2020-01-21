# Function to group documents in a corpus by similarity using a combination of text 
# and graph based analytics

library(tm)
library(SnowballC) 
library(igraph)
library(lsa)
library(wordcloud)

# Set Cosine Similarity threshold
cosSimThres<-.9
# Set the LSA Rank
lsaRank<-25

# Process the corpus, might be duplicate ticket entries so take the very first incidence of each ticket
tickets<-read.csv(file="exampleTXT.csv")
uTickets<-unique(tickets$Incident.Identifier)
numTickets<-length(uTickets)

saveIndex<-vector()
for (i in 1:numTickets){
  match<-which(tickets$Incident.Identifier==uTickets[i])
  saveIndex[i]<-match[1]
}
tickets<-tickets[saveIndex,]


# Create a corpus based on the first incidence of a ticket (dups have been filtered)
docs<-Corpus(VectorSource(tickets$Incident.Long.Description..CLOB.))
#docs<-Corpus(VectorSource(tickets$Incident.Closure.Code.Description.1))

# Perform some subset of the normal text processing
docs <- tm_map(docs, removePunctuation) 
docs <- tm_map(docs, removeNumbers)  
docs <- tm_map(docs, removeWords, stopwords("english"))   
# docs <- tm_map(docs, stemDocument)
# docs <- tm_map(docs, stripWhitespace) 
# docs <- tm_map(docs, tolower) 
# docs <- tm_map(docs, PlainTextDocument)

# Create the term document matrix
td.mat <- as.matrix(TermDocumentMatrix(docs))

# Create the LSA SPACE
td.mat.lsa <- lw_bintf(td.mat) * gw_idf(td.mat)  # weighting
lsaSpace <- lsa(td.mat.lsa,lsaRank)  # create LSA space

sk<-lsaSpace$sk
dk<-lsaSpace$dk
tk<-lsaSpace$tk

# Create the reduced rank LSA space version of the tdm
#ak<-t(dk)*sk
ak<-tk %*% diag(sk) %*% t(dk)


# Calculate the cosine similarity in LSA space and then set the similarity to 0 for any pairs that do not make the threshold
s<-cosine(ak)
filterIt<-which(s<cosSimThres)
s[filterIt]<-0

# Set the diagonal entries = 0 so we do not display loops in the graph
diag(s)<-0
g<-graph.adjacency(s,mode="undirected",weighted=TRUE)
pLayout<-layout_with_fr(g )
plot(g,layout=pLayout,vertex.size=5,edge.width=E(g)$weight,vertex.label=NA)

# Now find the communities
comsInfo<-cluster_label_prop(g)
comsSize<-sizes(comsInfo)
comsSize<-sort(comsSize,decreasing=TRUE)
#barplot(comsSize)
comsLabel<-names(comsSize)
matches<-which(comsInfo$membership==comsLabel[1])
newCorpus<-docs[matches]
#wordcloud(newCorpus,max.words=5)


# plot(g,layout=pLayout,vertex.size=2,edge.width=E(g)$weight)

# tkplot(g,vertex.color="yellow",vertex.size=3)


