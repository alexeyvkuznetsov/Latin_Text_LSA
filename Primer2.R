
# https://github.com/tifaniwarnita/Document-Similarity/blob/master/Document%20Similarity/doc-sim%20(cosine%20dist).R

# Creating Term Document Matrix
tdm.tf <- TermDocumentMatrix(myCorpus)
tdm.bin <- weightBin(tdm.tf)
tdm.tfidf <- weightTfIdf(tdm.tf, normalize = TRUE)

# Calculate Cosine Distance (Normal)
library(slam)
cosine.dist.bin <- crossprod_simple_triplet_matrix(tdm.bin)/(sqrt(col_sums(tdm.bin^2) %*% t(col_sums(tdm.bin^2))))
cosine.dist.tf <- crossprod_simple_triplet_matrix(tdm.tf)/(sqrt(col_sums(tdm.tf^2) %*% t(col_sums(tdm.tf^2))))
cosine.dist.tfidf <- crossprod_simple_triplet_matrix(tdm.tfidf)/(sqrt(col_sums(tdm.tfidf^2) %*% t(col_sums(tdm.tfidf^2))))

# Calculate Cosine Distance (LSA)
lsa.space.bin <- lsa(tdm.bin)
lsa.space.tf <- lsa(tdm.tf)
lsa.space.tfidf <- lsa(tdm.tfidf)
lsa.mat.bin <- diag(lsa.space.bin$sk) %*% t(lsa.space.bin$dk)
lsa.mat.tf <- diag(lsa.space.tf$sk) %*% t(lsa.space.tf$dk)
lsa.mat.tfidf <- diag(lsa.space.tfidf$sk) %*% t(lsa.space.tfidf$dk)
lsa.cosine.dist.bin <- lsa::cosine(lsa.mat.bin)
lsa.cosine.dist.tf <- lsa::cosine(lsa.mat.tf)
lsa.cosine.dist.tfidf <- lsa::cosine(lsa.mat.tfidf)

# Print Result
cosine.dist.bin
lsa.cosine.dist.bin
cosine.dist.tf
lsa.cosine.dist.tf
cosine.dist.tfidf
lsa.cosine.dist.tfidf

corrplot(lsa.cosine.dist.tfidf, method = "number")



# Creating Term Document Matrix
tdm <- TermDocumentMatrix(myCorpus)
lsaSpace <- lsa(tdm)
# lsaMatrix now is a k x (num doc) matrix, in k-dimensional LSA space
lsaMatrix <- diag(lsaSpace$sk) %*% t(lsaSpace$dk)
# Use the `cosine` function in `lsa` package to get cosine similarities matrix
distMatrix <- cosine(lsaMatrix)

distMatrix

corrplot(distMatrix, method = "number")













#https://github.com/DivyaMaharshi/rsudio_setup_trial/blob/2dc2216155ba6e4ae154cdd5c27df4949a241579/content_similarity.R

td.mat <- TermDocumentMatrix(myCorpus)
# inspect(td.mat[1:10,1:10])

#td.mat<-create_tdm(df)

#------------------------------------------------------------------------------
# MDS with raw term-document matrix compute distance matrix
dist.mat <- dist(t(as.matrix(td.mat)))
#------------------------------------------------------------------------------
# MDS with LSA
lsaSpace <- lsa(td.mat)  # create LSA space
dist.mat.lsa <- dist(t(as.textmatrix(lsaSpace))) 
# compute distance matrix
df.dist=as.matrix(dist.mat.lsa, labels=TRUE)
lsaMatrix <- diag(lsaSpace$sk) %*% t(lsaSpace$dk)
# Use the `cosine` function in `lsa` package to get cosine similarities matrix
# (subtract from 1 to get dissimilarity matrix)
distMatrix <- cosine(lsaMatrix)
corrplot(distMatrix, method = "number")











