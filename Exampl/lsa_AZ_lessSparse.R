library(tm)
library(lsa)
library(ggplot2)

load("C:/Users/mcoyne/Documents/R/basicTm/capstone/processData/AZ_df.corpus.clean.Rdata")
AZ.tdm <- TermDocumentMatrix (AZ_df.corpus.clean);
## Remove sparsity
AZ.tdm.sp <- removeSparseTerms  (AZ.tdm, sparse= 0.85)

dsName <- "AZ"

## remove all columns that contain only Zero
xa <- as.matrix(AZ.tdm.sp)
ind <- colSums(xa!=0)

AZ.tdm.sp.new <- AZ.tdm.sp[, ind >0]

## clock date and time when it starts 
start_dt <- paste0 ("START building LSA Space for <", dsName, "> at: ", Sys.time())
start_dt
LSAspace <- lsa(AZ.tdm.sp.new, dims=dimcalc_raw())
# Clock the date and time when it ends
end_dt <- paste0 ("END building LSA Space for <", dsName, "> at: ", Sys.time())
end_dt


myMatrix <- as.textmatrix(LSAspace)

# A = U . D . t(V)
s = svd(myMatrix)
D = diag(s$d)

  
#  lsvd <- LSAspace$tk
#dia <- diag(LSAspace$sk)  ## This is same as D= diag(s$d)
#rsvd <- t(LSAspace$dk)
#DocSimilarity <- rsvd %*% dia
#WordSimilarity <- lsvd %*% dia

#C <- round(lsvd %*% dia %*% rsvd) 

## C should have same dimention as tdm
#svd.tdm <- C %*% t(C)

dist.lsa <- dist(t(myMatrix))


fit <- cmdscale(dist.lsa, eig = TRUE, k=2)
## ==========================================================
## Since it takes so long to calculate, save these R objects
## ==========================================================
LSAspace.AZ.tdm.sp.new <- LSAspace
dist.lsa.AZ.tdm.sp.new <- dist.lsa
save(LSAspace.AZ.tdm.sp.new,file='C:/Users/mcoyne/Documents/R/basicTm/capstone/processData/LSAspace_AZ.tmd.sp.new.Rdata')
save(dist.lsa.AZ.tdm.sp.new,file='C:/Users/mcoyne/Documents/R/basicTm/capstone/processData/dist.lsa.AZ.tmd.sp.new.Rdata')


# Business score
## ===========================================================
## Graph the distance of LSA, the color based on Business stars
## The second graph where the score based on review stars
## ===========================================================
from = 1
toid = dim(myMatrix)[2]

docids <- colnames(myMatrix)[from:toid]
b_score <- matrix(data=NA, nrow=toid)
for(i in from:toid) {
  b_score[i] = AZ_df$b_stars[i]
}

title <- paste0("Distance as calculated by LSA. Colored by Business stars.  Data: Healthcare, doctor, physician business for state Arizona (AZ)");
points <- data.frame(x=fit$points[from:toid,1], y=fit$points[from:toid,2], b_score=b_score)
ggplot (points, x=x , y=y) +
  geom_point(data=points, aes(x=x, y=y, colour=factor(b_score))) +
  scale_color_manual(values=c("#66c2a5","#f781bf", "#a65628","#011ab8","#ff7f00",	"#984ea3","#4daf4a","#377eb8", "#e41a1c"))+
  ggtitle(title) +
  theme(plot.title=element_text(size=14, family="Trebuchet MS", face="bold")) 
  #geom_text(data =points, aes(x=x, y=y-0.5), label=row.names(points))


## review score
from = 1
toid = dim(myMatrix)[2]

docids <- colnames(myMatrix)[from:toid]
r_score <- matrix(data=NA, nrow=toid)
for(i in from:toid) {
  r_score[i] = AZ_df$r_stars[i]
}


title <- paste0("Distance as calculated by LSA. Colored by Review stars.  Data: Healthcare, doctor, physician business for state Arizona (AZ)");
points <- data.frame(x=fit$points[from:toid,1], y=fit$points[from:toid,2], b_score=b_score)
ggplot (points, x=x , y=y) +
  geom_point(data=points, aes(x=x, y=y, colour=factor(r_score))) +
  scale_color_manual(values=c("#1b9e77","#d95f02","#7570b3","#e7298a","#66a61e")) +
  ggtitle(title) +
  theme(plot.title=element_text(size=14,  family="Trebuchet MS", face="bold")) 
#geom_text(data =points, aes(x=x, y=y-0.5), label=row.names(points))















