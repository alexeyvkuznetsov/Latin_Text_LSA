library(tm)
library(lsa)
## ===================================================================
## Working with entire dataset for AZ in heath business
## ===================================================================

load('C:/Users/mcoyne/Documents/R/basicTm/capstone/processData/AZ_df.Rdata')
AZ_df.corpus <- Corpus(VectorSource(AZ_df$r_text))
## ===================================================================
## Using the corpus to regenerate the tdm
## ===================================================================
myStopwords <- c(stopwords("english"), "the", "and","she", "you", "they","ive");
#idx <- which(myStopwords=="not");  ## remove Not from stopword
#myStopwords <- myStopwords[-idx];
## clean corpus
## AZ
source('~/R/basicTm/capstone/src/dtm_tdm/clean.R')
AZ_df.corpus.clean <- clean(AZ_df.corpus, myStopwords)
AZ.dtm <- DocumentTermMatrix(AZ_df.corpus.clean);

## Remove sparsity
AZ.dtm.sp <- removeSparseTerms  (AZ.dtm, sparse= 0.85)

## ===================================================================
## Calculate LSA Space
## ===================================================================

dsName <- "AZ"
## Let LSA calculate the dimesion
## Strange:  using AZ.tdm.sp does not work ???
tdm <- AZ.tdm
## clock date and time when it starts 
start_dt <- paste0 ("START building LSA Space for <", dsName, "> at: ", Sys.time())
start_dt
LSAspace <- lsa(AZ.dtm.sp, dims=dimcalc_raw())
# Clock the date and time when it ends
end_dt <- paste0 ("END building LSA Space for <", dsName, "> at: ", Sys.time())
end_dt


## ==============================================
## look at the LSA results
## ==============================================

s = svd(myMatrix)
D = diag(s$d)
# A = T . S . t(D)   This is R lsa notatioT 
T <- LSAspace$tk   
S <- LSAspace$sk
dia <- diag(S)
tD <- LSAspace$dk
D  <- t(tD)


DocSim <- tD %*% dia
WordSim <- T %*% dia 

DocSim_2 <- D %*% dia

##=========================================================
## How close the terms that make up LSASpace
## ========================================================

myMatrix <- as.textmatrix(LSAspace)
dist.lsa <- dist(t(myMatrix))
fit <- cmdscale(dist.lsa, eig = TRUE, k=2)

points <- data.frame(x=fit$points[,1], y=fit.$points[,2])
ggplot (points, x=x , y=y) +
  geom_point(data=points, aes(x=x, y=y, colour="orange")) +
  geom_text(data =points, aes(x=x, y=y-0.5), label=row.names(points), angle=20, size=4)

points.1 <- subset(points, (x <= 0 & y >=0) )
ggplot (points.1, x=x , y=y) +
  geom_point(data=points.1, aes(x=x, y=y, colour="orange")) +
  geom_text(data =points.1, aes(x=x, y=y-0.5), label=row.names(points.1), angle=20, size=4)

##=========================================================
## How close the docs are
## ========================================================
dist.lsa.d <- dist(myMatrix)
fit.d <- cmdscale(dist.lsa.d, eig=TRUE, k=2)
fit.d.kis46 <- cmdscale(dist.lsa.d, eig=TRUE, k=46)

points.d <- data.frame(x=fit.d$points[,1], y=fit.d$points[,2], r_stars=AZ_df$r_stars, bid=AZ_df$bid, r_id=AZ_df$r_id, b_stars=AZ_df$b_stars )
ggplot (points.d, x=x , y=y) +
  geom_point(data=points.d, aes(x=x, y=y,colour=factor(r_stars))) + 
  scale_color_manual(values=c("#e7298a","#551A8B","#00ffff","#0000FF","#66a61e"))

## ======================================================
## subset only 1, 5 star review
## =====================================================
points.d.r1and5 <- subset (points.d, r_stars== c(1,5))
ggplot (points.d.r1and5, x=x , y=y) +
  geom_point(data=points.d.r1and5, aes(x=x, y=y,colour=factor(r_stars))) + 
  scale_color_manual(values=c("#e7298a","#66a61e"))

## =====================================================
## subset points.d.r1and5 to x <= 5 and x > 5
## =====================================================
points.d.r1and5.1 <- subset (points.d.r1and5, x <= 5.0 )
ggplot (points.d.r1and5.1, x=x , y=y) +
  geom_point(data=points.d.r1and5.1, aes(x=x, y=y,colour=factor(r_stars))) + 
  scale_color_manual(values=c("#e7298a","#66a61e"))

points.d.r1and5.1.1 <- subset (points.d.r1and5, x <= 0.0 )
ggplot (points.d.r1and5.1.1, x=x , y=y) +
  geom_point(data=points.d.r1and5.1.1, aes(x=x, y=y,colour=factor(b_stars))) + 
  scale_color_manual(values=c("#7f3b08","#b35806","#e08214","#fdb863","#fee0b6","#f7f7f7","#d8daeb","#b2abd2","#8073ac","#542788","#2d004b"))

## out of points.d.r1and5.1.1, take only business rank 1 and 5
## NOT working, cannot separate b_stars
points.d.r1and5.1.1.b1and5 <- subset (points.d.r1and5.1.1, as.numeric(b_stars)<= 1.0 )
x <- subset (points.d.r1and5.1.1, as.numeric(b_stars) == as.numeric(5.0) )
points.d.r1and5.1.1.b1and5  <- rbind(points.d.r1and5.1.1.b1and5 ,x)
ggplot (points.d.r1and5.1.1.b1and5, x=x , y=y) +
  geom_point(data=points.d.r1and5.1.1, aes(x=x, y=y,colour=factor(r_stars))) + 
  scale_color_manual(values=c("#e08214","#2d004b"))

points.d.r1and5.1.1.2 <- subset (points.d.r1and5.1.1, x <= -1.75 )
ggplot (points.d.r1and5.1.1.b1and5, x=x , y=y) +
  geom_point(data=points.d.r1and5.1.1.2, aes(x=x, y=y,colour=factor(r_stars))) + 
  scale_color_manual(values=c("#e08214","#2d004b"))

## =================================================================
## looking for two review ids:
## NbcYFZRNBAlkzJHWtKgpZQ -- rank 5
## -d1Sl2KzWUIBsXOxH_0jdQ -- rank 1
##==================================================================
points.seek <- subset(points.d, (points.d$r_id == '-d1Sl2KzWUIBsXOxH_0jdQ' | points.d$r_id == 'NbcYFZRNBAlkzJHWtKgpZQ') )
ggplot (points.seek, x=x , y=y) +
  geom_point(data=points.seek, aes(x=x, y=y,colour=factor(r_stars))) + 
  scale_color_manual(values=c("#e08214","#2d004b"))


ggplot (points.d, x=x , y=y) +
  geom_point(data=points.d, aes(x=x, y=y,colour=factor(r_stars)), alpha=0.3) + 
  geom_point(data=points.seek, aes(x=x, y=y,colour=factor(r_stars)), size=10, shape=17) + 
  scale_color_manual(values=c("#e7298a","#551A8B","#00ffff","#0000FF","#66a61e"))

## ==================================================================
## since call and back are in k=4,5 will try to fit 5
## k =4 is back; k=5 is call
## ==================================================================
## dist.lsa.d <- dist(myMatrix)
#fit.d.6 <- cmdscale(dist.lsa.d, eig=TRUE, k=6)
points.d.6<- data.frame(x=fit.d.6$points[,4]
                         , y=fit.d.6$points[,5]
                         , r_stars=AZ_df$r_stars
                         , bid=AZ_df$bid
                         , r_id=AZ_df$r_id
                         , b_stars=AZ_df$b_stars )

points.d.6.seek <- subset(points.d.6, (points.d.6$r_id == '-d1Sl2KzWUIBsXOxH_0jdQ' | points.d.6$r_id == 'NbcYFZRNBAlkzJHWtKgpZQ') )

title <- paste0("Fig. 5 - Review Text Similarity (clusters), k=6")
ggplot (points.d.6, x=x , y=y) +
  geom_point(data=points.d.6, aes(x=x, y=y,colour=factor(r_stars)), alpha=0.3) + 
  geom_point(data=points.d.6.seek, aes(x=x, y=y,colour=factor(r_stars)), size=8, shape=17) +
  ggtitle(title) +
  scale_color_manual(values=c("#e7298a","#551A8B","#00ffff","#0000FF","#66a61e"))












