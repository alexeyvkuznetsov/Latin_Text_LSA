##Trying to fuck around with programming LSA
##Hail Mary, Mother, Full of Grace, Protect us in our Hour of Need
##Libraries needed for this script: "tm" and dependencies, "slam", "SnowballC", "lsa" and "ggplot2"

#Working Dir.
setwd("C:/Users/Caleb/Desktop/Fall 2016/Lab Work")
mydir <- ("C:/Users/Caleb/Desktop/Fall 2016/Lab Work")

#Preparing package "tm"
install.packages("tm",dependencies=TRUE)

#Load Libraries
library("tm", lib.loc="~/R/win-library/3.2")
library("SnowballC", lib.loc="~/R/win-library/3.2")
library("lsa", lib.loc="~/R/win-library/3.2")
library("ggplot2", lib.loc="~/R/win-library/3.2")

#Import files and save as Corpus
text <- readLines("Isaiah_1_NoFrills.txt")
docs <- Corpus(VectorSource(text))

# read files into a document-term matrix
myMatrix = textmatrix("Isaiah_1_NoFrills.txt", minWordLength = 1, stopwords = NULL, stemming = TRUE)
myMatrix

# Not sure if this is necessary...? 
myMatrix = lw_logtf(myMatrix) * gw_idf(myMatrix)

##Here is where I got to before everything went to hell. Something is wrong with the subscript.
#This is the error the code returned: Error in SVD$u[, 1:dims] : subscript out of bounds

# create the latent semantic space
myLSAspace = lsa(myMatrix, dims=dimcalc_raw())

# display it as a textmatrix again
round(as.textmatrix(myLSAspace),2) # should give the original
# create the latent semantic space
myLSAspace = lsa(myMatrix, dims=dimcalc_share())
# display it as a textmatrix again
myNewMatrix = as.textmatrix(myLSAspace)
myNewMatrix # should look be different!
# compare two terms with the cosine measure
cosine(myNewMatrix["dog",], myNewMatrix["cat",])
# compare two documents with pearson
cor(myNewMatrix[,1], myNewMatrix[,2], method="pearson")
# clean up
unlink(td, recursive=TRUE)
