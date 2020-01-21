#Load Libraries
library("tm")
library("SnowballC")
library("lsa")
library("ggplot2")

#Set Pathname
pathname = "C:/Users/Caleb/Desktop/Fall 2016/Lab Work/Isaiah Source Texts/Work Folder"

# read files into a document-term matrix
myMatrix = textmatrix(pathname, 
                      minWordLength = 1, 
                      stopwords = stopwords_en, 
                      stemming = TRUE,
                      removeXML = TRUE,
                      removeNumbers = TRUE)
myMatrix = lw_logtf(myMatrix) * gw_idf(myMatrix)

#Better to use CSV to see all words/frequencies
#freqmatrix = table(myMatrix)
#View(freqmatrix)
write.csv(myMatrix, "LSAWordList.csv")

# create the latent semantic space
myLSAspace = lsa(myMatrix, dims=dimcalc_share())

# display it as a textmatrix again for viewing
checkvalues = table(round(as.textmatrix(myLSAspace),2)) # should give the original
View(checkvalues)

#Convert LSA to Text-Frequency Vector Matrix for Cosines
myNewMatrix = as.textmatrix(myLSAspace)
NewMatrixtable = table(myNewMatrix)
View(NewMatrixtable)

##Playing around with this stuff
NMatrixTable = table(myNewMatrix)
View(NMatrixTable)

# compare two terms with the cosine measure
#Working towards terms to use here:
cosine(myNewMatrix["lord",], myNewMatrix["god",])
cosinematrix = cosine(myNewMatrix)

# compare two documents with pearson
cor(myNewMatrix[,1], myNewMatrix[,2], method="pearson")