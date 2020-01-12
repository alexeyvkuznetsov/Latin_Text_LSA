

#https://github.com/Sathiyarajan/data-science-repo-r-py/blob/9a76df4b28f328e7fb43ecd66876e0d834a6d8ac/R/Mastering-R-Programming/Codes/Section%207/7.3.R

tdm_wiki <- TermDocumentMatrix(myCorpus)

# Cosine Similarity.
m <- as.matrix(tdm_wiki)
View(m)
csn <- lsa::cosine(m)
csn


# Run LSA
lsa_out = lsa::lsa(td.mat.tfidf, dims=lsa::dimcalc_share())
lsa_out

# docs_df
docs_mat <- lsa_out$dk[, c(1:2)]
plotmat_docs_df <- as.data.frame(docs_mat)
colnames(plotmat_docs_df) <- c("Dim1", "Dim2")

# k-means cluster the docs dataframe.
set.seed(101)
#clus <- kmeans(plotmat_docs_df)
clus <- kmeans(plotmat_docs_df, 3)
plotmat_docs_df$cluster <- factor(clus$cluster)


# plot documents in ggplot2
# devtools::install_github("slowkow/ggrepel@0.6.2")
library(ggplot2)
library(ggrepel)
ggplot2::ggplot(plotmat_docs_df, aes(x=Dim1, y=Dim2)) +
  ggplot2::geom_point(size=2, aes(color=cluster)) +
  ggrepel::geom_text_repel(aes(label = rownames(plotmat_docs_df)), 
                           data = plotmat_docs_df, size=3) + 
  ggplot2::theme_bw()