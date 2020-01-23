library(stringr)
library(text2vec)
data("movie_review")
# select 1000 rows for faster running times
movie_review_train = movie_review[1:700, ]
movie_review_test = movie_review[701:1000, ]
prep_fun = function(x) {
  # make text lower case
  x = str_to_lower(x)
  # remove non-alphanumeric symbols
  x = str_replace_all(x, "[^[:alpha:]]", " ")
  # collapse multiple spaces
  x = str_replace_all(x, "\\s+", " ")
}
movie_review_train$review = prep_fun(movie_review_train$review)

#Создание векторного пространства

it = itoken(movie_review_train$review, progressbar = FALSE)
v = create_vocabulary(it)
v = prune_vocabulary(v, doc_proportion_max = 0.1, term_count_min = 5)
vectorizer = vocab_vectorizer(v)



dtm = create_dtm(it, vectorizer)
# Tf-Idf
tfidf = TfIdf$new()
dtm_tfidf = fit_transform(dtm, tfidf)
#LSA

lsa = LSA$new(n_topics = 50)
lsa = LSA$new(n_topics = 100)

dtm_tfidf_lsa = fit_transform(dtm_tfidf, lsa)

dtm_tfidf_lsa

#cosine similarity
d1_d2_tfidf_cos_sim = sim2(x = dtm_tfidf_lsa, method = "cosine", norm = "l2")
d1_d2_tfidf_cos_sim[1:2, 1:5]

d1_d2_tfidf_cos_sim

x = dtm_tfidf_lsa[1:250, ]
y = dtm_tfidf_lsa[251:500, ]
head(psim2(x = x, y = y, method = "cosine", norm = "l2"))







# pipe friendly transformation
doc_embeddings =  fit_transform(dtm, tfidf)
doc_embeddings =  fit_transform(doc_embeddings, lsa)

dim(doc_embeddings)

dim(lsa$components)



new_data = movie_review_test
it = itoken(new_data$review, preprocessor = prep_fun, progressbar = FALSE)
new_doc_embeddings = create_dtm(it, vectorizer)
# apply exaxtly same scaling wcich was used in train data
new_doc_embeddings = transform(new_doc_embeddings, tfidf)
# embed into same space as was in train data
new_doc_embeddings = transform(new_doc_embeddings, lsa)
dim(new_doc_embeddings)








library(stringr)
library(text2vec)
data("movie_review")
# select 500 rows for faster running times
movie_review = movie_review[1:500, ]
prep_fun = function(x) {
  # make text lower case
  x = str_to_lower(x)
  # remove non-alphanumeric symbols
  x = str_replace_all(x, "[^[:alnum:]]", " ")
  # collapse multiple spaces
  str_replace_all(x, "\\s+", " ")
}
movie_review$review_clean = prep_fun(movie_review$review)




doc_set_1 = movie_review[1:300, ]
it1 = itoken(doc_set_1$review_clean, progressbar = FALSE)

# specially take different number of docs in second set
doc_set_2 = movie_review[301:500, ]
it2 = itoken(doc_set_2$review_clean, progressbar = FALSE)



it = itoken(movie_review$review_clean, progressbar = FALSE)
v = create_vocabulary(it)
v = prune_vocabulary(v, doc_proportion_max = 0.1, term_count_min = 5)
vectorizer = vocab_vectorizer(v)


dtm = create_dtm(it, vectorizer)
tfidf = TfIdf$new()
dtm_tfidf = fit_transform(dtm, tfidf)


d1_d2_tfidf_cos_sim = sim2(x = dtm_tfidf, method = "cosine", norm = "l2")
d1_d2_tfidf_cos_sim[1:2, 1:5]



lsa = LSA$new(n_topics = 100)
dtm_tfidf_lsa = fit_transform(dtm_tfidf, lsa)



d1_d2_tfidf_cos_sim = sim2(x = dtm_tfidf_lsa, method = "cosine", norm = "l2")
d1_d2_tfidf_cos_sim[1:2, 1:5]


x = dtm_tfidf_lsa[1:250, ]
y = dtm_tfidf_lsa[251:500, ]
head(psim2(x = x, y = y, method = "cosine", norm = "l2"))



setwd("F:/R_test/Isidorus Historia/")

load("TASA.rda")

getwd()



