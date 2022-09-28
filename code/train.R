#source("code/preprocessing.R")
source("/Users/sariaslaso/Google Drive/courses/data science specialization/capstone project/code/preprocessing.R")

# read the data from a file
#corpus_text <- load_text("dataset/en_US_sample/")
corpus_text <- load_text("/Users/sariaslaso/Google Drive/courses/data science specialization/capstone project/dataset/en_US_sample/")

# split into training and test set
# define what ratio of the data will be used as training data
ratio = 0.7
corpus_split <- split_text(corpus_text, ratio)
training_set <- corpus_split$training
test_set <- corpus_split$test

# tokenize the training set
token_training <- tokenize_text(training_set)
# tokenize the test set
token_test <- tokenize_text(test_set)


# store the count of ngrams in a dictionary
unigrams <- count_ngrams(token_training, n = 1)
bigrams <- count_ngrams(token_training, n = 2)
trigrams <- count_ngrams(token_training, n = 3)


# use save_ngram to write the n-grams, extracted from a dictionary to a 
# data set, to a file
#model_dir <- "model"
model_dir <- "/Users/sariaslaso/Google Drive/courses/data science specialization/capstone project/model"

save_ngrams(unigrams, paste(model_dir, "unigrams_train.csv", sep = "/"))
save_ngrams(bigrams, paste(model_dir, "bigrams_train.csv", sep = "/"))
save_ngrams(trigrams, paste(model_dir, "trigrams_train.csv", sep = "/"))







