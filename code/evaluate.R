source("code/preprocessing.R")


# evaluate the model on the test set
# store the n-grams of the test set to a file
# evaluate the model built with the training set: 
#   for every word in the test set, find the most likely following word 
#   (using kLargest). build all the possible bigrams using the most probable
#   combinations
#   compare with the actual bigrams in the test set