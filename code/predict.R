source("code/preprocessing.R")

library(DescTools)
library(stringi)

# load model
minFrequency = 20
model_dir <- "model"

# load the n-grams from a file
load_model <- function(model_dir, minFrequency) {
    unigrams <- load_ngrams(paste(model_dir, "unigrams_train.csv", sep = "/"), minFrequency)
    bigrams <- load_ngrams(paste(model_dir, "bigrams_train.csv", sep = "/"), minFrequency)
    trigrams <- load_ngrams(paste(model_dir, "trigrams_train.csv", sep = "/"), minFrequency)
    
    bigrams_prob <- conditional(bigrams, unigrams)
    trigrams_prob <- conditional(trigrams, bigrams)
    
    return(list(unigrams = unigrams, bigrams = bigrams_prob, trigrams = trigrams_prob))
}

model <- load_model(model_dir, minFrequency)

# save the model data to a file in the shiny_app directory
saveRDS(model$unigrams, file = "shiny_app/unigrams.rds")
saveRDS(model$bigrams, file = "shiny_app/bigrams.rds")
saveRDS(model$trigrams, file = "shiny_app/trigrams.rds")



# determine the k(= 3) most likely words that follow an ngram (n = 1, 2)
# used to test the model and see how accurately predicts the test set
predictions_dict <- function(words, model, k = 3) {
    
    tokens <- unlist(strsplit(words, " "))
    l = length(tokens)
    
    predictions = hash()
    
    if (l == 1) {
        prefix = tokens[l]
        prediction_hash = model$bigrams
        
    } else {
        prefix = paste(tokens[(l-1):l], collapse = " ")
        prediction_hash = model$trigrams
    }
    
    if (has.key(prefix, prediction_hash)) {
        
        predictions = prediction_hash[[prefix]]
        
    } else {
        # what to return in case the entered word is not in the dictionary of
        # conditional probabilities
        
        # use the most common unigram and return a dictionary with all the
        # possible bigrams of word and their probabilities
        next_word = names(Large(values(model$unigrams), 1))
        predictions = model$bigrams[[next_word]]
    }
    
    return(predictions)
}    


# from the dictionary of predictions returned by top_3_words
# select the predictions with the three highest probabilities
top_3_words <- function(words, model, k = 3) {
    
    vals = values(predictions_dict(words, model))
    sort_k = sort(Large(vals, k), decreasing = TRUE)
    
    return(sort_k)
}


# evaluate the model on the test set
# store the n-grams of the test set to a file
# evaluate the model built with the training set: 
#   for every word in the test set, find the most likely following word 
#   (using kLargest). build all the possible bigrams using the most probable
#   combinations
#   compare with the actual bigrams in the test set
# calculate perplexity and accuracy of the test set

# load the tokenized test set
# calculate perplexity of the trained model

# to be called using unlist of the tokenized test set
model_perplexity <- function(text_entry, model) {
    
    exponent = 0
    N = length(text_entry)
    delta = 1e-6
        
    for (indx in 1:(N-1)) {
        # dictionary of all the possible next words with their probabilities
        probs = predictions_dict(text_entry[[indx]], model)
        next_word = text_entry[[indx + 1]]
        #print(paste(indx, next_word, sep = " "))
        
        if (has.key(next_word, probs)) {
            
            q_i = probs[[next_word]] + delta
            
        } else {
            
            q_i = delta
        }
        
        exponent = exponent + log2(q_i)
        perplexity = 2 ** (-1/N * exponent)
        
    }   
    
    return(perplexity / N)
}

# calculate accuracy
# determines which fraction of the test set is predicted
# text_entry would be the tokenized test set (token_test) with unlist()

accuracy_at_3 <- function(text_entry, model) {
    predicted_count = 0
    N = length(text_entry)
    pred_map = hash()
    
    for (indx in 1:(N-1)) {
        if (stri_isempty(text_entry[[indx]])) {
            next
        }
        
        if (indx %% 100 == 0) {
            cat(sprintf("%d/%d\n", indx, N))
        }
        
        if (has.key(text_entry[[indx]], pred_map)) {
            nw_pred = pred_map[[text_entry[[indx]]]]
            predicted_count <- predicted_count + 1
        }
        
        else {
            top_words = top_3_words(text_entry[[indx]], model)
            if (text_entry[[indx + 1]] %in% names(top_words)) {
                pred_map[[text_entry[[indx]]]] = text_entry[[indx + 1]]
                predicted_count <- predicted_count + 1
            }
        }
    }
    
    return(predicted_count / N)
}

accuracy <- function(text_entry, model) {
    
    predicted_count = 0
    N = length(text_entry)
    pred_map = hash()
    
    for (indx in 1:(N-1)) {
        
        #print(text_entry[[indx]])
        if (stri_isempty(text_entry[[indx]])) {
            next
        }
        
        if (indx %% 100 == 0) {
            cat(sprintf("%d/%d\n", indx, N))
        }
        
        if (has.key(text_entry[[indx]], pred_map)) {
            nw_pred = pred_map[[text_entry[[indx]]]]
        } 
        
        else {
            top_words = names(top_3_words(text_entry[[indx]], model))
            nw_pred = top_words[[1]]
            pred_map[[text_entry[[indx]]]] = nw_pred
        }
        
        if (nw_pred == text_entry[[indx + 1]]) {
            predicted_count <- predicted_count + 1.0
        }
        
    }
    
    return(predicted_count / N)
}


















