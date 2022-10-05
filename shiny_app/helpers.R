library(hash)
library(stringi)
library(DescTools)

model_unigrams <- readRDS("unigrams.rds")
model_bigrams <- readRDS("bigrams.rds")
model_trigrams <- readRDS("trigrams.rds")

model <- list(unigrams = model_unigrams, bigrams = model_bigrams, 
              trigrams = model_trigrams)

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
        next_word = names(Large(values(model$unigrams), 1))
        predictions = model$bigrams[[next_word]]
    }
    
    return(predictions)
} 


top_3_words <- function(words, model, k = 3) {
    
    vals = values(predictions_dict(words, model))
    sort_k = sort(Large(vals, k), decreasing = TRUE)
    
    return(names(sort_k))
}




