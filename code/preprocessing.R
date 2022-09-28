library(tm)
library(hash)
library(dplyr)

# read the lines of the input file and only write a line to the output file
# if it's not an empty line and the condition that a random generated number is
# <= than the input ratio that determines the fraction of the file to be stored 
sample_lines <- function(input_file, output_file, ratio) {
    input_con <- file(input_file, "r")
    output_con <- file(output_file, "w")
    
    while(TRUE) {
        line <- readLines(input_con, n=1)
        if (length(line) > 0) {
            if (runif(1) <= ratio) {
                writeLines(line, output_con)
            }
        } else {
            break
        }
    }
    
    close(input_con)
    close(output_con)
}

# sample a reduced sample of the datasets, for each file in the list one keeps a % of the
# number of lines
sample_files <- function() {
    for (f in c("en_US.blogs.txt", "en_US.news.txt", "en_US.twitter.txt")) {
        input_file <- paste("dataset/en_US/", f, sep = "")
        output_file <- paste("dataset/en_US_sample/", f, sep = "")
        sample_lines(input_file, output_file, 0.085)
    }    
}

# load and process the corpus
load_text <- function(dir) {

    textData <- VCorpus(DirSource(directory = dir))
    
    textData <- tm_map(textData, removePunctuation)
    textData <- tm_map(textData, removeNumbers)
    #textData <- tm_map(textData, removeWords, stopwords("SMART"))
    textData <- tm_map(textData, content_transformer(tolower))
    
    # remove all the characters that are not letters or numbers
    toSpace <- content_transformer(function(x, pattern) {
        return (gsub(pattern, " ", x))
    })
    textData <- tm_map(textData, toSpace, "[^a-z']")
    textData <- tm_map(textData, stripWhitespace)
    
    return(textData)
}

# split the data into training and test sets
split_text <- function(textData, trainingRatio) {
    # UPDATE COMMENT
    # initialize two empty lists, one for the training set and another for test set
    # for each document in textData 
    # -split into training and test set
    # store the training set in the training list (and test list)
    # returns training list and test list
    training_set <- character()
    test_set <- character()
    
    for (i in 1:length(textData)) {
        doc <- as.character(textData[[i]])
        set.seed(12345)
        training_indx <- sample.int(length(doc), size = floor(length(doc)*trainingRatio))
        doc_training <- doc[training_indx]
        doc_test <- doc[-training_indx]
        training_set <- c(training_set, doc_training)
        test_set <- c(test_set, doc_test)
    }
    
    return(list(training = training_set, test = test_set))
}

# split a text into tokens
tokenize_text <- function(doc) {
    f <- function(vec) {return(strsplit(vec, " "))}
    doc_split <- sapply(doc, f)
    
    return(doc_split)
}

# returns a dictionary with the counts of ngrams
count_ngrams <- function(tokenized_text, n) {
    f <- function(list) {return(ngrams(list, n))}
    text_ngrams <- sapply(tokenized_text, f)
    
    ngram_dictionary <- hash()
    
    for(doc_index in 1:length(text_ngrams)) {
        if (doc_index %% 1000 == 0) {
            cat(sprintf("%d/%d\n", doc_index, length(text_ngrams)))
        }
        for (ngram in text_ngrams[[doc_index]]) {
            if (prod(nchar(ngram)) > 0) {
                key <- paste(ngram, collapse = " ")
                if (has.key(key, ngram_dictionary)) {
                    ngram_dictionary[[key]] = ngram_dictionary[[key]] + 1
                } else {
                    ngram_dictionary[[key]] = 1
                }
            } 
        }
    }
    return(ngram_dictionary)
}

# create a function that writes the dictionaries into files as data frames
# save_ngrams
save_ngrams <- function(ngram, path) {
    # convert the dictionaries to a data frame
    # save to csv file
    ngram_df <- data.frame(keys = names(ngram), values = values(ngram), 
                           row.names = NULL)
    write.csv(ngram_df, file = path, row.names = FALSE)
}

# create a function that loads the data frames into a hash
# load_ngrams
# filter out the ngrams with low occurrences 
load_ngrams <- function(path, minFreq = 1) {
    ngram_df <- read.csv(path, sep = ",") 
    ngram_df <- filter(ngram_df, values >= minFreq)
    ngram_hash <- hash(ngram_df$keys, ngram_df$values)
    return(ngram_hash)
}

# calculate the conditional probability P(X | Y) and store in a hash
# probability of X given Y
#conditional <- function(x, y) {
#    probability <- hash()
#    
#    for (x_name in names(x)) {
#        x_tokens <- strsplit(x_name, " ")[[1]]
#        x_prefix <- x_tokens[1:(length(x_tokens) - 1)]
#        x_prefix <- paste(x_prefix, collapse = " ")
#        p = x[[x_name]] / y[[x_prefix]]
#        probability[[x_name]] = p
#    }
#    
#    return(probability)
#}

conditional <- function(x, y) {
    
    probabilities <- hash()
    
    for (x_name in names(x)) {
        x_tokens <- strsplit(x_name, " ")[[1]]
        x_prefix <- x_tokens[1:(length(x_tokens) - 1)]
        x_prefix <- paste(x_prefix, collapse = " ")
        x_next <- x_tokens[length(x_tokens)]
        
        if (has.key(x_prefix, probabilities)) {
            p = x[[x_name]] / y[[x_prefix]]
            probabilities[[x_prefix]][[x_next]] = p
        }
        
        else {
            probabilities[[x_prefix]] = hash()
            p = x[[x_name]] / y[[x_prefix]]
            probabilities[[x_prefix]][[x_next]] = p
        }
    }
    
    return(probabilities)
    
}

# partition the (tokenized) training set to enter less data into the count_ngrams
# functions and avoid the stack overflow error
partition <- function(indx_i, indx_f, tokenized_text) {
    part_train <- list()
    
    for (i in 1:length(indx_i:indx_f)) {
        
        part_train[[i]] = tokenized_text[[indx_i + i - 1]]
        }
    
    return(part_train)
}

# merge two dictionaries with ngram/count as key/value pairs
merge_hash <- function(hash_1, hash_2) {
    
    for (hash_1_name in names(hash_1)) {
        if (has.key(hash_1_name, hash_2)){
            hash_2[[hash_1_name]] = hash_2[[hash_1_name]] + hash_1[[hash_1_name]]
        } else {
            hash_2[[hash_1_name]] = hash_1[[hash_1_name]]
        }
    }
    
    return(hash_2)
}














