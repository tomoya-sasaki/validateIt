# run validation model 
# based on with validateIt

# read AWS key
# aws_key <- read.table("/Users/tomoyasasaki/rootkey.csv", sep = ",")

# load package
library(keyATM)

# function to extract top words for LDA
top_words_lda <- function(x, n = 10, measure = c("probability", "lift")){
  # check_arg_type(x, "keyATM_output")
  if (is.null(n))
    n <- nrow(x$theta)

  measuref <- function(xrow){
    colnames(x$phi)[order(xrow, decreasing = TRUE)[1:n]]
  }

  res <- apply(x$phi, 1, measuref)

  res
}

# load data
# out_key <- readRDS("data/base/fitted/bill_key_extraK0_iter3000_randnum3_paper.rds")
# out_lda <- readRDS("output/base/tmp/bill_lda_extraK0_iter3000_randnum3_paper.rds")

# extract topwords
topic_label <- gsub("^[0-9]+_", "", names(out_key$keywords_raw))

top_key <- top_words(out_key)
top_lda <- top_words_lda(out_lda)

colnames(top_key) <- topic_label
colnames(top_lda) <- topic_label

top_key_con <- apply(top_key, 2, function(x) {y <- gsub("\\[.*\\]", "", x); paste(y, collapse = " ")})
top_lda_con <- apply(top_lda, 2, function(x) {paste(x, collapse = " ")})

K <- ncol(top_key)

#############################################
## setup the pyMturk
#############################################
library(pyMTurkR)
Sys.setenv(AWS_ACCESS_KEY_ID = aws_key[1, 2])
Sys.setenv(AWS_SECRET_ACCESS_KEY = aws_key[2, 2])
options(pyMTurkR.sandbox = T) # change sandbox = F when ready to run on MTurk 
AccountBalance()

##############################################
## Across comparison keyATM VS LDA
##############################################
N <- 10
task_mat <- matrix("", ncol = 3, nrow = N)

for (i in 1:N) {
  j <- sample(1:K, 1)
  task_mat[i, 1] <- topic_label[j]
  task_mat[i, 2] <- top_key_con[j]
  task_mat[i, 3] <- top_lda_con[j]
}


hit_param_names <- c('label', 'word1', 'word2')

# check Mturk 
hit_type <- "3VMQHMXNFUPVGXFL2PY8HCN9THQRPT"
hit_layout <- "31IV0VNEWDWVSOGL43SWYP580MKSWC"

# prepare sending tasks
batch_annotation <- NULL
HITidspath <- NULL
n_assignments <- '1'
expire_in_seconds <- as.character(60 * 60 * 8)
tosend <- task_mat

tasksids <- 1:5 # change this
if (length(tasksids) > N) stop("Task id needs to be smaller than the number of tasks")

current_HIT_ids <- rep(NA, nrow(tosend))
map_ids <- as.data.frame(matrix(NA, nrow = nrow(tosend), ncol = 2))
colnames(map_ids) <- c("tasksids", "Mturkids")

for(i in 1:nrow(tosend)){
  hit_params <- list()
  for(j in 1:length(hit_param_names)){
    hit_params[[j]] <- list(Name = hit_param_names[j],
                            Value = tosend[i, j])
  }
  current_HIT_ids[i] <- suppressMessages(CreateHIT(hit.type = hit_type,
                                                   hitlayoutid = hit_layout,
                                                   hitlayoutparameters = hit_params,
                                                   assignments = n_assignments,
                                                   expiration = expire_in_seconds,
                                                   annotation = batch_annotation,
                                                   verbose = FALSE))$HITId

  map_ids[i,] <- cbind(tasksids[i], current_HIT_ids[i])
}


##############################################
## Within comparison keyATM and LDA respectively
##############################################
N <- 10 # the number of tasks
task_mat <- matrix("", ncol = 5, nrow = N)
task_mat[, 1] <- topic_label[1:N]

correct_answer <- matrix(0, ncol = 2, nrow = N)
ks <- 1:K
for (i in 1:N) {
  j <- sample(1:K, 1) # id for the label
  # fill in the label
  task_mat[i, 1] <- topic_label[j]
  # fill in words

  ks2 <- ks[!ks %in% j]

  k <- sample(ks2, 4) # incorrect answers
  vec <- sample(c(k, j)) # shuffle indexes 
  tmp <- top_key_con[vec] # create a word vector to show
  task_mat[i, 2:5] <- tmps

  correct_answer[i, 1] <- j # true id
  correct_answer[i, 2] <- which(vec == j) # which word is true id
}

hit_param_names <- c('label', 'word1', 'word2', 'word3', 'word4')

# check Mturk 
hit_type <- "3VMQHMXNFUPVGXFL2PY8HCN9THQRPT"
hit_layout <- "3ESX45B0IS06NG20SW3P5S83L5IZJ0"

# prepare for sending tasks
batch_annotation <- NULL
HITidspath <- NULL
n_assignments <- '1'
expire_in_seconds <- as.character(60 * 60 * 8)
tosend <- task_mat
tasksids <- 1:5
if (length(tasksids) > N) stop("Task id needs to be smaller than the number of tasks")

current_HIT_ids <- rep(NA, nrow(tosend))
map_ids <- as.data.frame(matrix(NA, nrow = nrow(tosend), ncol = 2))
colnames(map_ids) <- c("tasksids", "Mturkids")
message('Sending task to MTurk')

# library(pyMTurkR)
# Sys.setenv(AWS_ACCESS_KEY_ID = aws_key[1, 2])
# Sys.setenv(AWS_SECRET_ACCESS_KEY = aws_key[2, 2])
# options(pyMTurkR.sandbox = T) # change sandbox = F when ready to run on MTurk 
# AccountBalance()

for(i in 1:nrow(tosend)){
  hit_params <- list()
  for(j in 1:length(hit_param_names)){
    hit_params[[j]] <- list(Name = hit_param_names[j],
                            Value = tosend[i, j])
  }
  current_HIT_ids[i] <- suppressMessages(CreateHIT(hit.type = hit_type,
                                                   hitlayoutid = hit_layout,
                                                   hitlayoutparameters = hit_params,
                                                   assignments = n_assignments,
                                                   expiration = expire_in_seconds,
                                                   annotation = batch_annotation,
                                                   verbose = FALSE))$HITId

  map_ids[i,] <- cbind(tasksids[i], current_HIT_ids[i])
}


