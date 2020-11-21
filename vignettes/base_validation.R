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
# out_lda <- readRDS("output/base/tmp/bill_lda_extraK0_iter3000_randnum5_paper.rds")

# extract topwords
topic_label <- gsub("^[0-9]+_", "", names(out_key$keywords_raw))

top_key <- top_words(out_key)
top_lda <- top_words_lda(out_lda)

colnames(top_key) <- topic_label
colnames(top_lda) <- topic_label

top_key <- apply(top_key, 2, function(x) {gsub("\\[.*\\]", "", x)})
# top_lda_con <- apply(top_lda, 2, function(x) {paste(x, collapse = " ")})

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
N <- 15
task_mat <- data.frame(matrix("", ncol = 4, nrow = N), stringsAsFactors = FALSE)

set.seed(123)
for (i in 1:N) {
  j <- sample(1:K, 1)
  task_mat[i, 1] <- topic_label[j]
  task_mat[i, 2] <- paste(sample(top_key[, j], 5), collapse = " ")
  task_mat[i, 3] <- paste(sample(top_lda[, j], 5), collapse = " ") 
  task_mat[i, 4] <- as.integer(0)
}

gold_std <- read.csv("data/base/bill_validation_gold_task.csv", header = FALSE, stringsAsFactor = FALSE)
colnames(gold_std) <- paste0("X", 1:4)
# task_mat <- rbind(task_mat %>% data.frame(stringsAsFactors =  FALSE), gold_std)
# shuffle the order of tasks to make sure gold standard tasks appear in the same frequency
task_mat2 <- mixGold(tasks = task_mat, golds = gold_std)

# shuffle the choices in each task
# flip rows if 1
ind <- sample(0:1, size = N + nrow(gold_std), replace = TRUE)

for (i in 1:N) {
  if (ind[i] == 1) {
    word1 <- task_mat2[i, 3] 
    word2 <- task_mat2[i, 2]
    task_mat2[i, 2] <- word1
    task_mat2[i, 3] <- word2
  }
}

task_mat2 <- cbind(task_mat2, ind)

task_recorded <- recordTasks(type = "LI2", tasks = task_mat2, path = "output/base/LI2tasks_test.Rdata")


hit_param_names <- c('label', 'word1', 'word2')

# check Mturk 
hit_type <- "3VMQHMXNFUPVGXFL2PY8HCN9THQRPT"
hit_layout <- "31IV0VNEWDWVSOGL43SWYP580MKSWC"

# prepare sending tasks
batch_annotation <- NULL
HITidspath <- NULL
n_assignments <- '1'
expire_in_seconds <- as.character(60 * 60 * 8)
tosend <- task_mat[, c(1:3)]

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

  k <- sample(ks2, 3) # incorrect answers
  vec <- sample(c(k, j)) # shuffle indexes 
  tmp <- top_key_con[vec] # create a word vector to show
  task_mat[i, 2:5] <- tmp

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


