##
#    ,--.,--.
#  ,-|  |`--' ,---.  ,---.  ,--,--. ,---. ,---.
# ' .-. |,--.(  .-' | .-. |' ,-.  || .--'| .-. :
# \ `-' ||  |.-'  `)| '-' '\ '-'  |\ `--.\   --.
#  `---' `--'`----' |  |-'  `--`--' `---' `----'
#                   `--'
#     "To wander, to roam, move about."
#
# Copyright 2014-2017 Harm Brouwer <me@hbrouwer.eu>
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
##

###########################################################################
###########################################################################

if (!exists("model"))
        stop("'model' not set")
if (!exists("model_fb")) {
        model_nm <- tail(strsplit(model, "/")[[1]], 1)
        model_fb <- paste(model, model_nm, sep = "/")
}

###########################################################################
###########################################################################

obs_file <- paste(model_fb, ".observations", sep = "")

# read observations and vectors
cat("Reading observations ...\n", file = stderr())
df.obs <- read.csv(obs_file, sep = " ", head = TRUE, check.names = FALSE)

# convert data frame to matrix
df.mtx <- as.matrix(df.obs)

##
# Reduce the dimensionality of an m x n observation matrix X using subset
# sampling. The following procedure is repeated for "epochs" epochs to
# arrive at an k x n matrix X' (where k < m) that maximally reflects the
# knowledge encoded in the original matrix X:
# 
#     (1) Take a subset of k rows of matrix X, and call it X'
#
#     (2) Check if all columns of matrix X' are informative, and if
#         the reduced matrix encodes the same hard constraints as
#         the unreduced matrix, otherwise skip to the next epoch
#
#     (3) Compute the similarity between X and X' on the basis of
#         the comprehension scores in X and X'.
#
#     (4) If X' is the best approximation of X so far, store it
#
#     (5) Run next epoch, and rerun from step (1)
#
#     (6) If we have reached "epochs", return the best X' found
##
reduce <- function(dims, epochs, write = FALSE)
{
        cv <- comprehension_vector(df.mtx)

        reduced.mtx <- matrix()
        reduced.sim <- 0

        epoch <- 1
        while (epoch <= epochs) {
                cat(paste("Epoch:", epoch), file = stderr())
                
                # (1): Take a subset of k rows of matrix X, and call it X'
                trial.mtx <- df.mtx[sample(1 : nrow(df.mtx), dims, replace = FALSE),]
                trial.cv  <- comprehension_vector(trial.mtx)

                # (2): Check if all columns of matrix X' are informative, and
                # if the reduced matrix encodes the same hard constraints as 
                # the unreduced matrix
                if (!all_informative_vectors(trial.mtx) | !equal_hard_constraints(cv, trial.cv)) {
                        cat("\tBad sample ... Skipping epoch!\n", file = stderr())
                        next
                }

                # (3): Compute the similarity between X and X' on the basis of
                # the comprehension scores in X and X'.
                trial.sim <- similarity(cv, comprehension_vector(trial.mtx))
                cat(paste("\tSim:", trial.sim), file = stderr())

                # (4): If X' is the best approximation of X so far, store it
                if (epoch == 1 | trial.sim > reduced.sim) {
                        reduced.mtx <- trial.mtx
                        reduced.sim <- trial.sim
                        cat(" ***", file = stderr())
                }

                cat("\n", file = stderr())

                # (5): Run next epoch, and rerun from step (1)
                epoch <- epoch + 1
        }

        cat(paste("Sim: ", reduced.sim, "\n"), file = stderr())

        # write matrix (if required)
        if (write) {
                vec_file <- paste(model_fb, ".vectors", sep = "")
                cat(paste("Wrote:", vec_file, "\n"), file = stderr())
                colnames(reduced.mtx) <- colnames(df.mtx)
                write.table(reduced.mtx, vec_file, quote = FALSE, sep = " ",
                        col.names = TRUE, row.names = FALSE)
        }
        
        reduced.mtx
}

# equal hard constraints
equal_hard_constraints <- function(cv1, cv2)
{
        all(replace(cv1, cv1 > -1 & cv1 < 1, 0) == replace(cv2, cv2 > -1 & cv2 < 1, 0))
}

# returns similarity between two vectors
similarity <- function(v1, v2)
{
        cor(v1, v2)
}

# returns whether all matrix columns are informative
all_informative_vectors <- function(mtx)
{
        all(colSums(mtx) > 0)
}

# returns a vector of comprehension scores
comprehension_vector <- function(mtx) 
{
        cv <- c()
        
        for (i in 1 : ncol(mtx)) {
                for (j in 1 : ncol(mtx)) {
                        cv <- c(cv, comprehension_score(mtx[,i], mtx[,j]))
                }
        }

        cv
}

# prior probability
prior_prob <- function(a)
{
        sum(a) / length(a)
}

# conjunction probability
conj_prob <- function(a, b)
{
        if (all(a == b)) {
                return(prior_prob(a))
        } else {
                return(sum(a * b) / length(a))
        }
}

# conditional probability
cond_prob <- function(a, b)
{
        conj_prob(a, b) / prior_prob(b)
}

# comprehension score
comprehension_score <- function(a, b)
{
        pr_ab <- cond_prob(a, b)
        pr_a  <- prior_prob(a)

        if (pr_ab > pr_a) {
                cs <- (pr_ab - pr_a) / (1.0 - pr_a)
        } else {
                cs <- (pr_ab - pr_a) / pr_a
        }

        cs
}