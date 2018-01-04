##
# reduce_subset.r
#
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

## REQUIRES:
##
## model: path to a DSS model.
##
## DEFINES:
##
## reduce(dims, epochs, write): reduces the dimensionality of a DSS using 
##      subset sampling.
##
##      dims:           desired number of dimensions;
##      epochs:         number of subsetting epochs;
##      write:          boolean flagging whether reduced vectors should;
##                      be saved (TRUE) or not (FALSE).

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

file.obs <- paste(model_fb, ".observations", sep = "")
file.vec <- paste(model_fb, ".vectors",      sep = "")

# read observations
cat("Reading observations ...\n", file = stderr())
df.mtx <- as.matrix(read.csv(file.obs, sep = " ", head = TRUE, check.names = FALSE))

###########################################################################
####                   S U B S E T   S A M P L I N G                   ####
###########################################################################

##
# Reduce the dimensionality of an m x n observation matrix X using subset
# sampling. The following procedure is repeated for "epochs" epochs to
# arrive at an k x n matrix X' (where k < m) that maximally reflects the
# knowledge encoded in the original matrix X:
# 
# (1) Take a subset of k rows of matrix X, and call it X';
#
# (2) Check if all columns of matrix X' are informative, and if the reduced 
#     matrix encodes the same hard constraints as the unreduced matrix,
#     otherwise skip to the next epoch;
#
# (3) Compute the similarity between X and X' on the basis of the 
#     comprehension scores in X and X';
#
# (4) If X' is the best approximation of X so far, store it;
#
# (5) Run next epoch, and rerun from step (1);
#
# (6) If we have reached "epochs", return the best X' found.
##
reduce <- function(dims, epochs, write = FALSE)
{
        cv <- comprh_vector(df.mtx)

        reduced.mtx <- matrix()
        reduced.sim <- 0

        epoch <- 1
        while (epoch <= epochs) {
                cat(paste("Epoch:", epoch), file = stderr())
                
                # (1): Take a subset of k rows of matrix X, and call it X';
                trial.mtx <- df.mtx[sample(1 : nrow(df.mtx), dims, replace = FALSE),]
                trial.cv  <- comprh_vector(trial.mtx)

                # (2): Check if all columns of matrix X' are informative, and
                # if the reduced matrix encodes the same hard constraints as 
                # the unreduced matrix;
                if (!all_informative_vectors(trial.mtx) | !equal_hard_constraints(cv, trial.cv)) {
                        cat("\tBad sample ... Skipping epoch!\n", file = stderr())
                        next
                }

                # (3): Compute the similarity between X and X' on the basis of
                # the comprehension scores in X and X';
                trial.sim <- cor(cv, trial.cv)
                cat(paste("\tSim:", trial.sim), file = stderr())

                # (4): If X' is the best approximation of X so far, store it;
                if (epoch == 1 | trial.sim > reduced.sim) {
                        reduced.mtx <- trial.mtx
                        reduced.sim <- trial.sim
                        cat(" ***", file = stderr())
                }

                cat("\n", file = stderr())

                # (5): Run next epoch, and rerun from step (1);
                epoch <- epoch + 1
        }
        cat(paste("Sim: ", reduced.sim, "\n"), file = stderr())

        # write matrix (if required)
        if (write) {
                colnames(reduced.mtx) <- colnames(df.mtx)
                write.table(reduced.mtx, file.vec, quote = FALSE, sep = " ",
                        col.names = TRUE, row.names = FALSE)
                cat(paste("Wrote:", file.vec, "\n"), file = stderr())
        }
        
        reduced.mtx
}

###########################################################################
###########################################################################

##
# Returns TRUE if all columns are non-zero vectors.
##
all_informative_vectors <- function(mtx)
{
        all(colSums(mtx) > 0)
}

##
# Returns TRUE if comprehension vectors cv1 and cv2 have extreme comprehension
# values (1 or -1) in the exact same positions.
##
equal_hard_constraints <- function(cv1, cv2)
{
        all(replace(cv1, cv1 > -1 & cv1 < 1, 0) == replace(cv2, cv2 > -1 & cv2 < 1, 0))
}

##
# Construct a vector of comprehension scores cs(a,b) for each combination of
# events a and b.
##
comprh_vector <- function(mtx) 
{
        cv <- c()  
        for (i in 1 : ncol(mtx)) {
                for (j in 1 : ncol(mtx)) {
                        cv <- c(cv, comprh_score(mtx[,i], mtx[,j]))
                }
        }
        cv
}

##
# Computes the prior probability of a situation vector v.
##
prior_prob <- function(v)
{
        sum(v) / length(v)
}

##
# Computes the conjuction probability of v1 and v2. In case v1 and v2 are
# identical, the conjunction probability is their prior probability.
##
conj_prob <- function(v1, v2)
{
        pr.conj <- 0
        if (all(v1 == v2)) {
                pr.conj <- prior_prob(v1)
        } else {
                pr.conj <- sum(v1 * v2) / length(v1)
        }
        pr.conj
}

##
# Computes the conditional probablity of v1 given v2.
##
cond_prob <- function(v1, v2)
{
        conj_prob(v1, v2) / prior_prob(v2)
}

##
# Computes the comprehension scores cs(v1,v2).
##
comprh_score <- function(v1, v2)
{
        pr.cond  <- cond_prob(v1, v2)
        pr.prior <- prior_prob(v1)

        cs <- 0
        if (pr.cond > pr.prior) {
                cs <- (pr.cond - pr.prior) / (1.0 - pr.prior)
        } else {
                cs <- (pr.cond - pr.prior) / pr.prior
        }

        cs
}
