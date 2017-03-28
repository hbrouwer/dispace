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
vec_file <- paste(model_fb, ".vectors", sep ="")

# read observations and vectors
cat("Reading observations ...\n", file = stderr())
df.obs <- read.csv(obs_file, sep = " ", head = TRUE, check.names = FALSE)
cat("Reading vectors ...\n", file = stderr())
df.vec <- read.csv(vec_file, sep = " ", head = TRUE, check.names = FALSE)

# transpose data frame, and convert to matrix
mtx.obs <- as.matrix(df.obs)
mtx.vec <- as.matrix(df.vec)

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
        pr_a <- prior_prob(a)

        if (pr_ab > pr_a) {
                cs <- (pr_ab - pr_a) / (1.0 - pr_a)
        } else {
                cs <- (pr_ab - pr_a) / pr_a
        }

        cs
}

# compute comprehension vectors
cat("Computing comprehension vector (observations) ...\n", file = stderr())
cv.obs <- comprehension_vector(mtx.obs)
cat("Computing comprehension vector (vectors) ...\n", file = stderr())
cv.vec <- comprehension_vector(mtx.vec)

# output comparison
ecs <- equal_hard_constraints(cv.obs, cv.vec)
cat(paste("\nAll hard constraints equal: ", ecs, "\n", sep = ""), file = stderr())
sim <- similarity(cv.obs, cv.vec)
cat(paste("Similarity: ", sim, "\n", sep = ""), file= stderr())
