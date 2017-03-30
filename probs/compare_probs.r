##
# compare_probs.r
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
## COMPUTES:
##
## (1) The similarity of comprehension scores derived from reduced and
##     unreduced situation vectors, and whether they these scores encode the 
##     same hard constraints.
##
## (2) Correlations of the prior, conjunction, and conditional probabilities
##     as derived from the reduced and unreduced vectors.

require(lattice)

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

file.obs   <- paste(model_fb, ".observations",  sep = "")
file.vec   <- paste(model_fb, ".vectors",       sep = "")
file.probs <- paste(model_fb, ".probabilities", sep = "")

# read observations and vectors
cat("Reading observations ...\n", file = stderr())
mtx.obs <- as.matrix(read.csv(file.obs, sep = " ", head = TRUE, check.names = FALSE))
cat("Reading vectors ...\n", file = stderr())
mtx.vec <- as.matrix(read.csv(file.vec, sep = " ", head = TRUE, check.names = FALSE))

# read probablities
cat("Reading probabilities ...\n", file = stderr())
df.probs <- read.csv(file.probs, head = TRUE)

df.prior <- df.probs[df.probs$Type == "prior",]
df.conj  <- df.probs[df.probs$Type == "conj",]
df.cond  <- df.probs[df.probs$Type == "cond",]

###########################################################################
###########################################################################

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

###########################################################################
###########################################################################

# compute comprehension vectors
cat("Computing comprehension vector (observations) ...\n", file = stderr())
cv.obs <- comprh_vector(mtx.obs)
cat("Computing comprehension vector (vectors) ...\n", file = stderr())
cv.vec <- comprh_vector(mtx.vec)

# report comparison of comprehension vectors
cat(paste("\nSimilarity: ", cor(cv.obs, cv.vec), "\n", sep = ""), file= stderr())
cat(paste("\nAll hard constraints equal: ", equal_hard_constraints(cv.obs, cv.vec),
        "\n", sep = ""), file = stderr())

# scatterplots prior probabilities
quartz()
print(xyplot(df.prior$Pr_reduced ~ df.prior$Pr_unreduced,
        main = paste("r =", signif(cor(df.prior$Pr_reduced, df.prior$Pr_unreduced), 3)),
        xlab = expression(paste("Unreduced ", tau, "(b)")),
        ylab = expression(paste("Reduced ", tau, "(b)"))))
cat(paste("\ncor(priors): ", cor(df.prior$Pr_reduced, df.prior$Pr_unreduced),
        "\n", sep = ""), file= stderr())

# scatterplots conjunction probabilities
quartz()
print(xyplot(df.conj$Pr_reduced ~ df.conj$Pr_unreduced,
        main = paste("r =", signif(cor(df.conj$Pr_reduced, df.conj$Pr_unreduced), 3)),
        xlab = expression(paste("Unreduced ", tau, "(a&b)")),
        ylab = expression(paste("Reduced ", tau, "(a&b)"))))
cat(paste("\ncor(conj):  ", cor(df.conj$Pr_reduced, df.conj$Pr_unreduced),
        "\n", sep = ""), file= stderr())

# scatterplots conditional probabilities
quartz()
print(xyplot(df.cond$Pr_reduced ~ df.cond$Pr_unreduced,
        main = paste("r =", signif(cor(df.cond$Pr_reduced, df.cond$Pr_unreduced), 3)),
        xlab = expression(paste("Unreduced ", tau, "(a|b)")),
        ylab = expression(paste("Reduced ", tau, "(a|b)"))))
cat(paste("\ncor(cond):  ", cor(df.cond$Pr_reduced, df.cond$Pr_unreduced),
        "\n", sep = ""), file= stderr())
