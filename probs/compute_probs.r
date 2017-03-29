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

## REQUIRES:
##
## model: path to a DSS model.
##
## COMPUTES:
##
## The prior, conjunction, and conditional probabilities for the reduced
## and unreduced vectors .

require(data.table)

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
df.obs <- read.csv(file.obs, sep = " ", head = TRUE, check.names = FALSE)
cat("Reading vectors ...\n", file = stderr())
df.vec <- read.csv(file.vec, sep = " ", head = TRUE, check.names = FALSE)

# number of events
n.events <- length(colnames(df.obs))

# add negated events
for (e in names(df.obs)) {
        df.obs[,paste("not(", e, ")", sep = "")] <- (1 - df.obs[,e])
        df.vec[,paste("not(", e, ")", sep = "")] <- (1 - df.vec[,e])
}

###########################################################################
####               P R I O R   P R O B A B I L I T I E S               ####
###########################################################################

cat("Computing prior probabilities ...\n", file = stderr())
df.prior <- data.frame(
        Type         = rep("prior", 2 * n.events),
        Item         = rep(NA, 2 * n.events),
        Pr_unreduced = rep(NA, 2 * n.events),
        Pr_reduced   = rep(NA, 2 * n.events))
n <- 0
for (e in names(df.obs)) {
        n <- n + 1
        df.prior[n,]$Item         <- e
        df.prior[n,]$Pr_unreduced <- (1 / nrow(df.obs)) * sum(df.obs[,e])
        df.prior[n,]$Pr_reduced   <- (1 / nrow(df.vec)) * sum(df.vec[,e])
}

###########################################################################
####         C O N J U N C T I O N   P R O B A B I L I T I E S         ####
###########################################################################

cat("Computing conjunction probabilities ...\n", file = stderr())
tbl.prior <- data.table(df.prior)
setkey(tbl.prior, Item)
df.conj <- data.frame(
        Type         = rep("conj", 2 * n.events * n.events),
        Item         = rep(NA,     2 * n.events * n.events),
        Pr_unreduced = rep(NA,     2 * n.events * n.events),
        Pr_reduced   = rep(NA,     2 * n.events * n.events))
n <- 0
for (e2 in names(df.obs)) {
        for (e1 in names(df.obs)) {
                # skip negated events as first argument
                if (all(substr(e1, 1, 4) == "not("))
                        next
                n <- n + 1
                cat(paste(n, ":", e1, "^", e2, "\n"))
                df.conj[n,]$Item <- paste(e1, "^", e2, sep = "")
                if (all(e1 == e2)) {
                        df.conj[n,]$Pr_unreduced <- tbl.prior[e1,]$Pr_unreduced
                        df.conj[n,]$Pr_reduced   <- tbl.prior[e1,]$Pr_reduced
                } else {
                        df.conj[n,]$Pr_unreduced <- (1 / nrow(df.obs)) * sum(df.obs[,e1] * df.obs[,e2])
                        df.conj[n,]$Pr_reduced   <- (1 / nrow(df.vec)) * sum(df.vec[,e1] * df.vec[,e2])
                }
        }
}

###########################################################################
####         C O N D I T I O N A L   P R O B A B I L I T I E S         ####
###########################################################################

cat("Computing conditional probabilities ...\n", file = stderr())
tbl.conj <- data.table(df.conj)
setkey(tbl.conj, Item)
df.cond <- data.frame(
        Type         = rep("cond", 2 * n.events * n.events),
        Item         = rep(NA,     2 * n.events * n.events),
        Pr_unreduced = rep(NA,     2 * n.events * n.events),
        Pr_reduced   = rep(NA,     2 * n.events * n.events))
n <- 0
for (e2 in names(df.obs)) {
        prior_unreduced <- tbl.prior[e2,]$Pr_unreduced
        prior_reduced   <- tbl.prior[e2,]$Pr_reduced
        for (e1 in names(df.obs)) {
                # skip negated events as first argument
                if (all(substr(e1, 1, 4) == "not("))
                        next
                n <- n + 1
                cat(paste(n, ":", e1, "|", e2, "\n"))                
                conj <- paste(e1, "^", e2, sep = "")
                df.cond[n,]$Item <- paste(e1, "|", e2, sep = "")
                if (prior_unreduced > 0)
                        df.cond[n,]$Pr_unreduced <- tbl.conj[conj,]$Pr_unreduced / prior_unreduced
                if (prior_reduced > 0)
                        df.cond[n,]$Pr_reduced   <- tbl.conj[conj,]$Pr_reduced   / prior_reduced
        }
}

###########################################################################
###########################################################################

# merge data frames
df <- data.frame()
df <- rbind(df, df.prior)
df <- rbind(df, df.conj)
df <- rbind(df, df.cond)

# write data frame
cat("Writing probabilities ...\n", file = stderr())
write.csv(df, file = file.probs, quote = TRUE, row.names = FALSE)