##
# event_probs.r
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
## model_reduced: boolean flagging whether reduced probabilities should be
##      used (TRUE) or not (FALSE).
##
## DEFINES:
##
## prior_probs(): plots the prior Pr(a) probabilities in the world.
##
## conj_probs(e): plots the conjuction probabilities Pr(e & B) and Pr(A & e)
##      for the specified event e
##
## cond_probs(e): plots the conditional probabilities Pr(e | B) and Pr(A | e)
##      for the specified event e.
##
## comprh_scores(e): plots the comprehension scores cs(e,B) and Pr(A,e) for
##      the specified event e.
##
## comprh_scores_heatmap(): plots a heatmap of the comprehension scores cs(a,b)
##      for every combination of events a and b.
##
## comprh_scores_diff_heatmap(): plots a heatmap of the difference in 
##      comprehension scores cs(a,b) for every combination of events a and b as 
##      determined from the reduced and unreduced probabilities.

require(ggplot2)
require(grid)
require(gridExtra)
require(reshape)
require(data.table)

###########################################################################
###########################################################################

if (!exists("model"))
        stop("'model' not set")
if (!exists("model_fb")) {
        model_nm <- tail(strsplit(model, "/")[[1]], 1)
        model_fb <- paste(model, model_nm, sep = "/")
}
if (!exists("model_reduced"))
        stop("'model_reduced' not set")

###########################################################################
###########################################################################

cat("Reading probabilities ...\n", file = stderr())
df <- read.csv(paste(model_fb, ".probabilities", sep = ""), head = TRUE)

# probabilities from unreduced or reduced vectors
if (!model_reduced) {
        df$Pr <- df$Pr_unreduced
} else {
        df$Pr <- df$Pr_reduced
}

###########################################################################
###########################################################################

## 
# Plots the prior Pr(a) probabilities in the world.
##
prior_probs <- function()
{
        df.prior <- df[df$Type == "prior" & substr(df$Item,1,4) != "not(",]

        plot <- ggplot(df.prior, aes(x = Item, y = Pr, fill = Pr))
        plot <- plot + geom_bar(stat = "identity", position = "stack")
        plot <- plot + guides(fill = FALSE)
        plot <- plot + ggtitle("Pr(a)")
        plot <- plot + xlab("Event a")
        plot <- plot + ylab("Probability")
        plot <- plot + theme_update(axis.text.x = element_text(angle = 90, hjust = 1))

        quartz()
        plot
}

##
# Plots the conjuction probabilities Pr(e & B) and Pr(A & e) for the
# specified event e.
##
conj_probs <- function(event)
{
        df.conj <- df[df$Type == "conj",]

        df.conj$A <- sapply(strsplit(as.character(df.conj$Item), "\\^"), "[", 1)
        df.conj$B <- sapply(strsplit(as.character(df.conj$Item), "\\^"), "[", 2)
        
        df.conj1 <- df.conj[df.conj$A == event & substr(df.conj$B, 1, 4) != "not(",] # (event & B)
        df.conj2 <- df.conj[df.conj$B == event,]                                     # (A & event)

        # plot (event & B)
        plot1 <- ggplot(df.conj1, aes(x = B, y = Pr, fill = Pr))
        plot1 <- plot1 + geom_bar(stat = "identity", position = "stack")
        plot1 <- plot1 + guides(fill = FALSE)
        plot1 <- plot1 + ggtitle(paste("Pr(", event, " & b)", sep = ""))
        plot1 <- plot1 + xlab("Event b")
        plot1 <- plot1 + ylab("Probability")
        plot1 <- plot1 + theme_update(axis.text.x = element_text(angle = 90, hjust = 1))

        # plot (A & event)
        plot2 <- ggplot(df.conj2, aes(x = A, y = Pr, fill = Pr))
        plot2 <- plot2 + geom_bar(stat = "identity", position = "stack")
        plot2 <- plot2 + guides(fill = FALSE)
        plot2 <- plot2 + ggtitle(paste("Pr(a & ", event, ")", sep=""))
        plot2 <- plot2 + xlab("Event a")
        plot2 <- plot2 + ylab("Probability")
        plot2 <- plot2 + theme_update(axis.text.x = element_text(angle = 90, hjust = 1))

        quartz()
        grid.arrange(plot1, plot2, nrow = 2, top = paste("Conjunction probabilities for", event))
}

##
# Plots the conditional probabilities Pr(e | B) and Pr(A | e) for the
# specified event e.
##
cond_probs <- function(event)
{
        df.cond <- df[df$Type == "cond",]

        df.cond$A <- sapply(strsplit(as.character(df.cond$Item), "\\|"), "[", 1)
        df.cond$B <- sapply(strsplit(as.character(df.cond$Item), "\\|"), "[", 2)

        df.cond1 <- df.cond[df.cond$A == event & substr(df.cond$B, 1, 4) != "not(",] # (event | B)
        df.cond2 <- df.cond[df.cond$B == event,]                                     # (A | event)

        # plot (event | B)
        plot1 <- ggplot(df.cond1, aes(x = B, y = Pr, fill = Pr))
        plot1 <- plot1 + geom_bar(stat = "identity", position = "stack")
        plot1 <- plot1 + guides(fill = FALSE)
        plot1 <- plot1 + ggtitle(paste("Pr(", event, " | b)", sep = ""))
        plot1 <- plot1 + xlab("Event b")
        plot1 <- plot1 + ylab("Probability")
        plot1 <- plot1 + theme_update(axis.text.x = element_text(angle = 90, hjust = 1))

        # plot (A | event)
        plot2 <- ggplot(df.cond2, aes(x = A, y = Pr, fill = Pr))
        plot2 <- plot2 + geom_bar(stat = "identity", position = "stack")
        plot2 <- plot2 + guides(fill = FALSE)
        plot2 <- plot2 + ggtitle(paste("Pr(a | ", event, ")", sep = ""))
        plot2 <- plot2 + xlab("Event a")
        plot2 <- plot2 + ylab("Probability")
        plot2 <- plot2 + theme_update(axis.text.x = element_text(angle = 90, hjust = 1))

        quartz()
        grid.arrange(plot1, plot2, nrow = 2, top = paste("Conditional probabilities for", event))        
}

##
# Plots the comprehension scores cs(e,B) and Pr(A,e) for the
# specified event e.
##
comprh_scores <- function(event)
{
        df.comp <- df[df$Type == "cond",]

        df.comp$A <- sapply(strsplit(as.character(df.comp$Item), "\\|"), "[", 1)
        df.comp$B <- sapply(strsplit(as.character(df.comp$Item), "\\|"), "[", 2)

        df.comp1 <- df.comp[df.comp$A == event & substr(df.comp$B, 1, 4) != "not(",] # (event | B)
        df.comp2 <- df.comp[df.comp$B == event,]                                     # (A | event)

        # compute comprehension scores
        for (i in 1 : nrow(df.comp1)) {
                df.comp1$CS[i] = comprh_score(df.comp1$Pr[i], df[df$Item == df.comp1$A[i],]$Pr[1])
                df.comp2$CS[i] = comprh_score(df.comp2$Pr[i], df[df$Item == df.comp2$A[i],]$Pr[1])
        } 

        # plot cs(e,B)
        plot1 <- ggplot(df.comp1, aes(x = B, y = CS, fill = CS))
        plot1 <- plot1 + geom_bar(stat = "identity", position = "dodge")
        plot1 <- plot1 + guides(fill = FALSE)
        plot1 <- plot1 + ggtitle(paste("Events that lead to understanding", event, sep = " "))
        plot1 <- plot1 + xlab("Event b")
        plot1 <- plot1 + ylab("Comprehension score")
        plot1 <- plot1 + theme_update(axis.text.x = element_text(angle = 90, hjust = 1))
        plot1 <- plot1 + coord_cartesian(ylim = c(-1.0, 1.0))

        # plot cs(A,e)
        plot2 <- ggplot(df.comp2, aes(x = A, y = CS, fill = CS))
        plot2 <- plot2 + geom_bar(stat = "identity", position = "dodge")
        plot2 <- plot2 + guides(fill = FALSE)
        plot2 <- plot2 + ggtitle(paste("Events understood from", event, sep = " "))
        plot2 <- plot2 + xlab("Event a")
        plot2 <- plot2 + ylab("Comprehension score")
        plot2 <- plot2 + theme_update(axis.text.x = element_text(angle = 90, hjust = 1))
        plot2 <- plot2 + coord_cartesian(ylim = c(-1.0, 1.0)) 

        quartz()
        grid.arrange(plot1, plot2, nrow = 2, top = paste("Comprehension scores for", event))
}

##
# Plots a heatmap of the comprehension scores cs(a,b) for every
# combination of events a and b.
##
comprh_scores_heatmap <- function()
{
        df.sm <- melt(comprh_scores_matrix(df))

        hmap <- ggplot(df.sm, aes(X1, X2))
        hmap <- hmap + geom_tile(aes(fill = value))
        hmap <- hmap + scale_fill_gradient2(low = "firebrick1", mid="white", high="chartreuse3", limits = c(-1,1))
        hmap <- hmap + ggtitle("Comprehension scores")
        hmap <- hmap + xlab("Atomic event a")
        hmap <- hmap + ylab("Atomic event b")
        hmap <- hmap + theme_update(axis.text.x = element_text(angle = 90, hjust = 1))

        quartz()
        hmap
}

##
# Plots a heatmap of the difference in comprehension scores cs(a,b) for every
# combination of events a and b as determined from the reduced and unreduced
# probabilities.
##
comprh_scores_diff_heatmap <- function()
{
        df.sm <- df

        df.sm$Pr <- df.sm$Pr_unreduced
        mtx.unreduced = comprh_scores_matrix(df.sm)
        df.sm$Pr <- df.sm$Pr_reduced
        mtx.reduced = comprh_scores_matrix(df.sm)

        mtx.difference <- mtx.unreduced - mtx.reduced
        df.sm <- melt(mtx.difference)

        hmap <- ggplot(df.sm, aes(X1, X2))
        hmap <- hmap + geom_tile(aes(fill = value))
        hmap <- hmap + scale_fill_gradient2(low = "firebrick1", mid="white", high="chartreuse3", limits = c(-2,2))
        hmap <- hmap + ggtitle("Difference in comprehension scores")
        hmap <- hmap + xlab("Atomic event a")
        hmap <- hmap + ylab("Atomic event b")
        hmap <- hmap + theme_update(axis.text.x = element_text(angle = 90, hjust = 1))

        quartz()
        hmap
}

###########################################################################
###########################################################################

##
# This computes the comprehension score (Frank et al., 2009), which is 
# defined as:
#
#                     | tau(a|z) - tau(a)
#                     | ----------------- , if tau(a|z) > tau(a)
#                     |    1 - tau(a)
#     comprehension = |
#                     | tau(a|z) - tau(a)
#                     | ----------------- , otherwise
#                     |      tau(a)
#
# References
#
# Frank, S. L., Haselager, W. F. G, & van Rooij, I. (2009). Connectionist
#     semantic systematicity. Cognition, 110, 358-379.
##
comprh_score <- function(az, a)
{
        score <- 0

        if (az > a) {
                score <- (az - a) / (1.0 - a)
        } else {
                score <- (az - a) / a
        }

        return(score)
}

##
# Constructs a matrix of comprehension scores cs(a,b) for every
# combination of events a and b.
##
comprh_scores_matrix <- function(df)
{
        tbl.prior <- data.table(df[df$Type == "prior",])
        setkey(tbl.prior, Item)
        tbl.cond <- data.table(df[df$Type == "cond",])
        setkey(tbl.cond, Item)

        items <- as.character(factor(df[df$Type == "prior" & substr(df$Item, 1, 4) != "not(",]$Item))
        
        cat("Computing comprehension scores matrix ...\n", file = stderr())
        mtx <- matrix(nrow = length(items), ncol = length(items), dimnames = list(items, items))
        for (a in 1 : length(items)) {
                cat(paste(a, ":", items[a], "\n"))
                prior_a <- tbl.prior[items[a],]$Pr
                for (b in 1 : length(items)) {
                        cond_ab <- tbl.cond[paste(items[a], "|" , items[b], sep=""),]$Pr
                        mtx[a,b] <- comprh_score(cond_ab, prior_a)
                }
        }

        mtx
}