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

###########################################################################
###########################################################################

df <- read.csv(paste(model_fb, ".probabilities", sep = ""), head = TRUE)

# original or reduced
#df$Pr <- df$Pr_original
df$Pr <- df$Pr_reduced

probs <- function(event)
{
        ### prior probabilities ###

        dfpr <- df[df$Type == "prior" & substr(df$Item,1,4) != "not(",]

        gbpr_A <- ggplot(dfpr, aes(x = Item, y = Pr, fill = Pr))
        gbpr_A <- gbpr_A + geom_bar(stat = "identity", position = "stack")
        gbpr_A <- gbpr_A + guides(fill=FALSE)
        gbpr_A <- gbpr_A + ggtitle("Pr(a)")
        gbpr_A <- gbpr_A + xlab("Event a")
        gbpr_A <- gbpr_A + ylab("Probability")
        gbpr_A <- gbpr_A + theme_update(axis.text.x = element_text(angle = 90, hjust = 1))

        ### conjunction probabilities ###

        dfcj <- df[df$Type == "conj",]

        dfcj$A <- sapply(strsplit(as.character(dfcj$Item),"\\^"),"[",1)
        dfcj$B <- sapply(strsplit(as.character(dfcj$Item),"\\^"),"[",2)

        dfcj_Ab <- dfcj[dfcj$A == event & substr(dfcj$B,1,4) != "not(",] # (event&B)
        dfcj_aB <- dfcj[dfcj$B == event,]                                # (A&event)

        # plot for (event&B)
        gbcj_Ab <- ggplot(dfcj_Ab, aes(x = B, y = Pr, fill = Pr))
        gbcj_Ab <- gbcj_Ab + geom_bar(stat = "identity", position = "stack")
        gbcj_Ab <- gbcj_Ab + guides(fill = FALSE)
        gbcj_Ab <- gbcj_Ab + ggtitle(paste("Pr(", event, "&b)", sep = ""))
        gbcj_Ab <- gbcj_Ab + xlab("Event b")
        gbcj_Ab <- gbcj_Ab + ylab("Probability")
        gbcj_Ab <- gbcj_Ab + theme_update(axis.text.x = element_text(angle = 90, hjust = 1))

        ### conditional probabilities ###

        dfcd <- df[df$Type == "cond",]

        dfcd$A <- sapply(strsplit(as.character(dfcd$Item),"\\|"),"[",1)
        dfcd$B <- sapply(strsplit(as.character(dfcd$Item),"\\|"),"[",2)

        dfcd_Ab <- dfcd[dfcd$A == event & substr(dfcd$B,1,4) != "not(",] # (event|B)
        dfcd_aB <- dfcd[dfcd$B == event,]                                # (A|event)

        # plot for (event|B)
        gbcd_Ab <- ggplot(dfcd_Ab, aes(x = B, y = Pr, fill = Pr))
        gbcd_Ab <- gbcd_Ab + geom_bar(stat = "identity", position = "stack")
        gbcd_Ab <- gbcd_Ab + guides(fill = FALSE)
        gbcd_Ab <- gbcd_Ab + ggtitle(paste("Pr(", event, "|b)", sep = ""))
        gbcd_Ab <- gbcd_Ab + xlab("Event b")
        gbcd_Ab <- gbcd_Ab + ylab("Probability")
        gbcd_Ab <- gbcd_Ab + theme_update(axis.text.x = element_text(angle = 90, hjust = 1))

        # plot for (A|event)
        gbcd_aB <- ggplot(dfcd_aB, aes(x = A, y = Pr, fill = Pr))
        gbcd_aB <- gbcd_aB + geom_bar(stat = "identity", position = "stack")
        gbcd_aB <- gbcd_aB + guides(fill = FALSE)
        gbcd_aB <- gbcd_aB + ggtitle(paste("Pr(a|", event, ")", sep = ""))
        gbcd_aB <- gbcd_aB + xlab("Event a")
        gbcd_aB <- gbcd_aB + ylab("Probability")
        gbcd_aB <- gbcd_aB + theme_update(axis.text.x = element_text(angle = 90, hjust = 1))

        quartz()
        grid.arrange(gbpr_A, gbcj_Ab, gbcd_Ab, gbcd_aB, nrow = 2, ncol = 2, top=paste("Probabilities for", event))
}


# prior event probabilities
prior_probs <- function()
{
        dfp <- df[df$Type == "prior" & substr(df$Item,1,4) != "not(",]

        gbp_A <- ggplot(dfp, aes(x = Item, y = Pr, fill = Pr))
        gbp_A <- gbp_A + geom_bar(stat = "identity", position = "stack")
        gbp_A <- gbp_A + guides(fill = FALSE)
        gbp_A <- gbp_A + ggtitle("Pr(a)")
        gbp_A <- gbp_A + xlab("Event a")
        gbp_A <- gbp_A + ylab("Probability")
        gbp_A <- gbp_A + theme_update(axis.text.x = element_text(angle = 90, hjust = 1))

        quartz()
        gbp_A
}

# conditional event probabilities
cond_probs <- function(event)
{
        dfc <- df[df$Type == "cond",]

        dfc$A <- sapply(strsplit(as.character(dfc$Item),"\\|"),"[",1)
        dfc$B <- sapply(strsplit(as.character(dfc$Item),"\\|"),"[",2)

        dfc_Ab <- dfc[dfc$A == event & substr(dfc$B,1,4) != "not(",] # (event|B)
        dfc_aB <- dfc[dfc$B == event,]                               # (A|event)

        # plot for (event|B)
        gbp_Ab <- ggplot(dfc_Ab, aes(x = B, y = Pr, fill = Pr))
        gbp_Ab <- gbp_Ab + geom_bar(stat = "identity", position = "stack")
        gbp_Ab <- gbp_Ab + guides(fill = FALSE)
        gbp_Ab <- gbp_Ab + ggtitle(paste("Pr(", event, "|b)", sep = ""))
        gbp_Ab <- gbp_Ab + xlab("Event b")
        gbp_Ab <- gbp_Ab + ylab("Probability")
        gbp_Ab <- gbp_Ab + theme_update(axis.text.x = element_text(angle = 90, hjust = 1))

        # plot for (A|event)
        gbp_aB <- ggplot(dfc_aB, aes(x = A, y = Pr, fill = Pr))
        gbp_aB <- gbp_aB + geom_bar(stat = "identity", position = "stack")
        gbp_aB <- gbp_aB + guides(fill = FALSE)
        gbp_aB <- gbp_aB + ggtitle(paste("Pr(a|", event, ")", sep = ""))
        gbp_aB <- gbp_aB + xlab("Event a")
        gbp_aB <- gbp_aB + ylab("Probability")
        gbp_aB <- gbp_aB + theme_update(axis.text.x = element_text(angle = 90, hjust = 1))

        quartz()
        grid.arrange(gbp_Ab, gbp_aB, nrow = 2, top = paste("Conditional probabilities for", event))
}

# conjunction event probabilities
conj_probs <- function(event)
{
        dfc <- df[df$Type == "conj",]

        dfc$A <- sapply(strsplit(as.character(dfc$Item),"\\^"),"[",1)
        dfc$B <- sapply(strsplit(as.character(dfc$Item),"\\^"),"[",2)

        dfc_Ab <- dfc[dfc$A == event & substr(dfc$B,1,4) != "not(",] # (event&B)
        dfc_aB <- dfc[dfc$B == event,]                               # (A&event)

        # plot for (event&B)
        gbp_Ab <- ggplot(dfc_Ab, aes(x = B, y = Pr, fill = Pr))
        gbp_Ab <- gbp_Ab + geom_bar(stat = "identity", position = "stack")
        gbp_Ab <- gbp_Ab + guides(fill = FALSE)
        gbp_Ab <- gbp_Ab + ggtitle(paste("Pr(", event, "&b)", sep = ""))
        gbp_Ab <- gbp_Ab + xlab("Event b")
        gbp_Ab <- gbp_Ab + ylab("Probability")
        gbp_Ab <- gbp_Ab + theme_update(axis.text.x = element_text(angle = 90, hjust = 1))

        # plot for (A&event)
        gbp_aB <- ggplot(dfc_aB, aes(x=A, y = Pr, fill = Pr))
        gbp_aB <- gbp_aB + geom_bar(stat = "identity", position = "stack")
        gbp_aB <- gbp_aB + guides(fill = FALSE)
        gbp_aB <- gbp_aB + ggtitle(paste("Pr(a&", event, ")", sep=""))
        gbp_aB <- gbp_aB + xlab("Event a")
        gbp_aB <- gbp_aB + ylab("Probability")
        gbp_aB <- gbp_aB + theme_update(axis.text.x = element_text(angle = 90, hjust = 1))

        quartz()
        grid.arrange(gbp_Ab, gbp_aB, nrow = 2, top = paste("Conjunction probabilities for", event))
}

##
# This plots comprehension scores to answer two questions:
#
# 1) From what events Y do we understand event X? (X|Y)
#
# 2) What events Y do we understand from event X? (Y|X)
##
scores <- function(event)
{
        dfc <- df[df$Type == "cond",]

        dfc$A <- sapply(strsplit(as.character(dfc$Item),"\\|"),"[",1)
        dfc$B <- sapply(strsplit(as.character(dfc$Item),"\\|"),"[",2)

        dfc_Ab <- dfc[dfc$A == event & substr(dfc$B,1,4) != "not(",] # (event|B)
        dfc_aB <- dfc[dfc$B == event,]                               # (A|event)
        #dfc_Ab <- dfc[dfc$A == event & dfc$B != event & substr(dfc$B,1,4) != "not(",] # (event|B)
        #dfc_aB <- dfc[dfc$B == event & dfc$A != event,]                               # (A|event)

        # compute the comprehension scores
        for (i in 1 : nrow(dfc_Ab)) {
                dfc_Ab$Comp_score[i] = cscore(dfc_Ab$Pr[i], df[df$Item == dfc_Ab$A[i],]$Pr[1])
                dfc_aB$Comp_score[i] = cscore(dfc_aB$Pr[i], df[df$Item == dfc_aB$A[i],]$Pr[1])
        }

        # plot of from what events Y we understand event X
        gbp_Ab <- ggplot(dfc_Ab, aes(x = B, y = Comp_score, fill = Comp_score))
        gbp_Ab <- gbp_Ab + geom_bar(stat = "identity", position = "dodge")
        gbp_Ab <- gbp_Ab + guides(fill = FALSE)
        gbp_Ab <- gbp_Ab + ggtitle(paste("Events that lead to understanding", event, sep = " "))
        gbp_Ab <- gbp_Ab + xlab("Event b")
        gbp_Ab <- gbp_Ab + ylab("Comprehension score")
        gbp_Ab <- gbp_Ab + theme_update(axis.text.x = element_text(angle = 90, hjust = 1))
        gbp_Ab <- gbp_Ab + coord_cartesian(ylim = c(-1.0, 1.0))

        # plot of what events Y we understand from event X
        gbp_aB <- ggplot(dfc_aB, aes(x = A, y = Comp_score, fill = Comp_score))
        gbp_aB <- gbp_aB + geom_bar(stat = "identity", position = "dodge")
        gbp_aB <- gbp_aB + guides(fill = FALSE)
        gbp_aB <- gbp_aB + ggtitle(paste("Events understood from", event, sep = " "))
        gbp_aB <- gbp_aB + xlab("Event a")
        gbp_aB <- gbp_aB + ylab("Comprehension score")
        gbp_aB <- gbp_aB + theme_update(axis.text.x = element_text(angle = 90, hjust = 1))
        gbp_aB <- gbp_aB + coord_cartesian(ylim = c(-1.0, 1.0)) 

        quartz()
        #grid.arrange(gbp_Ab, gbp_aB, nrow=2, top=paste("Comprehension scores for", event))
        grid.arrange(gbp_aB, gbp_Ab, nrow = 2, top = paste("Comprehension scores for", event))
        #print(gbp_aB)
}

# matrix of comprehension scores
scores_matrix <- function()
{
        dfp <- df[df$Type == "prior",]
        tbl.dfp <- data.table(dfp)
        setkey(tbl.dfp, Item)

        dfc <- df[df$Type == "cond",]
        tbl.dfc <- data.table(dfc)
        setkey(tbl.dfc, Item)

        items <- as.character(factor(df[df$Type == "prior" & substr(df$Item,1,4) != "not(",]$Item))

        mtx <- matrix(nrow = length(items), ncol = length(items), dimnames = list(items, items))

        cat("Computing comprehension scores matrix ...\n", file = stderr())
        for (a in 1 : length(items)) {
                cat(paste(a, ":", items[a], "\n"))
                prior_a <- tbl.dfp[items[a],]$Pr
                for (b in 1 : length(items)) {
                        cond_ab <- tbl.dfc[paste(items[a], "|" , items[b], sep=""),]$Pr
                        mtx[a,b] <- cscore(cond_ab, prior_a)
                }
        }

        mtx
}

###########################################################################
###########################################################################

# only extremes
scores_matrix_extremes <- function(mtx)
{
        emtx <- mtx
        emtx[emtx > -1 & emtx < 1] <- 0
        emtx
}

# heatmap of comprehension scores matrix
scores_heatmap <- function(mtx)
{
        df.sm <- melt(mtx)

        hmap <- ggplot(df.sm, aes(X1, X2))
        hmap <- hmap + geom_tile(aes(fill = value))
        #hmap <- hmap + scale_fill_gradient(low = "darkslategray", high = "deeppink", limits = c(-1,1))
        hmap <- hmap + scale_fill_gradient2(low = "firebrick1", mid="white", high="chartreuse3", limits = c(-1,1))
        hmap <- hmap + ggtitle("Comprehension scores")
        hmap <- hmap + xlab("Atomic event a")
        hmap <- hmap + ylab("Atomic event b")
        hmap <- hmap + theme_update(axis.text.x = element_text(angle = 90, hjust = 1))
        #hmap <- hmap + coord_cartesian(ylim = c(0, dims))

        # for Japan ...
        hmap <- hmap + theme_update(axis.title = element_text(size = 20))
        hmap <- hmap + theme_update(axis.text = element_text(size = 20))
        hmap <- hmap + theme_update(title = element_text(size = 20))
        hmap <- hmap + theme_update(legend.key.size = unit(50, "pt"))
        hmap <- hmap + theme_update(legend.title = element_text(size = 20))
        hmap <- hmap + theme_update(legend.text = element_text(size = 20))

        quartz()
        print(hmap)

        ggsave("~/Desktop/comp_hmap.pdf", width = 25, height = 25)
}

# heatmap of the difference between two score matrices
scores_diff_heatmap <- function(mtx1,mtx2)
{
        mtx <- mtx1 - mtx2
        df.sm <- melt(mtx)

        hmap <- ggplot(df.sm, aes(X1, X2))
        hmap <- hmap + geom_tile(aes(fill = value))
        #hmap <- hmap + scale_fill_gradient(low = "darkslategray", high = "deeppink", limits = c(-1,1))
        hmap <- hmap + scale_fill_gradient2(low = "firebrick1", mid="white", high="chartreuse3", limits = c(-2,2))
        hmap <- hmap + ggtitle("Difference in comprehension scores")
        hmap <- hmap + xlab("Atomic event a")
        hmap <- hmap + ylab("Atomic event b")
        hmap <- hmap + theme_update(axis.text.x = element_text(angle = 90, hjust = 1))
        #hmap <- hmap + coord_cartesian(ylim = c(0, dims))

        # for Japan ...
        hmap <- hmap + theme_update(axis.title = element_text(size = 20))
        hmap <- hmap + theme_update(axis.text = element_text(size = 20))
        hmap <- hmap + theme_update(title = element_text(size = 20))
        hmap <- hmap + theme_update(legend.key.size = unit(50, "pt"))
        hmap <- hmap + theme_update(legend.title = element_text(size = 20))
        hmap <- hmap + theme_update(legend.text = element_text(size = 20))
        
        quartz()
        print(hmap)

        ggsave("~/Desktop/comp_hmap.pdf", width = 25, height = 25)
}

scores_as_table <- function(event)
{
        dfc <- df[df$Type == "cond",]

        dfc$A <- sapply(strsplit(as.character(dfc$Item),"\\|"),"[",1)
        dfc$B <- sapply(strsplit(as.character(dfc$Item),"\\|"),"[",2)

        #dfc_Ab <- dfc[dfc$A == event & substr(dfc$B,1,4) != "not(",] # (event|B)
        #dfc_aB <- dfc[dfc$B == event,]                               # (A|event)
        dfc_Ab <- dfc[dfc$A == event & dfc$B != event & substr(dfc$B,1,4) != "not(",] # (event|B)
        dfc_aB <- dfc[dfc$B == event & dfc$A != event,]                               # (A|event)

        # compute the comprehension scores
        for (i in 1 : nrow(dfc_Ab)) {
                dfc_Ab$Comp_score[i] = cscore(dfc_Ab$Pr[i], df[df$Item == dfc_Ab$A[i],]$Pr[1])
                dfc_aB$Comp_score[i] = cscore(dfc_aB$Pr[i], df[df$Item == dfc_aB$A[i],]$Pr[1])
        }

        cat("\n")

        cat(paste("Events understood from", event, "\n", sep=" "))
        print(dfc_aB[,c("A","B","Comp_score")])
        cat("\n")

        cat(paste("Events that lead to understanding", event, "\n", sep = " "))
        print(dfc_Ab[,c("A","B","Comp_score")])
        cat("\n")
}

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
cscore <- function(az, a)
{
        #if (a == 0) {
        #        return(0)
        #}
        if (az > a) {
                score <- (az - a) / (1.0 - a)
        } else {
                score <- (az - a) / a
        }
        return(score)
}

###########################################################################
###########################################################################

##
# This plots surprisal estimates to answer two questions:
#
# 1) From what events Y do we understand event X? (X|Y)
#
# 2) What events Y do we understand from event X? (Y|X)
##
surprisal <- function(event)
{
        dfc <- df[df$Type == "cond",]

        dfc$A <- sapply(strsplit(as.character(dfc$Item),"\\|"),"[",1)
        dfc$B <- sapply(strsplit(as.character(dfc$Item),"\\|"),"[",2)

        dfc_Ab <- dfc[dfc$A == event & substr(dfc$B,1,4) != "not(",] # (event|B)
        dfc_aB <- dfc[dfc$B == event,]                               # (A|event)

        # compute the surprisal estimates
        for (i in 1 : nrow(dfc_Ab)) {
                dfc_Ab$Surprisal[i] = -log(dfc_Ab$Pr[i])
                dfc_aB$Surprisal[i] = -log(dfc_aB$Pr[i])
        }
        
        # max surprisal lower than upper limit
        ul <- max(dfc_Ab[dfc_Ab$Surprisal != Inf,]$Surprisal,dfc_aB[dfc_aB$Surprisal != Inf,]$Surprisal) + 0.5

        # plot of from what events Y we understand event X
        gbp_Ab <- ggplot(dfc_Ab, aes(x = B, y = Surprisal, fill = Surprisal))
        gbp_Ab <- gbp_Ab + geom_bar(stat = "identity", position = "dodge")
        gbp_Ab <- gbp_Ab + scale_fill_gradient(low = "deeppink1", high = "pink1", limits = c(0,ul))
        gbp_Ab <- gbp_Ab + guides(fill = FALSE)
        gbp_Ab <- gbp_Ab + ggtitle(paste(paste("Surprisal of", event, sep = " "), "given 'Event b'", sep = " "))
        gbp_Ab <- gbp_Ab + xlab("Event b")
        gbp_Ab <- gbp_Ab + ylab("Surprisal estimates")
        gbp_Ab <- gbp_Ab + theme_update(axis.text.x = element_text(angle = 90, hjust = 1))
        gbp_Ab <- gbp_Ab + coord_cartesian(ylim = c(0, ul))

        # plot of what events Y we understand from event X
        gbp_aB <- ggplot(dfc_aB, aes(x = A, y = Surprisal, fill = Surprisal))
        gbp_aB <- gbp_aB + geom_bar(stat = "identity", position = "dodge")
        gbp_aB <- gbp_aB + scale_fill_gradient(low = "deeppink1", high = "pink1", limits = c(0,ul))
        gbp_aB <- gbp_aB + guides(fill = FALSE)
        gbp_aB <- gbp_aB + ggtitle(paste("Surprisal of 'Event a' given", event, sep = " "))
        gbp_aB <- gbp_aB + xlab("Event a")
        gbp_aB <- gbp_aB + ylab("Surprisal estimates")
        gbp_aB <- gbp_aB + theme_update(axis.text.x = element_text(angle = 90, hjust = 1))
        gbp_aB <- gbp_aB + coord_cartesian(ylim = c(0, ul)) 

        quartz()
        #grid.arrange(gbp_Ab, gbp_aB, nrow=2, top=paste("Surprisal estimates for", event))
        grid.arrange(gbp_aB, gbp_Ab, nrow = 2, top = paste("Surprisal estimates for", event))
}

# matrix of surprisal estimates
surprisal_matrix <- function()
{
        dfc <- df[df$Type == "cond",]
        tbl.dfc <- data.table(dfc)
        setkey(tbl.dfc, Item)

        items <- as.character(factor(df[df$Type == "prior" & substr(df$Item,1,4) != "not(",]$Item))

        mtx <- matrix(nrow = length(items), ncol = length(items), dimnames = list(items, items))

        cat("Computing surprisal matrix ...\n", file = stderr())
        for (a in 1 : length(items)) {
                cat(paste(a, ":", items[a], "\n"))
                for (b in 1 : length(items)) {
                        cond_ab <- tbl.dfc[paste(items[a], "|" , items[b], sep=""),]$Pr
                        mtx[a,b] <- -log(cond_ab)
                }
        }

        mtx
}

# heatmap of suprisal estimates
surprisal_heatmap <- function(mtx)
{
        df.sm <- melt(mtx)
        str(df.sm)

        # Max surprisal lower than upper limit
        ul <- max(df.sm[df.sm$value != Inf,]$value) + 1
        df.sm[df.sm$value == Inf,]$value <- ul

        hmap <- ggplot(df.sm, aes(X1, X2))
        hmap <- hmap + geom_tile(aes(fill = value))
        hmap <- hmap + scale_fill_gradient2(low = "white", high = "deeppink", limits = c(0,ul))
        #hmap <- hmap + scale_fill_gradient2(low = "firebrick1", mid="white", high="chartreuse3", limits = c(-1,1))
        hmap <- hmap + ggtitle("Surprisal estimates")
        hmap <- hmap + xlab("Atomic event a")
        hmap <- hmap + ylab("Atomic event b")
        hmap <- hmap + theme_update(axis.text.x = element_text(angle = 90, hjust = 1))
        #hmap <- hmap + coord_cartesian(ylim = c(0, dims))

        # for Japan ...
        hmap <- hmap + theme_update(axis.title = element_text(size = 20))
        hmap <- hmap + theme_update(axis.text = element_text(size = 20))
        hmap <- hmap + theme_update(title = element_text(size = 20))
        hmap <- hmap + theme_update(legend.key.size = unit(50, "pt"))
        hmap <- hmap + theme_update(legend.title = element_text(size = 20))
        hmap <- hmap + theme_update(legend.text = element_text(size = 20))

        quartz()
        print(hmap)

        ggsave("~/Desktop/surprisal_hmap.pdf", width = 25, height = 25)
}
