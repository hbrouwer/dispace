##
# event_surprisal.r
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
## surprisal_values(e): plots the surprisal value S(e,B) and S(A,e) for the 
##      specified event e.
##
## surprisal_values_heatmap(): plots a heatmap of the surprisal values S(a,b)
##      for every combination of events a and b.

require(reshape2)
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
# Plots the surprisal value S(e,B) and S(A,e) for the specified event e.
##
surprisal_values <- function(event)
{
        df.cond <- df[df$Type == "cond",]

        df.cond$A <- sapply(strsplit(as.character(df.cond$Item), "\\|"), "[", 1)
        df.cond$B <- sapply(strsplit(as.character(df.cond$Item), "\\|"), "[", 2)

        df.cond1 <- df.cond[df.cond$A == event & substr(df.cond$B, 1, 4) != "not(",] # (event | B)
        df.cond2 <- df.cond[df.cond$B == event,]                                     # (A | event)

        # compute surprisal values
        for (i in 1 : nrow(df.cond1)) {
                df.cond1$S[i] = -log(df.cond1$Pr[i])
                df.cond2$S[i] = -log(df.cond2$Pr[i])
        }

        # determine upper bound (= max non-infinite Surprisal plus one)
        ub <- max(df.cond1[df.cond1$S != Inf,]$S, df.cond2[df.cond2$S != Inf,]$S) + 1

        # plot S(e,b)
        plot1 <- ggplot(df.cond1, aes(x = B, y = S, fill = S))
        plot1 <- plot1 + geom_bar(stat = "identity", position = "dodge")
        plot1 <- plot1 + scale_fill_gradient(low = "deeppink1", high = "pink1", limits = c(0, ub))
        plot1 <- plot1 + guides(fill = FALSE)
        plot1 <- plot1 + ggtitle(paste(paste("Surprisal of", event, sep = " "), "given 'event b'", sep = " "))
        plot1 <- plot1 + xlab("Event b")
        plot1 <- plot1 + ylab("Surprisal estimates")
        plot1 <- plot1 + theme_update(axis.text.x = element_text(angle = 90, hjust = 1))
        plot1 <- plot1 + coord_cartesian(ylim = c(0, ub))

        # plot S(A,e)
        plot2 <- ggplot(df.cond2, aes(x = A, y = S, fill = S))
        plot2 <- plot2 + geom_bar(stat = "identity", position = "dodge")
        plot2 <- plot2 + scale_fill_gradient(low = "deeppink1", high = "pink1", limits = c(0, ub))
        plot2 <- plot2 + guides(fill = FALSE)
        plot2 <- plot2 + ggtitle(paste("Surprisal of 'event a' given", event, sep = " "))
        plot2 <- plot2 + xlab("Event a")
        plot2 <- plot2 + ylab("Surprisal estimates")
        plot2 <- plot2 + theme_update(axis.text.x = element_text(angle = 90, hjust = 1))
        plot2 <- plot2 + coord_cartesian(ylim = c(0, ub)) 

        quartz()
        grid.arrange(plot1, plot2, nrow = 2, top = paste("Surprisal estimates for", event))
}

##
# Plots a heatmap of the surprisal values S(a,b) for every combination of
# events a and b.
##
surprisal_values_heatmap <- function()
{
        df.sm <- melt(surprisal_values_matrix(df))

        # determine upper bound
        ub <- max(df.sm[df.sm$value != Inf,]$value) + 1

        # replace ininity with upper bound (= max non-infinite Surprisal plus one)
        if (Inf %in% df.sm$value)
                df.sm[df.sm$value == Inf,]$value <- ub

        hmap <- ggplot(df.sm, aes(Var1, Var2))
        hmap <- hmap + geom_tile(aes(fill = value))
        hmap <- hmap + scale_fill_gradient2(low = "white", high = "deeppink", limits = c(0, ub))
        hmap <- hmap + ggtitle("Surprisal estimates")
        hmap <- hmap + xlab("Atomic event a")
        hmap <- hmap + ylab("Atomic event b")
        hmap <- hmap + theme_update(axis.text.x = element_text(angle = 90, hjust = 1))

        quartz()
        hmap
}

###########################################################################
###########################################################################

##
# Constructs a matrix of surprisal values S(a,b) for every combination of 
# events a and b.
##
surprisal_values_matrix <- function(df)
{
        tbl.cond <- data.table(df[df$Type == "cond",])
        setkey(tbl.cond, Item)

        items <- as.character(factor(df[df$Type == "prior" & substr(df$Item, 1, 4) != "not(",]$Item))

        cat("Computing surprisal matrix ...\n", file = stderr())
        mtx <- matrix(nrow = length(items), ncol = length(items), dimnames = list(items, items))
        for (a in 1 : length(items)) {
                cat(paste(a, ":", items[a], "\n"))
                for (b in 1 : length(items)) {
                        cond_ab <- tbl.cond[paste(items[a], "|" , items[b], sep=""),]$Pr
                        mtx[a,b] <- -log(cond_ab)
                }
        }

        mtx
}
