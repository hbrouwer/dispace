##
# situation_space.r
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
## model_reduced: boolean flagging whether a reduced space should be
##      used (TRUE) or not (FALSE).
##
## COMPUTES:
##
## A heatmap showing the Distributed Situation-state Space.

require(gdata)
require(ggplot2)
require(reshape)

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

file.obs <- paste(model_fb, ".observations", sep = "")
file.vec <- paste(model_fb, ".vectors",      sep = "")

if (!model_reduced) {
        cat("Reading observations ...\n", file = stderr())
        df <- read.csv(file.obs, sep = " ", head = TRUE, check.names = FALSE)
} else {
        cat("Reading vectors ...\n", file = stderr())
        df <- read.csv(file.vec, sep = " ", head = TRUE, check.names = FALSE)
}

###########################################################################
###########################################################################

# dimensions and columns
dims <- nrow(df)
cols <- ncol(df)

# reshape
df <- melt(df)
df$dimension <- rep(seq(1, dims, 1), cols)

# heatmap
hmap <- ggplot(df, aes(variable, dimension))
hmap <- hmap + geom_tile(aes(fill = value))
hmap <- hmap + scale_fill_gradient(low = "white", high = "darkred", limits = c(0,1))
hmap <- hmap + ggtitle("Situation space")
hmap <- hmap + xlab("Atomic event")
hmap <- hmap + ylab("Vector")
hmap <- hmap + theme_update(axis.text.x = element_text(angle = 90, hjust = 1))
hmap <- hmap + coord_cartesian(ylim = c(0, dims)) 

quartz()
print(hmap)
