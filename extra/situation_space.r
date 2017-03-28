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

###########################################################################
###########################################################################

vec_file <- paste(model_fb, ".vectors", sep = "")

# read vectors
cat("Reading vectors ...\n", file = stderr())
df.vec <- read.csv(vec_file, sep = " ", head = TRUE, check.names = FALSE)

# dimensions and columns
dims <- nrow(df.vec)
cols <- ncol(df.vec)

# reshape
df.vec <- melt(df.vec)
df.vec$dimension <- rep(seq(1, dims, 1), cols)

# heatmap
hmap <- ggplot(df.vec, aes(variable, dimension))
hmap <- hmap + geom_tile(aes(fill = value))
#hmap <- hmap + scale_fill_gradient(low = "red", high = "blue", limits = c(0,1))
hmap <- hmap + scale_fill_gradient(low = "white", high = "darkred", limits = c(0,1))
hmap <- hmap + ggtitle("Situation space")
hmap <- hmap + xlab("Atomic event")
hmap <- hmap + ylab("Vector")
hmap <- hmap + theme_update(axis.text.x = element_text(angle = 90, hjust = 1))
hmap <- hmap + coord_cartesian(ylim = c(0, dims)) 

quartz()
print(hmap)