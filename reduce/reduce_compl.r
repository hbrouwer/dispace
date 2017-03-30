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
## DEFINES:
##
## reduce(dims, epochs, alpha, alpha_sf, beta, beta_sf, write): reduces
##      the dimensionality of a DSS using a Competitive Layer algorihm.
##
##      dims:           desired number of dimensions;
##      epochs:         number of training epochs;
##      alpha:          learning parameter for weights;
##      alpha_sf:       epochwise scaling factor for alpha;
##      beta:           learning parameter for biases;
##      beta_sf:        epochwise scaling factor for beta;
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
####                C O M P E T I T I V E   L A Y E R                  ####
###########################################################################

##
# Transforms observation vectors S into situation vectors u(a) using
# a Competitive Layer algorithm: 
#
#         e=1 e=2 e=3     e=N                     e=1 e=2 e=3 ... e=4
# k = 1 [  1   1   0  ...  0  ]        u(a) = 1 [ .00 .04 .57 ... .57 ]
# k = 2 [  1   0   1  ...  1  ]  -->>  u(a) = 2 [ .99 .99 .00 ... .00 ]
# k = 3 [  0   0   0  ...  0  ]  ==>>  u(a) = 3 [ .00 .00 .36 ... .00 ]
# ..... [  .   .   .       .  ]  -->>  ........ [ ... ... ... ... ... ]
# k = M [  0   0   1  ...  0  ]        u(a) = m [ .00 .99 .00 ... ... ]
#
# Each unit i of a Competitive Layer is assigned a weight vector, with
# a weight for each basic event e in the microworld. In addition, each unit
# i is assigned a single bias unit b_i.  Weights are initially set to 0.5,
# and biases to 1. The resulting Competitive Layer is then trained for
# N epochs. During each epoch, the following procedure is repeated for each
# obsveration Sk:
#
# (1) For each unit i, the cityblock distance is determined between the
#     weight vector assigned to unit i, and observation Sk:
#
#          dist(u_i,Sk) = sum_a|u_i(a) - Sk(a)|
#
# (2) The unit w with the shortest, biased distance to Sk(a) is determined:
#
#          w = argmin_i(dist(u_i,Sk) - b_i)
#
# (3) The weight vector of unit w (the winner) is updated:
#
#          Du_w = alpha * (Sk - u_w)
#
#     where alpha is a learning rate parameter.
#
# (4) The bias of unit w (the winner) is decreased (to a minimum of 1):
#
#          Db_w = beta * b_w(1 - b_w)
#
#     where beta is a learning rate parameter.
#
# (5) The biases of all other units (i != w; the losers) are increased:
#
#          Db_i = beta * b_i
#
# The above is adapted from:
#
# Frank, S. L., Haselager, W. F. G., and van Rooij, I. (2009). Connectionist
#     semantic systematicity. Cognition, 110(3):358–379.
#
# Note: in the present implementation, alpha and beta are scaled after each
#     training epoch
##
reduce <- function(
        dims     = 150,
        epochs   = 20,
        alpha    = 0.1, 
        alpha_sf = 0.1,
        beta     = 0.00008,
        beta_sf  = 0.1,
        write    = FALSE)
{
        biases  <- rep(1.0, dims)
        weights <- matrix(rep(0.5, dims * ncol(df.mtx)), dims, ncol(df.mtx))

        epoch <- 1
        while (epoch <= epochs) {
                cat(paste(
                        "Epoch:",   epoch, 
                        "\tAlpha:",   round(alpha,    3),
                        "\tAlphaSF:", round(alpha_sf, 3),
                        "\tBeta:",    round(beta,     3),
                        "\tBetaSF:",  round(beta_sf,  3), 
                        "\n"),
                        file = stderr())

                for (i in 1 : nrow(df.mtx)) {
                        dv <- c()

                        # (1): For each unit i, the cityblock distance is 
                        # determined between the weight vector assigned to
                        # unit i, and observation Sk:
                        #
                        #     dist(u_i,Sk) = sum_a|u_i(a) - Sk(a)|
                        for (j in 1 : nrow(weights))
                                dv <- c(dv, sum(abs(weights[j,] - df.mtx[i,])))

                        # (2): The unit w with the shortest, biased distance
                        # to Sk(a) is determined:
                        #
                        #    w = argmin_i(dist(u_i,Sk) - b_i)
                        w <- which.min(dv - biases)

                        # (3): The weight vector of unit w (the winner) is 
                        # updated:
                        #
                        #     Du_w = alpha * (Sk - u_w)
                        weights[w,] <- weights[w,] + alpha * (df.mtx[i,] - weights[w,])

                        # (4): The bias of unit w (the winner) is decreased 
                        # (to a minimum of 1):
                        #
                        #     Db_w = beta * b_w(1 - b_w)
                        biases[w] <- biases[w] + beta * biases[w] * (1 - biases[w])

                        # (5): The biases of all other units (i != w; the
                        # losers) are increased:
                        #
                        #     Db_i = beta * b_i
                        biases[-w] <- biases[-w] + beta * biases[-w]
                }

                epoch <- epoch + 1;

                # scale alpha and beta
                alpha <- alpha - alpha_sf * alpha;
                beta  <- beta  - beta_sf  * beta;
        }

        # write weights (if required)
        if (write) {
                colnames(weights) <- colnames(df.mtx)
                write.table(weights, file.vec, quote = FALSE, sep = " ",
                        col.names = TRUE, row.names = FALSE)
                cat(paste("Wrote:", file.vec, "\n"), file = stderr())
        }

        weights
}