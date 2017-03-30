##
# comprh_score.r
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

## COMPUTES:
##
## A 2D and 3D visualization of the comprehension score.

require(car)
require(rgl)

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

###########################################################################
###########################################################################

# compute comprehension scores for a range of az an a values.
df <- data.frame()
for (az in seq(0, 1, 0.05)) {
        for (a in seq(0, 1, 0.05)) {
                cs <- comprh_score(az, a)
                df <- rbind(df, data.frame(az = az, a = a, cs = cs))
        }
}

# 2D scatterplot
quartz()
scatterplot(df$cs ~ df$az | df$a,
        ylab          = "comprehension(a,b)",
        xlab          = "B(a|b)",
        reg.line      = FALSE,
        smooth        = FALSE,
        legend.title  = "B(a)",
        legend.coords = "topleft")

# 3D scatterplot
scatter3d(df$cs ~ df$az + df$a,
        fit           = "smooth",
        xlab          = "B(a|b)",
        ylab          = "comprehension(a,b)",
        zlab          = "B(a)",
        axis.col      = c("black", "blue", "black"),
        fill          = TRUE,
        surface.alpha = 0.35,
        surface.col   = "lightblue",
        point.col     = "darkgray")
rgl.quads(
        c(0.0, 0.0, 1.0, 1.0),
        c(0.5, 0.5, 0.5, 0.5),
        c(0.0, 1.0, 1.0, 0.0),
        color        = c("yellow"),
        alpha        = 0.35)