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

df <- read.csv(paste(model_fb, ".probabilities", sep = ""), head = TRUE)

df_prior <- df[df$Type == "prior",]
df_conj  <- df[df$Type == "conj",]
df_cond  <- df[df$Type == "cond",]

# prior probabilities scatterplot
quartz()
print(xyplot(df_prior$Pr_reduced ~ df_prior$Pr_original,
             main=paste("r =", signif(cor(df_prior$Pr_reduced, df_prior$Pr_original), 3)),
             xlab=expression(paste("Original ", tau, "(b)")),
             ylab=expression(paste("Reduced", tau, "(b)"))))
print(cor(df_prior$Pr_reduced, df_prior$Pr_original))

# prior probabilities histogram
quartz()
print(histogram(df_prior$Pr_reduced,
          main=expression(paste("Distribution of ", tau, "(b)")),
          xlab=expression(paste("Reduced ", tau, "(b)"))))

# conjunction probabilities scatterplot
quartz()
print(xyplot(df_conj$Pr_reduced ~ df_conj$Pr_original,
             main=paste("r =", signif(cor(df_conj$Pr_reduced, df_conj$Pr_original), 3)),
             xlab=expression(paste("Original ", tau, "(a&b)")),
             ylab=expression(paste("Reduced", tau, "(a&b)"))))
print(cor(df_conj$Pr_reduced, df_conj$Pr_original))

# conjunction probabilities histogram
quartz()
print(histogram(df_conj$Pr_reduced,
          main=expression(paste("Distribution of ", tau, "(a&b)")),
          xlab=expression(paste("Reduced ", tau, "(a&b)"))))

# conditional probabilities scatterplot
quartz()
print(xyplot(df_cond$Pr_reduced ~ df_cond$Pr_original,
             main=paste("r =", signif(cor(df_cond$Pr_reduced, df_cond$Pr_original), 3)),
             xlab=expression(paste("Original ", tau, "(a|b)")),
             ylab=expression(paste("Reduced", tau, "(a|b)"))))
print(cor(df_cond$Pr_reduced, df_cond$Pr_original))

# conditional probabilities histogram
quartz()
print(histogram(df_cond$Pr_reduced,
          main=expression(paste("Distribution of ", tau, "(a|b)")),
          xlab=expression(paste("Reduced ", tau, "(a|b)"))))
