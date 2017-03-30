##
# graph_descript.r
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

## Note: This is alpha code, and requires polishing.

require(visNetwork)
require(igraph)

edges <- read.csv("/Users/harm/Desktop/EventGraphs/edges.csv", head = TRUE)
edges$from <- as.character(edges$from)
edges$to   <- as.character(edges$to)

nodes <- data.frame(id = unique(c(edges$from, edges$to)))
nodes$label <- nodes$id

## betweenness centrality
ig <- graph_from_data_frame(edges, directed = FALSE)
nodes$value <- betweenness(ig)

## community clustering
clusters <- cluster_edge_betweenness(ig)
nodes$group <- clusters$membership

print(visNetwork(nodes, edges, width = "100%", height = "2000") %>% visEdges(arrows = 'to', scaling = list(min = 2, max = 2)))