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

# world specification
world = world.pl

# model name
model = model

# model filebase
model_fb = $(model)/$(model)

###########################################################################
####                       D S S   S A M P L E S                       ####
###########################################################################

# number of samples observations
num_samples = 25000

##
# observations:
#
# This goal generates a DSS model, consisting of a set of sampled
# observations. The result is a directory with the file:
#
#   <model>/<model>.observations	file with sampled observations
##
observations:
	@mkdir -p $(model)
	@swipl -f $(world) -g "dss_model($(num_samples),'$(model_fb)'), halt."

##
# vectors:
#
# Same as 'samples' target but assumes no further dimension reduction/selection,
# and therefore copies <model>/<model>.observations into 
# <model>/<model>.vectors:
#
#   <model>/<model>.observations	file with sampled observations
#   <model>/<model>.vectors		file with situation vectors
##
vectors:
	@mkdir -p $(model)
	@swipl -f $(world) -g "dss_model($(num_samples),'$(model_fb)'), halt."
	@cp $(model_fb).observations $(model_fb).vectors

###########################################################################
###          T R A I N I N G   A N D   T E S T I N G   S E T S         ####
###########################################################################

# vector type ( coals | localist | twofold_inversed_localist )
vector_type = localist

# set type ( comprehension | production )
set_type = comprehension

# set name
set = train

##
# set:
#
# This goal creates all sentences of a given set. Depending on the type of
# the set, its item either map from input sequences to DSS vectors
# (comprehension) or the other way around (production). This produces the
# following file:
#
#   <model>/<model>.<set>.<vector_type>.set	item set
##
set:
	@swipl -f $(world) -g                                             \
		"dss_read_vectors('$(model_fb).vectors',SM),        	  \
		gen_set('$(set)','$(set_type)',          		  \
		'$(vector_type)',[],SM,'$(model_fb)'), halt."

##
# strict_set:
#
# This goal creates all sentences of a given set in a strict manner: all
# unlawful situations are assigned a zero vector. This produces:
#
#   <model>/<model>.<set>.<vector_type>.set	strict item set
##
strict_set:
	@swipl -f $(world) -g                                             \
		"dss_read_vectors('$(model_fb).observations',SM0),    	  \
		dss_read_vectors('$(model_fb).vectors',SM1),        	  \
		gen_set('$(set)','$(set_type)',          		  \
		'$(vector_type)',SM0,SM1,'$(model_fb)'), halt."

##
# all_sets:
#
# This goal creates all sentences of all available sets. For each set, it
# produces the following file:
#
#   <model>/<model>.<set>.<vector_type>.set	item set
##
all_sets:
	@swipl -f $(world) -g                                             \
		"dss_read_vectors('$(model_fb).vectors',SM),        	  \
		gen_all_sets('$(set_type)','$(vector_type)',[],SM,        \
		'$(model_fb)'), halt."

##
# all_sets_strict:
#
# This goal creates all sentences of all available sets in a strict manner
# (see above). For each set, it produces the following file:
#
#   <model>/<model>.<set>.<vector_type>.set	strict item set
##
all_sets_strict:
	@swipl -f $(world) -g                                             \
		"dss_read_vectors('$(model_fb).observations',SM0),  	  \
		dss_read_vectors('$(model_fb).vectors',SM1),        	  \
		gen_all_sets('$(set_type)','$(vector_type)',SM0,SM1,      \
		'$(model_fb)'), halt."

##
# basic_events_set:
#
# This goal creates a set of input-output pairs, in which each input is a
# zero vector, and each output the vector of a basic/atomic event. This
# produces:
#
#  <model>/basic_events.set			basic event set
##
basic_events_set:
	@swipl -f $(world) -g                                             \
		"dss_read_vectors('$(model_fb).vectors',SM),        	  \
		gen_basic_events_set(SM,'$(model_fb)'), halt."
