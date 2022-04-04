```                                    
   ,--.,--.
 ,-|  |`--' ,---.  ,---.  ,--,--. ,---. ,---.
' .-. |,--.(  .-' | .-. |' ,-.  || .--'| .-. :
\ `-' ||  |.-'  `)| '-' '\ '-'  |\ `--.\   --.
 `---' `--'`----' |  |-'  `--`--' `---' `----'
                  `--'
    "To wander, to roam, move about."
```

**Note:**  This is an implementation of the Distributed Situation-state
Space (DSS) model [1,2], as used in [4]. For a recent model-theoretic
reformulation of this approach in terms of Distributional Formal Semantics
(DFS) [5], see [DFS Tools](https://github.com/hbrouwer/dfs-tools).

# Introduction

**dispace** is an implementation of the Distributed Situation-state Space
(DSS) model [1,2], which offers a framework for neural semantics [3].
**dispace** implements machinery for constructing DSSs, as well as for
generating sentences that map into DSS vectors. As such, we distinguish
between two parts of a DSS model:

1. **World**: definition of atomic propositions and their (probabilistic)
   co-occurrence constraints, from which the DSS is derived;

2. **Grammar**: definition of a grammar that produces sentences describing
   situations in the world implemented by the DSS.

A DSS model is specified in a single Prolog file that uses the **dispace**
module and defines four predicates: **event/1**, **violation/1**,
**probability/3**, **sentence/4**. The first three predicates define the
**World** part of the DSS model:

1. **event(-Event)**: specifies an atomic proposition *Event*;

2. **violation(+StateOfAffairs)**: returns *True* if *StateOfAffairs* contains
   a violation of world knowledge and *False* otherwise;

3. **probability(+Event,+StateOfAffairs,-Probability)**: returns the
   *Probability* of *Event* given the *StateOfAffairs*;

The fourth predictate defines the **Grammar** part:

4. **sentence(?Set,?Semantics,?Sentence,[])** *(note: this is a DCG rule!)*:
   returns each full *Sentence* with its associated *Semantics* for a given
   *Set* of sentences.

The following template
[[worlds/template.pl](https://github.com/hbrouwer/dispace/blob/master/worlds/template.pl)]
provides a minimal example of the above:

```prolog
%%
% template.pl
%
%    ,--.,--.
%  ,-|  |`--' ,---.  ,---.  ,--,--. ,---. ,---.
% ' .-. |,--.(  .-' | .-. |' ,-.  || .--'| .-. :
% \ `-' ||  |.-'  `)| '-' '\ '-'  |\ `--.\   --.
%  `---' `--'`----' |  |-'  `--`--' `---' `----'
%                   `--'
%     "To wander, to roam, move about."
%
% Copyright 2014-2017 Harm Brouwer <me@hbrouwer.eu>
%
% Licensed under the Apache License, Version 2.0 (the "License");
% you may not use this file except in compliance with the License.
% You may obtain a copy of the License at
%
%     http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS,
% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
% See the License for the specific language governing permissions and
% limitations under the License.
%%

:- use_module('../src/dispace.pl').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%                           E V E N T S                             %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% event(-Event)
%
%  Returns an atomic event.

event(example1(arg1,arg2)).
event(example2(arg1,arg2)).
event(example3(arg3,arg2)).
event(example4(arg4,arg2)).
event(example5(arg5,arg2)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%                       V I O L A T I O N S                         %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% violation(+StateOfAffairs)
%
%  Succeeds if StateOfAffairs contains a violation of world knowledge.

violation(_) :- false.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%                    P R O B A B I L I T I E S                      %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% probability(+Event,+StateOfAffairs,-Probability)
%
%  Returns the Probability of Event to be the case, given the StateOfAffairs
%  constructed thus far.

probability(_,_,0.5). % <-- coin flip

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%                         S E N T E N C E S                         %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% sentence(?Set,?Semantics,?Sentence,[])
%
%  Returns the Semantics for Sentence (or vice versa).

sentence('set',Sem) --> [], { Sem = [] }.
```

# Using **dispace**

The file
[[worlds/frank_world.pl](https://github.com/hbrouwer/dispace/blob/master/worlds/frank_world.pl)]
implements the DSS model described in [2]. In what follows, we will use this
model to work through the general mode of operation with **dispace**. As a start,
we can load up the model in the Prolog interpretation (here we use [[SWI-Prolog](http://www.swi-prolog.org))]:

```
$ swipl -f frank_world.pl
```

## Observation generation

#### Manual observation generation

Once loaded, we can now sample observations from the world implemented by
the DSS model using the **dss_sample_observation(-Observation)** predicate:

```prolog
?- dss_sample_observation(X).
X = [(play(charlie, chess), 0),  (play(charlie, hide_and_seek), 1),  (play(charlie, soccer), 0),  (play(heidi, chess), 0),  (play(heidi, hide_and_seek), 0),  (play(heidi, soccer), 0),  (play(sophia, chess), 0),  (play(..., ...), 1),  (..., ...)|...].
```

Or we can sample multiple observations using **dss_sample_observations(+NumSamples,-Observation)**:

```prolog
?- dss_sample_observations(5,X).
Sample 1: play(charlie,hide_and_seek) play(sophia,hide_and_seek) play(heidi,doll) win(sophia) lose(charlie) place(charlie,bedroom) place(heidi,playground) place(sophia,bathroom) manner(play(charlie),badly) manner(win,difficultly)
Sample 2: play(charlie,puzzle) play(heidi,ball) place(charlie,bedroom) place(heidi,street) place(sophia,bathroom)
Sample 3: play(heidi,chess) play(sophia,chess) play(charlie,ball) place(charlie,playground) place(heidi,bedroom) place(sophia,bedroom) manner(play(heidi),well) manner(play(sophia),well)
Sample 4: play(charlie,ball) play(heidi,doll) play(sophia,ball) place(charlie,street) place(heidi,playground) place(sophia,playground)
Sample 5: play(charlie,soccer) play(sophia,soccer) play(heidi,doll) play(sophia,ball) place(charlie,street) place(heidi,playground) place(sophia,street) manner(play(charlie),well)
X = [[(play(charlie, chess), 0),  (play(charlie, hide_and_seek), 1),  (play(charlie, soccer), 0),  (play(heidi, chess), 0),  (play(heidi, hide_and_seek), 0),  (play(heidi, soccer), 0),  (play(..., ...), 0),  (..., ...)|...], [(play(charlie, chess), 0),  (play(charlie, hide_and_seek), 0),
(play(charlie, soccer), 0),  (play(heidi, chess), 0),  (play(heidi, hide_and_seek), 0),  (play(..., ...), 0),  (..., ...)|...], [(play(charlie, chess), 0),  (play(charlie, hide_and_seek), 0),  (play(charlie, soccer), 0),  (play(heidi, chess), 1),  (play(..., ...), 0),  (..., ...)|...], [(p
lay(charlie, chess), 0),  (play(charlie, hide_and_seek), 0),  (play(charlie, soccer), 0),  (play(..., ...), 0),  (..., ...)|...], [(play(charlie, chess), 0),  (play(charlie, hide_and_seek), 0),  (play(..., ...), 1),  (..., ...)|...]].
 ```

#### Automated observation generation

While manual observation generation is useful for debugging a DSS model, one
generally wants to generate a large number of observations, and save them to
a file. In **dispace** this process is automated through a [[worlds/Makefile](https://github.com/hbrouwer/dispace/blob/master/worlds/Makefile)]. Let us
construct *25K* sampled observations:

```
$ make observations world=frank_world.pl model=frank_model num_samples=25000
```

This process - which may take a while - will create a folder
**frank_model/** containing the file **frank_model.observations**. This file
contains *25K* sampled observations, stored in vector format.

## DSS vectors

In principle, we could directly use the *25K*-dimensional observation
vectors as DSS vectors. However, to use DSS vectors in neural network
models, we typically want to reduce their dimensionality. Following [2], we
can employ a competitive layer algorithm to achieve this. The file
[[reduce/reduce_compl.r](https://github.com/hbrouwer/dispace/blob/master/reduce/reduce_compl.r)]
implements this method. Start **R** and type:

```R
> model <- "frank_model/"
> source("../reduce/reduce_compl.r")
Reading observations ...
```

We can now reduce the dimensionality using a competitive layer algorithm.
Here, we use the parameters used in [2]:

```R
> reduce(dims = 150, epochs = 20, alpha = 0.1, alpha_sf = 0.1, beta = 0.00008, beta_sf = 0.1, write = TRUE)
```
Once this process finished, the file **frank_model/frank_model.vectors**
will contain real valued *150*-dimensional DSS vectors.

**Note 1:** One can also use subset sampling as an alternative to the
competitive layer algorithm for dimension reduction [4]. The file
[[reduce/reduce_subset.r](https://github.com/hbrouwer/dispace/blob/master/reduce/reduce_subset.r)]
implements this method.

**Note 2:** If no dimension reduction is desired, one could simply use the
observations file (**frank_model/frank_model.observations**) as vectors file
(**frank_model/frank_model.vectors**). Using the *vectors* instead of the
*observations* target in the
[[worlds/Makefile](https://github.com/hbrouwer/dispace/blob/master/worlds/Makefile)]
provides a shortcut to do this.

## Probabilistic structure

Once both the observations (**frank_model/frank_model.observations**) and
vectors (**frank_model/frank_model.vectors**) are in place (which may or may
not be the same), we can investigate and compare the probabilistic structure
of the DSS models they implement. First, we need to compute their prior,
conjunction, and conditional probabilities. This is done using the file
[[probs/compute_probs.r](https://github.com/hbrouwer/dispace/blob/master/reduce/reduce_subset.r)]:

```R
> model <- "frank_model/"
> source("../probs/compute_probs.r")
```

This will produce a file called (e.g., **frank_model/frank_model.probabilities**)
containing the prior, conjunction, and conditional probabilities as derived
from both the observations (**frank_model/frank_model.observations**) and
vectors (**frank_model/frank_model.vectors**) file. In fact, we can now
compare these probabilities to see how well the reduced DSS model approximates
the unreduced model:

```R
> model <- "frank_model/"
> source("../probs/compare_probs.r")
Loading required package: lattice
Reading observations ...
Reading vectors ...
Reading probabilities ...
Computing comprehension vector (observations) ...
Computing comprehension vector (vectors) ...

Similarity: 0.976082804610413

All hard constraints equal: FALSE

cor(priors): 0.999995649152656

cor(conj):  0.99795369926599

cor(cond):  0.995378947759713
```

All looks good, and the reduced vectors are ready to be used as a DSS model.
To further explore the probabilistic structure of the DSS model, see
*probs/event_probs.r* and *probs/event_surprisal.r* which provide a range of
different graphing functions.

## Sentence generation

Now that the DSS model is ready for usage, we can start generating sentences
that map into DSS vectors.

#### Manual sentence generation

In Prolog, we can use the **sentence(?Set,?Semantics,?Sentence,[])**
DCG rule to generate sentences with their corresponding semantics:

```prolog
?- sentence(train, Sem, Sen, []).
Sem = or(event(play(charlie, chess)), or(event(play(charlie, hide_and_seek)), or(event(play(charlie, soccer)), or(event(play(charlie, puzzle)), or(event(play(charlie, ball)), event(play(charlie, doll))))))),
Sen = [charlie, plays] ;
Sem = event(play(charlie, chess)),
Sen = [charlie, plays, chess] .
```

Note that the DCG rule gives the semantics in propositional logic form, which
we can convert into DSS vectors:

```prolog
?- dss_read_vectors('frank_model/frank_model.vectors',StateMatrix), sentence(train,Sem,Sen,[]), dss_semantics_vector(Sem,StateMatrix,StateVector).
StateMatrix = [[(play(charlie, chess), 6.45487843226108e-39),  (play(charlie, hide_and_seek), 7.52916461377073e-53),  (play(charlie, soccer), 3.23471813757475e-58),  (play(heidi, chess), 0.999999999999987),  (play(heidi, hide_and_seek), 1.95650964929445e-17),  (play(heidi, soccer), 3.23471
813757475e-58),  (play(..., ...), 0.999999999999987),  (..., ...)|...], [(play(charlie, chess), 2.40305705610957e-76),  (play(charlie, hide_and_seek), 2.13060876152765e-71),  (play(charlie, soccer), 0.999999999999999),  (play(heidi, chess), 2.40305705610957e-76),  (play(heidi, hide_and_see
k), 3.13147869211559e-71),  (play(..., ...), 3.70339566519143e-46),  (..., ...)|...], [(play(charlie, chess), 2.77258101328272e-39),  (play(charlie, hide_and_seek), 0.194544434425211),  (play(charlie, soccer), 8.20405376894656e-61),  (play(heidi, chess), 2.04210155538888e-39),  (play(...,
...), 0.962292649905679),  (..., ...)|...], [(play(charlie, chess), 5.39306802373601e-59),  (play(charlie, hide_and_seek), 0.999999999999997),  (play(charlie, soccer), 4.19884712865358e-55),  (play(..., ...), 1.40628569934104e-47),  (..., ...)|...], [(play(charlie, chess), 1.15068041492108
e-50),  (play(charlie, hide_and_seek), 3.64166988026518e-16),  (play(..., ...), 1.29829922994669e-64),  (..., ...)|...], [(play(charlie, chess), 4.11395436787035e-65),  (play(..., ...), 1.18412257760702e-62),  (..., ...)|...], [(play(..., ...), 0.979014066522671),  (..., ...)|...], [(...,
...)|...], [...|...]|...],
Sem = or(event(play(charlie, chess)), or(event(play(charlie, hide_and_seek)), or(event(play(charlie, soccer)), or(event(play(charlie, puzzle)), or(event(play(charlie, ball)), event(play(charlie, doll))))))),
Sen = [charlie, plays],
StateVector = [0.3945041986665515, 0.9999999999999996, 0.7040893616840458, 0.999999999999997, 0.7990321130183982, 0.980883741469724, 0.979273134279705, 0.9997652085445852, 0.9999969452155467|...]
```

#### Automated sentence generation

Obviously, one ideally wants to automate the generation of sentences and
their corresponding DSS vectors. This is again realized through
[[worlds/Makefile](https://github.com/hbrouwer/dispace/blob/master/worlds/Makefile)].
For instance, one can generate all sentences of a set:

```
make set world=frank_world.pl model=frank_model set=train
```

This produces the file **frank_model/frank_model.train.localist.set**. See
[[worlds/Makefile](https://github.com/hbrouwer/dispace/blob/master/worlds/Makefile)]
for further output formats.

# References

[1] Frank, S. L., Koppen, M., Noordman, L. G. M., and Vonk, W. (2003).
Modeling knowledge-based inferences in story comprehension. *Cognitive
Science 27*(6), pp. 785-910.

[2] Frank, S. L., Haselager, W. F. G., and van Rooij, I. (2009).
Connectionist semantic systematicity. *Cognition 110*(3), pp. 358-379.

[3] Brouwer, H., Crocker, M. W., and Venhuizen N. J. (2017). Neural
Semantics. In: Wieling, M., Kroon, M., van Noord, G. J. M., and Bouma, G.
(eds.), *From Semantics to Dialectometry: Festschrift for John Nerbonne*,
pp. 75-83. College Publications.

[4] Venhuizen, N. J., Crocker, M. W., and Brouwer, H. (2019).
Expectation-based Comprehension: Modeling the Interaction of World Knowledge
and Linguistic Experience. *Discourse Processes, 56*:3, 229-255.

[5] Venhuizen, N. J., Hendriks, P., Crocker, M. W., and Brouwer, H. (in
press). Distributional Formal Semantics. *Information and Computation*.
arXiv preprint arXiv:2103.01713
