```                                    
   ,--.,--.
 ,-|  |`--' ,---.  ,---.  ,--,--. ,---. ,---.
' .-. |,--.(  .-' | .-. |' ,-.  || .--'| .-. :
\ `-' ||  |.-'  `)| '-' '\ '-'  |\ `--.\   --.
 `---' `--'`----' |  |-'  `--`--' `---' `----'
                  `--'
    "To wander, to roam, move about."
```

## Introduction

Implementation of the Distributed Situation-state Space (DSS) model [1,2],
which offers framework for neural semantics [3]. **dispace** implements
machinery for constructing DSSs, as well as for generating sentences that
map into DSS vectors. As such, we distinguish between two parts of a DSS
model:

1. **World**: definition of atomic propositions and their (probabilistic)
   co-occurrence constraints, from which the DSS is derived;

2. **Grammar**: definition of a grammar that produces sentences describing
   situations in the world implemented by the DSS.

## Defining a DSS model

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
[[template.pl](https://github.com/hbrouwer/dispace/blob/master/worlds/template.pl)]
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

## References

[1] Frank, S. L., Koppen, M., Noordman, L. G. M., and Vonk, W. (2003).
Modeling knowledge-based inferences in story comprehension. *Cognitive
Science 27*(6), pp. 785-910.

[2] Frank, S. L., Haselager, W. F. G., and van Rooij, I. (2009).
Connectionist semantic systematicity. *Cognition 110*(3), pp. 358-379.

[3] Brouwer, H., Crocker, M. W., and Venhuizen N. J. (2017). Neural
Semantics. In: Wieling, M., Kroon, M., van Noord, G. J. M., and Bouma, G.
(eds.), *From Semantics to Dialectometry: Festschrift for John Nerbonne*,
pp. 75-83. College Publications.
