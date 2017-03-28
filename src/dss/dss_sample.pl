%%
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

:- module(dss_sample,
   [
        dss_basic_events/1,
        dss_sample_observation/1,
        dss_sample_observations/2
   ]).

:- use_module(library(random)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%              O B S E R V A T I O N   S A M P L I N G              %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% dss_basic_events(-Events)
%
%  Returns the basic events in the microworld.

dss_basic_events(Events) :-
        findall(Event,world:event(Event),Events).

%% dss_sample_observation(-StateVector)
%
%  Returns a list of (event,state) tuples representing a random microworld
%  observation.

dss_sample_observation(StateVector) :-
        dss_basic_events(Events),
        initial_state_vector(Events,StateVector0),
        sample_event_states(StateVector0,StateVector1),
        \+ violation(StateVector1),
        order_events(StateVector0,StateVector1,StateVector),
        !.
dss_sample_observation(StateVector) :-
        dss_sample_observation(StateVector).

%% dss_samples_observations(+NumSamples,-StateMatrix)
% 
%  Returns a list of NumSamples random microworld observations.

dss_sample_observations(NumSamples,StateMatrix) :-
        dss_sample_observations_(NumSamples,0,StateMatrix).

dss_sample_observations_(NumSamples,NumSamples,[]) :- !.
dss_sample_observations_(NumSamples,Sample,[StateVector|StateVectors]) :-
        Sample < NumSamples,
        dss_sample_situation(StateVector),
        dss_model:dss_state_of_affairs(StateVector,StateOfAffairs),
        Sample0 is Sample + 1,
        format('Sample ~d: ',Sample0),
        foreach(member(Event,StateOfAffairs),format('~w ',Event)),
        format('\n'),
        dss_sample_observations_(NumSamples,Sample0,StateVectors).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% initial_state_vector(+Events,-StateVector)

initial_state_vector([],[]).
initial_state_vector([Event|Events],[(Event,0.5)|Tuples]) :-
        initial_state_vector(Events,Tuples).

% order_events(+OrderedUndecided,+UnorderedDecided,-OrderedDecided)

order_events([],[],[]).
order_events([(Event0,_)|Events0],Events1,[(Event0,State)|Events3]) :-
        select((Event0,State),Events1,Events2),
        order_events(Events0,Events2,Events3).

%% sample_event_states(+UndecidedEvents,-DecidedEvents)
%
%  Samples the states of all basic events in the microworld. A state of
%  a random event is toggled to be either the case or not (depending on its
%  probability given the unfolding state of affairs), and on the basis of
%  the updated state of affairs, we then try to infer as many other event
%  states as possible.  This is procedure is repeated until no more events
%  have undecided state.
%
%  The state of an event E is certain if either its being the case or not
%  being the case can be uniquely determined. This is established by
%  generating a state of affairs S1 in which event E is the case, and
%  a state of affairs S2 in which it is not the case, and then verifying
%  these states of affairs with respect to world knowledge. There are four
%  possible outcomes:
% 
%  (1) S1 and S2 are both consistent with world knowledge: The state of
%  event E is not certain, and cannot be inferred.
% 
%  (2) Only S1 is consistent with world knowledge: E is inferred to be the
%  case, meaning that S1 is the new state of affairs.
% 
%  (3) Only S2 is consistent with world knowledge: E is inferred not to be
%  the case, meaning that S2 is the new state of affairs.
%
%  (4) Neither S1 nor S2 is consistent with world knowledge: The state of
%  affairs before trying to infer E is in conflict with world knowledge.

sample_event_states(Undecided,Decided) :-
        sample_event_states_(Undecided,[],Decided).

sample_event_states_([],Decided,Decided) :- !.
sample_event_states_(Undecided0,Decided0,Decided3) :-
        toggle_random_event(Undecided0,Undecided1,Decided0,Decided1),
        infer_event_states(Undecided1,Undecided2,Decided1,Decided2),
        !,
        sample_event_states_(Undecided2,Decided2,Decided3).

% toggle_random_event(+Undecided,+RestUndecided,+UpdatedDecided)

toggle_random_event(Undecided0,Undecided1,Decided0,[(Event,State)|Decided0]) :-
        random_select((Event,_),Undecided0,Undecided1),
        probability(Event,Decided0,Pr), !,
        (  maybe(Pr)
        -> State = 1
        ;  State = 0 ).

% infer_event_states(+Undecided,-RestUndecided,+Decided,-UpdatedDecided)

infer_event_states(Undecided0,Undecided2,Decided0,Decided1) :-
        random_permutation(Undecided0,Undecided1),
        infer_event_states_(Undecided1,[],Undecided2,Decided0,Decided1).

infer_event_states_([],Undecided,Undecided,Decided,Decided).
infer_event_states_([(Event,_)|Undecided0],Undecided1,Undecided3,Decided0,Decided2) :-
        event_inference_tuple([(Event,1)|Decided0],[(Event,0)|Decided0],States),
        infer(States,Event,Undecided1,Undecided2,Decided0,Decided1),
        infer_event_states_(Undecided0,Undecided2,Undecided3,Decided1,Decided2).

% event_inference_tuple(+Decided0,Decided1,StateTuple)

event_inference_tuple(Decided0,Decided1,(State0,State1)) :-
        (  world:violation(Decided0)
        -> State0 = 0
        ;  State0 = 1 ),
        (  world:violation(Decided1)
        -> State1 = 0
        ;  State1 = 1 ).

% infer(+StateTuple,+Event,+Undecided,-RestUndecided,+Decided,-UpdatedDecided)

infer((1,1),Event,Undecided,[(Event,0.5)|Undecided],Decided,Decided).
infer((1,0),Event,Undecided,Undecided,Decided,[(Event,1)|Decided]).
infer((0,1),Event,Undecided,Undecided,Decided,[(Event,0)|Decided]).
% infer((0,0),_,_,_,_,_) :- fail. %% (note: uncommenting has same effect)