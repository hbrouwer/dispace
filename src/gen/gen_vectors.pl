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

:- module(gen_vectors,
   [
        gen_lexical_items/1,
        gen_localist_vectors/2,
        gen_coals_vectors/3,
        gen_zero_vector/2
   ]).

% gen_lexical_items(-LexicalItems)

gen_lexical_items(Items) :-
        findall(Sen,sentence(_,_,Sen,[]),Sens0),
        flatten(Sens0,Sens1),
        list_to_ord_set(Sens1,Items).

% gen_localist_vectors(+LexicalItems,-Tuples)

gen_localist_vectors(Items,Tuples) :-
        length(Items,NumItems),
        gen_localist_vectors_(Items,0,NumItems,Tuples).

gen_localist_vectors_([],NumItems,NumItems,[]).
gen_localist_vectors_([Item|Items],I0,N,[(Item,Vector)|Tuples]) :-
        I1 is I0 + 1,
        gen_localist_vector(1,I1,N,Vector),
        gen_localist_vectors_(Items,I1,N,Tuples).

gen_localist_vector(I,_,N,[]) :-
        I > N, !.
gen_localist_vector(I,T,N,[0.0|Units]) :-
        I \= T, !,
        I1 is I + 1,
        gen_localist_vector(I1,T,N,Units).
gen_localist_vector(T,T,N,[1.0|Units]) :-
        !, I1 is T + 1,
        gen_localist_vector(I1,T,N,Units).

% gen_coals_vectors(+LexicalItems,+CoalsVectors,-Tuples)

gen_coals_vectors(Items,CoalsVectors,Tuples) :-
        consult(CoalsVectors),
        gen_coals_vectors_(Items,Tuples).

gen_coals_vectors_([],[]).
gen_coals_vectors_([Item|Items],[(Item,Vector)|Tuples]) :-
        upcase_atom(Item,ItemUC),
        coals:vector(ItemUC,Vector),
        gen_coals_vectors_(Items,Tuples).

% gen_zero_vector(+VectorSize,-ZeroVector)

gen_zero_vector(0,[]) :- !.
gen_zero_vector(N,[0|Units]) :-
        N0 is N - 1,
        gen_zero_vector(N0,Units).
