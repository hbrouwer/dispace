%%
% dss_semantics.pl
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

:- module(dss_semantics,
   [
        dss_semantics_vector/3,
        dss_lawful_vector/1,
        dss_format_formula/2,
        dss_semantics_event/4,
        dss_coordinate_disjuncts/3,
        dss_embed_in_event/3
   ]).

:- reexport(dss_fuzzy_logic).

%% dss_semantics_vector(+Formula,+StateMatrix,-SemanticsVector)
%
%  Transforms a first-order logic Formula into a DSS SemanticsVector, on the
%  basis of the vectors in StateMatrix.

dss_semantics_vector(not(Formula),StateMatrix,Vector1) :-
        !, dss_semantics_vector(Formula,StateMatrix,Vector0),
        dss_fuzzy_logic:dss_fuzzy_not(Vector0,Vector1).
dss_semantics_vector(and(Formula0,Formula1),StateMatrix,Vector2) :-
        !, dss_semantics_vector(Formula0,StateMatrix,Vector0),
        dss_semantics_vector(Formula1,StateMatrix,Vector1),
        dss_fuzzy_logic:dss_fuzzy_and(Vector0,Vector1,Vector2).
dss_semantics_vector(or(Formula0,Formula1),StateMatrix,Vector2) :-
        !, dss_semantics_vector(Formula0,StateMatrix,Vector0),
        dss_semantics_vector(Formula1,StateMatrix,Vector1),
        dss_fuzzy_logic:dss_fuzzy_or(Vector0,Vector1,Vector2).
dss_semantics_vector(xor(Formula0,Formula1),StateMatrix,Vector2) :-
        !, dss_semantics_vector(Formula0,StateMatrix,Vector0),
        dss_semantics_vector(Formula1,StateMatrix,Vector1),
        dss_fuzzy_logic:dss_fuzzy_xor(Vector0,Vector1,Vector2).
dss_semantics_vector(imp(Formula0,Formula1),StateMatrix,Vector2) :-
        !, dss_semantics_vector(Formula0,StateMatrix,Vector0),
        dss_semantics_vector(Formula1,StateMatrix,Vector1),
        dss_fuzzy_logic:dss_fuzzy_imp(Vector0,Vector1,Vector2).
dss_semantics_vector(Event,StateMatrix,Vector) :-
        dss_event_vector(Event,StateMatrix,Vector).

%% dss_lawful_vector/1
%
%  Returns whether a vector indexes a 'lawful' situation. Vectors with only
%  zero units are unlawful.

dss_lawful_vector([]) :-
        false, !.
dss_lawful_vector([Unit|_]) :-
        Unit > 0.0, !.
dss_lawful_vector([_|Units]) :-
        dss_lawful_vector(Units).

%% dss_format_formula(+Formula,-Atom)
%
%  Formats a first-order logic formula for pretty printing.

dss_format_formula(not(Formula),Atom1) :-
        !, dss_format_formula(Formula,Atom0),
        format(atom(Atom1),'!(~w)',[Atom0]).
dss_format_formula(and(Formula0,Formula1),Atom2) :-
        !, dss_format_formula(Formula0,Atom0),
        dss_format_formula(Formula1,Atom1),
        format(atom(Atom2),'(~w & ~w)',[Atom0,Atom1]).
dss_format_formula(or(Formula0,Formula1),Atom2) :-
        !, dss_format_formula(Formula0,Atom0),
        dss_format_formula(Formula1,Atom1),
        format(atom(Atom2),'(~w | ~w)',[Atom0,Atom1]).
dss_format_formula(xor(Formula0,Formula1),Atom2) :-
        !, dss_format_formula(Formula0,Atom0),
        dss_format_formula(Formula1,Atom1),
        format(atom(Atom2),'(~w || ~w)',[Atom0,Atom1]).
dss_format_formula(imp(Formula0,Formula1),Atom2) :-
        !, dss_format_formula(Formula0,Atom0),
        dss_format_formula(Formula1,Atom1),
        format(atom(Atom2),'(~w -> ~w)',[Atom0,Atom1]).
dss_format_formula(Event,Atom) :-
        format(atom(Atom),'~w',[Event]).

%% dss_semantics_event(+Predicate,+FirstArguments,SecondArguments,-Formula)
%
%  Constructs a first-order logic disjunction Formula of event(Predicate)
%  with each combination of FirstArguments and SecondArguments.
%
%  e.g., dss_semantics_event(ex,[a,b],[c],Formula).
%  Formula = or(ex(a, c), ex(b, c)).

dss_semantics_event(Pred,[X],Ys,Sem0) :-
        !, dss_semantics_event_(Pred,X,Ys,Sem0).
dss_semantics_event(Pred,[X|Xs],Ys,or(Sem0,Sem1)) :-
        dss_semantics_event_(Pred,X,Ys,Sem0),
        dss_semantics_event(Pred,Xs,Ys,Sem1).

dss_semantics_event_(Pred,X,[],Event) :-
        !, Event =.. [Pred,X].
dss_semantics_event_(Pred,X,[Y],Event) :-
        !, Event =.. [Pred,X,Y].
dss_semantics_event_(Pred,X,[Y|Ys],or(Event,Sem)) :-
        Event =.. [Pred,X,Y],
        dss_semantics_event_(Pred,X,Ys,Sem).

%% dss_coordinate_disjuncts(+Formula0,+Formula1,-Formula2)
%
%  Coordinates all disjuncts in Formula0 and Formula1 in a conjunction,
%  resulting in a single Formula.
%
%  The working asumption is that Formula0 and Formula1 both contain
%  disjuncts that partition the set of their arguments (e.g., people,
%  places, etc.) in the same way. For instance, given the set of people
%  {c,h,s} and formulas:
%
%  Formula0 = p(c,c) | (p(h,c) | p(s,c))
%  Formula1 = x(c,b) | (x(h,b) | x(s,b))
%
%  the resulting Formula2 combines these conditions in a conjunction, while
%  keeping the partitioning of arguments the same as in Formula0 and
%  Formula1:
%
%  Formula2 = (p(c,c) & (x(c,b)) | ((p(h,c) & x(h,b)) | (p(s,c) & x(s,b)))
%
%  In case Formula0 and Formula1 do not contain disjuncts that partition the
%  set of their arguments in the same way, we simply take the conjuction of
%  Formula0 and Formula1.

dss_coordinate_disjuncts(or(E0,Es0),or(E1,Es1),or(and(E0,E1),Es2)) :-
        equal_partitioning(or(E0,Es0),or(E1,Es1)),
        !, dss_coordinate_disjuncts(Es0,Es1,Es2).
dss_coordinate_disjuncts(Es0,Es1,and(Es0,Es1)).

% Check whether Formula0 and Formula1 partition their arguments in the same
% way. This is the case, iff:

% 1): Formula0 = (A v B), Formula1 = (A v B)
equal_partitioning(or(Arg1,Arg2),or(Arg3,Arg4)) :-
        extract_operator(Arg1,Op1),
        extract_operator(Arg2,Op2),
        extract_operator(Arg3,Op1),
        extract_operator(Arg4,Op2).
% 2): Formula0 = (A v A), Formula1 = (B v B)
equal_partitioning(or(Arg1,Arg2),or(Arg3,Arg4)) :-
        extract_operator(Arg1,Op1),
        extract_operator(Arg2,Op1),
        extract_operator(Arg3,Op2),
        extract_operator(Arg4,Op2).
% 3): Formula0 = (A v (A v A)), Formula1 = (B v (B v B))
equal_partitioning(or(Arg1,or(Arg2,Arg3)),or(Arg4,or(Arg5,Arg6))) :-
        extract_operator(Arg1,Op1),
        extract_operator(Arg2,Op1),
        extract_operator(Arg3,Op1),
        extract_operator(Arg4,Op4),
        extract_operator(Arg5,Op4),
        extract_operator(Arg6,Op4).

% extract_operator(event(_),event).
extract_operator(not(_),not).
extract_operator(and(_,_),and).
extract_operator(or(_,_),or).
extract_operator(xor(_,_),xor).
extract_operator(imp(_,_),imp).
extract_operator(_,event).

%% dss_embed_in_event(+Predicates,+Event,-EmbeddedEvents)
%
%  Embeds all predicates in Predicates in an Event, resulting in a list of
%  EmbeddedEvents.
%
%  e.g., dss_embed_in_event([pred(X),pred(Y)],event,EmbeddedEvents).
%  EmbeddedEvents = [event(pred(X)), event(pred(Y))].

dss_embed_in_event([],_,[]).
dss_embed_in_event([X|Xs],Pred,[EX|EXs]) :-
        EX =.. [Pred,X],
        dss_embed_in_event(Xs,Pred,EXs).
