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

:- module(gen_stats,
  [
        gen_word_stats/1,
        gen_structure_stats/1,
        gen_dss_vector_stats/2
  ]).

%:- use_module(library(md5)).
:- use_module(library(sha)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%                    W O R D   S T A T I S T I C S                  %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% gen_word_stats(+Set)

gen_word_stats(Set) :-
        gen_lexical_items(Words),
        findall(Sen,sentence(Set,_,Sen,[]),Sens),
        flatten(Sens,Tokens),
        gen_word_stats_(Words,Tokens,Counts),
        foreach(member((Word,Count),Counts),format('~a: ~a ~d~n',[Set,Word,Count])),
        length(Tokens,NumTokens),
        format('~nTokens: ~d~n',[NumTokens]),
        list_to_ord_set(Tokens,Types),
        length(Types,NumTypes),
        format('Types: ~d~n',[NumTypes]).

gen_word_stats_([],_,[]).
gen_word_stats_([Word|Words],Corpus0,[(Word,Count)|Counts]) :-
        length(Corpus0,CorpusSize0),
        subtract(Corpus0,[Word],Corpus1),
        length(Corpus1,CorpusSize1),
        Count is CorpusSize0 - CorpusSize1,
        gen_word_stats_(Words,Corpus1,Counts).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%              S T R U C T U R E   S T A T I S T I C S              %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% gen_structure_stats(+Set)

gen_structure_stats(Set) :-
        setof(Struct,Sem^Sen^structure(Set,Struct,Sem,Sen,[]),Structs),
        gen_structure_stats_(Structs,Set,Counts),
        foreach(member((Struct,ExSen,ExSem,NumTokens,NumTypes),Counts),
                format('~a: ~t ~a ~t ~d ~t (~d) ~t ~w ~t ~w~n',[Set,Struct,NumTokens,NumTypes,ExSen,ExSem])),
        findall(Sen,sentence(Set,_,Sen,[]),Sens), %% <-- sanity check
        length(Sens,NumSens),
        format('~nTotal: ~d~n',[NumSens]).

gen_structure_stats_([],_,[]).
gen_structure_stats_([Struct|Structs],Set,[(Struct,ExSen,ExSem,NumTokens,NumTypes)|Counts]) :-
        findall((Sen,Sem),structure(Set,Struct,Sem,Sen,[]),Sens),
        memberchk((ExSen,ExSem),Sens),
        length(Sens,NumTokens),
        list_to_ord_set(Sens,Types),
        length(Types,NumTypes),
        gen_structure_stats_(Structs,Set,Counts).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%              D S S   V E C T O R   S T A T I S T I C S            %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% gen_dss_vectors_stats(+Set,+SamplesFile)

gen_dss_vector_stats(Set,SamplesFile) :-
        dss_read_vectors(SamplesFile,StateMatrix),
        findall((Sen,Sem,Hash,Lawful),(
                sentence(Set,Sem,Sen,[]),
                dss_semantics:dss_semantics_vector(Sem,StateMatrix,Vec),
                unit_vector_to_code_vector(Vec,CodeVec),
                %md5_hash(CodeVec,Hash,[]),
                sha_hash(CodeVec,Hash,[algorithm(sha256)]),
                %sha_hash(CodeVec,Hash,[algorithm(sha512)]),
                (  dss_semantics:dss_lawful_vector(Vec)
                -> write('V '), Lawful = true
                ;  write('X '), Lawful = false
                ),
                write(Sen),write(' => '),write(Hash),nl),Quads),
        length(Quads,NumQuads),
        format('~nNumber of sentence-DSS pairs: ~d~n',[NumQuads]),
        setof(Hash,Sen^Sem^Lawful^member((Sen,Sem,Hash,Lawful),Quads),UniqueVecs),
        length(UniqueVecs,NumUniqueVecs),
        format('Number of unique DSS vectors: ~d~n',[NumUniqueVecs]),
        setof(Sem,Sen^Hash^Lawful^member((Sen,Sem,Hash,Lawful),Quads),UniqueFOLForms),
        length(UniqueFOLForms,NumUniqueFOLForms),
        format('Number of unique FOL formulas: ~d~n',[NumUniqueFOLForms]),
        findall(Lawful,(member((_,_,_,Lawful),Quads),Lawful == true),LawfulVecs),
        length(LawfulVecs,NumLawfulVecs),
        format('Number of lawful sentence-DSS pairs: ~d~n',[NumLawfulVecs]),
        NumUnlawfulVecs is NumQuads - NumLawfulVecs,
        format('Number of unlawful sentence-DSS pairs: ~d~n~n',[NumUnlawfulVecs]).

% unit_vector_to_code_vector(+UnitVector,-CodeVector)

unit_vector_to_code_vector(UnitVector,CodeVector) :-
        unit_vector_to_code_vector_(UnitVector,CodeVector0),
        flatten(CodeVector0,CodeVector).

unit_vector_to_code_vector_([],[]) :- !.
unit_vector_to_code_vector_([Unit|Units],[Code|Codes]) :-
        number_codes(Unit,Code),
        unit_vector_to_code_vector_(Units,Codes).
