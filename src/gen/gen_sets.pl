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

:- module(gen_sets,
  [
        gen_basic_events_set/2,
        gen_all_sets/5,
        gen_set/6,
        gen_lexical_semantics_set/2
  ]).

:- reexport(gen_vectors).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%                    S E T   G E N E R A T I O N                    %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% gen_basic_event_set(+StateMatrix,+FileBase)

gen_basic_events_set(StateMatrix,FileBase) :-
        dss_sample:dss_basic_events(Events),
        gen_vectors:gen_lexical_items(Words),
        length(Words,NumWords),
        gen_vectors:gen_zero_vector(NumWords,ZeroVec),
        format(atom(Filename),'~w.basic_events.set',FileBase),
        open(Filename,write,Stream),
        write_basic_event_items(Events,StateMatrix,ZeroVec,Stream),
        close(Stream).

% gen_all_sets(+SetType,+VectorType,+OriginalMatrix,+ReducedMatrix,+FileBase)

gen_all_sets(SetType,VecType,OriginalSM,ReducedSM,FileBase) :-
        setof(Set,Sem^Sen^sentence(Set,Sem,Sen,[]),Sets),
        gen_all_sets_(Sets,SetType,VecType,OriginalSM,ReducedSM,FileBase).

gen_all_sets_([],_,_,_,_,_).
gen_all_sets_([Set|Sets],SetType,VecType,OriginalSM,ReducedSM,FileBase) :-
        gen_set(Set,SetType,VecType,OriginalSM,ReducedSM,FileBase),
        gen_all_sets_(Sets,SetType,VecType,OriginalSM,ReducedSM,FileBase).

% gen_set(+Set,+SetType,VecType,+OriginalMatrix,+ReducedMatrix,+FileBase)

gen_set(Set,SetType,VecType,OriginalSM,ReducedSM,FileBase) :-
        findall((Sen,Sem),sentence(Set,Sem,Sen,[]),Sens),
        gen_vectors:gen_lexical_items(Words),
        (  VecType == 'coals'
        -> format(atom(CoalsVectors),'~w.coals',FileBase),
           gen_vectors:gen_coals_vectors(Words,CoalsVectors,WordVecs),
           format(atom(Filename),'~w.~w.coals.set',[FileBase,Set])
        ;  true
        ),
        (  VecType == 'localist'
        -> gen_vectors:gen_localist_vectors(Words,WordVecs),
           format(atom(Filename),'~w.~w.localist.set',[FileBase,Set])
        ;  true
        ),
        open(Filename,write,Stream),
        (  SetType == 'comprehension'
        -> write_comprehension_items(Sens,OriginalSM,ReducedSM,WordVecs,Stream)
        ;  write_production_items(Sens,OriginalSM,ReducedSM,WordVecs,Stream)
        ),
        close(Stream).

% gen_lexical_semantics_set(+Set,+FileBase)

gen_lexical_semantics_set(Set,FileBase) :-
        setof(Pfx,Set^Sem^Sen^(sentence(Set,Sem,Sen,[]),prefix(Pfx,Sen),Pfx \= []),Sens),
        gen_vectors:gen_lexical_items(Words),
        gen_vectors:gen_localist_vectors(Words,InputWordVecs),
        format(atom(CoalsVectors),'~w.coals',FileBase),
        gen_vectors:gen_coals_vectors(Words,CoalsVectors,TargetWordVecs),
        format(atom(Filename),'~w.~w.lexical_semantics.set',[FileBase,Set]),
        open(Filename,write,Stream),
        write_lexical_semantics_items(Sens,InputWordVecs,TargetWordVecs,Stream),
        close(Stream).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% write_basic_event_items(+Events,+StateMatrix,+ZeroVector,+Stream)

write_basic_event_items([],_,_,_).
write_basic_event_items([Event|Events],StateMatrix,ZeroVec,Stream) :-
        write(Event), nl,
        dss_event_vector(Event,StateMatrix,TargetVec),
        format_item([Event],Event,[ZeroVec],[TargetVec],Stream),
        write_basic_event_items(Events,StateMatrix,ZeroVec,Stream).

% write_comprehension_items(+Tuples,+OriginalMatrix,+ReducedMatrix,+WordVectors,+Stream)

write_comprehension_items([],_,_,_,_).
write_comprehension_items([(Sen,Sem)|Sens],OriginalSM,ReducedSM,WordVecs,Stream) :-
        words_to_vectors(Sen,WordVecs,InputVecs),
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        %% Check if Situation is 'lawful'
        (  OriginalSM \= []
        -> dss_semantics:dss_semantics_vector(Sem,OriginalSM,Vec),
           dss_semantics:dss_lawful_vector(Vec)
        ;  true
        ),
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        dss_semantics:dss_semantics_vector(Sem,ReducedSM,TargetVec), !,
        write('V '), write(Sen), nl,
        dss_semantics:dss_format_formula(Sem,FormattedSem),
        format_item(Sen,FormattedSem,InputVecs,[TargetVec],Stream),
        write_comprehension_items(Sens,OriginalSM,ReducedSM,WordVecs,Stream).
write_comprehension_items([(Sen,Sem)|Sens],OriginalSM,ReducedSM,WordVecs,Stream) :-
        words_to_vectors(Sen,WordVecs,InputVecs),
        length(ReducedSM,NumDims),
        gen_zero_vector(NumDims,TargetVec), !,
        write('X '), write(Sen), nl,
        dss_semantics:dss_format_formula(Sem,FormattedSem),
        format_item(Sen,FormattedSem,InputVecs,[TargetVec],Stream),
        write_comprehension_items(Sens,OriginalSM,ReducedSM,WordVecs,Stream).

% write_production_items(+Tuples,+OriginalMatrix,+ReducedMatrix,+WordVectors,+Stream)

write_production_items([],_,_,_,_).
write_production_items([(Sen,Sem)|Sens],OriginalSM,ReducedSM,WordVecs,Stream) :-
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        %% Check if Situation is 'lawful'
        (  OriginalSM \= [] 
        -> dss_semantics:dss_semantics_vector(Sem,OriginalSM,Vec),
           dss_semantics:dss_lawful_vector(Vec)
        ;  true
        ),
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        dss_semantics:dss_semantics_vector(Sem,ReducedSM,InputVec), !,
        words_to_vectors(Sen,WordVecs,TargetVecs),
        write('V '), write(Sen), nl,
        dss_semantics:dss_format_formula(Sem,FormattedSem),
        format_item(Sen,FormattedSem,[InputVec],TargetVecs,Stream),
        write_production_items(Sens,OriginalSM,ReducedSM,WordVecs,Stream).
write_production_items([(Sen,Sem)|Sens],OriginalSM,ReducedSM,WordVecs,Stream) :-
        length(ReducedSM,NumDims),
        gen_zero_vector(NumDims,InputVec),
        words_to_vectors(Sen,WordVecs,TargetVecs), !,
        write('X '), write(Sen), nl,
        dss_semantics:dss_format_formula(Sem,FormattedSem),
        % format_item(Sen,FormattedSem,[InputVec],TargetVecs,Stream),
        format_item(Sen,FormattedSem,[InputVec],TargetVecs,user_error),
        write_production_items(Sens,OriginalSM,ReducedSM,WordVecs,Stream).

% write_lexical_semantics_items(+Sentences,+InputVectors,+TargetVectors,+Stream)

write_lexical_semantics_items([],_,_,_).
write_lexical_semantics_items([Sen|Sens],InputWordVecs,TargetWordVecs,Stream) :-
        words_to_vectors(Sen,InputWordVecs,InputVecs),
        words_to_vectors(Sen,TargetWordVecs,TargetVecs),
        write(Sen), nl,
        format_item(Sen,'(null)',InputVecs,TargetVecs,Stream),
        write_lexical_semantics_items(Sens,InputWordVecs,TargetWordVecs,Stream).

% word_to_input_vectors(+Words,+WordVectors,-Vectors) 

words_to_vectors([],_,[]).
words_to_vectors([Word|Words],WordVecs,[Vec|Vecs]) :-
        memberchk((Word,Vec),WordVecs),
        words_to_vectors(Words,WordVecs,Vecs).

% format_item(+Sentence,+MetaInfo,+InputVectors,+TargetVectors,+Stream)

format_item(Sen,Meta,InputVecs,TargetVecs,Stream) :-
        format(Stream,'Item \"',[]),
        format_name(Sen,Stream),
        length(InputVecs,NumInputs),
        length(TargetVecs,NumTargets),
        NumEvents is max(NumInputs,NumTargets),
        format(Stream,'\" ~d ',NumEvents),
        format(Stream,'\"~w\"\n',Meta), %% ~s to ~w for Sem
        format_events(InputVecs,TargetVecs,Stream),
        format(Stream,'\n',[]).

% format_name(+Words,+Stream)

format_name([Word],Stream) :-
        !, format(Stream,'~w',Word). %% ~s to ~w for Event
format_name([Word|Words],Stream) :-
        format(Stream,'~w ',Word),   %% ~s to ~w for Event
        format_name(Words,Stream).

% format_events(+InputVectors,+TargetVector,+Stream)

format_events([],_,_).
format_events([InputVec],[TargetVec],Stream) :-
        !, format_event(InputVec,TargetVec,Stream).
format_events([InputVec|InputVecs],[TargetVec],Stream) :-
        !, format_event(InputVec,TargetVec,Stream),
        format_events(InputVecs,[TargetVec],Stream).
format_events([InputVec],[TargetVec|TargetVecs],Stream) :-
        !, format_event(InputVec,TargetVec,Stream),
        format_events([InputVec],TargetVecs,Stream).
format_events([InputVec|InputVecs],[TargetVec|TargetVecs],Stream) :-
        !, format_event(InputVec,TargetVec,Stream),
        format_events(InputVecs,TargetVecs,Stream).

% format_events(+InputVector,+TargetVector,+Stream)

format_event(InputVec,TargetVec,Stream) :-
        format(Stream,'Input ',[]),
        format_vector(InputVec,Stream),
        format(Stream,' Target ',[]),
        format_vector(TargetVec,Stream),
        format(Stream,'\n',[]).

% format_vector(+Vector,+Stream)

format_vector([Unit],Stream) :-
        !, format(Stream,'~f',Unit).
format_vector([Unit|Units],Stream) :-
        format(Stream,'~f ',Unit),
        format_vector(Units,Stream).
