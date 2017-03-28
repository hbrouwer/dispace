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

:- module(dss_model,
   [
        dss_model/2,
        dss_state_of_affairs/2,
        dss_read_vectors/2,
        dss_event_vector/3,
        dss_event_state_vector/3
   ]).

:- reexport(dss_sample).

%:- use_module(library(clpfd)).
%:- use_module(library(readutil)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%      D I S T R I B U T E D   S I T U A T I O N   S P A C E        %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% dss_write_model(+NumSamples,+FileBase)
%
%  Generates NumSamples random microworld observations. The predicate
%  produces the file:
%
%  FileBase.observations    random microworld observation vectors

dss_model(NumSamples,FileBase) :-
        format(atom(File),'~w.observations',FileBase),
        open(File,write,Stream),
        dss_basic_events(Events),
        write_events(Events,Stream),
        write_samples(NumSamples,Stream),
        close(Stream).

%% dss_state_of_affairs(+StateVector,-EventList)
%
%  Converts a list of (event,state) tuples (StateVector) into a list of
%  events that "are the case" (the "state of affairs").

dss_state_of_affairs([],[]) :- !.
dss_state_of_affairs([(Event,1)|Tuples],[Event|Events]) :-
        !, dss_state_of_affairs(Tuples,Events).
dss_state_of_affairs([(_,_)|Tuples],Events) :-
        dss_state_of_affairs(Tuples,Events).

%% dss_read_vectors(+File)

dss_read_vectors(File,StateMatrix) :-
        open(File,read,Stream),
        read_events(Stream,Events),
        read_state_vectors(Stream,Events,StateMatrix),
        close(Stream).

% dss_event_vector(+Event,+StateMatrix,-Vector)

dss_event_vector(Event,StateMatrix,Vector) :-
        dss_event_state_vector(Event,StateMatrix,StateVector),
        state_vector_to_vector(StateVector,Vector).

%% dss_event_state_vector(+Event,+StateMatrix,-StateVector) :-
%
%  Returns the StateVector of Event as determined by its states in
%  StateMatrix.

dss_event_state_vector(_,[],[]) :- !.
dss_event_state_vector(Event,[Vec|Vecs],[(Event,State)|Tuples]) :-
        memberchk((Event,State),Vec),
        dss_event_state_vector(Event,Vecs,Tuples).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% write_events(+Events,-Stream)

write_events([Event],Stream) :-
        !, format(Stream,'~w\n',Event).
write_events([Event|Events],Stream) :-
        format(Stream,'~w ',Event),
        write_events(Events,Stream).

% write_samples(+NumSamples,+Stream)

write_samples(NumSamples,Stream) :-
        write_samples_(NumSamples,0,Stream).

write_samples_(NumSamples,NumSamples,_) :- !.
write_samples_(NumSamples,Sample,Stream) :-
        dss_sample_observation(StateVector),
        dss_state_of_affairs(StateVector,StateOfAffairs),
        Sample0 is Sample + 1,
        format('Sample ~d: ',Sample0),
        foreach(member(Event,StateOfAffairs),format('~w ',Event)),
        format('\n'),        
        state_vector_to_vector(StateVector,Vector),
        write_vector(Vector,Stream),        
        write_samples_(NumSamples,Sample0,Stream).

% state_vector_to_vector(+StateVector,-Vector).

state_vector_to_vector([],[]) :- !.
state_vector_to_vector([(_,Unit)|Tuples],[Unit|Units]) :-
        state_vector_to_vector(Tuples,Units).

% write_vector(+Vector,+Stream)

write_vector([Unit],Stream) :- 
        !, format(Stream,'~f\n',Unit).
write_vector([Unit|Units],Stream) :-
        format(Stream,'~f ',Unit),
        write_vector(Units,Stream).

% read_events(+Stream,-Events)

read_events(Stream,Events) :-
        read_line_to_codes(Stream,Line),
        atom_codes(Atom,Line),
        atomic_list_concat(Events0,' ',Atom),
        atoms_to_terms(Events0,Events).

% read_state_vectors(+Stream,+Events,-StateMatrix)

read_state_vectors(Stream,Events,StateVectors) :-
        read_line_to_codes(Stream,Line),    
        (  Line \= end_of_file
        -> atom_codes(Atom,Line),
           atomic_list_concat(Vector0,' ',Atom),
           atoms_to_terms(Vector0,Vector),
           vector_to_state_vector(Vector,Events,StateVector),
           read_state_vectors(Stream,Events,StateVectors0),
           append([StateVector],StateVectors0,StateVectors)
        ;  StateVectors = []).

% atoms_to_terms(+Atoms,-Terms)

atoms_to_terms([],[]) :- !.
atoms_to_terms([Atom|Atoms],[Term|Terms]) :-
        read_term_from_atom(Atom,Term,[]),
        atoms_to_terms(Atoms,Terms).

% vector_to_state_vector(+Vector,+Events,-StateVector).

vector_to_state_vector([],[],[]) :- !.
vector_to_state_vector([Unit|Units],[Event|Events],[(Event,Unit)|Tuples]) :-
        vector_to_state_vector(Units,Events,Tuples).
