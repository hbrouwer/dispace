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

:- use_module('../src/tinyworlds.pl').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%                 S T A T E S   O F   A F F A I R S                 %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% file(-FileName)
%
%  Specifies the observation file.

file('../worlds/test/test.observations').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% states_of_affairs

states_of_affairs :-
        file(File),
        dss_model:dss_read_vectors(File,StateMatrix),
        open('/Users/harm/Desktop/states_of_affairs.txt',write,Stream), 
        write_states_of_affairs(StateMatrix,Stream),
        close(Stream).

% write_state_of_affairs(+StateMatrix,+Stream)

write_states_of_affairs([],_) :- !.
write_states_of_affairs([StateVector|StateVectors],Stream) :-
        state_of_affairs(StateVector,StateOfAffairs),
        write(Stream,'"'),
        write(Stream,StateOfAffairs),
        write(Stream,'"\n'),
        write_states_of_affairs(StateVectors,Stream).

%% state_of_affairs(+StateVector,-EventList)
%
%  Converts a list of (event,state) tuples (StateVector) into a list of
%  events that "are the case" (the "state of affairs").

state_of_affairs([],[]) :- !.
state_of_affairs([(Event,1.0)|Tuples],[Event|Events]) :-
        !, state_of_affairs(Tuples,Events).
state_of_affairs([(_,_)|Tuples],Events) :-
        state_of_affairs(Tuples,Events).
