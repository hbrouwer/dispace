%%
% dss_fuzzy_logic.pl
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

:- module(dss_fuzzy_logic,
   [
        dss_fuzzy_not/2,
        dss_fuzzy_and/3,
        dss_fuzzy_nand/3,
        dss_fuzzy_or/3,
        dss_fuzzy_xor/3,
        dss_fuzzy_imp/3
   ]).

%% dss_fuzzy_not(+Vector,-NotVector)
%
%  u(!a) = 1 - u(a)

dss_fuzzy_not([],[]) :- !.
dss_fuzzy_not([Unit0|Units0],[Unit1|Units1]) :-
        Unit1 is 1.0 - Unit0,
        dss_fuzzy_not(Units0,Units1).

%% dss_fuzzy_and(+Vector1,Vector2,-AndVector)
%
%  u(a & b) = u(a) * u(b), where u(a & a) = u(a)

dss_fuzzy_and(Vector,Vector,Vector) :- !. % a and a = a
dss_fuzzy_and(Vector0,Vector1,Vector2) :-
        dss_fuzzy_and_(Vector0,Vector1,Vector2).

dss_fuzzy_and_([],[],[]) :- !.
dss_fuzzy_and_([Unit0|Units0],[Unit1|Units1],[Unit2|Units2]) :-
        Unit2 is Unit0 * Unit1,
        dss_fuzzy_and_(Units0,Units1,Units2).

%% dss_fuzzy_nand(+Vector1,+Vector2,-NandVector)
%  
%  u(a nand b) = u(!(a & b))

dss_fuzzy_nand(Vector0,Vector1,Vector3) :-
        dss_fuzzy_and(Vector0,Vector1,Vector2),
        dss_fuzzy_not(Vector2,Vector3).

%% dss_fuzzy_or(+Vector1,+Vector2,-OrVector)
%
%  u(a v b) = u(u(a nand a)) nand u(b nand b))

dss_fuzzy_or(Vector0,Vector2,Vector4) :-
        dss_fuzzy_nand(Vector0,Vector0,Vector1), % a nand a
        dss_fuzzy_nand(Vector2,Vector2,Vector3), % b nand b
        dss_fuzzy_nand(Vector1,Vector3,Vector4). % (a nand a) nand (b nand b)

%% dss_fuzzy_xor(+Vector1,Vector2,-XorVector)
%
%  u(a xor b) = u(u(a nand u(a nand b)) nand u(b nand u(a nand b)))

dss_fuzzy_xor(Vector0,Vector1,Vector5) :-
        dss_fuzzy_nand(Vector0,Vector1,Vector2), % a nand b
        dss_fuzzy_nand(Vector0,Vector2,Vector3), % a nand (a nand b)
        dss_fuzzy_nand(Vector1,Vector2,Vector4), % b nand (a nand b)
        dss_fuzzy_nand(Vector3,Vector4,Vector5). % (a nand (a nand b)) nand (b nand (a nand b))

%% dss_fuzzy_imp(+Vector1,+Vector2,-ImpVector)
%  
%  u(a -> b) = u(a nand u(b nand b))

dss_fuzzy_imp(Vector0,Vector1,Vector3) :-
        dss_fuzzy_nand(Vector1,Vector1,Vector2), % b nand b
        dss_fuzzy_nand(Vector0,Vector2,Vector3). % a nand (b nand b)