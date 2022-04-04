
%%
% frank-world.pl
%
% Copyright 2014-2017 Harm Brouwer <me@hbrouwer.eu>
%     and Noortje Venhuizen <njvenhuizen@gmail.com> 
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

%%
% This implements the microworld described in: 
%
% Frank, S. L., Haselager, W. F. G., and van Rooij, I. (2009). Connectionist
%     semantic systematicity. Cognition, 110(3):358–379.
%
% Note: Some comments in the code are literal citations from this paper.
%%

:- use_module('../src/dispace.pl').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                             E V E N T S                               %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

event(play(P,G)) :-
        person(P),
        game(G).
event(play(P,T)) :-
        person(P),
        toy(T).
event(win(P)) :-
        person(P).
event(lose(P)) :-
        person(P).
event(place(P,X)) :-
        person(P),
        place(X).
event(manner(play(P),M)) :-
        person(P),
        manner_play(M).
event(manner(win,M)) :-
        manner_win(M).

person('charlie').
person('heidi').
person('sophia').

game('chess').
game('hide_and_seek').
game('soccer').

toy('puzzle').
toy('ball').
toy('doll').

place('bathroom').
place('bedroom').
place('playground').
place('street').

manner_play('well').
manner_play('badly').

manner_win('easily').
manner_win('difficultly').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                         V I O L A T I O N S                           %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%
% Games and toys.
%
% As listed in Table 4, there are restrictions on the places where each game
% and each toy can be played (with), as well as the number of people that
% can play a particular game or with a particular toy at any one time. Each
% person can only play one game or with one toy at a time. Someone who plays
% soccer, plays with the ball, but no other combination of game and toy is
% possible. Someone who plays well or badly, must play a game.
%%

% There are restrictions on the places where each game and each toy can be
% played (with), ...
%violation(Ts) :- member((play(P,'chess'        ),1),Ts), memberchk((place(P,'bathroom'),1),Ts). % <-- matches paper?
%violation(Ts) :- member((play(P,'chess'        ),1),Ts), memberchk((place(P,'street'  ),1),Ts). % <-- matches paper?
violation(Ts) :- member((play(P,'chess'        ),1),Ts), memberchk((place(P,'bedroom' ),0),Ts).  % <-- matches code?
violation(Ts) :- member((play(P,'hide_and_seek'),1),Ts), memberchk((place(P,'street'  ),1),Ts).
violation(Ts) :- member((play(P,'soccer'       ),1),Ts), memberchk((place(P,'street'  ),0),Ts).
violation(Ts) :- member((play(P,'puzzle'       ),1),Ts), memberchk((place(P,'bedroom' ),0),Ts).
violation(Ts) :- member((play(P,'ball'         ),1),Ts), memberchk((place(P,'bedroom' ),1),Ts).
violation(Ts) :- member((play(P,'ball'         ),1),Ts), memberchk((place(P,'bathroom'),1),Ts).
violation(Ts) :- member((play(P,'doll'         ),1),Ts), memberchk((place(P,'street'  ),1),Ts).
violation(Ts) :- member((play(P,'doll'         ),1),Ts), memberchk((place(P,'bathroom'),1),Ts).

% ... as well as the number of people that can play a particular game or
% with a particular toy at any one time. 
violation(Ts) :-
        findall(P1,(member((play(P1,'chess'),1),Ts)),P1s),
        findall(P0,(member((play(P0,'chess'),0),Ts)),P0s),
        length(P1s,L1),
        length(P0s,L0),
        ( L1 == 3 ; (L1 == 1, L0 == 2) ).
violation(Ts) :-
        findall(P1,(member((play(P1,'hide_and_seek'),1),Ts)),P1s),
        findall(P0,(member((play(P0,'hide_and_seek'),0),Ts)),P0s),
        length(P1s,L1),
        length(P0s,L0),
        L1 == 1, L0 == 2.
violation(Ts) :-
        findall(P1,(member((play(P1,'soccer'),1),Ts)),P1s),
        findall(P0,(member((play(P0,'soccer'),0),Ts)),P0s),
        length(P1s,L1),
        length(P0s,L0),
        L1 == 1, L0 == 2.
violation(Ts) :-
        findall(P1,(member((play(P1,'puzzle'),1),Ts)),P1s),
        length(P1s,L1),
        L1 > 1.

% Each person can only play one game or with one toy at a time.
violation(Ts) :- person(P), findall(G,(member((play(P,G),1),Ts),game(G)),Gs), length(Gs,L), L > 1.
violation(Ts) :- person(P), findall(O,(member((play(P,O),1),Ts),toy(O)),Os), length(Os,L), L > 1.

% Someone who plays soccer, plays with the ball, but no other combination of
% game and toy is possible.
violation(Ts) :-
        member((play(P,G),1),Ts),
        game(G),
        member((play(P,T),1),Ts),
        toy(T),
        \+ (G == 'soccer', T == 'ball').

% XXX: This constraint is there in the code, but not in the paper.

% A person cannot play well and badly at the same time.
violation(Ts) :- member((manner(play(P),'well'),1),Ts), memberchk((manner(play(P),'badly'),1),Ts).

%%
% Being there.
%
% Everybody is at exactly one place. If someone plays hide_and_seek in the
% playground, all players are in the playground. The two players of a chess
% match are in the same place. The girls tend to hang out at the same place,
% while charlie avoids them.
%%

% Everybody is at exactly one place.
violation(Ts) :- person(P), findall(X,member((place(P,X),0),Ts),Xs), length(Xs,L), L == 4.
violation(Ts) :- person(P), findall(X,member((place(P,X),1),Ts),Xs), length(Xs,L), L > 1.

% If someone plays hide_and_seek in the playground, all players are in the
% playground.
violation(Ts) :-
        member((place(P0,'playground'),1),Ts),
        memberchk((play(P0,'hide_and_seek'),1),Ts),
        member((play(P1,'hide_and_seek'),1),Ts),
        memberchk((place(P1,'playground'),0),Ts).

% XXX: Whereas this constraint is in the paper, I cannot find it in the
% code:

% The two players of a chess match are in the same place.
violation(Ts) :-
        member((play(P0,'chess'),1),Ts),
        member((play(P1,'chess'),1),Ts),
        P1 \= P0,
        memberchk((place(P0,X0),1),Ts),
        memberchk((place(P1,X1),1),Ts),
        X0 \= X1.

%%
% Winning and losing.
%
% One cannot both win and lose, nor can two people win at the same time. If
% someone wins, all other players lose, and if there is a loser, there must
% be one winner. Someone who wins or loses, plays a game. Someone who plays
% well is more likely to win, and whoever plays badly is more likely to
% lose. Winning is usually done easily by someone who plays well and
% difficultly by those playing badly.
%%

% One cannot both win and lose, ...
violation(Ts) :- member((win(P),1),Ts), memberchk((lose(P),1),Ts).

% ..., nor can two people win at the same time.
violation(Ts) :- member((win(P0),1),Ts), member((win(P1),1),Ts), P0 \= P1.

% NOTE: This seems wrong in Stefan's code; in case of a winner, he
% checks if there is someone who loses, and not if all other players
% of the same game lose.

%violation(Ts) :-
%        member((win(_),1),Ts),
%        findall(L,member((lose(L),0),Ts),Ls),
%        length(Ls,LL),
%        LL = 3.

% If someone wins, all other players lose, ...
violation(Ts) :-
        member((win(P0),1),Ts),
        game(G),
        memberchk((play(P0,G),1),Ts),
        member((play(P1,G),1),Ts),
        P0 \= P1,
        memberchk((lose(P1),0),Ts).

% If a game is won in some manner, there needs to be a winner, and if there
% is a loser, there must be one winner.
violation(Ts) :-
        ( memberchk((lose(_),1),Ts) ; memberchk((manner(win,_),1),Ts)),
        findall(W,member((win(W),0),Ts),Ws),
        length(Ws,WL),
        WL == 3.

% Someone who wins or loses, plays a game. And, someone who plays well or
% badly, must play a game.
violation(Ts) :-
        ( member((win(P),1),Ts) ; member((lose(P),1),Ts) ; member((manner(play(P),_),1),Ts) ),
        findall(G,(member((play(P,G),0),Ts),game(G)),Gs),
        length(Gs,GL),
        GL == 3.

% XXX: These constraints are there in the code, but not in the paper.

% A game cannot be won easily and difficultly at the same time.
violation(Ts) :- member((manner(win,'easily'),1),Ts), memberchk((manner(win,'difficultly'),1),Ts).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                      P R O B A B I L I T I E S                        %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

base_probability(play('charlie','chess'        ),0.6).
base_probability(play('charlie','hide_and_seek'),0.2).
base_probability(play('charlie','soccer'       ),0.4).

base_probability(play('heidi','chess'          ),0.4).
base_probability(play('heidi','hide_and_seek'  ),0.3).
base_probability(play('heidi','soccer'         ),0.4).

base_probability(play('sophia','chess'         ),0.4).
base_probability(play('sophia','hide_and_seek' ),0.2).
base_probability(play('sophia','soccer'        ),0.6).

base_probability(play('charlie','puzzle'       ),0.6).
base_probability(play('charlie','ball'         ),0.4).
base_probability(play('charlie','doll'         ),0.4).

base_probability(play('heidi','puzzle'         ),0.4).
base_probability(play('heidi','ball'           ),0.4).
base_probability(play('heidi','doll'           ),0.6).

base_probability(play('sophia','puzzle'        ),0.4).
base_probability(play('sophia','ball'          ),0.6).
base_probability(play('sophia','doll'          ),0.4).

base_probability(win(_                         ),0.2).

base_probability(lose(_                        ),0.2).

base_probability(place('charlie','bathroom'    ),0.6).
base_probability(place('charlie','bedroom'     ),0.6).
base_probability(place('charlie','playground'  ),0.4).
base_probability(place('charlie','street'      ),0.4).

base_probability(place('heidi','bathroom'      ),0.5).
base_probability(place('heidi','bedroom'       ),0.5).
base_probability(place('heidi','playground'    ),0.5).
base_probability(place('heidi','street'        ),0.5).

base_probability(place('sophia','bathroom'     ),0.5).
base_probability(place('sophia','bedroom'      ),0.5).
base_probability(place('sophia','playground'   ),0.6).
base_probability(place('sophia','street'       ),0.6).

base_probability(manner(play(_),_              ),0.2).

base_probability(manner(win,_                  ),0.1).

base_probability(_,0.5).

%%
% Personal characteristics.
%
% Each of the three people in our microworld has a ‘specialty’: a game that
% (s)he usually and more easily wins.  Also, charlie, heidi, and sophia
% differ in preferred toy (the one most often played with) and places most
% often visited. For example, play(charlie, puzzle) occurs more often than
% either play(charlie, ball) or play(charlie, doll), and win(sophia) is more
% likely to co-occur with play(sophia, soccer) than with either play(sophia,
% chess) or play(sophia, hide_and_seek).
%%

max_probability(0.95).

specialty('charlie','chess').
specialty('heidi','hide_and_seek').
specialty('sophia','soccer').

% Playing his/her speciality G is more or less probable for player
% P depending on whether he/she is winning or losing.
probability(play(P,G),Ts,Pr) :-
        base_probability(play(P,G),Bpr),

        specialty(P,G),
        ( memberchk((win(P),1),Ts)  -> Adj0 is 1.0 * 2.0 ; Adj0 is 1.0 ),
        ( memberchk((lose(P),1),Ts) -> Adj1 is 1.0 / 2.0 ; Adj1 is 1.0 ),
        
        max_probability(Mpr),
        Pr is min(Bpr * Adj0 * Adj1,Mpr).

% Winning is: 
% * more probable if a player is playing his/her specialty;
% * more probable if a player is playing well;
% * ... and even more probable if the win is easy;
% * less probable if a player is playing badly; 
% * ... and even less probable if th win is easy.
probability(win(P),Ts,Pr) :-
        base_probability(win(P),Bpr),

        (  memberchk((play(P,S),1),Ts), specialty(P,S)  -> Adj0 is 1.0 * 2.0 ; Adj0 is 1.0 ),

        (  memberchk((manner(play(P),'well'),1),Ts)
        -> ( memberchk((manner(win,'easily'),1),Ts)     -> Adj1 is 2.0 * 1.5 ; Adj1 is 2.0 ),
           ( memberchk((manner(win,'difficultly'),1),Ts) -> Adj1 is 2.0 / 1.5 ; Adj1 is 2.0 )
        ;  Adj1 is 2.0 ),

        ( memberchk((manner(play(P),'badly'),1),Ts) 
        -> ( memberchk((manner(win,'easily'),1),Ts)     -> Adj2 is 0.5 / 1.5 ; Adj2 is 0.5 ),
           ( memberchk((manner(win,'difficultly'),1),Ts) -> Adj2 is 0.5 * 1.5 ; Adj2 is 0.5 )
        ;  Adj2 is 0.5 ),

        max_probability(Mpr),
        Pr is min(Bpr * Adj0 * Adj1 * Adj2,Mpr).

% Losing is:
% * less probable if a player is playing his/her specialty;
% * less probable if a player is playing well;
% * ... and even less probable if the win is easy;
% * more probable if a player is playing badly;
% * ... and even more probable if the win is easy.
probability(lose(P),Ts,Pr) :-
        base_probability(lose(P),Bpr),

        (  memberchk((play(P,S),1),Ts), specialty(P,S)  -> Adj0 is 1.0 / 2.0 ; Adj0 is 1.0 ),

        (  memberchk((manner(play(P),'well'),1),Ts)
        -> ( memberchk((manner(win,'easily'),1),Ts)     -> Adj1 is 0.5 / 1.5 ; Adj1 is 0.5 ),
           ( memberchk((manner(win,'difficultly'),1),Ts) -> Adj1 is 0.5 * 1.5 ; Adj1 is 0.5 )
        ;  Adj1 is 0.5 ),

        ( memberchk((manner(play(P),'badly'),1),Ts) 
        -> ( memberchk((manner(win,'easily'),1),Ts)     -> Adj2 is 2.0 * 1.5 ; Adj2 is 2.0 ),
           ( memberchk((manner(win,'difficultly'),1),Ts) -> Adj2 is 2.0 / 1.5 ; Adj2 is 2.0 )
        ;  Adj2 is 2.0 ),

        max_probability(Mpr),
        Pr is min(Bpr * Adj0 * Adj1 * Adj2,Mpr).

% The girls tend to hang out at the same place, while charlie avoids them.
probability(place('charlie',X),Ts,Pr) :-
        base_probability(place('charlie',X),Bpr),
        
        ((memberchk((place('heidi',X),1),Ts) ; 
          memberchk((place('sophia',X),1),Ts)) -> Adj0 is 1.0 / 2.0 ; Adj0 is 1.0 ),
        ( memberchk((place('heidi',X),1),Ts),
          memberchk((place('sophia',X),1),Ts)  -> Adj1 is 1.0 / 1.5 ; Adj1 is 1.0 ),
        ((memberchk((place('heidi',X),0),Ts) ; 
          memberchk((place('sophia',X),0),Ts)) -> Adj2 is 1.0 * 2.0 ; Adj2 is 1.0 ),
        ( memberchk((place('heidi',X),0),Ts),
          memberchk((place('sophia',X),0),Ts)  -> Adj3 is 1.0 * 1.5 ; Adj3 is 1.0 ),

        max_probability(Mpr),
        Pr is min(Bpr * Adj0 * Adj1 * Adj2 * Adj3,Mpr).

probability(place('heidi',X),Ts,Pr) :-
        base_probability(place('heidi',X),Bpr),

        ( memberchk((place('charlie',X),1),Ts) -> Adj0 is 1.0 / 1.5 ; Adj0 is 1.0 ),
        ( memberchk((place('charlie',X),0),Ts) -> Adj1 is 1.0 * 1.5 ; Adj1 is 1.0 ),
        ( memberchk((place('sophia', X),1),Ts) -> Adj2 is 1.0 * 2.0 ; Adj2 is 1.0 ),
        ( memberchk((place('sophia', X),0),Ts) -> Adj3 is 1.0 / 2.0 ; Adj3 is 1.0 ),

        max_probability(Mpr),
        Pr is min(Bpr * Adj0 * Adj1 * Adj2 * Adj3,Mpr).

probability(place('sophia',X),Ts,Pr) :-
        base_probability(place('sophia',X),Bpr),

        ( memberchk((place('charlie',X),1),Ts) -> Adj0 is 1.0 / 1.5 ; Adj0 is 1.0 ),
        ( memberchk((place('charlie',X),0),Ts) -> Adj1 is 1.0 * 1.5 ; Adj1 is 1.0 ),
        ( memberchk((place('heidi',  X),1),Ts) -> Adj2 is 1.0 * 2.0 ; Adj2 is 1.0 ),
        ( memberchk((place('heidi',  X),0),Ts) -> Adj3 is 1.0 / 2.0 ; Adj3 is 1.0 ),

        max_probability(Mpr),
        Pr is min(Bpr * Adj0 * Adj1 * Adj2 * Adj3,Mpr).

% Playing well is:
% * more probable if a player is winning;
% * ... and even more probable if the win is easy;
% * less probable if a player is losing;
% * ... and even less probable if the win is easy.
probability(manner(play(P),'well'),Ts,Pr) :-
        base_probability(manner(play(P),'well'),Bpr),

        (  memberchk((win(P),1),Ts)
        -> ( memberchk((manner(win,'easily'),1),Ts)     -> Adj0 is 2.0 * 1.5 ; Adj0 is 2.0 ),
           ( memberchk((manner(win,'difficultly'),1),Ts) -> Adj0 is 2.0 / 1.5 ; Adj0 is 2.0 )
        ;  Adj0 is 2.0 ),

        ( memberchk((lose(P),1),Ts)
        -> ( memberchk((manner(win,'easily'),1),Ts)     -> Adj1 is 0.5 / 1.5 ; Adj1 is 0.5 ),
           ( memberchk((manner(win,'difficultly'),1),Ts) -> Adj1 is 0.5 * 1.5 ; Adj1 is 0.5 )
        ;  Adj1 is 0.5 ),

        max_probability(Mpr),
        Pr is min(Bpr * Adj0 * Adj1,Mpr).

% Playing badly is:
% * less probable if a player is winning;
% * ... and even less probable if the win is easy;
% * more probable if a player is losing;
% * ... and even more probable if the win is easy.
probability(manner(play(P),'badly'),Ts,Pr) :-
        base_probability(manner(play(P),'badly'),Bpr),

        (  memberchk((win(P),1),Ts)
        -> ( memberchk((manner(win,'easily'),1),Ts)     -> Adj0 is 0.5 / 1.5 ; Adj0 is 0.5 ),
           ( memberchk((manner(win,'difficultly'),1),Ts) -> Adj0 is 0.5 * 1.5 ; Adj0 is 0.5 )
        ;  Adj0 is 0.5 ),

        ( memberchk((lose(P),1),Ts)
        -> ( memberchk((manner(win,'easily'),1),Ts)     -> Adj1 is 2.0 * 1.5 ; Adj1 is 2.0 ),
           ( memberchk((manner(win,'difficultly'),1),Ts) -> Adj1 is 2.0 / 1.5 ; Adj1 is 2.0 )
        ;  Adj1 is 2.0 ),

        max_probability(Mpr),
        Pr is min(Bpr * Adj0 * Adj1,Mpr).

% Winning easily is:
% * more probable if a player is winning his/her specialty;
% * less probable if a player is losing his/her specialty;
% * more probable if a player is winning and playing well;
% * less probable if a player is winning and playing badly;
% * less probable if a player is losing and playing well;
% * more probable if a player is losing and playing badly.
probability(manner(win,'easily'),Ts,Pr) :-
        base_probability(manner(win,'easily'),Bpr),

        ( member((play(P,G),1),Ts), specialty(P,G), memberchk((win(P),1),Ts)  -> Adj0 is 1.0 * 2.0 ; Adj0 is 1.0 ),
        ( member((play(P,G),1),Ts), specialty(P,G), memberchk((lose(P),1),Ts) -> Adj1 is 1.0 / 2.0 ; Adj1 is 1.0 ),

        ( member((win(P),1),Ts), memberchk((manner(play(P),'well')),Ts)   -> Adj2 is 1.0 * 2.0 ; Adj2 is 1.0 ),
        ( member((win(P),1),Ts), memberchk((manner(play(P),'badly')),Ts)  -> Adj3 is 1.0 / 2.0 ; Adj3 is 1.0 ),

        ( member((lose(P),1),Ts), memberchk((manner(play(P),'well')),Ts)  -> Adj4 is 1.0 / 2.0 ; Adj4 is 1.0 ),
        ( member((lose(P),1),Ts), memberchk((manner(play(P),'badly')),Ts) -> Adj5 is 1.0 * 2.0 ; Adj5 is 1.0 ),

        max_probability(Mpr),
        Pr is min(Bpr * Adj0 * Adj1 * Adj2 * Adj3 * Adj4 * Adj5,Mpr).

% Winning difficultly is:
% * more probable if a player is winning a non-specialty game;
% * less probable if a player is losing a non-specialty game;
% * less probable if a player is winning and playing well;
% * more probable if a player is winning and playing badly;
% * more probable if a player is losing and playing well;
% * less probable if a player is losing and playing badly.
probability(manner(win,'difficultly'),Ts,Pr) :-
        base_probability(manner(win,'difficultly'),Bpr),

        ( member((play(P,G),1),Ts), \+ specialty(P,G), memberchk((win(P),1),Ts)  -> Adj0 is 1.0 * 1.5 ; Adj0 is 1.0 ),
        ( member((play(P,G),1),Ts), \+ specialty(P,G), memberchk((lose(P),1),Ts) -> Adj1 is 1.0 / 1.5 ; Adj1 is 1.0 ),

        ( member((win(P),1),Ts), memberchk((manner(play(P),'well')),Ts)   -> Adj2 is 1.0 / 2.0 ; Adj2 is 1.0 ),
        ( member((win(P),1),Ts), memberchk((manner(play(P),'badly')),Ts)  -> Adj3 is 1.0 * 2.0 ; Adj3 is 1.0 ),

        ( member((lose(P),1),Ts), memberchk((manner(play(P),'well')),Ts)  -> Adj4 is 1.0 * 2.0 ; Adj4 is 1.0 ),
        ( member((lose(P),1),Ts), memberchk((manner(play(P),'badly')),Ts) -> Adj5 is 1.0 / 2.0 ; Adj5 is 1.0 ),

        max_probability(Mpr),
        Pr is min(Bpr * Adj0 * Adj1 * Adj2 * Adj3 * Adj4 * Adj5,Mpr).

%probability(_,_,0.5). % <-- coin flip
probability(E,_,Pr) :-
        base_probability(E,Pr).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                           S E N T E N C E S                           %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sentence('train',Sem) --> s_np_vp_app(Sem).
  
s_np_vp_app(Sem) --> n(N,NP1), vp(N,V,NP1,NP2), app(N,V,NP1,NP2,Sem).

n(person,charlie) --> ['charlie'].
n(person,heidi)   --> ['heidi'].
n(person,sophia)  --> ['sophia'].
n(person,someone) --> ['someone'].
n(person,boy)     --> ['boy'].
n(person,girl)    --> ['girl'].

n(game,chess)         --> ['chess'].
n(game,hide_and_seek) --> ['hide_and_seek'].
n(game,soccer)        --> ['soccer'].
n(game,football)      --> ['football'].
n(game,game)          --> ['game'].

n(toy,puzzle) --> ['puzzle'].
n(toy,ball)   --> ['ball'].
n(toy,doll)   --> ['doll'].
n(toy,jigsaw) --> ['jigsaw'].
n(toy,toy)    --> ['toy'].

vp(person,play,_,_) --> ['plays'].

vp(person,win,_,_)  --> ['wins'].
vp(person,win,NP1,NP2) --> ['beats'], n(person,NP2), { NP1 \= NP2 }. %% WRONG?

vp(person,lose,_,_)  --> ['loses'].
vp(person,lose,NP1,NP2) --> ['loses','to'], n(person,NP2), { NP1 \= NP2 }. %% WRONG?

vp(game,play,_,_) --> ['is','played'].
vp(game,win,_,_) --> ['is','won'].
vp(game,lose,_,_) --> ['is','lost'].

vp(toy,play,_,_) --> ['is','played','with']. 

% app(person,play) --> [n(game)], [manner], [place]
app(person,play,NP1,_,Sem) --> [],
        { semantics_subject(NP1,S),
          semantics_object(something,O),
          dss_semantics_event(play,S,O,Sem) }.
app(person,play,NP1,_,Sem) --> n(game,G),
        { semantics_subject(NP1,S),
          semantics_object(G,O),
          dss_semantics_event(play,S,O,Sem) }.
app(person,play,NP1,_,Sem) --> manner(M),
        { semantics_subject(NP1,S),
          semantics_object(something,O),
          dss_semantics_event(play,S,O,Sem0),
          dss_embed_in_event(S,play,ES),
          dss_semantics_event(manner,ES,[M],Sem1),
          dss_coordinate_disjuncts(Sem0,Sem1,Sem) }.
app(person,play,NP1,_,Sem) --> place(X),
        { semantics_subject(NP1,S),
          semantics_object(something,O),
          dss_semantics_event(play,S,O,Sem0),
          semantics_place(X,P),
          dss_semantics_event(place,S,P,Sem1),
          dss_coordinate_disjuncts(Sem0,Sem1,Sem) }.
app(person,play,NP1,_,Sem) --> n(game,G), manner(M),
        { semantics_subject(NP1,S),
          semantics_object(G,O),
          dss_semantics_event(play,S,O,Sem0),
          dss_embed_in_event(S,play,ES),
          dss_semantics_event(manner,ES,[M],Sem1),
          dss_coordinate_disjuncts(Sem0,Sem1,Sem) }.
app(person,play,NP1,_,Sem) --> n(game,G), place(X),
        { semantics_subject(NP1,S),
          semantics_object(G,O),
          dss_semantics_event(play,S,O,Sem0),
          semantics_place(X,P),
          dss_semantics_event(place,S,P,Sem1),
          dss_coordinate_disjuncts(Sem0,Sem1,Sem) }.
app(person,play,NP1,_,Sem) --> manner(M), place(X),
        { semantics_subject(NP1,S),
          semantics_object(something,O),
          dss_semantics_event(play,S,O,Sem0),
          dss_embed_in_event(S,play,ES),
          dss_semantics_event(manner,ES,[M],Sem1),
          dss_coordinate_disjuncts(Sem0,Sem1,Sem2),
          semantics_place(X,P),
          dss_semantics_event(place,S,P,Sem3),
          dss_coordinate_disjuncts(Sem2,Sem3,Sem) }.
app(person,play,NP1,_,Sem) --> n(game,G), manner(M), place(X),
        { semantics_subject(NP1,S),
          semantics_object(G,O),
          dss_semantics_event(play,S,O,Sem0),
          dss_embed_in_event(S,play,ES),
          dss_semantics_event(manner,ES,[M],Sem1),
          dss_coordinate_disjuncts(Sem0,Sem1,Sem2),
          semantics_place(X,P),
          dss_semantics_event(place,S,P,Sem3),
          dss_coordinate_disjuncts(Sem2,Sem3,Sem) }.
% app(person,play) --> pp(toy), [place]
app(person,play,NP1,_,Sem) --> pp(toy,T),
        { semantics_subject(NP1,S),
          semantics_object(T,O),
          dss_semantics_event(play,S,O,Sem) }.
app(person,play,NP1,_,Sem) --> pp(toy,T), place(X),
        { semantics_subject(NP1,S),
          semantics_object(T,O),
          dss_semantics_event(play,S,O,Sem0),
          semantics_place(X,P),
          dss_semantics_event(place,S,P,Sem1), 
          dss_coordinate_disjuncts(Sem0,Sem1,Sem) }.
% app(person,play) --> place, pp(toy)
app(person,play,NP1,_,Sem) --> place(X), pp(toy,T),
        { semantics_subject(NP1,S),
          semantics_object(T,O),
          dss_semantics_event(play,S,O,Sem0),
          semantics_place(X,P),
          dss_semantics_event(place,S,P,Sem1), 
          dss_coordinate_disjuncts(Sem0,Sem1,Sem) }.

% app(person,win) --> [pp(manner)], [pp(game)], [place]
app(person,win,NP1,NP2,Sem) --> [],
        { semantics_subject(NP1,S1),
          dss_semantics_event(win,S1,[],Sem0),
          semantics_losers(NP2,Sem0,Sem) }.
app(person,win,NP1,NP2,Sem) --> pp(manner,M),
        { semantics_subject(NP1,S),
          dss_semantics_event(win,S,[],Sem0),
          dss_semantics_event(manner,[win],[M],Sem1),
          Sem2 = and(Sem0,Sem1),
          semantics_losers(NP2,Sem2,Sem) }.
app(person,win,NP1,NP2,Sem) --> pp(game,G),
        { semantics_subject(NP1,S),
          dss_semantics_event(win,S,[],Sem0),
          semantics_object(G,O),
          dss_semantics_event(play,S,O,Sem1),
          dss_coordinate_disjuncts(Sem0,Sem1,Sem2),
          semantics_losers(NP2,Sem2,Sem) }.
app(person,win,NP1,NP2,Sem) --> place(X),
        { semantics_subject(NP1,S),
          dss_semantics_event(win,S,[],Sem0),
          semantics_place(X,P),
          dss_semantics_event(place,S,P,Sem1),
          dss_coordinate_disjuncts(Sem0,Sem1,Sem2),
          semantics_losers(NP2,Sem2,Sem) }.
app(person,win,NP1,NP2,Sem) --> pp(manner,M), pp(game,G),
        { semantics_subject(NP1,S),
          dss_semantics_event(win,S,[],Sem0),
          semantics_object(G,O),
          dss_semantics_event(play,S,O,Sem1),
          dss_coordinate_disjuncts(Sem0,Sem1,Sem2),
          dss_semantics_event(manner,[win],[M],Sem3),
          Sem4 = and(Sem2,Sem3),
          semantics_losers(NP2,Sem4,Sem) }.
app(person,win,NP1,NP2,Sem) --> pp(manner,M), place(X),
        { semantics_subject(NP1,S),
          dss_semantics_event(win,S,[],Sem0),
          semantics_place(X,P),
          dss_semantics_event(place,S,P,Sem1),
          dss_coordinate_disjuncts(Sem0,Sem1,Sem2),
          dss_semantics_event(manner,[win],[M],Sem3),
          Sem4 = and(Sem2,Sem3),
          semantics_losers(NP2,Sem4,Sem) }.
app(person,win,NP1,NP2,Sem) --> pp(game,G), place(X),
        { semantics_subject(NP1,S),
          dss_semantics_event(win,S,[],Sem0),
          semantics_object(G,O),
          dss_semantics_event(play,S,O,Sem1),
          dss_coordinate_disjuncts(Sem0,Sem1,Sem2),
          semantics_place(X,P),
          dss_semantics_event(place,S,P,Sem3),
          dss_coordinate_disjuncts(Sem2,Sem3,Sem4),
          semantics_losers(NP2,Sem4,Sem) }.
app(person,win,NP1,NP2,Sem) --> pp(manner,M), pp(game,G), place(X),
        { semantics_subject(NP1,S),
          dss_semantics_event(win,S,[],Sem0),
          semantics_object(G,O),
          dss_semantics_event(play,S,O,Sem1),
          dss_coordinate_disjuncts(Sem0,Sem1,Sem2),
          semantics_place(X,P),
          dss_semantics_event(place,S,P,Sem3),
          dss_coordinate_disjuncts(Sem2,Sem3,Sem4),
          dss_semantics_event(manner,[win],[M],Sem5),
          Sem6 = and(Sem4,Sem5),
          semantics_losers(NP2,Sem6,Sem) }.
% app(person,win) --> pp(game), pp(manner)
app(person,win,NP1,NP2,Sem) --> pp(game,G), pp(manner,M),
        { semantics_subject(NP1,S),
          dss_semantics_event(win,S,[],Sem0),
          semantics_object(G,O),
          dss_semantics_event(play,S,O,Sem1),
          dss_coordinate_disjuncts(Sem0,Sem1,Sem2),
          dss_semantics_event(manner,[win],[M],Sem3),
          Sem4 = and(Sem2,Sem3),
          semantics_losers(NP2,Sem4,Sem) }.
% app(person,win) --> place, pp(game)
app(person,win,NP1,NP2,Sem) --> place(X), pp(game,G),
        { semantics_subject(NP1,S),
          dss_semantics_event(win,S,[],Sem0),
          semantics_object(G,O),
          dss_semantics_event(play,S,O,Sem1),
          dss_coordinate_disjuncts(Sem0,Sem1,Sem2),
          semantics_place(X,P),
          dss_semantics_event(place,S,P,Sem3),
          dss_coordinate_disjuncts(Sem2,Sem3,Sem4),
          semantics_losers(NP2,Sem4,Sem) }.

  % app(person,lose) --> [pp(game)], [place]
app(person,lose,NP1,NP2,Sem) --> [],
        { semantics_subject(NP1,S),
          dss_semantics_event(lose,S,[],Sem0),
          semantics_winners(NP2,Sem0,Sem) }.
app(person,lose,NP1,NP2,Sem) --> pp(game,G),
        { semantics_subject(NP1,S),
          dss_semantics_event(lose,S,[],Sem0),
          semantics_object(G,O),
          dss_semantics_event(play,S,O,Sem1),
          dss_coordinate_disjuncts(Sem0,Sem1,Sem2),
          semantics_winners(NP2,Sem2,Sem) }.
app(person,lose,NP1,NP2,Sem) --> place(X),
        { semantics_subject(NP1,S),
          dss_semantics_event(lose,S,[],Sem0),
          semantics_place(X,P),
          dss_semantics_event(place,S,P,Sem1),
          dss_coordinate_disjuncts(Sem0,Sem1,Sem2),
          semantics_winners(NP2,Sem2,Sem) }.
app(person,lose,NP1,NP2,Sem) --> pp(game,G), place(X),
        { semantics_subject(NP1,S),
          dss_semantics_event(lose,S,[],Sem0),
          semantics_object(G,O),
          dss_semantics_event(play,S,O,Sem1),
          dss_coordinate_disjuncts(Sem0,Sem1,Sem2),
          semantics_place(X,P),
          dss_semantics_event(place,S,P,Sem3),
          dss_coordinate_disjuncts(Sem2,Sem3,Sem4),
          semantics_winners(NP2,Sem4,Sem) }.
% app(person,lose) --> place, pp(game)
app(person,lose,NP1,NP2,Sem) --> place(X), pp(game,G),
        { semantics_subject(NP1,S),
          dss_semantics_event(lose,S,[],Sem0),
          semantics_object(G,O),
          dss_semantics_event(play,S,O,Sem1),
          dss_coordinate_disjuncts(Sem0,Sem1,Sem2),
          semantics_place(X,P),
          dss_semantics_event(place,S,P,Sem3),
          dss_coordinate_disjuncts(Sem2,Sem3,Sem4),
          semantics_winners(NP2,Sem4,Sem) }.

% app(game,play) --> [manner], [pp(person)], [place]
app(game,play,NP1,_,Sem) --> [],
        { semantics_subject(someone,S),
          semantics_object(NP1,O),
          dss_semantics_event(play,S,O,Sem) }.
app(game,play,NP1,_,Sem) --> manner(M),
        { semantics_subject(someone,S),
          semantics_object(NP1,O),
          dss_semantics_event(play,S,O,Sem0),
          dss_embed_in_event(S,play,ES),
          dss_semantics_event(manner,ES,[M],Sem1),
          dss_coordinate_disjuncts(Sem0,Sem1,Sem) }.
app(game,play,NP1,_,Sem) --> pp(person,NP2),
        { semantics_subject(NP2,S),
          semantics_object(NP1,O),
          dss_semantics_event(play,S,O,Sem) }.
app(game,play,NP1,_,Sem) --> place(X),
        { semantics_subject(someone,S),
          semantics_object(NP1,O),
          dss_semantics_event(play,S,O,Sem0),
          semantics_place(X,P),
          dss_semantics_event(place,S,P,Sem1),
          dss_coordinate_disjuncts(Sem0,Sem1,Sem) }.
app(game,play,NP1,_,Sem) --> manner(M), pp(person,NP2),
        { semantics_subject(NP2,S),
          semantics_object(NP1,O),
          dss_semantics_event(play,S,O,Sem0),
          dss_embed_in_event(S,play,ES),
          dss_semantics_event(manner,ES,[M],Sem1),
          dss_coordinate_disjuncts(Sem0,Sem1,Sem) }. 
app(game,play,NP1,_,Sem) --> manner(M), place(X),
        { semantics_subject(someone,S),
          semantics_object(NP1,O),
          dss_semantics_event(play,S,O,Sem0),
          semantics_place(X,P),
          dss_semantics_event(place,S,P,Sem1),
          dss_coordinate_disjuncts(Sem0,Sem1,Sem2),
          dss_embed_in_event(S,play,ES),
          dss_semantics_event(manner,ES,[M],Sem3),
          dss_coordinate_disjuncts(Sem2,Sem3,Sem) }.
app(game,play,NP1,_,Sem) --> pp(person,NP2), place(X),
        { semantics_subject(NP2,S),
          semantics_object(NP1,O),
          dss_semantics_event(play,S,O,Sem0),
          semantics_place(X,P),
          dss_semantics_event(place,S,P,Sem1),
          dss_coordinate_disjuncts(Sem0,Sem1,Sem) }.
app(game,play,NP1,_,Sem) --> manner(M), pp(person,NP2), place(X),
        { semantics_subject(NP2,S),
          semantics_object(NP1,O),
          dss_semantics_event(play,S,O,Sem0),
          semantics_place(X,P),
          dss_semantics_event(place,S,P,Sem1),
          dss_coordinate_disjuncts(Sem0,Sem1,Sem2),
          dss_embed_in_event(S,play,ES),
          dss_semantics_event(manner,ES,[M],Sem3),
          dss_coordinate_disjuncts(Sem2,Sem3,Sem) }.

% app(game,win) --> [pp(manner)], [pp(person)], [place]
app(game,win,NP1,_,Sem) --> [],
        { semantics_subject(someone,S),
          semantics_object(NP1,O),
          dss_semantics_event(play,S,O,Sem0),
          dss_semantics_event(win,S,[],Sem1),
          dss_coordinate_disjuncts(Sem0,Sem1,Sem) }.
app(game,win,NP1,_,Sem) --> pp(manner,M),
        { semantics_subject(someone,S),
          semantics_object(NP1,O),
          dss_semantics_event(play,S,O,Sem0),
          dss_semantics_event(win,S,[],Sem1),
          dss_coordinate_disjuncts(Sem0,Sem1,Sem2),
          dss_semantics_event(manner,[win],[M],Sem3),
          Sem = and(Sem2,Sem3) }.
app(game,win,NP1,NP2,Sem) --> pp(person,NP2),
        { semantics_subject(NP2,S),
          semantics_object(NP1,O),
          dss_semantics_event(play,S,O,Sem0),
          dss_semantics_event(win,S,[],Sem1),
          dss_coordinate_disjuncts(Sem0,Sem1,Sem) }.
app(game,win,NP1,_,Sem) --> place(X),
        { semantics_subject(someone,S),
          semantics_object(NP1,O),
          dss_semantics_event(play,S,O,Sem0),
          dss_semantics_event(win,S,[],Sem1),
          dss_coordinate_disjuncts(Sem0,Sem1,Sem2),
          semantics_place(X,P),
          dss_semantics_event(place,S,P,Sem3),
          dss_coordinate_disjuncts(Sem2,Sem3,Sem) }.
app(game,win,NP1,_,Sem) --> pp(manner,M), pp(person,NP2),
        { semantics_subject(NP2,S),
          semantics_object(NP1,O),
          dss_semantics_event(play,S,O,Sem0),
          dss_semantics_event(win,S,[],Sem1),
          dss_coordinate_disjuncts(Sem0,Sem1,Sem2),
          dss_semantics_event(manner,[win],[M],Sem3),
          Sem = and(Sem2,Sem3) }.
app(game,win,NP1,_,Sem) --> pp(manner,M), place(X),
        { semantics_subject(someone,S),
          semantics_object(NP1,O),
          dss_semantics_event(play,S,O,Sem0),
          dss_semantics_event(win,S,[],Sem1),
          dss_coordinate_disjuncts(Sem0,Sem1,Sem2),
          semantics_place(X,P),
          dss_semantics_event(place,S,P,Sem3),
          dss_coordinate_disjuncts(Sem2,Sem3,Sem4),
          dss_semantics_event(manner,[win],[M],Sem5),
          Sem = and(Sem4,Sem5) }.
app(game,win,NP1,_,Sem) --> pp(person,NP2), place(X),
        { semantics_subject(NP2,S),
          semantics_object(NP1,O),
          dss_semantics_event(play,S,O,Sem0),
          dss_semantics_event(win,S,[],Sem1),
          dss_coordinate_disjuncts(Sem0,Sem1,Sem2),
          semantics_place(X,P),
          dss_semantics_event(place,S,P,Sem3),
          dss_coordinate_disjuncts(Sem2,Sem3,Sem) }.
app(game,win,NP1,_,Sem) --> pp(manner,M), pp(person,NP2), place(X),
        { semantics_subject(NP2,S),
          semantics_object(NP1,O),
          dss_semantics_event(play,S,O,Sem0),
          dss_semantics_event(win,S,[],Sem1),
          dss_coordinate_disjuncts(Sem0,Sem1,Sem2),
          semantics_place(X,P),
          dss_semantics_event(place,S,P,Sem3),
          dss_coordinate_disjuncts(Sem2,Sem3,Sem4),
          dss_semantics_event(manner,[win],[M],Sem5),
          Sem = and(Sem4,Sem5) }.

% app(game,lose) --> [pp(person)], [place]
app(game,lose,NP1,_,Sem) --> [],
        { semantics_subject(someone,S),
          semantics_object(NP1,O),
          dss_semantics_event(play,S,O,Sem0),
          dss_semantics_event(lose,S,[],Sem1),
          dss_coordinate_disjuncts(Sem0,Sem1,Sem) }.
app(game,lose,NP1,_,Sem) --> pp(person,NP2),
        { semantics_subject(NP2,S),
          semantics_object(NP1,O),
          dss_semantics_event(play,S,O,Sem0),
          dss_semantics_event(lose,S,[],Sem1),
          dss_coordinate_disjuncts(Sem0,Sem1,Sem) }.
app(game,lose,NP1,_,Sem) --> place(X),
        { semantics_subject(someone,S),
          semantics_object(NP1,O),
          dss_semantics_event(play,S,O,Sem0),
          dss_semantics_event(lose,S,[],Sem1),
          dss_coordinate_disjuncts(Sem0,Sem1,Sem2),
          semantics_place(X,P),
          dss_semantics_event(place,S,P,Sem3),
          dss_coordinate_disjuncts(Sem2,Sem3,Sem) }.
app(game,lose,NP1,_,Sem) --> pp(person,NP2), place(X),
        { semantics_subject(NP2,S),
          semantics_object(NP1,O),
          dss_semantics_event(play,S,O,Sem0),
          dss_semantics_event(lose,S,[],Sem1),
          dss_coordinate_disjuncts(Sem0,Sem1,Sem2),
          semantics_place(X,P),
          dss_semantics_event(place,S,P,Sem3),
          dss_coordinate_disjuncts(Sem2,Sem3,Sem) }.

% app(toy,play) --> [pp(person)], [place]
app(toy,play,NP1,_,Sem) --> [],
        { semantics_subject(someone,S),
          semantics_object(NP1,O),
          dss_semantics_event(play,S,O,Sem) }.
app(toy,play,NP1,_,Sem) --> pp(person,NP2),
        { semantics_subject(NP2,S),
          semantics_object(NP1,O),
          dss_semantics_event(play,S,O,Sem) }.
app(toy,play,NP1,_,Sem) --> place(X),
        { semantics_subject(someone,S),
          semantics_object(NP1,O),
          dss_semantics_event(play,S,O,Sem1),
          semantics_place(X,P),
          dss_semantics_event(place,S,P,Sem2),
          dss_coordinate_disjuncts(Sem1,Sem2,Sem) }.
app(toy,play,NP1,_,Sem) --> pp(person,NP2), place(X),
        { semantics_subject(NP2,S),
          semantics_object(NP1,O),
          dss_semantics_event(play,S,O,Sem1),
          semantics_place(X,P),
          dss_semantics_event(place,S,P,Sem2),
          dss_coordinate_disjuncts(Sem1,Sem2,Sem) }.
% app(toy,play) --> place, pp(person).
app(toy,play,NP1,_,Sem) --> place(X), pp(person,NP2),
        { semantics_subject(NP2,S),
          semantics_object(NP1,O),
          dss_semantics_event(play,S,O,Sem1),
          semantics_place(X,P),
          dss_semantics_event(place,S,P,Sem2),
          dss_coordinate_disjuncts(Sem1,Sem2,Sem) }.

manner(well)  --> ['well'].
manner(badly) --> ['badly'].

place(inside) --> ['inside'].
place(outside) --> ['outside'].
place(X) --> pp(place,X).

pp(place,bathroom)   --> ['in','bathroom'].
pp(place,bathroom)   --> ['in','shower'].
pp(place,bedroom)    --> ['in','bedroom'].
pp(place,street)     --> ['in','street'].
pp(place,playground) --> ['in','playground'].

pp(person,NP) --> ['by'], n(person,NP).

pp(game,NP) --> ['at'], n(game,NP).

pp(toy,NP) --> ['with'], n(toy,NP).

pp(manner,easily)      --> ['with','ease'].
pp(manner,difficultly) --> ['with','difficulty'].

semantics_subject(P,[P]) :- person(P). % charlie v heidi v sophia
semantics_subject(someone,[charlie,heidi,sophia]).
semantics_subject(boy,[charlie]).
semantics_subject(girl,[heidi,sophia]).

semantics_object(O,[O]) :- game(O). % chess, hide_and_seek, soccer
semantics_object(football,[soccer]).
semantics_object(O,[O]) :- toy(O).  % puzzle, ball, doll
semantics_object(jigsaw,[puzzle]).
semantics_object(game,[chess,hide_and_seek,soccer]).
semantics_object(toy,[puzzle,ball,doll]).
semantics_object(something,[chess,hide_and_seek,soccer,puzzle,ball,doll]).

semantics_place(X,[X]) :- place(X).
semantics_place(inside,[bathroom,bedroom]).
semantics_place(outside,[street,playground]).

semantics_losers(NP2,Sem0,Sem0) :-
        var(NP2), !.
semantics_losers(NP2,Sem0,Sem2) :-
        semantics_subject(NP2,S),
        dss_semantics_event(lose,S,[],Sem1),
        Sem2 = and(Sem0,Sem1).

semantics_winners(NP2,Sem0,Sem0) :-
        var(NP2), !.
semantics_winners(NP2,Sem0,Sem2) :-
        semantics_subject(NP2,S),
        dss_semantics_event(win,S,[],Sem1),
        Sem2 = and(Sem0,Sem1).
