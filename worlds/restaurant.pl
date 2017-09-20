%%
% restaurant.pl
%
% Copyright 2016 Noorje Venhuizen <njvenhuizen@gmail.com> 
%                Harm Brouwer <me@hbrouwer.eu>
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
%%                             E V E N T S                               %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

event(enter(Person,Place)) :-
        person(Person),
        place(Place).

event(ask_menu(Person)) :-
        person(Person).

event(order(Person,Order)) :-
        person(Person),
        food(Order).
event(order(Person,Order)) :-
        person(Person),
        drink(Order).

event(eat(Person,Food)) :-
        person(Person),
        food(Food).

event(drink(Person,Drink)) :-
        person(Person),
        drink(Drink).

event(pay(Person)) :-
        person(Person).

event(leave(Person)) :-
        person(Person).

%% persons

person('beth').
person('dave').
person('thom').

%% places

place('cinema').
place('restaurant').

%% orders

food('dinner').
food('popcorn').

drink('water').
drink('cola').
drink('champagne').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                         V I O L A T I O N S                           %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%% Leaving and Entering %%%%%%%%

%% A person can only enter a single place.
violation(SoA) :-
        member((enter(Person,Place1),1),SoA),
        member((enter(Person,Place2),1),SoA),
        Place1 \= Place2.

%%%%%%%% Ordering %%%%%%%%

%% A person can only order a single type of food
violation(SoA) :-
        member((order(Person,Food1),1),SoA),
        member((order(Person,Food2),1),SoA),
        food(Food1),
        food(Food2),
        Food1 \= Food2.

%% If a person eats something, he must have ordered it.
violation(SoA) :-
        member((eat(Person,S),1),SoA),
        memberchk((order(Person,S),0),SoA).

%% If a person drinks something, he must have ordered it.
violation(SoA) :-
        member((drink(Person,D),1),SoA),
        memberchk((order(Person,D),0),SoA).

%% If has entered somewhere and pays, he must have ordered something.
violation(SoA) :-
        member((pay(Person),1),SoA),
        memberchk((enter(Person,_),1),SoA),
        findall(Order,member((order(Person,Order),0),SoA),Orders),
        findall(O,event(order(Person,O)),POrders),
        length(POrders,PO),
        length(Orders,PO).

%% If a person has seen the menu and pays, he must have ordered something.
violation(SoA) :-
        member((pay(Person),1),SoA),
        memberchk((ask_menu(Person),1),SoA),
        findall(Order,member((order(Person,Order),0),SoA),Orders),
        findall(O,event(order(Person,O)),POrders),
        length(POrders,PO),
        length(Orders,PO).

%%%%%%%% Paying %%%%%%%%

%% If a person leaves and has ordered something, he must have paid.
violation(SoA) :-
        member((leave(Person),1),SoA),  
        memberchk((order(Person,_),1),SoA),
        memberchk((pay(Person),0),SoA).

%%%%%%%% General %%%%%%%%

violation(SoA) :-
        \+ memberchk((_,1),SoA).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                      P R O B A B I L I T I E S                        %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

max_probability(1.0).

%%%%%%%% Base probabilities %%%%%%%%

base_probability(order(_,'water'),0.7).

base_probability(_,0.4).

%%%%%%%% General %%%%%%%%

%probability(_,_,0.5). % <-- coin flip

probability(enter(Person,Place),SoA,Pr) :-
        base_probability(enter(Person,Place),Bpr),
        probability_enter(Person,Place,SoA,Bpr,Pr).
probability(ask_menu(Person),SoA,Pr) :-
        base_probability(ask_menu(Person),Bpr),
        probability_menu(Person,SoA,Bpr,Pr).
probability(order(Person,Order),SoA,Pr) :-
        base_probability(order(Person,Order),Bpr),
        probability_order(Person,Order,SoA,Bpr,Pr).
probability(eat(Person,Food),SoA,Pr) :-
        base_probability(eat(Person,Food),Bpr),
        probability_eat(Person,Food,SoA,Bpr,Pr).
probability(drink(Person,Drink),SoA,Pr) :-
        base_probability(drink(Person,Drink),Bpr),
        probability_drink(Person,Drink,SoA,Bpr,Pr).
probability(pay(Person),SoA,Pr) :-
        base_probability(pay(Person),Bpr),
        probability_pay(Person,SoA,Bpr,Pr).
probability(leave(Person),SoA,Pr) :-
        base_probability(leave(Person),Bpr),
        probability_leave(Person,SoA,Bpr,Pr).

probability(E,_,Pr) :-
        base_probability(E,Pr).

%%%%%%%% Probability calculations %%%%%%%%

%% Probability of person being in a restaurant
%% * increases if he/she ordered dinner
%% * decreases if he/she ordered popcorn
probability_enter(Person,'restaurant',SoA,Ppr,Pr) :-
        member((order(Person,Order),1),SoA),
        (  Order == 'dinner'
        -> Epr is Ppr * 10.0
        %;  (  ( Order == 'popcorn' | Order == 'champagne' )
        ;  ( Order == 'popcorn'
           -> Epr is Ppr / 10.0
           ;  Epr is Ppr
           )
        ),
        max_probability(Mpr),
        Pr is min(Epr,Mpr).

%% Probability of person being in a cinema
%% * increases if he/she ordered popcorn
%% * decreases if he/she ordered dinner
probability_enter(Person,'cinema',SoA,Ppr,Pr) :-
        member((order(Person,Order),1),SoA),
        (  Order == 'popcorn' 
        -> Epr is Ppr * 10.0
        ;  ( Order == 'dinner'
           -> Epr is Ppr / 10.0
           ;  Epr is Ppr
           )
        ),
        max_probability(Mpr),
        Pr is min(Epr,Mpr).

%% Probability of person entering (general)
probability_enter(_,_,_,Ppr,Ppr).

%% Probability of person ordering dinner
%% * increases if person is in restaurant
%% * decreases if person is in cinema
probability_order(Person,'dinner',SoA,Ppr,Pr) :-
        member((enter(Person,Place),1),SoA),
        (  Place == 'restaurant'
        -> Epr is Ppr * 10.0
        ; (  Place == 'cinema'
          -> Epr is Ppr / 10.0
          ;  Epr is Ppr
          )
        ),
        max_probability(Mpr),
        Pr is min(Epr,Mpr).

%% Probability of person ordering popcorn
%% * increases if person is in cinema
%% * decreases if person is in restaurant
probability_order(Person,'popcorn',SoA,Ppr,Pr) :-
        member((enter(Person,Place),1),SoA),
        (  Place == 'cinema'
        -> Epr is Ppr * 10.0
        ;  (  Place == 'restaurant'
           -> Epr is Ppr / 10.0
           ;  Epr is Ppr
           )
        ),
        max_probability(Mpr),
        Pr is min(Epr,Mpr).

%% Probability of person ordering a drink
%% * decreases if person already ordered another drink
probability_order(Person,Drink,SoA,Ppr,Pr) :-
        drink(Drink),
        (  member((order(Person,Drink2),1),SoA),
           drink(Drink2),
           Drink \= Drink2
        -> Epr is Ppr / 10.0
        ;  Epr is Ppr
        ),
        max_probability(Mpr),
        Pr is min(Epr,Mpr).

%% Probability of person ordering (general)
probability_order(_,_,_,Ppr,Ppr).

%% Probability of person drinking (general)
probability_drink(_,_,_,Ppr,Ppr).

%% Probability of person asking for the menu (general)
probability_menu(_,_,Ppr,Ppr).

%% Probability of person eating (general)
probability_eat(_,_,_,Ppr,Ppr).

%% Probability of person paying (general)
probability_pay(_,_,Ppr,Ppr).

%% Probability of person leaving (general)
probability_leave(_,_,Ppr,Ppr).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                           S E N T E N C E S                           %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%sentence(Sem) --> [], { Sem = [] }.
sentence('train',Sem) --> structure('train',_,Sem).

%%%% Structures %%%% 

structure('train','freq0',Sem) --> sen('train',Sem,'freq0').
structure('train','freq1',Sem) --> sen('train',Sem,'freq1').
structure('train','freq2',Sem) --> sen('train',Sem,'freq2').

%%%% Frequencies %%%%
% f0:f1:f2 = 1:5:9  (for balancing: 2x5 = 1+9)

sen('train',Sem,'freq0') --> sen('train',Sem,'f0',_).

sen('train',Sem,'freq1') --> sen('train',Sem,'f1',_).
sen('train',Sem,'freq1') --> sen('train',Sem,'f1',_).
sen('train',Sem,'freq1') --> sen('train',Sem,'f1',_).
sen('train',Sem,'freq1') --> sen('train',Sem,'f1',_).
sen('train',Sem,'freq1') --> sen('train',Sem,'f1',_).

sen('train',Sem,'freq2') --> sen('train',Sem,'f2',_).
sen('train',Sem,'freq2') --> sen('train',Sem,'f2',_).
sen('train',Sem,'freq2') --> sen('train',Sem,'f2',_).
sen('train',Sem,'freq2') --> sen('train',Sem,'f2',_).
sen('train',Sem,'freq2') --> sen('train',Sem,'f2',_).
sen('train',Sem,'freq2') --> sen('train',Sem,'f2',_).
sen('train',Sem,'freq2') --> sen('train',Sem,'f2',_).
sen('train',Sem,'freq2') --> sen('train',Sem,'f2',_).
sen('train',Sem,'freq2') --> sen('train',Sem,'f2',_).

%%%% Constituents %%%%

nm(beth) --> ['beth'].
nm(dave) --> ['dave'].
nm(thom) --> ['thom'].
%nm(someone) --> ['someone'].

np(place,cinema)     --> ['the','cinema'].
np(place,restaurant) --> ['the','restaurant'].

np(food,dinner)      --> ['dinner'].
np(food,popcorn)     --> ['popcorn'].

np(drink,champagne)  --> ['champagne'].
np(drink,cola)       --> ['cola'].
np(drink,water)      --> ['water'].

%%%% Sentences %%%%

% Contrast 1
sen('train',Sem,'f2','C1')  --> nm(N), vp(order,N,O,Sem),
        { O == 'dinner' }.
sen('train',Sem,'f0','C1')  --> nm(N), vp(order,N,O,Sem),
        { O == 'popcorn' }.

% Contrast 1 - Fillers
sen('train',Sem,'f2','FIL') --> nm(N), vp(eat,N,O,Sem),
        { O == 'popcorn' }.
sen('train',Sem,'f0','FIL') --> nm(N), vp(eat,N,O,Sem),
        { O == 'dinner' }.

% Contrast 2a
sen('train',Sem,'f0','C2a')  --> nm(N), vp(enter,N,'cinema',Sem0), ['and'], vp(order,N,O,Sem1),
        { food(O), Sem = and(Sem0,Sem1) }.

% Contrast 2b
sen('train',Sem,'f0','C2b')  --> nm(N), vp(enter,N,'restaurant',Sem0), ['and'], vp(order,N,O,Sem1),
        { food(O), Sem = and(Sem0,Sem1) }.

% Contrast 3
sen('train',Sem,'f0','C3') --> nm(N), vp(ask_menu,N,_,Sem0), ['and'], vp(leave,N,_,Sem1),
        { Sem = and(Sem0,Sem1) }.
sen('train',Sem,'f0','C3') --> nm(N), vp(pay,N,_,Sem0), ['and'], vp(leave,N,_,Sem1),
        { Sem = and(Sem0,Sem1) }.

% Contrast 4
sen('train',Sem,'f2','C4')  --> nm(N), vp(order,N,O,Sem),
        { O == 'champagne' }.
sen('train',Sem,'f1','C4')  --> nm(N), vp(order,N,O,Sem),
        { O == 'cola' }.
sen('train',Sem,'f0','C4')  --> nm(N), vp(order,N,O,Sem),
        { O == 'water' }.

% Contrast 4 - Fillers
sen('train',Sem,'f2','FIL') --> nm(N), vp(drink,N,O,Sem),
        { O == 'water' }.
sen('train',Sem,'f1','FIL') --> nm(N), vp(drink,N,O,Sem),
        { O == 'cola' }.
sen('train',Sem,'f0','FIL') --> nm(N), vp(drink,N,O,Sem),
        { O == 'champagne' }.

% Contrast 5
sen('train',Sem,'f0','C5')  --> nm(N), vp(enter,N,_,Sem0), ['and'], vp(ask_menu,N,_,Sem1),
        { Sem = and(Sem0,Sem1) }.

% Contrast 5 - Fillers (Balancing out 'asked' vs 'ordered')
%sen('train',Sem,'f2','FIL') --> nm(N), vp(ask_menu,N,_,Sem).
%sen('train',Sem,'f2','FIL') --> nm(N), vp(ask_menu,N,_,Sem).
%sen('train',Sem,'f2','FIL') --> nm(N), vp(ask_menu,N,_,Sem).
%sen('train',Sem,'f2','FIL') --> nm(N), vp(ask_menu,N,_,Sem).
%sen('train',Sem,'f1','FIL') --> nm(N), vp(ask_menu,N,_,Sem).

% Other Fillers

sen('train',Sem,'f0','FIL') --> nm(N), vp(ask_menu,N,_,Sem).
sen('train',Sem,'f0','FIL') --> nm(N), vp(pay,N,_,Sem).
sen('train',Sem,'f0','FIL') --> nm(N), vp(leave,N,_,Sem).

sen('train',Sem,'f0','FIL') --> nm(N), vp(enter,N,_,Sem0), ['and'], vp(order,N,O,Sem1),
        { drink(O), Sem = and(Sem0,Sem1) }.
sen('train',Sem,'f0','FIL') --> nm(N), vp(enter,N,_,Sem0), ['and'], vp(leave,N,_,Sem1),
        { Sem = and(Sem0,Sem1) }.
sen('train',Sem,'f0','FIL') --> nm(N), vp(ask_menu,N,_,Sem0), ['and'], vp(order,N,_,Sem1),
        { Sem = and(Sem0,Sem1) }.
sen('train',Sem,'f0','FIL') --> nm(N), vp(pay,N,_,Sem0), ['and'], vp(order,N,_,Sem1),
        { Sem = and(Sem0,Sem1) }.

%%%% Main Clause VPs %%%%

vp(enter,N,P,Sem)    --> ['entered'], np(place,P),
        {semantics_event(enter,[N],[P],Sem) }.

vp(ask_menu,N,_,Sem) --> ['asked','for','the','menu'],
        { semantics_event(ask_menu,[N],[],Sem) }.

vp(order,N,D,Sem)    --> ['ordered'], np(drink,D),
        {semantics_event(order,[N],[D],Sem) }.

vp(order,N,F,Sem)    --> ['ordered'], np(food,F),
        {semantics_event(order,[N],[F],Sem) }.

vp(eat,N,F,Sem)    --> ['ate'], np(food,F),
        {semantics_event(eat,[N],[F],Sem) }.

vp(drink,N,D,Sem)    --> ['drank'], np(drink,D),
        {semantics_event(drink,[N],[D],Sem) }.

vp(pay,N,_,Sem)    --> ['paid'],
        {semantics_event(pay,[N],[],Sem) }.

vp(leave,N,_,Sem)    --> ['left'],
        {semantics_event(leave,[N],[],Sem) }.

%%%% Semantics %%%%

% Adapted from dss_semantics_event/4
semantics_event(Pred,[S],Os,Sem0) :-
        !, semantics_event_(Pred,S,Os,Sem0).
semantics_event(Pred,[S|Ss],Os,or(Event,Sem1)) :-
        Event =.. [Pred,S|Os],
        semantics_event(Pred,Ss,Os,Sem1).
