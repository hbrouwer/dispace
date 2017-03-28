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

:- use_module(library(sgml)).
:- use_module(library(xpath)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%            P A R S I N G   D E S C R I P T   D A T A              %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% ds_load_gold_set(+XMLFile,-GoldSet)

ds_load_gold_set(XMLFile,gold_set(ID,Paraphrases)) :-
        file_name_to_id(XMLFile,ID),
        load_xml(XMLFile,DOM,[]),
        parse_gold_set(DOM,Paraphrases).

% ds_load_scenario(+File,-Scenario)

ds_load_scenario(XMLFile,scenario(ID,ESDs)) :-
        file_name_to_id(XMLFile,ID),
        load_xml(XMLFile,DOM,[]),
        parse_esds(DOM,ESDs).

% ds_load_directory(+Path,-Directory)

ds_load_directory(Path,directory(Path,Scenarios)) :-
        directory_files(Path,Files),
        xml_files_list(Files,Path,XMLFiles),
        findall(Scenario,(
                member(XMLFile,XMLFiles),
                ds_load_scenario(XMLFile,Scenario)),
                Scenarios).

% file_name_to_id(+FileName,-ID)

file_name_to_id(FileName,ID) :-
        downcase_atom(FileName,FileName0),
        file_name_extension(FileName0,'xml',FileName1),
        file_base_name(FileName1,ID).

%% xml_files_list(+Files,+Path,-XMLFiles)
%
%  Takes a list of Files and a Path, and returns absolute path to all
%  XMLFiles in Files.

xml_files_list([],_,[]) :- !.
xml_files_list([File0|Files0],Path,[File1|Files1]) :-
        downcase_atom(File0,File),
        file_name_extension(_,'xml',File), !,
        absolute_file_name(File0,File1,[relative_to(Path)]),
        xml_files_list(Files0,Path,Files1).
xml_files_list([_|Files0],Path,Files1) :-
        xml_files_list(Files0,Path,Files1).

%% parse_gold_set(+DOM,-Paraphrases)
%
%  Returns a list of paraphrases, with their frequency, average slot
%  position, average number of total slots, and the average fractional slot
%  position.

parse_gold_set(DOM,Paraphrases) :-
        findall(paraphrase(Event,Freq,AvgSlot,AvgTotalSlots,AvgFrac),(
                xpath(DOM,//label,element(_,LabelAttr,LabelDOM)),
                member((event=Event),LabelAttr),
                findall(item(Slot,TotalSlots,Frac),(
                        xpath(LabelDOM,//item,element(_,ItemAttr,_)),
                        member((slot=Slot0),ItemAttr),
                        member((total_slots=TotalSlots0),ItemAttr),
                        atom_number(Slot0,Slot),
                        atom_number(TotalSlots0,TotalSlots),
                        Frac is Slot / TotalSlots),
                        Items),
                length(Items,Freq),
                average_eds(Items,Freq,AvgSlot,AvgTotalSlots,AvgFrac)),
                Paraphrases).

% average_eds(+Items,+Freq,-AvgSlot,-AvgTotalSlots,-AvgFrac)

average_eds(Items,Freq,AvgSlot,AvgTotalSlots,AvgFrac) :-
        average_eds_(Items,Freq,0,AvgSlot,0,AvgTotalSlots,0,AvgFrac).

average_eds_([],_,AccS,AccS,AccTS,AccTS,AccFR,AccFR) :- !.
average_eds_([item(S,TS,FR)|Items],F,AccS,AvgS,AccTS,AvgTS,AccFR,AvgFR) :-
        AccS0  is AccS  + (1 / F) * S,
        AccTS0 is AccTS + (1 / F) * TS,
        AccFR0 is AccFR + (1 / F) * FR,
        write(AccS0), nl,
        average_eds_(Items,F,AccS0,AvgS,AccTS0,AvgTS,AccFR0,AvgFR).

%% parse_esds(+DOM,-ESDs)

parse_esds(DOM,ESDs) :-
        findall(esd(ID,Source,EDs),(
                xpath(DOM,//script,element(_,ScriptAttr,ScriptDOM)),
                member((id=ID),ScriptAttr),
                member((source=Source),ScriptAttr),
                findall(ed(Slot,ED),(
                        xpath(ScriptDOM,//item,element(_,ItemAttr,_)),
                        member((slot=Slot),ItemAttr),
                        member((original=RawED),ItemAttr),
                        ed_to_atom(RawED,ED)),
                        EDs)
                ), ESDs).

% ed_to_atom(+RawEd,-AtomED)

ed_to_atom(RawED,AtomED) :-
        atom_chars(RawED,RawChars),
        ed_to_atom_(RawChars,AtomChars),
        atom_chars(AtomED,AtomChars).

ed_to_atom_([],[]) :- !.
ed_to_atom_([C0|C0s],[C1|C1s]) :-
        char_type(C0,alnum), !,
        to_lower(C0,C1),
        ed_to_atom_(C0s,C1s).
ed_to_atom_([_|C0s],[C1|C1s]) :-
        char_code('_',C1),
        ed_to_atom_(C0s,C1s).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%              G O L D   E V E N T   S E Q U E N C E S              %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% ds_derive_gold_esds(+XMLFile,-GoldESDs)
%
%  For each paraphrase in the gold set, collect all EDs for that paraphrase.
%  Next, for each ED_x, check if ED_y that follows it in the original ESD is
%  part of the gold standard, and if so, derive an ESD between ED_x and ED_y.

ds_derive_gold_esds(XMLFile,gold_esds(ID,GoldESDs)) :-
        file_name_to_id(XMLFile,ID),
        load_xml(XMLFile,DOM,[]),
        derive_gold_esds(DOM,GoldESDs).

% derive_gold_esds(DOM,GoldESDs)

derive_gold_esds(DOM,GoldESDs) :-
        findall(seq(Event0,Event1),(
                xpath(DOM,//label,element(_,LabelAttr0,LabelDOM0)),
                member((event=Event0),LabelAttr0),
                findall(Event2,(
                        xpath(LabelDOM0,//item,element(_,ItemAttr,_)),
                        member((source=Source),ItemAttr),
                        member((script=Script),ItemAttr),
                        member((slot=Slot0),ItemAttr),
                        atom_number(Slot0,Slot),
                        NextSlot0 is Slot + 1,
                        atom_number(NextSlot,NextSlot0),
                        xpath(DOM,//label,element(_,LabelAttr1,LabelDOM1)),
                        xpath(LabelDOM1,//item(@source=Source,@script=Script,@slot=NextSlot),_),
                        member((event=Event2),LabelAttr1)),
                        Events2),
                member(Event1,Events2),
                Event1 \= Event0),
                GoldESDs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%                         R   E X P O R T                           %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% ds_export_to_r(+GoldESDs,+CSVFile)

ds_export_to_r(gold_esds(_,SEQs),CSVFile) :-
        open(CSVFile,'write',Stream),
        format(Stream,'from,to,weight\n',[]),
        list_to_ord_set(SEQs,SEQSet),
        findall(edge(From,To,Weight),(
                member(seq(From,To),SEQSet),
                edge_weight(seq(From,To),SEQs,Weight)),
                Edges),
        foreach(member(edge(From,To,Weight),Edges),
                format(Stream,'~a,~a,~d\n',[From,To,Weight])),
        close(Stream).

% edge_weight(+SEQ,+SEQSet,-Weight)

edge_weight(SEQ,SEQSet,Weight) :-
        edge_weight_(SEQ,SEQSet,0,Weight).

edge_weight_(_,[],Weight,Weight) :- !.
edge_weight_(SEQ,[SEQ|SEQs],WeightAcc,Weight) :-
        !, WeightAcc0 is WeightAcc + 1,
        edge_weight_(SEQ,SEQs,WeightAcc0,Weight).
edge_weight_(SEQ,[_|SEQs],WeightAcc,Weight) :-
        edge_weight_(SEQ,SEQs,WeightAcc,Weight).