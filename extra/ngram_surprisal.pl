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

:- consult('contrasts_rn.pl').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%                    N G R A M   S U R P R I S A L                  %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% write_surprisal_values(+NgramSize,+File)

write_surprisal_values(N,File) :-
        ngrams(N,train,Ngrams),
        findall((Set,Sen,Sem,Surprisal),(
          sentence(Set,Sem,Sen,[]),
          Set \= train,
          surprisal(Sen,Ngrams,Surprisal)),Quads),
        open(File,write,Stream),
        format(Stream,'Cond,Sen,Sem,N,Surprisal~n',[]),
        write_surprisal_values_(Quads,N,Stream),
        close(Stream).

write_surprisal_values_([],_,_).
write_surprisal_values_([(Cond,Sen,Sem,Surprisal)|Quads],N,Stream) :-
        write(Sen), nl,
        format(Stream,'\"~w\",',Cond),
        format(Stream,'\"',[]),
        gen_sets:format_name(Sen,Stream),
        format(Stream,'\",',[]),
        dss_semantics:dss_format_formula(Sem,FormattedSem),
        format(Stream,'\"~w\",',FormattedSem),
        format(Stream,'~d,',N),
        format(Stream,'~f~n',Surprisal),
        write_surprisal_values_(Quads,N,Stream).

% ngrams(+NgramSize,+Set,-Ngrams)

ngrams(N,Set,Ngrams) :-
        findall(SenNgrams,(sentence(Set,_,Sen,[]),
          ngrams_(N,Sen,SenNgrams)),AllSenNgrams),
        flatten_ngrams(AllSenNgrams,Ngrams).

ngrams_(N,Sen,[Sen]) :-
        length(Sen,N), !.
ngrams_(N,[Word|Words],[Prefix|Ngrams]) :-
        prefixN(N,[Word|Words],Prefix),
        ngrams_(N,Words,Ngrams).

% prefixN(+PrefixSize,+Sentence,-Prefix)

prefixN(0,_,[]) :- !.
prefixN(N,[Word|Words],[Word|Prefix]) :-
        N0 is N - 1,
        prefixN(N0,Words,Prefix).

% suffixN(+SuffixSize,+Sentence,-Suffix)

suffixN(N,Words,Suffix) :-
        reverse(Words,RevWords),
        prefixN(N,RevWords,Prefix),
        reverse(Prefix,Suffix).

% flatten_ngrams(+NgramsPerSentence,-Ngrams)

flatten_ngrams(AllSenNgrams,Ngrams) :-
        flatten_ngrams_(AllSenNgrams,[],Ngrams0),
        reverse(Ngrams0,Ngrams).

flatten_ngrams_([],Ngrams,Ngrams).
flatten_ngrams_([SenNgrams|AllSenNgrams],NgramsAcc,Ngrams) :-
        reverse(SenNgrams,RevSenNgrams),
        append(RevSenNgrams,NgramsAcc,NgramsAcc0),
        flatten_ngrams_(AllSenNgrams,NgramsAcc0,Ngrams).

% surprisal(+Sentence,+Ngrams,-Surprisal)

surprisal(Sen,Ngrams,Surprisal) :-
        select(Ngram,Ngrams,_), !,
        length(Ngram,N),
        suffixN(N,Sen,Suffix0),
        reverse(Suffix0,[_|RevSuffix]),
        reverse(RevSuffix,Suffix1),
        ngram_frequency(Suffix0,Ngrams,Freq0),
        ngram_frequency(Suffix1,Ngrams,Freq1),
        Surprisal is log(Freq1) - log(Freq0).

% ngram_frequency(+Ngram,+Ngrams,-Frequency)

ngram_frequency(Ngram,Ngrams,Freq) :-
        ngram_frequency_(Ngram,Ngrams,0,Freq).

ngram_frequency_(_,[],Freq,Freq) :- !.
ngram_frequency_(Ngram,[Ngram0|Ngrams],FreqAcc,Freq) :-
        prefix(Ngram,Ngram0), !,
        FreqAcc0 is FreqAcc + 1,
        ngram_frequency_(Ngram,Ngrams,FreqAcc0,Freq).
ngram_frequency_(Ngram,[_|Ngrams],FreqAcc,Freq) :-
        ngram_frequency_(Ngram,Ngrams,FreqAcc,Freq).