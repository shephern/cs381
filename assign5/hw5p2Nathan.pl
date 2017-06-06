%Part a

rdup([],_).
%Base case, the left list is empty, so done

rdup([H|L],[H|M]) :- not(member(H,M)), rdup(L,M).
%Add to M if it's unique, then remove head

rdup([H|L],M) :- member(H,M), rdup(L,M).
%If it's not unique, then ignore it and go

%Note: This code was mostly copied from
%https://stackoverflow.com/questions/2260792/remove-duplicates-in-list-prolog
%with some modification (Base case is different), at the suggestion of a TA.

%Part b

flat([],[]).
flat([H|L],[H|M]) :- atom(H), flat(L,M).
flat([I|_],M) :- flat(I,M).
%Adapted from stackoverflow, https://stackoverflow.com/questions/9059572/flatten-a-list-in-prolog

%Append if no other list, else move in to the list.
%Hopefully doing this right: First case is list, second is not. If list, just
%Take off the head, otherwise append it. Then hopefully recursion goes next head

%c
project(_,[],_).
%This one shouldn't come up often, only when the second list starts empty.
project([],_,_).
%Base cases, if either lists are empty, we're done.


project([H|X],[Y|T],L) :- project([H|X],[Y|T],L,[Y|T]).
%Runs a helper 4-var function, with the 4th variable keeping the original list.


project(_,[],_,_).
%This one is a bit longer because it only runs in the helper 4-var function.

project([1|X],[H|_],[H|L],N) :- project(X,N,L).
%Base case. If X is 1, append H to L, then go back to the original 3 var
%variant sans the head of the first list.

project([H|X],[_|Y],L,N) :- H1 is H - 1,
														project([H1|X],Y,L,N).
%This traverses the head of the first list, subtracting 1 all the way down.
