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
flat([I|L],M) :- flat(I,M).
%Adapted from stackoverflow, https://stackoverflow.com/questions/9059572/flatten-a-list-in-prolog

%Append if no other list, else move in to the list.
%Hopefully doing this right: First case is list, second is not. If list, just
%Take off the head, otherwise append it. Then hopefully recursion goes next head

%c
project(_,[],_).
project([],_,_).
%Base cases, if either lists are empty, we're done.

project([0|_],[H|_],[H|_]).
%Another base case. If X is 0, add the current head of Y to L.

project([X|T],[_|Y],L) :- X1 is X-1,
		      X1 >= 0,
		      project([X1|T],Y,L).
%This basically just traverses Y X times. It'll trigger the second base case if
%it can't be done, and third if it can.

project([H|X],Y,L) :- H1 is H - 1, 
		      project([H1|X],Y,L), 
		      project(X,Y,L).
%The head of X gets subtracted by one and sent in to itself to use
%the above 2 clauses, then it goes to the next head on the tail.
%It will trigger the first base case.

