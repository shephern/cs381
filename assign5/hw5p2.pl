/* Exercise 2 */
%a

rdup([],_).
%Base case, the left list is empty, so done

rdup([H|L],[H|M]) :- not(member(H,L)), rdup(L,M).
%Add to M if it's unique, then remove head

rdup([H|L],M) :- member(H,M), rdup(L,M).
%If it's not unique, then ignore it and go

%If just member, rdup. If not, append it then rdup.

%b
flat([],_).
%If left is empty, done.

flat([H|L],F) :- flat(H,F), flat(L,F).
%If the left variable is a list, recurse for both the left and the right.

flat(H,[H|_]).
%If the left variable isn't in a list, add it.

%Append if no other list, else move in to the list.
%Hopefully doing this right: First case is list, second is not. If list, just
%Take off the head, otherwise append it. Then hopefully recursion goes next head

%c
suball([H|T]) :- H-1,suball(T).

project(X,[_|Y],L) :- suball(X),project([X,Y,L]).
project([0|T],[H|Y],[H|L]) :- project(T,Y,L).



%Keep subtracting 1 from the list until the head is 0, then remove that
%head. Repeat until it's over.
