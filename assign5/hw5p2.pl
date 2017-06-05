/* Exercise 2 */
%a
rdup([H|L],M) :- member(H,M), rdup(L,M)
rdup([H|L],M) :- \+ member(H,M), append(M,H,M), rdup(L,M)

%So: If member, rdup. If not, append it them rdup.

%b
flat([H|L],F) :- flat(H,F), flat(L,F)
flat(L,F) :- append(F,H,F)

%Append if no other list, else move in to the list.
%Hopefully doing this right: First case is list, second is not. If list, just
%Take off the head, otherwise append it. Then hopefully recursion goes next head

%c
project(X,[H,Y],L) :- suball(X),project([X,Y,L)
project([X,T],[H,Y],L) :- x=0,append(L,H,L),project(T,Y,L)
suball([H|T]) :- H-1,suball(T)

%So if it only increases, I could probably just recurse through the thing once
%and if the amount of recursions equal the head of X, append it to
%Okay so: Keep subtracting 1 from the list until the head is 0, then remove that
%head. Repeat until it's over.
