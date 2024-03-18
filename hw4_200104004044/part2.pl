%===========================
% Atakan Kurt - 200104004044
% HW4: Part-2
%===========================

% Decision tree rules
classify(_, X2, _, _, 'Iris-setosa') :- X2 =< 1.9, !.

classify(_, X2, X3, _, 'Iris-versicolor') :- 
    X2 > 1.9,
    X3 =< 1.5,
    X2 =< 4.9,
    !.

classify(X1, X2, X3, _, 'Iris-versicolor') :- 
    X2 > 1.9,
    X3 > 1.5,
    X2 =< 5.0,
    X1 > 2.8,
    !.

classify(_, X2, X3, _, 'Iris-virginica') :- 
    X2 > 1.9,
    X3 =< 1.5,
    X2 > 4.9,
    !.

classify(X1, X2, X3, _, 'Iris-virginica') :- 
    X2 > 1.9,
    X3 > 1.5,
    X2 =< 5.0,
    X1 =< 2.8,
    !.

classify(_, X2, X3, _, 'Iris-virginica') :- 
    X2 > 1.9,
    X3 > 1.5,
    X2 > 5.0,
    !.

classify(X1, X2, X3, _) :-
    classify(X1, X2, X3, _, Class),
    write(Class), nl, !.