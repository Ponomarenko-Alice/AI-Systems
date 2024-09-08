% Knowledge bases

witcher('Gerald').
witcher('Leto').

:- dynamic witcher_health/2.
witcher_health('Gerald', 100).
witcher_health('Leto', 150).

change_witcher_health(V, NewH) :-
    retract(witcher_health(V, _)),
    assertz(witcher_health(V, NewH)),
    write(V), write("'s health changed to "), write(NewH), nl.

change_witcher_force(V, NewF) :-
    retract(witcher_force(V, _)),
    assertz(witcher_force(V, NewF)),
    write(V), write("'s force changed to "), write(NewF), nl.

change_creature_health(C, NewH) :-
    retract(creature_health(C, _)),
    assertz(creature_health(C, NewH)),
    write(C), write("'s health changed to "), write(NewH), nl.

witcher_bag('Gerald', ['potion1', 'potion3', 'oil2', 'bomb2', 'bomb3']).
witcher_bag('Leto', ['potion1', 'potion2', 'oil1', 'bomb1', 'bomb2']).

try_to_use(W, X) :-
    witcher_bag(W, A),
    member(X, A), !,
    excl(X, A, _),
    use_addition(X, W),
    write(W), write(" uses "), write(X), nl.

%bestiary
creature('a').
creature('b').
creature('c').
creature('d').
creature('e').

creature_type('a', a1).
creature_type(b, a1).
creature_type(c, a2).
creature_type(d, a2).
creature_type(e, a2).

:- dynamic creature_health/2.
creature_health('a', 300).
creature_health('b', 200).
creature_health('c', 250).
creature_health('d', 400).
creature_health('e', 250).

:- dynamic witcher_force/2.
witcher_force('Gerald', 50).
witcher_force('Leto', 40).

creature_force(a, 40).
creature_force(b, 35).
creature_force(c, 45).
creature_force(d, 50).
creature_force(e, 55).

potion_against_creature(potion1, a1).
potion_against_creature(potion2, a2).
potion_against_creature(potion3, a2).

oil_against_creature(oil1, a1).
oil_against_creature(oil2, a2).

bomb_against_creature(bomb1, a1).
bomb_against_creature(bomb2, a2).
bomb_against_creature(bomb3, a2).

addition('potion1').
addition('potion2').
addition('potion3').
addition('oil1').
addition('oil2').
addition('bomb1').
addition('bomb2').
addition('bomb3').

use_addition(potion1, W) :-
    witcher_health(W, X),
    NewH is X + 100,
    change_witcher_health(W, NewH).

use_addition(potion2, W) :-
    witcher_health(W, X),
    NewH is X * 1.5,
    change_witcher_health(W, NewH).

use_addition(potion3, W) :-
    witcher_health(W, X),
    NewH is X + 150,
    change_witcher_health(W, NewH).

use_addition(oil1, W) :-
    witcher_force(W, X),
    NewF is X + 20,
    change_witcher_force(W, NewF).

use_addition(oil2, W) :-
    witcher_force(W, X),
    NewF is X + 100,
    change_witcher_force(W, NewF).

use_addition(bomb1, C) :-
    creature_health(C, X),
    NewH is X - 50,
    change_creature_health(C, NewH).

use_addition(bomb2, C) :-
    creature_health(C, X),
    NewH is X - 70,
    change_creature_health(C, NewH).

use_addition(bomb3, C) :-
    creature_health(C, X),
    NewH is X - 100,
    change_creature_health(C, NewH).

fight(_, []). % условие завершения ( _ - наличие ведьмаков, [] - отсутствие монстров)
fight(W, C) :-
    member(X, W), % выбираем ведьмака, который есть в живых
    member(Y, C), % выбираем существо, которое есть в живых
    choose_addition_to_fight_for_witcher(X), % ведьмак выбирает добавку
    witcher_vs_creature(X, Y, W, C, WW, CC), % бой между ведьмаком и существом
    fight(WW, CC). % продолжаем бой с обновленными списками

witcher_vs_creature(X, Y, W, C, WW, CC) :-
    witcher_health(X, WX),
    creature_health(Y, WY),
    (
      WX >= WY -> excl(Y, C, CC), WW = W,
      write(X), write(" wins "), write(Y), nl; % Если ведьмак побеждает, исключаем существо
      excl(X, W, WW), CC = C, % Если существо побеждает, исключаем ведьмака
      write(Y), write(" wins "), write(X), nl
    ).

choose_addition_to_fight_for_witcher(W) :-
    addition(X),
    try_to_use(W, X).

% Удаление элемента из списка
excl(_, [], []).
excl(H, [H|T], T).
excl(X, [H|T], [H|TT]) :- excl(X, T, TT).
