% Knowledge bases

witcher('Gerald').
witcher('Leto').

witcher_health('Gerald', 100).
witcher_health('Leto', 150).

witcher_bag('Gerald', [potion1, potion3, bomb2, bomb3]).
witcher_bag('Leto', [potion1, potion2, bomb1, bomb3]).

%bestiary
creature('Fiend').
creature('Leshen').
creature('Ghoul').
creature('Alghoul').
creature('Drowner').

creature_type('Fiend', relict).
creature_type('Leshen', relict).
creature_type('Ghoul', necrophage).
creature_type('Alghoul', necrophage).
creature_type('Drowner', necrophage).

creature_health('Fiend', 100).
creature_health('Leshen', 200).
creature_health('Ghoul', 350).
creature_health('Alghoul', 400).
creature_health('Drowner', 460).

addition(potion1).
addition(potion2).
addition(potion3).
addition(bomb1).
addition(bomb2).
addition(bomb3).

potion_against_creature(potion1, relict).
potion_against_creature(potion2, necrophage).
potion_against_creature(potion3, necrophage).

bomb_against_creature(bomb1, relict).
bomb_against_creature(bomb2, relict).
bomb_against_creature(bomb3, necrophage).

failed_fight(Ws, _) :- (Ws == []), !.

% Бой
fight(_, [], Solution). % Если список существ пуст, бой окончен
fight(Witchers, Creatures, Solution) :-
    member(Witcher, Witchers),            
    member(Creature, Creatures),
    witcher_health(Witcher, WH),
    creature_health(Creature, CH),
	witcher_bag(Witcher, Bag),
    (fight_round(Witcher, Creature, WH, CH, 0, Bag) ->  
    excl(Creature, Creatures, NewCreatures);
    excl(Witcher, Witchers, NewWitchers)
    ),
    not(failed_fight(NewWitchers, NewCreatures)),
    fight(NewWitchers, NewCreatures).  % Continue the fight with updated lists

% Основное рекурсивное правило боя
fight_round(Witcher, Creature, WH, CH, Intox, Bag) :-
    WH > CH, !; % Если здоровье ведьмака больше, он выигрывает
    Intox < 3, % Если интоксикация меньше 3, можно использовать добавку
    find_addition(Bag, Creature, Addition), % Находим подходящую добавку против существа
    use_addition(Addition, WH, NewWH, CH, NewCH, Bag, NewBag), % Применяем добавку и обновляем здоровье
    NewIntox is Intox + 1, % Увеличиваем интоксикацию
    fight_round(Witcher, Creature, NewWH, NewCH, NewIntox, NewBag). % Продолжаем бой с обновленными значениями

% Если интоксикация больше или равна 3 и здоровье существа больше, ведьмак проигрывает
fight_round(_, _, _, CH, 3, _) :-
    CH > 0, !, fail.

% Поиск подходящей добавки для существа
find_addition([Addition|_], Creature, Addition) :-
    creature_type(Creature, CType),
    (potion_against_creature(Addition, CType); bomb_against_creature(Addition, CType)), !.

find_addition([_|Rest], Creature, Addition) :-
    find_addition(Rest, Creature, Addition).


use_addition(potion1, WHealth, NewHealth, CHealth, CHealth, Bag, NewBag) :-
    NewHealth is WHealth + 100,
    excl(potion1, Bag, NewBag).

use_addition(potion2, WHealth, NewHealth, CHealth, CHealth, Bag, NewBag) :-
    NewHealth is WHealth * 1.5,
    excl(potion2, Bag, NewBag).

use_addition(potion3, WHealth, NewHealth, CHealth, CHealth, Bag, NewBag) :-
    NewHealth is WHealth + 150,
    excl(potion3, Bag, NewBag), !.

use_addition(bomb1, WHealth, WHealth, CHealth, NewCHealth, Bag, NewBag) :-
    NewCHealth is CHealth - 50,
    excl(bomb1, Bag, NewBag).

use_addition(bomb2, WHealth, WHealth, CHealth, NewCHealth, Bag, NewBag) :-
    NewCHealth is CHealth - 70,
    excl(bomb2, Bag, NewBag).

use_addition(bomb3, WHealth, WHealth, CHealth, NewCHealth, Bag, NewBag) :-
    NewCHealth is CHealth - 100,
    excl(bomb3, Bag, NewBag), !.

% Utils
excl(_, [], []).
excl(H, [H|T], T).
excl(X, [H|T], [H|TT]) :- excl(X, T, TT).
