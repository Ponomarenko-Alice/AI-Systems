witcher('Gerald').
witcher('Leto').

witcher_health('Gerald', 100).
witcher_health('Leto', 150).

witcher_bag('Gerald', [potion1, potion3, bomb2, bomb3]).
witcher_bag('Leto', [potion1, potion2, bomb1, bomb3]).

% Bestiary
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
creature_health('Alghoul', 360).
creature_health('Drowner', 400).

addition(potion1).
addition(potion2).
addition(potion3).
addition(bomb1).
addition(bomb2).
addition(bomb3).

% Potions that act against certain types of creatures.
% Increase the witchers' health
potion_against_creature(potion1, relict).
potion_against_creature(potion2, necrophage).
potion_against_creature(potion3, necrophage).

% Bombs that act against certain types of creatures.
% Decrease the creatures' health
bomb_against_creature(bomb1, relict).
bomb_against_creature(bomb2, relict).
bomb_against_creature(bomb3, necrophage).

% Сhecking that all Witchers died
failed_fight(Ws) :- (Ws == []).

% The main recursive predicate of fight between all witchers and creatures.
% The fight is finished if the list of creatures is empty.
fight(_, []).
fight(Witchers, Creatures) :-
    member(Witcher, Witchers),            
    member(Creature, Creatures),
    witcher_health(Witcher, WH),
    creature_health(Creature, CH),
	witcher_bag(Witcher, Bag),
    (fight_round(Witcher, Creature, WH, CH, 0, Bag) ->  
    excl(Creature, Creatures, NewCreatures),
        write(Witcher), write(WH), write(' wins '), write(Creature), write(CH), nl;
    excl(Witcher, Witchers, NewWitchers),
        write(Creature), write(CH), write(' wins '), write(Witcher), write(WH), nl
    ),
    not(failed_fight(NewWitchers)),
    fight(NewWitchers, NewCreatures). % Continue the fight with updated lists

% The recursive fight between certain witcher and creature.
% Returns true if the Witcher wins in this fight, otherwise returns false.
% In one fight, the Witcher can't use more than 3 additions due to high intoxication.
fight_round(Witcher, Creature, WH, CH, Intox, Bag) :-
    WH > CH, !; % If the witcher's health is higher, he wins
    Intox < 3,
    find_addition(Bag, Creature, Addition), 
    use_addition(Addition, WH, NewWH, CH, NewCH, Bag, NewBag), 
    NewIntox is Intox + 1, 
    fight_round(Witcher, Creature, NewWH, NewCH, NewIntox, NewBag). % Continue the fight with updated values

% If intoxication is greater than or equal to 3 and the creature's health is greater, the witcher loses
fight_round(_, _, _, CH, 3, _) :-
    CH > 0, !, fail.

% Finding the right addition for a fight with creature. 
% Certain additions are suitable for specific creatures.
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
