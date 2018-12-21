/* Lists for testing purpose: */
initList([on(b1, p1), on(b2, b1), on(b3, p2), on(b4, p4), clear(b2), clear(b3), clear(p3), clear(b4)]).
%! goalsList([clear(b1)]). /* Case: one move to succed. */
 goalsList([clear(p1)]). /* Case: two moves to succed. */
%! goalsList([on(b4, b3)]). /* Case: one move to succeed. */
%! goalsList([on(b4, b1)]). /* Case: two moves to succeed. */
%! goalsList([on(b4, b3), clear(b2)]). /* Case: one move for first goal, second is alredy fulfilled. */
%! goalsList([on(b4, b3), clear(b3)]). /* Case: second goal erases first one. */

%! engine start

delete_member(X, [X|R], R).
delete_member(X, [Y|R], [Y|R1]) :-
    delete_member(X, R, R1).

%! engine end

goal_achieved(clear(X/Y),State) :-
    !, goal_achieved(Y, State),
    member(clear(X), State).
goal_achieved(on(X, Y/Z),State) :-
    !, goal_achieved(Z, State),
    member(on(X,Y), State).
goal_achieved(Goal,State) :-
    member(Goal, State).

goals_achieved([],_).
goals_achieved(Goals,State) :-
    [G|Tail] = Goals,
    goal_achieved(G,State),
    goals_achieved(Tail,State).

choose_goal(Goal, Goals, RestGoals, InitState) :-
    delete_member(Goal, Goals, RestGoals),
    not(goal_achieved(Goal, InitState)).

achieves(on(X, Z), move(X, _, Z)).
achieves(clear(Y), move(_, Y, _)).

requires(move(X, Y, Z), CondGoals, Conditions) :-
    nonvar(X),
    !,
    CondGoals = [clear(X), clear(Z)],
    Conditions = [on(X, Y)].
requires(move(X, Y, Z), CondGoals, Conditions) :-
    CondGoals = [clear(X/on(X, Y))],
    Conditions = [diff(X, Z), clear(Z)].

inst_move(move(X, Y/_, Z), move(X, Y, Z)) :-
    !.
inst_move(Action, Action).

inst_action(Action, [on(X, Y)], State, InstAction) :-
    member(on(X, Y), State),
    InstAction = Action.
inst_action(Action, [diff(X, Z), clear(Z)], [clear(A)| _], InstAction) :-
    A \= X,
    Z = A,
    inst_move(Action, InstAction).
inst_action(Action, [diff(X, Z), clear(Z)], [_| StateTail], InstAction) :-
    inst_action(Action, [diff(X, Z), clear(Z)], StateTail, InstAction).

perform_action(State1, InstAction, State2) :-
    move(X, Y, Z) = InstAction,
    /* Moved element is still clear - do nothing with it */
    /* Source element should be cleared: */
    delete(State1, on(X, Y), StateA),
    /* Target shoud be "uncleared", and moved element should be on target: */
    delete(StateA, clear(Z), StateB),
    [clear(Y)| [on(X, Z)| StateB]] = State2.

set_preLimit(Max, Limit, _) :-
    Max =< Limit,
    !, fail.
set_preLimit(_, Limit, Limit).
set_preLimit(Max, Limit, Result) :-
    NewLimit is Limit + 1,
    set_preLimit(Max, NewLimit, Result).

check_action(_, []).
check_action(move(_, _, Z), [clear(Z) | _]) :-
    !, fail.
check_action(move(X, _, _), [on(X, _) | _]) :-
    !, fail.
check_action(Action, [_ | RestGoals]) :-
    check_action(Action, RestGoals).

add_goal_achieved(on(X, Y), AchievedSoFar, [on(X, Y) | AchievedSoFar]).
add_goal_achieved(clear(X/_), AchievedSoFar, [clear(X) | AchievedSoFar]) :-
    !.
add_goal_achieved(clear(X), AchievedSoFar, [clear(X) | AchievedSoFar]).

possible_action(Action, Conditions, State, ActionList) :-
    findall(InstAction, inst_action(Action, Conditions, State, InstAction), ActionList).

check_decision(X, ListLen, X) :-
   number(X),
   X =< ListLen,
   !.
check_decision(c, _, c) :-
   !.
check_decision(_, ListLen, Dec) :-
   write("Wrong argument, try again."),
   read_decision(Dec,ListLen).

read_decision(X, ListLen) :-
    read(Val),
    check_decision(Val, ListLen, X).

make_decision(c, _, _) :-
    !, fail.
make_decision(Num, ActionList, Action) :-
    nth1(Num, ActionList, Action).

choose_action(State, ActionList, InstAction) :-
    write("State: "),
    write(State), nl,
    write("ActionList: "),
    write(ActionList), nl,
    length(ActionList, ListLen),
    read_decision(Decision, ListLen),
    make_decision(Decision, ActionList, InstAction).

plan(State, Goals, _,  _, [  ], State) :-
    goals_achieved(Goals, State),
    !.
plan(_, _, _, 0, _, _) :-
    !, fail.
plan(InitState, Goals, AchievedGoals, Limit, Plan, FinalState) :-
    set_preLimit(Limit, 0, PreLimit),
    choose_goal(Goal, Goals, RestGoals, InitState),
    achieves(Goal, Action),
    requires(Action, CondGoals, Conditions),
    plan(InitState, CondGoals, AchievedGoals, PreLimit, PrePlan, State1),
    possible_action(Action, Conditions, State1, ActionList),
    choose_action(State1, ActionList, InstAction),
    check_action(InstAction, AchievedGoals),
    perform_action(State1, InstAction, State2),
    add_goal_achieved(Goal, AchievedGoals, AchievedGoals1),
    PostLimit is Limit - PreLimit - 1,
    plan(State2, RestGoals, AchievedGoals1, PostLimit, PostPlan, FinalState),
    append(PrePlan, [ InstAction | PostPlan ], Plan).

/* Function exists only for easier test running. */
run(Limit,Plan, FinalState) :-
    initList(InitState),
    goalsList(Goals),
    plan(InitState, Goals, [], Limit, Plan, FinalState).
