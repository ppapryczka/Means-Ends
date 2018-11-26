/* Lists for testing purpose: */
initList([on(b1, p1), on(b2, b1), on(b3, p2), on(b4, p4), clear(b2), clear(b3), clear(p3), clear(b4)]).
/*goalsList([clear(b1)]).*/ /* Case: one field clear. */
/*goalsList([on(b4, b3)]).*/ /* Case: one move to succeed. */
goalsList([on(b4, b1)]). /* Case: two moves to succeed. */
/*goalsList([on(b4, b3), clear(b2)]).*/ /* Case: one move for first goal, one for second. */
/*goalsList([on(b4, b3), clear(b3)]).*/ /* Case: second goal erases first one. */

goal_achieved(Goal,State) :-
    /* "on" condition has always two instantiated members.*/
    on(_, _) = Goal,
    member(Goal, State).
goal_achieved(Goal,State) :-
    clear(Element) = Goal,
    atom(Element),
    member(Goal, State).
goal_achieved(Goal,State) :-
    clear(Pair) = Goal,
    \+ atom(Pair),
    inst_pair(Pair, State, InstElem),
    member(clear(InstElem), State).

goals_achieved([],_).
goals_achieved(Goals,State) :-
    [G|Tail] = Goals,
    goal_achieved(G,State),
    goals_achieved(Tail,State).

choose_goal(Goal, Goals, RestGoals, InitState) :-
    [Goal| Tail] = Goals,
    not(goal_achieved(Goal, InitState)),
    RestGoals = Tail.
choose_goal(Goal, Goals, RestGoals, InitState) :-
    [X| Tail] = Goals,
    goal_achieved(X, InitState),
    choose_goal(Goal, Tail, TailRest, InitState),
    append([X], TailRest, RestGoals).

achieves(Goal, Action) :-
    on(X, Y) = Goal,
    Action = move(X, _, Y).
achieves(Goal, Action) :-
    clear(X) = Goal,
    Action = move(_, X, _).

requires(Action, CondGoals, Conditions) :-
    move(X, Y, Z) = Action,
    nonvar(X),
    CondGoals = [clear(X), clear(Z)],
    Conditions = [on(X, Y)].
requires(Action, CondGoals, Conditions) :-
    move(X, Y, Z) = Action,
    var(X),
    CondGoals = [clear(X/on(X, Y))],
    Conditions = [diff(X/on(X,Y), Z), clear(Z)].

/*inst_conditon(Condition, State) :-
    Condition = on(_, _),
    inst_on_condition(Condition, State).
inst_conditon(Condition, State) :-
    Condition = clear(_),
    inst_clear_condition(Condition, State).*/

inst_pair(Pair, State, InstElem) :-
    X/on(X, Y) = Pair,
    atom(Y),
    member(on(InstElem, Y), State).
inst_pair(Pair, State, InstElem) :-
    X/on(X, NextPair) = Pair,
    \+ atom(NextPair),
    inst_pair(NextPair, State, NextElem),
    member(on(InstElem, NextElem), State).

inst_diff_cond_first_arg(Condition, State, InstFirstArg) :-
    diff(X, _) = Condition,
    inst_pair(X, State, InstFirstArg).

find_different_clear(Clear1, Clear2, State) :-
    [X| _] = State,
    X = clear(Place),
    not(Place == Clear1),
    Clear2 = Place.
find_different_clear(Clear1, Clear2, State) :-
    [_| Tail] = State,
    find_different_clear(Clear1, Clear2, Tail).

inst_action(Action, Conditions, State1, InstAction) :-
    /*
     * If there is "on" condidion, move type is "move known element from unknown
     * field".
     */
    [on(A, B)] = Conditions,
    member(on(A, B), State1),
    move(X, _, Z) = Action,
    InstAction = move(X, B, Z).
inst_action(Action, Conditions, State1, InstAction) :-
    /*
     * If there are "diff" and clear conditions, move type is "move unknown
     * from known element.
     */
    move(_, From, _) = Action,
    [diff(A, B), clear(B)] = Conditions,
    inst_diff_cond_first_arg(diff(A,B), State1, Moved),
    /* Target have to be different than moved element and clear. */
    find_different_clear(Moved, Target, State1),
    InstAction = move(Moved, From, Target).

perform_action(State1, InstAction, State2) :-
    move(X, Y, Z) = InstAction,
    /* Moved element is still clear - do nothing with it */
    /* Source element should be cleared: */
    append(State1, [clear(Y)], StateA),
    delete(StateA, on(X, Y), StateB),
    /* Target shoud be "uncleared", and moved element should be on target: */
    delete(StateB, clear(Z), StateC),
    append(StateC, [on(X, Z)], State2).


plan(State, Goals, [  ], State) :-
    goals_achieved(Goals, State).

plan(InitState, Goals, Plan, FinalState) :-
    choose_goal(Goal, Goals, RestGoals, InitState),
    achieves(Goal, Action),
    requires(Action, CondGoals, Conditions),
    plan(InitState, CondGoals, PrePlan, State1),
    inst_action(Action, Conditions, State1, InstAction),
    perform_action(State1, InstAction, State2),
    plan(State2, RestGoals, PostPlan, FinalState),
    append(PrePlan, [ InstAction | PostPlan ], Plan).

run(Plan, FinalState) :-
    initList(InitState),
    goalsList(Goals),
    plan(InitState, Goals, Plan, FinalState).
