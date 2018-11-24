initList([on(b1, p1), on(b2, b1), on(b3, p2), on(b4, p4), clear(b2), clear(b3), clear(p3), clear(b4)]).
goalsList([on(b4, b3)]).

goal_achieved(Goal,State) :-
    nonvar(Goal),
    member(Goal,State).
goal_achieved(Goal,State) :-
    var(Goal),
    clear(X/on(X, Y)) = Goal,
    goal_achieved(Y, State).

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
    Conditions = on(X, Y).
requires(Action, CondGoals, Conditions) :-
    move(X, Y, Z) = Action,
    var(X),
    CondGoals = [clear(X/on(X, Y))],
    Conditions = [diff(X/on(X,Y), Z), clear(Z)].

inst_conditon(Condition, State) :-
    Condition = on(_, _),
    inst_on_condition(Condition, State).
inst_conditon(Condition, State) :-
    Condition = clear(_),
    inst_clear_condition(Condition, State).

inst_pair(Pair, State) :-
    X/on(X, Y) = Pair,
    nonvar(Y),
    member(on(X, Y), State),
    Pair = X/on(X, Y).
inst_pair(Pair, State) :-
    X/on(X, NextPair) = Pair,
    var(NextPair), /* as an assertion */
    inst_pair(NextPair, State),
    Z/_ = NextPair,
    member(on(X, Z), State),
    Pair = X/on(X, NextPair).

inst_clear_cond_first_arg(Condition, _) :-
    nonvar(Condition).
inst_diff_cond_first_arg(Condition, State) :-
    diff(X, Y) = Condition,
    inst_pair(X, State),
    Condition = diff(X, Y).

find_different_clear(Clear1, Clear2, State) :-
    [X| _] = State,
    X = clear(Place),
    not(Place == Clear1),
    Clear2 = Place.
find_different_clear(Clear1, Clear2, State) :-
    [_| Tail] = State,
    find_different_clear(Clear1, Clear2, Tail).

inst_action(Action, _, State1, InstAction) :-
    move(X, _, Z) = Action,
    nonvar(X),
    member(on(X, Y), State1),
    InstAction = move(X, Y, Z).
inst_action(Action, Conditions, State1, InstAction) :-
    move(X, Y, Z) = Action,
    var(X),
    /*
     * If action is in "move from" type (to make clear), first conditon is always
     * that moved element is different than its target position.
     * Hence we can find moved element (find instance).
     */
    [Cond|_] = Conditions,
    inst_diff_cond_first_arg(Cond, State1),
    diff(Pair, _) = Cond,
    X/_ = Pair,
    /* Choose Z different than X. */
    find_different_clear(X, Z, State1),
    InstAction = move(X, Y, Z).

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
