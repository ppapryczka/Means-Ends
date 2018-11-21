initList([on(b1, p1), on(b2, b1), on(b3, p2), on(b4, p4), clear(b2), clear(b3), clear(p3), clear(b4)]).
goalsList([on(b4, b3)]).

goals_achieved([],_).
goals_achieved(Goals,State) :-
    [G|Tail] = Goals,
    member(G,State),
    goals_achieved(Tail,State).

choose_goal(Goal, Goals, RestGoals, InitState) :-
    [Goal| Tail] = Goals,
    not(member(Goal, InitState)),
    RestGoals = Tail.
choose_goal(Goal, Goals, RestGoals, InitState) :-
    [X| Tail] = Goals,
    member(X, InitState),
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
    CondGoals = [clear(X/on(X, Y)],
    Conditions = [clear(Z), diff(Z, X/on(X, Y))].

inst_action(Action, Conditions, State1, InstAction) :-
    move(X, _, Z) = Action,
    nonvar(X),
    member(on(X, Y), State1),
    InstAction = move(X, Y, Z).
inst_action(Action, Conditions, State1, InstAction) :-
    move(X, Y, Z) = Action,
    var(X),
    

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
    conc (PrePlan, [ InstAction | PostPlan ], Plan).

/**
* Test comment.
*/
