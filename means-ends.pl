plan (State, Goals, [  ], State) :-
    goals_achieved (Goals, State).
    
plan (InitState, Goals, Plan, FinalState) :-
    choose_goal (Goal, Goals, RestGoals, InitState),
    achieves ( Goal, Action),
    requires (Action, CondGoals, Conditions),
    plan (InitState, CondGoals, PrePlan, State1),
    inst_action(Action, Conditions, State1, InstAction),
    perform_action (State1, InstAction, State2),
    plan (State2, RestGoals, PostPlan, FinalState),
    conc (PrePlan, [ InstAction | PostPlan ], Plan).
