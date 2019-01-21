/* Lists for testing purpose: */
initList([on(b1, p1), on(b2, b1), on(b3, p2), on(b4, p4), clear(b2), clear(b3), clear(p3), clear(b4)]).
%! goalsList([clear(b1)]). /* Case: one move to succed. */
%! goalsList([clear(p1)]). /* Case: two moves to succed. */
%! goalsList([on(b4, b3)]). /* Case: one move to succeed. */
 goalsList([on(b4, b1)]). /* Case: two moves to succeed. */
%! goalsList([on(b4, b3), clear(b2)]). /* Case: one move for first goal, second is alredy fulfilled. */
%goalsList([on(b4, b3), clear(b3)]). /* Case: second goal erases first one. */

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
goals_achieved([G|Tail],State) :-
    goal_achieved(G,State),
    goals_achieved(Tail,State).

choose_goal(Goal, Goals, RestGoals, InitState) :-
    delete_member(Goal, Goals, RestGoals),
    not(goal_achieved(Goal, InitState)).

achieves(on(X, Z), move(X, _, Z)).
achieves(clear(Y), move(_, Y, _)).

requires(move(X, Y, Z), [clear(X), clear(Z)], [on(X, Y)]) :-
    nonvar(X),
    !.
requires(move(X, Y, Z), [clear(X/on(X, Y))], [diff(X, Z), clear(Z)]).

inst_move(move(X, Y/_, Z), move(X, Y, Z)) :-
    !.
inst_move(Action, Action).

inst_action(Action, [on(X, Y)], State, Action) :-
    member(on(X, Y), State).
inst_action(Action, [diff(X, A), clear(A)], [clear(A)| _], InstAction) :-
    A \= X,
    inst_move(Action, InstAction).
inst_action(Action, [diff(X, Z), clear(Z)], [_| StateTail], InstAction) :-
    inst_action(Action, [diff(X, Z), clear(Z)], StateTail, InstAction).

perform_action(State1, move(X, Y, Z), [clear(Y)| [on(X, Z)| StateB]]) :-
    /* Moved element is still clear - do nothing with it */
    /* Source element should be cleared: */
    delete(State1, on(X, Y), StateA),
    /* Target shoud be "uncleared", and moved element should be on target: */
    delete(StateA, clear(Z), StateB).

set_preLimit(Max, Limit, _) :-
    Max =< Limit,
    write("Fail: out of limit.\n"),
    flush_output(),
    !, fail.
set_preLimit(_, Limit, Limit).
set_preLimit(Max, Limit, Result) :-
    NewLimit is Limit + 1,
    set_preLimit(Max, NewLimit, Result).

check_action(_, []).
check_action(move(_, _, Z), [clear(Z) | _]) :-
    !,
    write("Fail: ereasing previoulsy achieved goal.\n"),
    flush_output(),
   fail.
check_action(move(X, _, _), [on(X, _) | _]) :-
    !,
    write("Fail: ereasing previoulsy achieved goal.\n"),
    flush_output(),
    fail.
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
   X =< ListLen.
check_decision(c, _, c) :-
   !.
check_decision(_, ListLen, Dec) :-
   write("Wrong argument, try again.\n"),
   flush_output(),
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

plan(State, Goals, _,  _, [  ], State, Level) :-
    my_trace_rec(1, plan, 1, Level, ['State'/State, 'Goals'/Goals]),
    goals_achieved(Goals, State),
    my_trace_rec(4, plan, 1, Level, ['State'/State, 'Goals'/Goals]),
    !.
plan(_, _, _, 0, _, _, Level) :-
    my_trace_rec(1, plan, 1, Level, ['Limit'/0]),
    write("Fail: out of limit.\n"),
    flush_output(),
    !, fail.
plan(InitState, Goals, AchievedGoals, Limit, Plan, FinalState, Level) :-
    my_trace_rec(1, plan, 3, Level, ['InitState'/InitState, 'Goals'/Goals, 'AchievedGoals'/AchievedGoals, 'Limit'/Limit, 'Plan'/Plan, 'FinalState'/FinalState]),  
    NextLevel is Level + 1,

    my_trace_rec(2, plan, 3, Level, set_preLimit),
    set_preLimit(Limit, 0, PreLimit),
    my_trace_rec(3, plan, 3, Level, set_preLimit, ['PreLimit'/PreLimit]),

    my_trace_rec(2, plan, 3, Level, choose_goal),
    choose_goal(Goal, Goals, RestGoals, InitState),
    my_trace_rec(3, plan, 3, Level, choose_goal, ['Goal'/Goal, 'RestGoals'/RestGoals]),

    my_trace_rec(2, plan, 3, Level, achieves),
    achieves(Goal, Action),
    my_trace_rec(3, plan, 3, Level, achieves, ['Action'/Action]),

    my_trace_rec(2, plan, 3, Level, requires),
    requires(Action, CondGoals, Conditions),
    my_trace_rec(3, plan, 3, Level, requires, ['CondGoals'/CondGoals, 'Conditions'/Conditions]),

    my_trace_rec(2, plan, 3, Level, plan),
    plan(InitState, CondGoals, AchievedGoals, PreLimit, PrePlan, State1, NextLevel),
    my_trace_rec(3, plan, 3, Level, plan, ['State1'/State1, 'PrePlan'/PrePlan]),

    my_trace_rec(2, plan, 3, Level, possible_action),
    possible_action(Action, Conditions, State1, ActionList),
    my_trace_rec(3, plan, 3, Level, possible_action, ['ActionLinst'/ActionList]),

    my_trace_rec(2, plan, 3, Level, choose_action),
    choose_action(State1, ActionList, InstAction),
    my_trace_rec(3, plan, 3, Level, choose_action, ['InstAction'/InstAction]),

    check_action(InstAction, AchievedGoals),

    my_trace_rec(2, plan, 3, Level, perform_action),
    perform_action(State1, InstAction, State2),
    my_trace_rec(3, plan, 3, Level, perform_action, ['State'/State2]),

    my_trace_rec(2, plan, 3, Level, add_goal_achieved),
    add_goal_achieved(Goal, AchievedGoals, AchievedGoals1),
    my_trace_rec(3, plan, 3, Level, add_goal_achieved, ['AchievedGoals1'/AchievedGoals1]),

    PostLimit is Limit - PreLimit - 1,

    my_trace_rec(2, plan, 3, Level, plan),
    plan(State2, RestGoals, AchievedGoals1, PostLimit, PostPlan, FinalState, NextLevel),
    my_trace_rec(3, plan, 3, Level, plan, ['PostPlan'/PostPlan, 'FinalState'/FinalState]),

    my_trace_rec(2, plan, 3, Level, append),
    append(PrePlan, [ InstAction | PostPlan ], Plan),
    my_trace_rec(3, plan, 3, Level, append, ['Plan'/Plan]),

    my_trace_rec(4, plan, 3, Level, ['InitState'/InitState, 'Goals'/Goals, 'AchievedGoals'/AchievedGoals, 'Limit'/Limit, 'Plan'/Plan, 'FinalState'/FinalState]).

plan_wraper(InitState, Goals, Limit, MaxLimit, Plan, FinalState) :-
    my_trace_rec(1, plan_wraper, 1, 0, ['InitState'/InitState, 'Goals'/Goals, 'Limit'/Limit, 'MaxLimit'/MaxLimit, 'Plan'/Plan, 'FinalState'/FinalState]),  
    set_preLimit(MaxLimit, Limit, GivenLimit),
    plan(InitState, Goals, [], GivenLimit, Plan, FinalState, 1).

/* Function exists only for easier test running. */
run(Limit, MaxLimit, Plan, FinalState) :-
    initList(InitState),
    goalsList(Goals),
    plan_wraper(InitState, Goals, Limit, MaxLimit, Plan, FinalState).


% procedure my_trace_rec

% w celu wyprowadzenia wartosci zmiennych na wejeciu do procedury
my_trace_rec(1,ProcName, Clause,Level, ArgList) :-
	nl, nl, nl, write(ProcName),
	write('   poziom   '), write(Level),
	write('   klauzula   '), write(Clause),
	write('   wejscie'),
	write_args(ArgList), nl.%, read(_).
% w celu wyprowadzenia komunikatu o wywołaniu innej procedury
my_trace_rec(2,ProcName, Clause, Level, ProcName2) :-
	nl, write(ProcName),
	write('   poziom   '), write(Level),
	write('   klauzula   '), write(Clause),
	nl,write('wywołanie   '), write(ProcName2), nl.
% w celu wyprowadzenia wartosci zmiennych po powrocie z innej procedury
my_trace_rec(3,ProcName, Clause, Level,ProcName2, ArgList) :-
	nl, write(ProcName),
	write('   poziom   '), write(Level),
	write('   klauzula   '), write(Clause),
	nl,write('po wykonaniu   '), write(ProcName2),
	write_args(ArgList), nl.%, read(_).
% w celu wyprowadzenia wartosci zmiennych na zakończenie
% wykonania procedury na danym poziomie rekurencji
my_trace_rec(4,ProcName, Clause,Level, ArgList) :-
	nl, write(ProcName),
	write('   poziom   '), write(Level),
	write('   klauzula   '), write(Clause),
	nl,write('KONIEC WYKONANIA NA POZIOMIE  '),write(Level),
	write_args(ArgList),
	end_trace(Level, ProcName).

end_trace(Level,ProcName)  :-
	Level<2,
	nl,  nl,  write('KONIEC SLEDZENIA  '), write(ProcName), nl, nl.
end_trace(Level,_)  :-
	Level >= 1,
	nl.%, read(_).


write_args([]).
write_args([First|Rest]) :-
	write_one_arg(First),
	write_args(Rest).
write_one_arg(Name/Val)  :-
	nl, write(Name), write('='), write(Val).


% end my_trace_rec
