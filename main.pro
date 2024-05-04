% serdar bahar
% 2022400264
% compiling: yes
% complete: yes


:- ['cmpefarm.pro'].
:- init_from_map.


% helpers

make_dict([], SingleDict, _, SingleDict).
make_dict([Head|Tail], StartDict, N, SingleDict) :-
	put_dict(N, StartDict, Head, Dict),
	N1 is N+1,
	make_dict(Tail, Dict, N1, SingleDict).

	
chicken(Agent) :- Agent.subtype = chicken.
cow(Agent) :- Agent.subtype = cow.
wolf(Agent) :- Agent.subtype = wolf.

grass(Object) :- Object.subtype = grass.
grain(Object) :- Object.subtype = grain.
corn(Object) :- Object.subtype = corn.

move_offset(move_up, 0, -1).
move_offset(move_down, 0, 1).
move_offset(move_left, -1, 0).
move_offset(move_right, 1, 0).
move_offset(move_up_right, 1, -1).
move_offset(move_up_left, -1, -1).
move_offset(move_down_right, 1, 1).
move_offset(move_down_left, -1, 1).

get_first_element([Agents|_], Agents).

my_length([], 0).
my_length([_| Tail], L) :-
    my_length(Tail, L1),
    L is L1+1.


del_from_list([], _, []).
del_from_list([Head|Tail], Del, [Head|Tail2]) :- 
	\+ Head = Del,
	del_from_list(Tail, Del, Tail2).

del_from_list([Head|Tail], Del, Tail2) :-
	Head = Del,
	del_from_list(Tail, Del, Tail2).


my_append([],List_2,List_2).
my_append([Head|List_1], List_2, [Head|List_3]) :- my_append(List_1, List_2, List_3).

contains(Element, [Element|_]).
contains(Element, [_|Tail]) :- contains(Element,Tail).


try_depth(X, DepthLimit) :- 
    try_depth_helper(X, 1, DepthLimit).

try_depth_helper(X, X, _).
try_depth_helper(X, CurrDepth, DepthLimit) :-
    CurrDepth < DepthLimit,
    NextDepth is CurrDepth + 1,
    try_depth_helper(X, NextDepth, DepthLimit).


% 1- agents_distance(+Agent1, +Agent2, -Distance)
agents_distance(Agent1, Agent2, Distance) :-
	Distance is abs(Agent1.x - Agent2.x)+abs(Agent1.y - Agent2.y).



% 2- number_of_agents(+State, -NumberOfAgents)
number_of_agents([Agents|_], NumberOfAgents) :- 
	dict_pairs(Agents, _, Pairs),
	number_of_agents_helper(Pairs, NumberOfAgents).

number_of_agents_helper([], 0).
number_of_agents_helper([_|Tail], NumberOfAgents) :-
	number_of_agents_helper(Tail, N),
	NumberOfAgents is N+1. 

number_of_objects([_, Objects|_], NumberOfObjects) :-
        dict_pairs(Objects, _, Pairs),
        number_of_objects_helper(Pairs, NumberOfObjects).

number_of_objects_helper([], 0).
number_of_objects_helper([_|Tail], NumberOfObjects) :-
        number_of_objects_helper(Tail, N),             
        NumberOfObjects is N+1.
	


% 3- value_of_farm(+State, -Value)
value_of_farm([Agents,Objects|_], Value) :- 
	dict_pairs(Agents,_,AgentPairs),
	value_of_agents(AgentPairs, V1),
	
	dict_pairs(Objects,_,ObjectPairs),
	value_of_objects(ObjectPairs,V2),
	Value is V1+V2.
 
value_of_agents([], 0).




value_of_agents([Head|Tail], Value) :-
	Head = _-Agent,
	value_of_agents(Tail, V1),
	( Agent.subtype = wolf ->
		V is 0
	;	value(Agent.subtype,V)
	),
	Value is V1+V.

value_of_objects([], 0). 
value_of_objects([Head|Tail], Value) :-
    Head = _-Object,
    value_of_objects(Tail, V1),
	value(Object.subtype,V),
    Value is V1+V.



% 4- find_food_coordinates(+State, +AgentId, -Coordinates)
	
find_food_coordinates([Agents,Objects|_], AgentId, Coordinates) :-
	number_of_agents([Agents|_], NumOfAgents),
	number_of_objects([_,Objects|_], NumOfObjects),
	(Agents.AgentId.subtype = wolf ->
		find_food_coordinates_wolf(0, Agents, NumOfAgents, Coordinates)
	;	find_food_coordinates_herb(0, Objects, Agents.AgentId, NumOfObjects, Coordinates)
	).

% for wolves
find_food_coordinates_wolf(1,_,0,[]).

find_food_coordinates_wolf(X, Agents, NumOfAgents, Tail) :-
    N1 is NumOfAgents-1,
	Idx is NumOfAgents-1,
	Idx >= 0,
	wolf(Agents.Idx),
	find_food_coordinates_wolf(X, Agents, N1, Tail).

find_food_coordinates_wolf(_, Agents, NumOfAgents, [ [X,Y] | Tail ]) :-
    N1 is NumOfAgents-1,
	Idx is NumOfAgents-1,
	Idx >= 0,
	(chicken(Agents.Idx) ; cow(Agents.Idx)),
	find_food_coordinates_wolf(1, Agents, N1, Tail),
	X is Agents.Idx.x,
	Y is Agents.Idx.y.


% for cows and chicken
find_food_coordinates_herb(1,_,_,0,[]).

find_food_coordinates_herb(X, Objects, Agent, NumOfObjects, Tail) :-
	N1 is NumOfObjects-1,
	Idx is NumOfObjects-1,
	Idx >= 0,
	\+ (can_eat(Agent.subtype, Objects.Idx.subtype)),
	find_food_coordinates_herb(X, Objects, Agent, N1, Tail).

find_food_coordinates_herb(_, Objects, Agent, NumOfObjects, [[X,Y] | Tail ]) :-
	N1 is NumOfObjects-1,
	Idx is NumOfObjects-1,
	Idx >= 0,
	can_eat(Agent.subtype, Objects.Idx.subtype),
	find_food_coordinates_herb(1, Objects, Agent, N1, Tail),
	X is Objects.Idx.x,
	Y is Objects.Idx.y.

% 5- find_nearest_agent(+State, +AgentId, -Coordinates, -NearestAgent)

find_nearest_agent([Agents|_], AgentId, [X,Y], NearestAgent) :-
	number_of_agents([Agents|_], NumberOfAgents),
	find_nearest_agent_helper(Agents, AgentId, NumberOfAgents, NearestAgent),
	X is NearestAgent.x,
	Y is NearestAgent.y.

find_nearest_agent_helper(Agents,0,1,Agents.1).
find_nearest_agent_helper(Agents,_,1,Agents.0).

find_nearest_agent_helper(Agents, AgentId, N, NearestAgent) :-
	AgentId =:= N - 1,
	Idx is N-1,
	Idx >= 0,
	find_nearest_agent_helper(Agents, AgentId, Idx, NearestAgent).

find_nearest_agent_helper(Agents, AgentId, N, NearestAgent) :-
	Idx is N-1,
	Idx >= 0,
	find_nearest_agent_helper(Agents, AgentId, Idx, CurrNearest),
	agents_distance(CurrNearest, Agents.AgentId, CurrMinimum),
	agents_distance(Agents.Idx, Agents.AgentId, Curr),
	Curr =< CurrMinimum,
	NearestAgent = Agents.Idx.

find_nearest_agent_helper(Agents, AgentId, N, NearestAgent) :-
	Idx is N-1,
	Idx >= 0,
	find_nearest_agent_helper(Agents, AgentId, Idx, CurrNearest),
	agents_distance(CurrNearest, Agents.AgentId, CurrMinimum),
	agents_distance(Agents.Idx, Agents.AgentId, Curr),
	Curr > CurrMinimum,
	NearestAgent = CurrNearest.


% 6- find_nearest_food(+State, +AgentId, -Coordinates, -FoodType, -Distance)

find_nearest_food(State, AgentId, [X,Y], FoodType, Distance) :-	
	State = [Agents,Objects|_],
	(cow(Agents.AgentId) ; chicken(Agents.AgentId)),
	dict_pairs(Objects,_,ObjectPairs),
	find_some_food(Agents.AgentId, ObjectPairs, SomeFood),
	find_nearest_food_helper(Agents.AgentId, ObjectPairs, SomeFood, NearestFood),
	X = NearestFood.x,
	Y = NearestFood.y,
	FoodType = NearestFood.subtype,
	agents_distance(Agents.AgentId, NearestFood, Distance).

find_nearest_food([Agents|_], AgentId, [X,Y], FoodType, Distance) :-	
	wolf(Agents.AgentId),
	dict_pairs(Agents,_,AgentPairs),
	find_some_food(Agents.AgentId, AgentPairs, SomeFood),
	find_nearest_food_helper(Agents.AgentId, AgentPairs, SomeFood, NearestFood),
	X = NearestFood.x,
	Y = NearestFood.y,
	FoodType = NearestFood.subtype,
	agents_distance(Agents.AgentId, NearestFood, Distance).


% finds the first consumable food, searching from the last. goal fails if none exists.

find_some_food(Agent,[_-Obj],SomeFood) :-
	can_eat(Agent.subtype, Obj.subtype),
	SomeFood = Obj.

find_some_food(Agent, [_-Head|_], SomeFood) :-
	can_eat(Agent.subtype, Head.subtype),
	SomeFood = Head.

find_some_food(Agent, [_|Tail], SomeFood) :-
	find_some_food(Agent, Tail, SomeFood).

% helper for find_nearest_food/
find_nearest_food_helper(_, [], SomeFood, NearestFood) :-
	NearestFood = SomeFood.

find_nearest_food_helper(Agent, [_-Head|Tail], SomeFood, NearestFood) :-
	find_nearest_food_helper(Agent, Tail, SomeFood, CurrNearest),
	can_eat(Agent.subtype, Head.subtype),
	agents_distance(CurrNearest, Agent, CurrMinimum),
	agents_distance(Head, Agent, Curr),
	Curr =< CurrMinimum,
	NearestFood = Head.

find_nearest_food_helper(Agent, [_-Head|Tail], SomeFood, NearestFood) :-
	find_nearest_food_helper(Agent, Tail, SomeFood, CurrNearest),
	can_eat(Agent.subtype, Head.subtype),
	agents_distance(CurrNearest, Agent, CurrMinimum),
	agents_distance(Head, Agent, Curr),
	Curr > CurrMinimum,
	NearestFood = CurrNearest.

find_nearest_food_helper(Agent, [_-Head|Tail], SomeFood, NearestFood) :-
	\+ (can_eat(Agent.subtype, Head.subtype)),
	find_nearest_food_helper(Agent, Tail, SomeFood, NearestFood).



% 7- move_to_coordinate(+State, +AgentId, +X, +Y, -ActionList, +DepthLimit)

move_to_coordinate(State, AgentId, X, Y, ActionList, DepthLimit) :-
	get_first_element(State, Agents),
	move_to_coordinate_helper(State, AgentId, Agents.AgentId.x, Agents.AgentId.y, X, Y, DepthLimit, ActionList).

%helper for move_to_coordinate/
move_to_coordinate_helper(_, _, X, Y, X, Y, 0, []).

move_to_coordinate_helper(_, _, X, Y, X, Y, DepthLimit, []) :-
	DepthLimit > 0.

move_to_coordinate_helper(State, AgentId, CurrX, CurrY, X, Y, DepthLimit, [Head | Tail]) :-
	DepthLimit > 0,
	get_first_element(State, Agents),	
	can_move(Agents.AgentId.subtype, Action),
	move(State, AgentId, Action, NewState),
	move_offset(Action, OffsetX, OffsetY),
	NewX is CurrX + OffsetX,
	NewY is CurrY + OffsetY,
	NewDepth is DepthLimit - 1,
	Head = Action,
	move_to_coordinate_helper(NewState, AgentId, NewX, NewY, X, Y, NewDepth, Tail).

% 8- move_to_nearest_food(+State, +AgentId, -ActionList, +DepthLimit)

move_to_nearest_food(State, AgentId, ActionList, DepthLimit) :-
	find_nearest_food(State, AgentId, [X,Y], _, _),
	move_to_coordinate(State, AgentId, X, Y, ActionList, DepthLimit).


% 9- consume_all(+State, +AgentId, -NumberOfMoves, -Value, -NumberOfChildren, +DepthLimit)

consume_all(State, AgentId, NumberOfMoves, Value, NumberOfChildren, DepthLimit) :-
	make_foodlist_ready(State, AgentId, FoodList),
	consume_all_helper(0, 0, State, FoodList, AgentId, NumberOfMoves, NumberOfChildren, DepthLimit, NewState),
	value_of_farm(NewState, Value).


% helper for consume all. FoodList stores the available foods. Its head is used to reach, if failed, list is permuted so that
% the new head is the next nearest. T is the permutation counter. Z is a boolean var which indicates whether a food has been
% eaten by the agent. NewState is returned to calculate the value.
consume_all_helper(1, _, TempState, _, AgentId,0,0,_,NewState) :-
	TempState = [Agents,Objects,_,_],
	(wolf(Agents.AgentId) ->
		\+ (get_dict(_,Agents, Agent), Agent.type == herbivore)
	;
	\+ (get_dict(_,Objects, Object), can_eat(Agents.AgentId.subtype, Object.subtype))
	),
	NewState = TempState.
	

consume_all_helper(Z, T, State, FoodList, AgentId, NumberOfMoves, NumberOfChildren, DepthLimit, NewState) :-

	convert_list(State, AgentId, FoodList, NewFoodList),	

	NewFoodList = [[X,Y]|_],

	(try_depth(TryDepth, DepthLimit),move_to_coordinate(State, AgentId, X, Y, ActionList, TryDepth) ->

		execute_actions(ActionList, AgentId, State, Temp1State),

		eat(Temp1State, AgentId, Temp2State),

		del_from_list(NewFoodList, [X,Y], New1FoodList),

		consume_all_helper(1, 0, Temp2State, New1FoodList, AgentId, NumOfMoves1, NumOfChildren1, DepthLimit, NewState),

		number_of_agents(Temp1State, NumOfAgentsinTemp1),
		number_of_agents(Temp2State, NumOfAgentsinTemp2),
		(NumOfAgentsinTemp2 > NumOfAgentsinTemp1 -> 
			NumberOfChildren is NumOfChildren1 + 1
		;	NumberOfChildren = NumOfChildren1
		),

		NumberOfMoves is NumOfMoves1 + TryDepth
	; 	

		convert_list(State, AgentId, FoodList, New1FoodList),

		T1 is T + 1,
		my_length(New1FoodList, NumOfFoods),

		(T1 =< NumOfFoods ->
			consume_all_helper(Z, T1, State, New1FoodList, AgentId, NumberOfMoves, NumberOfChildren, DepthLimit, NewState)
		; 	
			Z = 1,
			NumberOfMoves = 0,
			NumberOfChildren = 0,
			NewState = State
		)
	).



convert_list(State, AgentId, [ Food | DeletedFoods], NewFoodList) :-

	find_nearest_food(State, AgentId, [XCurr, YCurr],_,_),
	
	Food = [XHead,YHead],
	( XCurr = XHead, YCurr = YHead ->
		NewFoodList = [Food | DeletedFoods],
		!
	;

	(DeletedFoods = [] ->
		NewFoodList = [Food],
		!
	
	; State = [Agents, Objects | _ ],

	(Agents.AgentId.type = herbivore ->
		findall(Object, 
				(get_dict(_, Objects, Object),
				Xhere is Object.x,
				Yhere is Object.y,
				contains([Xhere,Yhere], DeletedFoods)
				),DeletedFoodsDictList)
	;	findall(Agent,
				(get_dict(_, Agents, Agent),
				Xhere is Agent.x,
				Yhere is Agent.y,
				contains([Xhere,Yhere], DeletedFoods)
				),DeletedFoodsDictList)
	),

	make_dict(DeletedFoodsDictList, _{}, 0, DeletedFoodsDict),


	(Agents.AgentId.type = herbivore ->
		TempState = [Agents, DeletedFoodsDict, _, _]
	;	TempState = [DeletedFoodsDict, Objects, _, _]
	),


	find_nearest_food(TempState, AgentId, [X,Y], _, _),

	del_from_list(DeletedFoods, [X,Y], TempFoodList),

	Temp2FoodList = [[X,Y]|TempFoodList],


	my_append(Temp2FoodList, [Food], NewFoodList),
	!
	)).


make_foodlist_ready(State, AgentId, FoodList) :-


	State = [Agents, Objects | _ ],
	
	(Agents.AgentId.type = herbivore ->
		findall([X,Y], 
			(get_dict(_, Objects, Object),
			X = Object.x, Y = Object.y
			),FoodList)
	;	findall([X,Y], 
			(get_dict(_, Agents, Agent),
			X = Agent.x, Y = Agent.y
			),FoodList)
	).
	

execute_actions([], _, NewState, NewState).
execute_actions([HeadAction|Tail], AgentId, State, NewState) :-
	move(State, AgentId, HeadAction, TempState),
	execute_actions(Tail, AgentId, TempState, NewState).