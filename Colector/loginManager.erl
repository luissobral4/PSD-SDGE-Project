-module(loginManager).
-export([run/0, create_account/2, close_account/2, login/3, logout/1]).

run() -> 
	register(?MODULE, spawn ( fun() -> loginManager(#{}) end) ).

create_account(User, Pass) ->
	?MODULE ! {create_account, User, Pass, self()},
	receiveReply().

close_account(User, Pass) ->
	?MODULE ! {close_account, User, Pass, self()},
	receiveReply().

login(User, Pass, Type) ->
	?MODULE ! {login, User, Pass,Type, self()},
	receiveReply().

logout(User) ->
	?MODULE ! {logout, User, self()},
	receiveReply().

loginManager(Map) ->
  	receive
    	{create_account, User, Pass, From} ->
      		case maps:find(User, Map) of
        		error ->
          			From ! {?MODULE, created},
					io:format("New user ~p\n", [User]),
          			loginManager(maps:put(User, {Pass, false}, Map));
        		_ -> 
          			From ! {?MODULE, user_exists},
          			loginManager(Map)
      		end;

    	{close_account, User, Pass, From} ->
      		case maps:find(User, Map) of
        		{ok, {Pass, _}} ->
          			From ! {?MODULE, ok},
          			loginManager(maps:remove(User, Map));
        		_ -> 
          			From ! {?MODULE, invalid},
          			loginManager(Map)
      		end;

    	{login, User, Pass, Type,From} ->
      		case maps:find(User, Map) of
				% user e pass certos
        		{ok, {Pass, false}} ->
          			From ! {?MODULE, logged, User,Type},
          			loginManager(maps:update(User, {Pass, From}, Map));
				% Pass errada
				{ok, {_, false}} ->
					From ! {?MODULE,invalid},
					loginManager(Map);

				% User já loggado
				{ok, {_,_}}->
					From ! {?MODULE,alreadyOnline},
					loginManager(Map);
				% Não existe user -> cria um novo
        		_ -> 
					io:format("New user ~p\n", [User]),
          			From ! {?MODULE, logged,User,Type},
					loginManager(maps:put(User, {Pass, From}, Map))
					%From ! {?MODULE, invalid},
          			%loginManager(Map)
      		end;

    	{logout, User, From} -> 
			case maps:find(User,Map) of
				{ok, {Pass,From}} ->
					loginManager(maps:update(User, {Pass, false}, Map)),
					From ! {?MODULE, ok};
				_ -> 
					io:format("Logout com user errado\n"),
					loginManager(Map)
			end
    end.

receiveReply() ->
	receive
		{?MODULE, Reply} -> 
			Reply
	end.