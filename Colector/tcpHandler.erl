-module(tcpHandler).
-export([run/1]).

run(Port) -> 
	application:start(chumak),
	{ok,PushSocket} = chumak:socket(push),
	case chumak:connect(PushSocket,tcp,"localhost",Port+1) of
		{ok, _BindPid} ->
			io:format("Binding OK, with Pid ~p\n",[_BindPid]);
		{error, Reason}->
			io:format("Connection failed: ~p\n",[Reason]);
		X ->
			io:format("Undhandled reply for bind ~p\n",[X])
	end,

    {ok, LSock} = gen_tcp:listen(Port, [{active, once}, {packet, line},
                                      {reuseaddr, true}]),
	loginManager:run(),
	LoginManager = whereis(loginManager),
	io:format("pid: ~p\n",[LoginManager]),

	%acceptor(LSock,LoginManager,PushSocket).
	spawn(fun() -> acceptor(LSock,LoginManager, PushSocket) end),
  	receive
		  after infinity ->
			  ok
	end.

acceptor(LSock,LoginManager, PushSocket) ->
  {Result, Sock} = gen_tcp:accept(LSock),
  case Result of
	ok ->
		io:format("Sucesso: ~p\n",[Result]),
  		spawn(fun() -> acceptor(LSock,LoginManager, PushSocket) end),
  		userHandler(Sock,LoginManager, PushSocket);
	_ -> 
		io:format("Erro: ~p\n",[Result]),
		acceptor(LSock,LoginManager,PushSocket)
	end.
userHandler(Sock,LoginManager, PushSocket) ->
	receive
		{_, created} ->
			inet:setopts(Sock, [{active, once}]),
			%gen_tcp:send(Sock,"Conta criada com sucesso\n"),
			io:format("Conta criada com sucesso\n"),
			userHandler(Sock, LoginManager, PushSocket);
		{_,user_exists} -> 
			inet:setopts(Sock, [{active, once}]),
			%gen_tcp:send(Sock,"User já existe com esse username\n"),
			io:format("User já existe com esse username\n"),
			userHandler(Sock, LoginManager, PushSocket);
		{_ ,logged, User, Type} ->
			inet:setopts(Sock, [{active, once}]),
			gen_tcp:send(Sock,"Success\n"),
			io:format("Logged in\n"),
			ok = chumak:send(PushSocket,"login,"++User++","++Type),
			autenticado(Sock,LoginManager,User, PushSocket);
		{_ ,invalid} ->
			inet:setopts(Sock, [{active, once}]),
			io:format("Invalid\n"),
			userHandler(Sock, LoginManager, PushSocket);
		{_,alreadyOnline} ->
			inet:setopts(Sock, [{active, once}]),
			io:format("Already online\n"),
			userHandler(Sock, LoginManager, PushSocket);
		{tcp, _, "register " ++ Data} ->
			Args = string:tokens(Data, [$\s]),
			io:format("Register Args: ~p\n",[Args]),
			case mylength(Args) of 
				3 ->
					User = lists:nth(1,Args),
					Pass1 = lists:nth(2,Args),
					Pass2 = string:trim(lists:nth(3,Args)),
					case string:equal(Pass1,Pass2) of 
						true ->
							LoginManager ! {create_account,User, Pass1, self()};
						_ ->
							%gen_tcp:send(Sock,"Palavras pass não coincidem\n"),
							io:format("Palavras pass não coincidem\n")
					end,
					inet:setopts(Sock, [{active, once}]),
					userHandler(Sock,LoginManager, PushSocket);
				_ -> 
					inet:setopts(Sock, [{active, once}]),
					io:format("bad arguments\n"),
					
					userHandler(Sock,LoginManager, PushSocket)
			end;
		{tcp, _, "login " ++ Data} ->
			Args = string:tokens(Data, [$\s]),
			case mylength(Args) of 
				3 ->
					User = lists:nth(1,Args),
					Pass = lists:nth(2,Args),
					Type = string:trim(lists:nth(3,Args)),
					LoginManager ! {login, User, Pass, Type,self()},
					inet:setopts(Sock, [{active, once}]),
					userHandler(Sock,LoginManager, PushSocket);
				_ -> 
					inet:setopts(Sock, [{active, once}]),
					io:format("bad arguments\n"),
					userHandler(Sock,LoginManager, PushSocket)
			end;
		{tcp_closed, _} ->
			io:format("tcp closed\n");
		{tcp_error, _, _} ->
			io:format("tcp error\n")
  	end.

autenticado(Sock,LoginManager,User, PushSocket) ->
	receive
		{tcp,_,"event " ++ Data} ->
			inet:setopts(Sock, [{active, once}]),
			%gen_tcp:send(Sock,"Evento registado\n"),
			io:format("~p : Evento! ~p\n",[User,Data]),
			ok = chumak:send(PushSocket,"event," ++ User ++","++string:trim(Data)),
			autenticado(Sock,LoginManager,User, PushSocket);
		{tcp, _, "logout\n"} ->
			LoginManager ! {logout,User,self()},
			inet:setopts(Sock, [{active, once}]),
			ok = chumak:send(PushSocket,"logout,"++User),
			%gen_tcp:send(Sock,"Logged out...\n"),
			autenticado(Sock,LoginManager,User, PushSocket);
		{tcp_closed, _} ->
			io:format("tcp closed ... logging out (ativo)\n"),
			ok = chumak:send(PushSocket,"logout,"++User),
			LoginManager ! {logout, User,self()};
		{tcp_error, _, _} ->
			io:format("tcp error ... logging out (ativo)\n"),
			ok = chumak:send(PushSocket,"logout,"++User),
			LoginManager ! {logout, User,self()}
		after 5000 -> 
			io:format("Inativo ~p\n",[User]),
			ok = chumak:send(PushSocket,"inactive,"++User),
			inactive(Sock,LoginManager,User,PushSocket)
	end.

inactive(Sock,LoginManager,User, PushSocket) ->
	receive
		{tcp,_,"event " ++ Data} ->
			inet:setopts(Sock, [{active, once}]),
			%gen_tcp:send(Sock,"Evento registado\n"),
			io:format("~p : Evento! ~p\n",[User,Data]),
			ok = chumak:send(PushSocket,"event," ++ User ++","++string:trim(Data)),
			autenticado(Sock,LoginManager,User, PushSocket);
		{tcp, _, "logout\n"} ->
			LoginManager ! {logout,User,self()},
			inet:setopts(Sock, [{active, once}]),
			ok = chumak:send(PushSocket,"logout,"++User),
			%gen_tcp:send(Sock,"Logged out...\n"),
			autenticado(Sock,LoginManager,User, PushSocket);
		{tcp_closed, _} ->
			io:format("tcp closed ... logging out (inativo\n"),
			ok = chumak:send(PushSocket,"logout,"++User),
			LoginManager ! {logout, User,self()};
		{tcp_error, _, _} ->
			io:format("tcp error ... logging out (inativo)\n"),
			ok = chumak:send(PushSocket,"logout,"++User),
			LoginManager ! {logout, User,self()}
	end.
	

mylength([]) -> 0;
mylength([_|T]) -> 1+mylength(T).
