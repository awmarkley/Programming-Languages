% Boniface Sindala
% Andrew Markley
% CS401
% Assignment 4

-module(t3).

-export([first/2, next/2, check/1, checkvalid/4, checkop/1, checkname/2]).
-export([checkturn/1, turn/0, endgame/1, tell/1, messageTwoOne/1, messageOneTwo/1]).
-export([newgame/1, readyPlayerTwo/1, player/1, oppstart/1, playwith/1, randomFirst/0]).
-export([newboard/3, placetoken/1, printboard/3, printboard2/1, whoseturn/3]).

% (DO THIS FIRST) Player 2 begins waiting for connection from Player 1
readyPlayerTwo(Name) ->
	io:fwrite(Name),
	io:fwrite(" is waiting on player...\n"),
	register(readyPlayerTwo, spawn(t3, oppstart, [Name])).

% (DO THIS NEXT) Player 1 establishes connection with Player 2 and begins the game.
newgame(Player2) ->
	io:fwrite("Starting a new game!\n"),
	io:fwrite("Connecting to ~w...~n", [Player2]),
	Player1 = spawn(t3, player, [Player2]),
	{readyPlayerTwo, Player2} ! {connect, Player1},
	register(first, spawn(t3, first, [Player1, Player2])),
	register(checkop, spawn(t3, checkop, [Player2])),
	register(messageOneTwo, spawn(t3, messageOneTwo, [Player2])),
  playwith(Player2).

% Message handling between players.
player(Player2) ->
	receive
		connect ->
      io:fwrite("Successfully connected~n"),
      player(Player2);
		{x, Player1} ->
			io:fwrite("Your move: \n   Use t3:placetoken() to make a move\n   Valid moves are from a1 to c3\n"),
			{readyPlayerTwo, Player2} ! {playerfirst, Player1, Player2},
			player(Player2);
		turn ->
      io:fwrite("Your move: \n   Use t3:placetoken() to make a move\n   Valid moves are from a1 to c3\n"),
      {checkturn, Player2} ! player,
      player(Player2);
		{board, Board, Name, Kind, _} ->
      io:fwrite("~w (~w) has completed their turn.", [Name, Kind]),
			printboard2(Board),
      player(Player2);
		{endgame, Winner, Marker} ->
      io:fwrite("~w (~w) has won the game!~n", [Winner, Marker]),
      player(Player2);
		{endgame, draw} ->
      io:fwrite("Nobody won the game! You should try again!"),
      player(Player2);
		{messageTwoOne, Message} ->
      messageOneTwo ! {messageTwoOne, Message},
      player(Player2);
		notvalid ->
      messageOneTwo ! notvalid,
      player(Player2)
	end.

% Begin game with Player 2
playwith(Opponent) ->
	checkop ! {checkop, self()},
	receive
		undefined ->
      io:fwrite("~w is not ready or does not exist.", [Opponent]);
		Player2 ->
      case checkname(Opponent, Player2) of
		      undefined ->
            io:fwrite("~w is not ready or does not exist.", [Opponent]);
					defined ->
            io:fwrite("Game with ~w has begun!~n", [Opponent]),
            spawn(t3, randomFirst, [])
				end
	end.

%Attempt to place a token - only works if it is the player's turn.
placetoken(Coordinate) ->
  case turn() of
    yes ->
	     case whereis(readyPlayerTwo) of
		       undefined ->
            case Coordinate of
				         a1 -> checkop ! a1;
				         a2 -> checkop ! a2;
				         a3 -> checkop ! a3;
				         b1 -> checkop ! b1;
				         b2 -> checkop ! b2;
				         b3 -> checkop ! b3;
				         c1 -> checkop ! c1;
			           c2 -> checkop ! c2;
				         c3 -> checkop ! c3;
		             _ -> io:fwrite("Nope move~n")
		        end;

			      _ -> case Coordinate of
				         a1 -> who ! a1;
				         a2 -> who ! a2;
				         a3 -> who ! a3;
			           b1 -> who ! b1;
				         b2 -> who ! b2;
			           b3 -> who ! b3;
				         c1 -> who ! c1;
				         c2 -> who ! c2;
				         c3 -> who ! c3;
				         _ -> io:fwrite("Invalid move~n")
			      end
	      end;
    no -> io:fwrite("You must wait for your turn.~n")
end.

%Whose turn is it?
checkturn(Turn) ->
	receive
		player -> checkturn(player);
		readyPlayerTwo -> checkturn(readyPlayerTwo);
		{checkturn, Player1} -> Player1 ! Turn, checkturn(Turn)
	end.

%Is it my turn?
turn() ->
	case whereis(checkturn) of
		undefined -> checkop ! {checkturn, self()},
				receive
					player -> yes;
					_ -> no
				end;
		_ -> checkturn ! {checkturn, self()},
				receive
					player -> no;
					_ -> yes
				end
	end.

%Is player 2 ready? Handles player moves
checkop(Player2) ->
	receive
		{checkop, Player1} ->
      case rpc:call(Player2, erlang, whereis, [readyPlayerTwo]) of
						undefined ->
              Player1 ! undefined,
              checkop(Player2);
						_ ->
              Player1 ! Player2,
              checkop(Player2)
					end;
		{checkturn, Player1} ->
      {checkturn, Player2} ! {checkturn, Player1},
      checkop(Player2);
		Coordinate ->
      {who, Player2} ! Coordinate,
      checkop(Player2)
	end.

%Verifies validity of name provided for Player 2
checkname(Name, Player2) ->
	{readyPlayerTwo, Player2} ! {checkname, Name, self()},
	receive
		undefined -> undefined;
		defined -> defined
	end.

%Notifies player to take first turn
first(Player1, Player2) ->
	receive
		player1 -> Player1 ! {x, Player1};
		player2 -> {readyPlayerTwo, Player2} ! {x, Player1, Player2}
	end.

%Randomly determines player to take first move
randomFirst() ->
	random:seed(now()),
	case random:uniform(2) of
		1 ->  first ! player1;
		2 ->  first ! player2
	end.

%Communication handler; registers necessary processes to listen for messages from other nodes
oppstart(Name) ->
	receive
		{checkname, Name2, Player1} ->
      case Name2 of
				Name ->
          io:fwrite("Player challenges ~w~n", [Name]),
					Player1 ! defined, oppstart(Name);
				_ -> Player1 ! undefined, oppstart(Name)
			end;
		{connect, Playerid} ->
      Playerid ! connect, oppstart(Name);
		{x, Player1, Player2} ->
			io:fwrite("~w's turn: \n   Use t3:placetoken() to make a move\n   Valid moves are from a1 to c3", [Name]),
			Board = {'_', '_', '_',
				       '_', '_', '_',
				       '_', '_', '_'},
			register(board, spawn(t3, newboard, [Board, Player1, Player2])),
			register(next, spawn(t3, next, [Player1, Player2])),
			register(who, spawn(t3, whoseturn, [Name, x, Name])),
			register(checkturn, spawn(t3, checkturn, [readyPlayerTwo])),
			register(endgame, spawn(t3, endgame, [Player1])),
			register(messageTwoOne, spawn(t3, messageTwoOne, [Player1])),
			oppstart(Name);
		{playerfirst, Player1, Player2} ->
			Board = {'_', '_', '_',
				       '_', '_', '_',
				       '_', '_', '_'},
			register(board, spawn(t3, newboard, [Board, Player1, Player2])),
			register(next, spawn(t3, next, [Player1, Player2])),
			register(who, spawn(t3, whoseturn, [player, x, Name])),
			register(checkturn, spawn(t3, checkturn, [player])),
			register(endgame, spawn(t3, endgame, [Player1])),
			register(messageTwoOne, spawn(t3, messageTwoOne, [Player1])),
			oppstart(Name);
		turn ->
			io:fwrite(Name),
			io:fwrite("'s turn: \n"), checkturn ! readyPlayerTwo, oppstart(Name)
	end.

%Send a message to other node
tell(Message) ->
	case whereis(messageOneTwo) of
		undefined ->
      messageTwoOne ! {message, Message};
		_ ->
      messageOneTwo ! {message, Message}
	end.

%Message from Player 2 to Player 1
messageTwoOne(Player1) ->
	receive
		{messageOneTwo, Playermessid, Message} ->
      io:fwrite("Message from Player 1: "),
      io:fwrite(Message),
      io:fwrite("\n"),
      messageTwoOne(Playermessid);
		{message, Message} ->
      Player1 ! {messageTwoOne, Message},
      messageTwoOne(Player1);
		notvalid ->
      Player1 ! notvalid,
      messageTwoOne(Player1)
	end.

%Message from Player 1 to Player 2
messageOneTwo(Player2) ->
	receive
		{messageTwoOne, Message} ->
      io:fwrite("Message from Player 2: "),
      io:fwrite(Message),
      io:fwrite("\n"),
      messageOneTwo(Player2);
		{message, Message} ->
      {messageTwoOne, Player2} ! {messageOneTwo, self(), Message},
      messageOneTwo(Player2);
		notvalid ->
      io:fwrite("\nNot a valid move!\n"),
      messageOneTwo(Player2)
	end.


%Pattern match to determine actions upon player move
newboard(Board, Player1, Player2) ->
	receive
    {player, Symbol, Loc} ->
        io:fwrite("Player 1 (~w) completed their turn.", [Symbol]),
		    A1 = setelement(Loc, Board, Symbol),
				{A,B} = checkvalid(Board, Loc, A1, player1),
        who ! B, printboard(A, B, Player2),
				Player1 ! {board, A, player, Symbol, Loc},
        newboard(A, Player1, Player2);

		{_, Symbol, Loc} ->
        io:fwrite("Player 2 (~w) completed their turn.", [Symbol]),
				A1 = setelement(Loc, Board, Symbol),
				{A,B} = checkvalid(Board, Loc, A1, player2),
				who ! B, printboard(A, B, Player2),
				Player1 ! {board, A, opponent, Symbol, Loc},
        newboard(A, Player1, Player2)
	end.

%Can we place a token at this location?
checkvalid(Board, N, Board2, Who) ->
	A = element(N, Board),
	case A of
		'_' -> case Who of
				      player1 -> {Board2, readyPlayerTwo};
				      player2 -> {Board2, player}
				end;
		_ -> io: fwrite("\n"), io:fwrite("Not a valid move!\n"), messageTwoOne ! notvalid,
				case Who of
				      player1 -> {Board, player};
				      player2 -> {Board, readyPlayerTwo}
				end
	end.

%Assesses turn and listens for placetoken() messages.
whoseturn(Who, Marker, Name) ->
	receive
		a1 -> case Marker of
				x -> board ! {Who, x, 1},
				case Who of
					player -> whoseturn(Name, o, Name);
					Name-> whoseturn(player, o, Name)
				end;
				o -> board ! {Who, o, 1},
				case Who of
					player -> whoseturn(Name, x, Name);
					Name-> whoseturn(player, x, Name)
				end
			end;

		a2 -> case Marker of
				x -> board ! {Who, x, 2},
				case Who of
					player -> whoseturn(Name, o, Name);
					Name-> whoseturn(player, o, Name)
				end;
				o -> board ! {Who, o, 2},
				case Who of
					player -> whoseturn(Name, x, Name);
					Name-> whoseturn(player, x, Name)
				end
			end;

		a3 -> case Marker of
				x -> board ! {Who, x, 3},
				case Who of
					player -> whoseturn(Name, o, Name);
					Name-> whoseturn(player, o, Name)
				end;
				o -> board ! {Who, o, 3},
				case Who of
					player -> whoseturn(Name, x, Name);
					Name-> whoseturn(player, x, Name)
				end
			end;

		b1 -> case Marker of
				x -> board ! {Who, x, 4},
				case Who of
					player -> whoseturn(Name, o, Name);
					Name-> whoseturn(player, o, Name)
				end;
				o -> board ! {Who, o, 4},
				case Who of
					player -> whoseturn(Name, x, Name);
					Name-> whoseturn(player, x, Name)
				end
			end;

		b2 -> case Marker of
				x -> board ! {Who, x, 5},
				case Who of
					player -> whoseturn(Name, o, Name);
					Name-> whoseturn(player, o, Name)
				end;
				o -> board ! {Who, o, 5},
				case Who of
					player -> whoseturn(Name, x, Name);
					Name-> whoseturn(player, x, Name)
				end
			end;

		b3 -> case Marker of
				x -> board ! {Who, x, 6},
				case Who of
					player -> whoseturn(Name, o, Name);
					Name-> whoseturn(player, o, Name)
				end;
				o -> board ! {Who, o, 6},
				case Who of
					player -> whoseturn(Name, x, Name);
					Name-> whoseturn(player, x, Name)
				end
			end;

		c1 -> case Marker of
				x -> board ! {Who, x, 7},
				case Who of
					player -> whoseturn(Name, o, Name);
					Name-> whoseturn(player, o, Name)
				end;
				o -> board ! {Who, o, 7},
				case Who of
					player -> whoseturn(Name, x, Name);
					Name-> whoseturn(player, x, Name)
				end
			end;

		c2 -> case Marker of
				x -> board ! {Who, x, 8},
				case Who of
					player -> whoseturn(Name, o, Name);
					Name-> whoseturn(player, o, Name)
				end;
				o -> board ! {Who, o, 8},
				case Who of
					player -> whoseturn(Name, x, Name);
					Name-> whoseturn(player, x, Name)
				end
			end;

		c3 -> case Marker of
				x -> board ! {Who, x, 9},
				case Who of
					player -> whoseturn(Name, o, Name);
					Name-> whoseturn(player, o, Name)
				end;
				o -> board ! {Who, o, 9},
				case Who of
					player -> whoseturn(Name, x, Name);
					Name-> whoseturn(player, x, Name)
				end
			end;

		player -> case Who of
				player -> whoseturn(player, Marker, Name);
				Name -> case Marker of
						x -> whoseturn(player, o, Name);
						o -> whoseturn(player, x, Name)
						end
				end;
		readyPlayerTwo -> case Who of
				Name -> whoseturn(Name, Marker, Name);
				player -> case Marker of
						x -> whoseturn(Name, o, Name);
						o -> whoseturn(Name, x, Name)
						end
				end;

		xwin -> case Who of
				player -> case Marker of
							x -> io:fwrite("Player (x) wins!\n"), endgame ! {player, x}, whoseturn(player, x, Name);
							o -> io:fwrite(Name), io:fwrite(" (x) wins!\n"), endgame ! {Name, x}, whoseturn(Name, x, Name)
						end;
				Name -> case Marker of
						x -> io:fwrite(Name), io:fwrite(" (x) wins!\n"), endgame ! {Name, x}, whoseturn(Name, x, Name);
						o -> io:fwrite("Player (x) wins!\n"), endgame ! {player, x},
							whoseturn(player, x, Name)
					end
			end;

		owin -> case Who of
				player -> case Marker of
							x -> io:fwrite(Name), io:fwrite(" (o) wins!\n"), endgame ! {Name, o}, whoseturn(Name, o, Name);
							o -> io:fwrite("Player (o) wins!\n"), endgame ! {player, o},
								whoseturn(player, o, Name)
						end;
				Name -> case Marker of
						x -> io:fwrite("Player (o) wins!\n"), endgame ! {player, o}, whoseturn(player, o, Name);
						o -> io:fwrite(Name), io:fwrite(" (o) wins!\n"), endgame ! {Name, o}, whoseturn(Name, o, Name)
					end
			end;
		draw -> io:fwrite("Game is a draw!\n"), endgame ! draw, whoseturn(Who, Marker, Name)

	end.

% Assess end of game conditions
endgame(Player1) ->
	receive
		{Winner, Marker} -> Player1 ! {endgame, Winner, Marker}, endgame(Player1);
		draw  -> Player1 ! {endgame, draw}, endgame(Player1)
	end.

% Context switching for ending a player's turn
next(Player1, Player2) ->
	receive
		player -> Player1 ! turn, next(Player1, Player2);
		readyPlayerTwo -> {readyPlayerTwo, Player2} ! turn, next(Player1, Player2)
	end.

% Print board for Player 1
printboard2(Board) ->
	A = element(1, Board), B = element(2, Board),	C = element(3, Board),
	D = element(4, Board), E = element(5, Board),	F = element(6, Board),
	G = element(7, Board), H = element(8, Board),	I = element(9, Board),
  io:fwrite("~n ~3w ~3w ~3w ~n", [A,B,C]),
  io:fwrite(" ~3w ~3w ~3w ~n", [D,E,F]),
  io:fwrite(" ~3w ~3w ~3w ~n~n", [G,H,I]).

% Print board for Player 2 and check for victory/draw conditions
printboard(Board, Next, Player2) ->
  A = element(1, Board), B = element(2, Board),	C = element(3, Board),
	D = element(4, Board), E = element(5, Board),	F = element(6, Board),
	G = element(7, Board), H = element(8, Board),	I = element(9, Board),
  io:fwrite("~n ~3w ~3w ~3w ~n", [A,B,C]),
  io:fwrite(" ~3w ~3w ~3w ~n", [D,E,F]),
  io:fwrite(" ~3w ~3w ~3w ~n~n", [G,H,I]),


	case check(Board) of
		ok -> {next, Player2} ! Next;
		draw -> {who, Player2} ! draw;
		{victory, x} -> {who, Player2} ! xwin;
		{victory, o} -> {who, Player2} ! owin
	end.

%Verifies winning conditions for the game
check(Board) ->
    case Board of
        {x, x, x,
         _, _, _,
         _, _, _} -> {victory, x};

        {_, _, _,
         x, x, x,
         _, _, _} -> {victory, x};

        {_, _, _,
         _, _, _,
         x, x, x} -> {victory, x};

        {x, _, _,
         x, _, _,
         x, _, _} -> {victory, x};

        {_, x, _,
         _, x, _,
         _, x, _} -> {victory, x};

        {_, _, x,
         _, _, x,
         _, _, x} -> {victory, x};

        {x, _, _,
         _, x, _,
         _, _, x} -> {victory, x};

        {_, _, x,
         _, x, _,
         x, _, _} -> {victory, x};

        {o, o, o,
         _, _, _,
         _, _, _} -> {victory, o};

        {_, _, _,
         o, o, o,
         _, _, _} -> {victory, o};

        {_, _, _,
         _, _, _,
         o, o, o} -> {victory, o};

        {o, _, _,
         o, _, _,
         o, _, _} -> {victory, o};

        {_, o, _,
         _, o, _,
         _, o, _} -> {victory, o};

        {_, _, o,
         _, _, o,
         _, _, o} -> {victory, o};

        {o, _, _,
         _, o, _,
         _, _, o} -> {victory, o};

        {_, _, o,
         _, o, _,
         o, _, _} -> {victory, o};

        {A, B, C,
         D, E, F,
         G, H, I} when A =/= '_', B =/= '_', C =/= '_',
                       D =/= '_', E =/= '_', F =/= '_',
                       G =/= '_', H =/= '_', I =/= '_' ->
            draw;

        _ -> ok
    end.
