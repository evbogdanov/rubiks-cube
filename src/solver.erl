-module(solver).

-export([start/1]).

-include("models.hrl").

%% BEGIN SOLVING
%% -----------------------------------------------------------------------------

start(Cube) ->
	{ok, Cube2}  = daisy(Cube),
	{ok, Cube3}  = white_cross(Cube2),
	{ok, Cube4}  = white_corners(Cube3),
	{ok, Cube5}  = middle_layer(Cube4),
	{ok, Cube6}  = yellow_cross(Cube5),
	{ok, Cube7}  = fixed_yellow_cross(Cube6),
	{ok, Cube8}  = yellow_corners(Cube7),
	{ok, _Cube9} = grand_final(Cube8).

%% DAISY
%% -----------------------------------------------------------------------------

%% Create daisy formation: 4 white stickers around yellow center. Have a look:
%% https://www.youtube.com/watch?v=rmnSpUgOvyI&t=1m31s

daisy(#rubiks_cube{stickers = Stickers} = Cube) ->
	WhiteStickersOnEdgePieces
		= [S || #sticker{x = X, y = Y, z = Z, color = white} = S <- Stickers,
		   X =:= 0 orelse Y =:= 0 orelse Z =:= 0,
		   S =/= #sticker{x = 0, y = 3, z = 0, color = white}],

	case WhiteStickersOnEdgePieces of
		[#sticker{y = -3}, #sticker{y = -3},
		 #sticker{y = -3}, #sticker{y = -3}] ->
			%% Every white sticker is where I want it to be. Daisy solved.
			{ok, Cube};
		_ ->
			%% More stickers to move to D face. Find next and continue building
			%% my dear daisy.
			WhiteSticker = daisy_find_sticker(WhiteStickersOnEdgePieces),
			Directions   = daisy_directions(WhiteSticker, Stickers),
			Cube2        = rubiks_cube:rotate(Cube, Directions),
			daisy(Cube2)
	end.

%% Find me the next white sticker to work with.
daisy_find_sticker(Stickers) ->
	hd(lists:sort(fun daisy_search_order/2, Stickers)).

daisy_search_order(#sticker{y = Y1}, #sticker{y = Y2}) ->
	Order = #{-2 => 0,
	           3 => 1,
	           2 => 2,
	           0 => 3,
	          -3 => 4}, % Y =:= -3 means that sticker is already found,
	                    % please don't care about it

	maps:get(Y1, Order) < maps:get(Y2, Order).

%% Function daisy_directions/2 returns directions (one or a list) to make
%% current WhiteSticker closer to its desired destination.
daisy_directions(#sticker{x = X, y = -2, z = Z}, _) when {X, -2, Z} =:= ?COORD_F8 ->
	[f, di, l, d];
daisy_directions(#sticker{y = -2}, _) ->
	%% Rotate sticker until it will be at F8 coord...
	d;
daisy_directions(#sticker{x = X, y = 3, z = Z}, Stickers) ->
	%% It should be easy to move sticker to the opposite face, right?
	%% Not so fast. The first thing to do is to check whether or not the
	%% opposite coordinate is occupied by another white sticker.
	case lists:member(#sticker{x = X, y = -3, z = Z, color = white}, Stickers) of
		true ->
			%% Not ready to solve just yet. Need some space in D face.
			d;
		false ->
			%% Have space for this sticker. But which direction to rotate to?
			if
				X > 0 -> [r, r];
				X < 0 -> [l, l];
				Z > 0 -> [f, f];
				Z < 0 -> [b, b]
			end
	end;
daisy_directions(#sticker{x = X, y = 2, z = Z}, Stickers) ->
	{{X2, Y2, Z2}, Dirs} = case {X, 2, Z} of
		?COORD_L2 -> {?COORD_D4, [l, l]};
		?COORD_F2 -> {?COORD_D2, [f, f]};
		?COORD_R2 -> {?COORD_D6, [r, r]};
		?COORD_B2 -> {?COORD_D8, [b, b]}
	end,
	case lists:member(#sticker{x = X2, y = Y2, z = Z2, color = white}, Stickers) of
		true  -> d;
		false -> Dirs
	end;
daisy_directions(#sticker{x = X, y = 0, z = Z}, Stickers) ->
	{{X2, Y2, Z2}, Dirs} = case {X, 0, Z} of
		?COORD_L4 -> {?COORD_D4, li};
		?COORD_L6 -> {?COORD_D4, l};
		?COORD_F4 -> {?COORD_D2, fi};
		?COORD_F6 -> {?COORD_D2, f};
		?COORD_R4 -> {?COORD_D6, ri};
		?COORD_R6 -> {?COORD_D6, r};
		?COORD_B4 -> {?COORD_D8, bi};
		?COORD_B6 -> {?COORD_D8, b}
	end,
	case lists:member(#sticker{x = X2, y = Y2, z = Z2, color = white}, Stickers) of
		true  -> d;
		false -> Dirs
	end.

%% WHITE CROSS
%% -----------------------------------------------------------------------------

%% Daisy formation goes to U face and becomes the white cross, where all edge
%% pieces (white and other colored sticker) are in their right places.

white_cross(#rubiks_cube{stickers = Stickers} = Cube) ->
	WhiteCrossOK = lists:all(fun({X, Y, Z}) ->
		lists:member(#sticker{x = X, y = Y, z = Z, color = white}, Stickers)
	end, [?COORD_U2, ?COORD_U4, ?COORD_U6, ?COORD_U8]),

	if
		WhiteCrossOK ->
			{ok, Cube};
		true ->
			Cube2 = lists:foldl(fun({{X1, Y1, Z1}, {X2, Y2, Z2}, Color, Dirs},
			                        #rubiks_cube{stickers = Ss} = Cub) ->
				PieceOK = lists:member(#sticker{x = X1, y = Y1, z = Z1,
				                                color = white}, Ss)
				andalso   lists:member(#sticker{x = X2, y = Y2, z = Z2,
				                                color = Color}, Ss),
				if
					PieceOK -> rubiks_cube:rotate(Cub, Dirs);
					true    -> Cub
				end
			end, Cube, [{?COORD_D2, ?COORD_F8, red,    [f, f]},
			            {?COORD_D4, ?COORD_L8, green,  [l, l]},
			            {?COORD_D6, ?COORD_R8, blue,   [r, r]},
			            {?COORD_D8, ?COORD_B8, orange, [b, b]}]),

			Cube3 = rubiks_cube:rotate(Cube2, d),
			white_cross(Cube3)
	end.

%% WHITE CORNERS
%% -----------------------------------------------------------------------------

%% With the white cross in U face, now I can solve the white corner pieces.

white_corners(#rubiks_cube{stickers = Stickers} = Cube) ->
	%% Desired stickers on each piece
	P1Stickers = [?STICKER(?COORD_U1, white),
	              ?STICKER(?COORD_L1, green), 
	              ?STICKER(?COORD_B3, orange)],
	P3Stickers = [?STICKER(?COORD_U3, white),
	              ?STICKER(?COORD_R3, blue), 
	              ?STICKER(?COORD_B1, orange)],
	P7Stickers = [?STICKER(?COORD_U7, white),
	              ?STICKER(?COORD_F1, red), 
	              ?STICKER(?COORD_L3, green)],
	P9Stickers = [?STICKER(?COORD_U9, white),
	              ?STICKER(?COORD_F3, red), 
	              ?STICKER(?COORD_R1, blue)],

	OkP1 = length(Stickers -- P1Stickers) =:= ?N_STICKERS - 3,
	OkP3 = length(Stickers -- P3Stickers) =:= ?N_STICKERS - 3,
	OkP7 = length(Stickers -- P7Stickers) =:= ?N_STICKERS - 3,
	OkP9 = length(Stickers -- P9Stickers) =:= ?N_STICKERS - 3,

	if
		OkP1 andalso OkP3 andalso OkP7 andalso OkP9 ->
			{ok, Cube};
		not OkP1 ->
			wc(Cube, #piece{colors = [green, orange, white]}, ?COORD_P1);
		not OkP3 ->
			wc(Cube, #piece{colors = [blue, orange, white]}, ?COORD_P3);
		not OkP7 ->
			wc(Cube, #piece{colors = [green, red, white]}, ?COORD_P7);
		not OkP9 ->
			wc(Cube, #piece{colors = [blue, red, white]}, ?COORD_P9)
	end.

wc(Cube, Piece, {X, _Y, Z} = XYZ) ->
	Where = rubiks_cube:where_are_pieces(Cube),
	Directions = case maps:get(Piece, Where) of
		{X, _, Z} ->
			wc_directions(XYZ);
		{_, -2, _} ->
			di;
		{_, 2, _} = XYZNeedToFix ->
			[D1, D2, D3, _D4] = wc_directions(XYZNeedToFix),
			[D1, D2, D3]
	end,
	Cube2 = rubiks_cube:rotate(Cube, Directions),
	white_corners(Cube2).

wc_directions({-2, _Y, -2}) ->
	[li, di, l, d];
wc_directions({2, _Y, -2}) ->
	[bi, di, b, d];
wc_directions({-2, _Y, 2}) ->
	[fi, di, f, d];
wc_directions({2, _Y, 2}) ->
	[ri, di, r, d].

%% MIDDLE LAYER
%% -----------------------------------------------------------------------------

%% The first (white) layer is complete. Pieces P1..P9 in the correct place.
%% Now I have to solve the middle layer by positioning four edge pieces:
%% P10 * P12
%%  *  *  *
%% P16 * P18

middle_layer(#rubiks_cube{stickers = Stickers} = Cube) ->
	OkP10 = lists:member(?STICKER(?COORD_L4, green), Stickers) andalso
	        lists:member(?STICKER(?COORD_B6, orange), Stickers),
	OkP12 = lists:member(?STICKER(?COORD_R6, blue), Stickers) andalso
	        lists:member(?STICKER(?COORD_B4, orange), Stickers),
	OkP16 = lists:member(?STICKER(?COORD_L6, green), Stickers) andalso
	        lists:member(?STICKER(?COORD_F4, red), Stickers),
	OkP18 = lists:member(?STICKER(?COORD_R4, blue), Stickers) andalso
	        lists:member(?STICKER(?COORD_F6, red), Stickers),

	if
		OkP10 andalso OkP12 andalso OkP16 andalso OkP18 ->
			{ok, Cube};
		not OkP10 ->
			ml(Cube,
			   #piece{colors = [green, orange]},
			   {?COORD_P22, ?STICKER(?COORD_L8, green)},
			   {?COORD_P20, ?STICKER(?COORD_B8, orange)},
			   ?COORD_P10);
		not OkP12 ->
			ml(Cube,
			   #piece{colors = [blue, orange]},
			   {?COORD_P24, ?STICKER(?COORD_R8, blue)},
			   {?COORD_P20, ?STICKER(?COORD_B8, orange)},
			   ?COORD_P12);
		not OkP16 ->
			ml(Cube,
			   #piece{colors = [green, red]},
			   {?COORD_P22, ?STICKER(?COORD_L8, green)},
			   {?COORD_P26, ?STICKER(?COORD_F8, red)},
			   ?COORD_P16);
		not OkP18 ->
			ml(Cube,
			   #piece{colors = [blue, red]},
			   {?COORD_P24, ?STICKER(?COORD_R8, blue)},
			   {?COORD_P26, ?STICKER(?COORD_F8, red)},
			   ?COORD_P18)
	end.

ml(#rubiks_cube{stickers = Stickers} = Cube,
   Piece,
   {XYZFrom1, Sticker1},
   {XYZFrom2, Sticker2},
   XYZTo
) ->
	Where = rubiks_cube:where_are_pieces(Cube),
	Directions = case maps:get(Piece, Where) of
		XYZFrom1 ->
			case lists:member(Sticker1, Stickers) of
				true  -> ml_directions(XYZFrom1, XYZTo);
				false -> d
			end;
		XYZFrom2 ->
			case lists:member(Sticker2, Stickers) of
				true  -> ml_directions(XYZFrom2, XYZTo);
				false -> d
			end;
		{_X, -2, _Z} ->
			d;
		{_X, 0, _Z} = XYZNeedToFix ->
			ml_directions_fix(XYZNeedToFix)
	end,
	Cube2 = rubiks_cube:rotate(Cube, Directions),
	middle_layer(Cube2).

ml_directions(?COORD_P26, ?COORD_P16) ->
	[d, l, di, li, di, fi, d, f];
ml_directions(?COORD_P26, ?COORD_P18) ->
	[di, ri, d, r, d, f, di, fi];
ml_directions(?COORD_P24, ?COORD_P18) ->
	[d, f, di, fi, di, ri, d, r];
ml_directions(?COORD_P24, ?COORD_P12) ->
	[di, bi, d, b, d, r, di, ri];
ml_directions(?COORD_P20, ?COORD_P12) ->
	[d, r, di, ri, di, bi, d, b];
ml_directions(?COORD_P20, ?COORD_P10) ->
	[di, li, d, l, d, b, di, bi];
ml_directions(?COORD_P22, ?COORD_P10) ->
	[d, b, di, bi, di, li, d, l];
ml_directions(?COORD_P22, ?COORD_P16) ->
	[di, fi, d, f, d, l, di, li].	

ml_directions_fix(?COORD_P10) ->
	ml_directions(?COORD_P20, ?COORD_P10);
ml_directions_fix(?COORD_P12) ->
	ml_directions(?COORD_P24, ?COORD_P12);
ml_directions_fix(?COORD_P16) ->
	ml_directions(?COORD_P26, ?COORD_P16);
ml_directions_fix(?COORD_P18) ->
	ml_directions(?COORD_P26, ?COORD_P18).

%% YELLOW CROSS
%% -----------------------------------------------------------------------------

%% First two layers are solved. The next step is to create the yellow cross.
%% Note: for now I don't care about the other colors on the cross pieces.

yellow_cross(#rubiks_cube{stickers = Stickers} = Cube) ->
	Return = case yc_stickers_ok(Stickers, #{}) of
		#{d2 := ok, d4 := ok, d6 := ok, d8 := ok} -> {ok, Cube};
		#{d4 := ok, d6 := ok}                     -> [f, l, d, li, di, fi];
		#{d2 := ok, d8 := ok}                     -> [r, f, d, fi, di, ri];
		#{d2 := ok, d6 := ok}                     -> [l, b, d, bi, di, li];
		#{d6 := ok, d8 := ok}                     -> [f, l, d, li, di, fi];
		#{d4 := ok, d8 := ok}                     -> [r, f, d, fi, di, ri];
		#{d2 := ok, d4 := ok}                     -> [b, r, d, ri, di, bi];
		#{}                                       -> [f, l, d, li, di, fi]
	end,
	case Return of
		{ok, Cube} -> {ok, Cube};
		Directions -> yellow_cross(rubiks_cube:rotate(Cube, Directions))
	end.

yc_stickers_ok([], OKs) ->
	OKs;
yc_stickers_ok([?YELLOW_STICKER_D2 | Stickers], OKs) ->
	yc_stickers_ok(Stickers, maps:put(d2, ok, OKs));
yc_stickers_ok([?YELLOW_STICKER_D4 | Stickers], OKs) ->
	yc_stickers_ok(Stickers, maps:put(d4, ok, OKs));
yc_stickers_ok([?YELLOW_STICKER_D6 | Stickers], OKs) ->
	yc_stickers_ok(Stickers, maps:put(d6, ok, OKs));
yc_stickers_ok([?YELLOW_STICKER_D8 | Stickers], OKs) ->
	yc_stickers_ok(Stickers, maps:put(d8, ok, OKs));
yc_stickers_ok([_Sticker | Stickers], OKs) ->
	yc_stickers_ok(Stickers, OKs).

%% FIXED YELLOW CROSS
%% -----------------------------------------------------------------------------

%% Time to fix all edge colors on the yellow cross.

fixed_yellow_cross(#rubiks_cube{stickers = Stickers} = Cube) ->
	Return = case fyc_colors_ok(Stickers, #{}) of
		#{red    := ok, blue   := ok,
		  orange := ok, green  := ok} -> {ok, Cube};
		#{red    := ok, blue   := ok} -> [r, d, ri, d, r, d, d, ri, d];
		#{blue   := ok, orange := ok} -> [b, d, bi, d, b, d, d, bi, d];
		#{orange := ok, green  := ok} -> [l, d, li, d, l, d, d, li, d];
		#{green  := ok, red    := ok} -> [f, d, fi, d, f, d, d, fi, d];
		#{blue   := ok, green  := ok} -> [l, d, li, d, l, d, d, li];
		#{red    := ok, orange := ok} -> [f, d, fi, d, f, d, d, fi];
		#{}                           -> d
	end,
	case Return of
		{ok, Cube} -> {ok, Cube};
		Directions -> fixed_yellow_cross(rubiks_cube:rotate(Cube, Directions))
	end.

fyc_colors_ok([], OKs) ->
	OKs;
fyc_colors_ok([?RED_STICKER_F8 | Stickers], OKs) ->
	fyc_colors_ok(Stickers, maps:put(red, ok, OKs));
fyc_colors_ok([?BLUE_STICKER_R8 | Stickers], OKs) ->
	fyc_colors_ok(Stickers, maps:put(blue, ok, OKs));
fyc_colors_ok([?ORANGE_STICKER_B8 | Stickers], OKs) ->
	fyc_colors_ok(Stickers, maps:put(orange, ok, OKs));
fyc_colors_ok([?GREEN_STICKER_L8 | Stickers], OKs) ->
	fyc_colors_ok(Stickers, maps:put(green, ok, OKs));
fyc_colors_ok([_Sticker | Stickers], OKs) ->
	fyc_colors_ok(Stickers, OKs).

%% YELLOW CORNERS
%% -----------------------------------------------------------------------------

%% P19 * P21
%%  *  *  *
%% P25 * P27

yellow_corners(Cube) ->
	Where = rubiks_cube:where_are_pieces(Cube),
	
	P19XYZ = maps:get(#piece{colors = [green, orange, yellow]}, Where),
	P21XYZ = maps:get(#piece{colors = [blue, orange, yellow]}, Where),
	P25XYZ = maps:get(#piece{colors = [green, red, yellow]}, Where),
	P27XYZ = maps:get(#piece{colors = [blue, red, yellow]}, Where),

	Return = case {P19XYZ, P21XYZ, P25XYZ, P27XYZ} of
		{?COORD_P19, ?COORD_P21, ?COORD_P25, ?COORD_P27} ->
			{ok, Cube};
		{?COORD_P19, {X, Y, Z}, _, _} when {Z, Y, -(X)} =:= ?COORD_P21 ->
			[di, li, d, r, di, l, d, ri];
		{?COORD_P19, _, {X, Y, Z}, _} when {-(Z), Y, X} =:= ?COORD_P25 ->
			[d, b, di, fi, d, bi, di, f];
		{_, ?COORD_P21, _, {X, Y, Z}} when {Z, Y, -(X)} =:= ?COORD_P27 ->
			[di, bi, d, f, di, b, d, fi];
		{{X, Y, Z}, ?COORD_P21, _, _} when {-(Z), Y, X} =:= ?COORD_P19 ->
			[d, r, di, li, d, ri, di, l];
		{{X, Y, Z}, _, ?COORD_P25, _} when {Z, Y, -(X)} =:= ?COORD_P19 ->
			[di, fi, d, b, di, f, d, bi];		
		{_, _, ?COORD_P25, {X, Y, Z}} when {-(Z), Y, X} =:= ?COORD_P27 ->
			[d, l, di, ri, d, li, di, r];			
		{_, _, {X, Y, Z}, ?COORD_P27} when {Z, Y, -(X)} =:= ?COORD_P25 ->
			[di, ri, d, l, di, r, d, li];
		{_, {X, Y, Z}, _, ?COORD_P27} when {-(Z), Y, X} =:= ?COORD_P21 ->
			[d, f, di, bi, d, fi, di, b];
		_NoMatch ->
			[d, l, di, ri, d, li, di, r]
	end,
	case Return of
		{ok, Cube} -> {ok, Cube};
		Directions -> yellow_corners(rubiks_cube:rotate(Cube, Directions))
	end.

%% GRAND FINAL
%% -----------------------------------------------------------------------------

grand_final(#rubiks_cube{stickers = Stickers} = Cube) ->
	#rubiks_cube{stickers = InitialStickers} = rubiks_cube:init(),
	case InitialStickers -- Stickers of
		[] ->
			{ok, Cube};
		_CubeNotSolvedYet ->
			Pieces = rubiks_cube:pieces(Cube),

			Directions = case {maps:get(?COORD_P27, Pieces),
			                   maps:get(?COORD_P9, Pieces)} of

				{#piece{colors = [blue, red, yellow]}, _} ->
					gf_piece27(red, blue, Stickers);
				{_, #piece{colors = [blue, red, yellow]}} ->
					[r, u, ri, ui];

				{#piece{colors = [blue, orange, yellow]}, _} ->
					gf_piece27(blue, orange, Stickers);
				{_, #piece{colors = [blue, orange, yellow]}} ->
					[r, u, ri, ui];

				{#piece{colors = [green, orange, yellow]}, _} ->
					gf_piece27(orange, green, Stickers);
				{_, #piece{colors = [green, orange, yellow]}} ->
					[r, u, ri, ui];

				{#piece{colors = [green, red, yellow]}, _} ->
					gf_piece27(green, red, Stickers);
				{_, #piece{colors = [green, red, yellow]}} ->
					[r, u, ri, ui]

			end,
			Cube2 = rubiks_cube:rotate(Cube, Directions),
			grand_final(Cube2)
	end.

gf_piece27(F9Color, R7Color, Stickers) ->
	case lists:member(?STICKER(?COORD_F9, F9Color), Stickers) andalso
	     lists:member(?STICKER(?COORD_R7, R7Color), Stickers) of
		true  -> di;
		false -> [r, u, ri, ui]
	end.

%% The end.
