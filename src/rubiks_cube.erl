-module(rubiks_cube).

-export([init/0,
         pieces/1,
         where_are_pieces/1,
         rotate/2,
         scramble/0,
         scramble/1,
         print/1]).

-include("models.hrl").

%% API
%% -----------------------------------------------------------------------------

init() ->
	Stickers = init_stickers(),
	#rubiks_cube{stickers = Stickers}.

%% Represent the Cube as 27 pieces.
%% Return map: piece coord => piece record
pieces(#rubiks_cube{stickers = Stickers}) ->
	lists:foldl(fun(#sticker{x = X, y = Y, z = Z, color = Color}, Pieces) ->
		PXYZ = sticker_xyz_to_piece_xyz({X, Y, Z}),
		Piece = case maps:find(PXYZ, Pieces) of
			error ->
				#piece{colors = [Color]};
			{ok, #piece{colors = Colors}} ->
				#piece{colors = lists:sort([Color | Colors])}
		end,
		maps:put(PXYZ, Piece, Pieces)
	end, #{}, Stickers).

%% Return map: piece record => piece coord
where_are_pieces(#rubiks_cube{stickers = _Stickers} = Cube) ->
	Pieces = pieces(Cube),
	where_are_pieces(Pieces);
where_are_pieces(Pieces) ->
	maps:fold(fun(XYZ, Piece, XYZs) ->
		maps:put(Piece, XYZ, XYZs)
	end, #{}, Pieces).

rotate(Cube, u) ->
	rotate(Cube, fun is_sticker_up/1, fun xz_clockwise/1);
rotate(Cube, ui) ->
	rotate(Cube, fun is_sticker_up/1, fun xz_counterclockwise/1);
rotate(Cube, d) ->
	rotate(Cube, fun is_sticker_down/1, fun xz_counterclockwise/1);
rotate(Cube, di) ->
	rotate(Cube, fun is_sticker_down/1, fun xz_clockwise/1);
rotate(Cube, r) ->
	rotate(Cube, fun is_sticker_right/1, fun yz_clockwise/1);
rotate(Cube, ri) ->
	rotate(Cube, fun is_sticker_right/1, fun yz_counterclockwise/1);
rotate(Cube, l) ->
	rotate(Cube, fun is_sticker_left/1, fun yz_counterclockwise/1);
rotate(Cube, li) ->
	rotate(Cube, fun is_sticker_left/1, fun yz_clockwise/1);
rotate(Cube, f) ->
	rotate(Cube, fun is_sticker_front/1, fun xy_clockwise/1);
rotate(Cube, fi) ->
	rotate(Cube, fun is_sticker_front/1, fun xy_counterclockwise/1);
rotate(Cube, b) ->
	rotate(Cube, fun is_sticker_back/1, fun xy_counterclockwise/1);
rotate(Cube, bi) ->
	rotate(Cube, fun is_sticker_back/1, fun xy_clockwise/1);
rotate(Cube, []) ->
	Cube;
rotate(Cube, [Direction | Directions]) ->
	Cube2 = rotate(Cube, Direction),
	rotate(Cube2, Directions).

scramble() ->
	scramble(init()).

scramble(Cube) ->
	Directions = [random_direction() || _ <- lists:seq(1, 500)],
	rotate(Cube, Directions).

print(#rubiks_cube{stickers = Stickers}) ->
	Letters = #{
		white  => "w",
		yellow => "y",
		blue   => "b",
		green  => "g",
		red    => "r",
		orange => "o"
	},
	Colors = maps:from_list([
		{{X, Y, Z}, Color} || #sticker{x = X, y = Y, z = Z, color = Color} <- Stickers
	]),
	Get = fun(Coord) ->
		Color = maps:get(Coord, Colors),
		maps:get(Color, Letters)
	end,
io:format("
                 +-------+-------+-------+
                /       /       /       /|
               /   ~s   /   ~s   /   ~s   / |
              +-------+-------+-------+  |
             /       /       /       /|~s +
            /   ~s   /   ~s   /   ~s   / | /|
           +-------+-------+-------+  |/ |
          /       /       /       /|~s +  |
         /   ~s   /   ~s   /   ~s   / | /|~s +
        +-------+-------+-------+  |/ | /|
        |       |       |       |~s +  |/ |
        |   ~s   |   ~s   |   ~s   | /|~s +  |
        |       |       |       |/ | /|~s +
        +-------+-------+-------+  |/ | /
        |       |       |       |~s +  |/
        |   ~s   |   ~s   |   ~s   | /|~s +
        |       |       |       |/ | /
        +-------+-------+-------+  |/
        |       |       |       |~s +
        |   ~s   |   ~s   |   ~s   | /
        |       |       |       |/
        +-------+-------+-------+

                 +-------+-------+-------+
                /|       |       |       |
               / |   ~s   |   ~s   |   ~s   |
              +  |       |       |       |
             /|~s +-------+-------+-------+
            / | /|       |       |       |
           +  |/ |   ~s   |   ~s   |   ~s   |
          /|~s +  |       |       |       |
         / | /|~s +-------+-------+-------+
        +  |/ | /|       |       |       |
        |~s +  |/ |   ~s   |   ~s   |   ~s   |
        | /|~s +  |       |       |       |
        |/ | /|~s +-------+-------+-------+
        +  |/ | /       /       /       /
        |~s +  |/   ~s   /   ~s   /   ~s   /
        | /|~s +-------+-------+-------+
        |/ | /       /       /       /
        +  |/   ~s   /   ~s   /   ~s   /
        |~s +-------+-------+-------+
        | /       /       /       /
        |/   ~s   /   ~s   /    ~s  /
        +-------+-------+-------+
", [Get(?COORD_U1), Get(?COORD_U2), Get(?COORD_U3), Get(?COORD_R3), Get(?COORD_U4),
    Get(?COORD_U5), Get(?COORD_U6), Get(?COORD_R2), Get(?COORD_U7), Get(?COORD_U8),
    Get(?COORD_U9), Get(?COORD_R6), Get(?COORD_R1), Get(?COORD_F1), Get(?COORD_F2),
    Get(?COORD_F3), Get(?COORD_R5), Get(?COORD_R9), Get(?COORD_R4), Get(?COORD_F4),
    Get(?COORD_F5), Get(?COORD_F6), Get(?COORD_R8), Get(?COORD_R7), Get(?COORD_F7),
	Get(?COORD_F8), Get(?COORD_F9),
    Get(?COORD_B3), Get(?COORD_B2), Get(?COORD_B1), Get(?COORD_L1), Get(?COORD_B6),
    Get(?COORD_B5), Get(?COORD_B4), Get(?COORD_L2), Get(?COORD_L4), Get(?COORD_L3),
    Get(?COORD_B9), Get(?COORD_B8), Get(?COORD_B7), Get(?COORD_L5), Get(?COORD_L7),
    Get(?COORD_L6), Get(?COORD_D7), Get(?COORD_D8), Get(?COORD_D9), Get(?COORD_L8),
    Get(?COORD_D4), Get(?COORD_D5), Get(?COORD_D6), Get(?COORD_L9), Get(?COORD_D1),
    Get(?COORD_D2), Get(?COORD_D3)]).

%% INTERNALS
%% -----------------------------------------------------------------------------

init_stickers() ->
	init_stickers(up, white)   ++ init_stickers(down, yellow) ++
	init_stickers(right, blue) ++ init_stickers(left, green)  ++
	init_stickers(front, red)  ++ init_stickers(back, orange).

init_stickers(Face, Color) when ?IS_FACE(Face),
                                ?IS_COLOR(Color) ->
	[#sticker{x = X, y = Y, z = Z, color = Color} || {X, Y, Z} <- xyz(Face)].

xyz(up)    -> xyz(?COORDS_NORMAL,  ?COORDS_Y_UP,   ?COORDS_NORMAL);
xyz(down)  -> xyz(?COORDS_NORMAL,  ?COORDS_Y_DOWN, ?COORDS_NORMAL);
xyz(right) -> xyz(?COORDS_X_RIGHT, ?COORDS_NORMAL, ?COORDS_NORMAL);
xyz(left)  -> xyz(?COORDS_X_LEFT,  ?COORDS_NORMAL, ?COORDS_NORMAL);
xyz(front) -> xyz(?COORDS_NORMAL,  ?COORDS_NORMAL, ?COORDS_Z_FRONT);
xyz(back)  -> xyz(?COORDS_NORMAL,  ?COORDS_NORMAL, ?COORDS_Z_BACK).

xyz(Xs, Ys, Zs) ->
	[{X, Y, Z} || X <- Xs, Y <- Ys, Z <- Zs].

rotate(#rubiks_cube{stickers = Stickers} = Cube, ShouldUpdate, UpdateCoords) ->
	{StickersToUpdate, StickersNotUpdated}
		= lists:partition(ShouldUpdate, Stickers),
	NewStickers = lists:map(UpdateCoords, StickersToUpdate)
	              ++
	              StickersNotUpdated,
	Cube#rubiks_cube{stickers = NewStickers}.

is_sticker_up(#sticker{y = Y}) -> Y >= ?COORD_Y_UP - 1.
is_sticker_down(#sticker{y = Y}) -> Y =< ?COORD_Y_DOWN + 1.
is_sticker_right(#sticker{x = X}) -> X >= ?COORD_X_RIGHT - 1.
is_sticker_left(#sticker{x = X}) -> X =< ?COORD_X_LEFT + 1.
is_sticker_front(#sticker{z = Z}) -> Z >= ?COORD_Z_FRONT - 1.
is_sticker_back(#sticker{z = Z}) -> Z =< ?COORD_Z_BACK + 1.

xz_clockwise(#sticker{x = X, z = Z} = S) -> S#sticker{x = -(Z), z = X}.
xz_counterclockwise(#sticker{x = X, z = Z} = S) -> S#sticker{x = Z, z = -(X)}.

yz_clockwise(#sticker{y = Y, z = Z} = S) -> S#sticker{y = Z, z = -(Y)}.
yz_counterclockwise(#sticker{y = Y, z = Z} = S) -> S#sticker{y = -(Z), z = Y}.

xy_clockwise(#sticker{x = X, y = Y} = S) -> S#sticker{x = Y, y = -(X)}.
xy_counterclockwise(#sticker{x = X, y = Y} = S) -> S#sticker{x = -(Y), y = X}.

random_direction() ->
	N = rand:uniform(length(?DIRECTIONS)),
	lists:nth(N, ?DIRECTIONS).

sticker_xyz_to_piece_xyz({3, Y, Z}) ->
	{2, Y, Z};
sticker_xyz_to_piece_xyz({-3, Y, Z}) ->
	{-2, Y, Z};
sticker_xyz_to_piece_xyz({X, 3, Z}) ->
	{X, 2, Z};
sticker_xyz_to_piece_xyz({X, -3, Z}) ->
	{X, -2, Z};
sticker_xyz_to_piece_xyz({X, Y, 3}) ->
	{X, Y, 2};
sticker_xyz_to_piece_xyz({X, Y, -3}) ->
	{X, Y, -2}.
