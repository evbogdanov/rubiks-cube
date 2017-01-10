%%
%%              +------------+
%%              | U1  U2  U3 |
%%              |            |
%%              | U4  U5  U6 |
%%              |            |
%%              | U7  U8  U9 |
%% +------------+------------+------------+------------+
%% | L1  L2  L3 | F1  F2  F3 | R1  R2  R3 | B1  B2  B3 |
%% |            |            |            |            |
%% | L4  L5  L6 | F4  F5  F6 | R4  R5  R6 | B4  B5  B6 |
%% |            |            |            |            |
%% | L7  L8  L9 | F7  F8  F9 | R7  R8  R9 | B7  B8  B9 |
%% +------------+------------+------------+------------+
%%              | D1  D2  D3 |
%%              |            |
%%              | D4  D5  D6 |
%%              |            |
%%              | D7  D8  D9 |
%%              +------------+
%%
%%                  +-------+-------+-------+
%%                 /       /       /       /|
%%                /  U1   /  U2   /  U3   / |
%%               +-------+-------+-------+  |
%%              /       /       /       /|R3+
%%             /  U4   /  U5   /  U6   / | /|
%%            +-------+-------+-------+  |/ |
%%           /       /       /       /|R2+  |
%%          /  U7   /  U8   /  U9   / | /|R6+
%%         +-------+-------+-------+  |/ | /|
%%         |       |       |       |R1+  |/ |
%%         |  F1   |  F2   |  F3   | /|R5+  |
%%         |       |       |       |/ | /|R9+
%%         +-------+-------+-------+  |/ | /
%%         |       |       |       |R4+  |/
%%         |  F4   |  F5   |  F6   | /|R8+
%%         |       |       |       |/ | /
%%         +-------+-------+-------+  |/
%%         |       |       |       |R7+
%%         |  F7   |  F8   |  F9   | /
%%         |       |       |       |/
%%         +-------+-------+-------+
%% 
%%                  +-------+-------+-------+
%%                 /|       |       |       |
%%                / |  B3   |  B2   |  B1   |
%%               +  |       |       |       |
%%              /|L1+-------+-------+-------+
%%             / | /|       |       |       |
%%            +  |/ |  B6   |  B5   |  B4   |
%%           /|L2+  |       |       |       |
%%          / | /|L4+-------+-------+-------+
%%         +  |/ | /|       |       |       |
%%         |L3+  |/ |  B9   |  B8   |  B7   |
%%         | /|L5+  |       |       |       |
%%         |/ | /|L7+-------+-------+-------+
%%         +  |/ | /       /       /       /
%%         |L6+  |/  D7   /  D8   /  D9   /
%%         | /|L8+-------+-------+-------+
%%         |/ | /       /       /       /
%%         +  |/  D4   /  D5   /  D6   /
%%         |L9+-------+-------+-------+
%%         | /       /       /       /
%%         |/  D1   /  D2   /   D3  /
%%         +-------+-------+-------+
%%
%%                  +-------+-------+-------+
%%                 /       /       /       /•
%%                /  P1   /  P2   /  P3   / •
%%               +-------+-------+-------+  •
%%              /       /       /       /   •
%%             /  P4   /  P5   /  P6   /    •
%%            +-------+-------+-------+     •
%%           /       /       /       /      •
%%          /  P7   /  P8   /   P9  /       •
%%         +-------+-------+-------+        •
%%         •                                •
%%         •        +-------+-------+-------+
%%         •       /       /       /       /•
%%         •      /  P10  /  P11  /  P12  / •
%%         •     +-------+-------+-------+  •
%%         •    /       /       /       /   •
%%         •   /  P13  /  P14  /  P15  /    •
%%         •  +-------+-------+-------+     •
%%         • /       /       /       /      •
%%         •/  P16  /  P17  /  P18  /       •
%%         +-------+-------+-------+        •
%%         •                                •
%%         •        +-------+-------+-------+
%%         •       /       /       /       /
%%         •      /  P19  /  P20  /  P21  /
%%         •     +-------+-------+-------+
%%         •    /       /       /       /
%%         •   /  P22  /  P23  /  P24  /
%%         •  +-------+-------+-------+
%%         • /       /       /       /
%%         •/  P25  /  P26  /  P27  /
%%         +-------+-------+-------+
%%

%% FACE
%% -----------------------------------------------------------------------------

%% The Rubik's Cube consists of six independently rotating faces.

-type face() :: up    | down |
                right | left |
                front | back.

-define(IS_FACE(Face),
        Face =:= up    orelse Face =:= down orelse
        Face =:= right orelse Face =:= left orelse
        Face =:= front orelse Face =:= back).

%% COLOR
%% -----------------------------------------------------------------------------

%% Each face has colored stickers.

-type color() :: white | yellow |
                 blue  | green  |
                 red   | orange.

-define(IS_COLOR(Color),
        Color =:= white orelse Color =:= yellow orelse
        Color =:= blue  orelse Color =:= green  orelse
        Color =:= red   orelse Color =:= orange).

%% STICKER
%% -----------------------------------------------------------------------------

-define(N_STICKERS, 54).

-record(sticker, {x     :: integer(),
                  y     :: integer(),
                  z     :: integer(),
                  color :: color()}).

-type sticker() :: #sticker{}.

%% COORDINATES
%% -----------------------------------------------------------------------------

%% The value of each coordinate can be one of the following:
%% -3, -2, 0, 2, 3
%% Values -3 and 3 are special. They belong to the six faces of the cube.

-type coord_x() :: -3 | -2 | 0 | 2 | 3.
-type coord_y() :: -3 | -2 | 0 | 2 | 3.
-type coord_z() :: -3 | -2 | 0 | 2 | 3.
-type coord() :: {coord_x(), coord_y(), coord_z()}.

-define(COORD_Y_UP,     3).
-define(COORD_Y_DOWN,  -3).
-define(COORD_X_RIGHT,  3).
-define(COORD_X_LEFT,  -3).
-define(COORD_Z_FRONT,  3).
-define(COORD_Z_BACK,  -3).

-define(COORDS_NORMAL,  [-2, 0, 2]).
-define(COORDS_Y_UP,    [?COORD_Y_UP]).
-define(COORDS_Y_DOWN,  [?COORD_Y_DOWN]).
-define(COORDS_X_RIGHT, [?COORD_X_RIGHT]).
-define(COORDS_X_LEFT,  [?COORD_X_LEFT]).
-define(COORDS_Z_FRONT, [?COORD_Z_FRONT]).
-define(COORDS_Z_BACK,  [?COORD_Z_BACK]).

-define(COORD_U1, {-2, 3, -2}).
-define(COORD_U2, {0, 3, -2}).
-define(COORD_U3, {2, 3, -2}).
-define(COORD_U4, {-2, 3, 0}).
-define(COORD_U5, {0, 3, 0}).
-define(COORD_U6, {2, 3, 0}).
-define(COORD_U7, {-2, 3, 2}).
-define(COORD_U8, {0, 3, 2}).
-define(COORD_U9, {2, 3, 2}).

-define(COORD_D1, {-2, -3, 2}).
-define(COORD_D2, {?COORD_X_D2, ?COORD_Y_D2, ?COORD_Z_D2}).
-define(COORD_D3, {2, -3, 2}).
-define(COORD_D4, {?COORD_X_D4, ?COORD_Y_D4, ?COORD_Z_D4}).
-define(COORD_D5, {0, -3, 0}).
-define(COORD_D6, {?COORD_X_D6, ?COORD_Y_D6, ?COORD_Z_D6}).
-define(COORD_D7, {-2, -3, -2}).
-define(COORD_D8, {?COORD_X_D8, ?COORD_Y_D8, ?COORD_Z_D8}).
-define(COORD_D9, {2, -3, -2}).

-define(COORD_R1, {3, 2, 2}).
-define(COORD_R2, {3, 2, 0}).
-define(COORD_R3, {3, 2, -2}).
-define(COORD_R4, {3, 0, 2}).
-define(COORD_R5, {3, 0, 0}).
-define(COORD_R6, {3, 0, -2}).
-define(COORD_R7, {3, -2, 2}).
-define(COORD_R8, {?COORD_X_R8, ?COORD_Y_R8, ?COORD_Z_R8}).
-define(COORD_R9, {3, -2, -2}).

-define(COORD_L1, {-3, 2, -2}).
-define(COORD_L2, {-3, 2, 0}).
-define(COORD_L3, {-3, 2, 2}).
-define(COORD_L4, {-3, 0, -2}).
-define(COORD_L5, {-3, 0, 0}).
-define(COORD_L6, {-3, 0, 2}).
-define(COORD_L7, {-3, -2, -2}).
-define(COORD_L8, {?COORD_X_L8, ?COORD_Y_L8, ?COORD_Z_L8}).
-define(COORD_L9, {-3, -2, 2}).

-define(COORD_F1, {-2, 2, 3}).
-define(COORD_F2, {0, 2, 3}).
-define(COORD_F3, {2, 2, 3}).
-define(COORD_F4, {-2, 0, 3}).
-define(COORD_F5, {0, 0, 3}).
-define(COORD_F6, {2, 0, 3}).
-define(COORD_F7, {-2, -2, 3}).
-define(COORD_F8, {?COORD_X_F8, ?COORD_Y_F8, ?COORD_Z_F8}).
-define(COORD_F9, {2, -2, 3}).

-define(COORD_B1, {2, 2, -3}).
-define(COORD_B2, {0, 2, -3}).
-define(COORD_B3, {-2, 2, -3}).
-define(COORD_B4, {2, 0, -3}).
-define(COORD_B5, {0, 0, -3}).
-define(COORD_B6, {-2, 0, -3}).
-define(COORD_B7, {2, -2, -3}).
-define(COORD_B8, {?COORD_X_B8, ?COORD_Y_B8, ?COORD_Z_B8}).
-define(COORD_B9, {-2, -2, -3}).

-define(COORD_P1, {-2, 2, -2}).
-define(COORD_P2, {0, 2, -2}).
-define(COORD_P3, {2, 2, -2}).
-define(COORD_P4, {-2, 2, 0}).
-define(COORD_P5, {0, 2, 0}).
-define(COORD_P6, {2, 2, 0}).
-define(COORD_P7, {-2, 2, 2}).
-define(COORD_P8, {0, 2, 2}).
-define(COORD_P9, {2, 2, 2}).

-define(COORD_P10, {-2, 0, -2}).
-define(COORD_P11, {0, 0, -2}).
-define(COORD_P12, {2, 0, -2}).
-define(COORD_P13, {-2, 0, 0}).
-define(COORD_P14, {0, 0, 0}).
-define(COORD_P15, {2, 0, 0}).
-define(COORD_P16, {-2, 0, 2}).
-define(COORD_P17, {0, 0, 2}).
-define(COORD_P18, {2, 0, 2}).

-define(COORD_P19, {-2, -2, -2}).
-define(COORD_P20, {0, -2, -2}).
-define(COORD_P21, {2, -2, -2}).
-define(COORD_P22, {-2, -2, 0}).
-define(COORD_P23, {0, -2, 0}).
-define(COORD_P24, {2, -2, 0}).
-define(COORD_P25, {-2, -2, 2}).
-define(COORD_P26, {0, -2, 2}).
-define(COORD_P27, {2, -2, 2}).

%% STICKER WITH COORD AND COLOR
%% -----------------------------------------------------------------------------

%% Very handy: ?STICKER(?COORD_F5, red)

-define(STICKER(XYZ, Color), #sticker{x     = element(1, XYZ),
                                      y     = element(2, XYZ),
                                      z     = element(3, XYZ),
                                      color = Color}).

%% SPECIAL FOR YELLOW CROSS
%% -----------------------------------------------------------------------------

-define(COORD_X_D2, 0).
-define(COORD_Y_D2, -3).
-define(COORD_Z_D2, 2).

-define(COORD_X_D4, -2).
-define(COORD_Y_D4, -3).
-define(COORD_Z_D4, 0).

-define(COORD_X_D6, 2).
-define(COORD_Y_D6, -3).
-define(COORD_Z_D6, 0).

-define(COORD_X_D8, 0).
-define(COORD_Y_D8, -3).
-define(COORD_Z_D8, -2).

-define(YELLOW_STICKER_D2, #sticker{x     = ?COORD_X_D2,
                                    y     = ?COORD_Y_D2,
                                    z     = ?COORD_Z_D2,
                                    color = yellow}).

-define(YELLOW_STICKER_D4, #sticker{x     = ?COORD_X_D4,
                                    y     = ?COORD_Y_D4,
                                    z     = ?COORD_Z_D4,
                                    color = yellow}).

-define(YELLOW_STICKER_D6, #sticker{x     = ?COORD_X_D6,
                                    y     = ?COORD_Y_D6,
                                    z     = ?COORD_Z_D6,
                                    color = yellow}).

-define(YELLOW_STICKER_D8, #sticker{x     = ?COORD_X_D8,
                                    y     = ?COORD_Y_D8,
                                    z     = ?COORD_Z_D8,
                                    color = yellow}).

%% SPECIAL FOR FIXED YELLOW CROSS
%% -----------------------------------------------------------------------------

-define(COORD_X_F8, 0).
-define(COORD_Y_F8, -2).
-define(COORD_Z_F8, 3).

-define(COORD_X_R8, 3).
-define(COORD_Y_R8, -2).
-define(COORD_Z_R8, 0).

-define(COORD_X_B8, 0).
-define(COORD_Y_B8, -2).
-define(COORD_Z_B8, -3).

-define(COORD_X_L8, -3).
-define(COORD_Y_L8, -2).
-define(COORD_Z_L8, 0).

-define(RED_STICKER_F8, #sticker{x     = ?COORD_X_F8,
                                 y     = ?COORD_Y_F8,
                                 z     = ?COORD_Z_F8,
                                 color = red}).

-define(BLUE_STICKER_R8, #sticker{x     = ?COORD_X_R8,
                                  y     = ?COORD_Y_R8,
                                  z     = ?COORD_Z_R8,
                                  color = blue}).

-define(ORANGE_STICKER_B8, #sticker{x     = ?COORD_X_B8,
                                    y     = ?COORD_Y_B8,
                                    z     = ?COORD_Z_B8,
                                    color = orange}).

-define(GREEN_STICKER_L8, #sticker{x     = ?COORD_X_L8,
                                   y     = ?COORD_Y_L8,
                                   z     = ?COORD_Z_L8,
                                   color = green}).

%% RUBIK'S CUBE REPRESENTATION
%% -----------------------------------------------------------------------------

%% Rubik's Cube can be viewed as 54 separate stickers.

-record(rubiks_cube, {stickers :: [sticker()]}).

-type rubiks_cube() :: #rubiks_cube{}.

%% DIFFERENT CUBE REPRESENTATION: 27 PIECES (AKA "CUBIES")
%% -----------------------------------------------------------------------------

-record(piece, {colors = [] :: [color()]}).

-type piece() :: #piece{}.

-type pieces() :: #{ coord() => piece() }.

%% ROTATION DIRECTIONS
%% -----------------------------------------------------------------------------

-define(DIRECTIONS, [u, ui,
                     d, di,
                     r, ri,
                     l, li,
                     f, fi,
                     b, bi]).
