import Data.List
import Utilities
import Neural
import Genetics

newtype Board = Board [[Piece]]
data Piece = Player | Enemy | Empty

exampleBoard :: Board
exampleBoard = Board [[Empty, Empty, Empty, Empty, Empty, Enemy], 
                [Empty, Empty, Empty, Empty, Empty, Empty], 
                [Empty, Empty, Empty, Empty, Empty, Empty], 
                [Empty, Empty, Empty, Empty, Empty, Empty], 
                [Empty, Empty, Empty, Empty, Player, Player], 
                [Empty, Empty, Empty, Empty, Empty, Empty], 
                [Empty, Empty, Empty, Empty, Empty, Empty]]

instance Show Piece where
    show Empty = "."
    show Enemy = "X"
    show Player = "O"

instance Show Board where
    show (Board b) = transpose b
                    & ((map.map) show )
                    & map concat
                    & unlines
