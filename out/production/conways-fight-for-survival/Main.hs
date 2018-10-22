module Main where
import Lib

main :: IO ()
main = print $ show $ 1 + 1

{-|
 Title screen, game setup(# of players, # of cells, # of iterations per round, etc)

 deployment phase

 fixed number of iterations run

 are all of one player's cells dead? then they're out

 is there only one player remaining? IS EVERYBODY DEAD?? then the game is over

 if not, players add reinforcements (additional cells)

 game runs again, perhaps with an increaing number of iterations

 repeat until one player remains or everyone is dead

 game logic - needs to be modified - how to handle a newly spawned cell from adjacent cells belonging to different
 players? should it be a hybrid cell that belongs to both players?

 should there be rewards in the form of bonus reinforcements for players
 whose cells "kill" a certain number of enemy cells? what about killing their own cells? or creating a stable
 entity that survives for a certain number of turns?

 should there be "special" cells? e.g. indestructable for a certain number of iterations.

 is the deployment phase turn based? (probably best)

 there are some additional Haskell packages that will be useful for us
 are we allowed to use them? ie.https://github.com/mhwombat/grid
-}