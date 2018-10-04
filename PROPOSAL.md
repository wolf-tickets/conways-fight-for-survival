
# Functional Programming Project Proposal


## CPSC 312 - Winter 2018 Term


#### *Team Members: Al Smith, Tristan MacKinlay, Rob Willoughby*

___

We propose to implement a simple graphical multiplayer game in a purely functional programming paradigm, using Haskell. 
The following synopsis assumes some familiarity with 
[Conway's Game Of Life](https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life), upon which the game is based.

The basic logic of Conway's cellular automaton will remain intact, but there is an added competitive aspect: each 
player will initially deploy a limited number of cells (identified by colour or some other graphical element). The game 
will then run for a fixed number of iterations in a modified version of the classic Game Of Life rules. 
After this, if a player's cells have not been completely eliminated, they will have 
an opportunity to deploy reinforcements and try to survive another set of game iterations. The iterations stage may 
become longer as the game goes on, requiring greater foresight and planning to survive. The winner is the last primitive
civilization standing. If time permits, we would like to include optional AI opponents.

Possible working titles include "Conway's Game Of Death", "Conway's Fight For Survival", "Conway's Primordial Rage in 
the Cage", and "Conway's Horrible Darwinian Nightmare".

The purpose of this project is to investigate the feasibility of a real-time, graphical, multiplayer game implemented 
entirely in Haskell, using simultaneous, persistent IO and somewhat sophisticated graphics.
