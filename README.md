# HTGL - Haskell Text Games Library

This library provides a DSL for describing the rules of a multiplayer game. Games described
this way are portable, and can be played with friends over different media, using game drivers.
The library can take of concerns like saving the state of a game and resuming it later, handling 
players leaving and reconnecting, undoing moves, etc.

## Implementing Games

Import `HTGL.Play` to use the `Play` monad. The `Play` monad allows you to:
 - Send public messages to the entire table, or private messages to a single player
 - Wait for a decision from a player
 - Obtain randomness via the MonadRandom methods
 - Attach a timeout 

Import `HTGL.Dice` for a number of intuitive wrappers for rolling combinations of standard dice.

Sample games live under `HTGL.Game.*`.

## Implementing Drivers

Import `HTGL.Run`. The module provide functions to execute a game. The driver is responsible
for broadcasting public game messages and sending private messages to the correct player. When
the game requests input, the driver is also responsible for passing user input to the game
(or the lack thereof, if a timeout has been reached)

Sample drivers live under `HTGL.Run.*`.



*[DSL]: Domain Specific Language
