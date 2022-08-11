open! CS17SetupGame;
open Game; 

module AIPlayer = (MyGame: Game) => {
  module PlayerGame = MyGame

  /*
   Recursion Diagrams
   =============================================================================
   OI: P1, [9.0, 10.0, 3.0, 11.0]
   RI: P1, [10.0, 3.0, 11.0]
   RO: 11.0

   take max or min depending on if p1 or p2, respectively

   OO: 10.0
   =============================================================================
   OI: P2, [100.0]
   RI: n/a
   RO: n/a

  for single element list, output x
  recur on min/max, consed onto tail

   OO: 100.0
   =============================================================================
    argMinMax: whichPlayer * list(float) => float
    input: player, whichPlayer indicating which player's turn it currently is
          lof, a list of floats
    output: the biggest float in lof when player is P1, and the smallest float 
      in lof when player is P2
*/

  let rec argMinMax: (PlayerGame.whichPlayer, list(float)) => float = 
    (player, lof) =>
      switch (player, lof) {
        | (P1, [x, y,...tl]) => argMinMax(PlayerGame.P1, [max(x, y), ...tl])
        | (P1, [x]) => x 
        | (P2, [x, y,...tl]) => argMinMax(PlayerGame.P2, [min(x, y), ...tl])
        | (P2, [x]) => x
        | _ => failwith("Empty list")
      };

  /*
   Recursion Diagrams
   =============================================================================
   OI: (-1000.0, [-1000.0, 0.0, -100.0], [Move(1), Move(2), Move(3)])
   RI: n/a
   RO: n/a

   if first element of lof is equal to target, output first element of lom
   
   OO: Move(1)
   =============================================================================
   OI: (7.0, [8.0, 9.0, 3.0, 7.0], [Move(2), Move(4), Move(1), Move(6)])
   RI: (7.0, [8.0, 9.0, 3.0, 7.0], [Move(2), Move(4), Move(1), Move(6)])
   RO: Move(6)

  target stays the same
  recur on tails of lof and lom
  ro is oo

   OO: Move(6)
   =============================================================================
    getMove: float * list(float) * list(move) => move
    input: target, the float in lof that is being extracted
           lof, a list of floats 
           lom, a list of moves
    output: the move in the same place in lom as the corresponding float in lof
*/

  let rec getMove: (float, list(float), list(PlayerGame.move)) => 
    PlayerGame.move = (target, lof, lom) =>
      switch(lof, lom) {
        | ([x], [y]) => y
        | ([x, ...tl1], [y, ...tl2]) => 
          if (target == x) { y } else { getMove(target, tl1, tl2) }
        };

  /*
   Recursion Diagrams
   =============================================================================
   OI: [[], [], []]
   RI:
   RO:
   OO:
   =============================================================================
   OI:
   RI:
   RO:
   OO:
   =============================================================================
    minimax: state * int => float
    input: 
    output: 
*/

    let rec listOfStates: (PlayerGame.state, int) => list(PlayerGame.state) = 
    (s, n) => switch (n) {
      | 0 => []
      | _ => [s, ...listOfStates(s, n-1)]
    }

    let rec minimax: (PlayerGame.state, int) => float = (s, n)  => 
      if (n == 0)
        { PlayerGame.estimateValue(s) }
      else {
          if (PlayerGame.gameStatus(s) == PlayerGame.Ongoing(P1)) {
            argMinMax(PlayerGame.P1, List.map((x) => minimax(x, n-1), 
              List.map2(PlayerGame.nextState, 
                listOfStates(s, List.length(PlayerGame.legalMoves(s))),
                PlayerGame.legalMoves(s)))) }
          else {
            argMinMax(PlayerGame.P2, List.map((x) => minimax(x, n-1), 
              List.map2(PlayerGame.nextState, 
                  listOfStates(s, List.length(PlayerGame.legalMoves(s))),
                  PlayerGame.legalMoves(s))))
          }; 
        };

  let nextMove: (PlayerGame.state => PlayerGame.move) = s => {
    getMove(minimax(s, 4), 
            List.map((x) => minimax(x, 4), 
              List.map2(PlayerGame.nextState, 
                    listOfStates(s, List.length(PlayerGame.legalMoves(s))), 
                    PlayerGame.legalMoves(s))),
                      PlayerGame.legalMoves(s))
    // consider player
    // minimax to get list of floats
    // get list of moves 
  };
  // list of moves, list of floats

  /* put your team name here! */
  let playerName = "boi";
  
};

module TestGame = Connect4.Connect4;
open Player;

module TestAIPlayer = AIPlayer(TestGame); 
module MyAIPlayer:Player = TestAIPlayer;
open TestAIPlayer; 

/* insert test cases for any procedures that don't take in 
 * or return a state here */
