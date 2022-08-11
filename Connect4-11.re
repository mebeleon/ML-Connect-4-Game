open!  CS17SetupGame;   
open Game; 

module Connect4 = {

  /* a type whichPlayer is either P1 (representing player 1) or player 2 
    (representing player 2), and nothing else */
  type whichPlayer =
    | P1
    | P2;

  /* either a player has won, it's a draw, or it's ongoing 
  a type status is Win(whichPlayer) (where one player has won the game), Draw 
  (where the game has ended and neither player won), or Ongoing(whichPlayer) 
    (where the game is still going on), with whichPlayer being either P1 or P2.
  */
  type status =
    | Win(whichPlayer)
    | Draw
    | Ongoing(whichPlayer);
  
  /* a type token is either blue, red, or empty*/
  type token =
    | Blue
    | Red
    | Empty;

  /* a type matrix is a list of a list of token where a token can either be 
  blue, red, or empty*/
  type matrix = list(list(token));
  
  /* A type state represents everything about the progress of a game including
     how the current board represented, and whose turn is it. It includes a 
     type status of the current game and a matrix of the current board
     */
  type state =
    | State(status, matrix);
  
  /* a type move represents all the possible moves a player might ever be able 
  to make.*/
  type move =
    | Move(int);

  /*
   a procedure used to switch the players
   otherPlayer: whichPlayer => whichPlayer
   input: player, whichPlayer
   output: a whichPlayer in which P1 outputs P2, and vice versa.
   */
    let otherPlayer: whichPlayer => whichPlayer = player => 
      switch (player) {
        | P1 => P2
        | P2 => P1
        };

/* RECURSION DIAGRAMS: getMatrixColumns
================================================================================
OI: 4
RI: 3
RO: [Empty, Empty, Empty]

cons an empty onto ro

OO: [Empty, Empty, Empty, Empty]
================================================================================
OI: 0
RI: N/A
RO: N/A

base case - output empty list

OO: []
================================================================================
getMatrixColumns: int => list(token)
input:
output:
*/

    let rec getMatrixColumns: int => list(token) = n => 
      switch (n) {
        | 0 => []
        | _x => [Empty, ...getMatrixColumns(n - 1)]
        };


/* RECURSION DIAGRAMS: getMatrixRows
================================================================================
OI: (4, 4)
RI: (3, 4)
RO: [[Empty, Empty, Empty, Empty], [Empty, Empty, Empty, Empty], 
[Empty, Empty, Empty, Empty]]

only switch on width 
call getMatrixColumns on first input to get list of empties
cons this onto ro

OO: [[Empty, Empty, Empty, Empty], [Empty, Empty, Empty, Empty], 
[Empty, Empty, Empty, Empty], [Empty, Empty, Empty, Empty]]
================================================================================
OI: (1, 2)
RI: (0, 2)
RO: []

base case - 0 should output empty list
recur on first input - 1, and SAME SECOND INPUT

OO: [[Empty, Empty]]
================================================================================
     a procedure that makes the rows of a matrix
     getMatrixRows: (int,int) => matrix
     getMatrixRows input: a width and a height represented as two ints
     getMatrixRows output: a matrix is as a nonempty list of nonempty lists of
        numbers, where each list represents a row of the matrix.
     */

    let rec getMatrixRows: (int, int) => matrix = (width, height) =>
      switch (width) {
        | 0 => []
        | _x => [getMatrixColumns(height), ...getMatrixRows(width - 1, height)]
      }



  /* a procedure that takes in an initializing string, and produces a state.
     initialState: string => state
     intialState input: a string
     intialState output: a state which takes in a status and a matrix
     */

    let initialState: string => state =
      s => {
        let boardDims = parseBoardDims(s);
        let boardHeight = getBoardHeight(boardDims);
        let boardWidth = getBoardWidth(boardDims);

        State(Ongoing(P1), getMatrixRows(boardWidth, boardHeight))
        /* your initial state, using boardHeight and boardWidth, goes here */
      };

/* RECURSION DIAGRAMS: counter
================================================================================
OI: (State(Ongoing(P1), [[Empty, Red, Red], [Red, Blue, Blue], 
  [Empty, Red, Blue]]), 1)
RI: (State(Ongoing(P1), [[Red, Blue, Blue], [Empty, Red, Blue]]), 2)
RO: [Move(3)]

always call counter with 1
we do not care about status (stays the same), so use let to switch on matrix
recursion should add 1 to inputted in

OO: [Move(1), Move(3)]
================================================================================
OI: (State(Ongoing(P2), [[Red, Blue, Red], [Red, Blue, Blue], 
[Empty, Empty, Red], [Empty, Red, Blue], [Empty, Empty, Empty]]), 1)
RI: (State(Ongoing(P2), [[Empty, Empty, Red], [Red, Blue, Blue],
[Empty, Red, Blue], [Empty, Empty, Empty]]), 2)
RO: [Move(2), Move(4), Move(5)]

REPRESENT TOP OF COLUMN W FIRST ELEMENT OF LIST
pattern match - if first item in list is empty, add move(int) to list then recur
else recur (w/o adding move to list)
counter is number of columns - stop when you get to empty list

OO: [Move(2), Move(4), Move(5)]
================================================================================
     a helper for legal moves that shows the different types of moves available
        counter: (state, int) => list(move)
        counter input: a state which takes in a status and a matrix and an int
        represented as (s,i)
        counter output: a list of move represented as list(move)
     */

    let rec counter: (state, int) => list(move) = (s, i) =>
    { let State(x, y) = s;
    switch(y) {
      | [] => []
      | [[Empty, ..._tl], ... tl] => [Move(i), ... counter(State(x, tl), i + 1)]
      | [[_, ..._tl], ... tl] => counter(State(x, tl), i + 1)
    }
    };

  /* a procedure that creates a list of all the legal moves at a given state.
     legalMoves: state => list(move)
     legalMoves input: a state which takes in a status and a matrix
     legalMoves ouput: a list of move represented as list(move)
     */

    let legalMoves: state => list(move) = s => 
      counter(s, 1)



    /*
Recursion Diagrams
================================================================================
OI: [[Empty, Red, Red], [Red, Blue, Blue], [Empty, Red, Blue]]
RI: [[Blue, Blue], [Red, Blue]]
RO: [[Blue, Red], [Blue, Blue]]

map each element of sublist to become head of each sublist

OO: [[Empty, Red, Empty], [Red, Blue, Red], [Red, Blue, Blue]]
================================================================================
OI: [[Empty], [Red], [Blue]]
RI: [[Red], [Blue]]
RO: [[Red, Blue]]

For a matrix of one element lists, all elements of mat should be appended(?)
in one list

OO: [[Empty, Red, Blue]]
================================================================================

transpose: matrix => matrix
input: mat, a matrix 
output: a matrix where the element in the i-th row and j-th column of the output
is the same as the element in the j-th row and i-th column of mat
*/

    let rec transpose: matrix => matrix = mat =>
        switch (mat) {
            | []
            | [[], ..._] => failwith("A matrix cannot be 0-dimensional.")
            | [[_], ..._] => [List.concat(mat)]
            | [[_, ..._], ..._] => [List.map(List.hd, mat), ... 
                transpose(List.map(List.tl, mat))]
        };

/* RECURSION DIAGRAMS: diagTranspose
================================================================================
OI: [[Empty, Empty, Blue, Red], [Empty, Empty, Empty, Blue, Red], 
[Empty, Empty, Red, Red, Blue, Blue],[Empty, Empty, Empty, Red, Red, Red, Blue]]
RI: [[Empty, Empty, Empty, Blue, Red], [Empty, Empty, Red, Red, Blue, Blue], 
[Empty, Empty, Empty, Red, Red, Red, Blue]]
RO:



OO: 
================================================================================
OI: [[Empty, Blue, Red]]
RI: 
RO: [[Empty], [Blue], [Red]]




OO:
================================================================================
      diagtranspose: matrix => matrix
          diagtranspose input: mat, a matrix
          diagtranspose output: a matrix where the element in the i-th row and
          j-th column of the output is the same as the element in the j-th row 
          and i-th column of mat
     */

   let rec diagTranspose: matrix => matrix = mat =>
    switch (mat) {
        | [] => []
        | [[], ...tl] => diagTranspose(tl)
        | [[_, ..._], ..._] => [List.map(List.hd, mat), ... 
            diagTranspose(List.map(List.tl, mat))]
    };

  /*
   a procedure that returns the status of the game at the given state
   gameStatus: state => status
   gameStatus input: a state which takes in a status and a matrix represented as
   s
   gameStatus output: a status of the game which tells if either a player has
   won, it's a draw, or it's ongoing
   */

    /* returns the status of the game at the given state */
    let gameStatus: state => status = s => {
      let State(x, y) = s;
       x };


/* RECURSION DIAGRAMS: addTokenToColumn
================================================================================
OI: (P2, [Empty, Empty, Red])
RI: (P2, [Empty, Red])
RO: [Empty, Blue, Red]

Cannot completely get rid of emptys because there might not be any more spaces
cases: ET, EE (cons Empty then recur on [E,...tl]), TT (error)
give failwith if column is full (legalMoves should prevent this)

OO: [E]
================================================================================
OI: (P1, [Empty, Blue, Blue, Blue])
RI: n/a
RO: n/a

ET - add token - separate for each player
ro is oo

OO: [Red, Blue, Blue, Blue]
================================================================================
     a procedure that adds tokens to columns
          addTokenToColumn: (whichPlayer, list(token)) => list(token)
          addTokentoColumn input: whichPlayer and list(token) represented as
          (player,mat)
          addTokentoColumn output: a list of tokens which are either blue, red 
          or empty
     */

    let rec addTokenToColumn: (whichPlayer, list(token)) => list(token) = 
    (player, mat) =>
      switch(player, mat) {
        | (P1, [Empty]) => [Red]
        | (P2, [Empty]) => [Blue]
        | (_plyr, [Empty, Empty, ...tl]) => 
            [Empty, ...addTokenToColumn(player, [Empty, ...tl])]
        | (P1, [Empty, tok, ...tl]) => [Red, tok, ...tl]
        | (P2, [Empty, tok, ...tl]) => [Blue, tok, ...tl]
        | (_, [Red, ... tl]) => 
            failwith("You cannot insert a token into a full column")
        | (_, [Blue, ... tl]) => 
            failwith("You cannot insert a token into a full column")
      };

/* RECURSION DIAGRAMS: newMatrixWToken
================================================================================
OI: (P1, Move(3), [[Empty, Red, Red], [Red, Blue, Blue], [Empty, Red, Blue]])
RI: (P1, Move(2), [[Red, Blue, Blue], [Empty, Red, Blue]])
RO: [[Red, Blue, Blue], [Red, Red, Blue]]

need helper to add token to column
recur on tl of matrix
decrease int of move by 1 when recurring

OO: [[Empty, Red, Red], [Red, Blue, Blue], [Red, Red, Blue]]
================================================================================
OI: (P2, Move(1), [[Empty, Empty, Red], [Empty, Red, Blue], [Empty, Blue, Red]])
RI: n/a
RO: n/a

Move(1) is base case where helper is used - addTokenToColumn
Move(1) means you got to the column you want to insert the token in
Call helper and wrap in list
helper should output a list(token) not a matrix
then cons helper onto tl of original matrix

OO: [[Empty, Red, Red], [Empty, Red, Blue], [Empty, Blue, Red]]
================================================================================
     a procedure that produces a new matrix with the new added tokens
        newMatrixWToken: (whichPlayer, move, matrix) => matrix
        newMatrixWToken input: whichPlayer , a move, and a matrix
        represented as (player, m, mat)
        newMatrixWToken output: a matrix
     */

    let rec newMatrixWToken: (whichPlayer, move, matrix) => matrix = 
    (player, m, mat) 
      => switch(player, m, mat) {
          | (plyr, Move(1), [[Empty, ...tl], ...tl2]) => 
              [addTokenToColumn(plyr, [Empty, ...tl]), ...tl2]
          | (_, Move(n), [[_], ...tl]) => newMatrixWToken(player, Move(n-1), tl)
      };


  /* WIN PROCS */

/* RECURSION DIAGRAMS: addZeroes
================================================================================
OI: [[Empty, Empty, Blue, Red], [Empty, Empty, Blue, Red], 
[Red, Red, Blue, Blue], [Red, Red, Red, Blue], [Empty, Empty, Red, Blue], 
[Empty, Empty, Blue, Red]]
RI: [[Empty, Empty, Blue, Red], [Red, Red, Blue, Blue], [Red, Red, Red, Blue], 
[Empty, Empty, Red, Blue], [Empty, Empty, Blue, Red]]
RO: [[Empty, Empty, Empty, Blue, Red], [Empty, Empty, Red, Red, Blue, Blue], 
[Empty, Empty, Empty, Red, Red, Red, Blue], 
[Empty, Empty, Empty, Empty, Empty, Empty, Red, Blue], 
[Empty, Empty, Empty, Empty, Empty, Empty, Empty, Blue, Red]]

base case: empty matrix => empty matrix
cons hd of list onto ro
explicitly write out cons of tail then recur on the tail's tail

OO: [[Empty, Empty, Blue, Red], [Empty, Empty, Empty, Blue, Red], 
[Empty, Empty, Red, Red, Blue, Blue], 
[Empty, Empty, Empty, Red, Red, Red, Blue], 
[Empty, Empty, Empty, Empty, Empty, Empty, Red, Blue], 
[Empty, Empty, Empty, Empty, Empty, Empty, Empty, Blue, Red]]
================================================================================
OI: [[Empty, Red, Blue]]
RI: N/A
RO: N/a

BASE CASE => just output matrix if only single element
pattern match and write out both tls (tl of first element in mat and tl of mat)
map an empty onto tl of matrix and recur on tl of matrix 

OO: [[Empty, Red, Blue]]
================================================================================
a procedure that adds zeros to the diagonals
          addZeros input: a matrix represented as mat
          addZeros output: a new matrix with the added zeros
          */  

   let rec addZeroes: matrix => matrix = mat => 
      switch(mat) {
        | [] => []
        | [[hd, ... tl]] => mat
        | [[hd, ... tl1], ...tl2] => 
            [[hd, ... tl1], ... List.map(x=>[Empty, ...x], addZeroes(tl2))]
      };


/* RECURSION DIAGRAMS: vertFourInARow
================================================================================
OI: ([[Empty, Red, Red, Blue], [Blue, Red, Red, Blue], [Red, Red, Red, Red]]
RI: [[Red, Red, Blue], [Blue, Red, Red, Blue], [Red, Red, Red, Red]], P1)
RO: Some(P1)

base case: three element list in matrix => return none
Pattern match - if it starts with anything other than 4 tokens in a row, recur
RO is OO

OO: 
================================================================================
OI: ([[Blue, Red, Red, Red, Red], [Blue, Blue, Red, Blue, Blue], 
[Blue, Red, Blue, Blue, Blue], [Red, Blue, Blue, Red, Red]]
RI: [[Red, Red, Red, Red], [Blue, Blue, Red, Blue, Blue], 
[Blue, Red, Blue, Blue, Blue], [Red, Blue, Blue, Red, Red]], P1)
RO: Some(P1)

recur both on tail of first column of matrix + tail of matrix 
also recur when hd list is empty 

OO: Some(P1)
================================================================================
     a procedure that checks to see if a player has a vertical FourinARow
        vertFourInARow: (matrix, whichPlayer) => option(whichPlayer)
        vertFourInARow input: a matrix and whichPlayer represented as 
          (matrix,player)
        vertFourInARow output: the player that has a vertical FourinARow 
        represented as option(whichPlayer)
     */

    let rec vertFourInARow: (matrix, whichPlayer) => option(whichPlayer) = 
    (matrix, player) =>
        switch (player, matrix) {
          | (_,[])
          | (_, [[_,_,_]])=> None
          | (_, [[Red, Red, Red, Red, ... _], ... _]) => Some(P1)
          | (_, [[Blue, Blue, Blue, Blue, ... _], ... _]) => Some(P2)
          | (a, [[_, ... tl], ... tl2]) => vertFourInARow([tl, ... tl2],a)
          | (a, [[], ... tl]) => vertFourInARow(tl,a)
        };

  /* a procedure that checks to see if a player has a horizontal four in a row
       horizonFourInARow: (matrix, whichPlayer) => option(whichPlayer)
       horizonFourInARow input: a matrix and whichPlayer represented as
       (matrix,whichPlayer)
       horizonFourInARow output: the player that has a horizontal FourinARow
       represented as option(whichPlayer)
     */
    let horizonFourInARow: (matrix, whichPlayer) => option(whichPlayer) = 
    (mat, player) =>
      vertFourInARow(transpose(mat), player);

      /*
     a procedure that checks if there are four tokens in a row from the upper
     right to lower left
     diagNEtoSWP: (matrix, whichPlayer) => option(whichPlayer)
     diagNEtoSWP input: a matrix and whichPlayer represented as
     (matrix,whichPlayer)
     diagNEtoSWP output: the player that has a a diagnal FourinARow from the
     upper right to lower left represented as option(whichPlayer)
   */
    let diagNEtoSWP: (matrix, whichPlayer) => option(whichPlayer) = 
    (mat, player) =>
      vertFourInARow(diagTranspose(addZeroes(mat)), player);

  /*
     a procedure that checks if there are four tokens in a row from upper left
     to lower right
     diagNWtoSEP: (matrix, whichPlayer) => option(whichPlayer)
     diagNWtoSEP input: a matrix and whichPlayer represented as
     (matrix,whichPlayer)
     diagNWtoSEP output: the player that has a a diagnal FourinARow from the
     upper left to lower right represented as option(whichPlayer)
   */

    let diagNWtoSEP: (matrix, whichPlayer) => option(whichPlayer) = 
    (mat, player) =>
      vertFourInARow(diagTranspose(addZeroes(List.rev(mat))), player);

  /*
      Given a state and a move, produces the state that results from making that move.
      nextState: (state, move) => state
      nextState input: a state and a move, represented as (s,m)
      nextState output: a state which takes in a status and a matrix
   */
    let nextState: (state, move) => state = 
      (s, m) =>
        switch (s, m) {
        | (State(Win(_), _), Move(_)) 
        | (State(Draw(_), _), Move(_)) => s
        | (State(Ongoing(p), mat), mov) => 
            if 
            (vertFourInARow(mat, p) == Some(p) 
            || horizonFourInARow(mat, p) == Some(p)
            || diagNEtoSWP(mat, p) == Some(p) 
            || diagNWtoSEP(mat, p) == Some(p))
            { State(Win(p), mat) } 
            else
            { State(Ongoing(otherPlayer(p)), newMatrixWToken(p, mov, mat))}  
    };


  /*
   a procedure that returns the string of player
   stringOfPlayer: whichPlayer => string
   stringOfPlayer input: whichPlayer either P1 and P2
   stringOfPlayer output: a string
    */

    let stringOfPlayer: whichPlayer => string = player =>
     switch (player) {
       | P1 => "P1"
       | P2 => "P2"
     };

  /*
   a procedure that returns the string of player's turn
   stringOfState: state => string
   stringOfState input: s, the current state 
   stringOfPlayer output: a string representing the current board
    */
    let stringOfState: state => string = s => {
      let State(x, y) = s;

       let stringOfToken: token => string = tok =>
      switch(tok) {
        | Red => "Red"
        | Blue => "Blue"
        | Empty => "--"
      };
        
    let rec stringOfRow: list(token) => string = lot => 
      switch (lot) {
        | [] => ""
        | [hd, ...tl] => stringOfToken(hd) ++ stringOfRow(tl)
      };
      
    let rec stringOfBoard: matrix => string = mat =>
      switch(mat) {
        | [] => ""
        | [[hd, ...tl1], ...tl2] => ("[" ++ stringOfRow([hd, ...tl1]) ++ "] \n")  
              ++ stringOfBoard(tl2)
      };
      
      "Current Board: \n" ++ stringOfBoard(transpose(y))
        };

    /*
    stringOfMove: move => string
    input: mov, move
    output: string "add token to column " with int of move at the end
    */
    let stringOfMove: move => string = mov => {
      let Move(x) = mov;
      "add token to column " ++ string_of_int(x) };

    /* for transforming human player input into
    internal representation of move */
    let moveOfString: (string, state) => move = (str, s) =>
      Move(int_of_string(str));

  /*
    RECURSION DIAGRAMS: vertValue
      ================================================================================
      OI: [Empty, Red, Red, Red]
      RI: n/a
      RO: 10000
     recur through the rest of the list to see if a player is close to winning

      OO: 10000
      ================================================================================
      OI: [Empty, Empty, Blue, Red]
      RI: [Empty, Blue, Red]
      RO: 0
     recur through the rest of the list to see if a player is close to winning
      OO: 0
      ================================================================================
      vertValue: list(token) => float
      vertValue input: a list token which can either be red, blue, or empty
      vertValue output: a float
   */
  

  /*
    RECURSION DIAGRAMS: vertChecker
      ================================================================================
      OI: [[Empty, Red, Red, Red]]
      RI: [[Red, Red, Red]]
      RO: 10000
     recur through the rest of the list to see if a player is close to winning

      OO: 10000
      ================================================================================
      OI: [[Empty, Red, Blue, Red]]
      RI: [[Red, Blue, Red]]
      RO: 0
     recur through the rest of the list to see if a player is close to winning
      OO: 0
      ================================================================================
      vertChecker: matrix => float
      vertChecker input: a matrix
      vertChecker output: a float
   */
 

  /*
    RECURSION DIAGRAMS: horizontalValue
      ================================================================================
      OI: [Empty, Red, Red, Red, Empty]
      RI: [Red, Red, Red, Empty]
      RO: 10000
     recur through the rest of the list to see if a player is close to winning
      OO: 10000
      ================================================================================
      OI: [Empty, Blue, Blue, Blue, Empty]
      RI: [Blue, Blue, Blue, Empty]
      RO: -10000
     recur through the rest of the list to see if a player is close to winning
      OO: -10000
      ================================================================================
      horizontalValue: list(token) => float
      horizontalValue input: a list token which can either be red, blue, or empty
      horizontalValue output: a float
   */
  

  /*
    RECURSION DIAGRAMS: horizChecker
      ================================================================================
      OI: [[Empty, Red, Red, Red, Empty]]
      RI: [[Red, Red, Red, Empty]]
      RO: 10000
     recur through the rest of the list to see if a player is close to winning
      OO: 10000
      ================================================================================
      OI: [[Empty, Blue, Blue, Blue, Empty]]
      RI: [[Blue, Blue, Blue, Empty]]
      RO: -10000
     recur through the rest of the list to see if a player is close to winning
      OO: -10000
      ================================================================================
      horizChecker: matrix => float
      horizChecker input: a matrix
      horizChecker output: a float
   */
 

  /* estimates the value of a given state (static evaluation) */
let rec estimateValue: state => float =
    st =>
      switch (st) {
      | State(Win(P1), _) => 5000.
      | State(Win(P2), _) => (-5000.)
      | State(Draw, _) => 0.
      | State(Ongoing(P1), x) => {
      let rec vertValue: list(token) => float = t =>
      switch (t) {
      | [Empty, Red, Red, Red] => 10000.
      | [Empty, Blue, Blue, Blue] => (-10000.)
      | [Empty, Empty, ...tl] => 0.0 +. vertValue([Empty, ...tl])
      | _ => 0.0
      };
      let rec vertChecker: matrix => float = m =>
      switch (m) {
      | [] => 0.0
      | [hd, ...tl] => vertValue(hd) +. vertChecker(tl)
      }; 
      let rec horizontalValue: list(token) => float = t =>
      switch (t) {
      | [Empty, Red, Red, Red, Empty, ...tl] =>
        10000. +. horizontalValue([Empty, ...tl])
      | [Empty, Blue, Blue, Blue, Empty, ...tl] =>
        (-10000.) +. horizontalValue([Empty, ...tl])
      | [Empty, Red, Red, Red, ...tl] =>
        5000. +. horizontalValue([Empty, ...tl])
      | [Empty, Blue, Blue, Blue, ...tl] =>
        (-5000.) +. horizontalValue([Empty, ...tl])
      | [Red, Empty, Red, Red, ...tl] =>
        5000. +. horizontalValue([Empty, ...tl])
      | [Red, Red, Empty, Red, ...tl] =>
        5000. +. horizontalValue([Empty, ...tl])
      | [Blue, Empty, Blue, Blue, ...tl] =>
        5000. +. horizontalValue([Empty, ...tl])
      | [Blue, Blue, Empty, Blue, ...tl] =>
        5000. +. horizontalValue([Empty, ...tl])
      | [Empty, ...tl] => 0.0 +. horizontalValue(tl)
      | [_, ...tl] => 0.0 +. horizontalValue(tl)
      | _ => 0.0
      };
      let rec horizChecker: matrix => float = m =>
      switch (m) {
      | [] => 0.0
      | [hd, ...tl] => horizontalValue(hd) +. horizChecker(tl)
      };
  3.0


}}}



module MyGame : Game = Connect4;
open Connect4;

/* test cases */
  checkExpect(otherPlayer(P2), P1, "switch player P2");
  checkExpect(otherPlayer(P1), P2, "switch player P1");

