## Functions

### fill/1
Given a matrix M, replaces all unknown cells with a list of the
possible values that could be put there. In other words, if the cell value is
zero it will be replaced with a list of the elements 1 to 9. Otherwise, the
value remains unchanged.

### refine/1
Given a matrix M, filters all cells containing candidates to
only contain valid candidates. A valid candidate means that the digit is not
present in the current row, col or block.

### guess/1
Given a matrix M, returns the position of the cell with the least
number of valid candidates.

### guesses/1
Given a matrix M, returns a list of new matrices where the unknown cell 
returned by ```guess/1``` has been replaced with the possible guesses also
returned by ```guess/1```.

### hard/1
Given a matrix M, returns the sum of possible candidates for all
cells in the matrix. The greater the sum the harder the matrix. For this to
make sense, M should be refined so that the candidates are values that needs to
be guessed.

### solved/1
Checks that the value in every cell is between 1 and 9 (not 0 or list).

### solve_refined/1
Given a matrix M, first checks if M is already solved, otherwise 
calls ```solve_one/1```.

### solve_one/1
Takes a list of partially solved matrices. It utilizes a depth-first approach 
where it tries to solve the head of the list of matrices. If there is no
solution it proceeds with the next element in the list.

### solve/1
Takes an initial matrix, fills the unknown cells with candidates, refines it
and then calls ```solve_refined/1``` for the actual solving.