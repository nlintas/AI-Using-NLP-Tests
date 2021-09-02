:-['Lab05_ID3_ALGORITHM.pl'].
/** Author: Nikolaos Lintas from University of Sheffield */
/*-------------------------------------------------------------------------------
  Example for applying ID3 in order to produce if-then rules.

  example/3:
  - number id for sample
  - outcome (the "then" part of the resulting if-then rule)
  - [attr1=val1, attr2=val2, ...] all attributes and their corresponding values
---------------------------------------------------------------------------------*/


example(1, good_candidate,   [current_debts=high,     income=high, married=yes]).
example(2, bad_candidate,   [current_debts=low,     income=high, married=no]).
example(3, good_candidate,   [current_debts=low,     income=high, married=yes]).
example(4, bad_candidate,   [current_debts=high,     income=low, married=yes]).
example(5, bad_candidate,   [current_debts=low,     income=low, married=yes]).

/*-------------------------------------------------------------------------------
  son/2:
  - the value (one by one)
  - ?
---------------------------------------------------------------------------------*/

son(high,?).
son(low,?).

son(yes,?).
son(no,?).

