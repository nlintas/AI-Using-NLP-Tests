:-['Lab04_RULES_LIBRARY.pl'].
:-['Lab04_FRAMES_LIBRARY.pl'].
/** Author: Nikolaos Lintas from University of Sheffield */

/*---------------------------------------------------------------------

"Anyone who passes AI and wins the lottery is happy. Anyone who studies a topic or she is lucky can pass her exams in this topic. Anyone who is lucky wins the lottery."

Rules Syntax:

if predicate_1 and predicate_2 then conclusion_1.
if predicate_3 and predicate_4 then conclusion_2.
etc.


---------------------------------------------------------------------*/

/*---------------------------------------------------------------------
  KNOWLEDGE REPRESENTION
---------------------------------------------------------------------*/

% PUT THE RULES HERE
if pass(ai,Anyone) and wins(lottery, Anyone) then happy(Anyone).
if studies(topic, Anyone) or lucky(Anyone) then pass(ai,Anyone).
if lucky(Anyone) then wins(lottery, Anyone).

if not_studies(Anyone, ai) and lucky(Anyone) then happy(Anyone).
/*---------------------------------------------------------------------
 INITIAL DATA (OBSERVATIONS)
---------------------------------------------------------------------*/
not_studies(ioanna,ai).
lucky(ioanna).

studies(ai, ioanna).
