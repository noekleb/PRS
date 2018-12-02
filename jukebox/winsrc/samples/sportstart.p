SESSION:NUMERIC-FORMAT = "European".
SESSION:SUPPRESS-WARNINGS = YES.

/* &GLOBAL-DEFINE AdvGuiWin  */

RUN jboxappstart.p
  (FALSE,
   "Sports 2000 " + proversion,
   "EN",
   "EN",
   "EN",
   "",
   "bha",
   "."
   ).
quit.
