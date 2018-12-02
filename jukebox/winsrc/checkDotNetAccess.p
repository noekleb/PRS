
DEF OUTPUT PARAM obDotNetAccess AS LOG NO-UNDO.

DEFINE VARIABLE windowContainer1 AS Progress.Windows.WindowContainer NO-UNDO.

windowContainer1 = NEW Progress.Windows.WindowContainer() NO-ERROR.

IF VALID-OBJECT(windowContainer1) THEN DO:
  obDotNetAccess = YES.  
  DELETE OBJECT windowContainer1.
END.

