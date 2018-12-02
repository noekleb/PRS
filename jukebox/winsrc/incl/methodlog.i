/* Check if a call to this method should appear on the method call log: */

IF CAN-DO(DYNAMIC-FUNCTION("getAttribute",SESSION,"methodloglist"),ENTRY(1,PROGRAM-NAME(1)," ")) THEN DO:
  IF DYNAMIC-FUNCTION("getAttribute",SESSION,"methodlogfile") NE "" THEN DO:
    OUTPUT TO VALUE(DYNAMIC-FUNCTION("getAttribute",SESSION,"methodlogfile")) APPEND.
    PUT UNFORMATTED STRING(TODAY) "-" STRING(TIME,"HH:MM:SS") " ** " PROGRAM-NAME(1) "  Called by: " PROGRAM-NAME(2) "  " {1} " Etime: " ETIME / 1000.
    PUT UNFORMATTED SKIP. 
    ETIME(YES).
    OUTPUT CLOSE.
  END.
  ELSE MESSAGE PROGRAM-NAME(1) SKIP
               "Called by: " PROGRAM-NAME(2)
               VIEW-AS ALERT-BOX INFORMATION.
END.
