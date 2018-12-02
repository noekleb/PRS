DEF VAR gLoop AS INT NO-UNDO.
DEF VAR gStr AS CHAR NO-UNDO.
DEF VAR gAnt AS INT  NO-UNDO.


ASSIGN
    CURRENT-WINDOW:WIDTH = 200.
FIND BestHode NO-LOCK WHERE besthode.bestnr = 327.  /* 327 */

FOR EACH BestSort OF BestHode NO-LOCK:
  DISPLAY
      BestSort.Fri
      BestSort.Fordeling
      BestSort.Storrelser
      WITH WIDTH 198.

  DO gLoop = 1 TO NUM-ENTRIES(BestSort.Storrelser," "):
      ASSIGN
        gStr = TRIM(ENTRY(gLoop,BestSort.Storrelser," "))
        gAnt = INT(ENTRY(gLoop,BestSort.Fordeling," ")).
      
      /* Ta bort dette hvis det ikke er formaterte størrelser */
      RUN FiksStorl (INPUT-OUTPUT gStr).

      FIND BestStr EXCLUSIVE-LOCK WHERE
           BestSTr.BestNR   = BEstHode.BEstNr AND
           BestSTr.Butik    = 2 AND
           BestStr.Storl    = gStr AND
           BestStr.BestStat = BestHode.BEstStat NO-ERROR.

      IF NOT AVAILABLE BestSTr THEN
      DO:
        CREATE BestStr.
        ASSIGN
          BestSTr.BestNR   = BEstHode.BEstNr 
          BestSTr.Butik    = 2 
          BestStr.Storl    = gStr 
          BestStr.BestStat = BestHode.BEstStat.
      END.
      
      assign
          BestStr.Bestilt = BestStr.Bestilt + gAnt.

      /*
      MESSAGE 
          gloop
          gstr
          gant
          BestStr.Storl
          BestStr.BEstilt
         VIEW-AS ALERT-BOX.
       */
  END.
END.

PROCEDURE FiksStorl:
  def INPUT-output parameter wStorl as char NO-UNDO.

  DEF VAR wDecimaler as CHAR NO-UNDO.

  {syspara.i 1 1 16 wDecimaler}

  assign
     wStorl = trim(wStorl)
     wStorl = caps(wStorl)
     wStorl = if (length(wStorl) = 1 or
                  length(wStorl) = 3
                  )
                 then " " + wStorl
                 else wStorl.

  /* Bytter ut eventuelle comma med punkt. */
  if index(wStorl,",") <> 0 then
    OVERLAY(wStorl, index(wStorl,","), 1, "CHARACTER") = ".".

  /* Sjekker om det er benyttet gyldige tegn i halvnummer. */
  /* Er det ikke det, tas halvnummeret bort.               */
  if NUM-ENTRIES(wStorl,".") = 2 then
    DO:
      if NOT CAN-DO(wDecimaler,ENTRY(2,wStorl,".")) then
        wStorl = ENTRY(1,wStorl,".").
    END.

  RETURN wStorl.   /* Function return value. */

END PROCEDURE.
