DEF INPUT  PARAM icBuffer    AS CHAR NO-UNDO.
DEF INPUT  PARAM icRowid     AS CHAR NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.

FIND VarebehHode WHERE ROWID(VarebehHode) = TO-ROWID(icRowid) NO-LOCK NO-ERROR.
IF AVAIL VarebehHode THEN 
DO ON ERROR undo, LEAVE TRANSACTION:
  FOR EACH VarebehLinje EXCLUSIVE-LOCK OF VarebehHode:
    RUN delval_butvarebehlinje.p ("VarebehLinje",
                                   STRING(ROWID(VarebehLinje)),
                                   icSessionId,
                                   OUTPUT ocReturn).
    IF ocReturn NE "" THEN DO:
      ocReturn = ocReturn + CHR(10) + "Artikkelnr: " + STRING(VarebehLinje.ArtikkelNr).
      UNDO, LEAVE.
    END.
    DELETE VarebehLinje.
  END.
  FOR EACH VarebehLinjeTHode EXCLUSIVE-LOCK OF VarebehHode:
    DELETE VarebehLinjeTHode.
  END.
  FOR EACH VarebehLinjeTrans EXCLUSIVE-LOCK OF VarebehHode:
    DELETE VarebehLinjeTrans.
  END.
  FOR EACH Ordre EXCLUSIVE-LOCK OF VarebehHode:
    DELETE Ordre.      
  END.
END.
IF ERROR-STATUS:ERROR THEN
  ocReturn = "Feil oppsto ved sletting av Varebehandlingslinje(r)." + CHR(10) + ocReturn.
