/* Oppdater person-team link fra person
   Parameters:  teamid|personliste 
   Opprettet: 20.04.06 av Brynjar Hasle                  
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR ix           AS INT    NO-UNDO.
DEF VAR fMesseNr     AS DEC    NO-UNDO.
DEF VAR cButikkListe AS CHAR   NO-UNDO.

ASSIGN fMesseNr     = DEC(ENTRY(1,icParam))
       cButikkListe = ENTRY(2,icParam)
                    .

DO TRANSACTION:
  FOR EACH MesseForButikk EXCLUSIVE-LOCK
      WHERE MesseForButikk.MesseNr = fMesseNr:
    IF LOOKUP(STRING(MesseForButikk.ButikkNr),cButikkListe,"|") = 0 THEN
      DELETE MesseForButikk.
  END.
  DO ix = 2 TO NUM-ENTRIES(cButikkListe,"|"):
    FIND MesseForButikk 
         WHERE MesseForButikk.MesseNr  = fMesseNr
           AND MesseForButikk.ButikkNr = INT(ENTRY(ix,cButikkListe,"|"))
         NO-LOCK NO-ERROR.
    IF NOT AVAIL MesseForButikk THEN DO:
      CREATE MesseForButikk.
      ASSIGN MesseForButikk.MesseNr         = fMesseNr
             MesseForButikk.ButikkNr        = INT(ENTRY(ix,cButikkListe,"|"))
             MesseForButikk.RegistrertDato  = TODAY
             MesseForButikk.RegistrertTid   = TIME
             NO-ERROR.
      IF ERROR-STATUS:ERROR THEN ocReturn = ERROR-STATUS:GET-MESSAGE(1).
    END.
  END.
END.

IF ocReturn = "" THEN obOk = TRUE.

