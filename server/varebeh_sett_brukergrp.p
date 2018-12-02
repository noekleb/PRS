/* Sett brukergrupper med tilgang til messebok
   Parametere: <VarebehNr>,<rowid-liste for brukergrupper>
   Opprettet: 17.09.07 av BHa                  
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR ix            AS INT    NO-UNDO.
DEF VAR cRowidList    AS CHAR   NO-UNDO.
DEF VAR fVarebehNr    AS DEC    NO-UNDO.

ASSIGN fVarebehNr = DEC(ENTRY(1,icParam,";"))
       cRowidList = ENTRY(2,icParam,";")
       .

FIND VareBehHode NO-LOCK
     WHERE VareBehHode.VarebehNr = fVarebehNr
     NO-ERROR.
IF NOT AVAIL VareBehHode THEN DO:
  ocReturn = "Finner ikke angitt varehåndteringsbok".
  RETURN.
END.

FOR EACH VareBhBrukerGrp OF VareBehHode EXCLUSIVE-LOCK 
   ,FIRST BrukerGrp OF VareBhBrukerGrp NO-LOCK
          WHERE NOT CAN-DO(cRowidList,STRING(ROWID(BrukerGrp)))
    :
  DELETE VareBhBrukerGrp.
END.

DO ix = 1 TO NUM-ENTRIES(cRowidList):
  FIND FIRST BrukerGrp NO-LOCK
       WHERE ROWID(BrukerGrp) = TO-ROWID(ENTRY(ix,cRowidList))
       NO-ERROR.
  IF AVAIL BrukerGrp 
     AND NOT CAN-FIND(FIRST VareBhBrukerGrp
                      WHERE VareBhBrukerGrp.VareBehNr = fVarebehNr
                        AND VareBhBrukerGrp.BrGrpNr   = BrukerGrp.BrGrpNr)
     THEN DO:
    CREATE VareBhBrukerGrp.
    ASSIGN VareBhBrukerGrp.VareBehNr = fVarebehNr
           VareBhBrukerGrp.BrGrpNr   = BrukerGrp.BrGrpNr
           .
  END.
END.


obOk = ocReturn = "".

