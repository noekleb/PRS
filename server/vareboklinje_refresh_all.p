/* Oppdater artikkelinformasjon i varebok
   Parametere:  Vareboknr 
   Kommentar: Oppdaterer også andre varebøker for samme messe (for de samme artikler)
   
   Opprettet: 02.09.04 av BHa                  
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR fVarebokNr AS DEC  NO-UNDO.
DEF VAR cArtNrLst  AS CHAR NO-UNDO.
DEF VAR cFieldList AS CHAR NO-UNDO.
DEF VAR piLoop     AS INT  NO-UNDO.

ASSIGN
    fVarebokNr = DEC(entry(1,icParam,";"))
    cArtNrLst  = (IF NUM-ENTRIES(icParam,";") > 1
                  THEN ENTRY(2,icParam,";")
                  ELSE "")
    cFieldList = (IF NUM-ENTRIES(icParam,";") > 2
                  THEN ENTRY(3,icParam,";")
                  ELSE "")
    .

/* Alle artikler */
IF cArtNrLst = "" THEN DO:
  FOR EACH VarebokLinje NO-LOCK
      WHERE VarebokLinje.VarebokNr = fVarebokNr:
    RUN update_varebok_from_artbas.p (STRING(VarebokLinje.ArtikkelNr) + "," + STRING(VarebokLinje.VarebokNr) + "," + cFieldList,
                                      ?,
                                      icSessionId,
                                      OUTPUT ocReturn,
                                      OUTPUT obOk).

    IF NOT obOk THEN LEAVE.
  END.
END.
/* Valgte artikler */
ELSE DO piLoop = 1 TO NUM-ENTRIES(cArtNrLst):
  RUN update_varebok_from_artbas.p (ENTRY(piLoop,cArtNrLst) + "," + STRING(fVarebokNr) + "," + cFieldList,
                                    ?,
                                    icSessionId,
                                    OUTPUT ocReturn,
                                    OUTPUT obOk).

  IF NOT obOk THEN LEAVE.
END.

IF ocReturn = "" THEN obOk = TRUE.

