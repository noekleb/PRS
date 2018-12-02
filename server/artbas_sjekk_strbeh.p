DEF INPUT PARAM  irBuffer      AS ROWID NO-UNDO.
DEF INPUT PARAM  icParam       AS CHAR  NO-UNDO.
DEF INPUT PARAM  icSessionId   AS CHAR  NO-UNDO.
DEF OUTPUT PARAM ocReturn      AS CHAR  NO-UNDO.

DEF VAR cStrList     AS CHAR NO-UNDO.
DEF VAR bLager       AS LOG  NO-UNDO.
DEF VAR iButikkNr    AS INT  NO-UNDO.
DEF VAR iSumbeh      AS INT  NO-UNDO.
DEF VAR cButikkListe AS CHAR NO-UNDO.
DEF VAR iButAnt      AS INT  NO-UNDO EXTENT 200.
DEF VAR ix           AS INT  NO-UNDO.
DEF VAR bStrFound    AS LOG  NO-UNDO.

ASSIGN cStrList     = REPLACE(ENTRY(1,icParam,"¤"),"&",",")
       bLager       = LOGICAL(ENTRY(2,icParam,"¤"))
       iButikkNr    = INT(ENTRY(3,icParam,"¤"))
       .
    
FOR FIRST ArtBas FIELDS(ArtikkelNr) NO-LOCK
    WHERE ROWID(ArtBas) = irBuffer:
  IF cStrList NE "" THEN DO:
    FOR EACH ArtLag FIELDS(lagant butik) NO-LOCK
        WHERE ArtLag.ArtikkelNr = ArtBas.ArtikkelNr
          AND CAN-DO(cStrList,TRIM(ArtLag.Storl)):
      IF iButikkNr > 0 AND ArtLag.butik NE iButikkNr THEN NEXT.
      ELSE IF iButikkNr = -1 THEN DO:
        IF NOT CAN-DO(cButikkListe,STRING(ArtLag.butik)) THEN
          cButikkListe = cButikkListe + STRING(ArtLag.butik) + ",".
        iButAnt[LOOKUP(STRING(ArtLag.butik),cButikkListe)] = iButAnt[LOOKUP(STRING(ArtLag.butik),cButikkListe)] + ArtLag.lagant.
      END.
      ASSIGN iSumBeh = iSumBeh + ArtLag.lagant
             bStrFound = TRUE.
    END.
    IF NOT bStrFound THEN ocReturn = "skiprow".
  END.
  ELSE FOR EACH Lager FIELDS(lagant butik) NO-LOCK
           WHERE Lager.ArtikkelNr = ArtBas.ArtikkelNr:
    IF iButikkNr > 0 AND Lager.butik NE iButikkNr THEN NEXT.
    ELSE IF iButikkNr = -1 THEN DO:
      IF NOT CAN-DO(cButikkListe,STRING(Lager.butik)) THEN
        cButikkListe = cButikkListe + STRING(Lager.butik) + ",".
      iButAnt[LOOKUP(STRING(Lager.butik),cButikkListe)] = iButAnt[LOOKUP(STRING(Lager.butik),cButikkListe)] + Lager.lagant.
    END.
    iSumBeh = iSumBeh + Lager.lagant.
  END.
END.

IF NOT ocReturn = "skiprow" THEN DO:
  IF bLager AND iSumBeh LE 0 THEN ocReturn = "skiprow".
  ELSE IF iButikkNr = -1 THEN DO:
    cButikkListe = TRIM(cButikkListe,",").
    DO ix = 1 TO NUM-ENTRIES(cButikkListe):
      IF iButAnt[ix] NE 0 THEN
        ocReturn = ocReturn + ENTRY(ix,cButikkListe) + ": " + STRING(iButAnt[ix]) + " ".
    END.
  END.
  ELSE ocReturn = STRING(iSumBeh).
END.


