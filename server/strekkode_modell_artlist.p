/* Finn alle artikler i modell ut fra en strekkode
   Kan også sjekke om artikler fins i en kontekst, f.eks varebok
   Opprettet: 15.05.07 av BHa              
   Endret:    01.08.07 av BHa
            - Returnerer også størrelseskoden 
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR cStrekkode      AS CHAR   NO-UNDO.
DEF VAR cContextId      AS CHAR   NO-UNDO.
DEF VAR cContextIdField AS CHAR   NO-UNDO.
DEF VAR cContext        AS CHAR   NO-UNDO.
DEF VAR hContextBuff    AS HANDLE NO-UNDO.
DEF VAR bOk             AS LOG    NO-UNDO.

DEF BUFFER bArtBas FOR ArtBas.

IF NUM-ENTRIES(icParam) > 1 THEN
  ASSIGN cContext        = ENTRY(1,icParam)
         cContextIdField = ENTRY(2,icParam)
         cContextId      = ENTRY(3,icParam)
         cStrekkode      = ENTRY(4,icParam)
         .
ELSE cStrekkode = icParam.

FIND FIRST strekkode NO-LOCK
     WHERE strekkode.Kode = cStrekkode
     NO-ERROR.
IF NOT AVAIL strekkode THEN DO:
  ocReturn = "Finner ikke strekkode".
  RETURN.
END.
IF strekkode.StrKode = 0 THEN DO:
  ocReturn = "Ugyldig strekkode for scanning".
  RETURN.
END.

IF cContext NE "" THEN DO:
  CREATE BUFFER hContextBuff FOR TABLE cContext NO-ERROR.
END.

FIND FIRST ArtBas NO-LOCK OF strekkode NO-ERROR.
IF AVAIL ArtBas THEN DO:
  bOK = YES.
  IF VALID-HANDLE(hContextBuff) THEN 
    bOk = hContextBuff:FIND-FIRST("WHERE " + cContextIdField + " = " + cContextId + " AND ArtikkelNr = " + STRING(ArtBas.ArtikkelNr)) NO-ERROR.
  IF bOk THEN DO:
    ocReturn = STRING(ArtBas.ArtikkelNr).

    /* Modellhåndtering skal bare kontrolleres når modellfarge er større enn 0 */
    IF ArtBas.ModellFarge > 0 THEN
    DO:
        FOR EACH bArtBas NO-LOCK
            WHERE bArtBas.ModellFarge = ArtBas.ModellFarge
              AND bArtBas.ArtikkelNr NE ArtBas.ArtikkelNr:
          bOk = YES.
          
          IF VALID-HANDLE(hContextBuff) THEN 
            bOk = hContextBuff:FIND-FIRST("WHERE " + cContextIdField + " = " + cContextId + " AND ArtikkelNr = " + STRING(bArtBas.ArtikkelNr)) NO-ERROR.
          IF bOk THEN
            ocReturn = ocReturn + "," + STRING(bArtBas.ArtikkelNr).
          
        END.
    END.

  END.
END.

IF VALID-HANDLE(hContextBuff) THEN DELETE OBJECT hContextBuff.

IF ocReturn NE "" THEN DO:
  FIND FIRST StrKonv OF strekkode NO-LOCK NO-ERROR.
  IF AVAIL StrKonv THEN
    ocReturn = ocReturn + ";" + TRIM(StrKonv.Storl).
  obOk = YES.
END.
ELSE ocReturn = "Ingen artikler funnet for strekkode".
