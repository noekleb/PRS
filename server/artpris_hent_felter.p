/* Hente felter fra artpris
   Parameter:  Artikkelnr;Profilnr(opsjon);tilbud(yes/no eller 1/2 - opsjon)|komma-sep liste over felter (opsjon)
   Opprettet: 09.08.07 av BHa              
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR cFindParam      AS CHAR   NO-UNDO.
DEF VAR cFields         AS CHAR   NO-UNDO.
DEF VAR fArtikkelNr     AS DEC    NO-UNDO.
DEF VAR iExtent         AS INT    NO-UNDO.
DEF VAR bTilbud         AS LOG    NO-UNDO.
DEF VAR iProfilNr       AS INT    NO-UNDO.
DEF VAR iCl             AS INT    NO-UNDO.
DEF VAR hArtPris        AS HANDLE NO-UNDO.
DEF VAR ix              AS INT    NO-UNDO.
DEF VAR hField          AS HANDLE NO-UNDO.

ASSIGN cFindParam  = ENTRY(1,icParam,"|")
       cFields     = ENTRY(2,icParam,"|")
       fArtikkelNr = DEC(ENTRY(1,cFindParam,";"))
       hArtPris    = BUFFER ArtPris:HANDLE.
IF NUM-ENTRIES(cFindParam,";") > 1 AND ENTRY(2,cFindParam,";") NE "" THEN
  iProfilNr = INT(ENTRY(2,cFindParam,";")).
ELSE DO:
  {syspara.i 5 1 1 iCl INT}.
  FIND Butiker NO-LOCK 
       WHERE Butiker.Butik = iCl
       NO-ERROR.
  IF AVAIL Butiker THEN
    iProfilNr = Butiker.ProfilNr.
END.
IF NUM-ENTRIES(cFindParam,";") > 2 THEN DO:
  bTilbud = LOGICAL(ENTRY(3,cFindParam,";")) NO-ERROR.
  IF NOT ERROR-STATUS:ERROR THEN
    iExtent = IF bTilbud THEN 2 ELSE 1.
  ELSE iExtent = INT(ENTRY(2,cFindParam,";")).
END.

FIND FIRST ArtPris NO-LOCK
     WHERE ArtPris.ArtikkelNr = fArtikkelNr
       AND ArtPris.ProfilNr   = iProfilNr
     NO-ERROR.
IF AVAIL ArtPris THEN DO:
  IF iExtent = 0 AND ArtPris.Tilbud THEN 
    iExtent = 2.
  ELSE iExtent = 1.
  DO ix = 1 TO NUM-ENTRIES(cFields):
    hField = hArtPris:BUFFER-FIELD(ENTRY(ix,cFields)).
    IF hField:EXTENT > 0 THEN
      ocReturn = ocReturn + (IF hField:BUFFER-VALUE[iExtent] NE ? THEN STRING(hField:BUFFER-VALUE[iExtent]) ELSE "") + "|".
    ELSE
      ocReturn = ocReturn + (IF hField:BUFFER-VALUE NE ? THEN STRING(hField:BUFFER-VALUE) ELSE "") + "|".
  END.
  ASSIGN ocReturn = TRIM(ocReturn,"|")
         obOK     = YES.
END.

