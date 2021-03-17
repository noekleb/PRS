/* Sjekker at alle størrelser i en alfastørrelse har strekkode 
   Returnerer en modifisert liste over størrelser
   Parametere:  Artikkelnr
   
   Opprettet: 15.06.06 av BHa                  
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR fArtikkelNr AS DEC  NO-UNDO.
DEF VAR ix          AS INT  NO-UNDO.
DEF VAR cVirkStr    AS CHAR NO-UNDO.

fArtikkelNr = DEC(icParam).

FIND ArtBas NO-LOCK
     WHERE ArtBas.ArtikkelNr = fArtikkelNr 
     NO-ERROR.
IF NOT AVAIL ArtBas THEN 
  RETURN.

FIND FIRST StrType OF ArtBas NO-LOCK
     NO-ERROR.
IF NOT AVAIL StrType THEN
  RETURN.

DO ix = 1 TO NUM-ENTRIES(StrType.Fordeling):
  IF CAN-FIND(FIRST Strekkode OF Artbas
              WHERE Strekkode.StrKode = INT(ENTRY(ix,StrType.Fordeling))) THEN
    cVirkStr = cVirkStr + ENTRY(ix,StrType.AlfaFordeling) + ",".
END.

cVirkStr = TRIM(cVirkStr,",").

IF cVirkStr NE "" THEN
  ASSIGN ocReturn = cVirkStr
         obOK     = YES.

