 /* Henter liste over størrelser som det ligger strekkode på for en artikkel
   
   Opprettet: 20.01.06 av BHa          
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR ix AS INT NO-UNDO.        
         
FIND FIRST ArtBas 
     WHERE ArtBas.ArtikkelNr = DEC(icParam)
     NO-LOCK NO-ERROR.
IF AVAIL ArtBas THEN DO:

  FIND FIRST StrType OF ArtBas NO-LOCK NO-ERROR.
  IF AVAIL StrType THEN DO ix = 1 TO NUM-ENTRIES(StrType.fordeling):
    FIND FIRST StrekKode OF ArtBas NO-LOCK
         WHERE Strekkode.kode > ""
           AND StrekKode.StrKode = INT(ENTRY(ix,StrType.fordeling))
         NO-ERROR.
    IF AVAIL StrekKode AND StrekKode.StrKode NE 0 THEN
      ocReturn = ocReturn + TRIM(ENTRY(ix,StrType.AlfaFordeling)) + ",".
  END.
END.

ocReturn = TRIM(ocReturn,",").
IF ocReturn NE "" THEN obOk = TRUE.

