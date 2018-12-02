
/* Sjekk om det finnes en post i kampanjetilbud
   Opprettet: 29.05.2007 Geir Otto Olsen
 -----------------------------------------------------------------------------------*/
 DEF INPUT  PARAM hBuffer     AS HANDLE NO-UNDO.
 DEF INPUT  PARAM icFields    AS CHAR NO-UNDO.
 DEF INPUT  PARAM icValues    AS CHAR NO-UNDO.
 DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
 DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO. 

/*  DEF VAR iId  AS INT NO-UNDO. */
 DEF VAR iId2      AS INT NO-UNDO.
 DEF VAR iTilbType AS INT NO-UNDO.
 DEF VAR cNavn     AS CHAR NO-UNDO.
 DEF VAR cTekst    AS CHAR NO-UNDO.

 DEF BUFFER bTable FOR KampanjeTilbud.

/*  ASSIGN                                             */
/*   iId = hBuffer:BUFFER-FIELD('KampId'):BUFFER-VALUE */
/*  .                                                  */
 
 /*    cNavn = DYNAMIC-FUNCTION("getValue" IN SOURCE-PROCEDURE,"KampNavn") */
  ASSIGN 
   iTilbType = INT(ENTRY(LOOKUP('KampTilbTypeId',icFields),icValues,"|"))
   cNavn     = ENTRY(LOOKUP('KampTilbnavn',icFields),icValues,"|")
   cTekst    = ENTRY(LOOKUP('KampTilbKvitteringstekst',icFields),icValues,"|")
  NO-ERROR.
  IF ERROR-STATUS:ERROR THEN
  DO:
   ocReturn = ERROR-STATUS:GET-MESSAGE(1).
   RETURN.
  END.
  
  IF iTilbType LE 0 THEN
  DO:
    ocReturn = 'Kampanje type må være satt før lagring'.
    RETURN.
  END.

  IF cNavn = '' THEN
  DO:
    ocReturn = 'Navn kan ikke være blank.'.
    RETURN.
  END.
  
  IF cTekst = '' THEN
  DO:
    ocReturn = 'Kvitteringstekst kan ikke være blank.'.
    RETURN.
  END.

  /* Validering av type 10 */
  IF iTilbType EQ 10 THEN
  DO:
    IF DECIMAL((ENTRY(LOOKUP('KampTilbBelop',icFields),icValues,"|"))) <= 0 THEN 
      ocReturn = 'Beløp må angis.'.
    IF DECIMAL((ENTRY(LOOKUP('KampTilbGrenseAntall',icFields),icValues,"|"))) <= 0 THEN 
      ocReturn = 'Antall må angis.'.
    IF ocReturn <> '' THEN RETURN.
  END.
  /* Validering av type 11 */
  IF iTilbType EQ 11 THEN
  DO:
    IF DECIMAL((ENTRY(LOOKUP('KampTilbBelop',icFields),icValues,"|"))) <= 0 THEN 
      ocReturn = 'Beløp må angis.'.
    IF ocReturn <> '' THEN RETURN.
  END.

 DO ON ERROR UNDO, RETURN:
/*    FOR LAST bTable WHERE bTable.KampId = iId NO-LOCK: LEAVE. END. */
   FOR LAST bTable WHERE bTable.KampTilbId GT 0 NO-LOCK: LEAVE. END.
   IF AVAIL bTable THEN
     iId2 = bTable.KampTilbId + 1.
   ELSE
     iId2 = 1.
   hBuffer:BUFFER-FIELD("KampTilbId"):BUFFER-VALUE = iId2.
 END.

