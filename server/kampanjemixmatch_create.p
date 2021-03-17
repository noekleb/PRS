
/* Opprettet: 26.09.2007 Geir Otto Olsen
 -----------------------------------------------------------------------------------*/
DEF INPUT  PARAM hBuffer     AS HANDLE NO-UNDO.
DEF INPUT  PARAM icFields    AS CHAR NO-UNDO.
DEF INPUT  PARAM icValues    AS CHAR NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO. 

 DEF VAR iId    AS INT NO-UNDO.
 DEF VAR dStart AS DATE NO-UNDO.
 DEF VAR dSlutt AS DATE NO-UNDO.
 DEF VAR cNavn  AS CHAR NO-UNDO.
 DEFINE VARIABLE lKampId AS DECIMAL NO-UNDO.
 
 DEF BUFFER bTable FOR KampanjeMixMatch.

/* 
 MESSAGE 'icFields' icFields SKIP
         'icValues' icValues SKIP 
         hBuffer:BUFFER-FIELD('KampId'):BUFFER-VALUE  
 VIEW-AS ALERT-BOX.
*/

 ASSIGN 
   /*Midlertidig fjernet grunnet bug i rammeverk*/
/*    iId    = INT(DYNAMIC-FUNCTION("getValue" IN SOURCE-PROCEDURE,"KampEierId"))     */
/*    dStart = DATE(DYNAMIC-FUNCTION("getValue" IN SOURCE-PROCEDURE,"KampStartDato")) */
/*    dSlutt = DATE(DYNAMIC-FUNCTION("getValue" IN SOURCE-PROCEDURE,"KampSluttDato")) */
/*    cNavn = DYNAMIC-FUNCTION("getValue" IN SOURCE-PROCEDURE,"KampNavn") */
   iId    = INT(ENTRY(LOOKUP('KampEierId',icFields),icValues,"|"))
   dStart = DATE(ENTRY(LOOKUP("KampStartDato",icFields),icValues,"|"))
   dSlutt = DATE(ENTRY(LOOKUP("KampSluttDato",icFields),icValues,"|"))
   cNavn  = ENTRY(LOOKUP("KampNavn",icFields),icValues,"|")
   lKampId = DECIMAL(hBuffer:BUFFER-FIELD('KampId'):BUFFER-VALUE)
 NO-ERROR.
 IF ERROR-STATUS:ERROR THEN
 DO:
   ocReturn = ERROR-STATUS:GET-MESSAGE(1).
   RETURN.
 END.
 IF NOT CAN-FIND(FIRST KampanjeEier WHERE KampanjeEier.KampEierId = iId) THEN
 DO:
   ocReturn = 'Kampanje eier må registreres før kampanjen kan opprettes.'.
   RETURN.
 END.
 IF cNavn = '' THEN
 DO:
   ocReturn = 'Kampanjenavn kan ikke være blank.'.
   RETURN.
 END.
 
 IF (dStart NE ? AND dSlutt NE ?) THEN
 DO:
   IF dStart GT dSlutt THEN
   DO:
     ocReturn = 'Kampanjens start dato må settes tidligere enn slutt dato'.
     RETURN.
   END.
 END.
 ELSE IF dSlutt NE ? THEN
 DO:
   ocReturn = 'Kampanjens start dato må settes tidligere enn slutt dato'.
   RETURN.
 END.
  
 FIND KampanjeEier NO-LOCK WHERE 
    KampanjeEier.KampEierId = iId NO-ERROR.
 IF AVAILABLE KampanjeEier THEN 
  DO TRANSACTION:
    IF KampanjeEier.ButikkNr > 0 THEN 
    DO:
      IF NOT CAN-FIND(FIRST KampanjeButikker WHERE 
                            KampanjeButikker.KampId = lKampId AND 
                            KampanjeButikker.Butik  = KampanjeEier.ButikkNr) THEN 
      DO:
        CREATE KampanjeButikker.
        ASSIGN
            KampanjeButikker.KampId = lKampId 
            KampanjeButikker.Butik  = KampanjeEier.ButikkNr 
            . 
        RELEASE KampanjeButikker.       
      END.
    END.
  END.
 