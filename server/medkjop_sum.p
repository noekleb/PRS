/* Registrer Slett medrabsjekk record
   Parameter:  
   Opprettet: 5.4.2011             
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR dGrunnlag AS DEC FORMAT "->,>>>,>>>,>>9.99" NO-UNDO.
DEF VAR dTildelt  AS DEC FORMAT "->,>>>,>>>,>>9.99" NO-UNDO.
DEF VAR dSaldo    AS DEC FORMAT "->,>>>,>>>,>>9.99" NO-UNDO.

DEF VAR hQuery AS HANDLE NO-UNDO.

CREATE QUERY hQuery.
hQuery:SET-BUFFERS(ihBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + ihBuffer:NAME + " NO-LOCK").
hQuery:QUERY-OPEN().

hQuery:GET-FIRST().
REPEAT WHILE NOT hQuery:QUERY-OFF-END:
  ASSIGN 
    ocReturn = ''
    obOk     = TRUE
  .

  DO TRANSACTION:
    FIND FIRST MedKjop WHERE MedKjop.b_id = DEC(ihBuffer:BUFFER-FIELD('b_id'):BUFFER-VALUE)
                         NO-LOCK NO-ERROR.
    
    IF AVAIL MedKjop THEN
    DO:
      ASSIGN
        dGrunnlag = dGrunnlag + MedKjop.KjopsBelop
        dTildelt  = dTildelt  + MedKjop.TildeltBelop
        dSaldo    = dSaldo    + MedKjop.Saldo
        .
      obOk = NOT ERROR-STATUS:ERROR.
      IF NOT obOk THEN
      DO:
        ocReturn = ERROR-STATUS:GET-MESSAGE(1).
        LEAVE.
      END.
    END.
  END.
  IF AVAIL MedKjop THEN RELEASE MedKjop.
  hQuery:GET-NEXT().
END.
IF obOk THEN
    ocReturn = 'Kjop: ' + STRING(dGrunnlag) + ' Tildelt: ' + STRING(dTildelt) + ' Saldo: ' + STRING(dSaldo) + ' Sjekksum: ' + STRING(dGrunnlag - (dTildelt + dSaldo)). 

