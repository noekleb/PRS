/* Parametere:  
   Opprettet: 14.11.2008 - GOO                  
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icRowid             AS CHAR NO-UNDO.
DEF INPUT  PARAM icFields            AS CHAR NO-UNDO.
DEF INPUT  PARAM icValues            AS CHAR NO-UNDO.
DEF INPUT  PARAM icSessionId         AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn            AS CHAR NO-UNDO.
    
DEF VAR hb AS HANDLE NO-UNDO.

DO TRANSACTION:
    hb = BUFFER SBudMalDag:HANDLE.
    hb:FIND-BY-ROWID(TO-ROWID(icRowid),EXCLUSIVE-LOCK, NO-WAIT).

    IF hb:AVAILABLE THEN
    DO:
      IF NUM-ENTRIES(icFields,'|') GT 1 THEN
      DO: /*Alle felter*/

      END.
      ELSE
      DO: /*Kun et felt, kommer fra oppdatering av browse kolonne*/
        CASE ENTRY(1,icFields,'|'):
          WHEN 'Prosent' THEN
          DO:
              ASSIGN
                  hb:BUFFER-FIELD('Prosent'):BUFFER-VALUE = DEC(ENTRY(1,icValues,'|'))
                  /*
                  hb:BUFFER-FIELD('OpptVerdi'):BUFFER-VALUE  = hb:BUFFER-FIELD('AntallTalt'):BUFFER-VALUE * hb:BUFFER-FIELD('VVareKost'):BUFFER-VALUE
                  hb:BUFFER-FIELD('AntallDiff'):BUFFER-VALUE = hb:BUFFER-FIELD('AntallPar'):BUFFER-VALUE - hb:BUFFER-FIELD('AntallTalt'):BUFFER-VALUE
                  hb:BUFFER-FIELD('VerdiDiff'):BUFFER-VALUE  = hb:BUFFER-FIELD('AntallDiff'):BUFFER-VALUE * hb:BUFFER-FIELD('VVareKost'):BUFFER-VALUE
                  */
                  .
          END.
          WHEN 'DbProsent' THEN
          DO:
              ASSIGN
                  hb:BUFFER-FIELD('DbProsent'):BUFFER-VALUE = DEC(ENTRY(1,icValues,'|'))
                  .
          END.
        END CASE.
      END.
    END.
END. /* TRANSACTION */

ocReturn = "".

