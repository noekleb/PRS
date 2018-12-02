/* Marker ordre som hasteordre 
   Parametere: - liste over ordrenr
               - skal/skal ikke
   
   Opprettet: 25.05.07 av BHa       
   Endret:    04.07.07 av BHa
              - Setter leveringsdato for hasteordre avhengig av oppsett       
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR cOrdreNrList AS CHAR NO-UNDO.
DEF VAR ix           AS INT  NO-UNDO.
DEF VAR bHaster      AS LOG  NO-UNDO.
DEF VAR bSendt       AS LOG  NO-UNDO.
DEF VAR iButNr       AS INT  NO-UNDO.
DEF VAR iLevNr       AS INT  NO-UNDO.
DEF VAR iTid         AS INT  NO-UNDO.
DEF VAR dToday       AS DATE NO-UNDO.
DEF VAR dTomorrow    AS DATE NO-UNDO.

ASSIGN cOrdreNrList = REPLACE(ENTRY(1,icParam),"|",",")
       bHaster      = LOGICAL(ENTRY(2,icParam))
       dToday       = IF WEEKDAY(TODAY) > 1 AND WEEKDAY(TODAY) < 7 THEN TODAY
                      ELSE IF WEEKDAY(TODAY) = 7 THEN TODAY + 2
                      ELSE TODAY + 1
       dTomorrow    = IF WEEKDAY(TODAY) = 6 THEN TODAY + 3
                      ELSE dToday + 1
       .


DO ix = 1 TO NUM-ENTRIES(cOrdreNrList):
  FIND Ordre EXCLUSIVE-LOCK
       WHERE Ordre.OrdreNr = INT(ENTRY(ix,cOrdreNrList)) 
       NO-ERROR.
  IF AVAIL Ordre AND Ordre.OrdreStatus < 2 THEN DO:      
    FIND FIRST DefaultLevDato NO-LOCK
         WHERE DefaultLevDato.ButikkNr = Ordre.CL
           AND DefaultLevDato.LevNr    = Ordre.LevNr
         NO-ERROR.
    IF AVAIL DefaultLevDato AND DefaultLevDato.HasterTid NE 0 THEN
      iTid = DefaultLevDato.HasterTid.
    ELSE iTid = 12 * 3600.

    ASSIGN Ordre.Hasteordre = bHaster
           Ordre.LeveringsDato = IF TIME < iTid THEN dToday ELSE dTomorrow.
    FOR EACH BestHode OF Ordre 
        EXCLUSIVE-LOCK:
      BestHode.LevDato = Ordre.LeveringsDato.
    END.
  END.
  ELSE IF AVAIL Ordre THEN 
    bSendt = YES.
END.

obOk = ocReturn = "" AND NOT (bSendt AND ix = 2).

IF bSendt THEN DO:
  IF ix < 3 THEN
    ocReturn = "Ordre er sendt og kan ikke endres til hasteordre".
  ELSE
    ocReturn = "Minst en av valgte ordre er sent og ble ikke satt som hasteordre".
END.
