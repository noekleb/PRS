/* Sjekk om bruker skal varsles
   Parametere: 
   Opprettet: 18.06.06 av BHa                  
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO INIT YES.

DEF VAR cAlarmSjekkProgListe AS CHAR NO-UNDO.
DEF VAR ix                   AS INT  NO-UNDO.
DEF VAR bAlarm               AS LOG  NO-UNDO.

DYNAMIC-FUNCTION("startASlib" IN SOURCE-PROCEDURE).

cAlarmSjekkProgListe = DYNAMIC-FUNCTION("getFieldList","SysGruppe,SysPara;distinct Parameter1;Parameter2",
                                        "WHERE SysHId = 300 AND NOT Beskrivelse BEGINS 'x'"
                                      + ",EACH SysPara OF SysGruppe NO-LOCK"
                                        ).

DO ix = 1 TO NUM-ENTRIES(cAlarmSjekkProgListe,"|") BY 2:
  IF NOT bAlarm AND SEARCH(ENTRY(ix,cAlarmSjekkProgListe,"|")) NE ? THEN
    bAlarm = DYNAMIC-FUNCTION("runProc",ENTRY(ix + 1,cAlarmSjekkProgListe,"|"),"",?).
  ELSE IF bAlarm THEN LEAVE.
END.

ocReturn = STRING(bAlarm).
