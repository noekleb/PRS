/* Parametere:  
   Opprettet: 14.11.2008 - GOO                  
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icBuffer    AS CHAR NO-UNDO.
DEF INPUT  PARAM icRowid     AS CHAR NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
  
FIND sysGruppe WHERE ROWID(sysGruppe) = TO-ROWID(icRowid) NO-LOCK NO-ERROR.

IF AVAIL sysGruppe THEN 
DO ON ERROR undo, LEAVE TRANSACTION:
  FOR EACH syspara OF sysGruppe EXCLUSIVE-LOCK:
    DELETE syspara NO-ERROR.
  END.
  ASSIGN
      ocReturn = IF ERROR-STATUS:ERROR THEN ERROR-STATUS:GET-MESSAGE(1) ELSE "". /*Blank hvis ok*/
END. /*Transaction*/
