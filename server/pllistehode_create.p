/* Oppretter plListeHode post 
 -----------------------------------------------------------------------------------*/
DEF INPUT  PARAM hBuffer     AS HANDLE NO-UNDO.
DEF INPUT  PARAM icFields    AS CHAR NO-UNDO.
DEF INPUT  PARAM icValues    AS CHAR NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.

DEF VAR iPlListeHodeId AS INT NO-UNDO INIT 1.
DEF VAR cUser          AS CHAR NO-UNDO.

DEFINE BUFFER bPlListeHode FOR PlListeHode.


FIND LAST bPlListeHode NO-LOCK
     NO-ERROR.
IF AVAIL bPlListeHode THEN
  iPlListeHodeId = bPlListeHode.PlListeId + 1.

ASSIGN hBuffer:BUFFER-FIELD("PlListeId"):BUFFER-VALUE = iPlListeHodeId
       hBuffer:BUFFER-FIELD("PlLType"):BUFFER-VALUE = 2
       .

cUser = DYNAMIC-FUNCTION("getASuserId" IN SOURCE-PROCEDURE) NO-ERROR.
IF NOT ERROR-STATUS:ERROR THEN
  hBuffer:BUFFER-FIELD("RegistrertAv"):BUFFER-VALUE = cUser.
