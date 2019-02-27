DEF INPUT PARAM icSessionId  AS CHAR   NO-UNDO.
DEF INPUT PARAM icUserId     AS CHAR   NO-UNDO.
DEF INPUT PARAM irawPwd      AS RAW    NO-UNDO.
DEF INPUT PARAM irawKey      AS RAW    NO-UNDO.
DEF INPUT PARAM icUserList   AS CHAR   NO-UNDO.
DEF INPUT PARAM iiResourceId AS INT    NO-UNDO.
DEF OUTPUT PARAM bOk         AS LOG    NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR   NO-UNDO.

DEF VAR mpPwd  AS MEMPTR NO-UNDO.
DEF VAR cPwd   AS CHAR   NO-UNDO.

mpPwd = DECRYPT(irawPwd,irawKey).
cPwd = GET-STRING(mpPwd,1).

IF cPwd = "" OR cPwd = ? THEN RETURN.

bOK = SETUSERID(icUserId,cPwd,LDBNAME(1)).

/* DEF STREAM sLog.                                                                              */
/* OUTPUT STREAM sLog TO ./conflog.txt.                                                          */
/* PUT STREAM sLog UNFORMATTED "bOk: " bOk " icUser: " icUserId " icUserList: " icUserList SKIP. */

IF bOk AND icUserList NE "" AND NOT CAN-DO(icUserList,icUserId) THEN bOK = FALSE.

/* PUT STREAM sLog UNFORMATTED "bOk: " bOk SKIP. */
/*                                               */
/* OUTPUT STREAM sLog CLOSE.                     */

