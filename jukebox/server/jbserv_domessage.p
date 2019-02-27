/* jbserv_getusermessage.p
   Lookup user message to be returned from f.ex server validation procs
   
   Created 16.08.10 by brynjar@chemistry.no
--------------------------------------------------------------------------*/   
DEF INPUT  PARAM iiMsgNo         AS INT  NO-UNDO.
DEF INPUT  PARAM icMessage       AS CHAR NO-UNDO.
DEF INPUT  PARAM icTitle         AS CHAR NO-UNDO.
DEF INPUT  PARAM icData          AS CHAR NO-UNDO.
DEF INPUT  PARAM icSessionId     AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocMessage       AS CHAR NO-UNDO.

DEF VAR bOk         AS LOG    NO-UNDO.
DEF VAR ix          AS INT    NO-UNDO.
DEF VAR httRunProc  AS HANDLE NO-UNDO.

RUN jbserv_runproc.p (icSessionId,
                      "jbserv_getusermessage.p",
                      STRING(iiMsgNo) + "|"
                      + icMessage + "|"
                      + icTitle,
                      INPUT-OUTPUT TABLE-HANDLE httRunProc,
                      OUTPUT ocMessage,
                      OUTPUT bOK
                      ).
IF icData NE "" THEN
  DO ix = 1 TO NUM-ENTRIES(icData,"|"):
    ocMessage = REPLACE(ocMessage,"&" + STRING(ix),ENTRY(ix,icData,"|")).
  END.
