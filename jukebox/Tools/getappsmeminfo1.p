/* File       : getappsmeminfo1.p
>   Author     : Peter van Dam (Netsetup)
>   Description: Fetch memory usage information from the first connected
>                AppServer (client side).
>
>                If there is no AppServers connected information from
>                the current session is returned.
>
>                All returned information is published for logging.
>                Information below level 40 is shown in a message box as
> well.
>
>                See 'A Practical Use For Named Events' on www.v9stuff.com
>                for information on logging levels.
>
> */

DEF INPUT PARAM icLogFile AS CHAR NO-UNDO.

/* DEF VAR hServer          AS HANDLE NO-UNDO.                                              */
/* DEF VAR cConnParam       AS CHAR NO-UNDO INIT "-AppService Sports2000 -H 192.168.105.3". */
/*                                                                                          */
/* CREATE SERVER hServer.                                                                   */
/* hServer:CONNECT(cConnParam) NO-ERROR.                                                    */

DEF TEMP-TABLE t-info NO-UNDO
   FIELD iLevel AS INT
   FIELD nInfo AS CHAR.

DEF VAR happs# AS HANDLE NO-UNDO.


/* "Walk the AppServer tree" */
IF DYNAMIC-FUNCTION("getAppServiceHandle") NE  ? THEN 
  happs# = DYNAMIC-FUNCTION("getAppServiceHandle").
ELSE happs# = SESSION.
/* happs# = SESSION:FIRST-SERVER.                      */
/* IF VALID-HANDLE(happs#) AND happs# NE SESSION THEN  */
/*  PUBLISH "message" (18, "going to the appserver!"). */
/* ELSE DO:                                            */
/*  ASSIGN happs# = SESSION.                           */
/*  PUBLISH "message" (18, "local session").           */
/* END.                                                */

DO ON ERROR UNDO, LEAVE
  ON STOP UNDO, LEAVE:

 RUN getappsmeminfo2.p ON happs# (OUTPUT TABLE t-info).


 IF icLogFile NE "" THEN DO:
   OUTPUT TO VALUE(icLogFile) APPEND.
   PUT SKIP(1).
   PUT TODAY " " STRING(TIME,"HH:MM:SS") ":" SKIP.
 END.
 FOR EACH t-Info:
/*    PUBLISH "message" (t-info.iLevel, t-info.ninfo).  */
   IF t-info.iLevel < 40 OR t-info.iLevel = 65 THEN DO:
     IF icLogFile NE "" THEN 
       PUT UNFORMATTED t-info.ninfo SKIP.
     ELSE
       MESSAGE t-info.ninfo VIEW-AS ALERT-BOX.
   END.
 END.
 IF icLogFile NE "" THEN DO:
   PUT UNFORMATTED "--------------------------------------------------------------" SKIP(1).
   OUTPUT CLOSE.
 END.
END.

