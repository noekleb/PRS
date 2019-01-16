/* print_test.p
   


   Created: 15/1-19 TN
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR ix          AS INT    NO-UNDO.
DEF VAR cUserId     AS CHAR   NO-UNDO.
DEF VAR hAsLib 	    AS HANDLE NO-UNDO.

hAsLib = DYNAMIC-FUNCTION("startASlib" IN SOURCE-PROCEDURE).

/* To make the lib available to a sub-procedure: */
FUNCTION startASlib RETURNS HANDLE():
  RETURN hAsLib.
END FUNCTION.




MESSAGE 'MOTTATT:' icParam
VIEW-AS ALERT-BOX.


FINALLY:
  obOk = ocReturn = "".
END FINALLY.
