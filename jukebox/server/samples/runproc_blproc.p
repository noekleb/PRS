/* BL procedure invoked from the runproc service interface
   ihBuffer : runproc.p passes this buffer for optional temp-table parameter
   Similar procedure can be used for the ProcessQuery method
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

obOk = ocReturn = "".                                                    

