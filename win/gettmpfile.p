/************************************************************
    Program:  gettmpfile.p
    Created:  TN   17 Nov 99
Description:

Last change:  TN   17 Nov 99    6:29 pm
************************************************************/
DEF INPUT PARAMETER wPrefix AS CHAR NO-UNDO.

DEF VAR chrBuffer   AS CHAR   NO-UNDO FORMAT 'X(128)'.
DEF VAR chrTempFile AS CHAR   NO-UNDO FORMAT 'X(128)'.
DEF VAR intRC       AS INTE   NO-UNDO.

ASSIGN chrBuffer   = FILL(" ",128)
       chrTempFile = FILL(" ",128).

RUN GetTempPathA(128, OUTPUT chrBuffer, OUTPUT intRC).
RUN GetTempFileNameA(chrBuffer, wPrefix, 0, OUTPUT chrTempFile, OUTPUT intRC).

RETURN chrTempFile.

PROCEDURE GetTempPathA EXTERNAL 'KERNEL32.DLL':
   DEFINE INPUT PARAMETER intSize AS LONG.
   DEFINE OUTPUT PARAMETER chrBuffer AS CHARACTER.
   DEFINE RETURN PARAMETER intRC AS LONG.
END PROCEDURE. 

PROCEDURE GetTempFileNameA EXTERNAL 'KERNEL32.DLL':
   DEFINE INPUT PARAMETER chrpath AS CHARACTER.
   DEFINE INPUT PARAMETER chrPrefix AS CHARACTER.
   DEFINE INPUT PARAMETER intUnique AS LONG.
   DEFINE OUTPUT PARAMETER chrTempFile AS CHARACTER.
   DEFINE RETURN PARAMETER intRC AS LONG.
END PROCEDURE.

