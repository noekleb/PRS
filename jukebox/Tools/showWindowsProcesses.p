CURRENT-WINDOW:WIDTH-CHARS = 200.
DEFINE VARIABLE iCommandLineStart AS INTEGER NO-UNDO.
DEFINE VARIABLE iPidStart AS INTEGER NO-UNDO.
DEFINE VARIABLE cLine AS CHARACTER NO-UNDO.

DEFINE TEMP-TABLE ttProcessInfo NO-UNDO
FIELD iPid AS INTEGER FORMAT 999999
FIELD cCommand AS CHARACTER FORMAT "X(40)"
FIELD cCommandLine AS CHARACTER FORMAT "X(120)".

INPUT THROUGH VALUE ("wmic PROCESS get Caption,Commandline,Processid").
IMPORT UNFORMATTED cLine.
ASSIGN iCommandLineStart = INDEX(cLine,"CommandLine")
iPidStart = INDEX(cLine,"ProcessId").

REPEAT:
IMPORT UNFORMATTED cLine.
CREATE ttProcessInfo.
ASSIGN ttProcessInfo.iPid = INTEGER(TRIM(SUBSTRING(cLine,iPidStart)))
ttProcessInfo.cCommand = TRIM(SUBSTRING(cLine,1,iCommandLineStart - 1))
ttProcessInfo.cCommandLine = TRIM(SUBSTRING(cLine,iCommandLineStart,iPidStart - (iCommandLineStart + 1 )))
.
END.
INPUT CLOSE.

FOR EACH ttprocessinfo BY ttprocessinfo.ccommand:
     DISPLAY ttProcessInfo.iPid
             ttprocessinfo.ccommand
             ttprocessinfo.ccommandLine 
             WITH WIDTH 200.
END.
