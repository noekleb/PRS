/************************************************************
    Program:  currdir.p
    Created:  TN    7 Jul 98
Description:  Henter navn på current-directory.

Last change:  TN    7 Jul 98    9:01 pm
************************************************************/

DEF OUTPUT PARAMETER parCurrDirName as CHAR NO-UNDO.

DEFINE VARIABLE chrDirectoryName AS CHARACTER NO-UNDO FORMAT "X(256)".
DEFINE VARIABLE intBufferSize    AS INTEGER   NO-UNDO INITIAL 256.
DEFINE VARIABLE intResult        AS INTEGER   NO-UNDO.
DEFINE VARIABLE ptrToString      AS MEMPTR    NO-UNDO.

PROCEDURE GetCurrentDirectoryA EXTERNAL "KERNEL32.DLL":
    DEFINE INPUT        PARAMETER intBufferSize AS LONG.
    DEFINE INPUT-OUTPUT PARAMETER ptrToString   AS MEMPTR.
    DEFINE RETURN       PARAMETER intResult     AS SHORT.
END PROCEDURE.

SET-SIZE(ptrToString) = 256.
RUN GetCurrentDirectoryA (INPUT        intBufferSize,
                          INPUT-OUTPUT ptrToString,
                          OUTPUT       intResult).

ASSIGN chrDirectoryName = GET-STRING(ptrToString,1).
IF intResult = 0 THEN
    parCurrDirName = "<avbryt>". /* Function call failed, not sure why */
ELSE
    IF intResult = LENGTH(chrDirectoryName) THEN
      parCurrDirName = chrDirectoryName.
    ELSE
      parCurrDirName = "<avbryt>". /* Buffer size is too small. */

SET-SIZE(ptrToString) = 0.
