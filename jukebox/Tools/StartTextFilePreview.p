DEF VAR hTextFilePreview AS HANDLE NO-UNDO.
DEF VAR cPreviewFile     AS CHAR   NO-UNDO.
DEF VAR cSessId          AS CHAR   NO-UNDO.
DEF VAR cSessFile        AS CHAR   NO-UNDO.
DEF VAR cLine            AS CHAR   NO-UNDO.

cPreviewFile = SEARCH("jboxuilib.p").

RUN jboxloadlib.p ("ResizeLib.p,JBoxUIlib.p,JBoxASlib.p").


cSessfile = SEARCH("incl/custdevmode.i").
IF cSessfile NE ? THEN DO:
  INPUT FROM VALUE(cSessfile).
  REPEAT:
    IMPORT UNFORMATTED cLine.
    IF cLine MATCHES '*setSessionId*' THEN
      cSessId = REPLACE(REPLACE(REPLACE(ENTRY(NUM-ENTRIES(cLine),cLine),'"',""),".",""),")","").
  END.
END.
IF cSessId = "" OR cSessId = "ocSessionId" THEN cSessId = "validsession".

DYNAMIC-FUNCTION("setSessionId",cSessId).
DYNAMIC-FUNCTION("setLanguageCode","EN").

RUN JBoxTextFilePreview.w PERSIST SET hTextFilePreview.

/* Export to Excel does only apply for column reports with dashes (-) to underline header columns
   To remove the Excel button (must be called before InitializeObject): */

RUN InitializeObject IN hTextFilePreview.
    
    /* YES in the last parameter: Append the existing preview: */
/*     setEndColumnHeader("---------- -------- "). */
    /*  For a paged column report you can tell the preview what the last row in the header starts with to 
        suppress headers on pages after the first from preview: */
/*     LoadPreviewFromFile("c:\temp\rpprev.txt",NO). */

/* DYNAMIC-FUNCTION("LoadPreviewFromFile" IN hTextFilePreview,cPreviewFile,NO).  */

DYNAMIC-FUNCTION("setWindowTitle" IN hTextFilePreview,"View file").

RUN MoveToTop IN hTextFilePreview.

SUBSCRIBE TO "InvalidateHandle" IN hTextFilePreview.

WAIT-FOR "close" OF THIS-PROCEDURE.

PROCEDURE InvalidateHandle:
  DEF INPUT PARAM ihProc AS HANDLE NO-UNDO.

  IF ihProc = hTextFilePreview THEN
    APPLY "close" TO THIS-PROCEDURE.
END PROCEDURE.
