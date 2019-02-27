DEF VAR hTextFilePreview AS HANDLE NO-UNDO.
DEF VAR cPreviewFile     AS CHAR   NO-UNDO.

/* Load a file into text preview: */

cPreviewFile = SEARCH("trg\cre_jboxmenu.p").

RUN jboxloadlib.p ("ResizeLib.p,JBoxUIlib.p,JBoxASlib.p").

RUN JBoxTextFilePreview.w PERSIST SET hTextFilePreview.

/* Export to Excel does only apply for column reports with dashes (-) to underline header columns
   To remove the Excel button (must be called before InitializeObject): */

/* DYNAMIC-FUNCTION("setEnableExcel" IN hTextFilePreview,NO).  */

RUN InitializeObject IN hTextFilePreview.
    
DYNAMIC-FUNCTION("LoadPreviewFromFile" IN hTextFilePreview,cPreviewFile,NO).

DYNAMIC-FUNCTION("setWindowTitle" IN hTextFilePreview,cPreviewFile).


/* Then append some data from the sports2000 db: */

DEF VAR cFileName AS CHAR NO-UNDO.
DEF TEMP-TABLE ttSalesrep
    FIELD cText AS CHAR.

cFileName = SESSION:TEMP-DIRECTORY + "_" + STRING(TIME) + ".txt".
OUTPUT TO VALUE(cFileName).
FOR EACH Salesrep NO-LOCK.
  DISP Salesrep.SalesRep
       Salesrep.RepName
       WITH STREAM-IO.
END.
OUTPUT CLOSE.

DYNAMIC-FUNCTION("LoadPreviewFromFile" IN hTextFilePreview,cFileName,YES). /* append */
OS-DELETE VALUE(cFileName).


RUN MoveToTop IN hTextFilePreview.

SUBSCRIBE TO "InvalidateHandle" IN hTextFilePreview.

WAIT-FOR "close" OF THIS-PROCEDURE.

PROCEDURE InvalidateHandle:
  DEF INPUT PARAM ihProc AS HANDLE NO-UNDO.

  IF ihProc = hTextFilePreview THEN
    APPLY "close" TO THIS-PROCEDURE.
END PROCEDURE.
