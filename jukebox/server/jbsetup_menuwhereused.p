/* Report where-used menu item:
   
   client-side usage (JBoxJlwMenuView.w)
   
    PROCEDURE WhereUsed:
   
    IF NOT VALID-HANDLE(hPrintPreview) THEN DO:
      RUN JBoxTextFilePreview.w PERSIST SET hPrintPreview.
      /* To remove Excel button (Excel only applies for column reports with regular column headers): */
    /*   DYNAMIC-FUNCTION("setEnableExcel" IN hPrintPreview,NO). */
      RUN InitializeObject IN hPrintPreview.
    END.
    
    DYNAMIC-FUNCTION("setWindowTitle" IN hPrintPreview,"JBoxMenuprint from " + PROGRAM-NAME(1) + " Server-routine: samples/JBoxMenu_print.p").
    
    /* If the header looks nasty (like here) in Excel it can be skipped from the export: */
    DYNAMIC-FUNCTION("setViewHeadersInExcel" IN hPrintPreview,NO).
    
    /* If (like here) the column headers don't occur on the first page, give the start of the column header line so
       column headers may be viewed the first time they occur: */
    DYNAMIC-FUNCTION("setStartColumnHeader" IN hPrintPreview,"Line Num ").
    
    /* For pages after the first, instuct the preview to hide header lines, including the column headers */
    DYNAMIC-FUNCTION("setEndColumnHeader" IN hPrintPreview,"-----"). /* <- the beginning of the end */
    
    /* To hide the footer from the preview: */
    DYNAMIC-FUNCTION("setFooterLineNum" IN hPrintPreview,39). 
    
    DYNAMIC-FUNCTION("LoadPreviewFromTT" IN hPrintPreview,
                     DYNAMIC-FUNCTION("getTempTable","JBoxMenu_print.p"
                                       ,STRING(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("iJBoxMenuId"):BUFFER-VALUE),?)
                    ,"",NO).
    
    RUN MoveToTop IN hPrintPreview.
      
    END PROCEDURE.
-----------------------------------------------------------------*/
DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO. 
DEF INPUT  PARAM icParam      AS CHAR NO-UNDO.
DEF OUTPUT PARAM TABLE-HANDLE hTempTable.
DEF OUTPUT PARAM ocReturn     AS CHAR NO-UNDO.

DEF VAR iMenuItem    AS INT    NO-UNDO.
DEF VAR cFileName    AS CHAR   NO-UNDO.
DEF VAR cMenuId      AS CHAR   NO-UNDO.

ASSIGN iMenuItem = INTEGER(icParam)
       cFileName = SESSION:TEMP-DIRECTORY + PROGRAM-NAME(1) + "_" + STRING(RANDOM(1,1000)) + "_" + STRING(TIME) + ".lst".

DEF TEMP-TABLE ttStruct
    FIELD  cMenuId    AS CHAR  FORMAT "x(20)" COLUMN-LABEL "Menu id"
    FIELD  cMenuType  AS CHAR  FORMAT "x(12)" COLUMN-LABEL "Type"
    FIELD  cMenuLabel AS CHAR  FORMAT "x(30)" COLUMN-LABEL "Label"
    FIELD  cLaunch    AS CHAR  FORMAT "x(30)" COLUMN-LABEL "Launch"
    .
    

/* Temp-table for transport. Default field is cText but the preview could also read another field */
DEF TEMP-TABLE ttMenu
    FIELD cText   AS CHAR 
    .
hTempTable = BUFFER ttMenu:HANDLE:TABLE-HANDLE.

FIND FIRST JBoxMenu NO-LOCK
     WHERE JBoxMenu.iJBoxMenuId = iMenuItem
     NO-ERROR.
IF NOT AVAIL JBoxMenu THEN RETURN.

OUTPUT TO VALUE(cFileName) PAGED PAGE-SIZE 40.

FORM HEADER 
     "** Where used: " FILL("*",80) FORMAT "x(80)"
     "Date:" AT 100 TODAY STRING(TIME,"HH:MM") SKIP(1) 
     "MenuId: " + STRING(JBoxMenu.iJBoxMenuId) + " Type: " + JBoxMenu.cMenuType
   + "  Label: " + JBoxMenu.cMenuLabel + " Launch: " + JBoxMenu.cLaunch FORMAT "x(120)" SKIP
     FILL("*",120) FORMAT "x(120)"
     WITH FRAME my_header NO-LABELS WIDTH 125 PAGE-TOP NO-ATTR-SPACE STREAM-IO.

FORM HEADER
     "Page:" AT 110 PAGE-NUMBER FORMAT ">9"    SKIP(1)
     WITH FRAME my_bottom NO-LABELS WIDTH 125 PAGE-BOTTOM NO-ATTR-SPACE STREAM-IO.

VIEW FRAME my_header.
VIEW FRAME my_bottom.

FORM ttStruct.cMenuId
     ttStruct.cMenuType  
     ttStruct.cMenuLabel 
/*      ttStruct.cLaunch */
     WITH FRAME JBoxMenu DOWN WIDTH 125 STREAM-IO.

/*
DISP Customer EXCEPT Customer.Comments
     WITH FRAME customer WIDTH 200 2 COLUMNS STREAM-IO.

DISP JBoxMenu EXCEPT JBoxMenu.CustNum
     WITH FRAME JBoxMenu WIDTH 200 2 COLUMNS STREAM-IO.

PAGE.
*/

FUNCTION ViewParent RETURNS LOGICAL (INPUT iiMenuId AS INTEGER, INPUT iiLevel AS INT):

  DEF BUFFER JBoxMenu FOR JBoxMenu.
  DEF BUFFER JBoxMenuToMenu FOR JBoxMenuToMenu.

  iiLevel = iiLevel + 1.

  FOR EACH JBoxMenuToMenu NO-LOCK
      WHERE JBoxMenuToMenu.iFromMenuId = iiMenuId
     ,FIRST JBoxMenu NO-LOCK 
            WHERE JBoxMenu.iJBoxMenuId = JBoxMenuToMenu.iToMenuId
      :
    CREATE ttStruct.
    BUFFER-COPY JBoxMenu TO ttStruct.
    ttStruct.cMenuId     = FILL(" ",iiLevel * 2) + STRING(JBoxMenu.iJBoxMenuId).

    ViewParent(JBoxMenu.iJBoxMenuId,iiLevel).
  END.

END FUNCTION.

ViewParent (iMenuItem,-1).

FOR EACH ttStruct:
  DISP ttStruct.cMenuId
       ttStruct.cMenuType
       ttStruct.cMenuLabel
/*        ttStruct.cLaunch */
  WITH FRAME JBoxMenu.
  DOWN WITH FRAME JBoxMenu.
END.

OUTPUT CLOSE.

INPUT FROM VALUE(cFileName).
REPEAT:
  CREATE ttMenu.
  IMPORT UNFORMATTED ttMenu.cText.
END.
INPUT CLOSE.

OS-DELETE VALUE(cFileName).
