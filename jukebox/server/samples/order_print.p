/* Print an Order the good old way and pass the result back to the client in a temp-table
   where it is previewed using this code (invoked by the print action in the toolbar in winsrc\samples\OrderBrw.w):
   
    PROCEDURE PrintRecord:
   
    IF NOT VALID-HANDLE(hPrintPreview) THEN DO:
      RUN JBoxTextFilePreview.w PERSIST SET hPrintPreview.
      /* To remove Excel button (Excel only applies for column reports with regular column headers): */
    /*   DYNAMIC-FUNCTION("setEnableExcel" IN hPrintPreview,NO). */
      RUN InitializeObject IN hPrintPreview.
    END.
    
    DYNAMIC-FUNCTION("setWindowTitle" IN hPrintPreview,"Orderprint from " + PROGRAM-NAME(1) + " Server-routine: samples/order_print.p").
    
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
                     DYNAMIC-FUNCTION("getTempTable","order_print.p"
                                       ,STRING(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("Ordernum"):BUFFER-VALUE),?)
                    ,"",NO).
    
    RUN MoveToTop IN hPrintPreview.
      
    END PROCEDURE.
-----------------------------------------------------------------*/
DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO. 
DEF INPUT  PARAM icParam      AS CHAR NO-UNDO.
DEF OUTPUT PARAM TABLE-HANDLE hTempTable.
DEF OUTPUT PARAM ocReturn     AS CHAR NO-UNDO.

DEF VAR iOrdernum    AS INT    NO-UNDO.
DEF VAR cFileName    AS CHAR   NO-UNDO.

ASSIGN iOrdernum    = INTEGER(icParam)
       cFileName = SESSION:TEMP-DIRECTORY + PROGRAM-NAME(1) + "_" + STRING(RANDOM(1,1000)) + "_" + STRING(TIME) + ".lst".

/* Temp-table for transport. Default field is cText but the preview could also read another field */
DEF TEMP-TABLE ttOrder
    FIELD cText   AS CHAR 
    .
hTempTable = BUFFER ttOrder:HANDLE:TABLE-HANDLE.

FIND FIRST Order NO-LOCK
     WHERE Order.Ordernum = iOrdernum
     NO-ERROR.
IF NOT AVAIL Order THEN RETURN.

FIND FIRST Customer NO-LOCK OF Order.

OUTPUT TO VALUE(cFileName) PAGED PAGE-SIZE 40.

FORM HEADER 
     "** Sports 2000 " FILL("*",80) FORMAT "x(80)"
     "Date:" AT 100 TODAY STRING(TIME,"HH:MM") SKIP(1) 
     "Ordernum: " Order.Ordernum 
     "Customer: " Customer.CustNum " " Customer.Name  SKIP
     FILL("*",120) FORMAT "x(120)"
     WITH FRAME my_header NO-LABELS WIDTH 125 PAGE-TOP NO-ATTR-SPACE STREAM-IO.

FORM HEADER
     "Page:" AT 110 PAGE-NUMBER FORMAT ">9"    SKIP(1)
     WITH FRAME my_bottom NO-LABELS WIDTH 125 PAGE-BOTTOM NO-ATTR-SPACE STREAM-IO.

VIEW FRAME my_header.
VIEW FRAME my_bottom.

DISP Customer EXCEPT Customer.Comments
     WITH FRAME customer WIDTH 200 2 COLUMNS STREAM-IO.

DISP Order EXCEPT Order.CustNum
     WITH FRAME order WIDTH 200 2 COLUMNS STREAM-IO.

PAGE.

FOR EACH OrderLine NO-LOCK
    OF Order:
  DISP OrderLine EXCEPT OrderLine.Ordernum
       WITH FRAME OrderLine DOWN WIDTH 125 STREAM-IO.
  DOWN WITH FRAME OrderLine.
END.

OUTPUT CLOSE.

INPUT FROM VALUE(cFileName).
REPEAT:
  CREATE ttOrder.
  IMPORT UNFORMATTED ttOrder.cText.
END.
INPUT CLOSE.

OS-DELETE VALUE(cFileName).
