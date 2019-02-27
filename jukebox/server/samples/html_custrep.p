{incl/htmlwrapperdef.i}
DEF VAR cReportName  AS CHAR NO-UNDO INIT "Customer report".
DEF VAR cFileLabels  AS CHAR NO-UNDO INIT "Custnum|Name|Address|Address 2".
DEF VAR cFreeText    AS CHAR NO-UNDO INIT "Free text".
DEF VAR cUserName    AS CHAR NO-UNDO 
/*     INIT "Brynjar Hasle" */
    .
DEF VAR cThisProg    AS CHAR NO-UNDO 
/*     INIT "html_custrep.p" */
    .
DEF VAR cBodyContent AS CHAR NO-UNDO.
DEF VAR iCount       AS INT  NO-UNDO.

OUTPUT TO c:\temp\custrep.html.

PUT UNFORMATTED
    HTML;Start ("|", cReportName,'<font SIZE="10">')
    HTML;Head1 (cReportName,"100%","",1,0,2,NUM-ENTRIES(cFileLabels, "|"))
    HTML;Head2 (cFreeText)
/*     HTML;Head3 (STRING(TODAY, "99/99/9999") + " : " + STRING(TIME, "HH:MM") + ", " + cUserName + " (" + cThisProg + ")")  */
    HTML;ColHead (cFileLabels,"R|L|L|L").

FOR EACH customer NO-LOCK:
  cBodyContent = STRING(customer.custnum) + "|" + customer.NAME + "|" + customer.address + "|" + customer.address2.
  PUT UNFORMATTED HTML;Col(cBodyContent, "", 0).
  iCount = iCount + 1.
END.

cBodyContent = "COUNT:|" + STRING(iCount).
  
PUT UNFORMATTED
    HTML;ColFooter(cBodyContent)
    HTML;Footer1 ("End report")
    HTML;Footer2 ("It was a pleasure")
    HTML;END     ().


OUTPUT CLOSE.
