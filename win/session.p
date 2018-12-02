/* Programnavn: session.p
   Laget av   : SJ 22.06.98
   System     : Felles VS90
   Beskrivelse: Viser session-attributter
   Parametre  :
   Endringer  :
	Last change:  TN   20 Apr 99   11:03 pm
*/
DEF VAR fproc       AS HANDLE NO-UNDO.
DEF VAR lproc       AS HANDLE NO-UNDO.
DEF VAR fchild      AS HANDLE NO-UNDO.
DEF VAR lchild      AS HANDLE NO-UNDO.

DEF VAR wTmpFil AS CHAR NO-UNDO.
  
ASSIGN fproc       = SESSION:FIRST-PROCEDURE
       lproc       = SESSION:LAST-PROCEDURE
       fchild      = SESSION:FIRST-CHILD
       lchild      = SESSION:LAST-CHILD.

RUN gettmpfile.p("SES").
ASSIGN wTmpFil = RETURN-VALUE.

OUTPUT TO VALUE(wTmpFil).

PUT UNFORMATTED
   "      SESJONSINNSTILLINGER " CURRENT-WINDOW:TITLE "  (" TODAY FORMAT "99.99.99" ")" SKIP
   "      --------------------------------------------------------" SKIP
   "      APPL-ALERT-BOXES       = "  STRING(SESSION:APPL-ALERT-BOXES) SKIP
   "      BATCH-MODE             = "  STRING(SESSION:BATCH-MODE) SKIP
   "      CHARSET                = "  SESSION:CHARSET SKIP
   "      CPCASE                 = "  (IF SESSION:CPCASE NE ? THEN SESSION:CPCASE ELSE "?") SKIP
   "      CPCOLL                 = "  (IF SESSION:CPCOLL NE ? THEN SESSION:CPCOLL ELSE "?") SKIP
   "      CPINTERNAL             = "  (IF SESSION:CPINTERNAL NE ? THEN SESSION:CPINTERNAL ELSE "?") SKIP
   "      CPLOG                  = "  (IF SESSION:CPLOG NE ? THEN SESSION:CPLOG ELSE "?") SKIP
   "      CPPRINT                = "  (IF SESSION:CPPRINT NE ? THEN SESSION:CPPRINT ELSE "?") SKIP
   "      CPRCODEIN              = "  (IF SESSION:CPRCODEIN NE ? THEN SESSION:CPRCODEIN ELSE "?") SKIP
   "      CPRCODEOUT             = "  (IF SESSION:CPRCODEOUT NE ? THEN SESSION:CPRCODEOUT ELSE "?") SKIP
   "      CPSTREAM               = "  (IF SESSION:CPSTREAM NE ? THEN SESSION:CPSTREAM ELSE "?") SKIP
   "      CPTERM                 = "  (IF SESSION:CPTERM NE ? THEN SESSION:CPTERM ELSE "?") SKIP
   "      DATA-ENTRY-RETURN      = "  STRING(SESSION:DATA-ENTRY-RETURN) SKIP
   "      DATE-FORMAT            = "  STRING(SESSION:DATE-FORMAT) SKIP
   "      DISPLAY-TYPE           = "  SESSION:DISPLAY-TYPE  SKIP
   "      FIRST-CHILD            = "  STRING(SESSION:FIRST-CHILD) + (IF CAN-QUERY(fchild,"TYPE") THEN " [" + fchild:TYPE + "]" ELSE "") SKIP
   "      FIRST-PROCEDURE        = "  (IF VALID-HANDLE(fproc) THEN fproc:FILE-NAME ELSE "<none>") SKIP
   "      FIRST-SERVER           = "  (IF SESSION:FIRST-SERVER NE ? THEN STRING(SESSION:FIRST-SERVER) ELSE "?") SKIP
   "      FRAME-SPACING          = "  STRING(SESSION:FRAME-SPACING) SKIP
   "      HEIGHT-CHARS           = "  STRING(SESSION:HEIGHT-CHARS) SKIP
   "      HEIGHT-PIXELS          = "  STRING(SESSION:HEIGHT-PIXELS) SKIP
   "      IMMEDIATE-DISPLAY      = "  STRING(SESSION:IMMEDIATE-DISPLAY) SKIP
   "      LAST-CHILD             = "  STRING(SESSION:LAST-CHILD) + (IF CAN-QUERY(lchild,"TYPE") THEN " [" + lchild:TYPE + "]" ELSE "") SKIP
   "      LAST-PROCEDURE         = "  (IF VALID-HANDLE(lproc) THEN lproc:FILE-NAME ELSE "<none>") SKIP
   "      LAST-SERVER            = "  (IF SESSION:LAST-SERVER NE ? THEN STRING(SESSION:LAST-SERVER) ELSE "?") SKIP
   "      MULTITASKING-INTERVAL  = "  STRING(SESSION:MULTITASKING-INTERVAL) SKIP
   "      NUMERIC-FORMAT         = "  SESSION:NUMERIC-FORMAT SKIP
   "      PARAMETER              = "  SESSION:PARAMETER SKIP
   "      PIXELS-PER-COLUMN      = "  STRING(SESSION:PIXELS-PER-COLUMN) SKIP
   "      PIXELS-PER-ROW         = "  STRING(SESSION:PIXELS-PER-ROW) SKIP
   "      PRINTER-CONTROL-HANDLE = "  STRING(SESSION:PRINTER-CONTROL-HANDLE) SKIP
   "      PRINTER-NAME           = "  (IF SESSION:PRINTER-NAME NE ? THEN SESSION:PRINTER-NAME ELSE "?") SKIP
   "      PRINTER-PORT           = "  (IF SESSION:PRINTER-PORT NE ? THEN SESSION:PRINTER-PORT ELSE "?") SKIP
   "      REMOTE                 = "  (IF SESSION:REMOTE THEN "TRUE" ELSE "FALSE") SKIP
   "      STREAM                 = "  SESSION:STREAM SKIP
   "      SUPPRESS-WARNINGS      = "  STRING(SESSION:SUPPRESS-WARNINGS) SKIP
   "      SYSTEM-ALERT-BOXES     = "  STRING(SESSION:SYSTEM-ALERT-BOXES) SKIP
   "      TEMP-DIRECTORY         = "  SESSION:TEMP-DIRECTORY SKIP
   "      THREE-D                = "  STRING(SESSION:THREE-D) SKIP
   "      TIME-SOURCE            = "  SESSION:TIME-SOURCE SKIP
   "      TOOLTIPS               = "  (IF SESSION:TOOLTIPS THEN "TRUE" ELSE "FALSE") SKIP
   "      TYPE                   = "  SESSION:TYPE SKIP
   "      V6DISPLAY              = "  STRING(SESSION:V6DISPLAY) SKIP
   "      WIDTH-CHARS            = "  STRING(SESSION:WIDTH-CHARS) SKIP
   "      WIDTH-PIXELS           = "  STRING(SESSION:WIDTH-PIXELS) SKIP
   "      WINDOW-SYSTEM          = "  SESSION:WINDOW-SYSTEM SKIP
   "      YEAR-OFFSET            = "  STRING(SESSION:YEAR-OFFSET) SKIP
   "      --------------------------------------------------------".

OUTPUT CLOSE.

RUN d-fhvis.w (0,"Sesjonsinnstillinger",wTmpFil,"F",0,ENTRY(1,THIS-PROCEDURE:FILE-NAME,".")).

OS-DELETE VALUE(wTmpfil).

