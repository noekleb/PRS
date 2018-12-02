&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBulder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

{ xprint.i }

PROCEDURE getBinNames EXTERNAL "xprBin.dll":
    DEF INPUT PARAMETER A AS CHAR.
    DEF INPUT PARAMETER B AS MEMPTR.
    END.

def var M   as MEMPTR NO-undo.
def var L   as char no-undo.
DEF VAR binList AS CHAR NO-UNDO.

/*    formatList  = 'LETTER,LETTERSMALL,TABLOID,LEDGER,LEGAL,STATEMENT,EXECUTIVE,A3,'     /* 1-8   */    */
/*                    + 'A4,A4SMALL,A5,B4,B5,FOLIO,QUARTO,10X14,11X17,'                   /* 9-17   */   */
/*                    + 'NOTE,ENV_9,ENV_10,ENV_11,ENV_12,ENV_14,CSHEET,'                  /* 18-24   */  */
/*                    + 'DSHEET,ESHEET,ENV_DL,ENV_C5,ENV_C3,ENV_C4,ENV_C6,ENV_C65,'       /* 25-32   */  */
/*                    + 'ENV_B4,ENV_B5,ENV_B6,ENV_ITALY,ENV_MONARCH,'                     /* 33-37   */  */
/*                    + 'ENV_PERSONAL,FANFOLD_US,FANFOLD_STD_GERMAN,FANFOLD_LGL_GERMAN'   /* 38-41   */  */
/*                    + ',ISO_B4,JAPANESE_POSTCARD,X11,10X11,15X11,ENV_INVITE,RESERVED_48' /* 42-48   */ */
/*                    + ',RESERVED_49,LETTER_EXTRA,LEGAL_EXTRA,TABLOID_EXTRA,A4_EXTRA'     /* 43-53   */ */
/*                    + ',LETTER_TRANSVERSE,A4_TRANSVERSE,LETTER_EXTRA_TRANSVERSE'         /* 54-56   */ */
/*                    + ',A_PLUS,B_PLUS,LETTER_PLUS,A4_PLUS,A5_TRANSVERSE,B5_TRANSVERSE'   /* 57-62   */ */
/*                    + ',A3_EXTRA,A5_EXTRA,B5_EXTRA,A2'                                                 */
/*                    + ',A3_TRANSVERSE'        /* 67      A3 Transverse 297 x 420 mm         */         */
/*                    + ',A3_EXTRA_TRANSVERSE'  /* 68      A3 Extra Transverse 322 x 445 mm   */         */
                   .

binList = "UPPER,LOWER,MIDDLE,MANUAL,ENVELOPE,ENVMANUAL,AUTO,TRACTOR,SMALLFMT,LARGEFMT,LARGECAPACITY,,,CASSETTE".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS iName trayList BUTTON-2 nBins sPrinter ~
RECT-1 
&Scoped-Define DISPLAYED-OBJECTS iName trayList nBins sPrinter 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-2 
     LABEL "Select the desired bin and test with xPrint" 
     SIZE 55 BY 1.14.

DEFINE VARIABLE iName AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 30
     DROP-DOWN-LIST
     SIZE 103 BY 1 NO-UNDO.

DEFINE VARIABLE nBins AS CHARACTER FORMAT "X(256)":U 
     LABEL "# bins" 
      VIEW-AS TEXT 
     SIZE 6 BY .71
     BGCOLOR 15 FGCOLOR 9  NO-UNDO.

DEFINE VARIABLE sPrinter AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 89 BY .71
     BGCOLOR 15 FGCOLOR 9  NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 107 BY 10.

DEFINE VARIABLE trayList AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE NO-DRAG 
     SIZE 103 BY 8.33
     FONT 0 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     iName AT ROW 2.43 COL 3 NO-LABEL
     trayList AT ROW 5.05 COL 4 NO-LABEL
     BUTTON-2 AT ROW 14.1 COL 52
     nBins AT ROW 4.1 COL 9 COLON-ALIGNED
     sPrinter AT ROW 4.1 COL 16 COLON-ALIGNED NO-LABEL
     RECT-1 AT ROW 3.86 COL 2
     "Please select the printer..." VIEW-AS TEXT
          SIZE 32 BY .95 AT ROW 1.24 COL 3
          FGCOLOR 9 FONT 6
     "4GL / xPrint - Bin names (xprBin.dll)" VIEW-AS TEXT
          SIZE 39 BY .95 AT ROW 14.1 COL 2
          FGCOLOR 9 
     SPACE(70.59) SKIP(0.37)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Bin names".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Dialog-Box
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
                                                                        */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR COMBO-BOX iName IN FRAME Dialog-Frame
   ALIGN-L                                                              */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Bin names */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-2 Dialog-Frame
ON CHOOSE OF BUTTON-2 IN FRAME Dialog-Frame /* Select the desired bin and test with xPrint */
DO:
DEF VAR OK AS LOGICAL INITIAL TRUE.
DEF VAR I  AS INT NO-UNDO.

  IF sPrinter:SCREEN-VALUE = '' THEN DO:
      MESSAGE "Choose a printer..." VIEW-AS ALERT-BOX WARNING.
      RETURN NO-APPLY.
      END.

  ASSIGN TrayList.
  
/*   MESSAGE "You need xPrint 4.99 to test this bin option."      */
/*             SKIP                                               */
/*           "Are you OK ?"                                       */
/*           VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE OK. */
          
IF NOT OK THEN RETURN NO-APPLY.
  I = trayList:LOOKUP(trayList).      
          
  OUTPUT TO "bintest.xpr".
  PUT UNFORMATTED "<preview><PRINTER"
        sPrinter:SCREEN-VALUE
        "><BIN=" int(    ENTRY(I, traylist:PRIVATE-DATA, CHR(1))) ">"
            SKIP(10)
        "<FArial><fgcolor=BLUE><P12>     <P30>Hello World !" SKIP(3)
        "<P12><fgcolor=black>     Test printing from the bin '<B>" replace(trayList:SCREEN-VALUE, '<', '\<') "</B>'" SKIP(1)
        "     On printer '<B>" sPrinter:SCREEN-VALUE "</B>'"
        "<units=mm><AT=280,120><I>Powered by <fgcolor=blue><B>xPrint</B> <fgcolor=black><U>www.4gl.fr".
        
  OUTPUT CLOSE.
  FILE-INFO:FILE-NAME = "bintest.xpr".
  RUN printFile( FILE-INFO:FULL-PATHNAME).
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME iName
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL iName Dialog-Frame
ON VALUE-CHANGED OF iName IN FRAME Dialog-Frame
DO:
  RUN getInfo.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */

set-size(M) = 4097.
RUN getPrinterList(4096, M).

L = get-String(M, 1).

set-size(M) = 0.

iName:LIST-ITEMS = L.

MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  iname:SCREEN-VALUE = entry(1, iname:LIST-ITEMS).

  APPLY "value-changed" TO iname.

  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dialog-Frame  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  HIDE FRAME Dialog-Frame.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dialog-Frame  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  DISPLAY iName trayList nBins sPrinter 
      WITH FRAME Dialog-Frame.
  ENABLE iName trayList BUTTON-2 nBins sPrinter RECT-1 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getInfo Dialog-Frame 
PROCEDURE getInfo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR A               AS MEMPTR.
DEF VAR xprBinReturn    AS CHAR.
DEF VAR nTrays          AS INT NO-UNDO.
DEF VAR i               AS INT.
DEF VAR loadList        AS CHAR.
DEF VAR pList           AS CHAR.
DEF VAR bin#            AS INT.

/*  I have written this code very quickly,
    Sorry for the lack or documentation or ergonomy.
    I promise to do best the next time...
                Marcel
                
    Thinking to the V8 users, I have changed my code to avoid the use of list-item-pairs.
    */
    
ASSIGN FRAME {&FRAME-NAME} iName.

SET-SIZE(A) = 4096.

RUN getBinNames(iName, A).
/*  Structure of returned value 
    Printer name    <13>
    Num of bins     <13>
    Bin Name 1      <13>
    Bin name 2      <13>
     ... ...
    Bin Code 1      <13>
    Bin Code 2      <13>
    ... ...
*/


       
xprBinReturn = GET-STRING(A, 1).
SET-SIZE(A) = 0.

/* MESSAGE xprBinReturn VIEW-AS ALERT-BOX TITLE 'Returned from xprBin.dll'. */


nTrays = int(ENTRY(2, xprBinReturn, CHR(13))).         /* number of trays */

traylist:DELIMITER = CHR(1).

/* V9   version
   ============
DO I = 1 TO nTrays:    
    loadList = loadlist + CHR(1) +
            string(ENTRY(2 + I, xprBinReturn, CHR(13)), "x(30)") + "(" + ENTRY(2 + I + nTrays, xprBinReturn, CHR(13)) + ")"
            + CHR(1) + ENTRY(2 + I + nTrays, xprBinReturn, CHR(13)).
    END.
    
loadlist = SUBSTRING(loadList, 2).
traylist:LIST-ITEM-PAIRS        = loadList.
trayList:SCREEN-VALUE           = ENTRY(2, loadList, CHR(1)).
*/

/* V8-v9 versions
   ============*/
   
DO I = 1 TO nTrays:    
    loadList = loadlist + CHR(1) +
            string(ENTRY(2 + I, xprBinReturn, CHR(13)), "x(30)").
    bin# = INT( ENTRY(2 + I + nTrays, xprBinReturn, CHR(13)) ).
    IF bin# <= NUM-ENTRIES(binList)
    AND ENTRY(bin#, binlist) > "" THEN
                loadlist = loadlist + string("<BIN=" + ENTRY(bin#, binList) + ">", "x(25)").
    ELSE
                loadList = loadlist + FILL(" ", 25).

    loadlist = loadlist + "<BIN=" + ENTRY(2 + I + nTrays, xprBinReturn, CHR(13)) + ">".
    pList    = plist            
            + CHR(1) + ENTRY(2 + I + nTrays, xprBinReturn, CHR(13)).
    END.
    
loadlist = SUBSTRING(loadList, 2).
pList    = SUBSTRING(pList, 2).

traylist:LIST-ITEMS        = loadList.
trayList:PRIVATE-DATA      = pList.

trayList:SCREEN-VALUE           = ENTRY(1, loadList, CHR(1)).
    

NBins:SCREEN-VALUE      = STRING(nTrays).           /* number of bins */
sPrinter:SCREEN-VALUE   = ENTRY(1, xprBinReturn, CHR(13)).     /* Printer name   */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

