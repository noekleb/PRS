&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
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
&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    DEFINE VAR cKundNrListe AS CHAR INIT "200000001,200000002,200000003,200000004,200000005" NO-UNDO.
&ELSE
    DEFINE INPUT PARAMETER cKundNrListe AS CHAR NO-UNDO.
&ENDIF


DEFINE FRAME PageHeader
   HEADER
     "<ALIGN=BASE><FArial><P12><B><R4><C6>" "Skobutik" "<C35><P24>" "SALDOLISTE" "</B><P12>" SKIP
     "<R6><C6><FROM><R6><C78><LINE>" SKIP
     WITH PAGE-TOP STREAM-IO WIDTH 255.
DEFINE FRAME PageFooter
   HEADER
     "<R62><C6><FROM><R62><C78><LINE>" SKIP
     "<ALIGN=BASE><FArial><P12><B><R63><C6>" "Footer" "<C35>" "XX" "</B>" SKIP
     WITH PAGE-BOTTOM STREAM-IO WIDTH 255.

/* DEFINE VARIABLE iStartRad AS INTEGER    NO-UNDO. */
/* DEFINE VARIABLE iStartCol AS INTEGER    NO-UNDO. */

{xPrint.i}
{runlib.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Btn_OK Btn_Cancel BUTTON-1 Btn_Help ~
iStartRad iStartCol lE65 
&Scoped-Define DISPLAYED-OBJECTS iStartRad iStartCol lE65 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Cancel" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_Help 
     LABEL "&Help" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON BUTTON-1 
     LABEL "SkrivRapport" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE iStartCol AS INTEGER FORMAT ">9":U INITIAL 0 
     LABEL "Start col" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE iStartRad AS INTEGER FORMAT ">9":U INITIAL 0 
     LABEL "StartRad" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE lE65 AS LOGICAL INITIAL no 
     LABEL "E65" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     Btn_OK AT ROW 1.48 COL 51
     Btn_Cancel AT ROW 2.71 COL 51
     BUTTON-1 AT ROW 4.1 COL 21
     Btn_Help AT ROW 4.71 COL 51
     iStartRad AT ROW 6.95 COL 12 COLON-ALIGNED
     iStartCol AT ROW 8.29 COL 12 COLON-ALIGNED
     lE65 AT ROW 9.57 COL 14
     SPACE(41.59) SKIP(1.90)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "<insert dialog title>"
         DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_Cancel.


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

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* <insert dialog title> */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Help
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Help Dialog-Frame
ON CHOOSE OF Btn_Help IN FRAME Dialog-Frame /* Help */
OR HELP OF FRAME {&FRAME-NAME}
DO: /* Call Help Function (or a simple message). */
  MESSAGE "Help for File: {&FILE-NAME}" VIEW-AS ALERT-BOX INFORMATION.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-1 Dialog-Frame
ON CHOOSE OF BUTTON-1 IN FRAME Dialog-Frame /* SkrivRapport */
DO:
  ASSIGN iStartRad
         iStartCol
         lE65.
  RUN SkrivRapport.
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
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
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
  DISPLAY iStartRad iStartCol lE65 
      WITH FRAME Dialog-Frame.
  ENABLE Btn_OK Btn_Cancel BUTTON-1 Btn_Help iStartRad iStartCol lE65 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NySide Dialog-Frame 
PROCEDURE NySide :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT-OUTPUT PARAMETER ioRad   AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER       lNySide AS LOGICAL    NO-UNDO.
  IF lNySide THEN
      PAGE.
  VIEW FRAME PageHeader.
  ASSIGN ioRad = 4.
  RUN SkrivDato(INPUT-OUTPUT ioRad).
  VIEW FRAME PageFooter.
  ASSIGN ioRad = ioRad + 1.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkrivDato Dialog-Frame 
PROCEDURE SkrivDato :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT-OUTPUT PARAMETER ioRad AS INTEGER    NO-UNDO.
/*   PUT UNFORMATTED "<FArial><C6><R" STRING(ioRad) ">" STRING(TODAY)  SKIP. */
  PUT UNFORMATTED "<FArial><P10></B><C6><R-1>" STRING(TODAY)  SKIP.
  IF SEARCH("icon\orderlogo.bmp") <> ? THEN DO:
      ASSIGN FILE-INFO:File-NAME = "icon\orderlogo.bmp".
      PUT UNFORMATTED
          "<TRANSPARENT=false><R" STRING(10) ",2><C78><#3><R3,25><C60,2><IMAGE#3="
          FILE-INFO:FULL-PATHNAME + ">".
  END.
  ASSIGN ioRad = ioRad + 1.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkrivRapport Dialog-Frame 
PROCEDURE SkrivRapport :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iRad         AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iCount       AS INTEGER    NO-UNDO.
  DEFINE VARIABLE pcRappFil    AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE lFirst       AS LOGICAL    NO-UNDO.
  DEFINE VARIABLE iPageSize    AS INTEGER INIT 64   NO-UNDO.
  DEFINE VARIABLE cPostAdresse AS CHARACTER  NO-UNDO.
  IF VALID-HANDLE(wLibHandle) THEN
    RUN GetTempFileName IN wLibHandle ("SaldoListe", "xpr", OUTPUT pcRappFil). 

  /* Åpner stream til skriverfil. */
  OUTPUT TO VALUE(pcRappFil) PAGED PAGE-SIZE VALUE(iPageSize).
  PUT CONTROL '<PREVIEW=ZoomToWidth>'.
  DO iCount = 1 TO NUM-ENTRIES(cKundNrListe):
      FIND Kunde WHERE Kunde.KundeNr = DECI(ENTRY(iCount,cKundNrListe)) NO-LOCK NO-ERROR.
      IF NOT AVAIL Kunde THEN
          NEXT.
      FIND Post WHERE Post.PostNr = Kunde.PostNr.
      ASSIGN cPostAdresse = IF AVAIL Post THEN Post.Beskrivelse ELSE "".
      RUN NySide(INPUT-OUTPUT iRad,lFirst).
      ASSIGN lFirst = TRUE.
      PUT UNFORMATTED
          "<FArial><P12><R" STRING(iStartRad) "><C" STRING(iStartCol) ">" 
                                          Kunde.Navn SKIP
          "<R+0><C" STRING(iStartCol) ">" Kunde.Adresse1 SKIP
          "<R+0><C" STRING(iStartCol) ">" Kunde.Adresse2 SKIP
          "<R+0><C" STRING(iStartCol) ">" TRIM(Kunde.PostNr) " " cPostAdresse SKIP
          "<R+0><C" STRING(iStartCol) ">" IF Kunde.Land = "" THEN "" ELSE Kunde.Land SKIP.
      IF lE65 THEN
          PUT UNFORMATTED "<UNITS=MM><AT=108,0><FROM><C3><LINE>" SKIP.
  END.
  OUTPUT TO TERMINAL.

  /* Klargjør rapportfilnavnet */
  ASSIGN FILE-INFO:File-NAME = pcRappFil.
    
  /* Sender filen til visning og utskrift. */
 RUN PrintPDF(FILE-INFO:FULL-PATHNAME, 'POLYGON SOFTWARE AS', 'A1a9T4h4e2h_mqe2mbka' ). 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

