&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          skotex           PROGRESS
*/
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
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEFINE INPUT  PARAMETER rButikkForsalj AS ROWID      NO-UNDO.
/* Local Variable Definitions ---                                       */
DEFINE VARIABLE cReturVerdi AS CHARACTER INIT "AVBRYT" NO-UNDO.
DEFINE VARIABLE iIdForslag AS INTEGER    NO-UNDO.

DEFINE BUFFER bButikkForsalj FOR ButikkForsalj.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES butikkforsalj Butiker

/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define FIELDS-IN-QUERY-Dialog-Frame Butiker.Butik Butiker.ButNamn ~
Butiker.KortNavn butikkforsalj.KassererId 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Dialog-Frame ~
butikkforsalj.KassererId 
&Scoped-define ENABLED-TABLES-IN-QUERY-Dialog-Frame butikkforsalj
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-Dialog-Frame butikkforsalj
&Scoped-define QUERY-STRING-Dialog-Frame FOR EACH butikkforsalj ~
      WHERE ROWID(ButikkForsalj) = rButikkForsalj SHARE-LOCK, ~
      EACH Butiker OF butikkforsalj SHARE-LOCK
&Scoped-define OPEN-QUERY-Dialog-Frame OPEN QUERY Dialog-Frame FOR EACH butikkforsalj ~
      WHERE ROWID(ButikkForsalj) = rButikkForsalj SHARE-LOCK, ~
      EACH Butiker OF butikkforsalj SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-Dialog-Frame butikkforsalj Butiker
&Scoped-define FIRST-TABLE-IN-QUERY-Dialog-Frame butikkforsalj
&Scoped-define SECOND-TABLE-IN-QUERY-Dialog-Frame Butiker


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS butikkforsalj.KassererId 
&Scoped-define ENABLED-TABLES butikkforsalj
&Scoped-define FIRST-ENABLED-TABLE butikkforsalj
&Scoped-Define ENABLED-OBJECTS RECT-48 Btn_OK Btn_Cancel CB-Brukte Btn_Help 
&Scoped-Define DISPLAYED-FIELDS Butiker.Butik Butiker.ButNamn ~
Butiker.KortNavn butikkforsalj.KassererId 
&Scoped-define DISPLAYED-TABLES Butiker butikkforsalj
&Scoped-define FIRST-DISPLAYED-TABLE Butiker
&Scoped-define SECOND-DISPLAYED-TABLE butikkforsalj
&Scoped-Define DISPLAYED-OBJECTS CB-Brukte 

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

DEFINE VARIABLE CB-Brukte AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "Brukte" 
     VIEW-AS COMBO-BOX SORT INNER-LINES 10
     LIST-ITEMS "0" 
     DROP-DOWN-LIST
     SIZE 8.8 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-48
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 44 BY 5.24.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Dialog-Frame FOR 
      butikkforsalj, 
      Butiker SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     Btn_OK AT ROW 1.52 COL 45.6
     Butiker.Butik AT ROW 1.57 COL 15 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     Butiker.ButNamn AT ROW 2.57 COL 15 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 22 BY 1
     Btn_Cancel AT ROW 2.76 COL 45.6
     Butiker.KortNavn AT ROW 3.57 COL 15 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.6 BY 1
     butikkforsalj.KassererId AT ROW 4.57 COL 15 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 6.2 BY 1
     CB-Brukte AT ROW 4.57 COL 33 COLON-ALIGNED
     Btn_Help AT ROW 4.76 COL 45.6
     RECT-48 AT ROW 1.1 COL 1
     SPACE(15.99) SKIP(0.08)
    WITH VIEW-AS DIALOG-BOX 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Bytt kassererid"
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
   FRAME-NAME                                                           */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN Butiker.Butik IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Butiker.ButNamn IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Butiker.KortNavn IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX Dialog-Frame
/* Query rebuild information for DIALOG-BOX Dialog-Frame
     _TblList          = "skotex.butikkforsalj,skotex.Butiker OF skotex.butikkforsalj"
     _Options          = "SHARE-LOCK"
     _Where[1]         = "ROWID(ButikkForsalj) = rButikkForsalj"
     _Query            is OPENED
*/  /* DIALOG-BOX Dialog-Frame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON GO OF FRAME Dialog-Frame /* Bytt kassererid */
DO:
  ASSIGN cReturVerdi = "OK".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Bytt kassererid */
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


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:
/*   DEFINE BUFFER bButikkforsalj FOR Butikkforsalj. */
  IF butikkforsalj.KassererId:MODIFIED AND
      INPUT butikkforsalj.KassererId <> butikkforsalj.KassererId THEN DO:
      IF CAN-FIND(bButikkForsalj WHERE bButikkForsalj.Butik = ButikkForsalj.Butik AND
                     bButikkForsalj.KassererId = INPUT ButikkForsalj.KassererId) THEN DO:
          MESSAGE "Kassererid brukt fra før."
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
          APPLY "ENTRY" TO ButikkForsalj.KassererId.
          RETURN NO-APPLY.
      END.
      ELSE DO:
          ASSIGN INPUT ButikkForsalj.KassererId.
      END.
  END.
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
  RUN InitCB.
  RUN enable_UI.
  ASSIGN butikkforsalj.KassererId:SCREEN-VALUE = STRING(iIdForslag).
  APPLY "ENTRY" TO butikkforsalj.KassererId.
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.
RETURN cReturVerdi.

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

  {&OPEN-QUERY-Dialog-Frame}
  GET FIRST Dialog-Frame.
  DISPLAY CB-Brukte 
      WITH FRAME Dialog-Frame.
  IF AVAILABLE Butiker THEN 
    DISPLAY Butiker.Butik Butiker.ButNamn Butiker.KortNavn 
      WITH FRAME Dialog-Frame.
  IF AVAILABLE butikkforsalj THEN 
    DISPLAY butikkforsalj.KassererId 
      WITH FRAME Dialog-Frame.
  ENABLE RECT-48 Btn_OK Btn_Cancel butikkforsalj.KassererId CB-Brukte Btn_Help 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitCB Dialog-Frame 
PROCEDURE InitCB :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cListItems   AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE iButik       AS INTEGER    NO-UNDO.
    FIND bButikkForsalj WHERE ROWID(bButikkForsalj) = rButikkForsalj NO-LOCK NO-ERROR.
    IF AVAIL bButikkForsalj THEN DO:
        ASSIGN iButik = bButikkForsalj.Butik.
        FOR EACH bButikkforsalj NO-LOCK WHERE bButikkforsalj.Butik = iButik BY bButikkforsalj.KassererId:
            ASSIGN iIdForslag = bButikkforsalj.KassererId.
                   cListItems = cListItems + 
                                (IF cListItems <> "" THEN "," ELSE "") + 
                                STRING(bButikkforsalj.KassererId).
        END.
        ASSIGN CB-Brukte:LIST-ITEMS IN FRAME {&FRAME-NAME} = cListItems
               CB-Brukte = INT(ENTRY(1,CB-Brukte:LIST-ITEMS)).
        IF iIdForslag + 1 > 999 THEN DO iIdForslag = 1 TO 999:
            IF NOT CAN-DO(cListItems,STRING(iIdForslag)) THEN
                LEAVE.
        END.
        ELSE
            iIdForslag = iIdForslag + 1.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

