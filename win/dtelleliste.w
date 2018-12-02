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
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
  def var cParaListe as CHAR NO-UNDO.
&ELSE
  DEF OUTPUT PARAMETER cParaListe AS CHAR NO-UNDO.
&ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS B-SokLev FI-Vg FI-LevNr CB-Farg CB-MatKod ~
CB-Sasong FI-LevKod CB-Diff CB-Talt CB-Lager RS-Verdi Btn_OK Btn_Cancel ~
FILL-IN-8 RECT-63 
&Scoped-Define DISPLAYED-OBJECTS FI-Vg FI-LevNr CB-Farg CB-MatKod CB-Sasong ~
FI-LevKod CB-Diff CB-Talt CB-Lager RS-Verdi FILL-IN-8 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-SokLev  NO-FOCUS
     LABEL "..." 
     SIZE 4.63 BY 1.

DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Avslutt" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE CB-Diff AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Differanse" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "[Alle]", 0,
                     "< 0", 1,
                     "> 0", 2,
                     "<> 0", 3,
                     "= 0", 4
     DROP-DOWN-LIST
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE CB-Farg AS INTEGER FORMAT "->,>>>,>>9":U 
     LABEL "Farge" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "[Alle]", 0
     DROP-DOWN-LIST
     SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE CB-Lager AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Antall lager" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "[Alle]", 0,
                     "< 0", 1,
                     "> 0", 2,
                     "<> 0", 3,
                     "= 0", 4
     DROP-DOWN-LIST
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE CB-MatKod AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Material" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "[Alle]",0
     DROP-DOWN-LIST
     SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE CB-Sasong AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Sesong" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "[Alle]",0
     DROP-DOWN-LIST
     SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE CB-Talt AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Antall talt" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "[Alle]", 0,
                     "< 0", 1,
                     "> 0", 2,
                     "<> 0", 3,
                     "= 0", 4
     DROP-DOWN-LIST
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE FI-LevKod AS CHARACTER FORMAT "X(256)":U 
     LABEL "LevArtNr" 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE FI-LevNr AS INTEGER FORMAT ">>>>>9":U 
     LABEL "Leverandør" 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Vg AS INTEGER FORMAT ">>>>>9":U 
     LABEL "Varegruppe" 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-8 AS CHARACTER FORMAT "X(256)":U INITIAL "Utskriftskriterier (BRUKES IKKE!!!!)" 
      VIEW-AS TEXT 
     SIZE 65 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE RS-Verdi AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Vektet varekost", 0,
"Salgspris inkl.mva", 1
     SIZE 22 BY 2.14 NO-UNDO.

DEFINE RECTANGLE RECT-63
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 73 BY 7.62.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     B-SokLev AT ROW 3.86 COL 28 NO-TAB-STOP 
     FI-Vg AT ROW 2.91 COL 15 COLON-ALIGNED
     FI-LevNr AT ROW 3.86 COL 15 COLON-ALIGNED
     CB-Farg AT ROW 4.81 COL 15 COLON-ALIGNED
     CB-MatKod AT ROW 5.76 COL 15 COLON-ALIGNED
     CB-Sasong AT ROW 6.71 COL 15 COLON-ALIGNED
     FI-LevKod AT ROW 7.67 COL 15 COLON-ALIGNED
     CB-Diff AT ROW 2.91 COL 56 COLON-ALIGNED
     CB-Talt AT ROW 3.86 COL 56 COLON-ALIGNED
     CB-Lager AT ROW 4.81 COL 56 COLON-ALIGNED
     RS-Verdi AT ROW 7.19 COL 52 NO-LABEL
     Btn_OK AT ROW 9.81 COL 2
     Btn_Cancel AT ROW 9.81 COL 60
     FILL-IN-8 AT ROW 1.24 COL 2 NO-LABEL
     RECT-63 AT ROW 1.95 COL 2
     SPACE(0.59) SKIP(1.47)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Utskrift telleliste"
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
   Custom                                                               */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-8 IN FRAME Dialog-Frame
   ALIGN-L                                                              */
ASSIGN 
       FILL-IN-8:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON GO OF FRAME Dialog-Frame /* Utskrift telleliste */
DO:
    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN
            cParaListe =  FI-Vg:SCREEN-VALUE + CHR(1) +
                          FI-LevNr:SCREEN-VALUE + CHR(1) + 
                          FI-LevNr:SCREEN-VALUE + CHR(1) + 
                          CB-Farg:SCREEN-VALUE + CHR(1) + 
                          CB-MatKod:SCREEN-VALUE + CHR(1) + 
                          CB-Sasong:SCREEN-VALUE + CHR(1) + 
                          FI-LevKod:SCREEN-VALUE + CHR(1) + 
                          CB-Diff:SCREEN-VALUE + CHR(1) + 
                          CB-Talt:SCREEN-VALUE + CHR(1) + 
                          CB-Lager:SCREEN-VALUE + CHR(1) + 
                          RS-Verdi:SCREEN-VALUE 
            .
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Utskrift telleliste */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SokLev
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SokLev Dialog-Frame
ON CHOOSE OF B-SokLev IN FRAME Dialog-Frame /* ... */
DO:
    {soek.i
      &Felt       = FI-LevNr
      &FRAME-NAME = Dialog-Frame
      &Program    = d-blevbas.w 
    }
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Sikrer at det er nok entries i listen. */
ASSIGN
    cParaListe = cParaListe + FILL(CHR(1),10)
    .

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  {lng.i}

  APPLY "ENTRY" TO FI-Vg.

  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

MESSAGE cParaListe
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

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
  DISPLAY FI-Vg FI-LevNr CB-Farg CB-MatKod CB-Sasong FI-LevKod CB-Diff CB-Talt 
          CB-Lager RS-Verdi FILL-IN-8 
      WITH FRAME Dialog-Frame.
  ENABLE B-SokLev FI-Vg FI-LevNr CB-Farg CB-MatKod CB-Sasong FI-LevKod CB-Diff 
         CB-Talt CB-Lager RS-Verdi Btn_OK Btn_Cancel FILL-IN-8 RECT-63 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

