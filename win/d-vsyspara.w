&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
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
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
  DEFINE VAR wRecid  as recid NO-UNDO.
  define var wModus  as char  no-undo.
  Define var wRecid2 as recid no-undo.
  
&ELSE
  DEFINE INPUT-output PARAMETER wRecid  as recid NO-UNDO.
  define input        parameter wModus  as char  no-undo.
  Define input        parameter wRecid2 as recid no-undo.
&ENDIF

/* Local Variable Definitions ---                                       */

{runlib.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES SysPara

/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define FIELDS-IN-QUERY-Dialog-Frame SysPara.ParaNr ~
SysPara.Beskrivelse SysPara.Parameter1 SysPara.Parameter2 ~
SysPara.Hjelpetekst1 SysPara.Hjelpetekst2 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Dialog-Frame SysPara.Beskrivelse ~
SysPara.Parameter1 SysPara.Parameter2 SysPara.Hjelpetekst1 ~
SysPara.Hjelpetekst2 
&Scoped-define ENABLED-TABLES-IN-QUERY-Dialog-Frame SysPara
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-Dialog-Frame SysPara
&Scoped-define QUERY-STRING-Dialog-Frame FOR EACH SysPara SHARE-LOCK
&Scoped-define OPEN-QUERY-Dialog-Frame OPEN QUERY Dialog-Frame FOR EACH SysPara SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-Dialog-Frame SysPara
&Scoped-define FIRST-TABLE-IN-QUERY-Dialog-Frame SysPara


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS SysPara.Beskrivelse SysPara.Parameter1 ~
SysPara.Parameter2 SysPara.Hjelpetekst1 SysPara.Hjelpetekst2 
&Scoped-define ENABLED-TABLES SysPara
&Scoped-define FIRST-ENABLED-TABLE SysPara
&Scoped-Define ENABLED-OBJECTS RECT-1 Btn_OK Btn_Cancel Btn_Help 
&Scoped-Define DISPLAYED-FIELDS SysPara.ParaNr SysPara.Beskrivelse ~
SysPara.Parameter1 SysPara.Parameter2 SysPara.Hjelpetekst1 ~
SysPara.Hjelpetekst2 
&Scoped-define DISPLAYED-TABLES SysPara
&Scoped-define FIRST-DISPLAYED-TABLE SysPara


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Avbryt" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_Help 
     LABEL "&Hjelp" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 3 GRAPHIC-EDGE  NO-FILL   
     SIZE 122.2 BY 6.57.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Dialog-Frame FOR 
      SysPara SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     SysPara.ParaNr AT ROW 1.76 COL 15 COLON-ALIGNED FORMAT ">>>>>9"
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
     SysPara.Beskrivelse AT ROW 1.76 COL 24.2 COLON-ALIGNED NO-LABEL FORMAT "X(245)"
          VIEW-AS FILL-IN 
          SIZE 44.4 BY 1
     SysPara.Parameter1 AT ROW 3 COL 15 COLON-ALIGNED FORMAT "X(3245)"
          VIEW-AS FILL-IN 
          SIZE 104.4 BY 1
     SysPara.Parameter2 AT ROW 4 COL 15 COLON-ALIGNED FORMAT "X(3245)"
          VIEW-AS FILL-IN 
          SIZE 104.4 BY 1
     SysPara.Hjelpetekst1 AT ROW 5.29 COL 15 COLON-ALIGNED FORMAT "X(3245)"
          VIEW-AS FILL-IN 
          SIZE 104.4 BY 1
     SysPara.Hjelpetekst2 AT ROW 6.29 COL 15 COLON-ALIGNED FORMAT "X(3245)"
          VIEW-AS FILL-IN 
          SIZE 104.4 BY 1
     Btn_OK AT ROW 7.95 COL 1.8
     Btn_Cancel AT ROW 7.95 COL 17.4
     Btn_Help AT ROW 7.95 COL 108.6
     RECT-1 AT ROW 1.19 COL 1.8
     SPACE(0.59) SKIP(1.33)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Vedlikehold systemparametre - Gruppe"
         CANCEL-BUTTON Btn_Cancel.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
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

/* SETTINGS FOR FILL-IN SysPara.Beskrivelse IN FRAME Dialog-Frame
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN SysPara.Hjelpetekst1 IN FRAME Dialog-Frame
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN SysPara.Hjelpetekst2 IN FRAME Dialog-Frame
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN SysPara.Parameter1 IN FRAME Dialog-Frame
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN SysPara.Parameter2 IN FRAME Dialog-Frame
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN SysPara.ParaNr IN FRAME Dialog-Frame
   NO-ENABLE EXP-FORMAT                                                 */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX Dialog-Frame
/* Query rebuild information for DIALOG-BOX Dialog-Frame
     _TblList          = "SkoTex.SysPara"
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX Dialog-Frame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Vedlikehold systemparametre - Gruppe */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Help
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Help Dialog-Frame
ON CHOOSE OF Btn_Help IN FRAME Dialog-Frame /* Hjelp */
OR HELP OF FRAME {&FRAME-NAME}
DO: /* Call Help Function (or a simple message). */
 {diahelp.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:
  run LagrePost.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.

/* Henter posten som skal endres. */
if wModus = "ENDRE" then
  do:
    find SysPara no-lock where
      recid(SysPara) = wRecid no-error.
    if not available SysPara then
      do:
        message "Finner ingen post å endre!" 
          view-as alert-box title "Feil ved initiering".
        return no-apply.
      end. 
  end.
find SysGruppe no-lock where 
  recid(SysGruppe) = wRecid2 no-error.
find sysHode   of SysGruppe no-lock no-error.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

  {lng.i} RUN enable_UI.

  if wModus = "Ny" then
    assign
      SysPara.ParaNr:sensitive = true.

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
  IF AVAILABLE SysPara THEN 
    DISPLAY SysPara.ParaNr SysPara.Beskrivelse SysPara.Parameter1 
          SysPara.Parameter2 SysPara.Hjelpetekst1 SysPara.Hjelpetekst2 
      WITH FRAME Dialog-Frame.
  ENABLE RECT-1 SysPara.Beskrivelse SysPara.Parameter1 SysPara.Parameter2 
         SysPara.Hjelpetekst1 SysPara.Hjelpetekst2 Btn_OK Btn_Cancel Btn_Help 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LagrePost Dialog-Frame 
PROCEDURE LagrePost :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
do with frame Dialog-Frame transaction:
  /* Sjekker nye poster. */
  if wModus = "Ny" then
    do:
      if can-find(SysPara where
                  SysPara.SysHId = SysHode.SysHId and
                  SysPara.SysGr  = SysGruppe.SysGr and
                  SysPara.ParaNr = int(SysPara.ParaNr:screen-value)) or
         int(SysPara.ParaNr:screen-value) = 0 then
        do:
          if int(SysPara.ParaNr:screen-value) = 0 then
            do:
              message "ParameterNr må være større enn 0"
                view-as alert-box title "Lagringsfeil".
              return no-apply.
            end.      
          else
            message "ParameterNr finnes allerede med Nr:" SysPara.ParaNr:screen-value
              view-as alert-box title "Lagringsfeil".
          return "AVBRYT".
        end.
      create SysPara.
      assign 
        SysPara.SysHId = SysHode.SysHId 
        SysPara.SysGr  = SysGruppe.SysGr
        SysPara.ParaNr = int(SysPara.ParaNr:screen-value)
        wRecid         = recid(SysPara).
    end.
  else 
    find SysPara Exclusive-lock where
      recid(SysPara) = wRecid no-error.
  assign
    SysPara.Beskrivelse
    SysPara.Parameter1
    SysPara.Parameter2
    SysPara.HjelpeTekst1
    SysPara.HjelpeTekst2.
end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

