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
  def    var wRecid2 as recid no-undo.
  
&ELSE
  DEFINE INPUT-output PARAMETER wRecid as recid  NO-UNDO.
  define input        parameter wModus as char   no-undo.
  def    input        parameter wRecid2 as recid no-undo.
&ENDIF

/* Local Variable Definitions ---                                       */

{runlib.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES SysGruppe

/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define FIELDS-IN-QUERY-Dialog-Frame SysGruppe.SysGr ~
SysGruppe.Beskrivelse SysGruppe.Hjelpetekst 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Dialog-Frame SysGruppe.Beskrivelse ~
SysGruppe.Hjelpetekst 
&Scoped-define ENABLED-TABLES-IN-QUERY-Dialog-Frame SysGruppe
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-Dialog-Frame SysGruppe
&Scoped-define OPEN-QUERY-Dialog-Frame OPEN QUERY Dialog-Frame FOR EACH SysGruppe SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-Dialog-Frame SysGruppe
&Scoped-define FIRST-TABLE-IN-QUERY-Dialog-Frame SysGruppe


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS SysGruppe.Beskrivelse SysGruppe.Hjelpetekst 
&Scoped-define ENABLED-TABLES SysGruppe
&Scoped-define FIRST-ENABLED-TABLE SysGruppe
&Scoped-define DISPLAYED-TABLES SysGruppe
&Scoped-define FIRST-DISPLAYED-TABLE SysGruppe
&Scoped-Define ENABLED-OBJECTS RECT-1 Btn_OK Btn_Cancel Btn_Help 
&Scoped-Define DISPLAYED-FIELDS SysGruppe.SysGr SysGruppe.Beskrivelse ~
SysGruppe.Hjelpetekst 

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
     SIZE 68.8 BY 2.86.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Dialog-Frame FOR 
      SysGruppe SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     SysGruppe.SysGr AT ROW 1.67 COL 14.8 COLON-ALIGNED
          LABEL "Gruppe"
          VIEW-AS FILL-IN 
          SIZE 7.6 BY 1
     SysGruppe.Beskrivelse AT ROW 1.67 COL 23 COLON-ALIGNED NO-LABEL FORMAT "X(245)"
          VIEW-AS FILL-IN 
          SIZE 43.8 BY 1
     SysGruppe.Hjelpetekst AT ROW 2.71 COL 14.8 COLON-ALIGNED FORMAT "X(245)"
          VIEW-AS FILL-IN 
          SIZE 52 BY 1
     Btn_OK AT ROW 4.24 COL 1.8
     Btn_Cancel AT ROW 4.24 COL 17.4
     Btn_Help AT ROW 4.24 COL 55.2
     RECT-1 AT ROW 1.19 COL 1.8
     SPACE(0.19) SKIP(1.33)
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
                                                                        */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN SysGruppe.Beskrivelse IN FRAME Dialog-Frame
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN SysGruppe.Hjelpetekst IN FRAME Dialog-Frame
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN SysGruppe.SysGr IN FRAME Dialog-Frame
   NO-ENABLE EXP-LABEL                                                  */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX Dialog-Frame
/* Query rebuild information for DIALOG-BOX Dialog-Frame
     _TblList          = "SkoTex.SysGruppe"
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

find SysHode no-lock where
  recid(SysHode) = wRecid2 no-error.
if not available SysHode then
  do:
    message "Ingen SysHode tilgjengelig" view-as alert-box.
  end.

/* Henter posten som skal endres. */
if wModus = "ENDRE" then
  do:
    find SysGruppe no-lock where
      recid(SysGruppe) = wRecid no-error.
    if not available SysGruppe then
      do:
        message "Finner ingen post å endre!" 
          view-as alert-box title "Feil ved initiering".
        return no-apply.
      end. 
  end.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

  {lng.i} RUN enable_UI.

  if wModus = "Ny" then
    assign
      SysGruppe.SysGr:sensitive = true.

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
  IF AVAILABLE SysGruppe THEN 
    DISPLAY SysGruppe.SysGr SysGruppe.Beskrivelse SysGruppe.Hjelpetekst 
      WITH FRAME Dialog-Frame.
  ENABLE RECT-1 SysGruppe.Beskrivelse SysGruppe.Hjelpetekst Btn_OK Btn_Cancel 
         Btn_Help 
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
      if can-find(SysGruppe where
                  SysGruppe.SysHId = SysHode.SysHId and
                  SysGruppe.SysGr  = int(SysGruppe.SysGr:screen-value)) or
         int(SysGruppe.SysGr:screen-value) = 0 then
        do:
          if int(SysGruppe.SysGr:screen-value) = 0 then
            do:
              message "Gruppe-Id må være større enn 0"
                view-as alert-box title "Lagringsfeil".
              return no-apply.
            end.      
          else
            message "Gruppe-Id finnes allerede med Id-Nr:" SysGruppe.SysGr:screen-value
              view-as alert-box title "Lagringsfeil".
          return "AVBRYT".
        end.
      create SysGruppe.
      assign 
        SysGruppe.SysHId = SysHode.SysHId 
        SysGruppe.SysGr  = int(SysGruppe.SysGr:screen-value)
        wRecid         = recid(SysGruppe).
    end.
  else 
    find SysGruppe Exclusive-lock where
      recid(SysGruppe) = wRecid no-error.
  assign
    SysGruppe.Beskrivelse
    SysGruppe.HjelpeTekst.
end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

