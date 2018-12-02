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

{tmphtmlrapp.i &New = "New"}

/* Parameters Definitions ---                                           */
&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    DEFINE VAR wParent AS WIDGET-HANDLE NO-UNDO.
    DEFINE VAR ch_Grid AS COM-HANDLE    NO-UNDO.
    define var wArtBasRecid as recid    no-undo.
&ELSE
    DEFINE INPUT PARAMETER wParent AS WIDGET-HANDLE NO-UNDO.
    DEFINE INPUT PARAMETER ch_Grid AS COM-HANDLE    NO-UNDO.
    define input parameter wArtBasRecid as recid no-undo.
&ENDIF
/* Local Variable Definitions ---                                       */
&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    DEFINE            TEMP-TABLE Overfor
&ELSE
    DEFINE     SHARED TEMP-TABLE Overfor
&ENDIF
        FIELD FraButik AS CHAR LABEL "Från butik"
        FIELD TilButik AS CHAR LABEL "Till butik"
        FIELD FraStrl  AS CHAR LABEL "Från Strl"
        FIELD TilStrl  AS CHAR LABEL "Till Strl"
        FIELD Antal    AS INTE LABEL "Antal"
        FIELD FraRow   AS INTE
        FIELD FraCol   AS INTE
        FIELD TilRow   AS INTE
        FIELD TilCol   AS INTE
        INDEX FraButik IS PRIMARY FraButik ASCENDING
                                  FraStrl  ASCENDING
        INDEX TilButik TilButik ASCENDING
                       TilStrl  ASCENDING.


{runlib.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame
&Scoped-define BROWSE-NAME BROWSE-Overfor

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Overfor

/* Definitions for BROWSE BROWSE-Overfor                                */
&Scoped-define FIELDS-IN-QUERY-BROWSE-Overfor FraButik TilButik FraStrl TilStrl Antal   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-Overfor   
&Scoped-define SELF-NAME BROWSE-Overfor
&Scoped-define OPEN-QUERY-BROWSE-Overfor OPEN QUERY {&SELF-NAME} FOR EACH Overfor USE-INDEX FraButik.
&Scoped-define TABLES-IN-QUERY-BROWSE-Overfor Overfor
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-Overfor Overfor


/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define OPEN-BROWSERS-IN-QUERY-Dialog-Frame ~
    ~{&OPEN-QUERY-BROWSE-Overfor}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BROWSE-Overfor Btn_Avsluta Btn_TaBort B-Rapp ~
Btn_Help 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-Rapp 
     LABEL "Rapport" 
     SIZE 15 BY 1.14.

DEFINE BUTTON Btn_Avsluta AUTO-GO 
     LABEL "Avsluta" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_Help 
     LABEL "&Help" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_TaBort AUTO-END-KEY 
     LABEL "Ta bort" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-Overfor FOR 
      Overfor SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-Overfor
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-Overfor Dialog-Frame _FREEFORM
  QUERY BROWSE-Overfor DISPLAY
      FraButik FORMAT "X(6)"
      TilButik FORMAT "X(6)"
      FraStrl FORMAT  "X(8)"
      TilStrl  FORMAT "X(8)"
      Antal    FORMAT ">,>>9-"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 48 BY 10.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     BROWSE-Overfor AT ROW 1.19 COL 2
     Btn_Avsluta AT ROW 1.24 COL 51
     Btn_TaBort AT ROW 2.43 COL 51
     B-Rapp AT ROW 3.67 COL 51
     Btn_Help AT ROW 10.05 COL 51
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Översikt överföringar"
         DEFAULT-BUTTON Btn_Avsluta CANCEL-BUTTON Btn_TaBort.


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
/* BROWSE-TAB BROWSE-Overfor 1 Dialog-Frame */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-Overfor
/* Query rebuild information for BROWSE BROWSE-Overfor
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH Overfor USE-INDEX FraButik.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BROWSE-Overfor */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Översikt överföringar */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Rapp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Rapp Dialog-Frame
ON CHOOSE OF B-Rapp IN FRAME Dialog-Frame /* Rapport */
DO:
  run Html-Rapport.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-Overfor
&Scoped-define SELF-NAME BROWSE-Overfor
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-Overfor Dialog-Frame
ON VALUE-CHANGED OF BROWSE-Overfor IN FRAME Dialog-Frame
DO:
    IF VALID-HANDLE(ch_Grid) THEN
        ASSIGN Btn_TaBort:SENSITIVE IN FRAME {&FRAME-NAME} = 
           INT(ch_Grid:TextMatrix(Overfor.TilRow,Overfor.TilCol)) >= Overfor.Antal.
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


&Scoped-define SELF-NAME Btn_TaBort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_TaBort Dialog-Frame
ON CHOOSE OF Btn_TaBort IN FRAME Dialog-Frame /* Ta bort */
DO:
    IF VALID-HANDLE(wParent) THEN
        RUN Angra IN wParent (ROWID(Overfor)).
    BROWSE BROWSE-Overfor:DELETE-CURRENT-ROW( ).
    IF BROWSE BROWSE-Overfor:FOCUSED-ROW = ? THEN
        APPLY "WINDOW-CLOSE" TO FRAME {&FRAME-NAME}.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


find ArtBas no-lock where
  recid(ArtBAs) = wArtBasRecid no-error.
if not available ArtBAs then 
  return "AVBRYT".

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
  CREATE overfor.
      ASSIGN FraButik = "100"
             TilButik = "200"
             FraStrl  = "34.5"
             TilStrl  = "35.0"
             Antal    = 23.
&ENDIF
  RUN enable_UI.
  APPLY "VALUE-CHANGED" TO BROWSE BROWSE-Overfor.
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
  ENABLE BROWSE-Overfor Btn_Avsluta Btn_TaBort B-Rapp Btn_Help 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Html-Rapport Dialog-Frame 
PROCEDURE Html-Rapport :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE VAR wTitle       AS CHAR NO-UNDO.
  DEFINE VAR wRapport     AS CHAR NO-UNDO.
  DEFINE VAR wHead1       AS CHAR NO-UNDO.
  DEFINE VAR wHead1Set    AS CHAR NO-UNDO.
  DEFINE VAR wHead2       AS CHAR NO-UNDO.
  DEFINE VAR wColHead     AS CHAR NO-UNDO.
  DEFINE VAR wColHeadForm AS CHAR NO-UNDO.
  DEFINE VAR wTabell      AS CHAR NO-UNDO.
  DEFINE VAR wQY          AS CHAR NO-UNDO.
  DEFINE VAR wFields      AS CHAR NO-UNDO.
  DEFINE VAR wDokTyp      AS CHAR NO-UNDO.

  /* {tmphtmlrapp.i &New = "New"} --- Skal ligge i definisjonsblokken */
  
  {sww.i}
  
  /* Tømmer tmp buffer */
  for each tmpHtml: delete tmpHtml. end.

  /* Bygger tmpbuffer */
  for each Overfor:
    create tmpHtml.
    assign
      tmpHtml.Data01 = string(ArtBas.Vg,"zzz9")
      tmpHtml.Data02 = string(ArtBas.LopNr,"zzz9")
      tmpHtml.Data03 = string(Overfor.TilButik)
      tmpHtml.Data04 = string(Overfor.FraButik)
      tmpHtml.Data05 = string(Overfor.TilStrl)
      tmpHtml.Data06 = string(Overfor.FraStrl)
      tmpHtml.Data07 = string(Overfor.Antal,"zzz9").
  end.
  
  ASSIGN wTitle       = "Rapportgenerator"
         wRapport     = "Overføringer"
         wHead1       = "Rapport " + wRapport
         wHead1Set    = "80%,,1,0,2," + "7"
         wHead2       = "Bruker: " + USERID("dictdb") + "<Html:BreakLn><Html:BreakLn>" +
                      "<Html:CenterOn><Html:BoldOn>Dato: " + string(today,"99/99/9999")
         wColHead     = "VareGr|LøpneNr|TilBut|FraBut|TilStr|FraStr|Antal"
         wFields      = "Data01,Data02,Data03,Data04,Data05,Data06,Data07"
         wColHeadForm = "L,5%|L,20%|L,20%"
         wTabell      = "tmpHtml"
         wQY          = "for each " + wTabell + " NO-LOCK by Data01 by Data02 by Data03"
         wDokTyp      = "Html".
  
  RUN stdrapphtml2.p (wTitle,wRapport,wHead1,wHead1Set,wHead2,wColHead,wColHeadForm,wTabell,wQY,wFields,wDokTyp).
  if valid-handle(wLibHandle) then
    RUN OpenWeb in wLibHandle ("rapport.htm").
  {swn.i}


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

