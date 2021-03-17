&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
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
DEF INPUT PARAMETER cOrg    AS CHAR NO-UNDO.
DEF INPUT PARAMETER cENDRET AS CHAR NO-UNDO.

/* Local Variable Definitions ---                                       */

DEF VAR lArtikkelNr  LIKE ArtBas.ArtikkelNr NO-UNDO.
DEF VAR lModellFarge LIKE ArtBas.ModellFarge NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS T-LevNr T-Sasong T-Vg T-VmId T-ProdNr ~
T-RAvdNr T-WebButikkArtikkel T-VPIBildeKode Btn_OK Btn_Cancel 
&Scoped-Define DISPLAYED-OBJECTS T-LevNr T-Sasong T-Vg T-VmId T-ProdNr ~
T-RAvdNr T-WebButikkArtikkel T-VPIBildeKode FILL-IN-20 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "AVbryt" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE FILL-IN-20 AS CHARACTER FORMAT "X(256)":U INITIAL "Kopier informasjon til de andre artiklene i modellen" 
      VIEW-AS TEXT 
     SIZE 62 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE T-LevNr AS LOGICAL INITIAL NO 
     LABEL "Leverandørnummer" 
     VIEW-AS TOGGLE-BOX
     SIZE 49 BY .81 NO-UNDO.

DEFINE VARIABLE T-ProdNr AS LOGICAL INITIAL NO 
     LABEL "Produsent" 
     VIEW-AS TOGGLE-BOX
     SIZE 50 BY .81 NO-UNDO.

DEFINE VARIABLE T-RAvdNr AS LOGICAL INITIAL NO 
     LABEL "Vareområde" 
     VIEW-AS TOGGLE-BOX
     SIZE 48 BY .81 NO-UNDO.

DEFINE VARIABLE T-Sasong AS LOGICAL INITIAL NO 
     LABEL "Sesong" 
     VIEW-AS TOGGLE-BOX
     SIZE 51 BY .81 NO-UNDO.

DEFINE VARIABLE T-Vg AS LOGICAL INITIAL NO 
     LABEL "Varegruppe" 
     VIEW-AS TOGGLE-BOX
     SIZE 50 BY .81 NO-UNDO.

DEFINE VARIABLE T-VmId AS LOGICAL INITIAL NO 
     LABEL "Varemerke" 
     VIEW-AS TOGGLE-BOX
     SIZE 50 BY .81 NO-UNDO.

DEFINE VARIABLE T-VPIBildeKode AS LOGICAL INITIAL NO 
     LABEL "VPIBildekode" 
     VIEW-AS TOGGLE-BOX
     SIZE 52 BY .81 NO-UNDO.

DEFINE VARIABLE T-WebButikkArtikkel AS LOGICAL INITIAL NO 
     LABEL "Aktivert i nettbutikk" 
     VIEW-AS TOGGLE-BOX
     SIZE 50 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     T-LevNr AT ROW 3 COL 13
     T-Sasong AT ROW 4 COL 13
     T-Vg AT ROW 5 COL 13
     T-VmId AT ROW 6 COL 13
     T-ProdNr AT ROW 7 COL 13
     T-RAvdNr AT ROW 8 COL 13
     T-WebButikkArtikkel AT ROW 9 COL 13
     T-VPIBildeKode AT ROW 10 COL 13
     Btn_OK AT ROW 11.48 COL 34.4
     Btn_Cancel AT ROW 11.48 COL 50
     FILL-IN-20 AT ROW 1.62 COL 3 NO-LABEL
     SPACE(0.19) SKIP(10.42)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Kopier i modell"
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

/* SETTINGS FOR FILL-IN FILL-IN-20 IN FRAME Dialog-Frame
   NO-ENABLE ALIGN-L                                                    */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Kopier i modell */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:
  RUN KopierTilModell.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT EQ ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  
  
    ASSIGN T-LevNr:hidden             = ENTRY(2,cOrg,'|') = entry(2,cEndret,'|') no-error.
    ASSIGN T-Sasong:hidden            = ENTRY(3,cOrg,'|') = entry(3,cEndret,'|') no-error.
    ASSIGN T-Vg:hidden                = ENTRY(4,cOrg,'|') = entry(4,cEndret,'|') no-error.
    ASSIGN T-VmId:hidden              = ENTRY(5,cOrg,'|') = entry(5,cEndret,'|') no-error.
    ASSIGN T-ProdNr:hidden            = ENTRY(6,cOrg,'|') = entry(6,cEndret,'|') no-error.
    ASSIGN T-RAvdNr:hidden            = ENTRY(7,cOrg,'|') = entry(7,cEndret,'|') no-error.
    ASSIGN T-WebButikkArtikkel:hidden = ENTRY(8,cOrg,'|') = entry(8,cEndret,'|') no-error.
    ASSIGN T-VPIBildeKode:hidden      = ENTRY(9,cOrg,'|') = entry(9,cEndret,'|') no-error.
  
  
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
  DISPLAY T-LevNr T-Sasong T-Vg T-VmId T-ProdNr T-RAvdNr T-WebButikkArtikkel 
          T-VPIBildeKode FILL-IN-20 
      WITH FRAME Dialog-Frame.
  ENABLE T-LevNr T-Sasong T-Vg T-VmId T-ProdNr T-RAvdNr T-WebButikkArtikkel 
         T-VPIBildeKode Btn_OK Btn_Cancel 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE KopierTilModell Dialog-Frame 
PROCEDURE KopierTilModell :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DO WITH FRAME Dialog-Frame TRANSACTION:
  ASSIGN
    lArtikkelNr = dec(ENTRY(1,cOrg,'|'))
    .
  FIND ArtBas NO-LOCK WHERE
    ArtBas.ArtikkelNr = lArtikkelNr NO-ERROR.
  IF NOT AVAILABLE ArtBas THEN RETURN.
  ASSIGN
    lModellFarge = ArtBas.ModellFarge.
  IF lArtikkelNr = 0 OR lModellFarge = 0 THEN RETURN.
  
  FOR EACH ArtBas EXCLUSIVE-LOCK WHERE
    ArtBas.ModellFarge = lModellFarge:
    
    IF t-LevNr:checked THEN ArtBas.LevNr = int(ENTRY(2,cEndret,'|')) NO-ERROR.
    IF t-Sasong:checked THEN ArtBas.Sasong = int(ENTRY(3,cEndret,'|')) NO-ERROR.
    IF t-Vg:checked THEN ArtBas.Vg = int(ENTRY(4,cEndret,'|')) NO-ERROR.
    IF t-VmId:checked THEN ArtBas.VmId = int(ENTRY(5,cEndret,'|')) NO-ERROR.
    IF t-ProdNr:checked THEN ArtBas.ProdNr = int(ENTRY(6,cEndret,'|')) NO-ERROR.
    IF t-RAvdNr:checked THEN ArtBas.RAvdNr = int(ENTRY(7,cEndret,'|')) NO-ERROR.
    IF t-WebButikkArtikkel:checked THEN ArtBas.WebButikkArtikkel = (IF CAN-DO('yes,true',ENTRY(8,cEndret,'|')) 
                                                                     THEN TRUE ELSE FALSE) NO-ERROR.
    IF t-VPIBildeKode:checked THEN ArtBas.VPIBildeKode = ENTRY(9,cEndret,'|') NO-ERROR.
  END.
    
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

