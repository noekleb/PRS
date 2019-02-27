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
def input parameter cOrg    as char no-undo.
def input parameter cENDRET as char no-undo.

/* Local Variable Definitions ---                                       */

def var lArtikkelNr  like ArtBas.ArtikkelNr no-undo.
def var lModellFarge like ArtBas.ModellFarge no-undo.

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

DEFINE VARIABLE T-LevNr AS LOGICAL INITIAL no 
     LABEL "Leverandørnummer" 
     VIEW-AS TOGGLE-BOX
     SIZE 49 BY .81 NO-UNDO.

DEFINE VARIABLE T-ProdNr AS LOGICAL INITIAL no 
     LABEL "Produsent" 
     VIEW-AS TOGGLE-BOX
     SIZE 50 BY .81 NO-UNDO.

DEFINE VARIABLE T-RAvdNr AS LOGICAL INITIAL no 
     LABEL "Vareområde" 
     VIEW-AS TOGGLE-BOX
     SIZE 48 BY .81 NO-UNDO.

DEFINE VARIABLE T-Sasong AS LOGICAL INITIAL no 
     LABEL "Sesong" 
     VIEW-AS TOGGLE-BOX
     SIZE 51 BY .81 NO-UNDO.

DEFINE VARIABLE T-Vg AS LOGICAL INITIAL no 
     LABEL "Varegruppe" 
     VIEW-AS TOGGLE-BOX
     SIZE 50 BY .81 NO-UNDO.

DEFINE VARIABLE T-VmId AS LOGICAL INITIAL no 
     LABEL "Varemerke" 
     VIEW-AS TOGGLE-BOX
     SIZE 50 BY .81 NO-UNDO.

DEFINE VARIABLE T-VPIBildeKode AS LOGICAL INITIAL no 
     LABEL "VPIBildekode" 
     VIEW-AS TOGGLE-BOX
     SIZE 52 BY .81 NO-UNDO.

DEFINE VARIABLE T-WebButikkArtikkel AS LOGICAL INITIAL no 
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
  run KopierTilModell.
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
  
  assign
    T-LevNr:hidden             = entry(2,cOrg,'|') = entry(2,cEndret,'|')
    T-Sasong:hidden            = entry(3,cOrg,'|') = entry(3,cEndret,'|')
    T-Vg:hidden                = entry(4,cOrg,'|') = entry(4,cEndret,'|')
    T-VmId:hidden              = entry(5,cOrg,'|') = entry(5,cEndret,'|')
    T-ProdNr:hidden            = entry(6,cOrg,'|') = entry(6,cEndret,'|')
    T-RAvdNr:hidden            = entry(7,cOrg,'|') = entry(7,cEndret,'|')
    T-WebButikkArtikkel:hidden = entry(8,cOrg,'|') = entry(8,cEndret,'|')
    T-VPIBildeKode:hidden      = entry(9,cOrg,'|') = entry(9,cEndret,'|')
    .
  
  
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

do with frame Dialog-Frame transaction:
  assign
    lArtikkelNr = dec(entry(1,cOrg,'|'))
    .
  find ArtBas no-lock where
    ArtBas.ArtikkelNr = lArtikkelNr no-error.
  if not available ArtBas then return.
  assign
    lModellFarge = ArtBas.ModellFarge.
  if lArtikkelNr = 0 or lModellFarge = 0 then return.
  
  for each ArtBas exclusive-lock where
    ArtBas.ModellFarge = lModellFarge:
    
    if t-LevNr:checked then ArtBas.LevNr = int(entry(2,cEndret,'|')).
    if t-Sasong:checked then ArtBas.Sasong = int(entry(3,cEndret,'|')).
    if t-Vg:checked then ArtBas.Vg = int(entry(4,cEndret,'|')).
    if t-VmId:checked then ArtBas.VmId = int(entry(5,cEndret,'|')).
    if t-ProdNr:checked then ArtBas.ProdNr = int(entry(6,cEndret,'|')).
    if t-RAvdNr:checked then ArtBas.RAvdNr = int(entry(7,cEndret,'|')).
    if t-WebButikkArtikkel:checked then ArtBas.WebButikkArtikkel = (if can-do('yes,true',entry(8,cEndret,'|')) 
                                                                     then true else false).
    if t-VPIBildeKode:checked then ArtBas.VPIBildeKode = entry(9,cEndret,'|').
  end.
    
end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

