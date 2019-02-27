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
    DEFINE VARIABLE iButik          LIKE Butiker.butik INIT 174 NO-UNDO.
    DEFINE VARIABLE rArtBasRowid    AS ROWID           NO-UNDO.
    DEFINE VARIABLE cArtikkelEti    AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cEtiketter      AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cAntallEti      AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cIndividNr      AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE iIndividBatchNr AS INTEGER    NO-UNDO.
&ELSE
    DEFINE INPUT PARAMETER  iButik       LIKE Butiker.Butik NO-UNDO.
    DEFINE INPUT PARAMETER  rArtBasRowid AS ROWID           NO-UNDO.
    DEFINE OUTPUT PARAMETER cArtikkelEti AS CHARACTER  NO-UNDO.
    DEFINE OUTPUT PARAMETER cEtiketter   AS CHARACTER  NO-UNDO.
    DEFINE OUTPUT PARAMETER cAntallEti   AS CHARACTER  NO-UNDO.
    DEFINE OUTPUT PARAMETER cIndividNr   AS CHARACTER  NO-UNDO.
    DEFINE OUTPUT PARAMETER iIndividBatchNr AS INTEGER    NO-UNDO.
&ENDIF


/* Local Variable Definitions ---                                       */

DEFINE VARIABLE cStorlekar AS CHARACTER  NO-UNDO.
DEF VAR wAktivStrl          AS CHAR NO-UNDO.
DEFINE VAR iCol             AS INTE NO-UNDO.
DEFINE VAR wGridInitialized AS LOGI NO-UNDO.
DEFINE VAR wSHC#            AS INTE INIT 18 NO-UNDO. /* Antal hela synliga Cols */
DEFINE VAR iCL              AS INTEGER    NO-UNDO.
DEFINE VARIABLE iStrTypeId  AS INTEGER    NO-UNDO.
DEFINE VARIABLE cArtNrListe AS CHARACTER  NO-UNDO.
DEFINE VARIABLE dModellNr   AS DECIMAL    NO-UNDO.
DEFINE VARIABLE hsuperSkapaIndivid AS HANDLE     NO-UNDO.
/* Temp-Table Definitions ---                                       */

DEFINE TEMP-TABLE TT_Vare
    FIELD iRadNr     AS INTEGER
    FIELD Artikkelnr LIKE ArtBas.ArtikkelNr
    FIELD Farbeskr   LIKE Farg.farbeskr
    FIELD Varekost   AS DECIMAL
    FIELD LevNr      LIKE ArtBas.LevNr
    FIELD Vg         LIKE ArtBas.Vg   
    FIELD LopNr      LIKE ArtBas.LopNr
    INDEX Radnr iRadnr.


DEFINE BUFFER clButiker FOR Butiker.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES LevSAnt

/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define QUERY-STRING-Dialog-Frame FOR EACH LevSAnt SHARE-LOCK
&Scoped-define OPEN-QUERY-Dialog-Frame OPEN QUERY Dialog-Frame FOR EACH LevSAnt SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-Dialog-Frame LevSAnt
&Scoped-define FIRST-TABLE-IN-QUERY-Dialog-Frame LevSAnt


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Btn_OK Btn_Cancel Btn_Help FI-Txt 
&Scoped-Define DISPLAYED-OBJECTS FI-Butikk FI-ArtBeskr FI-Txt 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD FiksStorl Dialog-Frame 
FUNCTION FiksStorl RETURNS CHARACTER
  ( input wStorl as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD Registrerat Dialog-Frame 
FUNCTION Registrerat RETURNS LOGICAL
  ( INPUT ipRow AS INTEGER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE vsFlexGrid AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chvsFlexGrid AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Avbryt" 
     SIZE 12 BY 1.1
     BGCOLOR 8 .

DEFINE BUTTON Btn_Help 
     LABEL "&Hjelp" 
     SIZE 12 BY 1.1
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 12 BY 1.1
     BGCOLOR 8 .

DEFINE BUTTON BUTTON-Slett 
     LABEL "&Slett" 
     SIZE 12 BY 1.1.

DEFINE VARIABLE FI-ArtBeskr AS CHARACTER FORMAT "X(256)":U 
     LABEL "Varebeskr." 
     VIEW-AS FILL-IN 
     SIZE 48.4 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Butikk AS CHARACTER FORMAT "X(256)":U 
     LABEL "Butikk" 
     VIEW-AS FILL-IN 
     SIZE 41.6 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Txt AS CHARACTER FORMAT "X(256)":U INITIAL "Alt-L - Lagre" 
      VIEW-AS TEXT 
     SIZE 37 BY .62 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Dialog-Frame FOR 
      LevSAnt SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     FI-Butikk AT ROW 1.24 COL 8.4 COLON-ALIGNED
     FI-ArtBeskr AT ROW 1.24 COL 64.6 COLON-ALIGNED
     Btn_OK AT ROW 2.43 COL 148
     BUTTON-Slett AT ROW 3.62 COL 148
     Btn_Cancel AT ROW 4.81 COL 148
     Btn_Help AT ROW 6 COL 148
     FI-Txt AT ROW 6.48 COL 2 COLON-ALIGNED NO-LABEL
     SPACE(121.99) SKIP(0.13)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Forenklet varemottak"
         DEFAULT-BUTTON Btn_OK.


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
   EXP-POSITION                                                         */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:ROW              = 5
       FRAME Dialog-Frame:COLUMN           = -4
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON BUTTON-Slett IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-ArtBeskr IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Butikk IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX Dialog-Frame
/* Query rebuild information for DIALOG-BOX Dialog-Frame
     _TblList          = "skotex.LevSAnt"
     _Options          = "SHARE-LOCK"
     _Query            is OPENED
*/  /* DIALOG-BOX Dialog-Frame */
&ANALYZE-RESUME

 


/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME vsFlexGrid ASSIGN
       FRAME           = FRAME Dialog-Frame:HANDLE
       ROW             = 2.48
       COLUMN          = 3
       HEIGHT          = 1.57
       WIDTH           = 144
       HIDDEN          = no
       SENSITIVE       = yes.
      vsFlexGrid:NAME = "vsFlexGrid":U .
/* vsFlexGrid OCXINFO:CREATE-CONTROL from: {C5DE3F86-3376-11d2-BAA4-04F205C10000} type: vsFlexGrid */
      vsFlexGrid:MOVE-AFTER(Btn_OK:HANDLE IN FRAME Dialog-Frame).

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Forenklet varemottak */
DO:
/*  RUN LeaveCell.
  IF RETURN-VALUE = "FEL":U THEN DO:
      APPLY "ENTRY":U TO vsFlexGrid.
      RETURN NO-APPLY.
  END. */
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Help
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Help Dialog-Frame
ON CHOOSE OF Btn_Help IN FRAME Dialog-Frame /* Hjelp */
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
    RUN LeaveCell.
    IF RETURN-VALUE = "FEL" THEN DO:
        APPLY "ENTRY" TO vsFlexGrid.
        RETURN NO-APPLY.
    END.
    IF NOT Registrerat(?) THEN DO:
        MESSAGE "Det finnes inget å lagre." VIEW-AS ALERT-BOX INFORMATION.
        APPLY "ENTRY" TO vsFlexGrid.
        RETURN NO-APPLY.
    END.
    RUN LagreInnlev.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Slett
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Slett Dialog-Frame
ON CHOOSE OF BUTTON-Slett IN FRAME Dialog-Frame /* Slett */
DO:
    DEF VAR wIdx AS INTE NO-UNDO.
/*     MESSAGE "Ønsker du å slette alt ned til minste tillatte verdi?" VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO */
/*                                                       UPDATE wSvar AS LOGI.                                   */
                                                      
    DO wIdx = chvsFlexGrid:FixedCols TO chvsFlexGrid:Cols - 1:
        IF INT(chvsFlexGrid:TextMatrix(1,wIdx)) - INT(chvsFlexGrid:TextMatrix(2,wIdx)) < 0 THEN DO:
/*             RUN ChangeFri IN hWindow (wIdx - chvsFlexGrid:FixedCols + 1,INT(chvsFlexGrid:TextMatrix(1,wIdx)) - INT(chvsFlexGrid:TextMatrix(2,wIdx))). */
            ASSIGN chvsFlexGrid:TextMatrix(2,1) = STRING(INT(chvsFlexGrid:TextMatrix(2,1)) + INT(chvsFlexGrid:TextMatrix(1,wIdx)) - INT(chvsFlexGrid:TextMatrix(2,wIdx))) + " "
                   chvsFlexGrid:TextMatrix(2,wIdx) = chvsFlexGrid:TextMatrix(1,wIdx).
        END.
    END.
    ASSIGN chvsFlexGrid:COL = chvsFlexGrid:FixedCols.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME vsFlexGrid
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vsFlexGrid Dialog-Frame OCX.EnterCell
PROCEDURE vsFlexGrid.vsFlexGrid.EnterCell .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  None required for OCX.
  Notes:       
------------------------------------------------------------------------------*/
    IF NOT wGridInitialized THEN RETURN.
     ASSIGN /* chvsFlexGrid:Col = IF wCol = ? THEN chvsFlexGrid:Col ELSE wCol */
            wAktivStrl = chvsFlexGrid:TextMatrix(0,chvsFlexGrid:Col).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vsFlexGrid Dialog-Frame OCX.KeyPress
PROCEDURE vsFlexGrid.vsFlexGrid.KeyPress .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  Required for OCX.
    KeyAscii
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT-OUTPUT PARAMETER p-KeyAscii AS INTEGER NO-UNDO.
DEFINE VARIABLE               wCentralValue AS INTE    NO-UNDO.
DEFINE VARIABLE               wCellValue    AS INTE    NO-UNDO.
DEFINE VARIABLE               wDiff         AS INTE    NO-UNDO.
IF p-KeyAscii = 13 THEN DO:
    IF  chvsFlexGrid:Col = chvsFlexGrid:Cols - 1 THEN
        APPLY "ENTRY" TO Btn_OK IN FRAME {&FRAME-NAME}.
    ELSE DO:
      ASSIGN chvsFlexGrid:Col = chvsFlexGrid:Col + 1.
      IF chvsFlexGrid:Col >= wSHC# AND chvsFlexGrid:LeftCol < 
                chvsFlexGrid:Col - wSHC# + 1 + chvsFlexGrid:FixedCols THEN
          chvsFlexGrid:LeftCol = chvsFlexGrid:Col - wSHC# + + 1 + chvsFlexGrid:FixedCols.
    END.
    RETURN NO-APPLY.
END.
IF p-KeyAscii = 13 AND chvsFlexGrid:Col = chvsFlexGrid:Cols - 1 THEN DO:
    APPLY "ENTRY" TO Btn_OK IN FRAME {&FRAME-NAME}.
    RETURN NO-APPLY.
END.
IF p-KeyAscii = 8 OR (p-KeyAscii >= 43 AND p-KeyAscii <= 45) OR
                     (p-KeyAscii >= 48 AND p-KeyAscii <= 57)     THEN DO:
    IF p-KeyAscii = 8 OR p-KeyAscii = 44 THEN DO:
        IF INT(chvsFlexGrid:Text) = 0 THEN
            RETURN NO-APPLY.
        ASSIGN wDiff = INT(chvsFlexGrid:Text)
               chvsFlexGrid:Text = 
             SUBSTR(chvsFlexGrid:Text,1,LENGTH(chvsFlexGrid:Text) - 2) + " ".
        ASSIGN wDiff = INT(chvsFlexGrid:Text) - wDiff.
    END.
    ELSE IF p-KeyAscii = 43 THEN DO: /* + */
        IF INT(chvsFlexGrid:Text) + 1 > 9999 THEN
            RETURN NO-APPLY.
        ASSIGN chvsFlexGrid:Text = STRING(INT(chvsFlexGrid:Text) + 1) + " "
               wDiff = 1.
    END.
    ELSE IF p-KeyAscii = 45 THEN DO: /* - */
        IF INT(chvsFlexGrid:Text) = 0 THEN
            RETURN NO-APPLY.
        ASSIGN chvsFlexGrid:Text =
            IF INT(chvsFlexGrid:Text) - 1 = 0 THEN
                "" 
            ELSE            
                STRING(INT(chvsFlexGrid:Text) - 1) + " ".
        ASSIGN wDiff = -1.
    END.
    ELSE IF p-KeyAscii > 48 OR (chvsFlexGrid:Text > "" AND p-KeyAscii = 48) THEN DO:
        IF INT(chvsFlexGrid:Text) * 10 + p-KeyAscii - 48 > 9999 THEN
            chvsFlexGrid:Text.
        ELSE DO:
            ASSIGN wDiff = (INT(chvsFlexGrid:Text) * 10 + p-KeyAscii - 48) - INT(chvsFlexGrid:Text)
                   chvsFlexGrid:Text = STRING(INT(chvsFlexGrid:Text) * 10 + p-KeyAscii - 48) + " ".
        END.
    END.
/*     RUN ChangeFri IN hWindow (chvsFlexGrid:Col - chvsFlexGrid:FixedCols + 1,wDiff). */
    ASSIGN chvsFlexGrid:TextMatrix(chvsFlexGrid:ROW,1) = STRING(INT(chvsFlexGrid:TextMatrix(chvsFlexGrid:ROW,1)) + wDiff) + " "
           BUTTON-Slett:SENSITIVE IN FRAME {&FRAME-NAME} = INT(chvsFlexGrid:TextMatrix(chvsFlexGrid:ROW,1)) > 0.
END.
ELSE
    RETURN NO-APPLY.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vsFlexGrid Dialog-Frame OCX.KeyUp
PROCEDURE vsFlexGrid.vsFlexGrid.KeyUp .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  Required for OCX.
    KeyCode
    Shift
  Notes:       
------------------------------------------------------------------------------*/

DEFINE INPUT-OUTPUT PARAMETER p-KeyCode AS INTEGER NO-UNDO.
DEFINE INPUT        PARAMETER p-Shift   AS INTEGER NO-UNDO.
    IF p-KeyCode = 76 AND p-Shift = 4 THEN
        APPLY "CHOOSE" TO Btn_OK IN FRAME {&FRAME-NAME}.
    RETURN NO-APPLY.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.

/* Setter sentrallager butikk */
{syspara.i 5 1 1 iCl INT}

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  APPLY "ENTRY" TO SELF.
&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    FIND ArtBas WHERE Artbas.artikkelnr = 9000909 NO-LOCK.
    ASSIGN rArtBasRowid = ROWID(ArtBas).
&ENDIF
  FIND ArtBas WHERE ROWID(ArtBas) = rArtBasRowid NO-LOCK NO-ERROR.
  IF AVAIL ArtBas THEN DO:
      FIND StrType OF ArtBas NO-LOCK NO-ERROR.
      IF NOT AVAIL StrType THEN DO:
          MESSAGE "Finner ikke størrelsetype"
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
          RETURN.
      END.
  END.
  ELSE DO:
      MESSAGE "Finner ikke varen"
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN.
  END.
  FIND Butiker WHERE Butiker.Butik = iButik NO-LOCK NO-ERROR.
  IF NOT AVAIL Butiker THEN DO:
      MESSAGE "Finner ikke butik: " iButik
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN.
  END.
  FIND ArtPris OF ArtBas WHERE ArtPris.ProfilNr = Butiker.ProfilNr NO-LOCK NO-ERROR.
  IF NOT AVAIL ArtPris THEN DO:
      FIND clButiker WHERE clButiker.Butik = iCL NO-LOCK NO-ERROR.
      IF AVAIL clButiker THEN
          FIND ArtPris OF ArtBas WHERE ArtPris.ProfilNr = clButiker.ProfilNr NO-LOCK NO-ERROR.
      IF NOT AVAIL ArtPris THEN
          FIND FIRST ArtPris OF ArtBas NO-LOCK NO-ERROR.
  END.
  IF NOT AVAIL ArtPris THEN DO:
      MESSAGE "Finner ikke kalkyle for varen."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN.
  END.
  ELSE IF ArtPris.VareKost[1] = 0 THEN DO:
      MESSAGE "Varekost for varen er ikke satt."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN.
  END.
  IF ArtBas.Modell <> 0 THEN DO:
      MESSAGE "Varen ingår i modell." SKIP
          "Skall flere varer i modellen inleveres?"
          VIEW-AS ALERT-BOX BUTTONS YES-NO UPDATE lHeleModell AS LOGICAL.
      IF lHeleModell THEN
          ASSIGN iStrTypeId  = ArtBas.StrTypeId
                 dModellNr   = ArtBas.Modell.

  END.
  ASSIGN FI-Butikk   = Butiker.butnamn
         FI-ArtBeskr = ArtBas.beskr.
  FIND Farg OF ArtBas NO-LOCK NO-ERROR.
  CREATE TT_Vare.
  ASSIGN TT_Vare.iRadNr = 1
         TT_Vare.Artikkelnr = ArtBas.Artikkelnr
         TT_Vare.Farbeskr   = IF AVAIL Farg THEN Farg.farbeskr ELSE ""
         TT_Vare.VareKost   = Artpris.VareKost[1]
         TT_Vare.LevNr      = ArtBas.LevNr
         TT_Vare.Vg         = ArtBas.Vg   
         TT_Vare.LopNr      = ArtBas.LopNr.
  RUN InitStorrelser.
  IF lHeleModell THEN DO:
      RUN SkapaTT_Vare (ArtBas.ArtikkelNr).
  END.
  RUN enable_UI.
  {lng.i}
  ASSIGN wGridInitialized = TRUE.
  APPLY "ENTRY" TO vsFlexGrid.
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE control_load Dialog-Frame  _CONTROL-LOAD
PROCEDURE control_load :
/*------------------------------------------------------------------------------
  Purpose:     Load the OCXs    
  Parameters:  <none>
  Notes:       Here we load, initialize and make visible the 
               OCXs in the interface.                        
------------------------------------------------------------------------------*/

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN
DEFINE VARIABLE UIB_S    AS LOGICAL    NO-UNDO.
DEFINE VARIABLE OCXFile  AS CHARACTER  NO-UNDO.

OCXFile = SEARCH( "d-gridtst.wrx":U ).
IF OCXFile = ? THEN
  OCXFile = SEARCH(SUBSTRING(THIS-PROCEDURE:FILE-NAME, 1,
                     R-INDEX(THIS-PROCEDURE:FILE-NAME, ".":U), "CHARACTER":U) + "wrx":U).

IF OCXFile <> ? THEN
DO:
  ASSIGN
    chvsFlexGrid = vsFlexGrid:COM-HANDLE
    UIB_S = chvsFlexGrid:LoadControls( OCXFile, "vsFlexGrid":U)
  .
  RUN initialize-controls IN THIS-PROCEDURE NO-ERROR.
END.
ELSE MESSAGE "d-gridtst.wrx":U SKIP(1)
             "The binary control file could not be found. The controls cannot be loaded."
             VIEW-AS ALERT-BOX TITLE "Controls Not Loaded".

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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
  RUN control_load.

  {&OPEN-QUERY-Dialog-Frame}
  GET FIRST Dialog-Frame.
  DISPLAY FI-Butikk FI-ArtBeskr FI-Txt 
      WITH FRAME Dialog-Frame.
  ENABLE Btn_OK Btn_Cancel Btn_Help FI-Txt 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initialize-controls Dialog-Frame 
PROCEDURE initialize-controls :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VAR iIdx        AS INTE NO-UNDO.
DEFINE VAR iRow        AS INTE NO-UNDO.
ASSIGN chvsFlexGrid = chvsFlexGrid:vsFlexGrid.

ASSIGN chvsFlexGrid:CellPictureAlignment = 1
       chvsFlexGrid:Redraw = FALSE. /* disable repaint while populating */

chvsFlexGrid:Clear().
FIND LAST TT_Vare.
IF TT_Vare.iRadNr > 4 THEN
    ASSIGN FRAME Dialog-Frame:HEIGHT = FRAME Dialog-Frame:HEIGHT + ((2 + TT_Vare.iRadNr - 4) * .78)
           FI-Txt:ROW = FI-Txt:ROW + ((2 + TT_Vare.iRadNr - 4) * .78).
ASSIGN vsFlexGrid:HEIGHT = (2 + TT_Vare.iRadNr) * .78 + .15.

ASSIGN chvsFlexGrid:AllowUserResizing = 0 /* user may resize columns/rows */
       chvsFlexGrid:Enabled = TRUE  /* make it an updateable grid */
       chvsFlexGrid:AllowBigSelection = FALSE 
       chvsFlexGrid:Appearance = 1 
       chvsFlexGrid:Rows = TT_Vare.iRadNr + 1
       chvsFlexGrid:Cols = NUM-ENTRIES(cStorlekar," ") + 2
       chvsFlexGrid:FixedRows = 1
       chvsFlexGrid:FixedCols = 2
       chvsFlexGrid:HonorProKeys = FALSE
       chvsFlexGrid:TextStyle = 0
       chvsFlexGrid:TextStyleFixed = 0
       chvsFlexGrid:ColWidth(1) = 615.

ASSIGN chvsFlexGrid:Row = 0.
DO iIdx = 0 TO chvsFlexGrid:Cols - 1:
    ASSIGN chvsFlexGrid:Col = iIdx.
           chvsFlexGrid:CellBackColor = 16777215.
END.

ASSIGN chvsFlexGrid:TextMatrix(0,0) = "Farge "
       chvsFlexGrid:TextMatrix(0,1) = "Total ".

ASSIGN iIdx = 0.
DO iIdx = 1 TO NUM-ENTRIES(cStorlekar," "):
    ASSIGN chvsFlexGrid:ColWidth(iIdx + 1) = 510
           chvsFlexGrid:TextMatrix(0,iIdx + 1) = ENTRY(iIdx,cStorlekar," ") + " ".
END.
FOR EACH TT_Vare:
    ASSIGN iRow = TT_Vare.iRadNr 
           chvsFlexGrid:TextMatrix(iRow,0) = IF TT_Vare.Farbeskr <> "" THEN TT_Vare.Farbeskr ELSE " "
           chvsFlexGrid:TextMatrix(iRow,1) = "0 ".
END.
ASSIGN chvsFlexGrid:Row = 1
       chvsFlexGrid:Col = 2
       chvsFlexGrid:Redraw = TRUE. /* disable repaint while populating */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitStorrelser Dialog-Frame 
PROCEDURE InitStorrelser :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FOR EACH StrTstr OF StrType NO-LOCK:
        ASSIGN cStorlekar = cStorlekar + 
                  IF cStorlekar = "" THEN
                      left-trim(StrTStr.SoStorl) ELSE " " + left-trim(StrTStr.SoStorl).

    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LagreInnlev Dialog-Frame 
PROCEDURE LagreInnlev :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VAR cStorlek      AS CHAR NO-UNDO. /* strl i find, går ej med chvsFlexGrid */
  DEFINE VAR iRow          AS INTE NO-UNDO.
  DEFINE VAR iLevert       AS INTE NO-UNDO.
  DEFINE VAR iBatchNr      AS INTE NO-UNDO.
  DEFINE VAR iTransNr      AS INTE NO-UNDO.
  DEFINE VAR cEtiketterTmp AS CHARACTER  NO-UNDO.
  DEFINE VAR cAntallEtiTmp AS CHARACTER  NO-UNDO.
  DEFINE VAR iCount        AS INTEGER    NO-UNDO.
  DEFINE VARIABLE dIndividNr LIKE Individ.IndividNr  NO-UNDO.
  DEFINE VARIABLE cIndividNrTmp AS CHARACTER  NO-UNDO.
  /* Skal følgeseddel ved innleveranse skrives ut. */

/*   IF cPassordkrav = "1" THEN DO:                  */
/*       RUN d-bekreftbruker.w ("Bekreft brukerid"). */
/*       IF RETURN-VALUE = "AVBRYT" THEN DO:         */
/*           MESSAGE "Lagring avbrutt"               */
/*               VIEW-AS ALERT-BOX INFO BUTTONS OK.  */
/*           RETURN NO-APPLY.                        */
/*       END.                                        */
/*       ELSE                                        */
/*           ASSIGN cUserid = RETURN-VALUE.          */
/*   END.                                            */
TRANSBLOKK:
do TRANSACTION:                    
  {sww.i}
  IF dModellNr = 0 THEN
      run batchlogg.w (program-name(1), 
                       "Forenklet varemottak: " + string(ArtBas.Artikkelnr),
                        output iBatchNr).
  ELSE
      run batchlogg.w (program-name(1), 
                       "Forenklet varemottak modell: " + STRING(dModellNr),
                        output iBatchNr).
/*       run batchlogg.w (program-name(1),                                                                       */
/*                        "Forenklet varemottak modell: " + STRING(dModellNr) + "/" + STRING(ArtBas.Artikkelnr), */
/*                         output iBatchNr).                                                                     */
  FOR EACH TT_Vare:
      IF NOT Registrerat(TT_Vare.iRadNr) THEN
          NEXT.
      ASSIGN cEtiketterTmp = ""
             cAntallEtiTmp = ""
             cIndividNrTmp = "".
      FIND ArtBas WHERE ArtBas.ArtikkelNr = TT_Vare.Artikkelnr NO-LOCK.
      IF ArtBas.Inn_Dato = ? THEN
      DO:
          FIND CURRENT ArtBas EXCLUSIVE-LOCK.
          ASSIGN
              ArtBas.Inn_Dato = TODAY.
          FIND CURRENT ArtBas NO-LOCK.
      END.
      IF ArtBas.IndividType > 0 THEN DO:
          FIND HuvGr OF ArtBas NO-LOCK.
          FIND LevBas OF ArtBas NO-LOCK.
          FIND Varemerke OF ArtBas NO-LOCK NO-ERROR.
          ASSIGN iIndividBatchNr = iBatchNr.
      END.
      Do with frame DEFAULT-FRAME:
               .
        DO iRow = TT_Vare.iRadNr TO TT_Vare.iRadNr:
            /* Transaksjonsnummer for butikken. */

            DO iCol = 2 TO chvsFlexGrid:Cols - 1:
                IF INT(chvsFlexGrid:TextMatrix(iRow,iCol)) = 0 THEN
                    NEXT.
                find last TransLogg no-lock where
                  TransLogg.Butik = Butiker.Butik use-index TransLogg no-error.
                if available TransLogg then
                  iTransNr = TransLogg.TransNr + 1.
                else 
                  iTransNr = 1.

                ASSIGN cStorlek = FiksStorl(chvsFlexGrid:TextMatrix(0,iCol))
                       iLevert = INT(chvsFlexGrid:TextMatrix(iRow,iCol)).
                IF ArtBas.IndividType > 0 THEN DO:
                    FIND StrKonv WHERE StrKonv.Storl = cStorlek NO-LOCK NO-ERROR.
                END.
                /* Oppretter transaksjon */
                LAG_TRANS:
                DO iCount = 1 TO (IF ArtBas.IndividType > 0 THEN iLevert ELSE 1):
                  /* Sjekker at transnr er ledig */
                  if can-find(TransLogg where
                              TransLogg.Butik   = Butiker.Butik and
                              TransLogg.TransNr = iTransNr) then
                    NESTE_NR:
                    do while true:
                      iTransNr = iTransNr + 1.
                      if can-find(TransLogg where
                                  TransLogg.Butik   = Butiker.Butik and
                                  TransLogg.TransNr = iTransNr) then
                        next NESTE_NR.
                      else
                        leave NESTE_NR.
                    end. /* NESTE_NR */
                  IF ArtBas.IndividType > 0 AND AVAIL StrKonv THEN
                      RUN SkapaIndivid IN THIS-PROCEDURE (INPUT iBatchNr,INPUT StrKonv.Strkode,INPUT StrKonv.Storl, OUTPUT dIndividNr).
                  create TransLogg.
                  assign TransLogg.Butik        = Butiker.Butik
                         TransLogg.TransNr      = iTransNr
                         TransLogg.SeqNr        = 1.
                  assign TransLogg.BatchNr      = iBatchNr
                         TransLogg.KundNr       = 0
                         TransLogg.TTId         = 5 /* Varekjøp */
                         TransLogg.TBId         = 1
                         TransLogg.ArtikkelNr   = TT_Vare.ArtikkelNr
                         TransLogg.LevNr        = TT_Vare.LevNr
                         TransLogg.BongId       = 0
                         TransLogg.BongLinjeNr  = 0
                         TransLogg.KassaNr      = 0
                         TransLogg.Vg           = TT_Vare.Vg
                         TransLogg.LopNr        = TT_Vare.LopNr
                         TransLogg.Antall       = IF ArtBas.IndividType > 0 THEN 1 ELSE iLevert
                         TransLogg.Pris         = TT_Vare.VareKost
                         TransLogg.RabKr        = 0
                         TransLogg.Mva          = 0
                         TransLogg.Plukket      = true /* Skal ikke ut på plukkliste */
                         TransLogg.Dato         = today
                         TransLogg.Tid          = time
                         TransLogg.SattVVareKost = TRUE
                         TransLogg.BestNr       = 99 /* förslag när vi gör Forenklet varemottak */
                         TransLogg.Postert      = false
                         TransLogg.IndividNr    = dIndividNr.                                             
                  assign
                         TransLogg.Storl        = cStorlek
                         cEtiketterTmp          = cEtiketterTmp + (IF cEtiketterTmp <> "" THEN "," ELSE "") + TransLogg.Storl
                         cAntallEtiTmp          = cAntallEtiTmp + (IF cAntallEtiTmp <> "" THEN "," ELSE "") + STRING(TransLogg.Antall).
                         cIndividNrTmp          = cIndividNrTmp + (IF cIndividNrTmp <> "" THEN "," ELSE "") + STRING(TransLogg.IndividNr).
                end. /* LAG_TRANS */
            END.                       
        END.
      end.
      RUN genStrekKode.p(TT_Vare.ArtikkelNr,iBatchNr,"TRANSLOGG").
      IF cEtiketterTmp <> "" THEN
          ASSIGN cArtikkelEti = cArtikkelEti + (IF cArtikkelEti <> "" THEN CHR(1) ELSE "") + STRING(TT_Vare.ArtikkelNr)
                 cEtiketter   = cEtiketter   + (IF cEtiketter   <> "" THEN CHR(1) ELSE "") + cEtiketterTmp
                 cAntallEti   = cAntallEti   + (IF cAntallEti   <> "" THEN CHR(1) ELSE "") + cAntallEtiTmp
                 cIndividNr   = cIndividNr   + (IF cIndividNr   <> "" THEN CHR(1) ELSE "") + cIndividNrTmp.
      if available ArtBas then
        release ArtBas.
      if available TransLogg then
        release TransLogg.
  END.
  run batchstatus.p (iBatchNr, 2).
  {swn.i}
/*   RETURN "UTSKRIFT". */
end. /* TRANSBLOKK TRANSACTION */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LeaveCell Dialog-Frame 
PROCEDURE LeaveCell :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*  DEF VAR wSoStorl AS CHAR NO-UNDO.
  IF INT(chvsFlexGrid:TextMatrix(2,chvsFlexGrid:Col)) < INT(chvsFlexGrid:TextMatrix(1,chvsFlexGrid:Col)) THEN DO:
     MESSAGE "Otillåtet värde." VIEW-AS ALERT-BOX ERROR TITLE "Fel".
     wCol = chvsFlexGrid:Col.
     RETURN "FEL".
  END.
  ELSE DO:
     ASSIGN wCol = ?
            wSoStorl = chvsFlexGrid:TextMatrix(0,chvsFlexGrid:Col).
     FIND FIRST Fri-Str WHERE Fri-Str.SoStorl = wSoStorl.
     IF INT(chvsFlexGrid:TextMatrix(2,chvsFlexGrid:Col)) <> Fri-Str.SoAnt THEN DO:
         ASSIGN  chvsFlexGrid:TextMatrix(2,1) = STRING(INT(chvsFlexGrid:TextMatrix(2,1)) +
                    INT(chvsFlexGrid:TextMatrix(2,chvsFlexGrid:Col)) - Fri-Str.SoAnt) + " "
                 Fri-Str.SoAnt = INT(chvsFlexGrid:TextMatrix(2,chvsFlexGrid:Col)).
         RUN ChangeFri IN hWindow (Fri-Str.SeqNr).
     END.
  END. */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkapaIndivid Dialog-Frame 
PROCEDURE SkapaIndivid :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER iBatchNr    AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER iStrKode   LIKE StrKonv.StrKode   NO-UNDO.
  DEFINE INPUT  PARAMETER cStorl     LIKE StrKonv.Storl     NO-UNDO.
  DEFINE OUTPUT PARAMETER dIndividNr LIKE Individ.individnr NO-UNDO.
  DEFINE        VARIABLE  dSeqNr      AS DECIMAL            NO-UNDO.
  FIND LAST Individ WHERE Individ.butnr = Butiker.Butik USE-INDEX SeqNr NO-LOCK NO-ERROR.
  ASSIGN dSeqnr = IF NOT AVAIL Individ THEN 1 ELSE Individ.SeqNr + 1.
  CREATE Individ.
  REPEAT:
      ASSIGN dIndividNr         = DECI(STRING(Butiker.butik) + STRING(dSeqnr))
             Individ.butnr      = Butiker.Butik
             Individ.SeqNr      = dSeqNr
             Individ.ArtikkelNr = ArtBas.ArtikkelNr
             Individ.StrKode    = iStrKode
             Individ.individnr  = dIndividNr NO-ERROR.
      IF NOT ERROR-STATUS:ERROR THEN
          LEAVE.
      ASSIGN dSeqNr = dSeqNr + 1.
  END.
  ASSIGN Individ.AvdelingNr    = HuvGr.AvdelingNr
         Individ.Beskr         = ArtBas.Beskr
         Individ.Hg            = ArtBas.Hg
         Individ.IndividType   = ArtBas.IndividType
         Individ.LevNamn       = LevBas.Levnamn
         Individ.levnr         = ArtBas.LevNr
         Individ.NyVare        = TRUE
         Individ.Storl         = cStorl
         Individ.StrKode       = iStrKode
         Individ.Vg            = ArtBas.Vg
         Individ.VmBeskrivelse = IF AVAIL Varemerke THEN Varemerke.Beskrivelse ELSE ""
         Individ.VMId          = ArtBas.VMId
         Individ.BatchNr       = iBatchNr.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkapaTT_Vare Dialog-Frame 
PROCEDURE SkapaTT_Vare :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT  PARAMETER dSkapadArt AS DECIMAL    NO-UNDO.
   DEFINE        VARIABLE  iRadNr     AS INTEGER INIT 1 NO-UNDO.
   FOR EACH ArtBas WHERE ArtBas.Modell = dModellNr AND
                         ArtBas.ArtikkelNr <> dSkapadArt AND
                         ArtBas.StrTypeId = iStrTypeiD NO-LOCK.
       FIND ArtPris OF ArtBas WHERE ArtPris.ProfilNr = Butiker.ProfilNr NO-LOCK NO-ERROR.
       IF NOT AVAIL ArtPris THEN DO:
           FIND clButiker WHERE clButiker.Butik = iCL NO-LOCK NO-ERROR.
           IF AVAIL clButiker THEN
               FIND ArtPris OF ArtBas WHERE ArtPris.ProfilNr = clButiker.ProfilNr NO-LOCK NO-ERROR.
           IF NOT AVAIL ArtPris THEN
               FIND FIRST ArtPris OF ArtBas NO-LOCK NO-ERROR.
       END.
       IF AVAIL ArtPris AND ArtPris.VareKost[1] > 0 THEN DO:
           ASSIGN iRadNr = iRadNr + 1.
           FIND Farg OF ArtBas NO-LOCK NO-ERROR.
           CREATE TT_Vare.
           ASSIGN TT_Vare.iRadNr     = iRadNr
                  TT_Vare.Artikkelnr = ArtBas.Artikkelnr
                  TT_Vare.Farbeskr   = IF AVAIL Farg THEN Farg.farbeskr ELSE ""
                  TT_Vare.VareKost   = Artpris.VareKost[1]
                  TT_Vare.LevNr      = ArtBas.LevNr
                  TT_Vare.Vg         = ArtBas.Vg   
                  TT_Vare.LopNr      = ArtBas.LopNr.
       END.
   END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION FiksStorl Dialog-Frame 
FUNCTION FiksStorl RETURNS CHARACTER
  ( input wStorl as char ) :
/*------------------------------------------------------------------------------
  Purpose:  Formaterer størrelsen korrekt etter SkoTex standard.
    Notes:  
------------------------------------------------------------------------------*/
 assign
    wStorl = trim(wStorl)
    wStorl = caps(wStorl)
    wStorl = if (length(wStorl) = 1 or 
                 length(wStorl) = 3
                 ) 
                then " " + wStorl
                else wStorl.          

  /* Bytter ut eventuelle comma med punkt. */
  if index(wStorl,",") <> 0 then
    OVERLAY(wStorl, index(wStorl,","), 1, "CHARACTER") = ".".

  RETURN wStorl.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION Registrerat Dialog-Frame 
FUNCTION Registrerat RETURNS LOGICAL
  ( INPUT ipRow AS INTEGER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR wRow AS INT NO-UNDO.
  DEF VAR wCol AS INT NO-UNDO.
  IF ipRow = ? THEN DO wRow = chvsFlexGrid:FixedRows TO chvsFlexGrid:Rows - 1:
      DO wCol = chvsFlexGrid:FixedCols TO chvsFlexGrid:Cols - 1:
          IF INT(chvsFlexGrid:TextMatrix(wRow,wCol)) > 0 THEN
             RETURN TRUE.
      END.
  END.
  ELSE DO wCol = chvsFlexGrid:FixedCols TO chvsFlexGrid:Cols - 1:
      IF INT(chvsFlexGrid:TextMatrix(ipRow,wCol)) > 0 THEN
         RETURN TRUE.
  END.

  
  RETURN FALSE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

