&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
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
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

DEFINE INPUT PARAMETER ihBrowse   AS HANDLE NO-UNDO.
DEFINE INPUT PARAMETER icProfilNr AS CHAR   NO-UNDO.
DEFINE INPUT PARAMETER icButikkNr AS CHAR   NO-UNDO.

/* Local Variable Definitions ---                                       */

DEF VAR bClose  AS LOG NO-UNDO.
DEF VAR cForste AS CHAR NO-UNDO.
DEF VAR cTekst  AS CHAR NO-UNDO.
DEF VAR cArtNr  AS CHAR NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS fiKode fiAntall tbAlleStr cmbButikk BUTTON-1 
&Scoped-Define DISPLAYED-OBJECTS fiKode fiAntall tbAlleStr cmbButikk 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-1 AUTO-END-KEY 
     LABEL "Avbryt" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE cmbButikk AS CHARACTER FORMAT "X(256)":U 
     LABEL "Butikk" 
     VIEW-AS COMBO-BOX INNER-LINES 30
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 29.6 BY 1 NO-UNDO.

DEFINE VARIABLE fiAntall AS INTEGER FORMAT ">9":U INITIAL 1 
     LABEL "Antall" 
     VIEW-AS FILL-IN 
     SIZE 5.6 BY 1 NO-UNDO.

DEFINE VARIABLE fiKode AS DECIMAL FORMAT "9999999999999":U INITIAL 0 
     LABEL "Strekkode" 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1.67
     FONT 12 NO-UNDO.

DEFINE VARIABLE tbAlleStr AS LOGICAL INITIAL no 
     LABEL "Inkluder alle størrelser" 
     VIEW-AS TOGGLE-BOX
     SIZE 23.6 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     fiKode AT ROW 2.05 COL 19.4 COLON-ALIGNED
     fiAntall AT ROW 3.86 COL 19.4 COLON-ALIGNED
     tbAlleStr AT ROW 5 COL 21.4
     cmbButikk AT ROW 6.05 COL 19.4 COLON-ALIGNED
     BUTTON-1 AT ROW 7.91 COL 42.8
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 57.8 BY 8.33.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Scann strekkode for etikettkø"
         HEIGHT             = 8.33
         WIDTH              = 57.8
         MAX-HEIGHT         = 16
         MAX-WIDTH          = 80
         VIRTUAL-HEIGHT     = 16
         VIRTUAL-WIDTH      = 80
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Scann strekkode for etikettkø */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Scann strekkode for etikettkø */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiAntall
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiAntall C-Win
ON LEAVE OF fiAntall IN FRAME DEFAULT-FRAME /* Antall */
DO:
  APPLY "entry" TO fiKode.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiKode
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiKode C-Win
ON RETURN OF fiKode IN FRAME DEFAULT-FRAME /* Strekkode */
DO:
  cTekst = fiKode:SCREEN-VALUE.
  RUN sjekkKode (INPUT-OUTPUT cTekst).
  fiKode:SCREEN-VALUE = cTekst.

  cArtNr = DYNAMIC-FUNCTION("getFieldValues","Strekkode","WHERE Kode = '" + fiKode:SCREEN-VALUE + "'","ArtikkelNr").
  IF cArtNr NE ? THEN DO:
    IF NOT DYNAMIC-FUNCTION("runProc","etikettartpris_send_til.p",icProfilnr + ";" 
                               + (IF NUM-ENTRIES(cmbButikk:LIST-ITEM-PAIRS,"|") > 1 THEN cmbButikk:SCREEN-VALUE ELSE "") 
                               + ";" + fiAntall:SCREEN-VALUE + ";" + tbAlleStr:INPUT-VALUE
                             + "¤" + cArtNr + "|" + fiKode:SCREEN-VALUE,?) THEN
      DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").
    ELSE RUN InvokeMethod(ihBrowse,"OpenQuery").
  END.
  APPLY 'ENTRY' TO fiKode.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tbAlleStr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tbAlleStr C-Win
ON VALUE-CHANGED OF tbAlleStr IN FRAME DEFAULT-FRAME /* Inkluder alle størrelser */
DO:
  APPLY "entry" TO fiKode.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE DO:
  bClose = YES.
  RUN disable_UI.    
END.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  fiKode:SIDE-LABEL-HANDLE:FONT = fiKode:FONT.
  fiKode:SIDE-LABEL-HANDLE:X = fiKode:SIDE-LABEL-HANDLE:X - 35.

  ASSIGN cmbButikk:DELIMITER = "|"
         cmbButikk:LIST-ITEM-PAIRS = DYNAMIC-FUNCTION("getFieldList","Butiker;Butik|ButNamn;butik","WHERE profilnr = " + icProfilnr)
         .

  IF NUM-ENTRIES(cmbButikk:LIST-ITEM-PAIRS,"|") > 2 THEN 
    cmbButikk:LIST-ITEM-PAIRS = "Alle| |" + cmbButikk:LIST-ITEM-PAIRS.

  IF LOOKUP(icButikkNr,cmbButikk:LIST-ITEM-PAIRS,"|") > 0 THEN
    cmbButikk:SCREEN-VALUE = icButikkNr.
  ELSE IF (cmbButikk:ENTRY(1) = '' OR cmbButikk:ENTRY(1)= ?) THEN
    cmbButikk:SCREEN-VALUE = cmbButikk:ENTRY(2).
  ELSE 
    cmbButikk:SCREEN-VALUE = cmbButikk:ENTRY(1).

  IF DYNAMIC-FUNCTION("getFieldList","Bruker;BrukerType","WHERE BrukerId = '" + DYNAMIC-FUNCTION("getASuserId") + "'") NE "1" THEN
    cmbButikk:SENSITIVE = NO.

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

IF NOT bClose THEN DO:
  {incl/wintrigg.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
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
  DISPLAY fiKode fiAntall tbAlleStr cmbButikk 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE fiKode fiAntall tbAlleStr cmbButikk BUTTON-1 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sjekkKode C-Win 
PROCEDURE sjekkKode :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT-OUTPUT PARAMETER cKode AS CHAR NO-UNDO.  

  IF DEC(cKode) <> 0 THEN DO:
      /* Sjekker rått den koden som er tastet inn. */
      FIND Strekkode NO-LOCK WHERE Strekkode.Kode = cKode NO-ERROR.
      
      IF NOT AVAILABLE Strekkode THEN
      DO:
          /* UPC-E koder er på 6 siffer. Konverteres til UPC-A og derfra til EAN13 kode.*/
          IF LENGTH(cKode) = 6 THEN
              RUN bibl_chkean.p (INPUT-OUTPUT cKode).
          /* EAN 13 kode */
          ELSE DO:
              cKode = STRING(DEC(cKode),"9999999999999"). 
              RUN bibl_chkean.p (INPUT-OUTPUT cKode).
          END.

          /* Sjekker med korrigert strekkode. */
          FIND Strekkode NO-LOCK WHERE Strekkode.Kode = cKode NO-ERROR.
      END.
      /* Sjekker uten nullutfylling. */
      IF NOT AVAILABLE Strekkode THEN
      DO:
          FIND Strekkode NO-LOCK WHERE Strekkode.Kode = LEFT-TRIM(cKode,"0") NO-ERROR.
          IF AVAILABLE Strekkode THEN cKode = LEFT-TRIM(cKode,"0").
      END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

