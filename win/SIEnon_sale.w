&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          skotex           PROGRESS
*/
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

/* Local Variable Definitions ---                                       */

DEFINE BUFFER standardTrans FOR SIEtranstyp.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BR-SIENON

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES SIENon_saleTbId ArtBas

/* Definitions for BROWSE BR-SIENON                                     */
&Scoped-define FIELDS-IN-QUERY-BR-SIENON SIENon_saleTbId.ArtikkelNr ~
ArtBas.BongTekst ArtBas.NegVare SIENon_saleTbId.TBId 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BR-SIENON 
&Scoped-define QUERY-STRING-BR-SIENON FOR EACH SIENon_saleTbId NO-LOCK, ~
      EACH ArtBas OF SIENon_saleTbId NO-LOCK ~
    BY SIENon_saleTbId.TBId INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BR-SIENON OPEN QUERY BR-SIENON FOR EACH SIENon_saleTbId NO-LOCK, ~
      EACH ArtBas OF SIENon_saleTbId NO-LOCK ~
    BY SIENon_saleTbId.TBId INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BR-SIENON SIENon_saleTbId ArtBas
&Scoped-define FIRST-TABLE-IN-QUERY-BR-SIENON SIENon_saleTbId
&Scoped-define SECOND-TABLE-IN-QUERY-BR-SIENON ArtBas


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-BR-SIENON}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BUTTON-1 RECT-1 BR-SIENON B-Angre CB-TBId ~
B-Lagre FI-saknas 
&Scoped-Define DISPLAYED-OBJECTS FI-ArtikkelNr FI-BongTekst CB-TBId ~
FI-saknas 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-Angre 
     IMAGE-UP FILE "icon/reset.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "&Ångra" 
     SIZE 4.6 BY 1.05 TOOLTIP "Angre (Alt-A)".

DEFINE BUTTON B-Lagre 
     IMAGE-UP FILE "icon/saverec.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "&Lagra" 
     SIZE 4.6 BY 1.05 TOOLTIP "Lagre post (Alt-L)".

DEFINE BUTTON BUTTON-1  NO-FOCUS
     LABEL "Hämta artiklar / refresh" 
     SIZE 25 BY 1.14.

DEFINE VARIABLE CB-TBId AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "TBId" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Item 1",0
     DROP-DOWN-LIST
     SIZE 33.2 BY 1 NO-UNDO.

DEFINE VARIABLE FI-ArtikkelNr AS DECIMAL FORMAT "zzzzzzzzzzzz9" INITIAL 0 
     LABEL "Artikkelnummer" 
     VIEW-AS FILL-IN 
     SIZE 20.2 BY 1
     BGCOLOR 15 .

DEFINE VARIABLE FI-BongTekst AS CHARACTER FORMAT "X(30)" 
     LABEL "Bongtekst" 
     VIEW-AS FILL-IN 
     SIZE 33.2 BY 1
     BGCOLOR 15 .

DEFINE VARIABLE FI-saknas AS CHARACTER FORMAT "X(256)":U 
     LABEL "" 
      VIEW-AS TEXT 
     SIZE 116.8 BY 1.52
     FGCOLOR 12 FONT 8 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 56 BY 6.62.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BR-SIENON FOR 
      SIENon_saleTbId, 
      ArtBas SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BR-SIENON
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BR-SIENON C-Win _STRUCTURED
  QUERY BR-SIENON NO-LOCK DISPLAY
      SIENon_saleTbId.ArtikkelNr FORMAT "zzzzzzzzzzzz9":U
      ArtBas.BongTekst FORMAT "X(30)":U
      ArtBas.NegVare FORMAT "yes/no":U
      SIENon_saleTbId.TBId FORMAT ">>9":U WIDTH 42.4
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 93 BY 16.43 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     BUTTON-1 AT ROW 1.48 COL 3 NO-TAB-STOP 
     BR-SIENON AT ROW 4.81 COL 3
     B-Angre AT ROW 9.71 COL 121 NO-TAB-STOP 
     FI-ArtikkelNr AT ROW 5.86 COL 113.8 COLON-ALIGNED
     FI-BongTekst AT ROW 7 COL 113.8 COLON-ALIGNED HELP
          "Bongtekst - Tekst som vises på kundens kvittering"
     CB-TBId AT ROW 8.19 COL 113.8 COLON-ALIGNED
     B-Lagre AT ROW 9.71 COL 116 NO-TAB-STOP 
     FI-saknas AT ROW 3.14 COL 1 COLON-ALIGNED
     RECT-1 AT ROW 4.81 COL 98
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 154.8 BY 20.81.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Non Sale för SIEexport"
         HEIGHT             = 20.81
         WIDTH              = 154.8
         MAX-HEIGHT         = 25.62
         MAX-WIDTH          = 240.6
         VIRTUAL-HEIGHT     = 25.62
         VIRTUAL-WIDTH      = 240.6
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
/* BROWSE-TAB BR-SIENON RECT-1 DEFAULT-FRAME */
/* SETTINGS FOR FILL-IN FI-ArtikkelNr IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-BongTekst IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BR-SIENON
/* Query rebuild information for BROWSE BR-SIENON
     _TblList          = "SkoTex.SIENon_saleTbId,SkoTex.ArtBas OF SkoTex.SIENon_saleTbId"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _OrdList          = "SkoTex.SIENon_saleTbId.TBId|yes"
     _FldNameList[1]   = SkoTex.SIENon_saleTbId.ArtikkelNr
     _FldNameList[2]   = SkoTex.ArtBas.BongTekst
     _FldNameList[3]   = SkoTex.ArtBas.NegVare
     _FldNameList[4]   > SkoTex.SIENon_saleTbId.TBId
"SIENon_saleTbId.TBId" ? ? "integer" ? ? ? ? ? ? no ? no no "42.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE BR-SIENON */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Non Sale för SIEexport */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Non Sale för SIEexport */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Angre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Angre C-Win
ON CHOOSE OF B-Angre IN FRAME DEFAULT-FRAME /* Ångra */
DO:
  APPLY "VALUE-CHANGED" TO BR-SIENON.
  APPLY "ENTRY" TO BR-SIENON.

  RETURN NO-APPLY.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Lagre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Lagre C-Win
ON CHOOSE OF B-Lagre IN FRAME DEFAULT-FRAME /* Lagra */
DO:
    ASSIGN CB-TBId.
    FIND CURRENT SIENon_saleTbId.
    SIENon_saleTbId.TBId  = INPUT CB-TBId.
    BROWSE BR-SIENON:REFRESH().
    APPLY "VALUE-CHANGED" TO BR-SIENON.
    APPLY "ENTRY" TO BR-SIENON.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BR-SIENON
&Scoped-define SELF-NAME BR-SIENON
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BR-SIENON C-Win
ON VALUE-CHANGED OF BR-SIENON IN FRAME DEFAULT-FRAME
DO:
    B-Angre:SENSITIVE = FALSE.
    B-Lagre:SENSITIVE = FALSE.
    RUN VisaDetalj.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-1 C-Win
ON CHOOSE OF BUTTON-1 IN FRAME DEFAULT-FRAME /* Hämta artiklar / refresh */
DO:
    IF NOT AVAIL standardTrans THEN DO:
        BELL.
        FI-saknas:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "Kontrollera standard SIE transtyp för nonsale".
    END.
    ELSE DO:
        RUN GetArtbas.
        {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
    END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CB-TBId
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-TBId C-Win
ON VALUE-CHANGED OF CB-TBId IN FRAME DEFAULT-FRAME /* TBId */
DO:
    B-Angre:SENSITIVE = TRUE.
    B-Lagre:SENSITIVE = TRUE.
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
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
    FIND FIRST standardTrans WHERE standardTrans.TTId = 1 AND standardTrans.TBId = 10 NO-LOCK NO-ERROR.
    IF NOT AVAIL standardTrans THEN
        FI-Saknas = "Det finns ingen registrerad SIE transtyp för nonsale".
    ELSE IF standardTrans.KontoNr = 0 THEN DO:
        FI-Saknas = "Konto saknas på standard nonsale SIE transtyp".
        RELEASE standardTrans.
    END.
    ELSE
        RUN InitCB.
    
  RUN enable_UI.
  B-Angre:SENSITIVE = FALSE.
  B-Lagre:SENSITIVE = FALSE.
  APPLY "VALUE-CHANGED" TO BR-SIENON.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
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
  DISPLAY FI-ArtikkelNr FI-BongTekst CB-TBId FI-saknas 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE BUTTON-1 RECT-1 BR-SIENON B-Angre CB-TBId B-Lagre FI-saknas 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetArtbas C-Win 
PROCEDURE GetArtbas :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FOR EACH artbas WHERE artbas.non_sale = TRUE NO-LOCK.
        IF NOT CAN-FIND(SIENon_saleTbId WHERE SIENon_saleTbId.ArtikkelNr = artbas.artikkelnr) THEN DO:
            CREATE SIENon_saleTbId.
            ASSIGN SIENon_saleTbId.ArtikkelNr = artbas.artikkelnr
                   SIENon_saleTbId.TBId       = standardTrans.tbid.
        END.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitCB C-Win 
PROCEDURE InitCB :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cListItemPairs AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE iAntNoKonto AS INTEGER     NO-UNDO.
    FOR EACH sietranstype WHERE ttid = 1 AND tbid > 9 NO-LOCK.
        IF konto > 0 THEN
            cListItemPairs = cListItemPairs + (IF cListItemPairs <> "" THEN "," ELSE "") + STRING(SIETransType.TBId) + " " + SIETransType.Beskrivelse + "," + STRING(SIETransType.TBId).
        ELSE
            iAntNoKonto = iAntNoKonto + 1.
    END.
    IF cListItemPairs = "" THEN
        cListItemPairs = "0,0".
     CB-TBId:LIST-ITEM-PAIRS IN FRAME {&FRAME-NAME} = cListItemPairs.
     IF iAntNoKonto > 0 THEN
         FI-Saknas = "Konto saknas på " + STRING(iAntNoKonto) +  " nonsale SIE transtyp".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE VisaDetalj C-Win 
PROCEDURE VisaDetalj :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DO WITH FRAME {&FRAME-NAME}:
       IF AVAIL artbas THEN DO:
           FI-Artikkelnr:SCREEN-VALUE = string(Artbas.artikkelnr).
           FI-Bongtekst:SCREEN-VALUE  = Artbas.bongtekst.
           CB-TBId:SCREEN-VALUE = STRING(SIENon_saleTbId.TBId).
       END.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

