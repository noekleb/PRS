&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          skotex           PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE TT_Kjede NO-UNDO LIKE Kjede
       FIELD Feil AS LOGICAL.
DEFINE TEMP-TABLE TT_KjedeDistrikt NO-UNDO LIKE KjedeDistrikt
       FIELD Feil AS LOGICAL.
DEFINE TEMP-TABLE TT_KjedensButikker NO-UNDO LIKE KjedensButikker
       FIELD Feil AS LOGICAL
       FIELD FeilType AS INTEGER.
DEFINE TEMP-TABLE TT_KjedeRegion NO-UNDO LIKE KjedeRegion
       FIELD Feil AS LOGICAL.



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

DEFINE VARIABLE hNode      AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE dFG31Dec   AS DATE       NO-UNDO.
DEFINE VARIABLE cColorList AS CHARACTER  INIT "7995257,255,48895,16766935,16766935" NO-UNDO.
DEFINE VARIABLE cLand      AS CHARACTER  NO-UNDO.
DEFINE VARIABLE iOnsketDag AS INTEGER    NO-UNDO. /* anvænds vid anrop om speciell dag */
DEFINE VARIABLE hSubWin    AS HANDLE     NO-UNDO.
DEFINE VARIABLE hParentSamma AS HANDLE     NO-UNDO.
/*
DEFINE VARIABLE iIkkeAktivCol AS INTEGER INIT  16766935 NO-UNDO. /* ljusblå  */
DEFINE VARIABLE iClosedCol AS INTEGER INIT  255      NO-UNDO. /* rød  */
DEFINE VARIABLE iHelligCol AS INTEGER INIT  11184895 NO-UNDO. /* rosa */
DEFINE VARIABLE iDataOKCol AS INTEGER INIT  65280    NO-UNDO. /* grøn */
DEFINE VARIABLE iDataNOKCol AS INTEGER INIT 48895    NO-UNDO. /* orange? */
 */

{windows.i}
{runlib.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES KjedeDistrikt Kjede

/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define QUERY-STRING-DEFAULT-FRAME FOR EACH KjedeDistrikt SHARE-LOCK, ~
      EACH Kjede OF KjedeDistrikt SHARE-LOCK
&Scoped-define OPEN-QUERY-DEFAULT-FRAME OPEN QUERY DEFAULT-FRAME FOR EACH KjedeDistrikt SHARE-LOCK, ~
      EACH Kjede OF KjedeDistrikt SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-DEFAULT-FRAME KjedeDistrikt Kjede
&Scoped-define FIRST-TABLE-IN-QUERY-DEFAULT-FRAME KjedeDistrikt
&Scoped-define SECOND-TABLE-IN-QUERY-DEFAULT-FRAME Kjede


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS CB_KjedeNavn FI-Dato CB-Aar BUTTON-Print ~
BUTTON-Igaar BUTTON-SokDato 
&Scoped-Define DISPLAYED-OBJECTS CB_KjedeNavn FI-Dato CB-Aar 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ButikkFeil C-Win 
FUNCTION ButikkFeil RETURNS LOGICAL
  ( OUTPUT iFeilType AS INTEGER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE MENU POPUP-MENU-FRAME-A 
       MENU-ITEM m_Butikkregister LABEL "Butikkregister"
       MENU-ITEM m_pningsskjema LABEL "Åpningsskjema" .


/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE CFTreeView AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCFTreeView AS COMPONENT-HANDLE NO-UNDO.
DEFINE VARIABLE Farger AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chFarger AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-Igaar 
     IMAGE-UP FILE "icon/reset.bmp":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON BUTTON-Print 
     IMAGE-UP FILE "icon/e-print.bmp":U NO-FOCUS
     LABEL "P" 
     SIZE 4.4 BY 1.

DEFINE BUTTON BUTTON-SokDato 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE VARIABLE CB-Aar AS INTEGER FORMAT ">>>9":U INITIAL 0 
     LABEL "År" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "0" 
     DROP-DOWN-LIST
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE CB_KjedeNavn AS INTEGER FORMAT ">9" INITIAL 0 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Item 1",0
     DROP-DOWN-LIST
     SIZE 29 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Dato AS DATE FORMAT "99/99/99":U 
     VIEW-AS FILL-IN 
     SIZE 13.2 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY DEFAULT-FRAME FOR 
      KjedeDistrikt, 
      Kjede SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     CB_KjedeNavn AT ROW 1.19 COL 2.4 HELP
          "Kjedens navn" NO-LABEL
     FI-Dato AT ROW 1.19 COL 34.6 COLON-ALIGNED NO-LABEL
     CB-Aar AT ROW 1.19 COL 56.8 COLON-ALIGNED
     BUTTON-Print AT ROW 1.19 COL 31.8
     BUTTON-Igaar AT ROW 1.19 COL 50.2
     BUTTON-SokDato AT ROW 1.19 COL 71
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 74.6 BY 26.57.

DEFINE FRAME FRAME-A
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 11 ROW 20
         SIZE 1 BY 2.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
   Temp-Tables and Buffers:
      TABLE: TT_Kjede T "?" NO-UNDO skotex Kjede
      ADDITIONAL-FIELDS:
          FIELD Feil AS LOGICAL
      END-FIELDS.
      TABLE: TT_KjedeDistrikt T "?" NO-UNDO skotex KjedeDistrikt
      ADDITIONAL-FIELDS:
          FIELD Feil AS LOGICAL
      END-FIELDS.
      TABLE: TT_KjedensButikker T "?" NO-UNDO skotex KjedensButikker
      ADDITIONAL-FIELDS:
          FIELD Feil AS LOGICAL
          FIELD FeilType AS INTEGER
      END-FIELDS.
      TABLE: TT_KjedeRegion T "?" NO-UNDO skotex KjedeRegion
      ADDITIONAL-FIELDS:
          FIELD Feil AS LOGICAL
      END-FIELDS.
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Mottakskontroll salgsdata"
         HEIGHT             = 26.57
         WIDTH              = 74.6
         MAX-HEIGHT         = 26.57
         MAX-WIDTH          = 160.8
         VIRTUAL-HEIGHT     = 26.57
         VIRTUAL-WIDTH      = 160.8
         MAX-BUTTON         = no
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = yes
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
/* REPARENT FRAME */
ASSIGN FRAME FRAME-A:FRAME = FRAME DEFAULT-FRAME:HANDLE.

/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* SETTINGS FOR COMBO-BOX CB_KjedeNavn IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
/* SETTINGS FOR FRAME FRAME-A
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME FRAME-A:POPUP-MENU       = MENU POPUP-MENU-FRAME-A:HANDLE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME DEFAULT-FRAME
/* Query rebuild information for FRAME DEFAULT-FRAME
     _TblList          = "SkoTex.KjedeDistrikt,SkoTex.Kjede OF SkoTex.KjedeDistrikt"
     _Query            is OPENED
*/  /* FRAME DEFAULT-FRAME */
&ANALYZE-RESUME

 


/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME CFTreeView ASSIGN
       FRAME           = FRAME DEFAULT-FRAME:HANDLE
       ROW             = 2.38
       COLUMN          = 2.4
       HEIGHT          = 23.81
       WIDTH           = 72.6
       HIDDEN          = no
       SENSITIVE       = yes.

CREATE CONTROL-FRAME Farger ASSIGN
       FRAME           = FRAME DEFAULT-FRAME:HANDLE
       ROW             = 26.52
       COLUMN          = 2.4
       HEIGHT          = .91
       WIDTH           = 72.6
       HIDDEN          = no
       SENSITIVE       = yes.
/* CFTreeView OCXINFO:CREATE-CONTROL from: {C74190B6-8589-11D1-B16A-00C0F0283628} type: TreeView */
/* Farger OCXINFO:CREATE-CONTROL from: {0F026C11-5A66-4c2b-87B5-88DDEBAE72A1} type: VSFlexGrid */
      CFTreeView:MOVE-AFTER(CB-Aar:HANDLE IN FRAME DEFAULT-FRAME).
      Farger:MOVE-AFTER(FRAME FRAME-A:HANDLE).

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Mottakskontroll salgsdata */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Mottakskontroll salgsdata */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FRAME-A
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FRAME-A C-Win
ON GO OF FRAME FRAME-A
DO:
    run Apply-mouse-menu-click(self:handle).
    return no-apply.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Igaar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Igaar C-Win
ON CHOOSE OF BUTTON-Igaar IN FRAME DEFAULT-FRAME /* ... */
DO:
    ASSIGN FI-Dato:SCREEN-VALUE = STRING(TODAY - 1)
           FI-Dato = TODAY - 1.
    APPLY "VALUE-CHANGED" TO CB-Aar.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Print
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Print C-Win
ON CHOOSE OF BUTTON-Print IN FRAME DEFAULT-FRAME /* P */
DO:
    DEFINE VARIABLE d31DecFGAar AS DATE       NO-UNDO.
    DEFINE VARIABLE cFeilDato   AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE iCount      AS INTEGER    NO-UNDO.
    DEFINE VARIABLE cFilename   AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cManTxt     AS CHARACTER  NO-UNDO.
    DEF VAR cTekst              AS CHAR       NO-UNDO.

    ASSIGN d31DecFGAar = DATE(12,31,YEAR(INPUT FI-Dato) - 1)
           cFilename   = SESSION:TEMP-DIRECTORY + "schemakontroll" + 
                         STRING(TODAY,"99-99-99") + "-" + STRING(TIME) + ".txt"
           cManTxt     = "jan,feb,mar,apr,maj,jun,jul,aug,sep,okt,nov,dec".
    OUTPUT TO VALUE(cFilename).
    PUT UNFORMATTED "Schemainfo perioden " STRING(d31DecFGAar + 1) "-" STRING(INPUT FI-Dato) SKIP
                    "Butik;Data saknas" SKIP.
    FOR EACH KjedensButikker:
        FIND FIRST Kasse NO-LOCK WHERE
            Kasse.ButikkNr = Kjedensbutikker.ButikkNr AND
            Kasse.GruppeNr = 1 NO-ERROR.
        IF AVAILABLE Kasse THEN
        DO:
            {syspara.i 1 10 Kasse.ModellNr cTekst}
        END.
        ELSE
            cTekst = "".
        IF cTekst = "" AND AVAILABLE Kasse THEN
            cTekst = STRING(Kasse.ModellNr).
            
        ASSIGN cFeilDato = "".
        FIND ApnSkjema WHERE ApnSkjema.ButikkNr = KjedensButikker.ButikkNr AND 
                             ApnSkjema.Ar       = YEAR(INPUT FI-Dato) NO-LOCK NO-ERROR.
        IF NOT AVAIL ApnSkjema THEN
            PUT UNFORMATTED KjedensButikker.ButikkNr ";" 
                            cTekst ";"
                            "schema saknas" SKIP.
        ELSE 
        DO: 
          DO iCount = 1 TO INPUT FI-Dato - d31DecFGAar:
            IF CAN-DO("1,2",ENTRY(iCount,ApnSkjema.OpenClosed)) THEN
                ASSIGN cFeilDato = cFeilDato + (IF cFeilDato <> "" THEN ";" ELSE "") +
                       STRING(DAY(d31DecFGAar + iCount),"99") + ENTRY(MONTH(d31DecFGAar + iCount),cManTxt).
          END.
          PUT UNFORMATTED 
              STRING(KjedensButikker.ButikkNr) ";"
              cTekst ";"
              (IF cFeilDato = "" THEN "butik OK" ELSE cFeilDato) SKIP.
        END.
    END.
    MESSAGE "Resultatfil:" cFilename SKIP "(öppna i Excel)"
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokDato C-Win
ON CHOOSE OF BUTTON-SokDato IN FRAME DEFAULT-FRAME /* ... */
DO:
    DEFINE VARIABLE cTittel AS CHARACTER INIT "Datosøk" NO-UNDO.
    DEFINE VARIABLE dDato   AS DATE       NO-UNDO.
    ASSIGN dDato = IF YEAR(TODAY) = CB-Aar THEN TODAY - 1 ELSE DATE(12,31,CB-Aar).
    RUN kalender.w (INPUT-OUTPUT dDato,cTittel).
    IF RETURN-VALUE <> "<avbryt>" THEN DO:
        IF dDato >= TODAY OR YEAR(dDato) < YEAR(TODAY) - 1 THEN DO:
            MESSAGE "Ugyldig datoangivelse"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
        END.
        ELSE DO:
            RUN w-Mottakkontroll.w PERSISTENT SET hSubWin.
            RUN VisDag IN hSubWin (dDato).
        END.
    END.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CB-Aar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-Aar C-Win
ON VALUE-CHANGED OF CB-Aar IN FRAME DEFAULT-FRAME /* År */
DO:
  DEFINE VARIABLE iBeforeAar AS INTEGER    NO-UNDO.
  ASSIGN iBeforeAar = CB-Aar.
  ASSIGN INPUT CB-Aar.
  EMPTY TEMP-TABLE TT_Kjede.
  EMPTY TEMP-TABLE TT_KjedeRegion.
  EMPTY TEMP-TABLE TT_KjedeDistrikt.
  EMPTY TEMP-TABLE TT_KjedensButikker.
  RUN Create_TT.
  ASSIGN FI-Dato:SENSITIVE = CB-Aar = YEAR(TODAY)
         BUTTON-Igaar:SENSITIVE = FI-Dato:SENSITIVE
         FI-Dato = IF FI-Dato:SENSITIVE AND iBeforeAar = CB-Aar THEN FI-Dato ELSE 
                       IF FI-Dato:SENSITIVE THEN TODAY - 1 ELSE DATE(12,31,CB-Aar)
         FI-Dato:SCREEN-VALUE = STRING(FI-Dato).
  APPLY "VALUE-CHANGED" TO CB_KjedeNavn.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CB_KjedeNavn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB_KjedeNavn C-Win
ON VALUE-CHANGED OF CB_KjedeNavn IN FRAME DEFAULT-FRAME
DO:
  ASSIGN hNode = chCFTreeView:TreeView:Nodes:CLEAR().
  ASSIGN CB_KjedeNavn.
  RUN FillTreeView.
  APPLY "ENTRY" TO CFTreeView.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CFTreeView
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CFTreeView C-Win OCX.Click
PROCEDURE CFTreeView.TreeView.Click .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  None required for OCX.
  Notes:       
------------------------------------------------------------------------------*/

  ASSIGN hNode:CHECKED = ENTRY(3,hNode:Tag,CHR(1)) = "J".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CFTreeView C-Win OCX.MouseUp
PROCEDURE CFTreeView.TreeView.MouseUp .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  Required for OCX.
    Button
    Shift
    x
    y
  Notes:       
------------------------------------------------------------------------------*/

DEFINE INPUT PARAMETER p-Button AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER p-Shift  AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER p-x      AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER p-y      AS INTEGER NO-UNDO.
DEFINE VARIABLE        rRowId AS ROWID      NO-UNDO.
  IF VALID-HANDLE(hParentSamma) OR p-Button = 1 THEN
      RETURN.
  IF NUM-ENTRIES(hNode:Tag,CHR(1)) = 3 AND ENTRY(1,hNode:Tag,CHR(1)) = "B" THEN DO:
      IF ENTRY(1,hNode:Tag,CHR(1)) = "B" THEN DO:
          ASSIGN rRowId = TO-ROWID(ENTRY(2,hNode:Tag,CHR(1))).
          FIND TT_KjedensButikker WHERE ROWID(TT_KjedensButikker) = rRowId NO-ERROR.
          IF AVAIL TT_KjedensButikker THEN DO:
              FIND Butiker WHERE Butiker.Butik = TT_KjedensButikker.ButikkNr NO-LOCK NO-ERROR.
              IF AVAIL Butiker THEN DO:
                  FIND ApnSkjema WHERE ApnSkjema.ButikkNr = Butiker.Butik AND
                                       ApnSkjema.Ar       = CB-Aar NO-LOCK NO-ERROR.
                  Run FixPopUp.
                  APPLY "GO" TO FRAME FRAME-A.
              END.
              RETURN.
          END.
      END.
  END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CFTreeView C-Win OCX.NodeCheck
PROCEDURE CFTreeView.TreeView.NodeCheck .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  Required for OCX.
    Node
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE INPUT PARAMETER p-Node AS COM-HANDLE NO-UNDO.
  hNode = p-Node.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CFTreeView C-Win OCX.NodeClick
PROCEDURE CFTreeView.TreeView.NodeClick .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  Required for OCX.
    Node
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER p-Node AS COM-HANDLE NO-UNDO.
      ASSIGN hNode = p-Node.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-Dato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Dato C-Win
ON ENTRY OF FI-Dato IN FRAME DEFAULT-FRAME
DO:
  ASSIGN BUTTON-Print:SENSITIVE = FALSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Dato C-Win
ON LEAVE OF FI-Dato IN FRAME DEFAULT-FRAME
OR "RETURN" OF FI-Dato DO:
    DEFINE VARIABLE dTstDato AS DATE       NO-UNDO.
    ASSIGN dTstDato = DATE(FI-Dato:SCREEN-VALUE) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
        MESSAGE "Feil dato"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN NO-APPLY.
    END.
    ELSE IF INPUT FI-Dato >= TODAY THEN DO:
        MESSAGE "Dato > idag"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN NO-APPLY.
    END.
    ELSE IF INPUT FI-Dato < DATE(1,1,CB-Aar) THEN DO:
        MESSAGE "Dato < 1/1"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN NO-APPLY.
    END.
    ASSIGN FI-Dato.
    APPLY "VALUE-CHANGED" TO CB-Aar.
    ASSIGN BUTTON-Print:SENSITIVE = TRUE.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Butikkregister
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Butikkregister C-Win
ON CHOOSE OF MENU-ITEM m_Butikkregister /* Butikkregister */
DO:
    RUN PopUpChoose ("BUTIK").
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_pningsskjema
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_pningsskjema C-Win
ON CHOOSE OF MENU-ITEM m_pningsskjema /* Åpningsskjema */
DO:
    RUN PopUpChoose ("SKJEMA").
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */

/* Om vi kommer från samma program                                      */
/* hParenSamma används för att frysa anropande program                  */
IF PROGRAM-NAME(2) MATCHES "*" + PROGRAM-NAME(1) + "*" THEN
    ASSIGN hParentSamma = CURRENT-WINDOW.
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.
    
/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE DO:
    RUN disable_UI.
    IF VALID-HANDLE(hSubWin) THEN
        APPLY "CLOSE" TO hSubWin.
    IF VALID-HANDLE(chCFTreeView) THEN
        RELEASE OBJECT chCFTreeView NO-ERROR.
    IF VALID-HANDLE(CFTreeView) THEN
        DELETE OBJECT CFTreeView NO-ERROR.
    ASSIGN chCFTreeView  = ?
           CFTreeView    = ?.
    IF VALID-HANDLE(hParentSamma) THEN DO:
        ASSIGN hParentSamma:SENSITIVE = TRUE
               CURRENT-WINDOW = hParentSamma.
        APPLY "ENTRY" TO CURRENT-WINDOW.
    END.
END.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.
/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
    {syspara.i 1 3 1 cLand}
  IF cLand = "" THEN
    ASSIGN cLand = "NOR". /* syspara.i !!!!!! */
  ASSIGN dFG31Dec       = DATE(12,31,YEAR(TODAY) - 1).
  RUN InitCombo.
/*   IF NOT PROGRAM-NAME(2) MATCHES "*" + PROGRAM-NAME(1) + "*" THEN */
  IF NOT VALID-HANDLE(hParentSamma) THEN
      RUN Create_TT.
  ELSE DO:
      ASSIGN hParentSamma:SENSITIVE = FALSE
             hParentSamma:X   = 1
             hParentSamma:Y   = 1
             CURRENT-WINDOW:X = 380
             CURRENT-WINDOW:Y = 1.
  END.
  RUN enable_UI.
  {lng.i}
/*   IF NOT PROGRAM-NAME(2) MATCHES "*" + PROGRAM-NAME(1) + "*" THEN */
  IF NOT VALID-HANDLE(hParentSamma) THEN
      APPLY "VALUE-CHANGED" TO CB_KjedeNavn.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Apply-mouse-menu-click C-Win 
PROCEDURE Apply-mouse-menu-click :
/*------------------------------------------------------------------------------
Purpose:     Programatic click the right mouse button on a widget
Parameters:  Widget-handle on which you want to click
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER  p-wh   AS WIDGET-HANDLE  NO-UNDO.
   DEF VAR ReturnValue AS INTEGER NO-UNDO.
   RUN SendMessage{&A} in hpApi (INPUT p-wh:HWND, 
                                 INPUT {&WM_RBUTTONDOWN},
                                 INPUT {&MK_RBUTTON},
                                 INPUT 0,
                                 OUTPUT ReturnValue).
   RUN SendMessage{&A} in hpApi (INPUT p-wh:HWND, 
                                 INPUT {&WM_RBUTTONUP},
                                 INPUT 0, 
                                 INPUT 0,
                                 OUTPUT ReturnValue).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE control_load C-Win  _CONTROL-LOAD
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

OCXFile = SEARCH( "w-Mottakkontroll.wrx":U ).
IF OCXFile = ? THEN
  OCXFile = SEARCH(SUBSTRING(THIS-PROCEDURE:FILE-NAME, 1,
                     R-INDEX(THIS-PROCEDURE:FILE-NAME, ".":U), "CHARACTER":U) + "wrx":U).

IF OCXFile <> ? THEN
DO:
  ASSIGN
    chCFTreeView = CFTreeView:COM-HANDLE
    UIB_S = chCFTreeView:LoadControls( OCXFile, "CFTreeView":U)
    CFTreeView:NAME = "CFTreeView":U
    chFarger = Farger:COM-HANDLE
    UIB_S = chFarger:LoadControls( OCXFile, "Farger":U)
    Farger:NAME = "Farger":U
  .
  RUN initialize-controls IN THIS-PROCEDURE NO-ERROR.
END.
ELSE MESSAGE "w-Mottakkontroll.wrx":U SKIP(1)
             "The binary control file could not be found. The controls cannot be loaded."
             VIEW-AS ALERT-BOX TITLE "Controls Not Loaded".

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Create_TT C-Win 
PROCEDURE Create_TT :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iFeilType AS INTEGER    NO-UNDO.
  DEFINE VARIABLE lFeil AS LOGICAL    NO-UNDO.
  FOR EACH Kjede NO-LOCK:
      CREATE TT_Kjede.
      BUFFER-COPY Kjede TO TT_Kjede.
      FOR EACH KjedeRegion OF TT_Kjede NO-LOCK:
          CREATE TT_KjedeRegion.
          BUFFER-COPY KjedeRegion TO TT_KjedeRegion.
          FOR EACH KjedeDistrikt OF KjedeRegion NO-LOCK.
              CREATE TT_KjedeDistrikt.
              BUFFER-COPY KjedeDistrikt TO TT_KjedeDistrikt.
              FOR EACH KjedensButikker OF KjedeDistrikt NO-LOCK
                  WHERE CAN-FIND(Butiker WHERE Butiker.Butik = KjedensButikker.ButikkNr AND Butiker.ApningsDato <> ?).
/*                   WHERE CAN-FIND(Butiker WHERE Butiker.Butik = KjedensButikker.ButikkNr). */
                  FIND Butiker WHERE Butiker.Butik = KjedensButikker.ButikkNr NO-LOCK. /* används i Butikkfeil() */
                  IF Butiker.NedlagtDato <> ? AND Butiker.NedlagtDato < TODAY THEN
                      NEXT.
                  IF NOT CAN-FIND(FIRST kasse WHERE kasse.butikknr = butiker.butik AND Kasse.Aktiv = TRUE) THEN
                      NEXT.
                  CREATE TT_KjedensButikker.
                  BUFFER-COPY KjedensButikker TO TT_KjedensButikker.
                  ASSIGN lFeil = ButikkFeil(OUTPUT iFeilType)
                         TT_KjedensButikker.Feil     = lFeil
                         TT_KjedensButikker.FeilType = iFeilType.
                  IF lFeil THEN
                      ASSIGN TT_Kjede.Feil          = TRUE
                             TT_KjedeRegion.Feil    = TRUE
                             TT_KjedeDistrikt.Feil  = TRUE.
              END.
              RELEASE TT_KjedensButikker.
          END.
          RELEASE TT_KjedeDistrikt.
      END.
      RELEASE TT_KjedeRegion.
  END.
  RELEASE TT_Kjede.
  RELEASE Kjede.
  RELEASE KjedeRegion.
  RELEASE Kjededistrikt.
  RELEASE KjedensButikker.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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
  RUN control_load.

  {&OPEN-QUERY-DEFAULT-FRAME}
  GET FIRST DEFAULT-FRAME.
  DISPLAY CB_KjedeNavn FI-Dato CB-Aar 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE CB_KjedeNavn FI-Dato CB-Aar BUTTON-Print BUTTON-Igaar BUTTON-SokDato 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FillTreeView C-Win 
PROCEDURE FillTreeView :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE ix1 AS INTEGER INIT 0.
DEFINE VARIABLE ix2 AS INTEGER INIT 0.
DEFINE VARIABLE ix3 AS INTEGER INIT 0.
/* chCFTreeView:TreeView:ImageList = chCFImagelist:ImageList. */
FIND TT_Kjede WHERE TT_Kjede.KjedeNr = CB_KjedeNavn.
FIND Kjede OF TT_Kjede.
ASSIGN hNode = chCFTreeView:TreeView:Nodes:Add(, ,"k", TT_Kjede.KjedeNavn, ,)
       hNode:Tag = "K" + CHR(1) + STRING(ROWID(TT_Kjede)) + CHR(1) + STRING(TT_Kjede.Feil,"J/N")
       hNode:CHECKED  = TT_Kjede.Feil
       hNode:Expanded = hNode:CHECKED.
/* >>> chCFTreeView:TreeView:Nodes:Add("k",4 ,"r" , "Regioner", ,). */
/*
chCFTreeView:TreeView:Nodes:Add("r",4 ,"d" , "District", 8,).
chCFTreeView:TreeView:Nodes:Add("d",4 ,"b" , "Butiker", 8,).
*/
FOR EACH TT_KjedeRegion OF TT_Kjede NO-LOCK.
    ASSIGN ix1 = ix1 + 1
           hNode = chCFTreeView:TreeView:Nodes:Add("k",4 ,"r" + STRING(ix1), TT_KjedeRegion.RegionNavn, ,)
/* >>>    chCFTreeView:TreeView:Nodes:Add("r",4 ,"r" + STRING(ix1), TT_KjedeRegion.RegionNavn, ,). */
           hNode:Tag = "R" + CHR(1) + STRING(ROWID(TT_KjedeRegion)) + CHR(1) + STRING(TT_KjedeRegion.Feil,"J/N")
           hNode:CHECKED  = TT_KjedeRegion.Feil
           hNode:Expanded = hNode:CHECKED.
    FOR EACH TT_KjedeDistrikt OF TT_KjedeRegion NO-LOCK.
        ASSIGN ix2 = ix2 + 1
               hNode = chCFTreeView:TreeView:Nodes:Add("r" + STRING(ix1),4 ,"d" + STRING(ix2), TT_KjedeDistrikt.DistriktNavn, ,)
               hNode:Tag = "D" + CHR(1) + STRING(ROWID(TT_KjedeDistrikt)) + CHR(1) + STRING(TT_KjedeDistrikt.Feil,"J/N")
               hNode:CHECKED  = TT_KjedeDistrikt.Feil
               hNode:Expanded = hNode:CHECKED.
        FOR EACH TT_KjedensButikker OF TT_KjedeDistrikt NO-LOCK.
            ASSIGN ix3 = ix3 + 1
                   hNode = chCFTreeView:TreeView:Nodes:Add("d" + STRING(ix2),4 ,"b" + STRING(ix3), STRING(TT_KjedensButikker.ButikkNr,"zzzzz9") + " " + TT_KjedensButikker.ButikkNavn, ,)
                   hNode:Tag = "B" + CHR(1) + STRING(ROWID(TT_KjedensButikker)) + CHR(1) + STRING(TT_KjedensButikker.Feil,"J/N")
                   hNode:CHECKED  = TT_KjedensButikker.Feil
                   hNode:Expanded = hNode:CHECKED
                   hNode:BackColor = IF TT_KjedensButikker.FeilType = 6 THEN hNode:BackColor ELSE
                                     INT(ENTRY(TT_KjedensButikker.FeilType,cColorList)).
        END.
    END.
END.
RELEASE TT_KjedeRegion.
RELEASE TT_KjedeDistrikt.
RELEASE TT_KjedensButikker.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FixPopUp C-Win 
PROCEDURE FixPopUp :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    MENU-ITEM m_pningsskjema:SENSITIVE IN MENU POPUP-MENU-FRAME-A = AVAIL ApnSkjema.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetData C-Win 
PROCEDURE GetData :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER cLevel AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER cRowid AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE         rRowid   AS ROWID      NO-UNDO.
  ASSIGN rRowId = TO-ROWID(cRowId).
  CASE cLevel:
      WHEN "K" THEN DO:
          RELEASE TT_KjedeRegion.
          RELEASE TT_KjedeDistrikt.
          RELEASE TT_KjedensButikker.
          IF AVAIL TT_Kjede AND ROWID(TT_Kjede) = rRowId THEN
              RETURN.
          FIND TT_Kjede WHERE ROWID(TT_Kjede) = rRowId NO-LOCK.
      END.
      WHEN "R" THEN DO:
          RELEASE TT_KjedeDistrikt.
          RELEASE TT_KjedensButikker.
          IF AVAIL TT_KjedeRegion AND ROWID(TT_KjedeRegion) = rRowId THEN
              RETURN.
          FIND TT_KjedeRegion WHERE ROWID(TT_KjedeRegion) = rRowId NO-LOCK.
      END.
      WHEN "D" THEN DO:
          RELEASE TT_KjedensButikker.
          IF AVAIL TT_KjedeDistrikt AND ROWID(TT_KjedeDistrikt) = rRowId THEN
              RETURN.
          FIND TT_KjedeDistrikt WHERE ROWID(TT_KjedeDistrikt) = rRowId NO-LOCK.
          FIND TT_KjedeRegion OF TT_KjedeDistrikt WHERE TT_KjedeRegion.RegionNr = TT_KjedeDistrikt.RegionNr NO-LOCK.
      END.
      WHEN "B" THEN DO:
          IF AVAIL TT_KjedensButikker AND ROWID(TT_KjedensButikker) = rRowId THEN
              RETURN.
          FIND TT_KjedensButikker WHERE ROWID(TT_KjedensButikker) = rRowId NO-LOCK.
          FIND TT_KjedeRegion OF TT_Kjede WHERE TT_KjedeRegion.RegionNr = TT_KjedensButikker.RegionNr NO-LOCK.
          FIND TT_KjedeDistrikt OF TT_KjedeRegion WHERE TT_KjedeDistrikt.DistriktNr = TT_KjedensButikker.DistriktNr NO-LOCK.
      END.

  END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitCombo C-Win 
PROCEDURE InitCombo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iCount         AS INTEGER    NO-UNDO.
  DEFINE VARIABLE cListItemPairs AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cListItems     AS CHARACTER  NO-UNDO.
  DO WITH FRAME {&FRAME-NAME}:
    FOR EACH Kjede NO-LOCK.
        ASSIGN cListItemPairs = cListItemPairs + (IF cListItemPairs = "" THEN "" ELSE ",") +
            Kjede.KjedeNavn + "," + STRING(Kjede.KjedeNr).
    END.
    ASSIGN CB_KjedeNavn:LIST-ITEM-PAIRS = cListItemPairs
           CB_KjedeNavn = INT(ENTRY(2,cListItemPairs)).
  END.
  DO iCount = YEAR(TODAY) - 1 TO YEAR(TODAY) + 1:
      ASSIGN cListItems = cListItems + (IF cListItems = "" THEN "" ELSE ",") +
          STRING(iCount).
  END.
  ASSIGN CB-Aar:LIST-ITEMS = cListItems
         CB-Aar = YEAR(TODAY)
         FI-Dato = TODAY - 1.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initialize-controls C-Win 
PROCEDURE initialize-controls :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

/* CFTabStrip:MOVE-TO-BOTTOM(). */

/* *** Initialize ListView Control **** */

/* chCFListView:ListView:View = 0. */

/* The image list icons have been set through the Custom dialog off
   of the Property sheet */
/* chCFListView:ListView:Icons = chCFImagelist:ImageList. */

/* FOR EACH _file WHERE not _file._hidden:                   */
/*     chCFListView:ListView:ListItems:Add(,,_file-name,1,). */
/* END.                                                      */
    
/* *** Initialize TreeView Control **** */
    
  ASSIGN chCFTreeView:TreeView:LineStyle   = 1 /* RootLines */
         chCFTreeView:TreeView:Style       = 6 /* Lines, plus/minus, image, and text */
         chCFTreeView:TreeView:Indentation = 10
         chCFTreeView:TreeView:LabelEdit   = 1
         chCFTreeView:TreeView:Checkboxes  = TRUE
         chCFTreeView:TreeView:BorderStyle = 0
         chFarger = chFarger:vsFlexGrid
         chFarger:Rows      = 1
         chFarger:FixedRows = 0
         chFarger:FixedCols = 0
         chFarger:Cols      = 5
         chFarger:TextMatrix(0,2)  = IF cLand = "SVE" THEN "Schema saknas" ELSE "Skjema mangler"
         chFarger:TextMatrix(0,3)  = IF cLand = "SVE" THEN "Data saknas" ELSE "Data mangler"
         chFarger:TextMatrix(0,1)  = "Data OK"
         chFarger:TextMatrix(0,4)  = IF cLand = "SVE" THEN "Inte aktiv" ELSE "Ikke aktiv"
         chFarger:Cell(6,0,1,0,1)  = INT(ENTRY(1,cColorList))
         chFarger:Cell(6,0,2,0,2)  = INT(ENTRY(2,cColorList)) 
/*         chFarger:Cell(13,0,2,0,2) = TRUE */
         chFarger:Cell(6,0,3,0,3)  = INT(ENTRY(3,cColorList))
         chFarger:Cell(6,0,4,0,4)  = INT(ENTRY(4,cColorList))
         chFarger:BorderStyle      = 1
         chFarger:ColWidth(0)      = 1
         chFarger:ColWidth(1)      = 1300
         chFarger:ColWidth(2)      = 1435
         chFarger:ColWidth(3)      = 1300
         chFarger:ColWidth(4)      = 1300
         chFarger:Enabled          = FALSE /* Updateable grid */
         chFarger:ROW              = 0.
/*    RUN FillTreeView. */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PopUpChoose C-Win 
PROCEDURE PopUpChoose :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER cRutin AS CHARACTER  NO-UNDO.
  DEFINE        VARIABLE  iFeilType AS INTEGER NO-UNDO.
  DEFINE        VARIABLE  lFeil     AS LOGICAL    NO-UNDO.
  DEFINE        VARIABLE  rRowId AS ROWID      NO-UNDO.
  IF cRutin = "BUTIK" AND AVAIL Butiker THEN
      RUN w-vButiker.w (RECID(Butiker),"ENDRE").
  ELSE IF cRutin = "SKJEMA" AND AVAIL ApnSkjema THEN DO:
      ASSIGN rRowId = ROWID(ApnSkjema).
      RUN w-ButOppetMal.w (INPUT-OUTPUT rRowId,?,?,?,"").
      IF RETURN-VALUE = "AVBRYT" THEN
          RETURN.
  END.
  ASSIGN lFeil = ButikkFeil(OUTPUT iFeilType).
  IF TT_KjedensButikker.FeilType <> iFeilType THEN DO:
      ASSIGN TT_KjedensButikker.Feil = lFeil
             TT_KjedensButikker.FeilType = iFeilType
             hNode:CHECKED = TT_KjedensButikker.Feil
             hNode:BackColor = IF TT_KjedensButikker.FeilType = 6 THEN hNode:BackColor ELSE
                               INT(ENTRY(TT_KjedensButikker.FeilType,cColorList)).
      chCFTreeView:TreeView:REFRESH().
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE VisDag C-Win 
PROCEDURE VisDag :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER ipDag AS DATE       NO-UNDO.
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN dFG31Dec                 = DATE(12,31,YEAR(ipDag) - 1)
           iOnsketDag               = ipDag - dFG31Dec
           CB-Aar                   = YEAR(ipDag)
           CB-Aar:SCREEN-VALUE      = STRING(CB-Aar)
           CB-Aar:SENSITIVE         = FALSE
           BUTTON-SokDato:SENSITIVE = FALSE
           C-Win:TITLE              = C-Win:TITLE + " " + STRING(ipDag)
           FI-Dato:SCREEN-VALUE     = STRING(ipDag)
           FI-Dato:SENSITIVE        = FALSE
           Button-Igaar:SENSITIVE   = FALSE.
    RUN Create_TT.
    APPLY "VALUE-CHANGED" TO CB_KjedeNavn.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ButikkFeil C-Win 
FUNCTION ButikkFeil RETURNS LOGICAL
  ( OUTPUT iFeilType AS INTEGER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cOpenClosed AS CHARACTER  NO-UNDO.
  FIND CURRENT Butiker NO-LOCK.
  IF CB-Aar > YEAR(TODAY) THEN DO:
      ASSIGN iFeilType = 6. /* Vi färglägger inte */
  END.
  ELSE IF Butiker.ApningsDato <> ? AND YEAR(Butiker.ApningsDato) > CB-Aar THEN DO:
      ASSIGN iFeilType = 4.
  END.
  ELSE IF Butiker.NedlagtDato <> ? AND YEAR(Butiker.NedlagtDato) < CB-Aar THEN DO:
      ASSIGN iFeilType = 5.
  END.
  ELSE DO:
      FIND ApnSkjema WHERE ApnSkjema.ButikkNr = TT_KjedensButikker.ButikkNr AND
                           ApnSkjema.Ar = CB-Aar NO-LOCK NO-ERROR.
      IF NOT AVAIL ApnSkjema THEN
          ASSIGN iFeilType = 2.
      ELSE IF iOnsketDag = 0 THEN DO:
          IF CB-Aar < YEAR(TODAY) THEN
              ASSIGN cOpenClosed = ApnSkjema.OpenClosed.
          ELSE
              ASSIGN cOpenClosed = SUBSTR(ApnSkjema.OpenClosed,1,2 * (FI-Dato - dFG31Dec) - 1).
/*               ASSIGN cOpenClosed = SUBSTR(ApnSkjema.OpenClosed,1,2 * (TODAY - dFG31Dec - 1) - 1). */
          IF CAN-DO(cOpenClosed,"1") OR CAN-DO(cOpenClosed,"2") THEN
              ASSIGN iFeilType = 3.
          ELSE 
              ASSIGN iFeilType = 1.
      END.
      ELSE DO: /* Visar dataresultat viss dato */
          ASSIGN iFeilType = IF CAN-DO("1,2",ENTRY(iOnsketDag,ApnSkjema.OpenClosed)) THEN 3 ELSE 1.
      END.
  END.
  RETURN iFeilType = 2 OR iFeilType = 3.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

