&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: 

  Description:        Container for a JukeBox window program

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author:            brynjar@chemistry.no

  Created:           18.oct.2006

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

DEF VAR bOk         AS LOG    NO-UNDO.
DEF VAR ix          AS INT    NO-UNDO.
DEF VAR hBrowse     AS HANDLE NO-UNDO.
DEF VAR hBrowse2    AS HANDLE NO-UNDO.
DEF VAR hToolbar    AS HANDLE NO-UNDO.
DEF VAR hToolbar2   AS HANDLE NO-UNDO.
DEF VAR hFieldMap   AS HANDLE NO-UNDO.
DEF VAR hFieldMap2  AS HANDLE NO-UNDO.
DEF VAR cCL         AS CHAR   NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rsAktiv fiFortjenesteProf1 ValKod ValNr ~
ValAktiv ValNavn ValLand ValKurs indeks KasseValKurs Retur ValDatum ~
cmbPrisProfil rsAktiv-2 fiFortjeneste ValKod-2 ValNavn-2 ValLand-2 ~
ValKurs-2 indeks-2 KasseValKurs-2 Retur-2 ValDatum-2 ValNr-2 ValAktiv-2 ~
brwKasValutaProf1 tbValutaProf1 brwKasValuta tbValuta 
&Scoped-Define DISPLAYED-OBJECTS rsAktiv fiFortjenesteProf1 ValKod ValNr ~
ValAktiv ValNavn ValLand ValKurs indeks KasseValKurs Retur ValDatum ~
cmbPrisProfil rsAktiv-2 fiFortjeneste ValKod-2 ValNavn-2 ValLand-2 ~
ValKurs-2 indeks-2 KasseValKurs-2 Retur-2 ValDatum-2 ValNr-2 ValAktiv-2 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD AdjustBrowseColumns C-Win 
FUNCTION AdjustBrowseColumns RETURNS LOGICAL
  ( INPUT ihBrowse     AS HANDLE,
    INPUT icBrowseName AS CHAR) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD SaveFortjeneste C-Win 
FUNCTION SaveFortjeneste RETURNS LOGICAL
  ( INPUT icProfilnr        AS CHAR,
    INPUT icFortjeneste     AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE cmbPrisProfil AS CHARACTER FORMAT "X(256)":U 
     LABEL "Prisprofil" 
     VIEW-AS COMBO-BOX INNER-LINES 40
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 36.8 BY 1 NO-UNDO.

DEFINE VARIABLE indeks AS INTEGER FORMAT ">>9" INITIAL 0 
     LABEL "Antall" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEMS "1","100" 
     DROP-DOWN-LIST
     SIZE 17.8 BY 1.

DEFINE VARIABLE fiFortjeneste AS DECIMAL FORMAT "->>9.99":U INITIAL 0 
     LABEL "Fortjeneste%" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fiFortjenesteProf1 AS DECIMAL FORMAT "->>9.99":U INITIAL 0 
     LABEL "Fortjeneste%" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE indeks-2 AS INTEGER FORMAT ">>9" INITIAL 0 
     LABEL "Antall" 
     VIEW-AS FILL-IN 
     SIZE 6.2 BY 1.

DEFINE VARIABLE KasseValKurs AS DECIMAL FORMAT "zz9.999999" INITIAL 0 
     LABEL "Kjøpkurs i kasse" 
     VIEW-AS FILL-IN 
     SIZE 17.8 BY 1.

DEFINE VARIABLE KasseValKurs-2 AS DECIMAL FORMAT "zz9.999999" INITIAL 0 
     LABEL "Kjøpkurs i kasse" 
     VIEW-AS FILL-IN 
     SIZE 17.6 BY 1.

DEFINE VARIABLE Retur AS DECIMAL FORMAT "zz9.999999" INITIAL 0 
     LABEL "Salgkurs i kasse" 
     VIEW-AS FILL-IN 
     SIZE 17.8 BY 1.

DEFINE VARIABLE Retur-2 AS DECIMAL FORMAT "zz9.999999" INITIAL 0 
     LABEL "Salgkurs i kasse" 
     VIEW-AS FILL-IN 
     SIZE 17.8 BY 1.

DEFINE VARIABLE ValDatum AS DATE FORMAT "99/99/99" 
     LABEL "Oppdatert" 
     VIEW-AS FILL-IN 
     SIZE 17.8 BY 1 TOOLTIP "Dato for siste oppdatering".

DEFINE VARIABLE ValDatum-2 AS DATE FORMAT "99/99/99" 
     LABEL "Oppdatert" 
     VIEW-AS FILL-IN 
     SIZE 17.6 BY 1 TOOLTIP "Dato for siste oppdatering".

DEFINE VARIABLE ValKod AS CHARACTER FORMAT "x(3)" 
     LABEL "ValutaKode" 
     VIEW-AS FILL-IN 
     SIZE 8.6 BY 1 TOOLTIP "Valutakode (tre karrakterer)".

DEFINE VARIABLE ValKod-2 AS CHARACTER FORMAT "x(3)" 
     LABEL "ValutaKode" 
     VIEW-AS FILL-IN 
     SIZE 8.6 BY 1 TOOLTIP "Valutakode (tre karrakterer)".

DEFINE VARIABLE ValKurs AS DECIMAL FORMAT "zz9.999999" INITIAL 0 
     LABEL "Valutakurs" 
     VIEW-AS FILL-IN 
     SIZE 17.8 BY 1 TOOLTIP "Valutakurs".

DEFINE VARIABLE ValKurs-2 AS DECIMAL FORMAT "zz9.999999" INITIAL 0 
     LABEL "Valutakurs" 
     VIEW-AS FILL-IN 
     SIZE 17.6 BY 1 TOOLTIP "Valutakurs".

DEFINE VARIABLE ValLand AS CHARACTER FORMAT "x(15)" 
     LABEL "Land" 
     VIEW-AS FILL-IN 
     SIZE 17.8 BY 1 TOOLTIP "Land hvor valuta gjelder".

DEFINE VARIABLE ValLand-2 AS CHARACTER FORMAT "x(15)" 
     LABEL "Land" 
     VIEW-AS FILL-IN 
     SIZE 17.6 BY 1 TOOLTIP "Land hvor valuta gjelder".

DEFINE VARIABLE ValNavn AS CHARACTER FORMAT "X(30)" 
     LABEL "Valutanavn" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1.

DEFINE VARIABLE ValNavn-2 AS CHARACTER FORMAT "X(30)" 
     LABEL "Valutanavn" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1.

DEFINE VARIABLE ValNr AS INTEGER FORMAT ">9" INITIAL 0 
     LABEL "Valutanr" 
     VIEW-AS FILL-IN 
     SIZE 4.8 BY 1 TOOLTIP "Bestemmer rekkefølge i kassens valutaliste".

DEFINE VARIABLE ValNr-2 AS INTEGER FORMAT ">9" INITIAL 0 
     LABEL "Valutanr" 
     VIEW-AS FILL-IN 
     SIZE 4.8 BY 1 TOOLTIP "Bestemmer rekkefølge i kassens valutaliste".

DEFINE VARIABLE rsAktiv AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Aktive", 1,
"Alle", 2
     SIZE 22 BY .81 NO-UNDO.

DEFINE VARIABLE rsAktiv-2 AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Aktive", 1,
"Alle", 2
     SIZE 22 BY .81 NO-UNDO.

DEFINE RECTANGLE brwKasValuta
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 81 BY 10.24.

DEFINE RECTANGLE brwKasValutaProf1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 81 BY 10.24.

DEFINE RECTANGLE tbValuta
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 9.8 BY .91.

DEFINE RECTANGLE tbValutaProf1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 9.8 BY .91.

DEFINE VARIABLE ValAktiv AS LOGICAL INITIAL yes 
     LABEL "Aktiv" 
     VIEW-AS TOGGLE-BOX
     SIZE 13 BY 1.

DEFINE VARIABLE ValAktiv-2 AS LOGICAL INITIAL yes 
     LABEL "Aktiv" 
     VIEW-AS TOGGLE-BOX
     SIZE 13 BY 1.

DEFINE BUTTON btnSplitBarX 
     IMAGE-UP FILE "bmp/tableft.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Button 1" 
     SIZE .8 BY 10.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     rsAktiv AT ROW 2.52 COL 3 NO-LABEL
     fiFortjenesteProf1 AT ROW 2.43 COL 42 COLON-ALIGNED
     ValKod AT ROW 14 COL 22.2 COLON-ALIGNED
     ValNr AT ROW 14 COL 49.4 COLON-ALIGNED
     ValAktiv AT ROW 13.95 COL 58.8
     ValNavn AT ROW 15.05 COL 22.2 COLON-ALIGNED
     ValLand AT ROW 16.1 COL 22.2 COLON-ALIGNED
     ValKurs AT ROW 17.14 COL 22.2 COLON-ALIGNED
     indeks AT ROW 18.19 COL 22.2 COLON-ALIGNED
     KasseValKurs AT ROW 19.24 COL 22.2 COLON-ALIGNED
     Retur AT ROW 20.24 COL 22 COLON-ALIGNED
     ValDatum AT ROW 21.29 COL 22 COLON-ALIGNED
     cmbPrisProfil AT ROW 1.24 COL 128 COLON-ALIGNED
     rsAktiv-2 AT ROW 2.52 COL 87.6 NO-LABEL
     fiFortjeneste AT ROW 2.43 COL 128 COLON-ALIGNED
     ValKod-2 AT ROW 14 COL 109.4 COLON-ALIGNED
     ValNavn-2 AT ROW 15.05 COL 109.4 COLON-ALIGNED
     ValLand-2 AT ROW 16.1 COL 109.4 COLON-ALIGNED
     ValKurs-2 AT ROW 17.14 COL 109.4 COLON-ALIGNED
     indeks-2 AT ROW 18.19 COL 109.4 COLON-ALIGNED
     KasseValKurs-2 AT ROW 19.24 COL 109.4 COLON-ALIGNED
     Retur-2 AT ROW 20.29 COL 109.4 COLON-ALIGNED
     ValDatum-2 AT ROW 21.29 COL 109.4 COLON-ALIGNED
     ValNr-2 AT ROW 14 COL 136.6 COLON-ALIGNED
     ValAktiv-2 AT ROW 13.95 COL 146
     brwKasValutaProf1 AT ROW 3.62 COL 2
     tbValutaProf1 AT ROW 1.29 COL 2
     brwKasValuta AT ROW 3.62 COL 86.2
     tbValuta AT ROW 1.29 COL 86.2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 167 BY 21.95.

DEFINE FRAME frmSplitBarX
     btnSplitBarX AT ROW 1 COL 14.2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 70 ROW 3.71
         SIZE 29 BY 10.


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
         TITLE              = "<Insert window title>"
         HEIGHT             = 21.95
         WIDTH              = 167
         MAX-HEIGHT         = 29.95
         MAX-WIDTH          = 213
         VIRTUAL-HEIGHT     = 29.95
         VIRTUAL-WIDTH      = 213
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB C-Win 
/* ************************* Included-Libraries *********************** */

{incl/devmode.i}
{incl/custdevmode.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* REPARENT FRAME */
ASSIGN FRAME frmSplitBarX:FRAME = FRAME DEFAULT-FRAME:HANDLE.

/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME Custom                                                    */

DEFINE VARIABLE XXTABVALXX AS LOGICAL NO-UNDO.

ASSIGN XXTABVALXX = FRAME frmSplitBarX:MOVE-BEFORE-TAB-ITEM (rsAktiv:HANDLE IN FRAME DEFAULT-FRAME)
/* END-ASSIGN-TABS */.

/* SETTINGS FOR FRAME frmSplitBarX
                                                                        */
ASSIGN 
       btnSplitBarX:MOVABLE IN FRAME frmSplitBarX          = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME frmSplitBarX
/* Query rebuild information for FRAME frmSplitBarX
     _Query            is NOT OPENED
*/  /* FRAME frmSplitBarX */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* <Insert window title> */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* <Insert window title> */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME frmSplitBarX
&Scoped-define SELF-NAME btnSplitBarX
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSplitBarX C-Win
ON END-MOVE OF btnSplitBarX IN FRAME frmSplitBarX /* Button 1 */
DO:
  DYNAMIC-FUNCTION("ResizeKeepsWindowLocked",THIS-PROCEDURE:CURRENT-WINDOW) NO-ERROR.
  DYNAMIC-FUNCTION("setSplitBarX",THIS-PROCEDURE:CURRENT-WINDOW,btnSplitBarX:HANDLE IN FRAME frmSplitBarX,NO).
  DEF VAR hColumn AS HANDLE NO-UNDO.
  hColumn = hBrowse:GET-BROWSE-COLUMN(1).
  APPLY "end-resize" TO hColumn.
  DYNAMIC-FUNCTION("ResizeKeepsWindowLocked",?).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME cmbPrisProfil
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cmbPrisProfil C-Win
ON VALUE-CHANGED OF cmbPrisProfil IN FRAME DEFAULT-FRAME /* Prisprofil */
DO:  
  IF cmbPrisProfil:SCREEN-VALUE NE ? AND cmbPrisProfil:SCREEN-VALUE NE "0" THEN
    DYNAMIC-FUNCTION("setAttribute",hBrowse2,"queryFilter"," AND Profilnr = " + cmbPrisProfil:SCREEN-VALUE).
  ELSE
    DYNAMIC-FUNCTION("setAttribute",hBrowse2,"queryFilter","").
  
  IF INTEGER(cmbPrisProfil:SCREEN-VALUE) > 1 AND cmbPrisProfil:SCREEN-VALUE NE ? THEN DO:
    ASSIGN fiFortjeneste:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldValues","KasValuta",
                                                          "WHERE ValKod = 'F%' AND Profilnr = " + cmbPrisProfil:SCREEN-VALUE,"ValKurs")
           fiFortjeneste:SENSITIVE = YES.  
    DYNAMIC-FUNCTION("setAttribute",hToolbar2,"enabledEvents","COPY").
    DYNAMIC-FUNCTION("setAttribute",hToolbar2,"disabledEvents","").
  END.
  ELSE DO:
    ASSIGN fiFortjeneste:SCREEN-VALUE = ?
           fiFortjeneste:SENSITIVE = NO.  
    DYNAMIC-FUNCTION("setAttribute",hToolbar2,"disabledEvents","COPY").
    DYNAMIC-FUNCTION("setAttribute",hToolbar2,"enabledEvents","").
  END.

  RUN InvokeMethod(hBrowse2,"OpenQuery").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiFortjeneste
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiFortjeneste C-Win
ON LEAVE OF fiFortjeneste IN FRAME DEFAULT-FRAME /* Fortjeneste% */
DO:
  IF fiFortjeneste:MODIFIED THEN DO:
    ASSIGN fiFortjeneste.
    IF fiFortjeneste = 0 AND DYNAMIC-FUNCTION("DoMessage",0,1,"Fortjeneste er satt til 0. Vil du lagre dette?","","") = 2 THEN 
      fiFortjeneste:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldValues","KasValuta",
                                                    "WHERE ValKod = 'F%' AND Profilnr = " + cmbPrisProfil:SCREEN-VALUE,"ValKurs").  
    ELSE IF Savefortjeneste(cmbPrisProfil:SCREEN-VALUE,fiFortjeneste:SCREEN-VALUE) THEN
      RUN InvokeMethod(hBrowse2,"OpenQuery").
    fiFortjeneste:MODIFIED = NO.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiFortjenesteProf1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiFortjenesteProf1 C-Win
ON LEAVE OF fiFortjenesteProf1 IN FRAME DEFAULT-FRAME /* Fortjeneste% */
DO:
  IF fiFortjenesteProf1:MODIFIED THEN DO:
    ASSIGN fiFortjenesteProf1.
    IF fiFortjenesteProf1 = 0 AND DYNAMIC-FUNCTION("DoMessage",0,1,"Fortjeneste er satt til 0. Vil du lagre dette?","","") = 2 THEN
      fiFortjenesteProf1:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldValues","KasValuta",
                                                         "WHERE ValKod = 'F%' AND Profilnr = 1","ValKurs").
    ELSE IF Savefortjeneste("1",fiFortjenesteProf1:SCREEN-VALUE) THEN
      RUN InvokeMethod(hBrowse,"OpenQuery").
    fiFortjenesteProf1:MODIFIED = NO.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rsAktiv
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rsAktiv C-Win
ON VALUE-CHANGED OF rsAktiv IN FRAME DEFAULT-FRAME
DO:
  ASSIGN rsAktiv.
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"queryWhere",
                   IF rsAktiv = 1 THEN " AND ValAktiv" ELSE "").
  RUN InvokeMethod(hBrowse,"OpenQuery").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rsAktiv-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rsAktiv-2 C-Win
ON VALUE-CHANGED OF rsAktiv-2 IN FRAME DEFAULT-FRAME
DO:
  ASSIGN rsAktiv-2.
  DYNAMIC-FUNCTION("setAttribute",hBrowse2,"queryWhere",
                   IF rsAktiv-2 = 1 THEN " AND ValAktiv" ELSE "").
  RUN InvokeMethod(hBrowse2,"OpenQuery").
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
  IF NOT DYNAMIC-FUNCTION("DoCleanUpObjects",THIS-PROCEDURE:CURRENT-WINDOW) THEN RETURN NO-APPLY.
  DYNAMIC-FUNCTION("SetCleanUpResize",THIS-PROCEDURE:CURRENT-WINDOW).
  PUBLISH "InvalidateHandle" (THIS-PROCEDURE).
  RUN disable_UI.
END.

{incl/wintrigg.i}

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  
 
  &IF DEFINED(UIB_is_Running) NE 0 &THEN
    RUN InitializeObject.
    RUN MoveToTop.
  &ENDIF
  
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CopyRecord C-Win 
PROCEDURE CopyRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF DYNAMIC-FUNCTION("ProcessQuery",hBrowse,"kasvaluta_kopier_til_profil.p",cmbPrisProfil:SCREEN-VALUE IN FRAME {&FRAME-NAME}) THEN DO:
  Savefortjeneste(cmbPrisProfil:SCREEN-VALUE,fiFortjeneste:SCREEN-VALUE).  
  RUN InvokeMethod(hBrowse2,"OpenQuery").
END.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplayRecord C-Win 
PROCEDURE DisplayRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR rRepos AS ROWID NO-UNDO.

RUN SUPER.

DO WITH FRAME {&FRAME-NAME}:
  IF DYNAMIC-FUNCTION("getCurrentObject") = hBrowse THEN
    ASSIGN ValKod:READ-ONLY  = YES
/*            ValLand:READ-ONLY = YES  */
/*            ValNavn:READ-ONLY = YES  */
           .
  ELSE
    ASSIGN ValKurs-2:SENSITIVE  = cmbPrisProfil:SCREEN-VALUE NE ? AND cmbPrisProfil:SCREEN-VALUE NE "0"
           Retur-2:SENSITIVE    = ValKurs-2:SENSITIVE
           ValNr-2:SENSITIVE    = ValKurs-2:SENSITIVE
           ValAktiv-2:SENSITIVE = ValKurs-2:SENSITIVE
           .
/*   ELSE IF hFieldMap2:AVAIL THEN DO:                                                                                  */
/*     bOk = hFieldMap:FIND-FIRST("WHERE ValKod = '" + hFieldMap2:BUFFER-FIELD("ValKod"):BUFFER-VALUE + "'") NO-ERROR.  */
/*     IF bOk THEN DO:                                                                                                  */
/*       rRepos = hFieldMap:ROWID.                                                                                      */
/*       hBrowse:DESELECT-ROWS() NO-ERROR.                                                                              */
/*       hBrowse:SET-REPOSITIONED-ROW(hBrowse:DOWN,"conditional").                                                      */
/*       hBrowse:QUERY:REPOSITION-TO-ROWID(rRepos) NO-ERROR.                                                            */
/*       IF NOT ERROR-STATUS:ERROR THEN DO:                                                                             */
/*         hBrowse:SELECT-FOCUSED-ROW().                                                                                */
/*         APPLY "VALUE-CHANGED" TO hBrowse.                                                                            */
/*       END.                                                                                                           */
/*     END.                                                                                                             */
/*   END.                                                                                                               */
END.
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
  DISPLAY rsAktiv fiFortjenesteProf1 ValKod ValNr ValAktiv ValNavn ValLand 
          ValKurs indeks KasseValKurs Retur ValDatum cmbPrisProfil rsAktiv-2 
          fiFortjeneste ValKod-2 ValNavn-2 ValLand-2 ValKurs-2 indeks-2 
          KasseValKurs-2 Retur-2 ValDatum-2 ValNr-2 ValAktiv-2 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE rsAktiv fiFortjenesteProf1 ValKod ValNr ValAktiv ValNavn ValLand 
         ValKurs indeks KasseValKurs Retur ValDatum cmbPrisProfil rsAktiv-2 
         fiFortjeneste ValKod-2 ValNavn-2 ValLand-2 ValKurs-2 indeks-2 
         KasseValKurs-2 Retur-2 ValDatum-2 ValNr-2 ValAktiv-2 brwKasValutaProf1 
         tbValutaProf1 brwKasValuta tbValuta 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  ENABLE btnSplitBarX 
      WITH FRAME frmSplitBarX IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-frmSplitBarX}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitializeObject C-Win 
PROCEDURE InitializeObject :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none> 
  Notes:       Refer to the <jukebox>\winsrc\samples for working examples for Sports2000
------------------------------------------------------------------------------*/
DEF VAR cButnr    AS CHAR NO-UNDO.
DEF VAR cProfilnr AS CHAR NO-UNDO.
DEF VAR cTekst    AS CHAR NO-UNDO.

RUN enable_UI.

DYNAMIC-FUNCTION("CreateStatusBar",THIS-PROCEDURE:CURRENT-WINDOW,"",0,YES,?).

DO WITH FRAME {&FRAME-NAME}:

  cCL = DYNAMIC-FUNCTION("getFieldValues","SysPara",
                         "WHERE SysHId = 5 and SysGr = 1 and ParaNr = 1","Parameter1").

  cButNr    = DYNAMIC-FUNCTION("getFieldValues","bruker","WHERE brukerid = '" + DYNAMIC-FUNCTION("getASuserId") + "'","ButikkNr").
  IF cButNr = ? OR cButNr = "0" THEN 
    cButNr = cCL.

  cProfilNr = DYNAMIC-FUNCTION("getFieldValues","Butiker","WHERE butik = " + cButNr,"ProfilNr").

  IF cProfilNr = ? OR cProfilnr = "0" THEN DO:
    DYNAMIC-FUNCTION("DoMessage",0,0,"Butikk mangler prisprofil","","").
    APPLY "close" TO THIS-PROCEDURE.
    RETURN.
  END.
  cTekst = TRIM('|0|' + DYNAMIC-FUNCTION("getFieldList","Prisprofil;Profilnr|Beskrivelse;Profilnr",
                         "WHERE " + (IF cProfilnr NE "1" THEN "Profilnr = '" + cProfilnr + "'" ELSE "Profilnr > 1") + " BY Profilnr")).

  ASSIGN cmbPrisProfil:DELIMITER       = "|"
         cmbPrisProfil:LIST-ITEM-PAIRS = cTekst
         cmbPrisProfil:SCREEN-VALUE    = (IF LOOKUP(cProfilNr,cTekst,'|') > 0 THEN cProfilNr ELSE "0")
         cmbPrisProfil:SENSITIVE       = cProfilnr = "1"
         cmbPrisProfil:BGCOLOR         = 15
         fiFortjenesteProf1:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldValues","KasValuta",
                                                            "WHERE ValKod = 'F%' AND Profilnr = 1","ValKurs")
         fiFortjenesteProf1:MODIFIED   = NO
         fiFortjenesteProf1:SENSITIVE  = cProfilnr = "1"
         .

  hBrowse = DYNAMIC-FUNCTION("NewBrowse"
          ,brwKasValutaProf1:HANDLE
          ,100
          ,"MULTIPLE,TITLE|Kurser prisprofil 1 (HK)"
          ,"KasValuta"
            + ";ValKod|Valuta"
            + ";ValNavn|Navn"
            + ";!ProfilNr|Profil"
            + ";ValKurs|Kurs"
            + ";Indeks|Ant"
            + ";KasseValKurs|Kjøpkurs"
            + ";Retur|Salgkurs"
            + ";ValDatum"
            + ";ValNr|Nr"
            + ";ValAktiv|Akt|J/N"
            + ";EDato"
            + ";BrukerID"
            + ";ValLand"
          ,"WHERE false"
          ,"").

  DYNAMIC-FUNCTION("setAttribute",hBrowse,"baseQuery","WHERE ValKod NE 'F%' AND Profilnr = 1").
  DYNAMIC-FUNCTION("setSortString",hBrowse,"ValKod").

  hFieldMap = DYNAMIC-FUNCTION("NewFieldMap"
          ,hBrowse:QUERY
          ,FRAME {&FRAME-NAME}:HANDLE
          ,(IF cProfilnr = "1" THEN "ValKurs,Retur,ValAktiv,indeks,ValKod,ValLand,ValNavn,Valnr" ELSE ""),""
          ,(IF cProfilnr = "1" THEN "" 
            ELSE "ValKurs,ValAktiv,indeks,ValKod,ValLand,ValNavn,ValNr,") 
          + "KasseValKurs,Retur,ValDatum",""
          ,"").

  DYNAMIC-FUNCTION("setAttribute",hFieldMap,"bufferExtraFields","ProfilNr").
  DYNAMIC-FUNCTION("setAttribute",hFieldMap,"bufferExtraValues","1").
  DYNAMIC-FUNCTION("setAttribute",hFieldMap,"customUpdateValProc","ignore").
  DYNAMIC-FUNCTION("setAttribute",hFieldMap,"postUpdateProc","kasvaluta_post_update.p").

  DYNAMIC-FUNCTION("CreateObjectLink",hFieldMap,hBrowse).

  hToolbar = DYNAMIC-FUNCTION("NewToolBar"
          ,tbValutaProf1:HANDLE
          ,"Fil"
          ,(IF cProfilnr = "1" THEN "new;Ny,undo;Angre,delete;Slett,save;Lagre," ELSE "")
        + "excel;Eksporter til E&xcel"
          ,"maxborder").

  DYNAMIC-FUNCTION("CreateObjectLink",hBrowse,hToolbar).
  DYNAMIC-FUNCTION("CreateObjectLink",hFieldMap,hToolbar).


  hBrowse2 = DYNAMIC-FUNCTION("NewBrowse"
          ,brwKasValuta:HANDLE
          ,100
          ,"TITLE|Kurser andre profiler"
          ,"KasValuta"
            + ";ProfilNr|Profil"
            + ";ValKod|Valuta"
            + ";ValNavn|Navn"
            + ";ValKurs|Kurs"
            + ";Indeks|Ant"
            + ";KasseValKurs|Kjøpkurs"
            + ";Retur|Salgkurs"
            + ";ValDatum"
            + ";ValNr|Nr"
            + ";ValAktiv|Akt|J/N"
            + ";EDato"
            + ";BrukerID"
            + ";ValLand"
          ,"WHERE false"
          ,"").

  DYNAMIC-FUNCTION("setAttribute",hBrowse2,"baseQuery","WHERE ValKod NE 'F%' AND Profilnr > 1").
  DYNAMIC-FUNCTION("setSortString",hBrowse2,"ValKod").

  hFieldMap2 = DYNAMIC-FUNCTION("NewFieldMap"
          ,hBrowse2:QUERY
          ,FRAME {&FRAME-NAME}:HANDLE
          ,"ValKurs,Retur,ValAktiv,ValNr"
          ,"ValKurs-2,Retur-2,ValAktiv-2,ValNr-2"
          ,"KasseValKurs,ValDatum,indeks,ValKod,ValLand,ValNavn"
          ,"KasseValKurs-2,ValDatum-2,indeks-2,ValKod-2,ValLand-2,ValNavn-2"
          ,"").
  
  DYNAMIC-FUNCTION("setAttribute",hFieldMap2,"bufferExtraFields","ProfilNr").

  DYNAMIC-FUNCTION("CreateObjectLink",hFieldMap2,hBrowse2).

  hToolbar2 = DYNAMIC-FUNCTION("NewToolBar"
          ,tbValuta:HANDLE
          ,"Fil"
          ,"COPY;Kopier fra profil 1,undo;Angre,delete;Slett,save;Lagre,excel;Eksporter til E&xcel"
          ,"").

  DYNAMIC-FUNCTION("setAttribute",hFieldMap2,"postUpdateProc","kasvaluta_post_update.p").

  DYNAMIC-FUNCTION("CreateObjectLink",hBrowse2,hToolbar2).
  DYNAMIC-FUNCTION("CreateObjectLink",hFieldMap2,hToolbar2).

/*   RUN InvokeMethod(hBrowse,"OpenQuery"). */
  APPLY "value-changed" TO rsAktiv.

  APPLY "value-changed" TO rsAktiv-2.
  APPLY "VALUE-CHANGED" TO cmbPrisProfil.

  DYNAMIC-FUNCTION("InitTranslation",THIS-PROCEDURE:CURRENT-WINDOW).

  DYNAMIC-FUNCTION("setSplitBarX",THIS-PROCEDURE:CURRENT-WINDOW,btnSplitBarX:HANDLE IN FRAME frmSplitBarX,NO).

  DYNAMIC-FUNCTION("setFollowSplitBarX",THIS-PROCEDURE:CURRENT-WINDOW,btnSplitBarX:HANDLE IN FRAME frmSplitBarX, 
                    STRING(brwKasValutaProf1:HANDLE) + "," + STRING(hBrowse) 
            + "," + STRING(brwKasValuta:HANDLE) + "," + STRING(hBrowse2)
            + "," + STRING(indeks-2:HANDLE) + "," + STRING(ValDatum-2:HANDLE) + "," + STRING(ValKod-2:HANDLE) + "," + STRING(ValKurs-2:HANDLE) + "," + STRING(ValLand-2:HANDLE) + "," + STRING(ValNavn-2:HANDLE)
            + "," + STRING(fiFortjeneste:HANDLE)
            + "," + STRING(KasseValKurs-2:HANDLE) + "," + STRING(Retur-2:HANDLE) +  "," + STRING(ValNr-2:HANDLE) + "," + STRING(rsAktiv-2:HANDLE) + "," + STRING(ValAktiv-2:HANDLE)
            + "," + DYNAMIC-FUNCTION("getToolbarHandles",hToolbar2,"button")
            + "," + STRING(cmbPrisProfil:HANDLE)
                    ).

  DYNAMIC-FUNCTION("setNoResizeX", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, "brwKasValutaProf1," + hBrowse:NAME).

  DYNAMIC-FUNCTION("setOrgWinSize",THIS-PROCEDURE:CURRENT-WINDOW,680,200,0,550).
  
END.

THIS-PROCEDURE:CURRENT-WINDOW:HIDDEN = FALSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MoveToTop C-Win 
PROCEDURE MoveToTop :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
THIS-PROCEDURE:CURRENT-WINDOW:HIDDEN = NO.
THIS-PROCEDURE:CURRENT-WINDOW:WINDOW-STATE = 3.
THIS-PROCEDURE:CURRENT-WINDOW:MOVE-TO-TOP().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NewRecord C-Win 
PROCEDURE NewRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
RUN SUPER.
IF DYNAMIC-FUNCTION("getCurrentObject") = hToolbar THEN 
DO WITH FRAME {&FRAME-NAME}:
    fiFortjenesteProf1:SENSITIVE = FALSE.
    APPLY "ENTRY" TO ValKod.  

END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SaveRecord C-Win 
PROCEDURE SaveRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF DYNAMIC-FUNCTION("getCurrentObject") = hToolbar2 THEN
  DYNAMIC-FUNCTION("setAttribute",hFieldMap2,"bufferExtraValues",cmbPrisProfil:SCREEN-VALUE IN FRAME {&FRAME-NAME}).

RUN SUPER.

DO WITH FRAME {&FRAME-NAME}:
    fiFortjenesteProf1:SENSITIVE = TRUE.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION AdjustBrowseColumns C-Win 
FUNCTION AdjustBrowseColumns RETURNS LOGICAL
  ( INPUT ihBrowse     AS HANDLE,
    INPUT icBrowseName AS CHAR):
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
    
            + ";ValNr"
            + ";ValAktiv|Aktiv|J/N"
            + ";ValKod|Valuta"
            + ";ValNavn"
            + ";!ProfilNr|Profil"
            + ";ValKurs"
            + ";Indeks"
            + ";KasseValKurs"
            + ";ValDatum"
            + ";EDato"
            + ";BrukerID"
            + ";ValLand"
    
------------------------------------------------------------------------------*/
  
CASE icBrowseName:
    WHEN 'brwKasValutaProf1' THEN
        ASSIGN
          ihBrowse:GET-BROWSE-COLUMN(1):WIDTH-PIXELS  = 40
          ihBrowse:GET-BROWSE-COLUMN(2):WIDTH-PIXELS  = 80
          ihBrowse:GET-BROWSE-COLUMN(3):WIDTH-PIXELS  = 50
          ihBrowse:GET-BROWSE-COLUMN(4):WIDTH-PIXELS  = 20
          ihBrowse:GET-BROWSE-COLUMN(5):WIDTH-PIXELS  = 60
          ihBrowse:GET-BROWSE-COLUMN(6):WIDTH-PIXELS  = 60
          ihBrowse:GET-BROWSE-COLUMN(7):WIDTH-PIXELS  = 20
          ihBrowse:GET-BROWSE-COLUMN(8):WIDTH-PIXELS  = 20
          .
    WHEN 'brwKasValuta' THEN
        ASSIGN
          ihBrowse:GET-BROWSE-COLUMN(1):WIDTH-PIXELS  = 40
          ihBrowse:GET-BROWSE-COLUMN(2):WIDTH-PIXELS  = 40
          ihBrowse:GET-BROWSE-COLUMN(3):WIDTH-PIXELS  = 80
          ihBrowse:GET-BROWSE-COLUMN(4):WIDTH-PIXELS  = 50
          ihBrowse:GET-BROWSE-COLUMN(5):WIDTH-PIXELS  = 20
          ihBrowse:GET-BROWSE-COLUMN(6):WIDTH-PIXELS  = 60
          ihBrowse:GET-BROWSE-COLUMN(7):WIDTH-PIXELS  = 60
          ihBrowse:GET-BROWSE-COLUMN(8):WIDTH-PIXELS  = 20
          ihBrowse:GET-BROWSE-COLUMN(9):WIDTH-PIXELS  = 20
          .
END CASE.


  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION SaveFortjeneste C-Win 
FUNCTION SaveFortjeneste RETURNS LOGICAL
  ( INPUT icProfilnr        AS CHAR,
    INPUT icFortjeneste     AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
IF NOT DYNAMIC-FUNCTION("runProc","kasvaluta_lagre_forjeneste.p",icProfilnr + "|" + icFortjeneste,?) THEN 
  DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").
ELSE RETURN YES.

RETURN NO.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

