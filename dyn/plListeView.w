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

DEF VAR bOk             AS LOG    NO-UNDO.
DEF VAR ix              AS INT    NO-UNDO.
DEF VAR hBrowse         AS HANDLE NO-UNDO.
DEF VAR hQuery          AS HANDLE NO-UNDO.
DEF VAR hToolbar        AS HANDLE NO-UNDO.
DEF VAR hFieldMap       AS HANDLE NO-UNDO.
DEF VAR cBrukerButikk   AS CHAR   NO-UNDO.
DEF VAR hParentBrowse    AS HANDLE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS tbPlListeHode PlNavn plListeStatus LevNr ~
btnLevNr LevNamn FraButikkNr btnFraButikkNr ButNamn PlMerknad ~
RegistrertDato RegistrertAv EDato BrukerId fiMerknadLabel 
&Scoped-Define DISPLAYED-OBJECTS PlNavn plListeStatus LevNr LevNamn ~
FraButikkNr ButNamn PlMerknad RegistrertDato RegistrertAv EDato BrukerId ~
fiMerknadLabel 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setParentQuery C-Win 
FUNCTION setParentQuery RETURNS LOGICAL
  ( INPUT ihParentBrowse AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnFraButikkNr 
     LABEL "..." 
     SIZE 4 BY 1.

DEFINE BUTTON btnLevNr 
     LABEL "..." 
     SIZE 4 BY 1.

DEFINE VARIABLE plListeStatus AS CHARACTER FORMAT "X(256)" INITIAL "0" 
     LABEL "Ordrestatus" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEM-PAIRS "Item 1","0"
     DROP-DOWN-LIST
     SIZE 45 BY 1 TOOLTIP "Behandlingsstatus".

DEFINE VARIABLE PlMerknad AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 45 BY 3.57 TOOLTIP "Kort merknad til plukklisten".

DEFINE VARIABLE BrukerId AS CHARACTER FORMAT "X(15)" 
     LABEL "av" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE ButNamn AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE EDato AS DATE FORMAT "99/99/99" 
     LABEL "Endret dato" 
     VIEW-AS FILL-IN 
     SIZE 13.2 BY 1.

DEFINE VARIABLE fiMerknadLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Merknad:" 
      VIEW-AS TEXT 
     SIZE 9 BY .62 NO-UNDO.

DEFINE VARIABLE FraButikkNr AS INTEGER FORMAT ">>>>>9" INITIAL 0 
     LABEL "Butikk" 
     VIEW-AS FILL-IN 
     SIZE 10.4 BY 1 TOOLTIP "Plukkes fra butikk".

DEFINE VARIABLE LevNamn AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE LevNr AS INTEGER FORMAT ">>>>>9" INITIAL 0 
     LABEL "Lev.nr" 
     VIEW-AS FILL-IN 
     SIZE 10.4 BY 1 TOOLTIP "Leverandørnummer".

DEFINE VARIABLE PlNavn AS CHARACTER FORMAT "X(30)" 
     LABEL "Beskrivelse" 
     VIEW-AS FILL-IN 
     SIZE 45 BY 1 TOOLTIP "Navn på plukkliste".

DEFINE VARIABLE RegistrertAv AS CHARACTER FORMAT "X(15)" 
     LABEL "av" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE RegistrertDato AS DATE FORMAT "99/99/99" 
     LABEL "Opprettet" 
     VIEW-AS FILL-IN 
     SIZE 13.2 BY 1.

DEFINE RECTANGLE tbPlListeHode
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 21 BY 1.19.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     PlNavn AT ROW 3.62 COL 24 COLON-ALIGNED
     plListeStatus AT ROW 4.71 COL 24 COLON-ALIGNED
     LevNr AT ROW 5.86 COL 24 COLON-ALIGNED
     btnLevNr AT ROW 5.86 COL 36.4 NO-TAB-STOP 
     LevNamn AT ROW 5.91 COL 38.8 COLON-ALIGNED NO-LABEL
     FraButikkNr AT ROW 6.91 COL 24 COLON-ALIGNED
     btnFraButikkNr AT ROW 6.91 COL 36.4 NO-TAB-STOP 
     ButNamn AT ROW 6.95 COL 38.8 COLON-ALIGNED NO-LABEL
     PlMerknad AT ROW 8.38 COL 26 NO-LABEL
     RegistrertDato AT ROW 12.91 COL 24 COLON-ALIGNED
     RegistrertAv AT ROW 12.91 COL 42 COLON-ALIGNED
     EDato AT ROW 14.1 COL 24 COLON-ALIGNED
     BrukerId AT ROW 14.1 COL 42 COLON-ALIGNED
     fiMerknadLabel AT ROW 8.38 COL 14.6 COLON-ALIGNED NO-LABEL
     tbPlListeHode AT ROW 1.24 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 79.6 BY 15.05.


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
         TITLE              = "Ordreforslag"
         HEIGHT             = 14.91
         WIDTH              = 80.6
         MAX-HEIGHT         = 28.1
         MAX-WIDTH          = 147
         VIRTUAL-HEIGHT     = 28.1
         VIRTUAL-WIDTH      = 147
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
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Ordreforslag */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Ordreforslag */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnFraButikkNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnFraButikkNr C-Win
ON CHOOSE OF btnFraButikkNr IN FRAME DEFAULT-FRAME /* ... */
DO:
  DEF VAR cReturnValues   AS CHAR NO-UNDO.
  DEF VAR bOk             AS LOG  NO-UNDO.

  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxLookup.w (THIS-PROCEDURE,200,
                    "Butiker"
                    + ";ButNamn"
                    + ";Butik"
                   ,"WHERE true"
                    ,""                                                  
                    ,"Butik,ButNamn",   /* <- return values for these fields */
                    OUTPUT cReturnValues,
                    OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF bOk AND cReturnValues NE "" THEN DO:
    ASSIGN FraButikkNr:SCREEN-VALUE = ENTRY(1,cReturnValues,"|")
           ButNamn:SCREEN-VALUE = ENTRY(1,cReturnValues,"|")
           .

    APPLY "any-printable" TO FraButikkNr.
  END.
  THIS-PROCEDURE:CURRENT-WINDOW:MOVE-TO-TOP().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnLevNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnLevNr C-Win
ON CHOOSE OF btnLevNr IN FRAME DEFAULT-FRAME /* ... */
DO:
  DEF VAR cReturnValues   AS CHAR NO-UNDO.
  DEF VAR bOk             AS LOG  NO-UNDO.

  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxLookup.w (THIS-PROCEDURE,200,
                    "LevBas"
                    + ";LevNamn"
                    + ";LevNr"
                   ,"WHERE true"
                    ,""                                                  
                    ,"LevNr,LevNamn",   /* <- return values for these fields */
                    OUTPUT cReturnValues,
                    OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF bOk AND cReturnValues NE "" THEN DO:
    ASSIGN LevNr:SCREEN-VALUE   = ENTRY(1,cReturnValues,"|")
           LevNamn:SCREEN-VALUE = ENTRY(2,cReturnValues,"|")
           .

    APPLY "any-printable" TO LevNr.
  END.
  THIS-PROCEDURE:CURRENT-WINDOW:MOVE-TO-TOP().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FraButikkNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FraButikkNr C-Win
ON F3 OF FraButikkNr IN FRAME DEFAULT-FRAME /* Butikk */
DO:
  APPLY "choose" TO btnFraButikkNr.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME LevNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL LevNr C-Win
ON F3 OF LevNr IN FRAME DEFAULT-FRAME /* Lev.nr */
DO:
  APPLY "choose" TO btnLevNr.
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
IF hFieldMap:AVAIL AND hFieldMap:BUFFER-FIELD("OverfortDato"):BUFFER-VALUE NE ? THEN
  DYNAMIC-FUNCTION("setAttribute",hToolbar,"disabledEvents","new,save").
ELSE
  DYNAMIC-FUNCTION("setAttribute",hToolbar,"disabledEvents","").

RUN SUPER.

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
  DISPLAY PlNavn plListeStatus LevNr LevNamn FraButikkNr ButNamn PlMerknad 
          RegistrertDato RegistrertAv EDato BrukerId fiMerknadLabel 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE tbPlListeHode PlNavn plListeStatus LevNr btnLevNr LevNamn FraButikkNr 
         btnFraButikkNr ButNamn PlMerknad RegistrertDato RegistrertAv EDato 
         BrukerId fiMerknadLabel 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
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
RUN enable_UI.

/* DYNAMIC-FUNCTION("CreateStatusBar",THIS-PROCEDURE:CURRENT-WINDOW,"",0,YES,?).  */

DO WITH FRAME {&FRAME-NAME}:

  ASSIGN 
    cBrukerButikk = DYNAMIC-FUNCTION("getFieldValues","Bruker",
                                     "WHERE Bruker.BrukerId  = '" + DYNAMIC-FUNCTION("getASuserId") + "'","ButikkNr")
    plListeStatus:DELIMITER = "|"
    plListeStatus:LIST-ITEM-PAIRS = RIGHT-TRIM("|0|" + DYNAMIC-FUNCTION("getFieldList","sysPara;ParaNr|Parameter1;ParaNr","where sysHId = 5 and sysGr = 3 and ParaNr <= 2"),"|") 
    .

  hQuery = DYNAMIC-FUNCTION("NewQuery"
        ,100
        ,""
       ,"PlListeHode"
      + ",Butiker"
       + ";ButNamn"
      + ",LevBas"
       + ";LevNamn"
       ,"WHERE FALSE "
       + ",FIRST Butiker OUTER-JOIN WHERE Butiker.butik = FraButikkNr NO-LOCK "
       + ",FIRST LevBas  OUTER-JOIN WHERE LevBas.LevNr = LevNr NO-LOCK "
        ,"").

  hFieldMap = DYNAMIC-FUNCTION("NewFieldMap"
      ,hQuery
      ,FRAME {&FRAME-NAME}:HANDLE
      ,"PlNavn,plListeStatus,LevNr,FraButikkNr,PlMerknad",""
      ,"ButNamn,LevNamn,RegistrertDato,RegistrertAv,BrukerId,EDato",""
      ,"btnFraButikkNr,btnLevNr").

  DYNAMIC-FUNCTION("setAttribute",hFieldMap,"customCreateProc","pllistehode_create.p").
  DYNAMIC-FUNCTION("setAttribute",hFieldMap,"customUpdateValProc","+pllistehode_update.p").

  DYNAMIC-FUNCTION("CreateObjectLink",hQuery,hFieldMap).

  hToolbar = DYNAMIC-FUNCTION("NewToolBar"
    ,tbPlListeHode:HANDLE
    ,"Fil"
    ,"new;Ny,undo;Angre,save;Lagre"
   + ",rule,first|Naviger;Første,prev|Naviger;Forrige,next|Naviger;Neste,last|Naviger;Siste"
    ,"maxborder").

  DYNAMIC-FUNCTION("CreateObjectLink",hToolbar,hQuery).
  DYNAMIC-FUNCTION("CreateObjectLink",hToolbar,hFieldMap).

/*   RUN InvokeMethod(hQuery,"OpenQuery").  */
END.


DYNAMIC-FUNCTION("setOrgWinSize",THIS-PROCEDURE:CURRENT-WINDOW,200,200,0,0).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LeaveOfFieldHandle C-Win 
PROCEDURE LeaveOfFieldHandle :
/*------------------------------------------------------------------------------
  Purpose:     Retrieve lookup values when a foreign key field is modified
  Parameters:  Handle to foreign key field
  Notes:       The cReturn variable should be replaced with a fill-in
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihField AS HANDLE NO-UNDO.

DEF VAR cReturn AS CHAR NO-UNDO.

IF ihField:MODIFIED THEN DO WITH FRAME {&FRAME-NAME}:
  CASE ihField:NAME:
    WHEN "LevNr"       THEN LevNamn:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldValues","LevBas","WHERE LevNr = '" + ihField:SCREEN-VALUE + "'","LevNamn").
    WHEN "FraButikkNr" THEN ButNamn:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldValues","Butiker","WHERE Butik = '" + ihField:SCREEN-VALUE + "'","ButNamn").
  END CASE.
END.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NewListe C-Win 
PROCEDURE NewListe :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
RUN InvokeMethod(hToolbar,"NewRecord").

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
ASSIGN FraButikkNr:SCREEN-VALUE IN FRAME {&FRAME-NAME} = cBrukerButikk
       plListeStatus:SCREEN-VALUE = plListeStatus:ENTRY(2)
       .

RUN LeaveOfFieldHandle (FraButikkNr:HANDLE).

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
RUN SUPER.
DYNAMIC-FUNCTION("RefreshRowids",hParentBrowse,hParentBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("RowIdent1"):BUFFER-VALUE).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE UndoRecord C-Win 
PROCEDURE UndoRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
RUN SUPER.
IF VALID-HANDLE(hParentBrowse) AND hParentBrowse:QUERY:GET-BUFFER-HANDLE(1):AVAIL THEN
  RUN InvokeMethod(hParentBrowse,"DisplayRecord").
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setParentQuery C-Win 
FUNCTION setParentQuery RETURNS LOGICAL
  ( INPUT ihParentBrowse AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
hParentBrowse = ihParentBrowse.
DYNAMIC-FUNCTION("CreateOneToOneLink",hQuery,hParentBrowse,"PlListeId").

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

