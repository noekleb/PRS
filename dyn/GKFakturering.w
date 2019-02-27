&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
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

/* Local Variable Definitions ---                                       */

DEF VAR bOK           AS LOG    NO-UNDO.
DEF VAR ix            AS INT    NO-UNDO.

DEF VAR hBrowseGK       AS HANDLE NO-UNDO.
DEF VAR hBrowseTL       AS HANDLE NO-UNDO.
DEF VAR hToolbar      AS HANDLE NO-UNDO.
DEF VAR hParent       AS HANDLE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rectGK rectToolbar rectTL cmbButikk rsType 
&Scoped-Define DISPLAYED-OBJECTS cmbButikk rsType 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE cmbButikk AS CHARACTER FORMAT "X(256)":U 
     LABEL "Butikk" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 32.6 BY 1 NO-UNDO.

DEFINE VARIABLE rsType AS CHARACTER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Gavekort", "Gavekort",
"Tilgode", "Tilgode"
     SIZE 32 BY .95 NO-UNDO.

DEFINE RECTANGLE rectGK
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 150 BY 27.48.

DEFINE RECTANGLE rectTL
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 150 BY 27.48.

DEFINE RECTANGLE rectToolbar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 15 BY .95.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     cmbButikk AT ROW 1.48 COL 9 COLON-ALIGNED
     rsType AT ROW 1.48 COL 45 NO-LABEL
     rectGK AT ROW 2.81 COL 2
     rectToolbar AT ROW 1.48 COL 128.6
     rectTL AT ROW 2.81 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 152 BY 29.43.


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
         TITLE              = "Fakturer"
         HEIGHT             = 29.43
         WIDTH              = 152
         MAX-HEIGHT         = 50
         MAX-WIDTH          = 320
         VIRTUAL-HEIGHT     = 50
         VIRTUAL-WIDTH      = 320
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
  NOT-VISIBLE,                                                          */
/* SETTINGS FOR FRAME DEFAULT-FRAME
                                                                        */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Fakturer */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Fakturer */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-RESIZED OF C-Win /* Fakturer */
DO:
  DYNAMIC-FUNCTION("setWidgetResize",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE:CURRENT-WINDOW,"Resize","").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cmbButikk
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cmbButikk C-Win
ON VALUE-CHANGED OF cmbButikk IN FRAME DEFAULT-FRAME /* Butikk */
DO:
  RUN OpenQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rsType
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rsType C-Win
ON VALUE-CHANGED OF rsType IN FRAME DEFAULT-FRAME
DO:
  ASSIGN rsType.

  IF rsType = "Tilgode" THEN DO:
    DYNAMIC-FUNCTION("DeleteObjectLink",hBrowseGK,hToolbar).
    DYNAMIC-FUNCTION("CreateObjectLink",hBrowseTL,hToolbar).
    DYNAMIC-FUNCTION("setCurrentObject",hBrowseTL).
    RUN OpenQuery.
    hBrowseTL:HIDDEN = FALSE.
    hBrowseGK:HIDDEN = TRUE.
  END.
  ELSE DO:
    DYNAMIC-FUNCTION("DeleteObjectLink",hBrowseTL,hToolbar).
    DYNAMIC-FUNCTION("CreateObjectLink",hBrowseGK,hToolbar).
    DYNAMIC-FUNCTION("setCurrentObject",hBrowseGK).
    RUN OpenQuery.
    hBrowseTL:HIDDEN = TRUE.
    hBrowseGK:HIDDEN = FALSE.
  END.
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
  IF NOT DYNAMIC-FUNCTION("DoCleanUpObjects",THIS-PROCEDURE:CURRENT-WINDOW) THEN
    RETURN NO-APPLY.
  PUBLISH "InvalidateHandle" (THIS-PROCEDURE).
  DYNAMIC-FUNCTION("setCleanUpResize",THIS-PROCEDURE:CURRENT-WINDOW).
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
  hParent = SOURCE-PROCEDURE.
  RUN InitWindow.
  THIS-PROCEDURE:CURRENT-WINDOW:HIDDEN = FALSE.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

{incl/wintrigg.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DefaultActionBrowse C-Win 
PROCEDURE DefaultActionBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
RUN setSearch IN hParent (STRING(hBrowseGK:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("KundeNr"):BUFFER-VALUE)
                          ,YES
                          ,"").
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
  DISPLAY cmbButikk rsType 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE rectGK rectToolbar rectTL cmbButikk rsType 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FakturerRecord C-Win 
PROCEDURE FakturerRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:

  IF rsType = "Gavekort" THEN
  DO:
      IF NOT DYNAMIC-FUNCTION("RunProc","gavekort_fakturer.p",
                               cmbButikk:SCREEN-VALUE + ",gavekort",
                               hBrowseGK:QUERY:GET-BUFFER-HANDLE:TABLE-HANDLE) THEN
        DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Feil i fakturering av gavekort",""). 
  END.
  ELSE DO:
      IF NOT DYNAMIC-FUNCTION("RunProc","gavekort_fakturer.p",
                               cmbButikk:SCREEN-VALUE + ",tilgode",
                               hBrowseTL:QUERY:GET-BUFFER-HANDLE:TABLE-HANDLE) THEN
        DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Feil i fakturering av tilgodelapper",""). 
  END.

END.

RUN OpenQuery.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FakturerValgteRecord C-Win 
PROCEDURE FakturerValgteRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cRowIdList AS CHAR NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
  IF rsType = "Gavekort" THEN DO:
    DO ix = 1 TO hBrowseGK:NUM-SELECTED-ROWS:
      IF hBrowseGK:FETCH-SELECTED-ROW(ix) THEN 
        cRowIdList = cRowIdList + hBrowseGK:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("Rowident1"):BUFFER-VALUE + ",". 
    END.
    IF NOT DYNAMIC-FUNCTION("RunProc","gavekort_fakturer.p",
                             cmbButikk:SCREEN-VALUE + ",gavekort," + TRIM(cRowIdList,","),
                             ?) THEN
      DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Feil i fakturering av gavekort",""). 
  END.
  ELSE DO:
    DO ix = 1 TO hBrowseTL:NUM-SELECTED-ROWS:
      IF hBrowseTL:FETCH-SELECTED-ROW(ix) THEN 
        cRowIdList = cRowIdList + hBrowseTL:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("Rowident1"):BUFFER-VALUE + ",". 
    END.
    IF NOT DYNAMIC-FUNCTION("RunProc","gavekort_fakturer.p",
                             cmbButikk:SCREEN-VALUE + ",tilgode," + TRIM(cRowIdList,","),
                             ?) THEN
      DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Feil i fakturering av tilgodelapper",""). 
  END.
END.
RUN OpenQuery.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitWindow C-Win 
PROCEDURE InitWindow :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
  ASSIGN cmbButikk:DELIMITER = "|"
         cmbButikk:LIST-ITEM-PAIRS = "|0|" + DYNAMIC-FUNCTION("getFieldList","Butiker;butik|butnamn;butik","where true")
         cmbButikk:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldValues","Bruker","WHERE BrukerId = '" + DYNAMIC-FUNCTION("getASuserId") + "'","ButikkNr")
         . 

  hBrowseGK = DYNAMIC-FUNCTION("NewBrowse",
                          rectGK:HANDLE,        
                          10000,                      
                          "MULTIPLE",                
                          "Gavekort"
                            + ";butnr|Utstedt butikk"
                            + ";BruktButNr|Brukt butikk"
                            + ";IdentType|Type|>>>9"
                            + ";IdentNr"
                            + ";Belop"
                            + ";RabKr|Rabatt"
                            + ";Dato|Reg.dato|99/99/99"
                            + ";+cRegTid|CHARACTER|x(5)|int_to_hhmm_time.p(Tid)|Tid"
                            + ";Gyldigdato|Gyld.til|99/99/99"
                            + ";Bruktdato|Brukt|99/99/99;+cBruktTid|CHARACTER|x(5)|int_to_hhmm_time.p(BruktTid)|Tid"
                            + ";KasseNr"
                            + ";BongNr"
                            + ";Selgernr"
                            + ";kassnr"
                         ,"WHERE false"
                         ,"").         
  hBrowseGK:NAME = "brwGavekort".

  DYNAMIC-FUNCTION("NewMenuBand",
                    hBrowseGK,  /* parent widget */
                    "Deselect;Fjern markering av rad"
                    + ",MultiSortBrowse;Sorter på flere kolonner" 
                    + ",FakturerValgte;Fakturer valgte"
                    ,"").


  hBrowseTL = DYNAMIC-FUNCTION("NewBrowse",
                          rectTL:HANDLE,        
                          10000,                      
                          "MULTIPLE",                
                          "Tilgode"
                            + ";butnr"
                            + ";IdentType|Type|>>>9"
                            + ";IdentNr"
                            + ";Belop"
                            + ";RabKr|Rabatt"
                            + ";Dato|Reg.dato|99/99/99"
                            + ";+cRegTid|CHARACTER|x(5)|int_to_hhmm_time.p(Tid)|Tid"
                            + ";Gyldigdato|Gyld.til|99/99/99"
                            + ";Bruktdato|Brukt|99/99/99;+cBruktTid|CHARACTER|x(5)|int_to_hhmm_time.p(BruktTid)|Tid"
                            + ";KasseNr"
                            + ";BongNr"
                            + ";Selgernr"
                            + ";kassnr"
                         ,"WHERE false"
                         ,"").         
  hBrowseTL:NAME = "brwTilgode".


  DYNAMIC-FUNCTION("NewMenuBand",
                    hBrowseTL,  /* parent widget */
                    "Deselect;Fjern markering av rad"
                    + ",MultiSortBrowse;Sorter på flere kolonner" 
                    + ",FakturerValgte;Fakturer valgte"
                    ,"").

  hToolbar = DYNAMIC-FUNCTION("NewToolBar",
                    rectToolBar:HANDLE,             
                    "Fil",
                    "excel"
                  + ",Fakturer;Fakturer alle"
                    ,"").
  DYNAMIC-FUNCTION("setAttribute",hToolbar,"enabledevents","StartQuery").
  DYNAMIC-FUNCTION("createObjectLink",hBrowseGK,hToolbar).

  APPLY "value-changed" TO rsType.

  DYNAMIC-FUNCTION("InitTranslation",THIS-PROCEDURE:CURRENT-WINDOW).
  DYNAMIC-FUNCTION("setOrgWinSize",THIS-PROCEDURE:CURRENT-WINDOW,450,500,500,550).
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
THIS-PROCEDURE:CURRENT-WINDOW:MOVE-TO-TOP().
THIS-PROCEDURE:CURRENT-WINDOW:WINDOW-STATE = 3.
APPLY "entry" TO cmbButikk IN FRAME {&FRAME-NAME}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpenQuery C-Win 
PROCEDURE OpenQuery :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
  ASSIGN rsType.

  IF rsType = "Gavekort" THEN DO:
    DYNAMIC-FUNCTION("setAttribute",hBrowseGK,"querywhere","WHERE Faktura_id = 0"
                   + " AND BruktButNr = " + cmbButikk:SCREEN-VALUE
                   + " AND ButNr <> 0" 
                   + " AND ButNr NE BruktButNr"
                   /*+ " AND NOT Utgatt"*/
                     ).
    DYNAMIC-FUNCTION("setCurrentObject",hBrowseGK).
  END.
  ELSE DO:
    DYNAMIC-FUNCTION("setAttribute",hBrowseTL,"querywhere","WHERE Faktura_id = 0"
                   + " AND BruktButNr = " + cmbButikk:SCREEN-VALUE
                   + " AND ButNr <> 0"
                   + " AND ButNr NE BruktButNr"
                   /*+ " AND NOT Utgatt"*/
                     ).
    DYNAMIC-FUNCTION("setCurrentObject",hBrowseTL).
  END.
  RUN SUPER.

  IF rsType = "Gavekort" THEN 
    ASSIGN hBrowseGK:HIDDEN = FALSE
           hBrowseTL:HIDDEN = TRUE.
  ELSE
    ASSIGN hBrowseGK:HIDDEN = TRUE
           hBrowseTL:HIDDEN = FALSE.

END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

