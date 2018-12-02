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

DEF VAR hBrowse       AS HANDLE NO-UNDO.
DEF VAR hQuery        AS HANDLE NO-UNDO.
DEF VAR hBuffer       AS HANDLE NO-UNDO.
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
&Scoped-Define ENABLED-OBJECTS rectFaktHode rectToolbar FraDato TilDato ~
cmbButikk tbIkkeFakt 
&Scoped-Define DISPLAYED-OBJECTS FraDato TilDato cmbButikk tbIkkeFakt 

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

DEFINE VARIABLE FraDato AS DATE FORMAT "99/99/9999":U 
     LABEL "Fra salgsdato" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE TilDato AS DATE FORMAT "99/99/9999":U 
     LABEL "til" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE RECTANGLE rectFaktHode
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 150 BY 27.48.

DEFINE RECTANGLE rectToolbar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 15 BY .95.

DEFINE VARIABLE tbIkkeFakt AS LOGICAL INITIAL yes 
     LABEL "Kun ikke-fakturerte" 
     VIEW-AS TOGGLE-BOX
     SIZE 22.6 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     FraDato AT ROW 1.48 COL 16 COLON-ALIGNED
     TilDato AT ROW 1.48 COL 36 COLON-ALIGNED
     cmbButikk AT ROW 1.48 COL 87.4 COLON-ALIGNED
     tbIkkeFakt AT ROW 1.62 COL 55.4
     rectFaktHode AT ROW 2.81 COL 2
     rectToolbar AT ROW 1.48 COL 134.2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 152 BY 29.43.


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
         TITLE              = "Purreliste"
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
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME DEFAULT-FRAME
                                                                        */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Purreliste */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Purreliste */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-RESIZED OF C-Win /* Purreliste */
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


&Scoped-define SELF-NAME tbIkkeFakt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tbIkkeFakt C-Win
ON VALUE-CHANGED OF tbIkkeFakt IN FRAME DEFAULT-FRAME /* Kun ikke-fakturerte */
DO:
  RUN OpenQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TilDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TilDato C-Win
ON RETURN OF TilDato IN FRAME DEFAULT-FRAME /* til */
DO:
  RUN OpenQuery.
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
RUN setSearch IN hParent (STRING(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("KundeNr"):BUFFER-VALUE)
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
  DISPLAY FraDato TilDato cmbButikk tbIkkeFakt 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE rectFaktHode rectToolbar FraDato TilDato cmbButikk tbIkkeFakt 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
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
  TilDato = TODAY.
  DISP TilDato.

  ASSIGN cmbButikk:DELIMITER = "|"
         cmbButikk:LIST-ITEM-PAIRS = "|0|" + DYNAMIC-FUNCTION("getFieldList","Butiker;butik|butnamn;butik","where true")
         cmbButikk:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldValues","Bruker","WHERE BrukerId = '" + DYNAMIC-FUNCTION("getASuserId") + "'","ButikkNr")
         . 

  hBrowse = DYNAMIC-FUNCTION("NewBrowse",
                          rectFaktHode:HANDLE,       /* Coordinates */
                          10000,                     /* Batchsize */
                          "MULTIPLE",                /* Attributes that must be set at creation time for the browse */
                          "FakturaHode"              /* Buffers and fields (and calculated fields) for browse */
                           + ";Faktura_id"
                           + ";Dato|Salgsdato"
                           + ";Butikksalg|BS|*/"
                           + ";ButikkNr"
                           + ";FakturaNr"
                           + ";FakturertDato"
                           + ";ForfallsDato" 
                           + ";Totalt|Sum ink.mva"
                           + ";AvgFriSalg|Avgfri"
                           + ";AvgPlSalg|Avgpl"
                           + ";Nettopris|Netto"
                           + ";AvrundingKr"
                           + ";TotalRabattKr|Rabatt"
                           + ";TotalRabatt%|Rabatt%"
                           + ";MvaKr"
                           + ";VaarRef"
                           + ";DeresRef"
                           + ";Referanse"
                           + ";LeveringsDato"
                           + ";Telefon"
                           + ";FaktAdresse1"
                           + ";FaktAdresse2"
                           + ";FaktLand"
                           + ";FaktPostNr"
                           + ";FaktPoststed"
                           + ";Kundenr"
                        + ",Kunde"
                          + ";Navn"
                         ,"WHERE false"
                        + ",FIRST Kunde OF FakturaHode NO-LOCK"
                          ,"sort|FakturertDato desc").         
  hBrowse:NAME = "brwFaktHode".
  hQuery = hBrowse:QUERY.
  hBuffer = hQuery:GET-BUFFER-HANDLE(1).

  hBrowse:MOVE-COLUMN(26,1).
  hBrowse:MOVE-COLUMN(27,2).


  hToolbar = DYNAMIC-FUNCTION("NewToolBar",
                    rectToolBar:HANDLE,             
                    "Fil",                          
                    "Reject;&Slett;Fjern markerte poster fra kjøring;RejectRecord;bmp/rejec16e.bmp"
                  + ",excel,print"
                  + ",StartQuery;&StartSpørring;Start spørring;OpenQuery;bmp/searc16e.bmp"
                    ,"").
  DYNAMIC-FUNCTION("setAttribute",hToolbar,"enabledevents","StartQuery").
  DYNAMIC-FUNCTION("createObjectLink",hBrowse,hToolbar).

  RUN OpenQuery.

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
APPLY "entry" TO FraDato IN FRAME {&FRAME-NAME}.
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
  ASSIGN FraDato TilDato tbIkkeFakt.

  DYNAMIC-FUNCTION("setAttribute",hBrowse,"querywhere","WHERE BilagsType = 10" /* Purrefaktura */
                 + (IF FraDato NE ? THEN
                     " AND Dato GE DATE('" + STRING(FraDato) + "')"
                    ELSE "") 
                 + (IF TilDato NE ? THEN
                     " AND Dato LE DATE('" + STRING(TilDato) + "')"
                    ELSE "")
                 + (IF tbIkkeFakt THEN
                     " AND FakturaNr = ?"
                    ELSE "")
                 + (IF cmbButikk:SCREEN-VALUE NE "0" AND cmbButikk:SCREEN-VALUE NE ? THEN
                     " AND ButikkNr = " + cmbButikk:SCREEN-VALUE
                    ELSE "")
                   ).
END.

DYNAMIC-FUNCTION("setCurrentObject",hBrowse).

RUN SUPER.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PrintRecord C-Win 
PROCEDURE PrintRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cIdList  AS CHAR NO-UNDO.
DEF VAR cPrinter AS CHAR NO-UNDO.
DEF VAR iAntEks  AS INT  NO-UNDO.
DEF VAR iFormat  AS INT  NO-UNDO INIT 1.

hQuery:GET-FIRST().
REPEAT WHILE NOT hQuery:QUERY-OFF-END:
  cIdList = cIdList + STRING(hBuffer:BUFFER-FIELD("Faktura_id"):BUFFER-VALUE) + ",".
  hQuery:GET-NEXT().
END.

IF cIdList NE "" THEN DO:
  RUN DSelectPrinter.w (INPUT-OUTPUT cPrinter,INPUT-OUTPUT iAntEks,INPUT-OUTPUT iFormat,"FakturaSkriver,FakturaKopi",OUTPUT bOk).
  IF bOk THEN DO:
    cIdList = TRIM(cIdList,",").
    IF NOT DYNAMIC-FUNCTION("runproc","faktura_produksjon.p","idlist|" + cIdList,?) THEN 
      DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").
    ELSE DO:
/*     IF NUM-ENTRIES(cIdList) > 1 THEN                                              */
/*       RUN skrivfaktura.p ("|WHERE CAN-DO('" + cIdList + "',STRING(Faktura_id))",  */
/*                           FALSE,"setup").                                         */
/*     ELSE IF cIdList NE "" THEN                                                    */
      RUN skrivfaktura.p (cIdList + "|",TRUE,cPrinter,iAntEks,"",iFormat). 
    END.
  END.
END.

RUN OpenQuery.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RejectRecord C-Win 
PROCEDURE RejectRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF hBrowse:NUM-SELECTED-ROWS > 0 THEN 
  hBrowse:DELETE-SELECTED-ROWS().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

