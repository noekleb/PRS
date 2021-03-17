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

DEF VAR bOK            AS LOG    NO-UNDO.
DEF VAR ix             AS INT    NO-UNDO.

DEF VAR hBrowse        AS HANDLE NO-UNDO.
DEF VAR hQuery         AS HANDLE NO-UNDO.
DEF VAR hBuffer        AS HANDLE NO-UNDO.
DEF VAR hToolbar       AS HANDLE NO-UNDO.
DEF VAR hParent        AS HANDLE NO-UNDO.

DEF VAR cQueryJoin     AS CHAR   NO-UNDO.
DEF VAR hBrwFillInDato AS HANDLE NO-UNDO.
DEF VAR hBrwFillInKID  AS HANDLE NO-UNDO.

DEF VAR cKID AS CHAR NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rectFaktHode rectToolbar Navn KundeNr Belop ~
Avvik Saldo cmbButikk FakturaNr 
&Scoped-Define DISPLAYED-OBJECTS Navn KundeNr Belop Avvik Saldo cmbButikk ~
FakturaNr 

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

DEFINE VARIABLE Avvik AS DECIMAL FORMAT ">>9.99":U INITIAL 0 
     LABEL "+/-" 
     VIEW-AS FILL-IN 
     SIZE 10.6 BY 1 NO-UNDO.

DEFINE VARIABLE Belop AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Fakturabeløp" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FakturaNr AS DECIMAL FORMAT ">>>>>>>>>>>>9" INITIAL 0 
     LABEL "Fakturanr" 
     VIEW-AS FILL-IN 
     SIZE 20.2 BY 1.

DEFINE VARIABLE KundeNr AS DECIMAL FORMAT ">>>>>>>>>>>>9" INITIAL 0 
     LABEL "Kundenr" 
     VIEW-AS FILL-IN 
     SIZE 20.2 BY 1.

DEFINE VARIABLE Navn AS CHARACTER FORMAT "X(256)":U 
     LABEL "Kundenavn" 
     VIEW-AS FILL-IN 
     SIZE 18.8 BY 1 NO-UNDO.

DEFINE VARIABLE Saldo AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Fakturasaldo" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE RECTANGLE rectFaktHode
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 199 BY 26.67.

DEFINE RECTANGLE rectToolbar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 11 BY 1.19.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     Navn AT ROW 1.24 COL 13.2 COLON-ALIGNED
     KundeNr AT ROW 1.24 COL 57 COLON-ALIGNED HELP
          "Kundenummer"
     Belop AT ROW 1.24 COL 102 COLON-ALIGNED
     Avvik AT ROW 1.86 COL 120.4 COLON-ALIGNED
     Saldo AT ROW 2.38 COL 102 COLON-ALIGNED
     cmbButikk AT ROW 2.43 COL 13 COLON-ALIGNED
     FakturaNr AT ROW 2.43 COL 57 COLON-ALIGNED HELP
          "Fakturanummer"
     rectFaktHode AT ROW 3.62 COL 2
     rectToolbar AT ROW 1.24 COL 191
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 201 BY 30.14.


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
         TITLE              = "Registrer åpne poster som fullt betalt"
         HEIGHT             = 30.14
         WIDTH              = 201
         MAX-HEIGHT         = 50
         MAX-WIDTH          = 320
         VIRTUAL-HEIGHT     = 50
         VIRTUAL-WIDTH      = 320
         RESIZE             = yes
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
ON END-ERROR OF C-Win /* Registrer åpne poster som fullt betalt */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Registrer åpne poster som fullt betalt */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-RESIZED OF C-Win /* Registrer åpne poster som fullt betalt */
DO:
  DEF VAR hColumn AS HANDLE NO-UNDO.
  DYNAMIC-FUNCTION("setWidgetResize",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE:CURRENT-WINDOW,"Resize","").
  hColumn = hBrowse:GET-BROWSE-COLUMN(1).
  APPLY "end-resize" TO hColumn.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Avvik
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Avvik C-Win
ON RETURN OF Avvik IN FRAME DEFAULT-FRAME /* +/- */
DO:
  RUN OpenQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Belop
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Belop C-Win
ON RETURN OF Belop IN FRAME DEFAULT-FRAME /* Fakturabeløp */
DO:
  RUN OpenQuery.
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


&Scoped-define SELF-NAME FakturaNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FakturaNr C-Win
ON RETURN OF FakturaNr IN FRAME DEFAULT-FRAME /* Fakturanr */
DO:
  RUN OpenQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME KundeNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL KundeNr C-Win
ON RETURN OF KundeNr IN FRAME DEFAULT-FRAME /* Kundenr */
DO:
  RUN OpenQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Navn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Navn C-Win
ON RETURN OF Navn IN FRAME DEFAULT-FRAME /* Kundenavn */
DO:
  RUN OpenQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Saldo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Saldo C-Win
ON RETURN OF Saldo IN FRAME DEFAULT-FRAME /* Fakturasaldo */
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplayRecord C-Win 
PROCEDURE DisplayRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF hBrowse:QUERY:GET-BUFFER-HANDLE(1):AVAIL THEN DO:

  IF hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("InnbetDato"):BUFFER-VALUE = ? AND
     DYNAMIC-FUNCTION("getLinkedObject",hBrowse,"browseoverlay","from") = ? THEN DO:
    hBrwFillInDato = DYNAMIC-FUNCTION("NewBrowseFillIn",
                      hBrowse,
                      "InnbetDato",
                      "InnbetDato",
                      "","","","").
    DYNAMIC-FUNCTION("CreateOverlayLink",hBrowse,hBrwFillInDato,"InnbetDato").
    DYNAMIC-FUNCTION("setAttribute",hBrwFillInDato,"refreshrow","yes").
    hBrwFillInDato:HELP = "Registrering av innbet.dato posterer restsaldo som innbetalt".

    hBrwFillInKID = DYNAMIC-FUNCTION("NewBrowseFillIn",
                     hBrowse,
                     "KID",
                     "KID",
                     "","","","").
    DYNAMIC-FUNCTION("CreateOverlayLink",hBrowse,hBrwFillInKID,"KID").
    DYNAMIC-FUNCTION("setAttribute",hBrwFillInKID,"refreshrow","yes").
    hBrwFillInKID:HELP = "Registrering av KID".
  END.
  ELSE IF hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("InnbetDato"):BUFFER-VALUE NE ? AND
     DYNAMIC-FUNCTION("getLinkedObject",hBrowse,"browseoverlay","from") NE ? THEN 
  DO:
    DYNAMIC-FUNCTION("DeleteObjectLink",hBrwFillInDato,hBrowse).
    hBrwFillInDato:HIDDEN = TRUE.
    DYNAMIC-FUNCTION("DeleteObjectLink",hBrwFillInKID,hBrowse).
    hBrwFillInKID:HIDDEN = TRUE.
  END.
END.
ELSE IF VALID-HANDLE(hBrwFillInDato) THEN 
    ASSIGN 
      hBrwFillInDato:HIDDEN = TRUE
      hBrwFillInKID:HIDDEN  = TRUE
      .

DYNAMIC-FUNCTION("setCurrentObject",hBrowse).

RUN SUPER.

IF DYNAMIC-FUNCTION("getLinkedObject",hBrowse,"browseoverlay","from") = ? THEN
  APPLY "entry" TO hBrowse.
ELSE IF VALID-HANDLE(hBrwFillInDato) AND NOT hBrwFillInDato:HIDDEN THEN
  APPLY "entry" TO hBrwFillInDato.


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
  DISPLAY Navn KundeNr Belop Avvik Saldo cmbButikk FakturaNr 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE rectFaktHode rectToolbar Navn KundeNr Belop Avvik Saldo cmbButikk 
         FakturaNr 
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
  STATUS INPUT "".

  cKID = DYNAMIC-FUNCTION("getFieldValues","SysPara",
                             "WHERE SysHId = 19 and SysGr = 100 and ParaNr = 5","Parameter1").
  ASSIGN cmbButikk:DELIMITER = "|"
         cmbButikk:LIST-ITEM-PAIRS = "|0|" + DYNAMIC-FUNCTION("getFieldList","Butiker;butik|butnamn;butik","where true")
         cmbButikk:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldValues","Bruker","WHERE BrukerId = '" + DYNAMIC-FUNCTION("getASuserId") + "'","ButikkNr")

         cQueryJoin = ",FIRST Bilagsart OF Kundereskontr NO-LOCK"
         . 

  hBrowse = DYNAMIC-FUNCTION("NewBrowse",
                          rectFaktHode:HANDLE,        /* Coordinates */
                          10000,                      /* Batchsize */
                          "",                        /* Attributes that must be set at creation time for the browse */
                          "Kundereskontr"              /* Buffers and fields (and calculated fields) for browse */
                          + ";FakturaNr"
                          + ";BArtNr"
                          + ";Belop"
                          + ";Saldo"
                          + ";+InnbetDato|DATE|99/99/99|innbetreg_innbetdato.p(Reskontro_id)|Innbet.dato"
                          + (IF cKID = '1' THEN ";KID" ELSE '')
                          + ";FakturertDato"
                          + ";ForfallsDato" 
                          + ";Purretrinn"
                          + ";SistePurredato"
                          + ";OpprForfallsDato"
                          + ";Reklamert"
                          + ";KundeNr@1"
                          + ";!Reskontro_id"
                        + ",Bilagsart"
                          + ";BArtTekst@5"
                        + ",Kunde"
                          + ";Navn@2"
                        + ",FakturaHode"
                          + ";ButikkNr@6"
                          ,"WHERE false" + cQueryJoin
                           + ",FIRST Kunde OF Kundereskontr NO-LOCK"
                           + ",FIRST FakturaHode WHERE FakturaHode.FakturaNr = Kundereskontr.FakturaNr NO-LOCK"
                          ,"sort|FakturertDato desc").         
  hBrowse:NAME = "brwFaktHode".
  hQuery = hBrowse:QUERY.
  hBuffer = hQuery:GET-BUFFER-HANDLE(1).

  /*
  hBrowse:MOVE-COLUMN(12,1).
  hBrowse:MOVE-COLUMN(13,4).
  hBrowse:MOVE-COLUMN(14,2).
  hBrowse:MOVE-COLUMN(15,6).
  */
  DO ix = 1 TO 6:
    hBrowse:GET-BROWSE-COLUMN(ix):WIDTH-PIXELS = hBrowse:GET-BROWSE-COLUMN(ix):WIDTH-PIXELS - 20.
  END.
  hBrowse:GET-BROWSE-COLUMN(2):WIDTH-PIXELS = hBrowse:GET-BROWSE-COLUMN(2):WIDTH-PIXELS - 20.
  hBrowse:GET-BROWSE-COLUMN(5):WIDTH-PIXELS = hBrowse:GET-BROWSE-COLUMN(5):WIDTH-PIXELS - 20.

  DYNAMIC-FUNCTION("setNoColumnSort",hBrowse,"InnbetDato").
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"customUpdateValProc","=kunderes_fullbet.p").

  hToolbar = DYNAMIC-FUNCTION("NewToolBar",
                    rectToolBar:HANDLE,             
                    "Fil",                          
                    "excel"
                  + ",StartQuery;&StartSpørring;Start spørring;OpenQuery;bmp/searc16e.bmp"
                    ,"").
  DYNAMIC-FUNCTION("setAttribute",hToolbar,"enabledevents","StartQuery").
  DYNAMIC-FUNCTION("createObjectLink",hBrowse,hToolbar).

  DYNAMIC-FUNCTION("InitTranslation",THIS-PROCEDURE:CURRENT-WINDOW).
  DYNAMIC-FUNCTION("setOrgWinSize",THIS-PROCEDURE:CURRENT-WINDOW,400,500,500,550).

  APPLY "value-changed" TO cmbButikk.
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
APPLY "entry" TO Navn IN FRAME {&FRAME-NAME}.
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
DEF VAR cFraBelop AS CHAR NO-UNDO.
DEF VAR cTilBelop AS CHAR NO-UNDO.
DEF VAR cFraSaldo AS CHAR NO-UNDO.
DEF VAR cTilSaldo AS CHAR NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
  ASSIGN Belop Saldo Avvik
         cFraBelop = STRING(Belop - Avvik)
         cTilBelop = STRING(Belop + Avvik)
         cFraSaldo = STRING(Saldo - Avvik)
         cTilSaldo = STRING(Saldo + Avvik)
         .

  DYNAMIC-FUNCTION("setAttribute",hBrowse,"querywhere",
                   "WHERE Saldo > 0"
                 + (IF Kundenr:SCREEN-VALUE NE "0" THEN
                     "  AND Kundenr = " + Kundenr:SCREEN-VALUE
                    ELSE "")
                 + (IF FakturaNr:SCREEN-VALUE NE "0" THEN
                     "  AND FakturaNr = " + FakturaNr:SCREEN-VALUE
                    ELSE "")
                 + (IF DEC(Belop:SCREEN-VALUE) NE 0 THEN
                     "  AND Belop GE DEC(" + cFraBelop + ")"
                   + "  AND Belop LE DEC(" + cTilBelop + ")"
                    ELSE "")
                 + (IF DEC(Saldo:SCREEN-VALUE) NE 0 THEN
                     "  AND Saldo GE DEC(" + cFraSaldo + ")"
                   + "  AND Saldo LE DEC(" + cTilSaldo + ")"
                    ELSE "")
                   ).
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"queryjoin",cQueryJoin
                 + (IF Navn:SCREEN-VALUE NE "" THEN
                     ",FIRST Kunde OF Kundereskontr NO-LOCK WHERE Kunde.Navn " +
                     (IF INDEX(Navn:SCREEN-VALUE,"*") > 0 THEN "MATCHES" ELSE "BEGINS") + " '" + Navn:SCREEN-VALUE + "'"
                    ELSE ",FIRST Kunde OF Kundereskontr NO-LOCK")
                 + (IF cmbButikk:SCREEN-VALUE NE "0" AND cmbButikk:SCREEN-VALUE NE ? THEN
                     ",FIRST FakturaHode NO-LOCK WHERE FakturaHode.FakturaNr = Kundereskontr.FakturaNr AND FakturaHode.ButikkNr = " + cmbButikk:SCREEN-VALUE
                    ELSE ",FIRST FakturaHode WHERE FakturaHode.FakturaNr = Kundereskontr.FakturaNr NO-LOCK")
                   ).
END.

/* CLIPBOARD:VALUE =  DYNAMIC-FUNCTION("getAttribute",hBrowse,"querywhere").  */
        
DYNAMIC-FUNCTION("setCurrentObject",hBrowse).

RUN SUPER.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

