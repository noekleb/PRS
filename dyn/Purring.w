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

DEF VAR cQueryJoin    AS CHAR   NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rectFaktHode rectToolbar cbPurreTrinn ~
cmbButikk 
&Scoped-Define DISPLAYED-OBJECTS cbPurreTrinn cmbButikk 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE cbPurreTrinn AS CHARACTER FORMAT "X(256)":U 
     LABEL "Purretrinn" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 40.4 BY 1 NO-UNDO.

DEFINE VARIABLE cmbButikk AS CHARACTER FORMAT "X(256)":U 
     LABEL "Butikk" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 32.6 BY 1 NO-UNDO.

DEFINE RECTANGLE rectFaktHode
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 150 BY 27.48.

DEFINE RECTANGLE rectToolbar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 14 BY 1.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     cbPurreTrinn AT ROW 1.48 COL 10.6 COLON-ALIGNED
     cmbButikk AT ROW 1.48 COL 60.4 COLON-ALIGNED
     rectFaktHode AT ROW 2.81 COL 2
     rectToolbar AT ROW 1.43 COL 137.8
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
         TITLE              = "Purring"
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
   FRAME-NAME                                                           */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Purring */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Purring */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-RESIZED OF C-Win /* Purring */
DO:
  DYNAMIC-FUNCTION("setWidgetResize",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE:CURRENT-WINDOW,"Resize","").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cbPurreTrinn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbPurreTrinn C-Win
ON VALUE-CHANGED OF cbPurreTrinn IN FRAME DEFAULT-FRAME /* Purretrinn */
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
  DISPLAY cbPurreTrinn cmbButikk 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE rectFaktHode rectToolbar cbPurreTrinn cmbButikk 
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

  ASSIGN cbPurreTrinn:DELIMITER = "|"
         cbPurreTrinn:LIST-ITEM-PAIRS = DYNAMIC-FUNCTION("getFieldList","Purretrinn;PurreTekst|DagerForfallt;Purretrinn","WHERE TRUE")

         cmbButikk:DELIMITER = "|"
         cmbButikk:LIST-ITEM-PAIRS = "|0|" + DYNAMIC-FUNCTION("getFieldList","Butiker;butik|butnamn;butik","where true")
         cmbButikk:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldValues","Bruker","WHERE BrukerId = '" + DYNAMIC-FUNCTION("getASuserId") + "'","ButikkNr")

         cQueryJoin = ",FIRST Kunde OF Kundereskontr NO-LOCK"
                    + ",FIRST Bilagsart OF Kundereskontr NO-LOCK"
         . 

  hBrowse = DYNAMIC-FUNCTION("NewBrowse",
                          rectFaktHode:HANDLE,        /* Coordinates */
                          10000,                      /* Batchsize */
                          "MULTIPLE",                 /* Attributes that must be set at creation time for the browse */
                          "Kundereskontr"             /* Buffers and fields (and calculated fields) for browse */
                          + ";FakturaNr"
                          + ";BArtNr"
                          + ";Belop"
                          + ";Saldo"
                          + ";FakturertDato"
                          + ";ForfallsDato" 
                          + ";Purretrinn"
                          + ";SistePurredato"
                          + ";OpprForfallsDato"
                          + ";Reklamert"
                          + ";KundeNr"
                          + ";!Reskontro_id"
                        + ",Kunde"
                          + ";Navn"
                        + ",Bilagsart"
                          + ";BArtTekst"
                        + ",FakturaHode"
                          + ";ButikkNr"
                          ,"WHERE false" + cQueryJoin
                           + ",FIRST FakturaHode WHERE FakturaHode.FakturaNr = Kundereskontr.FakturaNr NO-LOCK"
                          ,"sort|FakturertDato desc").         
  hBrowse:NAME = "brwFaktHode".
  hQuery = hBrowse:QUERY.
  hBuffer = hQuery:GET-BUFFER-HANDLE(1).

  hBrowse:MOVE-COLUMN(11,1).
  hBrowse:MOVE-COLUMN(12,2).
  hBrowse:MOVE-COLUMN(13,5).
  hBrowse:MOVE-COLUMN(14,6).


  hToolbar = DYNAMIC-FUNCTION("NewToolBar",
                    rectToolBar:HANDLE,             
                    "Fil",                          
                    "Reject;&Slett;Fjern markerte poster fra kjøring;RejectRecord;bmp/rejec16e.bmp"
                  + ",excel"
/*                   + ",PrintPreview;Forhvisn;Forhåndsvisning;PrintPreviewRecord;gif/afprintpre.gif"  */
                  + ",print"
                    ,"").
  DYNAMIC-FUNCTION("createObjectLink",hBrowse,hToolbar).

  DYNAMIC-FUNCTION("InitTranslation",THIS-PROCEDURE:CURRENT-WINDOW).
  DYNAMIC-FUNCTION("setOrgWinSize",THIS-PROCEDURE:CURRENT-WINDOW,400,500,500,550).
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
APPLY "entry" TO cbPurreTrinn IN FRAME {&FRAME-NAME}.
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
  IF cbPurreTrinn:SCREEN-VALUE = ? THEN DO:
    DYNAMIC-FUNCTION("DoMessage",0,0,"Purretrinn må angis","","").
    RETURN.
  END.

  DYNAMIC-FUNCTION("setAttribute",hBrowse,"querywhere",
                   "WHERE Purretrinn LT " + cbPurreTrinn:SCREEN-VALUE
                 + "  AND Purretrinn GE " + cbPurreTrinn:SCREEN-VALUE + " - 1"
                 + "  AND ForfallsDato LE DATE('" + STRING(TODAY - INT(ENTRY(NUM-ENTRIES(DYNAMIC-FUNCTION("getDropDownLabel",cbPurreTrinn:HANDLE,"|"),"/"),DYNAMIC-FUNCTION("getDropDownLabel",cbPurreTrinn:HANDLE,"|"),"/"))) + "')"
                 + "  AND SendtInkasso = ?"
                 + "  AND Reklamert    = ?"
                 + "  AND BilagsType   = 1"
                 + "  AND Saldo        > 0"
                   ).
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"queryjoin",cQueryJoin
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PrintPreviewRecord C-Win 
PROCEDURE PrintPreviewRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF VAR cIdList  AS CHAR NO-UNDO.

DO ix = 1 TO hBrowse:NUM-SELECTED-ROWS:
  IF hBrowse:FETCH-SELECTED-ROW(ix) THEN 
    cIdList = cIdList + STRING(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("Reskontro_id"):BUFFER-VALUE) + ",".
END.
IF cIdList NE "" THEN DO WITH FRAME {&FRAME-NAME}:
  cIdList = TRIM(cIdList,",").
  IF NOT DYNAMIC-FUNCTION("runproc","faktura_purring.p","idlist|" + cIdList + "|" + cbPurreTrinn:SCREEN-VALUE,?) THEN 
    DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").
  ELSE DO:
    /* Får tilbake liste over genererte purrefaktuaer: */
    cIdList = DYNAMIC-FUNCTION("getTransactionMessage").

    RUN skrivfaktura.p (cIdList + "|",FALSE,"",1,
                        DYNAMIC-FUNCTION("getFieldValues","Kunde","WHERE KundeNr = " + STRING(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("KundeNr"):BUFFER-VALUE),"ePostAdresse"),1). 
    RUN OpenQuery.
  END.
END.
ELSE DYNAMIC-FUNCTION("DoMessage",0,0,"Ingen faktura er valgt","","").

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
DEF VAR iAntEks  AS INT  NO-UNDO INIT 1.
DEF VAR iFormat  AS INT  NO-UNDO INIT 1.

hQuery:GET-FIRST().
REPEAT WHILE NOT hQuery:QUERY-OFF-END:
  cIdList = cIdList + STRING(hBuffer:BUFFER-FIELD("Reskontro_id"):BUFFER-VALUE) + ",".
  hQuery:GET-NEXT().
END.

IF cIdList NE "" THEN DO WITH FRAME {&FRAME-NAME}:
  cIdList = TRIM(cIdList,",").
  RUN DSelectPrinter.w (INPUT-OUTPUT cPrinter,INPUT-OUTPUT iAntEks,INPUT-OUTPUT iFormat,"Fakturaskriver",OUTPUT bOk).
  IF NOT bOk THEN RETURN.

  IF NOT DYNAMIC-FUNCTION("runproc","faktura_purring.p","idlist|" + cIdList + "|" + cbPurreTrinn:SCREEN-VALUE,?) THEN 
    DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").
  ELSE DO:
    /* Får tilbake liste over genererte purrefaktuaer: */
    cIdList = DYNAMIC-FUNCTION("getTransactionMessage").

/*       RUN skrivfaktura.p (cIdList + "|",TRUE,cPrinter,iAntEks,"",iFormat). */
      RUN skrivfaktura.p (cIdList + "|",TRUE,cPrinter,iAntEks,"",iFormat).

/*       RUN skrivfaktura.p (cIdList + "|",FALSE,"",1,                                                                                                                  */
/*                           DYNAMIC-FUNCTION("getFieldValues","Kunde","WHERE KundeNr = " + STRING(hFieldMap:BUFFER-FIELD("KundeNr"):BUFFER-VALUE),"ePostAdresse"),1).  */
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StartSearch C-Win 
PROCEDURE StartSearch :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
  IF cbPurreTrinn:SCREEN-VALUE = ? THEN RETURN.
  RUN SUPER.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

