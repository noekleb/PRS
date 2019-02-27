&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
/* Procedure Description
"Basic Window Template

Use this template to create a new window. Alter this default template or create new ones to accomodate your needs for different default sizes and/or attributes."
*/
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*********************************************************************
* Copyright (C) 2001 by Progress Software Corporation ("PSC"),       *
* 14 Oak Park, Bedford, MA 01730, and other contributors as listed   *
* below.  All Rights Reserved.                                       *
*                                                                    *
* The Initial Developer of the Original Code is PSC.  The Original   *
* Code is Progress IDE code released to open source December 1, 2000.*
*                                                                    *
* The contents of this file are subject to the Possenet Public       *
* License Version 1.0 (the "License"); you may not use this file     *
* except in compliance with the License.  A copy of the License is   *
* available as of the date of this notice at                         *
* http://www.possenet.org/license.html                               *
*                                                                    *
* Software distributed under the License is distributed on an "AS IS"*
* basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. You*
* should refer to the License for the specific language governing    *
* rights and limitations under the License.                          *
*                                                                    *
* Contributors:                                                      *
*                                                                    *
*********************************************************************/
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

DEF VAR bOK             AS LOG NO-UNDO.
DEF VAR ix              AS INT NO-UNDO.
DEF VAR iTab            AS INT NO-UNDO.

DEF VAR hToolbar        AS HANDLE NO-UNDO.
DEF VAR hBrowse         AS HANDLE NO-UNDO.
DEF VAR hBuffer         AS HANDLE NO-UNDO.
DEF VAR hSearchField    AS HANDLE NO-UNDO.
DEF VAR hTabFolder      AS HANDLE NO-UNDO.
DEF VAR hWinToolBar     AS HANDLE NO-UNDO.
 
DEF VAR hCurrTabProc    AS HANDLE NO-UNDO.
DEF VAR hCurrTabFrame   AS HANDLE NO-UNDO.
DEF VAR iCurrTab        AS INT    NO-UNDO.
DEF VAR hKOFsok         AS HANDLE NO-UNDO.
DEF VAR cTabProgList    AS CHAR   NO-UNDO.

DEF VAR hFakturering    AS HANDLE NO-UNDO.
DEF VAR hPurring        AS HANDLE NO-UNDO.
DEF VAR hSaldoAlder     AS HANDLE NO-UNDO.
DEF VAR hInnbetaling    AS HANDLE NO-UNDO.
DEF VAR hFakturaListe   AS HANDLE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rectFolder rectWinToolBar tbKunde cmbButikk ~
Kundetekst Kunde 
&Scoped-Define DISPLAYED-OBJECTS cmbButikk Kundetekst Kunde 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getKundeNr C-Win 
FUNCTION getKundeNr RETURNS DECIMAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD InitializeResize C-Win 
FUNCTION InitializeResize RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setHeading C-Win 
FUNCTION setHeading RETURNS LOGICAL
  ( INPUT ihBuffer AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ShuffleFrames C-Win 
FUNCTION ShuffleFrames RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD TabChanged C-Win 
FUNCTION TabChanged RETURNS LOGICAL
  ( INPUT iiTab AS INT )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE cmbButikk AS CHARACTER FORMAT "X(256)":U 
     LABEL "Butikk" 
     VIEW-AS COMBO-BOX INNER-LINES 40
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 33.2 BY 1 NO-UNDO.

DEFINE VARIABLE Kundetekst AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 185 BY 1.67 TOOLTIP "Siste hendelse" NO-UNDO.

DEFINE VARIABLE Kunde AS CHARACTER FORMAT "x(255)" 
      VIEW-AS TEXT 
     SIZE 124 BY .62
     FONT 6.

DEFINE RECTANGLE rectFolder
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 188 BY 26.91.

DEFINE RECTANGLE rectWinToolBar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 4.8 BY 1.

DEFINE RECTANGLE tbKunde
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 4.8 BY 1.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     cmbButikk AT ROW 1.19 COL 13.4 COLON-ALIGNED
     Kundetekst AT ROW 2.43 COL 4 NO-LABEL
     Kunde AT ROW 1.33 COL 48.6 COLON-ALIGNED NO-LABEL
     rectFolder AT ROW 4.33 COL 2
     rectWinToolBar AT ROW 1.19 COL 185
     tbKunde AT ROW 1.24 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 189.8 BY 30.29.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window Template
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Fakturering/reskontro"
         HEIGHT             = 30.29
         WIDTH              = 189.8
         MAX-HEIGHT         = 52.38
         MAX-WIDTH          = 320
         VIRTUAL-HEIGHT     = 52.38
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

&IF '{&WINDOW-SYSTEM}' NE 'TTY' &THEN
IF NOT C-Win:LOAD-ICON("ico/cntrlhry.ico":U) THEN
    MESSAGE "Unable to load icon: ico/cntrlhry.ico"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
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
ASSIGN 
       Kunde:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       Kundetekst:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME DEFAULT-FRAME
/* Query rebuild information for FRAME DEFAULT-FRAME
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME DEFAULT-FRAME */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Fakturering/reskontro */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Fakturering/reskontro */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cmbButikk
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cmbButikk C-Win
ON VALUE-CHANGED OF cmbButikk IN FRAME DEFAULT-FRAME /* Butikk */
DO:
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"baseQuery",
                   (IF cmbButikk:SCREEN-VALUE NE ? AND cmbButikk:SCREEN-VALUE NE "0" THEN 
                      "WHERE ButikkNr = " + cmbButikk:SCREEN-VALUE
                    ELSE "")).
    
  RUN InvokeMethod(hBrowse,"OpenQuery").
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
  IF VALID-HANDLE(hKOFsok)       THEN APPLY "close" TO hKOFsok.
  IF VALID-HANDLE(hFakturering)  THEN APPLY "close" TO hFakturering.
  IF VALID-HANDLE(hPurring)      THEN APPLY "close" TO hPurring.
  IF VALID-HANDLE(hInnbetaling)  THEN APPLY "close" TO hInnbetaling.
  IF VALID-HANDLE(hFakturaListe) THEN APPLY "close" TO hFakturaListe.
  RUN disable_UI.
END.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.


{incl/wintrigg.i}
/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

  RUN enable_UI.
  &IF DEFINED(UIB_is_Running) NE 0 &THEN
    RUN InitializeObject.
    RUN MoveToTop.
  &ENDIF

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

ON 'alt-s':U OF FRAME {&FRAME-NAME} ANYWHERE
  RUN SearchRecord.

ON 'window-resized':U OF {&WINDOW-NAME} DO:
  DYNAMIC-FUNCTION("setWidgetResize",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE:CURRENT-WINDOW,"Resize","").

  ShuffleFrames().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AddUpdRecord C-Win 
PROCEDURE AddUpdRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM icTBstate AS CHAR NO-UNDO.
DEF INPUT PARAM ihBuffer  AS HANDLE NO-UNDO.

DEF VAR rRepos AS ROWID NO-UNDO.

IF icTBstate = "" THEN RETURN.

IF icTBstate = "delete" THEN
  hBuffer:BUFFER-DELETE().
ELSE DO:
  IF icTBstate = "new" THEN 
    hBuffer:BUFFER-CREATE().

  hBuffer:BUFFER-COPY(ihBuffer).
END.

IF icTBstate = "new" THEN DO:
  rRepos = hBuffer:ROWID.
  hBrowse:QUERY:QUERY-OPEN().
  hBrowse:QUERY:REPOSITION-TO-ROWID(rRepos).
END.
ELSE hBrowse:REFRESH().

DYNAMIC-FUNCTION("setCurrentObject",hBrowse).
RUN DisplayRecord.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AltSKundeOrdre C-Win 
PROCEDURE AltSKundeOrdre :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM icTabProg AS CHAR NO-UNDO.

IF NOT VALID-HANDLE(hKOFsok) THEN 
  RUN KOFsok.w PERSIST SET hKOFsok.

RUN InitializeObject IN hKOFsok (icTabProg).

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
  DISPLAY cmbButikk Kundetekst Kunde 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE rectFolder rectWinToolBar tbKunde cmbButikk Kundetekst Kunde 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FakturaListeRecord C-Win 
PROCEDURE FakturaListeRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF NOT VALID-HANDLE(hFakturaListe) THEN
  RUN FakturaListe.w PERSIST SET hFakturaListe.

RUN MoveToTop IN hFakturaListe.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Fakturering C-Win 
PROCEDURE Fakturering :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF NOT VALID-HANDLE(hFakturering) THEN
  RUN Fakturering.w PERSIST SET hFakturering.

RUN MoveToTop IN hFakturering.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FlatViewDblClick C-Win 
PROCEDURE FlatViewDblClick :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM icBuffer AS CHAR NO-UNDO.
DEF INPUT PARAM icRowId  AS CHAR NO-UNDO.

DEF VAR cKundeNr    AS CHAR NO-UNDO.

IF icBuffer NE "Kunde" THEN RETURN.

IF DYNAMIC-FUNCTION("runproc","kunde_find_by_rowid.p",icRowId,?) THEN
  RUN SetSearch(DYNAMIC-FUNCTION("getTransactionMessage"),TRUE,"").
ELSE DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitializeObject C-Win 
PROCEDURE InitializeObject :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cBrukerTypeBut AS CHAR  NO-UNDO.
DEF VAR cTabList       AS CHAR  NO-UNDO.
DEF VAR iy             AS INT   NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
  DYNAMIC-FUNCTION("NewObject",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE,"container").

  ASSIGN cBrukerTypeBut = DYNAMIC-FUNCTION("getFieldValues","Bruker","WHERE BrukerId = '" + DYNAMIC-FUNCTION("getASuserId") + "'","BrukerType,ButikkNr")
         cmbButikk:DELIMITER = "|"
         cmbButikk:LIST-ITEM-PAIRS = (IF ENTRY(2,cBrukerTypeBut,"|") <> "2" THEN "|0|" ELSE "")
                                   + DYNAMIC-FUNCTION("getFieldList","Butiker;butik|butnamn;butik",
                                                          "WHERE TRUE BY butik")
         cmbButikk:SCREEN-VALUE = ENTRY(2,cBrukerTypeBut,"|")
         cmbButikk:SENSITIVE    = ENTRY(1,cBrukerTypeBut,"|") NE "2"
         cmbButikk:BGCOLOR  = 15
         .

  hTabFolder = DYNAMIC-FUNCTION("NewTabFolder",rectFolder:HANDLE).
  
  /* X and Y limits for move of widget are not yet set for the window. 
     Since we want the tabs to resize according to the window size these values must be provided here and
     they must be exact the same as in setOrwWinSize (under) */
  DYNAMIC-FUNCTION("setMinXYmove",800,500). 


  cTabList = "Kundeliste|kunde_brw.w|Kunde|KundeView.w|Notater kunde|Kundekommentar.w|Faktura|Faktura.w|Reskontro|Kundereskontr.w|Medlemmer|Medlemmer.w|Kundetranser|KundeTrans.w".

  DYNAMIC-FUNCTION("InitPages" IN hTabFolder,cTabList,hBrowse).

  DO ix = 1 TO NUM-ENTRIES(cTabList,"|") BY 2:
    IF ix MOD 2 = 1 THEN iy = iy + 1.
    cTabProgList = cTabProgList + ENTRY(ix + 1,cTabList,"|") + ",".
  END.
  cTabProgList = TRIM(cTabProgList,",").

  DYNAMIC-FUNCTION("buildFolder" IN hTabFolder).

  hToolbar = DYNAMIC-FUNCTION("NewToolBar",
                    tbKunde:HANDLE,             
                    "Fil",                          
                    "Search;&Søk;Søk;SearchRecord;bmp/searc16e.bmp¤enable"
                  + ",|Innkreving/Innbetaling,|Rapporter¤enable"
                   ,"enable,maxborder").

  DYNAMIC-FUNCTION("NewMenuBand",
                   WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",hToolbar,"placeholder1")),
                   "Fakturering;Fakturering;Fakturering¤enable"
                 + ",Purring¤enable"
                 + ",Innbetaling¤enable"
                 + ",|-" 
                 + ",Purreliste¤enable"
                   ,"").

  DYNAMIC-FUNCTION("NewMenuBand",
                   WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",hToolbar,"placeholder2")),
                   "SaldoAlder;Aldersfordelt saldoliste¤enable"
                 + ",FakturaListe;Fakturaliste¤enable"
                   ,"").

  hWinToolBar = DYNAMIC-FUNCTION("NewToolBar",
                    rectWinToolBar:HANDLE,             /* Rectangle to define coordinates for toolbar */
                    "Fil",                          /* Corresponding menu label - no menu if blank */
                    "|Innstillinger,Close;Avslutt,Help|Hjelp",
                                                    /* Buttons / Menu items: action;label;tooltip;Method;image,action;label.. 
                                                       Any number of properties accepted (one ok - if predef. action) */
                    "enable,right").                        /* Misc - for something I might need in next version.. */

  InitializeResize().

END.

SUBSCRIBE TO "AltSKundeOrdre"   ANYWHERE.
SUBSCRIBE TO "FlatViewDblClick" ANYWHERE.

DYNAMIC-FUNCTION("initTranslation",THIS-PROCEDURE:CURRENT-WINDOW).
DYNAMIC-FUNCTION("TabFolderChanged" IN hTabFolder,1).


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InnbetalingRecord C-Win 
PROCEDURE InnbetalingRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF NOT VALID-HANDLE(hInnbetaling) THEN
  RUN Innbetaling.w PERSIST SET hInnbetaling.

RUN MoveToTop IN hInnbetaling.

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
{&WINDOW-NAME}:WINDOW-STATE = 3.
{&WINDOW-NAME}:HIDDEN = NO.
{&WINDOW-NAME}:MOVE-TO-TOP().

RUN MoveToTop IN hCurrTabProc NO-ERROR.
THIS-PROCEDURE:CURRENT-WINDOW:HIDDEN = NO.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PurrelisteRecord C-Win 
PROCEDURE PurrelisteRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF NOT VALID-HANDLE(hPurring) THEN
  RUN Purreliste.w PERSIST SET hPurring.

RUN MoveToTop IN hPurring.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PurringRecord C-Win 
PROCEDURE PurringRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF NOT VALID-HANDLE(hPurring) THEN
  RUN Purring.w PERSIST SET hPurring.

RUN MoveToTop IN hPurring.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RefreshCurrentRow C-Win 
PROCEDURE RefreshCurrentRow :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DYNAMIC-FUNCTION("RefreshRowids",hBrowse,hBuffer:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE).
APPLY "value-changed" TO hBrowse.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SaldoAlderRecord C-Win 
PROCEDURE SaldoAlderRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR hFlatBrw AS HANDLE NO-UNDO.

RUN JBoxDataBrw.w PERSIST SET hSaldoAlder.
hSaldoAlder:CURRENT-WINDOW:TITLE = "Aldersfordelt saldo".
DYNAMIC-FUNCTION("setUseLocalData" IN hSaldoAlder,TRUE).
/* DYNAMIC-FUNCTION("setInitFilter" IN hSaldoAlder,"Navn","BEGINS","'a'").  */
RUN InitializeObject IN hSaldoAlder
    ("Kunde"
     + ";KundeNr"
     + ";Navn"
     + ";ButikkNr|Oppr.butikk"
     + ";SisteKjop"
     + ";KundeSaldo"
     + ";+Forfalt|DECIMAL|-><>>><>>9.99|saldo_forfalt.p(KundeNr)"
     + ";+0_15|DECIMAL|-><>>><>>9.99|saldo_0_15.p(KundeNr)"
     + ";+16_30|DECIMAL|-><>>><>>9.99|saldo_16_30.p(KundeNr)"
     + ";+31_60|DECIMAL|-><>>><>>9.99|saldo_31_60.p(KundeNr)"
     + ";+61_90|DECIMAL|-><>>><>>9.99|saldo_61_90.p(KundeNr)"
     + ";+91_|DECIMAL|-><>>><>>9.99|saldo_91_.p(KundeNr)"
    ,"WHERE KundeSaldo NE 0"
    ,"KundeNr",
     TRUE).

hFlatBrw = DYNAMIC-FUNCTION("getBrowseHandle" IN hSaldoAlder).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SearchRecord C-Win 
PROCEDURE SearchRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
APPLY "alt-s" TO hCurrTabFrame.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setSearch C-Win 
PROCEDURE setSearch :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM cKunde    AS CHAR NO-UNDO.
DEF INPUT PARAM ibFind    AS LOG  NO-UNDO.
DEF INPUT PARAM icTabProg AS CHAR NO-UNDO.

DEF VAR cFindString AS CHAR NO-UNDO.

DYNAMIC-FUNCTION("setCurrentObject",hBrowse).
IF ibFind THEN DO:
  cFindString = "WHERE " + IF cKunde NE "" THEN "Kundenr = " + ENTRY(1,cKunde,"|") ELSE "true".
  bOk = hBuffer:FIND-FIRST(cFindString) NO-ERROR.
  IF NOT bOK THEN DO:
    DYNAMIC-FUNCTION("setAttribute",hBrowse,"queryfilter","").
    DYNAMIC-FUNCTION("setAttribute",hBrowse,"querywhere",cFindString).
    RUN InvokeMethod(hBrowse,"OpenQuery").
  END.
  ELSE DO:
    hBrowse:QUERY:REPOSITION-TO-ROWID(hBuffer:ROWID).
    RUN DisplayRecord.
  END.
  
  IF NUM-ENTRIES(cKunde,"|") > 1 THEN DO:
    DYNAMIC-FUNCTION("TabFolderChanged" IN hTabFolder,LOOKUP(icTabProg,cTabProgList)).
    RUN setSearch IN hCurrTabProc (ENTRY(2,cKunde,"|")).
  END.
END.
ELSE DO:
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"queryfilter","").
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"querywhere",cKunde).
  RUN InvokeMethod(hBrowse,"OpenQuery").
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getKundeNr C-Win 
FUNCTION getKundeNr RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
IF hBrowse:QUERY:GET-BUFFER-HANDLE(1):AVAIL THEN
  RETURN hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("KundeNr"):BUFFER-VALUE.
ELSE
  RETURN 0.00.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION InitializeResize C-Win 
FUNCTION InitializeResize RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
/*   DYNAMIC-FUNCTION("setSplitBarX" , THIS-PROCEDURE:CURRENT-WINDOW, btnSplitBarX:HANDLE IN FRAME frSplitBarX,NO).                                     */
/*                                                                                                                                                      */
/*   DYNAMIC-FUNCTION("setFollowSplitBarX",THIS-PROCEDURE:CURRENT-WINDOW,btnSplitBarX:HANDLE IN FRAME frSplitBarX,                                      */
/*                     STRING(KundeTekst:HANDLE) + "," + STRING(Kunde:HANDLE) + "," +                                                                   */
/*                     DYNAMIC-FUNCTION("getWidgetsByLasso",rectBrowse:HANDLE IN FRAME {&FRAME-NAME},0,"frame,control-frame,rectangle,browse") + "," +  */
/*                     DYNAMIC-FUNCTION("getWidgetsByLasso",rectFolder:HANDLE IN FRAME {&FRAME-NAME},0,"frame,browse,control-frame,rectangle,editor")   */
/*                     ).                                                                                                                               */
/*   DYNAMIC-FUNCTION("setNoResizeX", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, "rectBrowse,brwKunde,searchField").                    */
  DYNAMIC-FUNCTION("setNoResizeY", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, "rectToolbar,KundeTekst").
  DYNAMIC-FUNCTION("setOrgWinSize",THIS-PROCEDURE:CURRENT-WINDOW,800,500,0,570).
END.
  
RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setHeading C-Win 
FUNCTION setHeading RETURNS LOGICAL
  ( INPUT ihBuffer AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
  IF ihBuffer:AVAIL THEN DO:
    ASSIGN Kunde:SCREEN-VALUE = ihBuffer:BUFFER-FIELD("Kundenr"):BUFFER-VALUE + " " + 
                                ihBuffer:BUFFER-FIELD("Navn"):BUFFER-VALUE + " " +
                                "Saldo: " + STRING(ihBuffer:BUFFER-FIELD("KundeSaldo"):BUFFER-VALUE)
           KundeTekst:SCREEN-VALUE = (IF ihBuffer:BUFFER-FIELD("Telefon"):BUFFER-VALUE NE "" THEN 
                                       "Tlf: " + ihBuffer:BUFFER-FIELD("Telefon"):BUFFER-VALUE
                                      ELSE "")
                                   + (IF ihBuffer:BUFFER-FIELD("MobilTlf"):BUFFER-VALUE NE "" THEN 
                                       " Mob: " + ihBuffer:BUFFER-FIELD("MobilTlf"):BUFFER-VALUE
                                      ELSE "")
                                   + (IF ihBuffer:BUFFER-FIELD("ePostAdresse"):BUFFER-VALUE NE "" THEN 
                                      " Epost: " + ihBuffer:BUFFER-FIELD("ePostAdresse"):BUFFER-VALUE
                                      ELSE "") 
                                   + CHR(10) 
                                   + (IF ihBuffer:BUFFER-FIELD("KontNavn"):BUFFER-VALUE NE "" THEN 
                                       " Kontaktperson: " + ihBuffer:BUFFER-FIELD("KontNavn"):BUFFER-VALUE
                                      ELSE "")
                                   + (IF ihBuffer:BUFFER-FIELD("KontTelefon"):BUFFER-VALUE NE "" THEN 
                                       " Tlf: " + ihBuffer:BUFFER-FIELD("KontTelefon"):BUFFER-VALUE 
                                      ELSE "")
                                   + (IF ihBuffer:BUFFER-FIELD("KontMobilTlf"):BUFFER-VALUE NE "" THEN 
                                       " Mob: " + ihBuffer:BUFFER-FIELD("KontMobilTlf"):BUFFER-VALUE
                                      ELSE "")
                                   + (IF ihBuffer:BUFFER-FIELD("KontE-post"):BUFFER-VALUE NE "" THEN 
                                       " Epost: " + ihBuffer:BUFFER-FIELD("KontE-post"):BUFFER-VALUE
                                      ELSE "")
                                     .
    IF iCurrTab = 2 THEN
      RUN ValueChangedField IN hCurrTabProc ("Privat") NO-ERROR.
  END.
  ELSE 
    ASSIGN Kunde:SCREEN-VALUE      = ""
           KundeTekst:SCREEN-VALUE = ""
           .
END.
  
RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ShuffleFrames C-Win 
FUNCTION ShuffleFrames RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DYNAMIC-FUNCTION("DoLockWindow",THIS-PROCEDURE:CURRENT-WINDOW).
RUN MoveToTop IN hCurrTabProc NO-ERROR.
IF ERROR-STATUS:ERROR THEN
  hCurrTabFrame:MOVE-TO-TOP() NO-ERROR.
DYNAMIC-FUNCTION("DoLockWindow",?).

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION TabChanged C-Win 
FUNCTION TabChanged RETURNS LOGICAL
  ( INPUT iiTab AS INT ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DYNAMIC-FUNCTION("DeleteObjectLink",hBrowse,DYNAMIC-FUNCTION("getPageQuery" IN hTabFolder,iCurrTab)).

DYNAMIC-FUNCTION("setAttribute",hToolbar,"disabledevents","").

IF iiTab = 1 AND iTab = 0 THEN DO:
  ASSIGN hBrowse = DYNAMIC-FUNCTION("getPageQuery" IN hTabFolder,iiTab)
         hBuffer = hBrowse:QUERY:GET-BUFFER-HANDLE(1).

  APPLY "VALUE-CHANGED" TO cmbButikk IN FRAME {&FRAME-NAME}.
END.
ELSE IF iiTab > 1 THEN
  DYNAMIC-FUNCTION("CreateParentLink",DYNAMIC-FUNCTION("getPageQuery" IN hTabFolder,iiTab),hBrowse,"KundeNr").

ASSIGN hCurrTabProc  = DYNAMIC-FUNCTION("getPageHandle" IN hTabFolder,iiTab)
       hCurrTabFrame = DYNAMIC-FUNCTION("getPageFrame" IN hTabFolder,iiTab)
       iTab          = iiTab
       .

IF CAN-DO(hCurrTabProc:INTERNAL-ENTRIES,"setFilter") THEN 
  DYNAMIC-FUNCTION("setFilter" IN hCurrTabProc).
IF CAN-DO("Faktura.w,KOrdre.w,Kundereskontr.w",hCurrTabProc:FILE-NAME) THEN 
  DYNAMIC-FUNCTION("TabChanged" IN hCurrTabProc,1).

APPLY "VALUE-CHANGED" TO hBrowse.

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

