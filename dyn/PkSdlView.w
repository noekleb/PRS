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

/* Local Variable Definitions ---                                       */

DEF VAR bOk               AS LOG  NO-UNDO.
DEF VAR ix                AS INT  NO-UNDO.
DEF VAR iReturn           AS INT  NO-UNDO.
DEF VAR hParent           AS HANDLE NO-UNDO.

DEF VAR hParentBuffer     AS HANDLE NO-UNDO.
DEF VAR hParentBrowse     AS HANDLE NO-UNDO.
DEF VAR hBrowse           AS HANDLE NO-UNDO.
DEF VAR hBuffer           AS HANDLE NO-UNDO.
DEF VAR hToolbar          AS HANDLE NO-UNDO.
DEF VAR hToolbarPris      AS HANDLE NO-UNDO.
DEF VAR hQuery            AS HANDLE NO-UNDO.
DEF VAR hFieldMap         AS HANDLE NO-UNDO.
DEF VAR hGyldigKodeCol    AS HANDLE NO-UNDO.
DEF VAR hArtBilde         AS HANDLE NO-UNDO.
DEF VAR hArtBildeFrame    AS HANDLE NO-UNDO.

DEF VAR hCurrTabProc      AS HANDLE NO-UNDO.
DEF VAR hCurrTabFrame     AS HANDLE NO-UNDO.
DEF VAR iCurrTab          AS INT    NO-UNDO.
DEF VAR iCurrBildNr       AS INT    NO-UNDO.

DEF VAR hLevAnt           AS HANDLE NO-UNDO.
DEF VAR hStrekkode        AS HANDLE NO-UNDO.
DEF VAR hArtikkelkort     AS HANDLE NO-UNDO.

DEF VAR iFontWingdings    AS INT    NO-UNDO.
iFontWingdings = DYNAMIC-FUNCTION("setAppFont","Wingdings, size=11 Script=symbol","").

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS ButNamn cRegTid fiMerknadLabel MeldingFraLev ~
PkSdlStatus Merknad cOrdreNrList CL PkSdlNr RegistrertAv RegistrertDato ~
SendtDato fiMeldingLabel SumFrakt TBPkSdlHode 
&Scoped-Define DISPLAYED-OBJECTS ButNamn cEkstIdList cRegTid fiMerknadLabel ~
MeldingFraLev PkSdlStatus Merknad cOrdreNrList CL PkSdlNr RegistrertAv ~
RegistrertDato SendtDato fiMeldingLabel SumFrakt 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getFrameHandle C-Win 
FUNCTION getFrameHandle RETURNS HANDLE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD InitializeResize C-Win 
FUNCTION InitializeResize RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setParent C-Win 
FUNCTION setParent RETURNS LOGICAL
  ( INPUT ihParent AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setParentQuery C-Win 
FUNCTION setParentQuery RETURNS LOGICAL
  ( INPUT ihParentQuery AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setQuery C-Win 
FUNCTION setQuery RETURNS LOGICAL
  (INPUT ihQuery AS HANDLE)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE PkSdlStatus AS CHARACTER FORMAT "X(256)":U 
     LABEL "Status" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 20 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE MeldingFraLev AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 63 BY 5.14 NO-UNDO.

DEFINE VARIABLE Merknad AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 63 BY 4.29 NO-UNDO.

DEFINE VARIABLE ButNamn AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 53.4 BY 1 NO-UNDO.

DEFINE VARIABLE cEkstIdList AS CHARACTER FORMAT "X(256)":U 
     LABEL "Eksterne ordre" 
     VIEW-AS FILL-IN 
     SIZE 63 BY 1 NO-UNDO.

DEFINE VARIABLE CL AS INTEGER FORMAT ">>>>>9" INITIAL 0 
     LABEL "Sentrallager" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1.

DEFINE VARIABLE cOrdreNrList AS CHARACTER FORMAT "X(256)":U 
     LABEL "Ordrenumre" 
     VIEW-AS FILL-IN 
     SIZE 63 BY 1 NO-UNDO.

DEFINE VARIABLE cRegTid AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 8.2 BY 1 NO-UNDO.

DEFINE VARIABLE fiMeldingLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Melding fra lev:" 
      VIEW-AS TEXT 
     SIZE 14.4 BY .62 NO-UNDO.

DEFINE VARIABLE fiMerknadLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Merknad:" 
      VIEW-AS TEXT 
     SIZE 9 BY .62 NO-UNDO.

DEFINE VARIABLE PkSdlNr AS CHARACTER FORMAT "X(15)" 
     LABEL "Pakkseddelnr" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE RegistrertAv AS CHARACTER FORMAT "X(10)" 
     LABEL "Av" 
     VIEW-AS FILL-IN 
     SIZE 12.6 BY 1.

DEFINE VARIABLE RegistrertDato AS DATE FORMAT "99/99/9999" 
     LABEL "Registrert" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE SendtDato AS DATE FORMAT "99/99/9999" 
     LABEL "Sendt dato" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE SumFrakt AS DECIMAL FORMAT "->>,>>9.99" INITIAL 0 
     LABEL "Sum frakt" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE RECTANGLE TBPkSdlHode
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 9.8 BY .91.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     ButNamn AT ROW 5.24 COL 30.6 COLON-ALIGNED NO-LABEL
     cEkstIdList AT ROW 7.57 COL 21 COLON-ALIGNED
     cRegTid AT ROW 21.24 COL 37.4 COLON-ALIGNED NO-LABEL
     fiMerknadLabel AT ROW 11.24 COL 11.4 COLON-ALIGNED NO-LABEL
     MeldingFraLev AT ROW 15.76 COL 23 NO-LABEL
     PkSdlStatus AT ROW 2.91 COL 46.2 COLON-ALIGNED
     Merknad AT ROW 11.24 COL 23 NO-LABEL
     cOrdreNrList AT ROW 6.43 COL 21 COLON-ALIGNED
     CL AT ROW 5.24 COL 21 COLON-ALIGNED HELP
          "Sentrallager pakkseddelen er sendt til."
     PkSdlNr AT ROW 2.91 COL 21 COLON-ALIGNED HELP
          "Pakkseddelnummer"
     RegistrertAv AT ROW 21.24 COL 50 COLON-ALIGNED HELP
          "Brukerid på den som registrerte posten"
     RegistrertDato AT ROW 21.24 COL 21 COLON-ALIGNED HELP
          "Dato da posten ble registrert i registeret"
     SendtDato AT ROW 4.05 COL 21 COLON-ALIGNED HELP
          "Dato da varene er sendt fra leverandør."
     fiMeldingLabel AT ROW 15.76 COL 6 COLON-ALIGNED NO-LABEL
     SumFrakt AT ROW 8.81 COL 21 COLON-ALIGNED
     TBPkSdlHode AT ROW 1.24 COL 1.8
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SCROLLABLE SIZE 143.6 BY 24.95.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* SUPPRESS Window definition (used by the UIB) 
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "<insert window title>"
         HEIGHT             = 24.95
         WIDTH              = 143.8
         MAX-HEIGHT         = 57.14
         MAX-WIDTH          = 320
         VIRTUAL-HEIGHT     = 57.14
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
                                                                        */
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME
ASSIGN C-Win = CURRENT-WINDOW.


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
   FRAME-NAME Custom                                                    */
ASSIGN 
       FRAME DEFAULT-FRAME:HEIGHT           = 24.95
       FRAME DEFAULT-FRAME:WIDTH            = 143.6.

/* SETTINGS FOR FILL-IN cEkstIdList IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
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
ON END-ERROR OF C-Win /* <insert window title> */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* <insert window title> */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
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
  PUBLISH "InvalidateHandle".
  IF VALID-HANDLE(hStrekkode)    THEN APPLY "close" TO hStrekkode.
  IF VALID-HANDLE(hArtikkelkort) THEN APPLY "close" TO hArtikkelkort.
  IF VALID-HANDLE(hArtBilde)     THEN APPLY "close" TO hArtBilde.
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

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.
{incl/supptrigg.i hQuery}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ArtikkelKort C-Win 
PROCEDURE ArtikkelKort :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF NOT VALID-HANDLE(hArtikkelkort) THEN 
  RUN w-vartkor.w  PERSIST SET hArtikkelkort (DYNAMIC-FUNCTION("getRecId","ArtBas",hBuffer:BUFFER-FIELD("RowIdent4"):BUFFER-VALUE), "ENDRE," + STRING(THIS-PROCEDURE)).

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
  /* Hide all frames. */
  HIDE FRAME DEFAULT-FRAME.
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
/* IF DYNAMIC-FUNCTION("getCurrentObject") = hBrowse THEN DO:                           */
/*   DYNAMIC-FUNCTION("DoLockWindow",THIS-PROCEDURE:CURRENT-WINDOW).                    */
/*   IF VALID-HANDLE(hArtikkelkort) THEN                                                */
/*     DYNAMIC-FUNCTION("DoLockWindow",hArtikkelkort:CURRENT-WINDOW).                   */
/*   DYNAMIC-FUNCTION("setAttribute",SESSION,"userkeepswindowlocked","yes").            */
/*                                                                                      */
/*   IF hParentBuffer:AVAIL THEN DO:                                                    */
/*     IF hParentBuffer:BUFFER-FIELD("PkSdlStatus"):BUFFER-VALUE LE 10 THEN DO:         */
/*       IF DYNAMIC-FUNCTION("getLinkedObject",hLevAnt,"browseoverlay","from") = ? THEN */
/*         DYNAMIC-FUNCTION("CreateOverlayLink",hBrowse,hLevAnt,"AntLevert").           */
/*     END.                                                                             */
/*     ELSE DO:                                                                         */
/*       IF DYNAMIC-FUNCTION("getLinkedObject",hLevAnt,"browse","from") NE ? THEN DO:   */
/*         DYNAMIC-FUNCTION("DeleteObjectLink",hLevAnt,hBrowse).                        */
/*         hLevAnt:HIDDEN = YES.                                                        */
/*       END.                                                                           */
/*     END.                                                                             */
/*   END.                                                                               */
/* END.                                                                                 */

DYNAMIC-FUNCTION("setAttribute",hToolbar,"disabledevents","").

IF (hParentBuffer:AVAIL AND hParentBuffer:BUFFER-FIELD("PkSdlStatus"):BUFFER-VALUE > 10) OR
   (hFieldMap:AVAIL AND hFieldMap:BUFFER-FIELD("SumFrakt"):BUFFER-VALUE = 0) 
   THEN
  DYNAMIC-FUNCTION("setAttribute",hToolbar,"disabledevents","FordelFrakt").

RUN SUPER.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE editRecord C-Win 
PROCEDURE editRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
RUN ArtikkelKort.

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
  DISPLAY ButNamn cEkstIdList cRegTid fiMerknadLabel MeldingFraLev PkSdlStatus 
          Merknad cOrdreNrList CL PkSdlNr RegistrertAv RegistrertDato SendtDato 
          fiMeldingLabel SumFrakt 
      WITH FRAME DEFAULT-FRAME.
  ENABLE ButNamn cRegTid fiMerknadLabel MeldingFraLev PkSdlStatus Merknad 
         cOrdreNrList CL PkSdlNr RegistrertAv RegistrertDato SendtDato 
         fiMeldingLabel SumFrakt TBPkSdlHode 
      WITH FRAME DEFAULT-FRAME.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FinnStrekkode C-Win 
PROCEDURE FinnStrekkode :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM icStrekkode AS CHAR NO-UNDO.

DEF VAR cArtNrStr AS CHAR NO-UNDO.

cArtNrStr = DYNAMIC-FUNCTION("getFieldValues","Strekkode","WHERE Kode = '" + icStrekkode + "'","ArtikkelNr,StrKode").
IF cArtNrStr NE "" THEN DO:
  bOK = hBuffer:FIND-FIRST("WHERE ArtikkelNr = " + ENTRY(1,cArtNrStr,"|") + " AND StrKode = " + ENTRY(2,cArtNrStr,"|")) NO-ERROR.
  IF bOk THEN DO:
    hBrowse:QUERY:REPOSITION-TO-ROWID(hBuffer:ROWID) NO-ERROR.
    IF NOT ERROR-STATUS:ERROR THEN
      APPLY "value-changed" TO hBrowse.
  END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FordelFraktRecord C-Win 
PROCEDURE FordelFraktRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF DYNAMIC-FUNCTION("DoMessage",0,1,"Bekreft fordeling av " + SumFrakt:SCREEN-VALUE IN FRAME {&FRAME-NAME} 
                                  + " ihht innkjøpspris","","") = 1 THEN
  IF NOT DYNAMIC-FUNCTION("runProc","pksdl_fordel_frakt.p",STRING(hFieldMap:BUFFER-FIELD("PkSdlId"):BUFFER-VALUE),?) THEN
    DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").

{&WINDOW-NAME}:MOVE-TO-TOP().
APPLY "entry" TO FRAME {&FRAME-NAME}.

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
DEF VAR cTabList    AS CHAR   NO-UNDO.
DEF VAR hPageObject AS HANDLE NO-UNDO.
DEF VAR iy          AS INT    NO-UNDO.
DEF VAR hTabFrame   AS HANDLE NO-UNDO.
DEF VAR cProfilNr   AS CHAR   NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:

  DYNAMIC-FUNCTION("NewObject",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE,"procedure").


  ASSIGN PkSdlStatus:DELIMITER = "|"
         PkSdlStatus:LIST-ITEM-PAIRS = DYNAMIC-FUNCTION("getFieldList","SysPara;ParaNr|Parameter1;ParaNr",
                                                        "WHERE SysHId = 5 and SysGr = 25")
         .

  hQuery = DYNAMIC-FUNCTION("NewQuery"
                            ,100
                            ,""
                            ,"PkSdlHode"
                            + ";CL"
                            + ";PkSdlNr"
                            + ";PkSdlStatus"
                            + ";RegistrertAv"
                            + ";RegistrertDato"
                            + ";SendtDato"
                            + ";MeldingFraLev"
                            + ";Merknad"
                            + ";PkSdlId"
                            + ";SumFrakt"
                            + ";+cOrdreNrList|CHARACTER|x(30)|pksdl_ordrelist(PkSdlId)" 
                            + ";+cEkstIdList|CHARACTER|x(30)|pksdl_ekstidlist"
                            + ";+cRegTid|CHARACTER|x(5)|jb_hhmm(RegistrertTid)"
                          + ",Butiker"
                             + ";ButNamn"
                            ,"WHERE false"
                          + ",FIRST Butiker WHERE Butiker.Butik = PkSdlHode.CL NO-LOCK"
                            ,"").
  
  DYNAMIC-FUNCTION("CreateObjectLink",THIS-PROCEDURE,hQuery).

  DYNAMIC-FUNCTION("setAttribute",hQuery,"calcfieldproc","pksdlhode_brwcalc.p").

  hFieldMap = DYNAMIC-FUNCTION("NewFieldMap"
                            ,hQuery
                            ,FRAME {&FRAME-NAME}:HANDLE
                            ,"SumFrakt,Merknad",""
                            ,"CL,PkSdlNr,PkSdlStatus,RegistrertAv,RegistrertDato,SendtDato,MeldingFraLev,ButNamn,cOrdreNrList,cEkstIdlist,cRegTid",""
                            ,"").
  DYNAMIC-FUNCTION("CreateObjectLink",hFieldMap,hQuery).
/*   DYNAMIC-FUNCTION("setAttribute",hQuery,"calcfieldproc","artpris_brwcalc.p"). */

  hToolbar = DYNAMIC-FUNCTION("NewToolbar",
                            TBPkSdlHode:HANDLE,
                            "",
                            "Save"
                          + ",FordelFrakt;Fordel frakt&"
                           ,"maxborder").
  DYNAMIC-FUNCTION("CreateObjectLink",hQuery,hToolbar).
  DYNAMIC-FUNCTION("CreateObjectLink",hFieldMap,hToolbar).

  DYNAMIC-FUNCTION("setNoResizeY", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE,
                                   "MeldingFraLev,Merknad").
END.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InnlevRecord C-Win 
PROCEDURE InnlevRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF hLevAnt:MODIFIED THEN
  APPLY "return" TO hLevAnt.
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
FRAME {&FRAME-NAME}:MOVE-TO-TOP().
APPLY "entry" TO FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PrevNext C-Win 
PROCEDURE PrevNext :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER icDir AS CHARACTER  NO-UNDO.

IF CAN-DO("Prev,Next",icDir) THEN DO:
  IF icDir = "Prev" THEN
    hBrowse:SELECT-PREV-ROW().
  ELSE
    hBrowse:SELECT-NEXT-ROW().
  APPLY "value-changed" TO hBrowse.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RowDisplayBrowse C-Win 
PROCEDURE RowDisplayBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
ASSIGN hGyldigKodeCol:FONT         = iFontWingdings
       hGyldigKodeCol:FORMAT       = CHR(254) + "/"  + CHR(168)
       .
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StrekkodeRecord C-Win 
PROCEDURE StrekkodeRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF NOT VALID-HANDLE(hStrekkode) THEN
  RUN Strekkode.w PERSIST SET hStrekkode (THIS-PROCEDURE).

RUN MoveToTop IN hStrekkode.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ValueChangedField C-Win 
PROCEDURE ValueChangedField :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM icFieldName AS CHAR NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
  IF icFieldName = "OverstyrPris" THEN DO:      
    DYNAMIC-FUNCTION("setCurrentObject",hQuery).
    RUN SaveRecord.
  END.
END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getFrameHandle C-Win 
FUNCTION getFrameHandle RETURNS HANDLE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

RETURN FRAME {&FRAME-NAME}:HANDLE. 

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
DYNAMIC-FUNCTION("setNoResizeY", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE,
                                 "MeldingFraLev,Merknad").

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setParent C-Win 
FUNCTION setParent RETURNS LOGICAL
  ( INPUT ihParent AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  hParent = ihParent.

  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setParentQuery C-Win 
FUNCTION setParentQuery RETURNS LOGICAL
  ( INPUT ihParentQuery AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
hParentBrowse = ihParentQuery.
hParentBuffer = hParentBrowse:QUERY:GET-BUFFER-HANDLE(1).
  
RETURN YES.


END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setQuery C-Win 
FUNCTION setQuery RETURNS LOGICAL
  (INPUT ihQuery AS HANDLE) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
ASSIGN hParentBrowse = ihQuery
       hParentBuffer = ihQuery:QUERY:GET-BUFFER-HANDLE(1).
RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

