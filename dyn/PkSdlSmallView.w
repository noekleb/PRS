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

DEF VAR bAllowCreate      AS LOG    NO-UNDO INIT YES.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Ordretype SendtFraLagerTilOutlet PkSdlOpphav ~
PkSdlNr SendtDato EkstId SumFrakt fiMerknadLabel CL ButNamn levnr levnamn ~
cEkstIdList cRegTid Merknad MeldingFraLev cOrdreNrList RegistrertAv ~
RegistrertDato fiMeldingLabel PkSdlId btnlevnr TBPkSdlHode 
&Scoped-Define DISPLAYED-OBJECTS Ordretype SendtFraLagerTilOutlet ~
PkSdlOpphav PkSdlNr SendtDato PkSdlStatus EkstId SumFrakt fiMerknadLabel CL ~
ButNamn levnr levnamn cEkstIdList cRegTid Merknad MeldingFraLev ~
cOrdreNrList RegistrertAv RegistrertDato fiMeldingLabel PkSdlId 

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
DEFINE BUTTON btnlevnr 
     LABEL "..." 
     SIZE 4 BY 1.

DEFINE VARIABLE PkSdlStatus AS CHARACTER FORMAT "X(256)":U 
     LABEL "Status" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 20 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE MeldingFraLev AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 44 BY 2.71 NO-UNDO.

DEFINE VARIABLE Merknad AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 44 BY 2.33 NO-UNDO.

DEFINE VARIABLE ButNamn AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 34 BY 1 NO-UNDO.

DEFINE VARIABLE cEkstIdList AS CHARACTER FORMAT "X(256)":U 
     LABEL "Eksterne ordre" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 45 BY 1 NO-UNDO.

DEFINE VARIABLE CL AS INTEGER FORMAT ">>>>>9" INITIAL 0 
     LABEL "Sentrallager" 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1.

DEFINE VARIABLE cOrdreNrList AS CHARACTER FORMAT "X(256)":U 
     LABEL "Ordrenumre" 
     VIEW-AS FILL-IN 
     SIZE 45 BY 1 NO-UNDO.

DEFINE VARIABLE cRegTid AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 8.2 BY 1 NO-UNDO.

DEFINE VARIABLE EkstId AS CHARACTER FORMAT "X(15)" 
     LABEL "Eksternt Id" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 TOOLTIP "Kobllingsfelt for å koble til ekstern ordre.".

DEFINE VARIABLE fiMeldingLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Melding fra lev:" 
      VIEW-AS TEXT 
     SIZE 14.4 BY .62 NO-UNDO.

DEFINE VARIABLE fiMerknadLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Merknad:" 
      VIEW-AS TEXT 
     SIZE 9 BY .62 NO-UNDO.

DEFINE VARIABLE levnamn AS CHARACTER FORMAT "x(30)" 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1.

DEFINE VARIABLE levnr AS INTEGER FORMAT "zzzzz9" INITIAL 0 
     LABEL "Leverandør" 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1.

DEFINE VARIABLE Ordretype AS CHARACTER FORMAT "X(256)":U 
     LABEL "Ordretype" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE PkSdlId AS DECIMAL FORMAT ">>>>>>>>>>>>9" INITIAL 0 
     LABEL "Id" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1.

DEFINE VARIABLE PkSdlNr AS CHARACTER FORMAT "X(15)" 
     LABEL "Pakkseddelnr" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE PkSdlOpphav AS INTEGER FORMAT ">9":U INITIAL 0 
     LABEL "Opphav" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 TOOLTIP "Fra hvor pakkseddelen eropprettet." NO-UNDO.

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

DEFINE VARIABLE SendtFraLagerTilOutlet AS DATETIME FORMAT "99/99/99 HH:MM:SS":U 
     LABEL "Sendt outlet" 
     VIEW-AS FILL-IN 
     SIZE 17.8 BY 1 TOOLTIP "Når pakkseddel ble sendt Outlet." NO-UNDO.

DEFINE VARIABLE SumFrakt AS DECIMAL FORMAT "->>,>>9.99" INITIAL 0 
     LABEL "Sum frakt" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE RECTANGLE TBPkSdlHode
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 9.8 BY .91.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     Ordretype AT ROW 11.81 COL 15 COLON-ALIGNED
     SendtFraLagerTilOutlet AT ROW 10.71 COL 42.2 COLON-ALIGNED
     PkSdlOpphav AT ROW 10.71 COL 15 COLON-ALIGNED
     PkSdlNr AT ROW 2.62 COL 15 COLON-ALIGNED HELP
          "Pakkseddelnummer"
     SendtDato AT ROW 3.76 COL 15 COLON-ALIGNED HELP
          "Dato da varene er sendt fra leverandør."
     PkSdlStatus AT ROW 3.76 COL 40.2 COLON-ALIGNED
     EkstId AT ROW 5.05 COL 15 COLON-ALIGNED
     SumFrakt AT ROW 5.05 COL 44 COLON-ALIGNED
     fiMerknadLabel AT ROW 13.05 COL 5.4 COLON-ALIGNED NO-LABEL
     CL AT ROW 6.24 COL 15 COLON-ALIGNED HELP
          "Sentrallager pakkseddelen er sendt til."
     ButNamn AT ROW 6.24 COL 26 COLON-ALIGNED NO-LABEL
     levnr AT ROW 7.33 COL 15 COLON-ALIGNED
     levnamn AT ROW 7.33 COL 30 COLON-ALIGNED NO-LABEL
     cEkstIdList AT ROW 9.57 COL 15 COLON-ALIGNED
     cRegTid AT ROW 18.14 COL 31.4 COLON-ALIGNED NO-LABEL
     Merknad AT ROW 12.91 COL 17 NO-LABEL
     MeldingFraLev AT ROW 15.33 COL 17 NO-LABEL
     cOrdreNrList AT ROW 8.43 COL 15 COLON-ALIGNED
     RegistrertAv AT ROW 18.14 COL 44 COLON-ALIGNED HELP
          "Brukerid på den som registrerte posten"
     RegistrertDato AT ROW 18.14 COL 15 COLON-ALIGNED HELP
          "Dato da posten ble registrert i registeret"
     fiMeldingLabel AT ROW 14.48 COL 2 NO-LABEL
     PkSdlId AT ROW 2.62 COL 40.2 COLON-ALIGNED HELP
          "Internt pakkseddelid."
     btnlevnr AT ROW 7.33 COL 28 NO-TAB-STOP 
     TBPkSdlHode AT ROW 1.24 COL 1.8
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 62.2 BY 18.33.


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
         HEIGHT             = 18.52
         WIDTH              = 63.6
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
       cEkstIdList:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN fiMeldingLabel IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
ASSIGN 
       Ordretype:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       PkSdlOpphav:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR COMBO-BOX PkSdlStatus IN FRAME DEFAULT-FRAME
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


&Scoped-define SELF-NAME btnlevnr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnlevnr C-Win
ON CHOOSE OF btnlevnr IN FRAME DEFAULT-FRAME /* ... */
DO:
  DEF VAR cReturnValues   AS CHAR NO-UNDO.
  DEF VAR bOk             AS LOG  NO-UNDO.

  /*
  MESSAGE VALID-HANDLE(THIS-PROCEDURE) CAN-FIND(FIRST LevBas)
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxLookup.w (THIS-PROCEDURE,40,
                    "LevBas"
                    + ";LevNr"
                    + ";LevNamn"
                   ,"WHERE true"
                    ,""                                                  
                    ,"LevNr",   /* <- return values for these fields */
                    OUTPUT cReturnValues,
                    OUTPUT bOK).


  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.
  */
  cReturnValues = "LevNamn;LevNr".

  RUN JBoxDLookup.w ("LevBas;LevNamn;LevNr", 
                     "WHERE true",
                     INPUT-OUTPUT cReturnValues).

  
  IF /*bOk AND */ cReturnValues NE "" THEN DO:
    ASSIGN levnr:SCREEN-VALUE = ENTRY(2,cReturnValues,"|")
           levnamn:SCREEN-VALUE = ENTRY(1,cReturnValues,"|")
           .

    APPLY "any-printable" TO levnr.
  END.
  THIS-PROCEDURE:CURRENT-WINDOW:MOVE-TO-TOP().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME levnr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL levnr C-Win
ON F3 OF levnr IN FRAME DEFAULT-FRAME /* Leverandør */
DO:
 APPLY "choose" TO btnLevnr.
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
  IF VALID-HANDLE(hStrekkode)    THEN APPLY "close" TO hStrekkode.
  IF VALID-HANDLE(hArtikkelkort) THEN APPLY "close" TO hArtikkelkort.
  IF VALID-HANDLE(hArtBilde)     THEN APPLY "close" TO hArtBilde.
  RUN disable_UI.
  PUBLISH "InvalidateHandle".
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

DO WITH FRAME DEFAULT-FRAME:
    DYNAMIC-FUNCTION("setAttribute",hToolbar,"disabledevents","").

    IF (hParentBuffer:AVAIL AND hParentBuffer:BUFFER-FIELD("PkSdlStatus"):BUFFER-VALUE > 10) OR
       (hFieldMap:AVAIL AND hFieldMap:BUFFER-FIELD("SumFrakt"):BUFFER-VALUE = 0) 
       THEN
      DYNAMIC-FUNCTION("setAttribute",hToolbar,"disabledevents","FordelFrakt").


RUN SUPER.

PkSdlStatus:SENSITIVE = FALSE.
OrdreType:SENSITIVE = FALSE.

END.

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
  DISPLAY Ordretype SendtFraLagerTilOutlet PkSdlOpphav PkSdlNr SendtDato 
          PkSdlStatus EkstId SumFrakt fiMerknadLabel CL ButNamn levnr levnamn 
          cEkstIdList cRegTid Merknad MeldingFraLev cOrdreNrList RegistrertAv 
          RegistrertDato fiMeldingLabel PkSdlId 
      WITH FRAME DEFAULT-FRAME.
  ENABLE Ordretype SendtFraLagerTilOutlet PkSdlOpphav PkSdlNr SendtDato EkstId 
         SumFrakt fiMerknadLabel CL ButNamn levnr levnamn cEkstIdList cRegTid 
         Merknad MeldingFraLev cOrdreNrList RegistrertAv RegistrertDato 
         fiMeldingLabel PkSdlId btnlevnr TBPkSdlHode 
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
         /* Tilpass denne: 
         bAllowCreate = DYNAMIC-FUNCTION("getFieldList","SysPara;ParaNr|Parameter1;ParaNr",
                                                        "WHERE SysHId = 5 and SysGr = 25") = 1
         */
         btnLevnr:HIDDEN = NOT bAllowCreate
         .

  IF NOT bAllowCreate THEN 
    ASSIGN levNamn:X = butNamn:X 
           levNamn:WIDTH-CHARS = butNamn:WIDTH-CHARS
           .
  ELSE 
    DYNAMIC-FUNCTION("setAttribute",SESSION,"allowPkSdlCreate","yes"). /* Setter denne her slik at den kan plukkes opp fra varevedl */

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
                            + ";EkstId"
                            + ";SumFrakt"
                            + ";+cOrdreNrList|CHARACTER|x(30)|pksdl_ordrelist(PkSdlId)" 
                            + ";+cEkstIdList|CHARACTER|x(30)|pksdl_ekstidlist"
                            + ";+cRegTid|CHARACTER|x(5)|jb_hhmm(RegistrertTid)"
                            + ";levnr"
                            + ";levnamn"
                            + ";PkSdlOpphav"
                            + ";OrdreType"
                            + ";SendtFraLagerTilOutlet"
                          + ",Butiker"
                             + ";ButNamn"
                            ,"WHERE false"
                          + ",FIRST Butiker WHERE Butiker.Butik = PkSdlHode.CL NO-LOCK OUTER-JOIN"
                            ,"").
  
  DYNAMIC-FUNCTION("CreateObjectLink",THIS-PROCEDURE,hQuery).

  DYNAMIC-FUNCTION("setAttribute",hQuery,"calcfieldproc","pksdlhode_brwcalc.p").

  hFieldMap = DYNAMIC-FUNCTION("NewFieldMap",
                            hQuery,
                            FRAME {&FRAME-NAME}:HANDLE,
                            "PkSdlNr,EkstId,SumFrakt,SendtDato,CL,levnr,levnamn,Merknad,MeldingFraLev,PkSdlStatus,OrdreType",
                            "",
                            "PkSdlId,RegistrertAv,RegistrertDato,ButNamn,cOrdreNrList,cRegTid,PkSdlOpphav,SendtFraLagerTilOutlet",
                            "",
                            ""
                            ).
  DYNAMIC-FUNCTION("CreateObjectLink",hFieldMap,hQuery).
/*   DYNAMIC-FUNCTION("setAttribute",hQuery,"calcfieldproc","artpris_brwcalc.p"). */

  hToolbar = DYNAMIC-FUNCTION("NewToolbar",
                            TBPkSdlHode:HANDLE,
                            "",
                            (IF bAllowCreate THEN "New;Ny,Undo;Angre," ELSE "") + "Save;Lagre"
/*                           + ",FordelFrakt;Fordel frakt&"  */
                           ,IF bAllowCreate THEN "btnLevnr" ELSE "").
  DYNAMIC-FUNCTION("CreateObjectLink",hQuery,hToolbar).
  DYNAMIC-FUNCTION("CreateObjectLink",hFieldMap,hToolbar).

  DYNAMIC-FUNCTION("setNoResizeY", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE,
                                   "MeldingFraLev,Merknad").
  PkSdlStatus:SENSITIVE = FALSE.
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
    WHEN "CL" THEN butnamn:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldValues","Butiker","WHERE butik = " + ihField:SCREEN-VALUE,"butnamn").
    WHEN "levnr" THEN levnamn:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldValues","LevBas","WHERE levnr = " + ihField:SCREEN-VALUE,"levnamn").
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
FRAME {&FRAME-NAME}:MOVE-TO-TOP().
APPLY "entry" TO FRAME {&FRAME-NAME}.

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

DO WITH FRAME {&FRAME-NAME}:
  ASSIGN PkSdlStatus:SCREEN-VALUE = "10"
         Ordretype:SCREEN-VALUE = '90'
         SendtDato:SCREEN-VALUE = STRING(TODAY)
         CL:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldValues","SysPara",
                            "WHERE SysHId = 5 and SysGr = 1 and ParaNr = 1","Parameter1")
         OrdreType:SENSITIVE = FALSE 
         PkSdlStatus:SENSITIVE = FALSE
         .
  RUN LeaveOfFieldHandle (CL:HANDLE).
END.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SaveRecord C-Win 
PROCEDURE SaveRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
RUN SUPER.
IF VALID-HANDLE(hParentBrowse) THEN
  DYNAMIC-FUNCTION("RefreshRowids",hParentBrowse,hFieldMap:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE).
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

