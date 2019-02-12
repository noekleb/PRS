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

DEF VAR bOk                    AS LOG  NO-UNDO.
DEF VAR ix                     AS INT  NO-UNDO.
DEF VAR iReturn                AS INT  NO-UNDO.
DEF VAR hParent                AS HANDLE NO-UNDO.
                               
DEF VAR hParentBuffer          AS HANDLE NO-UNDO.
DEF VAR hParentQuery           AS HANDLE NO-UNDO.
DEF VAR hBrowse                AS HANDLE NO-UNDO.
DEF VAR hFieldMap              AS HANDLE NO-UNDO.
DEF VAR hToolbar               AS HANDLE NO-UNDO.
DEF VAR hTabFolder             AS HANDLE NO-UNDO.
                               
DEF VAR hCurrTabProc           AS HANDLE NO-UNDO.
DEF VAR hCurrTabFrame          AS HANDLE NO-UNDO.
DEF VAR iCurrTab               AS INT NO-UNDO.
                               
                               
DEF VAR hLinjeNrOverlay        AS HANDLE NO-UNDO.
DEF VAR hVareNrOverlay         AS HANDLE NO-UNDO.
DEF VAR hVareTekstOverlay      AS HANDLE NO-UNDO.
DEF VAR hStorlOverlay          AS HANDLE NO-UNDO.
DEF VAR hAntallOverlay         AS HANDLE NO-UNDO.
DEF VAR hLinjeRabOverlay       AS HANDLE NO-UNDO.
DEF VAR hNettoPrisOverlay      AS HANDLE NO-UNDO.
DEF VAR hMomsKodOverlay        AS HANDLE NO-UNDO.
                               
DEF VAR hArtBasSok             AS HANDLE NO-UNDO.
DEF VAR hTilbField             AS HANDLE NO-UNDO.
DEF VAR hPrisColumn            AS HANDLE NO-UNDO.
DEF VAR hAntallColumn          AS HANDLE NO-UNDO.
                               
DEF VAR bNew                   AS LOG    NO-UNDO.
DEF VAR bEnableEdit            AS LOG    NO-UNDO.
DEF VAR bCurrEditEnable        AS LOG    NO-UNDO.

DEF VAR hLagerStatus           AS HANDLE NO-UNDO.
DEF VAR hLagerStatusFrame      AS HANDLE NO-UNDO.
DEF VAR hLagerStatusOCXframe   AS HANDLE NO-UNDO.

DEF VAR hFiRekvNr              AS HANDLE NO-UNDO.
DEF VAR hFiRekvTekst           AS HANDLE NO-UNDO.
DEF VAR cNyRekv                AS CHAR   NO-UNDO.
DEF VAR iPakkeIdx              AS INT    NO-UNDO.
DEF VAR cDeselectedRows        AS CHAR   NO-UNDO.
DEF VAR bSelectRekv            AS LOG    NO-UNDO.
DEF VAR hSelectSrcBrowse       AS HANDLE NO-UNDO.
DEF VAR iNumPakkeVarer         AS INT    NO-UNDO.
DEF VAR iOrgNumPakkeVarer      AS INT    NO-UNDO.
DEF VAR hSelectSrcTilbField    AS HANDLE NO-UNDO.
DEF VAR hTargetSrcTilbField    AS HANDLE NO-UNDO.

DEF VAR hArtikkelkort          AS HANDLE NO-UNDO.
DEF VAR hPakkePris             AS HANDLE NO-UNDO.
DEF VAR cPakkePris             AS CHAR   NO-UNDO.
DEF VAR cMomsKosList           AS CHAR   NO-UNDO.

DEFINE TEMP-TABLE tt_Dummy NO-UNDO /* för att användas vid antop av asWebPlock. måste ha en handle */
    FIELD a AS INTE.

{ttKOrdre.i &Shared=SHARED}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME frmFaktLinje

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BrwKOrdreLinje rectToolbar rectLagerStatus ~
rsLagerStatusSalg Varespesifikasjon ArbeidsBeskr RefNr RefTekst MomsKode ~
Leveringsdato fiSumOrdreEksMva fiSumOrdreDbLabel fiSumOrdreDb%Label ~
fiSumRabattKrLabel fiSumOrdreLabel fiSumOrdreEksMvaLabel 
&Scoped-Define DISPLAYED-OBJECTS rsLagerStatusSalg Varespesifikasjon ~
ArbeidsBeskr RefNr RefTekst MomsKode Leveringsdato fiSumOrdreDb ~
fiSumOrdreDb% fiSumRabattKr fiSumOrdre fiSumOrdreEksMva fiSumOrdreDbLabel ~
fiSumOrdreDb%Label fiSumRabattKrLabel fiSumOrdreLabel fiSumOrdreEksMvaLabel 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD AddStr C-Win 
FUNCTION AddStr RETURNS LOGICAL
  ( INPUT ifArtikkelNr AS DEC,
    INPUT icStorl      AS CHAR,
    INPUT ifPlukkAnt   AS DEC,
    INPUT icAction     AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD AdjustBrowseColumns C-Win 
FUNCTION AdjustBrowseColumns RETURNS LOGICAL
  ( INPUT ihBrowse     AS HANDLE,
    INPUT icBrowseName AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getCurrentAntall C-Win 
FUNCTION getCurrentAntall RETURNS LOGICAL
  ( INPUT ihBuffer AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getFrameHandle C-Win 
FUNCTION getFrameHandle RETURNS HANDLE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getLocalQueryMaxValue C-Win 
FUNCTION getLocalQueryMaxValue RETURNS CHARACTER
  ( INPUT ihBrowseOrQuery AS HANDLE,
    INPUT icField         AS CHAR,
    INPUT ifIncrement     AS DEC )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getMyQueryStat C-Win 
FUNCTION getMyQueryStat RETURNS CHARACTER
  ( INPUT ihDataObject    AS HANDLE,
    INPUT icStatFields    AS CHAR,
    INPUT icCriteria      AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getParentBrowse C-Win 
FUNCTION getParentBrowse RETURNS HANDLE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getStat C-Win 
FUNCTION getStat RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getToolbarHandle C-Win 
FUNCTION getToolbarHandle RETURNS HANDLE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD InitializeResize C-Win 
FUNCTION InitializeResize RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD initOverlays C-Win 
FUNCTION initOverlays RETURNS LOGICAL
  ( INPUT ibEnable AS LOG)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setParent C-Win 
FUNCTION setParent RETURNS LOGICAL
  ( INPUT ihParent AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setQuery C-Win 
FUNCTION setQuery RETURNS LOGICAL
  (INPUT ihQuery AS HANDLE)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setStrKode C-Win 
FUNCTION setStrKode RETURNS LOGICAL
  ( INPUT icStorl AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setVareNr C-Win 
FUNCTION setVareNr RETURNS LOGICAL
  ( INPUT icVareNr AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD VisLagerStatus C-Win 
FUNCTION VisLagerStatus RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE MomsKode AS CHARACTER FORMAT "X(256)":U 
     LABEL "Mva" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 19.8 BY 1 NO-UNDO.

DEFINE VARIABLE ArbeidsBeskr AS CHARACTER FORMAT "X(256)":U 
     LABEL "Merknad" 
     VIEW-AS FILL-IN 
     SIZE 56.8 BY 1 TOOLTIP "Merknad" NO-UNDO.

DEFINE VARIABLE fiSumOrdre AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 17.8 BY .86 NO-UNDO.

DEFINE VARIABLE fiSumOrdreDb AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 15 BY .86 NO-UNDO.

DEFINE VARIABLE fiSumOrdreDb% AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 8.8 BY .86 NO-UNDO.

DEFINE VARIABLE fiSumOrdreDb%Label AS CHARACTER FORMAT "X(256)":U INITIAL "Db%:" 
      VIEW-AS TEXT 
     SIZE 7.4 BY .62 NO-UNDO.

DEFINE VARIABLE fiSumOrdreDbLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Sum DbKr:" 
      VIEW-AS TEXT 
     SIZE 14 BY .62 NO-UNDO.

DEFINE VARIABLE fiSumOrdreEksMva AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 18.2 BY .86 NO-UNDO.

DEFINE VARIABLE fiSumOrdreEksMvaLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Sum eks.mva:" 
      VIEW-AS TEXT 
     SIZE 14 BY .62 NO-UNDO.

DEFINE VARIABLE fiSumOrdreLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Totalsum:" 
      VIEW-AS TEXT 
     SIZE 14 BY .62 NO-UNDO.

DEFINE VARIABLE fiSumRabattKr AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 17.8 BY .86 NO-UNDO.

DEFINE VARIABLE fiSumRabattKrLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Sum rabatt:" 
      VIEW-AS TEXT 
     SIZE 14 BY .62 NO-UNDO.

DEFINE VARIABLE Leveringsdato AS DATE FORMAT "99/99/9999":U 
     LABEL "Levert" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE RefNr AS INTEGER FORMAT "->,>>>,>>9" INITIAL 0 
     LABEL "Rekv.nr" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE RefTekst AS CHARACTER FORMAT "X(40)" 
     VIEW-AS FILL-IN 
     SIZE 40.2 BY 1 TOOLTIP "Referansetekst (rekvisisjonstekst)".

DEFINE VARIABLE Varespesifikasjon AS CHARACTER FORMAT "X(256)":U 
     LABEL "Varefakta" 
     VIEW-AS FILL-IN 
     SIZE 56.8 BY 1 TOOLTIP "Varespesifikasjon" NO-UNDO.

DEFINE VARIABLE rsLagerStatusSalg AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Lagerstatus (lager/i best)", 1,
"Salg", 2
     SIZE 38.2 BY .67 NO-UNDO.

DEFINE RECTANGLE BrwKOrdreLinje
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 139 BY 5.33.

DEFINE RECTANGLE rectLagerStatus
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 71 BY 2.57.

DEFINE RECTANGLE rectToolbar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 17 BY 1.19.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME frmFaktLinje
     rsLagerStatusSalg AT ROW 7.86 COL 69.8 NO-LABEL
     Varespesifikasjon AT ROW 7.95 COL 10.2 COLON-ALIGNED HELP
          "Varespesifikasjon"
     ArbeidsBeskr AT ROW 9 COL 10.2 COLON-ALIGNED HELP
          "Merknad"
     RefNr AT ROW 10.05 COL 10.2 COLON-ALIGNED HELP
          "Referansenr (Normalt til rekvisisjon eller ordre)"
     RefTekst AT ROW 10.05 COL 26.8 COLON-ALIGNED HELP
          "Referansetekst. Ofte i forbindelse med rekvisisjon o.l." NO-LABEL
     MomsKode AT ROW 11.14 COL 10.2 COLON-ALIGNED
     Leveringsdato AT ROW 11.14 COL 37.8 COLON-ALIGNED
     fiSumOrdreDb AT ROW 11.95 COL 58.4 COLON-ALIGNED NO-LABEL NO-TAB-STOP 
     fiSumOrdreDb% AT ROW 11.95 COL 74 COLON-ALIGNED NO-LABEL NO-TAB-STOP 
     fiSumRabattKr AT ROW 11.95 COL 83.2 COLON-ALIGNED NO-LABEL NO-TAB-STOP 
     fiSumOrdre AT ROW 11.95 COL 101.6 COLON-ALIGNED NO-LABEL NO-TAB-STOP 
     fiSumOrdreEksMva AT ROW 11.95 COL 120.2 COLON-ALIGNED NO-LABEL NO-TAB-STOP 
     fiSumOrdreDbLabel AT ROW 11.33 COL 58.4 COLON-ALIGNED NO-LABEL
     fiSumOrdreDb%Label AT ROW 11.33 COL 74.2 COLON-ALIGNED NO-LABEL
     fiSumRabattKrLabel AT ROW 11.33 COL 83.2 COLON-ALIGNED NO-LABEL
     fiSumOrdreLabel AT ROW 11.33 COL 101.6 COLON-ALIGNED NO-LABEL
     fiSumOrdreEksMvaLabel AT ROW 11.33 COL 120.4 COLON-ALIGNED NO-LABEL
     BrwKOrdreLinje AT ROW 2.43 COL 1.6
     rectToolbar AT ROW 1.19 COL 1.6
     rectLagerStatus AT ROW 8.52 COL 69.6
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 140.4 BY 11.81.


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
/* SUPPRESS Window definition (used by the UIB) 
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "<insert window title>"
         HEIGHT             = 11.81
         WIDTH              = 140.2
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
/* SETTINGS FOR FRAME frmFaktLinje
   FRAME-NAME                                                           */
/* SETTINGS FOR FILL-IN fiSumOrdre IN FRAME frmFaktLinje
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiSumOrdreDb IN FRAME frmFaktLinje
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiSumOrdreDb% IN FRAME frmFaktLinje
   NO-ENABLE                                                            */
ASSIGN 
       fiSumOrdreEksMva:READ-ONLY IN FRAME frmFaktLinje        = TRUE.

/* SETTINGS FOR FILL-IN fiSumRabattKr IN FRAME frmFaktLinje
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME frmFaktLinje
/* Query rebuild information for FRAME frmFaktLinje
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME frmFaktLinje */
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


&Scoped-define SELF-NAME rsLagerStatusSalg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rsLagerStatusSalg C-Win
ON VALUE-CHANGED OF rsLagerStatusSalg IN FRAME frmFaktLinje
DO:
  VisLagerStatus().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Varespesifikasjon
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Varespesifikasjon C-Win
ON BACK-TAB OF Varespesifikasjon IN FRAME frmFaktLinje /* Varefakta */
DO:
  APPLY "entry" TO hBrowse.
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
  IF VALID-HANDLE(hArtBasSok) THEN APPLY "close" TO hArtBasSok.
  IF VALID-HANDLE(hLagerstatus) THEN APPLY "close" TO hLagerstatus.
  IF VALID-HANDLE(hArtikkelkort) THEN APPLY "close" TO hArtikkelkort.
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
{incl/supptrigg.i hBrowse}

ON 'alt-s':U OF FRAME {&FRAME-NAME} ANYWHERE DO:
  PUBLISH "AltSKundeOrdre" ("kundeordre").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AltSKundeOrdre C-Win 
PROCEDURE AltSKundeOrdre :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM icParam AS CHAR NO-UNDO.

IF icParam = "kundeordre" THEN
  RUN HentArtRecord.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BestillingRecord C-Win 
PROCEDURE BestillingRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cPkVareList  AS CHAR  NO-UNDO.
DEF VAR cRowIdList   AS CHAR  NO-UNDO.
DEF VAR iReturn      AS INT   NO-UNDO.
DEF VAR fNettoSum    AS DEC   NO-UNDO.
DEF VAR cFordeling   AS CHAR  NO-UNDO.
DEF VAR iNyPakke     AS INT   NO-UNDO.
DEF VAR cRepos       AS CHAR  NO-UNDO.

THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = NO.

RUN JBoxSelector.w (THIS-PROCEDURE,0,
                    "KOrdreLinje"  
                    + ";KOrdreLinjeNr|Lnr"
                    + ";VareNr|Varenr"
                    + ";VareTekst|Varetekst"
                    + ";Storl"
                    + ";Pris"
                    + ";!KundeRab%|Knd.rab%"
                    + ";Tilbud|T|*/"
                    + ";!BruttoPris|Beste pris"
                    + ";Antall|Ant"
                    + ";!LinjeRab%|LinjeRab%"
                    + ";!NettoPris|Nto.pris"
                    + ";NettoLinjesum|Sum"
                    + ";!+SumEksMvaKr|DECIMAL|->>><>>9.99|kordrelinje_sumeksmvakr.p|Sum eks MVA"
                    + ";!Db%"
                    + ";!DbKr"
                    + ";!Mva%"
                    + ";!KundeRabattKr|Tot.kunderab"
                    + ";!OrdreRabattKr|Tot.ordrerab"
                    + ";!LinjeRabattKr|Tot.linjerab"
                    + ";!Varekost"
                    + ";!Pakkeidx"
                    + ";!Leveringsdato"
                    + ";!KOrdre_id"
                    ,DYNAMIC-FUNCTION("getAttribute",hBrowse,"basequery") 
                  + " AND CAN-DO('0," + STRING(iPakkeIdx) + "',STRING(Pakkeidx))" 
                  + " AND Leveringsdato = ?"
                    ,INPUT-OUTPUT cRowIdList,
                    "KOrdreLinjeNr",
                    INPUT-OUTPUT cPkVareList,
                    "","",
                    OUTPUT bOK).

THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = YES.


IF bOk AND (DEC(cPakkePris) NE 0 OR iNumPakkeVarer = 0) THEN DO:
  IF DYNAMIC-FUNCTION("runproc","kordrelinje_kalkpakke.p","prod;" + cPakkePris + ";" + cRowIdList + ";1;" 
                                + STRING(iPakkeIdx) + ";"
                                + STRING(hFieldMap:BUFFER-FIELD("KOrdre_id"):BUFFER-VALUE),?) THEN DO:
    DYNAMIC-FUNCTION("setCurrentObject",hParentQuery).
    RUN DisplayRecord IN hParent.
  END.
  ELSE DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").
END.
ELSE IF DEC(cPakkePris) = 0 AND iNumPakkeVarer NE iOrgNumPakkeVarer THEN
  DYNAMIC-FUNCTION("DoMessage",0,0,"Pakkepris eller sammensetning ble ikke endret pga at ny pris ikke var satt","Informasjon","").


DYNAMIC-FUNCTION("setCurrentObject",hBrowse).
RUN OpenQuery.
IF cRepos NE "" THEN DO:
  bOK = hFieldMap:FIND-FIRST("WHERE RowIdent1 = '" + cRepos + "'") NO-ERROR.
  IF bOK THEN
    hBrowse:QUERY:REPOSITION-TO-ROWID(hFieldMap:ROWID).
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BrowseColumnLookup C-Win 
PROCEDURE BrowseColumnLookup :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR hCurrObject AS HANDLE NO-UNDO.

hCurrObject = DYNAMIC-FUNCTION("getCurrentObject").

IF hCurrObject = hStorlOverlay THEN DO:
/*   IF hFieldMap:BUFFER-FIELD("VareNr"):BUFFER-VALUE NE "" THEN                                                                                                                   */
/*     DYNAMIC-FUNCTION("setAttribute",                                                                                                                                            */
/*                      hStorlOverlay,                                                                                                                                             */
/*                      "querycriteria",                                                                                                                                           */
/*                      "WHERE false"                                                                                                                                              */
/* /*                      "WHERE StrTypeId = " + DYNAMIC-FUNCTION("getFieldValues","ArtBas","WHERE ArtikkelNr = " + hFieldMap:BUFFER-FIELD("VareNr"):BUFFER-VALUE,"StrTypeId") */ */
/*                      + ",EACH StrTStr OF StrType NO-LOCK"                                                                                                                       */
/* /*                    + " BY SoStorl" */                                                                                                                                        */
/*                      ).                                                                                                                                                         */
/*                                                                                                                                                                                 */
/*   ELSE DO:                                                                                                                                                                      */
  IF hFieldMap:BUFFER-FIELD("VareNr"):BUFFER-VALUE = "" THEN DO:
    DYNAMIC-FUNCTION("DoMessage",0,0,"Størrelse kan bare velges hvis artikkelnr er valgt","","").
    RETURN.
  END.

  RUN SUPER.
  
  IF hCurrObject = hVareNrOverlay THEN
    setVareNr(hCurrObject:SCREEN-VALUE).
  ELSE IF hCurrObject = hStorlOverlay THEN
    setStrKode(hCurrObject:SCREEN-VALUE).
END.
ELSE IF hCurrObject = hVareNrOverlay THEN DO:
  IF VALID-HANDLE(hArtBasSok) THEN APPLY "close" TO hArtBasSok.

  RUN ArtBasSok.w PERSIST SET hArtBasSok.
  DYNAMIC-FUNCTION("setButikkNr" IN hArtBasSok,hParentBuffer:BUFFER-FIELD("ButikkNr"):BUFFER-VALUE).
  DYNAMIC-FUNCTION("setOrdreId"  IN hArtBasSok,hParentBuffer:BUFFER-FIELD("KOrdre_Id"):BUFFER-VALUE).
  DYNAMIC-FUNCTION("setCloseOnSelect" IN hArtBasSok,YES).
  DYNAMIC-FUNCTION("setUpdateCurrentRow" IN hArtBasSok,YES).
  RUN InitializeObject IN hArtBasSok.

  RUN MoveToTop IN hArtBasSok.
END.
ELSE RUN SUPER.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CopyRecord C-Win 
PROCEDURE CopyRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR iNextLnr AS INT NO-UNDO.

IF NOT hFieldMap:AVAIL THEN RETURN.

DYNAMIC-FUNCTION("setAttribute",hFieldMap,"bufferextrafields","KOrdreLinjeNr"
               + (IF hFieldMap:BUFFER-FIELD("Leveringsdato"):BUFFER-VALUE NE ? THEN ",Leveringsdato" ELSE "")).

iNextLnr = DYNAMIC-FUNCTION("getLocalQueryMaxValue",hBrowse,"KOrdreLinjeNr",10.00).
DYNAMIC-FUNCTION("setAttribute",hFieldMap,"bufferextravalues",
                 STRING(iNextLnr)
              + (IF hFieldMap:BUFFER-FIELD("Leveringsdato"):BUFFER-VALUE NE ? THEN "|" ELSE "")
                 ).
RUN SUPER.

DYNAMIC-FUNCTION("setAttribute",hFieldMap,"bufferextrafields","").
DYNAMIC-FUNCTION("setAttribute",hFieldMap,"bufferextravalues","").

APPLY "entry" TO hLinjeNrOverlay.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DefaultActionBrowse C-Win 
PROCEDURE DefaultActionBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
RUN SUPER.
IF VALID-HANDLE(hMomsKodOverlay) AND hMomsKodOverlay:SENSITIVE THEN
  hMomsKodOverlay:SCREEN-VALUE = STRING(hFieldMap:BUFFER-FIELD("MomsKod"):BUFFER-VALUE).
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
  HIDE FRAME frmFaktLinje.
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
IF VALID-HANDLE(hArtBasSok) AND hParentBuffer:AVAIL THEN DO:
  DYNAMIC-FUNCTION("setButikkNr" IN hArtBasSok,hParentBuffer:BUFFER-FIELD("ButikkNr"):BUFFER-VALUE).
  DYNAMIC-FUNCTION("setOrdreId"  IN hArtBasSok,hParentBuffer:BUFFER-FIELD("KOrdre_Id"):BUFFER-VALUE).
END.

IF hFieldMap:AVAIL THEN DO:
  InitOverlays(IF hFieldMap:BUFFER-FIELD("LeveringsDato"):BUFFER-VALUE NE ? THEN NO ELSE bEnableEdit).
  IF bEnableEdit AND hFieldMap:BUFFER-FIELD("LeveringsDato"):BUFFER-VALUE NE ? THEN
    DYNAMIC-FUNCTION("setAttribute",hToolbar,"disabledevents","delete,save").
  ELSE IF bEnableEdit THEN
    DYNAMIC-FUNCTION("setAttribute",hToolbar,"disabledevents","").

  IF NOT CAN-FIND(FIRST KOrdreLinjeRejectPlock WHERE KOrdreLinjeRejectPlock.KOrdre_Id    = hFieldMap:BUFFER-FIELD("KOrdre_Id"):BUFFER-VALUE AND
                                              KOrdreLinjeRejectPlock.KOrdreLinjeNr = hFieldMap:BUFFER-FIELD("KOrdreLinjeNr"):BUFFER-VALUE) THEN
      DYNAMIC-FUNCTION("setAttribute",hToolbar,"disabledevents","Nekade").
END.
IF hParentBuffer:AVAIL THEN
  DYNAMIC-FUNCTION("setAttribute",hToolbar,"enabledevents","HentArt").
ELSE
  DYNAMIC-FUNCTION("setAttribute",hToolbar,"enabledevents","").
/* IF hParentBuffer:AVAIL AND CAN-FIND(FIRST KOrdreLinjeRejectPlock WHERE KOrdreLinjeRejectPlock.KOrdre_Id = hParentBuffer:BUFFER-FIELD("KOrdre_Id"):BUFFER-VALUE) THEN */
/*     DYNAMIC-FUNCTION("setAttribute",hToolbar,"enabledevents","Nekade").                                                                                              */
/*                                                                            */
/* ELSE                                                                       */
/* /* DYNAMIC-FUNCTION("setAttribute",hToolbar,"disabledevents","Nekade"). */ */

RUN SUPER.

IF VALID-HANDLE(hArtikkelkort) AND hFieldMap:AVAIL THEN
  RUN ByttArtikkel IN hArtikkelkort (hFieldMap:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE).

VisLagerStatus().
/* IF NOT bNew AND hFieldMap:AVAIL AND hFieldMap:BUFFER-FIELD("VareNr"):BUFFER-VALUE NE "" THEN  */
/*   DYNAMIC-FUNCTION("DeleteObject",hVareNrOverlay).                                        */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EditRecord C-Win 
PROCEDURE EditRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF hFieldMap:BUFFER-FIELD("VareNr"):BUFFER-VALUE = "" THEN RETURN.

DEF VAR rRecId AS RECID NO-UNDO.
IF NOT VALID-HANDLE(hArtikkelkort) THEN DO:
  rRecId = DYNAMIC-FUNCTION("getRecId","ArtBas",hFieldMap:BUFFER-FIELD("RowIdent3"):BUFFER-VALUE).
  IF rRecId NE ? THEN
    RUN w-vartkor.w  PERSIST SET hArtikkelkort (rRecId, "ENDRE," + STRING(THIS-PROCEDURE)).
END.
ELSE 
  RUN ByttArtikkel IN hArtikkelkort (hFieldMap:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE).
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
  DISPLAY rsLagerStatusSalg Varespesifikasjon ArbeidsBeskr RefNr RefTekst 
          MomsKode Leveringsdato fiSumOrdreDb fiSumOrdreDb% fiSumRabattKr 
          fiSumOrdre fiSumOrdreEksMva fiSumOrdreDbLabel fiSumOrdreDb%Label 
          fiSumRabattKrLabel fiSumOrdreLabel fiSumOrdreEksMvaLabel 
      WITH FRAME frmFaktLinje.
  ENABLE BrwKOrdreLinje rectToolbar rectLagerStatus rsLagerStatusSalg 
         Varespesifikasjon ArbeidsBeskr RefNr RefTekst MomsKode Leveringsdato 
         fiSumOrdreEksMva fiSumOrdreDbLabel fiSumOrdreDb%Label 
         fiSumRabattKrLabel fiSumOrdreLabel fiSumOrdreEksMvaLabel 
      WITH FRAME frmFaktLinje.
  {&OPEN-BROWSERS-IN-QUERY-frmFaktLinje}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ExtraCloseAndDisableChilds C-Win 
PROCEDURE ExtraCloseAndDisableChilds :
/*------------------------------------------------------------------------------
  Purpose:     Ekstra innstilling ved ny kundeordre. Kalles automatisk fra UIlib
               Parameters:  <none>
  Notes:       Prosedyren kalles automatisk fra UIlib hvis den finnes
               For å stille tilbake toolbaren til standard (HentArt alltid tilgj),
               se DisplayRecord i KOrdreHode.w
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihBrowse AS HANDLE NO-UNDO.

DO WITH FRAME {&FRAME-NAME}: 
  DYNAMIC-FUNCTION("setAttribute",hToolbar,"enabledevents","").
  ASSIGN fiSumOrdre:SCREEN-VALUE       = "0"
         fiSumOrdreDb:SCREEN-VALUE     = "0" 
         fiSumOrdreDb%:SCREEN-VALUE    = "0" 
         fiSumOrdreEksMva:SCREEN-VALUE = "0"
         .
  InitOverlays(NO).
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ExtraSetBrowseRecord C-Win 
PROCEDURE ExtraSetBrowseRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihBrowse AS HANDLE NO-UNDO.

getStat().
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

DEF VAR cVarenr  AS CHAR NO-UNDO.

IF SOURCE-PROCEDURE:CURRENT-WINDOW NE THIS-PROCEDURE:CURRENT-WINDOW THEN RETURN.

cVarenr = DYNAMIC-FUNCTION("getFieldValues","Strekkode","WHERE Strekkode.kode = '" + icStrekkode + "'","ArtikkelNr").
IF cVarenr NE ? THEN DO:
  bOk = hFieldMap:FIND-FIRST("WHERE Varenr = '" + cVarenr + "'") NO-ERROR.
  IF bOk THEN
    hBrowse:QUERY:REPOSITION-TO-ROWID(hFieldMap:ROWID) NO-ERROR.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getSelectorAttributes C-Win 
PROCEDURE getSelectorAttributes :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       Kalles automatisk fra select-box når OK trykkes
------------------------------------------------------------------------------*/
DEF INPUT  PARAM ihBrwSource AS HANDLE NO-UNDO.
DEF INPUT  PARAM ihBrwTarget AS HANDLE NO-UNDO.
DEF INPUT  PARAM icDeSelRows AS CHAR   NO-UNDO.
DEF OUTPUT PARAM oiReturn    AS INT    NO-UNDO.

IF bSelectRekv THEN DO:
  IF cNyRekv = hFiRekvNr:SCREEN-VALUE + "|" + hFiRekvTekst:SCREEN-VALUE THEN
    cDeselectedRows = icDeSelRows.
  
  cNyRekv = hFiRekvNr:SCREEN-VALUE + "|" + hFiRekvTekst:SCREEN-VALUE.
END.
ELSE IF VALID-HANDLE(hPakkePris) THEN 
  ASSIGN iNumPakkeVarer = ihBrwTarget:QUERY:NUM-RESULTS
         cPakkePris     = DYNAMIC-FUNCTION("getPakkePris" IN hPakkePris).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE HentArtRecord C-Win 
PROCEDURE HentArtRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF NOT VALID-HANDLE(hArtBasSok) THEN DO:
  RUN ArtBasSok.w PERSIST SET hArtBasSok.
  DYNAMIC-FUNCTION("setButikkNr" IN hArtBasSok,hParentBuffer:BUFFER-FIELD("ButikkNr"):BUFFER-VALUE).
  DYNAMIC-FUNCTION("setOrdreId"  IN hArtBasSok,hParentBuffer:BUFFER-FIELD("KOrdre_Id"):BUFFER-VALUE).
  RUN InitializeObject IN hArtBasSok.
END.

getStat().

RUN MoveToTop IN hArtBasSok.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE HideOverlays C-Win 
PROCEDURE HideOverlays :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihWindow AS HANDLE NO-UNDO.

DEF VAR hColumn AS HANDLE NO-UNDO.

IF ihWindow = THIS-PROCEDURE:CURRENT-WINDOW THEN DO:
  hColumn = hBrowse:GET-BROWSE-COLUMN(1).
  APPLY "end-resize" TO hColumn.
END.

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
DO WITH FRAME {&FRAME-NAME}:

  DYNAMIC-FUNCTION("NewObject",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE,"procedure").

  ASSIGN MomsKode:DELIMITER = "|"
         MomsKode:LIST-ITEM-PAIRS = DYNAMIC-FUNCTION("GetFieldList","Moms;MomsKod|Beskrivelse;MomsKod","WHERE TRUE BY MomsKod").

  DYNAMIC-FUNCTION("setAttribute",BrwKOrdreLinje:HANDLE,"calcfieldproc","kordrelinje_brwcalc.p").
  /* Create the browse: */
  hBrowse = DYNAMIC-FUNCTION("NewBrowse",
                           BrwKOrdreLinje:HANDLE,        
                           100,                      
                           "",                       
                           "KOrdreLinje"             
                           + ";KOrdreLinjeNr|Lnr"
                           + ";Pakkeidx|Pknr|>9"
                           + ";VareNr|Art.nr"
                           + ";Varetekst"
                           + ";Bestillingsnummer|Lev.artnr"
                           + ";LevFargKod|Lev.farge"
                           + ";Storl|Str"
                           + ";Pris"
                           + ";KundeRab%|Knd.rab%"
                           + ";Tilbud|T|*/"
                           + ";BruttoPris|Beste pris"
                           + ";Antall|Ant"
                           + ";LinjeRab%|LinjeRab%"
                           + ";NettoPris|Nto.pris"
                           + ";NettoLinjesum|Sum"
                           + ";+SumEksMvaKr|DECIMAL|->>><>>9.99|kordrelinje_sumeksmvakr|Sum eks MVA"
                           + ";MomsKod"
                           + ";Db%"
                           + ";DbKr"
                           + ";LinjeRabattKr|Tot.linjerab"
                           + ";KundeRabattKr|Tot.kunderab"
                           + ";OrdreRabattKr|Tot.ordrerab"
                           + ";ReturKodeId|Ret.kode"
                           + ";Kode|Ean|x(15)"
                           + ";Varespesifikasjon|Varefakta"
                           + ";ArbeidsBeskr|Merknad"
                           + ";Varekost"
                           + ";+!SumVarekost|DECIMAL|->>><>>9.99|kordrelinje_sumvarekost|Sum varekost"
                           + ";Leveringsdato"
                           + ";RefNr|Rekv.nr"
                           + ";RefTekst|Rekv.tekst"
                           + ";PlukkButikk|Plukk but"
                           + ";UtleverButikk|Utlev. but"
                           + ";KOrdre_Id|Ordrenr"
                           + ";plockstatus|Plukkstatus"
                           + ";plockdatetime|Plukket|99/99/99"
                         + ",KordreHode"
                           + ";ButikkNr"
                         + ",ArtBas"
                           + ";!ArtikkelNr"
/*                            + ";+TilbPris|LOGICAL|J/|art_paa_tilbud.p(ROWID)|Tilb" */
                           ,"WHERE false"
                           + ",FIRST KOrdreHode no-lock where KOrdreHode.KOrdre_ID = KOrdreLinje.KOrdre_Id, FIRST ArtBas WHERE ArtikkelNr = DEC(KOrdreLinje.VareNr) NO-LOCK OUTER-JOIN"
                           ,"sort|KOrdreLinjeNr").  

  DYNAMIC-FUNCTION("createObjectLink",hBrowse,THIS-PROCEDURE).

  DYNAMIC-FUNCTION("setAttribute",hBrowse,"insertbrowserow","yes").
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"nexttabitem",STRING(Varespesifikasjon:HANDLE)).
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"prevtabitem",DYNAMIC-FUNCTION("getLastTabWidget" IN hParent)).
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"mandatoryfields","varenr,storl,antall").  
    
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"enableondblclick","yes").
  DYNAMIC-FUNCTION("setATtribute",hBrowse,"setReadOnlyOnReturn","yes").
/*   DYNAMIC-FUNCTION("setAttribute",hBrowse,"setReadOnlyOnReturnOfLastField","yes"). */
/*   DYNAMIC-FUNCTION("setAttribute",hBrowse,"gotoFirstEnabledFromBackTab","yes").  */
/*   DYNAMIC-FUNCTION("setAttribute",hBrowse,"gotoLastEnabledFromTab","yes").       */


  InitOverlays(YES).

  hToolbar = DYNAMIC-FUNCTION("NewToolbar",
                            rectToolbar:HANDLE,
                            "",
                            "new;Ny,Copy;Kopier,Save;Lagre,delete;Slett,Edit;Artikkelkort&,Excel,BrowseConfig;Kolonneoppsett"
                          + ",HentArt;Legg til artikler&"
                          + ",Rekvisisjon;&Rekvisisjon;Angi rekvisisjonsnummer for artikler"
                          + ",Pakke;Sett &pakkepris;Sett pakkepris for artikler"
                          + ",Plukk;Bytt plukkbutikk"
                          + ",Nekade;Nekad pluck"
/*                           + ",Bestilling;&Bestill varer;Lag suppleringsordre for artikler"  */
                           ,"maxborder").

  DYNAMIC-FUNCTION("CreateObjectLink",hBrowse,hToolbar).
  DYNAMIC-FUNCTION("setLinjeToolbar" IN hParent,hToolbar).


  hFieldMap = DYNAMIC-FUNCTION("NewFieldMap",
                              hBrowse,
                              FRAME {&FRAME-NAME}:HANDLE,
                              "Varespesifikasjon,ArbeidsBeskr,RefNr,RefTekst","",
                              "Leveringsdato,MomsKod","Leveringsdato,MomsKode",
                              "").
  DYNAMIC-FUNCTION("setAttribute",hFieldMap,"postupdateproc","kordrelinje_post_update.p").

  DYNAMIC-FUNCTION("CreateObjectLink",hBrowse,hFieldMap).
  DYNAMIC-FUNCTION("CreateObjectLink",hToolbar,hFieldMap).


  RUN FlexGrid.w PERSIST SET hLagerstatus.
  RUN InitializeObject IN hLagerstatus (rectLagerStatus:HANDLE IN FRAME {&FRAME-NAME}).
  hLagerStatusFrame = DYNAMIC-FUNCTION("getFrameHandle" IN hLagerStatus).
  hLagerStatusOCXframe = DYNAMIC-FUNCTION("getOCXframe" IN hLagerStatus).


  DYNAMIC-FUNCTION("setNoResizeX",THIS-PROCEDURE:CURRENT-WINDOW,hLagerStatusFrame,"").
  
  DYNAMIC-FUNCTION("setNoResizeY",THIS-PROCEDURE:CURRENT-WINDOW,hLagerStatusFrame,hLagerStatusFrame:NAME).
  DYNAMIC-FUNCTION("setNoResizeY",THIS-PROCEDURE:CURRENT-WINDOW,hLagerStatusOCXframe,hLagerStatusOCXframe:NAME).

  /* DYNAMIC-FUNCTION("setNoMoveX",THIS-PROCEDURE:CURRENT-WINDOW,FRAME {&FRAME-NAME}:HANDLE,"Grid"). */
  DYNAMIC-FUNCTION("setNoMoveY",THIS-PROCEDURE:CURRENT-WINDOW,hLagerStatusFrame,"").
  DYNAMIC-FUNCTION("setAddMoveY",THIS-PROCEDURE:CURRENT-WINDOW,hLagerStatusFrame,"{&FRAME-NAME}").

/*   DYNAMIC-FUNCTION("setAnchor",THIS-PROCEDURE:CURRENT-WINDOW,BrwKOrdreLinje:HANDLE IN FRAME {&FRAME-NAME},  */
/*                    STRING(rectLagerStatus:HANDLE) + ","                                                 */
/*                  + STRING(hLagerStatusFrame)                                                            */
/*                   ,"Y").                                                                                */

  SUBSCRIBE TO "setLookupAttributes" ANYWHERE.
  SUBSCRIBE TO "setEnableEditOrderLine" ANYWHERE.
  SUBSCRIBE TO "AltSKundeOrdre" ANYWHERE.
  SUBSCRIBE TO "InvalidateHandle" ANYWHERE.
  SUBSCRIBE TO "HideOverlays" ANYWHERE.
  SUBSCRIBE TO "FinnStrekkode" ANYWHERE.
  
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InvalidateHandle C-Win 
PROCEDURE InvalidateHandle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF SOURCE-PROCEDURE:FILE-NAME = "KOrdreView.w" THEN
  IF VALID-HANDLE(hLagerStatus) THEN APPLY "close" TO hLagerstatus.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LeaveBrowseFillIn C-Win 
PROCEDURE LeaveBrowseFillIn :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cVarenrStr AS CHAR NO-UNDO.
DEF VAR cStorl     AS CHAR NO-UNDO.
DEF VAR fArtNr     AS DEC  NO-UNDO.

IF DYNAMIC-FUNCTION("getCurrentObject") = hVareNrOverlay AND hVareNrOverlay:MODIFIED THEN DO:
  cVarenrStr = DYNAMIC-FUNCTION("getFieldList","Strekkode;ArtikkelNr,StrKonv;Storl",
                                "WHERE Strekkode.kode = '" + hVareNrOverlay:SCREEN-VALUE + "'" +
                                ",FIRST StrKonv OF Strekkode NO-LOCK").
  IF cVarenrStr = "" THEN DO:
    fArtNr = DEC(hVareNrOverlay:SCREEN-VALUE) NO-ERROR.
    IF NOT ERROR-STATUS:ERROR THEN
      cVarenrStr = DYNAMIC-FUNCTION("getFieldValues","Artbas","WHERE Artbas.Artikkelnr = " + hVareNrOverlay:SCREEN-VALUE,"ArtikkelNr").
    IF cVarenrStr = "" OR cVarenrStr = ? THEN DO:        
      cVarenrStr = DYNAMIC-FUNCTION("getFieldValues","Artbas","WHERE Artbas.LevKod = '" + hVareNrOverlay:SCREEN-VALUE + "'","ArtikkelNr").
      IF cVarenrStr NE ? THEN 
        hVareNrOverlay:SCREEN-VALUE = cVarenrStr.
      ELSE DO:
        APPLY "f3" TO hVarenrOverlay.
        IF NOT (hVareNrOverlay:SCREEN-VALUE BEGINS "02" AND LENGTH(hVareNrOverlay:SCREEN-VALUE) = 13) THEN
          DYNAMIC-FUNCTION("setWidgetEnter",DYNAMIC-FUNCTION("SetLevKodQuery" IN hArtBasSok,hVareNrOverlay:SCREEN-VALUE)).
        RETURN.
      END.
    END.
  END.
  ELSE DO:

    ASSIGN hVareNrOverlay:SCREEN-VALUE = ENTRY(1,cVarenrStr,"|")
           cStorl                      = TRIM(ENTRY(2,cVarenrStr,"|")).      
  END. 
END.
ELSE IF DYNAMIC-FUNCTION("getCurrentObject") = hStorlOverlay AND
   hStorlOverlay:SCREEN-VALUE = "" AND
   hFieldMap:BUFFER-FIELD("VareNr"):BUFFER-VALUE NE "" THEN DO:
  DYNAMIC-FUNCTION("DoMessage",0,0,"Velg en størrelse","","").
  APPLY "entry" TO hStorlOverlay.
  RETURN.
END.
ELSE IF DYNAMIC-FUNCTION("getCurrentObject") = hLinjeRabOverlay AND
   DEC(hLinjeRabOverlay:SCREEN-VALUE) NE 0 THEN DO:

  IF NOT LOGICAL(DYNAMIC-FUNCTION("getFieldValues","ArtBas","WHERE Artikkelnr = DEC('" + 
                                                    STRING(hFieldMap:BUFFER-FIELD("VareNr"):BUFFER-VALUE) + 
                                                    "')","ManRabIKas")) THEN DO:
      
    DYNAMIC-FUNCTION("DoMessage",0,0,"Artikkel kan ikke rabatteres","","").
    APPLY "entry" TO hLinjeRabOverlay.
    RETURN.
  END.
END.

RUN SUPER.

IF DYNAMIC-FUNCTION("getCurrentObject") = hVareNrOverlay THEN DO:
  bNew = NO.
  getStat().

  IF cStorl NE "" THEN DO:
    hStorlOverlay:SCREEN-VALUE = cStorl.
    APPLY "tab" TO hStorlOverlay.
  END.
END.
ELSE getStat().

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
DEF VAR hColumn AS HANDLE NO-UNDO.
hColumn = hBrowse:GET-BROWSE-COLUMN(1).
APPLY "end-resize" TO hColumn.
FRAME {&FRAME-NAME}:MOVE-TO-TOP().

ASSIGN 
/*        rectLagerStatus:HEIGHT-PIXELS      = 53 */
/*        rectLagerStatus:WIDTH-PIXELS       = 335 */
       hLagerStatusFrame:Y = DYNAMIC-FUNCTION("getAbsPosition",rsLagerStatusSalg:HANDLE,"Y") + 16
/*        hLagerStatusFrame:WIDTH-PIXELS     = rectLagerStatus:WIDTH-PIXELS - 50 */
/*        hLagerStatusOCXframe:WIDTH-PIXELS  = rectLagerStatus:WIDTH-PIXELS - 100 */
/*        hLagerStatusFrame:HEIGHT-PIXELS    = rectLagerStatus:HEIGHT-PIXELS  */
/*        hLagerStatusOCXframe:HEIGHT-PIXELS =  rectLagerStatus:HEIGHT-PIXELS - 10  */
       .

hLagerStatusFrame:MOVE-TO-TOP().
APPLY "entry" TO hBrowse.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MySaveBrowseFillIn C-Win 
PROCEDURE MySaveBrowseFillIn :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT  PARAM ihFillIn  AS HANDLE NO-UNDO.
DEF INPUT  PARAM ihBuffer  AS HANDLE NO-UNDO.
DEF OUTPUT PARAM obOk      AS LOG    NO-UNDO.

/* Må settes for å få riktig navigering fra overlay pga at funksjoene under endrer LAST-EVENT */
DYNAMIC-FUNCTION("setAttribute",ihFillIn,"last-event",LAST-EVENT:LABEL).

IF ihFillIn = hVareNrOverlay THEN 
  obOK = setVareNr(ihFillIn:SCREEN-VALUE).
ELSE IF ihFillIn = hStorlOverlay THEN
  obOK = setStrKode(ihFillIn:SCREEN-VALUE).
ELSE DO:
  DYNAMIC-FUNCTION("setPostUpdProc","kordrelinje_post_update.p").
  obOK = DYNAMIC-FUNCTION("DoUpdate",ihBuffer:NAME,"",
                "",
                ihBuffer:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE,
                DYNAMIC-FUNCTION("getAttribute",ihFillIn,"buffercolumn"),
                ihFillIn:SCREEN-VALUE,
                TRUE).
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NekadeRecord C-Win 
PROCEDURE NekadeRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE dKOrdre_Id     AS DECI   NO-UNDO.
DEFINE VARIABLE iKOrdreLinjeNr AS INTEGER     NO-UNDO.
IF hFieldMap:AVAIL THEN DO:
    ASSIGN dKOrdre_Id     = hFieldMap:BUFFER-FIELD("KOrdre_Id"):BUFFER-VALUE
           iKOrdreLinjeNr = hFieldMap:BUFFER-FIELD("KOrdreLinjeNr"):BUFFER-VALUE NO-ERROR.
    IF NOT ERROR-STATUS:ERROR THEN
        RUN dRejectlista.w (dKOrdre_Id,iKOrdreLinjeNr).
END.
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
DEF VAR iNextLnr       AS INT    NO-UNDO.
DEF VAR hFirstOverlay  AS HANDLE NO-UNDO.

IF NOT hParentBuffer:AVAILABLE THEN RETURN.

DYNAMIC-FUNCTION("setAttribute",hFieldMap,"bufferextrafields","KOrdre_id,KOrdreLinjeNr,Antall").

iNextLnr = DYNAMIC-FUNCTION("getLocalQueryMaxValue",hBrowse,"KOrdreLinjeNr",10.00).
DYNAMIC-FUNCTION("setAttribute",hFieldMap,"bufferextravalues",
                 STRING(hParentBuffer:BUFFER-FIELD("KOrdre_Id"):BUFFER-VALUE) + "|" +
                 STRING(iNextLnr) + "|1").

RUN SUPER.

DYNAMIC-FUNCTION("setAttribute",hFieldMap,"bufferextrafields","").
DYNAMIC-FUNCTION("setAttribute",hFieldMap,"bufferextravalues","").

IF NOT PROGRAM-NAME(2) BEGINS "AddStr" THEN DO:
  hFirstOverlay = WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",hBrowse,"firstenabledoverlay")).
  IF VALID-HANDLE(hFirstOverlay) THEN
    APPLY "entry" TO hFirstOverlay.
  ELSE DO:
    IF NOT VALID-HANDLE(hVareNrOverlay) THEN DO: 
      bEnableEdit = YES.
      InitOverlays(YES).
      APPLY "value-changed" TO hBrowse.
    END.
    APPLY "entry" TO hVareNrOverlay.
  END.
END.

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
/* IF hParentBuffer:AVAIL THEN                                                       */
/*   DYNAMIC-FUNCTION("setAttribute",hBrowse,"calcparamTilbPris",                    */
/*                    STRING(hParentBuffer:BUFFER-FIELD("ButikkNr"):BUFFER-VALUE)).  */

RUN SUPER.

getStat().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PakkeRecord C-Win 
PROCEDURE PakkeRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cPkVareList  AS CHAR  NO-UNDO.
DEF VAR cRowIdList   AS CHAR  NO-UNDO.
DEF VAR iReturn      AS INT   NO-UNDO.
DEF VAR fNettoSum    AS DEC   NO-UNDO.
DEF VAR cFordeling   AS CHAR  NO-UNDO.
DEF VAR iNyPakke     AS INT   NO-UNDO.
DEF VAR cRepos       AS CHAR  NO-UNDO.

IF hFieldMap:BUFFER-FIELD("Pakkeidx"):BUFFER-VALUE NE 0 THEN
  cRowIdList = DYNAMIC-FUNCTION("getRowIdList","KOrdreLinje","",
                                "WHERE KOrdre_id = " + STRING(hFieldMap:BUFFER-FIELD("KOrdre_id"):BUFFER-VALUE)
                               + " AND Pakkeidx = " + STRING(hFieldMap:BUFFER-FIELD("Pakkeidx"):BUFFER-VALUE)
                                ).

THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = NO.

ASSIGN cDeselectedRows = ""
       iPakkeIdx = hFieldMap:BUFFER-FIELD("Pakkeidx"):BUFFER-VALUE
       .

RUN JBoxSelector.w (THIS-PROCEDURE,0,
                    "KOrdreLinje"  
                    + ";KOrdreLinjeNr|Lnr"
                    + ";VareNr|Varenr"
                    + ";VareTekst|Varetekst"
                    + ";Storl"
                    + ";Pris"
                    + ";!KundeRab%|Knd.rab%"
                    + ";Tilbud|T|*/"
                    + ";!BruttoPris|Beste pris"
                    + ";Antall|Ant"
                    + ";!LinjeRab%|LinjeRab%"
                    + ";!NettoPris|Nto.pris"
                    + ";NettoLinjesum|Sum"
                    + ";!+SumEksMvaKr|DECIMAL|->>><>>9.99|kordrelinje_sumeksmvakr.p|Sum eks MVA"
                    + ";!Db%"
                    + ";!DbKr"
                    + ";!Mva%"
                    + ";!KundeRabattKr|Tot.kunderab"
                    + ";!OrdreRabattKr|Tot.ordrerab"
                    + ";!LinjeRabattKr|Tot.linjerab"
                    + ";!Varekost"
                    + ";!Pakkeidx"
                    + ";!Leveringsdato"
                    + ";!KOrdre_id"
                  + ",ArtBas;!ManRabIKas"
                    ,DYNAMIC-FUNCTION("getAttribute",hBrowse,"basequery") 
                    + " AND CAN-DO('0," + STRING(iPakkeIdx) + "',STRING(Pakkeidx))" 
                    + " AND Leveringsdato = ?"
                    + " AND Varekost > 0"
                  + ",FIRST ArtBas NO-LOCK WHERE ArtBas.ArtikkelNr = DEC(KOrdreLinje.VareNr)"
                    + " AND ArtBas.ManRabIKas" 
                    ,INPUT-OUTPUT cRowIdList,
                    "KOrdreLinjeNr",
                    INPUT-OUTPUT cPkVareList,
                    "","",
                    OUTPUT bOK).

THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = YES.


IF bOk AND (DEC(cPakkePris) NE 0 OR iNumPakkeVarer = 0) THEN DO:
  IF DYNAMIC-FUNCTION("runproc","kordrelinje_kalkpakke.p","prod;" + cPakkePris + ";" + cRowIdList + ";1;" 
                                + STRING(iPakkeIdx) + ";"
                                + STRING(hFieldMap:BUFFER-FIELD("KOrdre_id"):BUFFER-VALUE),?) THEN DO:
    DYNAMIC-FUNCTION("setCurrentObject",hParentQuery).
    RUN DisplayRecord IN hParent.
  END.
  ELSE DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").
END.
ELSE IF DEC(cPakkePris) = 0 AND iNumPakkeVarer NE iOrgNumPakkeVarer THEN
  DYNAMIC-FUNCTION("DoMessage",0,0,"Pakkepris eller sammensetning ble ikke endret pga at ny pris ikke var satt","Informasjon","").


DYNAMIC-FUNCTION("setCurrentObject",hBrowse).
RUN OpenQuery.
IF cRepos NE "" THEN DO:
  bOK = hFieldMap:FIND-FIRST("WHERE RowIdent1 = '" + cRepos + "'") NO-ERROR.
  IF bOK THEN
    hBrowse:QUERY:REPOSITION-TO-ROWID(hFieldMap:ROWID).
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PlukkRecord C-Win 
PROCEDURE PlukkRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR pcButLst             AS CHAR NO-UNDO.
  DEF VAR cLagerFieldList      AS CHAR NO-UNDO.
  DEFINE VARIABLE cOrderbutikk AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cOldPlukk    AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE iPlockstatus AS INTEGER     NO-UNDO.
  DEFINE VARIABLE cKOLinjeRowId AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cKOBekreftadRowid AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cWaitforConfRowid AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE lRet AS LOGICAL     NO-UNDO.
  DEFINE VARIABLE iTst AS INTEGER     NO-UNDO.
  DEFINE VARIABLE lOK AS LOGICAL     NO-UNDO.
/* RETURN NO-APPLY. */
  {syspara.i 150 1 4 pcButLst}
  IF hFieldMap:AVAIL THEN DO:
      IF hFieldMap:BUFFER-FIELD("VAretekst"):BUFFER-VALUE MATCHES "*FRAKT*" OR hFieldMap:BUFFER-FIELD("Varenr"):BUFFER-VALUE MATCHES "*BET*" THEN
          RETURN.
      cOldPlukk    = hFieldMap:BUFFER-FIELD("Plukkbutikk"):BUFFER-VALUE.
      cOrderbutikk = hFieldMap:BUFFER-FIELD("ButikkNr"):BUFFER-VALUE.
      cKOLinjeRowId = hFieldMap:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE.
/*       OUTPUT TO "CLIPBOARD".                                                 */
/*       PUT UNFORMATTED hFieldMap:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE SKIP. */
/*       OUTPUT CLOSE.                                                          */
/*       MESSAGE 'Gurre var her ' NOW SKIP                                                                    */
/*           hFieldMap:BUFFER-FIELD("VareNr"):BUFFER-VALUE                                                    */
/*           hFieldMap:BUFFER-FIELD("VAretekst"):BUFFER-VALUE                                                 */
/*           hFieldMap:BUFFER-FIELD("Antall"):BUFFER-VALUE                                                    */
/*           hFieldMap:BUFFER-FIELD("Nettopris"):BUFFER-VALUE                                                 */
/*           hFieldMap:BUFFER-FIELD("Storl"):BUFFER-VALUE SKIP                                                */
/*           "But" hFieldMap:BUFFER-FIELD("ButikkNr"):BUFFER-VALUE SKIP                                       */
/*           hFieldMap:BUFFER-FIELD("Plukkbutikk"):BUFFER-VALUE SKIP                                          */
/*           pcButLst SKIP                                                                                    */
/*                                                                                                            */
/*           "WHERE ArtikkelNr = " + hFieldMap:BUFFER-FIELD("VareNr"):BUFFER-VALUE + " AND Storl = '" +       */
/*                               hFieldMap:BUFFER-FIELD("Storl"):BUFFER-VALUE +                               */
/*                               "' AND LagAnt > '0'" +                                                       */
/*                               " AND Artlag.butik <> " + hFieldMap:BUFFER-FIELD("Plukkbutikk"):BUFFER-VALUE */
/*                                                                                                            */
/*           VIEW-AS ALERT-BOX INFO BUTTONS OK.                                                               */
      IF hFieldMap:BUFFER-FIELD("Plockstatus"):BUFFER-VALUE = 2 THEN DO:
          MESSAGE "STATUS: PLOCK HAR BEKRÄFTATS." SKIP
                  "Plockstatus ändras till 1." SKIP
                  "Vara överförs tillbaka till butik:" cOldPlukk UPDATE lOK
              VIEW-AS ALERT-BOX INFO BUTTONS YES-NO.
          IF lOK THEN DO:
              /* Om vi skall reversera BEKRÄFTAD så skall vi återföra lager vi val av ny butik */
              cKOBekreftadRowid = cKOLinjeRowId.
/*               FIND KOrdrelinje WHERE ROWID(KOrdreLinje) = TO-ROWID(cKOLinjeRowId) NO-ERROR. */
/*               IF AVAIL KOrdrelinje THEN DO:                                          */
/*                   ASSIGN KOrdrelinje.PlockStatus = 1                                 */
/*                          KOrdreLinje.plockdatetime = ?.                              */
/*                   RELEASE KOrdreLinje.                                               */
/*                   DYNAMIC-FUNCTION("RefreshRowids",hBrowse,TRIM(cKOLinjeRowId,",")). */
/*                   RUN StartQuery.                                                    */
/*               END.                                                                   */
          END.
          ELSE
              RETURN.
      END.
      ELSE IF hFieldMap:BUFFER-FIELD("Plockstatus"):BUFFER-VALUE = 1 THEN DO:
          MESSAGE "STATUS: VÄNTAR PÅ ATT BEKRÄFTAS." SKIP
                  "Vara överförs tillbaka till butik:" cOldPlukk UPDATE lOK
              VIEW-AS ALERT-BOX INFO BUTTONS YES-NO.
          IF lOK THEN DO:
              /* Om vi skall reversera BEKRÄFTAD så skall vi återföra lager vi val av ny butik */
              cWaitforConfRowid = cKOLinjeRowId.
/*               FIND KOrdrelinje WHERE ROWID(KOrdreLinje) = TO-ROWID(cKOLinjeRowId) NO-ERROR. */
/*               IF AVAIL KOrdrelinje THEN DO:                                          */
/*                   ASSIGN KOrdrelinje.PlockStatus = 1                                 */
/*                          KOrdreLinje.plockdatetime = ?.                              */
/*                   RELEASE KOrdreLinje.                                               */
/*                   DYNAMIC-FUNCTION("RefreshRowids",hBrowse,TRIM(cKOLinjeRowId,",")). */
/*                   RUN StartQuery.                                                    */
/*               END.                                                                   */
          END.
          ELSE
              RETURN.
      END.

      THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
      RUN JBoxLookup.w (THIS-PROCEDURE,50,
                        "ArtLag"
                         + ";Butik"
                         + ";Storl"
                         + ";LagAnt"
                         ,
/*                        "WHERE ArtikkelNr = '" + hFieldMap:BUFFER-FIELD("VareNr"):BUFFER-VALUE + "' AND Storl = '" +                                 */
/*                                            hFieldMap:BUFFER-FIELD("Storl"):BUFFER-VALUE + "' AND NOT CAN-DO('" + pcButLst + "',STRING('ButikkNr'))" */
                        "WHERE ArtikkelNr = " + hFieldMap:BUFFER-FIELD("VareNr"):BUFFER-VALUE + " AND trim(Storl) = '" + 
                                            TRIM(hFieldMap:BUFFER-FIELD("Storl"):BUFFER-VALUE) + 
                                            "' AND LagAnt > '0'" +
                                            " AND Artlag.butik <> " + cOldPlukk + 
                                            " AND Artlag.butik <> " + cOrderbutikk
                        ,""
                        ,"Butik",
                        OUTPUT cLagerFieldList,
                        OUTPUT bOK).
      THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

      IF bOk AND cLagerFieldList NE "" THEN DO:
          iTst = INT(cLagerFieldList) NO-ERROR.
          IF ERROR-STATUS:ERROR OR iTst = 0 THEN
              RETURN.
          IF cKOBekreftadRowid <> "" OR cWaitforConfRowid <> "" THEN DO:
              IF cKOBekreftadRowid <> "" THEN DO:
                  FIND KOrdrelinje WHERE ROWID(KOrdreLinje) = TO-ROWID(cKOBekreftadRowid) NO-ERROR.
                  IF AVAIL KOrdrelinje THEN DO:
                      ASSIGN KOrdrelinje.PlockStatus = 1
                             KOrdreLinje.plockdatetime = ?.
                      RELEASE KOrdreLinje.
                  END.
              END.
              RUN asWebPlock.p (INPUT INT(cOldPlukk),"CONFIRM",cKOLinjeRowId,9,OUTPUT lRet,INPUT-OUTPUT TABLE tt_Dummy).
          END.
        RUN sett_plukkbutikk_kundeordreJF.p ("",cKOLinjeRowId,iTst).
        DYNAMIC-FUNCTION("RefreshRowids",hBrowse,TRIM(cKOLinjeRowId,",")).
/*           MESSAGE cLagerFieldList                */
/*               VIEW-AS ALERT-BOX INFO BUTTONS OK. */
      END.

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
DEFINE INPUT  PARAMETER icDirection AS CHARACTER  NO-UNDO.

IF CAN-DO("Prev,Next",icDirection) THEN DO:
  DYNAMIC-FUNCTION("DoLockWindow",THIS-PROCEDURE:CURRENT-WINDOW).
  DYNAMIC-FUNCTION("setAttribute",SESSION,"userKeepsWindowLocked","yes").
  hBrowse:SELECT-FOCUSED-ROW().
  CASE icDirection:
    WHEN "Prev" THEN
        hBrowse:SELECT-PREV-ROW().
    WHEN "Next" THEN
      hBrowse:SELECT-NEXT-ROW().
  END CASE.
  APPLY "value-changed" TO hBrowse.
  DYNAMIC-FUNCTION("setAttribute",SESSION,"userKeepsWindowLocked","").
  DYNAMIC-FUNCTION("DoLockWindow",?).
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RekvisisjonRecord C-Win 
PROCEDURE RekvisisjonRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cVareList   AS CHAR  NO-UNDO.
DEF VAR cRowIdList  AS CHAR  NO-UNDO.
DEF VAR iRekvNr     AS INT   NO-UNDO.
DEF VAR cRepos      AS CHAR  NO-UNDO.

THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = NO.

ASSIGN cNyRekv         = ""
       cDeselectedRows = ""
       bSelectRekv     = YES.

iRekvNr = hFieldMap:BUFFER-FIELD("RefNr"):BUFFER-VALUE.

IF hBrowse:QUERY:IS-OPEN THEN DO:
  cRepos = hFieldMap:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE.
  hBrowse:QUERY:GET-FIRST().
  REPEAT WHILE NOT hBrowse:QUERY:QUERY-OFF-END:
    IF iRekvNr NE 0 AND hFieldMap:BUFFER-FIELD("RefNr"):BUFFER-VALUE NE iRekvNr THEN
      hBrowse:QUERY:GET-NEXT().
    ELSE DO:
      IF cNyRekv = "" AND (hFieldMap:BUFFER-FIELD("RefNr"):BUFFER-VALUE NE 0 OR hFieldMap:BUFFER-FIELD("RefTekst"):BUFFER-VALUE NE "") THEN
        ASSIGN cNyRekv = STRING(hFieldMap:BUFFER-FIELD("RefNr"):BUFFER-VALUE) + "|" + hFieldMap:BUFFER-FIELD("RefTekst"):BUFFER-VALUE
               cRowIdList = cRowIdList + hFieldMap:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE + ",".
      ELSE IF cNyRekv = STRING(hFieldMap:BUFFER-FIELD("RefNr"):BUFFER-VALUE) + "|" + hFieldMap:BUFFER-FIELD("RefTekst"):BUFFER-VALUE THEN
        cRowIdList = cRowIdList + hFieldMap:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE + ",".
      hBrowse:QUERY:GET-NEXT().
    END.
  END.
  cRowIdList = TRIM(cRowIdList,",").
END.

RUN JBoxSelector.w (THIS-PROCEDURE,0,
                    "KOrdreLinje"                               
                    + ";KOrdreLinjeNr"
                    + ";VareNr"
                    + ";VareTekst"
                    + ";Antall"
                    + ";!KOrdre_id"
                    ,DYNAMIC-FUNCTION("getAttribute",hBrowse,"basequery")
                    ,INPUT-OUTPUT cRowIdList,
                    "KOrdreLinjeNr",
                    INPUT-OUTPUT cVareList,
                    "","",
                    OUTPUT bOK).

THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = YES.

bSelectRekv = NO.

IF bOk THEN DO:
  IF NOT DYNAMIC-FUNCTION("runproc",
                          "kordrelinje_rekvisisjon.p",
                          cNyRekv + ";" + cRowIdList + ";" + cDeselectedRows
                         ,?) THEN
    DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").
END.

DYNAMIC-FUNCTION("setCurrentObject",hBrowse).
RUN OpenQuery.

IF cRepos NE "" THEN DO:
  bOK = hFieldMap:FIND-FIRST("WHERE RowIdent1 = '" + cRepos + "'") NO-ERROR.
  IF bOK THEN
    hBrowse:QUERY:REPOSITION-TO-ROWID(hFieldMap:ROWID).
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
IF hTilbField:BUFFER-VALUE AND hFieldMap:BUFFER-FIELD("VareNr"):BUFFER-VALUE NE "" THEN
  hPrisColumn:BGCOLOR = 12.
IF hFieldMap:BUFFER-FIELD("Leveringsdato"):BUFFER-VALUE NE ? THEN
  hAntallColumn:BGCOLOR = 8.
ELSE IF CAN-FIND(FIRST ttKOrdreLinje WHERE 
                 ttKOrdreLinje.KOrdre_Id = hFieldMap:BUFFER-FIELD("KOrdre_Id"):BUFFER-VALUE AND 
                 ttKORdreLinje.KOrdreLinjeNr = hFieldMap:BUFFER-FIELD("KOrdreLinjeNr"):BUFFER-VALUE AND 
                 ttKORdreLinje.Manko = TRUE) THEN 
    hAntallColumn:BGCOLOR = 11.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RowDisplaySelector C-Win 
PROCEDURE RowDisplaySelector :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihBrowse AS HANDLE NO-UNDO.
DEF INPUT PARAM ihBuffer AS HANDLE NO-UNDO.

IF NOT bSelectRekv THEN DO:
  IF ihBrowse = hSelectSrcBrowse AND ihBuffer:BUFFER-FIELD("Tilbud"):BUFFER-VALUE THEN
    hSelectSrcTilbField:BGCOLOR = 12.
  ELSE IF ihBuffer:BUFFER-FIELD("Tilbud"):BUFFER-VALUE THEN
    hTargetSrcTilbField:BGCOLOR = 12.    
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
RUN SUPER.

getStat().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setEnableEditOrderLine C-Win 
PROCEDURE setEnableEditOrderLine :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ibEnableEdit AS LOG NO-UNDO.

bEnableEdit = ibEnableEdit.

IF bEnableEdit THEN
  DYNAMIC-FUNCTION("setAttribute",hToolbar,"disabledevents","").
ELSE
/*   DYNAMIC-FUNCTION("setAttribute",hToolbar,"disabledevents","new,copy,delete,save,HentArt,Rekvisisjon"). */
  DYNAMIC-FUNCTION("setAttribute",hToolbar,"disabledevents","new,copy,delete,save,HentArt,Rekvisisjon,Pakke,Plukk").
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setLookupAttributes C-Win 
PROCEDURE setLookupAttributes :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihBrowse AS HANDLE NO-UNDO.
DEF INPUT PARAM ihDummy1 AS HANDLE NO-UNDO.
DEF INPUT PARAM ihDummy2 AS HANDLE NO-UNDO.
DEF VAR hBuffer          AS HANDLE NO-UNDO.

IF ihBrowse:QUERY:GET-BUFFER-HANDLE(1):NAME = "_file" THEN DO:
  hBuffer = ihBrowse:QUERY:GET-BUFFER-HANDLE(1).
  hBuffer:EMPTY-TEMP-TABLE().
  ihDummy1 = DYNAMIC-FUNCTION("getTempTable","artbas_str_beh.p",
                            STRING(hFieldMap:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE) + "," + STRING(hParentBuffer:BUFFER-FIELD("ButikkNr"):BUFFER-VALUE) + ",1" ,
                            hBuffer).
  DYNAMIC-FUNCTION("setAttribute",ihBrowse,"uselocaldata","yes").
  DYNAMIC-FUNCTION("setCurrentObject",ihBrowse).
  RUN OpenQuery.
END.

ELSE IF ihBrowse:QUERY:GET-BUFFER-HANDLE(1):NAME = "ArtBas" THEN DO:
  DYNAMIC-FUNCTION("setAttribute",ihBrowse,"calcparamTilbPris",
                   STRING(hParentBuffer:BUFFER-FIELD("ButikkNr"):BUFFER-VALUE)).
  DYNAMIC-FUNCTION("setAttribute",ihBrowse,"calcparamPris",STRING(hParentBuffer:BUFFER-FIELD("ButikkNr"):BUFFER-VALUE) + "¤0").
  DYNAMIC-FUNCTION("setAttribute",ihBrowse,"calcparamArtStrBeh",
                   "¤no¤" + STRING(hParentBuffer:BUFFER-FIELD("ButikkNr"):BUFFER-VALUE)).
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setSelectorAttributes C-Win 
PROCEDURE setSelectorAttributes :
/*------------------------------------------------------------------------------
  Purpose:     Definere inn tilleggselementer i select-box
  Parameters:  <none>
  Notes:       Kalles automatisk fra select-box
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihBrwSource AS HANDLE NO-UNDO.
DEF INPUT PARAM ihBrwTarget AS HANDLE NO-UNDO.

DEF VAR hLabel          AS HANDLE NO-UNDO.
DEF VAR hSelector       AS HANDLE NO-UNDO.
DEF VAR hPakkePrisFrame AS HANDLE NO-UNDO.
DEF VAR hSelectorWin    AS HANDLE NO-UNDO.
DEF VAR hSelectorSplitB AS HANDLE NO-UNDO.

hSelector = SOURCE-PROCEDURE.

IF bSelectRekv THEN DO:
  DYNAMIC-FUNCTION("adjustBrowseHeight" IN hSelector,-40).

  CREATE TEXT hLabel 
    ASSIGN FRAME            = ihBrwSource:FRAME
           X                = ihBrwSource:X
           Y                = ihBrwSource:Y + ihBrwSource:HEIGHT-PIXELS + 10
           FORMAT           = "x(256)"
           SCREEN-VALUE     = "Rekvisisjon:"
           WIDTH            = 20
           HEIGHT-PIXELS    = 21
           VISIBLE          = YES
           .
  
  CREATE FILL-IN hFiRekvNr
    ASSIGN FRAME            = ihBrwSource:FRAME
           NAME             = "rekvnr"
           DATA-TYPE        = "INTEGER"
           FORMAT           = ">>>>>>>>9"
           X                = ihBrwSource:X + 60
           Y                = ihBrwSource:Y + ihBrwSource:HEIGHT-PIXELS + 10
           WIDTH-PIXELS     = 90
           HEIGHT-PIXELS    = 21
           VISIBLE          = YES
           SENSITIVE        = YES
           TAB-STOP         = YES
           TOOLTIP          = "Rekvisjonsnr"
           SIDE-LABEL-HANDLE = hLabel
           .
  
  CREATE FILL-IN hFiRekvTekst
    ASSIGN FRAME            = ihBrwSource:FRAME
           NAME             = "rekvtekst"
           DATA-TYPE        = "CHARACTER"
           FORMAT           = "x(256)"
           X                = ihBrwSource:X + 155
           Y                = ihBrwSource:Y + ihBrwSource:HEIGHT-PIXELS + 10
           WIDTH-PIXELS     = 200
           HEIGHT-PIXELS    = 21
           VISIBLE          = YES
           SENSITIVE        = YES
           TAB-STOP         = YES
           TOOLTIP          = "Tekst"
           SIDE-LABEL-HANDLE = hLabel
           .
  
  DYNAMIC-FUNCTION("setAttribute",ihBrwTarget,"nextTabItem",STRING(hFiRekvNr)).
  
  IF cNyRekv NE "" THEN
    ASSIGN hFiRekvNr:SCREEN-VALUE    = ENTRY(1,cNyRekv,"|")
           hFiRekvTekst:SCREEN-VALUE = ENTRY(2,cNyRekv,"|")
           .
END.
ELSE DO:
  RUN LoadPanel IN hSelector ("PakkePrisPanel.w",OUTPUT hPakkePris).
  IF VALID-HANDLE(hPakkePris) THEN DO:

    DO ix = 1 TO 7:
      IF ix = 2 THEN
        ihBrwSource:GET-BROWSE-COLUMN(ix):WIDTH-PIXELS  = ihBrwSource:GET-BROWSE-COLUMN(ix):WIDTH-PIXELS - 43.
      ELSE IF ix = 3 THEN
        ihBrwSource:GET-BROWSE-COLUMN(ix):WIDTH-PIXELS  = ihBrwSource:GET-BROWSE-COLUMN(ix):WIDTH-PIXELS - 30.
      ELSE IF ix NE 5 THEN
        ihBrwSource:GET-BROWSE-COLUMN(ix):WIDTH-PIXELS  = ihBrwSource:GET-BROWSE-COLUMN(ix):WIDTH-PIXELS - 10.

      ihBrwTarget:GET-BROWSE-COLUMN(ix):WIDTH-PIXELS  = ihBrwSource:GET-BROWSE-COLUMN(ix):WIDTH-PIXELS.
      IF ix = 3 THEN
        ihBrwTarget:GET-BROWSE-COLUMN(ix):WIDTH-PIXELS  = ihBrwTarget:GET-BROWSE-COLUMN(ix):WIDTH-PIXELS - 20.
    END.

    ASSIGN hSelectorWin               = ihBrwSource:WINDOW
           hSelectorWin:WIDTH-PIXELS  = hSelectorWin:WIDTH-PIXELS + 400
           hSelectorWin:HEIGHT-PIXELS = hSelectorWin:HEIGHT-PIXELS + 200
           hSelectorSplitB            = DYNAMIC-FUNCTION("getSplitBarHandle",hSelectorWin,"x")
           hSelectSrcBrowse           = ihBrwSource
           hSelectSrcTilbField        = ihBrwSource:GET-BROWSE-COLUMN(6)
           hTargetSrcTilbField        = ihBrwTarget:GET-BROWSE-COLUMN(6)
           iOrgNumPakkeVarer          = ihBrwTarget:QUERY:NUM-RESULTS
           .
    APPLY "window-resized" TO hSelectorWin.
    hSelectorSplitB:X = hSelectorSplitB:FRAME:WIDTH-PIXELS / 2 - hSelectorSplitB:WIDTH-PIXELS / 2.

    APPLY "end-move" TO hSelectorSplitB.
    DYNAMIC-FUNCTION("adjustBrowseHeight" IN hSelector,-100).
    hPakkePrisFrame = DYNAMIC-FUNCTION("getFrameHandle" IN hPakkePris).
    hPakkePrisFrame:Y = ihBrwSource:Y + ihBrwSource:HEIGHT-PIXELS + 5. 
    RUN InitializeObject IN hPakkePris (ihBrwTarget,THIS-PROCEDURE,iPakkeIdx,hFieldMap:BUFFER-FIELD("KOrdre_id"):BUFFER-VALUE).

  END.
  ELSE DYNAMIC-FUNCTION("DoMessage",0,0,"Program for pakkepris er ikke installert","","").
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StartQuery C-Win 
PROCEDURE StartQuery :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ifKOrdre_id AS DEC NO-UNDO.

IF NOT VALID-HANDLE(hLinjeNrOverlay) THEN InitOverlays(YES).
DYNAMIC-FUNCTION("setAttribute",hBrowse,"basequery","WHERE KOrdre_id = " + STRING(ifKOrdre_id)).
DYNAMIC-FUNCTION("setCurrentObject",hBrowse).
RUN OpenQuery.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ValChngBrowseDropDown C-Win 
PROCEDURE ValChngBrowseDropDown :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
RUN SUPER.
getStat().
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION AddStr C-Win 
FUNCTION AddStr RETURNS LOGICAL
  ( INPUT ifArtikkelNr AS DEC,
    INPUT icStorl      AS CHAR,
    INPUT ifPlukkAnt   AS DEC,
    INPUT icAction     AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose: Legg til eller endre artikkel. Kalles fra artbassok.w 
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR bReturnFocus AS LOG   NO-UNDO.
DEF VAR cStrList     AS CHAR  NO-UNDO.
DEF VAR bOkStr       AS LOG   NO-UNDO.

DYNAMIC-FUNCTION("setCurrentObject",hToolbar).

IF ifPlukkAnt NE 0 AND ifPlukkAnt NE ? THEN DO:

  bOk = hFieldMap:FIND-FIRST("WHERE VareNr = '" + STRING(ifArtikkelNr) + "'"
                           + "  AND Storl  = '" + icStorl + "'") NO-ERROR.
  IF bOk THEN DO:
    hBrowse:SET-REPOSITIONED-ROW(hBrowse:DOWN,"conditional").
    hBrowse:QUERY:REPOSITION-TO-ROWID(hFieldMap:ROWID).
    hAntallOverlay:SCREEN-VALUE = STRING(ifPlukkAnt).
    DYNAMIC-FUNCTION("setCurrentObject",hAntallOverlay).
    RUN LeaveBrowseFillIn.
    RETURN YES.
  END.
END.
  
IF icAction = "add" THEN
  RUN NewRecord.
ELSE 
  APPLY "value-changed" TO hBrowse.

IF NOT VALID-HANDLE(hVareNrOverlay) THEN DO: 
  bEnableEdit = YES.
  InitOverlays(YES).
END.

hVareNrOverlay:SCREEN-VALUE = STRING(ifArtikkelNr). 
APPLY "tab" TO hVareNrOverlay.

IF icStorl NE "" THEN DO:
  hStorlOverlay:SCREEN-VALUE = icStorl.
  APPLY "tab" TO hStorlOverlay.
  
  IF ifPlukkAnt NE ? AND ifPlukkAnt NE 0 THEN
    ASSIGN hAntallOverlay:SCREEN-VALUE = STRING(ifPlukkAnt)
           bReturnFocus = YES.
  
  hAntallOverlay:MODIFIED = YES.
  APPLY "tab" TO hAntallOverlay.
END.
ELSE DO:
  APPLY "entry" TO hStorlOverlay.
  cStrList = REPLACE(REPLACE(
             DYNAMIC-FUNCTION("getFieldList","ArtBas;,StrType;AlfaFordeling",
                              "WHERE ArtikkelNr = " + STRING(ifArtikkelNr) + ",FIRST StrType OF ArtBas NO-LOCK")
             ," ","")," ","").
  IF hStorlOverlay:SCREEN-VALUE NE "" AND CAN-DO(cStrList,hStorlOverlay:SCREEN-VALUE) THEN
    bOkStr = YES.
  ELSE
    hStorlOverlay:SCREEN-VALUE = TRIM(ENTRY(1,cStrList)).
  APPLY "entry" TO hStorlOverlay.
END.

RETURN bReturnFocus.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION AdjustBrowseColumns C-Win 
FUNCTION AdjustBrowseColumns RETURNS LOGICAL
  ( INPUT ihBrowse     AS HANDLE,
    INPUT icBrowseName AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose: Juster kolonner på browser iht det orginale oppsettet. 
    Notes: Dette må gjøres før kolonnene byttes iht brukeroppsett,
           derfor denne funksjonen som kalles automatisk fra NewBrowe i ObjLib
------------------------------------------------------------------------------*/
ASSIGN ihBrowse:GET-BROWSE-COLUMN(1):WIDTH-PIXELS  = 30
       ihBrowse:GET-BROWSE-COLUMN(2):WIDTH-PIXELS  = 28
       ihBrowse:GET-BROWSE-COLUMN(3):WIDTH-PIXELS  = 60
       ihBrowse:GET-BROWSE-COLUMN(4):WIDTH-PIXELS  = 80
       ihBrowse:GET-BROWSE-COLUMN(5):WIDTH-PIXELS  = 40
       ihBrowse:GET-BROWSE-COLUMN(6):WIDTH-PIXELS  = 40
       ihBrowse:GET-BROWSE-COLUMN(7):WIDTH-PIXELS  = 35
       ihBrowse:GET-BROWSE-COLUMN(8):WIDTH-PIXELS  = 50
       ihBrowse:GET-BROWSE-COLUMN(9):WIDTH-PIXELS  = 57
       ihBrowse:GET-BROWSE-COLUMN(10):WIDTH-PIXELS  = 10
       ihBrowse:GET-BROWSE-COLUMN(11):WIDTH-PIXELS = 55
       ihBrowse:GET-BROWSE-COLUMN(13):WIDTH-PIXELS = 53

       hPrisColumn   = ihBrowse:GET-BROWSE-COLUMN(10)
       hTilbField    = ihBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("Tilbud")
       hAntallColumn = ihBrowse:GET-BROWSE-COLUMN(12)
       .

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getCurrentAntall C-Win 
FUNCTION getCurrentAntall RETURNS LOGICAL
  ( INPUT ihBuffer AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR hQuery AS HANDLE NO-UNDO.

CREATE QUERY hQuery.

hQuery:SET-BUFFERS(ihBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + ihBuffer:NAME).
hQuery:QUERY-OPEN().

hQuery:GET-FIRST().
REPEAT WHILE ihBuffer:AVAIL:
  bOk = hFieldMap:FIND-FIRST("WHERE VareNr = '" + STRING(ihBuffer:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE) + "'"
                           + "  AND Storl  = '" + ihBuffer:BUFFER-FIELD("Storl"):BUFFER-VALUE + "'") NO-ERROR.
  IF bOk THEN
    ihBuffer:BUFFER-FIELD("OrdreAntStr"):BUFFER-VALUE = hFieldMap:BUFFER-FIELD("Antall"):BUFFER-VALUE.
  ELSE
    ihBuffer:BUFFER-FIELD("OrdreAntStr"):BUFFER-VALUE = 0.

  ihBuffer:BUFFER-FIELD("PlukkAntStr"):BUFFER-VALUE = 0.
  hQuery:GET-NEXT().
END.

DELETE OBJECT hQuery NO-ERROR.

RETURN FALSE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getLocalQueryMaxValue C-Win 
FUNCTION getLocalQueryMaxValue RETURNS CHARACTER
  ( INPUT ihBrowseOrQuery AS HANDLE,
    INPUT icField         AS CHAR,
    INPUT ifIncrement     AS DEC ) : 
/*------------------------------------------------------------------------------
  Purpose:  Get max value of a field in a local query
    Notes:  Field must be type INT or DEC
------------------------------------------------------------------------------*/
DEF VAR hBuffer       AS HANDLE NO-UNDO.
DEF VAR hQuery        AS HANDLE NO-UNDO.
DEF VAR hQueryBuffer  AS HANDLE NO-UNDO.
DEF VAR fMax          AS DEC    NO-UNDO.

IF ihBrowseOrQuery:TYPE = "browse" THEN
  hBuffer = ihBrowseOrQuery:QUERY:GET-BUFFER-HANDLE(1).
ELSE
  hBuffer = ihBrowseOrQuery:GET-BUFFER-HANDLE(1).

CREATE BUFFER hQueryBuffer FOR TABLE hBuffer.
CREATE QUERY hQuery.
hQuery:SET-BUFFERS(hQueryBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + hQueryBuffer:NAME + " BY " + icField).
hQuery:QUERY-OPEN().
hQuery:GET-FIRST().
REPEAT WHILE hQueryBuffer:AVAIL:
  fMax = hQueryBuffer:BUFFER-FIELD(icField):BUFFER-VALUE.
  hQuery:GET-NEXT().
END.

DELETE OBJECT hQuery.
DELETE OBJECT hQueryBuffer.

RETURN STRING(fMax + ifIncrement).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getMyQueryStat C-Win 
FUNCTION getMyQueryStat RETURNS CHARACTER
  ( INPUT ihDataObject    AS HANDLE,
    INPUT icStatFields    AS CHAR,
    INPUT icCriteria      AS CHAR ) : 
/*------------------------------------------------------------------------------
  Purpose:  Sum up listed fields in a local query
    Notes:  If no fields are specified go and get them from the querystatfields attribute
            and then store the result back in the querystatfieldvalues
------------------------------------------------------------------------------*/
DEF VAR hBuffer       AS HANDLE NO-UNDO.
DEF VAR hQuery        AS HANDLE NO-UNDO.
DEF VAR hQueryBuffer  AS HANDLE NO-UNDO.
DEF VAR cStatFields   AS CHAR   NO-UNDO.
DEF VAR fStatValues   AS DEC    NO-UNDO EXTENT 100.
DEF VAR cReturnString AS CHAR   NO-UNDO.
DEF VAR iCount        AS INT    NO-UNDO.
DEF VAR lDec          AS DEC    NO-UNDO.
DEFINE VARIABLE iKoeff AS INTEGER     NO-UNDO.
IF icStatFields = "" THEN
  cStatFields = DYNAMIC-FUNCTION("getAttribute",ihDataObject,"querystatfields").
ELSE cStatFields = icStatFields.

IF ihDataObject:TYPE = "browse" THEN
  hBuffer = ihDataObject:QUERY:GET-BUFFER-HANDLE(1).
ELSE IF ihDataObject:TYPE = "query" THEN
  hBuffer = ihDataObject:GET-BUFFER-HANDLE(1).
ELSE hBuffer = ihDataObject.

CREATE BUFFER hQueryBuffer FOR TABLE hBuffer.
CREATE QUERY hQuery.
hQuery:SET-BUFFERS(hQueryBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + hQueryBuffer:NAME + " " + icCriteria).
hQuery:QUERY-OPEN().
hQuery:GET-FIRST().
REPEAT WHILE NOT hQuery:QUERY-OFF-END:
  iCount = iCount + 1.
  lDec = dec(hQueryBuffer:BUFFER-FIELD('VareNr'):BUFFER-VALUE) NO-ERROR.
  IF NOT ERROR-STATUS:ERROR THEN 
  DO ix = 1 TO NUM-ENTRIES(cStatFields):
    IF hQueryBuffer:BUFFER-FIELD("Antall"):BUFFER-VALUE > 0 THEN
        iKoeff = 1.
    ELSE
        iKoeff = -1.
    fStatValues[ix] = fStatValues[ix] + (hQueryBuffer:BUFFER-FIELD(ENTRY(ix,cStatFields)):BUFFER-VALUE * iKoeff).
  END.
  hQuery:GET-NEXT().
END.

DELETE OBJECT hQuery.
DELETE OBJECT hQueryBuffer.

IF icCriteria NE "" THEN RETURN STRING(fStatValues[1]).

DO ix = 1 TO NUM-ENTRIES(cStatFields):
  cReturnString = cReturnString + ENTRY(ix,cStatFields) + "|" + (IF fStatValues[ix] NE ? THEN STRING(fStatValues[ix]) ELSE "") + ";".
/*   IF fStatValues[ix] = ? THEN          */
/*     MESSAGE PROGRAM-NAME(1) SKIP       */
/*             ENTRY(ix,cStatFields) SKIP */
/*             VIEW-AS ALERT-BOX.         */
END.

IF icStatFields = "" THEN DO:
  DYNAMIC-FUNCTION("setAttribute",ihDataObject,"querystatfieldvalues",TRIM(cReturnString,";")).
  DYNAMIC-FUNCTION("setAttribute",ihDataObject,"recordcount",STRING(iCount)).
  DYNAMIC-FUNCTION("ViewRecordCount",ihDataObject).
END.
ELSE 
  cReturnString = "rowcount|" + STRING(iCount) + ";" + cReturnString.

RETURN TRIM(cReturnString,";").

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getParentBrowse C-Win 
FUNCTION getParentBrowse RETURNS HANDLE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

RETURN hBrowse.  

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getStat C-Win 
FUNCTION getStat RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cQueryStat AS CHAR NO-UNDO.
DEF VAR fTotRabatt AS DEC  NO-UNDO.
DEF VAR lSumOrdreEksMva AS DEC NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:

  ASSIGN fiSumOrdre:SCREEN-VALUE    = ""
         fiSumOrdreEksMva:SCREEN-VALUE = ""
         fiSumRabattKr:SCREEN-VALUE  = ""
         fiSumOrdreDb:SCREEN-VALUE  = ""
         fiSumOrdreDb%:SCREEN-VALUE  = ""
         fiSumOrdre:HIDDEN = NOT CAN-DO(DYNAMIC-FUNCTION("getAttribute",hBrowse,"currviewfields"),"NettoLinjeSum")
         fiSumOrdreLabel:HIDDEN = NOT CAN-DO(DYNAMIC-FUNCTION("getAttribute",hBrowse,"currviewfields"),"NettoLinjeSum")
         fiSumOrdreEksMva:HIDDEN = NOT CAN-DO(DYNAMIC-FUNCTION("getAttribute",hBrowse,"currviewfields"),"SumEksMvaKr")
         fiSumOrdreEksMvaLabel:HIDDEN = NOT CAN-DO(DYNAMIC-FUNCTION("getAttribute",hBrowse,"currviewfields"),"SumEksMvaKr")
         fiSumOrdreDb:HIDDEN = NOT CAN-DO(DYNAMIC-FUNCTION("getAttribute",hBrowse,"currviewfields"),"DbKr")
         fiSumOrdreDbLabel:HIDDEN = NOT CAN-DO(DYNAMIC-FUNCTION("getAttribute",hBrowse,"currviewfields"),"DbKr")
         fiSumOrdreDb%:HIDDEN = fiSumOrdreDb:HIDDEN
         fiSumOrdreDb%Label:HIDDEN = fiSumOrdreDb:HIDDEN
         fiSumRabattKr:HIDDEN = NOT CAN-DO(DYNAMIC-FUNCTION("getAttribute",hBrowse,"currviewfields"),"LinjeRabattKr") OR
                                NOT CAN-DO(DYNAMIC-FUNCTION("getAttribute",hBrowse,"currviewfields"),"OrdreRabattKr") OR
                                NOT CAN-DO(DYNAMIC-FUNCTION("getAttribute",hBrowse,"currviewfields"),"KundeRabattKr")
         fiSumRabattKrLabel:HIDDEN = fiSumRabattKr:HIDDEN
         .

  cQueryStat = getMyQueryStat(hBrowse,"NettoLinjeSum,SumEksMvaKr,DbKr,LinjeRabattKr,OrdreRabattKr,KundeRabattKr","").
  
  IF NUM-ENTRIES(cQueryStat,";") > 1 THEN DO ix = 2 TO NUM-ENTRIES(cQueryStat,";"):
    CASE ix:
      WHEN 2 THEN fiSumOrdre:SCREEN-VALUE    = ENTRY(2,ENTRY(ix,cQueryStat,";"),"|").
      WHEN 3 THEN 
        DO:
          ASSIGN
            lSumOrdreEksMva = dec(ENTRY(2,ENTRY(ix,cQueryStat,";"),"|"))
            fiSumOrdreEksMva:SCREEN-VALUE = ENTRY(2,ENTRY(ix,cQueryStat,";"),"|").
        END.
      WHEN 4 THEN fiSumOrdreDb:SCREEN-VALUE   = ENTRY(2,ENTRY(ix,cQueryStat,";"),"|").
      WHEN 5 THEN fiSumRabattKr:SCREEN-VALUE  = ENTRY(2,ENTRY(ix,cQueryStat,";"),"|").
      WHEN 6 THEN fiSumRabattKr:SCREEN-VALUE  = STRING(DEC(fiSumRabattKr:SCREEN-VALUE) + DEC(ENTRY(2,ENTRY(ix,cQueryStat,";"),"|"))).
      WHEN 7 THEN fiSumRabattKr:SCREEN-VALUE  = STRING(DEC(fiSumRabattKr:SCREEN-VALUE) + DEC(ENTRY(2,ENTRY(ix,cQueryStat,";"),"|"))).
    END CASE.
  END.
  fiSumOrdreDb%:SCREEN-VALUE  = STRING(DEC(fiSumOrdreDb:SCREEN-VALUE) / lSumOrdreEksMva * 100).
  IF fiSumOrdreDb%:SCREEN-VALUE = ? OR fiSumOrdreDb%:SCREEN-VALUE = '?' THEN 
    fiSumOrdreDb%:SCREEN-VALUE = '0'.
END.
  
RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getToolbarHandle C-Win 
FUNCTION getToolbarHandle RETURNS HANDLE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

RETURN hToolbar.

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

/* DYNAMIC-FUNCTION("setNoResizeX",THIS-PROCEDURE:CURRENT-WINDOW,FRAME {&FRAME-NAME}:HANDLE,"rectLagerStatus").  */
DYNAMIC-FUNCTION("setNoResizeY",THIS-PROCEDURE:CURRENT-WINDOW,FRAME {&FRAME-NAME}:HANDLE,"rectToolbar,rectLagerStatus").
DYNAMIC-FUNCTION("setAddMoveX",THIS-PROCEDURE:CURRENT-WINDOW,FRAME {&FRAME-NAME}:HANDLE,"fiSumOrdre,fiSumOrdreEksMva,fiSumOrdreDb,fiSumOrdreDb%,fiSumRabattKr,fiSumOrdreLabel,fiSumOrdreEksMvaLabel,fiSumOrdreDbLabel,fiSumOrdreDb%Label,fiSumRabattKrLabel").
DYNAMIC-FUNCTION("setAddMoveY",THIS-PROCEDURE:CURRENT-WINDOW,FRAME {&FRAME-NAME}:HANDLE,"Varespesifikasjon,ArbeidsBeskr,MomsKode,Leveringsdato,rectLagerStatus,rsLagerStatusSalg,fiSumOrdre,fiSumOrdreEksMva,fiSumOrdreDb,fiSumOrdreDb%,fiSumRabattKr,fiSumOrdreLabel,fiSumOrdreEksMvaLabel,fiSumOrdreDbLabel,fiSumOrdreDb%Label,fiSumRabattKrLabel,RefNr,RefTekst").
/* DYNAMIC-FUNCTION("setAddResizeX",THIS-PROCEDURE:CURRENT-WINDOW,FRAME {&FRAME-NAME}:HANDLE,"Varespesifikasjon,ArbeidsBeskr").  */

/* Sender beskjed til container om at disse skal følge bevegelser til splitbar, dvs krympe når den flyttes til høyre: */
/* PUBLISH "addFollowSplitBar" (THIS-PROCEDURE:CURRENT-WINDOW,                                 */
/*                              STRING(Varespesifikasjon:HANDLE IN FRAME {&FRAME-NAME}) + ","  */
/*                            + STRING(ArbeidsBeskr:HANDLE)                                    */
/*                              ).    
                                                         */
/* DYNAMIC-FUNCTION("setAnchor",THIS-PROCEDURE:CURRENT-WINDOW,BrwKOrdreLinje:HANDLE IN FRAME {&FRAME-NAME},  */
/*                  STRING(rectLagerStatus:HANDLE)                                                       */
/*                 ,"Y").                                                                                */

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION initOverlays C-Win 
FUNCTION initOverlays RETURNS LOGICAL
  ( INPUT ibEnable AS LOG) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
IF VALID-HANDLE(hMomsKodOverlay) THEN DO:
  IF hParentBuffer:AVAIL AND hParentBuffer:BUFFER-FIELD("Mvafri"):BUFFER-VALUE THEN
    hMomsKodOverlay:LIST-ITEM-PAIRS = "0|0".
  ELSE
    hMomsKodOverlay:LIST-ITEM-PAIRS = cMomsKosList.
END.

IF ibEnable = bCurrEditEnable THEN RETURN FALSE.

IF NOT ibEnable THEN DO:
  DYNAMIC-FUNCTION("DoLockWindow",THIS-PROCEDURE:CURRENT-WINDOW).
  DYNAMIC-FUNCTION("DeleteObject",hLinjeNrOverlay).
  DYNAMIC-FUNCTION("DeleteObject",hVareNrOverlay).
  DYNAMIC-FUNCTION("DeleteObject",hVareTekstOverlay).
  DYNAMIC-FUNCTION("DeleteObject",hStorlOverlay).
  DYNAMIC-FUNCTION("DeleteObject",hAntallOverlay).
  DYNAMIC-FUNCTION("DeleteObject",hLinjeRabOverlay).
  DYNAMIC-FUNCTION("DeleteObject",hNettoPrisOverlay).
  DYNAMIC-FUNCTION("DeleteObject",hMomsKodOverlay).
  DYNAMIC-FUNCTION("DoLockWindow",?).
END.

IF ibEnable AND NOT VALID-HANDLE(hLinjeNrOverlay) THEN DO:
  hLinjeNrOverlay = DYNAMIC-FUNCTION("NewBrowseFillIn",
                    hBrowse,
                    "KOrdreLinjeNr",
                    "KOrdreLinjeNr",
                    "","","", 
                    "").
  DYNAMIC-FUNCTION("setAttribute",hLinjeNrOverlay,"refreshrow","yes").
  DYNAMIC-FUNCTION("CreateOverlayLink",hBrowse,hLinjeNrOverlay,"KOrdreLinjeNr").   
  

  hVareNrOverlay = DYNAMIC-FUNCTION("NewBrowseFillIn",
                    hBrowse,
                    "VareNr",
                    "VareNr",
                    "ArtBas"
                    + ";Beskr"
                    + ";ArtikkelNr"
                    + ";+Pris|DECIMAL|>><>>><>>9.99|artpris_pris_2.p(ROWID)|Pris"
                    + ";AnbefaltPris|Anbef.pris"
                    + ";+ArtStrBeh|INTEGER|->>>>9|artbas_sjekk_strbeh.p(ROWID)|Beh"
                    + ";+TilbPris|LOGICAL|*/|art_paa_tilbud.p(ROWID)|Tilb"
                    + ";Vg"
                  + ",VarGr"
                    + ";VgBeskr"
                    ,"WHERE false,FIRST VarGr OF ArtBas NO-LOCK OUTER-JOIN"
                   ,"ArtikkelNr", /* Parameters for the lookup */
                    "").
  hVareNrOverlay:HELP = "Angi artikkelnr, leverandørens artikkelnr eller strekkode".
  DYNAMIC-FUNCTION("setAttribute",hVareNrOverlay,"refreshrow","yes").
  DYNAMIC-FUNCTION("CreateOverlayLink",hBrowse,hVareNrOverlay,"VareNr").
  

  hVaretekstOverlay = DYNAMIC-FUNCTION("NewBrowseFillIn",
                                      hBrowse,
                                      "Varetekst",
                                      "Varetekst",
                                      "","","","").
  DYNAMIC-FUNCTION("CreateOverlayLink",hBrowse,hVaretekstOverlay,"Varetekst").
  hVaretekstOverlay:NAME = "Varetekst".
    
  hStorlOverlay = DYNAMIC-FUNCTION("NewBrowseFillIn",
                    hBrowse,
                    "Storl",
                    "Storl",
                    "_file"
                    + ";+Storl|CHARACTER|x(5)||Str"
                    + ";+Lagant|DECIMAL|->><>>9||Beh"
                   ,"WHERE false"
                   ,"Storl"
                   ,"").

  DYNAMIC-FUNCTION("setAttribute",hStorlOverlay,"refreshrow","yes").          
  DYNAMIC-FUNCTION("CreateOverlayLink",hBrowse,hStorlOverlay,"Storl").   
  
  hAntallOverlay = DYNAMIC-FUNCTION("NewBrowseFillIn",
                                       hBrowse,
                                       "Antall",
                                       "Antall",
                                       "","","","").
  DYNAMIC-FUNCTION("CreateOverlayLink",hBrowse,hAntallOverlay,"Antall").
  DYNAMIC-FUNCTION("setAttribute",hAntallOverlay,"refreshrow","yes").
  
  hLinjeRabOverlay = DYNAMIC-FUNCTION("NewBrowseFillIn",
                                       hBrowse,
                                       "LinjeRab%",
                                       "LinjeRab%",
                                       "","","","").
  DYNAMIC-FUNCTION("CreateOverlayLink",hBrowse,hLinjerabOverlay,"LinjeRab%").
  DYNAMIC-FUNCTION("setAttribute",hLinjeRabOverlay,"refreshrow","yes").
  hLinjeRabOverlay:HELP = "Dersom linjerabatt angis så overstyrer den evt kunde(varegruppe) rabatt".

  hNettoPrisOverlay = DYNAMIC-FUNCTION("NewBrowseFillIn",
                                       hBrowse,
                                       "NettoPris",
                                       "NettoPris",
                                       "","","","").
  DYNAMIC-FUNCTION("CreateOverlayLink",hBrowse,hNettoPrisOverlay,"NettoPris").
  DYNAMIC-FUNCTION("setAttribute",hNettoPrisOverlay,"refreshrow","yes").

  hMomsKodOverlay = DYNAMIC-FUNCTION("NewBrowseDropDown",hBrowse,"MomsKod","MomsKod"
                  ,"Moms;MomsKod|Beskrivelse;MomsKod","WHERE true"
                  ,"").
  DYNAMIC-FUNCTION("CreateOverlayLink",hBrowse,hMomsKodOverlay,"MomsKod").
  DYNAMIC-FUNCTION("setAttribute",hMomsKodOverlay,"refreshrow","yes").

  cMomsKosList = hMomsKodOverlay:LIST-ITEM-PAIRS.
END.

bCurrEditEnable = ibEnable.

RETURN TRUE.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setQuery C-Win 
FUNCTION setQuery RETURNS LOGICAL
  (INPUT ihQuery AS HANDLE) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
hParentQuery = ihQuery.

IF ihQuery:TYPE = "browse" THEN
  hParentBuffer = ihQuery:QUERY:GET-BUFFER-HANDLE(1).
ELSE
  hParentBuffer = ihQuery:GET-BUFFER-HANDLE(1).

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setStrKode C-Win 
FUNCTION setStrKode RETURNS LOGICAL
  ( INPUT icStorl AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose: Assign StrKode to order-line 
    Notes:  
------------------------------------------------------------------------------*/
IF NOT DYNAMIC-FUNCTION("runproc","kordrelinje_set_strkode.p",hFieldMap:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE + "|" + icStorl,?) THEN DO:
  DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").
  RETURN FALSE.   
END.
ELSE RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setVareNr C-Win 
FUNCTION setVareNr RETURNS LOGICAL
  ( INPUT icVareNr AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose: Assign StrKode to order-line 
    Notes:  
------------------------------------------------------------------------------*/
IF NOT DYNAMIC-FUNCTION("runproc","kordrelinje_set_varenr.p",hFieldMap:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE 
                                                          + "|" + icVareNr 
                                                          + "|" + hFieldMap:BUFFER-FIELD("Storl"):BUFFER-VALUE
                                 ,?) THEN DO:
  DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").
  RETURN FALSE.   
END.
ELSE DO:
  hVareNrOverlay:SCREEN-VALUE = DYNAMIC-FUNCTION("getTransactionMessage").
  RETURN YES.
END.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION VisLagerStatus C-Win 
FUNCTION VisLagerStatus RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  Sett innhold i grid-objekt for lagerstatus
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cLagerInfo AS CHAR NO-UNDO.
DEF VAR cRows      AS CHAR NO-UNDO.

DEF VAR cCurrLabels  AS CHAR NO-UNDO.
DEF VAR cNewLabels   AS CHAR NO-UNDO.
DEF VAR cTranslation AS CHAR NO-UNDO.

hLagerStatusFrame:MOVE-TO-TOP().

IF NOT hFieldMap:AVAIL OR hFieldMap:BUFFER-FIELD("VareNr"):BUFFER-VALUE = "" THEN DO:
  DYNAMIC-FUNCTION("setLabels" IN hLagerStatus,"").
  RETURN FALSE.
END.

IF rsLagerStatusSalg:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "1" THEN DO:
  IF DYNAMIC-FUNCTION("runproc","artikkel_lagerinfo.p",hFieldMap:BUFFER-FIELD("VareNr"):BUFFER-VALUE + ";" + STRING(hParentBuffer:BUFFER-FIELD("ButikkNr"):BUFFER-VALUE),?) THEN DO:
    cLagerInfo = DYNAMIC-FUNCTION("getTransactionMessage").

    IF cLagerInfo = "" THEN RETURN NO.
                
    ASSIGN cCurrLabels  = REPLACE(ENTRY(1,cLagerInfo,"¤"),",","|")
           cTranslation = DYNAMIC-FUNCTION("getStringTranslation",THIS-PROCEDURE:FILE-NAME,"flexgridlabel",cCurrLabels).
    DO ix = 1 TO NUM-ENTRIES(cTranslation,"|"):
      cNewLabels = cNewLabels + (IF ENTRY(ix,cTranslation,"|") NE "" THEN ENTRY(ix,cTranslation,"|") ELSE ENTRY(ix,cCurrLabels,"|")) + "|".
    END.
    DYNAMIC-FUNCTION("setLabels" IN hLagerStatus,SUBSTR(cNewLabels,1,LENGTH(cNewLabels) - 1)).

/*     DYNAMIC-FUNCTION("setLabels" IN hLagerStatus,REPLACE(ENTRY(1,cLagerInfo,"¤"),",","|")). */

    IF NUM-ENTRIES(cLagerInfo,"¤") > 1 THEN DO ix = 1 TO NUM-ENTRIES(ENTRY(2,cLagerInfo,"¤"),";"):
      DYNAMIC-FUNCTION("addRow" IN hLagerStatus,ENTRY(ix,ENTRY(2,cLagerInfo,"¤"),";")).
    END.
    IF NUM-ENTRIES(cLagerInfo,"¤") = 3 THEN DO:
      DO ix = 1 TO NUM-ENTRIES(ENTRY(3,cLagerInfo,"¤"),";"):
        DYNAMIC-FUNCTION("addRow" IN hLagerStatus,ENTRY(ix,ENTRY(3,cLagerInfo,"¤"),";")).
      END.
      DYNAMIC-FUNCTION("setLabelRows" IN hLagerStatus,3).
    END.

    DYNAMIC-FUNCTION("setLabelColumns" IN hLagerStatus,2).
  END.
  ELSE DYNAMIC-FUNCTION("setLabels" IN hLagerStatus,"").
END.
ELSE DO:
  IF DYNAMIC-FUNCTION("runproc","artikkel_salg.p",hFieldMap:BUFFER-FIELD("VareNr"):BUFFER-VALUE + ";" + STRING(hParentBuffer:BUFFER-FIELD("ButikkNr"):BUFFER-VALUE),?) THEN DO:
    cLagerInfo = DYNAMIC-FUNCTION("getTransactionMessage").
    
    IF cLagerInfo = "" THEN RETURN NO.

    ASSIGN cCurrLabels  = REPLACE(ENTRY(1,cLagerInfo,"¤"),",","|")
           cTranslation = DYNAMIC-FUNCTION("getStringTranslation",THIS-PROCEDURE:FILE-NAME,"flexgridlabel",cCurrLabels).
    DO ix = 1 TO NUM-ENTRIES(cTranslation,"|"):
      cNewLabels = cNewLabels + (IF ENTRY(ix,cTranslation,"|") NE "" THEN ENTRY(ix,cTranslation,"|") ELSE ENTRY(ix,cCurrLabels,"|")) + "|".
    END.
    DYNAMIC-FUNCTION("setLabels" IN hLagerStatus,SUBSTR(cNewLabels,1,LENGTH(cNewLabels) - 1)).

/*     DYNAMIC-FUNCTION("setLabels" IN hLagerStatus,REPLACE(ENTRY(1,cLagerInfo,"¤"),",","|")). */

    IF NUM-ENTRIES(cLagerInfo,"¤") > 1 THEN DO ix = 1 TO NUM-ENTRIES(ENTRY(2,cLagerInfo,"¤"),";"):
      DYNAMIC-FUNCTION("addRow" IN hLagerStatus,ENTRY(ix,ENTRY(2,cLagerInfo,"¤"),";")).
    END.
    IF NUM-ENTRIES(cLagerInfo,"¤") = 3 THEN DO:
      DO ix = 1 TO NUM-ENTRIES(ENTRY(3,cLagerInfo,"¤"),";"):
        DYNAMIC-FUNCTION("addRow" IN hLagerStatus,ENTRY(ix,ENTRY(3,cLagerInfo,"¤"),";")).
      END.
      DYNAMIC-FUNCTION("setLabelRows" IN hLagerStatus,3).
    END.

    DYNAMIC-FUNCTION("setLabelColumns" IN hLagerStatus,1).
  END.
  ELSE DYNAMIC-FUNCTION("setLabels" IN hLagerStatus,"").
END.

DYNAMIC-FUNCTION("adjustColumns" IN hLagerStatus).

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

