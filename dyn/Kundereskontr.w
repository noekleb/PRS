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

DEF VAR bOk               AS LOG    NO-UNDO.
DEF VAR ix                AS INT    NO-UNDO.
DEF VAR iReturn           AS INT    NO-UNDO.
DEF VAR hParent           AS HANDLE NO-UNDO.

DEF VAR hParentBuffer     AS HANDLE NO-UNDO.
DEF VAR hBrowse           AS HANDLE NO-UNDO.
DEF VAR hFieldMap         AS HANDLE NO-UNDO.
DEF VAR hToolbar          AS HANDLE NO-UNDO.
DEF VAR hTabFolder        AS HANDLE NO-UNDO.
DEF VAR hBtnPayment       AS HANDLE NO-UNDO.
DEF VAR hMenuPayment      AS HANDLE NO-UNDO.
DEF VAR hMenuCredNote     AS HANDLE NO-UNDO.
DEF VAR hMenuPayOut       AS HANDLE NO-UNDO.
DEF VAR hMenuDebet        AS HANDLE NO-UNDO.
DEF VAR hMenuComplaint    AS HANDLE NO-UNDO.
DEF VAR hMenuBong         AS HANDLE NO-UNDO.

DEF VAR hCurrTabProc      AS HANDLE NO-UNDO.
DEF VAR hCurrTabFrame     AS HANDLE NO-UNDO.
DEF VAR iCurrTab          AS INT    NO-UNDO.

DEF VAR hReklamert        AS HANDLE NO-UNDO.
DEF VAR hBrowseColumns    AS HANDLE NO-UNDO EXTENT 30.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME frmFaktLinje

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rectBrowse rectToolbar rectFolder ~
cbBilagstype rsAlle tbBong Notat 
&Scoped-Define DISPLAYED-OBJECTS cbBilagstype rsAlle tbBong Notat 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getBongId C-Win 
FUNCTION getBongId RETURNS DECIMAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getFrameHandle C-Win 
FUNCTION getFrameHandle RETURNS HANDLE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getParentBrowse C-Win 
FUNCTION getParentBrowse RETURNS HANDLE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD InitializeResize C-Win 
FUNCTION InitializeResize RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setFilter C-Win 
FUNCTION setFilter RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setMenuAccess C-Win 
FUNCTION setMenuAccess RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD TabChanged C-Win 
FUNCTION TabChanged RETURNS LOGICAL
  ( INPUT iiTab AS INT )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE cbBilagstype AS CHARACTER FORMAT "X(256)":U 
     LABEL "Type" 
     VIEW-AS COMBO-BOX INNER-LINES 20
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 29 BY 1 NO-UNDO.

DEFINE VARIABLE Notat AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 141 BY 1.91
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE rsAlle AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Åpne poster", 1,
"Avsluttede", 2,
"Alle", 3
     SIZE 39 BY .95 NO-UNDO.

DEFINE RECTANGLE rectBrowse
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 141.8 BY 13.1.

DEFINE RECTANGLE rectFolder
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 141 BY 7.62.

DEFINE RECTANGLE rectToolbar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 17 BY 1.19.

DEFINE VARIABLE tbBong AS LOGICAL INITIAL no 
     LABEL "Kun poster med bong-referanse" 
     VIEW-AS TOGGLE-BOX
     SIZE 34 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME frmFaktLinje
     cbBilagstype AT ROW 1.24 COL 23 COLON-ALIGNED
     rsAlle AT ROW 1.33 COL 103 NO-LABEL
     tbBong AT ROW 1.38 COL 58
     Notat AT ROW 15.95 COL 1.6 NO-LABEL
     rectBrowse AT ROW 2.67 COL 1.2
     rectToolbar AT ROW 1.19 COL 1.6
     rectFolder AT ROW 18.1 COL 1.6
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 143.4 BY 24.86.


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
         HEIGHT             = 24.86
         WIDTH              = 143.4
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


&Scoped-define SELF-NAME cbBilagstype
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbBilagstype C-Win
ON VALUE-CHANGED OF cbBilagstype IN FRAME frmFaktLinje /* Type */
DO:
  setFilter().
  DYNAMIC-FUNCTION("setCurrentObject",hBrowse).
  RUN OpenQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rsAlle
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rsAlle C-Win
ON VALUE-CHANGED OF rsAlle IN FRAME frmFaktLinje
DO:
  setFilter().
  DYNAMIC-FUNCTION("setCurrentObject",hBrowse).
  RUN OpenQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tbBong
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tbBong C-Win
ON VALUE-CHANGED OF tbBong IN FRAME frmFaktLinje /* Kun poster med bong-referanse */
DO:
  setFilter().
  DYNAMIC-FUNCTION("setCurrentObject",hBrowse).
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
  PUBLISH "InvalidateHandle".
/*   RUN disable_UI.  */
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
{incl/supptrigg.i hFieldMap}

ON 'alt-s':U OF FRAME {&FRAME-NAME} ANYWHERE 
  PUBLISH "AltSKundeOrdre" ("KundeView.w").

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BongRecord C-Win 
PROCEDURE BongRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
RUN gviskvittokopi.w (0,0,0,0,0).
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
RUN SUPER.
hCurrObject = DYNAMIC-FUNCTION("getCurrentObject").
IF DEC(hCurrObject:SCREEN-VALUE) NE 0 THEN
  APPLY "tab" TO hCurrObject.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ComplaintRecord C-Win 
PROCEDURE ComplaintRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR dReklamert   AS DATE  NO-UNDO.
DEF VAR cNotat       AS CHAR NO-UNDO.

IF hBrowse:NUM-SELECTED-ROWS > 1 THEN DO: 
  DYNAMIC-FUNCTION("DoMessage",0,0,"Kun én faktura kan velges for reklamasjon","","").
  RETURN.
END.
IF hFieldMap:BUFFER-FIELD("BilagsType"):BUFFER-VALUE NE 1 THEN DO:
  DYNAMIC-FUNCTION("DoMessage",0,0,"Ugyldig bilagstype for reklamasjon","","").
  RETURN.
END.

RUN KunderesReklamasjon.w (IF hFieldMap:BUFFER-FIELD("Reklamert"):BUFFER-VALUE NE ? THEN hFieldMap:BUFFER-FIELD("Reklamert"):BUFFER-VALUE ELSE TODAY,
                           hFieldMap:BUFFER-FIELD("Notat"):BUFFER-VALUE
                          ,OUTPUT dReklamert,OUTPUT cNotat,OUTPUT bOK).
  
IF bOk THEN DO:
  IF DYNAMIC-FUNCTION("DoUpdate","Kundereskontr","",
                       "Reskontro_id",
                       STRING(hFieldMap:BUFFER-FIELD("Reskontro_id"):BUFFER-VALUE),
                       "Reklamert,Notat",
                       (IF dReklamert NE ? THEN STRING(dReklamert) ELSE "?") + "|" + cNotat,
                       TRUE) THEN 
    RUN RefreshCurrentRow IN hParent NO-ERROR.
  ELSE DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CredNoteRecord C-Win 
PROCEDURE CredNoteRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:      Endret av BHa 15.03.06:
              - Tillater kreditering av purregebyr (kun hvis beløp = saldo, se setMenuAccess)
------------------------------------------------------------------------------*/
DEF VAR fKreditBelop  AS DEC  NO-UNDO.
DEF VAR dKredit       AS DATE NO-UNDO.
DEF VAR cIdKredit     AS CHAR NO-UNDO.
DEF VAR fSumDebet     AS DEC  NO-UNDO.
DEF VAR fKredit       AS DEC  NO-UNDO.
DEF VAR fiNotat       AS CHAR NO-UNDO.
DEF VAR cNotat        AS CHAR NO-UNDO.

IF hBrowse:NUM-SELECTED-ROWS > 1 THEN DO: 
  DYNAMIC-FUNCTION("DoMessage",0,0,"Kun én faktura kan velges ved kreditering","","").
  RETURN.
END.
IF hFieldMap:BUFFER-FIELD("BilagsType"):BUFFER-VALUE NE 1 AND 
   hFieldMap:BUFFER-FIELD("BilagsType"):BUFFER-VALUE NE 11 THEN DO:
  DYNAMIC-FUNCTION("DoMessage",0,0,"Kun bilag av type Faktura (1) eller Purregebyr (11) kan velges ved kreditering","","").
  RETURN.
END.

cIdKredit = STRING(hFieldMap:BUFFER-FIELD("Reskontro_id"):BUFFER-VALUE).

IF hFieldMap:BUFFER-FIELD("BilagsType"):BUFFER-VALUE = 1 THEN
  RUN KunderesKrediterFakt.w ((IF hFieldMap:BUFFER-FIELD("Saldo"):BUFFER-VALUE NE 0 THEN 
                                hFieldMap:BUFFER-FIELD("Saldo"):BUFFER-VALUE
                               ELSE hFieldMap:BUFFER-FIELD("Belop"):BUFFER-VALUE)
                              ,OUTPUT fKreditBelop,OUTPUT cNotat).

ELSE IF DYNAMIC-FUNCTION("DoMessage",0,1,"Bekreft kreditering av purregebyr","","") = 1 THEN
  fKreditBelop = hFieldMap:BUFFER-FIELD("Saldo"):BUFFER-VALUE.
  
IF fKreditBelop NE 0 THEN DO:
  IF DYNAMIC-FUNCTION("runproc","kunderes_krednota.p",
                       STRING(DYNAMIC-FUNCTION("getKundeNr" IN hParent))
                     + "|" + cIdKredit 
                     + "|" + STRING(fKreditBelop)
                     + "|" + cNotat
                      ,?) THEN 
    RUN RefreshCurrentRow IN hParent NO-ERROR.
  ELSE DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DebetRecord C-Win 
PROCEDURE DebetRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

IF hBrowse:NUM-SELECTED-ROWS > 1 THEN DO: 
  DYNAMIC-FUNCTION("DoMessage",0,0,"Kun én kreditpost kan velges for korreksjon","","").
  RETURN.
END.
IF hFieldMap:BUFFER-FIELD("BilagsType"):BUFFER-VALUE NE 2 AND hFieldMap:BUFFER-FIELD("BilagsType"):BUFFER-VALUE NE 3 THEN DO:
  DYNAMIC-FUNCTION("DoMessage",0,0,"Ugyldig bilagstype for debet-korrigering","","").
  RETURN.
END.

IF DYNAMIC-FUNCTION("DoMessage",0,1,"Reverser kreditpostering pålydende " + 
                    STRING(hFieldMap:BUFFER-FIELD("Belop"):BUFFER-VALUE),"","") = 1 THEN DO:
  IF DYNAMIC-FUNCTION("runproc","kunderes_debet_korr.p",
                       STRING(DYNAMIC-FUNCTION("getKundeNr" IN hParent))
                     + "|" + STRING(hFieldMap:BUFFER-FIELD("Reskontro_id"):BUFFER-VALUE)
                     + "|" + STRING(hFieldMap:BUFFER-FIELD("Belop"):BUFFER-VALUE * -1)
                     + "|"
                      ,?) THEN 
    RUN RefreshCurrentRow IN hParent NO-ERROR.
  ELSE DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DeselectRecord C-Win 
PROCEDURE DeselectRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
hBrowse:DESELECT-FOCUSED-ROW().
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

IF hFieldMap:AVAIL THEN DO: 
  IF iCurrTab = 1 THEN DO:
    DYNAMIC-FUNCTION("DeleteObjectLink",hBrowse,DYNAMIC-FUNCTION("getPageQuery" IN hTabFolder,iCurrTab)).
    DYNAMIC-FUNCTION("CreateParentLink",DYNAMIC-FUNCTION("getPageQuery" IN hTabFolder,iCurrTab),hBrowse,
                     (IF hFieldMap:BUFFER-FIELD("Belop"):BUFFER-VALUE > 0 THEN 
                       "DReskontro_id"
                      ELSE "KReskontro_id")
                   + ";Reskontro_id").
  END.
END.

RUN SUPER.

setMenuAccess().

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
  DISPLAY cbBilagstype rsAlle tbBong Notat 
      WITH FRAME frmFaktLinje.
  ENABLE rectBrowse rectToolbar rectFolder cbBilagstype rsAlle tbBong Notat 
      WITH FRAME frmFaktLinje.
  {&OPEN-BROWSERS-IN-QUERY-frmFaktLinje}
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
DEF VAR iy          AS INT    NO-UNDO.
DEF VAR hPageObject AS HANDLE NO-UNDO.
DEF VAR hTabFrame   AS HANDLE NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:

  DYNAMIC-FUNCTION("NewObject",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE,"procedure").

  ASSIGN cbBilagsType:DELIMITER = "|"
         cbBilagsType:LIST-ITEM-PAIRS = "||" 
                   + DYNAMIC-FUNCTION("getFieldList","BilagsType;BTTekst|BilagsType;BilagsType",
                                      "where Bruk GE 2")
         cbBilagsType:SCREEN-VALUE = cbBilagsType:ENTRY(1)
         .
  /* Create the browse: */
   hBrowse = DYNAMIC-FUNCTION("NewBrowse",
                           rectBrowse:HANDLE,        
                           100,                      
                           "MULTIPLE",                       
                           "Kundereskontr"   
                           + ";Reskontro_id"
                           + ";FakturaNr"
                           + ";!BArtNr"
                           + ";!BilagsType"
                           + ";FakturertDato"
                           + ";ForfallsDato"
                           + ";Belop"
                           + ";Saldo"
                           + ";PurreTrinn"
                           + ";SistePurredato"
                           + ";Reklamert"
                           + ";RegnskapsPeriode"
                           + ";BilagsNr"
                           + ";OverfortRegnskap"
                           + ";OpprForfallsDato"
                           + ";KID"
                           + ";Notat"
                           + ";RegistrertDato"
                           + ";RegistrertAv"
                           + ";BuntNr"
                           + ";!B_Id"
                         + ",BilagsArt"
                           + ";BArtTekst|Art"
                         + ",BilagsType"
                           + ";BTTekst|Type"
                           ,"WHERE false"
                          + ",FIRST BilagsArt OF Kundereskontr NO-LOCK"
                          + ",FIRST BilagsType OF Kundereskontr NO-LOCK"
                           ,"sort|FakturaNr").         
  DYNAMIC-FUNCTION("createObjectLink",hBrowse,THIS-PROCEDURE).
  hBrowse:NAME = "brwKundeResk".

  hBrowse:MOVE-COLUMN(19,3).
  hBrowse:MOVE-COLUMN(20,3).
  hBrowse:GET-BROWSE-COLUMN(3):WIDTH-PIXELS = 100.
  hBrowse:GET-BROWSE-COLUMN(4):WIDTH-PIXELS = 100.

  hReklamert = hBrowse:QUERY:GET-BUFFER-HANDLE:BUFFER-FIELD("Reklamert").
  DO ix = 1 TO hBrowse:NUM-COLUMNS:
    hBrowseColumns[ix] = hBrowse:GET-BROWSE-COLUMN(ix).
  END.

  DYNAMIC-FUNCTION("NewMenuBand",
                    hBrowse,  
                    "Deselect;Fjern markering av rad"
                  + ",Payment;Registrer ny innbetaling eller kryss valgt innbetaling mot valgt(e) faktura(er)"
                  + ",CredNote;Krediter faktura"
                  + ",PayOut;Utbetaling"
                  + ",Debet;Reverser kreditpostering"
                  + ",Complaint;Reklamasjon"
                  + ",Bong;Vis bong"
                    ,"").     
  hMenuPayment   = WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",hBrowse,"menu-itemPayment")).
  hMenuCredNote  = WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",hBrowse,"menu-itemCredNote")).
  hMenuPayOut    = WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",hBrowse,"menu-itemPayOut")).
  hMenuDebet     = WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",hBrowse,"menu-itemDebet")).
  hMenuComplaint = WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",hBrowse,"menu-itemComplaint")).
  hMenuBong      = WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",hBrowse,"menu-itemBong")).

  hFieldMap = DYNAMIC-FUNCTION("NewFieldMap",
                               hBrowse:QUERY,
                               FRAME {&FRAME-NAME}:HANDLE,
                               "","",
                               "Notat","",
                               "").
  DYNAMIC-FUNCTION("createObjectLink",hBrowse,hFieldMap).

  hToolbar = DYNAMIC-FUNCTION("NewToolbar",
                            rectToolbar:HANDLE,
                            "",
                            "Payment;Bet;Registrer ny innbetaling eller kryss valgt innbetaling mot valgt(e) faktura(er);PaymentRecord;bmp/consl16e.bmp"
                          + ",excel",
                            "maxborder").

  hBtnPayment = WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",hToolbar,"buttonPayment")).

/*   DYNAMIC-FUNCTION("setToolbar",hToolbar,"disable"). */
  DYNAMIC-FUNCTION("CreateObjectLink",hBrowse,hToolbar).

  hTabFolder = DYNAMIC-FUNCTION("NewTabFolder",rectFolder:HANDLE).
  
  DYNAMIC-FUNCTION("InitPages" IN hTabFolder,"Krysstabell|KunderesKobling.w",hBrowse).
  
  DYNAMIC-FUNCTION("buildFolder" IN hTabFolder).

/*
  IF cTabList = "" THEN
    cTabList = "Krysstabell|KunderesKobling.w".

  DO ix = 1 TO NUM-ENTRIES(cTabList,"|") BY 2:
    IF ix MOD 2 = 1 THEN iy = iy + 1.
    hPageObject = DYNAMIC-FUNCTION("addFolder" IN hTabFolder,iy,ENTRY(ix,cTabList,"|"),ENTRY(ix + 1,cTabList,"|"),"").
    DYNAMIC-FUNCTION("setQuery" IN hPageObject,hBrowse).
    RUN InitializeObject IN hPageObject.
    DYNAMIC-FUNCTION("setAttribute",hTabFolder,"pageframe" + STRING(iy),STRING(DYNAMIC-FUNCTION("getFrameHandle" IN hPageObject))).
  END.

  DYNAMIC-FUNCTION("buildFolder" IN hTabFolder).
  */

  hTabFrame = DYNAMIC-FUNCTION("getTabFrame" IN hTabFolder).
  
  DYNAMIC-FUNCTION("setTabMoveY" IN hTabFolder).
/*   DYNAMIC-FUNCTION("setAddMoveY",THIS-PROCEDURE:CURRENT-WINDOW,DYNAMIC-FUNCTION("getTabFrame" IN hTabFolder),"frmTabFrame"). */
/*   DYNAMIC-FUNCTION("setNoMoveY",THIS-PROCEDURE:CURRENT-WINDOW,DYNAMIC-FUNCTION("getTabFrame" IN hTabFolder),"frmProgram").   */
/*   DYNAMIC-FUNCTION("setAddMoveY",THIS-PROCEDURE:CURRENT-WINDOW,FRAME {&FRAME-NAME}:HANDLE,"rectFolder,Notat").               */
/*   DYNAMIC-FUNCTION("setNoResizeY",THIS-PROCEDURE:CURRENT-WINDOW,FRAME {&FRAME-NAME}:HANDLE,"rectToolbar,rectFolder,Notat").  */

  DYNAMIC-FUNCTION("setAnchor",THIS-PROCEDURE:CURRENT-WINDOW,hBrowse,
                    STRING(rsAlle:HANDLE)
                   ,"").
END.

DYNAMIC-FUNCTION("TabFolderChanged" IN hTabFolder,1).
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
DEF VAR hWidget AS HANDLE NO-UNDO.
hWidget = hBrowse:GET-BROWSE-COLUMN(1).
APPLY "end-resize" TO hWidget.
FRAME {&FRAME-NAME}:MOVE-TO-TOP().

DYNAMIC-FUNCTION("MoveTabToTop" IN hTabFolder,rectFolder:HANDLE IN FRAME {&FRAME-NAME}).

APPLY "entry" TO hBrowse.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PaymentRecord C-Win 
PROCEDURE PaymentRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR fInnbetBelop  AS DEC  NO-UNDO.
DEF VAR dInnbet       AS DATE NO-UNDO.
DEF VAR cIdListDebet  AS CHAR NO-UNDO.
DEF VAR cIdKredit     AS CHAR NO-UNDO.
DEF VAR fSumDebet     AS DEC  NO-UNDO.
DEF VAR fKredit       AS DEC  NO-UNDO.
DEF VAR fiNotat       AS CHAR NO-UNDO.
DEF VAR cNotat        AS CHAR NO-UNDO.
DEF VAR fiKID         AS DEC  NO-UNDO.

IF hBrowse:NUM-SELECTED-ROWS > 0 THEN DO ix = 1 TO hBrowse:NUM-SELECTED-ROWS:
  IF hBrowse:FETCH-SELECTED-ROW(ix) THEN DO:
    IF hFieldMap:BUFFER-FIELD("Saldo"):BUFFER-VALUE > 0 THEN 
      ASSIGN cIdListDebet  = cIdListDebet + STRING(hFieldMap:BUFFER-FIELD("Reskontro_id"):BUFFER-VALUE) + ","
             fSumDebet     = fSumDebet + hFieldMap:BUFFER-FIELD("Saldo"):BUFFER-VALUE.
    ELSE DO:
      IF fKredit NE 0 THEN DO:
        DYNAMIC-FUNCTION("DoMessage",0,0,"Kun én kreditpost kan velges ved kryssing","","").
        RETURN.
      END.
      ASSIGN cIdKredit = STRING(hFieldMap:BUFFER-FIELD("Reskontro_id"):BUFFER-VALUE)
             fKredit   = hFieldMap:BUFFER-FIELD("Saldo"):BUFFER-VALUE * -1
             dInnbet   = hFieldMap:BUFFER-FIELD("ForfallsDato"):BUFFER-VALUE
             .
    END.
  END.
END.

cIdListDebet  = TRIM(cIdListDebet,",").

IF cIdKredit NE "" AND cIdListDebet = "" THEN DO:
  DYNAMIC-FUNCTION("DoMessage",0,0,"En kreditpost må føres mot et krav","","").
  RETURN.
END.

RUN KunderesInnbetFakt.w (cIdListDebet,fSumDebet,fKredit,OUTPUT fInnbetBelop,INPUT-OUTPUT dInnbet,OUTPUT cNotat, OUTPUT fiKID).
  
IF fInnbetBelop NE 0 THEN DO:
  IF DYNAMIC-FUNCTION("runproc","kunderes_innbet.p",
                       STRING(DYNAMIC-FUNCTION("getKundeNr" IN hParent))
                     + "|" + cIdListDebet
                     + "|" + cIdKredit 
                     + "|" + STRING(dInnbet)
                     + "|" + STRING(fInnbetBelop)
                     + "|" + cNotat
                     + "|" + STRING(fiKID)
                     + "|0|0"
                      ,?) THEN 
    RUN RefreshCurrentRow IN hParent NO-ERROR.
  ELSE DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PayOutRecord C-Win 
PROCEDURE PayOutRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR fDebetBelop  AS DEC  NO-UNDO.
DEF VAR cNotat       AS CHAR NO-UNDO.

IF hBrowse:NUM-SELECTED-ROWS > 1 THEN DO: 
  DYNAMIC-FUNCTION("DoMessage",0,0,"Kun én kreditpost kan velges for utbetaling","","").
  RETURN.
END.
IF hFieldMap:BUFFER-FIELD("BilagsType"):BUFFER-VALUE NE 2 AND hFieldMap:BUFFER-FIELD("BilagsType"):BUFFER-VALUE NE 3 THEN DO:
  DYNAMIC-FUNCTION("DoMessage",0,0,"Ugyldig bilagstype for utbetaling","","").
  RETURN.
END.

RUN KunderesUtbetal.w (hFieldMap:BUFFER-FIELD("Saldo"):BUFFER-VALUE * -1,
                       OUTPUT fDebetBelop,OUTPUT cNotat).
  
IF fDebetBelop NE 0 THEN DO:
  IF DYNAMIC-FUNCTION("runproc","kunderes_utbet.p",
                       STRING(DYNAMIC-FUNCTION("getKundeNr" IN hParent))
                     + "|" + STRING(hFieldMap:BUFFER-FIELD("Reskontro_id"):BUFFER-VALUE)
                     + "|" + STRING(fDebetBelop)
                     + "|" + cNotat
                      ,?) THEN 
    RUN RefreshCurrentRow IN hParent NO-ERROR.
  ELSE DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").
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
IF hReklamert:BUFFER-VALUE NE ? THEN
  DO ix = 1 TO 20:
    hBrowseColumns[ix]:BGCOLOR = 14.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getBongId C-Win 
FUNCTION getBongId RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
IF hFieldMap:AVAIL THEN
  RETURN hFieldMap:BUFFER-FIELD("B_Id"):BUFFER-VALUE.
ELSE
  RETURN 0.00.   /* Function return value. */

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION InitializeResize C-Win 
FUNCTION InitializeResize RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  DYNAMIC-FUNCTION("setAddMoveY",THIS-PROCEDURE:CURRENT-WINDOW,FRAME {&FRAME-NAME}:HANDLE,"rectFolder,Notat").
  DYNAMIC-FUNCTION("setNoResizeY",THIS-PROCEDURE:CURRENT-WINDOW,FRAME {&FRAME-NAME}:HANDLE,"rectToolbar,rectFolder,Notat").
  RETURN FALSE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setFilter C-Win 
FUNCTION setFilter RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
  ASSIGN rsAlle.
  CASE rsAlle:
    WHEN 1 THEN DO:
      DYNAMIC-FUNCTION("setAttribute",hBrowse,"queryfilter",
                       "WHERE Saldo NE 0 " 
                     + (IF cbBilagstype:SCREEN-VALUE NE "" AND cbBilagstype:SCREEN-VALUE NE ? THEN
                         "AND BilagsType = " + cbBilagstype:SCREEN-VALUE
                        ELSE "")
                     + (IF tbBong:CHECKED THEN " AND B_Id NE 0" ELSE "")
                       ).
      DYNAMIC-FUNCTION("setAttribute",hToolbar,"disabledevents","").
    END.
    WHEN 2 THEN DO:
      DYNAMIC-FUNCTION("setAttribute",hBrowse,"queryfilter",
                       "WHERE Saldo = 0 " 
                     + (IF cbBilagstype:SCREEN-VALUE NE "" AND cbBilagstype:SCREEN-VALUE NE ? THEN
                         "AND BilagsType = " + cbBilagstype:SCREEN-VALUE
                        ELSE "")
                     + (IF tbBong:CHECKED THEN " AND B_Id NE 0" ELSE "")
                       ).
      DYNAMIC-FUNCTION("setAttribute",hToolbar,"disabledevents","payment").
    END.
    WHEN 3 THEN DO:
      DYNAMIC-FUNCTION("setAttribute",hBrowse,"queryfilter",
                       (IF cbBilagstype:SCREEN-VALUE NE "" AND cbBilagstype:SCREEN-VALUE NE ? THEN
                         "WHERE BilagsType = " + cbBilagstype:SCREEN-VALUE
                        ELSE "")
                     + (IF tbBong:CHECKED THEN " AND B_Id NE 0" ELSE "")
                       ).
      DYNAMIC-FUNCTION("setAttribute",hToolbar,"disabledevents","payment").
    END.
  END CASE.
END.
 
RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setMenuAccess C-Win 
FUNCTION setMenuAccess RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes: Endret 15.03.06 av bha
          - Tillater kreditering av purregebyr 
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
  ASSIGN rsAlle.
  IF rsAlle = 1 THEN
    ASSIGN hBtnPayment:SENSITIVE      = TRUE
           hMenuPayment:SENSITIVE     = TRUE
           .
  ELSE
    ASSIGN hBtnPayment:SENSITIVE      = FALSE
           hMenuPayment:SENSITIVE     = FALSE
           .

  IF hFieldMap:AVAIL AND hFieldMap:BUFFER-FIELD("BilagsType"):BUFFER-VALUE = 1 THEN
    ASSIGN hMenuCredNote:SENSITIVE  = TRUE
           hMenuComplaint:SENSITIVE = TRUE
           .
  ELSE
    ASSIGN hMenuCredNote:SENSITIVE  = FALSE
           hMenuComplaint:SENSITIVE = FALSE
           .

  IF hFieldMap:AVAIL AND hFieldMap:BUFFER-FIELD("BilagsType"):BUFFER-VALUE = 11 AND
     hFieldMap:BUFFER-FIELD("Saldo"):BUFFER-VALUE = hFieldMap:BUFFER-FIELD("Belop"):BUFFER-VALUE THEN
    hMenuCredNote:SENSITIVE  = TRUE.

  IF hFieldMap:AVAIL AND hFieldMap:BUFFER-FIELD("Saldo"):BUFFER-VALUE < 0 THEN
    hMenuPayOut:SENSITIVE = TRUE.
  ELSE
    hMenuPayOut:SENSITIVE = FALSE.

  IF hFieldMap:AVAIL AND 
     (hFieldMap:BUFFER-FIELD("BilagsType"):BUFFER-VALUE = 2 OR hFieldMap:BUFFER-FIELD("BilagsType"):BUFFER-VALUE = 3) THEN
    hMenuDebet:SENSITIVE = TRUE.
  ELSE
    hMenuDebet:SENSITIVE = FALSE.

  IF hFieldMap:AVAIL AND hFieldMap:BUFFER-FIELD("B_Id"):BUFFER-VALUE NE 0 THEN
    hMenuBong:SENSITIVE = TRUE.
  ELSE
    hMenuBong:SENSITIVE = FALSE.

END.

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
hParentBuffer = ihQuery:QUERY:GET-BUFFER-HANDLE(1).
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
IF iCurrTab > 0 THEN
  DYNAMIC-FUNCTION("DeleteObjectLink",hBrowse,DYNAMIC-FUNCTION("getPageQuery" IN hTabFolder,iCurrTab)).

DYNAMIC-FUNCTION("setAttribute",hToolbar,"disabledevents","").
/* DYNAMIC-FUNCTION("CreateParentLink",DYNAMIC-FUNCTION("getPageQuery" IN hTabFolder,iiTab),hBrowse,"DReskontro_id;Reskontro_id").  */

ASSIGN hCurrTabProc  = DYNAMIC-FUNCTION("getPageHandle" IN hTabFolder,iiTab)
       hCurrTabFrame = DYNAMIC-FUNCTION("getPageFrame" IN hTabFolder,iiTab)
       iCurrTab      = iiTab
       .

RUN MoveToTop.

DYNAMIC-FUNCTION("setCurrentObject",hBrowse).
RUN DisplayRecord.

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

