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

DEFINE VARIABLE bOk                AS LOGICAL   NO-UNDO.
DEFINE VARIABLE bVisButikker       AS LOGICAL   NO-UNDO.
DEFINE VARIABLE hbcChkAktiv        AS HANDLE    NO-UNDO.
DEFINE VARIABLE hbcChkAktivLev     AS HANDLE    NO-UNDO.
DEFINE VARIABLE hbfChkAktiv        AS HANDLE    NO-UNDO.
DEFINE VARIABLE hbfChkAktivLev     AS HANDLE    NO-UNDO.
DEFINE VARIABLE hBrwHode           AS HANDLE    NO-UNDO.
DEFINE VARIABLE hBrwModell         AS HANDLE    NO-UNDO.
DEFINE VARIABLE hBrwArtikkel       AS HANDLE    NO-UNDO.
DEFINE VARIABLE hBrwLinje          AS HANDLE    NO-UNDO.
DEFINE VARIABLE hChild             AS HANDLE    NO-UNDO.
DEFINE VARIABLE hParent            AS HANDLE    NO-UNDO.
DEFINE VARIABLE hParentBrowse      AS HANDLE    NO-UNDO.
DEFINE VARIABLE hSearchFieldHode   AS HANDLE    NO-UNDO.
DEFINE VARIABLE hSearchFieldModell AS HANDLE    NO-UNDO.
DEFINE VARIABLE hToolbar           AS HANDLE    NO-UNDO.
DEFINE VARIABLE ix                 AS INTEGER   NO-UNDO.

DEF VAR hBrowseOrdre           AS HANDLE NO-UNDO.
DEF VAR hBrowseOrdreBest       AS HANDLE NO-UNDO.
DEF VAR hBrowseBestHode        AS HANDLE NO-UNDO.
DEF VAR hBrowseAndreBest       AS HANDLE NO-UNDO.
DEF VAR hBrowseOrdreBestStr    AS HANDLE NO-UNDO.
DEF VAR hBrowseOrdreBestLev    AS HANDLE NO-UNDO.
DEF VAR hOrdreToolbar          AS HANDLE NO-UNDO.
DEF VAR hBestToolbar           AS HANDLE NO-UNDO.
DEF VAR hArtikkelkort          AS HANDLE NO-UNDO.

DEF VAR hBestBilde             AS HANDLE NO-UNDO.
DEF VAR hBestBildeFrame        AS HANDLE NO-UNDO.

DEF VAR iFontWingdings   AS INT    NO-UNDO.
iFontWingdings = DYNAMIC-FUNCTION("setAppFont","Wingdings, size=11 Script=symbol","") NO-ERROR.

{buildfunction.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rectBrowseOrdre BrwOrdreBestStr OrdreToolbar ~
BestBilde 

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setParent C-Win 
FUNCTION setParent RETURNS LOGICAL
  ( INPUT ihParent AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setQuery C-Win 
FUNCTION setQuery RETURNS LOGICAL
  ( INPUT ihQuery AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE RECTANGLE BestBilde
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 15 BY 2.86.

DEFINE RECTANGLE BrwOrdreBestStr
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 187 BY 5.95.

DEFINE RECTANGLE OrdreToolbar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 18 BY 1.1.

DEFINE RECTANGLE rectBrowseOrdre
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 187.4 BY 11.67.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     rectBrowseOrdre AT ROW 4.33 COL 1.6
     BrwOrdreBestStr AT ROW 16.24 COL 2
     OrdreToolbar AT ROW 3.14 COL 2.2
     BestBilde AT ROW 1.14 COL 174
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SCROLLABLE SIZE 189 BY 21.52.


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
         TITLE              = "VPI Leverandører"
         HEIGHT             = 21.48
         WIDTH              = 188.6
         MAX-HEIGHT         = 57.14
         MAX-WIDTH          = 320
         VIRTUAL-HEIGHT     = 57.14
         VIRTUAL-WIDTH      = 320
         MAX-BUTTON         = no
         RESIZE             = no
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
   FRAME-NAME                                                           */
ASSIGN 
       FRAME DEFAULT-FRAME:HEIGHT           = 21.52
       FRAME DEFAULT-FRAME:WIDTH            = 189.

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
ON END-ERROR OF C-Win /* VPI Leverandører */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* VPI Leverandører */
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

{incl/supptrigg.i hBrwHode}

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
IF NOT VALID-HANDLE(hArtikkelkort) THEN DO:
  IF DYNAMIC-FUNCTION("getCurrentObject") = hBrowseOrdreBestStr THEN
    RUN w-vartkor.w  PERSIST SET hArtikkelkort (DYNAMIC-FUNCTION("getRecId","ArtBas",hBrowseOrdreBestStr:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("RowIdent2"):BUFFER-VALUE), "ENDRE," + STRING(THIS-PROCEDURE)).
END.
ELSE DO:
  IF DYNAMIC-FUNCTION("getCurrentObject") = hBrowseOrdreBestStr THEN
    RUN ByttArtikkel IN hArtikkelkort (hBrowseOrdreBestStr:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE).
END.
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
IF DYNAMIC-FUNCTION("getCurrentObject") = hBrowseOrdreBestStr THEN
  RUN ArtikkelKort.

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
RUN SUPER.

IF DYNAMIC-FUNCTION("getCurrentObject") = hBrowseOrdreBestStr THEN 
  RUN VisMiniBilde IN hBestBilde 
      (IF hBrowseOrdreBestStr:QUERY:GET-BUFFER-HANDLE(1):AVAIL THEN 
         hBrowseOrdreBestStr:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("BildNr"):BUFFER-VALUE 
       ELSE 0).


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
  ENABLE rectBrowseOrdre BrwOrdreBestStr OrdreToolbar BestBilde 
      WITH FRAME DEFAULT-FRAME.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
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

  RUN VisMiniBilde.w PERSIST SET hBestBilde.
  RUN InitializeObject IN hBestBilde (BestBilde:HANDLE).
  hBestBildeFrame = DYNAMIC-FUNCTION("getFrameHandle" IN hBestBilde).

  hBrowseOrdre = DYNAMIC-FUNCTION("NewBrowse",
                    rectBrowseOrdre:HANDLE,
                    200,
                    "MULTIPLE",
                    "Ordre"
                    + ";OrdreNr"
                    + ";Hasteordre|Haster|Ja/Nei"
                    + ";SendtDato"
                    + ";CL"
                    + ";+OrdreTotAnt|INTEGER|>>>>9|ordre_tot_antall|Antall"
                    + ";+OrdreTotPris|INTEGER|->>>>>9.99|ordre_tot_pris|Sum innk.pris"
                    + ";+OrdreTotDBkr|INTEGER|->>>>>9.99|ordre_tot_dbkr|Sum DB"
                    + ";!+OrdreTotAntLev|INTEGER|>>>>9|ordre_tot_levert|Ant.lev"
                    + ";!+OrdreTotRestAnt|INTEGER|->>>9|ordre_tot_rest|Ant.rest"
                    + ";!LeveringsDato"
                    + ";!RegistrertDato"
                    + ";!BekreftetDato|Bekr.dato"
                    + ";LevNr|Levnr"
                    + ";OrdreStatus|St"
                    + ";OrdreMottaker"
                    + ";!VarebehNr"
/*                     + ";!+StatusOgButikkSjekk|CHARACTER|x|ordre_for_butikk(ROWID)"  */
                    + ";!PlListeId"
                  + ",LevBas"
                    + ";LevNamn|Levnavn@9"
                  + ",buf1_SysPara"
                    + ";Beskrivelse|Status@10"
                    ,"WHERE false"
                   + ",FIRST LevBas OF Ordre NO-LOCK"
                   + ",FIRST buf1_SysPara WHERE  buf1_SysPara.SysHId = 5 and buf1_SysPara.SysGr = 3 AND buf1_SysPara.ParaNr = Ordre.OrdreStatus NO-LOCK OUTER-JOIN"
                    ,""
                     ).
  DYNAMIC-FUNCTION("CreateObjectLink",hBrowseOrdre,THIS-PROCEDURE).

  DYNAMIC-FUNCTION("setAttribute",hBrowseOrdre,"getrecordcount","yes").  
  DYNAMIC-FUNCTION("setAttribute",hBrowseOrdre,"customDeleteValProc","=ordre_delete_all.p").
  DYNAMIC-FUNCTION("setAttribute",hBrowseOrdre,"copytoolbartobrowse","yes").
  DYNAMIC-FUNCTION("setAttribute",hBrowseOrdre,"windowsbrowse","yes").
  DYNAMIC-FUNCTION("setAttribute",hBrowseOrdre,"calcfieldproc","ordre_browsekalk.p").
  DYNAMIC-FUNCTION("setSortString",hBrowseOrdre,"LeveringsDato;desc").


  hBrowseOrdreBestStr = DYNAMIC-FUNCTION("NewBrowse",
                    BrwOrdreBestStr:HANDLE,
                    100,
                    "",
                    "BestHode"
                    + ";BestNr|Best.nr"
                    + ";ArtikkelNr"
                    + ";!Ordrenr"
                  + ",ArtBas"
                    + ";Beskr|Varetekst@2"
                    + ";!BildNr"
                  + ",BestStr"
                    + ";Butik|Butikk"
                    + ";Storl"
                    + ";Bestilt"
                    + ";BestStat"
                    + ";!BestNr"
                    + ";!+BestStrUnique|CHARACTER|x(20)|beststr_distinct.p(ROWID)"
                  + ",StrKonv"
                    + ";!StrKode"
                    ,"WHERE false"
                   + ",FIRST ArtBas NO-LOCK OF BestHode"
                   + ",EACH BestStr NO-LOCK OF BestHode"
                   + ",FIRST StrKonv NO-LOCK WHERE TRIM(StrKonv.Storl) = TRIM(Storl)"
                    ,"sort|Butik;BY;StrKode").

  DYNAMIC-FUNCTION("CreateParentLink",hBrowseOrdreBestStr,hBrowseOrdre,"OrdreNr").


  hOrdreToolbar = DYNAMIC-FUNCTION("NewToolbar",
                   OrdreToolbar:HANDLE,
                   "",
/*                    "HasteOrdre;Haster;Marker valgte ordre som hasteordre" */
/*                    + ",SendOrdre;Send ordre"                              */
/*                    + ",EndreOrdreLevDato;Endre lev.dato"                  */
/*                    + (IF NOT bHKinst OR cButikkListe = "*" THEN           */
/*                         ",BekrOrdre;Bekreft ordre"                        */
/*                       + ",InnlevOrdre;Innlever ordre"                     */
/*                       + ",InnlevRapport;Innleveranserapport"              */
/*                       + ",OverforVaremottak;Overfør til varemottak"       */
/*                       + ",MakulerOrdre;Avskriv rest"                      */
/*                       + ",copy;Kopier ordre"                              */
/*                       ELSE "")                                            */
/*                    + ",ReSendOrdre;Resend ordre til butikk"               */
/*                    + ",delete;Slett ordre&"                               */
                    "excel"
/*                    + ",Print;Ordreutskrift"         */
/*                    + ",BrowseConfig;Kolonneoppsett" */
                   ,"").
  DYNAMIC-FUNCTION("CreateObjectLink",hBrowseOrdre,hOrdreToolbar).


  DYNAMIC-FUNCTION("setNoResizeX", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE,
                   "BestBilde").
  DYNAMIC-FUNCTION("setNoResizeY", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE,
                   "BrwOrdreBestStr,BestBilde,"
                 + hBrowseOrdreBestStr:NAME).
  DYNAMIC-FUNCTION("setAddMoveY", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE,
                   "BrwOrdreBestStr,"
                 + hBrowseOrdreBestStr:NAME).

  DYNAMIC-FUNCTION("setAddMoveX", THIS-PROCEDURE:CURRENT-WINDOW, hBestBildeFrame,hBestBildeFrame:NAME).
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
APPLY "entry" TO hBrowseOrdre.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getParentBrowse C-Win 
FUNCTION getParentBrowse RETURNS HANDLE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN hBrwHode.

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
DYNAMIC-FUNCTION("setNoResizeX", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE,
                 "BestBilde").
DYNAMIC-FUNCTION("setNoResizeY", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE,
                 "BrwOrdreBestStr,BestBilde").
DYNAMIC-FUNCTION("setAddMoveX", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE,
                 "BestBilde").
DYNAMIC-FUNCTION("setAddMoveY", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE,
                 "BrwOrdreBestStr").
  
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

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setQuery C-Win 
FUNCTION setQuery RETURNS LOGICAL
  ( INPUT ihQuery AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
hParentBrowse = ihQuery.

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

