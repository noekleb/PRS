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
DEFINE VARIABLE hBrwModell         AS HANDLE    NO-UNDO.
DEFINE VARIABLE hBrwArtikkel       AS HANDLE    NO-UNDO.
DEFINE VARIABLE hBrwLinje          AS HANDLE    NO-UNDO.
DEFINE VARIABLE hOverlayAntPl      AS HANDLE    NO-UNDO.
DEFINE VARIABLE hChild             AS HANDLE    NO-UNDO.
DEFINE VARIABLE hParent            AS HANDLE    NO-UNDO.
DEFINE VARIABLE hParentBrowse      AS HANDLE    NO-UNDO.
DEFINE VARIABLE hSearchFieldHode   AS HANDLE    NO-UNDO.
DEFINE VARIABLE hSearchFieldModell AS HANDLE    NO-UNDO.
DEFINE VARIABLE hToolbar           AS HANDLE    NO-UNDO.
DEFINE VARIABLE ix                 AS INTEGER   NO-UNDO.
DEFINE VARIABLE cProfnr            AS CHAR      NO-UNDO.

DEF VAR hBestBilde             AS HANDLE NO-UNDO.
DEF VAR hBestBildeFrame        AS HANDLE NO-UNDO.
DEF VAR hArtikkelkort          AS HANDLE NO-UNDO.
DEF VAR hFlatView              AS HANDLE NO-UNDO.
DEF VAR hPanelVert             AS HANDLE NO-UNDO.
DEF VAR hPanelFrame            AS HANDLE NO-UNDO.

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
&Scoped-Define ENABLED-OBJECTS BrwPlListeModell BrwPlListeArtikkel ~
BrwPlListeLinje searchFieldModell BestBilde BtnPanel SumKostForslag ~
SumKostBekreftet 
&Scoped-Define DISPLAYED-OBJECTS SumKostForslag SumKostBekreftet 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD AdjustBrowseColumns C-Win 
FUNCTION AdjustBrowseColumns RETURNS LOGICAL
  ( INPUT ihBrowse     AS HANDLE,
    INPUT icBrowseName AS CHAR) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getFrameHandle C-Win 
FUNCTION getFrameHandle RETURNS HANDLE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getLocalQueryStat C-Win 
FUNCTION getLocalQueryStat RETURNS CHARACTER
  ( INPUT ihBrowseOrQuery AS HANDLE,
    INPUT icStatFields    AS CHAR )  FORWARD.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ViewQueryStat C-Win 
FUNCTION ViewQueryStat RETURNS LOGICAL
  ( INPUT ihQueryObject AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE SumKostBekreftet AS DECIMAL FORMAT "->>>>,>>9.99":U INITIAL 0 
     LABEL "Bekreftet" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE SumKostForslag AS DECIMAL FORMAT "->>>>,>>9.99":U INITIAL 0 
     LABEL "Sum forslag" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE RECTANGLE BestBilde
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 20 BY 3.91.

DEFINE RECTANGLE BrwPlListeArtikkel
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 64 BY 14.05.

DEFINE RECTANGLE BrwPlListeLinje
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 66 BY 14.05.

DEFINE RECTANGLE BrwPlListeModell
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 56 BY 14.05.

DEFINE RECTANGLE BtnPanel
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 88 BY 2.38.

DEFINE RECTANGLE searchFieldModell
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 19 BY .95.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     SumKostForslag AT ROW 19.62 COL 149.8 COLON-ALIGNED
     SumKostBekreftet AT ROW 19.62 COL 173.8 COLON-ALIGNED
     BrwPlListeModell AT ROW 5.29 COL 2
     BrwPlListeArtikkel AT ROW 5.29 COL 59
     BrwPlListeLinje AT ROW 5.29 COL 124
     searchFieldModell AT ROW 4.19 COL 2.6
     BestBilde AT ROW 1.14 COL 169
     BtnPanel AT ROW 1.95 COL 76
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SCROLLABLE SIZE 189.6 BY 19.76.


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
         HEIGHT             = 19.76
         WIDTH              = 189.8
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
       FRAME DEFAULT-FRAME:HEIGHT           = 19.76
       FRAME DEFAULT-FRAME:WIDTH            = 189.6.

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

{incl/supptrigg.i hBrwModell}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Artikkelkort C-Win 
PROCEDURE Artikkelkort :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF NOT VALID-HANDLE(hArtikkelkort) THEN DO:
  IF DYNAMIC-FUNCTION("getCurrentObject") = hBrwArtikkel THEN
    RUN w-vartkor.w  PERSIST SET hArtikkelkort (DYNAMIC-FUNCTION("getRecId","ArtBas",hBrwArtikkel:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("RowIdent2"):BUFFER-VALUE), "ENDRE," + STRING(THIS-PROCEDURE)).
END.
ELSE DO:
  IF DYNAMIC-FUNCTION("getCurrentObject") = hBrwArtikkel THEN
    RUN ByttArtikkel IN hArtikkelkort (hBrwArtikkel:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE).
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BekreftRecord C-Win 
PROCEDURE BekreftRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
MESSAGE PROGRAM-NAME(1)
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
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
IF DYNAMIC-FUNCTION("getCurrentObject") = hBrwArtikkel THEN
  RUN ArtikkelKort.
ELSE RUN SUPER.
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

IF DYNAMIC-FUNCTION("getCurrentObject") = hBrwArtikkel THEN 
  RUN VisMiniBilde IN hBestBilde 
      (IF hBrwArtikkel:QUERY:GET-BUFFER-HANDLE(1):AVAIL THEN 
         hBrwArtikkel:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("BildNr"):BUFFER-VALUE 
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
  DISPLAY SumKostForslag SumKostBekreftet 
      WITH FRAME DEFAULT-FRAME.
  ENABLE BrwPlListeModell BrwPlListeArtikkel BrwPlListeLinje searchFieldModell 
         BestBilde BtnPanel SumKostForslag SumKostBekreftet 
      WITH FRAME DEFAULT-FRAME.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ExtraFlatViewRecord C-Win 
PROCEDURE ExtraFlatViewRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihFlatView   AS HANDLE NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO INIT YES.


DEF VAR hFlatBrw    AS HANDLE NO-UNDO.

hFlatBrw  = DYNAMIC-FUNCTION("getBrowseHandle" IN ihFlatView).
DYNAMIC-FUNCTION("setAttribute",hFlatBrw,"availDistinctColumns",
                 "Artikkelnr,Beskr,Vg,VgBeskr,LevNamn,FarBeskr").

DYNAMIC-FUNCTION("setAttribute",hFlatBrw,"availAccumFields",
                 "KostForslag,KostBekreftet").

hFlatView = ihFlatView.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FlatViewRecord C-Win 
PROCEDURE FlatViewRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
RUN SUPER.

RUN InitQuery IN hFlatView (
                    ""   /* Filter fields (comma sep) */
                   ,""   /* Filter operators (comma sep) */
                   ,""   /* Filter values (pipe sep) */
                   ,""   /* Initial sort column(s) (comma sep) */
                   ,"Vg,VgBeskr"   /* Distinct columns (comma sep) */
                   ,"KostForslag,KostBekreftet"   /* Columns to accumulate (comma sep) */
                   ).
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
  
  cProfnr = DYNAMIC-FUNCTION("getFieldValues","FIRST Butiker",
                             "WHERE Sentrallager","ProfilNr").
  IF cProfnr = ? THEN
    cProfnr = DYNAMIC-FUNCTION("getFieldValues","FIRST Prisprofil",
                               "WHERE true","ProfilNr").

  hBrwModell = DYNAMIC-FUNCTION("NewBrowse"
                          ,BrwPlListeModell:HANDLE
                          ,100
                          ,""
                          ,"PlListeModell"
                           + ";ModellFarge|Art/mod.nr"
                           + ";!PlListeId"
                         + ",ArtBas"
                           + ";LevNr|Lev.nr"
                           + ";Beskr@2"
                           + ";LevKod@3"
                         + ",LevBas"
                           + ";LevNamn"
                         + ",VarGr"
                           + ";Vg"
                           + ";VgBeskr"
                          ,"WHERE false"
                         + ",FIRST ArtBas NO-LOCK WHERE ArtBas.ArtikkelNr = PlListeModell.ModellFarge"
                         + ",FIRST LevBas NO-LOCK OF ArtBas"
                         + ",FIRST VarGr NO-LOCK OF ArtBas"
                          ,"").
  DYNAMIC-FUNCTION("setSortString",hBrwModell,"ModellFarge").
  DYNAMIC-FUNCTION("CreateObjectLink",hBrwModell,THIS-PROCEDURE).
  
  hSearchFieldModell = DYNAMIC-FUNCTION("NewBrowseSearchField",SearchFieldModell:HANDLE,hBrwModell,1).
  DYNAMIC-FUNCTION("CreateObjectLink",hSearchFieldModell,hBrwModell).

  hBrwArtikkel = DYNAMIC-FUNCTION("NewBrowse"
                          ,BrwPlListeArtikkel:HANDLE
                          ,100
                          ,""
                          ,"PlListeArtikkel"
                           + ";ArtikkelNr|Art.nr"
                           + ";!ModellFarge"
                           + ";!PlListeId"
                         + ",buf1_ArtBas"
                           + ";LevFargKod|Lev.farge"
                           + ";!Farg"
                           + ";!Bildnr"
                         + ",Farg"
                           + ";FarBeskr|Farge@2"
                         + ",ArtPris"
                           + ";Varekost[1]|Varekost"
                          ,"WHERE false"
                         + ",FIRST buf1_ArtBas NO-LOCK OF PlListeArtikkel"
                         + ",FIRST Farg NO-LOCK OF buf1_ArtBas OUTER-JOIN"
                         + ",FIRST ArtPris NO-LOCK OF buf1_ArtBas WHERE Profilnr = " + cProfnr
                          ,"").
  DYNAMIC-FUNCTION("setSortString",hBrwArtikkel,"ArtikkelNr").
  DYNAMIC-FUNCTION("CreateParentLink",hBrwArtikkel,hBrwModell,"PlListeId,ModellFarge").
  hBrwArtikkel:TOOLTIP = "Dobbeltklikk for å åpne artikkelkort".

  hBrwLinje = DYNAMIC-FUNCTION("NewBrowse"
                          ,BrwPlListeLinje:HANDLE
                          ,100
                          ,""
                          ,"PlListeLinje"
                           + ";+AlfaStr|CHARACTER|x(8)|plliste_alfastr|Str"
                           + ";Antall|Forslag|>>>9"
                           + ";+KostForslag|DECIMAL|>>>><>>9.99|plliste_forslkost|Kost.forsl"
                           + ";AntallPlukket|Bekreftet|>>>9"
                           + ";+KostBekreftet|DECIMAL|>>>><>>9.99|plliste_bekrkost|Kost.bekr"
                           + ";!StrKode"
                           + ";!ArtikkelNr|Art.nr"
                           + ";!LevFargKod"
                           + ";!PlListeId"
                           + ";!+StrSeq|INTEGER|>>9|plliste_strseq"
                          ,"WHERE false"
                          ,"").
  hBrwLinje:TOOLTIP = "Dobbeltklikk for å endre bekreftet antall".

  DYNAMIC-FUNCTION("setSortString",hBrwLinje,"StrSeq").
  DYNAMIC-FUNCTION("CreateParentLink",hBrwLinje,hBrwArtikkel,"PlListeId,ArtikkelNr").
  DYNAMIC-FUNCTION("setAttribute",hBrwLinje,"calcFieldProc","plliste_brwcalc.p").
  DYNAMIC-FUNCTION("setAttribute",hBrwLinje,"noColumnSort","AlfaStr,Antall,AntallPlukket").
  DYNAMIC-FUNCTION("setAttribute",hBrwLinje,"queryStatFields","KostForslag,KostBekreftet").
  DYNAMIC-FUNCTION("setAttribute",hBrwLinje,"enableOnDblClick","yes").

  hOverlayAntPl  = DYNAMIC-FUNCTION("NewBrowseFillIn",hBrwLinje,"AntallPlukket","AntallPlukket"
            ,"","","","").
  DYNAMIC-FUNCTION("CreateOverlayLink",hBrwLinje,hOverlayAntPl,"AntallPlukket").
  hOverlayAntPl:TOOLTIP = "Bekreftet antall".
  DYNAMIC-FUNCTION("setAttribute",hOverlayAntPl,"refreshRow","yes").

/*   hToolbar = DYNAMIC-FUNCTION("NewToolBar",                           */
/*                     rectToolBar:HANDLE,                               */
/*                     "Fil",                                            */
/*                     "FlatView;Totalrapport for bestillingsforslaget"  */
/*                     ,"").                                             */
/*                                                                       */
/*   DYNAMIC-FUNCTION("CreateObjectLink",hBrwModell,hToolbar).           */


  hPanelVert = DYNAMIC-FUNCTION("NewPanel"
            ,BtnPanel:HANDLE
            ,"" /* New sub-menu (blank for none) */ 
            ,"Bekreft;&Bekreft forslag;Send bestilling på bekreftet antall for alle forslag;BekreftRecord;ico\important_note.ico"
           + ",Rapport;Sum pr varegr;Sum pr varegr;FlatViewRecord;ico\MSACCESSforms.ico"
           + ",NullstillArt;Nullstill artikkel;Nullstill artikkel;NullstillArt;ico\erase.ico"
            ,130,36     
            ,"").
  DYNAMIC-FUNCTION("CreateObjectLink",hPanelVert,hBrwModell).
  hPanelFrame = DYNAMIC-FUNCTION("getPanelFrame" IN hPanelVert).
  

  DYNAMIC-FUNCTION("setResizeXGroup", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE,33,
                   "BrwPlListeModell,BrwPlListeArtikkel,BrwPlListeLinje,"
                 + hBrwModell:NAME + "," + hBrwArtikkel:NAME + "," + hBrwLinje:NAME 
                   ).

  DYNAMIC-FUNCTION("setMoveXGroup", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE,33,
                   "BrwPlListeArtikkel," + hBrwArtikkel:NAME).
  DYNAMIC-FUNCTION("setMoveXGroup", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE,67,
                   "BrwPlListeLinje," + hBrwLinje:NAME).

  DYNAMIC-FUNCTION("setAddMoveX", THIS-PROCEDURE:CURRENT-WINDOW, hBestBildeFrame,hBestBildeFrame:NAME).
  DYNAMIC-FUNCTION("setAddMoveX", THIS-PROCEDURE:CURRENT-WINDOW, hPanelFrame,hPanelFrame:NAME).
END.
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
RUN SUPER.
getLocalQueryStat(hBrwLinje,"").
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
hPanelFrame:MOVE-TO-TOP().
APPLY "entry" TO hBrwModell.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NullstillArt C-Win 
PROCEDURE NullstillArt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF DYNAMIC-FUNCTION("ProcessQuery",hBrwLinje,"pllinje_nullstillant.p","") THEN
  RUN InvokeMethod(hBrwLinje,"OpenQuery").
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
/*   IF VALID-HANDLE(hbcChkAktiv) THEN                            */
/*       ASSIGN                                                   */
/*         hbcChkAktiv:FONT      = iFontWingdings                 */
/*         hbcChkAktiv:FORMAT    = CHR(254) + "/"  + CHR(168)     */
/*         .                                                      */
/*   IF VALID-HANDLE(hbcChkAktivLev) THEN                         */
/*       ASSIGN                                                   */
/*         hbcChkAktivLev:FONT      = iFontWingdings              */
/*         hbcChkAktivLev:FORMAT    = CHR(254) + "/"  + CHR(168)  */
/*         .                                                      */
RUN SUPER.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION AdjustBrowseColumns C-Win 
FUNCTION AdjustBrowseColumns RETURNS LOGICAL
  ( INPUT ihBrowse     AS HANDLE,
    INPUT icBrowseName AS CHAR):
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
IF icBrowseName = "BrwPlListeArtikkel" THEN
  ASSIGN
    ihBrowse:GET-BROWSE-COLUMN(2):WIDTH-PIXELS = 60
/*     ihBrowse:GET-BROWSE-COLUMN(5):WIDTH-PIXELS = 20          */
/*     ihBrowse:GET-BROWSE-COLUMN(6):WIDTH-PIXELS = 20          */
/*     ihBrowse:GET-BROWSE-COLUMN(9):WIDTH-PIXELS = 200 /* 8 */ */
/*     hbcChkAktivLev = ihBrowse:GET-BROWSE-COLUMN(5)                                   /* Ny */ */
/*     hbfChkAktivLev = ihBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('ChkAktivLev') /* Ny */ */
/*     hbcChkAktiv    = ihBrowse:GET-BROWSE-COLUMN(6)  /*5*/                                     */
/*     hbfChkAktiv    = ihBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('ChkAktiv')             */
  .
  
RETURN TRUE.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getLocalQueryStat C-Win 
FUNCTION getLocalQueryStat RETURNS CHARACTER
  ( INPUT ihBrowseOrQuery AS HANDLE,
    INPUT icStatFields    AS CHAR ) : 
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

IF icStatFields = "" THEN
  cStatFields = DYNAMIC-FUNCTION("getAttribute",ihBrowseOrQuery,"querystatfields").
ELSE cStatFields = icStatFields.

IF ihBrowseOrQuery:TYPE = "browse" THEN
  hBuffer = ihBrowseOrQuery:QUERY:GET-BUFFER-HANDLE(1).
ELSE
  hBuffer = ihBrowseOrQuery:GET-BUFFER-HANDLE(1).

CREATE BUFFER hQueryBuffer FOR TABLE hBuffer.
CREATE QUERY hQuery.
hQuery:SET-BUFFERS(hQueryBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + hQueryBuffer:NAME).
hQuery:QUERY-OPEN().
hQuery:GET-FIRST().
REPEAT WHILE NOT hQuery:QUERY-OFF-END:
  iCount = iCount + 1.
  DO ix = 1 TO NUM-ENTRIES(cStatFields):
    fStatValues[ix] = fStatValues[ix] + hQueryBuffer:BUFFER-FIELD(ENTRY(ix,cStatFields)):BUFFER-VALUE.
  END.
  hQuery:GET-NEXT().
END.

DELETE OBJECT hQuery.
DELETE OBJECT hQueryBuffer.

DO ix = 1 TO NUM-ENTRIES(cStatFields):
  cReturnString = cReturnString + ENTRY(ix,cStatFields) + "|" + STRING(fStatValues[ix]) + ";".
  DYNAMIC-FUNCTION("setAttribute",ihBrowseOrQuery,"statvalue" + ENTRY(ix,cStatFields),STRING(fStatValues[ix])).
END.

IF icStatFields = "" THEN DO:
  DYNAMIC-FUNCTION("setAttribute",ihBrowseOrQuery,"querystatfieldvalues",TRIM(cReturnString,";")).
  DYNAMIC-FUNCTION("setAttribute",ihBrowseOrQuery,"recordcount",STRING(iCount)).
  ViewQueryStat(ihBrowseOrQuery).
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

  RETURN hBrwModell.

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
                 "BestBilde,BtnPanel").
DYNAMIC-FUNCTION("setNoResizeY", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE,
                 "BestBilde,BtnPanel").

DYNAMIC-FUNCTION("setResizeXGroup", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE,33,
                 "BrwPlListeModell,BrwPlListeArtikkel,BrwPlListeLinje"
                 ).

DYNAMIC-FUNCTION("setMoveXGroup", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE,33,
                 "BrwPlListeArtikkel").
DYNAMIC-FUNCTION("setMoveXGroup", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE,67,
                 "BrwPlListeLinje").

DYNAMIC-FUNCTION("setAddMoveX", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE,
                 "SumKostForslag,SumKostBekreftet,BestBilde,BtnPanel"). 

DYNAMIC-FUNCTION("setAddMoveY", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE,
                 "SumKostForslag,SumKostBekreftet"). 

RETURN YES.   /* Function return value. */

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ViewQueryStat C-Win 
FUNCTION ViewQueryStat RETURNS LOGICAL
  ( INPUT ihQueryObject AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
IF ihQueryObject = hBrwLinje THEN DO WITH FRAME {&FRAME-NAME}:
  ASSIGN SumKostForslag:SCREEN-VALUE = 
             DYNAMIC-FUNCTION("getAttribute",
             ihQueryObject,"statValueKostForslag")
         SumKostBekreftet:SCREEN-VALUE = 
             DYNAMIC-FUNCTION("getAttribute",
             ihQueryObject,"statValueKostBekreftet")
         .
END.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

