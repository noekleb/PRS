&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: 

  Description: 

update_varebok_from_artbas.p
vareboklinjeDvelgfelter.w           
vareboklinje_refresh_all.p           
           
           
           
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
                          
DEF VAR hToolbar          AS HANDLE NO-UNDO.
DEF VAR hBrowse           AS HANDLE NO-UNDO.
DEF VAR hBrowse-2         AS HANDLE NO-UNDO.
 
DEF VAR hParent           AS HANDLE NO-UNDO.
DEF VAR hParentBrowse      AS HANDLE NO-UNDO.

DEF VAR hChild            AS HANDLE NO-UNDO.

DEF VAR VarebokNr         AS dec NO-UNDO.

DEF VAR hbcChkArtReg         AS HANDLE NO-UNDO.
DEF VAR hbfChkArtReg         AS HANDLE NO-UNDO.
DEF VAR hbcChkVarebok        AS HANDLE NO-UNDO.
DEF VAR hbfChkVarebok        AS HANDLE NO-UNDO.
DEF VAR hbcChkPris           AS HANDLE NO-UNDO.
DEF VAR hbfChkPris           AS HANDLE NO-UNDO.
DEF VAR hbcChkArtInfo        AS HANDLE NO-UNDO.
DEF VAR hbfChkArtInfo        AS HANDLE NO-UNDO.
DEF VAR hbcChkStrek          AS HANDLE NO-UNDO.
DEF VAR hbfChkStrek          AS HANDLE NO-UNDO.
DEF VAR hArtikkelKort        AS HANDLE NO-UNDO.
DEF VAR hSearchField         AS HANDLE NO-UNDO.
DEF VAR hbcChkAnonseArtikkel AS HANDLE NO-UNDO.
DEF VAR hbfChkAnonseArtikkel AS HANDLE NO-UNDO.
DEF VAR hbcChkKontroll       AS HANDLE NO-UNDO.
DEF VAR hbfChkKontroll       AS HANDLE NO-UNDO.

DEF VAR cRowidList           AS CHAR   NO-UNDO.

/*Valg av poster*/
DEF VAR cDato   AS CHAR NO-UNDO.
DEF VAR iReturn AS INT  NO-UNDO.

/* DEF VAR iFontWingdings    AS INT    NO-UNDO.                                                    */
/* iFontWingdings = DYNAMIC-FUNCTION("setAppFont","Wingdings, size=11 Script=symbol","") NO-ERROR. */

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
&Scoped-Define ENABLED-OBJECTS rectBrowse rectToolBar searchField ~
rectBrowse-2 

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
DEFINE RECTANGLE rectBrowse
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 186 BY 6.43.

DEFINE RECTANGLE rectBrowse-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 186 BY 9.29.

DEFINE RECTANGLE rectToolBar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 28 BY .95.

DEFINE RECTANGLE searchField
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 17.4 BY .95.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     rectBrowse AT ROW 3.38 COL 1
     rectToolBar AT ROW 1.24 COL 1.6
     searchField AT ROW 2.29 COL 1.6
     rectBrowse-2 AT ROW 9.81 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 186.2 BY 18.19.


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
         TITLE              = "Lokasjonslinjer"
         HEIGHT             = 18.19
         WIDTH              = 186.2
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
   FRAME-NAME Custom                                                    */
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
ON END-ERROR OF C-Win /* Lokasjonslinjer */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Lokasjonslinjer */
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
  IF VALID-HANDLE(hChild) THEN APPLY "close" TO hChild.
  IF VALID-HANDLE(hArtikkelkort) THEN APPLY "close" TO hArtikkelkort.
END.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  
  DEF NEW SHARED VAR wCurrLng   AS CHAR   INITIAL "DES" NO-UNDO.
  DEF NEW SHARED VAR wLngHandle AS HANDLE NO-UNDO.

  RUN enable_UI.

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

{incl/supptrigg.i hBrowse}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AnnulerKoblingRecord C-Win 
PROCEDURE AnnulerKoblingRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF VAR cLokasjonRowIdList AS CHAR  NO-UNDO.
DEF VAR cLokasjonidList    AS CHAR  NO-UNDO.
DEF VAR iTellenr           AS INT   NO-UNDO.
DEF VAR rRowidParent       AS ROWID NO-UNDO.
DEF VAR rRowid             AS ROWID NO-UNDO.

  IF hParentBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('oppdatert'):BUFFER-VALUE NE ? THEN
  DO:
    MESSAGE 'Kan ikke annuleres, tellelisten er oppdatert.'
       VIEW-AS ALERT-BOX INFO BUTTONS OK.
    LEAVE.
  END.
  MESSAGE 
    'Ønsker du å annulere tellingen?'
     VIEW-AS ALERT-BOX INFO BUTTONS YES-NO UPDATE bOk.
  IF NOT bOk THEN LEAVE.

  ASSIGN
      cLokasjonIdList = STRING(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('TelleNr'):BUFFER-VALUE)
      bOk = FALSE
      .

  DO WITH FRAME {&FRAME-NAME}:
    RUN annulerlokasjon.p (INPUT hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('KobletTilTelleNr'):BUFFER-VALUE,
                           INPUT int(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('ButikkListe'):BUFFER-VALUE),
                           INPUT cLokasjonIdList).
    
    DYNAMIC-FUNCTION("runproc","tellehode_oppdatersum.p",string(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('rowident1'):BUFFER-VALUE),?).
    
    ASSIGN 
      iTellenr = hParentBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('tellenr'):BUFFER-VALUE
      rRowidParent = hParentBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("RowIdent1"):BUFFER-VALUE
/*       rRowid       = hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("RowIdent1"):BUFFER-VALUE */
    .
    RUN InvokeMethod(hParentBrowse,"OpenQuery").
    hParentBrowse:QUERY:GET-BUFFER-HANDLE(1):FIND-FIRST('WHERE tellenr = ' + STRING(iTellenr) ,NO-LOCK).
    hParentBrowse:QUERY:REPOSITION-TO-ROWID(hParentBrowse:QUERY:GET-BUFFER-HANDLE(1):ROWID).
/*     RUN InvokeMethod(hBrowse,"OpenQuery"). */
    DYNAMIC-FUNCTION("TabChanged" IN hParent,3).

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
/*   IF hBrowse:QUERY:GET-BUFFER-HANDLE(1):AVAIL THEN */
/*     RUN openVisAvvik.                              */

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
  ENABLE rectBrowse rectToolBar searchField rectBrowse-2 
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
                                   + ";VgLopNr"
                                 + ";ArtikkelNr"
                                 + ";LevKod"
                                 + ";Beskr"
                                 + ";LevFargKod"
                                 + ";Storl"
                                 + ";!AntallPar|Ant.par|-z.zzz.zz9" 
                                 + ";OpprAntalTalt|Oppr.talt|-z.zzz.zz9"
                                 + ";AntallTalt|Ant.talt|-z.zzz.zz9"
                                 + ";!AntallDiff|Ant.diff|-z.zzz.zz9" 
                                 + ";!OpprVerdi"
                                 + ";OpptVerdi"
                                 + ";!VerdiDiff"
                                 + ";VVareKost"
                                 + ";Oppdatert"                               
                                 + ";Merknad|Merknad|x(30)"
                                 + ";Vg"
                                 + ";Sasong"
                                 + ";LevNr"
                                 + ";RegistrertDato"
                                 + ";!RegistrertTid"
                                 + ";RegistrertAv"
                                 + ";EDato"
                                 + ";!ETid"
                                 + ";BrukerID"
                                 + ";Butik"
                                 + ";!TelleNr"
                                 + ";!SeqNr"
                                 + ";!RabKr"
                                 + ";!Nedskrevet"
                                 + ";!MatKod"
                                 + ";!LopNr"
                                 + ";!Kode"
                                 + ";!Farg"

------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
  DYNAMIC-FUNCTION("NewObject",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE,"procedure").
  hBrowse = DYNAMIC-FUNCTION("NewBrowse",
                              rectBrowse:HANDLE,
                              100,
                              "",
                              "tellehode"
                                 + ";TelleNr"
                                 + ";!TTId"
                                 + ";!ButikkListe|Butikk|x(10)"
                                 + ";!TelleType"
                                 + ";!OrdreNr"
                                 + ";!PkSdlNr"
                                 + ";Beskrivelse"
                                 + ";StartDato"
                                 + ";Oppdatert"
                                 + ";!BatchNr"
                                 + ";AntLinjer"
                                 + ";!AntallPar|Ant.lager|->>>>>>9" 
                                 + ";AntallTalt|Ant.talt|->>>>>>9"
                                 + ";!OpprVerdi"
                                 + ";OpptVerdi"
                                 + ";!AntallDiff|Ant.diff" 
                                 + ";!AntallPosDiff"
                                 + ";!AntallNegDiff"
                                 + ";!VerdiDiff"
                                 + ";!VerdiPosDiff"
                                 + ";!VerdiNegDiff"
                                 + ";!TilButikk"
                                 + ";RegistrertDato"
                                 + ";!RegistrertTid"
                                 + ";RegistrertAv"
                                 + ";EDato"
                                 + ";!Etid"
                                 + ";+EndrTid|CHARACTER|x(5)|jb_hhmm(ETid)|Tid"
                                 + ";BrukerID"
                                 + ";LokasjonsId"
                                 + ";KobletTilTelleNr"
                                 + ";Filid"
                                 + ";FilDatoPDA"
                                 + ";!FilTidPDA"
                                 + ";+EndrTid2|CHARACTER|x(5)|jb_hhmm(FilTidPDA)|Kl"
                                 + ";BrukerIdPDA"
                                 + ";!TBId"
                                 + ";!Notat"
                             ,"WHERE false"
                             ,"").
  hBrowse-2 = DYNAMIC-FUNCTION("NewBrowse",
                              rectBrowse-2:HANDLE,
                              100,
                              "MULTIPLE",
                              "tellelinje"
                               + ";linjeNr|Linjenr|->>>>>>>9"
                               + ";VgLopNr"
                               + ";ArtikkelNr"
                               + ";LevKod"
                               + ";Beskr|Varetekst"
                               + ";LevFargKod"
                               + ";Storl"
                               + ";AntallPar|Ant.lager|->>>>>>9" 
                               + ";OpprAntalTalt|Oppr.talt|->>>>>>9"
                               + ";AntallTalt|Ant.talt|->>>>>>9"
                               + ";AntallDiff|Ant.dif|->>>>>>9" 
                               + ";OpprVerdi"
                               + ";OpptVerdi|Oppt.verdi|->>>>>>9.99"
                               + ";VerdiDiff"
                               + ";VVareKost"
                               + ";Oppdatert"                               
                               + ";Merknad|Merknad|x(30)"
                               + ";Vg"
                               + ";Sasong"
                               + ";LevNr"
                               + ";RegistrertDato"
                               + ";!RegistrertTid"
                               + ";RegistrertAv"
                               + ";EDato"
                               + ";!ETid"
                               + ";BrukerID"
                               + ";Butik"
                               + ";!TelleNr"
                               + ";!SeqNr"
                               + ";!RabKr"
                               + ";!Nedskrevet"
                               + ";!MatKod"
                               + ";!LopNr"
                               + ";!Kode"
                               + ";!Farg"
                             ,"WHERE false"
                             ,"linjenr DESC").
  hSearchField = DYNAMIC-FUNCTION("NewBrowseSearchField",SearchField:HANDLE,hBrowse,1).
  DYNAMIC-FUNCTION("CreateObjectLink",hSearchField,hBrowse).
  
/*   DYNAMIC-FUNCTION('setAttribute',hBrowse,'calcfieldproc','vpiartbas_varebok_brwcalc.p'). */
  DYNAMIC-FUNCTION("createObjectLink",hBrowse,THIS-PROCEDURE).
  
  hToolbar = DYNAMIC-FUNCTION("NewToolBar",
                    rectToolBar:HANDLE,             /* Rectangle to define coordinates for toolbar */
                    "Fil",                         /* Corresponding menu label - no menu if blank */
                    "AnnulerKobling;Annuler kobling,Rule"
                    ,"maxborder").                  /* Misc - enable, maxborder.. */

  DYNAMIC-FUNCTION("CreateObjectLink",hBrowse,hToolbar).  
  DYNAMIC-FUNCTION("CreateParentLink",hBrowse-2,hBrowse,'tellenr').

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
DYNAMIC-FUNCTION('setCurrentObject',hBrowse).  
DYNAMIC-FUNCTION("CreateObjectLink",hBrowse,hToolbar).  
hChild = ?.
  APPLY 'value-changed' TO hBrowse.
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
APPLY "entry" TO hBrowse.
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
/* DEF VAR cWhere   AS CHAR NO-UNDO.                                                                                          */
/* DEF VAR cVPIDato AS CHAR NO-UNDO.                                                                                          */
/*                                                                                                                            */
/*   DO WITH FRAME {&FRAME-NAME}:                                                                                             */
/*     ASSIGN                                                                                                                 */
/*         cVPIDato = '¤' +                                                                                                   */
/*                    (IF INPUT FraVPIDato <> ? THEN STRING(INPUT FraVPIDato) ELSE '') + '¤' +                                */
/*                    (IF INPUT TilVPIDato <> ? THEN STRING(INPUT TilVPIDato) ELSE '')                                        */
/*         .                                                                                                                  */
/*                                                                                                                            */
/*     DYNAMIC-FUNCTION("setAttribute",hBrowse,"calcparamChkArtReg",tbAvvikArtBas:SCREEN-VALUE).                              */
/*     DYNAMIC-FUNCTION("setAttribute",hBrowse,"calcparamChkVarebok",STRING(VarebokNr) + "¤" + tbAvvikVarebok:SCREEN-VALUE).  */
/*     DYNAMIC-FUNCTION("setAttribute",hBrowse,"calcparamChkPris",STRING(VarebokNr) + "¤" + tbAvvikPris:SCREEN-VALUE).        */
/*     DYNAMIC-FUNCTION("setAttribute",hBrowse,"calcparamChkArtInfo",STRING(VarebokNr) + "¤" + tbAvvikArtInfo:SCREEN-VALUE).  */
/*     DYNAMIC-FUNCTION("setAttribute",hBrowse,"calcparamChkStrek",tbAvvikStrekKode:SCREEN-VALUE).                            */
/*     DYNAMIC-FUNCTION("setAttribute",hBrowse,"calcparamChkAnonseArtikkel",tbAnonseArtikkel:SCREEN-VALUE).                   */
/*     DYNAMIC-FUNCTION("setAttribute",hBrowse,"calcparamChkKontroll",IF cbcKontroll:SCREEN-VALUE = ?                         */
/*                                                                      THEN ''                                               */
/*                                                                      ELSE (cbcKontroll:SCREEN-VALUE + cVPIDato) ).         */
/*     DYNAMIC-FUNCTION("setAttribute",hBrowse,"QueryFilter",'').                                                             */
/*                                                                                                                            */
/*     cWhere = buildFilter(cWhere,varetekst:HANDLE,'beskr','BEGINS').                                                        */
/*     cWhere = cWhere + buildFilter(cWhere,LevKod:HANDLE,'LevKod','BEGINS').                                                 */
/*     cWhere = cWhere + buildFilter(cWhere,fraVPIdato:HANDLE,'VPIdato','GE').                                                */
/*     cWhere = cWhere + buildFilter(cWhere,tilVPIdato:HANDLE,'VPIdato','LE').                                                */
/*     cWhere = cWhere + buildFilter(cWhere,SaSong:HANDLE,'SaSong','EQ').                                                     */
/*     cWhere = cWhere + buildFilter(cWhere,LevNr:HANDLE,'LevNr','EQ').                                                       */
/*     cWhere = cWhere + buildFilter(cWhere,behStatus:HANDLE,'behStatus','EQ').                                               */
/*     cWhere = cWhere + buildFilter(cWhere,Utvidetsok:HANDLE,'Utvidetsok','CONTAINS').                                       */
/* /*     cWhere = cWhere + buildFilter(cWhere,tbAnonseArtikkel:HANDLE,'AnonseArtikkel','EQ'). */                             */
/*                                                                                                                            */
/*     DYNAMIC-FUNCTION("setAttribute",hBrowse,"QueryFilter",cWhere).                                                         */
/*                                                                                                                            */
/*     ASSIGN                                                                                                                 */
/*       fraVPIDato:MODIFIED      = FALSE                                                                                     */
/*       tilVPIDato:MODIFIED      = FALSE                                                                                     */
/*       Levkod:MODIFIED          = FALSE                                                                                     */
/*       SaSong:MODIFIED          = FALSE                                                                                     */
/*       LevNr:MODIFIED           = FALSE                                                                                     */
/*       VareTekst:MODIFIED       = FALSE .                                                                                   */
/*                                                                                                                            */
/*     hParentBrowse = DYNAMIC-FUNCTION("getLinkedObject",hBrowse,"parent","from").                                           */
/*     IF hParentBrowse:QUERY:GET-BUFFER-HANDLE(1):AVAIL AND                                                                  */
/*        Strekkode:SCREEN-VALUE NE "" THEN                                                                                   */
/*       DYNAMIC-FUNCTION("setAttribute",hBrowse,"prescanQueryFilter",                                                        */
/*                        "VPIstrekkode WHERE VPIStrekkode.EkstVPILevNr = "                                                   */
/*                        + STRING(hParentBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("EkstVPILevNr"):BUFFER-VALUE)        */
/*                        + " and VPIstrekkode.Kode = '" + Strekkode:SCREEN-VALUE + "'"                                       */
/*                        + ",FIRST VPIartBas OF VPIstrekkode NO-LOCK").                                                      */
/*     ELSE                                                                                                                   */
/*       DYNAMIC-FUNCTION("setAttribute",hBrowse,"prescanQueryFilter","").                                                    */
/*                                                                                                                            */
/*                                                                                                                            */
/*   END.                                                                                                                     */
/*                                                                                                                            */
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
/*   ihBrowse:MOVE-COLUMN(22,1).                                                                       */
/*                                                                                                     */
/*   ASSIGN                                                                                            */
/*     ihBrowse:GET-BROWSE-COLUMN(1):WIDTH-PIXELS  = 120                                               */
/*     ihBrowse:GET-BROWSE-COLUMN(2):WIDTH-PIXELS  = 50                                                */
/*     ihBrowse:GET-BROWSE-COLUMN(3):WIDTH-PIXELS  = 50                                                */
/*     ihBrowse:GET-BROWSE-COLUMN(4):WIDTH-PIXELS  = 150                                               */
/*     ihBrowse:GET-BROWSE-COLUMN(5):WIDTH-PIXELS  = 100                                               */
/*     ihBrowse:GET-BROWSE-COLUMN(6):WIDTH-PIXELS  = 50                                                */
/*     ihBrowse:GET-BROWSE-COLUMN(7):WIDTH-PIXELS  = 20                                                */
/*     ihBrowse:GET-BROWSE-COLUMN(8):WIDTH-PIXELS  = 20                                                */
/*     ihBrowse:GET-BROWSE-COLUMN(9):WIDTH-PIXELS  = 20                                                */
/*     ihBrowse:GET-BROWSE-COLUMN(10):WIDTH-PIXELS = 20                                                */
/*     ihBrowse:GET-BROWSE-COLUMN(11):WIDTH-PIXELS = 20                                                */
/*     ihBrowse:GET-BROWSE-COLUMN(12):WIDTH-PIXELS = 20                                                */
/*     ihBrowse:GET-BROWSE-COLUMN(13):WIDTH-PIXELS = 20                                                */
/*                                                                                                     */
/*     hbcChkArtReg   = ihBrowse:GET-BROWSE-COLUMN(7)                                                  */
/*     hbfChkArtReg   = ihBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('ChkArtReg')                  */
/*     hbcChkVarebok  = ihBrowse:GET-BROWSE-COLUMN(8)                                                  */
/*     hbfChkVarebok  = ihBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('ChkVarebok')                 */
/*     hbcChkPris     = ihBrowse:GET-BROWSE-COLUMN(9)                                                  */
/*     hbfChkPris     = ihBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('ChkPris')                    */
/*     hbcChkArtInfo  = ihBrowse:GET-BROWSE-COLUMN(10)                                                 */
/*     hbfChkArtInfo  = ihBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('ChkArtInfo')                 */
/*     hbcChkStrek    = ihBrowse:GET-BROWSE-COLUMN(11)                                                 */
/*     hbfChkStrek    = ihBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('ChkStrek')                   */
/*     hbcChkAnonseArtikkel    = ihBrowse:GET-BROWSE-COLUMN(12)                                        */
/*     hbfChkAnonseArtikkel    = ihBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('ChkAnonseArtikkel') */
/*     hbcChkKontroll    = ihBrowse:GET-BROWSE-COLUMN(13)                                              */
/*     hbfChkKontroll    = ihBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('ChkKontroll')             */
/*                                                                                                     */
/*     hbcChkArtReg:COLUMN-FONT         = 8                                                            */
/*     hbcChkVarebok:COLUMN-FONT        = 8                                                            */
/*     hbcChkPris:COLUMN-FONT           = 8                                                            */
/*     hbcChkArtInfo:COLUMN-FONT        = 8                                                            */
/*     hbcChkStrek:COLUMN-FONT          = 8                                                            */
/*     hbcChkAnonseArtikkel:COLUMN-FONT = 8                                                            */
/*     hbcChkKontroll:COLUMN-FONT       = 8                                                            */
/*   .                                                                                                 */
/*                                                                                                     */
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION InitializeResize C-Win 
FUNCTION InitializeResize RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  DYNAMIC-FUNCTION("setNoResizeX", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, "searchField").
  DYNAMIC-FUNCTION("setNoResizeY", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, "rectBrowse").

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

