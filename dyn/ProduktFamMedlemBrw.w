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
                          
DEF VAR hToolbar          AS HANDLE NO-UNDO.
DEF VAR hBrowse           AS HANDLE NO-UNDO.
 
DEF VAR hParent           AS HANDLE NO-UNDO.
DEF VAR hParentQuery      AS HANDLE NO-UNDO.

DEF VAR hChild            AS HANDLE NO-UNDO.
DEF VAR hArtBasSok        AS HANDLE NO-UNDO.
DEF VAR bFirstRecord      AS LOG NO-UNDO.

DEF VAR cButikkNr       AS CHAR   NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rectBrowse rectToolBar 
&Scoped-Define DISPLAYED-OBJECTS fLowPrice fDiffPrice fHighPrice 

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD adjustBrowseColumns C-Win 
FUNCTION adjustBrowseColumns RETURNS LOGICAL
  ( INPUT ihBrowse     AS HANDLE,
    INPUT icBrowseName AS CHAR)  FORWARD.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setMinMaksPris C-Win 
FUNCTION setMinMaksPris RETURNS LOGICAL
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
DEFINE VARIABLE fDiffPrice AS DECIMAL FORMAT "->>>>>>>>9.99":U INITIAL 0 
     LABEL "Diff" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fHighPrice AS DECIMAL FORMAT "->>>>>>>>9.99":U INITIAL 0 
     LABEL "Høyest" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fLowPrice AS DECIMAL FORMAT "->>>>>>>>9.99":U INITIAL 0 
     LABEL "Laveste" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE RECTANGLE rectBrowse
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 67 BY 7.38.

DEFINE RECTANGLE rectToolBar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 28 BY .95.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     fLowPrice AT ROW 2.43 COL 10 COLON-ALIGNED
     fDiffPrice AT ROW 2.43 COL 29.8 COLON-ALIGNED
     fHighPrice AT ROW 2.43 COL 53 COLON-ALIGNED
     rectBrowse AT ROW 3.62 COL 2
     rectToolBar AT ROW 1.24 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SCROLLABLE SIZE 69.2 BY 10.14.


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
         TITLE              = "Artikkler"
         HEIGHT             = 10.19
         WIDTH              = 69.4
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
       FRAME DEFAULT-FRAME:HEIGHT           = 10.14
       FRAME DEFAULT-FRAME:WIDTH            = 69.2.

/* SETTINGS FOR FILL-IN fDiffPrice IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       fDiffPrice:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN fHighPrice IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       fHighPrice:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN fLowPrice IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       fLowPrice:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

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
ON END-ERROR OF C-Win /* Artikkler */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Artikkler */
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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BrowseColumnLookup C-Win 
PROCEDURE BrowseColumnLookup :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR hCurrObject AS HANDLE NO-UNDO.
  
  hCurrObject = DYNAMIC-FUNCTION("getCurrentObject").
  
  IF /*hCurrObject = hProdFamArtikkelNrOverlay 
    OR*/ hCurrObject:NAME = 'ProdFamArtikkelNr' THEN
  DO:
    DEF VAR iMinMaksPris AS INT NO-UNDO.
    DEF VAR iArtPris     AS INT NO-UNDO.
  
    IF VALID-HANDLE(hArtBasSok) THEN 
      APPLY "close" TO hArtBasSok.

    RUN ProdFamArtBasSok.w PERSIST SET hArtBasSok.  
    /*RUN ArtBasSok2.w PERSIST SET hArtBasSok.*/
  /*   DYNAMIC-FUNCTION("setButikkNr" IN hArtBasSok,hParentBuffer:BUFFER-FIELD("ButikkNr"):BUFFER-VALUE).  */
  /*   DYNAMIC-FUNCTION("setOrdreId"  IN hArtBasSok,hParentBuffer:BUFFER-FIELD("KOrdre_Id"):BUFFER-VALUE). */
    IF NOT bFirstRecord THEN
    DO:
      DYNAMIC-FUNCTION("setCloseOnSelect" IN hArtBasSok,NO).
      ASSIGN 
        iArtPris = hParentQuery:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('ProdFamPrisLinje'):BUFFER-VALUE
        iMinMaksPris = IF iArtPris = 0 THEN 0 ELSE RANDOM(10,100)
      .
      DYNAMIC-FUNCTION("setMinMaksPris" IN hArtBasSok,iMinMaksPris,iArtPris).
    END.
    ELSE
    DO:
      DYNAMIC-FUNCTION("setCloseOnSelect" IN hArtBasSok,YES).
    END.
    /*DYNAMIC-FUNCTION("setUpdateCurrentRow" IN hArtBasSok,YES).*/
    RUN InitializeObject IN hArtBasSok.
  
    RUN MoveToTop IN hArtBasSok.
  
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DeleteRecord C-Win 
PROCEDURE DeleteRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR cArtNr AS CHAR NO-UNDO.

  RUN SUPER.
  
  cArtNr = DYNAMIC-FUNCTION('getFieldValues','ProduktFamMedlem','WHERE ProduktFamMedlem.ProdFamId         = DEC(' + STRING(hParentQuery:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('ProdFamId'):BUFFER-VALUE) + ')','ProdFamArtikkelnr').
  ASSIGN 
    bFirstRecord = cArtNr = ?
  .

  DYNAMIC-FUNCTION('runProc','produktfamilie_update.p',STRING(hParentQuery:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('ProdFamId'):BUFFER-VALUE) + ';ProdFamPrisLinje;' + STRING(0),?).
  
  DYNAMIC-FUNCTION('setMinMaksPris').
  RUN getInfo.

/*   IF bFirstRecord AND VALID-HANDLE(hArtBasSok) THEN */
/*   DO:                                               */
/*     RUN StartQuery IN hArtBasSok.                   */
/*   END.                                              */
  

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
  DISPLAY fLowPrice fDiffPrice fHighPrice 
      WITH FRAME DEFAULT-FRAME.
  ENABLE rectBrowse rectToolBar 
      WITH FRAME DEFAULT-FRAME.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getInfo C-Win 
PROCEDURE getInfo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEF VAR cReturn AS CHAR NO-UNDO.
  IF VALID-HANDLE(hParentQuery) AND hParentQuery:QUERY:GET-BUFFER-HANDLE(1):AVAIL THEN 
  DO:
    IF NOT DYNAMIC-FUNCTION("runproc","getproduktfammedlem_info.p",
                            hParentQuery:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("ProdFamId"):BUFFER-VALUE
                             ,?) THEN
      DYNAMIC-FUNCTION("DoMessage",0,1,DYNAMIC-FUNCTION("getTransactionMessage"),"Feil","").
    ELSE 
    DO WITH FRAME {&FRAME-NAME}:
      ASSIGN 
        cReturn = DYNAMIC-FUNCTION("getTransactionMessage")
        fLowPrice  = DEC(ENTRY(1,cReturn,'|'))
        fDiffPrice = DEC(ENTRY(2,cReturn,'|'))
        fHighPrice = DEC(ENTRY(3,cReturn,'|'))
      .
      DISPLAY fLowPrice fDiffPrice fHighPrice WITH FRAME {&FRAME-NAME}.
    END.
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
hBrowse = DYNAMIC-FUNCTION("NewBrowse",          /* Create a browse object */
                    rectBrowse:HANDLE,              /* Rectangle to define coordinates for browse */
                    100,                            /* Rows to batch */
                    "",                             /* Browse properties, ie MULTIPLE,NUM-LOCKED-COLUMNS, etc */
                    "ProduktFamMedlem"
                    + ";!ProdFamId;ProdFamArtikkelNr;!ProdFamStrKode"
                    + ";!RegistrertDato;!RegistrertTid;!RegistrertAv;!ETid;!EDato;!BrukerID"
                    + ",ArtBas;!ArtikkelNr;Beskr;vg;hg"
                    + ";+InnPris|DECIMAL|>><>>><>>9.99|artpris_innpris(ROWID)|Inn.Pris"
                    + ";+Pris|DECIMAL|>><>>><>>9.99|artpris_pris(ROWID)|Pris"
                    + ",ArtPris;!ArtikkelNr;!ProfilNr" /*;pris!2*/

                   ,"WHERE false " 
                       + ", FIRST artbas  NO-LOCK WHERE artbas.artikkelnr  = ProduktFamMedlem.ProdFamArtikkelNr"
                       + ", FIRST artpris NO-LOCK WHERE artpris.artikkelnr = artbas.artikkelnr"
                    ,"sort|ProdFamArtikkelNr").             /* Initial sort column */

  hBrowse:NAME = 'brw2'.
  DYNAMIC-FUNCTION("createObjectLink",hBrowse,THIS-PROCEDURE).

  cButikkNr      = IF cButikkNr = "" THEN DYNAMIC-FUNCTION("getFieldValues","Bruker","WHERE BrukerId = '" + DYNAMIC-FUNCTION("getASuserId") + "'","ButikkNr") ELSE cButikkNr.
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"calcfieldproc","artbassok2_brwcalc.p"). 

  hToolbar = DYNAMIC-FUNCTION("NewToolBar",
                   rectToolBar:HANDLE,             /* Rectangle to define coordinates for toolbar */
                   "File",                         /* Corresponding menu label - no menu if blank */
                   "new,Delete"
                   + ",rule,excel;Eksporter til E&xcel,flatview,BrowseConfig" /* Buttons / Menu items: action;label;tooltip;Method;image,action;label.. 
                                                      Any number of properties accepted (one ok - if predef. action) */
                   ,"maxborder").                  /* Misc - enable, maxborder.. */
  
  DYNAMIC-FUNCTION("CreateObjectLink",hBrowse,hToolbar).    
  
  SUBSCRIBE TO 'getInfo' IN hParent.

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
APPLY "entry" TO hBrowse.
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
  DEF VAR cArtNr AS CHAR NO-UNDO.

  IF VALID-HANDLE(hArtBasSok) THEN 
    APPLY "close" TO hArtBasSok.

  RUN ProdFamArtBasSok.w PERSIST SET hArtBasSok.
  
  DYNAMIC-FUNCTION("setCloseOnSelect" IN hArtBasSok,NO).
  
  cArtNr = DYNAMIC-FUNCTION('getFieldValues','ProduktFamMedlem','WHERE ProduktFamMedlem.ProdFamId = DEC(' + STRING(hParentQuery:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('ProdFamId'):BUFFER-VALUE) + ')','ProdFamArtikkelnr').
  ASSIGN 
    bFirstRecord = cArtNr = ?
  .

  DYNAMIC-FUNCTION('setMinMaksPris').
  DYNAMIC-FUNCTION('setProdFamId' IN hArtBasSok,hParentQuery:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('ProdFamId'):BUFFER-VALUE).
  RUN InitializeObject IN hArtBasSok.
  RUN MoveToTop IN hArtBasSok.

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
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"calcparamPris",cButikkNr + "¤" + STRING(-99999999999) + "¤" + STRING(9999999999)).
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"calcparamInnPris",cButikkNr).
  
  RUN SUPER.
  

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
  DEF VAR bReturnFocus  AS LOG   NO-UNDO.
  DEF VAR cStrList      AS CHAR  NO-UNDO.
  DEF VAR bOkStr        AS LOG   NO-UNDO.
  DEF VAR fArtNr        AS DEC   NO-UNDO.
  DEF VAR fArtikkelPris AS DEC   NO-UNDO.
  DEF VAR bCurrentFlag  AS LOG   NO-UNDO.
  DEF VAR fProdFamId    AS DEC   NO-UNDO.

  ASSIGN
    fProdFamId = hParentQuery:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('ProdFamId'):BUFFER-VALUE
    fArtNr     = DYNAMIC-FUNCTION('getFieldValues','ProduktFamMedlem','WHERE ProduktFamMedlem.ProdFamId       = DEC(' + STRING(fProdFamId) + ')'
                                                                   + ' AND ProduktFamMedlem.ProdFamArtikkelNr = DEC(' + STRING(ifArtikkelNr) + ')' ,'ProdFamArtikkelNr')
  NO-ERROR.
  
  IF fArtNr NE 0 AND fArtNr NE ? THEN
  DO:
    MESSAGE 'allerede registrert !'
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
    LEAVE.
  END.
  DO WITH FRAME {&FRAME-NAME}:
    bok =  DYNAMIC-FUNCTION('runProc','produktfammedlem_update.p',STRING(fProdFamId) 
                                                                + ';' + STRING(ifArtikkelNr)
                                                                + ';' + STRING(icStorl)
                           ,?).
    APPLY "value-changed" TO hParentQuery.

    IF bOk AND bFirstRecord THEN
    DO:
      fArtikkelPris = DEC(DYNAMIC-FUNCTION('getFieldValues','artpris','WHERE artpris.artikkelnr = DEC(' + STRING(ifArtikkelNr) + ')'
                                                                     + ' AND artpris.profilnr = 1' ,'pris;1')).
      bok =  DYNAMIC-FUNCTION('runProc','produktfamilie_update.p',STRING(fProdFamId) + ';ProdFamPrisLinje;' + STRING(fArtikkelPris),?).
      IF NOT bOk THEN MESSAGE DYNAMIC-FUNCTION("getTransactionMessage")
                        VIEW-AS ALERT-BOX INFO BUTTONS OK.

    END.
    
    ASSIGN
      bCurrentFlag = bFirstRecord
      bFirstRecord = IF bOk THEN FALSE ELSE bFirstRecord
    .
  
    DYNAMIC-FUNCTION('setMinMaksPris').
    
    IF bCurrentFlag AND VALID-HANDLE(hArtBasSok) THEN
    DO:
      RUN StartQuery IN hArtBasSok.
    END.
    
    RETURN bOk.    
  END. /*FRAME*/

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION adjustBrowseColumns C-Win 
FUNCTION adjustBrowseColumns RETURNS LOGICAL
  ( INPUT ihBrowse     AS HANDLE,
    INPUT icBrowseName AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose: Hook fra NewBrowse slik at kolonner kan flyttes regelbasert.
           Gjør også justeringer av kolonnebredder her slik at disse 
           blir tatt vare på.
    Notes:  
------------------------------------------------------------------------------*/
/*   ihBrowse:GET-BROWSE-COLUMN(1):WIDTH-PIXELS = 60.  */
/*   ihBrowse:GET-BROWSE-COLUMN(2):WIDTH-PIXELS = 200. */
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setMinMaksPris C-Win 
FUNCTION setMinMaksPris RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR iMinMaksPris AS INT  NO-UNDO.
  DEF VAR cArtNr       AS CHAR NO-UNDO.
  DEF VAR iArtPris     AS INT  NO-UNDO.
  DEF VAR iTemp        AS INT NO-UNDO.

  ASSIGN 
    iTemp = DYNAMIC-FUNCTION("getFieldValues","SysPara","WHERE SysHId = 17 and SysGr = 1 and ParaNr = 3","Parameter1")
    iTemp = IF iTemp = 0 THEN 50 ELSE iTEmp /*avtalt med TN*/
  .
  IF NOT bFirstRecord THEN
  DO:
    ASSIGN 
      iArtPris = INT(DYNAMIC-FUNCTION("getFieldValues","ProduktFamilie","WHERE ProdFamId = " + STRING(hParentQuery:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('ProdFamId'):BUFFER-VALUE),"ProdFamPrisLinje"))
/*       iArtPris = hParentQuery:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('ProdFamPrisLinje'):BUFFER-VALUE */
      iMinMaksPris = IF iArtPris = 0 THEN 0 ELSE iTemp
    .
  END.
  ELSE
    ASSIGN 
      iArtPris     = 0
      iMinMaksPris = 0
    .
  IF VALID-HANDLE(hArtBasSok) THEN DYNAMIC-FUNCTION("setMinMaksPris" IN hArtBasSok,iMinMaksPris,iArtPris).
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
hParentQuery = ihQuery.

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

