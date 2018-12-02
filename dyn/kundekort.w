&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
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

DEF VAR hToolbar        AS HANDLE NO-UNDO.
DEF VAR hBrowse         AS HANDLE NO-UNDO.
DEF VAR hFieldMap       AS HANDLE NO-UNDO.
DEF VAR hSearchField    AS HANDLE NO-UNDO.

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
&Scoped-Define ENABLED-OBJECTS btnFraUtgarDato fltKundeNr fltKortNr ~
fltFraUtgarDato fltTilUtgarDato fltInnehaver btnBlank btnStartUtvalg ~
btnTilUtgarDato btnKunde fltButikkNr fltNavn rectBrowse rectToolBar ~
rectWinToolbar searchField 
&Scoped-Define DISPLAYED-OBJECTS fltKundeNr fltKortNr fltFraUtgarDato ~
fltTilUtgarDato fltInnehaver fltButikkNr fltNavn 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnBlank 
     LABEL "Blank filter" 
     SIZE 13 BY 1.

DEFINE BUTTON btnFraUtgarDato 
     IMAGE-UP FILE "gif/calendar.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "..." 
     SIZE 3.8 BY 1.

DEFINE BUTTON btnKunde 
     LABEL "..." 
     SIZE 4 BY 1.

DEFINE BUTTON btnStartUtvalg 
     LABEL "Start søk" 
     SIZE 13 BY 1.

DEFINE BUTTON btnTilUtgarDato 
     IMAGE-UP FILE "gif/calendar.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "..." 
     SIZE 3.8 BY 1.

DEFINE VARIABLE fltButikkNr AS INTEGER FORMAT ">>>>>9" INITIAL 0 
     LABEL "Butikk" 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1.

DEFINE VARIABLE fltFraUtgarDato AS DATE FORMAT "99/99/99" 
     LABEL "Utgår dato fra" 
     VIEW-AS FILL-IN 
     SIZE 13.2 BY 1.

DEFINE VARIABLE fltInnehaver AS CHARACTER FORMAT "X(30)" 
     LABEL "Innehaver" 
     VIEW-AS FILL-IN 
     SIZE 34.2 BY 1.

DEFINE VARIABLE fltKortNr AS CHARACTER FORMAT "X(22)" 
     LABEL "Kundekortnr" 
     VIEW-AS FILL-IN 
     SIZE 25 BY 1.

DEFINE VARIABLE fltKundeNr AS DECIMAL FORMAT ">>>>>>>>>>>>9" INITIAL 0 
     LABEL "Kundenummer" 
     VIEW-AS FILL-IN 
     SIZE 25 BY 1.

DEFINE VARIABLE fltNavn AS CHARACTER FORMAT "X(22)" 
     LABEL "Kundenavn" 
     VIEW-AS FILL-IN 
     SIZE 25 BY 1.

DEFINE VARIABLE fltTilUtgarDato AS DATE FORMAT "99/99/99" 
     LABEL "til" 
     VIEW-AS FILL-IN 
     SIZE 13.2 BY 1.

DEFINE RECTANGLE rectBrowse
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 175 BY 25.

DEFINE RECTANGLE rectToolBar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 28 BY .95.

DEFINE RECTANGLE rectWinToolbar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 9.8 BY .91.

DEFINE RECTANGLE searchField
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 19 BY .95.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     btnFraUtgarDato AT ROW 2.38 COL 138.2 NO-TAB-STOP 
     fltKundeNr AT ROW 2.48 COL 78.2 COLON-ALIGNED
     fltKortNr AT ROW 3.43 COL 78.2 COLON-ALIGNED
     fltFraUtgarDato AT ROW 2.48 COL 122.8 COLON-ALIGNED
     fltTilUtgarDato AT ROW 2.43 COL 143.6 COLON-ALIGNED
     fltInnehaver AT ROW 3.43 COL 122.8 COLON-ALIGNED
     btnBlank AT ROW 2.43 COL 164
     btnStartUtvalg AT ROW 3.48 COL 164
     btnTilUtgarDato AT ROW 2.43 COL 159 NO-TAB-STOP 
     btnKunde AT ROW 2.48 COL 105.2 NO-TAB-STOP 
     fltButikkNr AT ROW 3.43 COL 8 COLON-ALIGNED
     fltNavn AT ROW 3.43 COL 36.8 COLON-ALIGNED
     rectBrowse AT ROW 4.81 COL 2
     rectToolBar AT ROW 1.14 COL 22.4
     rectWinToolbar AT ROW 1.14 COL 167
     searchField AT ROW 1.14 COL 2.2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 176.2 BY 28.86.


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
         TITLE              = "Kundekortliste m/kobling til medlemskort"
         HEIGHT             = 28.86
         WIDTH              = 176.2
         MAX-HEIGHT         = 57.14
         MAX-WIDTH          = 384
         VIRTUAL-HEIGHT     = 57.14
         VIRTUAL-WIDTH      = 384
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = yes
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
   FRAME-NAME Custom                                                    */
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
ON END-ERROR OF C-Win /* Kundekortliste m/kobling til medlemskort */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Kundekortliste m/kobling til medlemskort */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-RESIZED OF C-Win /* Kundekortliste m/kobling til medlemskort */
DO:
  DYNAMIC-FUNCTION("setWidgetResize",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE:CURRENT-WINDOW,"Resize","").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnBlank
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnBlank C-Win
ON CHOOSE OF btnBlank IN FRAME DEFAULT-FRAME /* Blank filter */
DO:
        DO WITH FRAME {&Frame-name}:
            ASSIGN
                fltKundeNR:SCREEN-VALUE      = ''
                fltKortNr:SCREEN-VALUE       = ''
                fltFraUtgarDato:SCREEN-VALUE = ''
                fltTilutgarDato:SCREEN-VALUE = ''
                fltButikkNr:SCREEN-VALUE     = ''
                .
            DYNAMIC-FUNCTION("setCurrentObject",hBrowse).
            RUN OpenQuery.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnFraUtgarDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnFraUtgarDato C-Win
ON CHOOSE OF btnFraUtgarDato IN FRAME DEFAULT-FRAME /* ... */
OR F10 OF fltFraUtgarDato IN FRAME DEFAULT-FRAME
DO:

  RUN Cal.w (fltFraUtgarDato:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnKunde
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnKunde C-Win
ON CHOOSE OF btnKunde IN FRAME DEFAULT-FRAME /* ... */
OR F10 OF fltKundeNr IN FRAME DEFAULT-FRAME
DO:
  DEF VAR cRowIdList  AS CHAR NO-UNDO.
  DEF VAR cIdList     AS CHAR NO-UNDO.  
  DEF VAR bOk         AS LOG  NO-UNDO.
  DEF VAR cWhere      AS CHAR NO-UNDO.
  
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxSelector.w (THIS-PROCEDURE,0,
                      "Kunde"      
                      + ";KundeNr"  
                      + ";Navn;Adresse1;PostNr"
                      + ",Post;Beskrivelse"
                      , "where true, first Post where Post.PostNr = Kunde.PostNr"
                      ,INPUT-OUTPUT cRowIdList,
                      "KundeNr", /* Primary key */
                      INPUT-OUTPUT cIdList,
                      "","",
                      OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF bOk THEN
  DO WITH FRAME {&Frame-name}:
    ASSIGN 
      fltKundeNr:SCREEN-VALUE = cIdList
    .
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnStartUtvalg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnStartUtvalg C-Win
ON CHOOSE OF btnStartUtvalg IN FRAME DEFAULT-FRAME /* Start søk */
DO:
  DYNAMIC-FUNCTION("setCurrentObject",hBrowse).
  RUN OpenQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnTilUtgarDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnTilUtgarDato C-Win
ON CHOOSE OF btnTilUtgarDato IN FRAME DEFAULT-FRAME /* ... */
OR F10 OF fltTilUtgarDato IN FRAME DEFAULT-FRAME
    DO:

        RUN Cal.w (fltTilUtgarDato:HANDLE).
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fltButikkNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fltButikkNr C-Win
ON RETURN OF fltButikkNr IN FRAME DEFAULT-FRAME /* Butikk */
DO:
  APPLY "CHOOSE" TO btnStartUtvalg.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fltFraUtgarDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fltFraUtgarDato C-Win
ON RETURN OF fltFraUtgarDato IN FRAME DEFAULT-FRAME /* Utgår dato fra */
DO:
  APPLY "CHOOSE" TO btnStartUtvalg.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fltInnehaver
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fltInnehaver C-Win
ON RETURN OF fltInnehaver IN FRAME DEFAULT-FRAME /* Innehaver */
DO:
  APPLY "CHOOSE" TO btnStartUtvalg.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fltKortNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fltKortNr C-Win
ON RETURN OF fltKortNr IN FRAME DEFAULT-FRAME /* Kundekortnr */
DO:
  APPLY "CHOOSE" TO btnStartUtvalg.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fltKundeNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fltKundeNr C-Win
ON RETURN OF fltKundeNr IN FRAME DEFAULT-FRAME /* Kundenummer */
DO:
  APPLY "CHOOSE" TO btnStartUtvalg.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fltNavn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fltNavn C-Win
ON RETURN OF fltNavn IN FRAME DEFAULT-FRAME /* Kundenavn */
DO:
  APPLY "CHOOSE" TO btnStartUtvalg.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fltTilUtgarDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fltTilUtgarDato C-Win
ON RETURN OF fltTilUtgarDato IN FRAME DEFAULT-FRAME /* til */
DO:
  APPLY "CHOOSE" TO btnStartUtvalg.
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
  IF NOT DYNAMIC-FUNCTION("DoCleanUpObjects",THIS-PROCEDURE:CURRENT-WINDOW) THEN RETURN NO-APPLY.
  DYNAMIC-FUNCTION("setCleanUpResize",THIS-PROCEDURE:CURRENT-WINDOW).
  PUBLISH "InvalidateHandle" (THIS-PROCEDURE).
  RUN disable_UI.
END.

{incl/wintrigg.i}

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

  RUN enable_UI.
  &IF DEFINED(UIB_is_Running) NE 0 &THEN
    RUN InitializeObject.
  &ENDIF

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE deleteRecord C-Win 
PROCEDURE deleteRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR iReturn     AS INT  NO-UNDO.
DEF VAR ocValue     AS CHAR NO-UNDO.
DEF VAR cStatusList AS CHAR NO-UNDO.
DEF VAR cRowIdList  AS CHAR NO-UNDO.

  RUN JBoxBrowseMsgUpdateVal.w ("Slette linje(r) ?",
                                hBrowse:NUM-SELECTED-ROWS,
                                IF INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"Totalcount")) LT INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"rowstobatch")) THEN
                                  INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"Totalcount"))
                                ELSE 99999,
                                "",
                                OUTPUT ocValue, 
                                OUTPUT iReturn).

IF iReturn = 1 THEN
  bOk = DYNAMIC-FUNCTION("ProcessQuery",hBrowse,"kundekort_delete.p",'').
ELSE IF iReturn = 2 THEN
DO:
  DO ix = 1 TO hBrowse:NUM-SELECTED-ROWS:
    IF hBrowse:FETCH-SELECTED-ROW(ix) THEN
      cRowIdList = cRowIdList + hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('RowIdent1'):BUFFER-VALUE + ','.
  END.
  bOk = DYNAMIC-FUNCTION("ProcessSelectedRows",hBrowse,"kundekort_delete.p",'').
END.
ELSE
  LEAVE.
IF NOT bOK THEN
  DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Feil i sending av informasjon ",""). 

RUN InvokeMethod (hBrowse,'OpenQuery').
/*
IF iReturn = 1 
    THEN RUN InvokeMethod (hBrowse,'OpenQuery'). 
ELSE 
    DYNAMIC-FUNCTION("RefreshRowids",hBrowse,TRIM(cRowIdList,",")).
*/
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplayRecord C-Win 
PROCEDURE DisplayRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  RUN SUPER.
  /*KundeNr:SENSITIVE IN FRAME {&FRAME-NAME} =  DYNAMIC-FUNCTION('getToolbarState',hToolbar) = 'New'.*/
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
  DISPLAY fltKundeNr fltKortNr fltFraUtgarDato fltTilUtgarDato fltInnehaver 
          fltButikkNr fltNavn 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE btnFraUtgarDato fltKundeNr fltKortNr fltFraUtgarDato fltTilUtgarDato 
         fltInnehaver btnBlank btnStartUtvalg btnTilUtgarDato btnKunde 
         fltButikkNr fltNavn rectBrowse rectToolBar rectWinToolbar searchField 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitializeObject C-Win 
PROCEDURE InitializeObject :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:      InitializeObject is called automatically when using JBoxDynMenu.w 
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
  hBrowse = DYNAMIC-FUNCTION("NewBrowse",          /* Create a browse object */
                    rectBrowse:HANDLE,              /* Rectangle to define coordinates for browse */
                    100,                            /* Rows to batch */
                    "MULTIPLE",                             /* Browse properties, ie MULTIPLE,NUM-LOCKED-COLUMNS, etc */
                    "KundeKort"
                    + ";KundeNr|KundeNr" 
                    + ";KortNr|Kortnr|x(6)"
                    + ";Innehaver|Innehaver|x(15)"
                    + ";AktivertDato"
                    + ";UtgarDato"
                    + ";Sperret"                             
                    + ";InterntKKortId"
                    /*+ ";Merknad|(Kunde)Merknad (Kundekort)"*/
                    /*+ ";+fMKortEAN|DECIMAL|>>>>>>>>>>>>9|medlemskort_ean(InterntKKortId)|(EAN)Kortnr@10"*/
                    /*+ ";+fMKortNr|DECIMAL|>>>>>>>>9|medlemskort_kortnr(InterntKKortId)|(Medlem)Kortnr"*/
                    /*+ ";+fKortNr|DECIMAL|>>>>>>>>9|kundekort_kortnr(KortNr)|Kortnr"*/
                    + ";RegistrertDato;!RegistrertTid;!RegistrertAv;!ETid;EDato;BrukerID"
                    + ",Kunde"
                      + ";ButikkNr|Butikknr@2"
                      + ";Navn|Navn(Kunde)|x(20)@4"
                      + ";EksterntKundeNr|Ekst.Knd@3"
                    + ",Medlem"
                      + ";AktivertFraWeb|WebInit"
                      + ";Fornavn|Fornavn(Medlem)|x(20)@7"
                      + ";Etternavn|Etternavn(Medlem)|x(20)@8"
                    + ",MedlemsKort"
                      + ";MedlemsNr|Medlemsnr@9"
                      + ";KortNr|Medl.kort|x(8)@6"
                      + ";!Innehaver|Innehaver (Medlem)"
/*                    ,"WHERE where KundeNr > 0" + */
                   ,"WHERE FALSE" + 
                             ",FIRST Kunde NO-LOCK OF Kundekort OUTER-JOIN" +
                             ",FIRST Medlem NO-LOCK where Medlem.KundeNr = Kunde.KundeNr OUTER-JOIN" +
                             ",FIRST MedlemsKort NO-LOCK where " + 
                               "MedlemsKort.MedlemsNr = Medlem.MedlemsNr and " + 
                               "MedlemsKort.InterntKKortId = KundeKort.InterntKKortId and " +  
                               "MedlemsKort.InterntKKortId > 0 OUTER-JOIN" 
                    ,"sort|KundeNr").             /* Initial sort column */

  DYNAMIC-FUNCTION("setAttribute",hBrowse,"calcfieldproc","kundekort_brwcalc.p"). 
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"nocolumnsearch","fKortNr"). 
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"basequery","WHERE KundeNr >= 0"). 
  hSearchField = DYNAMIC-FUNCTION("NewBrowseSearchField",SearchField:HANDLE,hBrowse,1).
  DYNAMIC-FUNCTION("CreateObjectLink",hSearchField,hBrowse).
  /*
  hFieldMap = DYNAMIC-FUNCTION("NewFieldMap",      /* A fieldmap object holds extra info for display and input fields (fill-ins) 
                                                       and their corresponding buffer columns return handle equals the buffer handle */
                    hBrowse:QUERY,
                    FRAME {&FRAME-NAME}:HANDLE,     /* Frame for the input/display fields (might not be the same frame as the browse) */
                    "KundeNr,KortNr",   /* Update columns in buffer */
                      "",                           /* Corresponding input fields (fill-in..). blank if equal to update columns */
                      "",        /* Additional buffer and displ.fields - not updateable*/
                      "",                           /* Corresponding fill-ins */
                    "").                            /* other input widgets and lookup buttons for update fill-ins */

  DYNAMIC-FUNCTION("CreateObjectLink",hFieldMap,hBrowse).
  */
  hToolbar = DYNAMIC-FUNCTION("NewToolBar",
                    rectToolBar:HANDLE,             /* Rectangle to define coordinates for toolbar */
                    "File",                         /* Corresponding menu label - no menu if blank */
                    "Save"
                    + ",delete,excel;Eksporter til E&xcel" /* Buttons / Menu items: action;label;tooltip;Method;image,action;label.. 
                                                       Any number of properties accepted (one ok - if predef. action) */
                    ,"maxborder").                  /* Misc - enable, maxborder.. */
  
  /*DYNAMIC-FUNCTION("CreateObjectLink",hFieldMap,hToolbar).*/
  DYNAMIC-FUNCTION("CreateObjectLink",hBrowse,hToolbar).

  DYNAMIC-FUNCTION("NewToolBar",
                    rectWinToolBar:HANDLE,          /* Rectangle to define coordinates for toolbar */
                    "File",                         /* Corresponding menu label - no menu if blank */
                    "close",
                    "right,enable").                      /* Misc - enable, maxborder.. */

  APPLY "RETURN" TO hSearchField.
  APPLY "value-changed" TO hBrowse.

END.

DYNAMIC-FUNCTION("setNoResizeY", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, "rectToolBar").
DYNAMIC-FUNCTION("setOrgWinSize", THIS-PROCEDURE:CURRENT-WINDOW,200,250,0,250).

DYNAMIC-FUNCTION("initTranslation",THIS-PROCEDURE:CURRENT-WINDOW).

THIS-PROCEDURE:CURRENT-WINDOW:HIDDEN = FALSE.

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
{&WINDOW-NAME}:MOVE-TO-TOP().
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
DEF VAR cWhere        AS CHAR NO-UNDO.
DEF VAR cPrescanKunde AS CHAR NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
  ASSIGN fltButikkNr.


  DYNAMIC-FUNCTION("setAttribute",hBrowse,"QueryFilter",''). 


  cWhere = buildFilter(cWhere,fltKundeNr:HANDLE,'KundeNr','EQ').      
/*   cWhere = cWhere + buildFilter(cWhere,fltNavn:HANDLE,'Navn','BEGINS').  */ /* <- queryFilter kan bare settes på primærtabellen (kundekort) */
  cWhere = cWhere + buildFilter(cWhere,fltKortNr:HANDLE,'KortNr','BEGINS').
  cWhere = cWhere + buildFilter(cWhere,fltFraUtgarDato:HANDLE,'UtgarDato','GE').
  cWhere = cWhere + buildFilter(cWhere,fltTilUtgarDato:HANDLE,'UtgarDato','LE').
  cWhere = cWhere + buildFilter(cWhere,fltInnehaver:HANDLE,'Innehaver','BEGINS').
/*   cWhere = cWhere + buildFilter(cWhere,fltButikkNr:HANDLE,'ButikkNr','EQ'). */

  DYNAMIC-FUNCTION("setAttribute",hBrowse,"QueryFilter",cWhere). 

  /* Denne virker ikke.. Ønsker å kombinere begge kriteriene. Navn og butikknr....
  
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"prescanQueryFilter",
                   (IF fltNavn:SCREEN-VALUE NE "" THEN
                      (IF INDEX(fltNavn:SCREEN-VALUE,"*") > 0 THEN
                         "Kunde WHERE Navn MATCHES '" + fltNavn:SCREEN-VALUE + "'"
                       ELSE IF fltNavn:SCREEN-VALUE NE "" THEN
                         ",EACH Kunde of KundeKort WHERE Navn BEGINS '" + fltNavn:SCREEN-VALUE + "'"
                       ELSE "")
                      + ",EACH KundeKort NO-LOCK OF Kunde"
                    ELSE "")
                   ).
  */

  /* Denne virker ut fra det jeg kan se. */
  /* Ja og den skriver over det som er satt over for navnesøk.. */
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"prescanQueryFilter",
                   (IF fltButikkNr NE 0 THEN
                       "Kunde WHERE ButikkNr = " + fltButikkNr:SCREEN-VALUE + ",EACH KundeKort NO-LOCK OF Kunde" 
                    ELSE "")
                 + (IF fltNavn:SCREEN-VALUE NE "" THEN
                     "|" /* <- Skilletegn hvis flere kriterier */
                   + (IF INDEX(fltNavn:SCREEN-VALUE,"*") > 0 THEN
                        "Kunde WHERE Navn MATCHES '" + fltNavn:SCREEN-VALUE + "'"
                      ELSE IF fltNavn:SCREEN-VALUE NE "" THEN
                        "Kunde WHERE Navn BEGINS '" + fltNavn:SCREEN-VALUE + "'"
                      ELSE "")
                      + ",EACH KundeKort NO-LOCK OF Kunde"
                    ELSE "")                     
                   ).
  ASSIGN
      fltFraUtgarDato:MODIFIED = FALSE 
      fltFraUtgarDato:MODIFIED = FALSE  
      fltKundeNr:MODIFIED      = FALSE  
      fltInnehaver:MODIFIED    = FALSE 
      fltButikkNr:MODIFIED     = FALSE
      .

END.

RUN SUPER.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

