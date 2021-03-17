&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
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
DEF INPUT PARAM ihParent AS HANDLE NO-UNDO.

/* Local Variable Definitions ---                                       */

DEF VAR ix               AS INT NO-UNDO.
DEF VAR bOk              AS LOG NO-UNDO.

DEF VAR hBrowse          AS HANDLE NO-UNDO.
DEF VAR hBuffer          AS HANDLE NO-UNDO.
DEF VAR hFillInStrekkode AS HANDLE NO-UNDO.
DEF VAR hUpdTB           AS HANDLE NO-UNDO.
DEF VAR hWinTB           AS HANDLE NO-UNDO.
DEF VAR hNewBuff         AS HANDLE NO-UNDO.
DEF VAR hParent          AS HANDLE NO-UNDO.
DEF VAR bModStrekkode    AS LOG    NO-UNDO.
DEF VAR cStorl           AS CHAR   NO-UNDO.
DEF VAR fArtikkelNr      AS DEC    NO-UNDO.

DEF VAR iAnt AS INT NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rectBrwStrekkode rectWinTB rectTBupd ~
rsEAN-UPC tbPrevNextMode fiTotAntArt fiAntUtenKode btnSave 
&Scoped-Define DISPLAYED-OBJECTS rsEAN-UPC tbPrevNextMode fiTotAntArt ~
fiAntUtenKode 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ViewStrekkodeStat C-Win 
FUNCTION ViewStrekkodeStat RETURNS LOGICAL
  ( INPUT iiTotAnt      AS INT,
    INPUT iiAntUtenKode AS INT )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ViewTbPrevNextMode C-Win 
FUNCTION ViewTbPrevNextMode RETURNS LOGICAL
  ( INPUT ibView AS LOG )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnSave 
     LABEL "&Lagre" 
     SIZE 27 BY 1.14.

DEFINE VARIABLE fiAntUtenKode AS INTEGER FORMAT ">>>9":U INITIAL 0 
     LABEL "/" 
     VIEW-AS FILL-IN 
     SIZE 7.2 BY .95 NO-UNDO.

DEFINE VARIABLE fiTotAntArt AS INTEGER FORMAT ">>>9":U INITIAL 0 
     LABEL "Totalt antall artikler/antall uten gyldig kode" 
     VIEW-AS FILL-IN 
     SIZE 7 BY .95 NO-UNDO.

DEFINE VARIABLE rsEAN-UPC AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "EAN", 1,
"UPC", 2
     SIZE 21.2 BY .95 NO-UNDO.

DEFINE RECTANGLE rectBrwStrekkode
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 126 BY 12.86.

DEFINE RECTANGLE rectTBupd
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 9 BY .95.

DEFINE RECTANGLE rectWinTB
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 17 BY 1.19.

DEFINE VARIABLE tbPrevNextMode AS LOGICAL INITIAL yes 
     LABEL "Neste/forrige: Gå til artikkel uten gyldig kode" 
     VIEW-AS TOGGLE-BOX
     SIZE 45.8 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     rsEAN-UPC AT ROW 1.24 COL 16.8 NO-LABEL
     tbPrevNextMode AT ROW 1.33 COL 37.6
     fiTotAntArt AT ROW 2.38 COL 77 COLON-ALIGNED
     fiAntUtenKode AT ROW 2.38 COL 86.8 COLON-ALIGNED
     btnSave AT ROW 16.43 COL 99.8
     rectBrwStrekkode AT ROW 3.38 COL 2
     rectWinTB AT ROW 1.24 COL 110
     rectTBupd AT ROW 1.24 COL 2.6
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 127.2 BY 16.71.


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
         TITLE              = "Strekkoder"
         HEIGHT             = 16.71
         WIDTH              = 127.2
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
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB C-Win 
/* ************************* Included-Libraries *********************** */

{dproclibstart.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
ASSIGN 
       fiAntUtenKode:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       fiTotAntArt:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

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
ON END-ERROR OF C-Win /* Strekkoder */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Strekkoder */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-RESIZED OF C-Win /* Strekkoder */
DO:
  DYNAMIC-FUNCTION("setWidgetResize",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE:CURRENT-WINDOW,"Resize","").  
  
  DEF VAR hColumn AS HANDLE NO-UNDO.
  hColumn = hBrowse:GET-BROWSE-COLUMN(3).
  APPLY "end-resize" TO hColumn.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSave
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSave C-Win
ON CHOOSE OF btnSave IN FRAME DEFAULT-FRAME /* Lagre */
DO:
  IF VALID-HANDLE(hFillInStrekkode) AND 
     hFillInStrekkode:MODIFIED AND 
     NOT hFillInStrekkode:HIDDEN AND 
     hFillInStrekkode:SCREEN-VALUE NE "" THEN 
  DO:
    bOK = TRUE.
    APPLY "return" TO hFillInStrekkode.
    IF NOT bOK THEN DO:
      APPLY "entry" TO hFillInStrekkode.
      RETURN NO-APPLY.
    END.
  END.

  IF NOT DYNAMIC-FUNCTION("runproc","strekkode_lagre.p","",hBuffer:TABLE-HANDLE) THEN
    DYNAMIC-FUNCTION("DoMessage",0,1,DYNAMIC-FUNCTION("GetTransactionMessage"),"Feil","").
  ELSE DO: 
    bModStrekkode = NO.
    RUN setStrekkode IN hParent (hBuffer:TABLE-HANDLE) NO-ERROR.
    RUN RefreshQuery IN hParent NO-ERROR.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rsEAN-UPC
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rsEAN-UPC C-Win
ON VALUE-CHANGED OF rsEAN-UPC IN FRAME DEFAULT-FRAME
DO:
  ASSIGN rsEAN-UPC.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tbPrevNextMode
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tbPrevNextMode C-Win
ON VALUE-CHANGED OF tbPrevNextMode IN FRAME DEFAULT-FRAME /* Neste/forrige: Gå til artikkel uten gyldig kode */
DO:
  DYNAMIC-FUNCTION("setPrevNextStrekkodeMode" IN hParent,tbPrevNextMode:CHECKED) NO-ERROR.
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
  DYNAMIC-FUNCTION("SetCleanUpResize",THIS-PROCEDURE:CURRENT-WINDOW).
  PUBLISH "InvalidateHandle" (THIS-PROCEDURE).
  DELETE OBJECT hNewBuff NO-ERROR.
  RUN disable_UI.
END.

ON 'alt-l':U OF FRAME {&FRAME-NAME} ANYWHERE
  APPLY "choose" TO btnSave.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.

  hParent = SOURCE-PROCEDURE.

  RUN InitWindow.
  THIS-PROCEDURE:CURRENT-WINDOW:HIDDEN = FALSE.

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BeforeNavBrowseFillIn C-Win 
PROCEDURE BeforeNavBrowseFillIn :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:     Denne rutinen er lagt inn for å ungå dobbelt linjeskift i browse.  
------------------------------------------------------------------------------*/

DEF INPUT  PARAM ihFillIn AS HANDLE NO-UNDO.
DEF INPUT  PARAM ihBuffer AS HANDLE NO-UNDO.
DEF OUTPUT PARAM obOk     AS LOG NO-UNDO INIT TRUE.

iAnt = iant + 1.
IF iAnt = 2 THEN
DO:
    iAnt = 0.
    obOk = FALSE.
END.

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
DEF VAR rRepos AS ROWID NO-UNDO.


IF hBuffer:AVAIL THEN DO:
  hNewBuff:BUFFER-CREATE().
  rRepos = hNewBuff:ROWID.
  hNewBuff:BUFFER-COPY(hBuffer,"kode").
  hNewBuff:BUFFER-FIELD("bBlank"):BUFFER-VALUE = TRUE.
  hBrowse:QUERY:QUERY-OPEN().
  hBrowse:SET-REPOSITIONED-ROW(hBrowse:DOWN,"conditional").
  hBrowse:QUERY:REPOSITION-TO-ROWID(rRepos).
  APPLY "value-changed" TO hBrowse.
END.

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
IF hBuffer:AVAIL THEN DO:
  IF hBuffer:BUFFER-FIELD("bBlank"):BUFFER-VALUE AND
     DYNAMIC-FUNCTION("getLinkedObject",hBrowse,"browseoverlay","from") = ? THEN DO:
    hFillInStrekkode = DYNAMIC-FUNCTION("NewBrowseFillIn",
                      hBrowse,
                      "Kode",
                      "Kode",
                      "","","","").
    DYNAMIC-FUNCTION("CreateOverlayLink",hBrowse,hFillInStrekkode,"Kode").
  END.
  ELSE IF NOT hBuffer:BUFFER-FIELD("bBlank"):BUFFER-VALUE AND
     DYNAMIC-FUNCTION("getLinkedObject",hBrowse,"browseoverlay","from") NE ? THEN DO:
    DYNAMIC-FUNCTION("DeleteObjectLink",hFillInStrekkode,hBrowse).
    hFillInStrekkode:HIDDEN = TRUE.
  END.
END.
ELSE IF VALID-HANDLE(hFillInStrekkode) THEN hFillInStrekkode:HIDDEN = TRUE.

DYNAMIC-FUNCTION("setCurrentObject",hBrowse).

RUN SUPER.

IF DYNAMIC-FUNCTION("getLinkedObject",hBrowse,"browseoverlay","from") = ? THEN
  APPLY "entry" TO hBrowse.
ELSE IF VALID-HANDLE(hFillInStrekkode) AND NOT hFillInStrekkode:HIDDEN THEN
  APPLY "entry" TO hFillInStrekkode.

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
  DISPLAY rsEAN-UPC tbPrevNextMode fiTotAntArt fiAntUtenKode 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE rectBrwStrekkode rectWinTB rectTBupd rsEAN-UPC tbPrevNextMode 
         fiTotAntArt fiAntUtenKode btnSave 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitWindow C-Win 
PROCEDURE InitWindow :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR iCountNew AS INT  NO-UNDO.
DEF VAR cTekst    AS CHAR NO-UNDO.

ASSIGN cStorl      = DYNAMIC-FUNCTION("getStorl" IN ihParent)
       fArtikkelNr = DYNAMIC-FUNCTION("getArtNr" IN ihParent)
       .

DO WITH FRAME {&FRAME-NAME}:
  ASSIGN tbPrevNextMode:HIDDEN = YES
         fiTotAntArt:HIDDEN    = YES
         fiAntUtenKode:HIDDEN  = YES.

  hBrowse = DYNAMIC-FUNCTION("NewBrowse",
                  rectBrwStrekkode:HANDLE,
                  100,
                  "",
                  "StrKonv"
                  + ";+Storl|CHARACTER|x(4)|strkonv_storl.p(StrKode)|Størrelse"
                  + ";+!bBlank|LOGICAL|yes/no"
               + ",Strekkode"
                  + ";!StrKode"
                  + ";KodeType"
                  + ";Kode"
                  + ";!ArtikkelNr"
               + ",ArtBas"
                  + ";Beskr@2"
                  + ";LevFargKod@3"
                  + ";LevKod@1"
                  ,
                 "WHERE CAN-DO('" + cStorl + "',TRIM(StrKonv.Storl))" +
                  ",EACH Strekkode WHERE Strekkode.ArtikkelNr = DEC('" + STRING(fArtikkelNr) + "')" +
                  "  AND Strekkode.StrKode = StrKonv.StrKode" +
                  "  AND NOT Strekkode.Kode BEGINS '02'" +
                  "  AND Strekkode.Kodetype = 1 USE-INDEX Artikkel"
               + ",FIRST ArtBas OF StrekKode NO-LOCK"
                  ,
                  "SORT|StrKode").
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"uselocaldata","yes").
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"querywhere","where true").
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"queryjoin","").
  DYNAMIC-FUNCTION("SetCurrentSourceProc",THIS-PROCEDURE).
  
  hBuffer = hBrowse:QUERY:GET-BUFFER-HANDLE(1).

  CREATE BUFFER hNewBuff FOR TABLE hBuffer.
  
  DO ix = 1 TO NUM-ENTRIES(cStorl):
    bOk = hBuffer:FIND-FIRST("WHERE TRIM(Storl) = '" + TRIM(ENTRY(ix,cStorl)) + "'") NO-ERROR.
    IF NOT bOK THEN DO:
      hBuffer:BUFFER-CREATE().
      ASSIGN hBuffer:BUFFER-FIELD("Storl"):BUFFER-VALUE   = ENTRY(ix,cStorl)
             hBuffer:BUFFER-FIELD("StrKode"):BUFFER-VALUE = 
                INT(DYNAMIC-FUNCTION("getFieldValues","StrKonv","WHERE TRIM(Storl) = '" + ENTRY(ix,cStorl) + "'","StrKode"))
             hBuffer:BUFFER-FIELD("bBlank"):BUFFER-VALUE = TRUE
             hBuffer:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE = fArtikkelNr
             hBuffer:BUFFER-FIELD("Kodetype"):BUFFER-VALUE = 1
             hBuffer:BUFFER-FIELD("Beskr"):BUFFER-VALUE = 
                           DYNAMIC-FUNCTION("getFieldValues","ArtBas","WHERE ArtikkelNr = " + STRING(fArtikkelNr),"Beskr")
             hBuffer:BUFFER-FIELD("LevKod"):BUFFER-VALUE = 
                           DYNAMIC-FUNCTION("getFieldValues","ArtBas","WHERE ArtikkelNr = " + STRING(fArtikkelNr),"LevKod")
             hBuffer:BUFFER-FIELD("LevFargKod"):BUFFER-VALUE = 
                           DYNAMIC-FUNCTION("getFieldValues","ArtBas","WHERE ArtikkelNr = " + STRING(fArtikkelNr),"LevFargKod")
             iCountNew   = iCountNew + 1
             NO-ERROR.
    END.
  END.

  DYNAMIC-FUNCTION("setAttribute",hBrowse,"totalcount",STRING(INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"totalcount")) + iCountNew)).

  hUpdTB = DYNAMIC-FUNCTION("NewToolbar",
                 rectTBupd:HANDLE,
                 "Fil",
                 "Copy;Registrer ny (tilleggskode)"
               + ",Prev;Forrige artikkel¤enable"
               + ",Next;Neste artikkel¤enable"
                ,"maxborder").
  DYNAMIC-FUNCTION("CreateObjectLink",hUpdTB,hBrowse).

  hWinTB = DYNAMIC-FUNCTION("NewToolbar",
                 rectWinTB:HANDLE,
                 "Fil",
                 "close;Avslutt",
                 "right,enable").

  DYNAMIC-FUNCTION("setNoResizeY", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, "rectTBupd").
  DYNAMIC-FUNCTION("setOrgWinSize", THIS-PROCEDURE:CURRENT-WINDOW,300,150,350,200).

  DYNAMIC-FUNCTION("setCurrentObject",hBrowse).
  RUN OpenQuery.
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
THIS-PROCEDURE:CURRENT-WINDOW:WINDOW-STATE = 3.
THIS-PROCEDURE:CURRENT-WINDOW:MOVE-TO-TOP().
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
DEF INPUT  PARAM ihFillIn AS HANDLE NO-UNDO.
DEF INPUT  PARAM ihBuffer AS HANDLE NO-UNDO.
DEF OUTPUT PARAM obOk     AS LOG NO-UNDO INIT TRUE.

DEF VAR lokOk     AS LOG NO-UNDO.
DEF VAR cArtikkel AS CHAR NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
  IF LENGTH(ihFillIn:SCREEN-VALUE) NE 12 AND rsEAN-UPC = 2 THEN 
    DYNAMIC-FUNCTION("DoMessage",0,0,"Ugyldig UPC kode (skal ha 12 siffer)","Feil","").
  ELSE IF LENGTH(ihFillIn:SCREEN-VALUE) = 12 AND rsEAN-UPC = 1 THEN 
    ihFillIn:SCREEN-VALUE = "0" + ihFillIn:SCREEN-VALUE.
  ELSE IF LENGTH(ihFillIn:SCREEN-VALUE) = 8 AND rsEAN-UPC = 1 THEN 
    ihFillIn:SCREEN-VALUE = "00000" + ihFillIn:SCREEN-VALUE.
    
  IF ihFillIn:SCREEN-VALUE NE DYNAMIC-FUNCTION("fixChkEAN" IN h_dproclib,SUBSTR(ihFillIn:SCREEN-VALUE,1,12)) THEN DO:
    DYNAMIC-FUNCTION("DoMessage",0,0,"Ugyldig strekkode","Feil","").
    ASSIGN obOk = FALSE
           bOK  = FALSE.
    RETURN.
  END.
  cArtikkel = DYNAMIC-FUNCTION("getFieldValues","Strekkode","WHERE Kode = '" + ihFillIn:SCREEN-VALUE + "'","Kode,ArtikkelNr").
  IF cArtikkel NE ? THEN 
  DO:
    IF hBuffer:AVAIL THEN
    DO:
        lokOk = FALSE.
        MESSAGE 'Strekkoden ligger på en annen artikkel. Vil du sanere?' + CHR(10) + CHR(10) + 
                'Sanering rydder og flytter all informasjon (Salg,  lager m.m.) fra gammelt til nytt artikkelkort.' + CHR(10) + CHR(10) +
                'Hvis du ikke sanerer, må antall levert settes til 0 på pakkseddelen og varen leveres inn via direkte varemottak.'
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE 'Behandling av feilkoblet strekkode' UPDATE lokOk.
        IF lokOk THEN
        DO:
            /* Sanerer artikkelen. */
            RUN SanerArtikkel.p (ENTRY(2,cArtikkel,'|'),STRING(hBuffer:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE),OUTPUT lokOk).
            IF lokOk = FALSE THEN
            DO:
                ASSIGN obOk = FALSE
                       bOK  = FALSE
                       ihFillIn:SCREEN-VALUE = ''.
                RETURN.
            END.
            RUN bibl_flytt_lager_str.p (hBuffer:BUFFER-FIELD("Kode"):BUFFER-VALUE, INT(hBuffer:BUFFER-FIELD("StrKode"):BUFFER-VALUE)).
            APPLY 'CHOOSE' TO btnSave.
        END.
    END.
    ELSE DO: 
        DYNAMIC-FUNCTION("DoMessage",0,0,"Strekkode er allerede registrert for artikkelnr " + ENTRY(2,cArtikkel,"|") 
                         ,"Feil","").
        ASSIGN obOk = FALSE
               bOK  = FALSE.
    END.
    RETURN.
  END.
  
  ASSIGN ihFillIn:MODIFIED = FALSE
         bModStrekkode     = YES.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NextRecord C-Win 
PROCEDURE NextRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF bModStrekkode THEN DO:
  IF DYNAMIC-FUNCTION("DoMessage",0,4,"Lagre endringer?","","") = 6 THEN DO:
    APPLY "choose" TO btnSave IN FRAME {&FRAME-NAME}.
    IF bModStrekkode THEN RETURN.
  END.
END.

RUN PrevNext IN hParent ("next") NO-ERROR.

IF NOT ERROR-STATUS:ERROR AND RETURN-VALUE NE "Error" THEN
  RUN StartQuery.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PrevRecord C-Win 
PROCEDURE PrevRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF bModStrekkode THEN DO:
  IF DYNAMIC-FUNCTION("DoMessage",0,4,"Lagre endringer?","","") = 6 THEN DO:
    APPLY "choose" TO btnSave IN FRAME {&FRAME-NAME}.
    IF bModStrekkode THEN RETURN.
  END.
END.

RUN PrevNext IN hParent ("prev") NO-ERROR.

IF NOT ERROR-STATUS:ERROR AND RETURN-VALUE NE "Error" THEN
  RUN StartQuery.

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
DEF VAR iCountNew AS INT NO-UNDO.

ASSIGN cStorl         = DYNAMIC-FUNCTION("getStorl" IN ihParent)
       fArtikkelNr    = DYNAMIC-FUNCTION("getArtNr" IN ihParent)
       bModStrekkode  = NO.

DYNAMIC-FUNCTION("setAttribute",hBrowse,"uselocaldata","").

DYNAMIC-FUNCTION("setAttribute",hBrowse,"basequery","WHERE CAN-DO('" + cStorl + "',TRIM(StrKonv.Storl))").
DYNAMIC-FUNCTION("setAttribute",hBrowse,"queryjoin",
                 ",EACH Strekkode WHERE Strekkode.ArtikkelNr = DEC('" + STRING(fArtikkelNr) + "')" +
                 "  AND Strekkode.StrKode = StrKonv.StrKode" +
                 "  AND NOT Strekkode.Kode BEGINS '02'" +
                 "  AND Strekkode.Kodetype = 1 USE-INDEX Artikkel"
               + ",FIRST ArtBas OF StrekKode NO-LOCK"
                 ).

RUN InvokeMethod(hBrowse,"OpenQuery").

DYNAMIC-FUNCTION("setAttribute",hBrowse,"uselocaldata","yes").

DO ix = 1 TO NUM-ENTRIES(cStorl):
  bOk = hBuffer:FIND-FIRST("WHERE TRIM(Storl) = '" + TRIM(ENTRY(ix,cStorl)) + "'") NO-ERROR.
  IF NOT bOK THEN DO:
    hBuffer:BUFFER-CREATE().
    ASSIGN hBuffer:BUFFER-FIELD("Storl"):BUFFER-VALUE   = ENTRY(ix,cStorl)
           hBuffer:BUFFER-FIELD("StrKode"):BUFFER-VALUE = 
              INT(DYNAMIC-FUNCTION("getFieldValues","StrKonv","WHERE TRIM(Storl) = '" + ENTRY(ix,cStorl) + "'","StrKode"))
           hBuffer:BUFFER-FIELD("bBlank"):BUFFER-VALUE = TRUE
           hBuffer:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE = fArtikkelNr
           hBuffer:BUFFER-FIELD("Kodetype"):BUFFER-VALUE = 1
           hBuffer:BUFFER-FIELD("Beskr"):BUFFER-VALUE = 
                         DYNAMIC-FUNCTION("getFieldValues","ArtBas","WHERE ArtikkelNr = " + STRING(fArtikkelNr),"Beskr")
           hBuffer:BUFFER-FIELD("LevKod"):BUFFER-VALUE = 
                         DYNAMIC-FUNCTION("getFieldValues","ArtBas","WHERE ArtikkelNr = " + STRING(fArtikkelNr),"LevKod")
           hBuffer:BUFFER-FIELD("LevFargKod"):BUFFER-VALUE = 
                         DYNAMIC-FUNCTION("getFieldValues","ArtBas","WHERE ArtikkelNr = " + STRING(fArtikkelNr),"LevFargKod")
           iCountNew   = iCountNew + 1
           NO-ERROR.
  END.
END.

RUN InvokeMethod(hBrowse,"OpenQuery").

DYNAMIC-FUNCTION("setAttribute",hBrowse,"totalcount",STRING(INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"totalcount")) + iCountNew)).



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ViewStrekkodeStat C-Win 
FUNCTION ViewStrekkodeStat RETURNS LOGICAL
  ( INPUT iiTotAnt      AS INT,
    INPUT iiAntUtenKode AS INT ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
ASSIGN fiAntUtenKode:HIDDEN IN FRAME {&FRAME-NAME} = NO
       fiTotAntArt:HIDDEN = NO
       fiTotAntArt:SCREEN-VALUE = STRING(iiTotAnt)
       fiAntUtenKode:SCREEN-VALUE = STRING(iiAntUtenKode)
       .

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ViewTbPrevNextMode C-Win 
FUNCTION ViewTbPrevNextMode RETURNS LOGICAL
  ( INPUT ibView AS LOG ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
TbPrevNextMode:HIDDEN IN FRAME {&FRAME-NAME} = NOT ibView.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

