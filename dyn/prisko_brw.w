&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: 

  Description:        JukeBox suppressed window template
                      This template is for use with a tabfolder or viewer object
                      to parent the suppressed window

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author:            brynjar@chemistry.no

  Created:           30.march.2008

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

DEF VAR bOk                 AS LOG    NO-UNDO.
DEF VAR ix                  AS INT    NO-UNDO.
DEF VAR hBrowse             AS HANDLE NO-UNDO.
DEF VAR hToolbar            AS HANDLE NO-UNDO.
DEF VAR hParent             AS HANDLE NO-UNDO.
DEF VAR hParentQueryObject  AS HANDLE NO-UNDO.

DEF VAR hPrisKoPris         AS HANDLE NO-UNDO.
DEF VAR hPrisKoAktiveresDato AS HANDLE NO-UNDO.
DEF VAR iReturn             AS INT    NO-UNDO.
DEF VAR hBtnPanel           AS HANDLE NO-UNDO.
DEF VAR hPanelFrame         AS HANDLE NO-UNDO.
DEF VAR hEndrTypeCol        AS HANDLE NO-UNDO.
DEF VAR hEndrTypeField      AS HANDLE NO-UNDO.
DEF VAR hFieldDB%           AS HANDLE NO-UNDO.
DEF VAR hFieldPris          AS HANDLE NO-UNDO.
DEF VAR hFieldVarekost      AS HANDLE NO-UNDO.
DEF VAR hFieldLokalPris     AS HANDLE NO-UNDO.
DEF VAR hFieldLokalVarekost AS HANDLE NO-UNDO.
DEF VAR hFieldLokalDB%      AS HANDLE NO-UNDO.
DEF VAR hColDB%             AS HANDLE NO-UNDO.
DEF VAR hColPris            AS HANDLE NO-UNDO.
DEF VAR hColVarekost        AS HANDLE NO-UNDO.
DEF VAR iBrukerType         AS INT    NO-UNDO.
DEF VAR hArtikkelkort       AS HANDLE NO-UNDO.
DEF VAR iAntProf            AS INT    NO-UNDO.
DEF VAR cSprak              AS CHAR        NO-UNDO.
DEF VAR cSprakLst           AS CHAR  INITIAL 'SE,SVE' NO-UNDO.

DEF VAR bHanSkal AS LOG INITIAL FALSE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS brwPrisko tbPrisko BtnPanel rsUtvalg 
&Scoped-Define DISPLAYED-OBJECTS rsUtvalg 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD AdjustBrowseColumns C-Win 
FUNCTION AdjustBrowseColumns RETURNS LOGICAL
  ( INPUT ihBrowse AS HANDLE,
    INPUT icBrowseName AS CHAR )  FORWARD.

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
  ( INPUT ihParentQueryObject AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE rsUtvalg AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Alle", 1,
"TOM i dag", 2,
"> i dag", 3
     SIZE 34 BY .95 NO-UNDO.

DEFINE RECTANGLE brwPrisko
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 79.6 BY 14.52.

DEFINE RECTANGLE BtnPanel
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 79 BY 1.1.

DEFINE RECTANGLE tbPrisko
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 9.8 BY .91.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     rsUtvalg AT ROW 1.33 COL 25 NO-LABEL
     brwPrisko AT ROW 4.1 COL 1.4
     tbPrisko AT ROW 1.24 COL 2
     BtnPanel AT ROW 2.52 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 81 BY 17.91.


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
         TITLE              = "Prisko browse"
         HEIGHT             = 17.76
         WIDTH              = 80.8
         MAX-HEIGHT         = 28.57
         MAX-WIDTH          = 160
         VIRTUAL-HEIGHT     = 28.57
         VIRTUAL-WIDTH      = 160
         SHOW-IN-TASKBAR    = no
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
   FRAME-NAME                                                           */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Prisko browse */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Prisko browse */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rsUtvalg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rsUtvalg C-Win
ON VALUE-CHANGED OF rsUtvalg IN FRAME DEFAULT-FRAME
DO:
  RUN InvokeMethod(hBrowse,"OpenQuery").
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
  /* The viewer or tabfolder object will see to that the Disable_UI is run
     and the container will make sure that all dynamic objects + resize settings are deleted
     also for the suppressed windows */
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

  &IF DEFINED(UIB_is_Running) NE 0 &THEN
    RUN InitializeObject.
  &ENDIF

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

{incl/supptrigg.i hBrowse}  /* <- To be able to capture keys (insert, delete..) on the browse */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AktiverOgPrintRecord C-Win 
PROCEDURE AktiverOgPrintRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cParam              AS CHAR NO-UNDO.

  IF CAN-DO(cSprakLst,cSprak) THEN DO:
      /* RUN JBoxBrowseSelectMsg.w ("Bekreft aktivering av priser. Artikler blir også flyttet til etikettkø", */
      RUN PriskoBekreftAktivering.w ("Bekräfta aktivering av priser. Artiklar blir också flyttade till etikettkö",
                                      hBrowse:NUM-SELECTED-ROWS,
                                      INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"recordCount")),
                                      hBrowse,
                                      iAntProf,
                                      OUTPUT iReturn,
                                      OUTPUT cParam).
  END.
  ELSE DO:
      /* RUN JBoxBrowseSelectMsg.w ("Bekreft aktivering av priser. Artikler blir også flyttet til etikettkø", */
      RUN PriskoBekreftAktivering.w ("Bekreft aktivering av priser. Artikler blir også flyttet til etikettkø",
                                      hBrowse:NUM-SELECTED-ROWS,
                                      INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"recordCount")),
                                      hBrowse,
                                      iAntProf,
                                      OUTPUT iReturn,
                                      OUTPUT cParam).
  END.



IF iReturn = 0 THEN RETURN.
ELSE IF iReturn = 1 THEN DO:
    DYNAMIC-FUNCTION("ProcessQuery",hBrowse,"prisko_etikett.p",cParam).
    DYNAMIC-FUNCTION("ProcessQuery",hBrowse,"prisko_aktiver.p",cParam).
END.
ELSE IF iReturn = 2 THEN DO:
    DYNAMIC-FUNCTION("ProcessSelectedRows",hBrowse,"prisko_etikett.p",cParam).
    DYNAMIC-FUNCTION("ProcessSelectedRows",hBrowse,"prisko_aktiver.p",cParam).
END.

RUN InvokeMethod(hBrowse,"OpenQuery").
RUN RefreshRecord IN hParent.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AktiverRecord C-Win 
PROCEDURE AktiverRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cParam AS CHAR NO-UNDO.

  IF CAN-DO(cSprakLst,cSprak) THEN DO:
      RUN PriskoBekreftAktivering.w ("Bekräfta aktivering av priser utan att etikett skrivs ut",
                                      hBrowse:NUM-SELECTED-ROWS,
                                      INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"recordCount")),
                                      hBrowse,
                                      iAntProf,
                                      OUTPUT iReturn,
                                      OUTPUT cParam).
  END.
  ELSE DO:
      RUN PriskoBekreftAktivering.w ("Bekreft aktivering av priser uten at etikett skrives ut",
                                      hBrowse:NUM-SELECTED-ROWS,
                                      INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"recordCount")),
                                      hBrowse,
                                      iAntProf,
                                      OUTPUT iReturn,
                                      OUTPUT cParam).
  END.


IF iReturn = 0 THEN RETURN.
ELSE IF iReturn = 1 THEN DO:
    DYNAMIC-FUNCTION("ProcessQuery",hBrowse,"prisko_simuler.p",cParam).
    DYNAMIC-FUNCTION("ProcessQuery",hBrowse,"prisko_aktiver.p",cParam).
END.
ELSE IF iReturn = 2 THEN DO:
    DYNAMIC-FUNCTION("ProcessSelectedRows",hBrowse,"prisko_simuler.p",cParam).
    DYNAMIC-FUNCTION("ProcessSelectedRows",hBrowse,"prisko_aktiver.p",cParam).
END.

RUN InvokeMethod(hBrowse,"OpenQuery").
RUN RefreshRecord IN hParent.

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
DEF VAR iArtBasBufferPos AS INT  NO-UNDO.
DEF VAR cBuffsAndFlds    AS CHAR NO-UNDO. 

  IF iBrukerType > 1 /* Butikkbruker */ THEN
  DO:
      IF NOT VALID-HANDLE(hArtikkelkort) THEN DO:
        RUN ArtBasVisTime.w   PERSIST SET hArtikkelkort (THIS-PROCEDURE,hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE).
      END.
      ELSE
        RUN ByttArtikkel IN hArtikkelkort (hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE).
  END.
  ELSE DO:
      IF NOT VALID-HANDLE(hArtikkelkort) THEN DO:
        RUN w-vartkor.w  PERSIST SET hArtikkelkort (DYNAMIC-FUNCTION("getRecId","ArtBas",hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("RowIdent2"):BUFFER-VALUE), "ENDRE," + STRING(THIS-PROCEDURE)).
      END.
      ELSE
        RUN ByttArtikkel IN hArtikkelkort (hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE).
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

DYNAMIC-FUNCTION("VisArtBilde" IN hParent,hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("Bildnr"):BUFFER-VALUE) NO-ERROR.

IF VALID-HANDLE(hArtikkelkort) THEN
  RUN ByttArtikkel IN hArtikkelkort (IF hBrowse:QUERY:GET-BUFFER-HANDLE(1):AVAIL 
                                       THEN hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE
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
  DISPLAY rsUtvalg 
      WITH FRAME DEFAULT-FRAME.
  ENABLE brwPrisko tbPrisko BtnPanel rsUtvalg 
      WITH FRAME DEFAULT-FRAME.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EtikettRecord C-Win 
PROCEDURE EtikettRecord :
/*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:                                                                                                                                            
        ------------------------------------------------------------------------------*/
  IF CAN-DO(cSprakLst,cSprak) THEN DO:
      RUN JBoxBrowseSelectMsg.w ("Skall posterna läggas till etikettkö",
                               hBrowse:NUM-SELECTED-ROWS,
                               INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"recordCount")),
                               OUTPUT iReturn).
  END.
  ELSE DO:
      RUN JBoxBrowseSelectMsg.w ("Skal posten(e) legges til etikettkø",
                               hBrowse:NUM-SELECTED-ROWS,
                               INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"recordCount")),
                               OUTPUT iReturn).
  END.

  IF iReturn = 0 THEN RETURN.
  ELSE IF iReturn = 1 THEN
    DYNAMIC-FUNCTION("ProcessQuery",hBrowse,"prisko_etikett.p","").
  ELSE IF iReturn = 2 THEN
    DYNAMIC-FUNCTION("ProcessSelectedRows",hBrowse,"prisko_etikett.p","").

  RUN InvokeMethod(hBrowse,"OpenQuery").
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ExtraLeaveBrowseFillIn C-Win 
PROCEDURE ExtraLeaveBrowseFillIn :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT  PARAM ihFillIn AS HANDLE NO-UNDO.
DEF INPUT  PARAM ihBuffer AS HANDLE NO-UNDO.
DEF OUTPUT PARAM obOk     AS LOG    NO-UNDO INIT YES.

DEF VAR iReposRow AS INT NO-UNDO.
IF VALID-HANDLE(hParentQueryObject) AND ihFillIn:MODIFIED THEN DO:
  DYNAMIC-FUNCTION("DoCommit",YES).
  hParentQueryObject:DESELECT-ROWS().
  hParentQueryObject:QUERY:GET-BUFFER-HANDLE(1):FIND-FIRST("WHERE ArtikkelNr = " + STRING(ihBuffer:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE)) NO-ERROR.
  IF hParentQueryObject:QUERY:GET-BUFFER-HANDLE(1):AVAIL THEN DO:
    iReposRow = hParentQueryObject:DOWN / 2.
    hParentQueryObject:SET-REPOSITIONED-ROW(iReposRow,"conditional").
    hParentQueryObject:QUERY:REPOSITION-TO-ROWID(hParentQueryObject:QUERY:GET-BUFFER-HANDLE(1):ROWID).
    DYNAMIC-FUNCTION("RefreshRowids",hParentQueryObject,
                     hParentQueryObject:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("RowIdent" + DYNAMIC-FUNCTION("getPrisKoBufferNum" IN hParent)):BUFFER-VALUE).
    DYNAMIC-FUNCTION("DispBrwOverlayWidgets",hParentQueryObject).
    hParentQueryObject:DESELECT-ROWS().
  END.
END.
ELSE DYNAMIC-FUNCTION("resetTransRecord").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitializeObject C-Win 
PROCEDURE InitializeObject :
/*------------------------------------------------------------------------------
  Purpose:     Create any dynamic objects, initialize drop-down lists etc
  Parameters:  <none>
  Notes:       The dynamic query object must be linked to the THIS-PROCEDURE
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
  DYNAMIC-FUNCTION("NewObject",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE,"procedure").

  SUBSCRIBE TO "ProfilnrChanged" IN hParent.
  SUBSCRIBE TO "InitStringTranslation" ANYWHERE.
  
  iAntProf    = DYNAMIC-FUNCTION("getRecordCount","Prisprofil","where true").
  iBrukerType = int(DYNAMIC-FUNCTION("getFieldValues","bruker","WHERE brukerid = '" + DYNAMIC-FUNCTION("getASuserId") + "'","BrukerType")).
  cSprak      = DYNAMIC-FUNCTION("getFieldValues","Bruker","WHERE brukerid = '" + DYNAMIC-FUNCTION("getASuserId") + "'","Lng").

  hBrowse = DYNAMIC-FUNCTION("NewBrowse"
          ,brwPrisko:HANDLE
          ,10000
          ,"multiple"
          ,"PrisKo"
           + ";ArtikkelNr|Art.nr"
           + ";+EndrType|CHARACTER|x(4)|endrType|Type"
           + ";Pris|Priskø-pris|>><>>><>>9.99"
           + ";DB%"
           + ";Varekost|Varekost"
           + ";AktiveresDato"
           + ";+AktivTid|CHAR|x(5)|jb_hhmm(AktiveresTid)|Akt.tid"
           + ";+etikettStat|CHARACTER|x(15)|etikettStatus|Etikettstatus"
           + ";+klarStat|CHARACTER|x(10)|klargjorStatus|Behandlingsstatus"
           + ";+LokalPris|DECIMAL|->>><>>9.99|lokal_pris(ROWID)|Lokal pris"
           + ";+LokalVarekost|DECIMAL|->>><>>9.99|lokal_varekost(ROWID)|Lokal varekost"
           + ";+LokalDB%|DECIMAL|->>9.99|lokal_db%(ROWID)|Lokal DB%"
           + ";Opphav"
           + ";!Aktivert"
           + ";!BrukerID"
           + ";!RegistrertDato"
           + ";Profilnr"
           + ";!AktiveresTid"
           + ";!Tilbud"
           + ";EndringsType"
           + ";!Type"
         + ",ArtBas"
           + ";Beskr@3"
           + ";LevKod|Lev.artnr@2"
           + ";LevFargKod|Lev.farge@4"
           + ";!Bildnr"
/*          + ",ArtPris"  */
/*            + ";DB%[1]" */
          ,"WHERE false"
         + ",FIRST ArtBas NO-LOCK OF PrisKo"
/*          + ",FIRST ArtPris NO-LOCK WHERE ArtPris.ArtikkelNr = Prisko.ArtikkelNr AND ArtPris.Profilnr = 1" */
          ,"").

  ASSIGN hEndrTypeCol        = DYNAMIC-FUNCTION("getBrowseColumn",hBrowse,"EndrType")
         hEndrTypeField      = hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("Type")
         hColDB%             = DYNAMIC-FUNCTION("getBrowseColumn",hBrowse,"DB%")
         hFieldDB%           = hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("DB%")
         hFieldPris          = hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("Pris")
         hFieldVarekost      = hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("Varekost")
         hColPris            = DYNAMIC-FUNCTION("getBrowseColumn",hBrowse,"Pris")
         hColVarekost        = DYNAMIC-FUNCTION("getBrowseColumn",hBrowse,"Varekost")
         hFieldLokalPris     = hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("LokalPris")
         hFieldLokalVarekost = hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("LokalVarekost")
         hFieldLokalDB%      = hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("LokalDB%")
         .

  DYNAMIC-FUNCTION("setAttribute",hBrowse,"getRecordCount","yes").
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"calcFieldProc","prisko_brwcalc.p").

  DYNAMIC-FUNCTION("CreateObjectLink",hBrowse,THIS-PROCEDURE).

  hPrisKoPris  = DYNAMIC-FUNCTION("NewBrowseFillIn",hBrowse,"Pris","Pris"
                 ,"","","","").
  DYNAMIC-FUNCTION("CreateOverlayLink",hBrowse,hPrisKoPris,"Pris").
  hPrisKoPris:HELP = "Registrer egen pris for artikkel".
  DYNAMIC-FUNCTION("setAttribute",hPrisKoPris,"refreshRow","yes").
  
  hPrisKoAktiveresDato  = DYNAMIC-FUNCTION("NewBrowseFillIn",hBrowse,"AktiveresDato","AktiveresDato"
                 ,"","","","").
  DYNAMIC-FUNCTION("CreateOverlayLink",hBrowse,hPrisKoAktiveresDato,"AktiveresDato").
  hPrisKoAktiveresDato:HELP = "Endre aktiveringsdato på priskøpost".
  DYNAMIC-FUNCTION("setAttribute",hPrisKoAktiveresDato,"refreshRow","yes").

  DYNAMIC-FUNCTION("setAttribute",hBrowse,"postUpdateProc","prisko_post_update.p").

  hToolbar = DYNAMIC-FUNCTION("NewToolBar"
           ,TbPrisko:HANDLE
           ,""
           ,"refresh,delete;Slett,browseConfig;Kolonneoppsett"
            + (IF iBrukerType <> 2 THEN ",excel" ELSE "")
            + ";Eksporter til E&xcel"
           ,"maxborder").

  DYNAMIC-FUNCTION("CreateObjectLink",hBrowse,hToolbar).

  DYNAMIC-FUNCTION("setNoMoveY", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE,"brwPrisko," + hBrowse:NAME).

  DYNAMIC-FUNCTION("setAttribute",hBrowse,"baseQuery","WHERE ProfilNr = " + DYNAMIC-FUNCTION("getProfilnr" IN hParent)).


END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitStringTranslation C-Win 
PROCEDURE InitStringTranslation :
/*------------------------------------------------------------------------------
                Purpose:                                                                                                                                          
                Notes:                                                                                                                                            
------------------------------------------------------------------------------*/
DEF INPUT PARAM        ihWindow    AS HANDLE NO-UNDO.
DEF INPUT-OUTPUT PARAM iocTypeList AS CHAR   NO-UNDO.

DEF VAR cAktiverOgPrintLbl  AS CHAR   NO-UNDO.
DEF VAR cAktiverLbl         AS CHAR   NO-UNDO.
DEF VAR cEtikettLbl         AS CHAR   NO-UNDO.
DEF VAR cAktiverOgPrintTT   AS CHAR   NO-UNDO.
DEF VAR cAktiverTT          AS CHAR   NO-UNDO.
DEF VAR cEtikettTT          AS CHAR   NO-UNDO.

IF ihWindow NE THIS-PROCEDURE:CURRENT-WINDOW THEN RETURN.

/* For translation: Init special translation type and corresponding values: */
IF NOT CAN-DO(iocTypeList,"BUTTON-AktiverOgPrint") THEN
  iocTypeList = iocTypeList + ",BUTTON-AktiverOgPrint".
IF NOT CAN-DO(iocTypeList,"BUTTON-Aktiver") THEN
  iocTypeList = iocTypeList + ",BUTTON-Aktiver".
IF NOT CAN-DO(iocTypeList,"BUTTON-Etikett") THEN
  iocTypeList = iocTypeList + ",BUTTON-Etikett".
IF NOT CAN-DO(iocTypeList,"TOOLTIP-AktiverOgPrint") THEN
  iocTypeList = iocTypeList + ",TOOLTIP-AktiverOgPrint".
IF NOT CAN-DO(iocTypeList,"TOOLTIP-Aktiver") THEN
  iocTypeList = iocTypeList + ",TOOLTIP-Aktiver".
IF NOT CAN-DO(iocTypeList,"TOOLTIP-Etikett") THEN
  iocTypeList = iocTypeList + ",TOOLTIP-Etikett".


ASSIGN cAktiverOgPrintLbl = DYNAMIC-FUNCTION("getStringTranslation",THIS-PROCEDURE:FILE-NAME,"BUTTON-AktiverOgPrint","Aktiver m/etikett")
       cAktiverLbl        = DYNAMIC-FUNCTION("getStringTranslation",THIS-PROCEDURE:FILE-NAME,"BUTTON-Aktiver","Aktiver")
       cEtikettLbl        = DYNAMIC-FUNCTION("getStringTranslation",THIS-PROCEDURE:FILE-NAME,"BUTTON-Etikett","Til etikettkø")
       cAktiverOgPrintTT  = DYNAMIC-FUNCTION("getStringTranslation",THIS-PROCEDURE:FILE-NAME,"TOOLTIP-AktiverOgPrint","Aktiver priser og send artikler til etikett-kø&")
       cAktiverTT         = DYNAMIC-FUNCTION("getStringTranslation",THIS-PROCEDURE:FILE-NAME,"TOOLTIP-Aktiver","Aktiver priser&")
       cEtikettTT         = DYNAMIC-FUNCTION("getStringTranslation",THIS-PROCEDURE:FILE-NAME,"TOOLTIP-Etikett","Send artikler til etikett-kø&")
       .

IF NOT VALID-HANDLE(hBtnPanel) THEN DO WITH FRAME Default-Frame:
  hBtnPanel = DYNAMIC-FUNCTION("NewPanel"
            ,BtnPanel:HANDLE
            ,"",
            "AktiverOgPrint;" + cAktiverOgPrintLbl + ";" + cAktiverOgPrintTT + ";;bmp\bullet_triangle_green.bmp"
          + ",Aktiver;" + cAktiverLbl + ";" + cAktiverTT + ";;bmp\bullet_triangle_blue.bmp"
          + ",Etikett;" + cEtikettLbl + ";" + cEtikettTT + ";;bmp\bullet_triangle_red.bmp" 
            ,120,23     
            ,"maxborder").
  DYNAMIC-FUNCTION("CreateObjectLink",hBtnPanel,hBrowse).
  hPanelFrame = DYNAMIC-FUNCTION("getPanelFrame" IN hBtnPanel).
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MoveToTop C-Win 
PROCEDURE MoveToTop :
/*------------------------------------------------------------------------------
  Purpose:     Mak
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
FRAME {&FRAME-NAME}:MOVE-TO-TOP().
IF VALID-HANDLE(hPanelFrame) THEN
  hPanelFrame:MOVE-TO-TOP() NO-ERROR.


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
DO WITH FRAME {&FRAME-NAME}:

  IF bHanSkal = FALSE THEN
      ASSIGN
        bHanSkal = TRUE
        rsUtvalg:SCREEN-VALUE = '2'.

  ASSIGN rsUtvalg.

  DYNAMIC-FUNCTION("setAttribute",hBrowse,"queryFilter",
                   (IF rsUtvalg = 1 THEN ""
                    ELSE IF rsUtvalg = 2 THEN "WHERE PrisKo.AktiveresDato LE DATE('" + STRING(TODAY) + "')"
                    ELSE "WHERE PrisKo.AktiveresDato GT DATE('" + STRING(TODAY) + "')")
                    ).

  RUN SUPER.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProfilnrChanged C-Win 
PROCEDURE ProfilnrChanged :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM icProfilnr AS CHAR NO-UNDO.

DYNAMIC-FUNCTION("setAttribute",hBrowse,"calcParamLokalPris",icProfilnr).
DYNAMIC-FUNCTION("setAttribute",hBrowse,"calcParamLokalVarekost",icProfilnr).
DYNAMIC-FUNCTION("setAttribute",hBrowse,"calcParamLokalDB%",icProfilnr).

DYNAMIC-FUNCTION("setAttribute",hBrowse,"baseQuery","WHERE ProfilNr = " + icProfilnr).
RUN InvokeMethod(hBrowse,"OpenQuery").

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
CASE INTEGER(hEndrTypeField:BUFFER-VALUE):
  WHEN 2 OR WHEN 3 THEN hEndrTypeCol:BGCOLOR = 12.
  WHEN 5 OR WHEN 6 THEN hEndrTypeCol:BGCOLOR = 4.
  WHEN 4 THEN hEndrTypeCol:BGCOLOR = 13.
  OTHERWISE hEndrTypeCol:BGCOLOR = ?.
END CASE.

IF hFieldLokalDB%:BUFFER-VALUE > hFieldDB%:BUFFER-VALUE THEN DO:
  IF hFieldLokalPris:BUFFER-VALUE NE hFieldPris:BUFFER-VALUE THEN
    hColPris:BGCOLOR     = 17.
  ELSE 
    hColPris:BGCOLOR     = ?.

  IF hFieldLokalVarekost:BUFFER-VALUE NE hFieldVarekost:BUFFER-VALUE THEN
    hColVarekost:BGCOLOR = 17.
  ELSE
    hColVarekost:BGCOLOR = ?.
END.

ELSE IF hFieldPris:BUFFER-VALUE < hFieldDB%:BUFFER-VALUE THEN DO:
  IF hFieldLokalPris:BUFFER-VALUE NE hFieldPris:BUFFER-VALUE THEN
    hColPris:BGCOLOR     = 18.
  ELSE 
    hColPris:BGCOLOR     = ?.

  IF hFieldLokalVarekost:BUFFER-VALUE NE hFieldVarekost:BUFFER-VALUE THEN
    hColVarekost:BGCOLOR = 18.
  ELSE
    hColVarekost:BGCOLOR = ?.    
END.
ELSE 
  ASSIGN hColPris:BGCOLOR     = ?
         hColVarekost:BGCOLOR = ?.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION AdjustBrowseColumns C-Win 
FUNCTION AdjustBrowseColumns RETURNS LOGICAL
  ( INPUT ihBrowse AS HANDLE,
    INPUT icBrowseName AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
ASSIGN ihBrowse:GET-BROWSE-COLUMN(1):WIDTH-PIXELS = 77
       ihBrowse:GET-BROWSE-COLUMN(2):WIDTH-PIXELS = 55
       ihBrowse:GET-BROWSE-COLUMN(3):WIDTH-PIXELS = 80
       ihBrowse:GET-BROWSE-COLUMN(4):WIDTH-PIXELS = 55 
       ihBrowse:GET-BROWSE-COLUMN(5):WIDTH-PIXELS = 25 
       ihBrowse:GET-BROWSE-COLUMN(6):WIDTH-PIXELS = 60 
       ihBrowse:GET-BROWSE-COLUMN(7):WIDTH-PIXELS = 60 
       .
  
RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getFrameHandle C-Win 
FUNCTION getFrameHandle RETURNS HANDLE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose: Make the frame of the child procedure known to the tabfolder or viewer object
    Notes: The procedure is mandatory
------------------------------------------------------------------------------*/

  RETURN FRAME {&FRAME-NAME}:HANDLE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION InitializeResize C-Win 
FUNCTION InitializeResize RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose: This function is invoked automatically from the viewer or tabfolder object before
           InitializeObject and also before the suppressed window is resized to fit the size of the 
           graphics placeholder.
  Notes:   Note that if the suppressed window contains a BtnPanel or toolbar object
           the placeholders for these must have resize settings here since the objects themselves have not been 
           initialized. (When a BtnPanel or tabfolder is created the resize is automatically taken care of).
           
           This function is not mandatory but is called if it exists
------------------------------------------------------------------------------*/

/* DYNAMIC-FUNCTION("setNoResizeX", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE,"BtnPanel").  */
DYNAMIC-FUNCTION("setNoResizeY", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE,"BtnPanel").
DYNAMIC-FUNCTION("setNoMoveY", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE,"brwPrisko").
  
RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setParent C-Win 
FUNCTION setParent RETURNS LOGICAL
  ( INPUT ihParent AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose: Set the handle to the parent procedure for the suppressed window
    Notes: This function is not mandatory but is called if it exists
------------------------------------------------------------------------------*/
hParent = ihParent.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setQuery C-Win 
FUNCTION setQuery RETURNS LOGICAL
  ( INPUT ihParentQueryObject AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose: Set the handle of the parent (or oneToOne navigation query/browse) so
           it is available for the child object 
    Notes: This function is not mandatory but is called if it exists 
------------------------------------------------------------------------------*/
hParentQueryObject = ihParentQueryObject.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

