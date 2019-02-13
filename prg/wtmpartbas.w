&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wWin 
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrwin.w - ADM SmartWindow Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  History: New V9 Version - January 15, 1998
          
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AB.              */
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
DEFINE VARIABLE hJmfRutine    AS HANDLE     NO-UNDO.
DEFINE VARIABLE hArtikkelkort AS HANDLE     NO-UNDO.

DEF VAR glUtvalg            AS LOG    NO-UNDO.
DEF VAR cHKInst             AS CHAR   NO-UNDO.

DEF VAR h_wartbasutvalg     AS HANDLE NO-UNDO.
DEF VAR hBrowse             AS HANDLE NO-UNDO.
DEF VAR ix                  AS INT    NO-UNDO.
DEF VAR bOk                 AS LOG    NO-UNDO.
DEF VAR iReturn             AS INT    NO-UNDO.

DEF VAR hKampanje           AS HANDLE NO-UNDO.
DEF VAR hRapportGen         AS HANDLE NO-UNDO.
DEF VAR hRapportPlus        AS HANDLE NO-UNDO.
DEF VAR hKampanjeHodeViewer AS HANDLE NO-UNDO.
DEF VAR hTelleHode          AS HANDLE NO-UNDO.
DEF VAR hVareBokHode        AS HANDLE NO-UNDO.
DEF VAR hVareBehHode        AS HANDLE NO-UNDO.
DEF VAR cCurrWhere          AS CHAR   NO-UNDO.
DEF VAR bTellingIsMaster    AS LOG    NO-UNDO.
DEF VAR bKampanjeIsMaster   AS LOG    NO-UNDO.
DEF VAR bVareBokIsMaster    AS LOG    NO-UNDO.
DEF VAR bVareBehIsMaster    AS LOG    NO-UNDO.
DEF VAR hwbildeimport       AS HANDLE NO-UNDO.
DEF VAR cValues             AS CHAR   NO-UNDO.
DEFINE VARIABLE cTekst      AS CHARACTER NO-UNDO.
DEFINE VARIABLE bKjede      AS LOG NO-UNDO.
DEFINE VARIABLE bGjFakt     AS LOG NO-UNDO.
DEFINE VARIABLE cEloggtyp AS CHARACTER   NO-UNDO.

DEF VAR cFilKAtalog AS CHAR NO-UNDO.
DEFINE VARIABLE hEtikettVindu AS HANDLE     NO-UNDO.
DEF BUFFER bArtBas FOR ArtBas.
DEF TEMP-TABLE tmpChild
  FIELD wChild AS HANDLE.

DEF TEMP-TABLE Etikett NO-UNDO
    FIELD Artikkelnr AS DECI
    FIELD EAN        AS CHAR.

{tmp2artbasdef.i &NEW=NEW}
{runlib.i} /* Starter procedurebibloteket. */

{syspara.i 2 4 30 cTekst}
IF CAN-DO('1,true,yes,ja,j,y',cTekst) 
THEN bKjede = TRUE.
ELSE bKjede = FALSE.
{syspara.i 2 4 31 cTekst}
IF CAN-DO('1,true,yes,ja,j,y',cTekst) 
THEN bGjFakt = TRUE.
ELSE bGjFakt = FALSE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME fMain

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnSendUtvalg B-Utvalg B-FlereArt ~
btnStopSelection BUTTON-ArtKort BUTTON-Overfor BUTTON-SettLopNr B-Jamfor ~
BUTTON-NyBest BUTTON-EndreBest BUTTON-Kalkyle BUTTON-SlettBest ~
BUTTON-Innleveranse RECT-1 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fLockvindu wWin 
FUNCTION fLockvindu RETURNS CHARACTER
  ( INPUT lLock AS LOGICAL )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getBrowseHandle wWin 
FUNCTION getBrowseHandle RETURNS HANDLE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getEloggtyp wWin 
FUNCTION getEloggtyp RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getSelectType wWin 
FUNCTION getSelectType RETURNS INTEGER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD InitFromKampanje wWin 
FUNCTION InitFromKampanje RETURNS LOGICAL
  ( INPUT ihKampanjeHodeViewer AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD InitFromTelling wWin 
FUNCTION InitFromTelling RETURNS LOGICAL
  ( INPUT ihTelleHode AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD InitFromVareBeh wWin 
FUNCTION InitFromVareBeh RETURNS LOGICAL
  ( INPUT ihVareBehHode AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD InitFromVareBok wWin 
FUNCTION InitFromVareBok RETURNS LOGICAL
  ( INPUT ihVareBokHode AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setBrowseReadOnly wWin 
FUNCTION setBrowseReadOnly RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setButtons wWin 
FUNCTION setButtons RETURNS LOGICAL
  ( INPUT ibSensitive AS LOG )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setEnableBtnSendUtvalg wWin 
FUNCTION setEnableBtnSendUtvalg RETURNS LOGICAL
  ( INPUT ibSensitive AS LOG )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setKampanjeIsMaster wWin 
FUNCTION setKampanjeIsMaster RETURNS LOGICAL
  ( INPUT ibKampanjeIsMaster AS LOG )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setTellingIsMaster wWin 
FUNCTION setTellingIsMaster RETURNS LOGICAL
  ( INPUT ibTellingIsMaster AS LOG )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD SetVareBehIsMaster wWin 
FUNCTION SetVareBehIsMaster RETURNS LOGICAL
  ( INPUT ibVareBehIsMaster AS LOG )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setVareBokIsMaster wWin 
FUNCTION setVareBokIsMaster RETURNS LOGICAL
  ( INPUT ibVareBokIsMaster AS LOG )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ViewHideStopButton wWin 
FUNCTION ViewHideStopButton RETURNS LOGICAL
  ( INPUT ibView AS LOG )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_bbesthodes AS HANDLE NO-UNDO.
DEFINE VARIABLE h_btmpartbas AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dartbas AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dbesthode AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dtmpartbas AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dyntoolbar AS HANDLE NO-UNDO.
DEFINE VARIABLE h_fvisbilde AS HANDLE NO-UNDO.
DEFINE VARIABLE h_sortsok AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-FlereArt 
     LABEL "Legg til artikler..." 
     SIZE 18 BY 1.14.

DEFINE BUTTON B-Jamfor 
     LABEL "Sa&mmenlign..." 
     SIZE 28.2 BY 1.14.

DEFINE BUTTON B-Utvalg 
     LABEL "&Nytt utvalg..." 
     SIZE 16 BY 1.14.

DEFINE BUTTON btnSendUtvalg 
     LABEL "Send utvalg" 
     SIZE 43 BY 1.14.

DEFINE BUTTON btnStopSelection 
     LABEL "Avbryt" 
     SIZE 18 BY 1.14.

DEFINE BUTTON BUTTON-ArtKort 
     LABEL "Arti&kkelkort..." 
     SIZE 28.2 BY 1.14.

DEFINE BUTTON BUTTON-EndreBest 
     LABEL "En&dre.." 
     SIZE 28.2 BY 1.14.

DEFINE BUTTON BUTTON-Innleveranse 
     LABEL "&Varemottak..." 
     SIZE 28 BY 1.14.

DEFINE BUTTON BUTTON-Kalkyle 
     LABEL "Kalky&le..." 
     SIZE 28.2 BY 1.14.

DEFINE BUTTON BUTTON-NyBest 
     LABEL "N&y.." 
     SIZE 28.2 BY 1.14.

DEFINE BUTTON BUTTON-Overfor 
     LABEL "Over&føringer..." 
     SIZE 28.2 BY 1.14.

DEFINE BUTTON BUTTON-SettLopNr 
     LABEL "Sett løpenummer..." 
     SIZE 28.2 BY 1.05.

DEFINE BUTTON BUTTON-SlettBest 
     LABEL "Slette" 
     SIZE 28.2 BY 1.14.

DEFINE BUTTON BUTTON-TilPakke 
     LABEL "Pakkemedlem..." 
     SIZE 28.2 BY 1.14.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 37 BY 1.67.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     btnSendUtvalg AT ROW 2.71 COL 85.6
     B-Utvalg AT ROW 2.71 COL 30
     B-FlereArt AT ROW 2.71 COL 47
     btnStopSelection AT ROW 2.71 COL 66.8
     BUTTON-ArtKort AT ROW 7.71 COL 129.6
     BUTTON-Overfor AT ROW 8.95 COL 129.6
     BUTTON-SettLopNr AT ROW 10.19 COL 129.6
     BUTTON-TilPakke AT ROW 11.33 COL 129.6
     B-Jamfor AT ROW 12.57 COL 129.6
     BUTTON-NyBest AT ROW 19.29 COL 129.6
     BUTTON-EndreBest AT ROW 20.57 COL 129.6
     BUTTON-Kalkyle AT ROW 21.86 COL 129.6
     BUTTON-SlettBest AT ROW 23.1 COL 129.6
     BUTTON-Innleveranse AT ROW 24.38 COL 129.8
     RECT-1 AT ROW 2.48 COL 29
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 157.2 BY 27.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wWin ASSIGN
         HIDDEN             = YES
         TITLE              = "Utvalg artikkelregister"
         HEIGHT             = 26.95
         WIDTH              = 157.2
         MAX-HEIGHT         = 57.14
         MAX-WIDTH          = 320
         VIRTUAL-HEIGHT     = 57.14
         VIRTUAL-WIDTH      = 320
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB wWin 
/* ************************* Included-Libraries *********************** */

{incl/DevMode.i}
{incl/CustDevMode.i}
{src/adm2/containr.i}
{hjelp.i}
{dproclibstart.i}
{incl/wintrigg.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW wWin
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME fMain
   FRAME-NAME Custom                                                    */
ASSIGN 
       btnSendUtvalg:HIDDEN IN FRAME fMain           = TRUE.

ASSIGN 
       btnStopSelection:HIDDEN IN FRAME fMain           = TRUE.

/* SETTINGS FOR BUTTON BUTTON-TilPakke IN FRAME fMain
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Utvalg artikkelregister */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Utvalg artikkelregister */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  
 RUN ExitObject.
/*   APPLY "CLOSE":U TO THIS-PROCEDURE. */
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-RESIZED OF wWin /* Utvalg artikkelregister */
DO:
  DYNAMIC-FUNCTION("setWidgetResize",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE:CURRENT-WINDOW,"Resize","").  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-FlereArt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-FlereArt wWin
ON CHOOSE OF B-FlereArt IN FRAME fMain /* Legg til artikler... */
DO:
  RUN Utvalg.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Jamfor
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Jamfor wWin
ON CHOOSE OF B-Jamfor IN FRAME fMain /* Sammenlign... */
DO:
  DEFINE VARIABLE cArtikkelNr AS CHARACTER  NO-UNDO.

  DO ix = 1 TO hBrowse:NUM-SELECTED-ROWS:
    IF hBrowse:FETCH-SELECTED-ROW(ix) AND ix < 7 THEN DO:
      cArtikkelNr = DYNAMIC-FUNCTION('columnValue':U IN h_dtmpartbas,
                                     INPUT "Artikkelnr" /* CHARACTER */).
  
      IF cArtikkelNr = "" THEN
        LEAVE.
      IF NOT VALID-HANDLE(hJmfRutine) THEN DO:
        RUN w-bildejmf.w PERSISTENT SET hJmfRutine.
        CREATE tmpChild.
        ASSIGN tmpChild.wChild = hJmfRutine.
      END.
      RUN NyArtBas IN hJmfRutine (DECI(cArtikkelNr)).
    END.
    ELSE IF ix > 6 THEN DO:
      DYNAMIC-FUNCTION("DoMessage",0,0,"Maks 6 artikler kan sammenlignes","","").
      LEAVE.
    END.
  END.
  IF VALID-HANDLE(hJmfRutine) THEN DO:
    hJmfRutine:CURRENT-WINDOW:WINDOW-STATE = 3.
    hJmfRutine:CURRENT-WINDOW:MOVE-TO-TOP().
  END.
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Utvalg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Utvalg wWin
ON CHOOSE OF B-Utvalg IN FRAME fMain /* Nytt utvalg... */
DO:

  IF DYNAMIC-FUNCTION("getRecordCount" IN h_dtmpartbas) > 0 THEN DO:
    MESSAGE "Skal utvalg nullstilles?"
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO 
        UPDATE bOk.
    IF bOk <> TRUE THEN
        RETURN NO-APPLY.
  END.

  setButtons(FALSE).
  RUN Nullstill.
  RUN Utvalg.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSendUtvalg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSendUtvalg wWin
ON CHOOSE OF btnSendUtvalg IN FRAME fMain /* Send utvalg */
DO:
  IF VALID-HANDLE(hTelleHode) THEN DO:
    RUN OverfUtvalgTilTelling(DYNAMIC-FUNCTION("getVareTellingNr" IN hTelleHode),OUTPUT bOK).
    IF NOT bOk THEN RETURN NO-APPLY.
  END.
  ELSE IF VALID-HANDLE(hKampanjeHodeViewer) THEN DO:
    RUN OverfUtvalgTilKampanje(DYNAMIC-FUNCTION("getKampanjeId" IN hKampanjeHodeViewer),OUTPUT bOK).
    IF NOT bOk THEN RETURN NO-APPLY.
  END.
  ELSE IF VALID-HANDLE(hVareBokHode) THEN DO:
    RUN OverfUtvalgTilVarebok(DYNAMIC-FUNCTION("getVareBokNr" IN hVareBokHode),OUTPUT bOK). 
    IF NOT bOk THEN RETURN NO-APPLY.
  END.
  ELSE IF VALID-HANDLE(hVareBehHode) THEN DO:
    RUN OverfUtvalgTilVbeh(DYNAMIC-FUNCTION("getVareBehNr" IN hVareBehHode),OUTPUT bOK). 
    IF NOT bOk THEN RETURN NO-APPLY.
  END.

  RUN exitObject.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnStopSelection
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnStopSelection wWin
ON CHOOSE OF btnStopSelection IN FRAME fMain /* Avbryt */
DO:
  DYNAMIC-FUNCTION("StopSelection" IN h_dtmpartbas).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-ArtKort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-ArtKort wWin
ON CHOOSE OF BUTTON-ArtKort IN FRAME fMain /* Artikkelkort... */
DO:
    RUN Artikkelkort.
    RUN setRadFokus IN h_btmpartbas NO-ERROR.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-EndreBest
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-EndreBest wWin
ON CHOOSE OF BUTTON-EndreBest IN FRAME fMain /* Endre.. */
DO:
  DEF VAR wBestHodeRecid AS RECID NO-UNDO.

  FIND BestHode WHERE BestHode.BestNr = INT(
  DYNAMIC-FUNCTION('columnValue':U IN h_dbesthode,
            INPUT "Bestnr")) NO-LOCK NO-ERROR.
  IF NOT AVAIL BestHode THEN
      RETURN NO-APPLY.
  FIND ArtBas OF BestHode NO-LOCK NO-ERROR.
  IF NOT AVAIL ArtBas THEN
      RETURN NO-APPLY.
  ASSIGN
    wBestHodeRecid = RECID(BestHode).
  fLockvindu(TRUE).
  RUN w-gridord.w (INPUT RECID(ArtBas), INPUT-OUTPUT wBestHodeRecid, "ENDRE").
  fLockvindu(FALSE).
  RUN refreshRow IN h_dbesthode.
  /*{&OPEN-QUERY-BROWSE-Ordre}*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Innleveranse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Innleveranse wWin
ON CHOOSE OF BUTTON-Innleveranse IN FRAME fMain /* Varemottak... */
DO:
  DEF VAR wBestHodeRecid AS RECID NO-UNDO.
/*   if not available ArtBas then */
/*     return no-apply.           */
    
  FIND BestHode WHERE BestHode.BestNr = INT(
  DYNAMIC-FUNCTION('columnValue':U IN h_dbesthode,
            INPUT "Bestnr")) NO-LOCK NO-ERROR.
  IF NOT AVAIL BestHode THEN
      RETURN NO-APPLY.
  FIND ArtBas OF BestHode NO-LOCK NO-ERROR.
  IF NOT AVAIL ArtBas THEN
      RETURN NO-APPLY.
  IF ArtBas.LopNr = 0 OR ArtBas.LopNr = ? THEN
    DO:
      MESSAGE "Artikkelen må tildeles løpenummer før innleveranse kan gjøres!"
        VIEW-AS ALERT-BOX MESSAGE TITLE "Melding".
      RETURN NO-APPLY.
    END.
    
  IF NOT AVAILABLE BestHode THEN
    RETURN NO-APPLY.

  IF BestHode.BestStat < 2  THEN
    DO:
      MESSAGE "Bestilling med denne status kan ikke innleveres!"
        VIEW-AS ALERT-BOX TITLE "Melding".
      RETURN NO-APPLY.
    END.  
  
  ASSIGN
    wBestHodeRecid = RECID(BestHode).
  CREATE tmpChild.
  RUN w-gridinnlev.w PERSISTENT SET tmpChild.wChild (INPUT RECID(ArtBas), INPUT-OUTPUT wBestHodeRecid, "INLEV").
  /*{&OPEN-QUERY-BROWSE-Ordre}*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Kalkyle
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Kalkyle wWin
ON CHOOSE OF BUTTON-Kalkyle IN FRAME fMain /* Kalkyle... */
DO:
  FIND BestHode WHERE BestHode.BestNr = INT(
  DYNAMIC-FUNCTION('columnValue':U IN h_dbesthode,
            INPUT "Bestnr")) NO-LOCK NO-ERROR.
  IF NOT AVAIL BestHode THEN
      RETURN NO-APPLY.
  FIND ArtBas OF BestHode NO-LOCK NO-ERROR.
  IF NOT AVAIL ArtBas THEN
      RETURN NO-APPLY.
  RUN d-vbestkalkyle.w (INPUT RECID(ArtBas), INPUT RECID(BestHode)).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-NyBest
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-NyBest wWin
ON CHOOSE OF BUTTON-NyBest IN FRAME fMain /* Ny.. */
DO:
  DEFINE VARIABLE cArtikkelNr AS CHARACTER  NO-UNDO.
  DEF VAR wBestHodeRecid AS RECID NO-UNDO.
/*   if not available ArtBas then */
/*     return no-apply.           */
  ASSIGN cArtikkelNr = DYNAMIC-FUNCTION('columnValue':U IN h_dtmpartbas,
            INPUT "Artikkelnr" /* CHARACTER */).
  IF cArtikkelNr = ? THEN DO:
      MESSAGE "Ingen artikkel tillgjengelig."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN NO-APPLY.
  END.
  IF DYNAMIC-FUNCTION('columnValue':U IN h_dtmpartbas,
         INPUT "Aktivert" /* CHARACTER */) = "no" THEN
    DO:
      MESSAGE "Artikkelen er ikke aktivert. Den må aktiveres før bestilling kan registreres."
        VIEW-AS ALERT-BOX MESSAGE TITLE "Melding".
      RETURN NO-APPLY.
    END.
    IF DYNAMIC-FUNCTION('columnValue':U IN h_dtmpartbas,
           INPUT "Lager" /* CHARACTER */) = "no" THEN
      DO:
      MESSAGE "Artikkelen har ikke lagerstyring. Bestilling kan ikke registreres."
        VIEW-AS ALERT-BOX MESSAGE TITLE "Melding".
      RETURN NO-APPLY.
    END.
  FIND ArtBas WHERE ArtBas.ArtikkelNr = DECI(cArtikkelnr) NO-LOCK.
  ASSIGN
    wBestHodeRecid = ?.
/*   create tmpChild.                                                                                          */
/*   run w-gridord.w  persistent set tmpChild.wChild (input recid(ArtBas), input-output wBestHodeRecid, "NY"). */
  fLockvindu(TRUE).
  RUN w-gridord.w   (INPUT RECID(ArtBas), INPUT-OUTPUT wBestHodeRecid, "NY").
  fLockvindu(FALSE).
  IF wBestHodeRecid <> ? THEN
      DYNAMIC-FUNCTION('openQuery':U IN h_dbesthode).
  /*
  if wBestHodeRecid <> ? then
    do:
      {&OPEN-QUERY-BROWSE-Ordre}
    end.
  */
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Overfor
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Overfor wWin
ON CHOOSE OF BUTTON-Overfor IN FRAME fMain /* Overføringer... */
DO:
    RUN Overfor.
    RETURN NO-APPLY.
/*   FIND ArtBas WHERE ArtBas.ArtikkelNr = DECI(DYNAMIC-FUNCTION('columnValue':U IN h_dartbas, */
/*         INPUT "Artikkelnr")) NO-LOCK NO-ERROR.                                              */
/*   if not available ArtBas then                                                              */
/*         return no-apply.                                                                    */
/*   if ArtBas.Aktivert = false then                                                           */
/*     do:                                                                                     */
/*       message "Artikkelen er ikke aktivert."                                                */
/*         view-as alert-box message title "Melding".                                          */
/*       return no-apply.                                                                      */
/*     end.                                                                                    */
/*   if ArtBas.Lager = false then                                                              */
/*     do:                                                                                     */
/*       message "Artikkelen har ikke lagerstyring."                                           */
/*         view-as alert-box message title "Melding".                                          */
/*       return no-apply.                                                                      */
/*     end.                                                                                    */
/*   if ArtBas.LopNr = 0 or ArtBas.LopNr = ? then                                              */
/*     do:                                                                                     */
/*       message "Artikkelen er ikke tildelt løpenummer!"                                      */
/*         view-as alert-box MESSAGE title "Melding".                                          */
/*       return no-apply.                                                                      */
/*     end.                                                                                    */
/*                                                                                             */
/*   if available ArtBas then                                                                  */
/*     do:                                                                                     */
/*       create tmpChild.                                                                      */
/*       run w-gridlager.w PERSISTENT set tmpChild.wChild (input recid(ArtBas), "SOEK").       */
/*     end.                                                                                    */

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SettLopNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SettLopNr wWin
ON CHOOSE OF BUTTON-SettLopNr IN FRAME fMain /* Sett løpenummer... */
DO:
  RUN SettLopenummer.
  RUN refreshRow IN h_dtmpartbas.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SlettBest
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SlettBest wWin
ON CHOOSE OF BUTTON-SlettBest IN FRAME fMain /* Slette */
DO:
    IF CAN-DO("4,5",DYNAMIC-FUNCTION('columnValue':U IN h_dbesthode,
              INPUT "BestStat" /* CHARACTER */)) THEN
  DO:
      MESSAGE "Bestilling " DYNAMIC-FUNCTION('columnValue':U IN h_dbesthode,
              INPUT "BestNr" /* CHARACTER */) "er sendt til leverandør (eller delhvis levert)!" SKIP
          "Skal den alikevel tas bort?"
          VIEW-AS ALERT-BOX BUTTONS YES-NO-CANCEL TITLE "Bekreftelse"
          UPDATE wOk AS LOGICAL.
  END.
  ELSE DO:  
    MESSAGE "Skal bestilling slettes?"
      VIEW-AS ALERT-BOX BUTTONS YES-NO-CANCEL TITLE "Bekreftelse" 
    UPDATE wOk.
  END.
  IF wOk THEN
      RUN SlettBestilling IN THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-TilPakke
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-TilPakke wWin
ON CHOOSE OF BUTTON-TilPakke IN FRAME fMain /* Pakkemedlem... */
DO:
/*   if not available ArtBas then                                              */
/*     return no-apply.                                                        */
/*   create tmpChild.                                                          */
/*   run w-barttranslogg.w PERSISTENT set tmpChild.wChild (ArtBas.ArtikkelNr). */
    RUN d-tilpakke.w (DYNAMIC-FUNCTION('columnValue':U IN h_dtmpartbas,
        INPUT "Artikkelnr"),DYNAMIC-FUNCTION('getFilename':U IN h_fvisbilde)).
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wWin 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */

{src/adm2/windowmn.i}

{lng.i &SDO = "SDO"}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects wWin  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/
  DEFINE VARIABLE currentPage  AS INTEGER NO-UNDO.

  ASSIGN currentPage = getCurrentPage().

  CASE currentPage: 

    WHEN 0 THEN DO:
       RUN constructObject (
             INPUT  'sdo/dtmpartbas.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessnoDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsObjectNamedtmpartbasOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch200RebuildOnReposnoToggleDataTargetsyes':U ,
             OUTPUT h_dtmpartbas ).
       RUN repositionObject IN h_dtmpartbas ( 26.00 , 130.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'prg/btmpartbas.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesUseSortIndicatoryesSearchFieldDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_btmpartbas ).
       RUN repositionObject IN h_btmpartbas ( 4.33 , 2.00 ) NO-ERROR.
       RUN resizeObject IN h_btmpartbas ( 23.57 , 127.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'sdo/dartbas.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessnoDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsArtBas.ArtikkelNr,ArtikkelNrObjectNamedartbasOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch200RebuildOnReposnoToggleDataTargetsyes':U ,
             OUTPUT h_dartbas ).
       RUN repositionObject IN h_dartbas ( 26.00 , 147.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'prg/fvisbilde.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'LogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_fvisbilde ).
       RUN repositionObject IN h_fvisbilde ( 2.43 , 130.00 ) NO-ERROR.
       /* Size in AB:  ( 4.81 , 28.20 ) */

       RUN constructObject (
             INPUT  'sdo/dbesthode.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessnoDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsBestHode.ArtikkelNr,ArtikkelNrObjectNamedbesthodeOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch200RebuildOnReposnoToggleDataTargetsyes':U ,
             OUTPUT h_dbesthode ).
       RUN repositionObject IN h_dbesthode ( 26.00 , 138.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'prg/bbesthodes.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesUseSortIndicatoryesSearchFieldDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_bbesthodes ).
       RUN repositionObject IN h_bbesthodes ( 13.86 , 129.60 ) NO-ERROR.
       RUN resizeObject IN h_bbesthodes ( 5.00 , 28.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'adm2/dyntoolbar.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'EdgePixels2DeactivateTargetOnHidenoDisabledActionsFlatButtonsyesMenunoShowBorderyesToolbaryesActionGroupsNavigationTableIOTypeSupportedLinksNavigation-sourceToolbarBandsToolbarAutoSizenoToolbarDrawDirectionHorizontalLogicalObjectNameDisabledActionsHiddenActionsUpdate,ResetHiddenToolbarBandsHiddenMenuBandsMenuMergeOrder0RemoveMenuOnHidenoCreateSubMenuOnConflictyesNavigationTargetNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dyntoolbar ).
       RUN repositionObject IN h_dyntoolbar ( 1.00 , 1.00 ) NO-ERROR.
       RUN resizeObject IN h_dyntoolbar ( 1.24 , 157.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'prg/sortsok.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'HideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_sortsok ).
       RUN repositionObject IN h_sortsok ( 2.43 , 1.00 ) NO-ERROR.
       /* Size in AB:  ( 1.76 , 27.60 ) */

       /* Links to SmartDataObject h_dtmpartbas. */
       RUN addLink ( h_dyntoolbar , 'Navigation':U , h_dtmpartbas ).

       /* Links to SmartDataBrowser h_btmpartbas. */
       RUN addLink ( h_dtmpartbas , 'Data':U , h_btmpartbas ).
       RUN addLink ( h_btmpartbas , 'Update':U , h_dtmpartbas ).

       /* Links to SmartDataObject h_dartbas. */
       RUN addLink ( h_dtmpartbas , 'Data':U , h_dartbas ).

       /* Links to SmartFrame h_fvisbilde. */
       RUN addLink ( h_dartbas , 'Data':U , h_fvisbilde ).

       /* Links to SmartDataObject h_dbesthode. */
       RUN addLink ( h_dtmpartbas , 'Data':U , h_dbesthode ).

       /* Links to SmartDataBrowser h_bbesthodes. */
       RUN addLink ( h_dbesthode , 'Data':U , h_bbesthodes ).

       /* Links to SmartObject h_sortsok. */
       RUN addLink ( h_btmpartbas , 'Sortera':U , h_sortsok ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_btmpartbas ,
             btnSendUtvalg:HANDLE IN FRAME fMain , 'AFTER':U ).
       RUN adjustTabOrder ( h_dyntoolbar ,
             h_btmpartbas , 'AFTER':U ).
       RUN adjustTabOrder ( h_sortsok ,
             h_dyntoolbar , 'AFTER':U ).
       RUN adjustTabOrder ( h_fvisbilde ,
             h_sortsok , 'AFTER':U ).
       RUN adjustTabOrder ( h_bbesthodes ,
             B-Jamfor:HANDLE IN FRAME fMain , 'AFTER':U ).
    END. /* Page 0 */

  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AnnulTilbud wWin 
PROCEDURE AnnulTilbud :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cArtNrList AS CHAR NO-UNDO.
iReturn = 0.
RUN JBoxBrowseSelectMsg.w ("Bekreft annulering av tilbud",
                            hBrowse:NUM-SELECTED-ROWS,
                            STRING(DYNAMIC-FUNCTION("getRecordCount" IN h_dtmpartbas)),
                            OUTPUT iReturn).
IF iReturn = 0 THEN RETURN.
IF hBrowse:NUM-SELECTED-ROWS > 0 AND iReturn = 2 THEN DO:
  DO ix = 1 TO hBrowse:NUM-SELECTED-ROWS:
    IF hBrowse:FETCH-SELECTED-ROW(ix) THEN
      cArtNrList = cArtNrList +
                        STRING(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE) + ",".
  END.
  IF NOT DYNAMIC-FUNCTION("RunProc","avbryt_Kampanje.p",
                          cValues + ",ARTNUM," + TRIM(cArtNrList,","),
                          ?) THEN
    DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Advarsel","").
END.
ELSE IF NOT DYNAMIC-FUNCTION("RunProc","avbryt_Kampanje.p",cValues,DYNAMIC-FUNCTION("getTmpArtBasHandle" IN h_dtmpartbas)) THEN
  DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Advarsel","").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ApplHjelp wWin 
PROCEDURE ApplHjelp :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN Hjelp ("","").
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ApplyBtnArtikkelKort wWin 
PROCEDURE ApplyBtnArtikkelKort :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
APPLY "choose" TO BUTTON-ArtKort IN FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ApplyEntryToBrowse wWin 
PROCEDURE ApplyEntryToBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/* MESSAGE "DEB"                          */
/*     VIEW-AS ALERT-BOX INFO BUTTONS OK. */
/* APPLY "entry" TO hBrowse.                                                 */
/* IF hBrowse:FOCUSED-ROW NE ? THEN hBrowse:SELECT-ROW(hBrowse:FOCUSED-ROW). */
RUN SetRadFokus IN h_btmpartbas NO-ERROR.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Artikkelkort wWin 
PROCEDURE Artikkelkort :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cArtikkelNr AS CHARACTER  NO-UNDO.
  ASSIGN cArtikkelNr = DYNAMIC-FUNCTION('columnValue':U IN h_dtmpartbas,
        INPUT "Artikkelnr" /* CHARACTER */).
  FIND ArtBas WHERE ArtBas.ArtikkelNr = DECI(cArtikkelNr) NO-LOCK NO-ERROR.
  fLockvindu(TRUE).
  RUN w-vartkor (INPUT RECID(ArtBas), "ENDRE," + STRING(THIS-PROCEDURE)).
  fLockvindu(FALSE).
  RUN refreshRow IN h_dtmpartbas.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BildeGrid wWin 
PROCEDURE BildeGrid :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
iReturn = 0.
RUN JBoxBrowseSelectMsg.w ("Bekreft overføring av artikler til bildegrid",
                            hBrowse:NUM-SELECTED-ROWS,
                            STRING(DYNAMIC-FUNCTION("getRecordCount" IN h_dtmpartbas)),
                            OUTPUT iReturn).
IF iReturn = 0 THEN RETURN.

RUN ByggTmpTabell.

RUN byggTTBildegrid.p ("FOR EACH tmp2ArtBas","tmp2ArtBas","Artikkelnr").
/* RUN byggTTBildegrid.p (DYNAMIC-FUNCTION('getQueryString':U IN h_dtmpartbas),"tmpArtBas","Artikkelnr"). */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BildeImport wWin 
PROCEDURE BildeImport :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cArtNrList AS CHAR NO-UNDO.
ASSIGN
    cFilKatalog = ""
    iReturn     = 0
    .
RUN d-BildeImport   ("Importer bilde på artikler",
                            hBrowse:NUM-SELECTED-ROWS,
                            STRING(DYNAMIC-FUNCTION("getRecordCount" IN h_dtmpartbas)),
                            OUTPUT iReturn,
                            OUTPUT bOK,
                            OUTPUT cFilKatalog).
IF iReturn = 0 THEN RETURN.

IF hBrowse:NUM-SELECTED-ROWS > 0 AND iReturn = 2 THEN DO:
  DO ix = 1 TO hBrowse:NUM-SELECTED-ROWS:
    IF hBrowse:FETCH-SELECTED-ROW(ix) THEN 
      cArtNrList = cArtNrList +
                        STRING(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE) + ",". 
  END.
  IF NOT DYNAMIC-FUNCTION("RunProc","art_bildeimport.p",
                          STRING(bOk) + "," + cFilKatalog + ",ARTNUM," + TRIM(cArtNrList,","),
                          ?) THEN
    DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Advarsel","").
END.
ELSE IF NOT DYNAMIC-FUNCTION("RunProc","art_bildeimport.p",STRING(bOk) + "," + cFilKatalog,DYNAMIC-FUNCTION("getTmpArtBasHandle" IN h_dtmpartbas)) THEN 
   DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Advarsel","").

IF iReturn = 2 AND cArtNrList = "" THEN
    RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ByggTmpTabell wWin 
PROCEDURE ByggTmpTabell :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR liLoop          AS INT  NO-UNDO.
  DEF VAR lcColValues     AS CHAR NO-UNDO.
  DEF VAR llArtikkelNr    AS DEC  NO-UNDO.
  DEF VAR lcRecordState   AS CHAR NO-UNDO.
  DEF VAR lcQueryPos      AS CHAR NO-UNDO.

  /* Sjekker om det er noen vare i utvalget. */
  {get QueryPosition lcQueryPos h_dtmpartbas}.
  IF lcQueryPos = 'NoRecordAvailable' THEN
      RETURN.

 {sww.i}
  RUN Byggtmp2ArtBas IN h_dtmpartbas (OUTPUT TABLE tmp2ArtBas).
 {swn.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI wWin  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
  THEN DELETE WIDGET wWin.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI wWin  _DEFAULT-ENABLE
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
  ENABLE btnSendUtvalg B-Utvalg B-FlereArt btnStopSelection BUTTON-ArtKort 
         BUTTON-Overfor BUTTON-SettLopNr B-Jamfor BUTTON-NyBest 
         BUTTON-EndreBest BUTTON-Kalkyle BUTTON-SlettBest BUTTON-Innleveranse 
         RECT-1 
      WITH FRAME fMain IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-fMain}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Etiketter wWin 
PROCEDURE Etiketter :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*   RUN ByggTmpTabell.                         */
/*                                              */
/*   FOR EACH tmp2artbas:                       */
/*       MESSAGE artikkelnr                     */
/*           VIEW-AS ALERT-BOX INFO BUTTONS OK. */
/*   END.                                       */
/*                                              */
/*                                              */
/*                                              */
  DEFINE VARIABLE ii AS INTEGER    NO-UNDO.
  DEF VAR cArtNrList AS CHAR NO-UNDO.
  DEFINE VAR TTh AS HANDLE.
  TTh = BUFFER Etikett:HANDLE.
  EMPTY TEMP-TABLE Etikett.
  iReturn = 0.
  RUN d-BekreftEtiketter.w   ("Etiketter fra artikler i utvalg",
                              hBrowse:NUM-SELECTED-ROWS,
                              STRING(DYNAMIC-FUNCTION("getRecordCount" IN h_dtmpartbas)),
                              OUTPUT iReturn,
                              OUTPUT cValues).

  IF iReturn = 0 THEN RETURN.
/*                                                                                                                                  */
  IF hBrowse:NUM-SELECTED-ROWS > 0 AND iReturn = 2 THEN DO:
    DO ix = 1 TO hBrowse:NUM-SELECTED-ROWS:
      IF hBrowse:FETCH-SELECTED-ROW(ix) THEN DO:
          CREATE Etikett.
          ASSIGN Etikett.artikkelnr = hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE NO-ERROR.
          IF ERROR-STATUS:ERROR THEN
              DELETE Etikett.
      END.
    END.
  END.
  ELSE DO:
      RUN ByggTmpTabell.
      FOR EACH tmp2artbas:
          CREATE Etikett.
          Etikett.artikkelnr = tmp2artbas.artikkelnr NO-ERROR.
          IF ERROR-STATUS:ERROR THEN
              DELETE Etikett.
      END.
  END.
  IF NOT CAN-FIND(FIRST Etikett) THEN
      RETURN.
  
  DYNAMIC-FUNCTION("getMyTempTable","get_ArtEan.p","",TTh).

  IF CAN-FIND(FIRST etikett WHERE etikett.ean <> "") THEN DO:
      IF NOT VALID-HANDLE(hEtikettVindu) THEN
          RUN w-TmpEtikett.w PERSISTENT SET hEtikettVindu (THIS-PROCEDURE:CURRENT-WINDOW).
      IF VALID-HANDLE(hEtikettVindu) THEN DO:
          FOR EACH Etikett WHERE Etikett.Ean <> "":
              DO ii = 1 TO NUM-ENTRIES(Etikett.Ean):
                  RUN NyEtikett IN hEtikettVindu (ENTRY(ii,Etikett.Ean),1,0).
              END.
          END.
      END.

  END.
/*     IF NOT DYNAMIC-FUNCTION("RunProc","art_sett_farge.p",                                                                        */
/*                             cValues + ",ARTNUM," + TRIM(cArtNrList,","),                                                         */
/*                             ?) THEN                                                                                              */
/*       DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Advarsel","").                                 */
/*   END.                                                                                                                           */
/*   ELSE IF NOT DYNAMIC-FUNCTION("RunProc","art_sett_farge.p",cValues,DYNAMIC-FUNCTION("getTmpArtBasHandle" IN h_dtmpartbas)) THEN */
/*     DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Advarsel","").                                   */
/*                                                                                                                                  */
/*                                                                                                                                  */
/*   DEF TEMP-TABLE Etikett NO-UNDO                                                                                                 */
/*       FIELD Artikkelnr AS DECI                                                                                                   */
/*       FIELD EAN        AS CHAR.                                                                                                  */
  
  
  
  
  
  
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ExcelRecord wWin 
PROCEDURE ExcelRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DYNAMIC-FUNCTION("ToExcelViaFile",hBrowse,0).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE exitObject wWin 
PROCEDURE exitObject :
/*------------------------------------------------------------------------------
  Purpose:  Window-specific override of this procedure which destroys 
            its contents and itself.
    Notes:  
------------------------------------------------------------------------------*/
  IF VALID-HANDLE(h_wartbasutvalg)     THEN APPLY "close" TO h_wartbasutvalg.
  IF VALID-HANDLE(hRapportGen)         THEN APPLY "close" TO hRapportGen.
  IF VALID-HANDLE(hEtikettVindu)       THEN APPLY "close" TO hEtikettVindu.
  /* Fjern knapp "Hent fra utvalg" i kampanje: */
  IF VALID-HANDLE(hKampanjeHodeViewer) THEN 
    DYNAMIC-FUNCTION("InitUtvalgToKampanje" IN hKampanjeHodeViewer,?,DYNAMIC-FUNCTION("UtvalgIsMaster" IN hKampanjeHodeViewer)). /* NO: Kampanje er master */ 
  /* Fjern knapp "Hent fra utvalg" i telling: */
  IF VALID-HANDLE(hTelleHode) THEN 
    DYNAMIC-FUNCTION("InitUtvalgToTelling" IN hTelleHode,?,DYNAMIC-FUNCTION("UtvalgIsMaster" IN hTelleHode)). /* NO: Kampanje er master */ 
  /* Fjern knapp "Hent fra utvalg" i Varebok: */
  IF VALID-HANDLE(hVareBokHode) THEN 
    DYNAMIC-FUNCTION("InitUtvalgToVarebok" IN hVareBokHode,?,DYNAMIC-FUNCTION("UtvalgIsMaster" IN hVareBokHode)). /* NO: Varebok er master */ 
  IF VALID-HANDLE(hVareBehHode) THEN 
    DYNAMIC-FUNCTION("InitUtvalgToVareBeh" IN hVareBehHode,?,DYNAMIC-FUNCTION("UtvalgIsMaster" IN hVareBehHode)). /* NO: VareBeh er master */ 

  DYNAMIC-FUNCTION("setCleanUpResize", THIS-PROCEDURE:CURRENT-WINDOW).
  
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE HtVare wWin 
PROCEDURE HtVare :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR iTypeId AS INTEGER    NO-UNDO.
  DEF VAR lcWhere AS CHAR       NO-UNDO.

/*   iReturn = DYNAMIC-FUNCTION("DoMessage",0,1,"Bekreft overføring av &1 artikler til håndterminal","", */
/*                    IF hBrowse:NUM-SELECTED-ROWS > 0 THEN                                              */
/*                      STRING(hBrowse:NUM-SELECTED-ROWS)                                                */
/*                    ELSE                                                                               */
/*                      STRING(DYNAMIC-FUNCTION("getRecordCount" IN h_dtmpartbas))                       */
/*                    ).                                                                                 */
/*   IF iReturn = 2 THEN RETURN.                                                                         */

  RUN d-velgHT.w (OUTPUT iTypeId).
  IF RETURN-VALUE = "AVBRYT" THEN
      RETURN NO-APPLY.

  RUN ByggTmpTabell.

  IF NOT CAN-FIND(FIRST tmp2ArtBas) THEN
  DO:
      MESSAGE "Ingen varer i utvalg."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN NO-APPLY.
  END.

  ASSIGN
       lcWhere = 'for each tmp2artbas no-lock by Beskr indexed-reposition'
      .
  
  RUN htvarefil.p (lcWhere,iTypeId,'tmp2ArtBas').
  
  RETURN NO-APPLY.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject wWin 
PROCEDURE initializeObject :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
{syspara.i 1 1 18 cHKInst}

/* Code placed here will execute PRIOR to standard behavior. */
IF VALID-HANDLE(h_dtmpartbas) THEN
    DYNAMIC-FUNCTION('setQueryWhere':U IN h_dtmpartbas,
   INPUT "FALSE" /* CHARACTER */).

RUN SUPER.

btnSendUtvalg:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.

/* Code placed here will execute AFTER standard behavior.    */
IF VALID-HANDLE(h_dyntoolbar) THEN DO:
    RUN OpprettKnapper.
 END.

SUBSCRIBE TO "ApplHjelp" IN h_dynToolbar.
RUN SwitchLng.
/*   IF VALID-HANDLE(h_dproclib) THEN                */
/*       RUN GetLng IN h_dproclib (OUTPUT wCurrLng). */
IF VALID-HANDLE(h_dtmpartbas) THEN
   DYNAMIC-FUNCTION('setQueryWhere':U IN h_dtmpartbas,
  INPUT "" /* CHARACTER */).


PUBLISH "Sortera" FROM h_btmpartbas.

hBrowse = DYNAMIC-FUNCTION("getBrowseHandle" IN h_btmpartbas).

DYNAMIC-FUNCTION("setNoResizeX",THIS-PROCEDURE:CURRENT-WINDOW,
                                DYNAMIC-FUNCTION("getContainerHandle" IN h_bbesthodes),
                                "f-Main,br_table").
DYNAMIC-FUNCTION("setNoMoveX",THIS-PROCEDURE:CURRENT-WINDOW,
                                DYNAMIC-FUNCTION("getContainerHandle" IN h_bbesthodes),
                                "br_table").

DYNAMIC-FUNCTION("setNoResizeX",THIS-PROCEDURE:CURRENT-WINDOW,
                                DYNAMIC-FUNCTION("getContainerHandle" IN h_fvisbilde),
                                "fMain,IMAGE-Sko").
DYNAMIC-FUNCTION("setNoResizeY",THIS-PROCEDURE:CURRENT-WINDOW,
                                DYNAMIC-FUNCTION("getContainerHandle" IN h_fvisbilde),
                                "fMain,IMAGE-Sko").
DYNAMIC-FUNCTION("setNoMoveX",THIS-PROCEDURE:CURRENT-WINDOW,
                                DYNAMIC-FUNCTION("getContainerHandle" IN h_fvisbilde),
                                "IMAGE-Sko").
DYNAMIC-FUNCTION("setNoMoveY",THIS-PROCEDURE:CURRENT-WINDOW,
                                DYNAMIC-FUNCTION("getContainerHandle" IN h_fvisbilde),
                                "IMAGE-Sko").

DYNAMIC-FUNCTION("setNoResizeX",THIS-PROCEDURE:CURRENT-WINDOW,
                                DYNAMIC-FUNCTION("getContainerHandle" IN h_sortsok),
                                "f-Main,rect-1").
DYNAMIC-FUNCTION("setNoResizeY",THIS-PROCEDURE:CURRENT-WINDOW,
                                DYNAMIC-FUNCTION("getContainerHandle" IN h_sortsok),
                                "f-Main,rect-1").

DYNAMIC-FUNCTION("setNoResizeX",THIS-PROCEDURE:CURRENT-WINDOW,
                                FRAME {&FRAME-NAME}:HANDLE,
                                "rect-1").
DYNAMIC-FUNCTION("setNoResizeY",THIS-PROCEDURE:CURRENT-WINDOW,
                                FRAME {&FRAME-NAME}:HANDLE,
                                "rect-1").

DYNAMIC-FUNCTION("SetResizeADM2panel",TRUE).

DYNAMIC-FUNCTION("setOrgWinSize",THIS-PROCEDURE:CURRENT-WINDOW,600,380,0,0).

PROCESS EVENTS.
APPLY "choose" TO B-Utvalg IN FRAME {&FRAME-NAME}.

THIS-PROCEDURE:CURRENT-WINDOW:HIDDEN = FALSE.

RUN fetchFirst IN h_dtmpartbas.
RUN refreshBrowse IN h_btmpartbas.

ViewHideStopButton(FALSE).

IF NOT bTellingIsMaster AND NOT bKampanjeIsMaster AND NOT bVareBokIsMaster THEN DO:
  DYNAMIC-FUNCTION("NewMenuBand",
                    hBrowse,  /* parent widget */
                    "DelUtv;Fjern markerte artikler fra utvalg;SlettFraUtvalg" +
                    ",excel;Eksporter til Excel" +
                    ",|-" +
                    ",BildeGrid;Vis bildegrid;BildeGrid" +
                    ",LagerVare;Vis lager;LagerVare" +
                    ",Statistikk;Vis statistikk;Statistikk" +
                    ",Translogg;Vis translogg;Translogg" +
                    (IF cHKInst <> "yes" THEN 
                      ",Lagerliste;Utskrift av lagerliste;Lagerliste"
                     ELSE  
                      "") +
                   ",Etiketter;Utskrift av etiketter;Etiketter" +
                   ",Plakater;Utskrift av plakater;Plakater" +
                    ",|-" +
                    ",|Overfør" +
                    ",|-" +
                    ",AnnulTilbud;Annuller tilbud;AnnulTilbud" +
                    ",|-" +
                    ",|Endre artikkel flagg" +
                    ",|Endre artikkel info" +
                    ",|-" +
                    ",DelArt;Slett artikler fra artikkelregister;SlettArtikkel" +
                    ",DelLogg;Vis slettelogg;VisSletteLogg" +
                    ",|-" +
                    ",BildeImport;Bilde import;BildeImport"
                    ,
                    "").     

  DYNAMIC-FUNCTION("NewMenuBand",
                    WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",hBrowse,"placeholder1")),  /* parent widget */                    
                    "HtVare;Overfør til håndterminal;HtVare"
                    + ",Tilkasse;Overfør til kasse;Tilkasse"
                    + ",Tilweb;Overfør til webshop;Tilweb"
                    + ",Tilwebart;Overfør til woocommweb artinfo;Tilwebart"
                    + ",VpiVare;Overfør til VPI-register/Send til butikk;VpiVare"
                    + ",PrisTilButikk;Overfør pris til butikk;PrisTilButikk"
                    + ",|-" 
                    + ",VpiTilHK;Overfør til HK;VpiTilHK"
                    + ",|-" 
                    + ",OverfKampanje;Overfør til kampanje/prisendring;OverfKampanje" 
                    + ",OverfTelling;Overfør til varetelling;OverfTelling" 
                    + ",OverfVarebok;Overfør til varebok;OverfVarebok" 
                    + ",OverfVarebeh;Overfør til varehåndteringsbok;OverfVarebeh" 
                    + ",|-" 
                    + ",OverfPRIKAT;Overfør til PRICAT;OverfPRIKAT" 
                    + ",OverfERP;Overfør til ERP;OverfERP" 
                    ,
                    "").     

  /* Artikkelflagg */
  DYNAMIC-FUNCTION("NewMenuBand",
                    WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",hBrowse,"placeholder2")),  /* parent widget */                    
                   "SettAnnonse;Sett annonseflagg;SettAnnonse" +
                   ",SettManRabIKasse;Sett manuell rabatt i kasse;SettManRabIKasse" +
                   ",SettKjedeVare;Sett kjedevareflagg;SettKjedevare" +
                   ",SettKunderabatt;Sett kunderabatt i kasse;SettKunderabatt" +
                   ",SettUtgatt;Sett utgåttflagg;SettUtgatt" +
                   ",SettVareIKasse;Sett vare i kasse;SettVareIKasse" +
                   ",SettGjFaktureres;Sett gjennomfaktureres kjedekontor;SettGjFaktureres" + 
                   ",SettMedlemsutbytte;Sett medlemsutbytte;SettMedlemsutbytte" + 
                   ",SettWebButikkArtikkel;Sett Web-Artikkel;SettWebButikkArtikkel" + 
                   ",SettWebArtikkelButikk;Sett Web-Artikkel-Butikk;SettWebArtikkelButikk" + 
                   ",SettHoyLavMva;Sett Høy/Lav Mva;SettHoyLavMva" +
                   ",SettGrunnsortiment;Sett grunnsortiment;SettGrunnsortiment" +
                   ",SettRabattGivende;Sett rabatt grunnlag;SettRabattGivende" 
                    ,
                    "").     
  /* Artikkel info */
  DYNAMIC-FUNCTION("NewMenuBand",
                    WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",hBrowse,"placeholder3")),  /* parent widget */                    
                    "SettVaregruppe;Endre varegruppe;SettVaregruppe" +
                    ",SettLeverandor;Endre leverandør;SettLeverandor" +
                    ",SettVaremerke;Endre varemerke;SettVaremerke" +
                    ",SettProdusent;Endre produsent;SettProdusent" +
                    ",SettFarge;Endre farge;SettFarge" +
                    ",SettMaterial;Endre material;SettMaterial" +
                    ",SettSesong;Endre sesong;SettSesong" +
                    ",SettVaretype;Endre varetype;SettVaretype" +
                    ",SettWebLeveringstid;Endre leveringstid fra nettbutikk;SettWebLeveringstid" +
                    ",SettWebMinLager;Endre min.lager i nettbutikk;SettWebMinLager" +
                    ",SettPostPakkeInfo;Endre postpakkeinformasjon;SettPostPakkeInfo" +
                    ",SettStrType;Endre størrelsestype;SettStrType" +
                    ",SettAnbefaltPris;Endre anbefalt pris;SettAnbefaltPris" +
                    ",SettRAvdNr;Endre vareområde;SettRAvdNr" +
                    ",SettSalgsenhet;Endre salgsenhet;SettSalgsenhet" +
                    ",SettLokasjon;Endre lokasjon;SettLokasjon" +
                    ",SettKampanjekode;Endre kampanjekode;SettKampanjekode" +
                    ",SettHovedkategori;Endre hoved og underkategori;SettHovedkategori" +
                    ",SettKjokkenskriver;Endre Kjøkkenskriver;SettKjokkenskriver"
                    ,
                    "").     

END.

setButtons(FALSE).
  DYNAMIC-FUNCTION("InitTranslation",THIS-PROCEDURE:CURRENT-WINDOW).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InvalidateHandle wWin 
PROCEDURE InvalidateHandle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       Invalidate handle to child program and enable the window
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihChild AS HANDLE NO-UNDO.

IF ihChild = hKampanje THEN hKampanje = ?.

THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Lagerliste wWin 
PROCEDURE Lagerliste :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lcWhere AS CHAR NO-UNDO.

  iReturn = 0.
  RUN JBoxBrowseSelectMsg.w ("Bekreft utskrift av lagerliste",
                              hBrowse:NUM-SELECTED-ROWS,
                              STRING(DYNAMIC-FUNCTION("getRecordCount" IN h_dtmpartbas)),
                              OUTPUT iReturn).
  IF iReturn = 0 THEN RETURN.

  RUN ByggTmpTabell.

  IF NOT CAN-FIND(FIRST tmp2ArtBas) THEN
  DO:
      MESSAGE "Ingen varer i utvalg."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN NO-APPLY.
  END.

  ASSIGN
       lcWhere = 'for each tmp2artbas no-lock by Beskr indexed-reposition'
      .

 {sww.i} 
  RUN byggTTartikkler.p (lcWhere,"tmp2Artbas","Artikkelnr",DYNAMIC-FUNCTION("getButikkIdListe" IN h_dtmpartbas)).
 {swn.i} 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LagerVare wWin 
PROCEDURE LagerVare :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

iReturn = 0.
RUN JBoxBrowseSelectMsg.w ("Bekreft visning av lager",
                            hBrowse:NUM-SELECTED-ROWS,
                            STRING(DYNAMIC-FUNCTION("getRecordCount" IN h_dtmpartbas)),
                            OUTPUT iReturn).
IF iReturn = 0 THEN RETURN.

RUN StartStatistikk ('LAGER').

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Nullstill wWin 
PROCEDURE Nullstill :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {sww.i} 
  RUN Nullstill IN h_dtmpArtBas.
  DYNAMIC-FUNCTION('openQuery':U IN h_dtmpartbas).

  /* Tømmer tabellen - klar for ny runde. */
  FOR EACH tmp2ArtBas:
    DELETE tmp2ArtBas.
  END.
  {swn.i} 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpprettKnapper wWin 
PROCEDURE OpprettKnapper :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*
   DEFINE VARIABLE hFrame  AS HANDLE     NO-UNDO.
   DEFINE VARIABLE hHandle AS HANDLE     NO-UNDO.
   DEFINE VARIABLE hButton AS HANDLE     NO-UNDO.
   DEFINE VARIABLE iPos    AS INTEGER    NO-UNDO.
   DEFINE VARIABLE piX     AS INTEGER    NO-UNDO.
   DEFINE VARIABLE cHKInst AS CHARACTER  NO-UNDO.
   {syspara.i 1 1 18 cHKInst}
   &SCOP dlmt + CHR(1) +
   
   ASSIGN hFrame  = DYNAMIC-FUNCTION('getContainerHandle':U IN h_dyntoolbar)
          hHandle = hFrame
          hHandle = hHandle:FIRST-CHILD  /* första field-group          */
          hHandle = hHandle:FIRST-CHILD. /* första widget i field-group */
   REPEAT WHILE VALID-HANDLE(hHandle):
       /* hämtar X-pos för sista 'rulen' i toolbaren */
       IF hHandle:TYPE = "RECTANGLE" AND hHandle:X > piX THEN
           ASSIGN iPos = hHandle:X + hHandle:WIDTH-PIXELS
                  piX  = hHandle:X.
       ASSIGN hHandle = hHandle:NEXT-SIBLING.
   END.
   
   /* Excel */
   ASSIGN  piX = piX + 4 /* lägger till 'luft' */
       hButton = DYNAMIC-FUNCTION('createButton':U IN h_dyntoolbar,
         INPUT hFrame,
         INPUT-OUTPUT piX        /* INTEGER */,
         INPUT "ExcelRecord"            /* CHARACTER */,
         INPUT "Eksporter til Excel"            /* CHARACTER Knapptext*/,
         INPUT "Eksporter til Excel"            /* CHARACTER */,
         INPUT "gif\afexcel.gif" /* CHARACTER */,
         INPUT TRUE              /* LOGICAL */).

  DYNAMIC-FUNCTION('defineAction':U IN h_dyntoolbar,
     INPUT "ExcelRecord":U /* CHARACTER */,
     INPUT "Name,Caption,Image,Type,OnChoose,AccessType,Parent" /* CHARACTER */,
     INPUT "ExcelRecord"      {&dlmt}
          "ExcelRecord"      {&dlmt}
          ""        {&dlmt}
          "PUBLISH":U {&dlmt}
          "ExcelRecord":U   {&dlmt}
          "":U        {&dlmt}
          "":U).

   /* Varefilbutton */
   ASSIGN  piX = piX + 4 /* lägger till 'luft' */
       hButton = DYNAMIC-FUNCTION('createButton':U IN h_dyntoolbar,
         INPUT hFrame,
         INPUT-OUTPUT piX        /* INTEGER */,
         INPUT "HtVare"            /* CHARACTER */,
         INPUT "Eksport håndterminal"            /* CHARACTER Knapptext*/,
         INPUT "Data til håndterminal"            /* CHARACTER */,
         INPUT "" /* CHARACTER */,
/*          INPUT "icon\ht.bmp" /* CHARACTER */, */
         INPUT TRUE              /* LOGICAL */).

  DYNAMIC-FUNCTION('defineAction':U IN h_dyntoolbar,
     INPUT "HtVare":U /* CHARACTER */,
     INPUT "Name,Caption,Image,Type,OnChoose,AccessType,Parent" /* CHARACTER */,
     INPUT "HtVare"      {&dlmt}
          "HtVare"      {&dlmt}
          "":U        {&dlmt}
          "PUBLISH":U {&dlmt}
          "HtVare":U   {&dlmt}
          "":U        {&dlmt}
          "":U).
 
  IF cHKInst <> "yes" THEN DO:
     ASSIGN piX = piX + 4
         hButton = DYNAMIC-FUNCTION('createButton':U IN h_dyntoolbar,
           INPUT hFrame,
           INPUT-OUTPUT piX                 /* INTEGER */,
           INPUT "Lagerliste"                  /* CHARACTER */,
           INPUT "Lagerliste" /* CHARACTER Knapptext*/,
           INPUT "Lagerliste"  /* CHARACTER */,
           INPUT "" /* CHARACTER */,
  /*          INPUT "icon\ht.bmp"  /* CHARACTER */, */
           INPUT TRUE              /* LOGICAL */).

    DYNAMIC-FUNCTION('defineAction':U IN h_dyntoolbar,
       INPUT "Lagerliste":U /* CHARACTER */,
       INPUT "Name,Caption,Image,Type,OnChoose,AccessType,Parent" /* CHARACTER */,
       INPUT "Lagerliste"      {&dlmt}
            "Lagerliste"      {&dlmt}
            "":U        {&dlmt}
            "PUBLISH":U {&dlmt}
            "Lagerliste":U   {&dlmt}
            "":U        {&dlmt}
            "":U).
    
    ASSIGN piX = piX + 4
        hButton = DYNAMIC-FUNCTION('createButton':U IN h_dyntoolbar,
          INPUT hFrame,
          INPUT-OUTPUT piX                 /* INTEGER */,
          INPUT "BildeGrid"                  /* CHARACTER */,
          INPUT "BildeGrid" /* CHARACTER Knapptext*/,
          INPUT "BildeGrid"  /* CHARACTER */,
          INPUT "" /* CHARACTER */,
   /*          INPUT "icon\ht.bmp"  /* CHARACTER */, */
          INPUT TRUE              /* LOGICAL */).

     DYNAMIC-FUNCTION('defineAction':U IN h_dyntoolbar,
        INPUT "BildeGrid":U /* CHARACTER */,
        INPUT "Name,Caption,Image,Type,OnChoose,AccessType,Parent" /* CHARACTER */,
        INPUT "BildeGrid"      {&dlmt}
             "BildeGrid"      {&dlmt}
             "":U        {&dlmt}
             "PUBLISH":U {&dlmt}
             "BildeGrid":U   {&dlmt}
             "":U        {&dlmt}
             "":U).


    ASSIGN piX = piX + 4
        hButton = DYNAMIC-FUNCTION('createButton':U IN h_dyntoolbar,
          INPUT hFrame,
          INPUT-OUTPUT piX                 /* INTEGER */,
          INPUT "Tilkasse"                  /* CHARACTER */,
          INPUT "Til kasse" /* CHARACTER Knapptext*/,
          INPUT "Tilkasse"  /* CHARACTER */,
          INPUT "" /* CHARACTER */,
 /*          INPUT "icon\ht.bmp"  /* CHARACTER */, */
          INPUT TRUE              /* LOGICAL */).

   DYNAMIC-FUNCTION('defineAction':U IN h_dyntoolbar,
      INPUT "Tilkasse":U /* CHARACTER */,
      INPUT "Name,Caption,Image,Type,OnChoose,AccessType,Parent" /* CHARACTER */,
      INPUT "Tilkasse"      {&dlmt}
           "Tilkasse"      {&dlmt}
           "":U        {&dlmt}
           "PUBLISH":U {&dlmt}
           "Tilkasse":U   {&dlmt}
           "":U        {&dlmt}
           "":U).
      
   /* Kampanje: */
   ASSIGN piX = piX + 4
       hKampanjeButton = DYNAMIC-FUNCTION('createButton':U IN h_dyntoolbar,
         INPUT hFrame,
         INPUT-OUTPUT piX                 /* INTEGER */,
         INPUT "OverfKampanje"                  /* CHARACTER */,
         INPUT "Overf.Kampanje" /* CHARACTER Knapptext*/,
         INPUT "Overfør til Kampanje"  /* CHARACTER */,
         INPUT "" /* CHARACTER */,
         INPUT TRUE              /* LOGICAL */).

  DYNAMIC-FUNCTION('defineAction':U IN h_dyntoolbar,
     INPUT "OverfKampanje":U /* CHARACTER */,
     INPUT "Name,Caption,Image,Type,OnChoose,AccessType,Parent" /* CHARACTER */,
     INPUT "OverfKampanje"      {&dlmt}
          "OverfKampanje"      {&dlmt}
          "":U        {&dlmt}
          "PUBLISH":U {&dlmt}
          "OverfKampanje":U   {&dlmt}
          "":U        {&dlmt}
          "":U).

  /* Varefilbutton */
/*   ASSIGN  piX = piX + 4 /* lägger till 'luft' */                                  */
/*       hButton = DYNAMIC-FUNCTION('createButton':U IN h_dyntoolbar,                */
/*         INPUT hFrame,                                                             */
/*         INPUT-OUTPUT piX        /* INTEGER */,                                    */
/*         INPUT "HtVare"            /* CHARACTER */,                                */
/*         INPUT "Eksport håndterminal"            /* CHARACTER Knapptext*/,         */
/*         INPUT "Data til håndterminal"            /* CHARACTER */,                 */
/*         INPUT "" /* CHARACTER */,                                                 */
/* /*          INPUT "icon\ht.bmp" /* CHARACTER */, */                               */
/*         INPUT TRUE              /* LOGICAL */).                                   */
/*                                                                                   */
/*    DYNAMIC-FUNCTION('defineAction':U IN h_dyntoolbar,                             */
/*       INPUT "HtVare":U /* CHARACTER */,                                           */
/*       INPUT "Name,Caption,Image,Type,OnChoose,AccessType,Parent" /* CHARACTER */, */
/*       INPUT "HtVare"      {&dlmt}                                                 */
/*            "HtVare"      {&dlmt}                                                  */
/*            "":U        {&dlmt}                                                    */
/*            "PUBLISH":U {&dlmt}                                                    */
/*            "HtVare":U   {&dlmt}                                                   */
/*            "":U        {&dlmt}                                                    */
/*            "":U).                                                                 */


  END.
  IF cHKInst = "yes" THEN DO:
     ASSIGN piX = piX + 4
         hButton = DYNAMIC-FUNCTION('createButton':U IN h_dyntoolbar,
           INPUT hFrame,
           INPUT-OUTPUT piX                 /* INTEGER */,
           INPUT "VpiVare"                  /* CHARACTER */,
           INPUT "Overfør til VPI-register" /* CHARACTER Knapptext*/,
           INPUT "Overfør til VPI-register"  /* CHARACTER */,
           INPUT "" /* CHARACTER */,
  /*          INPUT "icon\ht.bmp"  /* CHARACTER */, */
           INPUT TRUE              /* LOGICAL */).

    DYNAMIC-FUNCTION('defineAction':U IN h_dyntoolbar,
       INPUT "VpiVare":U /* CHARACTER */,
       INPUT "Name,Caption,Image,Type,OnChoose,AccessType,Parent" /* CHARACTER */,
       INPUT "VpiVare"      {&dlmt}
            "VpiVare"      {&dlmt}
            "":U        {&dlmt}
            "PUBLISH":U {&dlmt}
            "VpiVare":U   {&dlmt}
            "":U        {&dlmt}
            "":U).
  END.

  /* Funksjon for blanking av Annonseflagg */
  BLANKANNONSE:
  DO:
      ASSIGN piX = piX + 4
          hButton = DYNAMIC-FUNCTION('createButton':U IN h_dyntoolbar,
            INPUT hFrame,
            INPUT-OUTPUT piX                 /* INTEGER */,
            INPUT "BlankAnnonse"                  /* CHARACTER */,
            INPUT "Nullstill annonseflagg" /* CHARACTER Knapptext*/,
            INPUT "Nullstill annonseflagg"  /* CHARACTER */,
            INPUT "" /* CHARACTER */,
   /*          INPUT "icon\ht.bmp"  /* CHARACTER */, */
            INPUT TRUE              /* LOGICAL */).

     DYNAMIC-FUNCTION('defineAction':U IN h_dyntoolbar,
        INPUT "BlankAnnonse":U /* CHARACTER */,
        INPUT "Name,Caption,Image,Type,OnChoose,AccessType,Parent" /* CHARACTER */,
        INPUT "BlankAnnonse"      {&dlmt}
             "BlankAnnonse"      {&dlmt}
             "":U        {&dlmt}
             "PUBLISH":U {&dlmt}
             "BlankAnnonse":U   {&dlmt}
             "":U        {&dlmt}
             "":U).
  END. /* BLANKANNONSE */

   /* Action EXIT finns i toolbar sedan tidigare */
   ASSIGN piX = hFrame:WIDTH-PIXELS - 26 /*hButton:WIDTH-PIXELS - 4*/
          hButton = DYNAMIC-FUNCTION('createButton':U IN h_dyntoolbar,
            INPUT hFrame,
            INPUT-OUTPUT piX        /* INTEGER   */,
            INPUT "exit"            /* CHARACTER */,
            INPUT "exit"            /* CHARACTER */,
            INPUT "exit"            /* CHARACTER */,
            INPUT "icon\e-exit.bmp" /* CHARACTER */,
            INPUT TRUE              /* LOGICAL   */
            ).
   
   ASSIGN piX = hButton:X - hButton:WIDTH-PIXELS - 1.
          hButton = DYNAMIC-FUNCTION('createButton':U IN h_dyntoolbar,
            INPUT hFrame,
            INPUT-OUTPUT piX        /* INTEGER */,
            INPUT "HELP"            /* CHARACTER */,
            INPUT "HELP"            /* CHARACTER */,
            INPUT "HELP"            /* CHARACTER */,
            INPUT "icon\e-help.bmp" /* CHARACTER */,
            INPUT TRUE              /* LOGICAL */).

   DYNAMIC-FUNCTION('defineAction':U IN h_dyntoolbar,
      INPUT "HELP":U /* CHARACTER */,
      INPUT "Name,Caption,Image,Type,OnChoose,AccessType,Parent" /* CHARACTER */,
      INPUT "Help"      {&dlmt}
            "Help"      {&dlmt}
            "":U        {&dlmt}
            "PUBLISH":U {&dlmt}
            "ApplHjelp":U   {&dlmt}
            "":U        {&dlmt}
            "":U).
    
*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OverfERP wWin 
PROCEDURE OverfERP :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR iTypeId AS INTEGER    NO-UNDO.
  DEF VAR lcWhere AS CHAR       NO-UNDO.

  iReturn = DYNAMIC-FUNCTION("DoMessage",0,1,"Bekreft overføring av &1 artikler til ERP fil","",
                   IF hBrowse:NUM-SELECTED-ROWS > 0 THEN
                     STRING(hBrowse:NUM-SELECTED-ROWS)
                   ELSE
                     STRING(DYNAMIC-FUNCTION("getRecordCount" IN h_dtmpartbas))
                   ).
  IF iReturn = 2 THEN RETURN.

  RUN ByggTmpTabell.

  IF NOT CAN-FIND(FIRST tmp2ArtBas) THEN
  DO:
      MESSAGE "Ingen varer i utvalg."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN NO-APPLY.
  END.

  ASSIGN
       lcWhere = 'for each tmp2artbas no-lock by Beskr indexed-reposition'
      .
  {sww.i}
  RUN erpfil.p (lcWhere,iTypeId,'tmp2ArtBas').
  {swn.i}
  RETURN NO-APPLY.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OverfKampanje wWin 
PROCEDURE OverfKampanje :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF NOT VALID-HANDLE(hKampanjeHodeViewer) THEN DO:
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN wKampanje.w PERSIST SET hKampanje.
  RUN InitializeObject IN hKampanje.
  hKampanjeHodeViewer = DYNAMIC-FUNCTION("getKampanjeHodeViewer" IN hKampanje, THIS-PROCEDURE).
  DYNAMIC-FUNCTION("InitUtvalgToKampanje" IN hKampanjeHodeViewer, THIS-PROCEDURE,YES). /* Yes: Utvalg er master */
END.
ELSE DO:
  hKampanjeHodeViewer:CURRENT-WINDOW:MOVE-TO-TOP().
  hKampanjeHodeViewer:CURRENT-WINDOW:WINDOW-STATE = 3.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Overfor wWin 
PROCEDURE Overfor :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE hDetteVindu AS HANDLE     NO-UNDO.
  FIND ArtBas WHERE ArtBas.ArtikkelNr = DECI(DYNAMIC-FUNCTION('columnValue':U IN h_dtmpartbas,
        INPUT "Artikkelnr")) NO-LOCK NO-ERROR.
  IF NOT AVAILABLE ArtBas THEN
        RETURN NO-APPLY.
  IF ArtBas.Aktivert = FALSE THEN   
    DO:
      MESSAGE "Artikkelen er ikke aktivert."
        VIEW-AS ALERT-BOX MESSAGE TITLE "Melding".
      RETURN NO-APPLY.
    END.
  IF ArtBas.Lager = FALSE THEN   
    DO:
      MESSAGE "Artikkelen har ikke lagerstyring."
        VIEW-AS ALERT-BOX MESSAGE TITLE "Melding".
      RETURN NO-APPLY.
    END.
  IF ArtBas.LopNr = 0 OR ArtBas.LopNr = ? THEN
    DO:
      MESSAGE "Artikkelen er ikke tildelt løpenummer!"
        VIEW-AS ALERT-BOX MESSAGE TITLE "Melding".
      RETURN NO-APPLY.
    END.
  
    fLockvindu(TRUE).
    RUN w-gridlager.w (INPUT RECID(ArtBas), "SOEK").
    fLockvindu(FALSE).
/*   if available ArtBas then                                                            */
/*     do:                                                                               */
/*       create tmpChild.                                                                */
/*       run w-gridlager.w PERSISTENT set tmpChild.wChild (input recid(ArtBas), "SOEK"). */
/*     end.                                                                              */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OverfPRIKAT wWin 
PROCEDURE OverfPRIKAT :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR iTypeId AS INTEGER    NO-UNDO.
  DEF VAR lcWhere AS CHAR       NO-UNDO.
  DEF VAR obOk    AS LOG NO-UNDO.
/*
  iReturn = DYNAMIC-FUNCTION("DoMessage",0,1,"Bekreft overføring av &1 artikler til PRICAT fil","",
                   IF hBrowse:NUM-SELECTED-ROWS > 0 THEN
                     STRING(hBrowse:NUM-SELECTED-ROWS)
                   ELSE
                     STRING(DYNAMIC-FUNCTION("getRecordCount" IN h_dtmpartbas))
                   ).
  IF iReturn = 2 THEN RETURN.
*/
  RUN ByggTmpTabell.

  IF NOT CAN-FIND(FIRST tmp2ArtBas) THEN
  DO:
      MESSAGE "Ingen varer i utvalg."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN NO-APPLY.
  END.
  ELSE DO:
      obOk = TRUE.
      MESSAGE "Eksporterer alle artikler i utvalget til Prikat fil." + CHR(10) + 
       "Eksportere gjeldende kalkyle eller forhånds og suppleringsrabatt?" + CHR(10) + 
              "Nei = Gjeldende kalkyle, Ja = Forh. og supl. rabatt."
          VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO-CANCEL UPDATE obOk.
      IF obOk = ? THEN
          RETURN.
      ELSE IF obOk THEN
          iTypeId = 1.
      ELSE 
          iTypeId = 0.
  END.

  ASSIGN
       lcWhere = 'for each tmp2artbas no-lock by Beskr indexed-reposition'
      .
  /*{sww.i}*/
  RUN prikatfil.p (lcWhere,iTypeId,'tmp2ArtBas').
  /*{swn.i}*/
  RETURN NO-APPLY.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OverfTelling wWin 
PROCEDURE OverfTelling :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF NOT VALID-HANDLE(hTelleHode) THEN DO WITH FRAME {&FRAME-NAME}:

  /* Ny tellerutine aktivert */
  IF (CAN-FIND(FIRST EkstVPIFil WHERE
              EkstVPIFil.VPIFilAktiv = TRUE AND
              EkstVPIFil.VPIFilType  = 8 AND
              EkstVPIFil.VPIFilNavn             = 'TELLING') OR
      CAN-FIND(FIRST EkstVPIFil WHERE
              EkstVPIFil.VPIFilAktiv = TRUE AND
              EkstVPIFil.VPIFilType  = 8 AND
              EkstVPIFil.VPIFilNavn             = 'VARETRAN')) THEN
  DO:
      RUN varetelling.w PERSIST SET hTelleHode.
      IF VALID-HANDLE(hTelleHode) THEN 
      DO:
        RUN initializeObject IN hTellehode.
        PUBLISH 'setTransferBtn' FROM THIS-PROCEDURE (THIS-PROCEDURE,TRUE).
      END.
  END.
  /* Gammel tellerutine */
  ELSE DO:
    THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
    RUN w-bTellehode.w PERSIST SET hTelleHode.
    DYNAMIC-FUNCTION("InitUtvalgToTelling" IN hTelleHode, THIS-PROCEDURE,YES). /* Yes: Utvalg er master */
  END.
END.
ELSE DO:
  hTelleHode:CURRENT-WINDOW:MOVE-TO-TOP().
  hTelleHode:CURRENT-WINDOW:WINDOW-STATE = 3.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OverfUtvalgTilKampanje wWin 
PROCEDURE OverfUtvalgTilKampanje :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM  iiKampanjeId AS INT NO-UNDO.
DEF OUTPUT PARAM obOk         AS LOG NO-UNDO.

DEF VAR cArtnumList AS CHAR NO-UNDO.

iReturn = 0.
RUN JBoxBrowseSelectMsg.w ("Bekreft overføring av artikler til kampanje " + STRING(iiKampanjeId),
                            hBrowse:NUM-SELECTED-ROWS,
                            STRING(DYNAMIC-FUNCTION("getRecordCount" IN h_dtmpartbas)),
                            OUTPUT iReturn).
IF iReturn = 0 THEN RETURN.

IF iReturn = 1 THEN 
  bOK = DYNAMIC-FUNCTION("RunProc","art_to_kampanje.p",STRING(iiKampanjeId),DYNAMIC-FUNCTION("getTmpArtBasHandle" IN h_dtmpartbas)).
ELSE DO:
  DO ix = 1 TO hBrowse:NUM-SELECTED-ROWS:
    IF hBrowse:FETCH-SELECTED-ROW(ix) THEN 
      cArtnumList = cArtnumList + STRING(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE) + ",". 
  END.
  bOK = DYNAMIC-FUNCTION("RunProc","art_to_kampanje.p",
                           STRING(iiKampanjeId) + ",ARTNUM," + TRIM(cArtnumList,","),
                           ?).
END.
IF NOT bOK THEN
  DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Feil i overføring til Kampanje",""). 

obOk = bOK.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OverfUtvalgTilTelling wWin 
PROCEDURE OverfUtvalgTilTelling :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM iiTelleNr AS INT NO-UNDO.
DEF OUTPUT PARAM obOK     AS LOG NO-UNDO.

DEF VAR cArtnumList AS CHAR NO-UNDO.

iReturn = 0.
RUN JBoxBrowseSelectMsg.w ("Bekreft overføring av artikler til varetelling " + STRING(iiTelleNr), 
                            hBrowse:NUM-SELECTED-ROWS,
                            STRING(DYNAMIC-FUNCTION("getRecordCount" IN h_dtmpartbas)),
                            OUTPUT iReturn).
IF iReturn = 0 THEN RETURN.

IF iReturn = 1 THEN 
  bOK = DYNAMIC-FUNCTION("RunProc","art_to_telling.p",STRING(iiTelleNr),DYNAMIC-FUNCTION("getTmpArtBasHandle" IN h_dtmpartbas)).
ELSE DO:
  DO ix = 1 TO hBrowse:NUM-SELECTED-ROWS:
    IF hBrowse:FETCH-SELECTED-ROW(ix) THEN 
      cArtnumList = cArtnumList + STRING(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE) + ",". 
  END.
  bOK = DYNAMIC-FUNCTION("RunProc","art_to_telling.p",
                          STRING(iiTelleNr) + ",ARTNUM," + TRIM(cArtnumList,","),
                          ?).
END.
IF NOT bOK THEN
  DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Feil i overføring til Telling",""). 

obOK = bOK.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OverfUtvalgTilVarebok wWin 
PROCEDURE OverfUtvalgTilVarebok :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT  PARAM ifVareBokNr AS DEC NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR cArtnumList AS CHAR NO-UNDO.

iReturn = 0.
RUN JBoxBrowseSelectMsg.w ("Bekreft overføring av artikler til varebok " + STRING(ifVareBokNr),
                            hBrowse:NUM-SELECTED-ROWS,
                            STRING(DYNAMIC-FUNCTION("getRecordCount" IN h_dtmpartbas)),
                            OUTPUT iReturn).
IF iReturn = 0 THEN RETURN.

IF iReturn = 1 THEN 
  bOK = DYNAMIC-FUNCTION("RunProc","art_to_varebok.p",STRING(ifVareBokNr),DYNAMIC-FUNCTION("getTmpArtBasHandle" IN h_dtmpartbas)).
ELSE DO:
  DO ix = 1 TO hBrowse:NUM-SELECTED-ROWS:
    IF hBrowse:FETCH-SELECTED-ROW(ix) THEN 
      cArtnumList = cArtnumList + STRING(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE) + ",". 
  END.
  bOK = DYNAMIC-FUNCTION("RunProc","art_to_varebok.p",
                          STRING(ifVareBokNr) + ",ARTNUM," + TRIM(cArtnumList,","),
                          ?).
END.
IF NOT bOK THEN
  DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Feil i overføring til Varebok",""). 

obOk = bOK.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OverfUtvalgTilVbeh wWin 
PROCEDURE OverfUtvalgTilVbeh :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT  PARAM ifVareBehNr AS DEC NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR cArtnumList AS CHAR NO-UNDO.

iReturn = 0.
RUN JBoxBrowseSelectMsg.w ("Bekreft overføring av artikler til VareBeh " + STRING(ifVareBehNr),
                            hBrowse:NUM-SELECTED-ROWS,
                            STRING(DYNAMIC-FUNCTION("getRecordCount" IN h_dtmpartbas)),
                            OUTPUT iReturn).
IF iReturn = 0 THEN RETURN.

IF iReturn = 1 THEN 
  bOK = DYNAMIC-FUNCTION("RunProc","art_to_varebeh.p",STRING(ifVareBehNr),DYNAMIC-FUNCTION("getTmpArtBasHandle" IN h_dtmpartbas)).
ELSE DO:
  DO ix = 1 TO hBrowse:NUM-SELECTED-ROWS:
    IF hBrowse:FETCH-SELECTED-ROW(ix) THEN 
      cArtnumList = cArtnumList + STRING(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE) + ",". 
  END.
  bOK = DYNAMIC-FUNCTION("RunProc","art_to_varebeh.p",
                          STRING(ifVareBehNr) + ",ARTNUM," + TRIM(cArtnumList,","),
                          ?).
END.
IF NOT bOK THEN
  DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Feil i overføring til VareBeh",""). 

obOk = bOK.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OverfVarebeh wWin 
PROCEDURE OverfVarebeh :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF NOT VALID-HANDLE(hVareBehHode) THEN DO WITH FRAME {&FRAME-NAME}:
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN butVareBehhode.w PERSIST SET hVareBehHode.
  DYNAMIC-FUNCTION("InitUtvalgToVareBeh" IN hVareBehHode, THIS-PROCEDURE,YES). /* Yes: Utvalg er master */
END.
ELSE DO:
  hVareBehHode:CURRENT-WINDOW:MOVE-TO-TOP().
  hVareBehHode:CURRENT-WINDOW:WINDOW-STATE = 3.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OverfVarebok wWin 
PROCEDURE OverfVarebok :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF NOT VALID-HANDLE(hVareBokHode) THEN DO WITH FRAME {&FRAME-NAME}:
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN varebokhode.w PERSIST SET hVareBokHode.
  DYNAMIC-FUNCTION("InitUtvalgToVareBok" IN hVareBokHode, THIS-PROCEDURE,YES). /* Yes: Utvalg er master */
END.
ELSE DO:
  hVareBokHode:CURRENT-WINDOW:MOVE-TO-TOP().
  hVareBokHode:CURRENT-WINDOW:WINDOW-STATE = 3.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Plakater wWin 
PROCEDURE Plakater :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cArtikkelNr AS CHARACTER  NO-UNDO.
  ASSIGN cArtikkelNr = DYNAMIC-FUNCTION('columnValue':U IN h_dtmpartbas,
        INPUT "Artikkelnr" /* CHARACTER */).
  FIND ArtBas WHERE ArtBas.ArtikkelNr = DECI(cArtikkelNr) NO-LOCK NO-ERROR.
  fLockvindu(TRUE).
  RUN wSkrivUtPlakat.w (Artbas.Artikkelnr).
  fLockvindu(FALSE).
  RUN refreshRow IN h_dtmpartbas.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PrevNext wWin 
PROCEDURE PrevNext :
/*------------------------------------------------------------------------------
  Purpose:     bläddring från artikelkort
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER cRettning AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE pcState AS CHARACTER  NO-UNDO.
    IF CAN-DO("Prev,Next",cRettning) THEN DO:
        CASE cRettning:
            WHEN "Prev" THEN
                RUN fetchPrev IN h_dtmpartbas.
            WHEN "Next" THEN
                RUN fetchNext IN h_dtmpartbas.
        END CASE.
        PUBLISH "ByttArtikkel" (DECI(DYNAMIC-FUNCTION('columnValue':U IN h_dtmpartbas,
              INPUT "Artikkelnr"))).
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PrisTilButikk wWin 
PROCEDURE PrisTilButikk :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cArtnumList AS CHAR NO-UNDO.
DEF VAR bSend       AS LOG  NO-UNDO.
DEF VAR cEDBSystem  AS CHAR NO-UNDO.
DEF VAR iAntSlett   AS INT  NO-UNDO.
DEF VAR iAntNyEndre AS INT  NO-UNDO.
DEF VAR bBilder     AS LOG  NO-UNDO.
DEF VAR bOk         AS LOG  NO-UNDO.
DEF VAR cButLst     AS CHAR NO-UNDO.


DEF VAR cVareFiler AS CHAR NO-UNDO.
DEF VAR cMixFiler  AS CHAR NO-UNDO.
DEF VAR iAntVarer  AS CHAR NO-UNDO.
DEF VAR iAntPakker AS CHAR NO-UNDO.

DEFINE VARIABLE cOutputDir AS CHARACTER NO-UNDO.

{syspara.i 1 1 56 cOutputDir}
cOutputDir = TRIM(RIGHT-TRIM(cOutputDir,'\')).

iReturn = 0.
RUN d-setpristilbutikk.w   ("Overføre priser til butikk|TMPARTBAS",
                        hBrowse:NUM-SELECTED-ROWS,
                        STRING(DYNAMIC-FUNCTION("getRecordCount" IN h_dtmpartbas)),
                        OUTPUT iReturn,
                        OUTPUT cValues).

IF iReturn = 0 THEN RETURN.

/* skal det sendes til butikkene? */
/*
IF ENTRY(2,cValues,"|") = "YES" THEN bSend   = TRUE.
IF bSend = FALSE THEN RETURN.
*/
cButLst = TRIM(ENTRY(1,cValues,'|')).


IF cButLst = '' THEN
DO:
    MESSAGE 'Det er ikke angitt noen butikker. Den/de valgte artikler blir sendt til alle butikker.'
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE 'Bekreft sending til alle butikker' UPDATE bOk.
    IF bOk <> TRUE THEN RETURN.
END.

/* Flagger ELoggspostene spesielt slik at disse kan håndteres ulikt standard håndtering. */
cEDBSystem = "POS" + STRING(TIME).

/* Det skapes ELogg poster for den/de artiklene som skal sendes */
IF iReturn = 1 THEN 
    bOK = DYNAMIC-FUNCTION("RunProc","pris_til_butikk.p",
                           ("|" + cEDBSystem + "|||" + cButLst),
                           DYNAMIC-FUNCTION("getTmpArtBasHandle" IN h_dtmpartbas)).
ELSE DO:
  DO ix = 1 TO hBrowse:NUM-SELECTED-ROWS:
    IF hBrowse:FETCH-SELECTED-ROW(ix) THEN 
      cArtnumList = cArtnumList + STRING(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE) + ",".
  END.

  bOK = DYNAMIC-FUNCTION("RunProc","pris_til_butikk.p",
                         ("ARTNR|" + cEDBSystem) + "|" + TRIM(cArtnumList,",") + "||" + cButLst,
                         ?).
END.

IF NOT bOK THEN
  DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Feil i overføring til VPI-register",""). 
ELSE DO: 

  /* Sending til ALLE butikker hvis butikklisten er blank. */
  IF TRIM(cButLst) = '' THEN 
  DO:
    FOR EACH Butiker NO-LOCK WHERE
      Butiker.ApningsDato <> ? AND 
      Butiker.NedlagtDato = ? AND  
      Butiker.harButikksystem = TRUE:
      ASSIGN
        cButLst = cButLst + 
                (IF cButLst = '' THEN '' ELSE ',') +
                STRING(Butiker.Butik).
    END. 
  END.

  /* 2 Parameter efter cOutputdir ær en ingång vi skall anvænda før utlægg av kampanjer, Kommer nedan */
  RUN ArtBas2Nucleus.p (?,
                        INPUT cButLst,
                        INPUT cOutputDir,
                        0,
                        TRUE,
                        cEDBSystem,
                        OUTPUT cVareFiler,
                        OUTPUT cMixFiler,
                        OUTPUT iAntVarer,
                        OUTPUT iAntPakker).
  DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Antall varer og butikker det er overført priser til",""). 
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetKnapperad wWin 
PROCEDURE SetKnapperad :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER ipState AS CHARACTER  NO-UNDO.
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN BUTTON-ArtKort:SENSITIVE      = ipState <> "NoRecordAvailableExt"
           BUTTON-Overfor:SENSITIVE      = BUTTON-ArtKort:SENSITIVE AND
                            DYNAMIC-FUNCTION('columnValue':U IN h_dtmpartbas,
                                 INPUT "OPris" /* CHARACTER */) = "no"
           BUTTON-SettLopNr:SENSITIVE    = BUTTON-ArtKort:SENSITIVE AND
               DYNAMIC-FUNCTION('columnValue':U IN h_dtmpartbas,
                   INPUT "Lopnr" /* CHARACTER */) = ?
           BUTTON-TilPakke:SENSITIVE     = BUTTON-ArtKort:SENSITIVE AND 
               DYNAMIC-FUNCTION('columnValue':U IN h_dtmpartbas,
                   INPUT "Pakkenr" /* CHARACTER */) = "0" AND
               DYNAMIC-FUNCTION('columnValue':U IN h_dtmpartbas,
                    INPUT "OPris" /* CHARACTER */) = "no"
           B-Jamfor:SENSITIVE            = BUTTON-ArtKort:SENSITIVE
           BUTTON-NyBest:SENSITIVE       = BUTTON-TilPakke:SENSITIVE AND
               DYNAMIC-FUNCTION('columnValue':U IN h_dtmpartbas,
                    INPUT "OPris" /* CHARACTER */) = "no"
           BUTTON-EndreBest:SENSITIVE    = NOT ipState BEGINS "NoRecordAvailable"
           BUTTON-Innleveranse:SENSITIVE = BUTTON-EndreBest:SENSITIVE
           BUTTON-Kalkyle:SENSITIVE      = BUTTON-EndreBest:SENSITIVE
           BUTTON-SlettBest:SENSITIVE    = BUTTON-EndreBest:SENSITIVE.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SettAnbefaltPris wWin 
PROCEDURE SettAnbefaltPris :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cArtNrList AS CHAR NO-UNDO.


iReturn = 0.
RUN d-settprispunkt.w   ("Endre anbefalt pris på artikler i utvalg",
                            hBrowse:NUM-SELECTED-ROWS,
                            STRING(DYNAMIC-FUNCTION("getRecordCount" IN h_dtmpartbas)),
                            OUTPUT iReturn,
                            OUTPUT cValues).
IF iReturn = 0 THEN RETURN.

IF hBrowse:NUM-SELECTED-ROWS > 0 AND iReturn = 2 THEN DO:
  DO ix = 1 TO hBrowse:NUM-SELECTED-ROWS:
    IF hBrowse:FETCH-SELECTED-ROW(ix) THEN 
      cArtNrList = cArtNrList +
                        STRING(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE) + ",". 
  END.
  IF NOT DYNAMIC-FUNCTION("RunProc","art_sett_prispunkt.p",
                          cValues + ",ARTNUM," + TRIM(cArtNrList,","),
                          ?) THEN 
    DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Advarsel","").
END.
ELSE IF NOT DYNAMIC-FUNCTION("RunProc","art_sett_prispunkt.p",cValues,DYNAMIC-FUNCTION("getTmpArtBasHandle" IN h_dtmpartbas)) THEN 
  DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Advarsel","").


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SettAnnonse wWin 
PROCEDURE SettAnnonse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cAnnonseArtNrList AS CHAR NO-UNDO.


iReturn = 0.
RUN d-SettAnnonseFlagg.w   ("Sett annonseflagg for artikler",
                            hBrowse:NUM-SELECTED-ROWS,
                            STRING(DYNAMIC-FUNCTION("getRecordCount" IN h_dtmpartbas)),
                            OUTPUT iReturn,
                            OUTPUT bOK).
IF iReturn = 0 THEN RETURN.

IF hBrowse:NUM-SELECTED-ROWS > 0 AND iReturn = 2 THEN DO:
  DO ix = 1 TO hBrowse:NUM-SELECTED-ROWS:
    IF hBrowse:FETCH-SELECTED-ROW(ix) THEN 
      cAnnonseArtNrList = cAnnonseArtNrList +
                        STRING(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE) + ",". 
  END.
  IF NOT DYNAMIC-FUNCTION("RunProc","art_sett_annonse.p",
                          STRING(bOk) + ",ARTNUM," + TRIM(cAnnonseArtNrList,","),
                          ?) THEN 
    DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Advarsel","").
END.
ELSE IF NOT DYNAMIC-FUNCTION("RunProc","art_sett_annonse.p",STRING(bOk),DYNAMIC-FUNCTION("getTmpArtBasHandle" IN h_dtmpartbas)) THEN 
  DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Advarsel","").


/*   DEF VAR bOk     AS LOG  NO-UNDO.                                         */
/*   DEF VAR lcWhere AS CHAR NO-UNDO.                                         */
/*                                                                            */
/*   RUN ByggTmpTabell.                                                       */
/*                                                                            */
/*   IF NOT CAN-FIND(FIRST tmp2ArtBas) THEN                                   */
/*   DO:                                                                      */
/*       MESSAGE "Ingen varer i utvalg."                                      */
/*           VIEW-AS ALERT-BOX INFO BUTTONS OK.                               */
/*       RETURN NO-APPLY.                                                     */
/*   END.                                                                     */
/*                                                                            */
/*   ASSIGN                                                                   */
/*        lcWhere = 'for each tmp2artbas no-lock by Beskr indexed-reposition' */
/*       .                                                                    */
/*   ASSIGN bOk = FALSE.                                                      */
/*   MESSAGE "Skal annonseflagget på de valgte artikklene nullstilles?"       */
/*       VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO                            */
/*       UPDATE bOk.                                                          */
/*   IF bOk <> TRUE THEN                                                      */
/*       RETURN NO-APPLY.                                                     */
/*                                                                            */
/*   {sww.i}                                                                  */
/*   RUN blankannonseflagg.p (lcWhere,'tmp2ArtBas').                          */
/*   {swn.i}                                                                  */
/*                                                                            */
/*   DYNAMIC-FUNCTION('openQuery':U IN h_dtmpartbas).                         */
/*                                                                            */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SettFarge wWin 
PROCEDURE SettFarge :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cArtNrList AS CHAR NO-UNDO.


iReturn = 0.
RUN d-settfarge.w   ("Endre farge på artikler i utvalg",
                            hBrowse:NUM-SELECTED-ROWS,
                            STRING(DYNAMIC-FUNCTION("getRecordCount" IN h_dtmpartbas)),
                            OUTPUT iReturn,
                            OUTPUT cValues).
IF iReturn = 0 THEN RETURN.

IF hBrowse:NUM-SELECTED-ROWS > 0 AND iReturn = 2 THEN DO:
  DO ix = 1 TO hBrowse:NUM-SELECTED-ROWS:
    IF hBrowse:FETCH-SELECTED-ROW(ix) THEN 
      cArtNrList = cArtNrList +
                        STRING(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE) + ",". 
  END.
  IF NOT DYNAMIC-FUNCTION("RunProc","art_sett_farge.p",
                          cValues + ",ARTNUM," + TRIM(cArtNrList,","),
                          ?) THEN 
    DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Advarsel","").
END.
ELSE IF NOT DYNAMIC-FUNCTION("RunProc","art_sett_farge.p",cValues,DYNAMIC-FUNCTION("getTmpArtBasHandle" IN h_dtmpartbas)) THEN 
  DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Advarsel","").


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SettGjFaktureres wWin 
PROCEDURE SettGjFaktureres :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cArtNrList AS CHAR NO-UNDO.


iReturn = 0.
RUN d-SettGjFaktureres.w   ("Sett flagg gjennomfaktureres kjedekontor",
                            hBrowse:NUM-SELECTED-ROWS,
                            STRING(DYNAMIC-FUNCTION("getRecordCount" IN h_dtmpartbas)),
                            OUTPUT iReturn,
                            OUTPUT bOK).
IF iReturn = 0 THEN RETURN.

IF hBrowse:NUM-SELECTED-ROWS > 0 AND iReturn = 2 THEN DO:
  DO ix = 1 TO hBrowse:NUM-SELECTED-ROWS:
    IF hBrowse:FETCH-SELECTED-ROW(ix) THEN 
      cArtNrList = cArtNrList +
                        STRING(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE) + ",". 
  END.
  IF NOT DYNAMIC-FUNCTION("RunProc","art_sett_gjfaktureres.p",
                          STRING(bOk) + ",ARTNUM," + TRIM(cArtNrList,","),
                          ?) THEN 
    DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Advarsel","").
END.
ELSE IF NOT DYNAMIC-FUNCTION("RunProc","art_sett_gjfaktureres.p",STRING(bOk),DYNAMIC-FUNCTION("getTmpArtBasHandle" IN h_dtmpartbas)) THEN 
  DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Advarsel","").


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SettGrunnsortiment wWin 
PROCEDURE SettGrunnsortiment :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cArtNrList AS CHAR NO-UNDO.


iReturn = 0.
RUN d-SettHoyLav.w   ("Sett grunnsortiment for artikler",
                            hBrowse:NUM-SELECTED-ROWS,
                            STRING(DYNAMIC-FUNCTION("getRecordCount" IN h_dtmpartbas)),
                            OUTPUT iReturn,
                            OUTPUT bOK).
IF iReturn = 0 THEN RETURN.

IF hBrowse:NUM-SELECTED-ROWS > 0 AND iReturn = 2 THEN DO:
  DO ix = 1 TO hBrowse:NUM-SELECTED-ROWS:
    IF hBrowse:FETCH-SELECTED-ROW(ix) THEN 
      cArtNrList = cArtNrList +
                   STRING(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE) + ",". 
  END.
  IF NOT DYNAMIC-FUNCTION("RunProc","art_sett_grunnsortiment.p",
                          STRING(bOk) + ",ARTNUM," + TRIM(cArtNrList,","),
                          ?) THEN 
    DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Advarsel","").
END.
ELSE IF NOT DYNAMIC-FUNCTION("RunProc","art_sett_grunnsortiment.p",STRING(bOk),DYNAMIC-FUNCTION("getTmpArtBasHandle" IN h_dtmpartbas)) THEN 
  DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Advarsel","").


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SettHovedkategori wWin 
PROCEDURE SettHovedkategori :
/*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:                                                                                                                                            
        ------------------------------------------------------------------------------*/
DEF VAR cArtNrList AS CHAR NO-UNDO.


iReturn = 0.
  RUN d-settHovedOgUnderkategori.w
                          ("Endre hoved og underkategori på artikler",
                            hBrowse:NUM-SELECTED-ROWS,
                            STRING(DYNAMIC-FUNCTION("getRecordCount" IN h_dtmpartbas)),
                            OUTPUT iReturn,
                            OUTPUT cValues).
IF iReturn = 0 THEN RETURN.

IF hBrowse:NUM-SELECTED-ROWS > 0 AND iReturn = 2 THEN DO:
  DO ix = 1 TO hBrowse:NUM-SELECTED-ROWS:
    IF hBrowse:FETCH-SELECTED-ROW(ix) THEN 
      cArtNrList = cArtNrList +
                        STRING(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE) + ",". 
  END.
  IF NOT DYNAMIC-FUNCTION("RunProc","art_sett_HovedOgUnderkategori.p",
                          cValues + ",ARTNUM," + TRIM(cArtNrList,","),
                          ?) THEN 
    DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Advarsel","").
END.
ELSE IF NOT DYNAMIC-FUNCTION("RunProc","art_sett_HovedOgUnderkategori.p",cValues,DYNAMIC-FUNCTION("getTmpArtBasHandle" IN h_dtmpartbas)) THEN 
  DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Advarsel","").




END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SettHoyLavMva wWin 
PROCEDURE SettHoyLavMva :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cArtNrList AS CHAR NO-UNDO.


iReturn = 0.
RUN d-SettHoyLav.w   ("Sett høy/lav mvasats for artikler",
                            hBrowse:NUM-SELECTED-ROWS,
                            STRING(DYNAMIC-FUNCTION("getRecordCount" IN h_dtmpartbas)),
                            OUTPUT iReturn,
                            OUTPUT bOK).
IF iReturn = 0 THEN RETURN.

IF hBrowse:NUM-SELECTED-ROWS > 0 AND iReturn = 2 THEN DO:
  DO ix = 1 TO hBrowse:NUM-SELECTED-ROWS:
    IF hBrowse:FETCH-SELECTED-ROW(ix) THEN 
      cArtNrList = cArtNrList +
                   STRING(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE) + ",". 
  END.
  IF NOT DYNAMIC-FUNCTION("RunProc","art_sett_hoylav.p",
                          STRING(bOk) + ",ARTNUM," + TRIM(cArtNrList,","),
                          ?) THEN 
    DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Advarsel","").
END.
ELSE IF NOT DYNAMIC-FUNCTION("RunProc","art_sett_hoylav.p",STRING(bOk),DYNAMIC-FUNCTION("getTmpArtBasHandle" IN h_dtmpartbas)) THEN 
  DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Advarsel","").


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SettKampanjekode wWin 
PROCEDURE SettKampanjekode :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cArtNrList AS CHAR NO-UNDO.


iReturn = 0.
RUN d-settkampanjekode.w   ("Endre kampanjekode på artikler i utvalg",
                            hBrowse:NUM-SELECTED-ROWS,
                            STRING(DYNAMIC-FUNCTION("getRecordCount" IN h_dtmpartbas)),
                            OUTPUT iReturn,
                            OUTPUT cValues).
IF iReturn = 0 THEN RETURN.

IF cValues = '?' THEN
DO:
    MESSAGE "Kampanjekode er ikke angitt."
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN.
END.

IF hBrowse:NUM-SELECTED-ROWS > 0 AND iReturn = 2 THEN DO:
  DO ix = 1 TO hBrowse:NUM-SELECTED-ROWS:
    IF hBrowse:FETCH-SELECTED-ROW(ix) THEN 
      cArtNrList = cArtNrList +
                        STRING(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE) + ",". 
  END.
  IF NOT DYNAMIC-FUNCTION("RunProc","art_sett_kampanjekode.p",
                          cValues + ",ARTNUM," + TRIM(cArtNrList,","),
                          ?) THEN 
    DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Advarsel","").
END.
ELSE IF NOT DYNAMIC-FUNCTION("RunProc","art_sett_kampanjekode.p",cValues,DYNAMIC-FUNCTION("getTmpArtBasHandle" IN h_dtmpartbas)) THEN 
  DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Advarsel","").


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SettKjedevare wWin 
PROCEDURE SettKjedevare :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cArtNrList AS CHAR NO-UNDO.


iReturn = 0.
RUN d-SettKjedevareFlagg.w   ("Sett kjedevareflagg for artikler",
                            hBrowse:NUM-SELECTED-ROWS,
                            STRING(DYNAMIC-FUNCTION("getRecordCount" IN h_dtmpartbas)),
                            OUTPUT iReturn,
                            OUTPUT bOK).
IF iReturn = 0 THEN RETURN.

IF hBrowse:NUM-SELECTED-ROWS > 0 AND iReturn = 2 THEN DO:
  DO ix = 1 TO hBrowse:NUM-SELECTED-ROWS:
    IF hBrowse:FETCH-SELECTED-ROW(ix) THEN 
      cArtNrList = cArtNrList +
                   STRING(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE) + ",". 
  END.
  IF NOT DYNAMIC-FUNCTION("RunProc","art_sett_kjedevare.p",
                          STRING(bOk) + ",ARTNUM," + TRIM(cArtNrList,","),
                          ?) THEN 
    DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Advarsel","").
END.
ELSE IF NOT DYNAMIC-FUNCTION("RunProc","art_sett_kjedevare.p",STRING(bOk),DYNAMIC-FUNCTION("getTmpArtBasHandle" IN h_dtmpartbas)) THEN 
  DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Advarsel","").


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SettKjokkenskriver wWin 
PROCEDURE SettKjokkenskriver :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cArtNrList AS CHAR NO-UNDO.


iReturn = 0.
RUN d-settKjokkenskriver.w   ("Endre kjøkkenskriver på artikler i utvalg",
                            hBrowse:NUM-SELECTED-ROWS,
                            STRING(DYNAMIC-FUNCTION("getRecordCount" IN h_dtmpartbas)),
                            OUTPUT iReturn,
                            OUTPUT cValues).
IF iReturn = 0 THEN RETURN.

/*
IF DEC(cValues) = '0' THEN
DO:
    MESSAGE "Lokasjonskode er ikke angitt."
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN.
END.
*/

IF hBrowse:NUM-SELECTED-ROWS > 0 AND iReturn = 2 THEN DO:
  DO ix = 1 TO hBrowse:NUM-SELECTED-ROWS:
    IF hBrowse:FETCH-SELECTED-ROW(ix) THEN 
      cArtNrList = cArtNrList +
                        STRING(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE) + ",". 
  END.
  IF NOT DYNAMIC-FUNCTION("RunProc","art_sett_kjokkenskriver.p",
                          cValues + ",ARTNUM," + TRIM(cArtNrList,","),
                          ?) THEN 
    DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Advarsel","").
END.
ELSE IF NOT DYNAMIC-FUNCTION("RunProc","art_sett_kjokkenskriver.p",cValues,DYNAMIC-FUNCTION("getTmpArtBasHandle" IN h_dtmpartbas)) THEN 
  DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Advarsel","").


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SettKunderabatt wWin 
PROCEDURE SettKunderabatt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cArtNrList AS CHAR NO-UNDO.


iReturn = 0.
RUN d-SettKundeRabatt.w   ("Aktiver kunderabatt i kasse",
                            hBrowse:NUM-SELECTED-ROWS,
                            STRING(DYNAMIC-FUNCTION("getRecordCount" IN h_dtmpartbas)),
                            OUTPUT iReturn,
                            OUTPUT bOK).
IF iReturn = 0 THEN RETURN.

IF hBrowse:NUM-SELECTED-ROWS > 0 AND iReturn = 2 THEN DO:
  DO ix = 1 TO hBrowse:NUM-SELECTED-ROWS:
    IF hBrowse:FETCH-SELECTED-ROW(ix) THEN 
      cArtNrList = cArtNrList +
                        STRING(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE) + ",". 
  END.
  IF NOT DYNAMIC-FUNCTION("RunProc","art_sett_KundeRabatt.p",
                          STRING(bOk) + ",ARTNUM," + TRIM(cArtNrList,","),
                          ?) THEN 
    DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Advarsel","").
END.
ELSE IF NOT DYNAMIC-FUNCTION("RunProc","art_sett_KundeRabatt.p",STRING(bOk),DYNAMIC-FUNCTION("getTmpArtBasHandle" IN h_dtmpartbas)) THEN 
  DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Advarsel","").


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SettLeverandor wWin 
PROCEDURE SettLeverandor :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cArtNrList AS CHAR NO-UNDO.


iReturn = 0.
RUN d-settleverandor.w   ("Endre leverandør artikler i utvalg",
                            hBrowse:NUM-SELECTED-ROWS,
                            STRING(DYNAMIC-FUNCTION("getRecordCount" IN h_dtmpartbas)),
                            OUTPUT iReturn,
                            OUTPUT cValues).
IF iReturn = 0 THEN RETURN.

IF hBrowse:NUM-SELECTED-ROWS > 0 AND iReturn = 2 THEN DO:
  DO ix = 1 TO hBrowse:NUM-SELECTED-ROWS:
    IF hBrowse:FETCH-SELECTED-ROW(ix) THEN 
      cArtNrList = cArtNrList +
                        STRING(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE) + ",". 
  END.
  IF NOT DYNAMIC-FUNCTION("RunProc","art_sett_leverandor.p",
                          cValues + ",ARTNUM," + TRIM(cArtNrList,","),
                          ?) THEN 
    DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Advarsel","").
END.
ELSE IF NOT DYNAMIC-FUNCTION("RunProc","art_sett_leverandor.p",cValues,DYNAMIC-FUNCTION("getTmpArtBasHandle" IN h_dtmpartbas)) THEN 
  DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Advarsel","").


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SettLokasjon wWin 
PROCEDURE SettLokasjon :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cArtNrList AS CHAR NO-UNDO.


iReturn = 0.
RUN d-settlokasjon.w   ("Endre lokasjon på artikler i utvalg",
                            hBrowse:NUM-SELECTED-ROWS,
                            STRING(DYNAMIC-FUNCTION("getRecordCount" IN h_dtmpartbas)),
                            OUTPUT iReturn,
                            OUTPUT cValues).
IF iReturn = 0 THEN RETURN.

IF cValues = '?' THEN
DO:
    MESSAGE "Lokasjonskode er ikke angitt."
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN.
END.

IF hBrowse:NUM-SELECTED-ROWS > 0 AND iReturn = 2 THEN DO:
  DO ix = 1 TO hBrowse:NUM-SELECTED-ROWS:
    IF hBrowse:FETCH-SELECTED-ROW(ix) THEN 
      cArtNrList = cArtNrList +
                        STRING(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE) + ",". 
  END.
  IF NOT DYNAMIC-FUNCTION("RunProc","art_sett_lokasjon.p",
                          cValues + ",ARTNUM," + TRIM(cArtNrList,","),
                          ?) THEN 
    DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Advarsel","").
END.
ELSE IF NOT DYNAMIC-FUNCTION("RunProc","art_sett_lokasjon.p",cValues,DYNAMIC-FUNCTION("getTmpArtBasHandle" IN h_dtmpartbas)) THEN 
  DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Advarsel","").


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SettLopenummer wWin 
PROCEDURE SettLopenummer :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME DEFAULT-FRAME:
  FIND ArtBas WHERE ArtBas.ArtikkelNr = DECI(DYNAMIC-FUNCTION('columnValue':U IN h_dtmpartbas,
        INPUT "Artikkelnr")) NO-LOCK NO-ERROR.
  IF NOT AVAILABLE ArtBas THEN
        RETURN NO-APPLY.
  RUN d-vtildelopnr.w (INPUT RECID(ArtBas)).  
  IF RETURN-VALUE = "AVBRYT" THEN
    RETURN NO-APPLY.
  RUN refreshBrowse IN h_btmpartbas.

END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SettManRabIKasse wWin 
PROCEDURE SettManRabIKasse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cArtNrList AS CHAR NO-UNDO.


iReturn = 0.
RUN d-SettManRabIKasse.w   ("Sett flagg manuell rabatt i kasse",
                            hBrowse:NUM-SELECTED-ROWS,
                            STRING(DYNAMIC-FUNCTION("getRecordCount" IN h_dtmpartbas)),
                            OUTPUT iReturn,
                            OUTPUT bOK).
IF iReturn = 0 THEN RETURN.

IF hBrowse:NUM-SELECTED-ROWS > 0 AND iReturn = 2 THEN DO:
  DO ix = 1 TO hBrowse:NUM-SELECTED-ROWS:
    IF hBrowse:FETCH-SELECTED-ROW(ix) THEN 
      cArtNrList = cArtNrList +
                        STRING(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE) + ",". 
  END.
  IF NOT DYNAMIC-FUNCTION("RunProc","art_sett_manrabikasse.p",
                          STRING(bOk) + ",ARTNUM," + TRIM(cArtNrList,","),
                          ?) THEN 
    DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Advarsel","").
END.
ELSE IF NOT DYNAMIC-FUNCTION("RunProc","art_sett_manrabikasse.p",STRING(bOk),DYNAMIC-FUNCTION("getTmpArtBasHandle" IN h_dtmpartbas)) THEN 
  DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Advarsel","").


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SettMaterial wWin 
PROCEDURE SettMaterial :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cArtNrList AS CHAR NO-UNDO.


iReturn = 0.
RUN d-settmaterial.w   ("Endre material på artikler i utvalg",
                            hBrowse:NUM-SELECTED-ROWS,
                            STRING(DYNAMIC-FUNCTION("getRecordCount" IN h_dtmpartbas)),
                            OUTPUT iReturn,
                            OUTPUT cValues).
IF iReturn = 0 THEN RETURN.

IF hBrowse:NUM-SELECTED-ROWS > 0 AND iReturn = 2 THEN DO:
  DO ix = 1 TO hBrowse:NUM-SELECTED-ROWS:
    IF hBrowse:FETCH-SELECTED-ROW(ix) THEN 
      cArtNrList = cArtNrList +
                        STRING(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE) + ",". 
  END.
  IF NOT DYNAMIC-FUNCTION("RunProc","art_sett_material.p",
                          cValues + ",ARTNUM," + TRIM(cArtNrList,","),
                          ?) THEN 
    DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Advarsel","").
END.
ELSE IF NOT DYNAMIC-FUNCTION("RunProc","art_sett_material.p",cValues,DYNAMIC-FUNCTION("getTmpArtBasHandle" IN h_dtmpartbas)) THEN 
  DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Advarsel","").


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SettMedlemsutbytte wWin 
PROCEDURE SettMedlemsutbytte :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cArtNrList AS CHAR NO-UNDO.


iReturn = 0.
RUN d-SettMedlemsutbytte.w ("Sett flagg medlemsutbytte",
                            hBrowse:NUM-SELECTED-ROWS,
                            STRING(DYNAMIC-FUNCTION("getRecordCount" IN h_dtmpartbas)),
                            OUTPUT iReturn,
                            OUTPUT bOK).
IF iReturn = 0 THEN RETURN.

IF hBrowse:NUM-SELECTED-ROWS > 0 AND iReturn = 2 THEN DO:
  DO ix = 1 TO hBrowse:NUM-SELECTED-ROWS:
    IF hBrowse:FETCH-SELECTED-ROW(ix) THEN 
      cArtNrList = cArtNrList +
                        STRING(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE) + ",". 
  END.
  IF NOT DYNAMIC-FUNCTION("RunProc","art_sett_medlemsutbytte.p",
                          STRING(bOk) + ",ARTNUM," + TRIM(cArtNrList,","),
                          ?) THEN 
    DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Advarsel","").
END.
ELSE IF NOT DYNAMIC-FUNCTION("RunProc","art_sett_medlemsutbytte.p",STRING(bOk),DYNAMIC-FUNCTION("getTmpArtBasHandle" IN h_dtmpartbas)) THEN 
  DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Advarsel","").


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SettPostPakkeInfo wWin 
PROCEDURE SettPostPakkeInfo :
/*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:                                                                                                                                            
        ------------------------------------------------------------------------------*/
DEF VAR cArtNrList AS CHAR NO-UNDO.


iReturn = 0.
  RUN d-settPostPakkeInfo.w   ("Endre postpakkeinformasjon på artikler",
                            hBrowse:NUM-SELECTED-ROWS,
                            STRING(DYNAMIC-FUNCTION("getRecordCount" IN h_dtmpartbas)),
                            OUTPUT iReturn,
                            OUTPUT cValues).
IF iReturn = 0 THEN RETURN.

IF hBrowse:NUM-SELECTED-ROWS > 0 AND iReturn = 2 THEN DO:
  DO ix = 1 TO hBrowse:NUM-SELECTED-ROWS:
    IF hBrowse:FETCH-SELECTED-ROW(ix) THEN 
      cArtNrList = cArtNrList +
                        STRING(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE) + ",". 
  END.
  IF NOT DYNAMIC-FUNCTION("RunProc","art_sett_PostPakkeInfo.p",
                          cValues + ",ARTNUM," + TRIM(cArtNrList,","),
                          ?) THEN 
    DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Advarsel","").
END.
ELSE IF NOT DYNAMIC-FUNCTION("RunProc","art_sett_PostPakkeInfo.p",cValues,DYNAMIC-FUNCTION("getTmpArtBasHandle" IN h_dtmpartbas)) THEN 
  DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Advarsel","").




END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SettProdusent wWin 
PROCEDURE SettProdusent :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cArtNrList AS CHAR NO-UNDO.


iReturn = 0.
RUN d-settprodusent.w   ("Endre produsent på artikler i utvalg",
                            hBrowse:NUM-SELECTED-ROWS,
                            STRING(DYNAMIC-FUNCTION("getRecordCount" IN h_dtmpartbas)),
                            OUTPUT iReturn,
                            OUTPUT cValues).
IF iReturn = 0 THEN RETURN.

IF hBrowse:NUM-SELECTED-ROWS > 0 AND iReturn = 2 THEN DO:
  DO ix = 1 TO hBrowse:NUM-SELECTED-ROWS:
    IF hBrowse:FETCH-SELECTED-ROW(ix) THEN 
      cArtNrList = cArtNrList +
                        STRING(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE) + ",". 
  END.
  IF NOT DYNAMIC-FUNCTION("RunProc","art_sett_produsent.p",
                          cValues + ",ARTNUM," + TRIM(cArtNrList,","),
                          ?) THEN 
    DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Advarsel","").
END.
ELSE IF NOT DYNAMIC-FUNCTION("RunProc","art_sett_produsent.p",cValues,DYNAMIC-FUNCTION("getTmpArtBasHandle" IN h_dtmpartbas)) THEN 
  DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Advarsel","").


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SettRabattGivende wWin 
PROCEDURE SettRabattGivende :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cArtNrList AS CHAR NO-UNDO.


iReturn = 0.
RUN d-SettRabattGivende.w   ("Sett rabattgrunnlag for artikler",
                            hBrowse:NUM-SELECTED-ROWS,
                            STRING(DYNAMIC-FUNCTION("getRecordCount" IN h_dtmpartbas)),
                            OUTPUT iReturn,
                            OUTPUT bOK).
IF iReturn = 0 THEN RETURN.

IF hBrowse:NUM-SELECTED-ROWS > 0 AND iReturn = 2 THEN DO:
  DO ix = 1 TO hBrowse:NUM-SELECTED-ROWS:
    IF hBrowse:FETCH-SELECTED-ROW(ix) THEN 
      cArtNrList = cArtNrList +
                   STRING(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE) + ",". 
  END.
  IF NOT DYNAMIC-FUNCTION("RunProc","art_sett_rabattgivende.p",
                          STRING(bOk) + ",ARTNUM," + TRIM(cArtNrList,","),
                          ?) THEN 
    DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Advarsel","").
END.
ELSE IF NOT DYNAMIC-FUNCTION("RunProc","art_sett_rabattgivende.p",STRING(bOk),DYNAMIC-FUNCTION("getTmpArtBasHandle" IN h_dtmpartbas)) THEN 
  DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Advarsel","").


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SettRAvdNr wWin 
PROCEDURE SettRAvdNr :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cArtNrList AS CHAR NO-UNDO.


iReturn = 0.
RUN d-settravdnr.w   ("Endre regnskaps.avd.nr på artikler i utvalg",
                            hBrowse:NUM-SELECTED-ROWS,
                            STRING(DYNAMIC-FUNCTION("getRecordCount" IN h_dtmpartbas)),
                            OUTPUT iReturn,
                            OUTPUT cValues).
IF iReturn = 0 THEN RETURN.

IF hBrowse:NUM-SELECTED-ROWS > 0 AND iReturn = 2 THEN DO:
  DO ix = 1 TO hBrowse:NUM-SELECTED-ROWS:
    IF hBrowse:FETCH-SELECTED-ROW(ix) THEN 
      cArtNrList = cArtNrList +
                        STRING(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE) + ",". 
  END.
  IF NOT DYNAMIC-FUNCTION("RunProc","art_sett_ravdnr.p",
                          cValues + ",ARTNUM," + TRIM(cArtNrList,","),
                          ?) THEN 
    DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Advarsel","").
END.
ELSE IF NOT DYNAMIC-FUNCTION("RunProc","art_sett_ravdnr.p",cValues,DYNAMIC-FUNCTION("getTmpArtBasHandle" IN h_dtmpartbas)) THEN 
  DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Advarsel","").


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SettSalgsenhet wWin 
PROCEDURE SettSalgsenhet :
/*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:                                                                                                                                            
        ------------------------------------------------------------------------------*/
DEF VAR cArtNrList AS CHAR NO-UNDO.


iReturn = 0.
RUN d-settSalgsenhet.w   ("Endre salgsenhet på artikler i utvalg",
                            hBrowse:NUM-SELECTED-ROWS,
                            STRING(DYNAMIC-FUNCTION("getRecordCount" IN h_dtmpartbas)),
                            OUTPUT iReturn,
                            OUTPUT cValues).
IF iReturn = 0 THEN RETURN.

IF hBrowse:NUM-SELECTED-ROWS > 0 AND iReturn = 2 THEN DO:
  DO ix = 1 TO hBrowse:NUM-SELECTED-ROWS:
    IF hBrowse:FETCH-SELECTED-ROW(ix) THEN 
      cArtNrList = cArtNrList +
                        STRING(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE) + ",". 
  END.
  IF NOT DYNAMIC-FUNCTION("RunProc","art_sett_salgsenhet.p",
                          cValues + ",ARTNUM," + TRIM(cArtNrList,","),
                          ?) THEN 
    DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Advarsel","").
END.
ELSE IF NOT DYNAMIC-FUNCTION("RunProc","art_sett_salgsenhet.p",cValues,DYNAMIC-FUNCTION("getTmpArtBasHandle" IN h_dtmpartbas)) THEN 
  DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Advarsel","").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SettSesong wWin 
PROCEDURE SettSesong :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cArtNrList AS CHAR NO-UNDO.


iReturn = 0.
RUN d-settsesong.w   ("Endre sesong på artikler i utvalg",
                            hBrowse:NUM-SELECTED-ROWS,
                            STRING(DYNAMIC-FUNCTION("getRecordCount" IN h_dtmpartbas)),
                            OUTPUT iReturn,
                            OUTPUT cValues).
IF iReturn = 0 THEN RETURN.

IF hBrowse:NUM-SELECTED-ROWS > 0 AND iReturn = 2 THEN DO:
  DO ix = 1 TO hBrowse:NUM-SELECTED-ROWS:
    IF hBrowse:FETCH-SELECTED-ROW(ix) THEN 
      cArtNrList = cArtNrList +
                        STRING(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE) + ",". 
  END.
  IF NOT DYNAMIC-FUNCTION("RunProc","art_sett_sasong.p",
                          cValues + ",ARTNUM," + TRIM(cArtNrList,","),
                          ?) THEN 
    DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Advarsel","").
END.
ELSE IF NOT DYNAMIC-FUNCTION("RunProc","art_sett_sasong.p",cValues,DYNAMIC-FUNCTION("getTmpArtBasHandle" IN h_dtmpartbas)) THEN 
  DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Advarsel","").


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SettStrType wWin 
PROCEDURE SettStrType :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cArtNrList AS CHAR NO-UNDO.


iReturn = 0.
RUN d-settstrtype.w   ("Endre størrelsestype på artikler i utvalg",
                            hBrowse:NUM-SELECTED-ROWS,
                            STRING(DYNAMIC-FUNCTION("getRecordCount" IN h_dtmpartbas)),
                            OUTPUT iReturn,
                            OUTPUT cValues).
IF iReturn = 0 THEN RETURN.

IF hBrowse:NUM-SELECTED-ROWS > 0 AND iReturn = 2 THEN DO:
  DO ix = 1 TO hBrowse:NUM-SELECTED-ROWS:
    IF hBrowse:FETCH-SELECTED-ROW(ix) THEN 
      cArtNrList = cArtNrList +
                        STRING(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE) + ",". 
  END.
  IF NOT DYNAMIC-FUNCTION("RunProc","art_sett_strtype.p",
                          cValues + ",ARTNUM," + TRIM(cArtNrList,","),
                          ?) THEN 
    DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Advarsel","").
END.
ELSE IF NOT DYNAMIC-FUNCTION("RunProc","art_sett_strtype.p",cValues,DYNAMIC-FUNCTION("getTmpArtBasHandle" IN h_dtmpartbas)) THEN 
  DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Advarsel","").


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SettUtgatt wWin 
PROCEDURE SettUtgatt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cArtNrList AS CHAR NO-UNDO.


iReturn = 0.
RUN d-SettUtgattFlagg.w   ("Sett utgåttflagg for artikler",
                            hBrowse:NUM-SELECTED-ROWS,
                            STRING(DYNAMIC-FUNCTION("getRecordCount" IN h_dtmpartbas)),
                            OUTPUT iReturn,
                            OUTPUT bOK).
IF iReturn = 0 THEN RETURN.

IF hBrowse:NUM-SELECTED-ROWS > 0 AND iReturn = 2 THEN DO:
  DO ix = 1 TO hBrowse:NUM-SELECTED-ROWS:
    IF hBrowse:FETCH-SELECTED-ROW(ix) THEN 
      cArtNrList = cArtNrList +
                   STRING(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE) + ",". 
  END.
  IF NOT DYNAMIC-FUNCTION("RunProc","art_sett_utgatt.p",
                          STRING(bOk) + ",ARTNUM," + TRIM(cArtNrList,","),
                          ?) THEN 
    DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Advarsel","").
END.
ELSE IF NOT DYNAMIC-FUNCTION("RunProc","art_sett_utgatt.p",STRING(bOk),DYNAMIC-FUNCTION("getTmpArtBasHandle" IN h_dtmpartbas)) THEN 
  DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Advarsel","").


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SettVaregruppe wWin 
PROCEDURE SettVaregruppe :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cArtNrList AS CHAR NO-UNDO.


iReturn = 0.
RUN d-settvaregruppe.w   ("Endre varegruppe på artikler i utvalg",
                            hBrowse:NUM-SELECTED-ROWS,
                            STRING(DYNAMIC-FUNCTION("getRecordCount" IN h_dtmpartbas)),
                            OUTPUT iReturn,
                            OUTPUT cValues).
IF iReturn = 0 THEN RETURN.

IF hBrowse:NUM-SELECTED-ROWS > 0 AND iReturn = 2 THEN DO:
  DO ix = 1 TO hBrowse:NUM-SELECTED-ROWS:
    IF hBrowse:FETCH-SELECTED-ROW(ix) THEN 
      cArtNrList = cArtNrList +
                        STRING(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE) + ",". 
  END.
  IF NOT DYNAMIC-FUNCTION("RunProc","art_sett_varegruppe.p",
                          cValues + ",ARTNUM," + TRIM(cArtNrList,","),
                          ?) THEN 
    DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Advarsel","").
END.
ELSE IF NOT DYNAMIC-FUNCTION("RunProc","art_sett_varegruppe.p",cValues,DYNAMIC-FUNCTION("getTmpArtBasHandle" IN h_dtmpartbas)) THEN 
  DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Advarsel","").


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SettVareIKasse wWin 
PROCEDURE SettVareIKasse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cArtNrList AS CHAR NO-UNDO.


iReturn = 0.
RUN d-SettVareIKasse.w   ("Sett flagg Vare i kasse",
                            hBrowse:NUM-SELECTED-ROWS,
                            STRING(DYNAMIC-FUNCTION("getRecordCount" IN h_dtmpartbas)),
                            OUTPUT iReturn,
                            OUTPUT bOK).
IF iReturn = 0 THEN RETURN.

IF hBrowse:NUM-SELECTED-ROWS > 0 AND iReturn = 2 THEN DO:
  DO ix = 1 TO hBrowse:NUM-SELECTED-ROWS:
    IF hBrowse:FETCH-SELECTED-ROW(ix) THEN 
      cArtNrList = cArtNrList +
                        STRING(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE) + ",". 
  END.
  IF NOT DYNAMIC-FUNCTION("RunProc","art_sett_vare_i_kasse.p",
                          STRING(bOk) + ",ARTNUM," + TRIM(cArtNrList,","),
                          ?) THEN 
    DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Advarsel","").
END.
ELSE IF NOT DYNAMIC-FUNCTION("RunProc","art_sett_vare_i_kasse.p",STRING(bOk),DYNAMIC-FUNCTION("getTmpArtBasHandle" IN h_dtmpartbas)) THEN 
  DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Advarsel","").


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SettVaremerke wWin 
PROCEDURE SettVaremerke :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cArtNrList AS CHAR NO-UNDO.


iReturn = 0.
RUN d-settvaremerke.w   ("Endre varemerke på artikler i utvalg",
                            hBrowse:NUM-SELECTED-ROWS,
                            STRING(DYNAMIC-FUNCTION("getRecordCount" IN h_dtmpartbas)),
                            OUTPUT iReturn,
                            OUTPUT cValues).
IF iReturn = 0 THEN RETURN.

IF hBrowse:NUM-SELECTED-ROWS > 0 AND iReturn = 2 THEN DO:
  DO ix = 1 TO hBrowse:NUM-SELECTED-ROWS:
    IF hBrowse:FETCH-SELECTED-ROW(ix) THEN 
      cArtNrList = cArtNrList +
                        STRING(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE) + ",". 
  END.
  IF NOT DYNAMIC-FUNCTION("RunProc","art_sett_varemerke.p",
                          cValues + ",ARTNUM," + TRIM(cArtNrList,","),
                          ?) THEN 
    DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Advarsel","").
END.
ELSE IF NOT DYNAMIC-FUNCTION("RunProc","art_sett_varemerke.p",cValues,DYNAMIC-FUNCTION("getTmpArtBasHandle" IN h_dtmpartbas)) THEN 
  DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Advarsel","").


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SettVaretype wWin 
PROCEDURE SettVaretype :
/*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:                                                                                                                                            
        ------------------------------------------------------------------------------*/
DEF VAR cArtNrList AS CHAR NO-UNDO.


iReturn = 0.
  RUN d-settVaretype.w   ("Endre varetype på artikler i utvalg",
                            hBrowse:NUM-SELECTED-ROWS,
                            STRING(DYNAMIC-FUNCTION("getRecordCount" IN h_dtmpartbas)),
                            OUTPUT iReturn,
                            OUTPUT cValues).
IF iReturn = 0 THEN RETURN.

IF hBrowse:NUM-SELECTED-ROWS > 0 AND iReturn = 2 THEN DO:
  DO ix = 1 TO hBrowse:NUM-SELECTED-ROWS:
    IF hBrowse:FETCH-SELECTED-ROW(ix) THEN 
      cArtNrList = cArtNrList +
                        STRING(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE) + ",". 
  END.
  IF NOT DYNAMIC-FUNCTION("RunProc","art_sett_varetype.p",
                          cValues + ",ARTNUM," + TRIM(cArtNrList,","),
                          ?) THEN 
    DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Advarsel","").
END.
ELSE IF NOT DYNAMIC-FUNCTION("RunProc","art_sett_varetype.p",cValues,DYNAMIC-FUNCTION("getTmpArtBasHandle" IN h_dtmpartbas)) THEN 
  DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Advarsel","").


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SettWebArtikkelButikk wWin 
PROCEDURE SettWebArtikkelButikk :
/*------------------------------------------------------------------------------
  Purpose:   Koppla butik till webartikkel  
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF VAR cWebButikkArtikkelArtNrList AS CHAR NO-UNDO.
DEF VAR cTekst                      AS CHAR NO-UNDO.


iReturn = 0.
RUN d-SettWebArtButikk.w   ("Koble på/fra webartikkel i nettbutikk",
                            hBrowse:NUM-SELECTED-ROWS,
                            STRING(DYNAMIC-FUNCTION("getRecordCount" IN h_dtmpartbas)),
                            OUTPUT iReturn,
                            OUTPUT bOK).
cTekst = RETURN-VALUE.
IF iReturn = 0 OR bOK = FALSE THEN RETURN.

IF hBrowse:NUM-SELECTED-ROWS > 0 AND iReturn = 2 THEN DO:
  DO ix = 1 TO hBrowse:NUM-SELECTED-ROWS:
    IF hBrowse:FETCH-SELECTED-ROW(ix) THEN 
      cWebButikkArtikkelArtNrList = cWebButikkArtikkelArtNrList +
                        STRING(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE) + ",". 
  END.
  IF NOT DYNAMIC-FUNCTION("RunProc","art_sett_WebArtikkelBut.p",
                          cTekst + ",ARTNUM," + TRIM(cWebButikkArtikkelArtNrList,","),?) THEN 
        DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Advarsel","").
END.
ELSE IF NOT DYNAMIC-FUNCTION("RunProc","art_sett_WebArtikkelBut.p",cTekst,DYNAMIC-FUNCTION("getTmpArtBasHandle" IN h_dtmpartbas)) THEN 
  DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Advarsel","").


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SettWebButikkArtikkel wWin 
PROCEDURE SettWebButikkArtikkel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF VAR cWebButikkArtikkelArtNrList AS CHAR NO-UNDO.
DEF VAR cTekst                      AS CHAR NO-UNDO.


iReturn = 0.
RUN d-SettWebButFlagg.w   ("Aktiver/deaktiver artikkel i nettbutikk",
                            hBrowse:NUM-SELECTED-ROWS,
                            STRING(DYNAMIC-FUNCTION("getRecordCount" IN h_dtmpartbas)),
                            OUTPUT iReturn,
                            OUTPUT bOK).
cTekst = RETURN-VALUE.
IF iReturn = 0 THEN RETURN.

IF hBrowse:NUM-SELECTED-ROWS > 0 AND iReturn = 2 THEN DO:
  DO ix = 1 TO hBrowse:NUM-SELECTED-ROWS:
    IF hBrowse:FETCH-SELECTED-ROW(ix) THEN 
      cWebButikkArtikkelArtNrList = cWebButikkArtikkelArtNrList +
                        STRING(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE) + ",". 
  END.
  IF NOT DYNAMIC-FUNCTION("RunProc","art_sett_WebButikkArtikkel.p",
                          cTekst + ",ARTNUM," + TRIM(cWebButikkArtikkelArtNrList,","),
                          ?) THEN 
    DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Advarsel","").
END.
ELSE IF NOT DYNAMIC-FUNCTION("RunProc","art_sett_WebButikkArtikkel.p",cTekst,DYNAMIC-FUNCTION("getTmpArtBasHandle" IN h_dtmpartbas)) THEN 
  DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Advarsel","").


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SettWebLeveringstid wWin 
PROCEDURE SettWebLeveringstid :
/*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:                                                                                                                                            
        ------------------------------------------------------------------------------*/
DEF VAR cArtNrList AS CHAR NO-UNDO.


iReturn = 0.
  RUN d-settWebLeveringstid.w   ("Endre leveringstid fra nettbutikk på artikler i utvalg",
                            hBrowse:NUM-SELECTED-ROWS,
                            STRING(DYNAMIC-FUNCTION("getRecordCount" IN h_dtmpartbas)),
                            OUTPUT iReturn,
                            OUTPUT cValues).
IF iReturn = 0 THEN RETURN.

IF hBrowse:NUM-SELECTED-ROWS > 0 AND iReturn = 2 THEN DO:
  DO ix = 1 TO hBrowse:NUM-SELECTED-ROWS:
    IF hBrowse:FETCH-SELECTED-ROW(ix) THEN 
      cArtNrList = cArtNrList +
                        STRING(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE) + ",". 
  END.
  IF NOT DYNAMIC-FUNCTION("RunProc","art_sett_WebLeveringstid.p",
                          cValues + ",ARTNUM," + TRIM(cArtNrList,","),
                          ?) THEN 
    DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Advarsel","").
END.
ELSE IF NOT DYNAMIC-FUNCTION("RunProc","art_sett_WebLeveringstid.p",cValues,DYNAMIC-FUNCTION("getTmpArtBasHandle" IN h_dtmpartbas)) THEN 
  DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Advarsel","").


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SettWebMinLager wWin 
PROCEDURE SettWebMinLager :
/*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:                                                                                                                                            
        ------------------------------------------------------------------------------*/
DEF VAR cArtNrList AS CHAR NO-UNDO.


iReturn = 0.
  RUN d-settWebMinLager.w   ("Endre min.lager i nettbutikk på artikler i utvalg",
                            hBrowse:NUM-SELECTED-ROWS,
                            STRING(DYNAMIC-FUNCTION("getRecordCount" IN h_dtmpartbas)),
                            OUTPUT iReturn,
                            OUTPUT cValues).
IF iReturn = 0 THEN RETURN.

IF hBrowse:NUM-SELECTED-ROWS > 0 AND iReturn = 2 THEN DO:
  DO ix = 1 TO hBrowse:NUM-SELECTED-ROWS:
    IF hBrowse:FETCH-SELECTED-ROW(ix) THEN 
      cArtNrList = cArtNrList +
                        STRING(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE) + ",". 
  END.
  IF NOT DYNAMIC-FUNCTION("RunProc","art_sett_WebMinLager.p",
                          cValues + ",ARTNUM," + TRIM(cArtNrList,","),
                          ?) THEN 
    DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Advarsel","").
END.
ELSE IF NOT DYNAMIC-FUNCTION("RunProc","art_sett_WebMinLager.p",cValues,DYNAMIC-FUNCTION("getTmpArtBasHandle" IN h_dtmpartbas)) THEN 
  DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Advarsel","").


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SlettArtikkel wWin 
PROCEDURE SlettArtikkel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cSlettArtNrList AS CHAR NO-UNDO.

bOK = FALSE.
RUN d-SlettArtAdvarsel.w (IF hBrowse:NUM-SELECTED-ROWS > 0 THEN
                            STRING(hBrowse:NUM-SELECTED-ROWS)
                          ELSE 
                            STRING(DYNAMIC-FUNCTION("getRecordCount" IN h_dtmpartbas)),
                          OUTPUT bOk).

IF NOT bOk THEN RETURN.

iReturn = DYNAMIC-FUNCTION("DoMessage",0,4,"START SLETTING?","Dobbelt advarsel","").
IF iReturn = 6 THEN DO:
  IF hBrowse:NUM-SELECTED-ROWS > 0 THEN DO:
    DO ix = 1 TO hBrowse:NUM-SELECTED-ROWS:
      IF hBrowse:FETCH-SELECTED-ROW(ix) THEN 
        cSlettArtNrList = cSlettArtNrList +
                          STRING(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE) + ",". 
    END.
    IF NOT DYNAMIC-FUNCTION("RunProc","slett_artikkel.p",
                            "ARTNUM," + TRIM(cSlettArtNrList,","),
                            ?) THEN 
      DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Feil","").
  END.
  ELSE IF NOT DYNAMIC-FUNCTION("RunProc","slett_artikkel.p","",DYNAMIC-FUNCTION("getTmpArtBasHandle" IN h_dtmpartbas)) THEN 
    DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Feil","").
END.

RUN Nullstill.
RUN Utvalg IN h_dtmpArtBas (cCurrWhere).
DYNAMIC-FUNCTION("openQuery" IN h_dtmpartbas).

hBrowse:HELP = DYNAMIC-FUNCTION("getStatusString" IN h_dtmpartbas).
setBrowseReadOnly().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Slettbestilling wWin 
PROCEDURE Slettbestilling :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR wBestHodeRecid AS RECID NO-UNDO.
    FIND BestHode WHERE BestHode.BestNr = INT(
    DYNAMIC-FUNCTION('columnValue':U IN h_dbesthode,
              INPUT "Bestnr")) NO-LOCK NO-ERROR.
    IF NOT AVAIL BestHode THEN
        RETURN NO-APPLY.
    FIND ArtBas OF BestHode NO-LOCK NO-ERROR.
    IF NOT AVAIL ArtBas THEN
        RETURN NO-APPLY.
    ASSIGN
      wBestHodeRecid = RECID(BestHode).
    DO TRANSACTION:
      {sww.i}
      /* KanSlettes*/      
      RUN w-gridord.w (INPUT RECID(ArtBas), INPUT-OUTPUT wBestHodeRecid, "SLETT").
      DYNAMIC-FUNCTION('openQuery':U IN h_dbesthode).
      {swn.i}
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SlettFraUtvalg wWin 
PROCEDURE SlettFraUtvalg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    DO ix = 1 TO hBrowse:NUM-SELECTED-ROWS:
        IF hBrowse:FETCH-SELECTED-ROW(ix) THEN
            DYNAMIC-FUNCTION("deleteRow" IN h_dtmpartbas,STRING(hBrowse:QUERY:GET-BUFFER-HANDLE(1):ROWID)). 
    END.
    hBrowse:REFRESH().
/* IF hBrowse:NUM-SELECTED-ROWS = 1 THEN DO ix = 1 TO hBrowse:NUM-SELECTED-ROWS:                               */
/*   IF hBrowse:FETCH-SELECTED-ROW(ix) THEN                                                                    */
/*       DYNAMIC-FUNCTION("deleteRow" IN h_dtmpartbas,STRING(hBrowse:QUERY:GET-BUFFER-HANDLE(1):ROWID)).       */
/* /*       hBrowse:DELETE-SELECTED-ROW(ix). */                                                                */
/* END.                                                                                                        */
/* ELSE DO:                                                                                                    */
/*     DO ix = 1 TO hBrowse:NUM-SELECTED-ROWS:                                                                 */
/*         IF hBrowse:FETCH-SELECTED-ROW(ix) THEN                                                              */
/*             DYNAMIC-FUNCTION("deleteRow" IN h_dtmpartbas,STRING(hBrowse:QUERY:GET-BUFFER-HANDLE(1):ROWID)). */
/*     END.                                                                                                    */
/*     hBrowse:DELETE-SELECTED-ROWS().                                                                         */
/* END.                                                                                                        */

/* hBrowse:DELETE-SELECTED-ROWS(). */
hBrowse:HELP = STRING(DYNAMIC-FUNCTION("getRecordCount" IN h_dtmpartbas) + " (-" + STRING(ix - 1) + ")").

setBrowseReadOnly().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StartStatistikk wWin 
PROCEDURE StartStatistikk PRIVATE :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAMETER pcType AS CHAR NO-UNDO.

DEF VAR cSelectedRows   AS CHAR NO-UNDO.
DEF VAR hSelQuery       AS HANDLE NO-UNDO.
DEF VAR hSelBuffer      AS HANDLE NO-UNDO.


IF hBrowse:NUM-SELECTED-ROWS > 0 AND iReturn = 2 THEN DO:
  DO ix = 1 TO hBrowse:NUM-SELECTED-ROWS:
    IF hBrowse:FETCH-SELECTED-ROW(ix) THEN
      cSelectedRows = cSelectedRows + STRING(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE) + ",".
  END.
  ASSIGN hSelQuery  = DYNAMIC-FUNCTION("getQueryHandle" IN h_dtmpartbas).
         hSelBuffer = hSelQuery:GET-BUFFER-HANDLE(1)
         .
  CREATE QUERY hSelQuery.
  hSelQuery:SET-BUFFERS(hSelBuffer).
  hSelQuery:QUERY-PREPARE("FOR EACH " + hSelBuffer:NAME + " WHERE CAN-DO('" + TRIM(cSelectedRows,",") + "',STRING(ArtikkelNr))").
  hSelQuery:QUERY-OPEN().
END.

IF pcType = "TRANSLOGG" THEN DO:
    IF NOT VALID-HANDLE(hRapportPlus) THEN DO:
      RUN wRapportPlus.w PERSIST SET hRapportPlus.
      RUN InitializeObject IN hRapportPlus.
    END.

    RUN VisTranslogg IN hRapportPlus (IF hBrowse:NUM-SELECTED-ROWS > 0 AND iReturn = 2 THEN 
                                        hSelQuery 
                                      ELSE 
                                        DYNAMIC-FUNCTION("getQueryHandle" IN h_dtmpartbas),
                                      DYNAMIC-FUNCTION("getButikkIdListe" IN h_dtmpartbas), pcType).
END.
ELSE DO:
    IF NOT VALID-HANDLE(hRapportGen) THEN DO:
      RUN wrapportgen.w PERSIST SET hRapportGen.
      RUN InitializeObject IN hRapportGen.
    END.

    RUN VisLager_stat IN hRapportGen (IF hBrowse:NUM-SELECTED-ROWS > 0 AND iReturn = 2 THEN 
                                        hSelQuery 
                                      ELSE 
                                        DYNAMIC-FUNCTION("getQueryHandle" IN h_dtmpartbas),
                                      DYNAMIC-FUNCTION("getButikkIdListe" IN h_dtmpartbas), pcType).
END.

IF VALID-HANDLE(hSelQuery) THEN
  DELETE OBJECT hSelQuery.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StartUtvalg wWin 
PROCEDURE StartUtvalg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM lcWhere AS CHAR NO-UNDO.

cCurrWhere = lcWhere. 

ViewHideStopButton(TRUE).
RUN Utvalg IN h_dtmpArtBas (lcWhere).
DYNAMIC-FUNCTION('openQuery':U IN h_dtmpartbas).
glUtvalg = TRUE.
ViewHideStopButton(FALSE).

hBrowse:HELP = DYNAMIC-FUNCTION("getStatusString" IN h_dtmpartbas).
setBrowseReadOnly().
IF DYNAMIC-FUNCTION("getRecordCount" IN h_dtmpartbas) > 0 THEN
  setButtons(TRUE).
ELSE
  setButtons(FALSE).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Statistikk wWin 
PROCEDURE Statistikk :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
iReturn = 0.
RUN JBoxBrowseSelectMsg.w ("Bekreft visning av statistikk",
                            hBrowse:NUM-SELECTED-ROWS,
                            STRING(DYNAMIC-FUNCTION("getRecordCount" IN h_dtmpartbas)),
                            OUTPUT iReturn).
IF iReturn = 0 THEN RETURN.

RUN StartStatistikk ('STAT').
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Tilkasse wWin 
PROCEDURE Tilkasse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR lcWhere AS CHAR NO-UNDO.

iReturn = 0.
RUN JBoxBrowseSelectMsg.w ("Bekreft overføring av artikler til kasse",
                            hBrowse:NUM-SELECTED-ROWS,
                            STRING(DYNAMIC-FUNCTION("getRecordCount" IN h_dtmpartbas)),
                            OUTPUT iReturn).
IF iReturn = 0 THEN RETURN.

RUN ByggTmpTabell.

IF NOT CAN-FIND(FIRST tmp2ArtBas) THEN
DO:
    MESSAGE "Ingen varer i utvalg."
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN NO-APPLY.
END.

ASSIGN
     lcWhere = 'for each tmp2artbas no-lock by Beskr indexed-reposition'
    .
cEloggtyp = "". /* frågas efter i skapaElogg.p. infört för webartiklar Tilweb och Tilwebart*/
{sww.i} 
RUN skapaELogg.p (lcWhere,'tmp2ArtBas').
{swn.i} 
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Tilweb wWin 
PROCEDURE Tilweb :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR lcWhere AS CHAR NO-UNDO.

iReturn = 0.
RUN JBoxBrowseSelectMsg.w ("Bekreft overføring av artikler til kasse",
                            hBrowse:NUM-SELECTED-ROWS,
                            STRING(DYNAMIC-FUNCTION("getRecordCount" IN h_dtmpartbas)),
                            OUTPUT iReturn).
IF iReturn = 0 THEN RETURN.

RUN ByggTmpTabell.

IF NOT CAN-FIND(FIRST tmp2ArtBas) THEN
DO:
    MESSAGE "Ingen varer i utvalg."
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN NO-APPLY.
END.

ASSIGN
     lcWhere = 'for each tmp2artbas no-lock by Beskr indexed-reposition'
    .
cEloggtyp = "WEBBUT".
{sww.i} 
RUN skapaELogg.p (lcWhere,'tmp2ArtBas').
{swn.i} 
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Tilwebart wWin 
PROCEDURE Tilwebart :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR lcWhere AS CHAR NO-UNDO.

iReturn = 0.
RUN JBoxBrowseSelectMsg.w ("Bekreft overføring av artikler til kasse",
                            hBrowse:NUM-SELECTED-ROWS,
                            STRING(DYNAMIC-FUNCTION("getRecordCount" IN h_dtmpartbas)),
                            OUTPUT iReturn).
IF iReturn = 0 THEN RETURN.

RUN ByggTmpTabell.

IF NOT CAN-FIND(FIRST tmp2ArtBas) THEN
DO:
    MESSAGE "Ingen varer i utvalg."
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN NO-APPLY.
END.

ASSIGN
     lcWhere = 'for each tmp2artbas no-lock by Beskr indexed-reposition'
    .
cEloggtyp = "WEBBUTARTINFO".
{sww.i} 
RUN skapaELogg.p (lcWhere,'tmp2ArtBas').
{swn.i} 
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Translogg wWin 
PROCEDURE Translogg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

iReturn = 0.
RUN JBoxBrowseSelectMsg.w ("Bekreft visning av translogg",
                            hBrowse:NUM-SELECTED-ROWS,
                            STRING(DYNAMIC-FUNCTION("getRecordCount" IN h_dtmpartbas)),
                            OUTPUT iReturn).
IF iReturn = 0 THEN RETURN.

RUN StartStatistikk ('TRANSLOGG').

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Utvalg wWin 
PROCEDURE Utvalg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR llOk    AS LOG  NO-UNDO.
  DEF VAR lcWhere AS CHAR NO-UNDO.

  IF NOT VALID-HANDLE(h_wartbasutvalg) THEN DO:
    {sww.i} 
    RUN wartbasutvalg.w PERSIST SET h_wartbasutvalg (THIS-PROCEDURE,h_dtmpartbas).
    RUN initializeObject IN h_wartbasutvalg.
    {swn.i} 
  END.  
  RUN MoveToTop IN h_wartbasutvalg.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE VisSletteLogg wWin 
PROCEDURE VisSletteLogg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF DYNAMIC-FUNCTION("setWebDoc","open",SEARCH("slettelogg.txt")) NE "" THEN
  DYNAMIC-FUNCTION("DoMessage",0,0,"Slettelogg eksisterer ikke","","").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE VpiTilHK wWin 
PROCEDURE VpiTilHK :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cArtNrList AS CHAR NO-UNDO.

iReturn = 0.
RUN JBoxBrowseSelectMsg.w ("Bekreft overføring av artikler til HK",
                            hBrowse:NUM-SELECTED-ROWS,
                            STRING(DYNAMIC-FUNCTION("getRecordCount" IN h_dtmpartbas)),
                            OUTPUT iReturn).

IF hBrowse:NUM-SELECTED-ROWS > 0 AND iReturn = 2 THEN DO:
  DO ix = 1 TO hBrowse:NUM-SELECTED-ROWS:
    IF hBrowse:FETCH-SELECTED-ROW(ix) THEN 
      cArtNrList = cArtNrList +
                        STRING(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE) + ",". 
  END.
  IF NOT DYNAMIC-FUNCTION("RunProc","art_logg_korrpos.p",
                          cValues + ",ARTNUM," + TRIM(cArtNrList,","),
                          ?) THEN 
    DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Advarsel","").
END.
ELSE IF NOT DYNAMIC-FUNCTION("RunProc","art_logg_korrpos.p",cValues,DYNAMIC-FUNCTION("getTmpArtBasHandle" IN h_dtmpartbas)) THEN 
  DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Advarsel","").


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE VpiVare wWin 
PROCEDURE VpiVare :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cArtnumList AS CHAR NO-UNDO.
DEF VAR bSend       AS LOG  NO-UNDO.
DEF VAR cEDBSystem  AS CHAR NO-UNDO.
DEF VAR iAntSlett   AS INT  NO-UNDO.
DEF VAR iAntNyEndre AS INT  NO-UNDO.
DEF VAR bBilder     AS LOG  NO-UNDO.

iReturn = 0.
/* RUN JBoxBrowseSelectMsg.w ("Bekreft overføring av artikler til VPI registeret",         */
/*                             hBrowse:NUM-SELECTED-ROWS,                                  */
/*                             STRING(DYNAMIC-FUNCTION("getRecordCount" IN h_dtmpartbas)), */
/*                             OUTPUT iReturn).                                            */

IF CAN-DO("Ja,True,Yes,1",cHKInst) THEN
    RUN d-setvpiogsend.w   ("Overføre artikler til VPI register/sende til butikk|TMPARTBAS",
                            hBrowse:NUM-SELECTED-ROWS,
                            STRING(DYNAMIC-FUNCTION("getRecordCount" IN h_dtmpartbas)),
                            OUTPUT iReturn,
                            OUTPUT cValues).
ELSE
    RUN d-setvpiogsend.w   ("Overføre artikler til VPI register/sende til HK|TMPARTBAS",
                            hBrowse:NUM-SELECTED-ROWS,
                            STRING(DYNAMIC-FUNCTION("getRecordCount" IN h_dtmpartbas)),
                            OUTPUT iReturn,
                            OUTPUT cValues).
/* Slik bygges returverdi cValues i søkeprogrammet. 
STRING(fcButikkListe) + "|" + 
T-Send:SCREEN-VALUE   + "|" + 
T-Bilder:SCREEN-VALUE + "|" +
T-HKVpi:SCREEN-VALUE          - Denne benyttes ikke her. Går Alltid via HK's vpi register her. 
*/

IF iReturn = 0 THEN RETURN.

/* skal det sendes til butikkene? */
IF ENTRY(2,cValues,"|") = "YES" THEN bSend   = TRUE.
IF ENTRY(3,cValues,"|") = "YES" THEN bBilder = TRUE.

/* iReturn = DYNAMIC-FUNCTION("DoMessage",0,1,"Bekreft overføring av &1 artikler til VPI registeret." + CHR(10) +                             */
/*                                            "For artikler som allerede er overført, oppdateres priser og artikkelinformasjon."  + CHR(10) + */
/*                                            "NB: Kun artikler opprettet på HK (Artikkelnr. > 8500000) overføres.",                          */
/*                                            "",                                                                                             */
/*                                            IF hBrowse:NUM-SELECTED-ROWS > 0 THEN                                                           */
/*                                              STRING(hBrowse:NUM-SELECTED-ROWS)                                                             */
/*                                            ELSE                                                                                            */
/*                                              STRING(DYNAMIC-FUNCTION("getRecordCount" IN h_dtmpartbas))                                    */
/*                                            ).                                                                                              */
/* IF iReturn = 2 THEN RETURN.                                                                                                                */


cEDBSystem = "POS" + STRING(TIME).

IF iReturn = 1 THEN 
    bOK = DYNAMIC-FUNCTION("RunProc","art_to_vpi.p",
                           ("|" + IF bSend THEN cEDBSystem ELSE ""),
                           DYNAMIC-FUNCTION("getTmpArtBasHandle" IN h_dtmpartbas)).
ELSE DO:
  DO ix = 1 TO hBrowse:NUM-SELECTED-ROWS:
    IF hBrowse:FETCH-SELECTED-ROW(ix) THEN 
      cArtnumList = cArtnumList + STRING(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE) + ",".
  END.

  bOK = DYNAMIC-FUNCTION("RunProc","art_to_vpi.p",
                         ("ARTNR|" + IF bSend THEN cEDBSystem ELSE "") + "|" + TRIM(cArtnumList,","),
                         ?).
END.

/* Så skal den legges ut på fil og sendes til HK */
IF bSend THEN
    RUN vpieksport.w (cEDBSystem,
                      ENTRY(1,cValues,"|"), /* Ekstent skal være butikk som har sendt filen. */
                      (IF bBilder THEN 1 ELSE 0), /* Bildedata skal sendes med */
                      OUTPUT iAntSlett,
                      OUTPUT iAntNyEndre).
IF NOT bOK THEN
  DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Feil i overføring til VPI-register",""). 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fLockvindu wWin 
FUNCTION fLockvindu RETURNS CHARACTER
  ( INPUT lLock AS LOGICAL ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE hDetteVindu AS HANDLE     NO-UNDO.
  ASSIGN hDetteVindu = THIS-PROCEDURE:CURRENT-WINDOW
         hDetteVindu:SENSITIVE = NOT lLock.

  RETURN "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getBrowseHandle wWin 
FUNCTION getBrowseHandle RETURNS HANDLE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

RETURN hBrowse.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getEloggtyp wWin 
FUNCTION getEloggtyp RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN cEloggtyp.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getSelectType wWin 
FUNCTION getSelectType RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

RETURN iReturn.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION InitFromKampanje wWin 
FUNCTION InitFromKampanje RETURNS LOGICAL
  ( INPUT ihKampanjeHodeViewer AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
hKampanjeHodeViewer = ihKampanjeHodeViewer.

{&WINDOW-NAME}:MOVE-TO-TOP().
{&WINDOW-NAME}:WINDOW-STATE = 3.

btnSendUtvalg:LABEL IN FRAME {&FRAME-NAME} = "Send (del av) utvalg til kampanje". 
btnSendUtvalg:HIDDEN = FALSE.

RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION InitFromTelling wWin 
FUNCTION InitFromTelling RETURNS LOGICAL
  ( INPUT ihTelleHode AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
hTelleHode = ihTelleHode.

{&WINDOW-NAME}:MOVE-TO-TOP().
{&WINDOW-NAME}:WINDOW-STATE = 3.


btnSendUtvalg:LABEL IN FRAME {&FRAME-NAME} = "Send (del av) utvalg til telling". 
btnSendUtvalg:HIDDEN = FALSE.

RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION InitFromVareBeh wWin 
FUNCTION InitFromVareBeh RETURNS LOGICAL
  ( INPUT ihVareBehHode AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
hVareBehHode = ihVareBehHode.

{&WINDOW-NAME}:MOVE-TO-TOP().
{&WINDOW-NAME}:WINDOW-STATE = 3.


btnSendUtvalg:LABEL IN FRAME {&FRAME-NAME} = "Send (del av) utvalg til Varehåndtering". 
btnSendUtvalg:HIDDEN = FALSE.

RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION InitFromVareBok wWin 
FUNCTION InitFromVareBok RETURNS LOGICAL
  ( INPUT ihVareBokHode AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
hVareBokHode = ihVareBokHode.

{&WINDOW-NAME}:MOVE-TO-TOP().
{&WINDOW-NAME}:WINDOW-STATE = 3.


btnSendUtvalg:LABEL IN FRAME {&FRAME-NAME} = "Send (del av) utvalg til varebok". 
btnSendUtvalg:HIDDEN = FALSE.

RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setBrowseReadOnly wWin 
FUNCTION setBrowseReadOnly RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR hCol AS HANDLE NO-UNDO.
hCol = hBrowse:FIRST-COLUMN.
DO WHILE VALID-HANDLE(hCol):
  IF hCol:COLUMN-READ-ONLY THEN
    hCol:READ-ONLY = TRUE.
  hCol = hCol:NEXT-COLUMN.
END.
APPLY "entry" TO hBrowse.

RETURN TRUE. 

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setButtons wWin 
FUNCTION setButtons RETURNS LOGICAL
  ( INPUT ibSensitive AS LOG ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
  ASSIGN B-Jamfor:SENSITIVE = ibSensitive 
         BUTTON-ArtKort:SENSITIVE = ibSensitive 
         BUTTON-EndreBest:SENSITIVE = ibSensitive 
         BUTTON-Innleveranse:SENSITIVE = ibSensitive 
         BUTTON-Kalkyle:SENSITIVE = ibSensitive 
         BUTTON-NyBest:SENSITIVE = ibSensitive 
         BUTTON-Overfor:SENSITIVE = ibSensitive 
         BUTTON-SettLopNr:SENSITIVE = ibSensitive 
         BUTTON-SlettBest:SENSITIVE = ibSensitive 
         BUTTON-TilPakke:SENSITIVE = ibSensitive
         .
  DYNAMIC-FUNCTION("setToolbar",hBrowse,IF ibSensitive THEN "enable" ELSE "disable").
END.

RETURN TRUE. 

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setEnableBtnSendUtvalg wWin 
FUNCTION setEnableBtnSendUtvalg RETURNS LOGICAL
  ( INPUT ibSensitive AS LOG ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
btnSendUtvalg:SENSITIVE IN FRAME {&FRAME-NAME} = ibSensitive.

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setKampanjeIsMaster wWin 
FUNCTION setKampanjeIsMaster RETURNS LOGICAL
  ( INPUT ibKampanjeIsMaster AS LOG ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
bKampanjeIsMaster = ibKampanjeIsMaster.

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setTellingIsMaster wWin 
FUNCTION setTellingIsMaster RETURNS LOGICAL
  ( INPUT ibTellingIsMaster AS LOG ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
bTellingIsMaster = ibTellingIsMaster.

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION SetVareBehIsMaster wWin 
FUNCTION SetVareBehIsMaster RETURNS LOGICAL
  ( INPUT ibVareBehIsMaster AS LOG ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
bVareBehIsMaster = ibVareBehIsMaster.

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setVareBokIsMaster wWin 
FUNCTION setVareBokIsMaster RETURNS LOGICAL
  ( INPUT ibVareBokIsMaster AS LOG ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
bVareBokIsMaster = ibVareBokIsMaster.

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ViewHideStopButton wWin 
FUNCTION ViewHideStopButton RETURNS LOGICAL
  ( INPUT ibView AS LOG ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
IF ibView THEN
  btnStopSelection:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
ELSE
  btnStopSelection:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.

RETURN TRUE.   

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

