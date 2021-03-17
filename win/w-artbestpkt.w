&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          skotex           PROGRESS
*/
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
DEF INPUT PARAMETER dArtikkelNr LIKE ArtBas.ArtikkelNr NO-UNDO.
def input parameter wCurrent-Window    as handle no-undo.
def input parameter wParentHandle      as handle no-undo.

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE cEmptyField   AS CHARACTER LABEL "" NO-UNDO.
DEFINE VARIABLE wOk           AS LOGICAL    NO-UNDO.
DEFINE VARIABLE wBekreft      AS LOG        NO-UNDO.
DEFINE VARIABLE hEtikettVindu AS HANDLE     NO-UNDO.
DEFINE VARIABLE hIndividDet   AS HANDLE     NO-UNDO.
DEFINE VARIABLE iCl           AS INT        NO-UNDO.
DEFINE VAR      cStorl        AS CHAR FORMAT "x(10)" NO-UNDO.

DEF VAR wRecid AS RECID NO-UNDO.

define temp-table tmpChild
  field wChild as handle.

DEF BUFFER clButiker FOR butiker.
{runlib.i} /* Starter procedurebibloteket. */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-BestPkt
&Scoped-define BROWSE-NAME BROWSE-ArtBestPkt

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ArtBestPkt

/* Definitions for BROWSE BROWSE-ArtBestPkt                             */
&Scoped-define FIELDS-IN-QUERY-BROWSE-ArtBestPkt ArtBestPkt.ButikkNr ArtBestPkt.StrKode cStorl ArtBestPkt.MinAnt ArtBestPkt.MaksAnt ArtBestPkt.BestAnt ArtBestPkt.TillatBruttPk ArtBestPkt.EDato ArtBestPkt.BrukerID   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-ArtBestPkt ArtBestPkt.MinAnt ~
ArtBestPkt.MaksAnt ~
ArtBestPkt.BestAnt ~
ArtBestPkt.TillatBruttPk   
&Scoped-define ENABLED-TABLES-IN-QUERY-BROWSE-ArtBestPkt ArtBestPkt
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BROWSE-ArtBestPkt ArtBestPkt
&Scoped-define SELF-NAME BROWSE-ArtBestPkt
&Scoped-define QUERY-STRING-BROWSE-ArtBestPkt FOR EACH ArtBestPkt       WHERE ArtBestPkt.ArtikkelNr = dArtikkelNr and             (if fi-Butik > 0                then ArtBestPkt.ButikkNr = fi-Butik              else true)     NO-LOCK
&Scoped-define OPEN-QUERY-BROWSE-ArtBestPkt OPEN QUERY {&SELF-NAME} FOR EACH ArtBestPkt       WHERE ArtBestPkt.ArtikkelNr = dArtikkelNr and             (if fi-Butik > 0                then ArtBestPkt.ButikkNr = fi-Butik              else true)     NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BROWSE-ArtBestPkt ArtBestPkt
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-ArtBestPkt ArtBestPkt


/* Definitions for FRAME FRAME-BestPkt                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-57 fi-Butik B-Blank btnSokButikk ~
BROWSE-ArtBestPkt BUTTON-Ny BUTTON-Endre BUTTON-Slett BUTTON-Gen ~
BUTTON-SlettGen BUTTON-Oppdater 
&Scoped-Define DISPLAYED-OBJECTS fi-Butik fi-ButNamn 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-Blank 
     LABEL "Blank" 
     SIZE 8.2 BY 1.

DEFINE BUTTON btnSokButikk  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON BUTTON-Endre 
     LABEL "&Endre..." 
     SIZE 17 BY 1.14.

DEFINE BUTTON BUTTON-Gen 
     LABEL "Opprett punkter" 
     SIZE 17 BY 1.14.

DEFINE BUTTON BUTTON-Ny 
     LABEL "N&y..." 
     SIZE 17 BY 1.14.

DEFINE BUTTON BUTTON-Oppdater 
     LABEL "Frisk opp" 
     SIZE 17 BY 1.14.

DEFINE BUTTON BUTTON-Slett 
     LABEL "Sle&tt" 
     SIZE 17 BY 1.14.

DEFINE BUTTON BUTTON-SlettGen 
     LABEL "Slett tomme pkt" 
     SIZE 17 BY 1.14.

DEFINE VARIABLE fi-Butik AS INTEGER FORMAT ">>>>>9" INITIAL 0 
     LABEL "Butikk" 
     VIEW-AS FILL-IN 
     SIZE 10.4 BY 1.

DEFINE VARIABLE fi-ButNamn AS CHARACTER FORMAT "x(200)" 
     VIEW-AS FILL-IN 
     SIZE 73 BY 1.

DEFINE RECTANGLE RECT-57
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 114 BY 1.43.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-ArtBestPkt FOR 
      ArtBestPkt SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-ArtBestPkt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-ArtBestPkt C-Win _FREEFORM
  QUERY BROWSE-ArtBestPkt NO-LOCK DISPLAY
      ArtBestPkt.ButikkNr FORMAT ">>>>9":U
      ArtBestPkt.StrKode FORMAT ">>999":U
      cStorl COLUMN-LABEL "Str"
      ArtBestPkt.MinAnt FORMAT "->>>,>>9.999":U
      ArtBestPkt.MaksAnt FORMAT "->>>,>>9.999":U
      ArtBestPkt.BestAnt FORMAT "->>>,>>9.999":U
      ArtBestPkt.TillatBruttPk FORMAT "*/":U
      ArtBestPkt.EDato FORMAT "99/99/99":U
      ArtBestPkt.BrukerID COLUMN-LABEL "Endret av" FORMAT "X(10)":U
  ENABLE
      ArtBestPkt.MinAnt
      ArtBestPkt.MaksAnt
      ArtBestPkt.BestAnt
      ArtBestPkt.TillatBruttPk
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 173 BY 19.62 ROW-HEIGHT-CHARS .62 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-BestPkt
     fi-Butik AT ROW 2 COL 11.2 COLON-ALIGNED HELP
          "Butikknummer"
     B-Blank AT ROW 2 COL 28.8
     btnSokButikk AT ROW 2 COL 24 NO-TAB-STOP 
     fi-ButNamn AT ROW 2 COL 36 COLON-ALIGNED HELP
          "Butikkens navn" NO-LABEL
     BROWSE-ArtBestPkt AT ROW 3.48 COL 2
     BUTTON-Ny AT ROW 4.1 COL 178.2
     BUTTON-Endre AT ROW 5.29 COL 178.2
     BUTTON-Slett AT ROW 6.48 COL 178.2
     BUTTON-Gen AT ROW 8.86 COL 178.2
     BUTTON-SlettGen AT ROW 10.05 COL 178.2
     BUTTON-Oppdater AT ROW 21.95 COL 178.2
     RECT-57 AT ROW 1.76 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 9.81
         SIZE 207.8 BY 23.1.


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
         TITLE              = ""
         HEIGHT             = 32.29
         WIDTH              = 208
         MAX-HEIGHT         = 32.29
         MAX-WIDTH          = 208
         VIRTUAL-HEIGHT     = 32.29
         VIRTUAL-WIDTH      = 208
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




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME FRAME-BestPkt
   NOT-VISIBLE FRAME-NAME                                               */
/* BROWSE-TAB BROWSE-ArtBestPkt fi-ButNamn FRAME-BestPkt */
/* SETTINGS FOR FILL-IN fi-ButNamn IN FRAME FRAME-BestPkt
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-ArtBestPkt
/* Query rebuild information for BROWSE BROWSE-ArtBestPkt
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ArtBestPkt
      WHERE ArtBestPkt.ArtikkelNr = dArtikkelNr and
            (if fi-Butik > 0
               then ArtBestPkt.ButikkNr = fi-Butik
             else true)
    NO-LOCK.
     _END_FREEFORM
     _Options          = "NO-LOCK"
     _Where[1]         = "ArtBestPkt.ArtikkelNr = ArtBas.ArtikkelNr and
(if input fi-Butik in frame Dialog-Frame > 0
  then ArtBestPkt.ButikkNr = input fi-Butik  in frame Dialog-Frame
 else true)"
     _Query            is NOT OPENED
*/  /* BROWSE BROWSE-ArtBestPkt */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-BestPkt
/* Query rebuild information for FRAME FRAME-BestPkt
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME FRAME-BestPkt */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win
DO:
    if can-find(first tmpChild where
                 valid-handle(tmpChild.wChild)) then
      do:
        wBekreft = false.
        message 'Det er startet andre programmer fra dette vinduet.' skip
                'Avsluttes dette vinduet, vil alle underliggende programmer' skip
                'også bli avsluttet.'
                view-as alert-box warning buttons yes-no title 'Bekreft avsluttning'
                update wBekreft
                .
      end.
    else wBekreft = true.
    if wBekreft <> true then
    return no-apply.

    /* This event will close the window and terminate the procedure.  */
    APPLY "CLOSE":U TO THIS-PROCEDURE.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Blank
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Blank C-Win
ON CHOOSE OF B-Blank IN FRAME FRAME-BestPkt /* Blank */
DO:
  ASSIGN
      fi-Butik:SCREEN-VALUE = "0"
      fi-Butik              = 0
      .
   RUN OpenQueryArtBestPkt.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-ArtBestPkt
&Scoped-define SELF-NAME BROWSE-ArtBestPkt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-ArtBestPkt C-Win
ON ROW-DISPLAY OF BROWSE-ArtBestPkt IN FRAME FRAME-BestPkt
DO:
    cStorl = DYNAMIC-FUNCTION("getFieldValues","StrKonv",
                               "WHERE StrKode = " + string(ArtBestPkt.StrKode),"Storl").  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-ArtBestPkt C-Win
ON VALUE-CHANGED OF BROWSE-ArtBestPkt IN FRAME FRAME-BestPkt
DO:
    IF AVAILABLE ArtBas THEN
    DO:
      IF (CAN-DO("0,1",STRING(ArtBas.ArtSlag)) AND ArtBas.BestForslag) THEN
      ASSIGN 
          BUTTON-Slett:SENSITIVE    = BROWSE {&BROWSE-NAME}:FOCUSED-ROW <> ?
          BUTTON-Endre:SENSITIVE    = BROWSE {&BROWSE-NAME}:FOCUSED-ROW <> ?
          .
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSokButikk
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSokButikk C-Win
ON CHOOSE OF btnSokButikk IN FRAME FRAME-BestPkt /* ... */
OR F10 OF ArtBestPkt.StrKode
DO:
  DEF VAR cLookupValue AS CHAR NO-UNDO.

  /* Syntaks: Param1: <Tabell>;<Felt>;<Felt>...,<Tabell>;<Felt>;<Felt>...              */
  /*          Param2: <Where sats> m/Join                                              */
  /*          Param3: <Returfelt1>[;<Returfelt2>;......],<Filterfelt1>[;<Filterfelt2>] (Settes i cLookupValue) */
  /* Kalkulerte felt kan også benyttes, label, format o.l..       */
  cLookupValue = "Butik".
  RUN JBoxDLookup.w ("Butiker;Butik;ButNamn","where true",INPUT-OUTPUT cLookupValue).

  IF cLookupValue NE "" THEN 
  DO:
    ASSIGN
        fi-Butik:SCREEN-VALUE = cLookupValue
        fi-butik = INPUT fi-butik
        .
    cLookupValue = DYNAMIC-FUNCTION("getFieldList","Butiker;ButNamn","WHERE Butik = '" + fi-Butik:SCREEN-VALUE + "'").
    fi-butNamn:SCREEN-VALUE = ENTRY(1,cLookupValue,"|").
    RUN OpenQueryArtBestPkt.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Endre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Endre C-Win
ON CHOOSE OF BUTTON-Endre IN FRAME FRAME-BestPkt /* Endre... */
DO:
    IF NOT AVAILABLE ArtBas THEN
        RETURN NO-APPLY.
    IF NOT AVAILABLE ArtBestPkt THEN
        RETURN NO-APPLY.
    assign 
      wRecid = RECID(ArtBestPkt).
    run d-vartbestpkt.w (input-output wRecid,"Endre",ArtBas.ArtikkelNr,ArtBestPkt.ButikkNr).
    if return-value = "AVBRYT" then
      return no-apply.
    find ArtBestPkt no-lock where
      recid(ArtBestPkt) = wRecid no-error.
    {&BROWSE-NAME}:REFRESH().
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Gen
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Gen C-Win
ON CHOOSE OF BUTTON-Gen IN FRAME FRAME-BestPkt /* Opprett punkter */
DO:
  IF NOT AVAILABLE ArtBas THEN
      RETURN NO-APPLY.
  DO TRANSACTION:

      FOR EACH Strekkode NO-LOCK WHERE
          Strekkode.ArtikkelNr = ArtBas.ArtikkelNr:
          FOR EACH Butiker NO-LOCK WHERE
              (IF fi-Butik > 0
                 THEN Butiker.Butik = fi-Butik
                 ELSE TRUE) AND
              Butiker.NedlagtDato = ?:
              IF NOT CAN-FIND(ArtBestPkt WHERE
                              ArtBestPkt.ArtikkelNr = ArtBas.ArtikkelNr AND
                              ArtBestPkt.ButikkNr   = Butiker.Butik     AND
                              ArtBestPkt.StrKode    = Strekkode.StrKode) THEN
              DO:
                  CREATE ArtBestPkt.
                  ASSIGN
                      ArtBestPkt.ArtikkelNr    = ArtBas.ArtikkelNr 
                      ArtBestPkt.ButikkNr      = Butiker.Butik     
                      ArtBestPkt.StrKode       = Strekkode.StrKode 
                      ArtBestPkt.TillatBruttPk = TRUE
                      .
              END.
          END.
      END.
      /*
      FOR EACH StrTStr NO-LOCK WHERE
          StrTStr.STrTypeId = ArtBas.StrTypeId:
          FIND StrKonv NO-LOCK WHERE
              StrKonv.Storl = StrTStr.SoStorl NO-ERROR.
          IF AVAILABLE StrKonv THEN
          FOR EACH Butiker NO-LOCK WHERE
              (IF fi-Butik > 0
                 THEN Butiker.Butik = fi-Butik
                 ELSE TRUE) AND
              Butiker.NedlagtDato = ?:
              IF NOT CAN-FIND(ArtBestPkt WHERE
                              ArtBestPkt.ArtikkelNr = ArtBas.ArtikkelNr AND
                              ArtBestPkt.ButikkNr   = Butiker.Butik     AND
                              ArtBestPkt.StrKode    = StrKonv.StrKode) THEN
              DO:
                  CREATE ArtBestPkt.
                  ASSIGN
                      ArtBestPkt.ArtikkelNr    = ArtBas.ArtikkelNr 
                      ArtBestPkt.ButikkNr      = Butiker.Butik     
                      ArtBestPkt.StrKode       = StrKonv.StrKode 
                      ArtBestPkt.TillatBruttPk = TRUE
                      .
              END.
          END.
      END.
      */
  END.
  RUN OpenQueryArtBestPkt.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Ny
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Ny C-Win
ON CHOOSE OF BUTTON-Ny IN FRAME FRAME-BestPkt /* Ny... */
DO:
    IF NOT AVAILABLE ArtBas THEN
        RETURN NO-APPLY.
    assign 
      wRecid = ?.
    run d-vartbestpkt.w (input-output wRecid,"Ny",ArtBas.ArtikkelNr,iCl).
    if return-value = "AVBRYT" then
      return no-apply.
    find ArtBestPkt no-lock where
      recid(ArtBestPkt) = wRecid no-error.
    RUN OpenQueryArtBestPkt.
    REPOSITION {&BROWSE-NAME} TO ROWID rowid(ArtBestPkt) NO-ERROR.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Oppdater
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Oppdater C-Win
ON CHOOSE OF BUTTON-Oppdater IN FRAME FRAME-BestPkt /* Frisk opp */
DO:
    RUN OpenQueryArtBestPkt.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Slett
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Slett C-Win
ON CHOOSE OF BUTTON-Slett IN FRAME FRAME-BestPkt /* Slett */
DO:
  MESSAGE "Vill du slette bestillingspunktet? " VIEW-AS ALERT-BOX QUESTION
                BUTTONS YES-NO UPDATE wSvar AS LOGI.
  IF wSvar THEN DO TRANSACTION:
      FIND CURRENT ArtBestPkt EXCLUSIVE.
      DELETE ArtBestPkt.
      BROWSE {&BROWSE-NAME}:DELETE-CURRENT-ROW().
      APPLY "VALUE-CHANGED" TO BROWSE BROWSE-ArtBestPkt.
  END.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SlettGen
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SlettGen C-Win
ON CHOOSE OF BUTTON-SlettGen IN FRAME FRAME-BestPkt /* Slett tomme pkt */
DO:
    wOk = FALSE.
    MESSAGE "Skal alle 'tomme' bestilingspunkter slettes?" SKIP
            "0 i maks, min og antall å bestille."
        VIEW-AS ALERT-BOX QUESTION BUTTONS yes-no
        UPDATE wOk.
    IF wOk <> TRUE THEN
        RETURN NO-APPLY.

    IF NOT AVAILABLE ArtBas THEN
        RETURN NO-APPLY.
    DO TRANSACTION:
        FOR EACH ArtBestPkt OF ArtBas EXCLUSIVE-LOCK WHERE
            (IF fi-Butik > 0
               THEN ArtBestPkt.ButikkNr = fi-Butik
               ELSE TRUE):
            IF  (ArtBestPkt.MinAnt = 0 AND
                ArtBestPkt.MaksAnt = 0 AND
                ArtBestPkt.BestAnt = 0) THEN
                DELETE ArtBestPkt.
        END.
    END.
    RUN OpenQueryArtBestPkt.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-Butik
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-Butik C-Win
ON RETURN OF fi-Butik IN FRAME FRAME-BestPkt /* Butikk */
OR "TAB" OF fi-butik
DO:
  RUN OpenQueryArtBestPkt.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-Butik C-Win
ON VALUE-CHANGED OF fi-Butik IN FRAME FRAME-BestPkt /* Butikk */
DO:
  ASSIGN
      fi-Butik = INPUT fi-Butik
      .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = wCurrent-Window 
       THIS-PROCEDURE:CURRENT-WINDOW = wCurrent-Window.             

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
  ON CLOSE OF THIS-PROCEDURE DO:
  /*    RUN SaveBrowseSettings. */
      RUN SlettTmpChild.
      IF VALID-HANDLE(wParentHandle) THEN
          RUN SlettProg in wParentHandle.
      IF VALID-HANDLE(hEtikettVindu) THEN
          APPLY "CLOSE" TO hEtikettVindu.
      IF VALID-HANDLE(hIndividDet) THEN
          DELETE PROCEDURE hIndividDet.
      RUN disable_UI.
  END.

{syspara.i 5 1 1 iCl INT}
FIND clbutiker NO-LOCK WHERE
    clbutiker.butik = iCl NO-ERROR.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  FIND ArtBas WHERE ArtBas.ArtikkelNr = dArtikkelNr NO-LOCK.
  RUN enable_UI.
  {lng.i}
  SUBSCRIBE TO "ByttObjekt" IN wParentHandle NO-ERROR.
      BROWSE BROWSE-ArtBestPkt:MOVE-TO-TOP().
  IF ArtBas.IndiVidType > 0 THEN
      {&OPEN-QUERY-BROWSE-ArtBestPkt}
  RUN EnaDisBtn.
  APPLY "VALUE-CHANGED" TO BROWSE BROWSE-ArtBestPkt.
  APPLY "ENTRY" TO  BROWSE BROWSE-ArtBestPkt.
  FRAME FRAME-BestPkt:SENSITIVE = artbas.sanertdato = ?.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ByttObjekt C-Win 
PROCEDURE ByttObjekt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER ipArtikkelNr LIKE ArtBas.ArtikkelNr NO-UNDO.
  FIND ArtBas WHERE ArtBas.ArtikkelNr = ipArtikkelNr NO-LOCK.
  ASSIGN dArtikkelNr = ipArtikkelnr.
  FRAME FRAME-BestPkt:SENSITIVE = artbas.sanertdato = ?.
  {sww.i}  
        {&OPEN-QUERY-BROWSE-ArtBestPkt}
/*     ELSE                                     */
/*         CLOSE QUERY BROWSE-Individvare. */
    APPLY "VALUE-CHANGED" TO BROWSE BROWSE-ArtBestPkt.
  RUN EnaDisBtn.
  {swn.i}

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
  HIDE FRAME FRAME-BestPkt.
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
  DISPLAY fi-Butik fi-ButNamn 
      WITH FRAME FRAME-BestPkt.
  ENABLE RECT-57 fi-Butik B-Blank btnSokButikk BROWSE-ArtBestPkt BUTTON-Ny 
         BUTTON-Endre BUTTON-Slett BUTTON-Gen BUTTON-SlettGen BUTTON-Oppdater 
      WITH FRAME FRAME-BestPkt.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-BestPkt}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EnaDisBtn C-Win 
PROCEDURE EnaDisBtn :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  IF NOT AVAILABLE ArtBas THEN
      RETURN.
  DO WITH FRAME {&FRAME-NAME}:
      ASSIGN
          BUTTON-Ny:SENSITIVE         = CAN-DO("0,1",STRING(ArtBas.ArtSlag)) AND ArtBas.BestForslag
          BUTTON-Endre:SENSITIVE      = CAN-DO("0,1",STRING(ArtBas.ArtSlag)) AND ArtBas.BestForslag
          BUTTON-Slett:SENSITIVE      = CAN-DO("0,1",STRING(ArtBas.ArtSlag)) AND ArtBas.BestForslag
          BUTTON-Oppdater:SENSITIVE   = CAN-DO("0,1",STRING(ArtBas.ArtSlag)) AND ArtBas.BestForslag
          BUTTON-Gen:SENSITIVE        = CAN-DO("0,1",STRING(ArtBas.ArtSlag)) AND ArtBas.BestForslag
          fi-Butik:SENSITIVE          = CAN-DO("0,1",STRING(ArtBas.ArtSlag)) AND ArtBas.BestForslag
          btnSokButikk:SENSITIVE      = CAN-DO("0,1",STRING(ArtBas.ArtSlag)) AND ArtBas.BestForslag
          B-Blank:SENSITIVE           = CAN-DO("0,1",STRING(ArtBas.ArtSlag)) AND ArtBas.BestForslag
          BROWSE-ArtBestPkt:SENSITIVE = CAN-DO("0,1",STRING(ArtBas.ArtSlag)) AND ArtBas.BestForslag
          BUTTON-SlettGen:SENSITIVE   = CAN-DO("0,1",STRING(ArtBas.ArtSlag)) AND ArtBas.BestForslag
          fi-Butik:SCREEN-VALUE = "0"
          fi-ButNamn:SCREEN-VALUE = ""
          .
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MoveToTopp C-Win 
PROCEDURE MoveToTopp :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   IF FRAME FRAME-BestPkt:MOVE-TO-TOP() THEN.
   APPLY "ENTRY" TO  BROWSE BROWSE-ArtBestPkt.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpenQueryArtBestPkt C-Win 
PROCEDURE OpenQueryArtBestPkt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {&OPEN-QUERY-BROWSE-ArtBestPkt}
  APPLY "VALUE-CHANGED" TO BROWSE BROWSE-ArtBestPkt.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE openQueryBrowse C-Win 
PROCEDURE openQueryBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    {&OPEN-QUERY-{&BROWSE-NAME}}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE refreshBrowse C-Win 
PROCEDURE refreshBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    BROWSE {&BROWSE-NAME}:REFRESH().
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetEntry C-Win 
PROCEDURE SetEntry :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    APPLY "ENTRY" TO BROWSE BROWSE-ArtBestPkt.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SlettTmpChild C-Win 
PROCEDURE SlettTmpChild :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FOR EACH tmpChild:
        IF VALID-HANDLE(tmpChild.wChild) THEN
            DELETE PROCEDURE tmpChild.wChild.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

