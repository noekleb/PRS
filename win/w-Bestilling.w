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
def input parameter dArtikkelNr LIKE ArtBas.ArtikkelNr no-undo.
def input parameter wCurrent-Window    as handle no-undo.
def input parameter wParentHandle      as handle no-undo.

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE cEmptyField AS CHARACTER LABEL "" NO-UNDO.
DEFINE VARIABLE wBestHodeRecid AS RECID      NO-UNDO.
DEFINE VARIABLE wArtBasRecid   AS RECID      NO-UNDO.
DEFINE VARIABLE cHKinst        AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cAlle      AS CHARACTER  NO-UNDO.
DEFINE VARIABLE iBestStatSV AS INTE  NO-UNDO.
DEFINE VARIABLE iButSV AS INTE  NO-UNDO.
DEFINE VARIABLE iClbut AS INTE  NO-UNDO.
DEFINE VARIABLE cSkomodus AS CHARACTER NO-UNDO.
DEFINE VARIABLE cBekreftet AS CHARACTER   NO-UNDO.
def temp-table tmpChild 
  field wChild as handle.

DEF TEMP-TABLE TT_But NO-UNDO
    FIELD butikknr AS INTE.

DEFINE TEMP-TABLE ttDummy NO-UNDO
    FIELD dummy AS INTEGER.




{runlib.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-Ordre
&Scoped-define BROWSE-NAME BROWSE-Bestilling

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES BestHode BestStr ttDummy

/* Definitions for BROWSE BROWSE-Bestilling                             */
&Scoped-define FIELDS-IN-QUERY-BROWSE-Bestilling BestHode.BestNr BestHode.BestillingsDato BestHode.LevDato BestHode.BestStat getBekreftet() @ cBekreftet BestHode.LevNr BestHode.Beskrivelse BestHode.DirekteLev OrdreNummer() (BestHode.TotAntPar * IF BestHode.BestStat = 7 THEN -1 ELSE 1) ((BestHode.TotAntPar * IF BestHode.BestStat = 7 THEN -1 ELSE 1) - BestHode.TotInnLev - BestHode.TotMakulert) /* BestHode.TotMakulert */ /* */ /* (IF BestHode.BestStat = 7 THEN 0 ELSE BestHode.TotInnkjVerdi) */ /* */ /* (IF BestHode.BestStat = 7 THEN 0 ELSE BestHode.TotSalgsVerdi) */ /* */ /* (IF BestHode.BestStat = 7 THEN 0 ELSE BestHode.TotDbKr) */ /* */ /* BestHode.TotAntPar */ /* */ /* (BestHode.TotAntPar - BestHode.TotInnLev - BestHode.TotMakulert) */ /* */ BestHode.TotMakulert BestHode.TotInnkjVerdi BestHode.TotSalgsVerdi BestHode.TotDbKr   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-Bestilling   
&Scoped-define SELF-NAME BROWSE-Bestilling
&Scoped-define QUERY-STRING-BROWSE-Bestilling FOR EACH BestHode       WHERE BestHode.ArtikkelNr = (if available ArtBas                          then ArtBas.ArtikkelNr                          else -99) AND (IF iBestStatSV = 0 THEN TRUE ELSE BestHode.BestStat = iBestStatSV) NO-LOCK, ~
             FIRST BestStr OF BestHode NO-LOCK WHERE (IF iButSV = 0 THEN TRUE ELSE BestStr.butik = iButSV) OUTER-JOIN, ~
            FIRST ttDummy WHERE AVAILABLE beststr OR cSkomodus = "1"     BY BestHode.BestNr DESCENDING INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BROWSE-Bestilling OPEN QUERY {&SELF-NAME} FOR EACH BestHode       WHERE BestHode.ArtikkelNr = (if available ArtBas                          then ArtBas.ArtikkelNr                          else -99) AND (IF iBestStatSV = 0 THEN TRUE ELSE BestHode.BestStat = iBestStatSV) NO-LOCK, ~
             FIRST BestStr OF BestHode NO-LOCK WHERE (IF iButSV = 0 THEN TRUE ELSE BestStr.butik = iButSV) OUTER-JOIN, ~
            FIRST ttDummy WHERE AVAILABLE beststr OR cSkomodus = "1"     BY BestHode.BestNr DESCENDING INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BROWSE-Bestilling BestHode BestStr ttDummy
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-Bestilling BestHode
&Scoped-define SECOND-TABLE-IN-QUERY-BROWSE-Bestilling BestStr
&Scoped-define THIRD-TABLE-IN-QUERY-BROWSE-Bestilling ttDummy


/* Definitions for FRAME FRAME-Ordre                                    */
&Scoped-define OPEN-BROWSERS-IN-QUERY-FRAME-Ordre ~
    ~{&OPEN-QUERY-BROWSE-Bestilling}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BUTTON-Avskriv CB-BestStat CB-Butikk ~
BROWSE-Bestilling B-OppdBest BUTTON-EndreBest BUTTON-Innleveranse ~
BUTTON-Kalkyle BUTTON-NyBest BUTTON-SlettBest 
&Scoped-Define DISPLAYED-OBJECTS CB-BestStat CB-Butikk 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getBekreftet C-Win 
FUNCTION getBekreftet RETURNS CHARACTER
  (  )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD OrdreNummer C-Win 
FUNCTION OrdreNummer RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-OppdBest  NO-FOCUS
     LABEL "Oppdater browser" 
     SIZE 22 BY 1.14.

DEFINE BUTTON BUTTON-Avskriv  NO-FOCUS
     LABEL "Avskriv rest..." 
     SIZE 22 BY 1.14.

DEFINE BUTTON BUTTON-EndreBest  NO-FOCUS
     LABEL "Endre..." 
     SIZE 22 BY 1.14.

DEFINE BUTTON BUTTON-Innleveranse  NO-FOCUS
     LABEL "Varemottak..." 
     SIZE 22 BY 1.14.

DEFINE BUTTON BUTTON-Kalkyle  NO-FOCUS
     LABEL "Kalkyle..." 
     SIZE 22 BY 1.14.

DEFINE BUTTON BUTTON-NyBest  NO-FOCUS
     LABEL "Ny..." 
     SIZE 22 BY 1.14.

DEFINE BUTTON BUTTON-SlettBest  NO-FOCUS
     LABEL "Slette" 
     SIZE 22 BY 1.14.

DEFINE VARIABLE CB-BestStat AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Bestillingsstatus" 
     VIEW-AS COMBO-BOX INNER-LINES 10
     LIST-ITEM-PAIRS "[Alle]",0
     DROP-DOWN-LIST
     SIZE 34 BY 1 NO-UNDO.

DEFINE VARIABLE CB-Butikk AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Butikk" 
     VIEW-AS COMBO-BOX INNER-LINES 10
     LIST-ITEM-PAIRS "[Alle]",0
     DROP-DOWN-LIST
     SIZE 45.2 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-Bestilling FOR 
      BestHode, 
      BestStr, 
      ttDummy SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-Bestilling
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-Bestilling C-Win _FREEFORM
  QUERY BROWSE-Bestilling NO-LOCK DISPLAY
      BestHode.BestNr FORMAT ">>>>>>>9":U
      BestHode.BestillingsDato COLUMN-LABEL "BestDato" FORMAT "99/99/99":U
      BestHode.LevDato COLUMN-LABEL "LevDato" FORMAT "99/99/99":U
      BestHode.BestStat FORMAT ">9":U
      getBekreftet() @ cBekreftet COLUMN-LABEL "B" FORMAT "x(2)":U
      BestHode.LevNr FORMAT "zzzzz9":U
      BestHode.Beskrivelse FORMAT "x(30)":U WIDTH 27
      BestHode.DirekteLev FORMAT "yes/no":U
      OrdreNummer() COLUMN-LABEL "OrdreNr" FORMAT "x(8)":U WIDTH 9.4
          (BestHode.TotAntPar * IF BestHode.BestStat = 7 THEN -1 ELSE 1) COLUMN-LABEL "Ant.par" FORMAT "->,>>9":U
                WIDTH 13.6
          ((BestHode.TotAntPar * IF BestHode.BestStat = 7 THEN -1 ELSE 1) - BestHode.TotInnLev - BestHode.TotMakulert) COLUMN-LABEL "Rest.par" FORMAT "->,>>9":U
                WIDTH 13.6
/*           BestHode.TotMakulert COLUMN-LABEL "Makulert" FORMAT "->,>>9":U                                                                                         */
/*                 WIDTH 13.6                                                                                                                                       */
/*           (IF BestHode.BestStat = 7 THEN 0 ELSE BestHode.TotInnkjVerdi) COLUMN-LABEL "Innkj. verdi" FORMAT "->,>>>,>>9.99":U                                     */
/*                 WIDTH 15                                                                                                                                         */
/*           (IF BestHode.BestStat = 7 THEN 0 ELSE BestHode.TotSalgsVerdi) COLUMN-LABEL "Salgsverdi" FORMAT "->,>>>,>>9.99":U                                       */
/*                 WIDTH 16                                                                                                                                         */
/*           (IF BestHode.BestStat = 7 THEN 0 ELSE BestHode.TotDbKr) COLUMN-LABEL "DbKr" FORMAT "->,>>>,>>9.99":U                                                   */
/*                 WIDTH 13                                                                                                                                         */
/*       BestHode.TotAntPar COLUMN-LABEL "Ant.par" FORMAT "->,>>9":U                                                */
/*             WIDTH 13.6                                                                                           */
/*       (BestHode.TotAntPar - BestHode.TotInnLev - BestHode.TotMakulert) COLUMN-LABEL "Rest.par" FORMAT "->,>>9":U */
/*             WIDTH 13.6                                                                                           */
      BestHode.TotMakulert COLUMN-LABEL "Makulert" FORMAT "->,>>9":U
            WIDTH 13.6
      BestHode.TotInnkjVerdi COLUMN-LABEL "Innkj. verdi" FORMAT "->,>>>,>>9.99":U
            WIDTH 15
      BestHode.TotSalgsVerdi COLUMN-LABEL "Salgsverdi" FORMAT "->,>>>,>>9.99":U
            WIDTH 16
      BestHode.TotDbKr COLUMN-LABEL "DbKr" FORMAT "->,>>>,>>9.99":U
            WIDTH 13
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 163 BY 19.62 ROW-HEIGHT-CHARS .62 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-Ordre
     BUTTON-Avskriv AT ROW 9.81 COL 168 WIDGET-ID 2
     CB-BestStat AT ROW 1.95 COL 20 COLON-ALIGNED
     CB-Butikk AT ROW 1.95 COL 68.8 COLON-ALIGNED
     BROWSE-Bestilling AT ROW 3.48 COL 2
     B-OppdBest AT ROW 19.38 COL 168
     BUTTON-EndreBest AT ROW 4.67 COL 168
     BUTTON-Innleveranse AT ROW 8.52 COL 168
     BUTTON-Kalkyle AT ROW 5.95 COL 168
     BUTTON-NyBest AT ROW 3.38 COL 168
     BUTTON-SlettBest AT ROW 7.24 COL 168
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
         TITLE              = "<insert window title>"
         HEIGHT             = 32.29
         WIDTH              = 202
         MAX-HEIGHT         = 56.24
         MAX-WIDTH          = 384
         VIRTUAL-HEIGHT     = 56.24
         VIRTUAL-WIDTH      = 384
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
/* SETTINGS FOR FRAME FRAME-Ordre
   NOT-VISIBLE FRAME-NAME                                               */
/* BROWSE-TAB BROWSE-Bestilling CB-Butikk FRAME-Ordre */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-Bestilling
/* Query rebuild information for BROWSE BROWSE-Bestilling
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH BestHode
      WHERE BestHode.ArtikkelNr = (if available ArtBas
                         then ArtBas.ArtikkelNr
                         else -99) AND
(IF iBestStatSV = 0 THEN TRUE ELSE BestHode.BestStat = iBestStatSV) NO-LOCK,
      FIRST BestStr OF BestHode NO-LOCK WHERE (IF iButSV = 0 THEN TRUE ELSE BestStr.butik = iButSV) OUTER-JOIN,
     FIRST ttDummy WHERE AVAILABLE beststr OR cSkomodus = "1"
    BY BestHode.BestNr DESCENDING INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _OrdList          = "skotex.BestHode.BestNr|no"
     _Where[1]         = "BestHode.ArtikkelNr = (if available ArtBas
                         then ArtBas.ArtikkelNr
                         else -99) AND
(STRING(BestHode.BestStat) MATCHES cBestStatSV)
 and (if cbutsv <> ""*"" then string(bestlinje.butik) = cbutsv ELSE TRUE)"
     _Query            is OPENED
*/  /* BROWSE BROWSE-Bestilling */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-Ordre
/* Query rebuild information for FRAME FRAME-Ordre
     _Query            is NOT OPENED
*/  /* FRAME FRAME-Ordre */
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


&Scoped-define SELF-NAME B-OppdBest
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-OppdBest C-Win
ON CHOOSE OF B-OppdBest IN FRAME FRAME-Ordre /* Oppdater browser */
DO:
  {&OPEN-QUERY-{&BROWSE-NAME}}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-Bestilling
&Scoped-define SELF-NAME BROWSE-Bestilling
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-Bestilling C-Win
ON MOUSE-SELECT-DBLCLICK OF BROWSE-Bestilling IN FRAME FRAME-Ordre
DO:
  apply "CHOOSE":U to BUTTON-EndreBest in frame FRAME-Ordre.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-Bestilling C-Win
ON VALUE-CHANGED OF BROWSE-Bestilling IN FRAME FRAME-Ordre
DO:
  RUN ButtonEnaDis.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Avskriv
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Avskriv C-Win
ON CHOOSE OF BUTTON-Avskriv IN FRAME FRAME-Ordre /* Avskriv rest... */
DO:
  DEF VAR bOk AS LOG NO-UNDO.

  IF NOT AVAILABLE BestHode THEN 
      RETURN.
  IF BestHode.BestStat < 3 OR 
     BestHode.BestStat > 5  THEN 
  DO:
      MESSAGE "Bestilling med denne status kan ikke avskrives!"
        VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
  END.  
  MESSAGE "Skal bestillingen avskrives?"
    VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE bOk.
  IF NOT bOk THEN
      RETURN NO-APPLY.

  RUN besthode_avskriv_rest.p (BestHode.BestNr).
  FIND CURRENT BestHode NO-LOCK.
  BROWSE {&BROWSE-NAME}:REFRESH().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-EndreBest
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-EndreBest C-Win
ON CHOOSE OF BUTTON-EndreBest IN FRAME FRAME-Ordre /* Endre... */
DO:
  if not available BestHode then
    return no-apply.
  create tmpChild.
  ASSIGN
    wBestHodeRecid = RECID(BestHode).
  run w-gridord.w persistent set tmpChild.wChild (input RECID(ArtBas), input-output wBestHodeRecid, "ENDRE").
/*   FIND BestHode NO-LOCK WHERE                    */
/*       recid(BestHode) = wBestHodeRecid NO-ERROR. */
/*     ASSIGN wCurrent-Window:SENSITIVE = FALSE.                              */
/*     RUN w-gridord.w (RECID(ArtBas), INPUT-OUTPUT wBestHodeRecid, "ENDRE"). */
/*     ASSIGN wCurrent-Window:SENSITIVE = TRUE.                               */
/*     IF VALID-HANDLE(wParentHandle) THEN                                    */
/*         RUN SetWindowSensitive IN wParentHandle.                           */
/*     FIND CURRENT BestHode NO-LOCK.                                         */
/*     BROWSE {&BROWSE-NAME}:REFRESH().                                       */
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Innleveranse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Innleveranse C-Win
ON CHOOSE OF BUTTON-Innleveranse IN FRAME FRAME-Ordre /* Varemottak... */
DO:
  IF ArtBas.LopNr = 0 or ArtBas.LopNr = ? THEN DO:
      MESSAGE Tx("Artikkelen må tildeles løpenummer før innleveranse kan gjøres!",133)
        VIEW-AS ALERT-BOX MESSAGE TITLE Tx("Melding",107).
      RETURN NO-APPLY.
  END.
  IF BestHode.BestStat < 4  THEN DO:
      MESSAGE Tx("Bestilling med denne status kan ikke innleveres!",134)
        VIEW-AS ALERT-BOX TITLE Tx("Melding",107).
      RETURN NO-APPLY.
  END.  
  ASSIGN wBestHodeRecid = RECID(BestHode).
  
  /*ASSIGN wCurrent-Window:SENSITIVE = FALSE.*/
  RUN w-gridinnlev.w (RECID(ArtBas), INPUT-OUTPUT wBestHodeRecid, "INLEV").
  ASSIGN wCurrent-Window:SENSITIVE = TRUE.
  IF VALID-HANDLE(wParentHandle) THEN
      RUN SetWindowSensitive IN wParentHandle.
  
  FIND CURRENT BestHode NO-LOCK.
  BROWSE {&BROWSE-NAME}:REFRESH().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Kalkyle
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Kalkyle C-Win
ON CHOOSE OF BUTTON-Kalkyle IN FRAME FRAME-Ordre /* Kalkyle... */
DO:
  /*ASSIGN wCurrent-Window:SENSITIVE = FALSE.*/
  RUN d-vbestkalkyle.w (RECID(ArtBas), RECID(BestHode)).
  ASSIGN wCurrent-Window:SENSITIVE = TRUE.
  IF VALID-HANDLE(wParentHandle) THEN
      RUN SetWindowSensitive IN wParentHandle.
  FIND CURRENT BestHode NO-LOCK.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-NyBest
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-NyBest C-Win
ON CHOOSE OF BUTTON-NyBest IN FRAME FRAME-Ordre /* Ny... */
DO:
    if not available ArtBas then
      return no-apply.
    if ArtBas.Utgatt = true then   
      do:
        message Tx("Artikkelen er utgått. Den må settes som ikke utgått før bestilling kan registreres.",124)
          view-as alert-box message title Tx("Melding",107).
        return no-apply.
      end.
    if ArtBas.Lager = false then   
      do:
        message Tx("Artikkelen har ikke lagerstyring. Bestilling kan ikke registreres.",125)
          view-as alert-box message title Tx("Melding",107).
        return no-apply.
      end.
    ASSIGN /*wCurrent-Window:SENSITIVE = FALSE*/
           wBestHodeRecid = ?.
/*     run w-gridord.w (RECID(ArtBas), input-output wBestHodeRecid, "NY"). */
/*     ASSIGN wCurrent-Window:SENSITIVE = TRUE.                            */
/*     IF VALID-HANDLE(wParentHandle) THEN                                 */
/*         RUN SetWindowSensitive IN wParentHandle.                        */
/*     {&OPEN-QUERY-{&BROWSE-NAME}}                                        */

    create tmpChild.
    assign
      wBestHodeRecid = ?.
    run w-gridord.w persistent set tmpChild.wChild
           (RECID(ArtBas), input-output wBestHodeRecid, "NY").
/*     FIND BestHode NO-LOCK WHERE                    */
/*         recid(BestHode) = wBestHodeRecid NO-ERROR. */
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SlettBest
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SlettBest C-Win
ON CHOOSE OF BUTTON-SlettBest IN FRAME FRAME-Ordre /* Slette */
DO:

  RUN SlettBestilling.

  RETURN no-apply.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CB-BestStat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-BestStat C-Win
ON VALUE-CHANGED OF CB-BestStat IN FRAME FRAME-Ordre /* Bestillingsstatus */
DO:
ASSIGN CB-BestStat
     iBestStatSV = INPUT CB-BestStat.
    {&OPEN-QUERY-{&BROWSE-NAME}}

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CB-Butikk
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-Butikk C-Win
ON VALUE-CHANGED OF CB-Butikk IN FRAME FRAME-Ordre /* Butikk */
DO:
/*     cBestStatSV = CB-BestStat:SCREEN-VALUE. */
    ASSIGN CB-Butikk.
    iButSV = CB-Butikk.
    {&OPEN-QUERY-{&BROWSE-NAME}}

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
ON CLOSE OF THIS-PROCEDURE 
  do:
/*    RUN SaveBrowseSettings. */
    if valid-handle(wParentHandle) then
      run SlettProg in wParentHandle.
    RUN disable_UI.
  end.
  {syspara.i 1 1 18 cHKinst}
  {syspara.i 5 1 1 iClbut INT}
  {syspara.i 1 1 54 cSkomodus}
/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
    CREATE ttDummy.
  FIND ArtBas WHERE ArtBas.ArtikkelNr = dArtikkelNr NO-LOCK.
  RUN InitCB.
  RUN InitCBbut.
  RUN enable_UI.

  {lng.i}

  SUBSCRIBE TO "ByttObjekt" IN wParentHandle NO-ERROR.
  SUBSCRIBE TO "RefreshBest" ANYWHERE.
  BROWSE {&BROWSE-NAME}:SET-REPOSITIONED-ROW(BROWSE {&BROWSE-NAME}:DOWN,"CONDITIONAL").
  APPLY "VALUE-CHANGED" TO BROWSE {&BROWSE-NAME}.
  APPLY "ENTRY" TO BROWSE {&BROWSE-NAME}.
  FRAME FRAME-Ordre:SENSITIVE = artbas.sanertdato = ?.

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ButtonEnaDis C-Win 
PROCEDURE ButtonEnaDis :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:
      IF /*ArtBas.Aktivert = FALSE OR*/
         ArtBas.Lager    = FALSE OR 
         ArtBas.Pakke    = TRUE OR 
         ArtBas.OPris    = TRUE THEN
         ASSIGN BUTTON-NyBest:SENSITIVE       = FALSE
                BUTTON-EndreBest:SENSITIVE    = FALSE
                BUTTON-Kalkyle:SENSITIVE      = FALSE
                BUTTON-SlettBest:SENSITIVE    = FALSE
                BUTTON-Innleveranse:SENSITIVE = FALSE
                B-OppdBest:SENSITIVE = FALSE.
      ELSE DO:
          ASSIGN
            BUTTON-NyBest:SENSITIVE       = NOT ArtBas.Pakke = TRUE
            BUTTON-EndreBest:SENSITIVE    = BROWSE BROWSE-Bestilling:FOCUSED-ROW <> ?
            BUTTON-Kalkyle:SENSITIVE      = BUTTON-EndreBest:SENSITIVE
            BUTTON-SlettBest:SENSITIVE    = BUTTON-EndreBest:SENSITIVE
            BUTTON-Innleveranse:SENSITIVE = /*cHKinst = "no" AND*/ BUTTON-EndreBest:SENSITIVE
            B-OppdBest:SENSITIVE          = TRUE.
      END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ByttObjekt C-Win 
PROCEDURE ByttObjekt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER ipArtikkelNr LIKE ArtBas.ArtikkelNr NO-UNDO.
  ASSIGN dArtikkelNr = ipArtikkelNr.
  FIND ArtBas WHERE ArtBas.ArtikkelNr = dArtikkelNr NO-LOCK.
  FRAME FRAME-Ordre:SENSITIVE = artbas.sanertdato = ?.
  {sww.i}  
  {&OPEN-QUERY-{&BROWSE-NAME}}
  APPLY "VALUE-CHANGED" TO BROWSE {&BROWSE-NAME}.
  RUN ButtonEnaDis.
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
  HIDE FRAME FRAME-Ordre.
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
  DISPLAY CB-BestStat CB-Butikk 
      WITH FRAME FRAME-Ordre.
  ENABLE BUTTON-Avskriv CB-BestStat CB-Butikk BROWSE-Bestilling B-OppdBest 
         BUTTON-EndreBest BUTTON-Innleveranse BUTTON-Kalkyle BUTTON-NyBest 
         BUTTON-SlettBest 
      WITH FRAME FRAME-Ordre.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-Ordre}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitCB C-Win 
PROCEDURE InitCB :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def var wBestStatus as char no-undo.
  
  {syspara.i 1 100 1 cAlle}
  ASSIGN cAlle = TRIM(cAlle).
  IF cAlle = "" THEN
      ASSIGN cAlle = "[Alle]".
  
  
  ASSIGN wBestStatus = cAlle + ",0".

  SYSPARA:
  for each SysPAra no-lock where
    SysPara.SysHId = 5 and
    SysPara.SysGr  = 2 and
    SysPara.ParaNr < 99:
    
    assign
      wBestStatus = wBestStatus + 
                   (if wBestStatus = "" 
                      then ""
                      else ",") +
                   string(SysPara.ParaNr,"z9") + ": " + SysPara.Parameter1 + "," + string(SysPara.ParaNr).    
  end. /* SYSPARA */

  assign
    CB-BestStat:LIST-ITEM-PAIRS in frame {&FRAME-NAME} = wBestStatus
    CB-BestStat = 0
    iBestStatSV = 0.

  
  display CB-BestStat with frame {&FRAME-NAME}.
/*   ASSIGN CB-BestType:LIST-ITEM-PAIRS = cAlle + ",0,Grunnbestilling,1,Tilleggsbestilling,2" */
/*          CB-BestType:SCREEN-VALUE = "0".                                                   */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitCBbut C-Win 
PROCEDURE InitCBbut :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cListItemPairs AS CHARACTER  NO-UNDO.
    DO WITH FRAME {&FRAME-NAME}:
        FIND bruker WHERE bruker.brukerid = USERID("skotex") NO-LOCK.
        ASSIGN cListItemPairs = "".
        FOR EACH ButikkTeam NO-LOCK WHERE ButikkTeam.BrGrpNr = Bruker.BrGrpNr AND
                                  ButikkTeam.TeamTypeId = 1 BY ButikkTeam.Beskrivelse.
            FOR EACH ButikkKobling OF ButikkTeam.
                IF NOT CAN-FIND(FIRST TT_But WHERE TT_But.butikknr = ButikkKobling.butik) THEN DO:
                    CREATE TT_But.
                    ASSIGN tt_but.butikknr = ButikkKobling.butik.
                END.
            END.
        END.
        FOR EACH tt_but BY tt_but.butikknr:
            FIND butiker WHERE butiker.butik = tt_but.butikknr NO-LOCK NO-ERROR.
            IF AVAIL butiker THEN
                ASSIGN cListItemPairs = cListItemPairs + 
                                        (IF cListItemPairs <> "" THEN "," ELSE "") + 
                                        STRING(butiker.butik,"zzzz9") + ": " + replace(butiker.butnamn,","," ") + "," + string(butiker.butik).
        END.
        FIND tt_but WHERE tt_but.butikknr = bruker.butikknr NO-ERROR.
        ASSIGN CB-Butikk:LIST-ITEM-PAIRS = cAlle + "," + "0," + cListItemPairs.
    END.
    
    CB-Butikk = IF AVAIL tt_but AND tt_but.butikknr <> iClbut THEN tt_but.butikknr ELSE 0.
    iButSV = CB-Butikk.
    APPLY "VALUE-CHANGED" TO CB-Butikk.

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
  
   IF FRAME FRAME-Ordre:MOVE-TO-TOP() THEN.
   APPLY "ENTRY" TO BROWSE {&BROWSE-NAME}.
/*    APPLY "ENTRY" TO BUTTON-Ny. */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NyBest C-Win 
PROCEDURE NyBest :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  if not available ArtBas then
    return no-apply.
  if ArtBas.Aktivert = false then   
    do:
      message Tx("Artikkelen er ikke aktivert. Den må aktiveres før bestilling kan registreres.",124)
        view-as alert-box message title Tx("Melding",107).
      return no-apply.
    end.
  if ArtBas.Lager = false then   
    do:
      message Tx("Artikkelen har ikke lagerstyring. Bestilling kan ikke registreres.",125)
        view-as alert-box message title Tx("Melding",107).
      return no-apply.
    end.

  create tmpChild.
  assign
    wBestHodeRecid = ?.
  run w-gridord.w persistent set tmpChild.wChild (RECID(ArtBas), input-output wBestHodeRecid, "NY").  
  FIND BestHode NO-LOCK WHERE
      recid(BestHode) = wBestHodeRecid NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpenQueryRep C-Win 
PROCEDURE OpenQueryRep :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*     DEFINE INPUT  PARAMETER rStrekKode AS ROWID      NO-UNDO. */
/*     {&OPEN-QUERY-{&BROWSE-NAME}}                              */
/*     REPOSITION {&BROWSE-NAME} TO ROWID rStrekKode.            */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RefreshBest C-Win 
PROCEDURE RefreshBest :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER cType AS CHARACTER  NO-UNDO.
/* NY */    FIND ArtBas WHERE ArtBas.ArtikkelNr = dArtikkelNr NO-LOCK.
    IF cType = "NY" THEN
        {&OPEN-QUERY-{&BROWSE-NAME}}
/* NY */    ELSE IF AVAIL BestHode AND BestHode.ArtikkelNr = ArtBas.ArtikkelNr and
/* NY */                               cType = "ENDRE" THEN
/* NY */        BROWSE {&BROWSE-NAME}:REFRESH() NO-ERROR.
/* NY */    ELSE
/* NY */        {&OPEN-QUERY-{&BROWSE-NAME}}
/*     ELSE IF cType = "ENDRE" THEN                  */
/*         BROWSE {&BROWSE-NAME}:REFRESH() NO-ERROR. */
/*     wCurrent-Window:ALWAYS-ON-TOP = TRUE. */
/*     wParentHandle:MOVE-TO-TOP(). */
/*         wParentHandle:WINDOW-STATE = 3. */
/*         wParentHandle:MOVE-TO-TOP().    */
    RUN ButtonEnaDis.
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
/*     IF BROWSE {&BROWSE-NAME}:FOCUSED-ROW <> ? THEN */
        APPLY "ENTRY" TO BROWSE {&BROWSE-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SlettBestilling C-Win 
PROCEDURE SlettBestilling :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME FRAME-Ordre:
  DEFINE VARIABLE lSvar            AS LOGICAL    NO-UNDO.
  ASSIGN wBestHodeRecid = RECID(BestHode).

  IF CAN-DO("4,5",STRING(BestHode.BestStat)) THEN DO:
      MESSAGE Tx("Bestillingen er sendt til leverandør eller delhvis innlevert." + CHR(13) +
              "Skal den alikevel slettes?",127)
          VIEW-AS ALERT-BOX BUTTONS YES-NO-CANCEL TITLE Tx("Bekreftelse",105)
          UPDATE lSvar.
  END.
  ELSE DO:  
      MESSAGE Tx("Skal bestilling slettes?",128)
          VIEW-AS ALERT-BOX BUTTONS YES-NO-CANCEL TITLE Tx("Bekreftelse",105) 
      UPDATE lSvar.
  END.
  IF lSvar THEN
  DO TRANSACTION:
      {sww.i}
      /* KanSlettes*/      
      RUN w-gridord.w (INPUT wArtBasRecid, INPUT-OUTPUT wBestHodeRecid, "SLETT").
      IF wBestHodeRecid = ? THEN      
          BROWSE-Bestilling:DELETE-CURRENT-ROW( ).
      {swn.i}
      APPLY "ENTRY":U TO BROWSE BROWSE-Bestilling.
      RUN ButtonEnaDis.
      RETURN "OK".
  END.
  ELSE RETURN 'AVBRYT'.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getBekreftet C-Win 
FUNCTION getBekreftet RETURNS CHARACTER
  (  ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR cTekst AS CHAR NO-UNDO.
      cTekst = STRING(Besthode.bekreftetdato <> ?," J/").

  RETURN cTekst.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION OrdreNummer C-Win 
FUNCTION OrdreNummer RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  def var wOrdreNr as char no-undo.

  if available BestHode then
    do:
      if BestHode.OrdreNr = ? then
        wOrdreNr = "".
      else if BestHode.OrdreNr  = 0 then
        wOrdreNr = "".
      else
        wOrdreNr = string(BestHode.OrdreNr,"zzzzzzz9").
    end.
  else
    wOrdreNr = "".

  RETURN wOrdreNr.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

