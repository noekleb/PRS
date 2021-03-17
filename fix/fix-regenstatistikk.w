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

/* Local Variable Definitions ---                                       */
DEF VAR wTotAntall AS DEC FORMAT "zzz,zzz,zz9" NO-UNDO.
DEF VAR wWork            AS DEC  NO-UNDO.
DEF VAR wWork2           AS DEC  NO-UNDO.
DEF VAR wWork3           AS DEC  NO-UNDO.
DEF VAR wStop            AS LOG  INITIAL FALSE NO-UNDO.
DEF VAR wDato            AS DATE NO-UNDO.
DEF VAR wTid             AS INT  NO-UNDO.
DEF VAR wSkjerm          AS CHAR NO-UNDO.
DEF VAR wTilbud          AS LOG  NO-UNDO.
DEF VAR wOk              AS INT  NO-UNDO.
DEF VAR wAntArtikkler    AS INT  NO-UNDO.
DEF VAR wAntProfiler     AS INT  NO-UNDO.
DEF VAR wProgram-Handle  AS HANDLE NO-UNDO.
DEF VAR wBatchNr         AS INT  NO-UNDO.
DEF VAR cbutlst          AS CHAR NO-UNDO.
  

DEF BUFFER bufLager  FOR Lager.
DEF BUFFER bufArtLag FOR ArtLag.

{runlib.i}
{incl/DevMode.i}
{incl/CustDevMode.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS TOGGLE-17 RECT-47 RECT-48 RECT-64 RECT-65 ~
RECT-66 TOGGLE-12 TOGGLE-1 Toggle-13 BUTTON-1 TOGGLE-14 TOGGLE-15 TOGGLE-18 ~
TOGGLE-2 TOGGLE-3 TOGGLE-4 TOGGLE-6 TOGGLE-8 TOGGLE-9 TOGGLE-11 CB-Aar ~
TOGGLE-5 FI-d1Dato FI-d2Dato TOGGLE-16 T-18 TOGGLE-19 TOGGLE-20 TOGGLE-21 ~
B-Start BtnDone 
&Scoped-Define DISPLAYED-OBJECTS TOGGLE-17 TOGGLE-12 TOGGLE-1 Toggle-13 ~
TOGGLE-14 FI-TotAntall TOGGLE-15 FI-Profil TOGGLE-18 FI-Transaksjon ~
TOGGLE-2 FI-Oppdatert TOGGLE-3 TOGGLE-4 TOGGLE-6 TOGGLE-7 FI-StartInfo ~
TOGGLE-8 FI-SluttInfo TOGGLE-9 FI-TidBrukt TOGGLE-10 TOGGLE-11 CB-Aar ~
TOGGLE-5 FI-d1Dato FI-d2Dato TOGGLE-16 T-18 TOGGLE-19 TOGGLE-20 TOGGLE-21 ~
FI-Tittel 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-Start 
     LABEL "&Start oppdatering" 
     SIZE 36 BY 1.14.

DEFINE BUTTON BtnDone DEFAULT 
     LABEL "&Avslutt" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON BUTTON-1 
     LABEL "Vis logg..." 
     SIZE 16 BY 1.

DEFINE VARIABLE CB-Aar AS INTEGER FORMAT ">>>9":U INITIAL 0 
     LABEL "År" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "0" 
     DROP-DOWN-LIST
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE FI-d1Dato AS DATE FORMAT "99/99/99":U 
     LABEL "Dato" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-d2Dato AS DATE FORMAT "99/99/99":U 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Oppdatert AS CHARACTER FORMAT "X(256)":U 
     LABEL "Antall oppdatert" 
     VIEW-AS FILL-IN 
     SIZE 50 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Profil AS CHARACTER FORMAT "X(256)":U 
     LABEL "Butikk" 
     VIEW-AS FILL-IN 
     SIZE 50 BY 1 NO-UNDO.

DEFINE VARIABLE FI-SluttInfo AS CHARACTER FORMAT "X(256)":U 
     LABEL "Ferdig" 
     VIEW-AS FILL-IN 
     SIZE 50 BY 1 NO-UNDO.

DEFINE VARIABLE FI-StartInfo AS CHARACTER FORMAT "X(256)":U 
     LABEL "Startet" 
     VIEW-AS FILL-IN 
     SIZE 50 BY 1 NO-UNDO.

DEFINE VARIABLE FI-TidBrukt AS CHARACTER FORMAT "X(256)":U 
     LABEL "Tid brukt" 
     VIEW-AS FILL-IN 
     SIZE 50 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Tittel AS CHARACTER FORMAT "X(256)":U INITIAL "           Kjøring av FIX rutiner" 
      VIEW-AS TEXT 
     SIZE 73 BY 1.38
     FGCOLOR 1 FONT 8 NO-UNDO.

DEFINE VARIABLE FI-TotAntall AS CHARACTER FORMAT "X(256)":U 
     LABEL "Antall å oppdatere" 
     VIEW-AS FILL-IN 
     SIZE 39 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Transaksjon AS CHARACTER FORMAT "X(256)":U 
     LABEL "Transaksjon" 
     VIEW-AS FILL-IN 
     SIZE 50 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-47
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 72 BY 7.14.

DEFINE RECTANGLE RECT-48
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 73 BY 4.76.

DEFINE RECTANGLE RECT-64
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 72 BY 1.29.

DEFINE RECTANGLE RECT-65
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 72 BY 1.43.

DEFINE RECTANGLE RECT-66
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 87 BY 1.43.

DEFINE VARIABLE T-18 AS LOGICAL INITIAL no 
     LABEL "Rens ~"Sovende dame~" (Bilde)" 
     VIEW-AS TOGGLE-BOX
     SIZE 48 BY .81 NO-UNDO.

DEFINE VARIABLE TOGGLE-1 AS LOGICAL INITIAL no 
     LABEL "Korr. av vvarekost i lager." 
     VIEW-AS TOGGLE-BOX
     SIZE 46 BY .81 NO-UNDO.

DEFINE VARIABLE TOGGLE-10 AS LOGICAL INITIAL no 
     LABEL "Rens VPI importtabeller" 
     VIEW-AS TOGGLE-BOX
     SIZE 46 BY .81 NO-UNDO.

DEFINE VARIABLE TOGGLE-11 AS LOGICAL INITIAL no 
     LABEL "Kjør SYSINIT" 
     VIEW-AS TOGGLE-BOX
     SIZE 47 BY .81 NO-UNDO.

DEFINE VARIABLE TOGGLE-12 AS LOGICAL INITIAL no 
     LABEL "Kontroller/korriger strekkoder fra PRICAT." 
     VIEW-AS TOGGLE-BOX
     SIZE 47 BY .81 NO-UNDO.

DEFINE VARIABLE Toggle-13 AS LOGICAL INITIAL no 
     LABEL "Korr av SVK i lager" 
     VIEW-AS TOGGLE-BOX
     SIZE 41 BY .81 NO-UNDO.

DEFINE VARIABLE TOGGLE-14 AS LOGICAL INITIAL no 
     LABEL "Korr lager/artlag for arttikler ute av synk" 
     VIEW-AS TOGGLE-BOX
     SIZE 48.2 BY .81 NO-UNDO.

DEFINE VARIABLE TOGGLE-15 AS LOGICAL INITIAL no 
     LABEL "Korr av lager med vvarekost = 0." 
     VIEW-AS TOGGLE-BOX
     SIZE 50 BY .81 NO-UNDO.

DEFINE VARIABLE TOGGLE-16 AS LOGICAL INITIAL no 
     LABEL "Regenerer time og dagsrapport (On-Line)" 
     VIEW-AS TOGGLE-BOX
     SIZE 48 BY .81 NO-UNDO.

DEFINE VARIABLE TOGGLE-17 AS LOGICAL INITIAL no 
     LABEL "Rens DB for ~"løse~" poster." 
     VIEW-AS TOGGLE-BOX
     SIZE 48 BY .81 NO-UNDO.

DEFINE VARIABLE TOGGLE-18 AS LOGICAL INITIAL no 
     LABEL "Korr av lager med artlag fra flere artikkler" 
     VIEW-AS TOGGLE-BOX
     SIZE 50 BY .81 NO-UNDO.

DEFINE VARIABLE TOGGLE-19 AS LOGICAL INITIAL no 
     LABEL "Initiering av utvidetsøk" 
     VIEW-AS TOGGLE-BOX
     SIZE 42 BY .81 NO-UNDO.

DEFINE VARIABLE TOGGLE-2 AS LOGICAL INITIAL no 
     LABEL "Korri. StLager." 
     VIEW-AS TOGGLE-BOX
     SIZE 47 BY .81 NO-UNDO.

DEFINE VARIABLE TOGGLE-20 AS LOGICAL INITIAL no 
     LABEL "Initiering av VPI informasjon" 
     VIEW-AS TOGGLE-BOX
     SIZE 43 BY .81 NO-UNDO.

DEFINE VARIABLE TOGGLE-21 AS LOGICAL INITIAL no 
     LABEL "Korreksjon av pakkseddelpriser" 
     VIEW-AS TOGGLE-BOX
     SIZE 46 BY .81 NO-UNDO.

DEFINE VARIABLE TOGGLE-3 AS LOGICAL INITIAL no 
     LABEL "Korr. dobbelposterte gavekort." 
     VIEW-AS TOGGLE-BOX
     SIZE 47 BY .81 NO-UNDO.

DEFINE VARIABLE TOGGLE-4 AS LOGICAL INITIAL no 
     LABEL "Initiering av Hg/AvdNr i Størrelsestype" 
     VIEW-AS TOGGLE-BOX
     SIZE 47 BY .81 NO-UNDO.

DEFINE VARIABLE TOGGLE-5 AS LOGICAL INITIAL no 
     LABEL "Regenerering av statistikk." 
     VIEW-AS TOGGLE-BOX
     SIZE 47 BY .81 NO-UNDO.

DEFINE VARIABLE TOGGLE-6 AS LOGICAL INITIAL no 
     LABEL "Initiering av prispunkt (Anbefalt pris)" 
     VIEW-AS TOGGLE-BOX
     SIZE 47 BY .81 NO-UNDO.

DEFINE VARIABLE TOGGLE-7 AS LOGICAL INITIAL no 
     LABEL "Initiering av tilgodelapper og gavekort" 
     VIEW-AS TOGGLE-BOX
     SIZE 46 BY .81 NO-UNDO.

DEFINE VARIABLE TOGGLE-8 AS LOGICAL INITIAL no 
     LABEL "Initier bonghodeflagg" 
     VIEW-AS TOGGLE-BOX
     SIZE 47 BY .81 NO-UNDO.

DEFINE VARIABLE TOGGLE-9 AS LOGICAL INITIAL no 
     LABEL "Initiering av vareslag" 
     VIEW-AS TOGGLE-BOX
     SIZE 44 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     TOGGLE-17 AT ROW 1 COL 98.8
     TOGGLE-12 AT ROW 1.95 COL 98.6
     TOGGLE-1 AT ROW 3.14 COL 98.6
     Toggle-13 AT ROW 4.19 COL 98.8
     BUTTON-1 AT ROW 5.24 COL 81.2
     TOGGLE-14 AT ROW 5.33 COL 98.8
     FI-TotAntall AT ROW 5.57 COL 23 COLON-ALIGNED
     TOGGLE-15 AT ROW 6.52 COL 98.8
     FI-Profil AT ROW 6.76 COL 23 COLON-ALIGNED
     TOGGLE-18 AT ROW 7.57 COL 98.8
     FI-Transaksjon AT ROW 7.95 COL 23 COLON-ALIGNED
     TOGGLE-2 AT ROW 8.62 COL 98.6
     FI-Oppdatert AT ROW 9.14 COL 23 COLON-ALIGNED
     TOGGLE-3 AT ROW 9.81 COL 98.6
     TOGGLE-4 AT ROW 11 COL 98.6
     TOGGLE-6 AT ROW 12.19 COL 98.6
     TOGGLE-7 AT ROW 13.38 COL 98.6
     FI-StartInfo AT ROW 13.62 COL 23 COLON-ALIGNED
     TOGGLE-8 AT ROW 14.43 COL 98.6
     FI-SluttInfo AT ROW 14.81 COL 23 COLON-ALIGNED
     TOGGLE-9 AT ROW 15.52 COL 98.6
     FI-TidBrukt AT ROW 16 COL 23 COLON-ALIGNED
     TOGGLE-10 AT ROW 16.71 COL 98.6
     TOGGLE-11 AT ROW 17.67 COL 98.6
     CB-Aar AT ROW 18.76 COL 79 COLON-ALIGNED
     TOGGLE-5 AT ROW 18.86 COL 98.6
     FI-d1Dato AT ROW 20.14 COL 66.4 COLON-ALIGNED
     FI-d2Dato AT ROW 20.14 COL 81 COLON-ALIGNED NO-LABEL
     TOGGLE-16 AT ROW 20.24 COL 98.8
     T-18 AT ROW 21.71 COL 99
     TOGGLE-19 AT ROW 22.81 COL 99
     TOGGLE-20 AT ROW 23.86 COL 99
     TOGGLE-21 AT ROW 25.05 COL 99
     B-Start AT ROW 27.1 COL 3
     BtnDone AT ROW 27.1 COL 134
     FI-Tittel AT ROW 1.62 COL 3 NO-LABEL
     RECT-47 AT ROW 3.14 COL 4
     RECT-48 AT ROW 13.14 COL 3
     RECT-64 AT ROW 18.62 COL 76
     RECT-65 AT ROW 5.05 COL 76
     RECT-66 AT ROW 20 COL 61.2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 148 BY 27.33
         DEFAULT-BUTTON BtnDone.


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
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Oppdaterer lagertransaksjoner"
         HEIGHT             = 27.33
         WIDTH              = 148
         MAX-HEIGHT         = 27.33
         MAX-WIDTH          = 148
         VIRTUAL-HEIGHT     = 27.33
         VIRTUAL-WIDTH      = 148
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



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   NOT-VISIBLE FRAME-NAME                                               */
ASSIGN 
       B-Start:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "OPPDAT".

/* SETTINGS FOR FILL-IN FI-Oppdatert IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Profil IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-SluttInfo IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-StartInfo IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-TidBrukt IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Tittel IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN FI-TotAntall IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Transaksjon IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TOGGLE-10 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TOGGLE-7 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME DEFAULT-FRAME
/* Query rebuild information for FRAME DEFAULT-FRAME
     _Query            is NOT OPENED
*/  /* FRAME DEFAULT-FRAME */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Oppdaterer lagertransaksjoner */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Oppdaterer lagertransaksjoner */
DO:
  IF valid-handle(wProgram-Handle) THEN
    RETURN NO-APPLY.

  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Start
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Start C-Win
ON CHOOSE OF B-Start IN FRAME DEFAULT-FRAME /* Start oppdatering */
DO:
  
  ASSIGN
    wDato = TODAY
    wTid  = TIME.

  DEF-FRAME:
  DO WITH FRAME {&FRAME-NAME}:
      /* Renser DB for løse poster. */
      IF INPUT Toggle-17 = TRUE THEN DO:
          ASSIGN FI-Profil:SCREEN-VALUE = "T17: Renser DB for løse poster.".
          RUN fix-rens-lose-poster.p.
          ASSIGN FI-Profil:SCREEN-VALUE = "T17: Ferdig - Renser DB for løse poster.".
      END.

      /* Korreksjon av vektet varekost. */
      IF INPUT Toggle-12 = TRUE THEN DO:
          ASSIGN FI-Profil:SCREEN-VALUE = "T12: Korr av strekkoder.".
           RUN value('fix-korr-av-12 siffers-strekkoder.p').
          ASSIGN FI-Profil:SCREEN-VALUE = "T12: Ferdig - Korr av EAN koder.".
      END.

      /* Korreksjon av vektet varekost. */
      IF INPUT Toggle-1 = TRUE THEN DO:
          ASSIGN FI-Profil:SCREEN-VALUE = "T1: Korr vektet varekost.".
           RUN fix-korrvvarekostilager.p.
          ASSIGN FI-Profil:SCREEN-VALUE = "T1: Ferdig - Korr vektet varekost.".
      END.

      /* Korreksjon av SVK i lager. */
      IF INPUT Toggle-13 = TRUE THEN DO:
          ASSIGN FI-Profil:SCREEN-VALUE = "T1: Korr SVK i lager.".
           RUN fix-korrsvkilager.p.
          ASSIGN FI-Profil:SCREEN-VALUE = "T1: Ferdig - Korr SVK i lager.".
      END.

      /* Korreksjon av artikler ute av synk. */
      IF INPUT Toggle-14 = TRUE THEN DO:
          ASSIGN FI-Profil:SCREEN-VALUE = "T14: Korr. lager/artlag art. ute av synk.".
           RUN fix-kall-korr-lager-og-artlag.p.
          ASSIGN FI-Profil:SCREEN-VALUE = "T14: Ferdig - Korr. lager/artlag art. ute av synk.".
      END.

      /* Korreksjon av artikler med 0 i vvarekost. */
      IF INPUT Toggle-15 = TRUE THEN DO:
          ASSIGN FI-Profil:SCREEN-VALUE = "T15: Korr. lager med 0 i varekost".
           RUN fix-korrvvarekostilagernull.p.
          ASSIGN FI-Profil:SCREEN-VALUE = "T15: Ferdig - Korr. lager med 0 i varekost. ute av synk.".
      END.

      /* Korreksjon av artikler med 0 i vvarekost. */
      IF INPUT Toggle-18 = TRUE THEN DO:
          ASSIGN FI-Profil:SCREEN-VALUE = "T18: Korr. lager med artlag fra flere art.nr".
           RUN fix-regenartlag_m_ulikeartnr.p.
          ASSIGN FI-Profil:SCREEN-VALUE = "T18: Ferdig - Korr. lager med artlag fra flere art.nr".
      END.

      /* Korreksjon av StLager. */
      IF INPUT Toggle-2 = TRUE THEN DO:
          RUN byggButLst (OUTPUT cButLst).
          ASSIGN FI-Profil:SCREEN-VALUE = "T2: Korr StLager for butikker: " + cButLst.
          IF TRIM(cbutlst) <> '' THEN
              RUN byggomstlager.w (cButLst).
          ASSIGN FI-Profil:SCREEN-VALUE = "T2: Ferdig - Korr StLager.".
      END.

      /* Sletter dobbelposterte gavekort. */
      IF INPUT Toggle-3 = TRUE THEN DO:
          ASSIGN FI-Profil:SCREEN-VALUE = "T3: Korr rens gavekort.".
          RUN fix-renskundetrans.p.
          ASSIGN FI-Profil:SCREEN-VALUE = "T3: Ferdig - Korr rens gavekort.".
      END.

      /* Initiering av StrType. */
      IF INPUT Toggle-4 = TRUE THEN DO:
          ASSIGN FI-Profil:SCREEN-VALUE = "T4: Init av StrType.".
          RUN fix-initstrtypehg.p.
          ASSIGN FI-Profil:SCREEN-VALUE = "T4: Ferdig - Init av StrType.".
      END.

      /* Initiering av StrType. */
      IF INPUT Toggle-6 = TRUE THEN DO:
          ASSIGN FI-Profil:SCREEN-VALUE = "T6: Init av prispunkt.".
          RUN fix-initanbefaltpris.p.
          ASSIGN FI-Profil:SCREEN-VALUE = "T6: Ferdig - Init av prispunkt.".
      END.

      /* Initiering av tilgodelapper og gavekort. */
      IF INPUT Toggle-7 = TRUE THEN DO:
          ASSIGN FI-Profil:SCREEN-VALUE = "T7: Init av tilgode og gavekort.".
          RUN fix-gengaveogtilgodekort.p.
          ASSIGN FI-Profil:SCREEN-VALUE = "T7: Ferdig - Init av tilgode og gavekort.".
      END.

      /* Initiering av tilgodelapper og gavekort. */
      IF INPUT Toggle-8 = TRUE THEN DO:
          ASSIGN FI-Profil:SCREEN-VALUE = "T8: Init av bonghodeflagg.".
          RUN fix-settbonghodeflagg.p.
          ASSIGN FI-Profil:SCREEN-VALUE = "T8: Ferdig - Init av bonghodeflagg.".
      END.

      /* Initiering av vareslag. */
      IF INPUT Toggle-9 = TRUE THEN DO:
          ASSIGN FI-Profil:SCREEN-VALUE = "T9: Init av vareslag.".
          RUN fix-vareslag.p.
          ASSIGN FI-Profil:SCREEN-VALUE = "T9: Ferdig - Init av vareslag.".
      END.

      /* Initiering av vareslag. */
      IF INPUT Toggle-10 = TRUE THEN DO:
          ASSIGN FI-Profil:SCREEN-VALUE = "T10: Renst av VPI importtabell.".
          RUN Fix-rensvpiforlev.p.
          ASSIGN FI-Profil:SCREEN-VALUE = "T10: Ferdig - Renst av VPI importtabell.".
      END.

      /* Initiering av vareslag. */
      IF INPUT Toggle-11 = TRUE THEN DO:
          ASSIGN FI-Profil:SCREEN-VALUE = "T11: Kjører sysinit.".
          RUN sysinit.p.
          ASSIGN FI-Profil:SCREEN-VALUE = "T11: Ferdig - Kjører sysinit.".
      END.

      /* Tømmer statistikkene */
      IF INPUT Toggle-5 = TRUE THEN DO:
          ASSIGN FI-Profil:SCREEN-VALUE = "T5: Korr av statistikk.".
          ASSIGN
              FI-Profil:SCREEN-VALUE = "Sletter statistikker"
              .
          ASSIGN
              FI-Profil:SCREEN-VALUE = ""
              .
          RUN fix-oppdlagertrans.w PERSISTENT SET wProgram-Handle (THIS-PROCEDURE:handle, wBatchNr, INPUT INPUT CB-Aar).      
      END.

      /* Korreksjon av vektet varekost. */
      IF INPUT Toggle-16 = TRUE THEN DO:
          ASSIGN FI-Profil:SCREEN-VALUE = "T16: Korr time og aktrapp.".
           RUN fix-korr-time-og-dagsrapp.p (INPUT INPUT FI-d1Dato, INPUT INPUT FI-d2Dato).
          ASSIGN FI-Profil:SCREEN-VALUE = "T16: Ferdig - Korr Korr time og aktrapp.".
      END.

      /* Tømmer statistikkene */
      IF INPUT T-18 = TRUE THEN DO:
          ASSIGN FI-Profil:SCREEN-VALUE = "T18: Rens av sovende dame.".
          ASSIGN
              FI-Profil:SCREEN-VALUE = "Renser...(Sovende dame)"
              .
          FOR EACH BildeRegister EXCLUSIVE-LOCK WHERE
              Bilderegister.FilNavn = "0.jpg":
              FOR EACH BildeData OF BildeRegister EXCLUSIVE-LOCK:
                  DELETE BildeData.
              END.
              DELETE Bilderegister.
          END.
      END.

      /* Initiering av utvidet søk. */
      IF INPUT TOGGLE-19 = TRUE THEN DO:
          ASSIGN FI-Profil:SCREEN-VALUE = "T19: Initiering av utvidet søk.".
           RUN fix-utvidetsok.
          ASSIGN FI-Profil:SCREEN-VALUE = "T19: Ferdig - Utvidetsøk.".
      END.

      IF INPUT TOGGLE-20 = TRUE THEN
      DO:
          ASSIGN FI-Profil:SCREEN-VALUE = "T20: Initiering av VPI informasjon.".
           RUN fix-initArtikkelVPIInformasjon.p.
          ASSIGN FI-Profil:SCREEN-VALUE = "T20: Ferdig - VPI informasjon.".
      END.

      IF INPUT TOGGLE-21 = TRUE THEN
      DO:
          ASSIGN FI-Profil:SCREEN-VALUE = "T20: Initiering av VPI informasjon.".
           RUN fix-pksdlpris.p.
          ASSIGN FI-Profil:SCREEN-VALUE = "T20: Ferdig - VPI informasjon.".
      END.
      
      APPLY "choose" TO BtnDone.

  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnDone
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnDone C-Win
ON CHOOSE OF BtnDone IN FRAME DEFAULT-FRAME /* Avslutt */
DO:
  &IF "{&PROCEDURE-TYPE}" EQ "SmartPanel" &THEN
    &IF "{&ADM-VERSION}" EQ "ADM1.1" &THEN
      RUN dispatch IN THIS-PROCEDURE ('exit').
    &ELSE
      RUN exitObject.
    &ENDIF
  &ELSE
      APPLY "CLOSE":U TO THIS-PROCEDURE.
  &ENDIF
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-1 C-Win
ON CHOOSE OF BUTTON-1 IN FRAME DEFAULT-FRAME /* Vis logg... */
DO:
    IF DYNAMIC-FUNCTION("setWebDoc","open",SEARCH("Lagerkorr.txt")) NE "" THEN
    DYNAMIC-FUNCTION("DoMessage",0,0,"Korreksjonslogg eksisterer ikke","","").

        
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
/* Biblotek, inn/utmelding og ON CLOSE OF THIS... */
{genlib.i 
  &NoLibCall      = "Nei"
  &WindowName     = "Klargjøring av priskø"
}

ASSIGN wBatchNr = ?.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

SUBSCRIBE 'infoDisp' ANYWHERE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
    RUN InitCB.
  RUN enable_UI.
  {lng.i} 
  STATUS INPUT "".

  ASSIGN
      C-Win:HIDDEN = FALSE.
  
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AvbrytOppdatering C-Win 
PROCEDURE AvbrytOppdatering :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME DEFAULT-FRAME:
  
  IF wTotAntall = 0 THEN
    DO:
      MESSAGE "Ingen transaksjoner igjen å oppdatere"
        VIEW-AS ALERT-BOX MESSAGE TITLE "Melding".
      APPLY "close":U TO THIS-PROCEDURE.
    END.
    
  ASSIGN
    B-Start:label        = "&Start oppdatering"
    B-Start:Private-Data = "OPPDAT"
    FI-Profil            = ""
    FI-Transaksjon       = ""
    FI-Oppdatert         = ""
    FI-SluttInfo         = ""
    FI-StartInfo         = ""
    FI-TidBrukt          = "".

  DISPLAY
    FI-Profil
    FI-Transaksjon
    FI-Oppdatert  
    FI-SluttInfo  
    FI-StartInfo  
    FI-TidBrukt   
  WITH FRAME DEFAULT-FRAME.

END. /* FRAME */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BruktInfo C-Win 
PROCEDURE BruktInfo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER wDato      AS DATE NO-UNDO.
  DEF INPUT PARAMETER wFerdigTid AS INT  NO-UNDO.
  DEF INPUT PARAMETER wBruktTid  AS INT  NO-UNDO.
  
  DO WITH FRAME DEFAULT-FRAME:
    ASSIGN
      FI-SluttInfo:screen-value = string(wDato) + " " +
                                  string(wFerdigTid,"HH:MM:SS")
      FI-TidBrukt:screen-value  = string(wBruktTid,"HH:MM:SS").
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE byggButLst C-Win 
PROCEDURE byggButLst :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF OUTPUT PARAMETER pcButLst AS CHAR NO-UNDO.

  LES_BUTIKKER:
  FOR EACH Butiker NO-LOCK:
    IF Butiker.HarButikkSystem = FALSE THEN NEXT LES_BUTIKKER.  
    IF Butiker.Apningsdato     = ?     THEN NEXT LES_BUTIKKER. 
    IF Butiker.Apningsdato     > TODAY THEN NEXT LES_BUTIKKER. 
    IF (Butiker.NedlagtDato <> ? AND
        Butiker.NedlagtDato    <= TODAY) THEN NEXT LES_BUTIKKER. 
    ASSIGN
        pcButLst = pcButLst + ',' + 
                   STRING(Butiker.Butik).
  END. /* LES_BUTIKKER */
  pcButLst = TRIM(pcButLst,',').
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
  DISPLAY TOGGLE-17 TOGGLE-12 TOGGLE-1 Toggle-13 TOGGLE-14 FI-TotAntall 
          TOGGLE-15 FI-Profil TOGGLE-18 FI-Transaksjon TOGGLE-2 FI-Oppdatert 
          TOGGLE-3 TOGGLE-4 TOGGLE-6 TOGGLE-7 FI-StartInfo TOGGLE-8 FI-SluttInfo 
          TOGGLE-9 FI-TidBrukt TOGGLE-10 TOGGLE-11 CB-Aar TOGGLE-5 FI-d1Dato 
          FI-d2Dato TOGGLE-16 T-18 TOGGLE-19 TOGGLE-20 TOGGLE-21 FI-Tittel 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE TOGGLE-17 RECT-47 RECT-48 RECT-64 RECT-65 RECT-66 TOGGLE-12 TOGGLE-1 
         Toggle-13 BUTTON-1 TOGGLE-14 TOGGLE-15 TOGGLE-18 TOGGLE-2 TOGGLE-3 
         TOGGLE-4 TOGGLE-6 TOGGLE-8 TOGGLE-9 TOGGLE-11 CB-Aar TOGGLE-5 
         FI-d1Dato FI-d2Dato TOGGLE-16 T-18 TOGGLE-19 TOGGLE-20 TOGGLE-21 
         B-Start BtnDone 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE infoDisp C-Win 
PROCEDURE infoDisp :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER pcTekst AS CHAR NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
      ASSIGN
          FI-Oppdatert:SCREEN-VALUE = pcTekst
          .
  END.

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
    DEFINE VARIABLE iCount AS INTEGER    NO-UNDO.
    DEFINE VARIABLE cList-Items AS CHARACTER  NO-UNDO.
    DO iCount = YEAR(TODAY) TO YEAR(TODAY) - 5 BY -1:
        ASSIGN cList-Items = cList-Items + 
                (IF cList-Items <> "" THEN "," ELSE "") + STRING(iCount).
    END.
    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN CB-Aar:LIST-ITEMS = cList-Items
               CB-Aar            = INT(ENTRY(1,cList-Items)).
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OppdaterTrans C-Win 
PROCEDURE OppdaterTrans :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME DEFAULT-FRAME:

  IF wTotAntall = 0 THEN
    DO:
      MESSAGE "Ingen transaksjoner å eksportere"
        VIEW-AS ALERT-BOX MESSAGE TITLE "Melding".
      APPLY "close":U TO THIS-PROCEDURE.
    END.
    
  ASSIGN
    B-Start:label        = "&Avbryt eksport"
    B-Start:Private-Data = "AVBRYT".

  RUN Eksport.

END. /* FRAME */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProfilInfo C-Win 
PROCEDURE ProfilInfo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER wProfilInfo AS CHAR NO-UNDO.
  
  DO WITH FRAME DEFAULT-FRAME:
    ASSIGN
      FI-Profil:screen-value = wProfilInfo.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StartInfo C-Win 
PROCEDURE StartInfo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER wDato     AS DATE NO-UNDO.
  DEF INPUT PARAMETER wStartTid AS INT NO-UNDO.
  
  DO WITH FRAME DEFAULT-FRAME:
    ASSIGN
      FI-StartInfo:screen-value = string(wDato) + " " +
                                  string(wStartTid,"HH:MM:SS").
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE TransInfo C-Win 
PROCEDURE TransInfo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER wTransaksjon AS CHAR NO-UNDO.
  DEF INPUT PARAMETER wOppdatert   AS CHAR NO-UNDO.

  DO WITH FRAME DEFAULT-FRAME:
    ASSIGN
      FI-Transaksjon:screen-value = wTransaksjon
      FI-Oppdatert:screen-value   = wOppdatert.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

