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
DEF VAR bOk               AS LOG  NO-UNDO.
DEF VAR ix                AS INT  NO-UNDO.
DEF VAR iReturn           AS INT  NO-UNDO.
DEF VAR iKundeNr          AS DEC  NO-UNDO.

DEF BUFFER clButiker FOR Butiker.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-2 RECT-3 KobletTilKunde btnKundeDetSok ~
FI-CL btnButiker FraKortNr TilKortNr FI-GruppeId btnGruppeId FI-gyldighet ~
FI-SistBrukt BtnOK BtnDone 
&Scoped-Define DISPLAYED-OBJECTS KobletTilKunde KundeNavn FI-CL FI-ButNamn ~
FraKortNr TilKortNr FI-GruppeId FI-gyldighet FI-SistBrukt AntallKort ~
FI-Tekst1 FI-Tekst-2 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnButiker 
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON BtnDone DEFAULT 
     LABEL "Avbryt" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON btnGruppeId 
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON btnKundeDetSok 
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON BtnOK AUTO-GO DEFAULT 
     LABEL "Start generering" 
     SIZE 30 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE AntallKort AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Antall genererte kort" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE FI-ButNamn AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 29.6 BY 1 NO-UNDO.

DEFINE VARIABLE FI-CL AS INTEGER FORMAT ">>>>>9":U INITIAL 0 
     LABEL "Butikk" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE FI-GruppeId AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Kundegruppe" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE FI-gyldighet AS INTEGER FORMAT ">>9":U INITIAL 999 
     LABEL "Gyldighet" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE FI-SistBrukt AS CHARACTER FORMAT "X(256)":U 
     LABEL "Siste brukte kortnr." 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Tekst-2 AS CHARACTER FORMAT "X(256)":U INITIAL "Kundenummer settes = 0 hvis det skal genereres en kunde pr. kort." 
      VIEW-AS TEXT 
     SIZE 67 BY .62 NO-UNDO.

DEFINE VARIABLE FI-Tekst1 AS CHARACTER FORMAT "X(256)":U INITIAL "Kundenummer angis hvis kortene skal kobles mot en klubb/bedrift." 
      VIEW-AS TEXT 
     SIZE 67 BY .62 NO-UNDO.

DEFINE VARIABLE FraKortNr AS DECIMAL FORMAT ">>>>>9" INITIAL 0 
     LABEL "Fra/til kortnummer" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE KobletTilKunde AS DECIMAL FORMAT ">>>>>>>>>>>>9" INITIAL 0 
     LABEL "Kunde" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE KundeNavn AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE TilKortNr AS DECIMAL FORMAT ">>>>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 79 BY 4.67.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 79 BY 9.76.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     KobletTilKunde AT ROW 1.95 COL 24 COLON-ALIGNED HELP
          "Kundenummer"
     btnKundeDetSok AT ROW 1.95 COL 43 NO-TAB-STOP 
     KundeNavn AT ROW 1.95 COL 45.4 COLON-ALIGNED NO-LABEL
     FI-CL AT ROW 6.43 COL 24 COLON-ALIGNED
     btnButiker AT ROW 6.43 COL 43 NO-TAB-STOP 
     FI-ButNamn AT ROW 6.43 COL 45.4 COLON-ALIGNED NO-LABEL
     FraKortNr AT ROW 7.43 COL 24 COLON-ALIGNED HELP
          "Kundenummer"
     TilKortNr AT ROW 7.43 COL 45.4 COLON-ALIGNED HELP
          "Kundenummer" NO-LABEL
     FI-GruppeId AT ROW 8.43 COL 24 COLON-ALIGNED
     btnGruppeId AT ROW 8.43 COL 43 NO-TAB-STOP 
     FI-gyldighet AT ROW 9.43 COL 24 COLON-ALIGNED HELP
          "Antall dager kortet skal være gyldig."
     FI-SistBrukt AT ROW 13.1 COL 24 COLON-ALIGNED
     AntallKort AT ROW 14.14 COL 24 COLON-ALIGNED
     BtnOK AT ROW 15.76 COL 2
     BtnDone AT ROW 15.76 COL 65
     FI-Tekst1 AT ROW 3.62 COL 8 COLON-ALIGNED NO-LABEL
     FI-Tekst-2 AT ROW 4.57 COL 8 COLON-ALIGNED NO-LABEL
     RECT-2 AT ROW 1.1 COL 1.4
     RECT-3 AT ROW 5.76 COL 1.4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 16.


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
         TITLE              = "Generering av kunde/medlemskort"
         HEIGHT             = 16
         WIDTH              = 80
         MAX-HEIGHT         = 16
         MAX-WIDTH          = 80
         VIRTUAL-HEIGHT     = 16
         VIRTUAL-WIDTH      = 80
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

{incl/devmode.i}
{incl/custdevmode.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* SETTINGS FOR FILL-IN AntallKort IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-ButNamn IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       FI-SistBrukt:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN FI-Tekst-2 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       FI-Tekst-2:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN FI-Tekst1 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       FI-Tekst1:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN KundeNavn IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Generering av kunde/medlemskort */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Generering av kunde/medlemskort */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnButiker
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnButiker C-Win
ON CHOOSE OF btnButiker IN FRAME DEFAULT-FRAME /* ... */
DO:
  DEF VAR cButikerValueList    AS CHAR NO-UNDO.

  DO WITH FRAME default-frame:
      THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
      RUN JBoxLookup.w (THIS-PROCEDURE,50,
                        "Butiker;Butik;Butnamn"
                        ,"WHERE true"
                        ,""
                        ,"Butik,Butnamn"
                        ,OUTPUT cButikerValueList
                        ,OUTPUT bOK).
      THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

      IF bOk AND cButikerValueList NE "" THEN DO:
        ASSIGN FI-CL:SCREEN-VALUE = ENTRY(1,cButikerValueList,"|")
               FI-Butnamn:SCREEN-VALUE = ENTRY(2,cButikerValueList,"|").
        APPLY "any-printable" TO FI-CL.
      END.
      APPLY "entry" TO FI-CL.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnDone
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnDone C-Win
ON CHOOSE OF BtnDone IN FRAME DEFAULT-FRAME /* Avbryt */
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


&Scoped-define SELF-NAME btnGruppeId
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnGruppeId C-Win
ON CHOOSE OF btnGruppeId IN FRAME DEFAULT-FRAME /* ... */
DO:
  DEF VAR cGruppeIdList    AS CHAR NO-UNDO.

  DO WITH FRAME default-frame:
      THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
      RUN JBoxLookup.w (THIS-PROCEDURE,50,
                        "Kundegruppe;GruppeId;Beskrivelse"
                        ,"WHERE true"
                        ,""
                        ,"GruppeId,Beskrivelse"
                        ,OUTPUT cGruppeIdList
                        ,OUTPUT bOK).
      THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

      IF bOk AND cGruppeIdList NE "" THEN DO:
        ASSIGN FI-GruppeId:SCREEN-VALUE = ENTRY(1,cGruppeIdList,"|").
        APPLY "any-printable" TO FI-GruppeId.
      END.
      APPLY "entry" TO FI-GruppeId.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnKundeDetSok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnKundeDetSok C-Win
ON CHOOSE OF btnKundeDetSok IN FRAME DEFAULT-FRAME /* ... */
DO:
  DEF VAR cKundeValueList    AS CHAR NO-UNDO.

  DO WITH FRAME default-frame:
      THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
      RUN JBoxLookup.w (THIS-PROCEDURE,50,
                        "Kunde;Kundenr;Navn"
                        ,"WHERE true"
                        ,""
                        ,"KundeNr,Navn"
                        ,OUTPUT cKundeValueList
                        ,OUTPUT bOK).
      THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

      IF bOk AND cKundeValueList NE "" THEN DO:
        FIND Kunde NO-LOCK WHERE
            Kunde.KundeNr = DEC(ENTRY(1,cKundeValueList,"|")) NO-ERROR.
        FIND Butiker NO-LOCK WHERE
          Butiker.Butik = Kunde.ButikkNr NO-ERROR.
        ASSIGN 
            KobletTilKunde:SCREEN-VALUE = ENTRY(1,cKundeValueList,"|")
            KundeNavn:SCREEN-VALUE = Kunde.Navn
            FI-GruppeId:SCREEN-VALUE = string(Kunde.GruppeId)
            FI-GruppeId:SENSITIVE = FALSE
            btnGruppeId:SENSITIVE = FALSE
            FI-CL:SCREEN-VALUE       = IF AVAILABLE Butiker
                                         THEN STRING(Kunde.ButikkNr)
                                         ELSE STRING(clButiker.butik)
            FI-ButNamn:SCREEN-VALUE  = IF AVAILABLE Butiker
                                         THEN Butiker.ButNamn
                                         ELSE clButiker.butNamn
            FI-CL:SENSITIVE       = FALSE
            btnButiker:SENSITIVE  = FALSE
            FI-ButNamn:SENSITIVE  = FALSE
            .
        APPLY "any-printable" TO KobletTilKunde.
      END.
      ELSE
          ASSIGN
              FI-GruppeId:SCREEN-VALUE = "0"
              FI-GruppeId:SENSITIVE    = TRUE
              btnGruppeId:SENSITIVE    = TRUE
              FI-CL:SCREEN-VALUE       = STRING(clButiker.butik)
              FI-ButNamn:SCREEN-VALUE  = clButiker.butNamn
              FI-CL:SENSITIVE          = TRUE
              btnButiker:SENSITIVE     = TRUE
              FI-ButNamn:SENSITIVE     = TRUE
              .
      APPLY "entry" TO KobletTilKunde.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnOK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnOK C-Win
ON CHOOSE OF BtnOK IN FRAME DEFAULT-FRAME /* Start generering */
DO:
  /* Kontrollerer om det ligger kort innenfor den valgte nummerserie fra før. */
  IF CAN-FIND(FIRST KundeKort WHERE
              dec(KundeKort.KortNr) >= DEC(FraKortNr:SCREEN-VALUE) AND
              dec(KundeKort.KortNr) <= DEC(TilKortNr:SCREEN-VALUE)) THEN
  DO:
      MESSAGE "Det finnes kundekort innenfor det valgte intervall." SKIP
              "Serie " STRING(FraKortNr:SCREEN-VALUE) " til " STRING(TilKortNr:SCREEN-VALUE) "."
              "Velg et annet intervall."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN NO-APPLY.
  END.
    
  IF INPUT KobletTilKunde > 0 THEN
  DO:
      IF NOT CAN-FIND(Kunde WHERE
                      Kunde.KundeNr = INPUT KobletTilKunde) THEN
      DO:
          MESSAGE "Ugyldig kundenummer"
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
          RETURN NO-APPLY.
      END.
  END.

  IF INPUT FI-GruppeId > 0 THEN
  DO:
      IF NOT CAN-FIND(Kundegruppe WHERE
                      KundeGruppe.GruppeId = INPUT FI-GruppeId) THEN
      DO:
          MESSAGE "Ugyldig kundegruppe."
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
          RETURN NO-APPLY.
      END.

  END.
  ELSE DO:
      MESSAGE "Kundegruppe er ikke angitt."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN NO-APPLY.
  END.

  IF INPUT FI-Cl = 0 THEN
  DO:
      MESSAGE "Butiknummer må angis."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN NO-APPLY.
  END.

  IF NOT can-find(Butiker where
                  Butiker.Butik = INPUT FI-CL) THEN
  DO:
      MESSAGE "Ugyldig butikknummer angitt."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN NO-APPLY.
  END.
  
  IF INPUT FraKortNr > INPUT TilKortNr THEN
  DO:
      MESSAGE "Fra til på kortnummer er feilaktig angitt."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN NO-APPLY.
  END.

  IF INPUT FraKortNr = 0 or INPUT TilKortNr = 0 THEN
  DO:
      MESSAGE "Både fra og til kortnummer må angis."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN NO-APPLY.
  END.

  ASSIGN
      bOk = FALSE
      .
  MESSAGE "Skal generering av kunde-/medlemskort starte?"
      VIEW-AS ALERT-BOX INFO BUTTONS YES-NO UPDATE bOk.

  IF bOk = FALSE THEN
      RETURN NO-APPLY.

  RUN GenererKundeKort.
  RUN visSisteBrukteKortNr.

  APPLY "choose" TO BtnDone.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-CL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-CL C-Win
ON LEAVE OF FI-CL IN FRAME DEFAULT-FRAME /* Butikk */
DO:
  FIND Butiker NO-LOCK WHERE
      Butiker.Butik = INPUT FI-CL NO-ERROR.
  IF AVAILABLE Butiker THEN
  DO:
      FI-ButNamn:SCREEN-VALUE = Butiker.ButNamn.
  END.
  ELSE DO:
      FI-ButNamn:SCREEN-VALUE = "".
      MESSAGE "Ugyldig butikknummer."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FraKortNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FraKortNr C-Win
ON F10 OF FraKortNr IN FRAME DEFAULT-FRAME /* Fra/til kortnummer */
DO:
  APPLY "choose" TO btnKundeDetSok.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME KobletTilKunde
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL KobletTilKunde C-Win
ON F10 OF KobletTilKunde IN FRAME DEFAULT-FRAME /* Kunde */
DO:
  APPLY "choose" TO btnKundeDetSok.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL KobletTilKunde C-Win
ON LEAVE OF KobletTilKunde IN FRAME DEFAULT-FRAME /* Kunde */
DO:
    IF INPUT KobletTilKunde > 0 THEN DO:
      ASSIGN 
          FI-GruppeId:SENSITIVE = FALSE
          btnGruppeId:SENSITIVE = FALSE
          FI-CL:SENSITIVE       = FALSE
          btnButiker:SENSITIVE  = FALSE
          FI-ButNamn:SENSITIVE  = FALSE
          .
    END.
    ELSE DO:
        ASSIGN
            FI-GruppeId:SENSITIVE = TRUE
            btnGruppeId:SENSITIVE = TRUE
            FI-CL:SENSITIVE       = TRUE
            btnButiker:SENSITIVE  = TRUE
            FI-ButNamn:SENSITIVE  = TRUE
            .
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL KobletTilKunde C-Win
ON TAB OF KobletTilKunde IN FRAME DEFAULT-FRAME /* Kunde */
OR RETURN OF KobletTilKunde DO:
  IF INPUT KobletTilKunde > 0 THEN
  DO:
      FIND Kunde NO-LOCK WHERE
          Kunde.KundeNr = INPUT KobletTilKunde NO-ERROR.
      IF AVAILABLE Kunde THEN DO:
          FIND Butiker NO-LOCK WHERE
          Butiker.Butik = Kunde.ButikkNr NO-ERROR.
          ASSIGN
              KundeNavn:SCREEN-VALUE   = Kunde.Navn
              FI-GruppeId:SCREEN-VALUE = STRING(Kunde.GruppeId)
              FI-CL:SCREEN-VALUE       = IF AVAILABLE Butiker
                                           THEN STRING(Kunde.ButikkNr)
                                           ELSE STRING(clButiker.butik)
              FI-ButNamn:SCREEN-VALUE  = IF AVAILABLE Butiker
                                           THEN Butiker.ButNamn
                                           ELSE clButiker.butNamn
              .
      END.
      ELSE DO:
          ASSIGN
              KundeNavn:SCREEN-VALUE   = ""
              FI-GruppeId:SCREEN-VALUE = "0"
              FI-CL:SCREEN-VALUE       = STRING(clButiker.butik)
              FI-ButNamn:SCREEN-VALUE  = clButiker.butNamn
              .
          MESSAGE "Ugyldig kundenummer."
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
          RETURN NO-APPLY.
      END.
  END.
  ELSE DO:
      ASSIGN
          KundeNavn:SCREEN-VALUE   = ""
          FI-GruppeId:SCREEN-VALUE = "0"
          FI-CL:SCREEN-VALUE       = STRING(clButiker.butik)
          FI-ButNamn:SCREEN-VALUE  = clButiker.butNamn
          .
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TilKortNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TilKortNr C-Win
ON F10 OF TilKortNr IN FRAME DEFAULT-FRAME
DO:
  APPLY "choose" TO btnKundeDetSok.
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
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:


  RUN enable_UI.

  FIND FIRST clButiker NO-LOCK WHERE
      clButiker.Butik > 0 NO-ERROR.
  IF AVAILABLE clButiker THEN
  DO:
      ASSIGN
          FI-CL = clButiker.Butik
          FI-ButNamn = clButiker.ButNamn
          .
      DISPLAY
          FI-CL
          FI-ButNamn
          WITH FRAME Default-Frame.
  END.
  FIND FIRST KundeGruppe NO-LOCK WHERE
      KundeGruppe.GruppeId > 0 NO-ERROR.
  IF AVAILABLE KundeGruppe THEN
      FI-GruppeId:SCREEN-VALUE = STRING(KundeGruppe.GruppeId).

  RUN visSisteBrukteKortNr.

  APPLY "entry" TO KobletTilKunde.

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

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
  DISPLAY KobletTilKunde KundeNavn FI-CL FI-ButNamn FraKortNr TilKortNr 
          FI-GruppeId FI-gyldighet FI-SistBrukt AntallKort FI-Tekst1 FI-Tekst-2 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-2 RECT-3 KobletTilKunde btnKundeDetSok FI-CL btnButiker FraKortNr 
         TilKortNr FI-GruppeId btnGruppeId FI-gyldighet FI-SistBrukt BtnOK 
         BtnDone 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GenererKundeKort C-Win 
PROCEDURE GenererKundeKort :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF VAR plLoop      AS DEC  NO-UNDO.
DEF VAR plKundeNr   AS DEC  NO-UNDO.
DEF VAR plMedlemsNr AS DEC NO-UNDO.
DEF VAR bOk         AS LOG  NO-UNDO.
DEF VAR cMsgs       AS CHAR NO-UNDO.
DEF VAR piAntKort   AS INT  NO-UNDO.


  DO WITH FRAME Default-frame:
      /* Generering av kort for en kunde. */
      IF DEC(KobletTilKunde:SCREEN-VALUE) > 0 THEN
      DO:
        ASSIGN /* Alle kortene skal på denne kunden. */
            plKundeNr = DEC(KobletTilKunde:SCREEN-VALUE)
            .
        DO plLoop = DEC(FraKortNr:SCREEN-VALUE) TO DEC(TilKortNr:SCREEN-VALUE):
            /* Bare medlem skal opprettes. */
            RUN genkundeMedlem.p (INT(FI-CL:SCREEN-VALUE),
                                  INT(FI-GruppeId:SCREEN-VALUE),
                                  INPUT-OUTPUT plKundeNr,
                                  OUTPUT plMedlemsNr,
                                  OUTPUT bOk,
                                  OUTPUT cMsgs).
            /* Kundekortene legges opp på samme kunde, men medlemskortene */
            /* legges på separate medlemmer.                              */
            RUN genkundekort_og_medlem.p (INT(FI-CL:SCREEN-VALUE),
                                          DEC(KobletTilKunde:SCREEN-VALUE),
                                          plMedlemsNr,
                                          plLoop,
                                          plLoop,
                                          INT(FI-Gyldighet:SCREEN-VALUE),
                                          OUTPUT bOk,
                                          OUTPUT cMsgs).
            IF bOk THEN piAntKort = piAntKort + 1.

        END.
      END.

      /* Generering av kunder med et kort pr. kunde - EN til EN mellom kunde og medlem. */
      ELSE DO:
          DO plLoop = DEC(FraKortNr:SCREEN-VALUE) TO DEC(TilKortNr:SCREEN-VALUE):
              ASSIGN /* Kunde skal opprettes */
                  plKundeNr = 0
                  .
              /* Kunde og medlem skal opprettes. */
              RUN genkundeMedlem.p (INT(FI-CL:SCREEN-VALUE),
                                    INT(FI-GruppeId:SCREEN-VALUE),
                                    INPUT-OUTPUT plKundeNr,
                                    OUTPUT plMedlemsNr,
                                    OUTPUT bOk,
                                    OUTPUT cMsgs).
              
              /* Tar bort automatisk genererte kundekort som er lagt opp fra trigger. */
              /* De kortene skal ikke legges opp på kunder som genereres.             */
              SLETTKORT:
              FOR EACH KundeKort EXCLUSIVE-LOCK WHERE
                  KundeKort.KundeNr = plKundeNr:
                  DELETE KundeKort.
              END. /* SLETTKORT */

              RUN genkundekort_og_medlem.p (INT(FI-CL:SCREEN-VALUE),
                                            plKundeNr,
                                            plMedlemsNr,
                                            plLoop,
                                            plLoop,
                                            INT(FI-Gyldighet:SCREEN-VALUE),
                                            OUTPUT bOk,
                                            OUTPUT cMsgs).
              IF bOk THEN piAntKort = piAntKort + 1.
          END.
      END.
  END.

  ASSIGN
      AntallKort:SCREEN-VALUE = STRING(piAntKort)
      .

  MESSAGE "Det er generert " piAntKort "kunde-/medlemskort."
      VIEW-AS ALERT-BOX INFO BUTTONS OK TITLE "Medlemskortgenerering".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setKunde C-Win 
PROCEDURE setKunde :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAMETER piKundeNr AS DEC NO-UNDO.

FIND Kunde NO-LOCK WHERE
    Kunde.KundeNr = piKundeNr NO-ERROR.
IF AVAILABLE Kunde THEN
DO WITH FRAME Default-Frame:
    IF AVAILABLE Butiker THEN
        RELEASE Butiker.
    IF Kunde.ButikkNr > 0 THEN
        FIND Butiker NO-LOCK WHERE
        Butiker.Butik = Kunde.butikkNr NO-ERROR.

    ASSIGN
        iKundeNr = piKundeNr
        KobletTilKunde:SENSITIVE    = FALSE
        btnKundeDetSok:SENSITIVE    = FALSE
        FI-CL                       = IF AVAILABLE Butiker 
                                        THEN Butiker.Butik
                                        ELSE clButiker.Butik
        FI-ButNamn                  = IF AVAILABLE Butiker
                                        THEN Butiker.ButNamn
                                        ELSE clButiker.ButNamn
        FI-CL:SCREEN-VALUE          = STRING(FI-CL)
        FI-CL:SENSITIVE             = FALSE
        FI-ButNamn:SCREEN-VALUE     = FI-ButNamn
        btnButiker:SENSITIVE        = FALSE
        FI-GruppeId:SENSITIVE       = FALSE
        btnGruppeId:SENSITIVE       = FALSE
        KobletTilKunde:SCREEN-VALUE = STRING(iKundeNr)
        KundeNavn:SCREEN-VALUE      = Kunde.Navn
        FI-GruppeId:SCREEN-VALUE    = string(Kunde.GruppeId)
        .
    APPLY "ENTRY" TO FraKortNr. 
END.
ELSE DO WITH FRAME Default-Frame:
    FIND FIRST KundeGruppe NO-LOCK WHERE
        KundeGruppe.GruppeId > 0 NO-ERROR.
    IF AVAILABLE KundeGruppe THEN
        FI-GruppeId:SCREEN-VALUE = STRING(KundeGruppe.GruppeId).
END.

IF iKundeNr = 0 OR NOT CAN-FIND(Kunde WHERE
                                Kunde.KundeNr = iKundeNr) THEN
DO WITH FRAME Default-frame:
    MESSAGE "Ugyldig  eller ukjent kundenummer."
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    APPLY "CHOOSE" TO BtnDone.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE visSisteBrukteKortNr C-Win 
PROCEDURE visSisteBrukteKortNr :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cSisteBrukte AS CHAR NO-UNDO.
                                   
  DO WITH FRAME Default-Frame:
    SISTE:
    FOR EACH KundeKort NO-LOCK 
        BY INT(KundeKort.KortNr) DESCENDING:
      cSisteBrukte = KundeKort.KortNr.
      LEAVE SISTE.
    END. /* SISTE */
    {setsyspara.i 14 2 5 cSisteBrukte}
    ASSIGN
      FI-SistBrukt:SCREEN-VALUE = cSisteBrukte
      FraKortNr:SCREEN-VALUE    = STRING(INT(cSisteBrukte) + 1)
      TilKortNr:SCREEN-VALUE    = STRING(INT(cSisteBrukte) + 1)
      .
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

