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

/* Local Variable Definitions ---                                       */
def var wTotAntall as dec format "zzz,zzz,zz9" no-undo.
def var wWork            as dec  no-undo.
def var wWork2           as dec  no-undo.
def var wWork3           as dec  no-undo.
def var wStop            as log  initial false no-undo.
def var wDato            as date no-undo.
def var wTid             as int  no-undo.
def var wSkjerm          as char no-undo.
def var wTilbud          as log  no-undo.
def var wOk              as int  no-undo.
def var wArtBasRecid     as recid no-undo.
DEF VAR wArtikkelNr      AS DEC  NO-UNDO.
def var wValgtSkriver    as int  no-undo.

def buffer bArtBas for ArtBas.

{etikettlogg.i &New = "New"}

{runlib.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ArtBas

/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define QUERY-STRING-DEFAULT-FRAME FOR EACH ArtBas SHARE-LOCK
&Scoped-define OPEN-QUERY-DEFAULT-FRAME OPEN QUERY DEFAULT-FRAME FOR EACH ArtBas SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-DEFAULT-FRAME ArtBas
&Scoped-define FIRST-TABLE-IN-QUERY-DEFAULT-FRAME ArtBas


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS B-SokArt1 RECT-48 RECT-49 RECT-50 ~
FILL-IN_Vg1 FILL-IN_LopNr1 FILL-IN_Vg2 FILL-IN_LopNr2 CB-Skriver B-Start ~
Btn_Done TG-Varubeskr B-SokArt2 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN_Vg1 FILL-IN_LopNr1 FILL-IN_Beskr1 ~
FILL-IN_LevKod1 FILL-IN_Vg2 FILL-IN_LopNr2 FILL-IN_Beskr2 FILL-IN_LevKod2 ~
FI-Transaksjon FI-Oppdatert FI-StartInfo FI-SluttInfo FI-TidBrukt ~
CB-Skriver TG-Varubeskr FI-Tittel 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-SokArt1  NO-FOCUS
     LABEL "..." 
     SIZE 4.6 BY 1.

DEFINE BUTTON B-SokArt2  NO-FOCUS
     LABEL "..." 
     SIZE 4.6 BY 1.

DEFINE BUTTON B-Start 
     LABEL "&Start overføring" 
     SIZE 25.4 BY 1.14.

DEFINE BUTTON Btn_Done DEFAULT 
     LABEL "&Avslutt" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE CB-Skriver AS CHARACTER FORMAT "X(256)":U 
     LABEL "Etikettskriver/kø" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 59 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Oppdatert AS CHARACTER FORMAT "X(256)":U 
     LABEL "Ant. transaksj." 
     VIEW-AS FILL-IN 
     SIZE 59 BY 1 NO-UNDO.

DEFINE VARIABLE FI-SluttInfo AS CHARACTER FORMAT "X(256)":U 
     LABEL "Ferdig" 
     VIEW-AS FILL-IN 
     SIZE 59 BY 1 NO-UNDO.

DEFINE VARIABLE FI-StartInfo AS CHARACTER FORMAT "X(256)":U 
     LABEL "Startet" 
     VIEW-AS FILL-IN 
     SIZE 59 BY 1 NO-UNDO.

DEFINE VARIABLE FI-TidBrukt AS CHARACTER FORMAT "X(256)":U 
     LABEL "Tid brukt" 
     VIEW-AS FILL-IN 
     SIZE 59 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Tittel AS CHARACTER FORMAT "X(256)":U INITIAL "Overfør restpar til annen artikkel" 
      VIEW-AS TEXT 
     SIZE 77 BY 1.38
     FGCOLOR 1 FONT 8 NO-UNDO.

DEFINE VARIABLE FI-Transaksjon AS CHARACTER FORMAT "X(256)":U 
     LABEL "Status" 
     VIEW-AS FILL-IN 
     SIZE 59 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN_Beskr1 AS CHARACTER FORMAT "x(20)" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1.

DEFINE VARIABLE FILL-IN_Beskr2 AS CHARACTER FORMAT "x(20)" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1.

DEFINE VARIABLE FILL-IN_LevKod1 AS CHARACTER FORMAT "x(20)" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1.

DEFINE VARIABLE FILL-IN_LevKod2 AS CHARACTER FORMAT "x(20)" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1.

DEFINE VARIABLE FILL-IN_LopNr1 AS INTEGER FORMAT "zzz9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 7.6 BY 1.

DEFINE VARIABLE FILL-IN_LopNr2 AS INTEGER FORMAT "zzz9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 7.6 BY 1.

DEFINE VARIABLE FILL-IN_Vg1 AS INTEGER FORMAT "zzz9" INITIAL 0 
     LABEL "Fra artikkel" 
     VIEW-AS FILL-IN 
     SIZE 7.6 BY 1.

DEFINE VARIABLE FILL-IN_Vg2 AS INTEGER FORMAT "zzz9" INITIAL 0 
     LABEL "Til artikkel" 
     VIEW-AS FILL-IN 
     SIZE 7.6 BY 1.

DEFINE RECTANGLE RECT-48
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 83 BY 4.29.

DEFINE RECTANGLE RECT-49
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 83 BY 5.95.

DEFINE RECTANGLE RECT-50
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 83 BY 1.91.

DEFINE VARIABLE TG-Varubeskr AS LOGICAL INITIAL yes 
     LABEL "Varubeskrivning kopieras med" 
     VIEW-AS TOGGLE-BOX
     SIZE 34.4 BY .81 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY DEFAULT-FRAME FOR 
      ArtBas SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     B-SokArt1 AT ROW 3.62 COL 79.8 NO-TAB-STOP 
     FILL-IN_Vg1 AT ROW 3.62 COL 18.6 COLON-ALIGNED HELP
          "'varegruppenummer"
     FILL-IN_LopNr1 AT ROW 3.62 COL 26.2 COLON-ALIGNED HELP
          "Løpenummer innenfor varegruppen" NO-LABEL
     FILL-IN_Beskr1 AT ROW 3.62 COL 33.8 COLON-ALIGNED HELP
          "Kort beskrivelse av artikkelen" NO-LABEL
     FILL-IN_LevKod1 AT ROW 3.62 COL 55.8 COLON-ALIGNED HELP
          "Leverandørens artikkelnummer" NO-LABEL
     FILL-IN_Vg2 AT ROW 4.81 COL 18.6 COLON-ALIGNED HELP
          "'varegruppenummer"
     FILL-IN_LopNr2 AT ROW 4.81 COL 26.2 COLON-ALIGNED HELP
          "Løpenummer innenfor varegruppen" NO-LABEL
     FILL-IN_Beskr2 AT ROW 4.81 COL 33.8 COLON-ALIGNED HELP
          "Kort beskrivelse av artikkelen" NO-LABEL
     FILL-IN_LevKod2 AT ROW 4.81 COL 55.8 COLON-ALIGNED HELP
          "Leverandørens artikkelnummer" NO-LABEL
     FI-Transaksjon AT ROW 6.29 COL 18.6 COLON-ALIGNED
     FI-Oppdatert AT ROW 7.48 COL 18.6 COLON-ALIGNED
     FI-StartInfo AT ROW 9.81 COL 18.6 COLON-ALIGNED
     FI-SluttInfo AT ROW 11 COL 18.6 COLON-ALIGNED
     FI-TidBrukt AT ROW 12.19 COL 18.6 COLON-ALIGNED
     CB-Skriver AT ROW 14.33 COL 18.6 COLON-ALIGNED
     B-Start AT ROW 16 COL 39
     Btn_Done AT ROW 16 COL 66.2
     TG-Varubeskr AT ROW 16.19 COL 2.6
     B-SokArt2 AT ROW 4.81 COL 79.8 NO-TAB-STOP 
     FI-Tittel AT ROW 1.48 COL 3 COLON-ALIGNED NO-LABEL
     RECT-48 AT ROW 9.33 COL 2
     RECT-49 AT ROW 3.14 COL 2
     RECT-50 AT ROW 13.86 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 84.2 BY 16.29.


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
         TITLE              = "Overføring av restpar"
         HEIGHT             = 16.29
         WIDTH              = 84.2
         MAX-HEIGHT         = 16.29
         MAX-WIDTH          = 84.2
         VIRTUAL-HEIGHT     = 16.29
         VIRTUAL-WIDTH      = 84.2
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
/* SETTINGS FOR FILL-IN FI-SluttInfo IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-StartInfo IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-TidBrukt IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Tittel IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Transaksjon IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN_Beskr1 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN_Beskr2 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN_LevKod1 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN_LevKod2 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME DEFAULT-FRAME
/* Query rebuild information for FRAME DEFAULT-FRAME
     _TblList          = "skotex.ArtBas"
     _Query            is NOT OPENED
*/  /* FRAME DEFAULT-FRAME */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Overføring av restpar */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Overføring av restpar */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SokArt1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SokArt1 C-Win
ON CHOOSE OF B-SokArt1 IN FRAME DEFAULT-FRAME /* ... */
DO:
  run d-hsok.w (output wArtikkelNr,"").
  IF wArtikkelNr = ? THEN
      RETURN NO-APPLY.
  FIND ArtBas NO-LOCK WHERE
      ArtBas.ArtikkelNr = wArtikkelNr NO-ERROR.
  IF NOT AVAILABLE ArtBas THEN
      RETURN NO-APPLY.

  if available ArtBas then
  display 
    ArtBas.Vg     @ FILL-IN_Vg1
    ArtBas.LopNr  @ FILL-IN_LopNr1
    ArtBas.Beskr  @ FILL-IN_Beskr1
    ArtBas.LevKod @ FILL-IN_LevKod1
  with frame Default-FRAME.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SokArt2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SokArt2 C-Win
ON CHOOSE OF B-SokArt2 IN FRAME DEFAULT-FRAME /* ... */
DO:
  run d-hsok.w (output wArtikkelNr,"").
  IF wArtikkelNr = ? THEN
      RETURN NO-APPLY.
  FIND ArtBas NO-LOCK WHERE
      ArtBas.ArtikkelNr = wArtikkelNr NO-ERROR.
  IF NOT AVAILABLE ArtBas THEN
      RETURN NO-APPLY.

  if available ArtBas then
  display 
      ArtBas.Vg     @ FILL-IN_Vg2
      ArtBas.LopNr  @ FILL-IN_LopNr2
      ArtBas.Beskr  @ FILL-IN_Beskr2
      ArtBas.LevKod @ FILL-IN_LevKod2
  with frame Default-FRAME.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Start
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Start C-Win
ON CHOOSE OF B-Start IN FRAME DEFAULT-FRAME /* Start overføring */
DO:
  def var wTekst as char no-undo.
  
  display 
    "" @ FI-Oppdatert 
    "" @ FI-SluttInfo 
    "" @ FI-StartInfo 
    "" @ FI-TidBrukt 
    "" @ FI-Transaksjon
  with frame DEFAULT-FRAME.
  
  find ArtBas no-lock where
    ArtBas.Vg = input FILL-IN_Vg1 and
    ArtBAs.LopNr = input FILL-IN_LopNr1 no-error.
  if not available ArtBas then
    do:
      message "Okänd från artikel"
        view-as alert-box message title "Inputkontroll".
      return no-apply.
    end.
  ELSE if available ArtBas AND artbas.utgatt = TRUE then
        do:
          message "Utgått från artikel!"
            view-as alert-box message title "Inputkontroll".
          return no-apply.
        end.
  find bArtBas no-lock where
    bArtBas.Vg    = input FILL-IN_Vg2 and
    bArtBAs.LopNr = input FILL-IN_LopNr2 no-error.
  if not available bArtBas then
    do:
      message "Okänd TIL artikkel"
        view-as alert-box message title "Inputkontroll".
      return no-apply.
    end.
  ELSE if available ArtBas AND ArtBas.Utgatt = TRUE then
      do:
        message "Utgått till artikel!"
          view-as alert-box message title "Inputkontroll".
        return no-apply.
      end.    /* Sjekker om artikkelen har ikke fulleverte bestilölinger */
    IF CAN-FIND(FIRST BestHode OF ArtBas WHERE BestHode.BestStat < 6 AND
                                               BestHode.TotAntPar > 0) THEN DO:
        MESSAGE "Det finnes ikke fulleverte bestillinger på varen." SKIP
              "Överföring inte tillåten"
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN NO-APPLY.
    END.
  /* Sjekker om artikkelen ingår i pakke */
  IF CAN-FIND(FIRST PakkeLinje WHERE PakkeLinje.PkArtikkelNr = ArtBas.ArtikkelNr) THEN DO:
      MESSAGE "FRA artikkelen ingår i pakke. Kann ikke behandles!"
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN NO-APPLY.
  END.
  /* Sjekker om artikkelen ingår i erstattningsvare */
  IF CAN-FIND(FIRST Erstattningsvare WHERE Erstattningsvare.ArtikkelNr = ArtBas.ArtikkelNr) THEN DO:
      MESSAGE "FRA artikkelen er registrert som erstattningsvare. Kann ikke behandles!"
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN NO-APPLY.
  END.
  /* Sjekker om artikkelen har vært nullstilt før */
  IF ArtBas.Slasket = TRUE THEN DO:
          MESSAGE "FRA artikkelen er nullstilt fra før!"
            VIEW-AS ALERT-BOX MESSAGE TITLE "Inputkontroll".
          RETURN NO-APPLY.
  END.
  IF bArtBas.Slasket = TRUE THEN DO:
      MESSAGE "TIL artikkelen er nullstilt fra før!"
        VIEW-AS ALERT-BOX MESSAGE TITLE "Inputkontroll".
      RETURN NO-APPLY.
END.

  RUN StartInfo (today,time).
  
  assign
    B-Start:label        = "&Avbryt"
    B-Start:Private-Data = "AVBRYT".
  
  run x-overforrest.w (this-procedure:handle, recid(ArtBas), recid(bArtBas)).
  IF TG-Varubeskr:CHECKED AND TRIM(ArtBas.VareFakta) <> "" THEN DO:
      FIND CURRENT bArtBas EXCLUSIVE NO-ERROR NO-WAIT.
      IF AVAIL bArtBas THEN DO:
          bArtBas.VareFakta = bArtBas.VareFakta + (IF TRIM(bArtBas.VareFakta) <> "" THEN CHR(10) ELSE "") + TRIM(ArtBas.VareFakta) NO-ERROR.
          FIND CURRENT bArtBas NO-LOCK.
      END.
  END.
  find first EtikettLogg no-error.
  if available EtikettLogg then
    do:
      message "Skal etiketter skrives ut?" 
        view-as alert-box question buttons YES-NO title "Bekreftelse"
        update wSvar as log.
      if wSvar then
        RUN SendFil.
    end.

  assign
    B-Start:label        = "&Start overføring"
    B-Start:Private-Data = "OPPDAT".
    
  apply "entry":U to FILL-IN_Vg1.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Done
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Done C-Win
ON CHOOSE OF Btn_Done IN FRAME DEFAULT-FRAME /* Avslutt */
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


&Scoped-define SELF-NAME FILL-IN_LopNr1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_LopNr1 C-Win
ON TAB OF FILL-IN_LopNr1 IN FRAME DEFAULT-FRAME
DO:
  find ArtBas no-lock where
    ArtBas.Vg = input FILL-IN_Vg1 and
    ArtBAs.LopNr = input FILL-IN_LopNr1 no-error.
  if not available ArtBas then
    do:
      message "Okänd artikel!"
        view-as alert-box message title "Inputkontroll".
      return no-apply.
    end.
  ELSE if available ArtBas AND artbas.utgatt = TRUE then
      do:
        message "Utgått från artikel!"
          view-as alert-box message title "Inputkontroll".
        return no-apply.
      end.
  else
    display
      ArtBas.Beskr @ FILL-IN_Beskr1
      ArtBas.LevKod @ FILL-IN_LevKod1
    with frame DEFAULT-FRAME.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_LopNr2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_LopNr2 C-Win
ON TAB OF FILL-IN_LopNr2 IN FRAME DEFAULT-FRAME
DO:
  find ArtBas no-lock where
    ArtBas.Vg = input FILL-IN_Vg2 and
    ArtBAs.LopNr = input FILL-IN_LopNr2 no-error.
  if not available ArtBas then
    do:
      message "Ukjent artikkel!"
        view-as alert-box message title "Inputkontroll".
      return no-apply.
    end.
  ELSE if available ArtBas AND ArtBas.Utgatt = TRUE then
      do:
        message "Utgått till artikel!"
          view-as alert-box message title "Inputkontroll".
        return no-apply.
      end.
  else
    display
      ArtBas.Beskr @ FILL-IN_Beskr2
      ArtBas.LevKod @ FILL-IN_LevKod2
    with frame DEFAULT-FRAME.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_Vg1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_Vg1 C-Win
ON TAB OF FILL-IN_Vg1 IN FRAME DEFAULT-FRAME /* Fra artikkel */
or "RETURN":U of FILL-IN_Vg1
DO:
  find VarGr no-lock where
    VarGr.Vg = input FILL-IN_Vg1 no-error.
  if not available VarGr then
    do:
      message "Ukjent varegruppe!"
        view-as alert-box message title "Inputkontroll".
      return no-apply.
    end.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_Vg2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_Vg2 C-Win
ON TAB OF FILL-IN_Vg2 IN FRAME DEFAULT-FRAME /* Til artikkel */
or "RETURN":U of FILL-IN_Vg2
DO:
  find VarGr no-lock where
    VarGr.Vg = input FILL-IN_Vg2 no-error.
  if not available VarGr then
    do:
      message "Ukjent varegruppe!"
        view-as alert-box message title "Inputkontroll".
      return no-apply.
    end.
  
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

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN InitSkriverValg (1).
  RUN enable_UI.
  {lng.i}

  status input "".

  ASSIGN
      C-Win:HIDDEN = FALSE.

  apply "entry":U to FILL-IN_Vg1 in frame DEFAULT-FRAME.
  
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
do with frame DEFAULT-FRAME:
  
  if wTotAntall = 0 then
    do:
      message "Ingen transaksjoner igjen å oppdatere"
        view-as alert-box MESSAGE title "Melding".
      apply "close":U to this-procedure.
    end.
    
  assign
    B-Start:label        = "&Start oppdatering"
    B-Start:Private-Data = "OPPDAT"
    FI-Transaksjon       = ""
    FI-Oppdatert         = ""
    FI-SluttInfo         = ""
    FI-StartInfo         = ""
    FI-TidBrukt          = "".

  display
    FI-Transaksjon
    FI-Oppdatert  
    FI-SluttInfo  
    FI-StartInfo  
    FI-TidBrukt   
  with frame DEFAULT-FRAME.

end. /* FRAME */

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
  def input parameter wDato      as date no-undo.
  def input parameter wFerdigTid as int  no-undo.
  def input parameter wBruktTid  as int  no-undo.
  
  do with frame DEFAULT-FRAME:
    assign
      FI-SluttInfo:screen-value = string(wDato) + " " +
                                  string(wFerdigTid,"HH:MM:SS")
      FI-TidBrukt:screen-value  = string(wBruktTid,"HH:MM:SS").
  end.

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
  DISPLAY FILL-IN_Vg1 FILL-IN_LopNr1 FILL-IN_Beskr1 FILL-IN_LevKod1 FILL-IN_Vg2 
          FILL-IN_LopNr2 FILL-IN_Beskr2 FILL-IN_LevKod2 FI-Transaksjon 
          FI-Oppdatert FI-StartInfo FI-SluttInfo FI-TidBrukt CB-Skriver 
          TG-Varubeskr FI-Tittel 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE B-SokArt1 RECT-48 RECT-49 RECT-50 FILL-IN_Vg1 FILL-IN_LopNr1 
         FILL-IN_Vg2 FILL-IN_LopNr2 CB-Skriver B-Start Btn_Done TG-Varubeskr 
         B-SokArt2 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitSkriverValg C-Win 
PROCEDURE InitSkriverValg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def input parameter wSkriver as int no-undo.

/* Skriverliste. */
do with frame DEFAULT-FRAME:
  assign
    CB-Skriver            = " "
    CB-Skriver:List-Items = " ".
  For each SysPara no-lock where
    SysPara.SysHId  = 5 and
    SysPara.SysGr   = 21:
    assign
      CB-Skriver:list-items = CB-Skriver:list-items + 
                              (if CB-Skriver:list-items = ""
                                 then ""
                                 else ",") +
                              string(SysPara.ParaNr,"99") + ":"+ 
                              SysPara.Beskrivelse.
  end.
  assign
    CB-Skriver = entry(wSkriver,CB-Skriver:list-items).                       
end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SendFil C-Win 
PROCEDURE SendFil :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def var wEtikett_Fil as char no-undo.

  assign
    wValgtSkriver = int(entry(1,CB-Skriver:screen-value in frame DEFAULT-FRAME,":")).
  
  {syspara.i 5 20 wValgtSkriver wEtikett_Fil}

  run x-etikettsend.w (input wValgtSkriver).      

  if search(wEtikett_Fil) <> ? then
    do:
      message "Etikettfil ikke sendt. Skal den slettes?" 
        view-as alert-box question BUTTONS YES-NO title "Bekreft" 
        update wSvar as log.
      if wSvar then /* Slette fil. */
        os-delete value(wEtikett_Fil).
    end.

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
  def input parameter wDato     as date no-undo.
  def input parameter wStartTid as int no-undo.
  
  do with frame DEFAULT-FRAME:
    assign
      FI-StartInfo:screen-value = string(wDato) + " " +
                                  string(wStartTid,"HH:MM:SS").
  end.

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
  def input parameter wTransaksjon as char no-undo.
  def input parameter wOppdatert   as char no-undo.

  do with frame DEFAULT-FRAME:
    assign
      FI-Transaksjon:screen-value = wTransaksjon
      FI-Oppdatert:screen-value   = wOppdatert.
  end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

