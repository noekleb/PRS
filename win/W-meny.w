&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------
  Program:     w-meny.w 
  Forfatter:   Sturla Johnsen
  Beskrivelse: Ajourhold av menyer (tabell Meny)
  Parametere:  Ingen
  Endringer:   T.N 22/6-98 - Lagt inn parameter for lokal internprocedure
                           - --"-- for internprosedyre i bibliotek. 
                                   
------------------------------------------------------------------------*/

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN  
&ELSE
&ENDIF

DEF VAR wListitems   AS CHAR NO-UNDO.
DEF VAR wMdata       AS CHAR NO-UNDO.
DEF VAR i            AS INTE NO-UNDO INIT 1.
DEF VAR j            AS INTE NO-UNDO.
DEF VAR wNesteLevel  AS INTE NO-UNDO.
DEF VAR wNesteEntry  AS CHAR NO-UNDO.
DEF VAR wNesteLabel  AS CHAR NO-UNDO.
DEF VAR wEntry       AS CHAR NO-UNDO.
DEF VAR wSaveEntry   AS CHAR NO-UNDO.
DEF VAR wStrLabel    AS CHAR NO-UNDO.
DEF VAR wFlagg       AS CHAR NO-UNDO.
DEF VAR wHurtigtast  AS CHAR NO-UNDO.
DEF VAR wPrivdata    AS CHAR NO-UNDO.
DEF VAR wProgram     AS CHAR NO-UNDO.
DEF VAR wTekst       AS CHAR NO-UNDO.
DEF VAR wUnik        AS INTE NO-UNDO.
DEF VAR wD1          AS CHAR NO-UNDO INIT ";". /* Skiller hovedelementene */
DEF VAR wD2          AS CHAR NO-UNDO INIT "|". /* Skiller delelementene   */
DEF VAR wLevelCnt    AS CHAR NO-UNDO INIT "£". /* "Beskriver" nivået      */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS TOGGLE-RT-Param TOGGLE-Persistent ~
TOGGLE-LibIntProc TOGGLE-LokIntProc BUTTON-NyMeny BUTTON-Lukk RECT-1 
&Scoped-Define DISPLAYED-OBJECTS TOGGLE-RT-Param TOGGLE-Persistent ~
TOGGLE-LibIntProc TOGGLE-LokIntProc SELECT-Meny COMBO-BOX-Navn ~
FILL-IN-Tekst FILL-IN-Program FILL-IN-Hurtigtast FILL-IN-Parametere ~
TOGGLE-Deaktivert TOGGLE-LagVindu TOGGLE-Avkryssing TOGGLE-Avkrysset 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD Unik C-Win 
FUNCTION Unik RETURNS INTEGER
  ( INPUT wSV AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-Hoyre 
     LABEL "-&>" 
     SIZE 6 BY 1.1 TOOLTIP "Flytt ett nivå opp".

DEFINE BUTTON BUTTON-Lagre AUTO-GO DEFAULT 
     LABEL "&Lagre" 
     SIZE 13 BY 1.1
     BGCOLOR 8 .

DEFINE BUTTON BUTTON-LagreSom 
     LABEL "Lagre s&om..." 
     SIZE 16 BY 1.1.

DEFINE BUTTON BUTTON-Lukk AUTO-END-KEY 
     LABEL "Lukk" 
     SIZE 17 BY 1.1.

DEFINE BUTTON BUTTON-Ned 
     LABEL "N&ed" 
     SIZE 6 BY 1.1.

DEFINE BUTTON BUTTON-NyLinje 
     LABEL "&Linje" 
     SIZE 14 BY 1.1.

DEFINE BUTTON BUTTON-NyMeny 
     LABEL "&Ny..." 
     SIZE 14 BY 1.1.

DEFINE BUTTON BUTTON-NyProgram 
     LABEL "&Program" 
     SIZE 14 BY 1.1.

DEFINE BUTTON BUTTON-NySubmeny 
     LABEL "Sub&meny" 
     SIZE 14 BY 1.1.

DEFINE BUTTON BUTTON-Opp 
     LABEL "&Opp" 
     SIZE 6 BY 1.1.

DEFINE BUTTON BUTTON-Slett 
     LABEL "&Slett" 
     SIZE 14 BY 1.1.

DEFINE BUTTON BUTTON-SlettMeny 
     LABEL "S&lett..." 
     SIZE 12 BY 1.1.

DEFINE BUTTON BUTTON-SokProg 
     LABEL "..." 
     SIZE 4 BY 1.

DEFINE BUTTON BUTTON-SokTast 
     LABEL "..." 
     SIZE 4 BY 1.

DEFINE BUTTON BUTTON-Test 
     LABEL "&Test" 
     SIZE 40 BY 1.1.

DEFINE BUTTON BUTTON-Venstre 
     LABEL "&<-" 
     SIZE 6 BY 1.1 TOOLTIP "Flytt ett nivå ned".

DEFINE VARIABLE COMBO-BOX-Navn AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX SORT 
     LIST-ITEMS "?" 
     DROP-DOWN-LIST
     SIZE 55 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-Hurtigtast AS CHARACTER FORMAT "X(256)":U 
     LABEL "&Hurtigtast" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 39 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-Parametere AS CHARACTER FORMAT "X(256)":U 
     LABEL "P&arameter" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 44 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-Program AS CHARACTER FORMAT "X(256)":U 
     LABEL "Progra&m" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 39 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-Tekst AS CHARACTER FORMAT "X(256)":U 
     LABEL "&Tekst" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 44 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 50 BY 2.14.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 59 BY 5.95.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 59 BY 3.57.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 59 BY 5.19.

DEFINE VARIABLE SELECT-Meny AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     LIST-ITEMS "SUB  &Fil","     PRG  &Skriveroppsett...","     ------------------------------","     PRG  Avslutt","SUB  &Rediger","     PRG  &Ny...","     PRG  &Endre...","     PRG  &Slett","     ------------------------------","     SUB  &Vedlikehold","          PRG  &Oppdater saldo...","          PRG  &Tertialavslutning" 
     SIZE 50 BY 13.1 NO-UNDO.

DEFINE VARIABLE TOGGLE-Avkrysset AS LOGICAL INITIAL no 
     LABEL "Initielt a&vkrysset" 
     VIEW-AS TOGGLE-BOX
     SIZE 20 BY .81 NO-UNDO.

DEFINE VARIABLE TOGGLE-Avkryssing AS LOGICAL INITIAL no 
     LABEL "&Avkryssing" 
     VIEW-AS TOGGLE-BOX
     SIZE 15 BY .81 NO-UNDO.

DEFINE VARIABLE TOGGLE-Deaktivert AS LOGICAL INITIAL yes 
     LABEL "&Deaktivert" 
     VIEW-AS TOGGLE-BOX
     SIZE 15 BY .81 NO-UNDO.

DEFINE VARIABLE TOGGLE-LagVindu AS LOGICAL INITIAL no 
     LABEL "Lag vind&u (ikke for EXE-prog)" 
     VIEW-AS TOGGLE-BOX
     SIZE 32 BY .81 NO-UNDO.

DEFINE VARIABLE TOGGLE-LibIntProc AS LOGICAL INITIAL no 
     LABEL "Internprosedyre fra bibliotek" 
     VIEW-AS TOGGLE-BOX
     SIZE 32 BY .81 NO-UNDO.

DEFINE VARIABLE TOGGLE-LokIntProc AS LOGICAL INITIAL no 
     LABEL "Lokal internprosedyre" 
     VIEW-AS TOGGLE-BOX
     SIZE 31.8 BY .81 NO-UNDO.

DEFINE VARIABLE TOGGLE-Persistent AS LOGICAL INITIAL no 
     LABEL "Ikke persistent" 
     VIEW-AS TOGGLE-BOX
     SIZE 21.4 BY .81 NO-UNDO.

DEFINE VARIABLE TOGGLE-RT-Param AS LOGICAL INITIAL yes 
     LABEL "Run-Time Parameter" 
     VIEW-AS TOGGLE-BOX
     SIZE 31.8 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     TOGGLE-RT-Param AT ROW 15.57 COL 80.2
     TOGGLE-Persistent AT ROW 15.48 COL 57.8
     TOGGLE-LibIntProc AT ROW 13.38 COL 80
     TOGGLE-LokIntProc AT ROW 14.33 COL 80
     SELECT-Meny AT ROW 1.24 COL 2 NO-LABEL
     COMBO-BOX-Navn AT ROW 1.71 COL 54 COLON-ALIGNED NO-LABEL
     BUTTON-Lagre AT ROW 3.14 COL 56
     BUTTON-LagreSom AT ROW 3.14 COL 69
     BUTTON-NyMeny AT ROW 3.14 COL 85
     BUTTON-SlettMeny AT ROW 3.14 COL 99
     FILL-IN-Tekst AT ROW 6 COL 65 COLON-ALIGNED
     FILL-IN-Program AT ROW 7.19 COL 65 COLON-ALIGNED
     BUTTON-SokProg AT ROW 7.19 COL 107
     FILL-IN-Hurtigtast AT ROW 8.38 COL 65 COLON-ALIGNED
     BUTTON-SokTast AT ROW 8.38 COL 107
     FILL-IN-Parametere AT ROW 9.57 COL 65 COLON-ALIGNED
     TOGGLE-Deaktivert AT ROW 12.43 COL 58
     TOGGLE-LagVindu AT ROW 12.43 COL 80
     TOGGLE-Avkryssing AT ROW 13.38 COL 58
     BUTTON-NyProgram AT ROW 15.24 COL 3.8
     BUTTON-NySubmeny AT ROW 15.24 COL 19.8
     BUTTON-NyLinje AT ROW 15.24 COL 35.8
     TOGGLE-Avkrysset AT ROW 14.33 COL 58
     BUTTON-Slett AT ROW 17.19 COL 4
     BUTTON-Venstre AT ROW 17.19 COL 20
     BUTTON-Hoyre AT ROW 17.19 COL 28
     BUTTON-Opp AT ROW 17.19 COL 36
     BUTTON-Ned AT ROW 17.19 COL 44
     BUTTON-Test AT ROW 17.19 COL 54
     BUTTON-Lukk AT ROW 17.19 COL 96
     RECT-2 AT ROW 5.29 COL 54
     RECT-4 AT ROW 1.24 COL 54
     RECT-5 AT ROW 11.71 COL 54
     RECT-1 AT ROW 14.76 COL 1.8
     "Meny" VIEW-AS TEXT
          SIZE 6 BY .62 AT ROW 1 COL 55
     "Generelt" VIEW-AS TEXT
          SIZE 9 BY .62 AT ROW 5.05 COL 55
     "Valg" VIEW-AS TEXT
          SIZE 5 BY .62 AT ROW 11.48 COL 55
     "Ny" VIEW-AS TEXT
          SIZE 3 BY .62 AT ROW 14.52 COL 2.8
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 113 BY 17.48
         CANCEL-BUTTON BUTTON-Lukk.


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
         TITLE              = "Menyeditor"
         HEIGHT             = 17.48
         WIDTH              = 113
         MAX-HEIGHT         = 20.38
         MAX-WIDTH          = 141.2
         VIRTUAL-HEIGHT     = 20.38
         VIRTUAL-WIDTH      = 141.2
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



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   Custom                                                               */
ASSIGN 
       FRAME DEFAULT-FRAME:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON BUTTON-Hoyre IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BUTTON-Lagre IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BUTTON-LagreSom IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BUTTON-Ned IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BUTTON-NyLinje IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BUTTON-NyProgram IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BUTTON-NySubmeny IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BUTTON-Opp IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BUTTON-Slett IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BUTTON-SlettMeny IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BUTTON-SokProg IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BUTTON-SokTast IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BUTTON-Test IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BUTTON-Venstre IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX COMBO-BOX-Navn IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-Hurtigtast IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-Parametere IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-Program IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-Tekst IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-2 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-4 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-5 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR SELECTION-LIST SELECT-Meny IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TOGGLE-Avkrysset IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TOGGLE-Avkryssing IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TOGGLE-Deaktivert IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TOGGLE-LagVindu IN FRAME DEFAULT-FRAME
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
ON END-ERROR OF C-Win /* Menyeditor */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Menyeditor */
DO:
  RUN SjekkLagre. 
  IF CAN-DO("Feil,Avbryt",RETURN-VALUE) THEN RETURN NO-APPLY.        
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Hoyre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Hoyre C-Win
ON CHOOSE OF BUTTON-Hoyre IN FRAME DEFAULT-FRAME /* -> */
DO:
  DO WITH FRAME {&FRAME-NAME}:
     ASSIGN wEntry = "     " + SELECT-Meny:SCREEN-VALUE.
     IF SELECT-Meny:REPLACE(wEntry,SELECT-Meny:SCREEN-VALUE) THEN.
     ASSIGN SELECT-Meny:SCREEN-VALUE = wEntry.
     APPLY "VALUE-CHANGED" TO SELECT-Meny.
  END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Lagre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Lagre C-Win
ON CHOOSE OF BUTTON-Lagre IN FRAME DEFAULT-FRAME /* Lagre */
DO:
  RUN KontrollerMeny("Lagre").
  IF RETURN-VALUE = "Feil" THEN RETURN NO-APPLY.
  
  /* Lagrer menyen i wListitems */
  RUN LagMdata.
  
  /* Er endringer gjort? */
  IF Meny.Mdata <> wListitems THEN RUN LagreMeny.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-LagreSom
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-LagreSom C-Win
ON CHOOSE OF BUTTON-LagreSom IN FRAME DEFAULT-FRAME /* Lagre som... */
DO:
  DEF VAR wNyttNavn AS CHAR NO-UNDO.
  RUN KontrollerMeny("Lagre").
  IF RETURN-VALUE = "Feil" THEN RETURN NO-APPLY.
  RUN d-meny.w(Meny.Navn).
  IF RETURN-VALUE <> "" THEN 
  DO WITH FRAME {&FRAME-NAME}:
     ASSIGN wNyttNavn = RETURN-VALUE.
     RUN LagMdata.
     IF wNyttNavn <> Meny.Navn THEN 
     DO TRANSACTION:
        IF COMBO-BOX-Navn:ADD-LAST(wNyttNavn) THEN.
        ASSIGN COMBO-BOX-Navn:INNER-LINES  = COMBO-BOX-Navn:NUM-ITEMS
               COMBO-BOX-Navn:SCREEN-VALUE = wNyttNavn.
        CREATE Meny.
        ASSIGN Meny.Navn = wNyttNavn
               Meny.Mdata = wListitems.   
        RELEASE Meny.
        FIND Meny WHERE Meny.Navn = wNyttNavn NO-LOCK NO-ERROR.
     END.   
     ELSE   
     IF Meny.Mdata <> wListitems THEN RUN LagreMeny.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Lukk
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Lukk C-Win
ON CHOOSE OF BUTTON-Lukk IN FRAME DEFAULT-FRAME /* Lukk */
DO:
  APPLY "WINDOW-CLOSE" TO {&WINDOW-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Ned
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Ned C-Win
ON CHOOSE OF BUTTON-Ned IN FRAME DEFAULT-FRAME /* Ned */
DO:
   RUN FlyttOppNed("Ned"). 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-NyLinje
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-NyLinje C-Win
ON CHOOSE OF BUTTON-NyLinje IN FRAME DEFAULT-FRAME /* Linje */
DO:
   RUN NyttEntry("-----------------------------------").
   
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-NyMeny
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-NyMeny C-Win
ON CHOOSE OF BUTTON-NyMeny IN FRAME DEFAULT-FRAME /* Ny... */
DO:
  DEF VAR wNyMeny AS CHAR NO-UNDO.
  RUN d-meny.w("").
  ASSIGN wNyMeny = RETURN-VALUE.
  IF wNyMeny <> "" THEN 
  DO WITH FRAME {&FRAME-NAME}:
     RUN SjekkLagre.
     IF RETURN-VALUE = "Avbryt" THEN DO:
        IF AVAIL Meny THEN ASSIGN COMBO-BOX-Navn:SCREEN-VALUE = Meny.Navn.
        RETURN NO-APPLY.
     END.
     IF COMBO-BOX-Navn:ADD-LAST(wNyMeny) THEN.
     ASSIGN SELECT-Meny:LIST-ITEMS      = ""
            COMBO-BOX-Navn:SCREEN-VALUE = wNyMeny
            COMBO-BOX-Navn:INNER-LINES  = COMBO-BOX-Navn:NUM-ITEMS.
     FIND Meny WHERE Meny.Navn = wNyMeny NO-LOCK NO-ERROR.       
     RUN TomMeny.
     IF NOT BUTTON-SlettMeny:SENSITIVE THEN RUN MenyFinnes.
     APPLY "ENTRY" TO BUTTON-NySubMeny.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-NyProgram
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-NyProgram C-Win
ON CHOOSE OF BUTTON-NyProgram IN FRAME DEFAULT-FRAME /* Program */
DO:
  RUN NyttEntry("PRG").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-NySubmeny
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-NySubmeny C-Win
ON CHOOSE OF BUTTON-NySubmeny IN FRAME DEFAULT-FRAME /* Submeny */
DO:
  RUN NyttEntry("SUB").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Opp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Opp C-Win
ON CHOOSE OF BUTTON-Opp IN FRAME DEFAULT-FRAME /* Opp */
DO:
  RUN FlyttOppNed("Opp").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Slett
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Slett C-Win
ON CHOOSE OF BUTTON-Slett IN FRAME DEFAULT-FRAME /* Slett */
DO:
  DEF VAR wLinNr AS INTE NO-UNDO.
  DO WITH FRAME {&FRAME-NAME}:
     ASSIGN wLinNr = SELECT-Meny:LOOKUP(SELECT-Meny:SCREEN-VALUE) + 1
            wLinNr = IF SELECT-Meny:ENTRY(wLinNr) = ? THEN (SELECT-Meny:LOOKUP(SELECT-Meny:SCREEN-VALUE) - 1) ELSE wLinNr
            wEntry = SELECT-Meny:ENTRY(wLinNr).
     IF SELECT-Meny:DELETE(SELECT-Meny:SCREEN-VALUE) THEN.
     IF wEntry <> ? THEN DO:
        ASSIGN SELECT-Meny:SCREEN-VALUE = wEntry.
        APPLY "VALUE-CHANGED" TO SELECT-Meny.
     END.
     ELSE RUN TomMeny.
  END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SlettMeny
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SlettMeny C-Win
ON CHOOSE OF BUTTON-SlettMeny IN FRAME DEFAULT-FRAME /* Slett... */
DO:
  DEF VAR wSvar AS LOGI NO-UNDO.
  DO WITH FRAME {&FRAME-NAME}:
     MESSAGE 'Vil du slette meny "' + COMBO-BOX-Navn:SCREEN-VALUE + '"?'
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Slett meny"
          UPDATE wSvar.
     IF wSvar THEN DO TRANSACTION:
        FIND Meny WHERE Meny.Navn = COMBO-BOX-Navn:SCREEN-VALUE EXCLUSIVE NO-WAIT NO-ERROR.
        IF NOT AVAIL Meny THEN DO:
           IF LOCKED Meny THEN DO:
              MESSAGE "Menyen er sperret av en annen bruker." SKIP
                      "Den kan derfor ikke slettes nå."
                 VIEW-AS ALERT-BOX ERROR TITLE "Feil".
              FIND Meny WHERE Meny.Navn = COMBO-BOX-Navn:SCREEN-VALUE NO-LOCK.   
              RETURN NO-APPLY.              
           END.
           MESSAGE "Menyen er allerede slettet av en annen bruker." SKIP
                   "Trykk OK for å lukke vinduet...."
               VIEW-AS ALERT-BOX ERROR TITLE "Feil".
           APPLY "CLOSE" TO THIS-PROCEDURE.
           RETURN NO-APPLY.        
        END.
        DELETE Meny.
     END.
     IF NOT AVAIL Meny THEN RUN Initiering.
  END.           
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokProg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokProg C-Win
ON CHOOSE OF BUTTON-SokProg IN FRAME DEFAULT-FRAME /* ... */
DO:
  DEF VAR wSvar AS LOGI NO-UNDO.
  SYSTEM-DIALOG GET-FILE FILL-IN-Program
        TITLE      "Velg program ..."
        FILTERS    "Progress UIB-filer (*.w)"               "*.w",
                   "Progress kildekode (*.p)"               "*.p",
                   "Kompilerte Progress programmer (*.r)"   "*.r",
                   "Windows-programmer (*.exe)"             "*.exe"
        MUST-EXIST
        USE-FILENAME
        UPDATE wSvar.
  IF wSvar = YES THEN DO WITH FRAME {&FRAME-NAME}:
     DISPLAY FILL-IN-Program.
     APPLY "ENTRY" TO FILL-IN-Hurtigtast.
  END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokTast
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokTast C-Win
ON CHOOSE OF BUTTON-SokTast IN FRAME DEFAULT-FRAME /* ... */
DO:
  DO WITH FRAME {&FRAME-NAME}:
     RUN d-hurtig.w (FILL-IN-Hurtigtast:SCREEN-VALUE).
     IF RETURN-VALUE = "Avbryt" THEN RETURN NO-APPLY.
     ASSIGN FILL-IN-Hurtigtast:SCREEN-VALUE = RETURN-VALUE.
     APPLY "ENTRY" TO FILL-IN-Parametere.
  END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Test
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Test C-Win
ON CHOOSE OF BUTTON-Test IN FRAME DEFAULT-FRAME /* Test */
DO:
  DO WITH FRAME {&FRAME-NAME}:
     RUN KontrollerMeny("Test").
     IF RETURN-VALUE = "Feil" THEN RETURN NO-APPLY.
     
     /* Lagrer menydata i wListitems */
     RUN LagMdata.
     
     /* Fjerner evt. eksisterende meny i vinduet */
     ASSIGN {&WINDOW-NAME}:MENU-BAR = ?.
     
     /* Lager meny for vinduet og pop-up for knappen */    
     RUN lagmeny.p(wListitems,{&WINDOW-NAME},THIS-PROCEDURE). 
     RUN lagmeny.p(wListitems,SELF,          THIS-PROCEDURE).
     IF SELECT-Meny:NUM-ITEMS = 0 THEN
        ASSIGN SELF:LABEL              = "&Test"
               SELF:POPUP-MENU         = ?
               {&WINDOW-NAME}:MENU-BAR = ?.     

     ELSE ASSIGN SELF:LABEL = "Test (Høyreklikk her for pop-up meny)".
  END.   
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Venstre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Venstre C-Win
ON CHOOSE OF BUTTON-Venstre IN FRAME DEFAULT-FRAME /* <- */
DO:
  DO WITH FRAME {&FRAME-NAME}:
     ASSIGN wEntry = SUBSTR(SELECT-Meny:SCREEN-VALUE,6).
     IF SELECT-Meny:REPLACE(wEntry,SELECT-Meny:SCREEN-VALUE) THEN.
     ASSIGN SELECT-Meny:SCREEN-VALUE = wEntry.
     APPLY "VALUE-CHANGED" TO SELECT-Meny.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME COMBO-BOX-Navn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL COMBO-BOX-Navn C-Win
ON VALUE-CHANGED OF COMBO-BOX-Navn IN FRAME DEFAULT-FRAME
DO:
  RUN SjekkLagre.
  IF RETURN-VALUE = "Avbryt" THEN DO:
     IF AVAIL Meny THEN ASSIGN SELF:SCREEN-VALUE = Meny.Navn.
     RETURN NO-APPLY.
  END.
  ASSIGN wUnik = 0.
  RUN HentMeny.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-Hurtigtast
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-Hurtigtast C-Win
ON ANY-PRINTABLE OF FILL-IN-Hurtigtast IN FRAME DEFAULT-FRAME /* Hurtigtast */
DO:
  /* Sørger for at ingen skilletegn kommer inn */
  IF INDEX(wD1 + wD2 + wLevelCnt,KEYFUNCT(LASTKEY)) > 0 THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-Parametere
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-Parametere C-Win
ON ANY-PRINTABLE OF FILL-IN-Parametere IN FRAME DEFAULT-FRAME /* Parameter */
DO:
  /* Sørger for at ingen skilletegn kommer inn */
  IF INDEX(wD1 + wD2 + wLevelCnt,KEYFUNCT(LASTKEY)) > 0 THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-Program
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-Program C-Win
ON ANY-PRINTABLE OF FILL-IN-Program IN FRAME DEFAULT-FRAME /* Program */
DO:
  /* Sørger for at ingen skilletegn kommer inn */
  IF INDEX(wD1 + wD2 + wLevelCnt,KEYFUNCT(LASTKEY)) > 0 THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-Tekst
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-Tekst C-Win
ON ANY-PRINTABLE OF FILL-IN-Tekst IN FRAME DEFAULT-FRAME /* Tekst */
DO:
  /* Sørger for at ingen skilletegn kommer inn */
  IF INDEX(wD1 + wD2 + wLevelCnt,KEYFUNCT(LASTKEY)) > 0 THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME SELECT-Meny
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL SELECT-Meny C-Win
ON VALUE-CHANGED OF SELECT-Meny IN FRAME DEFAULT-FRAME
DO:
  DO WITH FRAME {&FRAME-NAME}:
     DISPL  ENTRY(7,SELF:SCREEN-VALUE,wD2) @ FILL-IN-Tekst
            ENTRY(6,SELF:SCREEN-VALUE,wD2) @ FILL-IN-Program
            ENTRY(4,SELF:SCREEN-VALUE,wD2) @ FILL-IN-Hurtigtast
            ENTRY(5,SELF:SCREEN-VALUE,wD2) @ FILL-IN-Parametere.
           
     ASSIGN TOGGLE-Deaktivert:SCREEN-VALUE = STRING(INDEX(ENTRY(3,SELF:SCREEN-VALUE,wD2),"D") > 0)
            TOGGLE-Avkryssing:SCREEN-VALUE = STRING(INDEX(ENTRY(3,SELF:SCREEN-VALUE,wD2),"C") > 0 OR
                                                    INDEX(ENTRY(3,SELF:SCREEN-VALUE,wD2),"U") > 0) 
            TOGGLE-Avkrysset:SCREEN-VALUE  = STRING(INDEX(ENTRY(3,SELF:SCREEN-VALUE,wD2),"C") > 0)
            TOGGLE-LagVindu:SCREEN-VALUE   = STRING(INDEX(ENTRY(3,SELF:SCREEN-VALUE,wD2),"V") > 0)
            TOGGLE-LibIntProc:SCREEN-VALUE = STRING(INDEX(ENTRY(3,SELF:SCREEN-VALUE,wD2),"L") > 0)
            TOGGLE-LokIntProc:SCREEN-VALUE = STRING(INDEX(ENTRY(3,SELF:SCREEN-VALUE,wD2),"O") > 0)
            TOGGLE-Persistent:SCREEN-VALUE = STRING(INDEX(ENTRY(3,SELF:SCREEN-VALUE,wD2),"P") > 0)
            TOGGLE-RT-Param:SCREEN-VALUE   = STRING(INDEX(ENTRY(3,SELF:SCREEN-VALUE,wD2),"F") > 0)
            BUTTON-Slett:SENSITIVE         = YES
            BUTTON-Opp:SENSITIVE           = SELF:LOOKUP(SELF:SCREEN-VALUE) > 1
            BUTTON-Ned:SENSITIVE           = SELF:LOOKUP(SELF:SCREEN-VALUE) < SELF:NUM-ITEMS
            BUTTON-Venstre:SENSITIVE       = LENGTH(SELF:SCREEN-VALUE) 
                                           - LENGTH(TRIM(SELF:SCREEN-VALUE)) > 0
            BUTTON-Hoyre:SENSITIVE         = LENGTH(SELF:SCREEN-VALUE) 
                                           - LENGTH(TRIM(SELF:SCREEN-VALUE)) < 55.
  
     CASE SUBSTR(TRIM(SELF:SCREEN-VALUE),1,3):
        WHEN "SUB" THEN DO:
           DISABLE FILL-IN-Program
                   BUTTON-SokProg
                   FILL-IN-HurtigTast
                   BUTTON-SokTast
                   FILL-IN-Parametere
                   TOGGLE-Avkryssing
                   TOGGLE-Avkrysset
                   TOGGLE-LagVindu
                   TOGGLE-LibIntProc
                   TOGGLE-LokIntProc
                   TOGGLE-Persistent
                   TOGGLE-RT-Param.
           ENABLE  FILL-IN-Tekst
                   TOGGLE-Deaktivert.        
        END.
        WHEN "PRG" THEN DO:
           ENABLE  FILL-IN-Tekst
                   FILL-IN-Program
                   BUTTON-SokProg
                   FILL-IN-HurtigTast
                   BUTTON-SokTast
                   FILL-IN-Parametere
                   TOGGLE-Deaktivert
                   TOGGLE-Avkryssing
                   TOGGLE-LagVindu
                   TOGGLE-LibIntProc
                   TOGGLE-LokIntProc
                   TOGGLE-Persistent
                   TOGGLE-RT-Param.
           ASSIGN TOGGLE-Avkrysset:SENSITIVE = INPUT TOGGLE-Avkryssing.
                      
        END.
        WHEN "---" THEN DO:
           DISABLE FILL-IN-Tekst
                   FILL-IN-Program
                   BUTTON-SokProg
                   FILL-IN-HurtigTast
                   BUTTON-SokTast
                   FILL-IN-Parametere
                   TOGGLE-Deaktivert
                   TOGGLE-Avkryssing
                   TOGGLE-Avkrysset
                   TOGGLE-LagVindu
                   TOGGLE-LibIntProc
                   TOGGLE-LokIntProc
                   TOGGLE-Persistent
                   TOGGLE-RT-Param.
        END.
     END CASE.
  END.      
  ASSIGN wSaveEntry = SELF:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOGGLE-Avkrysset
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOGGLE-Avkrysset C-Win
ON VALUE-CHANGED OF TOGGLE-Avkrysset IN FRAME DEFAULT-FRAME /* Initielt avkrysset */
DO:
  RUN EndreEntry.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOGGLE-Avkryssing
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOGGLE-Avkryssing C-Win
ON VALUE-CHANGED OF TOGGLE-Avkryssing IN FRAME DEFAULT-FRAME /* Avkryssing */
DO:
  DO WITH FRAME {&FRAME-NAME}:
     ASSIGN TOGGLE-Avkrysset:SENSITIVE = SELF:CHECKED.
     IF NOT TOGGLE-Avkrysset:SENSITIVE THEN 
        ASSIGN TOGGLE-Avkrysset = NO.
     DISPLAY TOGGLE-Avkrysset.      
  END.   
  RUN EndreEntry.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOGGLE-Deaktivert
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOGGLE-Deaktivert C-Win
ON VALUE-CHANGED OF TOGGLE-Deaktivert IN FRAME DEFAULT-FRAME /* Deaktivert */
DO:
  RUN EndreEntry.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOGGLE-LagVindu
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOGGLE-LagVindu C-Win
ON VALUE-CHANGED OF TOGGLE-LagVindu IN FRAME DEFAULT-FRAME /* Lag vindu (ikke for EXE-prog) */
DO:
  RUN EndreEntry.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOGGLE-LibIntProc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOGGLE-LibIntProc C-Win
ON VALUE-CHANGED OF TOGGLE-LibIntProc IN FRAME DEFAULT-FRAME /* Internprosedyre fra bibliotek */
DO: 
  DO WITH FRAME {&FRAME-NAME}:
     ASSIGN TOGGLE-LibIntProc.
     
     IF TOGGLE-LibIntProc:CHECKED THEN 
        ASSIGN TOGGLE-LokIntProc = NO.
     DISPLAY TOGGLE-LokIntProc.      
  END.   
  RUN EndreEntry.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOGGLE-LokIntProc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOGGLE-LokIntProc C-Win
ON VALUE-CHANGED OF TOGGLE-LokIntProc IN FRAME DEFAULT-FRAME /* Lokal internprosedyre */
DO:
  DO WITH FRAME {&FRAME-NAME}:
     ASSIGN TOGGLE-LokIntProc.
     
     IF TOGGLE-LokIntProc:CHECKED THEN 
        ASSIGN TOGGLE-LibIntProc = NO.
     DISPLAY TOGGLE-LibIntProc.      
  END.   
  RUN EndreEntry.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOGGLE-Persistent
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOGGLE-Persistent C-Win
ON VALUE-CHANGED OF TOGGLE-Persistent IN FRAME DEFAULT-FRAME /* Ikke persistent */
DO:
  RUN EndreEntry.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOGGLE-RT-Param
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOGGLE-RT-Param C-Win
ON VALUE-CHANGED OF TOGGLE-RT-Param IN FRAME DEFAULT-FRAME /* Run-Time Parameter */
DO:
  RUN EndreEntry.
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
/* terminate it.                                                       */
{genlib.i &WindowName = "Meny"}

ON LEAVE OF FILL-IN-Tekst, 
            FILL-IN-Program,
            FILL-IN-Hurtigtast,
            FILL-IN-Parametere,
            TOGGLE-Deaktivert,
            TOGGLE-Avkryssing,
            TOGGLE-Avkrysset,
            TOGGLE-LagVindu ,
            TOGGLE-LibIntProc,
            TOGGLE-LokIntProc,
            TOGGLE-Persistent,
            TOGGLE-RT-Param            
   IN FRAME {&FRAME-NAME} ANYWHERE
      RUN EndreEntry.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  
  RUN Initiering.

  {lng.i}
  RUN enable_UI.
  DO WITH FRAME {&FRAME-NAME}:
     IF SELECT-MENY:NUM-ITEMS > 0 THEN DO:
        ASSIGN SELECT-Meny:SCREEN-VALUE = SELECT-Meny:ENTRY(1).
        APPLY "VALUE-CHANGED" TO SELECT-Meny.
     END.   
  END.      
  assign
    {&WINDOW-NAME}:hidden = false.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

{meny.i 
   &PreRun = "MESSAGE 'RUN' wProgram + (IF wMsgPara <> '' THEN ' (' ELSE '') + wMsgPara + (IF wMsgPara <> '' THEN ')' ELSE '') + '.' SKIP(1)
                      'Vil du kjøre programmet nå?'
                VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE 'Valgt menypunkt'
                     UPDATE wSvar.
             IF wSvar = YES THEN"
}

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
  DISPLAY TOGGLE-RT-Param TOGGLE-Persistent TOGGLE-LibIntProc TOGGLE-LokIntProc 
          SELECT-Meny COMBO-BOX-Navn FILL-IN-Tekst FILL-IN-Program 
          FILL-IN-Hurtigtast FILL-IN-Parametere TOGGLE-Deaktivert 
          TOGGLE-LagVindu TOGGLE-Avkryssing TOGGLE-Avkrysset 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE TOGGLE-RT-Param TOGGLE-Persistent TOGGLE-LibIntProc TOGGLE-LokIntProc 
         BUTTON-NyMeny BUTTON-Lukk RECT-1 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  VIEW FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EndreEntry C-Win 
PROCEDURE EndreEntry :
/*------------------------------------------------------------------------------
  Purpose:     Håndterer endring av en menylinje
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR wFills       AS INTE NO-UNDO.
  DEF VAR wScreenValue AS CHAR NO-UNDO.
  DO WITH FRAME {&FRAME-NAME}:
     ASSIGN wScreenValue = SELECT-Meny:SCREEN-VALUE.
     IF wSaveEntry <> "" AND wScreenValue <> wSaveEntry THEN
         ASSIGN SELECT-Meny:SCREEN-VALUE = wSaveEntry.
       
     ASSIGN wFills = LENGTH(SELECT-Meny:SCREEN-VALUE) - LENGTH(TRIM(SELECT-Meny:SCREEN-VALUE))
            wEntry = FILL(" ",wFills) 
                   + SUBSTR(TRIM(SELECT-Meny:SCREEN-VALUE),1,3)
                   + "  " 
                   + FILL-IN-Tekst:SCREEN-VALUE
                   + FILL(" ",100)
                   + wD2
                   + ENTRY(2,SELECT-Meny:SCREEN-VALUE,wD2) /* Unikt nr. */
                   + wD2
                   + (IF INPUT TOGGLE-Deaktivert THEN "D" ELSE "")
                   + (IF INPUT TOGGLE-Avkryssing THEN (IF INPUT TOGGLE-Avkrysset THEN "C" ELSE "U") ELSE "")
                   + (IF INPUT TOGGLE-LagVindu   THEN "V" ELSE "")
                   + (IF INPUT TOGGLE-LibIntProc THEN "L" ELSE "")
                   + (IF INPUT TOGGLE-LokIntProc THEN "O" ELSE "")
                   + (IF INPUT TOGGLE-Persistent THEN "P" ELSE "")
                   + (IF INPUT TOGGLE-RT-Param   THEN "F" ELSE "")
                   + (IF SUBSTR(TRIM(SELECT-Meny:SCREEN-VALUE),1,3) = "SUB" THEN "S" ELSE "")
                   + wD2
                   + FILL-IN-Hurtigtast:SCREEN-VALUE
                   + wD2
                   + FILL-IN-Parametere:SCREEN-VALUE
                   + wD2
                   + FILL-IN-Program:SCREEN-VALUE
                   + wD2
                   + FILL-IN-Tekst:SCREEN-VALUE.
     IF SELECT-Meny:REPLACE(wEntry,SELECT-Meny:SCREEN-VALUE) THEN.
     ASSIGN SELECT-Meny:SCREEN-VALUE = wEntry. 
     IF wSaveEntry <> "" AND wScreenValue <> wSaveEntry THEN 
          ASSIGN SELECT-Meny:SCREEN-VALUE = wScreenValue.
     ELSE ASSIGN wSaveEntry = wEntry.   
  END.       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FlyttOppNed C-Win 
PROCEDURE FlyttOppNed :
/*------------------------------------------------------------------------------
  Purpose:     Flytter et element opp eller ned
  Parameters:  Inp CHAR wRetn
  Notes:       
------------------------------------------------------------------------------*/
  
   DEF INPUT PARAMETER wRetn AS CHAR NO-UNDO.
   DEF VAR wDenne AS CHAR NO-UNDO.
   DEF VAR wBytt  AS CHAR NO-UNDO.
   DEF VAR i      AS INTE NO-UNDO.
   
  DO WITH FRAME {&FRAME-NAME}:
     ASSIGN wDenne = SELECT-Meny:SCREEN-VALUE
            i      = LOOKUP(wDenne,SELECT-Meny:LIST-ITEMS,wD1)
            wBytt  = ENTRY(i + INT(STRING(wRetn = "Ned","1/-1")),SELECT-Meny:LIST-ITEMS,wD1).
     IF SELECT-Meny:REPLACE(wDenne,i + INT(STRING(wRetn = "Ned","1/-1"))) THEN.
     IF SELECT-Meny:REPLACE(wBytt,i) THEN.
     ASSIGN SELECT-Meny:SCREEN-VALUE = wDenne.
     APPLY "VALUE-CHANGED" TO SELECT-Meny.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE HentMeny C-Win 
PROCEDURE HentMeny :
/*------------------------------------------------------------------------------
  Purpose:     Henter menydata og fyller skjermbildet
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DO WITH FRAME {&FRAME-NAME}:
      IF COMBO-BOX-Navn:SCREEN-VALUE <> "" THEN
         FIND Meny WHERE Meny.Navn = COMBO-BOX-Navn:SCREEN-VALUE NO-LOCK NO-ERROR.

      ASSIGN wListitems             = ""
             SELECT-Meny:DELIMITER  = wD1
             SELECT-Meny:LIST-ITEMS = ""
             wUnik                  = 0.
                          
      /* Fyller selection-list med den første menyen */
      IF AVAIL Meny THEN DO:
         ASSIGN wMdata = Meny.Mdata
                i      = 1.
         IF wMdata <> "" THEN DO:     
            RUN LagListe(1).
      
            ASSIGN SELECT-Meny:LIST-ITEMS   = wListitems
                   SELECT-Meny:SCREEN-VALUE = SELECT-Meny:ENTRY(1).
            APPLY "VALUE-CHANGED" TO SELECT-Meny.       
         END.     
         ELSE RUN TomMeny.   
      END.   
      ELSE RUN TomMeny.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Initiering C-Win 
PROCEDURE Initiering :
/*------------------------------------------------------------------------------
  Purpose:     Initierer skjermbildet
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    
   /* Fyller combo-box for menynavn */
   ASSIGN wListitems = "".
   FOR EACH Meny FIELDS (Meny.Navn) NO-LOCK:
      ASSIGN wListitems = wListitems 
                        + (IF wListitems <> "" THEN wD1 ELSE "")
                        + Meny.Navn.
   END.                     
         
   DO WITH FRAME {&FRAME-NAME}:
      ASSIGN COMBO-BOX-Navn:DELIMITER    = wD1
             COMBO-BOX-Navn:LIST-ITEMS   = wListitems
             COMBO-BOX-Navn:INNER-LINES  = NUM-ENTRIES(wListitems,wD1)
             COMBO-BOX-Navn:SCREEN-VALUE = ENTRY(1,wListitems,wD1).
   END. 

   RUN HentMeny.
   DO WITH FRAME {&FRAME-NAME}:
      IF COMBO-BOX-Navn:LIST-ITEMS = ? THEN 
           RUN MenyMangler.
      ELSE RUN MenyFinnes. 
   END.           
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE KontrollerMeny C-Win 
PROCEDURE KontrollerMeny :
/*------------------------------------------------------------------------------
  Purpose:     Kontrollerer at menyen er ok
  Parameters:  Input char wKtype
  Notes:       
------------------------------------------------------------------------------*/
   DEF INPUT PARAMETER wKtype AS CHAR NO-UNDO.

   DEF VAR wMsg      AS CHAR NO-UNDO.
   DEF VAR wLevel    AS INTE NO-UNDO.
   DEF VAR wPreLevel AS INTE NO-UNDO.
   DEF VAR wPreType  AS CHAR NO-UNDO.
   
   Sjekk:
   DO WITH FRAME {&FRAME-NAME}:
      IF SELECT-Meny:ENTRY(1) BEGINS(" ") THEN DO:
         ASSIGN wMsg = "Første menypunkt må ligge på nivå 1." 
                     + CHR(13)
                     + "Bruk knappen <- for å flytte menypunktet til nivå 1."
                SELECT-Meny:SCREEN-VALUE = SELECT-Meny:ENTRY(1).
         APPLY "VALUE-CHANGED" TO SELECT-Meny.            
         LEAVE Sjekk.             
      END. 
      DO i = 1 TO SELECT-Meny:NUM-ITEMS.
         IF i > 1 THEN DO:
            ASSIGN wLevel    = (LENGTH(SELECT-Meny:ENTRY(i)) 
                             -  LENGTH(TRIM(SELECT-Meny:ENTRY(i))))
                             / 5 + 1
                   wPreLevel = (LENGTH(SELECT-Meny:ENTRY(i - 1)) 
                             -  LENGTH(TRIM(SELECT-Meny:ENTRY(i - 1))))
                             / 5 + 1
                   wPreType  = SUBSTR(TRIM(SELECT-Meny:ENTRY(i - 1)),1,3).
                             
             IF wLevel > wPreLevel AND wPreType <> "SUB" OR
                wLevel - wPreLevel > 1 THEN DO:
                ASSIGN wMsg = (IF TRIM(SELECT-Meny:ENTRY(i)) BEGINS "-" THEN "Linjen " ELSE
                              ('Menypunktet "' + TRIM(ENTRY(1,SELECT-Meny:ENTRY(i),wD2)) + '"')) + 'er plassert på for høyt nivå.'
                            + CHR(13)
                            + "Bruk knappen <- for å flytte " + (IF TRIM(SELECT-Meny:ENTRY(i)) BEGINS "-" THEN "den" ELSE "menypunktet") + " til et lavere nivå.".
                        SELECT-Meny:SCREEN-VALUE = SELECT-Meny:ENTRY(i).
                APPLY "VALUE-CHANGED" TO SELECT-Meny.            
                LEAVE Sjekk.             
             END.
         END.          
      END.
   END.
   IF wMsg <> "" THEN DO:
      MESSAGE (IF wKtype = "Test"  THEN "Kan ikke teste menyen fordi den inneholder en feil!" ELSE
               IF wKtype = "Lagre" THEN "Kan ikke lagre menyen fordi den inneholder en feil!" ELSE "") 
              SKIP(1) 
              wMsg VIEW-AS ALERT-BOX ERROR TITLE "Feil".
      RETURN "Feil".
   END.   
   RETURN "".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LagListe C-Win 
PROCEDURE LagListe :
/*------------------------------------------------------------------------------
  Purpose:     Lager innhold av selection-list for menyen (SELECT-Meny)
  Parameters:  Inp INT wLevel
  Notes:       
------------------------------------------------------------------------------*/
   &SCOP ListStreng   FILL(" ",100)     + wD2 + STRING(wUnik) + wD2 + wFlagg   ~
                    + wD2 + wHurtigtast + wD2 + wPrivdata     + wD2 + wProgram ~
                    + wD2 + wTekst
   
   DEF INPUT PARAMETER wLevel AS INTE NO-UNDO.
 
   REPEAT WHILE i <= NUM-ENTRIES(wMdata,wD1):
      ASSIGN wEntry      = ENTRY(i,wMdata,wD1)
             wStrLabel   = ENTRY(1,wEntry,wD2)
             wFlagg      = ENTRY(2,wEntry,wD2)
             wHurtigtast = ENTRY(3,wEntry,wD2)
             wPrivdata   = ENTRY(4,wEntry,wD2)
             wProgram    = ENTRY(5,wEntry,wD2)
             wTekst      = TRIM(SUBSTR(wStrLabel,wLevel))
             i           = i + 1.

      IF i <= NUM-ENTRIES(wMdata,wD1) THEN
           ASSIGN wNesteEntry = ENTRY(i,wMdata,     wD1)
                  wNesteLabel = ENTRY(1,wNesteEntry,wD2)
                  wNesteLevel = 2 + LENGTH(wNesteLabel) - LENGTH(TRIM(wNesteLabel + "X",wLevelCnt)).
      ELSE ASSIGN wNesteLevel = wlevel - 1.

      IF wNesteLevel = wLevel + 1 OR INDEX(wFlagg,"S") > 0 THEN DO:
         ASSIGN wUnik = wUnik + 1
                wListitems = wListitems 
                           + (IF wListitems <> "" THEN wD1 ELSE "") 
                           + FILL(" ",(wLevel - 1) * 5)
                           + "SUB  "
                           + wTekst 
                           + {&ListStreng}.
         IF wNesteLevel = wLevel + 1 THEN DO:
            RUN LagListe(wNesteLevel).
            IF wNesteLevel < wLevel THEN RETURN.
         END.   
      END.
      ELSE DO:
        
        ASSIGN j = INDEX(wFlagg,"R").
        IF j > 0 THEN
            ASSIGN wUnik      = wUnik + 1
                   wListitems = wListitems 
                              + (IF wListitems <> "" THEN wD1 ELSE "") 
                              + FILL(" ",(wLevel - 1) * 5)
                              + "-----------------------------------"
                              + {&ListStreng}.
        ELSE DO:
            ASSIGN wUnik = wUnik + 1
                   wListitems = wListitems 
                              + (IF wListitems <> "" THEN wD1 ELSE "") 
                              + FILL(" ",(wLevel - 1) * 5)
                              + "PRG  "
                              + wTekst 
                              + {&ListStreng}
                   j          = INDEX(wFlagg,"C").
            IF j = 0 THEN j = - INDEX(wFlagg,"U").
         END.
         IF      wNesteLevel = wLevel THEN NEXT.
         ELSE IF wNesteLevel < wLevel THEN RETURN.
      END.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LagMdata C-Win 
PROCEDURE LagMdata :
/*------------------------------------------------------------------------------
  Purpose:     Lager streng for menydata (i wListitems)
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   ASSIGN wListitems = "".
   DO WITH FRAME {&FRAME-NAME}:
      DO i = 1 TO NUM-ENTRIES(SELECT-Meny:LIST-ITEMS,wD1):
           ASSIGN wEntry     = ENTRY(i,SELECT-Meny:LIST-ITEMS,wD1) 
                  wListitems = wListitems
                             + (IF wListitems <> "" THEN wD1 ELSE "")
                             + FILL(wLevelCnt,INT((LENGTH(RIGHT-TRIM(wEntry)) - LENGTH(TRIM(wEntry))) / 5))
                             + ENTRY(7,wEntry,wD2)
                             + wD2 
                             + ENTRY(3,wEntry,wD2)
                             + wD2 
                             + ENTRY(4,wEntry,wD2)
                             + wD2 
                             + ENTRY(5,wEntry,wD2)
                             + wD2
                             + ENTRY(6,wEntry,wD2).
      END.                       
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LagreMeny C-Win 
PROCEDURE LagreMeny :
/*------------------------------------------------------------------------------
  Purpose:     Lagre menyen i basen
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO TRANSACTION WITH FRAME {&FRAME-NAME}:
     DEF VAR wMenyNavn AS CHAR NO-UNDO.
     ASSIGN wMenyNavn = Meny.Navn.
     FIND Meny WHERE Meny.Navn = wMenyNavn EXCLUSIVE NO-WAIT NO-ERROR.
     IF NOT AVAIL Meny THEN DO:
        IF LOCKED Meny THEN DO:
           MESSAGE "Menyen er sperret av annen bruker." SKIP
                   "Du kan derfor ikke lagre den nå!"
             VIEW-AS ALERT-BOX ERROR TITLE "Feil".
           FIND Meny WHERE Meny.Navn = wMenyNavn NO-LOCK NO-ERROR.        
           RETURN "Feil".
        END.
     END.
     ASSIGN Meny.Mdata = wListitems.   
     RELEASE Meny.
  END.
  FIND Meny WHERE Meny.Navn = wMenyNavn NO-LOCK NO-ERROR.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MenyFinnes C-Win 
PROCEDURE MenyFinnes :
/*------------------------------------------------------------------------------
  Purpose:     Enabler knapper osv. som har vært disablet fordi
               det ikke fantes en eneste meny.
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:
     ENABLE SELECT-Meny
            BUTTON-NyProgram
            BUTTON-NySubmeny
            BUTTON-NyLinje
            COMBO-BOX-Navn
            BUTTON-Lagre
            BUTTON-LagreSom
            BUTTON-SlettMeny
            BUTTON-Test.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MenyMangler C-Win 
PROCEDURE MenyMangler :
/*------------------------------------------------------------------------------
  Purpose:     Disabler alt untatt Ny meny.
  Parameters:  <none>
  Notes:       NB! De fleste felter vil allerede være disablet.
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:
     DISABLE SELECT-Meny
             BUTTON-NyProgram
             BUTTON-NySubmeny
             BUTTON-NyLinje
             COMBO-BOX-Navn
             BUTTON-Lagre
             BUTTON-LagreSom
             BUTTON-SlettMeny
             BUTTON-Test.
     APPLY "ENTRY" TO BUTTON-NyMeny.        
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NyttEntry C-Win 
PROCEDURE NyttEntry :
/*------------------------------------------------------------------------------
  Purpose:     Legger til et nytt element i menyen
  Parameters:  Input char wEntryTekst
  Notes:       
------------------------------------------------------------------------------*/
   DEF INPUT PARAMETER wEntryTekst AS CHAR NO-UNDO.
   
   DEF VAR wLinNr AS INTE NO-UNDO.
   DEF VAR wFills AS INTE NO-UNDO.
    
   DO WITH FRAME {&FRAME-NAME}:
      ASSIGN wUnik = wUnik + 1
             wFills = INDEX(SELECT-Meny:SCREEN-VALUE,SUBSTR(TRIM(SELECT-Meny:SCREEN-VALUE),1,1)) - 1
             wEntry = FILL(" ",IF wFills = ? THEN 0 ELSE wFills) 
                    + wEntryTekst
                    + FILL(" ",100) 
                    + wD2 
                    + STRING(wUnik)
                    + wD2
                    + (IF wEntryTekst BEGINS("-") THEN "R" ELSE IF wEntryTekst BEGINS("S") THEN "S" ELSE "")
                    + wD2 + wD2 + wD2 + wD2
             wLinNr = SELECT-Meny:LOOKUP(SELECT-Meny:SCREEN-VALUE).
      IF SELECT-Meny:INSERT(wEntry,(IF wLinNr = ? THEN 0 ELSE wLinNr) + 1) THEN.
      ASSIGN SELECT-Meny:SCREEN-VALUE = wEntry.
      APPLY "VALUE-CHANGED" TO SELECT-Meny.
      IF NOT wEntryTekst BEGINS "-" THEN APPLY "ENTRY" TO FILL-IN-Tekst.
   END.   

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SjekkLagre C-Win 
PROCEDURE SjekkLagre :
/*------------------------------------------------------------------------------
  Purpose:     Sjekker om det er gjort endringer i menyen, 
               spør om den skal lagres
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR wSvar AS LOGI NO-UNDO INIT YES. 
  IF AVAIL Meny THEN DO:
     RUN LagMdata.
     /* Er endringer gjort? */
     IF Meny.Mdata <> wListitems THEN DO WITH FRAME {&FRAME-NAME}:
        MESSAGE 'Vil du lagre endringene i meny "' + Meny.Navn + '"?'  
           VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO-CANCEL TITLE "Menyeditor"
           UPDATE wSvar.
        IF wSvar = ? THEN RETURN "Avbryt".
        IF wSvar = YES THEN DO:
           RUN KontrollerMeny("Lagre").
           IF RETURN-VALUE = "Feil" THEN RETURN "Avbryt".
           RUN LagreMeny.
           IF RETURN-VALUE = "Feil" THEN RETURN "Avbryt".
        END.
     END.   
  END.
  RETURN "".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE TomMeny C-Win 
PROCEDURE TomMeny :
/*------------------------------------------------------------------------------
  Purpose:     Rydder opp i skjermbildet (disabler osv.) hvis menyen er tom
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:   
     CLEAR ALL NO-PAUSE.
     ASSIGN  TOGGLE-Deaktivert = NO
             TOGGLE-Avkryssing = NO
             TOGGLE-Avkrysset  = NO
             TOGGLE-LagVindu   = NO
             TOGGLE-LibIntProc = NO
             TOGGLE-LokIntProc = NO
             TOGGLE-Persistent = YES
             TOGGLE-RT-Param   = YES.
             
     DISPLAY TOGGLE-Deaktivert
             TOGGLE-Avkryssing
             TOGGLE-Avkrysset
             TOGGLE-LagVindu
             TOGGLE-LibIntProc
             TOGGLE-LokIntProc
             TOGGLE-Persistent
             TOGGLE-RT-Param.             
             
     DISABLE BUTTON-Slett
             BUTTON-Venstre
             BUTTON-Hoyre
             BUTTON-Opp
             BUTTON-Ned
             FILL-IN-Tekst
             FILL-IN-Program
             BUTTON-SokProg
             FILL-IN-HurtigTast
             BUTTON-SokTast
             FILL-IN-Parametere
             TOGGLE-Deaktivert
             TOGGLE-Avkryssing
             TOGGLE-Avkrysset
             TOGGLE-LagVindu
             TOGGLE-LibIntProc 
             TOGGLE-LokIntProc
             TOGGLE-Persistent
             TOGGLE-RT-Param.
  END.              
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION Unik C-Win 
FUNCTION Unik RETURNS INTEGER
  ( INPUT wSV AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  Returnerer unik id fra en skjermverid i list-box
    Notes:  
------------------------------------------------------------------------------*/
  RETURN INT(SUBSTR(wSV,R-INDEX(wSV," ") + 1)).
 
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

