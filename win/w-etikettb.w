&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          skotex           PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win

/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE EtiBatch NO-UNDO LIKE etikett
       field Beskrivelse as char format "x(50)"
       field Antall as inte format ">>9".


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
def input parameter wBestNr as int no-undo.

/* Local Variable Definitions ---                                       */
DEF VAR wTekst1      AS CHAR NO-UNDO.
DEF VAR wTekst2      AS CHAR NO-UNDO.
def var wCl          as int  no-undo.
def var wSeqNr       as int  no-undo.
def var wLayout      as char no-undo.
def var wBatch       as char no-undo.
def var wEtikett_Fil as char no-undo.
def var wFirma       as char no-undo.
def var Linje        as char extent 50.
def var BLinje       as char extent 50.
def var TStorlek     as char format "x(4)".
def var Teller       as int  no-undo.

/* Diverse flagg. */
def var wLayout_Flagg  as log  no-undo.
def var i              as int  no-undo.
def var y              as int  no-undo.
def var wwBatch        as char no-undo.
def var wDefault       as int  no-undo.
def var wValgtSkriver  as int  no-undo.
def var wParaListe     as char no-undo.
def var wKommando      as char no-undo.
DEFINE VARIABLE iStartetikett AS INTEGER    NO-UNDO.
DEFINE VARIABLE cStartEtiPrinter AS CHARACTER  NO-UNDO.

{etikettlogg.i &NEW="NEW"}

{runlib.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BROWSE-EtiBatch

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES EtiBatch

/* Definitions for BROWSE BROWSE-EtiBatch                               */
&Scoped-define FIELDS-IN-QUERY-BROWSE-EtiBatch EtiBatch.levinnr ~
EtiBatch.Antall EtiBatch.texten 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-EtiBatch 
&Scoped-define QUERY-STRING-BROWSE-EtiBatch FOR EACH EtiBatch NO-LOCK ~
    BY EtiBatch.levinnr ~
       BY EtiBatch.rad INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BROWSE-EtiBatch OPEN QUERY BROWSE-EtiBatch FOR EACH EtiBatch NO-LOCK ~
    BY EtiBatch.levinnr ~
       BY EtiBatch.rad INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BROWSE-EtiBatch EtiBatch
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-EtiBatch EtiBatch


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-BROWSE-EtiBatch}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-49 RECT-50 CB-Skriver CB-Layout ~
BROWSE-EtiBatch B-Velg B-Slett ED-TransListe Btn_OK B-Skriv Btn_Help 
&Scoped-Define DISPLAYED-OBJECTS CB-Skriver CB-Layout FI-Valgte FI-Antall ~
ED-TransListe 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-Skriv 
     LABEL "&Skriv ut" 
     SIZE 26 BY 1.14.

DEFINE BUTTON B-Slett 
     LABEL "Slett" 
     SIZE 15 BY 1.14.

DEFINE BUTTON B-Velg 
     LABEL "Velg" 
     SIZE 15 BY 1.14.

DEFINE BUTTON Btn_Help DEFAULT 
     LABEL "&Hjelp" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO DEFAULT 
     LABEL "Avslutt" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE CB-Layout AS CHARACTER FORMAT "X(256)":U 
     LABEL "Layout" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 35.6 BY 1 NO-UNDO.

DEFINE VARIABLE CB-Skriver AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "Etikettskriver/kø" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Item 1",0
     DROP-DOWN-LIST
     SIZE 42 BY 1 NO-UNDO.

DEFINE VARIABLE ED-TransListe AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 104 BY 2.62 NO-UNDO.

DEFINE VARIABLE FI-Antall AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "Antall" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FI-Valgte AS CHARACTER FORMAT "X(256)":U 
     LABEL "Valgte" 
     VIEW-AS FILL-IN 
     SIZE 48 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-49
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 105 BY .05.

DEFINE RECTANGLE RECT-50
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 105 BY .05.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-EtiBatch FOR 
      EtiBatch SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-EtiBatch
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-EtiBatch C-Win _STRUCTURED
  QUERY BROWSE-EtiBatch NO-LOCK DISPLAY
      EtiBatch.levinnr COLUMN-LABEL "Bestilling" FORMAT "-zzzzzz9":U
      EtiBatch.Antall COLUMN-LABEL "Antall" FORMAT ">>9":U
      EtiBatch.texten COLUMN-LABEL "Merknad" FORMAT "x(80)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SEPARATORS SIZE 104 BY 9.86 ROW-HEIGHT-CHARS .62 EXPANDABLE.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     CB-Skriver AT ROW 1.57 COL 18 COLON-ALIGNED
     CB-Layout AT ROW 1.57 COL 68 COLON-ALIGNED
     BROWSE-EtiBatch AT ROW 3.38 COL 2
     B-Velg AT ROW 13.38 COL 63
     FI-Valgte AT ROW 13.43 COL 5.8
     FI-Antall AT ROW 14.57 COL 6.6
     B-Slett AT ROW 14.57 COL 63
     ED-TransListe AT ROW 15.76 COL 2 NO-LABEL
     Btn_OK AT ROW 19.1 COL 2
     B-Skriv AT ROW 19.1 COL 42
     Btn_Help AT ROW 19.1 COL 90
     RECT-49 AT ROW 1.24 COL 1
     RECT-50 AT ROW 2.91 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 106 BY 19.33.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
   Temp-Tables and Buffers:
      TABLE: EtiBatch T "?" NO-UNDO skotex etikett
      ADDITIONAL-FIELDS:
          field Beskrivelse as char format "x(50)"
          field Antall as inte format ">>9"
      END-FIELDS.
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Batch uskrift av etiketter"
         HEIGHT             = 19.33
         WIDTH              = 106
         MAX-HEIGHT         = 19.33
         MAX-WIDTH          = 106
         VIRTUAL-HEIGHT     = 19.33
         VIRTUAL-WIDTH      = 106
         TOP-ONLY           = yes
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
   NOT-VISIBLE                                                          */
/* BROWSE-TAB BROWSE-EtiBatch CB-Layout DEFAULT-FRAME */
/* SETTINGS FOR FILL-IN FI-Antall IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN FI-Valgte IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-L                                                    */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-EtiBatch
/* Query rebuild information for BROWSE BROWSE-EtiBatch
     _TblList          = "Temp-Tables.EtiBatch"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _OrdList          = "Temp-Tables.EtiBatch.levinnr|yes,Temp-Tables.EtiBatch.rad|yes"
     _FldNameList[1]   > Temp-Tables.EtiBatch.levinnr
"EtiBatch.levinnr" "Bestilling" "-zzzzzz9" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > "_<CALC>"
"EtiBatch.Antall" "Antall" ">>9" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.EtiBatch.texten
"EtiBatch.texten" "Merknad" "x(80)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is OPENED
*/  /* BROWSE BROWSE-EtiBatch */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Batch uskrift av etiketter */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Batch uskrift av etiketter */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Skriv
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Skriv C-Win
ON CHOOSE OF B-Skriv IN FRAME DEFAULT-FRAME /* Skriv ut */
DO:
  DEFINE VARIABLE iCount AS INTEGER    NO-UNDO.
  IF FI-Valgte:SCREEN-VALUE = "" THEN
      ASSIGN FI-Valgte:SCREEN-VALUE = STRING(EtiBatch.levinnr)
             FI-Antall:SCREEN-VALUE = STRING(EtiBatch.Antall).
  /* Henter utskriftskommanden. */
  {syspara.i 5 20 wValgtSkriver wEtikett_Fil}
  {syspara.i 5 21 wValgtSkriver wKommando}
  {syspar2.i 5 21 wValgtSkriver wParaListe}

  /* Lagrer valgt Layout */
  IF VALID-HANDLE(wLibHandle) THEN
      RUN SetEtikettLayout IN wLibHandle (INPUT CB-Layout:SCREEN-VALUE).

   case num-entries(wParaListe,";"):
     when 1 then
       ED-TransListe = "Skriver nr: " + string(wValgtSkriver) + chr(13) +
                       "Kommando  : " + wKommando + " " +
                       entry(1,wParaListe,";") + " " + 
                       wEtikett_Fil + chr(13) + 
                       ED-TransListe.
     when 2 then
       ED-TransListe = "Skriver nr: " + string(wValgtSkriver) + chr(13) +
                       "Kommando  : " + wKommando + " " +
                       entry(1,wParaListe,";") + " " + 
                       entry(2,wParaListe,";") + " " + 
                       wEtikett_Fil + chr(13) + 
                       ED-TransListe.
     when 3 then
       ED-TransListe = "Skriver nr: " + string(wValgtSkriver) + chr(13) +
                       "Kommando  : " + wKommando + " " +
                       entry(1,wParaListe,";") + " " + 
                       entry(3,wParaListe,";") + " " + 
                       wEtikett_Fil + chr(13) + 
                       ED-TransListe.
     when 4 then
       ED-TransListe = "Skriver nr: " + string(wValgtSkriver) + chr(13) +
                       "Kommando  : " + wKommando + " " +
                       entry(1,wParaListe,";") + " " + 
                       entry(4,wParaListe,";") + " " + 
                       wEtikett_Fil + chr(13) + 
                       ED-TransListe.
     otherwise    
       ED-TransListe = "Skriver nr: " + string(wValgtSkriver) + chr(13) +
                       "Kommando  : " + wKommando + " " +
                       wParaListe + " " + 
                       wEtikett_Fil + chr(13) + 
                       ED-TransListe.
   end case.
  display 
    ED-TransListe
  with frame {&FRAME-NAME}.
  IF CAN-DO(cStartEtiPrinter,CB-Skriver:SCREEN-VALUE) THEN DO:
      RUN d-Startetikett.w (INPUT-OUTPUT iStartetikett).
      ASSIGN iStartetikett = MAXIMUM(1,iStartetikett).
  END.
  run ByggEtikettLogg.

  find first EtikettLogg no-lock no-error.
  if not available EtikettLogg then
    do:
      message "Ingen etiketter å skrive ut!" 
        view-as alert-box message title "Melding".
      return no-apply.
    end.
  if search(wEtikett_Fil) <> ? then
    do:
      message "Utskrift pågår fra en annen terminal, vent litt og forsøk igjen." SKIP
              "Filnavn: " SEARCH(wEtikett_Fil)
        view-as alert-box message title "Melding".
      return no-apply.
    end. 
  
/*   def var wSvar as log initial false no-undo.                  */
/*   message "skal utskrift startes?"                             */
/*      view-as alert-box question BUTTONS YES-NO title "Bekreft" */
/*      update wSvar.                                             */
/*   if wSvar = false then                                        */
/*     return no-apply.                                           */
  SENDFIL:
  do:
    RUN SendFil.
    if return-value = "AVBRYT" THEN DO:
      ASSIGN FI-Valgte:SCREEN-VALUE = ""
             FI-Antall:SCREEN-VALUE = "0".
      return no-apply.
    END.
  
    /* Kaster utskrevne etiketter. */
    do icount = 1 TO NUM-ENTRIES(FI-Valgte:SCREEN-VALUE) transaction:
      FIND EtiBatch WHERE EtiBatch.LevInNr = INT(ENTRY(iCount,FI-Valgte:SCREEN-VALUE)).
      for each Etikett exclusive-lock where
        Etikett.LevInNr = EtiBatch.LevInNr:
        delete Etikett.
      end.
      delete EtiBatch.
    end.
    {&OPEN-QUERY-{&BROWSE-NAME}}
    
    assign
      FI-Valgte:SCREEN-VALUE = ""
      FI-Antall:SCREEN-VALUE = "0"
      ED-TransListe = "".
    display 
      ED-TransListe
    with frame DEFAULT-FRAME.

    APPLY "choose" TO Btn_Ok IN FRAME Default-Frame.
  end. /* SENDFIL */
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Slett
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Slett C-Win
ON CHOOSE OF B-Slett IN FRAME DEFAULT-FRAME /* Slett */
DO:
  ASSIGN FI-Valgte:SCREEN-VALUE = ""
         FI-Antall:SCREEN-VALUE = "0".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Velg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Velg C-Win
ON CHOOSE OF B-Velg IN FRAME DEFAULT-FRAME /* Velg */
DO:
    IF BROWSE-EtiBatch:FOCUSED-ROW = ? THEN
        RETURN NO-APPLY.
    IF NOT CAN-DO(FI-Valgte:SCREEN-VALUE,STRING(EtiBatch.levinnr)) THEN
    ASSIGN FI-Valgte:SCREEN-VALUE = FI-Valgte:SCREEN-VALUE +
        (IF FI-Valgte:SCREEN-VALUE = "" THEN "" ELSE ",") +
           STRING(EtiBatch.levinnr)
           FI-Antall:SCREEN-VALUE = 
                  STRING(INT(FI-Antall:SCREEN-VALUE) + EtiBatch.Antall).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-EtiBatch
&Scoped-define SELF-NAME BROWSE-EtiBatch
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-EtiBatch C-Win
ON MOUSE-SELECT-DBLCLICK OF BROWSE-EtiBatch IN FRAME DEFAULT-FRAME
DO:
    IF NOT CAN-DO(FI-Valgte:SCREEN-VALUE,STRING(EtiBatch.levinnr)) THEN
        ASSIGN FI-Valgte:SCREEN-VALUE = FI-Valgte:SCREEN-VALUE +
             (IF FI-Valgte:SCREEN-VALUE = "" THEN "" ELSE ",") + STRING(EtiBatch.levinnr)
               FI-Antall:SCREEN-VALUE = STRING(INT(FI-Antall:SCREEN-VALUE) + EtiBatch.Antall).
    ELSE DO:
        ASSIGN
        FI-Valgte:SCREEN-VALUE = REPLACE(FI-Valgte:SCREEN-VALUE,STRING(EtiBatch.levinnr),"")
        FI-Valgte:SCREEN-VALUE = REPLACE(FI-Valgte:SCREEN-VALUE,",,",",")
        FI-Valgte:SCREEN-VALUE = TRIM(FI-Valgte:SCREEN-VALUE,",")
        FI-Antall:SCREEN-VALUE = STRING(INT(FI-Antall:SCREEN-VALUE) - EtiBatch.Antall).
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Help
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Help C-Win
ON CHOOSE OF Btn_Help IN FRAME DEFAULT-FRAME /* Hjelp */
OR HELP OF FRAME {&FRAME-NAME}
DO: /* Call Help Function (or a simple message). */
  {winhlp.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK C-Win
ON CHOOSE OF Btn_OK IN FRAME DEFAULT-FRAME /* Avslutt */
DO:
  apply "close":U to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CB-Skriver
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-Skriver C-Win
ON VALUE-CHANGED OF CB-Skriver IN FRAME DEFAULT-FRAME /* Etikettskriver/kø */
DO:
    ASSIGN wValgtSkriver = int(CB-Skriver:SCREEN-VALUE).

   {syspar2.i 5 20 wValgtSkriver CB-Layout:LIST-ITEMS} 
   CB-Layout = ENTRY(1,CB-Layout:LIST-ITEMS).
   DISPLAY CB-Layout WITH FRAME {&FRAME-NAME}.


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ED-TransListe
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ED-TransListe C-Win
ON ANY-PRINTABLE OF ED-TransListe IN FRAME DEFAULT-FRAME
DO:
  apply "entry":U to BROWSE-EtiBatch.
  return no-apply.
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
  &WindowName     = "Manuell utskrift av etiketter"
  &PreIClose      = "RUN SaveSettings."
}

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.
run ByggListe.

assign
  wDefault      =  2
  wValgtSkriver = wDefault.

/* Sentrallager */
{syspara.i 5 1 1 wCl INT}
find butiker no-lock where
  Butiker.Butik = wCl no-error.
if not available Butiker then
  do:
    message "Sentrallager er ikke lagt opp!"
      view-as alert-box message title "Melding".
    return no-apply.
  end.
{syspara.i 1  1 100 wFirma}

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

  /* Henter innstilling fra lokal inifil. */
  RUN GetSettings (OUTPUT wTekst1, OUTPUT wTekst2).
  IF wTekst1 <> ? AND wTekst2 <> ? THEN
  DO:
    find SysPara no-lock where
      SysPara.SysHId  = 5 and
      SysPara.SysGr   = 20 AND
      SysPAra.ParaNr  = INT(wTekst2) NO-ERROR.
    IF AVAILABLE SysPara THEN
        ASSIGN wDefault = SysPAra.ParaNr.
        .
  END.
  RUN InitCombo.

  RUN enable_UI. 
  {lng.i} 
  APPLY "VALUE-CHANGED" TO CB-Skriver.
  apply "entry":U to BROWSE-EtiBatch in fram {&FRAME-NAME}.
  
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ByggEtikettLogg C-Win 
PROCEDURE ByggEtikettLogg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iCount AS INTEGER    NO-UNDO.
  if not available EtiBatch then
    return.
    
  /*For sikkerhets skyld.*/
  for each EtikettLogg:
    delete EtikettLogg.
  end.
  
  assign wSeqNr = 0.
  IF iStartetikett > 1 THEN DO:
      CREATE EtikettLogg.
      ASSIGN EtikettLogg.Butik  = 0
             EtikettLogg.SeqNr  = 0
             EtikettLogg.Storl  = "STARTETIKETT"
             EtikettLogg.Ant    = iStartetikett.
      /* vid EAN etiketter skall startetikett anges, vi använder Ant-fältet */
  END.
  DO iCount = 1 TO NUM-ENTRIES(FI-Valgte:SCREEN-VALUE IN FRAME {&FRAME-NAME}):
      for each Etikett no-lock where
        Etikett.LevInNr = INT(ENTRY(iCount,FI-Valgte:SCREEN-VALUE)):
        find ArtBas no-lock where
          ArtBas.Vg    = Etikett.Vg and
          ArtBas.LopNr = Etikett.LopNr no-error.
        create EtikettLogg.
        assign
          wSeqNr                = wSeqNr + 1
          EtikettLogg.Vg        = Etikett.Vg
          EtikettLogg.LopNr     = Etikett.LopNr
          EtikettLogg.Ant       = Etikett.Antal
          EtikettLogg.Storl     = Etikett.Storlek
          EtikettLogg.Bongtekst = if available ArtBas 
                                    then ArtBas.Bongtekst
                                    else ""
          EtikettLogg.Pris      = Etikett.Pris
          EtikettLogg.Butik     = Etikett.Butik
          EtikettLogg.SeqNr     = wSeqNr.    
      end.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ByggListe C-Win 
PROCEDURE ByggListe :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def var wTekst as char no-undo.
DEFINE VARIABLE iAntall AS INTEGER    NO-UNDO.
for each Etikett no-lock where   
  (if (wBestNr = ? or wBestNr = 0) then
     Etikett.LevInNr > 0
   else 
     Etikett.LevInNr = wBestNr)
  break
  by Etikett.LevInNr
  by Etikett.Rad:
  ASSIGN iAntall = iAntall + etikett.antal.
  if first-of(Etikett.LevInNr) then
    do:
      find BestHode no-lock where
        BestHode.BestNr = Etikett.LevInNr no-error.
      if available BestHode then
        do:
          find ArtBAs no-lock where
            ArtBAs.ArtikkelNr = BestHode.ArtikkelNr NO-ERROR.
          assign
            wTekst = "Dato: " + String(BestHode.Bestillingsdato) + " " +
                     BestHode.Beskrivelse + " " +
                     " Art: " +
                     (IF AVAILABLE ArtBas
                        THEN string(ArtBas.Vg)
                        ELSE "**") + "/" + 
                     (IF AVAILABLE ArtBas
                        THEN string(ArtBAs.LopNr)
                        ELSE "**") + " Lev/LevArtNr: " + 
                     string(BestHode.LevNr) + " "+ 
                     string(BestHode.LevKod). 
                     
        end.
      else do:
        assign
          wTekst = "Ordre: " + String(Etikett.LevInNr) + " Art: " +
                   string(Etikett.Vg) + "/" + 
                   string(Etikett.LopNr).       
      end.
      create EtiBatch.
      assign
        EtiBatch.Butik   = Etikett.Butik
        EtiBatch.Vg      = Etikett.Vg
        EtiBatch.LopNr   = Etikett.LopNr
        EtiBatch.Storlek = Etikett.Storlek
        EtiBatch.LevInNr = Etikett.LevInNr
        EtiBatch.Texten  = wTekst.
    end.
    if LAST-OF(Etikett.LevInNr) THEN DO:
        ASSIGN EtiBatch.Antall = iAntall
               iAntall         = 0.
    END.
end.  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Conv C-Win 
PROCEDURE Conv :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def input-output parameter navn as char no-undo.

SORRY:
repeat on error undo, retry:
    if index(navn,"~{") <> 0 then
    navn = substring(navn,1,index(navn,"~{") - 1)
             + chr(126)
             + "132"
             + substring(navn,index(navn,"~{") + 1,length(navn)).
    if index(navn,"~|") <> 0 then
    navn = substring(navn,1,index(navn,"~|") - 1)
             + chr(126)
             + "148"
             + substring(navn,index(navn,"~|") + 1,length(navn)).
    if index(navn,"~}") <> 0 then
    navn = substring(navn,1,index(navn,"~}") - 1)
             + chr(126)
             + "134"
             + substring(navn,index(navn,"~}") + 1,length(navn)).
    if index(navn,"~[") <> 0 then
    navn = substring(navn,1,index(navn,"~[") - 1)
             + chr(126)
             + "142"
             + substring(navn,index(navn,"~[") + 1,length(navn)).
    if index(navn,"~\") <> 0 then
    navn = substring(navn,1,index(navn,"~\") - 1)
             + chr(126)
             + "153"
             + substring(navn,index(navn,"~\") + 1,length(navn)).
    if index(navn,"~]") <> 0 then
    navn = substring(navn,1,index(navn,"~]") - 1)
             + chr(126)
             + "143"
             + substring(navn,index(navn,"~]") + 1,length(navn)).
    if index(navn,"~{") <> 0 or
       index(navn,"~|") <> 0 or
       index(navn,"~}") <> 0 or
       index(navn,"~[") <> 0 or
       index(navn,"~\") <> 0 or
       index(navn,"~]") <> 0 then
    do:
        next SORRY.
    end.
    else leave SORRY.
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
  DISPLAY CB-Skriver CB-Layout FI-Valgte FI-Antall ED-TransListe 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-49 RECT-50 CB-Skriver CB-Layout BROWSE-EtiBatch B-Velg B-Slett 
         ED-TransListe Btn_OK B-Skriv Btn_Help 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetSettings C-Win 
PROCEDURE GetSettings :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEF OUTPUT parameter wTekst1 AS CHAR NO-UNDO.
  DEF OUTPUT PARAMETER wTekst2 AS CHAR NO-UNDO.
  
  IF VALID-HANDLE(wLibHandle) THEN
  DO WITH FRAME default-frame:
    RUN HentParametre IN wLibHandle ("ETIKETTER", "LAYOUT",  OUTPUT wTekst1).
    RUN HentParametre IN wLibHandle ("ETIKETTER", "SKRIVER", OUTPUT wTekst2).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitCombo C-Win 
PROCEDURE InitCombo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cListItemPairs AS CHARACTER  NO-UNDO.
    DEFINE BUFFER bSysPara FOR SysPara.
    FOR EACH SysPara WHERE SysPara.SysHId = 5 AND 
                           SysPara.SysGr = 21 NO-LOCK:
        IF CAN-DO(REPLACE(SysPara.Parameter2,";",","),"AVAIL") THEN DO:
            FIND bSysPara WHERE bSysPara.SysHId = 5 AND 
                                bSysPara.SysGr  = 20 AND
                                bSysPara.ParaNr = SysPara.ParaNr NO-LOCK NO-ERROR.
            IF AVAIL bSysPara THEN DO:
                ASSIGN cListItemPairs = cListItemPairs + (IF cListItemPairs = "" THEN "" ELSE ",") +
                       bSysPara.Parameter2 + "," + STRING(bSysPara.ParaNr).
                IF CAN-DO(REPLACE(SysPara.Parameter2,";",","),"START") THEN
                    ASSIGN cStartEtiPrinter = cStartEtiPrinter + (IF cStartEtiPrinter = "" THEN "" ELSE ",") + 
                       STRING(bSysPara.ParaNr).
            END.
        END.
    END.
    ASSIGN CB-Skriver:LIST-ITEM-PAIRS IN FRAME {&FRAME-NAME}= cListItemPairs.
    IF CAN-DO(cListItemPairs,STRING(wDefault)) THEN
        ASSIGN CB-Skriver = wDefault.
    ELSE
        ASSIGN CB-Skriver = INT(ENTRY(2,cListItemPairs)).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SaveSettings C-Win 
PROCEDURE SaveSettings :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  IF VALID-HANDLE(wLibHandle) THEN
  DO WITH FRAME default-frame:
    RUN LagreParametre IN wLibHandle ("ETIKETTER", "LAYOUT", input INPUT CB-Layout).
    RUN LagreParametre IN wLibHandle ("ETIKETTER", "SKRIVER", INPUT entry(1,INPUT CB-Skriver,":")).
  END.

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

  run x-etikettsend.w (input wValgtSkriver).      

  if search(wEtikett_Fil) <> ? then
    do:
      message "Etikettfil ikke sendt. Skal den slettes?" 
        view-as alert-box question BUTTONS YES-NO-CANCEL title "Bekreft" 
        update wSvar as log.
      if wSvar= ? then /* Angre og vente litt til */
        do:
          os-delete value(wEtikett_Fil).
          return no-apply "AVBRYT" .
        end.
      else if wSvar then /* Slette fil. */
       os-delete value(wEtikett_Fil).
    end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

