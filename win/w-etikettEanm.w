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
DEFINE VARIABLE cStorl AS CHARACTER  NO-UNDO.
/* Diverse flagg. */
def var wLayout_Flagg  as log  no-undo.
def var i              as int  no-undo.
def var y              as int  no-undo.
def var wwBatch        as char no-undo.
def var wDefault       as int  no-undo.
def var wValgtSkriver  as int  no-undo.
def var wParaListe     as char no-undo.
def var wKommando      as char no-undo.
DEFINE VARIABLE cStartEtiPrinter AS CHARACTER  NO-UNDO.
{etikettlogg.i &New = "New"}

{runlib.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME B-Etikettlogg

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Etikettlogg

/* Definitions for BROWSE B-Etikettlogg                                 */
&Scoped-define FIELDS-IN-QUERY-B-Etikettlogg Storl cStorl Ant bongtekst Pris   
&Scoped-define ENABLED-FIELDS-IN-QUERY-B-Etikettlogg   
&Scoped-define SELF-NAME B-Etikettlogg
&Scoped-define QUERY-STRING-B-Etikettlogg FOR EACH Etikettlogg BY SeqNr
&Scoped-define OPEN-QUERY-B-Etikettlogg OPEN QUERY {&SELF-NAME} FOR EACH Etikettlogg BY SeqNr.
&Scoped-define TABLES-IN-QUERY-B-Etikettlogg Etikettlogg
&Scoped-define FIRST-TABLE-IN-QUERY-B-Etikettlogg Etikettlogg


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-B-Etikettlogg}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS CB-Skriver CB-Layout FI-StrekKode FI-Ant ~
FI-Pris B-Etikettlogg Btn_OK B-Skriv Btn_Help RECT-1 RECT-49 RECT-50 
&Scoped-Define DISPLAYED-OBJECTS CB-Skriver CB-Layout FI-StrekKode FI-Storl ~
FI-Ant FI-Pris FILL-IN-7 FILL-IN-9 FILL-IN-10 FILL-IN-11 

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
     SIZE 42 BY 1 NO-UNDO.

DEFINE VARIABLE CB-Skriver AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Etikettskriver/kø" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Item 1",0
     DROP-DOWN-LIST
     SIZE 42 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Ant AS INTEGER FORMAT "zzz9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Pris AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Storl AS CHARACTER FORMAT "X(4)":U 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FI-StrekKode AS DECIMAL FORMAT "zzzzzzzzzzzz9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 20.6 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-10 AS CHARACTER FORMAT "X(256)":U INITIAL "Antall" 
      VIEW-AS TEXT 
     SIZE 14 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FILL-IN-11 AS CHARACTER FORMAT "X(256)":U INITIAL "Pris" 
      VIEW-AS TEXT 
     SIZE 14 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FILL-IN-7 AS CHARACTER FORMAT "X(256)":U INITIAL "Strekkode" 
      VIEW-AS TEXT 
     SIZE 15 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FILL-IN-9 AS CHARACTER FORMAT "X(256)":U INITIAL "Storl" 
      VIEW-AS TEXT 
     SIZE 7 BY .62
     FONT 6 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 3 GRAPHIC-EDGE  NO-FILL 
     SIZE 72 BY 3.14.

DEFINE RECTANGLE RECT-49
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 72 BY .05.

DEFINE RECTANGLE RECT-50
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 72 BY .05.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY B-Etikettlogg FOR 
      Etikettlogg SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE B-Etikettlogg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS B-Etikettlogg C-Win _FREEFORM
  QUERY B-Etikettlogg DISPLAY
      Storl     FORMAT "X(16)" LABEL "StrekKode"
 cStorl    FORMAT "X(4)"  LABEL "Strl"
 Ant       FORMAT ">>9"   LABEL "Antall"
 bongtekst FORMAT "x(30)" LABEL "Bongtekst"
 Pris  LABEL "Pris"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 72 BY 10.48 ROW-HEIGHT-CHARS .62 EXPANDABLE.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     CB-Skriver AT ROW 1.48 COL 19 COLON-ALIGNED
     CB-Layout AT ROW 2.91 COL 19 COLON-ALIGNED
     FI-StrekKode AT ROW 5.76 COL 3.8 COLON-ALIGNED NO-LABEL
     FI-Storl AT ROW 5.76 COL 25.6 COLON-ALIGNED NO-LABEL
     FI-Ant AT ROW 5.76 COL 33.6 COLON-ALIGNED NO-LABEL
     FI-Pris AT ROW 5.76 COL 48.6 COLON-ALIGNED NO-LABEL
     B-Etikettlogg AT ROW 7.86 COL 2
     Btn_OK AT ROW 18.52 COL 2
     B-Skriv AT ROW 18.52 COL 18
     Btn_Help AT ROW 18.52 COL 56.4
     FILL-IN-7 AT ROW 5.05 COL 4.2 COLON-ALIGNED NO-LABEL
     FILL-IN-9 AT ROW 5.05 COL 25.8 COLON-ALIGNED NO-LABEL
     FILL-IN-10 AT ROW 5.05 COL 33.8 COLON-ALIGNED NO-LABEL
     FILL-IN-11 AT ROW 5.05 COL 48.6 COLON-ALIGNED NO-LABEL
     RECT-1 AT ROW 4.52 COL 2
     RECT-49 AT ROW 1.24 COL 2
     RECT-50 AT ROW 4.19 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 73 BY 18.67.


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
         TITLE              = "Manuell utskrift av etiketter"
         HEIGHT             = 18.67
         WIDTH              = 73
         MAX-HEIGHT         = 30.81
         MAX-WIDTH          = 84.2
         VIRTUAL-HEIGHT     = 30.81
         VIRTUAL-WIDTH      = 84.2
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
   NOT-VISIBLE                                                          */
/* BROWSE-TAB B-Etikettlogg FI-Pris DEFAULT-FRAME */
/* SETTINGS FOR FILL-IN FI-Storl IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-10 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-11 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-7 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-9 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE B-Etikettlogg
/* Query rebuild information for BROWSE B-Etikettlogg
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH Etikettlogg BY SeqNr.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE B-Etikettlogg */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Manuell utskrift av etiketter */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Manuell utskrift av etiketter */
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
    DEFINE VARIABLE iStartetikett AS INTEGER    NO-UNDO.

  /* Henter utskriftskommanden. */
  {syspara.i 5 20 wValgtSkriver wEtikett_Fil}
  {syspara.i 5 21 wValgtSkriver wKommando}
  {syspar2.i 5 21 wValgtSkriver wParaListe}
  find first EtikettLogg no-lock no-error.
  if not available EtikettLogg THEN do:
      message "Ingen etiketter å skrive ut!" 
        view-as alert-box message title "Melding".
      return no-apply.
  end.
    
/*   if search(wEtikett_Fil) <> ? then                                              */
/*     do:                                                                          */
/*       message "Utskrift pågår fra en annen terminal, vent litt og forsøk igjen." */
/*         view-as alert-box message title "Melding".                               */
/*       return no-apply.                                                           */
/*     end.                                                                         */
  
  IF CAN-DO(cStartEtiPrinter,STRING(wValgtSkriver)) THEN DO:
      RUN d-Startetikett.w (INPUT-OUTPUT iStartetikett).
      if iStartetikett = 0 then
          return no-apply.
      else do:
          CREATE EtikettLogg.
          ASSIGN EtikettLogg.SeqNr = 0  
                 EtikettLogg.Storl = "STARTETIKETT"
                 EtikettLogg.Ant   = iStartetikett.
          RELEASE EtikettLogg.
      end.
  END.
  RUN SendFil.
  EMPTY TEMP-TABLE EtikettLogg.
  {&OPEN-BROWSERS-IN-QUERY-{&FRAME-NAME}}
  assign wSeqNr        = 0
         FI-StrekKode  = 0
         FI-Ant        = 0
         FI-Pris       = 0
         FI-Storl      = "".
  DISPLAY FI-StrekKode
          FI-Ant
          FI-Pris 
          FI-Storl 
          with frame DEFAULT-FRAME.
  
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


&Scoped-define SELF-NAME FI-Ant
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Ant C-Win
ON RETURN OF FI-Ant IN FRAME DEFAULT-FRAME
DO:
  APPLY "TAB":U TO FI-Ant.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Ant C-Win
ON TAB OF FI-Ant IN FRAME DEFAULT-FRAME
DO:
  if input FI-Ant <= 0 then
    do:
      message "Antall må angis!"
              view-as alert-box message title "Melding".
      return no-apply.
    end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-Pris
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Pris C-Win
ON TAB OF FI-Pris IN FRAME DEFAULT-FRAME
or "return":U of FI-Pris 
DO:
  assign frame {&FRAME-NAME}
    FI-Ant
    FI-Pris = int(input FI-Pris).    
  IF FI-Ant = 0 THEN DO:
      APPLY "ENTRY" TO FI-Ant.
      APPLY "TAB" TO FI-Ant.
      RETURN NO-APPLY.
  END.
  create EtikettLogg.
  assign
    wSeqNr                = wSeqNr + 1
    EtikettLogg.Butik     = wSeqNr /* Det skal skrives ut i seqnr ordning. */
    EtikettLogg.Vg        = ArtBas.Vg
    EtikettLogg.LopNr     = ArtBas.LopNr
    EtikettLogg.Ant       = input FI-Ant
    EtikettLogg.Storl     = input FI-StrekKode
    EtikettLogg.Bongtekst = if available ArtBas 
                              then ArtBas.Bongtekst
                              else ""
    EtikettLogg.Pris      = input FI-Pris
    EtikettLogg.SeqNr     = wSeqNr.
    RELEASE Etikettlogg.
    {&OPEN-BROWSERS-IN-QUERY-{&FRAME-NAME}}
  ASSIGN FI-StrekKode:SCREEN-VALUE = ""
         FI-StrekKode:FORMAT       = "zzzzzzzzzzzz9"
         FI-Storl:SCREEN-VALUE     = ""
         FI-Ant:SCREEN-VALUE       = "0"
         FI-Pris:SCREEN-VALUE      = "0".
  apply "entry":U to FI-StrekKode.
  return no-apply.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-Storl
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Storl C-Win
ON TAB OF FI-Storl IN FRAME DEFAULT-FRAME
or "return":U of FI-Storl
DO:
  assign frame {&FRAME-NAME}
    FI-Storl.

  if valid-handle(wLibHandle) then
    run FiksStorl in wLibHandle (input-output FI-Storl).
    
  display 
    FI-Storl 
  with frame {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-StrekKode
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-StrekKode C-Win
ON RETURN OF FI-StrekKode IN FRAME DEFAULT-FRAME
DO:
    APPLY "TAB":U TO SELF.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-StrekKode C-Win
ON TAB OF FI-StrekKode IN FRAME DEFAULT-FRAME
DO:
  DEFINE VARIABLE cStrekKode AS CHARACTER  NO-UNDO.
  if input FI-Strekkode = 0 then
      RETURN NO-APPLY.
  ASSIGN cStrekKode = IF LENGTH(INPUT FI-Strekkode) < 6 THEN STRING(INPUT FI-Strekkode) ELSE
         IF LENGTH(INPUT FI-Strekkode) = 6 THEN DYNAMIC-FUNCTION('fixChkEAN':U IN h_dproclib,
                 INPUT DYNAMIC-FUNCTION('EkspanderUPC':U IN h_dproclib, INPUT STRING(INPUT FI-StrekKode)))
         ELSE STRING(INPUT FI-Strekkode,"9999999999999").
  ASSIGN cStorl = "".
  FIND StrekKode WHERE StrekKode.Kode = cStrekKode NO-LOCK NO-ERROR.
  IF AVAIL StrekKode THEN DO:
      ASSIGN FI-StrekKode:FORMAT = "9999999999999"
             FI-StrekKode:SCREEN-VALUE = cStrekKode.
      find ArtBas OF StrekKode NO-LOCK no-error.
      if available ArtBas THEN do:
          IF ArtBas.StrTypeId > 2 THEN DO:
              FIND StrKonv WHERE StrKonv.StrKode = StrekKode.StrKode NO-LOCK NO-ERROR.
              IF AVAIL StrKonv THEN
                  ASSIGN cStorl = StrKonv.Storl.
          END.
          ASSIGN FI-Storl:SCREEN-VALUE = cStorl.
          find ArtPris no-lock where
            ArtPris.ArtikkelNr = ArtBas.ArtikkelNr and
            ArtPris.ProfilNr   = Butiker.ProfilNr no-error.
        
          if available ArtPris then
            display
              int(ArtPris.Pris[if ArtPris.Tilbud
                             then 2
                             else 1]) @ FI-Pris
            with frame {&FRAME-NAME}.
        end.
      else do:
          message "Ukjent artikkel!" 
                  view-as alert-box message title "Melding".
          return no-apply.
      end.
  END.
  else do:
    message "Ukjent artikkel!" 
            view-as alert-box message title "Melding".
    return no-apply.
  end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME B-Etikettlogg
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
  apply "entry":U to FI-StrekKode in fram {&FRAME-NAME}.
  
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

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
  DISPLAY CB-Skriver CB-Layout FI-StrekKode FI-Storl FI-Ant FI-Pris FILL-IN-7 
          FILL-IN-9 FILL-IN-10 FILL-IN-11 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE CB-Skriver CB-Layout FI-StrekKode FI-Ant FI-Pris B-Etikettlogg Btn_OK 
         B-Skriv Btn_Help RECT-1 RECT-49 RECT-50 
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
      os-delete value(wEtikett_Fil).
/*     do:                                                                  */
/*       message "Etikettfil ikke sendt. Skal den slettes?"                 */
/*         view-as alert-box question BUTTONS YES-NO-CANCEL title "Bekreft" */
/*         update wSvar as log.                                             */
/*       if wSvar= ? then /* Angre og vente litt til */                     */
/*         do:                                                              */
/*           os-delete value(wEtikett_Fil).                                 */
/*           return no-apply "AVBRYT" .                                     */
/*         end.                                                             */
/*       else if wSvar then /* Slette fil. */                               */
/*        os-delete value(wEtikett_Fil).                                    */
/*     end.                                                                 */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

