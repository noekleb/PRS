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

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 RECT-49 RECT-50 CB-Skriver FI-Vg ~
FI-LopNr FI-Storl FI-Ant FI-Pris TG-Ordinarie ED-TransListe Btn_OK B-Skriv ~
Btn_Help 
&Scoped-Define DISPLAYED-OBJECTS CB-Skriver CB-Layout FI-Vg FI-LopNr ~
FI-Storl FI-Ant FI-Pris FI-ArtInfo TG-Ordinarie ED-TransListe FILL-IN-7 ~
FILL-IN-8 FILL-IN-9 FILL-IN-10 FILL-IN-11 

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
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 42 BY 1 NO-UNDO.

DEFINE VARIABLE CB-Skriver AS CHARACTER FORMAT "X(256)":U 
     LABEL "Etikettskriver/kø" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 42 BY 1 NO-UNDO.

DEFINE VARIABLE ED-TransListe AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 67 BY 9.76 NO-UNDO.

DEFINE VARIABLE FI-Ant AS INTEGER FORMAT "zzz9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-ArtInfo AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 54 BY 1 NO-UNDO.

DEFINE VARIABLE FI-LopNr AS INTEGER FORMAT "zzz9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Pris AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Storl AS CHARACTER FORMAT "X(4)":U 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Vg AS INTEGER FORMAT "zz9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-10 AS CHARACTER FORMAT "X(256)":U INITIAL "Antall" 
      VIEW-AS TEXT 
     SIZE 14 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FILL-IN-11 AS CHARACTER FORMAT "X(256)":U INITIAL "Pris" 
      VIEW-AS TEXT 
     SIZE 14 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FILL-IN-7 AS CHARACTER FORMAT "X(256)":U INITIAL "VgNr" 
      VIEW-AS TEXT 
     SIZE 7 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FILL-IN-8 AS CHARACTER FORMAT "X(256)":U INITIAL "LøpNr" 
      VIEW-AS TEXT 
     SIZE 8 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FILL-IN-9 AS CHARACTER FORMAT "X(256)":U INITIAL "Storl" 
      VIEW-AS TEXT 
     SIZE 7 BY .62
     FONT 6 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 3 GRAPHIC-EDGE  NO-FILL   
     SIZE 67 BY 5.

DEFINE RECTANGLE RECT-49
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 68.4 BY .05.

DEFINE RECTANGLE RECT-50
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 68.4 BY .05.

DEFINE VARIABLE TG-Ordinarie AS LOGICAL INITIAL yes 
     LABEL "Ordinarie pris" 
     VIEW-AS TOGGLE-BOX
     SIZE 19 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     CB-Skriver AT ROW 1.48 COL 19 COLON-ALIGNED
     CB-Layout AT ROW 2.91 COL 19 COLON-ALIGNED
     FI-Vg AT ROW 5.76 COL 7 COLON-ALIGNED NO-LABEL
     FI-LopNr AT ROW 5.76 COL 15 COLON-ALIGNED NO-LABEL
     FI-Storl AT ROW 5.76 COL 24 COLON-ALIGNED NO-LABEL
     FI-Ant AT ROW 5.76 COL 32 COLON-ALIGNED NO-LABEL
     FI-Pris AT ROW 5.76 COL 47 COLON-ALIGNED NO-LABEL
     FI-ArtInfo AT ROW 7.05 COL 7 COLON-ALIGNED NO-LABEL
     TG-Ordinarie AT ROW 8.38 COL 44
     ED-TransListe AT ROW 9.81 COL 2 NO-LABEL
     Btn_OK AT ROW 19.81 COL 2
     B-Skriv AT ROW 19.81 COL 18
     Btn_Help AT ROW 19.81 COL 54
     FILL-IN-7 AT ROW 5.05 COL 7 COLON-ALIGNED NO-LABEL
     FILL-IN-8 AT ROW 5.05 COL 15 COLON-ALIGNED NO-LABEL
     FILL-IN-9 AT ROW 5.05 COL 24 COLON-ALIGNED NO-LABEL
     FILL-IN-10 AT ROW 5.05 COL 32 COLON-ALIGNED NO-LABEL
     FILL-IN-11 AT ROW 5.05 COL 47 COLON-ALIGNED NO-LABEL
     RECT-1 AT ROW 4.57 COL 2
     RECT-49 AT ROW 1.24 COL 1
     RECT-50 AT ROW 4.19 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 68.6 BY 20.19.


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
         HEIGHT             = 20.19
         WIDTH              = 68.6
         MAX-HEIGHT         = 20.19
         MAX-WIDTH          = 84.2
         VIRTUAL-HEIGHT     = 20.19
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



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   NOT-VISIBLE FRAME-NAME                                               */
/* SETTINGS FOR COMBO-BOX CB-Layout IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-ArtInfo IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-10 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-11 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-7 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-8 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-9 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
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

  /* Henter utskriftskommanden. */
  {syspara.i 5 20 wValgtSkriver wEtikett_Fil}
  {syspara.i 5 21 wValgtSkriver wKommando}
  {syspar2.i 5 21 wValgtSkriver wParaListe}

  ASSIGN wParaListe = REPLACE(wParaListe,"AVAIL;","").
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

  find first EtikettLogg no-lock no-error.
  if not available EtikettLogg then
    do:
      message "Ingen etiketter å skrive ut!" 
        view-as alert-box message title "Melding".
      return no-apply.
    end.
    
  if search(wEtikett_Fil) <> ? then
    do:
      message "Utskrift pågår fra en annen terminal, vent litt og forsøk igjen." 
        view-as alert-box message title "Melding".
      return no-apply.
    end. 
  
  def var wSvar as log initial false no-undo.
  message "skal utskrift startes?" 
     view-as alert-box question BUTTONS YES-NO title "Bekreft"
     update wSvar.
  if wSvar = false then
    return no-apply.
  else do:
    RUN SendFil.
    if return-value = "AVBRYT" then
      return no-apply.
    
    assign
      ED-TransListe = ""
      FI-ArtInfo    = ""
      FI-Vg         = 0
      FI-LopNr      = 0
      FI-Ant        = 0
      FI-Pris       = 0
      FI-Storl      = "".
    display 
      FI-Vg
      FI-LopNr 
      FI-Ant
      FI-Pris 
      FI-Storl 
      FI-ArtInfo
      ED-TransListe
    with frame DEFAULT-FRAME.
  end.
  
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
    DEFINE VARIABLE cListItemPairs AS CHARACTER  NO-UNDO.
  assign
    wValgtSkriver = INT(CB-Skriver:screen-value).
  
  {syspar2.i 5 20 wValgtSkriver cListItemPairs} 
  cListItemPairs = cListItemPairs + "," + STRING(wValgtSkriver).
  CB-Layout:LIST-ITEM-PAIRS = cListItemPairs.
  CB-Layout:SCREEN-VALUE = ENTRY(2,CB-Layout:LIST-ITEM-PAIRS).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ED-TransListe
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ED-TransListe C-Win
ON ANY-PRINTABLE OF ED-TransListe IN FRAME DEFAULT-FRAME
DO:
  apply "entry":U to FI-Vg.
  return no-apply.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-Ant
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Ant C-Win
ON TAB OF FI-Ant IN FRAME DEFAULT-FRAME
or "return":U of FI-Ant
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


&Scoped-define SELF-NAME FI-LopNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-LopNr C-Win
ON TAB OF FI-LopNr IN FRAME DEFAULT-FRAME
or "return":U of FI-LopNr
DO:
  find ArtBas no-lock where
    ArtBas.Vg = input FI-Vg and
    ArtBas.LopNr = input FI-LopNr no-error.
  if available ArtBas then
    do:
      assign
        FI-ArtInfo = string(ArtBas.Vg,"zz9") + "/" +
                     string(ArtBas.LopNr,"zzz9") + " " +
                     ArtBas.Bongtekst.
      find ArtPris no-lock where
        ArtPris.ArtikkelNr = ArtBas.ArtikkelNr and
        ArtPris.ProfilNr   = Butiker.ProfilNr no-error.
        
      display 
        FI-ArtInfo 
      with frame {&FRAME-NAME}.

      if available ArtPris then
        display
          int(ArtPris.Pris[if TG-ordinarie:CHECKED THEN 1 ELSE 
                           IF ArtPris.Tilbud
                         then 2
                         else 1]) @ FI-Pris
        with frame {&FRAME-NAME}.
    end.
  else do:
    message "Ukjent artikkel!" 
            view-as alert-box message title "Melding".
    return no-apply.
      /*---
      assign
        FI-ArtInfo = string(FI-Vg,"zz9") + "/" +
                     string(FI-LopNr,"zzz9").
      ---*/               
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
    FI-Vg
    FI-LopNr
    FI-Storl
    FI-Ant
    FI-Pris = int(input FI-Pris).    

  if available ArtBas then
    do:
      assign
        ED-TransListe = string(ArtBas.Vg,"zz9") + "/" +
                        string(ArtBas.LopNr,"zzz9") + " " +
                        ArtBas.Bongtekst + " " +
                        FI-Storl + " " + 
                        string(input FI-Ant)+ " " + 
                        string(input FI-Pris)  + chr(13) +
                        ED-TransListe.
    end.
  else
        ED-TransListe = string(input FI-Vg,"zz9") + "/" +
                        string(input FI-LopNr,"zzz9") + " " +
                        FI-Storl + " " + 
                        string(input FI-Ant)+ " " + 
                        string(input FI-Pris)  + chr(13) +
                        ED-TransListe.
                        
  display 
    ED-TransListe 
  with frame {&FRAME-NAME}.
  
  create EtikettLogg.
  assign
    wSeqNr                = wSeqNr + 1
    EtikettLogg.Butik     = wSeqNr /* Det skal skrives ut i seqnr ordning. */
    EtikettLogg.Vg        = input FI-Vg
    EtikettLogg.LopNr     = input FI-LopNr
    EtikettLogg.Ant       = input FI-Ant
    EtikettLogg.Storl     = input FI-Storl
    EtikettLogg.Bongtekst = if available ArtBas 
                              then ArtBas.Bongtekst
                              else ""
    EtikettLogg.Pris      = input FI-Pris
    EtikettLogg.SeqNr     = wSeqNr.
  
  apply "entry":U to FI-Vg.
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


&Scoped-define SELF-NAME FI-Vg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Vg C-Win
ON TAB OF FI-Vg IN FRAME DEFAULT-FRAME
or "return":U of FI-Vg
DO:
  find VarGr no-lock where
    VarGr.Vg = input FI-Vg no-error.
  if available VarGr then
    do:
      assign
        FI-ArtInfo = string(VarGr.Vg,"zz9").
      display FI-ArtInfo with frame {&FRAME-NAME}.
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
  &WindowName     = "Manuell utskrift av etiketter"
  &PreIClose      = "RUN SaveSettings."
}


/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

assign
  wDefault      =  1
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

  RUN InitSkriverValg (input wDefault).

  /* Henter innstilling fra lokal inifil. */
/*   RUN GetSettings (OUTPUT wTekst1, OUTPUT wTekst2).                          */
/*   IF wTekst1 <> ? AND wTekst2 <> ? THEN                                      */
/*   DO:                                                                        */
/*     find SysPara no-lock where                                               */
/*       SysPara.SysHId  = 5 and                                                */
/*       SysPara.SysGr   = 20 AND                                               */
/*       SysPAra.ParaNr  = INT(wTekst2) NO-ERROR.                               */
/*     IF AVAILABLE SysPara THEN                                                */
/*         ASSIGN                                                               */
/*           CB-Layout:LIST-ITEMS = SysPara.Parameter2                          */
/*           CB-Layout = wTekst1.                                               */
/*         .                                                                    */
/*                                                                              */
/*     find SysPara no-lock where                                               */
/*       SysPara.SysHId  = 5 and                                                */
/*       SysPara.SysGr   = 21 AND                                               */
/*       SysPAra.ParaNr  = INT(wTekst2) NO-ERROR.                               */
/*     IF AVAILABLE SysPara THEN                                                */
/*         CB-Skriver = string(SysPara.ParaNr,"99") + ":"+ SysPAra.Beskrivelse. */
/*   END.                                                                       */
/*                                                                              */
/*   DISPLAY                                                                    */
/*       CB-Layout                                                              */
/*       CB-Skriver                                                             */
/*       WITH FRAME Default-frame.                                              */


  RUN enable_UI. 
  APPLY "VALUE-CHANGED" TO CB-SKRIVER.
  {lng.i} 
  DYNAMIC-FUNCTION("InitTranslation",THIS-PROCEDURE:CURRENT-WINDOW).

  apply "entry":U to FI-Vg in fram {&FRAME-NAME}.
  
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
  DISPLAY CB-Skriver CB-Layout FI-Vg FI-LopNr FI-Storl FI-Ant FI-Pris FI-ArtInfo 
          TG-Ordinarie ED-TransListe FILL-IN-7 FILL-IN-8 FILL-IN-9 FILL-IN-10 
          FILL-IN-11 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-1 RECT-49 RECT-50 CB-Skriver FI-Vg FI-LopNr FI-Storl FI-Ant 
         FI-Pris TG-Ordinarie ED-TransListe Btn_OK B-Skriv Btn_Help 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitSkriverValg C-Win 
PROCEDURE InitSkriverValg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def input parameter wSkriver as int no-undo.
DEFINE VARIABLE cListItemPairs AS CHARACTER  NO-UNDO.
wSkriver = 0.
/* Skriverliste. */
do with frame DEFAULT-FRAME:
/*   assign                         */
/*     CB-Skriver            = " "  */
/*     CB-Skriver:List-Items = " ". */
  For each SysPara no-lock where
    SysPara.SysHId  = 5 and
    SysPara.SysGr   = 21 AND
    SysPara.Parameter2 BEGINS "AVAIL":

    ASSIGN cListItemPairs = cListItemPairs + (IF cListItemPairs <> "" THEN "," ELSE "") + STRING(SysPara.ParaNr,"99") + ":" + SysPara.Beskrivelse + "," + STRING(SysPara.ParaNr).
/*     assign                                                       */
/*       CB-Skriver:list-items = CB-Skriver:list-items +            */
/*                               (if CB-Skriver:list-items = ""     */
/*                                  then ""                         */
/*                                  else ",") +                     */
/*                               string(SysPara.ParaNr,"99") + ":"+ */
/*                               SysPara.Beskrivelse.               */
/*       IF wSkriver = 0 THEN                                       */
/*           wSkriver = SysPara.ParaNr.                             */
  end.
/*   assign                                         */
/*     CB-Skriver = entry(1,CB-Skriver:list-items). */
end.
CB-Skriver:LIST-ITEM-PAIRS = cListItemPairs.
CB-Skriver:SCREEN-VALUE = ENTRY(2,CB-Skriver:LIST-ITEM-PAIRS).
wSkriver = INT(ENTRY(2,CB-Skriver:LIST-ITEM-PAIRS)).
/* LayoutListe */
{syspar2.i 5 20 wSkriver cListItemPairs} 
cListItemPairs = cListItemPairs + "," + STRING(wSkriver).
CB-Layout:LIST-ITEM-PAIRS = cListItemPairs.
CB-Layout:SCREEN-VALUE = ENTRY(2,CB-Layout:LIST-ITEM-PAIRS).

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

