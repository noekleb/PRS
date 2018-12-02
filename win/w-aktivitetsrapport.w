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
def var IO-Liste    as char no-undo.
def var wSvar       as log  no-undo.
DEF VAR wExcEkstent AS CHAR NO-UNDO.
DEF VAR wUkeDager   AS CHAR NO-UNDO.
DEF VAR wKunde      AS CHAR NO-UNDO.
DEF VAR wSkoTex     AS CHAR NO-UNDO.
DEF VAR wAntPoster  AS INT  NO-UNDO.
DEF VAR pcoldLst    AS CHAR NO-UNDO.
DEF VAR cFirstButik AS CHAR NO-UNDO.
DEF VAR cSprak             AS CHAR NO-UNDO.

DEF STREAM sExportFile.

/* def temp-table tmpChild NO-UNDO */
/*   field wChild as handle.       */
  
DEF TEMP-TABLE tmpAktRapp NO-UNDO
  FIELD Butik        AS INT FORMAT "zzzzz9"
  FIELD Kasse        AS INT FORMAT "zzz9"
  FIELD Tid          AS CHAR FORMAT "x(5)"
  FIELD Navn         AS CHAR FORMAT "x(30)"
  FIELD SolgtAnt     AS DEC FORMAT "->,>>>,>>9.999"
  FIELD Solgt%Ant    AS DEC FORMAT "->>9.9"
  FIELD SolgtVerdi   AS DEC FORMAT "->>>,>>>,>>9.99"
  FIELD Solgt%Verdi  AS DEC FORMAT "->>9.9"
  FIELD AntKunder    AS INT FORMAT "->,>>9"
  FIELD Ant%Kunder   AS DEC FORMAT "->>9.9"
  FIELD ParPrKunde   AS DEC FORMAT "->9.9"
  FIELD VerdiPrKunde AS DEC FORMAT "->>>,>>9.99"
  FIELD VerdiPrPar   AS DEC FORMAT "->>>,>>9.99".

{xPrint.i}
{runlib.i}
{methodexcel.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-53 B-Excel RECT-54 RECT-55 RECT-56 ~
FI-FraDato FI-TilDato T-Man T-Tir T-Ons T-Tor T-Fre B-XPrint T-Lor T-Son ~
B-Butikker BUTTON-SokDato-2 RS-VisPr BUTTON-SokDato B-Exit Btn_Help ~
FILL-IN-1 
&Scoped-Define DISPLAYED-OBJECTS FI-Butiker FI-FraDato FI-TilDato T-Man ~
T-Tir T-Ons T-Tor T-Fre T-Lor T-Son RS-VisPr FILL-IN-1 FILL-IN-10 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-Butikker 
     IMAGE-UP FILE "icon/e-sokpr.ico":U NO-FOCUS
     LABEL "&Merk..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON B-Excel 
     IMAGE-UP FILE "icon/excel.bmp":U NO-FOCUS
     LABEL "Excel..." 
     SIZE 4.4 BY 1.14 TOOLTIP "Eksporter alle eller merkede tellelinjer til Excel. Alt-X.".

DEFINE BUTTON B-Exit 
     IMAGE-UP FILE "icon/e-exit":U NO-FOCUS FLAT-BUTTON
     LABEL "Button 10" 
     SIZE 4.6 BY 1.14 TOOLTIP "Avslutt".

DEFINE BUTTON B-XPrint 
     IMAGE-UP FILE "icon/e-print.bmp":U NO-FOCUS
     LABEL "XPrint..." 
     SIZE 4.4 BY 1.14 TOOLTIP "Eksporter alle eller merkede tellelinjer til X-Print. Alt-P.".

DEFINE BUTTON Btn_Help DEFAULT 
     IMAGE-UP FILE "icon/e-help":U NO-FOCUS FLAT-BUTTON
     LABEL "&Hjelp" 
     SIZE 4.6 BY 1.14 TOOLTIP "Hjelp"
     BGCOLOR 8 .

DEFINE BUTTON BUTTON-SokDato 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON BUTTON-SokDato-2 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE VARIABLE FI-Butiker AS CHARACTER FORMAT "X(200)":U 
     LABEL "Butikker" 
     VIEW-AS FILL-IN 
     SIZE 43 BY 1 NO-UNDO.

DEFINE VARIABLE FI-FraDato AS DATE FORMAT "99/99/9999":U 
     LABEL "Fra - Til Dato" 
     VIEW-AS FILL-IN 
     SIZE 21.2 BY 1 NO-UNDO.

DEFINE VARIABLE FI-TilDato AS DATE FORMAT "99/99/9999":U 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-1 AS CHARACTER FORMAT "X(256)":U INITIAL "Ukedager" 
      VIEW-AS TEXT 
     SIZE 11 BY .67 NO-UNDO.

DEFINE VARIABLE FILL-IN-10 AS CHARACTER FORMAT "X(256)":U INITIAL "Vis totaler pr." 
      VIEW-AS TEXT 
     SIZE 14 BY .62 NO-UNDO.

DEFINE VARIABLE RS-VisPr AS INTEGER INITIAL 2 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Kasse", 1,
"Butikk", 2,
"Totalt", 3
     SIZE 38 BY .95 NO-UNDO.

DEFINE RECTANGLE RECT-53
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 42 BY 2.62.

DEFINE RECTANGLE RECT-54
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 77 BY .14.

DEFINE RECTANGLE RECT-55
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 77 BY .14.

DEFINE RECTANGLE RECT-56
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 42 BY 1.67.

DEFINE VARIABLE T-Fre AS LOGICAL INITIAL no 
     LABEL "Fre" 
     VIEW-AS TOGGLE-BOX
     SIZE 9 BY .81 NO-UNDO.

DEFINE VARIABLE T-Lor AS LOGICAL INITIAL no 
     LABEL "Lør" 
     VIEW-AS TOGGLE-BOX
     SIZE 9 BY .81 NO-UNDO.

DEFINE VARIABLE T-Man AS LOGICAL INITIAL yes 
     LABEL "Man" 
     VIEW-AS TOGGLE-BOX
     SIZE 9 BY .81 NO-UNDO.

DEFINE VARIABLE T-Ons AS LOGICAL INITIAL yes 
     LABEL "Ons" 
     VIEW-AS TOGGLE-BOX
     SIZE 9 BY .81 NO-UNDO.

DEFINE VARIABLE T-Son AS LOGICAL INITIAL no 
     LABEL "Søn" 
     VIEW-AS TOGGLE-BOX
     SIZE 9 BY .81 NO-UNDO.

DEFINE VARIABLE T-Tir AS LOGICAL INITIAL yes 
     LABEL "Tir" 
     VIEW-AS TOGGLE-BOX
     SIZE 9 BY .81 NO-UNDO.

DEFINE VARIABLE T-Tor AS LOGICAL INITIAL yes 
     LABEL "Tor" 
     VIEW-AS TOGGLE-BOX
     SIZE 9 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     B-Excel AT ROW 1.24 COL 2.2
     FI-Butiker AT ROW 3.38 COL 16 COLON-ALIGNED
     FI-FraDato AT ROW 5.05 COL 16 COLON-ALIGNED
     FI-TilDato AT ROW 5.05 COL 42 COLON-ALIGNED NO-LABEL
     T-Man AT ROW 7.43 COL 21
     T-Tir AT ROW 7.43 COL 32
     T-Ons AT ROW 7.43 COL 41
     T-Tor AT ROW 7.43 COL 50
     T-Fre AT ROW 8.38 COL 21
     B-XPrint AT ROW 1.24 COL 7.2
     T-Lor AT ROW 8.38 COL 32
     T-Son AT ROW 8.38 COL 41
     B-Butikker AT ROW 3.38 COL 61.6
     BUTTON-SokDato-2 AT ROW 5.05 COL 61.4
     RS-VisPr AT ROW 11 COL 20 NO-LABEL
     BUTTON-SokDato AT ROW 5.05 COL 35.6
     B-Exit AT ROW 1.24 COL 72
     Btn_Help AT ROW 1.24 COL 67
     FILL-IN-1 AT ROW 6.48 COL 20 NO-LABEL
     FILL-IN-10 AT ROW 10.05 COL 18 COLON-ALIGNED NO-LABEL
     RECT-53 AT ROW 6.95 COL 18
     RECT-54 AT ROW 1.14 COL 1
     RECT-55 AT ROW 2.43 COL 1
     RECT-56 AT ROW 10.52 COL 18
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 77 BY 11.71.


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
         TITLE              = "Bestilling/utskrift av aktivitetsrapport"
         HEIGHT             = 11.76
         WIDTH              = 77
         MAX-HEIGHT         = 16
         MAX-WIDTH          = 92.2
         VIRTUAL-HEIGHT     = 16
         VIRTUAL-WIDTH      = 92.2
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB C-Win 
/* ************************* Included-Libraries *********************** */

{dproclibstart.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* SETTINGS FOR FILL-IN FI-Butiker IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-1 IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FILL-IN-10 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Bestilling/utskrift av aktivitetsrapport */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Bestilling/utskrift av aktivitetsrapport */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Butikker
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Butikker C-Win
ON CHOOSE OF B-Butikker IN FRAME DEFAULT-FRAME /* Merk... */
DO:
  
    assign
      IO-Liste = FI-Butiker
      .
    IF IO-Liste = "[Alle]" THEN
    DO:
        ASSIGN
            IO-Liste = ""
            .
        FOR EACH Butiker NO-LOCK WHERE
             CAN-FIND(FIRST Kasse WHERE
                      Kasse.Butik = Butiker.Butik AND
                      Kasse.Aktiv = TRUE):
          ASSIGN
            IO-Liste = IO-Liste +
                          (IF IO-Liste = ""
                              THEN ""
                              ELSE ",") +
                           STRING(Butiker.Butik).
        END.
        pcOldLst = IO-Liste.
    END.

    run d-tagbutikerBgrp.w (input-output IO-Liste).
    IF RETURN-VALUE = "Avbryt" THEN
          RETURN NO-APPLY.

    IF pcOldLst <> IO-Liste THEN
        assign
        FI-Butiker = IO-Liste.
    IF IO-Liste = "" THEN
        ASSIGN
        IO-Liste = pcOldLst
        FI-Butiker = cFirstButik.
    display 
      FI-Butiker
    with frame {&FRAME-NAME}.
  

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Excel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Excel C-Win
ON CHOOSE OF B-Excel IN FRAME DEFAULT-FRAME /* Excel... */
DO:
    RUN ValiderKrit.
    IF RETURN-VALUE = "AVBRYT" THEN
      RETURN NO-APPLY.
      
    RUN ByggAktivitetsrapport.
    IF RETURN-VALUE = "AVBRYT" THEN
      RETURN NO-APPLY.    
    IF NOT CAN-FIND(FIRST tmpAktRapp) THEN DO:
        MESSAGE "Ingen data finnes"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN NO-APPLY.
    END.
      
    RUN PrintAktivitetsrapport.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Exit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Exit C-Win
ON CHOOSE OF B-Exit IN FRAME DEFAULT-FRAME /* Button 10 */
DO:
  apply "close":U to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-XPrint
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-XPrint C-Win
ON CHOOSE OF B-XPrint IN FRAME DEFAULT-FRAME /* XPrint... */
DO:
    RUN ValiderKrit.
    IF RETURN-VALUE = "AVBRYT" THEN
      RETURN NO-APPLY.
      
    RUN ByggAktivitetsrapport.
    IF RETURN-VALUE = "AVBRYT" THEN
      RETURN NO-APPLY.    
    IF NOT CAN-FIND(FIRST tmpAktRapp) THEN DO:
        MESSAGE "Ingen data finnes"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN NO-APPLY.
    END.
    RUN XPrintrapport.
    RETURN NO-APPLY.
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


&Scoped-define SELF-NAME BUTTON-SokDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokDato C-Win
ON CHOOSE OF BUTTON-SokDato IN FRAME DEFAULT-FRAME /* ... */
or F10 of FI-FraDato
DO:

  def var wTittel as char no-undo.
  assign FI-FraDato = date(FI-FraDato:screen-value in frame {&FRAME-NAME}).

  do with frame {&FRAME-NAME}:  

  wTittel = "Datosøk".

  /* Start søkeprogram */
  {soek.i
    &Felt        = FI-FraDato
    &Program     = kalender.w
    &Frame       = {&FRAME-NAME}
    &ExtraParam  = "input wTittel"
  }   
  
  end. /* FRAME */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokDato-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokDato-2 C-Win
ON CHOOSE OF BUTTON-SokDato-2 IN FRAME DEFAULT-FRAME /* ... */
or F10 of FI-TilDato
DO:

  def var wTittel as char no-undo.
  assign FI-TilDato = date(FI-FraDato:screen-value in frame {&FRAME-NAME}).

  do with frame {&FRAME-NAME}:  

  wTittel = "Datosøk".

  /* Start søkeprogram */
  {soek.i
    &Felt        = FI-TilDato
    &Program     = kalender.w
    &Frame       = {&FRAME-NAME}
    &ExtraParam  = "input wTittel"
  }   
  
  end. /* FRAME */
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
{genlib.i
  &NoLibCall      = "Nei"
  &WindowName     = "Aktivitetsrapport"
  &PreIClose      = " "
  &PostIClose     = " "
}

/*   &PostDisable_ui = "for each tmpChild:                      */
/*                        if valid-handle(tmpChild.wChild) then */
/*                          delete procedure tmpChild.wChild.   */
/*                      end."                                   */

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

RUN InitVariabler.

{syspara.i 1 4   1 wExcEkstent}
wExcEkstent = if wExcEkstent = "" then "sdv" else wExcEkstent.   
{syspara.i 1 1 100 wKunde}
{syspara.i 1 1 101 wSkoTex}

/* HotKeySøk - DYYYYRT */
on ALT-M of frame DEFAULT-FRAME anywhere 
  do:
    apply "CHOOSE":U to B-Butikker in frame DEFAULT-FRAME.
  end.
on ALT-X of frame DEFAULT-FRAME anywhere 
    do:
      apply "CHOOSE":U to B-Excel in frame DEFAULT-FRAME.
    end.
on ALT-P of frame DEFAULT-FRAME anywhere 
    do:
      apply "CHOOSE":U to B-XPrint in frame DEFAULT-FRAME.
    end.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
    FIND bruker WHERE bruker.brukerid = USERID("skotex") NO-LOCK NO-ERROR.
    IF AVAIL bruker THEN
        cSprak = TRIM(Bruker.Lng).
  RUN enable_UI.
  {lng.i} 
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ByggAktivitetsrapport C-Win 
PROCEDURE ByggAktivitetsrapport :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEF VAR ipWeekDay    AS INT  NO-UNDO.
  DEF VAR ipButik      AS INT  NO-UNDO.
  DEF VAR ipKasse      AS INT  NO-UNDO.
  DEF VAR ipButikLst   AS CHAR NO-UNDO.
  DEF VAR ipLoop1      AS INT  NO-UNDO.
  DEF VAR ipOms_Ant    LIKE Akt_Rapp.Oms_Ant NO-UNDO.
  DEF VAR ipOms_Verd   AS DEC  NO-UNDO.
  DEF VAR ipAnt_Kunder AS INT  NO-UNDO.
  DO WITH FRAME Default-frame:
    ASSIGN
      FI-FraDato
      FI-TilDato
      RS-VisPr
      T-Man
      T-Tir
      T-Ons
      T-Tor
      T-Fre
      T-Lor
      T-Son
      wUkeDager  = ""
      ipButikLst = /*"0," + */ FI-Butiker
      wUkeDager  = ""
      wAntPoster = 0
      .
  END.
  IF ipButikLst = "[Alle]" THEN
  DO:
      ASSIGN
          ipButikLst = "0"
          .
      FOR EACH Butiker NO-LOCK WHERE
           CAN-FIND(FIRST Kasse WHERE
                    Kasse.Butik = Butiker.Butik AND
                    Kasse.Aktiv = TRUE):
        ASSIGN
          ipButikLst = ipButikLst +
                        (IF ipButikLst = ""
                            THEN ""
                            ELSE ",") +
                         STRING(Butiker.Butik).
      END.
  END.
  ELSE
      ipButikLst = "0," + ipbutikLst.

  IF T-Man THEN wUkeDager = wUkeDager + (IF wUkeDager = "" THEN "1" ELSE ",1").
  IF T-Tir THEN wUkeDager = wUkeDager + (IF wUkeDager = "" THEN "2" ELSE ",2").
  IF T-Ons THEN wUkeDager = wUkeDager + (IF wUkeDager = "" THEN "3" ELSE ",3").
  IF T-Tor THEN wUkeDager = wUkeDager + (IF wUkeDager = "" THEN "4" ELSE ",4").
  IF T-Fre THEN wUkeDager = wUkeDager + (IF wUkeDager = "" THEN "5" ELSE ",5").
  IF T-Lor THEN wUkeDager = wUkeDager + (IF wUkeDager = "" THEN "6" ELSE ",6").
  IF T-Son THEN wUkeDager = wUkeDager + (IF wUkeDager = "" THEN "7" ELSE ",7"). 
  
  /* Nullstiller temp-table */
  STATUS DEFAULT "Nullstiller temp-table...".
  FOR EACH tmpAktRapp:
    DELETE tmpAktRapp.
  END.
  MAINLOOP:
  FOR EACH Akt_Rap NO-LOCK WHERE
    Akt_Rap.Dato >= FI-FraDato AND
    Akt_Rap.Dato <= FI-TilDato
    BY Akt_Rap.Dato
    BY Akt_Rap.Butik
    BY Akt_Rap.Kasse:

    /* Undertrykker butikker */
    IF NOT CAN-DO(ipButikLst,STRING(Akt_Rap.Butik)) THEN
        NEXT MAINLOOP.
    
    /* Melding til bruker */
    ASSIGN
      wAntPoster  = wAntPoster  + 1.
    IF wAntPoster MODULO 10 = 0 THEN
      STATUS default "Antall poster " + STRING(wAntPoster) + " (" + STRING(Akt_Rap.Dato) + ").".               
               
    /* Finner postens ukedag. */
    ASSIGN
      ipWeekDay = IF WEEKDAY(Akt_Rap.Dato) = 1
                   THEN 7
                   ELSE WEEKDAY(Akt_Rap.Dato) - 1.   

    /* Undertrykker poster med feil ukedag. */
    IF NOT CAN-DO(wUkeDager,STRING(ipWeekDay)) THEN
      NEXT MAINLOOP.
      
    /* Setter temp-table key */
    IF RS-VisPr = 1 THEN
      ASSIGN
        ipButik = Akt_Rap.Butik
        ipKasse = Akt_Rap.Kasse.
    ELSE IF RS-VisPr = 2 THEN
      ASSIGN
        ipButik = Akt_Rap.Butik
        ipKasse = 0.
    ELSE IF RS-VisPr = 3 THEN
      ASSIGN
        ipButik = 0
        ipKasse = 0.
        
    /* Henter eller oppretter temp-table */
    FIND tmpAktRapp WHERE
      tmpAktRapp.Butik = ipButik AND
      tmpAktRapp.Kasse = ipKasse AND
      tmpAktRapp.Tid   = Akt_Rap.Tid_Txt NO-ERROR.
    IF NOT AVAILABLE tmpAktRapp THEN
    DO:
      FIND Butiker NO-LOCK WHERE
        Butiker.butik = ipButik NO-ERROR.
      CREATE tmpAktRapp.
      assign
        tmpAktRapp.Butik = ipButik
        tmpAktRapp.Kasse = ipKasse
        tmpAktRapp.Tid   = Akt_Rap.Tid_Txt
        tmpAktRapp.Navn  = (IF AVAILABLE Butiker 
                              THEN Butiker.ButNamn
                              ELSE "").
    END.
    
    /* Akkumulerer data */
    ASSIGN
      tmpAktRapp.SolgtAnt   = tmpAktRapp.SolgtAnt   + Akt_Rap.Oms_Ant
      tmpAktRapp.SolgtVerdi = tmpAktRapp.SolgtVerdi + Akt_Rap.Oms_Verd
      tmpAktRapp.AntKunder  = tmpAktRapp.AntKunder  + Akt_Rap.Ant_Kunder.
          
  END. /* MAINLOOP */
  
  /* Beregner totaler og andre verdier. */
  /* For butikktotal.                   */
  STATUS DEFAULT "Beregner totaler....".
  DO ipLoop1 = 1 TO NUM-ENTRIES(ipButikLst):                 
    assign
      ipOms_Ant    = 0
      ipOms_Verd   = 0
      ipAnt_Kunder = 0.
    /* Sumerer totaler */
    FOR EACH tmpAktRapp NO-LOCK WHERE
      tmpAktRapp.Butik = INT(ENTRY(ipLoop1,ipButikLst))
      BY tmpAktRapp.Butik:
        assign
          ipOms_Ant    = ipOms_Ant    + tmpAktRapp.SolgtAnt
          ipOms_Verd   = ipOms_Verd   + tmpAktRapp.SolgtVerdi
          ipAnt_Kunder = ipAnt_Kunder + tmpAktRapp.AntKunder.     
    END.
    /* Utfører beregninger. */
    FOR EACH tmpAktRapp NO-LOCK WHERE
      tmpAktRapp.Butik = INT(ENTRY(ipLoop1,ipButikLst))
      BY tmpAktRapp.Butik:
        assign
          tmpAktRapp.Solgt%Ant    = (tmpAktRapp.SolgtAnt / ipOms_Ant) * 100
          tmpAktRapp.Solgt%Verdi  = (tmpAktRapp.SolgtVerdi / ipOms_Verd) * 100
          tmpAktRapp.Ant%Kunder   = (tmpAktRapp.AntKunder / ipAnt_Kunder) * 100
          tmpAktRapp.ParPrKunde   = (tmpAktRapp.SolgtAnt / tmpAktRapp.AntKunder)
          tmpAktRapp.VerdiPrKunde = (tmpAktRapp.SolgtVerdi / tmpAktRapp.AntKunder)
          tmpAktRapp.VerdiPrPar   = (tmpAktRapp.SolgtVerdi / tmpAktRapp.SolgtAnt)
          
          tmpAktRapp.Solgt%Ant    = IF tmpAktRapp.Solgt%Ant = ? THEN 0 ELSE tmpAktRapp.Solgt%Ant
          tmpAktRapp.Solgt%Verdi  = IF tmpAktRapp.Solgt%Verdi = ? THEN 0 ELSE tmpAktRapp.Solgt%Verdi
          tmpAktRapp.Ant%Kunder   = IF tmpAktRapp.Ant%Kunder = ? THEN 0 ELSE tmpAktRapp.Ant%Kunder
          tmpAktRapp.ParPrKunde   = IF tmpAktRapp.ParPrKunde = ? THEN 0 ELSE tmpAktRapp.ParPrKunde
          tmpAktRapp.VerdiPrKunde = IF tmpAktRapp.VerdiPrKunde = ? THEN 0 ELSE tmpAktRapp.VerdiPrKunde
          tmpAktRapp.VerdiPrPar   = IF tmpAktRapp.VerdiPrPar = ? THEN 0 ELSE tmpAktRapp.VerdiPrPar
          .
    END.    
  END.
  /* Rensar 0-poster i början och på slutet för varje butik-kasse */
  RUN RensNullPoster.
  
  STATUS DEFAULT "".
  
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
  DISPLAY FI-Butiker FI-FraDato FI-TilDato T-Man T-Tir T-Ons T-Tor T-Fre T-Lor 
          T-Son RS-VisPr FILL-IN-1 FILL-IN-10 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-53 B-Excel RECT-54 RECT-55 RECT-56 FI-FraDato FI-TilDato T-Man 
         T-Tir T-Ons T-Tor T-Fre B-XPrint T-Lor T-Son B-Butikker 
         BUTTON-SokDato-2 RS-VisPr BUTTON-SokDato B-Exit Btn_Help FILL-IN-1 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitVariabler C-Win 
PROCEDURE InitVariabler :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

ASSIGN
  FI-FraDato = TODAY
  FI-TilDato = TODAY.                                                        
                                                        
/* Legger opp en liste med alle butikker som default. */
FIND Bruker WHERE Bruker.BrukerId = USERID("skotex") NO-LOCK.
IF CAN-FIND(FIRST Butikktilgang WHERE Butikktilgang.BrGrpNr = Bruker.BrGrpNr AND
                                      Butikktilgang.butik = bruker.butikknr) THEN DO:
    ASSIGN cFirstButik = STRING(Bruker.ButikkNr).
END.
ELSE DO:
    FIND FIRST Butikktilgang WHERE Butikktilgang.BrGrpNr = Bruker.BrGrpNr NO-LOCK.
    ASSIGN cFirstButik = STRING(Butikktilgang.butik).
END.
/* FI-Butiker = "[Alle]". */
FI-Butiker = cFirstButik.
/* FOR EACH Butiker NO-LOCK:               */
/*   ASSIGN                                */
/*     FI-Butiker = FI-Butiker +           */
/*                  (IF FI-Butiker = ""    */
/*                     THEN ""             */
/*                     ELSE ",") +         */
/*                  STRING(Butiker.Butik). */
/* END.                                    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PrintAktivitetsrapport C-Win 
PROCEDURE PrintAktivitetsrapport :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE wFileName AS CHAR NO-UNDO.
  DEFINE VARIABLE iCount    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE cButListe AS CHARACTER  NO-UNDO.
  IF NUM-ENTRIES(FI-Butiker) > 15 THEN DO:
      DO iCount = 1 TO 12:
          ASSIGN CButListe = CButListe + (IF CButListe <> "" THEN "," ELSE "") + ENTRY(iCount,FI-Butiker).
      END.
      ASSIGN CButListe = CButListe + " ++++".
  END.
  ELSE
      ASSIGN CButListe = FI-Butiker.
  {sww.i}
  /* Finner temporært filnavn. */
  if valid-handle(wLibHandle) then
    run GetTempFileName in wLibHandle ("AktRap", wExcEkstent, output wFileName). 
  
  /* Åpner stream */
  OUTPUT STREAM sExportFile TO VALUE(wFileName) NO-ECHO.
  
  /* Legger ut overskrifter. */
  STATUS DEFAULT "Eksporterer data...".
  EXPORT STREAM sExportFile DELIMITER ";"
    "AKTIVITETSRAPPORT"
    ""
    ""
    ""
    ""
    ""
    ""
    ""
    ""
    ""
    ""
    ""
    ""
    SKIP.                                 
    EXPORT STREAM sExportFile DELIMITER ";"
    ""
    ""
    ""
    ""
    IF CAN-DO("SVE,SE",TRIM(cSprak)) THEN "Antal" ELSE "Antall"
    ""
    IF CAN-DO("SVE,SE",TRIM(cSprak)) THEN "Värde" ELSE "Verdi"
    ""
    IF CAN-DO("SVE,SE",TRIM(cSprak)) THEN "Antal" ELSE "Antall"
    ""
    ""
    ""
    ""
    SKIP.                      
IF CAN-DO("SVE,SE",TRIM(cSprak)) THEN DO:
    EXPORT STREAM sExportFile DELIMITER ";"
      "Butik"
      "Namn"
      "Kassa"
      "Tid"
      "Sålt"
      "%"
      "Sålt"
      "%"
      "Kunder"
      "%"
      "Varor/Kund"
      "Värde/Kund"
      "Värde/Vara"
      SKIP.                                 
END.
ELSE DO:
    EXPORT STREAM sExportFile DELIMITER ";"
      "Butikk"
      "Navn"
      "Kasse"
      "Tid"
      "Solgt"
      "%"
      "Solgt"
      "%"
      "Kunder"
      "%"
      "Par/Kunde"
      "Verdi/Kunde"
      "Verdi/Par"
      SKIP.                                 
END.
                                  
  /* Eksporterer data */
  EKSPORT:
  FOR EACH tmpAktRapp no-lock
    BY tmpAktRapp.Butik
    BY tmpAktRapp.Kasse 
    BY tmpAktRapp.Tid:
                    
    EXPORT STREAM sExportFile DELIMITER ";"
      /* A */ tmpAktRapp.Butik     
      /* B */ tmpAktRapp.Navn         
      /* C */ tmpAktRapp.Kasse        
      
      /* D */ tmpAktRapp.Tid          
      
      /* E */ tmpAktRapp.SolgtAnt     
      /* F */ tmpAktRapp.Solgt%Ant    
      /* G */ tmpAktRapp.SolgtVerdi   
      /* H */ tmpAktRapp.Solgt%Verdi  
      /* I */ tmpAktRapp.AntKunder    
      /* J */ tmpAktRapp.Ant%Kunder   
      
      /* K */ tmpAktRapp.ParPrKunde   
      /* L */ tmpAktRapp.VerdiPrKunde 
      /* M */ tmpAktRapp.VerdiPrPar.
                                
  END. /* EKSPORT */
                                  
  /* Lukker stream */
  OUTPUT STREAM sExportFile CLOSE.
  IF CAN-DO("SVE,SE",TRIM(cSprak)) THEN
      STATUS DEFAULT "Importerar data i Excel...".
  ELSE
      STATUS DEFAULT "Importerer data i Excel...".
  CREATE "Excel.Application" chExcelApplication.  
  chExcelApplication:Visible = FALSE.                                     
  chWorkbooks = chExcelApplication:Workbooks:OpenText(wFileName,2,1,1,1,1,FALSE,TRUE,FALSE,FALSE,FALSE).
 
  IF CAN-DO("SVE,SE",TRIM(cSprak)) THEN
      STATUS DEFAULT "Sätter aktivt ark...".
  ELSE
      STATUS DEFAULT "Setter aktivt ark...".
  chWorkSheets = chExcelApplication:Sheets:Item(1).
 
  IF CAN-DO("SVE,SE",TRIM(cSprak)) THEN
      STATUS DEFAULT "Sätter överskrift...".
  ELSE
      STATUS DEFAULT "Setter overskrift...".
  chWorkSheets:Range("A1:M3"):Font:Bold = TRUE.
  chWorkSheets:Range("A1:M3"):Font:Italic = TRUE.

  IF CAN-DO("SVE,SE",TRIM(cSprak)) THEN
      STATUS DEFAULT "Blockindelning...".
  ELSE
      STATUS DEFAULT "Blokkinndeling...".
  chWorkSheets:Range("C:C"):borders(10):LineStyle   = 9. /*** Dobbelt linje ****/
  chWorkSheets:Range("D:D"):borders(10):LineStyle   = 9. /*** Dobbelt linje ****/
  chWorkSheets:Range("J:J"):borders(10):LineStyle   = 9. /*** Dobbelt linje ****/
  chWorkSheets:Range("M:M"):borders(10):LineStyle   = 9. /*** Dobbelt linje ****/
  chWorkSheets:Range("A3:M3"):borders(10):LineStyle   = 9. /*** Dobbelt linje ****/
  
  IF CAN-DO("SVE,SE",TRIM(cSprak)) THEN
      STATUS DEFAULT "Sätter nummerformat...".
  ELSE
      STATUS DEFAULT "Setter nummerformat...".
  chWorkSheets:Range("E:E"):NumberFormat = "# ##0".
  chWorkSheets:Range("F:F"):NumberFormat = "# ##0,0".
  chWorkSheets:Range("G:G"):NumberFormat = "# ##0,00".
  chWorkSheets:Range("H:H"):NumberFormat = "# ##0,0".
  chWorkSheets:Range("I:I"):NumberFormat = "# ##0".
  chWorkSheets:Range("J:J"):NumberFormat = "# ##0,0".
  chWorkSheets:Range("K:K"):NumberFormat = "# ##0,0".
  chWorkSheets:Range("L:L"):NumberFormat = "# ##0,0".
  chWorkSheets:Range("M:M"):NumberFormat = "# ##0,00".

  IF CAN-DO("SVE,SE",TRIM(cSprak)) THEN
      STATUS DEFAULT "Sätter överskrift...".
  ELSE
      STATUS DEFAULT "Setter overskrift...".
  chWorkSheets:Range("A1:C1"):Merge().
  chWorkSheets:Range("A1:C1"):HorizontalAlignment = 3.
        
  chWorkSheets:Range("C3:C3"):HorizontalAlignment = 4.
  chWorkSheets:Range("E2:E2"):HorizontalAlignment = 4.
  chWorkSheets:Range("E3:E3"):HorizontalAlignment = 4.   
  
  chWorkSheets:Range("F3:F3"):HorizontalAlignment = 4.   
  
  chWorkSheets:Range("G2:G2"):HorizontalAlignment = 4.
  chWorkSheets:Range("G3:G3"):HorizontalAlignment = 4.   
  
  chWorkSheets:Range("H3:H3"):HorizontalAlignment = 4.   

  chWorkSheets:Range("I2:I2"):HorizontalAlignment = 4.
  chWorkSheets:Range("I3:I3"):HorizontalAlignment = 4.   
  
  chWorkSheets:Range("J3:J3"):HorizontalAlignment = 4.   
  
  chWorkSheets:Range("K3:K3"):HorizontalAlignment = 4.   
  chWorkSheets:Range("L3:L3"):HorizontalAlignment = 4.   
  chWorkSheets:Range("M3:M3"):HorizontalAlignment = 4.   
  
  IF CAN-DO("SVE,SE",TRIM(cSprak)) THEN
      STATUS DEFAULT "Sätter AutoFit...".
  ELSE
      STATUS DEFAULT "Setter AutoFit...".
  chWorkSheets:Columns("A:M"):AutoFit().

  IF CAN-DO("SVE,SE",TRIM(cSprak)) THEN
      STATUS DEFAULT "Sätter Kriterier...".
  ELSE
      STATUS DEFAULT "Setter Kriterier...".
  chWorkSheets:PageSetup:PrintTitleRows = "A1:X3".  
  IF CAN-DO("SVE,SE",TRIM(cSprak)) THEN
      chWorkSheets:PageSetup:LeftHeader     = "Kriterier - Period: " + string(FI-FraDato) + " - " + 
                                                         STRING(FI-TilDato) + ", " +
                                                         "Butiker: " + CButListe + ", " +
                                                         "Veckodag: " + wUkeDager.
  ELSE
      chWorkSheets:PageSetup:LeftHeader     = "Kriterier - Periode: " + string(FI-FraDato) + " - " + 
                                                         STRING(FI-TilDato) + ", " +
                                                         "Butikker: " + CButListe + ", " +
                                                         "UkeDager: " + wUkeDager.
  IF CAN-DO("SVE,SE",TRIM(cSprak)) THEN
      chWorkSheets:PageSetup:RightHeader    = "Antal poster: " + String(wAntPoster).
  ELSE
      chWorkSheets:PageSetup:RightHeader    = "Antall poster: " + String(wAntPoster).
  chWorkSheets:PageSetup:LeftFooter     = wKunde.
  chWorkSheets:PageSetup:RightFooter    = wSkoTex.
  chWorksheets:PageSetup:PrintArea      = "A:M".
  chWorkSheets:PageSetup:Orientation    = 2.
  chWorkSheets:PageSetup:FitToPagesWide = 1.
  
  IF CAN-DO("SVE,SE",TRIM(cSprak)) THEN
      STATUS DEFAULT "Sätter FreezePanes...".
  ELSE
      STATUS DEFAULT "Setter FreezePanes...".
  chWorkSheets:Range("A4"):Select().
  chExcelApplication:ActiveWindow:FreezePanes = True.
  
  /* Legger inn sumlinjer. */                                        
  /* Excel macro som gjør jobben.
  Range("A4").Select
    Selection.Subtotal GroupBy:=1, Function:=xlSum, TotalList:=Array(5, 7, 9), _
        Replace:=True, PageBreaks:=True, SummaryBelowData:=True  
  */
  IF CAN-DO("SVE,SE",TRIM(cSprak)) THEN
      STATUS DEFAULT "Sätter summeringar...".
  ELSE
      STATUS DEFAULT "Setter summeringer...".
  /*chWorkSheets:Range("E4:M50"):Subtotal(1 ,1 ,"5 , 7, 9" ,TRUE ,TRUE ,TRUE ).*/   
  
  chExcelApplication:Visible = TRUE.
  
  RELEASE OBJECT chWorksheets NO-ERROR.            /* release com-handles */
  RELEASE OBJECT chWorkbooks NO-ERROR.             /* release com-handles */
  RELEASE OBJECT chExcelApplication NO-ERROR.      /* release com-handles */
  ASSIGN chWorksheets       = ?
         chWorkbooks        = ?
         chExcelApplication = ?.
  STATUS DEFAULT "".

  {swn.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Ramar C-Win 
PROCEDURE Ramar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER cRader AS CHARACTER  NO-UNDO.
    DEFINE INPUT  PARAMETER cCols  AS CHARACTER  NO-UNDO.
    DEFINE        VARIABLE  iCount AS INTEGER    NO-UNDO.
    DO iCount = 1 TO NUM-ENTRIES(cRader):
        PUT UNFORMATTED
        SUBSTITUTE("<R&1><C&2><FROM><R&1><C&3><LINE>",ENTRY(iCount,cRader),ENTRY(1,cCols),ENTRY(NUM-ENTRIES(cCols),cCols)).
    END.
    DO iCount = 1 TO NUM-ENTRIES(cCols):
        PUT UNFORMATTED
        SUBSTITUTE("<R&1><C&2><FROM><R&3><C&2><LINE>",ENTRY(1,cRader),ENTRY(iCount,cCols),ENTRY(NUM-ENTRIES(cRader),cRader)).
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RensNullPoster C-Win 
PROCEDURE RensNullPoster :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF VAR iButik AS INTE INIT ? NO-UNDO.
   DEF VAR iKasse AS INTE INIT ? NO-UNDO.
   FOR EACH tmpAktRapp
      BY tmpAktRapp.Butik
      BY tmpAktRapp.Kasse
      BY tmpAktRapp.Tid:
      IF (tmpAktRapp.Butik <> iButik OR tmpAktRapp.Kasse <> iKasse) AND 
          tmpAktRapp.SolgtAnt     = 0 AND
          tmpAktRapp.Solgt%Ant    = 0 AND
          tmpAktRapp.SolgtVerdi   = 0 AND
          tmpAktRapp.SolgtVerdi   = 0 AND
          tmpAktRapp.Solgt%Verdi  = 0 AND
          tmpAktRapp.AntKunder    = 0 AND
          tmpAktRapp.Ant%Kunder   = 0 AND
          tmpAktRapp.ParPrKunde   = 0 AND
          tmpAktRapp.VerdiPrKunde = 0 AND
          tmpAktRapp.VerdiPrPar   = 0 THEN
           DELETE tmpAktRapp.
      ELSE
          ASSIGN iButik = tmpAktRapp.Butik
                 iKasse = tmpAktRapp.Kasse.
   END.
   ASSIGN iButik = ?
          iKasse = ?.
   FOR EACH tmpAktRapp
      BY tmpAktRapp.Butik
      BY tmpAktRapp.Kasse
      BY tmpAktRapp.Tid DESCENDING:
      IF (tmpAktRapp.Butik <> iButik OR tmpAktRapp.Kasse <> iKasse) AND 
          tmpAktRapp.SolgtAnt     = 0 AND
          tmpAktRapp.Solgt%Ant    = 0 AND
          tmpAktRapp.SolgtVerdi   = 0 AND
          tmpAktRapp.SolgtVerdi   = 0 AND
          tmpAktRapp.Solgt%Verdi  = 0 AND
          tmpAktRapp.AntKunder    = 0 AND
          tmpAktRapp.Ant%Kunder   = 0 AND
          tmpAktRapp.ParPrKunde   = 0 AND
          tmpAktRapp.VerdiPrKunde = 0 AND
          tmpAktRapp.VerdiPrPar   = 0 THEN
           DELETE tmpAktRapp.
      ELSE
          ASSIGN iButik = tmpAktRapp.Butik
                 iKasse = tmpAktRapp.Kasse.
   END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ValiderKrit C-Win 
PROCEDURE ValiderKrit :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR ipStatus AS CHAR INITIAL "AVBRYT" NO-UNDO.

DO WITH FRAME Default-Frame:                   
  IF INPUT FI-FraDato > 
     INPUT FI-TilDato THEN
  DO:
    MESSAGE "FraDato er større enn TilDato!"
      VIEW-AS ALERT-BOX message TITLE "Melding".  
  END.
  ELSE IF INPUT FI-FraDato = ? OR INPUT FI-TilDato = ? THEN
  DO:
    MESSAGE "Både fra og til dato må angis!"
      VIEW-AS ALERT-BOX message TITLE "Melding".  
  END.
  ELSE ipStatus = "OK".
END.

RETURN ipStatus.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE XPrintrapport C-Win 
PROCEDURE XPrintrapport :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR pcBildeFil   AS CHAR   NO-UNDO.
  DEF VAR piCopies     AS INT    NO-UNDO.
  DEF VAR pcRappFil    AS CHAR   NO-UNDO.
  DEF VAR pcRowIdent   AS CHAR   NO-UNDO.
  DEF VAR pcRowValues  AS CHAR   NO-UNDO.
  DEF VAR pcColValues  AS CHAR   NO-UNDO.
  DEF VAR pcSkadeListe AS CHAR   NO-UNDO.
  DEF VAR ph_Dummy     AS HANDLE NO-UNDO.
  DEF VAR pcRegNr      AS CHAR   NO-UNDO.
  DEF VAR piLoop       AS INT    NO-UNDO.
  DEF VAR pcTekst      AS CHAR   NO-UNDO.
  DEF VAR piRad        AS dec    NO-UNDO.

  DEF VAR iRad         AS INTE INIT 5 NO-UNDO.
  DEF VAR iButik       AS INTE INIT ? NO-UNDO.
  DEF VAR iKasse       AS INTE INIT ? NO-UNDO.
  DEF VAR cRader       AS CHAR        NO-UNDO.
  DEF VAR cCols        AS CHAR        NO-UNDO.
  DEF VAR cUkeDar      AS CHAR        NO-UNDO.

  DEF VAR iSolgtAnt    LIKE Akt_Rapp.Oms_Ant NO-UNDO.
  DEF VAR dSolgtVerdi  AS DECI    NO-UNDO.
  DEF VAR iAntKunder   AS INTE    NO-UNDO.

 DO WITH FRAME {&FRAME-NAME}:
  ASSIGN
    piCopies = 1
    .
  ASSIGN cUkeDar = TRIM((IF T-Man:CHECKED THEN T-Man:LABEL + "," ELSE "") + 
                   (IF T-Tir:CHECKED THEN T-Tir:LABEL + "," ELSE "") + 
                   (IF T-Ons:CHECKED THEN T-Ons:LABEL + "," ELSE "") + 
                   (IF T-Tor:CHECKED THEN T-Tor:LABEL + "," ELSE "") + 
                   (IF T-Fre:CHECKED THEN T-Fre:LABEL + "," ELSE "") + 
                   (IF T-Lor:CHECKED THEN T-Lor:LABEL + "," ELSE "") + 
                   (IF T-Son:CHECKED THEN T-Son:LABEL ELSE ""),",") .
 STATUS DEFAULT "Skriver ut, venligst vent...".

  /* Henter tempfilnavn */
/*  RUN GetTmpFileName IN hLibHandle ("utk","xpr", OUTPUT pcRappFil). */
  if valid-handle(wLibHandle) then
    run GetTempFileName in wLibHandle ("AktRap", "xpr", output pcRappFil). 
  
   /* Åpner stream til skriverfil. */
  output TO value(pcRappFil) PAGED page-size 60.
  PUT CONTROL '<PDF-OUTPUT=' + REPLACE(pcRappFil,"xpr","pdf") + '>'.
  put control '<PREVIEW=ZoomToWidth>'.
/*   If nCopies > 1 then                                                              */
/*     put control substitute("<x&1>", string(nCopies) ).  /* For the fun ! */        */
/*                                                                                    */
/*   If tDialog then                                                                  */
/*     put control "<Printer?>".     /* xPrint will display the Printer Dialog Box */ */
/*   If tSetup then                                                                   */
  /*
    put control "<PrinterSetup>". /* xPrint will display the Printer Setup Box */
    */
/*          _____________________________________________________________________
*/

/*  if xLanguage <> '' then                */
/*     put CONTROL '<LANG=' xLanguage '>'. */
/*   Put control                                        */
/*             "<UNITS=MM><LINECOLOR=BLUE></PROGRESS>". */
  Define Frame PageBottom
         header
            "<C75><P10><FArial></B>" Page-Number format ">>"  "/ <#Pages>"
            skip(6)
            with page-Top stream-io width 255.
  view frame PageBottom.
  
  /*ASSIGN cCols = "3,8.5,15.5,20.5,32.5,37.5,44.5,50,55.5,64.5,74".*/
  /*                              x         x                       */
    ASSIGN cCols = "3,8.5,15.5,20.8,32.5,37.7,44.5,50.1,56,64.5,74".
  
  FOR EACH tmpAktRapp
     BY tmpAktRapp.Butik
     BY tmpAktRapp.Kasse
     BY tmpAktRapp.Tid:

     /* Sumlinje */
     IF tmpAktRapp.Butik <> iButik OR tmpAktRapp.Kasse <> iKasse THEN DO:
         IF iButik <> ? THEN DO:
             PUT UNFORMATTED "<FCourier NEW><P10><B><R" + STRING(iRad) + ">" +
             "<C4>" +  "TOT" +
             "<C6.5>" + STRING(iSolgtAnt,"->>>>>9.999")    +
/*              "<C17>" + STRING(tmpAktRapp.Solgt%Ant,">9.9")   + */
             "<C19>" + STRING(dSolgtVerdi,"->>>,>>>,>>9.99")   +
/*              "<C34>" + STRING(tmpAktRapp.Solgt%Verdi,">9.9")  + */
             "<C37.5>" + STRING(iAntKunder,"->>>,>>9")    +
/*              "<C46>" + STRING(tmpAktRapp.Ant%Kunder,">9.9")   + */
             "<C50>" + STRING(iSolgtAnt / iAntKunder,"->>9.9")   +
             "<C56>" + STRING(dSolgtVerdi / iAntKunder,"->>,>>9.99") +
             "<C65>" + STRING(dSolgtVerdi / iSolgtAnt,"->>,>>9.99") + "</B>".
              RUN Ramar(cRader,cCols).
             PAGE.
         END.

         ASSIGN iRad        = 5
                iSolgtAnt   = 0
                dSolgtVerdi = 0
                iAntKunder  = 0.
         PUT UNFORMATTED "<FArial><P20><R" + STRING(iRad) + "><B><C25>Aktivitetsrapport "  + 
             ENTRY(RS-VisPr * 2 - 1,RS-VisPr:RADIO-BUTTONS) + "</B>".
         PUT UNFORMATTED "<FArial><P14><R" + STRING(iRad + 2) + "><B>" + 
             (IF FI-TilDato = FI-FraDato THEN "<C35>" ELSE "<C31>") + FI-FraDato:SCREEN-VALUE  + (IF FI-TilDato <> FI-FraDato THEN
                                                      " - " + FI-TilDato:SCREEN-VALUE ELSE "") + "</B>".
         ASSIGN iRad = iRad + 5.
         IF tmpAktRapp.Butik <> 0 THEN
             PUT UNFORMATTED "<P10><R" + STRING(iRad) + "><B><C10>" + ENTRY(2 * 2 - 1,RS-VisPr:RADIO-BUTTONS) + 
                 ": " + STRING(tmpAktRapp.Butik) + " " + tmpAktRapp.Navn + "</B>".
         ASSIGN iRad = iRad + 1.
         IF tmpAktRapp.Kasse <> 0 THEN
             PUT UNFORMATTED "<P10><R" + STRING(iRad) + "><B><C10>" + ENTRY(1,RS-VisPr:RADIO-BUTTONS) + 
                 ": " + STRING(tmpAktRapp.Kasse) + "</B>".
         
         ASSIGN iRad = iRad + 1.
         PUT UNFORMATTED "<P10><R" + STRING(iRad) + "><B><C10>" + FILL-IN-1:SCREEN-VALUE +
               ": " + cUkeDar + "</B>".
         
         ASSIGN iRad   = iRad + 2
                cRader = STRING(iRad).
         IF CAN-DO("SVE,SE",TRIM(cSprak)) THEN DO:
             PUT UNFORMATTED "<FCourier New><P10><B><R" + STRING(iRad) + "><C10>Antal<C25>Värde<C39>Antal<C51>Varor/<C59>Värde/<C67>Värde/".
             PUT UNFORMATTED "<P10><R" + STRING(iRad + 1) + ">" +
                         "<C5>Tid<C11>Sålt<C18>%<C25>Sålt<C35>%<C39>Kunder<C47>%<C51>Kund<C60>Kund<C68>Vara</B>".
         END.
         ELSE DO:
             PUT UNFORMATTED "<FCourier New><P10><B><R" + STRING(iRad) + "><C10>Antall<C25>Verdi<C39>Antall<C51>Varer/<C59>Verdi/<C67>Verdi/".
             PUT UNFORMATTED "<P10><R" + STRING(iRad + 1) + ">" +
                         "<C5>Tid<C11>Solgt<C18>%<C25>Solgt<C35>%<C39>Kunder<C47>%<C51>Kunde<C60>Kunde<C68>Vare</B>".
         END.

         ASSIGN iRad   = iRad + 2
                cRader = cRader + "," + STRING(iRad)
                iButik = tmpAktRapp.Butik
                iKasse = tmpAktRapp.Kasse.
     END.
     ASSIGN cRader = cRader + "," + STRING(iRad + 1).
/*                   */
/*      iBlankPixels */
/*      iTknPixels). */
     PUT UNFORMATTED "<FCourier NEW><P10><R" + STRING(iRad) + ">" +
     "<C4>" +  STRING(tmpAktRapp.Tid) +
     "<C6.5>" + STRING(tmpAktRapp.SolgtAnt,"->>>>>9.999")    +
     "<C15>" + STRING(tmpAktRapp.Solgt%Ant,"->>9.9")   +
     "<C19>" + STRING(tmpAktRapp.SolgtVerdi,"->>>,>>>,>>9.99")   +
     "<C32>" + STRING(tmpAktRapp.Solgt%Verdi,"->>9.9")  +
     "<C37.5>" + STRING(tmpAktRapp.AntKunder,"->>>,>>9")    +
     "<C44>" + STRING(tmpAktRapp.Ant%Kunder,"->>9.9")   +
     "<C50>" + STRING(tmpAktRapp.ParPrKunde,"->>9.9")   +
     "<C56>" + STRING(tmpAktRapp.VerdiPrKunde,"->>,>>9.99") +
     "<C65>" + STRING(tmpAktRapp.VerdiPrPar,"->>,>>9.99").
     ASSIGN iRad = iRad + 1
            iSolgtAnt   = iSolgtAnt   + tmpAktRapp.SolgtAnt
            dSolgtVerdi = dSolgtVerdi + tmpAktRapp.SolgtVerdi
            iAntKunder  = iAntKunder  + tmpAktRapp.AntKunder
            cRader = cRader + "," + STRING(iRad + 1).
  END.
  PUT UNFORMATTED "<FCourier NEW><P10><B><R" + STRING(iRad) + ">" +
  "<C4>" +  "TOT" +
  "<C6.5>" + STRING(iSolgtAnt,"->>>>>9.999")    +
/*              "<C17>" + STRING(tmpAktRapp.Solgt%Ant,">9.9")   + */
  "<C19>" + STRING(dSolgtVerdi,"->>>,>>>,>>9.99")   +
/*              "<C34>" + STRING(tmpAktRapp.Solgt%Verdi,">9.9")  + */
  "<C37.5>" + STRING(iAntKunder,"->>>,>>9")    +
/*              "<C46>" + STRING(tmpAktRapp.Ant%Kunder,">9.9")   + */
  "<C50>" + STRING(iSolgtAnt / iAntKunder,"->>9.9")   +
  "<C56>" + STRING(dSolgtVerdi / iAntKunder,"->>,>>9.99") +
  "<C65>" + STRING(dSolgtVerdi / iSolgtAnt,"->>,>>9.99") + "</B>".
  RUN Ramar(cRader,cCols).
  /* Legger ut bilinformasjonen */

  /* Lukker stream */
  OUTPUT TO TERMINAL.

  /* Klargjør rapportfilnavnet */
  ASSIGN
    FILE-INFO:File-NAME = pcRappFil.
    
  /* Sender filen til visning og utskrift. */
 RUN PrintPDF(FILE-INFO:FULL-PATHNAME, 'POLYGON SOFTWARE AS', 'A1a9T4h4e2h_mqe2mbka' ). 

  STATUS DEFAULT " ".
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

