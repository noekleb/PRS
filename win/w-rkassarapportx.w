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
DEF VAR IO-Liste    AS CHAR NO-UNDO.
DEF VAR wSvar       AS LOG  NO-UNDO.
DEF VAR wExcEkstent AS CHAR NO-UNDO.
DEF VAR wUkeDager   AS CHAR NO-UNDO.
DEF VAR wKunde      AS CHAR NO-UNDO.
DEF VAR wSkoTex     AS CHAR NO-UNDO.
DEF VAR wAntPoster  AS INT  NO-UNDO.
DEFINE VARIABLE wBruttoOmsetning AS DECIMAL    NO-UNDO.
DEF VAR cLogo              AS CHAR                NO-UNDO.
DEF VAR cTittel            AS CHAR FORMAT "x(26)" NO-UNDO.
DEF VAR cSubTittel1        AS CHAR FORMAT "x(70)" NO-UNDO.
DEF VAR cSubTittel2        AS CHAR FORMAT "x(70)" NO-UNDO.
DEF VAR cKrit1             AS CHAR                NO-UNDO.
DEF VAR cKrit2             AS CHAR                NO-UNDO.
DEF VAR cFirma             AS CHAR FORMAT "x(40)" NO-UNDO.
DEF VAR cClInfo            AS CHAR FORMAT "x(70)" NO-UNDO.
DEF VAR pcOldLst           AS CHAR NO-UNDO.

DEF VAR lKortSum           AS DEC NO-UNDO.
DEF VAR lDagensKontStrom   AS DEC NO-UNDO.
DEF VAR lKasseSlutt        AS DEC NO-UNDO.
DEF VAR lKasseEndring      AS DEC NO-UNDO.
DEF VAR lKasseDiff         AS DEC NO-UNDO.
DEF VAR cBokfNr            AS CHAR FORMAT "x(20)" NO-UNDO.
DEF VAR ipButikLst         AS CHAR NO-UNDO.
DEF VAR iCl                AS INT  NO-UNDO.
DEF VAR cTekst             AS CHAR NO-UNDO.
DEF VAR cHKInst            AS CHAR NO-UNDO.
DEF VAR cFirstButik        AS CHAR NO-UNDO.
DEF VAR lDirekte           AS LOG  NO-UNDO.
DEF VAR cButBatchPrinter AS CHARACTER  NO-UNDO.
DEF VAR lNonSale1 AS DEC FORMAT "->>,>>>,>>9.99" NO-UNDO.
DEF VAR lNonSale2  AS DEC FORMAT "->>,>>>,>>9.99" NO-UNDO.

DEF VAR iAntallUtbetBonger AS INT NO-UNDO.
DEF VAR lVerdiUtbetBonger AS DEC NO-UNDO.

DEFINE TEMP-TABLE tt_tilgode NO-UNDO LIKE Tilgode.
DEFINE TEMP-TABLE tt_kortretur NO-UNDO  /* bara vid 1 dag */
    FIELD butik AS INTE
    FIELD iAnt  AS INTE
    FIELD dSum AS DEC
    INDEX butik IS PRIMARY UNIQUE butik.

DEF TEMP-TABLE tmpChild
  FIELD wChild AS HANDLE.
  
{tmpKort_spes.i &NEW = NEW &SHARED = SHARED}
{tmpKas_Rap.i &NEW = NEW &SHARED = SHARED}
{tt_NON_Sale_spes.i &NEW = NEW &SHARED = SHARED}

/* {xPrint.i} */
{runlib.i}

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
&Scoped-Define ENABLED-OBJECTS B-SokButikker RECT-54 RECT-55 RECT-56 ~
CB-Rapport FI-FraDato FI-TilDato T-Kuntotal B-Butikker BUTTON-SokDato-2 ~
B-XPrint BUTTON-SokDato B-Exit Btn_Help 
&Scoped-Define DISPLAYED-OBJECTS CB-Rapport FI-Butiker FI-FraDato ~
FI-TilDato T-Kuntotal 

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

DEFINE BUTTON B-Exit 
     IMAGE-UP FILE "icon/e-exit":U NO-FOCUS FLAT-BUTTON
     LABEL "Button 10" 
     SIZE 4.6 BY 1.1 TOOLTIP "Avslutt".

DEFINE BUTTON B-SokButikker 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON B-XPrint 
     IMAGE-UP FILE "icon/e-print.bmp":U NO-FOCUS
     LABEL "XPrint..." 
     SIZE 4.4 BY 1.1 TOOLTIP "Eksporter alle eller merkede tellelinjer til X-Print. Alt-P.".

DEFINE BUTTON Btn_Help DEFAULT 
     IMAGE-UP FILE "icon/e-help":U NO-FOCUS FLAT-BUTTON
     LABEL "&Hjelp" 
     SIZE 4.6 BY 1.1 TOOLTIP "Hjelp"
     BGCOLOR 8 .

DEFINE BUTTON BUTTON-SokDato 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON BUTTON-SokDato-2 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE VARIABLE CB-Rapport AS INTEGER FORMAT "9":U INITIAL 0 
     LABEL "Rapporttype" 
     VIEW-AS COMBO-BOX INNER-LINES 20
     LIST-ITEM-PAIRS "Finansrapport",0,
                     "Bokføringsbilag",1
     DROP-DOWN-LIST
     SIZE 37.6 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Butiker AS CHARACTER FORMAT "X(256)":U 
     LABEL "Butikker" 
     VIEW-AS FILL-IN 
     SIZE 37.6 BY 1 NO-UNDO.

DEFINE VARIABLE FI-FraDato AS DATE FORMAT "99/99/99":U 
     LABEL "Fra - Til Dato" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE FI-TilDato AS DATE FORMAT "99/99/99":U 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-54
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 77 BY .1.

DEFINE RECTANGLE RECT-55
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 77 BY .1.

DEFINE RECTANGLE RECT-56
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 77 BY 6.19.

DEFINE VARIABLE T-Kuntotal AS LOGICAL INITIAL no 
     LABEL "Kun totaler" 
     VIEW-AS TOGGLE-BOX
     SIZE 35 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     B-SokButikker AT ROW 4.57 COL 55.8
     CB-Rapport AT ROW 2.91 COL 16 COLON-ALIGNED
     FI-Butiker AT ROW 4.57 COL 16 COLON-ALIGNED
     FI-FraDato AT ROW 6.24 COL 16 COLON-ALIGNED
     FI-TilDato AT ROW 6.24 COL 37.6 COLON-ALIGNED NO-LABEL
     T-Kuntotal AT ROW 7.76 COL 18.2
     B-Butikker AT ROW 4.57 COL 55.8
     BUTTON-SokDato-2 AT ROW 6.24 COL 55.8
     B-XPrint AT ROW 1.24 COL 1.4
     BUTTON-SokDato AT ROW 6.24 COL 34.2
     B-Exit AT ROW 1.24 COL 72
     Btn_Help AT ROW 1.24 COL 67
     RECT-54 AT ROW 1.1 COL 1
     RECT-55 AT ROW 2.43 COL 1
     RECT-56 AT ROW 2.67 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 77 BY 7.95.


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
         TITLE              = "Bestilling/utskrift av finansrapport"
         HEIGHT             = 7.95
         WIDTH              = 77
         MAX-HEIGHT         = 12.43
         MAX-WIDTH          = 77
         VIRTUAL-HEIGHT     = 12.43
         VIRTUAL-WIDTH      = 77
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
/* SETTINGS FOR FILL-IN FI-Butiker IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Bestilling/utskrift av finansrapport */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Bestilling/utskrift av finansrapport */
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
  
  ASSIGN
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
    
  RUN d-tagbutikerBgrp.w (INPUT-OUTPUT IO-Liste).
  IF RETURN-VALUE = "Avbryt" THEN
        RETURN NO-APPLY.
        
  IF pcOldLst <> IO-Liste THEN
      ASSIGN
      FI-Butiker = IO-Liste.
  IF IO-Liste = "" THEN
      ASSIGN
      IO-Liste   = pcOldLst
      FI-Butiker = cFirstButik.
/*       FI-Butiker = "[Alle]". */

  DISPLAY 
    FI-Butiker
  WITH FRAME {&FRAME-NAME}.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Exit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Exit C-Win
ON CHOOSE OF B-Exit IN FRAME DEFAULT-FRAME /* Button 10 */
DO:
  APPLY "close":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SokButikker
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SokButikker C-Win
ON CHOOSE OF B-SokButikker IN FRAME DEFAULT-FRAME /* ... */
DO:
  DO WITH FRAME FRAME-ArtInfo:
  /* Kaller søkerutine */
    RUN gbutikerBgrp.w (
      INPUT-OUTPUT cTekst, /* Returstreng - chr(1) separert */
      "", /* Feltliste avgrensningsfelt (kommaseparert) */
      "", /* Feltverdier (chr(1) sep) */ 
      FI-Butiker:SCREEN-VALUE /* Post markøren skal stå på */
      ).
    IF RETURN-VALUE = "AVBRYT" THEN
        RETURN NO-APPLY.
    IF NUM-ENTRIES(cTekst,CHR(1)) >= 3 THEN
    DO:
        /* Legger opp verdier I de aktuelle feltene */
        ASSIGN
          FI-Butiker:SCREEN-VALUE = ENTRY(2,cTekst,CHR(1))
          .
    END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-XPrint
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-XPrint C-Win
ON CHOOSE OF B-XPrint IN FRAME DEFAULT-FRAME /* XPrint... */
DO:
    ASSIGN INPUT FI-FraDato
           INPUT FI-TilDato
           FI-Butiker
           T-Kuntotal.
/*   IF INPUT CB-Rapport = 1 THEN                              */
/*     ASSIGN                                                  */
/*           FI-TilDato:SCREEN-VALUE = FI-FraDato:SCREEN-VALUE */
/* /* FI-TilDato:SCREEN-VALUE = STRING(INPUT FI-FraDato + 1) TEST */ */
/*     .      */
/*   ELSE     */
/*     ASSIGN */
/*     .      */
  
  RUN ValiderKrit.
  IF RETURN-VALUE = "AVBRYT" THEN
    RETURN NO-APPLY.
    
  RUN ByggFinansRapport.
  IF RETURN-VALUE = "AVBRYT" THEN
    RETURN NO-APPLY.    
  RUN ByggKortSpes.
/*     OUTPUT TO "CLIPBOARD".    */
/*      FOR EACH tmpKas_Rap      */
/*       BREAK                   */
/*       BY tmpKas_Rap.Dato      */
/*       BY tmpKas_Rap.butikk    */
/*       BY tmpKas_Rap.Kasse     */
/*       BY tmpKas_Rap.Z_Nummer: */
/*          EXPORT tmpKas_Rap.   */
/*      END.                     */
/*      OUTPUT CLOSE. RETURN.    */
    CASE INPUT CB-Rapport:
      WHEN 0 THEN RUN XPrintrapport.
      WHEN 1 THEN RUN XPrintBilag.
    END CASE.
    
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
OR F10 OF FI-FraDato
DO:

  DEF VAR wTittel AS CHAR NO-UNDO.
  ASSIGN FI-FraDato = DATE(FI-FraDato:screen-value IN FRAME {&FRAME-NAME}).

  DO WITH FRAME {&FRAME-NAME}:  

  wTittel = "Datosøk".

  /* Start søkeprogram */
  {soek.i
    &Felt        = FI-FraDato
    &Program     = kalender.w
    &Frame       = {&FRAME-NAME}
    &ExtraParam  = "input wTittel"
  }   
  
  END. /* FRAME */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokDato-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokDato-2 C-Win
ON CHOOSE OF BUTTON-SokDato-2 IN FRAME DEFAULT-FRAME /* ... */
OR F10 OF FI-TilDato
DO:

  DEF VAR wTittel AS CHAR NO-UNDO.
  ASSIGN FI-TilDato = DATE(FI-FraDato:screen-value IN FRAME {&FRAME-NAME}).

  DO WITH FRAME {&FRAME-NAME}:  

  wTittel = "Datosøk".

  /* Start søkeprogram */
  {soek.i
    &Felt        = FI-TilDato
    &Program     = kalender.w
    &Frame       = {&FRAME-NAME}
    &ExtraParam  = "input wTittel"
  }   
  
  END. /* FRAME */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CB-Rapport
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-Rapport C-Win
ON VALUE-CHANGED OF CB-Rapport IN FRAME DEFAULT-FRAME /* Rapporttype */
DO:
  IF INPUT CB-Rapport = 1 THEN
  DO:
      ASSIGN
      T-KunTotal:SCREEN-VALUE    = "yes"
      T-KunTotal:SENSITIVE       = FALSE
      FI-TilDato:SCREEN-VALUE    = FI-FraDato:SCREEN-VALUE
/*       FI-TilDato:SENSITIVE       = FALSE */
      FI-TilDato:SENSITIVE       = TRUE
      BUTTON-SokDato-2:SENSITIVE = FALSE
/*       FI-Butiker:SCREEN-VALUE    = string(clButiker.Butik) */
      FI-Butiker:SCREEN-VALUE    = cFirstButik
      B-SokButikker:HIDDEN       = FALSE 
      B-Butikker:HIDDEN          = TRUE
      .
  END.
  ELSE DO:
      ASSIGN
        T-KunTotal:SENSITIVE       = TRUE
        FI-TilDato:SENSITIVE       = TRUE 
        BUTTON-SokDato-2:SENSITIVE = TRUE
/*         FI-Butiker:SCREEN-VALUE    = "[Alle]" */
        FI-Butiker:SCREEN-VALUE    = cFirstButik
        B-SokButikker:HIDDEN       = TRUE 
        B-Butikker:HIDDEN          = FALSE
        .
  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-FraDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-FraDato C-Win
ON LEAVE OF FI-FraDato IN FRAME DEFAULT-FRAME /* Fra - Til Dato */
DO:
  IF INPUT CB-Rapport = 1 THEN
    ASSIGN
    FI-TilDato:SCREEN-VALUE = FI-FraDato:SCREEN-VALUE
    .
  ELSE
  ASSIGN
  .


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
  &PostDisable_ui = "for each tmpChild:
                       if valid-handle(tmpChild.wChild) then
                         delete procedure tmpChild.wChild.
                     end."
}


/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

RUN InitVariabler.

{syspara.i 1 4   1 wExcEkstent}
wExcEkstent = IF wExcEkstent = "" THEN "sdv" ELSE wExcEkstent.   
{syspara.i 1 1 100 wKunde}
{syspara.i 1 1 101 wSkoTex}
{syspara.i 5 1 1 iCl INT}
{syspara.i 1 1 18 cHKInst}

/* HotKeySøk - DYYYYRT */
ON ALT-M OF FRAME DEFAULT-FRAME ANYWHERE 
  DO:
    APPLY "CHOOSE":U TO B-Butikker IN FRAME DEFAULT-FRAME.
  END.
ON ALT-P OF FRAME DEFAULT-FRAME ANYWHERE 
    DO:
      APPLY "CHOOSE":U TO B-XPrint IN FRAME DEFAULT-FRAME.
    END.
/* Centrallager */
FIND clButiker NO-LOCK WHERE
    clButiker.Butik = iCl.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
/*     fi-butiker = FI-BUTIKER + ",2". */
  RUN enable_UI.
  {lng.i} 

  ASSIGN
      CB-Rapport = 1
      .
  DISPLAY
      CB-Rapport
      WITH FRAME {&FRAME-NAME}.
  APPLY "VALUE-CHANGED" TO CB-Rapport.

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AutoInit C-Win 
PROCEDURE AutoInit :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  iRapptype: 0 och 10 = finans,  0=preview 10 = direkt
                          1 och 11 = bokfbil, 1=preview 11 = direkt
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iRapptype        AS INTEGER    NO-UNDO. /* 0,10=finans,1,11=bokfb.*/
    DEFINE INPUT  PARAMETER iButik   LIKE Butiker.Butik NO-UNDO.
    DEFINE INPUT  PARAMETER dRappDat AS DATE            NO-UNDO.

    DEF VAR lBatch AS LOG NO-UNDO.

    cButBatchPrinter = "".
    IF CAN-DO("10,11",STRING(iRapptype)) THEN DO:
        FIND Butiker WHERE Butiker.Butik = iButik NO-LOCK NO-ERROR.
        IF AVAIL Butiker AND TRIM(Butiker.RAPPrinter) <> "" AND (SESSION:GET-PRINTERS() MATCHES '*' + Butiker.RAPPrinter + '*') THEN
            ASSIGN cButBatchPrinter = TRIM(Butiker.RAPPrinter).
        lBatch = TRUE.
    END.
    ELSE IF CAN-DO("20,21",STRING(iRapptype)) THEN
        iRappType = iRappType - 10.

    DO WITH FRAME {&FRAME-NAME}:
        C-Win:HIDDEN = TRUE.
        ASSIGN lDirekte = (iRappType    = 10 OR iRappType    = 11)
               iRappType = IF iRappType < 2 THEN iRappType ELSE iRappType - 10
               CB-Rapport:SCREEN-VALUE = STRING(iRapptype)
               FI-Butiker:SCREEN-VALUE = STRING(iButik)
               FI-Butiker              = STRING(iButik)
               FI-FraDato:SCREEN-VALUE = STRING(dRappDat)
               FI-TilDato:SCREEN-VALUE = STRING(dRappDat)
               FI-FraDato = dRappDat
               FI-TilDato = dRappDat
               T-Kuntotal = iRappType = 1
               T-Kuntotal:CHECKED = iRappType = 1 OR lBatch. /* Vi skall bara ha ut tot vid batch */
    END.
    
    IF lBatch AND  cButBatchPrinter <> "" THEN
        APPLY "CHOOSE" TO B-XPrint.
    ELSE IF NOT lBatch THEN
        APPLY "CHOOSE" TO B-XPrint.
    APPLY "CLOSE":U TO THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Bank C-Win 
PROCEDURE Bank :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE pcOverskr AS CHARACTER  NO-UNDO.
DEFINE VARIABLE pcLabel AS CHARACTER  NO-UNDO.
ASSIGN pcOverskr = "Bank" + CHR(1) + "Antall" + chr(1) + "Beløp".
ASSIGN pcLabel   = "Solgt bank" + CHR(1) + "Utbetalt bank" + CHR(1) + "Totalt bank" + CHR(1) + "Reserveløsning".
    
    PUT UNFORMATTED 
        "<P12><R31><C6><B>"  ENTRY(1,pcOverskr,CHR(1)) SKIP "</B><C6><FROM><C35><LINE>" SKIP
        "<P8><C16><RIGHT=C+6>"  ENTRY(2,pcOverskr,CHR(1))
        "<C24><RIGHT=C+11>"  ENTRY(3,pcOverskr,CHR(1)) SKIP
        "<C16><FROM><C22><LINE>" "<C24><FROM><C35><LINE>"
        "<C6>" +  entry(1,pcLabel,CHR(1)) +
          "<C16><RIGHT=C+6>" + STRING(tmpKas_Rap.AntBank,"->>,>>9")    +
          "<C24><RIGHT=C+11>" + STRING(tmpKas_Rap.Bank,"->>>,>>>,>>9.99") SKIP        
        "<C5>+<C6>" +  entry(2,pcLabel,CHR(1)) +
          "<C16><RIGHT=C+6>" + STRING(tmpKas_Rap.AntCashback,"->>,>>9")    +
          "<C24><RIGHT=C+11>" + STRING(tmpKas_Rap.Cashback,"->>>,>>>,>>9.99") SKIP
        "<C5>+<C6>" +  entry(4,pcLabel,CHR(1)) +
          "<C16><RIGHT=C+6>" + STRING(tmpKas_Rap.AntReservelosning,"->>,>>9")    +
          "<C24><RIGHT=C+11>" + STRING(tmpKas_Rap.Reservelosning,"->>>,>>>,>>9.99") SKIP
        .
/*     RUN KortSpes (1). */
    RUN KortSpesFinans.

    PUT UNFORMATTED
        "<C6><FROM><C35><LINE>"
        "<C5>=<C6>" +  entry(3,pcLabel,CHR(1))
        "<C24><RIGHT=C+11>" + STRING(Bank + Cashback + Reservelosning + lKortSum,"->>>,>>>,>>9.99") SKIP
        "<C6><FROM><C35><LINE>" SKIP.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Beholdning C-Win 
PROCEDURE Beholdning :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE pcOverskr AS CHARACTER  NO-UNDO.
DEFINE VARIABLE pcLabel AS CHARACTER  NO-UNDO.
ASSIGN pcOverskr = "Beholdning" + CHR(1) + "Antall" + CHR(1) + "Beløp".
ASSIGN pcLabel   = "Kontantbeholdning" + CHR(1) + "Vekselbeholdning" + CHR(1) + "Sjekkbeholdning" + CHR(1) + "Reserveløsing bank" + CHR(1) + 
                   "Rekvisisjon".
    
    PUT UNFORMATTED 
        "<P12><R54><C43><B>"  ENTRY(1,pcOverskr,CHR(1)) SKIP "</B><C43><FROM><C75><LINE>" SKIP
        "<P8><C56><RIGHT=C+6>"  ENTRY(2,pcOverskr,CHR(1))
        "<C64><RIGHT=C+11>"  ENTRY(3,pcOverskr,CHR(1)) SKIP
        "<C56><FROM><C62><LINE>" "<C64><FROM><C75><LINE>" 
        "<C43>" +  entry(1,pcLabel,CHR(1)) + "<C64><RIGHT=C+11>" + STRING(tmpKas_Rap.KontantBeholdning,"->>>,>>>,>>9.99") SKIP        
        "<C43>" +  entry(2,pcLabel,CHR(1)) + "<C64><RIGHT=C+11>" + STRING(tmpKas_Rap.VekselBeholdning,"->>>,>>>,>>9.99") SKIP
        "<C43>" +  entry(3,pcLabel,CHR(1)) + "<C56><RIGHT=C+6>" + STRING(tmpKas_Rap.AntSjekk,"->>,>>9") + "<C64><RIGHT=C+11>" + STRING(tmpKas_Rap.SjekkBeholdning,"->>>,>>>,>>9.99") SKIP
        "<C43>" +  entry(4,pcLabel,CHR(1)) + "<C56><RIGHT=C+6>" + STRING(tmpKas_Rap.AntReservelosning,"->>,>>9") + "<C64><RIGHT=C+11>" + STRING(tmpKas_Rap.Reservelosning,"->>>,>>>,>>9.99") SKIP
        "<C43>" +  entry(5,pcLabel,CHR(1)) + "<C56><RIGHT=C+6>" + STRING(tmpKas_Rap.AntRekvisisjon,"->>,>>9") + "<C64><RIGHT=C+11>" + STRING(tmpKas_Rap.Rekvisisasjon,"->>>,>>>,>>9.99") SKIP
        "<C43><FROM><C75><LINE>" SKIP.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Bilag1 C-Win 
PROCEDURE Bilag1 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE pcOverskr AS CHARACTER  NO-UNDO.
DEFINE VARIABLE pcLabel   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE pcKonto   AS CHARACTER  NO-UNDO.
DEFINE VAR      piLoop    AS INT        NO-UNDO.
DEFINE VAR      pcBank    AS CHAR       NO-UNDO.
DEFINE VAR      piInt     AS INT        NO-UNDO.
DEFINE VAR      pcTekst   AS CHAR       NO-UNDO.

ASSIGN
  pcOverskr = "Tekst" + CHR(1) + 
              "Konto" + CHR(1) + 
              "Beløp" + CHR(1) +
              "Mva"   + CHR(1) +
              "Beløp u/mva"
  pcLabel   = "Varesalg" + CHR(1) + 
              "Betalt med:" + CHR(1) + 
              "Bankkort" + CHR(1) + 
              "Reserveløsning"
  pcKonto   = "K 0000  0%" + CHR(1) +
              "K 3000 24%" + chr(1) +
              "K 3001 12%" + chr(1) +
              "K 0000  0%" + chr(1) +
              "K 0000  0%" + chr(1) +
              "K 0000  0%" + chr(1) +
              "K 0000  0%" + chr(1) +
              "K 0000  0%" + chr(1) +
              "K 0000  0%" + chr(1) +
              "K 0000  0%"
  pcBank    = "D 2380"
  .

    PUT UNFORMATTED 
        "<P8><R10><C6><B>"  ENTRY(1,pcOverskr,CHR(1)) 
        "<P8><C30>"  ENTRY(2,pcOverskr,CHR(1))
        "<C40><RIGHT=C+11>"  ENTRY(3,pcOverskr,CHR(1)) 
        "<C53><RIGHT=C+11>"  ENTRY(4,pcOverskr,CHR(1)) 
        "<C66><RIGHT=C+11>"  ENTRY(5,pcOverskr,CHR(1)) 
        SKIP  
      
        "<C6><FROM><C28><LINE>" 
        "<C30><FROM><C38><LINE>" 
        "<C40><FROM><C51><LINE>" 
        "<C53><FROM><C64><LINE>" 
        "<C66><FROM><C78><LINE>" 
        "</B>"
        SKIP.

/*         "</B>"                                                                */
/*         "<C6>" +  entry(1,pcLabel,chr(1))                                     */
/*         "<C30>" + entry(1,pcKonto,chr(1))                                     */
/*         "<C40><RIGHT=C+11>" + STRING(wBruttoOmsetning /* +                    */
/*                                      (tmpKas_Rap.Retur * -1) -                */
/*                                      tmpKas_Rap.GenerellRabatt -              */
/*                                      tmpKas_Rap.Kunderabatt -                 */
/*                                      tmpKas_Rap.Personalrabatt -              */
/*                                      tmpKas_Rap.Medlemsrabatt -               */
/*                                      tmpKas_Rap.Pakkerabatt -                 */
/*                                      tmpKas_Rap.Avrunding */,"->>>,>>>,>>9.99") */
/*         "<C52>+" SKIP.                                                        */
    /* Legger ut mva regnskapet. */
    DO piLoop = 1 TO 10:
        IF tmpKas_rap.MvaGrunnlag[piLoop] <> 0 THEN
        DO:
            ASSIGN
                piInt   = tmpKas_Rap.MvaGrp[piLoop] + 1
                pcTekst = ENTRY(piLoop,pcKonto,CHR(1))
                .
            IF tmpKas_rap.MvaGrunnlag[piLoop] > 0 THEN
                {syspara.i 20 1 piInt pcTekst}
            ELSE
                {syspar2.i 20 1 piInt pcTekst}

            FIND Moms NO-LOCK WHERE
                    Moms.MomsKod = tmpKas_rap.MvaGrp[piLoop] NO-ERROR.
            /*
            IF AVAILABLE Moms THEN
                pcTekst = pcTekst + " " + STRING(Moms.MomsProc) + "%".
            ELSE 
                pcTekst = pcTekst + " " + string(ROUND((tmpKas_rap.MvaBelop[piLoop] / tmpKas_rap.MvaGrunnlag[piLoop]) * 100,0)) + "%".
            */
            pcTekst = pcTekst + " " + string(ROUND((tmpKas_rap.MvaBelop[piLoop] / tmpKas_rap.MvaGrunnlag[piLoop]) * 100,0)) + "%".

            PUT UNFORMATTED 
            "<C6>" +  entry(1,pcLabel,CHR(1))
            "<C30>" pcTekst  
                                /*STRING(tmpKas_rap.MvaGrp[piLoop],">9")*/
            "<C40><RIGHT=C+11>" STRING(tmpKas_rap.MvaGrunnlag[piLoop] + 
                                      tmpKas_rap.MvaBelop[piLoop],"->>>,>>>,>>9.99")
            "<C52>+" 
            "<C53><RIGHT=C+11>" STRING(tmpKas_rap.MvaBelop[piLoop],"->>>,>>>,>>9.99")
            "<C66><RIGHT=C+11>" STRING(tmpKas_rap.MvaGrunnlag[piLoop],"->>>,>>>,>>9.99")
             SKIP.
        END.
    END.

    IF Bank + Cashback > 0 THEN
        {syspara.i 20 2 1 pcBank}
    ELSE
        {syspar2.i 20 2 1 pcBank}

    PUT UNFORMATTED
        SKIP(1)
        "<B>"
        "<C6>" +  entry(2,pcLabel,CHR(1)) SKIP
        "<C6><FROM><C28><LINE>" 
        "</B>"
        SKIP.

    IF (Bank + CashBack) <> 0 THEN
        PUT UNFORMATTED
        "<C6>" +  entry(3,pcLabel,CHR(1))
        "<C30>" + pcBank
        "<C40><RIGHT=C+11>" + STRING(Bank + Cashback,"->>>,>>>,>>9.99") + "<C52>-" SKIP
      .
    
    {syspara.i 20 2 13 pcBank}
    
    IF (Reservelosning) <> 0 THEN
    PUT UNFORMATTED
        "<C6>" +  entry(4,pcLabel,CHR(1))
        "<C30>" + pcBank /* bytt ut med kontonr for reserveløsning */
        "<C40><RIGHT=C+11>" + STRING(Reservelosning,"->>>,>>>,>>9.99") + "<C52>-" SKIP
      .


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Bilag2 C-Win 
PROCEDURE Bilag2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE pcOverskr AS CHARACTER  NO-UNDO.
DEFINE VARIABLE pcLabel   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE plSum     AS DEC        NO-UNDO.
DEFINE VARIABLE pcTekst   AS CHAR       NO-UNDO.
DEFINE VARIABLE piLoop    AS INT        NO-UNDO.

  ASSIGN 
    pcLabel   = /*  1 */ "Tilgodesedler, egne" + CHR(1) + 
                /*  2 */ "Gavekort, egne" + CHR(1) + 
                /*  3 */ "Gavekort, andre" + CHR(1) + 
                /*  4 */ "Utstedt:" + CHR(1) + 
                /*  5 */ "Tilgodesedler, ut" + CHR(1) + 
                /*  6 */ "Gavekort, ut" + CHR(1) + 
                /*  7 */ "Kredittsalg" + CHR(1) + 
                /*  8 */ "Fakturert" + CHR(1) + 
                /*  9 */ "Innbetalt kontant" + CHR(1) + 
                /* 10 */ "Utbetalinger:" + CHR(1) + 
                /* 11 */ "Utbetalt" + CHR(1) + 
                /* 12 */ "Dagens kontantstrøm" + CHR(1) +
                /* 13 */ "Kasse ved dagens begynnelse" + CHR(1) + 
                /* 14 */ "Innskudd bank" + CHR(1) + 
                /* 15 */ "Kasse ved dagens slutt, opptalt" + CHR(1) + 
                /* 16 */ "Endring kasse" + CHR(1) + 
                /* 17 */ "Differanse" + CHR(1) + 
                /* 18 */ "Kasseoppgjøret er utført av:" + CHR(1) +
                /* 19 */ "Cashback" + CHR(1) + 
                /* 20 */ "Tilgodesedler, andre" + CHR(1) + 
                /* 21 */ "Kupong1" + CHR(1) + 
                /* 22 */ "Kupong2" + CHR(1) + 
                /* 23 */ "Deponering" + CHR(1) + 
                /* 24 */ "Deponering, ut" + CHR(1) +
                /* 25 */ "Dropp," + CHR(1) +
                /* 26 */ "Innbetalt" + 
      FILL(CHR(1),50)
    .
  /* Overstyrer default */
  DO piLoop = 13 TO 50:
      {syspara.i 20  4 piLoop pcTekst}
    IF pcTekst <> "" THEN
    ASSIGN
        ENTRY(piLoop - 12,pcLabel,CHR(1)) = pcTekst
        pcTekst = ""
        .
  END.

  IF tmpKas_Rap.TilgodeInn >= 0 THEN {syspara.i 20 2 2 pcTekst} ELSE {syspar2.i 20 2 2 pcTekst}
  PUT UNFORMATTED 
    "</B>"
    /* Tilgodesedler egne */
    "<C6>" +  entry(1,pcLabel,CHR(1))
    "<C30>" + pcTekst /*entry(1,pcKonto,chr(1))*/
    "<C40><RIGHT=C+11>" + STRING(tmpKas_Rap.TilgodeInn /* - tmpKas_Rap.TilgodeAndre */,"->>>,>>>,>>9.99") + "<C52>-" SKIP.
  IF tmpKas_Rap.TilgodeAndre >= 0 THEN {syspara.i 20 2 3 pcTekst} ELSE {syspar2.i 20 2 3 pcTekst}
  PUT UNFORMATTED 
    /* Tilgodesedler andre */
    "<C6>" +  entry(20,pcLabel,CHR(1))
    "<C30>" + pcTekst /*entry(20,pcKonto,chr(1))*/
    "<C40><RIGHT=C+11>" + STRING(tmpKas_Rap.TilgodeAndre,"->>>,>>>,>>9.99") + "<C52>-" SKIP.
  IF tmpKas_Rap.GavekortInn >= 0 THEN {syspara.i 20 2 4 pcTekst} ELSE {syspar2.i 20 2 4 pcTekst}
  PUT UNFORMATTED 
    /* Gavekort, egne */
    "<C6>" +  entry(2,pcLabel,CHR(1))
    "<C30>" + pcTekst /*entry(2,pcKonto,chr(1))*/
    "<C40><RIGHT=C+11>" + STRING(tmpKas_Rap.GavekortInn - tmpKas_Rap.GavekortAndreInn,"->>>,>>>,>>9.99") + "<C52>-" SKIP.
  IF 0 >= 0 THEN {syspara.i 20 2 5 pcTekst} ELSE {syspar2.i 20 2 5 pcTekst}
  PUT UNFORMATTED 
    /* Gavekort, andre */
    "<C6>" +  entry(3,pcLabel,CHR(1))
    "<C30>" + pcTekst /*entry(3,pcKonto,chr(1))*/
    "<C40><RIGHT=C+11>" + STRING(tmpKas_Rap.GavekortAndreInn,"->>>,>>>,>>9.99") + "<C52>-" SKIP.

  IF 0 >= 0 THEN {syspara.i 20 2 10 pcTekst} ELSE {syspar2.i 20 2 10 pcTekst}
  PUT UNFORMATTED 
    /* Depositum */
    "<C6>" +  entry(23,pcLabel,CHR(1))
    "<C30>" + pcTekst /*entry(3,pcKonto,chr(1))*/
    "<C40><RIGHT=C+11>" + STRING(tmpKas_Rap.Layaway_Inn,"->>>,>>>,>>9.99") + "<C52>-" SKIP.

  IF tmpKas_Rap.Kupong1 <> 0 THEN
  DO:
      IF tmpKas_Rap.Kupong1 >= 0 THEN {syspara.i 20 2 6 pcTekst} ELSE {syspar2.i 20 2 6 pcTekst}
      PUT UNFORMATTED 
        /* Kupong1 */
        "<C6>" +  entry(21,pcLabel,CHR(1))
        "<C30>" + pcTekst /*entry(2,pcKonto,chr(1))*/
        "<C40><RIGHT=C+11>" + STRING(tmpKas_Rap.Kupong1,"->>>,>>>,>>9.99") + "<C52>-" SKIP.
  END.

  IF tmpKas_Rap.Kupong2 <> 0 THEN
  DO:
      IF tmpKas_Rap.Kupong2 >= 0 THEN {syspara.i 20 2 8 pcTekst} ELSE {syspar2.i 20 2 8 pcTekst}
      PUT UNFORMATTED 
        /* Kupong2 */
        "<C6>" +  entry(22,pcLabel,CHR(1))
        "<C30>" + pcTekst /*entry(2,pcKonto,chr(1))*/
        "<C40><RIGHT=C+11>" + STRING(tmpKas_Rap.Kupong2,"->>>,>>>,>>9.99") + "<C52>-" SKIP.
  END.

  PUT UNFORMATTED 
    /* Overskrift */
    " " SKIP
    "<B>"
    "<C6>" +  entry(4,pcLabel,CHR(1)) SKIP
    "<C6><FROM><C28><LINE>" SKIP
    "</B>".

/*     /* CashBack ut */                                                                 */
/*     "<C6>" +  entry(19,pcLabel,chr(1))                                                */
/*     "<C30>" + entry(18,pcKonto,chr(1))                                                */
/*     "<C40><RIGHT=C+11>" + STRING(tmpKas_Rap.CashBack,"->>>,>>>,>>9.99") + "<C52>-" SKIP */

  IF tmpKas_Rap.TilgodeUt >= 0 THEN {syspara.i 20 2 20 pcTekst} ELSE {syspar2.i 20 2 20 pcTekst}
  PUT UNFORMATTED 
    /* Tilgodesedler ut */
    "<C6>" +  entry(5,pcLabel,CHR(1))
    "<C30>" + pcTekst /*entry(5,pcKonto,chr(1))*/
    "<C40><RIGHT=C+11>" + STRING(tmpKas_Rap.TilgodeUt,"->>>,>>>,>>9.99") + "<C52>+" SKIP.

  IF tmpKas_Rap.GavekortUt - tmpKas_Rap.GavekortRabatt >= 0 THEN {syspara.i 20 2 21 pcTekst} ELSE {syspar2.i 20 2 21 pcTekst}
  PUT UNFORMATTED 
    /* Gavekort ut */
    "<C6>" +  entry( 6,pcLabel,CHR(1))
    "<C30>" + pcTekst /*entry(6,pcKonto,chr(1))*/
    "<C40><RIGHT=C+11>" + STRING(tmpKas_Rap.GavekortUt - tmpKas_Rap.GavekortRabatt,"->>>,>>>,>>9.99") + "<C52>+" SKIP.

  RUN NonSale1.

  IF tmpKas_Rap.Layaway_Ut >= 0 THEN {syspara.i 20 2 11 pcTekst} ELSE {syspar2.i 20 2 11 pcTekst}
  PUT UNFORMATTED 
    /* Depositum ut */
    "<C6>" +  entry(24,pcLabel,CHR(1))
    "<C30>" + pcTekst /*entry(6,pcKonto,chr(1))*/
    "<C40><RIGHT=C+11>" + STRING(tmpKas_Rap.Layaway_Ut,"->>>,>>>,>>9.99") + "<C52>+" SKIP(1).
  PUT UNFORMATTED 
    /* Overskrift */
    "<B>"
    "<C6>" +  entry(7,pcLabel,CHR(1)) SKIP
    "<C6><FROM><C28><LINE>" SKIP
    "</B>".
  IF tmpKas_Rap.Kredit >= 0 THEN {syspara.i 20 2 40 pcTekst} ELSE {syspar2.i 20 2 40 pcTekst}
  PUT UNFORMATTED 
    /* Fakturert */
    "<C6>" +  entry(8,pcLabel,CHR(1))
    "<C30>" + pcTekst /*entry(8,pcKonto,chr(1))*/
    "<C40><RIGHT=C+11>" + STRING(tmpKas_Rap.Kredit,"->>>,>>>,>>9.99") + "<C52>-" SKIP.
  IF tmpKas_Rap.InnbetaltKunde <> 0 THEN {syspara.i 20 2 41 pcTekst} ELSE {syspar2.i 20 2 41 pcTekst}
  PUT UNFORMATTED 
    /* Innbetalt konto */
    "<C6>" +  entry(9,pcLabel,CHR(1))
    "<C30>" + pcTekst /*entry(9,pcKonto,chr(1))*/
    "<C40><RIGHT=C+11>" + STRING(tmpKas_Rap.InnbetaltKunde,"->>>,>>>,>>9.99") + "<C52>+" SKIP(1).
  PUT UNFORMATTED 
    /* Overskrift */
    "<B>"
    "<C6>" +  entry(10,pcLabel,CHR(1)) SKIP
    "<C6><FROM><C28><LINE>" SKIP
    "</B>".
  IF tmpKas_Rap.kont_in  >= 0 THEN {syspara.i 20 2 59 pcTekst} ELSE {syspar2.i 20 2 59 pcTekst}
  PUT UNFORMATTED 
    /* Innbetalt */
    "<C6>" +  entry(26,pcLabel,CHR(1))
    "<C30>" + pcTekst /*entry(11,pcKonto,chr(1))*/
    "<C40><RIGHT=C+11>" + STRING(tmpKas_Rap.kont_in,"->>>,>>>,>>9.99") + "<C52>+" SKIP
    .
  IF tmpKas_Rap.kont_ut  <> 0 THEN {syspara.i 20 2 60 pcTekst} ELSE {syspar2.i 20 2 60 pcTekst}
  PUT UNFORMATTED 
    /* Utbetalt */
    "<C6>" +  entry(11,pcLabel,CHR(1))
    "<C30>" + pcTekst /*entry(11,pcKonto,chr(1))*/
    "<C40><RIGHT=C+11>" + STRING(tmpKas_Rap.kont_ut,"->>>,>>>,>>9.99") + "<C52>-" SKIP
    .
  
  RUN NonSale2.

  IF tmpKas_Rap.Dropp > 0 THEN {syspara.i 20 2 61 pcTekst} ELSE {syspar2.i 20 2 61 pcTekst}
  PUT UNFORMATTED 
    /* Dropp */
    "<C6>" +  entry(25,pcLabel,CHR(1))
    "<C30>" + pcTekst /*entry(11,pcKonto,chr(1))*/
    "<C40><RIGHT=C+11>" + STRING(tmpKas_Rap.Dropp,"->>>,>>>,>>9.99") + "<C52>-" SKIP
    "<C40><FROM><C51><LINE>" SKIP.
  PUT UNFORMATTED 
    /* Dagens kontantstrøm */
    "<B>"
    "<C6>" +  entry(12,pcLabel,CHR(1))
    "<C30>" /*+ entry(12,pcKonto,chr(1))*/
    "<C40><RIGHT=C+11>" + STRING(lDagensKontStrom,"->>>,>>>,>>9.99")  + "<C52>A"SKIP(1)
    "</B>".
  PUT UNFORMATTED 
    /* Kasse ved dagens start */
    "<C6>" +  entry(13,pcLabel,CHR(1))
    "<C30>" /*+ entry(13,pcKonto,chr(1))*/
    "<C40><RIGHT=C+11>" + STRING(tmpKas_Rap.OpptaltInnVeksel,"->>>,>>>,>>9.99") + "<C52>B" SKIP.
/*     /* Levert bank */                                                                          */
/*     "<C6>" +  entry(14,pcLabel,chr(1))                                                         */
/*     "<C30>" + entry(14,pcKonto,chr(1))                                                         */
/*     "<C40><RIGHT=C+11>" + STRING(tmpKas_Rap.OpptaltLevertBank,"->>>,>>>,>>9.99") + "<C52>C" SKIP */
  PUT UNFORMATTED 
    /* Kasse ved dagens slutt */
    "<C6>" +  entry(15,pcLabel,CHR(1))
    "<C30>" /*+ entry(15,pcKonto,chr(1))*/
    "<C40><RIGHT=C+11>" + STRING(lKasseSlutt,"->>>,>>>,>>9.99") + "<C52>C" SKIP
    "<C40><FROM><C51><LINE>" SKIP.
  IF lKasseEndring >= 0 THEN {syspara.i 20 2 70 pcTekst} ELSE {syspar2.i 20 2 70 pcTekst}
  PUT UNFORMATTED 
    /* Endring kasse */
    "<B>"
    "<C6>"  +  entry(16,pcLabel,CHR(1))
    "<C30>" + pcTekst
    "<C40><RIGHT=C+11>" + STRING(lKasseEndring,"->>>,>>>,>>9.99") + "<C52>D=C-B" SKIP(1)
    "</B>".
  IF lKasseDiff >= 0 THEN {syspara.i 20 2 90 pcTekst} ELSE {syspar2.i 20 2 90 pcTekst}
  PUT UNFORMATTED 
    /* Differanse */
    "<B>"
    "<C6>"  + entry(17,pcLabel,CHR(1))
    "<C30>" + pcTekst
    "<C40><RIGHT=C+11>" + STRING(lKasseDiff,"->>>,>>>,>>9.99") + "<C52>=D-A" SKIP(6)
    "</B>".
  IF FI-FraDato = FI-TilDato THEN
      PUT UNFORMATTED 
        /* Kvittering */
        "<B><R60>"
        "<C6>" + entry(18,pcLabel,CHR(1))
    /*        SKIP(1)                    */
    /*     "<C23><FROM><C60><LINE>" SKIP */
        "</B>"
        .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BilagSpes C-Win 
PROCEDURE BilagSpes :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:     

            "<C53><RIGHT=C+11>" STRING(tmpKas_rap.MvaBelop[piLoop],"->>>,>>>,>>9.99")
            "<C67><RIGHT=C+11>" STRING(tmpKas_rap.MvaGrunnlag[piLoop],"->>>,>>>,>>9.99")
             SKIP.
    END.

    PUT UNFORMATTED
        SKIP(1)
        "<B>"
        "<C6>" +  entry(2,pcLabel,chr(1)) SKIP
        "<C6><FROM><C28><LINE>" SKIP
        "</B>"
        "<C6>" +  entry(3,pcLabel,chr(1))
        "<C30>" + pcBank
        "<C40><RIGHT=C+11>" + STRING(Bank + Cashback,"->>>,>>>,>>9.99") + "<C52>-" SKIP
  
------------------------------------------------------------------------------*/
DEFINE VARIABLE pcOverskr AS CHARACTER  NO-UNDO.
DEFINE VARIABLE pcLabel   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE pcKonto   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE plSum     AS DEC        NO-UNDO.
DEFINE VARIABLE lKun1Dag  AS LOGICAL    NO-UNDO.
DO WITH FRAME {&FRAME-NAME}:
    IF INPUT FI-FraDato = INPUT FI-TilDato THEN
        ASSIGN lKun1Dag = TRUE.
    ASSIGN 
      pcLabel   = /*  1 */ "Nr"    + CHR(1) + 
                  /*  2 */ "Tekst" + CHR(1) + 
                  /*  3 */ "Beløp" + CHR(1) + 
                  /*  4 */ "Bilag" + CHR(1) +
                  /*  5 */ "Kasserer:" + CHR(1) +
                  /*  6 */ "Bankposer" + CHR(1) +
                  /*  7 */ "PoseNr" + CHR(1) +
                  /*  8 */ "Beløp"
      .
      PUT UNFORMATTED 
          "<R18>".

      IF CAN-FIND(FIRST KassererBilag WHERE
                        KassererBilag.Butikk   = INT(FI-Butiker) AND
                        KassererBilag.Dato     >= INPUT FI-FraDato AND
                        KassererBilag.Dato     <= INPUT FI-TilDato AND
                        KassererBilag.Belop   <> 0) THEN DO:
          IF lKun1Dag THEN
              PUT UNFORMATTED 
                  "<B>"
                  "<C53><P10>" + entry(4,pcLabel,CHR(1)) SKIP
                  "<C53><P8><RIGHT=C+3>"  + entry(1,pcLabel,CHR(1))
                  "<C57>" + entry(2,pcLabel,CHR(1))
                  "<C68><RIGHT=C+10>" + entry(3,pcLabel,CHR(1)) SKIP
                  "<C53><FROM><C56><LINE>"
                  "<C57><FROM><C67><LINE>"
                  "<C68><FROM><C78><LINE>" SKIP
                  .
          ELSE
              PUT UNFORMATTED 
                  "<B>"
                  "<C53><P10>" + entry(4,pcLabel,CHR(1)) SKIP
                  "<C53><P8><RIGHT=C+3>"  + "" /* entry(1,pcLabel,chr(1)) */
                  "<C57>" + ""                 /* entry(2,pcLabel,chr(1)) */
                  "<C68><RIGHT=C+10>" + entry(3,pcLabel,CHR(1)) SKIP
/*                   "<C53><FROM><C56><LINE>" */
/*                   "<C57><FROM><C67><LINE>" */
                  "<C68><FROM><C78><LINE>" SKIP
                  .
      END.

      /* Leser alle kortspesifikasjoner */
      BILAG:
      FOR EACH KassererBilag NO-LOCK WHERE
    /*     KassererBilag.Butikk   = tmpKas_Rap.Butikk AND */
    /*     KassererBilag.Dato     = tmpKas_Rap.Dato AND   */
        KassererBilag.Butikk    = INT(FI-Butiker)  AND
        KassererBilag.Dato     >= INPUT FI-FraDato AND
        KassererBilag.Dato     <= INPUT FI-TilDato AND
        KassererBilag.Belop    <> 0
        BREAK BY KassererBilag.KassererNr
              BY KassererBilag.BilagsNr
              BY KassererBilag.z_nummer:
        FIND Forsalj NO-LOCK WHERE
            Forsalj.ForsNr = KassererBilag.KassererNr NO-ERROR.

        IF lKun1Dag AND FIRST-OF(KassererBilag.KassererNr) THEN
            PUT UNFORMATTED
            "<B>"
            "<C53>" + entry(5,pcLabel,CHR(1))
                    + string(KassererBilag.KassererNr,">>>>>9") + " "
                    + (IF AVAILABLE Forsalj
                        THEN Forsalj.FoNamn
                        ELSE "*Ukjent")
            "</B>" SKIP.

        /* Sum bilag */
        ASSIGN
          plSum = plSum + KassererBilag.Belop
          .
        IF lKun1Dag THEN
            PUT UNFORMATTED 
                "<C53><RIGHT=C+3>" +  string(KassererBilag.BilagsNr,">>9")
                "<C57>" + substring(KassererBilag.Meknad,1,15)
                "<C68><RIGHT=C+10>" + STRING(KassererBilag.Belop,"->>,>>9.99")
                SKIP
              .
        IF lKun1Dag AND LAST-OF(KassererBilag.KassererNr) THEN
        DO:
            PUT UNFORMATTED
            "<B>"
            "<C68><FROM><C+10><LINE>" SKIP
            "<C68><RIGHT=C+10>" + STRING(plSum,"->>,>>9.99")
            "</B>" SKIP.

            ASSIGN
                plSum = 0
                .
        END.
        ELSE IF NOT lKun1Dag AND LAST(KassererBilag.z_nummer) THEN
            PUT UNFORMATTED
            "<B>"
/*             "<C68><FROM><C+10><LINE>" SKIP */
            "<C68><RIGHT=C+10>" + STRING(plSum,"->>,>>9.99")
            "</B>" SKIP.
      END. /* BILAG */

      IF CAN-FIND(FIRST KassererOppgj WHERE
                  KassererOppgj.Butikk   = INT(FI-Butiker) AND
                  KassererOppgj.Dato     >= INPUT FI-FraDato AND
                  KassererOppgj.Dato     <= INPUT FI-TilDato AND
                        KassererOppgj.OpptaltLevertBank <> 0) THEN 
          IF lKun1Dag THEN
              PUT UNFORMATTED 
                  "<B>"
                  "<C53><P10>" + entry(6,pcLabel,CHR(1)) SKIP
                  "<C53><P8>"  + entry(7,pcLabel,CHR(1))
                  "<C68><RIGHT=C+10>" + entry(8,pcLabel,CHR(1)) SKIP
                  "<C53><FROM><C66><LINE>"
                  "<C68><FROM><C78><LINE>" SKIP
                  "</B>"
                  .
          ELSE
              PUT UNFORMATTED 
                  "<B>"
                  "<C53><P10>" + entry(6,pcLabel,CHR(1)) SKIP
                  "<C53><P8>"  + ""
                  "<C68><RIGHT=C+10>" + entry(8,pcLabel,CHR(1)) SKIP
/*                   "<C53><FROM><C66><LINE>" */
                  "<C68><FROM><C78><LINE>" SKIP
                  "</B>"
                  .
      /* Leser alle bankposer */
      ASSIGN plSum = 0.
      BILAG:
      FOR EACH KassererOppgj NO-LOCK WHERE
          KassererOppgj.Butikk    = INT(FI-Butiker) AND
          KassererOppgj.Dato     >= INPUT FI-FraDato AND
          KassererOppgj.Dato     <= INPUT FI-TilDato AND
          KassererOppgj.OpptaltLevertBank   <> 0
        BREAK 
              BY KassererOppgj.Butikk
/*               BY KassererOppgj.Dato   */
              BY KassererOppgj.KassererNr:
/*               BY KassererOppgj.z_nummer: */
        FIND Forsalj NO-LOCK WHERE
            Forsalj.ForsNr = KassererBilag.KassererNr NO-ERROR.

    /*     IF FIRST-OF(KassererOppgj.KassererNr) THEN                    */
    /*         PUT UNFORMATTED                                           */
    /*         "<B>"                                                     */
    /*         "<C53>" + entry(5,pcLabel,chr(1))                         */
    /*                 + string(KassererOppgj.KassererNr,">>>>>9") + " " */
    /*                 + (IF AVAILABLE Forsalj                           */
    /*                     THEN Forsalj.FoNamn                           */
    /*                     ELSE "*Ukjent")                               */
    /*         "</B>" SKIP.                                              */

        /* Sum bilag */
        ASSIGN
          plSum = plSum + KassererOppgj.OpptaltLevertBank
          .
        IF lKun1Dag THEN
            PUT UNFORMATTED 
                "<C53>" + substring(KassererOppgj.PoseNr,1,15)
                "<C68><RIGHT=C+10>" + STRING(KassererOppgj.OpptaltLevertBank,"->>,>>9.99")
                SKIP
              .
        IF lKun1Dag AND LAST-OF(KassererOppgj.Butikk) THEN
        DO:
            PUT UNFORMATTED
            "<B>"
            "<C68><FROM><C+10><LINE>" SKIP
            "<C68><RIGHT=C+10>" + STRING(plSum,"->>,>>9.99")
            "</B>" SKIP.

            ASSIGN
                plSum = 0
                .
        END.
        IF NOT lKun1Dag AND LAST-OF(KassererOppgj.Butikk) THEN
        DO:
            PUT UNFORMATTED
            "<B>"
/*             "<C68><FROM><C+10><LINE>" SKIP */
            "<C68><RIGHT=C+10>" + STRING(plSum,"->>,>>9.99")
            "</B>" SKIP.

            ASSIGN
                plSum = 0
                .
        END.
      END. /* BILAG */

      PUT UNFORMATTED
          SKIP.
      IF tmpKas_Rap.AntRetur <> 0 THEN
      ANTALL_RETURER:
      DO:
          PUT UNFORMATTED
              SKIP
              "<C53>Returer" 
              "<C62><RIGHT=C+7>"
              STRING(tmpKas_Rap.AntRetur,"->>,>>9")
              "<C67><RIGHT=C+10>" + STRING(tmpKas_Rap.Retur,"->>,>>9.99").
      END. /* ANTALL_RETURER */
      IF tmpKas_Rap.AntReklamasjon <> 0 THEN
      ANTALL_REKLAMASJONER:
      DO:
          PUT UNFORMATTED
              SKIP
              "<C53>Reklamasjoner" 
              "<C62><RIGHT=C+7>"
              STRING(tmpKas_Rap.AntReklamasjon,"->>,>>9")
              "<C67><RIGHT=C+10>" + STRING(tmpKas_Rap.Reklamasjon,"->>,>>9.99").
      END. /* ANTALL_REKLAMASJONER */

      IF tmpKas_Rap.AntallUtbetBonger <> 0 THEN
      ANTALL_BONGER_MED_UTBETALING:
      DO:
          PUT UNFORMATTED
              SKIP
              "<C53>Bonger med utbetaling" 
              "<C62><RIGHT=C+7>"
              STRING(tmpKas_Rap.AntallUtbetBonger,"->>,>>9")
              "<C67><RIGHT=C+10>" + STRING(tmpKas_Rap.VerdiUtbetBonger,"->>,>>9.99").
      END. /* ANTALL_BONGER_MED_UTBETALING */
      FIND FIRST tt_kortretur NO-ERROR.
      IF AVAIL tt_kortretur THEN DO:
          PUT UNFORMATTED
              SKIP
              "<C53>Bonger med returer (bank)" 
              "<C62><RIGHT=C+7>"
              STRING(tt_kortretur.iAnt,"->>,>>9")
              "<C67><RIGHT=C+10>" + STRING(tt_kortretur.dSum,"->>,>>9.99").
      END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Butikrubrik C-Win 
PROCEDURE Butikrubrik :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cString AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE lFlereDar AS LOGICAL    NO-UNDO.
    ASSIGN lFlereDar = FI-FraDato <> FI-TilDato.
    FIND Butiker WHERE Butiker.Butik = tmpKas_rap.Butikk NO-LOCK NO-ERROR.
    CASE tmpKas_rap.Kasse:
        WHEN -99999 THEN
            ASSIGN cString = "Rapporttotal ".
        WHEN -9999 THEN
            ASSIGN cString = "Butikktotal ".
        OTHERWISE DO:
            ASSIGN cString = IF tmpKas_rap.Sortering = 0 AND lFlereDar THEN "Kassetotal Kasse " + STRING(ABS(tmpKas_rap.Kasse)) ELSE "Kasse " + string(tmpKas_rap.Kasse)
                             + " " + STRING(tmpKas_rap.Dato).
        END.
    END CASE.
/*     ASSIGN cString = IF tmpKas_rap.Kasse = -9999 THEN "Butikktotal" ELSE "Kasse " + string(tmpKas_rap.Kasse) */
/*                  + " " + STRING(tmpKas_rap.Dato).                                                            */
    IF AVAIL Butiker THEN
        PUT UNFORMATTED 
            "<P12><R8><C6><B>Butikk " tmpKas_rap.Butikk " " Butiker.ButNamn " " cString.
    ELSE 
        PUT UNFORMATTED
            "<P12><R8><C6><B>" cString.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ButikRubrikBokf C-Win 
PROCEDURE ButikRubrikBokf :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cString AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE lFlereDar AS LOGICAL    NO-UNDO.

    ASSIGN 
        lFlereDar = FI-FraDato <> FI-TilDato
        .

    FIND Butiker WHERE Butiker.Butik = tmpKas_rap.Butikk NO-LOCK NO-ERROR.

    CASE tmpKas_rap.Kasse:
        WHEN -99999 THEN
            ASSIGN cString = "Rapporttotal ".
        WHEN -9999 THEN
            ASSIGN cString = "Butikktotal ".
        OTHERWISE DO:
            ASSIGN cString = IF tmpKas_rap.Sortering = 0 AND lFlereDar THEN "Kassetotal Kasse " + STRING(ABS(tmpKas_rap.Kasse)) ELSE "Kasse " + string(tmpKas_rap.Kasse)
                             + " " + STRING(tmpKas_rap.Dato).
        END.
    END CASE.
/*     ASSIGN cString = IF tmpKas_rap.Kasse = -9999 THEN "Butikktotal" ELSE "Kasse " + string(tmpKas_rap.Kasse) */
/*                  + " " + STRING(tmpKas_rap.Dato).                                                            */
    IF AVAIL Butiker THEN
    DO:
        FIND FIRST Bokforingsbilag NO-LOCK WHERE 
            Bokforingsbilag.ButikkNr       = tmpKas_Rap.butikk AND
            Bokforingsbilag.OmsetningsDato = tmpKas_Rap.Dato NO-ERROR.
        IF AVAILABLE Bokforingsbilag AND Bokforingsbilag.GodkjentFlagg THEN
            cBokfNr = "Bokf.nr: " + STRING(Bokforingsbilag.BokforingsNr).
        ELSE
            cBokfNr = "Ikke godkjent".
        IF lFlereDar THEN
            cBokfNr = "  ".
        PUT UNFORMATTED 
            "<P12><R8><C6><B>Butikk " tmpKas_rap.Butikk " " Butiker.ButNamn " " cString 
            "<C58><RIGHT=C+20>" cBokfNr.
    END.
    ELSE 
    DO:
        cBokfNr = "Ikke godkjent".
        PUT UNFORMATTED
            "<P12><R8><C6><B>" cString "<C64><RIGHT=C+20>" cBokfNr.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ByggFinansRapport C-Win 
PROCEDURE ByggFinansRapport PRIVATE :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR ipButik      AS INT  NO-UNDO.
  DEF VAR ipKasse      AS INT  NO-UNDO.
  DEF VAR piLoop1      AS INT  NO-UNDO.
  DEF VAR pdDato       AS DATE NO-UNDO.
  DEFINE VARIABLE lFlereDar AS LOGICAL    NO-UNDO.

  DO WITH FRAME Default-frame:
    ASSIGN
      FI-FraDato
      FI-TilDato
      T-Kuntotal
/*       T-Butikk */
/*       T-Total */
      ipButikLst = INPUT FI-Butiker
      wAntPoster = 0
      lFlereDar  = FI-FraDato <> FI-TilDato
      .
  END.
  /* Tømmer temp-table */
  EMPTY TEMP-TABLE tmpKas_Rap NO-ERROR.

  IF ipButikLst = "[Alle]" THEN
  DO:
      ASSIGN
          ipButikLst = ""
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

  /* Bokføringsbilaget kan kun tas ut for en og en butikk. */
  IF INPUT CB-Rapport = 1 THEN
  DO:
      IF NUM-ENTRIES(ipButikLst) > 1 THEN
      DO:
          MESSAGE "Bokføringsbilaget kan bare tas ut for en butikk av gangen." SKIP
                  "Velg en butikk."
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
          RETURN NO-APPLY "AVBRYT".
      END.
      IF NOT CAN-FIND(Butiker WHERE
                      Butiker.Butik = INT(ipButikLst)) THEN
      DO:
          MESSAGE "Ukjent butikk eller [Alle] butikker er valg." SKIP
                  "Velg en butikk."
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
          RETURN NO-APPLY "AVBRYT".
      END.
  END.

  /* Bygger opp tabell */
  BYGG:
  DO pdDato = FI-FraDato TO FI-TilDato:

      /* Henter postene */
      KASSELOOP:
      FOR EACH Kas_Rap NO-LOCK WHERE
          Kas_Rap.Dato = pdDato AND
          CAN-DO(ipButikLst,STRING(Kas_Rap.Butikk))
          BREAK BY Kas_Rap.Dato:

          /* Melding til bruker */
          ASSIGN
            wAntPoster  = wAntPoster  + 1.
          IF wAntPoster MODULO 10 = 0 THEN
            STATUS DEFAULT "Antall poster " + STRING(wAntPoster) + " (" + STRING(Kas_Rap.Dato) + ").".               
          
          /* legger inn kasseposten i temp tabellen */
          IF NOT T-Kuntotal THEN
          DO:
              CREATE tmpKas_Rap.
              BUFFER-COPY Kas_Rap TO tmpKas_Rap.
              ASSIGN tmpKas_Rap.Sortering = IF lFlereDar THEN tmpKas_Rap.Kasse ELSE 0.
              RELEASE tmpKas_Rap.
          END.
          /* Sumerer opp bonger med utbetaling */
          ASSIGN
              iAntallUtbetBonger = 0
              lVerdiUtbetBonger  = 0
              .
          /* Dette er overkill, men det manger felt i kas_rap. Så bongene må leses. */
          FOR EACH BongHode NO-LOCK WHERE
              BongHode.ButikkNr = Kas_Rap.Butikk AND
              BongHode.GruppeNr = 1 AND
              BongHode.KasseNr  = Kas_Rap.Kasse AND
              BongHode.Dato     = FI-FraDato:

              IF BongHode.Belop >= 0 THEN NEXT.

              IF BongHode.KassererNr <> Kas_Rap.KassererNr THEN NEXT.

              IF CAN-FIND(FIRST BongLinje WHERE
                          BongLinje.B_Id = BongHode.B_Id AND
                          CAN-DO('50,70',STRING(BongLinje.TTId))) THEN
              ASSIGN
                  iAntallUtbetBonger = iAntallUtbetBonger + 1
                  lVerdiUtbetBonger  = lVerdiUtbetBonger  + BongHode.Belop
                  .
          END.
          /* Bygger kassetotaler */
/*           IF T-Kasse AND lFlereDar THEN */
          IF lFlereDar THEN
          KASSETOTAL:
          DO:
              FIND tmpKas_Rap WHERE
                   tmpKas_Rap.Dato      = FI-FraDato AND /* Kas_Rap.Dato AND */
                   tmpKas_Rap.Butikk    = Kas_Rap.Butikk AND
                   tmpKas_Rap.Kasse     = Kas_Rap.Kasse AND
                   tmpKas_Rap.Z_Nummer  = 0 AND
                   tmpKas_Rap.Sortering = 0 NO-ERROR.
              IF NOT AVAILABLE tmpKas_Rap THEN
              DO:
                  CREATE tmpKas_Rap.
                  ASSIGN
                      tmpKas_Rap.Dato      = FI-FraDato /* Kas_Rap.Dato  */
                      tmpKas_Rap.Butikk    = Kas_Rap.Butikk 
                      tmpKas_Rap.Kasse     = Kas_Rap.Kasse
                      tmpKas_Rap.Z_Nummer  = 0 
                      tmpKas_Rap.Sortering = 0 
                      .
              END.
              RUN SummerPost.
          END. /* KASSETOTAL */
          
          /* Bygger butikktotaler */
/*           IF T-Butikk AND CAN-FIND(FIRST Kasse WHERE Kasse.ButikkNr = Kas_Rap.Butikk AND */
/*                                                      Kasse.KasseNr <> Kas_Rap.Kasse AND  */
/*                                                      Kasse.Aktiv = TRUE) THEN            */
          BUTIKKTOTAL:
          DO:
              FIND tmpKas_Rap WHERE
                   tmpKas_Rap.Dato     = FI-FraDato AND /* Kas_Rap.Dato AND */
                   tmpKas_Rap.Butikk   = Kas_Rap.Butikk AND
                   tmpKas_Rap.Kasse    = -9999 AND
                   tmpKas_Rap.Z_Nummer = 0 NO-ERROR.
              IF NOT AVAILABLE tmpKas_Rap THEN
              DO:
                  CREATE tmpKas_Rap.
                  ASSIGN
                      tmpKas_Rap.Dato     = FI-FraDato /* Kas_Rap.Dato  */
                      tmpKas_Rap.Butikk   = Kas_Rap.Butikk 
                      tmpKas_Rap.Kasse    = -9999
                      tmpKas_Rap.Z_Nummer = 0
                      .
/*                   RUN HentKassererRapport. */
              END.
              IF FIRST-OF(Kas_Rap.Dato) THEN
                  RUN HentKassererRapport.
              RUN SummerPost.
          END. /* BUTIKKTOTAL */

          /* Bygger Total */
/*           IF T-Total AND NUM-ENTRIES(FI-Butiker) > 1 AND lFlereDar THEN */
          IF NUM-ENTRIES(FI-Butiker) > 1 THEN
          DO:
              FIND tmpKas_Rap WHERE
                   tmpKas_Rap.Dato     = FI-FraDato AND
                   tmpKas_Rap.Butikk   = -1  /* 1000000 */ AND
                   tmpKas_Rap.Kasse    = -99999 /* 9999  */ AND
                   tmpKas_Rap.Z_Nummer = 0 NO-ERROR.
              IF NOT AVAILABLE tmpKas_Rap THEN
              DO:
                  CREATE tmpKas_Rap.
                  ASSIGN
                      tmpKas_Rap.Dato     = FI-FraDato
                      tmpKas_Rap.Butikk   = -1 /* 1000000 */
                      tmpKas_Rap.Kasse    = -99999
                      tmpKas_Rap.Z_Nummer = 0
                      .
              END.
              RUN SummerPost.
          END.
      END. /* KASSELOOP */

  END. /* BYGG */
  IF CB-Rapport = 1  AND FI-FraDato = FI-TilDato THEN DO:
      ipButik = INT(ENTRY(1,ipButikLst)) NO-ERROR.
      IF NOT ERROR-STATUS:ERROR AND ipButik > 0 THEN
          RUN ByggTilgodeAndre (FI-FraDato, INT(ipButik)).
  END.
  STATUS DEFAULT "".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ByggKortSpes C-Win 
PROCEDURE ByggKortSpes :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR ipButik      AS INT  NO-UNDO.
  DEF VAR ipKasse      AS INT  NO-UNDO.
  DEF VAR piLoop1      AS INT  NO-UNDO.
  DEF VAR pdDato       AS DATE NO-UNDO.
  DEFINE VARIABLE lFlereDar AS LOGICAL    NO-UNDO.

  DO WITH FRAME Default-frame:
    ASSIGN
      FI-FraDato
      FI-TilDato
      T-Kuntotal
/*       T-Butikk */
/*       T-Total */
      ipButikLst = INPUT FI-Butiker
      wAntPoster = 0
      lFlereDar  = FI-FraDato <> FI-TilDato
      .
  END.
  /* Tømmer temp-table */
  EMPTY TEMP-TABLE tmpKort_Spes NO-ERROR.

  IF ipButikLst = "[Alle]" THEN
  DO:
      ASSIGN
          ipButikLst = ""
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

/* Bygger opp tabell */
  BYGG:
  DO pdDato = FI-FraDato TO FI-TilDato:

      /* Henter postene */
      KASSELOOP:
      FOR EACH Kort_Spes NO-LOCK WHERE
          Kort_Spes.Dato = pdDato
          BREAK BY Kort_Spes.Dato:

          /* Melding til bruker */
          ASSIGN
            wAntPoster  = wAntPoster  + 1.
          IF wAntPoster MODULO 10 = 0 THEN
            STATUS DEFAULT "Antall poster " + STRING(wAntPoster) + " (" + STRING(Kort_Spes.Dato) + ").".               
          
          /* Skipper poster som ikke skal være med. */
          IF NOT CAN-DO(ipButikLst,STRING(Kort_Spes.Butikk)) THEN
              NEXT KASSELOOP.
          /* Legger opp kassetotaler */
          IF NOT T-Kuntotal THEN
          DO:
              CREATE tmpKort_Spes.
              BUFFER-COPY Kort_Spes TO tmpKort_Spes.
              ASSIGN tmpKort_Spes.Sortering = IF lFlereDar THEN tmpKort_Spes.Kasse ELSE 0.
              RELEASE tmpKort_Spes.
          END.

          /* Bygger kassetotaler */
/*           IF T-Kasse AND lFlereDar THEN */
          IF lFlereDar THEN
          KASSETOTAL:
          DO:
              FIND tmpKort_Spes WHERE
                   tmpKort_Spes.Dato      = FI-FraDato AND /* Kort_Spes.Dato AND */
                   tmpKort_Spes.Butikk    = Kort_Spes.Butikk AND
                   tmpKort_Spes.Kasse     = Kort_Spes.Kasse AND
                   tmpKort_Spes.KortType  = Kort_Spes.KortType AND
                   tmpKort_Spes.Z_Nummer  = 0 AND
                   tmpKort_Spes.Sortering = 0 NO-ERROR.
              IF NOT AVAILABLE tmpKort_Spes THEN
              DO:
                  CREATE tmpKort_Spes.
                  ASSIGN
                      tmpKort_Spes.Dato      = FI-FraDato /* Kort_Spes.Dato  */
                      tmpKort_Spes.Butikk    = Kort_Spes.Butikk 
                      tmpKort_Spes.Kasse     = Kort_Spes.Kasse
                      tmpKort_Spes.KortType  = Kort_Spes.KortType
                      tmpKort_Spes.Z_Nummer  = 0 
                      tmpKort_Spes.Sortering = 0 
                      .
              END.
              IF tmpKort_Spes.KortType = 0 THEN
                  tmpKort_Spes.KortType = 50.
              RUN SummerKortSpes.
          END. /* KASSETOTAL */
          
          /* Bygger butikktotaler */
/*           IF T-Butikk AND CAN-FIND(FIRST Kasse WHERE Kasse.ButikkNr = Kort_Spes.Butikk AND */
/*                                                      Kasse.KasseNr <> Kort_Spes.Kasse AND  */
/*                                                      Kasse.Aktiv = TRUE) THEN            */
          BUTIKKTOTAL:
          DO:
              FIND tmpKort_Spes WHERE
                   tmpKort_Spes.Dato     = FI-FraDato AND /* Kort_Spes.Dato AND */
                   tmpKort_Spes.Butikk   = Kort_Spes.Butikk AND
                   tmpKort_Spes.Kasse    = -9999 AND
                   tmpKort_Spes.KortType = Kort_Spes.KortType AND
                   tmpKort_Spes.Z_Nummer = 0 NO-ERROR.
              IF NOT AVAILABLE tmpKort_Spes THEN
              DO:
                  CREATE tmpKort_Spes.
                  ASSIGN
                      tmpKort_Spes.Dato     = FI-FraDato /* Kort_Spes.Dato  */
                      tmpKort_Spes.Butikk   = Kort_Spes.Butikk 
                      tmpKort_Spes.Kasse    = -9999
                      tmpKort_Spes.KortType = Kort_Spes.KortType
                      tmpKort_Spes.Z_Nummer = 0
                      .
              END.
              IF tmpKort_Spes.KortType = 0 THEN
                  tmpKort_Spes.KortType = 50.
              RUN SummerKortSpes.
          END. /* BUTIKKTOTAL */

          /* Bygger Total */
/*           IF T-Total AND NUM-ENTRIES(FI-Butiker) > 1 AND lFlereDar THEN */
          IF NUM-ENTRIES(ipButikLst) > 1 THEN
          DO:
              FIND tmpKort_Spes WHERE
                   tmpKort_Spes.Dato     = FI-FraDato AND
                   tmpKort_Spes.Butikk   = -1  /* 1000000 */ AND
                   tmpKort_Spes.Kasse    = -99999 /* 9999  */ AND
                   tmpKort_Spes.KortType = Kort_Spes.KortType AND
                   tmpKort_Spes.Z_Nummer = 0 NO-ERROR.
              IF NOT AVAILABLE tmpKort_Spes THEN
              DO:
                  CREATE tmpKort_Spes.
                  ASSIGN
                      tmpKort_Spes.Dato     = FI-FraDato
                      tmpKort_Spes.Butikk   = -1 /* 1000000 */
                      tmpKort_Spes.Kasse    = -99999
                      tmpKort_Spes.KortType = Kort_Spes.KortType
                      tmpKort_Spes.Z_Nummer = 0
                      .
              END.
              IF tmpKort_Spes.KortType = 0 THEN
                  tmpKort_Spes.KortType = 50.
              RUN SummerKortSpes.
          END.
      END. /* KASSELOOP */

  END. /* BYGG */

  /* None_Sale_Spes */
  DO piLoop1 = 1 TO NUM-ENTRIES(ipButikLst): 
    FOR EACH Kasse NO-LOCK WHERE 
        Kasse.ButikkNr = INT(ENTRY(piLoop1,ipButikLst)):
      DO pdDato = FI-FraDato TO FI-TilDato:
          FOR EACH NON_Sale_spes WHERE NON_Sale_spes.butikk = INT(ENTRY(piLoop1,ipButikLst)) AND
                                       NON_Sale_spes.kasse  = kasse.kassenr  AND
                                       NON_Sale_spes.dato   = pdDato NO-LOCK:
              IF lFlereDar THEN
              KASSETOTAL:
              DO:
                  FIND tt_NON_Sale_spes WHERE
                      tt_NON_Sale_spes.Butikk        = NON_Sale_spes.butikk AND
                      tt_NON_Sale_spes.Kasse         = Kasse.KasseNr AND
                      tt_NON_Sale_spes.Dato          = FI-FraDato AND
                      tt_NON_Sale_spes.Non_Sale_Type = NON_Sale_spes.Non_Sale_Type NO-ERROR.
                  IF NOT AVAILABLE tt_NON_Sale_spes THEN
                  DO:
                      CREATE tt_NON_Sale_spes.
                      ASSIGN
                          tt_NON_Sale_spes.Butikk        = NON_Sale_spes.butikk 
                          tt_NON_Sale_spes.Kasse         = Kasse.KasseNr 
                          tt_NON_Sale_spes.Dato          = FI-FraDato 
                          tt_NON_Sale_spes.Non_Sale_Type = NON_Sale_spes.Non_Sale_Type.
                  END.
                  ASSIGN tt_NON_Sale_Spes.NON_SaleAntall = tt_NON_Sale_Spes.NON_SaleAntall + NON_Sale_Spes.NON_SaleAntall
                         tt_NON_Sale_Spes.NON_SaleVerdi  = tt_NON_Sale_Spes.NON_SaleVerdi  + NON_Sale_Spes.NON_SaleVerdi.
              END. /* KASSETOTAL */

              /* Bygger butikk Total */
              DO:
                  FIND tt_NON_Sale_spes WHERE
                      tt_NON_Sale_spes.Butikk        = NON_Sale_spes.butikk AND
                      tt_NON_Sale_spes.Kasse         = -9999 /* 9999  */ AND
                      tt_NON_Sale_spes.Dato          = FI-FraDato AND
                      tt_NON_Sale_spes.Non_Sale_Type = NON_Sale_spes.Non_Sale_Type NO-ERROR.
                  IF NOT AVAILABLE tt_NON_Sale_spes THEN
                  DO:
                      CREATE tt_NON_Sale_spes.
                      ASSIGN
                          tt_NON_Sale_spes.Butikk        = NON_Sale_spes.butikk 
                          tt_NON_Sale_spes.Kasse         = -9999 /* 9999  */ 
                          tt_NON_Sale_spes.Dato          = FI-FraDato 
                          tt_NON_Sale_spes.Non_Sale_Type = NON_Sale_spes.Non_Sale_Type.
                  END.
                  ASSIGN tt_NON_Sale_Spes.NON_SaleAntall = tt_NON_Sale_Spes.NON_SaleAntall + NON_Sale_Spes.NON_SaleAntall
                         tt_NON_Sale_Spes.NON_SaleVerdi  = tt_NON_Sale_Spes.NON_SaleVerdi  + NON_Sale_Spes.NON_SaleVerdi.
              END.

              /* Bygger Grand Total */
              IF NUM-ENTRIES(ipButikLst) > 1 THEN
              DO:
                  FIND tt_NON_Sale_spes WHERE
                      tt_NON_Sale_spes.Butikk        = -1  /* 1000000 */ AND
                      tt_NON_Sale_spes.Kasse         = -99999 /* 9999  */ AND
                      tt_NON_Sale_spes.Dato          = FI-FraDato AND
                      tt_NON_Sale_spes.Non_Sale_Type = NON_Sale_spes.Non_Sale_Type NO-ERROR.
                  IF NOT AVAILABLE tt_NON_Sale_spes THEN
                  DO:
                      CREATE tt_NON_Sale_spes.
                      ASSIGN
                          tt_NON_Sale_spes.Butikk        = -1  /* 1000000 */ 
                          tt_NON_Sale_spes.Kasse         = -99999 /* 9999  */ 
                          tt_NON_Sale_spes.Dato          = FI-FraDato 
                          tt_NON_Sale_spes.Non_Sale_Type = NON_Sale_spes.Non_Sale_Type.
                  END.
                  ASSIGN tt_NON_Sale_Spes.NON_SaleAntall = tt_NON_Sale_Spes.NON_SaleAntall + NON_Sale_Spes.NON_SaleAntall
                         tt_NON_Sale_Spes.NON_SaleVerdi  = tt_NON_Sale_Spes.NON_SaleVerdi  + NON_Sale_Spes.NON_SaleVerdi.
              END.

          END.
      END.
    END.
  END.
  IF lFlereDar = FALSE AND NUM-ENTRIES(ipButikLst) = 1 THEN DO:
      FOR EACH bonghode WHERE bonghode.butikknr = INT(ipButikLst) AND bonghode.dato = FI-FraDato NO-LOCK.
          FOR EACH bonglinje WHERE bonglinje.b_id = bonghode.b_id AND
                                   bonglinje.ttid = 52            AND
                                   bonglinje.linjesum < 0         USE-INDEX b_id NO-LOCK.
              IF NOT CAN-FIND(FIRST tt_kortretur WHERE tt_kortretur.butik = bonglinje.butikknr) THEN DO:
                  CREATE tt_kortretur.
                  ASSIGN tt_kortretur.butik = bonglinje.butikknr.
              END.
              ASSIGN tt_kortretur.iAnt = tt_kortretur.iAnt + 1
                     tt_kortretur.dSum = tt_kortretur.dSum + bonglinje.linjesum.
          END.
      END.
  END.

  STATUS DEFAULT "".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ByggTilgodeAndre C-Win 
PROCEDURE ByggTilgodeAndre :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER dDato  AS DATE        NO-UNDO.
    DEFINE INPUT  PARAMETER iButik AS INTEGER     NO-UNDO.
    DEFINE VARIABLE ii AS INTEGER     NO-UNDO.
    DEFINE VARIABLE i2 AS INTEGER     NO-UNDO.
    EMPTY TEMP-TABLE tt_tilgode.
    FOR EACH tilgode WHERE Tilgode.BruktDato = dDato AND Tilgode.BruktButNr = iButik NO-LOCK.
        IF Tilgode.butnr <> Tilgode.BruktButNr THEN DO:
            CREATE tt_tilgode.
            BUFFER-COPY tilgode TO tt_tilgode NO-ERROR.
            IF ERROR-STATUS:ERROR THEN
                DELETE tt_tilgode.
        END.
    END.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Diverse C-Win 
PROCEDURE Diverse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE pcOverskr AS CHARACTER  NO-UNDO.
DEFINE VARIABLE pcLabel AS CHARACTER  NO-UNDO.
ASSIGN pcOverskr = "Diverse" + CHR(1) + "Antall" + CHR(1) + "Beløp".
ASSIGN pcLabel   = "Utbetalt" + CHR(1) + "Innbetalt" + CHR(1) + "Tilgode inn" + CHR(1) + "Tilgode ut" + CHR(1) + 
                   "Dep-in" + CHR(1) + "Dep-ut" + CHR(1) + "Gavekort inn" + CHR(1) + 
                   "Gavekort ut" + CHR(1) + "Dropp" + CHR(1) + "Inngående interne overføringer" + CHR(1) + 
                   "Utgående interne overføringer" + CHR(1) + "Varemottak" + CHR(1) + "Lagerjustering" + CHR(1) +
                   "Brekkasje" + CHR(1) + "Internt forbruk" + CHR(1) + "Reklamasjon" + CHR(1) + 
                   "Medlemssalg" + CHR(1) + "Innbetalt kunde" + CHR(1) + "Veksel" + CHR(1) + "Returer" + CHR(1) + "Gavekortrabatt".
    
    PUT UNFORMATTED 
        "<P12><R29><C43><B>"  ENTRY(1,pcOverskr,CHR(1)) SKIP "</B><C43><FROM><C75><LINE>" SKIP
        "<P8><C56><RIGHT=C+6>"  ENTRY(2,pcOverskr,CHR(1))
        "<C64><RIGHT=C+11>"  ENTRY(3,pcOverskr,CHR(1)) SKIP
        "<C56><FROM><C62><LINE>" "<C64><FROM><C75><LINE>" 
        "<C43>" +  entry(1,pcLabel,CHR(1)) + "<C56><RIGHT=C+6>" + STRING(tmpKas_Rap.AntKont_ut,"->>,>>9") + "<C64><RIGHT=C+11>" + STRING(tmpKas_Rap.Kont_Ut,"->>>,>>>,>>9.99") SKIP        
        "<C43>" +  entry(2,pcLabel,CHR(1)) + "<C56><RIGHT=C+6>" + STRING(tmpKas_Rap.AntKont_inn,"->>,>>9") + "<C64><RIGHT=C+11>" + STRING(tmpKas_Rap.Kont_Inn,"->>>,>>>,>>9.99") SKIP
        "<C43>" +  entry(3,pcLabel,CHR(1)) + "<C56><RIGHT=C+6>" + STRING(tmpKas_Rap.AntTilgodeInn + tmpKas_Rap.AntTilgodeAndre,"->>,>>9") + "<C64><RIGHT=C+11>" + STRING(tmpKas_Rap.TilgodeInn + tmpKas_Rap.TilgodeAndre,"->>>,>>>,>>9.99") SKIP
        "<C43>" +  entry(4,pcLabel,CHR(1)) + "<C56><RIGHT=C+6>" + STRING(tmpKas_Rap.AntTilgodeUt,"->>,>>9") + "<C64><RIGHT=C+11>" + STRING(tmpKas_Rap.TilgodeUt,"->>>,>>>,>>9.99") SKIP
        "<C43>" +  entry(5,pcLabel,CHR(1)) + "<C56><RIGHT=C+6>" + STRING(tmpKas_Rap.AntLayAway_Inn,"->>,>>9") + "<C64><RIGHT=C+11>" + STRING(tmpKas_Rap.LayAway_Inn,"->>>,>>>,>>9.99") SKIP
        "<C43>" +  entry(6,pcLabel,CHR(1)) + "<C56><RIGHT=C+6>" + STRING(tmpKas_Rap.AntLayAway_Ut,"->>,>>9") + "<C64><RIGHT=C+11>" + STRING(tmpKas_Rap.LayAway_Ut,"->>>,>>>,>>9.99") SKIP
        "<C43>" +  entry(7,pcLabel,CHR(1)) + "<C56><RIGHT=C+6>" + STRING(tmpKas_Rap.AntGavekortInn,"->>,>>9") + "<C64><RIGHT=C+11>" + STRING(tmpKas_Rap.GavekortInn,"->>>,>>>,>>9.99") SKIP
        "<C43>" +  entry(8,pcLabel,CHR(1)) + "<C56><RIGHT=C+6>" + STRING(tmpKas_Rap.AntGavekortUt,"->>,>>9") + "<C64><RIGHT=C+11>" + STRING(tmpKas_Rap.GavekortUt,"->>>,>>>,>>9.99") SKIP
        "<C43>" +  entry(21,pcLabel,CHR(1)) + "<C56><RIGHT=C+6>" + STRING(tmpKas_Rap.AntGavekortRabUt,"->>,>>9") + "<C64><RIGHT=C+11>" + STRING(tmpKas_Rap.GavekortRabatt,"->>>,>>>,>>9.99") SKIP
        "<C43>" +  entry(9,pcLabel,CHR(1)) + "<C56><RIGHT=C+6>" + STRING(tmpKas_Rap.AntDropp,"->>,>>9") + "<C64><RIGHT=C+11>" + STRING(tmpKas_Rap.Dropp,"->>>,>>>,>>9.99") SKIP
        "<C43>" +  entry(10,pcLabel,CHR(1)) + "<C56><RIGHT=C+6>" + STRING(tmpKas_Rap.AntOverfortInn,"->>,>>9") + "<C64><RIGHT=C+11>" + STRING(tmpKas_Rap.OverfortInn,"->>>,>>>,>>9.99") SKIP
        "<C43>" +  entry(11,pcLabel,CHR(1)) + "<C56><RIGHT=C+6>" + STRING(tmpKas_Rap.AntOverfortUt,"->>,>>9") + "<C64><RIGHT=C+11>" + STRING(tmpKas_Rap.OverfortUt,"->>>,>>>,>>9.99") SKIP
        "<C43>" +  entry(12,pcLabel,CHR(1)) + "<C56><RIGHT=C+6>" + STRING(tmpKas_Rap.AntVaremottak,"->>,>>9") + "<C64><RIGHT=C+11>" + STRING(tmpKas_Rap.Varemottak,"->>>,>>>,>>9.99") SKIP
        "<C43>" +  entry(13,pcLabel,CHR(1)) + "<C56><RIGHT=C+6>" + STRING(tmpKas_Rap.AntLagerjustering,"->>,>>9") + "<C64><RIGHT=C+11>" + STRING(tmpKas_Rap.Lagerjustering,"->>>,>>>,>>9.99") SKIP
        "<C43>" +  entry(14,pcLabel,CHR(1)) + "<C56><RIGHT=C+6>" + STRING(tmpKas_Rap.AntBrekkasje,"->>,>>9") + "<C64><RIGHT=C+11>" + STRING(tmpKas_Rap.Brekkasje,"->>>,>>>,>>9.99") SKIP
        "<C43>" +  entry(15,pcLabel,CHR(1)) + "<C56><RIGHT=C+6>" + STRING(tmpKas_Rap.AntInterntForbruk,"->>,>>9") + "<C64><RIGHT=C+11>" + STRING(tmpKas_Rap.InterntForbruk,"->>>,>>>,>>9.99") SKIP
        "<C43>" +  entry(16,pcLabel,CHR(1)) + "<C56><RIGHT=C+6>" + STRING(tmpKas_Rap.AntReklamasjon,"->>,>>9") + "<C64><RIGHT=C+11>" + STRING(tmpKas_Rap.Reklamasjon,"->>>,>>>,>>9.99") SKIP
        "<C43>" +  entry(17,pcLabel,CHR(1)) + "<C56><RIGHT=C+6>" + STRING(tmpKas_Rap.AntMedlemssalg,"->>,>>9") + "<C64><RIGHT=C+11>" + STRING(tmpKas_Rap.Medlemssalg,"->>>,>>>,>>9.99") SKIP
        "<C43>" +  entry(18,pcLabel,CHR(1)) + "<C56><RIGHT=C+6>" + STRING(tmpKas_Rap.AntInnbetaltKunde,"->>,>>9") + "<C64><RIGHT=C+11>" + STRING(tmpKas_Rap.InnbetaltKunde,"->>>,>>>,>>9.99") SKIP
        "<C43>" +  entry(19,pcLabel,CHR(1)) + "<C56><RIGHT=C+6>" + STRING(tmpKas_Rap.AntVeksel,"->>,>>9") + "<C64><RIGHT=C+11>" + STRING(tmpKas_Rap.Veksel,"->>>,>>>,>>9.99") SKIP
        "<C43>" +  entry(20,pcLabel,CHR(1)) + "<C56><RIGHT=C+6>" + STRING(tmpKas_Rap.AntRetur,"->>,>>9") + "<C64><RIGHT=C+11>" + STRING(tmpKas_Rap.Retur,"->>>,>>>,>>9.99") SKIP
        "<C43><FROM><C75><LINE>" SKIP.

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
  DISPLAY CB-Rapport FI-Butiker FI-FraDato FI-TilDato T-Kuntotal 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE B-SokButikker RECT-54 RECT-55 RECT-56 CB-Rapport FI-FraDato FI-TilDato 
         T-Kuntotal B-Butikker BUTTON-SokDato-2 B-XPrint BUTTON-SokDato B-Exit 
         Btn_Help 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE HentKassererRapport C-Win 
PROCEDURE HentKassererRapport :
/*------------------------------------------------------------------------------
  Purpose:     Adderer inn kassereroppgjøret i butikktotalen når den første 
               gang opprettes.
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  /* Adderer opp opptalte verdier. */
  OPPGJOR:
  FOR EACH KassererOppgj NO-LOCK WHERE
    KassererOppgj.Dato     = Kas_Rap.Dato AND
    KassererOppgj.Butikk   = Kas_Rap.Butikk:
    ASSIGN
      tmpKas_Rap.OpptaltVeksel           = tmpKas_Rap.OpptaltVeksel           + KassererOppgj.OpptaltVeksel   
      tmpKas_Rap.OpptaltKontanter        = tmpKas_Rap.OpptaltKontanter        + KassererOppgj.OpptaltKontanter
      tmpKas_Rap.OpptaltSjekk            = tmpKas_Rap.OpptaltSjekk            + KassererOppgj.OpptaltSjekk    
      tmpKas_Rap.OpptaltReserve          = tmpKas_Rap.OpptaltReserve          + KassererOppgj.OpptaltReserve  
      tmpKas_Rap.OpptaltGavekort         = tmpKas_Rap.OpptaltGavekort         + KassererOppgj.OpptaltGavekort 
      tmpKas_Rap.OpptaltTilgode          = tmpKas_Rap.OpptaltTilgode          + KassererOppgj.OpptaltTilgode  
      tmpKas_Rap.OpptaltGaveKortAndre    = tmpKas_Rap.OpptaltGaveKortAndre    + KassererOppgj.OpptaltGaveKortAndre  
      tmpKas_Rap.OpptaltGavekortUtlevert = tmpKas_Rap.OpptaltGavekortUtlevert + KassererOppgj.OpptaltGavekortUtlevert  
      tmpKas_Rap.OpptaltTilgodeAndre     = tmpKas_Rap.OpptaltTilgodeAndre     + KassererOppgj.OpptaltTilgodeAndre  
      tmpKas_Rap.OpptaltTilgodeUtlevert  = tmpKas_Rap.OpptaltTilgodeUtlevert  + KassererOppgj.OpptaltTilgodeUtlevert  
      tmpKas_Rap.OpptaltInnVeksel        = tmpKas_Rap.OpptaltInnVeksel        + KassererOppgj.OpptaltInnVeksel  
      tmpKas_Rap.OpptaltValuta           = tmpKas_Rap.OpptaltValuta           + KassererOppgj.OpptaltValuta  
      tmpKas_Rap.OpptaltLevertBank       = tmpKas_Rap.OpptaltLevertBank       + KassererOppgj.OpptaltLevertBank  
      tmpKas_Rap.OpptaltBilag            = tmpKas_Rap.OpptaltBilag            + KassererOppgj.OpptaltBilag  
      .


/*     OUTPUT TO VALUE("logg.txt") APPEND.                  */
/*     PUT "GURRE" SKIP.                                    */
/*     DISPLAY                                              */
/*       KassererOppgj.Dato                                 */
/*       KassererOppgj.Butikk                               */
/*       KassererOppgj.Kasserer                             */
/*       tmpKas_Rap.OpptaltVeksel                           */
/*       tmpKas_Rap.OpptaltKontanter                        */
/*       tmpKas_Rap.OpptaltSjekk                            */
/*       tmpKas_Rap.OpptaltReserve                          */
/*       tmpKas_Rap.OpptaltBank                             */
/*       tmpKas_Rap.OpptaltGavekort                         */
/*       tmpKas_Rap.OpptaltKredit                           */
/*       tmpKas_Rap.OpptaltTilgode                          */
/*       tmpKas_Rap.OpptaltGaveKortAndre                    */
/*       tmpKas_Rap.OpptaltGavekortUtlevert                 */
/*       tmpKas_Rap.OpptaltTilgodeAndre                     */
/*       tmpKas_Rap.OpptaltTilgodeUtlevert                  */
/*       tmpKas_Rap.OpptaltInnVeksel                        */
/*       tmpKas_Rap.OpptaltValuta                           */
/*       tmpKas_Rap.OpptaltLevertBank                       */
/*       tmpKas_Rap.OpptaltBilag                            */
/*       tmpKas_rap.MvaGrunnlag[ 1] tmpKas_rap.MvaBelop[ 1] */
/*       tmpKas_rap.MvaGrunnlag[ 2] tmpKas_rap.MvaBelop[ 2] */
/*       tmpKas_rap.MvaGrunnlag[ 3] tmpKas_rap.MvaBelop[ 3] */
/*       tmpKas_rap.MvaGrunnlag[ 4] tmpKas_rap.MvaBelop[ 4] */
/*       tmpKas_rap.MvaGrunnlag[ 5] tmpKas_rap.MvaBelop[ 5] */
/*       tmpKas_rap.MvaGrunnlag[ 6] tmpKas_rap.MvaBelop[ 6] */
/*       tmpKas_rap.MvaGrunnlag[ 7] tmpKas_rap.MvaBelop[ 7] */
/*       tmpKas_rap.MvaGrunnlag[ 8] tmpKas_rap.MvaBelop[ 8] */
/*       tmpKas_rap.MvaGrunnlag[ 9] tmpKas_rap.MvaBelop[ 9] */
/*       tmpKas_rap.MvaGrunnlag[10] tmpKas_rap.MvaBelop[10] */
/*       WITH SIDE-LABELS 2 COLUMNS DOWN.                   */
/*     DOWN 1.                                              */
/*     OUTPUT CLOSE.                                        */

  END.

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
IF Bruker.ButikkNr <> 0 AND Bruker.ButikkNr <> ? THEN
    FIND FIRST Butikktilgang WHERE Butikktilgang.BrGrpNr = Bruker.BrGrpNr AND ButikkTilgang.Butik = Bruker.ButikkNr NO-LOCK NO-ERROR.
IF NOT AVAIL Butikktilgang THEN
    FIND FIRST Butikktilgang WHERE Butikktilgang.BrGrpNr = Bruker.BrGrpNr NO-LOCK.
ASSIGN cFirstButik = STRING(Butikktilgang.butik).
/* FI-Butiker = "[Alle]". */
FI-Butiker = cFirstButik.
/* FOR EACH Butiker NO-LOCK WHERE                */
/*      CAN-FIND(FIRST Kasse WHERE               */
/*               Kasse.Butik = Butiker.Butik AND */
/*               Kasse.Aktiv = TRUE):            */
/*   ASSIGN                                      */
/*     FI-Butiker = FI-Butiker +                 */
/*                  (IF FI-Butiker = ""          */
/*                     THEN ""                   */
/*                     ELSE ",") +               */
/*                  STRING(Butiker.Butik).       */
/* END.                                          */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE KasseTot C-Win 
PROCEDURE KasseTot :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE pcOverskr AS CHARACTER  NO-UNDO.
DEFINE VARIABLE pcLabel AS CHARACTER  NO-UNDO.

ASSIGN pcOverskr = "Kasse totalt" + CHR(1) + "Antall" + CHR(1) + "Beløp".
ASSIGN pcLabel   = "Brutto omsetning" + CHR(1) + "Neg. vare" + CHR(1) + "Gen. rabatt" + CHR(1) + "Kunderabatt" + CHR(1) + 
                   "Personalrabatt" + CHR(1) + "Medlemsrabatt" + CHR(1) + "Netto omsetning" + CHR(1) + 
                   "Avrunding" + CHR(1) + "Registrert omsetning".
    PUT UNFORMATTED 
        "<P12><R10><C6><B>"  ENTRY(1,pcOverskr,CHR(1)) SKIP "</B><C6><FROM><C35><LINE>" SKIP
        "<P8><C16><RIGHT=C+6>"  ENTRY(2,pcOverskr,CHR(1))
        "<C24><RIGHT=C+11>"  ENTRY(3,pcOverskr,CHR(1)) SKIP                                                                                                                                       
        "<C16><FROM><C22><LINE>" "<C24><FROM><C35><LINE>"
        "<C6>" +  entry(1,pcLabel,CHR(1)) + "<C24><RIGHT=C+11>" + STRING(wBruttoOmsetning,"->>>,>>>,>>9.99") SKIP        
        "<C5>+<C6>" +  entry(2,pcLabel,CHR(1)) + "<C24><RIGHT=C+11>" + STRING((tmpKas_Rap.Retur + tmpKas_Rap.Reklamasjon) * -1,"->>>,>>>,>>9.99") SKIP
        "<C5>-<C6>" +  entry(3,pcLabel,CHR(1)) + "<C16><RIGHT=C+6>" + STRING(tmpKas_Rap.AntGenerellRabatt,"->>,>>9") + "<C24><RIGHT=C+11>" + STRING(tmpKas_Rap.GenerellRabatt,"->>>,>>>,>>9.99") SKIP
        "<C5>-<C6>" +  entry(4,pcLabel,CHR(1)) + "<C16><RIGHT=C+6>" + STRING(tmpKas_Rap.AntKunderabatt,"->>,>>9") + "<C24><RIGHT=C+11>" + STRING(tmpKas_Rap.Kunderabatt,"->>>,>>>,>>9.99") SKIP
        "<C5>-<C6>" +  entry(5,pcLabel,CHR(1)) + "<C16><RIGHT=C+6>" + STRING(tmpKas_Rap.AntPersonalrabatt,"->>,>>9") + "<C24><RIGHT=C+11>" + STRING(tmpKas_Rap.Personalrabatt,"->>>,>>>,>>9.99") SKIP
        "<C5>-<C6>" +  entry(6,pcLabel,CHR(1)) + "<C16><RIGHT=C+6>" + STRING(tmpKas_Rap.AntMedlemsrabatt,"->>,>>9") + "<C24><RIGHT=C+11>" + STRING(tmpKas_Rap.Medlemsrabatt,"->>>,>>>,>>9.99") SKIP
        "<C5>-<C6>" +  "Gavekort ut" + "<C16><RIGHT=C+6>" + STRING(tmpKas_Rap.AntGavekortUt,"->>,>>9") + "<C24><RIGHT=C+11>" + STRING(tmpKas_Rap.GavekortUt,"->>>,>>>,>>9.99") SKIP
        "<C6><FROM><C35><LINE>"
        "<C5>=<C6>" +  entry(7,pcLabel,CHR(1)) + "<C24><RIGHT=C+11>" + STRING(wBruttoOmsetning + 
                                                                              ((tmpKas_Rap.Retur + tmpKas_Rap.Reklamasjon) * - 1) - GenerellRabatt - Kunderabatt - Personalrabatt -  Medlemsrabatt - Pakkerabatt,"->>>,>>>,>>9.99") SKIP
        "<C5>-<C6>" ENTRY(8,pcLabel,CHR(1)) + "<C24><RIGHT=C+11>" + STRING(tmpKas_Rap.Avrunding,"->>>,>>>,>>9.99") SKIP
        "<C6><FROM><C35><LINE>"
        "<C5>=<C6>" +  entry(9,pcLabel,CHR(1))
        "<C24><RIGHT=C+11>" + STRING(wBruttoOmsetning + ((tmpKas_Rap.Retur + tmpKas_Rap.Reklamasjon) * -1) - 
                              GenerellRabatt - Kunderabatt - Personalrabatt -  Medlemsrabatt - Pakkerabatt - Avrunding - GavekortUt,"->>>,>>>,>>9.99") SKIP 
        "<C6><FROM><C35><LINE>" SKIP.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE KortSpes C-Win 
PROCEDURE KortSpes :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:     BRUKES AV BÅDE BOKFØRINGSBILAG OG FINANSRAPPORT.
  
  1  *  Bankkort
  2  *  Bankkort (Postbanken)
  3     Visa
  4     Eurocard
  5     American express
  6     Diners
  7     Coop kort
  8     Multikort
  9  *  Bankkort (GE Capital)
 10  *  Bankkort (Gjensidige bank)
 11     JCB
 12     Trumf
 13     Domino
 14     Maestro
 15     Lindexkortet
 16     IKAno
 17  *  Bankkort (DnB kort)
 18     Proton
 19     SH kortet
 20  *  Bankkort (COOP)
 21     BBL
 22     Gavekort Kjede
 23     Gavekort Senter
  
------------------------------------------------------------------------------*/

DEF INPUT PARAMETER piRappType AS INT NO-UNDO. /* 1-Finans 2-Bokføringsbilag. */

DEFINE VARIABLE pcOverskr AS CHARACTER  NO-UNDO.
DEFINE VARIABLE pcLabel   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE pcKonto   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE plSum     AS DEC        NO-UNDO.
DEFINE VARIABLE piLoop    AS INT        NO-UNDO.
DEFINE VARIABLE pcTekst1  AS CHAR       NO-UNDO.
DEFINE VARIABLE pcTekst2  AS CHAR       NO-UNDO.

  
  ASSIGN
    pcLabel  = FILL(CHR(1),999)
    pcKonto  = FILL(CHR(1),999)
    lKortSum = 0
  .
  /*
  /* Henter BBS korttabell fra syspara. */
  DO piLoop = 1 TO 99:
      {syspara.i 20  3 piLoop pcTekst1}
      {syspar2.i 20  3 piLoop pcTekst2}
    ASSIGN
        ENTRY(piLoop,pcLabel,CHR(1)) = pcTekst1
        ENTRY(piLoop,pcKonto,CHR(1)) = pcTekst2
        pcTekst1 = ""
        pcTekst2 = ""
        .
  END.
  */
  /* Henter BBS korttabell fra syspara. */
  FOR EACH TransBeskr NO-LOCK WHERE
      TransBeskr.TTId = 52:
    ASSIGN
        ENTRY(TransBeskr.TBId,pcLabel,CHR(1)) = TransBeskr.Beskrivelse
        ENTRY(TransBeskr.TBId,pcKonto,CHR(1)) = "D 0000"
        .
  END.
  /* Leser alle kortspesifikasjoner */
  KORTSPES:
  FOR EACH tmpKort_Spes NO-LOCK WHERE
    tmpKort_Spes.Butikk = INT(FI-Butiker) AND
    tmpKort_Spes.Kasse    = -9999
    BREAK BY tmpKort_Spes.Dato
          BY tmpKort_Spes.Butikk
          BY tmpKort_Spes.KortType:
    /* Nullstiller */
    IF FIRST-OF(tmpKort_Spes.KortType) THEN
      plSum = 0.

    /* Sum for tmpKorttypen */
    ASSIGN
      plSum            = plSum            + tmpKort_Spes.Belop
      .

    IF LAST-OF(tmpKort_Spes.KortType) THEN
    DO:

      IF piRappType = 1 THEN /* Finansrapport */
      DO:
          ASSIGN
              lKortSum = lKortSum + plSum
              .
          PUT UNFORMATTED 
              "<C5>+<C6>" + entry(tmpKort_Spes.KortType,pcLabel,CHR(1))
              "<C16><RIGHT=C+6>" + STRING(tmpKort_Spes.AntKort,"->>,>>9")    +
              "<C24><RIGHT=C+11>" + STRING(plSum,"->>>,>>>,>>9.99")
              "<C52>-" SKIP
              .
      END.
      ELSE IF piRappType = 2 THEN /* Bokføringsbilag. */
          PUT UNFORMATTED 
              "<C6>" +  entry(tmpKort_Spes.KortType,pcLabel,CHR(1))
              "<C30>" + entry(tmpKort_Spes.KortType,pcKonto,CHR(1))
              "<C40><RIGHT=C+11>" + STRING(plSum,"->>>,>>>,>>9.99")
              "<C52>-" SKIP
              .
    END.
  END. /* KORTSPES */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE KortSpesFinans C-Win 
PROCEDURE KortSpesFinans :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:     BRUKES AV BÅDE BOKFØRINGSBILAG OG FINANSRAPPORT.
  
  1  *  Bankkort
  2  *  Bankkort (Postbanken)
  3     Visa
  4     Eurocard
  5     American express
  6     Diners
  7     Coop kort
  8     Multikort
  9  *  Bankkort (GE Capital)
 10  *  Bankkort (Gjensidige bank)
 11     JCB
 12     Trumf
 13     Domino
 14     Maestro
 15     Lindexkortet
 16     IKAno
 17  *  Bankkort (DnB kort)
 18     Proton
 19     SH kortet
 20  *  Bankkort (COOP)
 21     BBL
 22     Gavekort Kjede
 23     Gavekort Senter
  
------------------------------------------------------------------------------*/
DEFINE VARIABLE pcOverskr AS CHARACTER  NO-UNDO.
DEFINE VARIABLE pcLabel   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE pcKonto   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE plSum     AS DEC        NO-UNDO.
DEFINE VARIABLE piLoop    AS INT        NO-UNDO.
DEFINE VARIABLE pcTekst1  AS CHAR       NO-UNDO.
DEFINE VARIABLE pcTekst2  AS CHAR       NO-UNDO.

  ASSIGN
      pcLabel  = FILL(CHR(1),999)
      pcKonto  = FILL(CHR(1),999)
      lKortSum = 0
      .
  /*
  /* Overstyrer default */
  DO piLoop = 1 TO 99:
      {syspara.i 20  3 piLoop pcTekst1}
      {syspar2.i 20  3 piLoop pcTekst2}
    ASSIGN
        ENTRY(piLoop,pcLabel,CHR(1)) = pcTekst1
        ENTRY(piLoop,pcKonto,CHR(1)) = pcTekst2
        pcTekst1 = ""
        pcTekst2 = ""
        .
  END.
  */
  /* Henter BBS korttabell fra syspara. */
  FOR EACH TransBeskr NO-LOCK WHERE
      TransBeskr.TTId = 52:
    ASSIGN
        ENTRY(TransBeskr.TBId,pcLabel,CHR(1)) = TransBeskr.Beskrivelse
        ENTRY(TransBeskr.TBId,pcKonto,CHR(1)) = "D 0000"
        .
  END.

  /* Leser alle kortspesifikasjoner */
  FOR EACH tmpKort_Spes NO-LOCK WHERE
    tmpKort_Spes.Dato      = tmpKas_Rap.Dato AND
    tmpKort_Spes.Butikk    = tmpKas_Rap.Butikk AND
    tmpKort_Spes.Kasse     = tmpKas_Rap.Kasse AND
    tmpKort_Spes.Z_Nummer  = tmpKas_Rap.Z_Nummer  AND
    tmpKort_Spes.Sortering = tmpKas_Rap.Sortering
    BREAK BY tmpKort_Spes.Dato
          BY tmpKort_Spes.Butikk
          BY tmpKort_Spes.Kasse
          BY tmpKort_Spes.KortType:

    /* Nullstiller */
    IF FIRST-OF(tmpKort_Spes.KortType) THEN
      plSum = 0.

    /* Sum for korttypen */
    ASSIGN
      plSum = plSum + tmpKort_Spes.Belop
      .

    IF LAST-OF(tmpKort_Spes.KortType) THEN
    DO:
        ASSIGN
            lKortSum = lKortSum + plSum
            .
        PUT UNFORMATTED 
            "<C5>+<C6>" + entry(tmpKort_Spes.KortType,pcLabel,CHR(1))
            "<C16><RIGHT=C+6>" + STRING(tmpKort_Spes.AntKort,"->>,>>9")    +
            "<C24><RIGHT=C+11>" + STRING(plSum,"->>>,>>>,>>9.99")
            "<C52>-" SKIP
            .
    END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MvaRegnskap C-Win 
PROCEDURE MvaRegnskap :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE pcOverskr AS CHARACTER  NO-UNDO.
DEFINE VARIABLE pcLabel   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE iCount    AS INTEGER    NO-UNDO.
DEFINE VARIABLE dMvaGrunnlag AS DECIMAL    NO-UNDO.
DEFINE VARIABLE dMvaBelop AS DECIMAL    NO-UNDO.


    /* Mva regnskap totalt varesalg */
    ASSIGN 
       pcOverskr = "Mvaregnskap" + CHR(1) + "Gruppe" + CHR(1) + "Grunnlag" + CHR(1) + "Beløp" + CHR(1) + "Sum"
       pcLabel   = "Totalt".
    
    PUT UNFORMATTED
        "<P12><R+1><C6><B>"  ENTRY(1,pcOverskr,CHR(1)) SKIP "</B><C6><FROM><C35><LINE>" SKIP
        "<P8>"
        "<C6><RIGHT=C+6>" ENTRY(2,pcOverskr,CHR(1))
        "<C13><RIGHT=C+7>" ENTRY(3,pcOverskr,CHR(1))
        "<C21><RIGHT=C+6>" ENTRY(4,pcOverskr,CHR(1))
        "<C28><RIGHT=C+7>" ENTRY(5,pcOverskr,CHR(1)) SKIP
        "<C6><FROM><C12><LINE><C13><FROM><C20><LINE><C21><FROM><C27><LINE><C28><FROM><C35><LINE>".
    DO iCount = 1 TO 10:
        IF tmpKas_rap.MvaGrunnlag[iCount] <> 0 THEN
            PUT UNFORMATTED "<C6><RIGHT=C+6>" STRING(tmpKas_rap.MvaGrp[iCount],">9")
                            "<C13><RIGHT=C+7>" STRING(tmpKas_rap.MvaGrunnlag[iCount],"->>>,>>>,>>9.99")
                            "<C21><RIGHT=C+6>" STRING(tmpKas_rap.MvaBelop[iCount],"->>>,>>>,>>9.99")
                            "<C28><RIGHT=C+7>" STRING(tmpKas_rap.MvaGrunnlag[iCount] + tmpKas_rap.MvaBelop[iCount],"->>>,>>>,>>9.99") SKIP.
        ASSIGN dMvaGrunnlag = dMvaGrunnlag + tmpKas_rap.MvaGrunnlag[iCount]
               dMvaBelop    = dMvaBelop    + tmpKas_rap.MvaBelop[iCount].
    END.
    PUT UNFORMATTED "<C13><FROM><C20><LINE><C21><FROM><C27><LINE><C28><FROM><C35><LINE>".
    PUT UNFORMATTED "<C6><RIGHT=C+6>" pcLabel 
        "<C13><RIGHT=C+7>" STRING(dMvaGrunnlag,"->>>,>>>,>>9.99")
        "<C21><RIGHT=C+6>" STRING(dMvaBelop,"->>>,>>>,>>9.99")
        "<C28><RIGHT=C+7>" STRING(dMvaGrunnlag + dMvaBelop,"->>>,>>>,>>9.99") SKIP.

    /* Mva regnskap kreditsalg */
    ASSIGN 
       dMvaGrunnlag = 0
       dMvaBelop    = 0
       pcOverskr    = "Mvaregnskap (Kreditsalg)" + CHR(1) + "Gruppe" + CHR(1) + "Grunnlag" + CHR(1) + "Beløp" + CHR(1) + "Sum"
       pcLabel      = "Totalt".
    
    PUT UNFORMATTED
        "<P12><R+1><C6><B>"  ENTRY(1,pcOverskr,CHR(1)) SKIP "</B><C6><FROM><C35><LINE>" SKIP
        "<P8>"
        "<C6><RIGHT=C+6>" ENTRY(2,pcOverskr,CHR(1))
        "<C13><RIGHT=C+7>" ENTRY(3,pcOverskr,CHR(1))
        "<C21><RIGHT=C+6>" ENTRY(4,pcOverskr,CHR(1))
        "<C28><RIGHT=C+7>" ENTRY(5,pcOverskr,CHR(1)) SKIP
        "<C6><FROM><C12><LINE><C13><FROM><C20><LINE><C21><FROM><C27><LINE><C28><FROM><C35><LINE>".
    DO iCount = 1 TO 10:
        IF tmpKas_rap.MvaKredGrunnlag[iCount] <> 0 THEN
            PUT UNFORMATTED "<C6><RIGHT=C+6>" STRING(tmpKas_rap.MvaKredGrp[iCount],">9")
                            "<C13><RIGHT=C+7>" STRING(tmpKas_rap.MvaKredGrunnlag[iCount],"->>>,>>>,>>9.99")
                            "<C21><RIGHT=C+6>" STRING(tmpKas_rap.MvaKredBelop[iCount],"->>>,>>>,>>9.99")
                            "<C28><RIGHT=C+7>" STRING(tmpKas_rap.MvaKredGrunnlag[iCount] + tmpKas_rap.MvaBelop[iCount],"->>>,>>>,>>9.99") SKIP.
        ASSIGN dMvaGrunnlag = dMvaGrunnlag + tmpKas_rap.MvaKredGrunnlag[iCount]
               dMvaBelop    = dMvaBelop    + tmpKas_rap.MvaKredBelop[iCount].
    END.
    PUT UNFORMATTED "<C13><FROM><C20><LINE><C21><FROM><C27><LINE><C28><FROM><C35><LINE>".
    PUT UNFORMATTED "<C6><RIGHT=C+6>" pcLabel 
        "<C13><RIGHT=C+7>" STRING(dMvaGrunnlag,"->>>,>>>,>>9.99")
        "<C21><RIGHT=C+6>" STRING(dMvaBelop,"->>>,>>>,>>9.99")
        "<C28><RIGHT=C+7>" STRING(dMvaGrunnlag + dMvaBelop,"->>>,>>>,>>9.99") SKIP.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NonSale1 C-Win 
PROCEDURE NonSale1 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
------------------------------------------------------------------------------*/

DEFINE VARIABLE pcLabel   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE pcKonto   AS CHARACTER  NO-UNDO.

  
ASSIGN
  pcLabel  = "Positiv NonSale" + CHR(1) + "Positiv NonSale"
  pcKonto  = "K 0000" + CHR(1) + "K 0000"
  .
  
FOR EACH tt_NON_Sale_spes WHERE
    tt_NON_Sale_spes.Butikk = INT(FI-Butiker) AND
    tt_NON_Sale_spes.Kasse  = -9999 AND 
    tt_NON_Sale_spes.Non_Sale_Type = 1 
    BREAK BY tt_NON_Sale_spes.Butikk
          BY tt_NON_Sale_spes.Kasse
          BY tt_NON_Sale_spes.Non_Sale_Type:

    IF LAST-OF(tt_NON_Sale_spes.Non_Sale_Type) THEN
    DO:
        PUT UNFORMATTED 
            "<C6>" +  ENTRY(tt_NON_Sale_spes.Non_Sale_Type,pcLabel,CHR(1))
            "<C30>" + ENTRY(tt_NON_Sale_spes.Non_Sale_Type,pcKonto,CHR(1))
            "<C40><RIGHT=C+11>" + STRING(tt_NON_Sale_spes.NON_SaleVerdi,"->>>,>>>,>>9.99")
            "<C52>+" SKIP
            .
    END.
END.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NonSale2 C-Win 
PROCEDURE NonSale2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
------------------------------------------------------------------------------*/

DEFINE VARIABLE pcLabel   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE pcKonto   AS CHARACTER  NO-UNDO.

  
ASSIGN
  pcLabel  = "Negativ NonSale" + CHR(1) + "Negativ NonSale"
  pcKonto  = "K 0000" + CHR(1) + "K 0000"
  .

FOR EACH tt_NON_Sale_spes WHERE
    tt_NON_Sale_spes.Butikk = INT(FI-Butiker) AND
    tt_NON_Sale_spes.Kasse  = -9999 AND 
    tt_NON_Sale_spes.Non_Sale_Type = 2
    BREAK BY tt_NON_Sale_spes.Butikk
          BY tt_NON_Sale_spes.Kasse
          BY tt_NON_Sale_spes.Non_Sale_Type:

    IF LAST-OF(tt_NON_Sale_spes.Non_Sale_Type) THEN
    DO:
        PUT UNFORMATTED 
            "<C6>" +  ENTRY(tt_NON_Sale_spes.Non_Sale_Type,pcLabel,CHR(1))
            "<C30>" + ENTRY(tt_NON_Sale_spes.Non_Sale_Type,pcKonto,CHR(1))
            "<C40><RIGHT=C+11>" + STRING(tt_NON_Sale_spes.NON_SaleVerdi,"->>>,>>>,>>9.99")
            "<C52>-" SKIP.
    END.
END.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Omsetning C-Win 
PROCEDURE Omsetning :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE pcOverskr AS CHARACTER  NO-UNDO.
DEFINE VARIABLE pcLabel AS CHARACTER  NO-UNDO.
ASSIGN pcOverskr = "Omsetning" + CHR(1) + "Antall" + CHR(1) + "Beløp".
ASSIGN pcLabel   = "Kontant" + CHR(1) + "Sjekk" + CHR(1) + "Bank" + CHR(1) + "Kredit" + CHR(1) + 
                   "Reserveløsning bank" + CHR(1) + "Gavekort inn" + CHR(1) + "Tilgode" + CHR(1) + 
                   "Utbetalt" + CHR(1) + "Innbetalt" + CHR(1) + "Kort" + CHR(1) + "Kupong1" + CHR(1) + "Kupong2" + CHR(1) + "Registrert omsetning" + CHR(1) +
                   "Depositum ut".
    
    PUT UNFORMATTED 
        "<P12><R10><C43><B>"  ENTRY(1,pcOverskr,CHR(1)) SKIP "</B><C43><FROM><C75><LINE>" SKIP
        "<P8><C56><RIGHT=C+6>"  ENTRY(2,pcOverskr,CHR(1))
        "<C64><RIGHT=C+11>"  ENTRY(3,pcOverskr,CHR(1)) SKIP
        "<C56><FROM><C62><LINE>" "<C64><FROM><C75><LINE>" 
        "<C43>" +  entry(1,pcLabel,CHR(1)) +
          "<C56><RIGHT=C+6>" + STRING(tmpKas_Rap.AntKontant,"->>,>>9") + "<C64><RIGHT=C+11>" + STRING(tmpKas_Rap.Kontant + tmpKas_Rap.Layaway_inn /*- tmpKas_Rap.LayAway_ut*/,"->>>,>>>,>>9.99") SKIP        
        "<C42>+<C43>" +  entry(2,pcLabel,CHR(1)) + "<C56><RIGHT=C+6>" + STRING(tmpKas_Rap.AntSjekk,"->>,>>9") + "<C64><RIGHT=C+11>" + STRING(tmpKas_Rap.Sjekk,"->>>,>>>,>>9.99") SKIP
/* 16.12        "<C42>+<C43>" +  entry(3,pcLabel,chr(1)) + "<C64><RIGHT=C+11>" + STRING(tmpKas_Rap.Bank - Cashback,"->>>,>>>,>>9.99") SKIP */
/* 16.12  "<C42>+<C43>" +  entry(3,pcLabel,chr(1)) + "<C64><RIGHT=C+11>" + STRING(tmpKas_Rap.Bank + Cashback,"->>>,>>>,>>9.99") SKIP */
/* 06.01 */ "<C42>+<C43>" +  entry(3,pcLabel,CHR(1)) + "<C64><RIGHT=C+11>" + STRING(tmpKas_Rap.Bank,"->>>,>>>,>>9.99") SKIP
        "<C42>+<C43>" +  entry(4,pcLabel,CHR(1)) + "<C56><RIGHT=C+6>" + STRING(tmpKas_Rap.AntKredit,"->>,>>9") + "<C64><RIGHT=C+11>" + STRING(tmpKas_Rap.Kredit,"->>>,>>>,>>9.99") SKIP
        "<C42>+<C43>" +  entry(5,pcLabel,CHR(1)) + "<C56><RIGHT=C+6>" + STRING(tmpKas_Rap.AntReservelosning,"->>,>>9") + "<C64><RIGHT=C+11>" + STRING(tmpKas_Rap.Reservelosning,"->>>,>>>,>>9.99") SKIP
        "<C42>-<C43>" +  "Gavekort ut" + "<C64><RIGHT=C+11>" + STRING(tmpKas_Rap.GavekortUt,"->>>,>>>,>>9.99") SKIP
        "<C42>+<C43>" +  entry(6,pcLabel,CHR(1)) + "<C64><RIGHT=C+11>" + STRING(tmpKas_Rap.GavekortInn,"->>>,>>>,>>9.99") SKIP
        "<C42>+<C43>" +  entry(7,pcLabel,CHR(1)) + "<C56><RIGHT=C+6>" + STRING(tmpKas_Rap.AntTilgode,"->>,>>9") + "<C64><RIGHT=C+11>" + STRING(tmpKas_Rap.Tilgode,"->>>,>>>,>>9.99") SKIP
        "<C42>+<C43>" +  entry(8,pcLabel,CHR(1)) + "<C56><RIGHT=C+6>" + STRING(tmpKas_Rap.Antkont_ut,"->>,>>9") + "<C64><RIGHT=C+11>" + STRING(tmpKas_Rap.kont_ut,"->>>,>>>,>>9.99") SKIP
        "<C42>-<C43>" +  entry(9,pcLabel,CHR(1)) + "<C56><RIGHT=C+6>" + STRING(tmpKas_Rap.Antkont_in,"->>,>>9") + "<C64><RIGHT=C+11>" + STRING(tmpKas_Rap.kont_in,"->>>,>>>,>>9.99") SKIP

        "<C42>-<C43>" +  entry(14,pcLabel,CHR(1)) + "<C56><RIGHT=C+6>" + STRING(tmpKas_Rap.Antlayaway_Ut,"->>,>>9") + "<C64><RIGHT=C+11>" + STRING(tmpKas_Rap.layaway_Ut,"->>>,>>>,>>9.99") SKIP

        "<C42>+<C43>" +  entry(10,pcLabel,CHR(1)) + "<C56><RIGHT=C+6>" + STRING(tmpKas_Rap.AntKort,"->>,>>9") + "<C64><RIGHT=C+11>" + STRING(tmpKas_Rap.Kort,"->>>,>>>,>>9.99") SKIP
        "<C42>+<C43>" +  entry(11,pcLabel,CHR(1)) + "<C56><RIGHT=C+6>" + STRING(tmpKas_Rap.AntKupong1,"->>,>>9") + "<C64><RIGHT=C+11>" + STRING(tmpKas_Rap.Kupong1,"->>>,>>>,>>9.99") SKIP
        "<C42>+<C43>" +  entry(12,pcLabel,CHR(1)) + "<C56><RIGHT=C+6>" + STRING(tmpKas_Rap.AntKupong2,"->>,>>9") + "<C64><RIGHT=C+11>" + STRING(tmpKas_Rap.Kupong2,"->>>,>>>,>>9.99") SKIP
        "<C43><FROM><C75><LINE>"
        "<C43>" +  entry(13,pcLabel,CHR(1))
        "<C64><RIGHT=C+11>" + STRING(tmpKas_Rap.Kontant + 
                                     tmpKas_Rap.Sjekk + 
                                     tmpKas_Rap.Bank /* - tmpKas_Rap.Cashback */ + 
                                     tmpKas_Rap.Kredit + 
                                     tmpKas_Rap.Reservelosning + 
                                     tmpKas_Rap.GavekortInn - 
                                     tmpKas_Rap.GavekortUt +
                                     tmpKas_Rap.Tilgode + 
                                     tmpKas_Rap.Kont_Ut - 
                                     (tmpKas_Rap.Kont_Inn) +
                                     tmpKas_Rap.Kort + 
                                     tmpKas_Rap.Kupong1 + 
                                     tmpKas_Rap.Kupong2  +
                                     tmpKas_Rap.Layaway_inn - 
                                     tmpKas_Rap.LayAway_ut,"->>>,>>>,>>9.99")
                                     SKIP
        "<C43><FROM><C75><LINE>" SKIP.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SummerKortSpes C-Win 
PROCEDURE SummerKortSpes :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
      ASSIGN tmpKort_Spes.AntKort  = tmpKort_Spes.AntKort + Kort_Spes.AntKort
             tmpKort_Spes.Belop    = tmpKort_Spes.Belop   + Kort_Spes.Belop.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SummerPost C-Win 
PROCEDURE SummerPost :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:      
------------------------------------------------------------------------------*/
              ASSIGN
                tmpKas_Rap.Kontant           =  tmpKas_Rap.Kontant       + Kas_Rap.Kontant
                tmpKas_Rap.Sjekk             =  tmpKas_Rap.Sjekk         + Kas_Rap.Sjekk
                tmpKas_Rap.Kort              =  tmpKas_Rap.Kort          + Kas_Rap.Kort
                tmpKas_Rap.Kredit            =  tmpKas_Rap.Kredit        + Kas_Rap.Kredit
                tmpKas_Rap.Kupong1           =  tmpKas_Rap.Kupong1       + Kas_Rap.Kupong1
                tmpKas_Rap.Kupong2           =  tmpKas_Rap.Kupong2       + Kas_Rap.Kupong2
                tmpKas_Rap.Tilgode           =  tmpKas_Rap.Tilgode       + Kas_Rap.Tilgode
                tmpKas_Rap.Layaway_inn       =  tmpKas_Rap.Layaway_inn   + Kas_Rap.Layaway_inn
                tmpKas_Rap.Layaway_ut        =  tmpKas_Rap.Layaway_ut    + Kas_Rap.Layaway_ut
                tmpKas_Rap.Kont_inn          =  tmpKas_Rap.Kont_inn      + Kas_Rap.Kont_inn
                tmpKas_Rap.Kont_ut           =  tmpKas_Rap.Kont_ut       + Kas_Rap.Kont_ut
                tmpKas_Rap.Gavekort          =  tmpKas_Rap.Gavekort      + Kas_Rap.Gavekort

                tmpKas_Rap.Rekvisisasjon     =  tmpKas_Rap.Rekvisisasjon + Kas_Rap.Rekvisisasjon
                tmpKas_Rap.Pant              =  tmpKas_Rap.Pant          + Kas_Rap.Pant
                tmpKas_Rap.Bank              =  tmpKas_Rap.Bank          + Kas_Rap.Bank
                tmpKas_Rap.Dropp             =  tmpKas_Rap.Dropp         + Kas_Rap.Dropp
                tmpKas_Rap.AntVaremottak     =  tmpKas_Rap.AntVaremottak     + Kas_Rap.AntVaremottak    
                tmpKas_Rap.AntLagerjustering =  tmpKas_Rap.AntLagerjustering + Kas_Rap.AntLagerjustering
                tmpKas_Rap.AntBrekkasje      =  tmpKas_Rap.AntBrekkasje      + Kas_Rap.AntBrekkasje     
                tmpKas_Rap.AntInterntForbruk =  tmpKas_Rap.AntInterntForbruk + Kas_Rap.AntInterntForbruk
                tmpKas_Rap.Varemottak        =  tmpKas_Rap.Varemottak        + Kas_Rap.Varemottak    
                tmpKas_Rap.Lagerjustering    =  tmpKas_Rap.Lagerjustering    + Kas_Rap.Lagerjustering
                tmpKas_Rap.Brekkasje         =  tmpKas_Rap.Brekkasje         + Kas_Rap.Brekkasje     
                tmpKas_Rap.InterntForbruk    =  tmpKas_Rap.InterntForbruk    + Kas_Rap.InterntForbruk
                .

              ASSIGN
                tmpKas_Rap.Overfort          = tmpKas_Rap.Overfort          + Kas_Rap.Overfort
                tmpKas_Rap.CashBack          = tmpKas_Rap.CashBack          + Kas_Rap.CashBack
                tmpKas_Rap.Veksel            = tmpKas_Rap.Veksel            + Kas_Rap.Veksel
                tmpKas_Rap.Avrunding         = tmpKas_Rap.Avrunding         + Kas_Rap.Avrunding
                tmpKas_Rap.Reklamasjon       = tmpKas_Rap.Reklamasjon       + Kas_Rap.Reklamasjon
                tmpKas_Rap.Retur             = tmpKas_Rap.Retur             + Kas_Rap.Retur
                tmpKas_Rap.InnbetaltKunde    = tmpKas_Rap.InnbetaltKunde    + Kas_Rap.InnbetaltKunde
                tmpKas_Rap.Medlemssalg       = tmpKas_Rap.Medlemssalg       + Kas_Rap.Medlemssalg
                tmpKas_Rap.AntCashBack       = tmpKas_Rap.AntCashBack       + Kas_Rap.AntCashBack
                tmpKas_Rap.AntMedlemssalg    = tmpKas_Rap.AntMedlemssalg    + Kas_Rap.AntMedlemssalg
                tmpKas_Rap.AntInnbetaltKunde = tmpKas_Rap.AntInnbetaltKunde + Kas_Rap.AntInnbetaltKunde
                tmpKas_Rap.AntRetur          = tmpKas_Rap.AntRetur          + Kas_Rap.AntRetur
                tmpKas_Rap.AntKontant        = tmpKas_Rap.AntKontant        + Kas_Rap.AntKontant
                tmpKas_Rap.AntSjekk          = tmpKas_Rap.AntSjekk          + Kas_Rap.AntSjekk
                tmpKas_Rap.SjekkBeholdning   = tmpKas_Rap.SjekkBeholdning   + Kas_Rap.Sjekk
                tmpKas_Rap.AntKort           = tmpKas_Rap.AntKort           + Kas_Rap.AntKort
                tmpKas_Rap.AntKredit         = tmpKas_Rap.AntKredit         + Kas_Rap.AntKredit
                tmpKas_Rap.AntKupong1        = tmpKas_Rap.AntKupong1        + Kas_Rap.AntKupong1
                tmpKas_Rap.AntKupong2        = tmpKas_Rap.AntKupong2        + Kas_Rap.AntKupong2
                tmpKas_Rap.AntTilgode        = tmpKas_Rap.AntTilgode        + Kas_Rap.AntTilgode
                tmpKas_Rap.AntBank           = tmpKas_Rap.AntBank           + Kas_Rap.AntBank.

              ASSIGN
                tmpKas_Rap.GaveKortRabatt    = tmpKas_Rap.GaveKortRabatt    + Kas_Rap.GaveKortRabatt
                tmpKas_Rap.AntGaveKortRabUt  = tmpKas_Rap.AntGaveKortRabUt  + Kas_Rap.AntGaveKortRabUt
                tmpKas_Rap.AntGavekort       = tmpKas_Rap.AntGavekort       + Kas_Rap.AntGavekort
                tmpKas_Rap.AntRekvisisjon    = tmpKas_Rap.AntRekvisisjon    + Kas_Rap.AntRekvisisjon
                tmpKas_Rap.AntVeksel         = tmpKas_Rap.AntVeksel         + Kas_Rap.AntVeksel
                tmpKas_Rap.AntAvrunding      = tmpKas_Rap.AntAvrunding      + Kas_Rap.AntAvrunding
                tmpKas_Rap.AntDropp          = tmpKas_Rap.AntDropp          + Kas_Rap.AntDropp
                tmpKas_Rap.AntOverfort       = tmpKas_Rap.AntOverfort       + Kas_Rap.AntOverfort
                tmpKas_Rap.AntKont_Inn       = tmpKas_Rap.AntKont_Inn       + Kas_Rap.AntKont_Inn
                tmpKas_Rap.AntKont_Ut        = tmpKas_Rap.AntKont_Ut        + Kas_Rap.AntKont_Ut
                tmpKas_Rap.AntLayAway_Inn    = tmpKas_Rap.AntLayAway_Inn    + Kas_Rap.AntLayAway_Inn
                tmpKas_Rap.AntLayAway_Ut     = tmpKas_Rap.AntLayAway_Ut     + Kas_Rap.AntLayAway_Ut
                tmpKas_Rap.AntReturer        = tmpKas_Rap.AntReturer        + Kas_Rap.AntReturer
                tmpKas_Rap.TilgodeInn        = tmpKas_Rap.TilgodeInn        + Kas_Rap.TilgodeInn
                tmpKas_Rap.TilgodeAndre      = tmpKas_Rap.TilgodeAndre      + Kas_Rap.TilgodeAndre
                tmpKas_Rap.TilgodeUt         = tmpKas_Rap.TilgodeUt         + Kas_Rap.TilgodeUt
                tmpKas_Rap.AntTilgodeInn     = tmpKas_Rap.AntTilgodeInn     + Kas_Rap.AntTilgodeInn
                tmpKas_Rap.AntTilgodeUt      = tmpKas_Rap.AntTilgodeUt      + Kas_Rap.AntTilgodeUt
                tmpKas_Rap.AntTilgodeAndre   = tmpKas_Rap.AntTilgodeAndre   + Kas_Rap.AntTilgodeAndre
                tmpKas_Rap.GavekortUt        = tmpKas_Rap.GavekortUt        + Kas_Rap.GavekortUt
                tmpKas_Rap.GavekortInn       = tmpKas_Rap.GavekortInn       + Kas_Rap.GavekortInn
                tmpKas_Rap.GavekortAndreInn  = tmpKas_Rap.GavekortAndreInn  + Kas_Rap.GavekortAndreInn
                tmpKas_Rap.AntGavekortUt     = tmpKas_Rap.AntGavekortUt     + Kas_Rap.AntGavekortUt
                tmpKas_Rap.AntGavekortInn    = tmpKas_Rap.AntGavekortInn    + Kas_Rap.AntGavekortInn
                tmpKas_Rap.AntGavekortAndreInn = tmpKas_Rap.AntGavekortAndreInn + Kas_Rap.AntGavekortAndreInn
                tmpKas_Rap.Medlemsrabatt     = tmpKas_Rap.Medlemsrabatt     + Kas_Rap.Medlemsrabatt.

              ASSIGN
                tmpKas_Rap.Kunderabatt       = tmpKas_Rap.Kunderabatt       + Kas_Rap.Kunderabatt
                tmpKas_Rap.Personalrabatt    = tmpKas_Rap.Personalrabatt    + Kas_Rap.Personalrabatt
                tmpKas_Rap.GenerellRabatt    = tmpKas_Rap.GenerellRabatt    + Kas_Rap.GenerellRabatt
                tmpKas_Rap.AntPersonalrabatt = tmpKas_Rap.AntPersonalrabatt + Kas_Rap.AntPersonalrabatt
                tmpKas_Rap.AntMedlemsrabatt  = tmpKas_Rap.AntMedlemsrabatt  + Kas_Rap.AntMedlemsrabatt
                tmpKas_Rap.AntKunderabatt    = tmpKas_Rap.AntKunderabatt    + Kas_Rap.AntKunderabatt
                tmpKas_Rap.AntGenerellRabatt = tmpKas_Rap.AntGenerellRabatt + Kas_Rap.AntGenerellRabatt
                tmpKas_Rap.OverfortInn       = tmpKas_Rap.OverfortInn       + Kas_Rap.OverfortInn
                tmpKas_Rap.OverfortUt        = tmpKas_Rap.OverfortUt        + Kas_Rap.OverfortUt
                tmpKas_Rap.AntOverfortInn    = tmpKas_Rap.AntOverfortInn    + Kas_Rap.AntOverfortInn
                tmpKas_Rap.AntOverfortUt     = tmpKas_Rap.AntOverfortUt     + Kas_Rap.AntOverfortUt
                tmpKas_Rap.MvaGrp[ 1]        = Kas_Rap.MvaGrp[ 1]        
                tmpKas_Rap.MvaGrp[ 2]        = Kas_Rap.MvaGrp[ 2]        
                tmpKas_Rap.MvaGrp[ 3]        = Kas_Rap.MvaGrp[ 3]        
                tmpKas_Rap.MvaGrp[ 4]        = Kas_Rap.MvaGrp[ 4]        
                tmpKas_Rap.MvaGrp[ 5]        = Kas_Rap.MvaGrp[ 5]        
                tmpKas_Rap.MvaGrp[ 6]        = Kas_Rap.MvaGrp[ 6]        
                tmpKas_Rap.MvaGrp[ 7]        = Kas_Rap.MvaGrp[ 7]        
                tmpKas_Rap.MvaGrp[ 8]        = Kas_Rap.MvaGrp[ 8]        
                tmpKas_Rap.MvaGrp[ 9]        = Kas_Rap.MvaGrp[ 9].

              ASSIGN
                tmpKas_Rap.MvaGrp[10]        = Kas_Rap.MvaGrp[10]
                tmpKas_Rap.MvaGrunnlag[ 1]   = tmpKas_Rap.MvaGrunnlag[ 1]   + Kas_Rap.MvaGrunnlag[ 1]
                tmpKas_Rap.MvaGrunnlag[ 2]   = tmpKas_Rap.MvaGrunnlag[ 2]   + Kas_Rap.MvaGrunnlag[ 2]
                tmpKas_Rap.MvaGrunnlag[ 3]   = tmpKas_Rap.MvaGrunnlag[ 3]   + Kas_Rap.MvaGrunnlag[ 3]
                tmpKas_Rap.MvaGrunnlag[ 4]   = tmpKas_Rap.MvaGrunnlag[ 4]   + Kas_Rap.MvaGrunnlag[ 4]
                tmpKas_Rap.MvaGrunnlag[ 5]   = tmpKas_Rap.MvaGrunnlag[ 5]   + Kas_Rap.MvaGrunnlag[ 5]
                tmpKas_Rap.MvaGrunnlag[ 6]   = tmpKas_Rap.MvaGrunnlag[ 6]   + Kas_Rap.MvaGrunnlag[ 6]
                tmpKas_Rap.MvaGrunnlag[ 7]   = tmpKas_Rap.MvaGrunnlag[ 7]   + Kas_Rap.MvaGrunnlag[ 7]
                tmpKas_Rap.MvaGrunnlag[ 8]   = tmpKas_Rap.MvaGrunnlag[ 8]   + Kas_Rap.MvaGrunnlag[ 8]
                tmpKas_Rap.MvaGrunnlag[ 9]   = tmpKas_Rap.MvaGrunnlag[ 9]   + Kas_Rap.MvaGrunnlag[ 9]
                tmpKas_Rap.MvaGrunnlag[10]   = tmpKas_Rap.MvaGrunnlag[10]   + Kas_Rap.MvaGrunnlag[10]
                tmpKas_Rap.MvaBelop[ 1]      = tmpKas_Rap.MvaBelop[ 1]      + Kas_Rap.MvaBelop[ 1]
                tmpKas_Rap.MvaBelop[ 2]      = tmpKas_Rap.MvaBelop[ 2]      + Kas_Rap.MvaBelop[ 2]
                tmpKas_Rap.MvaBelop[ 3]      = tmpKas_Rap.MvaBelop[ 3]      + Kas_Rap.MvaBelop[ 3]
                tmpKas_Rap.MvaBelop[ 4]      = tmpKas_Rap.MvaBelop[ 4]      + Kas_Rap.MvaBelop[ 4]
                tmpKas_Rap.MvaBelop[ 5]      = tmpKas_Rap.MvaBelop[ 5]      + Kas_Rap.MvaBelop[ 5]
                tmpKas_Rap.MvaBelop[ 6]      = tmpKas_Rap.MvaBelop[ 6]      + Kas_Rap.MvaBelop[ 6]
                tmpKas_Rap.MvaBelop[ 7]      = tmpKas_Rap.MvaBelop[ 7]      + Kas_Rap.MvaBelop[ 7]
                tmpKas_Rap.MvaBelop[ 8]      = tmpKas_Rap.MvaBelop[ 8]      + Kas_Rap.MvaBelop[ 8]
                tmpKas_Rap.MvaBelop[ 9]      = tmpKas_Rap.MvaBelop[ 9]      + Kas_Rap.MvaBelop[ 9].

              ASSIGN
                tmpKas_Rap.MvaBelop[10]      = tmpKas_Rap.MvaBelop[10]      + Kas_Rap.MvaBelop[10]
                tmpKas_Rap.AntReklamasjoner  = tmpKas_Rap.AntReklamasjoner  + Kas_Rap.AntReklamasjoner
                tmpKas_Rap.Reservelosning    = tmpKas_Rap.Reservelosning    + Kas_Rap.Reservelosning
                tmpKas_Rap.AntReservelosning = tmpKas_Rap.AntReservelosning + Kas_Rap.AntReservelosning
                tmpKas_Rap.AntPakkerabatt    = tmpKas_Rap.AntPakkerabatt    + Kas_Rap.AntPakkerabatt
                tmpKas_Rap.Pakkerabatt       = tmpKas_Rap.Pakkerabatt       + Kas_Rap.Pakkerabatt
                tmpKas_Rap.KontantBeholdning = tmpKas_Rap.KontantBeholdning + Kas_Rap.KontantBeholdning
                tmpKas_Rap.VekselBeholdning  = tmpKas_Rap.VekselBeholdning  + Kas_Rap.VekselBeholdning
                tmpKas_Rap.AntallUtbetBonger = tmpKas_Rap.AntallUtbetBonger + iAntallUtbetBonger
                tmpKas_Rap.VerdiUtbetBonger  = tmpKas_Rap.VerdiUtbetBonger  + lVerdiUtbetBonger
                .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE TilgodeAndreList C-Win 
PROCEDURE TilgodeAndreList :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cbutnamn AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cIdentNr AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cSelger AS CHARACTER   NO-UNDO.
/*   DEFINE FRAME fPageHeaderTG                                                          */
/*      HEADER                                                                           */
/*         "<ALIGN=BASE><FArial>"                                                        */
/*         "<R4><P20><B><C5.5>" cTittel "<P12></B>" SKIP                                 */
/*         "<R5.5><P10><B><C6>" cSubTittel1 "<P10></B><P10>" SKIP                        */
/*         "<P8><C6>" cSubTittel2 "<P8><C74>" PAGE-NUMBER FORMAT ">>>" "/ <#Pages>" SKIP */
/*         "<R7.5><C6><FROM><R7.5><C78><LINE>" SKIP                                      */
/*         WITH PAGE-TOP STREAM-IO WIDTH 255.                                            */
    cTittel = "Tilgode fra andre butikker".
    cSubTittel1 = "".
  VIEW FRAME fPageHeader.
/*   DEFINE FRAME fPageFooter                                                                */
/*      HEADER                                                                               */
/*         "<R62><C6><FROM><C78><LINE>" SKIP                                                 */
/*         "<P8><C6>" STRING(TODAY) " " STRING(TIME,"HH:MM:SS") "<CENTER=C80>" cFirma "<P8>" */
/*         WITH PAGE-BOTTOM STREAM-IO WIDTH 255.                                             */
  VIEW FRAME fPAgeFooter.

  DEFINE VARIABLE pcOverskr AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE pcLabel   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE pcKonto   AS CHARACTER  NO-UNDO.
  DEFINE VAR      piLoop    AS INT        NO-UNDO.
  DEFINE VAR      pcBank    AS CHAR       NO-UNDO.
  DEFINE VAR      piInt     AS INT        NO-UNDO.
  DEFINE VAR      pcTekst   AS CHAR       NO-UNDO.
  DEFINE VARIABLE iRad      AS INTEGER     NO-UNDO.
  DEFINE BUFFER bufBut FOR butiker.
  ASSIGN
    pcOverskr = "Butikk" + CHR(1) + 
                "Identnr" + CHR(1) + 
                "Beløp" + CHR(1) +
                "Kasse"   + CHR(1) +
                "Bongnr" + CHR(1) + 
                "Selger".
  FIND bufBut WHERE bufBut.butik = INT(ENTRY(1,ipButikLst)) NO-ERROR.

  PUT UNFORMATTED 
      "<P12><R8><C6><B>Butikk " bufBut.Butik " " bufBut.ButNamn " " "<P8></B>"
  SKIP.
      PUT UNFORMATTED 
          "<P8><R10><C6><B>"  ENTRY(1,pcOverskr,CHR(1)) 
          "<P8><C22>"  ENTRY(2,pcOverskr,CHR(1))
          "<C43><RIGHT=C+11>"  ENTRY(3,pcOverskr,CHR(1)) 
          "<C55><RIGHT=C+4>"  ENTRY(4,pcOverskr,CHR(1)) 
          "<C60><RIGHT=C+4>"  ENTRY(5,pcOverskr,CHR(1)) 
          "<C65>"             ENTRY(6,pcOverskr,CHR(1)) 
          SKIP  

          "<C6><FROM><C21><LINE>" 
          "<C22><FROM><C47><LINE>" 
          "<C48><FROM><C54><LINE>" 
          "<C55><FROM><C59><LINE>" 
          "<C60><FROM><C64><LINE>" 
          "<C65><FROM><C75><LINE>" 
          "</B>"
          SKIP.

/*     OUTPUT TO "C:\tmp\tilgode.txt". */
    iRad = 10.
    FOR EACH tt_tilgode NO-LOCK BREAK BY tt_tilgode.butnr.
        FIND butiker  WHERE butiker.butik = tt_tilgode.butnr NO-LOCK NO-ERROR.
        IF AVAIL butiker THEN
            cbutnamn = butiker.butnamn.
        ELSE DO:
            FIND ekstbutiker WHERE ekstbutiker.Butik = tt_tilgode.butnr NO-LOCK NO-ERROR.
                IF AVAIL ekstbutiker THEN
                    cbutnamn = ekstbutiker.ButNamn.
        END.
        IF cButnamn = "" THEN
            cButnamn = "Ukjent".
        cIdentNr = tt_tilgode.IdentNr.
        FIND butikkselger WHERE ButikkSelger.butikknr = bufBut.butik AND ButikkSelger.SelgerId = tt_Tilgode.BruktSelgerNr NO-LOCK NO-ERROR.
        IF AVAIL butikkSelger THEN
            FIND selger OF butikkselger NO-LOCK NO-ERROR.
        ELSE
            RELEASE selger.
        cSelger = IF AVAIL selger THEN Selger.Navn ELSE "".
        IF LENGTH(cIdentNr) > 40 THEN
            cIdentNr = SUBSTR(cIdentNr,1,39) + "..".
        iRad = iRad + 1.
        PUT UNFORMATTED 
        "<R" STRING(iRad) ">"
        "<C6>" STRING(cButnamn,"x(20)")
        "<C22>" cIdentNr
                            /*STRING(tmpKas_rap.MvaGrp[piLoop],">9")*/
        "<C43><RIGHT=C+11>" STRING(tt_tilgode.belop,"->>>,>>9.99")
/*         "<C52>+" */
        "<C55><RIGHT=C+4>" STRING(tt_tilgode.Bruktkassenr)
        "<C60><RIGHT=C+4>" STRING(tt_tilgode.BruktBongNr)
        "<C65>"            cSelger
         SKIP.
        IF iRad = 60 AND NOT LAST(tt_tilgode.butnr) THEN DO:
            iRad = 10.
            PAGE.
            PUT UNFORMATTED 
                "<P12><R8><C6><B>Butikk " bufBut.Butik " " bufBut.ButNamn " " "<P8></B>"
            SKIP.
            PUT UNFORMATTED 
                "<P8><R10><C6><B>"  ENTRY(1,pcOverskr,CHR(1)) 
                "<P8><C22>"  ENTRY(2,pcOverskr,CHR(1))
                "<C43><RIGHT=C+11>"  ENTRY(3,pcOverskr,CHR(1)) 
                "<C55><RIGHT=C+4>"  ENTRY(4,pcOverskr,CHR(1)) 
                "<C60><RIGHT=C+4>"  ENTRY(5,pcOverskr,CHR(1)) 
                SKIP  

                "<C6><FROM><C21><LINE>" 
                "<C22><FROM><C47><LINE>" 
                "<C48><FROM><C54><LINE>" 
                "<C55><FROM><C59><LINE>" 
                "<C60><FROM><C64><LINE>" 
                "</B>"
                SKIP.
        END.
    END.
    PAGE.
/*     OUTPUT CLOSE. */
    EMPTY TEMP-TABLE tt_tilgode.
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
/*     ASSIGN FI-Tildato = INPUT FI-FraDato. /* detta gäller version 1  */ */
  IF INPUT FI-FraDato >
     INPUT FI-TilDato THEN
  DO:
    MESSAGE "FraDato er større enn TilDato!"
      VIEW-AS ALERT-BOX MESSAGE TITLE "Melding".
    RETURN "AVBRYT".
  END.
  ELSE IF INPUT FI-FraDato = ? OR INPUT FI-TilDato = ? THEN
  DO:
    MESSAGE "Både fra og til dato må angis!"
      VIEW-AS ALERT-BOX MESSAGE TITLE "Melding".
    RETURN "AVBRYT".
  END.
/*   ELSE IF (INPUT T-Accum:CHECKED AND INPUT T-Kasse  = FALSE AND */
/*       INPUT T-Butikk = FALSE AND                                */
/*       INPUT T-Total  = FALSE) THEN                              */
/*       MESSAGE "Det er ikke valgt totaler."                      */
/*           VIEW-AS ALERT-BOX INFO BUTTONS OK TITLE "Melding".    */
/*   ELSE ipStatus = "OK".                                         */

END.

/* RETURN ipStatus. */
RETURN "OK".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Varesalg C-Win 
PROCEDURE Varesalg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE pcOverskr AS CHARACTER  NO-UNDO.
DEFINE VARIABLE pcLabel AS CHARACTER  NO-UNDO.
DEFINE VARIABLE wNettoOmsetning AS DECIMAL    NO-UNDO.
ASSIGN pcOverskr = "Varesalg" + CHR(1) + "Beløp".
ASSIGN pcLabel   = "Registrert omsetning" + CHR(1) + "Avrunding" + CHR(1) + "Brutto vareslag".
       wNettoOmsetning = wBruttoOmsetning + ((tmpKas_rap.Retur + tmpKas_Rap.Reklamasjon) * -1) - GenerellRabatt - Kunderabatt - Personalrabatt -  Medlemsrabatt - Pakkerabatt.
    PUT UNFORMATTED 
        "<P12><R24><C6><B>"  ENTRY(1,pcOverskr,CHR(1)) SKIP "</B><C6><FROM><C35><LINE>" SKIP
        "<P8><C24><RIGHT=C+11>"  ENTRY(2,pcOverskr,CHR(1)) SKIP
        "<C24><FROM><C35><LINE>"
        "<C6>" +  entry(1,pcLabel,CHR(1)) +
          "<C24><RIGHT=C+11>" + STRING(wNettoOmsetning - tmpKas_rap.Avrunding - tmpKas_Rap.GavekortUt,"->>>,>>>,>>9.99") SKIP        
        "<C5>+<C6>" +  entry(2,pcLabel,CHR(1)) +
          "<C24><RIGHT=C+11>" + STRING(tmpKas_Rap.Avrunding,"->>>,>>>,>>9.99") SKIP
        "<C6><FROM><C35><LINE>"
        "<C5>=<C6>" +  entry(3,pcLabel,CHR(1))
        "<C24><RIGHT=C+11>" + STRING(wNettoOmsetning,"->>>,>>>,>>9.99") SKIP
/* ???     "<C24><RIGHT=C+11>" + STRING(wNettoOmsetning - Avrunding + Avrunding,"->>>,>>>,>>9.99") SKIP */
        "<C6><FROM><C35><LINE>" SKIP.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE XPrintBilag C-Win 
PROCEDURE XPrintBilag :
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
  DEF VAR piRad        AS DEC    NO-UNDO.

  DEF VAR iRad         AS INTE INIT 10 NO-UNDO.
  DEF VAR iButik       AS INTE INIT ? NO-UNDO.
  DEF VAR iKasse       AS INTE INIT ? NO-UNDO.
  DEF VAR cRader       AS CHAR        NO-UNDO.
  DEF VAR cCols        AS CHAR        NO-UNDO.
  DEF VAR cUkeDar      AS CHAR        NO-UNDO.

  DEF VAR iSolgtAnt    AS INTE    NO-UNDO.
  DEF VAR dSolgtVerdi  AS DECI    NO-UNDO.
  DEF VAR iAntKunder   AS INTE    NO-UNDO.

  DEF VAR pcLabel      AS CHAR    NO-UNDO.
  DEF VAR plOmsetning  AS DEC     NO-UNDO.
  DEF VAR pcOverskr    AS CHAR    NO-UNDO.
  DEFINE VARIABLE wOK  AS LOGICAL    NO-UNDO.
  DEFINE BUFFER btmpKas_Rap FOR tmpKas_Rap.
  DEFINE VARIABLE cPrinter AS CHARACTER  NO-UNDO.

  /* Initierer tekstene på rapporten */
  ASSIGN
      lDagensKontStrom = 0
      lKasseEndring    = 0
      lKasseSlutt      = 0
      cBokfNr          = "Ikke godkjent"
    .
  ASSIGN
      cTittel     = "Bokføringsbilag"
      cSubTittel1 = "Testbutikk AS"
      cSubTittel2 = "Kriterier" 
      cFirma      = "SkoTex/Test"
      cKrit1      = "Butikker:"
      cKrit2      = "Dato:"
      pcOverskr   = "Butikk" + CHR(1) + 
                    "Kasse" + CHR(1) +
                    "Dato" + CHR(1) +
                    "Konto" + CHR(1) + 
                    "Beløp" + CHR(1) + 
                    "Oppgjør" + CHR(1) + 
                    "Betalt med:" + CHR(1) + 
                    "Bankkort" + CHR(1) + 
                    "Tilgodesedler" + CHR(1) + 
                    "Gavekort egne" + CHR(1) +
                    "Gavekort universal" + CHR(1) +          
                    "Utstedt:" + CHR(1) + 
                    "Tilgodesedler" + CHR(1) + 
                    "Gavekort egne" + CHR(1) + 
                    "Kreditsalg:" + CHR(1) +
                    "Fakturert" + CHR(1) +
                    "Innbetalt kontant" + CHR(1) +
                    "Utbetalinger (Tekstes)" + CHR(1) +
                    "Utbetalt" + CHR(1) +
                    "Dagens kontantstrøm" + CHR(1) +
                    "Kasse ved dagens begynnelse" + CHR(1) +
                    "Innskudd bank" + CHR(1) +
                    "Kasse ved dagens slutt, opptalt" + CHR(1) +
                    "Endring kasse" + CHR(1) +
                    "Differanse" + CHR(1) +
                    "Kassereroppgjør utført av:" + CHR(1) +
                    FILL(CHR(1),50)
      .
{syspara.i  1 1 100 cSubTittel1}
{syspara.i  1 1 101 cFirma}
{syspara.i 20 4   1 cTittel}
{syspara.i 20 4   2 cSubTittel2}
{syspara.i 20 4   3 cKrit1}
{syspara.i 20 4   4 cKrit2}

  /* Overstyrer default */
  DO piLoop = 5 TO 50:
      {syspara.i 20  4 piLoop pcTekst}
    ASSIGN
        ENTRY(piLoop,pcOverskr,CHR(1)) = pcTekst
        pcTekst = ""
        .
  END.
 DO WITH FRAME {&FRAME-NAME}:
  ASSIGN
    cSubtittel2     = cSubTittel2 + " " + 
                      cKrit1 + " " + INPUT FI-Butiker + ", " + 
      cKrit2 + " " + STRING(INPUT FI-FraDato) + " - " + STRING(FI-TilDato)
/*                       cKrit2 + " " + STRING(INPUT FI-FraDato) + " - " + STRING(INPUT FI-TilDato)  */
    .

  ASSIGN
    piCopies = 1
    .
 STATUS DEFAULT "Skriver ut, venligst vent...".


 ASSIGN
     lNonSale1 = 0
     lNonSale2 = 0.
 FOR EACH tt_NON_Sale_spes WHERE
     tt_NON_Sale_spes.Butikk = INT(FI-Butiker) AND
     tt_NON_Sale_spes.Kasse  = -9999:

     IF tt_NON_Sale_spes.Non_Sale_Type = 1 THEN
         lNonSale1 = lNonSale1 + tt_NON_Sale_spes.NON_SaleVerdi. 
     IF tt_NON_Sale_spes.Non_Sale_Type = 2 THEN
         lNonSale2 = lNonSale2 + tt_NON_Sale_spes.NON_SaleVerdi. 
 END.

  /* Henter tempfilnavn */
/*  RUN GetTmpFileName IN hLibHandle ("utk","xpr", OUTPUT pcRappFil). */
  IF VALID-HANDLE(wLibHandle) THEN
    RUN GetTempFileName IN wLibHandle ("KasRap", "xpr", OUTPUT pcRappFil). 
   /* Åpner stream til skriverfil. */
  IF CAN-DO(SESSION:PARAMETER,"BATCH") THEN
      ASSIGN cPrinter = cButBatchPrinter.
  ELSE
      ASSIGN cPrinter = DYNAMIC-FUNCTION("getAttribute",SESSION,"SE_PRINTER").
  IF cPrinter = "" THEN DO:
      SYSTEM-DIALOG PRINTER-SETUP UPDATE wOK.
      IF NOT wOK THEN
          RETURN NO-APPLY.
      ASSIGN cPrinter = SESSION:PRINTER-NAME.
  END.
  OUTPUT TO VALUE(pcRappFil) PAGED PAGE-SIZE 255.
  IF NOT lDirekte THEN 
      PUT CONTROL '<PDF-OUTPUT=' + REPLACE(pcRappFil,"xpr","pdf") + '>'.
  PUT CONTROL     "<PRINTER" cPrinter ">".
  PUT CONTROL "<TRANSPARENT=true><UNITS=MM><LINECOLOR=BLACK><FGCOLOR=BLACK>".
  IF NOT lDirekte THEN 
      PUT CONTROL '<PREVIEW=ZoomToWidth>'.

  DEFINE FRAME fPageHeader
     HEADER
        "<ALIGN=BASE><FArial>"
        "<R4><P20><B><C5.5>" cTittel "<P12></B>" SKIP
        "<R5.5><P10><B><C6>" cSubTittel1 "<P10></B><P10>" SKIP
        "<P8><C6>" cSubTittel2 "<P8><C74>" PAGE-NUMBER FORMAT ">>>" "/ <#Pages>" SKIP
        "<R7.5><C6><FROM><R7.5><C78><LINE>" SKIP
        WITH PAGE-TOP STREAM-IO WIDTH 255.
  VIEW FRAME fPageHeader.
  DEFINE FRAME fPageFooter
     HEADER
        "<R62><C6><FROM><C78><LINE>" SKIP
        "<P8><C6>" STRING(TODAY) " " STRING(TIME,"HH:MM:SS") "<CENTER=C80>" cFirma "<P8>"
        WITH PAGE-BOTTOM STREAM-IO WIDTH 255.
  VIEW FRAME fPAgeFooter.

  ASSIGN
      wBruttoOmsetning = 0
      .
  
  /* Leser temp-table postene */
  TEMP-TABLEN:
  FOR EACH tmpKas_Rap WHERE  tmpKas_Rap.butikk = INT(FI-Butiker) AND 
                             tmpKas_Rap.Kasse = -9999
      BREAK 
      BY tmpKas_Rap.Dato
      BY tmpKas_Rap.butikk
      BY tmpKas_Rap.Kasse
      BY tmpKas_Rap.Z_Nummer:

    DO piLoop = 1 TO 10:       
        ASSIGN                                                  
            wBruttoOmsetning = wBruttoOmsetning +
                               tmpKas_rap.MvaGrunnlag[piLoop] + 
                               tmpKas_rap.MvaBelop[piLoop]      
            .             
    END.                       

    RUN ButikrubrikBokf.
    RUN Bilag1.
    RUN KortSpes (2).

/*     ASSIGN wBruttoOmsetning = tmpKas_Rap.kontant +                        */
/*                               tmpKas_Rap.sjekk +                          */
/*                               tmpKas_Rap.kort +                           */
/*                               tmpKas_Rap.kredit +                         */
/*                               tmpKas_Rap.kupong1 +                        */
/*                               tmpKas_Rap.kupong2 +                        */
/*                               tmpKas_Rap.Tilgode +                        */
/*                               tmpKas_Rap.Bank /*- tmpKas_Rap.Cashback*/ + */
/*                               tmpKas_Rap.Reservelosning +                 */
/*                               tmpKas_Rap.Gavekort -                       */
/*                               tmpKas_Rap.GaveKortUt +                     */
/*                               tmpKas_Rap.Avrunding -                      */
/*                               (tmpKas_Rap.InnbetaltKunde +                */
/*                                tmpKas_Rap.Kont_Inn -                      */
/*                                tmpKas_Rap.Kont_Ut                         */
/*                               ) +                                         */
/*                               tmpKas_Rap.GenerellRabatt +                 */
/*                               tmpKas_Rap.Kunderabatt +                    */
/*                               tmpKas_Rap.Personalrabatt +                 */
/*                               tmpKas_Rap.Medlemsrabatt +                  */
/*                               tmpKas_Rap.Pakkerabatt +                    */
/*                               tmpKas_Rap.Retur.                           */
    ASSIGN
      lKasseSlutt = tmpKas_Rap.OpptaltVeksel /* Ved dagens slutt */
                    + tmpKas_Rap.OpptaltKontanter
                    + tmpKas_Rap.OpptaltSjekk
                    + tmpKas_Rap.OpptaltReserve
                    + tmpKas_Rap.OpptaltValuta

  /* Kun det som regnes som kontanter skal tas med her.     */                  
  /*                   + tmpKas_Rap.OpptaltGavekort         */
  /*                   + tmpKas_Rap.OpptaltGavekortAndre    */
  /*                   - tmpKas_Rap.OpptaltGaveKortUtlevert */
  /*                   + tmpKas_Rap.OpptaltTilgode          */
  /*                   + tmpKas_Rap.OpptaltTilgodeAndre     */
  /*                   + tmpKas_Rap.OpptaltTilgodeUtlevert  */
  /*                   + tmpKas_Rap.OpptaltBilag            */
      .
    ASSIGN 
      /*lDagensKontStrom = wBruttoOmsetning + 
                         (tmpKas_Rap.Retur * -1) - 
                         tmpKas_Rap.GenerellRabatt - 
                         tmpKas_Rap.Kunderabatt - 
                         tmpKas_Rap.Personalrabatt -  
                         tmpKas_Rap.Medlemsrabatt - 
                         tmpKas_Rap.Pakkerabatt - 
                         tmpKas_Rap.Avrunding */
      lDagensKontStrom = wBruttoOmsetning
                         - tmpKas_Rap.TilgodeInn
                         - tmpKas_Rap.TilgodeAndre
                         /*- tmpKas_Rap.Gavekort */
                         - 0 /* Gavekort Universal */
                         - tmpKas_Rap.Kupong1
                         - tmpKas_Rap.Kupong2
                         + tmpKas_Rap.TilgodeUt
                         + tmpKas_Rap.GavekortUt
                         + lNonSale1
                         - tmpKas_Rap.GavekortInn
                         - tmpKas_Rap.Kredit
                         + tmpKas_Rap.InnbetaltKunde
                         - (tmpKas_Rap.kont_ut - tmpKas_Rap.kont_in)
                         - (tmpKas_Rap.Bank + tmpKas_Rap.CashBack)
                         - tmpKas_Rap.Reservelosning
                         + lNonSale2
                         + tmpKas_Rap.LayAway_Ut
                         - tmpKas_Rap.LayAway_Inn
                         - tmpKas_Rap.Dropp
      .

    /* Leser alle kortspesifikasjoner */
    FOR EACH tmpKort_Spes NO-LOCK WHERE
         tmpKort_Spes.Butikk   = INT(FI-Butiker) AND
         tmpKort_Spes.Kasse    = -9999:
      ASSIGN
        lDagensKontStrom = lDagensKontStrom - tmpKort_Spes.Belop
        .
    END.
/*     FOR EACH Kort_Spes NO-LOCK WHERE                          */
/*       Kort_Spes.Dato   = tmpKas_Rap.Dato AND                  */
/*       Kort_Spes.Butikk = tmpKas_Rap.Butikk:                   */
/*       ASSIGN                                                  */
/*         lDagensKontStrom = lDagensKontStrom - Kort_Spes.Belop */
/*         .                                                     */
/*     END.                                                      */

    ASSIGN
      lKasseEndring    = lKasseSlutt - tmpKas_Rap.OpptaltInnVeksel
      lKasseDiff       = lKasseEndring - lDagensKontStrom
      .

    RUN Bilag2.
    RUN BilagSpes.

    ASSIGN 
        iRad   = iRad + 1
        cRader = cRader + "," + STRING(iRad + 1)
        .
    /* Sidebryt pr. record. */
    PAGE.
  END. /* TEMP-TABLEN */
  IF CAN-FIND(FIRST tt_tilgode) THEN DO:
    RUN TilgodeAndreList.
  END.
  /* Lukker stream */
/*   OUTPUT TO TERMINAL. */
  OUTPUT CLOSE.
  /* Klargjør rapportfilnavnet */
  ASSIGN
      FILE-INFO:FILE-NAME = pcRappFil
      .
 RUN VisXprint.p (pcRappFil).    
  /* Sender filen til visning og utskrift. */
/*  RUN PrintPDF(FILE-INFO:FULL-PATHNAME, 'POLYGON SOFTWARE AS', 'A1a9T4h4e2h_mqe2mbka' ). */

  STATUS DEFAULT " ".
END.
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
  DEF VAR piRad        AS DEC    NO-UNDO.

  DEF VAR iRad         AS INTE INIT 10 NO-UNDO.
  DEF VAR iButik       AS INTE INIT ? NO-UNDO.
  DEF VAR iKasse       AS INTE INIT ? NO-UNDO.
  DEF VAR cRader       AS CHAR        NO-UNDO.
  DEF VAR cCols        AS CHAR        NO-UNDO.
  DEF VAR cUkeDar      AS CHAR        NO-UNDO.

  DEF VAR iSolgtAnt    AS INTE    NO-UNDO.
  DEF VAR dSolgtVerdi  AS DECI    NO-UNDO.
  DEF VAR iAntKunder   AS INTE    NO-UNDO.

  DEF VAR pcLabel      AS CHAR    NO-UNDO.
  DEF VAR plOmsetning  AS DEC     NO-UNDO.
  DEF VAR pcOverskr    AS CHAR    NO-UNDO.
  DEFINE VARIABLE wOK  AS LOGICAL    NO-UNDO.
  DEFINE VARIABLE cPrinter AS CHARACTER  NO-UNDO.
  DEFINE BUFFER btmpKas_Rap FOR tmpKas_Rap.
  /* Initierer tekstene på rapporten */
  ASSIGN
      cTittel     = "Finansrapport"
      cSubTittel1 = "Testbutikk AS"
      cSubTittel2 = "Kriterier" 
      cFirma      = "SkoTex/Test"
      cKrit1      = "Butikker:"
      cKrit2      = "Dato:"
      pcOverskr   = "Butikk" + CHR(1) + 
                    "Kasse" + CHR(1) +
                    "Dato" + CHR(1) +
                    "Antall" + CHR(1) + 
                    "Beløp" + CHR(1) + 
                    "Spesifikasjoner" + CHR(1) + 
                    "Deklareres" + CHR(1) + 
                    "Rabatter" + CHR(1) + 
                    "Omsetning" + CHR(1) + 
                    "Mvaregnskap" + CHR(1) +
                    "Gruppe" + CHR(1) +          
                    "Grunnlag" + CHR(1) + 
                    "Beløp" + CHR(1) + 
                    "Sum" + CHR(1) + 
                    "Total" + CHR(1).
{syspara.i 1 1 100 cSubTittel1}
{syspara.i 1 1 101 cFirma}

 DO WITH FRAME {&FRAME-NAME}:
  ASSIGN
    cSubtittel2     = cSubTittel2 + " " + 
                      cKrit1 + " " + INPUT FI-Butiker + ", " + 
      cKrit2 + " " + STRING(INPUT FI-FraDato) + " - " + STRING(FI-TilDato)
/*                       cKrit2 + " " + STRING(INPUT FI-FraDato) + " - " + STRING(INPUT FI-TilDato)  */
    .

  ASSIGN
    piCopies = 1
    .
 STATUS DEFAULT "Skriver ut, venligst vent...".

  /* Henter tempfilnavn */
/*  RUN GetTmpFileName IN hLibHandle ("utk","xpr", OUTPUT pcRappFil). */
  IF VALID-HANDLE(wLibHandle) THEN
    RUN GetTempFileName IN wLibHandle ("KasRap", "xpr", OUTPUT pcRappFil). 
   /* Åpner stream til skriverfil. */
  IF CAN-DO(SESSION:PARAMETER,"BATCH") THEN
      ASSIGN cPrinter = cButBatchPrinter.
  ELSE
      ASSIGN cPrinter = DYNAMIC-FUNCTION("getAttribute",SESSION,"SE_PRINTER").
  IF cPrinter = "" THEN DO:
      SYSTEM-DIALOG PRINTER-SETUP UPDATE wOK.
      IF NOT wOK THEN
          RETURN NO-APPLY.
      ASSIGN cPrinter = SESSION:PRINTER-NAME.
  END.
  OUTPUT TO value(pcRappFil) PAGED page-size 255.
  IF NOT lDirekte THEN
      PUT CONTROL '<PDF-OUTPUT=' + REPLACE(pcRappFil,"xpr","pdf") + '>'.
  PUT CONTROL     "<PRINTER" cPrinter ">".
  PUT CONTROL "<TRANSPARENT=true><UNITS=MM><LINECOLOR=BLACK><FGCOLOR=BLACK>".
  IF NOT lDirekte THEN
      PUT CONTROL '<PREVIEW=ZoomToWidth>'.

  DEFINE FRAME fPageHeader
     HEADER
        "<ALIGN=BASE><FArial>"
        "<R4><P20><B><C5.5>" cTittel "<P12></B>" SKIP
        "<R5.5><P10><B><C6>" cSubTittel1 "<P10></B><P10>" SKIP
        "<P8><C6>" cSubTittel2 "<P8><C74>" PAGE-NUMBER FORMAT ">>>" "/ <#Pages>" SKIP
        "<R7.5><C6><FROM><R7.5><C78><LINE>" SKIP
        WITH PAGE-TOP STREAM-IO WIDTH 255.
  VIEW FRAME fPageHeader.
  DEFINE FRAME fPageFooter
     HEADER
        "<R64><C6><FROM><C78><LINE>" SKIP
        "<P8><C6>" STRING(TODAY) " " STRING(TIME,"HH:MM:SS") "<C40><RIGHT=C+40>" cFirma "<P8>"
        WITH PAGE-BOTTOM STREAM-IO WIDTH 255.
  VIEW FRAME fPAgeFooter.
  
  /* Leser temp-table postene */
  TEMP-TABLEN:
  FOR EACH btmpKas_Rap WHERE btmpKas_Rap.Sortering = 0
      BREAK 
      BY btmpKas_Rap.Dato
      BY btmpKas_Rap.butikk
      BY btmpKas_Rap.Kasse
      BY btmpKas_Rap.Z_Nummer:

    FIND tmpKas_Rap WHERE ROWID(tmpKas_Rap) = ROWID(btmpKas_Rap).

    ASSIGN wBruttoOmsetning = tmpKas_Rap.kontant + 
                              tmpKas_Rap.sjekk + 
                              tmpKas_Rap.kort + 
                              tmpKas_Rap.kredit + 
                              tmpKas_Rap.kupong1 + 
                              tmpKas_Rap.kupong2 + 
                              tmpKas_Rap.Tilgode + 
                              tmpKas_Rap.Bank - tmpKas_Rap.InnbetaltKunde /* - tmpKas_Rap.Cashback */ + 
                              tmpKas_Rap.Reservelosning + 
                              /*tmpKas_Rap.Gavekort - */
                              /*tmpKas_Rap.GavekortUt*/ + 
                              tmpKas_Rap.GavekortInn + 
                              tmpKas_Rap.Avrunding - 
                              (tmpKas_Rap.Kont_Inn - 
                               tmpKas_Rap.Kont_Ut 
                              ) + 
                              tmpKas_Rap.GenerellRabatt + 
                              tmpKas_Rap.Kunderabatt + 
                              tmpKas_Rap.Personalrabatt + 
                              tmpKas_Rap.Medlemsrabatt + 
                              tmpKas_Rap.Pakkerabatt + 
                              tmpKas_Rap.layaway_inn -
                              tmpKas_Rap.layaway_Ut +
                              (tmpKas_Rap.Retur + tmpKas_Rap.Reklamasjon)
                              .

    RUN Butikrubrik.
    RUN KasseTot.
    RUN Varesalg.
    RUN Bank.
    RUN MvaRegnskap.
    RUN Omsetning.
    RUN Diverse.
    RUN Beholdning.
    ASSIGN 
        iRad   = iRad + 1
        cRader = cRader + "," + STRING(iRad + 1)
        .
    /* Sidebryt pr. record. */
    PAGE.
    IF FI-FraDato <> FI-TilDato AND btmpKas_Rap.Kasse > 0 THEN DO:
        FOR EACH tmpKas_Rap WHERE tmpKas_Rap.Butikk = btmpKas_Rap.Butikk AND 
                                  tmpKas_Rap.Kasse  = btmpKas_Rap.Kasse  AND
                                  tmpKas_Rap.Sortering > 0
            BREAK 
            BY tmpKas_Rap.Kasse
            BY tmpKas_Rap.Dato
            BY tmpKas_Rap.Z_Nummer:

            ASSIGN wBruttoOmsetning = tmpKas_Rap.kontant - 
                                      tmpKas_Rap.sjekk + 
                                      tmpKas_Rap.kort + 
                                      tmpKas_Rap.kredit + 
                                      tmpKas_Rap.kupong1 + 
                                      tmpKas_Rap.kupong2 + 
                                      tmpKas_Rap.Tilgode + 
                                      tmpKas_Rap.Bank /* - tmpKas_Rap.Cashback */ + 
                                      tmpKas_Rap.Reservelosning + 
                                      /*tmpKas_Rap.Gavekort - */
                                      /*tmpKas_Rap.GavekortUt*/ + 
                                      tmpKas_Rap.GavekortInn + 
                                      tmpKas_Rap.Avrunding - 
                                      (tmpKas_Rap.InnbetaltKunde  + 
                                       tmpKas_Rap.Kont_Inn - 
                                       tmpKas_Rap.Kont_Ut 
                                      ) + 
                                      tmpKas_Rap.GenerellRabatt + 
                                      tmpKas_Rap.Kunderabatt + 
                                      tmpKas_Rap.Personalrabatt + 
                                      tmpKas_Rap.Medlemsrabatt + 
                                      tmpKas_Rap.Pakkerabatt + 
                                      tmpKas_Rap.layaway_inn -
                                      tmpKas_Rap.layaway_Ut +
                                      (tmpKas_Rap.Retur + tmpKas_Rap.Reklamasjon)
                                      .

/*             ASSIGN wBruttoOmsetning =                                                    */
/*                 tmpKas_Rap.kontant +                                                     */
/*                 tmpKas_Rap.sjekk +                                                       */
/*                 tmpKas_Rap.kort +                                                        */
/*                 tmpKas_Rap.kredit +                                                      */
/*                 tmpKas_Rap.kupong1 +                                                     */
/*                 tmpKas_Rap.kupong2 +                                                     */
/*                 tmpKas_Rap.Tilgode +                                                     */
/*                 tmpKas_Rap.Bank -                                                        */
/*                 tmpKas_Rap.Cashback +                                                    */
/*                 tmpKas_Rap.Reservelosning +                                              */
/*                 tmpKas_Rap.Gavekort +                                                    */
/*                 tmpKas_Rap.Avrunding -                                                   */
/*                 (tmpKas_Rap.InnbetaltKunde + tmpKas_Rap.Kont_Inn - tmpKas_Rap.Kont_Ut) + */
/*                 tmpKas_Rap.GenerellRabatt +                                              */
/*                 tmpKas_Rap.Kunderabatt +                                                 */
/*                 tmpKas_Rap.Personalrabatt +                                              */
/*                 tmpKas_Rap.Medlemsrabatt +                                               */
/*                 tmpKas_Rap.Pakkerabatt +                                                 */
/*                 (tmpKas_Rap.Retur + tmpKas_Rap.Reklamasjon).                             */

            RUN Butikrubrik.
            RUN KasseTot.
            RUN Varesalg.
            RUN Bank.
            RUN MvaRegnskap.
            RUN Omsetning.
            RUN Diverse.
            RUN Beholdning.
            ASSIGN 
                iRad   = iRad + 1
                cRader = cRader + "," + STRING(iRad + 1)
                .
            /* Sidebryt pr. record. */
            PAGE.
        END.
    END.
  END. /* TEMP-TABLEN */

  /* Lukker stream */
/*   OUTPUT TO TERMINAL. */
  OUTPUT CLOSE.
  /* Klargjør rapportfilnavnet */
  ASSIGN
      FILE-INFO:FILE-NAME = pcRappFil
      .
    
  /* Sender filen til visning og utskrift. */
/*  RUN PrintPDF(FILE-INFO:FULL-PATHNAME, 'POLYGON SOFTWARE AS', 'A1a9T4h4e2h_mqe2mbka' ). */
  RUN VisXprint.p (pcRappFil).    

  STATUS DEFAULT " ".
END.
END PROCEDURE.
/*
     /* Sumlinje */
     IF tmpAktRapp.Butik <> iButik OR tmpAktRapp.Kasse <> iKasse THEN DO:
         IF iButik <> ? THEN DO:
             PUT UNFORMATTED "<FCourier NEW><P10><B><R" + STRING(iRad) + ">" +
             "<C4>" +  "TOT" +
             "<C10>" + STRING(iSolgtAnt,">>,>>9")    +
/*              "<C17>" + STRING(tmpAktRapp.Solgt%Ant,">9.9")   + */
             "<C22>" + STRING(dSolgtVerdi,">,>>>,>>9.99")   +
/*              "<C34>" + STRING(tmpAktRapp.Solgt%Verdi,">9.9")  + */
             "<C39>" + STRING(iAntKunder,">>,>>9")    +
/*              "<C46>" + STRING(tmpAktRapp.Ant%Kunder,">9.9")   + */
             "<C52>" + STRING(iSolgtAnt / iAntKunder,">>9.9")   +
             "<C57>" + STRING(dSolgtVerdi / iAntKunder,">>,>>9.99") +
             "<C66>" + STRING(dSolgtVerdi / iSolgtAnt,">>,>>9.99") + "</B>".
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
         PUT UNFORMATTED "<P10><R" + STRING(iRad) + "><B><C10>" + FILL-IN-1 +
               ": " + cUkeDar + "</B>".
         
         ASSIGN iRad   = iRad + 2
                cRader = STRING(iRad).
         PUT UNFORMATTED "<FCourier New><P10><B><R" + STRING(iRad) + "><C10>Antall<C25>Verdi<C39>Antall<C52>Par/<C59>Verdi/<C67>Verdi/".
         PUT UNFORMATTED "<P10><R" + STRING(iRad + 1) + ">" +
                     "<C5>Tid<C11>Solgt<C18>%<C25>Solgt<C35>%<C39>Kunder<C47>%<C51>Kunde<C60>Kunde<C69>Par</B>".
         ASSIGN iRad   = iRad + 2
                cRader = cRader + "," + STRING(iRad)
                iButik = tmpAktRapp.Butik
                iKasse = tmpAktRapp.Kasse.
     END.

*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

