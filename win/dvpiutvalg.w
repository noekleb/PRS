&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
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
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEF VAR bSvar AS LOG INITIAL FALSE NO-UNDO.
DEF VAR bStop AS LOG               NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-62 B-SokLevNr RECT-63 RECT-64 CB-VPILev ~
B-Nullstill FI-LevNr CB-Koblet FI-Beskr B-BeskrBlank CB-Hg FI-Bongtekst ~
B-Bongtekst CB-Vg FI-LevKod B-LevKodBlank CB-Sasong FI-ImpDato1 FI-ImpDato2 ~
B-ImportBlank FI-Endret1 FI-Endret2 B-Endre B-Utvalg Btn_OK 
&Scoped-Define DISPLAYED-OBJECTS CB-VPILev FI-AntValgt FI-LevNr CB-Koblet ~
FI-Beskr CB-Hg FI-Bongtekst CB-Vg FI-LevKod CB-Sasong FI-ImpDato1 ~
FI-ImpDato2 FI-Endret1 FI-Endret2 FI-Status FILL-IN-9 FILL-IN-8 FILL-IN-7 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-Avbryt 
     LABEL "Avbryt" 
     SIZE 15 BY 1.14.

DEFINE BUTTON B-BeskrBlank 
     LABEL "Blank" 
     SIZE 8 BY 1.

DEFINE BUTTON B-Bongtekst 
     LABEL "Blank" 
     SIZE 8 BY 1.

DEFINE BUTTON B-Endre 
     LABEL "Blank" 
     SIZE 8 BY 1.

DEFINE BUTTON B-ImportBlank 
     LABEL "Blank" 
     SIZE 8 BY 1.

DEFINE BUTTON B-LevKodBlank 
     LABEL "Blank" 
     SIZE 8 BY 1.

DEFINE BUTTON B-Nullstill 
     LABEL "Nullstill" 
     SIZE 15 BY 1.

DEFINE BUTTON B-SokLevNr  NO-FOCUS
     LABEL "..." 
     SIZE 4.6 BY 1.

DEFINE BUTTON B-Utvalg 
     LABEL "Start utvalg..." 
     SIZE 35 BY 1.14.

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 20 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE CB-Hg AS CHARACTER FORMAT "X(256)":U 
     LABEL "Hovedgruppe" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE CB-Koblet AS CHARACTER FORMAT "X(256)":U 
     LABEL "Koblet" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE CB-Sasong AS CHARACTER FORMAT "X(256)":U 
     LABEL "Ses&ong" 
     VIEW-AS COMBO-BOX INNER-LINES 10
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE CB-Vg AS CHARACTER FORMAT "X(256)":U 
     LABEL "&Varegruppe" 
     VIEW-AS COMBO-BOX INNER-LINES 20
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE CB-VPILev AS INTEGER FORMAT "zzzzzzzz9":U INITIAL 0 
     LABEL "VPI leverandører" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEM-PAIRS "Item 1",0
     DROP-DOWN-LIST
     SIZE 36 BY 1 NO-UNDO.

DEFINE VARIABLE FI-AntValgt AS INTEGER FORMAT "zzzzzzz9":U INITIAL 0 
     LABEL "Antall poster valgt" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Beskr AS CHARACTER FORMAT "x(20)" 
     LABEL "Beskrivelse" 
     VIEW-AS FILL-IN 
     SIZE 28.6 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Bongtekst AS CHARACTER FORMAT "X(256)":U 
     LABEL "Bongtekst" 
     VIEW-AS FILL-IN 
     SIZE 28.6 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Endret1 AS DATE FORMAT "99/99/99":U 
     LABEL "Endret" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Endret2 AS DATE FORMAT "99/99/99":U 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-ImpDato1 AS DATE FORMAT "99/99/99":U 
     LABEL "Importert" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-ImpDato2 AS DATE FORMAT "99/99/99":U 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-LevKod AS CHARACTER FORMAT "X(256)":U 
     LABEL "Levartnr" 
     VIEW-AS FILL-IN 
     SIZE 28.6 BY 1 NO-UNDO.

DEFINE VARIABLE FI-LevNr AS INTEGER FORMAT ">>>>>>":U INITIAL 0 
     LABEL "Levnr" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Status AS CHARACTER FORMAT "X(256)":U 
     LABEL "Status" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 61 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-7 AS CHARACTER FORMAT "X(256)":U INITIAL "Utvalg" 
      VIEW-AS TEXT 
     SIZE 9.6 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FILL-IN-8 AS CHARACTER FORMAT "X(256)":U INITIAL "Nullstille utvalg" 
      VIEW-AS TEXT 
     SIZE 18.6 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FILL-IN-9 AS CHARACTER FORMAT "X(256)":U INITIAL "Velg VPI leverandør" 
      VIEW-AS TEXT 
     SIZE 23.6 BY .62
     FONT 6 NO-UNDO.

DEFINE RECTANGLE RECT-62
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 104 BY 1.91.

DEFINE RECTANGLE RECT-63
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 104 BY 1.91.

DEFINE RECTANGLE RECT-64
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 105 BY 8.57.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     B-SokLevNr AT ROW 7.19 COL 34 NO-TAB-STOP 
     CB-VPILev AT ROW 1.95 COL 18 COLON-ALIGNED
     FI-AntValgt AT ROW 4.57 COL 18 COLON-ALIGNED
     B-Nullstill AT ROW 4.57 COL 34
     FI-LevNr AT ROW 7.19 COL 18 COLON-ALIGNED
     CB-Koblet AT ROW 7.19 COL 69.2 COLON-ALIGNED
     FI-Beskr AT ROW 8.19 COL 18 COLON-ALIGNED HELP
          "Kort beskrivelse av artikkelen"
     B-BeskrBlank AT ROW 8.19 COL 49
     CB-Hg AT ROW 8.19 COL 69.2 COLON-ALIGNED
     FI-Bongtekst AT ROW 9.19 COL 18 COLON-ALIGNED
     B-Bongtekst AT ROW 9.19 COL 49
     CB-Vg AT ROW 9.19 COL 69.2 COLON-ALIGNED
     FI-LevKod AT ROW 10.19 COL 18 COLON-ALIGNED
     B-LevKodBlank AT ROW 10.19 COL 49
     CB-Sasong AT ROW 10.19 COL 69.2 COLON-ALIGNED
     FI-ImpDato1 AT ROW 11.19 COL 18 COLON-ALIGNED
     FI-ImpDato2 AT ROW 11.19 COL 32.4 COLON-ALIGNED NO-LABEL
     B-ImportBlank AT ROW 11.19 COL 49
     FI-Endret1 AT ROW 12.14 COL 18 COLON-ALIGNED
     FI-Endret2 AT ROW 12.14 COL 32.4 COLON-ALIGNED NO-LABEL
     B-Endre AT ROW 12.19 COL 49
     B-Utvalg AT ROW 13.86 COL 36
     B-Avbryt AT ROW 13.86 COL 71
     Btn_OK AT ROW 15.52 COL 86
     FI-Status AT ROW 15.62 COL 18 COLON-ALIGNED
     FILL-IN-9 AT ROW 1 COL 2.4 NO-LABEL
     FILL-IN-8 AT ROW 3.52 COL 2.4 NO-LABEL
     FILL-IN-7 AT ROW 6.24 COL 2.4 NO-LABEL
     RECT-62 AT ROW 4.1 COL 1
     RECT-63 AT ROW 1.48 COL 1
     RECT-64 AT ROW 6.71 COL 1
     SPACE(0.00) SKIP(1.52)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "VPI utvalg for eksport"
         DEFAULT-BUTTON Btn_OK.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Dialog-Box
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
                                                                        */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON B-Avbryt IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-AntValgt IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Status IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-7 IN FRAME Dialog-Frame
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN FILL-IN-8 IN FRAME Dialog-Frame
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN FILL-IN-9 IN FRAME Dialog-Frame
   NO-ENABLE ALIGN-L                                                    */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* VPI utvalg for eksport */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Avbryt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Avbryt Dialog-Frame
ON CHOOSE OF B-Avbryt IN FRAME Dialog-Frame /* Avbryt */
DO:
  ASSIGN
    bStop = TRUE
    B-Utvalg:SENSITIVE = FALSE
    .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-BeskrBlank
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-BeskrBlank Dialog-Frame
ON CHOOSE OF B-BeskrBlank IN FRAME Dialog-Frame /* Blank */
DO:
  ASSIGN FI-Beskr:SCREEN-VALUE = "".
  APPLY "TAB" TO FI-Beskr.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Bongtekst
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Bongtekst Dialog-Frame
ON CHOOSE OF B-Bongtekst IN FRAME Dialog-Frame /* Blank */
DO:
  ASSIGN FI-BongTekst:SCREEN-VALUE = "".
  APPLY "TAB" TO FI-Bongtekst.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Endre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Endre Dialog-Frame
ON CHOOSE OF B-Endre IN FRAME Dialog-Frame /* Blank */
DO:
  ASSIGN 
    FI-Endret1:SCREEN-VALUE = ""
    FI-Endret2:SCREEN-VALUE = ""
    .
  APPLY "TAB" TO FI-Endret1.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-ImportBlank
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-ImportBlank Dialog-Frame
ON CHOOSE OF B-ImportBlank IN FRAME Dialog-Frame /* Blank */
DO:
  ASSIGN 
    FI-ImpDato1:SCREEN-VALUE = ""
    FI-ImpDato2:SCREEN-VALUE = ""
    .
  APPLY "TAB" TO FI-ImpDato1.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-LevKodBlank
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-LevKodBlank Dialog-Frame
ON CHOOSE OF B-LevKodBlank IN FRAME Dialog-Frame /* Blank */
DO:
  ASSIGN FI-LevKod:SCREEN-VALUE = "".
  APPLY "TAB" TO FI-LevKod.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Nullstill
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Nullstill Dialog-Frame
ON CHOOSE OF B-Nullstill IN FRAME Dialog-Frame /* Nullstill */
DO:
  IF INT(FI-AntValgt:SCREEN-VALUE) = 0 THEN
  DO:
    MESSAGE "Ingenting å nullstille."
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN NO-APPLY.
  END.
  ASSIGN
    bSvar = FALSE
    .
  MESSAGE "Skal utvalget nullstilles?"
    VIEW-AS ALERT-BOX INFO BUTTONS YES-NO UPDATE bSvar.
  IF bSvar <> TRUE THEN
    RETURN NO-APPLY.

  RUN NullstillUtvalg.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SokLevNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SokLevNr Dialog-Frame
ON CHOOSE OF B-SokLevNr IN FRAME Dialog-Frame /* ... */
OR F10 OF FI-LevNr
DO:
  DEFINE VARIABLE cTekst AS CHARACTER  NO-UNDO.
  ASSIGN
      cTekst = "".

  /* Kaller søkerutine */
  RUN gLevbas.w (
    INPUT-OUTPUT cTekst, /* Returstreng - chr(1) separert */
    "", /* Feltliste avgrensningsfelt (kommaseparert) */
    "", /* Feltverdier (chr(1) sep) */ 
    FI-LevNr:SCREEN-VALUE /* Post markøren skal stå på */
    ).
  IF RETURN-VALUE = "AVBRYT" THEN
      RETURN NO-APPLY.
  IF NUM-ENTRIES(cTekst,CHR(1)) = 3 THEN
  DO:
      /* Legger opp verdier I de aktuelle feltene */
      ASSIGN
        FI-LevNr:SCREEN-VALUE = ENTRY(2,cTekst,CHR(1))
        .

        /* Flagger at det er gjort endringer på recorden og trigger toolbar. */
        APPLY "TAB":U TO FI-LevNr.
        RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Utvalg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Utvalg Dialog-Frame
ON CHOOSE OF B-Utvalg IN FRAME Dialog-Frame /* Start utvalg... */
DO:
  ASSIGN
    bSvar = FALSE
    B-Avbryt:SENSITIVE = TRUE
    .
  MESSAGE "Skal utvalget gjøres?" SKIP
          "Nytt utvalg vil adderes til eventuelt eksisterende utvalg."
    VIEW-AS ALERT-BOX INFO BUTTONS YES-NO UPDATE bSvar.
  IF bSvar <> TRUE THEN
    RETURN NO-APPLY.

  ASSIGN
    B-Avbryt:SENSITIVE = TRUE
    .
  RUN KjorUtvalg.
  ASSIGN
    B-Avbryt:SENSITIVE = FALSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CB-Hg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-Hg Dialog-Frame
ON VALUE-CHANGED OF CB-Hg IN FRAME Dialog-Frame /* Hovedgruppe */
OR "TAB":U OF CB-Hg DO:
  RUN InitVg.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CB-Koblet
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-Koblet Dialog-Frame
ON VALUE-CHANGED OF CB-Koblet IN FRAME Dialog-Frame /* Koblet */
OR "TAB":U OF CB-Koblet DO:
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CB-VPILev
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-VPILev Dialog-Frame
ON VALUE-CHANGED OF CB-VPILev IN FRAME Dialog-Frame /* VPI leverandører */
DO:
  IF int(SELF:SCREEN-VALUE) <> 0 THEN
  DO:
    ASSIGN
      B-Utvalg:SENSITIVE       = TRUE
      FI-AntValgt:SCREEN-VALUE = "0"
      B-Nullstill:SENSITIVE    = TRUE 
      .
    RUN TellOppValgte.
  END.
  ELSE
    ASSIGN
      B-Utvalg:SENSITIVE       = FALSE
      FI-AntValgt:SCREEN-VALUE = "0"
      B-Nullstill:SENSITIVE    = FALSE
      .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-Beskr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Beskr Dialog-Frame
ON RETURN OF FI-Beskr IN FRAME Dialog-Frame /* Beskrivelse */
OR "TAB":U OF FI-Beskr DO:
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-Bongtekst
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Bongtekst Dialog-Frame
ON RETURN OF FI-Bongtekst IN FRAME Dialog-Frame /* Bongtekst */
OR "TAB":U OF FI-BongTekst DO:
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-Endret1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Endret1 Dialog-Frame
ON DELETE-CHARACTER OF FI-Endret1 IN FRAME Dialog-Frame /* Endret */
DO:
  ASSIGN
    SELF:SCREEN-VALUE = ""
    .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Endret1 Dialog-Frame
ON RETURN OF FI-Endret1 IN FRAME Dialog-Frame /* Endret */
OR "TAB":U OF FI-Endret1 DO:
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-Endret2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Endret2 Dialog-Frame
ON DELETE-CHARACTER OF FI-Endret2 IN FRAME Dialog-Frame
DO:
  ASSIGN
    SELF:SCREEN-VALUE = ""
    .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Endret2 Dialog-Frame
ON RETURN OF FI-Endret2 IN FRAME Dialog-Frame
OR "TAB":U OF FI-Endret2 DO:
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-ImpDato1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-ImpDato1 Dialog-Frame
ON DELETE-CHARACTER OF FI-ImpDato1 IN FRAME Dialog-Frame /* Importert */
DO:
  ASSIGN
    SELF:SCREEN-VALUE = ""
    .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-ImpDato1 Dialog-Frame
ON RETURN OF FI-ImpDato1 IN FRAME Dialog-Frame /* Importert */
OR "TAB":U OF FI-ImpDato1 DO:
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-ImpDato2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-ImpDato2 Dialog-Frame
ON DELETE-CHARACTER OF FI-ImpDato2 IN FRAME Dialog-Frame
DO:
  ASSIGN
    SELF:SCREEN-VALUE = ""
    .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-ImpDato2 Dialog-Frame
ON RETURN OF FI-ImpDato2 IN FRAME Dialog-Frame
OR "TAB":U OF FI-ImpDato2 DO:
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-LevKod
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-LevKod Dialog-Frame
ON RETURN OF FI-LevKod IN FRAME Dialog-Frame /* Levartnr */
OR "TAB":U OF FI-LevKod DO:
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-LevNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-LevNr Dialog-Frame
ON RETURN OF FI-LevNr IN FRAME Dialog-Frame /* Levnr */
OR "TAB":U OF FI-LevNr DO:
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


RUN InitFilter.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

  RUN enable_UI.

  DISPLAY
    CB-VPILev
    WITH FRAME {&FRAME-NAME}.
  IF CB-VPILev <> 0 THEN
    B-Utvalg:SENSITIVE = TRUE.
  ELSE
    B-Utvalg:SENSITIVE = FALSE.

  RUN TellOppValgte.

  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dialog-Frame  _DEFAULT-DISABLE
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
  HIDE FRAME Dialog-Frame.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dialog-Frame  _DEFAULT-ENABLE
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
  DISPLAY CB-VPILev FI-AntValgt FI-LevNr CB-Koblet FI-Beskr CB-Hg FI-Bongtekst 
          CB-Vg FI-LevKod CB-Sasong FI-ImpDato1 FI-ImpDato2 FI-Endret1 
          FI-Endret2 FI-Status FILL-IN-9 FILL-IN-8 FILL-IN-7 
      WITH FRAME Dialog-Frame.
  ENABLE RECT-62 B-SokLevNr RECT-63 RECT-64 CB-VPILev B-Nullstill FI-LevNr 
         CB-Koblet FI-Beskr B-BeskrBlank CB-Hg FI-Bongtekst B-Bongtekst CB-Vg 
         FI-LevKod B-LevKodBlank CB-Sasong FI-ImpDato1 FI-ImpDato2 
         B-ImportBlank FI-Endret1 FI-Endret2 B-Endre B-Utvalg Btn_OK 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitFilter Dialog-Frame 
PROCEDURE InitFilter :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
      CB-VPILev:LIST-ITEM-PAIRS = "[Ingen],0"
      .
    FOR EACH EkstVPILev NO-LOCK WHERE
      CAN-FIND(FIRST VPIArtBas OF EkstVPILev):
      IF CB-VPILev = 0 THEN
        CB-VPILev = EkstVPILEv.EkstVPILevNr.

      ASSIGN
        CB-VPILev:LIST-ITEM-PAIRS = CB-VPILev:LIST-ITEM-PAIRS + 
                                    (IF CB-VPILev:LIST-ITEM-PAIRS <> ""
                                      THEN ","
                                      ELSE "") + 
                                    EkstVPILev.Navn + "," + STRING(EkstVPILEv.EkstVPILevNr)
        .

    END.

    ASSIGN CB-Hg:LIST-ITEM-PAIRS = "[Alle],"
           CB-Hg:screen-value = " ".
    for each HuvGr no-lock:
        CB-Hg:add-last(string(HuvGr.Hg,"9999") + "   " + 
                                            REPLACE(HuvGr.HgBeskr,","," "),STRING(HuvGr.Hg)).
    end.
/*       CB-Vg:DELETE(1). */
    ASSIGN CB-Vg:LIST-ITEM-PAIRS = "[Alle],"
           CB-Vg:screen-value = " ".
/*     for each VarGr no-lock:                                                                   */
/*         CB-Vg:add-last(string(VarGr.Vg,"999999") + "   " +                                    */
/*                                             REPLACE(VarGr.VgBeskr,","," "),STRING(VarGr.Vg)). */
/*     end.                                                                                      */
/*       CB-Sasong:DELETE(1). */
    ASSIGN CB-Sasong:LIST-ITEM-PAIRS = "[Alle],"
           CB-Sasong:screen-value = " ".
    for each Sasong no-lock:
      CB-Sasong:add-last(string(SaSong.SaSong,"zzz9") + "   " + 
                                               REPLACE(Sasong.SasBeskr,","," "),string(SaSong.SaSong)).
    end.
    ASSIGN CB-Koblet:LIST-ITEM-PAIRS = "[Alle]," /* ,Pakker,yes,Ikke pakker,no" */
           CB-Koblet:screen-value = " ".
    CB-Koblet:ADD-LAST("Ja","yes").
    CB-Koblet:ADD-LAST("Nei","no").
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitVg Dialog-Frame 
PROCEDURE InitVg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
      ASSIGN CB-Vg:LIST-ITEM-PAIRS = "[Alle],"
             CB-Vg:screen-value = " ".
      for each VarGr NO-LOCK WHERE
          (IF int(INPUT CB-Hg) > 0
             THEN VarGr.Hg = int(INPUT CB-Hg)
             ELSE TRUE):
          CB-Vg:add-last(string(VarGr.Vg,"999999") + "   " +
                                              REPLACE(VarGr.VgBeskr,","," "),STRING(VarGr.Vg)).
      end.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE KjorUtvalg Dialog-Frame 
PROCEDURE KjorUtvalg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR piAntall AS INT NO-UNDO.
DEF VAR piLest   AS INT NO-UNDO.
DEF VAR piNye    AS INT NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:

  ASSIGN
    FI-Status:SCREEN-VALUE = ""
    piAntall = INPUT FI-AntValgt
    .


  UTVALG:
  FOR EACH VPIArtBas NO-LOCK WHERE
    VPIArtBas.EkstVPILevNr = INPUT CB-VPILev:

    /* Bruker avbryter. */
    PROCESS EVENTS.
    if bStop then
      do:
        message "Skal utvalg stoppes? " view-as alert-box buttons YES-NO 
                 set bStop.
        if bStop = true then
        DO:
          bStop = FALSE.
          B-Utvalg:SENSITIVE = TRUE.
          leave UTVALG.
        END.
      end.

    ASSIGN
      piLest = piLest + 1.
      .
    IF piLest MODULO 20 = 0 THEN
    DO:
      FI-Status:SCREEN-VALUE = "Lest " + STRING(piLest) + " " + 
                               "poster. " + STRING(piNye) + " poster lagt opp.".
    END.

    /* Opprettet dato */
    IF INPUT FI-ImpDato1 <> ? THEN
    DO:
      IF VPIArtBas.RegistrertDato < INPUT FI-ImpDato1 THEN
        NEXT UTVALG.
    END.
    IF INPUT FI-ImpDato2 <> ? THEN
    DO:
      IF VPIArtBas.RegistrertDato > INPUT FI-ImpDato2 THEN
        NEXT UTVALG.
    END.
    /* Endret dato */
    IF INPUT FI-Endret1 <> ? THEN
    DO:
      IF VPIArtBas.EDato < INPUT FI-Endret1 THEN
        NEXT UTVALG.
    END.
    IF INPUT FI-Endret2 <> ? THEN
    DO:
      IF VPIArtBas.EDato > INPUT FI-Endret2 THEN
        NEXT UTVALG.
    END.
    /* Beskrivelse */
    IF INPUT FI-Beskr <> "" THEN
    DO:
      IF SUBSTRING(INPUT FI-Beskr,1,1) = "*" THEN
        IF NOT VPIArtBas.Beskr MATCHES INPUT FI-Beskr + "*" THEN
          NEXT UTVALG.
      ELSE
        IF NOT VPIArtBas.Beskr BEGINS INPUT FI-Beskr THEN
          NEXT UTVALG.
    END.
    /* Bongtekst */
    IF INPUT FI-BongTekst <> "" THEN
    DO:
      IF SUBSTRING(INPUT FI-BongTekst,1,1) = "*" THEN
        IF NOT VPIArtBas.BongTekst MATCHES INPUT FI-BongTekst + "*" THEN
          NEXT UTVALG.
      ELSE
        IF NOT VPIArtBas.BongTekst BEGINS INPUT FI-BongTekst THEN
          NEXT UTVALG.
    END.
    /* LevKod  */
    IF INPUT FI-LevKod <> "" THEN
    DO:
      IF SUBSTRING(INPUT FI-LevKod,1,1) = "*" THEN
        IF NOT VPIArtBas.LevKod MATCHES INPUT FI-LevKod + "*" THEN
          NEXT UTVALG.
      ELSE
        IF NOT VPIArtBas.LevKod BEGINS INPUT FI-LevKod THEN
          NEXT UTVALG.
    END.
    /* Leverandør */
    IF INPUT FI-LevNr <> 0 THEN
    DO:
      IF VPIArtBas.LevNr <> INPUT FI-LevNr THEN
        NEXT UTVALG.
    END.

    FIND ELogg WHERE 
         ELogg.TabellNavn     = "VPIArtBas" AND
         ELogg.EksterntSystem = "POS"    AND
         ELogg.Verdier        = STRING(VPIArtBas.EkstVPILevNr) + CHR(1) + 
                                VPIArtBas.VareNr NO-ERROR.
    IF NOT AVAIL Elogg THEN DO:
        CREATE Elogg.
        ASSIGN ELogg.TabellNavn     = "VPIArtBas"
               ELogg.EksterntSystem = "POS"   
               ELogg.Verdier        = STRING(VPIArtBas.EkstVPILevNr) + CHR(1) + 
                                      VPIArtBas.VareNr.
        /* Teller opp */
        ASSIGN
          piAntall = piAntall + 1
          piNye    = piNye + 1
          .
        IF piAntall MODULO 20 = 0 THEN
          FI-AntValgt:SCREEN-VALUE = STRING(piAntall).

    END.
    ASSIGN ELogg.EndringsType = 1
           ELogg.Behandlet    = FALSE.
  END. /* UTVALG */
  ASSIGN
    FI-Status:SCREEN-VALUE = "Utvalg ferdig."
    B-Utvalg:SENSITIVE = TRUE 
    .
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NullstillUtvalg Dialog-Frame 
PROCEDURE NullstillUtvalg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR piAnt AS INT NO-UNDO.
  DO WITH FRAME Dialog-Frame:
    ASSIGN
      FI-Status:SCREEN-VALUE = "Nullstiller utvalg..."
      .
    FOR EACH ELogg EXCLUSIVE-LOCK WHERE 
           ELogg.TabellNavn     = "VPIArtBas" AND
           ELogg.EksterntSystem = "POS":
      ASSIGN
        piAnt = piAnt + 1
        .
      IF piAnt MODULO 20 = 0 THEN
      DO:
        FI-Status:SCREEN-VALUE = "Slettet " + STRING(piAnt) + " " + 
                                 "poster.".
      END.
      DELETE ELogg.
    END.
    ASSIGN
      FI-Status:SCREEN-VALUE   = "Utvalg nullstillt."
      FI-AntValgt:SCREEN-VALUE = ""
      .
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE TellOppValgte Dialog-Frame 
PROCEDURE TellOppValgte :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR piAnt AS INT NO-UNDO.

DO WITH FRAME Dialog-Frame:
  ASSIGN
    FI-Status:SCREEN-VALUE = "Teller opp..."
    .
  FOR EACH ELogg NO-LOCK WHERE 
         ELogg.TabellNavn     = "VPIArtBas" AND
         ELogg.EksterntSystem = "POS":
    ASSIGN
      piAnt = piAnt + 1
      .
    IF piAnt MODULO 20 = 0 THEN
    DO:

      FI-Status:SCREEN-VALUE = "Teller " + STRING(piAnt) + " " + 
                               "poster.".
    END.
  END.
  ASSIGN
    FI-Status:SCREEN-VALUE   = ""
    FI-AntValgt:SCREEN-VALUE = string(piAnt)
    .
END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

