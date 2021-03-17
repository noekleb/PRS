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
/*          This .W file was created with the Progress AppBulder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEF INPUT PARAMETER  dcArtikkelNr AS DEC  NO-UNDO.

/* Local Variable Definitions ---                                       */
DEF BUFFER bVarGr FOR VarGr.

DEF var iOVg          AS INT  NO-UNDO.
DEF VAR iVg           AS INT  NO-UNDO.
DEF VAR cReturn-Value AS CHAR INITIAL "AVBRYT" NO-UNDO.
DEFINE VARIABLE cForenkletArtkor AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iMaxLopnr AS INTEGER     NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BUTTON-SokVg RECT-55 FI-Vg Btn_OK Btn_Cancel ~
Btn_Help 
&Scoped-Define DISPLAYED-OBJECTS FI-OVg FI-OVgTekst FI-Vg FI-VgTekst 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Avbryt" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_Help 
     LABEL "&Hjelp" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON BUTTON-SokVg 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE VARIABLE FI-OVg AS INTEGER FORMAT ">>>>>9":U INITIAL 0 
     LABEL "Gammel varegruppe" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 TOOLTIP "Søk - F10" NO-UNDO.

DEFINE VARIABLE FI-OVgTekst AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 38 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Vg AS INTEGER FORMAT ">>>>>9":U INITIAL 0 
     LABEL "Ny varegruppe" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 TOOLTIP "Søk - F10" NO-UNDO.

DEFINE VARIABLE FI-VgTekst AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 38 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-55
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 77 BY 9.52.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     BUTTON-SokVg AT ROW 4.81 COL 33.2
     FI-OVg AT ROW 3.62 COL 21 COLON-ALIGNED
     FI-OVgTekst AT ROW 3.62 COL 36 COLON-ALIGNED NO-LABEL
     FI-Vg AT ROW 4.81 COL 21 COLON-ALIGNED
     FI-VgTekst AT ROW 4.81 COL 36 COLON-ALIGNED NO-LABEL
     Btn_OK AT ROW 11 COL 1
     Btn_Cancel AT ROW 11 COL 16
     Btn_Help AT ROW 11 COL 63
     RECT-55 AT ROW 1.24 COL 1
     SPACE(0.00) SKIP(1.52)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Bytt varegruppe"
         CANCEL-BUTTON Btn_Cancel.


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
   FRAME-NAME                                                           */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN FI-OVg IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-OVgTekst IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-VgTekst IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX Dialog-Frame
/* Query rebuild information for DIALOG-BOX Dialog-Frame
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX Dialog-Frame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON GO OF FRAME Dialog-Frame /* Bytt varegruppe */
DO:
  RUN LagrePost.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Bytt varegruppe */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Help
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Help Dialog-Frame
ON CHOOSE OF Btn_Help IN FRAME Dialog-Frame /* Hjelp */
OR HELP OF FRAME {&FRAME-NAME}
DO: /* Call Help Function (or a simple message). */
  MESSAGE "Help for File: {&FILE-NAME}" VIEW-AS ALERT-BOX INFORMATION.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:
  ASSIGN
      cReturn-Value = "OK"
      .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokVg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokVg Dialog-Frame
ON CHOOSE OF BUTTON-SokVg IN FRAME Dialog-Frame /* ... */
or F10 of FI-Vg
DO:
    DEF VAR cTekst AS CHAR NO-UNDO.
    cTekst = "Vg".
    RUN JBoxDLookup.w ("VarGr;vg;vgBeskr,HuvGr;Hg;HgBeskr,Moms;MomsProc@3,Avdeling;AvdelingNr;AvdelingNavn",
                       "WHERE true,FIRST HuvGr OF VarGr NO-LOCK, FIRST Moms of VarGr, FIRST Avdeling OF HuvGr NO-LOCK",
                       INPUT-OUTPUT cTekst).


    IF RETURN-VALUE = "AVBRYT" THEN
        RETURN NO-APPLY.
    FIND VarGr NO-LOCK WHERE
      VarGr.Vg = INT(cTekst) NO-ERROR.
    IF AVAILABLE VarGr THEN
    DO:
        /* Legger opp verdier I de aktuelle feltene */
        ASSIGN
          FI-Vg:SCREEN-VALUE      = cTekst
          FI-VgTekst:SCREEN-VALUE = VarGr.VgBeskr
          .
    END.
    ELSE DO:
        /* Legger opp verdier I de aktuelle feltene */
        ASSIGN
          FI-Vg:SCREEN-VALUE       = ''
          FI-VgTekst:SCREEN-VALUE  = ''
          .
    END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-Vg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Vg Dialog-Frame
ON LEAVE OF FI-Vg IN FRAME Dialog-Frame /* Ny varegruppe */
DO:
  FIND VarGr NO-LOCK WHERE
      VarGr.Vg = INPUT FI-Vg NO-ERROR.
  IF NOT AVAILABLE VarGr THEN
  DO:
      BELL.
      MESSAGE "Ugyldig varegruppe!"
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
      RETURN NO-APPLY.
  END.
  ELSE
      ASSIGN
          FI-Vg:SCREEN-VALUE      = STRING(VarGr.Vg)
          FI-VgTekst:SCREEN-VALUE = VarGr.VgBeskr
          .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.

{syspara.i 2 5 103 cForenkletArtkor}
IF cForenkletArtkor = "1" THEN
    iMaxLopnr = 999999.
ELSE
    iMaxLopnr = 9999.
FIND ArtBas NO-LOCK WHERE
    ArtBas.ArtikkelNr = dcArtikkelNr NO-ERROR.
IF NOT AVAILABLE ArtBas THEN
DO:
    BELL.
    MESSAGE "Ukjent artikkelnummer " dcArtikkelNr "!"
        VIEW-AS ALERT-BOX ERROR BUTTONS OK.
    RETURN.
END.

IF ArtBas.ArtikkelNr = ArtBas.Vg THEN DO:
    MESSAGE "Varen er varegruppevare, bytte ikke tillatt."
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN NO-APPLY.
END.

FIND bVarGr NO-LOCK WHERE
    bVarGr.Vg = ArtBas.Vg NO-ERROR.
FIND VarGr NO-LOCK WHERE
    VarGr.Vg = ArtBas.Vg NO-ERROR.
ASSIGN
    iVg  = ArtBas.Vg
    iOVg = Artbas.Vg
    .

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.

  /* Viser frem initverdier. */
  IF AVAILABLE bVarGr THEN
      ASSIGN
        FI-OVg:SCREEN-VALUE IN FRAME Dialog-Frame      = string(bVarGr.Vg)
        FI-OVgTekst:SCREEN-VALUE IN FRAME Dialog-Frame = bVarGr.VgBeskr
      .
  ELSE
      ASSIGN
        FI-OVg:SCREEN-VALUE IN FRAME Dialog-Frame      = ""
        FI-OVgTekst:SCREEN-VALUE IN FRAME Dialog-Frame = "" 
      .

  /* Viser frem initverdier. */
  IF AVAILABLE VarGr THEN
      ASSIGN
        FI-Vg:SCREEN-VALUE IN FRAME Dialog-Frame      = string(VarGr.Vg)
        FI-VgTekst:SCREEN-VALUE IN FRAME Dialog-Frame = VarGr.VgBeskr
      .
  ELSE
      ASSIGN
        FI-Vg:SCREEN-VALUE IN FRAME Dialog-Frame      = ""
        FI-VgTekst:SCREEN-VALUE IN FRAME Dialog-Frame = "" 
      .

  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

RETURN cReturn-Value.

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
  DISPLAY FI-OVg FI-OVgTekst FI-Vg FI-VgTekst 
      WITH FRAME Dialog-Frame.
  ENABLE BUTTON-SokVg RECT-55 FI-Vg Btn_OK Btn_Cancel Btn_Help 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LagrePost Dialog-Frame 
PROCEDURE LagrePost :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER bufArtBas FOR ArtBas.
  DEF BUFFER bufVArGr  FOR VarGr.

  DEF VAR piLopNr AS INT NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:

      ASSIGN
          piLopNr = ArtBas.LopNr
          .

      FIND bufVarGr NO-LOCK WHERE
          bufVarGr.Vg = INPUT FI-Vg.

      /* Sjekker om artikkelen må tildeles nytt løpenummer */
      IF CAN-FIND(bufArtBas WHERE 
                  bufArtBas.Vg    = INPUT FI-Vg AND
                  bufArtBas.LopNr = ArtBas.LopNr) THEN
      DO FOR bufArtBas:
          FIND LAST bufArtBas NO-LOCK WHERE
              bufArtBas.Vg = INPUT FI-Vg USE-INDEX ArtIn.
          ASSIGN
              piLopNr = bufArtBas.LopNr + 1.

/*           IF piLopNr > 9999 THEN */
          IF piLopNr > iMaxLopnr THEN
          DO:
              MESSAGE "Det finnes allerede en artikkel på valgt varegruppe med det samme løpenummer." SKIP
                      "Kan ikke bytte varegruppe. Nummerserie er full."
                  VIEW-AS ALERT-BOX INFO BUTTONS OK.
              cReturn-Value = "AVBRYT".
              RETURN cReturn-Value.
          END.
          MESSAGE "Nytt løpenummer er tildelt artikkelen." SKIP
                  "Gammelt løpenummer " + string(ArtBas.LopNr) + 
                  ". Nytt løpenummer " + STRING(piLopNr) + "."
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
      END.

      IF bufVarGr.MomsKod <> VarGr.MomsKod THEN
      DO:
          MESSAGE "Det er en annen momskode på den varegruppe som artikkelen er flyttet til." SKIP
                  "Artikkelens kalkyle må regnes om . Må gjøres manuelt."
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
      END.

      DO FOR bufArtBas:
        FIND bufArtBas  NO-LOCK WHERE
            bufArtBas.ArtikkelNr = ArtBas.ArtikkelNr NO-ERROR.
        IF NOT AVAILABLE bufArtBas THEN
        DO:
          MESSAGE "Artikkelen ikke tilgjengelig for endring."
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
          RETURN NO-APPLY "AVBRYT".
        END.
        ELSE DO TRANSACTION:
            FIND CURRENT bufArtBas EXCLUSIVE-LOCK.
            ASSIGN
                bufArtBas.Vg        = INPUT FI-Vg
                bufArtBas.LopNr     = piLopNr
                bufArtBas.Hg        = bufVarGr.Hg
                /*bufArtBas.BongTekst = VarGr.VgBeskr*/
                .
            FIND CURRENT bufArtBas NO-LOCK.
        END.

        /* Translogg */
        FOR EACH TransLogg EXCLUSIVE-LOCK WHERE
            TransLogg.ArtikkelNr = ArtBas.ArtikkelNr:
            ASSIGN
                TransLogg.Vg    = INPUT FI-Vg
                TransLogg.LopNr = piLopNr
                .
        END.

        /* KundeTrans */
        FOR EACH Kunde NO-LOCK,
            EACH KundeTrans OF Kunde EXCLUSIVE-LOCK WHERE
            KundeTrans.ArtikkelNr = ArtBas.ArtikkelNr:
            ASSIGN
                KundeTrans.Vg    = INPUT FI-Vg
                KundeTrans.LopNr = piLopNr
                .
        END.

        /* Medlemstrans */
        FOR EACH Medlem NO-LOCK,
            EACH MedTrans OF Medlem EXCLUSIVE-LOCK WHERE
            MedTrans.ArtikkelNr = ArtBas.ArtikkelNr:
            ASSIGN
                MedTrans.Vg    = INPUT FI-Vg
                MedTrans.LopNr = piLopNr
                .
        END.

        /* Artlag */
        FOR EACH ArtLag EXCLUSIVE-LOCK WHERE
            ArtLag.ArtikkelNr = ArtBas.ArtikkelNr:
            ASSIGN
                ArtLag.Vg    = INPUT FI-Vg
                ArtLag.LopNr = piLopNr
                .
        END.

        RELEASE bufArtBas.
      END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setLookupAttributes Dialog-Frame 
PROCEDURE setLookupAttributes :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihBrowse AS HANDLE NO-UNDO.
DEF INPUT PARAM ihDummy1 AS HANDLE NO-UNDO.
DEF INPUT PARAM ihDummy2 AS HANDLE NO-UNDO.

DEF VAR hCombo AS HANDLE NO-UNDO.

IF ihBrowse:QUERY:GET-BUFFER-HANDLE(1):NAME = "VarGr" THEN DO:


    CREATE COMBO-BOX hCombo 
        ASSIGN DELIMITER        = "|"
               DATA-TYPE        = "CHARACTER"
               FORMAT           = "x(256)"
               NAME             = "cmbHgr"
               SUBTYPE          = "DROP-DOWN-LIST"
               LIST-ITEM-PAIRS  = "<Velg hovedgruppe>|0|" + DYNAMIC-FUNCTION("getFieldList","HuvGr;HgBeskr;Hg","WHERE TRUE BY HgBeskr")
               INNER-LINES      = 50
               FRAME            = ihBrowse:FRAME
               X                = ihBrowse:X + 150
               Y                = ihBrowse:Y - 26
               WIDTH-PIXELS     = 300
               VISIBLE          = YES
               SENSITIVE        = TRUE
               HELP             = "Velg hovedgruppe"
               TOOLTIP          = "Velg hovedgruppe"
               TRIGGERS:
                 ON VALUE-CHANGED PERSISTENT RUN setVgLookupFilter IN THIS-PROCEDURE (ihBrowse,hCombo).
  /*                ON TAB           PERSISTENT RUN DoProcessEvent (SOURCE-PROCEDURE,"leave").     */
  /*                ON BACK-TAB      PERSISTENT RUN DoProcessEvent (SOURCE-PROCEDURE,"leave").     */
  /*                ON RETURN        PERSISTENT RUN DoProcessEvent (SOURCE-PROCEDURE,"leave").     */
  /*                ON ENTRY         PERSISTENT RUN DoProcessEvent (SOURCE-PROCEDURE,"entry").     */
  /*                ON END-ERROR     PERSISTENT RUN DoProcessEvent (SOURCE-PROCEDURE,"end-error"). */
               END TRIGGERS.
        ASSIGN hCombo:SCREEN-VALUE = '0'.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setVgLookupFilter Dialog-Frame 
PROCEDURE setVgLookupFilter :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihBrowse AS HANDLE NO-UNDO.
DEF INPUT PARAM ihCombo  AS HANDLE NO-UNDO.

DYNAMIC-FUNCTION("setAttribute",ihBrowse,"queryFilter",
                 IF ihCombo:SCREEN-VALUE NE ? AND ihCombo:SCREEN-VALUE NE "" THEN 
                   " AND Hg = " + ihCombo:SCREEN-VALUE
                 ELSE "").

RUN InvokeMethod(ihBrowse,"OpenQuery").


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

