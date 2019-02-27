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
DEF INPUT  PARAM cButik AS CHAR NO-UNDO.
DEF INPUT  PARAM cTekst AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocKode AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOk   AS LOG  NO-UNDO.

/* Local Variable Definitions ---                                       */

DEF VAR cReturnValue AS CHAR NO-UNDO.
DEF VAR cKode        AS CHAR NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnButikk FI-Kode TTId tgButikk Butik ~
btnNullstillFeilkode tgFeilkode FeilKode FeilNotat BtnCancel Btn_OK ~
btnFeilkode 
&Scoped-Define DISPLAYED-OBJECTS FI-Kode TTId tgButikk Butik ButNamn ~
tgFeilkode FeilKode FeilkodeBeskrivelse FeilNotat 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnButikk  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON BtnCancel AUTO-END-KEY DEFAULT 
     LABEL "Avbryt" 
     SIZE 22.2 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON btnFeilkode  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON btnNullstillFeilkode 
     LABEL "Nullstill feilkode" 
     SIZE 22 BY 1.14.

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "Avslutt" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE TTId AS INTEGER FORMAT ">9":U INITIAL 0 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "0",0
     DROP-DOWN-LIST
     SIZE 49 BY 1 NO-UNDO.

DEFINE VARIABLE FeilNotat AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 48 BY 8.24
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Butik AS INTEGER FORMAT ">>>>>9":U INITIAL 0 
     LABEL "Butikk" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE ButNamn AS CHARACTER FORMAT "x(20)" 
     VIEW-AS FILL-IN 
     SIZE 33 BY 1 NO-UNDO.

DEFINE VARIABLE FeilKode AS INTEGER FORMAT ">9" INITIAL 0 
     LABEL "Feilkode" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1.

DEFINE VARIABLE FeilkodeBeskrivelse AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN NATIVE 
     SIZE 33 BY .86 NO-UNDO.

DEFINE VARIABLE FI-Kode AS CHARACTER FORMAT "X(256)":U 
     LABEL "Strekkode" 
     VIEW-AS FILL-IN 
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE tgButikk AS LOGICAL INITIAL NO 
     LABEL "Angi butikk" 
     VIEW-AS TOGGLE-BOX
     SIZE 20 BY .81 NO-UNDO.

DEFINE VARIABLE tgFeilkode AS LOGICAL INITIAL NO 
     LABEL "Angi feilkode" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     btnButikk AT ROW 4.48 COL 23 NO-TAB-STOP 
     FI-Kode AT ROW 1.48 COL 11 COLON-ALIGNED
     TTId AT ROW 2.52 COL 13 NO-LABEL
     tgButikk AT ROW 3.57 COL 13
     Butik AT ROW 4.48 COL 11 COLON-ALIGNED
     ButNamn AT ROW 4.48 COL 26 COLON-ALIGNED HELP
          "Butikkens navn" NO-LABEL
     btnNullstillFeilkode AT ROW 6 COL 39 NO-TAB-STOP 
     tgFeilkode AT ROW 6.1 COL 13
     FeilKode AT ROW 7.05 COL 11 COLON-ALIGNED HELP
          "Feilkode som beskriver hva som er feil med varen."
     FeilkodeBeskrivelse AT ROW 7.14 COL 25.8 COLON-ALIGNED NO-LABEL
     FeilNotat AT ROW 8.24 COL 13 NO-LABEL
     BtnCancel AT ROW 16.71 COL 39
     Btn_OK AT ROW 16.76 COL 13.4
     btnFeilkode AT ROW 7.05 COL 23 NO-TAB-STOP 
     SPACE(35.19) SKIP(9.94)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Strekkoderegistrering"
         DEFAULT-BUTTON Btn_OK CANCEL-BUTTON BtnCancel.


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
   NOT-VISIBLE FRAME-NAME                                               */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE.

/* SETTINGS FOR FILL-IN ButNamn IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FeilkodeBeskrivelse IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       FeilkodeBeskrivelse:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* SETTINGS FOR COMBO-BOX TTId IN FRAME Dialog-Frame
   ALIGN-L                                                              */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Strekkoderegistrering */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnButikk
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnButikk Dialog-Frame
ON CHOOSE OF btnButikk IN FRAME Dialog-Frame /* ... */
OR F10 OF Butik
DO:
  DEF VAR cLookupValue AS CHAR NO-UNDO.

  cLookupValue = "Butik;ButNamn".

  RUN JBoxDLookup.w ("Butiker;Butik;ButNamn", 
                     "WHERE true",
                     INPUT-OUTPUT cLookupValue).
  IF cLookupValue NE "" THEN 
  DO:
    ASSIGN 
      Butik:SCREEN-VALUE   = ENTRY(1,cLookupValue,"|")
      butNamn:SCREEN-VALUE = ENTRY(2,cLookupValue,'|')
    .
    APPLY "ANY-PRINTABLE" TO Butik.
  END.
  APPLY "entry" TO Butik.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnCancel Dialog-Frame
ON CHOOSE OF BtnCancel IN FRAME Dialog-Frame /* Avbryt */
DO:
    ASSIGN
      obOk = FALSE
      .

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnFeilkode
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnFeilkode Dialog-Frame
ON CHOOSE OF btnFeilkode IN FRAME Dialog-Frame /* ... */
OR F10 OF Feilkode
DO:
  DEF VAR cLookupValue AS CHAR NO-UNDO.

  cLookupValue = "FeilKode;Beskrivelse".

  RUN JBoxDLookup.w ("Feilkode;FeilKode;Beskrivelse", 
                     "WHERE true",
                     INPUT-OUTPUT cLookupValue).

  IF cLookupValue NE "" THEN 
  DO:
    ASSIGN 
      FeilKode:SCREEN-VALUE            = ENTRY(1,cLookupValue,"|")
      FeilKodeBeskrivelse:SCREEN-VALUE = ENTRY(2,cLookupValue,"|")
    .
    APPLY 'ANY-PRINTABLE' TO FeilKode.
  END.
  APPLY "entry" TO FeilKode.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnNullstillFeilkode
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnNullstillFeilkode Dialog-Frame
ON CHOOSE OF btnNullstillFeilkode IN FRAME Dialog-Frame /* Nullstill feilkode */
DO:
  ASSIGN
      FeilKode:SCREEN-VALUE = ""
      FeilNotat:SCREEN-VALUE = ""
      FeilKodeBeskrivelse:SCREEN-VALUE = ""
      .
  APPLY "ENTRY" TO FI-Kode.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* Avslutt */
DO:
    ASSIGN
        cKode = FI-Kode:SCREEN-VALUE.
    IF (LENGTH(cKode) > 6 AND LENGTH(cKode) < 13) THEN
        cKode = FILL('0',13 - LENGTH(cKode)) + cKode.
    ASSIGN
        ocKode = cKode + "|" + 
                 (IF FeilKode:SCREEN-VALUE = ? THEN '' ELSE FeilKode:SCREEN-VALUE) + "|" +
                 (IF FeilNotat:SCREEN-VALUE = ? THEN '' ELSE FeilNotat:SCREEN-VALUE) + "|" +
                 (IF Butik:SCREEN-VALUE = ? THEN '' ELSE Butik:SCREEN-VALUE) + "|" +
                 (IF ButNamn:SCREEN-VALUE = ? THEN '' ELSE ButNamn:SCREEN-VALUE) + "|" +
                 (IF TTId:SCREEN-VALUE = ? THEN '' ELSE TTId:SCREEN-VALUE)
        obOk   = TRUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Butik
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Butik Dialog-Frame
ON TAB OF Butik IN FRAME Dialog-Frame /* Butikk */
OR 'RETURN' OF Butik
DO:
    RUN LeaveOfField(SELF:NAME).
    IF int(Butik:SCREEN-VALUE) = 0 THEN
        RETURN NO-APPLY.
    ELSE
        APPLY LASTKEY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FeilKode
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FeilKode Dialog-Frame
ON TAB OF FeilKode IN FRAME Dialog-Frame /* Feilkode */
OR 'RETURN' OF FeilKode
DO:
    RUN LeaveOfField(SELF:NAME).
    IF int(FeilKode:SCREEN-VALUE) = 0 THEN
        RETURN NO-APPLY.
    ELSE
        APPLY LASTKEY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FeilNotat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FeilNotat Dialog-Frame
ON TAB OF FeilNotat IN FRAME Dialog-Frame
OR "RETURN" OF FeilNotat
DO:
  APPLY "ENTRY" TO Btn_OK.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-Kode
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Kode Dialog-Frame
ON TAB OF FI-Kode IN FRAME Dialog-Frame /* Strekkode */
OR 'RETURN' OF FI-Kode
DO:
    RUN LeaveOfField('Strekkode').
    IF FI-Kode:SCREEN-VALUE = '' THEN
        RETURN NO-APPLY.
    ELSE DO:
        IF tgButikk:checked THEN DO:
            APPLY 'ENTRY' TO Butik.
            RETURN NO-APPLY.
        END.
        ELSE IF tgFeilkode:CHECKED THEN DO:
            APPLY 'ENTRY' TO FeilKode.
            RETURN NO-APPLY.
        END.
        ELSE DO:
            APPLY 'choose' TO Btn_Ok.
        END.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tgButikk
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tgButikk Dialog-Frame
ON VALUE-CHANGED OF tgButikk IN FRAME Dialog-Frame /* Angi butikk */
DO:
  RUN setButikkAktivPassiv.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tgFeilkode
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tgFeilkode Dialog-Frame
ON VALUE-CHANGED OF tgFeilkode IN FRAME Dialog-Frame /* Angi feilkode */
DO:
  RUN setFeilkodeAktivPassiv.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT EQ ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.

{incl/frametrigg.i}

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  RUN InitWindow.
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
  DISPLAY FI-Kode TTId tgButikk Butik ButNamn tgFeilkode FeilKode 
          FeilkodeBeskrivelse FeilNotat 
      WITH FRAME Dialog-Frame.
  ENABLE btnButikk FI-Kode TTId tgButikk Butik btnNullstillFeilkode tgFeilkode 
         FeilKode FeilNotat BtnCancel Btn_OK btnFeilkode 
      WITH FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitWindow Dialog-Frame 
PROCEDURE InitWindow :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
  DYNAMIC-FUNCTION("InitTranslation",FRAME {&FRAME-NAME}:HANDLE).
  ASSIGN FRAME {&FRAME-NAME}:TITLE = FRAME {&FRAME-NAME}:TITLE.

  ASSIGN 
    TTId:DELIMITER = "|"
    TTId:LIST-ITEM-PAIRS = "|0|" +  DYNAMIC-FUNCTION("getFieldList","sysPara;ParaNr|Parameter1;ParaNr","where sysHId = 15 and sysGr = 12")
  . 

  RUN setButikkAktivPassiv.
  RUN setFeilkodeAktivPassiv.
  ASSIGN
      Butik:SCREEN-VALUE     = cButik
      FeilKode:SCREEN-VALUE  = ENTRY(1,cTekst,"|")
      FeilNotat:SCREEN-VALUE = ENTRY(2,cTekst,"|")
      TTId:SCREEN-VALUE      = IF TRIM(ENTRY(3,cTekst,"|")) = '' THEN '4' ELSE ENTRY(3,cTekst,"|") 
      .
  IF INT(Butik:SCREEN-VALUE) <> 0 THEN DO:
      cReturnValue = DYNAMIC-FUNCTION('getFieldValues','Butiker','WHERE Butiker.Butik = ' + Butik:SCREEN-VALUE,'ButNamn').
      IF cReturnValue = '' OR cReturnValue = '?' OR cReturnValue = ? THEN. /* Gjør ingenting. */
      ELSE ButNamn:SCREEN-VALUE = cReturnValue.
  END.

  IF INT(FeilKode:SCREEN-VALUE) <> 0 THEN DO:
      cReturnValue = DYNAMIC-FUNCTION('getFieldValues','FeilKode','WHERE FeilKode.FeilKode = ' + FeilKode:SCREEN-VALUE,'Beskrivelse').
      IF cReturnValue = '' OR cReturnValue = '?' OR cReturnValue = ? THEN. /* Gjør ingenting. */
      ELSE FeilKodeBeskrivelse:SCREEN-VALUE = cReturnValue.
  END.

END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LeaveOfField Dialog-Frame 
PROCEDURE LeaveOfField :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM icFieldName AS CHAR NO-UNDO.

DEF VAR cReturnValue AS CHAR NO-UNDO.
DEF VAR fArtNr       AS DEC NO-UNDO.
DEF VAR cKode        AS CHAR NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
  CASE icFieldName:
    WHEN 'Strekkode' THEN
    DO:
      cReturnValue = DYNAMIC-FUNCTION("getFieldValues","Strekkode","WHERE Strekkode.Kode = '" + FI-Kode:SCREEN-VALUE + "'","StrKode").
      IF cReturnValue = '' OR cReturnValue = '?' OR cReturnValue = ? THEN
      DO:
        MESSAGE "Ukjent strekkode!" SKIP
                "Return-value:" cReturnValue SKIP
                "Fuksjonen:" DYNAMIC-FUNCTION("getFieldValues","Strekkode","WHERE Strekkode.Kode = '" + FI-Kode:SCREEN-VALUE + "'","StrKode") SKIP
                'FI-Kode:SCREEN-VALUE' FI-Kode:SCREEN-VALUE
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        FI-Kode:SCREEN-VALUE = ''.
        RETURN "ERROR".
      END.
    END.
    WHEN 'feilkode' THEN
    DO:
      cReturnValue = DYNAMIC-FUNCTION('getFieldValues','Feilkode','WHERE FeilKode.feilkode = ' + feilkode:SCREEN-VALUE,'Feilkode,beskrivelse').
      IF cReturnValue = '' OR cReturnValue = '?' OR cReturnValue = ? THEN
      DO:
        APPLY 'CHOOSE' TO btnFeilkode.
      END.
      ELSE
          ASSIGN 
            FeilkodeBeskrivelse:SCREEN-VALUE = ENTRY(2,cReturnValue,'|').
    END.
    WHEN 'Butik' THEN
    DO:
      cReturnValue = DYNAMIC-FUNCTION('getFieldValues','Butiker','WHERE Butiker.Butik = ' + Butik:SCREEN-VALUE,'Butik,ButNamn').
      IF cReturnValue = '' OR cReturnValue = '?' OR cReturnValue = ? THEN
      DO:
        APPLY 'CHOOSE' TO btnButikk.
        RETURN NO-APPLY.
      END.
      ELSE
      DO:
        ASSIGN 
          ButNamn:SCREEN-VALUE = ENTRY(2,cReturnValue,'|')
        .
      END.
    END.
  END CASE.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setButikkAktivPassiv Dialog-Frame 
PROCEDURE setButikkAktivPassiv :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

ASSIGN
    Butik:SENSITIVE IN FRAME Dialog-Frame     = tgButikk:CHECKED
    btnButikk:SENSITIVE IN FRAME Dialog-Frame = tgButikk:CHECKED
    .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setFeilkodeAktivPassiv Dialog-Frame 
PROCEDURE setFeilkodeAktivPassiv :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

ASSIGN
    FeilKode:SENSITIVE IN FRAME Dialog-Frame    = tgFeilkode:CHECKED
    btnFeilkode:SENSITIVE IN FRAME Dialog-Frame = tgFeilkode:CHECKED
    FeilNotat:SENSITIVE IN FRAME Dialog-Frame   = tgFeilkode:CHECKED
    .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

