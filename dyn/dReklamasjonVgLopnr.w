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
DEF VAR cStrKode     AS CHAR NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnButikk Vg LopNr Storl Antall TTId ~
tgButikk Butik btnNullstillFeilkode tgFeilkode FeilKode FeilNotat Btn_OK ~
BtnCancel btnFeilkode 
&Scoped-Define DISPLAYED-OBJECTS Vg LopNr Storl Antall TTId tgButikk Butik ~
ButNamn tgFeilkode FeilKode FeilkodeBeskrivelse FeilNotat FI-Header 

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
     SIZE 22.2 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE TTId AS INTEGER FORMAT ">9":U INITIAL 0 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "0",0
     DROP-DOWN-LIST
     SIZE 49 BY 1 NO-UNDO.

DEFINE VARIABLE FeilNotat AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 48 BY 8.33
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Antall AS DECIMAL FORMAT ">>9":U INITIAL 1 
     VIEW-AS FILL-IN 
     SIZE 9.2 BY 1 NO-UNDO.

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

DEFINE VARIABLE FI-Header AS CHARACTER FORMAT "X(40)":U INITIAL "Varegr, løpnr, størrlse og antall" 
      VIEW-AS TEXT 
     SIZE 46 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE LopNr AS INTEGER FORMAT ">>>>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10.2 BY 1 NO-UNDO.

DEFINE VARIABLE Storl AS CHARACTER FORMAT "X(14)":U 
     VIEW-AS FILL-IN 
     SIZE 17.8 BY 1 NO-UNDO.

DEFINE VARIABLE Vg AS INTEGER FORMAT ">>>>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10.2 BY 1 NO-UNDO.

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
     btnButikk AT ROW 5.33 COL 22.2 NO-TAB-STOP 
     Vg AT ROW 2.24 COL 10 COLON-ALIGNED HELP
          "'varegruppenummer" NO-LABEL
     LopNr AT ROW 2.24 COL 20.4 COLON-ALIGNED HELP
          "Løpenummer innenfor varegruppen" NO-LABEL
     Storl AT ROW 2.24 COL 30.8 COLON-ALIGNED NO-LABEL
     Antall AT ROW 2.29 COL 49 COLON-ALIGNED NO-LABEL
     TTId AT ROW 3.33 COL 12 NO-LABEL
     tgButikk AT ROW 4.43 COL 12.2 NO-TAB-STOP 
     Butik AT ROW 5.33 COL 10.2 COLON-ALIGNED
     ButNamn AT ROW 5.33 COL 25.2 COLON-ALIGNED HELP
          "Butikkens navn" NO-LABEL
     btnNullstillFeilkode AT ROW 6.86 COL 38.2 NO-TAB-STOP 
     tgFeilkode AT ROW 6.95 COL 12.2 NO-TAB-STOP 
     FeilKode AT ROW 7.91 COL 10.2 COLON-ALIGNED HELP
          "Feilkode som beskriver hva som er feil med varen."
     FeilkodeBeskrivelse AT ROW 8 COL 25 COLON-ALIGNED NO-LABEL
     FeilNotat AT ROW 9.1 COL 12.2 NO-LABEL
     Btn_OK AT ROW 17.67 COL 12.4 NO-TAB-STOP 
     BtnCancel AT ROW 17.67 COL 38 NO-TAB-STOP 
     btnFeilkode AT ROW 7.91 COL 22.2 NO-TAB-STOP 
     FI-Header AT ROW 1.38 COL 11 COLON-ALIGNED NO-LABEL
     SPACE(2.00) SKIP(16.90)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Vg/Løpen registrering"
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

/* SETTINGS FOR FILL-IN FI-Header IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       FI-Header:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* SETTINGS FOR COMBO-BOX TTId IN FRAME Dialog-Frame
   ALIGN-L                                                              */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Vg/Løpen registrering */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Antall
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Antall Dialog-Frame
ON TAB OF Antall IN FRAME Dialog-Frame
OR 'RETURN' OF Antall
DO:
    RUN LeaveOfField('Antall').
    IF int(Antall:SCREEN-VALUE) = 0 THEN
        RETURN NO-APPLY.
    ELSE DO:
      IF NOT tgButikk:CHECKED AND NOT tgFeilkode:CHECKED THEN
          APPLY 'CHOOSE' TO Btn_OK.
      ELSE APPLY LASTKEY.
    END.
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
      /*
      Vg:SCREEN-VALUE = ""
      LopNr:SCREEN-VALUE = ""
      Storl:SCREEN-VALUE = ""
      Antall:SCREEN-VALUE = "1"
      */
      .
  APPLY "ENTRY" TO Vg.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* Avslutt */
DO:
    ASSIGN
        ocKode = Vg:SCREEN-VALUE + "|" + 
                 LopNr:SCREEN-VALUE + "|" +
                 Storl:SCREEN-VALUE + "|" +
                 Antall:SCREEN-VALUE + "|" +
                 FeilKode:SCREEN-VALUE + "|" +
                 FeilNotat:SCREEN-VALUE + "|" +
                 Butik:SCREEN-VALUE + "|" +
                 ButNamn:SCREEN-VALUE  + "|" +
                 TTId:SCREEN-VALUE
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


&Scoped-define SELF-NAME LopNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL LopNr Dialog-Frame
ON TAB OF LopNr IN FRAME Dialog-Frame
OR 'RETURN' OF LopNr
DO:
    RUN LeaveOfField(SELF:NAME).
    IF int(LopNr:SCREEN-VALUE) = 0 THEN
        RETURN NO-APPLY.
    ELSE APPLY LASTKEY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Storl
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Storl Dialog-Frame
ON TAB OF Storl IN FRAME Dialog-Frame
OR 'RETURN' OF Storl
DO:
    RUN LeaveOfField(SELF:NAME).
    IF Storl:SCREEN-VALUE = "" THEN
        RETURN NO-APPLY.
    ELSE APPLY LASTKEY.
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


&Scoped-define SELF-NAME Vg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Vg Dialog-Frame
ON TAB OF Vg IN FRAME Dialog-Frame
OR 'RETURN' OF Vg
DO:
    RUN LeaveOfField(SELF:NAME).
    IF int(Vg:SCREEN-VALUE) = 0 THEN
        RETURN NO-APPLY.
    ELSE APPLY LASTKEY.
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
  DISPLAY Vg LopNr Storl Antall TTId tgButikk Butik ButNamn tgFeilkode FeilKode 
          FeilkodeBeskrivelse FeilNotat FI-Header 
      WITH FRAME Dialog-Frame.
  ENABLE btnButikk Vg LopNr Storl Antall TTId tgButikk Butik 
         btnNullstillFeilkode tgFeilkode FeilKode FeilNotat Btn_OK BtnCancel 
         btnFeilkode 
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
    WHEN 'Vg' THEN
    DO:
        cReturnValue = DYNAMIC-FUNCTION('getFieldValues','VarGr','WHERE VarGr.Vg = ' + Vg:SCREEN-VALUE,'Vg,VgBeskr').
        IF cReturnValue = '' OR cReturnValue = '?' OR cReturnValue = ? THEN
        DO:
          MESSAGE "Ugyldig varegruppe eller varegruppe ikke angitt."
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
          ASSIGN Vg:SCREEN-VALUE = "".
        END.
    END.
    WHEN 'LopNr' THEN
    DO:
      cReturnValue = DYNAMIC-FUNCTION('getFieldValues','ArtBas','WHERE ArtBas.Vg = ' + Vg:SCREEN-VALUE + 
                                        ' and ArtBas.LopNr = ' + LopNr:SCREEN-VALUE,'Vg,LopNr').
      IF cReturnValue = '' OR cReturnValue = '?' OR cReturnValue = ? THEN
      DO:
        MESSAGE "Ugyldig varegruppe/løpenummer." SKIP
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        ASSIGN Lopnr:SCREEN-VALUE = "".
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
    WHEN 'storl' THEN 
    DO:
      IF DYNAMIC-FUNCTION("runproc","bibl_fixstorl.p",storl:SCREEN-VALUE,?) THEN
          storl:SCREEN-VALUE = DYNAMIC-FUNCTION("getTransactionMessage").
      ELSE DO:
          MESSAGE "Feil ved kovnertering av størrelse:" SKIP
              DYNAMIC-FUNCTION("getTransactionMessage")
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
          storl:SCREEN-VALUE = "".
          RETURN.
      END.
      cReturnValue = DYNAMIC-FUNCTION('getFieldValues','strkonv','WHERE strkonv.storl = ' + QUOTER(storl:SCREEN-VALUE),'storl').
      IF cReturnValue = '' OR cReturnValue = '?' OR cReturnValue = ? THEN
      DO:
        MESSAGE "Ugyldig størrelse angitt." + "'" + cReturnValue + "'"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        storl:SCREEN-VALUE = "".
      END.
      ELSE
      DO:
        storl:SCREEN-VALUE = cReturnValue.
      END.
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

