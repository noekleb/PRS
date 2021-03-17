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
&IF DEFINED(UIB_is_Running) NE 0 &THEN
  DEF VAR icFunctions AS CHAR NO-UNDO.
  DEF VAR oiSelection AS INT  NO-UNDO.
  DEF VAR ioTellenr AS INT NO-UNDO.
&ELSE
  DEF INPUT  PARAM icFunctions AS CHAR NO-UNDO.
  DEF OUTPUT PARAM oiSelection AS INT  NO-UNDO.
  DEF input  param ioTellenr    AS INT NO-UNDO.
&ENDIF
/* Local Variable Definitions ---                                       */
DEF VAR iTellenr AS INT NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnTellelisteFull btnTellelisteRull ~
btnTellelisteKontroll btnLokasjonsliste btnLokasjonsTransliste 
&Scoped-Define DISPLAYED-OBJECTS EDITOR-Info-3 EDITOR-Info EDITOR-Full ~
EDITOR-Rull EDITOR-Kontroll EDITOR-Lokasjon EDITOR-Trans 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnLokasjonsliste AUTO-GO 
     LABEL "Lokasjonsliste" 
     SIZE 33 BY 5.1
     FONT 6.

DEFINE BUTTON btnLokasjonsTransliste AUTO-GO 
     LABEL "Transaksjonsliste" 
     SIZE 33 BY 5.1
     FONT 6.

DEFINE BUTTON btnTellelisteFull AUTO-GO 
     LABEL "Full varetelling" 
     SIZE 33 BY 5.1
     FONT 6.

DEFINE BUTTON btnTellelisteKontroll AUTO-GO 
     LABEL "Kontrolltelling" 
     SIZE 33 BY 5.1
     FONT 6.

DEFINE BUTTON btnTellelisteRull AUTO-GO 
     LABEL "Rullerende varetelling" 
     SIZE 33 BY 5.1
     FONT 6.

DEFINE VARIABLE EDITOR-Full AS CHARACTER 
     VIEW-AS EDITOR
     SIZE 139.4 BY 4.86
     FONT 6 NO-UNDO.

DEFINE VARIABLE EDITOR-Info AS CHARACTER 
     VIEW-AS EDITOR
     SIZE 165 BY 4.71
     BGCOLOR 15 FGCOLOR 0 FONT 6 NO-UNDO.

DEFINE VARIABLE EDITOR-Info-3 AS CHARACTER 
     VIEW-AS EDITOR
     SIZE 173 BY 6.57
     BGCOLOR 12 FGCOLOR 0 FONT 6 NO-UNDO.

DEFINE VARIABLE EDITOR-Kontroll AS CHARACTER 
     VIEW-AS EDITOR
     SIZE 139.4 BY 4.86
     FONT 6 NO-UNDO.

DEFINE VARIABLE EDITOR-Lokasjon AS CHARACTER 
     VIEW-AS EDITOR
     SIZE 139.4 BY 4.86
     FONT 6 NO-UNDO.

DEFINE VARIABLE EDITOR-Rull AS CHARACTER 
     VIEW-AS EDITOR
     SIZE 139.4 BY 4.86
     FONT 6 NO-UNDO.

DEFINE VARIABLE EDITOR-Trans AS CHARACTER 
     VIEW-AS EDITOR
     SIZE 139.4 BY 4.86
     FONT 6 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     EDITOR-Info-3 AT ROW 1.1 COL 2 NO-LABEL 
     EDITOR-Info AT ROW 2.05 COL 6 NO-LABEL
     btnTellelisteFull AT ROW 7.67 COL 1.2
     EDITOR-Full AT ROW 7.81 COL 34.8 NO-LABEL
     btnTellelisteRull AT ROW 12.81 COL 1.2
     EDITOR-Rull AT ROW 12.81 COL 34.8 NO-LABEL
     btnTellelisteKontroll AT ROW 17.95 COL 1.2
     EDITOR-Kontroll AT ROW 17.95 COL 34.8 NO-LABEL
     btnLokasjonsliste AT ROW 23.1 COL 1.2
     EDITOR-Lokasjon AT ROW 23.1 COL 34.8 NO-LABEL
     btnLokasjonsTransliste AT ROW 28.24 COL 1.2
     EDITOR-Trans AT ROW 28.24 COL 34.8 NO-LABEL
     SPACE(1.59) SKIP(0.46)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Velg type varetelling".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Dialog-Box
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB Dialog-Frame 
/* ************************* Included-Libraries *********************** */

{incl/DevMode.i}
{incl/CustDevMode.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   FRAME-NAME                                                           */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR EDITOR EDITOR-Full IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR EDITOR EDITOR-Info IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR EDITOR EDITOR-Info-3 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR EDITOR EDITOR-Kontroll IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR EDITOR EDITOR-Lokasjon IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR EDITOR EDITOR-Rull IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR EDITOR EDITOR-Trans IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Velg type varetelling */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnLokasjonsliste
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnLokasjonsliste Dialog-Frame
ON CHOOSE OF btnLokasjonsliste IN FRAME Dialog-Frame /* Lokasjonsliste */
DO:
  oiSelection = 4.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnLokasjonsTransliste
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnLokasjonsTransliste Dialog-Frame
ON CHOOSE OF btnLokasjonsTransliste IN FRAME Dialog-Frame /* Transaksjonsliste */
DO:
  oiSelection = 5.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnTellelisteFull
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnTellelisteFull Dialog-Frame
ON CHOOSE OF btnTellelisteFull IN FRAME Dialog-Frame /* Full varetelling */
DO:
  oiSelection = 1.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnTellelisteKontroll
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnTellelisteKontroll Dialog-Frame
ON CHOOSE OF btnTellelisteKontroll IN FRAME Dialog-Frame /* Kontrolltelling */
DO:
  oiSelection = 3.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnTellelisteRull
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnTellelisteRull Dialog-Frame
ON CHOOSE OF btnTellelisteRull IN FRAME Dialog-Frame /* Rullerende varetelling */
DO:
  oiSelection = 2.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.

{incl/frametrigg.i}

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.

  RUN InitializeObject.

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
  DISPLAY EDITOR-Info-3 EDITOR-Info EDITOR-Full EDITOR-Rull EDITOR-Kontroll 
          EDITOR-Lokasjon EDITOR-Trans 
      WITH FRAME Dialog-Frame.
  ENABLE btnTellelisteFull btnTellelisteRull btnTellelisteKontroll 
         btnLokasjonsliste btnLokasjonsTransliste 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitializeObject Dialog-Frame 
PROCEDURE InitializeObject :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:

  /*Forettningsregel*/
  iTellenr = INT(DYNAMIC-FUNCTION("getFieldValues","tellehode",
                                  "WHERE TelleType = 1 and TelleHode.Butikkliste = '" + string(ioTelleNr) + "' and TelleHode.ButikkListe > '' and ttid = 9 and oppdatert = ?","tellenr")).

  IF iTellenr GT 0 THEN 
  DO:
    ASSIGN
        btnTellelisteFull:SENSITIVE     = false
        btnTellelisteRull:SENSITIVE     = false
        btnTellelisteKontroll:SENSITIVE = false
        .
  END.
  IF NOT CAN-DO("ken1,tomn",USERID("skotex")) THEN
  DO:
      btnLokasjonsTransliste:SENSITIVE = false.
  END.

  DYNAMIC-FUNCTION("InitTranslation",FRAME {&FRAME-NAME}:HANDLE).

  ASSIGN
      EDITOR-Info:SCREEN-VALUE =  CHR(10) + CHR(13) +
                                 "Varetelling benyttes til å korrigere systemets lagertellere."  + CHR(10) + CHR(13) + 
                                 "Ved oppstart av telling, leses lagertellerne inn i tellelisten. Avvik beregnes ved å "  + 
                                 "finne differansen mellom lager ved åpning av telling med talt antall. Det er avviket som senere posteres som korreksjonspost."
      EDITOR-Full:SCREEN-VALUE = "Full varetelling innebærer at hele lageret telles"  + CHR(10) + CHR(13) + 
                                 "Tellelisten som opprettes vil automatisk bli initiert med lagerantall på alle artikler som "  + 
                                 "har lager. Også artikler med negativt lager legges inn i listen."
      EDITOR-Rull:SCREEN-VALUE = "Rullerende telling innebærer at bare deler av lageret skal telles"  + CHR(10) + CHR(13) + 
                                 "Tellelisten som opprettes må bygges ut fra avdeling, hovedgruppe, varegruppe eller leverandør."  + 
                                 "Varer som ikke telles opp som ligger innenfor satte kriterier, nullstilles når lageret oppdateres med telleresultatet."
      EDITOR-Kontroll:SCREEN-VALUE = "Kontrolltelling innebærer telling av varer med negativ lagerantall."  + CHR(10) + CHR(13) + 
                                 "Tellelisten som opprettes er i utgangspunktet tom. Bruk funksjonen 'Utvalg' til å hente inn "  + 
                                 "varene som skal telles i tellelisten.  " +
                                 "Ved oppdatering er det bare varene som ligger i tellelisten som påvirkes."
      EDITOR-Lokasjon:SCREEN-VALUE = "Lokasjonslister benyttes for å kontrollere tellelister fra tellepenn."  + CHR(10) + CHR(13) + 
                                 "Tellelisten som opprettes er i utgangspunktet tom. Les inn fil fra tellepenn i listen "  + 
                                 "og kontroller listen. Etter kontrollen skal listen oppdateres mot en telleliste."
      EDITOR-Trans:SCREEN-VALUE = "Transaksjonsliste brukes i supportsammenheng."  + CHR(10) + CHR(13) + 
                                 "Benyttes av support for å 'flytte' en telling til en annen dato. Med denne funksjonen "  + 
                                 "kan en telling flyttes frem eller tilbake i tid."
      .



END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

