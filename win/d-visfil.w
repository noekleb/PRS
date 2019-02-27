&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------
  Forfatter:   SJ
  Beskrivelse: Forhåndsvisning av utskrift (dialogutgave) 
  Parametere:
  Endringer:
------------------------------------------------------------------------*/

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
/* Parameter Definisjoner ---                                           */
&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN   
   DEF VAR wJobbNr   AS INTE NO-UNDO.
   DEF VAR wRappNavn AS CHAR NO-UNDO INIT "TestFil".
   DEF VAR wFilNavn  AS CHAR NO-UNDO INIT "c:\autoexec.bat". 
   DEF VAR wUtEnhet  AS CHAR NO-UNDO INIT "S".
   DEF VAR wSkrNr    AS INTE NO-UNDO.
   DEF VAR wHelpId   AS CHAR NO-UNDO.
&ELSE
   DEF INPUT PARAMETER wJobbNr   AS INTE NO-UNDO.
   DEF INPUT PARAMETER wRappNavn AS CHAR NO-UNDO.   
   DEF INPUT PARAMETER wFilnavn  AS CHAR NO-UNDO.
   DEF INPUT PARAMETER wUtEnhet  AS CHAR NO-UNDO.
   DEF INPUT PARAMETER wSkrNr    AS INTE NO-UNDO.
   DEF INPUT PARAMETER wHelpId   AS CHAR NO-UNDO.
&ENDIF

/* Preprossessor direktiver ---                                         */
&SCOP TomMelding Jobben ble kjørt, men ingen utskrift ble laget.
&SCOP IgangMelding Jobben kjører, prøv senere.
&SCOP IkkeStartetMelding Jobben er ikke startet.
/* Buffer og Temp-Table Definisjoner ---                                */

/* Lokale variabler ---                                                 */
def var retur-verdi as char initial "AVBRYT" no-undo.

define var m-ja              as logical no-undo.
define var m-i               as integer no-undo.
define var m-x               as character no-undo.
define var m-handle          as handle no-undo.
define var m-wh              as widget-handle no-undo.
DEF VAR h-FirstProc AS HANDLE NO-UNDO.

DEF VAR wSokeTekst AS CHAR NO-UNDO.
DEF VAR wSokeFlagg AS INTE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BUTTON-SkrivUt BUTTON-LagreSom BUTTON-Kopier ~
BUTTON-Sok BUTTON-FontZoom Btn_Help-2 EDITOR-fv 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Help-2 DEFAULT 
     IMAGE-UP FILE "./icon/e-help":U NO-FOCUS
     LABEL "&Hjelp" 
     SIZE 5 BY 1.14 TOOLTIP "Hjelp"
     BGCOLOR 8 .

DEFINE BUTTON BUTTON-FontZoom 
     IMAGE-UP FILE "./icon/e-vis":U NO-FOCUS
     LABEL "Større / mindre skrift" 
     SIZE 5 BY 1.14 TOOLTIP "Større eller mindre skrift".

DEFINE BUTTON BUTTON-Kopier 
     IMAGE-UP FILE "./icon/e-copy":U NO-FOCUS
     LABEL "Kopier" 
     SIZE 5 BY 1.14 TOOLTIP "Kopier".

DEFINE BUTTON BUTTON-LagreSom 
     IMAGE-UP FILE "./icon/e-save":U NO-FOCUS
     LABEL "Button 1" 
     SIZE 5 BY 1.14 TOOLTIP "Lagre som...".

DEFINE BUTTON BUTTON-SkrivUt 
     IMAGE-UP FILE "./icon/e-print":U NO-FOCUS
     LABEL "Skriv ut" 
     SIZE 5 BY 1.14 TOOLTIP "Skriv ut".

DEFINE BUTTON BUTTON-Sok 
     IMAGE-UP FILE "./icon/e-search":U NO-FOCUS
     LABEL "Søk" 
     SIZE 5 BY 1.14 TOOLTIP "Søk etter".

DEFINE VARIABLE EDITOR-fv AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL LARGE
     SIZE 131 BY 18.57
     FONT 0 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     BUTTON-SkrivUt AT ROW 1.24 COL 2
     BUTTON-LagreSom AT ROW 1.24 COL 8
     BUTTON-Kopier AT ROW 1.24 COL 17
     BUTTON-Sok AT ROW 1.24 COL 22
     BUTTON-FontZoom AT ROW 1.24 COL 27
     Btn_Help-2 AT ROW 1.24 COL 33
     EDITOR-fv AT ROW 2.43 COL 2 NO-LABEL
     SPACE(0.19) SKIP(0.04)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Forhåndsvisning".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS


/* ***************  Runtime Attributes and UIB Settings  ************** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
                                                                        */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR EDITOR EDITOR-fv IN FRAME Dialog-Frame
   NO-DISPLAY                                                           */
ASSIGN 
       EDITOR-fv:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 




/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Forhåndsvisning */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Help-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Help-2 Dialog-Frame
ON CHOOSE OF Btn_Help-2 IN FRAME Dialog-Frame /* Hjelp */
OR HELP OF FRAME {&FRAME-NAME}
DO: 
   IF wHelpId = "" THEN ASSIGN wHelpId = ENTRY(1,THIS-PROCEDURE:FILE-NAME,".").
   RUN Hjelp IN (SESSION:FIRST-PROCEDURE) (STRING(KEYLABEL(LASTKEY) = "CTRL-F1","M/H"),wHelpId).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-FontZoom
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-FontZoom Dialog-Frame
ON CHOOSE OF BUTTON-FontZoom IN FRAME Dialog-Frame /* Større / mindre skrift */
DO:
    DO WITH FRAME {&FRAME-NAME}:
       
       CASE EDITOR-fv:FONT:
          WHEN 0 THEN ASSIGN EDITOR-fv:FONT = 2.
          WHEN 2 THEN ASSIGN EDITOR-fv:FONT = 3.
          WHEN 3 THEN ASSIGN EDITOR-fv:FONT = 0.
       END CASE.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Kopier
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Kopier Dialog-Frame
ON CHOOSE OF BUTTON-Kopier IN FRAME Dialog-Frame /* Kopier */
DO:
   DO WITH FRAME {&FRAME-NAME}:
      IF NOT EDITOR-fv:TEXT-SELECTED THEN DO:
         BELL.
         RETURN NO-APPLY.
      END.
      RUN Kopier.      
   END.      

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-LagreSom
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-LagreSom Dialog-Frame
ON CHOOSE OF BUTTON-LagreSom IN FRAME Dialog-Frame /* Button 1 */
DO:
  DEF VAR wFile AS CHAR NO-UNDO.
  DEF VAR wDir  AS CHAR NO-UNDO.
  DEF VAR wOk   AS LOGI NO-UNDO.
  IF R-INDEX(wFilNavn,"\") > 0 THEN
     ASSIGN wFile = SUBSTR(wFilNavn,R-INDEX(wFilNavn,"\") + 1)
            wDir  = SUBSTR(wFilNavn,1,R-INDEX(wFilNavn,"\") - 1).
  ELSE 
     ASSIGN wFile = IF wFilNavn = "<tom>" THEN "Tom-jobb.txt" ELSE wFilNavn
            wDir  = OS-GETENV("Temp") NO-ERROR.          
  SYSTEM-DIALOG GET-FILE wFile USE-FILENAME SAVE-AS ASK-OVERWRITE INITIAL-DIR wDir Update wOk.
  if wOk THEN
  DO WITH FRAME {&FRAME-NAME}:
    
    IF Editor-fv:SAVE-FILE(wFile) THEN
       ASSIGN 
          wFilNavn = wFile
          FRAME {&FRAME-NAME}:TITLE = wRappNavn + " [" + wFilnavn + "]".
    ELSE
       MESSAGE "Kunne ikke lagre filen" wfile
          VIEW-AS ALERT-BOX ERROR TITLE "Feil".
  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SkrivUt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SkrivUt Dialog-Frame
ON CHOOSE OF BUTTON-SkrivUt IN FRAME Dialog-Frame /* Skriv ut */
DO:
   IF wSkrNr <> ? AND wUtEnhet <> "F" AND wFilNavn <> "<tom>" THEN
        RUN SkrivUtFil("SkoTex").
   ELSE RUN SkrivUtFil(STRING(wSkrNr = ?,"DIREKTE/DIALOG")).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Sok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Sok Dialog-Frame
ON CHOOSE OF BUTTON-Sok IN FRAME Dialog-Frame /* Søk */
DO:
  RUN SokEtter.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO:
  RUN LesInnFil.
  IF RETURN-VALUE = "Feil" THEN LEAVE MAIN-BLOCK.
  {lng.i} RUN enable_UI.

  WAIT-FOR GO, ENDKEY, END-ERROR OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

&IF DEFINED(UIB_IS_RUNNING) EQ 0 &THEN
 return retur-verdi.
&else
 message retur-verdi view-as alert-box.
&endif

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dialog-Frame _DEFAULT-DISABLE
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dialog-Frame _DEFAULT-ENABLE
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
  ENABLE BUTTON-SkrivUt BUTTON-LagreSom BUTTON-Kopier BUTTON-Sok 
         BUTTON-FontZoom Btn_Help-2 EDITOR-fv 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Kopier Dialog-Frame 
PROCEDURE Kopier :
/*------------------------------------------------------------------------------
  Purpose:     Kopierer valgt tekst til clopboard.
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:
     IF EDITOR-fv:TEXT-SELECTED THEN DO:
        OUTPUT TO "CLIPBOARD".
        PUT UNFORMATTED EDITOR-fv:SELECTION-TEXT.
        OUTPUT CLOSE.
     END.
   END.     

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LesInnFil Dialog-Frame 
PROCEDURE LesInnFil :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR wForStor AS LOGI NO-UNDO.
  DEF VAR i        AS INTE NO-UNDO.
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN 
          FRAME {&FRAME-NAME}:TITLE = wRappNavn + IF NOT wFilnavn BEGINS "<" THEN        
                                                (" [" + wFilnavn + "]") ELSE "".
                                                
    IF NOT wFilnavn BEGINS "<" THEN DO:
       EDITOR-fv:READ-FILE(wFilNavn) NO-ERROR.
       IF ERROR-STATUS:NUM-MESSAGES > 0 THEN DO:
          DO i = 1 TO ERROR-STATUS:NUM-MESSAGES:
             IF ERROR-STATUS:GET-NUMBER(i) = 225 THEN DO:
                ASSIGN wForStor = YES.
                LEAVE.
             END.
          END.      
          IF wForStor THEN RUN viewstorfil.p(wFilNavn).
          ELSE MESSAGE "Kan ikke lese utskriftsfil " wFilNavn + " Feil nr.(" + 
                       string(ERROR-STATUS:NUM-MESSAGES) + ")." 
                  VIEW-AS ALERT-BOX ERROR TITLE "Feil".
          PROCESS EVENTS.
          RETURN "Feil".
          
       END.
    END.  
    ELSE IF wFilnavn = "<tom>" THEN
      ASSIGN EDITOR-fv:SCREEN-VALUE = "{&TomMelding}" + " (Jobbnr. " + STRING(wJobbNr) + ")".
    ELSE IF wFilnavn = "<igang>" THEN
      ASSIGN EDITOR-fv:SCREEN-VALUE = "{&IgangMelding}" + " (Jobbnr. " + STRING(wJobbNr) + ")".
    ELSE IF wFilnavn = "<ikkestartet>" THEN
      ASSIGN EDITOR-fv:SCREEN-VALUE = "{&IkkeStartetMelding}" + " (Jobbnr. " + STRING(wJobbNr) + ")".
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkrivUtFil Dialog-Frame 
PROCEDURE SkrivUtFil :
/*------------------------------------------------------------------------------
  Purpose:     Utskrift av fil
  Parameters:  Input direkte/dialog
  Notes:       
------------------------------------------------------------------------------*/
   DEF INPUT PARAMETER wValg    AS CHAR NO-UNDO.
   DEF VAR             wMelding AS CHAR NO-UNDO.
   IF wFilnavn BEGINS "<" THEN DO:
     ASSIGN wMelding = IF wFilnavn = "<tom>" THEN "{&TomMelding}" ELSE
                       IF wFilnavn = "<igang>" THEN "{&IgangMelding}" ELSE
                       IF  wFilnavn = "<ikkestartet>" THEN "{&IkkeStartetMelding}"
                       ELSE "".
     MESSAGE wMelding 
             VIEW-AS ALERT-BOX ERROR TITLE "Skriv ut".
     RETURN.
   END.
   
   RUN d-skrivut.w(wValg,wUtenhet,wSkrNr,wFilnavn,FRAME {&FRAME-NAME}:TITLE,"{&TomMelding}" + " (Jobbnr. " + STRING(wJobbNr) + ")").
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SokEtter Dialog-Frame 
PROCEDURE SokEtter :
/*------------------------------------------------------------------------------
  Purpose:     Søk etter
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:
     IF EDITOR-fv:TEXT-SELECTED THEN DO:
        IF LENGTH(EDITOR-fv:SELECTION-TEXT) <= 100 THEN
             ASSIGN wSokeTekst = EDITOR-fv:SELECTION-TEXT.
        ELSE ASSIGN wSokeTekst = "".
     END.      
     
     RUN d-soek.w(EDITOR-fv:HANDLE,INPUT-OUTPUT wSokeTekst,OUTPUT wSokeFlagg).
  END.   

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


