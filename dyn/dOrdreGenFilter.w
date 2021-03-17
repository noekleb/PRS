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
  DEF VAR ocLevnr          AS CHAR NO-UNDO.
  DEF VAR ocArtNr          AS CHAR NO-UNDO.
  DEF VAR obRegenIkkeSendt AS LOG  NO-UNDO.
  DEF VAR obSendtStatus    AS LOG  NO-UNDO.
  DEF VAR obOk             AS LOG  NO-UNDO.
&ELSE 
  DEF OUTPUT PARAM ocLevnr          AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocArtNr          AS CHAR NO-UNDO.
  DEF OUTPUT PARAM obRegenIkkeSendt AS LOG  NO-UNDO.
  DEF OUTPUT PARAM obSendtStatus    AS LOG  NO-UNDO.
  DEF OUTPUT PARAM obOk             AS LOG  NO-UNDO.
&ENDIF

/* Local Variable Definitions ---                                       */

DEF VAR cVarebehNr    AS CHAR NO-UNDO.
DEF VAR bOK           AS LOG  NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS LevNr btnSokKunde fiLevNavn tbRegenIkkeSendt ~
tbSendt Btn_OK Btn_Cancel 
&Scoped-Define DISPLAYED-OBJECTS LevNr fiLevNavn tbRegenIkkeSendt tbSendt 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnSokKunde 
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON btnSokKunde-2 
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Avbryt" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE ArtikkelNr AS DECIMAL FORMAT ">>>>>>>>>>>>9" INITIAL 0 
     LABEL "Oppgi evt art.nr (0 for alle)" 
     VIEW-AS FILL-IN 
     SIZE 14.6 BY 1.

DEFINE VARIABLE fiBeskr AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1 NO-UNDO.

DEFINE VARIABLE fiLevNavn AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1 NO-UNDO.

DEFINE VARIABLE LevNr AS DECIMAL FORMAT ">>>>>>>>>>>>9" INITIAL 0 
     LABEL "Oppgi evt levnr (0 for alle)" 
     VIEW-AS FILL-IN 
     SIZE 14.6 BY 1.

DEFINE VARIABLE tbRegenIkkeSendt AS LOGICAL INITIAL no 
     LABEL "Re-generer ikke sendte bestillinger" 
     VIEW-AS TOGGLE-BOX
     SIZE 40 BY .81 TOOLTIP "Hvis valgt så vil generering fortsette selv om en eller flere artikler er sendt" NO-UNDO.

DEFINE VARIABLE tbSendt AS LOGICAL INITIAL no 
     LABEL "Skal ordre også settes til sendt?" 
     VIEW-AS TOGGLE-BOX
     SIZE 40 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     LevNr AT ROW 1.62 COL 25 COLON-ALIGNED HELP
          "Kundenummer"
     btnSokKunde AT ROW 1.62 COL 41.8
     fiLevNavn AT ROW 1.62 COL 44.6 COLON-ALIGNED NO-LABEL
     ArtikkelNr AT ROW 2.86 COL 25 COLON-ALIGNED HELP
          "Kundenummer"
     btnSokKunde-2 AT ROW 2.86 COL 41.8
     fiBeskr AT ROW 2.86 COL 44.4 COLON-ALIGNED NO-LABEL
     tbRegenIkkeSendt AT ROW 4.1 COL 27.2
     tbSendt AT ROW 5.1 COL 27.2
     Btn_OK AT ROW 6 COL 46.8
     Btn_Cancel AT ROW 6 COL 62.6
     SPACE(1.19) SKIP(0.28)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Tilleggsfilter for generering av ordre"
         DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_Cancel.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Dialog-Box
   Allow: Basic,Browse,DB-Fields,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB Dialog-Frame 
/* ************************* Included-Libraries *********************** */

{incl/devmode.i}
{incl/custdevmode.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   FRAME-NAME                                                           */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE.

/* SETTINGS FOR FILL-IN ArtikkelNr IN FRAME Dialog-Frame
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       ArtikkelNr:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR BUTTON btnSokKunde-2 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       btnSokKunde-2:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR FILL-IN fiBeskr IN FRAME Dialog-Frame
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       fiBeskr:HIDDEN IN FRAME Dialog-Frame           = TRUE
       fiBeskr:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

ASSIGN 
       fiLevNavn:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Tilleggsfilter for generering av ordre */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ArtikkelNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ArtikkelNr Dialog-Frame
ON RETURN OF ArtikkelNr IN FRAME Dialog-Frame /* Oppgi evt art.nr (0 for alle) */
OR "TAB" OF ArtikkelNr DO:
  IF SELF:MODIFIED AND SELF:SCREEN-VALUE NE "0" THEN DO:
     fiBeskr:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldValues",
                                               "ArtBas",
                                               "WHERE ArtikkelNr = " + SELF:SCREEN-VALUE,
                                               "Beskr").
    APPLY "entry" TO SELF.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSokKunde
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSokKunde Dialog-Frame
ON CHOOSE OF btnSokKunde IN FRAME Dialog-Frame /* ... */
DO:
  DEF VAR cLevBasRowIdList AS CHAR NO-UNDO.
  DEF VAR cLevBasIdList    AS CHAR NO-UNDO.

  RUN JBoxDSelector.w (THIS-PROCEDURE,0,
                      "LevBas;levnr;levnamn"
                    + ",VarebehLinje;",
                      "WHERE true" 
                    + ",FIRST VarebehLinje OF LevBas NO-LOCK WHERE VarebehNr = " + cVarebehNr
                      ,INPUT-OUTPUT cLevBasRowIdList,
                      "Levnr",
                      INPUT-OUTPUT cLevBasIdList,
                      "","",
                      OUTPUT bOK).

  IF bOk THEN DO:
    IF NUM-ENTRIES(cLevBasRowidList) > 1 THEN 
      ASSIGN LevNr:SCREEN-VALUE   = "0"
             fiLevNavn:SCREEN-VALUE = REPLACE(cLevBasIdList,"|",",").
    ELSE 
      ASSIGN LevNr:SCREEN-VALUE   = cLevBasIdList
             fiLevNavn:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldList","LevBas;LevNamn","WHERE LevNr = " + LevNr:SCREEN-VALUE)
             .
  END.



/*   DEF VAR cLookupValue AS CHAR NO-UNDO.                                                */
/*                                                                                        */
/*   cLookupValue = "LevNr".                                                              */
/*                                                                                        */
/*   RUN JBoxDLookup.w ("LevBas;LevNr;LevNamn", "where true", INPUT-OUTPUT cLookupValue). */
/*                                                                                        */
/*   IF cLookupValue NE "" THEN DO:                                                       */
/*     LevNr:SCREEN-VALUE = cLookupValue.                                                 */
/*     APPLY "tab" TO LevNr.                                                              */
/*   END.                                                                                 */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSokKunde-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSokKunde-2 Dialog-Frame
ON CHOOSE OF btnSokKunde-2 IN FRAME Dialog-Frame /* ... */
DO:
  DEF VAR cLookupValue AS CHAR NO-UNDO.

  cLookupValue = "ArtikkelNr".

  RUN JBoxDLookup.w ("ArtBas;ArtikkelNr;Beskr", "where true", INPUT-OUTPUT cLookupValue).

  IF cLookupValue NE "" THEN DO:
    ArtikkelNr:SCREEN-VALUE = cLookupValue.
    APPLY "tab" TO ArtikkelNr.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:
  ASSIGN ocLevnr          = IF INT(LevNr:SCREEN-VALUE) NE 0 THEN LevNr:SCREEN-VALUE ELSE 
                            IF NUM-ENTRIES(fiLevNavn:SCREEN-VALUE) > 1 THEN
                              REPLACE(fiLevNavn:SCREEN-VALUE,",","|")
                            ELSE ""
         ocArtNr          = ArtikkelNr:SCREEN-VALUE
         obRegenIkkeSendt = tbRegenIkkeSendt:CHECKED
         obSendtStatus    = tbSendt:CHECKED
         obOk             = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME LevNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL LevNr Dialog-Frame
ON RETURN OF LevNr IN FRAME Dialog-Frame /* Oppgi evt levnr (0 for alle) */
OR "TAB" OF LevNr DO:
  IF SELF:MODIFIED AND SELF:SCREEN-VALUE NE "0" THEN DO:
     fiLevNavn:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldValues",
                                               "LevBas",
                                               "WHERE Levnr = " + SELF:SCREEN-VALUE,
                                               "LevNamn").
    APPLY "entry" TO SELF.
    RETURN NO-APPLY.
  END.
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
  cVarebehNr = STRING(DYNAMIC-FUNCTION("getVarebehNr" IN SOURCE-PROCEDURE)).
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
  DISPLAY LevNr fiLevNavn tbRegenIkkeSendt tbSendt 
      WITH FRAME Dialog-Frame.
  ENABLE LevNr btnSokKunde fiLevNavn tbRegenIkkeSendt tbSendt Btn_OK Btn_Cancel 
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
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

