&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          skotex           PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------
  File:              d-vtekst.w  
  Description:       Detaljer for tekster
  Input Parameters:  INPUT-OUTPUT ROWID wioRowid
                     INPUT        CHAR  wiPrgNavn
  Output Parameters: <none>
  Author:            SJ
  Created:           27.02.00
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEF INPUT-OUTPUT PARAMETER wioRowid  AS ROWID NO-UNDO. /* Tekst */
DEF INPUT        PARAMETER wiPrgNavn AS CHAR  NO-UNDO.

/* Local Variable Definitions ---                                       */
DEF VAR wRetVal AS CHAR NO-UNDO INIT "<Avbryt>".
{runlib.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Tekst

/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define FIELDS-IN-QUERY-Dialog-Frame Tekst.PrgNavn Tekst.TxtNr ~
Tekst.Lng 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Dialog-Frame Tekst.TxtNr Tekst.Lng 
&Scoped-define ENABLED-TABLES-IN-QUERY-Dialog-Frame Tekst
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-Dialog-Frame Tekst
&Scoped-define QUERY-STRING-Dialog-Frame FOR EACH Tekst SHARE-LOCK
&Scoped-define OPEN-QUERY-Dialog-Frame OPEN QUERY Dialog-Frame FOR EACH Tekst SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-Dialog-Frame Tekst
&Scoped-define FIRST-TABLE-IN-QUERY-Dialog-Frame Tekst


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS Tekst.TxtNr Tekst.Lng 
&Scoped-define ENABLED-TABLES Tekst
&Scoped-define FIRST-ENABLED-TABLE Tekst
&Scoped-Define ENABLED-OBJECTS BUTTON-SokHg RECT-4 RECT-5 EDITOR-Tekst ~
Btn_OK Btn_Cancel Btn_Help 
&Scoped-Define DISPLAYED-FIELDS Tekst.PrgNavn Tekst.TxtNr Tekst.Lng 
&Scoped-define DISPLAYED-TABLES Tekst
&Scoped-define FIRST-DISPLAYED-TABLE Tekst


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Avbryt" 
     SIZE 15 BY 1.1
     BGCOLOR 8 .

DEFINE BUTTON Btn_Help 
     LABEL "&Hjelp" 
     SIZE 15 BY 1.1
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.1
     BGCOLOR 8 .

DEFINE BUTTON BUTTON-SokHg 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE VARIABLE EDITOR-Tekst AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-VERTICAL
     SIZE 60 BY 8.33 NO-UNDO.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 62 BY 10.48.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 62 BY 3.1.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Dialog-Frame FOR 
      Tekst SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     BUTTON-SokHg AT ROW 5.52 COL 24
     Tekst.PrgNavn AT ROW 1.71 COL 16 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 31 BY 1
     Tekst.TxtNr AT ROW 2.91 COL 16 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.2 BY 1
     Tekst.Lng AT ROW 5.52 COL 13 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 8.6 BY 1
     EDITOR-Tekst AT ROW 6.95 COL 3 NO-LABEL
     Btn_OK AT ROW 15.76 COL 2
     Btn_Cancel AT ROW 15.76 COL 19
     Btn_Help AT ROW 15.76 COL 49
     "Tekst" VIEW-AS TEXT
          SIZE 6 BY .62 AT ROW 4.57 COL 3
     RECT-4 AT ROW 5.05 COL 2
     RECT-5 AT ROW 1.24 COL 2
     SPACE(0.59) SKIP(12.52)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Tekst"
         DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_Cancel.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
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

/* SETTINGS FOR EDITOR EDITOR-Tekst IN FRAME Dialog-Frame
   NO-DISPLAY                                                           */
ASSIGN 
       EDITOR-Tekst:RETURN-INSERTED IN FRAME Dialog-Frame  = TRUE.

/* SETTINGS FOR FILL-IN Tekst.PrgNavn IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX Dialog-Frame
/* Query rebuild information for DIALOG-BOX Dialog-Frame
     _TblList          = "SkoTex.Tekst"
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX Dialog-Frame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON GO OF FRAME Dialog-Frame /* Tekst */
DO:
   IF wioRowid <> ? THEN DO:
      FIND Tekst WHERE ROWID(Tekst) = wioRowid EXCLUSIVE NO-WAIT NO-ERROR.
      IF NOT AVAIL Tekst THEN DO:
         IF LOCKED Tekst THEN DO:
            MESSAGE "Dataene er sperret av en annen bruker." SKIP
                    "Kan ikke lagre..."
               VIEW-AS ALERT-BOX ERROR TITLE "Feil".
            NEXT-PROMPT Tekst.Tekst.
            RETURN NO-APPLY.
         END.
         ELSE DO:
            MESSAGE "Finner ikke dataene. De kan ha blitt slettet av en annen bruker." SKIP
                    "Kan ikke lagre..."
               VIEW-AS ALERT-BOX ERROR TITLE "Feil".
            NEXT-PROMPT Tekst.Tekst.
            RETURN NO-APPLY.
         END.
      END.
   END.
   ELSE 
   DO WITH FRAME {&FRAME-NAME}:
      IF CAN-FIND(Tekst WHERE Tekst.PrgNavn = Tekst.PrgNavn:SCREEN-VALUE AND
                                   Tekst.TxtNr   = INT(Tekst.TxtNr:SCREEN-VALUE) AND
                                   Tekst.Lng     = Tekst.Lng:SCREEN-VALUE)
      THEN DO:
         MESSAGE "Det finnes allerede en tekst med angitt programnavn, tekstnr og språkkode." SKIP(1)
                 "Kan ikke lagre den nye teksten."
            VIEW-AS ALERT-BOX ERROR TITLE "Feil".
         NEXT-PROMPT Tekst.PrgNavn.
         RETURN NO-APPLY.
      END.                             
                                                          
      CREATE Tekst.
      ASSIGN wioRowid = ROWID(Tekst).

   END.   
   ASSIGN Tekst EXCEPT Tekst.Tekst.
   ASSIGN Tekst.Tekst = EDITOR-Tekst:SCREEN-VALUE
          wRetVal = "<Ok>".
 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Tekst */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Help
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Help Dialog-Frame
ON CHOOSE OF Btn_Help IN FRAME Dialog-Frame /* Hjelp */
OR HELP OF FRAME {&FRAME-NAME}
DO: 
  {diahelp.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokHg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokHg Dialog-Frame
ON CHOOSE OF BUTTON-SokHg IN FRAME Dialog-Frame /* ... */
or F10 of Tekst.Lng
DO:
  /* Start søkeprogram */
  /*
    {soek.i
    &Felt        = Tekst.Lng
    &Program     = d-bsprak.w
    &Frame       = Dialog-Frame
    &ParamType   = "Input"
    &ExtraParam  = "' '"
    &PostRun     = "find Sprak no-lock where
                    recid(Sprak) = int(return-value) no-error."
    &OptDisp     = "Sprak.Lng when available Sprak @ Tekst.Lng "
  }   
  */
  DEF VAR cLookupValue  AS CHAR NO-UNDO.

  cLookupValue = "Lng".

  RUN JBoxDLookup.w ("Sprak;Lng;Beskrivelse", "where true", INPUT-OUTPUT cLookupValue).

  IF cLookupValue NE "" THEN DO:
    Tekst.Lng:SCREEN-VALUE = cLookupValue.
    APPLY "return" TO Tekst.Lng.
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

ASSIGN FRAME {&FRAME-NAME}:TITLE = STRING(wioRowid = ?,"Ny tekst/Detaljer for tekst").

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  IF wioRowid <> ? THEN DO:
     FIND Tekst WHERE ROWID(Tekst) = wioRowid NO-LOCK NO-ERROR.
     IF NOT AVAIL Tekst THEN DO:
        MESSAGE Tx("Finner ikke opplysningene." + CHR(13) +
                   "De kan ha blitt slettet av en annen bruker.",100)
           VIEW-AS ALERT-BOX ERROR TITLE Tx("Feil",9999).
        LEAVE MAIN-BLOCK.           
     END.
     ASSIGN EDITOR-Tekst:SCREEN-VALUE = Tekst.Tekst.
  END.
  {lng.i}
  RUN enable_UI.
  IF wioRowid = ? THEN DO:
     ASSIGN Tekst.PrgNavn:SCREEN-VALUE = wiPrgNavn.
     APPLY "ENTRY" TO Tekst.TxtNr.
  END.
  ELSE ASSIGN Tekst.PrgNavn:SENSITIVE = NO
              Tekst.TxtNr:SENSITIVE   = NO
              Tekst.Lng:SENSITIVE     = NO.

  DISPLAY
      wCurrLng @ Tekst.Lng
  WITH FRAME Dialog-Frame.

  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.
RETURN wRetVal.

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
  IF AVAILABLE Tekst THEN 
    DISPLAY Tekst.PrgNavn Tekst.TxtNr Tekst.Lng 
      WITH FRAME Dialog-Frame.
  ENABLE BUTTON-SokHg RECT-4 RECT-5 Tekst.TxtNr Tekst.Lng EDITOR-Tekst Btn_OK 
         Btn_Cancel Btn_Help 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

