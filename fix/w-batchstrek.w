&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          skotex           PROGRESS
*/
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

{runlib.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define QUERY-NAME QUERY-ArtBas

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ArtBas

/* Definitions for QUERY QUERY-ArtBas                                   */
&Scoped-define OPEN-QUERY-QUERY-ArtBas OPEN QUERY QUERY-ArtBas FOR EACH ArtBas ~
      WHERE FALSE NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-QUERY-ArtBas ArtBas
&Scoped-define FIRST-TABLE-IN-QUERY-QUERY-ArtBas ArtBas


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Btn_Help RS-Type RS-Avgrens FI-Artfra ~
FI-Arttil FI-Vgfra BUTTON-Ok FI-Vgtil FI-Lopnrfra FI-Lopnrtil CB-Sesong ~
B-Generer B-SokVgFra B-SokVgTil RECT-2 RECT-28 RECT-3 
&Scoped-Define DISPLAYED-OBJECTS RS-Type RS-Avgrens FI-Artfra FI-Arttil ~
FI-Vgfra FI-Vgtil FI-Lopnrfra FI-Lopnrtil CB-Sesong 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-Generer 
     LABEL "Generer" 
     SIZE 15 BY 1.14.

DEFINE BUTTON B-SokVgFra 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON B-SokVgTil 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON Btn_Help DEFAULT 
     IMAGE-UP FILE "icon\e-help":U NO-FOCUS FLAT-BUTTON
     LABEL "&Help" 
     SIZE 4.6 BY 1.05
     BGCOLOR 8 .

DEFINE BUTTON BUTTON-Ok 
     IMAGE-UP FILE "icon\e-exit":U NO-FOCUS FLAT-BUTTON
     LABEL "&Ok" 
     SIZE 4.6 BY 1.05 TOOLTIP "Lagre og avslutt".

DEFINE VARIABLE CB-Sesong AS CHARACTER FORMAT "X(256)":U 
     LABEL "Sesong" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 32 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Artfra AS DECIMAL FORMAT ">>>>>>9":U INITIAL 0 
     LABEL "Artikkelnr fra/til" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Arttil AS DECIMAL FORMAT ">>>>>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Lopnrfra AS INTEGER FORMAT ">>>9":U INITIAL 0 
     LABEL "Løpenr fra/til" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Lopnrtil AS INTEGER FORMAT ">>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Vgfra AS INTEGER FORMAT ">>>9":U INITIAL 0 
     LABEL "Vg fra/til" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Vgtil AS INTEGER FORMAT ">>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE RS-Avgrens AS CHARACTER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Alle","0",
"Artikkelnr", "1",
"Varegruppe", "2",
"Løpenr", "3"
     SIZE 18.2 BY 5.24 NO-UNDO.

DEFINE VARIABLE RS-Type AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "EAN", 1,
"Interleave", 2
     SIZE 34 BY .95 NO-UNDO.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 55 BY 7.38.

DEFINE RECTANGLE RECT-28
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 76.2 BY .1.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 19.8 BY 7.38.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY QUERY-ArtBas FOR 
      ArtBas SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     Btn_Help AT ROW 1.24 COL 67 NO-TAB-STOP 
     RS-Type AT ROW 2.62 COL 4 NO-LABEL
     RS-Avgrens AT ROW 3.95 COL 1.8 NO-LABEL
     FI-Artfra AT ROW 5.33 COL 35.8 COLON-ALIGNED
     FI-Arttil AT ROW 5.33 COL 54.2 COLON-ALIGNED NO-LABEL
     FI-Vgfra AT ROW 6.71 COL 35.8 COLON-ALIGNED
     BUTTON-Ok AT ROW 1.24 COL 72 NO-TAB-STOP 
     FI-Vgtil AT ROW 6.71 COL 54.2 COLON-ALIGNED NO-LABEL
     FI-Lopnrfra AT ROW 8.1 COL 36 COLON-ALIGNED
     FI-Lopnrtil AT ROW 8.1 COL 54 COLON-ALIGNED NO-LABEL
     CB-Sesong AT ROW 9.48 COL 36 COLON-ALIGNED
     B-Generer AT ROW 11.52 COL 60.6
     B-SokVgFra AT ROW 6.76 COL 51.8
     B-SokVgTil AT ROW 6.76 COL 70.6
     RECT-2 AT ROW 3.86 COL 21
     RECT-28 AT ROW 2.33 COL 1
     RECT-3 AT ROW 3.86 COL 1.2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE .


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
         TITLE              = "Generer EAN/Interleave"
         HEIGHT             = 12.1
         WIDTH              = 76.2
         MAX-HEIGHT         = 12.1
         MAX-WIDTH          = 76.2
         VIRTUAL-HEIGHT     = 12.1
         VIRTUAL-WIDTH      = 76.2
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   Size-to-Fit                                                          */
ASSIGN 
       FRAME DEFAULT-FRAME:SCROLLABLE       = FALSE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK QUERY QUERY-ArtBas
/* Query rebuild information for QUERY QUERY-ArtBas
     _TblList          = "skotex.ArtBas"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Where[1]         = "FALSE"
     _Design-Parent    is FRAME DEFAULT-FRAME @ ( 9.57 , 8 )
*/  /* QUERY QUERY-ArtBas */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Generer EAN/Interleave */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Generer EAN/Interleave */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Generer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Generer C-Win
ON CHOOSE OF B-Generer IN FRAME DEFAULT-FRAME /* Generer */
DO:
  RUN KontrollerInput.
  IF RETURN-VALUE <> "AVBRYT" THEN DO:
      RUN GenererStrek IN THIS-PROCEDURE.
  END.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SokVgFra
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SokVgFra C-Win
ON CHOOSE OF B-SokVgFra IN FRAME DEFAULT-FRAME /* ... */
or F10 of FI-VgFra
DO:
  ASSIGN FI-VgFra.
  RUN d-bvargr (INPUT-OUTPUT FI-VgFra).
  ASSIGN FI-VgFra:SCREEN-VALUE = STRING(FI-VgFra).
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SokVgTil
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SokVgTil C-Win
ON CHOOSE OF B-SokVgTil IN FRAME DEFAULT-FRAME /* ... */
or F10 of FI-VgTil
DO:
  ASSIGN FI-VgTil.
  RUN d-bvargr (INPUT-OUTPUT FI-VgTil).
  ASSIGN FI-VgTil:SCREEN-VALUE = STRING(FI-VgTil).
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Help
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Help C-Win
ON CHOOSE OF Btn_Help IN FRAME DEFAULT-FRAME /* Help */
OR HELP OF FRAME {&FRAME-NAME}
DO: /* Call Help Function (or a simple message). */
   {winhlp.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Ok
&Scoped-define SELF-NAME RS-Avgrens
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RS-Avgrens C-Win
ON VALUE-CHANGED OF RS-Avgrens IN FRAME DEFAULT-FRAME
DO:
  RUN EnaDisField (SELF:SCREEN-VALUE).
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
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN InitCB.
  RUN enable_UI.
  {lng.i}
  APPLY "VALUE-CHANGED" TO RS-Avgrens.
  ASSIGN CB-Sesong:SCREEN-VALUE = ENTRY(2,CB-Sesong:LIST-ITEM-PAIRS).
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

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
  DISPLAY RS-Type RS-Avgrens FI-Artfra FI-Arttil FI-Vgfra FI-Vgtil FI-Lopnrfra 
          FI-Lopnrtil CB-Sesong 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE Btn_Help RS-Type RS-Avgrens FI-Artfra FI-Arttil FI-Vgfra BUTTON-Ok 
         FI-Vgtil FI-Lopnrfra FI-Lopnrtil CB-Sesong B-Generer B-SokVgFra 
         B-SokVgTil RECT-2 RECT-28 RECT-3 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EnaDisField C-Win 
PROCEDURE EnaDisField :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEFINE INPUT  PARAMETER cRS-Avgrens AS CHARACTER  NO-UNDO.
 DO WITH FRAME {&FRAME-NAME}:
   ASSIGN FI-Artfra:SCREEN-VALUE   = "0"
          FI-Arttil:SCREEN-VALUE   = "0"
          FI-Vgfra:SCREEN-VALUE    = "0"
          FI-Vgtil:SCREEN-VALUE    = "0"
          FI-Lopnrfra:SCREEN-VALUE = "0"
          FI-Lopnrtil:SCREEN-VALUE = "0"
          CB-Sesong:SCREEN-VALUE  = ENTRY(2,CB-Sesong:LIST-ITEM-PAIRS)
/*           RS-Avgrens:SENSITIVE  = cRS-Avgrens <> "0" */
          FI-Artfra:SENSITIVE   = cRS-Avgrens = "1"
          FI-Arttil:SENSITIVE   = cRS-Avgrens = "1"
          FI-Vgfra:SENSITIVE    = CAN-DO("2,3",cRS-Avgrens)
          FI-Vgtil:SENSITIVE    = cRS-Avgrens = "2"
          FI-Lopnrfra:SENSITIVE = cRS-Avgrens = "3"
          FI-Lopnrtil:SENSITIVE = cRS-Avgrens = "3"
          B-SokVgFra:SENSITIVE  = FI-VgFra:SENSITIVE
          B-SokVgTil:SENSITIVE  = FI-VgTil:SENSITIVE.
/*           CB-Sesong:SENSITIVE   = CAN-DO("0,2,3,4",cRS-Avgrens). */
   CASE cRS-Avgrens:
       WHEN "1" THEN
           APPLY "ENTRY" TO FI-Artfra.
       WHEN "2" OR WHEN "3" THEN
           APPLY "ENTRY" TO FI-Vgfra.
   END CASE.
 END.
          
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GenererStrek C-Win 
PROCEDURE GenererStrek :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE hQuery   AS HANDLE      NO-UNDO.
  DEFINE VARIABLE iCount   AS INTEGER     NO-UNDO.
  DEFINE VARIABLE cQString AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE hgenStrekkode AS HANDLE NO-UNDO.
  ASSIGN hQuery = QUERY QUERY-ArtBas:HANDLE.
  DO WITH FRAME {&FRAME-NAME}:
    IF RS-Avgrens:SCREEN-VALUE = "1" THEN DO:
        ASSIGN cQString = "FOR EACH ArtBas NO-LOCK"  + IF RS-Type:SCREEN-VALUE = "1" THEN ""
                              ELSE " WHERE ArtBas.Lopnr <> ?".
    END.
    ELSE IF RS-Avgrens:SCREEN-VALUE = "1" THEN DO:
        ASSIGN cQString = "FOR EACH ArtBas NO-LOCK WHERE ArtBas.ArtikkelNr >= " + FI-Artfra:SCREEN-VALUE +
            " AND ArtBas.ArtikkelNr <= " + FI-Arttil:SCREEN-VALUE + IF RS-Type:SCREEN-VALUE = "1" THEN ""
                              ELSE " AND ArtBas.Lopnr <> ?".
    END.
    ELSE IF RS-Avgrens:SCREEN-VALUE = "2" THEN DO:
        ASSIGN cQString = "FOR EACH ArtBas NO-LOCK WHERE ArtBas.Vg >= " + FI-Vgfra:SCREEN-VALUE +
            " AND ArtBas.Vg <= " + FI-Vgtil:SCREEN-VALUE + IF RS-Type:SCREEN-VALUE = "1" THEN ""
                              ELSE " AND ArtBas.Lopnr <> ?".
    END.
    ELSE IF RS-Avgrens:SCREEN-VALUE = "3" THEN DO:
        ASSIGN cQString = "FOR EACH ArtBas NO-LOCK WHERE ArtBas.Vg = " + FI-Vgfra:SCREEN-VALUE +
            IF FI-Lopnrtil <> 0  THEN
            " AND ArtBas.Lopnr >= " + FI-Lopnrfra:SCREEN-VALUE + 
            " AND ArtBas.Lopnr <= " + FI-Lopnrtil:SCREEN-VALUE ELSE IF RS-Type:SCREEN-VALUE = "1" THEN ""
                ELSE " AND ArtBas.Lopnr <> ?".
    END.
  END.
  ASSIGN cQString = cQString + IF CB-Sesong:SCREEN-VALUE = "" THEN
            "." ELSE (IF RS-Avgrens:SCREEN-VALUE = "0" THEN " WHERE" ELSE " AND") + " ArtBas.Sasong = " + CB-Sesong:SCREEN-VALUE + ".".
/*   MESSAGE cQString                       */
/*       VIEW-AS ALERT-BOX INFO BUTTONS OK. */
  hQuery:QUERY-PREPARE(cQString).
  hQuery:QUERY-OPEN().
  RUN genStrekkode.p PERSISTENT SET hgenStrekkode (?,INT(RS-Type:SCREEN-VALUE)).
  REPEAT:
      IF hQuery:GET-NEXT() THEN
          ASSIGN iCount = iCount + 1.
      IF NOT AVAIL ArtBas THEN
          LEAVE.
      IF RS-Type:SCREEN-VALUE = "1" THEN
          RUN GenEAN IN hgenStrekkode (ArtBas.Artikkelnr).
      ELSE 
          RUN GenInterleave IN hgenStrekkode (ArtBas.Artikkelnr).
  END.
  DELETE OBJECT hgenStrekkode.
  MESSAGE iCount
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitCB C-Win 
PROCEDURE InitCB :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cListItemPairs AS CHARACTER INIT "[Alle]," NO-UNDO.
    FOR EACH Sasong:
        ASSIGN cListItemPairs = cListItemPairs + (IF cListItemPairs = "" THEN "" ELSE ",") + 
                TRIM(Sasong.SasBeskr) + "," + STRING(SaSong.Sasong).
    END.
    ASSIGN CB-Sesong:LIST-ITEM-PAIRS IN FRAME {&FRAME-NAME} = cListItemPairs.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE KontrollerInput C-Win 
PROCEDURE KontrollerInput :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cMsgString AS CHARACTER  NO-UNDO.
  DO WITH FRAME {&FRAME-NAME}:
      CASE RS-Avgrens:SCREEN-VALUE:
          WHEN "1" THEN DO:
              ASSIGN FI-Artfra
                     FI-Arttil.
              IF FI-Artfra = 0 THEN DO:
                  ASSIGN cMsgString = "Angi fra nr.".
                  APPLY "ENTRY" TO FI-Artfra.
              END.
              ELSE IF FI-Arttil <> 0 AND FI-Artfra > FI-Arttil THEN DO:
                  ASSIGN cMsgString = "Fra > til.".
                  APPLY "ENTRY" TO FI-Arttil.
              END.
              ELSE IF FI-Arttil = 0 THEN DO:
                  FIND LAST ArtBas USE-INDEX ArtikkelNr NO-LOCK NO-ERROR.
                  IF AVAIL ArtBas THEN
                      ASSIGN FI-Arttil              = ArtBas.ArtikkelNr
                             FI-Arttil:SCREEN-VALUE = STRING(ArtBas.ArtikkelNr).
              END.
          END.
          WHEN "2" THEN DO:
              ASSIGN FI-Vgfra
                     FI-Vgtil.
              IF FI-Vgfra = 0 THEN DO:
                  ASSIGN cMsgString = "Angi fra nr.".
                  APPLY "ENTRY" TO FI-Vgfra.
              END.
              ELSE IF NOT CAN-FIND(VarGr WHERE varGr.Vg = FI-VgFra) THEN DO:
                  ASSIGN cMsgString = "Finnes ikke.".
                  APPLY "ENTRY" TO FI-Vgfra.
              END.
              ELSE IF FI-Vgtil <> 0 AND FI-Vgfra > FI-Vgtil THEN DO:
                  ASSIGN cMsgString = "Fra > til.".
                  APPLY "ENTRY" TO FI-Vgtil.
              END.
              ELSE IF FI-Vgtil = 0 THEN DO:
                  FIND LAST VarGr USE-INDEX vargrin NO-LOCK NO-ERROR.
                  IF AVAIL VarGr THEN
                      ASSIGN FI-Vgtil              = VarGr.Vg
                             FI-Vgtil:SCREEN-VALUE = STRING(VarGr.Vg).
              END.
              ELSE IF NOT CAN-FIND(VarGr WHERE varGr.Vg = FI-VgTil) THEN DO:
                  ASSIGN cMsgString = "Finnes ikke.".
                  APPLY "ENTRY" TO FI-Vgtil.
              END.
          END.
          WHEN "3" THEN DO:
              ASSIGN FI-Vgfra
                     FI-Lopnrfra
                     FI-Lopnrtil.
              IF FI-Vgfra = 0 THEN DO:
                  ASSIGN cMsgString = "Angi fra nr.".
                  APPLY "ENTRY" TO FI-Vgfra.
              END.
              ELSE IF NOT CAN-FIND(VarGr WHERE varGr.Vg = FI-VgFra) THEN DO:
                  ASSIGN cMsgString = "Finnes ikke.".
                  APPLY "ENTRY" TO FI-Vgfra.
              END.
              ELSE IF FI-Lopnrfra > FI-Lopnrtil THEN DO:
                  ASSIGN cMsgString = "Fra > til.".
                  APPLY "ENTRY" TO FI-Lopnrtil.
              END.
          END.
      END CASE.
  END.
  IF cMsgString <> "" THEN DO:
      MESSAGE cMsgString
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN "AVBRYT".
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

