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

DEFINE TEMP-TABLE TT_Rad
    FIELD cEAN AS CHARACTER FORMAT "x(20)"
    FIELD c2   AS CHARACTER FORMAT "x(20)"
    FIELD c3   AS CHARACTER FORMAT "x(20)"
    FIELD c4   AS CHARACTER FORMAT "x(20)"
    FIELD c5   AS CHARACTER FORMAT "x(20)"
    FIELD c6   AS CHARACTER FORMAT "x(20)"
    FIELD c7   AS CHARACTER FORMAT "x(20)"
    FIELD c8   AS CHARACTER FORMAT "x(20)"
    FIELD c9   AS CHARACTER FORMAT "x(20)"
    FIELD c10  AS CHARACTER FORMAT "x(20)"
    FIELD c11  AS CHARACTER FORMAT "x(20)"
    FIELD c12  AS CHARACTER FORMAT "x(20)"
    FIELD c13  AS CHARACTER FORMAT "x(20)"
    FIELD cKvittoTxt AS CHARACTER
    FIELD cMDText    AS CHARACTER.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS B-8119 FI-FIl1 FI-FIl2 BUTTON-4 
&Scoped-Define DISPLAYED-OBJECTS FI-FIl1 FI-FIl2 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-8119 
     LABEL "Läs in + konv" 
     SIZE 15 BY 1.14.

DEFINE BUTTON BUTTON-4 
     LABEL "Läs in + konv" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE FI-FIl1 AS CHARACTER FORMAT "X(256)":U INITIAL "C:~\Appdir~\Pressbyran~\Retail_Vare_8119.skv" 
     LABEL "Fil1" 
     VIEW-AS FILL-IN 
     SIZE 47 BY 1 NO-UNDO.

DEFINE VARIABLE FI-FIl2 AS CHARACTER FORMAT "X(256)":U INITIAL "C:~\Appdir~\Pressbyran~\Retail_Vare_6.skv" 
     LABEL "Fil2" 
     VIEW-AS FILL-IN 
     SIZE 47 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     B-8119 AT ROW 2.14 COL 71.2
     FI-FIl1 AT ROW 2.19 COL 18 COLON-ALIGNED
     FI-FIl2 AT ROW 3.48 COL 18 COLON-ALIGNED
     BUTTON-4 AT ROW 3.48 COL 71.2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 99.2 BY 10.14.


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
         TITLE              = "<insert window title>"
         HEIGHT             = 10.14
         WIDTH              = 99.2
         MAX-HEIGHT         = 18.52
         MAX-WIDTH          = 99.2
         VIRTUAL-HEIGHT     = 18.52
         VIRTUAL-WIDTH      = 99.2
         RESIZE             = yes
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
                                                                        */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* <insert window title> */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* <insert window title> */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-8119
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-8119 C-Win
ON CHOOSE OF B-8119 IN FRAME DEFAULT-FRAME /* Läs in + konv */
DO:
    DEFINE VARIABLE cLabels AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE icount AS INTEGER    NO-UNDO.
    DEFINE VARIABLE cSum   AS CHARACTER  NO-UNDO.
    cLabels =
 "EAN/PLU;Kvittotext;MD-text;Omsetning;Omsetning normal;Omsetning tilbud;Mengde;Bruttofortj;Bruttof %;Bruttofortj normal;Bruttofortj tilbud;SVI;Omsetning pr 100 kunder;Mengde pr 100 kunder;Bruttofortj pr 100 kunder".
cSum = ";;;404535,8;404535,8;0;6284;40530,15;10,01892787;40530,15;0;57,92578771;4348,444588;67,54810276;435,6675266".
EMPTY TEMP-TABLE TT_Rad.
    INPUT FROM VALUE(FI-Fil1:SCREEN-VALUE).
    REPEAT:
        CREATE TT_Rad.
        IMPORT DELIMITER ";" TT_Rad.
        icount = icount + 1.
    END.
    IF TT_Rad.c2 = "" AND TT_Rad.c3 = "" THEN 
        DELETE TT_Rad.
    INPUT CLOSE.
    FOR EACH TT_Rad:
        FIND FIRST BongLinje WHERE BongLinje.ButikkNr = 8119 AND BongLinje.StrekKode = TT_Rad.cEan NO-LOCK NO-ERROR.
        FIND StrekKode WHERE StrekKode.Kode = TT_Rad.cEAN NO-LOCK NO-ERROR.
        IF AVAIL StrekKode THEN
            FIND ArtBas OF StrekKode.
        ELSE
            RELEASE ArtBas.
        ASSIGN TT_Rad.cKvittoTxt = IF AVAIL Bonglinje THEN Bonglinje.BongTekst ELSE "---"
               TT_Rad.cMDText    = IF AVAIL ArtBas THEN ArtBas.Beskr ELSE "---".
    END.
    OUTPUT TO VALUE(REPLACE(FI-Fil1:SCREEN-VALUE,".skv","_ny.skv")).
    PUT UNFORMATTED cLabels SKIP.
    FOR EACH TT_Rad:
        EXPORT DELIMITER ";" cEAN     
                           cKvittoTxt 
                           cMDText    
                           c2         
                           c3         
                           c4         
                           c5         
                           c6         
                           c7         
                           c8         
                           c9         
                           c10        
                           c11        
                           c12        
                           c13.
/*         PUT UNFORMATTED cEAN ";"          */
/*                            cKvittoTxt ";" */
/*                            cMDText    ";" */
/*                            c2         ";" */
/*                            c3         ";" */
/*                            c4         ";" */
/*                            c5         ";" */
/*                            c6         ";" */
/*                            c7         ";" */
/*                            c8         ";" */
/*                            c9         ";" */
/*                            c10        ";" */
/*                            c11        ";" */
/*                            c12        ";" */
/*                            c13 SKIP.      */
    END.
    PUT UNFORMATTED cSum SKIP.
    OUTPUT CLOSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-4 C-Win
ON CHOOSE OF BUTTON-4 IN FRAME DEFAULT-FRAME /* Läs in + konv */
DO:
    DEFINE VARIABLE cLabels AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cSum    AS CHARACTER  NO-UNDO.
    cLabels =
  "EAN/PLU;Kvittotext;MD-text;Omsetning;Omsetning normal;Omsetning tilbud;Mengde;Bruttofortj;Bruttof %;Bruttofortj normal;Bruttofortj tilbud;SVI;Omsetning pr 100 kunder;Mengde pr 100 kunder;Bruttofortj pr 100 kunder".
    cSum = ";;;31971010,13;31971010,13;0;799893;3425641,52;10,71483668;3425641,52;0;35,68648383;2999,573124;75,04728611;321,3993613
".
    EMPTY TEMP-TABLE TT_Rad.
    INPUT FROM VALUE(FI-Fil2:SCREEN-VALUE).
    REPEAT:
        CREATE TT_Rad.
        IMPORT DELIMITER ";" TT_Rad.
    END.
    IF TT_Rad.c2 = "" AND TT_Rad.c3 = "" THEN 
        DELETE TT_Rad.
    INPUT CLOSE.
    FOR EACH TT_Rad:
        FIND FIRST BongLinje WHERE BongLinje.StrekKode = TT_Rad.cEan NO-LOCK NO-ERROR.
        FIND StrekKode WHERE StrekKode.Kode = TT_Rad.cEAN NO-LOCK NO-ERROR.
        IF AVAIL StrekKode THEN
            FIND ArtBas OF StrekKode.
        ELSE
            RELEASE ArtBas.
        ASSIGN TT_Rad.cKvittoTxt = IF AVAIL Bonglinje THEN Bonglinje.BongTekst ELSE "---"
               TT_Rad.cMDText    = IF AVAIL ArtBas THEN ArtBas.Beskr ELSE "---".
    END.
    OUTPUT TO VALUE(REPLACE(FI-Fil2:SCREEN-VALUE,".skv","_ny.skv")).
    PUT UNFORMATTED cLabels SKIP.
    FOR EACH TT_Rad:
        EXPORT DELIMITER ";" cEAN     
                           cKvittoTxt 
                           cMDText    
                           c2         
                           c3         
                           c4         
                           c5         
                           c6         
                           c7         
                           c8         
                           c9         
                           c10        
                           c11        
                           c12        
                           c13.
/*         PUT UNFORMATTED cEAN ";"          */
/*                            cKvittoTxt ";" */
/*                            cMDText    ";" */
/*                            c2         ";" */
/*                            c3         ";" */
/*                            c4         ";" */
/*                            c5         ";" */
/*                            c6         ";" */
/*                            c7         ";" */
/*                            c8         ";" */
/*                            c9         ";" */
/*                            c10        ";" */
/*                            c11        ";" */
/*                            c12        ";" */
/*                            c13 SKIP.      */
    END.
    PUT UNFORMATTED cSum SKIP.
    OUTPUT CLOSE.

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
  RUN enable_UI.
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
  DISPLAY FI-FIl1 FI-FIl2 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE B-8119 FI-FIl1 FI-FIl2 BUTTON-4 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

