&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS CB-Butik RADIO-SET-1 FI-Filnamn B-Export ~
FI-Txt1 
&Scoped-Define DISPLAYED-OBJECTS CB-Butik RADIO-SET-1 FI-Filnamn FI-Txt1 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD DatoChar C-Win 
FUNCTION DatoChar RETURNS CHARACTER
    ( INPUT dDato AS DATE ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-Export 
     LABEL "Lägg ut" 
     SIZE 29 BY 1.91
     FONT 8.

DEFINE VARIABLE CB-Butik AS INTEGER FORMAT ">>>>>9" INITIAL 0 
     LABEL "Butikk" 
     VIEW-AS COMBO-BOX INNER-LINES 20
     LIST-ITEM-PAIRS "0",1
     DROP-DOWN-LIST
     SIZE 46.8 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Filnamn AS CHARACTER FORMAT "X(256)":U INITIAL "C:~\tmp~\" 
     LABEL "KATALOG" 
     VIEW-AS FILL-IN 
     SIZE 33 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Txt1 AS CHARACTER FORMAT "X(256)":U INITIAL "MANUELLT UTLÄGG" 
      VIEW-AS TEXT 
     SIZE 52 BY 1.38
     FONT 8 NO-UNDO.

DEFINE VARIABLE RADIO-SET-1 AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Inget", 0,
"Säljare", 1,
"Inget", 3
     SIZE 12 BY 6.67 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     CB-Butik AT ROW 6.95 COL 19 COLON-ALIGNED HELP
          "Butikknummer"
     RADIO-SET-1 AT ROW 8.86 COL 12 NO-LABEL
     FI-Filnamn AT ROW 16.24 COL 19 COLON-ALIGNED
     B-Export AT ROW 18.14 COL 11
     FI-Txt1 AT ROW 4.81 COL 19 COLON-ALIGNED NO-LABEL
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 107.4 BY 29.14.


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
         HEIGHT             = 29.14
         WIDTH              = 107.4
         MAX-HEIGHT         = 29.14
         MAX-WIDTH          = 107.4
         VIRTUAL-HEIGHT     = 29.14
         VIRTUAL-WIDTH      = 107.4
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
   FRAME-NAME                                                           */
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


&Scoped-define SELF-NAME B-Export
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Export C-Win
ON CHOOSE OF B-Export IN FRAME DEFAULT-FRAME /* Lägg ut */
DO:
  MESSAGE cb-butik:SCREEN-VALUE SKIP cb-butik
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
  IF cb-butik:SCREEN-VALUE = ? THEN DO:
      MESSAGE "Välj butik"
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN NO-APPLY.
  END.
  ELSE
      RUN doexport.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CB-Butik
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-Butik C-Win
ON VALUE-CHANGED OF CB-Butik IN FRAME DEFAULT-FRAME /* Butikk */
DO:
    ASSIGN CB-Butik.
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
    RUN initCBbutiker.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE doexport C-Win 
PROCEDURE doexport :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE cFil AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cDatoTekst AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cString AS CHARACTER   NO-UNDO.
    cFil = RIGHT-TRIM(FI-Filnamn,"\") + "\" + "xPRSPosSelger." + string(CB-Butik).
    OUTPUT TO VALUE(cFil).
    FOR EACH ButikkSelger WHERE ButikkSelger.Butikk = CB-Butik NO-LOCK.
        FIND Selger WHERE Selger.SelgerNr = ButikkSelger.SelgerNr NO-LOCK NO-ERROR.
        IF AVAIL ButikkSelger THEN DO:
            FIND Selger WHERE Selger.SelgerNr = ButikkSelger.SelgerNr NO-LOCK NO-ERROR.
            IF AVAIL Selger THEN DO:
                cDatoTekst = DatoChar(Selger.FodtDato). 
    
                cString = "SELGER;" + 
                   "1" + ";" +
                   STRING(ButikkSelger.SelgerId) + ";" + 
                   REPLACE(REPLACE(Selger.NavnIKasse,";",""),'"'," ") + ";" +
                   STRING(Selger.SelgerNr) + ";" +       
                   STRING(Selger.Navn) + ";" +          
                   STRING(Selger.AnsattNr) + ";" +      
                   STRING(Selger.Adresse1) + ";" +      
                   STRING(Selger.Telefon) + ";" +       
                   STRING(Selger.PersonNr) + ";" +      
                   STRING(Selger.Mobiltelefon) + ";" +  
                   STRING(Selger.PostNr) + ";" +        
                   STRING(Selger.Adresse2) + ";" +      
                   STRING(Selger.ButikkNr) + ";" +      
                   STRING(Selger.BrukeridPRS) + ";" +   
                   STRING(Selger.ForNavn) + ";" +       
                   STRING(Selger.JobTittel) + ";" +     
                   cDatoTekst      
                   NO-ERROR.
                IF ERROR-STATUS:ERROR THEN
                    NEXT.
                ELSE
                    PUT UNFORMATTED cString SKIP.
            END.
        END.
    END.
    OUTPUT CLOSE.
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
  DISPLAY CB-Butik RADIO-SET-1 FI-Filnamn FI-Txt1 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE CB-Butik RADIO-SET-1 FI-Filnamn B-Export FI-Txt1 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initCBbutiker C-Win 
PROCEDURE initCBbutiker :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cLIP AS CHARACTER   NO-UNDO.
    FOR EACH butiker NO-LOCK WHERE CAN-FIND(FIRST ButikkSelger WHERE ButikkSelger.Butikk = butiker.butik):
        cLIP = cLIP + (IF cLIP <> "" THEN "," ELSE "") + butiker.butnamn + "," + STRING(butiker.butik).
    END.
    CB-Butik:LIST-ITEM-PAIRS IN FRAME {&FRAME-NAME} = cLIP.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION DatoChar C-Win 
FUNCTION DatoChar RETURNS CHARACTER
    ( INPUT dDato AS DATE ):
    /*------------------------------------------------------------------------------
            Purpose:                                                                      
            Notes:                                                                        
    ------------------------------------------------------------------------------*/
        DEFINE VARIABLE cTekst AS CHARACTER NO-UNDO.
        
        cTekst = IF dDato = ? 
                   THEN '00000000' 
                   ELSE (
                         STRING(YEAR(dDato),'9999') + 
                         STRING(MONTH(dDato),'99') + 
                         STRING(DAY(dDato),'99')
                        ).
        RETURN cTekst.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

