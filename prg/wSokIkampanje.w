&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          skotex           PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE TT_KampanjeHode NO-UNDO LIKE KampanjeHode.



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

DEFINE INPUT  PARAMETER dArtikkelnr AS DECIMAL     NO-UNDO.
DEFINE OUTPUT PARAMETER iKampanjeId AS INTEGER     NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BROWSE-2

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES TT_KampanjeHode

/* Definitions for BROWSE BROWSE-2                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-2 TT_KampanjeHode.KampanjeId ~
TT_KampanjeHode.Beskrivelse TT_KampanjeHode.Kamp% ~
TT_KampanjeHode.KampanjePris TT_KampanjeHode.StartDato ~
TT_KampanjeHode.AktiveresTid TT_KampanjeHode.SluttDato ~
TT_KampanjeHode.GyldigTilTid TT_KampanjeHode.LeverandorKampanje ~
TT_KampanjeHode.Aktivert TT_KampanjeHode.Komplett ~
TT_KampanjeHode.setAnnonse TT_KampanjeHode.ProfilNr ~
TT_KampanjeHode.RegistrertDato TT_KampanjeHode.RegistrertAv ~
TT_KampanjeHode.EDato TT_KampanjeHode.BrukerID TT_KampanjeHode.AvslagType ~
TT_KampanjeHode.ETid TT_KampanjeHode.KampId TT_KampanjeHode.NormalPris ~
TT_KampanjeHode.Notat TT_KampanjeHode.RegistrertTid 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-2 
&Scoped-define QUERY-STRING-BROWSE-2 FOR EACH TT_KampanjeHode NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BROWSE-2 OPEN QUERY BROWSE-2 FOR EACH TT_KampanjeHode NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BROWSE-2 TT_KampanjeHode
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-2 TT_KampanjeHode


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-BROWSE-2}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BROWSE-2 B-Velg B-Avbryt 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-Avbryt AUTO-END-KEY 
     LABEL "Avbryt" 
     SIZE 15 BY 1.14.

DEFINE BUTTON B-Velg AUTO-GO 
     LABEL "Velg" 
     SIZE 15 BY 1.14.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-2 FOR 
      TT_KampanjeHode SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-2 C-Win _STRUCTURED
  QUERY BROWSE-2 NO-LOCK DISPLAY
      TT_KampanjeHode.KampanjeId FORMAT "->,>>>,>>9":U
      TT_KampanjeHode.Beskrivelse FORMAT "X(40)":U
      TT_KampanjeHode.Kamp% FORMAT "->9.9":U
      TT_KampanjeHode.KampanjePris FORMAT "->>>,>>9.99":U
      TT_KampanjeHode.StartDato FORMAT "99/99/99":U
      TT_KampanjeHode.AktiveresTid FORMAT "->,>>>,>>9":U
      TT_KampanjeHode.SluttDato FORMAT "99/99/99":U
      TT_KampanjeHode.GyldigTilTid FORMAT "->,>>>,>>9":U
      TT_KampanjeHode.LeverandorKampanje FORMAT "yes/no":U
      TT_KampanjeHode.Aktivert FORMAT "yes/no":U
      TT_KampanjeHode.Komplett FORMAT "yes/no":U
      TT_KampanjeHode.setAnnonse FORMAT "yes/no":U
      TT_KampanjeHode.ProfilNr FORMAT ">>>>>>9":U
      TT_KampanjeHode.RegistrertDato FORMAT "99/99/9999":U
      TT_KampanjeHode.RegistrertAv FORMAT "X(10)":U
      TT_KampanjeHode.EDato FORMAT "99/99/99":U
      TT_KampanjeHode.BrukerID FORMAT "X(10)":U
      TT_KampanjeHode.AvslagType FORMAT ">9":U
      TT_KampanjeHode.ETid FORMAT "->,>>>,>>9":U
      TT_KampanjeHode.KampId FORMAT ">>>>>>>>>>>>9":U
      TT_KampanjeHode.NormalPris FORMAT "yes/no":U
      TT_KampanjeHode.Notat FORMAT "X(256)":U
      TT_KampanjeHode.RegistrertTid FORMAT "->,>>>,>>9":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 127 BY 17.1 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     BROWSE-2 AT ROW 1.52 COL 4
     B-Velg AT ROW 1.71 COL 133
     B-Avbryt AT ROW 3.38 COL 133
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 148.8 BY 18.19.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
   Temp-Tables and Buffers:
      TABLE: TT_KampanjeHode T "?" NO-UNDO SkoTex KampanjeHode
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Velg kampanje"
         HEIGHT             = 18.19
         WIDTH              = 148.8
         MAX-HEIGHT         = 26.33
         MAX-WIDTH          = 154.6
         VIRTUAL-HEIGHT     = 26.33
         VIRTUAL-WIDTH      = 154.6
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
/* BROWSE-TAB BROWSE-2 1 DEFAULT-FRAME */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-2
/* Query rebuild information for BROWSE BROWSE-2
     _TblList          = "Temp-Tables.TT_KampanjeHode"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   = Temp-Tables.TT_KampanjeHode.KampanjeId
     _FldNameList[2]   = Temp-Tables.TT_KampanjeHode.Beskrivelse
     _FldNameList[3]   = Temp-Tables.TT_KampanjeHode.Kamp%
     _FldNameList[4]   = Temp-Tables.TT_KampanjeHode.KampanjePris
     _FldNameList[5]   = Temp-Tables.TT_KampanjeHode.StartDato
     _FldNameList[6]   = Temp-Tables.TT_KampanjeHode.AktiveresTid
     _FldNameList[7]   = Temp-Tables.TT_KampanjeHode.SluttDato
     _FldNameList[8]   = Temp-Tables.TT_KampanjeHode.GyldigTilTid
     _FldNameList[9]   = Temp-Tables.TT_KampanjeHode.LeverandorKampanje
     _FldNameList[10]   = Temp-Tables.TT_KampanjeHode.Aktivert
     _FldNameList[11]   = Temp-Tables.TT_KampanjeHode.Komplett
     _FldNameList[12]   = Temp-Tables.TT_KampanjeHode.setAnnonse
     _FldNameList[13]   = Temp-Tables.TT_KampanjeHode.ProfilNr
     _FldNameList[14]   = Temp-Tables.TT_KampanjeHode.RegistrertDato
     _FldNameList[15]   = Temp-Tables.TT_KampanjeHode.RegistrertAv
     _FldNameList[16]   = Temp-Tables.TT_KampanjeHode.EDato
     _FldNameList[17]   = Temp-Tables.TT_KampanjeHode.BrukerID
     _FldNameList[18]   = Temp-Tables.TT_KampanjeHode.AvslagType
     _FldNameList[19]   = Temp-Tables.TT_KampanjeHode.ETid
     _FldNameList[20]   = Temp-Tables.TT_KampanjeHode.KampId
     _FldNameList[21]   = Temp-Tables.TT_KampanjeHode.NormalPris
     _FldNameList[22]   = Temp-Tables.TT_KampanjeHode.Notat
     _FldNameList[23]   = Temp-Tables.TT_KampanjeHode.RegistrertTid
     _Query            is OPENED
*/  /* BROWSE BROWSE-2 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Velg kampanje */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Velg kampanje */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Velg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Velg C-Win
ON CHOOSE OF B-Velg IN FRAME DEFAULT-FRAME /* Velg */
DO:
    IF AVAIL tt_kampanjehode THEN
        iKampanjeId = tt_kampanjehode.kampanjeid.
    APPLY "CLOSE" TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-2
&Scoped-define SELF-NAME BROWSE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-2 C-Win
ON DEFAULT-ACTION OF BROWSE-2 IN FRAME DEFAULT-FRAME /* Browse 1 */
DO:
  APPLY "CHOOSE" TO B-Velg.
  RETURN NO-APPLY.
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
    RUN SkapaTT.
  RUN enable_UI.
    {lng.i} /* Oversettelse */
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
  ENABLE BROWSE-2 B-Velg B-Avbryt 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkapaTT C-Win 
PROCEDURE SkapaTT :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FOR EACH Kampanjehode NO-LOCK WHERE CAN-FIND(FIRST kampanjelinje OF kampanjehode WHERE kampanjelinje.artikkelnr = dArtikkelnr).
        CREATE tt_kampanjehode.
        BUFFER-COPY kampanjehode TO tt_kampanjehode NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
            DELETE tt_kampanjehode.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

