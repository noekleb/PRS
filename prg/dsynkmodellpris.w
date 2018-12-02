&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
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
DEFINE INPUT  PARAMETER dArtikkelnr AS DECIMAL    NO-UNDO.
DEFINE INPUT  PARAMETER ipProfilnr AS INTEGER    NO-UNDO.
/* Local Variable Definitions ---                                       */
DEFINE VARIABLE hBrCol AS HANDLE     NO-UNDO.
DEFINE VARIABLE iCol2X AS INTEGER    NO-UNDO.

DEFINE BUFFER FraArtbas  FOR artbas.
DEFINE BUFFER FraArtpris FOR artpris.

DEFINE TEMP-TABLE TT_Modellpris NO-UNDO
    FIELD NyKalkyle  AS LOGICAL
    FIELD Kopier     AS LOGICAL
    FIELD Artikkelnr LIKE Artbas.artikkelnr
    FIELD Beskr      LIKE Artbas.beskr
    FIELD Levfargkod LIKE Artbas.levfargkod
    FIELD nvarekost  LIKE Artpris.varekost[1]
    FIELD tvarekost  LIKE Artpris.varekost[1]
    FIELD npris      LIKE Artpris.pris[1]
    FIELD tpris      LIKE Artpris.pris[1]
    FIELD tilbud     LIKE artpris.tilbud.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame
&Scoped-define BROWSE-NAME BROWSE-1

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES TT_Modellpris

/* Definitions for BROWSE BROWSE-1                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-1 TT_Modellpris.Kopier TT_Modellpris.NyKalkyle TT_Modellpris.Artikkelnr TT_Modellpris.Beskr TT_Modellpris.Levfargkod TT_Modellpris.nvarekost TT_Modellpris.npris TT_Modellpris.tilbud TT_Modellpris.tvarekost TT_Modellpris.tpris " "   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-1   
&Scoped-define SELF-NAME BROWSE-1
&Scoped-define QUERY-STRING-BROWSE-1 FOR EACH TT_Modellpris
&Scoped-define OPEN-QUERY-BROWSE-1 OPEN QUERY {&SELF-NAME} FOR EACH TT_Modellpris.
&Scoped-define TABLES-IN-QUERY-BROWSE-1 TT_Modellpris
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-1 TT_Modellpris


/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define OPEN-BROWSERS-IN-QUERY-Dialog-Frame ~
    ~{&OPEN-QUERY-BROWSE-1}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-66 Btn_OK Btn_Cancel Btn_Help BROWSE-1 ~
FI-Txt FI-Txt-2 FI-ColHelp 
&Scoped-Define DISPLAYED-OBJECTS FI-NVareKost FI-ArtikkelNr FI-NPris ~
FI-Beskr FI-Tilbud FI-LevFargKod FI-TVareKost FI-TPris FI-Txt FI-Txt-2 ~
FI-ColHelp 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Nei" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_Help 
     LABEL "&Help" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "Ja" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE FI-ArtikkelNr AS DECIMAL FORMAT "zzzzzzzzzzzz9" INITIAL 0 
     LABEL "Artikkelnummer" 
     VIEW-AS FILL-IN 
     SIZE 20.2 BY 1.

DEFINE VARIABLE FI-Beskr AS CHARACTER FORMAT "x(30)" 
     LABEL "Beskrivelse" 
     VIEW-AS FILL-IN 
     SIZE 34 BY 1.

DEFINE VARIABLE FI-ColHelp AS CHARACTER FORMAT "X(256)":U INITIAL "Kopier J/N - Endre=DobbelKlikk i kollonne 'Kopier'" 
      VIEW-AS TEXT 
     SIZE 64 BY .62 NO-UNDO.

DEFINE VARIABLE FI-LevFargKod AS CHARACTER FORMAT "X(15)" 
     LABEL "LevFargKod" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE FI-NPris AS DECIMAL FORMAT "->,>>>,>>9.99" INITIAL 0 
     LABEL "Normalpris" 
     VIEW-AS FILL-IN 
     SIZE 20.2 BY 1.

DEFINE VARIABLE FI-NVareKost AS DECIMAL FORMAT "->>,>>9.99" INITIAL 0 
     LABEL "Normal VK" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE FI-Tilbud AS LOGICAL FORMAT "J/N" INITIAL NO 
     LABEL "På tilbud" 
     VIEW-AS FILL-IN 
     SIZE 8.6 BY 1.

DEFINE VARIABLE FI-TPris AS DECIMAL FORMAT "->,>>>,>>9.99" INITIAL 0 
     LABEL "Tilbudspris" 
     VIEW-AS FILL-IN 
     SIZE 20.2 BY 1.

DEFINE VARIABLE FI-TVareKost AS DECIMAL FORMAT "->>,>>9.99" INITIAL 0 
     LABEL "Tilbud Vk" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE FI-Txt AS CHARACTER FORMAT "X(256)":U INITIAL "Kopier prisendring til de andre artikklene i modellen" 
      VIEW-AS TEXT 
     SIZE 76 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FI-Txt-2 AS CHARACTER FORMAT "X(256)":U INITIAL "Prisen vil bli kopiert til disse artiklene" 
      VIEW-AS TEXT 
     SIZE 76 BY .62
     FONT 6 NO-UNDO.

DEFINE RECTANGLE RECT-66
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 89 BY 6.43.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-1 FOR 
      TT_Modellpris SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-1 Dialog-Frame _FREEFORM
  QUERY BROWSE-1 DISPLAY
      TT_Modellpris.Kopier FORMAT "J/N" LABEL "Kopier"
  TT_Modellpris.NyKalkyle  FORMAT "J/"    
  TT_Modellpris.Artikkelnr FORMAT ">>>>>>>>9" LABEL "Artikkelnr"
  TT_Modellpris.Beskr      FORMAT "x(30)" LABEL "Varetekst"
  TT_Modellpris.Levfargkod LABEL "Levfarg/farg"
  TT_Modellpris.nvarekost  LABEL "Normal Vk"
  TT_Modellpris.npris      LABEL "Normalpris"
  TT_Modellpris.tilbud     FORMAT " J/ N" LABEL "På tilbud"
  TT_Modellpris.tvarekost  LABEL "Tilbud Vk"
  TT_Modellpris.tpris      LABEL "Tilbudspris"
  " "
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 119 BY 8.71 FIT-LAST-COLUMN TOOLTIP "bbbbr".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     Btn_OK AT ROW 1.52 COL 106.2
     FI-NVareKost AT ROW 2.48 COL 67 COLON-ALIGNED HELP
          "Kalkulert varekost"
     FI-ArtikkelNr AT ROW 2.52 COL 17 COLON-ALIGNED
     Btn_Cancel AT ROW 2.76 COL 106.2
     FI-NPris AT ROW 3.48 COL 67 COLON-ALIGNED HELP
          "Pris inkl. mva."
     FI-Beskr AT ROW 3.52 COL 17 COLON-ALIGNED HELP
          "Kort beskrivelse av artikkelen"
     FI-Tilbud AT ROW 4.48 COL 67 COLON-ALIGNED HELP
          "Angir om artikkelen er på tilbud."
     FI-LevFargKod AT ROW 4.52 COL 17 COLON-ALIGNED HELP
          "Leverandørens fargekode"
     Btn_Help AT ROW 4.76 COL 106.2
     FI-TVareKost AT ROW 5.48 COL 67 COLON-ALIGNED HELP
          "Kalkulert varekost"
     FI-TPris AT ROW 6.48 COL 67 COLON-ALIGNED HELP
          "Pris inkl. mva."
     BROWSE-1 AT ROW 8.86 COL 2.2
     FI-Txt AT ROW 1.19 COL 5 COLON-ALIGNED NO-LABEL
     FI-Txt-2 AT ROW 8.1 COL 3 COLON-ALIGNED NO-LABEL
     FI-ColHelp AT ROW 17.67 COL 1 COLON-ALIGNED NO-LABEL
     RECT-66 AT ROW 1.48 COL 2
     SPACE(31.19) SKIP(11.03)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Kopier kalkyle for modell"
         DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_Cancel.


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
   FRAME-NAME                                                           */
/* BROWSE-TAB BROWSE-1 FI-TPris Dialog-Frame */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN FI-ArtikkelNr IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Beskr IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-LevFargKod IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-NPris IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-NVareKost IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Tilbud IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-TPris IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-TVareKost IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-1
/* Query rebuild information for BROWSE BROWSE-1
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH TT_Modellpris.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BROWSE-1 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON GO OF FRAME Dialog-Frame /* Kopier kalkyle for modell */
DO:
  RUN SynkPriser.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Kopier kalkyle for modell */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-1
&Scoped-define SELF-NAME BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-1 Dialog-Frame
ON DEFAULT-ACTION OF BROWSE-1 IN FRAME Dialog-Frame
DO:
    DEFINE VARIABLE iMousepos AS INTEGER    NO-UNDO.

    iMousePos = DYNAMIC-FUNCTION("getMousePosition",BROWSE BROWSE-1:FRAME,"x").
    IF iMousePos < BROWSE BROWSE-1:X + iCol2X THEN DO:
        TT_Modellpris.Kopier = NOT TT_Modellpris.Kopier = TRUE.
        BROWSE BROWSE-1:REFRESH().
    END.
/*       MESSAGE BROWSE BROWSE-1:X iMousePos iCol1X iCol2X */
/*       VIEW-AS ALERT-BOX INFO BUTTONS OK.                */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Help
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Help Dialog-Frame
ON CHOOSE OF Btn_Help IN FRAME Dialog-Frame /* Help */
OR HELP OF FRAME {&FRAME-NAME}
DO: /* Call Help Function (or a simple message). */
  MESSAGE "Help for File: {&FILE-NAME}" VIEW-AS ALERT-BOX INFORMATION.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.

FIND PrisProfil NO-LOCK WHERE
    PrisProfil.ProfilNr = ipProfilnr NO-ERROR.
IF AVAILABLE PrisProfil THEN DO:
    FRAME {&FRAME-NAME}:TITLE = FRAME {&FRAME-NAME}:TITLE + " for prisprofil: " + 
        string(PrisProfil.ProfilNr) + " " + PrisProfil.Beskrivelse.
    VIEW FRAME {&FRAME-NAME}.
END.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

    RUN FyllTT.
    IF NOT CAN-FIND(FIRST TT_Modellpris) THEN
        RETURN.
  RUN enable_UI.
  hBrCol = {&BROWSE-NAME}:GET-BROWSE-COLUMN(2).
  iCol2X = hBrCol:X.
  APPLY "ENTRY" TO Btn_Cancel.
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
  DISPLAY FI-NVareKost FI-ArtikkelNr FI-NPris FI-Beskr FI-Tilbud FI-LevFargKod 
          FI-TVareKost FI-TPris FI-Txt FI-Txt-2 FI-ColHelp 
      WITH FRAME Dialog-Frame.
  ENABLE RECT-66 Btn_OK Btn_Cancel Btn_Help BROWSE-1 FI-Txt FI-Txt-2 FI-ColHelp 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FyllTT Dialog-Frame 
PROCEDURE FyllTT :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE dmodellfarge AS DECIMAL    NO-UNDO.
/* FraArtbas  */
/* FraArtpris */
FIND FraArtbas WHERE FraArtbas.artikkelnr = dartikkelnr NO-LOCK NO-ERROR.
IF NOT AVAIL FraArtbas OR FraArtbas.modellfarge = 0 THEN
    RETURN.
FIND FIRST FraArtpris OF FraArtbas WHERE profilnr = ipprofilnr NO-LOCK NO-ERROR.
IF NOT AVAIL FraArtpris THEN
    RETURN.
FIND farg OF FraArtbas NO-LOCK NO-ERROR.
ASSIGN 
    FI-ArtikkelNr = FraArtbas.artikkeLnr
    FI-Beskr      = FraArtbas.beskr
    FI-LevFargKod = TRIM(FraArtbas.levfargkod)
    FI-LevFargKod = IF FI-LevFargKod = "" AND AVAIL farg THEN farg.farbeskr ELSE FI-LevFargKod
    FI-NVareKost  = FraArtpris.VareKost[1] 
    FI-TVareKost  = IF FraArtpris.tilbud THEN FraArtpris.VareKost[2] ELSE 0
    FI-NPris      = FraArtpris.pris[1]
    FI-TPris      = IF FraArtpris.tilbud THEN FraArtpris.pris[2] ELSE 0
    FI-Tilbud     = FraArtpris.tilbud.

dmodellfarge = FraArtbas.modellfarge.
FOR EACH Artbas WHERE artbas.modellfarge = dmodellfarge AND
                      artbas.artikkelnr  <> dartikkelnr NO-LOCK.
    IF ArtBas.Sanert <> ? THEN
        NEXT.
    FIND FIRST artpris WHERE artpris.artikkelnr = Artbas.artikkelnr AND
                             artpris.profilnr   = ipprofilnr NO-LOCK NO-ERROR.
/*     IF NOT AVAIL artpris THEN */
/*         NEXT.                 */
    FIND farg OF artbas NO-LOCK NO-ERROR.
    CREATE TT_Modellpris.
    ASSIGN TT_Modellpris.NyKalkyle  = NOT AVAIL artpris
           TT_Modellpris.Kopier     = NOT TT_Modellpris.NyKalkyle = TRUE /* om inte kalkyl finns så skall kopier vara false */
           TT_Modellpris.Artikkelnr = artbas.artikkelnr
           TT_Modellpris.Beskr      = artbas.beskr
           TT_Modellpris.Levfargkod = TRIM(artbas.levfargkod)
           TT_Modellpris.Levfargkod = IF TT_Modellpris.Levfargkod = "" AND AVAIL farg THEN farg.farbeskr ELSE TT_Modellpris.Levfargkod
           TT_Modellpris.nvarekost  = IF AVAIL Artpris THEN ArtPris.VareKost[1] ELSE FraArtPris.VareKost[1]
           TT_Modellpris.tvarekost  = IF AVAIL artpris AND artpris.tilbud = TRUE THEN ArtPris.VareKost[2] ELSE 0
           TT_Modellpris.npris      = IF AVAIL Artpris THEN Artpris.pris[1] ELSE FraArtpris.pris[1]
           TT_Modellpris.tpris      = IF AVAIL artpris AND artpris.tilbud = TRUE THEN Artpris.pris[2] ELSE 0
           TT_Modellpris.tilbud     = IF AVAIL artpris THEN artpris.tilbud ELSE FALSE.

END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SynkPriser Dialog-Frame 
PROCEDURE SynkPriser :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE BUFFER bufArtpris FOR ArtPris.
    DEFINE BUFFER bufPrisko FOR Prisko.
/* FraArtbas  */
/* FraArtpris */

    FIND FIRST bufartpris WHERE bufartpris.artikkelnr = dartikkelnr AND
                                bufartpris.profilnr   = ipprofilnr NO-LOCK.
    FOR EACH TT_Modellpris WHERE TT_Modellpris.Kopier = TRUE:
        DO TRANSACTION:
            FOR EACH prisko WHERE prisko.artikkelnr = TT_Modellpris.artikkelnr AND
                                  prisko.profilnr   = ipprofilnr:
                DELETE prisko.
            END.
            FOR EACH bufprisko WHERE bufprisko.artikkelnr = dartikkelnr AND
                                     bufprisko.profilnr   = ipprofilnr:
                CREATE prisko.
                BUFFER-COPY bufprisko EXCEPT artikkelnr TO prisko
                    ASSIGN prisko.artikkelnr = TT_Modellpris.artikkelnr.
                RELEASE prisko.
            END.
            IF TT_Modellpris.NyKalkyle = FALSE THEN
                FIND FIRST artpris WHERE artpris.artikkelnr = TT_Modellpris.artikkelnr AND
                                         artpris.profilnr   = ipprofilnr.
            ELSE
                CREATE Artpris.
            BUFFER-COPY bufartpris EXCEPT artikkelnr TO artpris
                ASSIGN artpris.artikkelnr = TT_Modellpris.artikkelnr.
            RELEASE artpris.
        END.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

