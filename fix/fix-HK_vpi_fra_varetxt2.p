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


DEFINE TEMP-TABLE TT_Vare
    FIELD butnr       AS INTEGER FORMAT ">>9"                      /* I (3)      Butikknr                                                                                 */
    FIELD ean         AS DECIMAL DECIMALS 0 FORMAT ">>>>>>>>>>>>9" /* DE (13,0)  EAN/PLU-nr                                                                               */
    FIELD hgr         AS INTEGER FORMAT ">>>9"                     /* I (4)      Varegruppenr                                                                             */
    FIELD bong        AS CHARACTER FORMAT "x(20)"                  /* C (20)     Bongtekst                                                                                */
    FIELD opris       AS LOGICAL                                   /* L (yes/no) Flagg for åpen pris                                                                      */
    FIELD link        AS DECIMAL DECIMALS 0 FORMAT ">>>>>>>>>>>>9" /* De (13,0)  EAN/PLU-nr for lenkevare (for eksempel pant)                                             */
    FIELD kotype      AS INTEGER FORMAT "9"                        /* I (1)      Køtype (endringstype) - 1=ny/endring, 9=sletting                                         */
    FIELD vekt        AS INTEGER FORMAT "9"                        /* I (1)      Vekt/salgsenhet (0=stk, 1=kg, 3=meter, 4=m², 5=liter)                                    */
    FIELD utprisn     AS DECIMAL DECIMALS 2 FORMAT ">>>>9.99"      /* De (5,2)   Ordinær utsalgspris                                                                      */
    FIELD bonus       AS LOGICAL                                   /* L (yes/no) Flagg som angir om varen gir kjøpeutbytte                                                */
    FIELD mva         AS DECIMAL DECIMALS 2 FORMAT ">9.99"         /* De (2,2)   Momssats                                                                                 */
    FIELD krabatt     AS LOGICAL                                   /* L (yes/no) Flagg for om varen skal gi automatisk kunderabatt (via varegr)                           */
    FIELD varetekst   AS CHARACTER FORMAT "x(20)"                  /* C (30)     Lang varetekst - i InfoPOS etikettekst 1, kundevennlig                                   */
    FIELD nettokr     AS DECIMAL DECIMALS 2 FORMAT ">>>>9.99"      /* DE (5,2)   Ordinær nettopris                                                                        */
    FIELD bestnr      AS INTEGER FORMAT ">>>>>>>9"                 /* I (8)      Bestillingsnr                                                                            */
    FIELD mixnr       AS INTEGER FORMAT ">>>9"                     /* I (4)      Kobling til mixmatch fil på kassene                                                      */
    FIELD pakkenr     AS INTEGER FORMAT ">>>9"                     /* I (4)      For pakkevarer angir dette kobling til innholdet (i mixmatch)                            */
    FIELD bestvare    AS LOGICAL                                   /* L (yes/no) Benyttes til pantevarer (yes=pantevare/no=vanlig)                                                                              */
    FIELD utprist     AS DECIMAL DECIMALS 2 FORMAT ">>>>9.99"      /* De (5,2)   Kampanjepris                                                                             */
    FIELD tidsstyrtt  AS LOGICAL                                   /* L (yes/no) Flagg for om kampanjepris skal være aktiv når tidsstyrt pris ellers er den aktive prisen */
    FIELD fradatot    AS DATE                                      /* Da         Fradato for kampanjepris                                                                 */
    FIELD fratidt     AS CHARACTER FORMAT "x(5)"                   /* C (hh:mm)  Fratidspunkt for kampanjepris                                                            */
    FIELD tildatot    AS DATE                                      /* Da         Tildato for kampanjepris                                                                 */
    FIELD tiltidt     AS CHARACTER FORMAT "x(5)"                   /* C (hh:mm)  Tiltidspunkt for kampanjepris                                                            */
    FIELD utprism     AS DECIMAL DECIMALS 2 FORMAT ">>>>9.99"      /* De (5,2)   Medlemspris                                                                              */
    FIELD tidsstyrtm  AS LOGICAL                                   /* L (yes/no) Flagg for om medlemspris skal være aktiv når tidsstyrt pris ellers er den aktive prisen  */
    FIELD fradatom    AS DATE                                      /* Da         Fradato for medlemspris                                                                  */
    FIELD fratidm     AS CHARACTER FORMAT "x(5)"                   /* C (hh:mm)  Fratidspunkt for medlemspris                                                             */
    FIELD tildatom    AS DATE                                      /* Da         Tildato for medlemspris                                                                  */
    FIELD tiltidm     AS CHARACTER FORMAT "x(5)"                   /* C (hh:mm)  Tiltidspunkt for medlemspris                                                             */
    FIELD utprisa     AS DECIMAL DECIMALS 2 FORMAT ">>>>9.99"      /* De (5,2)   Tidsstyrt pris                                                                           */
    FIELD fra         AS CHARACTER FORMAT "x(5)"                   /* C (hh:mm)  Fratidspunkt for tidsstyrt pris                                                          */
    FIELD til         AS CHARACTER FORMAT "x(5)"                   /* C (hh:mm)  Tiltidspunkt for tidsstyrt pris                                                          */
    FIELD idkrav      AS LOGICAL                                   /* L (yes/no) Flagg for om salg av vare er aldersbegrenset (legitimasjonsplikt)                        */
    FIELD lager       AS LOGICAL                                   /* L (yes/no) Flagg for om vare er on-line lagerstyrt (kun mot InfoPOS bakromsløsning)                 */
    FIELD individ     AS LOGICAL                                   /* L (yes/no) Flagg for om varen er en individvare                                                     */
    FIELD garantikl   AS INTEGER FORMAT ">>>9"                     /* I (2)      Garantiklasse                                                                            */
    FIELD bilde       AS CHARACTER FORMAT "x(20)"                  /* C (20)     Navn på evt. bildefil                                                                    */
    FIELD timestat    AS LOGICAL                                   /* L (yes/no) Flagg for om statistikk for varen skal lagres på timenivå                                */
    FIELD nettokrt    AS DECIMAL DECIMALS 2 FORMAT ">>>>9.99"      /* De (5,2)  Nettopris for kampanje                                                                    */
    FIELD Stoppkode          AS INTEGER FORMAT "9"                     /* integer 1 siffer 0=ikke stoppet, 2=stoppet, 3=opphev stopp (InfoPOS bakrom legger alltid ut 0)                       */
    FIELD Storrelsesnr   AS INTEGER FORMAT ">>>>9"                 /* integer 5 siffer SE/Gresvig har krav til inntil 5 siffer (0 i Coop)                                                  */
    FIELD Storrelsestype AS INTEGER FORMAT ">>9"                   /* integer 3 siffer Foreløpig kun i SE (0 i Coop). Hentes fra modell i InfoPOS  */
    FIELD Lopenr                 AS INTEGER FORMAT ">>>>>9"                /* integer 6 siffer Internt i SE (0 i Coop) */
    FIELD Handlingskode  AS INTEGER FORMAT ">>>>>9"                /* DECIMAL 6 siffer   Internt i SE (0 i Coop) */
    FIELD Modell                 AS DECIMAL DECIMALS 0 FORMAT ">>>>>>9"                /* decimal 7 siffer Gresvig/SE (0 i Coop) */
    FIELD Bestnr2                AS CHARACTER FORMAT "x(20)"               /* character 20 tegn Alfanumerisk bestnr. Foreløpig kun SE. Hentes fra pris.bestnr2. ("" i Coop) */
    FIELD Vekttilbud     AS INTEGER FORMAT "9"                     /* INTEGER 1 siffer Likt med felt 8 (vektkode ordinær pris), men dedikert til kampanje. */
    FIELD Varenr             AS DECIMAL DECIMALS 0 FORMAT ">>>>>>>>>>>>9" /* DECIMAL 13 siffer       12 siffer i Gresvig/SE. For søk/salg av varer som er merket på gammelmåten */
    FIELD Fargenr            AS INTEGER FORMAT ">>>>9"                   /* INTEGER 5 siffer SE/Gresvig har krav til inntil 5 siffer (0 i Coop) */
    FIELD LevNr          LIKE LevBas.LevNr
    INDEX ButStrkKotype butnr ean kotype DESCENDING.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS FI-Varefil FI-AntPlu FI-AntPLUStrek ~
FI-AntOPris FI-AntStrek FI-AntArt BUTTON-1 
&Scoped-Define DISPLAYED-OBJECTS FI-Varefil FI-AntPlu FI-AntPLUStrek ~
FI-AntOPris FI-AntStrek FI-AntArt 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-1 
     LABEL "Test läs in" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE FI-AntArt AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Antall Artikler" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-AntOPris AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Antall Opris (ArtNr <>Vg)" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-AntPlu AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Antall PLU (ArtNr = Vg)" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-AntPLUStrek AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Antall PLU-strek" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-AntStrek AS CHARACTER FORMAT "X(256)":U 
     LABEL "Antall Strek" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Varefil AS CHARACTER FORMAT "X(256)":U INITIAL "C:~\DB~\AlbertBoe~\vareISO_M_Lev.txt" 
     LABEL "Varefil" 
     VIEW-AS FILL-IN 
     SIZE 28 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     FI-Varefil AT ROW 2.19 COL 27.2 COLON-ALIGNED
     FI-AntPlu AT ROW 3.38 COL 27.2 COLON-ALIGNED
     FI-AntPLUStrek AT ROW 3.38 COL 60.4 COLON-ALIGNED
     FI-AntOPris AT ROW 4.57 COL 27.2 COLON-ALIGNED
     FI-AntStrek AT ROW 5.86 COL 60.2 COLON-ALIGNED
     FI-AntArt AT ROW 6 COL 27 COLON-ALIGNED
     BUTTON-1 AT ROW 7.67 COL 21
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 16.


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
         HEIGHT             = 16
         WIDTH              = 80
         MAX-HEIGHT         = 16
         MAX-WIDTH          = 80
         VIRTUAL-HEIGHT     = 16
         VIRTUAL-WIDTH      = 80
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


&Scoped-define SELF-NAME BUTTON-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-1 C-Win
ON CHOOSE OF BUTTON-1 IN FRAME DEFAULT-FRAME /* Test läs in */
DO:
  RUN LesInn.
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
  DISPLAY FI-Varefil FI-AntPlu FI-AntPLUStrek FI-AntOPris FI-AntStrek FI-AntArt 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE FI-Varefil FI-AntPlu FI-AntPLUStrek FI-AntOPris FI-AntStrek FI-AntArt 
         BUTTON-1 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LesInn C-Win 
PROCEDURE LesInn :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE ii AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iiAB AS INTEGER    NO-UNDO.
  DEFINE VARIABLE cStrek AS CHARACTER  NO-UNDO.
  DO WITH FRAME {&FRAME-NAME}:
      OUTPUT TO VALUE("C:\DB\AlbertBoe\NollLev2.txt").
/*       INPUT FROM VALUE("C:\DB\AlbertBoe\NollLev.txt") CONVERT SOURCE "IBM850" . */
      INPUT FROM VALUE("C:\DB\AlbertBoe\NollLev.txt").
      REPEAT:
          ii = ii + 1.
          CREATE TT_Vare.
          IMPORT TT_Vare.
          IF TT_Vare.LevNr = 0 THEN DO:
              cStrek = STRING(TT_Vare.ean,"9999999999999").
              FIND vpistrekkode WHERE vpistrekkode.ekstvpi = 1 AND vpistrekkode.kode = cStrek NO-LOCK NO-ERROR.
              IF AVAIL vpistrekkode THEN
                  FIND vpiartbas OF vpistrekkode NO-LOCK NO-ERROR.
              IF AVAIL vpiartbas THEN
                  ASSIGN TT_Vare.levnr = vpiartbas.levnr.
              PUT UNFORMATTED ean ";" TT_Vare.bong ";" TT_Vare.levnr ";" vpiartbas.levnr SKIP.
          END.
/*           IF TT_Vare.Levnr = 0 THEN DO:                                                    */
/*               FIND vpiartbas WHERE VPIArtBas.EkstVPILevNr = 1 AND                          */
/*                                    Vpiartbas.artikkelnr = TT_Vare.modell NO-LOCK NO-ERROR. */
/*               IF AVAIL vpiartbas THEN                                                      */
/*                   ASSIGN TT_Vare.levnr = vpiartbas.levnr.                                  */
/*           END.                                                                             */
/*           EXPORT TT_Vare.                                                                  */
/*           FIND artbas WHERE artbas.artikkelnr = TT_Vare.modell NO-LOCK NO-ERROR. */
/*           IF AVAIL Artbas THEN                                                   */
/*               ASSIGN TT_Vare.Levnr = Artbas.Levnr.                               */
/*           EXPORT TT_Vare.                                                        */
/*           IF ERROR-STATUS:ERROR THEN                 */
/*               MESSAGE ii                             */
/*                   VIEW-AS ALERT-BOX INFO BUTTONS OK. */
/*           ASSIGN FI-AntArt = FI-AntArt + 1.                   */
/*           IF modell > 9999 AND modell < 9000000 THEN          */
/*           PUT UNFORMATTED modell ";" bong ";" varetekst SKIP. */
          DELETE TT_Vare.
      END.
      INPUT CLOSE.
      OUTPUT CLOSE.
      DISPLAY FI-AntArt.
  END.
/*    
    FI-AntArt 
    FI-AntOPris 
    FI-AntPlu 
    FI-AntPLUStrek 
    FI-AntStrek 
 */   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

