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
DEFINE VARIABLE iStrTypeId AS INTEGER    NO-UNDO.
DEFINE VARIABLE iCL        AS INTEGER    NO-UNDO.
DEFINE VARIABLE iProfilNr  AS INTEGER    NO-UNDO.

DEFINE BUFFER bVarGr FOR VarGr.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BROWSE-2

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES VarGr

/* Definitions for BROWSE BROWSE-2                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-2 VarGr.Vg VarGr.VgBeskr VarGr.Hg ~
VarGr.Kost_Proc VarGr.MomsKod getArtikkelNr() getKostProc() 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-2 
&Scoped-define QUERY-STRING-BROWSE-2 FOR EACH VarGr NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BROWSE-2 OPEN QUERY BROWSE-2 FOR EACH VarGr NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BROWSE-2 VarGr
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-2 VarGr


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-BROWSE-2}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BROWSE-2 B-Generer TG-Alle B-Rekalk EDITOR-1 ~
FI-Txt 
&Scoped-Define DISPLAYED-OBJECTS TG-Alle EDITOR-1 FI-Txt 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getArtikkelNr C-Win 
FUNCTION getArtikkelNr RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getKostProc C-Win 
FUNCTION getKostProc RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD SetLopeNr C-Win 
FUNCTION SetLopeNr RETURNS INTEGER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-Generer 
     LABEL "Generer artikkel" 
     SIZE 19 BY 1.14.

DEFINE BUTTON B-Rekalk 
     LABEL "Rekalkuler" 
     SIZE 19 BY 1.14.

DEFINE VARIABLE EDITOR-1 AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL
     SIZE 106 BY 3.81 NO-UNDO.

DEFINE VARIABLE FI-Txt AS CHARACTER FORMAT "X(256)":U INITIAL " Generering av varegrupper til artikkler" 
      VIEW-AS TEXT 
     SIZE 46 BY .62
     BGCOLOR 10 FONT 6 NO-UNDO.

DEFINE VARIABLE TG-Alle AS LOGICAL INITIAL no 
     LABEL "Alle" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-2 FOR 
      VarGr SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-2 C-Win _STRUCTURED
  QUERY BROWSE-2 NO-LOCK DISPLAY
      VarGr.Vg FORMAT ">>>>>9":U
      VarGr.VgBeskr FORMAT "x(30)":U
      VarGr.Hg FORMAT ">>>>9":U
      VarGr.Kost_Proc COLUMN-LABEL "Kost%Vg" FORMAT ">>9.9":U
      VarGr.MomsKod FORMAT ">9":U WIDTH 10
      getArtikkelNr() COLUMN-LABEL "ArtikkelNr" FORMAT "x(10)":U
            WIDTH 15
      getKostProc() COLUMN-LABEL "Kost%Artikkel" FORMAT "x(6)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 106 BY 12.38 ROW-HEIGHT-CHARS .62 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     BROWSE-2 AT ROW 2.43 COL 2.4
     B-Generer AT ROW 2.57 COL 110
     TG-Alle AT ROW 4.33 COL 110.4
     B-Rekalk AT ROW 5.48 COL 110
     EDITOR-1 AT ROW 15.05 COL 3 NO-LABEL
     FI-Txt AT ROW 1.48 COL 2.8 NO-LABEL
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 128.6 BY 18.76.


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
         TITLE              = "Varegruppeartikkler"
         HEIGHT             = 18.76
         WIDTH              = 128.6
         MAX-HEIGHT         = 26.52
         MAX-WIDTH          = 136.4
         VIRTUAL-HEIGHT     = 26.52
         VIRTUAL-WIDTH      = 136.4
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = yes
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
/* BROWSE-TAB BROWSE-2 1 DEFAULT-FRAME */
/* SETTINGS FOR FILL-IN FI-Txt IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-2
/* Query rebuild information for BROWSE BROWSE-2
     _TblList          = "skotex.VarGr"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > skotex.VarGr.Vg
"VarGr.Vg" ? ">>>>>9" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > skotex.VarGr.VgBeskr
"VarGr.VgBeskr" ? "x(30)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > skotex.VarGr.Hg
"VarGr.Hg" ? ">>>>9" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > skotex.VarGr.Kost_Proc
"VarGr.Kost_Proc" "Kost%Vg" ">>9.9" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > skotex.VarGr.MomsKod
"VarGr.MomsKod" ? ">9" "integer" ? ? ? ? ? ? no ? no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > "_<CALC>"
"getArtikkelNr()" "ArtikkelNr" "x(10)" ? ? ? ? ? ? ? no ? no no "15" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > "_<CALC>"
"getKostProc()" "Kost%Artikkel" "x(6)" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE BROWSE-2 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Varegruppeartikkler */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Varegruppeartikkler */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Generer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Generer C-Win
ON CHOOSE OF B-Generer IN FRAME DEFAULT-FRAME /* Generer artikkel */
DO:
  ASSIGN EDITOR-1:SCREEN-VALUE = ""
         EDITOR-1:BGCOLOR      = ?.
  IF NOT TG-Alle:CHECKED THEN DO:

      IF CAN-FIND(FIRST ArtBas WHERE ArtBas.ArtikkelNr = VarGr.Vg) THEN DO:
          ASSIGN EDITOR-1:SCREEN-VALUE = "Artikkel finnes: " + STRING(VarGr.Vg) + CHR(10)
                 EDITOR-1:BGCOLOR = 12.
          RETURN.
      END.
      IF VarGr.Kost_Proc = 0 THEN DO:
          ASSIGN EDITOR-1:SCREEN-VALUE = "Kostprosent = 0: " + STRING(VarGr.Vg) + CHR(10)
                 EDITOR-1:BGCOLOR = 12.
          RETURN.
      END.
      FIND StrekKode WHERE StrekKode.Kode = STRING(VarGr.Vg) NO-LOCK NO-ERROR.
      IF AVAIL StrekKode THEN DO:
          ASSIGN EDITOR-1:SCREEN-VALUE = "Strekkode: " + STRING(VarGr.Vg) + 
              " finnes registrert på artikkel: " + STRING(StrekKode.ArtikkelNr) + CHR(10)
                 EDITOR-1:BGCOLOR = 12.
/*           MESSAGE "Varegruppen kann ikke generere artikkel." SKIP                 */
/*                   "Strekkode: " + STRING(VarGr.Vg) + " finnes registrert på" SKIP */
/*               "artikkel: " STRING(StrekKode.ArtikkelNr) "."                       */
/*               VIEW-AS ALERT-BOX INFO BUTTONS OK.                                  */
          RETURN.
      END.
      STATUS DEFAULT "Plu for varegruppe: " + STRING(VarGr.Vg) + ".".
/*       RUN SkapaArtBas (VarGr.Vg). */
      RUN vgartopris.p (?,VarGr.Vg).
      STATUS DEFAULT "Plu for varegruppe: " + STRING(VarGr.Vg) + ".".
      BROWSE {&BROWSE-NAME}:REFRESH().
  END.
  ELSE DO:
      RUN vgartopris.p (?,?).
/*       FOR EACH VarGr NO-LOCK WHERE VarGr.Kost_Proc > 0:                             */
/*                                                                                     */
/*           IF CAN-FIND(FIRST ArtBas WHERE ArtBas.ArtikkelNr = VarGr.Vg) THEN         */
/*               NEXT.                                                                 */
/*                                                                                     */
/*           IF CAN-FIND(FIRST StrekKode WHERE StrekKode.Kode = STRING(VarGr.Vg)) THEN */
/*               NEXT.                                                                 */
/*                                                                                     */
/*           STATUS DEFAULT "Plu for varegruppe: " + STRING(VarGr.Vg) + ".".           */
/*           RUN SkapaArtBas (VarGr.Vg).                                               */
/*           STATUS DEFAULT "".                                                        */
/*       END.                                                                          */
      STATUS DEFAULT "".
      ASSIGN TG-Alle:CHECKED = FALSE.
      {&OPEN-QUERY-{&BROWSE-NAME}}
  END.
  APPLY "ENTRY" TO BROWSE {&BROWSE-NAME}.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Rekalk
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Rekalk C-Win
ON CHOOSE OF B-Rekalk IN FRAME DEFAULT-FRAME /* Rekalkuler */
DO:
  ASSIGN EDITOR-1:SCREEN-VALUE = ""
         EDITOR-1:BGCOLOR      = ?.
  IF NOT TG-Alle:CHECKED THEN DO:

      IF NOT CAN-FIND(FIRST ArtBas WHERE ArtBas.ArtikkelNr = VarGr.Vg) THEN DO:
          ASSIGN EDITOR-1:SCREEN-VALUE = "Har ikke artikkel: " + STRING(VarGr.Vg) + CHR(10)
                 EDITOR-1:BGCOLOR = 12.
          RETURN.
      END.
      IF VarGr.Kost_Proc = 0 THEN DO:
          ASSIGN EDITOR-1:SCREEN-VALUE = "Kostprosent = 0: " + STRING(VarGr.Vg) + CHR(10)
                 EDITOR-1:BGCOLOR = 12.
          RETURN.
      END.
      STATUS DEFAULT "Plu for varegruppe: " + STRING(VarGr.Vg) + ".".
      RUN vgartopris.p (?,VarGr.Vg).
/*       RUN Rekalkyler (VarGr.Vg). */
      IF RETURN-VALUE <> "" THEN
          ASSIGN EDITOR-1:SCREEN-VALUE = RETURN-VALUE + CHR(10)
                 EDITOR-1:BGCOLOR = 12.
      STATUS DEFAULT "Plu for varegruppe: " + STRING(VarGr.Vg) + ".".
      BROWSE {&BROWSE-NAME}:REFRESH().
  END.
  ELSE DO:
      RUN vgartopris.p (?,?).
/*       FOR EACH VarGr NO-LOCK WHERE VarGr.Kost_Proc > 0:                                     */
/*           IF NOT CAN-FIND(FIRST ArtBas WHERE ArtBas.ArtikkelNr = VarGr.Vg) THEN             */
/*               NEXT.                                                                         */
/*                                                                                             */
/* /*           IF NOT CAN-FIND(FIRST StrekKode WHERE StrekKode.Kode = STRING(lPluNr)) THEN */ */
/* /*               NEXT.                                                                   */ */
/*                                                                                             */
/*           STATUS DEFAULT "Plu for varegruppe: " + STRING(VarGr.Vg) + ".".                   */
/*           RUN Rekalkyler (VarGr.Vg).                                                        */
/*           STATUS DEFAULT "".                                                                */
/*       END.                                                                                  */
      STATUS DEFAULT "".
      ASSIGN TG-Alle:CHECKED = FALSE.
      {&OPEN-QUERY-{&BROWSE-NAME}}
  END.
  APPLY "ENTRY" TO BROWSE {&BROWSE-NAME}.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-2
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

{syspara.i 50 15 1 iStrTypeID INT}
IF iStrTypeId = 0 THEN
    iStrTypeId = 2.
{syspara.i 5 1 1 iCL INT}
FIND Butiker NO-LOCK WHERE Butiker.Butik = iCL.
ASSIGN iProfilNr = Butiker.ProfilNr.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  {lng.i}
  
  DYNAMIC-FUNCTION("InitTranslation",THIS-PROCEDURE:CURRENT-WINDOW).

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
  DISPLAY TG-Alle EDITOR-1 FI-Txt 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE BROWSE-2 B-Generer TG-Alle B-Rekalk EDITOR-1 FI-Txt 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Kalkyle C-Win 
PROCEDURE Kalkyle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE lNy AS LOGICAL    NO-UNDO.
    FIND ArtPris WHERE ArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND
                       ArtPris.ProfilNr   = iProfilNr NO-ERROR.
    
    IF NOT AVAIL ArtPris THEN DO:
        CREATE ArtPris.
        ASSIGN  /* nyckelfält */
            ArtPris.ArtikkelNr = ArtBas.ArtikkelNr 
            ArtPris.ProfilNr   = iProfilNr
            ArtPris.AktivFraDato    = TODAY
            lNy = TRUE.
    END.
    IF lNy THEN DO:
        IF ArtPris.ValPris[1] = bVarGr.Kost_Proc AND
           ArtPris.Mva%[1]    = Moms.MomsProc THEN
            RETURN.
    END.
    ASSIGN
        ArtPris.DB%[1]          = 100 - bVarGr.Kost_Proc
        ArtPris.DBKr[1]         = 100 - bVarGr.Kost_Proc
        ArtPris.EuroManuel      = TRUE
        ArtPris.LevNr           = 999999
        ArtPris.Mva%[1]         = Moms.MomsProc
        ArtPris.MvaKr[1]        = Moms.MomsProc
        ArtPris.Pris[1]         = 100 + Moms.MomsProc
        ArtPris.ValPris[1]      = bVarGr.Kost_Proc
        ArtPris.InnkjopsPris[1] = bVarGr.Kost_Proc
        ArtPris.VareKost[1]     = bVarGr.Kost_Proc.
    RELEASE ArtPris.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Rekalkyler C-Win 
PROCEDURE Rekalkyler :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iVg AS INT NO-UNDO.

    FIND bVarGr WHERE bVarGr.Vg = iVg NO-LOCK.
    FIND Moms OF bVarGr NO-LOCK NO-ERROR.
    IF NOT AVAIL Moms THEN DO:
        RETURN "Varegruppe " + STRING(iVg) + ": Feil momskode".
    END.
    FIND ArtBas WHERE ArtBas.ArtikkelNr = iVg NO-LOCK NO-ERROR.
    IF NOT AVAIL ArtBas THEN
        RETURN.
    /* Oppstandelsen */

    /* Oppretter kalkyle for sentrallageret. */
    RUN Kalkyle.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkapaArtBas C-Win 
PROCEDURE SkapaArtBas :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iVg AS INT NO-UNDO.

    FIND bVarGr WHERE bVarGr.Vg = iVg NO-LOCK.
    FIND Moms OF bVarGr NO-LOCK NO-ERROR.
    IF NOT AVAIL Moms THEN DO:
        MESSAGE "Momskode for varegruppe " bVarGr.Vg " feilaktig."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN.
    END.
    FIND ArtBas WHERE ArtBas.ArtikkelNr = iVg NO-LOCK NO-ERROR.
    IF AVAIL ArtBas THEN
        RETURN.
    /* Oppstandelsen */
    DO:
        CREATE ArtBas.
        ASSIGN ArtBas.ArtikkelNr   = iVg
               ArtBas.Vg           = bVarGr.Vg
               ArtBas.Hg           = bVarGr.Hg
               ArtBas.VgKat        = 1
               ArtBas.LopNr        = ?
               ArtBas.StrTypeId    = iStrTypeId
               ArtBas.SaSong       = 1
               ArtBas.ArtSlag      = 4.
        /* Setter løpenummer */
        ASSIGN ArtBas.LopNr = SetLopeNr().
    END.
    ASSIGN ArtBas.Beskr        = bVarGr.VgBeskr
           ArtBas.BongTekst    = TRIM(SUBSTRING(bVarGr.VgBeskr,1,30))
           ArtBas.LevKod       = ""   
           ArtBas.VmId         = 0
           ArtBas.LevNr        = 999999
           ArtBas.Notat        = "PLU-artikkel varegruppe " + STRING(bVarGr.Vg)
           ArtBas.Farg         = 0
           ArtBas.ValKod       = ""
           ArtBas.Lager        = FALSE
           ArtBas.Storrelser   = TRUE
           ArtBas.Aktivert     = TRUE
           ArtBas.OPris        = TRUE
           ArtBas.Utgatt       = false
           .

    /* Oppretter pluLeverandør */
    IF NOT CAN-FIND(LevBas WHERE LevBas.LevNr = 999999) THEN
    DO:
      CREATE LevBas.
      ASSIGN
        LevBas.LevNr = 999999
        LevBas.LevNamn = "PLU leverandør"
        .
    END.

    /* Oppretter lagerposter */
    FOR EACH Butiker NO-LOCK WHERE
        Butiker.Butik > 0:
        IF NOT CAN-FIND(Lager WHERE Lager.ArtikkelNr = ArtBas.ArtikkelNr AND
                                    Lager.Butik      = Butiker.Butik) THEN DO:
            CREATE Lager.
            ASSIGN Lager.ArtikkelNr = ArtBas.ArtikkelNr
                   Lager.Butik      = Butiker.Butik.
        END.
    END.

    /* Oppretter kalkyle for sentrallageret. */
    RUN Kalkyle.
/*     IF NOT CAN-FIND(ArtPris WHERE ArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND */
/*                                   ArtPris.ProfilNr   = iProfilNr) THEN DO:   */
/*         CREATE ArtPris.                                                      */
/*         ASSIGN  /* nyckelfält */                                             */
/*             ArtPris.ArtikkelNr = ArtBas.ArtikkelNr                           */
/*             ArtPris.ProfilNr   = iProfilNr.                                  */
/*         ASSIGN                                                               */
/*             ArtPris.AktivFraDato    = TODAY                                  */
/*             ArtPris.DB%[1]          = 100 - bVarGr.Kost_Proc                 */
/*             ArtPris.DBKr[1]         = 100 - bVarGr.Kost_Proc                 */
/*             ArtPris.EuroManuel      = TRUE                                   */
/*             ArtPris.LevNr           = 999999                                 */
/*             ArtPris.Mva%[1]         = Moms.MomsProc                          */
/*             ArtPris.MvaKr[1]        = Moms.MomsProc                          */
/*             ArtPris.Pris[1]         = 100 + Moms.MomsProc                    */
/*             ArtPris.ValPris[1]      = bVarGr.Kost_Proc                       */
/*             ArtPris.InnkjopsPris[1] = bVarGr.Kost_Proc                       */
/*             ArtPris.VareKost[1]     = bVarGr.Kost_Proc.                      */
/*     END.                                                                     */
    CREATE StrekKode.
    ASSIGN StrekKode.ArtikkelNr = iVg
           StrekKode.Kode       = STRING(iVg)
           StrekKode.HovedNr    = TRUE
           StrekKode.VareId     = StrekKode.ArtikkelNr.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getArtikkelNr C-Win 
FUNCTION getArtikkelNr RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  FIND ArtBas WHERE ArtBas.ArtikkelNr = VarGr.Vg NO-LOCK NO-ERROR.
  RETURN IF AVAIL ArtBas THEN STRING(ArtBas.ArtikkelNr) ELSE "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getKostProc C-Win 
FUNCTION getKostProc RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  FIND ArtBas WHERE ArtBas.ArtikkelNr = VarGr.Vg NO-LOCK NO-ERROR.
  IF AVAIL ArtBas THEN
      FIND FIRST ArtPris OF ArtBas NO-LOCK NO-ERROR.
  RETURN IF AVAIL ArtBas AND AVAIL ArtPris THEN STRING(ArtPris.ValPris[1],">>9.99") ELSE "".   /* Function return value. */

  RETURN "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION SetLopeNr C-Win 
FUNCTION SetLopeNr RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  def var wLoop as int no-undo.
      
  DEF BUFFER bufArtBas FOR ArtBas.

  FINN-NESTE:  
  repeat wLoop = 1 to 10000:
  
    if wLoop = 0 then
      next FINN-NESTE.
      
    if can-find(bufArtBas no-lock where
      bufArtBas.Vg    = ArtBas.Vg and
      bufArtBas.LopNr = wLoop) then
      do:
        next FINN-NESTE.
      end.
    else
      leave FINN-NESTE.          
  end. /* FINN-NESTE */
  
  if wLoop > 9999 then
      RETURN ?.
  ELSE
      RETURN wLoop.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

