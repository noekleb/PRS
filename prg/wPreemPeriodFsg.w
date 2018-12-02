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

DEFINE VARIABLE cHGundantag AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cButikliste AS CHARACTER  NO-UNDO.

DEFINE VARIABLE lClipboard  AS LOGICAL    NO-UNDO.

cHGundantag = "1,2,6,7,10,19,28,31,32,36,37,70,71,72,73,74,75,76,77,78,79,80,81,82,84".

DEFINE TEMP-TABLE TT_Rapport NO-UNDO
    FIELD Butikknr AS INTEGER
    FIELD TotFsgP1 AS DECI
    FIELD HgFsgP1  AS DECI
    FIELD KunderInneP1 AS INTE
    FIELD KunderUteP1 AS INTE
    FIELD TotFsgP2 AS DECI
    FIELD HgFsgP2  AS DECI
    FIELD KunderInneP2 AS INTE
    FIELD KunderUteP2 AS INTE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BUTTON-SokDatoP2 RECT-63 CB-ButikkTeam ~
FI-Hgliste FI-Dato FI-DatoTil FI-DatoP2 B-Rapport BUTTON-1 ~
BUTTON-SokDatoTil BUTTON-SokDato 
&Scoped-Define DISPLAYED-OBJECTS CB-ButikkTeam FI-Hgliste FI-Dato ~
FI-DatoTil FI-DatoP2 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-Rapport 
     LABEL "Rapport" 
     SIZE 15 BY 1.14.

DEFINE BUTTON BUTTON-1 
     IMAGE-UP FILE "icon/e-paste.bmp":U
     LABEL "Button 1" 
     SIZE 4.6 BY 1.1 TOOLTIP "Til clipboard Excelformat".

DEFINE BUTTON BUTTON-SokDato 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON BUTTON-SokDatoP2 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON BUTTON-SokDatoTil 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE VARIABLE CB-ButikkTeam AS CHARACTER FORMAT "X(256)":U 
     LABEL "Butikkteam" 
     VIEW-AS COMBO-BOX INNER-LINES 15
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Dato AS DATE FORMAT "99/99/99" 
     LABEL "Datum från/till period 1" 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1
     BGCOLOR 15 .

DEFINE VARIABLE FI-DatoP2 AS DATE FORMAT "99/99/99" 
     LABEL "Datum från period 2" 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1
     BGCOLOR 15 .

DEFINE VARIABLE FI-DatoTil AS DATE FORMAT "99/99/99" 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1
     BGCOLOR 15 .

DEFINE VARIABLE FI-Hgliste AS CHARACTER FORMAT "X(256)":U 
     LABEL "Inte medräknade hg" 
     VIEW-AS FILL-IN 
     SIZE 87 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-63
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 115 BY 11.19.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     BUTTON-SokDatoP2 AT ROW 8.14 COL 40
     CB-ButikkTeam AT ROW 2.91 COL 24.4 COLON-ALIGNED
     FI-Hgliste AT ROW 4.33 COL 24.4 COLON-ALIGNED
     FI-Dato AT ROW 6.81 COL 24.4 COLON-ALIGNED HELP
          "Transaksjonsdato"
     FI-DatoTil AT ROW 6.81 COL 44.2 COLON-ALIGNED HELP
          "Transaksjonsdato" NO-LABEL
     FI-DatoP2 AT ROW 8.1 COL 24.4 COLON-ALIGNED HELP
          "Transaksjonsdato"
     B-Rapport AT ROW 10.05 COL 26.4
     BUTTON-1 AT ROW 10.1 COL 44.2
     BUTTON-SokDatoTil AT ROW 6.81 COL 59.8
     BUTTON-SokDato AT ROW 6.81 COL 40
     RECT-63 AT ROW 1.24 COL 1.6
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 116.6 BY 11.76.


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
         TITLE              = "Periodjämförelse försäljning"
         HEIGHT             = 11.76
         WIDTH              = 116.6
         MAX-HEIGHT         = 28.48
         MAX-WIDTH          = 141.4
         VIRTUAL-HEIGHT     = 28.48
         VIRTUAL-WIDTH      = 141.4
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB C-Win 
/* ************************* Included-Libraries *********************** */

{dproclibstart.i}

/* _UIB-CODE-BLOCK-END */
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
ON END-ERROR OF C-Win /* Periodjämförelse försäljning */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Periodjämförelse försäljning */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Rapport
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Rapport C-Win
ON CHOOSE OF B-Rapport IN FRAME DEFAULT-FRAME /* Rapport */
DO:
    IF INPUT FI-Dato = ? THEN DO:
        MESSAGE "Fel datum, ange datum"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        APPLY "ENTRY" TO FI-Dato.
        RETURN NO-APPLY.
    END.
    ELSE IF INPUT FI-DatoTil = ? THEN DO:
        MESSAGE "Fel datum, ange datum"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        APPLY "ENTRY" TO FI-DatoTil.
        RETURN NO-APPLY.
    END.
    ELSE IF INPUT FI-Dato > INPUT FI-DatoTil THEN DO:
        MESSAGE "Fel datum, från datum > till datum"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        APPLY "ENTRY" TO FI-DatoTIl.
        RETURN NO-APPLY.
    END.
    ELSE IF INPUT FI-DatoP2 <> ? AND INPUT FI-DatoP2 >= INPUT FI-Dato AND INPUT FI-DatoP2 <= INPUT FI-DatoTil THEN DO:
            MESSAGE "Fel datum, från datum period 2 i period 1"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            APPLY "ENTRY" TO FI-DatoP2.
            RETURN NO-APPLY.
    END.
    {sww.i}
    FIND ButikkTeam WHERE ButikkTeam.BrGrpNr = INT(ENTRY(1,CB-ButikkTeam:SCREEN-VALUE,"|")) AND
                              ButikkTeam.TeamTypeId = 2 AND ButikkTeam.TeamNr = INT(ENTRY(2,CB-ButikkTeam:SCREEN-VALUE,"|")) NO-LOCK.
    ASSIGN cButikliste = "".
    FOR EACH ButikkKobling OF ButikkTeam.
        ASSIGN cButikliste = cButikliste + (IF cButikliste = "" THEN "" ELSE ",") 
                          + STRING(ButikkKobling.butik).
    END.
    RUN Rapport.
    {swn.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-1 C-Win
ON CHOOSE OF BUTTON-1 IN FRAME DEFAULT-FRAME /* Button 1 */
DO:
  lClipboard = TRUE.
  APPLY "CHOOSE" TO B-Rapport.
  lClipboard = FALSE.

  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokDato C-Win
ON CHOOSE OF BUTTON-SokDato IN FRAME DEFAULT-FRAME /* ... */
or F10 of FI-Dato
DO:
  /* Start søkeprogram */
    DEFINE VARIABLE cTittel AS CHARACTER INIT "Transdato" NO-UNDO.
    DEFINE VARIABLE dDato   AS DATE       NO-UNDO.
    ASSIGN dDato = INPUT FI-Dato.
    RUN kalender.w (INPUT-OUTPUT dDato,cTittel).
    IF RETURN-VALUE <> "<avbryt>" THEN DO:
        IF dDato > INPUT FI-DatoTil THEN DO:
            MESSAGE "Feil dato, > Til dato"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
        END.
        ELSE
            ASSIGN FI-Dato:SCREEN-VALUE = STRING(dDato).
    END.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokDatoP2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokDatoP2 C-Win
ON CHOOSE OF BUTTON-SokDatoP2 IN FRAME DEFAULT-FRAME /* ... */
or F10 of FI-Dato
DO:
  /* Start søkeprogram */
    DEFINE VARIABLE cTittel AS CHARACTER INIT "Transdato" NO-UNDO.
    DEFINE VARIABLE dDato   AS DATE       NO-UNDO.
    ASSIGN dDato = INPUT FI-Dato.
    RUN kalender.w (INPUT-OUTPUT dDato,cTittel).
    IF RETURN-VALUE <> "<avbryt>" THEN DO:
        ASSIGN FI-DatoP2:SCREEN-VALUE = STRING(dDato).
    END.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokDatoTil
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokDatoTil C-Win
ON CHOOSE OF BUTTON-SokDatoTil IN FRAME DEFAULT-FRAME /* ... */
or F10 of FI-DatoTil
DO:
  /* Start søkeprogram */
    DEFINE VARIABLE cTittel AS CHARACTER INIT "Transdato" NO-UNDO.
    DEFINE VARIABLE dDato   AS DATE       NO-UNDO.
    ASSIGN dDato = IF INPUT FI-DatoTil = ? THEN INPUT FI-Dato ELSE INPUT FI-DatoTil.
    RUN kalender.w (INPUT-OUTPUT dDato,cTittel).
    IF RETURN-VALUE <> "<avbryt>" THEN DO:
        IF dDato < INPUT FI-Dato THEN DO:
            MESSAGE "Feil dato, < Fra dato"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
        END.
        ELSE
            ASSIGN FI-DatoTil:SCREEN-VALUE = STRING(dDato).
    END.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CB-ButikkTeam
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-ButikkTeam C-Win
ON VALUE-CHANGED OF CB-ButikkTeam IN FRAME DEFAULT-FRAME /* Butikkteam */
DO:
  ASSIGN SELF:TOOLTIP = IF SELF:SCREEN-VALUE = "INGEN" THEN "" ELSE IF NUM-ENTRIES(SELF:SCREEN-VALUE,";") = 2 THEN
      ENTRY(1,SELF:SCREEN-VALUE,";") ELSE REPLACE(SELF:SCREEN-VALUE,CHR(1),",").
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
    FI-Hgliste  = cHGundantag.
    RUN InitCB.
  RUN enable_UI.
  {lng.i}
  APPLY "ENTRY" TO FI-Dato.
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
  DISPLAY CB-ButikkTeam FI-Hgliste FI-Dato FI-DatoTil FI-DatoP2 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE BUTTON-SokDatoP2 RECT-63 CB-ButikkTeam FI-Hgliste FI-Dato FI-DatoTil 
         FI-DatoP2 B-Rapport BUTTON-1 BUTTON-SokDatoTil BUTTON-SokDato 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
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
    DEFINE VARIABLE cListItemPairs    AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cButString        AS CHARACTER  NO-UNDO.
    FIND bruker WHERE bruker.brukerid = USERID("skotex") NO-LOCK.
    ASSIGN cListItemPairs = "".
    DO WITH FRAME {&FRAME-NAME}:
        FOR EACH ButikkTeam NO-LOCK WHERE ButikkTeam.BrGrpNr = Bruker.BrGrpNr AND
                                  ButikkTeam.TeamTypeId = 2 BY ButikkTeam.Beskrivelse.
            ASSIGN cButString = "".
/*             FOR EACH ButikkKobling OF ButikkTeam.                                         */
/*                 ASSIGN cButString = cButString + (IF cButString = "" THEN "" ELSE CHR(1)) */
/*                                   + STRING(ButikkKobling.butik).                          */
/*             END.                                                                          */
            ASSIGN cListItemPairs = cListItemPairs + (IF cListItemPairs = "" THEN "" ELSE ",") +
                                 ButikkTeam.Beskrivelse + "," + STRING(Bruker.BrGrpNr) + "|" + STRING(ButikkTeam.TeamNr).
        END.
        ASSIGN CB-ButikkTeam:LIST-ITEM-PAIRS = cListItemPairs
               CB-ButikkTeam:SCREEN-VALUE    = ENTRY(2,cListItemPairs).
        APPLY "VALUE-CHANGED" TO CB-ButikkTeam.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Rapport C-Win 
PROCEDURE Rapport :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE dDatum  AS DATE       NO-UNDO.
    DEFINE VARIABLE dDatumTilP2 AS DATE       NO-UNDO.
    DEFINE VARIABLE ii      AS INTEGER    NO-UNDO.
    DEFINE VARIABLE dTotSumP1 AS DECIMAL    NO-UNDO.
    DEFINE VARIABLE dTotMvaP1 AS DECIMAL    NO-UNDO.
    DEFINE VARIABLE dHGsumP1  AS DECIMAL    NO-UNDO.
    DEFINE VARIABLE dHGmvaP1  AS DECIMAL    NO-UNDO.
    DEFINE VARIABLE iAntInneP1 AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iAntUteP1 AS INTEGER    NO-UNDO.
    
    DEFINE VARIABLE dTotSumP2 AS DECIMAL    NO-UNDO.
    DEFINE VARIABLE dTotMvaP2 AS DECIMAL    NO-UNDO.
    DEFINE VARIABLE dHGsumP2  AS DECIMAL    NO-UNDO.
    DEFINE VARIABLE dHGmvaP2  AS DECIMAL    NO-UNDO.
    DEFINE VARIABLE iAntInneP2 AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iAntUteP2 AS INTEGER    NO-UNDO.
    DEFINE VARIABLE cLabels   AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cDetaljrad AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cFil AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE iRad AS INTEGER    NO-UNDO.
    DEFINE VARIABLE cSumCols    AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cSumString  AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cColAlign   AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cRowBold    AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cXrad       AS CHARACTER  NO-UNDO.

    cFil = SESSION:TEMP-DIRECTORY + "jmfrapp.txt".
    EMPTY TEMP-TABLE TT_Rapport.
    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN INPUT FI-DatoP2
               INPUT FI-DatoTil
               INPUT FI-Dato
               INPUT FI-Hgliste.
        DO ii = 1 TO NUM-ENTRIES(cButikliste):
            ASSIGN dTotSumP1 = 0
                   dHGsumP1  = 0
                   iAntInneP1 = 0
                   iAntUteP1  = 0
                   dTotMvaP1  = 0
                   dHGmvaP1   = 0
                   dTotSumP2 = 0
                   dHGsumP2  = 0
                   iAntInneP2 = 0
                   iAntUteP2  = 0
                   dTotMvaP2  = 0
                   dHGmvaP2   = 0.
            DO dDatum = FI-Dato TO FI-DatoTil:
                /* för att hitta bongar och räkna ut antal kunder */
                FOR EACH prBSdata WHERE prBSdata.butikknr = INT(ENTRY(ii,cButikliste)) AND
                                             prBSdata.dato     = dDatum NO-LOCK:
                    ASSIGN iAntInneP1 = iAntInneP1 + prBSData.AntKvittoInne
                           iAntUteP1  = iAntUteP1  + prBSData.AntKvittoUte.
                END.
                FOR EACH prFSdata WHERE prFSdata.ButikkNr = INT(ENTRY(ii,cButikliste)) AND
                                          prFSdata.Dato     = dDatum NO-LOCK.
/*                     prFSdata.MvaKr */
                    dTotSumP1 = dTotSumP1 + prFSdata.fsgnto.
                    dTotMvaP1 = dTotMvaP1 + prFSData.MvaKr.
                    /* FI-Hgliste innehåller det vi inte skall ha med */
                    IF NOT CAN-DO(FI-Hgliste,STRING(prFSdata.Hg)) THEN
                        ASSIGN dHGsumP1 = dHGsumP1 + prFSdata.fsgnto
                               dHGmvaP1 = dHGmvaP1 + prFSData.MvaKr.
                END.
            END.
            IF FI-DatoP2 <> ? THEN DO:
                dDatumTilP2 = FI-DatoP2 + (FI-DatoTil - FI-Dato).
                DO dDatum = FI-DatoP2 TO dDatumTilP2:
                    /* för att hitta bongar och räkna ut antal kunder */
                    FOR EACH prBSdata WHERE prBSdata.butikknr = INT(ENTRY(ii,cButikliste)) AND
                                                 prBSdata.dato     = dDatum NO-LOCK:
                        ASSIGN iAntInneP2 = iAntInneP2 + prBSData.AntKvittoInne
                               iAntUteP2  = iAntUteP2  + prBSData.AntKvittoUte.
                    END.
                    FOR EACH prFSdata WHERE prFSdata.ButikkNr = INT(ENTRY(ii,cButikliste)) AND
                                              prFSdata.Dato     = dDatum NO-LOCK.
    /*                     prFSdata.MvaKr */
                        dTotSumP2 = dTotSumP2 + prFSdata.fsgnto.
                        dTotMvaP2 = dTotMvaP2 + prFSData.MvaKr.
                        /* FI-Hgliste innehåller det vi inte skall ha med */
                        IF NOT CAN-DO(FI-Hgliste,STRING(prFSdata.Hg)) THEN
                            ASSIGN dHGsumP2 = dHGsumP2 + prFSdata.fsgnto
                                   dHGmvaP2 = dHGmvaP2 + prFSData.MvaKr.
                    END.
                END.
            END.
            CREATE TT_Rapport.
            ASSIGN TT_Rapport.Butikknr   = INT(ENTRY(ii,cButikliste))
                   TT_Rapport.TotFsgP1     = dTotSumP1 + dTotMvaP1
                   TT_Rapport.HgFsgP1      = dHGsumP1  + dHGmvaP1
                   TT_Rapport.KunderInneP1 = iAntInneP1
                   TT_Rapport.KunderUteP1  = iAntUteP1
                    TT_Rapport.TotFsgP2     = dTotSumP2 + dTotMvaP2
                    TT_Rapport.HgFsgP2      = dHGsumP2  + dHGmvaP2
                    TT_Rapport.KunderInneP2 = iAntInneP2
                    TT_Rapport.KunderUteP2  = iAntUteP2.
        
        END.
    END.
    IF lClipboard = TRUE THEN
        OUTPUT TO "CLIPBOARD".
    ELSE
        OUTPUT TO VALUE(cFil).

    cLabels = "Butik"           + CHR(9) +
              "Namn"            + CHR(9) +
              "Totfsg P1"       + CHR(9) +
              "Hgfsg P1"        + CHR(9) +
              "Kunder inne P1"  + CHR(9) +
              "Kunder tot P1"       + CHR(9) +
              " "               + CHR(9) +
              "Totfsg P2"       + CHR(9) +
              "Hgfsg P2"        + CHR(9) +
              "Kunder inne P2"  + CHR(9) +
              "Kunder tot P2"       + CHR(9) +
              " "               + CHR(9) +
              "Fsg/Kunder tot P1"   + CHR(9) +
              "Fsg/Kunder tot P2"   + CHR(9) +
              " "                   + CHR(9) +
              "Fsg/Kunder inne P1"   + CHR(9) +
              "Fsg/Kunder inne P2".

    IF lClipboard = TRUE THEN
        PUT UNFORMATTED cLabels SKIP.
    ELSE
        PUT UNFORMATTED "|" REPLACE(cLabels,CHR(9),"|").
/*     PUT UNFORMATTED "Butik"   CHR(9)         */
/*                      "Namn"   CHR(9)         */
/*                      "Totfsg P1"     CHR(9)  */
/*                     "Hgfsg P1"      CHR(9)   */
/*                     "Kunder P1" CHR(9)       */
/*                     "Fsg/Kunder P1"   CHR(9) */
/*                     " "            CHR(9)    */
/*                     "Totfsg P2"     CHR(9)   */
/*                    "Hgfsg P2"      CHR(9)    */
/*                    "Kunder P2" CHR(9)        */
/*                    "Fsg/Kunder P2"           */
/*                      SKIP.                   */
    FOR EACH TT_Rapport:
        FIND butiker WHERE butiker.butik = TT_Rapport.Butikknr NO-LOCK NO-ERROR.

        cDetaljrad = STRING(TT_Rapport.Butikknr)                           + CHR(9) +
                     (IF AVAIL butiker THEN butiker.butnamn ELSE "noname") + CHR(9) +
                     STRING(TT_Rapport.TotFsgP1,"->>>,>>>,>>9.99")         + CHR(9) +
            STRING(TT_Rapport.HgFsgP1,"->>>,>>>,>>9.99")                   + CHR(9) +
            STRING(TT_Rapport.KunderInneP1)                                + CHR(9) +
            STRING(TT_Rapport.KunderInneP1 + TT_Rapport.KunderUteP1)       + CHR(9) +
            " "                                                         + CHR(9) +
            STRING(TT_Rapport.TotFsgP2,"->>>,>>>,>>9.99")               + CHR(9) +
            STRING(TT_Rapport.HgFsgP2,"->>>,>>>,>>9.99")                + CHR(9) +
            STRING(TT_Rapport.KunderInneP2)                             + CHR(9) +
            STRING(TT_Rapport.KunderInneP2 + TT_Rapport.KunderUteP2)    + CHR(9) +
            " "                                                         + CHR(9) +
            (IF TT_Rapport.KunderInneP1 = 0 THEN "0" ELSE 
                                  STRING(ROUND(TT_Rapport.HgFsgP1 / (TT_Rapport.KunderInneP1 + TT_Rapport.KunderUteP1),2),"->>>,>>9.99")) + CHR(9) +
            (IF TT_Rapport.KunderInneP2 = 0 THEN "0" ELSE 
            STRING(ROUND(TT_Rapport.HgFsgP2 / (TT_Rapport.KunderInneP2 + TT_Rapport.KunderUteP2),2),"->>>,>>9.99")) + CHR(9) +
            " "                                                         + CHR(9) +
            (IF TT_Rapport.KunderInneP1 = 0 THEN "0" ELSE 
                                  STRING(ROUND(TT_Rapport.HgFsgP1 / TT_Rapport.KunderInneP1,2),"->>>,>>9.99")) + CHR(9) +
            (IF TT_Rapport.KunderInneP2 = 0 THEN "0" ELSE 
                                  STRING(ROUND(TT_Rapport.HgFsgP2 / TT_Rapport.KunderInneP2,2),"->>>,>>9.99")).
            
            iRad = iRad + 1.
            IF lClipboard = TRUE THEN
                PUT UNFORMATTED cDetaljrad SKIP.
            ELSE DO:
                cXrad = STRING(iRad) + "|" + REPLACE(cDetaljrad,CHR(9),"|").
                PUT CONTROL CHR(10) cXrad.
            END.
/*         PUT UNFORMATTED TT_Rapport.Butikknr   CHR(9)                                 */
/*                         (IF AVAIL butiker THEN butiker.butnamn ELSE "noname") CHR(9) */
/*                         TT_Rapport.TotFsgP1     CHR(9)                               */
/*                         TT_Rapport.HgFsgP1      CHR(9)                               */
/*                         TT_Rapport.KunderInneP1 CHR(9)                               */
/*                         ROUND(TT_Rapport.HgFsgP1 / TT_Rapport.KunderInneP1,2) CHR(9) */
/*                         " " CHR(9)                                                   */
/*                         TT_Rapport.TotFsgP2     CHR(9)                               */
/*                         TT_Rapport.HgFsgP2      CHR(9)                               */
/*                         TT_Rapport.KunderInneP2 CHR(9)                               */
/*                         ROUND(TT_Rapport.HgFsgP2 / TT_Rapport.KunderInneP2,2)        */
/*             SKIP.                                                                    */

    END.
    OUTPUT CLOSE.
    IF lClipboard = FALSE THEN DO:
        ASSIGN cSumCols =  "3,4,5,6,8,9,10,11,12,14"
               cSumString = "2,SUM"
               cColAlign  = "1,,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1".
               cRowBold  = "4;SUM,TOTAL".
        RUN wVisRapport.w (cFil,cSumCols,cSumString,cColAlign,
                           "Periodjämförelse försäljning" + CHR(2) + " P1 " + STRING(FI-Dato) + "-" + STRING(FI-DatoTil)
                           + (IF FI-DatoP2 <> ? THEN  " P2: " + STRING(FI-DatoP2) + "-" + STRING(dDatumTilP2) ELSE ""),cRowBold).
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE rapptmp C-Win 
PROCEDURE rapptmp :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*     DEFINE VARIABLE cButNrListe AS CHARACTER  NO-UNDO.                                                                              */
/*     DEFINE VARIABLE dArtikkelNr LIKE ArtBas.ArtikkelNr NO-UNDO.                                                                     */
/*     DEFINE VARIABLE dDatoLoop   AS DATE       NO-UNDO.                                                                              */
/*     DEFINE VARIABLE iCount      AS INTEGER    NO-UNDO.                                                                              */
/*     DEFINE VARIABLE iRadnr      AS INTEGER    NO-UNDO.                                                                              */
/*     DEFINE VARIABLE crad        AS CHARACTER  NO-UNDO.                                                                              */
/*     DEFINE VARIABLE cSumCols    AS CHARACTER  NO-UNDO.                                                                              */
/*     DEFINE VARIABLE cSumString  AS CHARACTER  NO-UNDO.                                                                              */
/*     DEFINE VARIABLE cColAlign   AS CHARACTER  NO-UNDO.                                                                              */
/*     DEFINE VARIABLE dTmpFraDato AS DATE       NO-UNDO.                                                                              */
/*     DEFINE VARIABLE dTmpTilDato AS DATE       NO-UNDO.                                                                              */
/*     DEFINE VARIABLE lPerArtikel AS LOGICAL    NO-UNDO.                                                                              */
/*     DEFINE VARIABLE dsumDiffPris AS DECIMAL    NO-UNDO.                                                                             */
/*     DEFINE VARIABLE dsumDiffVVK  AS DECIMAL    NO-UNDO.                                                                             */
/*     DEFINE VARIABLE dsumDiffDB   AS DECIMAL    NO-UNDO.                                                                             */
/*     DEFINE VARIABLE dtotDiffPris AS DECIMAL    NO-UNDO.                                                                             */
/*     DEFINE VARIABLE dtotDiffVVK  AS DECIMAL    NO-UNDO.                                                                             */
/*     DEFINE VARIABLE dtotDiffDB   AS DECIMAL    NO-UNDO.                                                                             */
/*     DEFINE VARIABLE cRowBold     AS CHARACTER  NO-UNDO.                                                                             */
/*                                                                                                                                     */
/*     DEFINE VARIABLE cFil AS CHARACTER  NO-UNDO.                                                                                     */
/*     cFil = SESSION:TEMP-DIRECTORY + "jmfrapp.txt".                                                                                  */
/*     OUTPUT TO VALUE(cFil).                                                                                                          */
/*     IF lAktuell OR iSortering = 1 THEN DO:                                                                                          */
/*         PUT UNFORMATTED "|Butik|Artikkelnr|Beskrivelse|Dato|Antal|Diff pris|Diff varekost|Diff TB".                                 */
/*         FOR EACH TT_Priskontroll:                                                                                                   */
/*         END.                                                                                                                        */
/*         ASSIGN cSumCols =  "6,7,8"                                                                                                  */
/*                cSumString = "4,SUM"                                                                                                 */
/*                cColAlign  = "1,1,,,1,1,1,1".                                                                                        */
/*     END.                                                                                                                            */
/*         ASSIGN iRadNr = iRadNR + 1                                                                                                  */
/*                cRad   = STRING(iRadNr) + "|" + " " +  "|" + " " + "|" +                                                             */
/*                   "TOTAL" + "|" + " " + "|" + " " + "|" +                                                                           */
/*                   STRING(dtotDiffPris,"->,>>>,>>9.99") + "|" + STRING(dtotDiffVVK,"->,>>>,>>9.99") + "|" +                          */
/*                   STRING(dtotDiffDB,"->,>>>,>>9.99").                                                                               */
/*         PUT CONTROL CHR(10) cRad.                                                                                                   */
/*         ASSIGN /* cSumCols =  "6,7,8"                                                                                               */
/*                cSumString = "4,SUM" */                                                                                              */
/*                cRowBold  = "3;SUM,TOTAL"                                                                                            */
/*                cColAlign  = "1,1,,,1,1,1,1".                                                                                        */
/*     END.                                                                                                                            */
/*         ASSIGN iRadNr = iRadNR + 1                                                                                                  */
/*                cRad   = STRING(iRadNr) + "|" + " " +  "|" + " " + "|" +                                                             */
/*                   "TOTAL" + "|" + " " + "|" + " " + "|" +                                                                           */
/*                   STRING(dtotDiffPris,"->,>>>,>>9.99") + "|" + STRING(dtotDiffVVK,"->,>>>,>>9.99") + "|" +                          */
/*                   STRING(dtotDiffDB,"->,>>>,>>9.99").                                                                               */
/*         PUT CONTROL CHR(10) cRad.                                                                                                   */
/*         ASSIGN /* cSumCols =  "6,7,8"                                                                                               */
/*                cSumString = "4,SUM" */                                                                                              */
/*                cRowBold  = "3;SUM,TOTAL"                                                                                            */
/*                cColAlign  = "1,1,,,1,1,1,1".                                                                                        */
/*     END.                                                                                                                            */
/*     OUTPUT CLOSE.                                                                                                                   */
/*         ASSIGN cSumCols =  "6,7,8"                                                                                                  */
/*                cSumString = "4,SUM"                                                                                                 */
/*                cColAlign  = "1,1,,,1,1,1,1".                                                                                        */
/*     RUN wVisRapport.w (cFil,cSumCols,cSumString,cColAlign,                                                                          */
/*                        "Prisavviksrapport" + CHR(2) + STRING(dFraDato) + "-" +                                                      */
/*                        STRING(dTilDato) + "  " + (IF NUM-ENTRIES(cButNrListe) = 1 THEN "Butik: " + cButNrListe                      */
/*                             ELSE "Butiker: " + ENTRY(1,cButNrListe) + "-" + ENTRY(NUM-ENTRIES(cButNrListe),cButNrListe)),cRowBold). */
/*     ASSIGN dFraDato = dTmpFraDato                                                                                                   */
/*            dTilDato = dTmpTilDato.                                                                                                  */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

