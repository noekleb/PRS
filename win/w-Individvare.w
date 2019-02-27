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
DEF INPUT PARAMETER dArtikkelNr LIKE ArtBas.ArtikkelNr NO-UNDO.
def input parameter wCurrent-Window    as handle no-undo.
def input parameter wParentHandle      as handle no-undo.

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE cEmptyField   AS CHARACTER LABEL "" NO-UNDO.
DEFINE VARIABLE wOk           AS LOGICAL    NO-UNDO.
DEFINE VARIABLE wBekreft      AS LOG        NO-UNDO.
DEFINE VARIABLE hEtikettVindu AS HANDLE     NO-UNDO.
DEFINE VARIABLE hIndividDet   AS HANDLE     NO-UNDO.
define temp-table tmpChild
  field wChild as handle.

{runlib.i} /* Starter procedurebibloteket. */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-Individ
&Scoped-define BROWSE-NAME BROWSE-Individvare

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Individ

/* Definitions for BROWSE BROWSE-Individvare                            */
&Scoped-define FIELDS-IN-QUERY-BROWSE-Individvare Individ.BatchNr ~
Individ.butnr Individ.individnr Individ.IndividType Individ.Storl ~
Individ.serienr Individ.Garantinummer Individ.salgdato Individ.Pris ~
Individ.kundenr Individ.Navn 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-Individvare 
&Scoped-define QUERY-STRING-BROWSE-Individvare FOR EACH Individ ~
      WHERE Individ.ArtikkelNr = ArtBas.ArtikkelNr NO-LOCK
&Scoped-define OPEN-QUERY-BROWSE-Individvare OPEN QUERY BROWSE-Individvare FOR EACH Individ ~
      WHERE Individ.ArtikkelNr = ArtBas.ArtikkelNr NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BROWSE-Individvare Individ
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-Individvare Individ


/* Definitions for FRAME FRAME-Individ                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BROWSE-Individvare BUTTON-Ny BUTTON-Endre ~
BUTTON-Slett BUTTON-Etikett BUTTON-Oppdater 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-Endre 
     LABEL "Endre..." 
     SIZE 15 BY 1.14.

DEFINE BUTTON BUTTON-Etikett 
     LABEL "Etikett" 
     SIZE 15 BY 1.14.

DEFINE BUTTON BUTTON-Ny 
     LABEL "Ny..." 
     SIZE 15 BY 1.14.

DEFINE BUTTON BUTTON-Oppdater 
     LABEL "Oppdater" 
     SIZE 15 BY 1.14.

DEFINE BUTTON BUTTON-Slett 
     LABEL "Slett" 
     SIZE 15 BY 1.14.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-Individvare FOR 
      Individ SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-Individvare
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-Individvare C-Win _STRUCTURED
  QUERY BROWSE-Individvare NO-LOCK DISPLAY
      Individ.BatchNr FORMAT "zzzzzzzz9":U
      Individ.butnr FORMAT ">>>>9":U
      Individ.individnr FORMAT ">>>>>>>>>>>9":U
      Individ.IndividType FORMAT ">9":U
      Individ.Storl FORMAT "x(4)":U
      Individ.serienr FORMAT "X(20)":U WIDTH 20.4
      Individ.Garantinummer FORMAT "X(25)":U
      Individ.salgdato FORMAT "99/99/99":U WIDTH 11
      Individ.Pris FORMAT "->>,>>9.99":U
      Individ.kundenr FORMAT ">>>>>9":U
      Individ.Navn FORMAT "X(40)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 156.2 BY 19.62 ROW-HEIGHT-CHARS .62.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-Individ
     BROWSE-Individvare AT ROW 3.48 COL 2
     BUTTON-Ny AT ROW 4.19 COL 162
     BUTTON-Endre AT ROW 5.48 COL 162
     BUTTON-Slett AT ROW 6.76 COL 162
     BUTTON-Etikett AT ROW 8.19 COL 162
     BUTTON-Oppdater AT ROW 11 COL 162
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 9.81
         SIZE 202 BY 23.14.


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
/* SUPPRESS Window definition (used by the UIB) 
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = ""
         HEIGHT             = 29.24
         WIDTH              = 176
         MAX-HEIGHT         = 29.24
         MAX-WIDTH          = 176
         VIRTUAL-HEIGHT     = 29.24
         VIRTUAL-WIDTH      = 176
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
                                                                        */
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME
ASSIGN C-Win = CURRENT-WINDOW.




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME FRAME-Individ
   NOT-VISIBLE                                                          */
/* BROWSE-TAB BROWSE-Individvare 1 FRAME-Individ */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-Individvare
/* Query rebuild information for BROWSE BROWSE-Individvare
     _TblList          = "SkoTex.Individ"
     _Options          = "NO-LOCK"
     _Where[1]         = "Individ.ArtikkelNr = ArtBas.ArtikkelNr"
     _FldNameList[1]   = SkoTex.Individ.BatchNr
     _FldNameList[2]   = SkoTex.Individ.butnr
     _FldNameList[3]   = SkoTex.Individ.individnr
     _FldNameList[4]   = SkoTex.Individ.IndividType
     _FldNameList[5]   = SkoTex.Individ.Storl
     _FldNameList[6]   > SkoTex.Individ.serienr
"Individ.serienr" ? ? "character" ? ? ? ? ? ? no ? no no "20.4" yes no no "U" "" ""
     _FldNameList[7]   = SkoTex.Individ.Garantinummer
     _FldNameList[8]   > SkoTex.Individ.salgdato
"Individ.salgdato" ? "99/99/99" "date" ? ? ? ? ? ? no ? no no "11" yes no no "U" "" ""
     _FldNameList[9]   = SkoTex.Individ.Pris
     _FldNameList[10]   = SkoTex.Individ.kundenr
     _FldNameList[11]   = SkoTex.Individ.Navn
     _Query            is NOT OPENED
*/  /* BROWSE BROWSE-Individvare */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-Individ
/* Query rebuild information for FRAME FRAME-Individ
     _Query            is NOT OPENED
*/  /* FRAME FRAME-Individ */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win
DO:
    if can-find(first tmpChild where
                 valid-handle(tmpChild.wChild)) then
      do:
        wBekreft = false.
        message 'Det er startet andre programmer fra dette vinduet.' skip
                'Avsluttes dette vinduet, vil alle underliggende programmer' skip
                'også bli avsluttet.'
                view-as alert-box warning buttons yes-no title 'Bekreft avsluttning'
                update wBekreft
                .
      end.
    else wBekreft = true.
    if wBekreft <> true then
    return no-apply.

    /* This event will close the window and terminate the procedure.  */
    APPLY "CLOSE":U TO THIS-PROCEDURE.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-Individvare
&Scoped-define SELF-NAME BROWSE-Individvare
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-Individvare C-Win
ON DEFAULT-ACTION OF BROWSE-Individvare IN FRAME FRAME-Individ
DO:
  IF AVAILABLE Individ THEN
      APPLY "CHOOSE" TO BUTTON-ENDRE IN FRAME FRAME-Individ.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-Individvare C-Win
ON VALUE-CHANGED OF BROWSE-Individvare IN FRAME FRAME-Individ
DO:
    ASSIGN BUTTON-Ny:SENSITIVE    = ArtBas.IndividType > 0
           BUTTON-Endre:SENSITIVE = BROWSE {&BROWSE-NAME}:FOCUSED-ROW <> ?
           BUTTON-Slett:SENSITIVE = BROWSE {&BROWSE-NAME}:FOCUSED-ROW <> ?
                                     AND Individ.salgdato = ?
           BUTTON-Etikett:SENSITIVE = BUTTON-Slett:SENSITIVE
           BUTTON-Oppdater:SENSITIVE    = ArtBas.IndividType > 0.

    IF VALID-HANDLE(hIndividDet) THEN
        RUN UpdateFromKey IN hIndividDet (ArtBas.IndividType,Individ.IndividNr).


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Endre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Endre C-Win
ON CHOOSE OF BUTTON-Endre IN FRAME FRAME-Individ /* Endre... */
DO:
    IF NOT AVAILABLE ArtBas THEN
        RETURN NO-APPLY.
    IF NOT AVAILABLE Individ THEN
        RETURN NO-APPLY.

    IF NOT VALID-HANDLE(hIndividDet) THEN
    DO:
        RUN IndividDet.w PERSIST SET hIndividDet (THIS-PROCEDURE).
        RUN InitFromKey IN hIndividDet (ArtBas.IndividType,Individ.IndividNr,BROWSE-Individvare:HANDLE).
        RUN setArtikkelNr IN hIndividDet (ArtBas.ArtikkelNr).
    END.
    ELSE DO:
        RUN UpdateFromKey IN hIndividDet (ArtBas.IndividType,Individ.IndividNr).
        RUN setArtikkelNr IN hIndividDet (ArtBas.ArtikkelNr).
    END.

    RUN MoveToTop IN hIndividDet.
    BROWSE {&BROWSE-NAME}:REFRESH().
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Etikett
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Etikett C-Win
ON CHOOSE OF BUTTON-Etikett IN FRAME FRAME-Individ /* Etikett */
DO:
      DEFINE VARIABLE bOK               AS LOGICAL    NO-UNDO.
      DEFINE VARIABLE cIndividRowIdList AS CHARACTER  NO-UNDO.
      DEFINE VARIABLE cIndividIdList    AS CHARACTER  NO-UNDO.
      DEFINE VARIABLE iAnt              AS INTEGER          NO-UNDO.
      DEFINE VARIABLE cStrekKode LIKE StrekKode.Kode NO-UNDO.
      DEFINE VARIABLE iCount            AS INTEGER    NO-UNDO.
      
      
      FIND FIRST Strekkode WHERE StrekKode.Artikkelnr = Individ.ArtikkelNr AND
                                 StrekKode.StrKode    = Individ.StrKode    AND
                                 StrekKode.Kodetype   = 1                  AND
                             NOT StrekKode.Kode BEGINS "02" NO-LOCK NO-ERROR.
      IF NOT AVAIL StrekKode THEN
          FIND FIRST Strekkode WHERE StrekKode.Artikkelnr = Individ.ArtikkelNr AND
                                     StrekKode.StrKode    = Individ.StrKode    AND
                                     StrekKode.Kodetype   = 1 NO-LOCK NO-ERROR.
      IF NOT AVAIL StrekKode THEN DO:
          MESSAGE "Finner ingen strekkode."
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
          RETURN NO-APPLY.
      END.
      ASSIGN cIndividRowIdList = STRING(ROWID(Individ)).
      RUN JBoxSelector.w (THIS-PROCEDURE,0,
                          "Individ;IndividNr;Storl;!ArtikkelNr;!Salgdato",
                          "where Individ.ArtikkelNr = " + STRING(ArtBas.ArtikkelNr) + 
                          " AND Individ.salgdato = '?'",
                          INPUT-OUTPUT cIndividRowIdList,
                          "IndividNr",
                          INPUT-OUTPUT cIndividIdList,
                          "","",
                          OUTPUT bOK).
      IF bOK = FALSE OR cIndividIdList = "" THEN
            RETURN NO-APPLY.
      DO iCount = 1 TO NUM-ENTRIES(cIndividIdList,"|").
          FIND Individ WHERE Individ.IndividNr = DECI(ENTRY(iCount,cIndividIdList,"|")) NO-LOCK NO-ERROR.
          IF NOT AVAIL Individ THEN
              NEXT.
          FIND FIRST Strekkode WHERE StrekKode.Artikkelnr = Individ.ArtikkelNr AND
                                     StrekKode.StrKode    = Individ.StrKode    AND
                                     StrekKode.Kodetype   = 1                  AND
                                 NOT StrekKode.Kode BEGINS "02" NO-LOCK NO-ERROR.
          IF NOT AVAIL StrekKode THEN
              FIND FIRST Strekkode WHERE StrekKode.Artikkelnr = Individ.ArtikkelNr AND
                                         StrekKode.StrKode    = Individ.StrKode    AND
                                         StrekKode.Kodetype   = 1 NO-LOCK NO-ERROR.
          IF NOT AVAIL StrekKode THEN
              NEXT.
          IF NOT VALID-HANDLE(hEtikettVindu) THEN
              RUN w-TmpEtikett.w PERSISTENT SET hEtikettVindu (C-Win).
          IF VALID-HANDLE(hEtikettVindu) THEN
              RUN NyEtikett IN hEtikettVindu (StrekKode.Kode,1,Individ.individnr).
      END.
      /*   RUN ButtonEnaDis. */
      ASSIGN CURRENT-WINDOW                = wCurrent-Window
             THIS-PROCEDURE:CURRENT-WINDOW = wCurrent-Window.
      RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Ny
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Ny C-Win
ON CHOOSE OF BUTTON-Ny IN FRAME FRAME-Individ /* Ny... */
DO:
    IF NOT AVAILABLE ArtBas THEN
        RETURN NO-APPLY.
    IF NOT AVAILABLE Individ THEN
        RETURN NO-APPLY.

    IF NOT VALID-HANDLE(hIndividDet) THEN
    DO:
        RUN IndividDet.w PERSIST SET hIndividDet (THIS-PROCEDURE).
        RUN InitFromKey IN hIndividDet (ArtBas.IndividType,'?',BROWSE-Individvare:HANDLE).
        RUN setArtikkelNr IN hIndividDet (ArtBas.ARtikkelNr).
    END.
    ELSE DO:
        RUN UpdateFromKey IN hIndividDet (ArtBas.IndividType,'?').
        RUN setArtikkelNr IN hIndividDet (ArtBas.ArtikkelNr).
    END.

    RUN MoveToTop IN hIndividDet.
    BROWSE {&BROWSE-NAME}:REFRESH() NO-ERROR.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Oppdater
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Oppdater C-Win
ON CHOOSE OF BUTTON-Oppdater IN FRAME FRAME-Individ /* Oppdater */
DO:
    RUN OpenQueryIndivid.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Slett
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Slett C-Win
ON CHOOSE OF BUTTON-Slett IN FRAME FRAME-Individ /* Slett */
DO:
  MESSAGE "Vill du slette individvaren? " VIEW-AS ALERT-BOX QUESTION
                BUTTONS YES-NO UPDATE wSvar AS LOGI.
  IF wSvar THEN DO TRANSACTION:
      FIND CURRENT Individ EXCLUSIVE.
      DELETE Individ.
      BROWSE {&BROWSE-NAME}:DELETE-CURRENT-ROW().
      APPLY "VALUE-CHANGED" TO BROWSE BROWSE-Individvare.
  END.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = wCurrent-Window 
       THIS-PROCEDURE:CURRENT-WINDOW = wCurrent-Window.             

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
  ON CLOSE OF THIS-PROCEDURE DO:
  /*    RUN SaveBrowseSettings. */
      RUN SlettTmpChild.
      IF VALID-HANDLE(wParentHandle) THEN
          RUN SlettProg in wParentHandle.
      IF VALID-HANDLE(hEtikettVindu) THEN
          APPLY "CLOSE" TO hEtikettVindu.
      IF VALID-HANDLE(hIndividDet) THEN
          DELETE PROCEDURE hIndividDet.
      RUN disable_UI.
  END.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  FIND ArtBas WHERE ArtBas.ArtikkelNr = dArtikkelNr NO-LOCK.
  RUN enable_UI.
  {lng.i}
  SUBSCRIBE TO "ByttObjekt" IN wParentHandle NO-ERROR.
      BROWSE BROWSE-Individvare:MOVE-TO-TOP().
  IF ArtBas.IndiVidType > 0 THEN
      {&OPEN-QUERY-BROWSE-Individvare}
  APPLY "VALUE-CHANGED" TO BROWSE BROWSE-Individvare.
  APPLY "ENTRY" TO  BROWSE BROWSE-Individvare.
  FRAME FRAME-Individ:SENSITIVE = artbas.sanertdato = ?.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ByttObjekt C-Win 
PROCEDURE ByttObjekt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER ipArtikkelNr LIKE ArtBas.ArtikkelNr NO-UNDO.
  FIND ArtBas WHERE ArtBas.ArtikkelNr = ipArtikkelNr NO-LOCK.
  FRAME FRAME-Individ:SENSITIVE = artbas.sanertdato = ?.

  ASSIGN dArtikkelNr = ipArtikkelnr.
  {sww.i}  
        {&OPEN-QUERY-BROWSE-Individvare}
/*     ELSE                                     */
/*         CLOSE QUERY BROWSE-Individvare. */
    APPLY "VALUE-CHANGED" TO BROWSE BROWSE-Individvare.
  {swn.i}

  IF VALID-HANDLE(hIndividDet) THEN
  DO:
      RUN UpdateFromKey IN hIndividDet (ArtBas.IndividType,Individ.IndividNr).
      RUN setArtikkelNr IN hIndividDet (ArtBas.ARtikkelNr).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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
  /* Hide all frames. */
  HIDE FRAME FRAME-Individ.
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
  ENABLE BROWSE-Individvare BUTTON-Ny BUTTON-Endre BUTTON-Slett BUTTON-Etikett 
         BUTTON-Oppdater 
      WITH FRAME FRAME-Individ.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-Individ}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LukkIndDet C-Win 
PROCEDURE LukkIndDet :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  IF VALID-HANDLE(hIndividDet) THEN
      DELETE PROCEDURE hIndividDet.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MoveToTopp C-Win 
PROCEDURE MoveToTopp :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   IF FRAME FRAME-Individ:MOVE-TO-TOP() THEN.
   APPLY "ENTRY" TO  BROWSE BROWSE-Individvare.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE openQueryBrowse C-Win 
PROCEDURE openQueryBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    {&OPEN-QUERY-{&BROWSE-NAME}}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpenQueryIndivid C-Win 
PROCEDURE OpenQueryIndivid :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {&OPEN-QUERY-BROWSE-Individvare}
  APPLY "VALUE-CHANGED" TO BROWSE BROWSE-Individvare.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE refreshBrowse C-Win 
PROCEDURE refreshBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    BROWSE {&BROWSE-NAME}:REFRESH().
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetEntry C-Win 
PROCEDURE SetEntry :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    APPLY "ENTRY" TO BROWSE BROWSE-Individvare.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SlettTmpChild C-Win 
PROCEDURE SlettTmpChild :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FOR EACH tmpChild:
        IF VALID-HANDLE(tmpChild.wChild) THEN
            DELETE PROCEDURE tmpChild.wChild.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

