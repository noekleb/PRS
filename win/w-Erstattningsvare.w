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
DEFINE VARIABLE hJmfRutine  AS HANDLE     NO-UNDO.
DEFINE VARIABLE cEmptyField AS CHARACTER LABEL "" NO-UNDO.
DEFINE VARIABLE iErstattId  AS INTEGER    NO-UNDO.
DEFINE VARIABLE wOk         AS LOGICAL    NO-UNDO.
DEFINE VARIABLE wBekreft    AS LOG        NO-UNDO.
DEFINE VARIABLE lPakke      AS LOGICAL    NO-UNDO.

define temp-table tmpChild
  field wChild as handle.

{runlib.i} /* Starter procedurebibloteket. */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-Pakkelinje
&Scoped-define BROWSE-NAME BROWSE-Erstattningsvare

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Erstattningsvare ArtBas

/* Definitions for BROWSE BROWSE-Erstattningsvare                       */
&Scoped-define FIELDS-IN-QUERY-BROWSE-Erstattningsvare ~
Erstattningsvare.ArtikkelNr ArtBas.Vg ArtBas.LopNr ArtBas.Beskr ~
ArtBas.BongTekst ArtBas.LevNr ArtBas.LevKod 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-Erstattningsvare 
&Scoped-define QUERY-STRING-BROWSE-Erstattningsvare FOR EACH Erstattningsvare ~
      WHERE Erstattningsvare.ErstattId = iErstattId ~
 AND Erstattningsvare.ArtikkelNr <> dArtikkelNr NO-LOCK, ~
      EACH ArtBas OF Erstattningsvare NO-LOCK
&Scoped-define OPEN-QUERY-BROWSE-Erstattningsvare OPEN QUERY BROWSE-Erstattningsvare FOR EACH Erstattningsvare ~
      WHERE Erstattningsvare.ErstattId = iErstattId ~
 AND Erstattningsvare.ArtikkelNr <> dArtikkelNr NO-LOCK, ~
      EACH ArtBas OF Erstattningsvare NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BROWSE-Erstattningsvare Erstattningsvare ~
ArtBas
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-Erstattningsvare Erstattningsvare
&Scoped-define SECOND-TABLE-IN-QUERY-BROWSE-Erstattningsvare ArtBas


/* Definitions for FRAME FRAME-Pakkelinje                               */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-57 BROWSE-Erstattningsvare BUTTON-Ny ~
BUTTON-Slett B-Jamfor B-ByttArtBas 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE Image-Sko AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chImage-Sko AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-ByttArtBas 
     LABEL "&Gå til artikkel" 
     SIZE 22 BY 1.14.

DEFINE BUTTON B-Jamfor 
     LABEL "Sa&mmenlign..." 
     SIZE 22 BY 1.14.

DEFINE BUTTON BUTTON-Ny 
     LABEL "Ny..." 
     SIZE 22 BY 1.14.

DEFINE BUTTON BUTTON-Slett 
     LABEL "Slett" 
     SIZE 22 BY 1.14.

DEFINE RECTANGLE RECT-57
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 26 BY 5.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-Erstattningsvare FOR 
      Erstattningsvare, 
      ArtBas SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-Erstattningsvare
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-Erstattningsvare C-Win _STRUCTURED
  QUERY BROWSE-Erstattningsvare NO-LOCK DISPLAY
      Erstattningsvare.ArtikkelNr FORMAT "zzzzzzzzzzzz9":U
      ArtBas.Vg COLUMN-LABEL "Vg" FORMAT "zzzzz9":U
      ArtBas.LopNr COLUMN-LABEL "Løpenr" FORMAT "->>>>>9":U
      ArtBas.Beskr FORMAT "x(20)":U
      ArtBas.BongTekst FORMAT "X(30)":U
      ArtBas.LevNr COLUMN-LABEL "Levnr" FORMAT "zzzzz9":U
      ArtBas.LevKod FORMAT "x(20)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 164.2 BY 19.62 ROW-HEIGHT-CHARS .62 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-Pakkelinje
     BROWSE-Erstattningsvare AT ROW 3.29 COL 1.8
     BUTTON-Ny AT ROW 8.76 COL 173.2
     BUTTON-Slett AT ROW 10.05 COL 173.2
     B-Jamfor AT ROW 12.62 COL 173.2
     B-ByttArtBas AT ROW 13.91 COL 173.2
     RECT-57 AT ROW 3.48 COL 171.6
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 9.86
         SIZE 202 BY 23.05.


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
         HEIGHT             = 31.95
         WIDTH              = 202
         MAX-HEIGHT         = 31.95
         MAX-WIDTH          = 202.4
         VIRTUAL-HEIGHT     = 31.95
         VIRTUAL-WIDTH      = 202.4
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
/* SETTINGS FOR FRAME FRAME-Pakkelinje
   NOT-VISIBLE FRAME-NAME                                               */
/* BROWSE-TAB BROWSE-Erstattningsvare RECT-57 FRAME-Pakkelinje */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-Erstattningsvare
/* Query rebuild information for BROWSE BROWSE-Erstattningsvare
     _TblList          = "SkoTex.Erstattningsvare,SkoTex.ArtBas OF SkoTex.Erstattningsvare"
     _Options          = "NO-LOCK"
     _Where[1]         = "SkoTex.Erstattningsvare.ErstattId = iErstattId
 AND SkoTex.Erstattningsvare.ArtikkelNr <> dArtikkelNr"
     _FldNameList[1]   = SkoTex.Erstattningsvare.ArtikkelNr
     _FldNameList[2]   > SkoTex.ArtBas.Vg
"ArtBas.Vg" "Vg" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > SkoTex.ArtBas.LopNr
"ArtBas.LopNr" "Løpenr" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   = SkoTex.ArtBas.Beskr
     _FldNameList[5]   = SkoTex.ArtBas.BongTekst
     _FldNameList[6]   > SkoTex.ArtBas.LevNr
"ArtBas.LevNr" "Levnr" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   = SkoTex.ArtBas.LevKod
     _Query            is NOT OPENED
*/  /* BROWSE BROWSE-Erstattningsvare */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-Pakkelinje
/* Query rebuild information for FRAME FRAME-Pakkelinje
     _Query            is NOT OPENED
*/  /* FRAME FRAME-Pakkelinje */
&ANALYZE-RESUME

 


/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME Image-Sko ASSIGN
       FRAME           = FRAME FRAME-Pakkelinje:HANDLE
       ROW             = 3.86
       COLUMN          = 172.8
       HEIGHT          = 4.19
       WIDTH           = 23.4
       HIDDEN          = no
       SENSITIVE       = yes.
/* Image-Sko OCXINFO:CREATE-CONTROL from: {9A93B740-C96B-11D0-8883-444553540000} type: Picbuf */
      Image-Sko:MOVE-AFTER(BROWSE-Erstattningsvare:HANDLE IN FRAME FRAME-Pakkelinje).

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


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


&Scoped-define SELF-NAME B-ByttArtBas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-ByttArtBas C-Win
ON CHOOSE OF B-ByttArtBas IN FRAME FRAME-Pakkelinje /* Gå til artikkel */
DO:
    RUN SlettTmpChild.
    RUN HentVis IN wParentHandle (?,Erstattningsvare.ArtikkelNr).
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Jamfor
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Jamfor C-Win
ON CHOOSE OF B-Jamfor IN FRAME FRAME-Pakkelinje /* Sammenlign... */
DO:
    DEFINE VARIABLE iCount AS INTEGER    NO-UNDO.
    DEFINE BUFFER bErstattningsvare FOR Erstattningsvare.
    IF NOT AVAIL ArtBas THEN
        RETURN.
    IF NOT VALID-HANDLE(hJmfRutine) THEN DO:
        RUN w-bildejmf.w PERSISTENT SET hJmfRutine.
        create tmpChild.
        ASSIGN tmpChild.wChild = hJmfRutine.
        RUN NyArtBas IN hJmfRutine (dArtikkelNr).
        FOR EACH bErstattningsvare WHERE bErstattningsvare.ErstattId =
            iErstattId AND bErstattningsvare.ArtikkelNr <> dArtikkelNr NO-LOCK.
            ASSIGN iCount = iCount + 1.
            RUN NyArtBas IN hJmfRutine (bErstattningsvare.ArtikkelNr).
            IF iCount = 5 THEN
                LEAVE.
        END.
    END.
    ELSE
        RUN NyArtBas IN hJmfRutine (Erstattningsvare.ArtikkelNr).
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-Erstattningsvare
&Scoped-define SELF-NAME BROWSE-Erstattningsvare
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-Erstattningsvare C-Win
ON VALUE-CHANGED OF BROWSE-Erstattningsvare IN FRAME FRAME-Pakkelinje
DO:
    IF BROWSE BROWSE-Erstattningsvare:FOCUSED-ROW <> ? THEN DO:
        RUN VisBilde(1).
    END.
    ELSE
        chImage-Sko:Picbuf:CLEAR(2).
    ASSIGN BUTTON-Ny:SENSITIVE    = NOT lPakke = TRUE AND ArtBas.OPris = FALSE
           BUTTON-Slett:SENSITIVE = BROWSE {&BROWSE-NAME}:FOCUSED-ROW <> ?
           B-Jamfor:SENSITIVE     = BUTTON-Slett:SENSITIVE
           B-ByttArtBas:SENSITIVE = BUTTON-Slett:SENSITIVE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Ny
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Ny C-Win
ON CHOOSE OF BUTTON-Ny IN FRAME FRAME-Pakkelinje /* Ny... */
DO:
    DEFINE VARIABLE wArtikkelNr AS dec      NO-UNDO.
    DEFINE VARIABLE xArtikkelnr LIKE ArtBas.ArtikkelNr NO-UNDO.
    DEFINE BUFFER bErstattningsvare FOR Erstattningsvare.
    DEFINE BUFFER bArtBas FOR ArtBas.
    DEFINE VARIABLE iAktErstattId AS INTEGER    NO-UNDO.
    /* se om artikelkortets artbas har en erstattningsvara */
    FIND bArtBas WHERE bArtBas.Artikkelnr = dArtikkelNr NO-LOCK.
    IF bArtBas.ModellFarge > 0 AND bArtBas.ArtikkelNr <> bArtBas.ModellFarge THEN DO:
        MESSAGE "Erstatningsvare registreres kun på hovedvare."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN NO-APPLY.
    END.
    FIND bErstattningsvare WHERE bErstattningsvare.ArtikkelNr = dArtikkelNr NO-LOCK NO-ERROR.
    ASSIGN iAktErstattId = IF AVAIL bErstattningsvare THEN bErstattningsvare.ErstattId ELSE ?.
    run d-hsok.w (output wArtikkelNr,"").
    IF wArtikkelNr <> ? THEN DO:
      FIND ArtBas WHERE
          ArtBas.ArtikkelNr = wArtikkelNr NO-LOCK NO-ERROR.
      IF AVAIL ArtBas AND ArtBas.Pakke = TRUE THEN DO:
          MESSAGE "Pakkevare kann ikke kobles."
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
          RETURN NO-APPLY.
      END.
      ELSE IF AVAIL ArtBas AND ArtBas.ArtikkelNr = dArtikkelNr THEN DO:
          MESSAGE "Samme artikkel, kann ikke velges."
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
          RETURN NO-APPLY.
      END.
      ELSE IF AVAIL ArtBas AND ArtBas.OPris = TRUE THEN DO:
          MESSAGE "Artikkel med åpen pris, kann ikke velges."
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
          RETURN NO-APPLY.
      END.
      ELSE IF AVAIL ArtBas AND ArtBas.ModellFarge = bArtBas.ArtikkelNr THEN DO:
          MESSAGE "Valgt artikkel kan ikke kobles, ingår i samme modell."
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
          RETURN NO-APPLY.
      END.
      ELSE IF AVAIL ArtBas AND ArtBas.ModellFarge <> 0 AND 
                           ArtBas.ModellFarge <> ArtBas.ArtikkelNr THEN DO:
          IF CAN-FIND(bErstattningsvare WHERE bErstattningsvare.ArtikkelNr = ArtBas.ModellFarge) THEN DO:
              MESSAGE "Valgt artikkel ingår i modell og modellens hovedvare er allerede" SKIP
                      "registrert som erstattningsvare."
                  VIEW-AS ALERT-BOX INFO BUTTONS OK.
              RETURN NO-APPLY.
          END.
          ELSE DO:
              MESSAGE "Valgt artikkel ingår i en modell. Ønsker du å koble mot modellens hovedvare?"
                  VIEW-AS ALERT-BOX INFO BUTTONS YES-NO UPDATE lBekreft AS LOGICAL.
              IF lBekreft THEN DO:
                  ASSIGN xArtikkelnr = ArtBas.ModellFarge.
                  FIND ArtBas WHERE ArtBas.ArtikkelNr = xArtikkelnr NO-LOCK.
              END.
              ELSE
                  RETURN NO-APPLY.
          END.
      END.
      IF AVAIL ArtBas THEN DO:
          FIND bErstattningsvare OF ArtBas NO-LOCK NO-ERROR.
          IF AVAIL bErstattningsvare AND iAktErstattId <> ? THEN DO:
              IF bErstattningsvare.ErstattId = iAktErstattId THEN
                  MESSAGE "Allerede registrert som erstattningsvarer."
                      VIEW-AS ALERT-BOX INFO BUTTONS OK.
              ELSE 
                  MESSAGE "Artikklerne allerede koblet til ulike varer."
                      VIEW-AS ALERT-BOX INFO BUTTONS OK.
          END.
          ELSE IF AVAIL bErstattningsvare THEN DO:
              ASSIGN iErstattId = bErstattningsvare.ErstattId.
              CREATE Erstattningsvare.
              ASSIGN Erstattningsvare.ErstattId  = bErstattningsvare.ErstattId
                     Erstattningsvare.ArtikkelNr = dArtikkelNr.
          END.
          ELSE DO:
              IF iAktErstattId = ? THEN DO:
                  FIND LAST bErstattningsvare USE-INDEX ErstattId EXCLUSIVE NO-ERROR.
                  IF LOCKED bErstattningsvare THEN DO:
                      MESSAGE "Uppdatering av annen bruker, forsök igjen."
                          VIEW-AS ALERT-BOX INFO BUTTONS OK.
                      RELEASE bErstattningsvare.
                      RETURN NO-APPLY.
                  END.
                  ELSE IF AVAIL bErstattningsvare THEN DO:
                      ASSIGN iErstattId = bErstattningsvare.ErstattId + 1.
                  END.
                  ELSE
                  ASSIGN iErstattId = 1.
                  CREATE Erstattningsvare.
                  ASSIGN Erstattningsvare.ErstattId  = iErstattId
                         Erstattningsvare.ArtikkelNr = dArtikkelNr.
              END.
              ELSE
                  ASSIGN iErstattId = iAktErstattId.
              CREATE Erstattningsvare.
              ASSIGN Erstattningsvare.ErstattId  = iErstattId
                     Erstattningsvare.ArtikkelNr = ArtBas.ArtikkelNr.
              RELEASE bErstattningsvare.
          END.
          RUN OpenQueryErstattning.
       END.
    END.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Slett
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Slett C-Win
ON CHOOSE OF BUTTON-Slett IN FRAME FRAME-Pakkelinje /* Slett */
DO:
  DEFINE VARIABLE iErstattId   LIKE Erstattningsvare.ErstattId NO-UNDO.

  MESSAGE "Vill du slette kobling til erstattningsvare? " VIEW-AS ALERT-BOX QUESTION
                BUTTONS YES-NO UPDATE wSvar AS LOGI.
  IF wSvar THEN DO:
      FIND Erstattningsvare WHERE 
          Erstattningsvare.ArtikkelNr = dArtikkelNr.
      ASSIGN iErstattId = Erstattningsvare.ErstattId.
      DELETE Erstattningsvare.
      FIND Erstattningsvare WHERE 
          Erstattningsvare.ErstattId = iErstattId NO-ERROR.
      IF AVAIL Erstattningsvare THEN
          DELETE Erstattningsvare.
      ASSIGN iErstattId = ?.
      CLOSE QUERY BROWSE-Erstattningsvare.
      APPLY "VALUE-CHANGED" TO BROWSE BROWSE-Erstattningsvare.
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
      RUN disable_UI.
      IF VALID-HANDLE(chImage-Sko) THEN
          RELEASE OBJECT chImage-Sko NO-ERROR.
      IF VALID-HANDLE(Image-Sko) THEN
          DELETE OBJECT Image-Sko NO-ERROR.
      ASSIGN Image-Sko   = ?
             chImage-Sko = ?.
  END.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  FIND ArtBas WHERE ArtBas.ArtikkelNr = dArtikkelNr NO-LOCK.
  ASSIGN lPakke = ArtBas.Pakke.
  FIND Erstattningsvare OF ArtBas NO-LOCK NO-ERROR.
  ASSIGN iErstattId = IF AVAIL Erstattningsvare THEN 
                Erstattningsvare.ErstattId ELSE ?.
  RUN enable_UI.
  {lng.i}
  SUBSCRIBE TO "ByttObjekt" IN wParentHandle NO-ERROR.
      BROWSE BROWSE-Erstattningsvare:MOVE-TO-TOP().
  IF iErstattId <> ? THEN
      {&OPEN-QUERY-BROWSE-Erstattningsvare}
  APPLY "VALUE-CHANGED" TO BROWSE BROWSE-Erstattningsvare.
  APPLY "ENTRY" TO  BROWSE BROWSE-Erstattningsvare.
  IF AVAIL Artbas THEN
      FRAME FRAME-Pakkelinje:SENSITIVE = artbas.sanertdato = ?.
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
  ASSIGN lPakke = ArtBas.Pakke.
/*   DISP {&FIELDS-IN-QUERY-FRAME-Pakkelinje} WITH FRAME FRAME-Pakkelinje. */
  
  FRAME FRAME-Pakkelinje:SENSITIVE = artbas.sanertdato = ?.
  
  IF VALID-HANDLE(hJmfRutine) THEN
      APPLY "CLOSE" TO hJmfRutine.
  ASSIGN dArtikkelNr = ipArtikkelnr.
  {sww.i}  
  FIND Erstattningsvare OF ArtBas NO-LOCK NO-ERROR.
  ASSIGN iErstattId = IF AVAIL Erstattningsvare THEN 
                Erstattningsvare.ErstattId ELSE ?.
    IF iErstattId <> ? THEN
        {&OPEN-QUERY-BROWSE-Erstattningsvare}
    ELSE
        CLOSE QUERY BROWSE-Erstattningsvare.
    APPLY "VALUE-CHANGED" TO BROWSE BROWSE-Erstattningsvare.
  {swn.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE control_load C-Win  _CONTROL-LOAD
PROCEDURE control_load :
/*------------------------------------------------------------------------------
  Purpose:     Load the OCXs    
  Parameters:  <none>
  Notes:       Here we load, initialize and make visible the 
               OCXs in the interface.                        
------------------------------------------------------------------------------*/

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN
DEFINE VARIABLE UIB_S    AS LOGICAL    NO-UNDO.
DEFINE VARIABLE OCXFile  AS CHARACTER  NO-UNDO.

OCXFile = SEARCH( "w-Erstattningsvare.wrx":U ).
IF OCXFile = ? THEN
  OCXFile = SEARCH(SUBSTRING(THIS-PROCEDURE:FILE-NAME, 1,
                     R-INDEX(THIS-PROCEDURE:FILE-NAME, ".":U), "CHARACTER":U) + "wrx":U).

IF OCXFile <> ? THEN
DO:
  ASSIGN
    chImage-Sko = Image-Sko:COM-HANDLE
    UIB_S = chImage-Sko:LoadControls( OCXFile, "Image-Sko":U)
    Image-Sko:NAME = "Image-Sko":U
  .
  RUN initialize-controls IN THIS-PROCEDURE NO-ERROR.
END.
ELSE MESSAGE "w-Erstattningsvare.wrx":U SKIP(1)
             "The binary control file could not be found. The controls cannot be loaded."
             VIEW-AS ALERT-BOX TITLE "Controls Not Loaded".

&ENDIF

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
  HIDE FRAME FRAME-Pakkelinje.
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
  RUN control_load.
  ENABLE RECT-57 BROWSE-Erstattningsvare BUTTON-Ny BUTTON-Slett B-Jamfor 
         B-ByttArtBas 
      WITH FRAME FRAME-Pakkelinje.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-Pakkelinje}
  VIEW C-Win.
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
   IF FRAME FRAME-PakkeLinje:MOVE-TO-TOP() THEN.
   APPLY "ENTRY" TO  BROWSE BROWSE-Erstattningsvare.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpenQueryErstattning C-Win 
PROCEDURE OpenQueryErstattning :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {&OPEN-QUERY-BROWSE-Erstattningsvare}
  APPLY "VALUE-CHANGED" TO BROWSE BROWSE-Erstattningsvare.
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
    APPLY "ENTRY" TO BROWSE BROWSE-Erstattningsvare.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE VisBilde C-Win 
PROCEDURE VisBilde :
/*------------------------------------------------------------------------------
  Purpose:     Visar bild på pakkemedlem
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE BUFFER bArtBas FOR ArtBas.
  FIND bArtBas WHERE bArtBas.ArtikkelNr = Erstattningsvare.ArtikkelNr NO-LOCK NO-ERROR.
  if not available bArtBas then
    return.
  {visbilde.i
   &BldOcx = chImage-Sko
   &BildNr = "bArtBas.BildNr"
  }

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

