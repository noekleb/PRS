&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          skotex           PROGRESS
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
DEFINE INPUT PARAMETER dArtikkelNr LIKE ArtBas.ArtikkelNr  NO-UNDO.
DEFINE INPUT PARAMETER cBildeFilNavn AS CHARACTER     NO-UNDO.
/* Local Variable Definitions ---                                       */
DEF VAR cStorl AS CHAR FORMAT "x(6)" NO-UNDO.

DEFINE BUFFER bArtbas FOR ArtBas.
DEFINE VARIABLE cEmptyCol AS CHARACTER LABEL "" NO-UNDO.
DEFINE VARIABLE wOk AS LOGICAL    NO-UNDO.
DEFINE VARIABLE cRetur_Verdi AS CHARACTER INIT "AVBRYT" NO-UNDO.
{runlib.i} /* Starter procedurebibloteket. */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame
&Scoped-define BROWSE-NAME BROWSE-Pakke

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ArtBas PakkeLinje

/* Definitions for BROWSE BROWSE-Pakke                                  */
&Scoped-define FIELDS-IN-QUERY-BROWSE-Pakke ArtBas.ArtikkelNr ArtBas.Beskr ~
"" @ cEmptyCol 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-Pakke 
&Scoped-define QUERY-STRING-BROWSE-Pakke FOR EACH ArtBas ~
      WHERE ArtBas.Pakke = TRUE NO-LOCK ~
    BY ArtBas.Pakkenr INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BROWSE-Pakke OPEN QUERY BROWSE-Pakke FOR EACH ArtBas ~
      WHERE ArtBas.Pakke = TRUE NO-LOCK ~
    BY ArtBas.Pakkenr INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BROWSE-Pakke ArtBas
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-Pakke ArtBas


/* Definitions for BROWSE BROWSE-PakkeLinje                             */
&Scoped-define FIELDS-IN-QUERY-BROWSE-PakkeLinje PakkeLinje.PkArtikkelNr ~
cStorl PakkeLinje.StrKode PakkeLinje.Antall "" @ cEmptyCol 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-PakkeLinje 
&Scoped-define QUERY-STRING-BROWSE-PakkeLinje FOR EACH PakkeLinje ~
      WHERE PakkeLinje.ArtikkelNr = ArtBas.ArtikkelNr NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BROWSE-PakkeLinje OPEN QUERY BROWSE-PakkeLinje FOR EACH PakkeLinje ~
      WHERE PakkeLinje.ArtikkelNr = ArtBas.ArtikkelNr NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BROWSE-PakkeLinje PakkeLinje
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-PakkeLinje PakkeLinje


/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define OPEN-BROWSERS-IN-QUERY-Dialog-Frame ~
    ~{&OPEN-QUERY-BROWSE-Pakke}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-55 RECT-56 RECT-57 Btn_OK B-LeggTil ~
CB-StrKode B-Oppdat FI-Antall Btn_Help BROWSE-Pakke BROWSE-PakkeLinje ~
FI-PakkeTxt FI-PakkeLinjeTxt 
&Scoped-Define DISPLAYED-OBJECTS FI-ArtikkelNr CB-StrKode FI-Vg FI-LopNr ~
FI-Besk FI-Antall FI-BongTekst FI-PakkeTxt FI-PakkeLinjeTxt 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE Image-Sko AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chImage-Sko AS COMPONENT-HANDLE NO-UNDO.
DEFINE VARIABLE Image-Sko-2 AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chImage-Sko-2 AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-LeggTil 
     LABEL "Legg til" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON B-Oppdat AUTO-END-KEY 
     LABEL "Oppdater" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_Help DEFAULT 
     LABEL "&Help" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "Avbryt" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE CB-StrKode AS CHARACTER FORMAT "X(256)":U 
     LABEL "Strekkode" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     DROP-DOWN-LIST
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Antall AS DECIMAL FORMAT "->>,>>9" INITIAL 0 
     LABEL "Antall" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-ArtikkelNr AS DECIMAL FORMAT "zzzzzzzzzzzz9" INITIAL 0 
     LABEL "Artikkelnummer" 
     VIEW-AS FILL-IN 
     SIZE 20.2 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Besk AS CHARACTER FORMAT "X(30)" 
     LABEL "Beskrivelse" 
     VIEW-AS FILL-IN 
     SIZE 34 BY 1.

DEFINE VARIABLE FI-BongTekst AS CHARACTER FORMAT "X(30)" 
     LABEL "Bongtekst" 
     VIEW-AS FILL-IN 
     SIZE 34 BY 1.

DEFINE VARIABLE FI-LopNr AS INTEGER FORMAT "zzzzz9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 12.6 BY 1.

DEFINE VARIABLE FI-PakkeLinjeTxt AS CHARACTER FORMAT "X(256)":U INITIAL "Pakkelinje" 
      VIEW-AS TEXT 
     SIZE 19 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FI-PakkeTxt AS CHARACTER FORMAT "X(256)":U INITIAL "Pakke" 
      VIEW-AS TEXT 
     SIZE 19 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FI-Vg AS INTEGER FORMAT "zzz9" INITIAL 0 
     LABEL "Vg/Løpnr" 
     VIEW-AS FILL-IN 
     SIZE 7.6 BY 1.

DEFINE RECTANGLE RECT-55
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 77 BY 5.24.

DEFINE RECTANGLE RECT-56
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 37 BY 5.24.

DEFINE RECTANGLE RECT-57
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 26 BY 5.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-Pakke FOR 
      ArtBas SCROLLING.

DEFINE QUERY BROWSE-PakkeLinje FOR 
      PakkeLinje SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-Pakke
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-Pakke Dialog-Frame _STRUCTURED
  QUERY BROWSE-Pakke NO-LOCK DISPLAY
      ArtBas.ArtikkelNr FORMAT "zzzzzzzzzzzz9":U
      ArtBas.Beskr FORMAT "x(20)":U
      "" @ cEmptyCol WIDTH 9.6
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 50 BY 13.86 EXPANDABLE.

DEFINE BROWSE BROWSE-PakkeLinje
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-PakkeLinje Dialog-Frame _STRUCTURED
  QUERY BROWSE-PakkeLinje NO-LOCK DISPLAY
      PakkeLinje.PkArtikkelNr FORMAT "zzzzzzzzzzzz9":U
      cStorl COLUMN-LABEL "Størrelse"
      PakkeLinje.StrKode FORMAT ">>9":U
      PakkeLinje.Antall FORMAT "->>,>>9":U
      "" @ cEmptyCol
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 55.6 BY 13.86 EXPANDABLE.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     Btn_OK AT ROW 1.24 COL 121
     FI-ArtikkelNr AT ROW 1.81 COL 41.6 COLON-ALIGNED
     B-LeggTil AT ROW 2.52 COL 121
     CB-StrKode AT ROW 2.71 COL 90.4 COLON-ALIGNED
     FI-Vg AT ROW 2.86 COL 41.6 COLON-ALIGNED HELP
          "'varegruppenummer"
     FI-LopNr AT ROW 2.86 COL 49.4 COLON-ALIGNED HELP
          "Løpenummer innenfor varegruppen" NO-LABEL
     B-Oppdat AT ROW 3.76 COL 121
     FI-Besk AT ROW 3.91 COL 41.6 COLON-ALIGNED HELP
          "Bongtekst - Tekst som vises på lvittering"
     FI-Antall AT ROW 3.91 COL 90.4 COLON-ALIGNED HELP
          "Antall enheter av artikkelen som inngår i pakken."
     FI-BongTekst AT ROW 4.95 COL 41.6 COLON-ALIGNED HELP
          "Bongtekst - Tekst som vises på lvittering"
     Btn_Help AT ROW 5.29 COL 121
     BROWSE-Pakke AT ROW 7.71 COL 2
     BROWSE-PakkeLinje AT ROW 7.71 COL 53.4
     FI-PakkeTxt AT ROW 6.81 COL 1 COLON-ALIGNED NO-LABEL
     FI-PakkeLinjeTxt AT ROW 6.81 COL 52.4 COLON-ALIGNED NO-LABEL
     RECT-55 AT ROW 1.24 COL 2
     RECT-56 AT ROW 1.24 COL 80
     RECT-57 AT ROW 7.67 COL 110.6
     SPACE(0.19) SKIP(9.08)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Legg til i pakke"
         DEFAULT-BUTTON Btn_OK.


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
                                                                        */
/* BROWSE-TAB BROWSE-Pakke Btn_Help Dialog-Frame */
/* BROWSE-TAB BROWSE-PakkeLinje BROWSE-Pakke Dialog-Frame */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN FI-ArtikkelNr IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Besk IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-BongTekst IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-LopNr IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Vg IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-Pakke
/* Query rebuild information for BROWSE BROWSE-Pakke
     _TblList          = "skotex.ArtBas"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _OrdList          = "skotex.ArtBas.Pakkenr|yes"
     _Where[1]         = "skotex.ArtBas.Pakke = TRUE"
     _FldNameList[1]   = skotex.ArtBas.ArtikkelNr
     _FldNameList[2]   = skotex.ArtBas.Beskr
     _FldNameList[3]   > "_<CALC>"
""""" @ cEmptyCol" ? ? ? ? ? ? ? ? ? no ? no no "9.6" yes no no "U" "" ""
     _Query            is OPENED
*/  /* BROWSE BROWSE-Pakke */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-PakkeLinje
/* Query rebuild information for BROWSE BROWSE-PakkeLinje
     _TblList          = "skotex.PakkeLinje"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Where[1]         = "skotex.PakkeLinje.ArtikkelNr = ArtBas.ArtikkelNr"
     _FldNameList[1]   = skotex.PakkeLinje.PkArtikkelNr
     _FldNameList[2]   > "_<CALC>"
"cStorl" "Størrelse" ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[3]   = skotex.PakkeLinje.StrKode
     _FldNameList[4]   = skotex.PakkeLinje.Antall
     _FldNameList[5]   > "_<CALC>"
""""" @ cEmptyCol" ? ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BROWSE-PakkeLinje */
&ANALYZE-RESUME

 


/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME Image-Sko ASSIGN
       FRAME           = FRAME Dialog-Frame:HANDLE
       ROW             = 1.86
       COLUMN          = 3.2
       HEIGHT          = 4.19
       WIDTH           = 23.4
       HIDDEN          = no
       SENSITIVE       = yes.

CREATE CONTROL-FRAME Image-Sko-2 ASSIGN
       FRAME           = FRAME Dialog-Frame:HANDLE
       ROW             = 8.05
       COLUMN          = 111.8
       HEIGHT          = 4.19
       WIDTH           = 23.4
       HIDDEN          = no
       SENSITIVE       = yes.
      Image-Sko:NAME = "Image-Sko":U .
/* Image-Sko OCXINFO:CREATE-CONTROL from: {9A93B740-C96B-11D0-8883-444553540000} type: Picbuf */
      Image-Sko-2:NAME = "Image-Sko-2":U .
/* Image-Sko-2 OCXINFO:CREATE-CONTROL from: {9A93B740-C96B-11D0-8883-444553540000} type: Picbuf */
      Image-Sko:MOVE-AFTER(FI-ArtikkelNr:HANDLE IN FRAME Dialog-Frame).
      Image-Sko-2:MOVE-AFTER(BROWSE-PakkeLinje:HANDLE IN FRAME Dialog-Frame).

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Legg til i pakke */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-LeggTil
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-LeggTil Dialog-Frame
ON CHOOSE OF B-LeggTil IN FRAME Dialog-Frame /* Legg til */
DO:
    IF ArtBas.iKasse THEN DO:
        MESSAGE "Vare i kasse er satt på pakken." SKIP
                "For endring av pakkens innhold, ta bort markering."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN NO-APPLY.
    END.
    IF INPUT FI-Antall = 0 OR INPUT FI-Antall = ? THEN
        MESSAGE "Antall = 0"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
    ELSE DO:
        MESSAGE "Lagre ny pakkelinje:" skip
                "Artikkelnr: " dArtikkelNr " (Vg: " FI-Vg " Løpnr: " FI-Lopnr ")" skip
                "Till pakke: " ArtBas.ArtikkelNr
            VIEW-AS ALERT-BOX INFO BUTTONS YES-NO UPDATE lChoice AS LOGICAL.
        IF lChoice THEN
            RUN LagrePakkeLinje.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Oppdat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Oppdat Dialog-Frame
ON CHOOSE OF B-Oppdat IN FRAME Dialog-Frame /* Oppdater */
DO:
  IF ArtBas.iKasse THEN DO:
      MESSAGE "Vare i kasse er satt på pakken." SKIP
              "For endring av pakkens innhold, ta bort markering."
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN NO-APPLY.
  END.
  IF INPUT FI-Antall = 0 OR INPUT FI-Antall = ? THEN
      MESSAGE "Antall = 0"
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
  ELSE DO:
      FIND CURRENT PakkeLinje EXCLUSIVE.
      ASSIGN PakkeLinje.StrKode   = int(CB-StrKode:SCREEN-VALUE)
             PakkeLinje.Antall = INPUT FI-Antall.
      FIND CURRENT PakkeLinje NO-LOCK.
      BROWSE BROWSE-PakkeLinje:REFRESH().
      APPLY "VALUE-CHANGED" TO BROWSE BROWSE-PakkeLinje.
  END.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-Pakke
&Scoped-define SELF-NAME BROWSE-Pakke
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-Pakke Dialog-Frame
ON VALUE-CHANGED OF BROWSE-Pakke IN FRAME Dialog-Frame
DO:
  {&OPEN-QUERY-BROWSE-PakkeLinje}
  APPLY "VALUE-CHANGED" TO BROWSE-PakkeLinje.
  ASSIGN B-LeggTil:SENSITIVE = NOT CAN-FIND(FIRST PakkeLinje OF ArtBas WHERE
                                          PakkeLinje.PkArtikkelNr = dArtikkelNr).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-PakkeLinje
&Scoped-define SELF-NAME BROWSE-PakkeLinje
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-PakkeLinje Dialog-Frame
ON VALUE-CHANGED OF BROWSE-PakkeLinje IN FRAME Dialog-Frame
DO:
  IF BROWSE BROWSE-PakkeLinje:FOCUSED-ROW <> ? THEN DO:
      RUN VisBilde(1).
      ASSIGN B-Oppdat:SENSITIVE   = PakkeLinje.PkArtikkel = dArtikkelNr
             CB-StrKode:SCREEN-VALUE = IF B-Oppdat:SENSITIVE THEN string(PakkeLinje.StrKode)
                                      ELSE CB-StrKode:SCREEN-VALUE
             FI-Antall:SCREEN-VALUE = IF B-Oppdat:SENSITIVE THEN STRING(PakkeLinje.Antall)
                                      ELSE "0".
/*              CB-Kode:SENSITIVE   = B-Oppdat:SENSITIVE   */
/*              FI-Antall:SENSITIVE =  B-Oppdat:SENSITIVE. */
  END.
  ELSE
      chImage-Sko-2:CLEAR(2).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Help
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Help Dialog-Frame
ON CHOOSE OF Btn_Help IN FRAME Dialog-Frame /* Help */
OR HELP OF FRAME {&FRAME-NAME}
DO: /* Call Help Function (or a simple message). */
  {diahelp.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Image-Sko
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Image-Sko Dialog-Frame OCX.DblClick
PROCEDURE Image-Sko.Picbuf.DblClick .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  None required for OCX.
  Notes:       
------------------------------------------------------------------------------*/

  if available bArtBAs then
    do:
      find BildeRegister of bArtBas no-error.
      if available BildeRegister then
        run d-visbil.w (input recid(BildeRegister)).
    end.
  return no-apply.    


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-Pakke
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.

FIND bArtBas WHERE bArtBas.ArtikkelNr = dArtikkelNr NO-LOCK NO-ERROR.
IF bArtBas.Pakke = TRUE THEN DO:
    MESSAGE "Pakkeartikkel kan ikke brukes"
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN "AVBRYT".
END.
ELSE IF NOT CAN-FIND(FIRST bArtBas WHERE bArtBas.Pakke = TRUE) THEN DO:
    MESSAGE "Det finnes ingen pakke. Kobling till pakke ikke mulig."
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN "AVBRYT".
END.
ELSE IF NOT CAN-FIND(FIRST StrekKode OF bArtBas) THEN DO:
    MESSAGE "Artikkel mangler strekkode"
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN "AVBRYT".
END.
/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
    {lng.i}
  RUN VisArtInfo.
  RUN enable_UI.
  RUN InitCB.
  IF SEARCH(cBildeFilNavn) <> ? THEN DO:
      ASSIGN chImage-Sko:Picbuf:FILENAME    = cBildeFilNavn.
      chImage-Sko:Picbuf:LOAD() NO-ERROR.
  END.
  APPLY "VALUE-CHANGED" TO BROWSE BROWSE-Pakke.
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.
IF VALID-HANDLE(chImage-Sko) THEN
    RELEASE OBJECT chImage-Sko NO-ERROR.
IF VALID-HANDLE(Image-Sko) THEN
    DELETE OBJECT Image-Sko NO-ERROR.
IF VALID-HANDLE(chImage-Sko-2) THEN
    RELEASE OBJECT chImage-Sko-2 NO-ERROR.
IF VALID-HANDLE(Image-Sko-2) THEN
    DELETE OBJECT Image-Sko-2 NO-ERROR.
ASSIGN chImage-Sko   = ?
       Image-Sko     = ?
       chImage-Sko-2 = ?
       Image-Sko-2   = ?.

RETURN cRetur_Verdi.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE control_load Dialog-Frame  _CONTROL-LOAD
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

OCXFile = SEARCH( "d-tilpakke.wrx":U ).
IF OCXFile = ? THEN
  OCXFile = SEARCH(SUBSTRING(THIS-PROCEDURE:FILE-NAME, 1,
                     R-INDEX(THIS-PROCEDURE:FILE-NAME, ".":U), "CHARACTER":U) + "wrx":U).

IF OCXFile <> ? THEN
DO:
  ASSIGN
    chImage-Sko = Image-Sko:COM-HANDLE
    UIB_S = chImage-Sko:LoadControls( OCXFile, "Image-Sko":U)
    chImage-Sko-2 = Image-Sko-2:COM-HANDLE
    UIB_S = chImage-Sko-2:LoadControls( OCXFile, "Image-Sko-2":U)
  .
  RUN initialize-controls IN THIS-PROCEDURE NO-ERROR.
END.
ELSE MESSAGE "d-tilpakke.wrx":U SKIP(1)
             "The binary control file could not be found. The controls cannot be loaded."
             VIEW-AS ALERT-BOX TITLE "Controls Not Loaded".

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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
  RUN control_load.
  DISPLAY FI-ArtikkelNr CB-StrKode FI-Vg FI-LopNr FI-Besk FI-Antall FI-BongTekst 
          FI-PakkeTxt FI-PakkeLinjeTxt 
      WITH FRAME Dialog-Frame.
  ENABLE RECT-55 RECT-56 RECT-57 Btn_OK B-LeggTil CB-StrKode B-Oppdat FI-Antall 
         Btn_Help BROWSE-Pakke BROWSE-PakkeLinje FI-PakkeTxt FI-PakkeLinjeTxt 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitCB Dialog-Frame 
PROCEDURE InitCB :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cHovedStrekKode AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cNoStrStrekKode AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cListItems      AS CHARACTER  NO-UNDO.

    FOR EACH StrekKode OF bArtBas NO-LOCK.
        FIND StrKonv OF Strekkode NO-LOCK NO-ERROR.
        IF NOT AVAIL StrKonv THEN
            NEXT.
        ASSIGN cListItems    = cListItems + (IF cListItems = "" THEN "" ELSE ",") + (StrKonv.Storl + "," + string(StrekKode.StrKode))
            cHovedStrekKode = IF StrekKode.HovedNr = TRUE THEN StrekKode.Kode ELSE cHovedStrekKode
            cNoStrStrekKode = IF cNoStrStrekKode = "" AND LENGTH(StrekKode.Kode) = 13
                                                      AND SUBSTR(StrekKode.Kode,10,3) = "000" THEN
                                             StrekKode.Kode ELSE cNoStrStrekKode.
    END.
    DO WITH FRAME {&FRAME-NAME}:
      ASSIGN 
          CB-StrKode:LIST-ITEMS   = cListItems
          CB-StrKode:SCREEN-VALUE = ENTRY(2,CB-StrKode:LIST-ITEMS)
          .
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initialize-controls Dialog-Frame 
PROCEDURE initialize-controls :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    ASSIGN 
/*            chImage-Sko   = chImage-Sko:Picbuf   */
/*            chImage-Sko-2 = chImage-Sko-2:Picbuf */
           chImage-Sko-2:Picbuf:AutoScale = TRUE
           chImage-Sko:PicBuf:AutoScale   = TRUE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LagrePakkeLinje Dialog-Frame 
PROCEDURE LagrePakkeLinje :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE rPakkeLinje AS ROWID      NO-UNDO.
    DEF BUFFER bPakkeLinje FOR PakkeLinje.  

    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN FI-Antall.
        CREATE bPakkeLinje.
        ASSIGN bPakkeLinje.ArtikkelNr   = ArtBas.ArtikkelNr 
               bPakkeLinje.PakkeNr      = ArtBas.PakkeNr 
               bPakkeLinje.PkArtikkelNr = dArtikkelNr 
               bPakkeLinje.StrKode      = int(CB-StrKode:SCREEN-VALUE)
               bPakkeLinje.Antall       = INPUT FI-Antall
               rPakkeLinje              = ROWID(bPakkeLinje).
        RELEASE bPakkeLinje.
        {&OPEN-QUERY-BROWSE-PakkeLinje}
        REPOSITION BROWSE-PakkeLinje TO ROWID rPakkeLinje.
        APPLY "VALUE-CHANGED" TO BROWSE BROWSE-Pakke.
        APPLY "VALUE-CHANGED" TO BROWSE BROWSE-PakkeLinje.
        ASSIGN cRetur_Verdi = "OK".
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE VisArtInfo Dialog-Frame 
PROCEDURE VisArtInfo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    ASSIGN FI-ArtikkelNr = bArtBas.ArtikkelNr
           FI-Vg         = bArtBas.Vg
           FI-Lopnr      = bArtBas.Lopnr
           FI-Besk       = bArtBas.Beskr
           FI-Bongtekst  = bArtBas.Bongtekst.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE VisBilde Dialog-Frame 
PROCEDURE VisBilde :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE BUFFER bArtBas FOR ArtBas.
  FIND bArtBas WHERE bArtBas.ArtikkelNr = PakkeLinje.PkArtikkelNr NO-LOCK NO-ERROR.
  if not available bArtBas then
    return.
  {visbilde.i
   &BldOcx = chImage-Sko-2
   &BildNr = "bArtBas.BildNr"
  }
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

