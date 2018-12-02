&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          skotex           PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File:              d-btekst.w  
  Description:       Browser for tekster
  Input Parameters:  INPUT CHAR wiProgNavn
  Output Parameters: <none>
  Author:            SJ
  Created:           27.02.00
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEF INPUT PARAMETER wiPrgNavn AS CHAR NO-UNDO.

/* Local Variable Definitions ---                                       */
DEF VAR wFraPrgNavn AS CHAR NO-UNDO.
DEF VAR wTilPrgNavn AS CHAR NO-UNDO INIT "xxxxxxxxxxxxxxxxxxxxx".
DEF VAR wRowid AS ROWID NO-UNDO.

{runlib.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame
&Scoped-define BROWSE-NAME BROWSE-TxtNr-ASC
&Scoped-define QUERY-NAME QUERY-Lng-ASC

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Tekst

/* Definitions for BROWSE BROWSE-TxtNr-ASC                              */
&Scoped-define FIELDS-IN-QUERY-BROWSE-TxtNr-ASC Tekst.PrgNavn Tekst.TxtNr ~
Tekst.Lng Tekst.Tekst 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-TxtNr-ASC Tekst.Lng 
&Scoped-define ENABLED-TABLES-IN-QUERY-BROWSE-TxtNr-ASC Tekst
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BROWSE-TxtNr-ASC Tekst
&Scoped-define OPEN-QUERY-BROWSE-TxtNr-ASC OPEN QUERY BROWSE-TxtNr-ASC FOR EACH Tekst ~
      WHERE Tekst.PrgNavn >= wFraPrgNavn AND ~
Tekst.PrgNavn <= wTilPrgNavn NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BROWSE-TxtNr-ASC Tekst
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-TxtNr-ASC Tekst


/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define OPEN-BROWSERS-IN-QUERY-Dialog-Frame ~
    ~{&OPEN-QUERY-BROWSE-TxtNr-ASC}

/* Definitions for QUERY QUERY-Lng-ASC                                  */
&Scoped-define OPEN-QUERY-QUERY-Lng-ASC OPEN QUERY QUERY-Lng-ASC FOR EACH Tekst ~
      WHERE Tekst.PrgNavn >= wFraPrgNavn AND ~
Tekst.PrgNavn <= wTilPrgNavn NO-LOCK ~
    BY Tekst.Lng INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-QUERY-Lng-ASC Tekst
&Scoped-define FIRST-TABLE-IN-QUERY-QUERY-Lng-ASC Tekst


/* Definitions for QUERY QUERY-Lng-DESC                                 */
&Scoped-define OPEN-QUERY-QUERY-Lng-DESC OPEN QUERY QUERY-Lng-DESC FOR EACH Tekst ~
      WHERE Tekst.PrgNavn >= wFraPrgNavn AND ~
Tekst.PrgNavn <= wTilPrgNavn NO-LOCK ~
    BY Tekst.Lng DESCENDING INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-QUERY-Lng-DESC Tekst
&Scoped-define FIRST-TABLE-IN-QUERY-QUERY-Lng-DESC Tekst


/* Definitions for QUERY QUERY-PrgNavn-ASC                              */
&Scoped-define OPEN-QUERY-QUERY-PrgNavn-ASC OPEN QUERY QUERY-PrgNavn-ASC FOR EACH Tekst ~
      WHERE Tekst.PrgNavn >= wFraPrgNavn AND ~
Tekst.PrgNavn <= wTilPrgNavn NO-LOCK ~
    BY Tekst.PrgNavn INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-QUERY-PrgNavn-ASC Tekst
&Scoped-define FIRST-TABLE-IN-QUERY-QUERY-PrgNavn-ASC Tekst


/* Definitions for QUERY QUERY-PrgNavn-DESC                             */
&Scoped-define OPEN-QUERY-QUERY-PrgNavn-DESC OPEN QUERY QUERY-PrgNavn-DESC FOR EACH Tekst ~
      WHERE Tekst.PrgNavn >= wFraPrgNavn AND ~
Tekst.PrgNavn <= wTilPrgNavn NO-LOCK ~
    BY Tekst.PrgNavn DESCENDING INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-QUERY-PrgNavn-DESC Tekst
&Scoped-define FIRST-TABLE-IN-QUERY-QUERY-PrgNavn-DESC Tekst


/* Definitions for QUERY QUERY-TxtNr-DESC                               */
&Scoped-define OPEN-QUERY-QUERY-TxtNr-DESC OPEN QUERY QUERY-TxtNr-DESC FOR EACH Tekst ~
      WHERE Tekst.PrgNavn >= wFraPrgNavn AND ~
Tekst.PrgNavn <= wTilPrgNavn NO-LOCK ~
    BY Tekst.TxtNr DESCENDING INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-QUERY-TxtNr-DESC Tekst
&Scoped-define FIRST-TABLE-IN-QUERY-QUERY-TxtNr-DESC Tekst


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BROWSE-TxtNr-ASC BUTTON-Ny BUTTON-Detaljer ~
BUTTON-Lukk BUTTON-Hjelp 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-Detaljer 
     LABEL "&Endre..." 
     SIZE 15 BY 1.1.

DEFINE BUTTON BUTTON-Hjelp 
     LABEL "&Hjelp" 
     SIZE 15 BY 1.1
     BGCOLOR 8 .

DEFINE BUTTON BUTTON-Lukk AUTO-GO 
     LABEL "Lukk" 
     SIZE 15 BY 1.1
     BGCOLOR 8 .

DEFINE BUTTON BUTTON-Ny 
     LABEL "&Ny..." 
     SIZE 15 BY 1.1.

DEFINE BUTTON BUTTON-Slett 
     LABEL "&Slett..." 
     SIZE 15 BY 1.1.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-TxtNr-ASC FOR 
      Tekst SCROLLING.

DEFINE QUERY QUERY-Lng-ASC FOR 
      Tekst SCROLLING.

DEFINE QUERY QUERY-Lng-DESC FOR 
      Tekst SCROLLING.

DEFINE QUERY QUERY-PrgNavn-ASC FOR 
      Tekst SCROLLING.

DEFINE QUERY QUERY-PrgNavn-DESC FOR 
      Tekst SCROLLING.

DEFINE QUERY QUERY-TxtNr-DESC FOR 
      Tekst SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-TxtNr-ASC
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-TxtNr-ASC Dialog-Frame _STRUCTURED
  QUERY BROWSE-TxtNr-ASC NO-LOCK DISPLAY
      Tekst.PrgNavn COLUMN-LABEL "Programnavn *" FORMAT "X(25)":U
            WIDTH 25.2
      Tekst.TxtNr COLUMN-LABEL "Tekstnr *" FORMAT "zzzzzzz9":U
            WIDTH 9.2
      Tekst.Lng COLUMN-LABEL "Språk *" FORMAT "x(3)":U WIDTH 8.2
      Tekst.Tekst FORMAT "x(60)":U WIDTH 49
  ENABLE
      Tekst.Lng
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 99 BY 17.86 EXPANDABLE.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     BROWSE-TxtNr-ASC AT ROW 2.67 COL 3
     BUTTON-Ny AT ROW 2.67 COL 104
     BUTTON-Detaljer AT ROW 4.1 COL 104
     BUTTON-Slett AT ROW 5.52 COL 104
     BUTTON-Lukk AT ROW 17.91 COL 104
     BUTTON-Hjelp AT ROW 19.33 COL 104
     SPACE(0.39) SKIP(0.27)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Tekster"
         DEFAULT-BUTTON BUTTON-Lukk.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
                                                                        */
/* BROWSE-TAB BROWSE-TxtNr-ASC QUERY-TxtNr-DESC Dialog-Frame */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON BUTTON-Slett IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-TxtNr-ASC
/* Query rebuild information for BROWSE BROWSE-TxtNr-ASC
     _TblList          = "SkoTex.Tekst"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Where[1]         = "Tekst.PrgNavn >= wFraPrgNavn AND
Tekst.PrgNavn <= wTilPrgNavn"
     _FldNameList[1]   > SkoTex.Tekst.PrgNavn
"Tekst.PrgNavn" "Programnavn *" ? "character" ? ? ? ? ? ? no ? no no "25.2" yes no no "U" "" ""
     _FldNameList[2]   > SkoTex.Tekst.TxtNr
"Tekst.TxtNr" "Tekstnr *" ? "integer" ? ? ? ? ? ? no ? no no "9.2" yes no no "U" "" ""
     _FldNameList[3]   > SkoTex.Tekst.Lng
"Tekst.Lng" "Språk *" ? "character" ? ? ? ? ? ? yes ? no no "8.2" yes no no "U" "" ""
     _FldNameList[4]   > SkoTex.Tekst.Tekst
"Tekst.Tekst" ? ? "character" ? ? ? ? ? ? no ? no no "49" yes no no "U" "" ""
     _Query            is OPENED
*/  /* BROWSE BROWSE-TxtNr-ASC */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK QUERY QUERY-Lng-ASC
/* Query rebuild information for QUERY QUERY-Lng-ASC
     _TblList          = "SkoTex.Tekst"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _OrdList          = "SkoTex.Tekst.Lng|yes"
     _Where[1]         = "Tekst.PrgNavn >= wFraPrgNavn AND
Tekst.PrgNavn <= wTilPrgNavn"
     _Design-Parent    is DIALOG-BOX Dialog-Frame @ ( 1.48 , 40 )
*/  /* QUERY QUERY-Lng-ASC */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK QUERY QUERY-Lng-DESC
/* Query rebuild information for QUERY QUERY-Lng-DESC
     _TblList          = "SkoTex.Tekst"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _OrdList          = "SkoTex.Tekst.Lng|no"
     _Where[1]         = "Tekst.PrgNavn >= wFraPrgNavn AND
Tekst.PrgNavn <= wTilPrgNavn"
     _Design-Parent    is DIALOG-BOX Dialog-Frame @ ( 1.48 , 45 )
*/  /* QUERY QUERY-Lng-DESC */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK QUERY QUERY-PrgNavn-ASC
/* Query rebuild information for QUERY QUERY-PrgNavn-ASC
     _TblList          = "SkoTex.Tekst"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _OrdList          = "SkoTex.Tekst.PrgNavn|yes"
     _Where[1]         = "Tekst.PrgNavn >= wFraPrgNavn AND
Tekst.PrgNavn <= wTilPrgNavn"
     _Design-Parent    is DIALOG-BOX Dialog-Frame @ ( 1.48 , 3 )
*/  /* QUERY QUERY-PrgNavn-ASC */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK QUERY QUERY-PrgNavn-DESC
/* Query rebuild information for QUERY QUERY-PrgNavn-DESC
     _TblList          = "SkoTex.Tekst"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _OrdList          = "SkoTex.Tekst.PrgNavn|no"
     _Where[1]         = "Tekst.PrgNavn >= wFraPrgNavn AND
Tekst.PrgNavn <= wTilPrgNavn"
     _Design-Parent    is DIALOG-BOX Dialog-Frame @ ( 1.48 , 8 )
*/  /* QUERY QUERY-PrgNavn-DESC */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK QUERY QUERY-TxtNr-DESC
/* Query rebuild information for QUERY QUERY-TxtNr-DESC
     _TblList          = "SkoTex.Tekst"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _OrdList          = "SkoTex.Tekst.TxtNr|no"
     _Where[1]         = "Tekst.PrgNavn >= wFraPrgNavn AND
Tekst.PrgNavn <= wTilPrgNavn"
     _Design-Parent    is DIALOG-BOX Dialog-Frame @ ( 1.48 , 30 )
*/  /* QUERY QUERY-TxtNr-DESC */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Tekster */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-TxtNr-ASC
&Scoped-define SELF-NAME BROWSE-TxtNr-ASC
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-TxtNr-ASC Dialog-Frame
ON DEFAULT-ACTION OF BROWSE-TxtNr-ASC IN FRAME Dialog-Frame
DO:
   APPLY "CHOOSE" TO BUTTON-Detaljer.
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Detaljer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Detaljer Dialog-Frame
ON CHOOSE OF BUTTON-Detaljer IN FRAME Dialog-Frame /* Endre... */
DO:
   ASSIGN wRowid = ROWID(Tekst).
   RUN d-vtekst.w (INPUT-OUTPUT wRowid,"").
   IF RETURN-VALUE <> "<Avbryt>" THEN 
      RUN RefreshBrowser (wRowid).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Hjelp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Hjelp Dialog-Frame
ON CHOOSE OF BUTTON-Hjelp IN FRAME Dialog-Frame /* Hjelp */
OR HELP OF FRAME {&FRAME-NAME}
DO: 
   {diahelp.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Ny
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Ny Dialog-Frame
ON CHOOSE OF BUTTON-Ny IN FRAME Dialog-Frame /* Ny... */
DO:
  ASSIGN wRowid = ?.
  /*
  RUN d-vtekst.w (INPUT-OUTPUT wRowid,
                  IF AVAIL Tekst THEN Tekst.PrgNavn ELSE "").
  */
  RUN d-vtekst.w (INPUT-OUTPUT wRowid,
                  wiPrgNavn).
  IF RETURN-VALUE <> "<Avbryt>" THEN
     RUN RefreshBrowser (wRowid).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Slett
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Slett Dialog-Frame
ON CHOOSE OF BUTTON-Slett IN FRAME Dialog-Frame /* Slett... */
DO:
  DEF VAR wSvar AS LOGI NO-UNDO.
  DEF BUFFER bTekst FOR Tekst.
  DO TRANSACTION:
     IF NOT AVAIL Tekst THEN RETURN NO-APPLY.
     MESSAGE "Vil du virkelig slette posten?"
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Slett" UPDATE wSvar.
     IF NOT wSvar = YES THEN RETURN NO-APPLY.
     FIND bTekst WHERE ROWID(bTekst) = ROWID(Tekst) EXCLUSIVE NO-WAIT NO-ERROR.
     IF NOT AVAIL bTekst THEN DO:
        IF LOCKED bTekst THEN DO:
           MESSAGE "Dataene er sperret av en annen bruker." SKIP
                   "Kan ikke slette..."
              VIEW-AS ALERT-BOX ERROR TITLE "Feil".
           RETURN NO-APPLY.
        END.
        ELSE DO:
           MESSAGE "Finner ikke dataene. De kan ha blitt slettet av en annen bruker." SKIP
                   "Kan ikke slette..."
              VIEW-AS ALERT-BOX ERROR TITLE "Feil".
           RETURN NO-APPLY.
        END.
     END.

     DELETE bTekst.
     BROWSE BROWSE-TxtNr-ASC:DELETE-CURRENT-ROW().
     RUN ButtonStatus.
   END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
   
  {lng.i} 
  
  IF wiPrgNavn <> "*" THEN
     ASSIGN wFraPrgNavn = wiPrgNavn
            wTilPrgNavn = wiPrgNavn
            FRAME {&FRAME-NAME}:TITLE = "Tekster for programmet " + wiPrgNavn.
            
  RUN enable_UI.
     
  /*------ Søk og sortering for BROWSE-TekstNr-ASC ------*/
  {brsort.i
     &Browse   = BROWSE-TxtNr-ASC
     &Tabell   = Tekst
     &Meny     = "&Tekstnr stigende,T&ekstnr synkende,&Språk stigende,Språ&k synkende"
     &Mb1      = BUTTON-Ny
     &Mb2      = BUTTON-Detaljer
     &Mb3      = BUTTON-Slett
     
     &KolonneA = TxtNr
     &QueryA1  = BROWSE-TxtNr-ASC
     &SokA1    = "WHERE BUFFTekst.PrgNavn >= wFraPrgNavn AND BUFFTekst.PrgNavn <= wTilPrgNavn AND BUFFTekst.TxtNr >= INT(wSok)"
     &QueryA2  = QUERY-TxtNr-DESC
     &SokA2    = "WHERE BUFFTekst.PrgNavn >= wFraPrgNavn AND BUFFTekst.PrgNavn <= wTilPrgNavn AND BUFFTekst.TxtNr >= INT(wSok)"

     &KolonneB = Lng
     &QueryB1  = QUERY-Lng-ASC
     &SokB1    = "WHERE BUFFTekst.PrgNavn >= wFraPrgNavn AND BUFFTekst.PrgNavn <= wTilPrgNavn AND BUFFTekst.Lng >= wSok"
     &QueryB2  = QUERY-Lng-DESC
     &SokB2    = "WHERE BUFFTekst.PrgNavn >= wFraPrgNavn AND BUFFTekst.PrgNavn <= wTilPrgNavn AND BUFFTekst.Lng >= wSok"

     &KolonneC = PrgNavn
     &QueryC1  = QUERY-PrgNavn-ASC
     &SokC1    = "WHERE BUFFTekst.PrgNavn >= wFraPrgNavn AND BUFFTekst.PrgNavn <= wTilPrgNavn AND BUFFTekst.PrgNavn >= wSok"
     &QueryC2  = QUERY-PrgNavn-DESC
     &SokC2    = "WHERE BUFFTekst.PrgNavn >= wFraPrgNavn AND BUFFTekst.PrgNavn <= wTilPrgNavn AND BUFFTekst.PrgNavn >= wSok"
  }
  RUN ButtonStatus.
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ButtonStatus Dialog-Frame 
PROCEDURE ButtonStatus :
/*------------------------------------------------------------------------------
  Purpose:     Enabler/disabler knapper     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   ASSIGN BUTTON-Detaljer:SENSITIVE IN FRAME {&FRAME-NAME} = AVAIL Tekst
          BUTTON-Slett:SENSITIVE    IN FRAME {&FRAME-NAME} = AVAIL Tekst.
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
  ENABLE BROWSE-TxtNr-ASC BUTTON-Ny BUTTON-Detaljer BUTTON-Lukk BUTTON-Hjelp 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpenQuery Dialog-Frame 
PROCEDURE OpenQuery :
/*------------------------------------------------------------------------------
  Purpose:     Åpner en navngitt query
  Parameters:  se under
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER wiQueryName AS CHAR NO-UNDO.
  CASE wiQueryName:
     WHEN "BROWSE-TxtNr-ASC"   THEN {&OPEN-QUERY-BROWSE-TxtNr-ASC}
     WHEN "QUERY-TxtNr-DESC"   THEN {&OPEN-QUERY-QUERY-TxtNr-DESC}
     WHEN "QUERY-Lng-ASC"      THEN {&OPEN-QUERY-QUERY-Lng-ASC}
     WHEN "QUERY-Lng-DESC"     THEN {&OPEN-QUERY-QUERY-Lng-DESC}
     WHEN "QUERY-PrgNavn-ASC"  THEN {&OPEN-QUERY-QUERY-PrgNavn-ASC}
     WHEN "QUERY-PrgNavn-DESC" THEN {&OPEN-QUERY-QUERY-PrgNavn-DESC}
  END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RefreshBrowser Dialog-Frame 
PROCEDURE RefreshBrowser :
/*------------------------------------------------------------------------------
  Purpose:     Oppdaterer browser
  Parameters:  se under
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER wiRowid AS ROWID NO-UNDO.
 
  DEF VAR wQueryH AS HANDLE NO-UNDO.
  ASSIGN wQueryH = BROWSE BROWSE-TxtNr-ASC:QUERY.
  IF wiRowid <> ? THEN DO:
     BROWSE BROWSE-TxtNr-ASC:SET-REPOSITIONED-ROW(BROWSE BROWSE-TxtNr-ASC:NUM-ITERATIONS,"CONDITIONAL").
     IF wQueryH:NAME <> "BROWSE-TxtNr-ASC" THEN
        RUN OpenQuery (wQueryH:NAME).
     wQueryH:REPOSITION-TO-ROWID(wiRowid) NO-ERROR.  
  END.  
  ELSE DO:
     IF NOT AVAIL Tekst THEN DO:
        RUN OpenQuery (wQueryH:NAME).
        IF NOT AVAIL Tekst THEN RETURN NO-APPLY.
     END.
     BROWSE BROWSE-TxtNr-ASC:REFRESH() NO-ERROR.
  END.   
  RUN ButtonStatus.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

