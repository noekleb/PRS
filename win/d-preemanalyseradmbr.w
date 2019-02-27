&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          skotex           PROGRESS
          data             PROGRESS
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
DEFINE INPUT         PARAMETER cType      AS CHARACTER   NO-UNDO.
DEFINE INPUT         PARAMETER iAnalyseId AS INTEGER     NO-UNDO.
DEFINE INPUT         PARAMETER iRadnr AS INTEGER     NO-UNDO.
DEFINE INPUT         PARAMETER iTyp       AS INTEGER     NO-UNDO.
DEFINE INPUT         PARAMETER cTyptxt    AS CHARACTER   NO-UNDO.
DEFINE INPUT         PARAMETER dElement   AS DECIMAL     NO-UNDO.
DEFINE INPUT         PARAMETER cElementTxt AS CHARACTER   NO-UNDO.
DEFINE INPUT-OUTPUT  PARAMETER rAnalysembr   AS ROWID       NO-UNDO.
/* Local Variable Definitions ---                                       */

DEFINE VARIABLE cRetValue AS CHARACTER   NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ArtBas preemanalysembr

/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define QUERY-STRING-Dialog-Frame FOR EACH ArtBas SHARE-LOCK, ~
      EACH preemanalysembr WHERE TRUE /* Join to ArtBas incomplete */ SHARE-LOCK
&Scoped-define OPEN-QUERY-Dialog-Frame OPEN QUERY Dialog-Frame FOR EACH ArtBas SHARE-LOCK, ~
      EACH preemanalysembr WHERE TRUE /* Join to ArtBas incomplete */ SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-Dialog-Frame ArtBas preemanalysembr
&Scoped-define FIRST-TABLE-IN-QUERY-Dialog-Frame ArtBas
&Scoped-define SECOND-TABLE-IN-QUERY-Dialog-Frame preemanalysembr


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 B-Kampanj Btn_OK Btn_Cancel Btn_Help ~
B-Alla FI_Element FI_KampTilbId B-Tilbud 
&Scoped-Define DISPLAYED-OBJECTS FI_AnalyseId FI_radnr FI_typ FI_typtxt ~
FI_Element FI_KampTilbId FI_beskr 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-Alla 
     LABEL "Alla" 
     SIZE 10 BY 1.12.

DEFINE BUTTON B-Kampanj 
     IMAGE-UP FILE "icon/e-sokpr.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "&Kampanj" 
     SIZE 4.57 BY 1.04 TOOLTIP "Ny post (Alt-N)".

DEFINE BUTTON B-Tilbud 
     IMAGE-UP FILE "icon/e-sokpr.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "&Tilbud" 
     SIZE 4.57 BY 1.04 TOOLTIP "Ny post (Alt-N)".

DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Avbryt" 
     SIZE 15 BY 1.15
     BGCOLOR 8 .

DEFINE BUTTON Btn_Help 
     LABEL "&Help" 
     SIZE 15 BY 1.15
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.15
     BGCOLOR 8 .

DEFINE VARIABLE FI_AnalyseId AS INTEGER FORMAT ">>>>>9" INITIAL 0 
     LABEL "Analysid" 
     VIEW-AS FILL-IN 
     SIZE 10.43 BY 1.

DEFINE VARIABLE FI_beskr AS CHARACTER FORMAT "X(30)" 
     LABEL "Beskriving" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1
     BGCOLOR 15 .

DEFINE VARIABLE FI_Element AS DECIMAL FORMAT ">>>>>>>>>>>>9" INITIAL 0 
     LABEL "Artikkelnr" 
     VIEW-AS FILL-IN 
     SIZE 20.14 BY 1.

DEFINE VARIABLE FI_KampTilbId AS INTEGER FORMAT ">>>>>>>>9" INITIAL 0 
     LABEL "KampTilbId" 
     VIEW-AS FILL-IN 
     SIZE 14.57 BY 1.

DEFINE VARIABLE FI_radnr AS INTEGER FORMAT ">>>9" INITIAL 0 
     LABEL "Radnr" 
     VIEW-AS FILL-IN 
     SIZE 7.57 BY 1.

DEFINE VARIABLE FI_typ AS INTEGER FORMAT "9":U INITIAL 0 
     LABEL "Typ" 
     VIEW-AS FILL-IN 
     SIZE 7.57 BY 1 NO-UNDO.

DEFINE VARIABLE FI_typtxt AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 24.57 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 52 BY 8.92.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Dialog-Frame FOR 
      ArtBas, 
      preemanalysembr SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     B-Kampanj AT ROW 5.69 COL 37.14 NO-TAB-STOP 
     Btn_OK AT ROW 1.46 COL 57
     FI_AnalyseId AT ROW 2.69 COL 14.86 COLON-ALIGNED HELP
          "Unikt nummer på analysen."
     Btn_Cancel AT ROW 2.69 COL 57
     FI_radnr AT ROW 3.69 COL 14.86 COLON-ALIGNED
     FI_typ AT ROW 4.69 COL 14.86 COLON-ALIGNED
     FI_typtxt AT ROW 4.69 COL 22.43 COLON-ALIGNED NO-LABEL
     Btn_Help AT ROW 4.69 COL 57
     B-Alla AT ROW 5.69 COL 42.14
     FI_Element AT ROW 5.77 COL 14.86 COLON-ALIGNED
     FI_KampTilbId AT ROW 6.81 COL 14.86 COLON-ALIGNED
     FI_beskr AT ROW 7.81 COL 14.86 COLON-ALIGNED HELP
          "Navn på analysen"
     B-Tilbud AT ROW 6.77 COL 37.14 NO-TAB-STOP 
     RECT-1 AT ROW 1.38 COL 2.14
     SPACE(19.05) SKIP(0.12)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Ny/Ändra analysmedlem"
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
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN FI_AnalyseId IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI_beskr IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI_radnr IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI_typ IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI_typtxt IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX Dialog-Frame
/* Query rebuild information for DIALOG-BOX Dialog-Frame
     _TblList          = "SkoTex.ArtBas,data.preemanalysembr WHERE SkoTex.ArtBas ..."
     _Options          = "SHARE-LOCK"
     _Query            is OPENED
*/  /* DIALOG-BOX Dialog-Frame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON GO OF FRAME Dialog-Frame /* Ny/Ändra analysmedlem */
DO:
  IF cType = "Endre" THEN
      FIND CURRENT preemanalysembr EXCLUSIVE.
  ELSE DO:
      CREATE preemanalysembr.
      ASSIGN preemanalysembr.AnalyseId = FI_AnalyseId.
      rAnalysembr = ROWID(preemanalysembr).
  END.
  ASSIGN preemanalysembr.radnr      = FI_radnr
         preemanalysembr.artikkelnr = IF FI_Typ = 4 OR FI_Typ = 6 THEN FI_element ELSE 0
         preemanalysembr.KampId     = IF FI_Typ = 5 THEN FI_element ELSE 0
         preemanalysembr.KampTilbId = IF FI_Typ = 5 THEN FI_KampTilbId ELSE 0.

  cRetValue = "OK".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Ny/Ändra analysmedlem */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Alla
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Alla Dialog-Frame
ON CHOOSE OF B-Alla IN FRAME Dialog-Frame /* Alla */
DO:
    FIND KampanjeMixMatch WHERE KampanjeMixMatch.KampId = FI_Element NO-LOCK NO-ERROR.
    IF NOT AVAIL KampanjeMixMatch THEN
        RETURN NO-APPLY.
    FOR EACH Kampanjetilbud WHERE KampanjeTilbud.KampId = KampanjeMixMatch.KampId NO-LOCK:
        IF NOT CAN-FIND(FIRST preemanalysembr where
                    preemanalysembr.AnalyseId  = FI_AnalyseId AND
                    preemanalysembr.radnr      = FI_radnr     AND
                    preemanalysembr.KampId     = KampanjeTilbud.KampId   AND
                    preemanalysembr.KampTilbId = KampanjeTilbud.KampTilbId) THEN DO:
            CREATE preemanalysembr.
            ASSIGN preemanalysembr.AnalyseId = FI_AnalyseId
                   preemanalysembr.radnr      = FI_radnr
                   preemanalysembr.artikkelnr = 0
                   preemanalysembr.KampId     = KampanjeTilbud.KampId
                   preemanalysembr.KampTilbId = KampanjeTilbud.KampTilbId.
            rAnalysembr = ROWID(preemanalysembr).
        END.
    END.
    cRetValue = "OK".
    APPLY "END-ERROR" TO FRAME Dialog-Frame.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Kampanj
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Kampanj Dialog-Frame
ON CHOOSE OF B-Kampanj IN FRAME Dialog-Frame /* Kampanj */
DO:
    DEFINE VARIABLE cLookupValue AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cRowIdList   AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cIdList      AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE bOK          AS LOGICAL     NO-UNDO.
    DEFINE VARIABLE dArtikkelNr AS DECIMAL     NO-UNDO.
    IF iTyp = 4 THEN DO: /* artikel */

        RUN d-hsok.w (OUTPUT dArtikkelNr,"NEI" + CHR(1) + "0"). /* JA = sök mot vpistrekkode */
        IF dArtikkelnr = ? THEN
            RETURN NO-APPLY.
        IF CAN-FIND(FIRST preemanalysembr where
                    preemanalysembr.AnalyseId  = FI_AnalyseId AND
/*                         preemanalysembr.radnr      = FI_radnr     AND */
                    preemanalysembr.Artikkelnr = dArtikkelnr) THEN DO:
            IF CAN-FIND(FIRST preemanalysembr where
                        preemanalysembr.AnalyseId  = FI_AnalyseId AND
                        preemanalysembr.radnr      = FI_radnr     AND
                        preemanalysembr.Artikkelnr = dArtikkelnr) THEN DO:
                MESSAGE "Redan registrerad på denna rapportrad"
                    VIEW-AS ALERT-BOX INFO BUTTONS OK.
            END.
            ELSE DO:
                MESSAGE "Redan registrerad på annan rapportrad"
                    VIEW-AS ALERT-BOX INFO BUTTONS OK.
            END.
            RETURN NO-APPLY.
        END.
        FIND Artbas WHERE Artbas.artikkelnr = dArtikkelnr NO-LOCK.
        ASSIGN FI_Element = dArtikkelnr
               FI_beskr   = Artbas.beskr.
        DISPLAY FI_Element FI_beskr WITH FRAME {&FRAME-NAME}.
/*         cLookupValue = "TeamNr,Beskrivelse".                                                                                               */
/*         RUN JBoxDLookup.w ("Butikkteam;Teamnr;TeamTypeid;Beskrivelse", "where BrGrpNr = 1 and TeamTypeId = 2", INPUT-OUTPUT cLookupValue). */
/*         IF NUM-ENTRIES(cLookupValue,"|") = 2 THEN DO:                                                                                      */
/*             ASSIGN FI_Teamnr   = INT(ENTRY(1,cLookupValue,"|"))                                                                            */
/*                    FI_Teamnamn = ENTRY(2,cLookupValue,"|").                                                                                */
/*             DISPLAY FI_Teamnr FI_Teamnamn WITH FRAME {&FRAME-NAME}.                                                                        */
/*         END.                                                                                                                               */
    END.
    IF iTyp = 5 THEN DO: /* kampanj */
        B-Alla:HIDDEN = TRUE.
       RUN JBoxDLookup.w ("KampanjeMixMatch;KampId;KampNavn;KampStartDato;KampSluttDato", 
                           "where KampanjeMixMatch.KampEierId = 1 and                     
                                  KampanjeMixMatch.KampKlar = true", INPUT-OUTPUT cIdList).
       IF cIdList <> "" AND FI_Element <> DECI(cIdList) THEN DO:
           ASSIGN FI_Element = DECI(cIdList)
                  FI_KampTilbId = 0
                  FI_beskr = "".
           DISPLAY FI_Element FI_KampTilbId FI_beskr WITH FRAME {&FRAME-NAME}.
           B-Alla:HIDDEN = FALSE.
           APPLY "ENTRY" TO FI_KampTilbId.
       END.
    END.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Tilbud
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Tilbud Dialog-Frame
ON CHOOSE OF B-Tilbud IN FRAME Dialog-Frame /* Tilbud */
DO:
    DEFINE VARIABLE cLookupValue AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cRowIdList   AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cIdList      AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE bOK          AS LOGICAL     NO-UNDO.
    IF iTyp = 5 THEN DO: /* kampanj */
        IF INPUT FI_Element = 0 THEN DO:
            MESSAGE "Registrera kampanj"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            RETURN NO-APPLY.
        END.
        RUN JBoxDLookup.w ("KampanjeTilbud;KampTilbId;KampTilbNavn;KampId", 
                            "WHERE KampanjeTilbud.KampId = " + STRING(FI_Element), INPUT-OUTPUT cIdList).

/*         RUN JBoxDSelector.w (THIS-PROCEDURE,0,                               */
/*                             "KampanjeTilbud;KampId;KampTilbId;KampTilbNavn", */
/*                             "WHERE KampanjeTilbud.KampId = FI_Element",      */
/*                             INPUT-OUTPUT cRowIdList,                         */
/*                             "KampTilbId",                                    */
/*                             INPUT-OUTPUT cIdList,                            */
/*                             "","",                                           */
/*                             OUTPUT bOK).                                     */
        IF cIdList <> "" AND FI_KampTilbId <> INT(cIdList) THEN DO:
            IF CAN-FIND(FIRST preemanalysembr where
                        preemanalysembr.AnalyseId  = FI_AnalyseId AND
/*                         preemanalysembr.radnr      = FI_radnr     AND */
                        preemanalysembr.KampId     = FI_element   AND
                        preemanalysembr.KampTilbId = INT(cIdList)) THEN DO:
                IF CAN-FIND(FIRST preemanalysembr where
                            preemanalysembr.AnalyseId  = FI_AnalyseId AND
                            preemanalysembr.radnr      = FI_radnr     AND
                            preemanalysembr.KampId     = FI_element   AND
                            preemanalysembr.KampTilbId = INT(cIdList)) THEN DO:
                    MESSAGE "Redan registrerad på denna rapportrad"
                        VIEW-AS ALERT-BOX INFO BUTTONS OK.
                END.
                ELSE DO:
                    MESSAGE "Redan registrerad på annan rapportrad"
                        VIEW-AS ALERT-BOX INFO BUTTONS OK.
                END.
                RETURN NO-APPLY.
            END.
            FI_KampTilbId = INT(cIdList).
            FIND KampanjeTilbud WHERE KampanjeTilbud.KampId = FI_Element AND
                                      KampanjeTilbud.KampTilbId = FI_KampTilbId NO-LOCK.
            FI_beskr = KampTilbNavn.
            DISPLAY FI_KampTilbId FI_beskr WITH FRAME {&FRAME-NAME}.
            APPLY "ENTRY" TO FI_beskr.
        END.
    END.
    RETURN NO-APPLY.
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


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:
  RUN kontrollerInput.
  IF RETURN-VALUE = "AVBRYT" THEN
      RETURN NO-APPLY.
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
  IF iTyp = 5 THEN
      FI_Element:LABEL = "KampanjId".
  ELSE IF iTyp = 6 THEN
      FI_Element:LABEL = "Vg".
  IF rAnalysembr <> ? THEN DO:
      FIND preemanalysembr WHERE ROWID(preemanalysembr) = rAnalysembr NO-LOCK.
      IF preemanalysembr.artikkelnr = 0 THEN DO:
          ASSIGN dElement      = preemanalysembr.KampId
                 FI_KampTilbId = preemanalysembr.KampTilbId.
          FIND KampanjeTilbud WHERE KampanjeTilbud.KampId = preemanalysembr.KampId AND
                                    KampanjeTilbud.KampTilbId = preemanalysembr.KampTilbId NO-LOCK.
          FI_beskr = KampanjeTilbud.KampTilbNavn.
      END.
      ELSE DO:
          dElement = preemanalysembr.artikkelnr.
          FIND Artbas WHERE artbas.artikkelnr = preemanalysembr.artikkelnr NO-LOCK.
          FI_beskr = Artbas.beskr.
      END.
                 
  END.
  ASSIGN 
      FI_AnalyseId = iAnalyseId
      FI_radnr     = iRadnr
      FI_typ       = iTyp
      FI_typtxt    = cTyptxt
      FI_Element   = dElement.
/*       FI_beskr     = cElementTxt. */
  RUN enable_UI.
  B-Alla:HIDDEN = TRUE.
  IF iTyp = 4 OR iTyp = 6 THEN DO:
      ASSIGN B-Tilbud:HIDDEN      = TRUE
             FI_KampTilbId:HIDDEN = TRUE.
  END.
  APPLY "ENTRY" TO FI_Element IN FRAME {&FRAME-NAME}.
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.
RETURN cRetValue.

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

  {&OPEN-QUERY-Dialog-Frame}
  GET FIRST Dialog-Frame.
  DISPLAY FI_AnalyseId FI_radnr FI_typ FI_typtxt FI_Element FI_KampTilbId 
          FI_beskr 
      WITH FRAME Dialog-Frame.
  ENABLE RECT-1 B-Kampanj Btn_OK Btn_Cancel Btn_Help B-Alla FI_Element 
         FI_KampTilbId B-Tilbud 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE kontrollerInput Dialog-Frame 
PROCEDURE kontrollerInput :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:
      ASSIGN INPUT FI_Element.
      IF INPUT FI_Element = 0 THEN DO:
          MESSAGE "Registrera " + FI_Element:LABEL
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
      END.
      IF FI_Typ = 4 THEN DO:
          FIND artbas WHERE artbas.artikkelnr = FI_Element NO-LOCK NO-ERROR.
          APPLY "ENTRY" TO FI_Element.
          IF NOT AVAIL artbas THEN DO:
              MESSAGE "Okänd artikel"
                  VIEW-AS ALERT-BOX INFO BUTTONS OK.
              RETURN NO-APPLY.
          END.
      END.
      ELSE IF FI_Typ = 5 THEN DO:
          FIND KampanjeMixMatch WHERE KampanjeMixMatch.KampId = FI_Element NO-LOCK NO-ERROR.
          IF NOT AVAIL KampanjeMixMatch THEN DO:
              MESSAGE "Finner inte kampanj"
                  VIEW-AS ALERT-BOX INFO BUTTONS OK.
              APPLY "ENTRY" TO FI_Element.
              RETURN NO-APPLY.
          END.
      END.
      ELSE IF FI_Typ = 6 THEN DO:
          FIND vargr WHERE vargr.vg = INT(FI_Element) NO-LOCK NO-ERROR.
          APPLY "ENTRY" TO FI_Element.
          IF NOT AVAIL vargr THEN DO:
              MESSAGE "Okänd varugrupp"
                  VIEW-AS ALERT-BOX INFO BUTTONS OK.
              RETURN NO-APPLY.
          END.
      END.
      ASSIGN INPUT FI_Element.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

