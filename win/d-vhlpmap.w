&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          skotex           PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------
  Forfatter:   SJ
  Beskrivelse: Program for SD (ikke brukerne ute). Detaljer/ny post for
               kobling mot hjelpefil. 
  Parametere:  INPUT CHAR wLdNavn, INPUT CHAR wId
  Endringer:
------------------------------------------------------------------------*/

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameter Definisjoner ---                                           */
&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN      
    DEF VAR wLdNavn AS CHAR NO-UNDO INIT "SkoTex".   
    DEF VAR wId     AS CHAR NO-UNDO. 
&ELSE
    DEF INPUT PARAMETER wLdNavn AS CHAR NO-UNDO.
    DEF INPUT PARAMETER wId     AS CHAR NO-UNDO.
&ENDIF

/* Preprossessor direktiver ---                                         */

/* Buffer og Temp-Table Definisjoner ---                                */

/* Lokale variabler ---                                                 */
def var retur-verdi as char initial "<avbryt>" no-undo.

define var m-ja              as logical no-undo.
define var m-i               as integer no-undo.
define var m-x               as character no-undo.
define var m-handle          as handle no-undo.
define var m-wh              as widget-handle no-undo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES HjelpMap

/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define OPEN-QUERY-Dialog-Frame OPEN QUERY Dialog-Frame FOR EACH HjelpMap SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-Dialog-Frame HjelpMap
&Scoped-define FIRST-TABLE-IN-QUERY-Dialog-Frame HjelpMap


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS HjelpMap.Id HjelpMap.Beskrivelse ~
HjelpMap.ContextNr 
&Scoped-define FIELD-PAIRS~
 ~{&FP1}Id ~{&FP2}Id ~{&FP3}~
 ~{&FP1}Beskrivelse ~{&FP2}Beskrivelse ~{&FP3}~
 ~{&FP1}ContextNr ~{&FP2}ContextNr ~{&FP3}
&Scoped-define ENABLED-TABLES HjelpMap
&Scoped-define FIRST-ENABLED-TABLE HjelpMap
&Scoped-Define ENABLED-OBJECTS RECT-1 Btn_OK Btn_Cancel 
&Scoped-Define DISPLAYED-FIELDS HjelpMap.Id HjelpMap.Beskrivelse ~
HjelpMap.ContextNr 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Avbryt" 
     SIZE 12 BY 1.1
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO DEFAULT 
     LABEL "OK" 
     SIZE 12 BY 1.1
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 71 BY 5.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Dialog-Frame FOR 
      HjelpMap SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     HjelpMap.Id AT ROW 1.71 COL 13 COLON-ALIGNED
          LABEL "&Identitet"
          VIEW-AS FILL-IN 
          SIZE 41 BY 1
     HjelpMap.Beskrivelse AT ROW 3.14 COL 3
          LABEL "&Beskrivelse"
          VIEW-AS FILL-IN 
          SIZE 57 BY 1
     HjelpMap.ContextNr AT ROW 4.57 COL 13 COLON-ALIGNED
          LABEL "&Contextnr"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     Btn_OK AT ROW 8.14 COL 2
     Btn_Cancel AT ROW 8.14 COL 61
     RECT-1 AT ROW 1.24 COL 2
     "Programmereren legger inn identitet og en kort beskrivelse." VIEW-AS TEXT
          SIZE 55 BY .62 AT ROW 6.48 COL 3
     "Den som har laget hjelpefila, legger inn Contextnr." VIEW-AS TEXT
          SIZE 48 BY .62 AT ROW 7.19 COL 3
     SPACE(22.00) SKIP(1.43)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Kobling mot hjelp"
         CANCEL-BUTTON Btn_Cancel.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS


/* ***************  Runtime Attributes and UIB Settings  ************** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   Custom                                                               */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN HjelpMap.Beskrivelse IN FRAME Dialog-Frame
   ALIGN-L EXP-LABEL                                                    */
/* SETTINGS FOR FILL-IN HjelpMap.ContextNr IN FRAME Dialog-Frame
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN HjelpMap.Id IN FRAME Dialog-Frame
   EXP-LABEL                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX Dialog-Frame
/* Query rebuild information for DIALOG-BOX Dialog-Frame
     _TblList          = "HjelpMap"
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX Dialog-Frame */
&ANALYZE-RESUME

 




/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON GO OF FRAME Dialog-Frame /* Kobling mot hjelp */
DO:
   DEF VAR wOrgId          AS CHAR CASE-SENSITIVE NO-UNDO.
   DEF VAR wNyId           AS CHAR CASE-SENSITIVE NO-UNDO.
   DEF VAR wOrgBeskrivelse AS CHAR CASE-SENSITIVE NO-UNDO.
   DEF VAR wNyBeskrivelse  AS CHAR CASE-SENSITIVE NO-UNDO.
   DEF BUFFER bHjelpMap FOR HjelpMap.
   DO WITH FRAME {&FRAME-NAME}:
      IF INPUT HjelpMap.Id = "" OR INPUT HjelpMap.Id = ? THEN DO:
         MESSAGE "Id må oppgis." VIEW-AS ALERT-BOX ERROR TITLE "Feil".
         APPLY "ENTRY" TO HjelpMap.Id.
         RETURN NO-APPLY.
      END.
   END.
   IF NOT AVAIL HjelpMap THEN DO:
      FIND bHjelpMap WHERE 
           bHjelpMap.LdNavn = wLdNavn AND
           bHjelpMap.Id     = INPUT HjelpMap.Id NO-LOCK NO-ERROR.
      IF AVAIL bHjelpMap THEN DO:
         MESSAGE "Det finnes allerede en annen kobling med samme Id." SKIP
                 "Du kan ikke benytte den angitte Id." 
           VIEW-AS ALERT-BOX ERROR TITLE "Feil".
         APPLY "ENTRY" TO HjelpMap.Id.
         RETURN NO-APPLY.
      END.            
      DO TRANSACTION WITH FRAME {&FRAME-NAME}:
         CREATE HjelpMap.
         ASSIGN HjelpMap.LdNavn = wLdNavn
                HjelpMap.Id
                HjelpMap.Beskrivelse
                HjelpMap.ContextNr
                Retur-verdi = STRING(ROWID(HjelpMap)).
      END.   
   END.
   ELSE DO WITH FRAME {&FRAME-NAME}:
      /* Sjekker mot CASE_SENSITIVE variabler om endring er gjort */
      ASSIGN wOrgId          = HjelpMap.Id
             wNyId           = INPUT HjelpMap.Id
             wOrgBeskrivelse = HjelpMap.Beskrivelse
             wNyBeskrivelse  = INPUT HjelpMap.Beskrivelse.
      IF wNyId <> wOrgId OR
         wNyBeskrivelse <> wOrgBeskrivelse OR
         INPUT HjelpMap.ConTextNr <> HjelpMap.ContextNr 
      THEN DO TRANSACTION WITH FRAME {&FRAME-NAME}:       
         FIND HjelpMap WHERE 
              HjelpMap.LdNavn = wLdNavn AND
              HjelpMap.Id     = wId     EXCLUSIVE NO-WAIT NO-ERROR.
         IF NOT AVAIL HjelpMap THEN DO:
            MESSAGE "Koblingen er sperret av en annen bruker." SKIP
                    "Kan ikke gjøre oppdateringen nå....."
              VIEW-AS ALERT-BOX ERROR TITLE "Feil".
            RETURN NO-APPLY.
         END.
      
         ASSIGN HjelpMap.Id
                HjelpMap.Beskrivelse
                HjelpMap.ContextNr
                Retur-verdi = STRING(ROWID(HjelpMap)).       
      END.   
   END.   
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Kobling mot hjelp */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */
{defaultbtn.i Btn_Ok}

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK: 
  RUN Initiering. 
  {lng.i} RUN enable_UI.
  DO WITH FRAME {&FRAME-NAME}:
     IF AVAIL HjelpMap THEN 
        APPLY "ENTRY" TO HjelpMap.ContextNr.
     ELSE
     IF wId <> "" THEN
        APPLY "ENTRY" TO HjelpMap.Beskrivelse.     
  END.
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

&IF DEFINED(UIB_IS_RUNNING) EQ 0 &THEN
 return retur-verdi.
&else
 message retur-verdi view-as alert-box.
&endif

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dialog-Frame _DEFAULT-DISABLE
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dialog-Frame _DEFAULT-ENABLE
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
  IF AVAILABLE HjelpMap THEN 
    DISPLAY HjelpMap.Id HjelpMap.Beskrivelse HjelpMap.ContextNr 
      WITH FRAME Dialog-Frame.
  ENABLE RECT-1 HjelpMap.Id HjelpMap.Beskrivelse HjelpMap.ContextNr Btn_OK 
         Btn_Cancel 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Initiering Dialog-Frame 
PROCEDURE Initiering :
/*------------------------------------------------------------------------------
  Purpose:     Initierer skjermbildet
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF wId <> "" THEN
  FIND HjelpMap WHERE
       HjelpMap.LdNavn = wLdNavn AND
       HjelpMap.Id     = wId     NO-LOCK NO-ERROR.
       
  ASSIGN FRAME {&FRAME-NAME}:TITLE = (IF NOT AVAIL HjelpMap THEN "Ny kobling mot hjelpefil" ELSE "Detaljer for kobling mot hjelpefil") + " (System " + CAPS(wLdNavn) + ")".
  DO WITH FRAME {&FRAME-NAME}:
     DISPL wId @ HjelpMap.Id.
  END.   

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


