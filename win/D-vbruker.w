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
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
  DEFINE VAR wRecid as recid NO-UNDO.
  define var wModus as char init "Ny" no-undo.
  
&ELSE
  DEFINE INPUT-output PARAMETER wRecid as recid NO-UNDO.
  define input        parameter wModus as char  no-undo.
&ENDIF

/* Preprossessor direktiver ---                                         */
&scoped-define br-tabell Bruker
&scoped-define KeyFelt BrukerID
&scoped-define OptKeyAssign

&scoped-define DataType STRING /* INT STRING DEC Datatype på nøkkelfelt*/
/* Henter eventuelle relaterte poster - før mainblokk */
&scoped-define FinnRelatertePoster 

/* Ekstra informasjon i find/where når det er flere ledd i indeks */
/* Brukes før KeyFelt assignes ved lagring.                       */
&scoped-define OptFind 

/* Felter som skal vises frem når rutinen VisPost kjøres */
&scoped-define VisPoster {&br-tabell}.Brukerid when available {&br-tabell} ~
                         {&br-tabell}.Navn when available {&br-tabell} ~
                         {&br-tabell}.Grad when available {&br-tabell} ~
                         {&br-tabell}.ButikkNr when available {&br-tabell} ~
                         {&br-tabell}.BrGrpNr when available {&br-tabell} ~
                         {&br-tabell}.BrukerType when available {&br-tabell}
                         
/* Alternative poster som skal vises når VisPost kjøres */
/* Ligger under display av data. Dvs selvstendig linje. */
&scoped-define VisAndreData IF AVAIL {&br-tabell} THEN CB-Sprak:SCREEN-VALUE = {&br-tabell}.Lng.

/* Ved sletting - Sjekker om posten kan slettes */
&scoped-define SjekkOmPostFinnes if can-find({&br-tabell} where ~
         {&br-tabell}.{&KeyFelt} = {&DataType}({&br-tabell}.{&KeyFelt}:screen-value)) or ~
         {&br-tabell}.{&KeyFelt}:screen-value = "" then ~
        do: ~
          if {&br-tabell}.{&KeyFelt}:screen-value = "" then do: ~
            message {&br-tabell}.{&KeyFelt}:LABEL " må ikke være blank" ~
            view-as alert-box title "Lagringsfeil". ~
            APPLY "ENTRY" TO {&br-tabell}.{&KeyFelt}. ~
          end. ~
          else do: ~
            message "{&br-tabell} finnes allerede med " {&br-tabell}.{&KeyFelt}:LABEL ~
            {&br-tabell}.{&KeyFelt}:screen-value ~
            view-as alert-box title "Lagringsfeil". ~
            APPLY "ENTRY" TO {&br-tabell}.{&KeyFelt}. ~
          end. ~
          return "AVBRYT". ~
        end.
        
&scoped-define EkstraSjekk if {&br-tabell}.Navn:screen-value = "" then do: ~
      message " Feltet må ikke være blank" ~
           view-as alert-box title "Lagringsfeil". ~
           APPLY "ENTRY" TO {&br-tabell}.Navn. ~
      RETURN "AVBRYT". ~
  end. ~
  IF NOT CAN-FIND(BrukerGrp WHERE BrukerGrp.BrGrpNr = INPUT {&br-tabell}.BrGrpNr) THEN DO: ~
     MESSAGE "Du må angi gyldig brukergruppe." VIEW-AS ALERT-BOX TITLE "Lagringsfeil". ~
     APPLY "ENTRY" TO {&br-tabell}.BrGrpNr. ~
     RETURN "AVBRYT". ~
  END.
        
/* Felter som assign'es når rutinen LagrePost kjøres */        
&scoped-define AssignFelter {&br-tabell}.Navn ~
                            {&br-tabell}.BrGrpNr ~
                            {&br-tabell}.ButikkNr ~
                            {&br-tabell}.Grad ~
                            {&br-tabell}.BrukerType

/* Tilleggs felter som assign'es når rutinen Lagre post kjøres. */
/* Står etter forrige assign. Dvs egen linje.                   */
&scoped-define TillegsAssign {&br-tabell}.Lng = CB-Sprak:SCREEN-VALUE.
/* Local Variable Definitions ---                                       */
def var wRetur-Verdi as char initial "AVBRYT" no-undo.

DEF VAR cBrukerId        AS CHAR NO-UNDO.
DEF VAR cLevBasRowIdList AS CHAR NO-UNDO.
DEF VAR cLevBasIdList    AS CHAR NO-UNDO.
DEF VAR bOk              AS LOG  NO-UNDO.


{runlib.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame
&Scoped-define BROWSE-NAME BROWSE-BrukerLev

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES BrukerLev LevBas

/* Definitions for BROWSE BROWSE-BrukerLev                              */
&Scoped-define FIELDS-IN-QUERY-BROWSE-BrukerLev BrukerLev.LevNr LevBas.levnamn   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-BrukerLev   
&Scoped-define SELF-NAME BROWSE-BrukerLev
&Scoped-define QUERY-STRING-BROWSE-BrukerLev FOR EACH BrukerLev NO-LOCK       WHERE BrukerLev.BrukerId = cBrukerId, ~
             FIRST LevBas NO-LOCK       WHERE LevBas.LevNr = BrukerLev.LevNr INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BROWSE-BrukerLev OPEN QUERY {&SELF-NAME} FOR EACH BrukerLev NO-LOCK       WHERE BrukerLev.BrukerId = cBrukerId, ~
             FIRST LevBas NO-LOCK       WHERE LevBas.LevNr = BrukerLev.LevNr INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BROWSE-BrukerLev BrukerLev LevBas
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-BrukerLev BrukerLev
&Scoped-define SECOND-TABLE-IN-QUERY-BROWSE-BrukerLev LevBas


/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define OPEN-BROWSERS-IN-QUERY-Dialog-Frame ~
    ~{&OPEN-QUERY-BROWSE-BrukerLev}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS Bruker.BrukerID Bruker.Navn Bruker.BrGrpNr ~
Bruker.BrukerType Bruker.grad Bruker.ButikkNr 
&Scoped-define ENABLED-TABLES Bruker
&Scoped-define FIRST-ENABLED-TABLE Bruker
&Scoped-Define ENABLED-OBJECTS B-LevNr RECT-1 RECT-64 BROWSE-BrukerLev ~
CB-Sprak btnSokBrGrp FILL-IN-PW FILL-IN-PW-2 Btn_OK Btn_Cancel Btn_Help ~
btnSokBrGrp-2 
&Scoped-Define DISPLAYED-FIELDS Bruker.BrukerID Bruker.Navn Bruker.BrGrpNr ~
Bruker.BrukerType Bruker.grad Bruker.ButikkNr 
&Scoped-define DISPLAYED-TABLES Bruker
&Scoped-define FIRST-DISPLAYED-TABLE Bruker
&Scoped-Define DISPLAYED-OBJECTS CB-Sprak FI-Beskrivelse FILL-IN-PW ~
FILL-IN-PW-2 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-LevNr  NO-FOCUS
     LABEL "Leverandør..." 
     SIZE 18 BY 1.14.

DEFINE BUTTON btnSokBrGrp  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON btnSokBrGrp-2  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Avbryt" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_Help 
     LABEL "&Hjelp" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE CB-Sprak AS CHARACTER FORMAT "X(256)":U 
     LABEL "Språkkode" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Beskrivelse AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-PW AS CHARACTER FORMAT "X(256)":U 
     LABEL "Passord" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-PW-2 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Bekreft passord" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 79 BY 9.05.

DEFINE RECTANGLE RECT-64
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 44 BY 9.05.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-BrukerLev FOR 
      BrukerLev, 
      LevBas SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-BrukerLev
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-BrukerLev Dialog-Frame _FREEFORM
  QUERY BROWSE-BrukerLev NO-LOCK DISPLAY
      BrukerLev.LevNr FORMAT "zzzzz9":U
      LevBas.levnamn FORMAT "x(30)":U WIDTH 30
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 42 BY 8.1 EXPANDABLE.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     B-LevNr AT ROW 10.76 COL 82.2 NO-TAB-STOP 
     Bruker.BrukerID AT ROW 2.19 COL 18 COLON-ALIGNED FORMAT "X(20)"
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     Bruker.Navn AT ROW 2.19 COL 34 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 42 BY 1
     BROWSE-BrukerLev AT ROW 2.19 COL 82
     Bruker.BrGrpNr AT ROW 3.19 COL 18 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 6.2 BY 1
     CB-Sprak AT ROW 4.19 COL 18 COLON-ALIGNED
     btnSokBrGrp AT ROW 3.19 COL 26.2 NO-TAB-STOP 
     FI-Beskrivelse AT ROW 4.19 COL 34 COLON-ALIGNED NO-LABEL
     FILL-IN-PW AT ROW 5.24 COL 18 COLON-ALIGNED
     FILL-IN-PW-2 AT ROW 6.24 COL 18 COLON-ALIGNED
     Bruker.BrukerType AT ROW 7.24 COL 18 COLON-ALIGNED
          VIEW-AS COMBO-BOX INNER-LINES 25
          LIST-ITEM-PAIRS "Kjede/System",1,
                     "Butikk",2,
                     "Leverandør",3
          DROP-DOWN-LIST
          SIZE 21 BY 1 TOOLTIP "Type av bruker."
     Bruker.grad AT ROW 8.24 COL 18 COLON-ALIGNED
          LABEL "Gradering"
          VIEW-AS COMBO-BOX INNER-LINES 25
          LIST-ITEM-PAIRS "Item 1",0
          DROP-DOWN-LIST
          SIZE 58 BY 1
     Bruker.ButikkNr AT ROW 9.29 COL 18 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     Btn_OK AT ROW 10.76 COL 2
     Btn_Cancel AT ROW 10.76 COL 17.8
     Btn_Help AT ROW 10.76 COL 109
     btnSokBrGrp-2 AT ROW 9.29 COL 30.6 NO-TAB-STOP 
     RECT-1 AT ROW 1.48 COL 2
     RECT-64 AT ROW 1.48 COL 81
     SPACE(0.99) SKIP(1.60)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Bruker"
         DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_Cancel.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB Dialog-Frame 
/* ************************* Included-Libraries *********************** */

{incl/DevMode.i}
{incl/CustDevMode.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
                                                                        */
/* BROWSE-TAB BROWSE-BrukerLev Navn Dialog-Frame */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE.

/* SETTINGS FOR FILL-IN Bruker.BrukerID IN FRAME Dialog-Frame
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN FI-Beskrivelse IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX Bruker.grad IN FRAME Dialog-Frame
   EXP-LABEL                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-BrukerLev
/* Query rebuild information for BROWSE BROWSE-BrukerLev
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH BrukerLev NO-LOCK
      WHERE BrukerLev.BrukerId = cBrukerId,
      FIRST LevBas NO-LOCK
      WHERE LevBas.LevNr = BrukerLev.LevNr INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _TblOptList       = ", OUTER"
     _Query            is OPENED
*/  /* BROWSE BROWSE-BrukerLev */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX Dialog-Frame
/* Query rebuild information for DIALOG-BOX Dialog-Frame
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX Dialog-Frame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Bruker */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-LevNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-LevNr Dialog-Frame
ON CHOOSE OF B-LevNr IN FRAME Dialog-Frame /* Leverandør... */
DO:
    DEFINE VARIABLE cRowIdList AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cIdList    AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE bOK        AS LOGICAL    NO-UNDO.

    DEF VAR cOldIdList AS CHAR NO-UNDO.
    DEF VAR cOldRowIdList AS CHAR NO-UNDO.
    DEF VAR piLoop     AS INT  NO-UNDO.

    FOR EACH BrukerLev NO-LOCK WHERE
        BrukerLev.BrukerId = cBrukerId:
        ASSIGN
            cRowIdList = cRowIdList + 
                         (IF cRowIdList = "" THEN "" ELSE ",") + 
                         STRING(ROWID(BrukerLev))
            cIdList    = cIdList + 
                         (IF cIdList = "" THEN "" ELSE "|") + 
                         STRING(BrukerLev.LevNr)
            .
    END.

    ASSIGN
        cOldRowIdList = cRowIdList
        cOldIdList = cIdList
        .

    run LagrePost.
    RUN JBoxDSelector.w (THIS-PROCEDURE,0,
                        "LevBas;LevNr;Levnamn",
                        "WHERE TRUE",
                        INPUT-OUTPUT cRowIdList,
                        "LevNr",
                        INPUT-OUTPUT cIdList,
                        "","",
                        OUTPUT bOK).
    IF bOK THEN DO:
        IF (cOldIdList <> "" OR cIdList <> "") THEN
        DO TRANSACTION:
            /* De dør */
            DO piLoop = 1 TO NUM-ENTRIES(cOldIdList,"|"):
                FIND BrukerLev EXCLUSIVE-LOCK WHERE
                    BrukerLev.BrukerId = cBrukerId AND
                    BrukerLev.LevNr    = INT(ENTRY(piLoop,cOldIdList,"|")) NO-ERROR.
                IF AVAILABLE BrukerLev THEN
                    DELETE BrukerLev.
            END.
            /* og gjenoppstår */
            DO piLoop = 1 TO NUM-ENTRIES(cIdList,"|"):
                FIND BrukerLev EXCLUSIVE-LOCK WHERE
                    BrukerLev.BrukerId = cBrukerId AND
                    BrukerLev.LevNr    = INT(ENTRY(piLoop,cIdList,"|")) NO-ERROR.
                IF NOT AVAILABLE BrukerLev THEN
                DO:
                    CREATE BrukerLev.
                    ASSIGN
                        BrukerLEv.BrukerId = cBrukerId
                        BrukerLev.LevNr    = INT(ENTRY(piLoop,cIdList,"|"))
                        .
                END.
            END.
        END.

        {&OPEN-QUERY-BROWSE-BrukerLev}
    END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSokBrGrp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSokBrGrp Dialog-Frame
ON CHOOSE OF btnSokBrGrp IN FRAME Dialog-Frame /* ... */
OR F10 OF Bruker.BrGrpNr
DO:
  DEF VAR cLookupValue AS CHAR NO-UNDO.

  /* Syntaks: Param1: <Tabell>;<Felt>;<Felt>...,<Tabell>;<Felt>;<Felt>...              */
  /*          Param2: <Where sats> m/Join                                              */
  /*          Param3: <Returfelt1>[;<Returfelt2>;......],<Filterfelt1>[;<Filterfelt2>] (Settes i cLookupValue) */
  /* Kalkulerte felt kan også benyttes, label, format o.l..       */
  cLookupValue = "BrGrpNr".
  RUN JBoxDLookup.w ("BrukerGrp;BrGrpNr;Beskrivelse","where true",INPUT-OUTPUT cLookupValue).

  IF cLookupValue NE "" THEN 
  DO:
    Bruker.BrGrpNr:SCREEN-VALUE = cLookupValue.
  END.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSokBrGrp-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSokBrGrp-2 Dialog-Frame
ON CHOOSE OF btnSokBrGrp-2 IN FRAME Dialog-Frame /* ... */
OR F10 OF Bruker.ButikkNr
DO:
  DEF VAR cLookupValue AS CHAR NO-UNDO.

  /* Syntaks: Param1: <Tabell>;<Felt>;<Felt>...,<Tabell>;<Felt>;<Felt>...              */
  /*          Param2: <Where sats> m/Join                                              */
  /*          Param3: <Returfelt1>[;<Returfelt2>;......],<Filterfelt1>[;<Filterfelt2>] (Settes i cLookupValue) */
  /* Kalkulerte felt kan også benyttes, label, format o.l..       */
  cLookupValue = "Butik".
  RUN JBoxDLookup.w ("Butiker;Butik;KortNavn;ButNamn","where true",INPUT-OUTPUT cLookupValue).

  IF cLookupValue NE "" THEN 
  DO:
    Bruker.ButikkNr:SCREEN-VALUE = cLookupValue.
  END.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Help
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Help Dialog-Frame
ON CHOOSE OF Btn_Help IN FRAME Dialog-Frame /* Hjelp */
OR HELP OF FRAME {&FRAME-NAME}
DO: /* Call Help Function (or a simple message). */
  {diahelp.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:
  run LagrePost.
  if return-value = "AVBRYT" then
    return no-apply.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CB-Sprak
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-Sprak Dialog-Frame
ON VALUE-CHANGED OF CB-Sprak IN FRAME Dialog-Frame /* Språkkode */
DO:
  FIND Sprak WHERE Sprak.Lng = SELF:SCREEN-VALUE NO-LOCK NO-ERROR.
  ASSIGN FI-Beskrivelse:SCREEN-VALUE = IF AVAIL Sprak THEN Sprak.Beskrivelse ELSE "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-PW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-PW Dialog-Frame
ON ENTRY OF FILL-IN-PW IN FRAME Dialog-Frame /* Passord */
DO:
  RUN SendMessageA(FILL-IN-PW:HWND,
                   204,
                   ASC("*"),
                   0).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-PW-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-PW-2 Dialog-Frame
ON ENTRY OF FILL-IN-PW-2 IN FRAME Dialog-Frame /* Bekreft passord */
DO:
  RUN SendMessageA(FILL-IN-PW-2:HWND,
                   204,
                   ASC("*"),
                   0).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-BrukerLev
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.

find {&br-tabell} no-lock where
  recid({&br-tabell}) = wRecid no-error.
if available {&br-tabell} then 
  do: 
    {&FinnRelatertePoster}  
  end.
      
/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  IF NOT CAN-FIND(FIRST BrukerGrp) THEN DO:
      MESSAGE "Brukergruppe mangler, må registreres."
          VIEW-AS ALERT-BOX Title "Feil".
      RETURN "AVBRYT".
  END.
  IF NOT CAN-FIND(FIRST Sprak) THEN DO:
      CREATE Sprak.
      ASSIGN Sprak.Lng = "DES"
             Sprak.Beskrivelse = "Design".
  END.
  RUN InitCB.
  RUN enable_UI.
  {lng.i}

  RUN VisCB.
  run VisPost.
    if wModus = "Ny" then
    assign
      {&br-tabell}.{&KeyFelt}:sensitive = true.
  else
    assign
      {&br-tabell}.{&KeyFelt}:sensitive = false.
  APPLY "VALUE-CHANGED" TO CB-Sprak.
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
  wRetur-Verdi = "OK".
END.
RUN disable_UI.

&IF DEFINED(UIB_IS_RUNNING) EQ 0 &THEN
 return wretur-verdi.
&else
 message wretur-verdi view-as alert-box.
&endif

PROCEDURE SendMessageA EXTERNAL "user32.dll":
   DEFINE INPUT PARAMETER win-handle AS LONG.
   DEFINE INPUT PARAMETER win-msg    AS LONG.
   DEFINE INPUT PARAMETER win-param1 AS LONG.
   DEFINE INPUT PARAMETER win-param2 AS LONG.
END PROCEDURE.

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
  DISPLAY CB-Sprak FI-Beskrivelse FILL-IN-PW FILL-IN-PW-2 
      WITH FRAME Dialog-Frame.
  IF AVAILABLE Bruker THEN 
    DISPLAY Bruker.BrukerID Bruker.Navn Bruker.BrGrpNr Bruker.BrukerType 
          Bruker.grad Bruker.ButikkNr 
      WITH FRAME Dialog-Frame.
  ENABLE B-LevNr RECT-1 RECT-64 Bruker.BrukerID Bruker.Navn BROWSE-BrukerLev 
         Bruker.BrGrpNr CB-Sprak btnSokBrGrp FILL-IN-PW FILL-IN-PW-2 
         Bruker.BrukerType Bruker.grad Bruker.ButikkNr Btn_OK Btn_Cancel 
         Btn_Help btnSokBrGrp-2 
      WITH FRAME Dialog-Frame.
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
  DEF VAR pcTekst   AS CHAR NO-UNDO.
  DEF VAR wSprakstr AS CHAR NO-UNDO.
  FOR EACH Sprak NO-LOCK:
      wSprakstr = 
          IF wSprakstr = "" THEN Sprak.Lng ELSE wSprakstr + "," + Sprak.Lng.
  END.
  ASSIGN CB-Sprak:LIST-ITEMS IN FRAME {&FRAME-NAME} = wSprakstr.

  
  FOR EACH SysPara NO-LOCK WHERE
      SysPara.SysHId = 18 AND
      SysPara.SysGr  = 1:
      pcTekst = pcTekst + 
                (IF pcTekst = ""
                   THEN ""
                   ELSE ",") +
                trim(STRING(SysPara.Parameter1)) + "/" + sysPara.Beskrivelse + "," + 
                STRING(SysPara.Parameter1).
  END.
  ASSIGN
      Bruker.Grad:LIST-ITEM-PAIRS = pcTekst
      Bruker.Grad:SCREEN-VALUE    = ENTRY(2,pcTekst)
      .
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LagrePost Dialog-Frame 
PROCEDURE LagrePost :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR wSkapaUser AS LOGI NO-UNDO.
do with frame Dialog-Frame transaction:
  /* Sjekker nye poster. */
  if wModus = "Ny" then
    do:
      {&SjekkOmPostFinnes}
      {&EkstraSjekk}
      IF /*(Fill-IN-PW:SCREEN-VALUE = "" OR Fill-IN-PW-2:SCREEN-VALUE = "") OR*/
         (Fill-IN-PW:SCREEN-VALUE <> Fill-IN-PW-2:SCREEN-VALUE) THEN DO:
          MESSAGE "Ulike passord angitt. Angi passord og bekreft." VIEW-AS ALERT-BOX TITLE
                  "Lagringsfeil".
          APPLY "ENTRY" TO FILL-IN-PW.
          RETURN "AVBRYT".
      END.
      ASSIGN wSkapaUser = TRUE.
      {&OptFind}
      create {&br-tabell}.
      assign 
        {&OptKeyAssign}
        {&br-tabell}.{&KeyFelt} = {&DataType}({&br-tabell}.{&KeyFelt}:screen-value)
        wRecid   = recid({&br-tabell}).
  end.
  else DO:
      {&EkstraSjekk}
      IF (Fill-IN-PW:SCREEN-VALUE <> "" OR Fill-IN-PW-2:SCREEN-VALUE <> "") THEN DO:
         IF (Fill-IN-PW:SCREEN-VALUE <> Fill-IN-PW-2:SCREEN-VALUE) THEN DO:
            MESSAGE "Angi passord og bekreft." VIEW-AS ALERT-BOX ERROR TITLE
                  "Feil passord".
            APPLY "ENTRY" TO FILL-IN-PW.
            RETURN "AVBRYT".
          END.
          ELSE DO:
              FIND _User WHERE _User._Userid = Bruker.Brukerid:SCREEN-VALUE NO-ERROR.
              IF AVAIL _User THEN
                  DELETE _User.
              ASSIGN wSkapaUser = TRUE.
          END.
      END.
     find {&br-tabell} Exclusive-lock where
       recid({&br-tabell}) = wRecid no-error.
  END.
  assign
    {&AssignFelter}.
  {&TillegsAssign}
  IF wSkapaUser = TRUE THEN DO:
    find _User exclusive-lock where
      _User._USerId = Bruker.BrukerId no-error.
    if not available _User then
      do:
        CREATE _User.
        
        ASSIGN _User._Userid = Bruker.Brukerid:SCREEN-VALUE.
      end.
    assign
      _User._Password = ENCODE(FILL-IN-PW:SCREEN-VALUE).
  END.
end. /* TRANSACTION */    

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE VisCB Dialog-Frame 
PROCEDURE VisCB :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR wDefSprak AS CHAR NO-UNDO.
  DO WITH FRAME {&FRAME-NAME}:
    if wModus = "Ny" OR Bruker.Lng = "" then
      {syspara.i 1 3 1 wDefSprak}
    ASSIGN CB-Sprak:SCREEN-VALUE = IF CAN-DO(CB-Sprak:LIST-ITEMS,wDefSprak) THEN
           wDefSprak ELSE ENTRY(1,CB-Sprak:LIST-ITEMS).
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE VisPost Dialog-Frame 
PROCEDURE VisPost :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  display 
    {&VisPoster}
  with frame Dialog-Frame.
  {&VisAndreData}

  IF AVAILABLE Bruker THEN DO:
      cBrukerId = Bruker.BrukerId.
      {&OPEN-QUERY-BROWSE-BrukerLev}
      
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

