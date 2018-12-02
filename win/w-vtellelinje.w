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

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
  DEFINE VAR wRowId AS ROWID NO-UNDO.
  DEFINE VAR wModus AS CHAR  NO-UNDO.
  DEFINE VAR wButik AS INT NO-UNDO.
  DEFINE VAR wTelleHodeRecid AS ROWID NO-UNDO.
  DEF    VAR wParentHandle AS HANDLE NO-UNDO.
  DEF    VAR wAlleBut AS LOG NO-UNDO.
  
&ELSE
  DEFINE INPUT-OUTPUT PARAMETER wRowId AS ROWID NO-UNDO.
  DEFINE INPUT        PARAMETER wModus AS CHAR  NO-UNDO.
  DEFINE INPUT        PARAMETER wbutik AS INT   NO-UNDO.
  DEFINE INPUT        PARAMETER wTelleHodeROWID AS ROWID NO-UNDO.
  DEFINE INPUT        PARAMETER wParentHandle   AS HANDLE NO-UNDO.
  DEFINE INPUT        PARAMETER wAlleBut        AS LOG NO-UNDO.
&ENDIF

/* Preprossessor direktiver ---                                         */
&scoped-define br-tabell TelleLinje
&scoped-define KeyFelt Vg
&scoped-define OptKeyAssign ~
  TelleLinje.TelleNr = TelleHode.TelleNr ~
  TelleLinje.butik   = wButik

&scoped-define DataType INT /* INT STRING DEC Datatype på nøkkelfelt*/
/* Henter eventuelle relaterte poster - før mainblokk */
&scoped-define FinnRelatertePoster 

/* Ekstra informasjon i find/where når det er flere ledd i indeks */
/* Brukes før KeyFelt assignes ved lagring.                       */
&scoped-define OptFind 

/* Alternative poster som skal vises når VisPost kjøres */
/* Ligger under display av data. Dvs selvstendig linje. */
&scoped-define VisAndreData

/* Ved sletting - Sjekker om posten kan slettes */
&scoped-define SjekkOmPostFinnes 
        
/* Tilleggs felter som assign'es når rutinen Lagre post kjøres. */
/* Står etter forrige assign. Dvs egen linje.                   */
&scoped-define TillegsAssign

/* Local Variable Definitions ---                                       */
DEF VAR wRetur-Verdi AS CHAR INITIAL "AVBRYT" NO-UNDO.

{runlib.i}

DEF VAR wStorl AS CHAR NO-UNDO.
DEF VAR wSvar  AS LOG NO-UNDO.
DEF VAR wNedskriv   AS LOG                NO-UNDO.
DEF VAR wTekst      AS CHAR               NO-UNDO.
DEF VAR wEDB-System AS CHAR               NO-UNDO.
DEF VAR wTabell     AS CHAR               NO-UNDO.
DEF VAR wCl         AS INT                NO-UNDO.
DEF VAR wDecimaler  AS CHAR               NO-UNDO.
DEF VAR cOKStorl    AS CHAR               NO-UNDO.
DEF VAR wArtikkelNr AS DEC                NO-UNDO.
DEF VAR cStrekKode  AS CHAR               NO-UNDO.
DEF VAR piInt       AS INT                NO-UNDO.
DEF VAR lDec        AS DEC                NO-UNDO.
DEF VAR wOk AS LOG INITIAL FALSE.

DEF BUFFER clButiker FOR Butiker.
DEF BUFFER bButiker  FOR Butiker.
DEF BUFFER bArtBas FOR ArtBas.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES TelleLinje

/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define FIELDS-IN-QUERY-DEFAULT-FRAME TelleLinje.Vg TelleLinje.LopNr 
&Scoped-define ENABLED-FIELDS-IN-QUERY-DEFAULT-FRAME TelleLinje.Vg ~
TelleLinje.LopNr 
&Scoped-define ENABLED-TABLES-IN-QUERY-DEFAULT-FRAME TelleLinje
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-DEFAULT-FRAME TelleLinje
&Scoped-define QUERY-STRING-DEFAULT-FRAME FOR EACH TelleLinje SHARE-LOCK
&Scoped-define OPEN-QUERY-DEFAULT-FRAME OPEN QUERY DEFAULT-FRAME FOR EACH TelleLinje SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-DEFAULT-FRAME TelleLinje
&Scoped-define FIRST-TABLE-IN-QUERY-DEFAULT-FRAME TelleLinje


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS TelleLinje.Vg TelleLinje.LopNr 
&Scoped-define ENABLED-TABLES TelleLinje
&Scoped-define FIRST-ENABLED-TABLE TelleLinje
&Scoped-Define ENABLED-OBJECTS RECT-59 RECT-60 RECT-61 Btn_OK Btn_Help ~
T-Scann FI-Strekkode FI-Tekst 
&Scoped-Define DISPLAYED-FIELDS TelleLinje.Vg TelleLinje.LopNr 
&Scoped-define DISPLAYED-TABLES TelleLinje
&Scoped-define FIRST-DISPLAYED-TABLE TelleLinje
&Scoped-Define DISPLAYED-OBJECTS T-Scann T-RabI% FI-Strekkode FI-LevKod ~
FI-Varetekst FI-LevNr FI-LevNamn FI-Storl FI-Ant FI-VVareKost1 ~
FI-VVareKost2 FI-Rabatt FI-Msg FI-Tekst 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD FiksStorl C-Win 
FUNCTION FiksStorl RETURNS CHARACTER
  ( INPUT wStorl AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getOKStorl C-Win 
FUNCTION getOKStorl RETURNS CHARACTER
  ( INPUT ipVg AS INTEGER,
    INPUT ipLopNr AS INTEGER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Help DEFAULT 
     IMAGE-UP FILE "icon/e-help":U
     LABEL "&Help" 
     SIZE 4.4 BY 1.1
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO DEFAULT 
     IMAGE-UP FILE "icon\e-exit":U
     LABEL "OK" 
     SIZE 5 BY 1.14 TOOLTIP "Avslutter programmet"
     BGCOLOR 8 .

DEFINE VARIABLE FI-Ant AS DECIMAL FORMAT "->>,>>9":U INITIAL 1 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-LevKod AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 39 BY 1 NO-UNDO.

DEFINE VARIABLE FI-LevNamn AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 39 BY 1 NO-UNDO.

DEFINE VARIABLE FI-LevNr AS INTEGER FORMAT "zzzzz9":U INITIAL 0 
     LABEL "Leverandør" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Msg AS CHARACTER FORMAT "X(256)":U 
     LABEL "Resultat" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 69 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Rabatt AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Storl AS CHARACTER FORMAT "X(256)":U 
     LABEL "Størrelse" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Strekkode AS CHARACTER FORMAT "X(256)":U 
     LABEL "Strekkode" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Tekst AS CHARACTER FORMAT "X(256)":U INITIAL "Alt-V-Gå til Vg feltet, Alt-L-Gå til løpenummerfeltet, Alt-N->Neste felt,Alt-F-Forrige felt., Alt-S-Søk" 
      VIEW-AS TEXT 
     SIZE 97 BY .62 NO-UNDO.

DEFINE VARIABLE FI-Varetekst AS CHARACTER FORMAT "X(256)":U 
     LABEL "Varetekst" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 69 BY 1 NO-UNDO.

DEFINE VARIABLE FI-VVareKost1 AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     LABEL "Varekost" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-VVareKost2 AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-59
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 98 BY .1.

DEFINE RECTANGLE RECT-60
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 98 BY .1.

DEFINE RECTANGLE RECT-61
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 97 BY 8.57.

DEFINE VARIABLE T-RabI% AS LOGICAL INITIAL yes 
     LABEL "&Rabatt i prosent" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.

DEFINE VARIABLE T-Scann AS LOGICAL INITIAL yes 
     LABEL "Strekkoderegistrering" 
     VIEW-AS TOGGLE-BOX
     SIZE 29 BY .81 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY DEFAULT-FRAME FOR 
      TelleLinje SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     Btn_OK AT ROW 1.43 COL 93.2 HELP
          "Avslutter programmet"
     Btn_Help AT ROW 1.48 COL 88.4
     T-Scann AT ROW 1.57 COL 2.2
     T-RabI% AT ROW 1.71 COL 62 NO-TAB-STOP 
     FI-Strekkode AT ROW 3.38 COL 26 COLON-ALIGNED
     TelleLinje.Vg AT ROW 4.57 COL 26 COLON-ALIGNED
          LABEL "Varegruppe/løpenummer"
          VIEW-AS FILL-IN 
          SIZE 10 BY 1
     TelleLinje.LopNr AT ROW 4.57 COL 41 COLON-ALIGNED NO-LABEL FORMAT "zzzzz9"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     FI-LevKod AT ROW 4.57 COL 56 COLON-ALIGNED NO-LABEL
     FI-Varetekst AT ROW 5.76 COL 26 COLON-ALIGNED
     FI-LevNr AT ROW 6.95 COL 26 COLON-ALIGNED
     FI-LevNamn AT ROW 6.95 COL 56 COLON-ALIGNED NO-LABEL
     FI-Storl AT ROW 8.14 COL 26 COLON-ALIGNED
     FI-Ant AT ROW 8.14 COL 41 COLON-ALIGNED HELP
          "Antall adderes til eventuelt eksisterende tellelinje." NO-LABEL
     FI-VVareKost1 AT ROW 9.33 COL 26 COLON-ALIGNED
     FI-VVareKost2 AT ROW 9.33 COL 41 COLON-ALIGNED NO-LABEL
     FI-Rabatt AT ROW 9.33 COL 56 COLON-ALIGNED NO-LABEL
     FI-Msg AT ROW 10.52 COL 26 COLON-ALIGNED
     FI-Tekst AT ROW 12.19 COL 2 NO-LABEL
     RECT-59 AT ROW 1.24 COL 1
     RECT-60 AT ROW 2.67 COL 1
     RECT-61 AT ROW 3.14 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 99.2 BY 12.1.


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
         TITLE              = "Registrering av tellelinjer"
         HEIGHT             = 12.1
         WIDTH              = 98.2
         MAX-HEIGHT         = 19.52
         MAX-WIDTH          = 147.6
         VIRTUAL-HEIGHT     = 19.52
         VIRTUAL-WIDTH      = 147.6
         MAX-BUTTON         = no
         ALWAYS-ON-TOP      = yes
         RESIZE             = no
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
   FRAME-NAME                                                           */
/* SETTINGS FOR FILL-IN FI-Ant IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-LevKod IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-LevNamn IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-LevNr IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Msg IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Rabatt IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Storl IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Tekst IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FI-Varetekst IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-VVareKost1 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-VVareKost2 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN TelleLinje.LopNr IN FRAME DEFAULT-FRAME
   EXP-FORMAT                                                           */
/* SETTINGS FOR TOGGLE-BOX T-RabI% IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN TelleLinje.Vg IN FRAME DEFAULT-FRAME
   EXP-LABEL                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME DEFAULT-FRAME
/* Query rebuild information for FRAME DEFAULT-FRAME
     _TblList          = "skotex.TelleLinje"
     _Query            is NOT OPENED
*/  /* FRAME DEFAULT-FRAME */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Registrering av tellelinjer */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
     
  /* ESC skal avbryte rutinen. 
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
  */
  APPLY "close":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Registrering av tellelinjer */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Help
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Help C-Win
ON CHOOSE OF Btn_Help IN FRAME DEFAULT-FRAME /* Help */
OR HELP OF FRAME {&FRAME-NAME}
DO: /* Call Help Function (or a simple message). */
  {winhlp.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK C-Win
ON CHOOSE OF Btn_OK IN FRAME DEFAULT-FRAME /* OK */
DO:
  {&return-ip}
  
  APPLY "close":U TO THIS-PROCEDURE.
  
  /*
  &IF DEFINED(assign-retur-verdi) &THEN
      {&assign-retur-verdi}
  &ELSE
      retur-verdi = "OK".
  &ENDIF
  */
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-Ant
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Ant C-Win
ON ALT-N OF FI-Ant IN FRAME DEFAULT-FRAME
DO:
  IF CAN-DO("1,3,10",STRING(TelleHode.TTId)) THEN  
  DO:
      APPLY "ENTRY":U TO FI-VVarekost2 IN FRAME DEFAULT-FRAME.
      RETURN NO-APPLY.
  END.
  ELSE DO:
          APPLY "ENTRY":U TO TelleLinje.Vg IN FRAME DEFAULT-FRAME.
  END.  
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Ant C-Win
ON TAB OF FI-Ant IN FRAME DEFAULT-FRAME
OR "RETURN":U OF FI-Ant
DO:
  ASSIGN 
    FI-Ant.
    
  /* Varesalg, reklamasjon og gjennkjøp. */
  IF CAN-DO("1,3,10",STRING(TelleHode.TTId)) THEN
    APPLY "entry":U TO FI-VVareKost2 IN FRAME DEFAULT-FRAME.
    
  /* Allt annet. */
  ELSE DO:   
    RUN LagrePost.
    IF RETURN-VALUE = "AVBRYT" THEN
      RETURN NO-APPLY.


    IF INPUT T-Scann THEN DO:
        ASSIGN
            FI-Strekkode:SCREEN-VALUE = ""
            FI-Ant:SCREEN-VALUE IN FRAME Default-frame = '1'.
            .
        STATUS DEFAULT "".
        APPLY "ENTRY":U TO FI-Strekkode IN FRAME DEFAULT-FRAME.
    END.
    ELSE
        APPLY "entry":U TO FI-Storl IN FRAME DEFAULT-FRAME.

    RETURN NO-APPLY.
  END.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-Rabatt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Rabatt C-Win
ON ALT-F OF FI-Rabatt IN FRAME DEFAULT-FRAME
DO:
  APPLY "entry":U TO FI-VVAreKost2 IN FRAME DEFAULT-FRAME.    
  RETURN NO-APPLY.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Rabatt C-Win
ON ALT-N OF FI-Rabatt IN FRAME DEFAULT-FRAME
DO:
  
  APPLY "entry":U TO TelleLinje.Vg IN FRAME DEFAULT-FRAME.  
  RETURN NO-APPLY.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Rabatt C-Win
ON TAB OF FI-Rabatt IN FRAME DEFAULT-FRAME
OR "RETURN":U OF FI-Rabatt
DO:
  IF NOT AVAILABLE ArtBas THEN
    DO:
      MESSAGE "Ukjent artikkel. gyldig referanse til artikkel må først angis!"
              VIEW-AS ALERT-BOX MESSAGE TITLE "Melding".
      RETURN NO-APPLY.
    END.

  RUN LagrePost.
  IF RETURN-VALUE = "AVBRYT" THEN
    RETURN NO-APPLY.
  ASSIGN
      fi-Strekkode:SCREEN-VALUE = ""
      .
  IF RETURN-VALUE = "AVBRYT" THEN
    RETURN NO-APPLY.
  APPLY "entry":U TO FI-STrekkode IN FRAME DEFAULT-FRAME.
  RETURN NO-APPLY.
  
/*   apply "entry":U to TelleLinje.Vg in frame DEFAULT-FRAME. */
/*   return no-apply.                                         */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-Strekkode
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Strekkode C-Win
ON TAB OF FI-Strekkode IN FRAME DEFAULT-FRAME /* Strekkode */
OR "RETURN" OF FI-Strekkode
DO:
    cStrekkode = TRIM(INPUT FI-Strekkode,"^").
    FI-Strekkode:SCREEN-VALUE = cStrekkode.

    IF INPUT FI-Strekkode = "" THEN
    DO:
        MESSAGE 
        "Strekkode ikke angitt."       
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN NO-APPLY.
    END.
    IF (FI-Strekkode = INPUT FI-Strekkode) AND
        INPUT FI-Strekkode <> "" AND SUBSTRING(INPUT FI-Strekkode,10,3) <> "000" THEN
    DO:
        wOk = TRUE.
        /*
        MESSAGE 
            "Samme strekkode angitt." SKIP
            "Skal antall talt på samme tellelinje økes med en?"        
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE wOk.
        IF wOk <> TRUE THEN
        DO:
            FI-Strekkode:SCREEN-VALUE = "".
            RETURN NO-APPLY.
        END.
        */
    END.
    ASSIGN 
      FI-Strekkode
      FI-Ant.

    ASSIGN /* EAN 128 kode? */ 
        lDec = DEC(FI-Strekkode)
        NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
    EAN128KODE:
    DO:
        FIND FIRST Strekkode NO-LOCK WHERE
            Strekkode.Bestillingsnummer = FI-Strekkode:SCREEN-VALUE NO-ERROR.
        IF AVAILABLE Strekkode THEN
            cStrekkode = FI-Strekkode.
    END. /* EAN128KODE */
    ELSE EANKODE: DO:
        ASSIGN
            cStrekKode = ""
            cStrekKode = IF LENGTH(INPUT FI-Strekkode) < 6 
                           THEN STRING(INPUT FI-Strekkode) 
                         ELSE IF LENGTH(INPUT FI-Strekkode) = 6 
                                THEN DYNAMIC-FUNCTION('fixChkEAN':U IN h_dproclib, 
                                     INPUT DYNAMIC-FUNCTION('EkspanderUPC':U IN h_dproclib, INPUT STRING(INPUT FI-StrekKode)))
                         ELSE STRING(INPUT FI-Strekkode,"9999999999999")
            cStrekkode = TRIM(cStrekkode).

        /* Sjekksifferkontroll */
        IF (LENGTH(cStrekkode) = 13 OR 
            LENGTH(cStrekkode) = 8) THEN
        ASSIGN cStrekkode = DYNAMIC-FUNCTION('EANprefixKonv':U IN h_dproclib, INPUT cStrekkode).
        IF LENGTH(cStrekkode) = 12 THEN
        DO:
            cStrekkode = STRING(DEC(cStrekkode),"9999999999999") NO-ERROR.
            IF ERROR-STATUS:ERROR THEN
            DO:
                BELL. BELL.
                MESSAGE 
                "Strekkode kan ikke inneholde alfanumeriske tegn (" + cStrekkode + ")."         
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
                fi-Strekkode:SCREEN-VALUE = "".
                RETURN NO-APPLY.
            END.
        END.

        /* Sjekker med nullutfylling. */
        FIND Strekkode NO-LOCK WHERE Strekkode.Kode = cStrekkode NO-ERROR.

        /* Sjekker uten nullutfylling. */
        IF NOT AVAILABLE Strekkode THEN
        DO:
            FIND Strekkode NO-LOCK WHERE Strekkode.Kode = LEFT-TRIM(cStrekkode,"0") NO-ERROR.
            IF AVAILABLE Strekkode THEN
                cStrekkode = LEFT-TRIM(cStrekkode,"0").         
        END.
    END. /* EANKODE */
    
    IF NOT AVAILABLE Strekkode THEN
    DO:
        BELL. BELL.
        MESSAGE 
        "Ugyldig strekkode (" + cStrekkode + ")."         
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
        fi-Strekkode:SCREEN-VALUE = "".
        RETURN NO-APPLY.
    END.

    FIND ArtBas OF Strekkode NO-LOCK NO-ERROR.
    IF NOT AVAILABLE ArtBas THEN
    DO:
        BELL. BELL.
        MESSAGE 
        "Strekkode funnet, men ingen artikkel knyttet til den."         
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
        fi-Strekkode:SCREEN-VALUE = "".
        RETURN NO-APPLY.
    END.
    IF ArtBas.OPris = TRUE THEN
    DO:
      MESSAGE "Artikkelen er en PLU artikkel og kan ikke legges inn på telleliste."
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
      fi-Strekkode:SCREEN-VALUE = "Artikkelen er en PLU artikkel.".
      RETURN NO-APPLY.
    END.
    FIND LevBas OF ArtBas NO-LOCK.

    DISPLAY 
      ArtBas.Vg      @ TelleLinje.Vg
      ArtBas.LopNr   @ Tellelinje.LopNr
      ArtBas.Beskr   @ FI-Varetekst
      ArtBas.LevKod  @ FI-LevKod
      LevBas.LevNr   @ FI-LevNr
      LevBas.LevNamn @ FI-LevNamn
      "1"            @ FI-Ant
      ""             @ FI-Storl
    WITH FRAME DEFAULT-FRAME.
    RUN VisPris.

    FIND StrKonv NO-LOCK WHERE
        StrKonv.StrKode = Strekkode.StrKode NO-ERROR.
/*     IF AVAILABLE StrKonv AND SUBSTRING(cStrekkode,10,3) <> "000" THEN */
    IF AVAILABLE StrKonv AND NOT (cStrekkode BEGINS "02" AND SUBSTRING(cStrekkode,10,3) = "000") THEN
    DO:
        ASSIGN 
        FI-Storl:SCREEN-VALUE = StrKonv.Storl.

        /* Varesalg, reklamasjon og gjennkjøp. */
        IF CAN-DO("1,3,10",STRING(TelleHode.TTId)) OR wNedskriv THEN
        DO:
            APPLY "entry":U TO FI-VVareKost2 IN FRAME DEFAULT-FRAME.
            RETURN NO-APPLY.
        END.

        /* Allt annet. */
        ELSE DO: 
          RUN LagrePost.
          ASSIGN
              fi-Strekkode:SCREEN-VALUE IN FRAME DEFAULT-FRAME = ""
              FI-Ant:SCREEN-VALUE IN FRAME DEFAULT-FRAME = '1'
              .
          IF RETURN-VALUE = "AVBRYT" THEN
            RETURN NO-APPLY.
          APPLY "entry":U TO FI-STrekkode IN FRAME DEFAULT-FRAME.
          RETURN NO-APPLY.
        END.
    END.
    ELSE DO:
        BELL. BELL. BELL.
        ASSIGN
            FI-Msg:SCREEN-VALUE = "Størrelse må angis.".
        APPLY "ENTRY":U TO FI-Storl.
        RETURN NO-APPLY.
    END.

    ASSIGN
        fi-Strekkode:SCREEN-VALUE IN FRAME DEFAULT-FRAME = ""
        FI-Ant:SCREEN-VALUE IN FRAME DEFAULT-FRAME = '1'
        .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Strekkode C-Win
ON VALUE-CHANGED OF FI-Strekkode IN FRAME DEFAULT-FRAME /* Strekkode */
DO:
  ASSIGN
      FI-Msg:SCREEN-VALUE = ""
      .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-VVareKost2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-VVareKost2 C-Win
ON ALT-F OF FI-VVareKost2 IN FRAME DEFAULT-FRAME
DO:
  APPLY "entry":U TO FI-Ant IN FRAME DEFAULT-FRAME.    
  RETURN NO-APPLY.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-VVareKost2 C-Win
ON ALT-N OF FI-VVareKost2 IN FRAME DEFAULT-FRAME
DO:
  
  APPLY "entry":U TO FI-Rabatt IN FRAME DEFAULT-FRAME.  
  RETURN NO-APPLY.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-VVareKost2 C-Win
ON TAB OF FI-VVareKost2 IN FRAME DEFAULT-FRAME
OR "RETURN" OF FI-VVareKost2
DO:
  IF TelleHode.TTId = 8 THEN
    DO:    
      RUN LagrePost.
      IF RETURN-VALUE = "AVBRYT" THEN
        RETURN NO-APPLY.
      APPLY "entry":U TO TelleLinje.Vg IN FRAME DEFAULT-FRAME.  
      RETURN NO-APPLY.
    END.
  ELSE DO:
      APPLY "TAB" TO SELF.
      RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TelleLinje.LopNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TelleLinje.LopNr C-Win
ON ALT-F OF TelleLinje.LopNr IN FRAME DEFAULT-FRAME /* Løpenummer */
DO:
  APPLY "entry":U TO TelleLinje.Vg IN FRAME DEFAULT-FRAME.    
  RETURN NO-APPLY.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TelleLinje.LopNr C-Win
ON ALT-N OF TelleLinje.LopNr IN FRAME DEFAULT-FRAME /* Løpenummer */
DO:
  APPLY "TAB":U TO TelleLinje.LopNr IN FRAME DEFAULT-FRAME.
  IF wNedSkriv THEN
    APPLY "entry":U TO FI-VVareKost2 IN FRAME DEFAULT-FRAME.    
  ELSE 
    APPLY "entry":U TO FI-Storl IN FRAME DEFAULT-FRAME.    
  RETURN NO-APPLY.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TelleLinje.LopNr C-Win
ON LEAVE OF TelleLinje.LopNr IN FRAME DEFAULT-FRAME /* Løpenummer */
DO:
  /*
  ASSIGN cOKStorl = getOKStorl(INPUT INPUT TelleLinje.Vg,INPUT INPUT TelleLinje.LopNr).
  ASSIGN 
      CB-Storl:LIST-ITEMS   = cOKStorl
      CB-Storl:SCREEN-VALUE = ENTRY(1,cOKStorl)
      .
  */
  FI-Storl:SCREEN-VALUE = ''.  

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TelleLinje.LopNr C-Win
ON TAB OF TelleLinje.LopNr IN FRAME DEFAULT-FRAME /* Løpenummer */
OR "RETURN":U OF TelleLinje.LopNr 
DO:
  IF int(INPUT TelleLinje.LopNr) > 0 THEN
    DO:
      IF NOT CAN-FIND(ArtBas WHERE
         ArtBas.Vg    = INPUT TelleLinje.Vg AND
         ArtBas.LopNr = INPUT TelleLinje.LopNr) THEN
        DO:
          MESSAGE "Ukjent artikkel!" VIEW-AS ALERT-BOX MESSAGE TITLE "Melding".
          RETURN NO-APPLY.
        END.
        
      FIND ArtBas NO-LOCK WHERE
        ArtBas.Vg    = INPUT TelleLinje.Vg AND
        ArtBas.LopNr = INPUT TelleLinje.LopNr NO-ERROR.

/*       IF ArtBas.Lager = FALSE THEN                                                    */
/*       DO:                                                                             */
/*         MESSAGE "Artikkelen er ikke lagerstyrt og kan ikke legges inn på telleliste." */
/*           VIEW-AS ALERT-BOX INFO BUTTONS OK.                                          */
/*         RETURN NO-APPLY.                                                              */
/*       END.                                                                            */
      IF ArtBas.OPris = TRUE THEN
      DO:
        MESSAGE "Artikkelen er en PLU artikkel og kan ikke legges inn på telleliste."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN NO-APPLY.
      END.
      FIND LevBas OF ArtBas NO-LOCK.
      DISPLAY 
        ArtBas.LevKod  @ FI-LevKod
        LevBas.LevNr   @ FI-LevNr
        LevBas.LevNamn @ FI-LevNamn
        ArtBas.Beskr   @ FI-Varetekst   
      WITH FRAME DEFAULT-FRAME.
      RUN VisPris.

      IF wNedSkriv THEN
        DO:
/*           assign wStorl = "".         */
/*           display                     */
/*             wStorl @ TelleLinje.Storl */
/*           with frame DEFAULT-FRAME.   */
          APPLY "ENTRY":U TO FI-VVAreKost2 IN FRAME DEFAULT-FRAME.
        END.
      ELSE
        APPLY "ENTRY":U TO FI-Storl.
      RETURN NO-APPLY.
    END.
  ELSE
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TelleLinje.LopNr C-Win
ON VALUE-CHANGED OF TelleLinje.LopNr IN FRAME DEFAULT-FRAME /* Løpenummer */
DO:
    ASSIGN
      FI-Msg:SCREEN-VALUE = ""
      .

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME T-RabI%
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL T-RabI% C-Win
ON VALUE-CHANGED OF T-RabI% IN FRAME DEFAULT-FRAME /* Rabatt i prosent */
DO:
  RUN VisPris.
  /*
  apply "ENTRY":U to TelleLinje.Vg in frame DEFAULT-FRAME.
  */
  RETURN NO-APPLY.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME T-Scann
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL T-Scann C-Win
ON VALUE-CHANGED OF T-Scann IN FRAME DEFAULT-FRAME /* Strekkoderegistrering */
DO:
    ASSIGN T-Scann.
  RUN setscann(INPUT INPUT T-Scann).
  IF INPUT T-Scann THEN
      APPLY "entry" TO FI-Strekkode IN FRAME Default-Frame.
  ELSE
      APPLY "entry":U TO TelleLinje.Vg IN FRAME DEFAULT-FRAME.
  IF T-Scann THEN
      FI-Tekst:SCREEN-VALUE = "Avslutt med Alt-F4".
  ELSE
      FI-Tekst:SCREEN-VALUE = "Alt-V-Gå til Vg feltet, Alt-L-Gå til løpenummerfeltet, Alt-N->Neste felt,Alt-F-Forrige felt., Alt-S-Søk".

  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TelleLinje.Vg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TelleLinje.Vg C-Win
ON ALT-N OF TelleLinje.Vg IN FRAME DEFAULT-FRAME /* Varegruppe/løpenummer */
DO:
  APPLY "entry":U TO TelleLinje.LopNr IN FRAME DEFAULT-FRAME.    
  RETURN NO-APPLY.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TelleLinje.Vg C-Win
ON TAB OF TelleLinje.Vg IN FRAME DEFAULT-FRAME /* Varegruppe/løpenummer */
OR "RETURN":U OF TelleLinje.Vg 
DO:
  IF int(INPUT TelleLinje.Vg) > 0 THEN
    DO:
      IF NOT CAN-FIND(VarGr WHERE
         VarGr.Vg = INPUT TelleLinje.Vg) THEN
        DO:
          MESSAGE "Ukjent varegruppe!" VIEW-AS ALERT-BOX MESSAGE TITLE "Melding".
          RETURN NO-APPLY.
        END.
    END.
  ELSE RETURN NO-APPLY.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TelleLinje.Vg C-Win
ON VALUE-CHANGED OF TelleLinje.Vg IN FRAME DEFAULT-FRAME /* Varegruppe/løpenummer */
DO:
    ASSIGN
      FI-Msg:SCREEN-VALUE = ""
      .

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

{syspara.i 1 2 4 wEDB-System}
{syspara.i 1 1 16 wDecimaler}
ASSIGN wTabell = "ArtBas".
{syspara.i 5 1 1 wCl INT}
FIND clButiker NO-LOCK WHERE
  clButiker.Butik = wCl.

{syspara.i 4 1 3 piInt INT}
IF piInt = 0 THEN
    T-Scann = TRUE.
ELSE
    t-Scann  = FALSE.

FIND TelleHode NO-LOCK WHERE
  ROWID(TelleHode) = wTelleHodeROWID NO-ERROR.
IF NOT AVAILABLE TelleHode THEN
  DO:
    MESSAGE "Ukjent tellehode!"
      VIEW-AS ALERT-BOX.
    RETURN.
  END.

{syspara.i 4 1 2 wTekst}
IF TelleHode.TTId = int(wTekst) THEN
  wNedskriv = TRUE.
ELSE
  wNedskriv = FALSE.

FIND {&br-tabell} NO-LOCK WHERE
  ROWID({&br-tabell}) = wRowId NO-ERROR.
IF AVAILABLE {&br-tabell} THEN 
  DO: 
    {&FinnRelatertePoster}  
  END.
ON 'Alt-S':U OF C-Win ANYWHERE 
DO:
    ASSIGN wArtikkelNr = ?.
    RUN d-hsok.w (OUTPUT wArtikkelNr,"NEI"). /* JA = sök mot vpistrekkode */
    IF wArtikkelNr = ? THEN
        RETURN NO-APPLY.
    FIND ArtBas NO-LOCK WHERE
        ArtBas.ArtikkelNr = wArtikkelNr NO-ERROR.
    IF NOT AVAILABLE ArtBas THEN
        RETURN NO-APPLY.
    DISPLAY 
        ArtBas.Vg    @ TelleLinje.Vg
        ArtBas.LopNr @ TelleLinje.LopNr
        WITH FRAME Default-Frame.
    FI-Storl:SCREEN-VALUE IN FRAME Default-Frame = ''.
    APPLY "TAB":U TO TelleLinje.Vg IN FRAME DEFAULT-FRAME.
    APPLY "TAB":U TO TelleLinje.LopNr IN FRAME DEFAULT-FRAME.
    RETURN.
END.
ON "Alt-L","Alt-l" OF C-Win ANYWHERE
  DO:
    APPLY "entry":U TO TelleLinje.LopNr IN FRAME DEFAULT-FRAME.
    RETURN NO-APPLY.  
  END.

ON "Alt-V","Alt-v" OF C-Win ANYWHERE
  DO:
    APPLY "entry":U TO TelleLinje.Vg IN FRAME DEFAULT-FRAME.
    RETURN NO-APPLY.  
  END.

ON ALT-R OF C-Win ANYWHERE 
  DO:
    ASSIGN
      T-RabI% = IF T-RabI% = FALSE THEN TRUE ELSE FALSE.
    DISPLAY
      T-RabI%
    WITH FRAME DEFAULT-FRAME.  
    RUN VisPris.
    APPLY "ENTRY":U TO TelleLinje.Vg IN FRAME DEFAULT-FRAME.
    RETURN NO-APPLY.  
  END.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  {lng.i} 

  FIND {&br-tabell} NO-LOCK WHERE
    ROWID({&br-tabell}) = wRowId NO-ERROR.
  ASSIGN
    T-RabI% = TRUE.
  DISPLAY
    T-RabI%
  WITH FRAME DEFAULT-FRAME.  

  RUN VisPost.
  
  ASSIGN
    FI-Storl:sensitive = CAN-DO("1,2,3,4,5,6,7,9,10,11",STRING(TelleHode.TTId))
    FI-ant:sensitive           = CAN-DO("1,2,3,4,5,6,7,9,10,11",STRING(TelleHode.TTId))
    FI-VVareKost2:sensitive    = CAN-DO("1,3,8,10",STRING(TelleHode.TTId))
    FI-Rabatt:sensitive        = CAN-DO("1,3,8,10",STRING(TelleHode.TTId))
    T-RabI%:sensitive          = CAN-DO("1,3,8,10",STRING(TelleHode.TTId))
    FI-VVareKost1:label        = IF CAN-DO("1,3,10",STRING(TelleHode.TTId))
                                   THEN "Pris"
                                 ELSE IF CAN-DO("5",STRING(TelleHode.TTId))
                                   THEN "Varekost"
                                 ELSE
                                   "Vektet varekost" .    
  DISPLAY T-Scann WITH FRAME Default-Frame.
  RUN setscann (INPUT T-Scann).
  IF T-Scann THEN
      APPLY "entry" TO FI-Strekkode IN FRAME Default-Frame.
  ELSE
      APPLY "entry":U TO TelleLinje.Vg IN FRAME DEFAULT-FRAME.

  IF T-Scann THEN
      FI-Tekst:SCREEN-VALUE = "Avslutt med Alt-F4".
  ELSE
      FI-Tekst:SCREEN-VALUE = "Alt-V-Gå til Vg feltet, Alt-L-Gå til løpenummerfeltet, Alt-N->Neste felt,Alt-F-Forrige felt., Alt-S-Søk".

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
  DISPLAY T-Scann T-RabI% FI-Strekkode FI-LevKod FI-Varetekst FI-LevNr 
          FI-LevNamn FI-Storl FI-Ant FI-VVareKost1 FI-VVareKost2 FI-Rabatt 
          FI-Msg FI-Tekst 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  IF AVAILABLE TelleLinje THEN 
    DISPLAY TelleLinje.Vg TelleLinje.LopNr 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-59 RECT-60 RECT-61 Btn_OK Btn_Help T-Scann FI-Strekkode 
         TelleLinje.Vg TelleLinje.LopNr FI-Tekst 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LagrePost C-Win 
PROCEDURE LagrePost :
/*------------------------------------------------------------------------------
  Purposemessage
       
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR wVVareKost AS DEC NO-UNDO.
DEF VAR wStorl     AS CHAR NO-UNDO.
DEF VAR wNyLinje   AS LOG  INITIAL FALSE NO-UNDO.
DEF VAR wLagAnt    LIKE LAger.LagAnt NO-UNDO.
DEF VAR wLagVerdi  AS DEC NO-UNDO.

DO WITH FRAME DEFAULT-FRAME TRANSACTION:

  IF NOT AVAILABLE ArtBas THEN
    DO:
      MESSAGE "Ukjent artikkel. gyldig referanse til artikkel må angis. " 
              VIEW-AS ALERT-BOX MESSAGE TITLE "Melding".
      RETURN NO-APPLY.
    END.
    
  /* NB: Det skal ikke være kontroll på at størrelsen ligger innenfor tillatte størrelser på artikkelen. */
  ASSIGN
    wStorl = FI-Storl:SCREEN-VALUE.
  IF VALID-HANDLE(wLibHandle) THEN
    DO:
      RUN FiksStorl    IN wLibHandle (INPUT-OUTPUT wStorl).
      IF AVAILABLE ArtBas THEN
        DO:
          RUN StrTypeSjekk IN wLibHandle (wStorl,ArtBas.StrTypeId).
          FI-Storl:SCREEN-VALUE = wStorl.
          /*
          IF RETURN-VALUE = "AVBRYT" THEN
            DO:
              MESSAGE "Ugyldig størrelse. Størrelsen er ikke definert i størrelsestypen" SKIP
                      "som artikkelen er knyttet til."
                      VIEW-AS ALERT-BOX MESSAGE TITLE "Melding".
              RETURN "AVBRYT".
            END.
          */
        END.
    END.

  /* Nedskrivning krever at artikkelen finnes og har et lager som kan nedskrives. */
  IF wNedSkriv THEN
    DO:
      /* Telling pr. butikk. Sjekker mot butikken lager. */
      IF wAlleBut = FALSE THEN
        DO:
          FIND Lager OF ArtBas NO-LOCK WHERE
            Lager.Butik = wButik NO-ERROR.
          IF NOT AVAILABLE Lager THEN
            DO:
              MESSAGE "Artikkelen har ikke noe på lager eller er ikke lagerstyrt. Kan ikke nedskrives."
                      VIEW-AS ALERT-BOX MESSAGE TITLE "Melding".
              RETURN NO-APPLY.
            END.
          ELSE 
            wLagAnt = Lager.LagAnt.
        END.
      /* Telling for alle butikker. Sjekker mot sum lagerantall alle butikker. */
      ELSE DO:
        ASSIGN
          wLagAnt   = 0
          wLagVerdi = 0.
        FOR EACH Lager OF ArtBas NO-LOCK:
          ASSIGN
            wLagVerdi = wLagVerdi + (Lager.LagAnt * Lager.VVareKost)
            wLagAnt = wLagAnt + Lager.LagAnt.
        END.
      END.
    
      IF wLagant = 0 THEN
        DO:
          MESSAGE "Artikkelen 0 - null i lagerantall. Kan ikke nedskrives."
                  VIEW-AS ALERT-BOX MESSAGE TITLE "Melding".
          RETURN NO-APPLY.
        END.
    END.  
    
  FIND Farg OF ArtBas NO-LOCK NO-ERROR.

  IF wNedSkriv THEN
    DO:
      ASSIGN
        wStorl = "".
      FIND FIRST TelleLinje EXCLUSIVE-LOCK WHERE
        TelleLinje.TelleNr    = TelleHode.TelleNr AND
        TelleLinje.ArtikkelNr = ArtBas.ArtikkelNr AND
        TelleLinje.Butik      = wButik NO-ERROR.
    END.
  ELSE DO:
      FIND TelleLinje EXCLUSIVE-LOCK WHERE
        TelleLinje.TelleNr    = TelleHode.TelleNr AND
        TelleLinje.ArtikkelNr = ArtBas.ArtikkelNr AND
        TelleLinje.Butik      = wButik AND
        TelleLinje.Storl      = wStorl NO-ERROR.
  END.
    
  IF NOT AVAILABLE TelleLinje THEN
    NY-POST:
    DO:
      FIND Lager OF ArtBas NO-LOCK WHERE
        Lager.Butik = wButik NO-ERROR.

      /* Skal det oppdateres som varesalg, må pris hentes. */
      IF CAN-DO("1,3,10",STRING(TelleHode.TTId)) THEN
        DO:
          FIND Butiker NO-LOCK WHERE
            Butiker.Butik = wButik.
          
          FIND ArtPris NO-LOCK WHERE
            ArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND
            ArtPris.ProfilNr   = Butiker.ProfilNr NO-ERROR.
            
          /* Henter pris fra sentrallageret hvis den ikke finnes for butikken. */
          IF NOT AVAILABLE ArtPris THEN
            DO:
              FIND ArtPris NO-LOCK WHERE
                ArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND
                ArtPris.ProfilNr   = clButiker.ProfilNr NO-ERROR.
            END.
          IF AVAILABLE ArtPris THEN
            wVVAreKost = ArtPris.Pris[IF ArtPris.Tilbud THEN 2 ELSE 1].
        END.
      /* Setter gammel varekost for nedskrivning. */
      ELSE IF CAN-DO("8",STRING(TelleHode.TTId)) THEN
        DO:
          ASSIGN
            wVVareKost = (wLagVerdi / wLagAnt)
            wVVareKost = IF wVVareKost = ? THEN 0 ELSE wVVareKost.
        END.
      ELSE DO:
        IF ArtBas.Lager = TRUE THEN 
        DO:
            IF AVAILABLE Lager THEN
              wVVareKost = Lager.VVareKost.
            ELSE 
              wVVareKost = 0.
            IF wVVareKost = 0 THEN
                wVVareKost = INPUT FI-VVareKost1.
        END.
        ELSE DO:
            wVVareKost = INPUT FI-VVareKost1.
        END.
      END.  
      CREATE TelleLinje.
      ASSIGN
        wNyLinje = TRUE
        TelleLinje.TelleNr    = TelleHode.TelleNr 
        TelleLinje.ArtikkelNr = ArtBas.ArtikkelNr
        Tellelinje.Beskr      = ArtBas.Beskr
        TelleLinje.butik      = wButik
        TelleLinje.Vg
        TelleLinje.LopNr
        TelleLinje.Storl      = wStorl
        TelleLinje.Kode       = INPUT FI-STrekkode.
        TelleLinje.VgLopNr    = STRING(TelleLinje.Vg,">>>>>9") + "/" + 
                                string(TelleLinje.LopNr,">>>>>9").
        
      IF wNedskriv = FALSE THEN
        FIND ArtLag NO-LOCK WHERE
          ArtLag.Artikkelnr = TelleLinje.artikkelnr AND
          ArtLag.Butik = TelleLinje.Butik AND
          ArtLag.Storl = wStorl NO-ERROR.

      /* Øvrig informasjon. */
      ASSIGN
        TelleLinje.LevKod     = ArtBas.LevKod
        TelleLinje.LevFargKod = IF ArtBas.LevFargKod <> ""
                                  THEN ArtBas.LevFargKod
                                ELSE IF AVAILABLE Farg 
                                  THEN Farg.FarBeskr
                                ELSE ""
        TelleLinje.AntallPar  = IF CAN-DO("3,10",STRING(TelleHode.TTId))
                                  THEN 0
                                ELSE IF wNedSkriv 
                                  THEN wLagAnt
                                ELSE IF AVAILABLE ArtLag 
                                   THEN Artlag.Lagant
                                ELSE 0
        TelleLinje.AntallTalt = 0
        TelleLinje.VVareKost  = wVVAreKost
        TelleLinje.NedSkrevet = IF CAN-DO("1,3,8,10",STRING(TelleHode.TTId))
                                  THEN INPUT FI-VVareKost2
                                ELSE wVVareKost
        TelleLinje.OpprVerdi  = TelleLinje.AntallPar  * wVVareKost
        TelleLinje.OpptVerdi  = TelleLinje.AntallTalt * TelleLinje.NedSkrevet
        TelleLinje.LevNr      = ArtBas.LevNr
        TelleLinje.Sasong     = ArtBas.SaSong
        TelleLinje.Farg       = ArtBas.Farg
        TelleLinje.MatKod     = ArtBas.MatKod
        TelleLinje.VgLopNr    = TRIM(STRING(ArtBas.Vg,">>>>>>9")) + "/" + trim(STRING(ArtBas.LopNr,">>>>>9")).
        IF NOT CAN-FIND(FIRST KonvReg WHERE
            KonvReg.EDB-System = wEDB-System AND
            KonvReg.Tabell     = wTabell     AND
            KonvReg.EkstId     = STRING(ArtBas.ArtikkelNr) + "," + 
                                 string(wButik) AND 
            KonvReg.InterntId  = STRING(ArtBas.ArtikkelNr) + "," + 
                                 string(wButik)) THEN
          DO:
            CREATE KonvReg.
            ASSIGN
              KonvReg.EDB-System = wEDB-System 
              KonvReg.Tabell     = wTabell     
              KonvReg.EkstId     = STRING(ArtBas.ArtikkelNr) + "," + 
                                   string(wButik)
              KonvReg.InterntId  = STRING(ArtBas.ArtikkelNr) + "," + 
                                   string(wButik)
              KonvReg.DivTekst   = STRING(TelleHode.TelleNr).
          END. 
    END. /* NY-POST */
  /* Oppdaterer med ny info */
  ASSIGN
    TelleLinje.AntallTalt = IF CAN-DO("3,10",STRING(TelleHode.TTId))
                              THEN INT(FI-Ant:SCREEN-VALUE) * -1
                            ELSE 
                              TelleLinje.AntallTalt + (IF wNedskriv 
                                                         THEN 0
                                                         ELSE INPUT FI-Ant)
    TelleLinje.AntallDiff = TelleLinje.AntallPar  - TelleLinje.AntallTalt
    TelleLinje.VVareKost = IF CAN-DO("1,3,10",STRING(TelleHode.TTId))
                              THEN INPUT FI-VVareKost2
                            ELSE TelleLinje.VVareKost
    TelleLinje.Nedskrevet = IF CAN-DO("8",STRING(TelleHode.TTId))
                              THEN INPUT FI-VVareKost2
                            ELSE TelleLinje.Nedskrevet
    TelleLinje.OpptVerdi  = TelleLinje.AntallTalt * TelleLinje.VVareKost
    TelleLinje.OpprVerdi  = TelleLinje.AntallPar  * TelleLinje.VVareKost
    TelleLinje.RabKr      = IF CAN-DO("1,3,10",STRING(TelleHode.TTId)) 
                              THEN INPUT FI-Rabatt
                            ELSE 0
    /* Rabatt er angitt i % */
    TelleLinje.RabKr      = IF INPUT T-RabI% THEN
                              (TelleLinje.VVareKost * INPUT FI-Rabatt) / 100
                            ELSE
                              TelleLinje.RabKr.
  IF wNedSkriv THEN
    TelleLinje.VerdiDiff  = (TelleLinje.VVareKost - TelleLinje.NedSkrevet) * TelleLinje.AntallDiff.    
  ELSE
    TelleLinje.VerdiDiff  = TelleLinje.AntallDiff * TelleLinje.VVareKost.

  /* Oppdaterer browser. */
  IF VALID-HANDLE(wParentHandle) THEN
    DO:
      IF wNyLinje THEN 
      DO:
          RUN NyLinje IN wParentHandle (ROWID(TelleLinje)).
          IF RETURN-VALUE = "Ok" THEN
              FI-Msg:SCREEN-VALUE = "Ny tellelinje opprettet...(" + INPUT FI-Strekkode + ")".
          ELSE
              FI-Msg:SCREEN-VALUE = "Feilet ved opprettelse av ny tellelinje.(" + INPUT FI-Strekkode + ")".
      END.
      ELSE DO:
          RUN RefreshBrowser IN wParentHandle (ROWID(TelleLinje)).  
          IF RETURN-VALUE = "Ok" THEN
              FI-Msg:SCREEN-VALUE = "Tellelinje oppdatert...(" + INPUT FI-Strekkode + ")".
          ELSE
              FI-Msg:SCREEN-VALUE = "Feilet ved oppdatering av tellelinje.(" + INPUT FI-Strekkode + ")".
      END.
    END.
  

  RELEASE TelleLinje.    

END. /* TRANSACTION */    


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setscann C-Win 
PROCEDURE setscann :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER plScann AS LOG NO-UNDO.

  DO WITH FRAME DEFAULT-FRAME:
      IF plScann THEN
          ASSIGN
          TelleLinje.Vg:SENSITIVE    =FALSE
          TelleLinje.LopNr:SENSITIVE = FALSE
          FI-Strekkode:SENSITIVE     = TRUE
          FI-Storl:SENSITIVE         = FALSE
          .
      ELSE
          ASSIGN
          TelleLinje.Vg:SENSITIVE    = TRUE 
          TelleLinje.LopNr:SENSITIVE = TRUE
          FI-Strekkode:SENSITIVE     = FALSE
          FI-Storl:SENSITIVE         = TRUE
          .
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE VisPost C-Win 
PROCEDURE VisPost :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  IF AVAILABLE TelleLinje THEN
    DO:
      FIND ArtBas NO-LOCK WHERE
        ArtBas.ArtikkelNr = TelleLinje.ArtikkelNr NO-ERROR.
      FIND LevBas OF ArtBas NO-LOCK NO-ERROR.

      DISPLAY 
        ArtBas.LevKod WHEN AVAILABLE ArtBas @ FI-LevKod
        LevBas.LevNr  WHEN AVAILABLE LEvBAs @ FI-LevNr
        LevBas.LevNamn WHEN AVAILABLE LevBas @ FI-LevNamn
        TelleLinje.Vg    
        TelleLinje.LopNr 
        ArtBas.Beskr @ FI-Varetekst
        1 @ FI-Ant
      WITH FRAME DEFAULT-FRAME.
  
      DO WITH FRAME DEFAULT-FRAME:
          ASSIGN 
              FI-Storl:SCREEN-VALUE = TelleLinje.Storl
              .
      END.
      RUN VisPris.
    END.
  ELSE 
    DISPLAY 
      "" @ FI-LevKod
      "" @ FI-LevNr
      "" @ FI-LevNamn
      "" @ TelleLinje.Vg    
      "" @ TelleLinje.LopNr 
/*       "" @ TelleLinje.Storl */
      1 @ FI-Ant
      "" @ FI-VVareKost1
      "" @ FI-VVareKost2
      "" @ FI-Varetekst
    WITH FRAME DEFAULT-FRAME.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE VisPris C-Win 
PROCEDURE VisPris :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR wRab% AS DEC NO-UNDO.

DO WITH FRAME DEFAULT-FRAME:
IF INPUT FI-Storl = "" THEN
  DO:
    FIND FIRST TelleLinje NO-LOCK WHERE
      TelleLinje.TelleNr    = TelleHode.TelleNr AND
      TelleLinje.ArtikkelNr = ArtBas.ArtikkelNr AND
      TelleLinje.Butik      = wButik  NO-ERROR.
  END.
ELSE DO:
  FIND TelleLinje NO-LOCK WHERE
    TelleLinje.TelleNr    = TelleHode.TelleNr AND
    TelleLinje.ArtikkelNr = ArtBas.ArtikkelNr AND
    TelleLinje.Butik      = wButik AND
    TelleLinje.Storl      = FI-Storl:SCREEN-VALUE NO-ERROR.
END.  
IF AVAILABLE TelleLinje THEN
  DO:
    ASSIGN
      wRab% = (TelleLinje.RabKr / TelleLinje.NedSkrevet) * 100
      wRab% = IF wRab% = ? THEN 0 ELSE wRab%.
  END.

FIND Lager OF ArtBAs NO-LOCK WHERE
  Lager.Butik = wButik NO-ERROR.
/*if can-do("1,3,10",string(TelleHode.TTId)) then*/
  DO:  
    /* Butikkens pris */
    FIND Butiker NO-LOCK WHERE
      Butiker.Butik = wButik NO-ERROR.
    IF AVAILABLE Butiker THEN
      FIND ArtPris NO-LOCK WHERE
        ArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND
        ArtPris.ProfilNr   = Butiker.ProfilNr NO-ERROR.
    /* Sentrallagerets pris, hvis det ikke er pris for butikken. */
    IF NOT AVAILABLE ArtPris THEN
      DO:
        FIND ArtPris NO-LOCK WHERE
          ArtPris.ArtikkelNr = ArtBAs.ArtikkelNr AND
          ArtPris.ProfilNr   = clButiker.ProfilNr NO-ERROR.
      END.
  END.    

/* Aktuell pris inklusive mva.           */
/* Varesalg, Kundereklamasjon, Gjennkjøp */
IF CAN-DO("1,3,10",STRING(TelleHode.TTId)) THEN
  DO:
    /* Er linjen kjent - endring, benyttes det som ligger på linjen. */
    IF AVAILABLE TelleLinje THEN
      DISPLAY
        TelleLinje.VVareKost     @ FI-VVareKost1  
        TelleLinje.Nedskrevet    @ FI-VVareKost2
        TelleLinje.RabKr         @ FI-Rabatt
        wRab% WHEN INPUT T-RabI% @ FI-Rabatt
      WITH FRAME DEFAULT-FRAME.  
    ELSE DO:
      IF AVAILABLE ArtPris THEN
        DISPLAY
          ArtPris.Pris[IF ArtPris.Tilbud THEN 2 ELSE 1] @ FI-VVareKost1  
          ArtPris.Pris[IF ArtPris.Tilbud THEN 2 ELSE 1] @ FI-VVareKost2
          0 @ FI-Rabatt
        WITH FRAME DEFAULT-FRAME.  
      ELSE   
        DISPLAY
          ""  @ FI-VVareKost1  
          ""  @ FI-VVareKost2
          ""  @ FI-Rabatt
        WITH FRAME DEFAULT-FRAME.  
    END.
  END.  

/* Aktuell varekost fra kalkyle */
/* Varekjøp                     */
IF CAN-DO("5",STRING(TelleHode.TTId)) THEN
  DO:
    /* Er linjen kjent - endring, benyttes det som ligger på linjen. */
    IF AVAILABLE TelleLinje THEN
      DISPLAY
        TelleLinje.VVareKost  @ FI-VVareKost1  
        TelleLinje.Nedskrevet @ FI-VVareKost2
        ""  @ FI-Rabatt
      WITH FRAME DEFAULT-FRAME.  
    ELSE DO:
      IF AVAILABLE ArtPris THEN
        DISPLAY
          ArtPris.VareKost[IF ArtPris.Tilbud THEN 2 ELSE 1] @ FI-VVareKost1  
          ArtPris.VareKost[IF ArtPris.Tilbud THEN 2 ELSE 1] @ FI-VVareKost2
          ""  @ FI-Rabatt
        WITH FRAME DEFAULT-FRAME.  
      ELSE   
        DISPLAY
          ""  @ FI-VVareKost1  
          ""  @ FI-VVareKost2
          ""  @ FI-Rabatt
        WITH FRAME DEFAULT-FRAME.  
    END.
  END.  

/* Brekkasje, Lagerreklam, Overfør, Lagerjustering, Nedskrivning, Svinn, Int.forbruk */
ELSE IF CAN-DO("2,4,6,7,8,9,11",STRING(TelleHode.TTId)) THEN
  DO:
    IF AVAILABLE TelleLinje THEN
      DISPLAY
        TelleLinje.VVareKost  @ FI-VVareKost1  
        TelleLinje.Nedskrevet @ FI-VVareKost2
        ""  @ FI-Rabatt
      WITH FRAME DEFAULT-FRAME.  
    ELSE DO:
      IF AVAILABLE Lager AND ArtBas.Lager = TRUE THEN
      DO:
          DISPLAY
            Lager.VVareKost @ FI-VVareKost1  
            Lager.VVareKost @ FI-VVareKost2
            ""  @ FI-Rabatt
          WITH FRAME DEFAULT-FRAME.  
          IF Lager.VVareKost = 0 THEN
              DISPLAY
                  ArtPris.VareKost[IF ArtPris.Tilbud THEN 2 ELSE 1] @ FI-VVareKost1  
                  ArtPris.VareKost[IF ArtPris.Tilbud THEN 2 ELSE 1] @ FI-VVareKost2
                  ""  @ FI-Rabatt
              WITH FRAME DEFAULT-FRAME.  
      END.
      ELSE   
        DISPLAY
            ArtPris.VareKost[IF ArtPris.Tilbud THEN 2 ELSE 1] @ FI-VVareKost1  
            ArtPris.VareKost[IF ArtPris.Tilbud THEN 2 ELSE 1] @ FI-VVareKost2
            ""  @ FI-Rabatt
        WITH FRAME DEFAULT-FRAME.  
    END. 
  END.
END.  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION FiksStorl C-Win 
FUNCTION FiksStorl RETURNS CHARACTER
  ( INPUT wStorl AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  Formaterer størrelsen korrekt etter SkoTex standard.
    Notes:  
------------------------------------------------------------------------------*/

 ASSIGN
    wStorl = TRIM(wStorl)
    wStorl = CAPS(wStorl)
    wStorl = IF (LENGTH(wStorl) = 1 OR 
                 LENGTH(wStorl) = 3
                 ) 
                then " " + wStorl
                else wStorl.          

  /* Bytter ut eventuelle comma med punkt. */
  IF INDEX(wStorl,",") <> 0 THEN
    OVERLAY(wStorl, INDEX(wStorl,","), 1, "CHARACTER") = ".".

  RETURN wStorl.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getOKStorl C-Win 
FUNCTION getOKStorl RETURNS CHARACTER
  ( INPUT ipVg AS INTEGER,
    INPUT ipLopNr AS INTEGER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cString AS CHARACTER  NO-UNDO.

  /*
  FOR EACH ArtLag WHERE ArtLag.Vg = ipVg AND ArtLag.LopNr = ipLopNr NO-LOCK.
      IF NOT CAN-DO(cString,ArtLag.Storl) THEN
          ASSIGN cString = cString + (IF cString = "" THEN "" ELSE ",") + ArtLag.Storl.
  END.
  */
  FIND bArtBas NO-LOCK WHERE
      bArtBas.Vg    = ipVg AND
      bArtBas.LopNr = ipLopNr NO-ERROR.
  IF AVAILABLE bArtBas THEN
  DO:
      FOR EACH StrTStr WHERE 
          StrTStr.StrTypeId = bArtBas.StrTypeId NO-LOCK:
          IF NOT CAN-DO(cString,StrTStr.SoStorl) THEN
              ASSIGN cString = cString + (IF cString = "" THEN "" ELSE ",") + StrTStr.SoStorl.
      END.
  END.
  RETURN cString.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

