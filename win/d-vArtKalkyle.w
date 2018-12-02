&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
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
  DEFINE VAR wArtBasRecid   AS RECID NO-UNDO.
&ELSE
  DEFINE INPUT PARAMETER wArtBasRecid   AS RECID NO-UNDO.
&ENDIF

DEF VAR wRetur-Verdi AS CHAR INITIAL "AVBRYT" NO-UNDO.
DEF VAR wSjekkStreng AS CHAR                  NO-UNDO.
DEF VAR wButik       AS INT                   NO-UNDO.
DEF VAR wCl          AS INT                   NO-UNDO.
DEF VAR wSvar        AS LOG INITIAL FALSE     NO-UNDO.

{runlib.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-44 FI-ProfilNr Btn_OK Btn_Cancel ~
Btn_Help BUTTON-SokProfil 
&Scoped-Define DISPLAYED-OBJECTS FI-ProfilNr FI-Beskrivelse FI-ValPris ~
FI-ValKod FI-InnPris FI-Rab1 FI-Rab1% FI-Rab2 FI-Rab2% FI-Frakt FI-Frakt% ~
FI-DivKost FI-DivKost% FI-Rab3 FI-Rab3% FI-VareKost FI-DB FI-DB% FI-MVA ~
FI-Mva% FI-Pris FI-EuPris T-Manuel 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD KalkStreng Dialog-Frame 
FUNCTION KalkStreng RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-Forrige  NO-FOCUS
     LABEL "&Forrige" 
     SIZE 13 BY 1.14.

DEFINE BUTTON B-Lagre  NO-FOCUS
     LABEL "&Lagre" 
     SIZE 13 BY 1.14.

DEFINE BUTTON B-Neste  NO-FOCUS
     LABEL "N&este" 
     SIZE 13 BY 1.14.

DEFINE BUTTON B-Slett  NO-FOCUS
     LABEL "&Slett" 
     SIZE 13 BY 1.14.

DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Avbryt" 
     SIZE 13 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_Help 
     LABEL "&Hjelp" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 13 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON BUTTON-SokProfil 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE VARIABLE FI-Beskrivelse AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 41 BY 1 NO-UNDO.

DEFINE VARIABLE FI-DB AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "DB (+)" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1 NO-UNDO.

DEFINE VARIABLE FI-DB% AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-DivKost AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Div. kost (+)" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1 NO-UNDO.

DEFINE VARIABLE FI-DivKost% AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-EuPris AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Pris (Euro)" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Frakt AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Frakt (+)" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Frakt% AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-InnPris AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Inköpspris" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1 NO-UNDO.

DEFINE VARIABLE FI-MVA AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Mva (+)" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Mva% AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Pris AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Pris" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1 NO-UNDO.

DEFINE VARIABLE FI-ProfilNr AS INTEGER FORMAT ">>>>>>9":U INITIAL 0 
     LABEL "Prisprofil" 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Rab1 AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Rabatt 1 (-)" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Rab1% AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Rab2 AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Rabatt 2 (-)" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Rab2% AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Rab3 AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Rabatt 3 (-)" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Rab3% AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-ValKod AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-ValPris AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Valutapris" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1 NO-UNDO.

DEFINE VARIABLE FI-VareKost AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Varekost" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-44
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 74 BY 16.19.

DEFINE VARIABLE T-Manuel AS LOGICAL INITIAL NO 
     LABEL "Manuel" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     B-Forrige AT ROW 5.05 COL 62 NO-TAB-STOP 
     FI-ProfilNr AT ROW 1.95 COL 16 COLON-ALIGNED
     FI-Beskrivelse AT ROW 1.95 COL 32 COLON-ALIGNED NO-LABEL
     FI-ValPris AT ROW 4.14 COL 16 COLON-ALIGNED
     FI-ValKod AT ROW 4.14 COL 32 COLON-ALIGNED NO-LABEL
     FI-InnPris AT ROW 5.38 COL 16 COLON-ALIGNED
     FI-Rab1 AT ROW 6.43 COL 16 COLON-ALIGNED
     FI-Rab1% AT ROW 6.43 COL 32 COLON-ALIGNED NO-LABEL
     FI-Rab2 AT ROW 7.48 COL 16 COLON-ALIGNED
     FI-Rab2% AT ROW 7.48 COL 32 COLON-ALIGNED NO-LABEL
     FI-Frakt AT ROW 8.48 COL 16 COLON-ALIGNED
     FI-Frakt% AT ROW 8.48 COL 32 COLON-ALIGNED NO-LABEL
     FI-DivKost AT ROW 9.43 COL 16 COLON-ALIGNED
     FI-DivKost% AT ROW 9.43 COL 32 COLON-ALIGNED NO-LABEL
     FI-Rab3 AT ROW 10.38 COL 16 COLON-ALIGNED
     FI-Rab3% AT ROW 10.38 COL 32 COLON-ALIGNED NO-LABEL
     FI-VareKost AT ROW 11.81 COL 16 COLON-ALIGNED
     FI-DB AT ROW 12.81 COL 16 COLON-ALIGNED
     FI-DB% AT ROW 12.81 COL 32 COLON-ALIGNED NO-LABEL
     FI-MVA AT ROW 13.81 COL 16 COLON-ALIGNED
     FI-Mva% AT ROW 13.81 COL 32 COLON-ALIGNED NO-LABEL
     FI-Pris AT ROW 15.05 COL 16 COLON-ALIGNED
     FI-EuPris AT ROW 16.05 COL 16 COLON-ALIGNED
     T-Manuel AT ROW 16.05 COL 34
     Btn_OK AT ROW 17.91 COL 3
     B-Lagre AT ROW 9.57 COL 62 NO-TAB-STOP 
     Btn_Cancel AT ROW 17.91 COL 43
     B-Neste AT ROW 3.86 COL 62 NO-TAB-STOP 
     Btn_Help AT ROW 17.91 COL 62
     B-Slett AT ROW 10.76 COL 62 NO-TAB-STOP 
     BUTTON-SokProfil AT ROW 1.95 COL 29.8
     "Kroner" VIEW-AS TEXT
          SIZE 14.2 BY .62 AT ROW 3.43 COL 19.8
          FONT 6
     "Prosent" VIEW-AS TEXT
          SIZE 13 BY .62 AT ROW 5.76 COL 35
          FONT 6
     "Valuta" VIEW-AS TEXT
          SIZE 13 BY .62 AT ROW 3.43 COL 35
          FONT 6
     RECT-44 AT ROW 1.48 COL 3
     SPACE(1.39) SKIP(1.46)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Vedlikehold kalkyle artikkel"
         CANCEL-BUTTON Btn_Cancel.


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
   NOT-VISIBLE FRAME-NAME                                               */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON B-Forrige IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON B-Lagre IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON B-Neste IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON B-Slett IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Beskrivelse IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-DB IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-DB% IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-DivKost IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-DivKost% IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-EuPris IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Frakt IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Frakt% IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-InnPris IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-MVA IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Mva% IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Pris IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Rab1 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Rab1% IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Rab2 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Rab2% IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Rab3 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Rab3% IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-ValKod IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-ValPris IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-VareKost IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX T-Manuel IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX Dialog-Frame
/* Query rebuild information for DIALOG-BOX Dialog-Frame
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX Dialog-Frame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON GO OF FRAME Dialog-Frame /* Vedlikehold kalkyle artikkel */
DO:
  RUN LagrePost.
  IF RETURN-VALUE = "AVBRYT" THEN
    RETURN NO-APPLY.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Vedlikehold kalkyle artikkel */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Forrige
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Forrige Dialog-Frame
ON CHOOSE OF B-Forrige IN FRAME Dialog-Frame /* Forrige */
DO:
  FIND LAST PrisProfil NO-LOCK WHERE
    PrisProfil.ProfilNr < INPUT FI-ProfilNr NO-ERROR.
  IF NOT AVAILABLE PrisProfil THEN
    DO:
      MESSAGE "Det finnes ikke flere prisprofiler!"
              VIEW-AS ALERT-BOX TITLE "Melding".

      FIND PrisProfil NO-LOCK WHERE
        PrisProfil.ProfilNr = INPUT FI-ProfilNr NO-ERROR.

      RETURN NO-APPLY.    
    END.
  ELSE DO:
    DISPLAY 
      PrisProfil.ProfilNr    @ FI-PRofilNr
      Prisprofil.Beskrivelse @ FI-Beskrivelse
    WITH FRAME Dialog-Frame.
  END.

  RUN Initkalkyle.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Lagre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Lagre Dialog-Frame
ON CHOOSE OF B-Lagre IN FRAME Dialog-Frame /* Lagre */
DO:
  RUN LagrePost.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Neste
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Neste Dialog-Frame
ON CHOOSE OF B-Neste IN FRAME Dialog-Frame /* Neste */
DO:
  FIND FIRST PrisProfil NO-LOCK WHERE
    PrisProfil.ProfilNr > INPUT FI-ProfilNr NO-ERROR.
  IF NOT AVAILABLE PrisProfil THEN
    DO:
      MESSAGE "Det finnes ikke flere prisprofiler!"
              VIEW-AS ALERT-BOX TITLE "Melding".

      FIND PrisProfil NO-LOCK WHERE
        PrisProfil.ProfilNr = INPUT FI-ProfilNr NO-ERROR.

      RETURN NO-APPLY.    
    END.
  ELSE DO:
    DISPLAY 
      PrisProfil.ProfilNr    @ FI-PRofilNr
      Prisprofil.Beskrivelse @ FI-Beskrivelse
    WITH FRAME Dialog-Frame.
  END.

  RUN Initkalkyle.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Slett
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Slett Dialog-Frame
ON CHOOSE OF B-Slett IN FRAME Dialog-Frame /* Slett */
DO:
  /* Henter posten */
  FIND ArtPris NO-LOCK WHERE
    ArtPris.ArtikkelNr   = ArtBas.ArtikkelNr AND
    ArtPris.ProfilNr = INPUT FI-ProfilNr NO-ERROR.
  IF NOT AVAILABLE ArtPris THEN
    DO:
      MESSAGE "Det finnes ingen kalkyle på denne profilen."
        VIEW-AS ALERT-BOX MESSAGE TITLE "Melding".
      RETURN NO-APPLY "AVBRYT".
    END.

  wSvar = FALSE.  
  MESSAGE "Skal kalkylen for denne prisprofilen slettes?"
    VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Bekreft"
    UPDATE wSvar.
  IF wSvar THEN
    DO:  
      RUN SlettPost.
      RUN Initkalkyle.
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


&Scoped-define SELF-NAME BUTTON-SokProfil
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokProfil Dialog-Frame
ON CHOOSE OF BUTTON-SokProfil IN FRAME Dialog-Frame /* ... */
OR "F10":U OF FI-ProfilNr
DO:
  DEF VAR cLookupValue AS CHAR NO-UNDO.

  cLookupValue = "ProfilNr;KortNavn;Beskrivelse".
  RUN JBoxDLookup.w ("Prisprofil;ProfilNr|Profilnr|>>>>>>9;KortNavn;Beskrivelse","where true",INPUT-OUTPUT cLookupValue).

  IF cLookupValue NE "" THEN 
  DO:
    FI-ProfilNr:SCREEN-VALUE = ENTRY(1,cLookupValue,"|").
    FI-Beskrivelse:SCREEN-VALUE = ENTRY(3,cLookupValue,"|").
    APPLY "TAB" TO FI-ProfilNr.
  END.
  FIND Prisprofil NO-LOCK WHERE
    Prisprofil.ProfilNr = int(ENTRY(1,cLookupValue,"|")) NO-ERROR.
  
  RUN Initkalkyle.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-DB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-DB Dialog-Frame
ON LEAVE OF FI-DB IN FRAME Dialog-Frame /* DB (+) */
DO:
  RUN Kalkulasjon (1).
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-DB%
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-DB% Dialog-Frame
ON LEAVE OF FI-DB% IN FRAME Dialog-Frame
DO:
  IF INPUT FI-DB% > 99.99 THEN
    DO:
      MESSAGE "Du kan ikke ha mer enn 99.99 i DB%!" 
              VIEW-AS ALERT-BOX TITLE "Kalkulasjonsfeil".
      RETURN NO-APPLY.
    END.
  IF ROUND(INPUT FI-DB%,2) <> round(dec(ENTRY(15,wSjekkStreng,";")),2) THEN
    DO:
      RUN Kalkulasjon (1).
    END.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-DivKost
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-DivKost Dialog-Frame
ON LEAVE OF FI-DivKost IN FRAME Dialog-Frame /* Div. kost (+) */
DO:
  RUN Kalkulasjon (1).
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-DivKost%
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-DivKost% Dialog-Frame
ON LEAVE OF FI-DivKost% IN FRAME Dialog-Frame
DO:
  RUN Kalkulasjon (1).
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-EuPris
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-EuPris Dialog-Frame
ON TAB OF FI-EuPris IN FRAME Dialog-Frame /* Pris (Euro) */
OR "RETURN":U OF FI-EuPris
DO:
  APPLY "Entry":U TO FI-ValPris IN FRAME Dialog-Frame.  
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-Frakt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Frakt Dialog-Frame
ON LEAVE OF FI-Frakt IN FRAME Dialog-Frame /* Frakt (+) */
DO:
  RUN Kalkulasjon (1).
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-Frakt%
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Frakt% Dialog-Frame
ON LEAVE OF FI-Frakt% IN FRAME Dialog-Frame
DO:
  RUN Kalkulasjon (1).
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-Pris
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Pris Dialog-Frame
ON LEAVE OF FI-Pris IN FRAME Dialog-Frame /* Pris */
DO:
  IF INPUT FI-Pris <> dec(ENTRY(18,wSjekkStreng,";")) THEN
    DO:
      RUN Kalkulasjon (1).
      /*
      apply "ENTRY":U to FI-DB% in frame Dialog-Frame.
      return no-apply.
      */
    END.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-ProfilNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-ProfilNr Dialog-Frame
ON TAB OF FI-ProfilNr IN FRAME Dialog-Frame /* Prisprofil */
OR "RETURN":U OF FI-ProfilNr 
DO:
  FIND PrisProfil NO-LOCK WHERE
    PrisProfil.ProfilNr = INPUT FI-ProfilNr NO-ERROR.
  IF NOT AVAILABLE PrisProfil THEN
    DO:
      MESSAGE "Ugyldig prosprofil!"
              VIEW-AS ALERT-BOX TITLE "Melding".
      RETURN NO-APPLY.    
    END.
  ELSE DO:
    DISPLAY 
      PrisProfil.ProfilNr    @ FI-PRofilNr
      Prisprofil.Beskrivelse @ FI-Beskrivelse
    WITH FRAME Dialog-Frame.
  END.

  RUN Initkalkyle.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-Rab1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Rab1 Dialog-Frame
ON LEAVE OF FI-Rab1 IN FRAME Dialog-Frame /* Rabatt 1 (-) */
DO:
  RUN Kalkulasjon (1).
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-Rab1%
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Rab1% Dialog-Frame
ON LEAVE OF FI-Rab1% IN FRAME Dialog-Frame
DO:
  RUN Kalkulasjon (1).
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-Rab2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Rab2 Dialog-Frame
ON LEAVE OF FI-Rab2 IN FRAME Dialog-Frame /* Rabatt 2 (-) */
DO:
  RUN Kalkulasjon (1).
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-Rab2%
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Rab2% Dialog-Frame
ON LEAVE OF FI-Rab2% IN FRAME Dialog-Frame
DO:
  RUN Kalkulasjon (1).
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-Rab3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Rab3 Dialog-Frame
ON LEAVE OF FI-Rab3 IN FRAME Dialog-Frame /* Rabatt 3 (-) */
DO:
  RUN Kalkulasjon (1).
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-Rab3%
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Rab3% Dialog-Frame
ON LEAVE OF FI-Rab3% IN FRAME Dialog-Frame
DO:
  RUN Kalkulasjon (1).
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-ValPris
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-ValPris Dialog-Frame
ON LEAVE OF FI-ValPris IN FRAME Dialog-Frame /* Valutapris */
DO:
  RUN Kalkulasjon (1).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME T-Manuel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL T-Manuel Dialog-Frame
ON RETURN OF T-Manuel IN FRAME Dialog-Frame /* Manuel */
OR "tab":U OF T-Manuel
DO:
  IF INPUT T-Manuel = FALSE THEN
    APPLY "Entry":U TO FI-ValPris IN FRAME Dialog-Frame.
  ELSE
    APPLY "Entry":U TO FI-EuPris IN FRAME Dialog-Frame.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL T-Manuel Dialog-Frame
ON VALUE-CHANGED OF T-Manuel IN FRAME Dialog-Frame /* Manuel */
DO:
  IF INPUT T-Manuel = FALSE THEN
    ASSIGN
      FI-EuPris:sensitive = FALSE.
  ELSE
    ASSIGN
      FI-EuPris:sensitive = TRUE.        
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT EQ ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.

FIND ArtBas NO-LOCK 
     WHERE RECID(ArtBas) = wArtBasRecid NO-ERROR.
IF NOT AVAILABLE ArtBas THEN
  DO:
    MESSAGE "Ukjent artikkel!"
      VIEW-AS ALERT-BOX ERROR TITLE "Feil".
    RETURN NO-APPLY.
  END.

/* Sentrallager */
{syspara.i 5 1 1 wButik int}
FIND Butiker NO-LOCK WHERE
  Butiker.Butik = wButik NO-ERROR.
IF NOT AVAILABLE Butiker THEN
  DO:
    MESSAGE "Sentrallager er ikke definert!"
      VIEW-AS ALERT-BOX ERROR TITLE "Feil".
    RETURN NO-APPLY.
  END.
FIND PrisProfil NO-LOCK WHERE
  PrisProfil.ProfilNr = Butiker.ProfilNr NO-ERROR.
IF NOT AVAILABLE PrisProfil THEN
  DO:
    MESSAGE "Profiler ikke definert på butikken!"
      VIEW-AS ALERT-BOX ERROR TITLE "Feil".
    RETURN NO-APPLY.
  END.
  
/* find LevBas no-lock where                   */
/*   LevBas.LevNr = BestHode.LevNr no-error.   */
/* if not available LevBas then                */
/*   do:                                       */
/*     message "Ukjent leverandør!"            */
/*       view-as alert-box ERROR title "Feil". */
/*     return no-apply.                        */
/*   end.                                      */
FIND Valuta OF ArtBas NO-LOCK NO-ERROR.
IF NOT AVAILABLE Valuta THEN
  DO:
    MESSAGE "Ukjent valuta på artikkelen!"
      VIEW-AS ALERT-BOX ERROR TITLE "Feil".
    RETURN NO-APPLY.
  END.
FIND VarGr OF artBas NO-LOCK NO-ERROR.
FIND Moms OF VarGr NO-LOCK NO-ERROR.
      
/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

  RUN EnableFelt.

  RUN enable_UI.
  {lng.i}

  /* Default skal sentrallagerets kalkyle vises. */
  FIND ArtPris NO-LOCK WHERE
       ArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND
       ArtPris.ProfilNr   = Butiker.ProfilNr NO-ERROR.

  VIEW FRAME Dialog-Frame.
  
  DISPLAY 
    Butiker.ProfilNr WHEN AVAILABLE Butiker @ FI-ProfilNr
  WITH FRAME Dialog-Frame.
  
  APPLY "TAB":U TO FI-ProfilNr IN FRAME Dialog-Frame. 
  

  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
  wRetur-Verdi = "OK".
END.
RUN disable_UI.

&IF DEFINED(UIB_IS_RUNNING) EQ 0 &THEN
 RETURN wretur-verdi.
&else
 MESSAGE wretur-verdi VIEW-AS ALERT-BOX.
&endif

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EnableFelt Dialog-Frame 
PROCEDURE EnableFelt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF NOT AVAILABLE ArtBas THEN
  RETURN NO-APPLY.
  
DO WITH FRAME Dialog-Frame:
  ASSIGN
    FI-DB:sensitive       = TRUE
    FI-DB%:sensitive      = TRUE 
    FI-DivKost:sensitive  = TRUE 
    FI-DivKost%:sensitive = TRUE 
    FI-EuPris:sensitive   = TRUE 
    FI-Frakt:sensitive    = TRUE 
    FI-Frakt%:sensitive   = TRUE 
    FI-Pris:sensitive     = TRUE 
    FI-Rab1:sensitive     = TRUE 
    FI-Rab1%:sensitive    = TRUE 
    FI-Rab2:sensitive     = TRUE 
    FI-Rab2%:sensitive    = TRUE 
    FI-Rab3:sensitive     = TRUE 
    FI-Rab3%:sensitive    = TRUE 
    FI-ValPris:sensitive  = TRUE 
    T-Manuel:sensitive    = TRUE
    B-Lagre:sensitive     = TRUE
    B-Slett:sensitive     = TRUE    
    B-Neste:sensitive     = TRUE
    B-Forrige:sensitive   = TRUE.    
END.
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
  DISPLAY FI-ProfilNr FI-Beskrivelse FI-ValPris FI-ValKod FI-InnPris FI-Rab1 
          FI-Rab1% FI-Rab2 FI-Rab2% FI-Frakt FI-Frakt% FI-DivKost FI-DivKost% 
          FI-Rab3 FI-Rab3% FI-VareKost FI-DB FI-DB% FI-MVA FI-Mva% FI-Pris 
          FI-EuPris T-Manuel 
      WITH FRAME Dialog-Frame.
  ENABLE RECT-44 FI-ProfilNr Btn_OK Btn_Cancel Btn_Help BUTTON-SokProfil 
      WITH FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Initkalkyle Dialog-Frame 
PROCEDURE Initkalkyle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR wFeltNr    AS INT  NO-UNDO.
  DEF VAR wSkjerm    AS CHAR NO-UNDO.
  DEF VAR wTilbud    AS LOG  INITIAL FALSE NO-UNDO.

  /* Setter FrameScoop */
  DO WITH FRAME Dialog-Frame: 

  FIND ArtPris NO-LOCK WHERE
    ArtPris.ArtikkelNr   = ArtBas.ArtikkelNr AND
    ArtPris.ProfilNr = INPUT FI-ProfilNr NO-ERROR.
      
  ASSIGN
    FI-ValKod = IF AVAILABLE Valuta 
                  THEN Valuta.ValKod
                  ELSE "".                                        
      
  /* Legger prisprofil opp i skjermen. */
  DISPLAY 
    FI-ValKod 
  WITH FRAME Dialog-Frame.    
  
  /* Legger nye verdier opp på skjermen igjen. */
  IF AVAILABLE ArtPris THEN
  DO:
    ASSIGN
      FI-ValPris:screen-value  = STRING(ArtPris.ValPris[1])
      FI-InnPris:screen-value  = STRING(ArtPris.InnkjopsPris[1])
      FI-Rab1:screen-value     = STRING(ArtPris.Rab1Kr[1])
      FI-Rab1%:screen-value    = STRING(ArtPris.Rab1%[1])
      FI-Rab2:screen-value     = STRING(ArtPris.Rab2Kr[1])
      FI-Rab2%:screen-value    = STRING(ArtPris.Rab2%[1])
      FI-Frakt:screen-value    = STRING(ArtPris.Frakt[1])
      FI-Frakt%:screen-value   = STRING(ArtPris.Frakt%[1])
      FI-DivKost:screen-value  = STRING(ArtPris.DivKostKr[1])
      FI-DivKost%:screen-value = STRING(ArtPris.DivKost%[1])
      FI-Rab3:screen-value     = STRING(ArtPris.Rab3Kr[1])
      FI-Rab3%:screen-value    = STRING(ArtPris.Rab3%[1])
      FI-VareKost:screen-value = STRING(ArtPris.VareKost[1])
      FI-Mva:screen-value      = STRING(ArtPris.MvaKr[1])
      FI-Mva%:screen-value     = STRING(ArtPris.Mva%[1])
      FI-DB:screen-value       = STRING(ArtPris.DbKr[1])
      FI-DB%:screen-value      = STRING(ArtPris.DB%[1])
      FI-Pris:screen-value     = STRING(ArtPris.Pris[1])
      FI-EUPris:screen-value   = STRING(ArtPris.EuroPris[1])
      T-Manuel                 = ArtPris.EuroManuel.
  END.
  /* Blanker kalkylen hvis den ikke er lagt opp. */
  ELSE DO:
    ASSIGN
      FI-ValPris:screen-value  = ""
      FI-InnPris:screen-value  = ""
      FI-Rab1:screen-value     = ""
      FI-Rab1%:screen-value    = ""
      FI-Rab2:screen-value     = ""
      FI-Rab2%:screen-value    = ""
      FI-Frakt:screen-value    = ""
      FI-Frakt%:screen-value   = ""
      FI-DivKost:screen-value  = ""
      FI-DivKost%:screen-value = ""
      FI-Rab3:screen-value     = ""
      FI-Rab3%:screen-value    = ""
      FI-VareKost:screen-value = ""
      FI-Mva:screen-value      = ""
      FI-Mva%:screen-value     = ""
      FI-DB:screen-value       = ""
      FI-DB%:screen-value      = ""
      FI-Pris:screen-value     = ""
      FI-EUPris:screen-value   = ""
      T-Manuel                 = FALSE.
  END.

  DISPLAY T-Manuel WITH FRAME Dialog-Frame.
  IF T-Manuel = TRUE THEN
    ASSIGN
      FI-EuPris:sensitive IN FRAME Dialog-Frame = TRUE.
  ELSE
    ASSIGN
      FI-EuPris:sensitive  IN FRAME Dialog-Frame = FALSE.
  END. /* FrameScoop */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Kalkulasjon Dialog-Frame 
PROCEDURE Kalkulasjon :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER wSetNr AS INT NO-UNDO. /* 1 - Kalkulasjonsramme          */
                                             /* 2 - Visningsramme (Høyre side) */

  DEF VAR wFeltListe AS CHAR   NO-UNDO.
  DEF VAR wFraFelt   AS CHAR   NO-UNDO.
  DEF VAR wFeltNr    AS INT    NO-UNDO.
  DEF VAR wSkjerm    AS CHAR   NO-UNDO.
  DEF VAR h_PrisKo   AS HANDLE NO-UNDO.
  
  ASSIGN
    wFraFelt   = frame-field
    wFeltListe = "ValPris,InnPris,Rab1,Rab1%,Rab2,Rab2%,Frakt,Frakt%," + 
                 "DivKost,DivKost%,Rab3,Rab3%,VareKost,DB,DB%," +
                 "FI-Mva,FI-Mva%,Pris,EU-Pris".
                 
  /* Finner i hvilket felt markøren sto når prosedyren ble kalt. */
  CASE wSetNr:
    WHEN 1 THEN ASSIGN 
                   wFraFelt = SUBSTRING(wFraFelt,4).
    WHEN 2 THEN ASSIGN 
                   wFraFelt = SUBSTRING(wFraFelt,4)
                   wFraFelt = SUBSTRING(wFraFelt,LENGTH(wFraFelt) - 2).
  END CASE. 
  ASSIGN
    wFeltNr = LOOKUP(wFraFelt,wFeltListe).
    
  /* Ukjent felt. */  
  IF wFeltNr = 0 THEN
    DO:
      MESSAGE "Ukjent felt!" VIEW-AS ALERT-BOX TITLE "Kalkylefeil".
      RETURN NO-APPLY.  
    END.

  FRAME-SCOOPE:
  DO WITH FRAME Dialog-Frame:

    /* Pakker ned verdiene som ligger i skjermen. */
    ASSIGN
      wSkjerm = KalkStreng().

    /* Det skal ikke skje noe ved tabbing mellom feltene                 */
    /* Gambler her på at det ikke kommer to artikkler med samme kalkyle. */
    /* Hvis så om atte, så gjør det ikke noe.                            */
    IF (wSjekkStreng <> wSkjerm) THEN
      DO:
        ASSIGN wSjekkStreng = wSkjerm.
      END.
    ELSE 
      RETURN NO-APPLY.

   IF NOT VALID-HANDLE(h_PrisKo) THEN
     RUN prisko.p PERSISTENT SET h_PrisKo.

   /* Starter omkalkulering.                         */
   /* NB: Kalkulasjonen skjer i prosedyrebilboteket. */
   IF VALID-HANDLE(h_PrisKo) THEN
     RUN Omregning IN h_PrisKo
          (INPUT wArtBasRecid, 
           INPUT PrisProfil.ProfilNr,
           INPUT-OUTPUT wSkjerm,
           INPUT Moms.MomsProc,
           INPUT Valuta.ValKurs, 
           INPUT wFeltNr,
           INPUT FALSE).
    ELSE 
      MESSAGE "Prosedyrebiblotek er ikke startet!" VIEW-AS ALERT-BOX.
                  
    IF VALID-HANDLE(h_PrisKo) THEN
        DELETE PROCEDURE h_PrisKo.

    /* Legger nye verier opp på skjermen igjen. */
    ASSIGN
      FI-ValPris:screen-value  = ENTRY(1,wSkjerm,";")
      FI-InnPris:screen-value  = ENTRY(2,wSkjerm,";")
      FI-Rab1:screen-value     = ENTRY(3,wSkjerm,";")
      FI-Rab1%:screen-value    = ENTRY(4,wSkjerm,";")
      FI-Rab2:screen-value     = ENTRY(5,wSkjerm,";")
      FI-Rab2%:screen-value    = ENTRY(6,wSkjerm,";")
      FI-Frakt:screen-value    = ENTRY(7,wSkjerm,";")
      FI-Frakt%:screen-value   = ENTRY(8,wSkjerm,";")
      FI-DivKost:screen-value  = ENTRY(9,wSkjerm,";")
      FI-DivKost%:screen-value = ENTRY(10,wSkjerm,";")
      FI-Rab3:screen-value     = ENTRY(11,wSkjerm,";")
      FI-Rab3%:screen-value    = ENTRY(12,wSkjerm,";")
      FI-VareKost:screen-value = ENTRY(13,wSkjerm,";")
      FI-Mva:screen-value      = ENTRY(14,wSkjerm,";")
      FI-Mva%:screen-value     = ENTRY(15,wSkjerm,";")
      FI-DB:screen-value       = ENTRY(16,wSkjerm,";")
      FI-DB%:screen-value      = ENTRY(17,wSkjerm,";")
      FI-Pris:screen-value     = ENTRY(18,wSkjerm,";")
      FI-EUPris:screen-value   = ENTRY(19,wSkjerm,";").
  END. /* FRAME-SCOOPE */

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
DO WITH FRAME Dialog-Frame TRANSACTION:

  /* Sjekker at aktiv kalkyle er relevant. */
  IF INPUT FI-Pris = 0 OR
     INPUT FI-VareKost = 0 OR
     INPUT FI-DB <= 0 OR
     INPUT FI-DB% <= 0 OR
     INPUT FI-Valpris = 0 THEN
    DO:
      wSvar = FALSE.
      MESSAGE "Kalkylen er ufulstendig." SKIP
              "Det mangler en eller flere av følgende opplysninger:" SKIP
              "Valutapris, varekost, dekningsbidrag eller utpris." SKIP(1)
              "Skal kalkylen lagres?"
              VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Bekreft"
              UPDATE wSvar.
      IF wSvar = FALSE THEN
        RETURN NO-APPLY "AVBRYT".
    END.

  /* Henter posten */
  FIND ArtPris EXCLUSIVE-LOCK WHERE
    ArtPris.ArtikkelNr   = ArtBas.ArtikkelNr AND
    ArtPris.ProfilNr = INPUT FI-ProfilNr NO-ERROR.
  IF NOT AVAILABLE ArtPris THEN
    DO:
      CREATE ArtPris.
      ASSIGN
        ArtPris.ArtikkelNr   = ArtBas.ArtikkelNr 
        ArtPris.ProfilNr = INPUT FI-ProfilNr.
    END.
    
  ASSIGN
    ArtPris.ValPris[1]      = DEC(FI-ValPris:SCREEN-VALUE)
    ArtPris.InnkjopsPris[1] = DEC(FI-InnPris:SCREEN-VALUE)
    ArtPris.Rab1Kr[1]       = DEC(FI-Rab1:SCREEN-VALUE)
    ArtPris.Rab1%[1]        = DEC(FI-Rab1%:SCREEN-VALUE)
    ArtPris.Rab2Kr[1]       = DEC(FI-Rab2:SCREEN-VALUE)
    ArtPris.Rab2%[1]        = DEC(FI-Rab2%:SCREEN-VALUE)
    ArtPris.Frakt[1]        = DEC(FI-Frakt:SCREEN-VALUE)
    ArtPris.Frakt%[1]       = DEC(FI-Frakt%:SCREEN-VALUE)
    ArtPris.DivKostKr[1]    = DEC(FI-DivKost:SCREEN-VALUE)
    ArtPris.DivKost%[1]     = DEC(FI-DivKost%:SCREEN-VALUE)
    ArtPris.Rab3Kr[1]       = DEC(FI-Rab3:SCREEN-VALUE)
    ArtPris.Rab3%[1]        = DEC(FI-Rab3%:SCREEN-VALUE)
    ArtPris.VareKost[1]     = DEC(FI-VareKost:SCREEN-VALUE)
    ArtPris.MvaKr[1]        = DEC(FI-Mva:SCREEN-VALUE)
    ArtPris.Mva%[1]         = DEC(FI-Mva%:SCREEN-VALUE)
    ArtPris.DbKr[1]         = DEC(FI-DB:SCREEN-VALUE)
    ArtPris.DB%[1]          = DEC(FI-DB%:SCREEN-VALUE)
    ArtPris.Pris[1]         = DEC(FI-Pris:SCREEN-VALUE)
    ArtPris.EuroPris[1]     = DEC(FI-EUPris:SCREEN-VALUE)
    ArtPris.EuroManuel      = INPUT T-Manuel.  

  /* Finner sentrallager. */
  FIND Butiker NO-LOCK WHERE
    butiker.butik = wButik NO-ERROR.
    
    RELEASE ArtPris.

END. /* TRANSACTION */    

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SlettPost Dialog-Frame 
PROCEDURE SlettPost :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME Dialog-Frame TRANSACTION:

  /* Henter posten */
  FIND ArtPris EXCLUSIVE-LOCK WHERE
    ArtPris.ArtikkelNr   = ArtBas.ArtikkelNr AND
    ArtPris.ProfilNr = INPUT FI-ProfilNr NO-ERROR.
  IF NOT AVAILABLE ArtPris THEN
    DO:
      MESSAGE "Det finnes ingen kalkyle på denne profilen."
        VIEW-AS ALERT-BOX MESSAGE TITLE "Melding".
      RETURN NO-APPLY "AVBRYT".
    END.
        
  /* Døden */
  DELETE ArtPris.

END. /* TRANSACTION */    

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION KalkStreng Dialog-Frame 
FUNCTION KalkStreng RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR wTekst AS CHAR NO-UNDO.
  
  DO WITH FRAME Dialog-Frame:
  ASSIGN
    wTekst =    STRING(INPUT FI-ValPris) + ";" +
                string(INPUT FI-InnPris) + ";" +
                string(INPUT FI-Rab1) + ";" +
                string(INPUT FI-Rab1%) + ";" +
                string(INPUT FI-Rab2) + ";" +
                string(INPUT FI-Rab2%) + ";" +
                string(INPUT FI-Frakt) + ";" +
                string(INPUT FI-Frakt%) + ";" +
                string(INPUT FI-DivKost) + ";" +
                string(INPUT FI-DivKost%) + ";" +
                string(INPUT FI-Rab3) + ";" +
                string(INPUT FI-Rab3%) + ";" +
                string(INPUT FI-VareKost) + ";" +
                string(INPUT FI-Mva) + ";" +
                string(INPUT FI-Mva%) + ";" +
                string(INPUT FI-DB) + ";" +
                string(INPUT FI-DB%) + ";" +
                string(INPUT FI-Pris) + ";" +
                string(INPUT FI-EUPris) + ";" +
                (IF INPUT T-Manuel = TRUE
                   THEN "True"
                   ELSE "False") + ";" +
                string(TODAY) + ";" +
                string(TODAY) + ";" +
                "0"  + ";" +
                "0"  + ";" +
                "false".
  
  END.
  
  RETURN wTekst.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

