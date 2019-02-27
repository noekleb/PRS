&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
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
/*          This .W file was created with the Progress AppBulder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameter Definisjoner ---                                           */
&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
  DEF VAR wArtikkelNr AS DEC NO-UNDO.
  DEF VAR cUtvidetSok  AS CHAR  NO-UNDO.
&ELSE
  DEF OUTPUT PARAMETER wArtikkelNr AS DEC NO-UNDO.
  DEF INPUT PARAMETER cUtvidetSok AS CHARACTER NO-UNDO  /* om blank bara sök i artbas */.
&ENDIF

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
DEF VAR cTekst         AS CHAR NO-UNDO.
DEF VAR cTekst2        AS CHAR NO-UNDO.
DEF VAR piEkstVPILevNr AS INT  NO-UNDO.
DEF VAR pcKode         AS CHAR NO-UNDO.
DEF VAR cEkstVPILevNrListe AS CHAR NO-UNDO.
DEF VAR plVPISjekk     AS LOG  NO-UNDO.
DEF VAR bHk            AS LOG  NO-UNDO.
DEF VAR iLevNr         AS INT  NO-UNDO.
DEF VAR wOldRecid      AS RECID NO-UNDO.
DEF VAR iSkjulFelt     AS INT  NO-UNDO.
DEF VAR cStartFelt     AS CHAR NO-UNDO.

DEFINE VARIABLE iCount AS INTEGER    NO-UNDO.
DEFINE VARIABLE cStrekKode AS CHARACTER  NO-UNDO.

DEF VAR cSok         AS CHARACTER  NO-UNDO.
DEF VAR wArtBasRecid AS RECID      NO-UNDO.
DEF VAR cSokStart    AS CHAR       NO-UNDO.
DEF VAR rUtvidetsok  AS RECID      NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BtnLevNr RECT-49 RECT-62 RECT-64 RECT-67 ~
RECT-68 FI-LevNr FI-Vg FI-LopNr FI-ArtikkelNr FI-LevKod FI-Beskr ~
FI-Strekkode FI-UtvidetSok FI-BestNr FI-ERPNr B-Sok Btn_OK 
&Scoped-Define DISPLAYED-OBJECTS FI-LevNr FI-Vg FI-LopNr FI-ArtikkelNr ~
FI-LevKod FI-Beskr FI-Strekkode FI-UtvidetSok FI-BestNr FI-ERPNr FI-Msg 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD InitScreen Dialog-Frame 
FUNCTION InitScreen RETURNS LOGICAL
  ( INPUT icFieldValueFocus AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-Sok 
     LABEL "Start søk" 
     SIZE 23 BY 1.14.

DEFINE BUTTON BtnLevNr  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON Btn_OK AUTO-GO DEFAULT 
     LABEL "Avbryt" 
     SIZE 23 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE FI-ArtikkelNr AS DECIMAL FORMAT "zzzzzzzzzzzz9":U INITIAL 0 
     LABEL "ArtikkelNr" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 TOOLTIP "Søk mot internt artikkelnummer." NO-UNDO.

DEFINE VARIABLE FI-Beskr AS CHARACTER FORMAT "X(256)":U 
     LABEL "Varetekst" 
     VIEW-AS FILL-IN 
     SIZE 59 BY 1 TOOLTIP "Søk mot varetekst. '*'  for føste, del av eller bakerst del av ord." NO-UNDO.

DEFINE VARIABLE FI-BestNr AS CHARACTER FORMAT "X(256)":U 
     LABEL "Bestillingsnr" 
     VIEW-AS FILL-IN 
     SIZE 59 BY 1 TOOLTIP "Søk mot bestillingsnummer som ligger på strekkoden" NO-UNDO.

DEFINE VARIABLE FI-ERPNr AS CHARACTER FORMAT "X(256)":U 
     LABEL "ERPNr" 
     VIEW-AS FILL-IN 
     SIZE 59 BY 1 TOOLTIP "Søk mot ERPNummer som ligger på strekkoden" NO-UNDO.

DEFINE VARIABLE FI-LevKod AS CHARACTER FORMAT "X(256)":U 
     LABEL "Lev.art.nr" 
     VIEW-AS FILL-IN 
     SIZE 59 BY 1 TOOLTIP "Søk mot modellnr. '*'  for føste, del av eller bakerst del av ord." NO-UNDO.

DEFINE VARIABLE FI-LevNr AS INTEGER FORMAT "zzzzz9" INITIAL 0 
     LABEL "Filter på lev.nr" 
     VIEW-AS FILL-IN 
     SIZE 10.4 BY 1 TOOLTIP "Filter på levnr. Avgrenser søk mot en leverandør.".

DEFINE VARIABLE FI-LopNr AS INTEGER FORMAT "zzzzz9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 TOOLTIP "Kombinert søk på varegruppe og løpenummer." NO-UNDO.

DEFINE VARIABLE FI-Msg AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN NATIVE 
     SIZE 74 BY 1
     BGCOLOR 15 FGCOLOR 0 FONT 6 NO-UNDO.

DEFINE VARIABLE FI-Strekkode AS DECIMAL FORMAT "zzzzzzzzzzzz9":U INITIAL 0 
     LABEL "Strekkode" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 TOOLTIP "Søk mot strekkode." NO-UNDO.

DEFINE VARIABLE FI-UtvidetSok AS CHARACTER FORMAT "X(256)":U 
     LABEL "Utvidet søk" 
     VIEW-AS FILL-IN 
     SIZE 59 BY 1 TOOLTIP "Utvidet søk. Bruk stjerne '*' bakerst for del av ord."
     BGCOLOR 16  NO-UNDO.

DEFINE VARIABLE FI-Vg AS INTEGER FORMAT "zzzzz9":U INITIAL 0 
     LABEL "Vg/LøpeNr" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 TOOLTIP "Kombinert søk på varegruppe og løpenummer." NO-UNDO.

DEFINE RECTANGLE RECT-49
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 74 BY 4.76.

DEFINE RECTANGLE RECT-62
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 74 BY 1.38.

DEFINE RECTANGLE RECT-64
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 74 BY 1.43.

DEFINE RECTANGLE RECT-67
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 74 BY 2.43.

DEFINE RECTANGLE RECT-68
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 74 BY 1.29.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     BtnLevNr AT ROW 1.48 COL 31.6 NO-TAB-STOP 
     FI-LevNr AT ROW 1.48 COL 19 COLON-ALIGNED HELP
          "Leverandørnummer"
     FI-Vg AT ROW 3 COL 14 COLON-ALIGNED
     FI-LopNr AT ROW 3 COL 24 COLON-ALIGNED NO-LABEL
     FI-ArtikkelNr AT ROW 4 COL 14 COLON-ALIGNED
     FI-LevKod AT ROW 5 COL 14 COLON-ALIGNED
     FI-Beskr AT ROW 6 COL 14 COLON-ALIGNED
     FI-Strekkode AT ROW 7.67 COL 14 COLON-ALIGNED
     FI-UtvidetSok AT ROW 9 COL 14 COLON-ALIGNED
     FI-BestNr AT ROW 10.43 COL 14 COLON-ALIGNED HELP
          "Søk mot bestillingsnummer som ligger på strekkoden"
     FI-ERPNr AT ROW 11.52 COL 14 COLON-ALIGNED HELP
          "Søk mot bestillingsnummer som ligger på strekkoden"
     FI-Msg AT ROW 12.95 COL 2 NO-LABEL
     B-Sok AT ROW 14.1 COL 2
     Btn_OK AT ROW 14.1 COL 53
     RECT-49 AT ROW 2.67 COL 2
     RECT-62 AT ROW 8.86 COL 2
     RECT-64 AT ROW 1.24 COL 2
     RECT-67 AT ROW 10.24 COL 2
     RECT-68 AT ROW 7.57 COL 2
     SPACE(0.59) SKIP(6.51)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Artikkelsøk".


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
{dproclibstart.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   FRAME-NAME                                                           */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN FI-Msg IN FRAME Dialog-Frame
   NO-ENABLE ALIGN-L                                                    */
ASSIGN 
       FI-Msg:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX Dialog-Frame
/* Query rebuild information for DIALOG-BOX Dialog-Frame
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX Dialog-Frame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON END-ERROR OF FRAME Dialog-Frame /* Artikkelsøk */
DO:
    APPLY "CHOOSE" TO Btn_OK.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Artikkelsøk */
DO:
    APPLY "CHOOSE" TO Btn_OK.
/*   APPLY "END-ERROR":U TO SELF. */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Sok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Sok Dialog-Frame
ON CHOOSE OF B-Sok IN FRAME Dialog-Frame /* Start søk */
DO:
    IF TRIM(INPUT FI-UtvidetSok) <> "" THEN
        APPLY "TAB" TO FI-UtvidetSok.
    ELSE
        APPLY "TAB" TO FI-BestNr.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnLevNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnLevNr Dialog-Frame
ON CHOOSE OF BtnLevNr IN FRAME Dialog-Frame /* ... */
OR F10 OF FI-LevNr
DO:
  DEF VAR cLookupValue AS CHAR NO-UNDO.

  /* Syntaks: Param1: <Tabell>;<Felt>;<Felt>...,<Tabell>;<Felt>;<Felt>...              */
  /*          Param2: <Where sats> m/Join                                              */
  /*          Param3: <Returfelt1>[;<Returfelt2>;......],<Filterfelt1>[;<Filterfelt2>] (Settes i cLookupValue) */
  /* Kalkulerte felt kan også benyttes, label, format o.l..       */
  cLookupValue = "LevNr".
  RUN JBoxDLookup.w ("LevBas;LevNr;LevNamn","where true",INPUT-OUTPUT cLookupValue).

  IF cLookupValue NE "" THEN 
  DO:
    fi-LevNr:SCREEN-VALUE = cLookupValue.
  END.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-ArtikkelNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-ArtikkelNr Dialog-Frame
ON ANY-PRINTABLE OF FI-ArtikkelNr IN FRAME Dialog-Frame /* ArtikkelNr */
DO:
  FI-Msg:SCREEN-VALUE = ''.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-ArtikkelNr Dialog-Frame
ON ENTRY OF FI-ArtikkelNr IN FRAME Dialog-Frame /* ArtikkelNr */
DO:
  FI-Msg:SCREEN-VALUE = SELF:TOOLTIP.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-ArtikkelNr Dialog-Frame
ON TAB OF FI-ArtikkelNr IN FRAME Dialog-Frame /* ArtikkelNr */
/* or "return" of FI-ArtikkelNR                                                    */
/* DO:                                                                             */
/*   if input FI-ArtikkelNr <> 0 then                                              */
/*     do:                                                                         */
/*       find ArtBas no-lock where                                                 */
/*         ArtBas.ArtikkelNr = input FI-ArtikkelNr no-error.                       */
/*       if not available ArtBas then                                              */
/*         do:                                                                     */
/*           message "Ukjent artikkel!" view-as alert-box message title "Melding". */
/*           return no-apply.                                                      */
/*         end.                                                                    */
/*       else do:                                                                  */
/*         assign wArtBasRecid = recid(ArtBAs).                                    */
/*         apply "choose":U to Btn_Ok.                                             */
/*       end.                                                                      */
/*     end.                                                                        */
/*                                                                                 */
/* END.                                                                            */

OR "return" OF FI-ArtikkelNr
DO:
    cStartFelt = 'FI-ArtikkelNr'.
  IF INPUT FI-ArtikkelNr <> 0 THEN DO:
      ASSIGN cStrekkode = FI-ArtikkelNr:SCREEN-VALUE.
      FIND ArtBas NO-LOCK WHERE ArtBas.ArtikkelNr = dec(cStrekkode) NO-ERROR.
      IF NOT AVAILABLE ArtBas AND cUtvidetSok <> "" THEN
        VPI-SJEKK:
        DO:
          cEkstVPILevNrListe = "1".
          IF NUM-ENTRIES(cUtvidetSok,CHR(1)) > 1 THEN
          ASSIGN
              cEkstVPILevNrListe = ENTRY(2,cUtvidetSok,CHR(1)) +
                                   (IF cEkstVPILevNrListe = ""
                                      THEN ""
                                      ELSE ",") + 
                                   cEkstVPILevNrListe.
          /* Sjekker VPI leverandørene i prioritert rekkefølge. */
          DO iCount = 1 TO NUM-ENTRIES(cEkstVPILevNrListe): /* i prioritert rekkef. */
              FIND FIRST VPIArtBas NO-LOCK WHERE
                  VPIArtBas.EkstVPILevNr = INT(ENTRY(iCount,cEkstVPILevNrListe)) AND
                  VPIArtBas.VareNr       = cStrekkode NO-ERROR.

              IF AVAILABLE VPIArtBas THEN
              /* Varsle at ekstern VPI er funnet og gi mulighet for å kunne velge å opprette lokalt. */
              VPI-FUNNET:
              DO:
                  ASSIGN
                      wArtBasRecid = VPIArtBas.EkstVPILevNr * -1 /* Flagger at VPI er funnet. */
                      pcKode       = VPIArtBas.VareNr /* Bruker artikkelnr      */
                      .
                  LEAVE.
              END. /* VPI-FUNNET */
          END.
          IF wArtBasRecid < 0 AND wArtBasRecid <> ? THEN DO:
              APPLY "choose":U TO Btn_Ok.
              RETURN NO-APPLY.
          END.
          ELSE DO:
              FI-Msg:SCREEN-VALUE = 'Ukjent artikkel!'.
              /*MESSAGE "Ukjent artikkelnummer!" VIEW-AS ALERT-BOX MESSAGE TITLE "Melding".*/
              RETURN NO-APPLY.
          END.
        END. /* VPI-SJEKK */
      ELSE DO:
        IF AVAIL ArtBas THEN DO:
            ASSIGN wArtBasRecid = RECID(ArtBAs).
            APPLY "choose":U TO Btn_Ok.
        END.
        ELSE DO:
            FI-Msg:SCREEN-VALUE = 'Ukjent artikkelnr!'.
            /*
            MESSAGE "Ukjent artikkelnummer." SKIP
                cStrekkode
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            */
            RETURN NO-APPLY.
        END.
      END.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-Beskr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Beskr Dialog-Frame
ON ANY-PRINTABLE OF FI-Beskr IN FRAME Dialog-Frame /* Varetekst */
DO:
  FI-Msg:SCREEN-VALUE = ''.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Beskr Dialog-Frame
ON ENTRY OF FI-Beskr IN FRAME Dialog-Frame /* Varetekst */
DO:
  FI-Msg:SCREEN-VALUE = SELF:TOOLTIP.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Beskr Dialog-Frame
ON TAB OF FI-Beskr IN FRAME Dialog-Frame /* Varetekst */
OR "return" OF FI-Beskr
DO:
  DEF VAR bOK     AS LOG NO-UNDO.
  DEF VAR hQuery  AS HANDLE NO-UNDO.
  DEF VAR lcTekst AS CHAR NO-UNDO.

        /* Bare gå videre */
    IF FI-Beskr:SCREEN-VALUE = "" THEN
        RETURN.
    ASSIGN
        cSok = ""
        FI-MSg:SCREEN-VALUE = "Bygger eksternv VPI liste..."
        .
    
    RUN EkstVPINrListe (OUTPUT cEkstVPILevNrListe).
    ASSIGN
        FI-MSg:SCREEN-VALUE = ""
        .
    SOKBLOKK:
    DO:
        IF INPUT FI-Beskr <> "" THEN 
        LOKALSOK:
        DO:
            ASSIGN 
                FI-Beskr:screen-value = RIGHT-TRIM(FI-Beskr:screen-value,'*')
                FI-MSg:SCREEN-VALUE = "Søk varetekst " + INPUT FI-Beskr + " starter..."
                cSok = INPUT FI-Beskr + IF INPUT FI-Beskr BEGINS "*" THEN "*" ELSE ""
                .
            IF (cSok BEGINS "*" AND CAN-FIND(FIRST ArtBas WHERE ArtBas.Beskr MATCHES cSok AND
                                     (IF INPUT FI-LevNr > 0 THEN ArtBas.LevNr = INPUT FI-LevNr ELSE TRUE))) OR
               (NOT cSok BEGINS "*" AND CAN-FIND(FIRST ArtBas WHERE ArtBas.Beskr BEGINS cSok AND
                                     (IF INPUT FI-LevNr > 0 THEN ArtBas.LevNr = INPUT FI-LevNr ELSE TRUE))) THEN
            BLOKKEN:
            DO:
                    ASSIGN
                        FI-MSg:SCREEN-VALUE = "Starter sø mot artikkelregsiter...".
                    RUN dartbas_sok.w (FI-LevNr:SCREEN-VALUE,
                                       FI-LevKod:SCREEN-VALUE,
                                       FI-Beskr:SCREEN-VALUE,
                                       FI-UtvidetSok:SCREEN-VALUE,
                                       INPUT-OUTPUT cTekst).
                    ASSIGN
                        FI-MSg:SCREEN-VALUE = "Ferdig gartbassok..."
                        .
                    IF RETURN-VALUE = "AVBRYT" THEN
                        RETURN NO-APPLY.
                    IF RETURN-VALUE = "AVBRYT" THEN DO:
                        CASE cStartFelt:
                            WHEN 'FI-LopNr' THEN APPLY 'ENTRY' TO FI-LopNr.
                            WHEN 'FI-ArtikkelNr' THEN APPLY 'ENTRY' TO FI-ArtikkelNr.
                            WHEN 'FI-LevKod' THEN APPLY 'ENTRY' TO FI-LevKod.
                            WHEN 'FI-Strekkode' THEN APPLY 'ENTRY' TO FI-Strekkode.
                            WHEN 'FI-Utvidetsok' THEN APPLY 'ENTRY' TO FI-Utvidetsok.
                            WHEN 'FI-BestNr' THEN APPLY 'ENTRY' TO FI-BestNr.
                            WHEN 'FI-ERPNr' THEN APPLY 'ENTRY' TO FI-ERPNr.
                        END CASE.
                        RETURN NO-APPLY.
                    END.
                    ELSE IF RETURN-VALUE BEGINS "VPISOK" THEN
                    DO:
                        IF cSok <> '' THEN
                            lcTekst = "UtvidetBeskr" + chr(1) + cSok.
                        /* Kaller søkerutine */
                        RUN gvpiartbas.w (
                          INPUT-OUTPUT cTekst2, /* Returstreng - chr(1) separert */
                          "EkstVPILevNr", /* Feltliste avgrensningsfelt (kommaseparert) */
                          "1", /* Feltverdier (chr(1) sep) */ 
                          lcTekst, /* Post markøren skal stå på */
                          wOldRecid
                          ).
                        IF RETURN-VALUE = "AVBRYT" THEN
                            RETURN NO-APPLY.
                        /* Flytter over resultatverdier */
                        cTekst = cTekst2.
                    END.
                    IF NUM-ENTRIES(cTekst,CHR(1)) >= 2 THEN
                    DO:
                        FIND ArtBas NO-LOCK WHERE
                            ArtBas.ArtikkelNr = DEC(ENTRY(2,cTekst,CHR(1))) NO-ERROR.
                        IF AVAILABLE ArtBas THEN DO:
                            ASSIGN wArtBasRecid = RECID(ArtBAs).
                            LEAVE SOKBLOKK.
                        END.
                        ELSE DO:
                            FI-Msg:SCREEN-VALUE = 'Ingen artikler funnet.'.
                            /*
                            MESSAGE "Ingen artikler funnet."
                                VIEW-AS ALERT-BOX INFO BUTTONS OK.
                            */
                            ASSIGN SELF:SCREEN-VALUE = "".
                            RETURN NO-APPLY.
                        END.
                    END.
            END. /* BLOKKEN */
            ELSE DO:
                IF cUtvidetSok BEGINS "JA" THEN
                    LEAVE LOKALSOK.
                ELSE DO:
                    FI-Msg:SCREEN-VALUE = 'Ingen artikler funnet i lokalt register.'.
                    /*
                    MESSAGE "Ingen artikler funnet i lokalt register."
                        VIEW-AS ALERT-BOX INFO BUTTONS OK.
                    */
                    RETURN NO-APPLY.
                END.
            END.
            ASSIGN
                FI-MSg:SCREEN-VALUE = ""
                .
        END. /* LOKALSOK */
   
        UTVIDETSOK:
        DO iCount = 1 TO NUM-ENTRIES(cEkstVPILevNrListe):
            ASSIGN
                FI-MSg:SCREEN-VALUE = "Utvidet søk mot VPIlev  " + ENTRY(iCount,cEkstVPILevNrListe) +  " ..."
                .
            {sww.i}
            /* Kontrollerer mot default VPI leverandør */
            IF cSok BEGINS "*" THEN
                FIND FIRST VPIArtBas NO-LOCK WHERE
                     VPIArtBas.EkstVPILevNr = int(ENTRY(iCount,cEkstVPILevNrListe)) AND
                     VPIArtBas.Beskr MATCHES cSok NO-ERROR.
            ELSE 
                FIND FIRST VPIArtBas NO-LOCK WHERE
                    VPIArtBas.EkstVPILevNr = int(ENTRY(iCount,cEkstVPILevNrListe)) AND
                    VPIArtBas.Beskr BEGINS cSok NO-ERROR.
            {swn.i}
            IF AVAILABLE VPIArtBas THEN
            /* Varsle at ekstern VPI er funnet og gi mulighet for å kunne velge å opprette lokalt. */
            VPI-FUNNET:
            DO:
                ASSIGN
                    FI-MSg:SCREEN-VALUE = "Ekstern VPI funnet..."
                    wArtBasRecid = VPIArtBas.EkstVPILevNr * -1 /* Flagger at VPI er funnet. */
                    pcKode       = "BESKR" + chr(1) + cSok /* Tar vare på strekkoden */
                    .
                LEAVE SOKBLOKK.
            END. /* VPI-FUNNET */
            ELSE DO:
                IF iCount < num-entries(cEkstVPILevNrListe) THEN
                    NEXT UTVIDETSOK.
                ELSE DO:
                    FI-Msg:SCREEN-VALUE = 'Ingen artikler funnet i lokalt eller VPI register.'.
                    /*
                    MESSAGE "Ingen artikler funnet i lokalt eller VPI register." VIEW-AS ALERT-BOX MESSAGE TITLE "Melding".
                    */
                    RETURN NO-APPLY.
                END.
            END.
        END. /* UTVIDETSOK */
    END. /* SOKBLOKK */
    ASSIGN
        FI-MSg:SCREEN-VALUE = ""
        .
    APPLY "choose":U TO Btn_Ok.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-BestNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-BestNr Dialog-Frame
ON ANY-PRINTABLE OF FI-BestNr IN FRAME Dialog-Frame /* Bestillingsnr */
DO:
  FI-Msg:SCREEN-VALUE = ''.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-BestNr Dialog-Frame
ON ENTRY OF FI-BestNr IN FRAME Dialog-Frame /* Bestillingsnr */
DO:
  FI-Msg:SCREEN-VALUE = SELF:TOOLTIP.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-BestNr Dialog-Frame
ON TAB OF FI-BestNr IN FRAME Dialog-Frame /* Bestillingsnr */
OR "return" OF FI-BestNr
DO:
  cStartFelt = 'FI-BestNr'.
  DEF VAR bOK     AS LOG NO-UNDO.
  DEF VAR hQuery  AS HANDLE NO-UNDO.
  DEF VAR lcTekst AS CHAR NO-UNDO.
        /* Bare gå videre */
    IF FI-BestNr:SCREEN-VALUE = "" THEN
        RETURN.
    IF NOT CAN-FIND(FIRST Strekkode NO-LOCK WHERE
        Strekkode.Bestillingsnummer BEGINS FI-BestNr:SCREEN-VALUE) THEN
    DO:
        MESSAGE 'Ingen artikler funnet.'
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN NO-APPLY.
    END.

    ASSIGN
        cSok = "".
    RUN EkstVPINrListe (OUTPUT cEkstVPILevNrListe).
    SOKBLOKK:
    DO:
        IF INPUT FI-BestNr <> "" THEN 
        LOKALSOK:
        DO:
            /* Bruk av matches virker ikke mot strekkodetabellen. det tar laaaang tid. */
            ASSIGN 
                cSok = TRIM(cSok,'*').
            /*    cSok = INPUT FI-BestNr + IF INPUT FI-BestNr BEGINS "*" 
                                                     THEN "*" 
                                                     ELSE "". */
            CREATE QUERY hQuery.
            hQuery:SET-BUFFERS(BUFFER Strekkode:HANDLE).
            hQuery:QUERY-PREPARE("FOR EACH Strekkode NO-LOCK WHERE TRUE " 
                               + (IF INDEX(cSok,"*") > 0 THEN 
                                    " AND Bestillingsnummer MATCHES '" + cSok + "*'"
                                  ELSE IF cSok NE "" THEN
                                    " AND Bestillingsnummer BEGINS '" + cSok + "'"
                                  ELSE "")
                               ). 
            hQuery:QUERY-OPEN().
            hQuery:GET-FIRST().
            IF hQuery:GET-BUFFER-HANDLE(1):AVAIL THEN
            BLOKKEN:
            DO:
                    RUN dstrekkode_sok.w (TRIM(FI-BestNr:SCREEN-VALUE,'*'), 
                                         TRIM(FI-ERPNr:SCREEN-VALUE,'*'), 
                                         INPUT-OUTPUT cTekst).
                    IF RETURN-VALUE = "AVBRYT" THEN
                        RETURN NO-APPLY.
                    IF RETURN-VALUE = "AVBRYT" THEN DO:
                        CASE cStartFelt:
                            WHEN 'FI-LopNr' THEN APPLY 'ENTRY' TO FI-LopNr.
                            WHEN 'FI-ArtikkelNr' THEN APPLY 'ENTRY' TO FI-ArtikkelNr.
                            WHEN 'FI-LevKod' THEN APPLY 'ENTRY' TO FI-LevKod.
                            WHEN 'FI-Strekkode' THEN APPLY 'ENTRY' TO FI-Strekkode.
                            WHEN 'FI-Utvidetsok' THEN APPLY 'ENTRY' TO FI-Utvidetsok.
                            WHEN 'FI-BestNr' THEN APPLY 'ENTRY' TO FI-BestNr.
                            WHEN 'FI-ERPNr' THEN APPLY 'ENTRY' TO FI-ERPNr.
                        END CASE.
                        RETURN NO-APPLY.
                    END.
                    IF NUM-ENTRIES(cTekst,CHR(1)) >= 2 THEN
                    DO:
                        FIND ArtBas NO-LOCK WHERE
                            ArtBas.ArtikkelNr = DEC(ENTRY(2,cTekst,CHR(1))) NO-ERROR.
                        IF AVAILABLE ArtBas THEN DO:
                            ASSIGN wArtBasRecid = RECID(ArtBAs).
                            LEAVE SOKBLOKK.
                        END.
                        ELSE DO:
                            MESSAGE "Ingen artikler funnet i lokalt register."
                                VIEW-AS ALERT-BOX INFO BUTTONS OK.
                            ASSIGN SELF:SCREEN-VALUE = "".
                            RETURN NO-APPLY.
                        END.
                    END.
            END. /* BLOKKEN */
            ELSE DO:
                IF cUtvidetSok BEGINS "JA" THEN
                    LEAVE LOKALSOK.
                ELSE DO:
                    MESSAGE "Ingen artikler funnet."
                        VIEW-AS ALERT-BOX INFO BUTTONS OK.
                    RETURN NO-APPLY.
                END.
            END.
        END.
    END. /* SOKBLOKK */

    DELETE OBJECT hQuery.
    APPLY "choose":U TO Btn_Ok.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-ERPNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-ERPNr Dialog-Frame
ON ENTRY OF FI-ERPNr IN FRAME Dialog-Frame /* ERPNr */
DO:
  FI-Msg:SCREEN-VALUE = SELF:TOOLTIP.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-ERPNr Dialog-Frame
ON TAB OF FI-ERPNr IN FRAME Dialog-Frame /* ERPNr */
OR "return" OF FI-ERPNr
DO:
  cStartFelt = 'FI-ERPNr'.

  DEF VAR bOK     AS LOG NO-UNDO.
  DEF VAR hQuery  AS HANDLE NO-UNDO.
  DEF VAR lcTekst AS CHAR NO-UNDO.
        /* Bare gå videre */
    IF FI-ERPNr:SCREEN-VALUE = "" THEN
        RETURN.

    ASSIGN
        cSok = "".
    RUN EkstVPINrListe (OUTPUT cEkstVPILevNrListe).
    SOKBLOKK:
    DO:
        IF INPUT FI-ERPNr <> "" THEN 
        LOKALSOK:
        DO:
            ASSIGN 
                cSok = TRIM(cSok,'*').
              /*  cSok = INPUT FI-ERPNr + IF INPUT FI-ERPNr BEGINS "*" 
                                                     THEN "*" 
                                                     ELSE "". */
            CREATE QUERY hQuery.
            hQuery:SET-BUFFERS(BUFFER Strekkode:HANDLE).
            hQuery:QUERY-PREPARE("FOR EACH Strekkode NO-LOCK WHERE TRUE " 
                               + (IF INDEX(cSok,"*") > 0 THEN 
                                    " AND ERPNr MATCHES '" + cSok + "*'"
                                  ELSE IF cSok NE "" THEN
                                    " AND ERPNr BEGINS '" + cSok + "'"
                                  ELSE "")
                               ). 
            hQuery:QUERY-OPEN().
            hQuery:GET-FIRST().
            IF hQuery:GET-BUFFER-HANDLE(1):AVAIL THEN
            BLOKKEN:
            DO:
                    RUN dstrekkode_sok.w (TRIM(FI-BestNr:SCREEN-VALUE,'*'), 
                                         TRIM(FI-ERPNr:SCREEN-VALUE,'*'), 
                                         INPUT-OUTPUT cTekst).
                    IF RETURN-VALUE = "AVBRYT" THEN
                        RETURN NO-APPLY.
                    IF RETURN-VALUE = "AVBRYT" THEN DO:
                        CASE cStartFelt:
                            WHEN 'FI-LopNr' THEN APPLY 'ENTRY' TO FI-LopNr.
                            WHEN 'FI-ArtikkelNr' THEN APPLY 'ENTRY' TO FI-ArtikkelNr.
                            WHEN 'FI-LevKod' THEN APPLY 'ENTRY' TO FI-LevKod.
                            WHEN 'FI-Strekkode' THEN APPLY 'ENTRY' TO FI-Strekkode.
                            WHEN 'FI-Utvidetsok' THEN APPLY 'ENTRY' TO FI-Utvidetsok.
                            WHEN 'FI-BestNr' THEN APPLY 'ENTRY' TO FI-BestNr.
                            WHEN 'FI-ERPNr' THEN APPLY 'ENTRY' TO FI-ERPNr.
                        END CASE.
                        RETURN NO-APPLY.
                    END.
                    IF NUM-ENTRIES(cTekst,CHR(1)) >= 2 THEN
                    DO:
                        FIND ArtBas NO-LOCK WHERE
                            ArtBas.ArtikkelNr = DEC(ENTRY(2,cTekst,CHR(1))) NO-ERROR.
                        IF AVAILABLE ArtBas THEN DO:
                            ASSIGN wArtBasRecid = RECID(ArtBAs).
                            LEAVE SOKBLOKK.
                        END.
                        ELSE DO:
                            MESSAGE "Ingen artikler funnet i lokalt register."
                                VIEW-AS ALERT-BOX INFO BUTTONS OK.
                            ASSIGN SELF:SCREEN-VALUE = "".
                            RETURN NO-APPLY.
                        END.
                    END.
            END. /* BLOKKEN */
            ELSE DO:
                IF cUtvidetSok BEGINS "JA" THEN
                    LEAVE LOKALSOK.
                ELSE DO:
                    MESSAGE "Ingen artikler funnet."
                        VIEW-AS ALERT-BOX INFO BUTTONS OK.
                    RETURN NO-APPLY.
                END.
            END.
        END.
    END. /* SOKBLOKK */

    DELETE OBJECT hQuery.
    APPLY "choose":U TO Btn_Ok.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-LevKod
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-LevKod Dialog-Frame
ON ANY-PRINTABLE OF FI-LevKod IN FRAME Dialog-Frame /* Lev.art.nr */
DO:
  FI-Msg:SCREEN-VALUE = ''.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-LevKod Dialog-Frame
ON ENTRY OF FI-LevKod IN FRAME Dialog-Frame /* Lev.art.nr */
DO:
  FI-Msg:SCREEN-VALUE = SELF:TOOLTIP.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-LevKod Dialog-Frame
ON TAB OF FI-LevKod IN FRAME Dialog-Frame /* Lev.art.nr */
OR "return" OF FI-LevKod
DO:
  DEF VAR bOK     AS LOG NO-UNDO.
  DEF VAR hQuery  AS HANDLE NO-UNDO.
  DEF VAR lcTekst AS CHAR NO-UNDO.

  cStartFelt = 'FI-LevKod'.


        /* Bare gå videre */
    IF FI-LevKod:SCREEN-VALUE = "" THEN
        RETURN.

    ASSIGN
        cSok = ""
        .

    RUN EkstVPINrListe (OUTPUT cEkstVPILevNrListe).
    SOKBLOKK:
    DO:
        IF INPUT FI-LevKod <> "" THEN 
        LOKALSOK:
        DO:
            ASSIGN 
                cSok = INPUT FI-LevKod + IF INPUT FI-LevKod BEGINS "*" 
                                                     THEN "*" 
                                                     ELSE "".

            CREATE QUERY hQuery.
            hQuery:SET-BUFFERS(BUFFER ArtBas:HANDLE).
            hQuery:QUERY-PREPARE("FOR EACH ArtBas NO-LOCK WHERE TRUE " 
                               + (IF INDEX(cSok,"*") > 0 THEN 
                                    " AND LevKod MATCHES '" + cSok + "*'"
                                  ELSE IF cSok NE "" THEN
                                    " AND LevKod BEGINS '" + cSok + "'"
                                  ELSE "")
                               + (IF FI-LevNr:SCREEN-VALUE NE "0" THEN
                                   " AND LevNr = " + FI-LevNr:SCREEN-VALUE  
                                  ELSE "")). 
            hQuery:QUERY-OPEN().
            hQuery:GET-FIRST().
            IF hQuery:GET-BUFFER-HANDLE(1):AVAIL THEN
            BLOKKEN:
            DO:
                    RUN dartbas_sok.w (FI-LevNr:SCREEN-VALUE,
                                       FI-LevKod:SCREEN-VALUE,
                                       FI-Beskr:SCREEN-VALUE,
                                       FI-UtvidetSok:SCREEN-VALUE,
                                       INPUT-OUTPUT cTekst).
                    IF RETURN-VALUE = "AVBRYT" THEN
                        RETURN NO-APPLY.
                    IF RETURN-VALUE = "AVBRYT" THEN DO:
                        CASE cStartFelt:
                            WHEN 'FI-LopNr' THEN APPLY 'ENTRY' TO FI-LopNr.
                            WHEN 'FI-ArtikkelNr' THEN APPLY 'ENTRY' TO FI-ArtikkelNr.
                            WHEN 'FI-LevKod' THEN APPLY 'ENTRY' TO FI-LevKod.
                            WHEN 'FI-Strekkode' THEN APPLY 'ENTRY' TO FI-Strekkode.
                            WHEN 'FI-Utvidetsok' THEN APPLY 'ENTRY' TO FI-Utvidetsok.
                            WHEN 'FI-BestNr' THEN APPLY 'ENTRY' TO FI-BestNr.
                            WHEN 'FI-ERPNr' THEN APPLY 'ENTRY' TO FI-ERPNr.
                        END CASE.
                        RETURN NO-APPLY.
                    END.
                    ELSE IF RETURN-VALUE BEGINS "VPISOK" THEN
                    DO:
                        IF cSok <> '' THEN
                            lcTekst = 'UtvidetLevKod' + CHR(1) + cSok.
                        /* Kaller søkerutine */
                        RUN gvpiartbas.w (
                          INPUT-OUTPUT cTekst2, /* Returstreng - chr(1) separert */
                          "EkstVPILevNr", /* Feltliste avgrensningsfelt (kommaseparert) */
                          "1", /* Feltverdier (chr(1) sep) */ 
                          lcTekst, /* Post markøren skal stå på */
                          wOldRecid
                          ).
                        IF RETURN-VALUE = "AVBRYT" THEN
                            RETURN NO-APPLY.
                        /* Flytter over resultatverdier */
                        cTekst = cTekst2.
                    END.
                    IF NUM-ENTRIES(cTekst,CHR(1)) >= 2 THEN
                    DO:
                        FIND ArtBas NO-LOCK WHERE
                            ArtBas.ArtikkelNr = DEC(ENTRY(2,cTekst,CHR(1))) NO-ERROR.
                        IF AVAILABLE ArtBas THEN DO:
                            ASSIGN wArtBasRecid = RECID(ArtBAs).
                            LEAVE SOKBLOKK.
                        END.
                        ELSE DO:
                            FI-Msg:SCREEN-VALUE = 'Ingen artikler funnet i lokalt register.'.
                            /*
                            MESSAGE "Ingen artikler funnet i lokalt register."
                                VIEW-AS ALERT-BOX INFO BUTTONS OK.
                            */
                            ASSIGN SELF:SCREEN-VALUE = "".
                            RETURN NO-APPLY.
                        END.
                    END.
            END. /* BLOKKEN */
            ELSE DO:
                IF cUtvidetSok BEGINS "JA" THEN
                    LEAVE LOKALSOK.
                ELSE DO:
                    FI-Msg:SCREEN-VALUE = 'Ingen artikler funnet.'.
                    /*
                    MESSAGE "Ingen artikler funnet."
                        VIEW-AS ALERT-BOX INFO BUTTONS OK.
                    */
                    RETURN NO-APPLY.
                END.
            END.
        END.
        UTVIDETSOK:
        DO iCount = 1 TO NUM-ENTRIES(cEkstVPILevNrListe):
            {sww.i}
            /* Kontrollerer mot default VPI leverandør */
            IF cSok BEGINS "*" THEN
                FIND FIRST VPIArtBas NO-LOCK WHERE
                     VPIArtBas.EkstVPILevNr = int(ENTRY(iCount,cEkstVPILevNrListe)) AND
                     VPIArtBas.LevKod MATCHES cSok AND
                     (IF FI-LevNr:SCREEN-VALUE NE "0" THEN 
                        VPIArtBas.LevNr = INT(FI-LevNr:SCREEN-VALUE)
                      ELSE TRUE)
                      NO-ERROR.
            ELSE 
                FIND FIRST VPIArtBas NO-LOCK WHERE
                    VPIArtBas.EkstVPILevNr = int(ENTRY(iCount,cEkstVPILevNrListe)) AND
                    VPIArtBas.LevKod BEGINS cSok AND
                    (IF FI-LevNr:SCREEN-VALUE NE "0" THEN 
                       VPIArtBas.LevNr = INT(FI-LevNr:SCREEN-VALUE)
                     ELSE TRUE)
                    NO-ERROR.
            {swn.i}
            IF AVAILABLE VPIArtBas THEN
            /* Varsle at ekstern VPI er funnet og gi mulighet for å kunne velge å opprette lokalt. */
            VPI-FUNNET:
            DO:
                ASSIGN
                    FI-MSg:SCREEN-VALUE = "Ekstern VPI funnet..."
                    wArtBasRecid = VPIArtBas.EkstVPILevNr * -1 /* Flagger at VPI er funnet. */
                    pcKode       = "LEVKOD" + chr(1) + cSok /* Tar vare på strekkoden */
                    .
                LEAVE SOKBLOKK.
            END. /* VPI-FUNNET */
            ELSE DO:
                IF iCount < num-entries(cEkstVPILevNrListe) THEN
                    NEXT UTVIDETSOK.
                ELSE DO:
                    FI-Msg:SCREEN-VALUE = 'Ingen artikler funnet i lokalt eller VPI register.'.
                    /*
                    MESSAGE "Ingen artikler funnet i lokalt eller VPI register." VIEW-AS ALERT-BOX MESSAGE TITLE "Melding".
                    */
                    RETURN NO-APPLY.
                END.
            END.
        END. /* UTVIDETSOK */
    END. /* SOKBLOKK */

    DELETE OBJECT hQuery.

    APPLY "choose":U TO Btn_Ok.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-LevNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-LevNr Dialog-Frame
ON ENTRY OF FI-LevNr IN FRAME Dialog-Frame /* Filter på lev.nr */
DO:
  FI-Msg:SCREEN-VALUE = SELF:TOOLTIP.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-LopNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-LopNr Dialog-Frame
ON ANY-PRINTABLE OF FI-LopNr IN FRAME Dialog-Frame
DO:
  FI-Msg:SCREEN-VALUE = ''.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-LopNr Dialog-Frame
ON ENTRY OF FI-LopNr IN FRAME Dialog-Frame
DO:
  FI-Msg:SCREEN-VALUE = SELF:TOOLTIP.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-LopNr Dialog-Frame
ON TAB OF FI-LopNr IN FRAME Dialog-Frame
OR "return" OF FI-LopNr
DO:
    cStartFelt = 'FI-LopNr'.
  IF INPUT FI-LopNr > 0 THEN
    DO:
      FIND FIRST ArtBas NO-LOCK WHERE
        ArtBas.Vg    = INPUT FI-Vg AND
        ArtBAs.LopNr = INPUT FI-LopNr NO-ERROR.
      IF NOT AVAILABLE ArtBas THEN
        DO:
          FI-Msg:SCREEN-VALUE = 'Ukjent artikkel!'.
          /*
          MESSAGE "Ukjent artikkel!" VIEW-AS ALERT-BOX MESSAGE TITLE "Melding".
          */
          RETURN NO-APPLY.
        END.
      ELSE DO:
        ASSIGN wArtBasRecid = RECID(ArtBas).
        APPLY "choose":U TO Btn_Ok.
      END.
    END.
      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-Strekkode
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Strekkode Dialog-Frame
ON ANY-PRINTABLE OF FI-Strekkode IN FRAME Dialog-Frame /* Strekkode */
DO:
  FI-Msg:SCREEN-VALUE = ''.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Strekkode Dialog-Frame
ON ENTRY OF FI-Strekkode IN FRAME Dialog-Frame /* Strekkode */
DO:
  FI-Msg:SCREEN-VALUE = SELF:TOOLTIP.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Strekkode Dialog-Frame
ON TAB OF FI-Strekkode IN FRAME Dialog-Frame /* Strekkode */
OR "return" OF FI-Strekkode
DO:
    cStartFelt = 'FI-Strekkode'.

  IF INPUT FI-Strekkode <> 0 THEN DO:
      ASSIGN   
          cStrekKode = STRING(INPUT FI-Strekkode)
          .
      /* Sjekker rått den koden som er tastet inn. */
      FIND Strekkode NO-LOCK WHERE Strekkode.Kode = cStrekkode NO-ERROR.
      
      IF NOT AVAILABLE Strekkode THEN
      DO:
          /* Plunummer er maks 5 siffer. */
          IF LENGTH(INPUT FI-Strekkode) < 6 THEN 
             cStrekKode = INPUT FI-Strekkode.
          /* UPC-E koder er på 6 siffer. Konverteres til UPC-A og derfra til EAN13 kode.*/
          ELSE IF LENGTH(INPUT FI-Strekkode) = 6 THEN
            DO: 
              cStrekKode = INPUT FI-Strekkode. 
              RUN bibl_chkean.p (INPUT-OUTPUT cStrekKode).
              FI-Strekkode:SCREEN-VALUE = cStrekKode.
            END. 
          /* EAN 13 kode */
          ELSE DO:
              cStrekKode = STRING(INPUT FI-Strekkode,"9999999999999"). 
              RUN bibl_chkean.p (INPUT-OUTPUT cStrekKode).
              FI-Strekkode:SCREEN-VALUE = cStrekKode.
          END.

          /* Sjekker med korrigert strekkode. */
          FIND Strekkode NO-LOCK WHERE Strekkode.Kode = cStrekkode NO-ERROR.
      END.
      /* Sjekker uten nullutfylling. */
      IF NOT AVAILABLE Strekkode THEN
      DO:
          FIND Strekkode NO-LOCK WHERE Strekkode.Kode = LEFT-TRIM(cStrekkode,"0") NO-ERROR.
          IF AVAILABLE Strekkode THEN DO:
              cStrekkode = LEFT-TRIM(cStrekkode,"0").
              FI-Strekkode:SCREEN-VALUE = cStrekKode.
          END.         
      END.
      /* Sjekker mot VPI registeret */
      IF NOT AVAILABLE Strekkode AND cUtvidetSok <> "" THEN
        VPI-SJEKK:
        DO:
          RUN EkstVPINrListe (OUTPUT cEkstVPILevNrListe).
          IF NUM-ENTRIES(cUtvidetSok,CHR(1)) > 1 THEN
          ASSIGN
              cEkstVPILevNrListe = ENTRY(2,cUtvidetSok,CHR(1)) +
                                   (IF cEkstVPILevNrListe = ""
                                      THEN ""
                                      ELSE ",") + 
                                   cEkstVPILevNrListe.
          /* Sjekker VPI leverandørene i prioritert rekkefølge. */
          DO iCount = 1 TO NUM-ENTRIES(cEkstVPILevNrListe): /* i prioritert rekkef. */
              FIND FIRST VPIStrekkode NO-LOCK WHERE
                  VPIStrekkode.EkstVPILevNr = INT(ENTRY(iCount,cEkstVPILevNrListe)) AND
                  VPIStrekkode.Kode         = cStrekkode NO-ERROR.

              IF AVAILABLE VPIStrekkode THEN
              /* Varsle at ekstern VPI er funnet og gi mulighet for å kunne velge å opprette lokalt. */
              VPI-FUNNET:
              DO:
                  ASSIGN
                      wArtBasRecid = VPIStrekkode.EkstVPILevNr * -1 /* Flagger at VPI er funnet. */
                      pcKode       = IF VPIStrekkode.EkstVPILevNr >= 100
                                       THEN cStrekkode          /* Tar vare på strekkoden */
                                       ELSE VPIStrekkode.VareNr /* Bruker artikkelnr      */
                      .
                  LEAVE.
              END. /* VPI-FUNNET */
          END.
          IF wArtBasRecid < 0 AND wArtBasRecid <> ? THEN DO:
              APPLY "choose":U TO Btn_Ok.
              RETURN NO-APPLY.
          END.
          ELSE DO:
              FI-Msg:SCREEN-VALUE = 'Ukjent strekkode! (Lokalt og i VPI register)'.
              /*
              MESSAGE "Ukjent strekkode! (Lokalt og i VPI register)" VIEW-AS ALERT-BOX MESSAGE TITLE "Melding".
              */
              RETURN NO-APPLY.
          END.
        END. /* VPI-SJEKK */
      ELSE DO:
        FIND ArtBas NO-LOCK WHERE ArtBas.ArtikkelNr = StrekKode.ArtikkelNr NO-ERROR.
        IF AVAIL ArtBas THEN DO:
            ASSIGN wArtBasRecid = RECID(ArtBAs).
            APPLY "choose":U TO Btn_Ok.
        END.
        ELSE DO:
            FI-Msg:SCREEN-VALUE = 'Strekkode finnes, ukjent artikkel. Kontakt systemadministratør.'.
            /*
            MESSAGE "Strekkode finnes, ukjent artikkel. Kontakt systemadministratør." SKIP
                cStrekkode
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            */
            RETURN NO-APPLY.
        END.
      END.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-UtvidetSok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-UtvidetSok Dialog-Frame
ON ANY-PRINTABLE OF FI-UtvidetSok IN FRAME Dialog-Frame /* Utvidet søk */
DO:
  FI-Msg:SCREEN-VALUE = ''.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-UtvidetSok Dialog-Frame
ON ENTRY OF FI-UtvidetSok IN FRAME Dialog-Frame /* Utvidet søk */
DO:
  FI-Msg:SCREEN-VALUE = SELF:TOOLTIP.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-UtvidetSok Dialog-Frame
ON TAB OF FI-UtvidetSok IN FRAME Dialog-Frame /* Utvidet søk */
OR "return" OF FI-UtvidetSok
DO:
  DEF VAR bOK     AS LOG NO-UNDO.
  DEF VAR hQuery  AS HANDLE NO-UNDO.
  DEF VAR lcTekst AS CHAR NO-UNDO.

  cStartFelt = 'FI-UtvidetSok'.

        /* Bare gå videre */
    IF FI-UtvidetSok:SCREEN-VALUE = "" THEN
        RETURN.
    IF SUBSTRING(TRIM(FI-UtvidetSok:SCREEN-VALUE),1,1) = "*" THEN
    DO:
        MESSAGE "'*' kan bare benyttes i enden av et ord. Det kan ikke ligge først i søkebegrepet."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN NO-APPLY.
    END.

    ASSIGN
        FI-UtvidetSok:SCREEN-VALUE = CAPS(FI-UtvidetSok:SCREEN-VALUE)
        cTekst2 = "Utvidetsok=" + CAPS(FI-UtvidetSok:SCREEN-VALUE)
        cSok   = ""
        .

    RUN EkstVPINrListe (OUTPUT cEkstVPILevNrListe).
    SOKBLOKK:
    DO:
        IF INPUT FI-UtvidetSok <> "" THEN 
        LOKALSOK:
        DO:
            ASSIGN 
                cSok = INPUT FI-UtvidetSok + IF INPUT FI-UtvidetSok BEGINS "*" 
                                                     THEN "*" 
                                                     ELSE "".

            CREATE QUERY hQuery.
            hQuery:SET-BUFFERS(BUFFER ArtBas:HANDLE).
            hQuery:QUERY-PREPARE("FOR EACH ArtBas NO-LOCK WHERE TRUE " 
                               + (IF cSok NE "" THEN
                                    " AND UtvidetSok contains '" + cSok + "'"
                                  ELSE "")
                               + (IF FI-LevNr:SCREEN-VALUE NE "0" THEN
                                   " AND LevNr = " + FI-LevNr:SCREEN-VALUE 
                                  ELSE "")). 
            hQuery:QUERY-OPEN().
            hQuery:GET-FIRST().
            IF hQuery:GET-BUFFER-HANDLE(1):AVAIL THEN

            BLOKKEN:
            DO:
                    RUN dartbas_sok.w (FI-LevNr:SCREEN-VALUE,
                                       FI-LevKod:SCREEN-VALUE,
                                       FI-Beskr:SCREEN-VALUE,
                                       FI-UtvidetSok:SCREEN-VALUE,
                                       INPUT-OUTPUT cTekst).
                    IF RETURN-VALUE = "AVBRYT" THEN DO:
                        CASE cStartFelt:
                            WHEN 'FI-LopNr' THEN APPLY 'ENTRY' TO FI-LopNr.
                            WHEN 'FI-ArtikkelNr' THEN APPLY 'ENTRY' TO FI-ArtikkelNr.
                            WHEN 'FI-LevKod' THEN APPLY 'ENTRY' TO FI-LevKod.
                            WHEN 'FI-Strekkode' THEN APPLY 'ENTRY' TO FI-Strekkode.
                            WHEN 'FI-Utvidetsok' THEN APPLY 'ENTRY' TO FI-Utvidetsok.
                            WHEN 'FI-BestNr' THEN APPLY 'ENTRY' TO FI-BestNr.
                            WHEN 'FI-ERPNr' THEN APPLY 'ENTRY' TO FI-ERPNr.
                        END CASE.
                        RETURN NO-APPLY.
                    END.
                    ELSE IF RETURN-VALUE BEGINS "VPISOK" THEN
                    DO:
                    
                        IF cSok <> '' THEN
                            lcTekst = "UtvidetSok" + chr(1) + cSok /*ENTRY(2,RETURN-VALUE,CHR(1)) */.
                        /* Kaller søkerutine */
                        RUN gvpiartbas.w (
                          INPUT-OUTPUT cTekst2, /* Returstreng - chr(1) separert */
                          "EkstVPILevNr", /* Feltliste avgrensningsfelt (kommaseparert) */
                          "1", /* Feltverdier (chr(1) sep) */ 
                          lcTekst, /* Post markøren skal stå på */
                          wOldRecid
                          ).
                        IF RETURN-VALUE = "AVBRYT" THEN
                            RETURN NO-APPLY.
                        /* Flytter over resultatverdier */
                        cTekst = cTekst2.
                    END.
                    IF NUM-ENTRIES(cTekst,CHR(1)) >= 2 THEN
                    DO:
                        FIND ArtBas NO-LOCK WHERE
                            ArtBas.ArtikkelNr = DEC(ENTRY(2,cTekst,CHR(1))) NO-ERROR.
                        IF AVAILABLE ArtBas THEN DO:
                            ASSIGN wArtBasRecid = RECID(ArtBAs).
                            LEAVE SOKBLOKK.
                        END.
                        ELSE DO:
                            FI-Msg:SCREEN-VALUE = 'Ingen artikler funnet i lokalt register.'.
                            ASSIGN SELF:SCREEN-VALUE = "".
                            RETURN NO-APPLY.
                        END.
                    END.
            END. /* BLOKKEN */
            ELSE DO:
                IF cUtvidetSok BEGINS "JA" THEN
                    LEAVE LOKALSOK.
                ELSE DO:
                    FI-Msg:SCREEN-VALUE = 'Ingen artikler funnet.'.
                    RETURN NO-APPLY.
                END.
            END.
        END.
        UTVIDETSOK:
        DO iCount = 1 TO NUM-ENTRIES(cEkstVPILevNrListe):
            rUtvidetSok = ?.
            {sww.i}
            /* Kontrollerer mot default VPI leverandør */
            IF cSok BEGINS "*" THEN
                FIND FIRST VPIArtBas NO-LOCK WHERE
                     VPIArtBas.EkstVPILevNr = int(ENTRY(iCount,cEkstVPILevNrListe)) AND
                     VPIArtBas.UtvidetSok MATCHES cSok AND
                     (IF FI-LevNr:SCREEN-VALUE NE "0" THEN 
                        VPIArtBas.LevNr = INT(FI-LevNr:SCREEN-VALUE)
                      ELSE TRUE)
                      NO-ERROR.
            ELSE DO:
                FOR EACH VPIArtBas NO-LOCK WHERE
                    VPIArtBas.EkstVPILevNr = int(ENTRY(iCount,cEkstVPILevNrListe)) AND
                    VPIArtBas.UtvidetSok CONTAINS cSok AND
                    (IF FI-LevNr:SCREEN-VALUE NE "0" THEN 
                       VPIArtBas.LevNr = INT(FI-LevNr:SCREEN-VALUE)
                     ELSE TRUE):
                     rUtvidetSok = RECID(VPIArtBas).
                END.
                IF rUtvidetSok <> ? THEN
                    FIND VPIArtBas NO-LOCK WHERE
                    RECID(VPIArtBas) = rUtvidetSok NO-ERROR.
            END.
            
            {swn.i}
            IF AVAILABLE VPIArtBas THEN
            /* Varsle at ekstern VPI er funnet og gi mulighet for å kunne velge å opprette lokalt. */
            VPI-FUNNET:
            DO:
                ASSIGN
                    FI-MSg:SCREEN-VALUE = "Ekstern VPI funnet..."
                    wArtBasRecid = VPIArtBas.EkstVPILevNr * -1 /* Flagger at VPI er funnet. */
                    pcKode       = "UtvidetSok" + chr(1) + cSok /* Tar vare på strekkoden */
                    .
                LEAVE SOKBLOKK.
            END. /* VPI-FUNNET */
            ELSE DO:
                IF iCount < num-entries(cEkstVPILevNrListe) THEN
                    NEXT UTVIDETSOK.
                ELSE DO:
                    FI-Msg:SCREEN-VALUE = 'Ingen artikler funnet i lokalt eller VPI register.'.
                    RETURN NO-APPLY.
                END.
            END.
        END. /* UTVIDETSOK */
    END. /* SOKBLOKK */

    DELETE OBJECT hQuery.

    APPLY "choose":U TO Btn_Ok.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-Vg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Vg Dialog-Frame
ON ANY-PRINTABLE OF FI-Vg IN FRAME Dialog-Frame /* Vg/LøpeNr */
DO:
  FI-Msg:SCREEN-VALUE = ''.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Vg Dialog-Frame
ON ENTRY OF FI-Vg IN FRAME Dialog-Frame /* Vg/LøpeNr */
DO:
  FI-Msg:SCREEN-VALUE = SELF:TOOLTIP.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Vg Dialog-Frame
ON TAB OF FI-Vg IN FRAME Dialog-Frame /* Vg/LøpeNr */
OR "return" OF FI-Vg
DO:
  cStartFelt = 'FI-LopNr'.
  IF INPUT FI-Vg > 0 THEN
    DO:
      FIND VarGr NO-LOCK WHERE
        VarGr.Vg = INPUT FI-Vg NO-ERROR.
      IF NOT AVAILABLE VarGr THEN
        DO:
          FI-Msg:SCREEN-VALUE = 'Ukjent varegruppe!'.
          /*
          MESSAGE "Ukjent varegruppe!" VIEW-AS ALERT-BOX MESSAGE TITLE "Melding".
          */
          RETURN NO-APPLY.
        END.
    END.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT EQ ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.

/* Dette kallet benyttes fra vreklamasjonslinje.w */
IF cUtvidetSok BEGINS "LEVNR" THEN
    ASSIGN
    iLevNr      = int(ENTRY(2,cUtvidetSok,"="))
    cUtvidetSok = ""
    .

ASSIGN
  wArtBasRecid = ?.
/* HK innstallasjon */
{syspara.i 1 1 18 cTekst}
IF CAN-DO("yes,1,ja,true",cTekst) THEN
    bHk = TRUE.
ELSE 
    bHk = FALSE.
{syspara.i 2 4 18 cSokStart}
{syspara.i 2 4 21 iSkjulFelt INT}

/* Default VPI leverandør. */
{syspara.i 1 12 1 piEkstVPILevNr INT}
RUN EkstVPINrListe (OUTPUT cEkstVPILevNrListe).
/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  {lng.i} RUN enable_UI.

  IF iLevNr > 0 AND
      CAN-FIND(LevBas WHERE LevBas.LevNr = iLevNr) THEN
      ASSIGN
      FI-LevNr:SCREEN-VALUE = STRING(iLevNr)
      /*
      FI-LevNr:SENSITIVE = FALSE
      BtnLevNr:SENSITIVE = FALSE
      */
      .
  IF iSkjulFelt = 1 THEN
      ASSIGN
      FI-LevKod:HIDDEN = TRUE
      FI-BestNr:HIDDEN = TRUE
      FI-ERPNr:HIDDEN  = TRUE
      FI-Beskr:hidden  = TRUE.

  IF cSokStart = "0" THEN APPLY "ENTRY" TO FI-Strekkode IN FRAME {&FRAME-NAME}.
  ELSE IF cSokStart = "1" THEN APPLY "ENTRY" TO FI-Vg IN FRAME {&FRAME-NAME}.
  ELSE APPLY "ENTRY" TO FI-UtvidetSok IN FRAME {&FRAME-NAME}.
  /*
  IF CAN-DO(SOURCE-PROCEDURE:INTERNAL-ENTRIES,"SetAltS-param") THEN
    RUN SetAltS-param IN SOURCE-PROCEDURE (THIS-PROCEDURE).  
  */
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.

  IF wArtBasRecid = ? THEN
      wArtikkelNr = ?.
  ELSE IF wArtBasRecid < 0 THEN
      wArtikkelNr = DEC(wArtBasRecid).
  ELSE DO:
      FIND ArtBas NO-LOCK WHERE
          RECID(ArtBas) = wArtBasRecid NO-ERROR.
      IF AVAILABLE ArtBas THEN
          wArtikkelNr = ArtBas.ArtikkelNr.
  END.
END.
RUN disable_UI.

RETURN pcKode. /* Returnerer strekkode på VPI artikkel hvis den er funnet. */
               /* Ellers er denne blank.                                   */

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EkstVPINrListe Dialog-Frame 
PROCEDURE EkstVPINrListe :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF OUTPUT PARAMETER pcEkstVPILEvNrListe AS CHAR NO-UNDO.

  DEF VAR piInt AS INT NO-UNDO.

  IF NUM-ENTRIES(cUtvidetSok,CHR(1)) > 1 THEN
      piInt = int(ENTRY(2,cUtvidetSok,CHR(1))).
  ELSE
      piInt = 0.

  /* 
  {syspara.i 1 12 4 pcEkstVPILEvNrListe}
  */

  FOR EACH EkstVPILev NO-LOCK WHERE
      (IF bHk 
         THEN EkstVPILev.EkstVPILevNr > 100 
         ELSE TRUE) AND
      (IF bHk 
         THEN EkstVPILev.EkstVPILevNr <= 999 
         ELSE TRUE) AND
      EkstVPILev.AktivLev = TRUE
      BREAK BY EkstVPILev.Prio:

      pcEkstVPILEvNrListe = pcEkstVPILEvNrListe + 
                            (IF pcEkstVPILEvNrListe = ""
                               THEN ""
                               ELSE ",") +
                            STRING(EkstVPILev.EkstVPILevNr).
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
  DISPLAY FI-LevNr FI-Vg FI-LopNr FI-ArtikkelNr FI-LevKod FI-Beskr FI-Strekkode 
          FI-UtvidetSok FI-BestNr FI-ERPNr FI-Msg 
      WITH FRAME Dialog-Frame.
  ENABLE BtnLevNr RECT-49 RECT-62 RECT-64 RECT-67 RECT-68 FI-LevNr FI-Vg 
         FI-LopNr FI-ArtikkelNr FI-LevKod FI-Beskr FI-Strekkode FI-UtvidetSok 
         FI-BestNr FI-ERPNr B-Sok Btn_OK 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION InitScreen Dialog-Frame 
FUNCTION InitScreen RETURNS LOGICAL
  ( INPUT icFieldValueFocus AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose: Assign initial value to one field and set focus (to another)
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR ix AS INT NO-UNDO.

/* DO ix = 1 TO NUM-ENTRIES(icFieldValueFocus) BY 2 WITH FRAME {&FRAME-NAME}:         */
/*   IF ix = 1 AND ENTRY(ix,icFieldValueFocus) NE "" THEN DO:                         */
/*     CASE ENTRY(ix,icFieldValueFocus):                                              */
/*       WHEN "Vg"     THEN FI-Vg:SCREEN-VALUE = ENTRY(ix + 1,icFieldValueFocus).     */
/*       WHEN "LevNr"  THEN FI-LevNr:SCREEN-VALUE = ENTRY(ix + 1,icFieldValueFocus).  */
/*       WHEN "LevKod" THEN FI-LevKod:SCREEN-VALUE = ENTRY(ix + 1,icFieldValueFocus). */
/*     END CASE.                                                                      */
/*   END.                                                                             */
/*   ELSE IF ix = 3 AND ENTRY(ix,icFieldValueFocus) NE "" THEN                        */
/*     CASE ENTRY(ix,icFieldValueFocus):                                              */
/*       WHEN "Vg"     THEN APPLY "entry" TO FI-Vg.                                   */
/*       WHEN "LevNr"  THEN APPLY "entry" TO FI-LevNr.                                */
/*       WHEN "LevKod" THEN APPLY "entry" TO FI-LevKod.                               */
/*     END CASE.                                                                      */
/* END.                                                                               */

RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

