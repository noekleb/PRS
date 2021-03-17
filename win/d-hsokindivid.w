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
  def var wArtikkelNr as DEC no-undo.
  DEF VAR cUtvidetSok  AS CHAR  NO-UNDO.
&ELSE
  def output parameter wArtikkelNr as dec no-undo.
  DEF INPUT PARAMETER cUtvidetSok AS CHARACTER NO-UNDO  /* om blank bara sök i artbas */.
&ENDIF

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
DEF VAR cTekst         AS CHAR NO-UNDO.
DEF VAR piEkstVPILevNr AS INT  NO-UNDO.
DEF VAR pcKode         AS CHAR NO-UNDO.
DEF VAR cEkstVPILevNrListe AS CHAR NO-UNDO.
DEF VAR plVPISjekk     AS LOG  NO-UNDO.
DEFINE VARIABLE iCount AS INTEGER    NO-UNDO.
DEFINE VARIABLE cStrekKode AS CHARACTER  NO-UNDO.

DEF VAR cSok         AS CHARACTER  NO-UNDO.
def var wArtBasRecid as recid      no-undo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-49 FI-Vg FI-LopNr FI-ArtikkelNr ~
FI-Strekkode FI-Beskr FI-Bongtekst FI-LevKod Btn_OK 
&Scoped-Define DISPLAYED-OBJECTS FI-Vg FI-LopNr FI-ArtikkelNr FI-Strekkode ~
FI-Beskr FI-Bongtekst FI-LevKod 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_OK AUTO-GO DEFAULT 
     LABEL "Avslutt" 
     SIZE 49 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE FI-ArtikkelNr AS DECIMAL FORMAT "zzzzzzzzzzzz9":U INITIAL 0 
     LABEL "ArtikkelNr" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Beskr AS CHARACTER FORMAT "X(256)":U 
     LABEL "Varetekst" 
     VIEW-AS FILL-IN 
     SIZE 34 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Bongtekst AS CHARACTER FORMAT "X(256)":U 
     LABEL "Bongtekst" 
     VIEW-AS FILL-IN 
     SIZE 34 BY 1 NO-UNDO.

DEFINE VARIABLE FI-LevKod AS CHARACTER FORMAT "X(256)":U 
     LABEL "Lev.art.nr" 
     VIEW-AS FILL-IN 
     SIZE 34 BY 1 NO-UNDO.

DEFINE VARIABLE FI-LopNr AS INTEGER FORMAT "zzz9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Strekkode AS DECIMAL FORMAT "zzzzzzzzzzzz9":U INITIAL 0 
     LABEL "Strekkode" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Vg AS INTEGER FORMAT "zzzzz9":U INITIAL 0 
     LABEL "Vg/LøpeNr" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-49
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 49 BY 7.86.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     FI-Vg AT ROW 1.71 COL 14 COLON-ALIGNED
     FI-LopNr AT ROW 1.71 COL 24 COLON-ALIGNED NO-LABEL
     FI-ArtikkelNr AT ROW 2.91 COL 14 COLON-ALIGNED
     FI-Strekkode AT ROW 4.1 COL 14 COLON-ALIGNED
     FI-Beskr AT ROW 5.29 COL 14 COLON-ALIGNED
     FI-Bongtekst AT ROW 6.48 COL 14 COLON-ALIGNED
     FI-LevKod AT ROW 7.67 COL 14 COLON-ALIGNED
     Btn_OK AT ROW 9.33 COL 2
     RECT-49 AT ROW 1.24 COL 2
     SPACE(0.00) SKIP(1.37)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Individsøk".


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

{dproclibstart.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
                                                                        */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

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
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Individsøk */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-ArtikkelNr
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

or "return" of FI-ArtikkelNr
DO:
  if input FI-ArtikkelNr <> 0 THEN do:
      ASSIGN cStrekkode = FI-ArtikkelNr:SCREEN-VALUE.
      find ArtBas no-lock WHERE ArtBas.ArtikkelNr = dec(cStrekkode) no-error.

      IF (AVAILABLE ArtBas AND ArtBas.ArtSlag <= 1 AND ArtBas.IndividType = 0) THEN
      DO:
          message "Artikkelen er ikke en individartikkel!" view-as alert-box message title "Melding".
          return no-apply.
      END.

      IF AVAIL ArtBas THEN DO:
          assign wArtBasRecid = recid(ArtBAs).
          apply "choose":U to Btn_Ok.
      END.
      ELSE DO:
          MESSAGE "Ukjent artikkelnummer." SKIP
              cStrekkode
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
          RETURN NO-APPLY.
      END.
  end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-Beskr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Beskr Dialog-Frame
ON TAB OF FI-Beskr IN FRAME Dialog-Frame /* Varetekst */
or "return" of FI-Beskr
DO:
        /* Bare gå videre */
    IF FI-Beskr:SCREEN-VALUE = "" THEN
        RETURN.
    ASSIGN
        cSok = ""
        .

    SOKBLOKK:
    DO:
        IF INPUT FI-Beskr <> "" THEN 
        LOKALSOK:
        DO:
            ASSIGN cSok = INPUT FI-Beskr + IF INPUT FI-Beskr BEGINS "*" THEN "*" ELSE "".
            IF (cSok BEGINS "*" AND CAN-FIND(FIRST ArtBas WHERE ArtBas.ArtSlag <= 1 and ArtBas.IndividType > 0 AND ArtBas.Beskr MATCHES cSok)) OR
               (NOT cSok BEGINS "*" AND CAN-FIND(FIRST ArtBas WHERE ArtBas.ArtSlag <= 1 and ArtBas.IndividType > 0 AND ArtBas.Beskr BEGINS cSok)) THEN
                BLOKKEN:
                DO:
                    /* Kaller søkerutine */
                    RUN gartbassok.w (
                    INPUT-OUTPUT cTekst, /* Returstreng - chr(1) separert */
                    "Beskr,ArtSlag,Individtype", /* Feltliste avgrensningsfelt (kommaseparert) */
                    INPUT cSok + CHR(1) + "2" + CHR(1) + "0", /* Feltverdier (chr(1) sep) */ 
                    (IF entry(1,cSok,CHR(1)) BEGINS "*"
                       THEN "MATCHES"
                       ELSE "BEGINS" + ",<,>")
                    ).
                    IF RETURN-VALUE = "AVBRYT" THEN
                        RETURN NO-APPLY.
                    IF NUM-ENTRIES(cTekst,CHR(1)) >= 2 THEN
                    DO:
                        FIND ArtBas NO-LOCK WHERE
                            ArtBas.ArtikkelNr = DEC(ENTRY(2,cTekst,CHR(1))) NO-ERROR.
                        IF AVAILABLE ArtBas THEN DO:
                            assign wArtBasRecid = recid(ArtBAs).
                            LEAVE SOKBLOKK.
                        END.
                        ELSE DO:
                            MESSAGE "Ingen artikler funnet."
                                VIEW-AS ALERT-BOX INFO BUTTONS OK.
                            ASSIGN SELF:SCREEN-VALUE = "".
                            RETURN NO-APPLY.
                        END.
                    END.
            END. /* BLOKKEN */
            ELSE DO:
                MESSAGE "Ingen individartikler funnet i lokalt register."
                    VIEW-AS ALERT-BOX INFO BUTTONS OK.
                RETURN NO-APPLY.
            END.
        END. /* LOKALSOK */
    END. /* SOKBLOKK */
    apply "choose":U to Btn_Ok.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-Bongtekst
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Bongtekst Dialog-Frame
ON TAB OF FI-Bongtekst IN FRAME Dialog-Frame /* Bongtekst */
or "return" of FI-Bongtekst
DO:
    /* Bare gå videre */
    IF FI-BongTekst:SCREEN-VALUE = "" THEN
        RETURN.

    ASSIGN
        cSok = ""
        .

    SOKBLOKK:
    DO:
        if input FI-BongTekst <> "" THEN 
        LOKALSOK:
        do:
            ASSIGN cSok = INPUT FI-BongTekst + IF INPUT FI-BongTekst BEGINS "*" THEN "*" ELSE "".
            IF (cSok BEGINS "*" AND CAN-FIND(FIRST ArtBas WHERE  ArtBas.ArtSlag <= 1 and ArtBas.IndividType > 0 AND  ArtBas.BongTekst MATCHES cSok)) OR
               (NOT cSok BEGINS "*" AND CAN-FIND(FIRST ArtBas WHERE  ArtBas.ArtSlag <= 1 and ArtBas.IndividType > 0 AND  ArtBas.BongTekst BEGINS cSok)) THEN
                BLOKKEN:
                DO:
                    RUN gartbassok.w (
                      INPUT-OUTPUT cTekst, /* Returstreng - chr(1) separert */
                      "BongTekst,ArtSlag,Individtype", /* Feltliste avgrensningsfelt (kommaseparert) */
                      INPUT cSok + CHR(1) + "2" + CHR(1) + "0", /* Feltverdier (chr(1) sep) */ 
                      (IF entry(1,cSok,CHR(1)) BEGINS "*"
                         THEN "MATCHES"
                         ELSE "BEGINS" + ",<,>")
                      ).

                    IF RETURN-VALUE = "AVBRYT" THEN
                        RETURN NO-APPLY.
                    IF NUM-ENTRIES(cTekst,CHR(1)) >= 2 THEN
                    DO:
                        FIND ArtBas NO-LOCK WHERE
                            ArtBas.ArtikkelNr = DEC(ENTRY(2,cTekst,CHR(1))) NO-ERROR.
                        IF AVAILABLE ArtBas THEN DO:
                            assign wArtBasRecid = recid(ArtBAs).
                            LEAVE SOKBLOKK.
                        END.
                        ELSE DO:
                            MESSAGE "Ingen artikler funnet."
                                VIEW-AS ALERT-BOX INFO BUTTONS OK.
                            ASSIGN SELF:SCREEN-VALUE = "".
                            RETURN NO-APPLY.
                        END.
                    END.
            END. /* BLOKKEN */
            ELSE DO:
                    MESSAGE "Ingen individartikler funnet i lokalt register."
                        VIEW-AS ALERT-BOX INFO BUTTONS OK.
                    RETURN NO-APPLY.
            END.
        END. /* LOKALSOK */
    END. /* SOKBLOKK */

    apply "choose":U to Btn_Ok.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-LevKod
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-LevKod Dialog-Frame
ON TAB OF FI-LevKod IN FRAME Dialog-Frame /* Lev.art.nr */
or "return" of FI-LevKod
DO:
        /* Bare gå videre */
    IF FI-LevKod:SCREEN-VALUE = "" THEN
        RETURN.

    ASSIGN
        cSok = ""
        .

    SOKBLOKK:
    DO:
        IF INPUT FI-LevKod <> "" THEN 
        LOKALSOK:
        DO:
            ASSIGN 
                cSok = INPUT FI-LevKod + IF INPUT FI-LevKod BEGINS "*" 
                                                     THEN "*" 
                                                     ELSE "".
            IF (cSok BEGINS "*" AND 
                CAN-FIND(FIRST ArtBas WHERE  ArtBas.ArtSlag <= 1 and ArtBas.IndividType > 0 AND 
                               ArtBas.LevKod MATCHES cSok)) OR
               (NOT cSok BEGINS "*" AND 
                    CAN-FIND(FIRST ArtBas WHERE  ArtBas.ArtSlag <= 1 and ArtBas.IndividType > 0 AND 
                                   ArtBas.LevKod BEGINS cSok)) THEN
                BLOKKEN:
                DO:
                    /* Kaller søkerutine */
                    RUN gartbassok.w (
                      INPUT-OUTPUT cTekst, /* Returstreng - chr(1) separert */
                      "LevKod,ArtSlag,Individtype", /* Feltliste avgrensningsfelt (kommaseparert) */
                      INPUT cSok + CHR(1) + "2" + CHR(1) + "0", /* Feltverdier (chr(1) sep) */ 
                      (IF entry(1,cSok,CHR(1)) BEGINS "*"
                         THEN "MATCHES"
                         ELSE "BEGINS" + ",<,>")
                      ).
                    IF RETURN-VALUE = "AVBRYT" THEN
                        RETURN NO-APPLY.
                    IF NUM-ENTRIES(cTekst,CHR(1)) >= 2 THEN
                    DO:
                        FIND ArtBas NO-LOCK WHERE
                            ArtBas.ArtikkelNr = DEC(ENTRY(2,cTekst,CHR(1))) NO-ERROR.
                        IF AVAILABLE ArtBas THEN DO:
                            assign wArtBasRecid = recid(ArtBAs).
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
                    MESSAGE "Ingen individartikler funnet."
                        VIEW-AS ALERT-BOX INFO BUTTONS OK.
                    RETURN NO-APPLY.
            END.
        END.
    END. /* SOKBLOKK */

    apply "choose":U to Btn_Ok.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-LopNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-LopNr Dialog-Frame
ON TAB OF FI-LopNr IN FRAME Dialog-Frame
or "return" of FI-LopNr
DO:
  if input FI-LopNr > 0 then
    do:
      find first ArtBas no-lock where
        ArtBas.Vg    = input FI-Vg and
        ArtBAs.LopNr = input FI-LopNr no-error.

      if not available ArtBas then
        do:
          message "Ukjent artikkel!" view-as alert-box message title "Melding".
          return no-apply.
        end.
      ELSE IF  (ArtBas.ArtSlag <= 1 AND ArtBAs.IndividType = 0) THEN
      DO:
          message "Artikkelen er ikke en individartikkel!" view-as alert-box message title "Melding".
          return no-apply.
      END.
      else do:
        assign wArtBasRecid = recid(ArtBas).
        apply "choose":U to Btn_Ok.
      end.
    end.
      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-Strekkode
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Strekkode Dialog-Frame
ON TAB OF FI-Strekkode IN FRAME Dialog-Frame /* Strekkode */
or "return" of FI-Strekkode
DO:
  if input FI-Strekkode <> 0 THEN 
    do:
      ASSIGN   
          cStrekKode = ""
          cStrekKode = IF LENGTH(INPUT FI-Strekkode) < 6 
                         THEN STRING(INPUT FI-Strekkode) 
                         ELSE IF LENGTH(INPUT FI-Strekkode) = 6 
                              THEN DYNAMIC-FUNCTION('fixChkEAN':U IN h_dproclib, 
                                   INPUT DYNAMIC-FUNCTION('EkspanderUPC':U IN h_dproclib, INPUT STRING(INPUT FI-StrekKode)))
                              ELSE STRING(INPUT FI-Strekkode,"9999999999999").
      /* Sjekksifferkontroll */
      IF (LENGTH(cStrekkode) = 13 OR 
          LENGTH(cStrekkode) = 8) THEN
      ASSIGN cStrekkode = DYNAMIC-FUNCTION('EANprefixKonv':U IN h_dproclib, INPUT cStrekkode).

      /* Sjekker med nullutfylling. */
      find Strekkode no-lock WHERE Strekkode.Kode = cStrekkode no-error.
      /* Sjekker uten nullutfylling. */
      IF NOT AVAILABLE Strekkode THEN
      DO:
          find Strekkode no-lock WHERE Strekkode.Kode = LEFT-TRIM(cStrekkode,"0") no-error.
          IF AVAILABLE Strekkode THEN
              cStrekkode = LEFT-TRIM(cStrekkode,"0").         
      END.
      IF AVAILABLE STrekkode THEN
          FIND ArtBas NO-LOCK WHERE ArtBas.ArtikkelNr = StrekKode.ArtikkelNr NO-ERROR.

      IF AVAILABLE ArtBas AND (ArtBas.ArtSlag <= 1 AND ArtBAs.IndividType = 0) THEN
      DO:
          message "Artikkelen er ikke en individartikkel!" view-as alert-box message title "Melding".
          return no-apply.
      END.
      ELSE IF AVAIL ArtBas THEN DO:
            assign wArtBasRecid = recid(ArtBAs).
            apply "choose":U to Btn_Ok.
      END.
      ELSE DO:
          MESSAGE "Strekkode finnes, ukjent artikkel. Kontakt systemadministratør." SKIP
              cStrekkode
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
          RETURN NO-APPLY.
      END.
    end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-Vg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Vg Dialog-Frame
ON TAB OF FI-Vg IN FRAME Dialog-Frame /* Vg/LøpeNr */
or "return" of FI-Vg
DO:
  if input FI-Vg > 0 then
    do:
      find VarGr no-lock where
        VarGr.Vg = input FI-Vg no-error.
      if not available VarGr then
        do:
          message "Ukjent varegruppe!" view-as alert-box message title "Melding".
          return no-apply.
        end.
    end.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.

assign
  wArtBasRecid = ?.

/* Default VPI leverandør. */
{syspara.i 1 12 1 piEkstVPILevNr INT}
RUN EkstVPINrListe (OUTPUT cEkstVPILevNrListe).
/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  {lng.i} RUN enable_UI.
  APPLY "ENTRY" TO FI-Strekkode IN FRAME {&FRAME-NAME}.
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
      EkstVPILev.AktivLev = TRUE
      BREAK BY EkstVPILev.Prio:

      IF EkstVPILev.EkstVPILevNr <> piInt THEN
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
  DISPLAY FI-Vg FI-LopNr FI-ArtikkelNr FI-Strekkode FI-Beskr FI-Bongtekst 
          FI-LevKod 
      WITH FRAME Dialog-Frame.
  ENABLE RECT-49 FI-Vg FI-LopNr FI-ArtikkelNr FI-Strekkode FI-Beskr 
         FI-Bongtekst FI-LevKod Btn_OK 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

