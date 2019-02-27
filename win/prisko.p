&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : 
    Purpose     :

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
/* Kalkulasjonsvariabler. */
DEF VAR wKalkModus   AS INT NO-UNDO.
DEF VAR FI-ValPris   AS DEC NO-UNDO.
DEF VAR FI-InnPris   AS DEC NO-UNDO.
DEF VAR FI-Rab1      AS DEC NO-UNDO.
DEF VAR FI-Rab1%     AS DEC NO-UNDO.
DEF VAR FI-Rab2      AS DEC NO-UNDO.
DEF VAR FI-Rab2%     AS DEC NO-UNDO.
DEF VAR FI-Frakt     AS DEC NO-UNDO.
DEF VAR FI-Frakt%    AS DEC NO-UNDO.
DEF VAR FI-DivKost   AS DEC NO-UNDO.
DEF VAR FI-DivKost%  AS DEC NO-UNDO.
DEF VAR FI-Rab3      AS DEC NO-UNDO.
DEF VAR FI-Rab3%     AS DEC NO-UNDO.
DEF VAR FI-VareKost  AS DEC NO-UNDO.
DEF VAR FI-Mva       AS DEC NO-UNDO.
DEF VAR FI-Mva%      AS DEC NO-UNDO.
DEF VAR FI-DB        AS DEC NO-UNDO.
DEF VAR FI-DB%       AS DEC NO-UNDO.
DEF VAR FI-Pris      AS DEC NO-UNDO.
DEF VAR FI-EUPris    AS DEC NO-UNDO.
DEF VAR FI-EuManuel  AS LOG NO-UNDO.
DEF VAR FI-Valkurs   AS DEC NO-UNDO.
DEF VAR FI-EuroKurs  AS DEC NO-UNDO.
DEF VAR wWork        AS DEC NO-UNDO.
DEF VAR rowPrisKo    AS RECID NO-UNDO.

DEFINE VARIABLE cTekst        AS CHARACTER NO-UNDO.
DEFINE VARIABLE iCL           AS INTEGER   NO-UNDO.
DEFINE VARIABLE iClProfilNr   AS INTEGER   NO-UNDO.
DEFINE VARIABLE iClOPTProfilNr   AS INTEGER   NO-UNDO.
DEFINE VARIABLE bKopierPrisko AS LOG       NO-UNDO.
DEFINE VARIABLE bKopierHKInnPris AS LOG NO-UNDO.
DEFINE VARIABLE bEtiTvang     AS LOG       NO-UNDO.
DEFINE VARIABLE bSettEtikett  AS LOG       NO-UNDO.
DEFINE VARIABLE bIkkeSlett    AS LOG       NO-UNDO.

DEFINE VARIABLE cOptProfilbutik     AS CHARACTER   NO-UNDO.

DEFINE BUFFER clButiker    FOR Butiker.
DEFINE BUFFER clOPTButiker FOR Butiker.

{runlib.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-ByttElement) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ByttElement Procedure 
FUNCTION ByttElement RETURNS CHARACTER
  ( INPUT ipSkjerm AS CHAR,
    INPUT ipElement AS INT,
    INPUT ipNyttElement AS CHAR,
    INPUT ipDelimiter AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-DB) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD DB Procedure 
FUNCTION DB RETURNS DECIMAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-DB%) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD DB% Procedure 
FUNCTION DB% RETURNS DECIMAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-DB1) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD DB1 Procedure 
FUNCTION DB1 RETURNS DECIMAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-DB2) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD DB2 Procedure 
FUNCTION DB2 RETURNS DECIMAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-DivKost) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD DivKost Procedure 
FUNCTION DivKost RETURNS DECIMAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-DivKost%) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD DivKost% Procedure 
FUNCTION DivKost% RETURNS DECIMAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-EuroPris) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD EuroPris Procedure 
FUNCTION EuroPris RETURNS DECIMAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Frakt) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD Frakt Procedure 
FUNCTION Frakt RETURNS DECIMAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Frakt%) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD Frakt% Procedure 
FUNCTION Frakt% RETURNS DECIMAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-InnPris) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD InnPris Procedure 
FUNCTION InnPris RETURNS DECIMAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Mva) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD Mva Procedure 
FUNCTION Mva RETURNS DECIMAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Mva2) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD Mva2 Procedure 
FUNCTION Mva2 RETURNS DECIMAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Pris) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD Pris Procedure 
FUNCTION Pris RETURNS DECIMAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Rab1) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD Rab1 Procedure 
FUNCTION Rab1 RETURNS DECIMAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Rab1%) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD Rab1% Procedure 
FUNCTION Rab1% RETURNS DECIMAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Rab2) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD Rab2 Procedure 
FUNCTION Rab2 RETURNS DECIMAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Rab2%) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD Rab2% Procedure 
FUNCTION Rab2% RETURNS DECIMAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Rab3) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD Rab3 Procedure 
FUNCTION Rab3 RETURNS DECIMAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Rab3%) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD Rab3% Procedure 
FUNCTION Rab3% RETURNS DECIMAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Varekost) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD Varekost Procedure 
FUNCTION Varekost RETURNS DECIMAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 20
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

/* Henter profilnr på sentrallageret. */
{syspara.i 5 1 1 iCL INT}
{syspar2.i 5 1 1 cOptProfilbutik}
cOptProfilbutik = TRIM(cOptProfilbutik).        
FIND clButiker NO-LOCK WHERE
  clButiker.Butik = iCl NO-ERROR.
IF AVAILABLE clButiker THEN 
  iClProfilNr = clButiker.ProfilNr.
ELSE
  iClProfilNr = 1.
  
/* Sjekker om priskøpost skal kopieres til alle andre prisprofiler. */
{syspara.i 2 4 40 cTekst}
IF CAN-DO('1,J,Ja,Y,YES,True',cTekst) THEN 
  bKopierPrisko = TRUE.
ELSE
  bKopierPrisko = FALSE.
  
/* HK's innpris kopieres til alle andre lokale prisprofiler. */
{syspara.i 2 4 47 cTekst}
IF CAN-DO('1,J,Ja,Y,YES,True',cTekst) THEN 
  bKopierHKInnPris = TRUE.
ELSE
  bKopierHKInnPris = FALSE.
  
/* Sjekker om det er tvang på etikett. */
{syspara.i 2 4 41 cTekst}
IF CAN-DO('1,J,Ja,Y,YES,True',cTekst) THEN 
  bEtiTvang = TRUE.
ELSE
  bEtiTvang = FALSE.
  
/* Sjekker om etikettflagg skal settes på hk prisprofil. */
{syspara.i 2 4 42 cTekst}
IF CAN-DO('1,J,Ja,Y,YES,True',cTekst) THEN 
  bSettEtikett = TRUE.
ELSE
  bSettEtikett = FALSE. 
  
/* For IPS filer skal ikke lokal kalkyle slettes selv om denne er lik hk kalkylen. */
{syspara.i 2 4 44 cTekst}
IF CAN-DO('1,J,Ja,Y,YES,True',cTekst) THEN 
  bIkkeSlett = TRUE.
ELSE
  bIkkeSlett = FALSE.   
  
RUN Euro IN THIS-PROCEDURE (OUTPUT FI-EuroKurs).

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-EndreDato) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EndreDato Procedure 
PROCEDURE EndreDato :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER prowidPrisKo AS ROWID NO-UNDO.

  DEF VAR pdDato     AS DATE NO-UNDO.
  DEF VAR pdcTid     AS DEC  NO-UNDO.
  DEF VAR pd2Dato    AS DATE NO-UNDO.
  DEF VAR pdc2Tid    AS DEC  NO-UNDO.
  DEF VAR pdcPris    AS DEC  NO-UNDO.
  DEF VAR pdcPrisOld AS DEC  NO-UNDO.
  DEF VAR pcOld      AS CHAR NO-UNDO.
  DEF VAR pcNy       AS CHAR NO-UNDO.
  DEF VAR pcError    AS CHAR NO-UNDO.
  DEF VAR pcStatus   AS CHAR NO-UNDO.
  DEF VAR plSvar     AS LOG  NO-UNDO.
  
  DEF BUFFER bPrisKo FOR PrisKo.

  FIND PrisKo NO-LOCK WHERE
      ROWID(PrisKo) = prowidPrisKo NO-ERROR.
  IF NOT AVAILABLE PrisKo THEN
      RETURN "AVBRYT".

  ASSIGN
      pcStatus   = "AVBRYT"
      pdDato     = PrisKo.AktiveresDato
      pdcTid     = dec(SUBSTRING(STRING(PrisKo.AktiveresTid,"HH:MM"),1,2) + "," + 
                       SUBstring(STRING(PrisKo.AktiveresTid,"HH:MM"),4,2))
      pd2Dato    = PrisKo.gyldigTilDato
      pdc2Tid    = dec(SUBSTRING(STRING(PrisKo.GyldigTilTid,"HH:MM"),1,2) + "," + 
                       SUBstring(STRING(PrisKo.GyldigTilTid,"HH:MM"),4,2))
      pdcPris    = PrisKo.Pris
      pdcPrisOld = PrisKo.Pris
      .

  CASE PrisKo.TYPE:
      WHEN 1 THEN 
      DO:
        pcOld   = STRING(pdDato) + STRING(pdcTid).
        RUN priskodato.w (INPUT PrisKo.Type, INPUT-OUTPUT pdDato, INPUT-OUTPUT pdcTid, INPUT-OUTPUT pdcPris).
        IF RETURN-VALUE = "AVBRYT" THEN
            RETURN NO-APPLY "AVBRYT".
        pcNy   = STRING(pdDato) + STRING(pdcTid).
      END.
      WHEN 2 THEN 
      DO:
        pcOld   = STRING(pdDato) + STRING(pdcTid) + STRING(pd2Dato) + STRING(pdc2Tid).
        RUN priskodato2.w (INPUT-OUTPUT pdDato,
                           INPUT-OUTPUT pdcTid,
                           INPUT-OUTPUT pd2Dato,
                           INPUT-OUTPUT pdc2Tid,
                           INPUT-OUTPUT pdcPris).
        IF RETURN-VALUE = "AVBRYT" THEN
            RETURN NO-APPLY "AVBRYT".
        pcNy   = STRING(pdDato) + STRING(pdcTid) + STRING(pd2Dato) + STRING(pdc2Tid).
      END.
      WHEN 3 THEN 
      DO:
        pcOld   = STRING(pdDato) + STRING(pdcTid).
        RUN priskodato.w (PrisKo.Type, INPUT-OUTPUT pdDato, INPUT-OUTPUT pdcTid, INPUT-OUTPUT pdcPris).
        IF RETURN-VALUE = "AVBRYT" THEN
            RETURN NO-APPLY "AVBRYT".
        pcNy   = STRING(pdDato) + STRING(pdcTid).
      END.
      WHEN 5 THEN 
      DO:
        pcOld   = STRING(pdDato) + STRING(pdcTid) + STRING(pd2Dato) + STRING(pdc2Tid).
        RUN priskodato2.w (INPUT-OUTPUT pdDato,
                           INPUT-OUTPUT pdcTid,
                           INPUT-OUTPUT pd2Dato,
                           INPUT-OUTPUT pdc2Tid,
                           INPUT-OUTPUT pdcPris).
        IF RETURN-VALUE = "AVBRYT" THEN
            RETURN NO-APPLY "AVBRYT".
        pcNy   = STRING(pdDato) + STRING(pdcTid) + STRING(pd2Dato) + STRING(pdc2Tid).
      END.
      WHEN 6 THEN 
      DO:
        pcOld   = STRING(pdDato) + STRING(pdcTid).
        RUN priskodato.w (PrisKo.Type, INPUT-OUTPUT pdDato, INPUT-OUTPUT pdcTid, INPUT-OUTPUT pdcPris).
        IF RETURN-VALUE = "AVBRYT" THEN
            RETURN NO-APPLY "AVBRYT".
        pcNy   = STRING(pdDato) + STRING(pdcTid).
      END.
      OTHERWISE RETURN "AVBRYT".
  END CASE.

  /* ingen endringer å utføre. */
  IF (pcOld = pcNy) AND (pdcPrisOld = pdcPris) THEN
      RETURN NO-APPLY "AVBRYT".

  /* Oppdaterer PrisKo med nye data. */
  IF CAN-FIND(bPrisKo WHERE
              bPrisKo.ArtikkelNr    = PrisKo.ArtikkelNr AND
              bPrisKo.ProfilNr      = PrisKo.ProfilNr AND
              bPrisKo.AktiveresDato = PrisKo.AktiveresDato AND
              bPrisKo.AktiveresTid  = 60 * 60 * int(SUBSTRING(STRING(pdcTid,"99.99"),1,2)) + 
                                      60 * int(SUBSTRING(STRING(pdcTid,"99.99"),4,2)) AND
              bPrisKo.Tilbud        = PrisKo.Tilbud AND
              ROWID(bPrisKo) <> ROWID(PrisKo)) THEN
  DO:
      MESSAGE "Det finnes allerede en prisendringspost på dette dato og klokkeslett."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN "AVBRYT".
  END.
  
  /* Sjekker om ny post kan opprettes */
  RUN SjekkNyPrisKo (PrisKo.ArtikkelNr,
                     PrisKo.ProfilNr,
                     pdDato,
                     (60 * 60 * int(SUBSTRING(STRING(pdcTid,"99.99"),1,2)) +  
                      60 * int(SUBSTRING(STRING(pdcTid,"99.99"),4,2))),
                     pd2Dato,
                     (60 * 60 * int(SUBSTRING(STRING(pdc2Tid,"99.99"),1,2)) +  
                      60 * int(SUBSTRING(STRING(pdc2Tid,"99.99"),4,2))),
                     ROWID(PrisKo),
                     (IF CAN-DO("2,3,5,6",STRING(PrisKo.Type))
                        THEN TRUE
                        ELSE FALSE) /* Tilbud */,
                     PrisKo.TYPE,
                     OUTPUT pcError).
  /* Henter posten pånytt - Det blir gjort en release i denne subrtuinen */
  IF NOT AVAILABLE PrisKo THEN
      FIND PrisKo NO-LOCK WHERE
        ROWID(PrisKo) = prowidPrisKo.
  /* Feil som krever avbrudd for tilbud og normalpris. */
  IF CAN-DO("1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19," + 
            "50,51,52,53,54,55,56,57",ENTRY(1,pcError,CHR(1))) THEN
  DO:
    MESSAGE ENTRY(2,pcError,CHR(1))
        VIEW-AS ALERT-BOX ERROR BUTTONS OK.
    RETURN NO-APPLY pcStatus.
  END.

  /* Håndtering av feilmeldinger på normalpris. */
  IF (IF CAN-DO("2,3,5,6",STRING(PrisKo.Type))
                        THEN TRUE
                        ELSE FALSE) = FALSE THEN
  DO:
      /* Feil som krever bekreftelse */
      IF CAN-DO("20",ENTRY(1,pcError,CHR(1))) THEN
      DO:
          plSvar = FALSE.
          MESSAGE ENTRY(2,pcError,CHR(1))
              VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Bekreft"
              UPDATE plSvar.
          IF plSvar = FALSE THEN
              RETURN NO-APPLY pcStatus.
      END.
  END.
  /* Håndtering av feilmeldinger på tilbud. */
  ELSE IF (IF CAN-DO("2,3,5,6",STRING(PrisKo.Type))
                        THEN TRUE
                        ELSE FALSE) = TRUE THEN
  DO:
      /* Feil som krever bekreftelse */
      IF CAN-DO("50",ENTRY(1,pcError,CHR(1))) THEN
      DO:
          plSvar = FALSE.
          MESSAGE ENTRY(2,pcError,CHR(1))
              VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Bekreft"
              UPDATE plSvar.
          IF plSvar = FALSE THEN
              RETURN NO-APPLY pcStatus.
      END.
  END.

  /* Sletter priskøposter for alle andre profiler. */
  IF bKopierPrisko AND PrisKo.ProfilNr = iClProfilNr THEN 
    RUN prisko_slett.p (ROWID(PrisKo)).

  /* Regner om kalkylen hvis prisen er endret. */
  IF pdcPrisOld <> pdcPris THEN
  DO:
      RUN NyPrisIPrisKo (ROWID(PrisKo), pdcPris).
      FIND CURRENT PrisKo NO-LOCK.
  END.

  DO TRANSACTION:
      FIND PrisKo EXCLUSIVE-LOCK WHERE
          ROWID(PrisKo) = prowidPrisKo NO-ERROR.
      IF AVAILABLE PrisKo THEN
      DO:
          CASE PrisKo.TYPE:
              WHEN 1 THEN
              DO:
                  ASSIGN
                      PrisKo.AktiveresDato = pdDato
                      PrisKo.AktiveresTid  = 60 * 60 * int(SUBSTRING(STRING(pdcTid,"99.99"),1,2)) + 
                      60 * int(SUBSTRING(STRING(pdcTid,"99.99"),4,2))
                      .
              END.
              WHEN 2 THEN
              DO:
                  ASSIGN
                      PrisKo.AktiveresDato = pdDato
                      PrisKo.AktiveresTid  = 60 * 60 * int(SUBSTRING(STRING(pdcTid,"99.99"),1,2)) + 
                                             60 * int(SUBSTRING(STRING(pdcTid,"99.99"),4,2))
                      PrisKo.GyldigTilDato = pd2Dato
                      PrisKo.GyldigTilTid  = 60 * 60 * int(SUBSTRING(STRING(pdc2Tid,"99.99"),1,2)) + 
                                             60 * int(SUBSTRING(STRING(pdc2Tid,"99.99"),4,2))
                      .
              END.
              WHEN 3 THEN
              DO:
                  ASSIGN
                      PrisKo.AktiveresDato = pdDato
                      PrisKo.AktiveresTid  = 60 * 60 * int(SUBSTRING(STRING(pdcTid,"99.99"),1,2)) +
                      60 * int(SUBSTRING(STRING(pdcTid,"99.99"),4,2))
                      .
              END.
              WHEN 5 THEN
              DO:
                  ASSIGN
                      PrisKo.AktiveresDato = pdDato
                      PrisKo.AktiveresTid  = 60 * 60 * int(SUBSTRING(STRING(pdcTid,"99.99"),1,2)) + 
                                             60 * int(SUBSTRING(STRING(pdcTid,"99.99"),4,2))
                      PrisKo.GyldigTilDato = pd2Dato
                      PrisKo.GyldigTilTid  = 60 * 60 * int(SUBSTRING(STRING(pdc2Tid,"99.99"),1,2)) + 
                                             60 * int(SUBSTRING(STRING(pdc2Tid,"99.99"),4,2))
                      .
              END.
              WHEN 6 THEN
              DO:
                  ASSIGN
                      PrisKo.AktiveresDato = pdDato
                      PrisKo.AktiveresTid  = 60 * 60 * int(SUBSTRING(STRING(pdcTid,"99.99"),1,2)) +
                      60 * int(SUBSTRING(STRING(pdcTid,"99.99"),4,2))
                      .
              END.
          END CASE.
          FIND CURRENT prisko NO-LOCK.

          /* Kopierer PrisKo posten til alle andre profiler som skal ha den. */
          IF bKopierPrisko AND PrisKo.ProfilNr = iClProfilNr THEN 
            RUN prisko_kopier.p (ROWID(PrisKo),?).

          RETURN "OK".
      END.
      ELSE DO:
          FIND CURRENT prisko NO-LOCK.
          RETURN "AVBRYT".
      END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Euro) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Euro Procedure 
PROCEDURE Euro :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF OUTPUT PARAMETER wKurs AS DEC NO-UNDO.

  DEF VAR wTekst AS CHAR NO-UNDO.


  {syspara.i 2 1 1 wtekst}
  ASSIGN wKurs = DEC(wTekst).
  IF wKurs = ? OR wKurs = 0 THEN
    wKurs = 0.5.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetLastPrisKo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetLastPrisKo Procedure 
PROCEDURE GetLastPrisKo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF OUTPUT PARAMETER prowPrisKo AS RECID NO-UNDO.

  ASSIGN
      prowPrisKo = rowPrisKo
      .
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-HentArtPris) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE HentArtPris Procedure 
PROCEDURE HentArtPris :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT  PARAMETER wArtikkelNr  LIKE ArtBas.ArtikkelNr NO-UNDO.
  DEF INPUT  PARAMETER FI-ProfilNr   AS INT   NO-UNDO.
  DEF INPUT  PARAMETER wTilbud       AS LOG   NO-UNDO.
  DEF OUTPUT PARAMETER wArtPrisRecid AS RECID NO-UNDO.

  DEF BUFFER LokArtPris FOR ArtPris.

  /* Strong scoope */
  DO FOR LokArtPris:

  FIND LokArtPris NO-LOCK WHERE
    LokArtPris.ArtikkelNr = wArtikkelNr AND
    LokArtPris.ProfilNr   = FI-ProfilNr NO-ERROR.
  IF NOT AVAILABLE lokArtPris THEN 
    RUN opprettArtPris (wArtikkelNr, FI-ProfilNr).

  FIND LokArtPris NO-LOCK WHERE
    LokArtPris.ArtikkelNr = wArtikkelNr AND
    LokArtPris.ProfilNr   = FI-ProfilNr NO-ERROR.

  IF AVAILABLE LokArtPris THEN
    wArtPrisRecid = RECID(LokArtPris).
  ELSE
    wArtPrisRecid = ?.
  END. /* Strong scoope */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-HentVareKost) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE HentVareKost Procedure 
PROCEDURE HentVareKost :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEF INPUT  PARAMETER wArtikkelNr LIKE ArtBas.ArtikkelNr NO-UNDO.
  DEF INPUT  PARAMETER wButikk     AS INT                 NO-UNDO.
  /* Pris er pr. stk og eksklusive mva */
  DEF INPUT  PARAMETER wPris       AS DEC                 NO-UNDO.
  DEF OUTPUT PARAMETER wVareKost   AS DEC                 NO-UNDO.

  DEF VAR wMva%   AS DEC NO-UNDO.
  DEF VAR wdb%    AS DEC NO-UNDO.
  DEF VAR wTilbud AS INT NO-UNDO.

  FIND Butiker NO-LOCK WHERE
    Butiker.Butik = wButikk NO-ERROR.
  IF NOT AVAILABLE Butiker THEN
    RETURN "AVBRYT".

  FIND ArtBas NO-LOCK WHERE
    ArtBas.ArtikkelNr = wArtikkelNr NO-ERROR.
  IF NOT AVAILABLE ArtBas THEN
    RETURN "AVBRYT".

  FIND ArtPris NO-LOCK WHERE
    ArtPris.ArtikkelNr = wArtikkelNr AND
    ArtPris.ProfilNr   = Butiker.ProfilNr NO-ERROR.
  FIND VarGr OF ArtBas NO-LOCK NO-ERROR.

  ASSIGN
      wVareKost = 0.

  /* Varekost ved hjelp av kalkyle, hvis denne finnes. */
  IF AVAILABLE ArtPris THEN
    DO:
      /* Flagger om artikkelen er på tilbud. */
      IF ArtPris.Tilbud THEN wTilbud = 2.
      ELSE wTilbud = 1.

      /* Henter kalkyleinformasjon og setter varekost fra kalkulert varekost. */
      ASSIGN
        wVareKost = ArtPris.VareKost[wTilbud]  /* Kalkulert varekost. */
        wMva%     = ArtPris.Mva%[wTilbud]      /* Mva prosent         */
        wDb%      = ArtPris.Db%[wTilbud].      /* Db  prosent         */

      /* Har artikkelen åpen pris, skal varekosten beregnes ut fra db% */
      IF ArtBas.OPris THEN
        ASSIGN 
          wVareKost = wPris * (1 - (wDb% / 100))
          wVareKost = (IF wVareKost = ? THEN 0 ELSE wVareKost).
    END.

  /* Henter kostpris ved hjelp av kostpris% på varegruppen. */
  IF NOT AVAILABLE ArtPris THEN
    VAREGRUPPE:
    DO:
      IF NOT AVAILABLE VarGr THEN
        LEAVE VAREGRUPPE.

      ASSIGN
        wDb%      = VarGr.Kost_Proc
        wVareKost = IF wDb% <> 0
                      THEN (wPris * (wDb% / 100))
                      ELSE wVareKost
        wVareKost = (IF wVareKost = ? THEN 0 ELSE wVareKost).
    END. /* VAREGRUPPE */

  /* Henter kostpris ved hjelp av kostnadsprosent på varegruppen. */
  IF wVareKost = 0 OR wVareKost = ? THEN
    DO:
      IF AVAILABLE VarGr THEN
        wVareKost = abs(ROUND((wVareKost * VarGr.Kost_Proc) / 100,2)).
      ELSE wVareKost = 0.
      IF wVareKost = ? THEN wVareKost = 0.
    END.

  /* Henter varekost basert på systemets default db% */
  IF wVareKost = 0 OR wVareKost = ? THEN
    DO:
      FIND Moms OF VarGr NO-LOCK NO-ERROR.
      IF NOT AVAILABLE Moms THEN
        {syspara.i 2 1 4 wMva% dec}
      ELSE wmva% = Moms.MomsProc.

      {syspara.i 2 1 3 wDb% dec}
      IF wDb% <> 0 THEN
        wVareKost = (wPris / (1 + (wmva% / 100))) / (1 + (wDb% / 100)).
      ELSE wVareKost = 0.
    END.

  /* Det er alltid absoluttverdi som skal returneres */
  ASSIGN
      wVareKost = ABS(wVareKost)
      wVareKost = IF wVareKost = ? THEN 0 ELSE wVareKost
      .
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-InitKalkyle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitKalkyle Procedure 
PROCEDURE InitKalkyle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  /* Interface */
  DEF INPUT        PARAMETER wArtBasRecid AS RECID NO-UNDO.
  DEF INPUT        PARAMETER FI-ProfilNr  AS INT   NO-UNDO.
  DEF INPUT-OUTPUT PARAMETER wSkjerm      AS CHAR  NO-UNDO.
  DEF INPUT        PARAMETER wMomsProc    AS DEC   NO-UNDO.
  DEF INPUT        PARAMETER wValKurs     AS DEC   NO-UNDO.
  DEF INPUT        PARAMETER wFeltNr      AS INT   NO-UNDO.
  DEF INPUT        PARAMETER wTilbud      AS LOG   NO-UNDO.

  DEF VAR wPrisIndex    AS INT   NO-UNDO.
  DEF VAR wArtPrisRecid AS RECID NO-UNDO.

  DEF BUFFER LokArtPris FOR ArtPris.
  DEF BUFFER LokArtBas  FOR ArtBas.

  /* Strong scoope */
  DO FOR LokArtPris, LokArtBas:

  ASSIGN
    wPrisIndex = IF wTilbud THEN 2 ELSE 1.

  /* Henter artikkelen. */
  FIND LokArtBas NO-LOCK WHERE
    RECID(LokArtBas) = wArtBasRecid NO-ERROR.

  /* Henter eller oppretter ArtPris for gjeldende prisprofil. */
  FIND LokArtPris OF LokArtBas NO-LOCK WHERE
    LokArtPris.ProfilNr = FI-ProfilNr NO-ERROR.

  IF NOT AVAILABLE LokArtPris THEN
    DO:
      RUN HentArtPris (INPUT LokArtBas.ArtikkelNr, INPUT FI-ProfilNr, INPUT wTilbud, OUTPUT wArtPrisRecid).
      FIND LokArtPris NO-LOCK WHERE
        RECID(LokArtPris) = wArtPrisRecid NO-ERROR.

    END.

  IF AVAILABLE LokArtPris THEN
    DO:
      ASSIGN
        wSkjerm = STRING(LokArtPris.ValPris[wPrisIndex]) + ";" +
                  string(LokArtPris.InnKjopsPris[wPrisIndex]) + ";" +
                  string(LokArtPris.Rab1Kr[wPrisIndex]) + ";" +
                  string(LokArtPris.Rab1%[wPrisIndex]) + ";" +
                  string(LokArtPris.Rab2Kr[wPrisIndex]) + ";" +
                  string(LokArtPris.Rab2%[wPrisIndex]) + ";" +
                  string(LokArtPris.Frakt[wPrisIndex]) + ";" +
                  string(LokArtPris.Frakt%[wPrisIndex]) + ";" +
                  string(LokArtPris.DivKostKr[wPrisIndex]) + ";" +
                  string(LokArtPris.DivKost%[wPrisIndex]) + ";" +
                  string(LokArtPris.Rab3Kr[wPrisIndex]) + ";" +
                  string(LokArtPris.Rab3%[wPrisIndex]) + ";" +
                  string(LokArtPris.VareKost[wPrisIndex]) + ";" +
                  string(LokArtPris.MvaKr[wPrisIndex]) + ";" +
                  string(LokArtPris.Mva%[wPrisIndex]) + ";" +
                  string(LokArtPris.DBKr[wPrisIndex]) + ";" +
                  string(LokArtPris.DB%[wPrisIndex]) + ";" +
                  string(LokArtPris.Pris[wPrisIndex]) + ";" +
                  string(LokArtPris.EuroPris[wPrisIndex]) + ";" +
                  STRING(LokArtPris.EuroManuel) + ";".
      IF LokArtPris.Tilbud = FALSE THEN
        wSkjerm = wSkjerm +
                  (IF LokArtPris.AktivFraDato <> ?
                     THEN STRING(LokArtPris.AktivFraDato)
                     ELSE "") + ";" +
                  "0;;;0;0;".
      ELSE
        wSkjerm = wSkjerm +
                  ";0;" +
                  (IF LokArtPris.TilbudFraDato <> ?
                     THEN STRING(LokArtPris.TilbudFraDato)
                     ELSE "") + ";" +                     /* 23 */
                  STRING(LokArtPris.TilbudFraTid) + ";" +    /* 25 */
                  (IF LokArtPris.TilbudTilDato <> ?
                     THEN STRING(LokArtPris.TilbudTilDato)
                     ELSE "") + ";" +                     /* 24 */
                  STRING(LokArtPris.TilbudTilTid) + ";".    /* 26 */
       ASSIGN
         wSkjerm = wSkjerm +
                  STRING(LokArtPris.TilbudTimeStyrt).        /* 27 */
    END.
    ELSE 
      DO:
        ASSIGN
          wSkjerm = ";" +
                    ";" +
                    ";" +
                    ";" +
                    ";" +
                    ";" +
                    ";" +
                    ";" +
                    ";" +
                    ";" +
                    ";" +
                    ";" +
                    ";" +
                    ";" +
                    ";" +
                    ";" +
                    ";" +
                    ";" +
                    ";" +
                    ";" + 
                    ";" +
                    "0;;;0;0;"
                    .
      END.
    IF (wSkjerm = ? OR wSkjerm = "") THEN
      DO:
        ASSIGN
          wSkjerm = ";" +
                    ";" +
                    ";" +
                    ";" +
                    ";" +
                    ";" +
                    ";" +
                    ";" +
                    ";" +
                    ";" +
                    ";" +
                    ";" +
                    ";" +
                    ";" +
                    ";" +
                    ";" +
                    ";" +
                    ";" +
                    ";" +
                    ";" + 
                    ";" +
                    "0;;;0;0;"
                    .
      END.
    IF AVAILABLE LokArtBas THEN
        RELEASE LokArtBAs.
    IF AVAILABLE LokArtPris THEN
        RELEASE LokArtPris.
  END. /* Strong scoope */


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-KlargjorPrisko) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE KlargjorPrisko Procedure 
PROCEDURE KlargjorPrisko :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
      DEF BUFFER LokPrisKo FOR PrisKo.
      DEF BUFFER LokArtBas FOR ArtBas.

      /* Har priskøposten dagens dato, skal tidspunktt sjekkes. */
      IF Prisko.AktiveresDato = TODAY THEN
        DO:
          IF PrisKo.AktiveresTid > TIME THEN
            RETURN "AVBRYT".    
        END.
      /* Etikettvang */
      IF bEtiTvang AND PrisKo.EtikettStatus = 0 THEN 
        RETURN "AVBRYT".

      /* Henter artikkelinformasjonen */
      FIND FIRST LokArtBas NO-LOCK WHERE
        LokArtBas.ArtikkelNr = PrisKo.ArtikkelNr NO-ERROR.
      IF NOT AVAILABLE LokArtBas THEN
        RETURN "AVBRYT".

      /* Leverandør og leverandørs valuta. */
      FIND LevBas OF LokArtBas NO-LOCK NO-ERROR.
      IF NOT AVAILABLE LevBas THEN
        RETURN "AVBRYT".
      FIND Valuta OF LokArtBas NO-LOCK NO-ERROR.
      IF NOT AVAILABLE Valuta THEN
        RETURN "AVBRYT".
        
      /* Varegruppe og mva */
      FIND VarGr OF LokArtBas NO-LOCK NO-ERROR.
      IF NOT AVAILABLE VarGr THEN
        RETURN "AVBRYT".
      FIND Moms OF VarGr NO-LOCK NO-ERROR.
      IF NOT AVAILABLE Moms THEN
        RETURN "AVBRYT".      
      
      /* Klargjør posten */
      RUN OppdaterPris.

      /* Døden m.m. */
      IF AVAILABLE PrisKo THEN
      DO:
          /* På tilbud, skal endres til AV tilbud.                                           */
          /* Gjelder vanlig tilbud og leverandørtilbud.                                      */
          /* Klargjør status flagget settes for at batch server skal kunne behandle postene. */
          IF PrisKo.TYPE = 2 OR PrisKo.TYPE = 5 THEN /* 2 = PÅ tilbud 3=AV */
          DO:
            ASSIGN
                PrisKo.AktiveresDato  = PrisKo.GyldigTilDato
                PrisKo.AktiveresTid   = PrisKo.GyldigTilTid
                PrisKo.GyldigTilDato  = PrisKo.GyldigTilDato
                PrisKo.GyldigTilTid   = PrisKo.GyldigTilTid
                PrisKo.TYPE           = IF PrisKo.TYPE = 2 
                                          THEN 3
                                          ELSE 6
                PrisKo.EtikettStatus  = 1
                PrisKo.KlargjorStatus = 1
                NO-ERROR.
            /* Det kan skje at bakgrunsserver klargjør pris samtidig med at det gjøres */
            /* i forgrunnen. Da kan situasjonen oppstå hvor posten allerede finnes.   */
            IF ERROR-STATUS:ERROR AND AVAILABLE PrisKo THEN
                DELETE PrisKo.
          END.
          /* Normal og AV poster. */
          ELSE DO: 
              /* Ferdigbehandlet og etikett skrevet. */
              IF PrisKo.Etikettstatus > 0 THEN 
                DELETE PrisKo.
              /* Etikett er ikke skrevet ut. */
              ELSE PrisKo.KlargjorStatus = 1.
          END.
      END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-KlargjorPriskoAlle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE KlargjorPriskoAlle Procedure 
PROCEDURE KlargjorPriskoAlle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR wOppdatertAntall AS INT  NO-UNDO.
DEF VAR wStartTid        AS INT  NO-UNDO.
DEF VAR wFerdigTid       AS INT  NO-UNDO.
DEF VAR wBruktTid        AS INT  NO-UNDO.

DEF BUFFER bPrisKo FOR PrisKo.

DO WITH FRAME DEFAULT-FRAME:
  /* Startet info */
  ASSIGN
    wStartTid = TIME
    .

  /* Priser klargjøres pr. prisprofil.                     */
  /* Det skapes en fil med prisinformasjon pr. prisprofil. */
  BUTIKKER-PROFIL:  
  FOR EACH PrisProfil NO-LOCK 
    BREAK BY PrisProfil.ProfilNr:
    
    /* For hver profil klarjøres PrisKøen. */
    OPPDAT_TRANS:
    FOR EACH bPrisKo NO-LOCK WHERE
      bPrisKo.ProfilNr       = PrisProfil.ProfilNr AND
      bPrisKo.AktiveresDato <= TODAY:
      
      /* Etikett må være skrevet ut. */
      IF bPrisko.Etikettstatus < 1 THEN 
        NEXT OPPDAT_TRANS.
        
      /* Poster som er automatisk klargjort, ligger som ferdig behandlet.                */
      /* Disse postene skal tas hånd om her. De andre skal håndteres av bruker i butikk. */
      IF bPrisko.KlargjorStatus < 1 THEN 
        NEXT OPPDAT_TRANS.

      /* Det kan være at Prisko posten holdes fra artikkelkortet */
      DO TRANSACTION:
          FIND PrisKo EXCLUSIVE-LOCK WHERE
              PrisKo.ArtikkelNr    = bPrisKo.ArtikkelNr    AND
              PrisKo.ProfilNr      = bPrisKo.ProfilNr      AND
              PrisKo.AktiveresDato = bPrisKo.AktiveresDato AND
              PrisKo.AktiveresTid  = bPrisKo.AktiveresTid  AND
              PrisKo.Tilbud        = bPrisKo.Tilbud NO-ERROR NO-WAIT.
          IF AVAILABLE PrisKo THEN
          DO:
              /* Behandler priskøposten. */
              RUN KlargjorPrisKo.
              RELEASE Prisko.
              IF RETURN-VALUE = "AVBRYT" THEN
                  NEXT OPPDAT_TRANS.
          END.
      END. /* TRANSACTION */

      /* info om transaksjon vises i skjermen */
      ASSIGN
        wOppdatertAntall = wOppdatertAntall + 1
        .
    END. /* OPPDAT_TRANS */
  END. /* PROFIL */

  /* Brukt info */
  ASSIGN
    wFerdigTid = TIME
    wBruktTid  = wFerdigTid - wStartTid
    .
END. /* FRAME */  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-KlargjorPriskoEn) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE KlargjorPriskoEn Procedure 
PROCEDURE KlargjorPriskoEn :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER prowidArtBas AS ROWID NO-UNDO.
  
  DEF VAR wOppdatertAntall AS INT  NO-UNDO.
  DEF VAR wStartTid        AS INT  NO-UNDO.
  DEF VAR wFerdigTid       AS INT  NO-UNDO.
  DEF VAR wBruktTid        AS INT  NO-UNDO.

  DEF BUFFER bPrisKo FOR PrisKo. 

  FIND ArtBas NO-LOCK WHERE
      ROWID(ArtBas) = prowidArtBas NO-ERROR.

  IF NOT AVAILABLE ArtBas THEN
      RETURN "AVBRYT".

  /* Startet info */
  ASSIGN
    wStartTid = TIME
    .

  /* Priser klargjøres pr. prisprofil.                     */
  /* Det skapes en fil med prisinformasjon pr. prisprofil. */
  BUTIKKER-PROFIL:  
  FOR EACH PrisProfil NO-LOCK 
    BREAK BY PrisProfil.ProfilNr:
    
    /* For hver profil klarjøres PrisKøen. */
    OPPDAT_TRANS:
    FOR EACH bPrisKo NO-LOCK WHERE
      bPrisKo.ArtikkelNr     = ArtBas.ArtikkelNr AND
      bPrisKo.ProfilNr       = PrisProfil.ProfilNr AND
      bPrisKo.AktiveresDato <= TODAY:

      /* Det kan være at Prisko posten holdes fra prisserver */
      DO TRANSACTION:
          FIND PrisKo EXCLUSIVE-LOCK WHERE
              PrisKo.ArtikkelNr    = bPrisKo.ArtikkelNr    AND
              PrisKo.ProfilNr      = bPrisKo.ProfilNr      AND
              PrisKo.AktiveresDato = bPrisKo.AktiveresDato AND
              PrisKo.AktiveresTid  = bPrisKo.AktiveresTid  AND
              PrisKo.Tilbud        = bPrisKo.Tilbud NO-ERROR NO-WAIT.
          IF AVAILABLE PrisKo THEN
          DO:
              /* Behandler priskøposten. */
              RUN KlargjorPrisKo.
              RELEASE Prisko.
              IF RETURN-VALUE = "AVBRYT" THEN
                  NEXT OPPDAT_TRANS.
          END.
      END. /* TRANSACTION */

      /* info om transaksjon vises i skjermen */
      ASSIGN
        wOppdatertAntall = wOppdatertAntall + 1
        .

    END. /* OPPDAT_TRANS */
  END. /* PROFIL */

  /* Brukt info */
  ASSIGN
    wFerdigTid = TIME
    wBruktTid  = wFerdigTid - wStartTid
    .
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-KlargjorPriskoEnAvbryt) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE KlargjorPriskoEnAvbryt Procedure 
PROCEDURE KlargjorPriskoEnAvbryt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes: Anropas från StopKampanje i avbrt_Kampanje.p
         Här sänds aldrig artbas rowid in. I alla fall inte den rätta
         Det mesta av denna koden är kopierat från KlargjorPriskoEn      
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER prowidPrisko AS ROWID NO-UNDO.
  
  DEF VAR wOppdatertAntall AS INT  NO-UNDO.
  DEF VAR wStartTid        AS INT  NO-UNDO.
  DEF VAR wFerdigTid       AS INT  NO-UNDO.
  DEF VAR wBruktTid        AS INT  NO-UNDO.

  DEF BUFFER bPrisKo FOR PrisKo. 

  /* Priser klargjøres pr. prisprofil.                     */
  /* Det skapes en fil med prisinformasjon pr. prisprofil. */
    FIND bPrisko NO-LOCK WHERE
        ROWID(bPrisko) = prowidprisko NO-ERROR.

    IF NOT AVAILABLE bPrisko THEN
        RETURN "AVBRYT".

      /* Det kan være at Prisko posten holdes fra prisserver */
    DO TRANSACTION:
        FIND PrisKo EXCLUSIVE-LOCK WHERE ROWID(Prisko) = ROWID(bPrisko) NO-ERROR NO-WAIT.
        IF AVAILABLE PrisKo THEN DO:
            /* Behandler priskøposten. */
            RUN KlargjorPrisKo.
            RELEASE Prisko.
        END.
    END. /* TRANSACTION */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-LagreArtPris) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LagreArtPris Procedure 
PROCEDURE LagreArtPris :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT        PARAMETER wArtBasRecid AS RECID NO-UNDO.
  DEF INPUT        PARAMETER FI-ProfilNr  AS INT   NO-UNDO.
  DEF INPUT-OUTPUT PARAMETER wSkjerm      AS CHAR  NO-UNDO.
  DEF INPUT        PARAMETER wTilbud      AS LOG   NO-UNDO.
  DEF INPUT        PARAMETER wDirekte     AS LOG   NO-UNDO.
  DEF INPUT        PARAMETER wType        AS INT   NO-UNDO.
  DEF INPUT        PARAMETER prPrisKo     AS ROWID NO-UNDO.

  DEF VAR wStatus       AS CHAR INITIAL "AVBRYT, ERROR" NO-UNDO.
  DEF VAR wPrisIndex    AS INT                          NO-UNDO.
  DEF VAR wArtPrisRecid AS RECID                        NO-UNDO.
  DEF VAR wEndringsNr   AS INT                          NO-UNDO.

  DEFINE BUFFER LokArtPris FOR ArtPris.
  DEFINE BUFFER LokArtBas  FOR ArtBas.
  DEFINE BUFFER LokPrisKo  FOR PrisKo.
  DEFINE BUFFER LokVarGr FOR VarGr.

  IF wType = 4 THEN
      wPrisIndex = 2.
  ELSE IF wType = 6 THEN
      wPrisIndex = 2.
  ELSE ASSIGN
    wPrisIndex = IF wTilbud THEN 2 ELSE 1.

  LOKALSCOOPE:
  DO FOR LokArtBas, LokArtPris, LokPrisKo WHILE TRUE:
      /* Henter artikkelen. */
      FIND LokArtBas NO-LOCK WHERE
        RECID(LokArtBas) = wArtBasRecid NO-ERROR.
      FIND LokVarGr OF LokArtBas NO-LOCK NO-ERROR.

      /* Henter ArtPris for gjeldende prisprofil. */
      RUN HentArtPris
              (INPUT LokArtBas.ArtikkelNr,
               INPUT FI-ProfilNr,
               INPUT wTilbud,
               OUTPUT wArtPrisRecid).

      FIND LokArtPris EXCLUSIVE-LOCK WHERE
        RECID(LokArtPris) = wArtPrisRecid NO-ERROR NO-WAIT.
      /* Looper til den er ledig */
      IF LOCKED LokArtPris THEN
      DO:
          PAUSE 1 MESSAGE "LokArtPris posten er låst. Prøver igjen om 2 sekunder.".
          NEXT LOKALSCOOPE.
      END.

      IF NOT AVAILABLE LokArtPris THEN
      DO:
          CREATE LokArtPris.
          ASSIGN
              LokArtPris.ArtikkelNr = LokArtBas.ArtikkelNr
              LokArtPris.ProfilNr   = FI-ProfilNr
              .
      END.

      /* Styring av tilbudsflagg i ArtPris. */
      CASE wType:
          /* Normalprisendring rører ikke tilbudsflagget. */
          WHEN 1 THEN
          DO:
              /* Gjør ingenting */
          END.

          /* På tilbud setter ALLTID tilbudsflagget. */
          WHEN 2 THEN
          TILBUD:
          DO WHILE TRUE:
              FIND CURRENT lokArtBas EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
              IF NOT AVAILABLE lokArtBas THEN 
              DO:
                PAUSE 1 NO-MESSAGE.
                NEXT TILBUD.
              END.
              ASSIGN
                LokArtPris.Tilbud        = TRUE
                LokArtBas.SattPaKampanje = ?
                .
              ASSIGN LokArtBas.SattPaKampanje = TODAY.
              FIND CURRENT lokArtBas NO-LOCK NO-ERROR NO-WAIT.
              
              /* Avslutter den evige loopen */
              LEAVE TILBUD.
          END. /* TILBUD */

          /* Den settes av tilbud hvis det ikke er tilbud i tilbud. */
          WHEN 3 THEN
          DO:
              /* Er vi inne i et utenpåliggende tilbud, skal vi fortsatt stå på tilbud */
              FIND LokPrisKo NO-LOCK WHERE
                ROWID(LokPrisKo) = prPrisKo.
              FIND NEXT LokPrisKo NO-LOCK WHERE
                LokPrisKo.ArtikkelNr    = LokArtBas.ArtikkelNr AND
                LokPrisKo.ProfilNr      = FI-ProfilNr AND
                LokPrisKo.TYPE          = 3 NO-ERROR.
              IF AVAILABLE LokPrisKo THEN
              DO:
                  /* gjør ingenting */
              END.
              ELSE DO:
                  ASSIGN LokArtPris.Tilbud = FALSE.
                  IF LokArtBas.WebButikkArtikkel THEN DO:
                      FIND ELogg NO-LOCK WHERE 
                         ELogg.TabellNavn     = "ArtBas" AND
                         ELogg.EksterntSystem = "WEBBUT"    AND
                         ELogg.Verdier        = STRING(LokArtPris.ArtikkelNr) NO-ERROR NO-WAIT.
                      IF NOT AVAIL Elogg THEN DO:
                        CREATE Elogg.
                        ASSIGN ELogg.TabellNavn     = "ArtBas"
                               ELogg.EksterntSystem = "WEBBUT"   
                               ELogg.Verdier        = STRING(LokArtPris.ArtikkelNr) NO-ERROR.
                        IF ERROR-STATUS:ERROR THEN 
                        DO:
                          DELETE ELogg.
                        END.
                      END.
                  END.
              END.
          END.

          /* Endring av tilbudspris rører ikke tilbudsflagget. */
          WHEN 4 THEN
          DO:
              /* Gjør ingenting */
          END.

          /* På tilbud setter ALLTID tilbudsflagget. */
          WHEN 5 THEN
          TILBUD:
          DO WHILE TRUE:
              FIND CURRENT lokArtBas EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
              IF NOT AVAILABLE lokArtBas THEN 
              DO:
                PAUSE 1 NO-MESSAGE.
                NEXT TILBUD.
              END.
              ASSIGN
                LokArtPris.Tilbud        = TRUE
                LokArtBas.SattPaKampanje = TODAY
                .
              FIND CURRENT lokArtBas NO-LOCK NO-ERROR NO-WAIT.
              
              /* Avslutter den evige loopen */
              LEAVE TILBUD.
          END. /* TILBUD */

          /* Den settes av tilbud hvis det ikke er tilbud i tilbud. */
          WHEN 6 THEN
          DO:
              /* Er vi inne i et utenpåliggende tilbud, skal vi fortsatt stå på tilbud */
              FIND LokPrisKo NO-LOCK WHERE
                ROWID(LokPrisKo) = prPrisKo.
              FIND NEXT LokPrisKo NO-LOCK WHERE
                LokPrisKo.ArtikkelNr    = LokArtBas.ArtikkelNr AND
                LokPrisKo.ProfilNr      = FI-ProfilNr AND
                LokPrisKo.TYPE          = 6 NO-ERROR.
              IF AVAILABLE LokPrisKo THEN
              DO:
                  /* gjør ingenting */
              END.
              ELSE
                ASSIGN
                  LokArtPris.Tilbud = FALSE
                  .
          END.
      END CASE.

      /* Er det tilbud i tilbud, skal kalkylen hentes fra AV posten på
         det utenforliggende tilbudet.                                 */
      IF wType = 3 AND AVAILABLE LokPrisKo THEN
      DO:
          ASSIGN
            LokArtPris.ValPris[wPrisIndex]      = LokPrisKo.ValPris              
            LokArtPris.InnKjopsPris[wPrisIndex] = LokPrisKo.InnKjopsPris         
            LokArtPris.Rab1Kr[wPrisIndex]       = LokPrisKo.Rab1Kr               
            LokArtPris.Rab1%[wPrisIndex]        = LokPrisKo.Rab1%                
            LokArtPris.Rab2Kr[wPrisIndex]       = LokPrisKo.Rab2Kr               
            LokArtPris.Rab2%[wPrisIndex]        = LokPrisKo.Rab2%                
            LokArtPris.Frakt[wPrisIndex]        = LokPrisKo.Frakt                
            LokArtPris.Frakt%[wPrisIndex]       = LokPrisKo.Frakt%               
            LokArtPris.DivKostKr[wPrisIndex]    = LokPrisKo.DivKostKr            
            LokArtPris.DivKost%[wPrisIndex]     = LokPrisKo.DivKost%             
            LokArtPris.Rab3Kr[wPrisIndex]       = LokPrisKo.Rab3Kr               
            LokArtPris.Rab3%[wPrisIndex]        = LokPrisKo.Rab3%                
            LokArtPris.VareKost[wPrisIndex]     = LokPrisKo.VareKost             
            LokArtPris.MvaKr[wPrisIndex]        = LokPrisKo.MvaKr                
            LokArtPris.Mva%[wPrisIndex]         = LokPrisKo.Mva% 
            LokArtPris.Momskod                  = LokVarGr.MomsKod                
            LokArtPris.DBKr[wPrisIndex]         = LokPrisKo.DBKr                 
            LokArtPris.DB%[wPrisIndex]          = LokPrisKo.DB%                  
            LokArtPris.Pris[wPrisIndex]         = LokPrisKo.Pris                 
            LokArtPris.EuroPris[wPrisIndex]     = LokPrisKo.EuroPris             
            LokArtPris.EuroManuel               = LokPrisKo.EuroManuel
            LokArtPris.TilbudFraDato            = TODAY 
            LokArtPris.TilbudFraTid             = TIME 
            LokArtPris.TilbudTilDato            = LokPrisKo.AktiveresDato
            LokArtPris.TilbudTilTid             = LokPrisKo.AktiveresTid
            LokArtPris.TilbudTimeStyrt          = TRUE
            .
      END.
      /* Ellers har vi normal håndtering. */
      ELSE DO:
        /* Vanlig prisoppdatering og vanlig kampanje. */
        DO:
          ASSIGN
            LokArtPris.ValPris[wPrisIndex]      = DEC(ENTRY( 1,wSkjerm,";"))
            LokArtPris.InnKjopsPris[wPrisIndex] = DEC(ENTRY( 2,wSkjerm,";"))
            LokArtPris.Rab1Kr[wPrisIndex]       = DEC(ENTRY( 3,wSkjerm,";"))
            LokArtPris.Rab1%[wPrisIndex]        = DEC(ENTRY( 4,wSkjerm,";"))
            LokArtPris.Rab2Kr[wPrisIndex]       = DEC(ENTRY( 5,wSkjerm,";"))
            LokArtPris.Rab2%[wPrisIndex]        = DEC(ENTRY( 6,wSkjerm,";"))
            LokArtPris.Frakt[wPrisIndex]        = DEC(ENTRY( 7,wSkjerm,";"))
            LokArtPris.Frakt%[wPrisIndex]       = DEC(ENTRY( 8,wSkjerm,";"))
            LokArtPris.DivKostKr[wPrisIndex]    = DEC(ENTRY( 9,wSkjerm,";"))
            LokArtPris.DivKost%[wPrisIndex]     = DEC(ENTRY(10,wSkjerm,";"))
            LokArtPris.Rab3Kr[wPrisIndex]       = DEC(ENTRY(11,wSkjerm,";"))
            LokArtPris.Rab3%[wPrisIndex]        = DEC(ENTRY(12,wSkjerm,";"))
            LokArtPris.VareKost[wPrisIndex]     = DEC(ENTRY(13,wSkjerm,";"))
            LokArtPris.MvaKr[wPrisIndex]        = DEC(ENTRY(14,wSkjerm,";"))
            LokArtPris.Mva%[wPrisIndex]         = DEC(ENTRY(15,wSkjerm,";"))
            LokArtPris.Momskod                  = LokVarGr.MomsKod                
            LokArtPris.DBKr[wPrisIndex]         = DEC(ENTRY(16,wSkjerm,";"))
            LokArtPris.DB%[wPrisIndex]          = DEC(ENTRY(17,wSkjerm,";"))
            LokArtPris.Pris[wPrisIndex]         = DEC(ENTRY(18,wSkjerm,";"))
            LokArtPris.EuroPris[wPrisIndex]     = DEC(ENTRY(19,wSkjerm,";")).
            LokArtPris.EuroManuel               = IF CAN-DO("True,Yes",ENTRY(20,wSkjerm,";"))
                                                    THEN TRUE
                                                    ELSE FALSE.
        END.

        /* Dato og tid ordin‘r kalkyle */
        IF wTilbud = FALSE THEN
          ASSIGN
            LokArtPris.AktivFraDato           = DATE(ENTRY(21,wSkjerm,";"))
            LokArtPris.AktivFraTid            = INT(ENTRY(22,wSkjerm,";")).

        ELSE
          ASSIGN
            LokArtPris.TilbudFraDato          = DATE(ENTRY(23,wSkjerm,";"))
            LokArtPris.TilbudFraTid           = INT(ENTRY(24,wSkjerm,";"))
            LokArtPris.TilbudTilDato          = DATE(ENTRY(25,wSkjerm,";"))
            LokArtPris.TilbudTilTid           = INT(ENTRY(26,wSkjerm,";"))
            LokArtPris.TilbudTimeStyrt        = IF CAN-DO("True,Yes",ENTRY(27,wSkjerm,";"))
                                                  THEN TRUE
                                                  ELSE FALSE.
      END.

      /* Etter ønske fra Gøran */
      ELOGG:
      DO:
          FIND ELogg NO-LOCK WHERE 
             ELogg.TabellNavn     = "ArtPris" AND
             ELogg.EksterntSystem = "POS"    AND
             ELogg.Verdier        = STRING(LokArtPris.ArtikkelNr) + CHR(1) + string(LokArtPris.ProfilNr) NO-ERROR NO-WAIT.
          IF NOT AVAIL Elogg THEN DO:
            CREATE Elogg.
            ASSIGN ELogg.TabellNavn     = "ArtPris"
                   ELogg.EksterntSystem = "POS"   
                   ELogg.Verdier        = STRING(LokArtPris.ArtikkelNr) + CHR(1) + string(LokArtPris.ProfilNr) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN 
            DO:
              DELETE ELogg.
            END.
          END.
      END. /* ELOGG */
      
      /* Logger i prishistorikken */
      {prishist.i
       &PrisIndex = "wPrisIndex"
       &wTilbud    = "wType"
      }
      /* Hvis vi går tilbake til normalpris, skal dette vises i historikken */
      IF (wType = 3 AND NOT AVAILABLE LokPrisKo) THEN
          DO:
              {prishist.i
               &PrisIndex = "1"
               &wTilbud    = "1"
              }
          END.

      /* Slipper postene. */
      IF AVAILABLE LokArtPris THEN
      DO:
          RELEASE LokArtPris.
      END.
      IF AVAILABLE LokArtBas THEN
          RELEASE LokArtBas.
      IF AVAILABLE LokPrisKo THEN
          RELEASE LokPrisKo.

      LEAVE LOKALSCOOPE.
  END. /* LOKALSCOOPE */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-MvaKalk) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MvaKalk Procedure 
PROCEDURE MvaKalk :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT  PARAMETER wMva%  AS DEC NO-UNDO.
  DEF INPUT  PARAMETER wBelop AS DEC NO-UNDO.
  DEF OUTPUT PARAMETER wMvaKr AS DEC NO-UNDO.

  ASSIGN
    FI-Mva% = wMva%
    FI-Pris = wBelop
    wMvaKr  = Mva2().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-NyPrisIPrisKo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NyPrisIPrisKo Procedure 
PROCEDURE NyPrisIPrisKo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER prowidPrisKo AS ROWID NO-UNDO.
  DEF INPUT PARAMETER pdcPris      AS DEC   NO-UNDO.

  DEF VAR wOkStatus AS CHAR NO-UNDO.
  DEF VAR wSkjerm   AS CHAR NO-UNDO.
  
  DEF BUFFER lokPrisKo FOR PrisKo.
  DEF BUFFER lokArtBas FOR ArtBas.

  ASSIGN
    wOkStatus = "AVBRYT".

  DO FOR lokPrisKo TRANSACTION:
     /* Henter subjectet */
    FIND lokPrisKo EXCLUSIVE-LOCK WHERE
        ROWID(lokPrisKo) = prowidPrisKo NO-ERROR.
    IF NOT AVAILABLE lokPrisKo THEN
        RETURN wOkStatus.

    FIND lokArtBas NO-LOCK WHERE
        lokArtBas.ArtikkelNr = lokPrisKo.ArtikkelNr NO-ERROR.
    IF NOT AVAILABLE lokArtBas THEN
        RETURN wOkStatus.

    FIND Valuta OF lokArtBas NO-ERROR.
    IF NOT AVAILABLE Valuta THEN
        RETURN wOkStatus.
    /* Henter strengen (wSkjerm) fra kalkylemodulen. */  
    RUN InitKalkyle
        (INPUT RECID(lokArtBas), 
         INPUT lokPrisKo.ProfilNr,
         INPUT-OUTPUT wSkjerm,
         INPUT lokPrisKo.Mva%,
         INPUT Valuta.ValKurs, 
         INPUT 1,
         INPUT FALSE).      
  
    /* Pris */
    wSkjerm = ByttElement(INPUT wSkjerm,
                    INPUT 18,
                    INPUT STRING(pdcPris),
                    INPUT ";").                          
                          
    /* Starter omkalkulering.                              */
    /* Simulerer at cursor forlater prisfeltet i kalkylen. */
    /* NB: Kalkulasjonen skjer i prosedyrebilboteket.      */
    RUN Omregning
        (INPUT RECID(lokArtBas),
         INPUT lokPrisKo.ProfilNr,
         INPUT-OUTPUT wSkjerm,
         INPUT lokPrisKo.Mva%,
         INPUT Valuta.ValKurs,
         INPUT 18,
         INPUT lokPrisKo.Tilbud).
         
    /* Oppdaterer priskøposten med ny kalkyle */
    ASSIGN
        LokPrisKo.ValPris      = DEC(ENTRY( 1,wSkjerm,";"))
        LokPrisKo.InnKjopsPris = DEC(ENTRY( 2,wSkjerm,";"))
        LokPrisKo.Rab1Kr       = DEC(ENTRY( 3,wSkjerm,";"))
        LokPrisKo.Rab1%        = DEC(ENTRY( 4,wSkjerm,";"))
        LokPrisKo.Rab2Kr       = DEC(ENTRY( 5,wSkjerm,";"))
        LokPrisKo.Rab2%        = DEC(ENTRY( 6,wSkjerm,";"))
        LokPrisKo.Frakt        = DEC(ENTRY( 7,wSkjerm,";"))
        LokPrisKo.Frakt%       = DEC(ENTRY( 8,wSkjerm,";"))
        LokPrisKo.DivKostKr    = DEC(ENTRY( 9,wSkjerm,";"))
        LokPrisKo.DivKost%     = DEC(ENTRY(10,wSkjerm,";"))
        LokPrisKo.Rab3Kr       = DEC(ENTRY(11,wSkjerm,";"))
        LokPrisKo.Rab3%        = DEC(ENTRY(12,wSkjerm,";"))
        LokPrisKo.VareKost     = DEC(ENTRY(13,wSkjerm,";"))
        LokPrisKo.MvaKr        = DEC(ENTRY(14,wSkjerm,";"))
        LokPrisKo.Mva%         = DEC(ENTRY(15,wSkjerm,";"))
        LokPrisKo.DBKr         = DEC(ENTRY(16,wSkjerm,";"))
        LokPrisKo.DB%          = DEC(ENTRY(17,wSkjerm,";"))
        LokPrisKo.Pris         = DEC(ENTRY(18,wSkjerm,";"))
        LokPrisKo.EuroPris     = DEC(ENTRY(19,wSkjerm,";"))
        .
    RELEASE lokPrisKo.
  END. /* TRANSACTION */

  /* Oppdatering OK */
  ASSIGN
    wOkStatus = "OK".

  RETURN wOkStatus.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-NyPrisKo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NyPrisKo Procedure 
PROCEDURE NyPrisKo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER        prArtBasRecid AS RECID NO-UNDO.
  DEF INPUT PARAMETER        piProfilNr    AS INT   NO-UNDO.
  DEF INPUT-OUTPUT PARAMETER pcSkjerm      AS CHAR  NO-UNDO.
  DEF INPUT PARAMETER        pd-Tilbud     AS LOG   NO-UNDO.
  DEF INPUT PARAMETER        pitype        AS INT   NO-UNDO.

  DEF VAR pcStatus       AS CHAR INITIAL "AVBRYT, ERROR" NO-UNDO.
  DEF VAR pcPrisIndex     AS INT                          NO-UNDO.
  DEF VAR prArtPrisRecid AS RECID                        NO-UNDO.
  DEF VAR plTilbud       AS LOG                          NO-UNDO.
  DEF VAR pdDato         AS DATE                         NO-UNDO.
  DEF VAR piTid          AS INT                          NO-UNDO.
  DEF VAR pd2Dato        AS DATE                         NO-UNDO.
  DEF VAR pi2Tid         AS INT                          NO-UNDO.

  DEFINE VARIABLE iTmpCLprofilnr AS INTEGER     NO-UNDO.
  DEFINE VARIABLE pRowIdPrisKo AS ROWID NO-UNDO.
  DEFINE VARIABLE fMvaKr AS DECIMAL NO-UNDO.
  DEFINE VARIABLE fDbKr  AS DECIMAL NO-UNDO.         
  DEFINE VARIABLE iCLOpt AS INTEGER     NO-UNDO.
  DEFINE BUFFER LokPrisKo  FOR PrisKo.

  /* Undertrykke feil som kan komme fra kallende program. */
  IF NUM-ENTRIES(pcSkjerm,';') < 26 THEN 
  DO:
      ASSIGN 
        pcStatus = 'FEIL'
        .
      RETURN.
  END.
  
  ASSIGN
    pcPrisIndex = 1
    plTilbud    = pd-Tilbud
    pdDato      = IF plTilbud = FALSE
                    THEN DATE(ENTRY(21,pcSkjerm,";"))
                    ELSE DATE(ENTRY(23,pcSkjerm,";"))
    piTid       = IF plTilbud = FALSE
                    THEN int(ENTRY(22,pcSkjerm,";")) 
                    ELSE int(ENTRY(24,pcSkjerm,";"))
    pd2Dato     = IF plTilbud = FALSE
                   THEN ?
                   ELSE DATE(ENTRY(25,pcSkjerm,";"))
    pi2Tid      = IF plTilbud = FALSE
                   THEN 0 
                   ELSE int(ENTRY(26,pcSkjerm,";"))
    .
   /* har vi OPT centrallager och tillhör profilen en centralbutik */
   iCLOpt = 0.
   IF cOptProfilbutik <> "" THEN DO:
       /* om prisköposten som kommer in är ett centrallager */
       FIND FIRST clOPTButiker WHERE clOPTButiker.profilnr = piProfilNr NO-LOCK NO-ERROR.
       IF AVAIL clOPTButiker AND clOPTButiker.sentrallager = TRUE THEN
           ASSIGN iCLOpt         = clOPTButiker.butik
                  iTmpCLprofilnr = clOPTButiker.profilnr.
       ELSE IF AVAIL clOPTButiker THEN DO:
           iCLOpt = clOPTButiker.clButikkNr.
           FIND clOPTButiker WHERE clOPTButiker.butik = iCLOpt NO-LOCK NO-ERROR.
           IF AVAIL clOPTButiker THEN
               iTmpCLprofilnr = clOPTButiker.profilnr.
           ELSE
               iTmpCLprofilnr = iClProfilNr.
       END.
       ELSE
           iTmpCLprofilnr = iClProfilNr.
   END.
   ELSE
       iTmpCLprofilnr = iClProfilNr.
                   
  /* Henter artikkelen. */
  FIND ArtBas NO-LOCK WHERE
    RECID(ArtBas) = prArtBasRecid NO-ERROR.

  /* En transaksjon rundt all opdatering. */
  TRANS_BLOKK:
  DO TRANSACTION:
    /* Henter ArtPris for gjeldende prisprofil. */
    RUN HentArtPris
            (INPUT ArtBas.ArtikkelNr,
             INPUT piProfilNr,
             INPUT FALSE,
             OUTPUT prArtPrisRecid).
    FIND ArtPris EXCLUSIVE-LOCK WHERE
      RECID(ArtPris) = prArtPrisRecid NO-ERROR.
    /* Er det en normalprisendring, og det ikke er noe endringer i kalkylen, skal det ikke opprettes priskøpost. */
    IF piType = 1 THEN 
    SJEKK:
    DO:
      IF ArtPris.ValPris[1]      <> DEC(ENTRY( 1,pcSkjerm,";")) OR 
         ArtPris.InnKjopsPris[1] <> DEC(ENTRY( 2,pcSkjerm,";")) OR
         ArtPris.Rab1Kr[1]       <> DEC(ENTRY( 3,pcSkjerm,";")) OR
         ArtPris.Rab1%[1]        <> DEC(ENTRY( 4,pcSkjerm,";")) OR
         ArtPris.Rab2Kr[1]       <> DEC(ENTRY( 5,pcSkjerm,";")) OR
         ArtPris.Rab2%[1]        <> DEC(ENTRY( 6,pcSkjerm,";")) OR
         ArtPris.Frakt[1]        <> DEC(ENTRY( 7,pcSkjerm,";")) OR
         ArtPris.Frakt%[1]       <> DEC(ENTRY( 8,pcSkjerm,";")) OR
         ArtPris.DivKostKr[1]    <> DEC(ENTRY( 9,pcSkjerm,";")) OR
         ArtPris.DivKost%[1]     <> DEC(ENTRY(10,pcSkjerm,";")) OR
         ArtPris.Rab3Kr[1]       <> DEC(ENTRY(11,pcSkjerm,";")) OR
         ArtPris.Rab3%[1]        <> DEC(ENTRY(12,pcSkjerm,";")) OR
         ArtPris.VareKost[1]     <> DEC(ENTRY(13,pcSkjerm,";")) OR 
         ArtPris.MvaKr[1]        <> DEC(ENTRY(14,pcSkjerm,";")) OR
         ArtPris.Mva%[1]         <> DEC(ENTRY(15,pcSkjerm,";")) OR
         ArtPris.DBKr[1]         <> DEC(ENTRY(16,pcSkjerm,";")) OR
         ArtPris.DB%[1]          <> DEC(ENTRY(17,pcSkjerm,";")) OR
         ArtPris.Pris[1]         <> DEC(ENTRY(18,pcSkjerm,";")) OR
         ArtPris.EuroPris[1]     <> DEC(ENTRY(19,pcSkjerm,";")) THEN 
           LEAVE SJEKK.
       ELSE 
         LEAVE TRANS_BLOKK.
    END. /* SJEKK */

    /* Hent/opprett aktiveringspost. */
    FIND LokPrisKo EXCLUSIVE-LOCK WHERE
      LokPrisKo.ArtikkelNr    = ArtBas.ArtikkelNr AND
      LokPrisKo.ProfilNr      = piProfilNr AND
      LokPrisKo.AktiveresDato = pdDato AND
      LokPrisKo.AktiveresTid  = piTid AND
      LokPrisKo.Tilbud        = plTilbud NO-ERROR.

    /* Oppretter ny post, hvis det ikke ligger noen i priskøen fra før. */
    /* Ligger det en post der, skrives den over.                        */
    IF NOT AVAILABLE LokPrisKo THEN
      SKAPELSEN:
      DO:
        /* Oppretter på tilbudspost. */
        CREATE LokPrisKo.
        ASSIGN
          LokPrisKo.ArtikkelNr    = ArtBas.ArtikkelNr
          LokPrisKo.ProfilNr      = piProfilNr
          LokPrisKo.AktiveresDato = pdDato
          LokPrisKo.AktiveresTid  = piTid
          LokPrisKo.Tilbud        = plTilbud.
        /* Øvrig info */
        ASSIGN
          LokPrisKo.GyldigTilDato = pd2Dato
          LokPrisKo.GyldigTilTid  = pi2Tid
          rowPrisKo               = RECID(LokPrisKo)
          .
      END. /* SKAPELSEN */

    /* Leverandørtilbud skal ikke røre utpris. */
    IF piType = 5 THEN 
    DO:
      ASSIGN
      /*LokPrisKo.TYPE         = if plTilbud THEN 2 ELSE 1*/
      LokPrisKo.TYPE         = piType
      LokPrisKo.ValPris      = DEC(ENTRY( 1,pcSkjerm,";"))
      LokPrisKo.InnKjopsPris = DEC(ENTRY( 2,pcSkjerm,";"))
      LokPrisKo.Rab1Kr       = DEC(ENTRY( 3,pcSkjerm,";"))
      LokPrisKo.Rab1%        = DEC(ENTRY( 4,pcSkjerm,";"))
      LokPrisKo.Rab2Kr       = DEC(ENTRY( 5,pcSkjerm,";"))
      LokPrisKo.Rab2%        = DEC(ENTRY( 6,pcSkjerm,";"))
      LokPrisKo.Frakt        = DEC(ENTRY( 7,pcSkjerm,";"))
      LokPrisKo.Frakt%       = DEC(ENTRY( 8,pcSkjerm,";"))
      LokPrisKo.DivKostKr    = DEC(ENTRY( 9,pcSkjerm,";"))
      LokPrisKo.DivKost%     = DEC(ENTRY(10,pcSkjerm,";"))
      LokPrisKo.Rab3Kr       = DEC(ENTRY(11,pcSkjerm,";"))
      LokPrisKo.Rab3%        = DEC(ENTRY(12,pcSkjerm,";"))
      LokPrisKo.VareKost     = DEC(ENTRY(13,pcSkjerm,";"))
      LokPrisKo.Mva%         = DEC(ENTRY(15,pcSkjerm,";"))
      LokPrisKo.Pris         = ArtPris.Pris[1]
      LokPrisKo.EuroPris     = ArtPris.EuroPris[1]
      LokPrisKo.MvaKr        = ArtPris.Pris[1] - (ArtPris.Pris[1] / (1 + (ArtPris.Mva%[1] / 100)))      
      LokPrisKo.DBKr         = ArtPris.Pris[1] - LokPrisKo.MvaKr - LokPrisKo.VareKost
      LokPrisKo.DB%          = ROUND((LokPrisKo.DBKr * 100) / (LokPrisKo.VareKost + LokPrisKo.DBKr),2)
      LokPrisKo.Db%          = IF (LokPrisKo.Db% = ? OR LokPrisKo.DB% < 0) 
                                 THEN 0
                                 ELSE LokPrisKo.DB%
      .
      
    END.
    /* Felles for tilbud og ordinær pris. */
    ELSE DO:
      ASSIGN
      /*LokPrisKo.TYPE       = if plTilbud THEN 2 ELSE 1*/
      LokPrisKo.TYPE         = piType
      LokPrisKo.ValPris      = DEC(ENTRY( 1,pcSkjerm,";"))
      LokPrisKo.InnKjopsPris = DEC(ENTRY( 2,pcSkjerm,";"))
      LokPrisKo.Rab1Kr       = DEC(ENTRY( 3,pcSkjerm,";"))
      LokPrisKo.Rab1%        = DEC(ENTRY( 4,pcSkjerm,";"))
      LokPrisKo.Rab2Kr       = DEC(ENTRY( 5,pcSkjerm,";"))
      LokPrisKo.Rab2%        = DEC(ENTRY( 6,pcSkjerm,";"))
      LokPrisKo.Frakt        = DEC(ENTRY( 7,pcSkjerm,";"))
      LokPrisKo.Frakt%       = DEC(ENTRY( 8,pcSkjerm,";"))
      LokPrisKo.DivKostKr    = DEC(ENTRY( 9,pcSkjerm,";"))
      LokPrisKo.DivKost%     = DEC(ENTRY(10,pcSkjerm,";"))
      LokPrisKo.Rab3Kr       = DEC(ENTRY(11,pcSkjerm,";"))
      LokPrisKo.Rab3%        = DEC(ENTRY(12,pcSkjerm,";"))
      LokPrisKo.VareKost     = DEC(ENTRY(13,pcSkjerm,";"))
      LokPrisKo.MvaKr        = DEC(ENTRY(14,pcSkjerm,";"))
      LokPrisKo.Mva%         = DEC(ENTRY(15,pcSkjerm,";"))
      LokPrisKo.DBKr         = DEC(ENTRY(16,pcSkjerm,";"))
      LokPrisKo.DB%          = DEC(ENTRY(17,pcSkjerm,";"))
      LokPrisKo.Pris         = DEC(ENTRY(18,pcSkjerm,";"))
      LokPrisKo.EuroPris     = DEC(ENTRY(19,pcSkjerm,";"))
      .
    END.
    ASSIGN   
      LokPrisKo.EuroManuel   = IF CAN-DO("True,Yes",ENTRY(20,pcSkjerm,";"))
                                          THEN TRUE
                                 ELSE FALSE
      LokPrisKo.TimeStyrt     = TRUE
    LokPrisKo.Opphav        = (IF iTmpCLprofilnr = LokPrisKo.ProfilNr THEN 'HK' ELSE 'LOK')
/* !!      LokPrisKo.Opphav        = (IF iClProfilNr = LokPrisKo.ProfilNr THEN 'HK' ELSE 'LOK') */
      pRowIdPrisKo            = ROWID(LokPrisKo)
      .
    /* Regler for om posten skal bli liggende i priskø eller ikke.            */
    /* Skal den ikke bli liggende i priskø, settes etikettstatus til skrevet. */
    RUN sjekkEtikettstatus.p (LokPrisko.ArtikkelNr, 
                              iTmpCLprofilnr,
/*                               iClProfilNr, */
                              bSettEtikett,
                              bEtiTvang,
                              LokPrisko.ProfilNr, 
                              LokPrisKo.TYPE, 
                              LokPrisKo.Opphav, 
                              LokPrisKo.VareKost, 
                              LokPrisKo.Pris, 
                              INPUT-OUTPUT LokPrisko.EtikettStatus, 
                              INPUT-OUTPUT LokPrisko.KlargjorStatus).  

    /* Slipper recordene. No-lock på de oppdaterte recordene. */
    IF AVAILABLE LokPrisKo THEN
      FIND CURRENT LokPrisKo NO-LOCK.
    IF AVAILABLE ArtPris THEN
        FIND CURRENT ArtPris NO-LOCK.
  END. /* TRANS_BLOKK TRANSACTION */

  /* Kopierer PrisKo posten til alle andre profiler som skal ha den. */
  IF AVAILABLE LokPrisKo THEN 
  DO:
/*     IF bKopierPrisko AND (LokPrisKo.ProfilNr = iClProfilNr OR iCLOpt > 0) THEN */
    IF bKopierPrisko AND (LokPrisKo.ProfilNr = iTmpCLprofilnr) THEN 
      RUN prisko_kopier.p (pRowIdPrisKo,ROWID(ArtPris)).
  END.
  
  /* Slipper ArtPris posten. */
  IF AVAILABLE ArtPris THEN
      FIND CURRENT ArtPris NO-LOCK.
  
  /* Slipper recordene. no-lock på de oppdaterte recordene. */
  IF AVAILABLE LokPrisKo THEN
    RELEASE LokPrisKo.

  /* Allt har gått bra. */
  ASSIGN
    pcStatus = "OK".

  /* Avslutter og kvitterer ut jobben. */
  RETURN pcStatus.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Omregning) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Omregning Procedure 
PROCEDURE Omregning :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  /* Interface */
  DEF INPUT        PARAMETER wArtBasRecid AS RECID NO-UNDO.
  DEF INPUT        PARAMETER FI-ProfilNr  AS INT   NO-UNDO.
  DEF INPUT-OUTPUT PARAMETER wSkjerm      AS CHAR  NO-UNDO.
  DEF INPUT        PARAMETER wMomsProc    AS DEC   NO-UNDO.
  DEF INPUT        PARAMETER wValKurs     AS DEC   NO-UNDO.
  DEF INPUT        PARAMETER wFeltNr      AS INT   NO-UNDO.
  DEF INPUT        PARAMETER wTilbud      AS LOG   NO-UNDO.

  DEF BUFFER bufArtBas FOR ArtBas.

  /* Leser av kalkulasjonsmodusen */
  wKalkModus = 0.
  {syspara.i 2 1 2 wKalkModus INT}
  IF wKalkModus = 0 THEN
    wKalkModus = 1. /* Default er utpris fast. */

  ASSIGN
    FI-ValKurs  = wValKurs
    FI-Mva%     = wMomsProc
    wSkjerm     = wSkjerm + ";;;;;;;;;;".
    

  /* Initierer variablene */
  ASSIGN
    FI-ValPris  = DEC(ENTRY(1,wSkjerm,";"))
    FI-InnPris  = DEC(ENTRY(2,wSkjerm,";"))
    FI-Rab1     = DEC(ENTRY(3,wSkjerm,";"))
    FI-Rab1%    = DEC(ENTRY(4,wSkjerm,";"))
    FI-Rab2     = DEC(ENTRY(5,wSkjerm,";"))
    FI-Rab2%    = DEC(ENTRY(6,wSkjerm,";"))
    FI-Frakt    = DEC(ENTRY(7,wSkjerm,";"))
    FI-Frakt%   = DEC(ENTRY(8,wSkjerm,";"))
    FI-DivKost  = DEC(ENTRY(9,wSkjerm,";"))
    FI-DivKost% = DEC(ENTRY(10,wSkjerm,";"))
    FI-Rab3     = DEC(ENTRY(11,wSkjerm,";"))
    FI-Rab3%    = DEC(ENTRY(12,wSkjerm,";"))
    FI-VareKost = DEC(ENTRY(13,wSkjerm,";"))
    FI-Mva      = DEC(ENTRY(14,wSkjerm,";"))
    /* FI-Mva%     = DEC(entry(15,wSkjerm,";")) */
    FI-DB       = DEC(ENTRY(16,wSkjerm,";"))
    FI-DB%      = DEC(ENTRY(17,wSkjerm,";"))
    FI-Pris     = DEC(ENTRY(18,wSkjerm,";"))
    FI-EUPris   = DEC(ENTRY(19,wSkjerm,";"))
    FI-EuManuel = IF CAN-DO("True,Yes",ENTRY(20,wSkjerm,";"))
                    THEN TRUE
                    ELSE FALSE
  NO-ERROR.
  
  IF ERROR-STATUS:ERROR THEN DO:
      FIND bufArtBas NO-LOCK WHERE
          RECID(bufArtBas) = wArtBasRecid.
      IF AVAILABLE bufArtBas THEN
      MESSAGE 
          "Omregning - Error Artikkel:" bufArtBas.ArtikkelNr bufArtbas.beskr SKIP
          wSkjerm SKIP(1)
          "FeltNr" wFeltNr ENTRY(wFeltNr,wSkjerm,";")
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
  END.
  
  /* Kalkulasjon av feltene. */
  /* KALKULASJON             */
  CASE wFeltNr:
    WHEN 1 THEN /* ValutaPris */
      DO:
        /* Regne ut Innkj›pspris ut fra valutapris. */
        /* Korrigerer deretter påvirkerde verdier.  */
        ASSIGN
          FI-InnPris  = Innpris()
          .
        IF FI-Rab1% <> 0 THEN
            FI-Rab1   = Rab1().
        ELSE
          FI-Rab1%    = Rab1%().

        IF FI-Rab2% <> 0 THEN
            FI-Rab2     = Rab2().
        ELSE
            FI-Rab2%    = Rab2%().
        ASSIGN
          FI-Frakt%   = Frakt%()   /* Regnes om! */
          FI-DivKost% = DivKost%(). /* Regnes om! */          
        IF FI-Rab3% <> 0 THEN
            FI-Rab3     = Rab3().

        ELSE
          FI-Rab3%    = Rab3%().
        ASSIGN
          FI-VareKost = VareKost()
          FI-DB       = DB1()
          FI-DB%      = DB%().
      END.

    WHEN 3 THEN /* Rabatt 1 Kr */
      DO:
        ASSIGN
          FI-Rab1%    = Rab1%()
          FI-Rab2%    = Rab2%()    /* Regnes om! */
          FI-Frakt%   = Frakt%()   /* Regnes om! */
          FI-DivKost% = DivKost%() /* Regnes om! */
          FI-Rab3%    = Rab3%()    /* Regnes om! */
          FI-VareKost = VareKost()
          FI-DB       = DB1()
          FI-DB%      = DB%().
      END.

    WHEN 4 THEN /* Rabatt 1 % */
      DO:
        ASSIGN
          FI-Rab1     = Rab1()
          FI-Rab2%    = Rab2%()    /* Regnes om! */
          FI-Frakt%   = Frakt%()   /* Regnes om! */
          FI-DivKost% = DivKost%() /* Regnes om! */
          FI-Rab3%    = Rab3%()    /* Regnes om! */
          FI-VareKost = VareKost()
          FI-DB       = DB1()
          FI-DB%      = DB%().
      END.

    WHEN 5 THEN /* Rabatt 2 Kr */
      DO:
        ASSIGN
          FI-Rab2%    = Rab2%()
          FI-Frakt%   = Frakt%()   /* Regnes om! */
          FI-DivKost% = DivKost%() /* Regnes om! */
          FI-Rab3%    = Rab3%()    /* Regnes om! */
          FI-VareKost = VareKost()
          FI-DB       = DB1()
          FI-DB%      = DB%().
      END.

    WHEN 6 THEN /* Rabatt 2 % */
      DO:
        ASSIGN
          FI-Rab2     = Rab2()
          FI-Frakt%   = Frakt%()   /* Regnes om! */
          FI-DivKost% = DivKost%() /* Regnes om! */
          FI-Rab3%    = Rab3%()    /* Regnes om! */
          FI-VareKost = VareKost()
          FI-DB       = DB1()
          FI-DB%      = DB%().
      END.

    WHEN 7 THEN /* Frakt Kr */
      DO:
        ASSIGN
          FI-Frakt%   = Frakt%()
          FI-DivKost% = DivKost%() /* Regnes om! */
          FI-Rab3%    = Rab3%()    /* Regnes om! */
          FI-VareKost = VareKost()
          FI-DB       = DB1()
          FI-DB%      = DB%().
      END.

    WHEN 8 THEN /* Frakt % */
      DO:
        ASSIGN
          FI-Frakt    = Frakt()
          FI-DivKost% = DivKost%() /* Regnes om! */
          FI-Rab3%    = Rab3%()    /* Regnes om! */
          FI-VareKost = VareKost()
          FI-DB       = DB1()
          FI-DB%      = DB%().
      END.

    WHEN 9 THEN /* DivKostKr */
      DO:
        ASSIGN
          FI-DivKost% = DivKost%()
          FI-Rab3%    = Rab3%()    /* Regnes om! */
          FI-VareKost = VareKost()
          FI-DB       = DB1()
          FI-DB%      = DB%().
      END.

    WHEN 10 THEN /* DivKost % */
      DO:
        ASSIGN
          FI-DivKost  = DivKost()
          FI-Rab3%    = Rab3%()    /* Regnes om! */
          FI-VareKost = VareKost()
          FI-DB       = DB1()
          FI-DB%      = DB%().
      END.

    WHEN 11 THEN /* Rabatt 3 Kr */
      DO:
        ASSIGN
          FI-Rab3%    = Rab3%()
          FI-VareKost = VareKost()
          FI-DB       = DB1()
          FI-DB%      = DB%().
      END.

    WHEN 12 THEN /* Rabatt 3 % */
      DO:
        ASSIGN
          FI-Rab3     = Rab3()
          FI-VareKost = VareKost()
          FI-DB       = DB1()
          FI-DB%      = DB%().
      END.

    WHEN 14 THEN /* DB Kr */
      DO:
        ASSIGN
          FI-DB%      = DB%()
          FI-Mva      = Mva()
          FI-Pris     = Pris().
        IF FI-EuManuel = FALSE THEN
          ASSIGN
            FI-EUPris = EuroPris().
      END.

    WHEN 15 THEN /* DB % */
      DO:
        ASSIGN
          FI-DB       = DB()
          FI-Mva      = Mva()
          FI-Pris     = Pris().
        IF FI-EuManuel = FALSE THEN
          ASSIGN
            FI-EUPris = EuroPris().
      END.

    WHEN 18 THEN /* Pris */
      DO:
        ASSIGN
          FI-Mva      = Mva2()
          FI-DB       = DB2()
          FI-DB%      = DB%().
        IF FI-EuManuel = FALSE THEN
          ASSIGN
            FI-EUPris = EuroPris().
      END.

  END CASE. /* KALKULASJON */

  /* Pakker returstreng */
  ASSIGN
    wSkjerm = STRING(FI-ValPris) + ";" +
              string(FI-InnPris) + ";" +
              string(FI-Rab1) + ";" +
              string(FI-Rab1%) + ";" +
              string(FI-Rab2) + ";" +
              string(FI-Rab2%) + ";" +
              string(FI-Frakt) + ";" +
              string(FI-Frakt%) + ";" +
              string(FI-DivKost) + ";" +
              string(FI-DivKost%) + ";" +
              string(FI-Rab3) + ";" +
              string(FI-Rab3%) + ";" +
              string(FI-VareKost) + ";" +
              string(FI-Mva) + ";" +
              string(FI-Mva%) + ";" +
              string(FI-DB) + ";" +
              string(FI-DB%) + ";" +
              string(FI-Pris) + ";" +
              string(FI-EUPris).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-OppdaterPris) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OppdaterPris Procedure 
PROCEDURE OppdaterPris :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR wFDato    AS CHAR NO-UNDO.
  DEF VAR wFTid     AS CHAR NO-UNDO.
  DEF VAR wTDato    AS CHAR NO-UNDO.
  DEF VAR wTTid     AS CHAR NO-UNDO.
  DEF VAR wOkStatus AS CHAR NO-UNDO.
  DEF VAR wTilbud   AS LOG  NO-UNDO.
  DEF VAR wSkjerm   AS CHAR NO-UNDO.
DEFINE VARIABLE iTmpCLprofilnr AS INTEGER     NO-UNDO.
DEFINE VARIABLE iCLOpt AS INTEGER     NO-UNDO.
  ASSIGN
    wTilbud   = PrisKo.Tilbud
    wFDato    = ""
    wFTid     = ""
    wTDato    = ""
    wTTid     = ""
    wOkStatus = "AVBRYT".

  FIND ArtBas NO-LOCK WHERE
      ArtBas.ArtikkelNr = PrisKo.ArtikkelNr NO-ERROR.
  IF NOT AVAILABLE ArtBas THEN
      RETURN "AVBRYT".
  FIND Valuta OF ArtBas NO-LOCK NO-ERROR.

  /* Henter strengen (wSkjerm) fra kalkylemodulen. */  
  RUN InitKalkyle
      (INPUT RECID(ArtBas), 
       INPUT PrisProfil.ProfilNr,
       INPUT-OUTPUT wSkjerm,
       INPUT PrisKo.Mva%,
       INPUT Valuta.ValKurs, 
       INPUT 1,
       INPUT FALSE).   

  /* Setter valutapris fra ArtBas. */
  wSkjerm = ByttElement(INPUT wSkjerm,
                  INPUT 1,
                  INPUT STRING(PrisKo.ValPris),
                  INPUT ";").
  /* Innkjøpspris */
  wSkjerm = ByttElement(INPUT wSkjerm,
                  INPUT 2,
                  INPUT STRING(PrisKo.InnkjopsPris),
                  INPUT ";").

  /* Rab1Kr */
  wSkjerm = ByttElement(INPUT wSkjerm,
                  INPUT 3,
                  INPUT STRING(PrisKo.Rab1Kr),
                  INPUT ";").

  /* Rab1% */
  wSkjerm = ByttElement(INPUT wSkjerm,
                  INPUT 4,
                  INPUT STRING(PrisKo.Rab1%),
                  INPUT ";").

  /* Rab2Kr */
  wSkjerm = ByttElement(INPUT wSkjerm,
                  INPUT 5,
                  INPUT STRING(PrisKo.Rab2Kr),
                  INPUT ";").

  /* Rab2% */
  wSkjerm = ByttElement(INPUT wSkjerm,
                  INPUT 6,
                  INPUT STRING(PrisKo.Rab2%),
                  INPUT ";").

  /* FraktKr */
  wSkjerm = ByttElement(INPUT wSkjerm,
                  INPUT 7,
                  INPUT STRING(PrisKo.Frakt),
                  INPUT ";").

  /* Frakt% */
  wSkjerm = ByttElement(INPUT wSkjerm,
                  INPUT 8,
                  INPUT STRING(PrisKo.Frakt%),
                  INPUT ";").

  /* DivkostKr */
  wSkjerm = ByttElement(INPUT wSkjerm,
                  INPUT 9,
                  INPUT STRING(PrisKo.DivKostKr),
                  INPUT ";").

  /* DivKost% */
  wSkjerm = ByttElement(INPUT wSkjerm,
                  INPUT 10,
                  INPUT STRING(PrisKo.DivKost%),
                  INPUT ";").

  /* Rab3Kr */
  wSkjerm = ByttElement(INPUT wSkjerm,
                  INPUT 11,
                  INPUT STRING(PrisKo.Rab3Kr),
                  INPUT ";").

  /* Rab3% */
  wSkjerm = ByttElement(INPUT wSkjerm,
                  INPUT 12,
                  INPUT STRING(PrisKo.Rab3%),
                  INPUT ";").

  /* Varekost (Innkjøpspris korrigert for rabatter) */
  wSkjerm = ByttElement(INPUT wSkjerm,
                  INPUT 13,
                  INPUT STRING(PrisKo.VareKost),
                  INPUT ";").
  /* DbKr */
  wSkjerm = ByttElement(INPUT wSkjerm,
                  INPUT 14,
                  INPUT STRING(PrisKo.DbKr),
                  INPUT ";").

  /* Db% */
  wSkjerm = ByttElement(INPUT wSkjerm,
                  INPUT 15,
                  INPUT STRING(PrisKo.Db%),
                  INPUT ";").
  
  /* MvaKr */
  wSkjerm = ByttElement(INPUT wSkjerm,
                  INPUT 16,
                  INPUT STRING(PrisKo.MvaKr),
                  INPUT ";").

  /* Mva% */
  wSkjerm = ByttElement(INPUT wSkjerm,
                  INPUT 17,
                  INPUT STRING(PrisKo.Mva%),
                  INPUT ";").
  
  /* Pris */
  wSkjerm = ByttElement(INPUT wSkjerm,
                  INPUT 18,
                  INPUT STRING(PrisKo.Pris),
                  INPUT ";").                          
                        
  /* EuroPris */
  wSkjerm = ByttElement(INPUT wSkjerm,
                  INPUT 19,
                  INPUT STRING(PrisKo.EuroPris),
                  INPUT ";").                          

  /* Sjekker om tilbud er aktivt. Hvis Ja, settes tilbudspris o.l. */
  IF wTilbud THEN
    TILBUDSBEHANDLING:
    DO:
      ASSIGN
        wFDato  = IF PrisKo.AktiveresDato = ?
                    THEN ""
                    ELSE STRING(PrisKo.AktiveresDato)
        wFTid   = STRING(PrisKo.AktiveresTid)
        wTDato  = IF PrisKo.GyldigTilDato = ?
                    THEN ""
                    ELSE STRING(PrisKo.GyldigTilDato)
        wTTid   = STRING(PrisKo.GyldigTilTid)
        wTDato  = IF Prisko.TYPE = 3 
                    THEN wFDato
                    ELSE wTDato
        wTTid   = IF PrisKo.TYPE = 3
                    THEN wFTid
                    ELSE wTTid
        .

      /* Tilbudspris */
      wSkjerm = ByttElement(INPUT wSkjerm,
                            INPUT 18,
                            INPUT STRING(PrisKo.Pris),
                            INPUT ";").
      /* Tilbud fra */
      wSkjerm = ByttElement(INPUT wSkjerm,
                            INPUT 23,
                            INPUT STRING(wFDato),
                            INPUT ";").
      /* Tilbud fra tid */
      wSkjerm = ByttElement(INPUT wSkjerm,
                            INPUT 24,
                            INPUT wFTid,
                            INPUT ";").
      /* Tilbud til */
      wSkjerm = ByttElement(INPUT wSkjerm,
                            INPUT 25,
                            INPUT STRING(wTDato),
                            INPUT ";").
      /* Tilbud til tid */
      wSkjerm = ByttElement(INPUT wSkjerm,
                            INPUT 26,
                            INPUT wTTid,
                            INPUT ";").
      /* Tilbud timestyrt */
      wSkjerm = ByttElement(INPUT wSkjerm,
                            INPUT 27,
                            INPUT "true",
                            INPUT ";").
    END. /* TILBUDSBEHANDLING */
  ELSE 
  ORDINAERPRIS:
  DO:
      ASSIGN
        wFDato  = IF PrisKo.AktiveresDato = ?
                    THEN ""
                    ELSE STRING(PrisKo.AktiveresDato)
        wFTid   = STRING(PrisKo.AktiveresTid)
        .

      /* Fra */
      wSkjerm = ByttElement(INPUT wSkjerm,
                            INPUT 23,
                            INPUT STRING(wFDato),
                            INPUT ";").
      /* Til tid */
      wSkjerm = ByttElement(INPUT wSkjerm,
                            INPUT 24,
                            INPUT wFTid,
                            INPUT ";").
      /* Timestyrt */
      wSkjerm = ByttElement(INPUT wSkjerm,
                            INPUT 27,
                            INPUT "true",
                            INPUT ";").
  END. /* ORDINAERPRIS */
    
  /* Starter omkalkulering.                              */
  /* Simulerer at cursor forlater prisfeltet i kalkylen. */
  /* NB: Kalkulasjonen skjer i prosedyrebilboteket.      */
  RUN Omregning
      (INPUT RECID(ArtBas),
       INPUT PrisProfil.ProfilNr,
       INPUT-OUTPUT wSkjerm,
       INPUT Prisko.Mva%,
       INPUT Valuta.ValKurs,
       INPUT 18,
       INPUT wTilbud).
       
  /* Fiffer opp strengen med felt som mangler.*/
  IF wTilbud THEN
    wSkjerm = wSkjerm                          + ";" +  /* Felt 1 --> 19      */
              "False"                          + ";" +  /* 20 EuroManuel      */
              ""                               + ";" +  /* 21 Aktiv Fra       */
              "0"                              + ";" +  /* 22 Aktiv fra tid   */
              string(wFDato)                   + ";" +  /* 23 TilbudFraDato   */
              string(wFTid)                    + ";" +  /* 24 TilbudFraTid    */
              string(wTDato)                   + ";" +  /* 25 TilbudTilDato   */
              string(wTTId)                    + ";" +  /* 26 TilbudTilTid    */
              "true".                                   /* 27 TilbudTimestyrt */
   ELSE
     wSkjerm = wSkjerm         + ";" +  /* Felt 1 --> 19      */
               "False"         + ";" +  /* 20 EuroManuel      */
               string(wFDato)  + ";" +  /* 21 AktivFraDato    */
               string(wFTid)   + ";" +  /* 22 Aktiv fra tid   */
               ""              + ";" +  /* 23 TilbudFraDato   */
               string(0)       + ";" +  /* 24 Tilbud fra tid  */
               ""              + ";" +  /* 25 TilbudTilDato   */
               string(0)       + ";" +  /* 26 TilbudTilTid    */
               "true".                  /* 27 TilbudTimestyrt */

  /* Er det et tilbud, skal tilbudspris oppdateres. */
  IF wTilbud THEN
      RUN LagreArtPris
          (INPUT RECID(ArtBas),
           INPUT PrisProfil.ProfilNr,
           INPUT-OUTPUT wSkjerm,
           INPUT wTilbud,
           INPUT TRUE,  /* Direkte oppdatering av prisene som er kalkulert */
           INPUT PrisKo.TYPE,
           INPUT ROWID(PrisKo)).

  /* Ellers oppdaterer vi ordinærkalkyle */
  ELSE DO:
      RUN LagreArtPris
          (INPUT RECID(ArtBas),
           INPUT PrisProfil.ProfilNr,
           INPUT-OUTPUT wSkjerm,
           INPUT FALSE,  /* wTilbud = false - Dvs ordinær kalkyle.          */
           INPUT TRUE,   /* Direkte oppdatering av prisene som er kalkulert */
           INPUT PrisKo.TYPE,
           INPUT ROWID(PrisKo)).
  END.

/*    iCLOpt = 0. */
   IF cOptProfilbutik <> "" THEN DO:
       /* om prisköposten som kommer in är ett centrallager */
       FIND FIRST clOPTButiker WHERE clOPTButiker.profilnr = PrisProfil.ProfilNr NO-LOCK NO-ERROR.
       IF AVAIL clOPTButiker AND clOPTButiker.sentrallager = TRUE THEN
           ASSIGN iCLOpt         = clOPTButiker.butik
                  iTmpCLprofilnr = clOPTButiker.profilnr.
       ELSE IF AVAIL clOPTButiker THEN DO:
           iCLOpt = clOPTButiker.clButikkNr.
           FIND clOPTButiker WHERE clOPTButiker.butik = iCLOpt NO-LOCK NO-ERROR.
           IF AVAIL clOPTButiker THEN
               iTmpCLprofilnr = clOPTButiker.profilnr.
           ELSE
               iTmpCLprofilnr = iClProfilNr.
       END.
       ELSE
           iTmpCLprofilnr = iClProfilNr. 
   END.
   ELSE
       iTmpCLprofilnr = iClProfilNr. 





  /* Hvis det er en butikkprofil, det ikke er tilbud og kalkylen er lik HK kalkylen, */
  /* skal artpris på butikkprofilen slettes.                                         */
  IF PrisProfil.ProfilNr <> iTmpCLprofilnr THEN DO:
    RUN sjekkArtpris (RECID(ArtBas), PrisProfil.ProfilNr).
  END.

  /* Er det hk kalkylen, skal innprisendring kopieres til alle lokale prifiler. */
  IF bKopierHKInnPris AND PrisProfil.ProfilNr = iTmpCLprofilnr THEN 
            RUN prisko_InnPrisHKkopier.p (ROWID(ArtBas),PrisProfil.ProfilNr).
  
  /* Oppdatering OK */
  ASSIGN
    wOkStatus = "OK".

  RETURN wOkStatus.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-opprettArtPris) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE opprettArtPris Procedure 
PROCEDURE opprettArtPris :
/*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:                                                                                                                                            
        ------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEF INPUT PARAMETER pfArtikkelNr AS DEC NO-UNDO.
  DEF INPUT PARAMETER piProfilNr   AS INT NO-UNDO.

  DEF VAR lInnkjopsPris AS DEC FORMAT "->>>,>>>,>>9.99" NO-UNDO.
  DEF VAR lRabatt       AS DEC FORMAT "->>>,>>>,>>9.99" NO-UNDO.
  DEF VAR lFrakt        AS DEC FORMAT "->>>,>>>,>>9.99" NO-UNDO.
  DEF VAR lDivKost      AS DEC FORMAT "->>>,>>>,>>9.99" NO-UNDO.
  DEF VAR lVarekost     AS DEC FORMAT "->>>,>>>,>>9.99" NO-UNDO.
  DEF VAR lDBKr         AS DEC FORMAT "->>>,>>>,>>9.99" NO-UNDO.
  DEF VAR lMvaKr        AS DEC FORMAT "->>>,>>>,>>9.99" NO-UNDO.
  DEF VAR lPris         AS DEC FORMAT "->>>,>>>,>>9.99" NO-UNDO.
  
  DEFINE BUFFER tmpArtBas  FOR ArtBas.
  DEFINE BUFFER tmpArtPris FOR ArtPris.
  DEFINE BUFFER tmpLevBas  FOR LevBas.
  DEFINE BUFFER tmpVarGr   FOR VarGr.
  DEFINE BUFFER tmpMoms    FOR Moms.
  
  /* Det skal bare opprettes artpris automatisk for artikler på hk's prisprofil. */
  /* REf. Brynjar. Denne kan ikke gjøres. Da får ikke butikkene opprettet priser 
  IF piProfilNr <> iClProfilNr THEN
    RETURN. 
  */  
  
  FIND tmpArtBas NO-LOCK WHERE tmpArtBas.ArtikkelNr = pfArtikkelNr NO-ERROR.
  IF NOT AVAILABLE tmpArtBas THEN RETURN.
  FIND tmpLevBas OF tmpArtBas NO-ERROR.
  IF NOT AVAILABLE tmpLevBas THEN RETURN.
  FIND tmpVarGr OF tmpArtBas NO-ERROR.
  IF NOT AVAILABLE tmpVarGr THEN RETURN.
  FIND tmpMoms OF tmpVarGr NO-ERROR.
  IF NOT AVAILABLE tmpMoms THEN RETURN.

  DO TRANSACTION:
      CREATE tmpArtPris.
      ASSIGN
          tmpArtPris.ArtikkelNr = pfArtikkelNr
          tmpArtPris.profilNr   = piProfilNr
          .
      IF AVAIL tmpLevBas AND AVAIL tmpvargr AND AVAIL tmpMoms THEN
      DO:
          ASSIGN lInnkjopsPris = 1.

          IF tmpLevBas.Rab1% > 0 THEN
              ASSIGN lRabatt  = ROUND((lInnkjopsPris * tmpLevBas.Rab1%) / 100,2)
                     lVareKost = lInnkjopspris - lRabatt.
          IF tmpLevBas.Frakt% > 0 THEN
              ASSIGN lFrakt    = ROUND((lVareKost * tmpLevBas.Frakt%) / 100,2)
                     lVareKost = lVareKost + lFrakt.
          IF tmpLevBas.DivKost% > 0 THEN
              ASSIGN lDivKost  = ROUND((lVareKost * tmpLevBas.DivKost%) / 100,2)
                     lVareKost = lVareKost + lDivKost.
          IF tmpLevBas.Rab2% > 0 THEN
              ASSIGN lDbKr  = ROUND((lVareKost / (1 - (tmpLevBas.Rab2% / 100))),2) - lVareKost.
          IF tmpMoms.MomsProc > 0 THEN
              ASSIGN lMvaKr  = ROUND(((lVareKost + lDbKr) * tmpMoms.MomsProc) / 100,2).
          ASSIGN 
              tmpArtPris.ValPris[1]      = lInnkjopsPris
              tmpArtPris.InnkjopsPris[1] = tmpArtPris.ValPris[1]
              tmpArtPris.Rab1%[1]        = tmpLevBas.Rab1%
              tmpArtPris.Rab1Kr[1]       = lRabatt
              tmpArtPris.Frakt%[1]       = tmpLevBas.Frakt%
              tmpArtPris.Frakt[1]        = lFrakt
              tmpArtPris.DivKost%[1]     = tmpLevBas.DivKost%
              tmpArtPris.DivKostKr[1]    = lDivKost
              
              tmpArtPris.VareKost[1]     = tmpArtPris.InnkjopsPris[1] - lRabatt + lFrakt + lDivKost
                           
              tmpArtPris.Pris[1]         = lVareKost + lDbKr + lMvaKr
              tmpArtPris.MomsKod         = tmpMoms.MomsKod
              tmpArtPris.Mva%[1]         = tmpMoms.MomsProc
              tmpArtPris.MvaKr[1]        = lMvaKr
              tmpArtPris.DbKr[1]         = lDbKr
              tmpArtPris.Db%[1]          = tmpLevBas.Rab2%
              tmpArtPris.AktivFraDato    = TODAY
              tmpArtPris.AktivFraTid     = TIME
              .
              
      END.
      FIND CURRENT tmpArtPris NO-LOCK.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-sjekkArtPris) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sjekkArtPris Procedure 
PROCEDURE sjekkArtPris :
/*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:                                                                                                                                            
        ------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER precidArtBas AS RECID   NO-UNDO.
  DEFINE INPUT PARAMETER piProfilNr   AS INTEGER NO-UNDO.
  
  DEF VAR piAnt AS INT NO-UNDO.

  DEFINE BUFFER diff1ArtPris FOR ArtPris.
  DEFINE BUFFER diff2ArtPris FOR ArtPris.
  DEFINE BUFFER sjekkPrisko FOR Prisko.
DEFINE VARIABLE iTmpCLprofilnr AS INTEGER     NO-UNDO.
DEFINE VARIABLE iCLOpt         AS INTEGER     NO-UNDO.
  FIND ArtBas NO-LOCK WHERE
    RECID(ArtBas) = precidArtBas NO-ERROR.
  IF NOT AVAILABLE ArtBas THEN RETURN.

iCLOpt = iCL.
IF cOptProfilbutik <> "" THEN DO:
  /* om prisköposten som kommer in är ett centrallager */
  FIND FIRST clOPTButiker WHERE clOPTButiker.profilnr = piProfilNr NO-LOCK NO-ERROR.
  IF AVAIL clOPTButiker AND clOPTButiker.sentrallager = TRUE THEN
      ASSIGN iCLOpt         = clOPTButiker.butik
             iTmpCLprofilnr = clOPTButiker.profilnr.
  ELSE IF AVAIL clOPTButiker THEN DO:
      iCLOpt = clOPTButiker.clButikkNr.
      FIND clOPTButiker WHERE clOPTButiker.butik = iCLOpt NO-LOCK NO-ERROR.
      IF AVAIL clOPTButiker THEN
          iTmpCLprofilnr = clOPTButiker.profilnr.
      ELSE
          ASSIGN iCLOpt         = iCL
                 iTmpCLprofilnr = iClProfilNr.
  END.
  ELSE
      iTmpCLprofilnr = iClProfilNr. 
END.
ELSE
  iTmpCLprofilnr = iClProfilNr. 

FIND diff1ArtPris OF ArtBas WHERE
    diff1ArtPris.ProfilNr = piProfilNr NO-ERROR.
  IF NOT AVAILABLE diff1ArtPris THEN RETURN.
  /* Tilbudsposter skal ikke sjekkes her. */
  IF diff1ArtPris.Tilbud THEN RETURN.
  
  FIND diff2ArtPris OF ArtBas WHERE
      diff2ArtPris.ProfilNr = iTmpCLprofilnr NO-ERROR.
/*     diff2ArtPris.ProfilNr = iClProfilNr NO-ERROR. */
  IF NOT AVAILABLE diff2ArtPris THEN RETURN.


  /* Er kalkylen lik HK's kalkyle, skal ArtPris posten slettes. */
  IF diff1ArtPris.ProfilNr <> iTmpCLprofilnr AND
/*       IF diff1ArtPris.ProfilNr <> iClProfilNr AND */
     (diff1ArtPris.InnkjopsPris[1] = diff2ArtPris.InnkjopsPris[1] AND  
      diff1ArtPris.Varekost[1]     = diff2ArtPris.Varekost[1] AND  
      diff1ArtPris.Pris[1]         = diff2ArtPris.Pris[1]) THEN  
  DO:
    /* Lokale priser skal ikke slettes selv om de er lik hk kalkylen.     */
    /* Men de skal alikevel flagges som endret for å legges ut til kasse. */
    IF bIkkeSlett THEN 
    ELOGG_ARTPRIS:
    DO TRANSACTION:
      FIND ELogg EXCLUSIVE-LOCK WHERE 
         ELogg.TabellNavn     = "ArtPris" AND
         ELogg.EksterntSystem = "POS"    AND
         ELogg.Verdier        = STRING(diff1ArtPris.ArtikkelNr) + CHR(1) + string(diff1ArtPris.ProfilNr) NO-ERROR NO-WAIT.
      IF NOT AVAIL Elogg THEN DO:
        CREATE Elogg.
        ASSIGN ELogg.TabellNavn     = "ArtPris"
               ELogg.EksterntSystem = "POS"   
               ELogg.Verdier        = STRING(diff1ArtPris.ArtikkelNr) + CHR(1) + string(diff1ArtPris.ProfilNr)
               ELogg.EndringsType   = 1
               ELogg.Behandlet      = FALSE NO-ERROR.
        IF ERROR-STATUS:ERROR THEN 
          DELETE ELogg.
      END.
    END. /* ELOGG_ARTPRIS TRANSACTION */

    /* Sletter lokal kalkyle som er lik hk kalkyle. */
    ELSE DO TRANSACTION:
      FIND CURRENT diff1ArtPris EXCLUSIVE-LOCK.
      IF AVAILABLE diff1ArtPris THEN 
      DO:
        DELETE diff1ArtPris.
      END.
    END.
  END.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SjekkNyPrisKo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SjekkNyPrisKo Procedure 
PROCEDURE SjekkNyPrisKo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:   
  
  /* Sjekker om ny post kan opprettes */
  RUN SjekkNyPrisKo IN h_PrisKo (pdcArtikkelNr,
                                 piProfilNr,
                                 pdDato,
                                 60 * 60 * int(SUBstring(STRING(piTid,"99.99"),1,2)) +
                                   60 * int(SUBstring(STRING(piTid,"99.99"),4,2)),
                                 plTilbud,
                                 OUTPUT piStatus). /* 1-Ok, 2-Bekreftet Ok, 3-Avvist. */
      
------------------------------------------------------------------------------*/
  DEF INPUT  PARAMETER pdcArtikkelNr AS DEC   NO-UNDO.
  DEF INPUT  PARAMETER piProfilNr    AS INT   NO-UNDO.
  DEF INPUT  PARAMETER pdDato        AS DATE  NO-UNDO.
  DEF INPUT  PARAMETER piTid         AS INT   NO-UNDO.
  DEF INPUT  PARAMETER pdDato2       AS DATE  NO-UNDO.
  DEF INPUT  PARAMETER piTid2        AS INT   NO-UNDO.
  DEF INPUT  PARAMETER prowidPrisKo  AS ROWID NO-UNDO.
  DEF INPUT  PARAMETER plTilbud      AS LOG   NO-UNDO.
  DEF INPUT  PARAMETER piType        AS INT   NO-UNDO.
  DEF OUTPUT PARAMETER pcError       AS CHAR  NO-UNDO.

  DEF BUFFER bufPrisko FOR Prisko.
  DEF BUFFER bufArtBas FOR ArtBas.

  DEF VAR plSvar AS LOG NO-UNDO.
 
  /* Sikrer at record ikke er tilgjengelig */
  IF AVAILABLE PrisKo THEN
    RELEASE PrisKo.

  /* Feilkode 10 - Ukjent artikkennummer.                    */
  /* Feilkode 11 - Ingen artpris tilbjengelig på artikkelen. */

  /* aktiveringsdato må angis. */
  IF pdDato = ? THEN
    DO:
      pcError = "8" + CHR(1) + "(8) Aktiveringsdato må angis!"
                .
      RETURN pcError.
    END.        

  /* Validerer dato på normalprisendring og AV tilbudsposter */
  IF CAN-DO("1,3",STRING(piType)) THEN
    DO:
      IF pdDato < TODAY THEN
        DO:
          pcError = "1" + CHR(1) + "(1) Aktiveringsdato må være større eller lik dagens dato!"
                    .
          RETURN pcError.
        END.        
      IF pdDato = ? THEN
        DO:
          pcError = "2" + CHR(1) + "(2) Aktiveringsdato må angis!"
              .
          RETURN pcError.
        END.
      ELSE IF pdDato < TODAY THEN
        DO:
          pcError = "3" + CHR(1) + "(3) Aktiveringsdato må være større eller lik dagens dato!"
              .
          RETURN pcError.
        END.
    END.
  /* Validering på startdato tilbud */
  ELSE DO:
    IF pdDato2 = ? THEN
      DO:
        pcError = "5" + CHR(1) + "(5) Tilbud avsluttningsdato må angis!"
                  .
        RETURN pcError.
      END.        
    IF pdDato < TODAY THEN
      DO:
        pcError = "4" + CHR(1) + "(4) Aktiveringsdato for tilbud må være større eller lik dagens dato!"
            .
        RETURN pcError.
      END.    
    IF pdDato = ? OR pdDato2 = ? THEN
      DO:
        pcError = "6" + CHR(1) + "(6) Aktiveringsdato og aktiv til dato må angis!"
            .
        RETURN pcError.
      END.
    IF pdDato2 < pdDato THEN
      DO:
        pcError = "7" + CHR(1) + "(7) Aktiv til dato må være lik eller større enn aktiv fra dato!"
            .
        RETURN pcError.
      END.      
  END.

  /* Ved endring av normalprisen, skal bruker bekrefte at posten i priskøen */
  /* skrives over. Det skal gjøres kontroll ned på dato og klokkeslett.     */
  IF plTilbud = FALSE THEN
  NORMALPRIS:
  DO:
    FIND PrisKo NO-LOCK WHERE
      PrisKo.ArtikkelNr    = pdcArtikkelNr AND
      PrisKo.ProfilNr      = piProfilNr AND
      PrisKo.AktiveresDato = pdDato AND
      PrisKo.AktiveresTid  = piTid AND 
      PrisKo.Tilbud        = plTilbud AND
      PrisKo.Type          = 1 AND
      ROWID(PrisKo)       <> prowidPrisKo
      NO-ERROR.
    IF AVAILABLE PrisKo THEN
      DO:
        /*
        pcError = "20" + CHR(1) + 
                  "(20) Det ligger en prisendringspost i priskøen." + chr(10) + 
                  "Prisen du godkjente vil overskrive prisendringen som ligger i køen." + chr(10) + 
                  "Er du sikker på at dette skal gjøres?".
        */
      END.
  END. /* NORMALPRIS */

  ELSE
  TILBUDSKONTROLL:
  DO:
    /* Kontrollerer at det ikke ligger en AV post på dato og tid. */
    FIND FIRST PrisKo NO-LOCK WHERE
      PrisKo.ArtikkelNr    = pdcArtikkelNr AND
      PrisKo.ProfilNr      = piProfilNr AND
      PrisKo.AktiveresDato = pdDato AND
      PrisKo.AktiveresTid  = piTid AND 
      PrisKo.Tilbud        = plTilbud AND
      PrisKo.Type          = 3 AND
      ROWID(PrisKo)       <> prowidPrisKo
      NO-ERROR.
    IF AVAILABLE PrisKo THEN
      DO TRANSACTION:
        FIND bufArtBas NO-LOCK WHERE
            bufArtBas.ArtikkelNr = pdcArtikkelNr.
        /* Artikkelen skal tas av kampanje. */
        FIND bufPrisko EXCLUSIVE-LOCK WHERE RECID(bufPrisKo) = RECID(PrisKo).
        ASSIGN
            bufPrisKo.AktiveresDato = TODAY
            bufPrisKo.AktiveresTid  = TIME - 20.
        RUN KlargjorPrisKoEn (ROWID(bufArtBas)).
        LEAVE TILBUDSKONTROLL.
      END. /* TRANSACTION */

    /* Kontrollerer at det ikke ligger en PÅ post på dato og tid. */
    FIND FIRST PrisKo NO-LOCK WHERE
      PrisKo.ArtikkelNr    = pdcArtikkelNr AND
      PrisKo.ProfilNr      = piProfilNr AND
      PrisKo.AktiveresDato = pdDato AND
      PrisKo.AktiveresTid  = piTid AND 
      PrisKo.Tilbud        = plTilbud AND
      PrisKo.Type          = 2 AND
      ROWID(PrisKo)       <> prowidPrisKo
      NO-ERROR.
    IF AVAILABLE PrisKo THEN
      DO TRANSACTION:
        /* Denne kampanjen skal kanseleres og erstattes med ny */
        FIND bufPrisko EXCLUSIVE-LOCK WHERE RECID(bufPrisKo) = RECID(PrisKo).
        DELETE bufPrisKo.
        LEAVE TILBUDSKONTROLL.
      END. /* TRANSACTION */

    /* Sjekker om tilbud i tilbud er aktivt.                                  */
    /* Ligger det en AV post i priskøen, må det nye tilbudet avsluttes før AV */
    /* posten aktiveres.                                                      */
    /* Eller det nye tilbudet må starte ETTER at av posten aktiveres.         */
    AV-LOOP:
    FOR EACH PrisKo NO-LOCK WHERE
      PrisKo.ArtikkelNr    = pdcArtikkelNr AND
      PrisKo.ProfilNr      = piProfilNr AND
      PrisKo.AktiveresDato >= pdDato AND
      PrisKo.Type          = 3 AND
      ROWID(PrisKo)       <> prowidPrisKo: 

      AV-SJEKK:
      DO:
        /* Ligger postene på samme dag, må tidspunkt sjekkes. */
        IF PrisKo.AktiveresDato = pdDato THEN
        DO:
            /* Hvis AV posten aktiveres før tilbudet starter, er det ok */
            IF PrisKo.AktiveresTid < piTid THEN
                LEAVE AV-SJEKK.
        END.

        /* Aktiveres det nye tilbudet etter AV posten, er det ok */
        IF pdDato > PrisKo.AktiveresDato THEN DO:
            LEAVE AV-SJEKK.
        END.

        /* Aktiveres tilbudet før AV posten, må det avsluttes før AV posten aktiveres. */
        ELSE DO:
            /* Avsluttes tilbudet på samme dag som AV posten aktiveres, må tiden sjekkes */
            IF pdDato2 = PrisKo.AktiveresDato THEN
            DO:
                /* Tilbudsposten må avsluttes før AV posten. */
                IF piTid2 < PrisKo.AktiveresTid THEN DO:
                    LEAVE AV-SJEKK.
                END.
                ELSE DO TRANSACTION:
                    FIND bufArtBas NO-LOCK WHERE
                        bufArtBas.ArtikkelNr = pdcArtikkelNr.
                    /* Artikkelen skal tas av kampanje. */
                    FIND bufPrisko EXCLUSIVE-LOCK WHERE RECID(bufPrisKo) = RECID(PrisKo).
                    ASSIGN
                        bufPrisKo.AktiveresDato = TODAY
                        bufPrisKo.AktiveresTid  = TIME - 20.
                    RUN KlargjorPrisKoEn (ROWID(bufArtBas)).
                    RETURN ''.
                END. /* TRANSACTION */
            END.
            /* Tilbud avsluttes før AV post aktiveres. */
            IF pdDato2 < PrisKo.AktiveresDato THEN DO:
                LEAVE AV-SJEKK.
            END.
            ELSE DO TRANSACTION:
                FIND bufArtBas NO-LOCK WHERE
                    bufArtBas.ArtikkelNr = pdcArtikkelNr.
                /* Artikkelen skal tas av kampanje. */
                FIND bufPrisko EXCLUSIVE-LOCK WHERE RECID(bufPrisKo) = RECID(PrisKo).

                ASSIGN
                    bufPrisKo.AktiveresDato = TODAY
                    bufPrisKo.AktiveresTid  = TIME - 20.

                FIND bufPrisko NO-LOCK WHERE RECID(bufPrisKo) = RECID(PrisKo).
                RUN KlargjorPrisKoEn (ROWID(bufArtBas)).
                RETURN ''.
            END. /* TRANSACTION */
        END.

      END. /* AV-SJEKK */
    END. /* AV-LOOP */

    /* Leser alle PÅ tilbudsposter som ligger i køen og gjennomfører kontroll */
    PAA-SJEKK:
    FOR EACH PrisKo NO-LOCK WHERE
      PrisKo.ArtikkelNr     = pdcArtikkelNr AND
      PrisKo.ProfilNr       = piProfilNr AND
      PrisKo.AktiveresDato >= pdDato AND
      PrisKo.Type           = 2 AND 
      ROWID(PrisKo)        <> prowidPrisKo: 

      /* Aktiveres postene på samme dag, må tidspunkt sjekkes. */
      IF PrisKo.AktiveresDato = pdDato THEN
      DO:
          /* Nytt tilbud ligger utenpå tilbud i køen */
          IF PrisKo.AktiveresTid > piTid THEN
          OMLIGGENDE:
          DO:
            /* Tilbud i køen må avsluttes før nytt tilbud.     */
            /* Avsluttes de på samme dag, må tidspunkt sjekkes */
            IF PrisKo.GyldigTilDato = pdDato2 THEN
            DO:
                /* Tilbud i køen avsluttes før nytt tilbud avsluttes. OK */
                IF PrisKo.GyldigTilTid < piTid2 THEN
                    LEAVE PAA-SJEKK.
                /* Tilbud i køen MÅ avsluttes før nytt tilbud avsluttes. */
                ELSE DO TRANSACTION:
                  /* Denne kampanjen skal kanseleres og erstattes med ny */
                  FIND bufPrisko EXCLUSIVE-LOCK WHERE RECID(bufPrisKo) = RECID(PrisKo).
                  DELETE bufPrisKo.
                  RETURN ''.
                END. /* TRANSACTION */
            END.
            /* Tilbud i køen avsluttes før nytt tilbud avsluttes. OK */
            IF Prisko.gyldigTilDato < pdDato2 THEN
                LEAVE PAA-SJEKK.
            /* Tilbud i køen MÅ avsluttes før nytt tilbud avsluttes. */
            ELSE DO TRANSACTION:
              /* Denne kampanjen skal kanseleres og erstattes med ny */
              FIND bufPrisko EXCLUSIVE-LOCK WHERE RECID(bufPrisKo) = RECID(PrisKo).
              DELETE bufPrisKo.
              RETURN ''.
            END. /* TRANSACTION */
          END. /* OMLIGGENDE */
          ELSE
          /* Det nye tilbudet ligger innenfor tilbudet i køen. */
          INNENFOR:
          DO:
              /* Nytt tilbud må avsluttes før tilbud i køen avsluttes. */
              /* Avsluttes de på samme dag, må tidspunkt sjekkes */
              IF PrisKo.GyldigTilDato = pdDato2 THEN
              DO:
                /* Nytt tilbud avsluttes før tilbud i køen. OK */
                IF piTid2 < Prisko.GyldigTilTid THEN
                    LEAVE PAA-SJEKK.
                /* Tilbud i køen MÅ avsluttes før nytt tilbud avsluttes. */
                ELSE DO TRANSACTION:
                  /* Denne kampanjen skal kanseleres og erstattes med ny */
                  FIND bufPrisko EXCLUSIVE-LOCK WHERE RECID(bufPrisKo) = RECID(PrisKo).
                  DELETE bufPrisKo.
                  RETURN ''.
                END. /* TRANSACTION */
              END.
              /* Nytt tilbud avsluttes før tilbud i køen. OK */
              IF pdDato2 < Prisko.gyldigTilDato THEN
                  LEAVE PAA-SJEKK.
              /* Tilbud i køen MÅ avsluttes før nytt tilbud avsluttes. */
              ELSE DO TRANSACTION:
                /* Denne kampanjen skal kanseleres og erstattes med ny */
                FIND bufPrisko EXCLUSIVE-LOCK WHERE RECID(bufPrisKo) = RECID(PrisKo).
                DELETE bufPrisKo.
                RETURN ''.
              END. /* TRANSACTION */
          END. /* INNENFOR */
      END.

      /* Tilbudet i køen starter før det nye tilbudet aktiveres. */
      /* Da må det nye tilbudet avsluttes før tilbudet i køen.   */
      IF PrisKo.AktiveresDato < pdDato THEN
      INNENFOR:
      DO:
          /* Avsluttes de på samme dato, må tidspunkt sjekkes */
          IF PrisKo.GyldigTilDato = pdDato2 THEN
          DO:
              /* Avsluttes tilbudet i køen etter det nye tilbudet, er det ok */
              IF PrisKo.gyldigTilTid > piTid2 THEN
                  LEAVE PAA-SJEKK.
              ELSE DO TRANSACTION:
                  /* Denne kampanjen skal kanseleres og erstattes med ny */
                  FIND bufPrisko EXCLUSIVE-LOCK WHERE RECID(bufPrisKo) = RECID(PrisKo).
                  DELETE bufPrisKo.
                  RETURN ''.
              END. /* TRANSACTION */
          END.

          /* Tilbudet avsluttes før tilbudet i køen. */
          IF PrisKo.GyldigTilDato > pdDato2 THEN
              LEAVE PAA-SJEKK.
          ELSE DO TRANSACTION:
              /* Denne kampanjen skal kanseleres og erstattes med ny */
              FIND bufPrisko EXCLUSIVE-LOCK WHERE RECID(bufPrisKo) = RECID(PrisKo).
              DELETE bufPrisKo.
              RETURN ''.
          END. /* TRANSACTION */
      END. /* INNENFOR */

      /* Tilbudet ligger lenger frem i tid */
      IF PrisKo.AktiveresDato > pdDato2 THEN
          LEAVE PAA-SJEKK.

      /* Tilbudet i køen aktiveres etter det nye tilbudet. */
      /* Da må avsluttningsdatoene kontrolleres.           */
      IF PrisKo.AktiveresDato > pdDato THEN
      OMLIGGENDE:
      DO:
          /* Avsluttes tilbud i køen på samme dag som nytt tilbud aktiveres, må tidspunkt kontrolleres. */
          IF PrisKo.GyldigTilDato = pdDato THEN
          DO:
              /* Tilbudet avsluttes før det nye begynner. */
              IF PrisKo.GyldigTilTid < piTid THEN
                  LEAVE PAA-SJEKK.
              /* Nytt tilbud avsluttes inne i et tilbud i køen. */
              ELSE DO TRANSACTION:
                  /* Denne kampanjen skal kanseleres og erstattes med ny */
                  FIND bufPrisko EXCLUSIVE-LOCK WHERE RECID(bufPrisKo) = RECID(PrisKo).
                  DELETE bufPrisKo.
                  RETURN ''.
              END. /* TRANSACTION */
          END.
          /* Tilbudet avsluttes før det nye begynner */
          IF PrisKo.gyldigTilDato < pdDato2 THEN
              LEAVE PAA-SJEKK.
          /* Nytt tilbud avsluttes inne i et tilbud i køen. */
          ELSE DO TRANSACTION:
              /* Denne kampanjen skal kanseleres og erstattes med ny */
              FIND bufPrisko EXCLUSIVE-LOCK WHERE RECID(bufPrisKo) = RECID(PrisKo).
              DELETE bufPrisKo.
              RETURN ''.
          END.
      END. /* OMLIGGENDE */

    END. /* PAA-SJEKK */

  END. /* TILBUDSKONTROLL */

  RETURN pcError.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SlettPrisKo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SlettPrisKo Procedure 
PROCEDURE SlettPrisKo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER prowidPrisKo AS ROWID NO-UNDO.

  DO TRANSACTION:
      FIND PrisKo EXCLUSIVE-LOCK WHERE
          ROWID(PrisKo) = prowidPrisKo NO-ERROR.
      IF AVAILABLE PrisKo THEN
      DO:
          IF PrisKo.TYPE = 3 THEN
          DO:
              MESSAGE "AV tilbudspost kan ikke slettes."
                  VIEW-AS ALERT-BOX INFO BUTTONS OK.
              RELEASE PrisKo.
              RETURN "AVBRYT".
          END.

          /* Sletter priskøposter for alle andre profiler også. */
          IF bKopierPrisko AND PrisKo.ProfilNr = iClProfilNr THEN 
            RUN prisko_slett.p (prowidPrisKo).

          DELETE PrisKo.
          RETURN "OK".
      END.
      ELSE
          RETURN "AVBRYT".
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-ByttElement) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ByttElement Procedure 
FUNCTION ByttElement RETURNS CHARACTER
  ( INPUT ipSkjerm AS CHAR,
    INPUT ipElement AS INT,
    INPUT ipNyttElement AS CHAR,
    INPUT ipDelimiter AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR ipLoop  AS INT NO-UNDO.
  DEF VAR ipTekst AS CHAR NO-UNDO.

  IF ipNyttElement = ? THEN
      ipNyttElement = "".
  
  ipTekst = "".
  DO ipLoop = 1 TO NUM-ENTRIES(ipSkjerm,ipDelimiter):
    ASSIGN ipTekst = ipTekst + 
           (IF ipTekst = ""
              THEN ""
              ELSE ipDelimiter) +
           (IF ipLoop = ipElement 
              THEN ipNyttElement
              ELSE ENTRY(ipLoop,ipSkjerm,ipDelimiter)). 
  END.

  RETURN ipTekst.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-DB) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION DB Procedure 
FUNCTION DB RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

    wWork = (FI-VareKost / (1 - (FI-DB% / 100))) - FI-VareKost.
    IF wWork = ? THEN wWork = 0.

    RETURN wWork.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-DB%) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION DB% Procedure 
FUNCTION DB% RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

    wWork = ROUND((FI-Db * 100) / (FI-VareKost + FI-DB),2).
    IF wWork = ? THEN wWork = 0.

    RETURN wWork.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-DB1) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION DB1 Procedure 
FUNCTION DB1 RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

    wWork = (FI-Pris - FI-Mva - FI-VareKost).
    IF wWork = ? THEN wWork = 0.

    RETURN wWork.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-DB2) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION DB2 Procedure 
FUNCTION DB2 RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

    RETURN (
            (FI-Pris - FI-VareKost - FI-Mva)
           ).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-DivKost) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION DivKost Procedure 
FUNCTION DivKost RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

    RETURN (
            ((FI-Innpris - FI-Rab1 - FI-Rab2 + FI-Frakt) * FI-DivKost%) / 100
           ).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-DivKost%) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION DivKost% Procedure 
FUNCTION DivKost% RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

    wWork = (FI-DivKost * 100) / (FI-InnPris - FI-Rab1 - FI-Rab2 + FI-Frakt).
    IF wWork = ? THEN wWork = 0.

    RETURN wWork.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-EuroPris) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION EuroPris Procedure 
FUNCTION EuroPris RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

    RETURN (FI-Pris * FI-EuroKurs).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Frakt) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION Frakt Procedure 
FUNCTION Frakt RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

    RETURN (
            ((FI-Innpris - FI-Rab1 - FI-Rab2) * FI-Frakt%) / 100
           ).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Frakt%) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION Frakt% Procedure 
FUNCTION Frakt% RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

    wWork = (FI-Frakt * 100) / (FI-InnPris - FI-Rab1 - FI-Rab2).
    IF wWork = ? THEN wWork = 0.

    RETURN wWork.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-InnPris) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION InnPris Procedure 
FUNCTION InnPris RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN (FI-ValPris * FI-ValKurs).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Mva) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION Mva Procedure 
FUNCTION Mva RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

    RETURN (
            ((FI-VareKost + FI-DB) * FI-Mva%) / 100
           ).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Mva2) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION Mva2 Procedure 
FUNCTION Mva2 RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

    wWork = FI-Pris - (FI-Pris / (1 + (FI-Mva% / 100))).
    IF wWork = ? THEN wWork = 0.

    RETURN wWork.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Pris) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION Pris Procedure 
FUNCTION Pris RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

    RETURN (
            (FI-VareKost + FI-DB + FI-MVA)
           ).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Rab1) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION Rab1 Procedure 
FUNCTION Rab1 RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

    RETURN (
            (FI-Innpris * FI-Rab1%) / 100
           ).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Rab1%) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION Rab1% Procedure 
FUNCTION Rab1% RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  wWork = (FI-Rab1 * 100) / FI-InnPris.

  IF wWork = ? THEN wWork = 0.
  RETURN wWork.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Rab2) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION Rab2 Procedure 
FUNCTION Rab2 RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

    RETURN (
            ((FI-Innpris - FI-Rab1) * FI-Rab2%) / 100
           ).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Rab2%) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION Rab2% Procedure 
FUNCTION Rab2% RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

    wWork = (FI-Rab2 * 100) / (FI-InnPris - FI-Rab1).
    IF wWork = ? THEN wWork = 0.

    RETURN wWork.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Rab3) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION Rab3 Procedure 
FUNCTION Rab3 RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

    wWork = ((FI-Innpris - FI-Rab1 - FI-Rab2 + FI-Frakt + FI-DivKost) * FI-Rab3%) / 100.
    IF wWork = ? THEN wWork = 0.

    RETURN wWork.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Rab3%) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION Rab3% Procedure 
FUNCTION Rab3% RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

    wWork = (FI-Rab3 * 100) / (FI-InnPris - FI-Rab1 - FI-Rab2 + FI-Frakt + FI-DivKost).
    IF wWork = ? THEN wWork = 0.

    RETURN wWork.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Varekost) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION Varekost Procedure 
FUNCTION Varekost RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

    RETURN (FI-InnPris -
            FI-Rab1    -
            FI-Rab2    +
            FI-Frakt   +
            FI-DivKost -
            FI-Rab3).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

