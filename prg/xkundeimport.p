&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : xvpispinnles.p
    Purpose     :

    Syntax      :

    Description : Dette programmet ble opprinnelig laget for import av kunder
                  hos Hjem & Hobby.
                  Kreditkunder som også skulle tildeles et medlemsnummer.

    Author(s)   :
    Created     :
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEF INPUT  PARAMETER lFilId      AS DEC    NO-UNDO.
DEF INPUT  PARAMETER h_Parent    AS HANDLE NO-UNDO.
DEF OUTPUT PARAMETER iAntLinjer  AS INT    NO-UNDO.

DEF VAR iTotAntLinjer AS INT  NO-UNDO.
DEF VAR cLinje        AS CHAR NO-UNDO.
DEF VAR cFilNavn      AS CHAR NO-UNDO.
DEF VAR cUtFilNavn    AS CHAR NO-UNDO.
DEF VAR cErrFIlNavn   AS CHAR NO-UNDO.
DEF VAR cTekst        AS CHAR NO-UNDO.
DEF VAR bOk           AS LOG  NO-UNDO.
DEF VAR iCl           AS INT  NO-UNDO.
DEF VAR piAntLinjer AS INT  NO-UNDO.
DEF VAR cKortNr AS CHAR NO-UNDO.
DEF VAR dFDato  AS DATE NO-UNDO.
DEFINE VARIABLE piLoop AS INTEGER NO-UNDO.

DEFINE VARIABLE ocReturn        AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEksterntSystem AS CHARACTER NO-UNDO.

DEF STREAM InnFil. /* Nye kunder.    */
DEF STREAM Utfil.  /* For dubletter. */

{windows.i}

DEF TEMP-TABLE tt_medlem
    /* 01 */ FIELD ButikkNr  AS INT
    /* 02 */ FIELD EksterntKundeNr AS CHAR FORMAT "x(20)"
    /* 03 */ FIELD Kundenavn AS CHAR FORMAT "x(30)" 
    /* 04 */ FIELD ForNavn   AS CHAR FORMAT "x(30)"
    /* 05 */ FIELD Etternavn AS CHAR FORMAT "x(30)"
    /* 06 */ FIELD Adresse1 AS CHAR FORMAT "x(30)"
    /* 07 */ FIELD Adresse2 AS CHAR FORMAT "x(30)"
    /* 08 */ FIELD PostNr   AS CHAR FORMAT "x(15)"
    /* 09 */ FIELD Poststed AS CHAR FORMAT "x(30)"
    /* 10 */ FIELD Telefon AS CHAR FORMAT "x(15)"
    /* 11 */ FIELD Telefaks AS CHAR FORMAT "x(15)"
    /* 12 */ FIELD Mobil AS CHAR FORMAT "x(15)"
    /* 13 */ FIELD Kontakt AS CHAR FORMAT "x(30)"
    /* 14 */ FIELD eMail AS CHAR FORMAT "x(30)"
    /* 15 */ FIELD Kredittgrense AS DEC FORMAT ">>,>>>,>>9.99"
    /* 16 */ FIELD OrgNr AS CHAR FORMAT "x(30)"
    /* 17 */ FIELD Kilde AS CHAR FORMAT "x(30)" 
    /* 18 */ FIELD TilgKilde AS CHAR FORMAT "x(30)"
    /* 19 */ FIELD Rabatt% AS DEC FORMAT ">>9.99"
    /* 20 */ FIELD KundeNr AS DEC  FORMAT "zzzzzzz999999" /* Ligger ikke i fil */
    /* 21 */ FIELD EksterntMedlemsNr AS CHAR 
             FIELD Dublett AS LOG 
    .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



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
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

{syspara.i 5 1 1 iCL INT}

FIND VPIFilHode NO-LOCK WHERE
    VPIFilHode.FilId = lFilId NO-ERROR.
IF NOT AVAILABLE VPIFilHode THEN
DO:
    RETURN " ** Ukjent VPIFilHode post (" + STRING(lFilId) + ").".
END.
ASSIGN
    cFilNavn    = VPIFilHode.Katalog + "~\" + VPIFilHode.FilNavn
    cUtfilNavn  = VPIFilHode.Katalog + "~\DUBL_" + VPIFilHode.FilNavn
    cErrfilNavn = VPIFilHode.Katalog + "~\ERR_" + VPIFilHode.FilNavn
    .

/* Sjekker om det skal legges ut data til eksternt medlemssystem. Ref. Cognito. */
FIND FIRST EkstEDBSystem WHERE 
  EkstEDBSystem.DataType = "MEDW" AND 
  EkstEDBSystem.Aktiv = TRUE NO-LOCK NO-ERROR.
IF AVAILABLE EkstEDBSystem THEN 
  cEksterntSystem = 'WEBINIT' + STRING(TIME).
  
/* Hvis ikke er det normal eksport. */
ELSE 
  cEksterntSystem = 'WEBINIT'.

RUN LesInnFil.

IF CAN-FIND(FIRST tt_Medlem WHERE tt_Medlem.Dublett = FALSE) THEN
    RUN OppdaterMedlem.    
IF CAN-FIND(FIRST tt_Medlem WHERE tt_Medlem.Dublett = TRUE) THEN
    RUN OppdMedlemInfo.    
IF CAN-FIND(FIRST ELogg WHERE 
            ELogg.TabellNavn     = "Medlem" AND
            ELogg.EksterntSystem = cEksterntSystem) THEN     
    RUN webWebMaker.w (OUTPUT ocReturn).

RETURN ''.

/* FOR EACH tt_Medlem:                        */
/*     MESSAGE tt_Medlem.MedlemsNr            */
/*             tt_Medlem.ForNavn              */
/*             tt_Medlem.Etternavn            */
/*             tt_Medlem.EAN                  */
/*         VIEW-AS ALERT-BOX INFO BUTTONS OK. */
/* END.                                       */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-getEkstEDBSystem) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getEkstEDBSystem Procedure 
PROCEDURE getEkstEDBSystem :
/*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:                                                                                                                                            
        ------------------------------------------------------------------------------*/
  DEFINE OUTPUT PARAMETER pcEkstEDBSystem AS CHARACTER NO-UNDO.
  
  ASSIGN 
      pcEkstEDBSystem = cEksterntSystem
      .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-LesInnFil) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LesInnFil Procedure 
PROCEDURE LesInnFil :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR piLinjeNr    AS INT  NO-UNDO.
  DEF VAR pcLinje      AS CHAR NO-UNDO.
  DEF VAR piAntFeil    AS INT  NO-UNDO.
  DEF VAR pcSep        AS CHAR NO-UNDO.
  DEF VAR iButikkNr    AS INT  NO-UNDO.
  DEF VAR cKundeKortNr AS CHAR NO-UNDO.
  DEF VAR iAntDublett  AS INT  NO-UNDO.
  DEF VAR iAntError    AS INT  NO-UNDO.
  DEFINE VARIABLE bDublett AS LOG NO-UNDO.
  
  ASSIGN
      pcSep = ";"
      .
  
  RUN TellOppLinjer.

  FIND LAST VPIFilLinje OF VPIFilHode NO-LOCK NO-ERROR.
  IF AVAILABLE VPIFilLinje THEN
      piLinjeNr = VPIFilLinje.LinjeNr + 1.
  ELSE
      piLinjeNr = 1.

  ASSIGN
      iAntLinjer  = 0
      iAntDublett = 0
      .
  INPUT STREAM InnFil FROM VALUE(cFilNavn) NO-ECHO.
  LESERLINJER:
  REPEAT:
    /* Leser linje fra filen */
    IMPORT STREAM InnFil UNFORMATTED pcLinje.

    /* Tar hånd om overskriftsrad */
    ASSIGN
      iAntLinjer = iAntLinjer + 1
      iButikkNr  = int(ENTRY( 1,pcLinje,pcSep))
      NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
        NEXT LESERLINJER.

    /* Omformer gammelt format fra 18 til 19 entries. */
    cTekst = ';;;;;;;;;;;;;;;;;;'. /* 19 Entries */
    IF NUM-ENTRIES(pcLinje,';') = 18 THEN 
      DO:
        DO piLoop = 1 TO 18:
          ENTRY(piLoop + (IF piLoop >= 13 THEN 1 ELSE 0),cTekst,';') = ENTRY(piLoop,pcLinje,';').
        END.
        pcLinje = cTekst.
      END. 

/*     /* Håndterer kontrollrecord */                      */
/*     IF int(ENTRY( 1,pcLinje,pcSep)) = 9 THEN            */
/*     DO:                                                 */
/*         IF iTotAntLinjer = int(ENTRY( 2,pcLinje,pcSep)) */
/*             THEN bOk = TRUE.                            */
/*         LEAVE LESERLINJER.                              */
/*     END.                                                */
    bOk = TRUE.

    IF NUM-ENTRIES(pcLinje,pcSep) <> 19 THEN
    DO:
      ASSIGN
        piAntFeil = piAntFeil + 1
        cTekst = "Feil antall elementer på linje " + 
                 string(iAntLinjer) + "." +
                 " (Skal være 19, det er " + string(NUM-ENTRIES(pcLinje,pcSep)) + ").".
      PUBLISH "VPIFilLogg" (cTekst + chr(1) + "3").
    END.

    /* Sikrer at det ikke kommer feil. */
    ASSIGN
        pcLinje      = pcLinje + FILL(pcSep,19)
        cKundeKortNr = TRIM(ENTRY( 2,pcLinje,pcSep),'"')
        bDublett     = FALSE.

    /* Dublettkontroll */
    IF CAN-FIND(FIRST KundeKort WHERE 
                KundeKort.KortNr = cKundeKortNr) THEN
    DUBLETT:
    DO:
        ASSIGN 
        bDublett    = TRUE 
        iAntDublett = iAntDublett + 1.
        OUTPUT STREAM UtFil TO VALUE(cUtFilNavn) APPEND.
        PUT STREAM UtFil UNFORMATTED pcLinje SKIP.
        OUTPUT STREAM UtFil CLOSE.
        /* Trigger nytt utlegg av medlemsinfo til Web. */ 
        RUN medlemTilWeb (cKundeKortNr).
        /*NEXT LESERLINJER.*/
    END. /* DUBLETT */

    /* Posterer linjen */
    CREATE tt_Medlem.
    ASSIGN
        tt_Medlem.ButikkNr        = INT(TRIM(ENTRY( 1,pcLinje,pcSep),'"'))
        tt_Medlem.EksterntKundeNr = TRIM(ENTRY( 2,pcLinje,pcSep),'"')
        tt_Medlem.Kundenavn       = TRIM(TRIM(ENTRY( 3,pcLinje,pcSep),'"'))
        tt_Medlem.Fornavn         = TRIM(TRIM(ENTRY( 4,pcLinje,pcSep),'"'))
        tt_Medlem.Etternavn       = TRIM(TRIM(ENTRY( 5,pcLinje,pcSep),'"'))
        tt_Medlem.Adresse1        = TRIM(TRIM(ENTRY( 6,pcLinje,pcSep),'"'))
        tt_Medlem.Adresse2        = TRIM(TRIM(ENTRY( 7,pcLinje,pcSep),'"'))
        tt_Medlem.PostNr          = TRIM(TRIM(ENTRY( 8,pcLinje,pcSep),'"'))
        tt_Medlem.Poststed        = TRIM(TRIM(ENTRY( 9,pcLinje,pcSep),'"'))
        tt_Medlem.Telefon         = TRIM(TRIM(ENTRY(10,pcLinje,pcSep),'"'))
        tt_Medlem.Telefaks        = TRIM(TRIM(ENTRY(11,pcLinje,pcSep),'"'))
        tt_Medlem.Mobil           = TRIM(TRIM(ENTRY(12,pcLinje,pcSep),'"'))
        tt_Medlem.Kontakt         = TRIM(TRIM(ENTRY(13,pcLinje,pcSep),'"'))
        tt_Medlem.eMail           = TRIM(TRIM(ENTRY(14,pcLinje,pcSep),'"'))
        tt_Medlem.Kredittgrense   = dec(TRIM(ENTRY(15,pcLinje,pcSep),'"'))
        tt_Medlem.OrgNr           = TRIM(TRIM(ENTRY(16,pcLinje,pcSep),'"'))
        tt_Medlem.Kilde           = TRIM(TRIM(ENTRY(17,pcLinje,pcSep),'"'))
        tt_Medlem.TilgKilde       = TRIM(TRIM(ENTRY(18,pcLinje,pcSep),'"'))
        tt_Medlem.Rabatt          = dec(TRIM(ENTRY(19,pcLinje,pcSep),'"'))
        tt_Medlem.Dublett         = bDublett
        piLinjeNr                 = piLinjeNr  + 1
        NO-ERROR.

    /* Feil dukket opp */
    IF ERROR-STATUS:ERROR THEN
    ERRORFIL:
    DO:
        iAntError = iAntError + 1.
        OUTPUT STREAM UtFil TO VALUE(cErrFilNavn) APPEND.
        PUT STREAM UtFil UNFORMATTED pcLinje SKIP.
        OUTPUT STREAM UtFil CLOSE.
        NEXT LESERLINJER.
    END. /* ERRORFIL */

    STATUS DEFAULT "Leser linje " + 
                   STRING(iAntLinjer) + 
                   " av " + 
                   STRING(iTotAntLinjer) + 
                   ".".

  END. /* LESERLINJER */
  INPUT STREAM InnFil CLOSE.

  /* Stempler posten som oppdatert. */
  DO TRANSACTION:
      FIND CURRENT VPIFilHode EXCLUSIVE-LOCK.
      ASSIGN
          VPIFilHode.VPIFilStatus = (IF bOk = TRUE 
                                       THEN 3
                                       ELSE 2)
          .
  END.
  IF AVAILABLE VPIFilHode THEN
      FIND CURRENT VPIFilHode    NO-LOCK.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-medlemTilWeb) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE medlemTilWeb Procedure 
PROCEDURE medlemTilWeb :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER cKortNr AS CHAR NO-UNDO.

    FIND FIRST KundeKort WHERE 
      KundeKort.KortNr = cKortNr NO-ERROR.
    IF NOT AVAILABLE KundeKort THEN
        RETURN.
    FIND FIRST MedlemsKort WHERE
        MedlemsKort.InterntKKortId = KundeKort.InterntKKortId NO-ERROR.
    IF AVAILABLE MedlemsKort THEN
    DO:
        /* Logger for sending av fil til Webside for initiering */
        MEDLEM_TIL_WEB:
        DO:
            FIND ELogg WHERE 
                 ELogg.TabellNavn     = "Medlem" AND
                 ELogg.EksterntSystem = cEksterntSystem    AND
                 ELogg.Verdier        = STRING(MedlemsKort.MedlemsNr) NO-ERROR.
            IF NOT AVAIL Elogg THEN DO:
                CREATE Elogg.
                ASSIGN ELogg.TabellNavn     = "Medlem"
                       ELogg.EksterntSystem = cEksterntSystem   
                       ELogg.Verdier        = STRING(MedlemsKort.MedlemsNr).
            END.
            ASSIGN ELogg.EndringsType = 1
                   ELogg.Behandlet    = FALSE.
        END. /* MEDLEM_TIL_WEB */
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-OppdaterMedlem) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OppdaterMedlem Procedure 
PROCEDURE OppdaterMedlem :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
  
        tt_Medlem.ButikkNr      = INT(ENTRY( 1,pcLinje,'"'))
        tt_Medlem.Kundenr       = dec(ENTRY( 2,pcLinje,'"'))
        tt_Medlem.Kundenavn     = trim(ENTRY( 3,pcLinje,'"'))
        tt_Medlem.Adresse1      = trim(ENTRY( 4,pcLinje,'"'))
        tt_Medlem.Adresse2      = trim(ENTRY( 5,pcLinje,'"'))
        tt_Medlem.PostNr        = trim(ENTRY( 6,pcLinje,'"'))
        tt_Medlem.Poststed      = trim(ENTRY( 7,pcLinje,'"'))
        tt_Medlem.Telefon       = trim(ENTRY( 8,pcLinje,'"'))
        tt_Medlem.Telefaks      = trim(ENTRY( 9,pcLinje,'"'))
        tt_Medlem.Mobil         = trim(ENTRY(10,pcLinje,'"'))
        tt_Medlem.Kontakt       = trim(ENTRY(11,pcLinje,'"'))
        tt_Medlem.eMail         = trim(ENTRY(12,pcLinje,'"'))
        tt_Medlem.Kredittgrense = dec(ENTRY(13,pcLinje,'"'))
        tt_Medlem.OrgNr         = trim(ENTRY(14,pcLinje,'"'))
  
------------------------------------------------------------------------------*/
DEF VAR dFDato      AS DATE NO-UNDO.
DEF VAR cKortNr     AS CHAR NO-UNDO.
DEF VAR plKundeNr   AS DEC  NO-UNDO.
DEF VAR plMedlemsNr AS DEC  NO-UNDO.
DEF VAR bOk         AS LOG  NO-UNDO.
DEF VAR piGruppeId  AS INT  NO-UNDO.
DEF VAR cMsgs       AS CHAR NO-UNDO.
DEF VAR cMedlemsNr  AS CHAR NO-UNDO.
DEF VAR piKortNr    AS INT  NO-UNDO.

/* Startpunkt for tildeling av kundekort.*/
{syspara.i 14 2 5 piKortNr INT}
IF piKortNr = 0 THEN piKortNr = 1.

FIND FIRST Kundegruppe NO-LOCK WHERE
    KundeGruppe.GruppeId > 0 NO-ERROR.
IF NOT AVAILABLE KundeGruppe 
    THEN piGruppeId = 1.
    ELSE piGruppeId = KundeGruppe.GruppeId.

FOR EACH tt_Medlem WHERE
    tt_Medlem.Dublett = FALSE:

    /* Spesial for Hjem & Hobby 
    ASSIGN
        cKortNr = string(tt_Medlem.ButikkNr) + string(tt_Medlem.Kundenr)
        .
    */

    piAntLinjer = piAntLinjer + 1.
    STATUS DEFAULT "Oppdaterer linje " + 
                   STRING(piAntLinjer) + 
                   " av " + 
                   STRING(iTotAntLinjer) + 
                   ".".


    /* Når det eksterne id skal være likt det interne kortnr */
    ASSIGN
        cKortNr = STRING(tt_Medlem.EksterntKundeNr)
        cKortNr = IF LENGTH(cKortNr) > 6 THEN "" ELSE cKortNr
        .
    IF AVAILABLE KundeKort THEN
        RELEASE KundeKort.
    IF cKortNr <> "" THEN
        FIND KundeKort NO-LOCK WHERE
        KundeKort.KortNr = cKortNr NO-ERROR.

    /* Er ikke kortnummer angitt, skal neste ledige kundekortnr skal benyttes. */ 
    IF cKortNr = "" THEN
    LOOPEN:
    DO WHILE TRUE:

        FIND KundeKort NO-LOCK WHERE
            KundeKort.KortNr = STRING(piKortNr) NO-ERROR.
        IF AVAILABLE KundeKort THEN 
        DO:
            STATUS DEFAULT "BLANK - Leter etter kortnr " + 
                           STRING(piKortNr) +
                           ".".
            
            piKortNr = piKortNr + 1.
            /* Nummerserie er brukt opp */
            IF piKortNr > 999999 THEN
            DO:
                MESSAGE "Kundekortnummerserien er brukt opp - 999999."
                    VIEW-AS ALERT-BOX INFO BUTTONS OK.
                QUIT.
            END.
            NEXT LOOPEN.
        END.
        ELSE DO TRANSACTION:
            FIND Syspara EXCLUSIVE-LOCK WHERE
               syspara.syshid = 14 AND
               syspara.sysgr  = 2  AND
               syspara.paranr = 5 NO-ERROR.
            IF AVAILABLE SysPara THEN DO:
                ASSIGN  
                    SysPara.Parameter1 = STRING(piKortNr)
                    .
                RELEASE SysPara.
            END.
            cKortNr = STRING(piKortNr).
            LEAVE LOOPEN.
        END. /* TRANSACTION */

        /* Nummerserie er brukt opp */
        IF piKortNr > 999999 THEN
        DO:
            MESSAGE "Kundekortnummerserien er brukt opp - 999999."
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            QUIT.
        END.
    END. /* LOOPEN */

    DO TRANSACTION:
        IF AVAILABLE KundeKort THEN
            RELEASE KundeKort.

        FIND FIRST KundeKort NO-LOCK WHERE
            KundeKort.KortNr = cKortNr NO-ERROR.

        /* GENERER KUNDE og MEDLEM */
        IF (NOT AVAILABLE KundeKort) THEN
        DO:
            ASSIGN /* Kunde skal opprettes */
                plKundeNr   = 0
                plMedlemsNr = 0
                .
            /* Kunde og medlem skal opprettes. */
            RUN genkundeMedlem.p (tt_Medlem.ButikkNr,
                                  piGruppeId,
                                  INPUT-OUTPUT plKundeNr,
                                  OUTPUT plMedlemsNr,
                                  OUTPUT bOk,
                                  OUTPUT cMsgs).

            /* Tar bort automatisk genererte kundekort som er lagt opp fra trigger. */
            /* De kortene skal ikke legges opp på kunder som genereres.             */
            SLETTKORT:
            FOR EACH KundeKort EXCLUSIVE-LOCK WHERE
                KundeKort.KundeNr = plKundeNr:
                DELETE KundeKort.
            END. /* SLETTKORT */

            RUN genkundekort_og_medlem.p (tt_Medlem.ButikkNr,
                                          plKundeNr,
                                          plMedlemsNr,
                                          int(cKortNr),
                                          int(cKortNr),
                                          999,
                                          OUTPUT bOk,
                                          OUTPUT cMsgs).
        END. /* GENERER KUNDE og MEDLEM */
        /* Henter eksisterende kunde. */
        ELSE DO:
            ASSIGN
                plKundeNr   = KundeKort.KundeNr
                plMedlemsNr = 0.

            IF KundeKort.InterntKKortId > 0 THEN
                FIND FIRST MedlemsKort NO-LOCK WHERE
                MedlemsKort.InterntKKortId = KundeKort.InterntKKortId NO-ERROR.
            IF AVAILABLE MedlemsKort THEN
                plMedlemsNr = MedlemsKort.MedlemsNr.
        END.

        FIND Kunde EXCLUSIVE-LOCK WHERE
            Kunde.KundeNr = plKundeNr NO-ERROR.
        FIND FIRST KundeKort OF Kunde EXCLUSIVE-LOCK.
        FIND Medlem EXCLUSIVE-LOCK WHERE
            Medlem.MedlemsNr = plMedlemsNr NO-ERROR.
        IF AVAILABLE Medlem THEN
            FIND FIRST Medlemskort OF Medlem EXCLUSIVE-LOCK NO-ERROR.

        /* Oppdaterer kortstatus */
        IF AVAILABLE MedlemsKort THEN
        DO:
            FIND CURRENT MedlemsKort.
            ASSIGN
                MedlemsKort.Sperret   = FALSE
                Medlem.Aktiv          = TRUE
                MedlemsKort.Innehaver = Medlem.ForNavn + " " + Medlem.EtterNavn
                MedlemsKort.Merknad   = "Kundeimport " + STRING(TODAY)
                .
        END.

        /* Oppdaterer medlemsinformasjonen */
        IF AVAILABLE Medlem THEN
        DO:
/*             ASSIGN                                                */
/*                 dFDato = DATE(INT(ENTRY(2,tt_Medlem.cFDato,'/')), */
/*                               INT(ENTRY(1,tt_Medlem.cFDato,'/')), */
/*                               INT(ENTRY(3,tt_Medlem.cFDato,'/'))  */
/*                               )                                   */
/*                 NO-ERROR.                                         */

            ASSIGN
                Medlem.Fornavn        = IF (tt_Medlem.fornavn + tt_Medlem.Etternavn) = ""
                                          THEN tt_Medlem.Kundenavn
                                          ELSE tt_Medlem.Fornavn
                Medlem.EtterNavn      = tt_Medlem.EtterNavn
                Medlem.FodtAr         = YEAR(dFDato)
                Medlem.ePostAdresse   = tt_Medlem.eMail
                Medlem.MobilTlf       = tt_Medlem.Mobil
                Medlem.Adresse1       = tt_Medlem.Adresse1
                Medlem.Adresse2       = tt_Medlem.Adresse2
                Medlem.PostNr         = tt_Medlem.PostNr
                /*Medlem.PostAdrese   = tt_Medlem.PostSted */
                Medlem.Kjonn          = FALSE
                Medlem.aktivertFraWeb = TODAY
                Medlem.ButikkNr       = IF Medlem.ButikkNr = 0
                                         THEN iCl 
                                         ELSE Medlem.ButikkNr
                Medlem.Kilde          = tt_Medlem.Kilde
                Medlem.TilgKilde      = tt_Medlem.TilgKilde
                Medlem.Rabatt         = tt_Medlem.Rabatt
                Medlem.EksterntMedlemsNr = tt_Medlem.EksterntKundeNr
                .
          FIND FIRST MedlemsGruppe NO-LOCK WHERE
              MedlemsGruppe.Beskrivelse MATCHES "* " + STRING(tt_Medlem.Rabatt) + "%*" NO-ERROR.
          IF AVAILABLE MedlemsGruppe THEN
              Medlem.MedGruppe = MedlemsGruppe.MedGruppe.
             
        END. /* Oppdatering av medlem. */

        /* Oppdaterer kundedata */
        IF AVAILABLE Kunde THEN
        OPPDATERKUNDE:
        DO:
            FIND Kunde EXCLUSIVE-LOCK WHERE
                Kunde.KundeNr = KundeKort.KundeNr.
            ASSIGN
                Kunde.Navn            = tt_Medlem.Kundenavn
                Kunde.ePostAdresse    = tt_Medlem.eMail
                Kunde.MobilTlf        = tt_Medlem.Mobil
                Kunde.Adresse1        = tt_Medlem.Adresse1
                Kunde.Adresse2        = tt_Medlem.Adresse2
                Kunde.PostNr          = tt_Medlem.PostNr
               
                Kunde.Telefon         = tt_Medlem.Telefon     
                Kunde.Telefaks        = tt_Medlem.Telefaks    
                Kunde.KontNavn        = tt_Medlem.Kontakt     
                Kunde.OrgNr           = tt_Medlem.OrgNr       
                Kunde.KreditSperret   = FALSE
                Kunde.MaksKredit      = tt_Medlem.Kredittgrense 

                Kunde.Kjon            = 0
                Kunde.BetType         = 2 /* Kredittkunde */
                Kunde.ButikkNr        = IF Kunde.ButikkNr = 0
                                          THEN iCl 
                                          ELSE Kunde.ButikkNr
                Kunde.Kilde           = tt_Medlem.Kilde
                Kunde.TilgKilde       = tt_Medlem.TilgKilde
                Kunde.TotalRabatt%    = tt_Medlem.Rabatt
                Kunde.EksterntKundeNr = tt_Medlem.EksterntKundeNr
                .
          FIND FIRST KundeGruppe NO-LOCK WHERE
              KundeGruppe.Beskrivelse MATCHES "* " + STRING(tt_Medlem.Rabatt) + "%*" NO-ERROR.
          IF AVAILABLE KundeGruppe THEN
              Kunde.GruppeId = KundeGruppe.GruppeId.
          
          RUN medlemTilWeb (KundeKort.KortNr).          
        END. /*OPPDATERKUNDE */

        IF TRIM(KundeKort.Innehaver) = "" 
            OR KundeKort.Innehaver BEGINS "Ukjent" THEN
        DO:
            FIND Kunde NO-LOCK WHERE
                Kunde.KundeNr = KundeKort.KundeNr NO-ERROR.
            IF AVAILABLE Kunde THEN
            DO:
                FIND CURRENT KundeKort EXCLUSIVE-LOCK.
                ASSIGN
                    KundeKort.Innehaver = Kunde.Navn
                    .
            END.
        END.
    
    END. /* TRANSACTION */

    DELETE tt_Medlem.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-OppdMedlemInfo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OppdMedlemInfo Procedure 
PROCEDURE OppdMedlemInfo :
/*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:                                                                                                                                            
        ------------------------------------------------------------------------------*/

DUBLETT:
FOR EACH tt_Medlem WHERE
    tt_Medlem.Dublett = TRUE:

    piAntLinjer = piAntLinjer + 1.
    STATUS DEFAULT "Eksisterende - Oppdaterer linje " + 
                   STRING(piAntLinjer) + 
                   " av " + 
                   STRING(iTotAntLinjer) + 
                   ".".

    /* Når det eksterne id skal være likt det interne kortnr */
    ASSIGN
        cKortNr = STRING(tt_Medlem.EksterntKundeNr).
    IF AVAILABLE KundeKort THEN
        RELEASE KundeKort.
    IF cKortNr <> "" THEN
        FIND FIRST KundeKort NO-LOCK WHERE
        KundeKort.KortNr = cKortNr NO-ERROR.
    /* Oppdaterer informasjon på kunden. */

    IF AVAILABLE KundeKort THEN
    KUNDEINFO: 
    DO TRANSACTION:
        FIND Kunde EXCLUSIVE-LOCK WHERE 
            Kunde.KundeNr = KundeKort.KundeNr NO-ERROR.
        IF NOT AVAILABLE Kunde THEN 
            LEAVE KUNDEINFO.
            
        ASSIGN
            Kunde.Navn            = tt_Medlem.Kundenavn
            Kunde.ePostAdresse    = tt_Medlem.eMail
            Kunde.MobilTlf        = tt_Medlem.Mobil
            Kunde.Adresse1        = tt_Medlem.Adresse1
            Kunde.Adresse2        = tt_Medlem.Adresse2
            Kunde.PostNr          = tt_Medlem.PostNr
           
            Kunde.Telefon         = tt_Medlem.Telefon     
            Kunde.Telefaks        = tt_Medlem.Telefaks    
            Kunde.KontNavn        = tt_Medlem.Kontakt     
            Kunde.OrgNr           = tt_Medlem.OrgNr       
            /*Kunde.KreditSperret   = FALSE*/
            Kunde.MaksKredit      = tt_Medlem.Kredittgrense 

            /*Kunde.Kjon            = 0*/
            Kunde.BetType         = 2 /* Kredittkunde */
            Kunde.ButikkNr        = IF Kunde.ButikkNr = 0
                                      THEN iCl 
                                      ELSE Kunde.ButikkNr
            Kunde.Kilde           = (IF tt_Medlem.Kilde <> '' THEN tt_Medlem.Kilde ELSE Kunde.Kilde)
            Kunde.TilgKilde       = (IF tt_Medlem.TilgKilde <> '' THEN tt_Medlem.TilgKilde ELSE Kunde.TilgKilde)
            Kunde.TotalRabatt%    = tt_Medlem.Rabatt
            
            /*Kunde.EksterntKundeNr = (IF tt_Medlem.EksterntKundeNr <> THEN tt_Medlem.EksterntKundeNr ELSE Medlem.EksterntKundeNr)*/
            .
        FIND FIRST KundeGruppe NO-LOCK WHERE
            KundeGruppe.Beskrivelse MATCHES "* " + STRING(tt_Medlem.Rabatt) + "%*" NO-ERROR.
        IF AVAILABLE KundeGruppe THEN
            Kunde.GruppeId = KundeGruppe.GruppeId.
      
        RUN medlemTilWeb (KundeKort.KortNr).          
    
        IF KundeKort.InterntKKortId > 0 THEN FIND 
          FIRST MedlemsKort NO-LOCK WHERE
              MedlemsKort.InterntKKortId = KundeKort.InterntKKortId NO-ERROR.
        /* Oppdaterer informasjon på medlemmet */
        IF AVAILABLE MedlemsKort THEN
        MEDLEMINFO: 
        DO:
            FIND Medlem EXCLUSIVE-LOCK WHERE
                Medlem.MedlemsNr = MedlemsKort.MedlemsNr NO-ERROR.
            IF NOT AVAILABLE Medlem THEN 
                LEAVE MEDLEMINFO.
                    
            ASSIGN
                Medlem.Fornavn        = IF (tt_Medlem.fornavn + tt_Medlem.Etternavn) = ""
                                          THEN tt_Medlem.Kundenavn
                                          ELSE tt_Medlem.Fornavn
                Medlem.EtterNavn      = tt_Medlem.EtterNavn
                Medlem.FodtAr         = YEAR(dFDato)
                Medlem.ePostAdresse   = tt_Medlem.eMail
                Medlem.MobilTlf       = tt_Medlem.Mobil
                Medlem.Adresse1       = tt_Medlem.Adresse1
                Medlem.Adresse2       = tt_Medlem.Adresse2
                Medlem.PostNr         = tt_Medlem.PostNr
                /*Medlem.PostAdrese   = tt_Medlem.PostSted */
                /*Medlem.Kjonn          = FALSE            */
                Medlem.aktivertFraWeb = TODAY
                Medlem.ButikkNr       = IF Medlem.ButikkNr = 0
                                         THEN iCl 
                                         ELSE Medlem.ButikkNr
                Medlem.Kilde          = (IF tt_Medlem.Kilde <> '' THEN tt_Medlem.Kilde ELSE Medlem.Kilde)
                Medlem.TilgKilde      = (IF tt_Medlem.TilgKilde <> '' THEN tt_Medlem.TilgKilde ELSE Medlem.TilgKilde)
                Medlem.Rabatt         = tt_Medlem.Rabatt
                Medlem.EksterntMedlemsNr = (IF tt_Medlem.EksterntMedlemsNr <> '' THEN tt_Medlem.EksterntMedlemsNr ELSE Medlem.EksterntMedlemsNr)
                .
        END. /* MEDLEMINFO */
    END. /* KUNDEINFO TRANSACTION */

END. /* DUBLETT */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-TellOppLinjer) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE TellOppLinjer Procedure 
PROCEDURE TellOppLinjer :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  ASSIGN
      iTotAntLinjer = 0
      .
  INPUT STREAM InnFil FROM VALUE(VPIFilHode.Katalog + "~\" + VPIFilHode.FilNavn) NO-ECHO.
  REPEAT:
    IMPORT STREAM InnFil UNFORMATTED cLinje.
    ASSIGN
        iTotAntLinjer = iTotAntLinjer + 1
        .
  END.
  INPUT STREAM InnFil CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

