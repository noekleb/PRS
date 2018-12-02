&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : xvpispinnles.p
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
DEF INPUT  PARAMETER lFilId      AS DEC    NO-UNDO.
DEF INPUT  PARAMETER h_Parent    AS HANDLE NO-UNDO.
DEF OUTPUT PARAMETER iAntLinjer  AS INT    NO-UNDO.

DEF VAR iTotAntLinjer AS INT  NO-UNDO.
DEF VAR cLinje        AS CHAR NO-UNDO.
DEF VAR cFilNavn      AS CHAR NO-UNDO.
DEF VAR cTekst        AS CHAR NO-UNDO.
DEF VAR bOk           AS LOG  NO-UNDO.
DEF VAR iCl           AS INT  NO-UNDO.

DEF STREAM InnFil.

{windows.i}

{medweb.i}

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
    cFilNavn = VPIFilHode.Katalog + "~\" + VPIFilHode.FilNavn.

RUN LesInnFil.

IF CAN-FIND(FIRST tt_Medlem) AND bOk = TRUE THEN
    RUN OppdaterMedlem.

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

&IF DEFINED(EXCLUDE-LesInnFil) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LesInnFil Procedure 
PROCEDURE LesInnFil :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR piLinjeNr  AS INT  NO-UNDO.
  DEF VAR pcLinje    AS CHAR NO-UNDO.
  DEF VAR piAntFeil  AS INT  NO-UNDO.
  DEF VAR pcSep      AS CHAR NO-UNDO.
  DEF VAR iButikkNr  AS INT  NO-UNDO.

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
      iAntLinjer = 0
      .
  INPUT STREAM InnFil FROM VALUE(cFilNavn) NO-ECHO.
  LESERLINJER:
  REPEAT:
    /* Leser linje fra filen */
    IMPORT STREAM InnFil UNFORMATTED pcLinje.

    assign
      iAntLinjer = iAntLinjer + 1
      iButikkNr  = int(ENTRY( 2,pcLinje,pcSep))
      NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
        NEXT LESERLINJER.

    /* Håndterer kontrollrecord */
    IF int(ENTRY( 1,pcLinje,pcSep)) = 9 THEN
    DO:
        IF iTotAntLinjer = int(ENTRY( 2,pcLinje,pcSep))
            THEN bOk = TRUE.
        LEAVE LESERLINJER.
    END.

    IF NUM-ENTRIES(pcLinje,pcSep) <> 19 THEN
    DO:
      ASSIGN
        piAntFeil = piAntFeil + 1
        cTekst = "Feil antall elementer på linje " + 
                 string(iAntLinjer) + "." +
                 " (Skal være 17, det er " + string(NUM-ENTRIES(pcLinje,pcSep)) + ").".
      PUBLISH "VPIFilLogg" (cTekst + chr(1) + "3").
    END.

    /* Posterer linjen */
    CREATE tt_Medlem.
    ASSIGN
        tt_Medlem.iRecType     = int(ENTRY( 1,pcLinje,pcSep))
        tt_Medlem.ButikkNr     = int(ENTRY( 2,pcLinje,pcSep))
        tt_Medlem.EksterntMedlemsNr = ENTRY( 3,pcLinje,pcSep)
        tt_Medlem.MedlemsNr    = dec(ENTRY( 4,pcLinje,pcSep))
        tt_Medlem.EAN          = dec(ENTRY( 5,pcLinje,pcSep))  
        tt_Medlem.Fornavn      = trim(ENTRY( 6,pcLinje,pcSep),'"')
        tt_Medlem.Etternavn    = trim(ENTRY( 7,pcLinje,pcSep),'"')
        tt_Medlem.KundeNr      = dec(ENTRY( 8,pcLinje,pcSep))
        tt_Medlem.KundeNavn    = trim(ENTRY( 9,pcLinje,pcSep),'"')
        tt_Medlem.cFDato       = trim(ENTRY(10,pcLinje,pcSep),'"')
        tt_Medlem.Telefon      = trim(ENTRY(11,pcLinje,pcSep),'"')
        tt_Medlem.Mobil        = trim(ENTRY(12,pcLinje,pcSep),'"')
        tt_Medlem.Adresse1     = trim(ENTRY(13,pcLinje,pcSep),'"') 
        tt_Medlem.Adresse2     = trim(ENTRY(14,pcLinje,pcSep),'"') 
        tt_Medlem.PostNr       = trim(ENTRY(15,pcLinje,pcSep),'"') 
        tt_Medlem.Poststed     = trim(ENTRY(16,pcLinje,pcSep),'"') 
        tt_Medlem.eMail        = trim(ENTRY(17,pcLinje,pcSep),'"')
        tt_Medlem.cKjonn       = trim(ENTRY(18,pcLinje,pcSep),'"')
        tt_Medlem.WebBrukerId  = trim(ENTRY(19,pcLinje,pcSep),'"')
        tt_Medlem.WebPassord   = trim(ENTRY(20,pcLinje,pcSep),'"')
        tt_Medlem.Aktiv        = int(ENTRY(21,pcLinje,pcSep)) 
        piLinjeNr              = piLinjeNr  + 1
        .

    STATUS DEFAULT "Lese linje " + 
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

&IF DEFINED(EXCLUDE-OppdaterMedlem) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OppdaterMedlem Procedure 
PROCEDURE OppdaterMedlem :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR dFDato      AS DATE NO-UNDO.
DEF VAR cKortNr     AS CHAR NO-UNDO.
DEF VAR plKundeNr   AS DEC  NO-UNDO.
DEF VAR plMedlemsNr AS DEC  NO-UNDO.
DEF VAR bOk         AS LOG  NO-UNDO.
DEF VAR piGruppeId  AS INT  NO-UNDO.
DEF VAR cMsgs       AS CHAR NO-UNDO.
DEF VAR cMedlemsNr AS CHAR NO-UNDO.

FIND FIRST Kundegruppe NO-LOCK WHERE
    KundeGruppe.GruppeId > 0 NO-ERROR.
IF NOT AVAILABLE KundeGruppe 
    THEN piGruppeId = 1.
    ELSE piGruppeId = KundeGruppe.GruppeId.

FOR EACH tt_Medlem:
    ASSIGN
        cMedlemsNr = LEFT-TRIM(SUBstring(STRING(tt_Medlem.EAN,"9999999999999"),3,10),"0")
        .

    DO TRANSACTION:
        IF AVAILABLE KundeKort THEN
            RELEASE KundeKort.

        FIND FIRST MedlemsKort NO-LOCK WHERE
            MedlemsKort.KortNr = cMedlemsNr NO-ERROR.

        IF AVAILABLE MedlemsKort AND MedlemsKort.InterntKKortId > 0 THEN
            FIND FIRST KundeKort NO-LOCK WHERE
              KundeKort.InterntKKortId = Medlemskort.InterntKKortId NO-ERROR.
        IF NOT AVAILABLE KundeKort THEN
            FIND FIRST KundeKort NO-LOCK WHERE
            KundeKort.KortNr = string(tt_Medlem.MedlemsNr) NO-ERROR.

        /* GENERER KUNDE og MEDLEM */
        IF (NOT AVAILABLE KundeKort AND int(tt_Medlem.MedlemsNr) < 1000000) THEN
        DO:
            ASSIGN /* Kunde skal opprettes */
                plKundeNr   = 0
                plMedlemsNr = 0
                .
            /* Kunde og medlem skal opprettes. */
            RUN genkundeMedlem.p (ICL,
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

            RUN genkundekort_og_medlem.p (iCL,
                                          plKundeNr,
                                          plMedlemsNr,
                                          int(cMedlemsNr),
                                          int(cMedlemsNr),
                                          365,
                                          OUTPUT bOk,
                                          OUTPUT cMsgs).
        END. /* GENERER KUNDE og MEDLEM */

        FIND FIRST MedlemsKort NO-LOCK WHERE
            MedlemsKort.KortNr = string(cMedlemsNr) NO-ERROR.
        
        IF AVAILABLE MedlemsKort AND MedlemsKort.InterntKKortId > 0 THEN
            FIND FIRST KundeKort NO-LOCK WHERE
              KundeKort.InterntKKortId = Medlemskort.InterntKKortId NO-ERROR.
        IF NOT AVAILABLE KundeKort THEN
            FIND FIRST KundeKort NO-LOCK WHERE
            KundeKort.KortNr = string(tt_Medlem.MedlemsNr) NO-ERROR.
        FIND Kunde OF Kundekort NO-LOCK NO-ERROR.
        FIND Medlem EXCLUSIVE-LOCK WHERE
            Medlem.MedlemsNr = MedlemsKort.MedlemsNr NO-ERROR.

        /* Oppdaterer kortstatus */
/*         IF AVAILABLE MedlemsKort THEN                                              */
/*         DO:                                                                        */
/*             FIND CURRENT MedlemsKort.                                              */
/*             ASSIGN                                                                 */
/*                 MedlemsKort.Sperret = IF tt_Medlem.Aktiv = 1 THEN FALSE ELSE TRUE  */
/*                 Medlem.Aktiv        = IF MedlemsKort.Sperret THEN FALSE ELSE TRUE. */
/*         END.                                                                       */

        /* Oppdaterer medlemsinformasjonen */
        IF AVAILABLE Medlem THEN
        DO:
            ASSIGN
                dFDato = DATE(INT(ENTRY(2,tt_Medlem.cFDato,'/')),
                              INT(ENTRY(1,tt_Medlem.cFDato,'/')),
                              INT(ENTRY(3,tt_Medlem.cFDato,'/'))
                              )
                NO-ERROR.
            ASSIGN
                Medlem.Fornavn        = tt_Medlem.Fornavn
                Medlem.EtterNavn      = tt_Medlem.EtterNavn
                Medlem.Fodselsdato    = dFDato
                Medlem.FodtAr         = YEAR(dFDato)
                Medlem.ePostAdresse   = tt_Medlem.eMail
                Medlem.MobilTlf       = tt_Medlem.Mobil
                Medlem.Adresse1       = tt_Medlem.Adresse1
                Medlem.Adresse2       = tt_Medlem.Adresse2
                Medlem.PostNr         = tt_Medlem.PostNr
                /*Medlem.PostAdrese   = tt_Medlem.PostSted */
                Medlem.Kjonn          = IF tt_Medlem.cKjonn = "M"
                                          THEN TRUE
                                         ELSE FALSE
                Medlem.aktivertFraWeb = TODAY
                Medlem.WebBrukerId    = tt_Medlem.WebBrukerId
                Medlem.WebPassord     = tt_Medlem.WebPassord
                Medlem.EksterntMedlemsNr = tt_Medlem.EksterntMedlemsNr
                .

            ASSIGN
                cKortNr = SUBSTRING(string(tt_Medlem.EAN,"9999999999999"),3)
                cKortNr = SUBSTRING(cKortNr,1,LENGTH(cKortNr) - 1)
                cKortNr = LEFT-TRIM(cKortNr,'0')
                NO-ERROR.
            IF ERROR-STATUS:ERROR = FALSE THEN
            DO:
                FIND Medlemskort EXCLUSIVE-LOCK WHERE
                    MedlemsKort.MedlemsNr = Medlem.MedlemsNr AND
                    MedlemsKort.KortNr    = cKortNr NO-ERROR.
                IF AVAILABLE Medlemskort THEN
                DO:
                    /* Her oppdateres medlemskortet.                              */
                    /* Koblede kundekort oppdateres via w_melemskort.p (Trigger). */
                    ASSIGN
                        MedlemsKort.Innehaver = Medlem.ForNavn + " " + Medlem.EtterNavn
                        MedlemsKort.Merknad   = "Init fra Web " + STRING(TODAY)
                        .
                END.
            END.
        END. /* Oppdatering av medlem. */
        /* Oppdaterer kundekort */
        IF MedlemsKort.InterntKKortId > 0 THEN
        DO:
            FIND KundeKort EXCLUSIVE-LOCK WHERE
                 KundeKort.InterntKKortId = Medlemskort.InterntKKortId NO-ERROR.
            IF AVAILABLE KundeKort THEN
            DO:
                ASSIGN
                    KundeKort.Innehaver    = MedlemsKort.Innehaver
                    KundeKort.AktivertDato = Medlemskort.aktivertDato
                    KundeKort.UtgarDato    = MedlemsKort.UtgarDato
                    KundeKort.Sperret      = MedlemsKort.Sperret
                    .
            END.
        END.


        /* Oppdaterer kundedata */
        IF AVAILABLE Kunde AND trim(Kunde.Navn) = "" OR
            Kunde.Navn BEGINS "Ukjent" THEN
        OPPDATERKUNDE:
        DO:
            FIND Kunde EXCLUSIVE-LOCK WHERE
                Kunde.KundeNr = KundeKort.KundeNr.
            ASSIGN
                Kunde.Navn      = tt_Medlem.Fornavn + " " + tt_Medlem.EtterNavn
                Kunde.Fodtdato  = dFDato
                Kunde.ePostAdresse   = tt_Medlem.eMail
                Kunde.MobilTlf       = tt_Medlem.Mobil
                Kunde.Adresse1       = tt_Medlem.Adresse1
                Kunde.Adresse2       = tt_Medlem.Adresse2
                Kunde.PostNr         = tt_Medlem.PostNr
                Kunde.Kjon           = IF tt_Medlem.cKjonn = "M"
                                        THEN 1
                                        ELSE 0
                Kunde.MaksKredit     = IF Kunde.MaksKredit > 100
                                            THEN Kunde.MaksKredit
                                            ELSE 100
                Kunde.BetType        = 2 /* Kredittkunde */
                .
        END. /*OPPDATERKUNDE */
    END. /* TRANSACTION */

    DELETE tt_Medlem.
END.

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
  repeat:
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

