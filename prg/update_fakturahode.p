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
DEF INPUT  PARAMETER lFaktura_Id AS DEC  NO-UNDO.
DEF INPUT  PARAMETER cFields     AS CHAR NO-UNDO.
DEF INPUT  PARAMETER cValues     AS CHAR NO-UNDO.
/* DUMMY - Opphav brukes ikke her. Settes på fakturalinjen. */
DEF INPUT  PARAMETER iOpphav     AS INT  NO-UNDO.

DEF BUFFER b1Post FOR Post.

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

FIND FakturaHode NO-LOCK WHERE
    FakturaHode.Faktura_Id = lFaktura_id NO-ERROR.
IF NOT AVAILABLE FakturaHode THEN
    RETURN "AVBRYT".
FIND Kunde NO-LOCK WHERE
    Kunde.KundeNr = FakturaHode.KundeNr NO-ERROR.
IF NOT AVAILABLE Kunde THEN              /* Det stod "IF NOT AVAILABLE FakturaHode THEN" ghg 20121101 */
    RETURN "AVBRYT".

RELEASE Butiker.

IF cFields = "Fakturagebyr" AND Kunde.SamleFaktura = TRUE THEN
    RETURN "AVBRYT".

/* Initier faktura første gang */
IF cFields = "INIT" THEN DO:
    RUN initFaktura.
    /* Vid fakturaproduktion så sänder vi iOpphav = 9. Det betyder att om kunden skall ha fakturaavgift och om det är samlefaktura så skall 
       fakturaavgift debiteras */
    IF iOpphav = 9 AND Kunde.SamleFaktura = TRUE AND Kunde.Fakturagebyr = TRUE THEN DO:
        cFields = "Fakturagebyr".
        RUN updateFaktura.
    END.
END.
ELSE IF cFields <> "" THEN
    RUN updateFaktura.
ELSE
    RETURN "AVBRYT".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-Fakturagebyr) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Fakturagebyr Procedure 
PROCEDURE Fakturagebyr :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR piLinjeNr AS INT NO-UNDO.
  DEFINE VARIABLE cSprak AS CHARACTER   NO-UNDO.
  IF FakturaHode.FakturaGebyr = TRUE THEN
      RETURN.

  FIND Butiker WHERE
      Butiker.Butik = FakturaHode.ButikkNr EXCLUSIVE NO-ERROR.
  IF NOT AVAILABLE Butiker OR Butiker.Fakturagebyr <= 0 THEN
      RETURN.
  FIND CURRENT Butiker NO-LOCK NO-ERROR.
  IF Kunde.Fakturagebyr = FALSE THEN
      RETURN.

  FIND LAST FakturaLinje OF FakturaHode NO-LOCK NO-ERROR.
  IF AVAILABLE FakturaLinje THEN
      piLinjeNr = FakturaLinje.FakturaLinjeNr + 1.
  ELSE
      piLinjeNr = 1.

FIND bruker WHERE bruker.brukerid = USERID("skotex") NO-LOCK NO-ERROR.
IF AVAIL bruker THEN
    cSprak = bruker.lng.
IF TRIM(cSprak) = "" THEN
    cSprak = "NN".

/*   FIND Bilagstype NO-LOCK WHERE            */
/*       Bilagstype.Bilagstype = 11 NO-ERROR. */
  FIND Moms NO-LOCK WHERE
      Moms.Momskod = Butiker.FGMomskod NO-ERROR.
  IF NOT AVAILABLE Moms THEN
      FIND FIRST Moms NO-LOCK NO-ERROR.

  PURREGEBYR:
  DO:
      CREATE FakturaLinje.
      ASSIGN
          FakturaHode.FakturaGebyr    = TRUE
          FakturaLinje.Faktura_Id     = FakturaHode.Faktura_Id
          FakturaLinje.FakturaLinjeNr = piLinjeNr
          piLinjeNr                   = piLinjeNr + 1
          FakturaLinje.Opphav         = 20
          FakturaLinje.Leveringsdato  = TODAY 
          FakturaLinje.TTId           = 0
          FakturaLinje.TBId           = 0
          FakturaLinje.EkstRefId      = ""
          FakturaLinje.EkstRefTekst   = ""
          .
      ASSIGN
          FakturaLinje.Antall        = 1
          FakturaLinje.VareNr        = ""
          FakturaLinje.Varetekst     = STRING(cSprak = "SE","Fakturaavgift/Fakturagebyr")
          FakturaLinje.LinjeRabattKr = 0
          FakturaLinje.LinjeRab%     = 0
          FakturaLinje.Pris          = Butiker.Fakturagebyr
          FakturaLinje.NettoPris     = Butiker.Fakturagebyr
          FakturaLinje.NettoLinjeSum = Butiker.Fakturagebyr * FakturaLinje.Antall
          FakturaLinje.Mva%          = (IF AVAILABLE Moms
                                          THEN Moms.MomsProc
                                          ELSE 0)
          FakturaLinje.MvaKr         = (FakturaLinje.NettoPris * FakturaLinje.Mva%) / 100
          FakturaLinje.MvaKr         = IF FakturaLinje.MvaKr = ?
                                         THEN 0
                                         ELSE FakturaLinje.MvaKr
          FakturaLinje.MomsKod       = 0
          FakturaLinje.Leveringsdato = TODAY
          FakturaLinje.Storl         = ""
          FakturaLinje.DbKr          = 0
          FakturaLinje.Db%           = 0
          FakturaLinje.Linjesum      = (FakturaLinje.NettoPris * FakturaLinje.Antall) + FakturaLinje.MvaKr
          FakturaLinje.EkstRefId     = ""
          FakturaLinje.EkstRefTekst  = ""
          .
  END. /* PURREGEBYR */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-initFaktura) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initFaktura Procedure 
PROCEDURE initFaktura :
/*------------------------------------------------------------------------------
  Purpose:     Initiering av faktura når det initieres fra kassen.
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  FIND b1Post NO-LOCK WHERE
      b1Post.PostNr = Kunde.PostNr NO-ERROR.

  /*
  Butikknummer, kundenummer, Leveringsform og faktura_Id er satt av rutine som opprettet
  faktura.
  */
  DO TRANSACTION:
      FIND CURRENT FakturaHode EXCLUSIVE-LOCK.
      FIND Betalingsbetingelser NO-LOCK WHERE
          Betalingsbetingelser.BetBet = Kunde.BetBet NO-ERROR.
      IF FakturaHode.BilagsType <> 0 THEN 
        FIND BilagsType NO-LOCK WHERE 
          BilagsType.BilagsType = FakturaHode.BilagsType NO-ERROR.
      IF NOT AVAILABLE BilagsType THEN 
        FIND Bilagstype NO-LOCK WHERE
          Bilagstype.Bilagstype = 1 NO-ERROR.
      ASSIGN
          FakturaHode.Navn         = Kunde.Navn
          FakturaHode.Adresse1     = Kunde.Adresse1
          FakturaHode.Adresse2     = Kunde.Adresse2
          FakturaHode.PostNr       = Kunde.PostNr
          FakturaHode.PostSted     = (IF AVAILABLE b1Post
                                        THEN b1Post.Beskrivelse
                                        ELSE "")
          FakturaHode.Telefon      = Kunde.Telefon
          FakturaHode.KontNavn     = Kunde.KontNavn
          FakturaHode.KontTelefon  = Kunde.KontTelefon
          FakturaHode.Telefaks     = Kunde.Telefaks
          FakturaHode.FaktTekstNr  = Kunde.FaktTekstNr
          FakturaHode.BetBet       = Kunde.BetBet
          FakturaHode.ValKod       = Kunde.ValKod
          FakturaHode.DeresRef     = Kunde.KontNavn
          FakturaHode.Land         = Kunde.Land
          FakturaHode.BetTekst     = (IF AVAILABLE Betalingsbetingelser 
                                        THEN Betalingsbetingelser.BetTekst
                                        ELSE "")
          FakturaHode.Bilagstype   = BilagsType.BilagSType
          FakturaHode.BTTekst      = (IF AVAILABLE Bilagstype
                                        THEN Bilagstype.BTTekst
                                        ELSE "Faktura")
          .
      FIND FakturaTekst NO-LOCK WHERE
          FakturaTekst.FaktTekstNr = FakturaHode.FaktTekstNr NO-ERROR.
      IF AVAILABLE FakturaTekst THEN
          FakturaHode.fNotat = FakturaHode.FNotat + 
                               (IF FakturaHode.FNotat <> '' THEN CHR(19) ELSE '') + 
                               FakturaTekst.FaktTekst.

      /* Fakturaadresse */
      IF Kunde.FaktAdresse1 <> "" THEN
      DO:
          FIND b1Post NO-LOCK WHERE
              b1Post.PostNr = Kunde.FaktPostNr NO-ERROR.
          ASSIGN
          FakturaHode.FaktAdresse1 = Kunde.FaktAdresse1
          FakturaHode.FaktAdresse2 = Kunde.FaktAdresse2
          FakturaHode.FaktPostNr   = Kunde.FaktPostNr
          FakturaHode.FaktPostSted = IF AVAILABLE b1Post
                                       THEN b1Post.Beskrivelse
                                       ELSE ""
          FakturaHode.FaktLand     = Kunde.FaktLand
          .
      END.
      ELSE DO:
          ASSIGN
          FakturaHode.FaktAdresse1 = Kunde.Adresse1
          FakturaHode.FaktAdresse2 = Kunde.Adresse2
          FakturaHode.FaktPostNr   = Kunde.PostNr
          FakturaHode.FaktPostSted = IF AVAILABLE b1Post
                                       THEN b1Post.Beskrivelse
                                       ELSE ""
          FakturaHode.FaktLand     = Kunde.Land
          .
      END.

      /* Fakturaadresse */
      IF Kunde.LevAdresse1 <> "" THEN
      DO:
          FIND b1Post NO-LOCK WHERE
              b1Post.PostNr = Kunde.LevPostNr NO-ERROR.
          ASSIGN
          FakturaHode.LevAdresse1 = Kunde.LevAdresse1
          FakturaHode.LevAdresse2 = Kunde.LevAdresse2
          FakturaHode.LevPostNr   = Kunde.LevPostNr
          FakturaHode.LevPostSted = IF AVAILABLE b1Post
                                       THEN b1Post.Beskrivelse
                                       ELSE ""
          FakturaHode.LevLand     = Kunde.LevLand
          .
      END.

      FIND Butiker WHERE
          Butiker.Butik = FakturaHode.ButikkNr EXCLUSIVE NO-ERROR.
      IF AVAILABLE Butiker THEN DO:
          FIND CURRENT Butiker NO-LOCK.
          FIND b1Post NO-LOCK WHERE
              b1Post.PostNr = Butiker.BuPonr NO-ERROR.
      END.
      IF AVAILABLE Butiker THEN
          ASSIGN
          FakturaHode.FirmaNavn            = IF TRIM(ButFirmaNavn) <> '' 
                                               THEN TRIM(ButFirmaNavn)
                                               ELSE Butiker.ButNamn
          FakturaHode.FirmaAdresse1        = Butiker.BuAdr
          FakturaHode.FirmaAdresse2        = ""
          FakturaHode.FirmaPostNr          = Butiker.BuPonr
          FakturaHode.FirmaPostSted        = (IF AVAILABLE b1Post
                                               THEN b1Post.Beskrivelse
                                               ELSE "")
          FakturaHode.VaarRef              = (IF Butiker.VaarRef <> ""
                                               THEN Butiker.VaarRef
                                               ELSE Butiker.BuKon)
          FakturaHode.FirmaTelefon         = Butiker.BuTel
          FakturaHode.FirmaOrganisasjonsNr = Butiker.OrganisasjonsNr
          FakturaHode.FirmaBankKonto       = Butiker.BankKonto
          FakturaHode.FirmaPostgiro        = Butiker.PostGiro  
          FakturaHode.FirmaURLAdresse      = Butiker.URLAdresse
          FakturaHode.FirmaEPost           = Butiker.ePostAdresse
          FakturaHode.FirmaLand            = Butiker.ButLand
          FakturaHode.FirmaTelefaks        = Butiker.Telefaks
          .

      FIND CURRENT FakturaHode NO-LOCK.
  END. /* TRANSACTION */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-KalkulerTotaler) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE KalkulerTotaler Procedure 
PROCEDURE KalkulerTotaler :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR piLoop AS INT NO-UNDO.
  DEF VAR plDiff AS DEC NO-UNDO.
  DO TRANSACTION:
      FIND CURRENT FakturaHode EXCLUSIVE-LOCK.

      /* Nullstill */
      ASSIGN
          FakturaHode.AvgPlSalg     = 0
          FakturaHode.AvgFriSalg    = 0
          FakturaHode.MvaKr         = 0
          FakturaHode.Totalt        = 0
          FakturaHode.AvrundingKr   = 0
          FakturaHode.TotalRabattKr = 0
          FakturaHode.NettoPris     = 0

          .
      SUMMER:
      FOR EACH FakturaLinje OF FakturaHode NO-LOCK:
          /* Forskuddsbetaling */
          IF CAN-DO("50,51,52,53,54,55,56,57,58,61,66,69,70,71,72,73,78,79",STRING(FakturaLinje.TTId)) THEN
              ASSIGN
              FakturaHode.Forskuddsbetalt = FakturaHode.Forskuddsbetalt + FakturaLinje.NettoLinjeSum
              .
          ELSE /* Varesalg */
          ASSIGN
              FakturaHode.AvgPlSalg       = FakturaHode.AvgPlSalg  + (IF FakturaLinje.Mva% > 0
                                                                       THEN FakturaLinje.NettoLinjeSum
                                                                       ELSE 0)
              FakturaHode.AvgFriSalg      = FakturaHode.AvgFriSalg + (IF FakturaLinje.Mva% = 0
                                                                      THEN FakturaLinje.NettoLinjeSum
                                                                      ELSE 0)
              FakturaHode.MvaKr           = FakturaHode.MvaKr         + FakturaLinje.MvaKr
              FakturaHode.NettoPris       = FakturaHode.NettoPris     + FakturaLinje.NettoLinjeSum
              .
          /* Her tar vi begge under ett, for betaling har ingenting i noen av feltene */
          ASSIGN
              FakturaHode.Totalt          = FakturaHode.Totalt        + FakturaLinje.LinjeSum
              FakturaHode.TotalRabattKr   = FakturaHode.TotalRabattKr + FakturaLinje.TotalrabattKr              .
      END. /* SUMMER */

      /* Rabatt */
      ASSIGN
          FakturaHode.TotalRabatt% = ABS(FakturaHode.TotalRabattKr / (FakturaHode.Totalt + FakturaHode.TotalRabattKr)) * 100
          FakturaHode.TotalRabatt% = IF FakturaHode.TotalRabatt% = ? THEN 0 ELSE FakturaHode.TotalRabatt%
          .

      /* Avrunding */
      ASSIGN
          plDiff = FakturaHode.Totalt - round(FakturaHode.Totalt,0)
          .
      IF plDiff < 0.5 THEN
          ASSIGN
              FakturaHode.Totalt      = FakturaHode.Totalt - plDiff
              FakturaHode.AvrundingKr = plDiff * -1
              .
      ELSE 
          ASSIGN
              FakturaHode.Totalt      = FakturaHode.Totalt + plDiff
              FakturaHode.AvrundingKr = plDiff
              .

  END. /* TRANSACTION */
  FIND CURRENT FakturaHode NO-LOCK.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Leveringsform) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Leveringsform Procedure 
PROCEDURE Leveringsform :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  FIND Leveringsform NO-LOCK WHERE
      Leveringsform.LevFNr = FakturaHode.LevFNr NO-ERROR.
  IF AVAILABLE Leveringsform THEN
      ASSIGN
      FakturaHode.LevFormBeskrivelse = Leveringsform.LevFormBeskrivelse
      FakturaHode.LevFormMetode      = Leveringsform.LevFormMetode
      .
      
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-updateFaktura) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE updateFaktura Procedure 
PROCEDURE updateFaktura :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR piLoop AS INT NO-UNDO.
  
  DO TRANSACTION:
      FIND CURRENT FakturaHode EXCLUSIVE-LOCK.

      IF cFields <> "" THEN
      DO piLoop = 1 TO NUM-ENTRIES(cFields):

          CASE ENTRY(piLoop,cFields):
              WHEN "Dato"            THEN FakturaHode.Dato         = DATE(ENTRY(piLoop,cValues,CHR(1))).
              WHEN "Butikksalg"      THEN FakturaHode.Butikksalg   = (IF ENTRY(piLoop,cValues,CHR(1)) = "YES" THEN TRUE ELSE FALSE).
              WHEN "TotalRabatt%"    THEN FakturaHode.TotalRabatt% = dec(ENTRY(piLoop,cValues,CHR(1))).
              WHEN "LevFNr"          THEN 
                                     DO:
                                       FakturaHode.LevFNr = INT(ENTRY(piLoop,cValues,CHR(1))).
                                       RUN Leveringsform.
                                     END.
              WHEN "Leveringsdato"   THEN FakturaHode.Leveringsdato = DATE(ENTRY(piLoop,cValues,CHR(1))).
              WHEN "Utsendelsesdato" THEN FakturaHode.Utsendelsesdato = DATE(ENTRY(piLoop,cValues,CHR(1))).
              WHEN "Referanse"       THEN FakturaHode.Referanse = FakturaHode.Referanse + (IF FakturaHode.Referanse = ""
                                                                     THEN ""
                                                                     ELSE ", ") + ENTRY(piLoop,cValues,CHR(1)) .
              WHEN "BilagsType"      THEN DO:
                  FakturaHode.BilagsType = INT(ENTRY(piLoop,cValues,CHR(1))). 
                  FIND Bilagstype NO-LOCK WHERE
                       Bilagstype.Bilagstype = FakturaHode.BilagsType NO-ERROR.
                  IF AVAILABLE BilagsType THEN
                      FakturaHode.BTTekst = BilagsType.BTTekst.
              END.

              /* Procedurer */
              WHEN "KalkulerTotaler" THEN RUN KalkulerTotaler.
              WHEN "Fakturagebyr"    THEN RUN Fakturagebyr.
          END CASE.
      END.
      FIND CURRENT FakturaHode EXCLUSIVE-LOCK.
  END. /* TRANSACTION */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

