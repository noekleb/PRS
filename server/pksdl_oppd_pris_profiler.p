
/*------------------------------------------------------------------------
    File        : pksdl_oppd_pris_profiler.p
    Purpose     : Synkroniserer pris mellom prisprofiler. 

    Syntax      :

    Description : Oppdaterer øvrige prisprofiler.  

    Author(s)   : tomn
    Created     : Mon Oct 14 09:33:45 CEST 2019
    Notes       : - Ved innlevering av pakkseddler. Prisendring på farge skal treffe alle varene i modellen.
                  - Ved innlevering av pakkseddler. Prisendring skal slå gjensidig på 1 og 16 (Hele modellen).
                  - Sende eMail når prisendring på modell detekteres. Sende til christina@gant.no, Are@Gant.no
                  
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE INPUT PARAMETER cLogg AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER lPkSdlId AS DECIMAL FORMAT ">>>>>>>>>>>>>>9" NO-UNDO.

DEFINE VARIABLE iCl AS INTEGER NO-UNDO.
DEFINE VARIABLE cOutletLst AS CHARACTER NO-UNDO.
DEFINE VARIABLE iLoop AS INTEGER NO-UNDO.
DEFINE VARIABLE bTest AS LOG NO-UNDO.
DEFINE VARIABLE hPrisKo AS HANDLE NO-UNDO.
DEFINE VARIABLE cSkjerm AS CHARACTER NO-UNDO.
DEFINE VARIABLE bOk AS LOG NO-UNDO.
DEFINE VARIABLE cparToADDRESS AS CHARACTER NO-UNDO.
DEFINE VARIABLE iX AS INTEGER NO-UNDO.
DEFINE VARIABLE rStandardFunksjoner AS cls.StdFunk.StandardFunksjoner NO-UNDO.
DEFINE VARIABLE rArtPrisKalkyle AS cls.Artikkel.ArtPrisKalkyle NO-UNDO.
DEFINE VARIABLE rSendEMail AS cls.SendEMail.SendEMail NO-UNDO.
DEFINE VARIABLE cFil AS CHARACTER NO-UNDO.

DEFINE TEMP-TABLE ttPkSdlPris NO-UNDO
  FIELD PkSdlId AS DECIMAL 
  FIELD ArtikkelNr AS DECIMAL FORMAT ">>>>>>>>>>>>>9"
  FIELD ValPris AS DECIMAL 
  FIELD InnkjopsPris AS DECIMAL
  FIELD Rab1Kr AS DECIMAL 
  FIELD Rab1% AS DECIMAL 
  FIELD Varekost AS DECIMAL  
  FIELD MvaKr AS DECIMAL 
  FIELD Mva% AS DECIMAL
  FIELD DbKr AS DECIMAL 
  FIELD Db% AS DECIMAL
  FIELD Pris AS DECIMAL  
  FIELD EuroPris AS DECIMAL  
  FIELD EuroManuel AS LOG
  FIELD Beskr AS CHARACTER  
  FIELD LevKod AS CHARACTER
  FIELD LevFargKod AS CHARACTER 
  FIELD Sesong AS CHARACTER  
  FIELD ProfilNr AS INTEGER
  FIELD ArtBasRecid AS RECID  
  INDEX idxProfil PkSdlId ArtikkelNr ProfilNr.

DEFINE BUFFER bufttPkSdlPris FOR ttPkSdlPris.
DEFINE BUFFER bufArtBas FOR ArtBas. 
DEFINE BUFFER bufArtPris FOR ArtPris.
DEFINE BUFFER clButiker FOR Butiker.

DEFINE STREAM Ut.

rStandardFunksjoner  = NEW cls.StdFunk.StandardFunksjoner( cLogg ) NO-ERROR.
rArtPrisKalkyle  = NEW cls.Artikkel.ArtPrisKalkyle( cLogg ) NO-ERROR.
rSendEMail  = NEW cls.SendEMail.SendEMail( ) NO-ERROR.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
{syspara.i 5 1 1 iCl INT}
{syspara.i 22 5 2 cOutletLst}
{syspara.i 50 50 36 cparToADDRESS}

IF SEARCH('test.txt') <> ? THEN 
  bTest = TRUE.
  
IF bTest THEN 
  rStandardFunksjoner:SkrivTilLogg(cLogg,
      '    Start pksdl_oppd_pris_profiler.p. (PkSdlId: ' + STRING(lPkSdlId) + ').'  
      ).

FIND clButiker NO-LOCK WHERE 
  clbutiker.butik = iCl NO-ERROR.
IF NOT AVAILABLE clButiker THEN 
DO:
  IF bTest THEN 
    rStandardFunksjoner:SkrivTilLogg(cLogg,
        '      Sentrallager ikke angitt.' 
        ).
  RETURN 'AVBRYT'.
END.

FIND PkSdlHode NO-LOCK WHERE 
  PkSdlHode.PkSdlId = lPkSdlId NO-ERROR.
IF NOT AVAILABLE PkSdlHode THEN 
DO:
  IF bTest THEN 
    rStandardFunksjoner:SkrivTilLogg(cLogg,
        '      Ukjent pakkseddel (PkSdlId) ' + STRING(lPkSdlId) + '.' 
        ).
  RETURN 'AVBRYT'.
END.
ELSE 
  IF bTest THEN 
    rStandardFunksjoner:SkrivTilLogg(cLogg,
        '      Mottatt pakkseddel (PkSdlId) ' + STRING(lPkSdlId) + '. Starter bearbeiding av denne.' 
        ).
        
/* Logger linjer som gir prisednring. */
RUN byggTempTable.

/* Er det linjer i loggen, skal prisene deres oppdateres på hele modellen. */
IF CAN-FIND(FIRST ttPkSdlPris) THEN
DO: 
  IF bTest THEN 
    rStandardFunksjoner:SkrivTilLogg(cLogg,
        '      Logglinjer funnet. Starter oppdatering av pris.' 
        ).
  RUN oppdaterPris.
END.

EMPTY TEMP-TABLE ttPkSdlPris.

IF bTest THEN 
  rStandardFunksjoner:SkrivTilLogg(cLogg,
      '    Slutt pksdl_oppd_pris_profiler.p.' 
      ).



/* **********************  Internal Procedures  *********************** */

PROCEDURE byggTempTable:
/*------------------------------------------------------------------------------
 Purpose: Logger prisposter som gir endringer på vare ved varemottak.
 Notes:
------------------------------------------------------------------------------*/
  DEFINE VARIABLE pbLogg AS LOG NO-UNDO.
  DEFINE VARIABLE piModell AS INTEGER NO-UNDO.
  
  FIND FIRST PkSdlLinje OF PkSdlHode NO-LOCK NO-ERROR.
  FIND Butiker NO-LOCK WHERE 
    Butiker.Butik = PkSdlLinje.ButikkNr NO-ERROR.

  IF bTest THEN 
    rStandardFunksjoner:SkrivTilLogg(cLogg,
        '        byggTempTable for pakkseddel (PkSdlId) ' + STRING(lPkSdlId) + '.' 
        ).
    
  PKSDLPRISLOOP:
  FOR EACH PkSdlPris OF PkSdlHode NO-LOCK:
    ASSIGN 
      pbLogg   = FALSE
      piModell = 0 
      .
        
    FIND ArtBas NO-LOCK WHERE 
      ArtBas.ArtikkelNr = PkSdlPris.ArtikkelNr NO-ERROR.
    IF NOT AVAILABLE ArtBas THEN 
      NEXT.

    IF bTest THEN 
      rStandardFunksjoner:SkrivTilLogg(cLogg,
          '          Artikkel: ' + STRING(PkSdlPris.ArtikkelNr) + '.' 
          ).
    
    /* Bare artikler som kan inngå i modell sjekkes. */  
    IF ArtBas.LevKod = '' OR 
       ArtBas.LevFargKod = '' OR 
       ArtBas.Beskr = '' OR 
       ArtBas.ModellFarge = 0 THEN 
       NEXT.

    IF bTest THEN 
      rStandardFunksjoner:SkrivTilLogg(cLogg,
          '          Artikkel inngår i modell: ' + STRING(PkSdlPris.ArtikkelNr) + '.' 
          ).
      
    /* Sjekker om artikkelen ligger i en modell. Gjør den ikke det, skal den ikke logges og gjøres noe med her. */
    FOR EACH bufArtBas NO-LOCK WHERE
      bufArtBas.ModellFarge = ArtBas.ModellFarge:
      piModell = piModell + 1.  
    END.
    IF piModell <= 1 THEN 
      NEXT. 

    IF bTest THEN 
      rStandardFunksjoner:SkrivTilLogg(cLogg,
          '          Antall artikler i modell: ' + STRING(piModell) + '.' 
          ).
    
    /* Henter prisen for profilen. Har profilen ingen pris, skal prisrecord kopieres fra en annen profil. */
    FIND ArtPris NO-LOCK WHERE 
      ArtPris.ArtikkelNr = PkSdlPris.ArtikkelNr AND 
      ArtPris.ProfilNr   = Butiker.ProfilNr NO-ERROR.
    IF NOT AVAILABLE ArtPris THEN
    OPPRETT_LOKAL_PRIS: 
    DO:
      IF bTest THEN 
        rStandardFunksjoner:SkrivTilLogg(cLogg,
            '          Oppretter lokal pris for artikel/profil: ' + STRING(PkSdlPris.ArtikkelNr) + '/' + STRING(Butiker.ProfilNr) + '.' 
            ).

      FIND ArtPris NO-LOCK WHERE 
        ArtPris.ArtikkelNr = PkSdlPris.ArtikkelNr AND 
        ArtPris.ProfilNr   = clButiker.ProfilNr NO-ERROR.
      IF NOT AVAILABLE ArtPris THEN 
      FIND FIRST ArtPris NO-LOCK WHERE 
        ArtPris.ArtikkelNr = PkSdlPris.ArtikkelNr NO-ERROR. 
      /* Oppretter lokal kalkyle. */
      rArtPrisKalkyle:KopierArtPris (PkSdlPris.ArtikkelNr,
                                     ArtPris.ProfilNr,
                                     Butiker.ProfilNr).
      /* Setter korrekt rabatt på lokal kalkyle. */
      rArtPrisKalkyle:SettRabatt (PkSdlPris.ArtikkelNr,
                                     Butiker.ProfilNr,
                                     Butiker.butik).
      FIND ArtPris NO-LOCK WHERE 
        ArtPris.ArtikkelNr = PkSdlPris.ArtikkelNr AND 
        ArtPris.ProfilNr   = Butiker.ProfilNr NO-ERROR.
    END. /* OPPRETT_LOKAL_PRIS */
    
    IF NOT AVAILABLE ArtPris THEN
    DO: 
      IF bTest THEN 
        rStandardFunksjoner:SkrivTilLogg(cLogg,
            '          Finner ikke pris for artikkel/profil: ' + STRING(PkSdlPris.ArtikkelNr) + '/' + STRING(Butiker.ProfilNr) + '.' 
            ).
      NEXT.
    END.
    ELSE 
    OPPRETTLOKALPRIS:
    DO: 
      IF bTest THEN 
        rStandardFunksjoner:SkrivTilLogg(cLogg,
            '          Artpris for artikkel/profil: ' + STRING(PkSdlPris.ArtikkelNr) + '/' + STRING(Butiker.ProfilNr) + ' Pris: ' + STRING(ArtPris.Pris[1]) + '.' 
            ).
      IF bTest THEN 
        rStandardFunksjoner:SkrivTilLogg(cLogg,
            '          Oppretter manglende lokale priser: ' 
            ).
      FOR EACH bufArtBas NO-LOCK WHERE
        bufArtBas.ModellFarge = ArtBas.ModellFarge:
        IF NOT CAN-FIND(bufArtPris WHERE 
                        bufArtPris.ArtikkelNr = bufArtBas.ArtikkelNr AND 
                        bufArtPris.ProfilNr   = ArtPris.ProfilNr) THEN 
        DO:
          CREATE bufArtPris.
          BUFFER-COPY ArtPris 
            EXCEPT ArtikkelNr
            TO bufArtPris
            ASSIGN 
              bufArtPris.ArtikkelNr = bufArtBas.ArtikkelNr
              .        
          IF bTest THEN 
            rStandardFunksjoner:SkrivTilLogg(cLogg,
            '            Ny lokal ArtPris/ProfilNr: ' + STRING(bufArtpris.ArtikkelNr) + '/' + STRING(bufArtPris.ProfilNr) + '.' 
                ).
        END.
      END.
    END. /* OPPRETTLOKALPRIS */
            
    /* Sjekker om det er prisendring. Flagger da at linjen skal logges. */
    IF ArtPris.Pris[1] <> PkSdlPris.NyPris THEN 
    DO:
      IF bTest THEN 
        rStandardFunksjoner:SkrivTilLogg(cLogg,
            '          Avvik i pris ArtPris/PkSdlPris: ' + STRING(Artpris.Pris[1]) + '/' + STRING(PkSdlPris.NyPris) + '.' 
            ).
      pbLogg = TRUE.
    END.
    
    /*Logger linjen og legger inn de andre artikklene i modellen. */
    IF pbLogg THEN
    LOGG: 
    DO:
      IF bTest THEN 
        rStandardFunksjoner:SkrivTilLogg(cLogg,
            '          Logger prisendring artikkel/profil: ' + STRING(PkSdlPris.ArtikkelNr) + '/' + STRING(Butiker.ProfilNr) + 
            ' Fra pris: ' + STRING(ArtPris.Pris[1]) + ' til ' + STRING(PkSdlPris.NyPris) + '.' 
            ).

      CREATE ttPkSdlPris.
      ASSIGN 
        ttPkSdlPris.PkSdlId      = PkSdlPris.PkSdlId 
        ttPkSdlPris.ArtikkelNr   = PkSdlPris.ArtikkelNr
        ttPkSdlPris.EuroManuel   = ArtPris.EuroManuel
        ttPkSdlPris.ProfilNr     = Butiker.ProfilNr
        ttPkSdlPris.ArtBasRecid  = RECID(ArtBas)  
        ttPkSdlPris.Mva%         = ArtPris.Mva%[1]
        ttPkSdlPris.Rab1%        = PkSdlPris.NyRab1% 
        ttPkSdlPris.InnkjopsPris = PkSdlPris.NyInnkjopsPris
        ttPkSdlPris.ValPris      = ttPkSdlPris.InnkjopsPris 
        ttPkSdlPris.Pris         = PkSdlPris.NyPris
        ttPkSdlPris.EuroPris     = PkSdlPris.NyPris
        ttPkSdlPris.ValPris      = ttPkSdlPris.Pris 
        ttPkSdlPris.Varekost     = PkSdlPris.NyVareKost
        ttPkSdlPris.Rab1Kr       = ttPkSdlPris.InnkjopsPris - ttPkSdlPris.Varekost
        ttPkSdlPris.Rab1Kr       = IF ttPkSdlPris.Rab1Kr = ? THEN 0 
                                   ELSE IF ttPkSdlPris.Rab1Kr < 0 THEN 0
                                   ELSE ttPkSdlPris.Rab1Kr  
        ttPkSdlPris.MvaKr        = ttPkSdlPris.Pris - (ttPkSdlPris.Pris / (1 + (ttPkSdlPris.Mva% / 100)))
        ttPkSdlPris.MvaKr        = IF ttPkSdlPris.MvaKr = ? THEN 0 ELSE ttPkSdlPris.MvaKr
        ttPkSdlPris.DbKr         = ttPkSdlPris.Pris - ttPkSdlPris.MvaKr - ttPkSdlPris.Varekost
        ttPkSdlPris.Db%          = ROUND((ttPkSdlPris.DbKr * 100) / (ttPkSdlPris.Pris - ttPkSdlPris.MvaKr),2)
        ttPkSdlPris.Db%          = IF ttPkSdlPris.Db% = ? THEN 0 ELSE ttPkSdlPris.Db%
        ttPksdlPris.Beskr        = ArtBas.Beskr 
        ttPksdlPris.LevKod       = ArtBas.LevKod
        ttPksdlPris.LevFargKod   = ArtBas.LevFargKod
        ttPksdlPris.Sesong       = STRING(ArtBas.Sasong)
        .
          
      IF bTest THEN 
        rStandardFunksjoner:SkrivTilLogg(cLogg,
            '          Kopierer modell:'  
            ).
      /* Kopierer opp for å oppdatere modellen. */
      ARTILER_I_MODELL:    
      FOR EACH bufArtBas NO-LOCK WHERE
        bufArtBas.ModellFarge = ArtBas.ModellFarge:
        IF NOT CAN-FIND(FIRST bufttPkSdlPris WHERE
                        bufttPkSdlPris.ArtikkelNr = bufArtBas.ArtikkelNr AND 
                        bufttPkSdlPris.ProfilNr   = Butiker.ProfilNr) THEN 
        DO:
          CREATE bufttPkSdlPris.
          BUFFER-COPY ttPkSdlPris 
            EXCEPT ArtikkelNr ArtBasRecid 
            TO bufttPkSdlPris
            ASSIGN 
              bufttPkSdlPris.ArtikkelNr  = bufArtBas.ArtikkelNr
              bufttPkSdlPris.ArtBasRecid = RECID(bufArtBas)  
              .
          IF bTest THEN 
            rStandardFunksjoner:SkrivTilLogg(cLogg,
                '            artikkel/profil: ' + STRING(bufttPkSdlPris.ArtikkelNr) + '/' + STRING(bufttPkSdlPris.ProfilNr)  
                ).
        END.  
      END. /* ARTILER_I_MODELL */
    END. /* LOGG */
  END. /* PKSDLPRISLOOP */

  /* Priser på profil 1 skal treffe 16 og motsatt. */
  IF CAN-DO('1,16',STRING(Butiker.ProfilNr)) THEN 
  KOPIER_TIL_PROFIL:
  DO:
    IF bTest THEN 
      rStandardFunksjoner:SkrivTilLogg(cLogg,
          '          Kopierer profil: ' + STRING(Butiker.ProfilNr)  
          ).

    FOR EACH ttPkSdlPris WHERE 
      ttPkSdlPris.ProfilNr = Butiker.ProfilNr:
      CREATE bufttPkSdlPris.
      BUFFER-COPY ttPkSdlPris 
        EXCEPT ProfilNr
        TO bufttPkSdlPris
        ASSIGN 
          bufttPkSdlPris.ProfilNr = (IF Butiker.ProfilNr = 1 THEN 16 ELSE 1)
          .
        IF bTest THEN 
          rStandardFunksjoner:SkrivTilLogg(cLogg,
              '            artikkel/profil: ' + STRING(bufttPkSdlPris.ArtikkelNr) + '/' + STRING(bufttPkSdlPris.ProfilNr)  
              ).
    END.
  END. /* KOPIER_TIL_PROFIL */
  
  IF CAN-FIND(FIRST ttPkSdlPris) THEN 
  DO:
    cFil = 'konv\PrisSjekkModell' + REPLACE(STRING(TODAY),'/','') + '_' + REPLACE(STRING(TIME,"HH:MM:SS"),':','') + '.txt'.
    OUTPUT STREAM Ut TO VALUE(cFil).
    PUT STREAM Ut 
      'ArtikkelNr    ' /* 14*/
      'Varetekst                     ' /* 30 */
      'Lev.art.nr          ' /* 20 */
      'Lev.fargekode       ' /* 20 */
      'ProfilNr ' /* 9 */
      'Pris      ' /* 10 */
      SKIP.
      
    /* Klargjør og sender eMail */
    FOR EACH ttPkSdlPris
      BREAK BY ttPkSdlPris.ArtikkelNr
            BY ttPkSdlPris.ProfilNr:

      PUT STREAM Ut 
        STRING(ttPkSdlPris.ArtikkelNr) + fill(' ',14 - LENGTH(STRING(ttPkSdlPris.ArtikkelNr))) FORMAT "x(14)"
        STRING(ttPkSdlPris.Beskr) + fill(' ',30 - LENGTH(STRING(ttPkSdlPris.Beskr))) FORMAT "x(30)"
        STRING(ttPkSdlPris.LevKod) + fill(' ',20 - LENGTH(STRING(ttPkSdlPris.LevKod))) FORMAT "x(20)"
        STRING(ttPkSdlPris.LevFargKod) + fill(' ',20 - LENGTH(STRING(ttPkSdlPris.LevFargKod))) FORMAT "x(20)"
        STRING(ttPkSdlPris.ProfilNr) + fill(' ',9 - LENGTH(STRING(ttPkSdlPris.ProfilNr))) FORMAT "x(9)"
        STRING(ttPkSdlPris.Pris) + fill(' ',10 - LENGTH(STRING(ttPkSdlPris.Pris))) FORMAT "x(10)"
        SKIP.
    END.
    OUTPUT STREAM Ut CLOSE.
    RUN SendEMail (cFil, Butiker.ProfilNr).
  END.
  
  IF bTest THEN 
    TEMP-TABLE ttPkSdlPris:WRITE-JSON('file', 'konv\ttPkSdlPrisModell' + REPLACE(STRING(TODAY),'/','') + '_' + REPLACE(STRING(TIME,"HH:MM:SS"),':','') + '.json', TRUE).
  
  IF bTest THEN 
    rStandardFunksjoner:SkrivTilLogg(cLogg,
        '        Ferdig byggTempTable for pakkseddel (PkSdlId) ' + STRING(lPkSdlId) + '..' 
        ).

END PROCEDURE.

PROCEDURE oppdaterPris:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  
  IF NOT VALID-HANDLE(hPrisKo) THEN RUN prisko.p PERSISTENT SET hPrisKo.
  RUN settLoggNavn IN hPrisKo (cLogg).
  
  IF bTest THEN 
    rStandardFunksjoner:SkrivTilLogg(cLogg,
        '        oppdaterPris for pakkseddel (PkSdlId) ' + STRING(lPkSdlId) + '. Handle: ' + STRING(VALID-HANDLE(hPrisKo)) + '.' 
        ).

  PRISLOOP:
  FOR EACH ttPkSdlPris:
  
    ASSIGN
      cSkjerm = STRING(ttPkSdlPris.ValPris) + ";" +
                string(ttPkSdlPris.InnkjopsPris) + ";" +
                string(ttPkSdlPris.Rab1Kr) + ";" +
                string(ttPkSdlPris.Rab1%)  + ";" +
                string(0) + ";" +
                string(0) + ";" +
                string(0) + ";" +
                string(0) + ";" +
                string(0) + ";" +
                string(0) + ";" +
                string(0) + ";" +
                string(0) + ";" +
                string(ttPkSdlPris.Varekost) + ";" +
                string(ttPkSdlPris.MvaKr)      + ";" +
                string(ttPkSdlPris.Mva%)       + ";" +
                string(ttPkSdlPris.DBKr)       + ";" +
                string(ttPkSdlPris.DB%)        + ";" +
                string(ttPkSdlPris.Pris)       + ";" +
                string(ttPkSdlPris.EuroPris)   + ";" +
                string(ttPkSdlPris.EuroManuel) + ";" + /* 20 */
                string(TODAY) + ";" + /* 21 Aktiv fra */
                "0" + ";" + /* 22 */
                ""  + ";" + /* 23 */
                "0" + ";" + /* 24 */
                ""  + ";" + /* 25 */
                "0" + ";" + /* 26 */
                ""  + ";" + /* 27 */
                "no".

    IF bTest THEN 
      rStandardFunksjoner:SkrivTilLogg(cLogg,
          ' '  
          ).
      rStandardFunksjoner:SkrivTilLogg(cLogg,
          '            artikkel/profil/pris: ' + STRING(ttPkSdlPris.ArtikkelNr) + '/' + STRING(ttPkSdlPris.ProfilNr) + '/' + STRING(ttPkSdlPris.Pris) + '.'  
          ).
    
    RUN LagreArtPris IN hPrisKo
      (INPUT ttPkSdlPris.ArtBasRecid,
       INPUT ttPkSdlPris.ProfilNr,
       INPUT-OUTPUT cSkjerm,
       INPUT FALSE,  /* wTilbud = false - Dvs ordinær kaflkyle.         */
       INPUT TRUE,   /* Direkte oppdatering av prisene som er kalkulert */
       INPUT 1,
       ?).

/*FIND ArtPris NO-LOCK WHERE                                                                                         */
/*  ArtPris.ArtikkelNr = ttPkSdlPris.ArtikkelNr AND                                                                  */
/*  ArtPris.ProFilNr = ttPkSdlPris.ProfilNr NO-ERROR.                                                                */
/*MESSAGE ArtPris.ArtikkelNr ArtPris.ProFilNr ArtPris.Pris[1] ArtPris.Pris[2] ArtPris.VareKost[1] ArtPris.VareKost[2]*/
/*VIEW-AS ALERT-BOX.                                                                                                 */

  END. /* PRISLOOP */
  
  IF VALID-HANDLE(hPrisko) THEN 
    DELETE PROCEDURE hPrisko.

  IF bTest THEN 
    rStandardFunksjoner:SkrivTilLogg(cLogg,
        '        Ferdig oppdaterPris for pakkseddel (PkSdlId) ' + STRING(lPkSdlId) + '. Handle: ' + STRING(VALID-HANDLE(hPrisKo)) + '.' 
        ).

    
END PROCEDURE.

PROCEDURE sendEMail:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER icFil AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER piProfilNr AS INTEGER NO-UNDO.

DEFINE BUFFER pbufPrisprofil FOR Prisprofil.

IF cparToADDRESS = '' THEN 
DO:
  rStandardFunksjoner:SkrivTilLogg (cLogg,'      Adresseliste for mottager av prissjekk Syspara: 50 50 36, er ikke satt opp.').
  RETURN.
END.

FIND pbufPrisprofil NO-LOCK WHERE 
    pbufPrisProfil.ProfilNr = piProfilNr NO-ERROR.

FILE-INFO:FILE-NAME = icFil.

rSendEMail:parToADDRESS = cparToADDRESS.
rSendEMail:parMailType = 'PRISMODELL'.
rSendEMail:parSUBJECT  = 'Prisendringer på modell for prisprofil: ' + STRING(piProfilNr) + ' ' + pbufPrisProfil.Beskrivelse + ' (Dato/Tid: ' + STRING(NOW,"99/99/9999 HH:MM:SS") + ').'.
rSendEMail:parMESSAGE  = "Loggfil: " + icFil + '.' + CHR(10) + 
                         "Det har her skjedd endringer på en eller flere farger i en modell, or resten av modellen har fått synkronisert prisen.".
rSendEMail:parFILE     = FILE-INFO:FULL-PATHNAME.  

rStandardFunksjoner:SkrivTilLogg (cLogg,'    eMail info:').
rStandardFunksjoner:SkrivTilLogg (cLogg,'        ' + rSendEMail:parMailType).
rStandardFunksjoner:SkrivTilLogg (cLogg,'        ' + rSendEMail:parSUBJECT).
rStandardFunksjoner:SkrivTilLogg (cLogg,'        ' + STRING(rSendEMail:parMESSAGE)).
rStandardFunksjoner:SkrivTilLogg (cLogg,'        ' + rSendEMail:parFILE).

bOk = rSendEMail:send( ).

rStandardFunksjoner:SkrivTilLogg (cLogg,'    eMail sende resultat: ' + STRING(bOk)).
                    
IF ERROR-STATUS:ERROR THEN 
    DO:
        rStandardFunksjoner:SkrivTilLogg (cLogg,'    **FEIL. eMail ikke sendt. Vedlegg ' + FILE-INFO:FULL-PATHNAME + '.').
        DO ix = 1 TO ERROR-STATUS:NUM-MESSAGES:
            rStandardFunksjoner:SkrivTilLogg (cLogg, '          ' 
                + STRING(ERROR-STATUS:GET-NUMBER(ix)) + ' ' + ERROR-STATUS:GET-MESSAGE(ix)    
                ).
        END.            
    END.



END PROCEDURE.

