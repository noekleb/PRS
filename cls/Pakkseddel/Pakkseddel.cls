 
 /*------------------------------------------------------------------------
    File        : Pakkseddel
    Purpose     : Lage en samling med funksjoner som kan kjøres mot en pakkseddel for å oppdatere/korrigere o.l.
    Syntax      : 
    Description : Klassen skal inneholde metoder som gjør noe med enkel pakkseddel.
    Author(s)   : tomn
    Created     : Fri Aug 21 11:34:13 CEST 2020
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.


CLASS cls.Pakkseddel.Pakkseddel: 
  /*------------------------------------------------------------------------------
   Purpose: Standard Constructor. 
   Notes:   Tar ikke imot loggfilnavn.
  ------------------------------------------------------------------------------*/
  DEFINE VARIABLE cLogg AS CHARACTER INITIAL 'Pakkseddel' NO-UNDO.
  DEFINE VARIABLE bTest AS LOG NO-UNDO.
  DEFINE VARIABLE iGantAktiv         AS INTEGER   NO-UNDO. 
  
  DEFINE VARIABLE rStandardFunksjoner AS cls.StdFunk.StandardFunksjoner NO-UNDO. 
  DEFINE VARIABLE rArtPrisKalkyle AS cls.Artikkel.ArtPrisKalkyle NO-UNDO. 
  
  DEF TEMP-TABLE EtikettLogg NO-UNDO
    FIELD Vg        LIKE ArtBas.Vg
    FIELD LopNr     LIKE ArtBas.LopNr
    FIELD Ant       AS INTEGER FORMAT "->>>>>>>9"
    FIELD Storl     AS CHAR
    FIELD bongtekst AS CHAR
    FIELD pris      AS DEC FORMAT "-zzz,zz9.99"
    FIELD pris2     AS DEC FORMAT "-zzz,zz9.99"
    FIELD individ   AS DEC DECIMALS 0
    FIELD butik     AS INT
    FIELD SeqNr     AS INT
    .
  
  CONSTRUCTOR PUBLIC Pakkseddel (  ):
    SUPER ().
    {syspara.i 210 100 8 iGantAktiv INT}
    
    ASSIGN 
      cLogg = 'Pakkseddel' + REPLACE(STRING(TODAY),'/','')
      bTest = TRUE 
      .
    rStandardFunksjoner = NEW cls.StdFunk.StandardFunksjoner() NO-ERROR.
    rArtPrisKalkyle = NEW cls.Artikkel.ArtPrisKalkyle( ) NO-ERROR.
    
  END CONSTRUCTOR.

  /*------------------------------------------------------------------------------
   Purpose: Standard Constructor.
   Notes:   Tar imot loggfilnavn.
  ------------------------------------------------------------------------------*/
    
  CONSTRUCTOR PUBLIC Pakkseddel ( INPUT pcLogg AS CHARACTER ):
    SUPER ().
    
    IF pcLogg = '' THEN 
      ASSIGN 
        cLogg = 'Pakkseddel' + REPLACE(STRING(TODAY),'/','')
        bTest = TRUE 
        .
    ELSE 
      ASSIGN 
        cLogg = pcLogg
        .
    rStandardFunksjoner  = NEW cls.StdFunk.StandardFunksjoner() NO-ERROR.
    
  END CONSTRUCTOR.

  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC DECIMAL setFakturaNrIPkSdlHode( INPUT plPkSdlId AS DECIMAL  ):
    
    DEFINE VARIABLE pbResult AS DECIMAL NO-UNDO.
    DEFINE VARIABLE piAntLev AS INTEGER NO-UNDO.
    DEFINE VARIABLE piAntFakt AS INTEGER NO-UNDO.
    
    DEFINE BUFFER bufPkSdlHode FOR PkSdlHode.

    SETTFAKTURANR:
    DO FOR bufPkSdlHode TRANSACTION:
      FIND bufPkSdlHode EXCLUSIVE-LOCK WHERE 
        bufPkSdlHode.PkSdlId = plPkSdlId NO-ERROR.
        
      IF NOT AVAILABLE bufPkSdlHode THEN 
      DO:
        IF bTest THEN         
          rStandardFunksjoner:SkrivTilLogg(cLogg,
              'UKJENT PksdlId/PkSdlNr: ' + 
              STRING(bufPkSdlHode.PkSdlId) + '/' + 
              bufPkSdlHode.PkSdlNr  + '.' 
              ).    
        LEAVE SETTFAKTURANR.
      END.
      
      /* Vi skal bare ha pakkseddler hvor fakturanr ikke er satt. */
      IF NOT bufPkSdlHode.FakturaNr = ? THEN 
      DO:
        IF bTest THEN         
          rStandardFunksjoner:SkrivTilLogg(cLogg,
              'OK PksdlId/PkSdlNr/FakturaNr: ' + 
              STRING(bufPkSdlHode.PkSdlId) + '/' + 
              bufPkSdlHode.PkSdlNr + '/' + 
              STRING(bufPkSdlHode.FakturaNr) + '.' 
              ).    
        LEAVE SETTFAKTURANR.
      END.
      ELSE DO:
        IF bTest THEN         
          rStandardFunksjoner:SkrivTilLogg(cLogg,
              'FUNNET PksdlId/PkSdlNr/FakturaNr: ' + 
              STRING(bufPkSdlHode.PkSdlId) + '/' + 
              bufPkSdlHode.PkSdlNr + '/' + 
              IF bufPkSdlHode.FakturaNr = ? THEN '?' ELSE STRING(bufPkSdlHode.FakturaNr) + '.' 
              ).    
      END.
      FIND Butiker NO-LOCK WHERE 
        Butiker.Butik = bufPkSdlHode.ButikkNr NO-ERROR.
      IF NOT AVAILABLE Butiker THEN 
        LEAVE SETTFAKTURANR.
        
      IF AVAILABLE FakturaHode THEN 
        RELEASE FakturaHode.
      
      FIND LAST FakturaHode NO-LOCK WHERE 
          FakturaHode.PkSdlNr       = bufPkSdlHode.PkSdlNr AND 
          FakturaHode.KundeNr       = Butiker.KundeNr NO-ERROR.
    
      piAntLev = 0.
      FOR EACH PkSdlLinje OF bufPkSdlHode NO-LOCK:
        piAntLev = piAntLev + PkSdlLinje.AntLevert.
      END.
      
      piAntFakt = 0.
      IF AVAILABLE FakturaHode THEN
      DO:
        FOR EACH FakturaLinje OF FakturaHode NO-LOCK:
          piAntFakt = piAntFakt + FakturaLinje.Antall.
        END.
      END.
      
      IF AVAILABLE FakturaHode /*AND 
        bufPkSdlHode.FakturaNr = ? AND 
        piAntLev = piAntFakt*/ THEN
      DO:
        ASSIGN 
          bufPkSdlHode.FakturaNr = FakturaHode.FakturaNr
          pbResult               = bufPkSdlHode.FakturaNr 
          .
        rStandardFunksjoner:SkrivTilLogg(cLogg,
            'Korrigert fakturanr PksdlId/PkSdlNr/FakturaNr/AntLev/AntFakt: ' + 
            STRING(bufPkSdlHode.PkSdlId) + '/' + 
            bufPkSdlHode.PkSdlNr + '/' + 
            STRING(bufPkSdlHode.FakturaNr) + '/' +
            STRING(piantLev)  + '/' +
            STRING(piantFakt) 
            + '.' 
            ).    
      END.

      IF AVAILABLE bufPkSdlHode THEN 
        RELEASE bufPkSdlHode.         
    END. /* SETTFAKTURANR TRANSACTION */

    RETURN pbResult.

  END METHOD.  

  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC LOGICAL EtikettUtskrift( INPUT plPkSdlId AS DECIMAL, piButNr AS INTEGER  ):
    
    DEFINE VARIABLE bResult AS LOGICAL NO-UNDO.
    DEFINE VARIABLE pcKommisjonsButLst AS CHARACTER NO-UNDO.
    DEFINE VARIABLE piSeq AS INTEGER NO-UNDO.

    /* Outlet butikker */
    IF iGantAktiv = 1 THEN 
    FOR EACH Butiker NO-LOCK WHERE 
      Butiker.Butik >= 10000 AND
      Butiker.Butik <= 10999:
      pcKommisjonsButLst = pcKommisjonsButLst + 
                          (IF pcKommisjonsButLst = '' THEN '' ELSE ',') + 
                          STRING(Butiker.Butik). 
    END.
    
    FIND PkSdlHode EXCLUSIVE-LOCK WHERE 
      PkSdlHode.PkSdlId = plPkSdlId NO-ERROR.
    IF NOT AVAILABLE PkSdlHode THEN 
    DO:
      IF bTest THEN         
        rStandardFunksjoner:SkrivTilLogg(cLogg,
            'EtikettUtskrift: UKJENT PksdlId/PkSdlNr: ' + 
            STRING(PkSdlHode.PkSdlId) + '/' + 
            PkSdlHode.PkSdlNr  + '.' 
            ).    
      bResult = FALSE.
      RETURN bResult.
    END.
    FIND Butiker NO-LOCK WHERE 
      Butiker.butik = piButNr NO-ERROR.
    IF NOT AVAILABLE Butiker THEN 
    DO:
      IF bTest THEN         
        rStandardFunksjoner:SkrivTilLogg(cLogg,
            'EtikettUtskrift: UKJENT butikk: ' + 
            STRING(piButNR) + '.' 
            ).    
      bResult = FALSE.
      RETURN bResult.
    END.
          
    IF iGantAktiv = 1 AND 
      CAN-DO(pcKommisjonsButLst,STRING(PkSdlHode.ButikkNr)) THEN
    DO: /* Oppretter SLUTT etikett (Den skrives ut først). */
      piSeq = piSeq + 1.
      CREATE EtikettLogg.
      ASSIGN
        EtikettLogg.Butik     = piButNr 
        EtikettLogg.Vg        = 0
        EtikettLogg.LopNr     = 0
        EtikettLogg.Ant       = 0
        EtikettLogg.Storl     = "PKSDL"
        EtikettLogg.Bongtekst = 'KOMMISJONSVARER' + CHR(1) +
                                PkSdlHode.PkSdlNr + CHR(1) +
                                (IF AVAILABLE Butiker THEN Butiker.butnamn ELSE "") + CHR(1) +
                                'SLUTT'
        EtikettLogg.Pris      = 0
        EtikettLogg.Pris2     = 0
        EtikettLogg.SeqNr     = piSeq
        .
    END.

    /* En etikettlogg post pr. varelinje. */
    LINJEBLOKK:
    FOR EACH PkSdlLinje OF PkSdlHode NO-LOCK WHERE  
      PkSdlLinje.AntLevert > 0:
      FIND ArtBas NO-LOCK WHERE
        ArtBas.ArtikkelNr = PkSdlLinje.ArtikkelNr NO-ERROR.
      IF NOT AVAILABLE ArtBas THEN 
        NEXT LINJEBLOKK.

      IF AVAILABLE ArtBas THEN
      DO: 
        FIND ArtPris NO-LOCK WHERE
          ArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND
          ArtPris.ProfilNr   = Butiker.ProfilNr NO-ERROR.
        IF NOT AVAILABLE ArtPris THEN 
          FIND FIRST ArtPris OF ArtBas NO-ERROR.
      END.
      ELSE 
        IF AVAILABLE ArtPris THEN RELEASE ArtPris.
        
      FIND PkSdlPris NO-LOCK WHERE
        PkSdlPris.PkSdlId = PkSdlLinje.PkSdlId AND 
        PkSdlPris.ArtikkelNr = PkSdlLinje.ArtikkelNr NO-ERROR.

      ASSIGN 
        piSeq = piSeq + 1.
      CREATE EtikettLogg.
      ASSIGN 
        EtikettLogg.Butik     = PkSdlLinje.ButikkNr
        EtikettLogg.Vg        = (IF AVAILABLE ArtBas THEN ArtBas.Vg ELSE 0)
        EtikettLogg.LopNr     = (IF AVAILABLE ArtBas THEN ArtBas.LopNr ELSE 0)
        EtikettLogg.LopNr     = (IF EtikettLogg.LopNr = ? THEN 0 ELSE EtikettLogg.LopNr)
        EtikettLogg.Ant       = PkSdlLinje.AntLevert
        EtikettLogg.Storl     = PkSdlLinje.Kode
        EtikettLogg.Bongtekst = PkSdlLinje.Beskr
        EtikettLogg.Pris      = PksdlPris.NyPris
        EtikettLogg.Pris2     = (IF AVAILABLE ArtBas THEN ArtBas.AnbefaltPris ELSE 0)
        EtikettLogg.SeqNr     = piSeq
        .         
    END. /* LINJEBLOKK*/

    IF CAN-FIND(FIRST EtikettLogg) THEN
    DO:
/*      TEMP-TABLE EtikettLogg:WRITE-JSON('file', 'konv\EtikettLogg' +                 */
/*                                            REPLACE(STRING(TODAY),'/','') +          */
/*                                            '_' +                                    */
/*                                            REPLACE(STRING(TIME,"HH:MM:SS"),':','') +*/
/*                                            '.json', TRUE).                          */
        
        
      RUN asEtikett.p (piButNr,2,INPUT TABLE EtikettLogg,OUTPUT bResult).
    END.

    RETURN bResult.

  END METHOD.  

  /*------------------------------------------------------------------------------
   Purpose: Oppdaterer aktiv pris på alle varer som ligger i pakkseddelen.
            Her benyttes 
   Notes:
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC VOID prisOppdatering(  INPUT plPkSdlId AS DECIMAL ):
    DEFINE BUFFER bufArtPris FOR ArtPris.
    
    FIND PkSdlHode NO-LOCK WHERE 
      PkSdlHode.PkSdlId = plPkSdlId NO-ERROR.
    IF NOT AVAILABLE PkSdlHode THEN 
      RETURN.
    FIND Butiker NO-LOCK WHERE 
      Butiker.Butik = PkSdlHode.ButikkNr NO-ERROR.
    IF NOT AVAILABLE Butiker THEN 
      RETURN. 

    PRISLOOP:
    FOR EACH PkSdlPris OF PkSdlHode NO-LOCK TRANSACTION:
      
      /* Sikrer at artikkelen har en pris på egen prisprofil. */
      IF NOT CAN-FIND(ArtPris WHERE 
                      ArtPris.ArtikkelNr = PkSdlPris.ArtikkelNr AND 
                      ArtPris.ProfilNr   = Butiker.ProfilNr) THEN 
          rArtPrisKalkyle:NyArtPris( INPUT PkSdlPris.ArtikkelNr,
                                     INPUT 1,
                                     INPUT Butiker.ProfilNr ).
                                     
      FIND bufArtPris NO-LOCK WHERE
        bufArtPris.ArtikkelNr = PkSdlPris.ArtikkelNr AND 
        bufArtPris.ProfilNr   = Butiker.ProfilNr NO-ERROR.

      IF AVAILABLE bufArtPris THEN 
        PRISBLOKK:
        DO: 
          FIND CURRENT bufArtPris EXCLUSIVE-LOCK.
          ASSIGN 
            bufArtPris.InnkjopsPris[1] = PkSdlPris.NyInnkjopsPris 
            bufArtPris.Rab1%[1]        = PkSdlPris.NyRab1%
            bufArtPris.Pris[1]         = PkSdlPris.NyPris
            .
          FIND CURRENT bufArtPris NO-LOCK. 
          /* Regner om og setter rabattene. */
          rArtPrisKalkyle:SettRabatt( INPUT bufArtPris.ArtikkelNr,
                               INPUT bufArtPris.ProfilNr,
                               INPUT PkSdlHode.ButikkNr,
                               PkSdlPris.NyRab1% ).
          rArtPrisKalkyle:settPrishistorikk( INPUT bufArtPris.ArtikkelNr,
                                      INPUT bufArtPris.ProfilNr,
                                      INPUT 1 /* Normalpris */ ).
        END. /* PRISBLOKK */
    END. /* PRISLOOP TRANSACTION */  
    
    RETURN.

  END METHOD.

  /*------------------------------------------------------------------------------
   Purpose: Oppdaterer pakkseddelen pris med det som står på aktikkelens kalkyle.
   Notes:   Dette er for å få satt LC pris som innkjøpspris på pakkseddlene.
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC VOID pksdlKommisjonOppdatering( INPUT plPkSdlId AS DECIMAL ):
    
  DEF VAR fMvaKr AS DEC NO-UNDO.
  DEF VAR fNyDbKr  AS DEC NO-UNDO.
  DEF VAR fNyPris AS DEC NO-UNDO.
  DEF VAR fNyVareKost AS DEC NO-UNDO.
  DEF VAR fNyRab% AS DEC NO-UNDO.
  DEF VAR fNyDb% AS DEC NO-UNDO.
  DEF VAR fInnkjopsPris AS DEC NO-UNDO.
  DEF VAR fPris AS DEC NO-UNDO.
      
    DEFINE BUFFER bufArtPris FOR ArtPris.
    
    FIND PkSdlHode NO-LOCK WHERE 
      PkSdlHode.PkSdlId = plPkSdlId NO-ERROR.
    IF NOT AVAILABLE PkSdlHode THEN 
      RETURN.
    FIND Butiker NO-LOCK WHERE 
      Butiker.Butik = PkSdlHode.ButikkNr NO-ERROR.
    IF NOT AVAILABLE Butiker THEN 
      RETURN. 

    PRISLOOP:
    FOR EACH PkSdlPris OF PkSdlHode EXCLUSIVE-LOCK TRANSACTION:
      
      FIND ArtPris NO-LOCK WHERE
        ArtPris.ArtikkelNr = PkSdlPris.ArtikkelNr AND 
        ArtPris.ProfilNr   = Butiker.ProfilNr NO-ERROR.



      IF AVAILABLE ArtPris THEN 
        PRISBLOKK:
        DO: 

          ASSIGN
            fNyRab%       = 0
            fInnkjopsPris = ArtPris.InnkjopsPris[1]
            fNyVareKost   = ArtPris.InnkjopsPris[1]
            fNyPris       = ArtPris.Pris[1]
            fMvaKr        = fNyPris - ROUND((fNyPris / (1 + (25 / 100))),2)
            fNyDbKr       = fNyPris - fMvaKr - fNyVareKost
            fNyDb%        = ROUND((fNyDbKr * 100) / (fNyPris - fMvaKr),2) 
            fNyDb%        = IF fNyDb% = ? THEN 0 ELSE fNyDb%
            .


          ASSIGN 
            PkSdlPris.NyInnkjopsPris = ArtPris.InnkjopsPris[1]
            PkSdlPris.NyVarekost     = ArtPris.InnkjopsPris[1]   
            PkSdlPris.NyDb%          = fNyDb%     
            PkSdlPris.NyFrakt        = 0 
            PkSdlPris.NyRab1%        = 0    
            .
        END. /* PRISBLOKK */
    END. /* PRISLOOP TRANSACTION */  
    
    RETURN.

  END METHOD.

  DESTRUCTOR PUBLIC Pakkseddel ( ):

  END DESTRUCTOR.

END CLASS.