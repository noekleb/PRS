/* pksdl_Artikkelliste.p
   Parameter:  
   Opprettet:   
     
DEFINE TEMP-TABLE ttArtikkelListe NO-UNDO
  FIELD PkSdlNr AS CHARACTER FORMAT "x(15)"
  FIELD PkSdlOpphav AS INTEGER FORMAT ">>9"
  FIELD ArtikkelNr AS DECIMAL FORMAT ">>>>>>>>>>>>>9"  
  FIELD SO AS INTEGER FORMAT ">9"
  FIELD Varetekst AS CHARACTER FORMAT "x(40)"
  FIELD LevKod AS CHARACTER FORMAT "x(20)"
  FIELD LevFargKod AS CHARACTER FORMAT "x(20)"
  FIELD Storl AS CHARACTER FORMAT "x(15)"
  FIELD Sesong AS INTEGER FORMAT ">>>>>>>9"
  FIELD MainGroup AS INTEGER FORMAT ">>>>>>>9"
  FIELD MainGrpTekst AS CHARACTER FORMAT "x(30)"
  FIELD ArtGroup AS INTEGER FORMAT ">>>>>>>9"
  FIELD ArtGrpTekst AS CHARACTER FORMAT "x(30)"
  FIELD LC AS DECIMAL FORMAT "->>>>>>>>>9.99"
  FIELD InnkjopsPris AS DECIMAL FORMAT "->>>>>>>>>9.99"
  FIELD AntPkSdl AS INTEGER FORMAT "->>>>>>>9"
  FIELD VerdiLC AS DECIMAL FORMAT "->>>>>>>>>9.99"
  FIELD TotAnt AS INTEGER  FORMAT "->>>>>>>9"
  FIELD TotLCVerdi AS DECIMAL FORMAT "->>>>>>>>>9.99"
  INDEX idxArtikkelNr AS UNIQUE PRIMARY ArtikkelNr Storl PksdlNr SO
  INDEX idxGant LevKod LevFargKod Storl PkSdlNr SO
                 
-----------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER icParam     AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ihBuffer    AS HANDLE NO-UNDO.
DEFINE INPUT  PARAMETER icSessionId AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER ocReturn    AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER obOK        AS LOG NO-UNDO.

DEFINE VARIABLE cLagerListe AS CHARACTER NO-UNDO.
DEFINE VARIABLE hQuery      AS HANDLE    NO-UNDO.
DEFINE VARIABLE cLevKod AS CHARACTER NO-UNDO.
DEFINE VARIABLE cLevFargKod AS CHARACTER NO-UNDO.
DEFINE VARIABLE cStorl AS CHARACTER NO-UNDO.
DEFINE VARIABLE cLogg AS CHARACTER NO-UNDO.
DEFINE VARIABLE cAnv-KodIdList AS CHARACTER NO-UNDO.
DEFINE VARIABLE cKategoriIdList AS CHARACTER NO-UNDO.
DEFINE VARIABLE cButikkIdList AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTekst AS CHARACTER NO-UNDO.

/* Standard funksjoner for logging */
DEFINE VARIABLE rStandardFunksjoner AS cls.StdFunk.StandardFunksjoner NO-UNDO.

{ ttArtikkelliste.i }

rStandardFunksjoner  = NEW cls.StdFunk.StandardFunksjoner( cLogg ) NO-ERROR.

ASSIGN 
  cLogg = 'pksdl_Artikkelliste' + REPLACE(STRING(TODAY),'/','')
  .

rStandardFunksjoner:SkrivTilLogg(cLogg,
    'Start.' 
    ).
rStandardFunksjoner:SkrivTilLogg(cLogg,
    '  Parametre:'
    ).
rStandardFunksjoner:SkrivTilLogg(cLogg,
    '  icParam:' + icParam
    ).
        
ASSIGN 
  ocReturn = ""
  cTekst      = ENTRY(1,icParam,'@')
  cLagerListe = ENTRY(1,cTekst,'|')
  cLevKod     = REPLACE('*' + ENTRY(2,cTekst,'|') + '*','**','')
  cLevFargKod = REPLACE('*' + ENTRY(3,cTekst,'|') + '*','**','')
  cStorl      = REPLACE('*' + ENTRY(4,cTekst,'|') + '*','**','')
  cAnv-KodIdList  = REPLACE(ENTRY(2,icParam,'@'),'|',',')
  cKategoriIdList = REPLACE(ENTRY(3,icParam,'@'),'|',',')
  cButikkIdList   = REPLACE(ENTRY(4,icParam,'@'),'|',',')
  
  .

rStandardFunksjoner:SkrivTilLogg(cLogg,
    '    cLagerliste: ' + cLagerListe 
    ).
rStandardFunksjoner:SkrivTilLogg(cLogg,
    '    cLevKod: ' + cLevKod
    ).
rStandardFunksjoner:SkrivTilLogg(cLogg,
    '    cLevFargKod: ' + cLevFargKod
    ).
rStandardFunksjoner:SkrivTilLogg(cLogg,
    '    cStorl: ' + cStorl
    ).
rStandardFunksjoner:SkrivTilLogg(cLogg,
    '    cAnv-KodIdList: ' + cAnv-KodIdList
    ).
rStandardFunksjoner:SkrivTilLogg(cLogg,
    '    cKategoriIdList: ' + cKategoriIdList
    ).
rStandardFunksjoner:SkrivTilLogg(cLogg,
    '  cButikkIdList:' + cButikkIdList
    ). 

CREATE QUERY hQuery.
hQuery:SET-BUFFERS(ihBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + ihBuffer:NAME + " WHERE TRUE").
hQuery:QUERY-OPEN().

hQuery:GET-FIRST().

BLOKKEN:
REPEAT WHILE NOT hQuery:QUERY-OFF-END TRANSACTION:
  FIND PkSdlHode NO-LOCK WHERE
    PkSdlHode.PkSdlId = DECIMAL(ihBuffer:BUFFER-FIELD("PkSdlId"):BUFFER-VALUE) NO-ERROR.
    
  IF AVAILABLE PkSdlHode THEN 
  DO:
    FIND FIRST PkSdlMottak NO-LOCK WHERE 
        PkSdlMottak.PkSdlId = PkSdlHode.PkSdlId NO-ERROR.

    FOR EACH PkSdlLinje OF PkSdlHode NO-LOCK,
      FIRST PkSdlPris OF PkSdlLinje NO-LOCK:
      FIND StrKonv OF PkSdlLinje NO-ERROR.
      IF AVAILABLE StrKonv AND cStorl <> '' THEN
      DO:
        IF NOT StrKonv.Storl MATCHES cStorl THEN
          NEXT.
      END.
      IF NOT AVAILABLE StrKonv THEN 
      DO:
        rStandardFunksjoner:SkrivTilLogg(cLogg,
            '  Manglende strkonv: ' + STRING(PkSdlLinje.ArtikkelNr) + ' ' + STRING(PkSdlLinje.StrKode)
            ).
        NEXT.
      END.
      FIND FIRST ttArtikkelListe WHERE
        ttArtikkelListe.ArtikkelNr = PkSdlLinje.ArtikkelNr AND
        ttArtikkelListe.Storl      = StrKonv.Storl AND 
        ttArtikkelListe.PkSdlNr    = PkSdlHode.PkSdlNr AND
        ttArtikkelListe.SO         = PkSdlHode.SendtOutlet NO-ERROR.
      IF NOT AVAILABLE ttArtikkelListe THEN 
      DO:
        FIND ArtBas NO-LOCK WHERE 
          ArtBas.ArtikkelNr = PkSdlLinje.ArtikkelNr NO-ERROR.
        IF NOT AVAILABLE ArtBas THEN 
          NEXT.
        IF cAnv-KodIdList <> '' THEN 
        DO:
          IF NOT CAN-DO(cAnv-KodIdList,STRING(ArtBas.Anv-Id)) THEN 
            NEXT.
        END.      
        IF cKategoriIdList <> '' THEN 
        DO:
          IF NOT CAN-DO(cKategoriIdList,STRING(ArtBas.HovedKatNr)) THEN 
            NEXT.
        END.      
        IF cButikkIdList <> '' THEN 
        DO:
          IF NOT CAN-DO(cButikkIdList,STRING(ihBuffer:BUFFER-FIELD("pksdl_FraButikk"):BUFFER-VALUE)) THEN 
            NEXT.
        END.      
        IF cLevKod <> '' THEN
        DO:
          IF NOT ArtBas.LevKod MATCHES cLevKod THEN
          DO:
            rStandardFunksjoner:SkrivTilLogg(cLogg,
                '    Missmatch: ' + ArtBas.LevKod + ' MATCHES ' + cLevKod
                ).
            NEXT.
          END.
        END.
        IF cLevFargKod <> '' THEN
        DO:
          IF NOT ArtBas.LevFargKod MATCHES cLevFargKod THEN
            NEXT.
        END.
        FIND Hovedkategori NO-LOCK WHERE 
          HovedKategori.HovedKatNr = ArtBas.HovedKatNr NO-ERROR.
        FIND Anv-Kod NO-LOCK WHERE
          Anv-Kod.Anv-Id = ArtBas.Anv-Id NO-ERROR.      
        FIND ArtPris NO-LOCK WHERE 
          ArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND 
          ArtPris.ProfilNr   = 2 NO-ERROR.
        IF NOT AVAILABLE ArtPris THEN 
          FIND FIRST ArtPris NO-LOCK WHERE 
            ArtPris.ArtikkelNr = ArtBas.ArtikkelNr NO-ERROR.
        CREATE ttArtikkelListe.
        ASSIGN 
          ttArtikkelListe.ArtikkelNr = PkSdlLinje.ArtikkelNr
          ttArtikkelListe.Storl      = StrKonv.Storl
          ttArtikkelListe.PkSdlNr    = PkSdlHode.PkSdlNr
          ttArtikkelListe.cPalleNr   = PkSdlHode.cPalleNr
          ttArtikkelListe.Lokasjon   = PkSdlHode.Lokasjon
          ttArtikkelListe.Varetype   = PkSdlHode.VareType
          ttArtikkelListe.LagerSesong = PkSdlHode.LagerSesong
          ttArtikkelListe.SO         = PkSdlHode.SendtOutlet 
          ttArtikkelListe.PkSdlOpphav = PkSdlHode.PkSdlOpphav
          ttArtikkelListe.VareTekst  = ArtBas.Beskr
          ttArtikkelListe.LevKod     = ArtBas.LevKod
          ttArtikkelListe.LevFargKod = ArtBas.LevFargKod
          ttArtikkelListe.MainGroup  = ArtBas.HovedKatNr 
          ttArtikkelListe.ArtGroup   = ArtBAs.Anv-Id
          ttArtikkelListe.Sesong     = ArtBas.Sasong
          ttArtikkelListe.SendtDato  = PkSdlHode.SendtDato
          
          ttArtikkelListe.InnkjopsPris = PkSdlPris.InnkjopsPri /*(IF AVAILABLE ArtPris THEN ArtPris.InnkjopsPris[1] ELSE 0)*/  
          ttArtikkelListe.LC           = ArtBas.KjedeInnkPris
          ttArtikkelListe.WholeSalePris = PkSdlPris.InnkjopsPris
          
          ttArtikkelListe.MainGrpTekst = (IF AVAILABLE Hovedkategori THEN HovedKategori.HovedKatTekst ELSE '*Ukjent maingrp')
          ttArtikkelListe.ArtGrpTekst  = (IF AVAILABLE Anv-Kod THEN Anv-Kod.AnvBeskr ELSE '*Ukjent artgroup')
          ttArtikkelListe.Innlevert    = IF AVAILABLE PkSdlMottak THEN STRING(PkSdlMottak.MottattDato,"99/99/9999") ELSE '' 
          .

        rStandardFunksjoner:SkrivTilLogg(cLogg,
            '  Artikkel: ' + 
            STRING(PkSdlLinje.ArtikkelNr) + ' ' + 
            STRING(PkSdlLinje.StrKode) + ' ' + 
            STRING(ttArtikkelListe.InnkjopsPris) + ' ' +
            STRING(ttArtikkelListe.LC) + ' ' + 
            STRING(ttArtikkelListe.WholeSalePris) 
            ).
      END.
      
      IF AVAILABLE ttArtikkelListe THEN 
        ASSIGN
          ttArtikkelListe.AntPkSdl     = ttArtikkelListe.AntPkSdl   + PkSdlLinje.AntLevert
          ttArtikkelListe.VerdiPkSdl   = ttArtikkelListe.VerdiPkSdl + (PkSdlLinje.AntLevert * ttArtikkelListe.InnkjopsPris)
          ttArtikkelListe.VerdiLC      = ttArtikkelListe.VerdiLC    + (PkSdlLinje.AntLevert * ttArtikkelListe.LC) 
          ttArtikkelListe.VerdiWholeSale = ttArtikkelListe.VerdiWholeSale + (PkSdlLinje.AntLevert * PkSdlPris.InnkjopsPris) 
          .  
    END.
  END.

  hQuery:GET-NEXT().
END. /* BLOKKEN */

IF CAN-FIND(FIRST ttArtikkelListe) THEN 
DO:
  TEMP-TABLE ttArtikkelListe:WRITE-JSON('file',cLagerliste).
  ASSIGN 
    obOK     = TRUE
    ocReturn = SEARCH(cLagerListe)
    .
END.
ELSE DO:
  ASSIGN 
    obOK = FALSE 
    ocReturn = '** Fant ikke data.'
    .
END.

DELETE OBJECT hQuery NO-ERROR.

rStandardFunksjoner:SkrivTilLogg(cLogg,
    'Slutt.' 
    ).
