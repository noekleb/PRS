/* pksdl_Lagerliste.p
   Parameter:  
   Opprettet:   
     
DEFINE TEMP-TABLE ttLagerListe NO-UNDO
  FIELD ArtikkelNr AS DECIMAL FORMAT ">>>>>>>>>>>>>9"  
  FIELD Varetekst AS CHARACTER FORMAT "x(40)"
  FIELD LevKod AS CHARACTER FORMAT "x(20)"
  FIELD LevFargKod AS CHARACTER FORMAT "x(20)"
  FIELD MainGroup AS INTEGER FORMAT ">>>>>>>9"
  FIELD ArtGroup AS INTEGER FORMAT ">>>>>>>9"
  FIELD InnkjopsPris AS DECIMAL FORMAT "->>>>>>>>>9.99"
  FIELD AntPkSdl AS INTEGER FORMAT "->>>>>>>9"
  FIELD VerdiPkSdl AS DECIMAL FORMAT "->>>>>>>>>9.99"
  FIELD VVarekostL10 AS DECIMAL FORMAT "->>>>>>>>>9.99"
  FIELD AntL10 AS INTEGER FORMAT "->>>>>>>9"
  FIELD VerdiL10 AS DECIMAL FORMAT "->>>>>>>>>9.99"
  FIELD VVarekostL40 AS DECIMAL FORMAT "->>>>>>>>>9.99"
  FIELD AntL40 AS INTEGER FORMAT "->>>>>>>9"
  FIELD VerdiL40 AS DECIMAL FORMAT "->>>>>>>>>9.99"
  INDEX idxArtikkelNr AS UNIQUE PRIMARY ArtikkelNr
  INDEX idxGant LevKod LevFargKod
                 
-----------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER icParam     AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ihBuffer    AS HANDLE NO-UNDO.
DEFINE INPUT  PARAMETER icSessionId AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER ocReturn    AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER obOK        AS LOG NO-UNDO.

DEFINE VARIABLE cLagerListe AS CHARACTER NO-UNDO.
DEFINE VARIABLE hQuery      AS HANDLE    NO-UNDO.
DEFINE VARIABLE cLogg       AS CHARACTER NO-UNDO.
DEFINE VARIABLE cAnv-KodIdList AS CHARACTER NO-UNDO.
DEFINE VARIABLE cKategoriIdList AS CHARACTER NO-UNDO.
DEFINE VARIABLE cButikkIdList AS CHARACTER NO-UNDO.

/* Standard funksjoner for logging */
DEFINE VARIABLE rStandardFunksjoner AS cls.StdFunk.StandardFunksjoner NO-UNDO.

{ ttLagerliste.i }

ASSIGN 
  cLogg       = 'pksdl_Lagerliste' + REPLACE(STRING(TODAY),'/','')
  ocReturn = ""
  cLagerListe     = ENTRY(1,icParam,'@')
  cAnv-KodIdList  = REPLACE(ENTRY(2,icParam,'@'),'|',',')
  cKategoriIdList = REPLACE(ENTRY(3,icParam,'@'),'|',',')
  cButikkIdList   = REPLACE(ENTRY(4,icParam,'@'),'|',',')
  .

rStandardFunksjoner  = NEW cls.StdFunk.StandardFunksjoner( cLogg ) NO-ERROR.
rStandardFunksjoner:SkrivTilLogg(cLogg,
    'Start.' 
    ).
rStandardFunksjoner:SkrivTilLogg(cLogg,
    '  Parametre:'
    ).
rStandardFunksjoner:SkrivTilLogg(cLogg,
    '    cLagerliste: ' + cLagerListe 
    ).
rStandardFunksjoner:SkrivTilLogg(cLogg,
    '    cAnv-KodIdList: ' + cAnv-KodIdList 
    ).
rStandardFunksjoner:SkrivTilLogg(cLogg,
    '    cKategoriIdList: ' + cKategoriIdList 
    ).
rStandardFunksjoner:SkrivTilLogg(cLogg,
    '    cButikkIdList: ' + cButikkIdList 
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
      FIND ttLagerListe WHERE
        ttLAgerListe.ArtikkelNr = PkSdlLinje.ArtikkelNr AND
        ttLagerListe.PkSdlNr    = PkSdlHode.PkSdlNr AND
        ttLagerListe.SO         = PkSdlHode.SendtOutlet NO-ERROR.
      IF NOT AVAILABLE ttLagerListe THEN 
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
        CREATE ttLagerListe.
        ASSIGN 
          ttLagerListe.ArtikkelNr = PkSdlLinje.ArtikkelNr
          ttLagerListe.PkSdlNr    = PkSdlHode.PkSdlNr
          ttLAgerListe.SO         = PkSdlHode.SendtOutlet 
          ttLagerListe.PkSdlOpphav = PkSdlHode.PkSdlOpphav
          ttLagerListe.VareTekst  = ArtBas.Beskr
          ttLagerliste.LevKod     = ArtBas.LevKod
          ttLagerliste.LevFargKod = ArtBas.LevFargKod
          
          ttLagerListe.Sesong     = ArtBas.Sasong
          ttLagerListe.InnkjopsPris  = PkSdlPris.InnkjopsPris  
          ttLagerListe.WholeSalePris = PkSdlPris.InnkjopsPris
          ttLagerListe.LC           = ArtBas.KjedeInnkPris
          ttLagerListe.SendtDato    = PkSdlHode.SendtDato

          ttLagerListe.cPalleNr   = PkSdlHode.cPalleNr
          ttLagerListe.Lokasjon   = PkSdlHode.Lokasjon
          ttLagerListe.Varetype   = PkSdlHode.VareType
          ttLagerListe.LagerSesong = PkSdlHode.LagerSesong
          
          ttLagerListe.MainGroup    = ArtBas.Anv-Id 
          ttLagerListe.MainGrpTekst = (IF AVAILABLE Anv-Kod THEN Anv-Kod.AnvBeskr ELSE '*Ukjent artgroup')

          ttLagerListe.ArtGroup     = ArtBas.HovedKatNr
          ttLagerListe.ArtGrpTekst  = (IF AVAILABLE Hovedkategori THEN HovedKategori.HovedKatTekst ELSE '*Ukjent maingrp')
          ttLagerListe.Innlevert    = IF AVAILABLE PkSdlMottak THEN STRING(PkSdlMottak.MottattDato,"99/99/9999") ELSE '' 
          .
      END.

      IF AVAILABLE ttLagerListe THEN 
        ASSIGN
          ttLagerListe.AntPkSdl       = ttLagerListe.AntPkSdl       + PkSdlLinje.AntLevert
          ttLagerListe.VerdiPkSdl     = ttLagerListe.VerdiPkSdl     + (PkSdlLinje.AntLevert * ttLagerListe.InnkjopsPris)
          ttLagerListe.VerdiLC        = ttLagerListe.VerdiLC        + (PkSdlLinje.AntLevert * ttLagerListe.LC) 
          ttLagerListe.VerdiWholeSale = ttLagerListe.VerdiWholeSale + (PkSdlLinje.AntLevert * ttLagerListe.WholeSalePris) 
          . 
    END.
  END.

  hQuery:GET-NEXT().
END. /* BLOKKEN */

DELETE OBJECT hQuery NO-ERROR.

IF CAN-FIND(FIRST ttLagerListe) THEN 
DO:
  TEMP-TABLE ttLagerliste:WRITE-JSON('file',cLagerliste).
  ASSIGN 
    obOK = TRUE
    ocReturn = SEARCH(cLagerListe)
    .
END.
ELSE DO:
  ASSIGN 
    obOK = FALSE 
    ocReturn = '** Fant ikke data.'
    .
END.
rStandardFunksjoner:SkrivTilLogg(cLogg,
    'Slutt.' 
    ).
