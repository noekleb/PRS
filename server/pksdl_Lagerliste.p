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

{ ttLagerliste.i }

ASSIGN 
  ocReturn = ""
  cLagerListe = 'konv\ttLagerListe' + REPLACE(STRING(TODAY),'/','') + '_' + REPLACE(STRING(TIME,"HH:MM:SS"),':','') + '.json'
  .

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
    FOR EACH PkSdlLinje OF PkSdlHode NO-LOCK:
      FIND ArtBas NO-LOCK WHERE 
        ArtBas.ArtikkelNr = PkSdlLinje.ArtikkelNr NO-ERROR.
      IF NOT AVAILABLE ArtBAs THEN 
        NEXT.
      FIND Hovedkategori NO-LOCK WHERE 
        HovedKategori.HovedKatNr = ArtBAs.HovedKAtNr NO-ERROR.
      FIND Anv-Kod NO-LOCK WHERE
        Anv-Kod.Anv-Id = ArtBas.Anv-Id NO-ERROR.      
      FIND ttLagerListe WHERE 
        ttLAgerListe.ArtikkelNr = PkSdlLinje.ArtikkelNr AND 
        ttLagerListe.PkSdlNr    = PkSdlHode.PkSdlNr AND 
        ttLagerListe.SO         = PkSdlHode.SendtOutlet NO-ERROR.
      IF NOT AVAILABLE ttLagerListe THEN 
      DO:
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
          ttLagerListe.VareTekst  = ArtBas.Beskr
          ttLagerliste.LevKod     = ArtBas.LevKod
          ttLagerliste.LevFargKod = ArtBAs.LevFargKod
          ttLagerListe.MainGroup  = ArtBas.HovedKatNr 
          ttLagerListe.ArtGroup   = ArtBAs.Anv-Id
          ttLagerListe.Sesong     = ArtBas.Sasong
          ttLagerListe.LC         = ArtBas.KjedeInnkPris
          ttLagerListe.InnkjopsPris = (IF AVAILABLE ArtPris THEN ArtPris.InnkjopsPris[1] ELSE 0)  
          ttLagerListe.MainGrpTekst = (IF AVAILABLE Hovedkategori THEN HovedKategori.HovedKatTekst ELSE '')
          ttLagerListe.ArtGrpTekst  = (IF AVAILABLE Anv-Kod THEN Anv-Kod.AnvBeskr ELSE '')
          .
      END.
      IF AVAILABLE ttLagerListe THEN 
        ASSIGN
          ttLagerListe.AntPkSdl     = ttLagerListe.AntPkSdl   + PkSdlLinje.AntLevert
          ttLagerListe.VerdiPkSdl   = ttLagerListe.VerdiPkSdl + (PkSdlLinje.AntLevert * ttLagerListe.InnkjopsPris)
          ttLagerListe.VerdiLC      = ttLagerListe.VerdiLC    + (PkSdlLinje.AntLevert * ArtBas.KjedeInnkPris) 
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
