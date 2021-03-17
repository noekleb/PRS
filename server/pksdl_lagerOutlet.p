/* pksdl_lagerOutlet.p

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
  FIELD TotAnt AS INTEGER  FORMAT "->>>>>>>9"
  FIELD TotVerdi AS DECIMAL FORMAT "->>>>>>>>>9.99"
  INDEX idxArtikkelNr AS UNIQUE PRIMARY ArtikkelNr
  INDEX idxGant LevKod LevFargKod

-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEFINE VARIABLE iProfilNr AS INTEGER NO-UNDO.
DEFINE VARIABLE iLoop AS INTEGER NO-UNDO.
DEFINE VARIABLE cOutletLst AS CHARACTER NO-UNDO.
DEFINE VARIABLE cAnv-KodIdList AS CHARACTER NO-UNDO.
DEFINE VARIABLE cKategoriIdList AS CHARACTER NO-UNDO.
DEFINE VARIABLE cButikkIdList AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTekst AS CHARACTER NO-UNDO.

{ ttLagerliste.i } 
      
{syspara.i 22  5 2 cOutletLst}  

DEFINE BUFFER bufttLagerListe FOR ttLagerListe.
   
ASSIGN 
  iProfilNR   = 2
  obOK        = YES
  ocReturn    = ""
  cAnv-KodIdList  = REPLACE(ENTRY(1,icParam,'@'),'|',',')
  cKategoriIdList = REPLACE(ENTRY(2,icParam,'@'),'|',',')
  cButikkIdList   = REPLACE(ENTRY(3,icParam,'@'),'|',',')
  .

/* Lagerliste akkumulert pr. artikkel - ALLE artikler som ligger på Outlet'ene. */
RUN LagerlisteOutlet.
/* Sumerer hver linje - outlet 10 + outlet 40. */  
RUN summerLagerListeOutlet.
  
ihBuffer:COPY-TEMP-TABLE (BUFFER ttLagerliste:HANDLE,NO,NO,YES).

/* **********************  Internal Procedures  *********************** */

PROCEDURE LagerlisteOutlet:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

IF cOutletLst <> '' THEN
BUTLOOP: 
DO iLoop = 1 TO NUM-ENTRIES(cOutletLst):  
  FOR EACH ArtLag NO-LOCK WHERE 
    ArtLag.Butik = INT(ENTRY(iLoop,cOutletLst)) AND 
    ArtLag.LagAnt > 0:
      
    IF ArtLag.LagAnt = ? THEN 
      NEXT.
    
    FIND ttLagerListe WHERE 
      ttLAgerListe.ArtikkelNr = ArtLag.ArtikkelNr AND 
      ttLagerliste.PkSdlNr    = '' AND 
      ttLagerListe.SO         = 9 /*PkSdlHode.SendtOutlet*/ NO-ERROR.
      
    IF NOT AVAILABLE ttLagerListe THEN 
      RUN opprettRecord.
    RUN sumerRecord.
    
  END. /* ARTLAG */
END. /* BUTLOOP */

END PROCEDURE.

PROCEDURE opprettRecord:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

  FIND ArtBas NO-LOCK WHERE 
    ArtBas.ArtikkelNr = ArtLag.ArtikkelNr NO-ERROR.
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
  FIND Hovedkategori NO-LOCK WHERE 
    HovedKategori.HovedKatNr = ArtBAs.HovedKAtNr NO-ERROR.
  FIND Anv-Kod NO-LOCK WHERE
    Anv-Kod.Anv-Id = ArtBas.Anv-Id NO-ERROR.      
  FIND ArtPris NO-LOCK WHERE 
    ArtPris.ArtikkelNr = ArtLag.ArtikkelNr AND 
    ArtPris.ProfilNr   = iProfilNr NO-ERROR.
  IF NOT AVAILABLE ArtPris THEN 
    FIND FIRST ArtPris NO-LOCK WHERE 
      ArtPris.ArtikkelNr = ArtLag.ArtikkelNr NO-ERROR.
  
  CREATE ttLagerListe.
  
  ASSIGN 
    ttLagerListe.ArtikkelNr = ArtLag.ArtikkelNr 
    ttLagerliste.PkSdlNr    = ''  
    ttLagerListe.SO         = 9 /*PkSdlHode.SendtOutlet*/ 
    ttLagerListe.VareTekst  = ArtBas.Beskr
    ttLagerliste.LevKod     = ArtBas.LevKod
    ttLagerliste.LevFargKod = ArtBAs.LevFargKod
    ttLagerListe.MainGroup  = ArtBas.HovedKatNr 
    ttLagerListe.ArtGroup   = ArtBAs.Anv-Id
    ttLagerListe.Sesong     = ArtBas.Sasong
    ttLagerListe.LC         = ArtBas.KjedeInnkPris
    ttLagerListe.WholeSalePris = (IF AVAILABLE ArtPris THEN ArtPris.InnkjopsPris[1] ELSE 0)
    ttLagerListe.InnkjopsPris  = (IF AVAILABLE ArtPris THEN ArtPris.InnkjopsPris[1] ELSE 0)
    ttLagerliste.VVarekostL10  = (IF AVAILABLE ArtPris THEN ArtPris.InnkjopsPris[1] ELSE 0)
    ttLagerliste.VVarekostL40  = (IF AVAILABLE ArtPris THEN ArtPris.InnkjopsPris[1] ELSE 0)
    ttLagerListe.MainGrpTekst  = (IF AVAILABLE Hovedkategori THEN HovedKategori.HovedKatTekst ELSE '')
    ttLagerListe.ArtGrpTekst   = (IF AVAILABLE Anv-Kod THEN Anv-Kod.AnvBeskr ELSE '')
    .

END PROCEDURE.

PROCEDURE sumerRecord:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

  IF AVAILABLE ttLagerListe AND iLoop = 1 THEN 
    ASSIGN
      ttLagerListe.AntL10           = ttLagerListe.AntL10   + ArtLag.LagAnt
      ttLagerListe.VerdiL10         = ttLagerListe.VerdiL10 + (ArtLag.Lagant * ttLagerListe.InnkjopsPris)
      ttLagerListe.VerdiLCL10       = ttLagerListe.VerdiLCL10 + (ArtLag.Lagant * ttLagerListe.LC) 
      ttLagerListe.VerdiWholeSale10 = ttLagerListe.VerdiWholeSale10 + (ArtLag.Lagant * ttLagerListe.WholeSalePris) 
      . 
  ELSE IF AVAILABLE ttLagerListe AND iLoop = 2 THEN 
    ASSIGN
      ttLagerListe.AntL40           = ttLagerListe.AntL40   + ArtLag.LagAnt
      ttLagerListe.VerdiL40         = ttLagerListe.VerdiL40 + (ArtLag.Lagant * ttLagerListe.InnkjopsPris) 
      ttLagerListe.VerdiLCL40       = ttLagerListe.VerdiLCL40 + (ArtLag.Lagant * ttLagerListe.LC) 
      ttLagerListe.VerdiWholeSale40 = ttLagerListe.VerdiWholeSale40 + (ArtLag.Lagant * ttLagerListe.WholeSalePris) 
      . 

END PROCEDURE.

PROCEDURE summerLagerListeOutlet:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

  FOR EACH ttLagerListe:
    ASSIGN 
      ttLagerListe.TotAnt     = ttLagerListe.AntL10     + ttLagerListe.AntL40
      ttLAgerListe.TotVerdi   = ttLagerListe.VerdiL10   + ttLagerListe.VerdiL40
      ttLAgerListe.TotLCVerdi = ttLagerListe.VerdiLCL10 + ttLagerListe.VerdiLCL40
      .
  END.
END PROCEDURE.
