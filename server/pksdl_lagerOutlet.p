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
DEFINE VARIABLE cLagerListe AS CHARACTER NO-UNDO.
DEFINE VARIABLE iValg AS INTEGER NO-UNDO.

{ ttLagerliste.i }
   
{syspara.i 22  5 2 cOutletLst}  

DEFINE BUFFER bufttLagerListe FOR ttLagerListe.
   
ASSIGN 
  cLagerListe = ENTRY(1,icParam,'|')
  iValg       = INT(ENTRY(2,icParam,'|'))
  iProfilNR   = 2
  obOK        = YES
  ocReturn    = ""
  .

/* Henter lagerliste med pakkseddel info. */
IF SEARCH(cLagerListe) <> ? THEN 
  TEMP-TABLE ttLagerListe:READ-JSON('file',cLagerListe,'EMPTY').

IF iValg = 1 THEN 
  RUN supplerLagerliste1.
ELSE 
  RUN supplerLagerliste2.
RUN summerLagerListe.

ihBuffer:COPY-TEMP-TABLE (BUFFER ttLagerliste:HANDLE,NO,NO,YES).

/* **********************  Internal Procedures  *********************** */

PROCEDURE summerLagerListe:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

FOR EACH ttLagerListe:
  ASSIGN 
    ttLagerListe.TotAnt   = ttLagerListe.AntPkSdl   + ttLagerListe.AntL10 + ttLagerListe.AntL40
    ttLAgerListe.TotVerdi = ttLagerListe.VerdiPkSdl + ttLagerListe.VerdiL10 + ttLagerListe.VerdiL40
    ttLAgerListe.TotLCVerdi = ttLagerListe.VerdiLCL10 + ttLagerListe.VerdiLCL40
    .
END.

END PROCEDURE.

PROCEDURE supplerLagerliste1:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

IF cOutletLst <> '' THEN
BUTLOOP: 
DO iLoop = 1 TO NUM-ENTRIES(cOutletLst):
  TEMP-TABLELst:
  FOR EACH bufttLagerListe:
  FOR EACH ArtLag NO-LOCK WHERE
    ArtLag.ArtikkelNr = bufttLagerListe.ArtikkelNr AND  
    ArtLag.Butik = INT(ENTRY(iLoop,cOutletLst)) AND 
    ArtLag.LagAnt > 0:
      
    IF ArtLag.LagAnt = ? THEN 
      NEXT.
    
    FIND ArtBas NO-LOCK WHERE 
      ArtBas.ArtikkelNr = ArtLag.ArtikkelNr NO-ERROR.
    IF NOT AVAILABLE ArtBas THEN 
      NEXT.
    FIND Hovedkategori NO-LOCK WHERE 
      HovedKategori.HovedKatNr = ArtBAs.HovedKAtNr NO-ERROR.
    FIND Anv-Kod NO-LOCK WHERE
      Anv-Kod.Anv-Id = ArtBas.Anv-Id NO-ERROR.      
    FIND ttLagerListe WHERE 
      ttLAgerListe.ArtikkelNr = bufttLagerListe.ArtikkelNr AND 
      ttLagerListe.PkSdlNr    = bufttLagerListe.PkSdlNr AND 
      ttLagerListe.SO         = bufttLagerListe.SO NO-ERROR.
    IF NOT AVAILABLE ttLagerListe THEN 
    DO:
      FIND ArtPris NO-LOCK WHERE 
        ArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND 
        ArtPris.ProfilNr   = iProfilNr NO-ERROR.
      IF NOT AVAILABLE ArtPris THEN 
        FIND FIRST ArtPris NO-LOCK WHERE 
          ArtPris.ArtikkelNr = ArtBas.ArtikkelNr NO-ERROR.
      CREATE ttLagerListe.
      ASSIGN 
        ttLagerListe.ArtikkelNr = bufttLagerListe.ArtikkelNr
        ttLagerListe.PkSdlNr    = bufttLagerListe.PkSdlNr
        ttLagerListe.SO         = bufttLagerListe.SO 
        ttLagerListe.VareTekst  = ArtBas.Beskr
        ttLagerliste.LevKod     = ArtBas.LevKod
        ttLagerliste.LevFargKod = ArtBAs.LevFargKod
        ttLagerListe.MainGroup  = ArtBas.HovedKatNr 
        ttLagerListe.ArtGroup   = ArtBAs.Anv-Id
        ttLagerListe.Sesong     = ArtBas.Sasong
        ttLagerListe.InnkjopsPris = (IF AVAILABLE ArtPris THEN ArtPris.InnkjopsPris[1] ELSE 0)  
        ttLagerListe.MainGrpTekst = (IF AVAILABLE Hovedkategori THEN HovedKategori.HovedKatTekst ELSE '')
        ttLagerListe.ArtGrpTekst  = (IF AVAILABLE Anv-Kod THEN Anv-Kod.AnvBeskr ELSE '')
        ttLagerliste.VVarekostL10 = (IF AVAILABLE ArtPris THEN ArtPris.InnkjopsPris[1] ELSE 0)
        ttLagerliste.VVarekostL40 = (IF AVAILABLE ArtPris THEN ArtPris.InnkjopsPris[1] ELSE 0)
        .
    END.
    IF AVAILABLE ttLagerListe AND iLoop = 1 THEN 
      ASSIGN
        ttLagerListe.AntL10     = ttLagerListe.AntL10   + ArtLag.LagAnt
        ttLagerListe.VerdiL10   = ttLagerListe.VerdiL10 + (ArtLag.Lagant * ttLagerListe.InnkjopsPris) 
        . 
    ELSE IF AVAILABLE ttLagerListe AND iLoop = 2 THEN 
      ASSIGN
        ttLagerListe.AntL40     = ttLagerListe.AntL40   + ArtLag.LagAnt
        ttLagerListe.VerdiL40   = ttLagerListe.VerdiL40 + (ArtLag.Lagant * ttLagerListe.InnkjopsPris) 
        . 
  END. /* ARTLAG */
  END. /* TEMP-TABLELst */
END. /* BUTLOOP */

END PROCEDURE.

PROCEDURE supplerLagerliste2:
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
    
    FIND ArtBas NO-LOCK WHERE 
      ArtBas.ArtikkelNr = ArtLag.ArtikkelNr NO-ERROR.
    IF NOT AVAILABLE ArtBas THEN 
      NEXT.
    FIND ttLagerListe WHERE 
      ttLAgerListe.ArtikkelNr = ArtLag.ArtikkelNr AND 
      ttLagerliste.PkSdlNr    = '' AND 
      ttLagerListe.SO         = 9 /*PkSdlHode.SendtOutlet*/ NO-ERROR.
    IF NOT AVAILABLE ttLagerListe THEN 
    DO:
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
        ttLagerListe.InnkjopsPris = (IF AVAILABLE ArtPris THEN ArtPris.InnkjopsPris[1] ELSE 0)
        ttLagerliste.VVarekostL10 = (IF AVAILABLE ArtPris THEN ArtPris.InnkjopsPris[1] ELSE 0)
        ttLagerliste.VVarekostL40 = (IF AVAILABLE ArtPris THEN ArtPris.InnkjopsPris[1] ELSE 0)
        .
    END.
    IF AVAILABLE ttLagerListe AND iLoop = 1 THEN 
      ASSIGN
        ttLagerListe.AntL10     = ttLagerListe.AntL10   + ArtLag.LagAnt
        ttLagerListe.VerdiL10   = ttLagerListe.VerdiL10 + (ArtLag.Lagant * ttLagerListe.InnkjopsPris)
        ttLagerListe.VerdiLCL10 = ttLagerListe.VerdiLCL10 + (ArtLag.Lagant * ttLagerListe.LC) 
        . 
    ELSE IF AVAILABLE ttLagerListe AND iLoop = 2 THEN 
      ASSIGN
        ttLagerListe.AntL40     = ttLagerListe.AntL40   + ArtLag.LagAnt
        ttLagerListe.VerdiL40   = ttLagerListe.VerdiL40 + (ArtLag.Lagant * ttLagerListe.InnkjopsPris) 
        ttLagerListe.VerdiLCL40 = ttLagerListe.VerdiLCL40 + (ArtLag.Lagant * ttLagerListe.LC) 
        . 
  END. /* ARTLAG */
END. /* BUTLOOP */

END PROCEDURE.
