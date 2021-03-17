/* pksdl_salgOutlet.p

-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEFINE VARIABLE iProfilNr AS INTEGER NO-UNDO.
DEFINE VARIABLE iLoop AS INTEGER NO-UNDO.
DEFINE VARIABLE dDato AS DATE NO-UNDO.
DEFINE VARIABLE dFraDato AS DATE NO-UNDO.
DEFINE VARIABLE dTilDato AS DATE NO-UNDO.
DEFINE VARIABLE cOutletLst AS CHARACTER NO-UNDO.
DEFINE VARIABLE cAnv-KodIdList AS CHARACTER NO-UNDO.
DEFINE VARIABLE cKategoriIdList AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTekst AS CHARACTER NO-UNDO.
DEFINE VARIABLE cLogg AS CHARACTER NO-UNDO.

/* Standard funksjoner for logging */
DEFINE VARIABLE rStandardFunksjoner AS cls.StdFunk.StandardFunksjoner NO-UNDO.

{ ttLagerliste.i }
    
{syspara.i 22  5 2 cOutletLst}  

rStandardFunksjoner  = NEW cls.StdFunk.StandardFunksjoner( cLogg ) NO-ERROR.  

DEFINE BUFFER bufttLagerListe FOR ttLagerListe.

ASSIGN 
  cLogg = 'pksdl_salgOutlet' + REPLACE(STRING(TODAY),'/','')
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
  iProfilNR   = 2
  obOK        = YES
  ocReturn    = ""
  cTekst      = ENTRY(1,icParam,'@')
  dFraDato    = DATE(ENTRY(1,cTekst,'|'))
  dTilDato    = DATE(ENTRY(2,cTekst,'|'))
  cAnv-KodIdList  = REPLACE(ENTRY(2,icParam,'@'),'|',',')
  cKategoriIdList = REPLACE(ENTRY(3,icParam,'@'),'|',',')
  .

rStandardFunksjoner:SkrivTilLogg(cLogg,
    '  dFraDato: ' + STRING(dFraDato)
    ).
rStandardFunksjoner:SkrivTilLogg(cLogg,
    '  dTilDato: ' + STRING(dTilDato)
    ).
rStandardFunksjoner:SkrivTilLogg(cLogg,
    '  cAnv-KodIdList: ' + cAnv-KodIdList
    ).
rStandardFunksjoner:SkrivTilLogg(cLogg,
    '  cKategoriIdList: ' + cKategoriIdList
    ).

/* Lagerliste akkumulert pr. artikkel - ALLE artikler som ligger på Outlet'ene. */
RUN LagerlisteOutlet.
/* Sumerer hver linje - outlet 10 + outlet 40. */  
RUN summerLagerListeOutlet.
  
ihBuffer:COPY-TEMP-TABLE (BUFFER ttLagerliste:HANDLE,NO,NO,YES).

rStandardFunksjoner:SkrivTilLogg(cLogg,
    '  Available ttLagerliste: ' + STRING(CAN-FIND(FIRST ttLagerListe))
    ).

rStandardFunksjoner:SkrivTilLogg(cLogg,
    'Slutt.' 
    ).

/* **********************  Internal Procedures  *********************** */

PROCEDURE LagerlisteOutlet:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

  rStandardFunksjoner:SkrivTilLogg(cLogg,
      '  LOOP:'
      ).

IF cOutletLst <> '' THEN
BUTLOOP: 
DO iLoop = 1 TO NUM-ENTRIES(cOutletLst):
  rStandardFunksjoner:SkrivTilLogg(cLogg,
      '  Outlet: ' + ENTRY(iLoop,cOutletLst)
      ).

  DATOLOOP:
  DO dDato = dFraDato TO dTilDato:
    rStandardFunksjoner:SkrivTilLogg(cLogg,
        '    Dato: ' + STRING(dDato)
        ).

    FOR EACH TransLogg NO-LOCK WHERE 
      TransLogg.Butik = INT(ENTRY(iLoop,cOutletLst)) AND
      TransLogg.Dato  = dDato USE-INDEX ButDatoTid:  
        
      /* Bare varesalg og retur telles med. */  
      IF NOT CAN-DO('1,10',STRING(TransLogg.TTId)) THEN 
        NEXT.
      
      /* Legger opp SO 9 for å skille fra andre. */
      /* Legger opp salgsrecord - setter S= = 9. */
      FIND FIRST ttLagerListe WHERE 
        ttLAgerListe.ArtikkelNr = Translogg.ArtikkelNr AND 
        ttLagerliste.PkSdlNr    = '' AND 
        ttLagerListe.SO         = 9 /*PkSdlHode.SendtOutlet*/
        NO-ERROR.
        
      IF NOT AVAILABLE ttLagerListe THEN 
        RUN opprettRecord.
      RUN sumerRecord.
      
    END. /* Translogg */
  END. /* DATOLOOP */
END. /* BUTLOOP */

END PROCEDURE.

PROCEDURE opprettRecord:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

  FIND ArtBas NO-LOCK WHERE 
    ArtBas.ArtikkelNr = Translogg.ArtikkelNr NO-ERROR.
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
    HovedKategori.HovedKatNr = ArtBas.HovedKatNr NO-ERROR.
  FIND Anv-Kod NO-LOCK WHERE
    Anv-Kod.Anv-Id = ArtBas.Anv-Id NO-ERROR.      
  FIND ArtPris NO-LOCK WHERE 
    ArtPris.ArtikkelNr = Translogg.ArtikkelNr AND 
    ArtPris.ProfilNr   = iProfilNr NO-ERROR.
  IF NOT AVAILABLE ArtPris THEN 
    FIND FIRST ArtPris NO-LOCK WHERE 
      ArtPris.ArtikkelNr = Translogg.ArtikkelNr NO-ERROR.
  
  CREATE ttLagerListe.
  
  ASSIGN 
    ttLagerListe.ArtikkelNr = Translogg.ArtikkelNr 
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

    rStandardFunksjoner:SkrivTilLogg(cLogg,
        '      Artikkel: ' + STRING(ttLagerListe.ArtikkelNr)
        ).

END PROCEDURE.

PROCEDURE sumerRecord:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

  IF AVAILABLE ttLagerListe AND iLoop = 1 THEN 
    ASSIGN
      ttLagerListe.AntL10     = ttLagerListe.AntL10   + TransLogg.Antall
      ttLagerListe.VerdiL10   = ttLagerListe.VerdiL10 + (TransLogg.Antall * ttLagerListe.InnkjopsPris)
      ttLagerListe.VerdiLCL10 = ttLagerListe.VerdiLCL10 + (TransLogg.Antall * ttLagerListe.LC) 
      ttLagerListe.VerdiWholeSale10 = ttLagerListe.VerdiWholeSale10 + (TransLogg.Antall * ttLagerListe.WholeSalePris) 
      . 
  ELSE IF AVAILABLE ttLagerListe AND iLoop = 2 THEN 
    ASSIGN
      ttLagerListe.AntL40     = ttLagerListe.AntL40   + TransLogg.Antall
      ttLagerListe.VerdiL40   = ttLagerListe.VerdiL40 + (TransLogg.Antall * ttLagerListe.InnkjopsPris) 
      ttLagerListe.VerdiLCL40 = ttLagerListe.VerdiLCL40 + (TransLogg.Antall * ttLagerListe.LC) 
      ttLagerListe.VerdiWholeSale40 = ttLagerListe.VerdiWholeSale40 + (TransLogg.Antall * ttLagerListe.WholeSalePris) 
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
      ttLAgerListe.TotVerdiWholeSale = ttLagerListe.VerdiWholeSale10 + ttLagerListe.VerdiWholeSale40
      .
  END.
END PROCEDURE.
