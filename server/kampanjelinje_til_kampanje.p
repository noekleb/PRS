/* kampanjelinje_til_kampanje.p

DEF TEMP-TABLE ArtLag
    FIELD butik AS INTEGER
    FIELD ArtLag_VmId AS INTEGER
    FIELD ArtLag_Varemerke AS CHARACTER
    FIELD ArtLag_SasId AS INTEGER
    FIELD ArtLag_Sasong AS CHARACTER
    FIELD ArtLag_Beskr AS CHARACTER
    FIELD ArtLag_LevKod AS CHARACTER
    FIELD ArtLag_LevFargKod AS CHARACTER
    FIELD storl AS CHARACTER
    FIELD ArtLag_Pris AS DECIMAL
    FIELD ArtLag_Kode AS CHARACTER
    FIELD lagant AS DECIMAL
    FIELD ArtLag_Reservert AS DECIMAL
    FIELD ArtLag_Varekost AS DECIMAL
    FIELD ArtLag_Tilbud AS CHARACTER
    FIELD ArtLag_Rab% AS DECIMAL
    FIELD ArtLag_VVareKost AS DECIMAL
    FIELD ArtBas_WebButikkArtikkel AS LOGICAL
    FIELD ArtBas_PubliserINettButikk AS LOGICAL
    FIELD EndretDatoTid AS DATETIME
    FIELD ArtikkelNr AS DECIMAL
    FIELD StrKode AS INTEGER
    FIELD ArtLag_LevNr AS INTEGER
    FIELD RowIdent1 AS CHARACTER 
    FIELD RowCount AS INTEGER
    FIELD jbCountDistinct AS INTEGER FORMAT '>>>,>>>,>>9' INIT 1
    FIELD jbAverage AS DECIMAL FORMAT '->>>,>>>,>>9.99'

-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEFINE VARIABLE hQuery          AS HANDLE    NO-UNDO.
DEFINE VARIABLE iKampanjeId AS INTEGER NO-UNDO.
DEF VAR iCl            AS INT NO-UNDO.
DEFINE VARIABLE iButNr AS INTEGER NO-UNDO.
DEFINE VARIABLE cLogg AS CHARACTER NO-UNDO.
DEFINE VARIABLE bTest AS LOG NO-UNDO.
DEFINE VARIABLE iLinjeNr AS INTEGER NO-UNDO.

DEFINE VARIABLE rStandardFunksjoner AS cls.StdFunk.StandardFunksjoner NO-UNDO.

DEFINE BUFFER clButiker FOR Butiker.

DEFINE TEMP-TABLE ttArtikkel
  FIELD ArtikkelNr AS DECIMAL FORMAT ">>>>>>>>>>>>9"
  INDEX idxArtikkel ArtikkelNr.

ASSIGN 
  bTest      = IF SEARCH('tnc.txt') <> ? THEN TRUE ELSE FALSE
  cLogg      = 'kampanjelinje_til_kampanje' + REPLACE(STRING(TODAY),'/','') 
  NO-ERROR.

rStandardFunksjoner  = NEW cls.StdFunk.StandardFunksjoner( cLogg ) NO-ERROR.
IF bTest THEN 
  rStandardFunksjoner:SkrivTilLogg(cLogg,
    'Start' 
    ).    

{syspara.i 5 1 1 iCl INT}.
FIND clButiker NO-LOCK WHERE
    clButiker.Butik = iCl NO-ERROR.

{syspara.i 150 1 2 iButNr INT}.
FIND Butiker NO-LOCK WHERE
    Butiker.Butik = iButNr NO-ERROR.

ASSIGN 
    obOk     = TRUE
    iKampanjeId = INT(ENTRY(1,icParam,'|'))
    .

FIND KampanjeHode NO-LOCK WHERE 
  KampanjeHode.KampanjeId = iKampanjeId NO-ERROR.
IF NOT AVAILABLE KampanjeHode THEN 
DO:
  ASSIGN 
    obok     = FALSE 
    ocReturn = '** Finner ikke kampanje ' + STRING(iKampanjeId)
    .
  IF bTest THEN
  DO: 
    rStandardFunksjoner:SkrivTilLogg(cLogg,
      '    ocReturn: ' + ocReturn + '. Avbryter.' 
      ).    
    rStandardFunksjoner:SkrivTilLogg(cLogg,
      '  icParam....: ' + icParam 
      ).        
    rStandardFunksjoner:SkrivTilLogg(cLogg,
      'Slutt' 
      ).
  END.    
  RETURN. 
END.

FIND LAST KampanjeLinje OF KampanjeHode USE-INDEX idxKampanjeLinje NO-LOCK NO-ERROR.
IF AVAILABLE KampanjeLinje THEN 
  iLinjeNr = KampanjeLinje.KampanjeLinje + 1.
ELSE 
  iLinjeNr = 1.

IF bTest THEN 
DO:
  rStandardFunksjoner:SkrivTilLogg(cLogg,
    '  iKampanjeId: ' + STRING(iKampanjeId) + ' Available: ' + STRING(AVAILABLE KampanjeHode) 
    ).    
  rStandardFunksjoner:SkrivTilLogg(cLogg,
    '  icParam....: ' + icParam 
    ).        
  rStandardFunksjoner:SkrivTilLogg(cLogg,
    '  ihBuffer:NAME: ' + ihBuffer:NAME 
    ).        
  rStandardFunksjoner:SkrivTilLogg(cLogg,
    '  LinjeNr....: ' + STRING(iLinjeNr) 
    ).        
END.
  
CREATE QUERY hQuery.
hQuery:SET-BUFFERS(ihBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + ihBuffer:NAME + " NO-LOCK").
hQuery:QUERY-OPEN().

hQuery:GET-FIRST().
LOOPEN:
REPEAT WHILE NOT hQuery:QUERY-OFF-END:
  ASSIGN 
    ocReturn = ''
    .
  FIND ArtBas NO-LOCK WHERE 
    ArtBas.ArtikkelNr = DEC(ihBuffer:BUFFER-FIELD('ArtikkelNr'):BUFFER-VALUE) NO-ERROR.
        
  IF AVAILABLE ArtBas AND NOT CAN-FIND(ttArtikkel WHERE 
                  ttArtikkel.ArtikkelNr = ArtBas.ArtikkelNr) THEN 
  DO:
    CREATE ttArtikkel.
    ASSIGN 
      ttArtikkel.ArtikkelNr = ArtBas.ArtikkelNr
      .
  END.      
        
  IF AVAILABLE ArtBas THEN 
  DO:

    rStandardFunksjoner:SkrivTilLogg(cLogg,
      '  Artikkel: ' + STRING(ArtBas.ArtikkelNr) + ' ' + ArtBas.Beskr + ' ' + ArtBas.LevKod + ' ' + ArtBas.LevFargKod 
      ).    

    IF NOT CAN-FIND(KampanjeLinje WHERE 
                    KampanjeLinje.KampanjeId = KampanjeHode.KampanjeId AND 
                    KampanjeLinje.Vg         = ArtBas.Vg AND 
                    KampanjeLinje.LopNr      = ArtBas.LopNr) THEN 
    DO:
      CREATE KampanjeLinje.
      ASSIGN 
        KampanjeLinje.KampanjeId = KampanjeHode.KampanjeId  
        KampanjeLinje.Vg         = ArtBas.Vg  
        KampanjeLinje.LopNr      = ArtBas.LopNr
        KampanjeLinje.KampanjeLinje = iLinjeNr
        KampanjeLinje.ArtikkelNr = ArtBas.ArtikkelNr
        KampanjeLinje.EDato      = TODAY 
        KampanjeLinje.ETid       = TIME
        KampanjeLinje.BrukerId   = USERID('skotex')
        KampanjeLinje.RegistrertDato = TODAY 
        KampanjeLinje.RegistrertTid  = TIME
        KampanjeLinje.RegistrertAv   = USERID('skotex')
        KampanjeLinje.ProfilNr   = KampanjeHode.ProfilNr
        iLinjeNr = iLinjeNr + 1 
        .
      FIND ArtPris NO-LOCK WHERE
        ArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND
        ArtPris.ProfilNr   = KampanjeHode.ProfilNr NO-ERROR.
      IF NOT AVAILABLE ArtPris THEN 
        FIND FIRST ArtPris NO-LOCK WHERE
          ArtPris.ArtikkelNr = ArtBas.ArtikkelNr NO-ERROR.
      IF AVAILABLE ArtPris THEN
        ASSIGN
          KampanjeLinje.VareKost = ArtPris.VareKost[1]
          KampanjeLinje.Pris[1]  = ArtPris.Pris[1]
          KampanjeLinje.Pris[2]  = ROUND(ArtPris.Pris[1] - ((ArtPris.Pris[1] * (KampanjeHode.Kamp% * -1)) / 100),2)
          KampanjeLinje.VareKost = IF KampanjeLinje.VareKost = ? THEN 0 ELSE KampanjeLinje.VareKost
          KampanjeLinje.Pris[2]  = IF KampanjeLinje.Pris[2] = ? THEN 0 ELSE KampanjeLinje.Pris[2]
          KampanjeLinje.Pris[1]  = IF KampanjeLinje.Pris[1] = ? THEN 0 ELSE KampanjeLinje.Pris[1]
          .
      rStandardFunksjoner:SkrivTilLogg(cLogg,
        '  Kampanjelinje: ' + STRING(iKampanjeId) + ' ' + STRING(ArtBas.Vg) + ' ' + STRING(ArtBas.LopNr) 
        ).    
    END.
  END.      

  hQuery:GET-NEXT().
END. /* LOOPEN */

ASSIGN 
  obOK     = TRUE 
  ocReturn = ''
  .

IF bTest THEN 
  rStandardFunksjoner:SkrivTilLogg(cLogg,
    'Slutt' 
    ).    
