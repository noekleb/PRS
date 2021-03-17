/* pksdl_totsum.p
   Parameter:  
   Opprettet:   
     
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
DEFINE VARIABLE cOutletIdLst AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTekst AS CHARACTER NO-UNDO.

/* Standard funksjoner for logging */
DEFINE VARIABLE rStandardFunksjoner AS cls.StdFunk.StandardFunksjoner NO-UNDO.

{ ttLagerliste.i }

DEFINE BUFFER ttSumLagerListe FOR ttLagerListe.

ASSIGN 
  cLogg       = 'pksdl_totsum' + REPLACE(STRING(TODAY),'/','')
  ocReturn = ""
  cTekst          = REPLACE(ENTRY(1,icParam,'@'),'|',',')
  cLagerListe     = ENTRY(1,cTekst,'|')
  cAnv-KodIdList  = REPLACE(ENTRY(2,icParam,'@'),'|',',')
  cKategoriIdList = REPLACE(ENTRY(3,icParam,'@'),'|',',')
  cButikkIdList   = REPLACE(ENTRY(4,icParam,'@'),'|',',')
  cOutletIdLst    = REPLACE(ENTRY(5,icParam,'@'),'|',',')
  
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
rStandardFunksjoner:SkrivTilLogg(cLogg,
    '    cOutletIdLst: ' + cOutletIdLst 
    ).

CREATE ttSumLagerListe.

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
    FOR EACH PkSdlLinje OF PkSdlHode NO-LOCK,
      FIRST PkSdlPris OF PkSdlLinje NO-LOCK:
      FIND ArtBas OF PkSdlLinje NO-LOCK NO-ERROR.
      IF AVAILABLE ArtBas THEN
      SUMMER: 
      DO:
        IF cAnv-KodIdList <> '' THEN 
        DO:
          IF NOT CAN-DO(cAnv-KodIdList,STRING(ArtBas.Anv-Id)) THEN 
            LEAVE SUMMER.
        END.      
        IF cKategoriIdList <> '' THEN 
        DO:
          IF NOT CAN-DO(cKategoriIdList,STRING(ArtBas.HovedKatNr)) THEN 
            LEAVE SUMMER.
        END.      
        IF cButikkIdList <> '' THEN 
        DO:
          IF NOT CAN-DO(cButikkIdList,STRING(ihBuffer:BUFFER-FIELD("pksdl_FraButikk"):BUFFER-VALUE)) THEN 
            LEAVE SUMMER.
        END.      
        IF cOutletIdLst <> '' THEN 
        DO:
          IF NOT CAN-DO(cOutletIdLst,STRING(ihBuffer:BUFFER-FIELD("pksdl_ButikkNr"):BUFFER-VALUE)) THEN 
            LEAVE SUMMER.
        END.      
        ASSIGN
          ttSumLagerListe.AntPkSdl      = ttSumLagerListe.AntPkSdl      + PkSdlLinje.AntLevert
          ttSumLagerListe.VerdiPkSdl    = ttSumLagerListe.VerdiPkSdl    + (PkSdlLinje.AntLevert * PkSdlPris.NyVareKost)
          ttSumLagerListe.VerdiLC       = ttSumLagerListe.VerdiLC       + (PkSdlLinje.AntLevert * ArtBas.KjedeInnkPris) 
          ttSumLagerListe.WholeSalePris = ttSumLagerListe.WholeSalePris + (PkSdlLinje.AntLevert * PkSdlPris.InnkjopsPris)
          .
        IF ArtBas.KjedeInnkPris = 0 THEN 
          ttSumLagerListe.VerdiWholeSale = ttSumLagerListe.VerdiWholeSale + (PkSdlLinje.AntLevert * PkSdlPris.InnkjopsPris).
        
      END. /* SUMMER */ 
    END.
  END.

  hQuery:GET-NEXT().
END. /* BLOKKEN */

DELETE OBJECT hQuery NO-ERROR.

IF CAN-FIND(FIRST ttSumLagerListe) THEN 
DO:
  ASSIGN 
    obOK = TRUE
    ocReturn = STRING(ttSumLagerListe.AntPkSdl)  + '|' + 
               STRING(ttSumLagerListe.VerdiPkSdl) + '|' + 
               STRING(ttSumLagerListe.VerdiLC) + '|' + 
               STRING(ttSumLagerListe.WholeSalePris) + '|' +
               STRING(ttSumLagerListe.VerdiWholeSale) 
    .
END.
ELSE DO:
  ASSIGN 
    obOK = TRUE 
    ocReturn = '||'
    .
END.
EMPTY TEMP-TABLE ttSumLagerListe.

rStandardFunksjoner:SkrivTilLogg(cLogg,
    'Slutt.' 
    ).
