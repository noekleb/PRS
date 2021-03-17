/* vare_til_pksdllinje.p

-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEFINE VARIABLE hQuery          AS HANDLE    NO-UNDO.
DEFINE VARIABLE lPkSdlId AS DECIMAL NO-UNDO.
DEF VAR iCl            AS INT NO-UNDO.
DEFINE VARIABLE iButNr AS INTEGER NO-UNDO.
DEFINE VARIABLE cLogg AS CHARACTER NO-UNDO.
DEFINE VARIABLE bTest AS LOG NO-UNDO.
DEFINE VARIABLE iLinjeNr AS INTEGER NO-UNDO.
DEF VAR bStdPrisOverf AS LOG  NO-UNDO.

DEFINE VARIABLE rStandardFunksjoner AS cls.StdFunk.StandardFunksjoner NO-UNDO.

DEFINE BUFFER clButiker FOR Butiker.

ASSIGN 
  bTest      = IF SEARCH('test.txt') <> ? THEN TRUE ELSE FALSE
  cLogg      = 'vare_til_pksdllinje' + REPLACE(STRING(TODAY),'/','') 
  NO-ERROR.

rStandardFunksjoner  = NEW cls.StdFunk.StandardFunksjoner( cLogg ) NO-ERROR.
IF bTest THEN 
  rStandardFunksjoner:SkrivTilLogg(cLogg,
    'Start' 
    ).    

{syspara.i 5 26 1 bStdPrisOverf LOGICAL}

{syspara.i 5 1 1 iCl INT}.
FIND clButiker NO-LOCK WHERE
    clButiker.Butik = iCl NO-ERROR.


ASSIGN 
    obOk     = TRUE
    lPkSdlId = INT(ENTRY(1,icParam,'|'))
    iButNr   =  INT(ENTRY(2,icParam,'|'))
    .
FIND Butiker NO-LOCK WHERE
    Butiker.Butik = iButNr NO-ERROR.

FIND PkSdlHode NO-LOCK WHERE 
  PkSdlHode.PkSdlId = lPkSdlId NO-ERROR.
IF NOT AVAILABLE PkSdlHode THEN 
DO:
  ASSIGN 
    obok     = FALSE 
    ocReturn = '** Finner ikke pakkseddel ' + STRING(lPkSdlId)
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

FIND LAST PkSdlLinje OF PkSdlHode NO-LOCK USE-INDEX PkSdlLinje NO-ERROR.
IF AVAILABLE PkSdlLinje 
  THEN iLinjeNr = PkSdlLinje.PkSdlLinjeId + 1.
ELSE 
  iLinjeNr = 1.

IF bTest THEN 
DO:
  rStandardFunksjoner:SkrivTilLogg(cLogg,
    '  lPkSdlId: ' + STRING(lPkSdlId) + ' Available: ' + STRING(AVAILABLE PkSdlHode) 
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
        
  IF AVAILABLE ArtBas THEN 
  DO:
    rStandardFunksjoner:SkrivTilLogg(cLogg,
      '  Artikkel: ' + STRING(ArtBas.ArtikkelNr) + ' ' + ArtBas.Beskr + ' ' + ArtBas.LevKod + ' ' + ArtBas.LevFargKod 
      ).    
  END.      

  FIND FIRST PkSdlLinje NO-LOCK WHERE 
    PkSdlLinje.PkSdlId = PkSdlHode.PkSdlId AND 
    PkSdlLinje.ArtikkelNr = ArtBas.ArtikkelNr AND 
    PkSdlLinje.Kode = ihBuffer:BUFFER-FIELD('ArtLag_Kode'):BUFFER-VALUE NO-ERROR.    
  IF NOT AVAILABLE PkSdlLinje THEN
  PKSDL_LINJE:
  DO: 
    CREATE PkSdlLinje.
    ASSIGN
      PkSdlLinje.Linjenr       = iLinjeNr
      PkSdlLinje.PkSdlLinjeId  = iLinjeNr

      PkSdlLinje.PkSdlId       = PksdlHode.PkSdlId
      PkSdlLinje.ArtikkelNr    = (IF AVAILABLE ArtBas THEN ArtBas.ArtikkelNr ELSE 0)
      PkSdlLinje.BestNr        = 0
      PkSdlLinje.OrdreNr       = 0
      PkSdlLinje.Beskr         = (IF AVAILABLE ArtBas THEN ArtBas.Beskr ELSE '')
      PkSdlLinje.LevFargKod    = (IF AVAILABLE ArtBas THEN ArtBas.LevFargKod ELSE '')
      PkSdlLinje.Antall        = 1
      PkSdlLinje.AntLevert     = 1
      PkSdlLinje.LevKod        = (IF AVAILABLE ArtBas THEN ArtBas.LevKod ELSE '')
      PkSdlLinje.LevNr         = (IF AVAILABLE ArtBas THEN ArtBas.LevNr ELSE 0)
      PkSdlLinje.StrKode       = INT(ihBuffer:BUFFER-FIELD('StrKode'):BUFFER-VALUE)
      PkSdlLinje.Kode          = ihBuffer:BUFFER-FIELD('ArtLag_Kode'):BUFFER-VALUE
      PkSdlLinje.Salgsenhet    = (IF AVAILABLE ArtBas THEN ArtBas.SalgsEnhet ELSE '')
      PkSdlLinje.ButikkNr      = iButNr
      PkSdlLinje.Pakke         = FALSE 
      PkSdlLinje.PakkeNr       = 0
      iLinjeNr = iLinjeNr + 1
      NO-ERROR.
  END. /* PKSDL_LINJE */

  /* Oppretter pakkseddel pris */
  FIND FIRST ArtPris NO-LOCK
       WHERE ArtPris.ArtikkelNr = ArtBas.ArtikkelNr
         AND ArtPris.ProfilNr   = Butiker.ProfilNr NO-ERROR.
  IF NOT AVAILABLE ArtPris THEN 
      FIND FIRST ArtPris NO-LOCK
           WHERE ArtPris.ArtikkelNr = ArtBas.ArtikkelNr
           AND ArtPris.ProfilNr     = clButiker.ProfilNr NO-ERROR.

  FIND PkSdlPris EXCLUSIVE-LOCK WHERE
    PkSdlPris.PkSdlId    = PkSdlHode.PkSdlId AND
    PkSdlPris.ArtikkelNr = ArtBas.ArtikkelNr NO-ERROR.
  IF NOT AVAILABLE PkSdlPris THEN 
  DO:
      CREATE PkSdlPris.
      ASSIGN
          PkSdlPris.PkSdlId    = PkSdlHode.PkSdlId
          PkSdlPris.ArtikkelNr = ArtBas.ArtikkelNr.        
      BUFFER-COPY ArtBas   
        EXCEPT    ArtikkelNr BrukerId EDato Etid RegistrertDato RegistrertTid RegistrertAv 
        TO        PkSdlPris.
  END.
  ASSIGN 
         PkSdlPris.VareKost       = ArtPris.VareKost[1]
         PkSdlPris.Rab1%          = ArtPris.Rab1%[1]
         PkSdlPris.Pris           = ArtPris.Pris[1]
         PkSdlPris.Frakt          = ArtPris.Frakt[1]
         PkSdlPris.Db%            = ArtPris.Db%[1]
         PkSdlPris.InnkjopsPris   = ArtPris.InnkjopsPris[1]
         PkSdlPris.OverstyrPris   = bStdPrisOverf
         /* Ny pris som skal gjelde i butikken */
         PkSdlPris.NyPris         = ArtPris.Pris[1]
         PkSdlPris.NyVarekost     = ArtPris.VareKost[1] 
         PkSdlPris.NyRab1%        = ArtPris.Rab1%[1]
         PkSdlPris.NyInnkjopsPris = ArtPris.InnkjopsPris[1]
         PkSdlPris.NyFrakt        = 0
         PkSdlPris.NyDB%          = ArtPris.Db%[1]
         PkSdlPris.OverstyrPris   = YES
         NO-ERROR.

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
