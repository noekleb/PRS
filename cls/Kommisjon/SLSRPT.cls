 
 /*------------------------------------------------------------------------
    File        : SLSRPT
    Purpose     : 
    Syntax      : 
    Description : Import av SLSRPT salgs data filer fra Illums butikkene.
    Author(s)   : Tom Nøkleby
    Created     : Sat Mar 06 16:41:22 CET 2021
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS cls.Kommisjon.SLSRPT: 
  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/
  DEFINE VARIABLE cLogg AS CHARACTER NO-UNDO.
  DEFINE VARIABLE pcTekst AS CHARACTER NO-UNDO.
  DEFINE VARIABLE pcEan AS CHARACTER NO-UNDO.
  DEFINE VARIABLE piLinjeNr AS INTEGER NO-UNDO.
  DEFINE VARIABLE piKasseNr AS INTEGER NO-UNDO.
  DEFINE VARIABLE piKassererNr AS INTEGER NO-UNDO.
  DEFINE VARIABLE piSelgerId AS INTEGER NO-UNDO.
  DEFINE VARIABLE plSelgerNr AS DECIMAL NO-UNDO.
  DEFINE VARIABLE plNtoSlgsPris AS DECIMAL NO-UNDO.
  DEFINE VARIABLE plPris AS DECIMAL NO-UNDO.
  DEFINE VARIABLE plVarekost AS DECIMAL NO-UNDO.
  DEFINE VARIABLE plRabatt AS DECIMAL NO-UNDO.
  DEFINE VARIABLE plQty AS DECIMAL NO-UNDO.
  DEFINE VARIABLE plArtikkelNr AS DECIMAL NO-UNDO.
 
  DEFINE PUBLIC PROPERTY piButNr AS INTEGER NO-UNDO
    GET.
    PRIVATE SET. 
  DEFINE PUBLIC PROPERTY plB_Id AS DECIMAL NO-UNDO
    GET.
    PRIVATE SET. 
  DEFINE PUBLIC PROPERTY cbAktiv AS LOG NO-UNDO 
    GET.
    PRIVATE SET.
  DEFINE PUBLIC PROPERTY ccEkstent AS CHARACTER NO-UNDO 
    GET.                 
    PRIVATE SET.
  DEFINE PUBLIC PROPERTY ccKatalog AS CHARACTER NO-UNDO  
    GET.
    SET.
  DEFINE PUBLIC PROPERTY ccKatalogLst AS CHARACTER NO-UNDO  
    GET.
    SET.
  DEFINE PUBLIC PROPERTY pcEDB-System AS CHARACTER NO-UNDO 
    GET.
    PRIVATE SET.
  DEFINE PUBLIC PROPERTY pcTabell AS CHARACTER NO-UNDO
    GET.
    PRIVATE SET. 
  DEFINE PUBLIC PROPERTY piBongNr AS INTEGER NO-UNDO
    GET.
    PRIVATE SET. 
  DEFINE PUBLIC PROPERTY pdRapportDato AS DATE NO-UNDO
    GET.
    PRIVATE SET. 
  DEFINE PUBLIC PROPERTY pdSalgsDato AS DATE NO-UNDO
    GET.
    PRIVATE SET. 
  DEFINE PUBLIC PROPERTY plFilId AS DECIMAL NO-UNDO
    GET.
    PRIVATE SET. 
  DEFINE PUBLIC PROPERTY plDataSettId AS DECIMAL NO-UNDO
    GET.
    PRIVATE SET. 
  DEFINE PUBLIC PROPERTY piSettNr AS INTEGER NO-UNDO
    GET.
    PRIVATE SET. 
  DEFINE PUBLIC PROPERTY plLinjeSum AS DECIMAL NO-UNDO
    GET.
    PRIVATE SET. 

  DEFINE VARIABLE rStandardFunksjoner AS CLASS cls.StdFunk.StandardFunksjoner NO-UNDO.
  
  {cls\kommisjon\ttBong.i}
  {cls\kommisjon\dsBong.i}
  
  DEFINE STREAM Inn.
  
  CONSTRUCTOR PUBLIC SLSRPT (  ):
    SUPER ().
    rStandardFunksjoner = NEW cls.StdFunk.StandardFunksjoner( ).

    ASSIGN 
      cLogg = 'SLSRPT' + STRING(TODAY,"99999999")
      .
    
    /* Initierer parametre. */
    setParametre(  ).

    rStandardFunksjoner:SkrivTilLogg(cLogg, 
      '  CONSTRUCTOR SLSRPT.cls.' 
      ).
    
  END CONSTRUCTOR.

  CONSTRUCTOR PUBLIC SLSRPT ( INPUT pcLogg AS CHARACTER  ):
    SUPER ().
    rStandardFunksjoner = NEW cls.StdFunk.StandardFunksjoner( ).

    ASSIGN 
      cLogg = pcLogg + STRING(TODAY,"99999999")
      .
    
    /* Initierer parametre. */
    setParametre(  ).
    
    rStandardFunksjoner:SkrivTilLogg(cLogg, 
      '  CONSTRUCTOR SLSRPT.cls m/logg.' 
      ).

  END CONSTRUCTOR.

  /*------------------------------------------------------------------------------
   Purpose: Initierer nødvendige parametre ved oppstart.
   Notes:
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC VOID setParametre(  ):
    ASSIGN 
      cbAktiv      = TRUE
      ccEkstent    = '.edi'
      ccKatalog    = ''
      ccKatalogLst = IF SEARCH('tnc.txt') <> ? 
                       THEN 'c:\nsoft\polygon\prs\kom\in\EDI' 
                     ELSE 'c:\appdir\se\kom\in\EDI'
      pcEDB-System = 'GANT Global'
      pcTabell     = 'KommisjonBut'
      piKasseNr    = 1
      piKassererNr = 1
      piSelgerId   = 1
      .
      
    RETURN.

  END METHOD.

  /*------------------------------------------------------------------------------
   Purpose:
   Notes: UNA:+.? '
          UNB+UNOC:3+7080003883322:14+893635122:+210219:1459+10'
          UNH+1+SLSRPT:D:96A:UN:EAN004'
          BGM+73E::9+10+9'
          DTM+137:20210219:102'
          DTM+356:20201214:102'
          NAD+SU+893635122::9'
          NAD+BY+7080003883322::9'
          CUX+2:NOK:10'
          LOC+162+100::9'
          LIN+1++7325705589753:EN'
          PRI+GRP:280.00'
          PRI+NTP:280.00'
          QTY+153:1.0000:PCE'
          LOC+162+100::9'
          LIN+2++7325705590209:EN'
          PRI+GRP:152.00'
          PRI+NTP:152.00'
          QTY+153:2.0000:PCE'
          UNT+18+1'
          UNZ+1+10'     
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC LOGICAL importerFil( INPUT pcFilNavn AS CHARACTER,
                                     OUTPUT pbOk AS LOG,
                                     OUTPUT pcReturn AS CHARACTER ):
    
    DEFINE VARIABLE bResult AS LOGICAL NO-UNDO.
    DEFINE VARIABLE pcRecord AS CHARACTER NO-UNDO.
    DEFINE VARIABLE pcSegment AS CHARACTER EXTENT 10 NO-UNDO.
    DEFINE VARIABLE piLoop AS INTEGER NO-UNDO.

    rStandardFunksjoner:SkrivTilLogg(cLogg, 
      '    importerFil: ' + pcFilNavn 
      ).

    IF SEARCH(pcFilNavn) <> ? THEN 
    IMPORTERFIL:
    DO ON ERROR UNDO, LEAVE:
      INPUT STREAM Inn FROM VALUE(pcFilNavn) NO-ECHO.
      REPEAT:
        IMPORT STREAM Inn UNFORMATTED
          pcRecord.

MESSAGE NUM-ENTRIES(pcRecord,"'")
VIEW-AS ALERT-BOX.
          
        /* Nullstiller segmentene. */
        ASSIGN pcSegment = ''.
        
        /* Plukker ut segmentene fra recordene. */  
        DO piLoop = 1 TO NUM-ENTRIES(pcRecord,'+'):
        ASSIGN 
          pcSegment[piLoop] = ENTRY(piLoop,pcRecord,'+')
          .
        END.
        CASE pcSegment[1]:
          WHEN 'UNA:' THEN 
            DO:
              /* UNA:+.? ' */
              /* Gjør ingenting med denne. */
            END.
          WHEN 'UNB' THEN /* Interchange Header */
            DO:
              /* UNB+UNOC:3+7080003883322:14+893635122:+210219:1459+10' */
              /* Gjør ingenting med denne. */
            END.
          WHEN 'UNH' THEN /* Message Header - To start and identify a message */
            DO:
              /* UNH+1+SLSRPT:D:96A:UN:EAN004' */
              IF ENTRY(1,pcSegment[3],':') <> 'SLSRPT' THEN 
                DO:
                  bResult = FALSE.
                  rStandardFunksjoner:SkrivTilLogg(cLogg, 
                      '  Filen er ikke en SLSRPT fil (  ' + ENTRY(1,pcSegment[3],':') + ').' 
                      ).
                            
                  LEAVE IMPORTERFIL.
                END.              
            END.
          WHEN 'BGM' THEN /* Beginning of message - To identify the invoice. */
            DO:
              /* BGM+73E::9+10+9' */
              piBongNr = INTEGER(ENTRY(1,pcSegment[3],':')).
            END.
          WHEN 'DTM' THEN /* Date/time/period - To specify a date */
            DO:
              /* DTM+137:20210219:102' */
              /* DTM+356:20201214:102' */
              CASE ENTRY(2,pcSegment[2],':'):
                WHEN '137' THEN 
                  DO:
                    pcTekst = ENTRY(2,pcSegment[2],':').
                    pdRapportDato = DATE(INTEGER(SUBSTRING(pcTekst,7,2)),
                                       INTEGER(SUBSTRING(pcTekst,5,2)),
                                       INTEGER(SUBSTRING(pcTekst,1,4))).
                  END.
                WHEN '356' THEN 
                  DO:
                    pcTekst = ENTRY(2,pcSegment[2],':').
                    pdSalgsDato = DATE(INTEGER(SUBSTRING(pcTekst,7,2)),
                                       INTEGER(SUBSTRING(pcTekst,5,2)),
                                       INTEGER(SUBSTRING(pcTekst,1,4))).
                  END.
              END.
            END.
          WHEN 'NAD' THEN /* Name and address */
            DO:
              /* NAD+SU+893635122::9' */
              /* NAD+BY+7080003883322::9' */
              /* Gjør ingenting med denne. */
            END.
          WHEN 'CUX' THEN /* Currency */
            DO:
              /* CUX+2:NOK:10' */
              opprettBongHode().
            END.
          WHEN 'LOC' THEN /* Location - To specify the location where an item has been sold */
            DO:
              /* LOC+162+100::9' */
              piButNr = getButNr(ENTRY(1,pcSegment[3],':')).
              plSelgerNr = getSelgerNr( piButNr ).
            END.
          WHEN 'LIN' THEN /* Line Item - To specify line number and EAN Code */
            DO:
              /* LIN+1++7325705589753:EN' */
              piLinjeNr = INTEGER(ENTRY(1,pcSegment[2],':')).
              pcEan     = ENTRY(1,pcSegment[4],':').
              /* Oppretter bong hvis linjenr = 1.                     */
              /* Da er alle linjer som inneholder bonghode info lest. */
              IF piLinjeNr = 1 THEN 
                opprettBongHode().
              /* Oppretter bonglinjen. */
              opprettBongLinje().
            END.
          WHEN 'PRI' THEN /* Price details - To specify price per unit */ 
            DO:
              /* PRI+GRP:280.00' */
              /* PRI+NTP:280.00' */
              CASE ENTRY(1,pcSegment[2],':'):
                WHEN 'NTP' THEN
                  DO: 
                    plNtoSlgsPris = DECIMAL(REPLACE(ENTRY(2,pcSegment[2],':'),'.',',')).
                    setKalkyle( plNtoSlgsPris ).
                  END.
              END.
            END.
          WHEN 'QTY' THEN /* Quantity - To specify invoiced quantity */
            DO:
              /* QTY+153:1.0000:PCE' */
              plQty = DECIMAL(REPLACE(ENTRY(2,pcSegment[2],':'),'.',',')).
            END.
          WHEN 'UNT' THEN 
            DO:
              /* UNT+18+1' */
              /* Gjør ingenting med denne. */
            END.
          WHEN 'UNZ' THEN 
            DO:
              /* UNZ+1+10' */              
              /* Bong ferdig innlest. Oppretter betalingslinjen. */
              opprettBetalingslinje(  ).              
            END.
        END CASE. 
           
      END.
      INPUT STREAM Inn CLOSE.
    
      bResult = TRUE.
    END. /* IMPORTERFIL  */

    IF ERROR-STATUS:ERROR THEN 
      INPUT STREAM Inn FROM VALUE(pcFilNavn) NO-ECHO.

    rStandardFunksjoner:SkrivTilLogg(cLogg, 
      '    Slutt importerFil: ' + STRING(bResult) + '.' 
      ).
      
    pbOk = bResult.
    RETURN bResult.

  END METHOD.

  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC INTEGER getButNr( INPUT pcButikkNr AS CHARACTER ):
    
    DEFINE VARIABLE piResult AS INTEGER NO-UNDO.

    FIND FIRST ImpKonv NO-LOCK WHERE 
      ImpKonv.EDB-System = pcEDB-System AND 
      ImpKonv.Tabell     = pcTabell AND 
      ImpKonv.EksterntId = pcButikkNr NO-ERROR.
    IF AVAILABLE ImpKonv THEN 
      piResult = INTEGER(ImpKonv.InterntID).
    ELSE DO: 
      piResult = 0.
      rStandardFunksjoner:SkrivTilLogg(cLogg, 
          '  Ukjent butikknr: ' + pcButikkNr + '.' 
          ).      
    END.


    RETURN piResult.

  END METHOD.

  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC DECIMAL getSelgerNr( INPUT pibutNr AS INTEGER ):
    
    DEFINE VARIABLE plResult AS DECIMAL NO-UNDO.

    FIND FIRST ButikkSelger NO-LOCK WHERE 
      ButikkSelger.ButikkNr = pibutNr AND 
      ButikkSelger.SelgerId = piSelgerId NO-ERROR.
    IF AVAILABLE ButikkSelger THEN 
      plResult = ButikkSelger.SelgerNr.
    ELSE DO: 
      plResult = 0.
      rStandardFunksjoner:SkrivTilLogg(cLogg, 
          '  Ukjent Selgernr for butikk og id: ' + STRING(piButNr) + '/' + STRING(piSelgerId) + '.' 
          ).
      
    END.
    RETURN plResult.

  END METHOD.

  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC LOGICAL setKalkyle( INPUT plSalgsPris AS DECIMAL ):
    
    DEFINE VARIABLE bResult AS LOGICAL NO-UNDO.

    FIND Butiker NO-LOCK WHERE 
      Butiker.butik = piButNr NO-ERROR.
    FIND Strekkode NO-LOCK WHERE 
      Strekkode.Kode = pcEan NO-ERROR.
    IF AVAILABLE Strekkode THEN 
      FIND ArtBas NO-LOCK WHERE 
        ArtBas.ArtikkelNr = Strekkode.ArtikkelNr NO-ERROR.
    IF AVAILABLE ArtBas AND AVAILABLE Butiker THEN 
      DO:
        FIND ArtPris NO-LOCK WHERE 
          ArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND 
          ArtPris.ProfilNr = butiker.ProfilNr NO-ERROR.
        IF NOT AVAILABLE ArtPris THEN 
          FIND FIRST ArtPris NO-LOCK WHERE 
            ArtPris.ArtikkelNr = ArtBas.ArtikkelNr NO-ERROR.
        IF AVAILABLE ArtPris THEN 
          DO:
            ASSIGN 
              plVareKost = ArtBas.KjedeInnkPris
              plPris     = 0
              plRabatt   = ArtPris.Pris[1] - plPris
              .
          END.
        
      END.

    RETURN bResult.

  END METHOD.

  /*------------------------------------------------------------------------------
   Purpose: Oppretter bonghode ved lesing av LOC og linjeNr = 1. 
            Da er all bonghode infomrasjon lest og det kan opprettes.
   Notes:
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC LOGICAL opprettBongHode(  ):
    
    DEFINE VARIABLE bResult AS LOGICAL NO-UNDO.

    DEFINE BUFFER bBongHode FOR BongHode.
    
    DO FOR bBongHode TRANSACTION:
      CREATE BongHode.
      ASSIGN
        bBongHode.ButikkNr      = piButNr 
        bBongHode.GruppeNr      = 1 
        bBongHode.KasseNr       = piKasseNr  
        bBongHode.Dato          = pdSalgsDato
        bBongHode.Tid           = 0
        bBongHode.BongNr        = piBongNr
        bBongHode.BongStatus    = 0 /* Under klargjøring */
        bBongHode.OpdKvit       = TRUE
        bBonghode.DataSettId    = plDataSettId
        bBongHode.Utskriftskopi = "Utskriftskopi ikke mottat for kvittering " + 
                                  STRING(piBongNr) + "."
        bBongHode.KassererNr    = 1
        bBongHode.KassererNavn  = "Kasserer"
        bBongHode.Konvertert    = TRUE
        bBongHode.SelgerNr      = plSelgerNr
        .
      FIND CURRENT bBongHode NO-LOCK.
      ASSIGN
          plB_Id  = bBongHode.B_Id
          bResult = TRUE
          . 
    END. /* TRANSACTION */

    RETURN bResult.

  END METHOD.

  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC LOGICAL ferdigBong(  ):
    
    DEFINE VARIABLE result AS LOGICAL NO-UNDO.

   DEF VAR pBongDato AS DATE NO-UNDO.

/*    IF bTest THEN                                           */
/*        RUN Bibl_LoggDbFri.p(cLogg,'    start ferdigBong'). */
/*                                                            */
/*    DO TRANSACTION:                                         */
/*        FIND CURRENT BongHode EXCLUSIVE-LOCK.               */
/*        BETALING:                                           */
/*        DO:                                                 */
/*            ASSIGN                                          */
/*                BongHode.Belop      = plLinjeSum            */
/*                BongHode.BongStatus = 5 /* Oppdatert */     */
/*                BongHode.Dato       = TODAY /*pBongDato*/   */
/*                .                                           */
/*        END. /* BETALING */                                 */
/*                                                            */
/*        FIND CURRENT BongHode NO-LOCK.                      */
/*    END.                                                    */
/*                                                            */
/*    IF bTest THEN                                           */
/*        RUN Bibl_LoggDbFri.p(cLogg,'    ferdig ferdigBong').*/

    RETURN result.

  END METHOD.

  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC LOGICAL opprettDatasett(  ):
    
    DEFINE VARIABLE bResult AS LOGICAL NO-UNDO.

    DEFINE VARIABLE piSettNr AS INT  NO-UNDO.

    DEFINE BUFFER bDatasett FOR Datasett. 
     
    OPPRETTDATASETT:
    DO TRANSACTION:
      /* Finner neste SettNr */
      FIND LAST bDatasett NO-LOCK WHERE
          bDatasett.ButikkNr = piButNr AND
          bDatasett.GruppeNr = 1 AND
          bDatasett.KasseNr  = piKasseNr  AND
          bDatasett.Dato     = TODAY AND
          bDataSett.FilType  = 1 /* EL-Journal */
          USE-INDEX DataSett NO-ERROR.
      IF AVAILABLE bDataSett THEN
          piSettNr = bDataSett.SettNr + 1.
      ELSE DO:
          piSettNr = 1.
      END.

      /* Finner neste DataSettId */
      FIND LAST bDataSett NO-LOCK
          USE-INDEX DataSettId NO-ERROR.
      IF AVAILABLE bDataSett THEN
          plDataSettId = bDataSett.DataSettId + 1.
      ELSE
          plDataSettId = 1.

      RELEASE bDataSett. /* Ny post skal skapes. */

      IF NOT AVAILABLE bDataSett THEN
      DO:
        CREATE bDataSett.
        ASSIGN
            bDataSett.DataSettId = plDataSettId
            bDataSett.SettStatus = 2 /* Ankommet */
            bDataSett.Behandlet  = 3 /* Behandlet */
            .
      END.

      ASSIGN
        bDataSett.ButikkNr   = piButNr
        bDataSett.GruppeNr   = 1 
        bDataSett.KasseNr    = piKasseNr
        bDataSett.Dato       = TODAY 
        bDataSett.SettNr     = piSettNr
        bDataSett.Tid        = 0
        bDataSett.FilId      = plFilId
        bDataSett.FilType    = 1 /* EL-Journal */
        bResult              = TRUE
        .
      RELEASE bDatasett.
    END. /* OPPRETTDATASETT */

    RETURN bResult.

  END METHOD.

  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC LOGICAL opprettFil( INPUT pcPathName AS CHARACTER, 
                                    INPUT pcFile-Name AS CHARACTER ):
    
    DEFINE VARIABLE bResult AS LOGICAL NO-UNDO.

    DEFINE BUFFER bFiler FOR filer.
    
    DO FOR bFiler TRANSACTION:
       /* Oppretter posten i filen. */
      IF NOT CAN-FIND(bFiler WHERE
                      bFiler.FilNavn   = pcFile-Name AND
                      bFiler.Dato      = TODAY /* KOrdreHode.Dato*/ AND
                      bFiler.Kl        = STRING(TIME,"HH:MM") AND
                      bFiler.Storrelse = 0 AND
                      bFiler.Katalog   = pcPathName
                     ) THEN
      DO:
        /* Finner FilId */
        FIND LAST bFiler NO-LOCK NO-ERROR.
        IF AVAILABLE bFiler THEN
          plFilId = bFiler.FilId + 1.
        ELSE
          plFilId = 1.
        CREATE bFiler.
        ASSIGN
          bFiler.FilId       = plFilId
          bFiler.FilNavn     = pcFile-Name
          bFiler.Dato        = TODAY /*KOrdreHode.Dato*/
          bFiler.Kl          = STRING(TIME,"HH:MM:SS")
          bFiler.Storrelse   = 0
          bFiler.Katalog     = pcPathName
          bFiler.AntLinjer   = 0
          bFiler.FilType     = 1
          bFiler.Innlest     = TRUE
          bFiler.InnlestDato = TODAY /*KOrdreHode.Dato*/
          bFiler.InnlestKl   = TIME
          bFiler.Oppdatert   = TRUE
          bFiler.OppdatertDato = TODAY /*KOrdreHode.Dato*/
          bFiler.OppdatertKl = TIME
          bResult            = TRUE
          .
        RELEASE bFiler.
      END.
    END. /* TRANSACTION */

    RETURN bResult.

  END METHOD.

  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC LOGICAL opprettBongLinje(  ):
    
    DEFINE VARIABLE bResult AS LOGICAL NO-UNDO.

    /* Varelinje */
    FOR EACH KOrdreLinje OF KOrdreHode EXCLUSIVE-LOCK WHERE
      KOrdreLinje.Leveringsdato <> ? AND
      KOrdreLinje.Faktura_Id = 0 AND 
      KOrdreLinje.Aktiv = TRUE:

      IF AVAILABLE ArtBas THEN RELEASE ArtBAs.
      
      ASSIGN
          plArtikkelNr = DEC(KOrdreLinje.VareNr) NO-ERROR.
      IF ERROR-STATUS:ERROR = FALSE AND plArtikkelNr > 0 THEN
      DO:
          FIND ArtBas NO-LOCK WHERE
              ArtBas.ArtikkelNr = plArtikkelNr NO-ERROR.
          IF AVAILABLE ArtBas THEN
              FIND FIRST ArtPris OF ArtBas NO-LOCK WHERE
                ArtPris.ProfilNr = Butiker.ProfilNr NO-ERROR.
          IF AVAILABLE ArtBas THEN
              FIND VarGr OF ArtBas NO-LOCK NO-ERROR.
      END.

      ASSIGN
          piLinjeNr = piLinjeNr + 1.

      /* Her legges bongens varelinjer opp. */
      IF AVAILABLE ArtBas THEN 
      VARE_BONGLINJE:
      DO:
          /* Henter lager og varekost for butikken */
          FIND Lager NO-LOCK WHERE
            Lager.ArtikkelNr = DECIMAL(KOrdreLinje.VareNr) AND
            Lager.Butik      = KOrdreHode.ButikkNr NO-ERROR.
          IF NOT AVAILABLE Lager THEN
            DO:
              CREATE Lager.
              ASSIGN
                  Lager.ArtikkelNr = DECIMAL(KOrdreLinje.VareNr)
                  Lager.Butik      = KOrdreHode.ButikkNr
                  Lager.VVareKost  = IF AVAILABLE ArtPris
                                       THEN ArtPris.Varekost[1]
                                       ELSE 0
                  Lager.VVareKost  = IF AVAILABLE ArtPris
                                       THEN ArtPris.Varekost[1]
                                       ELSE 0
                  NO-ERROR.
              /* Tar bort den delhvis opprettede posten hvis det feiler. */
              IF ERROR-STATUS:ERROR AND AVAILABLE Lager THEN
                  DELETE Lager.
            END.

          FIND VarGR NO-LOCK OF ArtBas NO-ERROR.

          FIND BongLinje EXCLUSIVE-LOCK WHERE
               BongLinje.ButikkNr = BongHode.ButikkNr AND
               BongLinje.GruppeNr = BongHode.GruppeNr AND
               BongLinje.KasseNr  = BongHode.KasseNr  AND
          BongLinje.Dato     = TODAY /*KOrdreLinje.Leveringsdato*/ AND
               BongLinje.BongNr   = BongHode.BongNr   AND
               BongLinje.LinjeNr  = piLinjeNr NO-ERROR.
          IF NOT AVAILABLE BongLinje THEN
          DO:
            CREATE BongLinje. /* */
            ASSIGN
                BongLinje.B_Id         = BongHode.B_Id
                BongLinje.ButikkNr     = BongHode.ButikkNr 
                BongLinje.GruppeNr     = BongHode.GruppeNr 
                BongLinje.KasseNr      = BongHode.KasseNr  
                BongLinje.Dato         = TODAY /*KOrdreLinje.Leveringsdato*/    
                BongLinje.TransDato    = TODAY /*KOrdreLinje.Leveringsdato*/
                BongLinje.TransTid     = BongHode.Tid
                BongLinje.BongNr       = BongHode.BongNr   
                BongLinje.TTId         = IF KOrdreLinje.Antall > 0 
                                           THEN 1  /* Kontant salg */
                                           ELSE 10 /* Retur */
                BongLinje.TBId         = 1
                BongLinje.LinjeNr      = piLinjeNr /*BongLinje*/
                .
          END.

          ASSIGN
            BongLinje.ArtikkelNr = STRING(ArtBas.ArtikkelNr)
            BongLinje.Strekkode  = KOrdreLinje.Kode
            BongLinje.VareGr     = ArtBas.Vg
            BongLinje.LopeNr     = ArtBas.LopNr
            BongLinje.Storrelse  = KOrdreLinje.Storl
            BongLinje.BongTekst  = KOrdreLinje.Varetekst
            BongLinje.Antall     = KOrdreLinje.Antall
            BongLinje.LinjeSum   = abs(KOrdreLinje.NettoLinjeSum) + abs(KOrdreLinje.LinjeRabattKr)
            BongLinje.BongPris   = abs(BongLinje.LinjeSum)
            BongLinje.VVarekost  = (IF AVAILABLE Lager THEN abs(Lager.VVarekost) ELSE 0) * abs(BongLinje.Antall)
            BongLinje.LinjeRab   = abs(KOrdreLinje.LinjeRabattKr)
            BongLinje.VareGruppeNavn = IF AVAILABLE VarGr
                                     THEN VarGr.VgBeskr
                                     ELSE ""

            BongLinje.Mva%       = KOrdreLinje.Mva%
            BongLinje.MvaKr      = ABSOLUTE(KOrdreLinje.MvaKr)
            BongLinje.FeilKode   = 0
            BongLinje.NotatKode  = 0
            BongLinje.RefNr      = KOrdreHode.Opphav
            BongLinje.RefTekst   = (IF KOrdreHode.EkstOrdreNr <> '' THEN 'OrdreNr. Nettbutikk: ' ELSE 'Kundeordre: ') + 
                                   (IF KOrdreHode.EkstOrdreNr <> '' THEN KOrdreHode.EkstOrdreNr ELSE STRING(KOrdreLinje.KOrdre_Id))
            .

          ASSIGN
            plLinjeSum = plLinjeSum + BongLinje.LinjeSum - KOrdreLinje.LinjeRabattKr
            .

          FIND FIRST Moms NO-LOCK WHERE
            Moms.MomsProc = BongLinje.Mva% NO-ERROR.
          IF AVAILABLE Moms THEN
            ASSIGN
            BongLinje.MvaGr         = Moms.MomsKod
            BongLinje.MvaGruppeNavn = Moms.Beskrivelse
            .
            
          IF AVAILABLE BongLinje THEN 
            RELEASE BongLinje.

      END. /* VARE_BONGLINJE */
  END.    /* VArelinje slutt */

    RETURN bResult.

  END METHOD.

  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC LOGICAL opprettBetalingslinje(  ):
    
    DEFINE VARIABLE bResult AS LOGICAL NO-UNDO.

    /* Start */
    BET_BONGLINJE:
    DO:
        FIND BongLinje EXCLUSIVE-LOCK WHERE
             BongLinje.ButikkNr = BongHode.ButikkNr AND
             BongLinje.GruppeNr = BongHode.GruppeNr AND
             BongLinje.KasseNr  = BongHode.KasseNr  AND
          BongLinje.Dato     = TODAY /*KOrdreLinje.Leveringsdato*/ AND
             BongLinje.BongNr   = BongHode.BongNr   AND
             BongLinje.LinjeNr  = piLinjeNr NO-ERROR.
        IF NOT AVAILABLE BongLinje THEN
        DO:
          CREATE BongLinje. /* */
          ASSIGN
              BongLinje.B_Id         = BongHode.B_Id
              BongLinje.ButikkNr     = BongHode.ButikkNr 
              BongLinje.GruppeNr     = BongHode.GruppeNr 
              BongLinje.KasseNr      = BongHode.KasseNr  
              BongLinje.Dato         = TODAY /*KOrdreLinje.Leveringsdato*/    
              BongLinje.TransDato    = TODAY /*KOrdreLinje.Leveringsdato*/
              BongLinje.TransTid     = BongHode.Tid
              BongLinje.BongNr       = BongHode.BongNr   
              BongLinje.TTId         = IF KOrdreLinje.VareNr = 'KUPONG' THEN 71 ELSE 52
              BongLinje.TBId         = 1
              BongLinje.LinjeNr      = piLinjeNr /*BongLinje*/
              BongLinje.BongTekst    = IF KOrdreLinje.VareNr = 'KUPONG' THEN "KUPONG1" ELSE 'KLARNA'
              BongLinje.Strekkode    = KOrdreLinje.Kode
              .
        END.

        ASSIGN
          BongLinje.BongPris = abs(KOrdreLinje.NettoLinjeSum)
          BongLinje.LinjeSum = abs(KOrdreLinje.NettoLinjeSum)
          BongLinje.Antall   = IF BongLinje.TTId = 71 THEN 0 ELSE 901
          .


        IF AVAILABLE BongLinje THEN RELEASE BongLinje.
    END. /* BET_BONGLINJE */      
    /* Slutt */

    RETURN bResult.

  END METHOD.

  DESTRUCTOR PUBLIC SLSRPT ( ):
    rStandardFunksjoner:SkrivTilLogg(cLogg, 
    '  DESTRUCTOR SLSRPT.cls.' 
    ).
  END DESTRUCTOR.

END CLASS.