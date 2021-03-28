 
/*------------------------------------------------------------------------
   File        : SLSRPT
   Purpose     : 
   Syntax      : 
   Description : Import av SLSRPT salgs data filer fra Illums butikkene.
   Author(s)   : Tom Nøkleby
   Created     : Sat Mar 06 16:41:22 CET 2021
   Notes       : 
    27/3-21 TN Korrigert beløp og rabatt m.m. på bonglinjene.
    
 ----------------------------------------------------------------------*/

USING Progress.Lang.*.

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS cls.Kommisjon.SLSRPT: 
  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/
  DEFINE VARIABLE cLogg         AS CHARACTER NO-UNDO.
  DEFINE VARIABLE pcTekst       AS CHARACTER NO-UNDO.
  DEFINE VARIABLE pcEan         AS CHARACTER NO-UNDO.
  DEFINE VARIABLE piLinjeNr     AS INTEGER   NO-UNDO.
  DEFINE VARIABLE piKasseNr     AS INTEGER   NO-UNDO.
  DEFINE VARIABLE piKassererNr  AS INTEGER   NO-UNDO.
  DEFINE VARIABLE piSelgerId    AS INTEGER   NO-UNDO.
  DEFINE VARIABLE plSelgerNr    AS DECIMAL   NO-UNDO.
  DEFINE VARIABLE plNtoSlgsPris AS DECIMAL   NO-UNDO.
  DEFINE VARIABLE plVarekost    AS DECIMAL   NO-UNDO.
  DEFINE VARIABLE plRabatt      AS DECIMAL   NO-UNDO.
  DEFINE VARIABLE plQty         AS DECIMAL   NO-UNDO.
  DEFINE VARIABLE plArtikkelNr  AS DECIMAL   NO-UNDO.
  DEFINE VARIABLE lcFile        AS LONGCHAR  NO-UNDO.
  DEFINE VARIABLE lOmrFaktor    AS DECIMAL   NO-UNDO.
  DEFINE VARIABLE cB_IdLst      AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cDataSettIdLst AS CHARACTER NO-UNDO.
 
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
      lOmrFaktor   = 0.55 /* Den andel Illums skal ha. */
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

  METHOD PUBLIC LOGICAL importerFil( INPUT pcFullPathName AS CHARACTER,
    INPUT pcFilNavn AS CHARACTER,
    OUTPUT pbOk AS LOG,
    OUTPUT pcReturn AS CHARACTER ):
    
    DEFINE VARIABLE bResult   AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE pcRecord  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE pcSegment AS CHARACTER EXTENT 10 NO-UNDO.
    DEFINE VARIABLE piLoop    AS INTEGER   NO-UNDO.
    DEFINE VARIABLE piRecLoop AS INTEGER   NO-UNDO.
    
    rStandardFunksjoner:SkrivTilLogg(cLogg, 
      '    importerFil: ' + pcFullPathName 
      ).

    /* Det kan komme varesalg fra flere butikker i samme fil.            */
    /* Listen her brukes til å logge dongene som er opprettet fra filen. */
    /* Hver bong skal senere avsluttes, og betalingslinje påføres.       */
    ASSIGN 
      cB_IdLst = ''
      cDataSettIdLst = ''
      .

    IF SEARCH(pcFullPathName) <> ? THEN 
    IMPORTERFIL:
    DO ON ERROR UNDO, LEAVE:
      /* Herter filinformasjonen. */
      FILE-INFO:FILE-NAME = SEARCH(pcFullPathName).
      /* Legger filen inn i variabelen. Filen har ikke CR etter hver linje. */
      COPY-LOB FILE FILE-INFO:FULL-PATHNAME TO lcFile.
      /* Tar bort siste, slik at vi ikke får en tom entry. */
      lcFile = RIGHT-TRIM(lcFile,"'").
      
      /* Her loopes det igjennon filen, og recordene plukkes ut. */
      RECORDLOOP:
      DO piRecLoop = 1 TO NUM-ENTRIES(lcFile,"'"):

        /* Nullstiller segmentene og plukker ut recorden. */
        ASSIGN 
          pcSegment = ''
          pcRecord  = ENTRY(piRecLoop,lcFile,"'")
          .
        
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
              CASE ENTRY(1,pcSegment[2],':'):
                WHEN '137' THEN 
                  DO:
                    pcTekst = ENTRY(2,pcSegment[2],':').
                    pdRapportDato = DATE(INTEGER(SUBSTRING(pcTekst,5,2)),
                      INTEGER(SUBSTRING(pcTekst,7,2)),
                      INTEGER(SUBSTRING(pcTekst,1,4))).
                  END.
                WHEN '356' THEN 
                  DO:
                    pcTekst = ENTRY(2,pcSegment[2],':').
                    pdSalgsDato = DATE(INTEGER(SUBSTRING(pcTekst,5,2)),
                      INTEGER(SUBSTRING(pcTekst,7,2)),
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
            /* Gjør ingenting med denne. */
            END.
          WHEN 'LOC' THEN /* Location - To specify the location where an item has been sold  */
            /* NB NB NB ... det kan komme salg fra flere butikker i samme fil. */
            DO:
              /* LOC+162+100::9' */
              IF piButNr <> getButNr(ENTRY(1,pcSegment[3],':')) THEN 
              DO:
                piButNr = getButNr(ENTRY(1,pcSegment[3],':')).
                /* Kan først opprette Datasett når butikknr. er kjent. */              
                opprettDatasett( ).                  
                /* Oppretter fil. Datasettid er nå satt. */
                opprettFil (INPUT  pcFullPathName, INPUT pcFilNavn).
                /* Oppretter bong hvis linjenr = 1.                     */
                opprettBongHode().
              END.
              plSelgerNr = getSelgerNr( piButNr ).
            END.
          WHEN 'LIN' THEN /* Line Item - To specify line number and EAN Code */
            DO:
              /* LIN+1++7325705589753:EN' */
              piLinjeNr = INTEGER(ENTRY(1,pcSegment[2],':')).
              pcEan     = ENTRY(1,pcSegment[4],':').
            END.
          WHEN 'PRI' THEN /* Price details - To specify price per unit */ 
            DO:
              /* PRI+GRP:280.00' */
              /* PRI+NTP:280.00' */
              CASE ENTRY(1,pcSegment[2],':'):
                WHEN 'NTP' THEN
                  DO: 
                    plNtoSlgsPris = DECIMAL(REPLACE(ENTRY(2,pcSegment[2],':'),'.',',')).
                    setVarekost( ).
                  END.
              END.
            END.
          WHEN 'QTY' THEN /* Quantity - To specify invoiced quantity */
            DO:
              /* QTY+153:1.0000:PCE' */
              plQty = DECIMAL(REPLACE(ENTRY(2,pcSegment[2],':'),'.',',')).
              /* Oppretter bonglinjen. QTY er siste info på bonglinje. */
              opprettBongLinje().
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
              /* Markerer bongen klar for oppdatering i PRS BO. */
              setBongStatus( ).            
            END.
        END CASE. 
           
      END. /* RECORDLOOP */
    
      bResult = TRUE.
    END. /* IMPORTERFIL  */

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
    ELSE 
    DO: 
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
    ELSE 
    DO: 
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

  METHOD PUBLIC LOGICAL setVareKost( ):
    
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
      FIND bBongHode NO-LOCK WHERE 
        bBongHode.ButikkNr      = piButNr AND 
        bBongHode.GruppeNr      = 1 AND 
        bBongHode.KasseNr       = piKasseNr AND   
        bBongHode.Dato          = pdSalgsDato AND 
        bBongHode.BongNr        = piBongNr NO-ERROR.
      IF NOT AVAILABLE bBongHode THEN 
      DO:
        CREATE bBongHode.
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
      END.
      ASSIGN
        plB_Id   = bBongHode.B_Id
        bResult  = TRUE
        .
      IF NOT CAN-DO(cB_IdLst,STRING(plB_Id)) THEN  
          cB_IdLst = cB_IdLst + (IF cB_IdLst = '' THEN '' ELSE ',') + STRING(plB_Id). 
    END. /* TRANSACTION */

    RETURN bResult.

  END METHOD.

  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC LOGICAL ferdigBong(  ):
    
    DEFINE VARIABLE result    AS LOGICAL NO-UNDO.

    DEF    VAR      pBongDato AS DATE    NO-UNDO.

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
    
    DEFINE VARIABLE bResult  AS LOGICAL NO-UNDO.
    DEFINE VARIABLE piSettNr AS INT     NO-UNDO.

    DEFINE BUFFER bDatasett FOR Datasett. 

    /* Finner neste SettNr */
    FIND LAST bDatasett NO-LOCK WHERE
      bDatasett.ButikkNr = piButNr AND
      bDatasett.GruppeNr = 1 AND
      bDatasett.KasseNr  = piKasseNr  AND
      bDatasett.Dato     = TODAY AND
      bDataSett.FilType  = 1 /* EL-Journal */
      USE-INDEX DataSett NO-ERROR.
    IF AVAILABLE bDataSett AND bDataSett.SettStatus = 8 THEN 
    DO: 
      ASSIGN 
        plDataSettId = bDataSett.DataSettId
        piSettNr     = bDataSEtt.SettNr
        bResult      = TRUE
        .
    END.
    ELSE
    OPPRETTDATASETT:
    DO TRANSACTION:
      IF AVAILABLE bDataSett THEN
        piSettNr = bDataSett.SettNr + 1.
      ELSE 
      DO:
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
          bDataSett.SettStatus = 8
          /*          bDataSett.SettStatus = 2 /* Ankommet */ */
          /*          bDataSett.Behandlet  = 3 /* Behandlet */*/
          .
      END.

      ASSIGN
        bDataSett.ButikkNr = piButNr
        bDataSett.GruppeNr = 1 
        bDataSett.KasseNr  = piKasseNr
        bDataSett.Dato     = TODAY 
        bDataSett.SettNr   = piSettNr
        bDataSett.Tid      = 0
        bDataSett.FilId    = plFilId
        bDataSett.FilType  = 1 /* EL-Journal */
        cDataSettIdLst     = cDataSettIdLst + (IF cDataSettIdLst = '' THEN '' ELSE ',') + STRING(bDataSett.DataSettId) 
        bResult            = TRUE
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
    
    /* Henter filinformasjonen */
    FILE-INFO:FILE-NAME = pcPathName.    
    
    DO FOR bFiler TRANSACTION:
      /* Oppretter posten i filen. */
      IF NOT CAN-FIND(bFiler WHERE
        bFiler.FilNavn   = pcFile-Name AND
      bFiler.Dato      = TODAY /* KOrdreHode.Dato*/ AND
        bFiler.Kl        = STRING(TIME,"HH:MM:SS") AND
        bFiler.Storrelse = FILE-INFO:FILE-SIZE AND
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
          bFiler.FilId         = plFilId
          bFiler.FilNavn       = pcFile-Name
          bFiler.Dato          = TODAY /*KOrdreHode.Dato*/
          bFiler.Kl            = STRING(TIME,"HH:MM:SS")
          bFiler.Storrelse     = FILE-INFO:FILE-SIZE
          bFiler.Katalog       = pcPathName
          bFiler.AntLinjer     = 0
          bFiler.FilType       = 1
          bFiler.Innlest       = TRUE
          bFiler.InnlestDato   = TODAY /*KOrdreHode.Dato*/
          bFiler.InnlestKl     = TIME
          bFiler.Oppdatert     = TRUE
          bFiler.OppdatertDato = TODAY /*KOrdreHode.Dato*/
          bFiler.OppdatertKl   = TIME
          bResult              = TRUE
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
    DEFINE VARIABLE lMvaKr  AS DECIMAL NO-UNDO.

    DEFINE BUFFER bBongHode  FOR BongHode.
    DEFINE BUFFER bBongLinje FOR BongLinje.

    IF AVAILABLE ArtBas THEN RELEASE ArtBAs.
    IF AVAILABLE ArtPris THEN RELEASE ArtPris.
    IF AVAILABLE VarGr THEN RELEASE VarGr.
    IF AVAILABLE StrKonv THEN RELEASE StrKonv.
    IF AVAILABLE Lager THEN RELEASE Lager.
    
    FIND Strekkode NO-LOCK WHERE 
      Strekkode.Kode = pcEan NO-ERROR.
    IF AVAILABLE Strekkode THEN
    DO: 
      FIND ArtBas OF Strekkode NO-LOCK NO-ERROR.
      FIND StrKonv OF Strekkode NO-LOCK NO-ERROR.
      ASSIGN
        plArtikkelNr = Strekkode.ArtikkelNr NO-ERROR.
    END.
    ELSE plArtikkelNr = 0.
    
    IF AVAILABLE ArtBas THEN 
    DO:
      FIND FIRST ArtPris OF ArtBas NO-LOCK WHERE
        ArtPris.ProfilNr = Butiker.ProfilNr NO-ERROR.
      FIND VarGr OF ArtBas NO-LOCK NO-ERROR.
    END.

    /* Her legges bongens varelinjer opp. */
    VARE_BONGLINJE:
    DO FOR bBongHode, bBongLinje TRANSACTION:
      FIND bBongHode NO-LOCK WHERE 
        bBongHode.B_Id = plB_Id NO-ERROR.
      FIND Butiker NO-LOCK WHERE 
        Butiker.Butik = bBongHode.ButikkNr NO-ERROR.
      
      /* Henter lager og varekost for butikken */
      FIND Lager NO-LOCK WHERE
        Lager.ArtikkelNr = plArtikkelNr AND
        Lager.Butik      = bBongHode.ButikkNr NO-ERROR.
      IF NOT AVAILABLE Lager THEN
      DO:
        CREATE Lager.
        ASSIGN
          Lager.ArtikkelNr = plArtikkelNr
          Lager.Butik      = bBongHode.ButikkNr
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

      FIND bBongLinje EXCLUSIVE-LOCK WHERE
        bBongLinje.ButikkNr = bBongHode.ButikkNr AND
        bBongLinje.GruppeNr = bBongHode.GruppeNr AND
        bBongLinje.KasseNr  = bBongHode.KasseNr  AND
        bBongLinje.Dato     = bBongHode.Dato AND
        bBongLinje.BongNr   = bBongHode.BongNr   AND
        bBongLinje.LinjeNr  = piLinjeNr NO-ERROR.
      IF NOT AVAILABLE bBongLinje THEN
      DO:
        CREATE bBongLinje. /* */
        ASSIGN
          bBongLinje.B_Id      = bBongHode.B_Id
          bBongLinje.ButikkNr  = bBongHode.ButikkNr 
          bBongLinje.GruppeNr  = bBongHode.GruppeNr 
          bBongLinje.KasseNr   = bBongHode.KasseNr  
          bBongLinje.Dato      = bBongHode.Dato    
          bBongLinje.TransDato = bBongHode.Dato
          bBongLinje.TransTid  = 0 /* Ligger ikke tidspunkt i filen. */
          bBongLinje.BongNr    = bBongHode.BongNr   
          bBongLinje.TTId      = IF plQty > 0 
                                     THEN 1  /* Kontant salg */
          ELSE 10 /* Retur */
          bBongLinje.TBId      = 1
          bBongLinje.LinjeNr   = piLinjeNr /*BongLinje*/
          .
      END.

      ASSIGN
        bBongLinje.ArtikkelNr     = STRING(plArtikkelNr)
        bBongLinje.Strekkode      = pcEan
        bBongLinje.VareGr         = IF AVAILABLE ArtBas THEN ArtBas.Vg ELSE 0
        bBongLinje.LopeNr         = IF AVAILABLE ArtBas THEN ArtBas.LopNr ELSE 0
        bBongLinje.Storrelse      = IF AVAILABLE StrKonv THEN StrKonv.Storl ELSE ''
        bBongLinje.BongTekst      = IF AVAILABLE ArtBas THEN ArtBas.Beskr ELSE ''
        bBongLinje.VareGruppeNavn = IF AVAILABLE VarGr THEN VarGr.VgBeskr ELSE ""
        bBongLinje.VVarekost      = (IF AVAILABLE ArtBas THEN ArtBas.KjedeInnkPris ELSE 0)
        bBongLinje.VVarekost      = (IF bBongLinje.VVarekost = 0 AND AVAILABLE ArtPris 
                                      THEN ROUND((ArtPris.InnkjopsPris[1] * 45) / 100,0) 
                                    ELSE bBongLinje.VVarekost)
        bBongLinje.Mva%           = IF AVAILABLE ArtPris THEN ArtPris.Mva%[1] ELSE 25
        lMvaKr                    = ROUND((plNtoSlgsPris * bBongLinje.Mva%) / 100,2) 
        bBongLinje.FeilKode       = 0
        bBongLinje.NotatKode      = 0
        bBongLinje.RefNr          = 0
        bBongLinje.RefTekst       = ''
        bBongLinje.Antall         = plQty
        bBongLinje.LinjeSum       = (plNtoSlgsPris + lMvaKr) * ABSOLUTE(plQty)
        bBongLinje.LinjeRab       = bBongLinje.LinjeSum * lOmrFaktor
        bBongLinje.BongPris       = ABSOLUTE(bBongLinje.LinjeSum)
        bBongLinje.MvaKr          = lMvaKr * ABSOLUTE(plQty)
        .

      FIND FIRST Moms NO-LOCK WHERE
        Moms.MomsProc = bBongLinje.Mva% NO-ERROR.
      IF AVAILABLE Moms THEN
        ASSIGN
          bBongLinje.MvaGr         = Moms.MomsKod
          bBongLinje.MvaGruppeNavn = Moms.Beskrivelse
          .
        
      IF AVAILABLE bBongLinje THEN
      DO: 
        RELEASE bBongLinje.
        bResult = TRUE.
      END.
    END. /* TRANSACTION VARE_BONGLINJE */

    RETURN bResult.

  END METHOD.

  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC LOGICAL opprettBetalingslinje(  ):
    
  DEFINE VARIABLE bResult AS LOGICAL NO-UNDO.
  DEFINE VARIABLE piLoop AS INTEGER NO-UNDO.
  DEFINE VARIABLE piLokLinjeNr AS INTEGER NO-UNDO.
  DEFINE VARIABLE plLokLinjeSum AS DECIMAL NO-UNDO.
    
  DEFINE BUFFER bBongLinje FOR BongLinje.
  DEFINE BUFFER bBongHode  FOR BongHode.
  DEFINE BUFFER bDataSett  FOR DataSett.

  /* Sluttbehandler de bongene som er opprettet fra filen. */
  IF cB_IdLst <> '' THEN
  BONGLOOP: 
  DO FOR bBongHode, bBongLinje piLoop = 1 TO NUM-ENTRIES(cB_IdLst) TRANSACTION:
    /* Henter bongHode. */
    FIND bBongHode EXCLUSIVE-LOCK WHERE 
      bBongHode.B_Id = DEC(ENTRY(piLoop,cB_IdLst)) NO-ERROR.
    IF NOT AVAILABLE bBongHode THEN 
      NEXT BONGLOOP.
      
    ASSIGN 
      piLokLinjeNr  = 0
      plLokLinjeSum = 0
      .
    
    /* Summerer opp linjene. */
    FOR EACH bBongLinje NO-LOCK WHERE 
      bBongLinje.B_Id = bBongHode.B_Id USE-INDEX b_id:
      
      /* Ved retur kommer plQty negativ. */
      ASSIGN 
        plLokLinjeSum = plLokLinjeSum + ((bBongLinje.LinjeSum - bBongLinje.LinjeRab) * IF bBongLinje.Antall < 0 THEN -1 ELSE 1)  
        .
      
      /* Siste linjeNr */
      piLokLinjeNr = bBongLinje.LinjeNr.  
    END.        
        
    /* Start */
    BET_BONGLINJE:
    DO:
        
      FIND bBongLinje EXCLUSIVE-LOCK WHERE
        bBongLinje.ButikkNr = bBongHode.ButikkNr AND
        bBongLinje.GruppeNr = bBongHode.GruppeNr AND
        bBongLinje.KasseNr  = bBongHode.KasseNr  AND
        bBongLinje.Dato     = bBongHode.Dato AND
        bBongLinje.BongNr   = bBongHode.BongNr   AND
        bBongLinje.LinjeNr  = piLokLinjeNr + 1 NO-ERROR.
      IF NOT AVAILABLE bBongLinje THEN
      DO:
        CREATE bBongLinje. /* */
        ASSIGN
          bBongLinje.B_Id      = bBongHode.B_Id
          bBongLinje.ButikkNr  = bBongHode.ButikkNr 
          bBongLinje.GruppeNr  = bBongHode.GruppeNr 
          bBongLinje.KasseNr   = bBongHode.KasseNr  
          bBongLinje.Dato      = bBongHode.Dato    
          bBongLinje.TransDato = bBongHode.Dato
          bBongLinje.TransTid  = 0
          bBongLinje.BongNr    = bBongHode.BongNr   
          bBongLinje.TTId      = 50 /* Kontant */
          bBongLinje.TBId      = 1
          bBongLinje.LinjeNr   = piLokLinjeNr + 1
          bBongLinje.BongTekst = 'KONTANT'
          bBongLinje.Strekkode = ''
          .
      END.

      ASSIGN
        bBongLinje.BongPris = plLokLinjeSum
        bBongLinje.LinjeSum = plLokLinjeSum
        bBongHode.Belop     = plLokLinjeSum
        bBongLinje.Antall   = 0
        plLinjeSum          = 0
        .

      IF AVAILABLE bBongLinje THEN
      DO: 
        RELEASE bBongLinje.
        RELEASE bBongHode.
      END.
    END. /* BET_BONGLINJE */      
  END. /* TRANSACTION BONGLOOP */
    
  IF cDataSettIdLst <> '' THEN 
  DO FOR bDataSett piLoop = 1 TO NUM-ENTRIES(cDataSettIdLst)  TRANSACTION:
    plDataSettId = DEC(ENTRY(piLoop,cDataSettIdLst)).
    FIND bDataSett EXCLUSIVE-LOCK WHERE 
      bDataSett.DataSettId = plDataSettId NO-ERROR.
    IF AVAILABLE bDataSett THEN 
      ASSIGN 
        bDataSett.SettStatus = 2 /* Ankommet */
        bDataSett.Behandlet  = 3 /* Behandlet */
        bDataSett.FilId      = plFilId
        .
    RELEASE bDataSEtt.
  END. /* TRANSACTION */

  /* Slutt */

  RETURN bResult.

END METHOD.

/*------------------------------------------------------------------------------
 Purpose: Markere bong klar for videre oppdatering i PRS BO.
 Notes:
------------------------------------------------------------------------------*/

METHOD PUBLIC LOGICAL setBongStatus(  ):
    
  DEFINE VARIABLE pbResult AS LOGICAL NO-UNDO.
  DEFINE VARIABLE piLoop AS INTEGER NO-UNDO.
  
    
  DEFINE BUFFER bBongHode FOR BongHode.

  DO FOR bBongHode piLoop = 1 TO NUM-ENTRIES(cB_IdLst) TRANSACTION:
    plB_Id = DEC(ENTRY(piLoop,cB_IdLst)).
    FIND bBongHode EXCLUSIVE-LOCK WHERE
      bBongHode.B_Id = plB_Id NO-WAIT NO-ERROR.
    IF AVAILABLE bBongHode AND NOT LOCKED bBongHode THEN
    DO:
      ASSIGN
        bBongHode.BongStatus = 5
        pbResult             = TRUE
        .
      RELEASE bBongHode.
    END.
  END.
  RETURN pbResult.

END METHOD.

DESTRUCTOR PUBLIC SLSRPT ( ):
  rStandardFunksjoner:SkrivTilLogg(cLogg, 
    '  DESTRUCTOR SLSRPT.cls.' 
    ).
END DESTRUCTOR.

END CLASS.