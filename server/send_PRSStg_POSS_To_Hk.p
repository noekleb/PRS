&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : 
    Purpose     :

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */


DEF VAR hServer           AS HANDLE    NO-UNDO.
DEF VAR cConnectionString AS CHARACTER NO-UNDO.
DEF VAR lConnected        AS LOGICAL   NO-UNDO.
DEF VAR obOk              AS LOG       NO-UNDO.
DEF VAR cIpAdr            AS CHAR      NO-UNDO.

DEF VAR iAntLinjer          AS INT     NO-UNDO.
DEF VAR iAlle               AS INT     NO-UNDO.
DEFINE VARIABLE lDec        AS DECIMAL NO-UNDO.
DEFINE VARIABLE bManuell    AS LOG     NO-UNDO.

DEFINE VAR dFraDato AS DATE NO-UNDO.
DEFINE VAR dTilDato AS DATE NO-UNDO.
DEF VAR dlDato AS DATE NO-UNDO.
DEF VAR cButListe AS CHAR NO-UNDO.

DEFINE VARIABLE pdLoopDato  AS DATE    NO-UNDO.
DEFINE VARIABLE lTid        AS INTEGER NO-UNDO.
DEFINE VARIABLE cLinje      AS CHARACTER NO-UNDO.
DEFINE VARIABLE cLoggFil    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cErrLoggFil AS CHARACTER NO-UNDO.

DEFINE VARIABLE ocReturn AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTekst   AS CHARACTER NO-UNDO.
DEFINE VARIABLE bStreamApen  AS LOG NO-UNDO.

DEFINE TEMP-TABLE tmpSalgPrStr NO-UNDO 
    FIELD ButikkNr AS INTEGER    
    FIELD ArtikkelNr AS DECIMAL 
    FIELD StrKode AS INTEGER     
    FIELD Dato AS DATE        
    FIELD Antall AS DECIMAL     
    FIELD InnVerdiKr AS DECIMAL  
    FIELD MvaKr AS DECIMAL      
    FIELD UtverdiKr AS DECIMAL
    FIELD LevNr AS INTEGER  
    FIELD Tid AS INT 
    FIELD TTId AS INT FORMAT ">>9"
    FIELD BongId AS INT FORMAT ">>>>>>>9"
    FIELD BongLinjeNr AS INT 
    FIELD SelgerNr AS DEC FORMAT ">>>>>>>>>>>9"
    FIELD Storl AS CHAR FORMAT "x(10)"
    FIELD RabattKr AS DEC FORMAT "->>,>>>,>>9.99"
    FIELD MomsProc AS DEC FORMAT "->>>9.99"
    FIELD KasseNr AS INT 
    INDEX SalgIdx ButikkNr ArtikkelNr StrKode Dato Tid TTId BongId BongLinjeNr.

DEFINE TEMP-TABLE tmpPoss
    FIELD Antall       AS DECIMAL   DECIMALS 2 FORMAT "->>>>>>>>>9.999"
    FIELD BongLinjeNr  AS INTEGER   FORMAT ">>>>>>>9"
    FIELD BongNr       AS INTEGER   FORMAT ">>>>>>>9"
    FIELD Butik        AS INTEGER   FORMAT ">>>>>9"
    FIELD Dato         AS DATE
    FIELD EAN          AS CHARACTER FORMAT "x(20)"
    FIELD ERPNr        AS CHARACTER FORMAT "x(20)"
    FIELD GjFakturert  AS LOGICAL
    FIELD InnVerdiKr   AS DECIMAL   DECIMALS 2 FORMAT "->>>>>>>>>9.99"
    FIELD KasseNr      AS INTEGER   FORMAT ">>>>>9"
    FIELD Kjedelevert  AS LOGICAL
    FIELD LevFargeKode AS CHARACTER FORMAT "x(40)"
    FIELD LevNr        AS INTEGER   FORMAT ">>>>>9"
    FIELD MellomGrp    AS INTEGER   FORMAT ">>>>>9"
    FIELD Modell       AS CHARACTER FORMAT "x(30)"
    FIELD Mva%         AS DECIMAL   DECIMALS 2 FORMAT "->>>>>9.99"
    FIELD MvaKr        AS DECIMAL   DECIMALS 2 FORMAT "->>>>>>>>>9.99"
    FIELD OmsEksMva    AS DECIMAL   DECIMALS 2 FORMAT "->>>>>>>>>9.99"
    FIELD PSTransId    AS DECIMAL   DECIMALS 2 FORMAT ">>>>>>>>>>>>>>>>>9"
    FIELD RabattKr     AS DECIMAL   DECIMALS 2 FORMAT "->>>>>>>>>9.99"
    FIELD SelgerNavn   AS CHARACTER FORMAT "x(30)"
    FIELD SelgerNr     AS DECIMAL   DECIMALS 2 FORMAT ">>>>>>>>>>>>9"
    FIELD SeqNr        AS INTEGER   FORMAT ">>>>>9"
    FIELD Storrelse    AS CHARACTER FORMAT "x(30)"
    FIELD Tid          AS INTEGER   FORMAT ">>>>>>>9"
    FIELD TransNr      AS DECIMAL   DECIMALS 2 FORMAT ">>>>>>>>>>>>9"
    FIELD TTId         AS INTEGER   FORMAT ">>>9"
    FIELD UtVerdiKr    AS DECIMAL   DECIMALS 2 FORMAT "->>>>>>>>>9.99"
    FIELD Varemerke    AS CHARACTER FORMAT "x(40)"
    FIELD VaremerkeId  AS INTEGER   FORMAT ">>>>>9"
    FIELD Vareomrade   AS INTEGER   FORMAT ">>>>>9"
    FIELD Varetekst    AS CHARACTER FORMAT "x(50)"
    FIELD Vg           AS INTEGER   FORMAT ">>>>>9"
    FIELD VismaNr      AS CHARACTER FORMAT "10"
    INDEX IdxPSSTransID PSTransId.
  
/*
DEFINE TEMP-TABLE tmpSelger LIKE Selger
    FIELD SistSolgtDato AS DATE FORMAT "99/99/9999".
DEFINE TEMP-TABLE tmpButikkSelger LIKE ButikkSelger.
*/
DEFINE TEMP-TABLE tmpSelger
  FIELD Adresse1       AS CHARACTER   FORMAT "x(30)" LABEL "Adresse"
  FIELD Adresse2       AS CHARACTER   FORMAT "X(30)" LABEL "Adresse"
  FIELD AnsattDato     AS DATE        LABEL "Ansatt dato"
  FIELD AnsattNr       AS CHARACTER   FORMAT "X(15)" LABEL "Ansattnummer" COLUMN-LABEL "AnsattNr"
  FIELD ArbeidsProsent AS DECIMAL     DECIMALS 2 FORMAT ">>>>9"
  FIELD BrukerID       AS CHARACTER   FORMAT "X(10)" LABEL "Bruker"
  FIELD BrukeridPRS    AS CHARACTER   FORMAT "X(15)" LABEL "PRS bruker"
  FIELD ButikkNr       AS INTEGER     FORMAT ">>>>>9" LABEL "Butikknr" COLUMN-LABEL "ButNr"
  FIELD EDato          AS DATE        FORMAT "99/99/9999" LABEL "Endret"
  FIELD ETid           AS INTEGER     LABEL "Endret tid" COLUMN-LABEL "ETid"
  FIELD FastLonn       AS DECIMAL     DECIMALS 2 FORMAT "->>>,>>9.99" LABEL "Fast månedslønn"
  FIELD FodtDato       AS DATE        LABEL "Født"
  FIELD ForNavn        AS CHARACTER   FORMAT "X(30)" LABEL "Fornavn"
  FIELD JobTittel      AS CHARACTER   FORMAT "X(30)" LABEL "Tittel"
  FIELD LonnProfil     AS CHARACTER   FORMAT "X(4)" LABEL "Lønnsprofil"
  FIELD Mobiltelefon   AS CHARACTER   FORMAT "X(15)" LABEL "Mobiltelefon"
  FIELD Navn           AS CHARACTER   FORMAT "X(30)" LABEL "Navn"
  FIELD NavnIKasse     AS CHARACTER   FORMAT "X(15)" LABEL "Navn i kasse"
  FIELD PersonNr       AS DECIMAL     DECIMALS 0 FORMAT "zzzzzzzzzz9" LABEL "PersonNr"
  FIELD PostNr         AS CHARACTER   FORMAT "X(10)" LABEL "PostNr"
  FIELD RegistrertAv   AS CHARACTER   FORMAT "X(10)" LABEL "Registrert av" COLUMN-LABEL "Reg.Av"
  FIELD RegistrertDato AS DATE        FORMAT "99/99/9999" LABEL "Registrert dato" COLUMN-LABEL "RDato"
  FIELD RegistrertTid  AS INTEGER     LABEL "Registreringstidspunkt"
  FIELD SelgerNr       AS DECIMAL     DECIMALS 2 FORMAT ">>>>>>>>>>>>9" LABEL "Selgernummer" COLUMN-LABEL "SelgerNr"
  FIELD SistSolgtDato  AS DATE        FORMAT "99/99/9999" LABEL "Sist solgt dato"
  FIELD SluttetDato    AS DATE        LABEL "Sluttet dato"
  FIELD Telefon        AS CHARACTER   FORMAT "x(15)" LABEL "Telefon"
  FIELD TimeLonn       AS DECIMAL     DECIMALS 2 LABEL "Timelønn"
    .
DEFINE TEMP-TABLE tmpButikkSelger
  FIELD BrukerID       AS CHARACTER   FORMAT "X(10)" LABEL "Bruker"
  FIELD ButikkNr       AS INTEGER     FORMAT ">>>>>9" LABEL "Butikk"
  FIELD EDato          AS DATE        FORMAT "99/99/9999" LABEL "Endret"
  FIELD ETid           AS INTEGER     LABEL "Endret tid" COLUMN-LABEL "ETid"
  FIELD RegistrertAv   AS CHARACTER   FORMAT "X(10)" LABEL "Registrert av" COLUMN-LABEL "Reg.Av"
  FIELD RegistrertDato AS DATE        FORMAT "99/99/9999" LABEL "Registrert dato" COLUMN-LABEL "RDato"
  FIELD RegistrertTid  AS INTEGER     LABEL "Registreringstidspunkt"
  FIELD SelgerId       AS INTEGER     FORMAT ">>>9" LABEL "SelgerId"
  FIELD SelgerNr       AS DECIMAL     DECIMALS 2 FORMAT ">>>>>>>>>>>>9" LABEL "Selgernummer" COLUMN-LABEL "SelgerNr"
    .

DEFINE BUFFER bufPOSS FOR tmpPOSS.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-Ping) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD Ping Procedure 
FUNCTION Ping RETURNS LOGICAL
    ( INPUT cHost AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

ASSIGN
    dFraDato = TODAY - 10
    dTilDato = TODAY
    cIpAdr = '192.168.200.2'
    cConnectionString = "-H " + cIpAdr + " -AppService asbroker1"
    cButListe = '' /* Skal være blank. Brukes bare for TEST */
    .

RUN testConnect(OUTPUT obOk).
IF obOk = FALSE THEN
DO:
    RUN bibl_loggDbFri.p ('send_PRSStg_POSS_To_Hk', 'AppServer Fikk ikke kontakt med AppServer.'). 
    RETURN.
END.
ELSE 
PREP_OG_SEND_DATA:    
DO:
/* Bygger liste over butikker som skal sende data. */
IF cButListe = '' THEN
FOR EACH Butiker NO-LOCK WHERE
  /*Butiker.Butik =  24 AND*/
  Butiker.harButikksystem = TRUE AND
  Butiker.ApningsDato     <> ?:
  IF NOT CAN-DO(cButListe,STRING(Butiker.Butik)) THEN 
      cButListe = cButListe + 
                  (IF cButListe <> '' THEN ',' ELSE '') + 
                  STRING(Butiker.Butik).
END. 
RUN bibl_loggDbFri.p ('send_PRSStg_POSS_To_Hk', 'Butikkliste ' + cButListe + '.').

DATOBLOKK:
DO dlDato = dFraDato TO dTilDato:
    EMPTY TEMP-TABLE tmpPoss.
    EMPTY TEMP-TABLE tmpSalgPrStr.
    
    RUN prepPOSS (dlDato).
    RUN prepSelger.
    
    CREATE SERVER hServer. 
    lConnected = hServer:CONNECT(cConnectionString) NO-ERROR.
    IF lConnected THEN DO: 
        RUN bibl_loggDbFri.p ('send_PRSStg_POSS_To_Hk', 'START SendData for ' + STRING(dlDato) + '.'). 

        /*        RUN asPrsStg.p ON SERVER hServer (INPUT TABLE tmpPoss, OUTPUT obOk).*/
        RUN asPrsStg2.p ON SERVER hServer (INPUT TABLE tmpPoss, INPUT TABLE tmpSelger, INPUT TABLE tmpButikkSelger, OUTPUT obOk).

        RUN bibl_loggDbFri.p ('send_PRSStg_POSS_To_Hk', 'FERDIG SendData' 
                              ).
        hServer:DISCONNECT().
    END.
    ELSE DO: 
        RUN bibl_loggDbFri.p ('send_PRSStg_POSS_To_Hk', 'Feil ved CONNECT. Avbrud.'). 
        obOk = FALSE.
        LEAVE DATOBLOKK.
    END.
    DELETE OBJECT hServer. 
END. /* DATOBLOKK */

END. /* PREP_OG_SEND_DATA */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-prepPOSS) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE prepPOSS Procedure 
PROCEDURE prepPOSS :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes: 
------------------------------------------------------------------------------*/
DEF INPUT PARAMETER idDato AS DATE NO-UNDO.

DEFINE VARIABLE iButikkNr       AS INT  NO-UNDO.
DEFINE VARIABLE cFilNavn        AS CHAR NO-UNDO.
DEFINE VARIABLE iAnt            AS INT  NO-UNDO.
DEFINE VARIABLE cSistSolgtDato  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEAN            AS CHAR NO-UNDO.
DEFINE VARIABLE lPSTransId      AS DECIMAL NO-UNDO.
DEFINE VARIABLE cLoggFil        AS CHARACTER NO-UNDO.
DEFINE VARIABLE cErrLoggFil     AS CHARACTER NO-UNDO.
DEFINE VARIABLE iLoop           AS INTEGER NO-UNDO.

{syspara.i 5 1 1 iButikkNr INT}

ASSIGN
    cLoggFil    = 'PRSStg_eksport'
    cErrLoggFil = 'PRSStg_eksportERR'
    .

lPSTransId = 0.
iAnt = 0.

BUTLISTEBLOKK:
DO iLoop = 1 TO NUM-ENTRIES(cButListe):
    BUTIKKLOOP:
    FOR EACH Butiker NO-LOCK WHERE
      Butiker.Butik = INT(ENTRY(iLoop,cButListe)): 

    RUN bibl_loggDbFri.p ('send_PRSStg_Poss_To_Hk', 'preptmpPoss: ' 
                          + STRING(Butiker.Butik) + ' '
                          + STRING(idDato)
                          ).

    LOOPEN:
    FOR EACH TransLogg NO-LOCK WHERE
      TransLogg.Butik      = Butiker.Butik AND
      TransLogg.Dato       = idDato
      BREAK 
        BY Translogg.Butik
        BY TransLogg.Dato:

      IF NOT CAN-FIND( FIRST tmpPoss WHERE
        tmpPoss.Butik   = TransLogg.Butik AND 
        tmpPoss.TransNr = TransLogg.TransNr AND 
        tmpPoss.SeqNr   = TransLogg.SeqNr) THEN 
      MORGENGRY:
      DO:
        /* INIT */
        FIND ArtBas NO-LOCK WHERE ArtBas.ArtikkelNr = TransLogg.ArtikkelNr NO-ERROR.
        IF AVAILABLE ArtBas THEN
          DO: 
              FIND FIRST ArtPris OF ArtBas NO-LOCK NO-ERROR.
              FIND VarGr NO-LOCK WHERE VarGr.Vg = ArtBas.Vg NO-ERROR. 
              IF AVAILABLE VarGr THEN FIND FIRST Moms NO-LOCK WHERE Moms.MomsKod = VarGr.MomsKod NO-ERROR.
              FIND Varemerke NO-LOCK WHERE
                  VareMerke.VmId = ArtBas.VmId NO-ERROR.
              FIND Regnskapsavdeling NO-LOCK WHERE
                  Regnskapsavdeling.RAvdNr = ArtBas.RavdNr NO-ERROR.
          END.

        FIND Selger NO-LOCK WHERE
              Selger.SelgerNr = TransLogg.SelgerNr NO-ERROR.
        FIND BongLinje NO-LOCK WHERE
              BongLinje.ButikkNr = TransLogg.Butik AND
              BongLinje.GruppeNr = 1 AND
              BongLinje.KasseNr  = TransLogg.KassaNr AND
              BongLinje.Dato     = TransLogg.Dato AND
              BongLinje.BongNr   = TransLogg.BongId AND
              BongLinje.LinjeNr  = TransLogg.BongLinjeNr NO-ERROR.
        IF AVAILABLE Strekkode THEN RELEASE Strekkode.
          cEAN = ''.
        IF AVAILABLE BongLinje THEN
          DO:
              cEAN = BongLinje.Strekkode. 
              RUN bibl_chkean.p (INPUT-OUTPUT cEAN).
              FIND Strekkode NO-LOCK WHERE 
                  Strekkode.Kode = cEAN NO-ERROR.
          END.
        /* INIT Slutt */

        IF lPSTransId = 0 THEN 
        DO:
            FIND LAST bufPoss USE-INDEX IdxPSSTransID NO-ERROR.
            IF AVAILABLE bufPoss THEN 
              lPSTransId = bufPoss.PSTransId + 1.
            ELSE 
              lPSTransId = 1.
        END.
        ELSE lPSTransId = lPSTransId + 1.
        EVIGHET:
        DO WHILE TRUE:
            IF CAN-FIND(FIRST tmpPoss WHERE 
                        tmpPoss.PSTransId = lPSTransId) THEN 
            DO:
               lPSTransId = lPSTransId + 1.
               NEXT EVIGHET.
            END.         
            ELSE DO:
                CREATE tmpPoss.
                ASSIGN
                    tmpPoss.PSTransId = lPSTransId
                    tmpPoss.Butik     = TransLogg.Butik 
                    tmpPoss.TransNr   = TransLogg.TransNr 
                    tmpPoss.SeqNr     = TransLogg.SeqNr
                    NO-ERROR.
                IF ERROR-STATUS:ERROR THEN 
                DO: 
                    IF AVAILABLE tmpPoss THEN DELETE tmpPoss.
                    lPSTransId = lPSTransId + 1.
                    NEXT EVIGHET.
                END. 
                ELSE LEAVE EVIGHET.
            END.
        END. /* EVIGHET */

        IF AVAILABLE tmpPoss THEN 
        ASSIGN
            iAnt              = IAnt + 1
            tmpPoss.TTId         = TransLogg.TTId                                                                                                          
            tmpPoss.VismaNr      = STRING(TransLogg.ArtikkelNr) + TRIM(STRING(IF AVAILABLE StrKonv THEN StrKonv.StrKode ELSE 0,">>999"))                   
            tmpPoss.EAN          = (IF AVAILABLE BongLinje THEN BongLinje.StrekKode ELSE '')                                                               
            tmpPoss.ERPNr        = (IF AVAILABLE Strekkode THEN Strekkode.ERPNr ELSE '')                                                                   
            tmpPoss.Dato         = TransLogg.Dato                                                                                                          
            tmpPoss.Antall       = TransLogg.Antall                                                                                                        
            tmpPoss.InnVerdiKr   = ROUND((Translogg.VVareKost * TransLogg.Antall),2)                                                                       
            tmpPoss.MvaKr        = ROUND(TransLogg.Mva * TransLogg.Antall,2)                                                                               
            tmpPoss.UtVerdiKr    = ROUND((TransLogg.Pris * TransLogg.Antall) - (TransLogg.RabKr * TransLogg.Antall),2)                                     
            tmpPoss.LevNr        = Translogg.LevNr                                                                                                         
            tmpPoss.Vg           = Translogg.Vg   
            tmpPoss.OmsEksMva    = ROUND((TransLogg.Pris * TransLogg.Antall) - (TransLogg.RabKr * TransLogg.Antall) - (TransLogg.Mva * TransLogg.Antall),2)
            tmpPoss.Varetekst    = REPLACE(Translogg.Bongtekst,';',' ')                                                                                    
            tmpPoss.LevFargeKode = IF AVAILABLE ArtBas THEN REPLACE(ArtBas.LevFargKod,';',' ') ELSE ''                                                     
            tmpPoss.Modell       = IF AVAILABLE ArtBas THEN REPLACE(ArtBas.LevKod,';',' ') ELSE ''                                                         
            tmpPoss.MellomGrp    = IF AVAILABLE VarGr THEN VarGr.Hg ELSE 0                                                                        
            tmpPoss.Kjedelevert  = IF AVAILABLE ArtBas THEN ArtBas.KjedeVare ELSE FALSE                                                                
            tmpPoss.GjFakturert  = IF AVAILABLE ArtBas THEN ArtBas.Gjennomfaktureres ELSE FALSE                                                        
            tmpPoss.Storrelse    = REPLACE(TransLogg.Storl,';',' ')                                                                                        
            tmpPoss.VaremerkeId  = IF AVAILABLE ArtBas THEN ArtBas.VmId ELSE 0                                                                    
            tmpPoss.Varemerke    = REPLACE((IF AVAILABLE VareMerke THEN VareMerke.Beskrivelse ELSE ''),';',' ')                                            
            tmpPoss.Vareomrade   = IF AVAILABLE ArtBas THEN ArtBas.RAvdNr ELSE 0                                                                  
            tmpPoss.Mva%         = (IF AVAILABLE Moms THEN Moms.MomsProc ELSE 0)                                                                           
            tmpPoss.RabattKr     = (TransLogg.RabKr * TransLogg.Antall)                                                                                    
            tmpPoss.BongNr       = TransLogg.BongId                                                                                                        
            tmpPoss.BongLinjeNr  = TransLogg.BongLinjeNr                                                                                                   
            tmpPoss.Tid          = TransLogg.Tid                                                                                                           
            tmpPoss.SelgerNr     = TransLogg.SelgerNr                                                                                                      
            tmpPoss.SelgerNavn   = (IF AVAILABLE Selger THEN Selger.Navn ELSE '')                                                                          
            tmpPoss.KasseNr      = TransLogg.KassaNr                                                                                                            
            NO-ERROR.
        IF ERROR-STATUS:ERROR THEN 
          DO:
            IF AVAILABLE tmpPoss THEN DELETE tmpPoss.  
          END.
      END. /* MORGENGRY */ 

    END. /* LOOPEN */

    END. /* BUTIKKLOOP */
END. /* BUTLISTEBLOKK*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

PROCEDURE prepSelger:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
EMPTY TEMP-TABLE tmpSelger.
EMPTY TEMP-TABLE tmpButikkSelger.

FOR EACH Selger NO-LOCK:
    CREATE tmpSelger.
    BUFFER-COPY Selger TO tmpSelger.
    
END.
FOR EACH ButikkSelger NO-LOCK:
    CREATE tmpButikkSelger.
    BUFFER-COPY ButikkSelger TO tmpButikkSelger.
END.

END PROCEDURE.

&IF DEFINED(EXCLUDE-testConnect) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE testConnect Procedure 
PROCEDURE testConnect :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF OUTPUT PARAMETER obOk AS LOG NO-UNDO.

/*IF DYNAMIC-FUNCTION('Ping',cIpAdr) THEN*/
DO:
    CREATE SERVER hServer. 
    lConnected = hServer:CONNECT(cConnectionString) NO-ERROR.
    IF lConnected THEN DO: 
        hServer:DISCONNECT().
        obOk = TRUE.
    END.
    ELSE obOk = FALSE.
    DELETE OBJECT hServer. 
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-Ping) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION Ping Procedure 
FUNCTION Ping RETURNS LOGICAL
    ( INPUT cHost AS CHAR ) :
   /*------------------------------------------------------------------------------
     Purpose:  
       Notes:  
   ------------------------------------------------------------------------------*/
       /*
       DEFINE VARIABLE strMachineName AS CHARACTER   NO-UNDO.
       DEFINE VARIABLE ipHost         AS System.Net.IPHostEntry NO-UNDO . 
       DEFINE VARIABLE ipAddr         AS System.Net.IPAddress NO-UNDO EXTENT . 
       DEFINE VARIABLE ping           AS System.Net.NetworkInformation.Ping      NO-UNDO. 
       DEFINE VARIABLE pingReply      AS System.Net.NetworkInformation.PingReply NO-UNDO.
       DEFINE VARIABLE pingTimeOut    AS INT INIT 120 NO-UNDO.
       DEFINE VARIABLE lStatus        AS LOGICAL NO-UNDO. 

       IF cHost = "localhost"  THEN
            strMachineName = System.Net.Dns:GetHostName().
       ELSE strMachineName = cHost.

       ipHost = System.Net.Dns:GetHostByName(strMachineName) NO-ERROR.
       IF ipHost = ? THEN RETURN FALSE. 
       ipAddr = ipHost:AddressList .
       ping = NEW System.Net.NetworkInformation.Ping().

       /* MESSAGE strMachineName SKIP ipHost:ToString() SKIP ipAddr[1]:ToString() VIEW-AS ALERT-BOX INFO BUTTONS OK. */
       pingReply = ping:SEND(ipAddr[1]:ToString(),pingTimeOut) NO-ERROR.
       /* MESSAGE PingReply:STATUS PingReply:RoundTripTime VIEW-AS ALERT-BOX. */ 

       lStatus = STRING(PingReply:STATUS) = "Success". 
       DELETE OBJECT ping NO-ERROR. 
       IF lStatus THEN RETURN TRUE. ELSE RETURN FALSE. 
       */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

