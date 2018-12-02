
/*
DEFINE VARIABLE iBuntNr          AS INTEGER NO-UNDO.

DEFINE VARIABLE plFaktura_Id     AS DECIMAL NO-UNDO.
DEF VAR lFakturaNr LIKE FakturaHode.FakturaNr NO-UNDO.
DEF VAR lKundeNr AS DEC NO-UNDO.

DEFINE VARIABLE iAntLinjer  AS INTEGER NO-UNDO.

*/

DEFINE VARIABLE iOverskuddslager AS INTEGER NO-UNDO. 
DEFINE VARIABLE iOutlet          AS INTEGER NO-UNDO.
DEFINE VARIABLE iCL              AS INTEGER NO-UNDO.
DEFINE VARIABLE iCLProfilNr      AS INTEGER NO-UNDO.
DEFINE VARIABLE iLevNr           AS INTEGER NO-UNDO.
DEFINE VARIABLE bStdPrisOverf    AS LOG     NO-UNDO.
DEFINE VARIABLE lRab%            AS DECIMAL NO-UNDO.
DEFINE VARIABLE cLogg AS CHARACTER NO-UNDO.

DEFINE BUFFER bufOverButiker FOR Butiker.
DEFINE BUFFER bufOutlButiker FOR Butiker.
DEFINE BUFFER clButiker      FOR Butiker.
DEFINE TEMP-TABLE ttpkSdlLinje LIKE PkSdlLinje.

DEF VAR piLoop        AS INT    NO-UNDO.
DEF VAR cFakturaListe AS CHAR   NO-UNDO.
DEF VAR ocReturn      AS CHAR   NO-UNDO.
DEF VAR obOK          AS LOG    NO-UNDO.
DEF VAR ihBuffer      AS HANDLE NO-UNDO.
DEF VAR cFilNavn      AS CHAR   NO-UNDO.
DEF VAR cLinje        AS CHAR   NO-UNDO.

FUNCTION FixChk RETURNS CHARACTER
    ( INPUT cKode AS CHARACTER )  FORWARD.

iOutlet = 16.
iOverskuddslager = 50.

{syspara.i 5 1 1 iCl INT}
FIND clButiker NO-LOCK WHERE
    clButiker.Butik = iCL NO-ERROR.
IF NOT AVAILABLE clButiker THEN 
    RETURN.
iClProfilNr = clButiker.ProfilNr. 

/* Setter leverandørnnumer som skal benyttes. */
{syspara.i 210 100 1 iLevNr INT}
IF iLevNr = 0 THEN
    iLevNr = 40.

{syspara.i 5 26 1 bStdPrisOverf LOGICAL}
{syspara.i 210 100 4 lRab% DEC}

ASSIGN 
    cLogg = 'finn_faktura_uten_pakkseddel_but16_50' + REPLACE(STRING(TODAY,"99/99/9999"),'/','').
.
        
RUN bibl_loggDbFri.p (cLogg, 
    'Start.'  
    ). 

FIND bufOverButiker NO-LOCK WHERE 
    bufOverButiker.Butik = iOverskuddslager NO-ERROR.  
FIND bufOutlButiker NO-LOCK WHERE 
    bufOutlButiker.Butik = iOutlet NO-ERROR.

IF (NOT AVAILABLE bufOverButiker  OR 
    NOT AVAILABLE bufOutlButiker) THEN 
    RETURN.


/* Løper igjennom alle fakturahoder og finner de overføringer fra 16 til 50 hvor det ikke er */
/* opprettet pakkseddel på butikk 16.                                                        */
FOR EACH FakturaHode NO-LOCK WHERE
    FakturaHode.butikkNr = 16 AND
    FakturaHode.FakturaNr > 0 AND /* 16604594  16604602 */
    FakturaHode.KundeNr = 100177,
    FIRST FakturaLinje OF FakturaHode NO-LOCK WHERE 
    FakturaLinje.BongLinjeNr > 0 AND
    FakturaLinje.TTId = 6,
    FIRST BongLinje NO-LOCK WHERE
    BongLinje.B_Id = FakturaLinje.B_Id AND
    BongLinje.LinjeNr = FakturaLinje.BongLinjeNr AND
    BongLinje.MButikkNr = 50:

    IF FakturaHode.FakturaNr = ? THEN
        NEXT.
    IF CAN-FIND(FIRST PkSdlHode WHERE
        PkSdlHode.PkSdlNr = STRING(FakturaHode.FakturaNr)) THEN
        NEXT.

    RUN bibl_loggDbFri.p (cLogg, 
        'Faktura fra butikk ' + STRING(FakturaHode.butikkNr) + ' ' + 
        'til kunde ' + STRING(FakturaHode.KundeNr) + ' ' + 
        'fakturanr ' + STRING(FakturaHode.FakturaNr) + ' ' + 
        'fakturadato ' + STRING(FakturaHode.FakturertDato) + ' ' +
        'beløp ' + STRING(FakturaHode.Totalt) + ' ' + 
        'overføring til butikk ' + STRING(BongLinje.MButikkNr) + '.'
        ). 

    RUN OpprettPakksedler.
END.

RUN bibl_loggDbFri.p (cLogg, 
    'Ferdig.'  
    ). 

PROCEDURE OpprettPakksedler:
    /*------------------------------------------------------------------------------
            Purpose:                                                                      
            Notes: Forutsetter at fakturaHode er tilgjengelig.                                                                        
    ------------------------------------------------------------------------------*/
    DEF    VAR      cButikkLst     AS CHAR    NO-UNDO.

    DEF    VAR      cPakkseddelLst AS CHAR    NO-UNDO.
    DEF    VAR      cOrdreLst      AS CHAR    NO-UNDO.
    DEF    VAR      piLoop         AS INT     NO-UNDO.
    DEF    VAR      pi2Loop        AS INT     NO-UNDO.
    DEF    VAR      cOVreFil       AS CHAR    NO-UNDO.
    DEF    VAR      iTelleNr       AS INT     NO-UNDO.
    DEF    VAR      iHtFilId       AS INT     NO-UNDO.
    DEF    VAR      iParaNr        AS INT     NO-UNDO.
    DEFINE VARIABLE fMvaKr         AS DECIMAL NO-UNDO.
    DEFINE VARIABLE fDbKr          AS DECIMAL NO-UNDO.

    DEFINE VARIABLE iLnr           AS INTEGER NO-UNDO.
    DEFINE VARIABLE fPkSdlId       AS DECIMAL FORMAT ">>>>>>>>>>>>9".
    DEFINE VARIABLE fPkSdlLinjeId  AS DECIMAL.

    DEF BUFFER bSysPara FOR SysPara.

    BUTIKKLOOP:
    DO:
        FIND Butiker NO-LOCK WHERE
            Butiker.Butik = iOutlet NO-ERROR.

        ASSIGN
            fPkSdlId      = 0
            iLnr          = 0
            fPkSdlLinjeId = 0.

        /* Oppretter Pakkseddel for varemottak */
        PKSDLHODE:
        DO:
            FIND LAST PkSdlHode NO-LOCK NO-ERROR.
            CREATE PkSdlHode.
            ASSIGN 
                PkSdlHode.PkSdlStatus = 10
                PkSdlHode.SendtDato   = TODAY
                fPkSdlId              = PkSdlHode.PkSdlId
                PkSdlHode.Merknad     = "Overf. fra 16 til 50"
                PkSdlHode.CL          = iCl
                PkSdlHode.PkSdlNr     = IF AVAILABLE FakturaHode THEN STRING(FakturaHode.FakturaNr) ELSE ''
                PkSdlHode.EkstId      = IF AVAILABLE FakturaHode THEN STRING(FakturaHode.Faktura_id) ELSE ''
                PkSdlHode.LevNr       = iLevNr
                .
        END. /* PKSDLHODE */ 

        /* Setter linjeId. */
        FIND LAST PkSdlLinje NO-LOCK
            WHERE PkSdlLinje.PkSdlId = fPkSdlId
            NO-ERROR.
        fPkSdlLinjeId = IF AVAIL PkSdlLinje THEN PkSdlLinje.PkSdlLinjeId + 1 ELSE 1.

        OPPRETT_LINJER:                    
        FOR EACH FakturaLinje OF FakturaHode NO-LOCK:

            FIND ArtBas NO-LOCK WHERE
                ArtBas.ArtikkelNr = DEC(FakturaLinje.ArtikkelNr) NO-ERROR.
            IF NOT AVAILABLE ArtBas THEN NEXT.
            FIND FIRST StrKonv NO-LOCK WHERE
                StrKonv.Storl = FakturaLinje.Storl NO-ERROR.
            IF NOT AVAILABLE StrKonv THEN NEXT.

            FIND FIRST Strekkode NO-LOCK WHERE
                Strekkode.ArtikkelNr = ArtBas.ArtikkelNr AND
                Strekkode.StrKode    = StrKonv.StrKode AND 
                LENGTH(Strekkode.Kode) = 13 AND 
                NOT Strekkode.Kode BEGINS '02' 
                NO-ERROR.
            IF NOT AVAILABLE Strekkode THEN 
                FIND FIRST Strekkode NO-LOCK WHERE
                    Strekkode.ArtikkelNr = ArtBas.ArtikkelNr AND
                    Strekkode.StrKode    = StrKonv.StrKode 
                    NO-ERROR.
            IF NOT AVAILABLE Strekkode THEN 
                RUN genEAN (ArtBas.ArtikkelNr,StrKonv.Storl). 
          
            CREATE PkSdlLinje.
            ASSIGN 
                iLnr                    = iLnr + 1 
                PkSdlLinje.Linjenr      = iLnr
                PkSdlLinje.PkSdlLinjeId = fPkSdlLinjeId
                PkSdlLinje.PkSdlId      = fPkSdlId
                PkSdlLinje.ArtikkelNr   = (IF AVAILABLE ArtBas THEN ArtBas.ArtikkelNr ELSE 0)
                PkSdlLinje.BestNr       = 0
                PkSdlLinje.OrdreNr      = 0
                PkSdlLinje.Beskr        = (IF AVAILABLE ArtBas THEN ArtBas.Beskr ELSE '')
                PkSdlLinje.LevFargKod   = (IF AVAILABLE ArtBas THEN ArtBas.LevFargKod ELSE '')
                PkSdlLinje.Antall       = FakturaLinje.Antall
                PkSdlLinje.AntLevert    = FakturaLinje.Antall
                PkSdlLinje.LevKod       = ArtBas.LevKod
                PkSdlLinje.LevNr        = ArtBas.LevNr
                PkSdlLinje.StrKode      = (IF AVAILABLE StrKonv THEN StrKonv.StrKode ELSE 0)
                PkSdlLinje.Kode         = (IF AVAILABLE Strekkode THEN Strekkode.Kode ELSE '')
                PkSdlLinje.Salgsenhet   = ArtBas.SalgsEnhet
                PkSdlLinje.ButikkNr     = iOutLet
                PkSdlLinje.Pakke        = FALSE 
                PkSdlLinje.PakkeNr      = 0
                fPkSdlLinjeId           = fPkSdlLinjeId + 1
                .
        
        
            /* Oppretter pakkseddel pris */
            FIND FIRST ArtPris NO-LOCK
                WHERE ArtPris.ArtikkelNr = ArtBas.ArtikkelNr NO-ERROR.
            FIND PkSdlPris EXCLUSIVE-LOCK WHERE
                PkSdlPris.PkSdlId    = fPkSdlId AND
                PkSdlPris.ArtikkelNr = ArtBas.ArtikkelNr NO-ERROR.
            IF NOT AVAILABLE PkSdlPris THEN 
            DO:
                CREATE PkSdlPris.
                ASSIGN
                    PkSdlPris.PkSdlId    = fPkSdlId
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
                PkSdlPris.NyPris         = ArtPris.Pris[1] - ((ArtPris.Pris[1] * lRab%) / 100) 
                PkSdlPris.NyVarekost     = ArtPris.InnkjopsPris[1] - ((ArtPris.InnkjopsPris[1] * lRab%) / 100)
                PkSdlPris.NyRab1%        = lRab%
                PkSdlPris.NyInnkjopsPris = ArtPris.InnkjopsPris[1]
                PkSdlPris.NyFrakt        = 0
                fMvaKr                   = PkSdlPris.NyPris - (PkSdlPris.NyPris / (1 + (ArtPris.Mva%[1] / 100)))
                fDbKr                    = PkSdlPris.NyPris - fMvaKr - PkSdlPris.NyVarekost                   
                PkSdlPris.NyDB%          = ROUND((fDbKr * 100) / (PkSdlPris.NyPris - fMvaKr),2)
                PkSdlPris.NyDB%          = IF PkSdlPris.NyDB% = ? THEN 0 ELSE PkSdlPris.NyDB%
                PkSdlPris.OverstyrPris   = YES
                .
        END. /* OPPRETT_LINJER */
    
    END. /* BUTIKKLOOP */

END PROCEDURE.

PROCEDURE genEAN :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER plArtikkelNr AS DEC  NO-UNDO.
    DEF INPUT PARAMETER cStorl       AS CHAR NO-UNDO.

    DEF VAR cKode AS CHAR NO-UNDO.
  
    DEFINE BUFFER bufStrKonv FOR StrKonv.
  
    FIND bufStrKonv WHERE bufStrKonv.Storl = cStorl USE-INDEX Storl NO-LOCK NO-ERROR.
    IF NOT AVAIL bufStrKonv THEN
        RETURN.
    /* Finnes det strekkode på størrrelsen fra før, skal vi ikke legge opp ny. */
    IF CAN-FIND(FIRST StrekKode WHERE StrekKode.ArtikkelNr = ArtBas.ArtikkelNr AND
        StrekKode.KodeType = 1 AND
        StrekKode.StrKode  = bufStrKonv.StrKode
        /*  AND StrekKode.Kode BEGINS "02" */
        ) THEN RETURN.

    ASSIGN 
        cKode = "02" + STRING(ArtBas.ArtikkelNr,"9999999") + STRING(bufStrKonv.StrKode,"999")
        cKode = FixChk(cKode).

    CREATE StrekKode.
    ASSIGN 
        StrekKode.ArtikkelNr = ArtBas.ArtikkelNr
        StrekKode.Kode       = cKode
        StrekKode.KodeType   = 1 /* använd inte iKodeType, vi kan ha 0 */
        StrekKode.StrKode    = bufStrKonv.StrKode 
        StrekKode.VareId     = ArtBas.ArtikkelNr
      NO-ERROR.
    /* TN Koden kan finnes fra før - 02 koder gav feilmelding. */
    IF ERROR-STATUS:ERROR THEN
    DO:
        IF AVAILABLE StrekKode THEN
            DELETE StrekKode.
    END.
END PROCEDURE.

FUNCTION FixChk RETURNS CHARACTER
    ( INPUT cKode AS CHARACTER ) :
    /*------------------------------------------------------------------------------
      Purpose:  
        Notes:  
    ------------------------------------------------------------------------------*/
    DEF VAR iCount1 AS INTE NO-UNDO.
    DEF VAR iMulti  AS INTE INIT 1 NO-UNDO.
    DEF VAR iSum    AS INTE NO-UNDO.
    DO iCount1 = LENGTH(cKode) TO 1 BY -1:  
        ASSIGN 
            iMulti = IF iMulti = 1 THEN 3 ELSE 1
            iSum   = iSum + INT(SUBSTR(cKode,iCount1,1)) * iMulti.
    END.
    RETURN cKode + string((10 - iSum MODULO 10) MODULO 10).

END FUNCTION.


