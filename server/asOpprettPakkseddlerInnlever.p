/* opprettPakkseddlerInnlever.p */

DEFINE INPUT  PARAMETER TABLE-HANDLE hTable.
DEFINE INPUT  PARAMETER cEkstId  AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER cPkSdlNr AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER bEtikett AS LOG NO-UNDO.
DEFINE OUTPUT PARAMETER fPkSdlId AS DECIMAL FORMAT ">>>>>>>>>>>>9" NO-UNDO.
DEFINE OUTPUT PARAMETER ocReturn AS CHARACTER NO-UNDO. 

DEFINE VARIABLE iCL         AS INTEGER NO-UNDO.
DEFINE VARIABLE iCLProfilNr AS INTEGER NO-UNDO.
DEFINE VARIABLE iLevNr      AS INTEGER NO-UNDO.
DEFINE VARIABLE iAntLinjer  AS INTEGER NO-UNDO.
DEFINE VARIABLE bStdPrisOverf AS LOG   NO-UNDO.
DEFINE VARIABLE lRab%       AS DECIMAL NO-UNDO.
DEFINE VARIABLE iOutlet     AS INTEGER NO-UNDO.
DEFINE VARIABLE stdRab%     AS DECIMAL INITIAL 30 NO-UNDO.
DEFINE VARIABLE lB_Id       AS DECIMAL NO-UNDO.
DEFINE VARIABLE hQuery      AS HANDLE  NO-UNDO.
DEFINE VARIABLE iButikkNr   AS INTEGER NO-UNDO.
DEFINE VARIABLE ihBuffer    AS HANDLE  NO-UNDO.
DEFINE VARIABLE obOk        AS LOG     NO-UNDO.
DEFINE VARIABLE cOutletLst  AS CHARACTER NO-UNDO.

DEFINE VARIABLE lPrisRab%   AS DECIMAL   NO-UNDO.
DEFINE VARIABLE lforhRab%   AS DECIMAL   NO-UNDO.
DEFINE VARIABLE cEDB-System AS CHARACTER INITIAL 'Gant Global' NO-UNDO.
DEFINE VARIABLE fMvaKr      AS DECIMAL   NO-UNDO.
DEFINE VARIABLE fDbKr       AS DECIMAL   NO-UNDO.

DEFINE BUFFER clButiker FOR Butiker.
DEFINE BUFFER outButiker FOR Butiker.

ihBuffer = hTable:DEFAULT-BUFFER-HANDLE.

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
{syspar2.i 22 20 1 iOutlet INT}
{syspara.i 22 5 2 cOutletLst}

CREATE QUERY hQuery.
hQuery:SET-BUFFERS(ihBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + ihBuffer:NAME + " NO-LOCK").
hQuery:QUERY-OPEN().

hQuery:GET-FIRST().
ASSIGN 
    lB_Id     = DEC(ihBuffer:BUFFER-FIELD("B_Id"):BUFFER-VALUE)
    iButikkNr = DEC(ihBuffer:BUFFER-FIELD("ButikkNr"):BUFFER-VALUE).

RUN bibl_loggDbFri.p ('PakkseddelInnlevFraKasse', 'opprettPakkseddlerInnlever.p: ' 
                               + ' Butikk: '     + STRING(iButikkNr)
                               + ' BongNr: '     + TRIM(STRING(iButikkNr))
                               + ' PakkseddelNr: '  + TRIM(cPkSdlNr)
                               + ' OrdreNr: '       + TRIM(cEkstId)
                               ).

IF iOutlet > 0 THEN 
  FIND outButiker NO-LOCK WHERE
    outButiker.Butik = iOutlet NO-ERROR.

FIND FIRST ImpKonv NO-LOCK WHERE 
    ImpKonv.EDB-System = cEDB-System AND 
    ImpKonv.Tabell     = 'Def.Rab%' AND 
    ImpKonv.EksterntId = STRING(iOutlet) NO-ERROR.
IF AVAILABLE ImpKonv 
    THEN ASSIGN 
        lforhRab%      = DEC(ImpKonv.Merknad)
        lPrisRab%      = DEC(ImpKonv.InterntId)
        . 

FUNCTION FixChk RETURNS CHARACTER
    ( INPUT cKode AS CHARACTER )  FORWARD.

FUNCTION FixStorl RETURNS CHARACTER 
	( pcStorl AS CHAR ) FORWARD.

RUN OpprettPakksedler.

/*RUN asEtikett ( iButikkNr, 1, TABLE-HANDLE hTable , OUTPUT obOk).*/
RUN asPakkseddel (iButikkNr, cPkSdlNr, FALSE, bEtikett, OUTPUT obOk, OUTPUT ocReturn).  
  

/* **********************  Internal Procedures  *********************** */

PROCEDURE OpprettPakksedler:
/*------------------------------------------------------------------------------
		Purpose:  																	  
		Notes:  																	  
------------------------------------------------------------------------------*/
DEFINE VARIABLE cButikkLst AS CHARACTER NO-UNDO.

DEF VAR cPakkseddelLst  AS CHAR   NO-UNDO.
DEF VAR cOrdreLst       AS CHAR   NO-UNDO.
DEF VAR piLoop     AS INT    NO-UNDO.
DEF VAR pi2Loop    AS INT    NO-UNDO.
DEF VAR cOVreFil   AS CHAR   NO-UNDO.
DEF VAR iTelleNr   AS INT    NO-UNDO.
DEF VAR iHtFilId   AS INT    NO-UNDO.
DEF VAR iParaNr    AS INT    NO-UNDO.
DEFINE VARIABLE fMvaKr AS DECIMAL NO-UNDO.
DEFINE VARIABLE fDbKr  AS DECIMAL NO-UNDO.

DEFINE VARIABLE iLnr          AS INTEGER NO-UNDO.
DEFINE VARIABLE fPkSdlLinjeId AS DECIMAL.

DEFINE BUFFER clArtPris FOR ArtPris.
DEFINE BUFFER bSysPara FOR SysPara.
DEFINE VARIABLE cStorl AS CHARACTER NO-UNDO.

BUTIKKLOOP:
DO:
    FIND Butiker NO-LOCK WHERE
      Butiker.Butik = iButikkNr NO-ERROR.

    ASSIGN
      fPkSdlId      = 0
      iLnr          = 0
      fPkSdlLinjeId = 0.

    /* Oppretter Pakkseddel for varemottak */
    PKSDLHODE:
    DO:
        FIND LAST PkSdlHode NO-LOCK NO-ERROR.
        CREATE PkSdlHode.
        ASSIGN PkSdlHode.PkSdlStatus    = 10
               PkSdlHode.SendtDato      = TODAY
               fPkSdlId                 = PkSdlHode.PkSdlId
               PkSdlHode.Merknad        = "Innlevert i kasse"
               PkSdlHode.CL             = iCl
               PkSdlHode.PkSdlNr        = cPkSdlNr
               PkSdlHode.EkstId         = cEkstId
               PkSdlHode.LevNr          = iLevNr
               PkSdlHode.PkSdlOpphav    = (IF CAN-DO(cOutletLst,STRING(iButikkNr)) 
                                               THEN 5
                                           ELSE 1)
               .
    END. /* PKSDLHODE */

    /* Setter linjeId. */
    FIND LAST PkSdlLinje NO-LOCK
         WHERE PkSdlLinje.PkSdlId = fPkSdlId
         NO-ERROR.
    fPkSdlLinjeId = IF AVAIL PkSdlLinje THEN PkSdlLinje.PkSdlLinjeId + 1 ELSE 1.

    hQuery:GET-FIRST().
    OPPRETT_LINJER: 
    REPEAT WHILE NOT hQuery:QUERY-OFF-END:
        RUN bibl_loggDbFri.p ('PakkseddelInnlevFraKasse', 'opprettPakkseddlerInnlever.p: ' 
                               + ' ArtikkelNr: '     + STRING(DEC(ihBuffer:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE))
                               + ' Størrelse: '  + STRING(ihBuffer:BUFFER-FIELD("Storrelse"):BUFFER-VALUE)
                               + ' Strekkode: '  + STRING(ihBuffer:BUFFER-FIELD("Strekkode"):BUFFER-VALUE)
                               + ' Makulert: '  + STRING(ihBuffer:BUFFER-FIELD("Makulert"):BUFFER-VALUE)
                               + ' TTId: '  + STRING(ihBuffer:BUFFER-FIELD("TTId"):BUFFER-VALUE)
                               + ' Antall: ' + STRING(ihBuffer:BUFFER-FIELD("Antall"):BUFFER-VALUE)
                               ).

        IF ihBuffer:BUFFER-FIELD("Makulert"):BUFFER-VALUE = TRUE OR 
           INT(ihBuffer:BUFFER-FIELD("TTId"):BUFFER-VALUE) <> 5 THEN. /* Gjør ingenting */
        ELSE 
        BLOKKEN: 
        DO: 
            FIND ArtBas NO-LOCK WHERE
              ArtBas.ArtikkelNr = DEC(ihBuffer:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE) NO-ERROR.
            IF NOT AVAILABLE ArtBas THEN
            DO:
              RUN bibl_loggDbFri.p ('PakkseddelInnlevFraKasse', 'opprettPakkseddlerInnlever.p: ' 
                                   + ' Ukjent artikkel ArtikkelNr: '     + STRING(DEC(ihBuffer:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE))
                               ).
              LEAVE BLOKKEN.
            END.

            FIND FIRST Strekkode NO-LOCK WHERE
              Strekkode.Kode = STRING(ihBuffer:BUFFER-FIELD("Strekkode"):BUFFER-VALUE) NO-ERROR.
            IF NOT AVAILABLE Strekkode THEN 
            DO:
                cStorl = TRIM(STRING(ihBuffer:BUFFER-FIELD("Storrelse"):BUFFER-VALUE)).
                cStorl = FixStorl(cStorl).
                FIND FIRST StrKonv NO-LOCK WHERE
                    StrKonv.Storl = cStorl NO-ERROR.
                IF NOT AVAILABLE StrKonv THEN 
                DO:
                  RUN bibl_loggDbFri.p ('PakkseddelInnlevFraKasse', 'opprettPakkseddlerInnlever.p: ' 
                                       + ' Ukjent størrelse på ArtikkelNr: '     + STRING(DEC(ihBuffer:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE))
                                       + ' Størrelse: '  + STRING(ihBuffer:BUFFER-FIELD("Storrelse"):BUFFER-VALUE)
                                       ).
                END.
                ELSE DO:

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
                END.
            END.

            CREATE PkSdlLinje.
            ASSIGN iLnr                     = iLnr + 1 
                   PkSdlLinje.Linjenr       = iLnr
                   PkSdlLinje.PkSdlLinjeId  = fPkSdlLinjeId
                   PkSdlLinje.PkSdlId       = fPkSdlId
                   PkSdlLinje.ArtikkelNr    = (IF AVAILABLE ArtBas THEN ArtBas.ArtikkelNr ELSE 0)
                   PkSdlLinje.BestNr        = 0
                   PkSdlLinje.OrdreNr       = 0
                   PkSdlLinje.Beskr         = (IF AVAILABLE ArtBas THEN ArtBas.Beskr ELSE '')
                   PkSdlLinje.LevFargKod    = (IF AVAILABLE ArtBas THEN ArtBas.LevFargKod ELSE '')
                   PkSdlLinje.Antall        = DEC(STRING(ihBuffer:BUFFER-FIELD("Antall"):BUFFER-VALUE))
                   PkSdlLinje.AntLevert     = DEC(STRING(ihBuffer:BUFFER-FIELD("Antall"):BUFFER-VALUE))
                   PkSdlLinje.LevKod        = ArtBas.LevKod
                   PkSdlLinje.LevNr         = ArtBas.LevNr
                   PkSdlLinje.StrKode       = (IF AVAILABLE Strekkode THEN Strekkode.StrKode ELSE 0)
                   PkSdlLinje.Kode          = (IF AVAILABLE Strekkode THEN Strekkode.Kode ELSE '')
                   PkSdlLinje.Salgsenhet    = ArtBas.SalgsEnhet
                   PkSdlLinje.ButikkNr      = INT(STRING(ihBuffer:BUFFER-FIELD("ButikkNr"):BUFFER-VALUE))
                   PkSdlLinje.Pakke         = FALSE 
                   PkSdlLinje.PakkeNr       = 0
                   fPkSdlLinjeId            = fPkSdlLinjeId + 1
                   .
            /* Oppretter pakkseddel pris */
            FIND FIRST ArtPris NO-LOCK
                 WHERE ArtPris.ArtikkelNr = ArtBas.ArtikkelNr
                   AND ArtPris.ProfilNr   = Butiker.ProfilNr NO-ERROR.
            IF NOT AVAILABLE ArtPris THEN 
                FIND FIRST ArtPris NO-LOCK
                     WHERE ArtPris.ArtikkelNr = ArtBas.ArtikkelNr
                     AND ArtPris.ProfilNr     = iClProfilNr NO-ERROR.
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
                   PkSdlPris.NyPris         = ArtPris.Pris[1] 
                   PkSdlPris.NyVarekost     = ArtPris.VareKost[1]
                   PkSdlPris.NyRab1%        = 0
                   PkSdlPris.NyInnkjopsPris = ArtPris.VareKost[1]
                   PkSdlPris.NyFrakt        = 0
                   fMvaKr                   = PkSdlPris.NyPris - (PkSdlPris.NyPris / (1 + (ArtPris.Mva%[1] / 100)))
                   fDbKr                    = PkSdlPris.NyPris - fMvaKr - PkSdlPris.NyVarekost                   
                   PkSdlPris.NyDB%          = ROUND((fDbKr * 100) / (PkSdlPris.NyPris - fMvaKr),2)
                   PkSdlPris.NyDB%          = IF PkSdlPris.NyDB% = ? THEN 0 ELSE PkSdlPris.NyDB%
                   PkSdlPris.OverstyrPris   = YES
                   .
            /*Outlet pris. */
            /* - Outlet skal ha lokal pris hvis rabatt% avviker.     */
            /* - Outlet skal ha 30% rabatt også på utpris med utgangspunkt.           */
            /* - Outlet skal alltid ha minst 30% rabatt.             */
            /* - Står det allerede 30% eller mer, gjør vi ingenting. */
            IF INT(STRING(ihBuffer:BUFFER-FIELD("ButikkNr"):BUFFER-VALUE)) = iOutlet AND AVAILABLE outButiker AND PkSdlPris.NyRab1% < lforhRab% THEN 
            DO:
                FIND FIRST clArtPris NO-LOCK
                     WHERE clArtPris.ArtikkelNr = ArtBas.ArtikkelNr
                     AND clArtPris.ProfilNr     = iClProfilNr NO-ERROR.
                ASSIGN 
                   /* Ny rabattert pris som skal gjelde i butikken */
                   PkSdlPris.NyPris         = ROUND(clArtPris.Pris[1] - (clArtPris.Pris[1] * lPrisRab% / 100),2) 
                   PkSdlPris.NyInnkjopsPris = ArtPris.InnkjopsPris[1]
                   PkSdlPris.NyRab1%        = lforhRab%
                   PkSdlPris.NyVarekost     = ROUND(ArtPris.InnkjopsPris[1] - (ArtPris.InnkjopsPris[1] * lforhRab% / 100),2)
                   fMvaKr                   = PkSdlPris.NyPris - (PkSdlPris.NyPris / (1 + (ArtPris.Mva%[1] / 100)))
                   fDbKr                    = PkSdlPris.NyPris - fMvaKr - PkSdlPris.NyVarekost                   
                   PkSdlPris.NyDB%          = ROUND((fDbKr * 100) / (PkSdlPris.NyPris - fMvaKr),2)
                   PkSdlPris.NyDB%          = IF PkSdlPris.NyDB% = ? THEN 0 ELSE PkSdlPris.NyDB%
                   .              
            END.
        END. /* BLOKKEN */
        hQuery:GET-NEXT().
    END. /* OPPRETT_LINJER */
    hQuery:QUERY-CLOSE().
    DELETE OBJECT hQuery.
    
    IF CAN-FIND(PkSdlHode WHERE 
                PkSdlHode.PkSdlId = fPkSdlId) THEN 
        RUN PkSdlSetLandedCost.p (STRING(fPkSdlId), ?, '', OUTPUT ocReturn, OUTPUT obOk).
    
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

  ASSIGN cKode = "02" + STRING(ArtBas.ArtikkelNr,"9999999") + STRING(bufStrKonv.StrKode,"999")
         cKode = FixChk(cKode).

  CREATE StrekKode.
  ASSIGN StrekKode.ArtikkelNr = ArtBas.ArtikkelNr
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



/* ************************  Function Implementations ***************** */
FUNCTION FixChk RETURNS CHARACTER
    ( INPUT cKode AS CHARACTER ) :
  /*------------------------------------------------------------------------------
    Purpose:  
      Notes:  
  ------------------------------------------------------------------------------*/
      DEF VAR iCount1 AS INTE NO-UNDO.
      DEF VAR iMulti  AS INTE INIT 1 NO-UNDO.
      DEF VAR iSum AS INTE NO-UNDO.
        DO iCount1 = LENGTH(cKode) TO 1 BY -1:  
            ASSIGN iMulti = IF iMulti = 1 THEN 3 ELSE 1
                   iSum = iSum + INT(SUBSTR(cKode,iCount1,1)) * iMulti.
        END.
        RETURN cKode + string((10 - iSum MODULO 10) MODULO 10).

END FUNCTION.

FUNCTION FixStorl RETURNS CHARACTER 
	    ( pcStorl AS CHAR ):
/*------------------------------------------------------------------------------
		Purpose:  																	  
		Notes:  																	  
------------------------------------------------------------------------------*/	
 ASSIGN
    pcStorl = TRIM(pcStorl)
    pcStorl = CAPS(pcStorl)
    pcStorl = IF (LENGTH(pcStorl) = 1 OR 
                 LENGTH(pcStorl) = 3
                 ) 
                then " " + pcStorl
                else pcStorl.          

  /* Bytter ut eventuelle comma med punkt. */
  IF INDEX(pcStorl,",") <> 0 THEN
    OVERLAY(pcStorl, INDEX(pcStorl,","), 1, "CHARACTER") = ".".

  RETURN pcStorl.   /* Function return value. */
END FUNCTION.
