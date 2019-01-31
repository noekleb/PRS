 
 /*------------------------------------------------------------------------
    File        : KundeordreBehandling
    Purpose     : Hensikten med klasen er å gjøre metodene tilgjengelig for andre anheter via AppServer. Slik at f.eks også kassen kan utføre funksjonene.
    Syntax      : 
    Description : Klassen inneholder metoder for håndtering av kundeordre.
    Author(s)   : tny
    Created     : Fri Dec 28 10:11:57 CET 2018
    Notes       : Endret idag.
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.



CLASS cls.Kundeordre.KundeordreBehandling: 
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE cLogg AS CHARACTER NO-UNDO.
    DEFINE VARIABLE bTest AS LOG NO-UNDO.
    DEFINE VARIABLE cTekst AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iLoop AS INTEGER NO-UNDO.
    DEFINE VARIABLE iCL AS INTEGER NO-UNDO.
    DEFINE VARIABLE obOk AS LOG NO-UNDO.
    DEFINE VARIABLE bSkrivPkSdl AS LOG NO-UNDO.

    DEFINE VARIABLE rStandardFunksjoner AS cls.Stdfunk.StandardFunksjoner NO-UNDO.
         
    CONSTRUCTOR PUBLIC KundeordreBehandling (  ):
        SUPER ().
  
        {syspara.i 5 1 1 iCL INT}
        {syspara.i 150 1 21 cTekst}
        IF CAN-DO('1,J,Yes,True',cTekst) THEN
            bSkrivPkSdl = TRUE.
        ELSE 
            bSkrivPkSdl = FALSE. 
        
        ASSIGN
            bTest = TRUE
            cLogg = 'KundeordreBehandling' + REPLACE(STRING(TODAY),'/','')
            .

        rStandardFunksjoner  = NEW cls.stdfunk.StandardFunksjoner(cLogg) NO-ERROR.
        rStandardFunksjoner:SkrivTilLogg(cLogg,
            'Klasse opprettet.'
            ).
        
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC LOGICAL LeverTilKunde( INPUT plKOrdre_Id AS DECIMAL,
                                         INPUT pcBruker AS CHARACTER,
                                         OUTPUT pcReturn AS CHARACTER,
                                         OUTPUT bResult AS LOG ):
        
        DEFINE VARIABLE pbOpprettFaktura AS LOG NO-UNDO.
        DEFINE VARIABLE plDec AS DECIMAL NO-UNDO.
        DEFINE VARIABLE pcPrinter AS CHARACTER NO-UNDO.
        
        {syspara.i 150 1 8 cTekst}
        pbOpprettFaktura = IF cTekst = '1' THEN TRUE ELSE FALSE.

        FIND Bruker NO-LOCK WHERE 
          Bruker.BrukerId = pcBruker NO-ERROR.
        IF AVAILABLE Bruker AND Bruker.Butik > 0 THEN 
        DO:
            FIND Butiker NO-LOCK WHERE
              Butiker.Butik = Bruker.Butik NO-ERROR.
            IF AVAILABLE Butiker THEN 
              pcPrinter = Butiker.RAPPrinter.    
        END.
        ELSE pcPrinter = ''.

        FIND KOrdreHode NO-LOCK WHERE 
            KOrdreHode.KOrdre_Id = plKOrdre_Id NO-ERROR.
            
        IF NOT AVAILABLE KOrdreHode THEN
        DO: 
            ASSIGN 
                pcReturn = 'Ukjent kundeordre (' + STRING(plKOrdre_Id) + ').'
                bResult  = FALSE 
                .
            RETURN bResult.
        END.
        /* HAr ordren feil status, kan den ikke innleveres. */
        IF NOT CAN-DO('30,35,40,45,47',KOrdreHode.LevStatus) THEN
        DO:
            ASSIGN 
                pcReturn = 'Feil status. Ordre (' + STRING(plKOrdre_Id) + ') kan ikke utleveres.'
                bResult  = FALSE 
                .
            RETURN bResult.
        END.

        /* Nettbutikk ordre sjekkes spesielt for gyldig artikkel på varelinjene. */
        /* Er ikke artiklene flagget for nettbutikk, flagges de nå.              */
        IF INT(KOrdreHode.Opphav) = 10 THEN 
            RUN kordre_sjekkartnettbutikk.p(KOrdreHode.KOrdre_id).

        /* Setter leveringsdato på ordrelinjene. */
        FOR EACH KOrdreLinje OF KOrdreHode EXCLUSIVE-LOCK:
          plDec = 0.
          ASSIGN plDec = DECIMAL(KOrdreLinje.VareNr) NO-ERROR.
          IF ERROR-STATUS:ERROR THEN 
            ERROR-STATUS:ERROR = FALSE.
          IF plDec > 0 AND CAN-FIND(ArtBas WHERE 
                                    ArtBas.ArtikkelNr = plDec) THEN   
            KOrdreLinje.Leveringsdato  = TODAY.
        END. 

        /* Leverer ut ordren. */
        RUN kordre_levering.p (STRING(KOrdreHode.KOrdre_id),?,'',OUTPUT cTekst,OUTPUT obOk).
        
        /* Gikk utlevering bra, skal det betales og ordren skrives ut. */
        IF obOk THEN 
            DO:
                IF pbOpprettFaktura = FALSE THEN 
                    RUN kordre_kontant.p (STRING(KOrdreHode.KOrdre_id),?,'',OUTPUT cTekst,OUTPUT obOk).
                ELSE 
                    RUN kordre_fakturer.p (STRING(KOrdreHode.KOrdre_id),?,'',OUTPUT cTekst,OUTPUT obOk).
                    
                /* Fikk vi betalt, settes status og ordren skrives ut. */
                IF obOk THEN
                DO:      
                    RUN skrivkundeordre.p (STRING(KOrdreHode.KOrdre_id) + "|utlev",
                                           YES,
                                           (IF bSkrivPkSdl THEN pcPrinter ELSE ''),
                                           1,
                                           "",
                                           cTekst
                                           ) NO-ERROR.
                    setStatusKundeordre( INPUT STRING(KOrdreHode.KOrdre_Id),
                                         INPUT IF KOrdreHode.LevStatus < '50' THEN 50 ELSE INT(KOrdreHode.LevStatus)).
                END.                           
            END.
        ELSE DO:
            ASSIGN 
                pcReturn = cTekst
                bResult  = FALSE 
                .
        END.
        RETURN bResult.

    END METHOD.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC LOGICAL plukkKundeordre( INPUT pcKOrdre_IdLst AS CHARACTER ):
        
        DEFINE VARIABLE bResult AS LOGICAL NO-UNDO.
        DEFINE VARIABLE piAnt AS INTEGER NO-UNDO.
        DEFINE VARIABLE plPlListeId AS DECIMAL NO-UNDO.
        DEFINE VARIABLE pdDummy AS DECIMAL NO-UNDO.

        DEFINE BUFFER bufKOrdreHode FOR KOrdreHode.

        IF pcKOrdre_IdLst <> '' THEN 
        DO iLoop = 1 TO NUM-ENTRIES(pcKOrdre_IdLst):
           DO FOR bufKOrdreHode TRANSACTION:
               FIND bufKOrdreHode EXCLUSIVE-LOCK WHERE 
                  bufKOrdreHode.KOrdre_Id = DEC(ENTRY(iLoop,pcKOrdre_IdLst)) NO-ERROR NO-WAIT.
               IF AVAILABLE bufKOrdreHode AND NOT LOCKED bufKOrdreHode AND bufKOrdreHode.LevStatus < '42' THEN 
               DO:
                   piAnt = 0.
                   FOR EACH KOrdreLinje OF bufKOrdreHode NO-LOCK:
                       piant = piant + KOrdreLinje.Antall.
                   END.
                   /* Ingenting å behanlde på denne ordren. */
                   IF piAnt = 0 THEN 
                     NEXT.
                   
                  OPPSTANDELSEN:
                  DO:
                      FIND LAST PlListeHode NO-LOCK USE-INDEX PlListeHode NO-ERROR.
                      IF AVAILABLE PlListeHode THEN
                          plPlListeId  = PlListeHode.PlListeId + 1.
                      ELSE plPlListeId = 1.
                      /* Er nummerserie full, får vi lete etter hull */
                      IF plPlListeId > 99999999 THEN
                      LOOPEN:
                      DO plPlListeId = 1 TO 99999999:
                        IF NOT CAN-FIND(PlListeHode WHERE
                                        PlListeHode.PlListeId = plPlListeId) THEN
                            LEAVE LOOPEN.
                      END. /* LOOPEN */
                      FIND Butiker NO-LOCK WHERE 
                        Butiker.Butik = bufKOrdreHode.ButikkNr NO-ERROR.
                      /* Oppretter listehode. */
                      CREATE PlListeHode.
                      ASSIGN
                          PlListeHode.PlListeId   = plPlListeId
                          PlListeHode.FraButikkNr = iCL
                          PlListeHode.TilButikkNr = bufKOrdreHode.ButikkNr
                          PlListeHode.DatoPlukket = ?
                          PlListeHode.TidPlukket  = 0
                          PlListeHode.PrioPlukket = (IF AVAILABLE Butiker THEN Butiker.PrioPlukket ELSE PlListeHode.PrioPlukket)
                          PlListeHode.PlNavn      = "Overf. til " + (IF AVAILABLE Butiker THEN Butiker.ButNamn ELSE '')
                          PlListeHode.PlMerknad   = "Kundeordre/ekst.ordrenr: " + STRING(bufKOrdreHode.KOrdre_Id) + '/' + bufKOrdrEHode.EkstOrdreNr
                          PlListeHode.PlLType     = 1 /* Plukkliste */
                          .
                  END. /* OPPSTANDELSEN */

                  RADENES_MORGEN:
                  FOR EACH KOrdreLinje OF bufKOrdrEHode NO-LOCK:
                      ASSIGN 
                        pdDummy = DEC(KOrdreLinje.VareNr) NO-ERROR.
                      IF ERROR-STATUS:ERROR THEN 
                        NEXT.
                        
                      FIND ArtBas NO-LOCK WHERE
                          ArtBas.ArtikkelNr = DEC(KOrdreLinje.VareNr) NO-ERROR.
                      IF NOT AVAILABLE ArtBas THEN
                          NEXT.
                      
                      /* Skaper plukklistelinjen hvis den ikke finnes fra før */
                      FIND PlListeLinje EXCLUSIVE-LOCK WHERE
                          PlListeLinje.PlListeId  = plPlListeId AND
                          PlListeLinje.PlLinjeNr  = KOrdreLinje.KOrdreLinjeNr NO-ERROR.
                      IF NOT AVAILABLE PlListeLinje THEN
                      DO:
                          CREATE PlListeLinje.
                          ASSIGN
                              PlListeLinje.PlListeId  = plPlListeId 
                              PlListeLinje.PlLinjeNr  = KOrdreLinje.KOrdreLinjeNr
                              PlListeLinje.ArtikkelNr = ArtBas.ArtikkelNr 
                              PlListeLinje.StrKode    = KOrdreLinje.StrKode 
                              PlListeLinje.Antall     = KOrdreLinje.Antall
                              .
                      END.
                  END. /* RADENES_MORGEN */

                   ASSIGN 
                       bResult                 = TRUE 
                       .
                   RELEASE bufKOrdreHode.
                   
                   /* if bSendMail then ... Her skal inn kode som sender eMail til kunde. */
               END.
               ELSE IF AVAILABLE bufKOrdreHode AND bufKOrdreHode.LevStatus >= '42' THEN 
                  bResult = TRUE.    
           END.
        END.
        
        bResult = TRUE.
        RETURN bResult.

    END METHOD.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC LOGICAL setStatusKundeordre( INPUT pcKOrdre_IdLst AS CHARACTER,
                                               INPUT piLevStatus AS INTEGER ):
        
        DEFINE VARIABLE bResult AS LOGICAL NO-UNDO.
        DEFINE VARIABLE bSendMail AS LOG NO-UNDO.

        DEFINE BUFFER bufKOrdreHode FOR KOrdreHode.

        {syspar2.i 19 14 piLevStatus cTekst}
        IF CAN-DO('1,J,Ja,Yes,TRUE',cTekst) THEN 
            bSendMail = TRUE.
        
        IF pcKOrdre_IdLst <> '' THEN 
        DO iLoop = 1 TO NUM-ENTRIES(pcKOrdre_IdLst):
           DO FOR bufKOrdreHode TRANSACTION:
               FIND bufKOrdreHode EXCLUSIVE-LOCK WHERE 
                  bufKOrdreHode.KOrdre_Id = DEC(ENTRY(iLoop,pcKOrdre_IdLst)) NO-ERROR NO-WAIT.
               IF AVAILABLE bufKOrdreHode AND NOT LOCKED bufKOrdreHode THEN 
               DO:
                   ASSIGN 
                       bufKOrdreHode.LevStatus = STRING(piLevStatus)
                       bResult                 = TRUE 
                       .
                   RELEASE bufKOrdreHode.
                   
                   /* if bSendMail then ... Her skal inn kode som sender eMail til kunde. */
               END.
               ELSE IF AVAILABLE bufKOrdreHode AND bufKOrdreHode.LevStatus >= '45' THEN 
                  bResult = TRUE.    
           END.
        END.

        RETURN bResult.

    END METHOD.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC LOGICAL sjekkTvang( OUTPUT piStatusLst AS INTEGER,
                                      OUTPUT pbSTvang AS LOG ):
        
        DEFINE VARIABLE bResult AS LOGICAL NO-UNDO.

        /* Parameter gruppe hvor statuslisten skal hentes fra. */
        {syspara.i 19 9 4 piStatusLst INT}
        IF piStatusLst = 0 THEN 
            piStatusLst = 1.
        ELSE 
            piStatusLst = 15.
        /* Tvang på å følge odrestatus i ordrebehandling. */
        IF piStatusLst = 15 THEN 
        DO:
            {syspar2.i 19 9 4 cTekst}
            IF CAN-DO('1',cTekst) THEN 
                pbSTvang = TRUE.
            ELSE 
                pbSTvang = FALSE.
        END.
        ELSE pbSTvang = FALSE.

        bResult = TRUE.
        RETURN bResult.

    END METHOD.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC LOGICAL skrivFlerePakkseddler( INPUT pcKOrdre_IdLst AS CHARACTER,
                                                 INPUT pcUtskriftstype AS CHARACTER,
                                                 INPUT piAnteks AS INTEGER,
                                                 INPUT pcBruker AS CHARACTER ):
        
        DEFINE VARIABLE bResult AS LOGICAL NO-UNDO.
        
        IF pcKOrdre_IdLst <> '' THEN 
            DO iLoop = 1 TO NUM-ENTRIES(pcKOrdre_IdLst):
                bResult = skrivPakkseddel( INPUT DEC(ENTRY(iLoop,pcKOrdre_IdLst)),
                                               INPUT pcUtskriftstype,
                                               INPUT piAnteks,
                                               INPUT pcBruker ).
            END.
        
        RETURN bResult.

    END METHOD.

    /*------------------------------------------------------------------------------
     Purpose: forenklet utskrift av pakkseddel.
     Notes: 
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC LOGICAL skrivPakkseddel( INPUT plKOrdre_Id AS DECIMAL,
                                           INPUT pcUtskriftstype AS CHARACTER,
                                           INPUT piAnteks AS INTEGER,
                                           INPUT pcBruker AS CHARACTER ):

        DEFINE VARIABLE bResult AS LOGICAL NO-UNDO.
        DEFINE VARIABLE bdirekte AS LOG NO-UNDO.
        DEFINE VARIABLE cPrinter AS CHARACTER NO-UNDO.

        IF pcUtskriftstype = '' THEN
            pcUtskriftstype = 'full'.
        bDirekte = TRUE.

        FIND Bruker NO-LOCK WHERE 
          Bruker.BrukerId = pcBruker NO-ERROR.
        IF AVAILABLE Bruker AND Bruker.Butik > 0 THEN 
        DO:
            FIND Butiker NO-LOCK WHERE
              Butiker.Butik = Bruker.Butik NO-ERROR.
            IF AVAILABLE Butiker THEN 
              cPrinter = Butiker.RAPPrinter.    
        END.
        ELSE cPrinter = ''.

/*        RUN skrivkundeordre.p (STRING(plKOrdre_Id) + "|full",  */
/*                         bdirekte, cPrinter, piAntEks, "", "").*/
        bResult = skrivPakkseddel( INPUT plKOrdre_Id,
                                      INPUT pcUtskriftsType,
                                      INPUT bDirekte,
                                      INPUT cPrinter,
                                      INPUT piAntEks,
                                      INPUT "",
                                      INPUT "" ).
        bResult = TRUE.
        RETURN bResult.

    END METHOD.

    /*------------------------------------------------------------------------------
     Purpose: Gir tilgang til fullt grensesnitt for utskrift av pakkseddel.
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC LOGICAL skrivPakkseddel( INPUT plKOrdre_Id AS DECIMAL,
                                           INPUT pcUtskriftsType AS CHARACTER,
                                           INPUT pbDirekte AS LOG,
                                           INPUT pcPrinter AS CHARACTER,
                                           INPUT piAntEks AS INTEGER,
                                           INPUT pcMailAdress AS CHARACTER,
                                           INPUT pcStatusTxt AS CHARACTER ):

        DEFINE VARIABLE bResult AS LOGICAL NO-UNDO.

        IF CAN-FIND(FIRST KOrdreHode WHERE
                    KOrdreHode.KOrdre_Id = plKOrdre_Id) THEN
        DO:
            RUN skrivkundeordre.p (STRING(plKOrdre_Id) + "|" + pcUtskriftsType,pbDirekte,pcPrinter,piAntEks,pcMailAdress,pcStatusTxt).
            bResult = TRUE.
        END.
        ELSE
            bResult = FALSE.

        RETURN bResult.

    END METHOD.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC LOGICAL skrivPostpakkeEtikett( INPUT pcKOrdre_IdLst AS CHARACTER ):
        
        DEFINE VARIABLE bResult AS LOGICAL NO-UNDO.
        DEFINE VARIABLE piIntegrasjon AS INTEGER NO-UNDO.        
        DEFINE VARIABLE pcSkriver AS CHARACTER NO-UNDO.

        {syspara.i 19 9 1 piIntegrasjon INT}
        {syspara.i 210 100 7 pcSkriver}
        
        IF piIntegrasjon > 0 THEN
        DO iLoop = 1 TO NUM-ENTRIES(pcKOrdre_IdLst):
            CASE piIntegrasjon:
                WHEN 1 THEN DO:
                    RUN ekspWinEDI.p(ENTRY(iLoop,pcKOrdre_IdLst) + '|WinEDI' + '|' + pcSkriver).
                    bResult = TRUE.
                END.
                WHEN 2 THEN DO:
                    RUN ekspUniFaun.p(ENTRY(iLoop,pcKOrdre_IdLst) + '|UniFaun' + '|' + pcSkriver).
                    bResult = TRUE.
                END.
                OTHERWISE DO:
                    
                END.
            END CASE.
        END.

        RETURN bResult.

    END METHOD.

    DESTRUCTOR PUBLIC KundeordreBehandling ( ):
        rStandardFunksjoner:SkrivTilLogg(cLogg,
            'Klasse avsluttet.'
            ).

    END DESTRUCTOR.

END CLASS.