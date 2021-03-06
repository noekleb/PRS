 
 /*------------------------------------------------------------------------
    File        : KundeordreBehandling
    Purpose     : Hensikten med klasen er � gj�re metodene tilgjengelig for andre anheter via AppServer. Slik at f.eks ogs� kassen kan utf�re funksjonene.
    Syntax      : 
    Description : Klassen inneholder metoder for h�ndtering av kundeordre.
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
    DEFINE VARIABLE lGant AS LOG NO-UNDO.

    DEFINE VARIABLE rStandardFunksjoner AS cls.Stdfunk.StandardFunksjoner NO-UNDO.
    DEFINE VARIABLE rSendEMail          AS cls.SendEMail.SendEMail        NO-UNDO.
         
    DEFINE STREAM Ut.     
         
    CONSTRUCTOR PUBLIC KundeordreBehandling (  ):
        SUPER ().
  
        {syspara.i 210 100 8 cTekst}
        IF CAN-DO('1,yes,Ja,true',cTekst) THEN 
          lGant = TRUE.
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
        rSendEMail  = NEW cls.SendEMail.SendEMail( ) NO-ERROR.
        IF bTest THEN 
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
        
/*        DEFINE VARIABLE pbOpprettFaktura AS LOG NO-UNDO.*/
        DEFINE VARIABLE plDec AS DECIMAL NO-UNDO.
        DEFINE VARIABLE pcPrinter AS CHARACTER NO-UNDO.
        
        DEFINE BUFFER bufKOrdreHode FOR KOrdreHode.
        
        {syspara.i 150 1 8 cTekst} /* er satt til 1 hos Gant. */
/*        pbOpprettFaktura = IF cTekst = '1' THEN TRUE ELSE FALSE.*/

        IF bTest THEN 
        DO:
          rStandardFunksjoner:SkrivTilLogg(cLogg,
              '  Lever til kunde:'
              ).
          rStandardFunksjoner:SkrivTilLogg(cLogg,
              '    plKOrdre_Id: ' + STRING(plKOrdre_Id)
              ).
          rStandardFunksjoner:SkrivTilLogg(cLogg,
              '    pcBruker: ' + pcBruker
              ).
/*          rStandardFunksjoner:SkrivTilLogg(cLogg,              */
/*              '    OpprettFaktura: ' + STRING(pbOpprettFaktura)*/
/*              ).                                               */
          rStandardFunksjoner:SkrivTilLogg(cLogg,
              '    bSkrivPkSdl: ' + STRING(bSkrivPkSdl)
              ).
        END.
        
        FIND Bruker NO-LOCK WHERE 
          Bruker.BrukerId = pcBruker NO-ERROR.
        IF AVAILABLE Bruker AND Bruker.Butik > 0 THEN 
        DO:
            FIND Butiker NO-LOCK WHERE
              Butiker.Butik = Bruker.Butik NO-ERROR.
            IF AVAILABLE Butiker THEN 
            DO:
/*              /* GANT skal ha fakturaskriver. */   */
/*              IF lGant THEN                        */
/*                pcPrinter = Butiker.Fakturaskriver.*/
/*              ELSE                                 */
              pcPrinter = Butiker.RAPPrinter.
            END.    
        END.
        ELSE pcPrinter = ''.

        IF bTest THEN 
          rStandardFunksjoner:SkrivTilLogg(cLogg,
              '  pcPrinter: ' + pcPrinter
              ).

        FIND KOrdreHode NO-LOCK WHERE 
            KOrdreHode.KOrdre_Id = plKOrdre_Id NO-ERROR.
        IF NOT AVAILABLE KOrdreHode THEN
        DO: 
            ASSIGN 
                pcReturn = 'Ukjent kundeordre (' + STRING(plKOrdre_Id) + ').'
                bResult  = FALSE 
                .
            IF bTest THEN 
              rStandardFunksjoner:SkrivTilLogg(cLogg,
                  '  Avbrudd: ' + pcReturn
                  ).
            RETURN bResult.
        END.
        /* Har ordren feil status, kan den ikke utleveres. */
        IF NOT CAN-DO('30,35,40,45,47,55',KOrdreHode.LevStatus) THEN
        DO:
            ASSIGN 
                pcReturn = 'Feil status. Ordre (' + STRING(plKOrdre_Id) + ') har status ' + KOrdreHode.LevStatus + ' og kan ikke utleveres.'
                bResult  = FALSE 
                .
            IF bTest THEN 
              rStandardFunksjoner:SkrivTilLogg(cLogg,
                  '  Avbrudd: ' + pcReturn
                  ).
            RETURN bResult.
        END.

        rStandardFunksjoner:SkrivTilLogg(cLogg,
            '    Utleverer kundeordre: ' + STRING(KOrdreHode.KOrdre_Id) + ' EkstORdreNr: ' + STRING(KOrdreHode.EkstOrdreNr) 
            ).

        /* Nettbutikk ordre sjekkes spesielt for gyldig artikkel p� varelinjene. */
        /* Er ikke artiklene flagget for nettbutikk, flagges de n�.              */
        IF INT(KOrdreHode.Opphav) = 10 THEN 
            RUN kordre_sjekkartnettbutikk.p(KOrdreHode.KOrdre_id).

        IF bTest THEN 
          rStandardFunksjoner:SkrivTilLogg(cLogg,
              '  Behandler kundeordre KORdre_Id/EkstORdreNr/LevfNr: ' + STRING(KOrdreHode.KOrdre_Id) + '/' + 
              KOrdreHode.EkstOrdreNr + '/' + 
              STRING(KOrdreHode.LevFNr) + '.'
              ).

        /* Setter leveringsdato p� ordrelinjene. */
        FOR EACH KOrdreLinje OF KOrdreHode EXCLUSIVE-LOCK:

          IF bTest THEN 
            rStandardFunksjoner:SkrivTilLogg(cLogg,
                '    VareNr: ' + KOrdreLinje.VareNr
                ).
          plDec = 0.
          ASSIGN plDec = DECIMAL(KOrdreLinje.VareNr) NO-ERROR.
          IF ERROR-STATUS:ERROR THEN 
            ERROR-STATUS:ERROR = FALSE.
          IF plDec > 0 AND CAN-FIND(ArtBas WHERE 
                                    ArtBas.ArtikkelNr = plDec) THEN   
            KOrdreLinje.Leveringsdato  = TODAY.
          IF bTest THEN 
            rStandardFunksjoner:SkrivTilLogg(cLogg,
                '    Leveringsdato: ' + STRING(KOrdreLinje.Leveringsdato)
                ).
        END. 

        IF bTest THEN 
          rStandardFunksjoner:SkrivTilLogg(cLogg,
              '  F�r utlevering.'
              ).

        /* Leverer ut ordren. */
        RUN kordre_levering.p (STRING(KOrdreHode.KOrdre_id),?,'',OUTPUT cTekst,OUTPUT obOk).

        IF bTest THEN 
          rStandardFunksjoner:SkrivTilLogg(cLogg,
              '  Etter utlevering: ' + STRING(obOk) + ' ' + cTekst
              ).
              
        /* Gikk utlevering bra, skal det betales og ordren skrives ut. */
        IF obOk THEN 
            DO:
                /* TN 15/12-19 Det er lovp�lagt � fakturere nettsalg fra 1/1-20. */
/*                IF pbOpprettFaktura = FALSE THEN                                                       */
/*                DO:                                                                                    */
/*                    IF bTest THEN                                                                      */
/*                      rStandardFunksjoner:SkrivTilLogg(cLogg,                                          */
/*                          '  F�r kundeordre_kontant.'                                                  */
/*                          ).                                                                           */
/*                    RUN kordre_kontant.p (STRING(KOrdreHode.KOrdre_id),?,'',OUTPUT cTekst,OUTPUT obOk).*/
/*                END.                                                                                   */
/*                ELSE                                                                                   */
                DO:
                    IF bTest THEN 
                      rStandardFunksjoner:SkrivTilLogg(cLogg,
                          '  F�r kundeordre_fakturer - SendingsNr: ' + KOrdreHode.SendingsNr
                          ).
                    /* Ved utlevering av VANLIG ordre opprettes faktura. */ 
                    IF (NOT KOrdreHode.EkstOrdreNr MATCHES '*RETUR*' AND 
                        NOT KOrdreHode.EkstOrdreNr MATCHES '*BYTTE*') THEN
                    UTLEVER_VANLIG_ORDRE:
                    DO:
                      IF bTest THEN 
                        rStandardFunksjoner:SkrivTilLogg(cLogg,
                            '  Utlever VANLIG ordre.'
                            ).
                      RUN kordre_fakturer.p (STRING(KOrdreHode.KOrdre_id),?,'',OUTPUT cTekst,OUTPUT obOk).
                      IF obOk THEN 
                      DO:
                        DO FOR bufKOrdreHode TRANSACTION:
                            FIND bufKOrdreHode EXCLUSIVE-LOCK WHERE 
                                bufKOrdreHode.KOrdre_Id = KOrdreHode.KOrdre_Id NO-ERROR.
                            IF AVAILABLE bufKOrdreHode THEN 
                                bufKOrdreHode.Faktura_Id = DEC(cTekst) NO-ERROR.
                            IF AVAILABLE bufKOrdreHode THEN 
                                RELEASE bufKORdreHode.
                        END.
                        RUN faktura_produksjon.p ("idlist|" + cTekst + '|' + STRING(TODAY) + '|' + STRING(KOrdreHode.Opphav),?,'',OUTPUT cTekst,OUTPUT obOk).

                        /* Er det byttet varer p� ordren, skal lager korrigeres p� de linjene hvor det er byttet. */
                        /* Det gj�res ved � opprette to overf�ringsordrer.                */
                        /*   -- Varene p� linjene hvor det er byttet vare.                */
                        /*      - Varene p� de aktive linjene skal hentes fra Lager (16). */
                        /*      - Varene fra passive linjene skal leveres tilbake til Lager (16). */ 
                        IF CAN-FIND(FIRST KOrdreLinje OF KOrdreHode NO-LOCK WHERE 
                                         KOrdreLinje.Aktiv = FALSE) THEN 
                          RUN ovordre_korrlager.p(STRING(KOrdreHode.KOrdre_Id),?).
                      END.
                      /* Varsler butikk som skal utlevere varen. */                     
                      IF KOrdreHode.LevFNr = 8 THEN
                      DO: 
                        sendMailUtleverbutikk( KOrdreHode.KOrdre_Id ).
                        IF bTest THEN 
                          rStandardFunksjoner:SkrivTilLogg(cLogg,
                              '  Sendt eMail varsel til utleverende butikk.'
                              ).
                      END.
                    END. /* UTLEVER_VANLIG_ORDRE */

                    /* Utlevering av RETUR ordre.                                       */
                    /* Ved utlevering av retur skal det ikke gj�res noe med faktura.    */
                    /* Her skal varer flyttes og retur og eventuell salgstranser lages. */
                    ELSE IF KOrdreHode.EkstOrdreNr MATCHES '*RETUR*' OR 
                            KOrdreHode.EkstOrdreNr MATCHES '*BYTTE*' THEN
                    DO:
                      IF KOrdreHode.EkstOrdreNr MATCHES '*RETUR*' THEN 
                      DO:
                        IF bTest THEN 
                          rStandardFunksjoner:SkrivTilLogg(cLogg,
                              '  Utlever RETUR ordre.'
                              ).
                        RUN  kordre_krediter.p (STRING(KOrdreHode.KOrdre_id),?,'',OUTPUT cTekst,OUTPUT obOk).
                        IF obOk THEN 
                        DO:
                          DO FOR bufKOrdreHode TRANSACTION:
                              FIND bufKOrdreHode EXCLUSIVE-LOCK WHERE 
                                  bufKOrdreHode.KOrdre_Id = KOrdreHode.KOrdre_Id NO-ERROR.
                              IF AVAILABLE bufKOrdreHode THEN 
                                  bufKOrdreHode.Faktura_Id = DEC(cTekst) NO-ERROR.
                              IF AVAILABLE bufKOrdreHode THEN 
                                  RELEASE bufKORdreHode.
                          END.
                          RUN faktura_produksjon.p ("idlist|" + cTekst + '|' + STRING(TODAY) + '|' + STRING(KOrdreHode.Opphav),?,'',OUTPUT cTekst,OUTPUT obOk).
                        END.
                      END.
                      ELSE IF bTest THEN 
                        rStandardFunksjoner:SkrivTilLogg(cLogg,
                            '  Utlever BYTTE ordre.'
                            ).
                                            
                      /* TN 16/4-19 Overf�rer varene tilbake til nettbutikkens lager ved utlevering av en RETUR. */                        
                      RUN ovordre_retur.p(STRING(KOrdreHode.KOrdre_Id),TRUE).
                      
                      /* Oppretter bong med de n�vendige retur og varesalgs transaksjonene.           */
                      /* Er det byttet vare p� eCom ved utlevering av retur, resulterer det i at ogs� */ 
                      /* en varesalgstransaksjon m� opprettes.                                        */
                      RUN kordre_retur.p(STRING(KOrdreHode.KOrdre_Id),OUTPUT cTekst,OUTPUT obOk).
                      obOk = TRUE.
                    END. 
                END.

                /* Fikk vi betalt, settes status og ordren skrives ut. */
                IF obOk THEN
                DO:      
                    IF bTest THEN 
                      rStandardFunksjoner:SkrivTilLogg(cLogg,
                          '  F�r skrivkundeordre.'
                          ).
                    RUN skrivkundeordre.p (STRING(KOrdreHode.KOrdre_id) + "|utlev",
                                           YES,
                                           (IF bSkrivPkSdl THEN pcPrinter ELSE ''),
                                           1,
                                           "",
                                           cTekst
                                           ) NO-ERROR.
                    IF bTest THEN 
                      rStandardFunksjoner:SkrivTilLogg(cLogg,
                          '  F�r setStatusKundeOrdree.'
                          ).
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

        IF bTest THEN 
          rStandardFunksjoner:SkrivTilLogg(cLogg,
              '  Ferdig: ' + cTekst
              ).

        bResult = obOk.
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
                   /* Ingenting � behanlde p� denne ordren. */
                   IF piAnt = 0 THEN 
                     NEXT.
                   
                  OPPSTANDELSEN:
                  DO:
                      FIND LAST PlListeHode NO-LOCK USE-INDEX PlListeHode NO-ERROR.
                      IF AVAILABLE PlListeHode THEN
                          plPlListeId  = PlListeHode.PlListeId + 1.
                      ELSE plPlListeId = 1.
                      /* Er nummerserie full, f�r vi lete etter hull */
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
                      
                      /* Skaper plukklistelinjen hvis den ikke finnes fra f�r */
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
                       bufKOrdreHode.LevStatus     = STRING(piLevStatus)
                       bufKOrdreHode.Kundeservice  = IF bufKOrdreHode.LevStatus = '50' THEN FALSE ELSE bufKOrdreHode.Kundeservice  
                       bResult                     = TRUE 
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

    METHOD PUBLIC INTEGER SettFargePaRecord( INPUT plKOrdre_Id AS DECIMAL ):
        
        DEFINE VARIABLE piResult AS INTEGER NO-UNDO.
        DEFINE VARIABLE piLevFNr AS INTEGER NO-UNDO.
        
        FIND KOrdreHode NO-LOCK WHERE 
            KOrdreHode.KOrdre_Id = plKOrdre_Id NO-ERROR.
        IF AVAILABLE KOrdreHode THEN 
        DO:
            piLevFnr = INT(DYNAMIC-FUNCTION("getFieldValues","KOrdreHode","WHERE KOrdre_id = " + STRING(KOrdreHode.KOrdre_Id),"LevFnr")). 
            
            IF KOrdreHode.cOpt1 <> '' OR KOrdreHode.LevFNr = 8 THEN 
            DO: 
                piResult = IF KOrdreHode.cOpt1 = "" THEN 10 /* GUL */ 
                              ELSE IF KOrdreHode.LevFNr <> 8 THEN 14  /* Gr�n */
                              ELSE 12. /* R�d */
            END.
            ELSE 
                piResult = 0.
        END.

        RETURN piResult.

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
        /* Tvang p� � f�lge odrestatus i ordrebehandling. */
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

  /*------------------------------------------------------------------------------
   Purpose: Sender varslingsmail til butikken som skal utlevere kundeordre.
            Mailen sendes n�r ordren utleveres fra eCom.
   Notes:
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC LOGICAL sendMailUtleverbutikk( INPUT plKOrdre_Id AS DECIMAL ):
    DEFINE VARIABLE bResult AS LOGICAL NO-UNDO.
    DEFINE VARIABLE pceMailLst AS CHARACTER NO-UNDO.
    DEFINE VARIABLE pcFilNavn AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER lokKOrdreHode FOR KOrdreHode.
    DEFINE BUFFER lokKOrdreLinje FOR KOrdreLinje.
    DEFINE BUFFER lokButiker FOR Butiker.

    DO FOR lokKOrdreHode, lokKOrdreLinje, lokButiker:
      FIND lokKOrdreHode NO-LOCK WHERE 
        LokKOrdreHode.KORdre_Id = plKOrdre_Id NO-ERROR.
      IF AVAILABLE lokKORdreHode AND LokKOrdreHode.LevFNr = 8 THEN
      MAILBLOKK: 
      DO:
        pcFilNavn  = REPLACE(rStandardFunksjoner:getTempFileName(),'.tmp','.txt').

        FIND FIRST lokKOrdreLinje OF lokKOrdreHode WHERE 
            lokKORdreLinje.UtleverButikk > 0 NO-ERROR.
        IF AVAILABLE lokKOrdreLinje THEN
            FIND lokButiker NO-LOCK WHERE 
                lokButiker.butik = lokKOrdreLinje.UtleverButikk NO-ERROR.
        
        pceMailLst = IF AVAILABLE lokButiker THEN lokButiker.ePostAdresse ELSE ''.
        IF NUM-ENTRIES(pceMailLst,'@') < 2 THEN 
          LEAVE MAILBLOKK.
              
        OUTPUT STREAM Ut TO VALUE(pcFilNavn).
        PUT STREAM Ut UNFORMATTED  
          'Nettordre er klargjort og sendt fra eCom til butikk:' + CHR(10) +
          '----------------------------------------------------' + CHR(10) +
          'NettOrdreNr.......: ' + lokKOrdreHode.EkstOrdreNr + CHR(10) + 
          'Leveringsdato.....: ' + STRING(lokKOrdreHode.Leveringsdato) + CHR(10) + 
          'KundeNr...........: ' + STRING(lokKORdreHode.KundeNr) + CHR(10) + 
          'Navn..............: ' + lokKORdrEHode.Navn + CHR(10) +
          'Mobil.............: ' + lokKOrdreHode.MobilTlf + CHR(10) + CHR(10) + 
          'Varelinjer:' + CHR(10) + 
          'Varetekst....................: ArtikkelNr.........: Farge..............: Str...........: Antall...:'
          SKIP. 
        FOR EACH lokKordreLinje OF lokKOrdreHode NO-LOCK WHERE 
          NOT lokKOrdreLinje.VareNr BEGINS 'BETALT':
          PUT STREAM Ut UNFORMATTED 
            SUBSTRING(lokKOrdreLinje.Varetekst,1,30) + FILL(' ',30 - LENGTH(SUBSTRING(lokKOrdreLinje.Varetekst,1,30)))
            SUBSTRING(lokKOrdreLinje.VareNr,1,20) + FILL(' ',20 - LENGTH(SUBSTRING(lokKOrdreLinje.VareNr,1,20)))
            SUBSTRING(lokKOrdreLinje.LevFargKod,1,20) + FILL(' ',20 - LENGTH(SUBSTRING(lokKOrdreLinje.LevFargKod,1,20)))
            SUBSTRING(lokKOrdreLinje.Storl,1,15) + FILL(' ',15 - LENGTH(SUBSTRING(lokKOrdreLinje.Storl,1,15)))
            SUBSTRING(STRING(lokKOrdreLinje.Antall),1,10) + FILL(' ',10 - LENGTH(SUBSTRING(STRING(lokKOrdreLinje.Antall),1,10)))
          SKIP. 
        END.
        OUTPUT STREAM Ut CLOSE.      
              
        rStandardFunksjoner:SkrivTilLogg(cLogg,
          '  Mailvarsel utlevert kundeordre sendt butikk ' + STRING(lokButiker.Butik) + ' eMail ' + pceMailLst + ' ' + STRING(NOW,"99/99/9999 HH:MM:SS") 
          ).
        
        rSendEMail:parToADDRESS       = pceMailLst.
        rSendEMail:parMailType        = 'PAKKSEDDEL'.
        rSendEMail:parSUBJECT         = (IF SEARCH('tnc.txt') <> ? THEN 'TEST ' ELSE '') +
                                        'Nettordre ' + lokKOrdreHode.EkstOrdreNr + ' sendt for utlevering i butikk ' + STRING(lokButiker.Butik) + ' ' + lokButiker.ButNamn + '.' +   
                                        ' Dato/Tid: ' + STRING(NOW,"99/99/99 hh:mm:ss") + ".".    
        rSendEMail:parMessage-Charset = ''. /* Blank eller 'UTF-8' n�r det g�r fra fil. */
        rSendEMail:parMessage-File    = pcFilNavn.
        
        rSendEMail:send( ).
        
        bResult = TRUE.
      END. /* MAILBLOKK */    
    END.
    RETURN bResult.

  END METHOD.

    DESTRUCTOR PUBLIC KundeordreBehandling ( ):
        rStandardFunksjoner:SkrivTilLogg(cLogg,
            'Klasse avsluttet.'
            ).

    END DESTRUCTOR.

END CLASS.