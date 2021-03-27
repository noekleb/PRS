 
 /*------------------------------------------------------------------------
    File        : pricatEksport
    Purpose     : Forenkle og standardisere eksport av data i pricat format.
    Syntax      : 
    Description : Klassen inneholder metoder for klargjøring og eksport av pricat fil.
    Author(s)   : tny
    Created     : Thu Oct 11 10:06:22 CEST 2018
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.


CLASS cls.Vpi.Pricat: 
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE cEksportKatalog AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cEkstent        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFilNavn        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cDir            AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iCl             AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iLoop           AS INTEGER   NO-UNDO.
    DEFINE VARIABLE ocValue         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE icSessionId     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iKalkyle        AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cVgTabell       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cLevTabell      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iProfilNr       AS INTEGER   NO-UNDO.

    DEFINE VARIABLE rStandardFunksjoner AS CLASS cls.StdFunk.StandardFunksjoner NO-UNDO.

    DEFINE PUBLIC PROPERTY ppcPricatFil  AS CHARACTER NO-UNDO 
        GET. 
        SET.
    DEFINE PUBLIC PROPERTY ppiAntPoster  AS INTEGER NO-UNDO 
        GET. 
        SET.
    DEFINE PUBLIC PROPERTY ppcEDB-System AS CHARACTER NO-UNDO 
        GET. 
        SET.
    DEFINE PUBLIC PROPERTY ppcTegn AS CHARACTER NO-UNDO 
        GET. 
        SET.
      
    {cls\vpi\ttPricat.i}
    {cls\vpi\ttVpiArtBas.i}
    
    DEF STREAM Ut.
    
    CONSTRUCTOR PUBLIC Pricat (  ):
        SUPER ().
    
        /* Aktiverer mapping ved eksport. */    
        {syspara.i 50 29 1 ppcEDB-System}
        {syspar2.i 50 29 1 ppcTegn}
    
        rStandardFunksjoner = NEW cls.StdFunk.StandardFunksjoner().
        
        EMPTY TEMP-TABLE ttPricat.
        
        cEksportKatalog = SESSION:TEMP-DIRECTORY.
        IF cEksportKatalog = "" THEN
            cEksportKatalog = "c:\HOME\lindbak\sendes".
        {syspara.i 5 1 1 iCl INT}
        FIND Butiker NO-LOCK WHERE
            Butiker.Butik = iCl NO-ERROR.
        
        ASSIGN 
            cLevTabell = 'LevBas'
            cVgTabell  = 'VarGr'
            cEkstent   = "csv"
            iProfilNr  = 1
            cFilnavn   = RIGHT-TRIM(RIGHT-TRIM(TRIM(cEksportKatalog), "\"),"/") + "\" + 
                            "pricat" + "." + cEkstent
            cDir = ENTRY(1,cFilnavn,"\") + "\"
            .
        DO iLoop = 2 TO NUM-ENTRIES(cFilnavn,"\") - 1:
            cDir = cDir + ENTRY(iLoop,cFilnavn,"\") + "\".
            OS-CREATE-DIR VALUE(cDir).
        END.
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC VOID eksporterVpiLev( INPUT piEkstVpiLevNr AS INTEGER ):
        DEFINE VARIABLE piMaksAnt AS INTEGER NO-UNDO.
        DEFINE VARIABLE piAnt     AS INTEGER NO-UNDO.
        
        ASSIGN 
            piMaksant = 0.
                
        /* Tømmer temp-tabeller. */
        emptyVpiArtBas().
        
        /*Legger alle ønskede data inn i temp-tabell. */
        BLOKKEN1:
        FOR EACH VPIArtBas NO-LOCK WHERE 
            VPIArtBas.EkstVPILevNr = piEkstVpiLevNr:
            
            /* oppretter enkeltposter. */
            opprettPricatRecord( 1, 2, ROWID(VpiArtBas)).
            
            ASSIGN piAnt = piAnt + 1.
            IF piMaksant > 0 AND piAnt >= piMaksant THEN 
                LEAVE BLOKKEN1.
        END. /* BLOKKEN1 */
        
        /* Eksporterer tabellen hvis det er innhold i den. */
        IF CAN-FIND(FIRST ttPricat) THEN 
            eksporterPricat( piEkstVpiLevNr ).        
        
        RETURN.

    END METHOD.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC VOID eksporterPricat( INPUT piEkstVpiLevNr AS INTEGER ):
        DEFINE VARIABLE pcFilNavn AS CHARACTER NO-UNDO.
        DEFINE VARIABLE piAnt AS INTEGER NO-UNDO.
        DEFINE VARIABLE pcEkstLevNr AS CHARACTER NO-UNDO.
        DEFINE VARIABLE pcEkstVgNr AS CHARACTER NO-UNDO.

        /* Eksporterer tabellen hvis det er innhold i den. */
        IF NOT CAN-FIND(FIRST ttPricat) THEN
        DO: 
            RETURN.
        END.        

        ASSIGN 
            pcfilNavn    = 'konv\pricat_' + STRING(piEkstVpiLevNr) + '_&LevNr_' + REPLACE(STRING(TODAY),'/','') + REPLACE(STRING(TIME,"HH:MM:SS"),':','') + '.json'
            .
        TEMP-TABLE ttPricat:WRITE-JSON('file', REPLACE(pcFilNavn,'&LevNr',STRING(ttPricat.LevNr)), TRUE).
        
        FOR EACH ttPricat
            BREAK BY ttPricat.EkstVPILevNr
                  BY ttPriCat.LevNr:

            IF ppcEDB-System <> '' THEN 
            DO:
                FIND FIRST ImpKonv NO-LOCK WHERE 
                  ImpKonv.EDB-System = ppcEDB-System AND 
                  ImpKonv.Tabell     = cVgTabell AND 
                  ImpKonv.InterntId  = ttPricat.Varegruppe NO-ERROR.
                IF AVAILABLE ImpKonv THEN 
                  ASSIGN pcEkstVgNr = ImpKonv.EksterntId.
                ELSE 
                  ASSIGN pcEkstVgNr = ''.
    
                FIND FIRST ImpKonv NO-LOCK WHERE 
                  ImpKonv.EDB-System = ppcEDB-System AND 
                  ImpKonv.Tabell     = cLevTabell AND 
                  ImpKonv.InterntId  = ttPricat.LevNr NO-ERROR.
                IF AVAILABLE ImpKonv THEN 
                  ASSIGN pcEkstLevNr = ImpKonv.EksterntId.
                ELSE 
                  ASSIGN pcEkstLevNr = ''.
            END.

            IF FIRST-OF(ttPriCat.LevNr) THEN
            APNEFIL: 
            DO:                
                OUTPUT STREAM Ut TO VALUE(REPLACE(REPLACE(pcFilNavn,'.json','.csv'),'&LevNr',STRING(IF pcEkstLevNr <> '' THEN pcEkstLevNr ELSE ttPriCat.LevNr))) APPEND.
        
                PUT STREAM Ut UNFORMATTED
                    "R1" + 
                    ";" + "LevNr" + 
                    ";" + "LevModellNr" +
                    ";" + "EANnr" + 
                    ";" + "VareTekst" +
                    ";" + "FargeKode" + 
                    ";" + "FargeTekst" + 
                    ";" + "Str" +       
                    ";" + "StrTab" + 
                    ";" + "Varemerke" +
                    ";" + "Enh" +
                    ";" + "AntIEnh" +
                    ";" + "LevPrisEngros" +
                    ";" + "ValKod" +
                    ";" + "forhRab%" +
                    ";" + "suppRab%" +
                    ";" + "VeilPris" +
                    ";" + "PAKstru" +
                    ";" + "LevUke1" +
                    ";" + "LevUke2" +
                    ";" + "LevUke3" +
                    ";" + "LevUke4" +
                    ";" + "VareGruppe" +
                    ";" + "LevNavn" +
                    ";" + "Bestillingsnummer" +
                    ";" + "nettoForh" +
                    ";" + "kalkForh" +
                    ";" + "BFforh" +
                    ";" + "nettoSupp" +
                    ";" + "kalkSupp" +
                    ";" + "BFsupp" + 
                    ";" + "MarkedsPris" +
                    ";" + "Sortiment" +
                    ";" + "Sesong" +
                    ";" + "VPIBildeKode" +
                    ";" + "Merknad" +
                    ";" + "KjedeValutaPris" + 
                    ";" + "KjedeProdusent" +    
                    ";" + "ERPNr" +           
                    ";" + "SalgsEnhetsType" + 
                    ";" + "AktivFraDato" +    
                    ";" + "AktivTilDato" +    
                    ";" + "Bongtekst" +      
                    ";" + "Etikettekst1" +    
                    ";" + "Funksjonskode"    
                    ";" + "Mva_Proc" +       
                    ";" + "LinkVare" +        
                    ";" + "PantBelop" +       
                    ";" + "Filial" +          
                    ";" + "Produsent" +       
                    ";" + "Mengde" +         
                    ";" + "JamforEnhet" +     
                    ";" + "Kontrolleres" +
                    ";" + "ArtikkelNr" +
                    ";" + "OpprettArtikkel" + 
                    ";" + "PosterPrisending" + 
                    ";" +
                    ";" +
                    ";" +
                    ";" +
                    ";" +
                    ";" +
                    ";" +
                    ";" +
                    ";" +
                    ";" +
                    ";" +
                    ";" +
                    ";" +
                    ";" +
                    ";" +
                    ";" +
                    ";" +
                    ";" +
                    ";" +
                    ";" +
                    ";" +
                    ";" +
                    ";" +
                    ";" +
                    ";" +
                    ";" +
                    ";" +
                    ";" +
                    ";" +
                    ";" +
                    ";" +
                    ";" +
                    ";" + "Lager" + 
                    ";" + "Etikett" +        
                    ";" + "Sortimentkoder" + 
                    ";" + "Lagerkoder" + 
                    ";" + "Gjennomfaktureres" + 
                    ";" + "KjedeVare" + 
                    ";" + "Kampanjeuker" + 
                    ";" + "Kampanjestotte" + 
                    ";" + "BehStatus" + 
                    ";" + "Grunnsortiment" +
                    ";" + "Opphav" +
                    ";" + "RAvdNr" +
                    ";" + 
                    ";" +  
                    ";" + "Etikettekst2" +
                    ";" + "ArtSlag" + 
                    ";" + "OPris" +
                    ";" + "NON_Sale" + 
                    ";" + "NegVare" + 
                    ";" + "Pant" +
                    ";" + "Telefonkort" + 
                    ";" + "WebButikkArtikkel" + 
                    ";" + "PubliserINettbutikk" + 
                    ";" + "HoyLav" +         
                    ";" + "WebLeveringstid" +   
                    ";" + "WebMinLager" +   
                    ";" + "KampanjeKode" +    
                    ";" + "LopNr" +
                    ";" + "GarantiKl" +
                    ";" + "PostBredde"
                    ";" + "PostHoyde" +
                    ";" + "PostLengde" +
                    ";" + "PostVekt" +
                    ";" + "AntLinkVare" +
                    ";" + "Aktivtilbud" +
                    ";" + "TilbudPris" +
                    ";" + "PrisProfil" +
                    ";" + "Kjonn"
                    SKIP
                    .                        
            END. /* APNEFIL */

            piant = piant + 1.
            
            PUT STREAM Ut UNFORMATTED 
            /*  1 */ ttPricat.R1 ";"           
            /*  2 */ (IF pcEkstLevNr <> '' THEN pcEkstLevNr ELSE ttPricat.LevNr) ";"        
            /*  3 */ ttPricat.LevModellNr ";"   
            /*  4 */ ttPricat.EANnr + ppcTegn ";"         
            /*  5 */ ttPricat.VareTekst ";"     
            /*  6 */ ttPricat.FargeKode ";"     
            /*  7 */ ttPricat.FargeTekst ";"    
            /*  8 */ ttPricat.Str ";"           
            /*  9 */ ttPricat.StrTab ";"        
            /* 10 */ ttPricat.Varemerke ";"     
            /* 11 */ ttPricat.Enh ";"           
            /* 12 */ ttPricat.AntIEnh ";"       
            /* 13 */ ttPricat.LevPrisEngros ";" 
            /* 14 */ ttPricat.ValKod ";"        
            /* 15 */ ttPricat.forhRab% ";"      
            /* 16 */ ttPricat.suppRab% ";"      
            /* 17 */ ttPricat.VeilPris ";"      
            /* 18 */ ttPricat.PAKstru ";"       
            /* 19 */ ttPricat.LevUke1 ";"       
            /* 20 */ ttPricat.LevUke2 ";"       
            /* 21 */ ttPricat.LevUke3 ";"       
            /* 22 */ ttPricat.LevUke4 ";"       
            /* 23 */ (IF pcEkstVgNr <> '' THEN pcEkstVgNr ELSE ttPricat.VareGruppe) ";"    
            /* 24 */ ttPricat.LevNavn ";"       
            /* 25 */ ttPricat.LevKod ";"  
            /* 26 */ ttPricat.nettoForh ";"     
            /* 27 */ ttPricat.kalkForh ";"      
            /* 28 */ ttPricat.BFforh ";"        
            /* 29 */ ttPricat.nettoSupp ";"     
            /* 30 */ ttPricat.kalkSupp ";"      
            /* 31 */ ttPricat.BFsupp ";"        
            /* 32 */ ttPricat.MarkedsPris ";"   
            /* 33 */ ttPricat.Sortiment ";"     
            /* 34 */ ttPricat.Sesong ";"        
            /* 35 */ ttPricat.VPIBildeKode ";"
            /* 36 */ ttPricat.Merknad ";"     
            /* 37 */ ttPricat.KjedeValutaPris ";"  
            /* 38 */ ttPricat.KjedeProdusent ";"    
            /* 39 */ ttPricat.ERPNr ";"           
            /* 40 */ ttPricat.SalgsEnhetsType ";" 
            /* 41 */ ttPricat.AktivFraDato ";"    
            /* 42 */ ttPricat.AktivTilDato ";"    
            /* 43 */ ttPricat.Bongtekst ";"       
            /* 44 */ ttPricat.Etikettekst1 ";"    
            /* 45 */ ttPricat.Funksjonskode ";"   
            /* 46 */ ttPricat.Mva_Proc ";"        
            /* 47 */ ttPricat.LinkVare ";"         
            /* 48 */ ttPricat.PantBelop ";"       
            /* 49 */ ttPricat.Filial ";"          
            /* 50 */ ttPricat.Produsent ";"       
            /* 51 */ ttPricat.Mengde ";"          
            /* 52 */ ttPricat.JamforEnhet ";"     
            /* 53 */ ttPricat.Kontrolleres ";"
            /* 54 */ ttPricat.ArtikkelNr ";"
            /* 55 */ ttPricat.OpprettArtikkel ";" 
            /* 56 */ ttPricat.PosterPrisending ";" 
            /* 57 */ ttPricat.KjedeRab% ";" 
            /* 58 */ ttPricat.KjedeSupRab% ";" 
            /* 59 */ ";" 
            /* 60 */ ";" 
            /* 61 */ ";" 
            /* 62 */ ";" 
            /* 63 */ ";" 
            /* 64 */ ";" 
            /* 65 */ ";" 
            /* 66 */ ";" 
            /* 67 */ ";" 
            /* 68 */ ";" 
            /* 69 */ ";" 
            /* 70 */ ";" 
            /* 71 */ ";" 
            /* 72 */ ";" 
            /* 73 */ ";" 
            /* 74 */ ";" 
            /* 75 */ ";" 
            /* 76 */ ttPricat.AlfaKode2 ";" 
            /* 77 */ ";" 
            /* 78 */ ";" 
            /* 79 */ ";" 
            /* 80 */ ttPricat.KjedeInnkPris ";" 
            /* 81 */ ttPricat.KjedeSupInnkPris ";" 
            /* 82 */ ";" 
            /* 83 */ ";" 
            /* 84 */ ";" 
            /* 85 */ ";" 
            /* 86 */ ";" 
            /* 87 */ ";" 
            /* 88 */ ";" 
            /* 89 */ ttPricat.Lager ";" 
            /* 90 */ ttPricat.Etikett ";"        
            /* 91 */ ttPricat.Sortimentkoder ";" 
            /* 92 */ ttPricat.Lagerkoder ";" 
            /* 93 */ ttPricat.Gjennomfaktureres ";" 
            /* 94 */ ttPricat.KjedeVare ";" 
            /* 95 */ ttPricat.Kampanjeuker ";" 
            /* 96 */ ttPricat.Kampanjestotte ";" 
            /* 97 */ ttPricat.BehStatus ";" 
            /* 98 */ ttPricat.Grunnsortiment ";"
            /* 99 */ ttPricat.Opphav ";"
            /*100 */ ttPricat.RAvdNr ";"
            /*101 */ ";"
            /*102 */ ";" 
            /*103 */ ttPricat.Etikettekst2 ";"
            /*104 */ ttPricat.ArtSlag ";" 
            /*105 */ ttPricat.OPris ";" 
            /*106 */ ttPricat.NON_Sale ";" 
            /*107 */ ttPricat.NegVare ";" 
            /*108 */ ttPricat.Pant ";" 
            /*109 */ ttPricat.Telefonkort ";" 
            /*110 */ ttPricat.WebButikkArtikkel ";" 
            /*111 */ ttPricat.PubliserINettbutikk ";" 
            /*112 */ ttPricat.HoyLav ";"         
            /*113 */ ttPricat.WebLeveringstid ";"   
            /*114 */ ttPricat.WebMinLager ";"   
            /*115 */ ttPricat.KampanjeKode ";"   
            /*116 */ ttPricat.LopNr ";" 
            /*117 */ ttPricat.GarantiKl ";"
            /*118 */ ttPricat.PostBredde ";"
            /*119 */ ttPricat.PostHoyde ";"
            /*120 */ ttPricat.PostLengde ";"
            /*121 */ ttPricat.PostVekt ";"
            /*122 */ ttPricat.AntLinkVare ";"
            /*123 */ ttPricat.Aktivtilbud ";"
            /*124 */ ttPricat.TilbudPris ";"
            /*125 */ ttPricat.PrisProfil ";"
            /*126 */ ttPricat.Kjonn
            SKIP      
            .
            
            IF LAST-OF(ttPriCat.LevNr) THEN
            LUKKEFIL:
            DO:
                OUTPUT STREAM Ut CLOSE.
            END.             
        END.
         
        ASSIGN 
            ppiAntPoster = piAnt
            ppcPricatFil = REPLACE(pcFilNavn,'.json','.csv') 
            . 
            
        RETURN.

    END METHOD.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC VOID emptyVpiArtBas(  ):
        EMPTY TEMP-TABLE ttVpiArtBas.
        EMPTY TEMP-TABLE ttVpiStrekkode.
        EMPTY TEMP-TABLE ttVpiArtPris.
        EMPTY TEMP-TABLE ttPricat.
                
        RETURN.

    END METHOD.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes: Forutsetter at VPIArtBas og VPIArtPris er tilgjengelige når metoden
            kalles.
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC LOGICAL opprettPricatRecord( INPUT piKalkyle AS INTEGER,
                                               INPUT piType AS INTEGER,
                                               INPUT prRowId AS ROWID ):
        /* Ny fil er under påfylling, gammelt filnavn nullstilles */
        ppcPricatFil = ?.                                                         
        
        IF AVAILABLE Varemerke THEN 
            RELEASE Varemerke.
        IF AVAILABLE LevBas THEN 
            RELEASE LevBas.
        /* Henter fra ArtBas. */        
        IF piType = 1 THEN 
        DO:
            FIND ArtBas NO-LOCK WHERE 
                ROWID(ArtBas) = prRowId NO-ERROR.
            IF NOT AVAILABLE ArtBas THEN
            DO: 
            END. 
            CREATE ttVpiArtBas.
            BUFFER-COPY ArtBas
                EXCEPT KatalogPris ForhRab%
                TO ttVpiArtBas
                ASSIGN 
                    ttVpiArtBas.VareNr         = STRING(ArtBas.ArtikkelNr)
                    ttVpiArtBas.EkstVpiLevNr   = 1
                    ttVpiArtBas.forhRab%[1]    = ArtBas.ForhRab%
                    ttVpiArtBas.SuppRab%[1]    = ArtBas.SupRab%
                    ttVpiArtBas.KatalogPris[1] = ArtBas.KatalogPris
                    .
            FOR EACH ArtPris OF ArtBas NO-LOCK:
                CREATE ttVpiArtPris.
                BUFFER-COPY ArtPris
                    TO ttVpiArtPris
                    ASSIGN 
                        ttVpiArtPris.VareNr       = STRING(ArtBas.ArtikkelNr)
                        ttVpiArtPris.EkstVpiLevNr = 1.
            END.
            FOR EACH Strekkode OF ArtBas NO-LOCK:
                CREATE ttVpiStrekkode.
                BUFFER-COPY Strekkode
                    TO ttVpiStrekkode
                    ASSIGN 
                        ttVpiStrekkode.VareNr       = STRING(ArtBas.ArtikkelNr)
                        ttVpiStrekkode.EkstVpiLevNr = 1.
            END.
            FIND Varemerke NO-LOCK WHERE 
                Varemerke.VMId = ArtBas.VMId NO-ERROR.
            FIND LevBas NO-LOCK WHERE 
                LevBas.levnr = ArtBas.LevNr NO-ERROR.
            FIND HovedKategori NO-LOCK WHERE 
                HovedKategori.HovedKatNr = ArtBas.HovedKatNr NO-ERROR.
        END.
        /* Henter fra VPIArtBas. */
        ELSE IF pitype = 2 THEN 
        DO:
            FIND VpiArtBas NO-LOCK WHERE 
                ROWID(VpiArtBas) = prRowId NO-ERROR.
            IF NOT AVAILABLE VpiArtBas THEN 
                RETURN FALSE. 
            CREATE ttVpiArtBas.
            BUFFER-COPY VpiArtBas
                TO ttVpiArtBas.
            FOR EACH VPIArtPris OF VPIArtBas NO-LOCK:
                CREATE ttVpiArtPris.
                BUFFER-COPY VpiArtPris
                    TO ttVpiArtPris.
            END.
            FOR EACH VPIStrekkode OF VPIArtBas NO-LOCK:
                CREATE ttVpiStrekkode.
                BUFFER-COPY VpiStrekkode
                    TO ttVpiStrekkode.
            END.
            FIND Varemerke NO-LOCK WHERE 
                Varemerke.VMId = VpiArtBas.VMId NO-ERROR.
            FIND LevBas NO-LOCK WHERE 
                LevBas.levnr = VpiArtBas.LevNr NO-ERROR.
            FIND HovedKategori NO-LOCK WHERE 
                HovedKategori.HovedKatNr = VPIArtBas.HovedKatNr NO-ERROR.
        END.
        
        IF AVAILABLE ArtPris THEN 
            RELEASE ArtPris.
        FIND ArtPris NO-LOCK WHERE 
            ArtPris.ArtikkelNr = DEC(ttVpiArtBas.ArtikkelNr) AND 
            ArtPris.ProfilNr = iProfilNr NO-ERROR.
        
        /* Oppretter bare poster på artikler som har strekkoder. */
        RECORD-OPPRETTELSE:
        FOR EACH ttVpiStrekkode OF ttVpiArtBas:
            
            FIND StrKonv NO-LOCK WHERE 
                StrKonv.StrKode = ttVPIStrekkode.StrKode NO-ERROR.
                
            CREATE ttPricat.
            ASSIGN
                ttPricat.R1               = ""
                ttPricat.LevNr            = STRING(ttVpiArtBas.LevNr)
                ttPricat.LevModellNr      = ttVpiArtBas.LevKod
                ttPricat.EANnr            = IF AVAILABLE ttVpiStrekkode THEN ttVpiStrekkode.Kode ELSE ""
                ttPricat.VareTekst        = ttVpiArtBas.Beskr
                ttPricat.FargeKode        = LEFT-TRIM(STRING(ttVpiArtBas.Farg),'0')      
                ttPricat.FargeTekst       = ttVpiArtBas.LevFargKod
        
                ttPricat.Str              = IF AVAILABLE StrKonv THEN StrKonv.Storl ELSE ""    
                ttPricat.StrTab           = STRING(ttVpiArtBas.StrTypeId)      
                ttPricat.Varemerke        = IF AVAILABLE Varemerke THEN Varemerke.Beskrivelse ELSE ""
                
                ttPricat.Enh              = ttVpiArtBas.SalgsEnhet
                ttPricat.AntIEnh          = STRING(ttVpiArtBas.AntIPkn)
                ttPricat.ValKod           = ttVpiArtBas.ValKod
                    
                    
                ttPricat.PAKstru          = ""
                    
                ttPricat.VareGruppe       = STRING(ttVpiArtBas.Vg)
                    
                ttPricat.LevNavn          = IF AVAILABLE LevBas THEN LevBas.LevNamn ELSE ""
                ttPricat.LevKod           = IF AVAILABLE ttVpiStrekkode THEN ttVpiStrekkode.Bestillingsnummer ELSE ""
                ttPricat.Sortiment        = IF ttVpiArtBas.AnonseArtikkel = TRUE THEN "1" ELSE ""
                ttPricat.Sesong           = STRING(ttVpiArtBas.Sasong)
                ttPricat.VPIBildeKode     = ttVpiArtBas.VPIBildeKode
                ttPricat.Merknad          = STRING(ttVpiArtBas.ArtikkelNr)
                      
                /* Tilleggsfelter */
                ttPricat.ArtikkelNr       = ttVpiArtBas.ArtikkelNr
                ttPricat.ModellFarge      = ttVpiArtBas.ModellFarge
                ttPricat.HovedModellFarge = ttVpiArtBas.HovedmodellFarge
                ttPricat.Vg               = ttVpiArtBas.Vg
                ttPricat.Hg               = ttVpiArtBas.Hg
                ttPricat.Farg             = ttVpiArtBas.Farg
                ttPricat.StrTypeId        = ttVpiArtBas.STrTypeId
                ttPricat.iLevNr           = ttVpiArtBas.LevNr
                ttPricat.MomsKod          = IF AVAILABLE VarGr THEN VarGr.MomsKod ELSE 0
                ttPricat.VmId             = IF AVAILABLE Varemerke THEN VareMErke.VmId ELSE 0
                ttPricat.Sasong           = ttVpiArtBas.Sasong
                /* TN Lagt til 25/2-13 */ 
                ttPricat.KjedeValutaPris   = ttVpiArtBas.KjedeValutaPris 
                ttPricat.KjedeProdusent    = ttVpiArtBas.KjedeProdusent    
                ttPricat.ERPNr             = ttVpiStrekkode.ERPNr           
                ttPricat.SalgsEnhetsType   = STRING(ttVpiArtBas.SalgsEnhetstype)
                ttPricat.AktivFraDato      = ""     
                ttPricat.AktivTilDato      = ""     
                ttPricat.Bongtekst         = ttVpiArtBas.BongTekst        
                ttPricat.Etikettekst1      = ttVpiArtBas.Etikettekst1    
                ttPricat.Etikettekst2      = ttVpiArtBas.Etikettekst2
                ttPricat.Funksjonskode     = ""    
                ttPricat.Mva_Proc          = IF AVAILABLE Moms THEN STRING(Moms.MomsProc) ELSE '25'        
                ttPricat.LinkVare          = STRING(ttVpiArtBas.LinkVareNr)         
                ttPricat.PantBelop         = ""       
                ttPricat.Filial            = ""          
                ttPricat.Produsent         = STRING(ttVpiArtBas.ProdNr)       
                ttPricat.Mengde            = STRING(ttVpiArtBas.Mengde)           
                ttPricat.JamforEnhet       = STRING(ttVpiArtBas.JamforEnhet)      
                ttPricat.Kontrolleres      = FALSE  
                ttPricat.ArtikkelNr        = 0
                ttPricat.OpprettArtikkel   = FALSE   
                ttPricat.PosterPrisending  = FALSE 
                ttPricat.Etikett           = ttVpiArtBas.Etikett         
                ttPricat.BehStatus         = ttPricat.BehStatus 
                ttPricat.Grunnsortiment    = STRING(ttVpiArtBas.Grunnsortiment)
                ttPricat.Lager             = ttVpiArtBas.Lager 
                ttPricat.Sortimentkoder    = ttVpiArtBas.Sortimentkoder 
                ttPricat.Lagerkoder        = ttVpiArtBas.LagerKoder
                ttPricat.Gjennomfaktureres = ttVpiArtBas.Gjennomfaktureres 
                ttPricat.KjedeVare         = ttVpiArtBas.KjedeVare 
                ttPricat.Kampanjeuker      = ttVpiArtBas.KampanjeUker
                ttPricat.Kampanjestotte    = ttVpiArtBas.KampanjeStotte
                ttPricat.Opphav            = '0'
                ttPricat.RAvdNr            = STRING(ttVpiArtBas.RAvdNr)    
                ttPricat.ArtSlag           = STRING(ttVpiArtBas.ArtSlag)
                ttPricat.OPris             = STRING(ttVpiArtBas.OPris)
                ttPricat.NON_Sale          = STRING(ttVpiArtBas.NON_Sale) 
                ttPricat.NegVare           = STRING(ttVpiArtBas.NegVare) 
                ttPricat.Pant              = STRING(ttVpiArtBas.Pant)
                ttPricat.Telefonkort       = STRING(ttVpiArtBas.Telefonkort) 
                ttPricat.WebButikkArtikkel = STRING(ttVpiArtBas.WebButikkArtikkel) 
                ttPricat.PubliserINettbutikk = STRING(ttVpiArtBas.PubliserINettbutikk) 
                ttPricat.HoyLav            = STRING(ttVpiArtBas.HoyLav)         
                ttPricat.WebLeveringstid   = STRING(ttVpiArtBas.WebLeveringstid)   
                ttPricat.WebMinLager       = STRING(ttVpiArtBas.WebMinLager)   
                ttPricat.KampanjeKode      = STRING(ttVpiArtBas.KampanjeKode)
                ttPricat.Kjonn             = (IF AVAILABLE HovedKategori THEN HovedKategori.HovedKatTekst ELSE '')     
                
                ttPricat.KjedeInnkPris     = ttVpiArtBas.KjedeInnkPris
                ttPricat.KjedeSupInnkPris  = ttVpiArtBas.KjedeSupInnkPris
                                                
                NO-ERROR.
            IF piKalkyle = 0 THEN 
              ASSIGN
                ttPricat.LevPrisEngros    = IF AVAILABLE ttVpiArtPris THEN STRING(ttVpiArtPris.InnkjopsPris[1]) ELSE ""
                ttPricat.forhRab%         = STRING(ttVpiArtPris.Rab1%[1])
                ttPricat.suppRab%         = ""
                ttPricat.VeilPris         = STRING(ttVpiArtBas.AnbefaltPris)
                ttPricat.nettoForh        = ""
                ttPricat.kalkForh         = ""
                ttPricat.BFforh           = ""
                ttPricat.nettoSupp        = ""
                ttPricat.kalkSupp         = ""
                ttPricat.BFsupp           = ""
                ttPricat.MarkedsPris      = IF AVAILABLE ttVpiArtPris THEN STRING(ttVpiArtPris.Pris[1]) ELSE ""
              .
            ELSE 
              ASSIGN
                ttPricat.LevPrisEngros    = IF AVAILABLE ttVpiArtPris THEN STRING(ttVpiArtPris.InnkjopsPris[1]) ELSE ""
                ttPricat.forhRab%         = STRING(ttVpiArtBas.forhRab%[1])
                ttPricat.forhRab%         = (IF DEC(ttPricat.ForhRab%) = 0 THEN STRING(ttVpiArtPris.Rab1%[1]) ELSE ttPricat.ForhRab%)
                ttPricat.suppRab%         = STRING(ttVpiArtBas.suppRab%[1])
                ttPricat.VeilPris         = STRING(ttVpiArtBas.AnbefaltPris)
                ttPricat.nettoForh        = ""
                ttPricat.kalkForh         = ""
                ttPricat.BFforh           = ""
                ttPricat.nettoSupp        = ""
                ttPricat.kalkSupp         = ""
                ttPricat.BFsupp           = ""
                ttPricat.MarkedsPris      = IF AVAILABLE ttVpiArtPris THEN STRING(ttVpiArtPris.Pris[1]) ELSE ""
              .
            LEVUKE:
            DO:
              ASSIGN
                ttPricat.LevUke1 = ''
                ttPricat.LevUke2 = ''
                ttPricat.LevUke3 = ''
                ttPricat.LevUke4 = ''
                .   
              /* Leveringsuke en er alltid angitt. Da tar vi også med eventuelt andre uker. */       
              IF ttVpiArtBas.LevDato1 <> ? THEN 
                DO:
                  ttPricat.LevUke1 = STRING(rStandardFunksjoner:WeekNum(ttVpiArtBas.LevDato1)).
                  ttPricat.LevUke2 = STRING(rStandardFunksjoner:WeekNum(ttVpiArtBas.LevDato2)).
                  ttPricat.LevUke3 = STRING(rStandardFunksjoner:WeekNum(ttVpiArtBas.LevDato3)).
                  ttPricat.LevUke4 = STRING(rStandardFunksjoner:WeekNum(ttVpiArtBas.LevDato4)).
                END.
            END. /* LEVUKE */  
            IF AVAILABLE ArtPris THEN 
            DO:
                ASSIGN
                    ttPricat.Aktivtilbud = STRING(ArtPris.Tilbud)
                    ttPricat.TilbudPris = (IF ArtPris.tilbud 
                                             THEN STRING(ArtPris.Pris[IF ArtPris.Tilbud THEN 2 ELSE 1])
                                             ELSE '')
                    ttPricat.PrisProfil = STRING(ArtPris.ProfilNr)
                    .
            END.  
        END. /* RECORD-OPPRETTELSE */
          
        RETURN TRUE.

    END METHOD.

    DESTRUCTOR PUBLIC Pricat ( ):
        EMPTY TEMP-TABLE ttPricat.
        EMPTY TEMP-TABLE ttVpiArtBas.
        EMPTY TEMP-TABLE ttVpiStrekkode.
        
    END DESTRUCTOR.

END CLASS.