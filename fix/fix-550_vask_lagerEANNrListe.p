CURRENT-WINDOW:WIDTH = 300.

DEF STREAM Inn.
DEF STREAM Ut.
DEF STREAM Pricat.

DEF VAR cVg         AS CHAR NO-UNDO.
DEF VAR cFilNavn    AS CHAR NO-UNDO.
DEF VAR cUtFilNavn  AS CHAR NO-UNDO.
DEF VAR cPricatFil  AS CHAR NO-UNDO.
DEF VAR cKode       AS CHAR NO-UNDO.
DEF VAR cLinje      AS CHAR NO-UNDO.
DEF VAR cPriLinje   AS CHAR NO-UNDO.
DEF VAR iAntLinjer  AS INT  NO-UNDO.

ASSIGN
    cFilNavn   = 'c:\appdir\HasleLagerverdiEtterTellin_Anders.csv'
    cUtFilNavn = 'c:\appdir\VPI620Lager_1.csv'
    cPricatFil = 'c:\appdir\VPI620Lager_2.csv'
    .


{ttpricat.i &NEW=" " &SHARED=" "}
DEF BUFFER bttPrikat FOR ttPrikat.

INPUT  STREAM Inn    FROM VALUE(cFilNavn) NO-ECHO.
OUTPUT STREAM Ut     TO VALUE(cUtFilNavn) NO-ECHO.
OUTPUT STREAM Pricat TO VALUE(cPricatFil) NO-ECHO.

REPEAT:
    IMPORT STREAM Inn UNFORMATTED
        cLinje.
    ASSIGN
        cKode = ENTRY(3,cLinje,';') NO-ERROR.
    IF ERROR-STATUS:ERROR = FALSE THEN
    DO:
        RUN bibl_chkean.p (INPUT-OUTPUT cKode).
        IF NOT CAN-FIND(Strekkode WHERE
                        Strekkode.Kode = cKode) THEN
        DO:
            PUT STREAM Ut UNFORMATTED 
              cLinje SKIP.
            RUN byggTmpTabell.
        END.
    END.
END.
RUN EksportVPIFil.
OUTPUT STREAM Pricat CLOSE.
OUTPUT STREAM Ut     CLOSE.
INPUT  STREAM Inn    CLOSE.

PROCEDURE byggTmpTabell:
DEF VAR piLinjeNr AS INT NO-UNDO.

    FIND FIRST VarGr NO-LOCK WHERE
        VarGr.VgBeskr = TRIM(ENTRY(5,cLinje,';'),'"') NO-ERROR.
    IF AVAILABLE VarGr THEN
        cVg = STRING(VarGr.Vg).
    ELSE
        cVg = '9399'.

    piLinjeNr = 1.
    CREATE ttPriKat.
    ASSIGN
        ttPriKat.EkstVPILevNr     = 1
        ttPriKat.LinjeNr          = piLinjeNr
        piLinjeNr                 = piLinjeNr  + 1
/*  1 */ ttPriKat.R1              = "R1"
/*  2 */ ttPriKat.LevNr           = '999999' 
/*  3 */ ttPriKat.LevModellNr     = ''
/*  4 */ ttPriKat.EANnr           = cKode
/*  5 */ ttPriKat.VareTekst       = TRIM(ENTRY(5,cLinje,';'),'"')
         ttPriKat.Varetekst       = REPLACE(ttPriKat.VareTekst,';',',')
/*  6 */ ttPriKat.FargeKode       = "" /* Farge er ikke i bruk */
/*  7 */ ttPriKat.FargeTekst      = "" /* Settes blank */
/*  8 */ ttPriKat.Str             = "1"
/*  9 */ ttPriKat.StrTab          = "2" /* Størrelsestype 2 */
/* 10 */ ttPriKat.Varemerke       = ''
/* 11 */ ttPriKat.Enh             = 'Stk'
/* 12 */ ttPriKat.AntIEnh         = '1'
/* 13 */ ttPriKat.LevPrisEngros   = TRIM(ENTRY(13,cLinje,';'),'"')
/* 14 */ ttPriKat.ValKod          = "NOK" 
/* 15 */ ttPriKat.forhRab%        = "" 
/* 16 */ ttPriKat.suppRab%        = "" 
/* 17 */ ttPriKat.VeilPris        = TRIM(ENTRY(14,cLinje,';'),'"')
/* 18 */ ttPriKat.PAKstru         = "" 
/* 19 */ ttPriKat.LevUke1         = "" 
/* 20 */ ttPriKat.LevUke2         = "" 
/* 21 */ ttPriKat.LevUke3         = "" 
/* 22 */ ttPriKat.LevUke4         = "" 
/* 23 */ ttPriKat.VareGruppe      = cVg 
/* 24 */ ttPriKat.LevNavn         = "" 
/* 25 */ ttPriKat.LevKod          = ""
/* 26 */ ttPriKat.nettoForh       = "" 
/* 27 */ ttPriKat.kalkForh        = "" 
/* 28 */ ttPriKat.BFforh          = "" 
/* 29 */ ttPriKat.nettoSupp       = "" 
/* 30 */ ttPriKat.kalkSupp        = "" 
/* 31 */ ttPriKat.BFsupp          = "" 
/* 32 */ ttPriKat.MarkedsPris     = TRIM(ENTRY(14,cLinje,';'),'"') 
/* 33 */ ttPriKat.Sortiment       = "" 
/* 34 */ ttPriKat.Sesong          = "" 
/* 35 */ ttPriKat.VPIBildeKode    = "" 
/* 36 */ ttPriKat.Merknad         = "" 
/* 37 */ ttPriKat.KjedeValutaPris = "" 
/* 38 */ ttPriKat.KjedeProdusent  = ""  
/* 39 */ ttPriKat.ERPNr           = ""
/* 40 */ ttPriKat.SalgsEnhetsType = ""
/* 41 */ ttPriKat.AktivFraDato    = ""
/* 42 */ ttPriKat.AktivTilDato    = ""
/* 43 */ ttPriKat.Bongtekst       = ttPriKat.Varetekst
/* 44 */ ttPriKat.Etikettekst1    = ttPriKat.Varetekst
/* 45 */ ttPriKat.Funksjonskode   = "N"
/* 46 */ ttPriKat.Mva_Proc        = ""
/* 47 */ ttPriKat.LinkVare        = ""
/* 48 */ ttPriKat.PantBelop       = ""
/* 49 */ ttPriKat.Filial          = "" 
/* 50 */ ttPriKat.Produsent       = ""
/* 51 */ ttPriKat.Mengde          = "1"
/* 52 */ ttPriKat.JamforEnhet     = 'Stk'
/* 53 */ ttPriKat.Kontrolleres    = FALSE
/* 99 */ ttPriKat.Opphav          = ''
/* 54  ttPriKat.ArtikkelNr      */
/* 55  ttPriKat.OpprettArtikkel */
         ttPriKat.Etikett         = 1
         ttPriKat.Lager           = TRUE        
         .
END PROCEDURE.

PROCEDURE EksportVPIFil :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR plFilId      AS DEC NO-UNDO.
DEF VAR pbOk         AS LOG NO-UNDO.
DEF VAR piAntLinjer  AS LOG NO-UNDO.
DEF VAR plArtikkelNr AS DEC NO-UNDO.
DEFINE VARIABLE piLoop AS INTEGER NO-UNDO.

DEF BUFFER bVPIFilHode FOR VPIFilHode.

EKSPORTER:                    
FOR EACH ttPrikat
    BREAK BY ttPriKat.ButikkNr
          BY ttPriKat.LevModellNr
          BY ttPriKat.VareTekst     
          BY ttPriKat.FargeTekst    
          BY ttPriKat.SeqNrStr           
          BY ttPriKat.MarkedsPris:
    
    PUT STREAM Pricat UNFORMATTED 
    /*  1 */ ttPriKat.R1 ";"           
    /*  2 */ ttPriKat.LevNr ";"        
    /*  3 */ ttPriKat.LevModellNr ";"   
    /*  4 */ ttPriKat.EANnr ";"         
    /*  5 */ ttPriKat.VareTekst ";"     
    /*  6 */ ttPriKat.FargeKode ";"     
    /*  7 */ ttPriKat.FargeTekst ";"    
    /*  8 */ ttPriKat.Str ";"           
    /*  9 */ ttPriKat.StrTab ";"        
    /* 10 */ ttPriKat.Varemerke ";"     
    /* 11 */ ttPriKat.Enh ";"           
    /* 12 */ ttPriKat.AntIEnh ";"       
    /* 13 */ ttPriKat.LevPrisEngros ";" 
    /* 14 */ ttPriKat.ValKod ";"        
    /* 15 */ ttPriKat.forhRab% ";"      
    /* 16 */ ttPriKat.suppRab% ";"      
    /* 17 */ ttPriKat.VeilPris ";"      
    /* 18 */ ttPriKat.PAKstru ";"       
    /* 19 */ ttPriKat.LevUke1 ";"       
    /* 20 */ ttPriKat.LevUke2 ";"       
    /* 21 */ ttPriKat.LevUke3 ";"       
    /* 22 */ ttPriKat.LevUke4 ";"       
    /* 23 */ ttPriKat.VareGruppe ";"    
    /* 24 */ ttPriKat.LevNavn ";"       
    /* 25 */ ttPriKat.LevKod ";"  
    /* 26 */ ttPriKat.nettoForh ";"     
    /* 27 */ ttPriKat.kalkForh ";"      
    /* 28 */ ttPriKat.BFforh ";"        
    /* 29 */ ttPriKat.nettoSupp ";"     
    /* 30 */ ttPriKat.kalkSupp ";"      
    /* 31 */ ttPriKat.BFsupp ";"        
    /* 32 */ ttPriKat.MarkedsPris ";"   
    /* 33 */ ttPriKat.Sortiment ";"     
    /* 34 */ ttPriKat.Sesong ";"        
    /* 35 */ ttPriKat.VPIBildeKode ";"
    /* 36 */ ttPriKat.Merknad ";"     
    /* 37 */ ttPriKat.KjedeValutaPris ";"  
    /* 38 */ ttPriKat.KjedeProdusent ";"    
    /* 39 */ ttPriKat.ERPNr ";"           
    /* 40 */ ttPriKat.SalgsEnhetsType ";" 
    /* 41 */ ttPriKat.AktivFraDato ";"    
    /* 42 */ ttPriKat.AktivTilDato ";"    
    /* 43 */ ttPriKat.Bongtekst ";"       
    /* 44 */ ttPriKat.Etikettekst1 ";"    
    /* 45 */ ttPriKat.Funksjonskode ";"   
    /* 46 */ ttPriKat.Mva_Proc ";"        
    /* 47 */ ttPriKat.LinkVare ";"         
    /* 48 */ ttPriKat.PantBelop ";"       
    /* 49 */ ttPriKat.Filial ";"          
    /* 50 */ ttPriKat.Produsent ";"       
    /* 51 */ ttPriKat.Mengde ";"          
    /* 52 */ ttPriKat.JamforEnhet ";"     
    /* 53 */ ttPriKat.Kontrolleres ";"
    /* 54 */ ttPriKat.ArtikkelNr ";"
    /* 55 */ ttPriKat.OpprettArtikkel ";" 
    /* 56 */ ttPriKat.PosterPrisending ";" 
    /* 57 */ ";" 
    /* 58 */ ";" 
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
    /* 76 */ ";" 
    /* 77 */ ";" 
    /* 78 */ ";" 
    /* 79 */ ";" 
    /* 80 */ ";" 
    /* 81 */ ";" 
    /* 82 */ ";" 
    /* 83 */ ";" 
    /* 84 */ ";" 
    /* 85 */ ";" 
    /* 86 */ ";" 
    /* 87 */ ";" 
    /* 88 */ ";" 
    /* 89 */ ttPriKat.Lager ";" 
    /* 90 */ ttPriKat.Etikett ";"
    /* 91 */ ";" 
    /* 92 */ ";" 
    /* 93 */ ";" 
    /* 94 */ ";" 
    /* 95 */ ";" 
    /* 96 */ ";" 
    /* 97 */ ";" 
    /* 98 */ ";" 
    /* 99 */ "ttPriKat.Opphav"                 
    SKIP.      
END. /* EKSPORTER */


END PROCEDURE.

