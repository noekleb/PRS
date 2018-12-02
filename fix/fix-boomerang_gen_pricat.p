CURRENT-WINDOW:WIDTH = 300.

DEF STREAM Inn.
DEF STREAM Ut.
DEF STREAM Inn2.
DEF STREAM Pricat.

DEF VAR cFilNavn    AS CHAR NO-UNDO.
DEF VAR cUtFilNavn  AS CHAR NO-UNDO.
DEF VAR c2FilNavn   AS CHAR NO-UNDO.
DEF VAR cPricatFil  AS CHAR NO-UNDO.
DEF VAR cKode       AS CHAR NO-UNDO.
DEF VAR cLinje      AS CHAR NO-UNDO.
DEF VAR cPriLinje   AS CHAR NO-UNDO.
DEF VAR iAntLinjer  AS INT  NO-UNDO.
DEF VAR lDec        AS DEC NO-UNDO.
/* konvertert 17/1-12 12:30 
ASSIGN
    cFilNavn   = 'C:\ArkivDokument\Kunder\GANT\Boomerang\VPI\VPI1\VPIBOEAN1Eankoder_for_S13_Boomerang.csv'
    c2FilNavn   = 'C:\ArkivDokument\Kunder\GANT\Boomerang\VPI\VPI1\VPIBOART1Prisfila Stepp2_norge spring13.csv'
    cUtFilNavn = 'C:\ArkivDokument\Kunder\GANT\Boomerang\VPI\VPI1\EAN_vasket.csv'
    cPricatFil = 'C:\ArkivDokument\Kunder\GANT\Boomerang\VPI\VPI1\VPI010Boomerang1.csv'
    .
*/
ASSIGN
    cFilNavn   = 'C:\ArkivDokument\Kunder\GANT\Boomerang\VPI\VPI2\VPIBOEAN2Art_fil_med_fob_priser_Step1.csv'
    c2FilNavn  = 'C:\ArkivDokument\Kunder\GANT\Boomerang\VPI\VPI2\VPIBOART2Prisfila_stepp_2.csv'
    cUtFilNavn = 'C:\ArkivDokument\Kunder\GANT\Boomerang\VPI\VPI2\EAN_vasket.csv'
    cPricatFil = 'C:\ArkivDokument\Kunder\GANT\Boomerang\VPI\VPI2\VPI010Boomerang2.csv'
    .
                            

{ttpricat.i &NEW=" " &SHARED=" "}
DEF BUFFER bttPrikat FOR ttPrikat.

OUTPUT STREAM Ut     TO VALUE(cUtFilNavn) NO-ECHO.
OUTPUT STREAM Pricat TO VALUE(cPricatFil) NO-ECHO.

INPUT  STREAM Inn    FROM VALUE(cFilNavn) NO-ECHO.
REPEAT:
    IMPORT STREAM Inn UNFORMATTED
        cLinje.                  
    ASSIGN
        lDec  = DEC(TRIM(ENTRY(6,cLinje,';')))
        cKode = TRIM(ENTRY(6,cLinje,';')) NO-ERROR.
    IF ERROR-STATUS:ERROR = FALSE THEN
    DO:
        RUN bibl_chkean.p (INPUT-OUTPUT cKode).
        RUN byggTmpTabell1.
    END.
END.
INPUT  STREAM Inn    CLOSE.

INPUT  STREAM Inn2   FROM VALUE(c2FilNavn) NO-ECHO.
REPEAT:
    IMPORT STREAM Inn2 UNFORMATTED
        cLinje.
    RUN byggTmpTabell2.
END.
INPUT  STREAM Inn2   CLOSE.

RUN EksportVPIFil.
OUTPUT STREAM Pricat CLOSE.
OUTPUT STREAM Ut     CLOSE.

PROCEDURE byggTmpTabell1:
DEF VAR piLinjeNr AS INT NO-UNDO.

  IF NOT CAN-FIND(Sasong WHERE
                  Sasong.Sasong = INT(SUBSTRING(REPLACE(TRIM(ENTRY(1,cLinje,';'),'"'),'USC','018'),1,2))) THEN
  DO:
      CREATE Sasong.
      ASSIGN
          Sasong.Sasong   = INT(SUBSTRING(REPLACE(TRIM(ENTRY(1,cLinje,';'),'"'),'USC','018'),1,2))
          Sasong.SasBeskr = "Sesong " + SUBSTRING(REPLACE(TRIM(ENTRY(1,cLinje,';'),'"'),'USC','018'),1,2)
          .
  END.

    piLinjeNr = 1.
    CREATE ttPriKat.
    ASSIGN
        ttPriKat.EkstVPILevNr     = 1
        ttPriKat.LinjeNr          = piLinjeNr
        piLinjeNr                 = piLinjeNr  + 1
/*  1 */ ttPriKat.R1              = "R1"
/*  2 */ ttPriKat.LevNr           = '100' 
/*  3 */ ttPriKat.LevModellNr     = SUBSTRING(REPLACE(TRIM(ENTRY(1,cLinje,';'),'"'),'USC','018'),7,4)
/*  4 */ ttPriKat.EANnr           = TRIM(ENTRY(6,cLinje,';'),'"')
/*  5 */ ttPriKat.VareTekst       = ""
/*  6 */ ttPriKat.FargeKode       = TRIM(ENTRY(2,cLinje,';'),'"')
/*  7 */ ttPriKat.FargeTekst      = TRIM(ENTRY(3,cLinje,';'),'"')
/*  8 */ ttPriKat.Str             = TRIM(ENTRY(4,cLinje,';'),'"') + 
                                    (IF TRIM(ENTRY(4,cLinje,';'),'"') <> '' THEN TRIM(ENTRY(4,cLinje,';'),'"') ELSE '')
/*  9 */ ttPriKat.StrTab          = "2" /* Størrelsestype 2 */
/* 10 */ ttPriKat.Varemerke       = SUBSTRING(REPLACE(TRIM(ENTRY(1,cLinje,';'),'"'),'USC','018'),3,1)
/* 11 */ ttPriKat.Enh             = 'Stk'
/* 12 */ ttPriKat.AntIEnh         = '1'
/* 13 */ ttPriKat.LevPrisEngros   = ""
/* 14 */ ttPriKat.ValKod          = "NOK" 
/* 15 */ ttPriKat.forhRab%        = "" 
/* 16 */ ttPriKat.suppRab%        = "" 
/* 17 */ ttPriKat.VeilPris        = ""
/* 18 */ ttPriKat.PAKstru         = "" 
/* 19 */ ttPriKat.LevUke1         = "" 
/* 20 */ ttPriKat.LevUke2         = "" 
/* 21 */ ttPriKat.LevUke3         = "" 
/* 22 */ ttPriKat.LevUke4         = "" 
/* 23 */ ttPriKat.VareGruppe      = SUBSTRING(REPLACE(TRIM(ENTRY(1,cLinje,';'),'"'),'USC','018'),4,3) 
/* 24 */ ttPriKat.LevNavn         = "" 
/* 25 */ ttPriKat.LevKod          = TRIM(ENTRY(1,cLinje,';'),'"') + '-' + STRING(INT(TRIM(ENTRY(2,cLinje,';'),'"')),"99")
/* 26 */ ttPriKat.nettoForh       = "" 
/* 27 */ ttPriKat.kalkForh        = "" 
/* 28 */ ttPriKat.BFforh          = "" 
/* 29 */ ttPriKat.nettoSupp       = "" 
/* 30 */ ttPriKat.kalkSupp        = "" 
/* 31 */ ttPriKat.BFsupp          = "" 
/* 32 */ ttPriKat.MarkedsPris     = ""
/* 33 */ ttPriKat.Sortiment       = "" 
/* 34 */ ttPriKat.Sesong          = SUBSTRING(REPLACE(TRIM(ENTRY(1,cLinje,';'),'"'),'USC','018'),1,2) 
/* 35 */ ttPriKat.VPIBildeKode    = "" 
/* 36 */ ttPriKat.Merknad         = "" 
/* 37 */ ttPriKat.KjedeValutaPris = "" 
/* 38 */ ttPriKat.KjedeProdusent  = ""  
/* 39 */ ttPriKat.ERPNr           = ""
/* 40 */ ttPriKat.SalgsEnhetsType = ""
/* 41 */ ttPriKat.AktivFraDato    = ""
/* 42 */ ttPriKat.AktivTilDato    = ""
/* 43 */ ttPriKat.Bongtekst       = ""
/* 44 */ ttPriKat.Etikettekst1    = ""
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

PROCEDURE byggTmpTabell2:
DEF VAR piLinjeNr AS INT NO-UNDO.
DEF VAR iAnt AS INT NO-UNDO.

  IF NOT CAN-FIND(Sasong WHERE
                  Sasong.Sasong = INT(SUBSTRING(REPLACE(TRIM(ENTRY(2,cLinje,';'),'"'),'USC','018'),1,2))) THEN
  DO:
      CREATE Sasong.
      ASSIGN
          Sasong.Sasong   = INT(SUBSTRING(REPLACE(TRIM(ENTRY(2,cLinje,';'),'"'),'USC','018'),1,2))
          Sasong.SasBeskr = "Sesong " + SUBSTRING(REPLACE(TRIM(ENTRY(2,cLinje,';'),'"'),'USC','018'),1,2)
          .
  END.

  FIND LAST HuvGr NO-LOCK NO-ERROR.
  IF AVAILABLE HuvGr 
    THEN iAnt = HuvGr.Hg + 1.
  ELSE iAnt = 1.

  IF NOT CAN-FIND(FIRST HuvGr WHERE 
                HuvGr.HgBeskr = TRIM(ENTRY(6,cLinje,';'))) THEN
  DO:
    CREATE HuvGr.
    ASSIGN
        HuvGr.Hg      = iant
        HuvGr.HgBeskr = TRIM(ENTRY(6,cLinje,';'))
        HuvGr.AvdelingNr = 1
        .
  END.

  IF NOT CAN-FIND(FIRST VarGr WHERE 
                  VarGr.Vg = INT(SUBSTRING(REPLACE(TRIM(ENTRY(2,cLinje,';')),'USC','018'),4,3))) THEN
  DO:
      CREATE VarGr.
      ASSIGN
          VarGr.Vg        = INT(SUBSTRING(REPLACE(TRIM(ENTRY(2,cLinje,';')),'USC','018'),4,3))
          VarGr.VgBeskr   = "** Automatisk opprettet"
          VarGr.Hg        = HuvGr.Hg
          VarGr.Kost_Proc = 65
          VarGr.MomsKod   = 1
          .
  END.

  FOR EACH ttPrikat WHERE
        ttPriKat.LevKod = TRIM(ENTRY(2,cLinje,';'),'"') + '-' + STRING(INT(TRIM(ENTRY(3,cLinje,';'),'"')),"99"):
    ASSIGN
/*  5 */ ttPriKat.VareTekst       = TRIM(ENTRY(4,cLinje,';'),'"')
         ttPriKat.Varetekst       = REPLACE(ttPriKat.VareTekst,';',',')
/* 11 */ ttPriKat.Enh             = TRIM(ENTRY(8,cLinje,';'),'"')
/* 12 */ ttPriKat.AntIEnh         = '1'
/* 13 */ ttPriKat.LevPrisEngros   = TRIM(ENTRY(17,cLinje,';'),'"')
/* 14 */ ttPriKat.ValKod          = "NOK" 
/* 15 */ ttPriKat.forhRab%        = "" 
/* 16 */ ttPriKat.suppRab%        = "" 
/* 17 */ ttPriKat.VeilPris        = TRIM(ENTRY(18,cLinje,';'),'"')
/* 18 */ ttPriKat.PAKstru         = "" 
/* 19 */ ttPriKat.LevUke1         = "" 
/* 20 */ ttPriKat.LevUke2         = "" 
/* 21 */ ttPriKat.LevUke3         = "" 
/* 22 */ ttPriKat.LevUke4         = "" 
/* 24 */ ttPriKat.LevNavn         = "" 
/* 26 */ ttPriKat.nettoForh       = "" 
/* 27 */ ttPriKat.kalkForh        = "" 
/* 28 */ ttPriKat.BFforh          = "" 
/* 29 */ ttPriKat.nettoSupp       = "" 
/* 30 */ ttPriKat.kalkSupp        = "" 
/* 31 */ ttPriKat.BFsupp          = "" 
/* 32 */ ttPriKat.MarkedsPris     = TRIM(ENTRY(18,cLinje,';'),'"') 
/* 33 */ ttPriKat.Sortiment       = "" 
/* 35 */ ttPriKat.VPIBildeKode    = "" 
/* 36 */ ttPriKat.Merknad         = "" 
/* 37 */ ttPriKat.KjedeValutaPris = "" 
/* 38 */ ttPriKat.KjedeProdusent  = ""  
/* 40 */ ttPriKat.SalgsEnhetsType = ""
/* 41 */ ttPriKat.AktivFraDato    = ""
/* 42 */ ttPriKat.AktivTilDato    = ""
/* 43 */ ttPriKat.Bongtekst       = ttPriKat.Varetekst
/* 44 */ ttPriKat.Etikettekst1    = ttPriKat.Varetekst
/* 46 */ ttPriKat.Mva_Proc        = ""
/* 47 */ ttPriKat.LinkVare        = ""
/* 48 */ ttPriKat.PantBelop       = ""
/* 49 */ ttPriKat.Filial          = "" 
/* 50 */ ttPriKat.Produsent       = ""
/* 99 */ ttPriKat.Opphav          = ''
         .
  END.

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
    /* 99 */ ttPriKat.Opphav                 
    SKIP.      
END. /* EKSPORTER */


END PROCEDURE.

