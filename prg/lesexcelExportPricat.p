/* lesexcelExportPricat.p */

/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER icAvvik AS CHAR NO-UNDO.  /* RS-Avvik:SCREEN-VALUE */
DEFINE INPUT  PARAMETER iEkstVPILevNr AS INTEGER NO-UNDO.
DEFINE OUTPUT PARAMETER cUtfil  AS CHARACTER NO-UNDO.

DEF VAR cFlyttDir       AS CHAR  NO-UNDO.
DEF VAR cFlyttKommando  AS CHAR  NO-UNDO.

DEFINE VARIABLE cFilnavn      AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cKatalog      AS CHARACTER  INITIAL 'c:\home\lindbak\ankommet' NO-UNDO.
DEFINE VARIABLE cTekst        AS CHARACTER  NO-UNDO.
DEFINE VARIABLE lFilId        LIKE VPIFilHode.FilId NO-UNDO.

DEFINE VARIABLE h_Prisko      AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dvpifilhode AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dvpiartbas  AS HANDLE NO-UNDO.
DEFINE VARIABLE pbOk          AS LOG NO-UNDO.
DEFINE VARIABLE piAntLinjer   AS INTEGER NO-UNDO.
DEFINE VARIABLE iEtikett      AS INTEGER NO-UNDO.

DEFINE STREAM Ut.


/* Etikett */
{syspara.i 50 15 49 iEtikett INT}
  
{syspara.i 1 1 52 cKatalog}
IF TRIM(cKatalog) <> '' THEN 
  cKatalog = RIGHT-TRIM(cKatalog,'\').

/* Temp tabell */
{lesexcelvpifil.i &SHARED = "SHARED"}
{ttpricat.i &NEW=" " &SHARED=" "}

ASSIGN
    cFilnavn = "GVPI" + STRING(iEkstVPILevNr,"999") + REPLACE(STRING(TODAY,"99/99/99"),'/','') + "-" + REPLACE(STRING(TIME,"HH:MM:SS"),':','') + ".csv"
    cUtfil   = RIGHT-TRIM(TRIM(cKatalog),"\") + "\" + cFilnavn
    .
 
RUN ByggTemTabell.

RUN eksportPricat.

IF VALID-HANDLE(h_dvpifilhode) THEN
    DELETE PROCEDURE h_dvpifilhode.
IF VALID-HANDLE(h_dvpiartbas) THEN
    DELETE PROCEDURE h_dvpiartbas.
IF VALID-HANDLE(h_PrisKo) THEN
    DELETE PROCEDURE h_PrisKo.

/* **********************  Internal Procedures  *********************** */


PROCEDURE ByggTemTabell:
/*------------------------------------------------------------------------------
        Purpose:                                                                      
        Notes:                                                                        
------------------------------------------------------------------------------*/

DEFINE VARIABLE piLinjeNr AS INTEGER NO-UNDO.

    
piLinjeNr = 1.

FOR EACH TT_Prisfil: /*  Om NY så använd tandem som origvare  */
    FIND FIRST Produsent NO-LOCK WHERE
        Produsent.Beskrivelse BEGINS tt_PrisFil.Vare_Produsent NO-ERROR.
    
    CREATE ttPriKat.
    ASSIGN
        ttPriKat.EkstVPILevNr     = iEkstVPILevNr
        ttPriKat.LinjeNr          = piLinjeNr
        piLinjeNr                 = piLinjeNr  + 1
/*  1 */ ttPriKat.R1              = "R1"
/*  2 */ ttPriKat.LevNr           = STRING(TT_Prisfil.artbas_levnr) 
/*  3 */ ttPriKat.LevModellNr     = STRING(TT_Prisfil.artbas_artnr)
/*  4 */ ttPriKat.EANnr           = LEFT-TRIM(STRING(TT_Prisfil.strekkode_kode),"0")
/*  5 */ ttPriKat.VareTekst       = REPLACE(TRIM(TT_Prisfil.vare_varetekst),","," ")
         ttPriKat.Varetekst       = REPLACE(ttPriKat.VareTekst,';',',')
/*  6 */ ttPriKat.FargeKode       = "" /* Farge er ikke i bruk */
/*  7 */ ttPriKat.FargeTekst      = "" /* Settes blank */
/*  8 */ ttPriKat.Str             = " 1" /* Størrelse 1 */
/*  9 */ ttPriKat.StrTab          = "2" /* Størrelsestype 2 */
/* 10 */ ttPriKat.Varemerke       = TRIM(REPLACE(TRIM(SUBSTR(TT_Prisfil.Vare_Varemerke,1,30)),","," "),'"') 
/* 11 */ ttPriKat.Enh             = tt_Prisfil.vare_Cenhet
         ttPriKat.Enh             = REPLACE(ttPriKat.Enh,';',',')
/* 12 */ ttPriKat.AntIEnh         = REPLACE(TRIM(TRIM(STRING(TT_Prisfil.vare_antpkn,">>>9")),'"'),'.',',') 
/* 13 */ ttPriKat.LevPrisEngros   = REPLACE(TRIM(REPLACE(STRING(TT_Prisfil.pris_engrosn),",","."),'"'),'.',',') 
/* 14 */ ttPriKat.ValKod          = "NOK" 
/* 15 */ ttPriKat.forhRab%        = "" 
/* 16 */ ttPriKat.suppRab%        = "" 
/* 17 */ ttPriKat.VeilPris        = REPLACE(TRIM(REPLACE(STRING(tt_Prisfil.pris_veilpris),",","."),'"'),'.',',') 
/* 18 */ ttPriKat.PAKstru         = "" 
/* 19 */ ttPriKat.LevUke1         = "" 
/* 20 */ ttPriKat.LevUke2         = "" 
/* 21 */ ttPriKat.LevUke3         = "" 
/* 22 */ ttPriKat.LevUke4         = "" 
/* 23 */ ttPriKat.VareGruppe      = STRING(TT_Prisfil.vare_hgr) 
/* 24 */ ttPriKat.LevNavn         = "" 
/* 25 */ ttPriKat.LevKod          = REPLACE(STRING(TT_Prisfil.pris_bestnr),","," ")
/* 26 */ ttPriKat.nettoForh       = "" 
/* 27 */ ttPriKat.kalkForh        = "" 
/* 28 */ ttPriKat.BFforh          = "" 
/* 29 */ ttPriKat.nettoSupp       = "" 
/* 30 */ ttPriKat.kalkSupp        = "" 
/* 31 */ ttPriKat.BFsupp          = "" 
/* 32 */ ttPriKat.MarkedsPris     = REPLACE(TRIM(REPLACE(STRING(TT_Prisfil.pris_utprisn),",","."),'"'),'.',',') 
/* 33 */ ttPriKat.Sortiment       = "" 
/* 34 */ ttPriKat.Sesong          = "" 
/* 35 */ ttPriKat.VPIBildeKode    = "" 
/* 36 */ ttPriKat.Merknad         = "" 
/* 37 */ ttPriKat.KjedeValutaPris = "" 
/* 38 */ ttPriKat.KjedeProdusent  = ""  
/* 39 */ ttPriKat.ERPNr           = ""
/* 40 */ ttPriKat.SalgsEnhetsType = "1"
/* 41 */ ttPriKat.AktivFraDato    = ""
/* 42 */ ttPriKat.AktivTilDato    = ""
/* 43 */ ttPriKat.Bongtekst       = REPLACE(TRIM(SUBSTR(TT_Prisfil.vare_varetekst,1,20)),","," ")
         ttPriKat.Bongtekst       = REPLACE(ttPriKat.Bongtekst,';',',')
/* 44 */ ttPriKat.Etikettekst1    = REPLACE(TRIM(SUBSTR(TT_Prisfil.vare_varetekst,1,30)),","," ")
         ttPriKat.Etikettekst1    = REPLACE(ttPriKat.Etikettekst1,';',',')
         ttPriKat.Etikettekst1    = (IF ttPriKat.Etikettekst1 = '' THEN ttPriKat.Varetekst ELSE ttPriKat.Etikettekst1)
/* 45 */ ttPriKat.Funksjonskode   = "N"
/* 46 */ ttPriKat.Mva_Proc        = REPLACE(TRIM(REPLACE(STRING(TT_Prisfil.vare_mva),",","."),'"'),'.',',')
/* 47 */ ttPriKat.LinkVare        = IF DECI(TT_Prisfil.vare_link) > 0 THEN STRING(TT_Prisfil.vare_link) ELSE "" 
/* 48 */ ttPriKat.PantBelop       = "" 
/* 49 */ ttPriKat.Filial          = TRIM(STRING(tt_PrisFil.Butikk_ButNr,">>>>>9")) 
/* 50 */ ttPriKat.Produsent       = IF AVAILABLE Produsent THEN STRING(Produsent.ProdNr) ELSE '0'
/* 51 */ ttPriKat.Mengde          = REPLACE(TRIM(IF TT_Prisfil.vare_mengde = 1 THEN "1" ELSE REPLACE(STRING(TT_Prisfil.vare_mengde),",","."),'"'),'.',',') /* Konv. faktor jamførpris */

/* 52 */ ttPriKat.JamforEnhet     = STRING(tt_Prisfil.vare_Cenhet)
/*/* 53 */ ttPriKat.Kontrolleres    = */
/*/* 99 */ ttPriKat.Opphav          = 1*/                                                                                                             

/* 54  ttPriKat.ArtikkelNr      */
/* 55  ttPriKat.OpprettArtikkel */
         ttPriKat.Etikett         = iEtikett
         ttPriKat.Lager           = NO 
        .      
     /* Hvis filial er prefikset med BS, skal dette strippes. */
     IF ttPriKat.Filial BEGINS 'BS' THEN 
       ttPriKat.Filial = SUBSTRING(ttPriKat.Filial,3).
  END.
END PROCEDURE.

PROCEDURE eksportPricat:
/*------------------------------------------------------------------------------
		Purpose:  																	  
		Notes:  																	  
------------------------------------------------------------------------------*/
DEFINE VARIABLE piAntLinjer AS INTEGER NO-UNDO.

DEFINE BUFFER bVPIFilHode FOR VPIFilHode.

OUTPUT STREAM Ut TO VALUE(cUtfil).
EKSPORTFIL:                    
FOR EACH ttPriKat
    BREAK BY ttPriKat.LevModellNr
          BY ttPriKat.VareTekst     
          BY ttPriKat.FargeTekst    
          BY ttPriKat.SeqNrStr           
          BY ttPriKat.MarkedsPris:
          
    piAntLinjer = piAntLinjer + 1.      
          
    PUT STREAM Ut UNFORMATTED  
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
    /* 89 */ ";" 
    /* 90 */ ttPriKat.Etikett ";"        
    /* 91 */ ";" 
    /* 92 */ ";" 
    /* 93 */ ";" 
    /* 94 */ ";" 
    /* 95 */ ";" 
    /* 96 */ ";" 
    /* 97 */ ttPriKat.BehStatus ";" 
    /* 98 */ ttPriKat.Grunnsortiment 
    SKIP.
END. /* EKSPORTFIL */
OUTPUT STREAM Ut CLOSE.

/* Finner FilId */
IF SEARCH(cUtFil) <> ? THEN 
DO FOR bVPIFilHode:
    FILE-INFO:FILE-NAME = cUtfil.

    FIND LAST bVPIFilHode NO-LOCK NO-ERROR.
    IF AVAILABLE bVPIFilHode THEN
      lFilId = bVPIFilHode.FilId + 1.
    ELSE
      lFilId = 1.
    DO TRANSACTION:
        CREATE bVPIFilHode.
        ASSIGN
          bVPIFilHode.FilId        = lFilId
          bVPIFilHode.FilNavn      = cFilnavn
          bVPIFilHode.Katalog      = RIGHT-TRIM(TRIM(cKatalog),"\")
          bVPIFilHode.Dato         = FILE-INFO:FILE-MOD-DATE
          bVPIFilHode.Kl           = STRING(FILE-INFO:FILE-MOD-TIME,"HH:MM:SS")
          bVPIFilHode.Storrelse    = FILE-INFO:FILE-SIZE
          bVPIFilHode.AntLinjer    = piAntLinjer
          bVPIFilHode.VPIFilType   = 1 /* VPI */
          bVPIFilHode.VPIFilStatus = 1
          bVPIFilHode.EkstVPILevNr = iEkstVPILevNr
          .
        RELEASE bVPIFilHode.
    END. /* TRANSACTION */
END. /* bVPIFilHode*/

IF lFilId > 0 THEN 
DO:
    pbOk = FALSE.    
    /* Starter program for lasting av VPI mottak */
    IF NOT VALID-HANDLE(h_PrisKo) THEN
        RUN prisko.p PERSISTENT SET h_PrisKo.
    IF NOT VALID-HANDLE(h_dvpifilhode) THEN
        RUN dvpifilhode.w PERSISTENT SET h_dvpifilhode.
    
    IF NOT VALID-HANDLE(h_dvpiartbas) THEN
    DO:
        RUN dvpiartbas.w PERSISTENT SET h_dvpiartbas.
        RUN SettAutoImport IN h_dvpiartbas (INPUT TRUE).
    END.
    
    /* Leser inn filen. */
    RUN LesInnFil IN h_dvpifilhode (INPUT STRING(lFilId), 
                               OUTPUT pbOk, 
                               OUTPUT piAntLinjer).
    /* Pakker ut fil. */
    RUN PakkUtFil IN h_dvpifilhode (INPUT STRING(lFilId)).

    FIND VPIFilhode NO-LOCK WHERE
    VPIFilHode.FilId = lFilId NO-ERROR.
/*     
MESSAGE 'test-1' SKIP
'iEkstVPILevNr' iEkstVPILevNr SKIP
'VPIFilHode.EkstVPILevNr' VPIFilHode.EkstVPILevNr skip
'VPIFilHode.FilNavn' VPIFilHode.FilNavn SKIP
'VPIFilHode.Katalog' VPIFilHode.Katalog SKIP
'VPIFilHode.Storrelse' VPIFilHode.Storrelse skip
'h_PrisKo' valid-handle(h_PrisKo) SKIP
'h_dvpifilhode' VALID-HANDLE(h_dvpifilhode) skip
'h_dvpiartbas' valid-handle(h_dvpiartbas) SKIP
pbOk piAntLinjer
VIEW-AS ALERT-BOX.    
*/    
END.

END PROCEDURE.

