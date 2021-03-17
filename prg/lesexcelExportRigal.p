/* lesexcelExportRigal.p */

/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER icAvvik AS CHAR NO-UNDO.  /* RS-Avvik:SCREEN-VALUE */
DEFINE OUTPUT PARAMETER cUtfil  AS CHARACTER NO-UNDO.

DEF VAR cSekvensFil  AS CHARACTER INIT "RigalRVPIsekvens.txt"  NO-UNDO.
DEF VAR cRigalversion AS CHAR INIT "RIGAL02,8.0" NO-UNDO.
DEF VAR cFlyttDir       AS CHAR  NO-UNDO.
DEF VAR cFlyttKommando  AS CHAR  NO-UNDO.
DEF VAR cExcel2RigalDir   AS CHAR  INIT "C:\home\lindbak\ankommet" NO-UNDO.
DEFINE VARIABLE cEksportDir AS CHARACTER INIT "C:\home\lindbak\sendes" NO-UNDO.
DEFINE VARIABLE cImportDir AS CHARACTER INIT "C:\home\lindbak\ankommet" NO-UNDO.

DEFINE VARIABLE cRigalEntries AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cRigalstr     AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cFilnamn      AS CHARACTER  NO-UNDO.
DEFINE VARIABLE iSekvens      AS INTEGER    NO-UNDO.
DEFINE VARIABLE cStr          AS CHARACTER  NO-UNDO.
DEFINE VARIABLE iRigalNr      AS INTEGER    NO-UNDO.
DEFINE VARIABLE bDistribIPS   AS LOG NO-UNDO.
DEFINE VARIABLE bDistribImp   AS LOG NO-UNDO.
DEFINE VARIABLE cTekst        AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cIPSFilNavn   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cImpFilNavn   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cEDB-System   LIKE ImpKonv.EDB-System NO-UNDO.
DEFINE VARIABLE cImpTabell    LIKE ImpKonv.Tabell     NO-UNDO.
DEFINE VARIABLE plFilId       AS DECIMAL NO-UNDO.
DEFINE VARIABLE iAntallLinjer AS INTEGER NO-UNDO.

DEFINE BUFFER bVPIFilHode FOR VPIFilHode.

ASSIGN
  cEDB-System = 'NæraDej'
  cImpTabell  = 'Butiker'
  .

/* Styrer om det skal legges ut RIGAL filer til butikkene */
{syspara.i 50 23 1 cTekst}
IF CAN-DO('1,j,ja,true,y,yes',cTekst) THEN 
  bDistribIPS = TRUE.

/* Styrer om artiklene også skal leses inn i VPI databasen */
{syspara.i 50 23 2 cTekst}
IF CAN-DO('1,j,ja,true,y,yes',cTekst) THEN 
  bDistribImp = TRUE.

{syspara.i 50 23 3 cTekst}
IF TRIM(cTekst) <> '' THEN 
  cEksportDir = RIGHT-TRIM(cTekst,'\').

{syspara.i 50 23 4 cTekst}
IF TRIM(cTekst) <> '' THEN 
  cImportDir = RIGHT-TRIM(cTekst,'\').

{lesexcelvpifil.i &SHARED = "SHARED"}

/* Katalog hvor sekvensnummer filen legges. */
{syspara.i 50 23 5 cExcel2RigalDir}
cExcel2RigalDir = RIGHT-TRIM(cExcel2RigalDir,"\").
IF SEARCH(cExcel2RigalDir + "\" + cSekvensfil) = ? THEN DO:
  OUTPUT TO VALUE(cExcel2RigalDir + "\" + cSekvensfil).
  PUT UNFORMATTED "0" SKIP.
    OUTPUT CLOSE.
END.

/* Rigalnr danner filekstent. */
iRigalNr = 1.

INPUT FROM VALUE(cExcel2RigalDir + "\" + cSekvensfil).
  IMPORT UNFORMATTED cStr.
  INPUT CLOSE.
  iSekvens = INT(cStr) + 1.
  OUTPUT TO VALUE(cExcel2RigalDir + "\" + cSekvensfil).
  PUT UNFORMATTED iSekvens SKIP.
OUTPUT CLOSE.

ASSIGN cFilnamn    = "v" + STRING(iSekvens,"999999") + "1." + STRING(iRigalNr,"999")
       cIPSFilNavn = cEksportDir + '\' + "v" + STRING(iSekvens,"999999") + "1." + STRING(iRigalNr,"999")
       cImpFilNavn = cImportDir + '\' + "v" + STRING(iSekvens,"999999") + "1." + STRING(iRigalNr,"999")
       cUtfil      = RIGHT-TRIM(TRIM(cExcel2RigalDir),"\") + "\" + cFilnamn
       .
 
ASSIGN cRigalEntries = FILL(",",109)
       ENTRY(1,cRigalEntries) = "VAR"
       ENTRY(2,cRigalEntries) = "E".
       ENTRY(5,cRigalEntries) = "N".

/*
OUTPUT TO VALUE(cUtfil).
PUT UNFORMATTED cRigalversion SKIP.
*/

/* Eksport til Ankommet katalog */
FOR EACH TT_Prisfil: /*  Om NY så använd tandem som origvare  */
    /* Skal filen leses inn lokalt, legges PRS butikknr inn her. */
    IF bDistribImp THEN 
    DO:
      FIND FIRST ImpKonv NO-LOCK WHERE 
        ImpKonv.EDB-System = cEDB-System AND 
        ImpKonv.Tabell     = cImpTabell AND 
        ImpKonv.EksterntId = STRING(tt_PrisFil.Butikk_ButNr) NO-ERROR.
      IF AVAILABLE ImpKonv THEN DO:
        tt_PrisFil.Butik = INT(ImpKonv.InterntId).
        RUN opprettEkstVPILev (tt_PrisFil.Butik).
      END.        
    END.
    /*
    FIND FIRST Produsent NO-LOCK WHERE
        Produsent.Beskrivelse BEGINS tt_PrisFil.Vare_Produsent NO-ERROR.
    ASSIGN cRigalstr = cRigalEntries
           ENTRY(  3,cRigalstr)   = LEFT-TRIM(STRING(TT_Prisfil.strekkode_kode),"0") /* tänk på tandem "40000" + TT_Input.Artnr  */
           ENTRY(  4,cRigalstr)   = STRING(TT_Prisfil.artbas_artnr)
           ENTRY(  6,cRigalstr)   = STRING(TT_Prisfil.artbas_levnr) /* STRING(piLevNr) */
           ENTRY(  7,cRigalstr) = REPLACE(STRING(TT_Prisfil.pris_bestnr),","," ") /* TT_Input.Artnr */
           ENTRY( 10,cRigalstr)  = '"' + REPLACE(TRIM(SUBSTR(TT_Prisfil.vare_varetekst,1,30)),","," ") + '"'
           ENTRY( 11,cRigalstr)  = '"' + REPLACE(TRIM(SUBSTR(TT_Prisfil.vare_varetekst,1,20)),","," ") + '"'
           ENTRY( 12,cRigalstr)  = '"' + REPLACE(TRIM(SUBSTR(TT_Prisfil.vare_varetekst,1,30)),","," ") + '"'
           ENTRY( 14,cRigalstr)  = "1" /* STRING(TT_Prisfil.vare_enhet) */ /* "1" */
           ENTRY( 15,cRigalstr)  = TRIM(STRING(TT_Prisfil.vare_antpkn,">>>9"))  /* "1" */
           ENTRY( 18,cRigalstr)  = STRING(TT_Prisfil.vare_hgr) /* TT_Input.Varuklasskod */
           ENTRY( 19,cRigalstr)  = "1"
           ENTRY( 20,cRigalstr)  = REPLACE(STRING(TT_Prisfil.pris_engrosn),",",".")  /* TT_Input.Inpris */
           ENTRY( 22,cRigalstr)  = REPLACE(STRING(TT_Prisfil.pris_utprisn),",",".")
           ENTRY( 23,cRigalstr)  = REPLACE(STRING(tt_Prisfil.pris_veilpris),",",".")
           ENTRY( 45,cRigalstr)  = REPLACE(STRING(TT_Prisfil.vare_mva),",",".")
           ENTRY( 48,cRigalstr)  = "N" /* Varetype  "N" = vanlig vare "K" = vektvare (veies i kassen)"O" = åpen   pris "I" = Ikke pris i kassen */
           ENTRY( 54,cRigalstr)  = TRIM(STRING(tt_PrisFil.Butikk_ButNr,">>>>>9"))  
           ENTRY( 58,cRigalstr)  = IF DECI(TT_Prisfil.vare_link) > 0 THEN STRING(TT_Prisfil.vare_link) ELSE ""
           ENTRY( 60,cRigalstr)  = IF AVAILABLE Produsent THEN STRING(Produsent.ProdNr) ELSE '0'
           ENTRY( 61,cRigalstr)  = IF DECI(TT_Prisfil.cTandem) > 0 THEN "E" ELSE ""
           ENTRY( 62,cRigalstr)  = IF DECI(TT_Prisfil.cTandem) > 0 THEN TT_Prisfil.cTandem ELSE ""
           ENTRY( 71,cRigalstr)  = "1" /* 1 = Opphav fra Excel GUI VPI import */
           ENTRY( 79,cRigalstr)  = IF TT_Prisfil.vare_mengde = 1 THEN "1" ELSE REPLACE(STRING(TT_Prisfil.vare_mengde),",",".")
           ENTRY( 92,cRigalstr)  = STRING(TT_Prisfil.vare_enhet)
           ENTRY(107,cRigalstr)  = '"' + REPLACE(TRIM(SUBSTR(TT_Prisfil.Vare_Varemerke,1,30)),","," ") + '"'
           .
    PUT UNFORMATTED cRigalStr SKIP.
    */
END.
/*OUTPUT CLOSE.*/

/* Kjøres ikke !!!!! Variablene er blanke. */
IF cFlyttkommando <> "" THEN
    RUN flyttprofiler.p ("cFlyttDir","cFlyttKommando",cUtfil) NO-ERROR.

/* Skal leses inn lokalt i butikkenes egne VPI lommer */
IF bDistribImp THEN 
LOKALIMPORT:
DO:
  iAntallLinjer = 0.
  /* Eksport til Sendes katalog */
  FOR EACH TT_Prisfil WHERE 
    tt_PrisFil.Butik > 0
    BREAK BY tt_PrisFil.Butik 
          BY TT_Prisfil.vare_varetekst 
          BY TT_Prisfil.pris_bestnr: /*  Om NY så använd tandem som origvare  */
    ASSIGN
      iAntallLinjer = iAntallLinjer + 1.
      
    IF FIRST-OF(tt_PrisFil.Butik) THEN 
    DO:
      ENTRY(2,cImpFilNavn,'.') = STRING(tt_PrisFil.Butik).
      OUTPUT TO VALUE(cImpFilNavn).
      PUT UNFORMATTED cRigalversion SKIP.
      /* Fil må opprettes */
      FIND LAST bVPIFilHode NO-LOCK NO-ERROR.
      IF AVAILABLE bVPIFilHode THEN
        plFilId = bVPIFilHode.FilId + 1.
      ELSE
        plFilId = 1.
      DO TRANSACTION:
        CREATE bVPIFilHode.
        ASSIGN
          bVPIFilHode.FilId        = plFilId
          bVPIFilHode.FilNavn      = ENTRY(NUM-ENTRIES(cImpFilNavn,'\'),cImpFilNavn,'\')
          bVPIFilHode.Katalog      = cImportDir
          bVPIFilHode.VPIFilType   = 2 /* RIGAL */
          bVPIFilHode.VPIFilStatus = 1
          bVPIFilHode.EkstVPILevNr = (1000000 + tt_PrisFil.Butik) 
          .
        FIND CURRENT bVPIFilHode NO-LOCK.
      END. /* TRANSACTION */  
    END.
    FIND FIRST Produsent NO-LOCK WHERE
        Produsent.Beskrivelse BEGINS tt_PrisFil.Vare_Produsent NO-ERROR.
    ASSIGN cRigalstr = cRigalEntries
           ENTRY(  3,cRigalstr)   = LEFT-TRIM(STRING(TT_Prisfil.strekkode_kode),"0") /* tänk på tandem "40000" + TT_Input.Artnr  */
           ENTRY(  4,cRigalstr)   = STRING(TT_Prisfil.artbas_artnr)
           ENTRY(  6,cRigalstr)   = STRING(TT_Prisfil.artbas_levnr) /* STRING(piLevNr) */
           ENTRY(  7,cRigalstr) = REPLACE(STRING(TT_Prisfil.pris_bestnr),","," ") /* TT_Input.Artnr */
           ENTRY( 10,cRigalstr)  = '"' + REPLACE(TRIM(SUBSTR(TT_Prisfil.vare_varetekst,1,30)),","," ") + '"'
           ENTRY( 11,cRigalstr)  = '"' + REPLACE(TRIM(SUBSTR(TT_Prisfil.vare_varetekst,1,20)),","," ") + '"'
           ENTRY( 12,cRigalstr)  = '"' + REPLACE(TRIM(SUBSTR(TT_Prisfil.vare_varetekst,1,30)),","," ") + '"'
           ENTRY( 14,cRigalstr)  = "1" /* STRING(TT_Prisfil.vare_enhet) */ /* "1" */
           ENTRY( 15,cRigalstr)  = TRIM(STRING(TT_Prisfil.vare_antpkn,">>>9"))  /* "1" */
           ENTRY( 18,cRigalstr)  = STRING(TT_Prisfil.vare_hgr) /* TT_Input.Varuklasskod */
           ENTRY( 19,cRigalstr)  = "1"
           ENTRY( 20,cRigalstr)  = REPLACE(STRING(TT_Prisfil.pris_engrosn),",",".")  /* TT_Input.Inpris */
           ENTRY( 22,cRigalstr)  = REPLACE(STRING(TT_Prisfil.pris_utprisn),",",".")
           ENTRY( 45,cRigalstr)  = REPLACE(STRING(TT_Prisfil.vare_mva),",",".")
           ENTRY( 48,cRigalstr)  = "N" /* Varetype  "N" = vanlig vare "K" = vektvare (veies i kassen)"O" = åpen   pris "I" = Ikke pris i kassen */
           ENTRY( 54,cRigalstr)  = TRIM(STRING(tt_PrisFil.Butikk_ButNr,">>>>>9"))  
           ENTRY( 58,cRigalstr)  = IF DECI(TT_Prisfil.vare_link) > 0 THEN STRING(TT_Prisfil.vare_link) ELSE ""
           ENTRY( 60,cRigalstr)  = IF AVAILABLE Produsent THEN STRING(Produsent.ProdNr) ELSE '0'
           ENTRY( 61,cRigalstr)  = IF DECI(TT_Prisfil.cTandem) > 0 THEN "E" ELSE ""
           ENTRY( 62,cRigalstr)  = IF DECI(TT_Prisfil.cTandem) > 0 THEN TT_Prisfil.cTandem ELSE ""
           ENTRY( 79,cRigalstr)  = IF TT_Prisfil.vare_mengde = 1 THEN "1" ELSE REPLACE(STRING(TT_Prisfil.vare_mengde),",",".")
           ENTRY( 92,cRigalstr)  = STRING(TT_Prisfil.vare_enhet)
           ENTRY(107,cRigalstr)  = '"' + REPLACE(TRIM(SUBSTR(TT_Prisfil.Vare_Varemerke,1,30)),","," ") + '"'
               .
    PUT UNFORMATTED cRigalStr SKIP.

    IF LAST-OF(tt_PrisFil.Butik) THEN
    DO TRANSACTION:
      OUTPUT CLOSE.
      FIND bVPIFilhode EXCLUSIVE-LOCK WHERE 
        bVPIFilHode.FilId = plFilId NO-ERROR.
      ASSIGN 
          FILE-INFO:FILE-NAME   = cImpFilNavn
          bVPIFilHode.Kl        = STRING(FILE-INFO:FILE-MOD-TIME,"HH:MM:SS")
          bVPIFilHode.Dato      = FILE-INFO:FILE-MOD-DATE
          bVPIFilHode.Storrelse = FILE-INFO:FILE-SIZE
          bVPIFilHode.AntLinjer = iAntallLinjer
          .
      /* Leser inn RIGAL filen. NB: Neg. filid inidkerer at det ikke skal gjøres utlegg til butikk 0. */
      RUN xri1viinnles.p (plFilId * -1, ?,OUTPUT iAntallLinjer).
    END. /* TRANSACTION */
  END.
END. /* LOKALIMPORT */

/* Eksport til butikkene */
IF bDistribIPS THEN 
DISTRIBUSJON:
DO:
  /* Eksport til Sendes katalog */
  FOR EACH TT_Prisfil WHERE 
    tt_PrisFil.Butikk_ButNr > 0
    BREAK BY tt_PrisFil.Butikk_ButNr 
          BY TT_Prisfil.vare_varetekst 
          BY TT_Prisfil.pris_bestnr: /*  Om NY så använd tandem som origvare  */

    IF FIRST-OF(tt_PrisFil.Butikk_ButNr) THEN 
    DO:
      ENTRY(2,cIPSFilNavn,'.') = STRING(tt_PrisFil.Butikk_ButNr).
      OUTPUT TO VALUE(cIPSFilNavn).
      PUT UNFORMATTED cRigalversion SKIP.
    END.
    FIND FIRST Produsent NO-LOCK WHERE
        Produsent.Beskrivelse BEGINS tt_PrisFil.Vare_Produsent NO-ERROR.
    ASSIGN cRigalstr = cRigalEntries
           ENTRY(  3,cRigalstr)   = LEFT-TRIM(STRING(TT_Prisfil.strekkode_kode),"0") /* tänk på tandem "40000" + TT_Input.Artnr  */
           ENTRY(  4,cRigalstr)   = STRING(TT_Prisfil.artbas_artnr)
           ENTRY(  6,cRigalstr)   = STRING(TT_Prisfil.artbas_levnr) /* STRING(piLevNr) */
           ENTRY(  7,cRigalstr) = REPLACE(STRING(TT_Prisfil.pris_bestnr),","," ") /* TT_Input.Artnr */
           ENTRY( 10,cRigalstr)  = '"' + REPLACE(TRIM(SUBSTR(TT_Prisfil.vare_varetekst,1,30)),","," ") + '"'
           ENTRY( 11,cRigalstr)  = '"' + REPLACE(TRIM(SUBSTR(TT_Prisfil.vare_varetekst,1,20)),","," ") + '"'
           ENTRY( 12,cRigalstr)  = '"' + REPLACE(TRIM(SUBSTR(TT_Prisfil.vare_varetekst,1,30)),","," ") + '"'
           ENTRY( 14,cRigalstr)  = "1" /* STRING(TT_Prisfil.vare_enhet) */ /* "1" */
           ENTRY( 15,cRigalstr)  = TRIM(STRING(TT_Prisfil.vare_antpkn,">>>9"))  /* "1" */
           ENTRY( 18,cRigalstr)  = STRING(TT_Prisfil.vare_hgr) /* TT_Input.Varuklasskod */
           ENTRY( 19,cRigalstr)  = "1"
           ENTRY( 20,cRigalstr)  = REPLACE(STRING(TT_Prisfil.pris_engrosn),",",".")  /* TT_Input.Inpris */
           ENTRY( 22,cRigalstr)  = REPLACE(STRING(TT_Prisfil.pris_utprisn),",",".")
           ENTRY( 45,cRigalstr)  = REPLACE(STRING(TT_Prisfil.vare_mva),",",".")
           ENTRY( 48,cRigalstr)  = "N" /* Varetype  "N" = vanlig vare "K" = vektvare (veies i kassen)"O" = åpen   pris "I" = Ikke pris i kassen */
           ENTRY( 54,cRigalstr)  = TRIM(STRING(tt_PrisFil.Butikk_ButNr,">>>>>9"))  
           ENTRY( 58,cRigalstr)  = IF DECI(TT_Prisfil.vare_link) > 0 THEN STRING(TT_Prisfil.vare_link) ELSE ""
           ENTRY( 60,cRigalstr)  = IF AVAILABLE Produsent THEN STRING(Produsent.ProdNr) ELSE '0'
           ENTRY( 61,cRigalstr)  = IF DECI(TT_Prisfil.cTandem) > 0 THEN "E" ELSE ""
           ENTRY( 62,cRigalstr)  = IF DECI(TT_Prisfil.cTandem) > 0 THEN TT_Prisfil.cTandem ELSE ""
           ENTRY( 79,cRigalstr)  = IF TT_Prisfil.vare_mengde = 1 THEN "1" ELSE REPLACE(STRING(TT_Prisfil.vare_mengde),",",".")
           ENTRY( 92,cRigalstr)  = STRING(TT_Prisfil.vare_enhet)
           ENTRY(107,cRigalstr)  = '"' + REPLACE(TRIM(SUBSTR(TT_Prisfil.Vare_Varemerke,1,30)),","," ") + '"'
               .
    PUT UNFORMATTED cRigalStr SKIP.

    IF LAST-OF(tt_PrisFil.Butikk_ButNr) THEN
      OUTPUT CLOSE.
  END.
END. /* DISTRIBUSJON */

/* **********************  Internal Procedures  *********************** */

PROCEDURE opprettEkstVPILev:
/*------------------------------------------------------------------------------
		Purpose:  																	  
		Notes:  																	  
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER piEkstVPILevNr LIKE EkstVPILev.EkstVPILevNr NO-UNDO.
  
    DEFINE BUFFER bEkstVPIFil FOR EkstVPIFil.
    DEFINE BUFFER bEkstVPILev FOR EkstVPILev.
  
    FIND Butiker NO-LOCK WHERE
      Butiker.Butik = piEkstVPILevNr NO-ERROR.
    IF NOT AVAILABLE Butiker THEN 
        RETURN.
  
    LOKAL_IMPORT:
    DO: 
        ASSIGN
          piekstVPILevNr = 1000000 + piEkstVPILevNr.

        IF NOT CAN-FIND(bEkstVPILev WHERE
                        bEkstVPILEv.EkstVPILevNr = piEkstVPILevNr) THEN
        DO:
            CREATE bEkstVPILev.
            ASSIGN
                bEkstVPILev.EkstVPILevNr = piEkstVPILevNr
                bEkstVPILev.KortNavn     = STRING(Butiker.Butik)
                bEkstVPILev.Navn         = Butiker.ButNamn
                bEkstVPILev.AktivLev     = TRUE
                bEkstVPILev.LevNr        = 0
                bekstVPILev.EDB-System   = cEDB-System
                .
            /* Oppretter et datasett */
            IF NOT CAN-FIND(FIRST VPIDatasett WHERE
                            VPIDatasett.EkstVPILevNr = bEkstVPILev.EkstVPILevNr) THEN
            DO:
                CREATE VPIDatasett.
                ASSIGN
                    VPIDatasett.EkstVPILevNr = bEkstVPILev.EkstVPILEvNr.
            END.
        END.
        /* VPIFil */
        IF NOT CAN-FIND(bEkstVPIFil WHERE
                        bEkstVPIFil.EkstVPILevNr = piEkstVPILevNr AND
                        bEkstVPIFil.VPIFilNr     = 1) THEN
        DO:
            CREATE bEkstVPIFil.
            ASSIGN
                bEkstVPIFil.EkstVPILEvNr          = piEkstVPILevNr 
                bEkstVPIFil.VPIFilNr              = 1
                bEkstVPIFil.VPIFilType            = 2
                bEkstVPIFil.VPIFilBeskrivelse     = "RIGAL VPI til PriCat"
                bEkstVPIFil.VPIFilNavn            = "V"
                bEkstVPIFil.VPIEkst               = STRING(piEkstVPILevNr - 1000000)
                bEkstVPIFil.VPIKatalog            = "c:\home\lindbak\ankommet"
                bEkstVPIFil.VPIInnlesningsrutine  = "xri1viinnles"
                bEkstVPIFil.VPIUtpakkingsrutine   = "xstdutpakk"
                bEkstVPIFil.VPIOperator           = 1
                bEkstVPIFil.VPIFilAktiv           = TRUE 
                .
            RELEASE bEkstVPIFil.
        END.
        IF NOT CAN-FIND(bEkstVPIFil WHERE
                        bEkstVPIFil.EkstVPILevNr = piEkstVPILevNr AND
                        bEkstVPIFil.VPIFilNr     = 2) THEN
        DO:
            CREATE bEkstVPIFil.
            ASSIGN
                bEkstVPIFil.EkstVPILEvNr          = piEkstVPILevNr 
                bEkstVPIFil.VPIFilNr              = 2
                bEkstVPIFil.VPIFilType            = 1
                bEkstVPIFil.VPIFilBeskrivelse     = "VPI PriCat konvertert fra RIGAL"
                bEkstVPIFil.VPIFilNavn            = "GVPI" + STRING(piEkstVPILevNr)
                bEkstVPIFil.VPIEkst               = "csv"
                bEkstVPIFil.VPIKatalog            = "c:\home\lindbak\ankommet"
                bEkstVPIFil.VPIInnlesningsrutine  = "xsport1vpiinnles"
                bEkstVPIFil.VPIUtpakkingsrutine   = "xsport1vpiutpakk"
                bEkstVPIFil.VPIOperator           = 1
                bEkstVPIFil.VPIFilAktiv           = TRUE 
                .
            RELEASE bEkstVPIFil.
        END.
    END. /* LOKAL_IMPORT */

END PROCEDURE.
