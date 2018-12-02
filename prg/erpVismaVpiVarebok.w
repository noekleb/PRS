&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
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
DEFINE OUTPUT PARAMETER ocRetur AS CHARACTER  NO-UNDO.
DEF VAR cTmpFilNavn  AS CHAR NO-UNDO.
DEF VAR cFilNavn     AS CHAR NO-UNDO.
DEF VAR iCl          AS INT  NO-UNDO.
DEF VAR cGetFilnavn  AS CHARACTER  NO-UNDO.
DEF VAR iAntEksport  AS INTEGER    NO-UNDO.
DEF VAR iCode        AS INT  INITIAL 1 NO-UNDO. 
DEF VAR cNotat1      AS CHAR NO-UNDO.
DEF VAR cNotat2      AS CHAR NO-UNDO.
DEF VAR cNotat3      AS CHAR NO-UNDO.

DEF STREAM Ut.  /* VPIL Data som skal inn i Karlssons klient */
DEF STREAM Ut2. /* VPIG Data som skal inn i Sport1's klient  */

DEFINE TEMP-TABLE TT_ELogg NO-UNDO LIKE ELogg.
DEFINE BUFFER   bTT_Elogg FOR TT_Elogg.

DEFINE TEMP-TABLE TT_FeilVareKost NO-UNDO
    FIELD ArtikkelNr LIKE ArtBas.ArtikkelNr
    FIELD Beskr      LIKE ArtBas.Beskr
    FIELD VareKost   AS DECIMAL DECIMALS 2 FORMAT "->>,>>9.99".

DEFINE TEMP-TABLE TT_FeilPris NO-UNDO
    FIELD ArtikkelNr LIKE ArtBas.ArtikkelNr
    FIELD Beskr      LIKE ArtBas.Beskr
    FIELD Pris   AS DECIMAL DECIMALS 2 FORMAT "->>,>>9.99".

DEFINE TEMP-TABLE TT_FeilStrekKode NO-UNDO
    FIELD ArtikkelNr LIKE ArtBas.ArtikkelNr
    FIELD Beskr      LIKE ArtBas.Beskr
    FIELD Kode       LIKE StrekKode.Kode.

DEFINE TEMP-TABLE TT_ArtTilEksport NO-UNDO
    FIELD VareBokNr  LIKE VareBokHode.VareBokNr
    FIELD ArtikkelNr LIKE ArtBas.ArtikkelNr
    FIELD Slette     AS LOGICAL
    FIELD Strekkoder AS CHAR
    FIELD StrKode    AS INT
    INDEX Artnr IS PRIMARY Artikkelnr Slette.

DEFINE TEMP-TABLE TT_Vare NO-UNDO
/* 1  */    FIELD butnr       AS INTEGER FORMAT ">>9"                      /* I (3)      Butikknr                                                                                 */
/* 2  */    FIELD ean         AS DECIMAL DECIMALS 0 FORMAT ">>>>>>>>>>>>9" /* DE (13,0)  EAN/PLU-nr                                                                               */
/* 3  */    FIELD hgr         AS INTEGER FORMAT ">>>9"                     /* I (4)      Varegruppenr                                                                             */
/* 4  */    FIELD bong        AS CHARACTER FORMAT "x(20)"                  /* C (20)     Bongtekst                                                                                */
/* 5  */    FIELD opris       AS LOGICAL                                   /* L (yes/no) Flagg for åpen pris                                                                      */
/* 6  */    FIELD link        AS DECIMAL DECIMALS 0 FORMAT ">>>>>>>>>>>>9" /* De (13,0)  EAN/PLU-nr for lenkevare (for eksempel pant)                                             */
/* 7  */    FIELD kotype      AS INTEGER FORMAT "9"                        /* I (1)      Køtype (endringstype) - 1=ny/endring, 9=sletting                                         */
/* 8  */    FIELD vekt        AS INTEGER FORMAT "9"                        /* I (1)      Vekt/salgsenhet (0=stk, 1=kg, 3=meter, 4=m², 5=liter)                                    */
/* 9  */    FIELD utprisn     AS DECIMAL DECIMALS 2 FORMAT ">>>>9.99"      /* De (5,2)   Ordinær utsalgspris                                                                      */
/* 10 */    FIELD bonus       AS LOGICAL                                   /* L (yes/no) Flagg som angir om varen gir kjøpeutbytte                                                */
/* 11 */    FIELD mva         AS DECIMAL DECIMALS 2 FORMAT ">9.99"         /* De (2,2)   Momssats                                                                                 */
/* 12 */    FIELD krabatt     AS LOGICAL                                   /* L (yes/no) Flagg for om varen skal gi automatisk kunderabatt (via varegr)                           */
/* 13 */    FIELD varetekst   AS CHARACTER FORMAT "x(20)"                  /* C (30)     Lang varetekst - i InfoPOS etikettekst 1, kundevennlig                                   */
/* 14 */    FIELD nettokr     AS DECIMAL DECIMALS 2 FORMAT ">>>>9.99"      /* DE (5,2)   Ordinær nettopris                                                                        */
/* 15 */    FIELD bestnr      AS INTEGER FORMAT ">>>>>>>9"                 /* I (8)      Bestillingsnr                                                                            */
/* 16 */    FIELD mixnr       AS INTEGER FORMAT ">>>9"                     /* I (4)      Kobling til mixmatch fil på kassene                                                      */
/* 17 */    FIELD pakkenr     AS INTEGER FORMAT ">>>9"                     /* I (4)      For pakkevarer angir dette kobling til innholdet (i mixmatch)                            */
/* 18 */    FIELD bestvare    AS LOGICAL                                   /* L (yes/no) Benyttes til pantevarer (yes=pantevare/no=vanlig)                                                                              */
/* 19 */    FIELD utprist     AS DECIMAL DECIMALS 2 FORMAT ">>>>9.99"      /* De (5,2)   Kampanjepris                                                                             */
/* 20 */    FIELD tidsstyrtt  AS LOGICAL                                   /* L (yes/no) Flagg for om kampanjepris skal være aktiv når tidsstyrt pris ellers er den aktive prisen */
/* 21 */    FIELD fradatot    AS DATE                                      /* Da         Fradato for kampanjepris                                                                 */
/* 22 */    FIELD fratidt     AS CHARACTER FORMAT "x(5)"                   /* C (hh:mm)  Fratidspunkt for kampanjepris                                                            */
/* 23 */    FIELD tildatot    AS DATE                                      /* Da         Tildato for kampanjepris                                                                 */
/* 24 */    FIELD tiltidt     AS CHARACTER FORMAT "x(5)"                   /* C (hh:mm)  Tiltidspunkt for kampanjepris                                                            */
/* 25 */    FIELD utprism     AS DECIMAL DECIMALS 2 FORMAT ">>>>9.99"      /* De (5,2)   Medlemspris                                                                              */
/* 26 */    FIELD tidsstyrtm  AS LOGICAL                                   /* L (yes/no) Flagg for om medlemspris skal være aktiv når tidsstyrt pris ellers er den aktive prisen  */
/* 27 */    FIELD fradatom    AS DATE                                      /* Da         Fradato for medlemspris                                                                  */
/* 28 */    FIELD fratidm     AS CHARACTER FORMAT "x(5)"                   /* C (hh:mm)  Fratidspunkt for medlemspris                                                             */
/* 29 */    FIELD tildatom    AS DATE                                      /* Da         Tildato for medlemspris                                                                  */
/* 30 */    FIELD tiltidm     AS CHARACTER FORMAT "x(5)"                   /* C (hh:mm)  Tiltidspunkt for medlemspris                                                             */
/* 31 */    FIELD utprisa     AS DECIMAL DECIMALS 2 FORMAT ">>>>9.99"      /* De (5,2)   Tidsstyrt pris                                                                           */
/* 32 */    FIELD fra         AS CHARACTER FORMAT "x(5)"                   /* C (hh:mm)  Fratidspunkt for tidsstyrt pris                                                          */
/* 33 */    FIELD til         AS CHARACTER FORMAT "x(5)"                   /* C (hh:mm)  Tiltidspunkt for tidsstyrt pris                                                          */
/* 34 */    FIELD idkrav      AS LOGICAL                                   /* L (yes/no) Flagg for om salg av vare er aldersbegrenset (legitimasjonsplikt)                        */
/* 35 */    FIELD lager       AS LOGICAL                                   /* L (yes/no) Flagg for om vare er on-line lagerstyrt (kun mot InfoPOS bakromsløsning)                 */
/* 36 */    FIELD individ     AS LOGICAL                                   /* L (yes/no) Flagg for om varen er en individvare                                                     */
/* 37 */    FIELD garantikl   AS INTEGER FORMAT ">>>9"                     /* I (2)      Garantiklasse                                                                            */
/* 38 */    FIELD bilde       AS CHARACTER FORMAT "x(20)"                  /* C (20)     Navn på evt. bildefil                                                                    */
/* 39 */    FIELD timestat    AS LOGICAL                                   /* L (yes/no) Flagg for om statistikk for varen skal lagres på timenivå                                */
/* 40 */    FIELD nettokrt    AS DECIMAL DECIMALS 2 FORMAT ">>>>9.99"      /* De (5,2)  Nettopris for kampanje                                                                    */
/* 41 */    FIELD Stoppkode      AS INTEGER FORMAT "9"                     /* integer 1 siffer 0=ikke stoppet, 2=stoppet, 3=opphev stopp (InfoPOS bakrom legger alltid ut 0)                       */
/* 42 */    FIELD Storrelsesnr   AS INTEGER FORMAT ">>>>9"                 /* integer 5 siffer SE/Gresvig har krav til inntil 5 siffer (0 i Coop)                                                  */
/* 43 */    FIELD Storrelsestype AS INTEGER FORMAT ">>9"                   /* integer 3 siffer Foreløpig kun i SE (0 i Coop). Hentes fra modell i InfoPOS  */
/* 44 */    FIELD Lopenr         AS INTEGER FORMAT ">>>>>9"                /* integer 6 siffer Internt i SE (0 i Coop) */
/* 45 */    FIELD Handlingskode  AS INTEGER FORMAT ">>>>>9"                /* DECIMAL 6 siffer   Internt i SE (0 i Coop) */
/* 46 */    FIELD Modell         AS DECIMAL DECIMALS 0 FORMAT ">>>>>>9"                /* decimal 7 siffer Gresvig/SE (0 i Coop) */
/* 47 */    FIELD Bestnr2        AS CHARACTER FORMAT "x(20)"               /* character 20 tegn Alfanumerisk bestnr. Foreløpig kun SE. Hentes fra pris.bestnr2. ("" i Coop) */
/* 48 */    FIELD Vekttilbud     AS INTEGER FORMAT "9"                     /* INTEGER 1 siffer Likt med felt 8 (vektkode ordinær pris), men dedikert til kampanje. */
/* 49 */    FIELD Varenr         AS DECIMAL DECIMALS 0 FORMAT ">>>>>>>>>>>>9" /* DECIMAL 13 siffer       12 siffer i Gresvig/SE. For søk/salg av varer som er merket på gammelmåten */
/* 50 */    FIELD Fargenr        AS INTEGER FORMAT ">>>>9"                   /* INTEGER 5 siffer SE/Gresvig har krav til inntil 5 siffer (0 i Coop) */
/* 51 */    FIELD Rabikas        AS LOGICAL 
            INDEX ButStrkKotype butnr ean kotype DESCENDING.

DEFINE TEMP-TABLE TT_PakkeTilEksport NO-UNDO
    FIELD Pakkenr LIKE Pakkelinje.PakkeNr
    FIELD Slette     AS LOGICAL
    FIELD Strekkoder AS CHAR
    FIELD Antall     AS CHAR
    INDEX PakkeNr IS PRIMARY PakkeNr Slette.

DEFINE TEMP-TABLE TT_MixMatchTilEksport NO-UNDO
    FIELD MixNr LIKE MixMatchHode.MixNr
    FIELD Slette     AS LOGICAL
    FIELD Strekkoder AS CHAR
    FIELD Antall     AS INTE
    FIELD radant     AS DECIMAL
    FIELD MixMatchType LIKE MixMatchHode.MixMatchType
    FIELD Utpris       LIKE MixMatchHode.Utpris
    INDEX MixNr IS PRIMARY MixNr Slette.

DEFINE TEMP-TABLE TT_Mix NO-UNDO
    FIELD butnr  AS INTEGER FORMAT ">>9"                      /* I (3)      Butikknr                                                                                 */
    FIELD aksjon AS INTEGER FORMAT "9"                        /* I (1)          Posttype, 1=ny/endring mixmatch m/totalt antall,2= ny/endring mixmatch m/antall pr vare,3= ny/endring pakkevareinnhold, 9=sletting */
    FIELD mixnr  AS INTEGER FORMAT ">>>9"                     /* I (4)          Mixmatchnr eller pakkevarenr */
    FIELD antall AS DECIMAL DECIMALS 3 FORMAT ">>>9.999"      /* De (4,3)       Totalt antall for alle varer som inngår i mixmatch */
    FIELD utpris AS DECIMAL DECIMALS 2 FORMAT ">>>>9.99"      /* De (5,2)       Salgspris for varene som inngår i mixmatch ved oppnåelse av denne */
    FIELD ean    AS DECIMAL DECIMALS 0 FORMAT ">>>>>>>>>>>>9" /* De (13,0)      EAN/PLU-nr for varer i mixmatch eller pakkevareinnhold */
    FIELD radant AS DECIMAL DECIMALS 3 FORMAT ">>>9.99"       /* De (4,3)       Antall pr vare i mixmatch eller pakkevare */
    .

DEFINE TEMP-TABLE TT_Hgr NO-UNDO
    FIELD butnr    AS INTEGER FORMAT ">>9"              /* I (3)      Butikknr                                                                                 */
    FIELD hgr      AS INTEGER FORMAT ">>>9"             /* I (4)      Varegruppenr */
    FIELD hgrtekst AS CHARACTER FORMAT "x(30)"          /* C (30)     Varegruppetekst */
    FIELD bonus    AS LOGICAL                           /* L (yes/no) Ikke i bruk (ikke salg direkte på varegr.tast) */
    FIELD mva_like AS DECIMAL DECIMALS 2 FORMAT ">9.99" /* De (2,2)   Ikke i bruk (ikke salg direkte på varegr.tast) */
    FIELD kunrab1  AS DECIMAL DECIMALS 2 FORMAT ">9.99" /* De (2,2)   Automatisk %-rabatt for kunder i kundegr 1 */
    FIELD kunrab2  AS DECIMAL DECIMALS 2 FORMAT ">9.99" /* De (2,2)   Automatisk %-rabatt for kunder i kundegr 2 */
    FIELD kunrab3  AS DECIMAL DECIMALS 2 FORMAT ">9.99" /* De (2,2)   Automatisk %-rabatt for kunder i kundegr 3 */
    FIELD kunrab4  AS DECIMAL DECIMALS 2 FORMAT ">9.99" /* De (2,2)   Automatisk %-rabatt for kunder i kundegr 4 */
    FIELD kunrab5  AS DECIMAL DECIMALS 2 FORMAT ">9.99" /* De (2,2)   Automatisk %-rabatt for kunder i kundegr 5 */
    FIELD kunrab6  AS DECIMAL DECIMALS 2 FORMAT ">9.99" /* De (2,2)   Automatisk %-rabatt for kunder i kundegr 6 */
    FIELD kunrab7  AS DECIMAL DECIMALS 2 FORMAT ">9.99" /* De (2,2)   Automatisk %-rabatt for kunder i kundegr 7 */
    FIELD kunrab8  AS DECIMAL DECIMALS 2 FORMAT ">9.99" /* De (2,2)   Automatisk %-rabatt for andre butikker (intern salg) */
    FIELD kunrab9  AS DECIMAL DECIMALS 2 FORMAT ">9.99" /* De (2,2)   Automatisk %-rabatt for ansatte (personalrabatt) */
    .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB Procedure 
/* ************************* Included-Libraries *********************** */

{incl\devmode.i}
{incl\custdevmode.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

{syspara.i 5 1 1 iCL INT}

FIND FIRST EkstEDBSystem WHERE EkstEDBSystem.DataType = "VPIV" AND EkstEDBSystem.Aktiv = TRUE NO-LOCK NO-ERROR.
IF NOT AVAIL EkstEDBSystem THEN DO:
    ocRetur = "ERROR - Ingen VPI-rutine aktiv".
    RETURN.
END.
/* Får tilbake filkatalog og filnavn. */
IF DYNAMIC-FUNCTION("runProc","get_ekstedbsys_filnavn.p",EkstEDBSystem.EDBSystem,?) THEN 
    ASSIGN cGetFilnavn = DYNAMIC-FUNCTION("getTransactionMessage").
IF NOT NUM-ENTRIES(cGetFilnavn,"|") = 3 THEN DO:
    ocRetur = "ERROR-" + cGetFilnavn.
    RETURN.
END.
ELSE DO:
    ASSIGN cTmpFilNavn  = RIGHT-TRIM(ENTRY(1,cGetFilnavn,"|"),"\") + "\" + "TMP" + ENTRY(2,cGetFilnavn,"|")
           cFilNavn     = ENTRY(2,cGetFilnavn,"|")
           /* Lagerførte varer */
           cTmpFilNavn  = replace(cTmpFilNavn,"VPI","VPIL")
           cFilNavn     = replace(cFilNavn,"VPI","VPIL")
           .
END.

FIND Butiker NO-LOCK WHERE
    Butiker.Butik = iCl NO-ERROR.
RUN KopierElogg.

RUN lesVareboklinjer. 
RUN EksporterVpi.
ocRetur = "OK," + String(iAntEksport).

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-EksporterVpi) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EksporterVpi Procedure 
PROCEDURE EksporterVpi :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF VAR bStreamAapen AS LOG NO-UNDO.
DEF VAR ii AS INTE NO-UNDO.

DEF BUFFER bSasong FOR Sasong.
    /* Vare på strekkodenivå til ERP systemet. */
    ARTIKKEL:
    FOR EACH TT_ArtTilEksport
        BY TT_ArtTilEksport.VareBokNr
        BY TT_ArtTilEksport.ArtikkelNr:

        FIND VareBokLinje NO-LOCK WHERE
            VareBokLinje.VareBokNr = TT_ArtTilEksport.VareBokNr AND
            VareBokLinje.ArtikkelNr = TT_ArtTilEksport.ArtikkelNr NO-ERROR.
        IF NOT AVAILABLE VareBokLinje THEN
            NEXT ARTIKKEL.
        FIND VareBokHode OF VareBokLinje NO-LOCK.
        FIND Messe OF VareBokHode NO-LOCK NO-ERROR.
        
        IF TT_ArtTilEksport.Slette = FALSE THEN DO:
            FIND ArtBas WHERE ArtBas.ArtikkelNr = TT_ArtTilEksport.ArtikkelNr NO-LOCK NO-ERROR.
        END.
        IF TT_ArtTilEksport.Slette = TRUE OR NOT AVAIL ArtBas THEN
            NEXT.

        IF AVAILABLE VarGr THEN
            RELEASE VarGr.
        IF AVAILABLE Avdeling THEN
            RELEASE Avdeling.
        IF AVAILABLE HuvGr THEN
            RELEASE HuvGr.

        FIND VarGr WHERE VarGr.Vg = VareBokLinje.Vg NO-LOCK NO-ERROR.
        IF AVAILABLE VarGr THEN
            FIND HuvGr OF VarGr NO-LOCK NO-ERROR.
        IF AVAILABLE HuvGr THEN
            FIND Avdeling OF HuvGr NO-LOCK NO-ERROR.
        FIND ArtPris OF ArtBas NO-LOCK WHERE
            ArtPris.ProfilNr = Butiker.ProfilNr NO-ERROR.
        IF NOT AVAILABLE ArtPris THEN
            NEXT ARTIKKEL.
        FIND LevBas NO-LOCK WHERE
            LevBas.LevNr = VareBokLinje.LevNr NO-ERROR.
        FIND Produsent NO-LOCK WHERE
            Produsent.ProdNr = VarebokLinje.ProdNr NO-ERROR.
        FIND Varemerke NO-LOCK WHERE
            Varemerke.VmId = ArtBas.VmId NO-ERROR.
        FIND Sasong NO-LOCK WHERE
            Sasong.Sasong = VarebokLinje.Sasong NO-ERROR.
        FIND bSasong NO-LOCK WHERE
            bSasong.Sasong = VarebokLinje.Sasong NO-ERROR.
        FIND Farg NO-LOCK WHERE
            Farg.Farg = ArtBas.Farg NO-ERROR.
        FIND Material NO-LOCK WHERE
            Material.MatKod = ArtBas.MatKod NO-ERROR.
        FIND Valuta NO-LOCK WHERE
            Valuta.ValKod = ArtBas.Valkod NO-ERROR.
    
        DO ii = 1 TO NUM-ENTRIES(TT_ArtTilEksport.StrekKoder):
            FIND Strekkode WHERE StrekKode.Kode = ENTRY(ii,TT_ArtTilEksport.StrekKoder) NO-LOCK NO-ERROR.
            IF NOT AVAIL StrekKode THEN
                NEXT.
            IF Strekkode.StrKode = 0 THEN
                NEXT.
            FIND StrKonv OF Strekkode NO-LOCK NO-ERROR.
            IF AVAILABLE StrKonv THEN
                FIND FIRST StrTStr NO-LOCK WHERE
                StrTStr.StrTypeId = ArtBAs.StrTypeId AND
                StrTStr.SoStorl   = StrKonv.Storl NO-ERROR.

            IF bStreamAapen = FALSE THEN
            DO:
                OUTPUT STREAM Ut TO VALUE(cTmpFilNavn) NO-ECHO.
                bStreamAapen = TRUE.
                RUN eksportHeader.
            END.
            ASSIGN
            iAntEksport = iAntEksport + 1
            cNotat1     = ArtBas.Notat
            cNotat2     = ArtBas.VareFakta
            cNotat3     = VarebokLinje.Linjemerknad
            /* Stripper notatfelt for chr(10) og chr(13). */
            cNotat1 = REPLACE (cNotat1,chr(13),"|")
            cNotat1 = REPLACE (cNotat1,chr(10),"|")
            cNotat1 = REPLACE (cNotat1,chr(1),"|")
            cNotat2 = REPLACE (cNotat2,chr(13),"|")
            cNotat2 = REPLACE (cNotat2,chr(10),"|")
            cNotat2 = REPLACE (cNotat2,chr(1),"|")
            cNotat3 = REPLACE (cNotat3,chr(13),"|")
            cNotat3 = REPLACE (cNotat3,chr(10),"|")
            cNotat3 = REPLACE (cNotat3,chr(1),"|")
            .

            /* Alle artikler skal ut. */
            EXPORT STREAM Ut DELIMITER ";"
                    {erpVismaVpiVarebok.i}    
        END.    
    END. /* ARTIKKEL */

    IF bStreamAapen THEN DO:
        OUTPUT STREAM Ut2 CLOSE.
        OUTPUT STREAM Ut CLOSE.
    END.

    OS-COMMAND SILENT VALUE("RENAME " + cTmpFilNavn  + " " + cFilNavn).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-eksportHeader) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE eksportHeader Procedure 
PROCEDURE eksportHeader :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    EXPORT STREAM Ut DELIMITER ";"
      {erpVismaVpi2.i}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-KopierElogg) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE KopierElogg Procedure 
PROCEDURE KopierElogg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE BUFFER bElogg FOR Elogg.
    DO:
        FOR EACH ELogg WHERE ELogg.TabellNavn = "VarebokVPI" AND
                             ELogg.EksterntSystem = "ERP" NO-LOCK:
            BUFFER-COPY ELogg TO TT_ELogg NO-ERROR.
            FIND bElogg WHERE ROWID(bElogg) = ROWID(Elogg) EXCLUSIVE NO-WAIT NO-ERROR.
            IF AVAIL bElogg THEN
                DELETE bELogg.
            IF AVAILABLE TT_Elogg THEN
                RELEASE TT_ELogg.
        END.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-lesVareboklinjer) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE lesVareboklinjer Procedure 
PROCEDURE lesVareboklinjer :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE lPakkeBort AS LOGICAL    NO-UNDO.
DEFINE VARIABLE dTest      AS DECIMAL    NO-UNDO.
DEFINE VARIABLE plVareBokNr LIKE VareBokHode.VareBokNr NO-UNDO.

def var rRecid as recid no-undo.

DEFINE BUFFER bTT_ELogg    FOR TT_ELogg.
DEFINE BUFFER bufStrekkode FOR Strekkode.

FOR EACH TT_ELogg WHERE 
            TT_ELogg.TabellNavn     = "VarebokVPI" AND
            TT_ELogg.EksterntSystem = "ERP"    AND
/*             etype 1 och 3 */
/*             TT_ELogg.EndringsType   = 1 AND */
            NUM-ENTRIES(TT_ELogg.Verdier,CHR(1)) = 2 BY TT_ELogg.Verdier:

    ASSIGN
        plVareBokNr = DECI(entry(1,TT_ELogg.Verdier,CHR(1)))
        .

    FIND ArtBas WHERE ArtBas.ArtikkelNr = DECI(entry(2,TT_ELogg.Verdier,CHR(1))) NO-LOCK NO-ERROR.
    IF AVAIL ArtBas THEN 
    DO:
        FOR EACH StrekKode OF ArtBas NO-LOCK WHERE 
            StrekKode.IKasse = TRUE
            BREAK
            BY Strekkode.artikkelNr
            BY Strekkode.StrKode
            BY Strekkode.Kode DESCENDING:

            IF AVAILABLE bufStrekkode THEN
                RELEASE bufStrekkode.

            /* Den EAN koden pr. størrelse som har størst numerisk verdi er den som legges ut. */
            IF FIRST-OF (Strekkode.StrKode) THEN
            STREKKODE:
            DO:                 
                /* Den sist endrede strekkoden som ikke er 02 kode, skal legges ut. Endret dato settes */
                /* når vpi leses inn. Derfor skal denne dato benyttes for å styre hvilken EAN kode som */
                /* Sendes over til ERP.                                                                */
                rRecid = 0.
                BUFSTREKKODE:
                for each bufStrekkode no-lock where
                  bufStrekkode.ArtikkelNr = Strekkode.ArtikkelNr and 
                  bufStrekkode.StrKode    = Strekkode.StrKode
                  break by bufStrekkode.ArtikkelNr
                        by bufStrekkode.StrKode
                        by bufStrekkode.EDato descending:
                        
                    /* 02 koder skal ignoreres. */    
                    IF LENGTH(bufStrekkode.Kode) = 13 AND
                        bufStrekkode.Kode BEGINS "02" THEN. /* Gjør ingenting :) */
                    else do: 
                      rRecid = recid(bufStrekkode).
                      leave BUFSTREKKODE. 
                    end.
                end. /* BUFSTREKKODE */
                if rRecid <> 0 then
                  find bufStrekkode  no-lock where
                    recid(bufStrekkode) = rRecid no-error.

                /* er ikke denne strekkoden merket som hovedkode for størrelsen, skal det sjekkes om */
                /* det er en av de andre kodene som er satt opp som hovednummer for størrelsen.      */
                /* Den strekkoden som står som hovednummer for størrelsen er den som skal legges ut. */
                IF not available bufStrekkode and Strekkode.Hovednr = FALSE THEN
                HOVEDNR:
                DO:
                    FIND FIRST bufStrekkode NO-LOCK WHERE
                        bufStrekkode.ArtikkelNr = Strekkode.ArtikkelNr AND
                        bufStrekkode.StrKode    = Strekkode.StrKode AND
                        bufStrekkode.HovedNr    = TRUE NO-ERROR.
                    /* 02 koder skal ikke legges ut hvis det finnes andre koder. */
                    IF AVAILABLE bufStrekkode THEN
                    DO:
                        IF LENGTH(bufStrekkode.Kode) = 13 AND
                            bufStrekkode.Kode BEGINS "02" THEN
                            RELEASE bufStrekkode.
                    END.
                END. /* HOVEDNR */

                /* Hovednummer */
                IF AVAILABLE bufStrekkode THEN
                HOVEDNR-UTLEGG:
                DO:
                    ASSIGN dTest = ABS(DECI(bufStrekKode.Kode)) NO-ERROR.
                    /* det har förekommit koder med - på slutet, därför denna test på slutet */
                    IF ERROR-STATUS:ERROR OR LENGTH(bufStrekKode.Kode) > 13 OR (bufStrekKode.Kode <> STRING(dTest,FILL("9",LENGTH(bufStrekKode.Kode)))) THEN DO:
                        CREATE TT_FeilStrekKode.
                        ASSIGN TT_FeilStrekKode.ArtikkelNr = ArtBas.ArtikkelNr
                               TT_FeilStrekKode.Beskr      = ArtBas.Beskr
                               TT_FeilStrekKode.Kode       = bufStrekKode.Kode.
                        NEXT.
                    END.
                    FIND TT_ArtTilEksport WHERE TT_ArtTilEksport.ArtikkelNr = ArtBas.ArtikkelNr AND
                                                TT_ArtTilEksport.Slette = (NOT ArtBas.iKasse) NO-ERROR.
                    IF AVAIL TT_ArtTilEksport AND NOT CAN-DO(TT_ArtTilEksport.Strekkoder,bufStrekKode.Kode) THEN
                        ASSIGN TT_ArtTilEksport.Strekkoder = TT_ArtTilEksport.Strekkoder + "," + bufStrekKode.Kode.
                    ELSE DO:
                        CREATE TT_ArtTilEksport.
                        ASSIGN TT_ArtTilEksport.VareBokNr  = plVareBokNr
                               TT_ArtTilEksport.ArtikkelNr = DECI(entry(2,TT_ELogg.Verdier,CHR(1)))
                               TT_ArtTilEksport.Slette     = FALSE 
                               TT_ArtTilEksport.Strekkoder = bufStrekKode.Kode.
                    END.
                END. /* HOVEDNR-UTLEGG */

                /* Ellers er det den koden som har størst numerisk verdi som skal legges ut. */                
                ELSE
                STORSTE-VERDI:
                DO:
                    ASSIGN dTest = ABS(DECI(StrekKode.Kode)) NO-ERROR.
                    /* det har förekommit koder med - på slutet, därför denna test på slutet */
                    IF ERROR-STATUS:ERROR OR LENGTH(StrekKode.Kode) > 13 OR (StrekKode.Kode <> STRING(dTest,FILL("9",LENGTH(StrekKode.Kode)))) THEN DO:
                        CREATE TT_FeilStrekKode.
                        ASSIGN TT_FeilStrekKode.ArtikkelNr = ArtBas.ArtikkelNr
                               TT_FeilStrekKode.Beskr      = ArtBas.Beskr
                               TT_FeilStrekKode.Kode       = StrekKode.Kode.
                        NEXT.
                    END.
                    FIND TT_ArtTilEksport WHERE TT_ArtTilEksport.ArtikkelNr = ArtBas.ArtikkelNr AND
                                                TT_ArtTilEksport.Slette = (NOT ArtBas.iKasse) NO-ERROR.
                    IF AVAIL TT_ArtTilEksport AND NOT CAN-DO(TT_ArtTilEksport.Strekkoder,StrekKode.Kode) THEN
                        ASSIGN TT_ArtTilEksport.Strekkoder = TT_ArtTilEksport.Strekkoder + "," + StrekKode.Kode.
                    ELSE DO:
                        CREATE TT_ArtTilEksport.
                        ASSIGN TT_ArtTilEksport.VareBokNr  = plVareBokNr
                               TT_ArtTilEksport.ArtikkelNr = DECI(entry(2,TT_ELogg.Verdier,CHR(1)))
                               TT_ArtTilEksport.Slette     = FALSE 
                               TT_ArtTilEksport.Strekkoder = StrekKode.Kode.
                    END.
                END. /* STORSTE-VERDI */

            END. /* STREKKODE */
        END.
    END.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

