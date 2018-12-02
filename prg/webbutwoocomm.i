DEF {&New} TEMP-TABLE tt_webArtikkel
    FIELD iRecType  AS INT FORMAT ">9"
    FIELD ArtikkelNr  AS dec FORMAT "9999999999999" DECIMALS 0
    FIELD ModellFarge  AS dec FORMAT "9999999999999" DECIMALS 0
    FIELD LevKod AS CHAR FORMAT "x(30)"
    FIELD Beskr AS CHAR FORMAT "x(100)"
    FIELD LevFargKod AS CHAR FORMAT "x(30)"             SERIALIZE-HIDDEN
    FIELD StrTypeId AS INT FORMAT ">>>>>9"              SERIALIZE-HIDDEN
    FIELD StrKode AS INT FORMAT ">>>9"                  SERIALIZE-HIDDEN
    FIELD Storl AS CHAR FORMAT "x(10)"                  SERIALIZE-HIDDEN
    FIELD Kode AS CHAR FORMAT "x(30)"                   SERIALIZE-HIDDEN
    FIELD SalgsEnhet AS CHAR FORMAT "x(4)"              SERIALIZE-HIDDEN
    FIELD AntIPakn AS INT FORMAT ">>>9"                 SERIALIZE-HIDDEN
    FIELD Lokasjon AS CHAR FORMAT "X(20)"               SERIALIZE-HIDDEN
    FIELD LevNr AS INT FORMAT ">>>>>9"                  SERIALIZE-HIDDEN
    FIELD Varefakta AS CHAR FORMAT "x(255)"
    FIELD VPIBildeKode AS CHAR FORMAT "x(30)"           SERIALIZE-HIDDEN
    FIELD Farg AS INTEGER FORMAT ">>>>9"                SERIALIZE-HIDDEN
    FIELD VmId AS INT FORMAT ">>>9"                     SERIALIZE-HIDDEN
    FIELD ProdNr AS INT FORMAT ">>>>>9"                 SERIALIZE-HIDDEN
    FIELD Sasong AS INT FORMAT ">>9"                    SERIALIZE-HIDDEN
    FIELD MatKod AS INT FORMAT ">9"                     SERIALIZE-HIDDEN
    FIELD Vg AS INT FORMAT ">>>>>9"                    
    FIELD VgKat AS INT FORMAT ">9"                      SERIALIZE-HIDDEN
    FIELD Anv-Id AS INT FORMAT ">9"                     SERIALIZE-HIDDEN
    FIELD PostVekt AS DECIMAL FORMAT "->>,>>>,>>9.99"   SERIALIZE-HIDDEN
    FIELD PostLengde AS INTEGER FORMAT "->>>,>>9"       SERIALIZE-HIDDEN
    FIELD PostBredde AS INTEGER FORMAT "->>>,>>9"       SERIALIZE-HIDDEN
    FIELD PostHoyde AS INTEGER FORMAT "->>>,>>9"        SERIALIZE-HIDDEN
    FIELD WebMinLager AS DECIMAL FORMAT "->>>,>>9.999"  SERIALIZE-HIDDEN
    FIELD KampanjeKode AS CHARACTER FORMAT "x(20)"      SERIALIZE-HIDDEN
    FIELD WebLeveringstid AS INTEGER FORMAT ">9"        SERIALIZE-HIDDEN
    FIELD VareType AS INTEGER FORMAT "9"                SERIALIZE-HIDDEN
    FIELD Leveringstid AS INTEGER FORMAT ">9"           SERIALIZE-HIDDEN
    FIELD VareTypeTekst AS CHARACTER FORMAT "x(30)"     SERIALIZE-HIDDEN
    FIELD Bonus_Givende AS LOG FORMAT "Yes/No"          SERIALIZE-HIDDEN
    FIELD PubliserINettbutikk AS LOG FORMAT "Yes/No"    SERIALIZE-HIDDEN
    FIELD OneSize AS LOG FORMAT "Yes/No"
    
    /* Prisfelt styrt av tilbudsflagget */
    FIELD InnkjopsPris AS DEC FORMAT "->>,>>>,>>9.99"
    FIELD Rab1_Proc AS DEC FORMAT "->9.99"              SERIALIZE-HIDDEN
    FIELD Varekost AS DEC FORMAT "->>,>>>,>>9.99"       SERIALIZE-HIDDEN
    FIELD MvaKode AS int FORMAT ">9"                    SERIALIZE-HIDDEN
    FIELD Mva_Proc AS DEC FORMAT "->9.99"               SERIALIZE-HIDDEN
    FIELD Db_Proc AS DEC FORMAT "->9.99"                SERIALIZE-HIDDEN
    FIELD Pris AS DEC FORMAT "->>,>>>,>>9.99"
    FIELD Tilbud AS LOG FORMAT "Yes/No"
    FIELD AktiveringsDato AS DATE FORMAT "99/99/9999"   SERIALIZE-HIDDEN
    FIELD AktiveringsTid AS CHAR /* HH:MM:SS */         SERIALIZE-HIDDEN
    FIELD AvsluttDato AS DATE FORMAT "99/99/9999"       SERIALIZE-HIDDEN
    FIELD AvsluttTid AS CHAR /* HH:MM:SS */             SERIALIZE-HIDDEN
    FIELD UtvidetSok AS CHARACTER                       SERIALIZE-HIDDEN
    /* Ordinær pris feltene. */
    FIELD OrdInnkjopsPris AS DEC FORMAT "->>,>>>,>>9.99"
    FIELD OrdRab1_Proc AS DEC FORMAT "->9.99"             SERIALIZE-HIDDEN
    FIELD OrdVarekost AS DEC FORMAT "->>,>>>,>>9.99"      SERIALIZE-HIDDEN
    FIELD OrdMvaKode AS int FORMAT ">9"                   SERIALIZE-HIDDEN
    FIELD OrdMva_Proc AS DEC FORMAT "->9.99"              SERIALIZE-HIDDEN
    FIELD OrdDb_Proc AS DEC FORMAT "->9.99"               SERIALIZE-HIDDEN
    FIELD OrdPris AS DEC FORMAT "->>,>>>,>>9.99"
    /* Førpris eller veilendende pris */
    FIELD AnbefaltPris AS DEC FORMAT "->>,>>>,>>9.99"
    /* Jamføringsehnet */
    FIELD JamforEnhet AS CHARACTER FORMAT "x(4)"          SERIALIZE-HIDDEN
    FIELD Mengde AS DECIMAL FORMAT "->>9.999"             SERIALIZE-HIDDEN
    FIELD Bestillingsnummer AS CHARACTER FORMAT "x(25)"
    FIELD HovedKatNr AS INTEGER FORMAT ">9"               SERIALIZE-HIDDEN
    FIELD UnderKatListe AS CHARACTER FORMAT "X(150)"      SERIALIZE-HIDDEN
    FIELD BildeStort AS CHARACTER FORMAT "x(30)"          SERIALIZE-HIDDEN
    FIELD BildeLite  AS CHARACTER FORMAT "x(30)"          SERIALIZE-HIDDEN
    FIELD Link_Til_Nettside AS CHARACTER FORMAT "x(40)"   SERIALIZE-HIDDEN
    /* Tilleggsfelter som JF vil ha ut. */
    FIELD Klack AS INTEGER FORMAT ">>>>>>>9"              SERIALIZE-HIDDEN
    FIELD Inner_Id AS INTEGER FORMAT ">>>>>>>9"           SERIALIZE-HIDDEN
    FIELD Ov_Id AS INTEGER FORMAT ">>>>>>>9"              SERIALIZE-HIDDEN
    FIELD Slit_Id AS INTEGER FORMAT ">>>>>>>9"            SERIALIZE-HIDDEN
    FIELD Last-Id AS INTEGER FORMAT ">>>>>>>9"            SERIALIZE-HIDDEN
    FIELD BehKode AS INTEGER FORMAT ">>>>>>>9"            SERIALIZE-HIDDEN
    FIELD RAvdNr AS INTEGER FORMAT ">>>>>>>9"             SERIALIZE-HIDDEN
    FIELD DivInfo1 AS CHARACTER FORMAT "x(30)"            SERIALIZE-HIDDEN
    FIELD DivInfo2 AS CHARACTER FORMAT "x(30)"            SERIALIZE-HIDDEN
    FIELD DivInfo3 AS CHARACTER FORMAT "x(30)"            SERIALIZE-HIDDEN
    FIELD DivInfo4 AS CHARACTER FORMAT "x(30)"            SERIALIZE-HIDDEN
    FIELD DivInfo5 AS CHARACTER FORMAT "x(30)"            SERIALIZE-HIDDEN
    FIELD DivInfo6 AS CHARACTER FORMAT "x(30)"            SERIALIZE-HIDDEN
    FIELD DivInfo7 AS CHARACTER FORMAT "x(30)"            SERIALIZE-HIDDEN
    FIELD DivInfo8 AS CHARACTER FORMAT "x(30)"            SERIALIZE-HIDDEN
    FIELD LopNr AS INTEGER FORMAT "->>>>>9"
    .


DEFINE TEMP-TABLE tt_attribute NO-UNDO SERIALIZE-NAME "Attribute"
    FIELD cType AS CHAR SERIALIZE-NAME "type"
    FIELD iId   AS INT  SERIALIZE-NAME "id"
    FIELD cValue AS CHAR SERIALIZE-NAME "value"
    INDEX TI IS PRIMARY UNIQUE cType iId.
DEFINE TEMP-TABLE tt_attributeExport NO-UNDO SERIALIZE-NAME "Attribute" LIKE tt_attribute.

DEFINE TEMP-TABLE tt_category NO-UNDO SERIALIZE-NAME "Category"
    FIELD cType AS CHAR SERIALIZE-NAME "type"
    FIELD iId   AS INT  SERIALIZE-NAME "id"
    FIELD cValue AS CHAR SERIALIZE-NAME "value"
    INDEX TI IS PRIMARY UNIQUE cType iId.
DEFINE TEMP-TABLE tt_categoryExport NO-UNDO SERIALIZE-NAME "Category" LIKE tt_category.

DEFINE TEMP-TABLE tt_farg NO-UNDO SERIALIZE-NAME "Farg"
    FIELD artikkelnr AS DECI  SERIALIZE-HIDDEN
    FIELD Farg       AS INTE  SERIALIZE-NAME "id"
    FIELD FarBeskr   AS CHAR  SERIALIZE-NAME "name"
    INDEX art IS PRIMARY artikkelnr.
DEFINE TEMP-TABLE tt_varemerke NO-UNDO SERIALIZE-NAME "VmId"
    FIELD artikkelnr  AS DECI  SERIALIZE-HIDDEN
    FIELD VMId        AS INTE  SERIALIZE-NAME "id"
    FIELD Beskrivelse AS CHAR  SERIALIZE-NAME "name"
    INDEX art IS PRIMARY artikkelnr.
DEFINE TEMP-TABLE tt_Material NO-UNDO SERIALIZE-NAME "MatKod"
    FIELD artikkelnr  AS DECI  SERIALIZE-HIDDEN
    FIELD MatKod        AS INTE  SERIALIZE-NAME "id"
    FIELD MatBeskr AS CHAR  SERIALIZE-NAME "name"
    INDEX art IS PRIMARY artikkelnr.
DEFINE TEMP-TABLE tt_HovedKategori NO-UNDO SERIALIZE-NAME "HovedKatNr"
    FIELD artikkelnr  AS DECI  SERIALIZE-HIDDEN
    FIELD HovedKatNr        AS INTE  SERIALIZE-NAME "id"
    FIELD HovedKatTekst AS CHAR  SERIALIZE-NAME "name"
    INDEX art IS PRIMARY artikkelnr.

DEFINE TEMP-TABLE tt_ArtBasUnderkategori NO-UNDO SERIALIZE-NAME "UnderKatListe"
    FIELD artikkelnr  AS DECI  SERIALIZE-HIDDEN
    FIELD UnderKatNr        AS INTE  SERIALIZE-NAME "id"
    FIELD UnderKatTekst AS CHAR  SERIALIZE-NAME "name"
    INDEX art IS PRIMARY artikkelnr.

DEFINE TEMP-TABLE tt_Klack NO-UNDO SERIALIZE-NAME "Klack"
    FIELD artikkelnr  AS DECI  SERIALIZE-HIDDEN
    FIELD klack-id        AS INTE  SERIALIZE-NAME "id"
    FIELD beskrivning AS CHAR  SERIALIZE-NAME "name"
    INDEX art IS PRIMARY artikkelnr.

DEFINE TEMP-TABLE tt_InnerSula NO-UNDO SERIALIZE-NAME "Inner-Id"
    FIELD artikkelnr  AS DECI  SERIALIZE-HIDDEN
    FIELD Inner-Id        AS INTE  SERIALIZE-NAME "id"
    FIELD InnerBeskr AS CHAR  SERIALIZE-NAME "name"
    INDEX art IS PRIMARY artikkelnr.

DEFINE TEMP-TABLE tt_Ovandel NO-UNDO SERIALIZE-NAME "Ov-Id"
    FIELD artikkelnr  AS DECI  SERIALIZE-HIDDEN
    FIELD Ov-Id        AS INTE  SERIALIZE-NAME "id"
    FIELD OvBeskr AS CHAR  SERIALIZE-NAME "name"
    INDEX art IS PRIMARY artikkelnr.

DEFINE TEMP-TABLE tt_SlitSula NO-UNDO SERIALIZE-NAME "Slit-Id"
    FIELD artikkelnr  AS DECI  SERIALIZE-HIDDEN
    FIELD Slit-Id        AS INTE  SERIALIZE-NAME "id"
    FIELD SlitBeskr AS CHAR  SERIALIZE-NAME "name"
    INDEX art IS PRIMARY artikkelnr.

DEFINE TEMP-TABLE tt_Last-Sko NO-UNDO SERIALIZE-NAME "Last-Id"
    FIELD artikkelnr  AS DECI  SERIALIZE-HIDDEN
    FIELD Last-Id        AS INTE  SERIALIZE-NAME "id"
    FIELD LastBeskr AS CHAR  SERIALIZE-NAME "name"
    INDEX art IS PRIMARY artikkelnr.


DEFINE TEMP-TABLE tt_Storl NO-UNDO SERIALIZE-NAME "Storl"
    FIELD artikkelnr  AS DECI  SERIALIZE-HIDDEN
    FIELD sku        AS CHAR  SERIALIZE-NAME "sku"
    FIELD strkode    AS INTE SERIALIZE-NAME "StrKode"
    FIELD storl AS CHAR  SERIALIZE-NAME "storl"
    INDEX art IS PRIMARY artikkelnr.



DEF {&New} TEMP-TABLE tt_webAvdeling
    FIELD iRecType  AS INT FORMAT ">9"
    FIELD AvdelingNr AS INT FORMAT ">>>9"
    FIELD AvdelingNavn AS CHAR FORMAT "x(30)"
    .

DEF {&New} TEMP-TABLE tt_webAktivitet
    FIELD iRecType  AS INT FORMAT ">9"
    FIELD AktNr AS INT FORMAT ">>9"
    FIELD Beskrivelse AS CHAR FORMAT "x(30)"
    .

DEF {&New} TEMP-TABLE tt_webHuvGr
    FIELD iRecType  AS INT FORMAT ">9"
    FIELD Hg AS INT FORMAT ">>>9"
    FIELD HgBeskr AS CHAR FORMAT "x(30)"
    FIELD AvdelingNr AS INT FORMAT ">>9"
    .

DEF {&New} TEMP-TABLE tt_webKategori
    FIELD iRecType  AS INT FORMAT ">9"
    FIELD KatNr AS INT FORMAT ">9"
    FIELD Beskrivelse AS CHAR FORMAT "x(30)"
    .

DEF {&New} TEMP-TABLE tt_webLevBas
    FIELD iRecType  AS INT FORMAT ">9"
    FIELD LevNr AS INT FORMAT ">>>>>9"
    FIELD LevNamn AS CHAR FORMAT "x(30)"
    .

DEF {&New} TEMP-TABLE tt_webMaterial
    FIELD iRecType  AS INT FORMAT ">9"
    FIELD MatKod AS INT FORMAT ">>>>>9"
    FIELD MatBeskr AS CHAR FORMAT "x(30)"
    .

DEF {&New} TEMP-TABLE tt_webSasong
    FIELD iRecType  AS INT FORMAT ">9"
    FIELD Sasong AS INT FORMAT ">>9"
    FIELD SasBeskr AS CHAR FORMAT "x(30)"
    .

DEF {&New} TEMP-TABLE tt_webProdusent
    FIELD iRecType  AS INT FORMAT ">9"
    FIELD ProdNr AS INT FORMAT ">>>>>9"
    FIELD Beskrivelse AS CHAR FORMAT "x(30)"
    .

DEF {&New} TEMP-TABLE tt_webVaremerke
    FIELD iRecType  AS INT FORMAT ">9"
    FIELD VmId AS INT FORMAT ">>>9"
    FIELD Beskrivelse AS CHAR FORMAT "x(30)"
    FIELD KortNavn AS CHAR FORMAT "x(10)"
    .

DEF {&New} TEMP-TABLE tt_webVarGr
    FIELD iRecType  AS INT FORMAT ">9"
    FIELD Vg AS INT FORMAT ">>>>>9"
    FIELD VgBeskr AS CHAR FORMAT "x(30)"
    FIELD Hg AS INT FORMAT ">>>9"
    FIELD MomsKod AS INT FORMAT ">9"
    FIELD Kost_Proc AS DEC FORMAT "->>9.99"
    FIELD Mva_Proc AS DEC FORMAT "->>9.99"
    .

DEF {&New} TEMP-TABLE tt_webVgKundeGrpRabatt
    FIELD iRecType  AS INT FORMAT ">9"
    FIELD Vg AS INT FORMAT ">>>>>9"
    FIELD GruppeId AS INT FORMAT ">>>9"
    FIELD Rabatt_Proc AS DEC FORMAT "->>9.99"
    .

DEF {&New} TEMP-TABLE tt_webLager
    FIELD iRecType  AS INT FORMAT ">9"                 SERIALIZE-HIDDEN
    FIELD ArtikkelNr  AS dec FORMAT "9999999999999" DECIMALS 0
    FIELD Butik AS INT FORMAT ">>>>>9" 
    FIELD Vg    AS INT
    FIELD LopNr AS INT
    FIELD LevKod AS CHAR FORMAT "x(30)"                SERIALIZE-HIDDEN
    FIELD Beskr AS CHAR FORMAT "x(40)"                 SERIALIZE-HIDDEN
    FIELD LevFargKod AS CHAR FORMAT "x(30)"            SERIALIZE-HIDDEN
    FIELD StrTypeId AS INT FORMAT ">>>>>9"             SERIALIZE-HIDDEN
    FIELD StrKode AS INT FORMAT ">>>9"
    FIELD Storl AS CHAR FORMAT "x(10)"                 SERIALIZE-HIDDEN
    FIELD VVarekost AS DEC FORMAT "->>,>>>,>>>,>>9.99" SERIALIZE-HIDDEN
    FIELD Lagant AS DEC FORMAT "->>,>>>,>>9" DECIMALS 0
    FIELD Kode AS CHAR FORMAT "x(30)"                  SERIALIZE-HIDDEN
    FIELD Pris AS DECIMAL FORMAT "->>,>>>,>>>,>>9.99"  SERIALIZE-HIDDEN
    FIELD OneSize AS LOG FORMAT "Yes/No"
    INDEX Akkum ArtikkelNr Butik Kode
    .

DEF {&New} TEMP-TABLE tt_webStrKonv
    FIELD iRecType  AS INT FORMAT ">9"
    FIELD StrKode AS INT FORMAT ">>>9"
    FIELD Storl AS CHAR FORMAT "x(15)"
    FIELD Merknad AS CHAR FORMAT "x(30)"
    .

DEF {&New} TEMP-TABLE tt_webKundeGruppe
    FIELD iRecType  AS INT FORMAT ">9"
    FIELD GruppeId AS INT FORMAT ">>>9"
    FIELD Beskrivelse AS CHAR FORMAT "x(40)"
.

DEF {&New} TEMP-TABLE tt_webMoms
    FIELD iRecType  AS INT FORMAT ">9"
    FIELD MomsKod AS INT FORMAT ">9"
    FIELD MomsProc AS DEC FORMAT ">>9.99"
    FIELD Beskrivelse AS CHAR FORMAT "x(30)"
    .

DEF {&New} TEMP-TABLE tt_webVgAkt
    FIELD iRecType  AS INT FORMAT ">9"
    FIELD Vg AS INT FORMAT ">>>>>9"
    FIELD AktNr AS INTEGER FORMAT ">>9"
.

DEF {&New} TEMP-TABLE tt_webVgKat
    FIELD iRecType  AS INT FORMAT ">9"
    FIELD Vg AS INT FORMAT ">>>>>9"
    FIELD VgKat AS INTEGER FORMAT ">9"
    FIELD KatNr AS INTEGER FORMAT ">9"
.

DEF {&New} TEMP-TABLE tt_webHandtering
    FIELD iRecType  AS INT FORMAT ">9"
    FIELD HandKode AS INT FORMAT ">>>9"
    FIELD Beskrivelse AS CHAR FORMAT "x(30)"
    .

DEF {&New} TEMP-TABLE tt_webKlack
    FIELD iRecType  AS INT FORMAT ">9"
    FIELD Klack-Id AS INT FORMAT ">>>9"
    FIELD Beskrivning AS CHAR FORMAT "x(30)"
    .

DEF {&New} TEMP-TABLE tt_webInnersula
    FIELD iRecType  AS INT FORMAT ">9"
    FIELD Inner-Id AS INT FORMAT ">>>9"
    FIELD InnerBeskr AS CHAR FORMAT "x(30)"
    .

DEF {&New} TEMP-TABLE tt_webOvandel
    FIELD iRecType  AS INT FORMAT ">9"
    FIELD Ov-Id AS INT FORMAT ">>>9"
    FIELD OvBeskr AS CHAR FORMAT "x(30)"
    .

DEF {&New} TEMP-TABLE tt_webSlitsula
    FIELD iRecType  AS INT FORMAT ">9"
    FIELD Slit-Id AS INT FORMAT ">>>9"
    FIELD SlitBeskr AS CHAR FORMAT "x(30)"
    .

DEF {&New} TEMP-TABLE tt_webLast-Sko
    FIELD iRecType  AS INT FORMAT ">9"
    FIELD Last-Id AS INT FORMAT ">>>9"
    FIELD LastBeskr AS CHAR FORMAT "x(30)"
    .

DEF {&New} TEMP-TABLE tt_webAnv-Kod
    FIELD iRecType  AS INT FORMAT ">9"
    FIELD Anv-Id AS INT FORMAT ">>>9"
    FIELD AnvBeskr AS CHAR FORMAT "x(30)"
    .

DEF {&New} TEMP-TABLE tt_webRegnskapsavdeling
    FIELD iRecType  AS INT FORMAT ">9"
    FIELD RAvdNr AS INT FORMAT ">>>9"
    FIELD RAvdBeskrivelse AS CHAR FORMAT "x(30)"
    .

DEFINE {&New} TEMP-TABLE tt_webKunde 
  FIELD iRecType  AS INT FORMAT ">9"
  FIELD Adresse1            AS CHARACTER   FORMAT "X(40)" 
  FIELD Adresse2            AS CHARACTER   FORMAT "X(40)" 
  FIELD Aktiv               AS LOGICAL     INITIAL TRUE 
  FIELD Alder               AS INTEGER     FORMAT ">>9" 
  FIELD BankKonto           AS CHARACTER   FORMAT "X(20)" 
  FIELD BetBet              AS INTEGER     FORMAT ">>9"  
  FIELD BetType             AS INTEGER     FORMAT ">9" 
  FIELD BrukerID            AS CHARACTER   FORMAT "X(10)" 
  FIELD ButikkNr            AS INTEGER     FORMAT ">>>>>9" 
  FIELD BydelsNr            AS CHARACTER   FORMAT "X(8)" 
  FIELD DeresRef            AS CHARACTER   FORMAT "X(30)" 
  FIELD EDato               AS DATE        FORMAT "99/99/9999" 
  FIELD EksterntKundeNr     AS CHARACTER   FORMAT "X(20)" 
  FIELD ePostAdresse        AS CHARACTER   FORMAT "X(40)" 
  FIELD Etablert            AS DATE        
  FIELD ETid                AS INTEGER     
  FIELD FaktAdresse1        AS CHARACTER   FORMAT "X(30)" 
  FIELD FaktAdresse2        AS CHARACTER   FORMAT "X(30)" 
  FIELD FaktLand            AS CHARACTER   FORMAT "X(30)" 
  FIELD FaktPostNr          AS CHARACTER   FORMAT "X(15)" 
  FIELD FaktTekstNr         AS INTEGER     FORMAT ">>9" 
  FIELD Fakturagebyr        AS LOGICAL     INITIAL TRUE 
  FIELD Faktureringsperiode AS INTEGER     FORMAT "9" 
  FIELD FodtDato            AS DATE        
  FIELD ForsteKjop          AS DATE        
  FIELD GruppeId            AS INTEGER     FORMAT "zzz9" 
  FIELD Hovedkunde          AS LOGICAL     
  FIELD Kilde               AS CHARACTER   FORMAT "X(30)" 
  FIELD Kjon                AS INTEGER     FORMAT "9" 
  FIELD KobletTilKunde      AS DECIMAL     DECIMALS 2 FORMAT ">>>>>>>>>>>>9" 
  FIELD KontE-Post          AS CHARACTER   FORMAT "X(40)" 
  FIELD KontMobilTlf        AS CHARACTER   FORMAT "X(15)" 
  FIELD KontNavn            AS CHARACTER   FORMAT "X(40)" 
  FIELD KontTelefaks        AS CHARACTER   FORMAT "X(15)" 
  FIELD KontTelefon         AS CHARACTER   FORMAT "X(15)" 
  FIELD KreditSperret       AS LOGICAL     
  FIELD KundeNr             AS DECIMAL     DECIMALS 2 FORMAT ">>>>>>>>>>>>9" 
  FIELD KundeSaldo          AS DECIMAL     DECIMALS 2 FORMAT "->,>>>,>>9.99" 
  FIELD Land                AS CHARACTER   FORMAT "X(30)" 
  FIELD LevAdresse1         AS CHARACTER   FORMAT "X(30)" 
  FIELD LevAdresse2         AS CHARACTER   FORMAT "X(30)" 
  FIELD LevLand             AS CHARACTER   FORMAT "X(30)" 
  FIELD LevPostNr           AS CHARACTER   FORMAT "X(10)" 
  FIELD MaksKredit          AS DECIMAL     DECIMALS 2 
  FIELD MobilTlf            AS CHARACTER   FORMAT "X(15)" 
  FIELD Momskod             AS INTEGER     FORMAT ">>9" 
  FIELD Navn                AS CHARACTER   FORMAT "X(40)" 
  FIELD Opphort             AS DATE        
  FIELD OrgNr               AS CHARACTER   FORMAT "X(15)" 
  FIELD Postgiro            AS CHARACTER   FORMAT "X(20)" 
  FIELD PostNr              AS CHARACTER   FORMAT "X(10)" 
  FIELD Privat              AS LOGICAL     
  FIELD PrivatTlf           AS CHARACTER   FORMAT "X(15)" 
  FIELD Purregebyr          AS LOGICAL     INITIAL TRUE 
  FIELD RegistrertAv        AS CHARACTER   FORMAT "X(10)" 
  FIELD RegistrertDato      AS DATE        FORMAT "99/99/9999" 
  FIELD RegistrertTid       AS INTEGER     
  FIELD SamleFaktura        AS LOGICAL     
  FIELD SisteKjop           AS DATE        
  FIELD Stilling            AS CHARACTER   FORMAT "X(30)" 
  FIELD Telefaks            AS CHARACTER   FORMAT "X(15)" 
  FIELD Telefon             AS CHARACTER   FORMAT "X(15)" 
  FIELD TilgKilde           AS CHARACTER   FORMAT "X(30)" 
  FIELD TotalRabatt%        AS DECIMAL     DECIMALS 2 FORMAT "->9.99" 
  FIELD TypeId              AS INTEGER     FORMAT "zzz9" 
  FIELD ValKod              AS CHARACTER   FORMAT "x(3)" 
  FIELD WebKunde            AS LOGICAL
.

DEFINE TEMP-TABLE tt_webKundesaldo
  FIELD iRecType  AS INT FORMAT ">9"
  FIELD BrukerID       AS CHARACTER   FORMAT "X(10)" LABEL "Bruker"
  FIELD ButikkNr       AS INTEGER     FORMAT ">>>>>9" LABEL "Butikk"
  FIELD DatoSiste      AS DATE        LABEL "Dato siste kjøp" COLUMN-LABEL "Siste kjøp"
  FIELD EDato          AS DATE        FORMAT "99/99/9999" LABEL "Endret"
  FIELD ETid           AS INTEGER     LABEL "Endret tid" COLUMN-LABEL "ETid"
  FIELD ForsteDato     AS DATE        LABEL "Dato første køp" COLUMN-LABEL "Første kjøp"
  FIELD ForsteTid      AS INTEGER     LABEL "FørsteTid"
  FIELD KundeNr        AS DECIMAL     DECIMALS 2 FORMAT ">>>>>>>>>>>>9" LABEL "Kundenummer" COLUMN-LABEL "KundeNr"
  FIELD RegistrertAv   AS CHARACTER   FORMAT "X(10)" LABEL "Registrert av" COLUMN-LABEL "Reg.Av"
  FIELD RegistrertDato AS DATE        FORMAT "99/99/9999" LABEL "Registrert dato" COLUMN-LABEL "RDato"
  FIELD RegistrertTid  AS INTEGER     LABEL "Registreringstidspunkt"
  FIELD Saldo          AS DECIMAL     DECIMALS 2 FORMAT "->>>,>>>,>>9.99" LABEL "Saldo"
  FIELD SisteTid       AS INTEGER     LABEL "SisteTid"
  FIELD TotaltKjop     AS DECIMAL     DECIMALS 2 FORMAT "->>>,>>>,>>9.99" LABEL "Totalt kjøp"
  .

DEFINE TEMP-TABLE tt_webKundekort
  FIELD iRecType  AS INT FORMAT ">9"
  FIELD AktivertDato   AS DATE        LABEL "Aktivert dato"
  FIELD BrukerID       AS CHARACTER   FORMAT "X(10)" LABEL "Bruker"
  FIELD EDato          AS DATE        FORMAT "99/99/9999" LABEL "Endret"
  FIELD ETid           AS INTEGER     LABEL "Endret tid" COLUMN-LABEL "ETid"
  FIELD Innehaver      AS CHARACTER   FORMAT "X(30)" LABEL "Innehaver"
  FIELD InterntKKortId AS DECIMAL     DECIMALS 2 FORMAT ">>>>>>>>>>>>9" LABEL "Internt kortid"
  FIELD KortNr         AS CHARACTER   FORMAT "X(22)" LABEL "Kortnummer"
  FIELD KundeNr        AS DECIMAL     DECIMALS 2 FORMAT ">>>>>>>>>>>>9" LABEL "Kundenummer" COLUMN-LABEL "KundeNr"
  FIELD MedlemsNr      AS DECIMAL     DECIMALS 2 FORMAT ">>>>>>>>>>>>9" LABEL "Medlemsnummer" COLUMN-LABEL "MedlemNr"
  FIELD Merknad        AS CHARACTER   FORMAT "X(40)" LABEL "Merknad"
  FIELD RegistrertAv   AS CHARACTER   FORMAT "X(10)" LABEL "Registrert av" COLUMN-LABEL "Reg.Av"
  FIELD RegistrertDato AS DATE        FORMAT "99/99/9999" LABEL "Registrert dato" COLUMN-LABEL "RDato"
  FIELD RegistrertTid  AS INTEGER     LABEL "Registreringstidspunkt"
  FIELD Sperret        AS LOGICAL     LABEL "Sperret"
  FIELD UtgarDato      AS DATE        LABEL "Utgår dato"
    .
    
DEFINE TEMP-TABLE tt_webMedlem
  FIELD iRecType          AS INT       FORMAT ">9"
  FIELD Adresse1          AS CHARACTER FORMAT "X(40)" LABEL "Adresse"
  FIELD Adresse2          AS CHARACTER FORMAT "X(40)" LABEL "Adresse"
  FIELD Aktiv             AS LOGICAL   INITIAL TRUE LABEL "Aktiv"
  FIELD AktivertFraWeb    AS DATE      FORMAT "99/99/9999" LABEL "Aktivert fra Web"
  FIELD BrukerID          AS CHARACTER FORMAT "X(10)" LABEL "Bruker"
  FIELD ButikkNr          AS INTEGER   FORMAT ">>>>>9" LABEL "Butikk"
  FIELD BydelsNr          AS CHARACTER FORMAT "X(8)" LABEL "Bydelsnummer" COLUMN-LABEL "BydelsNr"
  FIELD EDato             AS DATE      FORMAT "99/99/9999" LABEL "Endret"
  FIELD EksterntMedlemsNr AS CHARACTER FORMAT "X(20)" LABEL "Ekst.medlemsnr"
  FIELD ePostAdresse      AS CHARACTER FORMAT "X(40)" LABEL "E-Post adresse" COLUMN-LABEL "E-Post"
  FIELD ETid              AS INTEGER   LABEL "Endret tid" COLUMN-LABEL "ETid"
  FIELD EtterNavn         AS CHARACTER FORMAT "X(40)" LABEL "Etternavn"
  FIELD FodselsDato       AS DATE      LABEL "Fødselsdato"
  FIELD FodtAr            AS INTEGER   FORMAT "9999" LABEL "Fødselsår" COLUMN-LABEL ""
  FIELD ForNavn           AS CHARACTER FORMAT "X(40)" LABEL "Navn"
  FIELD HovedMedlemFlagg  AS LOGICAL   INITIAL TRUE LABEL "Hovedmedlem"
  FIELD HovedMedlemsNr    AS DECIMAL   DECIMALS 2 FORMAT ">>>>>>>>>>>>9" LABEL "Hovedmedlem" COLUMN-LABEL "MedlemNr"
  FIELD Kilde             AS CHARACTER FORMAT "X(30)" LABEL "Kilde"
  FIELD Kjonn             AS LOGICAL   FORMAT "M/K" INITIAL TRUE LABEL "Kjønn"
  FIELD KundeNr           AS DECIMAL   DECIMALS 2 FORMAT ">>>>>>>>>>>>9" LABEL "Kundenummer" COLUMN-LABEL "KundeNr"
  FIELD Land              AS CHARACTER FORMAT "X(30)" LABEL "Land"
  FIELD MedGruppe         AS INTEGER   FORMAT "zzz9" LABEL "Medlemsgruppe"
  FIELD MedlemsNr         AS DECIMAL   DECIMALS 2 FORMAT ">>>>>>>>>>>>9" LABEL "Medlemsnummer" COLUMN-LABEL "MedlemNr"
  FIELD MedType           AS INTEGER   FORMAT "zzz9" LABEL "Medlemstype"
  FIELD MobilTlf          AS CHARACTER FORMAT "X(15)" LABEL "Mobiltelefon"
  FIELD Opphort           AS DATE      LABEL "Opphørt"
  FIELD PostNr            AS CHARACTER FORMAT "X(10)" LABEL "PostNr"
  FIELD Rabatt            AS DECIMAL   DECIMALS 2 LABEL "Rabatt%"
  FIELD RegistrertAv      AS CHARACTER FORMAT "X(10)" LABEL "Registrert av" COLUMN-LABEL "Reg.Av"
  FIELD RegistrertDato    AS DATE      FORMAT "99/99/9999" LABEL "Registrert dato" COLUMN-LABEL "RDato"
  FIELD RegistrertTid     AS INTEGER   LABEL "Registreringstidspunkt"
  FIELD RegKode           AS CHARACTER FORMAT "X(10)" LABEL "Regionskode" COLUMN-LABEL "RegKode"
  FIELD Telefaks          AS CHARACTER FORMAT "X(15)" LABEL "Telefaks"
  FIELD Telefon           AS CHARACTER FORMAT "X(15)" LABEL "Telefon"
  FIELD TilgKilde         AS CHARACTER FORMAT "X(30)" LABEL "Tilg.kilde"
  FIELD WebBrukerId       AS CHARACTER FORMAT "X(15)" LABEL "Brukerid (Web)"
  FIELD WebPassord        AS CHARACTER FORMAT "X(15)" LABEL "Passord (Web)"
  FIELD Personnr          AS CHARACTER FORMAT "x(20)" LABEL "Personnr"
  .
    
DEFINE TEMP-TABLE tt_webMedlemsaldo 
  FIELD iRecType       AS INT       FORMAT ">9"
  FIELD BrukerID       AS CHARACTER FORMAT "X(10)" LABEL "Bruker"
  FIELD ButikkNr       AS INTEGER   FORMAT ">>>>>9" LABEL "Butikk"
  FIELD DatoSiste      AS DATE      LABEL "Dato siste kjøp" COLUMN-LABEL "Siste kjøp"
  FIELD EDato          AS DATE      FORMAT "99/99/9999" LABEL "Endret"
  FIELD ETid           AS INTEGER   LABEL "Endret tid" COLUMN-LABEL "ETid"
  FIELD ForsteDato     AS DATE      LABEL "Dato første køp" COLUMN-LABEL "Første kjøp"
  FIELD ForsteTid      AS INTEGER   LABEL "FørsteTid"
  FIELD MedlemsNr      AS DECIMAL   DECIMALS 2 FORMAT ">>>>>>>>>>>>9" LABEL "MEdlemsnummer" COLUMN-LABEL "MedlemsNr"
  FIELD RegistrertAv   AS CHARACTER FORMAT "X(10)" LABEL "Registrert av" COLUMN-LABEL "Reg.Av"
  FIELD RegistrertDato AS DATE      FORMAT "99/99/9999" LABEL "Registrert dato" COLUMN-LABEL "RDato"
  FIELD RegistrertTid  AS INTEGER   LABEL "Registreringstidspunkt"
  FIELD Saldo          AS DECIMAL   DECIMALS 2 FORMAT "->>>,>>>,>>9.99" LABEL "Saldo"
  FIELD SisteTid       AS INTEGER   LABEL "SisteTid"
  FIELD TotaltKjop     AS DECIMAL   DECIMALS 2 FORMAT "->>>,>>>,>>9.99" LABEL "Totalt kjøp"
  .

DEFINE TEMP-TABLE tt_webMedlemskort 
  FIELD iRecType  AS INT FORMAT ">9"
  FIELD AktivertDato    AS DATE        LABEL "Aktivert dato"
  FIELD BrukerID        AS CHARACTER   FORMAT "X(10)" LABEL "Bruker"
  FIELD EDato           AS DATE        FORMAT "99/99/9999" LABEL "Endret"
  FIELD ETid            AS INTEGER     LABEL "Endret tid" COLUMN-LABEL "ETid"
  FIELD Innehaver       AS CHARACTER   FORMAT "X(30)" LABEL "Innehaver"
  FIELD InterntKKortId  AS DECIMAL     DECIMALS 2 FORMAT ">>>>>>>>>>>>9" LABEL "Internt kortid"
  FIELD KortNr          AS CHARACTER   FORMAT "X(22)" LABEL "Kortnummer"
  FIELD KortType        AS INTEGER     FORMAT "9" INITIAL 1 LABEL "Korttype" COLUMN-LABEL "Korttype"
  FIELD KundeKortNr     AS CHARACTER   FORMAT "X(22)" LABEL "Kundekortnr"
  FIELD KundeRabattKort AS LOGICAL     LABEL "Kunderabattkort" COLUMN-LABEL "AnnenKlubb"
  FIELD MedlemsNr       AS DECIMAL     DECIMALS 2 FORMAT ">>>>>>>>>>>>9" LABEL "Medlemsnummer" COLUMN-LABEL "MedlemsNr"
  FIELD Merknad         AS CHARACTER   FORMAT "X(40)" LABEL "Merknad"
  FIELD RegistrertAv    AS CHARACTER   FORMAT "X(10)" LABEL "Registrert av" COLUMN-LABEL "Reg.Av"
  FIELD RegistrertDato  AS DATE        FORMAT "99/99/9999" LABEL "Registrert dato" COLUMN-LABEL "RDato"
  FIELD RegistrertTid   AS INTEGER     LABEL "Registreringstidspunkt"
  FIELD Sperret         AS LOGICAL     LABEL "Sperret"
  FIELD UtgarDato       AS DATE        LABEL "Utgår dato"
  .

DEFINE TEMP-TABLE tt_webMedlemsgruppe
  FIELD iRecType       AS INT       FORMAT ">9"
  FIELD Beskrivelse    AS CHARACTER FORMAT "X(40)" LABEL "Beskrivelse"
  FIELD BrukerID       AS CHARACTER FORMAT "X(10)" LABEL "Bruker"
  FIELD EDato          AS DATE      FORMAT "99/99/9999" LABEL "Endret"
  FIELD ETid           AS INTEGER   LABEL "Endret tid" COLUMN-LABEL "ETid"
  FIELD MedGruppe      AS INTEGER   FORMAT "zzz9" LABEL "Medlemsgruppe" COLUMN-LABEL "MGrp"
  FIELD Notat          AS CHARACTER FORMAT "X(8)" LABEL "Notat"
  FIELD RegistrertAv   AS CHARACTER FORMAT "X(10)" LABEL "Registrert av" COLUMN-LABEL "Reg.Av"
  FIELD RegistrertDato AS DATE      FORMAT "99/99/9999" LABEL "Registrert dato" COLUMN-LABEL "RDato"
  FIELD RegistrertTid  AS INTEGER   LABEL "Registreringstidspunkt"
    .

DEFINE TEMP-TABLE tt_webStrTstr
  FIELD iRecType       AS INT       FORMAT ">9"
  FIELD StrTypeID      AS INTEGER FORMAT '>>>>>>>9'
  FIELD Beskrivelse    AS CHARACTER FORMAT "x(30)"
  FIELD Intervall      AS CHARACTER FORMAT "x(30)"
  FIELD Fordeling      AS CHARACTER FORMAT "x(30)"
  FIELD KortNavn       AS CHARACTER FORMAT "x(30)"
  FIELD AlfaFordeling  AS CHARACTER FORMAT "x(30)"
  FIELD Hg             AS INTEGER FORMAT '>>>9'
  FIELD AvdelingNr     AS INTEGER FORMAT '>>9'
  FIELD SoStorl        AS CHARACTER FORMAT 'x(12)'
  FIELD SeqNr          AS INTEGER FORMAT '>>>>>>>9'
  FIELD StrKode        AS INTEGER FORMAT '>>>>9'
  INDEX tt_webStrTStrSeq IS PRIMARY StrTypeId.

DEFINE TEMP-TABLE tt_webFarg
  FIELD iRecType       AS INT       FORMAT ">9"
  FIELD Farg           AS INTEGER FORMAT 'zzzzz9'
  FIELD FarBeskr       AS CHARACTER FORMAT "x(30)"
  .
  
DEFINE TEMP-TABLE tt_webHovedkategori
  FIELD iRecType      AS INT       FORMAT ">9"      
  FIELD HovedKatNr    AS INTEGER   FORMAT ">9"
  FIELD HovedKatTekst AS CHARACTER FORMAT "X(30)"
  .  

DEFINE TEMP-TABLE tt_webUnderkategori
  FIELD iRecType      AS INT       FORMAT ">9"      
  FIELD UnderKatNr    AS INTEGER   FORMAT ">>9"
  FIELD UnderKatTekst AS CHARACTER FORMAT "X(30)"
  .  

DEFINE TEMP-TABLE tt_webkordrehode
  FIELD iRecType             AS INT         FORMAT ">9"
  FIELD Adresse1             AS CHARACTER   FORMAT "X(40)" LABEL "Adresse"
  FIELD Adresse2             AS CHARACTER   FORMAT "X(40)" LABEL "Adresse"
  FIELD AnsvVerksted         AS CHARACTER   FORMAT "X(8)" LABEL "Ansv. versted"
  FIELD AntDager             AS INTEGER     FORMAT ">>9" LABEL "Antall betalingsfrie dager" COLUMN-LABEL "AntDg"
  FIELD AntKolli             AS INTEGER     FORMAT ">9" LABEL "Antall kolli"
  FIELD AvgFriSalg           AS DECIMAL     DECIMALS 2 FORMAT "->>,>>>,>>9.99" LABEL "Avg. fritt salg" COLUMN-LABEL "AvgFriSalg"
  FIELD AvgPlSalg            AS DECIMAL     DECIMALS 2 FORMAT "->>,>>>,>>9.99" LABEL "Avg. pliktig salg" COLUMN-LABEL "AvgPlSalg"
  FIELD Avrund               AS DECIMAL     DECIMALS 2 LABEL "Avrundet"
  FIELD AvrundingKr          AS INTEGER     LABEL "Avrunding"
  FIELD AvrundingType        AS INTEGER     FORMAT ">9" LABEL "Avrundingstype"
  FIELD BetaltDato           AS DATE        LABEL "Betalt"
  FIELD BetBet               AS INTEGER     FORMAT ">>9" LABEL "Betalings.bet"
  FIELD BetTekst             AS CHARACTER   FORMAT "X(30)" LABEL "Betalingsbetingelse"
  FIELD BrukerID             AS CHARACTER   FORMAT "X(10)" LABEL "Bruker"
  FIELD Bruttovekt           AS DECIMAL     DECIMALS 2 FORMAT "->>,>>9.999" LABEL "Bruttovekt"
  FIELD Butik                AS INTEGER     FORMAT ">>>>>9" LABEL "Butikknummer" COLUMN-LABEL "ButNr"
  FIELD ButikkNr             AS INTEGER     FORMAT ">>>>>9" LABEL "Butikk"
  FIELD Dato                 AS DATE        LABEL "Fakturadato"
  FIELD DeresRef             AS CHARACTER   FORMAT "X(30)" LABEL "Deres referanse"
  FIELD EDato                AS DATE        FORMAT "99/99/9999" LABEL "Endret"
  FIELD EkstOrdreNr          AS CHARACTER   FORMAT "X(15)" LABEL "Ekst.ordrenr"
  FIELD Embalage             AS CHARACTER   FORMAT "X(30)" LABEL "Embalage/godsslag"
  FIELD ePostAdresse         AS CHARACTER   FORMAT "X(40)" LABEL "E-Post adresse" COLUMN-LABEL "E-Post"
  FIELD ETid                 AS INTEGER     LABEL "Endret tid" COLUMN-LABEL "ETid"
  FIELD FaktAdresse1         AS CHARACTER   FORMAT "X(30)" LABEL "Fakt.adresse"
  FIELD FaktAdresse2         AS CHARACTER   FORMAT "X(30)" LABEL "Fakt.Adresse"
  FIELD FaktLand             AS CHARACTER   FORMAT "X(30)" LABEL "Land"
  FIELD FaktPostNr           AS CHARACTER   FORMAT "X(15)" LABEL "Postnr"
  FIELD FaktPoststed         AS CHARACTER   FORMAT "X(8)" LABEL "Fakt. Poststed"
  FIELD FaktTekstNr          AS INTEGER     FORMAT ">>9" LABEL "FakturatekstNr"
  FIELD FakturertAv          AS CHARACTER   FORMAT "X(20)" LABEL "Fakturert av"
  FIELD FakturertDato        AS DATE        LABEL "Fakturert dato"
  FIELD FakturertTid         AS INTEGER     LABEL "Fakturert tid"
  FIELD FirmaAdresse1        AS CHARACTER   FORMAT "X(30)" LABEL "Firmaadresse"
  FIELD FirmaAdresse2        AS CHARACTER   FORMAT "X(30)" LABEL "Firmaadresse"
  FIELD FirmaBankKonto       AS CHARACTER   FORMAT "X(20)" LABEL "Bankkonto"
  FIELD FirmaEPost           AS CHARACTER   FORMAT "X(40)" LABEL "E-Post"
  FIELD FirmaLand            AS CHARACTER   FORMAT "X(30)" LABEL "Land"
  FIELD FirmaNavn            AS CHARACTER   FORMAT "X(30)" LABEL "Firma"
  FIELD FirmaOrganisasjonsNr AS CHARACTER   FORMAT "X(12)" LABEL "OrganisasjonsNr"
  FIELD FirmaPostgiro        AS CHARACTER   FORMAT "X(20)" LABEL "Postgiro"
  FIELD FirmaPostNr          AS CHARACTER   FORMAT "X(10)" LABEL "PostNr"
  FIELD FirmaPoststed        AS CHARACTER   FORMAT "X(30)" LABEL "Poststed"
  FIELD FirmaTelefaks        AS CHARACTER   FORMAT "X(15)" LABEL "Telefaks"
  FIELD FirmaTelefon         AS CHARACTER   FORMAT "X(15)" LABEL "Telefon"
  FIELD FirmaURLAdresse      AS CHARACTER   FORMAT "X(40)" LABEL "URL adresse"
  FIELD ForfallsDato         AS DATE        LABEL "Forfall"
  FIELD ForsNr               AS INTEGER     FORMAT ">>>>>9" LABEL "Kasserer"
  FIELD Fraktbrevtekst       AS CHARACTER   FORMAT "X(30)" LABEL "Fraktbrevtekst"
  FIELD Godsmerking          AS CHARACTER   FORMAT "X(30)" LABEL "Godsmerking"
  FIELD KasseNr              AS INTEGER     FORMAT ">>9" LABEL "Kassenummer" COLUMN-LABEL "KasseNr"
  FIELD KontNavn             AS CHARACTER   FORMAT "X(40)" LABEL "Kontaktperson"
  FIELD KontoNr              AS INTEGER     FORMAT ">>9999" LABEL "Kontonummer" COLUMN-LABEL "Konto"
  FIELD KontTelefon          AS CHARACTER   FORMAT "X(15)" LABEL "Telefon"
  FIELD KOrdre_Id            AS DECIMAL     DECIMALS 2 FORMAT ">>>>>>>>>>>>9" LABEL "KOrdre Id" COLUMN-LABEL "KOId"
  FIELD KProsjektNr          AS INTEGER     FORMAT ">>>9" LABEL "Kundeprosjekt"
  FIELD KundeNr              AS DECIMAL     DECIMALS 2 FORMAT ">>>>>>>>>>>>9" LABEL "Kundenummer" COLUMN-LABEL "KundeNr"
  FIELD LevAdresse1          AS CHARACTER   FORMAT "X(40)" LABEL "Leveringsadresse"
  FIELD LevAdresse2          AS CHARACTER   FORMAT "X(40)" LABEL "Leveringsadresse"
  FIELD Leveringsdato        AS DATE        LABEL "Leveringsdato"
  FIELD LevFNr               AS INTEGER     FORMAT ">9" LABEL "Leveringsform"
  FIELD LevLand              AS CHARACTER   FORMAT "X(30)" LABEL "Lev. Land"
  FIELD LevPostNr            AS CHARACTER   FORMAT "X(10)" LABEL "Lev. PostNr"
  FIELD LevPostSted          AS CHARACTER   FORMAT "X(30)" LABEL "Poststed"
  FIELD LevStatus            AS CHARACTER   FORMAT "x(2)" LABEL "Lev.status"
  FIELD Mva                  AS DECIMAL     DECIMALS 2 FORMAT "->>,>>>,>>9.99" LABEL "Mva"
  FIELD MvaKr                AS DECIMAL     DECIMALS 2 FORMAT "->>>,>>9.99" LABEL "Mva"
  FIELD Navn                 AS CHARACTER   FORMAT "X(40)" LABEL "Navn"
  FIELD Opphav               AS INTEGER     FORMAT ">9" LABEL "Opphav"
  FIELD PostNr               AS CHARACTER   FORMAT "X(10)" LABEL "PostNr"
  FIELD PostSted             AS CHARACTER   FORMAT "X(30)" LABEL "Poststed"
  FIELD ProdStatus           AS CHARACTER   FORMAT "X" LABEL "Prod.status"
  FIELD ProduksjonsDato      AS DATE        LABEL "Produsert"
  FIELD Referanse            AS CHARACTER   FORMAT "X(30)" LABEL "Referanse"
  FIELD RegistrertAv         AS CHARACTER   FORMAT "X(10)" LABEL "Registrert av" COLUMN-LABEL "Reg.Av"
  FIELD RegistrertDato       AS DATE        FORMAT "99/99/9999" LABEL "Registrert dato" COLUMN-LABEL "RDato"
  FIELD RegistrertTid        AS INTEGER     LABEL "Registreringstidspunkt"
  FIELD SelgerNr             AS DECIMAL     DECIMALS 2 FORMAT ">>>>>>>>>>>>9" LABEL "Selgernummer" COLUMN-LABEL "SelgerNr"
  FIELD SendingsNr           AS CHARACTER   FORMAT "X(30)" LABEL "Sendingsnummer"
  FIELD Telefaks             AS CHARACTER   FORMAT "X(15)" LABEL "Telefaks"
  FIELD Telefon              AS CHARACTER   FORMAT "X(15)" LABEL "Telefon"
  FIELD TotalRabatt%         AS DECIMAL     DECIMALS 2 FORMAT "->9.99" LABEL "Totalrabatt%"
  FIELD TotalRabattKr        AS DECIMAL     DECIMALS 2 FORMAT "->>>,>>9.99" LABEL "Totalrabatt"
  FIELD Totalt               AS DECIMAL     DECIMALS 2 FORMAT "->>>,>>>,>>9.99" LABEL "Totalt" COLUMN-LABEL "Totalt"
  FIELD TotaltVolum          AS DECIMAL     DECIMALS 2 FORMAT "->>,>>9.999" LABEL "Totalt volum"
  FIELD Utsendelsesdato      AS DATE        LABEL "Utsendelsesdato"
  FIELD VaarRef              AS CHARACTER   FORMAT "X(30)" LABEL "Vår referanse"
  FIELD ValKod               AS CHARACTER   FORMAT "x(3)" LABEL "ValutaKode"
  FIELD VerkstedMerknad      AS CHARACTER   FORMAT "X(80)" LABEL "Merknad (Verksted)"
  FIELD Verkstedordre        AS LOGICAL     LABEL "Verkstedordre"
  .

DEFINE TEMP-TABLE tt_webkordrelinje
  FIELD iRecType          AS INT         FORMAT ">9"
  FIELD Antall            AS DECIMAL     DECIMALS 2 FORMAT "->>,>>9" LABEL "Antall"
  FIELD ArbeidsBeskr      AS CHARACTER   FORMAT "X(40)" LABEL "Arbeidsbeskrivelse"
  FIELD Bestillingsnummer AS CHARACTER   FORMAT "X(25)" LABEL "Bestillingsnummer"
  FIELD BestNr            AS INTEGER     FORMAT ">>>>>>>9" LABEL "Bestilling" COLUMN-LABEL "BestNr"
  FIELD BrukerID          AS CHARACTER   FORMAT "X(10)" LABEL "Bruker"
  FIELD BruttoPris        AS DECIMAL     DECIMALS 2 FORMAT "->>>,>>9.99" LABEL "Bruttopris"
  FIELD Db%               AS DECIMAL     DECIMALS 2 LABEL "Db%"
  FIELD DbKr              AS DECIMAL     DECIMALS 2 FORMAT "->>>,>>9.99" LABEL "DbKr"
  FIELD Depositum         AS DECIMAL     DECIMALS 2 LABEL "Depositum"
  FIELD EDato             AS DATE        FORMAT "99/99/9999" LABEL "Endret"
  FIELD ETid              AS INTEGER     LABEL "Endret tid" COLUMN-LABEL "ETid"
  FIELD Faktura_Id        AS DECIMAL     DECIMALS 2 FORMAT ">>>>>>>>>>>>9" LABEL "Faktura Id" COLUMN-LABEL "FId"
  FIELD KOrdreLinjeNr     AS INTEGER     FORMAT ">>>>>>9" LABEL "KOrdreLinje"
  FIELD KOrdre_Id         AS DECIMAL     DECIMALS 2 FORMAT ">>>>>>>>>>>>9" INITIAL ? LABEL "KOrdre Id" COLUMN-LABEL "FId"
  FIELD KundeRab%         AS DECIMAL     DECIMALS 2 FORMAT "->,>>9.99" LABEL "Kunderabatt%"
  FIELD KundeRabattKr     AS DECIMAL     DECIMALS 2 FORMAT "->>>,>>9.99" LABEL "Kunderabatt"
  FIELD Leveringsdato     AS DATE        LABEL "Leveringsdato"
  FIELD LevFargKod        AS CHARACTER   FORMAT "X(15)" LABEL "LevFargKod"
  FIELD LinjeRab%         AS DECIMAL     DECIMALS 2 LABEL "Linjerabatt%"
  FIELD LinjeRabattKr     AS DECIMAL     DECIMALS 2 FORMAT "->>>,>>9.99" LABEL "Linjerabatt"
  FIELD Linjesum          AS DECIMAL     DECIMALS 2 FORMAT "->>>,>>9.99" LABEL "Linjesum"
  FIELD MomsKod           AS INTEGER     FORMAT "z9" LABEL "Mva"
  FIELD Mva%              AS DECIMAL     DECIMALS 2 LABEL "Mva%"
  FIELD MvaKr             AS DECIMAL     DECIMALS 2 FORMAT "->>>,>>9.99" LABEL "Mva"
  FIELD NettoLinjesum     AS DECIMAL     DECIMALS 2 FORMAT "->>>,>>9.99" LABEL "Netto linjesum"
  FIELD NettoPris         AS DECIMAL     DECIMALS 2 FORMAT "->>>,>>9.99" LABEL "Nettopris"
  FIELD Notat             AS CHARACTER   FORMAT "X(40)" LABEL "Notat"
  FIELD Opphav            AS INTEGER     FORMAT ">9" LABEL "Opphav"
  FIELD OrdreRabattKr     AS DECIMAL     DECIMALS 2 FORMAT "->>>,>>9.99" LABEL "Ordrerabattkr"
  FIELD PakkeIdx          AS INTEGER     FORMAT ">9"
  FIELD Pris              AS DECIMAL     DECIMALS 2 LABEL "Pris"
  FIELD RefNr             AS INTEGER     LABEL "Referanse nr." COLUMN-LABEL "Ref.nr"
  FIELD RefTekst          AS CHARACTER   FORMAT "X(40)" LABEL "Referansetekst" COLUMN-LABEL "Ref.tekst"
  FIELD RegistrertAv      AS CHARACTER   FORMAT "X(10)" LABEL "Registrert av" COLUMN-LABEL "Reg.Av"
  FIELD RegistrertDato    AS DATE        FORMAT "99/99/9999" LABEL "Registrert dato" COLUMN-LABEL "RDato"
  FIELD RegistrertTid     AS INTEGER     LABEL "Registreringstidspunkt"
  FIELD Storl             AS CHARACTER   FORMAT "x(10)" LABEL "Størrelse" COLUMN-LABEL "Str"
  FIELD StrKode           AS INTEGER     FORMAT ">>9" LABEL "Størrelseskode" COLUMN-LABEL "StrKode"
  FIELD Tilbud            AS LOGICAL     LABEL "Tilbud" COLUMN-LABEL "T"
  FIELD ValKod            AS CHARACTER   FORMAT "X(3)" LABEL "Valutakode" COLUMN-LABEL "Valuta"
  FIELD VareBehNr         AS DECIMAL     DECIMALS 2 FORMAT ">>>>>>>>>>>>9" LABEL "VarehåndteringsNr"
  FIELD VareKost          AS DECIMAL     DECIMALS 2 FORMAT "->,>>>,>>9.99" LABEL "Varekost"
  FIELD VareNr            AS CHARACTER   FORMAT "X(20)" LABEL "VareNr"
  FIELD Varespesifikasjon AS CHARACTER   FORMAT "X(30)" LABEL "Varespesifikasjon"
  FIELD Varetekst         AS CHARACTER   FORMAT "X(30)" LABEL "Varetekst"
  .

DEFINE DATASET dsArt FOR tt_webArtikkel, tt_Farg, tt_varemerke,tt_Material,tt_HovedKategori,tt_ArtBasUnderkategori,tt_Klack,tt_InnerSula,tt_Ovandel,tt_SlitSula,tt_Last-Sko,tt_Storl
       DATA-RELATION drArtFarg FOR tt_webArtikkel, tt_Farg RELATION-FIELDS (artikkelnr,artikkelnr) NESTED
       DATA-RELATION drArtvaremerke           FOR tt_webArtikkel, tt_varemerke           RELATION-FIELDS (artikkelnr,artikkelnr) NESTED
       DATA-RELATION drArtMaterial            FOR tt_webArtikkel, tt_Material            RELATION-FIELDS (artikkelnr,artikkelnr) NESTED
       DATA-RELATION drArtHovedKategori       FOR tt_webArtikkel, tt_HovedKategori       RELATION-FIELDS (artikkelnr,artikkelnr) NESTED
       DATA-RELATION drArtArtBasUnderkategori FOR tt_webArtikkel, tt_ArtBasUnderkategori RELATION-FIELDS (artikkelnr,artikkelnr) NESTED
       DATA-RELATION drArtKlack               FOR tt_webArtikkel, tt_Klack               RELATION-FIELDS (artikkelnr,artikkelnr) NESTED
       DATA-RELATION drArtInnerSula           FOR tt_webArtikkel, tt_InnerSula           RELATION-FIELDS (artikkelnr,artikkelnr) NESTED
       DATA-RELATION drArtOvandel             FOR tt_webArtikkel, tt_Ovandel             RELATION-FIELDS (artikkelnr,artikkelnr) NESTED
       DATA-RELATION drArtSlitSula            FOR tt_webArtikkel, tt_SlitSula            RELATION-FIELDS (artikkelnr,artikkelnr) NESTED
       DATA-RELATION drArtLast-Sko            FOR tt_webArtikkel, tt_Last-Sko            RELATION-FIELDS (artikkelnr,artikkelnr) NESTED
       DATA-RELATION drArtStorl               FOR tt_webArtikkel, tt_Storl               RELATION-FIELDS (artikkelnr,artikkelnr) NESTED.
