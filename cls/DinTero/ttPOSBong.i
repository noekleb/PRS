
/*------------------------------------------------------------------------
    File        : ttPOSBong.i
    Purpose     : 

    Syntax      :

    Description : Dfinierer temp-tabell for POS BongHode tabellen.

    Author(s)   : Tom Nøkleby
    Created     : Fri Nov 20 10:59:27 CET 2020
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */


/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

DEFINE TEMP-TABLE ttPOSBongHode NO-UNDO SERIALIZE-NAME 'BongHode'
  FIELD b_id             AS DECIMAL     DECIMALS 2 FORMAT ">>>>>>>>>>>9" LABEL "BongId"
  FIELD ButikkNr         AS INTEGER     FORMAT ">>>>>9" LABEL "Butikknummer" COLUMN-LABEL "ButNr"
  FIELD GruppeNr         AS INTEGER     FORMAT ">9" LABEL "Gruppenummer" COLUMN-LABEL "GrNr"
  FIELD KasseNr          AS INTEGER     FORMAT ">>9" LABEL "Kassenummer" COLUMN-LABEL "KasseNr"
  FIELD Dato             AS DATE        LABEL "Dato"
  FIELD BongNr           AS INTEGER     FORMAT ">>>>>>>9" LABEL "Bongnummer" COLUMN-LABEL "BongNr"
  FIELD Belop            AS DECIMAL     DECIMALS 2 FORMAT "->,>>>,>>9.99" LABEL "Beløp"
  FIELD BongStatus       AS INTEGER     FORMAT "9" LABEL "Status"
  FIELD DataSettId       AS DECIMAL     DECIMALS 2 FORMAT ">>>>>>>>>>>>>9" LABEL "DatasettId"
  FIELD EAv              AS CHARACTER   FORMAT "X(15)" LABEL "Endret av" COLUMN-LABEL "EAv"
  FIELD EDato            AS DATE        LABEL "Endret dato" COLUMN-LABEL "EDato"
  FIELD EksportertDato   AS DATE        LABEL "Eksportert"
  FIELD ETid             AS INTEGER     LABEL "Endret tid" COLUMN-LABEL "ETid"
  FIELD flBankkort       AS LOGICAL     LABEL "Bankkort"
  FIELD flBetalingskort  AS LOGICAL     LABEL "Betalingskort"
  FIELD flGavekort       AS LOGICAL     LABEL "Gavekort"
  FIELD flKreditkort     AS LOGICAL     LABEL "Kreditkort"
  FIELD flKupong1        AS LOGICAL     LABEL "Kupong1"
  FIELD flRabatt         AS LOGICAL     LABEL "Rabatt"
  FIELD flRekvisisasjon  AS LOGICAL     LABEL "Rekvisisasjon"
  FIELD flSjekk          AS LOGICAL     LABEL "Sjekk"
  FIELD flSlKort         AS INTEGER     FORMAT ">>>9"
  FIELD flSystemkort     AS LOGICAL     LABEL "Systemkort"
  FIELD Gradering        AS INTEGER     FORMAT ">9" LABEL "Gradering"
  FIELD Kampanje         AS LOGICAL     LABEL "Kampanje"
  FIELD KassererNavn     AS CHARACTER   FORMAT "X(30)" LABEL "Kasserernavn"
  FIELD KassererNr       AS DECIMAL     DECIMALS 2 FORMAT ">>>>>>>>>>>>9" LABEL "Kasserernummer" COLUMN-LABEL "KassererNr"
  FIELD Kontrollenhet    AS CHARACTER   FORMAT "x(17)" LABEL "Kontrollenhet"
  FIELD Kontrollkod      AS CHARACTER   FORMAT "x(59)" LABEL "Kontrollkod"
  FIELD Kontrollkodkopia AS CHARACTER   FORMAT "x(59)" LABEL "Kontrollkodkopia"
  FIELD Konvertert       AS LOGICAL     LABEL "Konvertert"
  FIELD KOrdre_Id        AS DECIMAL     DECIMALS 2 FORMAT ">>>>>>>>>>>>9" LABEL "KOrdre id"
  FIELD KortType         AS INTEGER     FORMAT ">9" LABEL "Korttype" COLUMN-LABEL "KortType"
  FIELD KundeKort        AS CHARACTER   FORMAT "X(22)" LABEL "Kundekort"
  FIELD KundeNavn        AS CHARACTER   FORMAT "X(30)" LABEL "Kundenavn"
  FIELD KundeNr          AS DECIMAL     DECIMALS 2 FORMAT ">>>>>>>>>>>>9" LABEL "Kundenummer" COLUMN-LABEL "KundeNr"
  FIELD Logg             AS CHARACTER   FORMAT "X(60)" LABEL "Logg"
  FIELD Makulert         AS INTEGER     FORMAT "9"
  FIELD MedlemNavn       AS CHARACTER   FORMAT "X(30)" LABEL "MedlemNavn"
  FIELD MedlemsKort      AS CHARACTER   FORMAT "X(16)" LABEL "Medlemskort"
  FIELD MedlemsNr        AS DECIMAL     DECIMALS 2 FORMAT ">>>>>>>>>>>>9" LABEL "Medlemsnummer" COLUMN-LABEL "MedlNr"
  FIELD OAv              AS CHARACTER   FORMAT "X(15)" LABEL "Opprettet av" COLUMN-LABEL "OAv"
  FIELD ODato            AS DATE        LABEL "OpprettetDato" COLUMN-LABEL "ODato"
  FIELD OpdKvit          AS LOGICAL
  FIELD OpdUtskKopi      AS LOGICAL     LABEL "Utskriftskopi"
  FIELD OrgBongNr        AS INTEGER     FORMAT ">>>>>>>9" LABEL "Salg Bongnummer" COLUMN-LABEL "BongNr"
  FIELD OrgButikkNr      AS INTEGER     FORMAT ">>>>>9" LABEL "Salg butikk" COLUMN-LABEL "SButnr"
  FIELD OrgSelgerNr      AS INTEGER     FORMAT ">>>>>>>>>>>>9" LABEL "Selgernummer" COLUMN-LABEL "Selger"
  FIELD OTid             AS INTEGER     LABEL "Opprettet tid" COLUMN-LABEL "OTid"
  FIELD OverforButikkNr  AS INTEGER     FORMAT ">>>>>9" LABEL "Til Butikk" COLUMN-LABEL ""
  FIELD OverforingsNr    AS DECIMAL     DECIMALS 2 FORMAT ">>>>>>>9" LABEL "Overføringsnummer" COLUMN-LABEL "OverfNr"
  FIELD pfFlagg          AS INTEGER     FORMAT ">9" INITIAL 1 LABEL "Overført ProfitBase"
  FIELD SelgerNavn       AS CHARACTER   FORMAT "X(30)" LABEL "Selgernavn"
  FIELD SelgerNr         AS INTEGER     FORMAT ">>>>>>>>>>>>9" LABEL "Selgernummer" COLUMN-LABEL "Selger"
  FIELD SkiftId          AS DECIMAL     DECIMALS 0 FORMAT "->>>>>>>>>>>>9"
  FIELD SkiftNr          AS INTEGER     FORMAT ">>>>>9" LABEL "Skiftnr"
  FIELD subrab%          AS DECIMAL     DECIMALS 2 LABEL "Subrab%"
  FIELD SubrabKr         AS DECIMAL     DECIMALS 2 FORMAT "->>>,>>9.99" LABEL "Subrabkr"
  FIELD Systemkort       AS CHARACTER   FORMAT "X(30)" LABEL "Systemkort"
  FIELD Tid              AS INTEGER     LABEL "Tid"
  FIELD TTId             AS INTEGER     FORMAT ">>>9" LABEL "Transaksjonstype" COLUMN-LABEL "TTId"
  FIELD UtskriftsKopi    AS CHARACTER   FORMAT "X(60)" LABEL "Utskriftskopi"
  FIELD Is_Changed       AS LOG         
  INDEX idxBong AS UNIQUE ButikkNr GruppeNr KasseNr Dato BongNr
  INDEX idxB_Id AS PRIMARY UNIQUE B_Id
  .  
  
  
DEFINE TEMP-TABLE ttPOSBongLinje NO-UNDO SERIALIZE-NAME 'BongLinje'
  FIELD AaaaMmDd               AS CHARACTER   FORMAT "X(8)" LABEL "ÅrMndDag"
  FIELD AlternativPrisRabatt   AS DECIMAL     DECIMALS 2 FORMAT "->,>>>,>>9.99"
  FIELD Antall                 AS DECIMAL     DECIMALS 3 FORMAT "->>>,>>9.999" LABEL "Antall" COLUMN-LABEL "Antall"
  FIELD ArtikkelNr             AS CHARACTER   FORMAT "X(20)" LABEL "Artikkelnummer" COLUMN-LABEL "ArtikkelNr"
  FIELD BongNr                 AS INTEGER     FORMAT ">>>>>>>>>>>>9" LABEL "Bongnummer" COLUMN-LABEL "BongNr"
  FIELD BongPris               AS DECIMAL     DECIMALS 2 FORMAT "->>,>>>,>>9.99" LABEL "BongPris" COLUMN-LABEL "BongPris"
  FIELD BongTekst              AS CHARACTER   FORMAT "X(30)" LABEL "Bongtekst" COLUMN-LABEL "Bongtekst"
  FIELD ButikkNr               AS INTEGER     FORMAT ">>>>>9" LABEL "Butikknummer" COLUMN-LABEL "ButNr"
  FIELD b_id                   AS DECIMAL     DECIMALS 2 FORMAT ">>>>>>>>>>>9" LABEL "BongId"
  FIELD Dato                   AS DATE        LABEL "Dato"
  FIELD EAv                    AS CHARACTER   FORMAT "X(15)" LABEL "Endret av" COLUMN-LABEL "EAv"
  FIELD EDato                  AS DATE        LABEL "Endret dato" COLUMN-LABEL "EDato"
  FIELD egavekort              AS LOGICAL     LABEL "Elektroniskt gavekort"
  FIELD egavekortPAN           AS CHARACTER   FORMAT "x(16)" LABEL "PAN"
  FIELD ETid                   AS INTEGER     LABEL "Endret tid" COLUMN-LABEL "ETid"
  FIELD FeilKode               AS INTEGER     FORMAT ">9" LABEL "Feilkode" COLUMN-LABEL "FK"
  FIELD FeilKodeTekst          AS CHARACTER   FORMAT "X(30)" LABEL "Feilkodetekst"
  FIELD ForKonvertering        AS CHARACTER   FORMAT "X(40)"
  FIELD GenerellRabatt         AS DECIMAL     DECIMALS 2 FORMAT "->,>>>,>>9.99" LABEL "Generell rabatt"
  FIELD GruppeNr               AS INTEGER     FORMAT ">9" LABEL "Gruppenummer" COLUMN-LABEL "GrNr"
  FIELD HovedGr                AS INTEGER     FORMAT ">>>9" LABEL "Hovedgruppe" COLUMN-LABEL "Hg"
  FIELD HovedGrBeskrivelse     AS CHARACTER   FORMAT "X(30)" LABEL "Hovedgruppe"
  FIELD KampanjeId             AS INTEGER     FORMAT ">>>>>>>9" LABEL "Kampanjeid"
  FIELD KampEierId             AS INTEGER     FORMAT ">>>>>9" LABEL "Kampanjeeier"
  FIELD KampId                 AS DECIMAL     DECIMALS 2 FORMAT ">>>>>>>9" LABEL "Kampanjeid"
  FIELD KampTilbId             AS INTEGER     FORMAT ">>>>>9" LABEL "Kampanjetilbud"
  FIELD KasseNr                AS INTEGER     FORMAT ">>9" LABEL "Kassenummer" COLUMN-LABEL "KasseNr"
  FIELD Kunderabatt            AS DECIMAL     DECIMALS 2 FORMAT "->,>>>,>>9.99" LABEL "Kunderabatt"
  FIELD LevNavn                AS CHARACTER   FORMAT "X(30)" LABEL "Leverandørnavn"
  FIELD LevNr                  AS INTEGER     FORMAT ">>>>>9" LABEL "Leverandørnummer"
  FIELD LinjeNr                AS INTEGER     FORMAT ">>>>9" LABEL "Linjenummer" COLUMN-LABEL "LinjeNr"
  FIELD LinjeRab               AS DECIMAL     DECIMALS 2 FORMAT "->,>>>,>>9.99" LABEL "Linjerabatt"
  FIELD LinjerabattPersonal    AS DECIMAL     DECIMALS 2 FORMAT "->,>>>,>>9.99"
  FIELD LinjeSum               AS DECIMAL     DECIMALS 2 FORMAT "->,>>>,>>>,>>9.99" LABEL "Linjesum" COLUMN-LABEL "LinSum"
  FIELD Linklinjenr            AS INTEGER     FORMAT ">>>>>>9" LABEL "Linjenr Link"
  FIELD LopeNr                 AS INTEGER     FORMAT ">>>9" LABEL "Løpenummer" COLUMN-LABEL "LøpeNr"
  FIELD Makulert               AS LOGICAL     LABEL "Makulert" COLUMN-LABEL "Mak"
  FIELD ManuelEndretPrisRabatt AS DECIMAL     DECIMALS 2 FORMAT "->,>>>,>>9.99"
  FIELD MButikkNr              AS INTEGER     FORMAT ">>>>>9" LABEL "Mottagende butikk" COLUMN-LABEL "MButNr"
  FIELD Medlemsrabatt          AS DECIMAL     DECIMALS 2 FORMAT "->,>>>,>>9.99" LABEL "Medlemsrabatt"
  FIELD MixMatchRabatt         AS DECIMAL     DECIMALS 2 FORMAT "->,>>>,>>9.99"
  FIELD Mva%                   AS DECIMAL     DECIMALS 2 LABEL "MVA%"
  FIELD MvaGr                  AS INTEGER     FORMAT ">9" LABEL "Mva gruppe" COLUMN-LABEL "Mva"
  FIELD MvaGruppeNavn          AS CHARACTER   FORMAT "X(30)" LABEL "Navn" COLUMN-LABEL "Navn"
  FIELD MvaKr                  AS DECIMAL     DECIMALS 2 FORMAT "->>,>>>,>>9.99" LABEL "MvaKr" COLUMN-LABEL "MvaKr"
  FIELD non_sale               AS LOGICAL
  FIELD Normalpris             AS DECIMAL     DECIMALS 2 FORMAT "->>>,>>9.99"
  FIELD NotatKode              AS INTEGER     FORMAT ">9" LABEL "Notatkode" COLUMN-LABEL "NK"
  FIELD NotatKodeTekst         AS CHARACTER   FORMAT "X(30)" LABEL "Notat"
  FIELD OAv                    AS CHARACTER   FORMAT "X(15)" LABEL "Opprettet av" COLUMN-LABEL "OAv"
  FIELD ODato                  AS DATE        LABEL "OpprettetDato" COLUMN-LABEL "ODato"
  FIELD OrgVareGr              AS INTEGER     FORMAT ">>>>>9" LABEL "Varegruppe" COLUMN-LABEL "Vg"
  FIELD OriginalData           AS CHARACTER   FORMAT "X(60)" LABEL "OriginalData"
  FIELD OTid                   AS INTEGER     LABEL "Opprettet tid" COLUMN-LABEL "OTid"
  FIELD Pakkenr                AS INTEGER     LABEL "Pakkenr"
  FIELD pappgavekort           AS LOGICAL     LABEL "Elektroniskt gavekort"
  FIELD pappgavekortREF        AS CHARACTER   FORMAT "x(16)" LABEL "PAN"
  FIELD Personalrabatt         AS DECIMAL     DECIMALS 2 FORMAT "->,>>>,>>9.99" LABEL "Personalrabatt"
  FIELD PrisPrSalgsenhet       AS DECIMAL     DECIMALS 2 FORMAT "->,>>>,>>>,>>9.99" LABEL "Pris pr. salgsenhet" COLUMN-LABEL "Pris pr. s.e."
  FIELD ProduktType            AS INTEGER     FORMAT "9" INITIAL 1 LABEL "Produkttype"
  FIELD RefNr                  AS INTEGER     LABEL "ReferanseNr" COLUMN-LABEL "RefNr"
  FIELD RefTekst               AS CHARACTER   FORMAT "X(40)" LABEL "Referansetekst" COLUMN-LABEL "Ref.tekst"
  FIELD ReturButikk            AS INTEGER     FORMAT ">>>>>9" LABEL "ReturButikk"
  FIELD ReturKasserer          AS DECIMAL     DECIMALS 2 FORMAT ">>>>>>>>>>>>9" LABEL "ReturKasserer" COLUMN-LABEL "ReturKasserer"
  FIELD ReturKassererNavn      AS CHARACTER   FORMAT "X(30)" LABEL "ReturKasserernavn"
  FIELD SalgsType              AS LOGICAL     LABEL "Salgstype"
  FIELD SeqNr                  AS INTEGER     FORMAT ">9" LABEL "SeqNr"
  FIELD SkiftNr                AS INTEGER     FORMAT ">>>>>9" LABEL "Skiftnr"
  FIELD Storrelse              AS CHARACTER   FORMAT "X(4)" LABEL "Størrelse" COLUMN-LABEL "Str"
  FIELD Strekkode              AS CHARACTER   FORMAT "X(20)" LABEL "Strekkode" COLUMN-LABEL "Kode"
  FIELD SubtotalRab            AS DECIMAL     DECIMALS 2 FORMAT "->,>>>,>>9.99" LABEL "Subtotalrabatt" COLUMN-LABEL "SubRab"
  FIELD SubtotalrabattPersonal AS DECIMAL     DECIMALS 2 FORMAT "->,>>>,>>9.99"
  FIELD TBId                   AS INTEGER     FORMAT "z9" LABEL "Transaksjonstype beskrivelse"
  FIELD Tilbudsrabatt          AS DECIMAL     DECIMALS 2 FORMAT "->,>>>,>>9.99"
  FIELD TransDato              AS DATE        LABEL "Transaksjonsdato" COLUMN-LABEL "TDato"
  FIELD TransNr                AS INTEGER     FORMAT "->>,>>>,>>9" LABEL "TransNr"
  FIELD TransTid               AS INTEGER     LABEL "TransaksjonsTid" COLUMN-LABEL "TTid"
  FIELD TTId                   AS INTEGER     FORMAT ">>>9" LABEL "Transaksjonstype" COLUMN-LABEL "TTId"
  FIELD Type                   AS INTEGER     FORMAT "9"
  FIELD VareGr                 AS INTEGER     FORMAT ">>>>>9" LABEL "Varegruppe" COLUMN-LABEL "Vg"
  FIELD VareGruppeNavn         AS CHARACTER   FORMAT "X(30)" LABEL "Varegruppenavn" COLUMN-LABEL "VgNavn"
  FIELD VVarekost              AS DECIMAL     DECIMALS 2 FORMAT "->,>>>,>>9.99" LABEL "VVarekost"
  INDEX idxBongLinje AS UNIQUE ButikkNr GruppeNr KasseNr Dato BongNr LinjeNr
  INDEX idxB_IdLinje AS PRIMARY UNIQUE B_Id LinjeNr
  .
  
  
