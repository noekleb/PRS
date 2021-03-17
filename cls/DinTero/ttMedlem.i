
/*------------------------------------------------------------------------
    File        : ttMedlem.i
    Purpose     : En include for definering av tabellene.  

    Syntax      :

    Description : Definerer alle tabeller som inngår i datasettet for medlem.

    Author(s)   : Tom Nøkleby
    Created     : Mon Nov 02 15:21:40 CET 2020
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttMedlem SERIALIZE-NAME 'Medlem'
  FIELD Adresse1              AS CHARACTER   FORMAT "X(40)" LABEL "Adresse"
  FIELD Adresse2              AS CHARACTER   FORMAT "X(40)" LABEL "Adresse"
  FIELD Aktiv                 AS LOGICAL     INITIAL TRUE LABEL "Aktiv"
  FIELD AktivertFraWeb        AS DATE        FORMAT "99/99/9999" LABEL "Aktivert fra Web"
  FIELD Bonus_Berettiget      AS LOGICAL     INITIAL TRUE LABEL "Bonus berettiget" COLUMN-LABEL "Bonus"
  FIELD Bonus_Forsendelse     AS INTEGER     FORMAT "9" INITIAL 1 LABEL "Bonus forsendelse"
  FIELD Bonus_varsel          AS INTEGER     FORMAT "9" INITIAL 1
  FIELD BrukerID              AS CHARACTER   FORMAT "X(10)" LABEL "Bruker"
  FIELD ButikkNr              AS INTEGER     FORMAT ">>>>>9" LABEL "Butikk"
  FIELD BydelsNr              AS CHARACTER   FORMAT "X(8)" LABEL "Bydelsnummer" COLUMN-LABEL "BydelsNr"
  FIELD EDato                 AS DATE        FORMAT "99/99/9999" LABEL "Endret"
  FIELD EksterntMedlemsNr     AS CHARACTER   FORMAT "X(20)" LABEL "Ekst.medlemsnr"
  FIELD ePostAdresse          AS CHARACTER   FORMAT "X(40)" LABEL "E-Post adresse" COLUMN-LABEL "E-Post"
  FIELD ETid                  AS INTEGER     LABEL "Endret tid" COLUMN-LABEL "ETid"
  FIELD EtterNavn             AS CHARACTER   FORMAT "X(40)" LABEL "Etternavn"
  FIELD FodselsDato           AS DATE        LABEL "Fødselsdato"
  FIELD FodtAr                AS INTEGER     FORMAT "9999" LABEL "Fødselsår" COLUMN-LABEL ""
  FIELD ForNavn               AS CHARACTER   FORMAT "X(40)" LABEL "Navn"
  FIELD HovedMedlemFlagg      AS LOGICAL     INITIAL TRUE LABEL "Hovedmedlem"
  FIELD HovedMedlemsNr        AS DECIMAL     DECIMALS 2 FORMAT ">>>>>>>>>>>>9" LABEL "Hovedmedlem" COLUMN-LABEL "MedlemNr"
  FIELD Kilde                 AS CHARACTER   FORMAT "X(30)" LABEL "Kilde"
  FIELD Kjonn                 AS LOGICAL     FORMAT "M/K" INITIAL TRUE LABEL "Kj�nn"
  FIELD KundeNr               AS DECIMAL     DECIMALS 2 FORMAT ">>>>>>>>>>>>9" LABEL "Kundenummer" COLUMN-LABEL "KundeNr"
  FIELD Land                  AS CHARACTER   FORMAT "X(30)" LABEL "Land"
  FIELD MedGruppe             AS INTEGER     FORMAT "zzz9" LABEL "Medlemsgruppe"
  FIELD MedlemInfo            AS CHARACTER   FORMAT "X(30)" LABEL "Info"
  FIELD MedlemNotat           AS CHARACTER   FORMAT "X(30)" LABEL "Notat"
  FIELD MedlemsNr             AS DECIMAL     DECIMALS 2 FORMAT ">>>>>>>>>>>>9" LABEL "Medlemsnummer" COLUMN-LABEL "MedlemNr"
  FIELD MedType               AS INTEGER     FORMAT "zzz9" LABEL "Medlemstype"
  FIELD MKlubbId              AS INTEGER     FORMAT ">>9" LABEL "Klubbnr"
  FIELD MobilTlf              AS CHARACTER   FORMAT "X(15)" LABEL "Mobiltelefon"
  FIELD MottaeMailUtsendelser AS LOGICAL     LABEL "Utsendelser"
  FIELD Opphort               AS DATE        LABEL "Opphørt"
  FIELD PersonNr              AS CHARACTER   FORMAT "X(15)" LABEL "Personnr."
  FIELD PostNr                AS CHARACTER   FORMAT "X(10)" LABEL "PostNr"
  FIELD Rabatt                AS DECIMAL     DECIMALS 2 LABEL "Rabatt%"
  FIELD RegistrertAv          AS CHARACTER   FORMAT "X(10)" LABEL "Registrert av" COLUMN-LABEL "Reg.Av"
  FIELD RegistrertDato        AS DATE        FORMAT "99/99/9999" LABEL "Registrert dato" COLUMN-LABEL "RDato"
  FIELD RegistrertTid         AS INTEGER     LABEL "Registreringstidspunkt"
  FIELD RegKode               AS CHARACTER   FORMAT "X(10)" LABEL "Regionskode" COLUMN-LABEL "RegKode"
  FIELD Telefaks              AS CHARACTER   FORMAT "X(15)" LABEL "Telefaks"
  FIELD Telefon               AS CHARACTER   FORMAT "X(15)" LABEL "Telefon"
  FIELD TilgKilde             AS CHARACTER   FORMAT "X(30)" LABEL "Tilg.kilde"
  FIELD WebBrukerId           AS CHARACTER   FORMAT "X(15)" LABEL "Brukerid (Web)"
  FIELD WebPassord            AS CHARACTER   FORMAT "X(15)" LABEL "Passord (Web)"
  INDEX idxMedlem AS PRIMARY UNIQUE MedlemsNr
  .

DEFINE TEMP-TABLE ttMedlemskort NO-UNDO SERIALIZE-NAME 'Medlemskort'
  FIELD MedlemsNr       AS DECIMAL     DECIMALS 2 FORMAT ">>>>>>>>>>>>9" LABEL "Medlemsnummer" COLUMN-LABEL "MedlemsNr"
  FIELD KortNr          AS CHARACTER   FORMAT "X(22)" LABEL "Kortnummer"
  FIELD AktivertDato    AS DATE        LABEL "Aktivert dato"
  FIELD BrukerID        AS CHARACTER   FORMAT "X(10)" LABEL "Bruker"
  FIELD EDato           AS DATE        FORMAT "99/99/9999" LABEL "Endret"
  FIELD ETid            AS INTEGER     LABEL "Endret tid" COLUMN-LABEL "ETid"
  FIELD Innehaver       AS CHARACTER   FORMAT "X(30)" LABEL "Innehaver"
  FIELD InterntKKortId  AS DECIMAL     DECIMALS 2 FORMAT ">>>>>>>>>>>>9" LABEL "Internt kortid"
  FIELD KortType        AS INTEGER     FORMAT "9" INITIAL 1 LABEL "Korttype" COLUMN-LABEL "Korttype"
  FIELD KundeKortNr     AS CHARACTER   FORMAT "X(22)" LABEL "Kundekortnr"
  FIELD KundeRabattKort AS LOGICAL     LABEL "Kunderabattkort" COLUMN-LABEL "AnnenKlubb"
  FIELD Merknad         AS CHARACTER   FORMAT "X(40)" LABEL "Merknad"
  FIELD RegistrertAv    AS CHARACTER   FORMAT "X(10)" LABEL "Registrert av" COLUMN-LABEL "Reg.Av"
  FIELD RegistrertDato  AS DATE        FORMAT "99/99/9999" LABEL "Registrert dato" COLUMN-LABEL "RDato"
  FIELD RegistrertTid   AS INTEGER     LABEL "Registreringstidspunkt"
  FIELD Sperret         AS LOGICAL     LABEL "Sperret"
  FIELD UtgarDato       AS DATE        LABEL "Utgår dato"
  INDEX idxMedlemskort AS PRIMARY UNIQUE MedlemsNr KortNr
  INDEX idxKort AS UNIQUE KortNr
  .

DEFINE TEMP-TABLE ttMedlemsaldo NO-UNDO SERIALIZE-NAME 'Medlemsaldo'
  FIELD MedlemsNr      AS DECIMAL     DECIMALS 2 FORMAT ">>>>>>>>>>>>9" LABEL "Medlemsnummer" COLUMN-LABEL "MedlemsNr"
  FIELD ButikkNr       AS INTEGER     FORMAT ">>>>>9" LABEL "Butikk"
  FIELD BrukerID       AS CHARACTER   FORMAT "X(10)" LABEL "Bruker"
  FIELD DatoSiste      AS DATE        LABEL "Dato siste kjøp" COLUMN-LABEL "Siste kjøp"
  FIELD EDato          AS DATE        FORMAT "99/99/9999" LABEL "Endret"
  FIELD ETid           AS INTEGER     LABEL "Endret tid" COLUMN-LABEL "ETid"
  FIELD ForsteDato     AS DATE        LABEL "Dato første kjøp" COLUMN-LABEL "Første kjøp"
  FIELD ForsteTid      AS INTEGER     LABEL "FørsteTid"
  FIELD RegistrertAv   AS CHARACTER   FORMAT "X(10)" LABEL "Registrert av" COLUMN-LABEL "Reg.Av"
  FIELD RegistrertDato AS DATE        FORMAT "99/99/9999" LABEL "Registrert dato" COLUMN-LABEL "RDato"
  FIELD RegistrertTid  AS INTEGER     LABEL "Registreringstidspunkt"
  FIELD Saldo          AS DECIMAL     DECIMALS 2 FORMAT "->>>,>>>,>>9.99" LABEL "Saldo"
  FIELD SisteTid       AS INTEGER     LABEL "SisteTid"
  FIELD TotaltKjop     AS DECIMAL     DECIMALS 2 FORMAT "->>>,>>>,>>9.99" LABEL "Totalt kjøp"
  INDEX idxMedlemsaldo AS PRIMARY UNIQUE Medlemsnr ButikkNr
  .

DEFINE TEMP-TABLE ttMedlemsgruppe NO-UNDO SERIALIZE-NAME 'Medlemsgruppe'
  FIELD MedGruppe      AS INTEGER     FORMAT "zzz9" LABEL "Medlemsgruppe" COLUMN-LABEL "MGrp"
  FIELD Beskrivelse    AS CHARACTER   FORMAT "X(40)" LABEL "Beskrivelse"
  FIELD BrukerID       AS CHARACTER   FORMAT "X(10)" LABEL "Bruker"
  FIELD EDato          AS DATE        FORMAT "99/99/9999" LABEL "Endret"
  FIELD ETid           AS INTEGER     LABEL "Endret tid" COLUMN-LABEL "ETid"
  FIELD Notat          AS CHARACTER   FORMAT "X(8)" LABEL "Notat"
  FIELD RegistrertAv   AS CHARACTER   FORMAT "X(10)" LABEL "Registrert av" COLUMN-LABEL "Reg.Av"
  FIELD RegistrertDato AS DATE        FORMAT "99/99/9999" LABEL "Registrert dato" COLUMN-LABEL "RDato"
  FIELD RegistrertTid  AS INTEGER     LABEL "Registreringstidspunkt"
  INDEX idxMedlemsgruppe AS PRIMARY UNIQUE MedGruppe
  .

DEFINE TEMP-TABLE ttMedlemsklubb NO-UNDO SERIALIZE-NAME 'Medlemsklubb'
  FIELD MKlubbId          AS INTEGER     FORMAT ">>9" LABEL "Klubbnr"
  FIELD BrukerID          AS CHARACTER   FORMAT "X(10)" LABEL "Bruker"
  FIELD EDato             AS DATE        FORMAT "99/99/9999" LABEL "Endret"
  FIELD ETid              AS INTEGER     LABEL "Endret tid" COLUMN-LABEL "ETid"
  FIELD MKlubbBeskrivelse AS CHARACTER   FORMAT "X(30)" LABEL "Beskrivelse"
  FIELD RegistrertAv      AS CHARACTER   FORMAT "X(10)" LABEL "Registrert av" COLUMN-LABEL "Reg.Av"
  FIELD RegistrertDato    AS DATE        FORMAT "99/99/9999" LABEL "Registrert dato" COLUMN-LABEL "RDato"
  FIELD RegistrertTid     AS INTEGER     LABEL "Registreringstidspunkt"
  INDEX idxMedlemsklubb AS PRIMARY UNIQUE MKlubbId
  .

DEFINE TEMP-TABLE ttMedlemstype NO-UNDO SERIALIZE-NAME 'Medlemstype'
  FIELD MedType        AS INTEGER     FORMAT "zzz9" LABEL "MedlemsType"
  FIELD Beskrivelse    AS CHARACTER   FORMAT "X(40)" LABEL "Beskrivelse"
  FIELD BrukerID       AS CHARACTER   FORMAT "X(10)" LABEL "Bruker"
  FIELD EDato          AS DATE        FORMAT "99/99/9999" LABEL "Endret"
  FIELD ETid           AS INTEGER     LABEL "Endret tid" COLUMN-LABEL "ETid"
  FIELD Notat          AS CHARACTER   FORMAT "X(8)" LABEL "Notat"
  FIELD RegistrertAv   AS CHARACTER   FORMAT "X(10)" LABEL "Registrert av" COLUMN-LABEL "Reg.Av"
  FIELD RegistrertDato AS DATE        FORMAT "99/99/9999" LABEL "Registrert dato" COLUMN-LABEL "RDato"
  FIELD RegistrertTid  AS INTEGER     LABEL "Registreringstidspunkt"
  INDEX idxMedlemstype AS PRIMARY UNIQUE MedType .


/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
