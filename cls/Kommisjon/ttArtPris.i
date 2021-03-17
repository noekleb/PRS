
/*------------------------------------------------------------------------
    File        : ttArtPris.i
    Purpose     : Deling av definisjon.

    Syntax      :

    Description : ArtPris temp tabell.

    Author(s)   : Tom Nøkleby
    Created     : Thu Jan 14 22:49:01 CET 2021
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttArtPris NO-UNDO 
  FIELD AktivFraDato    AS DATE        FORMAT "99/99/9999" LABEL "Aktiv fra"
  FIELD AktivFraTid     AS INTEGER     LABEL "Aktivert tidspunkt"
  FIELD ArtikkelNr      AS DECIMAL     DECIMALS 0 FORMAT "zzzzzzzzzzzz9" LABEL "Artikkelnummer"
  FIELD BrukerID        AS CHARACTER   FORMAT "X(10)" LABEL "Bruker"
  FIELD DB%             AS DECIMAL     EXTENT 2 DECIMALS 2 LABEL "DB%"
  FIELD DBKr            AS DECIMAL     EXTENT 2 DECIMALS 2 LABEL "DB"
  FIELD DivKost%        AS DECIMAL     EXTENT 2 DECIMALS 2 LABEL "Div.kost%"
  FIELD DivKostKr       AS DECIMAL     EXTENT 2 DECIMALS 2 LABEL "Div.kost"
  FIELD EDato           AS DATE        FORMAT "99/99/9999" LABEL "Endret"
  FIELD ETid            AS INTEGER     LABEL "Endret tid" COLUMN-LABEL "ETid"
  FIELD EuroManuel      AS LOGICAL
  FIELD EuroPris        AS DECIMAL     EXTENT 2 DECIMALS 2 FORMAT "->,>>>,>>9.99" LABEL "EuroPris"
  FIELD Frakt           AS DECIMAL     EXTENT 2 DECIMALS 2 LABEL "Frakt"
  FIELD Frakt%          AS DECIMAL     EXTENT 2 DECIMALS 2 LABEL "Frakt%"
  FIELD InnkjopsPris    AS DECIMAL     EXTENT 2 DECIMALS 2 LABEL "Innkjøpspris"
  FIELD LevNr           AS INTEGER     FORMAT "zzzzz9" LABEL "Leverandør" COLUMN-LABEL "LevNr"
  FIELD MengdeRabAntall AS INTEGER     FORMAT ">>>>>9" LABEL "Mengde rab. antall"
  FIELD MengdeRabPris   AS DECIMAL     DECIMALS 2 FORMAT "->,>>>,>>9.99"
  FIELD MomsKod         AS INTEGER     EXTENT 2 FORMAT ">9" LABEL "Momskode"
  FIELD Mva%            AS DECIMAL     EXTENT 2 DECIMALS 2 LABEL "Mva%"
  FIELD MvaKr           AS DECIMAL     EXTENT 2 DECIMALS 2 LABEL "Mva"
  FIELD Pris            AS DECIMAL     EXTENT 2 DECIMALS 2 FORMAT "->,>>>,>>9.99" LABEL "Pris"
  FIELD ProfilNr        AS INTEGER     FORMAT ">>>>>>9" LABEL "Prisprofil"
  FIELD Rab1%           AS DECIMAL     EXTENT 2 DECIMALS 2 FORMAT "->>9.99" LABEL "%Rabatt 1"
  FIELD Rab1Kr          AS DECIMAL     EXTENT 2 DECIMALS 2 LABEL "Rabatt 1"
  FIELD Rab2%           AS DECIMAL     EXTENT 2 DECIMALS 2 LABEL "%Rabatt 2"
  FIELD Rab2Kr          AS DECIMAL     EXTENT 2 DECIMALS 2 LABEL "Rabatt 2"
  FIELD Rab3%           AS DECIMAL     EXTENT 2 DECIMALS 2 LABEL "%Rabatt 3"
  FIELD Rab3Kr          AS DECIMAL     EXTENT 2 DECIMALS 2 LABEL "Rabatt 3"
  FIELD RegistrertAv    AS CHARACTER   FORMAT "X(10)" LABEL "Registrert av" COLUMN-LABEL "Reg.Av"
  FIELD RegistrertDato  AS DATE        FORMAT "99/99/9999" LABEL "Registrert dato" COLUMN-LABEL "RDato"
  FIELD RegistrertTid   AS INTEGER     LABEL "Registreringstidspunkt"
  FIELD Tilbud          AS LOGICAL     LABEL "Tilbud"
  FIELD TilbudFraDato   AS DATE        FORMAT "99/99/9999" LABEL "Tilbud fra"
  FIELD TilbudFraTid    AS INTEGER     LABEL "Tilbud fra tidspunkt"
  FIELD TilbudTilDato   AS DATE        FORMAT "99/99/9999" LABEL "Tilbud aktiv til"
  FIELD TilbudTilTid    AS INTEGER     LABEL "Tilbud til tid"
  FIELD TilbudTimeStyrt AS LOGICAL     LABEL "TimeStyrt"
  FIELD ValPris         AS DECIMAL     EXTENT 2 DECIMALS 2 FORMAT "->>>,>>>,>>9.99" LABEL "Valutapris"
  FIELD VareKost        AS DECIMAL     EXTENT 2 DECIMALS 2 LABEL "VareKost"
  INDEX idxArtPris AS PRIMARY UNIQUE ArtikkelNr ProfilNr
  .


/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
