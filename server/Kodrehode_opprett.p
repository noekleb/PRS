/* Kodrehode_opprett.p
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEFINE VARIABLE lKOrdre_Id AS DECIMAL  NO-UNDO.

DEFINE TEMP-TABLE KOrdreHode
  FIELD Adresse1             AS CHARACTER FORMAT "X(40)" LABEL "Adresse"
  FIELD Adresse2             AS CHARACTER FORMAT "X(40)" LABEL "Adresse"
  FIELD AnsvVerksted         AS CHARACTER FORMAT "X(8)" LABEL "Ansv. versted"
  FIELD AntApnet             AS INTEGER   FORMAT ">9" LABEL "Ant. åpnet"
  FIELD AntDager             AS INTEGER   FORMAT ">>9" LABEL "Antall betalingsfrie dager" COLUMN-LABEL "AntDg"
  FIELD AntKolli             AS INTEGER   FORMAT ">9" LABEL "Antall kolli"
  FIELD AntPPEti             AS INTEGER   FORMAT ">9" LABEL "Antall etiketter"
  FIELD AvdelingNr           AS INTEGER   FORMAT ">>>9" LABEL "Avdelingsnr" COLUMN-LABEL "AvdNr"
  FIELD AvgFriSalg           AS DECIMAL   DECIMALS 2 FORMAT "->>,>>>,>>9.99" LABEL "Avg. fritt salg" COLUMN-LABEL "AvgFriSalg"
  FIELD AvgPlSalg            AS DECIMAL   DECIMALS 2 FORMAT "->>,>>>,>>9.99" LABEL "Avg. pliktig salg" COLUMN-LABEL "AvgPlSalg"
  FIELD Avrund               AS DECIMAL   DECIMALS 2 LABEL "Avrundet"
  FIELD AvrundingKr          AS INTEGER   LABEL "Avrunding"
  FIELD AvrundingType        AS INTEGER   FORMAT ">9" LABEL "Avrundingstype"
  FIELD BetaltDato           AS DATE      LABEL "Betalt"
  FIELD BetBet               AS INTEGER   FORMAT ">>9" LABEL "Betalings.bet"
  FIELD BetTekst             AS CHARACTER FORMAT "X(30)" LABEL "Betalingsbetingelse"
  FIELD BrukerID             AS CHARACTER FORMAT "X(10)" LABEL "Bruker"
  FIELD Bruttovekt           AS DECIMAL   DECIMALS 2 FORMAT "->>,>>9.999" LABEL "Bruttovekt"
  FIELD Butik                AS INTEGER   FORMAT ">>>>>9" LABEL "Butikknummer" COLUMN-LABEL "ButNr"
  FIELD ButikkNr             AS INTEGER   FORMAT ">>>>>9" LABEL "Butikk"
  FIELD cOpt1                AS CHARACTER
  FIELD Dato                 AS DATE      LABEL "Fakturadato"
  FIELD DatoTidEndret        AS DATETIME  FORMAT "99/99/9999 HH:MM:SS.SSS" INITIAL ? LABEL "Endret"
  FIELD DatoTidOpprettet     AS DATETIME  FORMAT "99/99/9999 HH:MM:SS.SSS" INITIAL ?
  FIELD DeresRef             AS CHARACTER FORMAT "X(30)" LABEL "Deres referanse"
  FIELD dOpt1                AS DECIMAL   DECIMALS 2
  FIELD EDato                AS DATE      FORMAT "99/99/9999" LABEL "Endret"
  FIELD EkstOrdreNr          AS CHARACTER FORMAT "X(15)" LABEL "Ekst.ordrenr"
  FIELD Embalage             AS CHARACTER FORMAT "X(30)" LABEL "Embalage/godsslag"
  FIELD ePostAdresse         AS CHARACTER FORMAT "X(40)" LABEL "E-Post adresse" COLUMN-LABEL "E-Post"
  FIELD ETid                 AS INTEGER   LABEL "Endret tid" COLUMN-LABEL "ETid"
  FIELD FaktAdresse1         AS CHARACTER FORMAT "X(30)" LABEL "Fakt.adresse"
  FIELD FaktAdresse2         AS CHARACTER FORMAT "X(30)" LABEL "Fakt.Adresse"
  FIELD FaktLand             AS CHARACTER FORMAT "X(30)" LABEL "Land"
  FIELD FaktPostNr           AS CHARACTER FORMAT "X(15)" LABEL "Postnr"
  FIELD FaktPoststed         AS CHARACTER FORMAT "X(8)" LABEL "Fakt. Poststed"
  FIELD FaktTekstNr          AS INTEGER   FORMAT ">>9" LABEL "FakturatekstNr"
  FIELD Faktura_Id           AS DECIMAL   DECIMALS 2 FORMAT ">>>>>>>>>>>>9" LABEL "FakturaId"
  FIELD FakturertAv          AS CHARACTER FORMAT "X(20)" LABEL "Fakturert av"
  FIELD FakturertDato        AS DATE      LABEL "Fakturert dato"
  FIELD FakturertTid         AS INTEGER   LABEL "Fakturert tid"
  FIELD FirmaAdresse1        AS CHARACTER FORMAT "X(30)" LABEL "Firmaadresse"
  FIELD FirmaAdresse2        AS CHARACTER FORMAT "X(30)" LABEL "Firmaadresse"
  FIELD FirmaBankKonto       AS CHARACTER FORMAT "X(20)" LABEL "Bankkonto"
  FIELD FirmaEPost           AS CHARACTER FORMAT "X(40)" LABEL "E-Post"
  FIELD FirmaLand            AS CHARACTER FORMAT "X(30)" LABEL "Land"
  FIELD FirmaNavn            AS CHARACTER FORMAT "X(30)" LABEL "Firma"
  FIELD FirmaOrganisasjonsNr AS CHARACTER FORMAT "X(12)" LABEL "OrganisasjonsNr"
  FIELD FirmaPostgiro        AS CHARACTER FORMAT "X(20)" LABEL "Postgiro"
  FIELD FirmaPostNr          AS CHARACTER FORMAT "X(10)" LABEL "PostNr"
  FIELD FirmaPoststed        AS CHARACTER FORMAT "X(30)" LABEL "Poststed"
  FIELD FirmaTelefaks        AS CHARACTER FORMAT "X(15)" LABEL "Telefaks"
  FIELD FirmaTelefon         AS CHARACTER FORMAT "X(15)" LABEL "Telefon"
  FIELD FirmaURLAdresse      AS CHARACTER FORMAT "X(40)" LABEL "URL adresse"
  FIELD ForfallsDato         AS DATE      LABEL "Forfall"
  FIELD ForsNr               AS INTEGER   FORMAT ">>>>>9" LABEL "Kasserer"
  FIELD Fraktbrevtekst       AS CHARACTER FORMAT "X(30)" LABEL "Fraktbrevtekst"
  FIELD Godsmerking          AS CHARACTER FORMAT "X(30)" LABEL "Godsmerking"
  FIELD InternMerknad        AS CHARACTER FORMAT "X(40)" LABEL "Intern merknad"
  FIELD iOpt1                AS INTEGER
  FIELD KasseNr              AS INTEGER   FORMAT ">>9" LABEL "Kassenummer" COLUMN-LABEL "KasseNr"
  FIELD KontNavn             AS CHARACTER FORMAT "X(40)" LABEL "Kontaktperson"
  FIELD KontoNr              AS INTEGER   FORMAT ">>9999" LABEL "Kontonummer" COLUMN-LABEL "Konto"
  FIELD KontTelefon          AS CHARACTER FORMAT "X(15)" LABEL "Telefon"
  FIELD KOrdre_Id            AS DECIMAL   DECIMALS 2 FORMAT ">>>>>>>>>>>>9" LABEL "KOrdre Id" COLUMN-LABEL "KOId"
  FIELD KProsjektNr          AS INTEGER   FORMAT ">>>9" LABEL "Kundeprosjekt"
  FIELD KundeMerknad         AS CHARACTER FORMAT "X(40)" LABEL "Kunde merknad"
  FIELD KundeNr              AS DECIMAL   DECIMALS 2 FORMAT ">>>>>>>>>>>>9" LABEL "Kundenummer" COLUMN-LABEL "KundeNr"
  FIELD Kundeservice         AS LOGICAL   LABEL "Kundeservice"
  FIELD LevAdresse1          AS CHARACTER FORMAT "X(40)" LABEL "Leveringsadresse"
  FIELD LevAdresse2          AS CHARACTER FORMAT "X(40)" LABEL "Leveringsadresse"
  FIELD LeveresDatoTid       AS DATETIME  FORMAT "99/99/9999 HH:MM:SS.SSS" INITIAL ? COLUMN-LABEL "Leverings dato/tid"
  FIELD Leveringsdato        AS DATE      LABEL "Leveringsdato"
  FIELD LevFNr               AS INTEGER   FORMAT ">9" LABEL "Leveringsform"
  FIELD LevLand              AS CHARACTER FORMAT "X(30)" LABEL "Lev. Land"
  FIELD LevPostNr            AS CHARACTER FORMAT "X(10)" LABEL "Lev. PostNr"
  FIELD LevPostSted          AS CHARACTER FORMAT "X(30)" LABEL "Poststed"
  FIELD LevStatus            AS CHARACTER FORMAT "x(2)" LABEL "Lev.status"
  FIELD MobilTlf             AS CHARACTER FORMAT "X(15)" LABEL "Mobiltelefon"
  FIELD Mva                  AS DECIMAL   DECIMALS 2 FORMAT "->>,>>>,>>9.99" LABEL "Mva"
  FIELD MvaKr                AS DECIMAL   DECIMALS 2 FORMAT "->>>,>>9.99" LABEL "Mva"
  FIELD Navn                 AS CHARACTER FORMAT "X(40)" LABEL "Navn"
  FIELD Opphav               AS INTEGER   FORMAT ">9" LABEL "Opphav"
  FIELD PostNr               AS CHARACTER FORMAT "X(10)" LABEL "PostNr"
  FIELD PostSted             AS CHARACTER FORMAT "X(30)" LABEL "Poststed"
  FIELD ProdStatus           AS CHARACTER FORMAT "X" LABEL "Prod.status"
  FIELD ProduksjonsDato      AS DATE      LABEL "Produsert"
  FIELD Referanse            AS CHARACTER FORMAT "X(30)" LABEL "Referanse"
  FIELD RefKOrdre_Id         AS DECIMAL   DECIMALS 2 FORMAT ">>>>>>>>>>>>9" LABEL "KOrdre Id" COLUMN-LABEL "KOId"
  FIELD RegistrertAv         AS CHARACTER FORMAT "X(10)" LABEL "Registrert av" COLUMN-LABEL "Reg.Av"
  FIELD RegistrertDato       AS DATE      FORMAT "99/99/9999" LABEL "Registrert dato" COLUMN-LABEL "RDato"
  FIELD RegistrertTid        AS INTEGER   LABEL "Registreringstidspunkt"
  FIELD ReturNr              AS CHARACTER FORMAT "x(30)" LABEL "Retur nr."
  FIELD SelgerNr             AS DECIMAL   DECIMALS 2 FORMAT ">>>>>>>>>>>>9" LABEL "Selgernummer" COLUMN-LABEL "SelgerNr"
  FIELD SendingsNr           AS CHARACTER FORMAT "X(30)" LABEL "Sendingsnummer"
  FIELD ShipmentSendt        AS DATETIME  FORMAT "99/99/9999 HH:MM:SS.SSS" INITIAL ?
  FIELD SvarFrist            AS DATE      FORMAT "99/99/9999" LABEL "Svarfrist"
  FIELD Telefaks             AS CHARACTER FORMAT "X(15)" LABEL "Telefaks"
  FIELD Telefon              AS CHARACTER FORMAT "X(15)" LABEL "Telefon"
  FIELD TotalRabatt%         AS DECIMAL   DECIMALS 2 FORMAT "->9.99" LABEL "Totalrabatt%"
  FIELD TotalRabattKr        AS DECIMAL   DECIMALS 2 FORMAT "->>>,>>9.99" LABEL "Totalrabatt"
  FIELD Totalt               AS DECIMAL   DECIMALS 2 FORMAT "->>>,>>>,>>9.99" LABEL "Totalt" COLUMN-LABEL "Totalt"
  FIELD TotaltVolum          AS DECIMAL   DECIMALS 2 FORMAT "->>,>>9.999" LABEL "Totalt volum"
  FIELD Utsendelsesdato      AS DATE      LABEL "Utsendelsesdato"
  FIELD VaarRef              AS CHARACTER FORMAT "X(30)" LABEL "Vår referanse"
  FIELD ValKod               AS CHARACTER FORMAT "x(3)" LABEL "ValutaKode"
  FIELD VerkstedMerknad      AS CHARACTER FORMAT "X(80)" LABEL "Merknad (Verksted)"
  FIELD Verkstedordre        AS LOGICAL   LABEL "Verkstedordre"
  FIELD WebKanSendeEMail     AS LOGICAL
  FIELD WebKanSetteOrdre     AS LOGICAL
  FIELD WebKOrdreHode        AS LOGICAL
  FIELD RowIdent1            AS CHARACTER 
  FIELD RowCount             AS INTEGER
  INDEX idxRowids RowIdent1
  .

DEFINE BUFFER bufKOrdreHode FOR KOrdreHode.

/*DEF VAR hQuery        AS HANDLE NO-UNDO.*/
/*CREATE QUERY hQuery.                                                             */
/*hQuery:SET-BUFFERS(ihBuffer).                                                    */
/*hQuery:QUERY-PREPARE("FOR EACH " + ihBuffer:NAME + " BY dDateSent BY iTimeSent").*/
/*hQuery:QUERY-OPEN().                                                             */
/*hQuery:GET-FIRST().                                                              */
/*REPEAT WHILE NOT hQuery:QUERY-OFF-END:                                           */
/*                                                                                 */
/*  /* Customize from here: */                                                     */
/*                                                                                 */
/*/*  ASSIGN dDate      = ihBuffer:BUFFER-FIELD("dDateSent"):BUFFER-VALUE*/        */
/*/*         iTime      = ihBuffer:BUFFER-FIELD("iTimeSent"):BUFFER-VALUE*/        */
/*/*         cFileName  = ihBuffer:BUFFER-FIELD("cFileName"):BUFFER-VALUE*/        */
/*/*         .                                                           */        */
/*                                                                                 */
/*  hQuery:GET-NEXT().                                                             */
/*END.                                                                             */
/*                                                                                 */
/*DELETE OBJECT hQuery.                                                            */

ASSIGN 
  lKOrdre_Id = DEC(ENTRY(1,icParam,'|'))
  .

RUN opprettKOrdreHode.

/* Legger den lokale temp tabellen inn i det delta bufferet. */
ihBuffer:COPY-TEMP-TABLE (BUFFER KOrdreHode:HANDLE,NO,NO,YES).

obOK = YES.
obOk = ocReturn = "".

/* **********************  Internal Procedures  *********************** */

PROCEDURE opprettKOrdreHode:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  FIND bufKOrdreHode NO-LOCK WHERE 
    bufKOrdreHode.KOrdre_Id = lKOrdre_Id NO-ERROR.
  IF AVAILABLE bufKOrdreHode THEN 
    DO:
      CREATE KOrdreHode.
      BUFFER-COPY bufKOrdreHode 
        TO KOrdreHode
        ASSIGN 
          KOrdreHode.RowIdent1 = STRING(ROWID(bufKOrdreHode))
          .
    END.

END PROCEDURE.
