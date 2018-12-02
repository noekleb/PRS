DEFINE TEMP-TABLE ttEtiko SERIALIZE-NAME 'ttEetiko'
    FIELD ean               AS CHARACTER FORMAT "x(14)" LABEL "EAN/PLU-nr" COLUMN-LABEL "EAN/PLU-nr"
    FIELD etitekst1         AS CHARACTER FORMAT "x(40)" LABEL "Tekst 1" COLUMN-LABEL "Tekst 1"
    FIELD enhtekst          AS CHARACTER FORMAT "X(15)" LABEL "Enhetstekst"
    FIELD utpris            AS DECIMAL   DECIMALS 2 FORMAT ">>,>>9.99" LABEL "Utpris" COLUMN-LABEL "Utpris"
    FIELD utprisN           AS DECIMAL   DECIMALS 2 FORMAT ">>,>>9.99" LABEL "UtprisN" COLUMN-LABEL "UtprisN"
    FIELD brukerid          AS CHARACTER FORMAT "x(12)" LABEL "BrukerID" COLUMN-LABEL "BrukerID"
    FIELD stylecode         AS INTEGER   FORMAT ">>9" LABEL "Etikettnr"
    FIELD butnr             AS INTEGER   FORMAT ">>>>9" LABEL "Butnr" COLUMN-LABEL "Butnr"
    FIELD quantity          AS INTEGER   FORMAT ">>9" LABEL "Antall" COLUMN-LABEL "Antall"
    FIELD antpkn            AS DECIMAL   DECIMALS 3 FORMAT ">,>>9" LABEL "Antpkn" COLUMN-LABEL "Antpkn"
    FIELD emb               AS CHARACTER FORMAT "x(3)" LABEL "Emb" COLUMN-LABEL "Emb"
    FIELD hgr               AS INTEGER   FORMAT ">>>9" LABEL "Vgr"
    FIELD sortkode          AS CHARACTER FORMAT "X(3)" LABEL "Sortiment" COLUMN-LABEL "Sortiment"
    FIELD Levnr             AS DECIMAL   
    FIELD bestnr            AS CHARACTER FORMAT "X(20)" LABEL "Bestnr" COLUMN-LABEL "Bestnr"
    FIELD enhpris           AS DECIMAL   DECIMALS 2 FORMAT ">>,>>9.99" LABEL "Enhetspris" COLUMN-LABEL "Enh.pris"
    FIELD pristekst         AS CHARACTER FORMAT "X(20)" LABEL "Pristekst"
    FIELD prisntekst        AS CHARACTER FORMAT "X(20)" LABEL "Prisntekst"
    FIELD levvnr            AS CHARACTER FORMAT "X(30)" LABEL "Levvnr" COLUMN-LABEL "Levvnr"
    FIELD veilpris          AS DECIMAL   DECIMALS 2 FORMAT ">>,>>9.99" LABEL "Veilpris" COLUMN-LABEL "Veilpris"
    FIELD hgrtekst          AS CHARACTER FORMAT "x(30)" LABEL "Varegrtekst" COLUMN-LABEL "Varegrtekst"
    FIELD fabrikatnavn      AS CHARACTER FORMAT "x(30)" LABEL "Fabrikatnavn" COLUMN-LABEL "Fabrikatnavn"
    FIELD fargetekst        AS CHARACTER FORMAT "x(30)" LABEL "Fargetekst" COLUMN-LABEL "Fargetekst"
    FIELD storrtekst        AS CHARACTER FORMAT "x(30)" LABEL "Størrelse" COLUMN-LABEL "Størrelse"
    FIELD levnavn           AS CHARACTER FORMAT "x(30)" LABEL "Levnavn" COLUMN-LABEL "Levnavn"
    FIELD modellnr2         AS CHARACTER FORMAT "X(20)" LABEL "Modellnr2" COLUMN-LABEL "Modellnr2"
    FIELD Rfid1             AS CHARACTER FORMAT "X(20)" LABEL "Rfid1" COLUMN-LABEL "Rfid1"
    FIELD Rfid2             AS CHARACTER FORMAT "X(20)" LABEL "Rfid2" COLUMN-LABEL "Rfid2"
    FIELD Rfid3             AS CHARACTER FORMAT "X(20)" LABEL "Rfid3" COLUMN-LABEL "Rfid3"
    FIELD Rfid4             AS CHARACTER FORMAT "X(20)" LABEL "Rfid4" COLUMN-LABEL "Rfid4"
    FIELD Etitype           AS INTEGER   FORMAT "->>9"  LABEL "Etitype" COLUMN-LABEL "Etiketype"
    FIELD Sortering         AS CHARACTER FORMAT "X(20)" LABEL "Sortering" COLUMN-LABEL "Sortering"
    FIELD OpprettetDatoTid  AS DATETIME 
    FIELD RFIDTcpIp         AS CHARACTER 
    FIELD SekNr             AS INTEGER 
    FIELD RFIDSkrevetStatus AS INTEGER 
    FIELD utskrdato         AS DATE        FORMAT "99-99-99" LABEL "Utskr.dato"
    FIELD Skrivernavn       AS CHARACTER FORMAT "x(30)" LABEL "Skrivernavn" COLUMN-LABEL "Skrivernavn"
    INDEX idxEtiko AS UNIQUE BrukerId Stylecode butnr sortering ean OpprettetDatoTid.
  
DEFINE DATASET dsEtiko FOR ttEtiko.
