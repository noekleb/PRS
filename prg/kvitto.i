/* 25/02/02 workfile definition for table Kvitto */
/* {1} = "", "NEW" or "NEW SHARED" */
/* {2} = "" or "NO-UNDO" */

DEFINE {1} TEMP-TABLE ttkvitto {2} /* LIKE Kvitto */
  FIELD aterkop    AS CHARACTER FORMAT "X(8)"
  FIELD ButikkNr /*butik_id CHR */      AS INT FORMAT ">>>>>9"
  FIELD Dato       /* datum */          AS DATE      
  FIELD fnpTotal   AS DECIMAL   DECIMALS 2
  FIELD ipTotal       AS DECIMAL   DECIMALS 2
  FIELD KasseNr    /*kassaplats CHR */  AS INT 
  FIELD KassererNr /*kassor CHR */      AS DEC FORMAT ">>>>>>>>>>>>9"
  FIELD BongNr     /*knr CHR */         AS INT FORMAT ">>>>>>>9"
  FIELD kvitto_seq    AS DEC 
  FIELD moms1         AS DECIMAL   DECIMALS 2
  FIELD moms2         AS DECIMAL   DECIMALS 2
  FIELD moms3         AS DECIMAL   DECIMALS 2
  FIELD moms4         AS DECIMAL   DECIMALS 2
  FIELD momsTot       AS DECIMAL   DECIMALS 2
  FIELD momsx         AS DECIMAL   DECIMALS 2
  FIELD subTotal      AS DECIMAL   DECIMALS 2
  FIELD Tid           AS INT 
  FIELD totalExkl     AS DECIMAL   DECIMALS 2
  FIELD Belop      /*totalInkl */       AS DECIMAL   DECIMALS 2
  FIELD ExpDate       AS INT       FORMAT "9999"
  FIELD BongStatus    AS INT       FORMAT "9"
  FIELD OpdKvit       AS LOG       FORMAT "yes/no"
  FIELD DataSettId    AS DEC       FORMAT ">>>>>>>>>>>>>9"
  FIELD Utskriftskopi AS LOG       FORMAT "yes/no"
  FIELD GruppeNr      AS INT       FORMAT ">9"
  FIELD KundeNr       AS DEC       FORMAT ">>>>>>>>>>>>9"
  FIELD KundeNavn     AS CHAR      FORMAT "x(30)"
  FIELD MedlemsNr     AS DEC       FORMAT ">>>>>>>>>>>>9"
  FIELD MedlemNavn    AS CHAR      FORMAT "x(30)"
  FIELD KundeKort     AS CHAR      FORMAT "x(26)"
  FIELD MedlemsKort   AS CHAR      FORMAT "x(26)"
  FIELD KassererNav   AS CHAR      FORMAT "x(30)"
  FIELD KortType      AS INT       FORMAT "9"
  FIELD Avrunding     AS DEC       DECIMALS 2
  FIELD RabKr         AS DEC       DECIMALS 2
  FIELD Rab%          AS DEC       DECIMALS 2
  FIELD BruttoBelop   AS DEC       DECIMALS 2
  FIELD RabattGitt    AS LOG
  FIELD flBetalingskort AS LOG 
  FIELD flBankkort      AS LOG
  FIELD flKreditKort    AS LOG
  FIELD flGavekort      AS LOG
  FIELD flSjekk         AS LOG
  FIELD flRekvisisasjon AS LOG 
  FIELD flKupong1       AS LOG
  FIELD flSlKort        AS INT
  INDEX Kvitto IS PRIMARY kvitto_seq  
  INDEX BongHode ButikkNr GruppeNr KasseNr Dato BongNr
  .
