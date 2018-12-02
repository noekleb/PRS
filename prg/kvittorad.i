/* 25/02/02 workfile definition for table KvittoRad */
/* {1} = "", "NEW" or "NEW SHARED" */
/* {2} = "" or "NO-UNDO" */

DEFINE {1} TEMP-TABLE ttkvittorad {2} /* LIKE KvittoRad */
  FIELD ButikkNr   /*butik_id CHR*/      AS INT 
  FIELD GruppeNr                         AS INT 
  FIELD KasseNr    /*kassaplats CHR*/    AS INT 
  FIELD LinjeNr    /*RadNr CHR */        AS INT 
  FIELD TransDato  /*datum */            AS DATE      
  FIELD TransTid                         AS INT
  FIELD StrekKode  /*ean */              AS CHAR FORMAT "X(20)"
  FIELD ArtikkelNr /*eanKod INT */       AS CHAR FORMAT "x(20)"
  FIELD FeilKode   /*felkod */           AS INTEGER
  FIELD BongNr     /*knr CHR */          AS INT FORMAT "zzzzzzzz9"
  FIELD BongPris   /*kp-rxKvant */       AS DECIMAL   DECIMALS 2
  FIELD LinjeSum   /*kpxKvant */         AS DECIMAL   DECIMALS 2
  FIELD Antall     /*kvantitet */        AS DECIMAL   DECIMALS 2
  FIELD SUMixMatch                       AS LOG
  FIELD kvitto_seq                       AS DEC 
  FIELD mixMatchKvant                    AS INTEGER
  FIELD MixMatchSa                       AS DECIMAL   DECIMALS 2
  FIELD Mva%       /*momsSats */         AS DECIMAL   DECIMALS 2
  FIELD MvaKr                            AS DEC       DECIMALS 2
  FIELD MvaKode                          AS INT
  FIELD pristyp                          AS CHARACTER FORMAT 'X(3)'
  FIELD tid                              AS CHARACTER FORMAT 'X(2)' 
  FIELD LogId                            AS CHAR      FORMAT "x(4)"
  FIELD TTId                             AS INT
  FIELD TBId                             AS INT 
  FIELD Text_1                           AS CHAR      FORMAT "x(20)"
  FIELD Text_2                           AS CHAR      FORMAT "x(20)"
  FIELD Nb1                              AS INT      
  FIELD Nb2                              AS INT
  FIELD Nb3                              AS INT
  FIELD Avrunding                        AS DEC
  FIELD Makulert                         AS LOG       FORMAT "yes/no"
  FIELD OriginalData                     AS CHAR      
  FIELD LinjeRab                         AS DEC
  FIELD SubTotalRab                      AS DEC
  FIELD VareGr                           AS INT 
  FIELD VareGruppeNavn                   AS CHAR
  FIELD MvaGruppeNavn                    AS CHAR
  FIELD HovedGr                          AS INT
  FIELD HovedGrBEskrivelse               AS CHAR
  FIELD LopeNr                           AS INT
  FIELD ForKonvertering                  AS CHAR
  INDEX KvittoRad IS PRIMARY kvitto_seq LinjeNr
  .
