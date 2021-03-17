 /* Genererer registreringsunderlag for kombinasjon Varebeh.bok / butikk 
   Parametere:  Varebehnr,butikknr,artikkelnr
   
   Opprettet: 20.09.04 av BHa          
   Endret:    14.09.05 av BHa
            - Ny regel for strekkodehåndtering: Ikke ta hensyn til intern/ekstern kode 
   Endret:    18.01.06 av BHa
              - Kan også kalles for en artikkel i vareh.bok (brukes i DisplayRecord i varebehhode.w)    
              19.03.06 av BHa
              - Lev.dato kontrolleres av første mulige leveringsuke, messe                        
              03.05.07 av BHa
              - Legger inn strKode på VareBehLinjeTrans
              - Legger artikkelnr + string(strkode,"999") i kode feltet
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR hQuery                 AS HANDLE NO-UNDO.
DEF VAR hBuffVarebehLinjeThode AS HANDLE NO-UNDO.
DEF VAR hBuffVarebehLinjeTrans AS HANDLE NO-UNDO.
DEF VAR hBuffVarebehThodeLinje AS HANDLE NO-UNDO.
DEF VAR hBuffVarebehLinje      AS HANDLE NO-UNDO.
DEF VAR oiWeek1                AS INT    NO-UNDO.
DEF VAR oiWeek2                AS INT    NO-UNDO.
DEF VAR oiWeek3                AS INT    NO-UNDO.
DEF VAR oiWeek4                AS INT    NO-UNDO.
DEF VAR ix                     AS INT    NO-UNDO.
DEF VAR bOK                    AS LOG    NO-UNDO.
DEF VAR iAntInndeling          AS INT    NO-UNDO.
DEF VAR iAntFord               AS INT    NO-UNDO.
DEF VAR cSortStrList           AS CHAR   NO-UNDO.
DEF VAR cSortFordList          AS CHAR   NO-UNDO.
DEF VAR cQueryString           AS CHAR   NO-UNDO.
DEF VAR fVarebehNr             AS DEC    NO-UNDO.
DEF VAR fArtikkelNr            AS DEC    NO-UNDO.
DEF VAR iButikkNr              AS INT    NO-UNDO.
DEF VAR dStartMesse            AS DATE   NO-UNDO.
DEF VAR dLevDato1              AS DATE   NO-UNDO.
DEF VAR dLevDato2              AS DATE   NO-UNDO.
DEF VAR dLevDato3              AS DATE   NO-UNDO.
DEF VAR dLevDato4              AS DATE   NO-UNDO.

FIND FIRST VarebehHode NO-LOCK
     WHERE VarebehHode.VarebehNr = DEC(ENTRY(1,icParam))
     NO-ERROR.
IF AVAIL VarebehHode THEN DO:
  FIND FIRST Messe NO-LOCK
       WHERE Messe.MesseNr = VarebehHode.MesseNr
       NO-ERROR.
  IF AVAIL Messe THEN
    dStartMesse = Messe.FraDato.
END.

IF NUM-ENTRIES(icParam) = 3 THEN DO:
  ASSIGN fVarebehNr  = DEC(ENTRY(1,icParam))
         iButikkNr   = INT(ENTRY(2,icParam))
         fArtikkelNr = DEC(ENTRY(3,icParam))
         .
  IF CAN-FIND(FIRST VarebehLinjeTrans
              WHERE VarebehLinjeTrans.VarebehNr  = fVarebehNr
                AND VarebehLinjeTrans.ArtikkelNr = fArtikkelNr
                AND VarebehLinjeTrans.ButikkNr   = iButikkNr) THEN DO:
    obOk = TRUE.
    RETURN.
  END.
  ELSE
  cQueryString = "FOR EACH VarebehLinje NO-LOCK WHERE VarebehNr = " + ENTRY(1,icParam) + " AND ArtikkelNr = " + ENTRY(3,icParam).
END.
ELSE cQueryString = "FOR EACH VarebehLinje NO-LOCK WHERE VarebehNr = " + ENTRY(1,icParam).

CREATE BUFFER hBuffVarebehLinjeThode FOR TABLE "VarebehLinjeThode".
CREATE BUFFER hBuffVarebehLinjeTrans FOR TABLE "VarebehLinjeTrans".
CREATE BUFFER hBuffVarebehLinje      FOR TABLE "VarebehLinje".

CREATE QUERY hQuery.
hQuery:SET-BUFFERS(hBuffVarebehLinje).
obOk = hQuery:QUERY-PREPARE(cQueryString) NO-ERROR.
IF NOT obOk THEN DO:
  ocReturn = "Denne spørringen blir ikke godtatt: " + CHR(10) + cQueryString.
  RETURN.
END.

bOK = hBuffVarebehLinjeThode:FIND-FIRST("WHERE VarebehNr = " + ENTRY(1,icParam)
                                      + "  AND ButikkNr  = " + ENTRY(2,icParam),NO-LOCK) NO-ERROR.
IF NOT bOK THEN DO TRANSACTION:
  hBuffVarebehLinjeThode:BUFFER-CREATE().
  ASSIGN hBuffVarebehLinjeThode:BUFFER-FIELD("VarebehNr"):BUFFER-VALUE = DEC(ENTRY(1,icParam))
         hBuffVarebehLinjeThode:BUFFER-FIELD("ButikkNr"):BUFFER-VALUE = DEC(ENTRY(2,icParam))
         .
END.

hQuery:QUERY-OPEN().
hQuery:GET-FIRST().

REPEAT WHILE NOT hQuery:QUERY-OFF-END:
  FIND FIRST ArtBas 
       WHERE ArtBas.ArtikkelNr = DEC(hBuffVarebehLinje:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE)
       NO-LOCK NO-ERROR.
  IF NOT AVAIL ArtBas THEN DO:
    ocReturn = "Finner ikke artikkel " + hBuffVarebehLinje:BUFFER-FIELD("ArtikkelNr"):STRING-VALUE.
    RETURN.
  END.

  IF dStartMesse NE ? THEN
    ASSIGN dLevDato1 = (IF artbas.levdato1 NE ? THEN (IF artbas.levdato1 > dStartMesse THEN artbas.levdato1 ELSE dStartMesse) ELSE ?)
           dLevDato2 = (IF artbas.levdato2 NE ? THEN (IF artbas.levdato2 > dStartMesse THEN artbas.levdato2 ELSE dStartMesse) ELSE ?)
           dLevDato3 = (IF artbas.levdato3 NE ? THEN (IF artbas.levdato3 > dStartMesse THEN artbas.levdato3 ELSE dStartMesse) ELSE ?)
           dLevDato4 = (IF artbas.levdato4 NE ? THEN (IF artbas.levdato4 > dStartMesse THEN artbas.levdato4 ELSE dStartMesse) ELSE ?)
           .
  ELSE
    ASSIGN dLevDato1 = artbas.levdato1
           dLevDato2 = artbas.levdato2
           dLevDato3 = artbas.levdato3
           dLevDato4 = artbas.levdato4
           .

  IF dLevDato1 NE ? THEN RUN weeknum.p (dLevDato1,OUTPUT oiWeek1).
  ELSE oiWeek1 = 0.
  IF dLevDato2 NE ? THEN RUN weeknum.p (dLevDato2,OUTPUT oiWeek2).
  ELSE oiWeek2 = 0.
  IF dLevDato3 NE ? THEN RUN weeknum.p (dLevDato3,OUTPUT oiWeek3).
  ELSE oiWeek3 = 0.
  IF dLevDato4 NE ? THEN RUN weeknum.p (dLevDato4,OUTPUT oiWeek4).
  ELSE oiWeek4 = 0.

  FIND FIRST StrType OF ArtBas NO-LOCK NO-ERROR.
  
  IF AVAIL StrType THEN DO:

    /* Inndelinger: */
    iAntInndeling = 0.
    FOR EACH ArtSort NO-LOCK
        WHERE ArtSort.ArtikkelNr = ArtBas.ArtikkelNr
        TRANSACTION:
      
      IF NOT CAN-FIND(VarebehLinjeTrans WHERE 
                      VareBehLinjeTrans.VareBehNr  = DEC(ENTRY(1,icParam)) AND 
                      VareBehLinjeTrans.ButikkNr   = INT(ENTRY(2,icParam)) AND 
                      VareBehLinjeTrans.Kode       = ArtSort.SortId AND   
                      VareBehLinjeTrans.ArtikkelNr = hBuffVarebehLinje:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE) THEN 
      DO:
      iAntInndeling = iAntInndeling + 1.
      hBuffVarebehLinjeTrans:BUFFER-CREATE().
      ASSIGN hBuffVarebehLinjeTrans:BUFFER-FIELD("VarebehNr"):BUFFER-VALUE = DEC(ENTRY(1,icParam))
             hBuffVarebehLinjeTrans:BUFFER-FIELD("ButikkNr"):BUFFER-VALUE = DEC(ENTRY(2,icParam))
             hBuffVarebehLinjeTrans:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE = hBuffVarebehLinje:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE
             hBuffVarebehLinjeTrans:BUFFER-FIELD("Kode"):BUFFER-VALUE     = ArtSort.SortId
             hBuffVarebehLinjeTrans:BUFFER-FIELD("SeqNr"):BUFFER-VALUE    = iAntInndeling
             hBuffVarebehLinjeTrans:BUFFER-FIELD("LevDato1"):BUFFER-VALUE = dLevDato1
             hBuffVarebehLinjeTrans:BUFFER-FIELD("LevDato2"):BUFFER-VALUE = dLevDato2
             hBuffVarebehLinjeTrans:BUFFER-FIELD("LevDato3"):BUFFER-VALUE = dLevDato3
             hBuffVarebehLinjeTrans:BUFFER-FIELD("LevDato4"):BUFFER-VALUE = dLevDato4
             hBuffVarebehLinjeTrans:BUFFER-FIELD("Levuke1"):BUFFER-VALUE  = oiWeek1
             hBuffVarebehLinjeTrans:BUFFER-FIELD("Levuke2"):BUFFER-VALUE  = oiWeek2
             hBuffVarebehLinjeTrans:BUFFER-FIELD("Levuke3"):BUFFER-VALUE  = oiWeek3
             hBuffVarebehLinjeTrans:BUFFER-FIELD("Levuke4"):BUFFER-VALUE  = oiWeek4
             .
      END.
    END.

    /* Størrelser: */
    IF NOT (iAntInndeling > 0 AND NOT ArtBas.FrittTillegg) THEN
    
      DO ix = 1 TO NUM-ENTRIES(StrType.fordeling):
        FOR EACH StrekKode OF ArtBas NO-LOCK
            WHERE Strekkode.kode > "" /*
              AND StrekKode.StrKode = INT(ENTRY(ix,StrType.fordeling))
          , FIRST StrKonv OF StrekKode NO-LOCK */
            BREAK BY StrekKode.StrKode:
        
          IF LAST-OF(StrekKode.StrKode) THEN 
          DO TRANSACTION:
           IF NOT CAN-FIND(VarebehLinjeTrans WHERE 
                      VareBehLinjeTrans.VareBehNr  = DEC(ENTRY(1,icParam)) AND 
                      VareBehLinjeTrans.ButikkNr   = INT(ENTRY(2,icParam)) AND 
                      VareBehLinjeTrans.Kode       = STRING(ArtBas.ArtikkelNr) + STRING(StrekKode.StrKode,"999") AND   
                      VareBehLinjeTrans.ArtikkelNr = hBuffVarebehLinje:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE) THEN 
            DO:
            hBuffVarebehLinjeTrans:BUFFER-CREATE().
            ASSIGN hBuffVarebehLinjeTrans:BUFFER-FIELD("VarebehNr"):BUFFER-VALUE  = DEC(ENTRY(1,icParam))
                   hBuffVarebehLinjeTrans:BUFFER-FIELD("ButikkNr"):BUFFER-VALUE   = DEC(ENTRY(2,icParam))
                   hBuffVarebehLinjeTrans:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE = hBuffVarebehLinje:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE
/*                    hBuffVarebehLinjeTrans:BUFFER-FIELD("Kode"):BUFFER-VALUE       = strekkode.kode */
                   hBuffVarebehLinjeTrans:BUFFER-FIELD("Kode"):BUFFER-VALUE       = STRING(ArtBas.ArtikkelNr) + STRING(StrekKode.StrKode,"999")
                   hBuffVarebehLinjeTrans:BUFFER-FIELD("SeqNr"):BUFFER-VALUE      = iAntInndeling + ix
                   hBuffVarebehLinjeTrans:BUFFER-FIELD("LevDato1"):BUFFER-VALUE   = dLevDato1
                   hBuffVarebehLinjeTrans:BUFFER-FIELD("LevDato2"):BUFFER-VALUE   = dLevDato2
                   hBuffVarebehLinjeTrans:BUFFER-FIELD("LevDato3"):BUFFER-VALUE   = dLevDato3
                   hBuffVarebehLinjeTrans:BUFFER-FIELD("LevDato4"):BUFFER-VALUE   = dLevDato4
                   hBuffVarebehLinjeTrans:BUFFER-FIELD("Levuke1"):BUFFER-VALUE    = oiWeek1
                   hBuffVarebehLinjeTrans:BUFFER-FIELD("Levuke2"):BUFFER-VALUE    = oiWeek2
                   hBuffVarebehLinjeTrans:BUFFER-FIELD("Levuke3"):BUFFER-VALUE    = oiWeek3
                   hBuffVarebehLinjeTrans:BUFFER-FIELD("Levuke4"):BUFFER-VALUE    = oiWeek4
                   hBuffVarebehLinjeTrans:BUFFER-FIELD("StrKode"):BUFFER-VALUE    = StrekKode.StrKode
                   .
            END.
          END.
        END.
      END.
  END.

  hQuery:GET-NEXT().
END.

DELETE OBJECT hQuery.
DELETE OBJECT hBuffVarebehLinjeThode.
DELETE OBJECT hBuffVarebehLinjeTrans.
DELETE OBJECT hBuffVarebehLinje.

IF ocReturn = "" THEN obOk = TRUE.

