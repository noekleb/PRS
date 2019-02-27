 /* Kompletterer registreringsunderlag for kombinasjon Varebeh.bok, kilde-artikkelnr og liste med nye strekkoder
   Parametere:  Varebehnr,kilde-artikkelnr,pipe-separert liste over nye strekkoder
   
   Opprettet: 25.01.05 av BHa                  
   Endret:    24.08.07 av BHa:
            - Benytter størrelseskode - ikke strekkode
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR ix                     AS INT NO-UNDO.
DEF VAR iy                     AS INT NO-UNDO.
DEF VAR bOK                    AS LOG NO-UNDO.
DEF VAR oiWeek1                AS INT NO-UNDO.
DEF VAR oiWeek2                AS INT NO-UNDO.
DEF VAR oiWeek3                AS INT NO-UNDO.
DEF VAR oiWeek4                AS INT NO-UNDO.
DEF VAR bUse02kode             AS LOG NO-UNDO.

DEF VAR hBuffVarebehLinjeTrans AS HANDLE NO-UNDO.

CREATE BUFFER hBuffVarebehLinjeTrans FOR TABLE "VarebehLinjeTrans".

FIND FIRST VarebehLinje
     WHERE VarebehLinje.VarebehNr  = DEC(ENTRY(1,icParam))
       AND VarebehLinje.ArtikkelNr = DEC(ENTRY(2,icParam))
     NO-LOCK NO-ERROR.
IF NOT AVAIL VarebehLinje THEN DO:
  ocReturn = "Finner ikke varebehandlingslinje. Programmeringsfeil!".
  RETURN.
END.
FIND ArtBas WHERE ArtBas.ArtikkelNr = VarebehLinje.ArtikkelNr NO-LOCK NO-ERROR.
IF AVAIL ArtBas THEN DO TRANSACTION:
  IF artbas.levdato1 NE ? THEN RUN weeknum.p (artbas.levdato1,OUTPUT oiWeek1).
  ELSE oiWeek1 = 0.
  IF artbas.levdato2 NE ? THEN RUN weeknum.p (artbas.levdato2,OUTPUT oiWeek2).
  ELSE oiWeek2 = 0.
  IF artbas.levdato3 NE ? THEN RUN weeknum.p (artbas.levdato3,OUTPUT oiWeek3).
  ELSE oiWeek3 = 0.
  IF artbas.levdato4 NE ? THEN RUN weeknum.p (artbas.levdato4,OUTPUT oiWeek4).
  ELSE oiWeek4 = 0.

  FOR EACH VarebehLinjeThode NO-LOCK
      WHERE VarebehLinjeThode.VarebehNr = VarebehLinje.VarebehNr:

    iy = 0.
    FOR EACH VarebehLinjeTrans NO-LOCK
        WHERE VarebehLinjeTrans.VarebehNr  = VarebehLinjeThode.VarebehNr
          AND VarebehLinjeTrans.ButikkNr   = VarebehLinjeThode.ButikkNr
          AND VarebehLinjeTrans.ArtikkelNr = VarebehLinje.ArtikkelNr
        BY SeqNr:
      iy = SeqNr.
    END.

    IF iy > 0 THEN DO ix = 1 TO NUM-ENTRIES(ENTRY(3,icParam),"|"):
      FIND FIRST VarebehLinjeTrans NO-LOCK
           WHERE VarebehLinjeTrans.VarebehNr  = VarebehLinje.VarebehNr
             AND VarebehLinjeTrans.ButikkNr   = VarebehLinjeThode.ButikkNr
             AND VareBehLinjeTrans.ArtikkelNr = VareBehLinje.ArtikkelNr
             AND VarebehLinjeTrans.StrKode    = INT(ENTRY(ix,ENTRY(3,icParam),"|"))
           NO-ERROR.
      IF NOT AVAIL VarebehLinjeTrans THEN DO:
        hBuffVarebehLinjeTrans:BUFFER-CREATE().
        ASSIGN hBuffVarebehLinjeTrans:BUFFER-FIELD("VarebehNr"):BUFFER-VALUE  = VarebehLinje.VarebehNr
               hBuffVarebehLinjeTrans:BUFFER-FIELD("ButikkNr"):BUFFER-VALUE   = VarebehLinjeThode.ButikkNr
               hBuffVarebehLinjeTrans:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE = VarebehLinje.ArtikkelNr
               hBuffVarebehLinjeTrans:BUFFER-FIELD("StrKode"):BUFFER-VALUE    = INT(ENTRY(ix,ENTRY(3,icParam),"|"))
               hBuffVarebehLinjeTrans:BUFFER-FIELD("Kode"):BUFFER-VALUE       = "000000" + STRING(VarebehLinje.ArtikkelNr) + ENTRY(ix,ENTRY(3,icParam),"|")
               hBuffVarebehLinjeTrans:BUFFER-FIELD("SeqNr"):BUFFER-VALUE      = iy + ix
               hBuffVarebehLinjeTrans:BUFFER-FIELD("LevDato1"):BUFFER-VALUE   = ArtBas.LevDato1
               hBuffVarebehLinjeTrans:BUFFER-FIELD("LevDato2"):BUFFER-VALUE   = ArtBas.LevDato2
               hBuffVarebehLinjeTrans:BUFFER-FIELD("LevDato3"):BUFFER-VALUE   = ArtBas.LevDato3
               hBuffVarebehLinjeTrans:BUFFER-FIELD("LevDato4"):BUFFER-VALUE   = ArtBas.LevDato4
               hBuffVarebehLinjeTrans:BUFFER-FIELD("Levuke1"):BUFFER-VALUE    = oiWeek1
               hBuffVarebehLinjeTrans:BUFFER-FIELD("Levuke2"):BUFFER-VALUE    = oiWeek2
               hBuffVarebehLinjeTrans:BUFFER-FIELD("Levuke3"):BUFFER-VALUE    = oiWeek3
               hBuffVarebehLinjeTrans:BUFFER-FIELD("Levuke4"):BUFFER-VALUE    = oiWeek4
               .
      END.
    END.
  END.
END.

DELETE OBJECT hBuffVarebehLinjeTrans.
IF ocReturn = "" THEN obOk = TRUE.

