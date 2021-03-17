 /* Kompletterer registreringsunderlag for kombinasjon Varebeh.bok, kilde-artikkelnr og liste med nye artikkelnumre
   Parametere:  Varebehnr,kilde-artikkelnr,pipe-separert liste over nye artikkelnumre
   OBS:         Alle artikler som legges til vil få samme kalkyle som kilde-artikkelnr
   Opprettet: 24.01.05 av BHa                  
      Endret: 27.01.05 av BHa
              - Dersom artikkel i liste over ny artikler er det samme som kilde-artikkel
                gjøres ingen oppretting av artikkel i liste. Dermed kan programmet 
                også benyttes i forbindelse med komplettering av varehåndteringsbok fra varebok
                fordi artikkelen i dette tilfelle allerede er opprettet i vareboklinje_kopiertilvarebeh.p
      Endret: 18.01.06 av BHa
              - Kommentert ut oppretting av registreringslinjer (varebehlinjetrans)
                Dette er ikke lenger nødvendig siden det gjøre underveis i registrering
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

DEF BUFFER orgVarebehLinje FOR VarebehLinje.  /* Kilde artikkelnr i varebeh.linje */

FIND FIRST orgVarebehLinje
     WHERE orgVarebehLinje.VarebehNr  = DEC(ENTRY(1,icParam))
       AND orgVarebehLinje.ArtikkelNr = DEC(ENTRY(2,icParam))
     NO-LOCK NO-ERROR.

IF NOT AVAIL orgVarebehLinje THEN DO:
  ocReturn = "Finner ikke artikkel " + ENTRY(2,icParam) + CHR(10) +
             "i varehåndteringsbok " + ENTRY(1,icParam).
  RETURN.
END.

CREATE BUFFER hBuffVarebehLinjeTrans FOR TABLE "VarebehLinjeTrans".

VarebehLinjer:
DO TRANSACTION:
  DO ix = 1 TO NUM-ENTRIES(ENTRY(3,icParam),"|"):
    FIND ArtBas WHERE ArtBas.ArtikkelNr = DEC(ENTRY(ix,ENTRY(3,icParam),"|")) NO-LOCK NO-ERROR.
    IF NOT AVAIL ArtBas THEN DO:
      ocReturn = "Finner ikke artikkel " + ENTRY(ix,ENTRY(3,icParam),"|") + " i artikkelregister".
      UNDO, LEAVE VarebehLinjer.
    END.
    FIND FIRST VarebehLinje
         WHERE VarebehLinje.VarebehNr  = DEC(ENTRY(1,icParam))
           AND VarebehLinje.ArtikkelNr = DEC(ENTRY(ix,ENTRY(3,icParam),"|"))
         NO-LOCK NO-ERROR.
    IF NOT AVAIL VarebehLinje AND DEC(ENTRY(ix,ENTRY(3,icParam),"|")) NE DEC(ENTRY(2,icParam)) THEN DO:
      CREATE VarebehLinje.
      BUFFER-COPY orgVarebehLinje EXCEPT ArtikkelNr TO VarebehLinje.
      ASSIGN VarebehLinje.ArtikkelNr = ArtBas.Artikkelnr
             VarebehLinje.Beskr      = ArtBas.Beskr
             VarebehLinje.LevFargKod = ArtBas.LevFargKod
             VarebehLinje.LevKod     = ArtBas.LevKod
             .
    END.
    ELSE IF DEC(ENTRY(ix,ENTRY(3,icParam),"|")) NE DEC(ENTRY(2,icParam)) THEN DO:
      ocReturn = "Ugyldig bruk av program " + PROGRAM-NAME(1) + CHR(10) +
                 "Dersom artikkel eksisterer fra før i vareh.bok må artikkel det kopieres fra være den samme".
      UNDO, LEAVE VarebehLinjer.
    END.

    /*
    IF artbas.levdato1 NE ? THEN RUN weeknum.p (artbas.levdato1,OUTPUT oiWeek1).
    ELSE oiWeek1 = 0.
    IF artbas.levdato2 NE ? THEN RUN weeknum.p (artbas.levdato2,OUTPUT oiWeek2).
    ELSE oiWeek2 = 0.
    IF artbas.levdato3 NE ? THEN RUN weeknum.p (artbas.levdato3,OUTPUT oiWeek3).
    ELSE oiWeek3 = 0.
    IF artbas.levdato4 NE ? THEN RUN weeknum.p (artbas.levdato4,OUTPUT oiWeek4).
    ELSE oiWeek4 = 0.

    bUse02kode = NOT CAN-FIND(FIRST StrekKode OF ArtBas WHERE NOT StrekKode.Kode BEGINS "02").

    FIND FIRST StrType OF ArtBas NO-LOCK NO-ERROR.

    IF AVAIL StrType THEN 
      FOR EACH VarebehLinjeThode NO-LOCK
          WHERE VarebehLinjeThode.VarebehNr = orgVarebehLinje.VarebehNr:

        DO iy = 1 TO NUM-ENTRIES(StrType.fordeling):
          FOR EACH StrekKode OF ArtBas NO-LOCK
             WHERE Strekkode.kode > ""
               AND StrekKode.StrKode = INT(ENTRY(iy,StrType.fordeling))
           , FIRST StrKonv OF StrekKode NO-LOCK
             BREAK BY StrekKode.StrKode:

            IF NOT bUse02kode AND StrekKode.kode BEGINS "02" THEN NEXT.

            IF LAST-OF(StrekKode.StrKode) THEN DO:
              hBuffVarebehLinjeTrans:BUFFER-CREATE().
              ASSIGN hBuffVarebehLinjeTrans:BUFFER-FIELD("VarebehNr"):BUFFER-VALUE  = DEC(ENTRY(1,icParam))
                     hBuffVarebehLinjeTrans:BUFFER-FIELD("ButikkNr"):BUFFER-VALUE   = VarebehLinjeThode.ButikkNr
                     hBuffVarebehLinjeTrans:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE = VarebehLinje.ArtikkelNr
                     hBuffVarebehLinjeTrans:BUFFER-FIELD("Kode"):BUFFER-VALUE       = strekkode.kode
                     hBuffVarebehLinjeTrans:BUFFER-FIELD("SeqNr"):BUFFER-VALUE      = iy
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
    */
  END.
END.

DELETE OBJECT hBuffVarebehLinjeTrans.
IF ocReturn = "" THEN obOk = TRUE.
 

