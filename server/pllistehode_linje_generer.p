/* Leser translogg for angitte butikker og oppretter plukkliste
   Opprettet: 31.03.2007 Tom Nøkleby
 -----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR   NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR   NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR   NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG    NO-UNDO.

DEF VAR iAntall     AS INT  NO-UNDO.
DEF VAR cButikkLst  AS CHAR NO-UNDO.
DEF VAR dFraDato    AS DATE NO-UNDO.
DEF VAR dTilDato    AS DATE NO-UNDO.
DEF VAR cAvdlingLst AS CHAR NO-UNDO.
DEF VAR cHuvGrLst   AS CHAR NO-UNDO.
DEF VAR cVarGrLst   AS CHAR NO-UNDO.
DEF VAR iLoop       AS INT  NO-UNDO.
DEF VAR iButikkLoop AS INT  NO-UNDO.
DEF VAR iVarGrLoop  AS INT  NO-UNDO.
DEF VAR lPlListeId  AS DEC  NO-UNDO.
DEF VAR iCL         AS INT  NO-UNDO.
DEF VAR lSumAnt     AS DEC  NO-UNDO.

DEF BUFFER bufPlListeLinje FOR plListeLinje.

/* Sentrallager */
{syspara.i 5 1 1 iCL INT}

ASSIGN
    cButikkLst  = ENTRY(1,icParam,"|") 
    dFraDato    = date(ENTRY(2,icParam,"|"))
    dTilDato    = date(ENTRY(3,icParam,"|"))
    cAvdlingLst = ENTRY(4,icParam,"|")
    cHuvGrLst   = ENTRY(5,icParam,"|")
    cVarGrLst   = ENTRY(6,icParam,"|")
    obOk        = TRUE
    .

/* Temp-Tabell som holder på lagerantall på sentrallager. */
DEF TEMP-TABLE tmpArtLag
    FIELD ArtikkelNr LIKE ArtBas.ArtikkelNr
    FIELD StrKode    LIKE Strekkode.StrKode
    FIELD Antall     AS DEC FORMAT "->>,>>9.999"
    INDEX Lager ArtikkelNr StrKode.

/* Temp-Tabell som holder på antall solgt pr. artikkel, størrelse og butikk */
DEF TEMP-TABLE tmpPlukk
    FIELD ArtikkelNr   LIKE Artlag.ArtikkelNr
    FIELD StrKode      LIKE ArtLag.StrKode
    FIELD ButikkNr     AS INT FORMAT ">>>>>9"
    FIELD PrioPlukket  AS INT FORMAT ">>>9"
    FIELD Antall       AS DEC FORMAT "->>,>>9.999"
    INDEX Plukk PrioPlukket ButikkNr ArtikkelNr StrKode.

/*Validate*******************************/
DO iLoop = 1 TO NUM-ENTRIES(cButikkLst):
  IF NOT CAN-FIND(Butiker WHERE Butiker.Butik = INT(ENTRY(iLoop,cButikkLst))) THEN
  DO:
    ASSIGN
      ocReturn = ocReturn + (IF NOT ocReturn = '' THEN '\n' ELSE '') + 'FEIL: Kan ikke generere plukkliste, ugyldig butikknr. ' + ENTRY(iLoop,cButikkLst)
      obOk     = FALSE
    .
  END.
END.
IF cAvdlingLst <> "" THEN
DO iLoop = 1 TO NUM-ENTRIES(cAvdlingLst):
  IF NOT CAN-FIND(Avdeling WHERE Avdeling.AvdelingNr = INT(ENTRY(iLoop,cAvdlingLst))) THEN
  DO:
    ASSIGN
      ocReturn = ocReturn + (IF NOT ocReturn = '' THEN '\n' ELSE '') + 'FEIL: Kan ikke generere plukkliste, ugyldig avdelingsnr. ' + ENTRY(iLoop,cAvdlingLst)
      obOk     = FALSE
    .
  END.
END.
IF cHuvGrLst <> "" THEN
DO iLoop = 1 TO NUM-ENTRIES(cHuvGrLst):
  IF NOT CAN-FIND(HuvGr WHERE HuvGr.Hg = INT(ENTRY(iLoop,cHuvGrLst))) THEN
  DO:
    ASSIGN
      ocReturn = ocReturn + (IF NOT ocReturn = '' THEN '\n' ELSE '') + 'FEIL: Kan ikke generere plukkliste, ugyldig hovedgruppe. ' + ENTRY(iLoop,cHuvGrLst)
      obOk     = FALSE
    .
  END.
END.
IF cVarGrLst <> "" THEN
DO iLoop = 1 TO NUM-ENTRIES(cVarGrLst):
  IF NOT CAN-FIND(VarGr WHERE VarGr.Vg = INT(ENTRY(iLoop,cVarGrLst))) THEN
  DO:
    ASSIGN
      ocReturn = ocReturn + (IF NOT ocReturn = '' THEN '\n' ELSE '') + 'FEIL: Kan ikke generere plukkliste, ugyldig varegruppe. ' + ENTRY(iLoop,cVarGrLst)
      obOk     = FALSE
    .
  END.
END.
IF NOT obOk THEN RETURN.
/***********************************Validate*/

/**************** Pakker om avdeling og hovedgruppe til varegruppe ********* */
/* Avdeling valgt. Ikke hoved og varegrupper. */
IF cHuvGrLst = "" AND cAvdlingLst <> "" THEN
DO iLoop = 1 TO NUM-ENTRIES(cAvdlingLst):
    FOR EACH HuvGr NO-LOCK WHERE
        HuvGr.AvdelingNr = INT(ENTRY(iLoop,cAvdlingLst)):
        ASSIGN
          cHuvGrLst = cHuvGrLst + 
                      (IF cHuvGrLst = "" THEN "" ELSE ",") + 
                      STRING(HuvGr.Hg).
    END.
END.
/* Hovedgrupper valgt, men ikke varegrupper */
IF cVarGrLst = "" AND cHuvGrLst <> "" THEN
DO iLoop = 1 TO NUM-ENTRIES(cHuvGrLst):
    FOR EACH VarGr NO-LOCK WHERE
        VarGr.Hg = INT(ENTRY(iLoop,cHuvGrLst)):
        ASSIGN
          cVarGrLst = cVarGrLst + 
                      (IF cVarGrLst = "" THEN "" ELSE ",") + 
                      STRING(VarGr.Vg).
    END.
END.
/* Varegruppe fortsatt blank */
IF cVarGrLst = "" THEN
  FOR EACH VarGr NO-LOCK:
      ASSIGN
        cVarGrLst = cVarGrLst + 
                    (IF cVarGrLst = "" THEN "" ELSE ",") + 
                    STRING(VarGr.Vg).
  END.
/*************** Ompakking ferdig ******************************************/

/* Nullstiller */
obOk = FALSE.

/* Leser postene fra Translogg */
BUTIKKLOOP:
DO iButikkLoop = 1 TO NUM-ENTRIES(cButikkLst):
  /* Det skal ikke plukkes/sendes til sentralalger. */
  IF iCL = INT(entry(iButikkLoop,cButikkLSt)) THEN
      NEXT BUTIKKLOOP.

  /* Det skal opprettes en plukklste pr. butikk. */
  IF AVAILABLE PlListeHode THEN
      RELEASE PlListeHode.
  ASSIGN
      lPlListeId = 0
      lSumAnt    = 0.

  /* Tømmer for hver butikk */
  FOR EACH tmpPlukk:
    DELETE tmpPlukk.
  END.

  /* Går igjennom varegruppene */
  VAREGRUPPELOOP:
  DO iVarGrLoop = 1 TO NUM-ENTRIES(cVarGrLst):
    FIND Butiker NO-LOCK WHERE
        Butiker.Butik = int(entry(iButikkLoop,cButikkLSt)) NO-ERROR.

    /* Leser alle Translogg postene for hver dag og butikk. */
    TRANSLOGG:
    FOR EACH TransLogg EXCLUSIVE-LOCK WHERE
      TransLogg.Plukket = FALSE AND
      TransLogg.Butik   = int(entry(iButikkLoop,cButikkLSt)) AND
      TransLogg.Vg      = int(entry(iVarGrLoop,cVarGrLst)) AND
      TransLogg.Dato   >= dFraDato AND
      TransLogg.Dato   <= dTilDato AND
      TransLogg.Antall > 0:

      /* Bare Varesalg, reklam, og retur skal leses. */
      IF NOT CAN-DO("1,3,10",STRING(Translogg.TTId)) THEN
          NEXT TRANSLOGG.

      /* Tar kun transaksjoner som det er lagt inn artikkelnummer på */
      IF NOT CAN-FIND(ArtBas WHERE
                      ArtBas.ArtikkelNr = TransLogg.ArtikkelNr) THEN
          NEXT TRANSLOGG.
      /* disse skal ikke med */
      IF TransLogg.ArtikkelNr = 0 THEN
          NEXT TRANSLOGG.
      /* Må være en gyldig størrelse */
      FIND StrKonv NO-LOCK WHERE
          StrKonv.Storl = TransLogg.Storl NO-ERROR.
      IF NOT AVAILABLE StrKonv THEN
          NEXT TRANSLOGG.

      /* Logger at det er funnet transaksjoner. */
      IF obOk = FALSE THEN obOk = TRUE.

      ASSIGN
          iAntall = iAntall + 1
          .

      /* Oppretter plukklistehode. */
      IF (NOT AVAILABLE PlListeHode OR lPlListeId = 0) THEN
      OPPSTANDELSEN:
      DO:
          FIND LAST PlListeHode NO-LOCK USE-INDEX PlListeHode NO-ERROR.
          IF AVAILABLE PlListeHode THEN
              lPlListeId  = PlListeHode.PlListeId + 1.
          ELSE lPlListeId = 1.
          /* Er nummerserie full, får vi lete etter hull */
          IF lPlListeId > 99999999 THEN
          LOOPEN:
          DO lPlListeId = 1 TO 99999999:
            IF NOT CAN-FIND(PlListeHode WHERE
                            PlListeHode.PlListeId = lPlListeId) THEN
                LEAVE LOOPEN.
          END. /* LOOPEN */
          /* Oppretter listehode. */
          CREATE PlListeHode.
          ASSIGN
              PlListeHode.PlListeId   = lPlListeId
              PlListeHode.FraButikkNr = iCL
              PlListeHode.TilButikkNr = TransLogg.Butik
              PlListeHode.DatoPlukket = ?
              PlListeHode.TidPlukket  = 0
              PlListeHode.PrioPlukket = Butiker.PrioPlukket
              PlListeHode.PlNavn      = "Overf. til " + Butiker.ButNamn
              PlListeHode.PlMerknad   = "" /*"Plukkliste til " + Butiker.ButNamn*/
              PlListeHode.PlLType     = 1 /* Plukkliste */
              .
      END. /* OPPSTANDELSEN */

      /* Logger sentrallager antall */
      FIND tmpArtLag WHERE
           tmpArtLag.ArtikkelNr = TransLogg.ArtikkelNr AND
           tmpArtLAg.StrKode    = StrKonv.StrKode NO-ERROR.
      IF NOT AVAILABLE tmpArtLag THEN
      LAGER:
      DO:
          FIND ArtLag NO-LOCK WHERE
              ArtLag.ArtikkelNr = TransLogg.ArtikkelNr AND
              ArtLag.Storl      = TransLogg.Storl AND
              ArtLag.Butik      = iCL NO-ERROR.
          CREATE tmpArtLag.
          ASSIGN
              tmpArtLag.ArtikkelNr = Translogg.ArtikkelNr
              tmpArtLag.StrKode    = StrKonv.StrKode
              tmpArtLag.Antall     = IF AVAILABLE ArtLag
                                       THEN ArtLag.LagAnt
                                       ELSE 0
              .
      END. /* LAGER */

      /* Logger overføringsanmodning så lenge det er noe igjen på lageret. */
      IF tmpArtLag.Antall > 0 THEN
      TREKK_OG_POSTER:
      DO:
          FIND tmpPlukk WHERE
              tmpPlukk.ArtikkelNr = TransLogg.ArtikkelNr AND
              tmpPlukk.ButikkNr   = TransLogg.Butik AND
              tmpPlukk.StrKode    = StrKonv.StrKode NO-ERROR.
          IF NOT AVAILABLE tmpPlukk THEN
          NYOVANMODNING:
          DO:
              CREATE tmpPlukk.
              ASSIGN
                  tmpPlukk.ArtikkelNr  = TransLogg.ArtikkelNr 
                  tmpPlukk.ButikkNr    = TransLogg.Butik 
                  tmpPlukk.StrKode     = StrKonv.StrKode
                  tmpPlukk.PrioPlukket = Butiker.PrioPlukket
                  .
          END. /* NYOVANMODNING */

          /* Akkumulering */
          ASSIGN
              tmpPlukk.Antall = tmpPlukk.Antall   + (IF (tmpArtLag.Antall - TransLogg.Antall) >= 0
                                                      THEN TransLogg.Antall
                                                      ELSE tmpArtLag.Antall)
              lSumAnt         = lSumAnt           + (IF (tmpArtLag.Antall - TransLogg.Antall) >= 0
                                                      THEN TransLogg.Antall
                                                      ELSE tmpArtLag.Antall)
              tmpArtLag.Antall = IF Translogg.Antall < 0 THEN tmpArtLag.Antall 
                                    ELSE tmpArtLag.Antall - (IF (tmpArtLag.Antall - TransLogg.Antall) >= 0
                                                    THEN TransLogg.Antall
                                                    ELSE tmpArtLag.Antall).
      END. /* TREKK_OG_POSTER */

      /* Flagger translogg posten som plukket */
      ASSIGN
        Translogg.Plukket = TRUE.

      /* Dropper transaksjonene for de butikkene man ikke skal ha. */
    END. /* TRANSLOGG */
  END. /* VAREGRUPPELOOP */

  /* Skaper plukklistelinjer. */
  RUN PlukkBehandling (lPlListeId,lSumAnt).
END. /* BUTIKKLOOP */

/* MAIN BLOKK */
ASSIGN 
  ocReturn = 'Antall transaksjoner plukket: ' + STRING(iAntall)
  obOk     = TRUE
.
RETURN ocReturn.

/* Subrutine som oppretter plikkliste linjene som er logget. */
PROCEDURE PlukkBehandling:
    DEF INPUT PARAMETER plPlListeId AS DEC NO-UNDO.
    DEF INPUT PARAMETER plSumAnt    AS DEC NO-UNDO.
    
    /* Leser plukkanmodning i prioritert rekkefølge. */
    /* Her posteres plikkliste linjene.              */
    PLUKKBEHANDLING:
    FOR EACH tmpPlukk WHERE tmpPlukk.Antall > 0
        BREAK BY tmpPlukk.PrioPlukket
              BY tmpPlukk.ButikkNr
              BY tmpPlukk.ArtikkelNr
              BY tmpPlukk.StrKode:

      /* Må finnes */
      FIND ArtBas NO-LOCK WHERE
          ArtBas.ArtikkelNr = tmpPlukk.ArtikkelNr NO-ERROR.
      IF NOT AVAILABLE ArtBas THEN
          NEXT PLUKKBEHANDLING.
/*                                                             */
/*       /* Den må finnes her. */                              */
/*       FIND tmpArtLag WHERE                                  */
/*           tmpArtLag.ArtikkelNr = tmpPlukk.ArtikkelNr AND    */
/*           tmpArtLag.StrKode    = tmpPlukk.StrKode NO-ERROR. */
/*       IF NOT AVAILABLE tmpArtLag THEN                       */
/*           NEXT PLUKKBEHANDLING.                             */
/*                                                             */
/*       /* Ikke noe mer å plukke fra sentrallageret. */       */
/*       IF tmpArtLag.Antall <= 0 THEN                         */
/*           NEXT PLUKKBEHANDLING.                             */

      /* Skaper plukklistelinjen hvis den ikke finnes fra før */
      FIND PlListeLinje EXCLUSIVE-LOCK WHERE
          PlListeLinje.PlListeId  = plPlListeId AND
          PlListeLinje.ArtikkelNr = tmpPlukk.ArtikkelNr AND
          PlListeLinje.StrKode    = tmpPlukk.StrKode NO-ERROR.
      IF NOT AVAILABLE PlListeLinje THEN
      DO:
          FIND LAST bufPlListeLinje NO-LOCK WHERE
              bufPlListeLinje.PlListeId = plPlListeId USE-INDEX PlListeLinje NO-ERROR.
          CREATE PlListeLinje.
          ASSIGN
              PlListeLinje.PlListeId  = plPlListeId 
              PlListeLinje.PlLinjeNr  = IF AVAILABLE bufPlListeLinje
                                          THEN bufPlListeLinje.PlLinjeNr + 1
                                          ELSE 1
              PlListeLinje.ArtikkelNr = tmpPlukk.ArtikkelNr 
              PlListeLinje.StrKode    = tmpPlukk.StrKode 
              .
      END.

      /* Akkumulerer opp antall på plukke for artikkel, størrelse og butikk */
      IF AVAILABLE PlListeLinje THEN
          ASSIGN
            PlListeLinje.VarGr  = ArtBas.Vg
            plListeLinje.LopNr  = ArtBas.LopNr
            PlListeLinje.Antall = PlListeLinje.Antall + tmpPlukk.Antall.
    END. /* PLUKKBEHANDLING */

    HODESUM:
    DO TRANSACTION:
        FIND PlListeHode EXCLUSIVE-LOCK WHERE
            PlListeHode.PlListeId = plPlListeId NO-ERROR.
        IF AVAILABLE PlListeHode THEN 
        DO:
            ASSIGN
              PlListeHode.Antall = lSumAnt.
            /* Tomme lister fjernes (Linjer tas eventuelt i trigger) */
            IF PlListeHode.Antall = 0 OR
                NOT CAN-FIND(FIRST PlListeLinje OF PlListeHode) THEN
                DELETE PlListeHode.
        END.
    END. /* HODESUM */
END PROCEDURE.
