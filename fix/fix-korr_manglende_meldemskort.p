/* fix-korr_medlemsbong.p */
                                                                  
CURRENT-WINDOW:WIDTH = 250.

DEF VAR dDato      AS DATE NO-UNDO.
DEF VAR dvVarekost AS DEC  NO-UNDO.
DEF VAR iBatchNr   AS DEC  NO-UNDO.
DEF VAR wMedlemsNr AS DEC  NO-UNDO.
DEF VAR cLoggFil   AS CHAR NO-UNDO.

cLoggFil = 'MedlemsBonger_M_Feil.csv'.

BUTIKK:
FOR EACH butiker NO-LOCK:
  KASSE:
  FOR EACH kasse NO-LOCK WHERE 
      kasse.butik = butiker.butik:
    DATOLOOP:
      DO dDato = DATE(4,13,2010) TO DATE(9,30,2010):
      BONGHODELOOP:
      FOR EACH bonghode NO-LOCK WHERE 
          bonghode.butikknr = butiker.butik AND
          bonghode.gruppenr = 1 AND
          bonghode.kassenr  = kasse.kassenr AND
          bonghode.dato     = dDato:
          
          IF bongHode.MedlemsNr = 0 THEN
              NEXT BONGHODELOOP.

          /* Gjør oppslag og henter medlemmet hvis medlemskortet finnes. */
          /* Stempler inn medlemmet på bongen.                           */
          FIND FIRST Medlemskort NO-LOCK WHERE
              MedlemsKort.KortNr = BongHode.MedlemsKort NO-ERROR.
          FIND FIRST MEdlem NO-LOCK WHERE
              Medlem.MedlemsNr = BongHode.MedlemsNr NO-ERROR.

          IF NOT AVAILABLE MedlemsKort THEN
          DO TRANSACTION:
            CREATE MedlemsKort.
            ASSIGN
                MedlemsKort.MedlemsNr    = Medlem.MedlemsNr
                MedlemsKort.KortNr       = BongHode.MedlemsKort
                MedlemsKort.AktivertDato = BongHode.Dato
                MedlemsKort.UtgarDato    = BongHode.Dato + 999
                MedlemsKort.Innehaver    = Medlem.ForNavn + ' ' + Medlem.EtterNavn
                MedlemsKort.KortType     = 1
                .

            /*
            DISPLAY
              BongHode.B_Id
              BongHode.MedlemsNr
              BongHode.MedlemsKort
              Medlem.MedlemsNr WHEN AVAILABLE Medlem
              MedlemsKort.KortNr WHEN AVAILABLE MedlemsKort
            WITH WIDTH 250.
            */
          END. /* Transaction */
          
      END. /* BONGHODELOOP */
    END. /* DATOLOOP */
  END. /* KASSE */
END. /* BUTIKK */





