/* Oppretter PrisKo post dersom det er gjort endring i inn- eller utpris   
   Parametere: Rowid(ArtPris) "|" pris
   Opprettet: 17.11.2009 brynjar@chemistry.no
   Endret:    14.02.2013 av Brynjar
              - Tar imot enten utpris eller innpris som fNyPris
              - Dersom utpris angitt så hentes innpris fra ArtPris og vice versa
              - Dersom priskøpost eksister for dato (dagens dato) så oppdateres denne
                og utpris eller innpris hentes fra priskø-posten
 -----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR   NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR   NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR   NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG    NO-UNDO.

DEF VAR rArtPris    AS ROWID  NO-UNDO.
DEF VAR fNyPris     AS DEC    NO-UNDO.
DEF VAR cUserId     AS CHAR   NO-UNDO.
DEF VAR fPrisExMVA  AS DEC    NO-UNDO.
DEF VAR ix          AS INT    NO-UNDO.
DEF VAR iProfilnr   AS INT    NO-UNDO.
DEF VAR h_PrisKo    AS HANDLE NO-UNDO.

DEF VAR pdDato     AS DATE  NO-UNDO.
DEF VAR piTid      AS INT   NO-UNDO.
DEF VAR pdDato2    AS DATE  NO-UNDO.
DEF VAR piTid2     AS INT   NO-UNDO.
DEFINE VARIABLE pcError     AS CHARACTER NO-UNDO.
DEFINE VARIABLE wSkjerm     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cPrisFelt   AS CHARACTER NO-UNDO.
DEFINE VARIABLE fInnPris    AS DECIMAL   NO-UNDO.
DEFINE VARIABLE fUtPris     AS DECIMAL   NO-UNDO.
DEFINE VARIABLE fVarekost   AS DECIMAL   NO-UNDO.
DEFINE VARIABLE piTidAktiv  AS INT       NO-UNDO.

DEFINE VARIABLE cOptProfilbutik     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iCLOpt AS INTEGER     NO-UNDO.
{syspar2.i 5 1 1 cOptProfilbutik}
 cOptProfilbutik = TRIM(cOptProfilbutik). 


DEFINE BUFFER bufArtPris FOR ArtPris.
DEFINE BUFFER nyArtPris FOR ArtPris.
DEFINE BUFFER bufButiker FOR butiker.
ASSIGN rArtPris  = TO-ROWID(ENTRY(1,icParam,"|"))
       fNyPris   = DECIMAL(ENTRY(2,icParam,"|"))
       iProfilnr = INTEGER(ENTRY(3,icParam,"|"))
       cUserId   = DYNAMIC-FUNCTION("getASuserId" IN SOURCE-PROCEDURE)
       NO-ERROR.
       
IF NUM-ENTRIES(icParam,"|") > 3 THEN
  cPrisFelt = ENTRY(4,icParam,"|").
ELSE cPrisFelt = "PrisKoPris".

ASSIGN
    pdDato = TODAY 
    piTid  = TIME 
    pdDato2 = ?
    piTid2  = 0
    .
       
FIND bufArtPris NO-LOCK
     WHERE ROWID(bufArtPris) = rArtPris
     NO-ERROR.
/*Vedlikehold jobber alltid med ArtPris for HK. Derfor må artpris for prisprofil hentes. */       
IF AVAILABLE bufArtPris AND bufArtPris.ProfilNr <> iProfilNr THEN 
DO:
  /* Artpris for profilen. */
  FIND ArtPris NO-LOCK WHERE
    ArtPris.ArtikkelNr = bufArtPris.ArtikkelNr AND
    ArtPris.ProfilNr   = iProfilNr NO-ERROR.
  IF NOT AVAIL ArtPris AND cOptProfilbutik <> "" THEN DO:
      /* hitta huvudprofilen för profilen och dess artpris */
      FIND FIRST butiker WHERE butiker.profilnr = iProfilNr NO-LOCK NO-ERROR.
      IF AVAIL butiker AND butiker.sentrallager = FALSE THEN DO:
          FIND bufButiker WHERE bufbutiker.butik = butiker.clButikkNr NO-LOCK NO-ERROR.
          IF AVAIL bufbutiker THEN
              FIND ArtPris NO-LOCK WHERE
                ArtPris.ArtikkelNr = bufArtPris.ArtikkelNr AND
                ArtPris.ProfilNr   = bufbutiker.ProfilNr NO-ERROR.
          IF AVAIL artpris THEN DO:
              CREATE nyArtPris.
              BUFFER-COPY artpris EXCEPT profilnr TO nyArtPris
                  ASSIGN nyArtPris.profilnr = iProfilnr NO-ERROR.
              IF ERROR-STATUS:ERROR THEN
                  DELETE nyartpris.
              ELSE 
                  rArtPris = ROWID(nyArtPris) .
              RELEASE nyArtpris.
              RELEASE Artpris.
          END.
      END.
  END.
END.
/* Må alikevel benytte hk's artpris. */
IF NOT AVAILABLE ArtPris THEN 
  FIND ArtPris NO-LOCK
    WHERE ROWID(ArtPris) = rArtPris
    NO-ERROR.
 
IF AVAIL ArtPris THEN DO:
  IF cPrisFelt = "PrisKoPris" AND (ArtPris.Pris[1] = fNyPris OR fNyPris = 0) THEN DO:
    obOK = YES.
    RETURN.  
  END.
  ELSE IF cPrisFelt = "PrisKoInnPris" AND (ArtPris.InnkjopsPris[1] = fNyPris OR fNyPris = 0) THEN DO:
    obOK = YES.
    RETURN.  
  END.
  
  IF cPrisFelt = "PrisKoPris" THEN
    ASSIGN fInnPris  = ArtPris.InnkjopsPris[1]
           fUtPris   = fNyPris
           fVarekost = ArtPris.VareKost[1]
           .
  ELSE
    IF cPrisFelt = "PrisKoInnPris" THEN
      ASSIGN fUtPris  = ArtPris.Pris[1]
             fInnPris = fNyPris
             fVarekost = fInnPris - fInnPris * ArtPris.Rab1%[1] / 100
             .
  ELSE DO:
    ocReturn = "Feil angivelse av parameter: " + cPrisFelt + " i " + PROGRAM-NAME(1).
    RETURN.
  END.

  FOR EACH PrisKo NO-LOCK
      WHERE PrisKo.ArtikkelNr      = ArtPris.ArtikkelNr
        AND PrisKo.ProfilNr        = iProfilNr
        AND NOT PrisKo.Aktivert
      :
    ix = ix + 1.
    IF PrisKo.AktiveresDato = TODAY THEN DO:
      piTidAktiv = PrisKo.AktiveresTid.
      IF cPrisFelt = "PrisKoPris" THEN
        ASSIGN fInnPris  = PrisKo.InnkjopsPris
               fVarekost = PrisKo.VareKost.
      ELSE
        ASSIGN fUtPris   = PrisKo.Pris
               fVarekost = fInnPris - fInnPris * PrisKo.Rab1% / 100.
    END.
  END.
  IF ix > 1 THEN DO:
    IF cPrisFelt = "PrisKoPris" THEN
      ocReturn = "Det fins allerede mer enn en aktiv priskøpost for artikkelen." + CHR(10)
               + "Redigering må gjøres i priskø".
    ELSE
      ocReturn = "Det fins allerede mer enn en aktiv priskøpost for artikkelen." + CHR(10)
               + "For å redigere innpris kan det bare finnes en aktiv priskøpost".
    RETURN.
  END.

  FIND ArtBas OF ArtPris NO-LOCK NO-ERROR. 
  FIND LevBas NO-LOCK WHERE
    LevBas.levnr = ArtBas.LevNr NO-ERROR.
  IF AVAILABLE LevBas THEN 
    FIND Valuta OF LevBas NO-LOCK NO-ERROR. 

  /* Starter bibliotek for håndtering av priskøoppdateringer. */
  IF NOT VALID-HANDLE(h_PrisKo) THEN
    RUN prisko.p PERSISTENT SET h_PrisKo.

  /* Sjekker om ny post kan opprettes */
  RUN SjekkNyPrisKo IN h_PrisKo (ArtBas.ArtikkelNr,
                                 iProfilNr,
                                 pdDato,
                                 piTid,
                                 pdDato2,
                                 piTid2,
                                 ?,
                                 FALSE,
                                 1,
                                 OUTPUT pcError).
  /* Feil som krever avbrudd for tilbud og normalpris. */
  IF CAN-DO("1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19," + 
            "50,51,52,53,54,55,56,57",ENTRY(1,pcError,CHR(1))) THEN
  DO:
    ocReturn = ENTRY(2,pcError,CHR(1)).
    RETURN.
  END.

  /* Bygger skjermstreng. */
  RUN InitKalkyle IN h_PrisKo
      (INPUT RECID(ArtBas), 
       INPUT iProfilNr,
       INPUT-OUTPUT wSkjerm,
       INPUT ArtPris.Mva%[1],
       INPUT Valuta.ValKurs, 
       INPUT 1,
       INPUT FALSE).
       
  /* Legger endring inn i felt 18 i skjermstreng. */     
  ASSIGN
    wSkjerm  =  STRING(ArtPris.ValPris[1]) + ";" +
                string(fInnPris) + ";" +
/*                 string(ArtPris.InnkjopsPris[1]) + ";" + */
                string(ArtPris.Rab1Kr[1]) + ";" +
                string(ArtPris.Rab1%[1]) + ";" +
                string(ArtPris.Rab2Kr[1]) + ";" +
                string(ArtPris.Rab2%[1]) + ";" +
                string(ArtPris.Frakt[1]) + ";" +
                string(ArtPris.Frakt%[1]) + ";" +
                string(ArtPris.DivKostKr[1]) + ";" +
                string(ArtPris.DivKost%[1]) + ";" +
                string(ArtPris.Rab3Kr[1]) + ";" +
                string(ArtPris.Rab3%[1]) + ";" +
                string(fVareKost) + ";" +
/*                 string(ArtPris.VareKost[1]) + ";" + */
                string(ArtPris.MvaKr[1]) + ";" +
                string(ArtPris.Mva%[1]) + ";" +
                string(ArtPris.DBKr[1]) + ";" +
                string(ArtPris.DB%[1]) + ";" +
                string(fUtPris) + ";" +
/*                 string(fNyPris) + ";" + */
                string(ArtPris.EuroPris[1]) + ";" +
                "no" + ";" + 
                STRING(TODAY) + ";" +
                STRING(TIME) + ";;0;;0;;0;no"
                .
  /* Starter omkalkulering.                         */
  /* NB: Kalkulasjonen skjer i prosedyrebilboteket. */
  RUN Omregning IN h_PrisKo
      (INPUT RECID(ArtBas), 
       INPUT iProfilNr,
       INPUT-OUTPUT wSkjerm,
       INPUT ArtPris.Mva%[1],
       INPUT Valuta.ValKurs, 
       INPUT 18,
       FALSE).
  ASSIGN
    wSkjerm = wSkjerm +
              ";no" + ";" + 
              STRING(TODAY) + ";" +
              (IF piTidAktiv NE 0 THEN STRING(piTidAktiv) ELSE STRING(TIME)) 
            + ";;0;;0;;0;no".

  /* Lagrer prisendring i priskø. */
  RUN NyPrisKo IN h_PrisKo 
      (
       INPUT RECID(ArtBas), 
       INPUT iProfilNr,
       INPUT-OUTPUT wSkjerm,
       FALSE,
       1
      ).
         
  /* Starter bibliotek for håndtering av priskøoppdateringer. */
  IF VALID-HANDLE(h_PrisKo) THEN
    DELETE PROCEDURE h_PrisKo.

END.
ELSE ocReturn = "Finner ikke gjeldende artpris. Programfeil i " + PROGRAM-NAME(1).

obOK = ocReturn = "".
