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
DEFINE VARIABLE dDato AS DATE NO-UNDO.
DEF VAR cAvdlingLst AS CHAR NO-UNDO.
DEF VAR cHuvGrLst   AS CHAR NO-UNDO.
DEF VAR cVarGrLst   AS CHAR NO-UNDO.
DEF VAR iLoop       AS INT  NO-UNDO.
DEF VAR iButikkLoop AS INT  NO-UNDO.
DEF VAR iVarGrLoop  AS INT  NO-UNDO.
DEF VAR lPlListeId  AS DEC  NO-UNDO.
DEF VAR iCL         AS INT  NO-UNDO.
DEF VAR lSumAnt     AS DEC  NO-UNDO.
DEFINE VARIABLE iArUke1 AS INTEGER NO-UNDO.
DEFINE VARIABLE iArUke2 AS INTEGER NO-UNDO.
DEFINE VARIABLE iUkeDag AS INTEGER NO-UNDO.
DEFINE VARIABLE cMail   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTekst   AS CHARACTER NO-UNDO.
DEFINE VARIABLE bSeparat AS LOG NO-UNDO.
DEFINE VARIABLE iLogPartner   AS INTEGER   NO-UNDO.
DEFINE VARIABLE bGrunnsort    AS LOG NO-UNDO.
DEFINE VARIABLE cLevNrLst AS CHARACTER NO-UNDO.
DEFINE VARIABLE bGrunnsortKjede AS LOG NO-UNDO.
DEFINE VARIABLE cLogg AS CHARACTER NO-UNDO.
DEFINE VARIABLE iType AS INTEGER NO-UNDO.
DEFINE VARIABLE iAntMin AS INTEGER NO-UNDO.
DEFINE VARIABLE iAntSalgsdager AS INTEGER NO-UNDO.

DEF BUFFER bufPlListeLinje FOR plListeLinje.
DEFINE BUFFER bufArtLag FOR ArtLag.

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
    FIELD levNr        AS INTEGER FORMAT ">>>>>9"
    FIELD Kode         AS CHARACTER FORMAT "x(20)"
    INDEX Plukk ButikkNr ArtikkelNr StrKode
    INDEX Antall Antall.

/* Sentrallager */
{syspara.i 5 1 1 iCL INT}

/* Sjekk mot translogg eller lager */
{syspara.i 5 24 8 iType INT}
{syspar2.i 5 24 8 iAntMin INT}
IF iAntMin = 0 THEN 
    iantmin = 2.
{syspara.i 5 24 9 iAntSalgsdager INT}
IF iAntSalgsdager = 0 THEN 
    iAntSalgsdager = 20.    

/* Skal varer som er kjedeleverte legges opp på separate lev.nr? */
{syspara.i 5 24 4 cTekst}
IF CAN-DO('1,J,Ja,Y,Yes,True',cTekst) THEN 
  bSeparat = TRUE.
ELSE
  bSeparat = FALSE.
  
/* Skal det legges opp suppleringsordre på grunnsortimentsvarer. */
{syspara.i 5 24 5 cTekst}
IF CAN-DO('1,J,Ja,Y,Yes,True',cTekst) THEN 
  bGrunnsort = TRUE.
ELSE
  bGrunnsort = FALSE.

/* Skal det gjøres kontroll på kjede og grunnsortiment. */
{syspara.i 5 24 7 cTekst}
IF CAN-DO('1,J,Ja,Y,Yes,True',cTekst) THEN 
  bGrunnsortKjede = TRUE.
ELSE
  bGrunnsortKjede = FALSE. 
    
/* Lev.nr på logistikkpartner eller kjedens sentrallager som suppleringsordre for kjedeleverte varer skal legges på. */
{syspara.i 5 24 3 iLogPartner INT}
IF iLogPartner = 0 THEN iLogPartner = 38.

ASSIGN
    cButikkLst  = ENTRY(1,icParam,"|") 
    dFraDato    = DATE(ENTRY(2,icParam,"|"))
    dTilDato    = DATE(ENTRY(3,icParam,"|"))
    cAvdlingLst = ENTRY(4,icParam,"|")
    cHuvGrLst   = ENTRY(5,icParam,"|")
    cVarGrLst   = ENTRY(6,icParam,"|")
    cLevNrLst   = ENTRY(7,icParam,"|")
    obOk        = TRUE
    cLogg       = 'Suppleringsordre' + REPLACE(STRING(TODAY),'/','')
    .
/* Sjekker om det skal kjøres eMail. */
IF NUM-ENTRIES(icParam,"|") >= 8 THEN 
DO:
  cMail = ENTRY(8,icParam,"|").
  RUN bibl_loggDbFri.p (cLogg,  '  Mail skal sendes (' + cMail + ').').
END.

RUN bibl_loggDbFri.p (cLogg, '  Motatt parametre: ' + icParam).

/* Validate */
RUN validerLister.
RUN bibl_loggDbFri.p (cLogg,  '  Resultat valider lister: ' + STRING(obOk)).
IF NOT obOk THEN 
    RETURN.

/* Pakker om avdeling og hovedgruppe til varegruppe */
RUN ompakkingAvLister.

/* Nullstiller */
obOk = FALSE.

IF iType = 1 THEN 
DO:
    RUN bibl_loggDbFri.p (cLogg,'  lesLager.').
    /* Leser postene fra Translogg og legger opp poster i tmpPlukk. */
    RUN lesLager.
END.
ELSE DO:
    RUN bibl_loggDbFri.p (cLogg,'  lesTranslogg.').
    /* Leser postene fra Translogg og legger opp poster i tmpPlukk. */
    RUN lesTranslogg.
END.

/* Sender mail med suppleringsordreforslag, hvis dette er satt opp. */
IF cButikkLst <> "" AND CAN-DO('1,j.y,true,ja,yes',cMail) THEN
  DO:
    RUN bibl_loggDbFri.p (cLogg, '  Sender eMail til butikkene: ' + cButikkLSt + '.' ).
    RUN generer_send_Pl_mail.p (cButikkLst).
  END.
  
/* MAIN BLOKK */
ASSIGN 
  ocReturn = 'Antall transaksjoner plukket: ' + STRING(iAntall)
  obOk     = TRUE
.

IF itype = 1 THEN 
    RUN bibl_loggDbFri.p (cLogg, '  Antall endrede lagerposter: ' + STRING(iAntall) + ' ' + ocReturn).

ELSE 
    RUN bibl_loggDbFri.p (cLogg, '  Antall plukkede transer: ' + STRING(iAntall) + ' ' + ocReturn).

RETURN ocReturn.

/* ************************  Function Prototypes ********************** */
FUNCTION hentSalg RETURNS DECIMAL 
    ( INPUT plArtikkelNr AS DECIMAL,
      INPUT piSalgsdager AS INTEGER, 
      INPUT piButik AS INTEGER,
      INPUT pcTTidLst AS CHARACTER ) FORWARD.

/* **********************  Internal Procedures  *********************** */


PROCEDURE lesLager:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEFINE VARIABLE piButikkNr AS INTEGER NO-UNDO.

BUTIKKLOOP:
DO iButikkLoop = 1 TO NUM-ENTRIES(cButikkLst):
    IF INT(ENTRY(iButikkLoop,cButikkLst)) = 0 THEN NEXT.
    ASSIGN
        piButikkNr = INT(ENTRY(iButikkLoop,cButikkLst)) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN 
        NEXT.
    
    /* Det skal opprettes en plukklste pr. butikk. */
    IF AVAILABLE PlListeHode THEN
        RELEASE PlListeHode.
    ASSIGN
        lSumAnt    = 0.
    
    RUN bibl_loggDbFri.p (cLogg,'  Sjekker butikk: ' + ENTRY(iButikkLoop,cButikkLSt)).
    
  /* Leser alle lagerposter som er endret siden igår. */
    ARTLAGLOOP:
    FOR EACH ArtLag NO-LOCK WHERE 
        ArtLag.EndretDatoTid >= DATETIME(TODAY - 1) AND 
        ArtLag.Butik = piButikkNr AND 
        ArtLag.lagant <= iAntMin,
        FIRST ArtBas NO-LOCK WHERE 
            ArtBas.ArtikkelNr = ArtLag.ArtikkelNr:
    
        FIND LAST Strekkode NO-LOCK WHERE
            Strekkode.ArtikkelNr = ArtLag.ArtikkelNr AND
            Strekkode.StrKode    = ArtLag.StrKode NO-ERROR.
        IF ArtBas.Pakke THEN NEXT ARTLAGLOOP.
        IF ArtBas.OPris THEN NEXT ARTLAGLOOP.
        IF ArtBas.Pant  THEN NEXT ARTLAGLOOP.

        /* Sjekker om det er avgrensning på varegrupper. */
        IF cVarGrLst <> '' THEN 
          IF NOT CAN-DO(cVarGrLst,STRING(ArtBas.Vg)) THEN 
            NEXT ARTLAGLOOP.

        /* Sjekker om det er avgrensning på leverandører. */
        IF cLevNrLst <> '' THEN
          IF NOT CAN-DO(cLevNrLst,STRING(ArtBas.LevNr)) THEN 
            NEXT ARTLAGLOOP.

        /* Sjekk av sortiment */
        IF bGrunnsortKjede = TRUE THEN 
        DO:
          IF ArtBas.KjedeVare = FALSE AND ArtBas.Grunnsortiment = FALSE THEN 
            NEXT ARTLAGLOOP.
          /* Er ikke flagg for grunnsortiment satt, skal heller ikke disse med. */
          IF bGrunnsort = FALSE AND ArtBas.Kjedevare = FALSE THEN 
            NEXT ARTLAGLOOP.
        END.
        
        /* Logger at det er funnet transaksjoner. */
        IF obOk = FALSE THEN 
          obOk = TRUE.
        
        ASSIGN
            iAntall = iAntall + 1
            .
        
        FIND bufArtLag NO-LOCK WHERE
            bufArtLag.ArtikkelNr = ArtLag.ArtikkelNr AND
            bufArtLag.Storl      = ArtLag.Storl AND
            bufArtLag.Butik      = iCL NO-ERROR.
        /* Logger sentrallager antall */
        FIND tmpArtLag WHERE
             tmpArtLag.ArtikkelNr = ArtLag.ArtikkelNr AND
             tmpArtLAg.StrKode    = ArtLag.StrKode NO-ERROR.
        IF NOT AVAILABLE tmpArtLag THEN
        LAGER:
        DO:
            CREATE tmpArtLag.
            ASSIGN
                tmpArtLag.ArtikkelNr = ArtLag.ArtikkelNr
                tmpArtLag.StrKode    = ArtLag.StrKode
                tmpArtLag.Antall     = IF AVAILABLE bufArtLag
                                         THEN bufArtLag.LagAnt
                                         ELSE 0
                .
        END. /* LAGER */
        ELSE DO:
           ASSIGN
                tmpArtLag.Antall     = tmpArtLag.Antall 
                                       + IF AVAILABLE bufArtLag
                                           THEN bufArtLag.LagAnt
                                           ELSE 0.      
        END. /* LAGER */

        /* Logger ordreforslag. */
        POSTER:
        DO:
            FIND FIRST tmpPlukk WHERE
                tmpPlukk.ButikkNr   = ArtLag.Butik AND
                tmpPlukk.ArtikkelNr = ArtLag.ArtikkelNr AND
                tmpPlukk.StrKode    = ArtLag.StrKode NO-ERROR.
            IF NOT AVAILABLE tmpPlukk THEN
            NYOVANMODNING:
            DO:
                CREATE tmpPlukk.
                ASSIGN
                    tmpPlukk.ButikkNr    = ArtLag.Butik 
                    tmpPlukk.ArtikkelNr  = ArtLag.ArtikkelNr 
                    tmpPlukk.StrKode     = ArtLag.StrKode
                    tmpplukk.levnr       = ArtBas.levnr
                    tmpPlukk.Kode        = (IF AVAILABLE StrekKode THEN StrekKode.Kode ELSE '')
                    .
            END. /* NYOVANMODNING */
        
            /* Akkumulering */
            ASSIGN
                tmpPlukk.Antall  = hentSalg( tmpPlukk.ArtikkelNr,
                                             iAntSalgsdager, 
                                             (IF tmpPlukk.ButikkNr = 16 THEN 15 ELSE tmpPlukk.ButikkNr),
                                             '1,10' )
                lSumAnt          = lSumAnt + ArtLag.LagAnt
                .
        END. /* POSTER */
    END. /* ARTLAGLOOP */

    TEMP-TABLE tmpPlukk:WRITE-JSON ("file", 'konv\tmpPlukkLager' + STRING(piButikkNr) + '.JSon',TRUE).
    TEMP-TABLE tmpArtLag:WRITE-JSON ("file", 'konv\tmpArtLagLager' + STRING(piButikkNr) + '.JSon',TRUE).
  
    /* Skaper plukklistelinjer for hver butikk. */
    RUN PlukkBehandling (lSumAnt,piButikkNr).
  
    /* Tømmer for hver butikk */
    FOR EACH tmpPlukk:
      DELETE tmpPlukk.
    END.
  
    RUN bibl_loggDbFri.p (cLogg,'  Ferdig butikk: ' + ENTRY(iButikkLoop,cButikkLSt)).
END. /* BUTIKKLOOP */

END PROCEDURE.

PROCEDURE lesTransLogg:
/*------------------------------------------------------------------------------
		Purpose:  																	  
		Notes:  																	  
------------------------------------------------------------------------------*/
DEFINE VARIABLE piButikkNr AS INTEGER NO-UNDO.

BUTIKKLOOP:
DO iButikkLoop = 1 TO NUM-ENTRIES(cButikkLst):
  IF INT(ENTRY(iButikkLoop,cButikkLst)) = 0 THEN NEXT.
  ASSIGN
      piButikkNr = INT(ENTRY(iButikkLoop,cButikkLst)) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN 
      NEXT.

  /* Det skal opprettes en plukklste pr. butikk. */
  IF AVAILABLE PlListeHode THEN
      RELEASE PlListeHode.
  ASSIGN
      lSumAnt    = 0.

  RUN bibl_loggDbFri.p (cLogg,'  Leser franslogg for butikk. ' + ENTRY(iButikkLoop,cButikkLSt)).

  DATOLOOP:
  DO dDato = dFraDato TO dTilDato:
    RUN bibl_loggDbFri.p (cLogg,'   Genererer for butikk: Butikk. ' + ENTRY(iButikkLoop,cButikkLSt) + ' Dato: ' + STRING(dDato)).

    TRANSLOGG:
    FOR EACH TransLogg EXCLUSIVE-LOCK WHERE
      TransLogg.Dato  = dDato AND
      TransLogg.TTId  = 1 AND /* Bare varesalg */
      TransLogg.Butik = piButikkNr:

      /* disse skal ikke med */
      IF TransLogg.ArtikkelNr = 0 THEN NEXT TRANSLOGG.

      IF Translogg.OrdreForslag = TRUE THEN NEXT TRANSLOGG.

      /* Flagger translogg posten som plukket */
      ASSIGN
        Translogg.Ordreforslag = TRUE. 
      
      /* Disse transaksjonene skal ikke med. */
      IF TransLogg.Antall <= 0 THEN NEXT TRANSLOGG.      
      
      IF cVarGrLst <> '' THEN 
        IF NOT CAN-DO(cVarGrLst,STRING(TransLogg.Vg)) THEN NEXT TRANSLOGG.

      IF cLevNrLst <> '' THEN
      DO:
        FIND ArtBas NO-LOCK WHERE
          ArtBas.ArtikkelNr = TransLogg.ArtikkelNr NO-ERROR.
        IF NOT AVAILABLE ArtBas THEN NEXT TRANSLOGG. 
        IF NOT CAN-DO(cLevNrLst,STRING(ArtBas.LevNr)) THEN NEXT TRANSLOGG.
      END.
      
      /* Tar kun transaksjoner som det er lagt inn gyldig artikkelnummer på */
      IF NOT CAN-FIND(ArtBas WHERE
                      ArtBas.ArtikkelNr = TransLogg.ArtikkelNr) THEN
          NEXT TRANSLOGG.
          
      FIND ArtBas NO-LOCK WHERE
        ArtBas.ArtikkelNr = Translogg.ArtikkelNr NO-ERROR.
      IF AVAILABLE ArtBas THEN DO:
        IF ArtBas.Pakke THEN NEXT TRANSLOGG.
        IF ArtBas.OPris THEN NEXT TRANSLOGG.
        IF ArtBas.Pant  THEN NEXT TRANSLOGG.

        /* Sjekk av sortiment */
        IF bGrunnsortKjede = TRUE THEN 
        DO:
          IF ArtBas.KjedeVare = FALSE AND ArtBas.Grunnsortiment = FALSE THEN NEXT TRANSLOGG.
          /* Er ikke flagg for grunnsortiment satt, skal heller ikke disse med. */
          IF bGrunnsort = FALSE AND ArtBas.Kjedevare = FALSE THEN NEXT TRANSLOGG.
        END.
      END.
      
      /* Må være en gyldig størrelse */
      FIND StrKonv NO-LOCK WHERE
          StrKonv.Storl = Translogg.Storl NO-ERROR.
      IF NOT AVAILABLE StrKonv THEN
          NEXT TRANSLOGG.

      RUN bibl_loggDbFri.p (cLogg,'  Butikk. ' + 
                               ENTRY(iButikkLoop,cButikkLSt) + 
                               ' Dato: ' + STRING(dDato) +  
                               ' ArtikkelNr: ' + STRING(TransLogg.ArtikkelNr)
                       ).


      /* Logger at det er funnet transaksjoner. */
      IF obOk = FALSE THEN obOk = TRUE.


      ASSIGN
          iAntall = iAntall + 1
          .

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
      ELSE DO:
         ASSIGN
              tmpArtLag.Antall     = tmpArtLag.Antall 
                                     + IF AVAILABLE ArtLag
                                         THEN ArtLag.LagAnt
                                         ELSE 0.      
      END. /* LAGER */
      
      /* Logger ordreforslag. */
      POSTER:
      DO:
          FIND FIRST tmpPlukk WHERE
              tmpPlukk.ButikkNr   = TransLogg.Butik AND
              tmpPlukk.ArtikkelNr = TransLogg.ArtikkelNr AND
              tmpPlukk.StrKode    = StrKonv.StrKode NO-ERROR.
          IF NOT AVAILABLE tmpPlukk THEN
          NYOVANMODNING:
          DO:
              CREATE tmpPlukk.
              ASSIGN
                  tmpPlukk.ButikkNr    = TransLogg.Butik 
                  tmpPlukk.ArtikkelNr  = TransLogg.ArtikkelNr 
                  tmpPlukk.StrKode     = StrKonv.StrKode
                  tmpplukk.levnr       = ArtBas.levnr
                  .
          END. /* NYOVANMODNING */

          /* Akkumulering */
          ASSIGN
              tmpPlukk.Antall  = tmpPlukk.Antall   + TransLogg.Antall
              lSumAnt          = lSumAnt           + TransLogg.Antall
              tmpArtLag.Antall = TransLogg.Antall.
      END. /* POSTER */
    END. /* TRANSLOGG */
  END. /* DATOLOOP */

  RUN bibl_loggDbFri.p (cLogg,'  PlukkBehandling: Antall: ' + 
                    STRING(lSumAnt) + ' Butikk. ' + ENTRY(iButikkLoop,cButikkLSt)).

  TEMP-TABLE tmpPlukk:WRITE-JSON ("file", 'konv\tmpPlukk.JSon',TRUE).
  TEMP-TABLE tmpArtLag:WRITE-JSON ("file", 'konv\tmpArtLag.JSon',TRUE).

  /* Skaper plukklistelinjer for hver butikk. */
  RUN PlukkBehandling (lSumAnt,piButikkNr).

  /* Tømmer for hver butikk */
  FOR EACH tmpPlukk:
    DELETE tmpPlukk.
  END.
  
END. /* BUTIKKLOOP */

/* Sletter alle poster som har 0 i antall */
FOR EACH tmpPlukk WHERE 
  tmpPlukk.ButikkNr = piButikkNr AND 
  tmpPlukk.Antall   = 0:
  DELETE tmpPlukk.
END.

END PROCEDURE.

PROCEDURE ompakkingAvLister:
/*------------------------------------------------------------------------------
		Purpose:  																	  
		Notes:  																	  
------------------------------------------------------------------------------*/
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
END PROCEDURE.

PROCEDURE opprettPlListeHode:
/*------------------------------------------------------------------------------
		Purpose: Henter PlListeHode eller oppretter denne hvis den ikke finnes. 																	  
		Notes:   Det opprettes et hode pr. butikk for kjedeleverandør på 
		         artikler som er kjedelevert.
		         Hvis det også kjøres med supplering på grunnsortiment, vil de
		         artiklene som er flagget som grunnsortiment, men ikke er 
		         kjedeleverte, bli lagt opp på en PlListeHode for den aktuelle
		         butikk og leverandør.	
		bSeparat
		bGrunnsort															  
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER piButikkNr AS INTEGER NO-UNDO.
  
  lPlListeId = 0.
  IF AVAILABLE PlListeHode THEN 
    RELEASE PlListeHode.
    
  /* Behandler kjedeleverte varer. Det skal fylles på på åpne plukklister. */
  IF ArtBas.KjedeVare THEN 
  KJEDELEVERT:
  DO:
      /* Kjedeleverte varer - felles postering på kjedeleverandør. */
      IF bSeparat = FALSE THEN 
       DO:
          FIND LAST PlListeHode NO-LOCK WHERE 
            plListeHode.FraButikkNr   = piButikkNr AND 
            PlListeHode.PlLType       = 2 AND 
            plListeHode.LevNr         = iLogPartner AND 
            PlListeHode.PlListeStatus = 1 AND  
            PlListeHode.PlListeId     > 0
            NO-ERROR.
          IF AVAILABLE PlListeHode THEN
              lPlListeId  = PlListeHode.PlListeId.
          ELSE lPlListeId = 0.

           RUN bibl_loggDbFri.p (cLogg, '  Kjedelevert Separat = FALSE: ' 
               + IF AVAILABLE plListeHode THEN 'JA' ELSE 'NEI' 
               + ' lPlListeId: ' + STRING(lPlListeId)
               ).              

         /* Henter default bestillingsdato for fri levering */
         FIND FIRST DefaultLevDato NO-LOCK WHERE
           DefaultLevDato.ButikkNr = iCL AND
           DefaultLevDato.LevNr    = iLogPartner NO-ERROR.
         IF AVAILABLE DefaultLevDato THEN 
           iUkeDag = DefaultLevDato.UkeDag.
         ELSE
           iUkeDag = 7.
         IF lPlListeId = 0 THEN 
            RUN Oppstandelsen (piButikkNr,2,iLogPartner,1,OUTPUT lPlListeId).
      END.
      /* Legger opp kjedevarer pr. leverandør. */
      ELSE DO:
          FIND LAST PlListeHode NO-LOCK WHERE 
            plListeHode.FraButikkNr   = piButikkNr AND 
            PlListeHode.PlLType       = 2 AND 
            plListeHode.LevNr         = ArtBas.LevNr AND 
            PlListeHode.PlListeStatus = 1 AND  
            PlListeHode.PlListeId     > 0
            NO-ERROR.
          IF AVAILABLE PlListeHode THEN
              lPlListeId  = PlListeHode.PlListeId.
          ELSE lPlListeId = 0.

          RUN bibl_loggDbFri.p (cLogg, '  Kjedelevert Separat = TRUE: ' 
              + IF AVAILABLE plListeHode THEN 'JA' ELSE 'NEI' 
              + ' lPlListeId: ' + STRING(lPlListeId)
              ).              

         /* Henter default bestillingsdato for fri levering */
         FIND FIRST DefaultLevDato NO-LOCK WHERE
           DefaultLevDato.ButikkNr = iCL AND
           DefaultLevDato.LevNr    = ArtBas.LevNr NO-ERROR.
         IF AVAILABLE DefaultLevDato THEN 
           iUkeDag = DefaultLevDato.UkeDag.
         ELSE
           iUkeDag = 7.
         IF lPlListeId = 0 THEN 
            RUN Oppstandelsen (piButikkNr,2,ArtBas.LevNr,1,OUTPUT lPlListeId).
      END.
  END. /* KJEDELEVERT */

  /* Behanlder grunnsortimentsvarer som ikke er kjedeleverte */
  ELSE IF ArtBas.Grunnsortiment THEN 
  GRUNNSORTIMENT:
  DO:
      FIND LAST PlListeHode NO-LOCK WHERE 
        plListeHode.FraButikkNr   = piButikkNr AND 
        PlListeHode.PlLType       = 2 AND 
        plListeHode.LevNr         = ArtBas.LevNr AND 
        PlListeHode.PlListeStatus = 1 AND  
        PlListeHode.PlListeId     > 0
        NO-ERROR.
      IF AVAILABLE PlListeHode THEN
          lPlListeId  = PlListeHode.PlListeId.
      ELSE lPlListeId = 0.
      /* Henter default bestillingsdato for fri levering */

      RUN bibl_loggDbFri.p (cLogg, '  Grunnsortiment: ' 
          + IF AVAILABLE plListeHode THEN 'JA' ELSE 'NEI' 
          + ' lPlListeId: ' + STRING(lPlListeId)
          ).              

      FIND FIRST DefaultLevDato NO-LOCK WHERE
        DefaultLevDato.ButikkNr = iCL AND
        DefaultLevDato.LevNr    = ArtBas.LevNr NO-ERROR.
      IF AVAILABLE DefaultLevDato THEN 
        iUkeDag = DefaultLevDato.UkeDag.
      ELSE
        iUkeDag = 7.
      IF lPlListeId = 0 THEN RUN Oppstandelsen (piButikkNr,2,ArtBas.LevNr,1,OUTPUT lPlListeId).
  END. /* GRUNNSORTIMENT */

  /* Ingen behandling av kjede eller grunnsortiment */
  ELSE  
  INGEN_BEHANDLING:
  DO:
      FIND LAST PlListeHode NO-LOCK WHERE 
        plListeHode.FraButikkNr   = piButikkNr AND 
        PlListeHode.PlLType       = 2 AND 
        plListeHode.LevNr         = ArtBas.LevNr /*iLogPartner*/ AND 
        PlListeHode.PlListeStatus = 1 AND  
        PlListeHode.PlListeId     > 0
        NO-ERROR.
      IF AVAILABLE PlListeHode THEN
          lPlListeId  = PlListeHode.PlListeId.
      ELSE lPlListeId = 0.
      /* Henter default bestillingsdato for fri levering */

      RUN bibl_loggDbFri.p (cLogg, '  Ingen behandling: ' 
          + IF AVAILABLE plListeHode THEN 'JA' ELSE 'NEI' 
          + ' lPlListeId: ' + STRING(lPlListeId)
          ).              

      FIND FIRST DefaultLevDato NO-LOCK WHERE
        DefaultLevDato.ButikkNr = iCL AND
        DefaultLevDato.LevNr    = (IF bSeparat = FALSE THEN iLogPartner ELSE ArtBas.LevNr) NO-ERROR.
      IF AVAILABLE DefaultLevDato THEN 
        iUkeDag = DefaultLevDato.UkeDag.
      ELSE
        iUkeDag = 7.
      IF lPlListeId = 0 THEN 
        RUN Oppstandelsen (piButikkNr,2,IF bSeparat = FALSE THEN iLogPartner ELSE ArtBas.LevNr,1,OUTPUT lPlListeId).
  END. /* INGEN_BEHANDLING */

  RUN bibl_loggDbFri.p (cLogg, '  Suppl.ordre tilgengelig: ' 
              + IF AVAILABLE plListeHode THEN 'JA' ELSE 'NEI' 
              + ' lPlListeId: ' + STRING(lPlListeId)
              + ' OPPSLAG på Butikk: ' + STRING(piButikkNr) 
              + ' Plukkliste type: ' + IF AVAILABLE PlListeHode THEN STRING(PlListeHode.PlLType) ELSE ''
              + ' Leverandør: ' + IF AVAILABLE ArtBas THEN STRING(ArtBas.LevNr) ELSE ''
              + ' Artikkel: ' + IF AVAILABLE ArtBas THEN STRING(ArtBas.ArtikkelNr) ELSE ''
              + ' Logistikkpartner: ' + STRING(iLogPartner)
              + ' Status:' + IF AVAILABLE PlListeHode THEN STRING(PlListeHode.PlListeStatus) ELSE ''
              ).              

  /* Hvis fri leveringsdag er passert, skal ny ordre opprettes uansett. */
  IF AVAILABLE plListeHode THEN 
  DO: 
      IF WEEKDAY(TODAY) > iUkeDag THEN 
      DO:
        RUN bibl_loggDbFri.p (cLogg, '  Sjekk om ukedag > fri leveringsdag: ' 
                    + 'Ukedag: ' + STRING(WEEKDAY(TODAY)) + ' ' 
                    + 'Fri leveringsdag: ' + STRING(iUkeDag)  
                    + ' Variabel lPlListeId er nullstillt'
                    ).              

         lPlListeId = 0.
         RUN Oppstandelsen (piButikkNr,2,PlListeHode.LevNr,1,OUTPUT lPlListeId).
      END.
  END.

END PROCEDURE.

PROCEDURE Oppstandelsen:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER lokButikkNr   AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER lokPlType     AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER lokLogPartner AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER lokPlListeSTatus AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER LokPlListeId AS DECIMAL NO-UNDO.
    
    /* Oppretter plukklistehode. */
    OPPSTANDELSEN:
    DO:
        FIND Butiker NO-LOCK WHERE
            Butiker.Butik = lokButikkNr NO-ERROR.
        
        FIND LAST PlListeHode NO-LOCK USE-INDEX PlListeHode NO-ERROR.
        IF AVAILABLE PlListeHode THEN
            LokPlListeId  = PlListeHode.PlListeId + 1.
        ELSE LokPlListeId = 1.
        IF AVAILABLE plListeHode THEN RELEASE plListeHode.
      
        /* Er nummerserie full, får vi lete etter hull */
        IF LokPlListeId > 99999999 THEN
        LOOPEN:
        DO LokPlListeId = 1 TO 99999999:
            IF NOT CAN-FIND(PlListeHode WHERE
                PlListeHode.PlListeId = LokPlListeId) THEN
                LEAVE LOOPEN.
        END. /* LOOPEN */
        /* Oppstandelsen */
        DO:
            /* Oppretter listehode. */
            CREATE PlListeHode.
            ASSIGN
                PlListeHode.PlListeId     = LokPlListeId
                PlListeHode.FraButikkNr   = lokButikkNr
                PlListeHode.TilButikkNr   = lokButikkNr
                PlListeHode.DatoPlukket   = ?
                PlListeHode.TidPlukket    = 0
                PlListeHode.PlNavn        = "Ordreforslag butikk: " + STRING(lokButikkNr) + ' ' + (IF AVAILABLE Butiker THEN Butiker.ButNamn ELSE 'Ukjent butikk')
                PlListeHode.PlMerknad     = "" /*"Plukkliste til " + Butiker.ButNamn*/
                PlListeHode.PlLType       = lokPlType /* Supl.ordreforslag */
                plListeHode.PlListeStatus = lokPlListeSTatus
                plListeHode.LevNr         = lokLogPartner
                .
        END.
    END. /* OPPSTANDELSEN */


END PROCEDURE.

PROCEDURE PlukkBehandling:
    /*DEFINE INPUT PARAMETER plPlListeId AS DECIMAL NO-UNDO.*/
    DEFINE INPUT PARAMETER plSumAnt    AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER piButikkNr  AS INTEGER NO-UNDO.

    IF bSeparat = FALSE THEN 
    PLUKKBEHANDLING:
    FOR EACH tmpPlukk WHERE tmpPlukk.ButikkNr = piButikkNr
        BREAK BY tmpPlukk.ButikkNr
              BY tmpPlukk.ArtikkelNr
              BY tmpPlukk.StrKode:

      /* Må finnes */
      FIND ArtBas NO-LOCK WHERE
          ArtBas.ArtikkelNr = tmpPlukk.ArtikkelNr NO-ERROR.
      IF NOT AVAILABLE ArtBas THEN
          NEXT PLUKKBEHANDLING.

      /* Henter eller oppretter PlListehode som artikkelen skal legges inn på. */
      FIND PlListeHode NO-LOCK WHERE
            PlListeHode.PlListeId   = lPlListeId AND  
            plListeHode.FraButikkNr = piButikkNr
            NO-ERROR.
      IF NOT AVAILABLE PlListeHode THEN 
          RUN opprettPlListeHode (pibutikkNr).

      /* posterer eller akkumulerer linjen. */
      RUN posterLinje.
            
    END. /* PLUKKBEHANDLING */

    ELSE  
        /* Leser plukkanmodning i prioritert rekkefølge. */
        /* Her posteres plikkliste linjene.              */
        PLUKKleVBEHANDLING:
        FOR EACH tmpPlukk WHERE tmpPlukk.ButikkNr = piButikkNr
            BREAK BY tmpPlukk.ButikkNr
            BY tmpplukk.levnr
            BY tmpPlukk.ArtikkelNr
            BY tmpPlukk.StrKode:

            /* Må finnes */
            FIND ArtBas NO-LOCK WHERE
                ArtBas.ArtikkelNr = tmpPlukk.ArtikkelNr NO-ERROR.
            IF NOT AVAILABLE ArtBas THEN
                NEXT PLUKKLEVBEHANDLING.

            IF FIRST-OF(tmpPlukk.LevNr) THEN 
            DO:
                IF AVAILABLE PlListeHode THEN 
                  RELEASE PlListeHode.
                lPlListeId = 0.
            END.

            /* Henter eller oppretter PlListehode som artikkelen skal legges inn på. */
            FIND PlListeHode NO-LOCK WHERE
                PlListeHode.PlListeId   = lPlListeId AND  
                plListeHode.FraButikkNr = piButikkNr NO-ERROR.
            IF NOT AVAILABLE PlListeHode THEN 
                RUN opprettPlListeHode (pibutikkNr).

            /* posterer eller akkumulerer linjen. */
            RUN posterLinje.
            
        END. /* PLUKKBEHANDLING */

    /* Dette gjøres altså her ved sluttbehandling av hver butikk */
    HODESUM:
    DO TRANSACTION:
        FIND PlListeHode EXCLUSIVE-LOCK WHERE
            PlListeHode.PlListeId = lPlListeId NO-ERROR.

        IF AVAILABLE PlListeHode THEN 
        DO:

            ASSIGN
              PlListeHode.Antall = lSumAnt.

            /* Tomme lister fjernes (Linjer tas eventuelt i trigger) */
            IF NOT CAN-FIND(FIRST PlListeLinje OF PlListeHode) THEN
                DELETE PlListeHode.
        END.
    END. /* HODESUM */
END PROCEDURE.

PROCEDURE posterLinje:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    /* Skaper plukklistelinjen hvis den ikke finnes fra før */
    FIND FIRST PlListeLinje EXCLUSIVE-LOCK WHERE
        PlListeLinje.PlListeId  = lPlListeId AND
        PlListeLinje.ArtikkelNr = tmpPlukk.ArtikkelNr AND
        PlListeLinje.StrKode    = tmpPlukk.StrKode NO-ERROR.
    IF NOT AVAILABLE PlListeLinje THEN
    DO:
        FIND LAST bufPlListeLinje NO-LOCK WHERE
            bufPlListeLinje.PlListeId = lPlListeId USE-INDEX PlListeLinje NO-ERROR.
        CREATE PlListeLinje.
        ASSIGN
            PlListeLinje.PlListeId  = lPlListeId 
            PlListeLinje.PlLinjeNr  = IF AVAILABLE bufPlListeLinje
                                           THEN bufPlListeLinje.PlLinjeNr + 1
                                           ELSE 1
            PlListeLinje.ArtikkelNr = tmpPlukk.ArtikkelNr
            plListeLinje.LevFargKod = ArtBas.LevFargKod 
            plListeLinje.LevKod     = ArtBas.LevKod
            plListeLinje.VarGr      = ArtBas.Vg
            plListeLinje.Beskr      = ArtBas.Beskr
            PlListeLinje.StrKode    = tmpPlukk.StrKode 
            .
    END.

    /* Akkumulerer opp antall på plukke for artikkel, størrelse og butikk */
    IF AVAILABLE PlListeLinje THEN
        ASSIGN
            PlListeLinje.VarGr         = ArtBas.Vg
            plListeLinje.LopNr         = ArtBas.LopNr
            PlListeLinje.Antall        = PlListeLinje.Antall + tmpPlukk.Antall
            PlListeLinje.AntallPlukket = PlListeLinje.AntallPlukket + tmpPlukk.Antall
            .


END PROCEDURE.

PROCEDURE validerLister:
/*------------------------------------------------------------------------------
		Purpose:  																	  
		Notes:  																	  
------------------------------------------------------------------------------*/
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
IF TRIM(cLevNrLst) <> "" THEN
DO iLoop = 1 TO NUM-ENTRIES(cLevNrLst):
  IF NOT CAN-FIND(LevBas WHERE LevBas.LevNr = INT(ENTRY(iLoop,cLevNrLst))) THEN
  DO:
    ASSIGN
      ocReturn = ocReturn + (IF NOT ocReturn = '' THEN '\n' ELSE '') + 'FEIL: Kan ikke generere plukkliste, ugyldig leverandør. :' + ENTRY(iLoop,cLevNrLst) + ':'
      obOk     = FALSE
    .
  END.
END.
END PROCEDURE.


/* ************************  Function Implementations ***************** */

FUNCTION hentSalg RETURNS DECIMAL 
    ( INPUT plArtikkelNr AS DECIMAL,
      INPUT piSalgsdager AS INTEGER, 
      INPUT piButik AS INTEGER,
      INPUT pcTTidLst AS CHARACTER ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/    

    DEFINE VARIABLE piResult AS DECIMAL FORMAT "->>>>>>>>9"NO-UNDO.

    FOR EACH TransLogg NO-LOCK WHERE 
        TransLogg.ArtikkelNr  = plArtikkelNr AND 
        TransLogg.Dato       >= TODAY - piSalgsdager AND 
        TransLogg.Tid        >= 0 AND
        TransLogg.Butik       = piButik AND  
        CAN-DO(pcTTidLst,STRING(TransLogg.TTId)):
        ASSIGN 
            piResult = piResult + TransLogg.Antall
            .      
    END.
    IF piResult < 0 THEN 
        piResult = 0.
    RETURN piResult.
        
END FUNCTION.
