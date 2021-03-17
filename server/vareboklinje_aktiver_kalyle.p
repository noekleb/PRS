/* Aktiver kalkyle for varebok 
   Parametere:  Vareboknr;evt liste over artikler
   
   Opprettet: 10.08.05 av BHa 
   Endret:    25.04.06 av BHa
            - Endrer fra forhånd til suppleringskalkyle
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR hQuery            AS HANDLE NO-UNDO.
DEF VAR ix                AS INT    NO-UNDO.
DEF VAR bOK               AS LOG    NO-UNDO.
DEF VAR hPrisKo           AS HANDLE NO-UNDO.

DEF BUFFER bArtBas FOR ArtBas.

IF SEARCH("prisko.p") NE ? OR SEARCH("prisko.r") NE ? THEN
  RUN prisko.p PERSIST SET hPrisKo.
ELSE DO:
  ocReturn = "Finner ikke program prisko.p".
  RETURN.
END.

FIND FIRST VarebokHode WHERE VarebokHode.VarebokNr = DEC(ENTRY(1,icParam,";")) NO-LOCK NO-ERROR.
IF NOT AVAIL VarebokHode THEN DO:
  ocReturn = "Finner ikke varebok: " + ENTRY(1,icParam,";").
  RETURN.
END.

FUNCTION KalkStreng RETURNS CHARACTER() FORWARD.

CREATE QUERY hQuery.
hQuery:SET-BUFFERS(BUFFER VarebokLinje:HANDLE).
obOk = hQuery:QUERY-PREPARE("FOR EACH VarebokLinje NO-LOCK WHERE VarebokNr = " 
                           + ENTRY(1,icParam,";")  /* Vareboknr */
                           + (IF NUM-ENTRIES(icParam,";") > 1 THEN ENTRY(2,icParam,";") ELSE "")  /* Evt. liste over artikler (can-do) */
                           ) NO-ERROR.
IF NOT obOk THEN DO:
  ocReturn = "Denne spørringen blir ikke godtatt: " + CHR(10) + "FOR EACH VarebokLinje NO-LOCK WHERE VarebokNr = " + ENTRY(1,icParam,";") + " BY " + ENTRY(3,icParam,";").
  RETURN.
END.

hQuery:QUERY-OPEN().
hQuery:GET-FIRST().
REPEAT WHILE NOT hQuery:QUERY-OFF-END:
    
  FIND FIRST ArtBas WHERE ArtBas.ArtikkelNr = VarebokLinje.ArtikkelNr NO-LOCK NO-ERROR.
  IF AVAIL ArtBas THEN RUN LagreKalkyle.
           .
  IF NOT obOK THEN
    UNDO, LEAVE.

  hQuery:GET-NEXT().
END.

DELETE OBJECT hQuery.

IF ocReturn = "" THEN obOk = TRUE.

PROCEDURE LagreKalkyle:
/*------------------------------------------------------------------------------
  Purpose:  
    Notes: Tilpasset fra prosedyre LagreKalkyle i w-kalkyle.w 
------------------------------------------------------------------------------*/

  DEF VAR wSkjerm       AS CHAR  NO-UNDO.
  DEF VAR w2Skjerm      AS CHAR  NO-UNDO.
  
  DEF VAR pcError      AS CHAR  NO-UNDO.
  DEF VAR rowPrisKo    AS RECID NO-UNDO.
  DEF VAR wArtBasRecid AS RECID NO-UNDO.
  
  ASSIGN w2Skjerm = KalkStreng()
         wArtBasRecid = RECID(ArtBas)
         wSkjerm  = w2Skjerm
         .
  
  /* Sjekker om ny post kan opprettes */
  RUN SjekkNyPrisKo IN hPrisKo (ArtBas.ArtikkelNr,
                               VarebokHode.ProfilNr,
                               TODAY,
                               TIME - 10,
                               TODAY,
                               TIME - 10,
                               ?,
                               FALSE,
                               1,
                               OUTPUT pcError).
  /* Feil som krever avbrudd for tilbud og normalpris. */
  IF CAN-DO("1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19," + 
            "50,51,52,53,54,55,56,57",ENTRY(1,pcError,CHR(1))) THEN
  DO:
    ASSIGN ocReturn = ENTRY(2,pcError,CHR(1))
           obOk     = FALSE.
    RETURN.
  END.

  /* Sjekker om det finnes noen artpris post på artikkelen. Gjør det ikke
     det, skal prispost opprettes direkte og initieres med aktuell kalkyle.
     Dette gjelder bare normalprisposter.
  */
  IF NOT CAN-FIND(FIRST ArtPris WHERE
                  ArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND
                  ArtPris.ProfilNr   = PrisProfil.ProfilNr) THEN DO:
      rowPrisKO = ?.
      RUN GetLastPrisKo IN hPrisko (OUTPUT rowPrisKo).

      FIND PrisKo NO-LOCK WHERE
          RECID(PrisKo) = rowPrisKo NO-ERROR.

      IF AVAILABLE PrisKo THEN
        RUN LagreArtPris IN hPrisko
            (INPUT recid(ArtBas),
             INPUT VarebokHode.ProfilNr,
             INPUT-OUTPUT wSkjerm,
             INPUT FALSE,  /* wTilbud = false - Dvs ordinær kalkyle.          */
             INPUT TRUE,   /* Direkte oppdatering av prisene som er kalkulert */
             INPUT PrisKo.TYPE,
             INPUT ROWID(PrisKo)).
  END. 


  /* Lagrer normalprisendring i priskø. */
  RUN NyPrisKo IN hPrisKo 
      (
       INPUT wArtBasRecid, 
       INPUT VarebokHode.ProfilNr,
       INPUT-OUTPUT w2Skjerm,
       FALSE,
       1
      ).

  /* Oppdaterer anbefalt pris og suppleringsrabatt*/
  DO TRANSACTION:

    FIND bArtBas WHERE RECID(bArtBas) = RECID(ArtBas) EXCLUSIVE-LOCK NO-ERROR.
    IF AVAILABLE bArtBas THEN
    DO:
      ASSIGN 
             /* Artikkelinformasjon */
             bArtBas.LevKod            = VareBokLinje.LevKod
             bArtBas.Beskr             = VareBokLinje.Beskr
             bArtBas.LevFargKod        = VareBokLinje.LevFarg
             bArtBas.Gjennomfaktureres = VareBokLinje.Gjennomfaktureres
             bArtBas.KjedeVare         = VarebokLinje.Kjedevare
             bArtBas.ModellFarge       = VareBokLinje.ModellFarge
             /*bArtBas.Vg                = VareBokLinje.Vg  TN 16/12-09 Gir problem med løpenrtildeling. */
             bArtBas.Hg                = VareBokLinje.Hg
             bArtBas.LevNr             = VareBokLinje.LevNr
             bArtBas.LinjeMerknad      = VareBokLinje.LinjeMerknad
             bArtBas.LevDato1          = VarEBokLinje.LevDato1    
             bArtBas.LevDato2          = VarEBokLinje.LevDato2    
             bArtBas.LevDato3          = VarEBokLinje.LevDato3    
             bArtBas.LevDato4          = VarEBokLinje.LevDato4
             bArtBas.VPIDato           = VareBokLinje.VPIDato    

             /* VPI informasjon */
             bArtBas.forhRab%      = VarebokLinje.forhRab%
             bArtBas.supRab%       = VarebokLinje.supRab%
             bArtBas.SaSong        = VarebokLinje.SaSong
             bArtBas.AnbefaltPris  = VarebokLinje.AnbefaltPris
             bArtBas.Katalogpris  = Vareboklinje.Innkjopspris /*VareBokLinje.Katalogpris*/
             bArtBas.KjedeInnkPris = VareBokLinje.KjedeInnkPris
             bArtBas.KjedeRab%     = VareBokLinje.KjedeRab%
             .
      RELEASE bArtBas.
    END.
  END.

  RUN KlargjorPrisKoEn IN hPrisKo (ROWID(ArtBas)).
  
END PROCEDURE.


FUNCTION KalkStreng RETURNS CHARACTER() :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes: Tilpasset fra funksjon KalkStreng i w-kalkyle.w 
------------------------------------------------------------------------------*/
  DEF VAR wTekst   AS CHAR NO-UNDO.
  
  wTekst =  STRING(VarebokLinje.InnkjopsPris) + ";" +
            STRING(VarebokLinje.InnkjopsPris) + ";" +
            STRING(VarebokLinje.InnkjopsPris * VarebokLinje.supRab% / 100) + ";" +
            STRING(VarebokLinje.supRab%) + ";" +
            STRING(0) + ";" +
            STRING(0) + ";" +
            STRING(0) + ";" +
            STRING(0) + ";" +
            STRING(0) + ";" +
            STRING(0) + ";" +
            STRING(0) + ";" +
            STRING(0) + ";" +
            STRING(VarebokLinje.supVareKost) + ";" +
            STRING(VarebokLinje.Mva) + ";" +
            STRING(VarebokLinje.Mva%) + ";" +
            STRING(VarebokLinje.supDBkr) + ";" +
            STRING(VarebokLinje.supDB%) + ";" +
            STRING(VarebokLinje.Pris) + ";" +
            STRING(0) + ";" +
            "no" + ";" +
            STRING(TODAY) + ";" +
            STRING(TIME - 10) + ";" +
/* Tilbudaktiveringsdag / tid. */  
           ";0;;0;no".

/* Forhåndskalkyle: 
  wTekst =  STRING(VarebokLinje.InnkjopsPris) + ";" +
            STRING(VarebokLinje.InnkjopsPris) + ";" +
            STRING(VarebokLinje.InnkjopsPris * VarebokLinje.ForhRab% / 100) + ";" +
            STRING(VarebokLinje.ForhRab%) + ";" +
            STRING(0) + ";" +
          STRING(0) + ";" +
/*           STRING(VarebokLinje.SupRab%) + ";" + */
            STRING(0) + ";" +
            STRING(0) + ";" +
            STRING(0) + ";" +
            STRING(0) + ";" +
            STRING(0) + ";" +
            STRING(0) + ";" +
            STRING(VarebokLinje.VareKost) + ";" +
            STRING(VarebokLinje.Mva) + ";" +
            STRING(VarebokLinje.Mva%) + ";" +
            STRING(VarebokLinje.DBkr) + ";" +
            STRING(VarebokLinje.DB%) + ";" +
            STRING(VarebokLinje.Pris) + ";" +
            STRING(0) + ";" +
            "no" + ";" +
            STRING(TODAY) + ";" +
            STRING(TIME - 10) + ";" +
/* Tilbudaktiveringsdag / tid. */  
           ";0;;0;no".
           */
  
  RETURN wTekst.   /* Function return value. */

END FUNCTION.
