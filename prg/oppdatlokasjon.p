&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : 
    Purpose     :

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEF INPUT PARAMETER iTelleNr        AS INT  NO-UNDO.
DEF INPUT PARAMETER iButikkNr       AS INT  NO-UNDO.
DEF INPUT PARAMETER cLokasjonIdList AS CHAR NO-UNDO.

DEF VAR piLoop      AS INT  NO-UNDO.
DEF VAR wCl         AS INT  NO-UNDO.
DEF VAR wVVareKost  AS DEC  NO-UNDO.
DEF VAR wEDB-System AS CHAR NO-UNDO.
DEF VAR wTabell     AS CHAR NO-UNDO.


DEF BUFFER clButiker FOR Butiker.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

{syspara.i 1 2 4 wEDB-System}
ASSIGN wTabell = "ArtBas".

{syspara.i 5 1 1 wCl INT}
FIND clButiker WHERE
  clbutiker.Butik = wCl.

IF cLokasjonIdList <> "" THEN
DO piLoop = 1 TO NUM-ENTRIES(cLokasjonIdList,'|'):
    RUN oppdLokasjon (int(ENTRY(piLoop,cLokasjonIdList,'|'))).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-oppdLokasjon) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE oppdLokasjon Procedure 
PROCEDURE oppdLokasjon :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER piLokNr AS INT NO-UNDO.

  DEF BUFFER bHLokasjon FOR TelleHode.
  DEF BUFFER bLokasjon  FOR TelleLinje.

  /* Lokasjonslisten. */
  FIND bHLokasjon NO-LOCK WHERE
      bHLokasjon.TelleNr =  piLokNr NO-ERROR.
  IF NOT AVAILABLE bHLokasjon THEN
      RETURN "AVBRYT".
  IF bHLokasjon.TTId <> 9 THEN
      RETURN "AVBRYT".

  /* Hovedlisten som lokasjonslisten skal oppdateres mot. */
  FIND TelleHode NO-LOCK WHERE
      TelleHode.TelleNr = iTelleNr NO-ERROR.
  IF NOT AVAILABLE TelleHode THEN
      RETURN "AVBRYT".


  /* Går igjennom alle linjene i lokasjonslisten og oppdaterer mot hovedlisten. */
  LES_LINJE:
  FOR EACH bLokasjon OF bHLokasjon TRANSACTION:
      /* Tar bare med linjer det ligger noe på. */
      IF bLokasjon.AntallTalt = 0 THEN NEXT LES_LINJE.
      
      FIND Butiker NO-LOCK WHERE
          Butiker.Butik = bLokasjon.Butik NO-ERROR.
      FIND ArtBas NO-LOCK WHERE
          Artbas.ArtikkelNr = bLokasjon.ArtikkelNr NO-ERROR.
      IF NOT AVAILABLE ArtBas THEN
          FIND ArtBas NO-LOCK WHERE
          ArtBas.Vg = bLokasjon.Vg AND
          ArtBas.LopNr = bLokasjon.LopNr NO-ERROR.
      IF NOT AVAILABLE ArtBas THEN
        DO:
          IF AVAILABLE HT-FilLinje THEN HT-FilLinje.Ok = FALSE.
          NEXT LES_LINJE.
        END.
      FIND Farg OF ArtBas NO-LOCK NO-ERROR.

      /* Henter TelleLinjen om den finnes. */
      FIND TelleLinje EXCLUSIVE-LOCK WHERE
        TelleLinje.TelleNr    = TelleHode.TelleNr AND
        TelleLinje.ArtikkelNr = bLokasjon.ArtikkelNr AND
        TelleLinje.Butik      = bLokasjon.Butik AND
        TelleLinje.Storl      = bLokasjon.Storl NO-ERROR.            

      FIND ArtPris NO-LOCK WHERE
        ArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND
        ArtPris.ProfilNr   = Butiker.ProfilNr NO-ERROR.

      /* Henter pris fra sentrallageret hvis den ikke finnes for butikken. */
      IF NOT AVAILABLE ArtPris THEN
        DO:
          FIND ArtPris NO-LOCK WHERE
            ArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND
            ArtPris.ProfilNr   = clButiker.ProfilNr NO-ERROR.
        END.

      /* Finnes ikke linjen, legges den opp. */
      IF NOT AVAILABLE TelleLinje THEN
        NY-LINJE:
        DO:
          /* Henter lagerkost */
          FIND Lager OF ArtBas NO-LOCK WHERE
               Lager.Butik = bLokasjon.butik NO-ERROR.
          IF AVAILABLE Lager THEN
            wVVareKost = Lager.VVareKost.
          ELSE 
            wVVareKost = 0.

          /* Ukjent varekost ? */
          IF wVVarekost <= 0 OR ArtBas.Lager = FALSE THEN
          DO:
              FIND Butiker NO-LOCK WHERE
                Butiker.Butik = bLokasjon.Butik.

              FIND ArtPris NO-LOCK WHERE
                ArtPris.ArtikkelNr = bLokasjon.ArtikkelNr AND
                ArtPris.ProfilNr   = Butiker.ProfilNr NO-ERROR.

              /* Henter pris fra sentrallageret hvis den ikke finnes for butikken. */
              IF NOT AVAILABLE ArtPris THEN
                DO:
                  FIND ArtPris NO-LOCK WHERE
                    ArtPris.ArtikkelNr = bLokasjon.ArtikkelNr AND
                    ArtPris.ProfilNr   = clButiker.ProfilNr NO-ERROR.
                END.
              IF AVAILABLE ArtPris THEN
                wVVAreKost = ArtPris.Varekost[IF ArtPris.Tilbud THEN 2 ELSE 1].
          END.

          CREATE TelleLinje.
          ASSIGN
            TelleLinje.TelleNr    = Tellehode.TelleNr 
            TelleLinje.ArtikkelNr = bLokasjon.ArtikkelNr
            TelleLinje.Butik      = bLokasjon.Butik
            TelleLinje.Storl      = bLokasjon.Storl
            TelleLinje.VVareKost  = wVVAreKost
            TelleLinje.Vg         = bLokasjon.Vg
            TelleLinje.LopNr      = bLokasjon.LopNr
            TelleLinje.Kode       = bLokasjon.Kode
            Tellelinje.Beskr      = ArtBas.Beskr
            .

          FIND ArtLag NO-LOCK WHERE
            ArtLag.Butik = TelleLinje.Butik AND
            ArtLag.ArtikkelNr = TelleLinje.ArtikkelNr AND
            ArtLag.Storl = TelleLinje.Storl NO-ERROR.
          IF AVAILABLE ArtLag THEN
            ASSIGN
              TelleLinje.AntallPar = ArtLag.LagAnt
              TelleLinje.OpprVerdi = ArtLag.LagAnt * wVVareKost.

        END. /* NY-LINJE */


      /* Øvrig informasjon. */
      ASSIGN
        TelleLinje.LevKod     = bLokasjon.LevKod
        TelleLinje.LevFargKod = IF ArtBas.LevFargKod <> ""
                                  THEN ArtBas.LevFargKod
                                ELSE IF AVAILABLE Farg 
                                  THEN Farg.FarBeskr
                                ELSE ""
        TelleLinje.NedSkrevet    = TelleLinje.VVareKost
        
        TelleLinje.OpprAntalTalt = TelleLinje.OpprAntalTalt + bLokasjon.AntallTalt
        TelleLinje.AntallTalt    = TelleLinje.AntallTalt + bLokasjon.AntallTalt
        
        TelleLinje.AntallDiff    = TelleLinje.AntallPar  - TelleLinje.AntallTalt
        TelleLinje.VerdiDiff     = TelleLinje.AntallDiff * TelleLinje.VVareKost
        TelleLinje.OpptVerdi     = TelleLinje.AntallTalt * TelleLinje.VVareKost
        TelleLinje.LevNr         = bLokasjon.LevNr
        TelleLinje.Sasong        = bLokasjon.SaSong
        TelleLinje.Farg          = bLokasjon.Farg
        TelleLinje.MatKod        = bLokasjon.MatKod
        TelleLinje.VgLopNr       = bLokasjon.VgLopNr
        Tellelinje.Beskr         = IF TelleLinje.Beskr = "" THEN ArtBas.Beskr ELSE TelleLinje.Beskr
        .

      ASSIGN
          bLokasjon.Oppdatert = TRUE
          .
  END.

  /* Flagger lokasjonslisten som oppdatert. */
  DO TRANSACTION:
      FIND CURRENT bHLokasjon EXCLUSIVE-LOCK.
      ASSIGN
          bhLokasjon.KobletTilTelleNr = TelleHode.TelleNr
          bhLokasjon.Oppdatert = TODAY
          bhLokasjon.Notat     = bHLokasjon.Notat +
                                 (IF bHLokasjon.Notat <> ""
                                    THEN CHR(10)
                                    ELSE "") + 
                                 userid('skotex') + " " + string(TODAY) + " " + string(TIME,"HH:MM:SS") + " Lokasjonsliste oppdatert mot telling " +
                                 STRING(TelleHode.TelleNr) + " " + TelleHode.Beskrivelse.
      FIND CURRENT bHLokasjon NO-LOCK.

      FIND CURRENT TelleHode EXCLUSIVE-LOCK.
      ASSIGN
          Tellehode.Notat     = Tellehode.Notat +
                                 (IF Tellehode.Notat <> ""
                                    THEN CHR(10)
                                    ELSE "") + 
                                 userid('skotex') + " " + string(TODAY) + " " + string(TIME,"HH:MM:SS") + " Oppdatert med lokasjonsliste " +
                                 STRING(bHLokasjon.TelleNr) + " " + bHLokasjon.Beskrivelse.
      FIND CURRENT Tellehode NO-LOCK.

  END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

