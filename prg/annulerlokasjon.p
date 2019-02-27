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
def var wEDB-System as char no-undo.
def var wTabell     as char no-undo.


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
assign wTabell = "ArtBas".

{syspara.i 5 1 1 wCl INT}
find clButiker where
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
      FIND Butiker NO-LOCK WHERE
          Butiker.Butik = bLokasjon.Butik NO-ERROR.
      find ArtBas no-lock where
          Artbas.ArtikkelNr = bLokasjon.ArtikkelNr NO-ERROR.
      IF NOT AVAILABLE ArtBas THEN
          FIND ArtBas NO-LOCK WHERE
          ArtBas.Vg = bLokasjon.Vg AND
          ArtBas.LopNr = bLokasjon.LopNr NO-ERROR.
      if not available ArtBas then
        do:
          IF avail ht-fillinje THEN HT-FilLinje.Ok = false.
          next LES_LINJE.
        end.
      FIND Farg OF ArtBas NO-LOCK NO-ERROR.

      /* Henter TelleLinjen om den finnes. */
      find TelleLinje exclusive-lock where
        TelleLinje.TelleNr    = TelleHode.TelleNr and
        TelleLinje.ArtikkelNr = bLokasjon.ArtikkelNr and
        TelleLinje.Butik      = bLokasjon.Butik and
        TelleLinje.Storl      = bLokasjon.Storl no-error.            

      find ArtPris no-lock where
        ArtPris.ArtikkelNr = ArtBas.ArtikkelNr and
        ArtPris.ProfilNr   = Butiker.ProfilNr no-error.

      /* Henter pris fra sentrallageret hvis den ikke finnes for butikken. */
      if not available ArtPris then
        do:
          find ArtPris no-lock where
            ArtPris.ArtikkelNr = ArtBas.ArtikkelNr and
            ArtPris.ProfilNr   = clButiker.ProfilNr no-error.
        end.

      /* Oppdaterer tellelinjen */
      IF AVAILABLE TelleLinje THEN
      DO:
          assign
            TelleLinje.OpprAntalTalt = TelleLinje.OpprAntalTalt - bLokasjon.AntallTalt
            TelleLinje.AntallTalt    = TelleLinje.AntallTalt    - bLokasjon.AntallTalt
            TelleLinje.AntallDiff    = TelleLinje.AntallPar     - TelleLinje.AntallTalt
            TelleLinje.VerdiDiff     = TelleLinje.AntallDiff    * TelleLinje.VVareKost
            TelleLinje.OpptVerdi     = TelleLinje.AntallTalt    * TelleLinje.VVareKost
            .
      END.
      ASSIGN
          bLokasjon.Oppdatert = FALSE
          .
  END.

  /* Flagger lokasjonslisten som oppdatert. */
  DO TRANSACTION:
      FIND CURRENT bHLokasjon EXCLUSIVE-LOCK.
      ASSIGN
          bhLokasjon.KobletTilTelleNr = 0
          bhLokasjon.Oppdatert        = ?
          bhLokasjon.Notat            = bHLokasjon.Notat +
                                 (IF bHLokasjon.Notat <> ""
                                    THEN CHR(10)
                                    ELSE "") + 
                                 userid('skotex') + " " + string(today) + " " + string(time,"HH:MM:SS") + " Lokasjonsliste annulert mot telling " +
                                 STRING(TelleHode.TelleNr) + " " + TelleHode.Beskrivelse.
      FIND CURRENT bHLokasjon NO-LOCK.

      FIND CURRENT TelleHode EXCLUSIVE-LOCK.
      ASSIGN
          Tellehode.Notat     = Tellehode.Notat +
                                 (IF Tellehode.Notat <> ""
                                    THEN CHR(10)
                                    ELSE "") + 
                                 userid('skotex') + " " + string(today) + " " + string(time,"HH:MM:SS") + " Annulert lokasjonsliste " +
                                 STRING(bHLokasjon.TelleNr) + " " + bHLokasjon.Beskrivelse.
      FIND CURRENT Tellehode NO-LOCK.

  END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

