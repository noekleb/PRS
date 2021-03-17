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

DEF INPUT PARAMETER plMedlemsNr LIKE Medlem.MedlemsNr NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-RabUMva) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD RabUMva Procedure 
FUNCTION RabUMva RETURNS DECIMAL
  ( wMva% AS DEC )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SRabUMva) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD SRabUMva Procedure 
FUNCTION SRabUMva RETURNS DECIMAL
  ( wMva% AS DEC )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


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

DEF VAR wDataObjekt AS CHAR NO-UNDO.
DEF VAR wWork       AS INT  NO-UNDO.
DEF VAR wVareKost   AS DEC  NO-UNDO.
DEF VAR wWork2      AS DEC  NO-UNDO.
DEF VAR wMva%       AS DEC  NO-UNDO.
DEF VAR wDefMva%    AS DEC  NO-UNDO.

DEF BUFFER bufStLinje FOR StLinje.

/* Default MVA% */
{syspara.i 2 1 4 wDefMva% DEC}

FIND Medlem NO-LOCK WHERE
    Medlem.MedlemsNr = plMedlemsNr NO-ERROR.
IF NOT AVAILABLE Medlem THEN
    RETURN "AVBRYT".

/* Tar bort statistikk */
FOR EACH stLinje EXCLUSIVE where
         StLinje.StType = "MEDLEM" and
         StLinje.DataObjekt = string(Medlem.MedlemsNr,"9999999999999"):

   DELETE stLinje.
END.

/* Bygger opp statistikken pånytt. */
MEDTRANS:
FOR EACH Translogg OF Medlem NO-LOCK:
    FIND ArtBas NO-LOCK WHERE
      ArtBas.ArtikkelNr = TransLogg.ArtikkelNr NO-ERROR.
    IF AVAILABLE ArtBas AND ArtBas.Non_Sale = TRUE THEN NEXT MEDTRANS.
    /* Henter Moms% */
    FIND VarGr NO-LOCK WHERE 
        VarGr.Vg = Translogg.Vg NO-ERROR.
    IF AVAILABLE VarGr THEN
      DO:
        FIND Moms OF VarGr NO-LOCK NO-ERROR.
        IF AVAILABLE Moms THEN
          wMva% = Moms.MomsProc.
        ELSE
          wMva% = 0.
      END.
    ELSE
      wMva% = 0.
    /* Bruker default MVA hvis den ikke er satt. */
    IF NOT AVAILABLE VarGr AND wMva% = 0 THEN
      wMva% = wDefMva%.

    /* Oppdaterer statistikker */
    STATISTIKK_DEF:
    FOR EACH stdef NO-LOCK WHERE
      StDef.StType = "MEDLEM" 
      BREAK BY StDef.StTypeId:
    
      /* Henter og kontrollerer perioden. */
      FIND Periode NO-LOCK WHERE
       Periode.PerId = StDef.PerId NO-ERROR.
      IF NOT AVAILABLE Periode THEN 
        RETURN "NEXT".
    
      /* Henter PeriodeLinje */
      /* Faste perioder.     */
      IF Periode.Fast THEN
        DO:
          CASE Periode.PerId:
            WHEN "AAR"   THEN /* Kun en linje i et år. */
              wWork = 1. 
            WHEN "MANED" THEN /* 12 Linjer pr. år */
              wWork = MONTH(Translogg.Dato).
            WHEN "UKE"   THEN /* Opptil 53 linjer pr. år */
              DO:
                RUN weeknum.p (Translogg.Dato, OUTPUT wWork).
                wWork = int(SUBSTRING(STRING(wWork,"999999"),5,2)).
              END.
            WHEN "DAG"   THEN /* Opptil 366 dager pr. år. */
              wWork = (Translogg.Dato + 1) - date(01,01,YEAR(Translogg.Dato)).
          END.
        END.
      /* Fri periode         */
      ELSE DO:
        FIND FIRST PerLin NO-LOCK WHERE
          PerLin.PerId   =  Periode.PerId  AND
          PerLin.FraDato <= Translogg.Dato AND 
          PerLin.TilDato >= Translogg.Dato NO-ERROR.
        IF NOT AVAILABLE PerLin THEN
          FIND LAST PerLin WHERE
            PerLin.PerId = Periode.PerId NO-ERROR.
        IF AVAILABLE PerLin THEN
          wWork = PerLin.PerLinNr.
        ELSE
          wWork = ?. /* Her er ingenting mer å gjøre. */
      END.
      /* Håndtering av ukjente frie statistikkdefineisjoner. */
      IF wWork = 0 OR 
         wWork = ? THEN 
        NEXT STATISTIKK_DEF.
      
      /* Henter/oppretter og stempler statistikkhode */      
      FIND StHode EXCLUSIVE-LOCK WHERE
        StHode.StTypeId = StDef.StTypeId AND
        StHode.PerId    = Periode.PerId NO-ERROR.
      IF NOT AVAILABLE StHode THEN
        DO:
          CREATE StHode.
          ASSIGN
            StHode.StTypeId = StDef.StTypeId
            StHode.PerId    = Periode.PerId.
          ASSIGN
            StHode.RegistrertDato = TODAY
            StHode.RegistrertTid  = TIME
            StHode.RegistrertAv   = USERID("dictdb").            
        END.
      ASSIGN
        StHode.EDato    = TODAY
        StHode.ETid     = TIME
        StHode.BrukerId = USERID("dictdb").            
        
      /* Setter dataobjekt */
      CASE StDef.StTypeId:
        WHEN "MEDLEM" THEN
          wDataObjekt = IF Translogg.MedlemsNr <> 0
                          THEN STRING(Translogg.MedlemsNr,"9999999999999")
                          ELSE "9999999999999".
      END CASE.

      /* Medlemsstatistikk skal bare oppdateres hvis det er et gyldig medlemsnummer. */
      IF StDef.StTypeId = "MEDLEM" THEN
      DO:
        IF Translogg.MedlemsNr = 0 THEN
          NEXT STATISTIKK_DEF.
      END.
      
      /* Oppdaterer statistikklinjen. */
      FIND StLinje EXCLUSIVE-LOCK WHERE
        StLinje.StTypeId   = StDef.StTypeId AND
        StLinje.PerId      = Periode.PerId AND
        StLinje.DataObjekt = wDataObjekt AND
        StLinje.Diverse    = "" AND
        StLinje.Butik      = Translogg.Butik AND
        StLinje.Aar        = year(Translogg.Dato) AND
        StLinje.PerLinNr   = int(wWork) NO-ERROR.
      IF NOT AVAILABLE StLinje THEN
        DO:
          CREATE StLinje.
          ASSIGN
            StLinje.StTypeId   = StDef.StTypeId 
            StLinje.PerId      = Periode.PerId 
            StLinje.DataObjekt = wDataObjekt 
            StLinje.Diverse    = ""
            StLinje.Butik      = Translogg.Butik
            StLinje.Aar        = YEAR(Translogg.Dato)
            StLinje.PerLinNr   = int(wWork).      
        END.

      /* Oppdaterer statistikkfeltene */
      CASE Translogg.TTId:      
        WHEN 1 THEN
          DO:
          ASSIGN /* Varesalg */
            StLinje.AntSolgt   = StLinje.AntSolgt   + Translogg.Antall  
            StLinje.VerdiSolgt = StLinje.VerdiSolgt + 
                               (
                                (Translogg.Pris - Translogg.RabKr - Translogg.SubTotalRab) - Translogg.Mva
                               ) * Translogg.Antall
            StLinje.VVareKost  = StLinje.VVareKost + 
                                 (Translogg.VVareKost * Translogg.Antall)
            StLinje.MvaVerdi   = StLinje.MvaVerdi + 
                                 (Translogg.Mva * Translogg.Antall)
            /* Posterer rabatt. */
            StLinje.AntRab        = StLinje.AntRab +
                                    (if (Translogg.RabKr + Translogg.SubTotalRab) <> 0
                                      then Translogg.Antall 
                                      else 0)
            StLinje.VerdiRabatt   = StLinje.VerdiRabatt +
                                    (if (Translogg.RabKr + Translogg.SubTotalRab) <> 0
                                      then (
                                            Translogg.Antall * (Translogg.RabKr - RabUMva(wMva%)) +
                                            Translogg.Antall * (Translogg.SubTotalRab - SRabUMva(wMva%))
                                           )

                                      else 0)
            .
          END.                                      
        WHEN 3 THEN 
          DO:
            ASSIGN  /* Kundereklamasjon */
                    /* Skal salget trekkes ned? - I tilfelle hvilken verdi skal benyttes? */
                StLinje.ReklAnt    = StLinje.ReklAnt    + Translogg.Antall  
                StLinje.ReklVerdi  = StLinje.ReklVerdi  + (Translogg.VVArekost * Translogg.Antall)
                /* Korrigerer rabatt. */
                StLinje.AntRab        = StLinje.AntRab +
                                        (IF Translogg.RabKr <> 0
                                          THEN Translogg.Antall 
                                          ELSE 0)
                StLinje.VerdiRabatt   = StLinje.VerdiRabatt +
                                        (IF Translogg.RabKr <> 0
                                          THEN Translogg.Antall * (Translogg.RabKr - RabUMva(wMva%))
                                          ELSE 0)
                /* Korrigerer salget. */
                StLinje.AntSolgt   = StLinje.AntSolgt   + Translogg.Antall  
                StLinje.VerdiSolgt = StLinje.VerdiSolgt + 
                                     (
                                      (Translogg.Pris - Translogg.RabKr) - 
                                      Translogg.Mva
                                     ) * Translogg.Antall
                StLinje.VVareKost  = StLinje.VVareKost + 
                                     (Translogg.VVareKost * Translogg.Antall)
                StLinje.MvaVerdi   = StLinje.MvaVerdi + 
                                   (Translogg.Mva * Translogg.Antall).
          END. 
          
        WHEN 10 THEN
            ASSIGN  /* Gjennkjøp */
              StLinje.GjenkjopAnt   = StLinje.GjenkjopAnt    + (Translogg.Antall * -1) /* Negativt antall i Translogg */  
              StLinje.GjenkjopVerdi = StLinje.GjenkjopVerdi  + 
                                      (
                                       (Translogg.Pris - Translogg.RabKr - Translogg.SubTotalRab) - Translogg.Mva
                                      ) * Translogg.Antall
              /* Korrigerer salget ved gjenkjøp */
              StLinje.AntSolgt   = StLinje.AntSolgt   + Translogg.Antall  
              StLinje.VerdiSolgt = StLinje.VerdiSolgt + 
                                   (
                                    (Translogg.Pris - Translogg.RabKr - Translogg.SubTotalRab) - Translogg.Mva
                                   ) * Translogg.Antall
              StLinje.VVareKost  = StLinje.VVareKost + 
                                   (Translogg.VVareKost * Translogg.Antall)
              StLinje.MvaVerdi   = StLinje.MvaVerdi + 
                                 (Translogg.Mva * Translogg.Antall).
        WHEN 11 THEN
            ASSIGN  /* Internt forbruk */
              StLinje.IntAnt   = StLinje.IntAnt    + Translogg.Antall  
              StLinje.IntVerdi = StLinje.IntVerdi  + 
                                 (Translogg.VVareKost * Translogg.Antall).
       END CASE.
    
    END. /* STATISTIKK_DEF */      

END. /* MEDTRANS */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-RabUMva) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION RabUMva Procedure 
FUNCTION RabUMva RETURNS DECIMAL
  ( wMva% AS DEC ) :
/*------------------------------------------------------------------------------
  Purpose:  Beregner RABATT - MVA(På rabatten). Dvs nettorabatt.
    Notes:  
------------------------------------------------------------------------------*/

  DEF VAR wWork AS DEC NO-UNDO.

  wWork = (TransLogg.RabKr) -
          ((TransLogg.RabKr) / (1 + (wMva% / 100))).
  IF wWork = ? THEN wWork = 0.

  RETURN wWork.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SRabUMva) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION SRabUMva Procedure 
FUNCTION SRabUMva RETURNS DECIMAL
  ( wMva% AS DEC ) :
/*------------------------------------------------------------------------------
  Purpose:  Beregner RABATT - MVA(På rabatten). Dvs nettorabatt.
    Notes:  
------------------------------------------------------------------------------*/

  DEF VAR wWork AS DEC NO-UNDO.

  wWork = (TransLogg.SubTotalRab) -
          ((TransLogg.SubTotalRab) / (1 + (wMva% / 100))).
  IF wWork = ? THEN wWork = 0.

  RETURN wWork.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

