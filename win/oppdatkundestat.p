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

DEF INPUT PARAMETER plKundeNr LIKE Kunde.KundeNr NO-UNDO.

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
  ( wMva% as dec )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SRabUMva) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD SRabUMva Procedure 
FUNCTION SRabUMva RETURNS DECIMAL
  ( wMva% as dec )  FORWARD.

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

def var wDataObjekt as char no-undo.
DEF VAR wWork       AS INT  NO-UNDO.
DEF VAR wVareKost   AS DEC  NO-UNDO.
DEF VAR wWork2      AS DEC  NO-UNDO.
DEF VAR wMva%       AS DEC  NO-UNDO.
DEF VAR wDefMva%    AS DEC  NO-UNDO.

def buffer bufStLinje for StLinje.
DEF BUFFER bufLager   FOR Lager.

/* Default MVA% */
{syspara.i 2 1 4 wDefMva% DEC}

FIND Kunde NO-LOCK WHERE
    Kunde.KundeNr = plKundeNr NO-ERROR.
IF NOT AVAILABLE Kunde THEN
    RETURN "AVBRYT".

/* Tar bort statistikk */
FOR EACH stLinje EXCLUSIVE where
         StLinje.StType = "KUNDSTAT" and
         StLinje.DataObjekt = string(Kunde.KundeNr,"9999999999999"):

   DELETE stLinje.
END.

/* Bygger opp statistikken pånytt. */
KundeTrans:
FOR EACH KundeTrans OF Kunde NO-LOCK:
    /* Henter Moms% */
    FIND VarGr NO-LOCK WHERE 
        VarGr.Vg = KundeTrans.Vg NO-ERROR.
    if AVAILABLE VarGr then
      DO:
        FIND Moms OF VarGr NO-LOCK NO-ERROR.
        if AVAILABLE Moms then
          wMva% = Moms.MomsProc.
        else
          wMva% = 0.
      END.
    ELSE
      wMva% = 0.
    /* Bruker default MVA hvis den ikke er satt. */
    if NOT AVAILABLE VarGr AND wMva% = 0 then
      wMva% = wDefMva%.

    /* Oppdaterer statistikker */
    STATISTIKK_DEF:
    for each stdef NO-LOCK WHERE
        StDef.StType = "KUNDSTAT"
      break by StDef.StTypeId:
    
      /* Henter og kontrollerer perioden. */
      find Periode no-lock where
       Periode.PerId = StDef.PerId no-error.
      if not available Periode then 
        return "NEXT".
    
      /* Henter PeriodeLinje */
      /* Faste perioder.     */
      if Periode.Fast then
        do:
          case Periode.PerId:
            when "AAR"   then /* Kun en linje i et år. */
              wWork = 1. 
            when "MANED" then /* 12 Linjer pr. år */
              wWork = month(KundeTrans.Dato).
            when "UKE"   then /* Opptil 53 linjer pr. år */
              do:
                run weeknum.p (KundeTrans.Dato, output wWork).
                wWork = int(substring(string(wWork,"999999"),5,2)).
              end.
            when "DAG"   then /* Opptil 366 dager pr. år. */
              wWork = (KundeTrans.Dato + 1) - date(01,01,year(KundeTrans.Dato)).
          end.
        end.
      /* Fri periode         */
      else do:
        find first PerLin no-lock where
          PerLin.PerId   =  Periode.PerId  and
          PerLin.FraDato <= KundeTrans.Dato and 
          PerLin.TilDato >= KundeTrans.Dato no-error.
        if not available PerLin then
          find last PerLin where
            PerLin.PerId = Periode.PerId no-error.
        if available PerLin then
          wWork = PerLin.PerLinNr.
        else
          wWork = ?. /* Her er ingenting mer å gjøre. */
      end.
      /* Håndtering av ukjente frie statistikkdefineisjoner. */
      if wWork = 0 or 
         wWork = ? then 
        next STATISTIKK_DEF.
      
      /* Henter/oppretter og stempler statistikkhode */      
      find StHode exclusive-lock where
        StHode.StTypeId = StDef.StTypeId and
        StHode.PerId    = Periode.PerId no-error.
      if not available StHode then
        do:
          create StHode.
          assign
            StHode.StTypeId = StDef.StTypeId
            StHode.PerId    = Periode.PerId.
          assign
            StHode.RegistrertDato = today
            StHode.RegistrertTid  = time
            StHode.RegistrertAv   = userid("dictdb").            
        end.
      assign
        StHode.EDato    = today
        StHode.ETid     = time
        StHode.BrukerId = userid("dictdb").            
        
      /* Setter dataobjekt */
      case StDef.StTypeId:
        when "KUNDSTAT" then
          wDataObjekt = IF KundeTrans.KundeNr <> 0
                          THEN string(KundeTrans.KundeNr,"9999999999999")
                          ELSE "9999999999999".
      end case.

      /* Kundesstatistikk skal bare oppdateres hvis det er et gyldig Kundesnummer. */
      IF StDef.StTypeId = "KUNDSTAT" THEN
      DO:
        IF KundeTrans.KundeNr = 0 THEN
          NEXT STATISTIKK_DEF.
      END.
      
      /* Oppdaterer statistikklinjen. */
      find StLinje exclusive-lock where
        StLinje.StTypeId   = StDef.StTypeId and
        StLinje.PerId      = Periode.PerId and
        StLinje.DataObjekt = wDataObjekt and
        StLinje.Diverse    = "" and
        StLinje.Butik      = KundeTrans.Butik and
        StLinje.Aar        = year(KundeTrans.Dato) and
        StLinje.PerLinNr   = int(wWork) no-error.
      if not available StLinje then
        do:
          create StLinje.
          assign
            StLinje.StTypeId   = StDef.StTypeId 
            StLinje.PerId      = Periode.PerId 
            StLinje.DataObjekt = wDataObjekt 
            StLinje.Diverse    = ""
            StLinje.Butik      = KundeTrans.Butik
            StLinje.Aar        = year(KundeTrans.Dato)
            StLinje.PerLinNr   = int(wWork).      
        end.

      /* Henter Lager posten */
      find Lager no-lock where
        Lager.ArtikkelNr = KundeTrans.ArtikkelNr and
        Lager.Butik      = KundeTrans.Butik no-error. /* Denne skal finnes */
      if not available Lager then
        return "UNDO".

      /* Oppdaterer statistikkfeltene */
      case KundeTrans.TTId:      
        when 1 then
          do:
          assign /* Varesalg */
            StLinje.AntSolgt   = StLinje.AntSolgt   + KundeTrans.Antall  
            StLinje.VerdiSolgt = StLinje.VerdiSolgt + 
                               (
                                (KundeTrans.Pris - KundeTrans.RabKr - KundeTrans.SubTotalRab) - KundeTrans.Mva
                               ) * KundeTrans.Antall
            StLinje.VVareKost  = StLinje.VVareKost + 
                                 (KundeTrans.VVareKost * KundeTrans.Antall)
            StLinje.MvaVerdi   = StLinje.MvaVerdi + 
                                 (KundeTrans.Mva * KundeTrans.Antall)
            /* Posterer rabatt. */
            StLinje.AntRab        = StLinje.AntRab +
                                    (if (KundeTrans.RabKr + KundeTrans.SubTotalRab) <> 0
                                      then KundeTrans.Antall 
                                      else 0)
            StLinje.VerdiRabatt   = StLinje.VerdiRabatt +
                                    (if (KundeTrans.RabKr + KundeTrans.SubTotalRab) <> 0
                                      then (
                                            KundeTrans.Antall * (KundeTrans.RabKr - RabUMva(wMva%)) +
                                            KundeTrans.Antall * (KundeTrans.SubTotalRab - SRabUMva(wMva%))
                                           )

                                      else 0)
            .
          end.                                      
        when 2 then
            assign  /* Brekkasje */
              StLinje.BrekkAnt   = StLinje.BrekkAnt   + KundeTrans.Antall  
              StLinje.BrekkVerdi = StLinje.BrekkVerdi + 
                                   (wVareKost * KundeTrans.Antall).
        when 3 then 
          do:
            assign  /* Kundereklamasjon */
                    /* Skal salget trekkes ned? - I tilfelle hvilken verdi skal benyttes? */
              StLinje.ReklAnt    = StLinje.ReklAnt    + KundeTrans.Antall  
              StLinje.ReklVerdi  = StLinje.ReklVerdi  + 
                                   (wVareKost * KundeTrans.Antall).           
          end. 
        when 4 then
            assign  /* Lagerreklamasjon */
              StLinje.ReklLAnt   = StLinje.ReklLAnt   + KundeTrans.Antall  
              StLinje.ReklLVerdi = StLinje.ReklLVerdi + 
                                   (wVareKost * KundeTrans.Antall).
        when 5 then
          do: /* Varekjøp m/vektet vareverdi */
            assign  
              wWork  = Lager.Lagant   * wVareKost   /* Gammel lagerverdi */
              wWork2 = KundeTrans.Pris * KundeTrans.Antall. /* Verdi av innkjøp  */
            
            assign          
              StLinje.KjopAnt   = StLinje.KjopAnt    + KundeTrans.Antall  
              StLinje.KjopVerdi = StLinje.KjopVerdi  + wWork2.
          end.
          
        when 7 then
            assign  /* Lagerjustering */
              StLinje.JustAnt   = StLinje.JustAnt   + KundeTrans.Antall  
              StLinje.JustVerdi = StLinje.JustVerdi + 
                                  (KundeTrans.VVareKost * KundeTrans.Antall)
              .
        when 8 then
          do:
            find bufLager no-lock where
              bufLager.ArtikkelNr = KundeTrans.ArtikkelNr and
              bufLager.Butik      = KundeTrans.Butik no-error. /* Denne skal finnes */
            assign  /* Nedskriving */
                    /* Ingen endring i lagerantall. Kun VVarekost. */
              StLinje.NedAnt    = StLinje.NedAnt   + ((if available bufLager 
                                                        then Lager.LagAnt  
                                                        else 0) *
                                                       (if KundeTrans.Antall < 0
                                                         then -1
                                                         else 1))
              StLinje.NedVerdi  = StLinje.NedVerdi + 
                                  (KundeTrans.Pris * ((if available bufLager 
                                                        then Lager.LagAnt  
                                                        else 0)) *
                                                       (if KundeTrans.Antall < 0
                                                         then -1
                                                         else 1)).
          end.
        when 9 then
            assign  /* Svinn */
              StLinje.SvinnAnt   = StLinje.SvinnAnt   + KundeTrans.Antall  
              StLinje.SvinnVerdi = StLinje.SvinnVerdi + 
                                   (KundeTrans.Pris * KundeTrans.Antall).
        when 10 then
            assign  /* Gjennkjøp */
              StLinje.GjenkjopAnt   = StLinje.GjenkjopAnt    + (KundeTrans.Antall * -1) /* Negativt antall i KundeTrans */  
              StLinje.GjenkjopVerdi = StLinje.GjenkjopVerdi  + 
                                      (
                                       (KundeTrans.Pris - KundeTrans.RabKr - KundeTrans.SubTotalRab) - KundeTrans.Mva
                                      ) * KundeTrans.Antall
              /* Korrigerer rabatt ved gjennkjøp. */
              StLinje.AntRab        = Lager.AntRab +
                                      (if (KundeTrans.RabKr + KundeTrans.SubTotalRab) <> 0
                                        then KundeTrans.Antall 
                                        else 0)
              StLinje.VerdiRabatt   = Lager.VerdiRabatt +
                                      (if (KundeTrans.RabKr + KundeTrans.SubTotalRab) <> 0
                                        then (
                                               KundeTrans.Antall * (KundeTrans.RabKr - RabUMva(wMva%)) + 
                                               KundeTrans.Antall * (KundeTrans.SubTotalRab - SRabUMva(wMva%)) 
                                             )
                                        else 0)
              /* Korrigerer salget ved gjenkjøp */
              StLinje.AntSolgt   = StLinje.AntSolgt   + KundeTrans.Antall  
              StLinje.VerdiSolgt = StLinje.VerdiSolgt + 
                                   (
                                    (KundeTrans.Pris - KundeTrans.RabKr - KundeTrans.SubTotalRab) - KundeTrans.Mva
                                   ) * KundeTrans.Antall
              StLinje.VVareKost  = StLinje.VVareKost + 
                                   (KundeTrans.VVareKost * KundeTrans.Antall)
              StLinje.MvaVerdi   = StLinje.MvaVerdi + 
                                 (KundeTrans.Mva * KundeTrans.Antall).
        when 11 then
            assign  /* Internt forbruk */
              StLinje.IntAnt   = StLinje.IntAnt    + KundeTrans.Antall  
              StLinje.IntVerdi = StLinje.IntVerdi  + 
                                 (KundeTrans.VVareKost * KundeTrans.Antall).
       end case.
    
    end. /* STATISTIKK_DEF */      

END. /* KundeTrans */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-RabUMva) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION RabUMva Procedure 
FUNCTION RabUMva RETURNS DECIMAL
  ( wMva% as dec ) :
/*------------------------------------------------------------------------------
  Purpose:  Beregner RABATT - MVA(På rabatten). Dvs nettorabatt.
    Notes:  
------------------------------------------------------------------------------*/

  DEF VAR wWork as DEC NO-UNDO.

  wWork = (KundeTrans.RabKr) -
          ((KundeTrans.RabKr) / (1 + (wMva% / 100))).
  if wWork = ? THEN wWork = 0.

  RETURN wWork.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SRabUMva) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION SRabUMva Procedure 
FUNCTION SRabUMva RETURNS DECIMAL
  ( wMva% as dec ) :
/*------------------------------------------------------------------------------
  Purpose:  Beregner RABATT - MVA(På rabatten). Dvs nettorabatt.
    Notes:  
------------------------------------------------------------------------------*/

  DEF VAR wWork as DEC NO-UNDO.

  wWork = (KundeTrans.SubTotalRab) -
          ((KundeTrans.SubTotalRab) / (1 + (wMva% / 100))).
  if wWork = ? THEN wWork = 0.

  RETURN wWork.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

