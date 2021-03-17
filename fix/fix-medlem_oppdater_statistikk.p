&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
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

RUN lesTranslogg.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-lesTranslogg) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE lesTranslogg Procedure 
PROCEDURE lesTranslogg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

FOR EACH Translogg NO-LOCK:
    IF TransLogg.MedlemsNr > 0 THEN
        RUN MedlemsSalg.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-MedlemsSalg) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MedlemsSalg Procedure 
PROCEDURE MedlemsSalg :
/*------------------------------------------------------------------------------
  Purpose:     Posterer medlemssalg i MedTrans.
               Dato for første og siste kjøp settes også.
  Parameters:  Forutsetter at det er en Translogg post tilgjengelig
               hvor medlemsnummeret er <> 0.
               
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER bMedTrans    FOR MedTrans.
  DEF BUFFER bMedlemSaldo FOR MedlemSaldo.

  /* Sjekker at det er et gyldig medlem.   */
  /* Salg på medlemmet posteres uansett... */
  FIND Medlem NO-LOCK WHERE
      Medlem.MedlemsNr = TransLogg.MedlemsNr NO-ERROR.
  IF NOT AVAILABLE Medlem THEN
      RETURN "AVBRYT". /* Ingen medlem å postere på */
                                            
  /* Transaksjonen kan ha blitt oppdatert tidligere. */
  IF CAN-find(bMedTrans where
              bMedTrans.MedlemsNr = TransLogg.MedlemsNr AND
              bMedTrans.Butik     = TransLogg.Butik     AND
              bMedTrans.TransNr   = TransLogg.TransNr   AND
              bMedTrans.SeqNr     = TransLogg.SeqNr) THEN
    RETURN "AVBRYT".

  POSTERING-TRANS:
  DO FOR bMedTrans TRANSACTION:
    CREATE bMedTrans.
    ASSIGN
        bMedTrans.MedlemsNr      = TransLogg.MedlemsNr
        bMedTrans.Butik          = TransLogg.Butik
        bMedTrans.TransNr        = TransLogg.TransNr
        bMedTrans.SeqNr          = TransLogg.SeqNr
        .
    ASSIGN
      bMedTrans.BatchNr        = TransLogg.BatchNr
      bMedTrans.TTId           = TransLogg.TTId
      bMedTrans.TBId           = TransLogg.TBId
      bMedTrans.ArtikkelNr     = TransLogg.ArtikkelNr
      bMedTrans.LevNr          = TransLogg.LevNr
      bMedTrans.BongId         = TransLogg.BongId
      bMedTrans.BongLinjeNr    = TransLogg.BongLinjeNr
      bMedTrans.KassaNr        = TransLogg.KassaNr
      bMedTrans.Vg             = TransLogg.Vg
      bMedTrans.LopNr          = TransLogg.LopNr
      bMedTrans.Storl          = TransLogg.Storl
      bMedTrans.Antall         = TransLogg.Antall
      bMedTrans.Pris           = TransLogg.Pris
      bMedTrans.RabKr          = TransLogg.RabKr
      bMedTrans.Mva            = TransLogg.Mva
      bMedTrans.Dato           = TransLogg.Dato
      bMedTrans.Tid            = TransLogg.Tid
      bMedTrans.VVarekost      = TransLogg.VVarekost
      bMedTrans.SattVVareKost  = TransLogg.SattVVareKost
      bMedTrans.KortNr         = IF TransLogg.KortType = 3 /* Medlemskort */
                                   THEN TransLogg.KortNr
                                   ELSE ""
      bMedTrans.ForsNr         = TransLogg.ForsNr
      .

  END. /* POSTERING-TRANS */

  POSTERING-SALDO:
  DO FOR bMedlemSaldo WHILE TRUE TRANSACTION:
    FIND bMedlemSaldo EXCLUSIVE-LOCK where
        bMedlemSaldo.MedlemsNr = TransLogg.MedlemsNr AND
        bMedlemSaldo.Butik     = TransLogg.Butik
        NO-ERROR NO-WAIT.
    /* Posten holdes av en annen, vi forsøker igjen. */
    IF LOCKED bMedlemSaldo THEN
        NEXT POSTERING-SALDO.
    /* Oppretter ny post. */
    IF NOT AVAILABLE bMedlemSaldo THEN
    DO:
        CREATE bMedlemSaldo.
        ASSIGN
            bMedlemSaldo.MedlemsNr = TransLogg.MedlemsNr
            bMedlemSaldo.Butik     = TransLogg.Butik
            .
    END.
    /* Posterer 1.gangs kjøp. */
    IF bMedlemSaldo.ForsteDato = ? THEN
      ASSIGN
        bMedlemSaldo.ForsteDato = TransLogg.Dato
        bMedlemSaldo.ForsteTid  = TransLogg.Tid
        .
    /* Posterer siste gangs kjøp. */
    ASSIGN
        bMedlemSaldo.DatoSiste  = TransLogg.Dato
        bMedlemSaldo.SisteTid   = TransLogg.Tid
        .
    /* Oppdaterer saldo. */
    ASSIGN
        bMedlemSaldo.Saldo      = bMedlemSaldo.Saldo      + 
                                  ((TransLogg.Pris - TransLogg.RabKr) * TransLogg.Antall) 
        bMedlemSaldo.TotaltKjop = bMedlemSaldo.TotaltKjop + 
                                  ((TransLogg.Pris - TransLogg.RabKr) * TransLogg.Antall)
        .

    LEAVE POSTERING-SALDO.    
  END. /* POSTERING-SALDO */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PosterStatistikk) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PosterStatistikk Procedure 
PROCEDURE PosterStatistikk :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def var wDataObjekt as char no-undo.

  def buffer bufStLinje for StLinje.

    /* Oppdaterer statistikker */
    STATISTIKK_DEF:
    for each stdef no-lock
      break by StDef.StTypeId:

      /* Utkoblede statistikker */
      IF CAN-DO("SELGER-ART,KASS-ART",StDef.StTypeId) THEN
          NEXT.
    
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
              wWork = month(TransLogg.Dato).
            when "UKE"   then /* Opptil 53 linjer pr. år */
              do:
                run weeknum.p (TransLogg.Dato, output wWork).
                wWork = int(substring(string(wWork,"999999"),5,2)).
              end.
            when "DAG"   then /* Opptil 366 dager pr. år. */
              wWork = (TransLogg.Dato + 1) - date(01,01,year(TransLogg.Dato)).
          end.
        end.
      /* Fri periode         */
      else do:
        find first PerLin no-lock where
          PerLin.PerId   =  Periode.PerId  and
          PerLin.FraDato <= TransLogg.Dato and 
          PerLin.TilDato >= TransLogg.Dato no-error.
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
        when "ARTIKKEL" then          
          wDataObjekt = string(TransLogg.ArtikkelNr,"9999999999999").
        when "HOVEDGR"  then
          do:
            FIND VarGr OF ArtBas NO-LOCK NO-ERROR.
            IF NOT AVAILABLE VarGr THEN NEXT.
            IF CAN-FIND(HuvGr OF VarGr) THEN
              wDataObjekt = string(VarGr.Hg,"9999").
            else do:
              assign TransLogg.FeilKode = 9. /* Ukjent hovedgruppe på atikkelen. */
              run LoggFeilITrans (input TransLogg.FeilKode).
              return "UNDO".
            end.
          end.
        when "VAREGR"   then
          wDataObjekt = string(TransLogg.Vg,"999999").
        when "AVDELING"   then
        DO:
            FIND VarGr OF ArtBas NO-LOCK NO-ERROR.
            IF NOT AVAILABLE VarGr THEN NEXT.
            find HuvGr OF VarGr no-lock no-error.
            IF AVAILABLE HuvGr THEN
                FIND Avdeling NO-LOCK WHERE
                  Avdeling.AvdelingNr = HuvGr.AvdelingNr NO-ERROR.
            IF AVAILABLE Avdeling THEN
                wDataObjekt = string(Avdeling.AvdelingNr,"9999").
            else do:
              assign TransLogg.FeilKode = 15. /* Ukjent Avdeling på atikkelens hovedgruppe. */
              run LoggFeilITrans (input TransLogg.FeilKode).
              return "UNDO".
            end.
        END.
        when "LEVERAN"  then
          wDataObjekt = string(ArtBas.LevNr,"999999").
        when "LEVERAN-VG"  then
          wDataObjekt = string(ArtBas.LevNr,"999999") + CHR(1) + 
                        string(TransLogg.Vg,"999999").
        when "BUTSTAT" then
          wDataObjekt = string(TransLogg.Butik,"999999").
        when "KUNDSTAT" then
          wDataObjekt = string(TransLogg.KundNr,"9999999999999").
        when "SELGERSTAT" then
          wDataObjekt = string(TransLogg.ForsNr,"9999999999999").
        when "KASS-VG" then
          wDataObjekt = string(TransLogg.ForsNr,"9999999999999") + CHR(1) + 
                        string(TransLogg.Vg,"999999").
        when "KASS-ART" then
          wDataObjekt = string(TransLogg.ForsNr,"9999999999999") + CHR(1) + 
                        string(TransLogg.ArtikkelNr,"9999999999999").
        when "SELGER" then
          wDataObjekt = string(TransLogg.SelgerNr,"9999999999999").
        when "SELGER-VG" then
          wDataObjekt = string(TransLogg.SelgerNr,"9999999999999") + CHR(1) + 
                        string(TransLogg.Vg,"999999").
        when "SELGER-ART" then
          wDataObjekt = string(TransLogg.SelgerNr,"9999999999999") + CHR(1) + 
                        string(TransLogg.ArtikkelNr,"9999999999999").
        when "MEDLEM" then
          wDataObjekt = IF TransLogg.MedlemsNr <> 0
                          THEN string(TransLogg.MedlemsNr,"9999999999999")
                          ELSE "9999999999999".
        when "MEDLEMTOT" then
          wDataObjekt = "9999999999999". /* Statistikk totalt for kundeklubb. */
        when "KAMPANJE" then
          wDataObjekt = IF TransLogg.KampId > 0
                          THEN string(TransLogg.KampId,"9999999999999")
                          ELSE "".
        when "KAMPART" then
          wDataObjekt = IF (TransLogg.KampId > 0 AND Translogg.ArtikkelNr > 0)
                          THEN string(TransLogg.KampId,"9999999999999") + CHR(1) + 
                               string(Translogg.KampTilbId,"999999999") + CHR(1) + 
                               string(TransLogg.ArtikkelNr,"9999999999999")
                          ELSE "".
        when "KAMPTILB" then
          wDataObjekt = IF (TransLogg.KampId > 0 AND Translogg.KampTilbId > 0)
                          THEN string(TransLogg.KampId,"9999999999999") + CHR(1) + 
                               string(Translogg.KampTilbId,"999999999")
                          ELSE "".
      end case.

      /* Blanktt objekt skal ikke oppdateres. */
      IF wDataObjekt = "" THEN
          NEXT STATISTIKK_DEF.

      /* Medlemsstatistikk skal bare oppdateres hvis det er et gyldig medlemsnummer. */
      IF StDef.StTypeId = "MEDLEM" THEN
      DO:
        IF TransLogg.MedlemsNr = 0 THEN
          NEXT STATISTIKK_DEF.
      END.
      
      /* Medlemsklubbstatistikk skal bare oppdateres hvis det er et gyldig medlemsnummer. */
      IF StDef.StTypeId = "MEDLEMTOT" THEN
      DO:
        IF TransLogg.MedlemsNr = 0 THEN
          NEXT STATISTIKK_DEF.
      END.
      
      /* Kundestatistikk skal bare oppdateres hvis det er et gyldig kundenummer. */
      IF StDef.StTypeId = "KUNDSTAT" THEN
      DO:
        IF TransLogg.KundNr = 0 THEN
          NEXT STATISTIKK_DEF.
      END.
      
      /* Selgerstatiatikk skal bare oppdateres hvis det er et gyldig selgernr. */
      IF StDef.StTypeId = "SELGER" THEN
      DO:
        IF TransLogg.SelgerNr = 0 THEN
          NEXT STATISTIKK_DEF.
      END.

      IF StDef.StTypeId = "SELGER-VG" THEN
      DO:
        IF TransLogg.SelgerNr = 0 THEN
          NEXT STATISTIKK_DEF.
        IF TransLogg.Vg = 0 THEN
          NEXT STATISTIKK_DEF.
      END.

      /* Oppdaterer statistikklinjen. */
      find StLinje exclusive-lock where
        StLinje.StTypeId   = StDef.StTypeId and
        StLinje.PerId      = Periode.PerId and
        StLinje.DataObjekt = wDataObjekt and
        StLinje.Diverse    = "" and
        StLinje.Butik      = TransLogg.Butik and
        StLinje.Aar        = year(TransLogg.Dato) and
        StLinje.PerLinNr   = int(wWork) no-error.
      if not available StLinje then
        do:
          create StLinje.
          assign
            StLinje.StTypeId   = StDef.StTypeId 
            StLinje.PerId      = Periode.PerId 
            StLinje.DataObjekt = wDataObjekt 
            StLinje.Diverse    = ""
            StLinje.Butik      = TransLogg.Butik
            StLinje.Aar        = year(TransLogg.Dato)
            StLinje.PerLinNr   = int(wWork). 

        end.

      /* Henter Lager posten */
      find Lager no-lock where
        Lager.ArtikkelNr = TransLogg.ArtikkelNr and
        Lager.Butik      = TransLogg.Butik no-error. /* Denne skal finnes */
      if not available Lager then
        return "UNDO".

      /* Oppdaterer statistikkfeltene */
      case TransLogg.TTId:      
        when 1 then
          do:
          assign /* Varesalg */
            StLinje.AntSolgt   = StLinje.AntSolgt   + TransLogg.Antall  
            StLinje.VerdiSolgt = StLinje.VerdiSolgt + 
                               (
                                (TransLogg.Pris - Translogg.RabKr) - 
                                TransLogg.Mva
                               ) * TransLogg.Antall
            StLinje.VVareKost  = StLinje.VVareKost + 
                                 (TransLogg.VVareKost * TransLogg.Antall)
            StLinje.MvaVerdi   = StLinje.MvaVerdi + 
                                 (TransLogg.Mva * TransLogg.Antall)
            /* Posterer rabatt. */
            StLinje.AntRab        = StLinje.AntRab +
                                    (if TransLogg.RabKr <> 0
                                      then TransLogg.Antall 
                                      else 0)
            StLinje.VerdiRabatt   = StLinje.VerdiRabatt +
                                    (if TransLogg.RabKr <> 0
                                      then TransLogg.Antall * (Translogg.RabKr - RabUMva(wMva%))
                                      else 0).
/*
message "Rabatt:" RabUMva(wMva%) skip
        "wMva%:" wMva% skip
        "Translogg.RabKr:" TransLogg.RabKr skip
        "Translogg.AntRab:" TransLogg.Antall skip
        "StLinje.VerdiRabatt:" StLinje.VerdiRabatt
view-as alert-box.                                              
*/
          end.                                      
        when 2 then
            assign  /* Brekkasje */
              StLinje.BrekkAnt   = StLinje.BrekkAnt   + TransLogg.Antall  
              StLinje.BrekkVerdi = StLinje.BrekkVerdi + 
                                   (wVareKost * TransLogg.Antall).
        when 3 then 
          do:
            assign  /* Kundereklamasjon */
              StLinje.ReklAnt    = StLinje.ReklAnt    + TransLogg.Antall  
              StLinje.ReklVerdi  = StLinje.ReklVerdi  + 
                                   (wVareKost * TransLogg.Antall)  
              /* Korrigerer rabatt. */
              StLinje.AntRab        = StLinje.AntRab +
                                      (if TransLogg.RabKr <> 0
                                        then TransLogg.Antall 
                                        else 0)
              StLinje.VerdiRabatt   = StLinje.VerdiRabatt +
                                      (if TransLogg.RabKr <> 0
                                        then TransLogg.Antall * (Translogg.RabKr - RabUMva(wMva%))
                                        else 0)
              /* Korrigerer salget. */
              StLinje.AntSolgt   = StLinje.AntSolgt   + TransLogg.Antall  
              StLinje.VerdiSolgt = StLinje.VerdiSolgt + 
                                   (
                                    (TransLogg.Pris - Translogg.RabKr) - 
                                    TransLogg.Mva
                                   ) * TransLogg.Antall
              StLinje.VVareKost  = StLinje.VVareKost + 
                                   (TransLogg.VVareKost * TransLogg.Antall)
              StLinje.MvaVerdi   = StLinje.MvaVerdi + 
                                 (TransLogg.Mva * TransLogg.Antall).
          end. 
        when 4 then
            assign  /* Lagerreklamasjon */
              StLinje.ReklLAnt   = StLinje.ReklLAnt   + TransLogg.Antall  
              StLinje.ReklLVerdi = StLinje.ReklLVerdi + 
                                   (wVareKost * TransLogg.Antall).
        when 5 then
          do: /* Varekjøp m/vektet vareverdi */
            assign  
              wWork  = Lager.Lagant   * wVareKost   /* Gammel lagerverdi */
              wWork2 = TransLogg.Pris * TransLogg.Antall. /* Verdi av innkjøp  */
            
            assign          
              StLinje.KjopAnt   = StLinje.KjopAnt    + TransLogg.Antall  
              StLinje.KjopVerdi = StLinje.KjopVerdi  + wWork2.
          end.
        when 6 then
          do: /* Overføring */                
            /* Henter statistikk for mottagende butikk.            */
            /* Litt spesiell håndtering må til for å kunne postere */
            /* overføringer i butikkstatistikken.                  */
            find bufStLinje exclusive-lock where
              bufStLinje.StTypeId   = StDef.StTypeId and
              bufStLinje.PerId      = Periode.PerId and
              bufStLinje.DataObjekt = (IF StDef.StTypeId = "BUTSTAT"
                                        THEN STRING(TransLogg.OvButik,"999999")
                                        ELSE wDataObjekt) and
              bufStLinje.Diverse    = "" and
              bufStLinje.Butik      = TransLogg.OvButik and
              bufStLinje.Aar        = year(TransLogg.Dato) and
              bufStLinje.PerLinNr   = int(wWork) no-error.
            if not available bufStLinje then
              do:
                create bufStLinje.
                assign
                  bufStLinje.StTypeId   = StDef.StTypeId 
                  bufStLinje.PerId      = Periode.PerId 
                  bufStLinje.DataObjekt = (IF StDef.StTypeId = "BUTSTAT"
                                            THEN STRING(TransLogg.OvButik,"999999")
                                            ELSE wDataObjekt)  
                  bufStLinje.Diverse    = ""
                  bufStLinje.Butik      = TransLogg.OvButik
                  bufStLinje.Aar        = year(TransLogg.Dato)
                  bufStLinje.PerLinNr   = int(wWork).      
              end.
            
            assign  /* Trekker ned på FRA butikk.            */
                    /* Posteringer skjer med vektet varekost i fra butikken. */
              StLinje.OvAnt   = StLinje.OvAnt      - TransLogg.Antall  
              StLinje.OvVerdi = StLinje.OvVerdi    - 
                                (wVareKost * TransLogg.Antall).
   
            /* Henter Lager posten til mottagende butikk */
            /* Forutsetter her at VVarekost er satt, fordi varen er lagerstyrt. */
            find bufLager no-lock where
              bufLager.ArtikkelNr = TransLogg.ArtikkelNr and
              bufLager.Butik      = TransLogg.Butik no-error. /* Denne skal finnes */
            if not available bufLager then
              return "UNDO".
              
            assign  /* Posterer i TIL (mottagende) butikk.  */
                    /* Posteringer skjer med vektet varekost i mottagende butikk. */
              bufStLinje.OvAnt     = bufStLinje.OvAnt      + TransLogg.Antall  
              bufStLinje.OvVerdi   = bufStLinje.OvVerdi    + 
                                    (TransLogg.VVareKost * TransLogg.Antall).
          end.
          
        when 7 then
            assign  /* Lagerjustering */
              StLinje.JustAnt   = StLinje.JustAnt   + TransLogg.Antall  
              StLinje.JustVerdi = StLinje.JustVerdi + 
                                  (TransLogg.VVareKost * TransLogg.Antall).
        when 8 then
          do:
            find bufLager no-lock where
              bufLager.ArtikkelNr = TransLogg.ArtikkelNr and
              bufLager.Butik      = TransLogg.Butik no-error. /* Denne skal finnes */
            assign  /* Nedskriving */
              /* Ingen endring i lagerantall. Kun VVarekost. */
              StLinje.NedAnt    = StLinje.NedAnt   + ((if available bufLager 
                                                        then Lager.LagAnt  
                                                        else 0) *
                                                       (if TransLogg.Antall < 0
                                                         then -1
                                                         else 1))
              StLinje.NedVerdi  = StLinje.NedVerdi + 
                                  (TransLogg.Pris * ((if available bufLager 
                                                        then Lager.LagAnt  
                                                        else 0)) *
                                                       (if TransLogg.Antall < 0
                                                         then -1
                                                         else 1)).
          end.
        when 9 then
            assign  /* Svinn */
              StLinje.SvinnAnt   = StLinje.SvinnAnt   + TransLogg.Antall  
              StLinje.SvinnVerdi = StLinje.SvinnVerdi + 
                                   (TransLogg.Pris * TransLogg.Antall).
        when 10 then
            assign  /* Gjennkjøp */
              StLinje.GjenkjopAnt   = StLinje.GjenkjopAnt    + (TransLogg.Antall * -1) /* Negativt antall i TransLogg */  
              StLinje.GjenkjopVerdi = StLinje.GjenkjopVerdi  + 
                                      (
                                       (TransLogg.Pris - Translogg.RabKr) - 
                                       TransLogg.Mva
                                      ) * TransLogg.Antall
              /* Korrigerer rabatt ved gjennkjøp. */
              StLinje.AntRab        = StLinje.AntRab +
                                      (if TransLogg.RabKr <> 0
                                        then TransLogg.Antall 
                                        else 0)
              StLinje.VerdiRabatt   = StLinje.VerdiRabatt +
                                      (if TransLogg.RabKr <> 0
                                        then TransLogg.Antall * (Translogg.RabKr - RabUMva(wMva%))
                                        else 0)
              /* Korrigerer salget ved gjenkjøp */
              StLinje.AntSolgt   = StLinje.AntSolgt   + TransLogg.Antall  
              StLinje.VerdiSolgt = StLinje.VerdiSolgt + 
                                   (
                                    (TransLogg.Pris - Translogg.RabKr) - 
                                    TransLogg.Mva
                                   ) * TransLogg.Antall
              StLinje.VVareKost  = StLinje.VVareKost + 
                                   (TransLogg.VVareKost * TransLogg.Antall)
              StLinje.MvaVerdi   = StLinje.MvaVerdi + 
                                 (TransLogg.Mva * TransLogg.Antall).
        when 11 then
            assign  /* Internt forbruk */
              StLinje.IntAnt   = StLinje.IntAnt    + TransLogg.Antall  
              StLinje.IntVerdi = StLinje.IntVerdi  + 
                                 (TransLogg.VVareKost * TransLogg.Antall).
       end case.
    
    end. /* STATISTIKK_DEF */      

  return "OK".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

