CURRENT-WINDOW:WIDTH = 250.

DEF VAR h_PrisKo         AS HANDLE NO-UNDO.


FUNCTION RabUMva RETURNS DECIMAL
  ( wMva% as dec ) :
/*------------------------------------------------------------------------------
  Purpose:  Beregner RABATT - MVA(På rabatten). Dvs nettorabatt.
    Notes:  
------------------------------------------------------------------------------*/

  DEF VAR wWork as DEC NO-UNDO.

  wWork = (TransLogg.RabKr) -
          ((TransLogg.RabKr) / (1 + (wMva% / 100))).
  if wWork = ? THEN wWork = 0.

  RETURN wWork.
END FUNCTION.

FORM 
    WITH FRAME B DOWN WIDTH 250.

IF NOT VALID-HANDLE(h_PrisKo) THEN
    RUN PrisKo.p PERSISTENT SET h_PrisKo.

MAINLOOP:
FOR EACH BongHode WHERE
  BongHode.Medlemskort > "":
    
  if BongHode.MedlemsNr = 0 then next MAINLOOP.
  if BongHode.Dato < 01/01/2008 then next MAINLOOP.

  IF BongHode.MedlemsKort <> "" then
      RUN ValiderKort (BongHode.MedlemsKort,"MEDLEM").
      
    
    DISPLAY
        BongHode.KundeKort
        BongHode.KundeNr
        BongHode.MedlemsKort
        BongHode.MedlemsNr
        BongHode.BongNR
        BongHode.Dato
        WITH FRAME B WIDTH 250.
    DOWN WITH FRAME B.

    /* Logge medlemssalg. */
    IF BongHode.MedlemsNr <> 0 OR BongHode.Medlemskort <> "" THEN
      RUN Medlemssalg. 

    /* Leser transaksjonene */
    FOR EACH BongLinje  NO-LOCK where
        BongLinje.B_Id     = BongHode.B_Id AND
        BongLinje.Makulert = FALSE AND
        CAN-FIND(ArtBas WHERE
                 ArtBas.ArtikkelNr = dec(BongLinje.ArtikkelNr)) AND
        dec(BongLinje.ArtikkelNr) > 0:

        /* Transer */
        IF CAN-DO("001,003,010",STRING(BongLinje.TTId,"999")) THEN
        DO:
            FIND TransLogg NO-LOCK WHERE
                TransLogg.Butik = BongHode.ButikkNr AND
                TransLogg.TransNr = BongLinje.TransNr AND
                TransLogg.SeqNr   = BongLinje.SeqNr NO-ERROR.
            PAUSE 0.
            DISPLAY
                BongLinje.Makulert
                BongLinje.ArtikkelNr
                BongLinje.TTId
                BongLinje.Antall
                WITH FRAME B.
            IF AVAILABLE TransLogg THEN
            DO:
                PAUSE 0.
                DISPLAY
                    TransLogg.TransNr
                    TransLogg.ArtikkelNr
                    TransLogg.Antall
                    WITH FRAME B.
                RUN PosterStatistikk.
            END.
            DOWN WITH FRAME b.

        END.
    END.
END.

IF VALID-HANDLE(h_PrisKo) THEN
    DELETE PROCEDURE h_PrisKo.

/* ------------------------------------------------------------------------------ */
PROCEDURE ValiderKort:
  DEF INPUT PARAMETER pcKort     AS CHAR NO-UNDO.
  DEF INPUT PARAMETER pcType     AS CHAR NO-UNDO.

  DEF VAR wRecid     AS RECID NO-UNDO.
  DEF VAR piKortType AS INT   NO-UNDO.

  ASSIGN
      piKortType = IF pctype = "MEDLEM" 
                     THEN 3
                   ELSE IF pcType = "KUNDE"
                       THEN 2
                       ELSE 1.
      .
      
  /* Påfører KortType, Medlemsnummer og kundenummer. */
  IF piKortType >= 2 AND piKortType <= 3 THEN
  DO:
      RUN sjekkmedlem.p (INPUT  pcKort,
                         INPUT  BongHode.ButikkNr,
                         INPUT  BongHode.KasseNr,
                         INPUT  BongHode.Dato,
                         INPUT  piKortType,
                         OUTPUT BongHode.MedlemsNr,
                         OUTPUT BongHode.MedlemNavn,
                         OUTPUT BongHode.KundeNr,
                         OUTPUT BongHode.KundeNavn).
  END.

  ASSIGN
      BongHode.KortType = piKortType /* 1-Ingen, 2-Kunde, 3-Medlem */
      .
END PROCEDURE.

PROCEDURE Medlemssalg:
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
                
/* Det er sjekket på forhånd at det er medlemssalg på denne bongen. */
/* Dvs bongen skal posteres.                                        */ 
/* Det forutsettes her at det er kontrollert at medlemmet finnes og */
/* Her posteres nå salg og betalinger.                              */

DEF BUFFER bMedTrans FOR MedTrans.                
DEF BUFFER bMedlemSaldo FOR MEdlemSaldo.
                
DEF VAR dVVareKost AS DEC NO-UNDO.

/* Finner ikke medlemmet, posteres ikke medlemstransene.            */
FIND Medlem NO-LOCK WHERE
    Medlem.MedlemsNr = BongHode.MedlemsNr NO-ERROR.
IF NOT AVAILABLE Medlem THEN
    RETURN "".
                 
/* Leser og posteres varesalgstranser. */
VARESALG:
FOR EACH BongLinje NO-LOCK WHERE
    BongLinje.B_Id = BongHode.B_Id AND
    BongLinje.TTId < 50 AND
    BongLinje.Makulert = FALSE:

    /* Overføringer skal ikke med */
    IF BongLinje.TTId = 6 THEN
        NEXT VARESALG.

    /* Henter artikkel. */
    FIND ArtBas NO-LOCK WHERE
        ArtBas.Vg    = BongLinje.VareGr AND
        ArtBas.LopNr = BongLinje.LopeNr NO-ERROR.

    /* Transaksjonen kan ha blitt oppdatert tidligere. */
    IF CAN-find(bMedTrans where
                bMedTrans.MedlemsNr = BongHode.MedlemsNr AND
                bMedTrans.Butik     = BongLinje.ButikkNr    AND
                bMedTrans.TransNr   = BongLinje.TransNr  AND
                bMedTrans.SeqNr     = BongLinje.SeqNr) THEN
      RETURN "AVBRYT".

    /* Hånterer varekost */
    ASSIGN
        dVVareKost = BongLinje.VVareKost.
    IF dVVareKost = 0 THEN
        RUN SetVVAreKost (INPUT BongLinje.ArtikkelNr,
                          INPUT BongLinje.ButikkNr,
                          INPUT BongLinje.LinjeSum / BongLinje.Antall,
                          OUTPUT dVVareKost).

    POSTERING-TRANS:
    DO FOR bMedTrans TRANSACTION:
      CREATE bMedTrans.
      ASSIGN
          bMedTrans.MedlemsNr      = BongHode.MedlemsNr
          bMedTrans.Butik          = BongLinje.ButikkNr
          bMedTrans.TransNr        = BongLinje.TransNr
          bMedTrans.SeqNr          = BongLinje.SeqNr

          .
      ASSIGN
        bMedTrans.KassaNr        = BongLinje.KasseNr
        bMedTrans.Dato           = BongLinje.TransDato
        bMedTrans.Tid            = BongLinje.TransTid
        bMedTrans.BongId         = BongLinje.BongNr
        bMedTrans.BongLinjeNr    = BongLinje.LinjeNr

        bMedTrans.BatchNr        = 0
        bMedTrans.TTId           = BongLinje.TTId
        bMedTrans.TBId           = BongLinje.TBId
        bMedTrans.ArtikkelNr     = dec(BongLinje.ArtikkelNr)
        bMedTrans.LevNr          = (IF AVAILABLE ArtBas
                                    THEN ArtBas.LevNr
                                    ELSE 0)
        bMedTrans.Vg             = BongLinje.VareGr
        bMedTrans.LopNr          = BongLinje.LopeNr
        bMedTrans.Storl          = BongLinje.Storrelse
        bMedTrans.Antall         = BongLinje.Antall
        bMedTrans.Pris           = BongLinje.LinjeSum
        bMedTrans.RabKr          = BongLinje.LinjeRab
        bMedTrans.SubTotalRab    = BongLinje.SubTotalRab
        bMedTrans.Mva            = BongLinje.MvaKr
        bMedTrans.VVarekost      = dVVarekost
        bMedTrans.SelgerNr       = BongHode.SelgerNr
        bMedTrans.BongTekst      = BongLinje.BongTekst
        bMedTrans.SattVVareKost  = (IF BongLinje.VVareKost <> 0
                                      THEN TRUE 
                                      ELSE FALSE)
        bMedTrans.KortNr         = IF BongHode.KortType = 3 /* Medlemskort */
                                     THEN BongHode.MedlemsKort
                                   ELSE IF BongHode.KortType = 2 
                                     THEN BongHode.KundeKort
                                   ELSE ""
        bMedTrans.ForsNr         = BongHode.KassererNr
        bMedTrans.RefNr          = BongLinje.RefNr
        bMedTrans.RefTekst       = BongLinje.RefTekst
        .

      POSTERING-SALDO:
      DO FOR bMedlemSaldo WHILE TRUE:
        FIND bMedlemSaldo EXCLUSIVE-LOCK where
            bMedlemSaldo.MedlemsNr = BongHode.MedlemsNr AND
            bMedlemSaldo.Butik     = BongLinje.ButikkNr
            NO-ERROR NO-WAIT.
        /* Posten holdes av en annen, vi forsøker igjen. */
        IF LOCKED bMedlemSaldo THEN
            NEXT POSTERING-SALDO.
        /* Oppretter ny post. */
        IF NOT AVAILABLE bMedlemSaldo THEN
        DO:
            CREATE bMedlemSaldo.
            ASSIGN
                bMedlemSaldo.MedlemsNr = BongHode.MedlemsNr
                bMedlemSaldo.Butik     = BongLinje.ButikkNr
                .
        END.
        /* Posterer 1.gangs kjøp. */
        IF bMedlemSaldo.ForsteDato = ? THEN
          ASSIGN
            bMedlemSaldo.ForsteDato = BongLinje.TransDato
            bMedlemSaldo.ForsteTid  = BongLinje.TransTid
            .
        /* Posterer siste gangs kjøp. */
        ASSIGN
            bMedlemSaldo.DatoSiste  = BongLinje.TransDato
            bMedlemSaldo.SisteTid   = BongLinje.TransTid
            .
        /* Oppdaterer saldo. */
        ASSIGN
            bMedlemSaldo.Saldo      = bMedlemSaldo.Saldo + bMedTrans.Pris 
            bMedlemSaldo.TotaltKjop = bMedlemSaldo.TotaltKjop + bMedTrans.Pris
            .
   
        LEAVE POSTERING-SALDO.    
      END. /* POSTERING-SALDO */
    END. /* POSTERING-TRANS */

END. /* VARESALG */
END PROCEDURE.

PROCEDURE PosterStatistikk:

/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def var wDataObjekt as char no-undo.
  DEF VAR wWork AS DEC NO-UNDO.
  DEF VAR wMva% AS DEC NO-UNDO.
  DEF VAR wVareKost AS DEC NO-UNDO.

  def buffer bufStLinje for StLinje.

 wVareKost = TransLogg.VVarekost.

    /* Oppdaterer statistikker */
    STATISTIKK_DEF:
    for each stdef NO-LOCK WHERE
        StDef.SttypeId = "MEDLEM"
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
        when "MEDLEM" then
          wDataObjekt = IF TransLogg.MedlemsNr <> 0
                          THEN string(TransLogg.MedlemsNr,"9999999999999")
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
          end.                                      
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
       end case.
    
    end. /* STATISTIKK_DEF */      

  return "OK".

END PROCEDURE.

procedure SetVVAreKost:
  DEF INPUT  PARAMETER pdArtikkelNr AS DEC                   NO-UNDO.
  DEF INPUT  PARAMETER piButikkNr   AS INT                   NO-UNDO.
  DEF INPUT  PARAMETER pdPris       AS DEC                   NO-UNDO.
  DEF OUTPUT PARAMETER pdVVareKost  LIKE BongLinje.VVareKost NO-UNDO. 


  /* Henter varekost i butikken det overføres fra. */      
  /* Dette er pris eExMva.                         */
  FIND Lager NO-LOCK WHERE
      Lager.ArtikkelNr = pdArtikkelNr AND
      Lager.Butik      = piButikkNr NO-ERROR.
  IF AVAILABLE Lager THEN
      pdVVarekost = Lager.VVareKost.
  ELSE 
      pdVVareKost = 0.

  /* Sjekker om varekost er satt.                                       */
  /* Er det ikke satt noen varekost, merkes transaksjonen med feilkode. */
  if pdVVareKost = 0 then /* or wBrutto% *** Skal også utføres for brutto% artikkler */
    DO:
      if VALID-HANDLE(h_PrisKo) then
        RUN HentVareKost in h_PrisKo (INPUT  pdArtikkelNr, 
                                      input  piButikkNr, 
                                      INPUT  pdPris, 
                                      output pdVVareKost).
    END.
  IF pdVVareKost = ? THEN
      pdVVareKost = 0.

end.

