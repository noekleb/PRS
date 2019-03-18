/* Genererer TimeGrip på bakgrunn av bongdata.

  RUN generertimegripeksport.p
                          (DATE(FraDato:SCREEN-VALUE),
                           DATE(TilDato:SCREEN-VALUE),
                           cButiLst,
                           INPUT-OUTPUT iAntLest,
                           INPUT-OUTPUT iAntPostert,
                           OUTPUT cMsgs).
*/
DEFINE INPUT PARAMETER cLogg AS CHARACTER NO-UNDO.
DEF INPUT PARAMETER dFraDato AS DATE NO-UNDO.
DEF INPUT PARAMETER dTilDato AS DATE NO-UNDO.
DEF INPUT PARAMETER cButLst  AS CHARACTER NO-UNDO.
DEF INPUT-OUTPUT PARAMETER iAntLest     AS INT NO-UNDO.
DEF INPUT-OUTPUT PARAMETER iAntPostert  AS INT NO-UNDO.
DEF OUTPUT PARAMETER cMsgs AS CHAR NO-UNDO.

DEF VAR iCL                AS INTEGER NO-UNDO.
DEFINE VARIABLE iLoop      AS INTEGER NO-UNDO.
DEFINE VARIABLE dDato      AS DATE    NO-UNDO.
DEFINE VARIABLE iButikkNr  AS INTEGER NO-UNDO.
DEFINE VARIABLE iAnsattNr  AS INTEGER NO-UNDO.
DEFINE VARIABLE iAntCust   AS INTEGER NO-UNDO.
DEFINE VARIABLE dSalesDate AS INTEGER NO-UNDO.
DEFINE VARIABLE dLinjeSum  AS DECIMAL NO-UNDO.
DEFINE VARIABLE dSelgerNr  AS INT     NO-UNDO.
DEFINE VARIABLE iKasSelg   AS INTEGER NO-UNDO.
DEFINE VARIABLE iBranVen   AS INTEGER NO-UNDO.
DEFINE VARIABLE bTest      AS LOG     NO-UNDO.
DEFINE VARIABLE cMerknad   AS CHARACTER NO-UNDO.

{syspara.i  5 1 1 iCL INT}
{syspara.i 50 21 2 iBranVen INT}
{syspara.i 50 21 1 iKasSelg INT}

DEFINE BUFFER bufBongLinje FOR BongLinje.

ASSIGN
  bTest = TRUE.

IF bTest THEN 
    RUN bibl_logg.p (cLogg, 'generertimegripeksport.p - Dato: ' + STRING(dFraDato) + ' - ' + STRING(dTilDato) + ' Butiker: ' + cButLst).

/* Kontroll av butikkliste */
ASSIGN 
  cButLst = TRIM(TRIM(cButLst),',')
  cButLst = REPLACE(cButLst,'|',',').
IF cButLst = '' THEN 
  DO:
    cMsgs = '** Tom butikkliste mottatt i generertimegripeksport.p.'.
    IF bTest THEN 
        RUN bibl_logg.p (cLogg, 'generertimegripeksport.p - Msg: ' + cMsgs).
    RETURN.
  END.
/* Kontroll av datoer */
IF dFraDato = ? OR dTilDato = ? THEN 
  DO:
    cMsgs = '** Feil datoangivelse mottatt i generertimegripeksport.p.'.
    IF bTest THEN 
        RUN bibl_logg.p (cLogg, 'generertimegripeksport.p - Msg: ' + cMsgs).
    RETURN.
  END.
IF dFraDato > dTilDato THEN 
  DO:
    cMsgs = '** Fradato > Tildato mottatt i generertimegripeksport.p.'.
    IF bTest THEN 
        RUN bibl_logg.p (cLogg, 'generertimegripeksport.p - Msg: ' + cMsgs).
    RETURN.
  END.

/* Leser data for butikker i angitt periode. */
RUN LesButikkerOgDato.

/* **********************  Internal Procedures  *********************** */

PROCEDURE deleteData:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    SLETTELOOP: 
    DO TRANSACTION:
        FOR EACH TGEmp WHERE
            TGEmp.TGExportId   = TGExport.TGExportId AND 
            TGEmp.TGStore_Id   = TGExport.TGStore_Id:
            DELETE TGEmp. 
        END.

        FOR EACH TGSales WHERE 
            TGSales.TGExportId    = TGExport.TGExportId AND
            TGSales.TGStore_Id    = TGExport.TGStore_Id:
            DELETE TGSales.
        END.
    
        FOR EACH TGSales_Ext WHERE 
            TGSales_Ext.TGExportId    = TGExport.TGExportId AND
            TGSales_Ext.TGStore_Id    = TGExport.TGStore_Id:
            DELETE TGSales_Ext.        
        END.
        
        FOR EACH TGTimeStamp WHERE 
            TGTimeStamp.TGExportId    = TGExport.TGExportId AND
            TGTimeStamp.TGStore_Id    = TGExport.TGStore_Id:
            DELETE TGTimeStamp.    
        END.
        FIND CURRENT TGExport EXCLUSIVE-LOCK.
        ASSIGN
            TGExport.TGExportDate = ?
            TGExport.TGExportTime = 0 
            TGExport.TGNote       = ''
            .
        FIND CURRENT TGExport NO-LOCK.
        
    END. /* SLETTELOOP */
    IF bTest THEN RUN bibl_logg.p (cLogg, 'deleteData: ' + STRING(TGExport.TGStore_Id) + ' Dato: ' +  STRING(TGExport.TGSalesDate) + ' ').

END PROCEDURE.

PROCEDURE LesButikkerOgDato:
/*------------------------------------------------------------------------------
		Purpose:  																	  
		Notes:  																	  
------------------------------------------------------------------------------*/

BUTIKK_LOOP:
DO iLoop = 1 TO NUM-ENTRIES(cbutLst):
  /* Kontroll av butikknr. */
  iButikkNr = INT(ENTRY(iLoop,cButLst)) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN 
    NEXT BUTIKK_LOOP.
  IF NOT CAN-FIND(Butiker WHERE Butiker.Butik = iButikkNr) THEN 
    NEXT BUTIKK_LOOP.
  
  FIND Butiker NO-LOCK WHERE
    Butiker.Butik = iButikkNr NO-ERROR.
  IF AVAILABLE Butiker THEN
  BUTIKK: 
  DO:

    IF bTest THEN 
        RUN bibl_logg.p (cLogg, 'generertimegripeksport.p - BUTIKKLOOP: ' + STRING(Butiker.Butik) + ' ' + Butiker.ButNamn).

    DATO_LOOP:
    DO dDato = dFraDato TO dTilDato:

      IF bTest THEN RUN bibl_logg.p (cLogg, 'generertimegripeksport.p - DATO_LOOP: ' + STRING(dDato)).

      /* Sjekker om dato/butikk er behandlet tidligere. Er den det, tas neste dato. */    
      FIND FIRST TGExport NO-LOCK WHERE 
          TGExport.TGStore_Id     = iButikkNr AND 
          TGExport.TGSalesDate    = dDato NO-ERROR.
      IF AVAILABLE TGExport THEN
        DO: 
          IF bTest THEN RUN bibl_logg.p (cLogg, 'generertimegripeksport.p - TGExport finnes - Sletter data, data genererer og legges ut på nytt: ' + STRING(dDato)).
          RUN deleteData.
        END. 
      ELSE DO TRANSACTION:
        /* Id og dato/tid tildeles i trigger. */
        CREATE TGExport.
        ASSIGN
          TGExport.TGStore_Id     = iButikkNr 
          TGExport.TGSalesDate    = dDato
          TGExport.TGRemark       = 'Salgsdato ' + STRING(dDato) + ' ' + STRING(TIME,"HH:MM:SS")
          TGExport.ETid           = TIME 
          TGExport.EDato          = TODAY 
          .
        FIND CURRENT TGExport NO-LOCK.
        IF bTest THEN RUN bibl_logg.p (cLogg, 'generertimegripeksport.p - TGExport opprettet: ' + STRING(TGExport.TGStore_Id)).
      END. /* TRANSACTION */
        
      IF bTest THEN RUN bibl_logg.p (cLogg, 'generertimegripeksport.p - posterTGMed STARTES.').
      RUN posterTGMed. /* Medarbeiderinfo den angitte dagen. */  
        
      BONGHODE_LOOP:
      FOR EACH BongHode NO-LOCK WHERE
        BongHode.ButikkNr = iButikkNr AND 
        BongHode.Dato     = dDato:

        IF bTest THEN RUN bibl_logg.p (cLogg, 'generertimegripeksport.p - Bong: ' + STRING(BongHode.BongNr) + ' Butikk/dato:' + STRING(iButikkNr) + '/' + STRING(dDato)).
      
        /* Antall leste bonger og kunder. */
        ASSIGN
          iAntCust = (IF BongHode.Belop > 0 THEN 1 ELSE 0)
          iAntLest = iAntLest + 1.

        /* Makulerte bonger skal ikke tas med. */
        /*
        IF BongHode.Makulert >= 1 THEN 
          NEXT BONGHODE_LOOP.
        */
        /* Antall posterte bonger. */        
        iAntPostert = iAntPostert + 1.
        
        BONGLINJE:
        FOR EACH BongLinje WHERE 
          BongLinje.B_Id = BongHode.B_Id AND 
          BongLinje.Makulert = FALSE:
          
          RUN posterTGSales.     /* Postering av salgsdata pr. time. */
          RUN posterTGSales_Ext. /* Postering av datatyper           */
          
        END. /* BONGLINJE */

        /* Postering av inn/utmelding */ 
        cMerknad  = ''.       
        BONGLINJE:
        FOR EACH BongLinje WHERE 
          BongLinje.B_Id = BongHode.B_Id AND 
          BongLinje.Makulert = FALSE AND 
          CAN-DO("095,096,097,146",STRING(BongLinje.TTId,"999")):
          
          RUN posterTGTimeStamp. /* Postering av selger transer.     */
          
        END. /* BONGLINJE */
        
      END. /*BONGHODE_LOOP */
      
      
      /* Tar bort tomme record. */
      IF NOT CAN-FIND(FIRST TGTimeStamp OF TGExport) AND  
         NOT CAN-FIND(FIRST TGSales_Ext OF TGExport) AND 
         NOT CAN-FIND(FIRST TGSales     OF TGExport) THEN          
         DO TRANSACTION:
             IF AVAILABLE TGExport THEN
             DO: 
                 FIND CURRENT TGExport EXCLUSIVE-LOCK.                 
                 DELETE TGExport.
             END.
         END. /* TRANSACTION */
    END. /* DATO_LOOP */
  END. /* BUTIKK */
END. /* BUTIKK_LOOP */

IF bTest THEN RUN bibl_logg.p (cLogg, 'generertimegripeksport.p - BUTIKK_LOOP Ferdig.').

END PROCEDURE.

PROCEDURE posterTGMed:
/*------------------------------------------------------------------------------
		Purpose:  																	  
		Notes:  																	  
------------------------------------------------------------------------------*/
  IF bTest THEN RUN bibl_logg.p (cLogg, 'generertimegripeksport.p - posterTGMed.').

  IF iKasSelg = 1 THEN 
  KASSERER:
  DO:
      /* Postering av kassererdata / Ansatte */
      KASSERER_LOOP:
      FOR EACH ButikkForsalj NO-LOCK,
        EACH Forsalj NO-LOCK WHERE 
          Forsalj.ForsNr = ButikkForsalj.ForsNr:
        
        FIND Post NO-LOCK WHERE 
          Post.PostNr = Forsalj.FoPoNr NO-ERROR.

        iAnsattNr = 0.
        ASSIGN iAnsattNr = INT(Forsalj.FoAnstNr) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN 
          NEXT KASSERER_LOOP.
        
        
        IF NOT CAN-FIND(TGEmp WHERE
                        TGEmp.TGExportId   = TGExport.TGExportId AND 
                        TGEmp.TGStore_Id   = ButikkForsalj.Butik AND 
                        TGEmp.TGEmployeeId = iAnsattNr) THEN 
          DO TRANSACTION:
              CREATE TGEmp.
              ASSIGN 
                TGEmp.TGExportId       = TGExport.TGExportId  
                TGEmp.TGStore_Id       = ButikkForsalj.Butik  
                TGEmp.TGEmployeeId     = iAnsattNr
                TGEmp.TGFirstName      = Forsalj.FoForNavn
                TGEmp.TGSureName       = Forsalj.FoNamn
                TGEmp.TGSalaryProfile  = INT(Forsalj.LonnProfil)
                TGEmp.TGWorkPercentage = Forsalj.ArbeidsProsent
                TGEmp.TGHourlyRate     = Forsalj.TimeLonn
                TGEmp.TGFixedSalary    = Forsalj.FastLonn
                TGEmp.TGStoreName      = Butiker.ButNamn
                TGEmp.TGAdress1        = Forsalj.FoAdr
                TGEmp.TGAdress2        = Forsalj.FoAdr2
                TGEmp.TGPostalCode     = Forsalj.FoPoNr
                TGEmp.TGPostalArea     = (IF AVAILABLE Post THEN Post.Beskrivelse ELSE '')
                TGEmp.TGBirthDate      = Forsalj.FodtDato
                TGEmp.TGBeginDate      = Forsalj.AnsattDato
                TGEmp.TGLeaveDate      = Forsalj.SluttetDato
                TGEmp.TGEmployeeTitle  = Forsalj.JobbTittel
                
                NO-ERROR. 
              FIND CURRENT TGEmp NO-LOCK.
          END. /* TRANSACTION */
      END. /* KASSERER_LOOP */  
  END. /* KASSERER*/

  ELSE 
  SELGER: 
  DO:
      /* Postering av kassererdata / Ansatte */
      SELGER_LOOP:
      FOR EACH ButikkSelger NO-LOCK,
        EACH Selger NO-LOCK WHERE 
          Selger.SelgerNr = ButikkSelger.SelgerNr:
        
        FIND Post OF Selger NO-LOCK NO-ERROR.

        iAnsattNr = 0.
        ASSIGN iAnsattNr = INT(Selger.AnsattNr) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN NEXT SELGER_LOOP.
        
        
        IF NOT CAN-FIND(TGEmp WHERE
                        TGEmp.TGExportId   = TGExport.TGExportId AND 
                        TGEmp.TGStore_Id   = ButikkSelger.ButikkNr AND 
                        TGEmp.TGEmployeeId = iAnsattNr) THEN 
          DO TRANSACTION:
              CREATE TGEmp.
              ASSIGN 
                TGEmp.TGExportId       = TGExport.TGExportId  
                TGEmp.TGStore_Id       = ButikkSelger.ButikkNr  
                TGEmp.TGEmployeeId     = iAnsattNr
                TGEmp.TGFirstName      = Selger.ForNavn
                TGEmp.TGSureName       = Selger.Navn
                TGEmp.TGSalaryProfile  = INT(Selger.LonnProfil)
                TGEmp.TGWorkPercentage = Selger.ArbeidsProsent
                TGEmp.TGHourlyRate     = Selger.TimeLonn
                TGEmp.TGFixedSalary    = Selger.FastLonn
                TGEmp.TGStoreName      = Butiker.ButNamn
                TGEmp.TGAdress1        = Selger.Adresse1
                TGEmp.TGAdress2        = Selger.Adresse2
                TGEmp.TGPostalCode     = Selger.PostNr
                TGEmp.TGPostalArea     = (IF AVAILABLE Post THEN Post.Beskrivelse ELSE '')
                TGEmp.TGBirthDate      = Selger.FodtDato
                TGEmp.TGBeginDate      = Selger.AnsattDato
                TGEmp.TGLeaveDate      = Selger.SluttetDato
                TGEmp.TGEmployeeTitle  = Selger.JobTittel
                
                NO-ERROR. 
              FIND CURRENT TGEmp NO-LOCK.
          END. /* TRANSACTION */
      END. /* SELGER_LOOP */  
  END. /* SELGER:*/

END PROCEDURE.

PROCEDURE posterTGSales:
/*------------------------------------------------------------------------------
		Purpose:  																	  
		Notes:  																	  
------------------------------------------------------------------------------*/
DEFINE VARIABLE dDbKr      AS DECIMAL NO-UNDO.
DEFINE VARIABLE iVendorId  AS INTEGER NO-UNDO.

IF bTest THEN RUN bibl_logg.p (cLogg, 'generertimegripeksport.p - posterTGSales.').

DO TRANSACTION:
  /* Behandler bare salg, retur og reklamasjoner her. */
  IF NOT CAN-DO('1,3,10',STRING(BongLinje.TTId))
    THEN RETURN.
  
  IF iKasSelg = 1 THEN 
  DO:
    /* Henter kassererinfo */
    FIND Forsalj NO-LOCK WHERE
      Forsalj.ForsNr = INT(Bonghode.KassererNr) NO-ERROR.
    IF AVAILABLE Forsalj THEN 
      dSelgerNr = DECIMAL(Forsalj.FoAnstNr).
    ELSE 
      dSelgerNr = 0.
  END.
  ELSE DO:
      /* Henter selger og ansattnr. */
      FIND Selger NO-LOCK WHERE 
        Selger.SelgerNr = BongHode.SelgerNr NO-ERROR.
      IF AVAILABLE Selger THEN 
        dSelgerNr = DEC(Selger.AnsattNr).
      ELSE 
        dSelgerNr = 0.
  END.  
  ASSIGN
    dSalesDate = INT(
                       STRING(YEAR(BongLinje.TransDato),'9999') 
                     + STRING(MONTH(BongLinje.TransDato),'99')  
                     + STRING(DAY(BongLinje.TransDato),'99')
                     ).
                     
  /* Varemerke eller produsent. */
  IF     iBranVen > 0 
     AND DEC(BongLinje.ArtikkelNr) > 0 
     AND CAN-FIND(ArtBas WHERE ArtBas.ArtikkelNr = DEC(BongLinje.ArtikkelNr)) THEN 
  DO:
    FIND ArtBas NO-LOCK WHERE 
      ArtBas.ArtikkelNr = DEC(BongLinje.ArtikkelNr) NO-ERROR.
    IF iBranVen = 1 THEN iVendorId = ArtBas.ProdNr.
    ELSE IF iBranVen = 2 THEN iVendorId = ArtBas.VmId.
    ELSE iVendorId = 0.
  END.
  ELSE iVendorId = 0. 
  
  FIND FIRST TGSales EXCLUSIVE-LOCK WHERE 
    TGSales.TGExportId    = TGExport.TGExportId AND
    TGSales.TGStore_Id    = TGExport.TGStore_Id AND
    TGSales.TGOperator_Id = dSelgerNr AND 
    TGSales.TGDate        = dSalesDate AND 
    TGSales.TGTimePeriod  = INT(ENTRY(1,STRING(BongLinje.TransTid,'HH:MM:SS'),':')) AND 
    TGSales.TGVendorId    = iVendorId NO-ERROR.
  IF NOT AVAILABLE TGSales THEN 
  DO:
    CREATE TGSales.
    ASSIGN
      TGSales.TGExportId    = TGExport.TGExportId 
      TGSales.TGStore_Id    = TGExport.TGStore_Id 
      TGSales.TGOperator_Id = dSelgerNr 
      TGSales.TGDate        = dSalesDate
      TGSales.TGTimePeriod  = INT(ENTRY(1,STRING(BongLinje.TransTid,'HH:MM:SS'),':'))
      TGSales.TGVendorId    = iVendorId 
      .
  END.
  
  /* Henter Varekost fra TransLogg. */
  FIND FIRST TransLogg NO-LOCK WHERE
    TransLogg.Butik   = BongLinje.ButikkNr AND
    TransLogg.TransNr = BongLinje.TransNr  AND 
    TransLogg.SeqNr   = BongLinje.SeqNr NO-ERROR.
  
  /* Akkumulerer opp omsetning pr. time. */
  ASSIGN 
    dLinjeSum             = (BongLinje.LinjeSum - BongLinje.MvaKr - BongLinje.LinjeRab - BongLinje.SubtotalRab)
    dDbKr                 = (BongLinje.LinjeSum - BongLinje.MvaKr - BongLinje.LinjeRab - BongLinje.SubtotalRab)
                            - (ABS(BongLinje.Antall) * (IF AVAILABLE TransLogg THEN TransLogg.VVareKost ELSE BongLinje.VVareKost))
                            
    dDbKr                 = (IF dDbKr = ? THEN 0 ELSE dDbKr)
    TGSales.TGTurnOver    = TGSales.TGTurnOver    + (IF BongLinje.Antall > 0 THEN dLinjeSum ELSE (dLinjesum * -1))
                            
    TGSales.TGVAT         = TGSales.TGVAT         + (IF BongLinje.Antall > 0 THEN BongLinje.MvaKr ELSE (BongLinje.MvaKr * -1))
    TGSales.TGNoCustomers = TGSales.TGNoCustomers + iAntCust 
    iAntCust              = 0 /* Skal bare telle opp en gang pr. bong. */
    TGSales.TGNoItems     = TGSales.TGNoItems     + BongLinje.Antall  
    TGSales.TGGrossProfit = TGSales.TGGrossProfit + (IF BongLinje.Antall > 0 THEN dDbKr ELSE (dDbKr * -1))  
    .  

END. /* TRANSACTION */

END PROCEDURE.

PROCEDURE posterTGSales_Ext:
/*------------------------------------------------------------------------------
		Purpose:  																	  
		Notes:  																	  
------------------------------------------------------------------------------*/

DEFINE VARIABLE cDataType AS CHARACTER NO-UNDO.

IF bTest THEN RUN bibl_logg.p (cLogg, 'generertimegripeksport.p - posterTGSales_Ext.').

FIND TransType NO-LOCK WHERE
  TransType.TTId = BongLinje.TTId NO-ERROR.
IF AVAILABLE TransType THEN 
  cDataType = STRING(BongLinje.TTId,'999') + ' ' + TransType.Beskrivelse.
ELSE
  cDataType = 'Ukjent'.

DO TRANSACTION:
  ASSIGN
    dSalesDate = INT(
                       STRING(YEAR(BongLinje.TransDato),'9999') 
                     + STRING(MONTH(BongLinje.TransDato),'99')  
                     + STRING(DAY(BongLinje.TransDato),'99')
                     ).  
  
  FIND FIRST TGSales_Ext EXCLUSIVE-LOCK WHERE 
    TGSales_Ext.TGExportId    = TGExport.TGExportId AND
    TGSales_Ext.TGStore_Id    = TGExport.TGStore_Id AND
    TGSales_Ext.TGDate        = dSalesDate AND 
    TGSales_Ext.TGTimePeriod  = INT(ENTRY(1,STRING(BongLinje.TransTid,'HH:MM:SS'),':')) AND 
    TGSales_Ext.TGDataType    = cDataType NO-ERROR.
  IF NOT AVAILABLE TGSales_Ext THEN 
  DO:
    CREATE TGSales_Ext.
    ASSIGN
      TGSales_Ext.TGExportId     = TGExport.TGExportId 
      TGSales_Ext.TGStore_Id     = TGExport.TGStore_Id 
      TGSales_Ext.TGDate         = dSalesDate
      TGSales_Ext.TGTimePeriod   = INT(ENTRY(1,STRING(BongLinje.TransTid,'HH:MM:SS'),':'))
      TGSales_Ext.TGDataType = cDataType
      .
  END.
  
  /* Akkumulerer opp omsetning pr. time. */
  IF CAN-DO('1,3,10',STRING(BongLinje.TTId)) THEN 
  DO:
    ASSIGN 
      dLinjeSum             = (BongLinje.LinjeSum - BongLinje.MvaKr - BongLinje.LinjeRab - BongLinje.SubtotalRab)
      TGSales_Ext.TGValue   = TGSales_Ext.TGValue    + (IF BongLinje.Antall > 0 THEN dLinjeSum ELSE (dLinjesum * -1))
      .
  END.
  ELSE DO:
    ASSIGN 
      TGSales_Ext.TGValue   = TGSales_Ext.TGValue + BongLinje.LinjeSum
      .
  END.
END. /* TRANSACTION */



END PROCEDURE.

PROCEDURE posterTGTimeStamp:
/*------------------------------------------------------------------------------
		Purpose:  																	  
		Notes:  																	  
------------------------------------------------------------------------------*/
DEFINE VARIABLE dSelgerNr2 AS DECIMAL   NO-UNDO.
DEFINE VARIABLE cNavn      AS CHARACTER NO-UNDO.
DEFINE VARIABLE iTGAction  AS INTEGER   NO-UNDO.

IF bTest THEN RUN bibl_logg.p (cLogg, 'generertimegripeksport.p - posterTGTimeStamp.').

DO TRANSACTION:

  IF iKasSelg = 1 THEN 
  KASSELG1:
  DO:
      /* Behandler bare inn og utlogging av kasserere. */
      IF NOT CAN-DO('96,97',STRING(BongLinje.TTId))
        THEN RETURN.
      
      /* Merknad */
      FIND FIRST bufBongLinje WHERE 
                 bufBongLinje.B_Id     = BongHode.B_Id AND 
                 bufBongLinje.Makulert = FALSE AND 
                 bufBongLinje.TTId     = 95 NO-ERROR.
      IF AVAILABLE bufBongLinje THEN 
        cMerknad = bufBongLinje.BongTekst.      
        
      /* Henter kasserer og ansattnr. */
      FIND Forsalj NO-LOCK WHERE 
        Forsalj.ForsNr = INT(BongHode.KassererNr) NO-ERROR.
      IF AVAILABLE Forsalj THEN 
        ASSIGN
          dSelgerNr2 = Forsalj.ForsNr
          dSelgerNr  = DEC(Forsalj.FoAnstNr)
          cNavn      = Forsalj.FoForNavn + 
                       (IF Forsalj.FoForNavn <> '' THEN ' ' ELSE '') + 
                       Forsalj.FoNamn.
      ELSE 
        ASSIGN 
          dSelgerNr2 = 0
          dSelgerNr  = 0
          cNavn      = 'Ukjent'.
      ASSIGN 
        iTGAction = (IF BongLinje.TTId = 96 
                       THEN 1
                     ELSE IF BongLinje.TTId = 97
                       THEN 0
                     ELSE 2).
  END. /* KASSELG1 */
  ELSE 
  KASSELG2:
  DO:
      /* Behandler bare angivelse av selgernr. */
      IF NOT CAN-DO('96,97',STRING(BongLinje.TTId))
        THEN RETURN.

      /* Merknad */
      FIND FIRST bufBongLinje WHERE 
                 bufBongLinje.B_Id     = BongHode.B_Id AND 
                 bufBongLinje.Makulert = FALSE AND 
                 bufBongLinje.TTId     = 95 NO-ERROR.
      IF AVAILABLE bufBongLinje THEN 
        cMerknad = bufBongLinje.BongTekst.      

      /* Henter selger og ansattnr. */
      FIND Selger NO-LOCK WHERE 
        Selger.SelgerNr = BongHode.SelgerNr NO-ERROR.
      IF AVAILABLE Selger THEN 
        ASSIGN
          dSelgerNr2 = Selger.SelgerNr
          dSelgerNr  = DEC(Selger.AnsattNr)
          cNavn      = Selger.Fornavn + 
                       (IF Selger.ForNavn <> '' THEN ' ' ELSE '') + 
                       Selger.Navn.
      ELSE 
        ASSIGN 
          dSelgerNr2 = 0
          dSelgerNr  = 0
          cNavn      = 'Ukjent'.

      ASSIGN
        iTGAction = (IF BongLinje.TTId = 96 
                       THEN 1
                     ELSE IF BongLinje.TTId = 97
                       THEN 0
                     ELSE 2).
  END. /* KASSELG2 */  
    
  ASSIGN
    dSalesDate = INT(
                       STRING(YEAR(BongLinje.TransDato),'9999') 
                     + STRING(MONTH(BongLinje.TransDato),'99')  
                     + STRING(DAY(BongLinje.TransDato),'99')
                     ).  
  
  IF NOT CAN-FIND(FIRST TGTimeStamp WHERE 
    TGTimeStamp.TGExportId    = TGExport.TGExportId AND
    TGTimeStamp.TGStore_Id    = TGExport.TGStore_Id AND
    TGTimeStamp.TGOperator_Id = dSelgerNr AND 
    TGTimeStamp.TGDate        = dSalesDate AND 
    TGTimeStamp.TGTime        = INT(REPLACE(STRING(BongLinje.TransTid,'HH:MM:SS'),':','')) AND  
    TGTimeStamp.TGAction      = iTGAction) THEN  
  DO:

    CREATE TGTimeStamp.
    ASSIGN
      TGTimeStamp.TGExportId      = TGExport.TGExportId
      TGTimeStamp.TGStore_Id      = TGExport.TGStore_Id
      TGTimeStamp.TGOperator_Id   = dSelgerNr 
      TGTimeStamp.TGDate          = dSalesDate 
      TGTimeStamp.TGTime          = INT(REPLACE(STRING(BongLinje.TransTid,'HH:MM:SS'),':','')) 
      TGTimeStamp.TGAction        = iTGAction
      TGTimeStamp.TGComment       = cMerknad
      .

    ASSIGN 
      TGTimeStamp.TGOperator_Name = cNavn
      /*TGTimeStamp.TGOperator_Id2  = dSelgerNr2*/ 
      .

    FIND CURRENT TGTimeStamp NO-LOCK. 
    RELEASE TGTimeStamp.
  END.
  
END. /* TRANSACTION */

END PROCEDURE.
