DEF VAR iOrdretype AS INT NO-UNDO.
DEF VAR iAnt AS INT NO-UNDO.
DEF VAR iantstk AS INT NO-UNDO.
DEF VAR cFil AS CHAR NO-UNDO.
DEFINE VARIABLE iMButikkNr AS INTEGER NO-UNDO.
DEF VAR plMinusButikk   AS LOG NO-UNDO.
DEFINE VARIABLE lKalkvarekost AS DECIMAL NO-UNDO.
DEFINE VARIABLE bInnkjopsPris AS LOG NO-UNDO.
DEFINE VARIABLE ctekst AS CHARACTER NO-UNDO.
DEFINE VARIABLE lPkSdlId LIKE PkSdlHode.PkSdlId NO-UNDO.
DEF VAR hBuffer AS HANDLE NO-UNDO.
DEF VAR cReturn AS CHAR NO-UNDO.
DEF VAR bOk AS LOG NO-UNDO.

CURRENT-WINDOW:WIDTH = 350.

DEFINE BUFFER bufPkSdlLinje FOR PkSdlLinje.
DEFINE BUFFER bufBongLinje FOR BongLinje.
DEFINE BUFFER bufFAkturaLinje FOR FakturaLinje.
DEFINE BUFFER ovButiker FOR Butiker.
DEFINE BUFFER bufPksdlHode FOR PkSdlHode.

DEFINE TEMP-TABLE ttpkSdlLinje LIKE PkSdlLinje.
DEFINE TEMP-TABLE tt2pkSdlLinje LIKE PkSdlLinje.

DEF STREAM Ut.

/* Temp-Table and Buffer definitions                                    */
DEFINE NEW SHARED TEMP-TABLE TT_OvBuffer NO-UNDO LIKE OvBuffer.

{syspara.i 19 100 3 cTekst}
IF CAN-DO("Ja,yes,true,1",cTekst) THEN
    bInnkjopspris = TRUE. 
ELSE
    bInnkjopspris = FALSE. 

ASSIGN
    cFil = 'konv\pksdl_uten_linjer' + REPLACE(STRING(TODAY),'/','') + '.csv' 
    .

OUTPUT STREAM Ut TO VALUE(cFil) APPEND.
PUT STREAM Ut UNFORMATTED
    'SendtDato;'
    'PkSdlNr;'
    'PkSdlStatus;'
    'PkSdlOpphav;'
    'FakturaNr;'
    'Bilagstype;'
    'ArtikkelNr;'
    'Varetekst;'
    'BongNr;'
    'ButikkNr;'
    'MButikkNr;'
    'TTId;'
    'TBId;'
    'antall'
    SKIP.

OUTPUT STREAM Ut CLOSE.


LESALLE:
FOR EACH PkSdlhode EXCLUSIVE-LOCK WHERE 
    PkSdlHode.PkSdlStatus = 10 AND 
    PkSdlHode.SendtDato   >= 01/01/2017 AND
    NOT CAN-FIND(FIRST PkSdlLinje OF PkSdlHode),
    FIRST FakturaHode NO-LOCK WHERE
        FakturaHode.Bilagstype = 1 AND
        FakturaHode.FakturaNr = DEC(PkSdlHode.PkSdlNr) /*,
    FIRST bufFakturaLinje OF FakturaHode NO-LOCK,
    FIRST bufBongLinje  NO-LOCK WHERE
        bufBongLinje.B_Id    = bufFakturaLinje.B_Id AND
        bufBongLinje.LinjeNr = bufFakturaLinje.BongLinjeNr*/ :

    IF AVAILABLE FakturaHode THEN 
    DO:
        iAnt = iant + 1.

        DISPLAY
            PkSdlHode.SendtDato
            PkSdlHode.PkSdlNr
            PkSdlHode.PkSdlStatus
            PkSdlHode.PkSdlOpphav
            (IF AVAILABLE FakturaHode THEN STRING(FakturaHode.FakturaNr) ELSE '')
            (IF AVAILABLE FakturaHode THEN STRING(FakturaHode.Bilagstype) ELSE '')
            /*
            bufFakturaLinje.ArtikkelNr
            bufFakturaLinje.Varetekst
            bufBongLinje.BongNr
            bufBongLinje.ButikkNr
            bufBongLinje.MButikkNr
            bufBongLinje.TTId
            bufBongLinje.TBId COLUMN-LABEL 'TBId'
            bufBongLinje.antall
            */
        WITH WIDTH 350.

        OUTPUT STREAM Ut TO VALUE(cFil) APPEND.
        PUT STREAM Ut UNFORMATTED 
            PkSdlHode.SendtDato ';'
            PkSdlHode.PkSdlNr ';'
            PkSdlHode.PkSdlStatus ';'
            PkSdlHode.PkSdlOpphav ';'
            (IF AVAILABLE FakturaHode THEN STRING(FakturaHode.FakturaNr) ELSE '') ';'
            (IF AVAILABLE FakturaHode THEN STRING(FakturaHode.Bilagstype) ELSE '') ';'
            /*
            bufFakturaLinje.ArtikkelNr ';'
            bufFakturaLinje.Varetekst ';'
            bufBongLinje.BongNr ';'
            bufBongLinje.ButikkNr ';'
            bufBongLinje.MButikkNr ';'
            bufBongLinje.TTId ';'
            bufBongLinje.TBId ';'
            bufBongLinje.antall
            */
            SKIP.
        OUTPUT STREAM Ut CLOSE.

        RUN OpprettOvBuffer.
        FIND FIRST tt_Ovbuffer.
        
        IF AVAILABLE tt_Ovbuffer THEN
        DO:
            RUN opprettPakkseddlerOutlet.p (INPUT TABLE tt_OvBuffer, TT_OvBuffer.ButikkNrTil, TT_OvBuffer.ButikkNrTil, FakturaHode.Faktura_Id, 4, OUTPUT lPkSdlId) NO-ERROR.
            IF CAN-FIND(bufPkSdlHode WHERE 
                        bufPkSdlHode.PkSdlId = lPkSdlId) THEN 
                DELETE PkSdlHode.

            FIND bufPkSdlHode NO-LOCK WHERE 
                bufPkSdlHode.PkSdlId = lPkSdlId NO-ERROR.
            IF AVAILABLE bufPkSdlHode THEN 
            DO:
                EMPTY TEMP-TABLE tt2PkSdlLinje.
                FOR EACH pkSdlLinje NO-LOCK WHERE
                    PkSdlLinje.PkSdlId = bufPkSdlHode.PkSdlId:
                    CREATE tt2PkSdlLinje.
                      BUFFER-COPY PkSdlLinje TO tt2PkSdlLinje.
                END.
                hBuffer = TEMP-TABLE tt2PkSdlLinje:DEFAULT-BUFFER-HANDLE.
                RUN pksdl_innlever.p ('', hBuffer, '', OUTPUT cReturn, OUTPUT bOk).                      
                EMPTY TEMP-TABLE tt2PkSdlLinje.   
            END.
        END.
        
    END.
END. /* LESALLE */
MESSAGE 'Gurre er ferdig'
    VIEW-AS ALERT-BOX INFO BUTTONS OK.



/* **********************  Internal Procedures  *********************** */

PROCEDURE OpprettOvBuffer:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  DEFINE VARIABLE pcTTId AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vVareKost LIKE Lager.VVarekost NO-UNDO.
  DEFINE VARIABLE piLinjeNr AS INTEGER NO-UNDO.
  
  EMPTY TEMP-TABLE TT_OvBuffer.

  ASSIGN
      piLinjeNr = 1.
    
  TRANSRAD:      
  FOR EACH FakturaLinje OF FakturaHode NO-LOCK,
    FIRST BongLinje  NO-LOCK WHERE
        BongLinje.B_Id    = FakturaLinje.B_Id AND
        BongLinje.LinjeNr = FakturaLinje.BongLinjeNr AND 
        CAN-DO("006",STRING(BongLinje.TTId,"999")):
      
      FIND Butiker NO-LOCK WHERE
          Butiker.butik = BongLinje.ButikkNr NO-ERROR.
      ASSIGN
          pcTTId = STRING(BongLinje.TTId,"999")
          .

      /* Ukjente artikler kan ikke behandles */
      IF TRIM(BongLinje.ArtikkelNr) = "" THEN
          NEXT TRANSRAD.
  
      /* Henter Artikkel */
      FIND ArtBas NO-LOCK WHERE
          ArtBas.ArtikkelNr = DECIMAL(BongLinje.ArtikkelNr) NO-ERROR.
      IF NOT AVAILABLE ArtBas THEN
          NEXT TRANSRAD.
      FIND Lager NO-LOCK WHERE 
        Lager.ArtikkelNr = DECIMAL(BongLinje.ArtikkelNr) AND 
        Lager.Butik = BongLinje.ButikkNr NO-ERROR.
      IF AVAILABLE Lager THEN 
        vVareKost = Lager.vVarekost.
      IF vVareKost = ? THEN vVareKost = 0.
        
      IF AVAILABLE ArtBas THEN
        FIND ArtPris NO-LOCK WHERE
          Artpris.ArtikkelNr = ArtBas.ArtikkelNr AND
          ArtPris.ProfilNr   = Butiker.ProfilNr NO-ERROR.
      IF AVAILABLE ArtBas AND NOT AVAILABLE ArtPris THEN
        FIND ArtPris NO-LOCK WHERE
          Artpris.ArtikkelNr = ArtBas.ArtikkelNr AND
          ArtPris.ProfilNr   = 1 NO-ERROR.
      IF NOT AVAILABLE ArtPris THEN 
        FIND FIRST ArtPris OF ArtBas NO-LOCK NO-ERROR.

      /* Henter butikk det overføres til. */
      FIND ovButiker NO-LOCK WHERE
          ovButiker.Butik = INT(BongLinje.MButikkNr) NO-ERROR.
      IF AVAILABLE ovButiker THEN
          plMinusButikk = ovButiker.MinusButikk.
      ELSE
          plMinusButikk = FALSE.
      
      /* Logger mottagende butikk for bongen */    
      iMButikkNr = INT(BongLinje.MButikkNr).

      ASSIGN 
          lKalkVarekost = ArtPris.InnkjopsPris[1] - ROUND(((ArtPris.InnkjopsPris[1] * ArtPris.Rab1%[1]) / 100),2)
          . 

      /* Logger overføringstransaksjonen */
      CREATE TT_OvBuffer.
      ASSIGN TT_OvBuffer.BuntNr      = 999 /* dummy, kan vara vad som helst */
             TT_OvBuffer.LinjeNr     = piLinjeNr
             TT_OvBuffer.ArtikkelNr  = dec(Bonglinje.ArtikkelNr)
             TT_OvBuffer.Vg          = BongLinje.VareGr   
             TT_OvBuffer.LopNr       = BongLinje.LopeNr
             TT_OvBuffer.Antall      = BongLinje.Antall
             TT_OvBuffer.Merknad     = "Kasse FIX"
             TT_OvBuffer.Storl       = BongLinje.Storrelse
             TT_OvBuffer.TilStorl    = BongLinje.Storrelse
             TT_OvBuffer.Varekost    = (IF bInnkjopsPris THEN lKalkVarekost ELSE BongLinje.VVarekost) /* Vektet kostpris fra kassen */
             TT_OvBuffer.Varekost    = (IF TT_OvBuffer.Varekost = 0 THEN vVareKost ELSE TT_OvBuffer.Varekost) /* Vektet kostpris fra db */
             TT_OvBuffer.Varekost    = (IF TT_OvBuffer.Varekost = 0 THEN lKalkVarekost ELSE TT_OvBuffer.Varekost) /* Kostpris fra kalkyle. */
             piLinjeNr               = piLinjeNr + 1
             /* Setter datoinfo i registrert dato og tid. */
             TT_OvBuffer.RegistrertDato = BongLinje.TransDato
             TT_OvBuffer.RegistrertTid  = BongLinje.TransTid
             TT_OvBuffer.RegistrertAv   = USERID("SkoTex")
             .
      /* Vanlig overføring */
      IF plMinusbutikk = FALSE THEN
          ASSIGN
            TT_OvBuffer.ButikkNrFra = INT(BongLinje.ButikkNr)
            TT_OvBuffer.ButikkNrTil = INT(BongLinje.MButikkNr)        
            .
      /* Overføring fra kassen som markerer for lite mottatte varer.       */
      /* Her snur vi retningen på overføringen. Butikken skal motta varer. */
      ELSE
          ASSIGN
            TT_OvBuffer.ButikkNrFra = INT(BongLinje.MButikkNr)        
            TT_OvBuffer.ButikkNrTil = INT(BongLinje.ButikkNr)
            .
  END. /* TRANSRAD. */
    

END PROCEDURE.


