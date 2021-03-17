
/*------------------------------------------------------------------------
    File        : kordrelinje_opprett_retur_linjer.p
    Purpose     : Opprette varelinjer på retur ordre.

    Syntax      :

    Description : Ved retur av kundeordre, kopieres linjen fra opprinnelig 
                  ordre til retur ordren. Antall og beløp settes negative. 
                  Rutinen tar også høyde for at det kan være byttet vare på 
                  linjen.

    Author(s)   : tomn
    Created     : Wed Aug 21 11:21:12 CEST 2019
    Notes       :
          RUN kordrelinje_opprett_retur_linjer.p (KOrdreHode.KOrdre_Id,
                                                  bufKOrdreHode.KOrdre_Id, 
                                                  tt_Linjer.LinjeNr, 
                                                  tt_Linjer.feilkode,
                                                  tt_Linjer.Antall,
                                                  OUTPUT dSum
                                                  ).          
      
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE INPUT PARAMETER plRefKOrdre_Id AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER plKOrdre_Id AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER piLinjeNr AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER piFeilKode AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER plAntall AS DECIMAL NO-UNDO.
DEFINE OUTPUT PARAMETER dSum AS DECIMAL NO-UNDO.
DEFINE OUTPUT PARAMETER ocReturn AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER obOk AS LOG NO-UNDO.

DEFINE VARIABLE cLogg AS CHARACTER NO-UNDO.
DEFINE VARIABLE bTest AS LOG NO-UNDO.
DEFINE VARIABLE bBytte AS LOG NO-UNDO.

DEFINE BUFFER bufKOrdreLinje FOR KOrdreLinje.
DEFINE BUFFER bufKKOrdreLinje FOR KOrdreLinje.

DEFINE VARIABLE rStandardFunksjoner AS cls.StdFunk.StandardFunksjoner NO-UNDO.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

ASSIGN 
    cLogg        = 'kordrelinje_opprett_retur_linjer' + REPLACE(STRING(TODAY),'/','')
    bTest        = TRUE 
    NO-ERROR.

rStandardFunksjoner  = NEW cls.StdFunk.StandardFunksjoner( cLogg ) NO-ERROR.
IF bTest THEN 
DO:
  rStandardFunksjoner:SkrivTilLogg(cLogg,
      'Start' 
      ).    
  rStandardFunksjoner:SkrivTilLogg(cLogg,
      '      plRefKOrdre_Id : ' + STRING(plRefKOrdre_Id) 
      ).    
  rStandardFunksjoner:SkrivTilLogg(cLogg,
      '      plKOrdre_Id : ' + STRING(plKOrdre_Id) 
      ).    
  rStandardFunksjoner:SkrivTilLogg(cLogg,
      '      piLinjeNr  : ' + STRING(piLinjeNr) 
      ).    
  rStandardFunksjoner:SkrivTilLogg(cLogg,
      '      piFeilKode: ' + STRING(piFeilkode) 
      ).    
  rStandardFunksjoner:SkrivTilLogg(cLogg,
      '      plAntall: ' + STRING(plAntall) 
      ).    
END.

/* Henter Retur/bytte ordre. */
FIND KOrdreHode NO-LOCK WHERE 
  KOrdreHode.KOrdre_Id = plKOrdre_Id NO-ERROR.
IF AVAILABLE KOrdreHode AND KOrdreHode.EkstOrdreNr MATCHES '*BYTTE*' THEN 
  bBytte = TRUE.
ELSE 
  bBytte = FALSE.

DO TRANSACTION:
  /* Sjekker linjenr på retur/bytte ordre. */ 
  FIND bufKOrdreLinje EXCLUSIVE-LOCK WHERE 
    bufKOrdreLinje.KOrdre_Id     = plKOrdre_Id AND 
    bufKOrdreLinje.KOrdreLinjeNr = piLinjeNr NO-ERROR.
  /* Linjen bare kunne legges inn en gang. */
  IF AVAILABLE bufKOrdreLinje THEN 
  DO:
    ASSIGN 
      bufKOrdreLinje.ReturKodeId = piFeilkode
      obOk = FALSE 
      ocReturn = 'Ordrelinje er (kordrelinje_opprett_retur_linjer.p) allerede lagt inn på retur/bytte-ordre.'
      .
    RETURN.
  END.
  
  /* Henter linjen fra opprinnelig ordre og kopierer den til retur/bytte ordre. */
  FIND KORdreLinje EXCLUSIVE-LOCK WHERE 
    KOrdreLinje.KOrdre_Id     = plRefKOrdre_Id AND 
    KOrdreLinje.KOrdreLinjeNr = piLinjeNr NO-ERROR.
  IF AVAILABLE KOrdreLinje THEN 
  DO:
    CREATE bufKOrdreLinje.
    BUFFER-COPY KOrdreLinje
      EXCEPT KOrdre_Id Faktura_Id ReturKodeId
      TO bufKOrdreLinje
      ASSIGN 
        bufKOrdreLinje.KOrdre_Id   = plKOrdre_Id
        bufKOrdreLinje.ReturKodeId = piFeilkode
      .
    /* TN 13/2-19 For å gjøre det lettere å plukke ut returnerte linjer via Brynjar rammeverket. */
    ASSIGN 
      dSum                         = dSum + (KOrdreLinje.nettolinjesum)
      bufKOrdreLinje.Antall        = plAntall * -1
      bufKOrdreLinje.nettolinjesum = bufKOrdreLinje.nettolinjesum * -1
      bufKOrdreLinje.NettoPris     = bufKOrdreLinje.NettoPris * -1     
      bufKOrdreLinje.MvaKr         = bufKOrdreLinje.MvaKr * -1         
      bufKOrdreLinje.Mva%          = bufKOrdreLinje.Mva% * -1          
      bufKOrdreLinje.BruttoPris    = bufKOrdreLinje.BruttoPris * -1    
      bufKOrdreLinje.Pris          = bufKOrdreLinje.Pris * -1          
      bufKOrdreLinje.Linjesum      = bufKOrdreLinje.Linjesum * -1      
      .

    IF bBytte = FALSE THEN 
    DO:
      ASSIGN 
        KOrdreLinje.Returnert = TRUE
        .
      IF bTest THEN 
        rStandardFunksjoner:SkrivTilLogg(cLogg,
            '      RETUR og returflagg er satt: ' + STRING(KOrdreLinje.Returnert) 
            ).    
    END.
    ELSE DO:
      IF bTest THEN 
        rStandardFunksjoner:SkrivTilLogg(cLogg,
            '      BYTTE og returflagg ikke rørt: ' + STRING(KOrdreLinje.Returnert) 
            ).    
    END.

    FIND CURRENT KOrdreLinje NO-LOCK.
    
    IF bTest THEN 
        rStandardFunksjoner:SkrivTilLogg(cLogg,
            '  Opprettet linje KOrdre_Id: ' + STRING(bufKOrdreLinje.KOrdre_Id) + ' Linje: ' +
            STRING(bufKOrdreLinje.KORdreLinjeNr) + ' Returkode: ' +  
            STRING(bufKOrdreLinje.ReturKodeId) + ' Strekkode: ' + 
            bufKOrdreLinje.Kode + ' Aktiv: ' + 
            STRING(bufKOrdreLinje.Aktiv) + ' Returnert: ' + 
            STRING(bufKOrdreLinje.Returnert) + ' VareNr: ' +  
            bufKOrdreLinje.VareNr + ' Storl: ' +
            bufKOrdreLinje.Storl 
            ).    
    
    /* Tar med original passiv linje hvis linjen er kopiert. */
    IF KOrdreLinje.KopiKOrdreLinjeNr > 0 THEN
    TAR_MED_KOPI: 
    DO:
      /* Henter original linje. */
      FIND bufKKORdreLinje EXCLUSIVE-LOCK WHERE 
        bufKKOrdreLinje.KOrdre_Id     = KOrdreLinje.KOrdre_Id AND 
        bufKKOrdreLinje.KOrdreLinjeNr = KOrdreLinje.KopiKOrdreLinjeNr NO-ERROR.
      IF AVAILABLE bufKKORdreLinje THEN
      DO: 
        /* Legger opp kopi på retur ordre. */
        CREATE bufKOrdreLinje.
        BUFFER-COPY bufKKOrdreLinje
          EXCEPT KOrdre_Id Faktura_Id ReturKodeId
          TO bufKOrdreLinje
          ASSIGN 
            bufKOrdreLinje.KOrdre_Id   = plKOrdre_Id
            bufKOrdreLinje.ReturKodeId = piFeilkode
            .
        ASSIGN 
          bufKOrdreLinje.Antall        = plAntall * -1
          bufKOrdreLinje.nettolinjesum = bufKOrdreLinje.nettolinjesum * -1
          bufKOrdreLinje.NettoPris     = bufKOrdreLinje.NettoPris * -1     
          bufKOrdreLinje.MvaKr         = bufKOrdreLinje.MvaKr * -1         
          bufKOrdreLinje.Mva%          = bufKOrdreLinje.Mva% * -1          
          bufKOrdreLinje.BruttoPris    = bufKOrdreLinje.BruttoPris * -1    
          bufKOrdreLinje.Pris          = bufKOrdreLinje.Pris * -1          
          bufKOrdreLinje.Linjesum      = bufKOrdreLinje.Linjesum * -1      
          .

      IF bTest THEN 
          rStandardFunksjoner:SkrivTilLogg(cLogg,
              '  Opprettet KOPI passiv linje KOrdre_Id: ' + STRING(bufKOrdreLinje.KOrdre_Id) + ' Linje: ' +
              STRING(bufKOrdreLinje.KORdreLinjeNr) + ' Returkode: ' +  
              STRING(bufKOrdreLinje.ReturKodeId) + ' Strekkode: ' + 
              bufKOrdreLinje.Kode + ' Aktiv: ' + 
              STRING(bufKOrdreLinje.Aktiv) + ' Returnert: ' + 
              STRING(bufKOrdreLinje.Returnert) + ' VareNr: ' +  
              bufKOrdreLinje.VareNr + ' Storl: ' +
              bufKOrdreLinje.Storl 
              ).    
      END.
    END. /* TAR_MED_KOPI */
  END.
END. /* TRANSACTION */

ASSIGN 
  obOk = TRUE
  ocReturn = ''
  .
  
IF bTest THEN 
  rStandardFunksjoner:SkrivTilLogg(cLogg,
      'Slutt ' 
      ).    
  