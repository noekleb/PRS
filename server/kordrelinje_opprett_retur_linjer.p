
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
DEFINE INPUT PARAMETER plKOrdre_Id AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER plRetKOrdre_Id AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER piLinjeNr AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER piFeilKode AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER plAntall AS DECIMAL NO-UNDO.
DEFINE OUTPUT PARAMETER dSum AS DECIMAL NO-UNDO.

DEFINE BUFFER bufKOrdreLinje FOR KOrdreLinje.
DEFINE BUFFER bufKKOrdreLinje FOR KOrdreLinje.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

DO TRANSACTION:
  FIND KORdreLinje EXCLUSIVE-LOCK WHERE 
    KOrdreLinje.KOrdre_Id = plKOrdre_Id AND 
    KOrdreLinje.KOrdreLinjeNr = piLinjeNr NO-ERROR.
  IF AVAILABLE KORdreLinje THEN 
  DO:
    CREATE bufKOrdreLinje.
    BUFFER-COPY KOrdreLinje
      EXCEPT KOrdre_Id Faktura_Id
      TO bufKOrdreLinje
      ASSIGN 
        bufKOrdreLinje.KOrdre_Id = plRetKOrdre_Id
      .
    /* TN 13/2-19 For å gjøre det lettere å plukke ut returnerte linjer via Brynjar rammeverket. */
    ASSIGN 
      dSum                         = dSum + (KOrdreLinje.nettolinjesum)
      KOrdreLinje.ReturKodeId      = piFeilkode
      KOrdreLinje.Returnert        = TRUE
      bufKOrdreLinje.Antall        = plAntall * -1
      bufKOrdreLinje.nettolinjesum = bufKOrdreLinje.nettolinjesum * -1
      bufKOrdreLinje.NettoPris     = bufKOrdreLinje.NettoPris * -1     
      bufKOrdreLinje.MvaKr         = bufKOrdreLinje.MvaKr * -1         
      bufKOrdreLinje.Mva%          = bufKOrdreLinje.Mva% * -1          
      bufKOrdreLinje.BruttoPris    = bufKOrdreLinje.BruttoPris * -1    
      bufKOrdreLinje.Pris          = bufKOrdreLinje.Pris * -1          
      bufKOrdreLinje.Linjesum      = bufKOrdreLinje.Linjesum * -1      
      .
    FIND CURRENT KOrdreLinje NO-LOCK.
    /* Tar med original linje hvis linjen er kopiert. */
    IF KOrdreLinje.KopiKOrdreLinjeNr > 0 THEN
    TAR_MED_KOPI: 
    DO:
      /* Henter original linje. */
      FIND bufKKORdreLinje EXCLUSIVE-LOCK WHERE 
        bufKKOrdreLinje.KOrdre_Id = KOrdreLinje.KOrdre_Id AND 
        bufKKOrdreLinje.KOrdreLinjeNr = KOrdreLinje.KopiKOrdreLinjeNr NO-ERROR.
      IF AVAILABLE bufKKORdreLinje THEN
      DO: 
        /* Legger opp kopi på retur ordre. */
        CREATE bufKOrdreLinje.
        BUFFER-COPY bufKKOrdreLinje
          EXCEPT KOrdre_Id Faktura_Id
          TO bufKOrdreLinje
          ASSIGN 
            bufKOrdreLinje.KOrdre_Id = plRetKOrdre_Id
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
      END.
    END. /* TAR_MED_KOPI */
  END.
END. /* TRANSACTION */
