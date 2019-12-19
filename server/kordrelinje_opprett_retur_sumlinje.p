
/*------------------------------------------------------------------------
    File        : kordrelinje_opprett_retur_sumlinje.p
    Purpose     : 

    Syntax      :

    Description : Oppretter/oppdaterer sumlinje på retur odre.

    Author(s)   : tomn
    Created     : Wed Aug 21 11:27:42 CEST 2019
    Notes       : RUN kordrelinje_opprett_retur_sumlinje.p (KOrdreHode.KOrdre_Id, dSum).
                  Er sum <> 0 skal sum benyttes. Hvis ikke beregnes den her.          
      
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE INPUT PARAMETER plKOrdre_Id AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER plRefKOrdre_Id AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER dSum AS DECIMAL NO-UNDO.

DEFINE VARIABLE bTest AS LOG NO-UNDO.
DEFINE VARIABLE cLogg AS CHARACTER NO-UNDO.

DEFINE BUFFER bufKOrdreLinje FOR KOrdreLinje.
DEFINE BUFFER buf2KOrdreLinje FOR KOrdreLinje.
DEFINE BUFFER orgKOrdreLinje FOR KOrdreLinje.

DEFINE VARIABLE rStandardFunksjoner AS cls.StdFunk.StandardFunksjoner NO-UNDO.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

ASSIGN
    bTest = TRUE 
    cLogg = 'kordrelinje_opprett_retur_sumlinje' + REPLACE(STRING(TODAY),'/','')
    .
rStandardFunksjoner  = NEW cls.StdFunk.StandardFunksjoner( cLogg ) NO-ERROR.

IF bTest THEN 
DO:
  rStandardFunksjoner:SkrivTilLogg(cLogg,
      'Start' 
      ).    
  rStandardFunksjoner:SkrivTilLogg(cLogg,
      '  Parametre: ' 
      ).    
  rStandardFunksjoner:SkrivTilLogg(cLogg,
      '    plKOrdre_Id   : ' + STRING(plKOrdre_Id) 
      ).    
  rStandardFunksjoner:SkrivTilLogg(cLogg,
      '    plRefKOrdre_Id: ' + STRING(plRefKOrdre_Id) 
      ).    
  rStandardFunksjoner:SkrivTilLogg(cLogg,
      '    dSum          : ' + STRING(dSum) 
      ).    
END.

/* Finner betalingslinjen på den opprinnelige ordren. */
FIND FIRST KOrdreLinje NO-LOCK WHERE 
  KOrdreLinje.KOrdre_Id = plRefKordre_Id AND
  KOrdreLinje.VareNr    = "BETALT" NO-ERROR.
IF AVAILABLE KOrdreLinje THEN 
DO TRANSACTION:
  /* Sjekker om det ligger sumlinje på den nye ordren. */
  FIND FIRST bufKOrdreLinje EXCLUSIVE-LOCK WHERE 
    bufKOrdreLinje.KOrdre_Id = plKOrdre_Id AND
    bufKOrdreLinje.VareNr    = "BETALT" NO-ERROR.
  /* Oppretter den hvis den ikke finnes. */
  IF NOT AVAILABLE bufKOrdreLinje THEN
    DO: 
      CREATE bufKOrdreLinje.
      BUFFER-COPY KOrdreLinje
          EXCEPT KOrdre_Id
          TO bufKOrdreLinje
       ASSIGN 
        bufKOrdreLinje.KOrdre_Id = plKOrdre_Id
        .
      IF bTest THEN 
        rStandardFunksjoner:SkrivTilLogg(cLogg,
            '  Linje opprettet - linjenr: ' + STRING(bufKOrdreLinje.KOrdreLinjeNr) 
            ).    
    END.
    ELSE 
      IF bTest THEN 
        rStandardFunksjoner:SkrivTilLogg(cLogg,
            '  Linje funnet - linjenr: ' + STRING(bufKOrdreLinje.KOrdreLinjeNr) 
            ).    
  
  IF dSum = 0 THEN 
    DO:

      IF bTest THEN 
        rStandardFunksjoner:SkrivTilLogg(cLogg,
            '  Start beregne sum.' 
            ).    

      /* Summerer aktive linjer. */
      FOR EACH buf2KORdreLinje NO-LOCK WHERE 
        buf2KORdreLinje.KORdre_Id = plKOrdre_Id AND 
        buf2KOrdreLinje.Aktiv = TRUE:
        IF buf2KOrdreLinje.VareNr = 'BETALT' THEN 
          NEXT.
        dSum = dSum + buf2KOrdreLinje.nettolinjesum.
      END.
      
      IF bTest THEN 
        rStandardFunksjoner:SkrivTilLogg(cLogg,
            '    sum aktive linjer: ' + STRING(dSum) 
            ).    
      
      /* Summerer passive linjer hvor vare er byttet og vare på linjen på original ordre ikke er byttet. */
      FOR EACH buf2KORdreLinje NO-LOCK WHERE 
        buf2KORdreLinje.KORdre_Id = plKOrdre_Id AND 
        buf2KOrdreLinje.Aktiv = FALSE AND 
        buf2KOrdreLinje.KopiKOrdreLinje > 0:
        IF buf2KOrdreLinje.VareNr = 'BETALT' THEN 
          NEXT.

        /* Henter opprinnelig linje. */
        FIND orgKOrdreLinje NO-LOCK WHERE 
          orgKOrdreLinje.KOrdre_Id = plRefKOrdre_Id AND 
          orgKOrdreLinje.KOrdreLinjeNr = buf2KOrdreLinje.KOrdreLinje NO-ERROR.
        /* Er vare ikke byttet på denne linje, skal verdien med i ordresummen. */
        IF AVAILABLE orgKOrdreLinje AND orgKORdreLinje.KopiKOrdreLinjeNr = 0 THEN
        DO:    
          dSum = dSum + buf2KOrdreLinje.nettolinjesum.
        END.
      END.
      
      IF bTest THEN 
        rStandardFunksjoner:SkrivTilLogg(cLogg,
            '    Sum beregnet: ' + STRING(dSum) 
            ).    
    END.
    
  ASSIGN 
    bufKOrdreLinje.Antall        = 0
    bufKOrdreLinje.nettolinjesum = dSum 
    bufKOrdreLinje.NettoPris     = bufKOrdreLinje.nettolinjesum
    bufKOrdreLinje.MvaKr         = 0
    bufKOrdreLinje.Mva%          = 0
    bufKOrdreLinje.BruttoPris    = bufKOrdreLinje.NettoPris
    bufKOrdreLinje.Pris          = bufKOrdreLinje.NettoPris
    bufKOrdreLinje.Linjesum      = bufKOrdreLinje.NettoPris
    bufKOrdreLinje.Leveringsdato = TODAY 
    bufKOrdreLinje.Faktura_Id    = 0
    .
  IF bTest THEN 
    rStandardFunksjoner:SkrivTilLogg(cLogg,
        '  Linje oppdatert med ny info.' 
        ).    
  RELEASE bufKOrdreLinje.                        
END.  

IF bTest THEN 
  rStandardFunksjoner:SkrivTilLogg(cLogg,
      'Slutt' 
      ).    

