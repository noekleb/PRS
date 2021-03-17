/*
  OppdaterPkSdlHodeMedFakturaNr.p
  
  Går igjennom mottatte pakksedler og sjekker at de har fått påført fakturanr.
  Det er outlet butikkene og nettbutikkens lager som skal sjekkes.
  Sjekker 5 dager bakover i tid.
  
*/

DEFINE VARIABLE iAntFakt     AS INTEGER                   NO-UNDO.
DEFINE VARIABLE iAntLev      AS INTEGER                   NO-UNDO.
DEFINE VARIABLE iButNr       AS INTEGER                   NO-UNDO.
DEFINE VARIABLE dDato        AS DATE                      NO-UNDO.
DEFINE VARIABLE iPkSdlStatus AS INTEGER                   NO-UNDO.
DEFINE VARIABLE lFakturaNr   AS DECIMAL                   FORMAT ">>>>>>>>>>>>>>9" NO-UNDO.
DEFINE VARIABLE cOutletLst AS CHARACTER NO-UNDO.
DEFINE VARIABLE cNettLagLst AS CHARACTER NO-UNDO.
DEFINE VARIABLE cButLst AS CHARACTER NO-UNDO.
DEFINE VARIABLE iLoop AS INTEGER NO-UNDO.

DEFINE VARIABLE rPakkseddel  AS cls.Pakkseddel.Pakkseddel NO-UNDO. 

rPakkseddel  = NEW cls.Pakkseddel.Pakkseddel( ).    

{syspara.i 22 5 2 cOutletLst}
{syspara.i 150 1 3 cNettLagLst}

ASSIGN 
  cButLst      = TRIM(cOutletLst + ',' + cNettLagLst,',')
  dDato        = TODAY - 100 
  iPkSdlStatus = 20 /* Mottatt status */
  .

BUTBLOKK:
DO iLoop = 1 TO NUM-ENTRIES(cButLst):
  iButNr = INT(ENTRY(iLoop,cButLst)).
  
  FOR EACH PkSdlHode EXCLUSIVE-LOCK WHERE 
    PkSdlHode.ButikkNr = iButNr AND 
    PkSdlHode.PkSdlStatus = iPkSdlStatus, 
    FIRST PkSdlMottak OF PkSdlHode NO-LOCK WHERE 
    PkSdlMottak.MottattDato >= dDato:
    
    FIND Butiker NO-LOCK WHERE 
      Butiker.butik = PkSdlHode.ButikkNr NO-ERROR.
    
    /* Korrigerer fakturanr hvis det finnes en faktura på pakkseddelen. */
    IF PkSdlHode.FakturaNr = ? THEN
      lFakturaNr = rPakkseddel:setFakturaNrIPkSdlHode( PkSdlHode.PkSdlId ).          
  END.
END. /* BUTBLOKK */  

