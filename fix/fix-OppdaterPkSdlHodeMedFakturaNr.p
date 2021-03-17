DEF VAR iAntFakt AS INT NO-UNDO.
DEF VAR iAntLev AS INT NO-UNDO.
DEFINE VARIABLE iButNr AS INTEGER NO-UNDO.
DEFINE VARIABLE dDato AS DATE NO-UNDO.
DEFINE VARIABLE iPkSdlStatus AS INTEGER NO-UNDO.
DEFINE VARIABLE lFakturaNr AS DECIMAL FORMAT ">>>>>>>>>>>>>>9" NO-UNDO.

DEFINE VARIABLE rPakkseddel AS cls.Pakkseddel.Pakkseddel NO-UNDO. 

CURRENT-WINDOW:WIDTH = 350.
CURRENT-WINDOW:HEIGHT = 45. 

rPakkseddel  = NEW cls.Pakkseddel.Pakkseddel( ).    

ASSIGN 
  iButNr       = 16
  dDato        = 08/14/2020
  iPkSdlStatus = 20
  .

FOR EACH PkSdlHode EXCLUSIVE-LOCK WHERE 
  PkSdlHode.ButikkNr = iButNr AND 
  PkSdlHode.PkSdlStatus = iPkSdlStatus AND 
  PkSdlHode.PkSdlNr = '3000361',
  FIRST PkSdlMottak OF PkSdlHode NO-LOCK WHERE 
      PkSdlMottak.MottattDato >= dDato:
  
  FIND Butiker NO-LOCK WHERE 
    Butiker.butik = PkSdlHode.ButikkNr NO-ERROR.
  
  /* Korrigerer fakturanr hvis det finnes en faktura på pakkseddelen. */
  IF PkSdlHode.FakturaNr = ? THEN
  DO:
    lFakturaNr = rPakkseddel:setFakturaNrIPkSdlHode( PkSdlHode.PkSdlId ).
  END.
        
  DISPLAY
      PkSdlHode.ButikkNr
      PkSdlHode.PkSdlNr
      PkSdlMottak.MottattDato
      STRING(PkSdlMottak.MottattTid,"HH:MM:SS")
      PkSdlHode.PkSdlOpphav
      PkSdlHode.OrdreType
      Butiker.KundeNr WHEN AVAILABLE Butiker
      PkSdlHode.FakturaNr
      lFakturaNr
  WITH WIDTH 350.
END.
