DEF INPUT PARAMETER dMedlemsNr LIKE medlem.MedlemsNr NO-UNDO.

FIND Medlem NO-LOCK WHERE 
    Medlem.MedlemsNr = dMedlemsNr NO-ERROR.
IF AVAILABLE Medlem THEN
DO:
  FOR EACH Medlemskort OF Medlem EXCLUSIVE-LOCK:
    DELETE MedlemsKort.
  END.
  
  FOR EACH MedTrans OF Medlem EXCLUSIVE-LOCK:
    DELETE MedTrans.
  END.
  
  FOR EACH MedlemBetTrans OF Medlem EXCLUSIVE-LOCK:
    DELETE MedlemBetTrans.
  END.
  
  FOR EACH MedKjop OF Medlem EXCLUSIVE-LOCK:
    DELETE MedKjop.
  END.
  
  FOR EACH MedRabSjekk OF Medlem EXCLUSIVE-LOCK:
    DELETE MedRabSjekk.
  END.
  
  FOR EACH MedRabReskontr OF Medlem EXCLUSIVE-LOCK:
    DELETE MedRabReskontr.
  END.
  
  FOR EACH MedlemSaldo WHERE 
    MedlemSaldo.MedlemsNr = Medlem.MedlemsNr EXCLUSIVE-LOCK:
    DELETE MedlemSaldo.
  END.
  FIND CURRENT Medlem EXCLUSIVE-LOCK.
  IF AVAILABLE Medlem THEN DELETE Medlem.
END.
