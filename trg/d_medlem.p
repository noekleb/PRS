TRIGGER PROCEDURE FOR DELETE OF Medlem.

FOR EACH MedlemsKort OF Medlem:
    DELETE Medlemskort.
END.
FOR EACH MedTrans OF Medlem:
    DELETE MedTrans.
END.
FOR EACH MedKjop OF Medlem:
    DELETE MedKjop.
END.
FOR EACH MedlemBetTrans OF Medlem:
    DELETE MedlemBetTrans.
END.
FOR EACH MedlemSaldo WHERE MedlemSaldo.MedlemsNr = Medlem.MedlemsNr:
    DELETE MedlemSaldo.
END.
