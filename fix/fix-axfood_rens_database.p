FOR EACH Butiker WHERE Butiker.Butik <> 14:
    FOR EACH Kasse WHERE
        Kasse.ButikkNr = Butiker.Butik:

        DELETE kasse.

    END.
    DELETE Butiker.
END.

FOR EACH PrisProfil WHERE PrisProfil.ProfilNr > 1:
    DELETE PrisProfil.
END.

FOR EACH VarGr:
    DELETE VarGr.
END.
FOR EACH HuvGr:
    DELETE HuvGr.
END.
FOR EACH SkoTex.Avdeling:
    DELETE SkoTex.Avdeling.
END.
FOR EACH SkoTex.Butiker:
    Butiker.ProfilNr = 1. 
END.
FOR EACH ArtPris WHERE
    ArtPris.ProfilNr > 1:
    DELETE ArtPris.
END.

FOR EACH LevBas:
    DELETE LevBas.
END.
FOR EACH SkoTex.Produsent:
    DELETE SkoTex.Produsent.
END.
FOR EACH SkoTex.Post:
    DELETE SkoTex.Post.
END.
FOR EACH Kommune:
    DELETE Kommune.
END.
FOR EACH Fylke:
    DELETE Fylke.
END.
FOR EACH Moms:
    DELETE Moms.
END.
FOR EACH Strekkode:
    DELETE Strekkode.
END.
FOR EACH ArtPris:
    DELETE ArtPris.
END.
FOR EACH ArtBas:
    DELETE ArtBas.
END.
FOR EACH ELogg:
    DELETE ELogg.
END.
FOR EACH Selger:
  DELETE Selger.
END.
FOR EACH ButikkSelger:
  DELETE ButikkSelger.
END.
FOR EACH Forsalj:
  DELETE Forsalj.
END.
FOR EACH ButikkForsalj:
  DELETE ButikkForsalj.
END.
FOR EACH Kunde:
  FOR EACH KundeKort OF Kunde:
    DELETE kundekort.
  END.
  DELETE Kunde.
END.
FOR EACH Medlem:
  FOR EACH MedlemsKort:
    DELETE MedlemsKort.
  END.
  DELETE Medlem.
END.