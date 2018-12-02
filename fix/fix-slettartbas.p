DEF VAR wTekst AS CHAR NO-UNDO.

FOR EACH ArtBas EXCLUSIVE-LOCK:
  /* Fikser bestillingene */
  for each BestHode of ArtBas no-lock:
    RUN SlettBestilling(RECID(BestHode)).
  end.
        
  SLETTING:
  DO:

    /* Nullstiller VPI koblinger. */
    FOR EACH VPIArtBas WHERE
        VPIArtBas.ArtikkelNr = ArtBas.ArtikkelNr:
        ASSIGN
            VPIArtBas.ArtikkelNr = 0
            .
    END.

    /* Sletter kundetransaksjoner */
    FOR EACH KundeTrans WHERE KundeTrans.ArtikkelNr = ArtBAs.ArtikkelNr EXCLUSIVE-LOCK:
        DELETE KundeTrans.
    END.
    
    /* Sletter medlemstransaksjoner */
    FOR EACH MedTrans WHERE MedTrans.ArtikkelNr = ArtBas.ArtikkelNr EXCLUSIVE-LOCK:
        DELETE MedTrans.
    END.

    /* Sletter lageret */
    for each Lager of ArtBas exclusive-lock:
      delete Lager.
    end.
    
    /* Skal stLager tabellen trekkes ned ? */    
    for each ArtLag exclusive-lock where
      ArtLag.Vg    = ArtBas.Vg and
      ArtLag.LopNr = ArtBas.LopNr:
      /* ArtLag.ArtikkelNr = ArtBas.ArtikkelNr: */
      delete ArtLag.
    end.

    /* Sletter prisinformasjonen */
    for each ArtPris of ArtBas exclusive-lock:
      delete ArtPris.
    end.

    /* Sletter priskøposter */
    FOR EACH PrisKo OF ArtBas EXCLUSIVE-LOCK:
        DELETE PrisKo.
    END.

    /* Sletter prishistorikk */
    FOR EACH HPrisko OF ArtBAs EXCLUSIVE-LOCK:
        DELETE HPrisKo.
    END.

    /* Sletter transaksjoner på artikkelen. */
    FOR EACH TransLogg EXCLUSIVE-LOCK WHERE
      TransLogg.ArtikkelNr = ArtBas.ArtikkelNr:
      DELETE TransLogg.
    END.

    /* Sletter artikkelstatistikken. */
    FOR EACH StLinje EXCLUSIVE-LOCK WHERE
      StLinje.StType     = "ARTIKKEL" AND
      StLinje.DataObjekt = string(ArtBas.ArtikkelNr,"9999999999999"):
      DELETE StLinje.
    END.

    /* Sletter Pakkelinjer om Pakke */
    IF ArtBas.Pakke THEN DO:
        FOR EACH PakkeLinje OF ArtBas:
            DELETE PakkeLinje.
        END.
    END.

    /* Sletter StrekKoder */
    FOR EACH StrekKode OF ArtBas:
        DELETE StrekKode.
    END.

    /* Sletter peker for overføring av restpar. */
    {syspara.i 1 2 2 wTekst}
    if wTekst <> "" then
      for each KonvReg exclusive-lock where
        KonvReg.EDB-System = wTekst and
        KonvReg.Tabell     = "" and
        KonvReg.InterntId  = string(ArtBas.ArtikkelNr):
        delete KonvReg.
      end.

    delete ArtBas.
  END. 
END.

PROCEDURE SlettBestilling:
    DEFINE INPUT  PARAMETER rBestHodeRecid AS RECID      NO-UNDO.

  DO TRANSACTION:
      {sww.i}
      /* KanSlettes*/      
      RUN w-gridord.w (INPUT recid(ArtBas), INPUT-OUTPUT rBestHodeRecid, "SLETT").
      {swn.i}
      RETURN "OK".
  END.

END PROCEDURE.
