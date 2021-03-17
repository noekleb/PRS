/*delete from Behandlingskode.*/
DO TRANSACTION:
  DELETE FROM BestHLev.
  DELETE FROM BestHode.
  DELETE FROM BestKasse.
  DELETE FROM BestLevert.
  DELETE FROM BestLinje.
  DELETE FROM BestPris.
  DELETE FROM BestSort.
  DELETE FROM BestStr.
END.
FOR EACH Fributik:
    DELETE fributik.
END.
FOR EACH PKSdlHode:
    FOR EACH PKSDLLinje OF PKSdlHode:
        DELETE PkSDLLinje.
    END.
    DELETE PKSdlHode.
END.
FOR EACH PkSdlPris:
    DELETE PkSdlPris.
END.
FOR EACH PkSdlMottak:
    DELETE PkSdlMottak.
END.
