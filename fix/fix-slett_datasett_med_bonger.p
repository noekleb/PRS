CURRENT-WINDOW:WIDTH = 350.
FOR EACH DataSett EXCLUSIVE-LOCK WHERE 
    Datasett.DataSEttId >= 1789664 AND 
    DataSett.DataSettId <= 1789764,
    EACH Filer OF Datasett EXCLUSIVE-LOCK,
    EACH BongHode OF DataSett EXCLUSIVE-LOCK:

    FOR EACH FilLinje OF Filer:
        DELETE FilLinje.
    END.
    FOR EACH BongLinje WHERE 
        BongLinje.B_id = BongHode.B_Id EXCLUSIVE-LOCK:
        DELETE BongLinje.
    END.

    DISPLAY
        DataSett.DataSettId
        DataSett.Dato
        DataSett.ButikkNr
        Filer.FilId
        BongHode.BongNr
    WITH WIDTH 350.

    DELETE BongHode.
    DELETE DataSett.

END.
