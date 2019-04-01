FOR EACH TGExport WHERE 
    TGExport.ButikkNr >= 0 AND
    TGExport.TGSalesDate >= TODAY - 5:

    DELETE TGExport.
END.
