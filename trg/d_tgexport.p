TRIGGER PROCEDURE FOR DELETE OF TGExport.

FOR EACH TGEmp OF TGExport:
    DELETE TGEmp.
END.

FOR EACH TGTimeStamp OF TGExport:
    DELETE TGTimeStamp.
END.

FOR EACH TGSales_Ext OF TGExport:
    DELETE TGSales_Ext.
END.

FOR EACH TGSales OF TGExport:
    DELETE TGSales.
END.
