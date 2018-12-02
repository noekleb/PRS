CURRENT-WINDOW:WIDTH = 350.
FOR EACH varebehLinjeTrans WHERE 
VareBehNr = 1720000003 AND ButikkNr <> 176
    :
    DELETE VareBehLinjeTrans.
END.
