FOR EACH ArtBAs WHERE 
    ArtBAs.Artslag = 0:
    ArtBAs.SalgsEnhet = 'Stk'.
END.
FOR EACH ArtBAs WHERE 
    ArtBAs.Artslag = 1:
    ArtBAs.SalgsEnhet = 'Kg'.
END.
FOR EACH ArtBAs WHERE 
    ArtBAs.Artslag = 5:
    ArtBAs.SalgsEnhet = 'Liter'.
END.

