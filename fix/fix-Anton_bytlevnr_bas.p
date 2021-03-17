def input parameter iOldLevNr as int no-undo.
def input parameter iNewLevNr as int no-undo.

/* Skal ikke konverteres, har lest inn leverandører fra Sport1. 
for each LevBas where LevBas.levnr = iOldLevNr:
    LevBas.levnr = iNewLevNr.
end.
*/

for each EkstVPILev where EkstVPILev.LevNr = iOldLevNr:
    EkstVPILev.LevNr = iNewLevNr.
end.

for each Forsalj where Forsalj.LevNr = iOldLevNr:
    Forsalj.LevNr = iNewLevNr.
end.

for each Individ where Individ.levnr = iOldLevNr:
    Individ.levnr = iNewLevNr.
end.

for each LevLager where LevLager.LevNr = iOldLevNr:
    LevLager.LevNr = iNewLevNr.
end.

for each ArtBas where ArtBas.LevNr = iOldLevNr:
    ArtBas.LevNr = iNewLevNr.
end.

for each ReklamasjonsLogg where ReklamasjonsLogg.LevNr = iOldLevNr:
    ReklamasjonsLogg.LevNr = iNewLevNr.
end.

for each Bilderegister where Bilderegister.LevNr = iOldLevNr:
    Bilderegister.LevNr = iNewLevNr.
end.

for each LevSort where LevSort.LevNr = iOldLevNr:
    LevSort.LevNr = iNewLevNr.
end.

for each LevSAnt where LevSAnt.LevNr = iOldLevNr:
    LevSAnt.LevNr = iNewLevNr.
end.

for each ArtPris where ArtPris.LevNr = iOldLevNr:
    ArtPris.LevNr = iNewLevNr.
end.

for each LevPris where LevPris.LevNr = iOldLevNr:
    LevPris.LevNr = iNewLevNr.
end.

for each PrisKo where PrisKo.LevNr = iOldLevNr:
    PrisKo.LevNr = iNewLevNr.
end.

for each BestHode where BestHode.LevNr = iOldLevNr:
    BestHode.LevNr = iNewLevNr.
end.

for each Ordre where Ordre.LevNr = iOldLevNr:
    Ordre.LevNr = iNewLevNr.
end.

for each TransLogg where TransLogg.LevNr = iOldLevNr:
    TransLogg.LevNr = iNewLevNr.
end.

for each TelleLinje where TelleLinje.LevNr = iOldLevNr:
    TelleLinje.LevNr = iNewLevNr.
end.

for each HPrisKo where HPrisKo.LevNr = iOldLevNr:
    HPrisKo.LevNr = iNewLevNr.
end.

for each KundeTrans where KundeTrans.LevNr = iOldLevNr:
    KundeTrans.LevNr = iNewLevNr.
end.

for each MedTrans where MedTrans.LevNr = iOldLevNr:
    MedTrans.LevNr = iNewLevNr.
end.

for each VarebokTemaHode where VarebokTemaHode.LevNr = iOldLevNr:
    VarebokTemaHode.LevNr = iNewLevNr.
end.

for each AltLevBas where AltLevBas.LevNr = iOldLevNr:
    AltLevBas.LevNr = iNewLevNr.
end.

for each VareBokLinje where VareBokLinje.levnr = iOldLevNr:
    VareBokLinje.levnr = iNewLevNr.
end.

for each VareBehLinje where VareBehLinje.levnr = iOldLevNr:
    VareBehLinje.levnr = iNewLevNr.
end.

for each VareBehBestHode where VareBehBestHode.levnr = iOldLevNr:
    VareBehBestHode.levnr = iNewLevNr.
end.

for each LevKontakt where LevKontakt.LevNr = iOldLevNr:
    LevKontakt.LevNr = iNewLevNr.
end.

for each ArtSort where ArtSort.LevNr = iOldLevNr:
    ArtSort.LevNr = iNewLevNr.
end.

for each BrukerLev where BrukerLev.LevNr = iOldLevNr:
    BrukerLev.LevNr = iNewLevNr.
end.

for each VareBokTemaLinje where VareBokTemaLinje.LevNr = iOldLevNr:
    VareBokTemaLinje.LevNr = iNewLevNr.
end.

for each DefaultLevDato where DefaultLevDato.LevNr = iOldLevNr:
    DefaultLevDato.LevNr = iNewLevNr.
end.

for each Varetrans where Varetrans.LevNr = iOldLevNr:
    Varetrans.LevNr = iNewLevNr.
end.

for each PkSdlLinje where PkSdlLinje.LevNr = iOldLevNr:
    PkSdlLinje.LevNr = iNewLevNr.
end.

for each PkSdlHode where PkSdlHode.levnr = iOldLevNr:
    PkSdlHode.levnr = iNewLevNr.
end.

for each VPIMottak where VPIMottak.LevNr = iOldLevNr:
    VPIMottak.LevNr = iNewLevNr.
end.

