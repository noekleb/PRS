def input parameter iOldArtikkelNr as dec no-undo.
def input parameter iNewArtikkelNr as dec no-undo.

for each StrekKode where StrekKode.ArtikkelNr = iOldArtikkelNr:
    StrekKode.ArtikkelNr = iNewArtikkelNr.
end.

for each Individ where Individ.ArtikkelNr = iOldArtikkelNr:
    Individ.ArtikkelNr = iNewArtikkelNr.
end.

for each LevLager where LevLager.ArtikkelNr = iOldArtikkelNr:
    LevLager.ArtikkelNr = iNewArtikkelNr.
end.

for each ArtBas where ArtBas.ArtikkelNr = iOldArtikkelNr:
    ArtBas.ArtikkelNr = iNewArtikkelNr.
end.

for each ReklamasjonsLinje where ReklamasjonsLinje.ArtikkelNr = iOldArtikkelNr:
    ReklamasjonsLinje.ArtikkelNr = iNewArtikkelNr.
end.

for each ReklamasjonsLogg where ReklamasjonsLogg.ArtikkelNr = iOldArtikkelNr:
    ReklamasjonsLogg.ArtikkelNr = iNewArtikkelNr.
end.

for each Erstattningsvare where Erstattningsvare.ArtikkelNr = iOldArtikkelNr:
    Erstattningsvare.ArtikkelNr = iNewArtikkelNr.
end.

for each ArtLag where ArtLag.ArtikkelNr = iOldArtikkelNr:
    ArtLag.ArtikkelNr = iNewArtikkelNr.
end.

for each PakkeLinje where PakkeLinje.ArtikkelNr = iOldArtikkelNr:
    PakkeLinje.ArtikkelNr = iNewArtikkelNr.
end.

for each ArtPris where ArtPris.ArtikkelNr = iOldArtikkelNr:
    ArtPris.ArtikkelNr = iNewArtikkelNr.
end.

for each Lager where Lager.ArtikkelNr = iOldArtikkelNr:
    Lager.ArtikkelNr = iNewArtikkelNr.
end.

for each LevPris where LevPris.ArtikkelNr = iOldArtikkelNr:
    LevPris.ArtikkelNr = iNewArtikkelNr.
end.

for each PrisKo where PrisKo.ArtikkelNr = iOldArtikkelNr:
    PrisKo.ArtikkelNr = iNewArtikkelNr.
end.

for each BestHode where BestHode.ArtikkelNr = iOldArtikkelNr:
    BestHode.ArtikkelNr = iNewArtikkelNr.
end.

for each BestPris where BestPris.ArtikkelNr = iOldArtikkelNr:
    BestPris.ArtikkelNr = iNewArtikkelNr.
end.

for each TransLogg where TransLogg.ArtikkelNr = iOldArtikkelNr:
    TransLogg.ArtikkelNr = iNewArtikkelNr.
end.

for each TelleLinje where TelleLinje.ArtikkelNr = iOldArtikkelNr:
    TelleLinje.ArtikkelNr = iNewArtikkelNr.
end.

for each OvArt where OvArt.ArtikkelNr = iOldArtikkelNr:
    OvArt.ArtikkelNr = iNewArtikkelNr.
end.

for each OvLinje where OvLinje.ArtikkelNr = iOldArtikkelNr:
    OvLinje.ArtikkelNr = iNewArtikkelNr.
end.

for each HPrisKo where HPrisKo.ArtikkelNr = iOldArtikkelNr:
    HPrisKo.ArtikkelNr = iNewArtikkelNr.
end.

for each KundeTrans where KundeTrans.ArtikkelNr = iOldArtikkelNr:
    KundeTrans.ArtikkelNr = iNewArtikkelNr.
end.

for each MedTrans where MedTrans.ArtikkelNr = iOldArtikkelNr:
    MedTrans.ArtikkelNr = iNewArtikkelNr.
end.

for each OvBuffer where OvBuffer.ArtikkelNr = iOldArtikkelNr:
    OvBuffer.ArtikkelNr = iNewArtikkelNr.
end.

for each AltLevBas where AltLevBas.ArtikkelNr = iOldArtikkelNr:
    AltLevBas.ArtikkelNr = iNewArtikkelNr.
end.

for each ArtBestPkt where ArtBestPkt.ArtikkelNr = iOldArtikkelNr:
    ArtBestPkt.ArtikkelNr = iNewArtikkelNr.
end.

for each ArtLok where ArtLok.ArtikkelNr = iOldArtikkelNr:
    ArtLok.ArtikkelNr = iNewArtikkelNr.
end.

for each VareBokLinje where VareBokLinje.ArtikkelNr = iOldArtikkelNr:
    VareBokLinje.ArtikkelNr = iNewArtikkelNr.
end.

for each VareBehLinje where VareBehLinje.ArtikkelNr = iOldArtikkelNr:
    VareBehLinje.ArtikkelNr = iNewArtikkelNr.
end.

for each VareBehLinjeTrans where VareBehLinjeTrans.ArtikkelNr = iOldArtikkelNr:
    VareBehLinjeTrans.ArtikkelNr = iNewArtikkelNr.
end.

for each VareBehPris where VareBehPris.ArtikkelNr = iOldArtikkelNr:
    VareBehPris.ArtikkelNr = iNewArtikkelNr.
end.

for each VareBehBestHode where VareBehBestHode.ArtikkelNr = iOldArtikkelNr:
    VareBehBestHode.ArtikkelNr = iNewArtikkelNr.
end.

for each FakturaLinje where FakturaLinje.ArtikkelNr = iOldArtikkelNr:
    FakturaLinje.ArtikkelNr = iNewArtikkelNr.
end.

for each ArtSort where ArtSort.ArtikkelNr = iOldArtikkelNr:
    ArtSort.ArtikkelNr = iNewArtikkelNr.
end.

for each VareBokTemaLinje where VareBokTemaLinje.ArtikkelNr = iOldArtikkelNr:
    VareBokTemaLinje.ArtikkelNr = iNewArtikkelNr.
end.

for each KampanjeLinje where KampanjeLinje.ArtikkelNr = iOldArtikkelNr:
    KampanjeLinje.ArtikkelNr = iNewArtikkelNr.
end.

for each PkSdlLinje where PkSdlLinje.ArtikkelNr = iOldArtikkelNr:
    PkSdlLinje.ArtikkelNr = iNewArtikkelNr.
end.

for each PkSdlPris where PkSdlPris.ArtikkelNr = iOldArtikkelNr:
    PkSdlPris.ArtikkelNr = iNewArtikkelNr.
end.

for each VPIMottak where VPIMottak.ArtikkelNr = iOldArtikkelNr:
    VPIMottak.ArtikkelNr = iNewArtikkelNr.
end.

