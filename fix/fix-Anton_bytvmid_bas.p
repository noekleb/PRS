def input parameter iOldVmId as int no-undo.
def input parameter iNewVmId as int no-undo.

for each ArtBas where ArtBas.VMId = iOldVmId:
    ArtBas.VMId = iNewVmId.
end.

for each Varemerke where Varemerke.VMId = iOldVmId:
    Varemerke.VMId = iNewVmId.
end.

for each Individ where Individ.VMId = iOldVmId:
    Individ.VMId = iNewVmId.
end.

