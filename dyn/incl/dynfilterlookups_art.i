/* Use in FlatViewRecord to enable lookups in dynamic filter */
DYNAMIC-FUNCTION("setAttribute",hFlatBrw,"filterlookupfields_levnamn","LevBas;levnamn;levnr").
DYNAMIC-FUNCTION("setAttribute",hFlatBrw,"filterlookupquery_levnamn","where false").
DYNAMIC-FUNCTION("setAttribute",hFlatBrw,"filterlookupreturnfield_levnamn","levnamn").

DYNAMIC-FUNCTION("setAttribute",hFlatBrw,"filterlookupfields_levnr","LevBas;levnr;levnamn").
DYNAMIC-FUNCTION("setAttribute",hFlatBrw,"filterlookupquery_levnr","where false").
DYNAMIC-FUNCTION("setAttribute",hFlatBrw,"filterlookupreturnfield_levnr","levnr").

DYNAMIC-FUNCTION("setAttribute",hFlatBrw,"filterlookupfields_vg","VarGr;Vg;VgBeskr").
DYNAMIC-FUNCTION("setAttribute",hFlatBrw,"filterlookupquery_vg","where false").
DYNAMIC-FUNCTION("setAttribute",hFlatBrw,"filterlookupreturnfield_vg","vg").

DYNAMIC-FUNCTION("setAttribute",hFlatBrw,"filterlookupfields_hg","HuvGr;hg;hgBeskr").
DYNAMIC-FUNCTION("setAttribute",hFlatBrw,"filterlookupquery_hg","where false").
DYNAMIC-FUNCTION("setAttribute",hFlatBrw,"filterlookupreturnfield_hg","hg").

DYNAMIC-FUNCTION("setAttribute",hFlatBrw,"filterlookupfields_AvdelingNr","Avdeling;AvdelingNr;AvdelingNavn").
DYNAMIC-FUNCTION("setAttribute",hFlatBrw,"filterlookupquery_AvdelingNr","where false").
DYNAMIC-FUNCTION("setAttribute",hFlatBrw,"filterlookupreturnfield_AvdelingNr","AvdelingNr").

