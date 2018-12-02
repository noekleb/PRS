/************************************************************
    Program:  rutskrkolleksjon.i
    Created:  TN   23 Feb 100
Description:
      {rutskrkolleksjon.i &SysHId=6 &Fra=301 &Til=399 &Ekstra=" "}


Last change:  TN   23 Feb 100   12:17 pm
************************************************************/

do:
  assign
    CB-Layout = " "
    CB-Layout:List-Items = " ".
  for each SysGruppe no-lock where
    SysGruppe.SysHId  = {&SysHId} and
    SysGruppe.SysGr  >= {&Fra} and
    SysGruppe.SysGr  <= {&Til}
    {&Ekstra} :

    CB-Layout:List-items = CB-Layout:list-items +
                           (if CB-Layout:list-items = ""
                             then ""
                             else ",") +
                           string(SysGruppe.SysGr,"zzz9") + ": " +
                           (if num-entries(SysGruppe.Beskrivelse,":") = 2
                              then entry(2,SysGruppe.Beskrivelse,":")
                              else SysGruppe.Beskrivelse).
  end.
  assign
    CB-Layout = entry(1,CB-Layout:list-items).
end.


