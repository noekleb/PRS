
def stream Ut.

output stream Ut to value('Tabeller_med_Artikkelnr.csv.').

put stream Ut unformatted.

for each _field no-lock where
  _field._Field-Name = 'ArtikkelNr':
  find _File of _Field no-lock no-error.
  
  put stream Ut unformatted
    _File._File-Name + '.' + _Field._Field-Name skip.
end.

output stream Ut close.
