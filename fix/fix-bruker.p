for each _user no-lock:
  find Bruker where
    Bruker.BrukerId = _User._Userid no-error.
    if not available Bruker then
      do:
        create Bruker.
        assign
          Bruker.BrukerId = _User._UserId
          Bruker.NAvn     = _User._UserId
          Bruker.Lng      = "DES"
          Bruker.BrGrpNr  = 1.
      end.
end.
