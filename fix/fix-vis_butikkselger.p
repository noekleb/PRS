CURRENT-WINDOW:WIDTH = 300.
FOR EACH ButikkSelger WHERE SelgerId = 6,
    FIRST Selger OF ButikkSelger
     BY SelgerId:
    DISPLAY
        Selger.NAvn
        WITH WIDTH 300.
    DISPLAY
        ButikkSelger
        WITH WIDTH 300.

END.
