/*******************************************************************************

    Program:        itemlist.p
    
    Description:    Sample PDF generation using PDFinlucde.  This sample 
                    illustrates many different options available with PDFinclude.
                    Including font embedding, image embedding, and text
                    placements etc.

*******************************************************************************/

{ pdf_inc.i "NOT SUPER"}

DEFINE VARIABLE Vitems      AS INTEGER NO-UNDO.
DEFINE VARIABLE Vrow        AS INTEGER NO-UNDO.
DEFINE VARIABLE Vcat-desc   AS CHARACTER EXTENT 4 FORMAT "X(40)" NO-UNDO.

/* Create stream for new PDF file */
RUN pdf_new IN h_PDFinc ("Spdf","\gord\PDFinclude\samples\persistent\itemlist.pdf").

/* Load Bar Code Font */
RUN pdf_load_font IN h_PDFinc ("Spdf","Code39","\gord\PDFinclude\samples\support\code39.ttf","\gord\PDFinclude\samples\support\code39.afm",""). 

/* Load and Image that we will use to show how to place onto the page */
RUN pdf_load_image IN h_PDFinc ("Spdf","Product","\gord\PDFinclude\samples\support\product.jpg"). 

/* Set Document Information */ 
RUN pdf_set_info IN h_PDFinc ("Spdf","Author","Gordon Campbell").
RUN pdf_set_info IN h_PDFinc ("Spdf","Subject","Inventory").
RUN pdf_set_info IN h_PDFinc ("Spdf","Title","Item Catalog").
RUN pdf_set_info IN h_PDFinc ("Spdf","Keywords","Item Catalog Inventory").
RUN pdf_set_info IN h_PDFinc ("Spdf","Creator","PDFinclude V2").
RUN pdf_set_info IN h_PDFinc ("Spdf","Producer","itemlist.p").

/* Instantiate a New Page */
RUN new_page.

/* Loop through appropriate record set */
FOR EACH ITEM NO-LOCK 
    BREAK BY ItemNum:

  Vitems = Vitems + 1.
 
  RUN display_item_info.

  /* Process Record Counter */
  IF Vitems = 6 THEN
    RUN new_page.

END.

RUN pdf_close IN h_PDFinc ("Spdf").

/* -------------------- INTERNAL PROCEDURES -------------------------- */

PROCEDURE display_item_info:

  /* Draw main item Box */
  RUN pdf_stroke_fill IN h_PDFinc ("Spdf",.8784,.8588,.7098).
  RUN pdf_rect IN h_PDFinc ("Spdf", pdf_LeftMargin("Spdf"), Vrow, 
                        pdf_PageWidth("Spdf") - 20 , 110,1.0).

  /* Draw Smaller box (beige filled) to contain Category Description */
  RUN pdf_rect IN h_PDFinc ("Spdf", 350, Vrow + 10, 240, 45,1.0).

  /* Draw Smaller box (white filled) to contain Item Picture (when avail) */
  RUN pdf_stroke_fill IN h_PDFinc ("Spdf",1.0,1.0,1.0).
  RUN pdf_rect IN h_PDFinc ("Spdf", pdf_LeftMargin("Spdf") + 10, Vrow + 10, 
                        pdf_LeftMargin("Spdf") + 100  , 90,1.0).

  /* Place Link around the Image Box */
  RUN pdf_link IN h_PDFinc 
              ("Spdf", 
                20,                            
                pdf_GraphicY("Spdf") - 90 ,    
                130,                           
                pdf_GraphicY("Spdf"),          
                "http://www.epro-sys.com?ItemNum=" + STRING(Item.ItemNum ),
                1,
                0,
                0,
                1,
                "P").

  /* Display a JPEG picture in the First Box of each Frame */
  IF Vitems = 1 THEN DO:
    RUN pdf_place_image  IN h_PDFinc 
                        ("Spdf","Product",
                         pdf_LeftMargin("spdf") + 12, 
                         pdf_PageHeight("Spdf") - Vrow - 13,
                         pdf_LeftMargin("Spdf") + 95, 86).
  END.

  /* Display Labels with Bolded Font */
  RUN pdf_set_font IN h_PDFinc ("Spdf","Courier-Bold",10.0).
  RUN pdf_text_xy IN h_PDFinc ("Spdf","Part Number:", 140, Vrow + 90).
  RUN pdf_text_xy IN h_PDFinc ("Spdf","Part Name:", 140, Vrow + 80).
  RUN pdf_text_xy IN h_PDFinc ("Spdf","Category 1:", 140, Vrow + 40).
  RUN pdf_text_xy IN h_PDFinc ("Spdf","Category 2:", 140, Vrow + 30).
  RUN pdf_text_xy IN h_PDFinc ("Spdf","Qty On-Hand:", 350, Vrow + 90).
  RUN pdf_text_xy IN h_PDFinc ("Spdf","Price:", 350, Vrow + 80).
  RUN pdf_text_xy IN h_PDFinc ("Spdf","Category Description:", 350, Vrow + 60).
  
  /* Display Fields with regular Font */
  RUN pdf_set_font IN h_PDFinc ("Spdf","Courier",10.0).
  RUN pdf_text_xy IN h_PDFinc ("Spdf",STRING(item.ItemNuM), 230, Vrow + 90).
  RUN pdf_text_xy IN h_PDFinc ("Spdf",item.ItemName, 230, Vrow + 80).
  RUN pdf_text_xy IN h_PDFinc ("Spdf",item.Category1, 230, Vrow + 40).
  RUN pdf_text_xy IN h_PDFinc ("Spdf",item.Category2, 230, Vrow + 30).
  RUN pdf_text_xy IN h_PDFinc ("Spdf",STRING(item.OnHand), 440, Vrow + 90).
  RUN pdf_text_xy IN h_PDFinc ("Spdf",TRIM(STRING(item.Price,"$>>,>>9.99-")), 440, Vrow + 80).

  /* Now Load and Display the Category Description */
  RUN load_cat_desc.
  RUN pdf_text_xy IN h_PDFinc ("Spdf",Vcat-desc[1], 352, Vrow + 46).
  RUN pdf_text_xy IN h_PDFinc ("Spdf",Vcat-desc[2], 352, Vrow + 36).
  RUN pdf_text_xy IN h_PDFinc ("Spdf",Vcat-desc[3], 352, Vrow + 26).
  RUN pdf_text_xy IN h_PDFinc ("Spdf",Vcat-desc[4], 352, Vrow + 16).

  /* Display text in Image Box - but not for the first product */
  IF Vitems <> 1 THEN DO:
    RUN pdf_text_color IN h_PDFinc ("Spdf",1.0,.0,.0).
    RUN pdf_text_xy IN h_PDFinc ("Spdf","NO", 40, Vrow + 66).
    RUN pdf_text_xy IN h_PDFinc ("Spdf","IMAGE", 40, Vrow + 56).
    RUN pdf_text_xy IN h_PDFinc ("Spdf","AVAILABLE", 40, Vrow + 46).
  END.

  RUN pdf_text_color IN h_PDFinc ("Spdf",.0,.0,.0).

  /* Display the Product Number as a Bar Code */
  RUN pdf_set_font IN h_PDFinc ("Spdf","Code39",14.0). 
  RUN pdf_text_xy IN h_PDFinc ("Spdf",STRING(item.ItemNuM,"999999999"), 140, Vrow + 60).
  
  Vrow = Vrow - 120.
END. /* display_item_info */

PROCEDURE new_page:
  RUN pdf_new_page IN h_PDFinc ("Spdf").

  /* Reset Page Positioning etc */
  ASSIGN Vrow   = pdf_PageHeight("Spdf") - pdf_TopMargin("Spdf") - 110
         Vitems = 0.

END. /* new_page */

PROCEDURE load_cat_desc:
  DEFINE VARIABLE L_cat     AS CHARACTER NO-UNDO.

  DEFINE VARIABLE L_loop    AS INTEGER NO-UNDO.
  DEFINE VARIABLE L_extent  AS INTEGER NO-UNDO.
  
  ASSIGN Vcat-desc = ""
         L_cat      = item.catdescr.
  REPLACE(L_cat,CHR(13),"").
  REPLACE(L_cat,CHR(10),"").

  L_extent = 1.
  DO L_Loop = 1 TO NUM-ENTRIES(L_cat," "):
    IF (LENGTH(Vcat-desc[L_extent]) + LENGTH(ENTRY(L_loop,L_cat," ")) + 1) > 40 
    THEN DO:
      IF L_extent = 4 THEN LEAVE.
      
      L_extent = L_extent + 1.
    END.

    Vcat-desc[L_extent] = Vcat-desc[L_extent] + ENTRY(L_loop,L_cat," ") + " ".
  END.
END. /* load_cat_desc */

/* end of itemlist.p */
