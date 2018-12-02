/******************************************************************************

    Program:        pdf_inc.p

    Written By:     Gordon Campbell - PRO-SYS Consultants Ltd.
    Written On:     November 10, 2003

    Description:    Contains function and variable definitions for
                    generating a PDF document from within Progress

    Note:           This was copied from pdf_inc.i

    --------------------- Revision History ------------------

    Date:     Author        Change Description

    11/10/03  G Campbell    Initial Copy from pdf_inc.i

    11/17/03  G Campbell    Removed hard-coded 'sPDF' strings

                            Added pdf_encrypt procedure to allow for
                            silent or interactive PDF encryption.  Integrates
                            with the PSP (Pretty Safe PDF) product from PDFlib
                            GmbH.

                            Contact gcampbell@epro-sys.com to purchase a copy
                            of the PSP product.  Discounts for PEG Members.

    11/19/03  G Campbell    Changed how the skip was working.  Now skip based
                            on the current PointSize.

    11/20/03  G Campbell    Changed how pdf_text_xy was being processed.  It
                            now processes this text in the Graphic Grid.  It no
                            longer updates the TextX and TextY values.

    11/21/03  G Campbell    Added pdf_text_char procedure

    11/24/03  G Campbell    Fixed replace text bug RE:  Bug #847880

    11/25/03  G Campbell    Fixed European Numberic Format problem
                            RE: Bug #848975

                            This issue was previously identified by Herbert Bayer
                            (Herbert.Bayer@bundrinno.de) and I've used his
                            methodology for accomodating this issue.

    11/27/03  G Campbell    Added ability to add bookmarks to PDF documents. See
                            the custlist.p samples for sample implementation.

                            Included adding the following procedures:

                            pdf_bookmark (public procedure)
                            pdf_load_bookmarks (private procedure)
                            pdf_process_bookmarks (private procedure)

    12/04/03  G Campbell    Added following procedures:

                            pdf_set_linejoin
                            pdf_circle  ( Note: Graphic X and Y points become the
                                                X and Y Parameters - center point)
                            pdf_set_FillRed
                            pdf_set_FillGreen
                            pdf_set_FillBlue

                            Added following functions:

                            pdf_FillRed
                            pdf_FillGreen
                            pdf_FillBlue

                            string2dec (private - as per Peter Kiss
                                                  (peter.kiss@paradyme.be)

                            Plus: a whole whack of procedures used to
                                  merge disparate streams.  See sample
                                  procedures layer1.p and layer2.p.

                                  Thanks to Peter Kiss for this merging
                                  capability.  Hopefully I'm decribing the
                                  procedure uses effectively.

                            pdf_merge_stream
                            pdf_merge_stream_content (private)
                            pdf_merge_stream_link (private)
                            pdf_merge_stream_param (private)
                            pdf_merge_stream_image (private)
                            pdf_merge_stream_diff (private)
                            pdf_merge_stream_book (private)
                            pdf_merge_stream_book_detail (private)
                            pdf_ReplaceText

  12/17/03  G Campbell      Added procedure pdf_set_PageRotation
                            Added function pdf_PageRotation
                            Added temp-table TT_Pages to track Pages.

                            Updated pdf_reset_stream and pdf_reset_all to
                            ensure that all Temp Tables are being cleared
                            correctly.

  12/18/03  G Campbell      Added ability to Compress Page Streams using the
                            zlib (www.zlib.net) compression library.

                            Thanks to Maurits van Rijnen (peg@vanrijnen.nl)
                            for providing the command syntax in zlib.p and
                            zlibbind.p.

                            Added function pdf_get_parameter

                            Added procedures:
                            pdf_set_parameter
                            OutputMemPtr (private) - used to output Compressed
                                                     data

                            Added appropriate indices to Temp Tables.  Helped
                            with performance on large PDF files (thanks again
                            Herbert Bayer).

  12/29/03  G Campbell      Added ability to Encrypt using PDFencrypt.p (which
                            uses a couple of C functions for binary data
                            manipulation)

                            pdfencryopt.p currently only handles 40-bit
                            Standard Adobe Encryption.

  01/08/04  G Campbell      Started implementing Tools functionality.  This
                            included adding:

                            h_PDF-tool  Procedure Handle

                            pdf_tool_add Internal Procedure
                            pdf_tool_create Internal Procedure
                            pdf_set_tool_parameter Internal Procedure
                            pdf_get_tool_parameter Function

  01/09/04  G Campbell      Fixed issue with hardcoding of c:\temp\content.txt.
                            Now uses TT_pdf_stream.obj_UniqueID + "-C.txt" to
                            create Content file.

                            Thanks to Mikael Harjpe [mikael.hjerpe@stamford.se]
                            for pointing out this error.

  01/15/04  G Campbell      As per Gerben Wieringa (g.wieringa@vcd.nl)
  
                            In order to accommodate double-byte codepage 
                            (eg: 1252), with the startup parameter -checkdbe 
                            (check double byte enabled)
                            
                              - Added "character" option to all length() and 
                                substr() functions.
                              - Had to add "-1" in some of the substr() 
                                function-calls as third argument
                              
                            Added SESSION:TEMP-DIR when generating Content
                            File.
  
  01/16/04  G Campbell      Modified how the pdf_wrap_text procedure was 
                            working.  It wasn't correctly accounting for CHR(10)
                            within the text string plus it wasn't determining
                            the maximum allowable characters properly.  
                            
                            NOTE:  If you are currently using pdf_wrap_text 
                                   then these changes may result in changes to 
                                   your output

  01/20/04  G Campbell      Included updated merge methods - supplied by
                            Peter Kiss (peter.kiss@paradyme.be) of Paradyme 
                            Belgium.
                            
  01/20/04  G Campbell      Added procedure pdf_curve - this procedure allows
                            you to create a Bezier curve based on three x,y
                            locations.  If you call pdf_curve you need to also 
                            call pdf_close_path otherwise the curve won't 
                            appear.
                            
                            Also modified pdf_circle to use pdf_curve. 
                            pdf_circle automatically closes the 'path' (via
                            pdf_close_path or the curve.p in the samples/super
                            subdirectory).
                            
  01/21/04  G Campbell      Added function pdf_GetNumFittingChars.  This
                            replaces pdf_get_NumFittingChars (deprecated).
                            
                            Requires that you pass in the From X and and to X
                            point locations.  The difference is then used to
                            determine the maximum allowable characters.

                            Plus, Gerben missed adding one 'character' option on
                            a LENGTH statement.  Resolved.

  01/21/04  G Campbell    Added pdf_text_charxy procedure
                          This allows us to add a special character at a given
                          X/Y position

  01/21/04  G Campbell    Added function pdf_text_widthdec.  Similar to 
                          pdf_text_width except that it returns a decimal value 
                          for the text width.  This is useful for more specific
                          placement and verification of text size.

  01/22/04  G Campbell    Fixed issue with pdf_get_NumFittingChars and 
                          pdf_GetNumFittingChars.  Need to increment the Extent
                          by plus one for External fonts because the AFM file 
                          starts at Character zero.  Also had to increment the
                          FILL in the Base 14 font definitions to accomodate 
                          this.
  
                          Added Symbol and ZapfDingbats Standard Fonts.  The 
                          Base14 Fonts are now complete.

                          Added procedure pdf_text_align.  This allow us to 
                          position text based on an explicit X location. 
                          
                          Possible alignment possiblities are LEFT, RIGHT

                          Removed pdf_get_numFittingChars ... 

  01/28/04  G Campbell    BUG: During the addition of the ZapfDingbats and 
                          Symbol basefonts I had set the default Encoding to
                          StandardEncoding from WinAnsiEncoding.  This caused
                          issues with some installations.  I have restored the
                          default Encoding to WinAnsiEncoding and specifically
                          set the Encoding on the Symbol and ZapfDingbats to
                          StandardEncoding.
                          
  01/28/04  G Campbell    Changed Based Font Names from /Fn to /BFn.  Hopefully
                          this will be unique enough for future processing. At
                          this point I'm currently looking at being able to
                          include Pages from existing PDFs.  Including Fonts
                          may be an issue ... 

  01/29/04  G Campbell    Use dec2string in pdf_text_align - as per Peter Kiss.
  
  02/20/04  G Campbell    Added NO-UNDO to all TEMP-TABLE declarations

  02/24/04  G Campbell    Added new parameter called "LineSpacer".  This
                          parameter allows you to 'add' a number of points 
                          between each line when applying a pdf_skip.  This will
                          hopefully allow people to mimic the older method of
                          handling line feeds.

  02/25/04  G Campbell    Added procedure pdf_exec_footer as per Peter Kiss
  
                          This procedure allows you to call the Footer creation
                          procedure from anywhere.  This was the result of a 
                          small problem using the footer when you are sharing 
                          the stream over diff. programs.

                          Eg: When using a main program (without a footer set) 
                          then calling another program to do some printing 
                          using the same stream (file) and in the called 
                          program you specify a header and footer, 
                          then the last footer will not be printed bacause 
                          this is done in pdf_close

  02/27/04  G Campbell    Added procedures:
   
                          pdf_text_xy_dec 
                          pdf_line_dec
  
                          Changed procedures (do use decimals)
                          
                          pdf_GraphicX
                          pdf_GraphicY
                          pdf_set_GraphicX
                          pdf_set_GraphicY
                          
                          This allows for more exact placement of text, lines, etc
                          No rounding to the nearest point.
                                                    
  02/27/04  G Campbell    Added code to use Zlib to compress the JPEG images.
  
                          Added "NOEMBED" functionality to disallow embedding
                          of a True Type Font.  THIS IS NOT RECOMMENDED but has
                          been added because it could be useful if the PDF file
                          is being generated and printed for local (single
                          machine) purposes only.  This functionality removes
                          the embedded TTF file from the PDF document resulting
                          in a smaller PDF document.
                          
                          Note: The NOEMBED is added as the second entry when
                                loading the font, eg:
                                
                                RUN pdf_load_font  ("Spdf",
                                                    "Code 39,NOEMBED",
                                                    "",
                                                    "\gord\pdfinclude\samples\support\code39.afm","").
                                
                                When not EMBEDing, the AFM file entry is still 
                                required but the TTF font file is not.
                                
                                Also, you must enter the 'real name' of font.
                                For example, instead of using say 'TimesRoman',
                                you should use 'Times Roman'.  Spaces are now
                                important.

  03/01/04  G Campbell  As per Herbert Bayer
  
                        Added additional check when loading a Font or an Image.
                        Check to see if the font/image has been previously 
                        loaded.  If so, then return an error message.  An image
                        or Font can only be loaded once ... that is, can only
                        be loaded once with the same name.  You can in fact load
                        the same image/font multiple times under different names
                        but it doesn't make sense to do this.

  03/01/04  G Campbell  Added code to compress embedded TTF file.

  03/03/04  G Campbell  As per Bruno Van Loon (brunovanloon@hotmail.com)
  
                        Added LineSpacer  to the pdf_text_at procedure.
  
  03/04/04  G Campbell  Changed TT_pdf_annot to TT_pdf_annot
  
                        Added procedures:
                        
                        pdf_note
                        pdf_stamp
                        pdf_markup
                        
                        These procedures allow for additional annotation types.
                        See note.p in the samples/super subdirectory for an
                        example on how to use.  
                        
                        Note:  If using the Stamp or Markup procedures ensure
                               that the Style or Type is entered exactly as 
                               required.  That is, use 'Draft' not 'draft'.  
                               These values are case-sensitive.

  03/08/04  G Campbell  Removed LineSpacer from pdf_text_to and pdf_text_at.  
                        Seemed to be causing issues and the spacing should
                        be handled by pdf_skip.

  03/09/04  G Campbell  Added code in pdf_close to cleanup persistent handles 
                        for Zlib.p and zlibbind.p.
                        
                        Also added code to pdf_new that checks to see if h_zlib
                        is valid or not.  If not, then rerun zlib.p.

  03/22/04  G Campbell  Adjusted code to correctly handle placement of rotated
                        text.
                        
                        Also removed "BY obj_line DESC" when outputting text
                        content.  Not required as the Sequence # is unique 
                        enough.

  03/23/04  J Johnson   Added pdf_insert_page procedure to insert a page
                        somewhere within the document.

                        Modified pdf_new_page to handle inserted pages
                        by always adding the new page to the end of the
                        document.
                        
                        Added "ShowBookmarks" option to pdf_set_info
                        and pdf_get_info.   If set to "YES", Acrobat
                        will open the document with the bookmarks showing.

                        Added dynamic PAGEno function. ("@@PageNo")
                        Allows for dynamic page numbering when inserting pages.

                        Added dynamic justification ability to pdf_text_boxed_xy
                        Added delimited parameters to the "TEXTXY" object type
                        to allow for left, right or center justification
                        to correctly justify strings with functions such as
                        @@PageNo and @@TotalPages
    
                        pdf_text_boxed_xy modified to allow a weight of 0 for
                        the line.  If it is 0, then the text is only justified
                        and a box is not drawn.

                        modified pdf_content to dynamically justify text
                        with left, right or center attributes
                        defined as object type "TEXTXY"
                        re-computes and replaces pdfcolumn value based on
                        text being printed.
                    
                        references to EQ "TEXTXY" have been replaced with
                        BEGINS "TEXTXY" to allow for justification parameters
                        to be included in object type.

                        NOTE:  Someone may want to modify the pdf_text_align
                               procedure to use dynamic justification
                               This would allow for justifying things like
                               @@TotalPages and @@PageNo

  03/23/04  G Campbell  Added ability to use external templates via 
                        pdf_load_template and pdf_use_template - these
                        call internal procedures in the persistent routine
                        pdfTemplate.p.

  03/24/04  G Campbell  Changed delimiter to | (pipe) when using NOEMBED.  This
                        is because you can append Bold or Italic to the font
                        name when not embedded.  
                        
                        For example: Lucida Console,Bold
                                 or  Lucida Console,BoldItalic

  04/01/04  G Campbell  Added the following functions/procedures and modified
            M Edu       code appropriately.  These changes allow PDFinclude to
                        (again) work with Progress V8.
  
                        Added GetFileSize function 
                          - Used to determine the size of a file using 
                            different methods for different Progress versions.
                            
                        Added GetFileContent procedure
                           
                          - Use different methods to load file contents into
                            a MEMPTR variable based on different Progress
                            versions.
                            
                        Added PutFileContent procedure
                           
                          - Use different methods to save the contents of a 
                            MEMPTR variable to the PDF stream based on different
                            Progress Versions.
                            
                        Use PROVERSION preprocessor to determine which Zlib
                        procedure to run (zlib8.p for V8 and zlib.p for
                        everyone else).

  04/01/04  G Campbell  As per Peter Kiss
                        
                        Added 45,135,225,315 DEGREES for text rotation

  04/01/04  G Campbell  Removed VALUE(SEARCH()) when running pdftool.p. This 
                        wouldn't work if you only had the .r in the directory.
  
  04/07/04  G Campbell  Added page_width and page_height fields to the 
                        TT_pdf_page temp-table.  Now we can set the page/size
                        and width per page in the document therefore allowing us
                        to use pdf_set_orientation to change the orientation of
                        each page.  
                        
                        Added procedure pdf_new_page2 to allow us to add a page
                        and define which orientation we want it to be viewable
                        in.  
                        
                        The pdf_new_page2 accepts two parameters - PDF stream 
                        and orientation.

  04/12/04  G Campbell  Added the following parameters (used via 
                        pdf_set_parameter) to set the Viewer Preferences:
                        
                        HideToolbar - view application's toolbar when document
                                      is active.  Valid entry is TRUE/FALSE with
                                      FALSE being the default.
                        HideMenubar - view application's menubar when document
                                      is active.  Valid entry is TRUE/FALSE with
                                      FALSE being the default.
                        HideWindowUI - view user interface elements in the 
                                       document’s window (such as scroll bars 
                                       and navigation controls), leaving only 
                                       the document’s contents displayed. Valid
                                       entry is TRUE/FALSE with FALSE being the
                                       default.
                        FitWindow - resize the document’s window to fit the size 
                                    of the first displayed page.  Valid entry is
                                    TRUE/FALSE with FALSE being the default.
                        CenterWindow - position the document’s window in the 
                                       center of the screen.  Valid entry is
                                       TRUE/FALSE with FALSE being the default.
                        DisplayDocTitle - display the document title taken from 
                                          the Title entry of the document 
                                          information dictionary. Valid entry is
                                          TRUE/FALSE with FALSE being the 
                                          default.

                        NOTE: REMOVED ShowBookmarks option from pdf_set_info.
                              -------
                              
  04/12/04  G Campbell  Added the parameter PageMode- valid possibilities
                        are:
                        
                        UseNone     - Neither document outline nor thumbnail 
                                     images visible (DEFAULT)
                        UseOutlines - Document outline visible (this replaces
                                     the ShowBookmarks option in the 
                                     pdf_set_info procedure)
                        UseThumbs   - Thumbnail images visible
                        FullScreen  - Full-screen mode, with no menu bar, 
                                      window controls, or any other window 
                                      visible

                        Added the parameter PageLayout- valid possibilities
                        are:
                        
                        SinglePage     - Display one page at a time (DEFAULT)
                        OneColumn      - Display the pages in one column
                        TwoColumnLeft  - Display the pages in two columns, with 
                                         oddnumbered pages on the left
                        TwoColumnRight - Display the pages in two columns, with 
                                         oddnumbered pages on the right.

  04/12/04  G Campbell  Added procedure pdf_GetBestFont ... as per 
                        James McAteer [James.McAteer@Carltoncards.co.uk]
  
                        This procedure calculates the best font size to use 
                        when inserting text into a given range along the X axis 
                        - it tests in 0.5 point size increments.  
                        
                        Example: This is useful if you are trying to fit 
                        variable-size text within a rectangle and you need to 
                        determine the best font to use to ensure that the full
                        text appears (or use the pdfChopText option to fit the
                        most text with the smallest allowable font).
                        
  04/14/04  G Campbell  Updated the pdf_new_page, pdf_new_page2 and 
                        pdf_insert_page procedures to correctly set the Graphic
                        X/Y coordinates (based on Left Margin and Top Margin) 
                        plus ensure that the last Font/Point Size that was used
                        was being reset.

  04/27/04  G Campbell  Added pdf_place_image2 and pdf_rect2
  
                        pdf_place_image2 - allows you to place images below
                                           the graphical elements (such as 
                                           rectangles, lines etc) and text
                        pdf_rect2        - allows you to draw a rectangle
                                           without filling it
                                           
                        The combination of these two elements allows you to 
                        use an image as a Watermark and then draw rectangles
                        over top of the image.
                        
  06/03/04  G Campbell  Misc minodr bug fixes as identified by Peter Kiss
      
                        Fixed FOR EACH TT_pdf_ReplaceTxt.  Where clause was
                        incorrect.  
                       
                        Missing delete of TT_pdf_tool and  TT_pdf_tool_param
                        in the Reset procedures.
                            
                        In pdf_load_image, set the image_file equal to the
                        SEARCH(imagefile) so that it will be correctly found
                        in pdf_load_images procedure.

  06/03/04  G Campbell  Added LoadExternalXObjects.  Now handles the inclusion
                        of JPEG images when importing a PDF page.
                        
                        renamed pdfextract.i to pdfglobal.i

  06/04/04  G Campbell  Added code to handle inclusion of images when loading
                        a page from an external PDF page.  Has been tested 
                        against PDF documents with GIF and JPEG images 
                        embedded.

  06/10/04  G Campbell  Reworked logic to handle Adobe Distiller v5 and v6 plus
                        modified code to handle the inclusion of the external
                        page onto a separate page.

  06/10/04  G Campbell  Added pdf_rgb routine from Robert Ayris [rayris@comops.com.au]

                        This routine allows you to set colours based on the 
                        Hex or RGB colour values.  The procedure accepts 3
                        parameters:
                        
                        1. Stream Name   - Self-Explanatory
                        2. Function Name - One of:
                                             pdf_text_color
                                             pdf_stroke_color
                                             pdf_stroke_fill
                        3. Colour        - Either a hex value (begining with 0x)
                                           or a 9 digit RGB colour
                                           
                                           eg: hex = 0x006466 (Teal Green)
                                               rgb = 000100102 (Teal Green)

  06/11/04  G Campbell  Added function pdf_font_loaded
  
                        Lets you determine whether a font id has already
                        been loaded or not.  Returns TRUE if ID is loaded or 
                        FALSE if not.

  06/11/04  G Campbell  Modified how the VerticalSpace functionality worked.
  
                        Nobody seemed to be using it, so I implemented the
                        changes required by Robin Smith of Comops Australia
                        (since he seems to be the only person using it).  This
                        also include removing the default setting during the
                        pdf_init_param procedure and setting the return value
                        to 0 in pdf_VerticalSpace (if parameter not found).

  06/15/04  G Campbell  Added code to handle the new MATRIX tool (in pdftool.p)
  
  06/15/04  G Campbell  Added rudimentary XML handling.
                        See procedure pdf_load_xml ... which loads an XML file
                        into the TT_pdf_xml temp-table (defined in pdfglobal.i).
                        
                        You can then use the TT_pdf_xml temp-table (and buffers) 
                        to extract and place the XML data.  See the example in
                        /samples/super/xml.p
                        
                        Also, added GetXMLNodeValue which allows you to retrieve
                        a specific Node value (also used in xml.p).

  06/16/04  G Campbell  Added code to handle the /Encoding and /FontFile3 
                        operators of a Font Dictionary in an opened PDF file

  06/25/04  G Campbell  Copy fonts when merging streams .. as per Peter Kiss
  
  06/28/04  G Campbell  Removed use of TT_pdf_content temporary table. Now
                        writes the page contents out to a text file.  During
                        testing, this seemed to improve the performance.

  07/07/04  G Campbell  Added procedure pdf_fill_text which allows you to 'fill'
                        defined placeholders in an existing PDF document with
                        some data. The placeholders are Adobe Text Form Fields.

  07/15/04  G Campbell  Fixed bug: Was not using obj_stream when counting Pages
                                   in TT_pdf_page.  Thanks to Peter Kiss for
                                   noticing this.

  07/28/04  G Campbell  With the change to using text files (instead of 
                        TT_pdf_content) this introduced an issue when using
                        the pdf_merge_stream functionality.  After some changes
                        and testing, it has been resolved.

  07/30/04  G Campbell  Updated to allow for usage of multi-page PDF template
                        documents with Form Fields.

  08/04/04  G Campbell  Added procedure pdf_fill_multiline.
                        
                            This is used in conjunction with the filling of 
                            Adobe form fields.  Will place multiline text into 
                            the field placeholder using the form field rectangle
                            boundries to wrap/display the text.

                        Added function pdf_text_widthdec2

                            This is used to determine the decimal width of text.
                            Similar to pdf_text_widthdec but you pass in which
                            Fonttag and FontSize you want to use to determine
                            the text width.

  08/05/04  G Campbell  Added published events:

                        MaxPDFPage      - publishes the total number of pages
                                          for a stream
                        BuildPDFPage    - publishes the current page number in
                                          the build process
                        GeneratePDFpage - publishes the current page number when
                                          generating the PDF content

  08/31/04  G Campbell  Modified how the pdf_set_page was being run.  Now it
                        resets the appropriate elements to ensure that if you
                        use this procedure to 'change pages' midstream then the
                        appropriate changes occur.  Basically re-opens the 
                        original page output for re-use placing the Text and 
                        Graphic X/Y coordinates to the appropriate locations 
                        (as if you were re-starting the page .. ie: pdf_new_page)
                        
                        This is useful if you wanted to reprocess pages after 
                        the complete document was done.  For example, allows you
                        to programattically control the "Page 1 of 4".  See
                        example samples/super/multipage.p.

  09/13/04  G Campbell  Added code to accommodate landscape PDF templates.

  09/24/04  G Campbell  Modified how pdf_get_image_wh was determining the image
                        width and height.  Now uses a MEMPTR instead of reading
                        the JPEG file byte-by-byte ... thanks to Sebastian 
                        Lacroix for updating my original readjpeg.w to perform
                        better.

  09/27/04  G Campbell  Added function pdf_text_fontwidth2.

                        Same as pdf_text_fontwidth but it also accepts a Font
                        Size when determining the text width.

  09/28/04  J Johnson   Added function pdf_get_pdf_info
                        NOTES:
                            This funciton is different than pdf_get_info
                            pdf_get_info gets a value for the pdf that you
                            are CREATING
                            pdf_get_pdf_info gets a value from a pdf file
                            loaded with pdf_open_pdf.

                         Returns information about a pdf loaded with pdf_open_pdf 
                         i.e.  numPages = integer(pdf_info("SPDF",1,"pages"))     
                               author   = pdf_info("SPDF",1,"author")             

                         Possible info values are:
                            Pages    - total number of pages in imported PDF
                            Author   - who created the original PDF document
                            Producer - what product created the original PDF 
                                       document
                            Creator  - what procedure created the original PDF
                                       document
                            Title    - the original PDF document title
                            Subject  - the original PDF document Subject
                            Keywords - Keywords contained in the original PDF
                                       document
                            ModDate  - the last Modified Date of the orignal
                                       PDF document
                            ModTime - the last Modified Time of the original
                                      PDF document
                            CreationDate - the creation date of the orignal
                                           PDF document
                            CreationTime - the creation time of the original
                                           PDF document

                        The 'Pages' info is always available for all imported
                        documents but the additional information may or may not
                        be available (depends on if it was included in the 
                        original document or not).

  10/07/04  G Campbell  Added logic to handle multiple external pages with
                        the same Xobject names (eg: using the same image names
                        such as /Img1)

                        Added new parameter UseExternalPageSize.  This will
                        use the MediaBox as identified in the original PDF
                        document, overwriting the current pages Width/Height
                        settings.

  10/12/04  G Campbell  Removed call to zlib.p from zlib.i and placed it into
                        pdf_close procedure.  It isn't needed before that and
                        some people don't use the 'compress' option.

  10/19/04  G Campbell  Added new parameters called ScaleX and ScaleY.  These
                        allow you to specify the scaling of a font when using
                        the XY placement of text elements (eg: using pdf_text_xy
                        procedure).  The default for each of the parameters is
                        1 (scale same as point size).  To double the vertical 
                        size (or height) then:

                           RUN pdf_set_parameter("Spdf","ScaleY","2").

  11/02/04  G Campbell  When parsing AFM file ensure that the afm_LastChar is
                        set to a value greater than 0.  This is because some
                        AFM files set the unused characters to -1 and this
                        causes issues when defining the Font Descriptor.

  12/03/04  G Campbell  Added code to the pdf_load_external procedure that
                        allows us to import external PDF page content that is
                        greater than 31K.  Look at the t4form.p example.  If it
                        didn't have this updated code the Canadian flag image
                        wouldn't appear.

  12/08/04  G Campbell  Added OPSYS specific code to handle issue of using
                        backslashes in the pdf_replace_text procedure.

  01/25/05  G Campbell  Fixed issue with pdf_get_wrap_length.  It now mimics
                        the pdf_wrap_text procedure when determining when the
                        lines of text should break.

  03/09/05  G Campbell  Fixed issue with multiple streams being output at the
                        same time ... data from one stream was being included
                        into another stream.
                        
  03/14/05  G Campbell  Update pdf_set_parameter to allow setting of Temporary
                        parameter values.  These can be used to store some
                        stream specific data in your procedure.  For example,
                        if you want to store the default Base Font Size of a 
                        stream then set via:
                        
                        RUN pdf_set_parameter("MyStream","tmpBaseFontSize","8.0").
                        
                        You can then retrieve this value via the function named
                        pdf_get_parameter.
                        
                        IMPORTANT NOTE: Al temporary parameter values must begin 
                        with 'TMP" (case insensitive).
                        
  03/15/05  G Campbell  Removed the code that output the Text Colour and Font
                        each and every time in the pdf_text_xy_dec routine. 
                        This may get removed from other routines also ... but
                        for now, just the one.

  04/14/05  G Campbell  Added code to handle the cropping of external PDF pages
  
  04/25/05  G Campbell  Added VERSION parameter.  Can only be used with the
                        pdf_get_parameter procedure.  
                        
                        Also, code to handle DescendantFonts is introduced. This
                        basically occurs when you create a document in Word 
                        that includes a 'Symbol' (font).  Adobe Professional 
                        creates these as DescendatFonts which was causing an
                        issue when loading an external PDF doc.  Now, it Seems 
                        to work for most instances (or at least the ones that I
                        created anyway).

  05/18/05  G Campbell  Added code to handle new TABLE setup parameters added
                        by James McAteer (eg: UseFields).

  06/01/05  G Campbell  Added a {&pdfskip} before the endstream when importing
                        external PDF documents.  This is because the last
                        command in the page stream may not have a line feed
                        and therefore the endstream and the last command
                        ran together (creating an invalid statement like
                        ETendstream)
                        
  07/06/05  G Campbell  Locally included the Zlib Compression functionality.  
                        No longer in external procedures.

                        Locally includes the Encryption functionality.
                        No longer in external procedures.

  08/03/05  J Johnson   Fixed pdf_insert_page: when pdf_set_page was called,                         so that the problem with the
                        the page output stream had being already closed,
                        causing "attempt to write to closed stream" errors
                        Now the stream is closed, the pages re-numbered,
                        and the stream is re-opened so that pdf_set_page
                        can handle the stream normally

  08/24/05  G Campbell  Reworked how Xobjects with contents greater than 32K
                        were concatenated (see tempcat.txt).

  09/22/05  G Campbell  Fixed how the External Resource Font list for Xobjects 
                        was being built (this included add ext_page field to a
                        couple of Temp tables)

  03/02/06  G Campbell  Added code to allow for inline tagging.  That is, when
                        text is output to the PDF that contains a tag (<b>)
                        ensure that the correct formatting occurs.
                        
                        Added Parameters:
                        
                        TagColor:<name> = Sets the name of a valid colour using
                                          comma-delimted RGB values.
                                          
                                          Example.
                                          
                                          RUN pdf_set_parameter("Spdf",
                                                                "TagColor:Black",
                                                                "0,0,0").
                                                                
                        UseTags        = Possible value are: "TRUE" or "FALSE"
                        BoldFont       = name of a valid font
                        ItalicFont     = name of a valid font
                        BoldItalicFont = name of a valid font
                        DefaultFont    = name of a valid font
                        DefaultColor   = name of a previously defined TagColor
                                                
                        If TRUE then accommodate inline tagging of text,
                        otherwise don't.  Is processed during page build time 
                        (not at Close/Write) therefore can to turned on/off
                        during program processing (just in case you had sample
                        HTML you wanted to output).

                        Handles Tags for:
                        
                        <b></b> - Bold
                        <i></i> - Italic
                        <color=<ColorName></color> - setting color
                        
                        *** See samples/super/usetags.p for example of usage.

******************************************************************************/

&IF OPSYS = "UNIX" &THEN
  &GLOBAL-DEFINE pdfSKIP     CHR(13) CHR(10)
&ELSE
  &GLOBAL-DEFINE pdfSKIP     SKIP
&ENDIF

{ pdfglobal.i "SHARED"}

/* The following defines are used to determine the JPEG Image Height and Width */
&GLOBAL-DEFINE M_SOF0  "0xC0"		/* Start Of Frame N */
&GLOBAL-DEFINE M_SOF1  "0xC1"		/* N indicates which compression process */
&GLOBAL-DEFINE M_SOF2  "0xC2"       /* Only SOF0-SOF2 are now in common use */
&GLOBAL-DEFINE M_SOF3  "0xC3"
&GLOBAL-DEFINE M_SOF5  "0xC5"		/* NB: codes C4 and CC are NOT SOF markers */
&GLOBAL-DEFINE M_SOF6  "0xC6"
&GLOBAL-DEFINE M_SOF7  "0xC7"
&GLOBAL-DEFINE M_SOF9  "0xC9"
&GLOBAL-DEFINE M_SOF10 "0xCA"
&GLOBAL-DEFINE M_SOF11 "0xCB"
&GLOBAL-DEFINE M_SOF13 "0xCD"
&GLOBAL-DEFINE M_SOF14 "0xCE"
&GLOBAL-DEFINE M_SOF15 "0xCF"
&GLOBAL-DEFINE M_SOI   "0xD8"		/* Start Of Image (beginning of datastream) */
&GLOBAL-DEFINE M_EOI   "0xD9"		/* End Of Image (end of datastream) */
&GLOBAL-DEFINE M_SOS   "0xDA"		/* Start Of Scan (begins compressed data) */
&GLOBAL-DEFINE M_APP0  "0xE0"		/* Application-specific marker, type N */
&GLOBAL-DEFINE M_APP12 "0xEC"		/* (we don't bother to list all 16 APPn's) */
&GLOBAL-DEFINE M_COM   "0xFE"		/* COMment */
&GLOBAL-DEFINE M_MARK  "0xFF"   /* Marker */


&GLOBAL-DEFINE BoldOnChar    CHR(1)
&GLOBAL-DEFINE BoldOffChar   CHR(2)
&GLOBAL-DEFINE ItalicOnChar  CHR(3)
&GLOBAL-DEFINE ItalicOffChar CHR(4)
&GLOBAL-DEFINE ColorOnChar   CHR(5)
&GLOBAL-DEFINE ColorOffChar  CHR(6)

/* ---------------------------- Define TEMP-TABLES -------------------------


   The temp-tables are used to store the PDF streams and resources used when
   generating a PDF document */
DEFINE TEMP-TABLE TT_pdf_stream NO-UNDO
    FIELD obj_stream       AS CHARACTER
    FIELD obj_file         AS CHARACTER
    FIELD obj_encrypt      AS LOGICAL INIT FALSE
    FIELD obj_master       AS CHARACTER
    FIELD obj_user         AS CHARACTER
    FIELD obj_access       AS CHARACTER
    FIELD obj_key          AS INTEGER
    FIELD obj_silent       AS LOGICAL INIT FALSE
    FIELD obj_mode         AS CHARACTER
    FIELD obj_CallProc     AS HANDLE
    FIELD obj_footer       AS CHARACTER
    FIELD obj_header       AS CHARACTER
    FIELD obj_last         AS CHARACTER
    FIELD obj_id           AS CHARACTER
    FIELD obj_EncryptDict  AS INTEGER
    FIELD obj_UniqueID     AS CHARACTER
    FIELD obj_P            AS INTEGER
    FIELD obj_DoingText    AS LOGICAL
    FIELD obj_DoingGraphic AS LOGICAL
INDEX obj_stream  AS PRIMARY
      obj_stream.

/* The following temp-table is used to store/track parameters per stream */
DEFINE TEMP-TABLE TT_pdf_param NO-UNDO
    FIELD obj_stream    AS CHARACTER
    FIELD obj_parameter AS CHARACTER
    FIELD obj_valid     AS CHARACTER
    FIELD obj_value     AS CHARACTER
INDEX obj_stream AS PRIMARY
      obj_stream
      obj_parameter.

DEFINE TEMP-TABLE TT_pdf_error NO-UNDO
  FIELD obj_stream  AS CHARACTER
  FIELD obj_func    AS CHARACTER FORMAT "x(20)"
  FIELD obj_error   AS CHARACTER FORMAT "x(40)"
INDEX obj_stream AS PRIMARY
      obj_stream.

DEFINE TEMP-TABLE TT_pdf_page NO-UNDO
  FIELD obj_stream    AS CHARACTER
  FIELD page_nbr      AS INTEGER
  FIELD page_rotate   AS INTEGER
  FIELD page_width    AS INTEGER
  FIELD page_height   AS INTEGER
  FIELD UseTotalPages AS LOGICAL
  FIELD UsePageNo     AS LOGICAL
  FIELD page_use      AS INTEGER
  FIELD page_crop     AS CHARACTER
INDEX obj_stream AS PRIMARY
      obj_stream
      page_nbr.

DEFINE TEMP-TABLE TT_pdf_tool NO-UNDO
  FIELD obj_stream   AS CHARACTER
  FIELD tool_name    AS CHARACTER
  FIELD tool_type    AS CHARACTER
  FIELD tool_handle  AS HANDLE
INDEX obj_stream AS PRIMARY UNIQUE
      obj_stream
      tool_name.

DEFINE TEMP-TABLE TT_pdf_tool_param NO-UNDO
  FIELD obj_stream   AS CHARACTER
  FIELD tool_name    AS CHARACTER
  FIELD tool_param   AS CHARACTER
  FIELD tool_col     AS INTEGER
  FIELD tool_value   AS CHARACTER
INDEX obj_stream AS PRIMARY UNIQUE
      obj_stream
      tool_name
      tool_param
      tool_col.

/* The following temp-table is used to build a list of objects that will appear
   in the PDF document */
DEFINE TEMP-TABLE TT_pdf_object NO-UNDO
  FIELD obj_stream  AS CHARACTER
  FIELD obj_nbr     AS INTEGER
  FIELD obj_desc    AS CHARACTER
  FIELD obj_offset  AS DECIMAL DECIMALS 0 FORMAT "9999999999"
  FIELD gen_nbr     AS INTEGER FORMAT "99999"
  FIELD obj_type    AS CHARACTER FORMAT "X"
  FIELD obj_page    AS INTEGER
INDEX obj_stream AS PRIMARY
      obj_stream
      obj_nbr.

/* The following temp-table is used to build a list of Bookmarks that will appear
   in the PDF document */
DEFINE TEMP-TABLE TT_pdf_bookmark NO-UNDO
  FIELD obj_stream   AS CHARACTER
  FIELD book_obj     AS INTEGER
  FIELD book_nbr     AS INTEGER
  FIELD book_title   AS CHARACTER
  FIELD book_parent  AS INTEGER
  FIELD book_expand  AS LOGICAL
  FIELD book_child   AS INTEGER
  FIELD book_first   AS INTEGER
  FIELD book_last    AS INTEGER
  FIELD book_page    AS INTEGER
  FIELD book_Y       AS INTEGER
INDEX book_nbr AS PRIMARY UNIQUE
      obj_stream
      book_nbr.

/* The following temp-table is used to track Document Information */
DEFINE TEMP-TABLE TT_pdf_info NO-UNDO
    FIELD obj_stream    AS CHARACTER
    FIELD info_attr     AS CHARACTER
    FIELD info_value    AS CHARACTER
INDEX obj_stream AS PRIMARY
      obj_stream.

/* The following temp-table is used to track Links (Annotations)
   loaded into a PDF stream */
DEFINE TEMP-TABLE TT_pdf_annot NO-UNDO
    FIELD obj_stream     AS CHARACTER
    FIELD annot_type     AS CHARACTER
    FIELD annot_color    AS CHARACTER
    FIELD annot_content  AS CHARACTER
    FIELD annot_page     AS INTEGER
    FIELD annot_rect     AS CHARACTER 
    FIELD annot_border   AS INTEGER
    FIELD annot_style    AS CHARACTER
    FIELD annot_obj      AS INTEGER
    FIELD annot_icon     AS CHARACTER
    FIELD annot_add      AS CHARACTER /* For Additional Information */
INDEX obj_stream  AS PRIMARY
      obj_stream.

/* The following temp-table is used to track Images loaded into a PDF stream */
DEFINE TEMP-TABLE TT_pdf_image NO-UNDO
    FIELD obj_stream    AS CHARACTER
    FIELD image_name    AS CHARACTER
    FIELD image_file    AS CHARACTER
    FIELD image_tag     AS CHARACTER
    FIELD image_obj     AS INTEGER
    FIELD image_len     AS INTEGER
    FIELD image_h       AS INTEGER
    FIELD image_w       AS INTEGER
INDEX obj_image AS PRIMARY
      obj_stream
      image_name.

DEFINE TEMP-TABLE TT_pdf_external NO-UNDO
    FIELD obj_stream    AS CHARACTER
    FIELD ext_tag       AS CHARACTER
    FIELD ext_obj       AS INTEGER
    FIELD ext_file      AS CHARACTER
    FIELD ext_len       AS INTEGER
    FIELD ext_Media1    AS DECIMAL DECIMALS 5
    FIELD ext_Media2    AS DECIMAL DECIMALS 5
    FIELD ext_Media3    AS DECIMAL DECIMALS 5
    FIELD ext_Media4    AS DECIMAL DECIMALS 5
    FIELD ext_Crop1     AS DECIMAL DECIMALS 5
    FIELD ext_Crop2     AS DECIMAL DECIMALS 5
    FIELD ext_Crop3     AS DECIMAL DECIMALS 5
    FIELD ext_Crop4     AS DECIMAL DECIMALS 5
    FIELD ext_rotate    AS INTEGER
    FIELD page_id       AS INTEGER    /* This is the new PDF page # */
    FIELD ext_page      AS INTEGER    /* This is the external PDF page # */
INDEX obj_image AS PRIMARY
      obj_stream
      ext_tag.

/* The following temp-table is used to track Fonts loaded into a PDF stream */
DEFINE TEMP-TABLE TT_pdf_font NO-UNDO
    FIELD obj_stream        AS CHARACTER
    FIELD font_name         AS CHARACTER
    FIELD font_file         AS CHARACTER
    FIELD font_afm          AS CHARACTER
    FIELD font_dif          AS CHARACTER
    FIELD font_type         AS CHARACTER
    FIELD font_width        AS CHARACTER
    FIELD font_obj          AS INTEGER
    FIELD font_encoding     AS INTEGER
    FIELD font_descr        AS INTEGER
    FIELD font_stream       AS INTEGER
    FIELD font_len          AS INTEGER
    FIELD font_tag          AS CHARACTER
    FIELD font_embed        AS LOGICAL INIT TRUE
    FIELD afm_ItalicAngle   AS INTEGER
    FIELD afm_Ascender      AS CHARACTER
    FIELD afm_Descender     AS CHARACTER
    FIELD afm_FontBBox      AS CHARACTER
    FIELD afm_FirstChar     AS CHARACTER
    FIELD afm_LastChar      AS CHARACTER
    FIELD afm_Widths        AS CHARACTER
    FIELD afm_IsFixedPitch  AS CHARACTER INIT "0"
    FIELD afm_flags         AS CHARACTER
    FIELD ext_page          AS INTEGER
INDEX font_name AS PRIMARY
      font_name
INDEX obj_stream
      obj_stream.

DEFINE TEMP-TABLE TT_pdf_diff NO-UNDO
  FIELD obj_stream  AS CHARACTER
  FIELD font_name   AS CHARACTER
  FIELD char_num    AS INTEGER
  FIELD PS_name     AS CHARACTER
INDEX obj_stream  AS PRIMARY
      obj_stream.

DEFINE TEMP-TABLE TT_pdf_ReplaceTxt /* peki */ NO-UNDO
  FIELD obj_stream  AS CHARACTER
  FIELD mergenr     AS INTEGER
  FIELD txt_from    AS CHARACTER
  FIELD txt_to      AS CHARACTER
INDEX obj_stream    AS PRIMARY
      obj_stream.

DEFINE TEMP-TABLE TT_pdf_FillTxt NO-UNDO
  FIELD obj_stream    AS CHARACTER
  FIELD page_nbr      AS INTEGER
  FIELD fill_from     AS CHARACTER
  FIELD fill_to       AS CHARACTER
  FIELD fill_options  AS CHARACTER
INDEX obj_stream  AS PRIMARY
      obj_stream
      page_nbr.

DEFINE TEMP-TABLE TT-Merge-Pages NO-UNDO
  FIELD PageFrom    AS INTEGER
  FIELD PageTo      AS INTEGER
  FIELD MergeNr     AS INTEGER.

DEFINE TEMP-TABLE hexarray NO-UNDO
   FIELD hex-val   AS CHARACTER
   FIELD chr-val   AS INTEGER
INDEX hex-idx AS PRIMARY
      hex-val
INDEX chr-idx 
      chr-val.

/* ---------------------- Define LOCAL VARIABLES -------------------------- */
/* Variables required for PDF Encryption */
DEFINE VARIABLE vowner-str          AS MEMPTR NO-UNDO.
DEFINE VARIABLE vuser-str           AS MEMPTR NO-UNDO.
DEFINE VARIABLE vrc4-key            AS MEMPTR NO-UNDO.
DEFINE VARIABLE vrc4-128            AS MEMPTR NO-UNDO.
DEFINE VARIABLE m_O                 AS MEMPTR NO-UNDO.
DEFINE VARIABLE m_U                 AS MEMPTR NO-UNDO.
DEFINE VARIABLE p_U_Temp            AS MEMPTR NO-UNDO.

DEFINE VARIABLE hex_chr AS CHARACTER NO-UNDO INIT "0123456789abcdef".

DEFINE VARIABLE vstring     AS CHARACTER NO-UNDO.
DEFINE VARIABLE vhex        AS CHARACTER NO-UNDO.
DEFINE VARIABLE loop        AS INTEGER NO-UNDO.

/* DEFINE VARIABLE pdf_inc_ContentSequence AS INTEGER NO-UNDO. */
DEFINE VARIABLE pdf_inc_ObjectSequence  AS INTEGER NO-UNDO.
DEFINE VARIABLE pdf_OutlinesDict        AS INTEGER NO-UNDO.
DEFINE VARIABLE pdf_OutlinesLast        AS INTEGER NO-UNDO.

/* The following variables are used to store the Image Height Width */
DEFINE VARIABLE pdf_width     AS INTEGER NO-UNDO.
DEFINE VARIABLE pdf_height    AS INTEGER NO-UNDO.

/* Variables used in conjunction with Header/Footer requirements */
DEFINE VARIABLE pdf_ForFooter       AS LOGICAL   NO-UNDO.

/* Variables used in conjunction with Wrap Text requirements */
DEFINE VARIABLE pdf_WrapText        AS LOGICAL   NO-UNDO.
DEFINE VARIABLE pdf_WrapFont        AS CHARACTER NO-UNDO.
DEFINE VARIABLE pdf_WrapSize        AS DECIMAL NO-UNDO.

/* Miscellaneous Variables */
DEFINE VARIABLE pdf_CurrentStream     AS CHARACTER NO-UNDO.
DEFINE VARIABLE xml-seq               AS INTEGER NO-UNDO.
DEFINE VARIABLE pdf-Res-Object        AS INTEGER NO-UNDO.
DEFINE VARIABLE Vuse-font             AS CHARACTER NO-UNDO.
DEFINE VARIABLE pdf-Stream-Start      AS INTEGER NO-UNDO.
DEFINE VARIABLE pdf-Stream-End        AS INTEGER NO-UNDO.
DEFINE VARIABLE pdf-EncryptKeyMemPtr  AS MEMPTR NO-UNDO.
DEFINE VARIABLE pdf-EncryptMemPtr     AS MEMPTR NO-UNDO.
DEFINE VARIABLE h_PDF-Tool            AS HANDLE NO-UNDO.
DEFINE VARIABLE h_PDF-Template        AS HANDLE NO-UNDO.

/* Used to read the JPEG image file and determine width/height */
DEFINE VARIABLE mImage                AS MEMPTR  NO-UNDO.
DEFINE VARIABLE iImageByte            AS INTEGER NO-UNDO.

DEFINE VARIABLE mContent              AS MEMPTR    NO-UNDO.
DEFINE VARIABLE mHolder               AS MEMPTR    NO-UNDO.

DEFINE STREAM S_pdf_inc.
DEFINE STREAM S_pdf_inp.
DEFINE STREAM S_pdf_out.
DEFINE STREAM S_compress.

DEFINE BUFFER B_TT_pdf_param   FOR TT_pdf_param.

RUN LoadHexArray.

/* -------- Start of PDFEncryptLib External procedure definitions -------- */

FUNCTION BinaryXOR RETURNS INTEGER 
        (INPUT iFirstOperand AS integer,
         INPUT iSecondOperand AS integer):

  DEFINE VARIABLE iLoopCounter AS INTEGER no-undo.
  DEFINE VARIABLE iXORedResult AS INTEGER no-undo initial 0.
  DEFINE VARIABLE iFirstOpBit AS INTEGER no-undo.
  DEFINE VARIABLE iSecondOpBit AS INTEGER no-undo.

  DO iLoopCounter = 1 to 32:
    IF GET-BITS(iFirstOperand, iLoopCounter, 1) + GET-BITS(iSecondOperand, iLoopCounter, 1) EQ 1 THEN
    iXORedResult = iXORedResult + EXP(2, iLoopCounter - 1).
 END.

  RETURN iXORedResult.
END FUNCTION. /* BinaryXOR */

FUNCTION PDFendecrypt RETURNS LOGICAL
        (INPUT BufferPtr    AS MEMPTR,
         INPUT PasswordPtr  AS MEMPTR):
  RUN endecrypt
              (INPUT GET-POINTER-VALUE(BufferPtr), 
               INPUT GET-SIZE(BufferPtr),
               INPUT GET-POINTER-VALUE(PasswordPtr),
               INPUT GET-SIZE(PasswordPtr) ) .
  
  RETURN TRUE.
END FUNCTION. /* PDFendecrypt */

PROCEDURE endecrypt EXTERNAL "{&pdfencryptlib}" CDECL PERSISTENT:
    DEFINE INPUT        PARAMETER pBufferPtr    AS LONG   NO-UNDO.
    DEFINE INPUT        PARAMETER pBufferLen    AS LONG   NO-UNDO.
    DEFINE INPUT        PARAMETER pPasswordKey  AS LONG   NO-UNDO.
    DEFINE INPUT        PARAMETER pPasswordLen  AS LONG   NO-UNDO.
END PROCEDURE.


PROCEDURE ReleasePDFencryptlib:  /* PRIVATE */
  RELEASE EXTERNAL "{&pdfencryptlib}".
END.

/* ---------- End of PDFEncryptLib External procedure definitions --------- */


/* -------------- Functions for zlib handling in Progress ----------------- */
FUNCTION CompressBuffer RETURNS INTEGER
         (INPUT        InputBuffer  AS MEMPTR,
          INPUT-OUTPUT OutputBuffer AS MEMPTR,
          OUTPUT       OutputSize   AS INTEGER) :
  
  /* Compress a piece of memory and return a pointer to the compressed data,
     in case of failure the size of compressed data = -1
  */

  DEFINE VARIABLE InputSize  AS INTEGER NO-UNDO.
  DEFINE VARIABLE TempBuffer AS MEMPTR  NO-UNDO.
  
  DEFINE VARIABLE retcode AS INT NO-UNDO.

  InputSize  = GET-SIZE(InputBuffer).
  OutputSize = (InputSize * 1.01) + 12.
  SET-SIZE(TempBuffer) = OutputSize.

  RUN compress 
              (TempBuffer, 
               INPUT-OUTPUT OutputSize, 
               InputBuffer, 
               InputSize, 
               OUTPUT retcode) .
  
  IF retcode = 0 THEN
  DO:
    SET-SIZE(OutputBuffer) = OutputSize.
    OutputBuffer = GET-BYTES(TempBuffer, 1, OutputSize).
  END.
  ELSE 
     OutputSize = -1.

  SET-SIZE(TempBuffer) = 0.

  RETURN retcode.

END FUNCTION. /* Compress Buffer */

FUNCTION DeCompressBuffer RETURNS INTEGER
        (INPUT  InputBuffer  AS MEMPTR,
         OUTPUT OutputBuffer AS MEMPTR,
         OUTPUT OutputSize   AS INTEGER):

  /* DeCompress a piece of memory and return a pointer to the decompressed data,
     in case of failure the size of decompressed data = -1
  */

  DEFINE VARIABLE InputSize  AS INTEGER NO-UNDO.
  DEFINE VARIABLE TempBuffer AS MEMPTR  NO-UNDO.
  
  DEFINE VARIABLE retcode AS INT NO-UNDO.

  InputSize  = GET-SIZE(InputBuffer).
  OutputSize = (InputSize * 100).
  SET-SIZE(TempBuffer) = OutputSize.

  RUN uncompress 
      (TempBuffer, 
       INPUT-OUTPUT OutputSize, 
       InputBuffer, 
       InputSize, 
       OUTPUT retcode).
  
  IF retcode = 0 THEN
  DO:
    SET-SIZE(OutputBuffer) = OutputSize.
    OutputBuffer = GET-BYTES(TempBuffer, 1, OutputSize).
  END.
  ELSE 
     OutputSize = -1.
  SET-SIZE(TempBuffer) = 0.

  RETURN retcode.
END FUNCTION. /* DeCompress Buffer */

FUNCTION compressfile RETURNS LOGICAL
         (INPUT cInputFile  AS CHARACTER,
          INPUT cOutputFile AS CHARACTER):

    DEFINE VARIABLE pFileBuf    AS MEMPTR NO-UNDO.
    DEFINE VARIABLE iFileSize   AS INT  NO-UNDO.
    DEFINE VARIABLE pTargetBuf  AS MEMPTR NO-UNDO.
    DEFINE VARIABLE iTargetSize AS INT  NO-UNDO.

    FILE-INFO:FILE-NAME = cInputFile.
    iFileSize = FILE-INFO:FILE-SIZE.

    SET-SIZE(pFileBuf) = iFIleSize.

    INPUT STREAM S_compress FROM  VALUE(cInputFile) BINARY NO-CONVERT.
    IMPORT STREAM S_compress pFileBuf.
    INPUT STREAM S_compress CLOSE.
    
    compressbuffer(pFileBuf,
                   INPUT-OUTPUT pTargetBuf,
                   OUTPUT iTargetSize).

    IF iTargetSize > 0 THEN
    DO:
      OUTPUT STREAM S_compress TO VALUE(cOutputFile) BINARY NO-CONVERT.
      EXPORT STREAM S_compress pTargetBuf.
      OUTPUT STREAM S_compress CLOSE.
    END.
    
    SET-SIZE(pFileBuf) = 0.
    SET-SIZE(pTargetBuf) = 0.
END FUNCTION. /* Compress File */

FUNCTION decompressfile RETURNS LOGICAL
         (INPUT cInputFile  AS CHARACTER,
          INPUT cOutputFile AS CHARACTER):

    DEFINE VARIABLE iFileSize   AS INT  NO-UNDO.
    DEFINE VARIABLE pFileBuf    AS MEMPTR NO-UNDO.
    DEFINE VARIABLE iTargetSize AS INT  NO-UNDO.
    DEFINE VARIABLE pTargetBuf  AS MEMPTR NO-UNDO.

    FILE-INFO:FILE-NAME = cInputFile.
    iFileSize = FILE-INFO:FILE-SIZE.
    
    SET-SIZE(pFileBuf) = iFIleSize.

    INPUT  STREAM S_compress FROM VALUE(cInputFile) BINARY  NO-CONVERT.
    IMPORT STREAM S_compress pFileBuf  .     
    INPUT STREAM S_compress CLOSE.
    
    decompressbuffer(pFileBuf,
                     OUTPUT pTargetBuf,
                     OUTPUT iTargetSize).

    IF iTargetSize > 0 THEN
    DO:
      OUTPUT STREAM S_compress TO VALUE(cOutputFile) BINARY NO-CONVERT.
      EXPORT STREAM S_compress pTargetBuf.
      OUTPUT STREAM S_compress CLOSE.
    END.
    
    SET-SIZE(pFileBuf) = 0.
    SET-SIZE(pTargetBuf) = 0.
END FUNCTION. /* Decompress File */

/* ----------------- End of Zlib Function Definitons ---------------------- */

/* ------------ Start of External Zlib procedure definitions --------------- */
PROCEDURE compress EXTERNAL "{&zlib}" CDECL PERSISTENT: /* PRIVATE */
    DEFINE INPUT        PARAMETER pDestBuf    AS MEMPTR NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iDestSize   AS LONG NO-UNDO.
    DEFINE INPUT        PARAMETER pSourceBuf  AS MEMPTR NO-UNDO.
    DEFINE INPUT        PARAMETER iSourceSize AS LONG NO-UNDO.
    DEFINE RETURN PARAMETER iretcode AS LONG NO-UNDO.
END PROCEDURE.

PROCEDURE uncompress EXTERNAL "{&zlib}" CDECL PERSISTENT: /* PRIVATE */
    DEFINE INPUT        PARAMETER pDestBuf    AS MEMPTR NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iDestSize   AS LONG NO-UNDO.
    DEFINE INPUT        PARAMETER pSourceBuf  AS MEMPTR NO-UNDO.
    DEFINE INPUT        PARAMETER iSourceSize AS LONG NO-UNDO.
    DEFINE RETURN PARAMETER iretcode AS LONG NO-UNDO.
END PROCEDURE.

PROCEDURE ReleaseZlib: /* PRIVATE */
  RELEASE EXTERNAL "{&zlib}".
END.
/* ---------------- End of External Zlib procedure definitions ------------- */

/* ---------------------------- Define FUNCTIONS -------------------------- */

FUNCTION GetWidgetOption RETURNS CHARACTER   /* PRIVATE */
    (INPUT pOption  AS CHARACTER,
     INPUT pText    AS CHARACTER):

  DEFINE VARIABLE L_text  AS CHARACTER NO-UNDO.

  L_Text = TRIM(SUBSTR(pText, INDEX(pText,pOption + "=") + LENGTH(pOption) + 1)).
  
  RETURN L_Text.

END FUNCTION. /* GetWidgetOption */

FUNCTION GetFileSize RETURNS INTEGER:  /* PRIVATE */

  DEFINE VARIABLE i_Size  AS INTEGER NO-UNDO.

  RETURN FILE-INFO:FILE-SIZE.

END FUNCTION. /* GetFileSize */

  /* The following functions are used to determine the Images Width/Height */

FUNCTION hex RETURNS CHARACTER (INPUT asc-value AS INTEGER). /* PRIVATE */
  DEF VAR j AS INT  NO-UNDO.
  DEF VAR h AS CHAR NO-UNDO.

  DO WHILE TRUE:
    j = asc-value MODULO 16.
    h = (IF j < 10 THEN STRING(j) ELSE CHR(ASC("A") + j - 10)) + h.
    IF asc-value < 16 THEN LEAVE.
      asc-value = (asc-value - j) / 16.
  END.

  IF LENGTH(h, "character":u) = 1 THEN
    h = "0" + h.

  RETURN ("0x" + h).
END FUNCTION. /* hex */

FUNCTION hex2 RETURNS CHARACTER (INPUT asc-value AS INTEGER).
  DEF VAR j AS INT  NO-UNDO.
  DEF VAR h AS CHAR NO-UNDO.

  DO WHILE TRUE:
    j = asc-value MODULO 16.
    h = (IF j < 10 THEN STRING(j) ELSE CHR(ASC("A") + j - 10)) + h.
    IF asc-value < 16 THEN LEAVE.
    asc-value = (asc-value - j) / 16.
  END.

  IF LENGTH(h) = 1 THEN
    h = "0" + h.

  RETURN h.
END FUNCTION.

FUNCTION nextbyte RETURNS INTEGER ():  /* PRIVATE */

  iImageByte = iImageByte + 1.
  RETURN GET-BYTE(mImage,iImageByte).

END FUNCTION. /* nextbyte */

FUNCTION next2bytes RETURNS INTEGER (): /* PRIVATE */
  DEFINE VARIABLE L_c1      AS INTEGER NO-UNDO.
  DEFINE VARIABLE L_c2      AS INTEGER NO-UNDO.

  L_c1 = nextbyte().
  L_c2 = nextbyte().

  RETURN INT(L_c1 * 256 + L_c2).

END FUNCTION. /* next2bytes */

FUNCTION first_marker RETURN LOGICAL (): /* PRIVATE */
  DEFINE VARIABLE L_c1        AS INTEGER NO-UNDO.
  DEFINE VARIABLE L_c2        AS INTEGER NO-UNDO.

  L_c1 = nextbyte().
  L_c2 = nextbyte().

  IF hex(L_c1) <> {&M_Mark} AND hex(L_c2) <> {&M_SOI} THEN
    RETURN FALSE.
  ELSE RETURN TRUE.
END FUNCTION. /* first_marker */

FUNCTION next_marker RETURN INTEGER(): /* PRIVATE */
  DEFINE VARIABLE L_data    AS RAW NO-UNDO.

  LENGTH(L_data) = 1.
  DEFINE VARIABLE L_c       AS INTEGER NO-UNDO.
  DEFINE VARIABLE L_discard AS INTEGER NO-UNDO.

  L_c = nextbyte().
  DO WHILE hex(L_c) <> {&M_MARK}:
    L_discard = L_discard + 1.
    L_c = nextbyte().
  END. /* <> 0xFF */

  DO WHILE hex(L_c) = {&M_MARK}:
    L_c = nextbyte().
  END.

  RETURN L_c.
END FUNCTION. /* next_marker */

FUNCTION skip_variable RETURN LOGICAL (): /* PRIVATE */
  DEFINE VARIABLE L_Length  AS INTEGER NO-UNDO.
  DEFINE VARIABLE L_Loop    AS INTEGER NO-UNDO.

  L_length = next2bytes().

  DO L_Loop = 1 TO (L_Length - 2):
    nextbyte().
  END. /* Loop */
END FUNCTION. /* skip_variable */

FUNCTION process_SOF RETURNS LOGICAL (): /* PRIVATE */
  DEFINE VARIABLE L_Length  AS INTEGER NO-UNDO.

  next2bytes().       /* Skip Length */
  nextbyte().         /* Skip Data Precision */
  pdf_height = next2bytes().
  pdf_width  = next2bytes().

END FUNCTION. /* process_SOF */

/* end of Functions used to determine Image Height/Width */

FUNCTION dec2string RETURNS CHARACTER (INPUT decvalue AS DECIMAL): /* PRIVATE */
  IF SESSION:NUMERIC-FORMAT = "EUROPEAN" THEN
    RETURN REPLACE( STRING(decvalue),",",".").
  ELSE
    RETURN STRING(decvalue).
END.  /* dec2string */

FUNCTION string2dec RETURNS DECIMAL (INPUT stringvalue AS CHARACTER): /* PRIVATE */
  IF SESSION:NUMERIC-FORMAT = "EUROPEAN" THEN
    RETURN DEC(REPLACE(stringvalue,".",",")).
  ELSE
    RETURN DEC(stringvalue).
END.  /* string2dec */

PROCEDURE pdf_error :  /* Private */
  DEFINE INPUT PARAMETER pdfStream     AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfFunction   AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfError      AS CHARACTER NO-UNDO.

  CREATE TT_pdf_error.
  ASSIGN TT_pdf_error.obj_stream = pdfStream
         TT_pdf_error.obj_func   = pdfFunction
         TT_pdf_error.obj_error  = pdfError.

END. /* pdf_error */

FUNCTION pdf_get_parameter RETURNS CHARACTER
         (INPUT pdfStream     AS CHARACTER,
          INPUT pdfParameter  AS CHARACTER):

  DEFINE BUFFER B_TT_pdf_param FOR TT_pdf_param.

  FIND B_TT_pdf_param WHERE B_TT_pdf_param.obj_stream = pdfStream
                        AND B_TT_pdf_param.obj_parameter = pdfParameter
                        NO-LOCK NO-ERROR.
  IF AVAIL B_TT_pdf_param THEN
    RETURN B_TT_pdf_param.obj_value.
  ELSE
    RETURN "".

END FUNCTION. /* pdf_get_parameter */

FUNCTION pdf_Page RETURN INTEGER ( INPUT pdfStream AS CHARACTER):

  FIND FIRST TT_pdf_stream
       WHERE TT_pdf_stream.obj_stream = pdfStream NO-LOCK NO-ERROR.
  IF NOT AVAIL TT_pdf_stream THEN DO:
    RUN pdf_error(pdfStream,"pdf_Page","Cannot find Stream!").
    RETURN ERROR.
  END.

  FIND FIRST TT_pdf_param WHERE TT_pdf_param.obj_stream    = pdfStream
                            AND TT_pdf_param.obj_parameter = "Page"
                            NO-LOCK NO-ERROR.
  IF AVAIL TT_pdf_param THEN
    RETURN INT(TT_pdf_param.obj_value).
  ELSE
    RETURN 0.

END FUNCTION. /* pdf_Page */


FUNCTION pdf_LeftMargin RETURN INTEGER ( INPUT pdfStream AS CHARACTER):

  IF NOT CAN-FIND(FIRST TT_pdf_stream
                  WHERE TT_pdf_stream.obj_stream = pdfStream NO-LOCK)
  THEN DO:
    RUN pdf_error(pdfStream,"pdf_LeftMargin","Cannot find Stream!").
    RETURN ERROR.
  END.

  FIND FIRST TT_pdf_param WHERE TT_pdf_param.obj_stream    = pdfStream
                            AND TT_pdf_param.obj_parameter = "LeftMargin"
                            NO-LOCK NO-ERROR.
  IF AVAIL TT_pdf_param THEN
    RETURN INT(TT_pdf_param.obj_value).
  ELSE
    RETURN 10.

END FUNCTION. /* pdf_LeftMargin */

FUNCTION pdf_TopMargin RETURN INTEGER ( INPUT pdfStream AS CHARACTER):

  IF NOT CAN-FIND(FIRST TT_pdf_stream
                  WHERE TT_pdf_stream.obj_stream = pdfStream NO-LOCK)
  THEN DO:
    RUN pdf_error(pdfStream,"pdf_TopMargin","Cannot find Stream!").
    RETURN ERROR.
  END.

  FIND FIRST TT_pdf_param WHERE TT_pdf_param.obj_stream    = pdfStream
                            AND TT_pdf_param.obj_parameter = "TopMargin"
                            NO-LOCK NO-ERROR.
  IF AVAIL TT_pdf_param THEN
    RETURN INT(TT_pdf_param.obj_value).
  ELSE
    RETURN 50.

END FUNCTION. /* pdf_TopMargin */

FUNCTION pdf_BottomMargin RETURN INTEGER ( INPUT pdfStream AS CHARACTER):

  IF NOT CAN-FIND(FIRST TT_pdf_stream
                  WHERE TT_pdf_stream.obj_stream = pdfStream NO-LOCK)
  THEN DO:
    RUN pdf_error(pdfStream,"pdf_BottomMargin","Cannot find Stream!").
    RETURN ERROR.
  END.

  FIND FIRST TT_pdf_param WHERE TT_pdf_param.obj_stream    = pdfStream
                            AND TT_pdf_param.obj_parameter = "BottomMargin"
                            NO-LOCK NO-ERROR.
  IF AVAIL TT_pdf_param THEN
    RETURN INT(TT_pdf_param.obj_value).
  ELSE
    RETURN 50.

END FUNCTION. /* pdf_BottomMargin */

FUNCTION pdf_Font RETURN CHARACTER ( INPUT pdfStream AS CHARACTER):

  IF NOT CAN-FIND(FIRST TT_pdf_stream
                  WHERE TT_pdf_stream.obj_stream = pdfStream NO-LOCK)
  THEN DO:
    RUN pdf_error(pdfStream,"pdf_Font","Cannot find Stream!").
    RETURN ERROR.
  END.

  FIND FIRST TT_pdf_param WHERE TT_pdf_param.obj_stream    = pdfStream
                            AND TT_pdf_param.obj_parameter = "Font"
                            NO-LOCK NO-ERROR.
  IF AVAIL TT_pdf_param THEN
    RETURN TT_pdf_param.obj_value.
  ELSE
    RETURN "Courier".

END FUNCTION. /* pdf_Font */

FUNCTION pdf_Font_Loaded RETURN LOGICAL
        ( INPUT pdfStream AS CHARACTER,
          INPUT pdfFont   AS CHARACTER):

  IF NOT CAN-FIND(FIRST TT_pdf_stream
                  WHERE TT_pdf_stream.obj_stream = pdfStream NO-LOCK)
  THEN DO:
    RUN pdf_error(pdfStream,"pdf_Font_Loaded","Cannot find Stream!").
    RETURN ERROR.
  END.

  FIND FIRST TT_pdf_Font WHERE TT_pdf_Font.obj_stream = pdfStream
                           AND TT_pdf_Font.font_name  = pdfFont
                           NO-LOCK NO-ERROR.
  IF AVAIL TT_pdf_Font THEN
    RETURN TRUE.
  ELSE
    RETURN FALSE.

END FUNCTION. /* pdf_Font_Loaded */

FUNCTION pdf_FontType RETURN CHARACTER ( INPUT pdfStream AS CHARACTER):

  DEFINE BUFFER L_B_TT_pdf_font FOR TT_pdf_font.

  DEFINE VARIABLE L_Font    AS CHARACTER NO-UNDO.

  IF NOT CAN-FIND(FIRST TT_pdf_stream
                  WHERE TT_pdf_stream.obj_stream = pdfStream NO-LOCK)
  THEN DO:
    RUN pdf_error(pdfStream,"pdf_FontType","Cannot find Stream!").
    RETURN ERROR.
  END.

  L_Font = pdf_Font(pdfStream).
  FIND FIRST L_B_TT_pdf_font WHERE L_B_TT_pdf_font.obj_stream = pdfStream
                               AND L_B_TT_pdf_font.font_name = L_Font
                               NO-LOCK NO-ERROR.
  IF AVAIL L_B_TT_pdf_font THEN
    RETURN L_B_TT_pdf_font.Font_Type.
  ELSE
    RETURN "FIXED".

END FUNCTION. /* pdf_FontType */

FUNCTION pdf_ImageDim RETURN INTEGER ( INPUT pdfStream AS CHARACTER,
                                       INPUT pdfImage  AS CHARACTER,
                                       INPUT pdfDim    AS CHARACTER):

  DEFINE BUFFER L_B_TT_pdf_image FOR TT_pdf_image.

  IF NOT CAN-FIND(FIRST TT_pdf_stream
                  WHERE TT_pdf_stream.obj_stream = pdfStream NO-LOCK)
  THEN DO:
    RUN pdf_error(pdfStream,"pdf_ImageDim","Cannot find Stream!").
    RETURN ERROR.
  END.

  FIND FIRST L_B_TT_pdf_image WHERE L_B_TT_pdf_image.obj_stream = pdfStream
                               AND L_B_TT_pdf_image.image_name  = pdfImage
                               NO-LOCK NO-ERROR.
  IF AVAIL L_B_TT_pdf_image THEN DO:
    IF pdfDim = "HEIGHT" THEN
      RETURN L_B_TT_pdf_image.image_h.
    ELSE IF pdfDim = "WIDTH" THEN
      RETURN L_B_TT_pdf_image.image_w.
    ELSE
      RETURN 0.
  END.
  ELSE
    RETURN 0.

END FUNCTION. /* pdf_ImageDim */

FUNCTION pdf_TextX RETURN INTEGER ( INPUT pdfStream AS CHARACTER):

  IF NOT CAN-FIND (FIRST TT_pdf_stream
                   WHERE TT_pdf_stream.obj_stream = pdfStream NO-LOCK)
  THEN DO:
    RUN pdf_error(pdfStream,"pdf_TextX","Cannot find Stream!").
    RETURN ERROR.
  END.

  FIND FIRST TT_pdf_param WHERE TT_pdf_param.obj_stream    = pdfStream
                            AND TT_pdf_param.obj_parameter = "TextX"
                            NO-LOCK NO-ERROR.
  IF AVAIL TT_pdf_param THEN
    RETURN INT(TT_pdf_param.obj_value).
  ELSE
    RETURN 0.

END FUNCTION. /* pdf_TextX*/

FUNCTION pdf_TextY RETURN INTEGER ( INPUT pdfStream AS CHARACTER):

  FIND FIRST TT_pdf_stream
                  WHERE TT_pdf_stream.obj_stream = pdfStream NO-LOCK NO-ERROR.
  IF NOT AVAIL TT_pdf_stream THEN DO:
    RUN pdf_error(pdfStream,"pdf_TextY","Cannot find Stream!").
    RETURN ERROR.
  END.

  FIND FIRST TT_pdf_param WHERE TT_pdf_param.obj_stream    = pdfStream
                            AND TT_pdf_param.obj_parameter = "TextY"
                            NO-LOCK NO-ERROR.

  IF AVAIL TT_pdf_param THEN
    RETURN INT(TT_pdf_param.obj_value).
  ELSE
    RETURN 0.

END FUNCTION. /* pdf_TextY */

/* PRIVATE */
FUNCTION ObjectSequence RETURNS INTEGER ( INPUT pdfStream     AS CHARACTER,
                                          INPUT pdfSequence   AS INTEGER,
                                          INPUT pdfObjectDesc AS CHARACTER,
                                          INPUT pdfPage       AS INTEGER ):

  IF NOT CAN-FIND(FIRST TT_pdf_stream
                  WHERE TT_pdf_stream.obj_stream = pdfStream NO-LOCK)
  THEN DO:
    RUN pdf_error(pdfStream,"ObjectSequence","Cannot find Stream!").
    RETURN ERROR.
  END.

  CREATE TT_pdf_object.
  ASSIGN TT_pdf_object.obj_stream = pdfStream
         TT_pdf_object.obj_nbr    = pdfSequence
         TT_pdf_object.obj_desc   = pdfObjectDesc
         TT_pdf_object.obj_offset = IF pdfSequence <> 0 THEN
                                      SEEK(S_pdf_inc) + 1
                                    ELSE 0
         TT_pdf_object.gen_nbr    = IF pdfSequence <> 0 THEN 0
                                    ELSE 65535
         TT_pdf_object.obj_type   = IF pdfSequence = 0 THEN "f"
                                    ELSE "n"
         TT_pdf_object.obj_page   = pdfPage.

  pdf_inc_ObjectSequence = pdfSequence.

  RETURN pdf_inc_ObjectSequence.

END FUNCTION. /* ObjectSequence */

PROCEDURE pdf_new :
  DEFINE INPUT PARAMETER pdfStream   AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfFileName AS CHARACTER NO-UNDO.
  
  DEFINE BUFFER B_TT_pdf_Stream FOR TT_pdf_stream.

  IF INDEX(pdfStream, " ") > 0 THEN DO:
    RUN pdf_error(pdfStream,"pdf_new","Cannot have a space in the Stream Name!").
    RETURN.
  END.

  CREATE B_TT_pdf_stream.
  ASSIGN B_TT_pdf_stream.obj_stream = pdfStream
         B_TT_pdf_stream.obj_file   = pdfFileName.

  /* Determine Unique Stream ID -- this string is used to output temporary files
     that are used for building content or generating Encryption keys etc */
  B_TT_pdf_stream.obj_UniqueID = ENCODE(STRING(TODAY) + STRING(TIME) + B_TT_pdf_stream.obj_file).

  /* Delete any pre-existing temp files for stream */
  IF OPSYS = "UNIX" THEN
    OS-COMMAND SILENT VALUE("rm -f " + SESSION:TEMP-DIR + pdfStream + "*.txt").
  ELSE
    OS-COMMAND SILENT VALUE("del " + SESSION:TEMP-DIR + pdfStream + "*.txt").

  RUN pdf_LoadBase14 (pdfStream).
  RUN pdf_init_param (pdfStream).

  RUN pdf_set_info(pdfStream,"OutputTo",pdfFileName).

  ASSIGN B_TT_pdf_stream.obj_DoingText    = FALSE
         B_TT_pdf_stream.obj_DoingGraphic = FALSE.

  pdf_CurrentStream = pdfStream.

END. /* pdf_new */

FUNCTION pdf_VerticalSpace RETURN DECIMAL ( INPUT pdfStream AS CHARACTER):

  IF NOT CAN-FIND(FIRST TT_pdf_stream
                  WHERE TT_pdf_stream.obj_stream = pdfStream NO-LOCK)
  THEN DO:
    RUN pdf_error(pdfStream,"pdf_VerticalSpace","Cannot find Stream!").
    RETURN ERROR.
  END.

  FIND FIRST TT_pdf_param WHERE TT_pdf_param.obj_stream    = pdfStream
                            AND TT_pdf_param.obj_parameter = "VerticalSpace"
                            NO-LOCK NO-ERROR.
  IF AVAIL TT_pdf_param THEN
    RETURN string2dec(TT_pdf_param.obj_value).
  ELSE
    RETURN 0.0.

END FUNCTION. /* pdf_VerticalSpace */

FUNCTION pdf_PointSize RETURN DECIMAL ( INPUT pdfStream AS CHARACTER ):

  IF NOT CAN-FIND(FIRST TT_pdf_stream
                  WHERE TT_pdf_stream.obj_stream = pdfStream NO-LOCK)
  THEN DO:
    RUN pdf_error(pdfStream,"pdf_PointSize","Cannot find Stream!").
    RETURN ERROR.
  END.

  FIND FIRST TT_pdf_param WHERE TT_pdf_param.obj_stream    = pdfStream
                            AND TT_pdf_param.obj_parameter = "PointSize"
                            NO-LOCK NO-ERROR.
  IF AVAIL TT_pdf_param THEN
    RETURN string2dec(TT_pdf_param.obj_value).
  ELSE
    RETURN 10.0.

END FUNCTION. /* pdf_PointSize */

FUNCTION pdf_text_width RETURNS INTEGER ( INPUT pdfStream   AS CHARACTER,
                                          INPUT pdfText     AS CHARACTER):

  DEFINE VARIABLE L_width  AS INTEGER NO-UNDO.
  DEFINE VARIABLE L_font    AS CHARACTER NO-UNDO.

  L_font = pdf_Font(pdfStream).

  DEFINE VARIABLE L_Loop    AS INTEGER NO-UNDO.
  DEFINE VARIABLE L_tot     AS INTEGER NO-UNDO.

  IF NOT CAN-FIND(FIRST TT_pdf_stream
                  WHERE TT_pdf_stream.obj_stream = pdfStream NO-LOCK)
  THEN DO:
    RUN pdf_error(pdfStream,"pdf_text_width","Cannot find Stream!").
    RETURN ERROR.
  END.

  FIND FIRST TT_pdf_font WHERE TT_pdf_font.obj_stream = pdfStream
                           AND TT_pdf_font.font_name  = L_Font
                           NO-LOCK NO-ERROR.
  IF AVAIL TT_pdf_font THEN DO:
    IF TT_pdf_Font.font_type = "FIXED" THEN DO:
      L_width = INT((LENGTH(pdfText, "character":u) * INT(TT_pdf_font.font_width) / 1000) * pdf_PointSize(pdfStream)).
    end.
    ELSE DO:
      DO L_loop = 1 TO LENGTH(pdfText, "character":u):
        L_tot = L_tot + INT(ENTRY(INT(ASC(SUBSTR(pdfText,L_Loop,1, "character":u))) + 1 ,TT_pdf_Font.font_width, " ")) NO-ERROR.
      END.

      l_width = INT((L_tot / 1000) * pdf_PointSize(pdfStream)) NO-ERROR.
    END. /* Variable Width Font */
  END. /* Found the current font */

  RETURN l_width.
END FUNCTION. /* pdf_text_width */

FUNCTION pdf_text_widthdec RETURNS DECIMAL ( INPUT pdfStream   AS CHARACTER,
                                             INPUT pdfText     AS CHARACTER):

  DEFINE VARIABLE L_width   AS DECIMAL DECIMALS 5 NO-UNDO.
  DEFINE VARIABLE L_font    AS CHARACTER NO-UNDO.

  L_font = pdf_Font(pdfStream).

  DEFINE VARIABLE L_Loop    AS INTEGER NO-UNDO.
  DEFINE VARIABLE L_tot     AS DECIMAL NO-UNDO.

  IF NOT CAN-FIND(FIRST TT_pdf_stream
                  WHERE TT_pdf_stream.obj_stream = pdfStream NO-LOCK)
  THEN DO:
    RUN pdf_error(pdfStream,"pdf_text_widthdec","Cannot find Stream!").
    RETURN ERROR.
  END.

  FIND FIRST TT_pdf_font WHERE TT_pdf_font.obj_stream = pdfStream
                           AND TT_pdf_font.font_name  = L_Font
                           NO-LOCK NO-ERROR.
  IF AVAIL TT_pdf_font THEN DO:
    IF TT_pdf_Font.font_type = "FIXED" THEN DO:
      L_width = DEC((LENGTH(pdfText, "character":u) * INT(TT_pdf_font.font_width) / 1000) * pdf_PointSize(pdfStream)).
    END.
    ELSE DO:
      DO L_loop = 1 TO LENGTH(pdfText, "character":u):
        L_tot = L_tot + INT(ENTRY(INT(ASC(SUBSTR(pdfText,L_Loop,1, "character":u))) + 1 ,TT_pdf_Font.font_width, " ")) NO-ERROR.
      END.

      l_width = DEC((L_tot * pdf_PointSize(pdfStream)) / 1000) NO-ERROR.
    END. /* Variable Width Font */
  END. /* Found the current font */

  RETURN L_width.
END FUNCTION. /* pdf_text_widthDec */

FUNCTION pdf_text_widthdec2 RETURNS DECIMAL ( INPUT pdfStream   AS CHARACTER,
                                              INPUT pdfFontTag  AS CHARACTER, 
                                              INPUT pdfFontSize AS DECIMAL,
                                              INPUT pdfText     AS CHARACTER):


  DEFINE VARIABLE L_Loop    AS INTEGER NO-UNDO.
  DEFINE VARIABLE L_tot     AS DECIMAL NO-UNDO.
  DEFINE VARIABLE L_Width   AS DECIMAL NO-UNDO.

  IF NOT CAN-FIND(FIRST TT_pdf_stream
                  WHERE TT_pdf_stream.obj_stream = pdfStream NO-LOCK)
  THEN DO:
    RUN pdf_error(pdfStream,"pdf_text_widthdec2","Cannot find Stream!").
    RETURN ERROR.
  END.

  FIND FIRST TT_pdf_font WHERE TT_pdf_font.obj_stream = pdfStream
                           AND TT_pdf_font.font_tag   = pdfFontTag
                           NO-LOCK NO-ERROR.
  IF AVAIL TT_pdf_font THEN DO:
    IF TT_pdf_Font.font_type = "FIXED" THEN DO:
      L_width = DEC((LENGTH(pdfText, "character":u) * INT(TT_pdf_font.font_width) / 1000) * pdfFontSize).
    END.
    ELSE DO:
      DO L_loop = 1 TO LENGTH(pdfText, "character":u):
        L_tot = L_tot + INT(ENTRY(INT(ASC(SUBSTR(pdfText,L_Loop,1, "character":u))) + 1 ,TT_pdf_Font.font_width, " ")) NO-ERROR.
      END.

      l_width = DEC((L_tot * pdfFontSize) / 1000) NO-ERROR.
    END. /* Variable Width Font */
  END. /* Found the current font */

  RETURN L_width.
END FUNCTION. /* pdf_text_widthDec2 */

FUNCTION pdf_text_fontwidth RETURNS DECIMAL ( INPUT pdfStream   AS CHARACTER,
                                              INPUT pdfFont     AS CHARACTER,
                                              INPUT pdfText     AS CHARACTER):

  DEFINE VARIABLE L_width   AS DECIMAL DECIMALS 5 NO-UNDO.

  DEFINE VARIABLE L_Loop    AS INTEGER NO-UNDO.
  DEFINE VARIABLE L_tot     AS DECIMAL NO-UNDO.

  IF NOT CAN-FIND(FIRST TT_pdf_stream
                  WHERE TT_pdf_stream.obj_stream = pdfStream NO-LOCK)
  THEN DO:
    RUN pdf_error(pdfStream,"pdf_text_fontwidth","Cannot find Stream!").
    RETURN ERROR.
  END.

  FIND FIRST TT_pdf_font WHERE TT_pdf_font.obj_stream = pdfStream
                           AND TT_pdf_font.font_name  = pdfFont
                           NO-LOCK NO-ERROR.
  IF AVAIL TT_pdf_font THEN DO:
    IF TT_pdf_Font.font_type = "FIXED" THEN DO:
      L_width = DEC((LENGTH(pdfText, "character":u) * INT(TT_pdf_font.font_width) / 1000) * pdf_PointSize(pdfStream)).
    END.
    ELSE DO:
      DO L_loop = 1 TO LENGTH(pdfText, "character":u):
        L_tot = L_tot + INT(ENTRY(INT(ASC(SUBSTR(pdfText,L_Loop,1, "character":u))) + 1 ,TT_pdf_Font.font_width, " ")) NO-ERROR.
      END.
      
      l_width = DEC((L_tot * pdf_PointSize(pdfStream)) / 1000) NO-ERROR.
    END. /* Variable Width Font */
  END. /* Found the current font */

  RETURN L_width.
END FUNCTION. /* pdf_text_fontwidth */

FUNCTION pdf_text_fontwidth2 RETURNS DECIMAL ( INPUT pdfStream   AS CHARACTER,
                                               INPUT pdfFont     AS CHARACTER,
                                               INPUT pdfFontSize AS DECIMAL,
                                               INPUT pdfText     AS CHARACTER):

  DEFINE VARIABLE L_width   AS DECIMAL DECIMALS 5 NO-UNDO.

  DEFINE VARIABLE L_Loop    AS INTEGER NO-UNDO.
  DEFINE VARIABLE L_tot     AS DECIMAL NO-UNDO.

  IF NOT CAN-FIND(FIRST TT_pdf_stream
                  WHERE TT_pdf_stream.obj_stream = pdfStream NO-LOCK)
  THEN DO:
    RUN pdf_error(pdfStream,"pdf_text_fontwidth","Cannot find Stream!").
    RETURN ERROR.
  END.

  FIND FIRST TT_pdf_font WHERE TT_pdf_font.obj_stream = pdfStream
                           AND TT_pdf_font.font_name  = pdfFont
                           NO-LOCK NO-ERROR.
  IF AVAIL TT_pdf_font THEN DO:
    IF TT_pdf_Font.font_type = "FIXED" THEN DO:
      L_width = DEC((LENGTH(pdfText, "character":u) * INT(TT_pdf_font.font_width) / 1000) * pdfFontSize).
    END.
    ELSE DO:
      DO L_loop = 1 TO LENGTH(pdfText, "character":u):
        L_tot = L_tot + INT(ENTRY(INT(ASC(SUBSTR(pdfText,L_Loop,1, "character":u))) + 1 ,TT_pdf_Font.font_width, " ")) NO-ERROR.
      END.
      
      l_width = DEC((L_tot * pdfFontSize) / 1000) NO-ERROR.
    END. /* Variable Width Font */
  END. /* Found the current font */

  RETURN L_width.
END FUNCTION. /* pdf_text_fontwidth2 */

PROCEDURE pdf_set_TextRed :
  DEFINE INPUT PARAMETER pdfStream          AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfValue           AS DECIMAL NO-UNDO.

  IF NOT CAN-FIND(FIRST TT_pdf_stream
                  WHERE TT_pdf_stream.obj_stream = pdfStream NO-LOCK)
  THEN DO:
    RUN pdf_error(pdfStream,"pdf_set_TextRed","Cannot find Stream!").
    RETURN ERROR.
  END.

  FIND FIRST TT_pdf_param
       WHERE TT_pdf_param.obj_stream    = pdfStream
         AND TT_pdf_param.obj_parameter = "TextRed" EXCLUSIVE-LOCK NO-ERROR.
  IF NOT AVAIL TT_pdf_param THEN DO:
    CREATE TT_pdf_param.
    ASSIGN TT_pdf_param.obj_stream    = pdfStream
           TT_pdf_param.obj_parameter = "TextRed".
  END.

  TT_pdf_param.obj_value = dec2string(pdfValue).

END. /* pdf_set_TextRed */

PROCEDURE pdf_set_TextGreen :
  DEFINE INPUT PARAMETER pdfStream          AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfValue           AS DECIMAL NO-UNDO.

  IF NOT CAN-FIND(FIRST TT_pdf_stream
                  WHERE TT_pdf_stream.obj_stream = pdfStream NO-LOCK)
  THEN DO:
    RUN pdf_error(pdfStream,"pdf_set_TextGreen","Cannot find Stream!").
    RETURN .
  END.

  FIND FIRST TT_pdf_param
       WHERE TT_pdf_param.obj_stream    = pdfStream
         AND TT_pdf_param.obj_parameter = "TextGreen" EXCLUSIVE-LOCK NO-ERROR.
  IF NOT AVAIL TT_pdf_param THEN DO:
    CREATE TT_pdf_param.
    ASSIGN TT_pdf_param.obj_stream    = pdfStream
           TT_pdf_param.obj_parameter = "TextGreen".
  END.

  TT_pdf_param.obj_value = dec2string(pdfValue).

END. /* pdf_set_TextGreen */

PROCEDURE pdf_set_TextBlue :
  DEFINE INPUT PARAMETER pdfStream          AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfValue           AS DECIMAL NO-UNDO.

  IF NOT CAN-FIND(FIRST TT_pdf_stream
                  WHERE TT_pdf_stream.obj_stream = pdfStream NO-LOCK)
  THEN DO:
    RUN pdf_error(pdfStream,"pdf_set_TextBlue","Cannot find Stream!").
    RETURN.
  END.

  FIND FIRST TT_pdf_param
       WHERE TT_pdf_param.obj_stream    = pdfStream
         AND TT_pdf_param.obj_parameter = "TextBlue" EXCLUSIVE-LOCK NO-ERROR.
  IF NOT AVAIL TT_pdf_param THEN DO:
    CREATE TT_pdf_param.
    ASSIGN TT_pdf_param.obj_stream    = pdfStream
           TT_pdf_param.obj_parameter = "TextBlue".
  END.

  TT_pdf_param.obj_value = dec2string(pdfValue).

END. /* pdf_set_TextBlue */

FUNCTION pdf_TextRed RETURN DECIMAL ( INPUT pdfStream AS CHARACTER):

  IF NOT CAN-FIND(FIRST TT_pdf_stream
                  WHERE TT_pdf_stream.obj_stream = pdfStream NO-LOCK)
  THEN DO:
    RUN pdf_error(pdfStream,"pdf_TextRed","Cannot find Stream!").
    RETURN ERROR.
  END.

  FIND FIRST TT_pdf_param WHERE TT_pdf_param.obj_stream    = pdfStream
                            AND TT_pdf_param.obj_parameter = "TextRed"
                            NO-LOCK NO-ERROR.
  IF AVAIL TT_pdf_param THEN
    RETURN string2dec(TT_pdf_param.obj_value).
  ELSE
    RETURN 0.0.

END FUNCTION. /* pdf_TextRed */

FUNCTION pdf_TextGreen RETURN DECIMAL ( INPUT pdfStream AS CHARACTER):

  IF NOT CAN-FIND(FIRST TT_pdf_stream
                  WHERE TT_pdf_stream.obj_stream = pdfStream NO-LOCK)
  THEN DO:
    RUN pdf_error(pdfStream,"pdf_TextGreen","Cannot find Stream!").
    RETURN ERROR.
  END.

  FIND FIRST TT_pdf_param WHERE TT_pdf_param.obj_stream    = pdfStream
                            AND TT_pdf_param.obj_parameter = "TextGreen"
                            NO-LOCK NO-ERROR.
  IF AVAIL TT_pdf_param THEN
    RETURN string2dec(TT_pdf_param.obj_value).
  ELSE
    RETURN 0.0.

END FUNCTION. /* pdf_TextGreen */

FUNCTION pdf_TextBlue RETURN DECIMAL ( INPUT pdfStream AS CHARACTER):

  IF NOT CAN-FIND(FIRST TT_pdf_stream
                  WHERE TT_pdf_stream.obj_stream = pdfStream NO-LOCK)
  THEN DO:
    RUN pdf_error(pdfStream,"pdf_TextBlue","Cannot find Stream!").
    RETURN ERROR.
  END.

  FIND FIRST TT_pdf_param WHERE TT_pdf_param.obj_stream    = pdfStream
                            AND TT_pdf_param.obj_parameter = "TextBlue"
                            NO-LOCK NO-ERROR.
  IF AVAIL TT_pdf_param THEN
    RETURN string2dec(TT_pdf_param.obj_value).
  ELSE
    RETURN 0.0.

END FUNCTION. /* pdf_TextBlue */

PROCEDURE pdf_set_FillRed :
  DEFINE INPUT PARAMETER pdfStream          AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfValue           AS DECIMAL NO-UNDO.

  IF NOT CAN-FIND(FIRST TT_pdf_stream
                  WHERE TT_pdf_stream.obj_stream = pdfStream NO-LOCK)
  THEN DO:
    RUN pdf_error(pdfStream,"pdf_set_FillRed","Cannot find Stream!").
    RETURN ERROR.
  END.

  FIND FIRST TT_pdf_param
       WHERE TT_pdf_param.obj_stream    = pdfStream
         AND TT_pdf_param.obj_parameter = "FillRed" EXCLUSIVE-LOCK NO-ERROR.
  IF NOT AVAIL TT_pdf_param THEN DO:
    CREATE TT_pdf_param.
    ASSIGN TT_pdf_param.obj_stream    = pdfStream
           TT_pdf_param.obj_parameter = "FillRed".
  END.

  TT_pdf_param.obj_value = dec2string(pdfValue).

END. /* pdf_set_FillRed */

PROCEDURE pdf_set_FillGreen :
  DEFINE INPUT PARAMETER pdfStream          AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfValue           AS DECIMAL NO-UNDO.

  IF NOT CAN-FIND(FIRST TT_pdf_stream
                  WHERE TT_pdf_stream.obj_stream = pdfStream NO-LOCK)
  THEN DO:
    RUN pdf_error(pdfStream,"pdf_set_FillGreen","Cannot find Stream!").
    RETURN .
  END.

  FIND FIRST TT_pdf_param
       WHERE TT_pdf_param.obj_stream    = pdfStream
         AND TT_pdf_param.obj_parameter = "FillGreen" EXCLUSIVE-LOCK NO-ERROR.
  IF NOT AVAIL TT_pdf_param THEN DO:
    CREATE TT_pdf_param.
    ASSIGN TT_pdf_param.obj_stream    = pdfStream
           TT_pdf_param.obj_parameter = "FillGreen".
  END.

  TT_pdf_param.obj_value = dec2string(pdfValue).

END. /* pdf_set_FillGreen */

PROCEDURE pdf_set_FillBlue :
  DEFINE INPUT PARAMETER pdfStream          AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfValue           AS DECIMAL NO-UNDO.

  IF NOT CAN-FIND(FIRST TT_pdf_stream
                  WHERE TT_pdf_stream.obj_stream = pdfStream NO-LOCK)
  THEN DO:
    RUN pdf_error(pdfStream,"pdf_set_FillBlue","Cannot find Stream!").
    RETURN.
  END.

  FIND FIRST TT_pdf_param
       WHERE TT_pdf_param.obj_stream    = pdfStream
         AND TT_pdf_param.obj_parameter = "FillBlue" EXCLUSIVE-LOCK NO-ERROR.
  IF NOT AVAIL TT_pdf_param THEN DO:
    CREATE TT_pdf_param.
    ASSIGN TT_pdf_param.obj_stream    = pdfStream
           TT_pdf_param.obj_parameter = "FillBlue".
  END.

  TT_pdf_param.obj_value = dec2string(pdfValue).

END. /* pdf_set_FillBlue */

FUNCTION pdf_FillRed RETURN DECIMAL ( INPUT pdfStream AS CHARACTER):

  IF NOT CAN-FIND(FIRST TT_pdf_stream
                  WHERE TT_pdf_stream.obj_stream = pdfStream NO-LOCK)
  THEN DO:
    RUN pdf_error(pdfStream,"pdf_FillRed","Cannot find Stream!").
    RETURN ERROR.
  END.

  FIND FIRST TT_pdf_param WHERE TT_pdf_param.obj_stream    = pdfStream
                            AND TT_pdf_param.obj_parameter = "FillRed"
                            NO-LOCK NO-ERROR.
  IF AVAIL TT_pdf_param THEN
    RETURN string2dec(TT_pdf_param.obj_value).
  ELSE
    RETURN 0.0.

END FUNCTION. /* pdf_FillRed */

FUNCTION pdf_FillGreen RETURN DECIMAL ( INPUT pdfStream AS CHARACTER):

  IF NOT CAN-FIND(FIRST TT_pdf_stream
                  WHERE TT_pdf_stream.obj_stream = pdfStream NO-LOCK)
  THEN DO:
    RUN pdf_error(pdfStream,"pdf_FillGreen","Cannot find Stream!").
    RETURN ERROR.
  END.

  FIND FIRST TT_pdf_param WHERE TT_pdf_param.obj_stream    = pdfStream
                            AND TT_pdf_param.obj_parameter = "FillGreen"
                            NO-LOCK NO-ERROR.
  IF AVAIL TT_pdf_param THEN
    RETURN string2dec(TT_pdf_param.obj_value).
  ELSE
    RETURN 0.0.

END FUNCTION. /* pdf_FillGreen */

FUNCTION pdf_FillBlue RETURN DECIMAL ( INPUT pdfStream AS CHARACTER):

  IF NOT CAN-FIND(FIRST TT_pdf_stream
                  WHERE TT_pdf_stream.obj_stream = pdfStream NO-LOCK)
  THEN DO:
    RUN pdf_error(pdfStream,"pdf_FillBlue","Cannot find Stream!").
    RETURN ERROR.
  END.

  FIND FIRST TT_pdf_param WHERE TT_pdf_param.obj_stream    = pdfStream
                            AND TT_pdf_param.obj_parameter = "FillBlue"
                            NO-LOCK NO-ERROR.
  IF AVAIL TT_pdf_param THEN
    RETURN string2dec(TT_pdf_param.obj_value).
  ELSE
    RETURN 0.0.

END FUNCTION. /* pdf_FillBlue */

FUNCTION pdf_PageRotate RETURN INTEGER ( INPUT pdfStream AS CHARACTER):

  IF NOT CAN-FIND(FIRST TT_pdf_stream
                  WHERE TT_pdf_stream.obj_stream = pdfStream NO-LOCK)
  THEN DO:
    RUN pdf_error(pdfStream,"pdf_PageRotate","Cannot find Stream!").
    RETURN ERROR.
  END.

  FIND FIRST TT_pdf_param WHERE TT_pdf_param.obj_stream    = pdfStream
                            AND TT_pdf_param.obj_parameter = "PageRotate"
                            NO-LOCK NO-ERROR.
  IF AVAIL TT_pdf_param THEN
    RETURN INT(TT_pdf_param.obj_value).
  ELSE
    RETURN 0.

END FUNCTION. /* pdf_PageRotate */


FUNCTION pdf_PageWidth RETURN INTEGER ( INPUT pdfStream AS CHARACTER):

  IF NOT CAN-FIND(FIRST TT_pdf_stream
                  WHERE TT_pdf_stream.obj_stream = pdfStream NO-LOCK)
  THEN DO:
    RUN pdf_error(pdfStream,"pdf_PageWidth","Cannot find Stream!").
    RETURN ERROR.
  END.

  FIND FIRST TT_pdf_param WHERE TT_pdf_param.obj_stream    = pdfStream
                            AND TT_pdf_param.obj_parameter = "PageWidth"
                            NO-LOCK NO-ERROR.
  IF AVAIL TT_pdf_param THEN
    RETURN INT(TT_pdf_param.obj_value).
  ELSE
    RETURN 612.

END FUNCTION. /* pdf_PageWidth */

FUNCTION pdf_Pageheight RETURN INTEGER ( INPUT pdfStream AS CHARACTER):

  IF NOT CAN-FIND(FIRST TT_pdf_stream
                  WHERE TT_pdf_stream.obj_stream = pdfStream NO-LOCK)
  THEN DO:
    RUN pdf_error(pdfStream,"pdf_PageHeight","Cannot find Stream!").
    RETURN ERROR.
  END.

  FIND FIRST TT_pdf_param WHERE TT_pdf_param.obj_stream    = pdfStream
                            AND TT_pdf_param.obj_parameter = "PageHeight"
                            NO-LOCK NO-ERROR.
  IF AVAIL TT_pdf_param THEN
    RETURN INT(TT_pdf_param.obj_value).
  ELSE
    RETURN 792.
END FUNCTION. /* pdf_PageHeight */

PROCEDURE pdf_set_PageWidth :
  DEFINE INPUT PARAMETER pdfStream          AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfValue           AS INTEGER NO-UNDO.

  IF NOT CAN-FIND(FIRST TT_pdf_stream
                  WHERE TT_pdf_stream.obj_stream = pdfStream NO-LOCK)
  THEN DO:
    RUN pdf_error(pdfStream,"pdf_set_PageWidth","Cannot find Stream!").
    RETURN .
  END.

  IF pdfValue = 0 THEN DO:
    RUN pdf_error(pdfStream,"pdf_set_PageWidth","Page Width cannot be zero!").
    RETURN.
  END.

  FIND FIRST TT_pdf_param
       WHERE TT_pdf_param.obj_stream    = pdfStream
         AND TT_pdf_param.obj_parameter = "PageWidth" EXCLUSIVE-LOCK NO-ERROR.
  IF NOT AVAIL TT_pdf_param THEN DO:
    CREATE TT_pdf_param.
    ASSIGN TT_pdf_param.obj_stream    = pdfStream
           TT_pdf_param.obj_parameter = "PageWidth".
  END.

  TT_pdf_param.obj_value = STRING(pdfValue).

END. /* pdf_set_PageWidth */

PROCEDURE pdf_set_PageHeight :
  DEFINE INPUT PARAMETER pdfStream          AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfValue           AS INTEGER NO-UNDO.

  IF NOT CAN-FIND(FIRST TT_pdf_stream
                  WHERE TT_pdf_stream.obj_stream = pdfStream NO-LOCK)
  THEN DO:
    RUN pdf_error(pdfStream,"pdf_set_PageHeight","Cannot find Stream!").
    RETURN .
  END.

  IF pdfValue = 0 THEN DO:
    RUN pdf_error(pdfStream,"pdf_set_PageHeight","Page Height cannot be zero!").
    RETURN .
  END.

  FIND FIRST TT_pdf_param
       WHERE TT_pdf_param.obj_stream    = pdfStream
         AND TT_pdf_param.obj_parameter = "PageHeight" EXCLUSIVE-LOCK NO-ERROR.
  IF NOT AVAIL TT_pdf_param THEN DO:
    CREATE TT_pdf_param.
    ASSIGN TT_pdf_param.obj_stream    = pdfStream
           TT_pdf_param.obj_parameter = "PageHeight".
  END.

  TT_pdf_param.obj_value = STRING(pdfValue).

END. /* pdf_set_PageHeight */


PROCEDURE pdf_set_Page :
  DEFINE INPUT PARAMETER pdfStream          AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfValue           AS INTEGER NO-UNDO.

  DEFINE BUFFER B_TT_pdf_Stream FOR TT_pdf_Stream.

  FIND FIRST B_TT_pdf_stream
       WHERE B_TT_pdf_stream.obj_stream = pdfStream NO-ERROR.
  IF NOT AVAIL B_TT_pdf_Stream THEN DO:
    RUN pdf_error(pdfStream,"pdf_set_page","Cannot find Stream!").
    RETURN.
  END.

  IF pdfValue = 0 THEN DO:
    RUN pdf_error(pdfStream,"pdf_set_page","Value passed cannot be zero!").
    RETURN.
  END.

  FIND FIRST TT_pdf_param
       WHERE TT_pdf_param.obj_stream    = pdfStream
         AND TT_pdf_param.obj_parameter = "Page" EXCLUSIVE-LOCK NO-ERROR.
  IF NOT AVAIL TT_pdf_param THEN DO:
    CREATE TT_pdf_param.
    ASSIGN TT_pdf_param.obj_stream    = pdfStream
           TT_pdf_param.obj_parameter = "Page".
  END.

  TT_pdf_param.obj_value = STRING(pdfValue).

  IF B_TT_pdf_Stream.obj_DoingText THEN
    RUN OutputTextContent(pdfStream, 
                         "TEXT",
                         "ET",
                          "",
                          "").
  ELSE IF B_TT_pdf_Stream.obj_DoingGraphic THEN
    RUN OutputTextContent(pdfStream, 
                         "GRAPHIC",
                         "Q",
                          "",
                          "").

  OUTPUT STREAM S_pdf_out CLOSE.

  OUTPUT STREAM S_pdf_out TO 
         VALUE(  SESSION:TEMP-DIR 
               + B_TT_pdf_stream.obj_UniqueID  
               + "-Content-" 
               + STRING(pdfValue) + ".txt") 
               BINARY NO-MAP NO-CONVERT APPEND.

  ASSIGN B_TT_pdf_Stream.obj_DoingText = FALSE
         B_TT_pdf_Stream.obj_DoingGraphic = FALSE.

  /* Note: the placement of the following commands is important due to the
      setting of the X and Y attributes.  DO NOT CHANGE unless tested
      thoroughly */
  RUN pdf_set_LeftMargin(pdfStream, pdf_LeftMargin(pdfStream)).
  RUN pdf_set_TopMargin(pdfStream, pdf_TopMargin(pdfStream)).
  RUN pdf_set_BottomMargin(pdfStream, pdf_BottomMargin(pdfStream)).

  CASE pdf_PageRotate(pdfStream):
    WHEN 0 OR WHEN 180 THEN DO:
      RUN pdf_set_TextX(pdfStream, pdf_LeftMargin(pdfStream)).
      RUN pdf_set_Angle(pdfStream,0).
      RUN pdf_set_TextY(pdfStream, pdf_PageHeight(pdfStream) - pdf_TopMargin(pdfStream) ).
    END.

    WHEN 90 OR WHEN 270 THEN DO:
      RUN pdf_set_TextX(pdfStream, pdf_LeftMargin(pdfStream)).
      RUN pdf_set_Angle(pdfStream,0).
      RUN pdf_set_TextY(pdfStream, pdf_PageWidth(pdfStream) - pdf_TopMargin(pdfStream) ).
    END.

  END CASE.

  RUN pdf_set_GraphicX(pdfStream, pdf_LeftMargin(pdfStream)).
  RUN pdf_set_GraphicY(pdfStream, pdf_PageHeight(pdfStream) - pdf_TopMargin(pdfStream)).

END. /* pdf_set_Page */

PROCEDURE pdf_set_PageRotate :
  DEFINE INPUT PARAMETER pdfStream          AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfValue           AS INTEGER NO-UNDO.

  DEFINE VARIABLE l_PageHeight  AS INTEGER NO-UNDO.
  DEFINE VARIABLE l_PageWidth   AS INTEGER NO-UNDO.

  IF NOT CAN-FIND(FIRST TT_pdf_stream
                  WHERE TT_pdf_stream.obj_stream = pdfStream NO-LOCK)
  THEN DO:
    RUN pdf_error(pdfStream,"pdf_set_PageRotate","Cannot find Stream!").
    RETURN .
  END.

  IF  pdfValue <> 0
  AND pdfValue <> 90
  AND pdfValue <> 180
  AND pdfValue <> 270 THEN DO:
    RUN pdf_error(pdfStream,"pdf_set_PageRotate","Page Rotation value must be 0, 90, 180, or 270!").
    RETURN.
  END.

  FIND FIRST TT_pdf_param
       WHERE TT_pdf_param.obj_stream    = pdfStream
         AND TT_pdf_param.obj_parameter = "PageRotate" EXCLUSIVE-LOCK NO-ERROR.
  IF NOT AVAIL TT_pdf_param THEN DO:
    CREATE TT_pdf_param.
    ASSIGN TT_pdf_param.obj_stream    = pdfStream
           TT_pdf_param.obj_parameter = "PageRotate".
  END.

  TT_pdf_param.obj_value = STRING(pdfValue).

END. /* pdf_set_PageRotate */

FUNCTION pdf_Angle RETURN INTEGER ( INPUT pdfStream AS CHARACTER):

  IF NOT CAN-FIND(FIRST TT_pdf_stream
                  WHERE TT_pdf_stream.obj_stream = pdfStream NO-LOCK)
  THEN DO:
    RUN pdf_error(pdfStream,"pdf_Angle","Cannot find Stream!").
    RETURN ERROR.
  END.

  FIND FIRST TT_pdf_param WHERE TT_pdf_param.obj_stream    = pdfStream
                            AND TT_pdf_param.obj_parameter = "Angle"
                            NO-LOCK NO-ERROR.
  IF AVAIL TT_pdf_param THEN RETURN INT(TT_pdf_param.obj_value).
  ELSE RETURN ?.

END. /* pdf_Angle */

PROCEDURE pdf_set_TextX :
  DEFINE INPUT PARAMETER pdfStream          AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfValue           AS INTEGER NO-UNDO.

  DEFINE BUFFER L_TT_pdf_param FOR TT_pdf_param.

  IF NOT CAN-FIND(FIRST TT_pdf_stream
                  WHERE TT_pdf_stream.obj_stream = pdfStream NO-LOCK)
  THEN DO:
    RUN pdf_error(pdfStream,"pdf_set_TextX","Cannot find Stream (" + pdfStream + ")!").
    RETURN.
  END.

  IF pdfValue < 0 THEN DO:
    RUN pdf_error(pdfStream,"pdf_set_TextX","Value cannot be less than zero!").
    RETURN .
  END.

  FIND FIRST L_TT_pdf_param
       WHERE L_TT_pdf_param.obj_stream    = pdfStream
         AND L_TT_pdf_param.obj_parameter = "TextX" EXCLUSIVE-LOCK NO-ERROR.
  IF NOT AVAIL L_TT_pdf_param THEN DO:
    CREATE L_TT_pdf_param.
    ASSIGN L_TT_pdf_param.obj_stream    = pdfStream
           L_TT_pdf_param.obj_parameter = "TextX".
  END.

  L_TT_pdf_param.obj_value = STRING(pdfValue).

END. /* pdf_set_TextX */

FUNCTION pdf_Orientation RETURN CHARACTER ( INPUT pdfStream AS CHARACTER):

  IF NOT CAN-FIND(FIRST TT_pdf_stream
                  WHERE TT_pdf_stream.obj_stream = pdfStream NO-LOCK)
  THEN DO:
    RUN pdf_error(pdfStream,"pdf_Orientation","Cannot find Stream!").
    RETURN ERROR.
  END.

  FIND FIRST TT_pdf_param WHERE TT_pdf_param.obj_stream    = pdfStream
                            AND TT_pdf_param.obj_parameter = "Orientation"
                            NO-LOCK NO-ERROR.
  IF AVAIL TT_pdf_param THEN RETURN TT_pdf_param.obj_value.
  ELSE RETURN "Portrait".

END. /* pdf_Orientation */

PROCEDURE pdf_set_TextY :
  DEFINE INPUT PARAMETER pdfStream          AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfValue           AS INTEGER NO-UNDO.

  DEFINE BUFFER L_TT_pdf_param FOR TT_pdf_param.

  IF NOT CAN-FIND(FIRST TT_pdf_stream
                  WHERE TT_pdf_stream.obj_stream = pdfStream NO-LOCK)
  THEN DO:
    RUN pdf_error(pdfStream,"pdf_set_TextY","Cannot find Stream!").
    RETURN.
  END.

  IF pdf_Angle(pdfStream) = 0 AND pdfValue <= pdf_BottomMargin(pdfStream)
  AND NOT pdf_ForFooter
  THEN DO:
    RUN pdf_new_page2(pdfStream, pdf_Orientation(pdfStream)).
    pdfValue = pdf_TextY(pdfStream).
  END.

  FIND FIRST L_TT_pdf_param
       WHERE L_TT_pdf_param.obj_stream    = pdfStream
         AND L_TT_pdf_param.obj_parameter = "TextY" EXCLUSIVE-LOCK NO-ERROR.
  IF NOT AVAIL L_TT_pdf_param THEN DO:
    CREATE L_TT_pdf_param.
    ASSIGN L_TT_pdf_param.obj_stream    = pdfStream
           L_TT_pdf_param.obj_parameter = "TextY".
  END.

  CASE pdf_Angle(pdfStream):
    WHEN 90 THEN
      L_TT_pdf_param.obj_value = STRING(pdf_textY(pdfStream) - pdfValue - pdf_PointSize(pdfStream)).
    WHEN 270 THEN
      L_TT_pdf_param.obj_value = STRING(pdfValue).
    OTHERWISE
      L_TT_pdf_param.obj_value = STRING(pdfValue).
  END CASE.

END. /* pdf_set_TextY */


PROCEDURE pdf_set_GraphicX :
  DEFINE INPUT PARAMETER pdfStream          AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfValue           AS DECIMAL DECIMALS 4 NO-UNDO.

  IF NOT CAN-FIND(FIRST TT_pdf_stream
                  WHERE TT_pdf_stream.obj_stream = pdfStream NO-LOCK)
  THEN DO:
    RUN pdf_error(pdfStream,"pdf_set_GraphicX","Cannot find Stream!").
    RETURN.
  END.

  IF pdfValue < 0 THEN DO:
    RUN pdf_error(pdfStream,"pdf_set_GraphicX","Value cannot be less than 0!").
    RETURN.
  END.

  FIND FIRST TT_pdf_param
       WHERE TT_pdf_param.obj_stream    = pdfStream
         AND TT_pdf_param.obj_parameter = "GraphicX" EXCLUSIVE-LOCK NO-ERROR.
  IF NOT AVAIL TT_pdf_param THEN DO:
    CREATE TT_pdf_param.
    ASSIGN TT_pdf_param.obj_stream    = pdfStream
           TT_pdf_param.obj_parameter = "GraphicX".
  END.

  TT_pdf_param.obj_value = STRING(pdfValue).

END. /* pdf_set_GraphicX */

PROCEDURE pdf_set_GraphicY :
  DEFINE INPUT PARAMETER pdfStream          AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfValue           AS DECIMAL DECIMALS 4 NO-UNDO.

  IF NOT CAN-FIND(FIRST TT_pdf_stream
                  WHERE TT_pdf_stream.obj_stream = pdfStream NO-LOCK)
  THEN DO:
    RUN pdf_error(pdfStream,"pdf_set_GraphicY","Cannot find Stream!").
    RETURN.
  END.

  IF pdfValue < 0 THEN DO:
    RUN pdf_error(pdfStream,"pdf_set_GraphicY","Value cannot be less than zero!").
    RETURN.
  END.

  FIND FIRST TT_pdf_param
       WHERE TT_pdf_param.obj_stream    = pdfStream
         AND TT_pdf_param.obj_parameter = "GraphicY" EXCLUSIVE-LOCK NO-ERROR.
  IF NOT AVAIL TT_pdf_param THEN DO:
    CREATE TT_pdf_param.
    ASSIGN TT_pdf_param.obj_stream    = pdfStream
           TT_pdf_param.obj_parameter = "GraphicY".
  END.

  TT_pdf_param.obj_value = STRING(pdfValue).

END. /* pdf_set_GraphicY */

FUNCTION pdf_GraphicX RETURN DECIMAL ( INPUT pdfStream AS CHARACTER):

  IF NOT CAN-FIND(FIRST TT_pdf_stream
                  WHERE TT_pdf_stream.obj_stream = pdfStream NO-LOCK)
  THEN DO:
    RUN pdf_error(pdfStream,"pdf_GraphicX","Cannot find Stream!").
    RETURN ERROR.
  END.

  FIND FIRST TT_pdf_param WHERE TT_pdf_param.obj_stream    = pdfStream
                            AND TT_pdf_param.obj_parameter = "GraphicX"
                            NO-LOCK NO-ERROR.
  IF AVAIL TT_pdf_param THEN
    RETURN DEC(TT_pdf_param.obj_value).
  ELSE
    RETURN 0.0.

END FUNCTION. /* pdf_GraphicX */

FUNCTION pdf_GraphicY RETURN DECIMAL ( INPUT pdfStream AS CHARACTER):

  IF NOT CAN-FIND(FIRST TT_pdf_stream
                  WHERE TT_pdf_stream.obj_stream = pdfStream NO-LOCK)
  THEN DO:
    RUN pdf_error(pdfStream,"pdf_GraphicY","Cannot find Stream!").
    RETURN ERROR.
  END.

  FIND FIRST TT_pdf_param WHERE TT_pdf_param.obj_stream    = pdfStream
                            AND TT_pdf_param.obj_parameter = "GraphicY"
                            NO-LOCK NO-ERROR.
  IF AVAIL TT_pdf_param THEN
    RETURN DEC(TT_pdf_param.obj_value).
  ELSE
    RETURN 0.0.

END. /* pdf_GraphicY */

PROCEDURE pdf_set_info :
  DEFINE INPUT PARAMETER pdfStream    AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfAttribute AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfvalue     AS CHARACTER NO-UNDO.

  IF NOT CAN-FIND(FIRST TT_pdf_stream
                  WHERE TT_pdf_stream.obj_stream = pdfStream NO-LOCK)
  THEN DO:
    RUN pdf_error(pdfStream,"pdf_set_info","Cannot find Stream!").
    RETURN .
  END.

  DEFINE VARIABLE L_option  AS CHARACTER NO-UNDO.
  L_Option = "Author,Creator,Producer,Keywords,Subject,Title,OutputTo".

  IF LOOKUP(pdfAttribute,L_option) = 0 THEN DO:
    RUN pdf_error(pdfStream,"pdf_set_info","Invalid Attribute passed!").
    RETURN .
  END.

  IF NOT CAN-FIND( FIRST TT_pdf_info
                   WHERE TT_pdf_info.obj_stream = pdfStream
                     AND TT_pdf_info.info_attr  = pdfAttribute NO-LOCK)
  THEN DO:
    CREATE TT_pdf_info.
    ASSIGN TT_pdf_info.obj_stream = pdfStream
           TT_pdf_info.info_attr  = pdfAttribute.
  END.

  RUN pdf_replace_text(INPUT-OUTPUT pdfValue).

  TT_pdf_info.info_value = pdfValue.

END. /* pdf_set_info */

FUNCTION pdf_get_info RETURNS CHARACTER ( INPUT pdfStream    AS CHARACTER,
                                          INPUT pdfAttribute AS CHARACTER):

  IF NOT CAN-FIND(FIRST TT_pdf_stream
                  WHERE TT_pdf_stream.obj_stream = pdfStream NO-LOCK)
  THEN DO:
    RUN pdf_error(pdfStream,"pdf_get_info","Cannot find Stream!").
    RETURN ERROR.
  END.

  DEFINE VARIABLE L_option  AS CHARACTER NO-UNDO.
  L_Option = "Author,Creator,Producer,Keywords,Subject,Title,OutputTo".

  IF LOOKUP(pdfAttribute,L_option) = 0 THEN DO:
    RUN pdf_error(pdfStream,"pdf_Get_Info","Invalid Attribute passed!").
    RETURN ERROR.
  END.

  FIND FIRST TT_pdf_info WHERE TT_pdf_info.obj_stream = pdfStream
                           AND TT_pdf_info.info_attr  = pdfAttribute
                           NO-LOCK NO-ERROR.
  IF AVAIL TT_pdf_info THEN DO:
    RETURN TT_pdf_info.info_value.
  END.

  ELSE
    RETURN "".
END FUNCTION. /* pdf_get_info */

FUNCTION pdf_get_pdf_info RETURNS CHARACTER
        (pdfSTREAM AS CHARACTER,
         pdfID     AS CHARACTER,
         pInfo     AS CHARACTER):
/* Returns information about a pdf loaded with pdf_open_pdf */
/* i.e.  numPages = integer(pdf_info("SPDF",1,"pages"))     */
/*       author   = pdf_info("SPDF",1,"author")             */

    FOR FIRST TT_info
        WHERE TT_info.pdf_id = pdfID
          AND TT_info.info_name = pInfo NO-LOCK:
      RETURN info_value.
    END.

  RETURN "".

END FUNCTION.  /* pdf_get_pdf_info */

PROCEDURE pdf_move_to :
  DEFINE INPUT PARAMETER pdfStream    AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfToX       AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER pdfToY       AS INTEGER NO-UNDO.

  IF NOT CAN-FIND(FIRST TT_pdf_stream
                  WHERE TT_pdf_stream.obj_stream = pdfStream NO-LOCK)
  THEN DO:
    RUN pdf_error(pdfStream,"pdf_move_to","Cannot find Stream!").
    RETURN .
  END.

  RUN OutputTextContent(pdfStream, 
                        "GRAPHIC",
                        STRING(pdfToX) + " " + STRING(pdfToY) + " m",
                        "",
                        "").

  RUN pdf_set_GraphicY (pdfStream,pdfToY).
  RUN pdf_set_GraphicX (pdfStream, pdfToX).

END. /* pdf_moveto */

PROCEDURE pdf_rect :
  DEFINE INPUT PARAMETER pdfStream    AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfFromX     AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER pdfFromY     AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER pdfWidth     AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER pdfHeight    AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER pdfWeight    AS DECIMAL NO-UNDO.    /* JES ADDED */

  IF NOT CAN-FIND(FIRST TT_pdf_stream
                  WHERE TT_pdf_stream.obj_stream = pdfStream NO-LOCK)
  THEN DO:
    RUN pdf_error(pdfStream,"pdf_rect","Cannot find Stream!").
    RETURN.
  END.

  RUN OutputTextContent(pdfStream, 
                        "GRAPHIC",
                        dec2string(pdfWeight)
                        + " w" + CHR(10)  /* JES ADDED */
                        + STRING(pdfFromX) + " " + STRING(pdfFromY) + " "
                        + STRING(pdfWidth) + " " + STRING(pdfHeight)
                        + " re" + CHR(10)
                        + "B",
                        "",
                        "").

  RUN pdf_set_GraphicY (pdfStream,pdfFromY + pdfHeight).
  RUN pdf_set_GraphicX (pdfStream, pdfFromX + pdfWidth).

END. /* pdf_rect */

PROCEDURE pdf_rectdec :
  DEFINE INPUT PARAMETER pdfStream    AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfFromX     AS DECIMAL DECIMALS 4 NO-UNDO.
  DEFINE INPUT PARAMETER pdfFromY     AS DECIMAL DECIMALS 4 NO-UNDO.
  DEFINE INPUT PARAMETER pdfWidth     AS DECIMAL DECIMALS 4 NO-UNDO.
  DEFINE INPUT PARAMETER pdfHeight    AS DECIMAL DECIMALS 4 NO-UNDO.
  DEFINE INPUT PARAMETER pdfWeight    AS DECIMAL NO-UNDO.    /* JES ADDED */

  IF NOT CAN-FIND(FIRST TT_pdf_stream
                  WHERE TT_pdf_stream.obj_stream = pdfStream NO-LOCK)
  THEN DO:
    RUN pdf_error(pdfStream,"pdf_rect","Cannot find Stream!").
    RETURN.
  END.

  RUN OutputTextContent(pdfStream, 
                        "GRAPHIC",
                        dec2string(pdfWeight)
                        + " w" + CHR(10)  /* JES ADDED */
                        + STRING(pdfFromX) + " " + STRING(pdfFromY) + " "
                        + STRING(pdfWidth) + " " + STRING(pdfHeight)
                        + " re" + CHR(10)
                        + "B",
                        "",
                        "").

  RUN pdf_set_GraphicY (pdfStream,pdfFromY + pdfHeight).
  RUN pdf_set_GraphicX (pdfStream, pdfFromX + pdfWidth).

END. /* pdf_rectdec */

PROCEDURE pdf_rect2 :
  DEFINE INPUT PARAMETER pdfStream    AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfFromX     AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER pdfFromY     AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER pdfWidth     AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER pdfHeight    AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER pdfWeight    AS DECIMAL NO-UNDO.    /* JES ADDED */

  IF NOT CAN-FIND(FIRST TT_pdf_stream
                  WHERE TT_pdf_stream.obj_stream = pdfStream NO-LOCK)
  THEN DO:
    RUN pdf_error(pdfStream,"pdf_rect2","Cannot find Stream!").
    RETURN.
  END.

  RUN OutputTextContent(pdfStream, 
                        "GRAPHIC",
                        dec2string(pdfWeight)
                        + " w" + CHR(10)  /* JES ADDED */
                        + STRING(pdfFromX) + " " + STRING(pdfFromY) + " "
                        + STRING(pdfWidth) + " " + STRING(pdfHeight)
                        + " re" + CHR(10)
                        + "S",
                        "",
                        "").

  RUN pdf_set_GraphicY (pdfStream,pdfFromY + pdfHeight).
  RUN pdf_set_GraphicX (pdfStream, pdfFromX + pdfWidth).

END. /* pdf_rect2 */

PROCEDURE pdf_circle :
  /* Note:  pdfX and pdfY represent the center point of the circle.  These
            values become the new Graphic X and Y points after the drawing of
            the circle.  If you want the circle to be filled use pdf_stroke_fill
  */
  DEFINE INPUT PARAMETER pdfStream    AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfX         AS INTEGER   NO-UNDO.
  DEFINE INPUT PARAMETER pdfY         AS INTEGER   NO-UNDO.
  DEFINE INPUT PARAMETER pdfRadius    AS INTEGER   NO-UNDO.
  DEFINE INPUT PARAMETER pdfWeight     AS DECIMAL   NO-UNDO.

  DEFINE VARIABLE l_Constant    AS DECIMAL DECIMALS 10 NO-UNDO.
  DEFINE VARIABLE l_Length      AS DECIMAL DECIMALS 5 NO-UNDO.
  DEFINE VARIABLE c_NewY        AS CHARACTER NO-UNDO.
  DEFINE VARIABLE c_NewX        AS CHARACTER NO-UNDO.

  IF NOT CAN-FIND(FIRST TT_pdf_stream
                  WHERE TT_pdf_stream.obj_stream = pdfStream NO-LOCK)
  THEN DO:
    RUN pdf_error(pdfStream,"pdf_circle","Cannot find Stream!").
    RETURN.
  END.

  l_Constant = 0.5522847498. /* (4/3)*(sqrt(2)-1) - who knows where this calc came from */

  l_Length = pdfRadius * l_Constant.

  /* First Quadrant - Upper Right */
  c_NewX   = dec2string(l_Length + pdfX).
  c_NewY   = dec2string(l_Length + pdfY).
  RUN pdf_move_to(pdfStream, pdfX + pdfRadius, pdfY).

  RUN pdf_curve(pdfStream,
                pdfX + pdfRadius,
                c_newY,
                c_NewX,
                pdfY + pdfRadius,
                pdfX,
                pdfY + pdfRadius,
                pdfWeight).

  /* Second Quadrant - Upper Left */
  c_NewX   = dec2string(pdf_GraphicX(pdfstream) - l_Length).
  c_NewY   = dec2string(pdf_GraphicY(pdfstream) - pdfRadius + l_Length).
  RUN pdf_curve(pdfStream,
                c_NewX,
                pdf_GraphicY(pdfStream),
                pdfX - pdfRadius,
                c_NewY,
                pdfX - pdfRadius,
                pdf_GraphicY(pdfStream) - pdfRadius,
                pdfWeight).

  /* Third Quadrant - Lower Left */
  c_NewX   = dec2string(pdfX - l_Length).
  c_NewY   = dec2string(pdfY - l_Length).
  RUN pdf_curve(pdfStream,
                pdfX - pdfRadius,
                c_NewY,
                c_newX,
                pdfY - pdfRadius,
                pdfX,
                pdfY - pdfRadius,
                pdfWeight).

  /* Fourth Quadrant - Lower Right */
  c_NewX   = dec2string(pdfX + l_Length).
  c_NewY   = dec2string(pdfY - l_Length).
  RUN pdf_curve(pdfStream,
                c_NewX,
                pdfY - pdfRadius,
                pdfX + pdfRadius,
                c_NewY,
                pdfX + pdfRadius,
                pdfY,
                pdfWeight).

  /* Close the Path */
  RUN pdf_close_path(pdfStream).

  /* Set the current point to be the current Graphic X/Y Location */
  RUN pdf_set_GraphicY (pdfStream,pdfY).
  RUN pdf_set_GraphicX (pdfStream, pdfX).
  

END. /* pdf_circle */

PROCEDURE pdf_curve :
  /* A Bézier curve is added from the current Graphic X/Y Location to X3/Y3 
     using X1/Y1 and X2/Y2) as the control points. The X3/Y3 of the curve 
     becomes the new Graphic X/Y Location.  */
     
  DEFINE INPUT PARAMETER pdfStream    AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfX1         AS INTEGER   NO-UNDO.
  DEFINE INPUT PARAMETER pdfY1         AS INTEGER   NO-UNDO.
  DEFINE INPUT PARAMETER pdfX2         AS INTEGER   NO-UNDO.
  DEFINE INPUT PARAMETER pdfY2         AS INTEGER   NO-UNDO.
  DEFINE INPUT PARAMETER pdfX3         AS INTEGER   NO-UNDO.
  DEFINE INPUT PARAMETER pdfY3         AS INTEGER   NO-UNDO.
  DEFINE INPUT PARAMETER pdfWeight     AS DECIMAL   NO-UNDO.

  DEFINE VARIABLE l_Constant    AS DECIMAL DECIMALS 10 NO-UNDO.
  DEFINE VARIABLE l_Length      AS DECIMAL DECIMALS 5 NO-UNDO.
  DEFINE VARIABLE c_NewY        AS CHARACTER NO-UNDO.
  DEFINE VARIABLE c_NewX        AS CHARACTER NO-UNDO.

  IF NOT CAN-FIND(FIRST TT_pdf_stream
                  WHERE TT_pdf_stream.obj_stream = pdfStream NO-LOCK)
  THEN DO:
    RUN pdf_error(pdfStream,"pdf_curve","Cannot find Stream!").
    RETURN.
  END.

  IF pdf_GraphicY(pdfStream) = 0 OR pdf_GraphicX(pdfStream) = 0 THEN DO:
    RUN pdf_error(pdfStream,"pdf_curve","Graphic X/Y location has not been initialized!").
    RETURN.
  END.

  /* First Circle - Upper Right */
  c_NewX   = dec2string(l_Length + pdfX1).
  c_NewY   = dec2string(l_Length + pdfY1).

  RUN OutputTextContent(pdfStream, 
                        "GRAPHIC",
                          /* igc - rem CHR(10) + */ dec2string(pdfWeight)
                        + " w" + CHR(10)
                        + STRING(pdf_FillRed(pdfstream)) + " "
                        + STRING(pdf_FillGreen(pdfstream)) + " "
                        + STRING(pdf_FillBlue(pdfstream)) + " rg " + CHR(10)
                        + STRING(pdfX1) + " " + STRING(pdfY1)
                        + " " + STRING(pdfX2) + " " + STRING(pdfY2) + " "
                        + STRING(pdfX3) + " " + STRING(pdfY3) + " c",
                        "",
                        "").

    /* Set the new Graphic Points */
  RUN pdf_set_GraphicY (pdfStream, pdfY3).
  RUN pdf_set_GraphicX (pdfStream, pdfX3).
  
END. /* pdf_curve */

PROCEDURE pdf_close_path:
  DEFINE INPUT PARAMETER pdfStream  AS CHARACTER NO-UNDO.

  RUN OutputTextContent(pdfStream, 
                        "GRAPHIC",
                        /* igc - rem CHR(10) + */ "B",
                        "",
                        "").
END.

PROCEDURE pdf_stroke_color :
  DEFINE INPUT PARAMETER pdfStream    AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfRed       AS DECIMAL NO-UNDO.
  DEFINE INPUT PARAMETER pdfGreen     AS DECIMAL NO-UNDO.
  DEFINE INPUT PARAMETER pdfBlue      AS DECIMAL NO-UNDO.

  IF NOT CAN-FIND(FIRST TT_pdf_stream
                  WHERE TT_pdf_stream.obj_stream = pdfStream NO-LOCK)
  THEN DO:
    RUN pdf_error(pdfStream,"pdf_stroke_color","Cannot find Stream!").
    RETURN .
  END.

  pdfRed   = IF pdfRed < 0 THEN 0
             ELSE IF pdfRed > 1 THEN 1
             ELSE pdfRed.
  pdfGreen = IF pdfGreen < 0 THEN 0
             ELSE IF pdfGreen > 1 THEN 1
             ELSE pdfGreen.
  pdfBlue  = IF pdfBlue < 0 THEN 0
             ELSE IF pdfBlue > 1 THEN 1
             ELSE pdfBlue.

  RUN OutputTextContent(pdfStream, 
                        "GRAPHIC",
                          " " + dec2string(pdfRed)
                        + " " + dec2string(pdfGreen)
                        + " " + dec2string(pdfBlue) + " RG ",
                        "",
                        "").

END. /* pdf_stroke_color */

PROCEDURE pdf_stroke_fill :
  DEFINE INPUT PARAMETER pdfStream    AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfRed       AS DECIMAL NO-UNDO.
  DEFINE INPUT PARAMETER pdfGreen     AS DECIMAL NO-UNDO.
  DEFINE INPUT PARAMETER pdfBlue      AS DECIMAL NO-UNDO.

  IF NOT CAN-FIND(FIRST TT_pdf_stream
                  WHERE TT_pdf_stream.obj_stream = pdfStream NO-LOCK)
  THEN DO:
    RUN pdf_error(pdfStream,"pdf_stroke_fill","Cannot find Stream!").
    RETURN .
  END.

  pdfRed   = IF pdfRed < 0 THEN 0
             ELSE IF pdfRed > 1 THEN 1
             ELSE pdfRed.
  pdfGreen = IF pdfGreen < 0 THEN 0
             ELSE IF pdfGreen > 1 THEN 1
             ELSE pdfGreen.
  pdfBlue  = IF pdfBlue < 0 THEN 0
             ELSE IF pdfBlue > 1 THEN 1
             ELSE pdfBlue.

  RUN OutputTextContent(pdfStream, 
                        "GRAPHIC",
                          " " + dec2string(pdfRed)
                        + " " + dec2string(pdfGreen)
                        + " " + dec2string(pdfBlue) + " rg ",
                        "",
                        "").

  RUN pdf_set_FillRed(pdfStream, pdfRed).
  RUN pdf_set_FillGreen(pdfStream, pdfGreen).
  RUN pdf_set_FillBlue(pdfStream, pdfBlue).

END. /* pdf_stroke_fill */

PROCEDURE pdf_set_dash :
  DEFINE INPUT PARAMETER pdfStream  AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfOn      AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER pdfOff     AS INTEGER NO-UNDO.

  IF pdfOn  < 0 THEN pdfOn = 1.
  IF pdfOff < 0 THEN pdfOff = 1.

  IF NOT CAN-FIND(FIRST TT_pdf_stream
                  WHERE TT_pdf_stream.obj_stream = pdfStream NO-LOCK)
  THEN DO:
    RUN pdf_error(pdfStream,"pdf_set_dash","Cannot find Stream!").
    RETURN .
  END.

  RUN OutputTextContent(pdfStream, 
                        "GRAPHIC",
                          " [" + STRING(pdfOn) + " " + STRING(pdfOff)
                        + "] 0  d ",
                        "",
                        "").

END. /* pdf_set_dash */

PROCEDURE pdf_set_linejoin :
  /* Note:  This procedure allows you to define the Line Join Styles.  This will
            typically be used when drawing a Rectangle. Possible values are:

              0 - Miter Join
              1 - Round Join
              2 - Bevel Join
  */
  DEFINE INPUT PARAMETER pdfStream  AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfJoin    AS INTEGER NO-UNDO.

  IF pdfJoin  < 0 OR pdfJoin > 2 THEN pdfJoin = 0.

  IF NOT CAN-FIND(FIRST TT_pdf_stream
                  WHERE TT_pdf_stream.obj_stream = pdfStream NO-LOCK)
  THEN DO:
    RUN pdf_error(pdfStream,"pdf_set_linejoin","Cannot find Stream!").
    RETURN .
  END.

  RUN OutputTextContent(pdfStream, 
                        "GRAPHIC",
                        STRING(pdfJoin) + " j",
                        "",
                        "").

END. /* pdf_set_linejoin */

PROCEDURE pdf_line :
  DEFINE INPUT PARAMETER pdfStream    AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfFromX     AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER pdfFromY     AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER pdfToX       AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER pdfToY       AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER pdfWeight    AS DECIMAL NO-UNDO.

  IF NOT CAN-FIND(FIRST TT_pdf_stream
                  WHERE TT_pdf_stream.obj_stream = pdfStream NO-LOCK)
  THEN DO:
    RUN pdf_error(pdfStream,"pdf_line","Cannot find Stream!").
    RETURN.
  END.

  DEFINE VARIABLE L_FromY   AS INTEGER NO-UNDO.
  DEFINE VARIABLE L_ToY     AS INTEGER NO-UNDO.

  ASSIGN L_FromY =  (pdfFromY)
         L_ToY   =  (pdfToY ).

  RUN OutputTextContent(pdfStream, 
                        "GRAPHIC",
                          dec2string(pdfWeight) + " w" + CHR(10)
                        + STRING(pdfFromX) + " " + STRING(L_FromY) + " m" + CHR(10)
                        + STRING(pdfToX) + " " + STRING(L_ToY) + " l" + CHR(10)
                        + "S",
                        "",
                        "").

  RUN pdf_set_GraphicX(pdfStream, pdfToX ).
  RUN pdf_set_GraphicY(pdfstream, pdfToY).

END. /* pdf_line */

PROCEDURE pdf_line_dec :
  DEFINE INPUT PARAMETER pdfStream    AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfFromX     AS DECIMAL DECIMALS 4 NO-UNDO.
  DEFINE INPUT PARAMETER pdfFromY     AS DECIMAL DECIMALS 4 NO-UNDO.
  DEFINE INPUT PARAMETER pdfToX       AS DECIMAL DECIMALS 4 NO-UNDO.
  DEFINE INPUT PARAMETER pdfToY       AS DECIMAL DECIMALS 4 NO-UNDO.
  DEFINE INPUT PARAMETER pdfWeight    AS DECIMAL NO-UNDO.

  IF NOT CAN-FIND(FIRST TT_pdf_stream
                  WHERE TT_pdf_stream.obj_stream = pdfStream NO-LOCK)
  THEN DO:
    RUN pdf_error(pdfStream,"pdf_line","Cannot find Stream!").
    RETURN.
  END.

  DEFINE VARIABLE L_FromY   AS INTEGER NO-UNDO.
  DEFINE VARIABLE L_ToY     AS INTEGER NO-UNDO.

  ASSIGN L_FromY =  (pdfFromY)
         L_ToY   =  (pdfToY ) .

  RUN OutputTextContent(pdfStream, 
                        "GRAPHIC",
                          dec2string(pdfWeight) + " w" + CHR(10)
                        + dec2string(pdfFromX) + " " + STRING(L_FromY) + " m" + CHR(10)
                        + dec2string(pdfToX) + " " + STRING(L_ToY) + " l" + CHR(10)
                        + "S",
                        "",
                        "").

  RUN pdf_set_GraphicX(pdfStream, pdfToX ).
  RUN pdf_set_GraphicY(pdfstream, pdfToY).

END. /* pdf_line_dec */

PROCEDURE pdf_watermark:
  DEFINE INPUT PARAMETER pdfStream   AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfText     AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfFont     AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfSize     AS INTEGER   NO-UNDO.
  DEFINE INPUT PARAMETER pdfRed      AS DECIMAL   NO-UNDO.
  DEFINE INPUT PARAMETER pdfGreen    AS DECIMAL   NO-UNDO.
  DEFINE INPUT PARAMETER pdfBlue     AS DECIMAL   NO-UNDO.
  DEFINE INPUT PARAMETER pdfX        AS INTEGER   NO-UNDO.
  DEFINE INPUT PARAMETER pdfY        AS INTEGER   NO-UNDO.

  DEFINE VARIABLE L_Font      AS CHARACTER NO-UNDO.
  DEFINE VARIABLE L_PointSize AS CHARACTER NO-UNDO.

  IF NOT CAN-FIND(FIRST TT_pdf_stream
                  WHERE TT_pdf_stream.obj_stream = pdfStream NO-LOCK)
  THEN DO:
    RUN pdf_error(pdfStream,"pdf_watermark","Cannot find Stream!").
    RETURN .
  END.

  RUN pdf_replace_text (INPUT-OUTPUT pdfText).

  pdfRed   = IF pdfRed < 0 THEN 0
             ELSE IF pdfRed > 1 THEN 1
             ELSE pdfRed.
  pdfGreen = IF pdfGreen < 0 THEN 0
             ELSE IF pdfGreen > 1 THEN 1
             ELSE pdfGreen.
  pdfBlue  = IF pdfBlue < 0 THEN 0
             ELSE IF pdfBlue > 1 THEN 1
             ELSE pdfBlue.

  FIND FIRST TT_pdf_font WHERE TT_pdf_font.obj_stream = pdfStream
                           AND TT_pdf_font.font_name  = pdfFont
                           NO-LOCK NO-ERROR.
  IF NOT AVAIL TT_pdf_font THEN DO:
    RUN pdf_error(pdfStream,"pdf_watermark","Font has not been loaded!").
    RETURN .
  END.

  RUN OutputTextContent(pdfStream, 
                        "WATERMARK",
                          "BT" + CHR(10)
                        + STRING(TT_pdf_font.font_tag) + " " + STRING(pdfSize) +  " Tf" + CHR(10)
                        + "1 0 0 1 " + STRING(pdfX) +  " " 
                                     + STRING(pdfY) + " Tm" + CHR(10)
                        + " " + dec2string(pdfRed) + " "
                        + dec2string(pdfGreen) + " "
                        + dec2string(pdfBlue) + " rg " + CHR(10)
                        + "T* (",
                        pdfText,
                        ") Tj" + CHR(10) + "ET").

END. /* pdf_watermark */

PROCEDURE pdf_text :
  DEFINE INPUT PARAMETER pdfStream   AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfText     AS CHARACTER NO-UNDO.

  DEFINE VARIABLE L_Width       AS INTEGER NO-UNDO.

  IF NOT CAN-FIND(FIRST TT_pdf_stream
                  WHERE TT_pdf_stream.obj_stream = pdfStream NO-LOCK)
  THEN DO:
    RUN pdf_error(pdfStream,"pdf_text","Cannot find Stream!").
    RETURN .
  END.

  RUN pdf_replace_text (INPUT-OUTPUT pdfText).

  RUN OutputTextContent(pdfStream, 
                        "TEXT",
                        "(",
                        pdfText,
                        ") Tj").

  L_Width = pdf_text_width (pdfStream, pdfText).

  CASE pdf_Angle(pdfStream):
    WHEN 0 THEN
      RUN pdf_set_TextX( pdfStream, pdf_TextX(pdfStream) + L_width ).
    WHEN 180 THEN
      RUN pdf_set_TextX( pdfStream, pdf_TextX(pdfStream) - L_width ).
    WHEN 90 THEN
      RUN pdf_set_TextY( pdfStream, L_width ).
    WHEN 270 THEN
      RUN pdf_set_TextY( pdfStream, pdf_TextY(pdfStream) + L_width ).
  END CASE.

END. /* pdf_text */

PROCEDURE pdf_text_char :
  DEFINE INPUT PARAMETER pdfStream   AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfValue    AS INTEGER NO-UNDO.

  DEFINE VARIABLE L_Width       AS INTEGER NO-UNDO.

  IF NOT CAN-FIND(FIRST TT_pdf_stream
                  WHERE TT_pdf_stream.obj_stream = pdfStream NO-LOCK)
  THEN DO:
    RUN pdf_error(pdfStream,"pdf_text_char","Cannot find Stream!").
    RETURN .
  END.

  IF pdfValue <= 0 OR pdfValue > 377 THEN DO:
    RUN pdf_error(pdfStream,"pdf_text_char","Value must be >= 1 and <= 377!").
    RETURN .
  END.

  RUN OutputTextContent(pdfStream, 
                        "TEXT",
                        "(",
                        "~\" + STRING(pdfValue),
                        ") Tj").

  /* Increase by 1 Character Length */
  L_Width = pdf_text_width (pdfStream, "1").

  CASE pdf_Angle(pdfStream):
    WHEN 0 THEN
      RUN pdf_set_TextX( pdfStream, pdf_TextX(pdfStream) + L_width ).
    WHEN 180 THEN
      RUN pdf_set_TextX( pdfStream, pdf_TextX(pdfStream) - L_width ).
    WHEN 90 OR WHEN 270 THEN
      RUN pdf_set_TextY( pdfStream, L_width ).
  END CASE.

END. /* pdf_text_char */

PROCEDURE pdf_text_charxy :
  DEFINE INPUT PARAMETER pdfStream   AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfText     AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfColumn   AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER pdfRow      AS INTEGER NO-UNDO.

  IF NOT CAN-FIND(FIRST TT_pdf_stream
                  WHERE TT_pdf_stream.obj_stream = pdfStream NO-LOCK)
  THEN DO:
    RUN pdf_error(pdfStream,"pdf_text_charxy","Cannot find Stream!").
    RETURN .
  END.

  DEFINE VARIABLE L_color   AS CHARACTER NO-UNDO.
  L_color = dec2string(pdf_TextRed(pdfStream)) + " "
          + dec2string(pdf_TextGreen(pdfStream)) + " "
          + dec2string(pdf_TextBlue(pdfStream)) + " rg" + CHR(10).

  DEFINE VARIABLE L_PointSize   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE L_LeftMargin  AS CHARACTER NO-UNDO.
  L_PointSize  = dec2string(pdf_PointSize(pdfStream)).
  L_LeftMargin = STRING(pdf_LeftMargin(pdfStream)).

  DEFINE VARIABLE L_Font        AS CHARACTER NO-UNDO.
  L_font = pdf_Font(pdfStream).

  FIND FIRST TT_pdf_font
       WHERE TT_pdf_font.obj_stream = pdfStream
         AND TT_pdf_font.font_name  = L_Font NO-LOCK NO-ERROR.
  L_Font = IF AVAIL TT_pdf_font THEN TT_pdf_Font.font_tag ELSE "/BF1".

  IF pdfColumn = 0 THEN DO:
    RUN pdf_error(pdfStream,"pdf_text_charxy","Column cannot be zero!").
    RETURN .
  END.

  IF pdfRow    = 0 THEN DO:
    RUN pdf_error(pdfStream,"pdf_text_charxy","Row cannot be zero!").
    RETURN .
  END.

  RUN OutputTextContent(pdfStream, 
                        "TEXTXY",
                          /* igc - rem CHR(10) + */ L_Font + " " + L_PointSize + " Tf" + CHR(10)
                        + L_color
                        + pdf_get_parameter(pdfStream,"ScaleX") 
                        + " 0 0 " 
                        + pdf_get_parameter(pdfStream,"ScaleY")
                        + " " + STRING(pdfColumn) + " " 
                        + STRING(pdfRow) + " Tm" + CHR(10)
                        + "(",
                        "~\" + STRING(pdfText),
                        ") Tj").

END. /* pdf_text_charxy */

PROCEDURE pdf_text_rotate :
  DEFINE INPUT PARAMETER pdfStream   AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfAngle    AS INTEGER NO-UNDO.

  DEFINE VARIABLE L_Rotate           AS CHARACTER NO-UNDO.

  IF NOT CAN-FIND(FIRST TT_pdf_stream
                  WHERE TT_pdf_stream.obj_stream = pdfStream NO-LOCK)
  THEN DO:
    RUN pdf_error(pdfStream,"pdf_text_rotate","Cannot find Stream!").
    RETURN .
  END.

  CASE pdfAngle:
    WHEN 0 THEN
      L_Rotate = "1 0 0 1 ".
    WHEN 45 THEN
      L_Rotate = "1 -1 1 0 ".
    WHEN 90 THEN
      L_Rotate = "0 -1 1 0 ".
    WHEN 135 THEN
      L_Rotate = "-1 -1 1 0 ".
    WHEN 180 THEN
      L_Rotate = "-1 0 0 -1 ".
    WHEN 225 THEN
      L_Rotate = "-1 1 -1 0 ".
    WHEN 270 THEN
      L_Rotate = "0 1 -1 0 ".
    WHEN 315 THEN
      L_Rotate = "1 1 -1 0 ".
  END CASE.

  RUN OutputTextContent(pdfStream, 
                        "TEXT",
                          L_Rotate + STRING(pdf_textX(pdfStream))
                        + " " + STRING(pdf_TextY(pdfStream)) + " Tm",
                        "",
                        "").

  RUN pdf_set_angle(pdfstream, pdfAngle).

END. /* pdf_text_rotate */

FUNCTION pdf_GetNumFittingChars RETURNS INTEGER
                                ( INPUT pdfStream   AS CHARACTER,
                                  INPUT pdfText     AS CHARACTER,
                                  INPUT pdfFromX    AS INTEGER,
                                  INPUT pdfToX      AS INTEGER ):

  DEFINE VARIABLE L_font    AS CHARACTER NO-UNDO.
  DEFINE VARIABLE L_tot     AS INTEGER NO-UNDO.

  DEFINE VARIABLE iReqFMWidth AS INTEGER NO-UNDO. /* Required font-metric width */
  DEFINE VARIABLE iCurFMWidth AS DECIMAL NO-UNDO. /* Font-metric width of chars that fit */
  DEFINE VARIABLE iCurChar    AS INTEGER NO-UNDO. /* Char index up to */

  L_font = pdf_Font(pdfStream).

  iReqFMWidth = pdfToX - pdfFromX.

  IF NOT CAN-FIND(FIRST TT_pdf_stream
                  WHERE TT_pdf_stream.obj_stream = pdfStream NO-LOCK)
  THEN DO:
    RUN pdf_error(pdfStream,"pdf_getnumFittingChars","Cannot find Stream!").
    RETURN ERROR.
  END.

  iCurChar = 0.
  iCurFMWidth = 0.
  FIND FIRST TT_pdf_font WHERE TT_pdf_font.obj_stream = pdfStream
                           AND TT_pdf_font.font_name  = L_Font
                           NO-LOCK NO-ERROR.
  IF AVAILABLE TT_pdf_font THEN
  DO iCurChar = 1 TO LENGTH(pdfText, "character":u):

    /* Keep looping until width is too wide, then return one less char */
    iCurFMWidth = iCurFMWidth +
                   (IF TT_pdf_Font.font_type = "FIXED" THEN
                     INTEGER(TT_pdf_font.font_width) / 1000 * pdf_PointSize(pdfStream)
                   ELSE
                     DECIMAL(INTEGER(ENTRY(ASC(SUBSTR(pdfText, iCurChar, 1, "character":u)) + 1,  /* ASCII Value of char at cur pos */
                                              TT_pdf_Font.font_width,             /* Space-seperated values list widths */
                                              " "))) / 1000 * pdf_PointSize(pdfStream)) NO-ERROR.

    IF iCurFMWidth > iReqFMWidth THEN
      RETURN iCurChar - 1.
  END. /* Loop through text */

  RETURN iCurChar.

END FUNCTION. /* pdf_GetNumFittingChars */

PROCEDURE pdf_set_font :
  DEFINE INPUT PARAMETER pdfStream   AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfFont     AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfSize     AS DECIMAL NO-UNDO.

  IF NOT CAN-FIND(FIRST TT_pdf_stream
                  WHERE TT_pdf_stream.obj_stream = pdfStream NO-LOCK)
  THEN DO:
    RUN pdf_error(pdfStream,"pdf_set_font","Cannot find Stream!").
    RETURN .
  END.

  DEFINE VARIABLE L_PointSize   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE L_LeftMargin  AS CHARACTER NO-UNDO.
  L_PointSize  = dec2string(pdf_PointSize(pdfStream)).
  L_LeftMargin = STRING(pdf_LeftMargin(pdfStream)).

  pdfFont = REPLACE(pdfFont," ","#20").

  FIND FIRST TT_pdf_font WHERE TT_pdf_font.obj_stream = pdfStream
                           AND TT_pdf_font.font_name = pdfFont
                           NO-LOCK NO-ERROR.
  IF NOT AVAIL TT_pdf_font THEN DO:
    RUN pdf_error(pdfStream,"pdf_set_font","Font (" + pdfFont + ")has not been loaded!").
    RETURN .
  END.

  RUN OutputTextContent(pdfStream, 
                        "TEXT",
                          TT_pdf_font.font_tag + " "
                        + dec2string(pdfSize) +  " Tf",
                        "",
                        "").

  /* Set the Stream Font Parameter */
  FIND FIRST TT_pdf_param
       WHERE TT_pdf_param.obj_stream    = pdfStream
         AND TT_pdf_param.obj_parameter = "Font" EXCLUSIVE-LOCK NO-ERROR.
  IF NOT AVAIL TT_pdf_param THEN DO:
    CREATE TT_pdf_param.
    ASSIGN TT_pdf_param.obj_stream    = pdfStream
           TT_pdf_param.obj_parameter = "Font".
  END.

  TT_pdf_param.obj_value = pdfFont.

  /* Set the Stream Font Size */
  FIND FIRST TT_pdf_param
       WHERE TT_pdf_param.obj_stream    = pdfStream
         AND TT_pdf_param.obj_parameter = "PointSize" EXCLUSIVE-LOCK NO-ERROR.
  IF NOT AVAIL TT_pdf_param THEN DO:
    CREATE TT_pdf_param.
    ASSIGN TT_pdf_param.obj_stream    = pdfStream
           TT_pdf_param.obj_parameter = "PointSize".
  END.
  TT_pdf_param.obj_value = dec2string(pdfSize).

END. /* pdf_set_font */

PROCEDURE pdf_text_render :
  DEFINE INPUT PARAMETER pdfStream   AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfRender   AS INTEGER NO-UNDO.

  IF NOT CAN-FIND(FIRST TT_pdf_stream
                  WHERE TT_pdf_stream.obj_stream = pdfStream NO-LOCK)
  THEN DO:
    RUN pdf_error(pdfStream,"pdf_text_render","Cannot find Stream!").
    RETURN .
  END.

  IF pdfRender < 0 OR pdfRender > 3 THEN pdfRender = 0.

  RUN OutputTextContent(pdfStream, 
                        "TEXTRENDER",
                        STRING(pdfRender) +  " Tr",
                        "",
                        "").

  FIND FIRST TT_pdf_param
       WHERE TT_pdf_param.obj_stream    = pdfStream
         AND TT_pdf_param.obj_parameter = "Render" EXCLUSIVE-LOCK NO-ERROR.
  IF NOT AVAIL TT_pdf_param THEN DO:
    CREATE TT_pdf_param.
    ASSIGN TT_pdf_param.obj_stream    = pdfStream
           TT_pdf_param.obj_parameter = "Render".
  END.

  TT_pdf_param.obj_value = STRING(pdfRender).

END. /* pdf_text_render */

PROCEDURE pdf_text_color :
  DEFINE INPUT PARAMETER pdfStream   AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfRed      AS DECIMAL NO-UNDO.
  DEFINE INPUT PARAMETER pdfGreen    AS DECIMAL NO-UNDO.
  DEFINE INPUT PARAMETER pdfBlue     AS DECIMAL NO-UNDO.

  IF NOT CAN-FIND(FIRST TT_pdf_stream
                  WHERE TT_pdf_stream.obj_stream = pdfStream NO-LOCK)
  THEN DO:
    RUN pdf_error(pdfStream,"pdf_text_color","Cannot find Stream!").
    RETURN .
  END.

  DEFINE VARIABLE L_PointSize   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE L_LeftMargin  AS CHARACTER NO-UNDO.
  L_PointSize  = dec2string(pdf_PointSize(pdfStream)).
  L_LeftMargin = STRING(pdf_LeftMargin(pdfStream)).

  RUN OutputTextContent(pdfStream, 
                        "TEXTCOLOR",
                          dec2string(pdfRed) + " "
                        + dec2string(pdfGreen) + " "
                        + dec2string(pdfBlue) + " rg ",
                        "",
                        "").

  RUN pdf_set_TextRed(pdfStream, pdfRed).
  RUN pdf_set_TextGreen(pdfStream, pdfGreen).
  RUN pdf_set_TextBlue(pdfStream, pdfBlue).

END. /* pdf_text_color */

PROCEDURE pdf_load_font :
  DEFINE INPUT PARAMETER pdfStream   AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfFontName AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfFontFile AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfFontAFM  AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfFontDIF  AS CHARACTER NO-UNDO.

  DEFINE VARIABLE L_Embed AS CHARACTER NO-UNDO.

  IF NOT CAN-FIND(FIRST TT_pdf_stream
                  WHERE TT_pdf_stream.obj_stream = pdfStream NO-LOCK)
  THEN DO:
    RUN pdf_error(pdfStream,"pdf_load_font","Cannot find Stream!").
    RETURN .
  END.

  /* Spaces in the Font Name are invalid so replace them with # plus two-digit
     hex number eg:  Spaces is #20 */
  pdfFontName = REPLACE(pdfFontName," ","#20").

  IF CAN-FIND(FIRST TT_pdf_font
              WHERE TT_pdf_font.obj_stream = pdfStream 
                AND TT_pdf_font.font_name  = pdfFontName NO-LOCK)
  THEN DO:
    RUN pdf_error(pdfStream,"pdf_load_font","Font '" + pdfFontName + "' has already been loaded!").
    RETURN .
  END.

  L_Embed = ENTRY(2,pdfFontName,"|") NO-ERROR.

  IF L_Embed <> "NOEMBED" AND SEARCH(pdfFontFile) = ? THEN DO:
    RUN pdf_error(pdfStream,"pdf_load_font","Cannot find Font File for Loading!").
    RETURN .
  END.

  IF SEARCH(pdfFontAFM) = ? THEN DO:
    RUN pdf_error(pdfStream,"pdf_Load_Font","Cannot find Font AFM file for loading!").
    RETURN .
  END.

  IF pdfFontDIF <> "" AND SEARCH(pdfFontDIF) = ? THEN DO:
    RUN pdf_error(pdfStream,"pdf_Load_Font","Cannot find Font DIF file for loading!").
    RETURN .
  END.

  CREATE TT_pdf_font.
  ASSIGN TT_pdf_font.obj_stream  = pdfStream
         TT_pdf_font.font_name   = ENTRY(1,pdfFontName,"|")
         TT_pdf_font.font_file   = pdfFontFile
         TT_pdf_font.font_afm    = pdfFontAFM
         TT_pdf_font.font_dif    = pdfFontDIF.
  TT_pdf_font.font_tag    = "/" + STRING(TT_pdf_font.font_name).
  
  IF L_Embed = "NOEMBED" THEN
    TT_pdf_font.font_embed = FALSE.

  RUN pdf_ParseAFMFile
      (OUTPUT TT_pdf_font.afm_ItalicAngle,
       OUTPUT TT_pdf_font.afm_Ascender,
       OUTPUT TT_pdf_font.afm_Descender,
       OUTPUT TT_pdf_font.afm_FontBBox,
       OUTPUT TT_pdf_font.afm_FirstChar,
       OUTPUT TT_pdf_font.afm_LastChar,
       OUTPUT TT_pdf_font.afm_Widths,
       OUTPUT TT_pdf_font.afm_IsFixedPitch,
       OUTPUT TT_pdf_font.afm_Flags) NO-ERROR.

   TT_pdf_font.font_type  = IF TT_pdf_font.afm_IsFixedPitch = "0" THEN "FIXED"
                            ELSE "VARIABLE".

    /* igc - Added March 29, 2003
           - need to assign l_afm_width to TT_pdf_font.font_width so that we
             can find the actual width when using pdf_text_width */
   IF TT_pdf_font.font_type = "VARIABLE" THEN
     TT_pdf_font.font_width = TRIM(TT_pdf_font.afm_widths).
   ELSE
     TT_pdf_font.font_width = ENTRY(1, TRIM(TT_pdf_font.afm_widths)," ").

END. /* pdf_load_font */

PROCEDURE pdf_font_diff:
  DEFINE INPUT PARAMETER pdfStream   AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfFontName AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfCharNum  AS INTEGER   NO-UNDO.
  DEFINE INPUT PARAMETER pdfPSName   AS CHARACTER NO-UNDO.

  IF NOT CAN-FIND(FIRST TT_pdf_stream
                  WHERE TT_pdf_stream.obj_stream = pdfStream NO-LOCK)
  THEN DO:
    RUN pdf_error(pdfStream,"pdf_font_diff","Cannot find Stream!").
    RETURN .
  END.

  IF NOT CAN-FIND(FIRST TT_pdf_font
                  WHERE TT_pdf_font.obj_stream = pdfStream
                    AND TT_pdf_font.font_name  = pdfFontName NO-LOCK)
  THEN DO:
    RUN pdf_error(pdfStream,"pdf_font_diff","Cannot find Font Name = " + pdfFontName).
    RETURN .
  END.

  CREATE TT_pdf_diff.
  ASSIGN TT_pdf_diff.obj_stream = pdfStream
         TT_pdf_diff.font_name  = pdfFontName
         TT_pdf_diff.char_num   = pdfCharNum
         TT_pdf_diff.PS_Name    = IF pdfPSName BEGINS "/" THEN pdfPSName
                                  ELSE "/" + pdfPSName.

END. /* pdf_font_diff */

PROCEDURE pdf_load_image :
  DEFINE INPUT PARAMETER pdfStream      AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfImageName   AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfImageFile   AS CHARACTER NO-UNDO.

  IF NOT CAN-FIND(FIRST TT_pdf_stream
                  WHERE TT_pdf_stream.obj_stream = pdfStream NO-LOCK)
  THEN DO:
    RUN pdf_error(pdfStream,"pdf_load_image","Cannot find Stream!").
    RETURN .
  END.

  IF INDEX(pdfImageName," ") > 0 THEN DO:
    RUN pdf_error(pdfStream,"pdf_load_image","Image Name cannot contain spaces!").
    RETURN .
  END.

  IF CAN-FIND(FIRST TT_pdf_image
              WHERE TT_pdf_image.obj_stream = pdfStream 
                AND TT_pdf_image.image_name = pdfImageName NO-LOCK)
  THEN DO:
    RUN pdf_error(pdfStream,"pdf_load_image","Image '" + pdfImageName + "' has already been loaded!").
    RETURN .
  END.

  IF SEARCH(pdfImageFile) = ? THEN DO:
    RUN pdf_error(pdfStream,"pdf_load_image","Cannot find Image File when Loading!").
    RETURN .
  END.

  RUN pdf_get_image_wh (INPUT pdfStream,
                        INPUT pdfImageFile).

  CREATE TT_pdf_image.
  ASSIGN TT_pdf_image.obj_stream    = pdfStream
         TT_pdf_image.image_name    = pdfImageName
         TT_pdf_image.image_file    = SEARCH(pdfImageFile)
         TT_pdf_image.image_h       = pdf_Height
         TT_pdf_image.image_w       = pdf_Width.
  TT_pdf_image.image_tag    = "/Im" + STRING(TT_pdf_image.image_name).

END. /* pdf_load_image */

PROCEDURE pdf_place_image :
  DEFINE INPUT PARAMETER pdfStream    AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfImageName AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfColumn    AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER pdfRow       AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER pdfWidth     AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER pdfHeight    AS INTEGER NO-UNDO.

  IF NOT CAN-FIND(FIRST TT_pdf_stream
                  WHERE TT_pdf_stream.obj_stream = pdfStream NO-LOCK)
  THEN DO:
    RUN pdf_error(pdfStream,"pdf_place_image","Cannot find Stream!").
    RETURN .
  END.

  DEFINE VARIABLE L_PageHeight  AS INTEGER NO-UNDO.
  L_PageHeight  = pdf_PageHeight(pdfStream).

  FIND FIRST TT_pdf_image
       WHERE TT_pdf_image.obj_stream = pdfStream
         AND TT_pdf_image.image_name = pdfImageName NO-LOCK NO-ERROR.
  IF NOT AVAIL TT_pdf_image THEN DO:
    RUN pdf_error(pdfStream,"pdf_place_image","Cannot find Image Name for Placement!").
    RETURN .
  END.

  RUN OutputTextContent(pdfStream, 
                        "IMAGE",
                          STRING(pdfWidth) + " 0 0 " + STRING(pdfHeight)
                        + " " + STRING(pdfColumn) + " "
                        + STRING(L_PageHeight - pdfRow) + " cm "
                        + TT_pdf_image.image_tag + " Do",
                        "",
                        "").

  RUN pdf_set_GraphicX(pdfStream,pdfColumn).
  RUN pdf_set_GraphicY(pdfStream,pdfRow).

END. /* pdf_place_image */

PROCEDURE pdf_place_image2 :
  DEFINE INPUT PARAMETER pdfStream    AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfImageName AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfColumn    AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER pdfRow       AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER pdfWidth     AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER pdfHeight    AS INTEGER NO-UNDO.

  IF NOT CAN-FIND(FIRST TT_pdf_stream
                  WHERE TT_pdf_stream.obj_stream = pdfStream NO-LOCK)
  THEN DO:
    RUN pdf_error(pdfStream,"pdf_place_image2","Cannot find Stream!").
    RETURN .
  END.

  DEFINE VARIABLE L_PageHeight  AS INTEGER NO-UNDO.
  L_PageHeight  = pdf_PageHeight(pdfStream).

  FIND FIRST TT_pdf_image
       WHERE TT_pdf_image.obj_stream = pdfStream
         AND TT_pdf_image.image_name = pdfImageName NO-LOCK NO-ERROR.
  IF NOT AVAIL TT_pdf_image THEN DO:
    RUN pdf_error(pdfStream,"pdf_place_image2","Cannot find Image Name for Placement!").
    RETURN .
  END.

  RUN OutputTextContent(pdfStream, 
                        "IMAGE2",
                          STRING(pdfWidth) + " 0 0 " + STRING(pdfHeight)
                        + " " + STRING(pdfColumn) + " "
                        + STRING(L_PageHeight - pdfRow) + " cm "
                        + TT_pdf_image.image_tag + " Do",
                        "",
                        "").

  RUN pdf_set_GraphicX(pdfStream,pdfColumn).
  RUN pdf_set_GraphicY(pdfStream,pdfRow).

END. /* pdf_place_image2 */

PROCEDURE pdf_skip :
  DEFINE INPUT PARAMETER pdfStream   AS CHARACTER NO-UNDO.

  DEFINE VARIABLE L_Spacer     AS DECIMAL NO-UNDO.
  DEFINE VARIABLE L_VertSpace  AS DECIMAL NO-UNDO.
  
  
  IF NOT CAN-FIND(FIRST TT_pdf_stream
                  WHERE TT_pdf_stream.obj_stream = pdfStream NO-LOCK)
  THEN DO:
    RUN pdf_error(pdfStream,"pdf_skip","Cannot find Stream!").
    RETURN .
  END.  

  L_Spacer = INT(pdf_get_parameter(pdfStream,"LineSpacer")) NO-ERROR.
  L_VertSpace = pdf_VerticalSpace(pdfstream) NO-ERROR. 
  IF L_VertSpace NE ? AND l_VertSpace NE 0 THEN 
    L_Spacer = L_VertSpace - pdf_PointSize(pdfstream). 

  RUN OutputTextContent(pdfStream, 
                        "TEXTSKIP",
                         "1 0 0 1 " + STRING(pdf_LeftMargin(pdfstream)) + " "
                                    + STRING(pdf_TextY(pdfStream)) + " Tm" + CHR(10)
                         + "0 -" + dec2string(pdf_PointSize(pdfstream) + L_Spacer)
                        + " TD",
                        "",
                        "").

  RUN pdf_set_TextX (pdfStream, pdf_LeftMargin(pdfStream)).
  RUN pdf_set_TextY (pdfStream, pdf_TextY(pdfStream) - (pdf_PointSize(pdfStream) + L_Spacer)).
END. /* pdf_skip */

PROCEDURE pdf_skipn :
  DEFINE INPUT PARAMETER pdfStream   AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfNumber   AS INTEGER NO-UNDO.

  DEFINE VARIABLE l_Ctr              AS INTEGER NO-UNDO.

  IF NOT CAN-FIND(FIRST TT_pdf_stream
                  WHERE TT_pdf_stream.obj_stream = pdfStream NO-LOCK)
  THEN DO:
    RUN pdf_error(pdfStream,"pdf_skipn","Cannot find Stream!").
    RETURN .
  END.

  IF pdfNumber <= 0 THEN DO:
    RUN pdf_error(pdfStream,"pdf_skipn","Lines to skip cannot be <= zero!").
    RETURN .
  END.

  DO l_Ctr = 1 TO pdfNumber:
    RUN pdf_skip(pdfStream).
  END.

END. /* pdf_skipn */

PROCEDURE pdf_text_xy :
  DEFINE INPUT PARAMETER pdfStream   AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfText     AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfColumn   AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER pdfRow      AS INTEGER NO-UNDO.

  IF NOT CAN-FIND(FIRST TT_pdf_stream
                  WHERE TT_pdf_stream.obj_stream = pdfStream NO-LOCK)
  THEN DO:
    RUN pdf_error(pdfStream,"pdf_text_xy","Cannot find Stream!").
    RETURN .
  END.

  DEFINE VARIABLE L_color   AS CHARACTER NO-UNDO.
  L_color = dec2string(pdf_TextRed(pdfStream)) + " "
          + dec2string(pdf_TextGreen(pdfStream)) + " "
          + dec2string(pdf_TextBlue(pdfStream)) + " rg" + CHR(10).

  DEFINE VARIABLE L_PointSize   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE L_LeftMargin  AS CHARACTER NO-UNDO.
  L_PointSize  = dec2string(pdf_PointSize(pdfStream)).
  L_LeftMargin = STRING(pdf_LeftMargin(pdfStream)).

  DEFINE VARIABLE L_Font        AS CHARACTER NO-UNDO.
  L_font = pdf_Font(pdfStream).

  FIND FIRST TT_pdf_font
       WHERE TT_pdf_font.obj_stream = pdfStream
         AND TT_pdf_font.font_name  = L_Font NO-LOCK NO-ERROR.
  L_Font = IF AVAIL TT_pdf_font THEN TT_pdf_Font.font_tag ELSE "/BF1".

  DEFINE VARIABLE L_orig_text   AS CHARACTER NO-UNDO.
  L_orig_text = pdfText.

  IF pdfColumn = 0 THEN DO:
    RUN pdf_error(pdfStream,"pdf_text_xy","Column cannot be zero!").
    RETURN .
  END.

  IF pdfRow    = 0 THEN DO:
    RUN pdf_error(pdfStream,"pdf_text_xy","Row cannot be zero!").
    RETURN .
  END.

  RUN pdf_replace_text (INPUT-OUTPUT pdfText).

  RUN OutputTextContent(pdfStream, 
                        "TEXTXY",
                        L_Font + " " + L_PointSize + " Tf" + CHR(10)
                        + L_color
                        + pdf_get_parameter(pdfStream,"ScaleX") 
                        + " 0 0 " 
                        + pdf_get_parameter(pdfStream,"ScaleY")
                        + " " + STRING(pdfColumn) + " "
                        + STRING(pdfRow) + " Tm" + CHR(10)
                        + "(",
                        pdfText,
                        ") Tj").

END. /* pdf_text_xy */

PROCEDURE pdf_text_xy_dec :
  DEFINE INPUT PARAMETER pdfStream   AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfText     AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfColumn   AS DECIMAL DECIMALS 4 NO-UNDO.
  DEFINE INPUT PARAMETER pdfRow      AS DECIMAL DECIMALS 4 NO-UNDO.

  IF NOT CAN-FIND(FIRST TT_pdf_stream
                  WHERE TT_pdf_stream.obj_stream = pdfStream NO-LOCK)
  THEN DO:
    RUN pdf_error(pdfStream,"pdf_text_xy_dec","Cannot find Stream!").
    RETURN .
  END.

  DEFINE VARIABLE L_orig_text   AS CHARACTER NO-UNDO.
  L_orig_text = pdfText.

  IF pdfColumn = 0 THEN DO:
    RUN pdf_error(pdfStream,"pdf_text_xy_dec","Column cannot be zero!").
    RETURN .
  END.

  IF pdfRow    = 0 THEN DO:
    RUN pdf_error(pdfStream,"pdf_text_xy_dec","Row cannot be zero!").
    RETURN .
  END.

  RUN pdf_replace_text (INPUT-OUTPUT pdfText). 

  RUN OutputTextContent(pdfStream, 
                        "TEXTXY",
                          
                        /* igc - rem CHR(10) 
                        + */ /* L_Font + " " + L_PointSize + " Tf" + CHR(10)
                        + L_color 
                        + */ 
                        
                        pdf_get_parameter(pdfStream,"ScaleX") 
                        + " 0 0 " 
                        + pdf_get_parameter(pdfStream,"ScaleY")
                        + " " + dec2STRING(pdfColumn) + " "
                        + dec2STRING(pdfRow) + " Tm" + CHR(10)
                        + "(",
                        pdfText,
                        ") Tj").

END. /* pdf_text_xy_dec */

PROCEDURE pdf_text_boxed_xy :
  DEFINE INPUT PARAMETER pdfStream   AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfText     AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfColumn   AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER pdfRow      AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER pdfWidth    AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER pdfHeight   AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER pdfJustify  AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfWeight   AS INTEGER NO-UNDO.

  IF NOT CAN-FIND(FIRST TT_pdf_stream
                  WHERE TT_pdf_stream.obj_stream = pdfStream NO-LOCK)
  THEN DO:
    RUN pdf_error(pdfStream,"pdf_text_boxed_xy","Cannot find Stream!").
    RETURN .
  END.

  DEFINE VARIABLE L_color   AS CHARACTER NO-UNDO.
  L_color = dec2string(pdf_TextRed(pdfStream)) + " "
          + dec2string(pdf_TextGreen(pdfStream)) + " "
          + dec2string(pdf_TextBlue(pdfStream)) + " rg" + CHR(10).

  DEFINE VARIABLE L_PointSize   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE L_LeftMargin  AS CHARACTER NO-UNDO.
  L_PointSize  = dec2string(pdf_PointSize(pdfStream)).
  L_LeftMargin = STRING(pdf_LeftMargin(pdfStream)).

  DEFINE VARIABLE L_Font        AS CHARACTER NO-UNDO.
  L_Font = pdf_Font(pdfStream).
  FIND FIRST TT_pdf_font
       WHERE TT_pdf_font.obj_stream = pdfStream
         AND TT_pdf_font.font_name  = L_Font
         NO-LOCK NO-ERROR.
  L_Font = IF AVAIL TT_pdf_font THEN TT_pdf_Font.font_tag ELSE "/BF1".

  DEFINE VARIABLE L_orig_text   AS CHARACTER NO-UNDO.
  L_orig_text = pdfText.

  IF pdfColumn = 0 THEN DO:
    RUN pdf_error(pdfStream,"pdf_text_boxed_xy","Column cannot be zero!").
    RETURN .
  END.

  IF pdfRow    = 0 THEN DO:
    RUN pdf_error(pdfStream,"pdf_text_boxed_xy","Row cannot be zero!").
    RETURN .
  END.

  IF pdfHeight = 0 THEN DO:
    RUN pdf_error(pdfStream,"pdf_text_boxed_xy","Height cannot be zero!").
    RETURN .
  END.

  IF pdfWidth  = 0 THEN DO:
    RUN pdf_error(pdfStream,"pdf_text_boxed_xy","Width cannot be zero!").
    RETURN .
  END.

  IF LOOKUP(pdfJustify,"Left,Right,Center") = 0 THEN DO:
    RUN pdf_error(pdfStream,"pdf_text_boxed_xy","Invalid Justification option passed!").
    RETURN .
  END.

  RUN pdf_replace_text (INPUT-OUTPUT pdfText).

  RUN OutputTextContent(pdfStream, 
                        "TEXTXY",
                          L_Font + " " + L_PointSize + " Tf" + CHR(10)
                        + L_color
                        + pdf_get_parameter(pdfStream,"ScaleX") 
                        + " 0 0 " 
                        + pdf_get_parameter(pdfStream,"ScaleY")
                        + " " + STRING(pdfColumn) + " "
                        + STRING(pdfRow) + " Tm" + CHR(10)
                        + "(",
                        pdfText,
                        ") Tj").

  /* IF pdfWeight = 0 THEN pdfWeight = 1. */
  IF pdfWeight gt 0 THEN DO:
    RUN OutputTextContent(pdfStream, 
                          "GRAPHIC",
                            "q" + CHR(10)
                          + STRING(pdfWeight) + " w" + CHR(10)
                          + STRING(pdfColumn) + " " + STRING(pdfRow) + " "
                          + STRING(pdfWidth) + " " + STRING(pdfHeight)
                          + " re" + CHR(10)
                          + "S" + CHR(10)
                          + "Q",
                          "",
                          "").
  END.

  RUN pdf_set_TextY(pdfStream, pdfRow).
  RUN pdf_set_GraphicY(pdfStream, pdfRow).

END. /* pdf_text_boxed_xy */

PROCEDURE pdf_text_center:
  DEFINE INPUT PARAMETER pdfStream   AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfText     AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfColumn   AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER pdfRow      AS INTEGER NO-UNDO.

  RUN pdf_text_xy (pdfStream,pdfText,pdfColumn
                   - INTEGER(pdf_text_width(pdfStream,pdfText) / 2),pdfRow).

END. /* pdf_text_center */

PROCEDURE pdf_text_at :
  DEFINE INPUT PARAMETER pdfStream   AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfText     AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfColumn   AS INTEGER NO-UNDO.

  DEFINE VARIABLE L_Found AS LOGICAL NO-UNDO.
  DEFINE VARIABLE l_Page  AS INTEGER NO-UNDO.
  DEFINE VARIABLE l_Line  AS INTEGER NO-UNDO.

  IF NOT CAN-FIND(FIRST TT_pdf_stream
                  WHERE TT_pdf_stream.obj_stream = pdfStream NO-LOCK)
  THEN DO:
    RUN pdf_error(pdfStream,"pdf_text_at","Cannot find Stream!").
    RETURN .
  END.
  
  DEFINE VARIABLE L_TextWidth   AS INTEGER NO-UNDO.
  DEFINE VARIABLE L_TextY       AS CHARACTER NO-UNDO.
  DEFINE VARIABLE L_LeftMargin  AS CHARACTER NO-UNDO.
  L_TextY      = STRING(pdf_TextY(pdfStream)).
  L_LeftMargin = STRING(pdf_LeftMargin(pdfStream)).

  DEFINE VARIABLE L_orig_text   AS CHARACTER NO-UNDO.
  L_orig_text = pdfText.

  IF pdfColumn = 0 THEN DO:
    RUN pdf_error(pdfStream,"pdf_text_at","Column cannot be zero!").
    RETURN .
  END.

  RUN pdf_replace_text (INPUT-OUTPUT pdfText).

  RUN OutputTextContent(pdfStream, 
                        "TEXTAT",
                          "1 0 0 1 " + L_LeftMargin + " "
                        + L_TextY + " Tm" + CHR(10)
                        + "(",
                        FILL(" ", pdfColumn - 1) + pdfText,
                        ") Tj ").

  /* Increase the TextX dimension to include the new text.  The Y Dimension
     shouldn't change */
  RUN pdf_set_TextX(pdfStream,  pdf_TextX(pdfStream)
                              + pdf_text_width(pdfStream, FILL(" ", pdfColumn - 1) + pdfText)).

END. /* pdf_text_at */

PROCEDURE pdf_text_to:
  DEFINE INPUT PARAMETER pdfStream   AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfText     AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfColumn   AS INTEGER NO-UNDO.

  IF NOT CAN-FIND(FIRST TT_pdf_stream
                  WHERE TT_pdf_stream.obj_stream = pdfStream NO-LOCK)
  THEN DO:
    RUN pdf_error(pdfStream,"pdf_text_to","Cannot find Stream!").
    RETURN .
  END.

  DEFINE VARIABLE L_X            AS INTEGER   NO-UNDO.
  DEFINE VARIABLE L_Y            AS INTEGER   NO-UNDO.
  DEFINE VARIABLE L_LeftMargin   AS INTEGER NO-UNDO.

  L_Y          = pdf_TextY(pdfStream).
  L_LeftMargin = pdf_LeftMargin(pdfStream).

  DEFINE VARIABLE L_orig_text   AS CHARACTER NO-UNDO.
  L_orig_text = pdfText.

  IF pdfColumn = 0 THEN DO:
    RUN pdf_error(pdfStream,"pdf_text_to","Column cannot be zero!").
    RETURN .
  END.

  RUN pdf_replace_text (INPUT-OUTPUT pdfText).

  IF pdf_FontType(pdfStream) = "FIXED" THEN DO:

    RUN OutputTextContent(pdfStream, 
                          "TEXTTO",
                            "1 0 0 1 " + STRING((L_LeftMargin + pdf_text_width(pdfstream, FILL(" ", pdfColumn - LENGTH(L_orig_text, "character":u) - 1)))) + " " 
                          + STRING(L_Y) + " Tm" + CHR(10)
                          + "(",
                          pdfText,
                          ") Tj ").

    /* Set the TextX dimension to the new text column.  The Y Dimension
       shouldn't change */
    RUN pdf_set_TextX(pdfStream,  pdf_TextX(pdfStream)
                                + pdf_text_width(pdfStream, FILL(" ", pdfColumn - LENGTH(L_orig_text, "character":u) - 1)
                                                            + pdfText)).

  END. /* Fixed */

  /* Variable - proportional fonts */
  ELSE DO:
    L_X = pdf_text_width(pdfStream,FILL("E",pdfColumn))
        - pdf_text_width(pdfStream, L_orig_text).

    RUN OutputTextContent(pdfStream, 
                          "TEXTTO",
                            "1 0 0 1 "
                          + STRING(L_X) + " "
                          + STRING(L_Y) + " Tm" + CHR(10)
                          + "(",
                          L_orig_text,
                          ") Tj " ).

    /* Set the TextX dimension to the new text column.  The Y Dimension
       shouldn't change */
    RUN pdf_set_TextX(pdfStream,  L_X + pdf_text_width(pdfstream,L_Orig_text)).

  END. /* Variable */

END. /* pdf_text_to */

PROCEDURE pdf_text_align:
  DEFINE INPUT PARAMETER pdfStream   AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfText     AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfAlign    AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfX        AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER pdfY        AS INTEGER NO-UNDO.

  DEFINE VARIABLE L_color       AS CHARACTER NO-UNDO.
  DEFINE VARIABLE L_Font        AS CHARACTER NO-UNDO.
  DEFINE VARIABLE L_PointSize   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE L_LeftMargin  AS CHARACTER NO-UNDO.

  DEFINE VARIABLE L_Width       AS DECIMAL NO-UNDO.

  /* Begin Procedure Validation */
  IF NOT CAN-FIND(FIRST TT_pdf_stream
                  WHERE TT_pdf_stream.obj_stream = pdfStream NO-LOCK)
  THEN DO:
    RUN pdf_error(pdfStream,"pdf_text_align","Cannot find Stream!").
    RETURN .
  END.

  IF LOOKUP(pdfAlign,"LEFT,RIGHT,CENTER":U) = 0 THEN DO:
    RUN pdf_error(pdfStream,"pdf_text_align","Invalid Alignment option passed!").
    RETURN .
  END.

  IF pdfX = 0 THEN DO:
    RUN pdf_error(pdfStream,"pdf_text_align","X location cannot be zero!").
    RETURN .
  END.

  IF pdfY = 0 THEN DO:
    RUN pdf_error(pdfStream,"pdf_text_align","Y location cannot be zero!").
    RETURN .
  END.

  /* end of validation */
  L_color = dec2string(pdf_TextRed(pdfStream)) + " "
          + dec2string(pdf_TextGreen(pdfStream)) + " "
          + dec2string(pdf_TextBlue(pdfStream)) + " rg" + CHR(10).

  L_PointSize  = dec2string(pdf_PointSize(pdfStream)).
  L_LeftMargin = STRING(pdf_LeftMargin(pdfStream)).

  L_font = pdf_Font(pdfStream).

  FIND FIRST TT_pdf_font
       WHERE TT_pdf_font.obj_stream = pdfStream
         AND TT_pdf_font.font_name  = L_Font NO-LOCK NO-ERROR.
  L_Font = IF AVAIL TT_pdf_font THEN TT_pdf_Font.font_tag ELSE "/BF1".

  pdfText = TRIM(pdfText).
  RUN pdf_replace_text (INPUT-OUTPUT pdftext).

  L_Width = pdf_text_widthdec(pdfStream, pdftext).

  CASE pdfAlign:
    WHEN "RIGHT" THEN DO:
      RUN OutputTextContent(pdfStream, 
                            "TEXTXY",
                              /* igc - rem CHR(10) 
                            + */ L_Font + " " + L_PointSize + " Tf" + CHR(10)
                            + L_color
                            + pdf_get_parameter(pdfStream,"ScaleX") 
                            + " 0 0 " 
                            + pdf_get_parameter(pdfStream,"ScaleY")
                            + " " + dec2string(pdfX - L_Width) + " "
                            + STRING(pdfY) + " Tm" + CHR(10)
                            + "(",
                            pdfText,
                            ") Tj").

      RUN pdf_set_TextX(pdfStream,  pdfX ).
    END. /* Right Alignment */

    WHEN "LEFT" THEN DO:
      RUN OutputTextContent(pdfStream, 
                            "TEXTXY",
                              /* igc-rem CHR(10) 
                            + */ L_Font + " " + L_PointSize + " Tf" + CHR(10)
                            + L_color
                            + pdf_get_parameter(pdfStream,"ScaleX") 
                            + " 0 0 " 
                            + pdf_get_parameter(pdfStream,"ScaleY")
                            + " " +  STRING(pdfX) + " "
                            + STRING(pdfY) + " Tm" + CHR(10)
                            + "(",
                            pdfText,
                            ") Tj").

      RUN pdf_set_TextX(pdfStream,  INT(pdfX + L_Width)).
    END. /* Left Alignment */

    WHEN "CENTER" THEN DO:
      L_Width = L_Width / 2.

      RUN OutputTextContent(pdfStream, 
                            "TEXTXY",
                              /* igc - rem CHR(10) 
                            + */ L_Font + " " + L_PointSize + " Tf" + CHR(10)
                            + L_color
                            + pdf_get_parameter(pdfStream,"ScaleX") 
                            + " 0 0 " 
                            + pdf_get_parameter(pdfStream,"ScaleY")
                            + " " +  dec2string(pdfX - L_Width) + " "
                            + STRING(pdfY) + " Tm" + CHR(10)
                            + "(",
                            pdfText,
                            ") Tj" ).

      RUN pdf_set_TextX(pdfStream,  INT(pdfX + L_Width)).
    END. /* Centered Alignment */

  END CASE.
  
END. /* pdf_text_align */

PROCEDURE pdf_set_Angle :  /* PRIVATE */
  DEFINE INPUT PARAMETER pdfStream          AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfValue           AS INTEGER NO-UNDO.

  IF NOT CAN-FIND(FIRST TT_pdf_stream
                  WHERE TT_pdf_stream.obj_stream = pdfStream NO-LOCK)
  THEN DO:
    RUN pdf_error(pdfStream,"pdf_set_Angle","Cannot find Stream!").
    RETURN .
  END.

  DEFINE VARIABLE L_option  AS CHARACTER NO-UNDO.
  L_Option = "0,45,90,135,180,225,270,315".

  IF LOOKUP(STRING(pdfValue),L_option) = 0 THEN DO:
    RUN pdf_error(pdfStream,"pdf_set_Angle","Invalid Angle option passed!").
    RETURN .
  END.

  FIND FIRST B_TT_pdf_param
       WHERE B_TT_pdf_param.obj_stream    = pdfStream
         AND B_TT_pdf_param.obj_parameter = "Angle" EXCLUSIVE-LOCK NO-ERROR.
  IF NOT AVAIL B_TT_pdf_param THEN DO:
    CREATE B_TT_pdf_param.
    ASSIGN B_TT_pdf_param.obj_stream    = pdfStream
           B_TT_pdf_param.obj_parameter = "Angle".
  END.

  B_TT_pdf_param.obj_value = STRING(pdfValue).

END. /* pdf_set_Angle */

PROCEDURE pdf_set_Orientation :
  DEFINE INPUT PARAMETER pdfStream          AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfValue           AS CHARACTER NO-UNDO.

  DEFINE VARIABLE L_width   AS INTEGER NO-UNDO.
  DEFINE VARIABLE L_height  AS INTEGER NO-UNDO.

  IF NOT CAN-FIND(FIRST TT_pdf_stream
                  WHERE TT_pdf_stream.obj_stream = pdfStream NO-LOCK)
  THEN DO:
    RUN pdf_error(pdfStream,"pdf_set_Orientation","Cannot find Stream!").
    RETURN .
  END.

  DEFINE VARIABLE L_option  AS CHARACTER NO-UNDO.
  L_Option = "Portrait,Landscape".

  IF LOOKUP(pdfValue,L_option) = 0 THEN DO:
    RUN pdf_error(pdfStream,"pdf_set_Orientation","Invalid Orientation option passed!").
    RETURN .
  END.

  FIND FIRST B_TT_pdf_param
       WHERE B_TT_pdf_param.obj_stream    = pdfStream
         AND B_TT_pdf_param.obj_parameter = "Orientation" NO-LOCK NO-ERROR.
  IF NOT AVAIL B_TT_pdf_param THEN DO:
    CREATE B_TT_pdf_param.
    ASSIGN B_TT_pdf_param.obj_stream    = pdfStream
           B_TT_pdf_param.obj_parameter = "Orientation".
  END.

  IF B_TT_pdf_param.obj_value <> pdfValue THEN DO:
    L_width  = pdf_PageWidth(pdfStream).
    L_height = pdf_PageHeight(pdfStream).

    IF pdfValue = "Landscape" THEN DO:
      RUN pdf_set_PageWidth(pdfStream,L_height).
      RUN pdf_set_PageHeight(pdfStream,L_width).
    END.
    ELSE DO:
      IF B_TT_pdf_param.obj_value = "Landscape" THEN DO:
        L_width  = pdf_PageHeight(pdfStream).
        L_height = pdf_PageWidth(pdfStream).
      END.

      RUN pdf_set_PageWidth(pdfStream,L_width).
      RUN pdf_set_PageHeight(pdfStream,L_height).
    END.
  END.

  B_TT_pdf_param.obj_value = pdfValue.

END. /* pdf_set_Orientation */

PROCEDURE pdf_set_VerticalSpace :
  DEFINE INPUT PARAMETER pdfStream          AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfValue           AS DECIMAL NO-UNDO.

  IF NOT CAN-FIND(FIRST TT_pdf_stream
                  WHERE TT_pdf_stream.obj_stream = pdfStream NO-LOCK)
  THEN DO:
    RUN pdf_error(pdfStream,"pdf_set_VerticalSpace","Cannot find Stream!").
    RETURN .
  END.

  IF pdfValue = 0 THEN DO:
    RUN pdf_error(pdfStream,"pdf_set_VerticalSpace","Vertical Space cannot be zero!").
    RETURN .
  END.

  FIND FIRST TT_pdf_param
       WHERE TT_pdf_param.obj_stream    = pdfStream
         AND TT_pdf_param.obj_parameter = "VerticalSpace" EXCLUSIVE-LOCK NO-ERROR.
  IF NOT AVAIL TT_pdf_param THEN DO:
    CREATE TT_pdf_param.
    ASSIGN TT_pdf_param.obj_stream    = pdfStream
           TT_pdf_param.obj_parameter = "VerticalSpace".
  END.

  TT_pdf_param.obj_value = dec2string(pdfValue).

END. /* pdf_set_VerticalSpace */

PROCEDURE pdf_set_LeftMargin :
  DEFINE INPUT PARAMETER pdfStream          AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfValue           AS INTEGER NO-UNDO.

  IF NOT CAN-FIND(FIRST TT_pdf_stream
                  WHERE TT_pdf_stream.obj_stream = pdfStream NO-LOCK)
  THEN DO:
    RUN pdf_error(pdfStream,"pdf_set_LeftMargin","Cannot find Stream!").
    RETURN .
  END.

  IF pdfValue = 0 THEN DO:
    RUN pdf_error(pdfStream,"pdf_set_LeftMargin","Left Margin cannot be zero!").
    RETURN .
  END.

  FIND FIRST TT_pdf_param
       WHERE TT_pdf_param.obj_stream    = pdfStream
         AND TT_pdf_param.obj_parameter = "LeftMargin" EXCLUSIVE-LOCK NO-ERROR.
  IF NOT AVAIL TT_pdf_param THEN DO:
    CREATE TT_pdf_param.
    ASSIGN TT_pdf_param.obj_stream    = pdfStream
           TT_pdf_param.obj_parameter = "LeftMargin".
  END.

  TT_pdf_param.obj_value = STRING(pdfValue).

END. /* pdf_set_LeftMargin */

PROCEDURE pdf_set_TopMargin :
  DEFINE INPUT PARAMETER pdfStream          AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfValue           AS INTEGER NO-UNDO.

  IF NOT CAN-FIND(FIRST TT_pdf_stream
                  WHERE TT_pdf_stream.obj_stream = pdfStream NO-LOCK)
  THEN DO:
    RUN pdf_error(pdfStream,"pdf_set_TopMargin","Cannot find Stream!").
    RETURN .
  END.

  IF pdfValue = 0 THEN DO:
    RUN pdf_error(pdfStream,"pdf_set_TopMargin","Top Margin cannot be zero!").
    RETURN .
  END.

  FIND FIRST TT_pdf_param
       WHERE TT_pdf_param.obj_stream    = pdfStream
         AND TT_pdf_param.obj_parameter = "TopMargin" EXCLUSIVE-LOCK NO-ERROR.
  IF NOT AVAIL TT_pdf_param THEN DO:
    CREATE TT_pdf_param.
    ASSIGN TT_pdf_param.obj_stream    = pdfStream
           TT_pdf_param.obj_parameter = "TopMargin".
  END.

  TT_pdf_param.obj_value = STRING(pdfValue).

END. /* pdf_set_TopMargin */

PROCEDURE pdf_set_BottomMargin :
  DEFINE INPUT PARAMETER pdfStream          AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfValue           AS INTEGER NO-UNDO.

  IF NOT CAN-FIND(FIRST TT_pdf_stream
                  WHERE TT_pdf_stream.obj_stream = pdfStream NO-LOCK)
  THEN DO:
    RUN pdf_error(pdfStream,"pdf_set_BottomMargin","Cannot find Stream!").
    RETURN .
  END.

  IF pdfValue <= 0 THEN DO:
    RUN pdf_error(pdfStream,"pdf_set_BottomMargin","Bottom Margin cannot be <= zero!").
    RETURN .
  END.

  FIND FIRST TT_pdf_param
       WHERE TT_pdf_param.obj_stream    = pdfStream
         AND TT_pdf_param.obj_parameter = "BottomMargin" EXCLUSIVE-LOCK NO-ERROR.
  IF NOT AVAIL TT_pdf_param THEN DO:
    CREATE TT_pdf_param.
    ASSIGN TT_pdf_param.obj_stream    = pdfStream
           TT_pdf_param.obj_parameter = "BottomMargin".
  END.

  TT_pdf_param.obj_value = STRING(pdfValue).

END. /* pdf_set_BottomMargin */

PROCEDURE pdf_set_PaperType :
  DEFINE INPUT PARAMETER pdfStream          AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfValue           AS CHARACTER NO-UNDO.

  IF NOT CAN-FIND(FIRST TT_pdf_stream
                  WHERE TT_pdf_stream.obj_stream = pdfStream NO-LOCK)
  THEN DO:
    RUN pdf_error(pdfStream,"pdf_set_PaperType","Cannot find Stream!").
    RETURN .
  END.

  DEFINE VARIABLE L_option  AS CHARACTER NO-UNDO.
  DEFINE VARIABLE L_width   AS INTEGER NO-UNDO.
  DEFINE VARIABLE L_height  AS INTEGER NO-UNDO.

  L_Option = "A0,A1,A2,A3,A4,A5,A6,B5,LETTER,LEGAL,LEDGER".

  IF LOOKUP(pdfValue,L_option) = 0 THEN DO:
    RUN pdf_error(pdfStream,"pdf_set_PaperType","Invalid Paper Type option passed!").
    RETURN .
  END.

  FIND FIRST TT_pdf_param
       WHERE TT_pdf_param.obj_stream    = pdfStream
         AND TT_pdf_param.obj_parameter = "PaperType" EXCLUSIVE-LOCK NO-ERROR.
  IF NOT AVAIL TT_pdf_param THEN DO:
    CREATE TT_pdf_param.
    ASSIGN TT_pdf_param.obj_stream    = pdfStream
           TT_pdf_param.obj_parameter = "PaperType".
  END.

  /* Set the Paper Type */
  TT_pdf_param.obj_value = pdfValue.

  /* Determine the Paper Height and Width */
  CASE pdfValue:
    WHEN "A0" THEN
      ASSIGN L_width  = 2380
             L_height = 3368.
    WHEN "A1" THEN
      ASSIGN L_width  = 1684
             L_height = 2380.
    WHEN "A2" THEN
      ASSIGN L_width  = 1190
             L_height = 1684.
    WHEN "A3" THEN
      ASSIGN L_width  = 842
             L_height = 1190.
    WHEN "A4" THEN
      ASSIGN L_width  = 595
             L_height = 842.
    WHEN "A5" THEN
      ASSIGN L_width  = 421
             L_height = 595.
    WHEN "A6" THEN
      ASSIGN L_width  = 297
             L_height = 421.
    WHEN "B5" THEN
      ASSIGN L_width  = 501
             L_height = 709.
    WHEN "LETTER" THEN
      ASSIGN L_width  = 612
             L_height = 792.
    WHEN "LEGAL" THEN
      ASSIGN L_width  = 612
             L_height = 1008.
    WHEN "LEDGER" THEN
      ASSIGN L_width  = 1224
             L_height = 792.
    OTHERWISE
      ASSIGN L_width  = 612
             L_height = 792.
  END CASE.

  /* Now Set the Page Height and Width Parameters */
  IF pdf_Orientation(pdfStream) = "Portrait" THEN DO:
    RUN pdf_set_PageWidth(pdfStream,L_width).
    RUN pdf_set_PageHeight(pdfStream,L_height).
  END.
  ELSE DO:
    RUN pdf_set_PageWidth(pdfStream,L_height).
    RUN pdf_set_PageHeight(pdfStream,L_width).
  END.

END. /* pdf_set_PaperType */

FUNCTION pdf_PaperType RETURN CHARACTER ( INPUT pdfStream AS CHARACTER):

  IF NOT CAN-FIND(FIRST TT_pdf_stream
                  WHERE TT_pdf_stream.obj_stream = pdfStream NO-LOCK)
  THEN DO:
    RUN pdf_error(pdfStream,"pdf_PaperType","Cannot find Stream!").
    RETURN ERROR.
  END.

  FIND FIRST TT_pdf_param WHERE TT_pdf_param.obj_stream    = pdfStream
                            AND TT_pdf_param.obj_parameter = "PaperType"
                            NO-LOCK NO-ERROR.
  IF AVAIL TT_pdf_param THEN RETURN TT_pdf_param.obj_value.
  ELSE RETURN "LETTER".

END. /* pdf_PaperType */

FUNCTION pdf_Render RETURN INTEGER ( INPUT pdfStream AS CHARACTER):

  IF NOT CAN-FIND(FIRST TT_pdf_stream
                  WHERE TT_pdf_stream.obj_stream = pdfStream NO-LOCK)
  THEN DO:
    RUN pdf_error(pdfStream,"pdf_Render","Cannot find Stream!").
    RETURN ERROR.
  END.

  FIND FIRST TT_pdf_param WHERE TT_pdf_param.obj_stream    = pdfStream
                            AND TT_pdf_param.obj_parameter = "Render"
                            NO-LOCK NO-ERROR.
  IF AVAIL TT_pdf_param THEN RETURN INT(TT_pdf_param.obj_value).
  ELSE RETURN 0.

END. /* pdf_Render */

FUNCTION pdf_get_wrap_length RETURNS INTEGER ( INPUT pdfStream   AS CHARACTER,
                                               INPUT pdfText AS CHARACTER,
                                               INPUT pdfWidth AS INTEGER ):
  DEFINE VAR pdfHeight    AS INTEGER NO-UNDO.

  DEFINE VAR v-thisline   AS CHAR NO-UNDO.
  DEFINE VAR v-lastline   AS CHAR NO-UNDO.
  DEFINE VAR i            AS INTEGER NO-UNDO.
  DEFINE VAR v-pointsize  AS INTEGER NO-UNDO.
  DEFINE VAR v-maxwidth   AS INTEGER NO-UNDO.

  v-maxwidth = pdf_getnumfittingchars(pdfStream,
                                      FILL("W",pdfWidth),
                                      0,
                                      pdf_text_width(pdfstream,FILL("W",pdfWidth))).

  v-pointsize = PDF_VerticalSpace(pdfStream).
  IF v-PointSize = 0 THEN
    v-pointsize = pdf_PointSize(pdfStream).

  ASSIGN pdfText = REPLACE(pdfText,"|","&pipe;").
  /* Spaces */
  ASSIGN pdfText = REPLACE(pdfText," ","| ").
  /* Hyphens */
  ASSIGN pdfText = REPLACE(pdfText,"-","-|").
  /* Commas */
  ASSIGN pdfText = REPLACE(pdfText,",",",|").

  /* Divide up the pdf text into lines of width less than the
     available width */
  DO i = 1 TO NUM-ENTRIES(pdfText,"|"):
    ASSIGN v-lastline = v-thisline.
    ASSIGN v-thisline = v-thisline
                      + REPLACE(ENTRY(i,pdfText,"|"),"&pipe;","|").

    IF pdf_getnumfittingchars(pdfStream,
                              TRIM(v-thisline),
                              0,
                              pdf_text_width(pdfstream,TRIM(v-thisline))) > v-maxwidth
    THEN DO:
        ASSIGN pdfHeight = pdfHeight + v-pointsize.
        ASSIGN v-thisline = TRIM(REPLACE(ENTRY(i,pdfText,"|"),"&pipe;","|")).
    END.
  END.

  IF v-thisline NE "" THEN
    ASSIGN pdfHeight = pdfHeight + v-pointsize.

  RETURN pdfHeight.
END FUNCTION. /* pdf_get_wrap_length */

/* ---------------------- Define INTERNAL PROCEDURES ----------------------- */
PROCEDURE pdf_init_param:  /* PRIVATE */
  DEFINE INPUT PARAMETER pdfStream AS CHARACTER NO-UNDO.

  /* Create Parameters */
  RUN pdf_set_Orientation(pdfStream,"Portrait").
  RUN pdf_set_PaperType(pdfStream,"LETTER").  /* This also default the PageWidth
                                             and PageHeight Parameters */
  RUN pdf_set_Font(pdfStream,"Courier",10.0). /* This also sets the PointSize */
  RUN pdf_set_LeftMargin(pdfStream,10).
  RUN pdf_set_TopMargin(pdfStream,50).
  RUN pdf_set_BottomMargin(pdfStream,1).
  RUN pdf_set_Angle(pdfStream,0).
  RUN pdf_set_TextY(pdfStream,pdf_PageHeight(pdfStream) - pdf_TopMargin(pdfStream)).
  RUN pdf_set_TextX(pdfStream, pdf_LeftMargin(pdfStream)).
  RUN pdf_set_GraphicY(pdfStream,0).
  RUN pdf_set_GraphicX(pdfStream,0).
  RUN pdf_set_TextRed(pdfStream,.0).
  RUN pdf_set_TextGreen(pdfStream,.0).
  RUN pdf_set_TextBlue(pdfStream,.0).
  RUN pdf_set_FillRed(pdfStream,1.0).
  RUN pdf_set_FillGreen(pdfStream,1.0).
  RUN pdf_set_FillBlue(pdfStream,1.0).
  RUN pdf_set_PageRotate(pdfStream,0).    /* Default to zero rotation */

  RUN pdf_set_parameter(pdfStream,"PageMode","UseNone").
  RUN pdf_set_parameter(pdfStream,"PageLayout","SinglePage").

  RUN pdf_set_parameter(pdfStream,"ScaleX","1").
  RUN pdf_set_parameter(pdfStream,"ScaleY","1").

  RUN pdf_set_parameter(pdfStream,"VERSION","3.3"). /* Do not change */

END. /* pdf_init_param */

PROCEDURE pdf_Header : /* PRIVATE */
  DEFINE INPUT PARAMETER P_Stream   AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER p_Encrypt  AS LOGICAL NO-UNDO.

  DEFINE VARIABLE l_CreationDate  AS CHARACTER NO-UNDO.

  DEFINE VARIABLE m_EncryptKey    AS MEMPTR NO-UNDO.
  DEFINE VARIABLE m_Author        AS MEMPTR NO-UNDO.
  DEFINE VARIABLE m_CreationDate  AS MEMPTR NO-UNDO.
  DEFINE VARIABLE m_Creator       AS MEMPTR NO-UNDO.
  DEFINE VARIABLE m_Producer      AS MEMPTR NO-UNDO.
  DEFINE VARIABLE m_Subject       AS MEMPTR NO-UNDO.
  DEFINE VARIABLE m_Title         AS MEMPTR NO-UNDO.
  DEFINE VARIABLE m_Keywords      AS MEMPTR NO-UNDO.

  /* Version Compatibilities */
  PUT STREAM S_pdf_inc UNFORMATTED
      "%PDF-1.4" {&pdfSKIP}.

  /* Output 4 Binary Characters (greater than ASCII 128) to indicate to a binary
     file -- randomly selected codes */
  PUT STREAM S_pdf_inc UNFORMATTED
      "%" CHR(228) CHR(227) CHR(207) CHR(210) {&pdfSKIP}.

  l_CreationDate = "D:" + TRIM(STRING(YEAR(TODAY),"9999"))
                 + TRIM(STRING(MONTH(TODAY),"99"))
                 + TRIM(STRING(DAY(TODAY),"99"))
                 + REPLACE(STRING(TIME,"hh:mm:ss"),":","") + "-0800".

  ObjectSequence( p_Stream,1, "Info", 0 ).
  /* Display Creation, Title, Producer etc Information */
  PUT STREAM S_pdf_inc UNFORMATTED
      "1 0 obj" {&pdfSKIP}
      "<<" {&pdfSKIP}.

  IF p_Encrypt THEN DO:
    SET-SIZE(m_EncryptKey) = IF pdf_get_parameter(p_Stream,"EncryptKey") = "40" 
                             THEN 10 ELSE 32.
    RUN GetEncryptKey /* IN h_PDF-Encrypt */
        (INPUT  p_Stream,
         INPUT  TT_pdf_stream.obj_UniqueID,
         INPUT  1,
         INPUT  0,
         INPUT  pdf-EncryptKeyMemPtr,
         OUTPUT m_EncryptKey).

    IF LENGTH(pdf_get_info(p_Stream,"Author"), "character":u) > 0 THEN DO:
      SET-SIZE(m_Author) = LENGTH(pdf_get_info(P_Stream,"Author"), "character":u) + 1.
      PUT-STRING(m_Author,1) = pdf_get_info(P_Stream,"Author").

      RUN EncryptContent /* IN h_PDF-Encrypt */
                         (INPUT  m_EncryptKey,
                          INPUT  m_Author,
                          OUTPUT m_Author).

      PUT STREAM S_pdf_inc UNFORMATTED "/Author <".

      RUN OutputMemPtrAsHex (m_Author).
      SET-SIZE(m_Author) = 0.

      PUT STREAM S_pdf_inc UNFORMATTED ">" {&pdfSKIP}.
    END. /* Show Author */

    /* Show Creation Date */
    SET-SIZE(m_CreationDate) = LENGTH(l_CreationDate, "character":u) + 1.
    PUT-STRING(m_CreationDate,1) = l_CreationDate.

    RUN EncryptContent /* IN h_PDF-Encrypt */
                       (INPUT  m_EncryptKey,
                        INPUT  m_CreationDate,
                        OUTPUT m_CreationDate).

    PUT STREAM S_pdf_inc UNFORMATTED "/CreationDate<".

    RUN OutputMemPtrAsHex (m_CreationDate).
    SET-SIZE(m_CreationDate) = 0.
    PUT STREAM S_pdf_inc UNFORMATTED ">" {&pdfSKIP}.
    /* Show Creation Date */

    IF LENGTH(pdf_get_info(p_Stream,"Creator"), "character":u) > 0 THEN DO:
      SET-SIZE(m_Creator) = LENGTH(pdf_get_info(P_Stream,"Creator"), "character":u) + 1.
      PUT-STRING(m_Creator,1) = pdf_get_info(P_Stream,"Creator").

      RUN EncryptContent /* IN h_PDF-Encrypt */
                         (INPUT  m_EncryptKey,
                          INPUT  m_Creator,
                          OUTPUT m_Creator).

      PUT STREAM S_pdf_inc UNFORMATTED "/Creator <".

      RUN OutputMemPtrAsHex (m_Creator).
      SET-SIZE(m_Creator) = 0.
      PUT STREAM S_pdf_inc UNFORMATTED ">" {&pdfSKIP}.
    END. /* Show Creator */

    IF LENGTH(pdf_get_info(p_Stream,"Producer"), "character":u) > 0 THEN DO:
      SET-SIZE(m_Producer) = LENGTH(pdf_get_info(P_Stream,"Producer"), "character":u) + 1.
      PUT-STRING(m_Producer,1) = pdf_get_info(P_Stream,"Producer").

      RUN EncryptContent /* IN h_PDF-Encrypt */
                         (INPUT  m_EncryptKey,
                          INPUT  m_Producer,
                          OUTPUT m_Producer).

      PUT STREAM S_pdf_inc UNFORMATTED "/Producer <".

      RUN OutputMemPtrAsHex (m_Producer).
      SET-SIZE(m_Producer) = 0.
      PUT STREAM S_pdf_inc UNFORMATTED ">" {&pdfSKIP}.
    END. /* Show Producer */

    IF LENGTH(pdf_get_info(p_Stream,"Subject"), "character":u) > 0 THEN DO:
      SET-SIZE(m_Subject) = LENGTH(pdf_get_info(P_Stream,"Subject"), "character":u) + 1.
      PUT-STRING(m_Subject,1) = pdf_get_info(P_Stream,"Subject").

      RUN EncryptContent /* IN h_PDF-Encrypt */
                         (INPUT  m_EncryptKey,
                          INPUT  m_Subject,
                          OUTPUT m_Subject).

      PUT STREAM S_pdf_inc UNFORMATTED "/Subject <".

      RUN OutputMemPtrAsHex (m_Subject).
      SET-SIZE(m_Subject) = 0.
      PUT STREAM S_pdf_inc UNFORMATTED ">" {&pdfSKIP}.
    END. /* Show Subject */

    IF LENGTH(pdf_get_info(p_Stream,"Title"), "character":u) > 0 THEN DO:
      SET-SIZE(m_Title) = LENGTH(pdf_get_info(P_Stream,"Title"), "character":u) + 1.
      PUT-STRING(m_Title,1) = pdf_get_info(P_Stream,"Title").

      RUN EncryptContent /* IN h_PDF-Encrypt */
                         (INPUT  m_EncryptKey,
                          INPUT  m_Title,
                          OUTPUT m_Title).

      PUT STREAM S_pdf_inc UNFORMATTED "/Title <".

      RUN OutputMemPtrAsHex (m_Title).
      SET-SIZE(m_Title) = 0.
      PUT STREAM S_pdf_inc UNFORMATTED ">" {&pdfSKIP}.
    END. /* Show Title */

    IF LENGTH(pdf_get_info(p_Stream,"Keywords"), "character":u) > 0 THEN DO:
      SET-SIZE(m_Keywords) = LENGTH(pdf_get_info(P_Stream,"Keywords"), "character":u) + 1.
      PUT-STRING(m_Keywords,1) = pdf_get_info(P_Stream,"Keywords").

      RUN EncryptContent /* IN h_PDF-Encrypt */
                         (INPUT  m_EncryptKey,
                          INPUT  m_Keywords,
                          OUTPUT m_Keywords).

      PUT STREAM S_pdf_inc UNFORMATTED "/Keywords <".

      RUN OutputMemPtrAsHex (m_Keywords).
      SET-SIZE(m_Keywords) = 0.
      PUT STREAM S_pdf_inc UNFORMATTED ">" {&pdfSKIP}.
    END. /* Show Title */

    SET-SIZE(m_EncryptKey) = 0.
  END. /* If Encrypted PDF */

  ELSE DO: /* Not Encrypted */
    PUT STREAM S_pdf_inc UNFORMATTED
        "/Author (" pdf_get_info(p_Stream,"Author") ")" {&pdfSKIP}
        "/CreationDate (" l_CreationDate ")" {&pdfSKIP}
        "/Producer (" pdf_get_info(P_Stream,"Producer") ")" {&pdfSKIP}
        "/Creator (" pdf_get_info(P_Stream,"Creator") ")" {&pdfSKIP}
        "/Subject (" pdf_get_info(P_Stream,"Subject") ")" {&pdfSKIP}
        "/Title (" pdf_get_info(P_Stream,"Title") ")" {&pdfSKIP}
        "/Keywords (" pdf_get_info(P_Stream,"Keywords") ")" {&pdfSKIP}.
  END. /* Not Encrypted */

  PUT STREAM S_pdf_inc UNFORMATTED
      " >>" {&pdfSKIP}
      "endobj" {&pdfSKIP}.

END. /* pdf_header */

PROCEDURE pdf_LoadBase14: /* PRIVATE */

  DEFINE INPUT PARAMETER pdfStream  AS CHARACTER NO-UNDO.

  DEFINE VARIABLE L_Object          AS INTEGER NO-UNDO.

  L_Object = 5.

  /* ---- Beginning of Courier Fonts ---- */

  /* Create Associated Object */
  CREATE TT_pdf_font.
  ASSIGN TT_pdf_font.font_name  = "Courier"
         TT_pdf_font.font_file  = "PDFBASE14"
         TT_pdf_font.font_afm   = ""
         TT_pdf_font.font_dif   = ""
         TT_pdf_font.font_obj   = L_Object
         TT_pdf_font.font_tag   = "/BF1"
         TT_pdf_font.obj_stream = pdfStream
         TT_pdf_font.font_type  = "FIXED"
         TT_pdf_font.font_width = "600".
  L_Object = L_Object + 1.

  /* Create Associated Object */
  CREATE TT_pdf_font.
  ASSIGN TT_pdf_font.font_name  = "Courier-Oblique"
         TT_pdf_font.font_file  = "PDFBASE14"
         TT_pdf_font.font_afm   = ""
         TT_pdf_font.font_dif   = ""
         TT_pdf_font.font_obj   = L_Object
         TT_pdf_font.font_tag   = "/BF2"
         TT_pdf_font.obj_stream = pdfStream
         TT_pdf_font.font_type  = "FIXED"
         TT_pdf_font.font_width = "600".
  L_Object = L_Object + 1.

  /* Create Associated Object */
  CREATE TT_pdf_font.
  ASSIGN TT_pdf_font.font_name  = "Courier-Bold"
         TT_pdf_font.font_file  = "PDFBASE14"
         TT_pdf_font.font_afm   = ""
         TT_pdf_font.font_dif   = ""
         TT_pdf_font.font_obj   = L_Object
         TT_pdf_font.font_tag   = "/BF3"
         TT_pdf_font.obj_stream = pdfStream
         TT_pdf_font.font_type  = "FIXED"
         TT_pdf_font.font_width = "600".
  L_Object = L_Object + 1.

  /* Create Associated Object */
  CREATE TT_pdf_font.
  ASSIGN TT_pdf_font.font_name  = "Courier-BoldOblique"
         TT_pdf_font.font_file  = "PDFBASE14"
         TT_pdf_font.font_afm   = ""
         TT_pdf_font.font_dif   = ""
         TT_pdf_font.font_obj   = L_Object
         TT_pdf_font.font_tag   = "/BF4"
         TT_pdf_font.obj_stream = pdfStream
         TT_pdf_font.font_type  = "FIXED"
         TT_pdf_font.font_width = "600".
  L_Object = L_Object + 1.

  /* ---- End of Courier Fonts ---- */

  /* ---- Beginning of Helvetica Fonts ---- */

  /* Create Associated Object */
  CREATE TT_pdf_font.
  ASSIGN TT_pdf_font.font_name  = "Helvetica"
         TT_pdf_font.font_file  = "PDFBASE14"
         TT_pdf_font.font_afm   = ""
         TT_pdf_font.font_dif   = ""
         TT_pdf_font.font_obj   = L_Object
         TT_pdf_font.font_tag   = "/BF5"
         TT_pdf_font.obj_stream = pdfStream
         TT_pdf_font.font_type  = "VARIABLE"
         TT_pdf_font.font_width = FILL("788 ", 32) + "278 278 355 556 556 889 "
                                + "667 222 333 333 389 584 278 333 278 278 556 "
                                + "556 556 556 556 556 556 556 556 556 278 278 "
                                + "584 584 584 556 1015 667 667 722 722 667 611 778 "
                                + "722 278 500 667 556 833 722 778 667 778 722 "
                                + "667 611 722 667 944 667 667 611 278 278 278 "
                                + "469 556 222 556 556 500 556 556 278 556 556 "
                                + "222 222 500 222 833 556 556 556 556 333 500 "
                                + "278 556 500 722 500 500 500 334 260 334 584 "
                                + "333 556 556 167 556 556 556 556 191 333 556 "
                                + "333 333 500 500 556 556 556 278 537 350 222 "
                                + "333 333 556 1000 1000 611 333 333 333 333 333 "
                                + "333 333 333 333 333 333 333 333 1000 1000 370 "
                                + "556 778 1000 365 889 278 222 611 944 611".
  L_Object = L_Object + 1.

  CREATE TT_pdf_font.
  ASSIGN TT_pdf_font.font_name  = "Helvetica-Oblique"
         TT_pdf_font.font_file  = "PDFBASE14"
         TT_pdf_font.font_afm   = ""
         TT_pdf_font.font_dif   = ""
         TT_pdf_font.font_obj   = L_Object
         TT_pdf_font.font_tag   = "/BF6"
         TT_pdf_font.obj_stream = pdfStream
         TT_pdf_font.font_type  = "VARIABLE"
         TT_pdf_font.font_width = FILL("788 ", 32) + "278 278 355 556 556 889 "
                                + "667 222 333 333 389 584 278 333 278 278 556 "
                                + "556 556 556 556 556 556 556 556 556 278 278 "
                                + "584 584 584 556 1015 667 667 722 722 667 611 "
                                + "778 722 278 500 667 556 833 722 778 667 778 "
                                + "722 667 611 722 667 944 667 667 611 278 278 "
                                + "278 469 556 222 556 556 500 556 556 278 556 "
                                + "556 222 222 500 222 833 556 556 556 556 333 "
                                + "500 278 556 500 722 500 500 500 334 260 334 "
                                + "584 333 556 556 167 556 556 556 556 191 333 "
                                + "556 333 333 500 500 556 556 556 278 537 350 "
                                + "222 333 333 556 1000 1000 611 333 333 333 333 "
                                + "333 333 333 333 333 333 333 333 333 1000 "
                                + "1000 370 556 778 1000 365 889 278 222 611 "
                                + "944 611".
  L_Object = L_Object + 1.

  CREATE TT_pdf_font.
  ASSIGN TT_pdf_font.font_name  = "Helvetica-Bold"
         TT_pdf_font.font_file  = "PDFBASE14"
         TT_pdf_font.font_afm   = ""
         TT_pdf_font.font_dif   = ""
         TT_pdf_font.font_obj   = L_Object
         TT_pdf_font.font_tag   = "/BF7"
         TT_pdf_font.obj_stream = pdfStream
         TT_pdf_font.font_type  = "VARIABLE"
         TT_pdf_font.font_width = FILL("788 ", 32) + "278 333 474 556 556 889 "
                                + "722 278 333 333 389 584 278 333 278 278 556 "
                                + "556 556 556 556 556 556 556 556 556 333 333 "
                                + "584 584 584 611 975 722 722 722 722 667 611 "
                                + "778 722 278 556 722 611 833 722 778 667 778 "
                                + "722 667 611 722 667 944 667 667 611 333 278 "
                                + "333 584 556 278 556 611 556 611 556 333 611 "
                                + "611 278 278 556 278 889 611 611 611 611 389 "
                                + "556 333 611 556 778 556 556 500 389 280 389 "
                                + "584 333 556 556 167 556 556 556 556 238 500 "
                                + "556 333 333 611 611 556 556 556 278 556 350 "
                                + "278 500 500 556 1000 1000 611 333 333 333 333 "
                                + "333 333 333 333 333 333 333 333 333 1000 "
                                + "1000 370 611 778 1000 365 889 278 278 611 "
                                + "944 611".
  L_Object = L_Object + 1.

  CREATE TT_pdf_font.
  ASSIGN TT_pdf_font.font_name  = "Helvetica-BoldOblique"
         TT_pdf_font.font_file  = "PDFBASE14"
         TT_pdf_font.font_afm   = ""
         TT_pdf_font.font_dif   = ""
         TT_pdf_font.font_obj   = L_Object
         TT_pdf_font.font_tag   = "/BF8"
         TT_pdf_font.obj_stream = pdfStream
         TT_pdf_font.font_type  = "VARIABLE"
         TT_pdf_font.font_width = FILL("788 ", 32) + "278 333 474 556 556 889 "
                                + "722 278 333 333 389 584 278 333 278 278 556 "
                                + "556 556 556 556 556 556 556 556 556 333 333 "
                                + "584 584 584 611 975 722 722 722 722 667 611 "
                                + "778 722 278 556 722 611 833 722 778 667 778 "
                                + "722 667 611 722 667 944 667 667 611 333 278 "
                                + "333 584 556 278 556 611 556 611 556 333 611 "
                                + "611 278 278 556 278 889 611 611 611 611 389 "
                                + "556 333 611 556 778 556 556 500 389 280 389 "
                                + "584 333 556 556 167 556 556 556 556 238 500 "
                                + "556 333 333 611 611 556 556 556 278 556 350 "
                                + "278 500 500 556 1000 1000 611 333 333 333 "
                                + "333 333 333 333 333 333 333 333 333 333 "
                                + "1000 1000 370 611 778 1000 365 889 278 278 "
                                + "611 944 611".
  L_Object = L_Object + 1.

  /* ---- End of Helvetica Fonts ---- */

  /* ---- Beginning of Times Roman Fonts ---- */

  CREATE TT_pdf_font.
  ASSIGN TT_pdf_font.font_name  = "Times-Roman"
         TT_pdf_font.font_file  = "PDFBASE14"
         TT_pdf_font.font_afm   = ""
         TT_pdf_font.font_dif   = ""
         TT_pdf_font.font_obj   = L_Object
         TT_pdf_font.font_tag   = "/BF9"
         TT_pdf_font.obj_stream = pdfStream
         TT_pdf_font.font_type  = "VARIABLE"
         TT_pdf_font.font_width =   FILL("788 ", 32) + "250 " + "333 " + "408 "
                                + "500 500 833 778 180 333 333 500 564 250 "
                                + "333 250 278 500 500 500 500 500 500 500 "
                                + "500 500 500 278 278 564 564 564 444 921 "
                                + "722 667 667 722 611 556 722 722 333 389 "
                                + "722 611 889 722 722 556 722 667 556 611 "
                                + "722 722 944 722 722 611 333 278 333 469 "
                                + "500 333 444 500 444 500 444 333 500 500 "
                                + "278 278 500 278 778 500 500 500 500 333 "
                                + "389 278 500 500 722 500 500 444 480 200 "
                                + "480 541 778 500 578 333 500 444 1000 500 "
                                + "500 333 1000 556 333 889 667 611 722 444 "
                                + "333 333 444 444 350 500 1000 333 980 389 "
                                + "333 722 486 444 722 250 333 500 500 500 "
                                + "500 200 500 333 760 276 500 564 333 760 "
                                + "500 400 549 300 300 333 576 453 250 333 "
                                + "300 310 500 750 750 750 444 722 722 722 "
                                + "722 722 722 889 667 611 611 611 611 333 "
                                + "333 333 333 722 722 722 722 722 722 722 "
                                + "564 722 722 722 722 722 722 556 500 444 "
                                + "444 444 444 444 444 667 444 444 444 444 "
                                + "444 278 278 278 278 500 500 500 500 500 "
                                + "500 500 549 500 500 500 500 500 500 500 "
                                + "500".
  L_Object = L_Object + 1.

  CREATE TT_pdf_font.
  ASSIGN TT_pdf_font.font_name  = "Times-Italic"
         TT_pdf_font.font_file  = "PDFBASE14"
         TT_pdf_font.font_afm   = ""
         TT_pdf_font.font_dif   = ""
         TT_pdf_font.font_obj   = L_Object
         TT_pdf_font.font_tag   = "/BF10"
         TT_pdf_font.obj_stream = pdfStream
         TT_pdf_font.font_type  = "VARIABLE"
         TT_pdf_font.font_width =   FILL("788 ", 32) + "250 333 420 500 500 "
                                + "833 778 333 333 333 500 675 250 333 250 278 "
                                + "500 500 500 500 500 500 500 500 500 500 333 "
                                + "333 675 675 675 500 920 611 611 667 722 611 "
                                + "611 722 722 333 444 667 556 833 667 722 611 "
                                + "500 559 722 611 833 611 556 556 389 278 389 "
                                + "422 500 333 500 500 444 500 444 278 500 500 "
                                + "278 278 444 278 722 500 500 500 500 389 389 "
                                + "278 500 444 667 444 444 389 400 278 400 541 "
                                + "389 500 500 167 500 500 500 500 214 556 500 "
                                + "333 333 500 500 500 500 500 250 523 350 333 "
                                + "556 556 500 889 1000 500 333 333 333 333 333 "
                                + "333 333 333 333 333 333 333 333 889 889 276"
                                + "556 722 944 310 667 278 278 500 667 500".
  L_Object = L_Object + 1.

  CREATE TT_pdf_font.
  ASSIGN TT_pdf_font.font_name  = "Times-Bold"
         TT_pdf_font.font_file  = "PDFBASE14"
         TT_pdf_font.font_afm   = ""
         TT_pdf_font.font_dif   = ""
         TT_pdf_font.font_obj   = L_Object
         TT_pdf_font.font_tag   = "/BF11"
         TT_pdf_font.obj_stream = pdfStream
         TT_pdf_font.font_type  = "VARIABLE"
         TT_pdf_font.font_width =  FILL("788 ", 32) + "250 333 555 500 500 "
                                + "1000 833 333 333 333 500 570 250 333 250 "
                                + "278 500 500 500 500 500 500 500 500 500 500 "
                                + "333 333 570 570 570 500 930 722 667 722 722 "
                                + "667 611 778 778 389 500 778 667 944 722 778 "
                                + "611 778 722 556 667 722 722 1000 722 722 667 "
                                + "333 278 333 581 500 333 500 556 444 556 444 "
                                + "333 500 556 278 333 556 278 833 556 500 556 "
                                + "556 444 389 333 556 500 722 500 500 444 394 "
                                + "220 394 520 333 500 500 167 500 500 500 500 "
                                + "278 500 500 333 333 556 556 500 500 500 250 "
                                + "540 350 333 500 500 500 1000 1000 500 333 333 "
                                + "333 333 333 333 333 333 333 333 333 333 333 "
                                + "1000 1000 300 667 778 1000 330 722 278 278 "
                                + "500 722 556".
  L_Object = L_Object + 1.

  CREATE TT_pdf_font.
  ASSIGN TT_pdf_font.font_name  = "Times-BoldItalic"
         TT_pdf_font.font_file  = "PDFBASE14"
         TT_pdf_font.font_afm   = ""
         TT_pdf_font.font_dif   = ""
         TT_pdf_font.font_obj   = L_Object
         TT_pdf_font.font_tag   = "/BF12"
         TT_pdf_font.obj_stream = pdfStream
         TT_pdf_font.font_type  = "VARIABLE"
         TT_pdf_font.font_width = FILL("788 ", 32) + "250 389 555 500 500 833 "
                                + "778 333 333 333 500 570 250 333 250 278 500 "
                                + "500 500 500 500 500 500 500 500 500 333 333 "
                                + "570 570 570 500 832 667 667 667 722 667 667 "
                                + "722 778 389 500 667 611 889 722 722 611 722 "
                                + "667 556 611 722 667 889 667 611 611 333 278 "
                                + "333 570 500 333 500 500 444 500 444 333 500 "
                                + "556 278 278 500 278 778 556 500 500 500 389 "
                                + "278 556 444 667 500 444 389 348 220 348 570 "
                                + "389 500 500 167 500 500 500 500 278 500 500 "
                                + "333 333 556 556 500 500 500 250 500 350 333 "
                                + "500 500 500 1000 1000 500 333 333 333 333 333 "
                                + "333 333 333 333 333 333 333 333 1000 944 266 "
                                + "611 722 944 300 722 278 278 500 722 500".

  /* ---- End of Times Roman Fonts ---- */

  
  
  /* ---- Beginning of Symbol Font ---- */

  /* Create Associated Object */
  L_Object = L_Object + 1.
  CREATE TT_pdf_font.
  ASSIGN TT_pdf_font.font_name  = "Symbol"
         TT_pdf_font.font_file  = "PDFBASE14"
         TT_pdf_font.font_afm   = ""
         TT_pdf_font.font_dif   = ""
         TT_pdf_font.font_obj   = L_Object
         TT_pdf_font.font_tag   = "/BF13"
         TT_pdf_font.obj_stream = pdfStream
         TT_pdf_font.font_type  = "FIXED"
         TT_pdf_font.font_width = "600".
  
  /* ---- End of Symbol Font ---- */

  /* ---- Beginning of ZapfDingbats Font ---- */

  /* Create Associated Object */
  L_Object = L_Object + 1.
  CREATE TT_pdf_font.
  ASSIGN TT_pdf_font.font_name  = "ZapfDingbats"
         TT_pdf_font.font_file  = "PDFBASE14"
         TT_pdf_font.font_afm   = ""
         TT_pdf_font.font_dif   = ""
         TT_pdf_font.font_obj   = L_Object
         TT_pdf_font.font_tag   = "/BF14"
         TT_pdf_font.obj_stream = pdfStream
         TT_pdf_font.font_type  = "FIXED"
         TT_pdf_font.font_width = "600".
  
  /* ---- End of Symbol Font ---- */

  pdf_inc_ObjectSequence = L_Object.

END. /* pdf_LoadBase14 */

PROCEDURE pdf_Encoding: /* PRIVATE */
  DEFINE INPUT PARAMETER pdfStream  AS CHARACTER NO-UNDO.

  ObjectSequence(pdfStream,4, "Encoding", 0).
  PUT STREAM S_pdf_inc UNFORMATTED
      "4 0 obj" {&pdfSKIP}
      "<<" {&pdfSKIP}
      "/Type /Encoding" {&pdfSKIP}
      "/BaseEncoding /WinAnsiEncoding" {&pdfSKIP}
      ">>" {&pdfSKIP}
      "endobj" {&pdfSKIP}.

END. /* pdf_Encoding */

PROCEDURE pdf_Resources: /* PRIVATE */
  DEFINE INPUT PARAMETER pdfStream  AS CHARACTER NO-UNDO.

  DEFINE VARIABLE L_fontobj AS CHARACTER NO-UNDO.

  FOR EACH TT_pdf_font WHERE TT_pdf_font.obj_stream = pdfStream 
      AND TT_pdf_font.font_type <> "EXTERNAL" 
      AND TT_pdf_font.font_obj <> 0 NO-LOCK:
    ASSIGN L_fontobj = L_fontobj + " " + TT_pdf_font.font_tag + " "
                     + STRING(TT_pdf_font.font_obj) + " 0 R".
  END.

  Vuse-font = "/BF1".

  ObjectSequence(pdfStream, pdf_inc_ObjectSequence + 1, "Resource", 0).
  pdf-Res-Object = pdf_inc_ObjectSequence.
  PUT STREAM S_pdf_inc UNFORMATTED
      pdf_inc_ObjectSequence " 0 obj" {&pdfSKIP}
      "<<" {&pdfSKIP}
      "  /Font << " L_fontobj " >>" {&pdfSKIP}
      "  /ProcSet [ /PDF /Text /ImageB /ImageC /ImageI ]" {&pdfSKIP}
      "  /XObject << " {&pdfSKIP}.

  /* Output Image Definitions */
  FOR EACH TT_pdf_image WHERE TT_pdf_image.obj_stream = pdfStream:
    PUT STREAM S_pdf_inc UNFORMATTED
        TT_pdf_image.image_tag " " TT_pdf_image.image_obj " 0 R " {&pdfSKIP}.
  END.

  /* Output External Pages */
  FOR EACH TT_pdf_external WHERE TT_pdf_external.obj_stream = pdfStream:
    PUT STREAM S_pdf_inc UNFORMATTED
        TT_pdf_external.ext_tag " " TT_pdf_external.ext_obj " 0 R " {&pdfSKIP}.
  END.

  PUT STREAM S_pdf_inc UNFORMATTED
      " >>" {&pdfSKIP}
      ">>" {&pdfSKIP}
      "endobj" {&pdfSKIP}.

END. /* pdf_Resources */

PROCEDURE pdf_Content: /* PRIVATE */
  DEFINE INPUT PARAMETER pdfStream  AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfEncrypt AS LOGICAL   NO-UNDO.

  DEFINE BUFFER B_TT_pdf_Stream FOR TT_pdf_Stream.

  DEFINE VARIABLE L_font        AS CHARACTER NO-UNDO.
  DEFINE VARIABLE L_StreamKey   AS CHARACTER NO-UNDO.

  DEFINE VARIABLE L_SourceMem   AS MEMPTR NO-UNDO.
  DEFINE VARIABLE L_DestMem     AS MEMPTR NO-UNDO.
  DEFINE VARIABLE L_EncryptMem  AS MEMPTR NO-UNDO.
  DEFINE VARIABLE L_EncryptKey  AS MEMPTR NO-UNDO.

  DEFINE VARIABLE L_Loop        AS INTEGER NO-UNDO.
  DEFINE VARIABLE L_Page        AS INTEGER NO-UNDO.
  DEFINE VARIABLE L_Stat        AS INTEGER NO-UNDO.

  DEFINE VARIABLE vSource       AS MEMPTR NO-UNDO.
  DEFINE VARIABLE vDest         AS MEMPTR NO-UNDO.
  
  DEFINE VARIABLE vDestLen      AS INTEGER NO-UNDO.

  DEFINE VARIABLE L_justify     AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE L_fontsize    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE L_textWidth   AS INTEGER    NO-UNDO.
  DEFINE VARIABLE L_fromX       AS INTEGER    NO-UNDO.
  DEFINE VARIABLE L_toX         AS INTEGER    NO-UNDO.
  DEFINE VARIABLE L_textString  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE L_newtextX    AS INTEGER    NO-UNDO.
  
  PUBLISH "MaxPDFPage" (INPUT pdfStream, INPUT pdf_Page(pdfStream)).

  /* Produce each Page one at a time */
  DO L_Loop = 1 TO pdf_Page(pdfStream):
    L_page = L_page + 1.

    PUBLISH "BuildPDFPage" (INPUT pdfStream, INPUT L_Page).

    FIND FIRST TT_pdf_page 
         WHERE TT_pdf_page.obj_stream = pdfStream
           AND TT_pdf_page.page_nbr = L_Page NO-LOCK NO-ERROR.
    IF TT_pdf_page.UseTotalPages THEN
      RUN ChangePageText
          (pdfStream,
           L_Page,
           "@@TOTALPages-" + pdfStream,
           pdf_Page(pdfStream)).
    IF TT_pdf_page.UsePageNo THEN
      RUN ChangePageText
          (pdfStream,
           L_Page,
           "@@PageNo-" + pdfStream,
           L_Page).

    IF CAN-FIND(FIRST TT_pdf_FillTxt
                WHERE TT_pdf_FillTxt.obj_Stream = pdfStream
                  AND TT_pdf_FillTxt.page_nbr   = L_Page NO-LOCK) THEN DO:
      RUN ProcessFillText
          (pdfStream,
           L_Page).
    END.

    /* Start Page Definition */
    RUN pdf_Definition (pdfStream ,L_page, IF L_page = 1 THEN FALSE ELSE TRUE).

    /* PDFinclude Content Stream */
    ObjectSequence(pdfStream, pdf_inc_ObjectSequence + 1, "Content - PDFinclude", 0).

    PUT STREAM S_pdf_inc UNFORMATTED
        pdf_inc_ObjectSequence " 0 obj" {&pdfSKIP}
        "<<" {&pdfSKIP}
        "/Length " (pdf_inc_ObjectSequence + 1) " 0 R" {&pdfSKIP}.

    /* Only add the Filter if 'Compress' is turned on */
    IF pdf_get_parameter(pdfStream,"Compress") = "TRUE" THEN
      PUT STREAM S_pdf_inc UNFORMATTED
          "/Filter /FlateDecode" {&pdfSKIP}.

    PUT STREAM S_pdf_inc UNFORMATTED
        ">>" {&pdfSKIP}
        "stream" {&pdfSKIP} .

    pdf-Stream-Start = SEEK(S_pdf_inc).

    SET-SIZE(mContent) = 1.
      
    FILE-INFO:FILE-NAME = SESSION:TEMP-DIR + TT_pdf_stream.obj_UniqueID  + "-Content-" + STRING(L_Page) + ".txt".

    SET-SIZE(mContent) = 0.
    SET-SIZE(mContent) = GetFileSize().
      
    RUN GetFileContent(INPUT-OUTPUT mContent).

    mHolder = mContent.

    /** Compression happens before Encryption **/
    IF pdf_get_parameter(pdfStream,"Compress") = "TRUE" THEN DO:
      L_Stat = compressbuffer( mHolder,
                               INPUT-OUTPUT vDest,
                               OUTPUT vDestLen).

      SET-SIZE(mHolder) = 0.
      SET-SIZE(mHolder) = GET-SIZE(vDest).
      mHolder = Vdest.
    END.
      

    RUN OutputMemPtr(pdfStream, 
                     FALSE, 
                     TT_pdf_stream.obj_UniqueID, 
                     pdf_inc_ObjectSequence, 
                     mHolder).

    /*** igc99
    IF NOT pdfEncrypt THEN
      RUN OutputMemPtr( mHolder ).

    ELSE DO:
      SET-SIZE(L_EncryptKey) = 10.
      RUN GetEncryptKey /* IN h_PDF-Encrypt */
          (INPUT  TT_pdf_stream.obj_UniqueID,
           INPUT  pdf_inc_ObjectSequence,
           INPUT  0,
           INPUT  pdf-EncryptKeyMemPtr,
           OUTPUT L_EncryptKey).


      RUN EncryptContent /* IN h_PDF-Encrypt */
                         (INPUT  L_EncryptKey,
                          INPUT  mHolder,
                          OUTPUT L_EncryptMem).

      RUN OutputMemPtr (L_EncryptMem).

      SET-SIZE(L_EncryptMem) = 0.
      SET-SIZE(L_EncryptKey) = 0.

    END. /* Encrypted Content */
    ***/

    SET-SIZE(mHolder) = 0.
    SET-SIZE(mContent) = 0.

    SET-SIZE(vSource) = 0.
    SET-SIZE(vDest)   = 0.
      
    /* End Page Definition */
    pdf-Stream-End = SEEK(S_pdf_inc).
    PUT STREAM S_pdf_inc UNFORMATTED
        "endstream" {&pdfSKIP}
        "endobj" {&pdfSKIP}.

    /* Output Length */
    RUN pdf_length (pdfStream, pdf-Stream-End - pdf-Stream-Start).

  END. /* loop for each page */

  OS-DELETE VALUE(SESSION:TEMP-DIR + TT_pdf_stream.obj_UniqueID + "-C.txt").

  SET-SIZE(mHolder) = 0.
  SET-SIZE(mContent) = 0.
  SET-SIZE(vSource) = 0.
  SET-SIZE(vDest)   = 0.
  
  /* Load Bookmarks */
  IF CAN-FIND( FIRST TT_pdf_bookmark WHERE TT_pdf_bookmark.obj_Stream = pdfStream NO-LOCK)
  THEN RUN pdf_Load_Bookmarks (pdfStream).

  FIND FIRST B_TT_pdf_stream 
       WHERE B_TT_pdf_Stream.obj_stream = pdfStream NO-ERROR.
  ASSIGN B_TT_pdf_Stream.obj_DoingText    = FALSE
         B_TT_pdf_Stream.obj_DoingGraphic = FALSE.
  /* This will set the PDF Page to the max actual Page number */
  RUN pdf_set_Page(pdfStream, L_page).

  RUN pdf_catalog (pdfStream).
  RUN pdf_ListPages (pdfStream).

END. /* pdf_Content */

PROCEDURE pdf_definition: /* PRIVATE */
  DEFINE INPUT PARAMETER pdfStream      AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER P_page         AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER P_incl-annot   AS LOGICAL NO-UNDO.

  FIND TT_pdf_page WHERE TT_pdf_page.obj_stream = pdfStream
                     AND TT_pdf_page.page_nbr   = p_Page NO-ERROR.
  IF NOT AVAIL TT_pdf_page THEN DO:
    MESSAGE "Error occurred - could not find TT_pdf_page for Page=" p_Page
            VIEW-AS ALERT-BOX.
    RETURN.
  END.

  ObjectSequence (pdfStream, pdf_inc_ObjectSequence + 1, "PageDefinition", p_Page).
  PUT STREAM S_pdf_inc UNFORMATTED
      pdf_inc_ObjectSequence  " 0 obj" {&pdfSKIP}
      "<<" {&pdfSKIP}
      "/Type /Page" {&pdfSKIP}
      "/Parent 3 0 R" {&pdfSKIP}
      "/Resources " pdf-Res-Object " 0 R" {&pdfSKIP}
      "/Contents ".
    
  PUT STREAM S_pdf_inc UNFORMATTED
      (pdf_inc_ObjectSequence + 1) " 0 R" {&pdfSKIP}.

  PUT STREAM S_pdf_inc UNFORMATTED
      "/Rotate 0" /* STRING(TT_pdf_page.page_rotate) */ {&pdfSKIP}.

  IF TT_pdf_page.page_use <> 0 THEN DO:
    FIND FIRST TT_pdf_external 
         WHERE TT_pdf_external.obj_stream = pdfStream
           AND TT_pdf_external.page_id    = tt_pdf_page.page_use NO-ERROR.
    IF AVAIL TT_pdf_External AND TT_pdf_External.ext_rotate = 90 THEN
      PUT STREAM S_pdf_inc UNFORMATTED
          "/MediaBox [ 0 0 "  TT_pdf_Page.page_height " " TT_pdf_page.page_width
          " ]" {&pdfSKIP}.
    ELSE
      PUT STREAM S_pdf_inc UNFORMATTED
          "/MediaBox [ 0 0 "  TT_pdf_Page.page_width " " TT_pdf_page.page_height
          "]" {&pdfSKIP}.
  END.

  ELSE 
    PUT STREAM S_pdf_inc UNFORMATTED
        "/MediaBox [ 0 0 "  TT_pdf_Page.page_width " " TT_pdf_page.page_height
        "]" {&pdfSKIP}.

  IF TT_pdf_page.page_crop <> "" THEN
    PUT STREAM S_pdf_inc UNFORMATTED
        "/CropBox [ " + TT_pdf_page.page_crop + " ]" {&pdfSKIP}.

  PUT STREAM S_pdf_inc UNFORMATTED
      "/Annots [ " {&pdfSKIP}.

  /* Output Link Definitions */
  FOR EACH TT_pdf_annot WHERE TT_pdf_annot.obj_stream = pdfStream
                          AND TT_pdf_annot.annot_page  = p_Page:
    PUT STREAM S_pdf_inc UNFORMATTED
        TT_pdf_annot.annot_obj " 0 R" {&pdfSKIP}.
  END.

  PUT STREAM S_pdf_inc UNFORMATTED
      " ]" {&pdfSKIP}
      ">>" {&pdfSKIP}
      "endobj" {&pdfSKIP}.

END. /* pdf_definition */

PROCEDURE pdf_Length: /* PRIVATE */

  DEFINE INPUT PARAMETER pdfStream  AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER P_length   AS INTEGER NO-UNDO.

  ObjectSequence(pdfStream, pdf_inc_ObjectSequence + 1, "Length", 0).
  PUT STREAM S_pdf_inc UNFORMATTED
      pdf_inc_ObjectSequence " 0 obj" {&pdfSKIP}
      P_length {&pdfSKIP}
      "endobj" {&pdfSKIP}.

END. /* pdf_Length */

PROCEDURE pdf_Catalog : /* PRIVATE */
  DEFINE INPUT PARAMETER pdfStream  AS CHARACTER NO-UNDO.

  ObjectSequence(pdfStream, 2, "Catalog", 0).
  PUT STREAM S_pdf_inc UNFORMATTED
      "2 0 obj" {&pdfSKIP}
      "<<" {&pdfSKIP}
      "/Type /Catalog" {&pdfSKIP}
      "/Pages 3 0 R" {&pdfSKIP}.

  IF pdf_OutlinesDict <> 0 THEN
    PUT STREAM S_pdf_inc UNFORMATTED
      "/Outlines " pdf_OutlinesDict " 0 R" {&pdfSKIP}.

  /* Determine PageMode */
  PUT STREAM S_pdf_inc UNFORMATTED
       "/PageMode /" TRIM(pdf_get_parameter(pdfStream,"PageMode")) {&pdfSkip}.

  /* Determine PageLayout */
  PUT STREAM S_pdf_inc UNFORMATTED
       "/PageLayout /" 
       TRIM(pdf_get_parameter(pdfStream,"PageLayout")) {&pdfSkip}.

  /* Do Viewer Preferences */
  PUT STREAM s_pdf_inc UNFORMATTED
    "/ViewerPreferences << ".

  IF TRIM(pdf_get_parameter(pdfStream,"HideToolbar")) = "TRUE" THEN
    PUT STREAM s_pdf_inc UNFORMATTED
      "/HideToolbar true ".

  IF TRIM(pdf_get_parameter(pdfStream,"HideMenubar")) = "TRUE" THEN
    PUT STREAM s_pdf_inc UNFORMATTED
      "/HideMenubar true ".

  IF TRIM(pdf_get_parameter(pdfStream,"HideWindowUI")) = "TRUE" THEN
    PUT STREAM s_pdf_inc UNFORMATTED
      "/HideWindowUI true ".

  IF TRIM(pdf_get_parameter(pdfStream,"FitWindow")) = "TRUE" THEN
    PUT STREAM s_pdf_inc UNFORMATTED
      "/FitWindow true ".

  IF TRIM(pdf_get_parameter(pdfStream,"CenterWindow")) = "TRUE" THEN
    PUT STREAM s_pdf_inc UNFORMATTED
      "/CenterWindow true ".

  IF TRIM(pdf_get_parameter(pdfStream,"DisplayDocTitle")) = "TRUE" THEN
    PUT STREAM s_pdf_inc UNFORMATTED
      "/DisplayDocTitle true ".
 
  PUT STREAM s_pdf_inc UNFORMATTED
      " >>" {&pdfSKIP}.

  /* end of Viewer Preferences */

  PUT STREAM S_pdf_inc UNFORMATTED
      ">>" {&pdfSKIP}
      "endobj" {&pdfSKIP}.

END. /* pdf_Catalog */

PROCEDURE pdf_ListPages : /* PRIVATE */

  DEFINE INPUT PARAMETER pdfStream  AS CHARACTER NO-UNDO.

  ObjectSequence(pdfStream, 3, "Pages", 0).

  PUT STREAM S_pdf_inc UNFORMATTED
      "3 0 obj" {&pdfSKIP}
      "<<" {&pdfSKIP}
      "/Type /Pages" {&pdfSKIP}
      "/Count " pdf_Page(pdfStream) {&pdfSKIP}
      /* igc - don't need a default page size as it is defined in each page
               definition
      "/MediaBox [ 0 0 " pdf_PageWidth(pdfStream)
      " " pdf_PageHeight(pdfStream) " ]" {&pdfSKIP} */
      "/Kids [ " {&pdfSKIP}.

  FOR EACH TT_pdf_object WHERE TT_pdf_object.obj_stream = pdfStream
                           AND TT_pdf_object.obj_desc = "PageDefinition" NO-LOCK:
    PUT STREAM S_pdf_inc UNFORMATTED
        TT_pdf_object.obj_nbr " 0 R " {&pdfSKIP}.
  END. /* Display Pages */

  PUT STREAM S_pdf_inc UNFORMATTED
      "]"  {&pdfSKIP}
      ">>" {&pdfSKIP}
      "endobj" {&pdfSKIP}.

END. /* pdf_ListPages */

PROCEDURE pdf_xref : /* PRIVATE */

  DEFINE INPUT PARAMETER pdfStream  AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfEncrypt AS LOGICAL   NO-UNDO.

  DEFINE VARIABLE L_ctr AS INTEGER NO-UNDO.
  DEFINE VARIABLE L_obj AS INTEGER NO-UNDO.

  ObjectSequence(pdfStream, 0, "Xref", 0).
  FOR EACH TT_pdf_object WHERE TT_pdf_object.obj_stream = pdfStream:
    L_ctr = L_ctr + 1.
  END.

  /* Get the Xref start point */
  pdf-Stream-Start = SEEK(S_pdf_inc).

  PUT STREAM S_pdf_inc UNFORMATTED
      "xref" {&pdfSKIP}
      "0 " L_ctr {&pdfSKIP}.

  FOR EACH TT_pdf_object WHERE TT_pdf_object.obj_stream = pdfStream
      BREAK BY TT_pdf_object.obj_nbr:
    IF FIRST( TT_pdf_object.obj_nbr) THEN
      PUT STREAM S_pdf_inc CONTROL
          "0000000000"
          " "
          TT_pdf_object.gen_nbr " "
          TT_pdf_object.obj_type CHR(13) CHR(10).
    ELSE DO:
      TT_pdf_object.obj_off = TT_pdf_object.obj_off - 1.
      PUT STREAM S_pdf_inc CONTROL
          STRING(TT_pdf_object.obj_off, "9999999999")
          " "
          STRING(TT_pdf_object.gen_nbr, "99999") " "
          TT_pdf_object.obj_type CHR(13) CHR(10).
    END.
  END. /* each Object */

  FIND TT_pdf_stream WHERE TT_pdf_stream.obj_stream = pdfStream NO-LOCK NO-ERROR.

  FIND LAST TT_pdf_object WHERE TT_pdf_object.obj_stream = pdfStream NO-LOCK NO-ERROR.

  PUT STREAM S_pdf_inc UNFORMATTED
      "trailer" {&pdfSKIP}
      "<<" {&pdfSKIP}
      "/Size " (TT_pdf_object.obj_nbr + 1) {&pdfSKIP}
      "/Root 2 0 R" {&pdfSKIP}
      "/Info 1 0 R" {&pdfSKIP}.

  IF pdfEncrypt THEN DO:
    PUT STREAM S_pdf_inc UNFORMATTED
        "/Encrypt " TT_pdf_stream.obj_EncryptDict " 0 R" {&pdfSKIP}
        "/ID [<" TT_pdf_stream.obj_ID "><" TT_pdf_stream.obj_ID ">]" {&pdfSKIP}.
  END.

  PUT STREAM S_pdf_inc UNFORMATTED
      ">>" {&pdfSKIP}
      "startxref" {&pdfSKIP}
      pdf-Stream-Start {&pdfSKIP}
      "%%EOF" {&pdfSKIP}.

END. /* pdf_xref */

PROCEDURE pdf_Fonts: /* PRIVATE */

  DEFINE INPUT PARAMETER pdfStream  AS CHARACTER NO-UNDO.

  FOR EACH TT_pdf_font WHERE TT_pdf_font.obj_stream = pdfStream
                         AND TT_pdf_font.font_file  = "PDFBASE14"
      BY TT_pdf_font.font_obj:

    ObjectSequence(pdfStream, TT_pdf_font.font_obj, "Font", 0).
    PUT STREAM S_pdf_inc UNFORMATTED
        TT_pdf_font.FONT_obj " 0 obj" {&pdfSKIP}
        "<<" {&pdfSKIP}
        "/Type /Font" {&pdfSKIP}
        "/Subtype /Type1" {&pdfSKIP}
        "/Name " TT_pdf_font.font_tag {&pdfSKIP}.

    IF TT_pdf_font.font_name= "Symbol" 
    OR TT_pdf_font.font_name = "ZapfDingbats" THEN
      PUT STREAM S_pdf_inc UNFORMATTED
          "/Encoding << /BaseEncoding /StandardEncoding >>" {&pdfSKIP}.
    ELSE
      PUT STREAM S_pdf_inc UNFORMATTED
        "/Encoding 4 0 R" {&pdfSKIP}.

      PUT STREAM S_pdf_inc UNFORMATTED
        "/BaseFont /" TT_pdf_font.font_name {&pdfSKIP}
        ">>" {&pdfSKIP}
        "endobj" {&pdfSKIP}.
  END.

END. /* pdf_Fonts */

PROCEDURE pdf_Load_fonts : /* PRIVATE */
  DEFINE INPUT PARAMETER pdfStream  AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfEncrypt AS LOGICAL   NO-UNDO.

  DEFINE VARIABLE L_data  AS CHARACTER NO-UNDO.
  DEFINE VARIABLE L_count AS INTEGER NO-UNDO.
  DEFINE VARIABLE L_start AS INTEGER NO-UNDO.
  DEFINE VARIABLE L_end   AS INTEGER NO-UNDO.
  DEFINE VARIABLE L_Size  AS INTEGER NO-UNDO.

  DEFINE VARIABLE mFont         AS MEMPTR NO-UNDO.
  DEFINE VARIABLE m_EncryptKey  AS MEMPTR NO-UNDO.

  FOR EACH TT_pdf_font WHERE TT_pdf_font.obj_stream = pdfStream
                         AND TT_pdf_font.font_file <> "PDFBASE14"
                         AND TT_pdf_font.font_File <> "EXTERNAL":


    IF TT_pdf_font.font_embed THEN DO:
      FILE-INFO:FILE-NAME = TT_pdf_font.font_file.
      L_Size = GetFileSize().

      IF pdf_get_parameter(pdfStream,"Compress") = "TRUE" THEN DO:

        compressfile( TT_pdf_font.font_file,
                      SESSION:TEMP-DIR + TT_pdf_font.font_name + ".cmp").

        FILE-INFO:FILE-NAME = SESSION:TEMP-DIR + TT_pdf_font.font_name + ".cmp".
      END.
    END. /* Embed Font */

    /* igc - Added Sept 10, 2002 */
    ObjectSequence(pdfStream, pdf_inc_ObjectSequence + 1, "FontDescriptor", 0).
    TT_pdf_font.font_descr  = pdf_inc_ObjectSequence.

    /* Output the Font Descriptor */
    PUT STREAM S_pdf_inc UNFORMATTED
        TT_pdf_font.font_descr " 0 obj" {&pdfSKIP}

        "<< /Type /FontDescriptor" {&pdfSKIP}
        "   /Ascent " TT_pdf_font.afm_Ascender {&pdfSKIP}
        "   /Descent " TT_pdf_font.afm_Descender {&pdfSKIP}
        "   /CapHeight " TT_pdf_font.afm_Ascender {&pdfSKIP}
        "   /Flags " TT_pdf_font.afm_Flags {&pdfSKIP}
        "   /FontBBox [" TT_pdf_font.afm_FontBBox "]" {&pdfSKIP}
        "   /FontName /" TT_pdf_font.font_name {&pdfSKIP}
        "   /ItalicAngle " TT_pdf_font.afm_ItalicAngle  {&pdfSKIP}.

    IF TT_pdf_font.font_embed THEN
      PUT STREAM S_pdf_inc UNFORMATTED
        "   /FontFile2 " (TT_pdf_font.font_descr + 3) " 0 R" {&pdfSKIP}.

    PUT STREAM S_pdf_inc UNFORMATTED
        ">>" {&pdfSKIP}
        "endobj" {&pdfSKIP}.

    /* igc - Added Sept 10, 2002 */
    ObjectSequence(pdfStream, pdf_inc_ObjectSequence + 1, "Font", 0).
    TT_pdf_font.font_obj    = pdf_inc_ObjectSequence.

    PUT STREAM S_pdf_inc UNFORMATTED
        TT_pdf_font.font_obj " 0 obj" {&pdfSKIP}
        "<<" {&pdfSKIP}
        "/Type /Font" {&pdfSKIP}
        "/Subtype /TrueType" {&pdfSKIP}
        "/FirstChar " TT_pdf_font.afm_FirstChar {&pdfSKIP}
        "/LastChar " TT_pdf_font.afm_LastChar {&pdfSKIP}
        "/Widths [ " TT_pdf_font.afm_widths " ]" {&pdfSKIP}
        "/Encoding " (pdf_inc_ObjectSequence + 1) " 0 R" {&pdfSKIP}
        "/BaseFont /" TT_pdf_font.font_name {&pdfSKIP}
        "/FontDescriptor " TT_pdf_font.font_descr " 0 R" {&pdfSKIP}
        ">>" {&pdfSKIP}
        "endobj" {&pdfSKIP}.

    /* igc - Aug 28 - Added this code to allow for remapping of characters */
    ObjectSequence(pdfStream, pdf_inc_ObjectSequence + 1, "Encoding", 0).
    TT_pdf_font.font_encoding  = pdf_inc_ObjectSequence.

    PUT STREAM S_pdf_inc UNFORMATTED
        TT_pdf_font.font_encoding " 0 obj" {&pdfSKIP}
        "<< /Type /Encoding" {&pdfSKIP}
        "/BaseEncoding /WinAnsiEncoding" {&pdfSKIP}.

    IF TT_pdf_font.font_dif <> ""
    OR CAN-FIND(FIRST TT_pdf_diff
                WHERE TT_pdf_diff.obj_stream = pdfStream
                  AND TT_pdf_diff.font_name  = TT_pdf_font.font_name NO-LOCK)
    THEN DO:
      PUT STREAM S_pdf_inc UNFORMATTED "/Differences [ " {&pdfSKIP}.

      IF TT_pdf_font.font_dif <> "" THEN DO:
        INPUT STREAM S_pdf_inp FROM VALUE(SEARCH(TT_pdf_font.font_dif)) NO-CONVERT NO-MAP NO-ECHO.

          REPEAT:
            IMPORT STREAM S_pdf_inp UNFORMATTED L_data.
            PUT STREAM S_pdf_inc UNFORMATTED
                L_Data {&pdfSKIP}.
          END.

        INPUT STREAM S_pdf_inp CLOSE.
      END. /* Differences File */

      FOR EACH TT_pdf_diff WHERE TT_pdf_diff.obj_stream = pdfStream:
        PUT STREAM S_pdf_inc UNFORMATTED
            TT_pdf_diff.char_num
            " "
            TT_pdf_diff.PS_name {&pdfSKIP}.
      END.

      PUT STREAM S_pdf_inc UNFORMATTED "]". /* Close the Differences Array */

    END. /* Font Difference File exists */

    PUT STREAM S_pdf_inc UNFORMATTED
        ">>" {&pdfSKIP}
        "endobj" {&pdfSKIP}.

    /* igc - Aug 28 - end of character mapping code */

    IF TT_pdf_font.font_embed THEN DO:

      /* igc - Added Sept 10, 2002 */
      ObjectSequence(pdfStream, pdf_inc_ObjectSequence + 1, "FontStream", 0).
      TT_pdf_font.font_stream = pdf_inc_ObjectSequence.

      /* Display Embedded Font Stream */
      PUT STREAM S_pdf_inc UNFORMATTED
          TT_pdf_font.font_stream " 0 obj" {&pdfSKIP}
          "<<" {&pdfSKIP}
          "/Length " (TT_pdf_font.font_stream + 1) " 0 R" {&pdfSKIP}
          "/Length1 " L_Size {&pdfSKIP}.
      
      IF pdf_get_parameter(pdfStream,"Compress") = "TRUE" THEN
        PUT STREAM S_pdf_inc UNFORMATTED
            "/Filter /FlateDecode" {&pdfSKIP}.

      PUT STREAM S_pdf_inc UNFORMATTED
          ">>" {&pdfSKIP}
          "stream" {&pdfSKIP}.

      /* Get PDF Stream Start Offset */
      L_start = SEEK(S_pdf_inc).

      SET-SIZE(mFont) = GetFileSize(). /* Initialize Memory */
      
      RUN GetFileContent(INPUT-OUTPUT mFont).

      /*** igc99
      IF pdfEncrypt THEN DO:
        SET-SIZE(m_EncryptKey) = 10.
        RUN GetEncryptKey /* IN h_PDF-Encrypt */
            (INPUT  TT_pdf_stream.obj_UniqueID,
             INPUT  pdf_inc_ObjectSequence,
             INPUT  0,
             INPUT  pdf-EncryptKeyMemPtr,
             OUTPUT m_EncryptKey).

        RUN EncryptContent /* IN h_PDF-Encrypt */
                           (INPUT  m_EncryptKey,
                            INPUT  mFont,
                            OUTPUT mFont).

      END.
      ***/

      RUN OutputMemPtr(pdfStream,
                       FALSE,
                       TT_pdf_stream.obj_UniqueID, 
                       pdf_inc_ObjectSequence, 
                       mFont).

      /* RUN OutputMemPtr (mFont). */

      SET-SIZE(mFont) = 0. /* Release memory */

      L_end = SEEK(S_pdf_inc).

      PUT STREAM S_pdf_inc UNFORMATTED
          {&pdfSKIP} {&pdfSKIP} "endstream" {&PDFSKIP}
                    "endobj" {&pdfSKIP}.

      /* igc - Added Sept 10, 2002 */
      ObjectSequence(pdfStream, pdf_inc_ObjectSequence + 1, "FontLength", 0).
      TT_pdf_font.font_len = pdf_inc_ObjectSequence.

      /* Put out Length */
      PUT STREAM S_pdf_inc UNFORMATTED
          TT_pdf_font.font_len " 0 obj" {&pdfSKIP}
          "  " (L_end - L_start) {&pdfSKIP}
          "endobj" {&pdfSKIP}.

      SET-SIZE(m_EncryptKey) = 0.

      /* Remove Compressed File */
      IF pdf_get_parameter(pdfStream,"Compress") = "TRUE" THEN
        OS-DELETE VALUE(SESSION:TEMP-DIR + TT_pdf_font.font_name + ".cmp").

    END. /* Embed */

  END. /* each TTfont */

END. /* pdf_Load_fonts */

PROCEDURE pdf_ParseAFMFile: /* PRIVATE */
  DEFINE OUTPUT PARAMETER P_afm_ItalicAngle  AS INTEGER   NO-UNDO.
  DEFINE OUTPUT PARAMETER P_afm_Ascender     AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER P_afm_Descender    AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER P_afm_FontBBox     AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER P_afm_FirstChar    AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER P_afm_LastChar     AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER P_afm_Widths       AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER P_afm_IsFixedPitch AS CHARACTER NO-UNDO INIT "0".
  DEFINE OUTPUT PARAMETER P_afm_flags        AS CHARACTER NO-UNDO.

  DEFINE VARIABLE L_data  AS CHARACTER NO-UNDO.
  DEFINE VARIABLE L_key   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE L_flag  AS CHARACTER NO-UNDO
         INIT "00000000000000000000000000100010".
  /* Bit 6 (above) is set to identify NonSymbolic Fonts -- or Fonts that use
     the Standard Latin Character Set */

  DEFINE VARIABLE L_int   AS INTEGER NO-UNDO.
  DEFINE VARIABLE L_Loop  AS INTEGER NO-UNDO.
  DEFINE VARIABLE L_exp   AS INTEGER NO-UNDO.

  ASSIGN P_afm_ItalicAngle  = 0
         P_afm_Descender    = ""
         P_afm_FontBBox     = ""
         P_afm_IsFixedPitch = ""
         P_afm_FirstChar    = ""
         P_afm_LastChar     = ""
         P_afm_Widths       = "".

  INPUT STREAM S_pdf_inp FROM VALUE(SEARCH(TT_pdf_font.font_afm)) BINARY NO-CONVERT NO-MAP NO-ECHO.

    REPEAT:
      IMPORT STREAM S_pdf_inp UNFORMATTED L_data.
      L_Key = ENTRY(1, L_data, " ") NO-ERROR.
      IF ERROR-STATUS:ERROR THEN NEXT.

      CASE L_key:
        WHEN "ItalicAngle" THEN
          P_afm_ItalicAngle = INT(ENTRY( 2, L_data, " ")) NO-ERROR.

        WHEN "Ascender" THEN
          P_afm_Ascender = ENTRY( 2, L_data, " ") NO-ERROR.

        WHEN "Descender" THEN
          P_afm_Descender = ENTRY( 2, L_data, " ") .

        WHEN "FontBBox" THEN
          ASSIGN P_afm_FontBBox = REPLACE(L_data,"FontBBox ","").

        WHEN "IsFixedPitch" THEN
          P_afm_IsFixedPitch = IF ENTRY(2,L_data, " ") = "True" THEN "0" ELSE "1".

        WHEN "C" THEN DO:
          IF P_afm_FirstChar = "" THEN
            P_afm_FirstChar = ENTRY(2, L_data, " ") NO-ERROR.

          ASSIGN P_afm_Widths = P_afm_widths + " "
                              + ENTRY(5, L_data, " ") NO-ERROR.

          IF INT(ENTRY(2, L_data, " ")) > 0 THEN
           P_afm_LastChar = ENTRY(2, L_data, " ") NO-ERROR.
        END.
      END CASE.
    END. /* REPEAT */

  INPUT STREAM S_pdf_inp CLOSE.

  /* Determine Font Flags */
  IF P_afm_IsFixedPitch = "0" THEN
    OVERLAY( L_Flag, 32, 1, "CHARACTER") = "1".

  DO L_loop = LENGTH(L_Flag, "character":u) TO 1 BY -1 :
    IF SUBSTR(L_flag,L_loop,1, "character":u) = "1" THEN
      L_int = L_int + EXP(2, L_exp).

    L_exp = L_exp + 1.
  END.

  P_afm_Flags = STRING( L_int ).

END. /* pdf_ParseAFMFile */

PROCEDURE pdf_load_images: /* PRIVATE */

  DEFINE INPUT PARAMETER pdfStream  AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfEncrypt AS LOGICAL   NO-UNDO.

  DEFINE VARIABLE L_count AS INTEGER NO-UNDO.
  DEFINE VARIABLE L_start AS INTEGER NO-UNDO.
  DEFINE VARIABLE L_end   AS INTEGER NO-UNDO.

  DEFINE VARIABLE m_EncryptKey  AS MEMPTR NO-UNDO.
  SET-SIZE(mImage) = 0. /* Un-Initialize Memory */

  FOR EACH TT_pdf_image WHERE TT_pdf_image.obj_stream = pdfStream:

    IF pdf_get_parameter(pdfStream,"Compress") = "TRUE" THEN DO:
      compressfile( TT_pdf_image.image_file,
                    SESSION:TEMP-DIR + TT_pdf_image.image_name + ".cmp").

      FILE-INFO:FILE-NAME = SESSION:TEMP-DIR + TT_pdf_image.image_name + ".cmp".
    END.
    ELSE
      FILE-INFO:FILE-NAME = TT_pdf_image.image_file.

    /* igc - Added Sept 10, 2002 */
    ObjectSequence(pdfStream, pdf_inc_ObjectSequence + 1, "Image", 0).
    TT_pdf_image.image_obj = pdf_inc_ObjectSequence.

    PUT STREAM S_pdf_inc UNFORMATTED
        TT_pdf_image.image_obj " 0 obj" {&pdfSKIP}
        "<<" {&pdfSKIP}
        "/Type /XObject" {&pdfSKIP}
        "/Subtype /Image" {&pdfSKIP}
        "/Name " TT_pdf_image.image_tag {&pdfSKIP}
        "/Width " TT_pdf_image.image_w {&pdfSKIP}
        "/Height " TT_pdf_image.image_h {&pdfSKIP}
        "/BitsPerComponent 8" {&pdfSKIP}
        "/ColorSpace /DeviceRGB" {&pdfSKIP}
        "/Length " GetFileSize() /* (TT_pdf_image.image_obj + 1) " 0 R" */ {&pdfSKIP}
        "/Filter ".
    
    IF pdf_get_parameter(pdfStream,"Compress") = "TRUE" THEN
      PUT STREAM S_pdf_inc UNFORMATTED
          "[/FlateDecode /DCTDecode]" {&pdfSKIP}.
    ELSE
      PUT STREAM S_pdf_inc UNFORMATTED
          "/DCTDecode" {&pdfSKIP}.

    PUT STREAM S_pdf_inc UNFORMATTED
        ">>" {&pdfSKIP}
        "stream" {&pdfSKIP}.

    /* Get PDF Stream Start Offset */
    L_start = SEEK(S_pdf_inc).

    SET-SIZE(mImage) = GetFileSize(). /* Initialize Memory */
    
    RUN GetFileContent(INPUT-OUTPUT mImage).

    /*** igc99
    IF pdfEncrypt THEN DO:

      SET-SIZE(m_EncryptKey) = 10.
      
      RUN GetEncryptKey /* IN h_PDF-Encrypt */
          (INPUT  TT_pdf_stream.obj_UniqueID,
           INPUT  pdf_inc_ObjectSequence,
           INPUT  0,
           INPUT  pdf-EncryptKeyMemPtr,
           OUTPUT m_EncryptKey).

      RUN EncryptContent /* IN h_PDF-Encrypt */
                         (INPUT  m_EncryptKey,
                          INPUT  mImage,
                          OUTPUT mImage).
    END.
    ***/

    RUN OutputMemPtr(pdfStream,
                     FALSE,
                     TT_pdf_stream.obj_UniqueID, 
                     pdf_inc_ObjectSequence, 
                     mImage).

    /* RUN OutputMemPtr (mImage). */

    SET-SIZE(mImage) = 0. /* Release memory */

    L_end = SEEK(S_pdf_inc).

    PUT STREAM S_pdf_inc UNFORMATTED
        {&pdfSKIP} {&pdfSKIP} "endstream" {&pdfSKIP}
                    "endobj" {&pdfSKIP}.

    /* igc - Added Sept 10, 2002 */
    ObjectSequence(pdfStream, pdf_inc_ObjectSequence + 1, "ImageLen", 0).
    TT_pdf_image.image_len = pdf_inc_ObjectSequence.

    PUT STREAM S_pdf_inc UNFORMATTED
        TT_pdf_image.image_len " 0 obj" {&pdfSKIP}
        "  " (L_end - L_start) {&pdfSKIP}
        "endobj" {&pdfSKIP}.

    SET-SIZE(m_EncryptKey) = 0.

    /* Remove Compressed File */
    IF pdf_get_parameter(pdfStream,"Compress") = "TRUE" THEN
      OS-DELETE VALUE(SESSION:TEMP-DIR + TT_pdf_image.image_name + ".cmp").

  END. /* each TT_pdf_image */

END. /* pdf_load_images */

PROCEDURE pdf_load_external: /* PRIVATE */

  DEFINE INPUT PARAMETER pdfStream  AS CHARACTER NO-UNDO.

  DEFINE VARIABLE cContent  AS CHARACTER NO-UNDO.

  DEFINE VARIABLE L_count AS INTEGER NO-UNDO.
  DEFINE VARIABLE L_start AS INTEGER NO-UNDO.
  DEFINE VARIABLE L_end   AS INTEGER NO-UNDO.

  DEFINE VARIABLE L_EncryptMem  AS MEMPTR NO-UNDO.
  DEFINE VARIABLE L_EncryptKey  AS MEMPTR NO-UNDO.

  DEFINE VARIABLE mHolder AS MEMPTR  NO-UNDO.
  DEFINE VARIABLE mTotal  AS MEMPTR  NO-UNDO.
  DEFINE VARIABLE mTemp   AS MEMPTR  NO-UNDO.
  DEFINE VARIABLE mFile   AS MEMPTR  NO-UNDO.
  DEFINE VARIABLE vDest   AS MEMPTR NO-UNDO.

  DEFINE VARIABLE vDestLen  AS INTEGER NO-UNDO.
  DEFINE VARIABLE iPointer  AS INTEGER NO-UNDO.
  DEFINE VARIABLE iFileSize AS INTEGER NO-UNDO.
  DEFINE VARIABLE iMemSize  AS INTEGER NO-UNDO.

  DEFINE VARIABLE L_fontobj AS CHARACTER NO-UNDO.

  DEFINE VARIABLE L_Stat    AS INTEGER NO-UNDO.

  RUN LoadExternalFonts (pdfStream).
  RUN LoadExtGStates (pdfStream).
  RUN LoadColorSpaces (pdfStream).
  RUN LoadFunction (pdfStream).
  RUN LoadShading (pdfStream).
  RUN LoadExternalXObjects (pdfStream). 

  FOR EACH TT_pdf_external WHERE TT_pdf_external.obj_stream = pdfStream:

    L_FontObj = "".
    FOR EACH TT_pdf_font 
       WHERE TT_pdf_font.obj_stream = pdfStream 
         AND TT_pdf_font.ext_page   = TT_pdf_external.ext_page
         AND TT_pdf_font.font_type <> "EXTERNAL" 
         AND TT_pdf_font.font_obj <> 0 NO-LOCK:
      ASSIGN L_fontobj = L_fontobj + " " + TT_pdf_font.font_tag + " "
                       + STRING(TT_pdf_font.font_obj) + " 0 R".
    END.

    FILE-INFO:FILE-NAME = TT_pdf_external.ext_file.

    ObjectSequence(pdfStream, pdf_inc_ObjectSequence + 1, "External", 0).
    TT_pdf_external.ext_obj = pdf_inc_ObjectSequence.

    PUT STREAM S_pdf_inc UNFORMATTED
        TT_pdf_external.ext_obj " 0 obj" {&pdfSKIP}
        "<<" {&pdfSKIP}
        "/Type /XObject" {&pdfSKIP}
        "/Subtype /Form" {&pdfSKIP}
        "/FormType 1" {&pdfSKIP}.

    /* Handle an External Object that was produced in Landscape Mode */
    IF TT_pdf_external.ext_rotate = 90 THEN DO:
      FIND FIRST tt_pdf_page 
           WHERE tt_pdf_page.obj_stream = pdfStream
             AND TT_pdf_Page.page_nbr    = tt_pdf_external.page_id
             NO-LOCK NO-ERROR.
      PUT STREAM S_pdf_inc UNFORMATTED
          "/Matrix [0 -1 1 0 0 " TT_pdf_page.page_width "]" {&pdfSKIP}
          "/BBox [0 0 " TT_pdf_Page.page_width " " TT_pdf_Page.page_height "]" {&pdfSKIP}.
    END.
    ELSE
      PUT STREAM S_pdf_inc UNFORMATTED
        "/Matrix [1 0 0 1 0 0]" {&pdfSKIP}
        "/BBox [" TT_pdf_external.ext_media1 " " TT_pdf_external.ext_media2 " " TT_pdf_external.ext_media3 " " TT_pdf_external.ext_media4 "]" {&pdfSKIP}.

    PUT STREAM S_pdf_inc UNFORMATTED
        "/Resources <<" {&pdfSKIP}.

    /* Font */
    PUT STREAM S_pdf_inc UNFORMATTED
        "  /Font <<" L_Fontobj " >>" {&pdfSKIP}.

    /* XObject */
    PUT STREAM S_pdf_inc UNFORMATTED
        "  /XObject <<" {&pdfSKIP}.

    FOR EACH TT_Resource WHERE TT_Resource.res_type = "XObject"
                           AND TT_Resource.page_id  = tt_pdf_external.ext_page
                           AND TT_Resource.new_obj  <> 0:
      PUT STREAM S_pdf_inc UNFORMATTED
          TT_Resource.res_text " " TT_Resource.new_obj " " TT_Resource.new_gen " R " {&pdfSKIP}.
    END.

    /* End of /Resources */
    PUT STREAM S_pdf_inc UNFORMATTED
        " >>" {&pdfSKIP}.

    /* ExtGSpace */
    PUT STREAM S_pdf_inc UNFORMATTED
        "  /ExtGState <<" {&pdfSKIP}.
  
    FOR EACH TT_Resource WHERE TT_Resource.res_type = "ExtGState"
                           AND TT_Resource.page_id  = tt_pdf_external.ext_page
                           AND TT_Resource.new_obj  <> 0:
      PUT STREAM S_pdf_inc UNFORMATTED
          TT_Resource.res_text " " TT_Resource.new_obj " " TT_Resource.new_gen " R " {&pdfSKIP}.
    END.

    PUT STREAM S_pdf_inc UNFORMATTED
        " >>" {&pdfSKIP}.

    /* ColorSpace */
    PUT STREAM S_pdf_inc UNFORMATTED
        "  /ColorSpace <<" {&pdfSKIP}.
  
    FOR EACH TT_Resource WHERE TT_Resource.res_type = "ColorSpace"
                           AND TT_Resource.page_id  = tt_pdf_external.ext_page
                           AND TT_Resource.new_obj  <> 0
                           /* BREAK BY TT_Resource.res_text */ :
      /* IF FIRST-OF(TT_Resource.res_text) THEN */
        PUT STREAM S_pdf_inc UNFORMATTED
            TT_Resource.res_text " " TT_Resource.new_obj " " TT_Resource.new_gen " R " {&pdfSKIP}.
    END.

    PUT STREAM S_pdf_inc UNFORMATTED
        " >>" {&pdfSKIP}.


    /* Shading */
    PUT STREAM S_pdf_inc UNFORMATTED
        "  /Shading <<" {&pdfSKIP}.
  
    FOR EACH TT_Resource WHERE TT_Resource.res_type = "Shading"
                           AND TT_Resource.page_id  = tt_pdf_external.ext_page
                           AND TT_Resource.new_obj  <> 0:
      PUT STREAM S_pdf_inc UNFORMATTED
          TT_Resource.res_text " " TT_Resource.new_obj " " TT_Resource.new_gen " R " {&pdfSKIP}.
    END.

    PUT STREAM S_pdf_inc UNFORMATTED
        " >>" {&pdfSKIP}.

    PUT STREAM S_pdf_inc UNFORMATTED
        " >>" {&pdfSKIP}
        "/Length " (TT_pdf_external.ext_obj + 1) " 0 R" {&pdfSKIP}.

    IF pdf_get_parameter(pdfStream,"Compress") = "TRUE" THEN
      PUT STREAM S_pdf_inc UNFORMATTED
          "/Filter /FlateDecode" {&pdfSKIP}.

    PUT STREAM S_pdf_inc UNFORMATTED
        ">>" {&pdfSKIP}
        "stream" {&pdfSKIP}.

    /* Get PDF Stream Start Offset */
    L_start = SEEK(S_pdf_inc).

    SET-SIZE(mFile) = GetFileSize(). /* Initialize Memory */
    
    RUN GetFileContent(INPUT-OUTPUT mFile).

    /* If we can put the data into a Character Field then do so.
       Then change any Xobject references within the external file.  This is
       due to duplication of Object names when using multiple external PDF
       files */

    ASSIGN iFileSize = GET-SIZE(mFile)
           iPointer  = 1
           iMemSize  = 10000.

    IF iFileSize < 31000 THEN DO:
      cContent = GET-BYTES(mFile, 1, GET-SIZE(mFile) - 1).
      FOR EACH TT_Resource 
         WHERE TT_Resource.obj_stream = pdfStream
           AND TT_Resource.res_type   = "Xobject"
           AND TT_Resource.res_old   <> "":

        IF "~/" + TT_resource.pdf_id + STRING(TT_resource.page_id )
          = TT_pdf_external.ext_tag THEN DO:
          cContent = REPLACE(cContent, TT_resource.res_old, TT_resource.res_text).
          SET-SIZE(mFile) = 0.
          SET-SIZE(mFile) = LENGTH(cContent).
          PUT-STRING(mFile,1, LENGTH(cContent)) = cContent.
        END.
      END. /* each Resource */
    END.

    /* This portion handles content replacement where the total size of the
       original content is greater than 31K */
    ELSE DO: 
      OUTPUT TO VALUE(SESSION:TEMP-DIR + "tempcat.txt").

      /* igc - Aug 24/05 - greatly reduced the size of the incrementor because 
                           it seemed to cause an issue when outputting the 
                           text values ... I need to look into this further */
      DO WHILE TRUE:
        cContent = GET-STRING(mFile, iPointer, iMemSize).

        FOR EACH TT_Resource 
           WHERE TT_Resource.obj_stream = pdfStream
             AND TT_Resource.res_type   = "Xobject"
             AND TT_Resource.res_old   <> "":

          IF "~/" + TT_resource.pdf_id + STRING(TT_resource.page_id ) = TT_pdf_external.ext_tag THEN DO:
            cContent = REPLACE(cContent, TT_resource.res_old, TT_resource.res_text).
          END.

        END. /* each Resource */

        PUT UNFORMATTED cContent.

        ASSIGN iFileSize = iFileSize - iMemSize
               iPointer  = iPointer + iMemSize.

        IF iFileSize <= 0 THEN LEAVE.
        
        cContent = "".
      END.

      PUT {&pdfSKIP}.
      
      OUTPUT CLOSE.

      SET-SIZE(mFile) = 0.
      FILE-INFO:FILE-NAME = SESSION:TEMP-DIR + "tempcat.txt".
      SET-SIZE(mFile) = FILE-INFO:FILE-SIZE.
      INPUT FROM VALUE(SESSION:TEMP-DIR + "tempcat.txt").
        IMPORT mFile.
      INPUT CLOSE.

      OS-DELETE VALUE(SESSION:TEMP-DIR + "tempcat.txt").
    END. /* > 31 K */

    /** Compression happens before Encryption **/
    IF pdf_get_parameter(pdfStream,"Compress") = "TRUE" THEN DO:
      L_Stat = compressbuffer( mFile,
                               INPUT-OUTPUT vDest,
                               OUTPUT vDestLen).

      SET-SIZE(mFile) = 0.
      SET-SIZE(mFile) = GET-SIZE(vDest).
      mFile = vDest.
    END.     

    /*** igc99
    IF pdf_get_parameter(pdfStream,"Encrypt") = "TRUE" THEN DO:
      SET-SIZE(L_EncryptKey) = 10.
      RUN GetEncryptKey /* IN h_PDF-Encrypt */
          (INPUT  TT_pdf_stream.obj_UniqueID,
           INPUT  pdf_inc_ObjectSequence,
           INPUT  0,
           INPUT  pdf-EncryptKeyMemPtr,
           OUTPUT L_EncryptKey).

      RUN EncryptContent /* IN h_PDF-Encrypt */
                         (INPUT  L_EncryptKey,
                          INPUT  mFile,
                          OUTPUT L_EncryptMem).

      RUN OutputMemPtr (L_EncryptMem).

      SET-SIZE(L_EncryptMem) = 0.
      SET-SIZE(L_EncryptKey) = 0.

    END. /* Encrypted Content */
    ELSE 
      RUN OutputMemPtr (mFile).
    ***/

    RUN OutputMemPtr(pdfStream,
                     FALSE,
                     TT_pdf_stream.obj_UniqueID, 
                     pdf_inc_ObjectSequence, 
                     mFile).

    SET-SIZE(mFile) = 0. /* Release memory */
    SET-SIZE(vDest) = 0. /* Release memory */

    L_end = SEEK(S_pdf_inc).

    PUT STREAM S_pdf_inc UNFORMATTED
               {&pdfSKIP}  /* igc - added to ensure that the endstream was
                                    separated from the last command correctly 
                           */
               "endstream" {&pdfSKIP}
               "endobj" {&pdfSKIP}.

    /* igc - Added Sept 10, 2002 */
    ObjectSequence(pdfStream, pdf_inc_ObjectSequence + 1, "ExternalLen", 0).
    TT_pdf_external.ext_len = pdf_inc_ObjectSequence.

    PUT STREAM S_pdf_inc UNFORMATTED
        TT_pdf_external.ext_len " 0 obj" {&pdfSKIP}
        "  " (L_end - L_start) {&pdfSKIP}
        "endobj" {&pdfSKIP}.

  END. /* each TT_pdf_external */

END. /* pdf_load_external */

PROCEDURE pdf_load_links: /* PRIVATE */

  DEFINE INPUT PARAMETER pdfStream  AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfEncrypt AS LOGICAL   NO-UNDO.

  DEFINE VARIABLE L_data  AS RAW NO-UNDO.
  DEFINE VARIABLE L_start AS INTEGER NO-UNDO.
  DEFINE VARIABLE L_end   AS INTEGER NO-UNDO.

  DEFINE VARIABLE m_EncryptKey  AS MEMPTR NO-UNDO.
  DEFINE VARIABLE mLink         AS MEMPTR NO-UNDO.

  FOR EACH TT_pdf_annot WHERE TT_pdf_annot.obj_stream = pdfStream:

    /* igc - Added Sept 10, 2002 */
    ObjectSequence(pdfStream, pdf_inc_ObjectSequence + 1, "Link", 0).
    TT_pdf_annot.annot_obj = pdf_inc_ObjectSequence.

    PUT STREAM S_pdf_inc UNFORMATTED
        TT_pdf_annot.annot_obj " 0 obj" CHR(13)
        "<<" CHR(13)
        "/Type /Annot" CHR(13)
        "/Subtype /" TT_pdf_annot.annot_type CHR(13)
        "/Rect [ " TT_pdf_annot.annot_rect "]" CHR(13)
        "/C [ " TT_pdf_annot.annot_color " ]" CHR(13).

    CASE TT_pdf_annot.annot_type:
      WHEN "Link" THEN DO:
        PUT STREAM S_pdf_inc UNFORMATTED
            "/Subtype /Link" CHR(13)
            "/Border [0 0 " TT_pdf_annot.annot_border "]" CHR(13)
            "/H /" TT_pdf_annot.annot_style CHR(13)
            "/A << " CHR(13)
            "  /Type /Action" CHR(13)
            "  /S /URI" CHR(13)
            "  /URI ".

        IF pdfEncrypt THEN DO:
          SET-SIZE(m_EncryptKey) = IF pdf_get_parameter(pdfStream,"EncryptKey") = "40" 
                                   THEN 10 ELSE 32.
          RUN GetEncryptKey /* IN h_PDF-Encrypt */
              (INPUT  pdfStream,
               INPUT  TT_pdf_stream.obj_UniqueID,
               INPUT  pdf_inc_ObjectSequence,
               INPUT  0,
               INPUT  pdf-EncryptKeyMemPtr,
               OUTPUT m_EncryptKey).

          SET-SIZE(mLink) = LENGTH(TT_pdf_annot.annot_content, "character":u) + 1.
          PUT-STRING(mlink,1) = TT_pdf_annot.annot_content.

          RUN EncryptContent /* IN h_PDF-Encrypt */
                            (INPUT  m_EncryptKey,
                             INPUT  mLink,
                             OUTPUT mLink).

          PUT STREAM S_pdf_inc UNFORMATTED "<".

          RUN OutputMemPtrAsHex (mLink).
          SET-SIZE(mLink) = 0.

          PUT STREAM S_pdf_inc UNFORMATTED ">" CHR(13).
        END.

        ELSE DO:
          RUN pdf_replace_text(INPUT-OUTPUT TT_pdf_annot.annot_content).

          PUT STREAM S_pdf_inc UNFORMATTED
            "(" TT_pdf_annot.annot_content ")" CHR(13).
        END.

        PUT STREAM S_pdf_inc UNFORMATTED
            "  >> " CHR(13).
      END. /* Link */

      WHEN "Text" OR WHEN "Stamp" THEN DO:
        PUT STREAM S_pdf_inc UNFORMATTED
            "/T (" TT_pdf_annot.annot_style ")" CHR(13)
            "/Name /" TT_pdf_annot.annot_icon CHR(13)
            "/Border [0 0 " TT_pdf_annot.annot_border "]" CHR(13)
            "/Contents " CHR(13).

        IF pdfEncrypt THEN DO:
          SET-SIZE(m_EncryptKey) = IF pdf_get_parameter(pdfStream,"EncryptKey") = "40" 
                                 THEN 10 ELSE 32.
          RUN GetEncryptKey /* IN h_PDF-Encrypt */
              (INPUT  pdfStream,
               INPUT  TT_pdf_stream.obj_UniqueID,
               INPUT  pdf_inc_ObjectSequence,
               INPUT  0,
               INPUT  pdf-EncryptKeyMemPtr,
               OUTPUT m_EncryptKey).

          SET-SIZE(mLink) = LENGTH(TT_pdf_annot.annot_content, "character":u) + 1.
          PUT-STRING(mlink,1) = TT_pdf_annot.annot_content.

          RUN EncryptContent /* IN h_PDF-Encrypt */
                            (INPUT  m_EncryptKey,
                             INPUT  mLink,
                             OUTPUT mLink).

          PUT STREAM S_pdf_inc UNFORMATTED "<".

          RUN OutputMemPtrAsHex (mLink).
          SET-SIZE(mLink) = 0.

          PUT STREAM S_pdf_inc UNFORMATTED ">" CHR(13).
        END.

        ELSE DO:
          RUN pdf_replace_text(INPUT-OUTPUT TT_pdf_annot.annot_content).

          PUT STREAM S_pdf_inc UNFORMATTED
            "(" TT_pdf_annot.annot_Content ")" CHR(13).
        END.

      END. /* Text */

      WHEN "Highlight" OR WHEN "Underline" OR WHEN "Squiggly" OR WHEN "Strikeout"
      THEN DO:
        PUT STREAM S_pdf_inc UNFORMATTED
            "/QuadPoints [" TT_pdf_annot.annot_add "]" CHR(13) 
            "/T (" TT_pdf_annot.annot_style ")" CHR(13)
            "/Border [0 0 " TT_pdf_annot.annot_border "]" CHR(13)
            "/Contents " CHR(13).

        IF pdfEncrypt THEN DO:
          SET-SIZE(m_EncryptKey) = IF pdf_get_parameter(pdfStream,"EncryptKey") = "40" 
                                   THEN 10 ELSE 32.
          RUN GetEncryptKey /* IN h_PDF-Encrypt */
              (INPUT  pdfStream,
               INPUT  TT_pdf_stream.obj_UniqueID,
               INPUT  pdf_inc_ObjectSequence,
               INPUT  0,
               INPUT  pdf-EncryptKeyMemPtr,
               OUTPUT m_EncryptKey).

          SET-SIZE(mLink) = LENGTH(TT_pdf_annot.annot_content, "character":u) + 1.
          PUT-STRING(mlink,1) = TT_pdf_annot.annot_content.

          RUN EncryptContent /* IN h_PDF-Encrypt */
                            (INPUT  m_EncryptKey,
                             INPUT  mLink,
                             OUTPUT mLink).

          PUT STREAM S_pdf_inc UNFORMATTED "<".

          RUN OutputMemPtrAsHex (mLink).
          SET-SIZE(mLink) = 0.

          PUT STREAM S_pdf_inc UNFORMATTED ">" CHR(13).
        END.

        ELSE DO:
          RUN pdf_replace_text(INPUT-OUTPUT TT_pdf_annot.annot_content).

          PUT STREAM S_pdf_inc UNFORMATTED
            "(" TT_pdf_annot.annot_Content ")" CHR(13).
        END.

      END. /* Highlight,Underline,Squiggly,StrikeOut */

    END CASE.

    PUT STREAM S_pdf_inc UNFORMATTED
        ">>" CHR(13)
        "endobj" CHR(13).

    SET-SIZE(m_EncryptKey) = 0.

  END. /* each TT_pdf_annot */

END. /* pdf_load_links */

PROCEDURE pdf_get_image_wh: /* PRIVATE */
  DEFINE INPUT PARAMETER pdfStream  AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER Pimage     AS CHARACTER NO-UNDO.

  DEFINE VARIABLE iFileSize     AS INTEGER NO-UNDO.
  DEFINE VARIABLE pdf_marker    AS INTEGER NO-UNDO.

  FILE-INFO:FILE-NAME = pImage.
  iFileSize = GetFileSize().
  iImageByte = 0.

  SET-SIZE(mImage) = iFileSize.
  INPUT STREAM S_pdf_inp FROM VALUE(SEARCH(Pimage)) BINARY NO-MAP NO-CONVERT UNBUFFERED.
    IMPORT STREAM s_pdf_inp mImage.
  INPUT STREAM S_pdf_inp CLOSE.

  IF NOT first_marker() THEN DO:
    RUN pdf_error(pdfStream,"pdf_get_Image_wh","Cannot find Stream!").
    RETURN.
  END.

  DO WHILE TRUE:
    pdf_marker = next_marker().

    CASE hex(pdf_marker):
      WHEN {&M_SOF0} OR WHEN {&M_SOF1} OR WHEN {&M_SOF2} OR WHEN {&M_SOF3}
      OR WHEN {&M_SOF5} OR WHEN {&M_SOF6} OR WHEN {&M_SOF7} OR WHEN {&M_SOF9}
      OR WHEN {&M_SOF10} OR WHEN {&M_SOF11} OR WHEN {&M_SOF13}
      OR WHEN {&M_SOF14} OR WHEN {&M_SOF15} THEN DO:
        process_SOF().
        LEAVE.
      END.
      WHEN {&M_SOS} OR WHEN {&M_EOI} THEN
        LEAVE.
      OTHERWISE
        skip_variable().
    END CASE.
  END. /* true loop */

  SET-SIZE(mImage) = 0.

END.

PROCEDURE pdf_replace_text: /* PRIVATE */
  DEFINE INPUT-OUTPUT PARAMETER pdfText AS CHARACTER NO-UNDO.

  /* Replace any special characters in the data string since this
     will create a bad PDF doccument */
  pdfText = REPLACE(pdfText,"~\","~\~\").
  
  pdfText = REPLACE(pdfText,"(","~\(").
  pdfText = REPLACE(pdfText,")","~\)").
  pdfText = REPLACE(pdfText,"[","~\[").
  pdfText = REPLACE(pdfText,"]","~\]").

END. /* pdf_replace_text */

PROCEDURE pdf_reset_all: 
  /* clear out all streams, and reset variables as required */

  /* These are the only two variables that don't apear to be reset anywhere */
  ASSIGN /* pdf_inc_ContentSequence = 0 */
         pdf_inc_ObjectSequence  = 0.

  /* Clear all temp-tables */
    EMPTY TEMP-TABLE TT_pdf_stream.
    EMPTY TEMP-TABLE TT_pdf_page.
    EMPTY TEMP-TABLE TT_pdf_bookmark.
    EMPTY TEMP-TABLE TT_pdf_ReplaceTxt.
    EMPTY TEMP-TABLE TT_pdf_param.
    EMPTY TEMP-TABLE TT_pdf_error.
    EMPTY TEMP-TABLE TT_pdf_object.
    EMPTY TEMP-TABLE TT_pdf_info.
    EMPTY TEMP-TABLE TT_pdf_image.
    EMPTY TEMP-TABLE TT_pdf_font.
    EMPTY TEMP-TABLE TT_pdf_annot.
  
    EMPTY TEMP-TABLE TT_pdf_tool.
    EMPTY TEMP-TABLE TT_pdf_tool_param.
    EMPTY TEMP-TABLE TT_pdf_external.
    EMPTY TEMP-TABLE TT_Font.
    EMPTY TEMP-TABLE TT_Info.
    EMPTY TEMP-TABLE TT_Object.
    EMPTY TEMP-TABLE TT_Resource.
    EMPTY TEMP-TABLE TT_pdf_xml.

    EMPTY TEMP-TABLE TT_pdf_FillTxt.
    EMPTY TEMP-TABLE TT_Widget.

  FOR EACH TT_pdf_ext:
    IF OPSYS = "UNIX" THEN
      OS-COMMAND SILENT VALUE("rm -f " + SESSION:TEMP-DIR + TT_pdf_ext.pdf_id + "*.txt").
    ELSE
      OS-COMMAND SILENT VALUE("del " + SESSION:TEMP-DIR + TT_pdf_ext.pdf_id + "*.txt").

    DELETE TT_pdf_ext.
  END.

END. /* pdf_reset_all */

PROCEDURE pdf_reset_stream .
  /* Clear out an individual stream - reset the variables */
  DEFINE INPUT PARAMETER pdfStream     AS CHARACTER NO-UNDO.

  /* These are the only two variables that don't apear to be reset anywhere */
  ASSIGN pdf_inc_ObjectSequence  = 0.

  /* As far as I know, you gotta do a for each regardless of version */
  FOR EACH TT_pdf_stream WHERE TT_pdf_stream.obj_stream = pdfStream:
    DELETE TT_pdf_stream .
  END.

  FOR EACH TT_pdf_page WHERE TT_pdf_page.obj_stream = pdfStream:
    DELETE TT_pdf_page.
  END.

  FOR EACH TT_pdf_bookmark WHERE TT_pdf_bookmark.obj_stream = pdfStream:
    DELETE TT_pdf_bookmark.
  END.

  FOR EACH TT_pdf_ReplaceTxt WHERE TT_pdf_ReplaceTxt.obj_stream = pdfStream:
    DELETE TT_pdf_ReplaceTxt.
  END.

  FOR EACH TT_pdf_param WHERE TT_pdf_param.obj_stream = pdfStream:
    DELETE TT_pdf_param.
  END.

  FOR EACH TT_pdf_error WHERE TT_pdf_error.obj_stream = pdfStream:
    DELETE TT_pdf_error.
  END.

  FOR EACH TT_pdf_object WHERE TT_pdf_object.obj_stream = pdfStream:
    DELETE TT_pdf_object.
  END.

  FOR EACH TT_pdf_info WHERE TT_pdf_info.obj_stream = pdfStream:
    DELETE TT_pdf_info.
  END.

  FOR EACH TT_pdf_image WHERE TT_pdf_image.obj_stream = pdfStream:
    DELETE TT_pdf_image.
  END.

  FOR EACH TT_pdf_font WHERE TT_pdf_font.obj_stream = pdfStream:
    DELETE TT_pdf_font.
  END.

  FOR EACH TT_pdf_annot WHERE TT_pdf_annot.obj_stream = pdfStream:
    DELETE TT_pdf_annot.
  END.

  FOR EACH TT_pdf_tool WHERE TT_pdf_tool.obj_stream = pdfStream:
    DELETE TT_pdf_tool.
  END.

  FOR EACH TT_pdf_tool_param WHERE TT_pdf_tool_param.obj_stream = pdfStream:
    DELETE TT_pdf_tool_param.
  END.

  FOR EACH TT_pdf_external WHERE TT_pdf_external.obj_stream = pdfStream:
    DELETE TT_pdf_external.
  END.

  FOR EACH TT_pdf_ext WHERE TT_pdf_ext.obj_stream = pdfStream:
    IF OPSYS = "UNIX" THEN
      OS-COMMAND SILENT VALUE("rm -f " + SESSION:TEMP-DIR + TT_pdf_ext.pdf_id + "*.txt").
    ELSE
      OS-COMMAND SILENT VALUE("del " + SESSION:TEMP-DIR + TT_pdf_ext.pdf_id + "*.txt").

    DELETE TT_pdf_ext.
  END.

  FOR EACH TT_Font WHERE TT_Font.obj_stream = pdfStream:
    DELETE TT_Font.
  END.

  FOR EACH TT_Info WHERE TT_Info.obj_stream = pdfStream:
    DELETE TT_Info.
  END.

  FOR EACH TT_Object WHERE TT_Object.obj_stream = pdfStream:
    DELETE TT_Object.
  END.

  FOR EACH TT_Resource WHERE TT_Resource.obj_stream = pdfStream:
    DELETE TT_Resource.
  END.

  FOR EACH TT_pdf_xml WHERE TT_pdf_xml.obj_stream = pdfStream:
    DELETE TT_pdf_xml.
  END.

  FOR EACH TT_pdf_FillTxt WHERE TT_pdf_FillTxt.obj_stream = pdfStream:
    DELETE TT_pdf_FillTxt.
  END.

  FOR EACH TT_Widget WHERE TT_Widget.obj_stream = pdfStream:
    DELETE TT_Widget.
  END.

END. /* pdf_reset_stream */

PROCEDURE pdf_wrap_text :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER pdfStream     AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfText       AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfFromColumn AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER pdfToColumn   AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER pdfAlignMent  AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER pdfMaxY      AS INTEGER NO-UNDO.

  DEFINE VAR v-thisword  AS CHAR NO-UNDO.
  DEFINE VAR v-thisline  AS CHAR NO-UNDO.
  DEFINE VAR v-lastline  AS CHAR NO-UNDO.
  DEFINE VAR v-fontsize  AS DECIMAL NO-UNDO.
  DEFINE VAR v-mywidth   AS INTEGER NO-UNDO.
  DEFINE VAR v-maxwidth  AS INTEGER NO-UNDO.
  DEFINE VAR v-originalY AS INTEGER NO-UNDO.
  DEFINE VAR i           AS INTEGER NO-UNDO.
  DEFINE VAR v-index     AS INTEGER NO-UNDO.
  DEFINE VAR v-start     AS INTEGER NO-UNDO.
  DEFINE VAR v-leave     AS LOGICAL NO-UNDO.

  ASSIGN v-fontsize  = PDF_text_width(pdfStream,"i") * .81
         v-mywidth   = pdfToColumn - pdfFromColumn
         v-originalY = PDF_TextY(pdfStream).

  /* Replace Pipes in text string with processor directive &pipe; */
  ASSIGN pdfText = REPLACE(pdfText,"|","&pipe;").
  /* Spaces */
  ASSIGN pdfText = REPLACE(pdfText," ","| ").
  /* Hyphens */
  ASSIGN pdfText = REPLACE(pdfText,"-","-|").
  /* Commas */
  ASSIGN pdfText = REPLACE(pdfText,",",",|").
  /* Line Feeds */
  ASSIGN pdfText = REPLACE(pdfText,CHR(10),"|&skip;|").

  /* Determine the maximum possible column using the W (assuming it as the 
    largest possible character.  This is useful when using Proportional fonts -
    it won't matter when using Fixed since they are all the same width */
  v-maxwidth = pdf_getnumfittingchars(pdfStream,
                                       FILL("W",v-mywidth),
                                       0,
                                       pdf_text_width(pdfstream,FILL("W",v-mywidth))).

  /* Divide up the pdf text into lines of width less than the
     available width */
  pdf_WrapText = TRUE.
  pdf_WrapFont = pdf_Font(pdfStream).
  pdf_WrapSize = pdf_PointSize(pdfStream).

  THIS-LOOP:
  DO i = 1 TO NUM-ENTRIES(pdfText,"|"):
    ASSIGN v-lastline = v-thisline.

    IF ENTRY(i,pdfText,"|") = "&skip;" 
    THEN DO:
      CASE pdfAlignment:
        WHEN "left" THEN
          RUN pdf_text_at(pdfStream,v-thisline,pdfFromColumn).
        WHEN "right" THEN
          RUN pdf_text_to(pdfStream,v-thisline,pdfToColumn).
      END CASE.

      v-thisline = "".
      RUN pdf_skip(pdfStream).
      NEXT THIS-LOOP.
    END.


    v-thisword = REPLACE(ENTRY(i,pdfText,"|"),"&pipe;","|").

    ASSIGN v-thisline = v-thisline
                      + v-thisword.

    IF pdf_getnumfittingchars(pdfStream,
                              TRIM(v-thisline),
                              0,
                              pdf_text_width(pdfstream,TRIM(v-thisline))) > v-maxwidth THEN DO:

      ASSIGN v-index = 0
             v-start = 1
             v-leave = FALSE.

      ASSIGN v-lastline = TRIM(v-lastline).

      DO WHILE TRUE:
        v-index = INDEX(v-lastline,CHR(10), v-start + 1).

        IF v-index = 0 THEN
          ASSIGN v-leave = TRUE
                 v-index = LENGTH(v-lastline, "character":u).
        ELSE v-leave = FALSE.

        CASE pdfAlignment:
          WHEN "left" THEN
            RUN pdf_text_at(pdfStream,SUBSTR(v-lastline,v-start,v-index, "character":u),pdfFromColumn).
          WHEN "right" THEN
            RUN pdf_text_to(pdfStream,SUBSTR(v-lastline,v-start,v-index, "character":u),pdfToColumn).
        END CASE.

        IF v-leave THEN LEAVE.

        ELSE DO:
          v-start = INDEX(v-lastline,CHR(10), v-index + 1).
          RUN pdf_skip(pdfStream).
        END.

      END. /* while true */

      RUN pdf_skip(pdfStream).
      ASSIGN v-thisline = TRIM(REPLACE(ENTRY(i,pdfText,"|"),"&pipe;","|")).
    END.
  END. /* i = NUM-ENTRIES */

  IF v-thisline NE "" THEN DO:
    ASSIGN v-index = 0
           v-start = 1
           v-leave = FALSE.

    ASSIGN v-lastline = TRIM(v-thisline).
    DO WHILE TRUE:
      ASSIGN v-lastline = SUBSTR(v-lastline,v-start,-1, "character":u)
             v-index    = INDEX(v-lastline,CHR(10), v-start + 1).

      IF v-index = 0 THEN
        ASSIGN v-leave = TRUE
               v-index = LENGTH(v-lastline, "character":u).
      ELSE
        ASSIGN v-leave = FALSE.

      IF v-index <> 0 THEN DO:
        CASE pdfAlignment:
          WHEN "left" THEN
            RUN pdf_text_at(pdfStream, SUBSTR(v-lastline,v-start,v-index, "character":u),pdfFromColumn).
          WHEN "right" THEN
            RUN pdf_text_to(pdfStream, SUBSTR(v-lastline,v-start,v-index, "character":u),pdfToColumn).
        END CASE.
      END.
      ELSE
        v-leave = TRUE.

      IF v-leave THEN LEAVE.

      ELSE DO:
        v-lastline = SUBSTR(v-lastline,v-index + 1,-1, "character":u) .
        v-start = INDEX(v-lastline,CHR(10), v-index + 1).
        RUN pdf_skip(pdfStream).
      END.

    END. /* while true */
  END. /* ThisLine <> "" */

  ASSIGN pdfMaxY = PDF_TextY(pdfStream).

  pdf_WrapText = FALSE.

END PROCEDURE. /* pdf_wrap_text */

FUNCTION pdf_TotalPages RETURN CHARACTER
   (INPUT pdfStream AS CHARACTER).

  RETURN "@@TOTALPages-" + pdfStream.

END FUNCTION. /* pdf_TotalPages */

PROCEDURE pdf_link:
  DEFINE INPUT PARAMETER pdfStream   AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfFromX    AS INTEGER   NO-UNDO.
  DEFINE INPUT PARAMETER pdfFromY    AS INTEGER   NO-UNDO.
  DEFINE INPUT PARAMETER pdfWidth    AS INTEGER   NO-UNDO.
  DEFINE INPUT PARAMETER pdfHeight   AS INTEGER   NO-UNDO.
  DEFINE INPUT PARAMETER pdfLink     AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfRed      AS DECIMAL   NO-UNDO.
  DEFINE INPUT PARAMETER pdfGreen    AS DECIMAL   NO-UNDO.
  DEFINE INPUT PARAMETER pdfBlue     AS DECIMAL   NO-UNDO.
  DEFINE INPUT PARAMETER pdfBorder   AS INTEGER   NO-UNDO.
  DEFINE INPUT PARAMETER pdfStyle    AS CHARACTER NO-UNDO.

  pdfRed   = IF pdfRed < 0 THEN 0
             ELSE IF pdfRed > 1 THEN 1
             ELSE pdfRed.
  pdfGreen = IF pdfGreen < 0 THEN 0
             ELSE IF pdfGreen > 1 THEN 1
             ELSE pdfGreen.
  pdfBlue  = IF pdfBlue < 0 THEN 0
             ELSE IF pdfBlue > 1 THEN 1
             ELSE pdfBlue.

  IF LOOKUP(pdfStyle,"N,I,O,P") = 0 THEN
    pdfStyle = "N".

  CREATE TT_pdf_annot.
  ASSIGN TT_pdf_annot.obj_stream   = pdfStream
         TT_pdf_annot.annot_type    = "Link"
         TT_pdf_annot.annot_content = pdfLink.
         TT_pdf_annot.annot_page    = pdf_Page(pdfStream).
  ASSIGN TT_pdf_annot.annot_rect    = STRING(pdfFromX) + " "
                                    + STRING(pdfFromY) + " "
                                    + STRING(pdfWidth) + " "
                                    + STRING(pdfHeight)
         TT_pdf_annot.annot_color   = dec2string(pdfRed) + " "
                                  + dec2string(pdfGreen) + " "
                                  + dec2string(pdfBlue)
         TT_pdf_annot.annot_border  = IF pdfBorder < 0 THEN 0 ELSE pdfBorder
         TT_pdf_annot.annot_style   = pdfStyle.

END. /* pdf_link */

PROCEDURE pdf_new_page :
  DEFINE INPUT PARAMETER pdfStream   AS CHARACTER NO-UNDO.

  DEFINE VARIABLE maxPageNumber         AS INTEGER    NO-UNDO.
  DEFINE BUFFER B_TT_pdf_Stream FOR TT_pdf_Stream.

  FIND FIRST B_TT_pdf_stream
       WHERE B_TT_pdf_stream.obj_stream = pdfStream NO-ERROR.
  IF NOT AVAIL B_TT_pdf_Stream THEN DO:
    RUN pdf_error(pdfStream,"pdf_new_page","Cannot find Stream!").
    RETURN .
  END.

  /* If the Tool Suite is being used then re-run the appropriate tools.
     Only runs the Tools for pages greater than one since the pdf_tool_create
     should handle the first page */
  IF pdf_Page(pdfStream) >= 1 AND VALID-HANDLE(h_PDF-Tool) THEN DO:
    FOR EACH TT_pdf_tool NO-LOCK:
      IF  TT_pdf_tool.tool_type = "TABLE" THEN
        RUN DisplayTableOutline IN h_PDF-tool
            (pdfStream, TT_pdf_tool.tool_name).
    END.
  END.

  IF B_TT_pdf_stream.obj_Footer <> "" THEN DO:
    IF pdf_Page(pdfStream) >= 1 THEN DO:
      pdf_ForFooter = TRUE.

      RUN pdf_set_TextY(pdfStream,pdf_BottomMargin(pdfStream)).

      RUN OutputTextContent(pdfStream, 
                            "TEXT",
                              "1 0 0 1 " + STRING(pdf_LeftMargin(pdfStream))
                            + " " + STRING(pdf_BottomMargin(pdfStream)) 
                            + " Tm",
                            "",
                            "").

      RUN VALUE(B_TT_pdf_stream.obj_Footer) IN B_TT_pdf_stream.obj_CallProc NO-ERROR.

      RUN pdf_set_TextY(pdfStream,pdf_PageHeight(pdfStream)).

      pdf_ForFooter = FALSE.
    END.
  END.

  IF pdf_Page(pdfStream) > 0 THEN DO:

    IF B_TT_pdf_Stream.obj_DoingText THEN
      RUN OutputTextContent(pdfStream, 
                           "TEXT",
                           "ET",
                            "",
                            "").
    ELSE IF B_TT_pdf_Stream.obj_DoingGraphic THEN
      RUN OutputTextContent(pdfStream, 
                           "GRAPHIC",
                           "Q",
                            "",
                            "").

    ASSIGN B_TT_pdf_Stream.obj_DoingText = FALSE
           B_TT_pdf_Stream.obj_DoingGraphic = FALSE.
  END.

  maxPageNumber = 0.
  /* Calculate current total number of pages */
  FOR EACH tt_pdf_page WHERE TT_pdf_page.obj_stream = pdfStream
                          BY tt_pdf_page.page_nbr DESCENDING:
    maxPageNumber = TT_pdf_page.page_nbr.
    LEAVE.
  END.

  RUN pdf_set_Page(pdfStream,maxPageNumber + 1).


  CREATE TT_pdf_page.
  TT_pdf_page.obj_stream  = pdfStream.
  TT_pdf_page.page_nbr    = pdf_Page(pdfStream).
  TT_pdf_page.page_rotate = pdf_PageRotate(pdfStream).  
  TT_pdf_page.page_width  = pdf_PageWidth(pdfStream).
  TT_pdf_page.page_height = pdf_PageHeight(pdfStream).

  RUN pdf_set_TextY(pdfStream,pdf_PageHeight(pdfStream) - pdf_TopMargin(pdfStream)).

  IF B_TT_pdf_stream.obj_Header <> "" THEN DO:
    RUN VALUE(B_TT_pdf_stream.obj_Header) IN B_TT_pdf_Stream.obj_CallProc NO-ERROR.
  END.


  /* If the Tool Suite is being used then re-run the appropriate tools.
     Only runs the Tools for pages greater than one since the pdf_tool_create
     should handle the first page */
  IF pdf_Page(pdfStream) > 1 AND VALID-HANDLE(h_PDF-Tool) THEN DO:
    FOR EACH TT_pdf_tool NO-LOCK:
      IF TT_pdf_tool.tool_type = "TABLE" THEN
        RUN DisplayTableHeader IN h_PDF-tool
            (pdfStream, TT_pdf_tool.tool_name).
    END.
  END.

  IF pdf_WrapText THEN
    RUN pdf_set_font(pdfStream, pdf_WrapFont, pdf_WrapSize).
  ELSE
    RUN pdf_set_font(pdfStream, pdf_Font(pdfStream), pdf_PointSize(pdfStream)).
  
  PUBLISH "GeneratePDFPage" (INPUT pdfStream, INPUT pdf_Page(pdfStream)).

END. /* pdf_new_page */

PROCEDURE pdf_new_page2 :
  DEFINE INPUT PARAMETER pdfStream      AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfOrientation AS CHARACTER NO-UNDO.

  DEFINE VARIABLE maxPageNumber         AS INTEGER    NO-UNDO.
  DEFINE BUFFER B_TT_pdf_Stream FOR TT_pdf_Stream.

  FIND FIRST B_TT_pdf_stream
       WHERE B_TT_pdf_stream.obj_stream = pdfStream NO-LOCK NO-ERROR.
  IF NOT AVAIL B_TT_pdf_Stream THEN DO:
    RUN pdf_error(pdfStream,"pdf_new_page2","Cannot find Stream!").
    RETURN .
  END.

  /* If the Tool Suite is being used then re-run the appropriate tools.
     Only runs the Tools for pages greater than one since the pdf_tool_create
     should handle the first page */
  IF pdf_Page(pdfStream) >= 1 AND VALID-HANDLE(h_PDF-Tool) THEN DO:
    FOR EACH TT_pdf_tool NO-LOCK:
      IF  TT_pdf_tool.tool_type = "TABLE" THEN
        RUN DisplayTableOutline IN h_PDF-tool
            (pdfStream, TT_pdf_tool.tool_name).
    END.
  END.

  IF B_TT_pdf_stream.obj_Footer <> "" THEN DO:
    IF pdf_Page(pdfStream) >= 1 THEN DO:
      pdf_ForFooter = TRUE.

      RUN pdf_set_TextY(pdfStream,pdf_BottomMargin(pdfStream)).

      RUN OutputTextContent(pdfStream, 
                            "TEXT",
                              "1 0 0 1 " + STRING(pdf_LeftMargin(pdfStream))
                            + " " + STRING(pdf_BottomMargin(pdfStream)) 
                            + " Tm",
                            "",
                            "").

      RUN VALUE(B_TT_pdf_stream.obj_Footer) IN B_TT_pdf_stream.obj_CallProc NO-ERROR.

      RUN pdf_set_TextY(pdfStream,pdf_PageHeight(pdfStream)).

      pdf_ForFooter = FALSE.
    END.
  END.

  IF pdf_Page(pdfStream) > 0 THEN DO:

    IF B_TT_pdf_Stream.obj_DoingText THEN
      RUN OutputTextContent(pdfStream, 
                           "TEXT",
                           "ET",
                            "",
                            "").
    ELSE IF B_TT_pdf_Stream.obj_DoingGraphic THEN
      RUN OutputTextContent(pdfStream, 
                           "GRAPHIC",
                           "Q",
                            "",
                            "").

    ASSIGN B_TT_pdf_Stream.obj_DoingText = FALSE
           B_TT_pdf_Stream.obj_DoingGraphic = FALSE.

  END.

  maxPageNumber = 0.
  /* Calculate current total number of pages */
  FOR EACH tt_pdf_page WHERE TT_pdf_page.obj_stream = pdfStream
                          BY tt_pdf_page.page_nbr DESCENDING:
    maxPageNumber = TT_pdf_page.page_nbr.
    LEAVE.
  END.

  RUN pdf_set_Page(pdfStream,maxPageNumber + 1).


  CREATE TT_pdf_page.
  TT_pdf_page.obj_stream  = pdfStream.
  TT_pdf_page.page_nbr    = pdf_Page(pdfStream).
  TT_pdf_page.page_rotate = pdf_PageRotate(pdfStream).  

  RUN pdf_set_Orientation(pdfStream,pdfOrientation).
  TT_pdf_page.page_width  = pdf_PageWidth(pdfStream).
  TT_pdf_page.page_height = pdf_PageHeight(pdfStream).

  RUN pdf_set_TextY(pdfStream,pdf_PageHeight(pdfStream) - pdf_TopMargin(pdfStream)).

  IF B_TT_pdf_stream.obj_Header <> "" THEN DO:
    RUN VALUE(B_TT_pdf_stream.obj_Header) IN B_TT_pdf_Stream.obj_CallProc NO-ERROR.
  END.

  /* If the Tool Suite is being used then re-run the appropriate tools.
     Only runs the Tools for pages greater than one since the pdf_tool_create
     should handle the first page */
  IF pdf_Page(pdfStream) > 1 AND VALID-HANDLE(h_PDF-Tool) THEN DO:
    FOR EACH TT_pdf_tool NO-LOCK:
      IF TT_pdf_tool.tool_type = "TABLE" THEN
        RUN DisplayTableHeader IN h_PDF-tool
            (pdfStream, TT_pdf_tool.tool_name).
    END.
  END.

  IF pdf_WrapText THEN
    RUN pdf_set_font(pdfStream, pdf_WrapFont, pdf_WrapSize).
  ELSE
    RUN pdf_set_font(pdfStream, pdf_Font(pdfStream), pdf_PointSize(pdfStream)).

  PUBLISH "GeneratePDFPage" (INPUT pdfStream, INPUT pdf_Page(pdfStream)).

END. /* pdf_new_page2 */

PROCEDURE pdf_insert_page :
  DEFINE INPUT PARAMETER pdfStream       AS CHARACTER  NO-UNDO.
  DEFINE INPUT PARAMETER pageNo          AS INTEGER    NO-UNDO.
  DEFINE INPUT PARAMETER BeforeOrAfter   AS CHARACTER  NO-UNDO.
  
  DEFINE VARIABLE        newPageNo       AS INTEGER    NO-UNDO.

  DEFINE VARIABLE maxPageNumber         AS INTEGER    NO-UNDO.

  DEFINE BUFFER B_TT_pdf_Stream FOR TT_pdf_Stream.
  DEFINE BUFFER b_b_tt_pdfStream FOR TT_pdf_Stream.

  FIND FIRST B_TT_pdf_stream
       WHERE B_TT_pdf_stream.obj_stream = pdfStream NO-ERROR.
  IF NOT AVAIL B_TT_pdf_Stream THEN DO:
    RUN pdf_error(pdfStream,"pdf_insert_page","Cannot find Stream!").
    RETURN .
  END.

  /* If the Tool Suite is being used then re-run the appropriate tools.
     Only runs the Tools for pages greater than one since the pdf_tool_create
     should handle the first page */
  IF pdf_Page(pdfStream) >= 1 AND VALID-HANDLE(h_PDF-Tool) THEN DO:
    FOR EACH TT_pdf_tool NO-LOCK:
      IF  TT_pdf_tool.tool_type = "TABLE" THEN
        RUN DisplayTableOutline IN h_PDF-tool
            (pdfStream, TT_pdf_tool.tool_name).
    END.
  END.

  /**
  /* IF pdf_DoingText THEN DO: */
  IF B_TT_pdf_Stream.obj_DoingText THEN DO:
    PUT STREAM S_pdf_out UNFORMATTED "ET" {&pdfskip}.
    /* pdf_DoingText = FALSE. */
    B_TT_pdf_stream.obj_DoingText = FALSE.
  END.
  /* ELSE IF pdf_DoingGraphic THEN DO: */
  ELSE IF B_TT_pdf_stream.obj_DoingGraphic THEN DO:
    PUT STREAM S_pdf_out UNFORMATTED "Q" {&pdfskip}.
    /* pdf_DoingGraphic = FALSE. */
    B_TT_pdf_stream.obj_DoingGraphic = FALSE.
  END.
  */
  IF B_TT_pdf_Stream.obj_DoingText THEN
    RUN OutputTextContent(pdfStream, 
                         "TEXT",
                         "ET",
                          "",
                          "").
  ELSE IF B_TT_pdf_Stream.obj_DoingGraphic THEN
    RUN OutputTextContent(pdfStream, 
                         "GRAPHIC",
                         "Q",
                          "",
                          "").

  OUTPUT STREAM S_pdf_out CLOSE.

    ASSIGN B_TT_pdf_Stream.obj_DoingText = FALSE
           B_TT_pdf_Stream.obj_DoingGraphic = FALSE.

  maxPageNumber = 0.
  /* Calculate current total number of pages */
  FOR EACH tt_pdf_page 
     WHERE TT_pdf_page.obj_stream = pdfStream
       AND tt_pdf_page.page_nbr >= PageNo
     BREAK BY tt_pdf_page.page_nbr descending:
    IF FIRST(TT_pdf_page.page_nbr) THEN
      maxPageNumber = TT_pdf_page.page_nbr.
    
    IF TT_pdf_page.page_nbr = PageNo AND BeforeOrAfter = "AFTER" THEN NEXT.

    OS-RENAME VALUE(  SESSION:TEMP-DIR 
                      + TT_pdf_stream.obj_UniqueID  
                      + "-Content-" 
                      + STRING(TT_pdf_page.page_nbr) + ".txt") 
              VALUE(  SESSION:TEMP-DIR 
                      + TT_pdf_stream.obj_UniqueID  
                      + "-Content-" 
                      + STRING(TT_pdf_page.page_nbr + 1) + ".txt").
  END. /* determine maxpagenumber and renumber temp file holders */

/* Re-open page stream for current page using the
   updated file name 
   If the current page is greater or equal to the page we are inserting
   then the file will have been renamed.
   Otherwise, our current page came before the inserted page and the name
   remains the same
*/
  IF pdf_Page(pdfStream) GE pageNo THEN
      newPageNO = pdf_Page(pdfStream) + 1.
  ELSE
      newPageNo = pdf_Page(pdfStream).

   OUTPUT STREAM S_pdf_out TO
         VALUE(  SESSION:TEMP-DIR
               + TT_pdf_stream.obj_UniqueID
               + "-Content-"
               + string(newPageNo) + ".txt")
               BINARY NO-MAP NO-CONVERT APPEND.

  IF maxPageNumber = 0 or (pageNo gt maxPageNumber) THEN do:
    RUN pdf_error(pdfStream,"pdf_insert_page","Invalid page number!").
    RETURN .
  END.

  IF B_TT_pdf_stream.obj_Footer <> "" THEN DO:
    IF pdf_Page(pdfStream) >= 1 THEN DO:
      pdf_ForFooter = TRUE.

      RUN pdf_set_TextY(pdfStream,pdf_BottomMargin(pdfStream)).

      RUN OutputTextContent(pdfStream, 
                            "TEXT",
                              "1 0 0 1 " + STRING(pdf_LeftMargin(pdfStream))
                            + " " + STRING(pdf_BottomMargin(pdfStream)) 
                            + " Tm",
                            "",
                            "").

      RUN VALUE(B_TT_pdf_stream.obj_Footer) IN B_TT_pdf_stream.obj_CallProc NO-ERROR.

      RUN pdf_set_TextY(pdfStream,pdf_PageHeight(pdfStream)).

      pdf_ForFooter = FALSE.
    END.
  END.

  IF beforeOrAfter = "AFTER" THEN pageNo = pageNo + 1.

  /* Move all pages after the insert point ahead one */
  FOR each tt_pdf_object where TT_pdf_object.obj_stream  = pdfStream
                         and TT_pdf_object.obj_page    ge pageNo
                          by tt_pdf_object.obj_page descending:
        tt_pdf_object.obj_page = tt_pdf_object.obj_page + 1.
  END.

  FOR each tt_pdf_page where TT_pdf_page.obj_stream  = pdfStream
                         and TT_pdf_page.page_nbr    ge pageNo
                          by tt_pdf_page.page_nbr descending:
     tt_pdf_page.page_nbr = tt_pdf_page.page_nbr + 1.
  END.

  FOR each tt_pdf_bookmark where TT_pdf_bookmark.obj_stream  = pdfStream
                         and TT_pdf_bookmark.book_page    ge pageNo
                          by tt_pdf_bookmark.book_page descending:
    tt_pdf_bookmark.book_page = tt_pdf_bookmark.book_page + 1.
  END.

  FOR each tt_pdf_annot where TT_pdf_annot.obj_stream  = pdfStream
                         and TT_pdf_annot.annot_page    ge pageNo
                          by tt_pdf_annot.annot_page descending:
    tt_pdf_annot.annot_page = tt_pdf_annot.annot_page + 1.
  END.
  /* Done moving pages */

  /* This closes the output stream for the current page
     and re-opens the stream for the new page-number */
  RUN pdf_set_Page(pdfStream,pageNo).

  /* igc
  OUTPUT STREAM S_pdf_inc CLOSE.

  OUTPUT STREAM S_pdf_out TO 
         VALUE(  SESSION:TEMP-DIR 
               + TT_pdf_stream.obj_UniqueID  
               + "-Content-" 
               + STRING(pageNo) + ".txt") 
               BINARY NO-MAP NO-CONVERT APPEND.
  */

  CREATE TT_pdf_page.
  TT_pdf_page.obj_stream  = pdfStream.
  TT_pdf_page.page_nbr    = pdf_Page(pdfStream).
  TT_pdf_page.page_rotate = pdf_PageRotate(pdfStream).

  /* igc - Copied from below */
  TT_pdf_page.page_width  = pdf_PageWidth(pdfStream).
  TT_pdf_page.page_height = pdf_PageHeight(pdfStream).

  /********* igc
  /* Note: the placement of the following commands is important due to the
      setting of the X and Y attributes.  DO NOT CHANGE unless tested
      thoroughly */
  RUN pdf_set_LeftMargin(pdfStream, pdf_LeftMargin(pdfStream)).
  RUN pdf_set_TopMargin(pdfStream, pdf_TopMargin(pdfStream)).
  RUN pdf_set_BottomMargin(pdfStream, pdf_BottomMargin(pdfStream)).
  TT_pdf_page.page_width  = pdf_PageWidth(pdfStream).
  TT_pdf_page.page_height = pdf_PageHeight(pdfStream).
 
  CASE pdf_PageRotate(pdfStream):
    WHEN 0 OR WHEN 180 THEN DO:
      RUN pdf_set_TextX(pdfStream, pdf_LeftMargin(pdfStream)).
      RUN pdf_set_Angle(pdfStream,0).
      RUN pdf_set_TextY(pdfStream, pdf_PageHeight(pdfStream) - pdf_TopMargin(pdfStream) ).

    END.

    WHEN 90 OR WHEN 270 THEN DO:
      RUN pdf_set_TextX(pdfStream, pdf_LeftMargin(pdfStream)).
      RUN pdf_set_Angle(pdfStream,0).
      RUN pdf_set_TextY(pdfStream, pdf_PageWidth(pdfStream) - pdf_TopMargin(pdfStream) ).
    END.

  END CASE.

  RUN pdf_set_GraphicX(pdfStream, pdf_LeftMargin(pdfStream)).
  RUN pdf_set_GraphicY(pdfStream, pdf_PageHeight(pdfStream) - pdf_TopMargin(pdfStream)).
  **/

  RUN pdf_set_TextY(pdfStream,pdf_PageHeight(pdfStream) - pdf_TopMargin(pdfStream)).

  IF B_TT_pdf_stream.obj_Header <> "" THEN DO:
    RUN VALUE(B_TT_pdf_stream.obj_Header) IN B_TT_pdf_Stream.obj_CallProc NO-ERROR.
  END.

  /* If the Tool Suite is being used then re-run the appropriate tools.
     Only runs the Tools for pages greater than one since the pdf_tool_create
     should handle the first page */
  IF pdf_Page(pdfStream) > 1 AND VALID-HANDLE(h_PDF-Tool) THEN DO:
    FOR EACH TT_pdf_tool NO-LOCK:
      IF TT_pdf_tool.tool_type = "TABLE" THEN
        RUN DisplayTableHeader IN h_PDF-tool
            (pdfStream, TT_pdf_tool.tool_name).
    END.
  END.

  IF pdf_WrapText THEN
    RUN pdf_set_font(pdfStream, pdf_WrapFont, pdf_WrapSize).
  ELSE
    RUN pdf_set_font(pdfStream, pdf_Font(pdfStream), pdf_PointSize(pdfStream)).

END. /* pdf_insert_page */

PROCEDURE pdf_close :
  DEFINE INPUT PARAMETER pdfStream  AS CHARACTER NO-UNDO.

  DEFINE VARIABLE L_Content AS CHARACTER NO-UNDO.
  DEFINE VARIABLE L_Encrypt AS LOGICAL NO-UNDO INIT FALSE.
  DEFINE VARIABLE maxPageNumber         AS INTEGER    NO-UNDO.

  IF NOT CAN-FIND(FIRST TT_pdf_stream
                  WHERE TT_pdf_stream.obj_stream = pdfStream)
  THEN DO:
    RUN pdf_error(pdfStream,"pdf_close","Cannot find Stream!").
    RETURN .
  END.

  DEFINE VARIABLE L_EncryptStatus   AS CHARACTER NO-UNDO.

  FIND FIRST TT_pdf_stream WHERE TT_pdf_stream.obj_stream = pdfStream
       NO-ERROR.

  OUTPUT STREAM S_pdf_out CLOSE.

  OUTPUT STREAM S_pdf_out TO 
         VALUE(  SESSION:TEMP-DIR 
               + TT_pdf_stream.obj_UniqueID  
               + "-Content-" 
               + STRING(pdf_Page(pdfStream)) + ".txt") 
               BINARY NO-MAP NO-CONVERT APPEND.

  /* If the Tool Suite is being used then re-run the appropriate tools.
     Only runs the Tools for pages greater than one since the pdf_tool_create
     should handle the first page */
  IF pdf_Page(pdfStream) >= 1 AND VALID-HANDLE(h_PDF-Tool) THEN DO:
    FOR EACH TT_pdf_tool NO-LOCK:
      IF TT_pdf_tool.tool_type = "TABLE" THEN
        RUN DisplayTableOutline IN h_PDF-tool
            (pdfStream, TT_pdf_tool.tool_name).
    END.
  END.


  /* Ensure that Footer is run for the Last Page */
  IF TT_pdf_stream.obj_Footer <> "" THEN DO:
    pdf_ForFooter = TRUE.

    RUN pdf_set_TextY(pdfStream,pdf_BottomMargin(pdfStream)).

    RUN OutputTextContent(pdfStream, 
                          "TEXT",
                            /* igc - rem CHR(10) +  */ "1 0 0 1 " 
                          + STRING(pdf_LeftMargin(pdfStream))
                          + " " + STRING(pdf_BottomMargin(pdfStream)) 
                          + " Tm",
                          "",
                          "").

    RUN VALUE(TT_pdf_Stream.obj_Footer) IN TT_pdf_stream.obj_CallProc NO-ERROR.

    RUN pdf_set_TextY(pdfStream,pdf_PageHeight(pdfStream)).

    pdf_ForFooter = FALSE.
  END. /* Footer */

  IF TT_pdf_Stream.obj_DoingText THEN
    RUN OutputTextContent(pdfStream, 
                         "TEXT",
                         "ET",
                          "",
                          "").
  ELSE IF TT_pdf_Stream.obj_DoingGraphic THEN
    RUN OutputTextContent(pdfStream, 
                         "GRAPHIC",
                         "Q",
                          "",
                          "").

  /* Reset pdf_page to the last page of the document */
  maxPageNumber = 0.
  /* Calculate current total number of pages */
  FOR EACH tt_pdf_page WHERE TT_pdf_page.obj_stream = pdfStream
                          BY tt_pdf_page.page_nbr DESCENDING:
    maxPageNumber = TT_pdf_page.page_nbr.
    LEAVE.
  END.
  
  ASSIGN TT_pdf_Stream.obj_DoingText    = FALSE
         TT_pdf_Stream.obj_DoingGraphic = FALSE.

  RUN pdf_set_Page(pdfStream,maxPageNumber).

  OUTPUT STREAM S_pdf_out CLOSE.

  /* Now Build PDF File */
  OUTPUT STREAM S_pdf_inc TO VALUE( TT_pdf_stream.obj_file ) UNBUFFERED NO-CONVERT.

    IF pdf_get_parameter(pdfStream,"Encrypt") = "TRUE" THEN DO:

      /* RUN {&PDFDIR}pdfencrypt.p PERSISTENT SET h_PDF-Encrypt (INPUT "{&PDFDIR}"). */

      RUN BuildDocID /* IN h_PDF-encrypt */
                     (INPUT  TT_pdf_stream.obj_UniqueID,
                      OUTPUT TT_pdf_stream.obj_id).

      ASSIGN TT_pdf_stream.obj_master = pdf_get_parameter(pdfStream,"MasterPassword")
             TT_pdf_stream.obj_user   = pdf_get_parameter(pdfStream,"UserPassword").

      RUN DetermineOwnerKey /* IN h_PDF-Encrypt */ 
                            (INPUT pdfStream,
                             INPUT TT_pdf_stream.obj_UniqueID,
                             INPUT TT_pdf_stream.obj_user,
                             INPUT-OUTPUT TT_pdf_stream.obj_master) .

      IF pdf_get_parameter(pdfStream,"EncryptKey") = "40" THEN
        RUN DetermineUserKey40  /* IN h_PDF-Encrypt */ 
                              (INPUT TT_pdf_stream.obj_UniqueID,
                               INPUT TT_pdf_stream.obj_ID,
                               INPUT IF pdf_get_parameter(pdfStream,"AllowPrint") = "FALSE" THEN 0 ELSE 1,
                               INPUT IF pdf_get_parameter(pdfStream,"AllowCopy") = "FALSE" THEN 0 ELSE 1,
                               INPUT IF pdf_get_parameter(pdfStream,"AllowModify") = "FALSE" THEN 0 ELSE 1,
                               INPUT IF pdf_get_parameter(pdfStream,"AllowAnnots") = "FALSE" THEN 0 ELSE 1,
                               INPUT IF pdf_get_parameter(pdfStream,"AllowForms") = "FALSE" THEN 0 ELSE 1,
                               INPUT IF pdf_get_parameter(pdfStream,"AllowExtract") = "FALSE" THEN 0 ELSE 1,
                               INPUT IF pdf_get_parameter(pdfStream,"AllowAssembly") = "FALSE" THEN 0 ELSE 1,
                               OUTPUT pdf-EncryptKeyMemPtr,
                               INPUT-OUTPUT TT_pdf_stream.obj_user,
                               OUTPUT TT_pdf_stream.obj_P).
      ELSE
        RUN DetermineUserKey128
                              (INPUT TT_pdf_stream.obj_UniqueID,
                               INPUT TT_pdf_stream.obj_ID,
                               INPUT IF pdf_get_parameter(pdfStream,"AllowPrint") = "FALSE" THEN 0 ELSE 1,
                               INPUT IF pdf_get_parameter(pdfStream,"AllowCopy") = "FALSE" THEN 0 ELSE 1,
                               INPUT IF pdf_get_parameter(pdfStream,"AllowModify") = "FALSE" THEN 0 ELSE 1,
                               INPUT IF pdf_get_parameter(pdfStream,"AllowAnnots") = "FALSE" THEN 0 ELSE 1,
                               INPUT IF pdf_get_parameter(pdfStream,"AllowForms") = "FALSE" THEN 0 ELSE 1,
                               INPUT IF pdf_get_parameter(pdfStream,"AllowExtract") = "FALSE" THEN 0 ELSE 1,
                               INPUT IF pdf_get_parameter(pdfStream,"AllowAssembly") = "FALSE" THEN 0 ELSE 1,
                               OUTPUT pdf-EncryptKeyMemPtr,
                               INPUT-OUTPUT TT_pdf_stream.obj_user,
                               OUTPUT TT_pdf_stream.obj_P).

      L_Encrypt = TRUE.
    END. /* Encryption has been set */

    /* Output PDF Header Requirements */
    RUN pdf_Header (INPUT TT_pdf_stream.obj_stream, l_Encrypt).

    /* Load 12 Base Fonts - exclude wingdings etc */
    RUN pdf_Encoding (pdfStream).

    RUN pdf_Fonts (pdfStream).

    IF CAN-FIND (FIRST TT_pdf_error
                 WHERE TT_pdf_error.obj_stream = pdfStream NO-LOCK) THEN DO:
      FOR EACH TT_pdf_error:
        MESSAGE TT_pdf_error.obj_func TT_pdf_error.obj_error SKIP
                "stream=" TT_pdf_error.obj_stream view-as alert-box.
      END.
    END.

    ELSE DO:
      /* Load Embedded Fonts */
      IF CAN-FIND( FIRST TT_pdf_font WHERE TT_pdf_font.obj_stream = pdfStream
                                       AND TT_pdf_font.font_file <> "PDFBASE14" 
                                       AND TT_pdf_Font.font_file <> "EXTERNAL" NO-LOCK)
      THEN RUN pdf_Load_Fonts (pdfStream, L_Encrypt).

      /* Load Embedded Images */
      IF CAN-FIND( FIRST TT_pdf_image 
                   WHERE TT_pdf_image.obj_stream = pdfStream NO-LOCK)
      THEN RUN pdf_Load_Images (pdfStream,L_Encrypt).

      /* Load External Pages */
      IF CAN-FIND( FIRST TT_pdf_external 
                   WHERE TT_pdf_external.obj_stream = pdfStream NO-LOCK)
      THEN RUN pdf_Load_External (pdfStream).

      /* Load Annotations */
      IF CAN-FIND( FIRST TT_pdf_annot NO-LOCK)
      THEN RUN pdf_Load_Links (pdfStream, L_Encrypt).

      IF pdf_get_parameter(pdfStream,"Encrypt") = "TRUE" THEN DO:
        ObjectSequence(pdfStream, pdf_inc_ObjectSequence + 1, "Encrypt", 0).
        TT_pdf_stream.obj_encryptdict = pdf_inc_ObjectSequence.

        RUN pdf_EncryptDict(pdfStream, pdf_inc_ObjectSequence).
      END.

      RUN pdf_Resources (pdfStream).
      RUN pdf_Content (pdfStream, L_Encrypt).

      RUN pdf_Xref (pdfStream, L_Encrypt).

      IF TT_pdf_stream.obj_Last <> "" THEN
        RUN VALUE(TT_pdf_Stream.obj_Last) IN TT_pdf_stream.obj_CallProc NO-ERROR.

    END.

  OUTPUT STREAM S_pdf_inc CLOSE.

  /**
  IF TT_pdf_stream.obj_encrypt THEN DO:
    RUN PDFpsp.w
        (INPUT TT_pdf_stream.obj_file,
         INPUT TT_pdf_stream.obj_master,
         INPUT TT_pdf_stream.obj_user,
         INPUT TT_pdf_stream.obj_access,
         INPUT TT_pdf_stream.obj_key,
         INPUT TT_pdf_stream.obj_mode,
         INPUT TT_pdf_stream.obj_silent,
         OUTPUT l_EncryptStatus).
  END.
  **/

  /* Ensure we clean up some memory */
  SET-SIZE(mContent) = 0.
  SET-SIZE(mHolder) = 0.

  /*
  IF VALID-HANDLE(h_PDF-encrypt) THEN
    DELETE PROCEDURE h_PDF-encrypt.
  */

  IF VALID-HANDLE(h_PDF-tool) THEN
    DELETE PROCEDURE h_PDF-tool.

  IF VALID-HANDLE(h_PDF-template) THEN
    DELETE PROCEDURE h_PDF-template.

  RUN ReleaseZlib.
  RUN ReleasePDFencryptlib.

  OUTPUT STREAM S_pdf_out CLOSE.

  /* To Debug page content - comment this code out 
     Delete any pre-existing temp files for stream */
  IF OPSYS = "UNIX" THEN
    OS-COMMAND SILENT VALUE("rm -f " + SESSION:TEMP-DIR + TT_pdf_stream.obj_UniqueID + "*.txt").
  ELSE
    OS-COMMAND SILENT VALUE("del " + SESSION:TEMP-DIR + TT_pdf_Stream.obj_UniqueID + "*.txt").

  RUN pdf_reset_stream(pdfStream).
  
END PROCEDURE. /* pdf_Close */

FUNCTION pdf_PageFooter RETURN LOGICAL (INPUT pdfStream     AS CHARACTER,
                                        INPUT pdfProcHandle AS HANDLE,
                                        INPUT pdfFooterProc AS CHARACTER):

  DEFINE BUFFER B_TT_pdf_stream FOR TT_pdf_Stream.

  FIND FIRST B_TT_pdf_stream
       WHERE B_TT_pdf_stream.obj_stream = pdfStream EXCLUSIVE-LOCK NO-ERROR.
  IF NOT AVAIL B_TT_pdf_stream THEN DO:
    RUN pdf_error(pdfStream,"pdf_PageFooter","Cannot find Stream = " + pdfStream).
    RETURN FALSE.
  END.

  ASSIGN B_TT_pdf_stream.obj_CallProc = pdfProCHandle
         B_TT_pdf_stream.obj_Footer   = pdfFooterProc.

  RETURN TRUE.
END FUNCTION. /* pdf_PageFooter */

FUNCTION pdf_PageHeader RETURN LOGICAL (INPUT pdfStream     AS CHARACTER,
                                        INPUT pdfProcHandle AS HANDLE,
                                        INPUT pdfHeaderProc AS CHARACTER):

  DEFINE BUFFER B_TT_pdf_stream FOR TT_pdf_Stream.

  FIND FIRST B_TT_pdf_stream
       WHERE B_TT_pdf_stream.obj_stream = pdfStream EXCLUSIVE-LOCK NO-ERROR.
  IF NOT AVAIL B_TT_pdf_stream THEN DO:
    RUN pdf_error(pdfStream,"pdf_PageHeader","Cannot find Stream = " + pdfStream).
    RETURN FALSE.
  END.

  ASSIGN B_TT_pdf_stream.obj_CallProc = pdfProCHandle
         B_TT_pdf_stream.obj_Header   = pdfHeaderProc.

  RETURN TRUE.
END FUNCTION. /* pdf_PageHeader */

FUNCTION pdf_LastProcedure RETURN LOGICAL (INPUT pdfStream     AS CHARACTER,
                                           INPUT pdfProcHandle AS HANDLE,
                                           INPUT pdfLastProc   AS CHARACTER):

  DEFINE BUFFER B_TT_pdf_stream FOR TT_pdf_Stream.

  FIND FIRST B_TT_pdf_stream
       WHERE B_TT_pdf_stream.obj_stream = pdfStream EXCLUSIVE-LOCK NO-ERROR.
  IF NOT AVAIL B_TT_pdf_stream THEN DO:
    RUN pdf_error(pdfStream,"pdf_LastProcedure","Cannot find Stream = " + pdfStream).
    RETURN FALSE.
  END.

  ASSIGN B_TT_pdf_stream.obj_CallProc = pdfProCHandle
         B_TT_pdf_stream.obj_Last     = pdfLastProc.

  RETURN TRUE.

END FUNCTION. /* pdf_LastProcedure */

PROCEDURE pdf_set_parameter:
  DEFINE INPUT PARAMETER pdfStream    AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfParameter AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfValue     AS CHARACTER NO-UNDO.

  DEFINE BUFFER B_TT_pdf_param FOR TT_pdf_param.
  DEFINE BUFFER B_TT_pdf_Stream FOR TT_pdf_Stream.

  DEFINE VARIABLE L_Integer       AS INTEGER NO-UNDO.
  DEFINE VARIABLE L_Decimal       AS DECIMAL NO-UNDO.

  DEFINE VARIABLE L_Error         AS LOGICAL INIT FALSE.
  DEFINE VARIABLE L_ErrorMsg      AS CHARACTER NO-UNDO.
  DEFINE VARIABLE L_Valid_params  AS CHARACTER NO-UNDO.


  L_Valid_params = "Compress,Encrypt,UserPassword,MasterPassword,EncryptKey,"
                 + "AllowPrint,AllowCopy,AllowModify,AllowAnnots,AllowForms,"
                 + "AllowExtract,AllowAssembly,"
                 + "LineSpacer," 
                 + "HideToolbar,HideMenubar,HideWindowUI,FitWindow,CenterWindow,DisplayDocTitle,"
                 + "PageMode,PageLayout,"
                 + "UseExternalPageSize,"
                 + "ScaleX,ScaleY,VERSION,"
                 + "UseTags,BoldFont,ItalicFont,BoldItalicFont,DefaultFont,DefaultColor,ItalicCount,BoldCount,ColorLevel".


  FIND FIRST B_TT_pdf_stream
       WHERE B_TT_pdf_stream.obj_stream = pdfStream NO-LOCK NO-ERROR.
  IF NOT AVAIL B_TT_pdf_stream THEN DO:
    RUN pdf_error(pdfStream,"pdf_set_parameter","Cannot find Stream!").
    RETURN.
  END.

  IF (NOT pdfParameter BEGINS "tmp" AND NOT pdfParameter BEGINS "TagColor:")
  AND LOOKUP(pdfParameter,L_valid_params) = 0 THEN DO:
    RUN pdf_error(pdfStream,"pdf_set_parameter","Invalid parameter (" + pdfParameter + ") trying to be set!").
    RETURN.
  END.

  IF NOT pdfParameter BEGINS "tmp" 
  AND NOT pdfParameter BEGINS "TagColor:" THEN DO:
    CASE pdfParameter:
      WHEN "Compress" OR WHEN "Encrypt" OR WHEN "AllowPrint"
        OR WHEN "AllowCopy" OR WHEN "AllowModify" OR WHEN "AllowAnnots"
        OR WHEN "AllowForms" OR WHEN "AllowExtract" OR WHEN "AllowAssembly" 
        OR WHEN "HideToolBar" OR WHEN "HideMenuBar" OR WHEN "HideWindowUI"
        OR WHEN "FitWindow" OR WHEN "CenterWindow" OR WHEN "DisplayDocTitle"
        OR WHEN "UseExternalPageSize" OR WHEN "UseTags"
        THEN DO:
          IF pdfValue <> "TRUE" AND pdfValue <> "FALSE" AND pdfValue <> "" THEN
            ASSIGN L_Error    = TRUE
                   L_ErrorMsg = "Only TRUE, FALSE or blank allowed for '" + pdfParameter + "' Parameter!".

        /* Default the Encryption Key to 40 Bits - Rev 2 */
        IF pdfParameter = "Encrypt" THEN
          RUN pdf_set_parameter(pdfStream,"EncryptKey","40").
      END.

      WHEN "EncryptKey" THEN DO:
        /* Currently only allow for 40-Bit Encryption */
        IF pdfValue <> "40" AND pdfValue <> "128"  THEN
          ASSIGN L_Error    = TRUE
                 L_ErrorMsg = "Only a value of 40 or 128 is allowed for the '" + pdfParameter + "' Parameter!".
      END.

      WHEN "UserPassword" OR WHEN "MasterPassword" THEN DO:
        IF LENGTH(pdfValue, "character":u) > 32 THEN
          ASSIGN L_Error    = TRUE
                 L_ErrorMsg = "Password string cannot be greater than 32 characters for '" + pdfParameter + "' Parameter!".
      END.

      WHEN "LineSpacer" THEN DO:
        L_Integer = INT(pdfValue) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
          ASSIGN L_Error    = TRUE
                 L_ErrorMsg = "'LineSpacer' Parameter must be an integer value!".
      END.

      WHEN "ScaleX" OR WHEN "ScaleY" THEN DO:
        L_Decimal = DEC(pdfValue) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
          ASSIGN L_Error    = TRUE
                 L_ErrorMsg = "'Scale X/Y' Parameters must be decimal values!".
      END.

      WHEN "PageMode" THEN DO:
        IF LOOKUP(pdfValue,"UseNone,UseOutlines,UseThumbs,FullScreen") = 0 THEN
          ASSIGN L_Error    = TRUE
                 L_ErrorMsg = "Invalid entry (" + pdfValue + ") used for PageMode parameter!".
      END. /* PageMode */

      WHEN "PageLayout" THEN DO:
        IF LOOKUP(pdfValue,"SinglePage,OneColumn,TwoColumnLeft,TwoColumnRight") = 0 THEN
          ASSIGN L_Error    = TRUE
                 L_ErrorMsg = "Invalid entry (" + pdfValue + ") used for PageLayout parameter!".
      END. /* PageMode */

    END CASE.

    IF L_Error THEN DO:
      RUN pdf_error(pdfStream,"pdf_set_parameter",L_ErrorMsg).
      RETURN.
    END. /* L_error */
  END.

  FIND B_TT_pdf_param WHERE B_TT_pdf_param.obj_stream    = pdfStream
                        AND B_TT_pdf_param.obj_parameter = pdfParameter
                        EXCLUSIVE NO-ERROR.
  IF NOT AVAIL B_TT_pdf_param THEN DO:
    CREATE B_TT_pdf_param.
    ASSIGN B_TT_pdf_param.obj_stream    = pdfStream
           B_TT_pdf_param.obj_parameter = pdfParameter
           B_TT_pdf_param.obj_value     = pdfValue.
  END.
  ELSE
    ASSIGN B_TT_pdf_param.obj_value = pdfValue.

END. /* pdf_set_parameter */

/* igc - added Novermber 17, 2003 */
PROCEDURE pdf_Encrypt:
  DEFINE INPUT PARAMETER pdfStream  AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfMaster  AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfUser    AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfAccess  AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfKey     AS INTEGER   NO-UNDO.
  DEFINE INPUT PARAMETER pdfMode    AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfSilent  AS LOGICAL   NO-UNDO.

  DEFINE BUFFER B_TT_pdf_Stream FOR TT_pdf_Stream.

  DEFINE VARIABLE l_Loop        AS INTEGER NO-UNDO.

  FIND FIRST B_TT_pdf_stream
       WHERE B_TT_pdf_stream.obj_stream = pdfStream EXCLUSIVE-LOCK NO-ERROR.
  IF NOT AVAIL B_TT_pdf_stream THEN DO:
    RUN pdf_error(pdfStream,"pdf_Encrypt","Cannot find Stream!").
    RETURN.
  END.

  /* if either of the passwords have been supplied ensure that it doesn't match
     the other -- they must be different */
  IF pdfMaster <> "" OR pdfUser <> "" THEN DO:
    IF ENCODE(pdfMaster) = ENCODE(pdfUser) THEN DO:
      RUN pdf_error(pdfStream,"pdf_Encrypt","Master and User Passwords must be different!").
      RETURN .
    END.
  END.

  /* Ensure that the correct Access permissions have been specified -- if not
     error out and do not perform encryption */
  DO l_Loop = 1 TO NUM-ENTRIES(pdfAccess,";"):
    IF LOOKUP(ENTRY(l_Loop,pdfAccess,";"),
              "noprint,nohiresprint,nomodify,nocopy,noannots,noforms,noaccessible,noassemble")
       = 0 THEN DO:
      RUN pdf_error(pdfStream,"pdf_Encrypt","Invalid Access Type passed - " + ENTRY(l_Loop,pdfAccess)).
      RETURN.
    END.
  END.

  IF LOOKUP(STRING(pdfKey),"40,128") = 0 THEN DO:
    RUN pdf_error(pdfStream,"pdf_Encrypt","Invalid Key Length passed - only 40 or 128 allowed!").
    RETURN .
  END.

  IF LOOKUP(pdfMode,"COM,SHARED,OS") = 0 THEN DO:
    RUN pdf_error(pdfStream,"pdf_Encrypt","Invalid MODE passed - only COM, SHARED or OS allowed!").
    RETURN.
  END.

  ASSIGN B_TT_pdf_stream.obj_encrypt = TRUE
         B_TT_pdf_stream.obj_master  = pdfMaster
         B_TT_pdf_stream.obj_user    = pdfUser
         B_TT_pdf_stream.obj_access  = pdfAccess
         B_TT_pdf_stream.obj_key     = pdfKey
         B_TT_pdf_stream.obj_mode    = pdfMode
         B_TT_pdf_stream.obj_silent  = pdfSilent.

END. /* pdf_Encrypt */

/* igc - added November 25, 2003 */
PROCEDURE pdf_Bookmark:
  DEFINE INPUT  PARAMETER pdfStream    AS CHARACTER NO-UNDO.
  DEFINE INPUT  PARAMETER pdfTitle     AS CHARACTER NO-UNDO.
  DEFINE INPUT  PARAMETER pdfParent    AS INTEGER   NO-UNDO.
  DEFINE INPUT  PARAMETER pdfExpand    AS LOGICAL   NO-UNDO.
  DEFINE OUTPUT PARAMETER pdfBookmark  AS INTEGER   NO-UNDO.

  DEFINE BUFFER B_TT_pdf_bookmark FOR TT_pdf_bookmark.
  DEFINE BUFFER BB_TT_pdf_bookmark FOR TT_pdf_bookmark.

  FIND FIRST TT_pdf_stream
       WHERE TT_pdf_stream.obj_stream = pdfStream EXCLUSIVE-LOCK NO-ERROR.
  IF NOT AVAIL TT_pdf_stream THEN DO:
    RUN pdf_error(pdfStream,"pdf_Bookmark","Cannot find Stream!").
    RETURN.
  END.

  IF pdfParent <> 0
  AND NOT CAN-FIND(FIRST TT_pdf_bookmark
                   WHERE TT_pdf_bookmark.obj_stream  = pdfStream
                     AND TT_pdf_bookmark.book_parent = 0 NO-LOCK) THEN DO:

    RUN pdf_error(pdfStream,"pdf_Bookmark","Trying to add a Bookmark without having a Main Level defined!").
    RETURN.
  END.

  /* Find last Bookmark --- used to increment the unique Bookmark Number */
  FIND LAST TT_pdf_bookmark WHERE TT_pdf_bookmark.obj_stream = pdfStream NO-LOCK NO-ERROR.

  CREATE B_TT_pdf_bookmark.
  ASSIGN B_TT_pdf_bookmark.obj_stream  = pdfStream
         B_TT_pdf_bookmark.book_nbr    = IF NOT AVAIL TT_pdf_bookmark THEN
                                           1
                                         ELSE
                                           TT_pdf_bookmark.book_nbr + 1
         B_TT_pdf_bookmark.book_title  = pdfTitle
         B_TT_pdf_bookmark.book_parent = pdfParent
         B_TT_pdf_bookmark.book_expand = pdfExpand.
         B_TT_pdf_bookmark.book_page   = pdf_page(pdfstream).
         B_TT_pdf_bookmark.book_Y      = pdf_TextY(pdfstream)
                                       + pdf_PointSize(pdfStream).

  pdfBookmark = B_TT_pdf_bookmark.book_nbr.

  /* Now that we've added the Bookmark, find the parent and increment the
     parents child counter.  But only when the Parent is not 0 (Main Level) */
  IF pdfParent <> 0 THEN DO:
    FIND FIRST BB_TT_pdf_bookmark
         WHERE BB_TT_pdf_bookmark.obj_stream = pdfStream
           AND BB_TT_pdf_bookmark.book_nbr   = pdfParent NO-ERROR.
    IF AVAIL BB_TT_pdf_bookmark THEN DO:
      IF BB_TT_pdf_bookmark.book_expand THEN
        BB_TT_pdf_bookmark.book_child = BB_TT_pdf_bookmark.book_child + 1.
      ELSE
        BB_TT_pdf_bookmark.book_child = BB_TT_pdf_bookmark.book_child - 1.
    END.
  END. /* Increment Parent Children */

END. /* pdf_Bookmark */

PROCEDURE pdf_process_bookmarks:  /* PRIVATE */
  DEFINE INPUT PARAMETER pStream     AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pParentNbr  AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER pParentObj  AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER pEncrypt    AS LOGICAL NO-UNDO.

  DEFINE BUFFER B_TT_pdf_Bookmark    FOR TT_pdf_bookmark.
  DEFINE BUFFER LB_TT_pdf_Bookmark   FOR TT_pdf_bookmark.
  DEFINE BUFFER LBB_TT_pdf_Bookmark  FOR TT_pdf_bookmark.
  DEFINE BUFFER LBBB_TT_pdf_Bookmark FOR TT_pdf_bookmark.

  DEFINE VARIABLE L_Object           AS INTEGER NO-UNDO.
  DEFINE VARIABLE L_Count            AS INTEGER NO-UNDO.
  DEFINE VARIABLE L_Last             AS INTEGER NO-UNDO.

  DEFINE VARIABLE m_EncryptKey       AS MEMPTR NO-UNDO.

  FIND FIRST LBB_TT_pdf_Bookmark
       WHERE LBB_TT_pdf_Bookmark.obj_stream = pStream
         AND LBB_TT_pdf_bookmark.book_nbr   = pParentNbr NO-LOCK NO-ERROR.

  FOR EACH LB_TT_pdf_bookmark
     WHERE LB_TT_pdf_bookmark.obj_stream  = pStream
       AND LB_TT_pdf_bookmark.book_parent = pParentNbr
       BREAK BY LB_TT_pdf_bookmark.book_nbr:

    ObjectSequence(pStream, pdf_inc_ObjectSequence + 1, "BookMark", 0).
    LB_TT_pdf_bookmark.book_obj = pdf_inc_ObjectSequence.

    /* Find the associated Page Dictionary Object Number */
    FIND FIRST TT_pdf_object
         WHERE TT_pdf_object.obj_stream = pStream
           AND TT_pdf_object.obj_page   = LB_TT_pdf_bookmark.book_page
           NO-ERROR.
    IF NOT AVAIL TT_pdf_object THEN NEXT.
    L_object = TT_pdf_object.obj_nbr.

    PUT STREAM S_pdf_inc UNFORMATTED
        LB_TT_pdf_Bookmark.book_obj " 0 obj" CHR(13)
        "<<" CHR(13).

    IF pEncrypt THEN DO:
      PUT STREAM S_pdf_inc UNFORMATTED
        "/Title <".

      SET-SIZE(m_EncryptKey) = IF pdf_get_parameter(pStream,"EncryptKey") = "40" 
                               THEN 10 ELSE 32.
      RUN GetEncryptKey /* IN h_PDF-Encrypt */
          (INPUT  pStream,
           INPUT  TT_pdf_stream.obj_UniqueID,
           INPUT  pdf_inc_ObjectSequence,
           INPUT  0,
           INPUT  pdf-EncryptKeyMemPtr,
           OUTPUT m_EncryptKey).

      RUN ConvChar2MemPtr(LB_TT_pdf_bookmark.book_title).

      RUN EncryptContent /* IN h_PDF-Encrypt */
                         (INPUT  m_EncryptKey,
                          INPUT  mContent,
                          OUTPUT mContent).

      RUN OutputMemPtrAsHex (mContent).
      SET-SIZE(m_EncryptKey) = 0.

      PUT STREAM S_pdf_inc UNFORMATTED
        ">" CHR(13).
    END.

    ELSE DO:
      RUN pdf_replace_text(INPUT-OUTPUT LB_TT_pdf_bookmark.book_title).

      PUT STREAM S_pdf_inc UNFORMATTED
        "/Title ("
        LB_TT_pdf_bookmark.book_title ")" CHR(13).
    END.

    PUT STREAM S_pdf_inc UNFORMATTED
        "/Parent " LBB_TT_pdf_Bookmark.book_obj " 0 R" CHR(13).

   /* Determine if a Previous Bookmark exists on the same level */
    FIND LAST LBBB_TT_pdf_Bookmark
         WHERE LBBB_TT_pdf_Bookmark.obj_stream  = pStream
           AND LBBB_TT_pdf_Bookmark.book_parent = pParentNbr
           AND LBBB_TT_pdf_Bookmark.book_nbr    < LB_TT_pdf_Bookmark.book_nbr
           NO-LOCK NO-ERROR.
    IF AVAIL LBBB_TT_pdf_bookmark THEN
      PUT STREAM S_pdf_inc UNFORMATTED
          "/Prev " LBBB_TT_pdf_Bookmark.book_obj " 0 R" CHR(13).

   /* Determine if a Following (Next) Bookmark exists on the same level */
    FIND FIRST LBBB_TT_pdf_Bookmark
         WHERE LBBB_TT_pdf_Bookmark.obj_stream  = pStream
           AND LBBB_TT_pdf_Bookmark.book_parent = pParentNbr
           AND LBBB_TT_pdf_Bookmark.book_nbr    > LB_TT_pdf_Bookmark.book_nbr
           NO-LOCK NO-ERROR.
    IF AVAIL LBBB_TT_pdf_bookmark THEN
      PUT STREAM S_pdf_inc UNFORMATTED
          "/Next " pdf_OutlinesDict + LBBB_TT_pdf_Bookmark.book_nbr " 0 R" CHR(13).

    /* If Children are associated with this Bookmark then add some processing */
    IF LB_TT_pdf_bookmark.book_child <> 0 THEN DO:
      FIND LAST B_TT_pdf_Bookmark
          WHERE B_TT_pdf_Bookmark.obj_stream  = pStream
            AND B_TT_pdf_Bookmark.book_parent = LB_TT_pdf_bookmark.book_nbr
            NO-LOCK NO-ERROR.
      IF AVAIL B_TT_pdf_bookmark THEN DO:
        L_Last = pdf_OutlinesDict + B_TT_pdf_Bookmark.book_nbr.
      END.
      ELSE
        L_Last = pdf_OutlinesLast.

      PUT STREAM S_pdf_inc UNFORMATTED
          "~/First " LB_TT_pdf_Bookmark.book_obj + 1 " 0 R" CHR(13)
          "~/Last "  L_Last " 0 R" CHR(13)
          "~/Count " LB_TT_pdf_Bookmark.book_child CHR(13).

    END. /* Child <> 0 */

    PUT STREAM S_pdf_inc UNFORMATTED
        "/Dest [ " l_Object " 0 R /XYZ 0 " LB_TT_pdf_bookmark.book_Y " 0 ]"
        CHR(13)
         ">>" CHR(13)
        "endobj" CHR(13).

    IF LB_TT_pdf_Bookmark.book_child <> 0 THEN
      RUN pdf_process_bookmarks(pStream, LB_TT_pdf_bookmark.book_nbr, LBB_TT_pdf_bookmark.book_obj, pEncrypt).
  END.

END. /* pdf_process_bookmarks */

PROCEDURE pdf_load_bookmarks:  /* PRIVATE */

  DEFINE INPUT PARAMETER pdfStream  AS CHARACTER NO-UNDO.

  DEFINE VARIABLE L_Last    AS INTEGER NO-UNDO.
  DEFINE VARIABLE L_Object  AS INTEGER NO-UNDO.

  DEFINE VARIABLE L_Encrypt AS LOGICAL NO-UNDO INIT FALSE.

  DEFINE VARIABLE m_EncryptKey  AS MEMPTR NO-UNDO.

  DEFINE BUFFER B_TT_pdf_bookmark FOR TT_pdf_bookmark.

  IF pdf_get_parameter(pdfStream,"Encrypt") = "TRUE" THEN
    L_Encrypt = TRUE.

  FOR EACH TT_pdf_bookmark WHERE TT_pdf_bookmark.obj_stream = pdfStream
                             AND TT_pdf_bookmark.book_parent = 0
     BREAK BY TT_pdf_bookmark.book_nbr:

    IF FIRST(TT_pdf_bookmark.book_nbr) THEN DO:
      ObjectSequence(pdfStream, pdf_inc_ObjectSequence + 1, "BookMark", 0).
      TT_pdf_bookmark.book_obj = pdf_inc_ObjectSequence.
      pdf_OutlinesDict = pdf_inc_ObjectSequence.

      /* Determine the total number of Bookmarks in the document */
      FIND LAST B_TT_pdf_bookmark WHERE B_TT_pdf_bookmark.obj_stream = pdfStream NO-LOCK NO-ERROR.
      pdf_OutlinesLast = pdf_OutlinesDict + B_TT_pdf_bookmark.book_nbr.

      /* Output the Bookmark Dictionary */
      PUT STREAM S_pdf_inc UNFORMATTED
         pdf_OutlinesDict " 0 obj" CHR(13)
          "<< " CHR(13)
          "/Type /Outlines" CHR(13)
          "/First " pdf_OutlinesDict + 1 " 0 R" CHR(13)
          "/Last " pdf_OutlinesLast " 0 R" CHR(13)
          "/Count " B_TT_pdf_Bookmark.book_nbr CHR(13)
          ">>" CHR(13)
          "endobj" CHR(13).

    END. /* FIRST */

    ObjectSequence(pdfStream, pdf_inc_ObjectSequence + 1, "BookMark", 0).
    TT_pdf_bookmark.book_obj = pdf_inc_ObjectSequence.

    /* Find the associated Page Dictionary Object Number */
    FIND FIRST TT_pdf_object
         WHERE TT_pdf_object.obj_stream = pdfStream
           AND TT_pdf_object.obj_page   = TT_pdf_bookmark.book_page
           NO-ERROR.
    IF NOT AVAIL TT_pdf_object THEN NEXT.
    L_object = TT_pdf_object.obj_nbr.


    PUT STREAM S_pdf_inc UNFORMATTED
        TT_pdf_Bookmark.book_obj " 0 obj" CHR(13)
        "<<" CHR(13).

    IF l_Encrypt THEN DO:
      PUT STREAM S_pdf_inc UNFORMATTED
        "/Title <".

      SET-SIZE(m_EncryptKey) = IF pdf_get_parameter(pdfStream,"EncryptKey") = "40" 
                               THEN 10 ELSE 32.      
      RUN GetEncryptKey /* IN h_PDF-Encrypt */
          (INPUT  pdfStream,
           INPUT  TT_pdf_stream.obj_UniqueID,
           INPUT  pdf_inc_ObjectSequence,
           INPUT  0,
           INPUT  pdf-EncryptKeyMemPtr,
           OUTPUT m_EncryptKey).

      RUN ConvChar2MemPtr(TT_pdf_bookmark.book_title).

      RUN EncryptContent /* IN h_PDF-Encrypt */
                         (INPUT  m_EncryptKey,
                          INPUT  mContent,
                          OUTPUT mContent).

      RUN OutputMemPtrAsHex (mContent).

      PUT STREAM S_pdf_inc UNFORMATTED
        ">" CHR(13).

    END.

    ELSE DO:
      RUN pdf_replace_text(INPUT-OUTPUT TT_pdf_bookmark.book_title).

      PUT STREAM S_pdf_inc UNFORMATTED
          "/Title ("
          TT_pdf_bookmark.book_title
          ")" CHR(13).
    END.

    PUT STREAM S_pdf_inc UNFORMATTED
         "/Parent " pdf_OutlinesDict " 0 R" CHR(13).

   /* Determine if a Previous Bookmark exists on the same level */
    FIND LAST B_TT_pdf_Bookmark
         WHERE B_TT_pdf_Bookmark.obj_stream  = pdfStream
           AND B_TT_pdf_Bookmark.book_parent = 0
           AND B_TT_pdf_Bookmark.book_nbr    < TT_pdf_Bookmark.book_nbr
           NO-LOCK NO-ERROR.
    IF AVAIL B_TT_pdf_bookmark THEN
      PUT STREAM S_pdf_inc UNFORMATTED
         "/Prev " B_TT_pdf_Bookmark.book_obj " 0 R" CHR(13).

   /* Determine if a Following (Next) Bookmark exists on the same level */
    FIND FIRST B_TT_pdf_Bookmark
         WHERE B_TT_pdf_Bookmark.obj_stream  = pdfStream
           AND B_TT_pdf_Bookmark.book_parent = 0
           AND B_TT_pdf_Bookmark.book_nbr    > TT_pdf_Bookmark.book_nbr
           NO-LOCK NO-ERROR.
    IF AVAIL B_TT_pdf_bookmark THEN
      PUT STREAM S_pdf_inc UNFORMATTED
          "~/Next " pdf_OutlinesDict + B_TT_pdf_Bookmark.book_nbr " 0 R" CHR(13).

    /* If Children are associated with this Bookmark then add some processing */
    IF TT_pdf_bookmark.book_child <> 0 THEN DO:
      FIND LAST B_TT_pdf_Bookmark
          WHERE B_TT_pdf_Bookmark.obj_stream  = pdfStream
            AND B_TT_pdf_Bookmark.book_parent = TT_pdf_bookmark.book_nbr
            NO-LOCK NO-ERROR.
     IF AVAIL B_TT_pdf_bookmark THEN DO:
       L_Last = pdf_OutlinesDict + B_TT_pdf_Bookmark.book_nbr.
     END.
     ELSE
       L_Last = pdf_OutlinesLast.

     PUT STREAM S_pdf_inc UNFORMATTED
          "~/First " TT_pdf_Bookmark.book_obj + 1 " 0 R" CHR(13)
          "~/Last "  L_Last " 0 R" CHR(13)
          "~/Count " TT_pdf_Bookmark.book_child CHR(13).
    END.

    SET-SIZE(m_EncryptKey) = 0.

    PUT STREAM S_pdf_inc UNFORMATTED
        "/Dest [ " L_Object " 0 R /XYZ 0 " TT_pdf_bookmark.book_Y " 0 ]" CHR(13)
        ">>" CHR(13)
        "endobj" CHR(13).

    RUN pdf_process_bookmarks(pdfStream, TT_pdf_bookmark.book_nbr, TT_pdf_bookmark.book_obj, L_Encrypt).

  END. /* each TT_pdf_annot */

  SET-SIZE(m_EncryptKey) = 0.
  SET-SIZE(mContent) = 0.

END. /* pdf_load_bookmarks */

/* ************************************************* */
/*                  PEKI's procedures                */
/* ************************************************* */
PROCEDURE pdf_merge_stream :

    DEF INPUT        PARAM pdfStreamFrom         AS CHAR         NO-UNDO.
    DEF INPUT        PARAM pdfStreamTo           AS CHAR         NO-UNDO.
    DEF INPUT        PARAM pdfNbrCopies          AS INT          NO-UNDO.

    /* Empty Temp-Table */
    FOR EACH TT-Merge-Pages:
       DELETE TT-Merge-Pages.
    END.

    /* No Merge done when error is found */
    IF CAN-FIND (FIRST TT_pdf_error
                 WHERE TT_pdf_error.obj_stream = pdfStreamFrom NO-LOCK)
    OR TRIM(pdfStreamFrom) = ""
    OR TRIM(pdfStreamTo  ) = ""
       THEN RETURN.

    IF pdfNbrCopies <= 0
       THEN ASSIGN pdfNbrCopies = 1.

    RUN pdf_merge_stream_content(INPUT pdfStreamFrom,
                                 INPUT pdfStreamTo  ,
                                 INPUT pdfNbrCopies ).
    RUN pdf_merge_stream_page   (INPUT pdfStreamFrom,
                                 INPUT pdfStreamTo  ,
                                 INPUT pdfNbrCopies ).
    RUN pdf_merge_stream_param  (INPUT pdfStreamFrom,
                                 INPUT pdfStreamTo  ).
    RUN pdf_merge_stream_image  (INPUT pdfStreamFrom,
                                 INPUT pdfStreamTo  ).
    RUN pdf_merge_stream_diff   (INPUT pdfStreamFrom,
                                 INPUT pdfStreamTo  ).
    RUN pdf_merge_stream_link   (INPUT pdfStreamFrom,
                                 INPUT pdfStreamTo  ,
                                 INPUT pdfNbrCopies ).
    RUN pdf_merge_stream_book   (INPUT pdfStreamFrom,
                                 INPUT pdfStreamTo  ,
                                 INPUT pdfNbrCopies ).
    RUN pdf_merge_stream_Fonts  (INPUT pdfStreamFrom,
                                 INPUT pdfStreamTo  ).

END PROCEDURE. /* pdf_merge_stream */

PROCEDURE pdf_merge_stream_content : /* PRIVATE */

    DEF INPUT        PARAM pdfStreamFrom         AS CHAR         NO-UNDO.
    DEF INPUT        PARAM pdfStreamTo           AS CHAR         NO-UNDO.
    DEF INPUT        PARAM pdfNbrCopies          AS INT          NO-UNDO.


    DEF VAR vLastPage         AS INT             NO-UNDO.
    DEF VAR vCount            AS INT             NO-UNDO.
    DEF VAR vPage             AS INT             NO-UNDO.

    DEFINE BUFFER B_TT_pdf_stream  FOR TT_pdf_stream. 
    DEFINE BUFFER B_Stream_From    FOR TT_pdf_stream. 

    FIND B_TT_pdf_stream
         WHERE B_TT_pdf_stream.obj_stream = pdfStreamTo NO-LOCK NO-ERROR.
    IF NOT AVAIL B_TT_pdf_stream THEN DO:
      RUN pdf_error(pdfStreamTo,"pdf_merge_stream_content","Cannot find 'To' Stream = " + pdfStreamTo).
      RETURN.
    END.

    FIND B_Stream_From
         WHERE B_Stream_From.obj_stream = pdfStreamFrom NO-LOCK NO-ERROR.
    IF NOT AVAIL B_Stream_From THEN DO:
      RUN pdf_error(pdfStreamTo,"pdf_merge_stream_content","Cannot find 'From' Stream = " + pdfStreamTo).
      RETURN.
    END.
    
    /* Run the Page Footer for the parent stream before merging the child 
       streams */
    OUTPUT STREAM S_pdf_out CLOSE.
    OUTPUT STREAM S_pdf_out
         TO VALUE( SESSION:TEMP-DIR 
                   + B_TT_pdf_stream.obj_UniqueID  
                   + "-Content-" 
                   + STRING(pdf_Page(pdfStreamTo)) + ".txt") APPEND.

    IF B_TT_pdf_Stream.obj_footer <> "" THEN
      RUN VALUE(B_TT_pdf_Stream.obj_footer) IN B_TT_pdf_stream.obj_CallProc NO-ERROR.

    IF B_TT_pdf_Stream.obj_DoingText THEN
      RUN OutputTextContent(B_TT_pdf_stream.obj_stream, 
                           "TEXT",
                           "ET",
                            "",
                            "").
    ELSE IF B_TT_pdf_Stream.obj_DoingGraphic THEN
      RUN OutputTextContent(B_TT_pdf_stream.obj_stream, 
                           "GRAPHIC",
                           "Q",
                            "",
                            "").

    ASSIGN B_TT_pdf_Stream.obj_DoingText    = FALSE
           B_TT_pdf_Stream.obj_DoingGraphic = FALSE.
    OUTPUT STREAM S_pdf_out CLOSE.

    DO vCount = 1 TO pdfNbrCopies:

        /* Find Current Page */
        ASSIGN vLastPage = pdf_Page(pdfStreamTo).
        IF NOT ( vLastPage >= 1 )
           THEN ASSIGN vLastPage = 0.

        /* Now copy the pages of the 'From' stream */
        DO vPage = 1 TO pdf_Page(pdfStreamFrom):
          OS-COPY VALUE(  SESSION:TEMP-DIR 
                          + B_Stream_From.obj_UniqueID  
                          + "-Content-" 
                          + STRING(vPage) + ".txt") 
                  VALUE(  SESSION:TEMP-DIR 
                          + B_TT_pdf_stream.obj_UniqueID  
                          + "-Content-" 
                          + STRING(vLastPage + 1) + ".txt") .

          /* Save the NEW page indications for other copies */
          IF NOT CAN-FIND (TT-Merge-Pages WHERE TT-Merge-Pages.PageFrom = vPage
                                            AND TT-Merge-Pages.PageTo   = vLastPage + 1
                                            AND TT-Merge-Pages.MergeNr  = vCount)
            THEN DO:
                 CREATE TT-Merge-Pages.
                 ASSIGN TT-Merge-Pages.PageFrom = vPage
                        TT-Merge-Pages.PageTo   = vLastPage + 1
                        TT-Merge-Pages.MergeNr  = vCount.
            END.

            /* Replace a piece of text, for example: 'Title' to 'Copy Title' */
            FOR EACH TT_pdf_ReplaceTxt WHERE TT_pdf_ReplaceTxt.obj_Stream = pdfStreamFrom
                                         AND TT_pdf_ReplaceTxt.MergeNr    = vCount
                                         NO-LOCK:
              RUN ChangePageText
                  (pdfStreamTo,
                   vLastpage + 1,
                   TT_pdf_ReplaceTxt.txt_from,
                   TT_pdf_ReplaceTxt.txt_to).

            END.

            vLastPage = vLastPage + 1.

        END. /* each page of 'From' Stream */

        /* Set the last page */
        RUN pdf_set_Page(pdfStreamTo,vLastPage).
        
    END. /* END pdfCount = 1 TO pdfNbrCopies: */

    /* Reset the pointer to current Stream, is disturbed by calling pdf_Page function */
    FIND FIRST TT_pdf_stream
         WHERE TT_pdf_stream.obj_stream = pdfStreamFrom NO-LOCK NO-ERROR.
    
END PROCEDURE. /* pdf_merge_stream_content */

PROCEDURE pdf_merge_stream_page: /* PRIVATE */

    DEF INPUT        PARAM pdfStreamFrom         AS CHAR         NO-UNDO.
    DEF INPUT        PARAM pdfStreamTo           AS CHAR         NO-UNDO.
    DEF INPUT        PARAM pdfNbrCopies          AS INT          NO-UNDO.

    DEFINE BUFFER B_TT_pdf_page FOR TT_pdf_page.

    DEF VAR vCount         AS INT             NO-UNDO.

    DO vCount = 1 TO pdfNbrCopies:

        FOR EACH TT-Merge-Pages WHERE TT-Merge-Pages.MergeNr = vCount
                                NO-LOCK
             ,FIRST TT_pdf_page WHERE TT_pdf_page.obj_stream = pdfStreamFrom
                                  AND TT_pdf_page.page_nbr   = TT-Merge-Pages.PageFrom
                                NO-LOCK :

                CREATE B_TT_pdf_page.
                BUFFER-COPY TT_pdf_page   EXCEPT TT_pdf_page.obj_stream
                                                 TT_pdf_page.page_nbr
                         TO B_TT_pdf_page ASSIGN B_TT_pdf_page.page_nbr   = TT-Merge-Pages.PageTo
                                                 B_TT_pdf_page.obj_stream = pdfStreamTo.

                RELEASE B_TT_pdf_page.

        END.
    END.

END. /* pdf_merge_stream_page */


PROCEDURE pdf_merge_stream_link : /* PRIVATE */

    DEF INPUT        PARAM pdfStreamFrom         AS CHAR         NO-UNDO.
    DEF INPUT        PARAM pdfStreamTo           AS CHAR         NO-UNDO.
    DEF INPUT        PARAM pdfNbrCopies          AS INT          NO-UNDO.

    DEFINE BUFFER B_TT_pdf_annot FOR TT_pdf_annot.

    DEF VAR vCount         AS INT             NO-UNDO.

    DO vCount = 1 TO pdfNbrCopies:

        FOR EACH TT-Merge-Pages WHERE TT-Merge-Pages.MergeNr = vCount
                                NO-LOCK
           ,EACH TT_pdf_annot    WHERE TT_pdf_annot.obj_stream = pdfStreamFrom
                                  AND TT_pdf_annot.annot_page  = TT-Merge-Pages.PageFrom
                                NO-LOCK:
    
                CREATE B_TT_pdf_annot.
                BUFFER-COPY TT_pdf_annot   EXCEPT TT_pdf_annot.obj_stream
                                                 TT_pdf_annot.annot_page
                         TO B_TT_pdf_annot.

                ASSIGN B_TT_pdf_annot.obj_stream = pdfStreamTo.
                ASSIGN B_TT_pdf_annot.annot_page  = TT-Merge-Pages.PageTo.

                RELEASE B_TT_pdf_annot.
        END.
    END.

END PROCEDURE. /* pdf_merge_stream_link */


PROCEDURE pdf_merge_stream_book : /* PRIVATE */

    DEF INPUT        PARAM pdfStreamFrom         AS CHAR         NO-UNDO.
    DEF INPUT        PARAM pdfStreamTo           AS CHAR         NO-UNDO.
    DEF INPUT        PARAM pdfNbrCopies          AS INT          NO-UNDO.

    DEFINE BUFFER B_TT_pdf_bookmark FOR TT_pdf_bookmark.

    DEF VAR vBookNr        AS INT             NO-UNDO.
    DEF VAR vCount         AS INT             NO-UNDO.

    DO vCount = 1 TO pdfNbrCopies:

        FOR EACH TT-Merge-Pages WHERE TT-Merge-Pages.MergeNr = vCount NO-LOCK :

            /* Set the Qty to add to TT_pdf_bookmark.book_nbr */
            FIND LAST TT_pdf_bookmark WHERE TT_pdf_bookmark.obj_stream  = pdfStreamTo NO-LOCK NO-ERROR.
            IF AVAILABLE TT_pdf_bookmark
               THEN ASSIGN vBookNr = TT_pdf_bookmark.book_nbr.

            FOR EACH TT_pdf_bookmark WHERE TT_pdf_bookmark.obj_stream = pdfStreamFrom
                                       AND TT_pdf_bookmark.book_page  = TT-Merge-Pages.PageFrom
                                     NO-LOCK
                                     BREAK BY TT_pdf_bookmark.book_nbr:

                CREATE B_TT_pdf_bookmark.
                BUFFER-COPY TT_pdf_bookmark EXCEPT TT_pdf_bookmark.obj_stream
                                                   TT_pdf_bookmark.book_nbr
                                                   TT_pdf_bookmark.book_obj
                                                   TT_pdf_bookmark.book_first
                                                   TT_pdf_bookmark.book_last
                         TO B_TT_pdf_bookmark.

                ASSIGN B_TT_pdf_bookmark.book_nbr    = TT_pdf_bookmark.book_nbr + vBookNr.
                ASSIGN B_TT_pdf_bookmark.obj_stream  = pdfStreamTo.
                ASSIGN B_TT_pdf_bookmark.book_page   = TT-Merge-Pages.PageTo.

                IF TT_pdf_bookmark.book_parent <> 0
                   THEN ASSIGN B_TT_pdf_bookmark.book_parent = TT_pdf_bookmark.book_parent + vBookNr.

                RELEASE B_TT_pdf_bookmark.
           END.
        END.
    END.

END PROCEDURE. /* pdf_merge_stream_book */

PROCEDURE pdf_merge_stream_Fonts : /* PRIVATE */

  DEF INPUT PARAMETER pdfStreamFrom         AS CHAR NO-UNDO.
  DEF INPUT PARAMETER pdfStreamTo           AS CHAR NO-UNDO.

  DEFINE BUFFER B_TT_pdf_Font FOR TT_pdf_Font.

  FOR EACH TT_pdf_Font 
     WHERE TT_pdf_Font.obj_stream = pdfStreamFrom
     NO-LOCK TRANSACTION:

    IF NOT CAN-FIND(FIRST B_TT_pdf_Font WHERE B_TT_pdf_Font.obj_stream = pdfStreamTo
                                              AND B_TT_pdf_Font.font_name  = TT_pdf_Font.font_name)
    THEN DO:
      CREATE B_TT_pdf_Font.
      BUFFER-COPY TT_pdf_Font EXCEPT   TT_pdf_Font.obj_stream
               TO B_TT_pdf_Font ASSIGN B_TT_pdf_Font.obj_Stream = pdfStreamTo.
    END.
    
    RELEASE B_TT_pdf_Font NO-ERROR.

  END.

END PROCEDURE.  /* pdf_merger_stream_fonts */

PROCEDURE pdf_merge_stream_param : /* PRIVATE */

    DEF INPUT        PARAM pdfStreamFrom         AS CHAR         NO-UNDO.
    DEF INPUT        PARAM pdfStreamTo           AS CHAR         NO-UNDO.

    DEFINE BUFFER B_TT_pdf_param FOR TT_pdf_param.

    FOR EACH TT_pdf_param WHERE TT_pdf_param.obj_stream     = pdfStreamFrom
                            AND TT_pdf_param.obj_parameter <> "Page"
                          NO-LOCK TRANSACTION:
    
        FIND FIRST B_TT_pdf_param WHERE B_TT_pdf_param.obj_stream    = pdfStreamTo
                                    AND B_TT_pdf_param.obj_parameter = TT_pdf_param.obj_parameter
                                  EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
        IF NOT AVAILABLE B_TT_pdf_param
        THEN DO:
             CREATE B_TT_pdf_param.
             ASSIGN B_TT_pdf_param.obj_stream    = pdfStreamTo
                    B_TT_pdf_param.obj_parameter = TT_pdf_param.obj_parameter.
        END.

        ASSIGN B_TT_pdf_param.obj_valid = TT_pdf_param.obj_valid
               B_TT_pdf_param.obj_value = TT_pdf_param.obj_value.

        RELEASE B_TT_pdf_param.

    END.

END PROCEDURE.  /* pdf_merger_stream_param */


PROCEDURE pdf_merge_stream_image : /* PRIVATE */

    DEF INPUT        PARAM pdfStreamFrom         AS CHAR         NO-UNDO.
    DEF INPUT        PARAM pdfStreamTo           AS CHAR         NO-UNDO.

    DEFINE BUFFER B_TT_pdf_image FOR TT_pdf_image.

    FOR EACH TT_pdf_image WHERE TT_pdf_image.obj_stream = pdfStreamFrom
                          NO-LOCK TRANSACTION:
    
        IF CAN-FIND(FIRST B_TT_pdf_image WHERE B_TT_pdf_image.obj_stream = pdfStreamTo
                                           AND B_TT_pdf_image.image_name = TT_pdf_image.image_name
                                         NO-LOCK)
        THEN NEXT.

        BUFFER-COPY TT_pdf_image  EXCEPT TT_pdf_image.obj_stream
                 TO B_TT_pdf_image.

        ASSIGN B_TT_pdf_image.obj_stream = pdfStreamTo.

        RELEASE B_TT_pdf_image.

    END.

END PROCEDURE. /* pdf_merge_stream_image */


PROCEDURE pdf_merge_stream_diff : /* PRIVATE */

    DEF INPUT        PARAM pdfStreamFrom         AS CHAR         NO-UNDO.
    DEF INPUT        PARAM pdfStreamTo           AS CHAR         NO-UNDO.

    DEFINE BUFFER B_TT_pdf_diff FOR TT_pdf_diff.

    FOR EACH TT_pdf_diff WHERE TT_pdf_diff.obj_stream = pdfStreamFrom
                         NO-LOCK TRANSACTION:
    
        IF CAN-FIND(FIRST B_TT_pdf_diff WHERE B_TT_pdf_diff.obj_stream = pdfStreamTo
                                          AND B_TT_pdf_diff.font_name  = TT_pdf_diff.font_name
                                        NO-LOCK)
        THEN NEXT.

        BUFFER-COPY TT_pdf_diff  EXCEPT TT_pdf_diff.obj_stream
                 TO B_TT_pdf_diff.

        ASSIGN B_TT_pdf_diff.obj_stream = pdfStreamTo.

        RELEASE B_TT_pdf_diff.

    END.

END PROCEDURE.  /* pdf_merge_stream_diff */


PROCEDURE pdf_ReplaceText:

   DEFINE INPUT PARAMETER pdfStream     AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER pdfMergeNbr   AS INTEGER   NO-UNDO.
   DEFINE INPUT PARAMETER pdfTextFrom   AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER pdfTextTo     AS CHARACTER NO-UNDO.

   CREATE TT_pdf_ReplaceTxt.
   ASSIGN TT_pdf_ReplaceTxt.obj_stream  = pdfStream
          TT_pdf_ReplaceTxt.mergenr     = pdfMergeNbr
          TT_pdf_ReplaceTxt.txt_from    = pdfTextFrom
          TT_pdf_ReplaceTxt.txt_to      = pdfTextTo.

END PROCEDURE. /* pdf_ReplaceText */

/* END PEKI's procedures ******************************* */

PROCEDURE pdf_EncryptDict: /* PRIVATE */
  DEFINE INPUT PARAMETER pdfStream AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfObject AS INTEGER   NO-UNDO.

  FIND TT_pdf_stream WHERE TT_pdf_stream.obj_stream = pdfStream NO-ERROR.

  PUT STREAM S_pdf_inc UNFORMATTED
      pdfObject " 0 obj" {&pdfSKIP}
      "<<" {&pdfSKIP}
      "/Filter /Standard" {&pdfSKIP}
      "/V " (IF pdf_get_parameter(pdfStream,"EncryptKey") = "40" THEN "1" ELSE "2") {&pdfSKIP}
      "/R " (IF pdf_get_parameter(pdfStream,"EncryptKey") = "40" THEN "2" ELSE "3") {&pdfSKIP}.

  IF pdf_get_parameter(pdfStream,"EncryptKey") = "128" THEN
    PUT STREAM S_pdf_inc UNFORMATTED
        "/Length 128" {&pdfSKIP}.

  PUT STREAM S_pdf_inc UNFORMATTED
      "/O<" TT_pdf_stream.obj_master ">" {&pdfSKIP}
      "/U<" TT_pdf_stream.obj_user  ">" {&pdfSKIP}
      "/P " TT_pdf_stream.obj_P {&pdfSKIP}
      ">>" {&pdfSKIP}
      "endobj" {&pdfSKIP}.

END. /* pdf_EncryptDict */

PROCEDURE OutputMemPtr: /* PRIVATE */

  DEFINE INPUT PARAMETER pdfStream  AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pHasStream AS LOGICAL NO-UNDO.
  DEFINE INPUT PARAMETER pUniqueID  AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pObjSeq    AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER pMemPtr    AS MEMPTR NO-UNDO.

  DEFINE VARIABLE l_Header      AS CHARACTER NO-UNDO.

  DEFINE VARIABLE l_Loop        AS INTEGER NO-UNDO.
  DEFINE VARIABLE l_Bytes       AS INTEGER NO-UNDO.

  DEFINE VARIABLE L_TempMem     AS MEMPTR NO-UNDO.
  DEFINE VARIABLE L_EncryptMem  AS MEMPTR NO-UNDO.
  DEFINE VARIABLE L_EncryptKey  AS MEMPTR NO-UNDO.

  IF pdf_get_parameter(pdfStream,"Encrypt") = "TRUE" THEN DO:

    /* If the Word Stream will appear in the MemPtr then extract the text
       previous to and including string 'stream' from the MemPtr as we don't
       want that encrypted -- only the stream contents get encryted */
    IF pHasStream THEN DO:
      /* message "HasStream=" get-string(pMemPtr,1,100) view-as alert-box. */

      l_Bytes = GET-SIZE(pMemPtr).
      DO l_Loop = 1 TO l_Bytes:
        IF GET-BYTES(pMemPtr,l_Loop,6) = "stream" THEN DO:
          IF GET-BYTE(pMemPtr,l_Loop + 6) = 10 THEN DO:
            l_Header = GET-STRING(pMemPtr,1,l_Loop + 6) NO-ERROR.
            SET-SIZE(L_TempMem) = GET-SIZE(pMemPtr) - (L_Loop + 6).
            L_TempMem = GET-BYTES(pMemPtr,L_Loop + 6, GET-SIZE(l_TempMem)).
          END.

          SET-SIZE(pMemPtr) = 0.
          SET-SIZE(pMemPtr) = GET-SIZE(L_TempMem).
          pMemPtr = l_TempMem.

          SET-SIZE(l_TempMem) = 0.
          LEAVE.
        END.

      END. /* iLoop */

      IF l_Header <> "" THEN
        PUT STREAM S_PDF_inc UNFORMATTED l_Header.

    END. /* HasStream */


    SET-SIZE(L_EncryptKey) = IF pdf_get_parameter(pdfStream,"EncryptKey") = "40" 
                             THEN 10 ELSE 32.
    RUN GetEncryptKey /* IN h_PDF-Encrypt */
        (INPUT  pdfStream,
         INPUT  pUniqueID,
         INPUT  pObjSeq,
         INPUT  0,
         INPUT  pdf-EncryptKeyMemPtr,
         OUTPUT L_EncryptKey).

    RUN EncryptContent /* IN h_PDF-Encrypt */
                       (INPUT  L_EncryptKey,
                        INPUT  pMemPtr,
                        OUTPUT L_EncryptMem).

    RUN PutFileContent ( L_EncryptMem ).

    SET-SIZE(L_EncryptMem) = 0.
    SET-SIZE(L_EncryptKey) = 0.
  END. /* Encrypted */

  ELSE
    RUN PutFileContent(pMemPtr).

END. /* OutputMemPtr */

PROCEDURE OutputMemPtrAsHex: /* PRIVATE */
  DEFINE INPUT PARAMETER pMemPtr AS MEMPTR NO-UNDO.

  DEFINE VARIABLE l_Loop  AS INTEGER NO-UNDO.

  DO l_Loop = 1 TO GET-SIZE(pMemPtr):
    IF GET-BYTE(pMemPtr,l_Loop) = -1  THEN
      PUT STREAM S_PDF_inc "00".
    ELSE
      PUT STREAM S_PDF_inc UNFORMATTED
          SUBSTR(Hex( ASC(CHR(GET-BYTE(pMemPtr,l_Loop)))),3,2, "character":u).
  END.
END. /* OutputMemPtr */

PROCEDURE AddCharContent : /* PRIVATE */

  DEFINE INPUT PARAMETER pText  AS CHARACTER NO-UNDO.

  PUT UNFORMATTED ptext SKIP.

END. /* AddCharContent */

PROCEDURE ConvChar2MemPtr : /* PRIVATE */

  DEFINE INPUT PARAMETER pText  AS CHARACTER NO-UNDO.

  DEFINE VARIABLE mWork     AS MEMPTR NO-UNDO.
  
  /* Set the Working Memptr and store the new text */
  SET-SIZE(mWork) = 0.
  SET-SIZE(mWork) = LENGTH(pText, "character":u).
  
  /* Now Add the Original plus the Working to the Content MemPtr */
  SET-SIZE(mContent) = 0.
  SET-SIZE(mContent) = GET-SIZE(mWork) + 1.
  
  PUT-STRING(mContent,1) = pText.

  SET-SIZE(mWork) = 0.
  
END. /* ConvChar2MemPtr */

PROCEDURE pdf_tool_add : 

  DEFINE INPUT PARAMETER pdfStream      AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfToolName    AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfToolType    AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfToolData    AS HANDLE    NO-UNDO.

  IF NOT CAN-FIND(FIRST TT_pdf_stream
                  WHERE TT_pdf_stream.obj_stream = pdfStream NO-LOCK)
  THEN DO:
    RUN pdf_error(pdfStream,"pdf_tool_add","Cannot find Stream!").
    RETURN .
  END.

  IF INDEX(pdfToolName," ") > 0 THEN DO:
    RUN pdf_error(pdfStream,"pdf_tool_add","Tool Name cannot contain spaces!").
    RETURN .
  END.


  CREATE TT_pdf_tool.
  ASSIGN TT_pdf_tool.obj_stream   = pdfStream
         TT_pdf_tool.tool_name    = pdfToolName
         TT_pdf_tool.tool_Type    = pdfToolType
         TT_pdf_tool.tool_handle  = pdfToolData.

  /* Now configure a bunch of defaults for the Tool */
  CASE TT_pdf_tool.tool_type:
    WHEN "TABLE" THEN DO:
      RUN pdf_set_tool_parameter(pdfStream,pdfToolName,"Outline",0,"0").
      RUN pdf_set_tool_parameter(pdfStream,pdfToolName,"HeaderFont",0,"Courier-Bold").
      RUN pdf_set_tool_parameter(pdfStream,pdfToolName,"HeaderFontSize",0,"10").
      RUN pdf_set_tool_parameter(pdfStream,pdfToolName,"HeaderBGColor",0,"255,255,255").
      RUN pdf_set_tool_parameter(pdfStream,pdfToolName,"HeaderTextColor",0,"0,0,0").

      RUN pdf_set_tool_parameter(pdfStream,pdfToolName,"DetailFont",0,"Courier").
      RUN pdf_set_tool_parameter(pdfStream,pdfToolName,"DetailFontSize",0,"10").
      RUN pdf_set_tool_parameter(pdfStream,pdfToolName,"DetailBGColor",0,"255,255,255").
      RUN pdf_set_tool_parameter(pdfStream,pdfToolName,"DetailTextColor",0,"0,0,0").
      RUN pdf_set_tool_parameter(pdfStream,pdfToolName,"ColumnPadding",0,"5").

      /*
      RUN pdf_set_tool_parameter(pdfStream,pdfToolName,"StartY",0,"0").
      */
      RUN pdf_set_tool_parameter(pdfStream,pdfToolName,"CellUnderline",0,"0.5").

    END.
  END CASE.

  IF NOT VALID-HANDLE(h_PDF-Tool) THEN
    RUN {&PDFDIR}pdftool.p PERSISTENT SET h_PDF-Tool
        (INPUT THIS-PROCEDURE:HANDLE).

END. /* pdf_tool_add */

PROCEDURE pdf_tool_create : /* PRIVATE */
  DEFINE INPUT PARAMETER pdfStream      AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfToolName    AS CHARACTER NO-UNDO.

  FIND TT_pdf_tool
       WHERE TT_pdf_tool.obj_stream = pdfStream
       AND TT_pdf_tool.tool_name  = pdfToolName NO-LOCK NO-ERROR.
  IF NOT AVAIL TT_pdf_Tool THEN DO:
    RUN pdf_error(pdfStream,"pdf_tool_create","Cannot find Tool ("
                                              + pdfToolName + "for Stream ("
                                              + pdfStream + "!").
    RETURN .
  END.

  IF NOT VALID-HANDLE(h_PDF-Tool) THEN
    RUN {&PDFDIR}pdftool.p PERSISTENT SET h_PDF-Tool
        (INPUT THIS-PROCEDURE:HANDLE).

  /* If a call the pdf_new_page hasn't been done then ensure that we do it */
  IF pdf_Page(pdfStream) = 0 THEN
    RUN pdf_new_page2(pdfStream, pdf_Orientation(pdfStream)).

  CASE TT_pdf_tool.tool_type:
    WHEN "TABLE" THEN 
      RUN BuildTable IN h_PDF-Tool (INPUT pdfStream,
                                    INPUT pdfToolName,
                                    INPUT TT_pdf_tool.Tool_handle).
    WHEN "CALENDAR" THEN
      RUN BuildCalendar IN h_PDF-Tool (INPUT pdfStream,
                                       INPUT pdfToolName).
    WHEN "MATRIX" THEN
      RUN BuildMatrix IN h_PDF-Tool (INPUT pdfStream,
                                     INPUT pdfToolName).
  END CASE.

END. /* pdf_tool_create */

PROCEDURE pdf_set_tool_parameter :

  DEFINE INPUT PARAMETER pdfStream      AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfToolName    AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfToolParam   AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfToolCol     AS INTEGER   NO-UNDO.
  DEFINE INPUT PARAMETER pdfToolValue   AS CHARACTER NO-UNDO.

  DEFINE VARIABLE L_Integer      AS INTEGER NO-UNDO.
  DEFINE VARIABLE L_Decimal      AS DECIMAL NO-UNDO.

  DEFINE VARIABLE L_TableParams  AS CHARACTER NO-UNDO.
  L_TableParams = "Outline,CellUnderline,"
                + "ColumnHeader,ColumnWidth,ColumnX,ColumnPadding,MaxX,MaxY,"
                + "HeaderFont,HeaderFontSize,HeaderBGColor,HeaderTextColor,"
                + "DetailFont,DetailFontSize,DetailBGColor,DetailTextColor,"
                + "UseFields,StartY,Pages".

  FIND TT_pdf_tool
       WHERE TT_pdf_tool.obj_stream = pdfStream
         AND TT_pdf_tool.tool_name  = pdfToolName NO-LOCK NO-ERROR.
  IF NOT AVAIL TT_pdf_Tool THEN DO:
    RUN pdf_error(pdfStream,"pdf_tool_parameter","Cannot find Tool ("
                                              + pdfToolName + "for Stream ("
                                              + pdfStream + "!").
    RETURN .
  END.

  CASE TT_pdf_tool.tool_type:
    WHEN "TABLE" THEN DO:
      IF LOOKUP(pdfToolParam,L_TableParams) = 0 THEN DO:
        RUN pdf_error(pdfStream,"pdf_tool_parameter","Invalid Table Parameter entered(" + pdfToolName + ")!").
        RETURN .
      END.

      /* Verify for Integer Content */
      IF LOOKUP(pdfToolParam,"ColumnWidth,ColumnX,MaxX,MaxY,Outline,CellUnderline,Pages,StartY") > 0 
      THEN DO:
        L_Integer = INT(pdfToolValue) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN DO:
          RUN pdf_error(pdfStream,"pdf_tool_parameter",
                                  "Parameter (" + pdfToolParam + ") requires"
                                  + " an Integer value!").
          RETURN .
        END.

        IF LOOKUP(pdfToolParam,"Outline") = 0 AND L_Integer <= 0 THEN DO:
          RUN pdf_error(pdfStream,"pdf_tool_parameter",
                                  "Parameter (" + pdfToolParam + ") requires"
                                  + " a positive (non-zero) value!").
          RETURN .
        END.
      END. /* Integer Verification */

      ELSE IF LOOKUP(pdfToolParam,"HeaderFontSize,DetailFontSize") > 0 THEN DO:
        L_Decimal = DEC(pdfToolValue) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN DO:
          RUN pdf_error(pdfStream,"pdf_tool_parameter",
                                  "Parameter (" + pdfToolParam + ") requires"
                                  + " a Decimal value!").
          RETURN .
        END.
      END. /* Decimal Verification */

      ELSE IF LOOKUP(pdfToolParam,"HeaderBGColor,DetailBGColor,HeaderTextColor,DetailTextColor") > 0 THEN DO:
        IF NUM-ENTRIES(pdfToolValue) <> 3 THEN DO:
          RUN pdf_error(pdfStream,"pdf_tool_parameter",
                                  "Parameter (" + pdfToolParam + ") requires"
                                  + " 3 Entries in RGB sequence comma-delimited!").
          RETURN .
        END.
      END. /* Color Verification */

      ELSE IF LOOKUP(pdfToolParam,"HeaderFont,DetailFont") > 0 THEN DO:
        IF NOT CAN-FIND(FIRST TT_pdf_font
                        WHERE TT_pdf_font.obj_stream = pdfStream
                          AND TT_pdf_font.font_name  = pdfToolValue) THEN DO:
          RUN pdf_error(pdfStream,"pdf_tool_parameter",
                                  "Parameter (" + pdfToolParam + ") requires"
                                  + " an Valid Font Name!").
          RETURN .
        END.
      END. /* Font Name Verification */

    END. /* Table Parameter Check */
  END CASE.

  FIND TT_pdf_tool_param
       WHERE TT_pdf_tool_param.obj_stream = pdfStream
         AND TT_pdf_tool_param.tool_name  = pdfToolName
         AND TT_pdf_tool_param.tool_param = pdfToolParam
         AND TT_pdf_tool_param.tool_col   = pdfToolCol
         EXCLUSIVE NO-ERROR.
  IF NOT AVAIL TT_pdf_tool_param THEN DO:
    CREATE TT_pdf_tool_param.
    ASSIGN TT_pdf_tool_param.obj_stream   = pdfStream
           TT_pdf_tool_param.tool_name    = pdfToolName
           TT_pdf_tool_param.tool_param   = pdfToolParam
           TT_pdf_tool_param.tool_col     = pdfToolCol.
  END.

  TT_pdf_tool_param.tool_value   = pdfToolValue.

END. /* pdf_set_tool_parameter */

FUNCTION pdf_get_tool_parameter RETURNS CHARACTER
        (INPUT  pdfStream      AS CHARACTER,
         INPUT  pdfToolName    AS CHARACTER,
         INPUT  pdfToolParam   AS CHARACTER,
         INPUT  pdfToolCol     AS INTEGER):

  FIND TT_pdf_tool
       WHERE TT_pdf_tool.obj_stream = pdfStream
         AND TT_pdf_tool.tool_name  = pdfToolName NO-LOCK NO-ERROR.
  IF NOT AVAIL TT_pdf_Tool THEN DO:
    RUN pdf_error(pdfStream,"pdf_tool_parameter","Cannot find Tool ("
                                              + pdfToolName + "for Stream ("
                                              + pdfStream + "!").
    RETURN "".
  END.

  FIND TT_pdf_tool_param
       WHERE TT_pdf_tool_param.obj_stream = pdfStream
         AND TT_pdf_tool_param.tool_name  = pdfToolName
         AND TT_pdf_tool_param.tool_param = pdfToolParam
         AND TT_pdf_tool_param.tool_col   = pdfToolCol NO-LOCK NO-ERROR.

  RETURN IF AVAIL TT_pdf_tool_param THEN TT_pdf_tool_param.tool_value
         ELSE "".

END FUNCTION. /* pdf_get_tool_parameter */

PROCEDURE pdf_tool_destroy : /* PRIVATE */
  DEFINE INPUT PARAMETER pdfStream      AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfToolName    AS CHARACTER NO-UNDO.

  FIND TT_pdf_tool WHERE TT_pdf_tool.obj_stream = pdfStream
                     AND TT_pdf_tool.tool_name  = pdfToolName
                     EXCLUSIVE-LOCK NO-ERROR.

  IF NOT AVAIL TT_pdf_Tool THEN DO:
    RUN pdf_error(pdfStream,"pdf_tool_destroy","Cannot find Tool ("
                            + pdfToolName + "for Stream ("
                            + pdfStream + "!").
    RETURN.
  END.

  /* Firstly, remove all of the tool parameters */
  FOR EACH TT_pdf_tool_param WHERE TT_pdf_tool_param.obj_stream = pdfStream
                               AND TT_pdf_tool_param.tool_name  = pdfToolName
                               EXCLUSIVE-LOCK:
     DELETE TT_pdf_tool_param.
  END.

  /* The delete the tool */
  DELETE TT_pdf_tool.
END. /* pdf_tool_destroy */

PROCEDURE pdf_note :

  DEFINE INPUT PARAMETER pdfStream      AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfNoteText    AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfNoteTitle   AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfIcon        AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfLLX         AS DECIMAL DECIMALS 4 NO-UNDO.
  DEFINE INPUT PARAMETER pdfLLY         AS DECIMAL DECIMALS 4 NO-UNDO.
  DEFINE INPUT PARAMETER pdfURX         AS DECIMAL DECIMALS 4 NO-UNDO.
  DEFINE INPUT PARAMETER pdfURY         AS DECIMAL DECIMALS 4 NO-UNDO.
  DEFINE INPUT PARAMETER pdfRed         AS DECIMAL DECIMALS 4 NO-UNDO.
  DEFINE INPUT PARAMETER pdfGreen       AS DECIMAL DECIMALS 4 NO-UNDO.
  DEFINE INPUT PARAMETER pdfBlue        AS DECIMAL DECIMALS 4 NO-UNDO.

  IF LOOKUP(pdfIcon,"Note,Comment,Insert,Key,Help,NewParagraph,Paragraph") = 0 
  THEN pdfIcon = "Note".

  pdfRed   = IF pdfRed < 0 THEN 0
             ELSE IF pdfRed > 1 THEN 1
             ELSE pdfRed.
  pdfGreen = IF pdfGreen < 0 THEN 0
             ELSE IF pdfGreen > 1 THEN 1
             ELSE pdfGreen.
  pdfBlue  = IF pdfBlue < 0 THEN 0
             ELSE IF pdfBlue > 1 THEN 1
             ELSE pdfBlue.

  CREATE TT_pdf_annot.
  ASSIGN TT_pdf_annot.obj_stream   = pdfStream
         TT_pdf_annot.annot_type    = "Text"
         TT_pdf_annot.annot_content = pdfNoteText.
         TT_pdf_annot.annot_page    = pdf_Page(pdfStream).
  ASSIGN TT_pdf_annot.annot_icon    = pdfIcon
         TT_pdf_annot.annot_rect    = STRING(pdfLLX) + " "
                                    + STRING(pdfLLY) + " "
                                    + STRING(pdfURX) + " " 
                                    + STRING(pdfURY) 
         TT_pdf_annot.annot_color   = dec2string(pdfRed) + " "
                                    + dec2string(pdfGreen) + " "
                                    + dec2string(pdfBlue)
         TT_pdf_annot.annot_border  = 0
         TT_pdf_annot.annot_style   = pdfNoteTitle.

END. /* pdf_note */

PROCEDURE pdf_stamp :

  DEFINE INPUT PARAMETER pdfStream      AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfStampText   AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfTitle       AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfStamp       AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfLLX         AS DECIMAL DECIMALS 4 NO-UNDO.
  DEFINE INPUT PARAMETER pdfLLY         AS DECIMAL DECIMALS 4 NO-UNDO.
  DEFINE INPUT PARAMETER pdfURX         AS DECIMAL DECIMALS 4 NO-UNDO.
  DEFINE INPUT PARAMETER pdfURY         AS DECIMAL DECIMALS 4 NO-UNDO.
  DEFINE INPUT PARAMETER pdfRed         AS DECIMAL DECIMALS 4 NO-UNDO.
  DEFINE INPUT PARAMETER pdfGreen       AS DECIMAL DECIMALS 4 NO-UNDO.
  DEFINE INPUT PARAMETER pdfBlue        AS DECIMAL DECIMALS 4 NO-UNDO.

  DEFINE VARIABLE l_ValidStamps AS CHARACTER NO-UNDO.

  L_ValidStamps = "Approved,Experimental,NotApproved,"
                + "AsIs,Expired,NotForPublicRelease,"
                + "Confidential,Final,Sold,"
                + "Departmental,ForComment,TopSecret,"
                + "Draft,ForPublicRelease".

  IF LOOKUP(pdfStamp,L_ValidStamps) = 0 THEN
    pdfStamp = "Draft".

  pdfRed   = IF pdfRed < 0 THEN 0
             ELSE IF pdfRed > 1 THEN 1
             ELSE pdfRed.
  pdfGreen = IF pdfGreen < 0 THEN 0
             ELSE IF pdfGreen > 1 THEN 1
             ELSE pdfGreen.
  pdfBlue  = IF pdfBlue < 0 THEN 0
             ELSE IF pdfBlue > 1 THEN 1
             ELSE pdfBlue.

  CREATE TT_pdf_annot.
  ASSIGN TT_pdf_annot.obj_stream   = pdfStream
         TT_pdf_annot.annot_type    = "Stamp"
         TT_pdf_annot.annot_content = pdfStampText.
         TT_pdf_annot.annot_page    = pdf_Page(pdfStream).
  ASSIGN TT_pdf_annot.annot_icon    = pdfStamp
         TT_pdf_annot.annot_rect    = STRING(pdfLLX) + " "
                                    + STRING(pdfLLY) + " "
                                    + STRING(pdfURX) + " "
                                    + STRING(pdfURY)
         TT_pdf_annot.annot_color   = dec2string(pdfRed) + " "
                                    + dec2string(pdfGreen) + " "
                                    + dec2string(pdfBlue)
         TT_pdf_annot.annot_border  = 0
         TT_pdf_annot.annot_style   = pdfTitle.

END. /* pdf_stamp */

PROCEDURE pdf_Markup:

  DEFINE INPUT PARAMETER pdfStream      AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfContent     AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfTitle       AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfStyle       AS CHARACTER NO-UNDO CASE-SENSITIVE.

  DEFINE INPUT PARAMETER pdfX1          AS DECIMAL DECIMALS 4 NO-UNDO.
  DEFINE INPUT PARAMETER pdfY1          AS DECIMAL DECIMALS 4 NO-UNDO.
  DEFINE INPUT PARAMETER pdfX2          AS DECIMAL DECIMALS 4 NO-UNDO.
  DEFINE INPUT PARAMETER pdfY2          AS DECIMAL DECIMALS 4 NO-UNDO.
  DEFINE INPUT PARAMETER pdfX3          AS DECIMAL DECIMALS 4 NO-UNDO.
  DEFINE INPUT PARAMETER pdfY3          AS DECIMAL DECIMALS 4 NO-UNDO.
  DEFINE INPUT PARAMETER pdfX4          AS DECIMAL DECIMALS 4 NO-UNDO.
  DEFINE INPUT PARAMETER pdfY4          AS DECIMAL DECIMALS 4 NO-UNDO.

  DEFINE INPUT PARAMETER pdfRed         AS DECIMAL DECIMALS 4 NO-UNDO.
  DEFINE INPUT PARAMETER pdfGreen       AS DECIMAL DECIMALS 4 NO-UNDO.
  DEFINE INPUT PARAMETER pdfBlue        AS DECIMAL DECIMALS 4 NO-UNDO.
  
  IF LOOKUP(pdfStyle,"Highlight,Underline,Squiggly,StrikeOut") = 0 THEN 
    pdfStyle = "Highlight".

  pdfRed   = IF pdfRed < 0 THEN 0
             ELSE IF pdfRed > 1 THEN 1
             ELSE pdfRed.
  pdfGreen = IF pdfGreen < 0 THEN 0
             ELSE IF pdfGreen > 1 THEN 1
             ELSE pdfGreen.
  pdfBlue  = IF pdfBlue < 0 THEN 0
             ELSE IF pdfBlue > 1 THEN 1
             ELSE pdfBlue.

  CREATE TT_pdf_annot.
  ASSIGN TT_pdf_annot.obj_stream   = pdfStream
         TT_pdf_annot.annot_type    = pdfStyle
         TT_pdf_annot.annot_content = pdfContent.
         TT_pdf_annot.annot_page    = pdf_Page(pdfStream).
  ASSIGN TT_pdf_annot.annot_icon    = ""
         TT_pdf_annot.annot_rect    = STRING(pdfX1) + " "
                                    + STRING(pdfY4) + " "
                                    + STRING(pdfX2) + " "
                                    + STRING(pdfY1)
         TT_pdf_annot.annot_add     = STRING(pdfX1) + " "
                                    + STRING(pdfY1) + " "
                                    + STRING(pdfX2) + " "
                                    + STRING(pdfY2) + " " 
                                    + STRING(pdfX3) + " "
                                    + STRING(pdfY3) + " " 
                                    + STRING(pdfX4) + " "
                                    + STRING(pdfY4)
         TT_pdf_annot.annot_color   = dec2string(pdfRed) + " "
                                    + dec2string(pdfGreen) + " "
                                    + dec2string(pdfBlue)
         TT_pdf_annot.annot_border  = 0
         TT_pdf_annot.annot_style   = pdfTitle.

END. /* pdf_Markup */

PROCEDURE pdf_load_template:
  DEFINE INPUT PARAMETER pdfStream        AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfTemplateID    AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfTemplateFile  AS CHARACTER NO-UNDO.

  IF NOT VALID-HANDLE(h_PDF-template) THEN
    RUN {&PDFDIR}pdfTemplate.p PERSISTENT SET h_PDF-Template
        (INPUT THIS-PROCEDURE:HANDLE).

  RUN LoadTemplate IN h_PDF-template(pdfStream,
                                     pdfTemplateID,
                                     pdfTemplateFile).

  IF RETURN-VALUE <> "" THEN
    RUN pdf_error(pdfStream,"pdf_load_template",RETURN-VALUE).

END. /* pdf_load_template */

PROCEDURE pdf_use_template:
  DEFINE INPUT PARAMETER pdfStream        AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfTemplateID    AS CHARACTER NO-UNDO.

  DEFINE VARIABLE c_Content AS CHARACTER NO-UNDO.

  RUN GetContent IN h_PDF-template(pdfStream,
                                   pdfTemplateID,
                                   OUTPUT c_Content).
  IF RETURN-VALUE <> "" THEN
    RUN pdf_error(pdfStream,"pdf_use_template",RETURN-VALUE).

  RUN OutputTextContent(pdfStream, 
                          "GRAPHIC",
                          c_Content,
                          "",
                          "").

END. /* pdf_use_template */

PROCEDURE GetFileContent:  /* PRIVATE */

  DEFINE INPUT-OUTPUT PARAMETER pMemPtr AS MEMPTR NO-UNDO.

  DEFINE VARIABLE i_cnt   AS INTEGER NO-UNDO.
  DEFINE VARIABLE i_size  AS INTEGER NO-UNDO.
  
  INPUT FROM VALUE(FILE-INFO:FILE-NAME) NO-ECHO BINARY NO-CONVERT NO-MAP.
  
  i_size = GetFileSize().
  
  SET-SIZE(pMemPtr) = i_size.
  IMPORT pMemPtr.

  INPUT CLOSE.
END. /* GetFileContent */

PROCEDURE PutFileContent: /* PRIVATE */
  DEFINE INPUT PARAMETER pMemPtr AS MEMPTR NO-UNDO.
  
  DEFINE VARIABLE l_Loop  AS INTEGER   NO-UNDO.
  DEFINE VARIABLE i_size  AS INTEGER   NO-UNDO.
  DEFINE VARIABLE r_Byte  AS RAW NO-UNDO.
    
  EXPORT STREAM S_PDF_inc pMemPtr.
  
END. /* PutFileContent */

PROCEDURE pdf_GetBestFont:
/* Calculate the best font size to use to insert text into a given range along 
  the X axis - tests in 0.5 point size increments */

  DEFINE INPUT        PARAMETER pdfStream     AS CHARACTER  NO-UNDO. /* Stream name */
  DEFINE INPUT        PARAMETER pdfFont       AS CHARACTER  NO-UNDO. /* Font to use */
  DEFINE INPUT-OUTPUT PARAMETER pdfText       AS CHARACTER  NO-UNDO. /* Text to measure */
  DEFINE INPUT-OUTPUT PARAMETER pdfFontSize   AS DECIMAL    NO-UNDO. /* Start font size */
  DEFINE INPUT        PARAMETER pdfSmallest   AS DECIMAL    NO-UNDO. /* Smallest font size to use */
  DEFINE INPUT        PARAMETER pdfChopText   AS LOGICAL    NO-UNDO. /* If the smallest font is too */
                                                                       /* big then cut text to fit? */
  DEFINE INPUT        PARAMETER pdfFromX      AS INTEGER    NO-UNDO. /* Start X co-ord */
  DEFINE INPUT        PARAMETER pdfToX        AS INTEGER    NO-UNDO. /* End X co-ord   */

  DEFINE VARIABLE w-loop      AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE w-chars2fit AS INTEGER    NO-UNDO.

  BESTLOOP:
  DO w-loop = pdfFontSize TO pdfSmallest BY -0.5:
    RUN pdf_set_font (pdfStream, pdfFont, w-loop).
        
    ASSIGN w-chars2fit = pdf_GetNumFittingChars(pdfStream, pdfText, pdfFromX, pdfToX)
           pdfFontSize = w-loop.
    
    IF w-chars2fit >= LENGTH(pdfText) THEN
      LEAVE BESTLOOP.
    
    IF (w-loop = pdfSmallest) AND (w-chars2fit < length(pdfText)) AND (pdfChopText = TRUE) THEN
        ASSIGN pdfText = SUBSTR(pdfText, 1, w-chars2fit).
  END.
END PROCEDURE. /* pdf_GetBestFont */

PROCEDURE pdf_open_PDF:
  DEFINE INPUT PARAMETER pdfStream  AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfName    AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfID      AS CHARACTER NO-UNDO.

  FIND FIRST TT_pdf_ext WHERE TT_pdf_ext.obj_stream = pdfStream
                          AND TT_pdf_ext.pdf_id     = pdfID NO-ERROR.
  IF AVAIL TT_pdf_ext THEN DO:
    RUN pdf_error(pdfStream, "pdf_open_PDF","PDF ID (" + pdfID + ") has been used already").
    RETURN.
  END.

  FIND FIRST TT_pdf_ext WHERE TT_pdf_ext.obj_Stream = pdfStream
                          AND TT_pdf_ext.pdf_name   = pdfName NO-ERROR.
  IF AVAIL TT_pdf_ext THEN DO:
    RUN pdf_error(pdfStream, "pdf_open_PDF","PDF File Name (" + pdfName + ") has already been" 
                                + " opened with PDF ID = " + TT_pdf_ext.pdf_ID).
    RETURN.
  END.

  RUN {&PDFDIR}pdfextract.p (INPUT pdfStream,
                             INPUT pdfName,
                             INPUT pdfID).


  /* Create an entry for each External font so that they can be used by 
     PDFinclude */
  FOR EACH TT_Font WHERE TT_Font.obj_stream = pdfStream.
    CREATE TT_pdf_font.
    ASSIGN TT_pdf_font.font_file  = "EXTERNAL"
           TT_pdf_font.font_tag   = tt_font.font_tag
           TT_pdf_font.font_name  = TT_Font.font_name
           TT_pdf_font.font_width = TT_Font.font_width
           TT_pdf_font.ext_page   = TT_Font.page_id
           TT_pdf_font.obj_stream = pdfStream.
  END.
  
  CREATE TT_pdf_ext.
  ASSIGN TT_pdf_ext.obj_Stream = pdfStream
         TT_Pdf_ext.pdf_ID     = pdfID
         TT_pdf_ext.pdf_name   = pdfName.
END. /* pdf_open_PDF */


PROCEDURE pdf_use_PDF_page:
  DEFINE INPUT PARAMETER pdfStream  AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfID      AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfPage    AS INTEGER   NO-UNDO.

  DEFINE VARIABLE L_Page AS INTEGER NO-UNDO.
  
  DEFINE BUFFER B_TT_pdf_page FOR TT_pdf_page.

  IF NOT CAN-FIND(FIRST TT_pdf_stream
                  WHERE TT_pdf_stream.obj_stream = pdfStream NO-LOCK)
  THEN DO:
    RUN pdf_error(pdfStream,"pdf_use_PDF_Page","Cannot find Stream!").
    RETURN.
  END.

  IF NOT CAN-FIND(FIRST TT_pdf_ext
                  WHERE TT_pdf_ext.obj_stream = pdfStream
                    AND TT_pdf_ext.pdf_id = pdfID NO-LOCK)
  THEN DO:
    RUN pdf_error(pdfStream,"pdf_use_PDF_Page","Cannot find PDF ID (" + pdfID + ") !").
    RETURN.
  END.

  IF NOT CAN-FIND(FIRST TT_info
                  WHERE TT_info.pdf_id = pdfID
                    AND TT_info.info_name = "Page"
                    AND TT_info.info_value = STRING(pdfPage) NO-LOCK)
  THEN DO:
    RUN pdf_error(pdfStream,"pdf_use_PDF_Page","Invalid Page # for PDF ID = " + pdfID).
    RETURN.
  END.

  RUN OutputTextContent(pdfStream, 
                        "IMAGE-EXT",
                        "~/" + pdfID + STRING(pdfPage) + " Do",
                        "",
                        "").

  IF NOT CAN-FIND(FIRST TT_pdf_external
                  WHERE TT_pdf_external.obj_stream = pdfStream
                    AND TT_pdf_external.ext_tag    = "~/" + pdfID + STRING(pdfPage))
  THEN DO:
    FIND FIRST TT_Object 
         WHERE TT_Object.obj_stream = pdfStream
           AND TT_Object.pdf_id     = pdfID
           AND TT_Object.obj_type   = "Page"
           AND TT_Object.page_id    = pdfPage NO-ERROR.

    CREATE TT_pdf_external.
    ASSIGN TT_pdf_external.obj_stream  = pdfStream
           TT_pdf_external.ext_page    = pdfPage.
           TT_pdf_external.page_id     = pdf_Page(pdfStream).
    ASSIGN TT_pdf_external.ext_tag     = "~/" + pdfID + STRING(pdfPage)
           TT_pdf_external.ext_file    = SESSION:TEMP-DIR + pdfID + STRING(pdfPage) + ".txt".
           TT_pdf_external.ext_media1  = 0.
           TT_pdf_external.ext_media2  = 0.
           TT_pdf_external.ext_media3  = DEC(pdf_PageWidth(pdfstream)).
           TT_pdf_external.ext_media4  = DEC(pdf_PageHeight(pdfstream)).
           TT_pdf_external.ext_rotate  = IF AVAIL TT_Object THEN
                                           TT_Object.rotate
                                         ELSE 0.
    
   IF pdf_get_parameter(pdfStream,"UseExternalPageSize") = "TRUE" THEN DO:
      ASSIGN TT_pdf_external.ext_media1 = TT_Object.obj_Media1
             TT_pdf_external.ext_media2 = TT_Object.obj_Media2
             TT_pdf_external.ext_media3 = TT_Object.obj_Media3
             TT_pdf_external.ext_media4 = TT_Object.obj_Media4.

      ASSIGN TT_pdf_external.ext_Crop1 = TT_Object.obj_Crop1
             TT_pdf_external.ext_Crop2 = TT_Object.obj_Crop2
             TT_pdf_external.ext_Crop3 = TT_Object.obj_Crop3
             TT_pdf_external.ext_Crop4 = TT_Object.obj_Crop4.
   END.

  END.

  /* Find the current page record and tell it what external page we are using */
  L_Page = pdf_page(pdfStream).
  FIND FIRST B_TT_pdf_page 
       WHERE B_TT_pdf_page.obj_Stream = pdfStream
         AND B_TT_pdf_page.page_nbr   = L_Page NO-ERROR.
  IF AVAIL B_TT_pdf_page THEN DO:
    ASSIGN B_TT_pdf_page.page_use    = pdfPage
           B_TT_pdF_page.page_width  = TT_pdf_external.ext_media3
           B_TT_pdf_page.page_height = TT_pdf_external.ext_media4
           B_TT_pdf_page.page_rotate = TT_pdf_external.ext_rotate.

    IF NOT (    TT_pdf_external.ext_Crop1 = 0 AND TT_pdf_external.ext_Crop2 = 0
            AND TT_pdf_external.ext_Crop3 = 0 AND TT_pdf_external.ext_Crop3 = 0) THEN
      B_TT_pdf_page.page_crop = STRING(ext_Crop1) + " "
                              + STRING(ext_Crop2) + " " 
                              + STRING(ext_Crop3) + " " 
                              + STRING(ext_Crop4).

    IF tt_pdf_external.ext_rotate = 90 THEN DO:
     
      IF NOT (    TT_pdf_external.ext_Crop1 = 0 AND TT_pdf_external.ext_Crop2 = 0
              AND TT_pdf_external.ext_Crop3 = 0 AND TT_pdf_external.ext_Crop3 = 0) THEN
        B_TT_pdf_page.page_crop = STRING(ext_Crop1) + " "
                                + STRING(ext_Crop2) + " " 
                                + STRING(ext_Crop4) + " " 
                                + STRING(ext_Crop3).
    
    END. /* Rotate 90 */

  END. /* avail b_tt_pdf_page */

END. /* pdf_use_PDF_page */

PROCEDURE LoadExternalFonts: /* PRIVATE */
  DEFINE INPUT PARAMETER pdfStream  AS CHARACTER NO-UNDO.

  DEFINE VARIABLE l_text  AS CHARACTER NO-UNDO.

  DEFINE VARIABLE l_Obj         AS INTEGER NO-UNDO.
  DEFINE VARIABLE l_Seq         AS INTEGER NO-UNDO.
  DEFINE VARIABLE l_Descendant  AS INTEGER NO-UNDO.
  DEFINE VARIABLE l_Descriptor  AS INTEGER NO-UNDO.
  DEFINE VARIABLE l_Enc         AS INTEGER NO-UNDO.

  DEFINE VARIABLE L_EncryptMem  AS MEMPTR NO-UNDO.
  DEFINE VARIABLE L_EncryptKey  AS MEMPTR NO-UNDO.

  DEFINE VARIABLE l_FoundDescendant   AS LOGICAL NO-UNDO.
  DEFINE VARIABLE l_FoundDescriptor  AS LOGICAL NO-UNDO.
  DEFINE VARIABLE L_FoundEnc         AS LOGICAL NO-UNDO.

  DEFINE VARIABLE file-content  AS MEMPTR NO-UNDO.

  DEFINE BUFFER B_TT_Font FOR TT_Font.

        FOR EACH TT_Resource
           WHERE TT_Resource.obj_Stream = pdfStream
             AND TT_Resource.res_type   = "Font"
          BREAK BY TT_Resource.res_text
                BY TT_Resource.res_obj:

          /* Only Perform once action per Font */
          /* IF FIRST-OF(TT_Resource.res_text) THEN DO: */
            IF CAN-FIND(FIRST TT_pdf_font
                        WHERE TT_pdf_font.obj_Stream = pdfStream
                          AND TT_pdf_font.font_tag   = TT_resource.res_text)
            THEN DO:

              FIND FIRST TT_font 
                   WHERE TT_Font.obj_stream = pdfStream
                     AND TT_Font.obj_id = TT_Resource.res_obj
                     AND TT_Font.gen_id = TT_Resource.res_gen NO-LOCK NO-ERROR.

              /* Put Out Font Dictionary */
              ObjectSequence(pdfStream, pdf_inc_ObjectSequence + 1, "Font", 0).
              l_Obj = pdf_inc_ObjectSequence.
              PUT STREAM S_pdf_inc UNFORMATTED
                  pdf_inc_ObjectSequence " 0 obj" {&pdfSKIP}.

              l_Seq = pdf_inc_ObjectSequence.
              INPUT FROM VALUE(SESSION:TEMP-DIR + TT_Resource.pdf_id + "-" 
                               + STRING(TT_resource.res_obj) + "-" 
                               + STRING(TT_Resource.res_gen) + ".txt")
                    BINARY NO-MAP NO-CONVERT NO-ECHO.
                REPEAT:
                  IMPORT UNFORMATTED l_Text.
                
                  IF INDEX(l_Text,"~/FontDescriptor " + STRING(TT_Font.desc_obj) + " " + STRING(TT_Font.desc_gen)) > 0 THEN DO:
                    L_Seq = l_Seq + 1.
                    l_Text = REPLACE(l_Text,
                                     "~/FontDescriptor " 
                                     + STRING(TT_Font.desc_obj) + " "
                                     + STRING(TT_Font.desc_gen),
                                     "~/FontDescriptor " 
                                     + STRING(L_Seq ) + " 0").
                    L_FoundDescriptor = TRUE.
                    L_Descriptor = L_Seq.
                  END.

                  IF INDEX(l_Text,"~/Encoding " + STRING(TT_Font.enc_obj) + " " + STRING(TT_Font.enc_gen)) > 0 THEN DO:
                    L_Seq = l_Seq + 1.
                    l_Text = REPLACE(l_Text,
                                     "~/Encoding " 
                                     + STRING(TT_Font.enc_obj) + " "
                                     + STRING(TT_Font.enc_gen),
                                     "~/Encoding " 
                                     + STRING(L_Seq) + " 0").
                    L_FoundEnc = TRUE.
                    L_Enc = L_Seq.
                  END.

                  /* Begin - Handle Descendant Fonts */
                  IF INDEX(l_Text,"~/DescendantFonts[" + STRING(TT_Font.descend_obj) + " " + STRING(TT_Font.descend_gen)) > 0 
                  THEN DO:
                    L_Seq = l_Seq + 1.
                    l_Text = REPLACE(l_Text,
                                     "~/DescendantFonts[" 
                                     + STRING(TT_Font.descend_obj) + " "
                                     + STRING(TT_Font.descend_gen),
                                     "~/DescendantFonts[" 
                                     + STRING(L_Seq) + " 0").
                    L_FoundDescendant = TRUE.
                    L_Descendant = L_Seq.
                  END.

                  IF INDEX(l_Text,"~/DescendantFonts [ " + STRING(TT_Font.descend_obj) + " " + STRING(TT_Font.descend_gen)) > 0 
                  THEN DO:
                    L_Seq = l_Seq + 1.
                    l_Text = REPLACE(l_Text,
                                     "~/DescendantFonts [ " 
                                     + STRING(TT_Font.descend_obj) + " "
                                     + STRING(TT_Font.descend_gen),
                                     "~/DescendantFonts [ " 
                                     + STRING(L_Seq) + " 0").
                    L_FoundDescendant = TRUE.
                    L_Descendant = L_Seq.
                  END.

                  IF INDEX(l_Text,"~/ToUnicode " + STRING(TT_Font.uni_obj) + " " + STRING(TT_Font.uni_gen)) > 0 THEN DO:
                    L_Seq = l_Seq + 1.
                    l_Text = REPLACE(l_Text,
                                     "~/ToUnicode " 
                                     + STRING(TT_Font.uni_obj) + " "
                                     + STRING(TT_Font.uni_gen),
                                     "~/ToUnicode  " 
                                     + STRING(L_Seq) + " 0").
                    L_FoundEnc = TRUE.
                    L_Enc = L_Seq.
                  END.

                  /* End - Handle Descendant Fonts */

                  PUT STREAM s_pdf_inc UNFORMATTED l_Text {&pdfSKIP}.

                END.
              INPUT CLOSE.                                          

              PUT STREAM S_pdf_inc UNFORMATTED
                  "endobj" {&pdfSKIP}.

              IF (L_FoundDescriptor AND L_FoundEnc) 
              OR L_FoundDescendant THEN 
                L_Seq = 2.
              ELSE
                L_Seq = 1.

              /* Put Out Encoding Dictionary */
              IF TT_Font.enc_obj <> 0 THEN DO:
                ObjectSequence(pdfStream, L_Enc, "Encoding", 0).
                PUT STREAM S_pdf_inc UNFORMATTED
                    pdf_inc_ObjectSequence " 0 obj" {&pdfSKIP}.

                INPUT FROM VALUE(SESSION:TEMP-DIR + TT_Resource.pdf_id + "-" 
                                 + STRING(TT_Font.enc_obj) + "-" 
                                 + STRING(TT_Font.enc_gen) + ".txt")
                      BINARY NO-MAP NO-CONVERT NO-ECHO.
                  REPEAT:
                    IMPORT UNFORMATTED l_Text.

                    IF INDEX(l_Text,"~/Encoding " + STRING(TT_Font.enc_obj) + " " + STRING(TT_Font.enc_gen)) > 0 THEN DO:
                      l_Text = REPLACE(l_Text,
                                       "~/Encoding " 
                                       + STRING(TT_Font.enc_obj) + " "
                                       + STRING(TT_Font.enc_gen),
                                       "~/Encoding " 
                                       + STRING(pdf_inc_ObjectSequence) + " 0").
                    END.
                    PUT STREAM s_pdf_inc UNFORMATTED l_Text {&pdfSKIP}.

                  END.
                INPUT CLOSE.

                PUT STREAM S_pdf_inc UNFORMATTED
                  "endobj" {&pdfSKIP}.
              END. /* Encoding */

              /* Put Out FontDescriptor Dictionary */
              IF TT_Font.desc_obj <> 0 THEN DO:
                ObjectSequence(pdfStream, L_Descriptor, "FontDescriptor", 0).
                PUT STREAM S_pdf_inc UNFORMATTED
                    pdf_inc_ObjectSequence " 0 obj" {&pdfSKIP}.

                INPUT FROM VALUE(SESSION:TEMP-DIR + TT_Resource.pdf_id + "-" 
                                 + STRING(TT_Font.desc_obj) + "-" 
                                 + STRING(TT_Font.desc_gen) + ".txt")
                      BINARY NO-MAP NO-CONVERT NO-ECHO.
                  REPEAT:
                    IMPORT UNFORMATTED l_Text.

                    IF INDEX(l_Text,"~/FontFile2 " + STRING(TT_Font.file2_obj) + " " + STRING(TT_Font.file2_gen)) > 0 THEN DO:
                      l_Text = REPLACE(l_Text,
                                       "~/FontFile2 " 
                                       + STRING(TT_Font.file2_obj) + " "
                                       + STRING(TT_Font.file2_gen),
                                       "~/FontFile2 " 
                                       + STRING(pdf_inc_ObjectSequence + 1) + " 0").
                    END.

                    IF INDEX(l_Text,"~/FontFile3 " + STRING(TT_Font.file3_obj) + " " + STRING(TT_Font.file3_gen)) > 0 THEN DO:
                      l_Text = REPLACE(l_Text,
                                       "~/FontFile3 " 
                                       + STRING(TT_Font.file3_obj) + " "
                                       + STRING(TT_Font.file3_gen),
                                       "~/FontFile3 " 
                                       + STRING(pdf_inc_ObjectSequence + 1) + " 0").
                    END.

                    PUT STREAM s_pdf_inc UNFORMATTED l_Text {&pdfSKIP}.

                  END.
                INPUT CLOSE.

                PUT STREAM S_pdf_inc UNFORMATTED
                  "endobj" {&pdfSKIP}.
              END. /* FontDescriptor */

              /* Put Out DescendantFonts Dictionary */
              IF TT_Font.descend_obj <> 0 THEN DO:
                FIND FIRST B_TT_Font
                     WHERE B_TT_Font.obj_stream = pdfStream
                       AND B_TT_Font.obj_id     = TT_Font.descend_obj NO-ERROR.

                ObjectSequence(pdfStream, pdf_inc_ObjectSequence + 1, "DescendantFonts", 0).
                PUT STREAM S_pdf_inc UNFORMATTED
                    pdf_inc_ObjectSequence " 0 obj" {&pdfSKIP}.
 
                INPUT FROM VALUE(SESSION:TEMP-DIR + TT_Resource.pdf_id + "-" 
                                    + STRING(TT_Font.descend_obj) + "-" 
                                    + STRING(TT_Font.descend_gen) + ".txt")
                      BINARY NO-MAP NO-CONVERT NO-ECHO.

                  REPEAT:
                    IMPORT UNFORMATTED L_Text .

                    IF INDEX(l_Text,"~/FontDescriptor " + STRING(B_TT_Font.desc_obj) + " " + STRING(B_TT_Font.desc_gen)) > 0 THEN DO:
                      l_Text = REPLACE(l_Text,
                                       "~/FontDescriptor " 
                                       + STRING(B_TT_Font.desc_obj) + " "
                                       + STRING(B_TT_Font.desc_gen),
                                       "~/FontDescriptor " 
                                       + STRING(pdf_inc_ObjectSequence + 1) + " 0").
                    END.

                    PUT STREAM s_pdf_inc UNFORMATTED l_Text {&pdfSKIP}.

                  END.
                INPUT CLOSE.

                PUT STREAM S_pdf_inc UNFORMATTED
                    {&pdfSKIP} {&pdfSKIP} "endstream" {&pdfSKIP}
                    "endobj" {&pdfSKIP}.

                /* Now Put out Descendant Fonts FontDecriptor Dictionary */
                FOR EACH B_TT_Font
                   WHERE B_TT_Font.obj_stream = pdfStream
                     AND B_TT_Font.obj_id     = TT_Font.descend_obj:
                  
                  ObjectSequence(pdfStream, pdf_inc_ObjectSequence + 1, "FontDescriptor", 0).
                  PUT STREAM S_pdf_inc UNFORMATTED
                      pdf_inc_ObjectSequence " 0 obj" {&pdfSKIP}.

                  INPUT FROM VALUE(SESSION:TEMP-DIR + TT_Resource.pdf_id + "-" 
                                   + STRING(B_TT_Font.desc_obj) + "-" 
                                   + STRING(B_TT_Font.desc_gen) + ".txt")
                        BINARY NO-MAP NO-CONVERT NO-ECHO.
                    REPEAT:
                      IMPORT UNFORMATTED l_Text.

                      IF INDEX(l_Text,"~/FontFile2 " + STRING(B_TT_Font.file2_obj) + " " + STRING(TT_Font.file2_gen)) > 0 THEN DO:
                        l_Text = REPLACE(l_Text,
                                         "~/FontFile2 " 
                                         + STRING(B_TT_Font.file2_obj) + " "
                                         + STRING(B_TT_Font.file2_gen),
                                         "~/FontFile2 " 
                                         + STRING(pdf_inc_ObjectSequence + 1) + " 0").
                      END.

                      IF INDEX(l_Text,"~/FontFile3 " + STRING(B_TT_Font.file3_obj) + " " + STRING(TT_Font.file3_gen)) > 0 THEN DO:
                        l_Text = REPLACE(l_Text,
                                         "~/FontFile3 " 
                                         + STRING(B_TT_Font.file3_obj) + " "
                                         + STRING(B_TT_Font.file3_gen),
                                         "~/FontFile3 " 
                                         + STRING(pdf_inc_ObjectSequence + 1) + " 0").
                      END.

                      PUT STREAM s_pdf_inc UNFORMATTED l_Text {&pdfSKIP}.

                    END.
                  INPUT CLOSE.

                  PUT STREAM S_pdf_inc UNFORMATTED
                    "endobj" {&pdfSKIP}.

                  /* Put Out FontFile2 Dictionary */
                  IF B_TT_Font.file2_obj <> 0 THEN DO:
                    FILE-INFO:FILE-NAME = SESSION:TEMP-DIR + TT_Resource.pdf_id + "-" 
                                        + STRING(B_TT_Font.file2_obj) + "-" 
                                        + STRING(B_TT_Font.file2_gen) + ".txt".
                    SET-SIZE(file-content) = FILE-INFO:FILE-SIZE.
                    ObjectSequence(pdfStream, pdf_inc_ObjectSequence + 1, "FontFile2", 0).
                    PUT STREAM S_pdf_inc UNFORMATTED
                        pdf_inc_ObjectSequence " 0 obj" {&pdfSKIP}.

                    INPUT FROM VALUE(SESSION:TEMP-DIR + TT_Resource.pdf_id + "-" 
                                     + STRING(B_TT_Font.file2_obj) + "-" 
                                     + STRING(B_TT_Font.file2_gen) + ".txt")
                          BINARY NO-MAP NO-CONVERT NO-ECHO.
                      REPEAT:
                        IMPORT file-content.
                        /*
                        RUN OutputMemPtr(pdfStream, 
                                         FALSE,
                                         TT_pdf_stream.obj_UniqueID, 
                                         pdf_inc_ObjectSequence, 
                                         file-content).      */
                        EXPORT STREAM s_pdf_inc file-content.
                      END.
                    INPUT CLOSE.

                    PUT STREAM S_pdf_inc UNFORMATTED
                        {&pdfSKIP} {&pdfSKIP} "endstream" {&pdfSKIP}
                        "endobj" {&pdfSKIP}.
                  END. /* FontFile2 */

                  /* Put Out FontFile3 Dictionary */
                  IF B_TT_Font.file3_obj <> 0 THEN DO:
                    FILE-INFO:FILE-NAME = SESSION:TEMP-DIR + TT_Resource.pdf_id + "-" 
                                        + STRING(B_TT_Font.file3_obj) + "-" 
                                        + STRING(B_TT_Font.file3_gen) + ".txt".
                    SET-SIZE(file-content) = FILE-INFO:FILE-SIZE.
                    ObjectSequence(pdfStream, pdf_inc_ObjectSequence + 1, "FontFile3", 0).
                    PUT STREAM S_pdf_inc UNFORMATTED
                        pdf_inc_ObjectSequence " 0 obj" {&pdfSKIP}.

                    INPUT FROM VALUE(SESSION:TEMP-DIR + TT_Resource.pdf_id + "-" 
                                     + STRING(B_TT_Font.file3_obj) + "-" 
                                     + STRING(B_TT_Font.file3_gen) + ".txt")
                          BINARY NO-MAP NO-CONVERT NO-ECHO.
                      REPEAT:
                        IMPORT file-content.
                        RUN OutputMemPtr(pdfStream, 
                                         TRUE,
                                         TT_pdf_stream.obj_UniqueID, 
                                         pdf_inc_ObjectSequence, 
                                         file-content).
                        /** igc99 EXPORT STREAM s_pdf_inc file-content. **/
                      END.
                    INPUT CLOSE.

                    PUT STREAM S_pdf_inc UNFORMATTED
                        {&pdfSKIP} {&pdfSKIP} "endstream" {&pdfSKIP}
                        "endobj" {&pdfSKIP}.
                  END. /* FontFile3 */

                  SET-SIZE(file-content) = 0.

                END. /* each B_TT_Font */

              END. /* DescendantFonts */

              /* Put Out ToUnicode Dictionary */
              IF TT_Font.uni_obj <> 0 THEN DO:
                FILE-INFO:FILE-NAME = SESSION:TEMP-DIR + TT_Resource.pdf_id + "-" 
                                    + STRING(TT_Font.uni_obj) + "-" 
                                    + STRING(TT_Font.uni_gen) + ".txt".
                SET-SIZE(file-content) = FILE-INFO:FILE-SIZE.
                ObjectSequence(pdfStream, pdf_inc_ObjectSequence + 1, "ToUnicode", 0).
                PUT STREAM S_pdf_inc UNFORMATTED
                    pdf_inc_ObjectSequence " 0 obj" {&pdfSKIP}.
 
                INPUT FROM VALUE(SESSION:TEMP-DIR + TT_Resource.pdf_id + "-" 
                                 + STRING(TT_Font.uni_obj) + "-" 
                                 + STRING(TT_Font.uni_gen) + ".txt")
                      BINARY NO-MAP NO-CONVERT NO-ECHO.
                  REPEAT:
                    IMPORT file-content.
                    RUN OutputMemPtr(pdfStream, 
                                     TRUE,
                                     TT_pdf_stream.obj_UniqueID, 
                                     pdf_inc_ObjectSequence, 
                                     file-content).
                    /** igc99 EXPORT STREAM s_pdf_inc file-content.  **/
                  END.
                INPUT CLOSE.

                PUT STREAM S_pdf_inc UNFORMATTED
                    {&pdfSKIP} {&pdfSKIP} "endstream" {&pdfSKIP}
                    "endobj" {&pdfSKIP}.
              END. /* ToUnicode */

              /* Put Out FontFile2 Dictionary */
              IF TT_Font.file2_obj <> 0 THEN DO:
                FILE-INFO:FILE-NAME = SESSION:TEMP-DIR + TT_Resource.pdf_id + "-" 
                                    + STRING(TT_Font.file2_obj) + "-" 
                                    + STRING(TT_Font.file2_gen) + ".txt".
                SET-SIZE(file-content) = FILE-INFO:FILE-SIZE.
                ObjectSequence(pdfStream, pdf_inc_ObjectSequence + 1, "FontFile2", 0).
                PUT STREAM S_pdf_inc UNFORMATTED
                    pdf_inc_ObjectSequence " 0 obj" {&pdfSKIP}.
 
                INPUT FROM VALUE(SESSION:TEMP-DIR + TT_Resource.pdf_id + "-" 
                                 + STRING(TT_Font.file2_obj) + "-" 
                                 + STRING(TT_Font.file2_gen) + ".txt")
                      BINARY NO-MAP NO-CONVERT NO-ECHO.
                  REPEAT:
                    IMPORT file-content.
                    /*
                    RUN OutputMemPtr(pdfStream, 
                                     FALSE,
                                     TT_pdf_stream.obj_UniqueID, 
                                     pdf_inc_ObjectSequence, 
                                     file-content).
                    */
                    EXPORT STREAM s_pdf_inc file-content.
                  END.
                INPUT CLOSE.

                PUT STREAM S_pdf_inc UNFORMATTED
                    {&pdfSKIP} {&pdfSKIP} "endstream" {&pdfSKIP}
                    "endobj" {&pdfSKIP}.
              END. /* FontFile2 */

              /* Put Out FontFile3 Dictionary */
              IF TT_Font.file3_obj <> 0 THEN DO:
                FILE-INFO:FILE-NAME = SESSION:TEMP-DIR + TT_Resource.pdf_id + "-" 
                                    + STRING(TT_Font.file3_obj) + "-" 
                                    + STRING(TT_Font.file3_gen) + ".txt".
                SET-SIZE(file-content) = FILE-INFO:FILE-SIZE.
                ObjectSequence(pdfStream, pdf_inc_ObjectSequence + 1, "FontFile3", 0).
                PUT STREAM S_pdf_inc UNFORMATTED
                    pdf_inc_ObjectSequence " 0 obj" {&pdfSKIP}.
 
                INPUT FROM VALUE(SESSION:TEMP-DIR + TT_Resource.pdf_id + "-" 
                                 + STRING(TT_Font.file3_obj) + "-" 
                                 + STRING(TT_Font.file3_gen) + ".txt")
                      BINARY NO-MAP NO-CONVERT NO-ECHO.
                  REPEAT:
                    IMPORT file-content.
                    RUN OutputMemPtr(pdfStream, 
                                     TRUE,
                                     TT_pdf_stream.obj_UniqueID, 
                                     pdf_inc_ObjectSequence, 
                                     file-content).
                    /** igc99 EXPORT STREAM s_pdf_inc file-content. **/
                  END.
                INPUT CLOSE.

                PUT STREAM S_pdf_inc UNFORMATTED
                    {&pdfSKIP} {&pdfSKIP} "endstream" {&pdfSKIP}
                    "endobj" {&pdfSKIP}.
              END. /* FontFile3 */

              SET-SIZE(file-content) = 0.

              /* This had better exist or we are in trouble */
              FIND FIRST TT_pdf_Font 
                   WHERE TT_pdf_Font.obj_Stream = pdfStream
                     AND TT_pdf_Font.font_tag   = TT_Resource.res_text 
                     AND TT_pdf_Font.ext_page   = TT_Resource.page_id NO-ERROR.
              IF AVAIL TT_pdf_Font THEN
                TT_pdf_Font.font_obj = l_Obj.
            
              TT_Resource.new_obj = l_Obj.
            END.
          /* END. /* First-of */  */

        END. /* Each Font Resource */

END. /* LoadExternalFonts */

PROCEDURE LoadExternalXObjects: /* PRIVATE */
  DEFINE INPUT PARAMETER pdfStream  AS CHARACTER NO-UNDO.

  DEFINE VARIABLE l_text      AS CHARACTER NO-UNDO.
  DEFINE VARIABLE l_Obj       AS INTEGER NO-UNDO.
  DEFINE VARIABLE l_ObjIncr   AS INTEGER NO-UNDO.

  DEFINE VARIABLE file-content  AS MEMPTR NO-UNDO.
  DEFINE VARIABLE char-content  AS CHARACTER NO-UNDO.

  DEFINE BUFFER B_TT_Resource FOR TT_Resource.

        FOR EACH TT_Resource
           WHERE TT_Resource.obj_stream = pdfStream
             /* AND TT_Resource.page_id    = TT_pdf_content.obj_page */
             AND TT_Resource.res_type   = "XObject"
          BREAK BY TT_Resource.res_text:
          
          IF FIRST-OF(TT_Resource.res_text) THEN DO:
            SET-SIZE(file-content) = 0.
            SET-SIZE(file-content) = TT_Resource.res_len.

            /* Put Out XObject Dictionary */
            ObjectSequence(pdfStream, pdf_inc_ObjectSequence + 1, "XObject", 0).
            l_Obj = pdf_inc_ObjectSequence.
            l_ObjIncr = pdf_inc_ObjectSequence.
            PUT STREAM S_pdf_inc UNFORMATTED
                l_Obj " 0 obj" {&pdfSKIP}.

            INPUT FROM VALUE(SESSION:TEMP-DIR + TT_Resource.pdf_id + "-" 
                             + STRING(TT_Resource.res_obj) + "-" 
                             + STRING(TT_Resource.res_gen) + ".txt")
                  BINARY NO-MAP NO-CONVERT NO-ECHO.
              REPEAT:
                IMPORT UNFORMATTED l_Text.
                IF TT_Resource.res_old <> "" THEN DO:
                  IF INDEX(L_Text,"~/Name " + TT_Resource.res_old) > 0 THEN
                    L_Text = REPLACE(L_Text, 
                                     "~/Name " + TT_Resource.res_old,
                                     "~/Name " + TT_Resource.res_text).
                END.

                FOR EACH B_TT_Resource 
                   WHERE B_TT_Resource.obj_Stream = pdfStream:
                  /*
                   BREAK BY B_TT_Resource.res_text:

                  IF FIRST-OF(B_TT_REsource.res_text) THEN DO:
                  */
                    IF INDEX(l_Text,  " " + STRING(B_TT_Resource.res_obj) + " " 
                                    + STRING(B_TT_Resource.res_gen) + " R") > 0
                    THEN DO:
                      L_Text = REPLACE(l_Text,
                                       " " + STRING(B_TT_Resource.res_obj) + " " 
                                       + STRING(B_TT_Resource.res_gen) + " R",
                                       " " + STRING(B_TT_Resource.new_obj) + " 0 R"). 
                    END.
                    /*
                  END. /* First-of */ */
                END.

                PUT STREAM s_pdf_inc UNFORMATTED l_Text {&pdfSKIP}.

                IF INDEX(l_Text,"stream") > 0 THEN DO:
                  IMPORT file-content.
                  RUN OutputMemPtr(pdfStream, 
                                   TRUE,
                                   TT_pdf_stream.obj_UniqueID, 
                                   pdf_inc_ObjectSequence, 
                                  file-content).                  
                  /** igc99 EXPORT STREAM s_pdf_inc file-content. **/
                  SET-SIZE(file-content) = 0.
                END.
              END.
            INPUT CLOSE.

            PUT STREAM S_pdf_inc UNFORMATTED
                {&pdfSKIP} {&pdfSKIP}                "endstream" {&pdfSKIP}
                "endobj" {&pdfSKIP}.

            ASSIGN TT_Resource.new_obj = l_Obj
                   TT_Resource.new_gen = 0.

          END. /* First-OF res_text */
        END. /* Each XObject Resource */

END. /* LoadExternalXObjects */

PROCEDURE LoadExtGStates: /* PRIVATE */
  DEFINE INPUT PARAMETER pdfStream  AS CHARACTER NO-UNDO.

  DEFINE VARIABLE l_text  AS CHARACTER NO-UNDO.

  DEFINE VARIABLE file-content  AS MEMPTR NO-UNDO.

        FOR EACH TT_Resource
           WHERE TT_Resource.obj_stream = pdfStream
             /* AND TT_Resource.page_id    = TT_pdf_content.obj_page */
             AND TT_Resource.res_type   = "ExtGState"
          BREAK BY TT_Resource.res_text:
          
          /* Put Out ExtGState Dictionary */
          ObjectSequence(pdfStream, pdf_inc_ObjectSequence + 1, "ExtGState", 0).
          PUT STREAM S_pdf_inc UNFORMATTED
              pdf_inc_ObjectSequence " 0 obj" {&pdfSKIP}.

          INPUT FROM VALUE(SESSION:TEMP-DIR + TT_Resource.pdf_id + "-" 
                           + STRING(TT_Resource.res_obj) + "-" 
                           + STRING(TT_Resource.res_gen) + ".txt")
                BINARY NO-MAP NO-CONVERT NO-ECHO.
            REPEAT:
              IMPORT UNFORMATTED l_Text.

              PUT STREAM s_pdf_inc UNFORMATTED l_Text {&pdfSKIP}.

            END.
          INPUT CLOSE.

          PUT STREAM S_pdf_inc UNFORMATTED
              "endobj" {&pdfSKIP}.

          ASSIGN TT_Resource.new_obj = pdf_inc_ObjectSequence
                 TT_Resource.new_gen = 0.

        END. /* Each ExtGState Resource */

END. /* LoadExtGStates */

PROCEDURE LoadColorSpaces: /* PRIVATE */
  DEFINE INPUT PARAMETER pdfStream  AS CHARACTER NO-UNDO.

  DEFINE VARIABLE l_text      AS CHARACTER NO-UNDO.
  DEFINE VARIABLE l_text_temp AS CHARACTER NO-UNDO.

  DEFINE VARIABLE l_Obj       AS INTEGER NO-UNDO.
  DEFINE VARIABLE l_IndexObj  AS INTEGER NO-UNDO.
  DEFINE VARIABLE l_ObjIncr   AS INTEGER NO-UNDO.

  DEFINE VARIABLE file-content  AS MEMPTR NO-UNDO.

  DEFINE BUFFER B_TT_Resource FOR TT_Resource.

        FOR EACH TT_Resource
           WHERE TT_Resource.obj_stream = pdfStream
             AND TT_Resource.res_type   = "ColorSpace"
             BREAK BY TT_Resource.res_text:

          /* Put Out ColorSpace Dictionary */
          ObjectSequence(pdfStream, pdf_inc_ObjectSequence + 1, "ColorSpace", 0).
          l_Obj = pdf_inc_ObjectSequence.
          l_ObjIncr = pdf_inc_ObjectSequence.
          PUT STREAM S_pdf_inc UNFORMATTED
              l_Obj " 0 obj" {&pdfSKIP}.

          INPUT FROM VALUE(SESSION:TEMP-DIR + TT_Resource.pdf_id + "-" 
                           + STRING(TT_Resource.res_obj) + "-" 
                           + STRING(TT_Resource.res_gen) + ".txt")
                BINARY NO-MAP NO-CONVERT NO-ECHO.
            REPEAT:
              IMPORT UNFORMATTED l_Text.

              /* Change Indexed object for GIF Images */
              IF INDEX(L_Text,"~/Indexed") > 0 THEN DO:
                L_Text_temp = TRIM(SUBSTR(l_text,INDEX(L_Text,"~/Indexed"))).
                L_IndexObj = INT(ENTRY(2,l_Text_temp," ")).

                /* Process Base */
                IF l_IndexObj > 0 THEN DO:
                  FIND FIRST B_TT_Resource 
                       WHERE B_TT_Resource.obj_stream = pdfStream
                         AND B_TT_Resource.res_obj    = l_IndexObj
                         AND B_TT_Resource.res_gen    = 0 NO-ERROR.
                  
                  L_Text = REPLACE(L_Text,
                                   "~/Indexed " + STRING(l_IndexObj),
                                   "~/Indexed " + STRING(B_TT_Resource.new_obj)).
                END. /* Process Base */
              END.

              FOR EACH B_TT_Resource 
                 WHERE B_TT_Resource.obj_Stream = pdfStream
                   AND B_TT_Resource.par_obj = TT_Resource.res_obj
                   AND B_TT_Resource.par_gen = TT_Resource.res_gen:
                IF INDEX(l_Text,  " " + STRING(B_TT_Resource.res_obj) + " " 
                                + STRING(B_TT_Resource.res_gen) + " R") > 0
                THEN DO:
                  L_ObjIncr = l_ObjIncr + 1.
                  B_TT_Resource.new_obj = L_ObjIncr.          
                  L_Text = REPLACE(l_Text,
                                   " " + STRING(B_TT_Resource.res_obj) + " " 
                                   + STRING(B_TT_Resource.res_gen) + " R",
                                   " " + STRING(B_TT_Resource.new_obj) + " 0 R").
                END.
              END.

              PUT STREAM s_pdf_inc UNFORMATTED l_Text {&pdfSKIP}.

            END.
          INPUT CLOSE.

          PUT STREAM S_pdf_inc UNFORMATTED
              "endobj" {&pdfSKIP}.

          ASSIGN TT_Resource.new_obj = l_Obj
                 TT_Resource.new_gen = 0.

          FOR EACH B_TT_Resource 
             WHERE B_TT_Resource.obj_Stream = pdfStream
               AND B_TT_Resource.par_obj = TT_Resource.res_obj
               AND B_TT_Resource.par_gen = TT_Resource.res_gen:

            /* Put Out ColorSpace-Resource Dictionary */
            ObjectSequence(pdfStream, pdf_inc_ObjectSequence + 1, "ColorSpace-Resource", 0).
            FILE-INFO:FILE-NAME = SESSION:TEMP-DIR + B_TT_Resource.pdf_id + "-" 
                                + STRING(B_TT_Resource.res_obj) + "-" 
                                + STRING(B_TT_Resource.res_gen) + ".txt".
            SET-SIZE(file-content) = FILE-INFO:FILE-SIZE.


            PUT STREAM S_pdf_inc UNFORMATTED
                B_TT_Resource.new_obj " 0 obj" {&pdfSKIP}.

            INPUT FROM VALUE(FILE-INFO:FILE-NAME)
                  BINARY NO-MAP NO-CONVERT NO-ECHO.
              REPEAT:
                IMPORT file-content.
                RUN OutputMemPtr(pdfStream, 
                                 TRUE,
                                 TT_pdf_stream.obj_UniqueID, 
                                 pdf_inc_ObjectSequence, 
                                 file-content).
                /** igc99 EXPORT STREAM s_pdf_inc file-content. **/
              END.
            INPUT CLOSE.

            PUT STREAM S_pdf_inc UNFORMATTED
                {&pdfSKIP} {&pdfSKIP} "endstream" {&pdfSKIP}
                "endobj" {&pdfSKIP}.
          END.
        END. /* Each ColorSpace Resource */

END. /* LoadColorSpaces */

PROCEDURE LoadFunction: /* PRIVATE */
  DEFINE INPUT PARAMETER pdfStream  AS CHARACTER NO-UNDO.

  DEFINE VARIABLE l_text      AS CHARACTER NO-UNDO.
  DEFINE VARIABLE l_text_temp AS CHARACTER NO-UNDO.

  DEFINE VARIABLE l_Obj       AS INTEGER NO-UNDO.
  DEFINE VARIABLE l_IndexObj  AS INTEGER NO-UNDO.
  DEFINE VARIABLE l_ObjIncr   AS INTEGER NO-UNDO.

  DEFINE VARIABLE file-content  AS MEMPTR NO-UNDO.

  DEFINE BUFFER B_TT_Resource FOR TT_Resource.

        FOR EACH TT_Resource
           WHERE TT_Resource.obj_stream = pdfStream
             AND TT_Resource.res_type   = "Function"
          BREAK BY TT_Resource.res_text:
          
          /* Put Out Function Dictionary */
          ObjectSequence(pdfStream, pdf_inc_ObjectSequence + 1, "Shading", 0).
          l_Obj = pdf_inc_ObjectSequence.
          l_ObjIncr = pdf_inc_ObjectSequence.
          PUT STREAM S_pdf_inc UNFORMATTED
              l_Obj " 0 obj" {&pdfSKIP}.

          INPUT FROM VALUE(SESSION:TEMP-DIR + TT_Resource.pdf_id + "-" 
                           + STRING(TT_Resource.res_obj) + "-" 
                           + STRING(TT_Resource.res_gen) + ".txt")
                BINARY NO-MAP NO-CONVERT NO-ECHO.
            REPEAT:
              IMPORT UNFORMATTED l_Text.

              FOR EACH B_TT_Resource 
                 WHERE B_TT_Resource.obj_Stream = pdfStream
                   AND B_TT_Resource.par_obj = TT_Resource.res_obj
                   AND B_TT_Resource.par_gen = TT_Resource.res_gen:
                IF INDEX(l_Text,  STRING(B_TT_Resource.res_obj) + " " 
                                + STRING(B_TT_Resource.res_gen) + " R") > 0
                THEN DO:
                  L_ObjIncr = l_ObjIncr + 1.
                  B_TT_Resource.new_obj = L_ObjIncr.
                  L_Text = REPLACE(l_Text,
                                   STRING(B_TT_Resource.res_obj) + " " 
                                   + STRING(B_TT_Resource.res_gen) + " R",
                                   STRING(B_TT_Resource.new_obj) + " 0 R").
                END.
              END.

              PUT STREAM s_pdf_inc UNFORMATTED l_Text {&pdfSKIP}.

            END.
          INPUT CLOSE.

          PUT STREAM S_pdf_inc UNFORMATTED
              "endobj" {&pdfSKIP}.

          ASSIGN TT_Resource.new_obj = l_Obj
                 TT_Resource.new_gen = 0.

        END. /* Each Shading Resource */

END. /* LoadFunction */

PROCEDURE LoadShading: /* PRIVATE */
  DEFINE INPUT PARAMETER pdfStream  AS CHARACTER NO-UNDO.

  DEFINE VARIABLE l_text      AS CHARACTER NO-UNDO.
  DEFINE VARIABLE l_text_temp AS CHARACTER NO-UNDO.

  DEFINE VARIABLE l_Obj       AS INTEGER NO-UNDO.
  DEFINE VARIABLE l_IndexObj  AS INTEGER NO-UNDO.
  DEFINE VARIABLE l_ObjIncr   AS INTEGER NO-UNDO.

  DEFINE VARIABLE file-content  AS MEMPTR NO-UNDO.

  DEFINE BUFFER B_TT_Resource FOR TT_Resource.

        FOR EACH TT_Resource
           WHERE TT_Resource.obj_stream = pdfStream
             /* AND TT_Resource.page_id    = TT_pdf_content.obj_page */
             AND TT_Resource.res_type   = "Shading"
          BREAK BY TT_Resource.res_text:
          
          /* Put Out Shading Dictionary */
          ObjectSequence(pdfStream, pdf_inc_ObjectSequence + 1, "Shading", 0).
          l_Obj = pdf_inc_ObjectSequence.
          l_ObjIncr = pdf_inc_ObjectSequence.
          PUT STREAM S_pdf_inc UNFORMATTED
              l_Obj " 0 obj" {&pdfSKIP}.

          INPUT FROM VALUE(SESSION:TEMP-DIR + TT_Resource.pdf_id + "-" 
                           + STRING(TT_Resource.res_obj) + "-" 
                           + STRING(TT_Resource.res_gen) + ".txt")
                BINARY NO-MAP NO-CONVERT NO-ECHO.
            REPEAT:
              IMPORT UNFORMATTED l_Text.

              /* Change Function object for Shading */
              IF INDEX(L_Text,"~/Function") > 0 THEN DO:
                L_Text_temp = TRIM(SUBSTR(l_text,INDEX(L_Text,"~/Function"))).
                L_IndexObj = INT(ENTRY(2,l_Text_temp," ")).

                IF l_IndexObj > 0 THEN DO:
                  FIND FIRST B_TT_Resource 
                       WHERE B_TT_Resource.obj_stream = pdfStream
                         AND B_TT_Resource.res_obj    = l_IndexObj
                         AND B_TT_Resource.res_gen    = 0 NO-LOCK NO-ERROR.

                  L_Text = REPLACE(L_Text,
                                   "~/Function " + STRING(l_IndexObj),
                                   "~/Function " + STRING(B_TT_Resource.new_obj)).
                END.

              END.

              /* Change ColorSpace object for Shading */
              IF INDEX(L_Text,"~/ColorSpace") > 0 THEN DO:
                L_Text_temp = TRIM(SUBSTR(l_text,INDEX(L_Text,"~/ColorSpace"))).
                L_IndexObj = INT(ENTRY(2,l_Text_temp," ")).

                IF l_IndexObj > 0 THEN DO:
                  FIND FIRST B_TT_Resource 
                       WHERE B_TT_Resource.obj_stream = pdfStream
                         AND B_TT_Resource.res_obj    = l_IndexObj
                         AND B_TT_Resource.res_gen    = 0 NO-LOCK NO-ERROR.

                  L_Text = REPLACE(L_Text,
                                   "~/ColorSpace " + STRING(l_IndexObj),
                                   "~/ColorSpace " + STRING(B_TT_Resource.new_obj)).
                END.

              END.

              FOR EACH B_TT_Resource 
                 WHERE B_TT_Resource.obj_Stream = pdfStream
                   AND B_TT_Resource.par_obj = TT_Resource.res_obj
                   AND B_TT_Resource.par_gen = TT_Resource.res_gen:
                IF INDEX(l_Text,  STRING(B_TT_Resource.res_obj) + " " 
                                + STRING(B_TT_Resource.res_gen) + " R") > 0
                THEN DO:
                  L_ObjIncr = l_ObjIncr + 1.
                  B_TT_Resource.new_obj = L_ObjIncr.
                  L_Text = REPLACE(l_Text,
                                   STRING(B_TT_Resource.res_obj) + " " 
                                   + STRING(B_TT_Resource.res_gen) + " R",
                                   STRING(B_TT_Resource.new_obj) + " 0 R").
                END.
              END.

              PUT STREAM s_pdf_inc UNFORMATTED l_Text {&pdfSKIP}.

            END.
          INPUT CLOSE.

          PUT STREAM S_pdf_inc UNFORMATTED
              "endobj" {&pdfSKIP}.

          ASSIGN TT_Resource.new_obj = l_Obj
                 TT_Resource.new_gen = 0.

        END. /* Each Function Resource */
END. /* LoadShading */

/* Added functionality from Robert Ayris [rayris@comops.com.au] */

FUNCTION HexToInt returns int
        (input vp_Hex         as char) :  /* PRIVATE */

    def var vl_Num            as int  no-undo.
    def var vl_i              as int  no-undo.
    def var vl_pos            as int  no-undo.

    do vl_i = 1 to length(vp_Hex):
        vl_Pos = index("0123456789ABCDEF", substring(vp_Hex, vl_i, 1)).
        if vl_Pos = 0 then 
        do:
            vl_Num = 0.
            leave.
        end.
        if vl_Pos LE 10 then
            vl_Num = vl_Num + integer(substring(vp_Hex, vl_i, 1)) * exp(16, (length(vp_Hex) - vl_i)).
        else
            vl_Num = vl_Num + (vl_Pos - 1) * exp(16, (length(vp_Hex) - vl_i)).
    end.
    
    return vl_Num.

END FUNCTION.

FUNCTION GetPDFColor returns dec
        (input vp_Color      as char) :  /* PRIVATE */

   def var vl_Num             as int  no-undo.
   
   if substring(vp_Color, 1, 2) = "0x":U then
      vl_Num = HexToInt(substring(vp_Color, 3)).
   else
      vl_Num = int(vp_Color).
      
   if vl_Num GT 255 then return 1.0.      
      
   return round(vl_Num / 255, 4).
         
END FUNCTION.         

PROCEDURE pdf_rgb :
   def input param vp_Stream              as char no-undo.
   def input param vp_Function            as char no-undo.
   def input param vp_Color               as char no-undo.
   
   def var vl_rgb                         as dec  no-undo extent 3.

   if substring(vp_Color, 1, 2) = "0x":U and length(vp_Color) = 8 then       /* Hex Value 0xRRGGBB */
      assign vl_rgb[1] = GetPDFColor("0x":U + substring(vp_color, 3, 2))     /* RED */
             vl_rgb[2] = GetPDFColor("0x":U + substring(vp_color, 5, 2))     /* GREEN */
             vl_rgb[3] = GetPDFColor("0x":U + substring(vp_color, 7, 2))     /* BLUE */
             .
   if substring(vp_color, 1, 1) = "#":U and length(vp_color) = 7 then        /* Hex Value #RRGGBB */
      assign vl_rgb[1] = GetPDFColor("0x":U + substring(vp_color, 2, 2))     /* RED */
             vl_rgb[2] = GetPDFColor("0x":U + substring(vp_color, 4, 2))     /* GREEN */
             vl_rgb[3] = GetPDFColor("0x":U + substring(vp_color, 6, 2))     /* BLUE */
             .
   else if length(vp_color) = 9 then                                         /* Dec Value RRRGGGBBB */
      assign vl_rgb[1] = GetPDFColor(substring(vp_color, 1, 3))              /* RED */
             vl_rgb[2] = GetPDFColor(substring(vp_color, 4, 3))              /* GREEN */
             vl_rgb[3] = GetPDFColor(substring(vp_color, 7, 3))              /* BLUE */
             .
   else if num-entries(vp_color) = 3 then
     assign vl_rgb[1] = GetPDFColor(ENTRY(1,vp_color))              /* RED */
            vl_rgb[2] = GetPDFColor(ENTRY(2,vp_color))              /* GREEN */
            vl_rgb[3] = GetPDFColor(ENTRY(3,vp_color))              /* BLUE */
            .

   if can-do("pdf_text_color,pdf_stroke_color,pdf_stroke_fill", vp_Function) then
      run value(vp_Function)
         (vp_Stream, vl_rgb[1], vl_rgb[2], vl_rgb[3]).
      
END PROCEDURE. /* pdf_rgb */

PROCEDURE pdf_exec_footer:

  DEFINE INPUT PARAMETER pdfStream  AS CHARACTER NO-UNDO.

  IF NOT CAN-FIND(FIRST TT_pdf_stream
                  WHERE TT_pdf_stream.obj_stream = pdfStream NO-LOCK)
  THEN DO:
    RUN pdf_error(pdfStream,"pdf_exec_footer","Cannot find Stream!").
    RETURN .
  END.
  
  FIND FIRST TT_pdf_stream WHERE TT_pdf_stream.obj_stream = pdfStream
       NO-ERROR.

  /* Print the footer */
  IF TT_pdf_stream.obj_Footer <> "" THEN DO:

    IF pdf_Page(pdfStream) >= 1 THEN DO:
    
       pdf_ForFooter = TRUE.

       RUN pdf_set_TextY(pdfStream,pdf_BottomMargin(pdfStream)).

       RUN OutputTextContent(pdfStream, 
                             "TEXT",
                               "1 0 0 1 " 
                             + STRING(pdf_LeftMargin(pdfStream)) + " " 
                             + STRING(pdf_BottomMargin(pdfStream)) + " Tm",
                             "",
                             "").

       RUN VALUE(TT_pdf_stream.obj_Footer) IN TT_pdf_stream.obj_CallProc NO-ERROR.

       RUN pdf_set_TextY(pdfStream,pdf_PageHeight(pdfStream)).

       pdf_ForFooter = FALSE.
    END.
  
  END. /* Footer */

END PROCEDURE. /* pdf_exec_footer */

PROCEDURE pdf_load_xml :
  DEFINE INPUT PARAMETER pdfStream   AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfXMLFile AS CHARACTER NO-UNDO.

  DEFINE VARIABLE hDoc    AS HANDLE NO-UNDO.
  DEFINE VARIABLE hRoot   AS HANDLE NO-UNDO.
  DEFINE VARIABLE hNode   AS HANDLE NO-UNDO.

  DEFINE VARIABLE Good    AS LOGICAL NO-UNDO.

  IF NOT CAN-FIND(FIRST TT_pdf_stream
                  WHERE TT_pdf_stream.obj_stream = pdfStream NO-LOCK)
  THEN DO:
    RUN pdf_error(pdfStream,"pdf_load_font","Cannot find Stream!").
    RETURN .
  END.

  CREATE X-DOCUMENT hDoc.
  CREATE X-NODEREF  hRoot.
  CREATE X-NODEREF  hNode.

  Good = hDoc:LOAD("FILE",pdfXMLFile,FALSE) NO-ERROR.
  Good = hdoc:GET-DOCUMENT-ELEMENT(hroot) NO-ERROR.

  IF NOT Good THEN STOP.

  RUN LoadXMLNode(pdfstream, hRoot,"/" + hRoot:NAME,0).

  DELETE OBJECT hRoot.
  DELETE OBJECT hDoc.
  DELETE OBJECT hNode.

END. /* pdf_load_xml */

PROCEDURE LoadXMLNode: /* PRIVATE */

  DEFINE INPUT PARAMETER pdfStream   AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pNode       AS HANDLE    NO-UNDO.
  DEFINE INPUT PARAMETER pNodeName   AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pParentNode AS INTEGER NO-UNDO.

  DEFINE VARIABLE X-Child AS INTEGER NO-UNDO.
  
  DEFINE VARIABLE Good    AS LOGICAL NO-UNDO.

  DEFINE VARIABLE hNode        AS HANDLE NO-UNDO.
  DEFINE VARIABLE hChildValue  AS HANDLE NO-UNDO.

  DEFINE BUFFER B_TT_pdf_xml FOR TT_pdf_xml.

  CREATE X-NODEREF  hNode.
  CREATE X-NODEREF  hChildValue.

  DO X-Child = 1 TO pNode:NUM-CHILDREN:

    Good = pNode:GET-CHILD(hNode,x-child) NO-ERROR.

    IF NOT Good THEN NEXT.

    IF INDEX(hNode:NAME,"#text") > 0 THEN NEXT.

    Good = hNode:GET-CHILD(hChildValue,1) NO-ERROR.
    IF NOT Good THEN NEXT.

    CREATE B_TT_pdf_xml.
    ASSIGN B_TT_pdf_xml.obj_stream = pdfStream
           B_TT_pdf_xml.xml_parent = pNodeName
           B_TT_pdf_xml.xml_pnode  = pParentNode
           B_TT_pdf_xml.xml_node   = hNode:NAME
           B_TT_pdf_xml.xml_value  = hChildValue:NODE-VALUE
           B_TT_pdf_xml.xml_seq    = xml-seq + 1
           xml-seq                 = xml-seq + 1.

    /* remove CHR(10) from value -- don't know why it's setting this */
    IF B_TT_pdf_xml.xml_value = CHR(10) THEN
      B_TT_pdf_xml.xml_value = "".

    RUN LoadXMLNode(pdfStream, hNode,pNodeName + "/" + hNode:Name,xml-seq).

  END.

  DELETE OBJECT hNode.
  DELETE OBJECT hChildValue.
END. /* LoadXMLNode */

FUNCTION GetXMLNodeValue RETURNS CHARACTER 
  (INPUT pParent  AS CHARACTER,
   INPUT pNode    AS CHARACTER ):
  
  DEFINE BUFFER B_TT_pdf_xml FOR TT_pdf_xml.

  FIND FIRST B_TT_pdf_xml 
       WHERE B_TT_pdf_xml.xml_parent = pParent
         AND B_TT_pdf_xml.xml_node   = pNode 
         NO-LOCK NO-ERROR.
  IF AVAIL B_TT_pdf_xml THEN
    RETURN B_TT_pdf_xml.xml_value.
  ELSE
    RETURN "".
END. /* GetXMLNodeValue */

PROCEDURE OutputTextContent: /* PRIVATE */
  DEFINE INPUT PARAMETER pdfStream      AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfType        AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfPreContent  AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfContent     AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfPostContent AS CHARACTER NO-UNDO.

  DEFINE VARIABLE L_Font        AS CHARACTER NO-UNDO.
  L_font = pdf_Font(pdfStream).

  FIND FIRST TT_pdf_font
       WHERE TT_pdf_font.obj_stream = pdfStream
         AND TT_pdf_font.font_name  = L_Font NO-LOCK NO-ERROR.
  L_Font = IF AVAIL TT_pdf_font THEN TT_pdf_Font.font_tag ELSE "/BF1".

  FIND FIRST TT_pdf_stream WHERE TT_pdf_stream.obj_stream = pdfStream NO-ERROR.
  IF NOT AVAIL TT_pdf_stream THEN RETURN.

  IF pdf_Page(pdfStream) > 0 THEN DO:

    /* Determine if the stream name has changed during processing and if so
       close the old stream and open the new stream */
    IF pdfStream <> pdf_CurrentStream THEN DO:
      OUTPUT STREAM S_pdf_out CLOSE.

      OUTPUT STREAM S_pdf_out TO 
             VALUE(  SESSION:TEMP-DIR 
                   + TT_pdf_stream.obj_UniqueID  
                   + "-Content-" 
                   + STRING(pdf_Page(pdfStream)) + ".txt") 
                   BINARY NO-MAP NO-CONVERT APPEND.

      pdf_CurrentStream = pdfStream.
    END.

    IF pdfType BEGINS "TEXT" AND TT_pdf_Stream.obj_DoingText THEN DO:
      RUN PutStreamContent(pdfStream, 
                           pdfPreContent,
                           pdfContent,
                           pdfPostContent).
      RETURN.
    END.

    IF pdfType BEGINS "IMAGE" THEN DO:
     
      IF TT_pdf_stream.obj_DoingGraphic THEN
        PUT STREAM S_pdf_out UNFORMATTED 
                       {&pdfSkip}
                   "Q" {&pdfSkip}
                   "q" {&pdfSkip}.
      ELSE IF NOT TT_pdf_Stream.obj_DoingGraphic THEN DO:
        IF TT_pdf_stream.obj_DoingText THEN
           PUT STREAM S_pdf_out UNFORMATTED 
                           {&pdfSkip}
                      "ET" {&pdfSkip}.

        PUT STREAM S_pdf_out UNFORMATTED 
                       {&pdfSkip}
                   "q" {&pdfSkip}.
      END.

      ASSIGN TT_pdf_stream.obj_DoingText    = FALSE
             TT_pdf_stream.obj_DoingGraphic = TRUE.
    END. /* End Image */

    ELSE IF pdfType BEGINS "TEXT" AND NOT TT_pdf_Stream.obj_DoingText THEN DO:
      IF TT_pdf_Stream.obj_DoingGraphic THEN
        PUT STREAM S_pdf_out UNFORMATTED 
                       {&pdfSkip}
                   "Q" {&pdfSkip}.

      PUT STREAM S_pdf_out UNFORMATTED 
                           "BT" + CHR(10)
                           + L_Font + " " + dec2string(pdf_PointSize(pdfStream)) + " Tf" + CHR(10)
                           + " 1 0 0 1 " + STRING(pdf_LeftMargin(pdfStream)) + " "
                           + STRING(pdf_TextY(pdfStream))
                           + " Tm"
                           {&pdfSKIP}. 

      ASSIGN TT_pdf_Stream.obj_DoingGraphic = FALSE
             TT_pdf_Stream.obj_DoingText    = TRUE.
    END.

    ELSE IF TT_pdf_Stream.obj_DoingText AND NOT pdfType BEGINS "Text" THEN DO:
      PUT STREAM S_pdf_out UNFORMATTED 
               {&pdfSkip}
          "ET" {&pdfskip}
          "q" {&pdfskip}.

      ASSIGN TT_pdf_Stream.obj_DoingText    = FALSE
             TT_pdf_Stream.obj_DoingGraphic = TRUE.
    END.
    
    ELSE IF TT_pdf_Stream.obj_DoingGraphic AND pdfType BEGINS "TEXT" THEN DO:
      PUT STREAM S_pdf_out UNFORMATTED 
              {&pdfSkip}
          "Q" {&pdfskip}
          "BT" {&pdfskip}.

      ASSIGN TT_pdf_Stream.obj_DoingText    = TRUE
             TT_pdf_Stream.obj_DoingGraphic = FALSE.
    END.

    ELSE IF NOT TT_pdf_Stream.obj_DoingGraphic 
         AND NOT pdfType BEGINS "TEXT" THEN DO:
      PUT STREAM S_pdf_out UNFORMATTED 
              {&pdfSkip}
          "q" {&pdfskip}.

      ASSIGN TT_pdf_Stream.obj_DoingText    = FALSE
             TT_pdf_Stream.obj_DoingGraphic = TRUE.
    END.

    RUN PutStreamContent(pdfStream, 
                         pdfPreContent,
                         pdfContent,
                         pdfPostContent).

    IF pdfType BEGINS "IMAGE" THEN DO:
      PUT STREAM S_pdf_out UNFORMATTED 
                     {&pdfSkip}
                 "Q" {&pdfSkip}.
      ASSIGN TT_pdf_Stream.obj_DoingText = FALSE
             TT_pdf_Stream.obj_DoingGraphic = FALSE.
    END.

  END. /* Page > 0 */

END. /* OutputTextContent */

FUNCTION StripBoldItal RETURNS CHARACTER
  (INPUT pString  AS CHARACTER):

  ASSIGN pString = REPLACE(pString,{&BoldOnChar},"")
         pString = REPLACE(pString,{&BoldOffChar},"")
         pString = REPLACE(pString,{&ItalicOnChar},"")
         pString = REPLACE(pString,{&ItalicOffChar},"").

  RETURN pString.
END FUNCTION.

PROCEDURE PutStreamContent:  /* PRIVATE */

  DEFINE INPUT PARAMETER pdfStream      AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfPreContent  AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfContent     AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfPostContent AS CHARACTER NO-UNDO.

  DEFINE VARIABLE cDefaultFont    AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cBoldFont       AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cItalicFont     AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cBoldItalicFont AS CHARACTER NO-UNDO.

  DEFINE VARIABLE cColor          AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cColors         AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cColorChar      AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cOpenPar        AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cChar           AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cString         AS CHARACTER NO-UNDO.

  DEFINE VARIABLE iChar           AS INTEGER NO-UNDO.
  DEFINE VARIABLE iColor          AS INTEGER NO-UNDO.
  DEFINE VARIABLE iNumColors      AS INTEGER NO-UNDO.


  PUT STREAM S_pdf_out UNFORMATTED pdfPreContent.

  /* Convert tags to single characters - easier to work with */
  IF pdf_get_parameter(pdfStream,"UseTags") = "TRUE" THEN
    ASSIGN pdfContent = REPLACE(pdfContent, "<B>",  {&BoldOnChar} )
           pdfContent = REPLACE(pdfContent, "<~/B>", {&BoldOffChar} )
           pdfContent = REPLACE(pdfContent, "<I>",  {&ItalicOnChar} )
           pdfContent = REPLACE(pdfContent, "<~/I>", {&ItalicOffChar} ) 
           pdfContent = REPLACE(pdfContent, "<Color=",  {&ColorOnChar} )
           pdfContent = REPLACE(pdfContent, "<~/Color>", {&ColorOffChar} ) 
           NO-ERROR.

  IF pdf_get_parameter(pdfStream,"UseTags") = "TRUE" 
  AND (   INDEX(pdfContent,{&BoldOnChar} ) > 0
       OR INDEX(pdfContent,{&BoldOffChar} ) > 0
       OR INDEX(pdfContent,{&ItalicOnChar} ) > 0
       OR INDEX(pdfContent,{&ItalicOffChar} ) > 0
       OR INDEX(pdfContent,{&ColorOnChar} ) > 0
       OR INDEX(pdfContent,{&ColorOffChar} ) > 0
       )
  THEN DO:

    /* Obtain Tag font associations */
    ASSIGN cBoldFont       = pdf_get_parameter(pdfStream,"BoldFont")
           cItalicFont     = pdf_get_parameter(pdfStream,"ItalicFont")
           cBoldItalicFont = pdf_get_parameter(pdfStream,"BoldItalicFont")
           cDefaultFont    = pdf_get_parameter(pdfStream,"DefaultFont").

    /* Loop through the text finding the blocks to change */
    DO iChar = 1 TO LENGTH(pdfContent):
      cChar = SUBSTRING(pdfContent, iChar, 1).

      /* Toggle Font */
      CASE cChar:
        /* Toggle Bold ON */
        WHEN {&BoldOnChar} THEN DO:
          /* Write buffer at current position */
          IF LENGTH(cString) > 1 THEN 
            PUT STREAM S_pdf_out UNFORMATTED 
                StripBoldItal(cString)
                pdfPostContent {&pdfSKIP}.

          cString = "(".

          /* Increment the Bold level counter */
          RUN pdf_set_parameter(pdfStream,
                                "BoldCount",
                                STRING(INT(pdf_get_parameter(pdfStream,
                                                             "BoldCount")) + 1)).

          /* Determine whether we should bold and italicize or just bold */
          IF INT(pdf_get_parameter(pdfStream,"ItalicLevel")) > 0 THEN 
            RUN pdf_set_font (pdfStream, 
                              cBoldItalicFont, 
                              pdf_PointSize(pdfStream)) NO-ERROR.
          ELSE 
            RUN pdf_set_font (pdfStream, 
                              cBoldFont,
                              pdf_PointSize(pdfStream)) NO-ERROR.
        END. /* BoldOn */

        /* Toggle Bold OFF */
        WHEN {&BoldOffChar} THEN DO:
          /* Write buffer at current position */
          PUT STREAM S_pdf_out UNFORMATTED 
              StripBoldItal(cString)
              pdfPostContent {&pdfSKIP}.

          cString = "(".

          /* Decrement the Bold level counter */
          RUN pdf_set_parameter(pdfStream,
                                "BoldCount",
                                STRING(INT(pdf_get_parameter(pdfStream,
                                                             "BoldCount")) - 1)).

          /* Determine whether we return to Italics or the Default font */
          IF INT(pdf_get_parameter(pdfStream,"BoldCount")) <= 0 THEN DO: 
            IF INT(pdf_get_parameter(pdfStream,"ItalicLevel")) > 0 THEN 
              RUN pdf_set_font (pdfStream, 
                                cItalicFont,
                                pdf_PointSize(pdfStream)) NO-ERROR.
            ELSE 
              RUN pdf_set_font (pdfStream, 
                                cDefaultFont, 
                                pdf_PointSize(pdfStream)) NO-ERROR.
          END.
        END.

        /* Toggle Italics ON */
        WHEN {&ItalicOnChar} THEN DO:
          PUT STREAM S_pdf_out UNFORMATTED 
              StripBoldItal(cString)
              pdfPostContent {&pdfSKIP}.

          cString   = "(".

          /* Increment the Italic level counter */
          RUN pdf_set_parameter(pdfStream,
                                "ItalicCount",
                                STRING(INT(pdf_get_parameter(pdfStream,
                                                             "ItalicCount")) + 1)).

          /* Determine whether we should italicize and bold or just italicize */
          IF INT(pdf_get_parameter(pdfStream,"BoldCount")) > 0 THEN 
            RUN pdf_set_font (pdfStream, 
                              cBoldItalicFont,    
                              pdf_PointSize(pdfStream)) NO-ERROR.
          ELSE 
            RUN pdf_set_font (pdfStream, 
                              cItalicFont,
                              pdf_PointSize(pdfStream)) NO-ERROR.
        END.

        /* Toggle Italics OFF */
        WHEN {&ItalicOffChar} THEN DO:
          PUT STREAM S_pdf_out UNFORMATTED 
              StripBoldItal(cString)
              pdfPostContent {&pdfSKIP}.

          cString = "(".

          /* Decrement the Bold level counter */
          RUN pdf_set_parameter(pdfStream,
                                "ItalicCount",
                                STRING(INT(pdf_get_parameter(pdfStream,
                                                             "ItalicCount")) - 1)).

          /* Determine whether we return to Italics or the Default font */
          IF INT(pdf_get_parameter(pdfStream,"ItalicCount")) <= 0 THEN DO: 
            IF INDEX(pdf_Font(pdfStream), "Bold") > 0 THEN 
              RUN pdf_set_font (pdfStream, 
                                cBoldFont,
                                pdf_PointSize(pdfStream)) NO-ERROR.
            ELSE 
              RUN pdf_set_font (pdfStream, 
                                cDefaultFont,
                                pdf_PointSize(pdfStream)) NO-ERROR.
          END.
        END.

        /* Toggle Color ON */
        WHEN {&ColorOnChar} THEN DO:
          PUT STREAM S_pdf_out UNFORMATTED 
              StripBoldItal(cString)
              pdfPostContent {&pdfSKIP}.

          ASSIGN cString   = "("
                 cColor    = "".

          /* Read the colour */
          COLOR-LOOP:
          DO iColor = (iChar + 1) TO LENGTH(pdfContent):
            cColorChar= SUBSTRING(pdfContent, iColor, 1).

            IF cColorChar = ">" THEN LEAVE COLOR-LOOP.

            ELSE
              cColor = cColor + cColorChar.
          END.
          
          iChar = iColor.

          /* Store the Color */
          RUN pdf_set_parameter(pdfStream,
                                "ColorLevel",
                                IF pdf_get_parameter(pdfStream, "ColorLevel") <> "" 
                                THEN
                                  pdf_get_parameter(pdfStream, "ColorLevel") 
                                  + "," + cColor
                                ELSE 
                                  cColor).

          /* Find Color settings based on Color Name */
          cColor = pdf_get_parameter(pdfStream,
                                     "TagColor:" + cColor).
          IF cColor = "" THEN
            cColor = "0,0,0".

          /* Set Color */
          RUN pdf_rgb (pdfStream,
                       "pdf_text_color",
                       cColor).

        END.

        /* Toggle Color OFF */
        WHEN {&ColorOffChar} THEN DO:
          PUT STREAM S_pdf_out UNFORMATTED 
              StripBoldItal(cString)
              pdfPostContent {&pdfSKIP}.

          ASSIGN cString = "("
                 cColor  = "".

          /* Remove the Last Color */
          cColors    = pdf_get_parameter(pdfStream,"ColorLevel").
          iNumColors = NUM-ENTRIES(cColors) - 1.

          IF iNumColors >= 1 THEN 
          DO iColor = 1 TO iNumColors:
            RUN pdf_set_parameter(pdfStream,
                                  "ColorLevel",
                                    IF iColor > 1 THEN
                                      pdf_get_parameter(pdfStream, "ColorLevel") 
                                      + "," + ENTRY(iColor,cColors)
                                    ELSE 
                                      ENTRY(iColor,cColors)).
          END.

          /* Remove last entry */
          IF iNumColors < 1 THEN DO:
            RUN pdf_set_parameter(pdfStream,
                                  "ColorLevel",
                                  "").

            cColor = pdf_get_parameter(pdfStream,"DefaultColor").
            IF cColor = "" THEN
              cColor = "Black".
          END. 

          ELSE DO:
            cColor = ENTRY( iNumColors,
                            cColors ).

          END.

          /* Find Color settings based on Color Name */
          cColor = pdf_get_parameter(pdfStream,
                                     "TagColor:" + cColor).
          IF cColor = "" THEN
            cColor = "0,0,0".

          /* Set Color */
          RUN pdf_rgb (pdfStream,
                       "pdf_text_color",
                       cColor).
        END.

        /* Append to the current block of text */
        OTHERWISE ASSIGN cString = cString + cChar NO-ERROR.
      END CASE.

    END. /* Do iChar */

    /* Write any remaingin text at the current position */
    IF LENGTH(cString) > 0 THEN 
      PUT STREAM S_pdf_out UNFORMATTED 
          StripBoldItal(cString)
          pdfPostContent {&pdfSKIP}.

  END. /* Using Tags and Tags Found in Text */

  ELSE DO:
    PUT STREAM S_pdf_out UNFORMATTED pdfContent.

    IF INDEX(pdfContent,"@@TOTALPages-" + pdfStream) > 0 THEN
      TT_pdf_page.UseTotalPages = TRUE.

    IF INDEX(pdfContent,"@@PAGEno-" + pdfStream) > 0 THEN
      TT_pdf_page.UsePageNo = TRUE.

    PUT STREAM S_pdf_out UNFORMATTED pdfPostContent {&pdfSKIP}.

  END.


END. /* PutStreamContent */

PROCEDURE ChangePageText: /* PRIVATE */
  DEFINE INPUT PARAMETER pdfStream    AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfPage      AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER pdfFromText  AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfToText    AS CHARACTER NO-UNDO.

  DEFINE BUFFER B_TT_stream FOR TT_pdf_stream.

  DEFINE VARIABLE vEditor AS CHARACTER NO-UNDO.

  DEFINE FRAME f-editor
         veditor VIEW-AS EDITOR SIZE 40 BY 5 LARGE.
  
  FIND FIRST B_TT_Stream WHERE B_TT_Stream.obj_Stream = pdfStream 
       NO-LOCK NO-ERROR.
  IF NOT AVAIL B_TT_Stream THEN RETURN.

  vEditor:INSERT-FILE(  SESSION:TEMP-DIR 
                      + B_TT_stream.obj_UniqueID  
                      + "-Content-" 
                      + STRING(pdfPage) + ".txt").
  vEditor:REPLACE(pdfFromText,
                  pdfToText,
                  8). /* Global */
  vEditor:SAVE-FILE(  SESSION:TEMP-DIR 
                      + B_TT_stream.obj_UniqueID  
                      + "-Content-" 
                      + STRING(pdfPage) + ".txt").

END. /* ChangePageText */

PROCEDURE pdf_fill_text:
  DEFINE INPUT PARAMETER pdfStream    AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfFill      AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfText      AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfOptions   AS CHARACTER NO-UNDO.

  IF NOT CAN-FIND(FIRST TT_pdf_stream
                  WHERE TT_pdf_stream.obj_stream = pdfStream NO-LOCK)
  THEN DO:
    RUN pdf_error(pdfStream,"pdf_Fill_text","Cannot find Stream!").
    RETURN ERROR.
  END.

  CREATE TT_pdf_FillTxt.
  ASSIGN TT_pdf_FillTxt.obj_stream   = pdfStream.
  ASSIGN TT_pdf_FillTxt.page_nbr     = pdf_Page(pdfStream)
         TT_pdf_FillTxt.fill_from    = pdfFill
         TT_pdf_FillTxt.fill_to      = pdfText
         TT_pdf_FillTxt.fill_options = pdfOptions.

END. /* pdf_fill_text */

PROCEDURE ProcessFillText: /* PRIVATE */
  DEFINE INPUT PARAMETER pdfStream  AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfPage    AS INTEGER NO-UNDO.

  DEFINE VARIABLE L_Text    AS CHARACTER NO-UNDO.
  DEFINE VARIABLE L_Align   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE L_Font    AS CHARACTER NO-UNDO.
  DEFINE VARIABLE L_Disp    AS CHARACTER NO-UNDO.

  DEFINE VARIABLE L_FontSize  AS DECIMAL DECIMALS 2 NO-UNDO.
  DEFINE VARIABLE L_RectWidth AS DECIMAL DECIMALS 5 NO-UNDO.
  DEFINE VARIABLE L_Width     AS DECIMAL DECIMALS 5 NO-UNDO.

  DEFINE VARIABLE L_FoundWidget AS LOGICAL NO-UNDO.
  DEFINE VARIABLE L_MultiLine   AS LOGICAL NO-UNDO.

  DEFINE BUFFER B_TT_stream FOR TT_pdf_stream.

  FIND FIRST B_TT_Stream WHERE B_TT_Stream.obj_Stream = pdfStream 
       NO-ERROR.
  IF NOT AVAIL B_TT_Stream THEN RETURN.

  ASSIGN B_TT_Stream.obj_DoingText    = FALSE
         B_TT_Stream.obj_DoingGraphic = FALSE.

  /* Find the current Page record */
  FIND FIRST TT_pdf_page 
       WHERE TT_pdf_page.obj_Stream = pdfStream 
         AND TT_pdf_page.page_nbr   = pdfPage NO-LOCK NO-ERROR.
  IF NOT AVAIL TT_pdf_page THEN RETURN.

  OUTPUT STREAM S_pdf_out
         TO VALUE( SESSION:TEMP-DIR 
                   + B_TT_stream.obj_UniqueID  
                   + "-Content-" 
                   + STRING(pdfPage) + ".txt") APPEND.

    FOR EACH TT_pdf_FillTxt 
       WHERE TT_pdf_FillTxt.obj_Stream = pdfStream
         AND TT_pdf_FillTxt.page_nbr   = pdfPage:


      FOR EACH TT_Widget 
         WHERE TT_Widget.obj_stream  = pdfStream
           AND TT_Widget.widget_page = TT_pdf_page.page_use
           AND TT_Widget.widget_name = TT_pdf_FillTxt.fill_from
           AND TT_Widget.widget_rect <> "":

        FIND FIRST TT_Object 
             WHERE TT_Object.obj_stream = pdfStream
               AND TT_Object.pdf_ID     = TT_Widget.pdf_id
               AND TT_Object.obj_type   = "Page"
               AND TT_Object.page_id    = TT_Widget.widget_page NO-ERROR.
        
        IF TT_Widget.widget_disp <> "" THEN DO:
          L_Disp = TT_Widget.widget_disp.
          FOR EACH TT_Font WHERE TT_Font.obj_Stream = pdfStream:
            IF INDEX(L_Disp,"~/" + TT_Font.font_name + " ") > 0 THEN DO:
              L_Font = TT_Font.font_name.
              L_disp = REPLACE(L_Disp,
                                "~/" + TT_Font.font_name + " ",
                                TT_Font.font_tag + " ").
            END.

            IF INDEX(L_Disp," 0 Tf") > 0 THEN DO:
              L_disp = REPLACE(L_disp,
                               " 0 Tf",
                               " " + STRING(pdf_PointSize(pdfStream))
                               + " Tf").
              L_FontSize = pdf_PointSize(pdfStream).
            END.

            ELSE IF INDEX(L_Disp," Tf") > 0 THEN DO:
              L_FontSize = DEC(ENTRY(LOOKUP("Tf",L_Disp," ") - 1,L_Disp," ")).
            END.

            ELSE
              L_FontSize = pdf_PointSize(pdfStream).
          END.
        END.

        L_Text = TT_pdf_FillTxt.Fill_to.
        RUN pdf_replace_text (INPUT-OUTPUT L_Text).

        IF INDEX(L_Text,"@@TotalPages-" + pdfStream) > 0 THEN
          L_Text = REPLACE(L_Text,"@@TotalPages-" + pdfStream,STRING(pdf_Page(pdfStream))).

        ASSIGN L_Align     = ""
               L_MultiLine = FALSE
               L_Width     = 0
               L_RectWidth = 0.

        IF TT_pdf_FillTxt.fill_option <> "" THEN DO:          
          IF INDEX(TT_pdf_FillTxt.fill_options,"align=") > 0 THEN
            L_Align = GetWidgetOption("align",TT_pdf_fillTxt.fill_options).

          IF INDEX(TT_pdf_FillTxt.fill_options,"multiline") > 0 THEN
            ASSIGN L_MultiLine = TRUE
                   L_Align     = "LEFT".
        END. /* Options */
        
        CASE L_Align:
          WHEN "Right" THEN DO:
            IF L_Font <> "" THEN
              L_Width = pdf_text_fontwidth2(pdfStream, L_Font, L_FontSize, L_Text).
            ELSE
              L_Width = pdf_text_fontwidth2(pdfStream, "Courier", L_FontSize, L_Text).

            IF L_Width = 0 THEN
              L_Width = pdf_text_fontwidth2(pdfStream, "Courier", L_FontSize, L_Text).

            IF TT_Object.Rotate = 90 THEN 
              RUN OutputTextContent(pdfStream, 
                                    "TEXTXY",
                                    L_disp + CHR(10)
                                    + "1 0 0 1 " 
                                    + dec2string(DEC(ENTRY(4,TT_Widget.widget_rect," ")) - L_Width) + " "
                                    + dec2string(DEC(pdf_PageHeight(pdfStream)) - DEC(ENTRY(3, TT_Widget.widget_rect," ")) ) + " Tm" + CHR(10) 
                                    + "(",
                                    L_Text,
                                    ") Tj").

            ELSE
              RUN OutputTextContent(pdfStream, 
                                    "TEXTXY",
                                    L_disp + CHR(10)
                                    + "1 0 0 1 " 
                                    + dec2string(DEC(ENTRY(3,TT_Widget.widget_rect," ")) - L_Width) + " "
                                    + ENTRY(2,TT_Widget.widget_rect," ") + " Tm" + CHR(10)
                                    + "(",
                                    L_Text,
                                    ") Tj").

          END. /* Right */

          WHEN "Center" THEN DO:

            L_RectWidth = DEC(ENTRY(3,TT_Widget.widget_rect," "))
                        - DEC(ENTRY(1,TT_Widget.widget_rect," ")).
            L_RectWidth = L_RectWidth / 2.

            IF L_Font <> "" THEN
              L_Width = pdf_text_fontwidth2(pdfStream, L_Font, L_FontSize, L_Text).
            ELSE
              L_Width = pdf_text_fontwidth2(pdfStream, "Courier", L_FontSize, L_Text).

            IF L_Width = 0 THEN
              L_Width = pdf_text_fontwidth2(pdfStream, "Courier", L_FontSize, L_Text).

            L_Width = L_Width / 2.

            IF TT_Object.Rotate = 90 THEN 
              RUN OutputTextContent(pdfStream, 
                                    "TEXTXY",
                                    L_disp + CHR(10)
                                    + "1 0 0 1 " 
                                    + dec2string(DEC(ENTRY(2,TT_Widget.widget_rect," ")) + L_RectWidth) + " "
                                    + dec2string(DEC(pdf_PageHeight(pdfStream)) - DEC(ENTRY(3, TT_Widget.widget_rect," ")) ) + " Tm" + CHR(10) 
                                    + "(",
                                    L_Text,
                                    ") Tj").
            ELSE
              RUN OutputTextContent(pdfStream, 
                                    "TEXTXY",
                                    L_disp + CHR(10)
                                    + "1 0 0 1 " +  dec2string(DEC(ENTRY(1,TT_Widget.widget_rect," ")) + L_RectWidth - L_Width) + " "
                                    + ENTRY(2,TT_Widget.widget_rect," ") + " Tm" + CHR(10)
                                    + "(",
                                    L_Text,
                                    ") Tj" ).
          END. /* Center */

          OTHERWISE DO:   /* Assume Left */
            IF NOT L_MultiLine THEN DO:
      
              IF TT_Object.Rotate = 90 THEN 
                RUN OutputTextContent(pdfStream, 
                                      "TEXTXY",
                                      L_disp + CHR(10)
                                      + "1 0 0 1 " 
                                      + ENTRY(4,TT_Widget.widget_rect," ") + " "
                                      + dec2string(DEC(pdf_PageHeight(pdfStream)) - DEC(ENTRY(3, TT_Widget.widget_rect," ")) ) + " Tm" + CHR(10) 
                                      + "(",
                                      L_Text,
                                      ") Tj").

              ELSE
                RUN OutputTextContent(pdfStream, 
                                      "TEXTXY",
                                      L_disp + CHR(10)
                                      + "1 0 0 1 " 
                                      + ENTRY(1,TT_Widget.widget_rect," ") + " "
                                      + ENTRY(2,TT_Widget.widget_rect," ") + " Tm" + CHR(10)
                                      + "(",
                                      L_Text,
                                      ") Tj").
            END.
            ELSE
              RUN pdf_fill_multiline
                  (pdfStream,
                  L_Text,
                  L_Disp,
                  DEC(ENTRY(1,TT_Widget.widget_rect," ")),
                  DEC(ENTRY(2,TT_Widget.widget_rect," ")),
                  DEC(ENTRY(3,TT_Widget.widget_rect," ")),
                  DEC(ENTRY(4,TT_Widget.widget_rect," ")) ).

          END. /* Otherwise Assume Left */
        END CASE.

        L_FoundWidget = TRUE.
      END. /* Each widget */
    END.

    IF L_FoundWidget THEN
      PUT STREAM S_pdf_out
          {&pdfSKIP}
          "ET"
          {&pdfSKIP}.
  OUTPUT STREAM S_pdf_out CLOSE.

  ASSIGN B_TT_Stream.obj_DoingText    = FALSE
         B_TT_Stream.obj_DoingGraphic = FALSE.

END. /* ProcessFillText */

PROCEDURE pdf_fill_MultiLine:  /* PRIVATE */
  DEFINE INPUT PARAMETER pdfStream  AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfText    AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfDisplay AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pdfLX      AS DECIMAL DECIMALS 5 NO-UNDO.
  DEFINE INPUT PARAMETER pdfLY      AS DECIMAL DECIMALS 5 NO-UNDO.
  DEFINE INPUT PARAMETER pdfUX      AS DECIMAL DECIMALS 5 NO-UNDO.
  DEFINE INPUT PARAMETER pdfUY      AS DECIMAL DECIMALS 5 NO-UNDO.

  DEFINE VARIABLE L_Font      AS CHARACTER NO-UNDO.
  DEFINE VARIABLE L_Line      AS CHARACTER NO-UNDO.
  DEFINE VARIABLE L_Word      AS CHARACTER NO-UNDO.

  DEFINE VARIABLE L_Loop      AS INTEGER NO-UNDO.

  DEFINE VARIABLE L_FontSize  AS DECIMAL NO-UNDO.
  DEFINE VARIABLE L_LineWidth AS DECIMAL DECIMALS 5 NO-UNDO.
  DEFINE VARIABLE L_MaxWidth  AS DECIMAL DECIMALS 5 NO-UNDO.

  L_MaxWidth = pdfUX - pdfLX.

  IF LOOKUP("Tf",pdfDisplay," ") > 0 THEN 
    ASSIGN L_FontSize = DEC(ENTRY(LOOKUP("Tf",pdfDisplay," ") - 1, pdfDisplay, " "))
           L_Font     = ENTRY(LOOKUP("Tf",pdfDisplay," ") - 2, pdfDisplay, " ").
  ELSE
    ASSIGN L_FontSize = pdf_PointSize(pdfStream)
           L_Font     = pdf_Font(pdfStream).

  /* Line Feeds */
  ASSIGN pdfText = REPLACE(pdfText,CHR(10)," &skip; ")
         pdfText = REPLACE(pdfText,CHR(13),"").

  pdfUY = pdfUY - L_FontSize.

  LINE-LOOP:
  DO L_Loop = 1 TO NUM-ENTRIES(pdfText," "):
    L_Word = ENTRY(L_Loop,pdfText," ").
    
    
    IF L_Word = "&skip;" THEN DO:
      RUN OutputTextContent(pdfStream, 
                            "TEXTXY",
                            pdfDisplay + CHR(10)
                            + "1 0 0 1 " 
                            + STRING(pdfLX) + " "
                            + STRING(pdfUY) + " Tm" + CHR(10)
                            + "(",
                            TRIM(L_Line),
                            ") Tj").

      pdfUY = pdfUY - L_FontSize.
      L_Line = "".

      IF pdfUY < pdfLY THEN
        LEAVE LINE-LOOP.
      ELSE
        NEXT LINE-LOOP.
    END.

    L_LineWidth = pdf_text_widthdec2(pdfStream,
                                     L_Font,
                                     L_FontSize, 
                                     L_Line + L_Word).
    
    /* If the current line plus the new word is greater than */
    IF L_LineWidth > L_MaxWidth THEN DO:
      RUN OutputTextContent(pdfStream, 
                            "TEXTXY",
                            pdfDisplay + CHR(10)
                            + "1 0 0 1 " 
                            + STRING(pdfLX) + " "
                            + STRING(pdfUY) + " Tm" + CHR(10)
                            + "(",
                            TRIM(L_Line),
                            ") Tj").
      pdfUY = pdfUY - L_FontSize.
      IF pdfUY < pdfLY THEN DO:
        L_Line = "".
        LEAVE LINE-LOOP.
      END.
      ELSE DO:
        L_Line = l_Word.
        NEXT LINE-LOOP.
      END.
    END.
    
    L_Line = L_Line + " " + L_Word.

  END. /* end of Loop */

  IF L_Line <> "" THEN
    RUN OutputTextContent(pdfStream, 
                          "TEXTXY",
                          pdfDisplay + CHR(10)
                          + "1 0 0 1 " 
                          + STRING(pdfLX) + " "
                          + STRING(pdfUY) + " Tm" + CHR(10)
                          + "(",
                          TRIM(L_Line),
                          ") Tj").

END. /* pdf_Fill_Multiline */

/* Encryption Routines */
FUNCTION int2hex RETURNS CHARACTER
        ( vi AS INTEGER ):
  DEFINE VARIABLE hexBit AS CHARACTER FORMAT "x(1)" EXTENT 16 NO-UNDO INIT
        ['0','1','2','3','4','5','6','7','8','9','a','b','c','d','e','f'].

  IF vi < 16 THEN RETURN hexbit[vi + 1].

  RETURN int2hex( integer( TRUNCATE( vi / 16, 0 ) ) )
         + hexbit[ ( vi MODULO 16 ) + 1 ] . 
END FUNCTION. 

FUNCTION int2hexchar RETURNS CHARACTER
        ( vi AS INTEGER ):
  DEFINE VARIABLE chex AS CHARACTER NO-UNDO.
  chex = int2hex( vi ).
  RETURN '0x' + FILL( '0', 8 - LENGTH( chex ) ) + chex. 
END FUNCTION. 

PROCEDURE LoadHexArray:
  DEFINE VARIABLE vHexLoop AS INTEGER NO-UNDO.

  DO vHexLoop = 0 TO 255:
    CREATE HexArray.
    ASSIGN HexArray.hex-val = hex2(vHexLoop)
           HexArray.chr-val = vHexLoop.
  END.

END. /* LoadHexArray */

PROCEDURE md5Hash:
  DEFINE INPUT  PARAMETER pUniqueID AS CHARACTER NO-UNDO.
  DEFINE INPUT  PARAMETER pHashMem  AS MEMPTR NO-UNDO.
  DEFINE OUTPUT PARAMETER pHash     AS CHARACTER NO-UNDO.

  DEFINE VARIABLE cTxtFile   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cMD5File   AS CHARACTER NO-UNDO.

  ASSIGN cTxtFile = SESSION:TEMP-DIR + pUniqueID + "-O.txt"
         cMD5File = SESSION:TEMP-DIR + pUniqueID + "-O.md5".

  OUTPUT TO VALUE(cTxtFile).
    EXPORT pHashMem.
  OUTPUT CLOSE.

  IF OPSYS = "UNIX" THEN DO:
    INPUT THROUGH VALUE("{&MD5LIB} " + cTxtFile) NO-ECHO.
      IMPORT pHash.
    INPUT CLOSE.
  END.

  ELSE DO:
    OS-COMMAND SILENT VALUE("{&MD5LIB} " + cTxtFile + " > " + cMD5File) NO-ECHO.

    INPUT FROM VALUE(cMD5file) NO-ECHO NO-CONVERT.
      IMPORT pHash.
    INPUT CLOSE.

    OS-DELETE VALUE(cMD5File).
  END.

  OS-DELETE VALUE(cTxtFile).

END. /* md5Hash */

PROCEDURE DetermineOwnerKey:

  DEFINE INPUT        PARAMETER pdfStream       AS CHARACTER NO-UNDO.
  DEFINE INPUT        PARAMETER p_UniqueID      AS CHARACTER NO-UNDO.
  DEFINE INPUT        PARAMETER p_UserPassword  AS CHARACTER NO-UNDO.
  DEFINE INPUT-OUTPUT PARAMETER p_OwnerPassword AS CHARACTER NO-UNDO.

  DEFINE VARIABLE L_md5     AS INTEGER NO-UNDO.
  DEFINE VARIABLE L_Loop    AS INTEGER NO-UNDO.
  DEFINE VARIABLE L_Loop2   AS INTEGER NO-UNDO.
  DEFINE VARIABLE L_Length  AS INTEGER NO-UNDO.

  DEFINE VARIABLE L_Byte      AS INTEGER NO-UNDO.
  DEFINE VARIABLE L_md5mem    AS MEMPTR  NO-UNDO.

  DEFINE VARIABLE L_md5hash   AS CHARACTER NO-UNDO.

  RUN InitString (p_OwnerPassword, INPUT-OUTPUT vowner-str).
  RUN InitString (p_UserPassword, INPUT-OUTPUT vuser-str).

  /* Use Algorithm 3.3 from the Adobe 1.4 Spec */

  /* Step 1 - Pad the Owner String to 32-bytes */
  IF LENGTH(p_OwnerPassword) < 32 THEN
    RUN PadString (LENGTH(p_OwnerPassword),
                   INPUT-OUTPUT vOwner-Str).

  /* Step 2 - pass the padded Owner Password String to md5 Encryption hash */
  RUN md5Hash(INPUT  p_UniqueID,
              INPUT  vOwner-str,
              OUTPUT L_md5hash).

  /* Step 3 - only for Rev 3 encryption - do md5 hash another 50 times*/
  IF pdf_get_parameter(pdfStream,"EncryptKey") = "128" THEN DO:

    SET-SIZE(l_md5mem) = 0.
    DO l_Loop = 1 TO 50:
      SET-SIZE(l_md5mem) = 16.

      /* Build Byte value */
      L_Byte = 1.
      DO L_Loop2 = 1 TO 32 BY 2:
        FIND FIRST HexArray WHERE HexArray.Hex-Val = SUBSTR(L_md5hash,L_Loop2,2).
        IF HexArray.chr-val = 0 THEN
          PUT-BYTE(l_md5mem,L_Byte) = 0.
        ELSE
          PUT-BYTE(l_md5mem,L_Byte) = ASC(CHR(HexArray.chr-val)).

        L_Byte = L_byte + 1.
      END.

      RUN md5Hash(INPUT  p_UniqueID,
                  INPUT  l_MD5mem,
                  OUTPUT L_md5hash).

      SET-SIZE(l_md5mem) = 0.
    END. /* Loop 50 */

  END. /* 128 Bit encryption */

  /* Step 4 - create an RC4 Encryption key using the first n bytes of the
              MD5 hash obtained in Step 2. */
  L_Length = IF pdf_get_parameter(pdfStream,"EncryptKey") = "40" THEN 5 ELSE 16.
  SET-SIZE(vrc4-key)  = 0.
  SET-SIZE(vrc4-key)  = L_Length.
  L_Byte = 1.
  /* Since the md5hash is in hex we need to convert to Binary */
  DO L_md5 = 1 TO (L_Length * 2) BY 2:
    FIND FIRST HexArray WHERE HexArray.Hex-Val = SUBSTR(L_md5hash,L_Md5,2).
    IF HexArray.chr-val = 0 THEN
      PUT-BYTE(vrc4-key,L_Byte) = 0.
    ELSE
      PUT-BYTE(vrc4-key,L_Byte) = ASC(CHR(HexArray.chr-val)).
    L_Byte = L_Byte + 1.
  END.

  /* Step 5 - pad or truncate the User Password */
  IF LENGTH(p_UserPassword) < 32 THEN
    RUN PadString (LENGTH(p_UserPassword),
                   INPUT-OUTPUT vUser-Str).

  /* Step 6 - Encrypt the result of Step 5 using RC4 Key obtained in Step 4 */
  PDFendecrypt(vuser-str, vrc4-key).

  /* Step 7 - Perform this 19 times */
  IF l_Length = 16 THEN DO:  /* Len = 16 equals 128 Bit encryption */
    DO L_Loop = 1 TO 19:
      SET-SIZE(vrc4-128) = 16.
 
      DO l_Loop2 = 1 TO 16:
        PUT-BYTE(vrc4-128,l_Loop2) = BinaryXOR( GET-BYTE(vrc4-key,L_Loop2), l_Loop).
      END.

      PDFendecrypt(vuser-str, vrc4-128).

      SET-SIZE(vrc4-128) = 0.
    END.
  END.

  SET-SIZE(m_O) = GET-SIZE(vuser-str).
  m_O = vuser-str.
  SET-SIZE(vuser-str) = 0.

  p_OwnerPassword = "".
  DO L_loop = 1 TO GET-SIZE(m_O):
    IF ASC(CHR(GET-BYTE(m_O,L_loop))) = -1 THEN
      p_OwnerPassword = p_OwnerPassword + "00".
    ELSE
      p_OwnerPassword = p_OwnerPassword + hex2(ASC(CHR(GET-BYTE(m_O,L_loop)))).
  END.

END. /* DetermineOwnerKey */

PROCEDURE DetermineUserKey40:

  DEFINE INPUT  PARAMETER       p_UniqueID      AS CHARACTER NO-UNDO.
  DEFINE INPUT  PARAMETER       p_ID            AS CHARACTER NO-UNDO.
  DEFINE INPUT  PARAMETER       p_AllowPrint    AS INTEGER   NO-UNDO.
  DEFINE INPUT  PARAMETER       p_AllowCopy     AS INTEGER   NO-UNDO.
  DEFINE INPUT  PARAMETER       p_AllowModify   AS INTEGER   NO-UNDO.
  DEFINE INPUT  PARAMETER       p_AllowAnnots   AS INTEGER   NO-UNDO.
  DEFINE INPUT  PARAMETER       p_AllowForms    AS INTEGER   NO-UNDO.
  DEFINE INPUT  PARAMETER       p_AllowExtract  AS INTEGER   NO-UNDO.
  DEFINE INPUT  PARAMETER       p_AllowAssembly AS INTEGER   NO-UNDO.
  DEFINE OUTPUT PARAMETER       p_OrigKey       AS MEMPTR    NO-UNDO.
  DEFINE INPUT-OUTPUT PARAMETER p_UserPassword  AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER       p_IntID         AS INTEGER   NO-UNDO.

  DEFINE VARIABLE vLast-hash  AS CHARACTER NO-UNDO.

  DEFINE VARIABLE L_md5     AS INTEGER NO-UNDO.
  DEFINE VARIABLE L_Byte    AS INTEGER NO-UNDO.
  DEFINE VARIABLE L_Loop    AS INTEGER NO-UNDO.
  DEFINE VARIABLE vP-str    AS INTEGER NO-UNDO.

  DEFINE VARIABLE vPad-mem  AS MEMPTR  NO-UNDO.
  DEFINE VARIABLE vP-mem    AS MEMPTR  NO-UNDO.
  DEFINE VARIABLE vID-mem   AS MEMPTR  NO-UNDO.
  DEFINE VARIABLE vTot-mem  AS MEMPTR  NO-UNDO.

  /* Step 1 - Create an Encryption Key based on Algorithm 3.2 */
  
  /* Step 1A - Pad the User Password */
  RUN InitString (p_UserPassword, INPUT-OUTPUT vuser-str).

  IF LENGTH(p_UserPassword) < 32 THEN
    RUN PadString (LENGTH(p_UserPassword),
                   INPUT-OUTPUT vUser-Str).

  /* Step 1B - Pass concatenation of:
                 - Padded User Password 
                 - the value of the /O value
                 - the value of the /P entry 
                 - the first element of the /ID entry
               to the md5 hash function */

  SET-SIZE(vID-mem) = 16.    /* Set ID Memptr from /ID Hex string passed */
  L_Byte = 1.
  DO L_Loop = 1 TO 32 BY 2:
    FIND FIRST  HexArray WHERE HexArray.Hex-Val = SUBSTR(p_ID,L_loop,2).
    IF HexArray.chr-val = 0 THEN
      PUT-BYTE(vID-mem,L_Byte) = 0.
    ELSE
      PUT-BYTE(vID-mem,L_Byte) = ASC(CHR(HexArray.chr-val)).
    L_Byte = L_Byte + 1.
  END.
  
  /* If non Rev 3 (or 128 Bit Encryption) then the following must be 1 since 
     they are not valid in Rev 2 Encryption */
  ASSIGN p_AllowForms    = 1
         p_AllowExtract  = 1
         p_AllowAssembly = 1.

  PUT-BITS(vP-Str,1,1)   = 0.   /* Reserved */
  PUT-BITS(vP-Str,2,1)   = 0.   /* Reserved */
  PUT-BITS(vP-Str,3,1)   = p_AllowPrint.      /* Print the Document */
  PUT-BITS(vP-Str,4,1)   = p_AllowModify.     /* Modify the Contents */
  PUT-BITS(vP-Str,5,1)   = p_AllowCopy.       /* Copy Graphics or Text */
  PUT-BITS(vP-Str,6,1)   = p_AllowAnnots.     /* Add or modify Annotations */
  PUT-BITS(vP-Str,7,1)   = 1.                 /* Reserved - must be 1 */
  PUT-BITS(vP-Str,8,1)   = 1.                 /* Reserved - must be 1 */
  PUT-BITS(vP-Str,9,1)   = p_AllowForms.      /* Fill in existing Form Fields */
  PUT-BITS(vP-Str,10,1)  = p_AllowExtract.    /* Extract text and Graphics - Rev 3 */
  PUT-BITS(vP-Str,11,1)  = p_AllowAssembly.   /* Assemble the document - Rev 3 */
  PUT-BITS(vP-Str,12,1)  = 1.   /* Print the doc - Rev 3 */
  PUT-BITS(vP-Str,13,1)  = 1.   /* Reserved */
  PUT-BITS(vP-Str,14,1)  = 1.   /* Reserved */
  PUT-BITS(vP-Str,15,1)  = 1.   /* Reserved */
  PUT-BITS(vP-Str,16,1)  = 1.   /* Reserved */
  PUT-BITS(vP-Str,17,1)  = 1.   /* Reserved */
  PUT-BITS(vP-Str,18,1)  = 1.   /* Reserved */
  PUT-BITS(vP-Str,19,1)  = 1.   /* Reserved */
  PUT-BITS(vP-Str,20,1)  = 1.   /* Reserved */
  PUT-BITS(vP-Str,21,1)  = 1.   /* Reserved */
  PUT-BITS(vP-Str,22,1)  = 1.   /* Reserved */
  PUT-BITS(vP-Str,23,1)  = 1.   /* Reserved */
  PUT-BITS(vP-Str,24,1)  = 1.   /* Reserved */
  PUT-BITS(vP-Str,25,1)  = 1.   /* Reserved */
  PUT-BITS(vP-Str,26,1)  = 1.   /* Reserved */
  PUT-BITS(vP-Str,27,1)  = 1.   /* Reserved */
  PUT-BITS(vP-Str,28,1)  = 1.   /* Reserved */
  PUT-BITS(vP-Str,29,1)  = 1.   /* Reserved */
  PUT-BITS(vP-Str,30,1)  = 1.   /* Reserved */
  PUT-BITS(vP-Str,31,1)  = 1.   /* Reserved */
  PUT-BITS(vP-Str,32,1)  = 1.   /* Reserved */
  
  /* Return the value so we can set it in the Encryption Dictionary */
  p_IntID = Vp-Str. 

  SET-SIZE(vP-Mem) = 4.
  PUT-BYTE(vP-Mem,1) = GET-BITS(vP-str,1,8).
  PUT-BYTE(vP-Mem,2) = GET-BITS(vP-str,9,8).
  PUT-BYTE(vP-Mem,3) = GET-BITS(vP-str,17,8).
  PUT-BYTE(vP-Mem,4) = GET-BITS(vP-str,25,8).


  SET-SIZE(vTot-mem) = GET-SIZE(vuser-str) + GET-SIZE(m_O)
                     + GET-SIZE(vP-mem) + GET-SIZE(vID-mem).

  /* Add Padded User Password string to Total String */
  L_Byte = 1.
  DO L_Loop = 1 TO GET-SIZE(vuser-str):
    IF ASC(CHR(GET-BYTE(vuser-str,L_loop))) = -1 THEN DO:
      PUT-BYTE(vTot-mem,L_Byte) = 0.
    END.

    ELSE DO:
      FIND FIRST HexArray WHERE HexArray.Hex-Val = hex2(ASC(CHR(GET-BYTE(vuser-str,L_loop)))).
      PUT-BYTE(vTot-mem,L_Byte) = ASC(CHR(HexArray.chr-val)).
    END.

    L_Byte = L_byte + 1.
  END.

  /* Add /O entry to Total String */
  DO L_Loop = 1 TO GET-SIZE(m_O):
    IF ASC(CHR(GET-BYTE(m_O,L_loop))) = -1 THEN DO:
      PUT-BYTE(vTot-mem,L_Byte) = 0.
    END.

    ELSE DO:
      FIND FIRST HexArray WHERE HexArray.Hex-Val = hex2(ASC(CHR(GET-BYTE(m_O,L_loop)))).
      PUT-BYTE(vTot-mem,L_Byte) = ASC(CHR(HexArray.chr-val)).
    END.

    L_Byte = L_byte + 1.
  END.

  /* add the /P entry to Total String - low-order byte first */
  DO L_Loop = 1 TO GET-SIZE(vP-mem):
    IF ASC(CHR(GET-BYTE(vP-mem,L_loop))) = -1 THEN DO:
      PUT-BYTE(vTot-mem,L_Byte) = 0.
    END.
    ELSE DO:
      FIND FIRST HexArray WHERE HexArray.Hex-Val = hex2(ASC(CHR(GET-BYTE(vP-mem,L_loop)))).
      PUT-BYTE(vTot-mem,L_Byte) = ASC(CHR(HexArray.chr-val)).
    END.

    L_Byte = L_byte + 1.
  END.

  /* Add the /ID entry to Total String */
  DO L_Loop = 1 TO GET-SIZE(vID-mem):
    IF ASC(CHR(GET-BYTE(vID-mem,L_loop))) = -1 THEN DO:
      PUT-BYTE(vTot-mem,L_Byte) = 0.
    END.

    ELSE DO:
      FIND FIRST HexArray WHERE HexArray.Hex-Val = hex2(ASC(CHR(GET-BYTE(vID-mem,L_loop)))).
      PUT-BYTE(vTot-mem,L_Byte) = ASC(CHR(HexArray.chr-val)).
    END.

    L_Byte = L_byte + 1.
  END.
  
  RUN md5Hash(INPUT  p_UniqueID,
              INPUT  vTot-mem,
              OUTPUT vlast-hash).

  SET-SIZE(vrc4-key) = 0.
  SET-SIZE(Vrc4-key) = 5.
  L_Byte = 1.
  DO L_md5 = 1 TO 10 BY 2:
    IF SUBSTR(vlast-hash, L_md5,2) = "00" THEN DO:
      PUT-BYTE(vrc4-key,L_Byte) = 0.
    END.
    ELSE DO:
      FIND FIRST HexArray WHERE HexArray.Hex-Val = SUBSTR(vlast-hash,L_md5,2).
      PUT-BYTE(vrc4-key,L_Byte) = ASC(CHR(HexArray.chr-val)).
    END.
    L_Byte = L_Byte + 1.
  END.

  /* Now that we've got the original encryption key - store it for future use */
  SET-SIZE(p_OrigKey) = 5.
  p_OrigKey = vrc4-key.

  RUN PadString (0,
                 INPUT-OUTPUT vPad-mem).

  /* Plus run the encryption routine on the padded User password with this key */
  PDFendecrypt(vpad-mem, vrc4-key).
  
  /* But since we are outputting in hex we need to convert back to hex values */
  p_UserPassword = "".
  DO L_loop = 1 TO GET-SIZE(vpad-mem):
    IF ASC(CHR(GET-BYTE(vpad-mem,L_loop))) = -1 THEN
      p_UserPassword = p_UserPassword + "00".
    ELSE
      p_UserPassword = p_UserPassword + hex2(ASC(CHR(GET-BYTE(vpad-mem,L_loop)))).
  END.

END. /* DetermineUserKey40 */

PROCEDURE DetermineUserKey128:

  DEFINE INPUT  PARAMETER       p_UniqueID      AS CHARACTER NO-UNDO.
  DEFINE INPUT  PARAMETER       p_ID            AS CHARACTER NO-UNDO.
  DEFINE INPUT  PARAMETER       p_AllowPrint    AS INTEGER   NO-UNDO.
  DEFINE INPUT  PARAMETER       p_AllowCopy     AS INTEGER   NO-UNDO.
  DEFINE INPUT  PARAMETER       p_AllowModify   AS INTEGER   NO-UNDO.
  DEFINE INPUT  PARAMETER       p_AllowAnnots   AS INTEGER   NO-UNDO.
  DEFINE INPUT  PARAMETER       p_AllowForms    AS INTEGER   NO-UNDO.
  DEFINE INPUT  PARAMETER       p_AllowExtract  AS INTEGER   NO-UNDO.
  DEFINE INPUT  PARAMETER       p_AllowAssembly AS INTEGER   NO-UNDO.
  DEFINE OUTPUT PARAMETER       p_OrigKey       AS MEMPTR    NO-UNDO.
  DEFINE INPUT-OUTPUT PARAMETER p_UserPassword  AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER       p_IntID         AS INTEGER   NO-UNDO.

  DEFINE VARIABLE vLast-hash  AS CHARACTER NO-UNDO.

  DEFINE VARIABLE L_md5     AS INTEGER NO-UNDO.
  DEFINE VARIABLE L_Byte    AS INTEGER NO-UNDO.
  DEFINE VARIABLE L_Loop    AS INTEGER NO-UNDO.
  DEFINE VARIABLE L_Loop2   AS INTEGER NO-UNDO.
  DEFINE VARIABLE vP-str    AS INTEGER NO-UNDO.

  DEFINE VARIABLE vPad-mem  AS MEMPTR  NO-UNDO.
  DEFINE VARIABLE vP-mem    AS MEMPTR  NO-UNDO.
  DEFINE VARIABLE vID-mem   AS MEMPTR  NO-UNDO.
  DEFINE VARIABLE vTot-mem  AS MEMPTR  NO-UNDO.
  DEFINE VARIABLE v128-mem  AS MEMPTR  NO-UNDO.

  /* Step 1 - Create an Encryption Key based on Algorithm 3.2 */
  
  /* Step 1A - Pad the User Password */
  RUN InitString (p_UserPassword, INPUT-OUTPUT vuser-str).

  IF LENGTH(p_UserPassword) < 32 THEN
    RUN PadString (LENGTH(p_UserPassword),
                   INPUT-OUTPUT vUser-Str).

  /* Step 1B - Pass concatenation of:
                 - Padded User Password 
                 - the value of the /O value
                 - the value of the /P entry 
                 - the first element of the /ID entry
                   to the md5 hash function 
                 - 0xFFFFFFFF - no metadata therefore not encrypted */

  SET-SIZE(vID-mem) = 16.    /* Set ID Memptr from /ID Hex string passed */
  L_Byte = 1.
  DO L_Loop = 1 TO 32 BY 2:
    FIND FIRST  HexArray WHERE HexArray.Hex-Val = SUBSTR(p_ID,L_loop,2).
    IF HexArray.chr-val = 0 THEN
      PUT-BYTE(vID-mem,L_Byte) = 0.
    ELSE
      PUT-BYTE(vID-mem,L_Byte) = ASC(CHR(HexArray.chr-val)).
    L_Byte = L_Byte + 1.
  END.

  PUT-BITS(vP-Str,1,1)   = 0.   /* Reserved */
  PUT-BITS(vP-Str,2,1)   = 0.   /* Reserved */
  PUT-BITS(vP-Str,3,1)   = p_AllowPrint.      /* Print the Document */
  PUT-BITS(vP-Str,4,1)   = p_AllowModify.     /* Modify the Contents */
  PUT-BITS(vP-Str,5,1)   = p_AllowCopy.       /* Copy Graphics or Text */
  PUT-BITS(vP-Str,6,1)   = p_AllowAnnots.     /* Add or modify Annotations */
  PUT-BITS(vP-Str,7,1)   = 1.                 /* Reserved - must be 1 */
  PUT-BITS(vP-Str,8,1)   = 1.                 /* Reserved - must be 1 */
  PUT-BITS(vP-Str,9,1)   = p_AllowForms.      /* Fill in existing Form Fields */
  PUT-BITS(vP-Str,10,1)  = p_AllowExtract.    /* Extract text and Graphics - Rev 3 */
  PUT-BITS(vP-Str,11,1)  = p_AllowAssembly.   /* Assemble the document - Rev 3 */
  PUT-BITS(vP-Str,12,1)  = 1.   /* Print the doc - Rev 3 */
  PUT-BITS(vP-Str,13,1)  = 1.   /* Reserved */
  PUT-BITS(vP-Str,14,1)  = 1.   /* Reserved */
  PUT-BITS(vP-Str,15,1)  = 1.   /* Reserved */
  PUT-BITS(vP-Str,16,1)  = 1.   /* Reserved */
  PUT-BITS(vP-Str,17,1)  = 1.   /* Reserved */
  PUT-BITS(vP-Str,18,1)  = 1.   /* Reserved */
  PUT-BITS(vP-Str,19,1)  = 1.   /* Reserved */
  PUT-BITS(vP-Str,20,1)  = 1.   /* Reserved */
  PUT-BITS(vP-Str,21,1)  = 1.   /* Reserved */
  PUT-BITS(vP-Str,22,1)  = 1.   /* Reserved */
  PUT-BITS(vP-Str,23,1)  = 1.   /* Reserved */
  PUT-BITS(vP-Str,24,1)  = 1.   /* Reserved */
  PUT-BITS(vP-Str,25,1)  = 1.   /* Reserved */
  PUT-BITS(vP-Str,26,1)  = 1.   /* Reserved */
  PUT-BITS(vP-Str,27,1)  = 1.   /* Reserved */
  PUT-BITS(vP-Str,28,1)  = 1.   /* Reserved */
  PUT-BITS(vP-Str,29,1)  = 1.   /* Reserved */
  PUT-BITS(vP-Str,30,1)  = 1.   /* Reserved */
  PUT-BITS(vP-Str,31,1)  = 1.   /* Reserved */
  PUT-BITS(vP-Str,32,1)  = 1.   /* Reserved */
  
  /* Return the value so we can set it in the Encryption Dictionary */
  p_IntID = Vp-Str. 

  SET-SIZE(vP-Mem) = 4.
  PUT-BYTE(vP-Mem,1) = GET-BITS(vP-str,1,8).
  PUT-BYTE(vP-Mem,2) = GET-BITS(vP-str,9,8).
  PUT-BYTE(vP-Mem,3) = GET-BITS(vP-str,17,8).
  PUT-BYTE(vP-Mem,4) = GET-BITS(vP-str,25,8).


  SET-SIZE(vTot-mem) = GET-SIZE(vuser-str) + GET-SIZE(m_O)
                     + GET-SIZE(vP-mem) + GET-SIZE(vID-mem)
                     + 4.

  /* Add Padded User Password string to Total String */
  L_Byte = 1.
  DO L_Loop = 1 TO GET-SIZE(vuser-str):
    IF ASC(CHR(GET-BYTE(vuser-str,L_loop))) = -1 THEN DO:
      PUT-BYTE(vTot-mem,L_Byte) = 0.
    END.

    ELSE DO:
      FIND FIRST HexArray WHERE HexArray.Hex-Val = hex2(ASC(CHR(GET-BYTE(vuser-str,L_loop)))).
      PUT-BYTE(vTot-mem,L_Byte) = ASC(CHR(HexArray.chr-val)).
    END.

    L_Byte = L_byte + 1.
  END.

  /* Add /O entry to Total String */
  DO L_Loop = 1 TO GET-SIZE(m_O):
    IF ASC(CHR(GET-BYTE(m_O,L_loop))) = -1 THEN DO:
      PUT-BYTE(vTot-mem,L_Byte) = 0.
    END.

    ELSE DO:
      FIND FIRST HexArray WHERE HexArray.Hex-Val = hex2(ASC(CHR(GET-BYTE(m_O,L_loop)))).
      PUT-BYTE(vTot-mem,L_Byte) = ASC(CHR(HexArray.chr-val)).
    END.

    L_Byte = L_byte + 1.
  END.

  /* add the /P entry to Total String - low-order byte first */
  DO L_Loop = 1 TO GET-SIZE(vP-mem):
    IF ASC(CHR(GET-BYTE(vP-mem,L_loop))) = -1 THEN DO:
      PUT-BYTE(vTot-mem,L_Byte) = 0.
    END.
    ELSE DO:
      FIND FIRST HexArray WHERE HexArray.Hex-Val = hex2(ASC(CHR(GET-BYTE(vP-mem,L_loop)))).
      PUT-BYTE(vTot-mem,L_Byte) = ASC(CHR(HexArray.chr-val)).
    END.

    L_Byte = L_byte + 1.
  END.

  /* Add the /ID entry to Total String */
  DO L_Loop = 1 TO GET-SIZE(vID-mem):
    IF ASC(CHR(GET-BYTE(vID-mem,L_loop))) = -1 THEN DO:
      PUT-BYTE(vTot-mem,L_Byte) = 0.
    END.

    ELSE DO:
      FIND FIRST HexArray WHERE HexArray.Hex-Val = hex2(ASC(CHR(GET-BYTE(vID-mem,L_loop)))).
      PUT-BYTE(vTot-mem,L_Byte) = ASC(CHR(HexArray.chr-val)).
    END.

    L_Byte = L_byte + 1.
  END.

  /* Add 4 bytes with value 0xFFFFFFFF if metadata is not being encryted */
  DO l_Loop = 1 to 4:
    PUT-BYTE(vTot-mem,L_Byte) = 255.
    L_Byte = L_byte + 1.
  END.

  /* Step 1C - Hash the concatenated value */
  RUN md5Hash(INPUT  p_UniqueID,
              INPUT  vTot-Mem,
              OUTPUT vlast-hash).

  /* Step 1D - Now perform the hash 50 more times */
  SET-SIZE(vTot-mem) = 0.
  DO l_Loop = 1 TO 50:
    SET-SIZE(vTot-mem) = 16.

    L_Byte = 1.
    DO L_Loop2 = 1 TO 32 BY 2:
      FIND FIRST HexArray WHERE HexArray.Hex-Val = SUBSTR(vlast-hash,L_Loop2,2).
      IF HexArray.chr-val = 0 THEN
        PUT-BYTE(vTot-mem,L_Byte) = 0.
      ELSE
        PUT-BYTE(vTot-mem,L_Byte) = ASC(CHR(HexArray.chr-val)).

      L_Byte = L_byte + 1.
    END.

    RUN md5Hash(INPUT  p_UniqueID,
                INPUT  vTot-Mem,
                OUTPUT vlast-hash).

    SET-SIZE(vTot-mem) = 0.
  END. /* Loop 50 */

  /* Step 1E - Set the encryption Key to the first n (16) bytes of the final 
               md5hash */
  SET-SIZE(vrc4-key) = 0.
  SET-SIZE(Vrc4-key) = 16.
  L_Byte = 1.
  DO L_md5 = 1 TO 32 BY 2:
    IF SUBSTR(vlast-hash, L_md5,2) = "00" THEN DO:
      PUT-BYTE(vrc4-key,L_Byte) = 0.
    END.
    ELSE DO:
      FIND FIRST HexArray WHERE HexArray.Hex-Val = SUBSTR(vlast-hash,L_md5,2).
      IF HexArray.chr-val = 0 THEN
        PUT-BYTE(vrc4-key,L_Byte) = 0.
      ELSE
        PUT-BYTE(vrc4-key,L_Byte) = ASC(CHR(HexArray.chr-val)).
    END.
    L_Byte = L_Byte + 1.
  END.

  /* Now that we've got the original encryption key - store it for future use */
  SET-SIZE(p_OrigKey) = 16.
  p_OrigKey = vrc4-key.

  /* Step 2 0f Algorithm 3.5 */
  RUN PadString (0,
                 INPUT-OUTPUT vPad-mem).

  SET-SIZE(v128-mem) = 48.
  L_BYte = 1.
  DO L_Loop = 1 TO 32:
    PUT-BYTE(v128-mem,L_Byte) = GET-BYTE(vpad-mem,L_Byte).

    L_BYte = l_Byte + 1.
  END.

  DO L_Loop = 1 TO 16:
    PUT-BYTE(v128-mem,L_Byte) = GET-BYTE(vID-mem,L_Loop).

    L_Byte = l_Byte + 1.
  END.

  /* Step 3 0f Algorithm 3.5 */
  /* Now perform the Hash on the total bytes */
  RUN md5Hash(INPUT  p_UniqueID,
              INPUT  v128-mem,
              OUTPUT vlast-hash).

  SET-SIZE(vrc4-key) = 0.
  SET-SIZE(Vrc4-key) = 16.
  L_Byte = 1.
  DO L_md5 = 1 TO 32 BY 2:
    IF SUBSTR(vlast-hash, L_md5,2) = "00" THEN DO:
      PUT-BYTE(vrc4-key,L_Byte) = 0.
    END.
    ELSE DO:
      FIND FIRST HexArray WHERE HexArray.Hex-Val = SUBSTR(vlast-hash,L_md5,2).
      PUT-BYTE(vrc4-key,L_Byte) = ASC(CHR(HexArray.chr-val)).
    END.
    L_Byte = L_Byte + 1.
  END.

  /* Step 4 0f Algorithm 3.5 */
  /* run the encryption routine on the Hash Result using the Original key */
  PDFendecrypt(vrc4-key, p_OrigKey).  
  
  /* Step 5 0f Algorithm 3.5 */
  DO L_Loop = 1 TO 19:
    SET-SIZE(vrc4-128) = 16.
 
    DO l_Loop2 = 1 TO 16:
      PUT-BYTE(vrc4-128,l_Loop2) = BinaryXOR( GET-BYTE(p_OrigKey,L_Loop2), l_Loop).
    END.

    PDFendecrypt(vrc4-key, vrc4-128).

    SET-SIZE(vrc4-128) = 0.
  END.
  
  /* Step 6 0f Algorithm 3.5 */
  /* Append 16 Bytes of arbitrary padding */
  /* But since we are outputting in hex we need to convert back to hex values */
  p_UserPassword = "".
  DO L_loop = 1 TO GET-SIZE(vrc4-key):
    IF ASC(CHR(GET-BYTE(vrc4-key,L_loop))) = -1 THEN
      p_UserPassword = p_UserPassword + "00".
    ELSE
      p_UserPassword = p_UserPassword + hex2(ASC(CHR(GET-BYTE(vrc4-key,L_loop)))).
  END.

  /* Append 16 arbitrary bytes */
  DO L_Loop = 1 TO 16:
    P_UserPassWord = p_UserPassword + STRING(L_Loop,"99").
  END.

END. /* DetermineUserKey128 */

PROCEDURE InitString:
  DEFINE INPUT PARAMETER pString        AS CHARACTER NO-UNDO.
  DEFINE INPUT-OUTPUT PARAMETER pMemPtr AS MEMPTR NO-UNDO.

  DEFINE VARIABLE l_Loop    AS INTEGER NO-UNDO.

  /* No Password  */
  IF LENGTH(pString) > 0 AND LENGTH(pString) <= 32 THEN DO:
    SET-SIZE(pMemPtr) = 32.

    DO L_Loop = 1 TO LENGTH(pString):
      PUT-BYTE(pMemPtr, l_Loop) = ASC(SUBSTR(pString,l_Loop,1)).
    END.
  END.
END. /* InitString */

PROCEDURE PadString:
  DEFINE INPUT PARAMETER        pLength   AS INTEGER NO-UNDO.
  DEFINE INPUT-OUTPUT PARAMETER pMemPtr   AS MEMPTR NO-UNDO.

  DEFINE VARIABLE L_chr   AS MEMPTR NO-UNDO.
  DEFINE VARIABLE L_ctr   AS INTEGER NO-UNDO.
  DEFINE VARIABLE L_pad   AS INTEGER NO-UNDO.

  /* SET-SIZE(L_chr) = 1. */
  /* Set Password Pad String */ 
  SET-SIZE(L_chr) = 32.
  PUT-BYTE(L_Chr,1)  = ASC(CHR(40)).
  PUT-BYTE(L_Chr,2)  = ASC(CHR(191)).
  PUT-BYTE(L_Chr,3)  = ASC(CHR(78)).
  PUT-BYTE(L_Chr,4)  = ASC(CHR(94)).
  PUT-BYTE(L_Chr,5)  = ASC(CHR(78)).
  PUT-BYTE(L_Chr,6)  = ASC(CHR(117)).
  PUT-BYTE(L_Chr,7)  = ASC(CHR(138)).
  PUT-BYTE(L_Chr,8)  = ASC(CHR(65)).
  PUT-BYTE(L_Chr,9)  = ASC(CHR(100)).
  PUT-BYTE(L_Chr,10) = 0.
  PUT-BYTE(L_Chr,11) = ASC(CHR(78)).
  PUT-BYTE(L_Chr,12) = ASC(CHR(86)).
  PUT-BYTE(L_Chr,13) = ASC(CHR(255)).
  PUT-BYTE(L_Chr,14) = ASC(CHR(250)).
  PUT-BYTE(L_Chr,15) = ASC(CHR(1)).
  PUT-BYTE(L_Chr,16) = ASC(CHR(8)).
  PUT-BYTE(L_Chr,17) = ASC(CHR(46)).
  PUT-BYTE(L_Chr,18) = ASC(CHR(46)).
  PUT-BYTE(L_Chr,19) = 0.
  PUT-BYTE(L_Chr,20) = ASC(CHR(182)).
  PUT-BYTE(L_Chr,21) = ASC(CHR(208)).
  PUT-BYTE(L_Chr,22) = ASC(CHR(104)).
  PUT-BYTE(L_Chr,23) = ASC(CHR(62)).
  PUT-BYTE(L_Chr,24) = ASC(CHR(128)).
  PUT-BYTE(L_Chr,25) = ASC(CHR(47)).
  PUT-BYTE(L_Chr,26) = ASC(CHR(12)).
  PUT-BYTE(L_Chr,27) = ASC(CHR(169)).
  PUT-BYTE(L_Chr,28) = ASC(CHR(254)).
  PUT-BYTE(L_Chr,29) = ASC(CHR(100)).
  PUT-BYTE(L_Chr,30) = ASC(CHR(83)).
  PUT-BYTE(L_Chr,31) = ASC(CHR(105)).
  PUT-BYTE(L_Chr,32) = ASC(CHR(122)).
  
  SET-SIZE(pMemPtr) = 32.

  l_Ctr = pLength + 1.
  l_Pad = 1. 
  DO WHILE TRUE:
    IF ASC(CHR(GET-BYTE(l_Chr,l_pad))) = -1 THEN
      PUT-BYTE( pMemPtr, L_Ctr) = 0. 
    ELSE
      PUT-BYTE( pMemPtr, L_Ctr) = ASC(CHR(GET-BYTE(l_Chr,l_pad))). 

    l_Ctr = l_Ctr + 1.
    L_pad = L_pad + 1.
    IF L_Ctr > 32 THEN LEAVE.
  END. /* While True */

  SET-SIZE(L_Chr) = 0.
END. /* PadString */

PROCEDURE EncryptContent:
  DEFINE INPUT  PARAMETER p_EncryptKey     AS MEMPTR    NO-UNDO.
  DEFINE INPUT  PARAMETER p_Original       AS MEMPTR    NO-UNDO.
  DEFINE OUTPUT PARAMETER p_Encrypted      AS MEMPTR    NO-UNDO.

  DEFINE VARIABLE L_Hash    AS CHARACTER NO-UNDO.
  DEFINE VARIABLE L_NewKey  AS MEMPTR NO-UNDO.
  DEFINE VARIABLE L_Copy    AS MEMPTR NO-UNDO.
  DEFINE VARIABLE L_Keyptr  AS MEMPTR NO-UNDO.
  DEFINE VARIABLE L_Loop    AS INTEGER NO-UNDO.
  DEFINE VARIABLE L_Byte    AS INTEGER NO-UNDO.

  DEFINE VARIABLE L_Content   AS MEMPTR NO-UNDO.

  SET-SIZE(L_KeyPtr) = 0.
  SET-SIZE(L_Content) = 0.
  SET-SIZE(L_KeyPtr) = 10.
  SET-SIZE(L_Content) = GET-SIZE(p_Original).
  
  L_Content = p_Original.

  PDFendecrypt(L_Content, p_Encryptkey).

  p_Encrypted = L_Content.

  SET-SIZE(L_KeyPtr) = 0.
  SET-SIZE(L_Content) = 0.
  SET-SIZE(L_KeyPtr) = 0.
  SET-SIZE(L_Content) = 0.

END. /* EncryptContent */

PROCEDURE GetEncryptKey:
  DEFINE INPUT  PARAMETER pdfStream         AS CHARACTER NO-UNDO.
  DEFINE INPUT  PARAMETER p_UniqueID        AS CHARACTER NO-UNDO.
  DEFINE INPUT  PARAMETER p_Object          AS INTEGER   NO-UNDO.
  DEFINE INPUT  PARAMETER p_Gen             AS INTEGER   NO-UNDO.
  DEFINE INPUT  PARAMETER p_StreamKey       AS MEMPTR    NO-UNDO.
  DEFINE OUTPUT PARAMETER p_EncryptKey      AS MEMPTR    NO-UNDO.

  DEFINE VARIABLE L_Hex     AS CHARACTER NO-UNDO.
  DEFINE VARIABLE L_Hash    AS CHARACTER NO-UNDO.
  DEFINE VARIABLE L_Keyptr  AS MEMPTR NO-UNDO.
  DEFINE VARIABLE L_Loop    AS INTEGER NO-UNDO.
  DEFINE VARIABLE L_Byte    AS INTEGER NO-UNDO.
  DEFINE VARIABLE L_Len     AS INTEGER NO-UNDO.

  DEFINE VARIABLE L_TxtFile   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE L_md5File   AS CHARACTER NO-UNDO.

  ASSIGN L_TxtFile = SESSION:TEMP-DIR + p_UniqueID + "-key.txt"
         L_md5File = SESSION:TEMP-DIR + p_UniqueID + "-key.md5".

  L_Len = IF pdf_get_parameter(pdfStream,"EncryptKey") = "40" THEN 10 ELSE 32.
  SET-SIZE(L_KeyPtr) = 5 + INT(l_Len / 2).

  /* Take first n-bytes of original Rc4-Key ... */
  PUT-BYTES(l_KeyPtr,1) = GET-BYTES(p_StreamKey,1,INT(l_Len / 2)).

  IF l_Len = 10 THEN DO: /* 40 Bit */
    /* Append Object Number - Low Order Bytes First */
    L_Hex = SUBSTR(int2hexchar(p_Object),9,2).
    FIND HexArray WHERE HexArray.Hex-Val = L_Hex NO-ERROR.
    IF NOT AVAIL HexArray OR HexArray.chr-val = 0 THEN
      PUT-BYTE(L_KeyPtr,6) = 0.
    ELSE
      PUT-BYTE(L_KeyPtr,6) = ASC(CHR(HexArray.chr-val)).

    L_Hex = SUBSTR(int2hexchar(p_Object),7,2).
    FIND HexArray WHERE HexArray.Hex-Val = L_Hex NO-ERROR.
    IF NOT AVAIL HexArray OR HexArray.chr-val = 0 THEN
      PUT-BYTE(L_KeyPtr,7) = 0.
    ELSE
      PUT-BYTE(L_KeyPtr,7) = ASC(CHR(HexArray.chr-val)).

    L_Hex = SUBSTR(int2hexchar(p_Object),5,2).
    FIND HexArray WHERE HexArray.Hex-Val = L_Hex NO-ERROR.
    IF NOT AVAIL HexArray OR HexArray.chr-val = 0 THEN
      PUT-BYTE(L_KeyPtr,8) = 0.
    ELSE
      PUT-BYTE(L_KeyPtr,8) = ASC(CHR(HexArray.chr-val)).

    /* Append Generation Number - currently always zero */
    PUT-BYTE( L_KeyPtr, 9)   = 0. 
    PUT-BYTE( L_KeyPtr, 10)  = 0. 
  END.

  ELSE DO:  /* 128 Bit */
    /* Append Object Number - Low Order Bytes First */
    L_Hex = SUBSTR(int2hexchar(p_Object),9,2).
    FIND HexArray WHERE HexArray.Hex-Val = L_Hex NO-ERROR.
    IF NOT AVAIL HexArray OR HexArray.chr-val = 0 THEN
      PUT-BYTE(L_KeyPtr,17) = 0.
    ELSE
      PUT-BYTE(L_KeyPtr,17) = ASC(CHR(HexArray.chr-val)).

    L_Hex = SUBSTR(int2hexchar(p_Object),7,2).
    FIND HexArray WHERE HexArray.Hex-Val = L_Hex NO-ERROR.
    IF NOT AVAIL HexArray OR HexArray.chr-val = 0 THEN
      PUT-BYTE(L_KeyPtr,18) = 0.
    ELSE
      PUT-BYTE(L_KeyPtr,18) = ASC(CHR(HexArray.chr-val)).

    L_Hex = SUBSTR(int2hexchar(p_Object),5,2).
    FIND HexArray WHERE HexArray.Hex-Val = L_Hex NO-ERROR.
    IF NOT AVAIL HexArray OR HexArray.chr-val = 0 THEN
      PUT-BYTE(L_KeyPtr,19) = 0.
    ELSE
      PUT-BYTE(L_KeyPtr,19) = ASC(CHR(HexArray.chr-val)).

    /* Append Generation Number - currently always zero */
    PUT-BYTE( L_KeyPtr, 20)  = 0. 
    PUT-BYTE( L_KeyPtr, 21)  = 0. 
  END. /* 128 Bit */

  /* Then run the md5 hash against that n-byte key - orig + obj + gen */
  RUN md5hash(INPUT p_UniqueID,
              INPUT l_KeyPtr,
              OUTPUT L_Hash).

  /* Now take the first n + 5 bytes of the resulting md5 hash as the new key */
  L_Byte = 1.
  DO L_loop = 1 TO (L_len + 10) BY 2:
    IF SUBSTR(L_Hash,L_loop,2) = "00" THEN
      PUT-BYTE(p_EncryptKey,L_Byte) = 0.
    ELSE DO:
      FIND FIRST HexArray WHERE HexArray.Hex-Val = SUBSTR(L_Hash,L_Loop,2) NO-ERROR.
      IF NOT AVAIL HexArray OR HexArray.chr-val = 0 THEN
        PUT-BYTE(p_EncryptKey,L_Byte) = 0.
      ELSE
        PUT-BYTE(p_EncryptKey,L_Byte) = ASC(CHR(HexArray.chr-val)).
    END.

    L_Byte = L_Byte + 1.
  END.

  SET-SIZE(L_KeyPtr) = 0.

END. /* GetEncryptKey */

PROCEDURE BuildDocId:
  DEFINE INPUT  PARAMETER pUniqueID AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER pID       AS CHARACTER NO-UNDO.

  DEFINE VARIABLE L_Loop      AS INTEGER NO-UNDO.
  DEFINE VARIABLE L_TxtFile   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE L_md5File   AS CHARACTER NO-UNDO.

  DEFINE VARIABLE mID     AS MEMPTR NO-UNDO.

  ASSIGN L_TxtFile = SESSION:TEMP-DIR + pUniqueID + "-ID.txt"
         L_md5File = SESSION:TEMP-DIR + pUniqueID + "-ID.md5".

  /* Set Memory Size and Contents */
  SET-SIZE(mID) = LENGTH(pUniqueiD).
  PUT-STRING(mID,1,LENGTH(pUniqueID)) = pUniqueID.

  OUTPUT TO VALUE(L_TxtFile).
    EXPORT mID.
  OUTPUT CLOSE.

  IF OPSYS = "UNIX" THEN DO:
    INPUT THROUGH VALUE("{&MD5LIB} " + L_TxtFile) NO-ECHO.
      import pID.
    INPUT CLOSE.
  END.

  ELSE DO:
    OS-COMMAND SILENT VALUE( "{&MD5LIB} " 
                             + L_TxtFile
                             + " > "
                             + L_md5File ) .

    INPUT FROM VALUE(L_Md5File) NO-ECHO NO-CONVERT.
      IMPORT pID.
    INPUT CLOSE.

    OS-DELETE VALUE(L_md5File).
  END.

  OS-DELETE VALUE(L_TxtFile).

  /* Reallocate memory */
  SET-SIZE(mID) = 0.

END. /* BuildDocID */

/* end of pdf_inc.p */
