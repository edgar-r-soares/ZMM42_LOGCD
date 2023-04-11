*----------------------------------------------------------------------*
***INCLUDE ZMM_MM42_LOGISTIC_DATA_F01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  F_DISPLAY_DATA
*&---------------------------------------------------------------------*
FORM f_display_data .
  STATICS: ls_layout TYPE lvc_s_layo.
  STATICS: ls_variant TYPE disvariant.
  STATICS: lt_fieldcat TYPE lvc_t_fcat.
  FIELD-SYMBOLS: <ls_fieldcat> LIKE LINE OF lt_fieldcat.
  DATA: lt_comp TYPE cl_abap_structdescr=>component_table,
        ls_dfies TYPE dfies,
        lr_elemdescr TYPE REF TO cl_abap_elemdescr,
        lr_typedescr TYPE REF TO cl_abap_typedescr.
  FIELD-SYMBOLS <lr_comp> LIKE LINE OF lt_comp.
  DATA: lr_struct_typ TYPE REF TO cl_abap_structdescr.
  DATA: ls_data TYPE t_data.
  FIELD-SYMBOLS: <l_field>.
  DATA: l_help_id TYPE string.

  IF gr_alv IS INITIAL.
    lr_struct_typ ?= cl_abap_structdescr=>describe_by_data( ls_data ).
    lt_comp = lr_struct_typ->get_components( ).
    LOOP AT lt_comp ASSIGNING <lr_comp> WHERE name NE 'COLOR_TAB'.
      TRY.
          lr_typedescr ?= <lr_comp>-type.

          APPEND INITIAL LINE TO lt_fieldcat ASSIGNING <ls_fieldcat>.
          IF lr_typedescr->is_ddic_type( ) = 'X'.
            lr_elemdescr ?= <lr_comp>-type.
            ls_dfies = lr_elemdescr->get_ddic_field( sy-langu ).
            <ls_fieldcat>-tabname     = ls_dfies-tabname.
            <ls_fieldcat>-fieldname   = <lr_comp>-name.
            <ls_fieldcat>-scrtext_l   = ls_dfies-scrtext_l.
            <ls_fieldcat>-scrtext_m   = ls_dfies-scrtext_m.
            <ls_fieldcat>-scrtext_s   = ls_dfies-scrtext_s.

*            if <ls_fieldcat>-fieldname eq 'SALES_3D'.
*              <ls_fieldcat>-scrtext_l   = '3dVendas'.
*              <ls_fieldcat>-coltext     = <ls_fieldcat>-scrtext_l.
*              <ls_fieldcat>-scrtext_m   = <ls_fieldcat>-scrtext_m.
*              <ls_fieldcat>-scrtext_s   = <ls_fieldcat>-scrtext_s.
*              <ls_fieldcat>-qfieldname = 'MEINS'.
**              <ls_fieldcat>-edit = 'X'.
*              <ls_fieldcat>-tech = 'X'.
**editables
            IF <ls_fieldcat>-fieldname EQ 'RAUBE'.
              <ls_fieldcat>-edit = 'X'.
            ELSEIF <ls_fieldcat>-fieldname EQ 'TRAGR'.
              <ls_fieldcat>-edit = 'X'.
            ELSEIF <ls_fieldcat>-fieldname EQ 'MHDRZ'.
              <ls_fieldcat>-edit = 'X'.
            ELSEIF <ls_fieldcat>-fieldname EQ 'IPRKZ'.
              <ls_fieldcat>-edit = 'X'.
            ELSEIF <ls_fieldcat>-fieldname EQ 'MHDLP'.
              <ls_fieldcat>-edit = 'X'.
            ELSEIF <ls_fieldcat>-fieldname EQ 'PLIFZ'.
              <ls_fieldcat>-edit = 'X'.
            ELSEIF <ls_fieldcat>-fieldname EQ 'LADGR'.
              <ls_fieldcat>-edit = 'X'.
            ELSEIF <ls_fieldcat>-fieldname EQ 'LGFSB'.
              <ls_fieldcat>-edit = 'X'.
            ELSEIF <ls_fieldcat>-fieldname EQ 'MTVFP'.
              <ls_fieldcat>-edit = 'X'.
            ELSEIF <ls_fieldcat>-fieldname EQ 'FPRFM'.
              <ls_fieldcat>-edit = 'X'.
            ELSEIF <ls_fieldcat>-fieldname EQ 'LGNUM'. ""
*              <ls_fieldcat>-edit = 'X'.
            ELSEIF <ls_fieldcat>-fieldname EQ 'LVSME'.
              <ls_fieldcat>-edit = 'X'.
            ELSEIF <ls_fieldcat>-fieldname EQ 'PLKPT'.
              <ls_fieldcat>-edit = 'X'.
              <ls_fieldcat>-emphasize = 'X'.
            ELSEIF <ls_fieldcat>-fieldname EQ 'LHMG1'.
              <ls_fieldcat>-edit = 'X'.
              <ls_fieldcat>-qfieldname = 'LHME1'.
            ELSEIF <ls_fieldcat>-fieldname EQ 'LHME1'.
              <ls_fieldcat>-edit = 'X'.
            ELSEIF <ls_fieldcat>-fieldname EQ 'LETY1'.
              <ls_fieldcat>-edit = 'X'.
            ELSEIF <ls_fieldcat>-fieldname EQ 'AUSME'.
              <ls_fieldcat>-edit = 'X'.
            ELSEIF <ls_fieldcat>-fieldname EQ 'LTKZA'.
              <ls_fieldcat>-edit = 'X'.
*         lgbkz TYPE mlgn-lgbkz,
            ELSEIF <ls_fieldcat>-fieldname EQ 'LTKZE'.
              <ls_fieldcat>-edit = 'X'.
            ELSEIF <ls_fieldcat>-fieldname EQ 'LGBKZ'.
              <ls_fieldcat>-edit = 'X'.
            ELSEIF <ls_fieldcat>-fieldname EQ 'LGTYP'. """"
*              <ls_fieldcat>-edit = 'X'.
            ELSEIF <ls_fieldcat>-fieldname EQ 'LGPLA'.
              <ls_fieldcat>-edit = 'X'.
              <ls_fieldcat>-qfieldname = 'MEINS'.
            ELSEIF <ls_fieldcat>-fieldname EQ 'LPMAX'.
              <ls_fieldcat>-edit = 'X'.
              <ls_fieldcat>-qfieldname = 'MEINS'.
            ELSEIF <ls_fieldcat>-fieldname EQ 'LPMIN'.
              <ls_fieldcat>-edit = 'X'.
              <ls_fieldcat>-qfieldname = 'MEINS'.
            ELSEIF <ls_fieldcat>-fieldname EQ 'MAMNG'.
              <ls_fieldcat>-edit = 'X'.
              <ls_fieldcat>-qfieldname = 'MEINS'.
            ELSEIF <ls_fieldcat>-fieldname EQ 'NSMNG'.
              <ls_fieldcat>-edit = 'X'.
              <ls_fieldcat>-qfieldname = 'MEINS'.
            ELSEIF <ls_fieldcat>-fieldname EQ 'STOCK_DEP'.
              <ls_fieldcat>-scrtext_l   = 'Stock WM'.
              <ls_fieldcat>-coltext     = <ls_fieldcat>-scrtext_l.
              <ls_fieldcat>-scrtext_m   = <ls_fieldcat>-scrtext_l.
              <ls_fieldcat>-scrtext_s   = <ls_fieldcat>-scrtext_l.
              <ls_fieldcat>-seltext     = <ls_fieldcat>-scrtext_l.
              <ls_fieldcat>-tooltip     = <ls_fieldcat>-scrtext_l.
              <ls_fieldcat>-checkbox = 'X'.
            ELSEIF <ls_fieldcat>-fieldname EQ 'STOCK_PARK'.
              <ls_fieldcat>-scrtext_l   = 'Stock Parque'.
              <ls_fieldcat>-coltext     = <ls_fieldcat>-scrtext_l.
              <ls_fieldcat>-scrtext_m   = <ls_fieldcat>-scrtext_l.
              <ls_fieldcat>-scrtext_s   = <ls_fieldcat>-scrtext_l.
              <ls_fieldcat>-seltext     = <ls_fieldcat>-scrtext_l.
              <ls_fieldcat>-tooltip     = <ls_fieldcat>-scrtext_l.
              <ls_fieldcat>-checkbox = 'X'.
            ELSEIF <ls_fieldcat>-fieldname EQ 'STOCK_NDEP'.
              <ls_fieldcat>-scrtext_l   = 'StckOutrosDep'.
              <ls_fieldcat>-coltext     = <ls_fieldcat>-scrtext_l.
              <ls_fieldcat>-scrtext_m   = <ls_fieldcat>-scrtext_l.
              <ls_fieldcat>-scrtext_s   = <ls_fieldcat>-scrtext_l.
              <ls_fieldcat>-seltext     = <ls_fieldcat>-scrtext_l.
              <ls_fieldcat>-tooltip     = <ls_fieldcat>-scrtext_l.
              <ls_fieldcat>-checkbox = 'X'.
*            ENDIF.
            ENDIF.

            <ls_fieldcat>-reptext     = ls_dfies-reptext.
            <ls_fieldcat>-domname     = ls_dfies-domname.
            <ls_fieldcat>-rollname    = ls_dfies-rollname.
            <ls_fieldcat>-intlen      = ls_dfies-intlen.
            <ls_fieldcat>-outputlen   = ls_dfies-outputlen.
            <ls_fieldcat>-decimals    = ls_dfies-decimals.
            <ls_fieldcat>-datatype    = ls_dfies-datatype.
            <ls_fieldcat>-inttype     = ls_dfies-inttype.
            <ls_fieldcat>-rollname    = ls_dfies-rollname.
            <ls_fieldcat>-convexit    = ls_dfies-convexit.
            <ls_fieldcat>-f4availabl  = ls_dfies-f4availabl.
          ELSE.
            <ls_fieldcat>-intlen      = lr_typedescr->length.
            <ls_fieldcat>-inttype     = lr_typedescr->type_kind.
            <ls_fieldcat>-fieldname   = <lr_comp>-name.

          ENDIF.

*         --- describe the HELP-ID to take over information about ref_table and ref_field
          ASSIGN COMPONENT <ls_fieldcat>-fieldname OF STRUCTURE ls_data"<ls_line>
                                                                TO <l_field>.
          IF sy-subrc EQ 0.
            DESCRIBE FIELD <l_field> HELP-ID l_help_id.
            IF l_help_id CA '-'.
              SPLIT l_help_id AT '-' INTO <ls_fieldcat>-ref_table <ls_fieldcat>-ref_field.
            ELSE.
              <ls_fieldcat>-ref_field = l_help_id.
            ENDIF.
          ENDIF.
        CATCH cx_root.

      ENDTRY.
    ENDLOOP.



    CREATE OBJECT gr_alv
      EXPORTING
        i_parent          = cl_gui_custom_container=>screen0
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.

    ls_layout-cwidth_opt = 'X'.
    ls_layout-zebra = 'X'.
*    ls_layout-SEL_MODE = 'D'.
    ls_layout-ctab_fname = 'COLOR_TAB'.
    ls_variant-report = sy-repid.
    ls_variant-variant = p_var.

    DATA: lr_event_handler TYPE REF TO lcl_handle_events.

    CREATE OBJECT lr_event_handler.
    SET HANDLER lr_event_handler->on_double_click FOR gr_alv.
    SET HANDLER lr_event_handler->on_data_changed FOR gr_alv. "PB+
    SET HANDLER lr_event_handler->on_data_changed_end FOR gr_alv.
**  lr_alv->register_edit_event( i_event_id = cl_gui_alv_grid=>mc_evt_enter ).
    SET HANDLER lr_event_handler->handle_toolbar FOR gr_alv .
    SET HANDLER lr_event_handler->handle_command FOR gr_alv .
    gr_alv->register_edit_event( i_event_id = cl_gui_alv_grid=>mc_evt_modified ).

    CALL METHOD gr_alv->set_table_for_first_display
      EXPORTING
        is_variant                    = ls_variant
        i_save                        = 'X'
        is_layout                     = ls_layout
      CHANGING
        it_outtab                     = gt_data
        it_fieldcatalog               = lt_fieldcat
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.
  ENDIF.

ENDFORM.                    " F_DISPLAY_DATA
*&---------------------------------------------------------------------*
*&      Form  F_SAVE
*&---------------------------------------------------------------------*
FORM f_save.
  DATA: lt_mara TYPE TABLE OF t_mara WITH KEY matnr,
        ls_mara TYPE t_mara.
  FIELD-SYMBOLS: <ls_mara> TYPE t_mara,
                 <ls_marc> TYPE t_marc,
                 <ls_mlgn> TYPE t_mlgn,
                 <ls_mlgt> TYPE t_mlgt,
                 <ls_data> TYPE t_data.
  DATA: ls_return TYPE bapireturn1.
  DATA: lt_return TYPE TABLE OF bapireturn1.

  CONSTANTS:
    lc_bo_bus1006 TYPE swo_objtyp VALUE 'BUS1001006',
    lc_event      TYPE swo_event  VALUE 'LogModified'.


  LOOP AT gt_data ASSIGNING <ls_data>.
    READ TABLE lt_mara ASSIGNING <ls_mara> WITH TABLE KEY matnr = <ls_data>-matnr.
    IF sy-subrc NE 0.
      CLEAR ls_mara.
      ls_mara-matnr = <ls_data>-matnr.
      ls_mara-raube = <ls_data>-raube.
      ls_mara-tragr = <ls_data>-tragr.
      ls_mara-mhdrz = <ls_data>-mhdrz.
      ls_mara-iprkz = <ls_data>-iprkz.
      ls_mara-mhdlp = <ls_data>-mhdlp.
      INSERT ls_mara INTO TABLE lt_mara ASSIGNING <ls_mara>.
    ENDIF.
    APPEND INITIAL LINE TO <ls_mara>-marc ASSIGNING <ls_marc>.
    <ls_marc>-werks = <ls_data>-werks.
    <ls_marc>-plifz = <ls_data>-plifz.
    <ls_marc>-ladgr = <ls_data>-ladgr.
    <ls_marc>-lgfsb = <ls_data>-lgfsb.
    <ls_marc>-mtvfp = <ls_data>-mtvfp.
    <ls_marc>-fprfm = <ls_data>-fprfm.
    READ TABLE <ls_mara>-mlgn ASSIGNING <ls_mlgn> WITH KEY lgnum = <ls_data>-lgnum.
    IF sy-subrc NE 0.
      APPEND INITIAL LINE TO <ls_mara>-mlgn ASSIGNING <ls_mlgn>.
      <ls_mlgn>-lgnum = <ls_data>-lgnum.
    ENDIF.
    <ls_mlgn>-lvsme = <ls_data>-lvsme.
    <ls_mlgn>-plkpt = <ls_data>-plkpt.
    <ls_mlgn>-lhmg1 = <ls_data>-lhmg1.
    <ls_mlgn>-lhme1 = <ls_data>-lhme1.
    <ls_mlgn>-lety1 = <ls_data>-lety1.
    <ls_mlgn>-ltkza = <ls_data>-ltkza.
    <ls_mlgn>-lgbkz = <ls_data>-lgbkz.
    <ls_mlgn>-ltkze = <ls_data>-ltkze.
    READ TABLE <ls_mlgn>-mlgt ASSIGNING <ls_mlgt> WITH KEY lgtyp = <ls_data>-lgtyp.
    IF sy-subrc NE 0.
      APPEND INITIAL LINE TO <ls_mlgn>-mlgt ASSIGNING <ls_mlgt>.
      <ls_mlgt>-lgtyp = <ls_data>-lgtyp.
    ENDIF.
    <ls_mlgt>-lgpla = <ls_data>-lgpla.
    <ls_mlgt>-lpmax = <ls_data>-lpmax.
    <ls_mlgt>-lpmin = <ls_data>-lpmin.
    <ls_mlgt>-mamng = <ls_data>-mamng.
    <ls_mlgt>-nsmng = <ls_data>-nsmng.
  ENDLOOP.
  LOOP AT lt_mara ASSIGNING <ls_mara>.
    PERFORM f_update_material USING <ls_mara> ls_return.
    IF ls_return IS NOT INITIAL.
      APPEND ls_return TO lt_return.
    ENDIF.
  ENDLOOP.
  IF lt_return[] IS NOT INITIAL.
    zbatch_input=>display_return_messages( lt_return ).
*    EXPORTING
*      it_return =
*      .
***** PCA 21.07.2020 - Begin - Event to continue WF
*    READ TABLE lt_return WITH KEY type = 'E' TRANSPORTING NO FIELDS.
*    IF sy-subrc NE 0.
*      CALL FUNCTION 'SWE_EVENT_CREATE'
*        EXPORTING
*          objtype                       = lc_bo_bus1006
*          objkey                        = <ls_data>-matnr
*          event                         = lc_event
**         CREATOR                       = ' '
**         TAKE_WORKITEM_REQUESTER       = ' '
**         START_WITH_DELAY              = ' '
**         START_RECFB_SYNCHRON          = ' '
**         NO_COMMIT_FOR_QUEUE           = ' '
**         DEBUG_FLAG                    = ' '
**         NO_LOGGING                    = ' '
**         IDENT                         =
**         IMPORTING
**         EVENT_ID                      =
**         RECEIVER_COUNT                =
**        TABLES
**          event_container               = lt_event_cont
*        EXCEPTIONS
*          objtype_not_found             = 1
*          OTHERS                        = 2.
*      IF sy-subrc <> 0.
**          MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**             WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*      ENDIF.
*    ENDIF.
***** PCA 21.07.2020 - End - Event to continue WF
  ENDIF.
ENDFORM.                    " F_SAVE
*&---------------------------------------------------------------------*
*&      Form  F_DOWNLOAD
*&---------------------------------------------------------------------*
FORM f_download .
  DATA: l_file TYPE string.

  CHECK gt_data IS NOT INITIAL.
  PERFORM f_file_save_dialog USING l_file.
  IF l_file IS NOT INITIAL.
    PERFORM f_create_excel USING l_file.
  ENDIF.
ENDFORM.                    " F_DOWNLOAD
*&---------------------------------------------------------------------*
*&      Form  f_file_save_dialog
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_FILE     text
*----------------------------------------------------------------------*
FORM f_file_save_dialog USING p_file.
  DATA: l_file TYPE string,
        l_path TYPE string,
        l_fullpath TYPE string.

  CALL METHOD cl_gui_frontend_services=>file_save_dialog
    EXPORTING
      window_title         = 'Download template de carregamento'
      default_extension    = 'XLS'
*      default_file_name    =
*      file_filter          =
    CHANGING
      filename             = l_file
      path                 = l_path
      fullpath             = l_fullpath
    EXCEPTIONS
      cntl_error           = 1
      error_no_gui         = 2
      not_supported_by_gui = 3
      OTHERS               = 4
          .
  IF sy-subrc <> 0.
*   MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ELSE.
    TRANSLATE l_file TO LOWER CASE.
    IF l_file CP '*.xls'.
      CONCATENATE l_fullpath 'X' INTO p_file.
    ELSE.
      p_file = l_fullpath.
    ENDIF.
  ENDIF.

ENDFORM.                    "f_file_save_dialog
*&---------------------------------------------------------------------*
*&      Form  f_create_excel
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_FILE     text
*----------------------------------------------------------------------*
FORM f_create_excel USING p_file.
  DATA: lr_excel TYPE REF TO zcl_excel.

  CREATE OBJECT lr_excel.

  PERFORM f_init_excel_styles USING lr_excel.
  PERFORM f_add_table_to_excel USING lr_excel.

  DATA: cl_writer TYPE REF TO zif_excel_writer.
  DATA: xdata       TYPE xstring,             " Will be used for sending as email
        t_rawdata   TYPE solix_tab,           " Will be used for downloading or open directly
        bytecount   TYPE i.                   " Will be used for downloading or open directly

  CREATE OBJECT cl_writer TYPE zcl_excel_writer_2007.
  xdata = cl_writer->write_file( lr_excel ).
  t_rawdata = cl_bcs_convert=>xstring_to_solix( iv_xstring  = xdata ).
  bytecount = XSTRLEN( xdata ).

  cl_gui_frontend_services=>gui_download( EXPORTING bin_filesize = bytecount
                                                    filename     = p_file
                                                    filetype     = 'BIN'
                                           CHANGING data_tab     = t_rawdata ).
ENDFORM.                    "f_create_excel
*&---------------------------------------------------------------------*
*&      Form  f_init_excel_styles
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PR_EXCEL   text
*----------------------------------------------------------------------*
FORM f_init_excel_styles USING pr_excel TYPE REF TO zcl_excel.
  DATA: lo_worksheet            TYPE REF TO zcl_excel_worksheet,
        lo_style_header_up      TYPE REF TO zcl_excel_style,
        lo_style_header_edit    TYPE REF TO zcl_excel_style,
        lo_style_header_down    TYPE REF TO zcl_excel_style,
        lo_style_normal         TYPE REF TO zcl_excel_style,
        lo_style_edit           TYPE REF TO zcl_excel_style,
        lo_style_red            TYPE REF TO zcl_excel_style,
        lo_style_light_red      TYPE REF TO zcl_excel_style,
        lo_style_green          TYPE REF TO zcl_excel_style,
        lo_style_yellow         TYPE REF TO zcl_excel_style,
        lo_style_light_blue     TYPE REF TO zcl_excel_style,
        lo_border_dark          TYPE REF TO zcl_excel_style_border.

  " Create border object
  CREATE OBJECT lo_border_dark.
  lo_border_dark->border_color-rgb = zcl_excel_style_color=>c_black.
  lo_border_dark->border_style = zcl_excel_style_border=>c_border_thin.

  "Create style top header
  lo_style_header_up                         = pr_excel->add_new_style( ).
  lo_style_header_up->borders->allborders    = lo_border_dark.
  lo_style_header_up->fill->filltype     = zcl_excel_style_fill=>c_fill_solid.
  lo_style_header_up->fill->fgcolor-rgb  = '0F243E'. "top zahara blue
  lo_style_header_up->font->bold   = abap_true.
  lo_style_header_up->font->color-rgb  = zcl_excel_style_color=>c_white.
  lo_style_header_up->font->name         = zcl_excel_style_font=>c_name_calibri.
  lo_style_header_up->font->size = 10.
*  lo_style_header_up->font->scheme       = zcl_excel_style_font=>c_scheme_major.
*  lo_style_header_up->protection->locked = zcl_excel_style_protection=>c_protection_locked.
  s_styles-header_up                    = lo_style_header_up->get_guid( ).


  "Create style top header edit column
  lo_style_header_edit                         = pr_excel->add_new_style( ).
  lo_style_header_edit->borders->allborders    = lo_border_dark.
  lo_style_header_edit->fill->filltype     = zcl_excel_style_fill=>c_fill_solid.
  lo_style_header_edit->fill->fgcolor-rgb  = '0F243E'. "top zahara blue
  lo_style_header_edit->font->bold        = abap_true.
  lo_style_header_edit->font->color-rgb  = zcl_excel_style_color=>c_red.
  lo_style_header_edit->font->name         = zcl_excel_style_font=>c_name_calibri.
  lo_style_header_edit->font->size = 10.
*  lo_style_header_edit->font->scheme       = zcl_excel_style_font=>c_scheme_major.
*  lo_style_header_edit->protection->locked = zcl_excel_style_protection=>c_protection_locked.
  s_styles-header_edit                    = lo_style_header_edit->get_guid( ).

  "Create style second line header
  lo_style_header_down                         = pr_excel->add_new_style( ).
  lo_style_header_down->borders->allborders    = lo_border_dark.
  lo_style_header_down->fill->filltype     = zcl_excel_style_fill=>c_fill_solid.
  lo_style_header_down->fill->fgcolor-rgb  = 'ADD1A5'. "small header zahara green
  lo_style_header_down->font->name         = zcl_excel_style_font=>c_name_calibri.
  lo_style_header_down->font->size = 10.
*  lo_style_header_down->protection->locked = zcl_excel_style_protection=>c_protection_locked.
  s_styles-header_down                    = lo_style_header_down->get_guid( ).

  "Create style table cells
  lo_style_normal                         = pr_excel->add_new_style( ).
  lo_style_normal->borders->allborders    = lo_border_dark.
  lo_style_normal->font->name             = zcl_excel_style_font=>c_name_calibri.
  lo_style_normal->font->size             = 8.
*  lo_style_normal->protection->locked = zcl_excel_style_protection=>c_protection_locked.
  s_styles-normal                         = lo_style_normal->get_guid( ).

  lo_style_red                         = pr_excel->add_new_style( ).
  lo_style_red->borders->allborders    = lo_border_dark.
  lo_style_red->font->name             = zcl_excel_style_font=>c_name_calibri.
  lo_style_red->font->size             = 8.
  lo_style_red->fill->filltype     = zcl_excel_style_fill=>c_fill_solid.
  lo_style_red->fill->fgcolor-rgb  = 'FF3333'. "red
  s_styles-red                         = lo_style_red->get_guid( ).

  lo_style_light_red                         = pr_excel->add_new_style( ).
  lo_style_light_red->borders->allborders    = lo_border_dark.
  lo_style_light_red->font->name             = zcl_excel_style_font=>c_name_calibri.
  lo_style_light_red->font->size             = 8.
  lo_style_light_red->fill->filltype     = zcl_excel_style_fill=>c_fill_solid.
  lo_style_light_red->fill->fgcolor-rgb  = 'FFCCCC'. "light_red
  s_styles-light_red                         = lo_style_light_red->get_guid( ).

  lo_style_green                         = pr_excel->add_new_style( ).
  lo_style_green->borders->allborders    = lo_border_dark.
  lo_style_green->font->name             = zcl_excel_style_font=>c_name_calibri.
  lo_style_green->font->size             = 8.
  lo_style_green->fill->filltype     = zcl_excel_style_fill=>c_fill_solid.
  lo_style_green->fill->fgcolor-rgb  = '00CC44'. "green
  s_styles-green                         = lo_style_green->get_guid( ).

  lo_style_yellow                         = pr_excel->add_new_style( ).
  lo_style_yellow->borders->allborders    = lo_border_dark.
  lo_style_yellow->font->name             = zcl_excel_style_font=>c_name_calibri.
  lo_style_yellow->font->size             = 8.
  lo_style_yellow->fill->filltype     = zcl_excel_style_fill=>c_fill_solid.
  lo_style_yellow->fill->fgcolor-rgb  = 'FFFF00'. "yellow
  s_styles-yellow                         = lo_style_yellow->get_guid( ).

  lo_style_light_blue                         = pr_excel->add_new_style( ).
  lo_style_light_blue->borders->allborders    = lo_border_dark.
  lo_style_light_blue->font->name             = zcl_excel_style_font=>c_name_calibri.
  lo_style_light_blue->font->size             = 8.
  lo_style_light_blue->fill->filltype     = zcl_excel_style_fill=>c_fill_solid.
  lo_style_light_blue->fill->fgcolor-rgb  = 'DCE6F1'. "light blue
  s_styles-light_blue                         = lo_style_light_blue->get_guid( ).

  "Create style edit cells
  lo_style_edit                         = pr_excel->add_new_style( ).
  lo_style_edit->borders->allborders    = lo_border_dark.
  lo_style_edit->font->name             = zcl_excel_style_font=>c_name_calibri.
  lo_style_edit->font->size             = 8.
  lo_style_edit->fill->filltype     = zcl_excel_style_fill=>c_fill_solid.
  lo_style_edit->fill->fgcolor-rgb  = 'FFFF66'. "top zahara blue
  lo_style_edit->number_format->format_code = zcl_excel_style_number_format=>c_format_text."c_format_number.
  lo_style_edit->protection->locked = zcl_excel_style_protection=>c_protection_unlocked.
  s_styles-edit                        = lo_style_edit->get_guid( ).

ENDFORM.                    "f_init_excel_styles
*&---------------------------------------------------------------------*
*&      Form  f_add_table_to_excel
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PR_EXCEL   text
*----------------------------------------------------------------------*
FORM f_add_table_to_excel USING pr_excel TYPE REF TO zcl_excel.
  DATA: lo_column      TYPE REF TO cl_salv_column.
  DATA: lo_columns     TYPE REF TO cl_salv_columns_table.
  DATA: lt_col TYPE salv_t_column_ref.
  FIELD-SYMBOLS: <ls_col> TYPE LINE OF salv_t_column_ref.
  DATA: l_s_text TYPE scrtext_s.
  DATA: l_col TYPE i.
  FIELD-SYMBOLS: "<lt_table> TYPE STANDARD TABLE,
*                 <ls_line> TYPE ANY,
                 <l_field> TYPE ANY.
  DATA: lo_type  TYPE REF TO cl_abap_typedescr,
        lo_element  TYPE REF TO cl_abap_elemdescr,
        l_name TYPE string.
  DATA: l_td TYPE string.
  DATA: l_field(255).
  DATA: lr_line TYPE REF TO data.

  DATA: lo_worksheet TYPE REF TO zcl_excel_worksheet.
  DATA: l_row TYPE i.
  DATA: l_style TYPE zexcel_cell_style.
  DATA: l_color_column TYPE lvc_fname.
  DATA: lr_descr TYPE REF TO cl_abap_structdescr.
  FIELD-SYMBOLS: <ls_comp> TYPE abap_compdescr.
  DATA: lt_struct_fields  TYPE cl_abap_structdescr=>component_table.
  FIELD-SYMBOLS: <ls_struct_fields> TYPE LINE OF cl_abap_structdescr=>component_table.
  FIELD-SYMBOLS: <ls_data> TYPE t_data.
  DATA: lo_elem_descr TYPE REF TO cl_abap_elemdescr.
  DATA: l_dfies TYPE dfies.

  l_color_column = 'COLOR_TAB'.

  lo_worksheet = pr_excel->get_active_worksheet( ).
  lo_worksheet->set_title( ip_title = 'Logistica' ).

  READ TABLE gt_data ASSIGNING <ls_data> INDEX 1.
  lr_descr ?= cl_abap_structdescr=>describe_by_data( <ls_data> ).
  lt_struct_fields = lr_descr->get_components( ).

  l_col = 1.
  LOOP AT lr_descr->components ASSIGNING <ls_comp> WHERE name NE l_color_column.
    ASSIGN COMPONENT <ls_comp>-name OF STRUCTURE <ls_data> TO <l_field>.
    IF sy-subrc EQ 0.
      READ TABLE lt_struct_fields ASSIGNING <ls_struct_fields> WITH KEY name = <ls_comp>-name.
      lo_elem_descr ?= <ls_struct_fields>-type.
      l_dfies = lo_elem_descr->get_ddic_field( ).
      l_s_text = l_dfies-scrtext_l.
      lo_worksheet->set_cell( ip_column = l_col ip_row = 1 ip_value = l_s_text ip_style = s_styles-header_up ).
      lo_worksheet->set_cell( ip_column = l_col ip_row = 2 ip_value = <ls_comp>-name ip_style = s_styles-header_down ).
      ADD 1 TO l_col.
    ENDIF.
  ENDLOOP.

  l_row = 2.
  LOOP AT gt_data ASSIGNING <ls_data>.
    ADD 1 TO l_row.
    l_col = 1.

    LOOP AT lr_descr->components ASSIGNING <ls_comp> WHERE name NE l_color_column.
      ASSIGN COMPONENT <ls_comp>-name OF STRUCTURE <ls_data> TO <l_field>.
      IF sy-subrc EQ 0.
**        GET REFERENCE OF <ls_data> INTO lr_line.
**        l_name = <ls_comp>-name.
**        l_name = <ddic_info>-fieldname.
**          l_style = me->get_cell_style( i_name = l_name ir_line = lr_line ).
        CASE <ls_comp>-name.
          WHEN 'RAUBE' OR 'TRAGR' OR 'MHDRZ' OR 'IPRKZ' OR 'MHDLP' OR 'PLIFZ' OR 'LADGR' OR 'LGFSB' OR 'MTVFP' OR 'FPRFM' OR
               'LVSME' OR 'PLKPT' OR 'LHMG1' OR 'LHME1' OR 'LETY1' OR 'LTKZA' OR 'LTKZE' OR "'LGTYP' or
               'LGPLA' OR 'LPMAX' OR 'LPMIN' OR 'MAMNG' OR 'MSMNG' OR 'AUSME'.
            l_style = s_styles-edit.
          WHEN OTHERS.
            l_style = s_styles-normal.
        ENDCASE.
        lo_worksheet->set_cell( ip_column = l_col ip_row = l_row ip_value = <l_field> ip_style = l_style ).
        ADD 1 TO l_col.
      ENDIF.
    ENDLOOP.
  ENDLOOP.

ENDFORM.                    "f_add_table_to_excel
*&---------------------------------------------------------------------*
*&      Form  F_UPLOAD
*&---------------------------------------------------------------------*
FORM f_upload .
  DATA: l_file TYPE string.
  DATA: l_stable TYPE lvc_s_stbl.

  PERFORM f_file_open_dialog USING l_file.
  IF l_file IS NOT INITIAL.
    PERFORM f_read_excel USING l_file.

    l_stable-row = 'X'.
    l_stable-col = 'X'.
    IF gr_alv IS NOT INITIAL.
      gr_alv->refresh_table_display( is_stable = l_stable )." i_soft_refresh = 'X' ).
    ENDIF.

  ENDIF.
ENDFORM.                    " F_UPLOAD
*&---------------------------------------------------------------------*
*&      Form  f_file_open_dialog
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_FILE     text
*----------------------------------------------------------------------*
FORM f_file_open_dialog USING p_file.
  DATA: lt_file TYPE filetable,
        l_rc TYPE i.
  FIELD-SYMBOLS: <ls_file> TYPE LINE OF filetable.

  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      window_title            = 'Upload de template preenchido'
      default_extension       = 'XLS'
*      default_filename        =
*      file_filter             =
*      with_encoding           =
*      initial_directory       =
*      multiselection          =
    CHANGING
      file_table              = lt_file
      rc                      = l_rc
*      user_action             =
*      file_encoding           =
    EXCEPTIONS
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      not_supported_by_gui    = 4
      OTHERS                  = 5
          .
  IF sy-subrc <> 0 OR l_rc NE 1.

  ELSE.
    LOOP AT lt_file ASSIGNING <ls_file>.
      p_file = <ls_file>-filename.
      EXIT.
    ENDLOOP.
  ENDIF.

ENDFORM.                    "f_file_open_dialog
*&---------------------------------------------------------------------*
*&      Form  f_read_excel
*&---------------------------------------------------------------------*
FORM f_read_excel USING p_file.
  DATA: cl_reader TYPE REF TO zif_excel_reader.
  DATA: lo_excel TYPE REF TO zcl_excel.

  CREATE OBJECT cl_reader TYPE zcl_excel_reader_2007.
  TRY.
      CALL METHOD cl_reader->load_file
        EXPORTING
          i_filename = p_file
        RECEIVING
          r_excel    = lo_excel.
    CATCH zcx_excel .
      MESSAGE e899(mm) WITH 'Erro a abrir ficheiro'.
  ENDTRY.

  PERFORM f_populate_from_excel USING lo_excel.

ENDFORM.                    "f_read_excel
*&---------------------------------------------------------------------*
*&      Form  f_populate_from_excel
*&---------------------------------------------------------------------*
FORM f_populate_from_excel USING po_excel TYPE REF TO zcl_excel.
  FIELD-SYMBOLS: <ls_data> LIKE LINE OF gt_data.
  DATA: lo_worksheet            TYPE REF TO zcl_excel_worksheet.
  DATA: l_title TYPE zexcel_sheet_title.
  DATA: l_row TYPE i,
        l_col TYPE i.
  DATA: l_value TYPE zexcel_cell_value,
        l_rc TYPE sysubrc,
        l_lenum_temp TYPE lenum VALUE IS INITIAL .
  DATA: BEGIN OF ls_col,
         matnr TYPE i,
         raube TYPE i,"
         tragr TYPE i,"mara-tragr, "grp transp
         mhdrz TYPE i,"mara-mhdrz, "tmp val rest
         iprkz TYPE i,"mara-iprkz, "cod per valid
         mhdlp TYPE i,"mara-mhdlp, "% p armaz
         werks TYPE i,"marc-werks,
         plifz TYPE i,"marc-plifz, "prz entreg
         ladgr TYPE i,"marc-ladgr, "grp carreg
         lgfsb TYPE i,"marc-lgfsb, "dep armaz
         mtvfp TYPE i,"marc-mtvfp, "verif disp
         fprfm TYPE i,"marc-fprfm, "perfil distribuição
         lgnum TYPE i,"mlgn-lgnum,
         lvsme TYPE i,"mlgn-lvsme, "um wm
         plkpt TYPE i,"mlgn-plkpt, "tp dep plan
         lhmg1 TYPE i,"mlgn-lhmg1, "qt mac
         lhme1 TYPE i,"mlgn-lhme1,
         lety1 TYPE i,"mlgn-lety1,
         ltkza TYPE i,"mlgn-ltkza, "est entra
         lgbkz TYPE i,"mlgn-lgbkz,
         ltkze TYPE i,"mlgn-ltkze, "est said
         lgtyp TYPE i,"mlgt-lgtyp, "tp dep
         lgpla TYPE i,"mlgt-lgpla, "pos pick
         lpmax TYPE i,"mlgt-lpmax, "min
         lpmin TYPE i,"mlgt-lpmin, "max
         mamng TYPE i,"mlgt-mamng, "manej
         nsmng TYPE i,"mlgt-nsmng, "
         ausme TYPE i,
        END OF ls_col.
  FIELD-SYMBOLS: <l_comp>.
  DATA: l_empty_col.
  DATA: ls_data TYPE t_data.

  lo_worksheet = po_excel->get_active_worksheet( ).

  l_row = 2. "2nd header
  l_col = 1.
*map colunms
  DO.
    TRY.
        CALL METHOD lo_worksheet->get_cell
          EXPORTING
            ip_column = l_col
            ip_row    = l_row
          IMPORTING
            ep_value  = l_value
            ep_rc     = l_rc.
      CATCH zcx_excel .
        MESSAGE e899(mm) WITH 'Erro a ler célula' l_row l_col.
    ENDTRY.
    IF l_rc NE 0.
      EXIT.
    ENDIF.
    IF l_value IS INITIAL.
*      exit.
    ELSE.
      ASSIGN COMPONENT l_value OF STRUCTURE ls_col TO <l_comp>.
      IF sy-subrc EQ 0.
        <l_comp> = l_col.
      ENDIF.
    ENDIF.

    CLEAR l_empty_col.
    DO.
      ASSIGN COMPONENT sy-index OF STRUCTURE ls_col TO <l_comp>.
      IF sy-subrc NE 0.
        EXIT.
      ELSE.
        IF <l_comp> IS INITIAL.
          l_empty_col = 'X'.
        ENDIF.
      ENDIF.
    ENDDO.
    IF l_empty_col IS INITIAL.
      EXIT.
    ENDIF.
    IF l_col GT 9999. "max search
      EXIT.
    ENDIF.
    ADD 1 TO l_col.
  ENDDO.
  IF l_empty_col IS NOT INITIAL.
    MESSAGE e899(mm) WITH 'Formato de ficheiro' 'inválido'.
  ELSE.


*go
    DO.
      ADD 1 TO l_row.
      CLEAR ls_data.

      l_col = ls_col-matnr.
      TRY.
          CALL METHOD lo_worksheet->get_cell
            EXPORTING
              ip_column = l_col
              ip_row    = l_row
            IMPORTING
              ep_value  = l_value
              ep_rc     = l_rc.
        CATCH zcx_excel .
          MESSAGE e899(mm) WITH 'Erro a ler célula' l_row l_col.
      ENDTRY.
      IF l_rc NE 0.
        EXIT.
      ENDIF.
      IF l_value IS INITIAL.
        EXIT.
      ELSE.
        CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
          EXPORTING
            input        = l_value
          IMPORTING
            output       = ls_data-matnr
          EXCEPTIONS
            length_error = 1
            OTHERS       = 2.
      ENDIF.

      l_col = ls_col-werks.
      TRY.
          CALL METHOD lo_worksheet->get_cell
            EXPORTING
              ip_column = l_col
              ip_row    = l_row
            IMPORTING
              ep_value  = l_value
              ep_rc     = l_rc.
        CATCH zcx_excel .
          MESSAGE e899(mm) WITH 'Erro a ler célula' l_row l_col.
      ENDTRY.
      IF l_rc NE 0.
        EXIT.
      ENDIF.
      IF l_value IS INITIAL.
        EXIT.
      ELSE.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = l_value
          IMPORTING
            output = ls_data-werks.
      ENDIF.


      l_col = ls_col-ausme.
      TRY.
          CALL METHOD lo_worksheet->get_cell
            EXPORTING
              ip_column = l_col
              ip_row    = l_row
            IMPORTING
              ep_value  = l_value
              ep_rc     = l_rc.
        CATCH zcx_excel .
          MESSAGE e899(mm) WITH 'Erro a ler célula' l_row l_col.
      ENDTRY.
      IF l_rc NE 0.
        EXIT.
      ENDIF.
      IF l_value IS INITIAL.
        EXIT.
      ELSE.
        CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
          EXPORTING
            input          = l_value
          IMPORTING
            output         = ls_data-ausme
          EXCEPTIONS
            unit_not_found = 1
            OTHERS         = 2.
      ENDIF.

      l_col = ls_col-lgnum.
      IF l_col IS NOT INITIAL.
        TRY.
            CALL METHOD lo_worksheet->get_cell
              EXPORTING
                ip_column = l_col
                ip_row    = l_row
              IMPORTING
                ep_value  = l_value
                ep_rc     = l_rc.
          CATCH zcx_excel .
            MESSAGE e899(mm) WITH 'Erro a ler célula' l_row l_col.
        ENDTRY.
        IF l_rc NE 0.
          EXIT.
        ENDIF.
        IF l_value IS INITIAL.
*        EXIT.
        ELSE.
*          ls_data-lgnum = l_value.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = l_value
            IMPORTING
              output = ls_data-lgnum.
        ENDIF.
      ENDIF.

      l_col = ls_col-lgtyp.
      IF l_col IS NOT INITIAL.
        TRY.
            CALL METHOD lo_worksheet->get_cell
              EXPORTING
                ip_column = l_col
                ip_row    = l_row
              IMPORTING
                ep_value  = l_value
                ep_rc     = l_rc.
          CATCH zcx_excel .
            MESSAGE e899(mm) WITH 'Erro a ler célula' l_row l_col.
        ENDTRY.
        IF l_rc NE 0.
          EXIT.
        ENDIF.
        IF l_value IS INITIAL.
*        EXIT.
        ELSE.
*          ls_data-lgtyp = l_value.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = l_value
            IMPORTING
              output = ls_data-lgtyp.
        ENDIF.
      ENDIF.

*data
      l_col = ls_col-raube.
      IF l_col IS NOT INITIAL.
        TRY.
            CALL METHOD lo_worksheet->get_cell
              EXPORTING
                ip_column = l_col
                ip_row    = l_row
              IMPORTING
                ep_value  = l_value
                ep_rc     = l_rc.
          CATCH zcx_excel .
            MESSAGE e899(mm) WITH 'Erro a ler célula' l_row l_col.
        ENDTRY.
        IF l_rc NE 0.
          EXIT.
        ENDIF.
        IF l_value IS INITIAL.
*        EXIT.
        ELSE.
*          ls_data-raube = l_value.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = l_value
            IMPORTING
              output = ls_data-raube.

        ENDIF.
      ENDIF.

      l_col = ls_col-tragr.
      IF l_col IS NOT INITIAL.
        TRY.
            CALL METHOD lo_worksheet->get_cell
              EXPORTING
                ip_column = l_col
                ip_row    = l_row
              IMPORTING
                ep_value  = l_value
                ep_rc     = l_rc.
          CATCH zcx_excel .
            MESSAGE e899(mm) WITH 'Erro a ler célula' l_row l_col.
        ENDTRY.
        IF l_rc NE 0.
          EXIT.
        ENDIF.
        IF l_value IS INITIAL.
*        EXIT.
        ELSE.
*          ls_data-tragr = l_value.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = l_value
            IMPORTING
              output = ls_data-tragr.

        ENDIF.
      ENDIF.

      l_col = ls_col-mhdrz.
      IF l_col IS NOT INITIAL.
        TRY.
            CALL METHOD lo_worksheet->get_cell
              EXPORTING
                ip_column = l_col
                ip_row    = l_row
              IMPORTING
                ep_value  = l_value
                ep_rc     = l_rc.
          CATCH zcx_excel .
            MESSAGE e899(mm) WITH 'Erro a ler célula' l_row l_col.
        ENDTRY.
        IF l_rc NE 0.
          EXIT.
        ENDIF.
        IF l_value IS INITIAL.
*        EXIT.
        ELSE.
          ls_data-mhdrz = l_value.
        ENDIF.
      ENDIF.

      l_col = ls_col-iprkz.
      IF l_col IS NOT INITIAL.
        TRY.
            CALL METHOD lo_worksheet->get_cell
              EXPORTING
                ip_column = l_col
                ip_row    = l_row
              IMPORTING
                ep_value  = l_value
                ep_rc     = l_rc.
          CATCH zcx_excel .
            MESSAGE e899(mm) WITH 'Erro a ler célula' l_row l_col.
        ENDTRY.
        IF l_rc NE 0.
          EXIT.
        ENDIF.
        IF l_value IS INITIAL.
*        EXIT.
        ELSE.
          ls_data-iprkz = l_value.
        ENDIF.
      ENDIF.

      l_col = ls_col-mhdlp.
      IF l_col IS NOT INITIAL.
        TRY.
            CALL METHOD lo_worksheet->get_cell
              EXPORTING
                ip_column = l_col
                ip_row    = l_row
              IMPORTING
                ep_value  = l_value
                ep_rc     = l_rc.
          CATCH zcx_excel .
            MESSAGE e899(mm) WITH 'Erro a ler célula' l_row l_col.
        ENDTRY.
        IF l_rc NE 0.
          EXIT.
        ENDIF.
        IF l_value IS INITIAL.
*        EXIT.
        ELSE.
          ls_data-mhdlp = l_value.
        ENDIF.
      ENDIF.

      l_col = ls_col-plifz.
      IF l_col IS NOT INITIAL.
        TRY.
            CALL METHOD lo_worksheet->get_cell
              EXPORTING
                ip_column = l_col
                ip_row    = l_row
              IMPORTING
                ep_value  = l_value
                ep_rc     = l_rc.
          CATCH zcx_excel .
            MESSAGE e899(mm) WITH 'Erro a ler célula' l_row l_col.
        ENDTRY.
        IF l_rc NE 0.
          EXIT.
        ENDIF.
        IF l_value IS INITIAL.
*        EXIT.
        ELSE.
          ls_data-plifz = l_value.
        ENDIF.
      ENDIF.

      l_col = ls_col-ladgr.
      IF l_col IS NOT INITIAL.
        TRY.
            CALL METHOD lo_worksheet->get_cell
              EXPORTING
                ip_column = l_col
                ip_row    = l_row
              IMPORTING
                ep_value  = l_value
                ep_rc     = l_rc.
          CATCH zcx_excel .
            MESSAGE e899(mm) WITH 'Erro a ler célula' l_row l_col.
        ENDTRY.
        IF l_rc NE 0.
          EXIT.
        ENDIF.
        IF l_value IS INITIAL.
*        EXIT.
        ELSE.
*          ls_data-ladgr = l_value.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = l_value
            IMPORTING
              output = ls_data-ladgr.
        ENDIF.
      ENDIF.

      l_col = ls_col-lgfsb.
      IF l_col IS NOT INITIAL.
        TRY.
            CALL METHOD lo_worksheet->get_cell
              EXPORTING
                ip_column = l_col
                ip_row    = l_row
              IMPORTING
                ep_value  = l_value
                ep_rc     = l_rc.
          CATCH zcx_excel .
            MESSAGE e899(mm) WITH 'Erro a ler célula' l_row l_col.
        ENDTRY.
        IF l_rc NE 0.
          EXIT.
        ENDIF.
        IF l_value IS INITIAL.
*        EXIT.
        ELSE.
*          ls_data-lgfsb = l_value.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = l_value
            IMPORTING
              output = ls_data-lgfsb.

        ENDIF.
      ENDIF.

      l_col = ls_col-mtvfp.
      IF l_col IS NOT INITIAL.
        TRY.
            CALL METHOD lo_worksheet->get_cell
              EXPORTING
                ip_column = l_col
                ip_row    = l_row
              IMPORTING
                ep_value  = l_value
                ep_rc     = l_rc.
          CATCH zcx_excel .
            MESSAGE e899(mm) WITH 'Erro a ler célula' l_row l_col.
        ENDTRY.
        IF l_rc NE 0.
          EXIT.
        ENDIF.
        IF l_value IS INITIAL.
*        EXIT.
        ELSE.
*          ls_data-mtvfp = l_value.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = l_value
            IMPORTING
              output = ls_data-mtvfp.
        ENDIF.
      ENDIF.

      l_col = ls_col-fprfm.
      IF l_col IS NOT INITIAL.
        TRY.
            CALL METHOD lo_worksheet->get_cell
              EXPORTING
                ip_column = l_col
                ip_row    = l_row
              IMPORTING
                ep_value  = l_value
                ep_rc     = l_rc.
          CATCH zcx_excel .
            MESSAGE e899(mm) WITH 'Erro a ler célula' l_row l_col.
        ENDTRY.
        IF l_rc NE 0.
          EXIT.
        ENDIF.
        IF l_value IS INITIAL.
*        EXIT.
        ELSE.
*          ls_data-fprfm = l_value.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = l_value
            IMPORTING
              output = ls_data-fprfm.
        ENDIF.
      ENDIF.

      l_col = ls_col-lgnum.
      IF l_col IS NOT INITIAL.
        TRY.
            CALL METHOD lo_worksheet->get_cell
              EXPORTING
                ip_column = l_col
                ip_row    = l_row
              IMPORTING
                ep_value  = l_value
                ep_rc     = l_rc.
          CATCH zcx_excel .
            MESSAGE e899(mm) WITH 'Erro a ler célula' l_row l_col.
        ENDTRY.
        IF l_rc NE 0.
          EXIT.
        ENDIF.
        IF l_value IS INITIAL.
*        EXIT.
        ELSE.
*          ls_data-lgnum = l_value.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = l_value
            IMPORTING
              output = ls_data-lgnum.
        ENDIF.
      ENDIF.

      l_col = ls_col-lvsme.
      IF l_col IS NOT INITIAL.
        TRY.
            CALL METHOD lo_worksheet->get_cell
              EXPORTING
                ip_column = l_col
                ip_row    = l_row
              IMPORTING
                ep_value  = l_value
                ep_rc     = l_rc.
          CATCH zcx_excel .
            MESSAGE e899(mm) WITH 'Erro a ler célula' l_row l_col.
        ENDTRY.
        IF l_rc NE 0.
          EXIT.
        ENDIF.
        IF l_value IS INITIAL.
*        EXIT.
        ELSE.
          CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
            EXPORTING
              input          = l_value
            IMPORTING
              output         = ls_data-lvsme
            EXCEPTIONS
              unit_not_found = 1
              OTHERS         = 2.
        ENDIF.
      ENDIF.

      l_col = ls_col-plkpt.
      IF l_col IS NOT INITIAL.
        TRY.
            CALL METHOD lo_worksheet->get_cell
              EXPORTING
                ip_column = l_col
                ip_row    = l_row
              IMPORTING
                ep_value  = l_value
                ep_rc     = l_rc.
          CATCH zcx_excel .
            MESSAGE e899(mm) WITH 'Erro a ler célula' l_row l_col.
        ENDTRY.
        IF l_rc NE 0.
          EXIT.
        ENDIF.
        IF l_value IS INITIAL.
*        EXIT.
        ELSE.
*          ls_data-plkpt = l_value.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = l_value
            IMPORTING
              output = ls_data-plkpt.
        ENDIF.
      ENDIF.

      l_col = ls_col-lhmg1.
      IF l_col IS NOT INITIAL.
        TRY.
            CALL METHOD lo_worksheet->get_cell
              EXPORTING
                ip_column = l_col
                ip_row    = l_row
              IMPORTING
                ep_value  = l_value
                ep_rc     = l_rc.
          CATCH zcx_excel .
            MESSAGE e899(mm) WITH 'Erro a ler célula' l_row l_col.
        ENDTRY.
        IF l_rc NE 0.
          EXIT.
        ENDIF.
        IF l_value IS INITIAL.
*        EXIT.
        ELSE.
          ls_data-lhmg1 = l_value.
        ENDIF.
      ENDIF.

      l_col = ls_col-lhme1.
      IF l_col IS NOT INITIAL.
        TRY.
            CALL METHOD lo_worksheet->get_cell
              EXPORTING
                ip_column = l_col
                ip_row    = l_row
              IMPORTING
                ep_value  = l_value
                ep_rc     = l_rc.
          CATCH zcx_excel .
            MESSAGE e899(mm) WITH 'Erro a ler célula' l_row l_col.
        ENDTRY.
        IF l_rc NE 0.
          EXIT.
        ENDIF.
        IF l_value IS INITIAL.
*        EXIT.
        ELSE.
          CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
            EXPORTING
              input          = l_value
            IMPORTING
              output         = ls_data-lhme1
            EXCEPTIONS
              unit_not_found = 1
              OTHERS         = 2.
        ENDIF.
      ENDIF.

      l_col = ls_col-lety1.
      IF l_col IS NOT INITIAL.
        TRY.
            CALL METHOD lo_worksheet->get_cell
              EXPORTING
                ip_column = l_col
                ip_row    = l_row
              IMPORTING
                ep_value  = l_value
                ep_rc     = l_rc.
          CATCH zcx_excel .
            MESSAGE e899(mm) WITH 'Erro a ler célula' l_row l_col.
        ENDTRY.
        IF l_rc NE 0.
          EXIT.
        ENDIF.
        IF l_value IS INITIAL.
*        EXIT.
        ELSE.
*          ls_data-lety1 = l_value.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = l_value
            IMPORTING
              output = ls_data-lety1.
        ENDIF.
      ENDIF.

      l_col = ls_col-ltkza.
      IF l_col IS NOT INITIAL.
        TRY.
            CALL METHOD lo_worksheet->get_cell
              EXPORTING
                ip_column = l_col
                ip_row    = l_row
              IMPORTING
                ep_value  = l_value
                ep_rc     = l_rc.
          CATCH zcx_excel .
            MESSAGE e899(mm) WITH 'Erro a ler célula' l_row l_col.
        ENDTRY.
        IF l_rc NE 0.
          EXIT.
        ENDIF.
        IF l_value IS INITIAL.
*        EXIT.
        ELSE.
*          ls_data-ltkza = l_value.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = l_value
            IMPORTING
              output = ls_data-ltkza.
        ENDIF.
      ENDIF.

      l_col = ls_col-lgbkz.
      IF l_col IS NOT INITIAL.
        TRY.
            CALL METHOD lo_worksheet->get_cell
              EXPORTING
                ip_column = l_col
                ip_row    = l_row
              IMPORTING
                ep_value  = l_value
                ep_rc     = l_rc.
          CATCH zcx_excel .
            MESSAGE e899(mm) WITH 'Erro a ler célula' l_row l_col.
        ENDTRY.
        IF l_rc NE 0.
          EXIT.
        ENDIF.
        IF l_value IS INITIAL.
*        EXIT.
        ELSE.
*          ls_data-lgbkz = l_value.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = l_value
            IMPORTING
              output = ls_data-lgbkz.
        ENDIF.
      ENDIF.

      l_col = ls_col-ltkze.
      IF l_col IS NOT INITIAL.
        TRY.
            CALL METHOD lo_worksheet->get_cell
              EXPORTING
                ip_column = l_col
                ip_row    = l_row
              IMPORTING
                ep_value  = l_value
                ep_rc     = l_rc.
          CATCH zcx_excel .
            MESSAGE e899(mm) WITH 'Erro a ler célula' l_row l_col.
        ENDTRY.
        IF l_rc NE 0.
          EXIT.
        ENDIF.
        IF l_value IS INITIAL.
*        EXIT.
        ELSE.
*          ls_data-ltkze = l_value.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = l_value
            IMPORTING
              output = ls_data-ltkze.
        ENDIF.
      ENDIF.

      l_col = ls_col-lgpla.
      IF l_col IS NOT INITIAL.
        TRY.
            CALL METHOD lo_worksheet->get_cell
              EXPORTING
                ip_column = l_col
                ip_row    = l_row
              IMPORTING
                ep_value  = l_value
                ep_rc     = l_rc.
          CATCH zcx_excel .
            MESSAGE e899(mm) WITH 'Erro a ler célula' l_row l_col.
        ENDTRY.
        IF l_rc NE 0.
          EXIT.
        ENDIF.
        IF l_value IS INITIAL.
*        EXIT.
        ELSE.
          ls_data-lgpla = l_value.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = l_value
            IMPORTING
              output = ls_data-lgpla.
        ENDIF.
      ENDIF.

      l_col = ls_col-lpmax.
      IF l_col IS NOT INITIAL.
        TRY.
            CALL METHOD lo_worksheet->get_cell
              EXPORTING
                ip_column = l_col
                ip_row    = l_row
              IMPORTING
                ep_value  = l_value
                ep_rc     = l_rc.
          CATCH zcx_excel .
            MESSAGE e899(mm) WITH 'Erro a ler célula' l_row l_col.
        ENDTRY.
        IF l_rc NE 0.
          EXIT.
        ENDIF.
        IF l_value IS INITIAL.
*        EXIT.
        ELSE.
          ls_data-lpmax = l_value.
        ENDIF.
      ENDIF.

      l_col = ls_col-lpmin.
      IF l_col IS NOT INITIAL.
        TRY.
            CALL METHOD lo_worksheet->get_cell
              EXPORTING
                ip_column = l_col
                ip_row    = l_row
              IMPORTING
                ep_value  = l_value
                ep_rc     = l_rc.
          CATCH zcx_excel .
            MESSAGE e899(mm) WITH 'Erro a ler célula' l_row l_col.
        ENDTRY.
        IF l_rc NE 0.
          EXIT.
        ENDIF.
        IF l_value IS INITIAL.
*        EXIT.
        ELSE.
          ls_data-lpmin = l_value.
        ENDIF.
      ENDIF.

      l_col = ls_col-mamng.
      IF l_col IS NOT INITIAL.
        TRY.
            CALL METHOD lo_worksheet->get_cell
              EXPORTING
                ip_column = l_col
                ip_row    = l_row
              IMPORTING
                ep_value  = l_value
                ep_rc     = l_rc.
          CATCH zcx_excel .
            MESSAGE e899(mm) WITH 'Erro a ler célula' l_row l_col.
        ENDTRY.
        IF l_rc NE 0.
          EXIT.
        ENDIF.
        IF l_value IS INITIAL.
*        EXIT.
        ELSE.
          ls_data-mamng = l_value.
        ENDIF.
      ENDIF.

      l_col = ls_col-nsmng.
      IF l_col IS NOT INITIAL.
        TRY.
            CALL METHOD lo_worksheet->get_cell
              EXPORTING
                ip_column = l_col
                ip_row    = l_row
              IMPORTING
                ep_value  = l_value
                ep_rc     = l_rc.
          CATCH zcx_excel .
            MESSAGE e899(mm) WITH 'Erro a ler célula' l_row l_col.
        ENDTRY.
        IF l_rc NE 0.
          EXIT.
        ENDIF.
        IF l_value IS INITIAL.
*        EXIT.
        ELSE.
          ls_data-nsmng = l_value.
        ENDIF.
      ENDIF.


*here we go!
      READ TABLE gt_data ASSIGNING <ls_data> WITH KEY matnr = ls_data-matnr werks = ls_data-werks lgnum = ls_data-lgnum lgtyp = ls_data-lgtyp.
      IF sy-subrc NE 0.
        APPEND INITIAL LINE TO gt_data ASSIGNING <ls_data>.
        <ls_data>-matnr = ls_data-matnr.
        <ls_data>-werks = ls_data-werks.
        <ls_data>-lgnum = ls_data-lgnum.
        <ls_data>-lgtyp = ls_data-lgtyp.
        SELECT SINGLE * FROM marc INTO CORRESPONDING FIELDS OF <ls_data> WHERE matnr = ls_data-matnr AND werks = ls_data-werks.
        SELECT SINGLE * FROM mlgn INTO CORRESPONDING FIELDS OF <ls_data> WHERE matnr = ls_data-matnr AND lgnum = ls_data-lgnum.
        SELECT SINGLE * FROM mlgt INTO CORRESPONDING FIELDS OF <ls_data> WHERE matnr = ls_data-matnr AND lgnum = ls_data-lgnum AND lgtyp = ls_data-lgtyp.
        SELECT SINGLE maktx INTO <ls_data>-maktx FROM makt WHERE matnr EQ ls_data-matnr AND spras EQ sy-langu.
      ENDIF.
      <ls_data>-raube = ls_data-raube.
      <ls_data>-tragr = ls_data-tragr.
      <ls_data>-mhdrz = ls_data-mhdrz.
      <ls_data>-iprkz = ls_data-iprkz.
      <ls_data>-mhdlp = ls_data-mhdlp.
      <ls_data>-plifz = ls_data-plifz.
      <ls_data>-ladgr = ls_data-ladgr.
      <ls_data>-lgfsb = ls_data-lgfsb.
      <ls_data>-mtvfp = ls_data-mtvfp.
      <ls_data>-fprfm = ls_data-fprfm.
      <ls_data>-lvsme = ls_data-lvsme.
      <ls_data>-plkpt = ls_data-plkpt.
      <ls_data>-lhmg1 = ls_data-lhmg1.
      <ls_data>-lhme1 = ls_data-lhme1.
      <ls_data>-lety1 = ls_data-lety1.
      <ls_data>-ltkza = ls_data-ltkza.
      <ls_data>-lgbkz = ls_data-lgbkz.
      <ls_data>-ltkze = ls_data-ltkze.
      <ls_data>-lgpla = ls_data-lgpla.
      <ls_data>-lpmax = ls_data-lpmax.
      <ls_data>-lpmin = ls_data-lpmin.
      <ls_data>-mamng = ls_data-mamng.
      <ls_data>-nsmng = ls_data-nsmng.

    ENDDO.

  ENDIF.
ENDFORM.                    "f_populate_from_excel
*&---------------------------------------------------------------------*
*&      Form  F_UPDATE_MATERIAL
*&---------------------------------------------------------------------*
FORM f_update_material  USING    ps_mara TYPE t_mara
                                 ps_return TYPE bapireturn1.
  FIELD-SYMBOLS: <ls_marc> TYPE t_marc,
                 <ls_mlgn> TYPE t_mlgn,
                 <ls_mlgt> TYPE t_mlgt,
                 <ls_data> TYPE t_data.
  DATA: l_destination(30).
  DATA: ls_head TYPE bapie1mathead,
        lt_client TYPE TABLE OF bapie1marart,
        lt_clientx TYPE TABLE OF bapie1marartx,
        lt_plant TYPE TABLE OF bapie1marcrt,
        lt_plantx TYPE TABLE OF bapie1marcrtx,
        lt_whn TYPE TABLE OF bapie1mlgnrt,
        lt_whnx TYPE TABLE OF bapie1mlgnrtx,
        lt_wht TYPE TABLE OF bapie1mlgtrt,
        lt_whtx TYPE TABLE OF bapie1mlgtrtx.


  FIELD-SYMBOLS: <ls_client> TYPE bapie1marart.
  FIELD-SYMBOLS: <ls_clientx> TYPE bapie1marartx.
  FIELD-SYMBOLS: <ls_plant> TYPE bapie1marcrt.
  FIELD-SYMBOLS: <ls_plantx> TYPE bapie1marcrtx.
  FIELD-SYMBOLS: <ls_whn> TYPE bapie1mlgnrt.
  FIELD-SYMBOLS: <ls_whnx> TYPE bapie1mlgnrtx.
  FIELD-SYMBOLS: <ls_wht> TYPE bapie1mlgtrt.
  FIELD-SYMBOLS: <ls_whtx> TYPE bapie1mlgtrtx.

  DATA: ls_mara TYPE mara,
        ls_marc TYPE marc,
        ls_mlgn TYPE mlgn,
        ls_mlgt TYPE mlgt.

  CLEAR ps_return.
  CONCATENATE 'ERPMRP' sy-mandt INTO l_destination.
  ls_head-material = ps_mara-matnr.
  ls_head-logst_view = 'X'.
  ls_head-logdc_view = 'X'.
  ls_head-function = '004'.
  ls_head-no_appl_log = 'X'."p_no_log.


  SELECT SINGLE * INTO ls_mara FROM mara WHERE matnr EQ ps_mara-matnr.
  IF sy-subrc EQ 0.
    APPEND INITIAL LINE TO lt_client ASSIGNING <ls_client>.
    <ls_client>-function = '004'.
    <ls_client>-material = ps_mara-matnr.
    <ls_client>-changed_by = sy-uname.
    APPEND INITIAL LINE TO lt_clientx ASSIGNING <ls_clientx>.
    <ls_clientx>-function = '004'.
    <ls_clientx>-material = ps_mara-matnr.
    <ls_clientx>-changed_by = 'X'.

    IF ps_mara-raube NE ls_mara-raube.
      <ls_client>-stor_conds = ps_mara-raube.
      <ls_clientx>-stor_conds = 'X'.
    ENDIF.
    IF ps_mara-tragr NE ls_mara-tragr.
      <ls_client>-trans_grp = ps_mara-tragr.
      <ls_clientx>-trans_grp = 'X'.
    ENDIF.
    IF ps_mara-mhdrz NE ls_mara-mhdrz.
      <ls_client>-minremlife = ps_mara-mhdrz.
      <ls_clientx>-minremlife = 'X'.
    ENDIF.
    IF ps_mara-mhdrz NE ls_mara-mhdrz.
      <ls_client>-period_ind_expiration_date = ps_mara-iprkz.
      <ls_clientx>-period_ind_expiration_date = 'X'.
    ENDIF.
    IF ps_mara-mhdrz NE ls_mara-mhdrz.
      <ls_client>-stor_pct = ps_mara-mhdlp.
      <ls_clientx>-stor_pct = 'X'.
    ENDIF.

    LOOP AT ps_mara-marc ASSIGNING <ls_marc>.
      SELECT SINGLE * INTO ls_marc FROM marc WHERE matnr EQ ps_mara-matnr AND werks EQ <ls_marc>-werks.
      IF sy-subrc EQ 0.
        APPEND INITIAL LINE TO lt_plant ASSIGNING <ls_plant>.
        <ls_plant>-function = '004'.
        <ls_plant>-material = ps_mara-matnr.
        <ls_plant>-plant = <ls_marc>-werks.
        APPEND INITIAL LINE TO lt_plantx ASSIGNING <ls_plantx>.
        <ls_plantx>-function = '004'.
        <ls_plantx>-material = ps_mara-matnr.
        <ls_plantx>-plant = <ls_marc>-werks.

        IF <ls_marc>-plifz NE ls_marc-plifz.
          <ls_plant>-plnd_delry = <ls_marc>-plifz.
          <ls_plantx>-plnd_delry = 'X'.
        ENDIF.
        IF <ls_marc>-ladgr NE ls_marc-ladgr.
          <ls_plant>-loadinggrp = <ls_marc>-ladgr.
          <ls_plantx>-loadinggrp = 'X'.
        ENDIF.
        IF <ls_marc>-lgfsb NE ls_marc-lgfsb.
          <ls_plant>-sloc_exprc = <ls_marc>-lgfsb.
          <ls_plantx>-sloc_exprc = 'X'.
        ENDIF.
        IF <ls_marc>-mtvfp NE ls_marc-mtvfp.
          <ls_plant>-availcheck = <ls_marc>-mtvfp.
          <ls_plantx>-availcheck = 'X'.
        ENDIF.
        IF <ls_marc>-fprfm NE ls_marc-fprfm.
          <ls_plant>-distr_prof = <ls_marc>-fprfm.
          <ls_plantx>-distr_prof = 'X'.
        ENDIF.

        IF <ls_marc>-ausme NE ls_marc-ausme.
          <ls_plant>-issue_unit = <ls_marc>-ausme.
          <ls_plantx>-issue_unit = 'X'.
        ENDIF.

      ENDIF.
      IF <ls_plantx>+25 IS INITIAL.
        DELETE lt_plant WHERE plant EQ <ls_marc>-werks.
        DELETE lt_plantx WHERE plant EQ <ls_marc>-werks.
      ENDIF.
    ENDLOOP.

    LOOP AT ps_mara-mlgn ASSIGNING <ls_mlgn>.
      CLEAR ls_mlgn.
      SELECT SINGLE * INTO ls_mlgn FROM mlgn WHERE matnr EQ ps_mara-matnr AND lgnum EQ <ls_mlgn>-lgnum.

      APPEND INITIAL LINE TO lt_whn ASSIGNING <ls_whn>.
      <ls_whn>-function = '004'.
      <ls_whn>-material = ps_mara-matnr.
      <ls_whn>-whse_no = <ls_mlgn>-lgnum.
      APPEND INITIAL LINE TO lt_whnx ASSIGNING <ls_whnx>.
      <ls_whnx>-function = '004'.
      <ls_whnx>-material = ps_mara-matnr.
      <ls_whnx>-whse_no = <ls_mlgn>-lgnum.

      IF <ls_mlgn>-lvsme NE ls_mlgn-lvsme.
        <ls_whn>-wm_unit = <ls_mlgn>-lvsme.
        <ls_whnx>-wm_unit = 'X'.
      ENDIF.
      IF <ls_mlgn>-plkpt NE ls_mlgn-plkpt.
        <ls_whn>-stge_type = <ls_mlgn>-plkpt.
        <ls_whnx>-stge_type = 'X'.
      ENDIF.
      IF <ls_mlgn>-lhmg1 NE ls_mlgn-lhmg1.
        <ls_whn>-l_equip_1 = <ls_mlgn>-lhmg1.
        <ls_whnx>-l_equip_1 = 'X'.
      ENDIF.
      IF <ls_mlgn>-lhme1 NE ls_mlgn-lhme1.
        <ls_whn>-leq_unit_1 = <ls_mlgn>-lhme1.
        <ls_whnx>-leq_unit_1 = 'X'.
      ENDIF.
      IF <ls_mlgn>-lety1 NE ls_mlgn-lety1.
        <ls_whn>-unittype_1 = <ls_mlgn>-lety1.
        <ls_whnx>-unittype_1 = 'X'.
      ENDIF.
      IF <ls_mlgn>-ltkza NE ls_mlgn-ltkza.
        <ls_whn>-withdrawal = <ls_mlgn>-ltkza.
        <ls_whnx>-withdrawal = 'X'.
      ENDIF.
*      IF <ls_mlgn>-lgbkz NE ls_mlgn-lgbkz.
*        <ls_whn>-STGESECTOR = <ls_mlgn>-lgbkz.
*        <ls_whnx>-STGESECTOR = 'X'.
*      ENDIF.
      IF <ls_mlgn>-ltkze NE ls_mlgn-ltkze.
        <ls_whn>-placement = <ls_mlgn>-ltkze.
        <ls_whnx>-placement = 'X'.
      ENDIF.

      IF <ls_whnx>+24 IS INITIAL.
        DELETE lt_whn WHERE whse_no = <ls_mlgn>-lgnum.
        DELETE lt_whnx WHERE whse_no = <ls_mlgn>-lgnum.
      ENDIF.

      LOOP AT <ls_mlgn>-mlgt ASSIGNING <ls_mlgt>.
        CLEAR ls_mlgt.
        SELECT SINGLE * INTO ls_mlgt FROM mlgt WHERE matnr EQ ps_mara-matnr AND lgnum EQ <ls_mlgn>-lgnum AND lgtyp EQ <ls_mlgt>-lgtyp.

        APPEND INITIAL LINE TO lt_wht ASSIGNING <ls_wht>.
        <ls_wht>-function = '004'.
        <ls_wht>-material = ps_mara-matnr.
        <ls_wht>-whse_no = <ls_mlgn>-lgnum.
        <ls_wht>-stge_type = <ls_mlgt>-lgtyp.
        APPEND INITIAL LINE TO lt_whtx ASSIGNING <ls_whtx>.
        <ls_whtx>-function = '004'.
        <ls_whtx>-material = ps_mara-matnr.
        <ls_whtx>-whse_no = <ls_mlgn>-lgnum.
        <ls_whtx>-stge_type = <ls_mlgt>-lgtyp.

        IF <ls_mlgt>-lgpla NE ls_mlgt-lgpla.
          <ls_wht>-stge_bin = <ls_mlgt>-lgpla.
          <ls_whtx>-stge_bin = 'X'.
        ENDIF.
        IF <ls_mlgt>-lpmax NE ls_mlgt-lpmax.
          <ls_wht>-max_sb_qty = <ls_mlgt>-lpmax.
          <ls_whtx>-max_sb_qty = 'X'.
        ENDIF.
        IF <ls_mlgt>-lpmin NE ls_mlgt-lpmin.
          <ls_wht>-min_sb_qty = <ls_mlgt>-lpmin.
          <ls_whtx>-min_sb_qty = 'X'.
        ENDIF.
        IF <ls_mlgt>-mamng NE ls_mlgt-mamng.
          <ls_wht>-ctrl_qty = <ls_mlgt>-mamng.
          <ls_whtx>-ctrl_qty = 'X'.
        ENDIF.
        IF <ls_mlgt>-nsmng NE ls_mlgt-nsmng.
          <ls_wht>-replen_qty = <ls_mlgt>-nsmng.
          <ls_whtx>-replen_qty = 'X'.
        ENDIF.

        IF <ls_whtx>+27 IS INITIAL.
          DELETE lt_wht WHERE whse_no = <ls_mlgn>-lgnum AND stge_type = <ls_mlgt>-lgtyp.
          DELETE lt_wht WHERE whse_no = <ls_mlgn>-lgnum AND stge_type = <ls_mlgt>-lgtyp.
        ENDIF.

      ENDLOOP.

    ENDLOOP.

    IF lt_plant IS INITIAL AND lt_whn IS INITIAL AND lt_wht IS INITIAL AND <ls_clientx>+25 IS INITIAL.
      DELETE lt_client WHERE material EQ ps_mara-matnr.
      DELETE lt_clientx WHERE material EQ ps_mara-matnr.
    ENDIF.

    IF lt_client IS NOT INITIAL.
      CALL FUNCTION 'BAPI_MATERIAL_MAINTAINDATA_RT'
        DESTINATION l_destination
        EXPORTING
          headdata             = ls_head
        IMPORTING
          return               = ps_return
        TABLES
          clientdata           = lt_client
          clientdatax          = lt_clientx
          plantdata            = lt_plant
          plantdatax           = lt_plantx
          warehousenumberdata  = lt_whn
          warehousenumberdatax = lt_whnx
          storagetypedata      = lt_wht
          storagetypedatax     = lt_whtx.
*     STORAGELOCATIONDATA        =
*     STORAGELOCATIONDATAX       =
*     UNITSOFMEASURE             =
*     UNITSOFMEASUREX            =
*     INTERNATIONALARTNOS        =
      IF ps_return-type EQ 'E'.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'
          DESTINATION l_destination.
      ELSE.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          DESTINATION l_destination.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.                    " F_UPDATE_MATERIAL
*&---------------------------------------------------------------------*
*&      Form  F_SET_SCR_2010
*&---------------------------------------------------------------------*
FORM f_set_scr_2010 .
  DATA: l_field(40).
  FIELD-SYMBOLS: <l_field>.
  DATA: lt_result TYPE match_result_tab.
  FIELD-SYMBOLS <ls_result> LIKE LINE OF lt_result.
  LOOP AT SCREEN.
    CLEAR lt_result[].
    FIND FIRST OCCURRENCE OF '-' IN screen-name RESULTS lt_result.
    READ TABLE lt_result ASSIGNING <ls_result> INDEX 1.
    IF sy-subrc EQ 0.
      ADD 1 TO <ls_result>-offset.
      l_field = screen-name+<ls_result>-offset.
      ASSIGN COMPONENT l_field OF STRUCTURE gs_2010-columns TO <l_field>.
      IF <l_field> IS INITIAL.
        screen-input = '0'.
        screen-output = '0'.
        screen-invisible = '1'.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.


  ENDLOOP.


ENDFORM.                    " F_SET_SCR_2010
