*&---------------------------------------------------------------------*
*& Report  ZMM_MM42_LOGISTIC_DATA
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT  zmm_mm42_logistic_data.

TYPE-POOLS: icon.
TABLES: mara, marc, mlgn, mlgt.
TYPES: BEGIN OF t_data,
         matkl TYPE mara-matkl,
         matnr TYPE mara-matnr,
         maktx TYPE makt-maktx,
         raube TYPE mara-raube, "condições de stockagem
         tragr TYPE mara-tragr, "grp transp
         mhdrz TYPE mara-mhdrz, "tmp val rest
         iprkz TYPE mara-iprkz, "cod per valid
         mhdlp TYPE mara-mhdlp, "% p armaz
         werks TYPE marc-werks,
         plifz TYPE marc-plifz, "prz entreg
         ladgr TYPE marc-ladgr, "grp carreg
         lgfsb TYPE marc-lgfsb, "dep armaz
         mtvfp TYPE marc-mtvfp, "verif disp
         fprfm TYPE marc-fprfm, "perfil distribuição
         ausme TYPE marc-ausme, "un saída
         lgnum TYPE mlgn-lgnum,
         lvsme TYPE mlgn-lvsme, "um wm
         plkpt TYPE mlgn-plkpt, "tp dep plan
         lhmg1 TYPE mlgn-lhmg1, "qt mac
         lhme1 TYPE mlgn-lhme1,
         lety1 TYPE mlgn-lety1,
         ltkze TYPE mlgn-ltkze, "est entrada
         lgbkz TYPE mlgn-lgbkz, "area armazenamento
         ltkza TYPE mlgn-ltkza, "est saida
         lgtyp TYPE mlgt-lgtyp, "tp dep
         lgpla TYPE lagp-lgpla, "pos pick
         lpmax TYPE mlgt-lpmax, "min
         lpmin TYPE mlgt-lpmin, "max
         mamng TYPE mlgt-mamng, "manej
         nsmng TYPE mlgt-nsmng, "
         meins TYPE mara-meins,
         stock_dep TYPE char1,
         stock_park TYPE char1,
         stock_ndep TYPE char1,

         color_tab TYPE lvc_t_scol,

       END OF t_data.
DATA: gt_data TYPE TABLE OF t_data.
DATA: ok_code TYPE sy-ucomm.
DATA: gr_alv TYPE REF TO cl_gui_alv_grid.
DATA: g_flg_changed.

TYPES: BEGIN OF t_styles,
         header_up      TYPE zexcel_cell_style,
         header_edit    TYPE zexcel_cell_style,
         header_down    TYPE zexcel_cell_style,
         normal         TYPE zexcel_cell_style,
         edit           TYPE zexcel_cell_style,
         red            TYPE zexcel_cell_style,
         light_red      TYPE zexcel_cell_style,
         green          TYPE zexcel_cell_style,
         yellow         TYPE zexcel_cell_style,
         light_blue     TYPE zexcel_cell_style,
       END OF t_styles.
DATA: s_styles TYPE t_styles.

TYPES: BEGIN OF t_marc,
         werks TYPE marc-werks,
         plifz TYPE marc-plifz, "prz entreg
         ladgr TYPE marc-ladgr, "grp carreg
         lgfsb TYPE marc-lgfsb, "dep armaz
         mtvfp TYPE marc-mtvfp, "verif disp
         fprfm TYPE marc-fprfm, "perfil distribuição
         ausme TYPE marc-ausme,
       END OF t_marc,
       BEGIN OF t_mlgt,
         lgtyp TYPE mlgt-lgtyp, "tp dep
         lgpla TYPE mlgt-lgpla, "pos pick
         lpmax TYPE mlgt-lpmax, "min
         lpmin TYPE mlgt-lpmin, "max
         mamng TYPE mlgt-mamng, "manej
         nsmng TYPE mlgt-nsmng, "
       END OF t_mlgt,
       BEGIN OF t_mlgn,
         lgnum TYPE mlgn-lgnum,
         lvsme TYPE mlgn-lvsme, "um wm
         plkpt TYPE mlgn-plkpt, "tp dep plan
         lhmg1 TYPE mlgn-lhmg1, "qt mac
         lhme1 TYPE mlgn-lhme1,
         lety1 TYPE mlgn-lety1,
         ltkza TYPE mlgn-ltkza, "est entra
         lgbkz TYPE mlgn-lgbkz,
         ltkze TYPE mlgn-ltkze, "est said
         mlgt TYPE t_mlgt OCCURS 0,
       END OF t_mlgn,
       BEGIN OF t_mara,
         matnr TYPE mara-matnr,
         raube TYPE mara-raube, "condições de stockagem
         tragr TYPE mara-tragr, "grp transp
         mhdrz TYPE mara-mhdrz, "tmp val rest
         iprkz TYPE mara-iprkz, "cod per valid
         mhdlp TYPE mara-mhdlp, "% p armaz
         marc TYPE t_marc OCCURS 0,
         mlgn TYPE t_mlgn OCCURS 0,
       END OF t_mara.

TYPES: BEGIN OF t_2010_cols,
         raube," TYPE mara-raube, "condições de stockagem
        tragr," TYPE mara-tragr,
        mhdrz," TYPE mara-mhdrz,
        iprkz," TYPE mara-iprkz,
        mhdlp," TYPE mara-mhdlp,
        plifz," TYPE marc-plifz,
        ladgr," TYPE marc-ladgr,
        lgfsb," TYPE marc-lgfsb,
        mtvfp," TYPE marc-mtvfp,
        fprfm," TYPE marc-fprfm,
        ausme," TYPE marc-ausme,

        lvsme," TYPE mlgn-lvsme,
        plkpt," TYPE mlgn-plkpt,
        lhmg1," TYPE mlgn-lhmg1,
        lhme1," TYPE mlgn-lhme1,
        lety1," TYPE mlgn-lety1,
        ltkza," TYPE mlgn-ltkza,
        ltkze," TYPE mlgn-ltkze,
        lgbkz," TYPE mlgn-lgbkz,
        lgpla," TYPE mlgt-lgpla,
        lpmax," TYPE mlgt-lpmax,
        lpmin," TYPE mlgt-lpmin,
        mamng," TYPE mlgt-mamng,
        nsmng," TYPE mlgt-nsmng,

       END OF t_2010_cols.
DATA: BEGIN OF gs_2010,
         raube TYPE mara-raube, "condições de stockagem
        tragr TYPE mara-tragr,
        mhdrz TYPE mara-mhdrz,
        iprkz TYPE mara-iprkz,
        mhdlp TYPE mara-mhdlp,
        plifz TYPE marc-plifz,
        ladgr TYPE marc-ladgr,
        lgfsb TYPE marc-lgfsb,
        mtvfp TYPE marc-mtvfp,
        fprfm TYPE marc-fprfm,
        ausme TYPE marc-ausme,

        lvsme TYPE mlgn-lvsme,
        plkpt TYPE mlgn-plkpt,
        lhmg1 TYPE mlgn-lhmg1,
        lhme1 TYPE mlgn-lhme1,
        lety1 TYPE mlgn-lety1,
        ltkza TYPE mlgn-ltkza,
        ltkze TYPE mlgn-ltkze,
        lgbkz TYPE mlgn-lgbkz,
        lgpla TYPE mlgt-lgpla,
        lpmax TYPE mlgt-lpmax,
        lpmin TYPE mlgt-lpmin,
        mamng TYPE mlgt-mamng,
        nsmng TYPE mlgt-nsmng,

        columns type t_2010_cols,

      END OF gs_2010.


SELECT-OPTIONS: s_matnr FOR mara-matnr,
                s_matkl FOR mara-matkl,
                s_werks FOR marc-werks,
                s_lgnum FOR mlgn-lgnum,
                s_plkpt FOR mlgn-plkpt.
PARAMETERS: p_plkpt AS CHECKBOX DEFAULT 'X'.
SELECT-OPTIONS: s_lgtyp FOR mlgt-lgtyp,
                s_lgpla FOR mlgt-lgpla.

SELECTION-SCREEN BEGIN OF BLOCK v0 WITH FRAME TITLE text-v01.
PARAMETERS p_var TYPE slis_vari.
SELECTION-SCREEN END OF BLOCK v0.

INCLUDE zmm_mm42_logistic_data_o01.
INCLUDE zmm_mm42_logistic_data_c01.
INCLUDE zmm_mm42_logistic_data_f01.
INCLUDE zmm_mm42_logistic_data_i01.

START-OF-SELECTION.
  PERFORM f_select.

END-OF-SELECTION.
  IF gt_data IS NOT INITIAL.
    IF sy-batch IS INITIAL. "repid EQ 'ZMM_MRP_REPORT'.
      CALL SCREEN 2000.
    ELSE.
      PERFORM f_list_only.
    ENDIF.
  ENDIF.
*&---------------------------------------------------------------------*
*&      Form  f_select
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_select.
  FIELD-SYMBOLS: <ls_data> TYPE t_data.

  IF p_plkpt IS INITIAL.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE gt_data FROM mara JOIN makt ON makt~matnr EQ mara~matnr
           JOIN marc ON marc~matnr EQ mara~matnr JOIN mlgn ON mlgn~matnr EQ mara~matnr JOIN mlgt ON mlgt~matnr EQ mlgn~matnr AND mlgt~lgnum EQ mlgn~lgnum
           JOIN t320 ON t320~lgnum EQ mlgn~lgnum AND t320~werks EQ marc~werks
           WHERE mara~matnr IN s_matnr AND mara~matkl IN s_matkl AND marc~werks IN s_werks
             AND mlgn~lgnum IN s_lgnum AND mlgn~plkpt IN s_plkpt
             AND mlgt~lgtyp IN s_lgtyp AND mlgt~lgpla IN s_lgpla
             AND makt~spras EQ sy-langu.
  ELSE.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE gt_data FROM mara JOIN makt ON makt~matnr EQ mara~matnr
           JOIN marc ON marc~matnr EQ mara~matnr JOIN mlgn ON mlgn~matnr EQ mara~matnr JOIN mlgt ON mlgt~matnr EQ mlgn~matnr AND mlgt~lgnum EQ mlgn~lgnum AND mlgt~lgtyp EQ mlgn~plkpt
           JOIN t320 ON t320~lgnum EQ mlgn~lgnum AND t320~werks EQ marc~werks
           WHERE mara~matnr IN s_matnr AND mara~matkl IN s_matkl AND marc~werks IN s_werks
             AND mlgn~lgnum IN s_lgnum AND mlgn~plkpt IN s_plkpt
             AND mlgt~lgtyp IN s_lgtyp AND mlgt~lgpla IN s_lgpla
             AND makt~spras EQ sy-langu.
  ENDIF.
  IF s_lgtyp IS INITIAL AND s_lgpla IS INITIAL.
    SELECT * APPENDING CORRESPONDING FIELDS OF TABLE gt_data FROM mara JOIN makt ON makt~matnr EQ mara~matnr
           JOIN marc ON marc~matnr EQ mara~matnr
           JOIN t320 ON t320~werks EQ marc~werks
           JOIN mlgn ON mlgn~matnr EQ mara~matnr AND t320~lgnum EQ mlgn~lgnum
           WHERE mara~matnr IN s_matnr AND mara~matkl IN s_matkl AND marc~werks IN s_werks
             AND mlgn~lgnum IN s_lgnum AND mlgn~plkpt IN s_plkpt
             AND NOT EXISTS ( SELECT * FROM mlgt WHERE matnr EQ mlgn~matnr AND lgnum EQ mlgn~lgnum )
             AND makt~spras EQ sy-langu.
    IF s_lgnum IS INITIAL AND s_plkpt IS INITIAL.
      SELECT * APPENDING CORRESPONDING FIELDS OF TABLE gt_data FROM mara JOIN makt ON makt~matnr EQ mara~matnr
             JOIN marc ON marc~matnr EQ mara~matnr
             WHERE mara~matnr IN s_matnr AND mara~matkl IN s_matkl AND marc~werks IN s_werks
             AND NOT EXISTS ( SELECT * FROM mlgn WHERE matnr EQ mara~matnr )
             AND EXISTS ( SELECT * FROM t320 JOIN t001w ON t001w~werks EQ t320~werks WHERE t320~werks EQ marc~werks AND t001w~vlfkz EQ 'B' )
             AND makt~spras EQ sy-langu.
    ENDIF.
  ENDIF.
  sort gt_data.
  delete ADJACENT DUPLICATES FROM gt_data.

*stocks?
  DATA: l_lgort TYPE mard-lgort.
  LOOP AT gt_data ASSIGNING <ls_data> WHERE lgnum IS NOT INITIAL AND werks IS NOT INITIAL.
    SELECT SINGLE mard~lgort INTO l_lgort FROM mard JOIN t320 ON t320~werks EQ mard~werks AND t320~lgort EQ mard~lgort
           WHERE t320~lgnum EQ <ls_data>-lgnum AND mard~werks EQ <ls_data>-werks AND matnr EQ <ls_data>-matnr
             AND ( labst NE 0 OR speme NE 0 OR insme NE 0 ).
    IF sy-subrc EQ 0.
      <ls_data>-stock_dep = 'X'.
    ENDIF.

    SELECT SINGLE mard~lgort INTO l_lgort FROM mard JOIN t001l ON t001l~werks EQ mard~werks AND t001l~lgort EQ mard~lgort
           WHERE
*      t001l~zzpark NE space AND
                 mard~werks EQ <ls_data>-werks AND matnr EQ <ls_data>-matnr
                 AND ( labst NE 0 OR speme NE 0 OR insme NE 0 )
                 AND mard~lgort NOT IN ( SELECT lgort FROM t320 WHERE werks EQ mard~werks AND lgnum EQ <ls_data>-lgnum ) .
    IF sy-subrc EQ 0.
      <ls_data>-stock_park = 'X'.
    ENDIF.

    SELECT SINGLE mard~lgort INTO l_lgort FROM mard JOIN t001l ON t001l~werks EQ mard~werks AND t001l~lgort EQ mard~lgort
           WHERE
*      t001l~zzpark EQ space AND
                 mard~werks EQ <ls_data>-werks AND matnr EQ <ls_data>-matnr
                 AND ( labst NE 0 OR speme NE 0 OR insme NE 0 )
                 AND mard~lgort NOT IN ( SELECT lgort FROM t320 WHERE werks EQ mard~werks AND lgnum EQ <ls_data>-lgnum ) .
    IF sy-subrc EQ 0.
      <ls_data>-stock_ndep = 'X'.
    ENDIF.

  ENDLOOP.
ENDFORM.                    "f_select
*&---------------------------------------------------------------------*
*&      Form  f_list_only
*&---------------------------------------------------------------------*
FORM f_list_only .
  DATA: gr_salv TYPE REF TO cl_salv_table.

  cl_salv_table=>factory(
    IMPORTING
      r_salv_table = gr_salv
    CHANGING
      t_table      = gt_data ).

  DATA: lr_functions TYPE REF TO cl_salv_functions_list.
  lr_functions = gr_salv->get_functions( ).
  lr_functions->set_all( abap_true ).
  lr_functions->set_default( abap_true ).
  DATA: lo_funcs TYPE salv_t_ui_func.
  DATA: lo_func TYPE salv_s_ui_func.
  lo_funcs = lr_functions->get_functions( ).
  LOOP AT lo_funcs INTO lo_func.
    lo_func-r_function->set_enable( value = 'X' ).
    lo_func-r_function->set_visible( value = 'X' ).
  ENDLOOP.

  DATA: lo_layout  TYPE REF TO cl_salv_layout,
        lf_variant TYPE slis_vari,
        ls_key    TYPE salv_s_layout_key.
  lo_layout = gr_salv->get_layout( ).
  ls_key-report = sy-repid.

  lo_layout->set_key( ls_key ).
  lo_layout->set_save_restriction( if_salv_c_layout=>restrict_none ).
  lo_layout->set_default( abap_true ).

*  lf_variant = p_var.
  lo_layout->set_initial_layout( lf_variant ).

  gr_salv->display( ).

ENDFORM.                    "f_list_only
*
