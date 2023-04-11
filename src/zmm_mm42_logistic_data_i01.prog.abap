*----------------------------------------------------------------------*
***INCLUDE ZMM_MM42_LOGISTIC_DATA_I01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_2000  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_2000 INPUT.
  CASE ok_code.
    WHEN 'EXIT' OR 'BACK' OR 'CANCEL'.
      CLEAR ok_code.
      LEAVE TO SCREEN 0.
    WHEN 'SAVE'.
      CLEAR ok_code.
*      perform f_check.
      PERFORM f_save.
    WHEN OTHERS.
      CLEAR ok_code.

      DATA: l_stable TYPE lvc_s_stbl.
      l_stable-row = 'X'.
      l_stable-col = 'X'.
      IF gr_alv IS NOT INITIAL.
        gr_alv->refresh_table_display( is_stable = l_stable )." i_soft_refresh = 'X' ).
      ENDIF.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_2000  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_2010  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_2010 INPUT.
  IF ok_code EQ 'OK' OR ok_code EQ 'KO'.
    LEAVE TO SCREEN 0.
  ENDIF.
ENDMODULE.                 " USER_COMMAND_2010  INPUT
