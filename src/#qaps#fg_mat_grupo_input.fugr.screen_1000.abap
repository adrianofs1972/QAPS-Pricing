PROCESS BEFORE OUTPUT.
*{   INSERT         ECDK9A0F42                                        1
  MODULE status_1000.
*
*}   INSERT
PROCESS AFTER INPUT.
*{   INSERT         ECDK9A0F42                                        1
  MODULE exit_command AT EXIT-COMMAND.
  MODULE user_command_1000.
*}   INSERT

*PROCESS ON VALUE-REQUEST.
*
*  FIELD /qaps/categ_trns-id_categoria MODULE create_dropdown_box.
