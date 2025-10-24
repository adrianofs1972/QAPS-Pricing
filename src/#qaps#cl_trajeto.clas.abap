class /QAPS/CL_TRAJETO definition
  public
  final
  create public .

public section.

  methods GET_CAMINHO_MINIMO
    importing
      !IV_START_NODE type STRING
      !IV_END_NODE type STRING
    returning
      value(RETURN) type /QAPS/T_TRAJETO_MINIMO .
protected section.
PRIVATE SECTION.

  TYPES: BEGIN OF ty_edge,
           node   TYPE string,
           weight TYPE i,
         END OF ty_edge,
         ty_graph TYPE TABLE OF ty_edge.

  DATA: mt_graph          TYPE ty_graph,
        mt_distances      TYPE TABLE OF ty_edge,
        mt_previous_nodes TYPE TABLE OF ty_edge.
ENDCLASS.



CLASS /QAPS/CL_TRAJETO IMPLEMENTATION.


METHOD get_caminho_minimo.

  DATA: lv_current_node TYPE string,
        lv_weight       TYPE i,
        lv_min_distance TYPE i,
        lv_node         TYPE string.

  " Inicializa distâncias
  LOOP AT mt_graph INTO DATA(ls_edge).
    mt_distances[ ls_edge-node ]-node = ls_edge-node.
    mt_distances[ ls_edge-node ]-weight = 9999999. " Infinito
  ENDLOOP.

  mt_distances[ iv_start_node ]-weight = 0.

  " Inicializa lista de nós visitados
  DATA lt_visited_nodes TYPE /qaps/t_trajeto_minimo.

  WHILE lines( mt_distances ) > 0.
    " Encontra o nó com a menor distância
    CLEAR lv_min_distance.
    lv_min_distance = 9999999. " Infinito

    LOOP AT mt_distances INTO DATA(ls_distance).
      IF ls_distance-weight < lv_min_distance.
        lv_min_distance = ls_distance-weight.
        lv_current_node = ls_distance-node.
      ENDIF.
    ENDLOOP.

    " Marca o nó como visitado
    APPEND lv_current_node TO lt_visited_nodes.
    DELETE mt_distances WHERE node = lv_current_node.

    " Atualiza distâncias dos vizinhos
    LOOP AT mt_graph INTO ls_edge WHERE node = lv_current_node.
      READ TABLE mt_distances INTO DATA(ls_neighbor) WITH KEY node = ls_edge-node." TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        lv_weight = lv_min_distance + ls_edge-weight.
        IF lv_weight < ls_neighbor-weight.
          mt_distances[ ls_neighbor-node ]-weight = lv_weight.
          mt_previous_nodes[ ls_edge-node ]-node = lv_current_node.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDWHILE.

  " Reconstrói o caminho mais curto
  DATA lt_path TYPE /qaps/t_trajeto_minimo.
  lv_node = iv_end_node.

  WHILE lv_node IS NOT INITIAL.
    APPEND lv_node TO lt_path.
    READ TABLE mt_previous_nodes INTO DATA(ls_prev) WITH KEY node = lv_node.
    lv_node = ls_prev-node.
  ENDWHILE.

  " Inverte o caminho
*    reverse lt_path.
  return = lt_path.

ENDMETHOD.
ENDCLASS.
