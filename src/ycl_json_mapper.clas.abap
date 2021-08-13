CLASS ycl_json_mapper DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS map2abap
      IMPORTING
        !io_json          TYPE REF TO /uif/cl_json_item
        !iv_boolean_false TYPE char1 OPTIONAL
      EXPORTING
        !ev_content       TYPE any
      RAISING
        /ixbx/zcx_bx_exception.
    CLASS-METHODS map_object2table
      IMPORTING
        !io_json       TYPE REF TO /uif/cl_json_object OPTIONAL
        !it_properties TYPE /ui5/json_node_set_t OPTIONAL
        !io_type       TYPE REF TO cl_abap_tabledescr
      EXPORTING
        !ev_content    TYPE any
      RAISING
        /ixbx/zcx_bx_exception.
  PRIVATE SECTION.
    CLASS-DATA gv_boolean_false TYPE char1.

    CLASS-METHODS map
      IMPORTING
        io_json    TYPE REF TO /uif/cl_json_item
        io_type    TYPE REF TO cl_abap_typedescr
      EXPORTING
        ev_content TYPE any
      RAISING
        /ixbx/zcx_bx_exception.

    CLASS-METHODS map2elem
      IMPORTING
        io_json    TYPE REF TO /uif/cl_json_item
        io_type    TYPE REF TO cl_abap_elemdescr
      EXPORTING
        ev_content TYPE any
      RAISING
        /ixbx/zcx_bx_exception.

    CLASS-METHODS map2struct
      IMPORTING
        io_json    TYPE REF TO /uif/cl_json_item
        io_type    TYPE REF TO cl_abap_structdescr
      EXPORTING
        ev_content TYPE any
      RAISING
        /ixbx/zcx_bx_exception.

    CLASS-METHODS map2table
      IMPORTING
        io_json    TYPE REF TO /uif/cl_json_item
        io_type    TYPE REF TO cl_abap_tabledescr
      EXPORTING
        ev_content TYPE any
      RAISING
        /ixbx/zcx_bx_exception.

    CLASS-METHODS map2class
      IMPORTING
        io_json    TYPE REF TO /uif/cl_json_item
        io_type    TYPE REF TO cl_abap_classdescr
      EXPORTING
        ev_content TYPE any
      RAISING
        /ixbx/zcx_bx_exception.

    CLASS-METHODS map_object2struct
      IMPORTING
        io_json       TYPE REF TO /uif/cl_json_object
        it_components TYPE cl_abap_structdescr=>component_table
      EXPORTING
        ev_content    TYPE any
      RAISING
        /ixbx/zcx_bx_exception.

    CLASS-METHODS map_array2table
      IMPORTING
        io_json    TYPE REF TO /uif/cl_json_array
        io_type    TYPE REF TO cl_abap_tabledescr
      EXPORTING
        ev_content TYPE any
      RAISING
        /ixbx/zcx_bx_exception.



    CLASS-METHODS map2ref
      IMPORTING
        io_json    TYPE REF TO /uif/cl_json_item
        io_type    TYPE REF TO cl_abap_refdescr
      EXPORTING
        ev_content TYPE any
      RAISING
        /ixbx/zcx_bx_exception .
ENDCLASS.



CLASS yCL_JSON_MAPPER IMPLEMENTATION.


  METHOD map.
    DATA: lo_type_elem   TYPE REF TO cl_abap_elemdescr,
          lo_type_struct TYPE REF TO cl_abap_structdescr,
          lo_type_table  TYPE REF TO cl_abap_tabledescr,
          lo_type_ref    TYPE REF TO cl_abap_refdescr.

    CASE io_type->kind.
      WHEN cl_abap_typedescr=>kind_elem.
        lo_type_elem ?= io_type.
        map2elem(
          EXPORTING
            io_json = io_json
            io_type = lo_type_elem
          IMPORTING
            ev_content = ev_content ).

      WHEN cl_abap_typedescr=>kind_struct.
        lo_type_struct ?= io_type.
        map2struct(
          EXPORTING
            io_json = io_json
            io_type = lo_type_struct
          IMPORTING
            ev_content = ev_content ).

      WHEN cl_abap_typedescr=>kind_table.
        lo_type_table ?= io_type.
        map2table(
          EXPORTING
            io_json = io_json
            io_type = lo_type_table
          IMPORTING
            ev_content = ev_content ).

      WHEN cl_abap_typedescr=>kind_ref.
        lo_type_ref ?= io_type.
        map2ref(
          EXPORTING
            io_json = io_json
            io_type = lo_type_ref
          IMPORTING
            ev_content = ev_content ).

    ENDCASE.
  ENDMETHOD.                    "MAP


  METHOD map2abap.
    DATA: lo_type TYPE REF TO cl_abap_typedescr  .

    CLEAR ev_content.

    gv_boolean_false = iv_boolean_false.

    lo_type = cl_abap_typedescr=>describe_by_data( p_data = ev_content ).

    map(
      EXPORTING
        io_json = io_json
        io_type = lo_type
      IMPORTING
        ev_content = ev_content ).
  ENDMETHOD.                    "map2abap


  METHOD map2class.
    TRY.
        ev_content ?= io_json.
        RETURN.
      CATCH cx_sy_move_cast_error.
        "Not JSON
        RAISE EXCEPTION TYPE /ixbx/zcx_bx_error
          EXPORTING
            textid = /ixbx/zcx_bx_error=>m133.
    ENDTRY.

  ENDMETHOD.                    "MAP2CLASS


  METHOD map2elem.
    DATA: lo_json_simple TYPE REF TO /uif/cl_json_simple,
          lx_conv        TYPE REF TO cx_sy_conversion_error.

    TRY.
        lo_json_simple ?= io_json.
      CATCH cx_sy_move_cast_error.
        RAISE EXCEPTION TYPE /ixbx/zcx_bx_error
          EXPORTING
            textid = /ixbx/zcx_bx_error=>m125.
    ENDTRY.

    TRY.


        CASE io_type->type_kind.
          WHEN cl_abap_typedescr=>typekind_char OR
               cl_abap_typedescr=>typekind_clike OR
               cl_abap_typedescr=>typekind_csequence OR
               cl_abap_typedescr=>typekind_string OR
               cl_abap_typedescr=>typekind_w.

            CASE io_type->absolute_name.
              WHEN '\TYPE=XSDBOOLEAN'.
                IF lo_json_simple->mv_bool_value IS NOT INITIAL.
                  ev_content = lo_json_simple->mv_bool_value.
                ELSE.
                  ev_content = gv_boolean_false.
                ENDIF.

              WHEN OTHERS.
                "We have to guess what it is, so try bool otherwise string
                IF lo_json_simple->mv_bool_value IS NOT INITIAL.
                  ev_content = lo_json_simple->mv_bool_value.
                ELSE.
                  ev_content = lo_json_simple->mv_string_value.
                ENDIF.
            ENDCASE.

          WHEN cl_abap_typedescr=>typekind_date.
            "TODO date conversion
            ev_content = lo_json_simple->mv_string_value.

          WHEN cl_abap_typedescr=>typekind_time.
            "TODO time conversion
            ev_content = lo_json_simple->mv_string_value.

          WHEN cl_abap_typedescr=>typekind_decfloat OR
               cl_abap_typedescr=>typekind_decfloat16 OR
               cl_abap_typedescr=>typekind_decfloat34 OR
               cl_abap_typedescr=>typekind_float.
            IF lo_json_simple->mv_float_string_value IS NOT INITIAL.
              ev_content = lo_json_simple->mv_float_string_value.
            ELSEIF lo_json_simple->mv_int_value IS NOT INITIAL.
              ev_content = lo_json_simple->mv_int_value.
            ELSEIF lo_json_simple->mv_string_value IS NOT INITIAL.
              ev_content = lo_json_simple->mv_string_value.
            ENDIF.

          WHEN cl_abap_typedescr=>typekind_int OR
               cl_abap_typedescr=>typekind_int1 OR
               cl_abap_typedescr=>typekind_int2 OR
               '8'. "INT8 since 7.40
            ev_content = lo_json_simple->mv_int_value.

          WHEN cl_abap_typedescr=>typekind_num OR
               cl_abap_typedescr=>typekind_numeric.
            IF lo_json_simple->mv_string_value IS NOT INITIAL.
              ev_content = lo_json_simple->mv_string_value.
            ELSEIF lo_json_simple->mv_int_value IS NOT INITIAL.
              ev_content = lo_json_simple->mv_int_value.
            ENDIF.
          WHEN cl_abap_typedescr=>typekind_packed.
            "Todo verify
            IF lo_json_simple->mv_float_string_value IS NOT INITIAL.
              ev_content = lo_json_simple->mv_float_string_value.
            ELSE.
              ev_content = lo_json_simple->mv_int_value.
            ENDIF.
          WHEN OTHERS.
            "Not supported
            RAISE EXCEPTION TYPE /ixbx/zcx_bx_error
              EXPORTING
                textid = /ixbx/zcx_bx_error=>m128.
        ENDCASE.
      CATCH cx_sy_conversion_error INTO lx_conv.
        RAISE EXCEPTION TYPE /ixbx/zcx_bx_error
          EXPORTING
            textid   = /ixbx/zcx_bx_error=>m128
            previous = lx_conv.
    ENDTRY.
  ENDMETHOD.                    "MAP2ELEM


  METHOD map2ref.
    DATA: lo_target_type TYPE REF TO cl_abap_typedescr,
          lo_type_intf   TYPE REF TO cl_abap_intfdescr,
          lo_type_class  TYPE REF TO cl_abap_classdescr.

    lo_target_type = io_type->get_referenced_type( ).

    CASE lo_target_type->kind.
      WHEN cl_abap_typedescr=>kind_class.
        lo_type_class ?= lo_target_type.
        map2class(
          EXPORTING
            io_json = io_json
            io_type = lo_type_class
          IMPORTING
            ev_content = ev_content ).

      WHEN cl_abap_typedescr=>kind_intf.
        lo_type_intf ?= lo_target_type.
        RETURN.

      WHEN OTHERS.

    ENDCASE.
  ENDMETHOD.                    "MAP2REF


  METHOD map2struct.
    DATA: lt_components  TYPE cl_abap_structdescr=>component_table,
          lo_json_object TYPE REF TO /uif/cl_json_object,
          lo_json_array  TYPE REF TO /uif/cl_json_array.

    lt_components = io_type->get_components( ).

    TRY.
        lo_json_object ?= io_json.
        map_object2struct(
          EXPORTING
            io_json       = lo_json_object
            it_components = lt_components
          IMPORTING
            ev_content = ev_content ).
      CATCH cx_sy_move_cast_error.
        TRY.
            lo_json_array ?= io_json.
            RAISE EXCEPTION TYPE /ixbx/zcx_bx_error
              EXPORTING
                textid = /ixbx/zcx_bx_error=>m129.
          CATCH cx_sy_move_cast_error.
            RAISE EXCEPTION TYPE /ixbx/zcx_bx_error
              EXPORTING
                textid = /ixbx/zcx_bx_error=>m125.
        ENDTRY.
    ENDTRY.
  ENDMETHOD.                    "MAP2STRUCT


  METHOD map2table.
    DATA: lo_json_array  TYPE REF TO /uif/cl_json_array,
          lo_json_object TYPE REF TO /uif/cl_json_object.

    TRY.
        lo_json_array ?= io_json.
        map_array2table(
          EXPORTING
            io_json = lo_json_array
            io_type = io_type
          IMPORTING
            ev_content = ev_content ).

      CATCH cx_sy_move_cast_error.
        TRY.
            lo_json_object ?= io_json.
            map_object2table(
              EXPORTING
                io_json = lo_json_object
                io_type = io_type
              IMPORTING
                ev_content = ev_content ).
          CATCH cx_sy_move_cast_error.
            RAISE EXCEPTION TYPE /ixbx/zcx_bx_error
              EXPORTING
                textid = /ixbx/zcx_bx_error=>m126.
        ENDTRY.
    ENDTRY.

  ENDMETHOD.                    "MAP2TABLE


  METHOD map_array2table.
    DATA: lv_length    TYPE i,
          lv_index     TYPE string,
          lo_line_type TYPE REF TO cl_abap_datadescr,
          lo_json      TYPE REF TO /uif/cl_json_item,
          lr_line      TYPE REF TO data,
          lx_conv      TYPE REF TO /ixbx/zcx_bx_exception,
          lv_attr1     TYPE symsgv.
    FIELD-SYMBOLS: <ls_line>  TYPE any,
                   <lt_table> TYPE ANY TABLE.

    ASSIGN ev_content TO <lt_table>.
    lo_line_type = io_type->get_table_line_type( ).
    lv_length = io_json->length( ).
    DO lv_length TIMES.
      lo_json = io_json->get( iv_item_index = ( sy-index - 1 ) ).
      CREATE DATA lr_line TYPE HANDLE lo_line_type.
      ASSIGN lr_line->* TO <ls_line>.

      TRY.
          map(
            EXPORTING
              io_json    = lo_json
              io_type    = lo_line_type
            IMPORTING
              ev_content = <ls_line>
          ).
        CATCH /ixbx/zcx_bx_exception INTO lx_conv.
          lv_attr1 = sy-index.
          RAISE EXCEPTION TYPE /ixbx/zcx_bx_error
            EXPORTING
              textid   = /ixbx/zcx_bx_error=>m134
              mv_attr1 = lv_attr1.
      ENDTRY.

      INSERT <ls_line> INTO TABLE <lt_table>.

    ENDDO.
  ENDMETHOD.                    "MAP_ARRAY2TABLE


  METHOD map_object2struct.
    DATA: lo_json_property TYPE REF TO /uif/cl_json_item,
          lt_properties    TYPE string_table,
          lt_map           TYPE name2stringvalue_table,
          ls_map           TYPE name2stringvalue,
          lv_attr1         TYPE symsgv,
          lv_attr2         TYPE symsgv,
          lx_conv          TYPE REF TO /ixbx/zcx_bx_exception.

    FIELD-SYMBOLS: <ls_component> LIKE LINE OF it_components,
                   <lv_content>   TYPE any,
                   <ls_map>       TYPE name2stringvalue.

    lt_properties = io_json->get_property_names( ).
    LOOP AT lt_properties INTO ls_map-value.
      ls_map-name = ls_map-value.
      REPLACE ALL OCCURRENCES OF REGEX `([a-z])([A-Z])` IN ls_map-name WITH `$1_$2`. "#EC NOTEXT
      REPLACE ALL OCCURRENCES OF '.' IN ls_map-name WITH `_`. "#EC NOTEXT
      TRANSLATE ls_map-name TO UPPER CASE.
      APPEND ls_map TO lt_map.
    ENDLOOP.
    SORT lt_map.

    LOOP AT it_components ASSIGNING <ls_component>.
      READ TABLE lt_map ASSIGNING <ls_map> WITH KEY name = <ls_component>-name BINARY SEARCH.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.
      lo_json_property = io_json->get_property( iv_property_name = <ls_map>-value ).
      IF lo_json_property IS BOUND.
        ASSIGN COMPONENT <ls_component>-name OF STRUCTURE ev_content TO <lv_content>.
        TRY.
            map(
              EXPORTING
                io_json    = lo_json_property
                io_type    = <ls_component>-type
              IMPORTING
                ev_content = <lv_content>
            ).
          CATCH /ixbx/zcx_bx_exception.
            lv_attr1 = <ls_component>-name.
            lv_attr2 = <ls_map>-value.
            RAISE EXCEPTION TYPE /ixbx/zcx_bx_error
              EXPORTING
                textid   = /ixbx/zcx_bx_error=>m135
                mv_attr1 = lv_attr1
                mv_attr2 = lv_attr2.
        ENDTRY.
      ENDIF.

    ENDLOOP.
  ENDMETHOD.                    "MAP_OBJECT2STRUCT


  METHOD map_object2table.
    DATA: lo_line_type        TYPE REF TO cl_abap_structdescr,
          lo_json             TYPE REF TO /uif/cl_json_item,
          lr_line             TYPE REF TO data,
          lt_components       TYPE cl_abap_structdescr=>component_table,
          lv_components_count TYPE i,
          lt_properties       TYPE string_table,
          lv_is_arr_index     TYPE abap_bool,
          lx_conv             TYPE REF TO /ixbx/zcx_bx_exception,
          lo_json_object      TYPE REF TO /uif/cl_json_object,
          lx_cast             TYPE REF TO cx_sy_move_cast_error,
          lv_attr1            TYPE symsgv,
          ls_key              TYPE LINE OF abap_keydescr_tab,
          ls_type_key         TYPE LINE OF abap_keydescr_tab.

    FIELD-SYMBOLS: <ls_properties> LIKE LINE OF  it_properties,
                   <ls_component>  LIKE LINE OF lt_components,
                   <ls_line>       TYPE any,
                   <lt_table>      TYPE ANY TABLE,
                   <lv_name>       TYPE any,
                   <lv_value>      TYPE any.

    "Supported is moving a structure to name-value table with complex value or to structure with one dedicated key field
    TRY.
        lo_line_type ?= io_type->get_table_line_type( ).
      CATCH cx_sy_move_cast_error.
        RETURN.
    ENDTRY.
    lt_components = lo_line_type->get_components( ).

    "Prepare table line
    ASSIGN ev_content TO <lt_table>.
    CREATE DATA lr_line TYPE HANDLE lo_line_type.
    ASSIGN lr_line->* TO <ls_line>.

    "Analyse table
    DATA lv_is_name_value TYPE abap_bool.
    DESCRIBE TABLE lt_components LINES lv_components_count.
    IF lv_components_count = 2.
      "Appears to be name value table
      lv_is_name_value = abap_true.
      READ TABLE lt_components ASSIGNING <ls_component> INDEX 1.
      CASE <ls_component>-type->type_kind.
        WHEN cl_abap_typedescr=>typekind_char OR
             cl_abap_typedescr=>typekind_clike OR
             cl_abap_typedescr=>typekind_csequence OR
             cl_abap_typedescr=>typekind_string OR
             cl_abap_typedescr=>typekind_w.

        WHEN cl_abap_typedescr=>typekind_int OR
             cl_abap_typedescr=>typekind_int1 OR
             cl_abap_typedescr=>typekind_int2 OR
             '8'. "INT8 since 7.40.
          IF io_json IS BOUND. "Only in special mode
            RAISE EXCEPTION TYPE /ixbx/zcx_bx_error
              EXPORTING
                textid = /ixbx/zcx_bx_error=>m130.
          ELSE.
            lv_is_arr_index = abap_true.
          ENDIF.
        WHEN cl_abap_typedescr=>typekind_packed.

        WHEN OTHERS.
          RAISE EXCEPTION TYPE /ixbx/zcx_bx_error
            EXPORTING
              textid = /ixbx/zcx_bx_error=>m130.
      ENDCASE.

      "Assign name value fields
      ASSIGN COMPONENT <ls_component>-name OF STRUCTURE <ls_line> TO <lv_name>.

      READ TABLE lt_components ASSIGNING <ls_component> INDEX 2.
      ASSIGN COMPONENT <ls_component>-name OF STRUCTURE <ls_line> TO <lv_value>.

    ELSE.
      IF lines( io_type->key ) = 1.
        "Appears to be table with key field
        READ TABLE io_type->key INDEX 1 INTO ls_key.
        ASSIGN COMPONENT ls_key-name OF STRUCTURE <ls_line> TO <lv_name>.
        READ TABLE io_type->key INDEX 1 INTO ls_type_key.
        DELETE lt_components WHERE name = ls_type_key-name.

      ELSE.
        "Not supported
        RAISE EXCEPTION TYPE /ixbx/zcx_bx_error
          EXPORTING
            textid = /ixbx/zcx_bx_error=>m130.
      ENDIF.

    ENDIF.


    IF io_json IS BOUND.
      lt_properties = io_json->get_property_names( ).
      LOOP AT lt_properties INTO <lv_name>.
        lo_json = io_json->get_property( iv_property_name = <lv_name> ).
        IF lo_json IS BOUND.
          TRY.
              IF lv_is_name_value = abap_true.
                map(
                  EXPORTING
                    io_json    = lo_json
                    io_type    = <ls_component>-type
                  IMPORTING
                    ev_content = <lv_value>
                ).
              ELSE.
                TRY.
                    lo_json_object ?= lo_json.
                  CATCH cx_sy_move_cast_error INTO lx_cast.
                    RAISE EXCEPTION TYPE /ixbx/zcx_bx_error
                      EXPORTING
                        textid = /ixbx/zcx_bx_error=>m132.
                ENDTRY.
                map_object2struct(
                  EXPORTING
                    io_json                        = lo_json_object
                    it_components                  = lt_components
                  IMPORTING
                    ev_content                     = <ls_line>
                ).

              ENDIF.
            CATCH /ixbx/zcx_bx_exception INTO lx_conv.
              lv_attr1 = <lv_name>.
              RAISE EXCEPTION TYPE /ixbx/zcx_bx_error
                EXPORTING
                  textid   = /ixbx/zcx_bx_error=>m136
                  mv_attr1 = lv_attr1.
*
          ENDTRY.

          INSERT <ls_line> INTO TABLE <lt_table>.
        ENDIF.

        CLEAR <ls_line>.
      ENDLOOP.
    ELSE.
      LOOP AT it_properties ASSIGNING <ls_properties>.
        IF lv_is_arr_index = abap_false.
          <lv_name> = <ls_properties>-node_name.
        ELSE.
          <lv_name> = <ls_properties>-array_index.
        ENDIF.

        IF <ls_properties>-node IS BOUND.
          TRY.
              IF lv_is_name_value = abap_true.
                map(
                  EXPORTING
                    io_json    = <ls_properties>-node
                    io_type    = <ls_component>-type
                  IMPORTING
                    ev_content = <lv_value>
                ).
              ELSE.
                TRY.
                    lo_json_object ?= lo_json.
                  CATCH cx_sy_move_cast_error INTO lx_cast.
                    RAISE EXCEPTION TYPE /ixbx/zcx_bx_error
                      EXPORTING
                        textid = /ixbx/zcx_bx_error=>m132.
                ENDTRY.
                map_object2struct(
                  EXPORTING
                    io_json                        = lo_json_object
                    it_components                  = lt_components
                  IMPORTING
                    ev_content                     = <ls_line>
                ).

              ENDIF.
            CATCH /ixbx/zcx_bx_exception.
              lv_attr1 = <ls_properties>-node_name.
              RAISE EXCEPTION TYPE /ixbx/zcx_bx_error
                EXPORTING
                  textid   = /ixbx/zcx_bx_error=>m137
                  mv_attr1 = lv_attr1.
          ENDTRY.

          INSERT <ls_line> INTO TABLE <lt_table>.
        ENDIF.

        CLEAR <ls_line>.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.                    "MAP_OBJECT2TABLE
ENDCLASS.