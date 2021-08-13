*"* use this source file for your ABAP unit test classes
 CLASS ltcl_test DEFINITION FINAL FOR TESTING
   DURATION SHORT
   RISK LEVEL HARMLESS.

   PRIVATE SECTION.
     METHODS:
       map2structure FOR TESTING RAISING cx_static_check.

     METHODS:
       assert_conversion_error
         IMPORTING
           json     TYPE REF TO /uif/cl_json_item
           exp_path TYPE string
         EXPORTING
           data     TYPE any.
 ENDCLASS.


 CLASS ltcl_test IMPLEMENTATION.

   METHOD map2structure.
     DATA: lv_json TYPE string,
           lo_json TYPE REF TO /uif/cl_json_item.

     TYPES: BEGIN OF name_value_table_line,
              the_name TYPE string,
              BEGIN OF the_value,
                something TYPE string,
              END OF the_value,
            END OF name_value_table_line.
     TYPES: BEGIN OF something_else,
              something TYPE string,
              else      TYPE string,
            END OF something_else.
     TYPES: deep_tab_line TYPE STANDARD TABLE OF something_else WITH NON-UNIQUE DEFAULT KEY.
     TYPES: BEGIN OF struct2table2,
              name      TYPE string,
              something TYPE string,
              else      TYPE i,
            END OF struct2table2.
     TYPES: BEGIN OF result_struc,
              BEGIN OF name_mapping,
                camel_case1    TYPE string,
                camel_case2    TYPE string,
                upper          TYPE string,
                lower          TYPE string,
                with_some_dots TYPE string,
                not_in_source  TYPE string,
              END OF name_mapping,
              BEGIN OF element_mapping,
                f_string2string             TYPE string,
                f_special_chars             TYPE string,
                f_string2char               TYPE char10,
                f_int2int                   TYPE int4,
                f_int2float                 TYPE float,
                f_float2float               TYPE float,
                f_0int2int                  TYPE int4,
                f_0int2float                TYPE float,
                f_0float2float              TYPE float,
                f_string2num                TYPE numc3,
                f_int2num                   TYPE numc3,
                f_bool2char1true            TYPE char1,
                f_bool2char1false           TYPE char1,
                f_bool2char1undefined       TYPE char1,
                f_bool2xsdboolean_true      TYPE xsdboolean,
                f_bool2xsdboolean_false     TYPE xsdboolean,
                f_bool2xsdboolean_undefined TYPE xsdboolean,
              END OF element_mapping,
              struct2table  TYPE STANDARD TABLE OF name_value_table_line WITH NON-UNIQUE DEFAULT KEY,
              struct2table2 TYPE STANDARD TABLE OF struct2table2 WITH NON-UNIQUE KEY name,
              array2table   TYPE STANDARD TABLE OF deep_tab_line WITH NON-UNIQUE DEFAULT KEY,
              BEGIN OF json_object_mapping,
                object2item TYPE REF TO /uif/cl_json_item,
                elem2simple TYPE REF TO /uif/cl_json_simple,
                object2obj  TYPE REF TO /uif/cl_json_object,
                array2arr   TYPE REF TO /uif/cl_json_array,
              END OF json_object_mapping,
            END OF result_struc.

     DATA: lv_result TYPE result_struc,
           lv_float  TYPE float VALUE '42.5',
           lv_0float TYPE float VALUE '0.0'.
     FIELD-SYMBOLS: <struct2table_line>  TYPE name_value_table_line,
                    <struct2table2_line> TYPE struct2table2,
                    <deep_tab_line>      TYPE deep_tab_line,
                    <something_else>     TYPE something_else.


     CONCATENATE
     '{'
     '    "name_mapping" : {'
     '        "notInTarget": "This is not relevant",'
     '        "camelCase1": "camelCase1",'
     '        "CamelCase2": "CamelCase2",'
     '        "UPPER": "UPPER",'
     '        "lower": "lower",'
     '        "with.some.dots": "with.some.dots",'
     '        "camelCase": "camelCase"'
     '    },'
     '    "element_mapping" : {'
     '        "f_string2string": "aString",'
     '        "f_special_chars": "\\\"''`\u005C",'
     '        "f_string2char": "aQuiteLongString",'
     '        "f_int2int": 42,'
     '        "f_int2float": 42,'
     '        "f_float2float": 42.5,'
     '        "f_0int2int": 0,'
     '        "f_0int2float": 0,'
     '        "f_0float2float": 0.0,'
     '        "f_string2num": "042",'
     '        "f_int2num": 42,'
     '        "f_bool2char1true":true,'
     '        "f_bool2char1false":false,'
     '        "f_bool2xsdbooleanTrue":true,'
     '        "f_bool2xsdbooleanFalse":false'
     '    },'
     '    "struct2table": {'
     '        "!§$%&": {'
     '            "something": "something"'
     '        },'
     '        "\"\\": {'
     '            "something": "something"'
     '        },'
     '        "with.some.dots": {'
     '            "something": "something"'
     '        }'
     '    },'
     '    "struct2table2": {'
     '        "!§$%&": {'
     '            "something": "something",'
     '            "else": 42'
     '        },'
     '        "\"\\": {'
     '            "something": "something",'
     '            "else": 23'
     '        },'
     '        "with.some.dots": {'
     '            "something": "something",'
     '            "notNeeded": "useless"'
     '        }'
     '    },'
     '    "array2table" : ['
     '        ['
     '            {'
     '                "something" : "something"'
     '            },'
     '            {'
     '                "else" : "else"'
     '            }'
     '        ],'
     '        ['
     '        ]'
     '    ],'
     '    "json_object_mapping" : {'
     '        "object2item": {},'
     '        "elem2simple": "something",'
     '        "object2obj": {},'
     '        "array2arr": [],'
     '        "missmatch": []'
     '    }'
     '}' INTO lv_json.           "#EC STRING_OK  -> specical characters

     lo_json = /uif/cl_json=>parse( lv_json ).
     /ixbx/cl_json_mapper=>map2abap(
       EXPORTING
         io_json    = lo_json
         iv_boolean_false = '-'
       IMPORTING
         ev_content = lv_result
     ).
     cl_abap_unit_assert=>assert_equals( exp = 'camelCase1' act = lv_result-name_mapping-camel_case1 ).
     cl_abap_unit_assert=>assert_equals( exp = 'CamelCase2' act = lv_result-name_mapping-camel_case2 ).
     cl_abap_unit_assert=>assert_equals( exp = 'lower' act = lv_result-name_mapping-lower ).
     cl_abap_unit_assert=>assert_equals( exp = 'UPPER' act = lv_result-name_mapping-upper ).
     cl_abap_unit_assert=>assert_equals( exp = 'with.some.dots' act = lv_result-name_mapping-with_some_dots ).
     cl_abap_unit_assert=>assert_equals( exp = '' act = lv_result-name_mapping-not_in_source ).

     cl_abap_unit_assert=>assert_equals( exp = 'aString' act = lv_result-element_mapping-f_string2string ).
     cl_abap_unit_assert=>assert_equals( exp = 'aQuiteLong' act = lv_result-element_mapping-f_string2char ).
     cl_abap_unit_assert=>assert_equals( exp = '\"''`\' act = lv_result-element_mapping-f_special_chars ).
     cl_abap_unit_assert=>assert_equals( exp = 42 act = lv_result-element_mapping-f_int2int ).
     cl_abap_unit_assert=>assert_equals( exp = 42 act = lv_result-element_mapping-f_int2float ).
     cl_abap_unit_assert=>assert_equals( exp = lv_float act = lv_result-element_mapping-f_float2float ).
     cl_abap_unit_assert=>assert_equals( exp = 0 act = lv_result-element_mapping-f_0int2int ).
     cl_abap_unit_assert=>assert_equals( exp = 0 act = lv_result-element_mapping-f_0int2float ).
     cl_abap_unit_assert=>assert_equals( exp = lv_0float act = lv_result-element_mapping-f_0float2float ).
*     cl_abap_unit_assert=>assert_equals( exp = '' act = lv_result-element_mapping- ).
     cl_abap_unit_assert=>assert_equals( exp = 042 act = lv_result-element_mapping-f_string2num ).
     cl_abap_unit_assert=>assert_equals( exp = 042 act = lv_result-element_mapping-f_int2num ).

     cl_abap_unit_assert=>assert_equals( exp = abap_false act = lv_result-element_mapping-f_bool2char1false ).
     cl_abap_unit_assert=>assert_equals( exp = abap_true act = lv_result-element_mapping-f_bool2char1true ).
     cl_abap_unit_assert=>assert_equals( exp = abap_false act = lv_result-element_mapping-f_bool2char1undefined ).
     cl_abap_unit_assert=>assert_equals( exp = '-' act = lv_result-element_mapping-f_bool2xsdboolean_false ).
     cl_abap_unit_assert=>assert_equals( exp = abap_true act = lv_result-element_mapping-f_bool2xsdboolean_true ).
     cl_abap_unit_assert=>assert_equals( exp = abap_false act = lv_result-element_mapping-f_bool2xsdboolean_undefined ).

     READ TABLE lv_result-struct2table ASSIGNING <struct2table_line> INDEX 1.
     cl_abap_unit_assert=>assert_subrc( exp = 0 act = sy-subrc ).
     cl_abap_unit_assert=>assert_equals( exp = '!§$%&' act = <struct2table_line>-the_name ).
                                   "#EC STRING_OK -> special characters
     cl_abap_unit_assert=>assert_equals( exp = 'something' act = <struct2table_line>-the_value-something ).
     READ TABLE lv_result-struct2table ASSIGNING <struct2table_line> INDEX 2.
     cl_abap_unit_assert=>assert_subrc( exp = 0 act = sy-subrc ).
     cl_abap_unit_assert=>assert_equals( exp = '"\' act = <struct2table_line>-the_name ).
     cl_abap_unit_assert=>assert_equals( exp = 'something' act = <struct2table_line>-the_value-something ).
     READ TABLE lv_result-struct2table ASSIGNING <struct2table_line> INDEX 3.
     cl_abap_unit_assert=>assert_subrc( exp = 0 act = sy-subrc ).
     cl_abap_unit_assert=>assert_equals( exp = 'with.some.dots' act = <struct2table_line>-the_name ).
     cl_abap_unit_assert=>assert_equals( exp = 'something' act = <struct2table_line>-the_value-something ).

     READ TABLE lv_result-struct2table2 ASSIGNING <struct2table2_line> INDEX 1.
     cl_abap_unit_assert=>assert_subrc( exp = 0 act = sy-subrc ).
     cl_abap_unit_assert=>assert_equals( exp = '!§$%&' act = <struct2table2_line>-name ).
                                   "#EC STRING_OK -> special characters
     cl_abap_unit_assert=>assert_equals( exp = 'something' act = <struct2table2_line>-something ).
     cl_abap_unit_assert=>assert_equals( exp = '42' act = <struct2table2_line>-else ).
     READ TABLE lv_result-struct2table2 ASSIGNING <struct2table2_line> INDEX 2.
     cl_abap_unit_assert=>assert_subrc( exp = 0 act = sy-subrc ).
     cl_abap_unit_assert=>assert_equals( exp = '"\' act = <struct2table2_line>-name ).
     cl_abap_unit_assert=>assert_equals( exp = 'something' act = <struct2table2_line>-something ).
     cl_abap_unit_assert=>assert_equals( exp = '23' act = <struct2table2_line>-else ).
     READ TABLE lv_result-struct2table2 ASSIGNING <struct2table2_line> INDEX 3.
     cl_abap_unit_assert=>assert_subrc( exp = 0 act = sy-subrc ).
     cl_abap_unit_assert=>assert_equals( exp = 'with.some.dots' act = <struct2table2_line>-name ).
     cl_abap_unit_assert=>assert_equals( exp = 'something' act = <struct2table2_line>-something ).
     cl_abap_unit_assert=>assert_equals( exp = '0' act = <struct2table2_line>-else ).

     READ TABLE lv_result-array2table ASSIGNING <deep_tab_line> INDEX 1.
     cl_abap_unit_assert=>assert_subrc( exp = 0 act = sy-subrc ).
     READ TABLE <deep_tab_line> ASSIGNING <something_else> INDEX 1.
     cl_abap_unit_assert=>assert_subrc( exp = 0 act = sy-subrc ).
     cl_abap_unit_assert=>assert_equals( exp = 'something' act = <something_else>-something ).
     cl_abap_unit_assert=>assert_equals( exp = '' act = <something_else>-else ).
     READ TABLE <deep_tab_line> ASSIGNING <something_else> INDEX 2.
     cl_abap_unit_assert=>assert_subrc( exp = 0 act = sy-subrc ).
     cl_abap_unit_assert=>assert_equals( exp = '' act = <something_else>-something ).
     cl_abap_unit_assert=>assert_equals( exp = 'else' act = <something_else>-else ).

     READ TABLE <deep_tab_line> ASSIGNING <something_else> INDEX 3.
     cl_abap_unit_assert=>assert_subrc( exp = 4 act = sy-subrc ).

     READ TABLE lv_result-array2table ASSIGNING <deep_tab_line> INDEX 2.
     cl_abap_unit_assert=>assert_subrc( exp = 0 act = sy-subrc ).
     cl_abap_unit_assert=>assert_initial( act = <deep_tab_line> ).

     cl_abap_unit_assert=>assert_bound( act = lv_result-json_object_mapping-object2item ).
     cl_abap_unit_assert=>assert_bound( act = lv_result-json_object_mapping-elem2simple ).
     cl_abap_unit_assert=>assert_bound( act = lv_result-json_object_mapping-object2obj ).
     cl_abap_unit_assert=>assert_bound( act = lv_result-json_object_mapping-array2arr ).

     "***************************************************
     " Conversion errors
     "***************************************************
     "Object to simple
     DATA: lv_missmatch1 TYPE string.
     assert_conversion_error( EXPORTING json = lo_json  exp_path = '' IMPORTING data = lv_missmatch1 ).

     "Wrong object type
     DATA: BEGIN OF lv_missmatch2,
             BEGIN OF json_object_mapping,
               object2obj TYPE REF TO /uif/cl_json_array,
             END OF json_object_mapping,
           END OF lv_missmatch2 ##needed.
     assert_conversion_error( EXPORTING json = lo_json  exp_path = '/json_object_mapping/object2obj'
       IMPORTING data = lv_missmatch2 ).

     "Elem to table
     TYPES: BEGIN OF mistmatch3_tab,
              something TYPE stringtab,
            END OF mistmatch3_tab ##needed.
     TYPES: mistmatch3_tab_line TYPE STANDARD TABLE OF mistmatch3_tab WITH NON-UNIQUE DEFAULT KEY.
     DATA: BEGIN OF lv_missmatch3,
             array2table TYPE STANDARD TABLE OF mistmatch3_tab_line WITH NON-UNIQUE DEFAULT KEY,
           END OF lv_missmatch3 ##needed.
     assert_conversion_error( EXPORTING json = lo_json  exp_path = '/array2table[1][1]/something'
       IMPORTING data = lv_missmatch3 ).

     "Elem to struct
     DATA: BEGIN OF lv_missmatch4,
             BEGIN OF element_mapping,
               BEGIN OF f_string2string,
                 any_field TYPE string,
               END OF f_string2string,
             END OF element_mapping,
           END OF lv_missmatch4 ##needed.
     assert_conversion_error( EXPORTING json = lo_json  exp_path = '/element_mapping/f_string2string'
       IMPORTING data = lv_missmatch4 ).

     "Bad String to float
     DATA: BEGIN OF lv_missmatch5,
             BEGIN OF element_mapping,
               f_string2string TYPE float,
             END OF element_mapping,
           END OF lv_missmatch5 ##needed.
     assert_conversion_error( EXPORTING json = lo_json  exp_path = '/element_mapping/f_string2string'
       IMPORTING data = lv_missmatch5 ).

     "Array to structure
     DATA: BEGIN OF lv_missmatch6,
             BEGIN OF array2table,
               something TYPE string,
             END OF array2table,
           END OF lv_missmatch6 ##needed.
     assert_conversion_error( EXPORTING json = lo_json  exp_path = '/array2table' IMPORTING data = lv_missmatch6 ).

     "Struct to bad table
     TYPES: BEGIN OF lty_missmatch7,
              key1 TYPE string,
              key2 TYPE string,
              BEGIN OF valuefield,
                something TYPE string,
              END OF valuefield,
            END OF lty_missmatch7 ##needed.
     DATA: BEGIN OF lv_missmatch7,
             struct2table TYPE TABLE OF lty_missmatch7,
           END OF lv_missmatch7.
     assert_conversion_error( EXPORTING json = lo_json  exp_path = '/struct2table' IMPORTING data = lv_missmatch7 ).

     TYPES: BEGIN OF lty_missmatch8,
              key1 TYPE i,
              BEGIN OF valuefield,
                something TYPE string,
              END OF valuefield,
            END OF lty_missmatch8 ##needed.
     DATA: BEGIN OF lv_missmatch8,
             struct2table TYPE TABLE OF lty_missmatch8,
           END OF lv_missmatch8.
     assert_conversion_error( EXPORTING json = lo_json  exp_path = '/struct2table' IMPORTING data = lv_missmatch8 ).

   ENDMETHOD.                    "map2structure

   METHOD assert_conversion_error.

     DATA: lx_exception TYPE REF TO /ixbx/zcx_bx_exception.

     TRY.
         /ixbx/cl_json_mapper=>map2abap(
           EXPORTING
             io_json    = json
             iv_boolean_false = abap_false
           IMPORTING
             ev_content = data
         ).
         cl_aunit_assert=>fail( msg = 'Expected conversion error' ).
       CATCH /ixbx/zcx_bx_exception INTO lx_exception.
         "As expected
     ENDTRY.

   ENDMETHOD.                    "assert_conversion_error

 ENDCLASS.