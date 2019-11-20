REPORT zmultiply.

CLASS lcl_main DEFINITION.
  PUBLIC SECTION.
    METHODS start.
    METHODS constructor.
  PRIVATE SECTION.
    METHODS handle_sapevent
                FOR EVENT sapevent OF cl_abap_browser
      IMPORTING action
                query_table.
    DATA random TYPE REF TO cl_abap_random_int.
    DATA: n1 TYPE i,
          n2 TYPE i.
ENDCLASS.

CLASS lcl_main IMPLEMENTATION.

  METHOD constructor.
    random = cl_abap_random_int=>create( min = 0 max = 10 seed = cl_abap_random=>seed( ) ).
    n1 = random->get_next( ).
    n2 = random->get_next( ).
  ENDMETHOD.

  METHOD start.
    DATA: answer TYPE i.

    SET HANDLER handle_sapevent.

    DATA(html) =
      |<!DOCTYPE html>                                                        | &&
      |<html>                                                                 | &&
      |  <head>                                                               | &&
      |    <style>body \{ font-family:cursive; color:darkred;                 | &&
      |               background-color:lightyellow; font-size:24px; \}</style>| &&
      |    <meta http-equiv="X-UA-Compatible" content="IE=edge">              | &&
      |    <meta http-equiv='Content-Type' content='text/html;charset=UTF-8'/>| &&
      |  </head>                                                              | &&
      |    <script language="JavaScript">                                     | &&
      |      function sendInput(form) \{                                      | &&
      |          fname=form.name;                                             | &&
      |          document[fname].submit();                                    | &&
      |      \}                                                               | &&
      |      function InputKeyDown(form) \{                                   | &&
      |          if(event.keyCode == 13) \{                                   | &&
      |              fname=form.name;                                         | &&
      |              document[fname].submit();                                | &&
      |          \}                                                           | &&
      |      \}                                                               | &&
      |    </script                                                           | &&

      |  <body>| &&

      |<form name="INPUT" method="post" action="SAPEVENT:INPUT">| &&
      |<table><tr><td>| &&
      |What is { n1 } * {  n2 }?</td></tr>| &&
      |<tr><td>Answer:<input type=number name=answer onsubmit="this.form.submit()">| &&
      |<button id="enterButton" type="button" title="Enter" onClick="sendInput(INPUT);" onKeypress="if(event.keycode=13) sendInput(INPUT);">Check my answer</button>| &&
      |</td></tr></table>| &&
      |</form>| &&

      |  </body>| &&
      |</html>|.

    cl_abap_browser=>show_html(
      EXPORTING
        title        = 'The Multiplificator'
        html_string  = html
        dialog       = abap_false ).
  ENDMETHOD.

  METHOD handle_sapevent.
    DATA(answer) = CONV i( query_table[ name = 'answer' ]-value ).
    IF answer = n1 * n2.
      cl_demo_output=>display( 'Yay! \(ˆ˚ˆ)/' ).
    ELSE.
      cl_demo_output=>display( |Bzzt! Sorry, { n1 } * {  n2 } is { n1 * n2 }| ).
    ENDIF.
  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  NEW lcl_main( )->start( ).
