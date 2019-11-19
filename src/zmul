REPORT zmultiply.

CLASS lcl_main DEFINITION.
  PUBLIC SECTION.
    METHODS start.
  PRIVATE SECTION.
    CLASS-METHODS handle_sapevent
      FOR EVENT sapevent OF cl_abap_browser
      IMPORTING action
                query_table.
    class-DATA: n1     TYPE i VALUE 4,
                n2     TYPE i VALUE 2.
ENDCLASS.

CLASS lcl_main IMPLEMENTATION.

  METHOD start.
  DATA: answer TYPE i.

    SET HANDLER handle_sapevent.

    DATA(html) =
      |<!DOCTYPE html>| &&
      |<html>| &&
      |  <head>| &&
      |    <style>body \{ font-family: sans-serif; \}</style>| &&
      |    <meta http-equiv="X-UA-Compatible" content="IE=edge">| &&
      |    <meta http-equiv='Content-Type' content='text/html;charset=UTF-8'/>| &&
      |  </head>| &&
      |    <script language="JavaScript">| &&
      |      function sendInput(form) | &&
      |          \{ fname=form.name;       | &&
      |            document[fname].submit();\} | &&
      |      function InputKeyDown(form) \{| &&
      |        if(event.keyCode == 13) \{| &&
      |            fname=form.name;| &&
      |            document[fname].submit();\} \}| &&
      |    </script>| &&

      |  <body>| &&

      |<form name="INPUT" method="post" action="SAPEVENT:INPUT">| &&
      |<table><tr><td>| &&
      |What is { n1 } * {  n2 }?   Answer:<input type=number name=answer onsubmit="this.form.submit()">| &&
      |<button id="enterButton" type="button" title="Enter" onClick="sendInput(INPUT);" onKeypress="if(event.keycode=13) sendInput(INPUT);">Enter</button>| &&

      |<td>     | &&
      |</td></tr></table>| &&
      |</form>  | &&

      |  </body>| &&
      |</html>  |.
    cl_abap_browser=>show_html(
      EXPORTING
        title        = 'The Multiplificator'
        html_string  = html
        dialog       = abap_false ).
  ENDMETHOD.

  METHOD handle_sapevent.
    data(answer) = conv i( query_table[ name = 'answer' ]-value ).
    IF answer = n1 * n2.
      cl_demo_output=>display( 'Yay! \(ˆ˚ˆ)/' ).
    ELSE.
      cl_demo_output=>display( |Bzzt! Sorry, { n1 } * {  n2 } is { n1 * n2 }| ).
    ENDIF.
  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  NEW lcl_main( )->start( ).
