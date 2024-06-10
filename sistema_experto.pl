% Cargar la biblioteca para la interfaz gráfica
:- use_module(library(pce)).
:- pce_image_directory('C:/Users/darcm/OneDrive/Documentos/Prolog/imagenes').  % Usar la ruta absoluta
:- use_module(library(pce_style_item)).
:- dynamic conocido/1.
:- dynamic historial/1.

% Definir recursos de imágenes
resource(informatica, image, image('ingenieria_informatica.jpg')).
resource(teologia, image, image('teologia.jpg')).
resource(ingenieria_comercial, image, image('ingenieria_comercial.jpg')).
resource(general, image, image('general.jpg')).

% Mostrar imagen en la ventana
mostrar_imagen(Pantalla, Imagen) :- 
    new(Figura, figure),
    new(Bitmap, bitmap(resource(Imagen), @on)),
    send(Bitmap, name, 1),
    send(Figura, display, Bitmap),
    send(Figura, status, 1),
    send(Pantalla, display, Figura, point(100, 80)).

% Base de conocimiento: Carreras y características
conocimiento('informatica', [
    'te gusta programar',
    'te interesan las computadoras',
    'disfrutas resolver problemas tecnicos']).

conocimiento('teologia', [
    'te interesa la religion',
    'te gusta estudiar textos religiosos',
    'te gusta ayudar a otros espiritualmente']).

conocimiento('ingenieria_comercial', [
    'te interesan los negocios',
    'te gusta la economia',
    'te interesa el marketing']).

% Reglas para mostrar el diagnóstico y tratamiento
mostrar_diagnostico(X) :- haz_diagnostico(X), clean_scratchpad, guardar_diagnostico(X).
mostrar_diagnostico(lo_siento_diagnostico_desconocido) :- clean_scratchpad.

haz_diagnostico(Diagnosis) :- 
    obten_hipotesis_y_sintomas(Diagnosis, ListaDeSintomas),
    prueba_presencia_de(Diagnosis, ListaDeSintomas).

obten_hipotesis_y_sintomas(Diagnosis, ListaDeSintomas) :- 
    conocimiento(Diagnosis, ListaDeSintomas).

prueba_presencia_de(_, []).
prueba_presencia_de(Diagnosis, [Head | Tail]) :- 
    prueba_verdad_de(Diagnosis, Head),
    prueba_presencia_de(Diagnosis, Tail).

prueba_verdad_de(_, Sintoma) :- conocido(Sintoma).
prueba_verdad_de(Diagnosis, Sintoma) :- 
    not(conocido(is_false(Sintoma))),
    pregunta_sobre(Diagnosis, Sintoma, Reply), 
    Reply = 'si'.

pregunta_sobre(_, Sintoma, Reply) :- 
    preguntar(Sintoma, Respuesta),
    process(Sintoma, Respuesta, Reply).

process(Sintoma, si, si) :- asserta(conocido(Sintoma)).
process(Sintoma, no, no) :- asserta(conocido(is_false(Sintoma))).

clean_scratchpad :- retract(conocido(_)), fail.
clean_scratchpad.

conocido(_) :- fail.

not(X) :- X, !, fail.
not(_).

% Guardar el diagnóstico en un archivo
guardar_diagnostico(Diagnostico) :-
    open('diagnosticos.txt', append, Stream),
    get_time(Timestamp),
    format_time(string(DateTime), '%Y-%m-%d %H:%M:%S', Timestamp),
    format(Stream, '~w - Diagnóstico: ~w~n', [DateTime, Diagnostico]),
    close(Stream),
    asserta(historial(DateTime-Diagnostico)).

% Ver el historial de diagnósticos
ver_historial :-
    new(Dialog, dialog('Historial de Diagnósticos')),
    new(Tabla, list_browser),
    forall(historial(Fecha-Diag),
        send(Tabla, append, dict_item(string('~w - ~w', [Fecha, Diag])))),
    send(Dialog, append, Tabla),
    send(Dialog, open_centered).

% Interfaz gráfica
preguntar(Preg, Resp) :- 
    new(Di, dialog('Consultar Datos:')),
    new(L2, label(texto, 'Responde las siguientes preguntas')),
    imagen_pregunta(Di, Preg),
    new(La, label(prob, Preg)),
    new(B1, button(si, and(message(Di, return, si)))),
    new(B2, button(no, and(message(Di, return, no)))),
    send(Di, gap, size(25, 25)),
    send(Di, append, L2),
    send(Di, append, La),
    send(Di, append, B1),
    send(Di, append, B2),
    send(Di, default_button, 'si'),
    send(Di, open_centered),
    get(Di, confirm, Answer),
    free(Di),
    Resp = Answer.

imagen_pregunta(Ventana, Preg) :- 
    (Preg = 'te gusta programar' -> Img = informatica ;
    Preg = 'te interesan las computadoras' -> Img = informatica ;
    Preg = 'disfrutas resolver problemas tecnicos' -> Img = informatica ;
    Preg = 'te interesa la religion' -> Img = teologia ;
    Preg = 'te gusta estudiar textos religiosos' -> Img = teologia ;
    Preg = 'te gusta ayudar a otros espiritualmente' -> Img = teologia ;
    Preg = 'te interesan los negocios' -> Img = ingenieria_comercial ;
    Preg = 'te gusta la economia' -> Img = ingenieria_comercial ;
    Preg = 'te interesa el marketing' -> Img = ingenieria_comercial ;
    Img = general),
    mostrar_imagen(Ventana, Img).

interfaz_principal :- 
    new(@main, dialog('Sistema Experto para Elección de Carrera', size(1000, 1000))),
    new(@texto, label(nombre, 'El diagnostico a partir de los datos es:', font('times', 'roman', 18))),
    new(@resp1, label(nombre, '', font('times', 'roman', 22))),
    new(@lblExp1, label(nombre, '', font('times', 'roman', 14))),
    new(@lblExp2, label(nombre, '', font('times', 'roman', 14))),
    new(@salir, button('SALIR', and(message(@main, destroy), message(@main, free)))),
    new(@boton, button('Iniciar consulta', message(@prolog, botones))),
    new(@historial, button('Ver Historial', message(@prolog, ver_historial))),
    mostrar_imagen(@main, general),
    send(@main, display, @boton, point(138, 450)),
    send(@main, display, @historial, point(250, 450)),
    send(@main, display, @texto, point(20, 130)),
    send(@main, display, @salir, point(300, 450)),
    send(@main, display, @resp1, point(20, 180)),
    send(@main, open_centered).

botones :- 
    borrado,
    send(@boton, free),
    mostrar_diagnostico(Carrera),
    ( Carrera == lo_siento_diagnostico_desconocido -> send(@texto, selection('No se pudo determinar la carrera.')); true ),
    send(@resp1, selection(Carrera)),
    new(@boton, button('Iniciar consulta', message(@prolog, botones))),
    new(@btntratamiento, button('Detalles y Tratamiento', message(@prolog, mostrar_tratamiento, Carrera))),
    send(@main, display, @boton, point(20, 450)),
    send(@main, display, @btntratamiento, point(138, 450)).

mostrar_tratamiento(X) :- 
    new(@tratam, dialog('Tratamiento')),
    send(@tratam, append, label(nombre, 'Explicacion: ')),
    send(@tratam, display, @lblExp1, point(70, 51)),
    send(@tratam, display, @lblExp2, point(50, 80)),
    tratamiento(X),
    send(@tratam, transient_for, @main),
    send(@tratam, open_centered).

tratamiento(X) :- 
    send(@lblExp1, selection('De acuerdo al diagnostico, La Carrera Sugerida Es:')),
    mostrar_imagen(@tratam, X).

borrado :- send(@resp1, selection('')).

% Inicialización de la interfaz
crea_interfaz_inicio :- 
    new(@interfaz, dialog('Bienvenido al Sistema Experto para Elección de Carrera', size(1000, 1000))),
    mostrar_imagen(@interfaz, general),
    new(BotonComenzar, button('COMENZAR', and(message(@prolog, interfaz_principal), and(message(@interfaz, destroy), message(@interfaz, free))))),
    new(BotonSalir, button('SALIDA', and(message(@interfaz, destroy), message(@interfaz, free)))),
    send(@interfaz, append, BotonComenzar),
    send(@interfaz, append, BotonSalir),
    send(@interfaz, open_centered).

:- crea_interfaz_inicio.
