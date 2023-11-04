﻿% Copyright

implement main
    open core, file, stdio

domains
    type = жёсткий_диск; процессор; видеокарта; материнская_плата; блок_питания.
    slot = sata; socket; pin8; pci; atx20.

class facts - accessories
    комплектующее : (integer ID, type Тип, string Название).
    цена : (integer ID, integer Цена).
    интерфейсное_подключение : (integer ID, slot Слот).

class predicates
    список : (type Тип).
    совместимость_комплектующих : (string НазваниеА) failure.
    сколько_стоит : (string Название [out]) failure.
    стоимость : (string Название, integer Цена [out]) nondeterm.
    компьютер : (string НазваниеА [out], string НазваниеБ [out], string НазваниеВ [out], string НазваниеГ [out], string НазваниеД [out]) nondeterm.
    стоимость_компьютера : (real Цена) nondeterm.
    собрать : (type Тип) determ.

class facts
    s : (real Sum) single.

clauses
    s(0).

clauses
    список(Тип) :-
        комплектующее(_, Тип, Название),
        write("Тип комплектующего ", Тип, " Название: ", Название),
        nl,
        fail.

    список(_) :-
        write("Все варианты выведены\n").

    % правило: вывод всех комплектующих совместимых с заданным комплектующим (комплектующие одного типа не сравниваются)
    совместимость_комплектующих(НазваниеА) :-
        комплектующее(А, Атип, НазваниеА),
        комплектующее(Б, Бтип, НазваниеБ),
        Атип <> Бтип,
        интерфейсное_подключение(А, Разъем),
        интерфейсное_подключение(Б, Разъем),
        А <> Б,
        write(НазваниеА, " и ", НазваниеБ),
        nl,
        fail.

    % правило: вывод стоимости по названию комплектующего
    сколько_стоит(Название) :-
        комплектующее(ID, _, Название),
        цена(ID, Цена),
        write(Название, " стоит ", Цена),
        nl,
        fail.

    %правило:вспомагательное; стоимость комплектующего
    стоимость(Название, Цена) :-
        комплектующее(ID, _, Название),
        цена(ID, Цена).

    %правило: вспомагательное; набор комплектующих разных типов
    компьютер(А, Б, В, Г, Д) :-
        комплектующее(_, жёсткий_диск, А),
        комплектующее(_, процессор, Б),
        комплектующее(_, видеокарта, В),
        комплектующее(_, материнская_плата, Г),
        комплектующее(_, блок_питания, Д).

    % правило: вывод комплекта комплектующих в пределах заданной суммы
    стоимость_компьютера(Ц) :-
        компьютер(А, Б, В, Г, Д),
        стоимость(А, Ац),
        стоимость(Б, Бц),
        стоимость(В, Вц),
        стоимость(Г, Гц),
        стоимость(Д, Дц),
        if Ц >= Ац + Бц + Вц + Гц + Дц then
            write("Комплект по стоимости: ", А, ", ", Б, ", ", В, ", ", Г, " и ", Д, " стоит ", Ац + Бц + Вц + Гц + Дц),
            nl
        end if.

    % правило: вывод всех комплектующих одного типа и их суммарной стоимости
    собрать(Тип) :-
        assert(s(0)),
        комплектующее(А, Тип, _),
        цена(А, Цена),
        s(Sum),
        assert(s(Sum + Цена)),
        fail.

    собрать(Тип) :-
        s(Sum),
        write("Тип: ", Тип, ": Сумма всех комплектующих типа = ", Sum),
        nl.

clauses
    run() :-
        file::consult("../data.txt", accessories),
        fail.

    run() :-
        список(видеокарта),
        fail.

    run() :-
        write("\nСовместимость комплектующих: \n"),
        совместимость_комплектующих("Razer Disk").

    run() :-
        write("\nСтоимость комплектующих: \n"),
        сколько_стоит(_).

    run() :-
        write("\nСтоимость компьютера: \n"),
        стоимость_компьютера(17000),
        fail.

    run() :-
        write("\nСтоимость комплектующих одного типа: \n"),
        собрать(видеокарта),
        fail.

    run().

end implement main

goal
    console::runUtf8(main::run).
