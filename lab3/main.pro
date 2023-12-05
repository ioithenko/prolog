implement main
    open core, file, stdio

domains
    type = жёсткий_диск; процессор; видеокарта; материнская_плата; блок_питания.
    slot = sata; socket; pin8; pci; atx20.
    assembly = assembly(type Тип, string Название, real Цена).

class facts - accessories
    комплектующее : (integer ID, type Тип, string Название).
    цена : (integer ID, integer Цена).
    интерфейсное_подключение : (integer ID, slot Слот).
    сборка : (string Уровень, integer ID).

class predicates
    сумма_элем : (real* List) -> real Sum.
    max : (real* List, real Max [out]) nondeterm.
    max_цена : () -> real Max determ.

clauses
    сумма_элем([]) = 0.
    сумма_элем([H | T]) = сумма_элем(T) + H.

class predicates
    вывод : (string*) nondeterm.
    инфа_инт : (slot Название_слота) -> string* Слот nondeterm.
    сборка_компл : (string Уровень) -> assembly*.
    сборка_сумм : (string Уровень) -> real Summa.

clauses
    вывод([X | Y]) :-
        write("\t", X),
        nl,
        вывод(Y).

    инфа_инт(X) = [ Name || комплектующее(N, _, Name) ] :-
        интерфейсное_подключение(N, X).

    сборка_компл(X) =
        [ assembly(Type, Name, Price) ||
            сборка(X, ID),
            комплектующее(ID, Type, Name),
            цена(ID, Price)
        ].

    сборка_сумм(X) = Sum :-
        Sum =
            сумма_элем(
                [ Price ||
                    сборка(X, ID),
                    цена(ID, Price)
                ]).

    max([Max], Max).

    max([H1, H2 | T], Max) :-
        H1 >= H2,
        max([H1 | T], Max).

    max([H1, H2 | T], Max) :-
        H1 <= H2,
        max([H2 | T], Max).

    max_цена() = Res :-
        max([ Price || цена(_, Price) ], Max),
        Res = Max,
        !.

class predicates
    write_assem : (assembly* Компл_цена).
clauses
    write_assem(L) :-
        foreach assembly(Type, Name, Price) = list::getMember_nd(L) do
            write("\t", Type, ": ", Name, " - ", Price, "\n")
        end foreach.

clauses
    run() :-
        console::init(),
        file::consult("../data.txt", accessories),
        fail.

    run() :-
        X = atx20,
        write("Комплектующие в наличии с ", X, ":\n"),
        L = инфа_инт(X),
        вывод(L),
        fail.

    run() :-
        X = "улучшенная",
        write("Сборка уровня ", X, ":\n"),
        write_assem(сборка_компл(X)),
        write("\tОбщая сумма сборки: "),
        write(сборка_сумм(X)),
        nl,
        fail.

    run() :-
        write('Самое дорогое комплектующее стоит: '),
        write(max_цена()),
        nl,
        fail.

    run().

end implement main

goal
    console::runUtf8(main::run).
