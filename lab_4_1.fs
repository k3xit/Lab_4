open System


type 'T BinaryTree =
    | Node of 'T * 'T BinaryTree * 'T BinaryTree
    | Empty

let rec insert tree value =
    match tree with
    | Empty -> Node(value, Empty, Empty)
    | Node (currentValue, left, right) ->
        if value < currentValue then
            Node(currentValue, insert left value, right)
        elif value > currentValue then
            Node(currentValue, left, insert right value)
        else
            tree

let generateRandomString (rnd: Random) length =
    let chars = "abcdefghijklmnopqrstuvwxyz"
    [| 1 .. length |] 
    |> Array.map (fun _ -> chars.[rnd.Next(chars.Length)])
    |> String

let rec buildTree (rnd: Random) tree count =
    match count with
    | 0 -> tree
    | _ ->
        let newValue = generateRandomString rnd 5 
        let updatedTree = insert tree newValue
        buildTree rnd updatedTree (count - 1)


let rec mapTree f tree =
    match tree with
    | Empty -> Empty
    | Node (data, left, right) ->
        let newData = f data
        let newLeft = mapTree f left
        let newRight = mapTree f right
        Node (newData, newLeft, newRight)


let rec printSimple tree =
    match tree with
    | Empty -> ()
    | Node (data, left, right) ->
        printf "%A " data
        printSimple left
        printSimple right


let rec printSorted tree =
    match tree with
    | Empty -> ()
    | Node (data, left, right) ->
        printSorted left
        printf "%A " data
        printSorted right


let rec printTreeForm indent tree =
    match tree with
    | Empty -> ()
    | Node (data, left, right) ->
        printTreeForm (indent + "   ") right
        printfn "%s%A" indent data
        printTreeForm (indent + "   ") left


let rec readNodeCount () =
    printfn "Введите количество элементов начального дерева:"
    match Console.ReadLine() |> Int32.TryParse with
    | (true, n) when n > 0 -> n
    | _ ->
        printfn "Ошибка ввода. Введите положительное целое число."
        readNodeCount ()


let rec readTargetChar () =
    printfn "Введите символ для добавления в конец строк:"
    let input = Console.ReadLine()
    match String.IsNullOrEmpty(input) with
    | true ->
        printfn "Ошибка ввода. Строка не может быть пустой."
        readTargetChar ()
    | false ->
        match String.length(input) with
        | 1 -> input
        | _ -> 
            printfn "Ошибка ввода. Введено более одного символа"
            readTargetChar ()

[<EntryPoint>]
let main args =
    let rnd = Random()

    let nodesCount = readNodeCount ()
    let initialTree = buildTree rnd Empty nodesCount

    printfn "\nВывод начального дерева (Pre-order):"
    printSimple initialTree
    printfn ""

    printfn "Вывод в отсортированном виде (In-order):"
    printSorted initialTree
    printfn ""

    printfn "Визуализация начального дерева:"
    printTreeForm "" initialTree
    
    let targetChar = readTargetChar ()
    
    let transformTree = 
        mapTree 
            (fun (s: string) -> s + targetChar.ToString()) 
            initialTree

    printfn "\nВывод итогового дерева (Pre-order):"
    printSimple transformTree
    printfn ""

    printfn "Вывод в отсортированном виде (In-order):"
    printSorted transformTree
    printfn ""

    printfn "Визуализация итогового дерева:"
    printTreeForm "" transformTree

    0
