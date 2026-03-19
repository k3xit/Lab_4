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

let rec buildIntTree (rnd: Random) tree count =
    match count with
    | 0 -> tree
    | _ ->
        let newValue = rnd.Next(1, 100)
        let updatedTree = insert tree newValue
        buildIntTree rnd updatedTree (count - 1)

let rec collectEven tree =
    match tree with
    | Empty -> []
    | Node (data, left, right) ->
        let leftList = collectEven left
        let rightList = collectEven right
        if data % 2 = 0 then
            data :: leftList @ rightList
        else
            leftList @ rightList

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
    printfn "Введите количество элементов дерева:"
    match Console.ReadLine() |> Int32.TryParse with
    | (true, n) when n > 0 -> n
    | _ ->
        printfn "Ошибка ввода. Повторите ввод."
        readNodeCount ()

[<EntryPoint>]
let main args =
    let rnd = Random()

    let nodesCount = readNodeCount ()
    
    let intTree = buildIntTree rnd Empty nodesCount

    printfn "\nДерево целых чисел (Отсортированное):"
    printSorted intTree
    printfn ""

    printfn "Визуализация дерева:"
    printTreeForm "" intTree

    let evenList = collectEven intTree

    printfn "\nСписок четных элементов:"
    evenList |> List.iter (printf "%d ")
    printfn ""
    0
