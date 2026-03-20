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

let rec foldTree f acc Tree = 
    match Tree with
    | Empty -> acc
    | Node (data, left, right) ->
        let accLeft = foldTree f acc left
        let accNode = f accLeft (Node(data, left, right))
        foldTree f accNode right

let collectEven acc node =
    match node with
    | Node(data, left, right) ->
        if data % 2 = 0 then
            acc @ [data]
        else
            acc
    | _ ->
        acc

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

    let evenList = foldTree collectEven [] intTree

    printfn "\nСписок четных элементов:"
    evenList |> List.iter (printf "%d ")
    printfn ""
    0
